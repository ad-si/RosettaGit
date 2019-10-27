+++
title = "Word frequency"
description = ""
date = 2019-10-16T15:00:34Z
aliases = []
[extra]
id = 21560
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[Category:Text processing]]

;Task:
Given a text file and an integer n, print the n most
common words in the file (and the number of
their occurrences) in decreasing frequency.

For the purposes of this task:
* A word is a sequence of one or more contiguous letters
* You are free to define what a letter is.  Underscores, accented letters, apostrophes, and other special characters can be handled at the example writer's discretion.  For example, you may treat a compound word like "well-dressed" as either one word or two.  The word "it's" could also be one or two words as you see fit.  You may also choose not to support non US-ASCII characters.  Feel free to explicitly state the thoughts behind the program decisions.
* Assume words will not span multiple lines.
* Do not worry about normalization of word spelling differences.  Treat "color" and "colour" as two distinct words.
* Uppercase letters are considered equivalent to their lowercase counterparts
* Words of equal frequency can be listed in any order


Show example output using [http://www.gutenberg.org/files/135/135-0.txt Les Misérables from Project Gutenberg] as the text file input and display the top 10 most used words.


;History:
This task was originally taken from programming pearls from [https://doi.org/10.1145/5948.315654 Communications of the ACM June 1986 Volume 29 Number 6]
where this problem is solved by Donald Knuth using literate programming and then critiqued by Doug McIlroy,
demonstrating solving the problem in a 6 line Unix shell script (provided as an example below).


;References:
*[http://franklinchen.com/blog/2011/12/08/revisiting-knuth-and-mcilroys-word-count-programs/ McIlroy's program]





## AutoHotkey


```AutoHotkey
URLDownloadToFile, http://www.gutenberg.org/files/135/135-0.txt, % A_temp "\tempfile.txt"
FileRead, H, % A_temp "\tempfile.txt"
FileDelete,  % A_temp "\tempfile.txt"
words := []
while pos := RegExMatch(H, "\b[[:alpha:]]+\b", m, A_Index=1?1:pos+StrLen(m))
	words[m] := words[m] ? words[m] + 1 : 1
for word, count in words
	list .= count "`t" word "`r`n"
Sort, list, RN
loop, parse, list, `n, `r
{
	result .= A_LoopField "`r`n"
	if A_Index = 10
		break
}
MsgBox % "Freq`tWord`n" result
return
```

Outputs:
```txt
Freq	Word
41036	The
19946	of
14940	and
14589	A
13939	TO
11204	in
9645	HE
8619	WAS
7922	THAT
6659	it
```





## Batch File


This takes a very long time per word thus I have chosen to feed it a 200 line sample and go from there.

You could cut the length of this down drastically if you didn't need to be able to recall the word at nth position and wished only to display the top 10 words.


```dos

@echo off

call:wordCount 1 2 3 4 5 6 7 8 9 10 42 101

pause>nul
exit

:wordCount
setlocal enabledelayedexpansion

set word=100000
set line=0
for /f "delims=" %%i in (input.txt) do (
	set /a line+=1
	for %%j in (%%i) do (
		if not !skip%%j!==true (
			echo line !line! ^| word !word:~-5! - "%%~j"
			
			type input.txt | find /i /c "%%~j" > count.tmp
			set /p tmpvar=<count.tmp
			
			set tmpvar=000000000!tmpvar!
			set tmpvar=!tmpvar:~-10!
			set count[!word!]=!tmpvar! %%~j
			
			set "skip%%j=true"
			set /a word+=1
		)
	)
)
del count.tmp

set wordcount=0
for /f "tokens=1,2 delims= " %%i in ('set count ^| sort /+14 /r') do (
	set /a wordcount+=1
	for /f "tokens=2 delims==" %%k in ("%%i") do (
		set word[!wordcount!]=!wordcount!. %%j - %%k
	)
)

cls
for %%i in (%*) do echo !word[%%i]!
endlocal
goto:eof
	

```



'''Output'''


```txt

1.  - 0000000140 I
2.  - 0000000140 a
3.  - 0000000118 He
4.  - 0000000100 the
5.  - 0000000080 an
6.  - 0000000075 in
7.  - 0000000066 at
8.  - 0000000062 is
9.  - 0000000058 on
10.  - 0000000058 as
42.  - 0000000010 with
101.  - 0000000004 ears

```



## C++

{{trans|C#}}

```cpp>#include <algorithm

#include <iostream>
#include <fstream>
#include <map>
#include <regex>
#include <string>
#include <vector>

int main() {
    using namespace std;
    regex wordRgx("\\w+");
    map<string, int> freq;
    string line;

    ifstream in("135-0.txt");
    if (!in.is_open()) {
        cerr << "Failed to open file\n";
        return 1;
    }
    while (getline(in, line)) {
        auto words_itr = sregex_iterator(line.cbegin(), line.cend(), wordRgx);
        auto words_end = sregex_iterator();
        while (words_itr != words_end) {
            auto match = *words_itr;
            auto word = match.str();
            if (word.size() > 0) {
                auto entry = freq.find(word);
                if (entry != freq.end()) {
                    entry->second++;
                } else {
                    freq.insert(make_pair(word, 1));
                }
            }
            words_itr = next(words_itr);
        }
    }
    in.close();

    vector<pair<string, int>> pairs;
    for (auto iter = freq.cbegin(); iter != freq.cend(); ++iter) {
        pairs.push_back(*iter);
    }
    sort(pairs.begin(), pairs.end(), [=](pair<string, int>& a, pair<string, int>& b) {
        return a.second > b.second;
    });

    cout << "Rank  Word  Frequency\n";
    cout << "
### =  ====  ======
\n";
    int rank = 1;
    for (auto iter = pairs.cbegin(); iter != pairs.cend() && rank <= 10; ++iter) {
        printf("%2d   %4s   %5d\n", rank++, iter->first.c_str(), iter->second);
    }

    return 0;
}
```

{{out}}

```txt
Rank  Word  Frequency

### =  ====  ======

 1    the   36636
 2     of   19615
 3    and   14079
 4     to   13535
 5      a   13527
 6     in   10256
 7    was    8543
 8   that    7324
 9     he    6814
10    had    6139
```


=={{header|C#|C sharp}}==
{{trans|D}}

```csharp
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

namespace WordCount {
    class Program {
        static void Main(string[] args) {
            var text = File.ReadAllText("135-0.txt").ToLower();

            var match = Regex.Match(text, "\\w+");
            Dictionary<string, int> freq = new Dictionary<string, int>();
            while (match.Success) {
                string word = match.Value;
                if (freq.ContainsKey(word)) {
                    freq[word]++;
                } else {
                    freq.Add(word, 1);
                }

                match = match.NextMatch();
            }

            Console.WriteLine("Rank  Word  Frequency");
            Console.WriteLine("
### =  ====  ======
");
            int rank = 1;
            foreach (var elem in freq.OrderByDescending(a => a.Value).Take(10)) {
                Console.WriteLine("{0,2}    {1,-4}    {2,5}", rank++, elem.Key, elem.Value);
            }
        }
    }
}
```

{{out}}

```txt
Rank  Word  Frequency

### =  ====  ======

 1    the     41035
 2    of      19946
 3    and     14940
 4    a       14577
 5    to      13939
 6    in      11204
 7    he       9645
 8    was      8619
 9    that     7922
10    it       6659
```



## Clojure


```clojure
(defn count-words [file n]
  (->> file
    slurp
    clojure.string/lower-case
    (re-seq #"\w+")
    frequencies
    (sort-by val >)
    (take n)))
```


{{Out}}

```txt

user=> (count-words "135-0.txt" 10)
(["the" 41036] ["of" 19946] ["and" 14940] ["a" 14589] ["to" 13939]
 ["in" 11204] ["he" 9645] ["was" 8619] ["that" 7922] ["it" 6659])

```



## Common Lisp


```lisp

(defun count-word (n pathname)
  (with-open-file (s pathname :direction :input)
    (loop for line = (read-line s nil nil) while line
          nconc (list-symb (drop-noise line)) into words
          finally (return (subseq (sort (pair words)
                                        #'> :key #'cdr)
                                  0 n)))))

  (defun list-symb (s)
    (let ((*read-eval* nil))
      (read-from-string (concatenate 'string "(" s ")"))))

(defun drop-noise (s)
  (delete-if-not #'(lambda (x) (or (alpha-char-p x)
                                   (equal x #\space)
                                   (equal x #\-))) s))

(defun pair (words &aux (hash (make-hash-table)) ac)
  (dolist (word words) (incf (gethash word hash 0)))
  (maphash #'(lambda (e n) (push `(,e . ,n) ac)) hash) ac)

```


{{Out}}

```txt

> (count-word 10 "c:/temp/135-0.txt")
((THE . 40738) (OF . 19922) (AND . 14878) (A . 14419) (TO . 13702) (IN . 11172)
 (HE . 9577) (WAS . 8612) (THAT . 7768) (IT . 6467))

```



## D


```D
import std.algorithm : sort;
import std.array : appender, split;
import std.range : take;
import std.stdio : File, writefln, writeln;
import std.typecons : Tuple;
import std.uni : toLower;

//Container for a word and how many times it has been seen
alias Pair = Tuple!(string, "k", int, "v");

void main() {
    int[string] wcnt;

    //Read the file line by line
    foreach (line; File("135-0.txt").byLine) {
        //Split the words on whitespace
        foreach (word; line.split) {
            //Increment the times the word has been seen
            wcnt[word.toLower.idup]++;
        }
    }

    //Associative arrays cannot be sort, so put the key/value in an array
    auto wb = appender!(Pair[]);
    foreach(k,v; wcnt) {
        wb.put(Pair(k,v));
    }
    Pair[] sw = wb.data.dup;

    //Sort the array, and display the top ten values
    writeln("Rank  Word        Frequency");
    int rank=1;
    foreach (word; sw.sort!"a.v>b.v".take(10)) {
        writefln("%4s  %-10s  %9s", rank++, word.k, word.v);
    }
}
```


{{out}}

```txt
Rank  Word        Frequency
   1  the             40368
   2  of              19863
   3  and             14470
   4  a               14277
   5  to              13587
   6  in              11019
   7  he               9212
   8  was              8346
   9  that             7251
  10  his              6414
```



## F Sharp


```fsharp

open System.IO
open System.Text.RegularExpressions
let g=Regex("[A-Za-zÀ-ÿ]+").Matches(File.ReadAllText "135-0.txt")
[for n in g do yield n.Value.ToLower()]|>List.countBy(id)|>List.sortBy(fun n->(-(snd n)))|>List.take 10|>List.iter(fun n->printfn "%A" n)

```

{{out}}

```txt

("the", 41088)
("of", 19949)
("and", 14942)
("a", 14596)
("to", 13951)
("in", 11214)
("he", 9648)
("was", 8621)
("that", 7924)
("it", 6661)

```



## Factor

This program expects stdin to read from a file via the command line. ( e.g. invoking the program in Windows: <tt>>factor word-count.factor < input.txt</tt> ) The definition of a word here is simply any string surrounded by some combination of spaces, punctuation, or newlines.

```factor

USING: ascii io math.statistics prettyprint sequences
splitting ;
IN: rosetta-code.word-count

lines " " join " .,?!:;()\"-" split harvest [ >lower ] map
sorted-histogram <reversed> 10 head .

```

{{out}}

```txt

{
    { "the" 41021 }
    { "of" 19945 }
    { "and" 14938 }
    { "a" 14522 }
    { "to" 13938 }
    { "in" 11201 }
    { "he" 9600 }
    { "was" 8618 }
    { "that" 7822 }
    { "it" 6532 }
}

```



## Go

{{trans|Kotlin}}

```go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
    "regexp"
    "sort"
    "strings"
)

type keyval struct {
    key string
    val int
}

func main() {
    reg := regexp.MustCompile(`\p{Ll}+`)
    bs, err := ioutil.ReadFile("135-0.txt")
    if err != nil {
        log.Fatal(err)
    }
    text := strings.ToLower(string(bs))
    matches := reg.FindAllString(text, -1)
    groups := make(map[string]int)
    for _, match := range matches {
        groups[match]++
    }
    var keyvals []keyval
    for k, v := range groups {
        keyvals = append(keyvals, keyval{k, v})
    }
    sort.Slice(keyvals, func(i, j int) bool {
        return keyvals[i].val > keyvals[j].val
    })
    fmt.Println("Rank  Word  Frequency")
    fmt.Println("
### =  ====  ======
")
    for rank := 1; rank <= 10; rank++ {
        word := keyvals[rank-1].key
        freq := keyvals[rank-1].val
        fmt.Printf("%2d    %-4s    %5d\n", rank, word, freq)
    }
}
```


{{out}}

```txt

Rank  Word  Frequency

### =  ====  ======

 1    the     41088
 2    of      19949
 3    and     14942
 4    a       14596
 5    to      13951
 6    in      11214
 7    he       9648
 8    was      8621
 9    that     7924
10    it       6661

```



## Groovy

Solution:

```groovy
def topWordCounts = { String content, int n ->
    def mapCounts = [:]
    content.toLowerCase().split(/\W+/).each {
        mapCounts[it] = (mapCounts[it] ?: 0) + 1
    }
    def top = (mapCounts.sort { a, b -> b.value <=> a.value }.collect{ it })[0..<n]
    println "Rank Word Frequency\n
### = ==== ======
"
    (0..<n).each { printf ("%4d %-4s %9d\n", it+1, top[it].key, top[it].value) }
}
```


Test:

```groovy
def rawText = "http://www.gutenberg.org/files/135/135-0.txt".toURL().text
topWordCounts(rawText, 10)
```


Output:

```txt
Rank Word Frequency

### = ==== ======

   1 the      41036
   2 of       19946
   3 and      14940
   4 a        14589
   5 to       13939
   6 in       11204
   7 he        9645
   8 was       8619
   9 that      7922
  10 it        6659
```



## Haskell

{{trans|Clojure}}

```Haskell
module Main where

import Control.Category   -- (>>>)
import Data.Char          -- toLower, isSpace
import Data.List          -- sortBy, (Foldable(foldl')), filter
import Data.Ord           -- Down
import System.IO          -- stdin, ReadMode, openFile, hClose
import System.Environment -- getArgs

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

-- text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

frequencies :: Ord a => [a] -> Map a Integer
frequencies = foldl' (\m k -> M.insertWith (+) k 1 m) M.empty
{-# SPECIALIZE frequencies :: [Text] -> Map Text Integer #-}

main :: IO ()
main = do
  args <- getArgs
  (n,hand,filep) <- case length args of
    0 -> return (10,stdin,False)
    1 -> return (read $ head args,stdin,False)
    _ -> let (ns:fp:_) = args
         in fmap (\h -> (read ns,h,True)) (openFile fp ReadMode)
  T.hGetContents hand >>=
    (T.map toLower
      >>> T.split isSpace
      >>> filter (not <<< T.null)
      >>> frequencies
      >>> M.toList
      >>> sortBy (comparing (Down <<< snd)) -- sort the opposite way
      >>> take n
      >>> print)
  when filep (hClose hand)
```

{{Out}}

```txt

$ ./word_count 10 < ~/doc/les_miserables*
[("the",40368),("of",19863),("and",14470),("a",14277),("to",13587),("in",11019),("he",9212),("was",8346),("that",7251),("his",6414)]

```



Or, perhaps a little more simply:

```haskell
import Data.List (group, sort, sortBy)
import Control.Arrow ((&&&))
import Data.Ord (comparing)
import Data.Char (toLower)

main :: IO ()
main = do
  xs <-
    readFile "lesMiserables.txt" >>=
    \txt ->
       return
         (unlines $
          show <$>
          take
            10
            (sortBy
               (flip (comparing fst))
               ((length &&& head) <$> group (sort . words $ fmap toLower txt))))
  putStrLn xs
```

{{Out}}

```txt
(40370,"the")
(19863,"of")
(14470,"and")
(14277,"a")
(13587,"to")
(11019,"in")
(9212,"he")
(8346,"was")
(7251,"that")
(6414,"his")
```



## J

Text acquisition: store the entire text from the web page http://www.gutenberg.org/files/135/135-0.txt (the plain text UTF-8 link) into a file.  This linux example uses ~/downloads/books/LesMis.txt .

Program:
Reading from left to right,
10 {.  "ten take" from an array computed by words to the right.
\:~    "sort descending" by items of the array computed by whatever is to the right.
(#;{.)/.~ "tally linked with item" key
<nowiki>;:     "words" parses the argument to its right as a j sentence.</nowiki>
tolower changes to a common case

Hence the remainder of the j sentence must clean after loading the file.

<nowiki>The parenthesized expression (a.-.Alpha_j_,' ') computes to a vector of the j alphabet excluding [a-zA-Z ]</nowiki>
<nowiki>((e.&(a.-.Alpha_j_,' '))`(,:&' '))}  substitutes space character for the unwanted characters.</nowiki>
<nowiki>1!:1 reads the file named in the box <</nowiki>

         

```txt

   10{.\:~(#;{.)/.~;:tolower((e.&(a.-.Alpha_j_,' '))`(,:&' '))}1!:1<jpath'~/downloads/books/LesMis.txt'
┌─────┬────┐
│41093│the │
├─────┼────┤
│19954│of  │
├─────┼────┤
│14943│and │
├─────┼────┤
│14558│a   │
├─────┼────┤
│13953│to  │
├─────┼────┤
│11219│in  │
├─────┼────┤
│9649 │he  │
├─────┼────┤
│8622 │was │
├─────┼────┤
│7924 │that│
├─────┼────┤
│6661 │it  │
└─────┴────┘
   

```



## Java

{{trans|Kotlin}}

```Java
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class WordCount {
    public static void main(String[] args) throws IOException {
        Path path = Paths.get("135-0.txt");
        byte[] bytes = Files.readAllBytes(path);
        String text = new String(bytes);
        text = text.toLowerCase();

        Pattern r = Pattern.compile("\\p{javaLowerCase}+");
        Matcher matcher = r.matcher(text);
        Map<String, Integer> freq = new HashMap<>();
        while (matcher.find()) {
            String word = matcher.group();
            Integer current = freq.getOrDefault(word, 0);
            freq.put(word, current + 1);
        }

        List<Map.Entry<String, Integer>> entries = freq.entrySet()
            .stream()
            .sorted((i1, i2) -> Integer.compare(i2.getValue(), i1.getValue()))
            .limit(10)
            .collect(Collectors.toList());

        System.out.println("Rank  Word  Frequency");
        System.out.println("
### =  ====  ======
");
        int rank = 1;
        for (Map.Entry<String, Integer> entry : entries) {
            String word = entry.getKey();
            Integer count = entry.getValue();
            System.out.printf("%2d    %-4s    %5d\n", rank++, word, count);
        }
    }
}
```

{{out}}

```txt
Rank  Word  Frequency

### =  ====  ======

 1    the     41088
 2    of      19949
 3    and     14942
 4    a       14596
 5    to      13951
 6    in      11214
 7    he       9648
 8    was      8621
 9    that     7924
10    it       6661
```



## Julia

{{works with|Julia|1.0}}

```julia

using FreqTables

txt = read("les-mis.txt", String)
words = split(replace(txt, r"\P{L}"i => " "))
table = sort(freqtable(words); rev=true)
println(table[1:10])
```


{{out}}

```txt
Dim1   │
───────┼──────
"the"  │ 36671
"of"   │ 19618
"and"  │ 14081
"to"   │ 13541
"a"    │ 13529
"in"   │ 10265
"was"  │  8545
"that" │  7326
"he"   │  6816
"had"  │  6140
```



## Kotlin

The author of the Perl 6 entry has given a good account of the difficulties with this task and, in the absence of any clarification on the various issues, I've followed a similar 'literal' approach.

So, after first converting the text to lower case, I've assumed that a word is any sequence of one or more lower-case Unicode letters and obtained the same results as the Perl 6 version. 

There is no change in the results if the numerals 0-9 are also regarded as letters.

```scala
// version 1.1.3

import java.io.File

fun main(args: Array<String>) {
    val text = File("135-0.txt").readText().toLowerCase()
    val r = Regex("""\p{javaLowerCase}+""")
    val matches = r.findAll(text)
    val wordGroups = matches.map { it.value }
                    .groupBy { it }
                    .map { Pair(it.key, it.value.size) }
                    .sortedByDescending { it.second }
                    .take(10)
    println("Rank  Word  Frequency")
    println("
### =  ====  ======
")
    var rank = 1
    for ((word, freq) in wordGroups) 
        System.out.printf("%2d    %-4s    %5d\n", rank++, word, freq)   
}
```


{{out}}

```txt

Rank  Word  Frequency

### =  ====  ======

 1    the     41088
 2    of      19949
 3    and     14942
 4    a       14596
 5    to      13951
 6    in      11214
 7    he       9648
 8    was      8621
 9    that     7924
10    it       6661

```



## Lua

{{works with|lua|5.3}}

```lua
local freq = {}
for line in io.lines(arg[1]) do
	local lowerline = string.lower(line)
	for word in string.gmatch(lowerline, "%a+") do
		if not freq[word] then
			freq[word] = 1
		else
			freq[word] = freq[word] + 1
		end
	end
end

local array = {}
for word, count in pairs(freq) do
	table.insert(array, {word, count})
end

table.sort(array, function (a, b) return a[2] > b[2] end)

for i = 1, arg[2] or 20 do
	io.write(string.format('%7d %s\n', array[i][2] , array[i][1]))
end

```



## Objeck


```objeck
use System.IO.File;
use Collection;
use RegEx;

class Rosetta {
  function : Main(args : String[]) ~ Nil {
    if(args->Size() <> 1) {
      return;
    };
    
    input := FileReader->ReadFile(args[0]);
    filter := RegEx->New("\\w+");
    words := filter->Find(input);
    
    word_counts := StringMap->New();
    each(i : words) {
      word := words->Get(i)->As(String);
      if(word <> Nil & word->Size() > 0) {
        word := word->ToLower();
        if(word_counts->Has(word)) {
          count := word_counts->Find(word)->As(IntHolder);
          count->Set(count->Get() + 1);
        }
        else {
          word_counts->Insert(word, IntHolder->New(1));
        };
      };
    };  
    
    count_words := IntMap->New();
    words := word_counts->GetKeys();
    each(i : words) {
      word := words->Get(i)->As(String);
      count := word_counts->Find(word)->As(IntHolder);
      count_words->Insert(count->Get(), word);
    };
    
    counts := count_words->GetKeys();
    counts->Sort();
    
    index := 1;
    "Rank\tWord\tFrequency"->PrintLine();
    "
### =\t====\t=
"->PrintLine();
    for(i := count_words->Size() - 1; i >= 0; i -= 1;) {
      if(count_words->Size() - 10 <= i) {
        count := counts->Get(i);
        word := count_words->Find(count)->As(String);
        "{$index}\t{$word}\t{$count}"->PrintLine();
        index += 1;
      };
    };
  }
}
```


Output:

```txt

Rank    Word    Frequency

### =    ====    =

1       the     41036
2       of      19946
3       and     14940
4       a       14589
5       to      13939
6       in      11204
7       he      9645
8       was     8619
9       that    7922
10      it      6659

```




## OCaml



```ocaml
let () =
  let n =
    try int_of_string Sys.argv.(1)
    with _ -> 10
  in
  let ic = open_in "135-0.txt" in
  let h = Hashtbl.create 97 in
  let w = Str.regexp "[^A-Za-zéèàêâôîûœ]+" in
  try
    while true do
      let line = input_line ic in
      let words = Str.split w line in
      List.iter (fun word ->
        let word = String.lowercase_ascii word in
        match Hashtbl.find_opt h word with
        | None -> Hashtbl.add h word 1
        | Some x -> Hashtbl.replace h word (succ x)
      ) words
    done
  with End_of_file ->
    close_in ic;
    let l = Hashtbl.fold (fun word count acc -> (word, count)::acc) h [] in
    let s = List.sort (fun (_, c1) (_, c2) -> compare c2 c1) l in
    let r = List.init n (fun i -> List.nth s i) in
    List.iter (fun (word, count) ->
      Printf.printf "%d  %s\n" count word
    ) r
```


{{out}}

```txt

$ ocaml str.cma word_freq.ml 
41092  the
19954  of
14943  and
14554  a
13953  to
11219  in
9649  he
8622  was
7924  that
6661  it

```




## Perl

{{trans|Perl 6}}

```perl
$top = 10;

open $fh, "<", '135-0.txt';
($text = join '', <$fh>) =~ tr/A-Z/a-z/;

@matcher = (
    qr/[a-z]+/,     # simple 7-bit ASCII
    qr/\w+/,        # word characters with underscore
    qr/[a-z0-9]+/,  # word characters without underscore
);

for $reg (@matcher) {
    print "\nTop $top using regex: " . $reg . "\n";
    @matches = $text =~ /$reg/g;
    my %words;
    for $w (@matches) { $words{$w}++ };
    $c = 0;
    for $w ( sort { $words{$b} <=> $words{$a} } keys %words ) {
        printf "%-7s %6d\n", $w, $words{$w};
        last if ++$c >= $top;
    }
}
```


{{out}}

```txt
Top 10 using regex: (?^:[a-z]+)
the      41089
of       19949
and      14942
a        14608
to       13951
in       11214
he        9648
was       8621
that      7924
it        6661

Top 10 using regex: (?^:\w+)
the      41036
of       19946
and      14940
a        14589
to       13939
in       11204
he        9645
was       8619
that      7922
it        6659

Top 10 using regex: (?^:[a-z0-9]+)
the      41089
of       19949
and      14942
a        14608
to       13951
in       11214
he        9648
was       8621
that      7924
it        6661
```


## Perl 6

{{works with|Rakudo|2017.07}}

Note: much of the following exposition is no longer critical to the task as the requirements have been updated, but is left here for historical and informational reasons.

This is slightly trickier than it appears initially. The task specifically states: "A word is a sequence of one or more contiguous letters", so contractions and hyphenated words are broken up. Initially we might reach for a regex matcher like /\w+/ , but \w includes underscore, which is not a letter but a punctuation connector; and this text is '''full''' of underscores since that is how Project Gutenberg texts denote italicized text. The underscores are not actually parts of the words though, they are markup.

We might try /A-Za-z/ as a matcher but this text is bursting with French words containing various accented glyphs. Those '''are''' letters, so words will be incorrectly split up; (Misérables will be counted as 'mis' and 'rables', probably not what we want.)

Actually, in this case /A-Za-z/ returns '''very nearly''' the correct answer. Unfortunately, the name "Alèthe" appears once (only once!) in the text, gets incorrectly split into Al & the, and incorrectly reports 41089 occurrences of "the".
The text has several words like "Panathenæa", "ça", "aérostiers" and "Keksekça" so the counts for 'a' are off too. The other 8 of the top 10 are "correct" using /A-Za-z/, but it is mostly by accident.

A more accurate regex matcher would be some kind of Unicode aware /\w/ minus underscore. It may also be useful, depending on your requirements, to recognize contractions with embedded apostrophes, hyphenated words, and hyphenated words broken across lines.

Here is a  sample that shows the result when using various different matchers.

```perl6
sub MAIN ($filename, $top = 10) {
    my $file = $filename.IO.slurp.lc.subst(/ (<[\w]-[_]>'-')\n(<[\w]-[_]>) /, {$0 ~ $1}, :g );
    my @matcher = (
        rx/ <[a..z]>+ /,    # simple 7-bit ASCII
        rx/ \w+ /,          # word characters with underscore
        rx/ <[\w]-[_]>+ /,  # word characters without underscore
        rx/ <[\w]-[_]>+[["'"|'-'|"'-"]<[\w]-[_]>+]* /   # word characters without underscore but with hyphens and contractions
    );
    for @matcher -> $reg {
        say "\nTop $top using regex: ", $reg.perl;
        .put for $file.comb( $reg ).Bag.sort(-*.value)[^$top];
    }
}
```


{{out}}
Passing in the file name and 10:

```txt
Top 10 using regex: rx/ <[a..z]>+ /
the	41089
of	19949
and	14942
a	14608
to	13951
in	11214
he	9648
was	8621
that	7924
it	6661

Top 10 using regex: rx/ \w+ /
the	41035
of	19946
and	14940
a	14577
to	13939
in	11204
he	9645
was	8619
that	7922
it	6659

Top 10 using regex: rx/ <[\w]-[_]>+ /
the	41088
of	19949
and	14942
a	14596
to	13951
in	11214
he	9648
was	8621
that	7924
it	6661

Top 10 using regex: rx/ <[\w]-[_]>+[["'"|'-'|"'-"]<[\w]-[_]>+]* /
the	41081
of	19930
and	14934
a	14587
to	13735
in	11204
he	9607
was	8620
that	7825
it	6535
```



## Phix


```Phix
?"loading..."
constant subs = "\t\r\n_.,\"\'!;:?][()|=<>#/*{}+@%&$",
         reps = repeat(' ',length(subs)),
         fn = open("135-0.txt","r")
string text = lower(substitute_all(get_text(fn),subs,reps))
close(fn)
sequence words = append(sort(split(text,no_empty:=true)),"")
constant wf = new_dict()
string last = words[1]
integer count = 1
for i=2 to length(words) do
    if words[i]!=last then
        setd({count,last},0,wf)
        count = 0
        last = words[i]
    end if
    count += 1
end for
count = 10
function visitor(object key, object /*data*/, object /*user_data*/)
    ?key
    count -= 1
    return count>0
end function
traverse_dict(routine_id("visitor"),0,wf,true)
```

{{out}}

```txt

loading...
{40743,"the"}
{19925,"of"}
{14881,"and"}
{14474,"a"}
{13704,"to"}
{11174,"in"}
{9623,"he"}
{8613,"was"}
{7867,"that"}
{6612,"it"}

```



## PicoLisp


```PicoLisp
(setq *Delim " ^I^J^M-_.,\"'*[]?!&@#$%^\(\):;")
(setq *Skip (chop *Delim))

(de word+ NIL
   (prog1
      (lowc (till *Delim T))
      (while (member (peek) *Skip) (char)) ) )

(off B)
(in "135-0.txt"
   (until (eof)
      (let W (word+)
         (if (idx 'B W T) (inc (car @)) (set W 1)) ) ) )
(for L (head 10 (flip (by val sort (idx 'B))))
   (println L (val L)) )
```

{{out}}

```txt

"the" 41088
"of" 19949
"and" 14942
"a" 14545
"to" 13950
"in" 11214
"he" 9647
"was" 8620
"that" 7924
"it" 6661

```



## Python


### Collections


### =Python2.7=


```python
import collections
import re
import string
import sys

def main():
  counter = collections.Counter(re.findall(r"\w+",open(sys.argv[1]).read().lower()))
  print counter.most_common(int(sys.argv[2]))

if __name__ == "__main__":
  main()
```


{{Out}}

```txt

$ python wordcount.py 135-0.txt 10
[('the', 41036), ('of', 19946), ('and', 14940), ('a', 14589), ('to', 13939),
 ('in', 11204), ('he', 9645), ('was', 8619), ('that', 7922), ('it', 6659)]

```



### =Python3.6=


```python
from collections import Counter
from re import findall

les_mis_file = 'les_mis_135-0.txt'

def _count_words(fname):
    with open(fname) as f:
        text = f.read()
    words = findall(r'\w+', text.lower())
    return Counter(words)

def most_common_words_in_file(fname, n):
    counts = _count_words(fname)
    for word, count in [['WORD', 'COUNT']] + counts.most_common(n):
        print(f'{word:>10} {count:>6}')


if __name__ == "__main__":
    n = int(input('How many?: '))
    most_common_words_in_file(les_mis_file, n)
```


{{Out}}

```txt
How many?: 10
      WORD  COUNT
       the  41036
        of  19946
       and  14940
         a  14586
        to  13939
        in  11204
        he   9645
       was   8619
      that   7922
        it   6659
```



### Sorted and groupby

{{Works with|Python|3.7}}

```python
"""
Word count task from Rosetta Code
http://www.rosettacode.org/wiki/Word_count#Python
"""
from itertools import (groupby,
                       starmap)
from operator import itemgetter
from pathlib import Path
from typing import (Iterable,
                    List,
                    Tuple)


FILEPATH = Path('lesMiserables.txt')
COUNT = 10


def main():
    words_and_counts = most_frequent_words(FILEPATH)
    print(*words_and_counts[:COUNT], sep='\n')


def most_frequent_words(filepath: Path,
                        *,
                        encoding: str = 'utf-8') -> List[Tuple[str, int]]:
    """
    A list of word-frequency pairs sorted by their occurrences.
    The words are read from the given file.
    """
    def word_and_frequency(word: str,
                           words_group: Iterable[str]) -> Tuple[str, int]:
        return word, capacity(words_group)

    file_contents = filepath.read_text(encoding=encoding)
    words = file_contents.lower().split()
    grouped_words = groupby(sorted(words))
    words_and_frequencies = starmap(word_and_frequency, grouped_words)
    return sorted(words_and_frequencies, key=itemgetter(1), reverse=True)


def capacity(iterable: Iterable) -> int:
    """Returns a number of elements in an iterable"""
    return sum(1 for _ in iterable)


if __name__ == '__main__':
    main()

```

{{Out}}

```txt
('the', 40372)
('of', 19868)
('and', 14472)
('a', 14278)
('to', 13589)
('in', 11024)
('he', 9213)
('was', 8347)
('that', 7250)
('his', 6414)
```



## R

I chose to remove apostrophes only if they're followed by an s (so "mom" and "mom's" will show up as the same word but "they" and "they're" won't). I also chose not to remove hyphens.

```R

wordcount<-function(file,n){
  punctuation=c("`","~","!","@","#","$","%","^","&","*","(",")","_","+","=","{","[","}","]","|","\\",":",";","\"","<",",",">",".","?","/","'s")
  wordlist=scan(file,what=character())
  wordlist=tolower(wordlist)
  for(i in 1:length(punctuation)){
    wordlist=gsub(punctuation[i],"",wordlist,fixed=T)
  }
  df=data.frame("Word"=sort(unique(wordlist)),"Count"=rep(0,length(unique(wordlist))))
  for(i in 1:length(unique(wordlist))){
    df[i,2]=length(which(wordlist==df[i,1]))
  }
  df=df[order(df[,2],decreasing = T),]
  row.names(df)=1:nrow(df)
  return(df[1:n,])
}

```

{{Out}}

```txt

> wordcount("MobyDick.txt",10)
Read 212793 items
   Word Count
1   the 14346
2    of  6590
3   and  6340
4     a  4611
5    to  4572
6    in  4130
7  that  2903
8   his  2516
9    it  2308
10    i  1845

```



## Racket


```racket
#lang racket

(define (all-words f (case-fold string-downcase))
  (map case-fold (regexp-match* #px"\\w+" (file->string f))))

(define (l.|l| l) (cons (car l) (length l)))

(define (counts l (>? >)) (sort (map l.|l| (group-by values l)) >? #:key cdr))

(module+ main
  (take (counts (all-words "data/les-mis.txt")) 10))
```


{{out}}

```txt
'(("the" . 41036)
  ("of" . 19946)
  ("and" . 14940)
  ("a" . 14589)
  ("to" . 13939)
  ("in" . 11204)
  ("he" . 9645)
  ("was" . 8619)
  ("that" . 7922)
  ("it" . 6659))
```



## REXX


### version 1

This REXX version doesn't need to sort the list of words.

Currently, this version recognizes all the accented (non-Latin) accented letters that are present in the text (file) that is specified to be used   (and some other non-Latin letters as well).   This means that the word     <big><big> Alèthe </big></big>     is treated as one word, not as two words     <big><big> Al  the </big></big>     (and not thereby adding two words).

This version also supports words that contain embedded apostrophes (<b><big>''' ' '''</big></b>)     [that is, within a word, but not those words that start or end with an apostrophe; for those words, the apostrophe is elided].  

Thus,   ''' it's '''   is counted separately from   '''it'''   or   ''' its'''.

Since REXX doesn't support UTF-8 encodings, code was added to this REXX version to support the accented letters in the mandated input file. 

```rexx
/*REXX pgm displays top 10 words in a file (includes foreign letters),  case is ignored.*/
parse arg fID top .                              /*obtain optional arguments from the CL*/
if fID=='' | fID==","  then fID= 'les_mes.TXT'   /*None specified? Then use the default.*/
if top=='' | top==","  then top= 10              /*  "      "        "   "   "     "    */
@.=0;  c=0;   abcL="abcdefghijklmnopqrstuvwxyz'" /*initialize word list, count; alphabet*/
q= "'";       abcU= abcL;            upper abcU  /*define uppercase version of  alphabet*/
totW=0;       accL= 'üéâÄàÅÇêëèïîìéæôÖòûùÿáíóúÑ' /*   "       "    of some accented chrs*/
              accU= 'ÜéâäàåçêëèïîìÉÆôöòûùÿáíóúñ' /*   "   lowercase accented characters.*/
              accG= 'αßΓπΣσµτΦΘΩδφε'             /*   "   some upper/lower Greek letters*/
a=abcL || abcL ||accL ||accL || accG             /*   "   char string of  after letters.*/
b=abcL || abcU ||accL ||accU || accG || xrange() /*   "   char string of before    "    */
x= 'Çà åÅ çÇ êÉ ëÉ áà óâ ªæ ºç ¿è ⌐é ¬ê ½ë «î »ï ▒ñ ┤ô ╣ù ╗û ╝ü' /*list of 16-bit chars.*/
xs= words(x)                                                     /*num.  "   "      "   */
!.=                                              /*define the original word instances.  */
     do #=0  while lines(fID)\==0; $=linein(fID) /*loop whilst there are lines in file. */
     if pos('├', $)\==0  then do k=1  for xs;     _=word(x, k)      /*any 16-bit chars? */
                              $=changestr('├'left(_, 1), $, right(_, 1) )     /*convert.*/
                              end   /*k*/
     $=translate( $, a, b)                       /*remove superfluous blanks in the line*/
         do while $\='';      parse var  $  z  $ /*now, process each word in the $ list.*/
         parse var  z     z1  2  zr  ''  -1  zL  /*extract:  first, middle, & last char.*/
         if z1==q  then do; z=zr; if z=='' then iterate; end  /*starts with apostrophe? */
         if zL==q  then z=strip(left(z, length(z) - 1))       /*ends     "       "      */
         if z==''  then iterate                               /*if Z is now null,  skip.*/
         if @.z==0  then do;  c=c+1; !.c=z; end  /*bump word count; assign word to array*/
         totW=totW + 1;       @.z=@.z + 1        /*bump total words & count of the word.*/
         end   /*while*/
     end       /*#*/
say commas(totW)     ' words found  ('commas(c)    "unique)  in "    commas(#),
                     ' records read from file: '     fID;                              say
say right('word', 40)  " "  center(' rank ', 6)  "  count "   /*display title for output*/
say right('════', 40)  " "  center('══════', 6)  " ═══════"   /*   "    title separator.*/
tops=1
     do  until otops==tops | tops>top            /*process enough words to satisfy  TOP.*/
     WL=;         mk=0;     otops=tops           /*initialize the word list (to a NULL).*/
          do n=1  for c;    z=!.n;      k=@.z    /*process the list of words in the file*/
          if k==mk  then WL=WL z                 /*handle cases of tied number of words.*/
          if k> mk  then do;  mk=k;  WL=z;  end  /*this word count is the current max.  */
          end   /*n*/
     wr=max( length(' rank '), length(top) )     /*find the maximum length of the rank #*/
          do d=1  for words(WL);  _=word(WL, d)  /*process all words in the  word list. */
          if d==1  then w=max(10, length(@._) )  /*use length of the first number used. */
          say right(@._, 40)          right(commas(tops), wr)        right(commas(@._), w)
          @._= -1                                /*nullify word count for next go around*/
          end   /*d*/                            /* [↑]  this allows a non-sorted list. */
     tops=tops + words(WL)                       /*correctly handle any  tied  rankings.*/
     end        /*until*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas: procedure;  parse arg _;      n=_'.9';      #=123456789;       b=verify(n, #, "M")
        e=verify(n, #'0', , verify(n, #"0.", 'M') ) - 4
                 do j=e  to b  by -3;   _=insert(',', _, j);   end  /*j*/;        return _
```

{{out|output|text=  when using the default inputs:}}

```txt

574,122  words found  (23,414 unique)  in  67,663  records read from file:  les_mes.TXT

                                    word    rank    count
                                    ════   ══════  ═══════
                                     the      1     41,088
                                      of      2     19,949
                                     and      3     14,942
                                       a      4     14,595
                                      to      5     13,950
                                      in      6     11,214
                                      he      7      9,607
                                     was      8      8,620
                                    that      9      7,826
                                      it     10      6,535

```

To see a list of the top 1,000 words that show (among other things) words like   '''it's'''   and other accented words, see the discussion page. 




### version 2

Inspired by version 1 and adapted for ooRexx.
It ignores all characters other than a-z and A-Z (which are translated to a-z).
<lang>/*REXX program   reads  and  displays  a  count  of words a file.  Word case is ignored.*/
Call time 'R'
abc='abcdefghijklmnopqrstuvwxyz'
abcABC=abc||translate(abc)
parse arg fID_top                                /*obtain optional arguments from the CL*/
Parse Var fid_top fid ',' top
if fID=='' then fID= 'mis.TXT'                   /* Use default if not specified        */
if top=='' then top= 10                          /* Use default if not specified        */
occ.=0                                           /* occurrences of word (stem) in file  */
wn=0
Do While lines(fid)>0                            /*loop whilst there are lines in file. */
  line=linein(fID)
  line=translate(line,abc||abc,abcABC||xrange('00'x,'ff'x)) /*use only lowercase letters*/
  Do While line<>''
    Parse Var line word line                       /* take a word                         */
    If occ.word=0 Then Do                          /* not yet in word list                */
      wn=wn+1
      word.wn=word
      End
    occ.word=occ.word+1
    End
  End
Say 'We found' wn 'different words'
say right('word',40) ' rank   count '            /* header                              */
say right('----',40) '------ -------'            /* separator.                          */
tops=0
Do Until tops>=top | tops>=wn                    /*process enough words to satisfy  TOP.*/
  max_occ=0
  tl=''                                          /*initialize (possibly) a list of words*/
  Do wi=1 To wn                                  /*process the list of words in the file*/
    word=word.wi                                 /* take a word from the list           */
    Select
      When occ.word>max_occ Then Do              /* most occurrences so far             */
        tl=word                                  /* candidate for output                */
        max_occ=occ.word                         /* current maximum occurrences         */
        End
      When occ.word=max_occ Then Do              /* tied                                */
        tl=tl word                               /* add to output candidate             */
        End
      Otherwise                                  /* no candidate (yet)                  */
        Nop
      End
    End
    do d=1 for words(tl)
      word=word(tl,d)
      say right(word,40) right(tops+1,4) right(occ.word,8)
      occ.word=0                                /*nullify this word count for next time*/
      End
    tops=tops+words(tl)                         /*correctly handle the tied rankings.  */
  end
Say time('E') 'seconds elapsed'
```

{{out}}

```txt
We found 22820 different words
                                    word  rank   count
                                    ---- ------ -------
                                     the    1    41089
                                      of    2    19949
                                     and    3    14942
                                       a    4    14608
                                      to    5    13951
                                      in    6    11214
                                      he    7     9648
                                     was    8     8621
                                    that    9     7924
                                      it   10     6661
1.750000 seconds elapsed
```



## Ring


```ring

# project : Word count

fp = fopen("Miserables.txt","r")
str = fread(fp, getFileSize(fp))
fclose(fp) 

mis =substr(str, " ", nl)
mis = lower(mis)
mis = str2list(mis)
count = list(len(mis))
ready = []
for n = 1 to len(mis)
     flag = 0
     for m = 1 to len(mis)
           if mis[n] = mis[m] and n != m
              for p = 1 to len(ready)
                    if m = ready[p]
                       flag = 1
                    ok
              next
              if flag = 0
                 count[n] = count[n] + 1                 
              ok
           ok
     next
     if flag = 0
        add(ready, n)
     ok
next
for n = 1 to len(count)
     for m = n + 1 to len(count)
          if count[m] > count[n]
             temp = count[n]
             count[n] = count[m]
             count[m] = temp
             temp = mis[n]
             mis[n] = mis[m]
             mis[m] = temp
          ok
     next
next
for n = 1 to 10
     see mis[n] + " " + (count[n] + 1) + nl
next

func getFileSize fp
        c_filestart = 0
        c_fileend = 2
        fseek(fp,0,c_fileend)
        nfilesize = ftell(fp)
        fseek(fp,0,c_filestart)
        return nfilesize

func swap(a, b)
        temp = a
        a = b
        b = temp
        return [a, b]

```

Output:

```txt

the	41089
of	19949
and	14942
a	14608
to	13951
in	11214
he	9648
was	8621
that	7924
it	6661

```



## Ruby


```ruby

class String
  def wc
  n = Hash.new(0)
  downcase.scan(/[A-Za-zÀ-ÿ]+/) { |g| n[g] += 1 }
  n.sort{|n,g| n[1]<=>g[1]}
  end
end

open('135-0.txt') { |n| n.read.wc[-10,10].each{|n| puts n[0].to_s+"->"+n[1].to_s} }

```

{{out}}

```txt

it->6661
that->7924
was->8621
he->9648
in->11214
to->13951
a->14596
and->14942
of->19949
the->41088

```



## Rust


```Rust
use std::cmp::Reverse;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};

extern crate regex;
use regex::Regex;

fn word_count(file: File, n: usize) {
    let word_regex = Regex::new("(?i)[a-z']+").unwrap();

    let mut words = HashMap::new();
    for line in BufReader::new(file).lines() {
        word_regex
            .find_iter(&line.expect("Read error"))
            .map(|m| m.as_str())
            .for_each(|word| {
                *words.entry(word.to_lowercase()).or_insert(0) += 1;
            });
    }

    let mut words: Vec<_> = words.iter().collect();
    words.sort_unstable_by_key(|&(word, count)| (Reverse(count), word));

    for (word, count) in words.iter().take(n) {
        println!("{:8} {:>8}", word, count);
    }
}

fn main() {
    word_count(File::open("135-0.txt").expect("File open error"), 10)
}
```


{{out}}

```txt

the         41083
of          19948
and         14941
a           14604
to          13951
in          11212
he           9604
was          8621
that         7824
it           6534

```



## Scala


###  Featuring online remote file as input

{{Out}}
Best seen running in your browser [https://scastie.scala-lang.org/EP2Fm6HXQrC1DwtSNvnUzQ Scastie (remote JVM)].

```Scala
import scala.io.Source

object WordCount extends App {

  val url = "http://www.gutenberg.org/files/135/135-0.txt"
  val header = "Rank Word  Frequency\n
### = ======== ===
"

  def wordCnt =
    Source.fromURL(url).getLines()
      .filter(_.nonEmpty)
      .flatMap(_.split("""\W+""")).toSeq
      .groupBy(_.toLowerCase())
      .mapValues(_.size).toSeq
      .sortWith { case ((_, v0), (_, v1)) => v0 > v1 }
      .take(10).zipWithIndex

  println(header)
  wordCnt.foreach {
    case ((word, count), rank) => println(f"${rank + 1}%4d $word%-8s $count%6d")
  }

  println(s"\nSuccessfully completed without errors. [total ${scala.compat.Platform.currentTime - executionStart} ms]")

}
```

{{out}}

```txt
Rank Word  Frequency

### = ======== ===

   1 the       41036
   2 of        19946
   3 and       14940
   4 a         14589
   5 to        13939
   6 in        11204
   7 he         9645
   8 was        8619
   9 that       7922
  10 it         6659

Successfully completed without errors. [total 4528 ms]
```



## Seed7


The Seed7 program uses the function [http://seed7.sourceforge.net/libraries/gethttp.htm#getHttp(in_string) getHttp],
to get the file 135-0.txt directly from Gutemberg. The library [http://seed7.sourceforge.net/libraries/scanfile.htm scanfile.s7i]
provides [http://seed7.sourceforge.net/libraries/scanfile.htm#getSimpleSymbol(inout_file) getSimpleSymbol],
to get words from a fle. The words are [http://seed7.sourceforge.net/libraries/string.htm#lower(in_string) converted to lower case], to assure that "The" and "the" are considered the same.


```seed7
$ include "seed7_05.s7i";
  include "gethttp.s7i";
  include "strifile.s7i";
  include "scanfile.s7i";
  include "chartype.s7i";
  include "console.s7i";

const type: wordHash is hash [string] integer;
const type: countHash is hash [integer] array string;

const proc: main is func
  local
    var file: inFile is STD_NULL;
    var string: aWord is "";
    var wordHash: numberOfWords is wordHash.EMPTY_HASH;
    var countHash: countWords is countHash.EMPTY_HASH;
    var array integer: countKeys is 0 times 0;
    var integer: index is 0;
    var integer: number is 0;
  begin
    OUT := STD_CONSOLE;
    inFile := openStrifile(getHttp("www.gutenberg.org/files/135/135-0.txt"));
    while hasNext(inFile) do
      aWord := lower(getSimpleSymbol(inFile));
      if aWord <> "" and aWord[1] in letter_char then
        if aWord in numberOfWords then
          incr(numberOfWords[aWord]);
        else
          numberOfWords @:= [aWord] 1;
        end if;
      end if;
    end while;
    countWords := flip(numberOfWords);
    countKeys := sort(keys(countWords));
    writeln("Word    Frequency");
    for index range length(countKeys) downto length(countKeys) - 9 do
      number := countKeys[index];
      for aWord range sort(countWords[number]) do
        writeln(aWord rpad 8 <& number);
      end for;
    end for;
  end func;
```


{{out}}

```txt

Word    Frequency
the     41036
of      19946
and     14940
a       14589
to      13939
in      11204
he      9645
was     8619
that    7922
it      6659

```



## Sidef


```ruby
var count = Hash()
var file = File(ARGV[0] \\ '135-0.txt')

file.open_r.each { |line|
    line.lc.scan(/[\pL]+/).each { |word|
        count{word} := 0 ++
    }
}

var top = count.sort_by {|_,v| v }.last(10).flip

top.each { |pair|
    say "#{pair.key}\t-> #{pair.value}"
}
```

{{out}}

```txt

the	-> 41088
of	-> 19949
and	-> 14942
a	-> 14596
to	-> 13951
in	-> 11214
he	-> 9648
was	-> 8621
that	-> 7924
it	-> 6661

```



## Simula


```simula
COMMENT COMPILE WITH
$ cim -m64 word-count.sim
;
BEGIN

    COMMENT ----- CLASSES FOR GENERAL USE ;

    ! ABSTRACT HASH KEY TYPE ;
    CLASS HASHKEY;
    VIRTUAL:
        PROCEDURE HASH IS
            INTEGER PROCEDURE HASH;;
        PROCEDURE EQUALTO IS
            BOOLEAN PROCEDURE EQUALTO(K); REF(HASHKEY) K;;
    BEGIN
    END HASHKEY;

    ! ABSTRACT HASH VALUE TYPE ;
    CLASS HASHVAL;
    BEGIN
        ! THERE IS NOTHING REQUIRED FOR THE VALUE TYPE ;
    END HASHVAL;

    CLASS HASHMAP;
    BEGIN
        CLASS INNERHASHMAP(N); INTEGER N;
        BEGIN

            INTEGER PROCEDURE INDEX(K); REF(HASHKEY) K;
            BEGIN
                INTEGER I;
                IF K == NONE THEN
                    ERROR("HASHMAP.INDEX: NONE IS NOT A VALID KEY");
                I := MOD(K.HASH,N);
            LOOP:
                IF KEYTABLE(I) == NONE OR ELSE KEYTABLE(I).EQUALTO(K) THEN
                    INDEX := I
                ELSE BEGIN
                    I := IF I+1 = N THEN 0 ELSE I+1;
                    GO TO LOOP;
                END;
            END INDEX;

            ! PUT SOMETHING IN ;
            PROCEDURE PUT(K,V); REF(HASHKEY) K; REF(HASHVAL) V;
            BEGIN
                INTEGER I;
                IF V == NONE THEN
                    ERROR("HASHMAP.PUT: NONE IS NOT A VALID VALUE");
                I := INDEX(K);
                IF KEYTABLE(I) == NONE THEN BEGIN
                    IF SIZE = N THEN
                        ERROR("HASHMAP.PUT: TABLE FILLED COMPLETELY");
                    KEYTABLE(I) :- K;
                    VALTABLE(I) :- V;
                    SIZE := SIZE+1;
                END ELSE
                    VALTABLE(I) :- V;
            END PUT;

            ! GET SOMETHING OUT ;
            REF(HASHVAL) PROCEDURE GET(K); REF(HASHKEY) K;
            BEGIN
                INTEGER I;
                IF K == NONE THEN
                    ERROR("HASHMAP.GET: NONE IS NOT A VALID KEY");
                I := INDEX(K);
                IF KEYTABLE(I) == NONE THEN
                    GET :- NONE ! ERROR("HASHMAP.GET: KEY NOT FOUND");
                ELSE
                    GET :- VALTABLE(I);
            END GET;

            PROCEDURE CLEAR;
            BEGIN
                INTEGER I;
                FOR I := 0 STEP 1 UNTIL N-1 DO BEGIN
                    KEYTABLE(I) :- NONE;
                    VALTABLE(I) :- NONE;
                END;
                SIZE := 0;
            END CLEAR;

            ! DATA MEMBERS OF CLASS HASHMAP ;
            REF(HASHKEY) ARRAY KEYTABLE(0:N-1);
            REF(HASHVAL) ARRAY VALTABLE(0:N-1);
            INTEGER SIZE;

        END INNERHASHMAP;

        PROCEDURE PUT(K,V); REF(HASHKEY) K; REF(HASHVAL) V;
        BEGIN
            IF IMAP.SIZE >= 0.75 * IMAP.N THEN
            BEGIN
                COMMENT RESIZE HASHMAP ;
                REF(INNERHASHMAP) NEWIMAP;
                REF(ITERATOR) IT;
                NEWIMAP :- NEW INNERHASHMAP(2 * IMAP.N);
                IT :- NEW ITERATOR(THIS HASHMAP);
                WHILE IT.MORE DO
                BEGIN
                    REF(HASHKEY) KEY;
                    KEY :- IT.NEXT;
                    NEWIMAP.PUT(KEY, IMAP.GET(KEY));
                END;
                IMAP.CLEAR;
                IMAP :- NEWIMAP;
            END;
            IMAP.PUT(K, V);
        END;

        REF(HASHVAL) PROCEDURE GET(K); REF(HASHKEY) K;
            GET :- IMAP.GET(K);

        PROCEDURE CLEAR;
            IMAP.CLEAR;

        INTEGER PROCEDURE SIZE;
            SIZE := IMAP.SIZE;

        REF(INNERHASHMAP) IMAP;

        IMAP :- NEW INNERHASHMAP(16);
    END HASHMAP;

    CLASS ITERATOR(H); REF(HASHMAP) H;
    BEGIN
        INTEGER POS,KEYCOUNT;

        BOOLEAN PROCEDURE MORE;
            MORE := KEYCOUNT < H.SIZE;

        REF(HASHKEY) PROCEDURE NEXT;
        BEGIN
            INSPECT H DO
            INSPECT IMAP DO
            BEGIN
                WHILE KEYTABLE(POS) == NONE DO
                    POS := POS+1;
                NEXT :- KEYTABLE(POS);
                KEYCOUNT := KEYCOUNT+1;
                POS := POS+1;
            END;
        END NEXT;

    END ITERATOR;

    COMMENT ----- PROBLEM SPECIFIC CLASSES ;

    HASHKEY CLASS TEXTHASHKEY(T); VALUE T; TEXT T;
    BEGIN
        INTEGER PROCEDURE HASH;
        BEGIN
            INTEGER I;
            T.SETPOS(1);
            WHILE T.MORE DO
                I := 31*I+RANK(T.GETCHAR);
            HASH := I;
        END HASH;
        BOOLEAN PROCEDURE EQUALTO(K); REF(HASHKEY) K;
            EQUALTO := T = K QUA TEXTHASHKEY.T;
    END TEXTHASHKEY;

    HASHVAL CLASS COUNTER;
    BEGIN
        INTEGER COUNT;
    END COUNTER;

    REF(INFILE) INF;
    REF(HASHMAP) MAP;
    REF(TEXTHASHKEY) KEY;
    REF(COUNTER) VAL;
    REF(ITERATOR) IT;
    TEXT LINE, WORD;
    INTEGER I, J, MAXCOUNT, LINENO;
    INTEGER ARRAY MAXCOUNTS(1:10);
    REF(TEXTHASHKEY) ARRAY MAXWORDS(1:10);

    WORD :- BLANKS(1000);
    MAP :- NEW HASHMAP;
  
    COMMENT MAP WORDS TO COUNTERS ;

    INF :- NEW INFILE("135-0.txt");
    INF.OPEN(BLANKS(4096));
    WHILE NOT INF.LASTITEM DO
    BEGIN
        BOOLEAN INWORD;

        PROCEDURE SAVE;
        BEGIN
            IF WORD.POS > 1 THEN
            BEGIN
                KEY :- NEW TEXTHASHKEY(WORD.SUB(1, WORD.POS - 1));
                VAL :- MAP.GET(KEY);
                IF VAL == NONE THEN
                BEGIN
                    VAL :- NEW COUNTER;
                    MAP.PUT(KEY, VAL);
                END;
                VAL.COUNT := VAL.COUNT + 1;
                WORD := " ";
                WORD.SETPOS(1);
            END;
        END SAVE;

        LINENO := LINENO + 1;
        LINE :- COPY(INF.IMAGE).STRIP; INF.INIMAGE;

        COMMENT SEARCH WORDS IN LINE ;
        COMMENT A WORD IS ANY SEQUENCE OF LETTERS ;

        INWORD := FALSE;
        LINE.SETPOS(1);
        WHILE LINE.MORE DO
        BEGIN
            CHARACTER CH;
            CH := LINE.GETCHAR;
            IF CH >= 'a' AND CH <= 'z' THEN
                CH := CHAR(RANK(CH) - RANK('a') + RANK('A'));
            IF CH >= 'A' AND CH <= 'Z' THEN
            BEGIN
                IF NOT INWORD THEN
                BEGIN
                    SAVE;
                    INWORD := TRUE;
                END;
                WORD.PUTCHAR(CH);
            END ELSE
            BEGIN
                IF INWORD THEN
                BEGIN
                    SAVE;
                    INWORD := FALSE;
                END;
            END;
        END;
        SAVE; COMMENT LAST WORD ;
    END;
    INF.CLOSE;

    COMMENT FIND 10 MOST COMMON WORDS ;

    IT :- NEW ITERATOR(MAP);
    WHILE IT.MORE DO
    BEGIN
        KEY :- IT.NEXT;
        VAL :- MAP.GET(KEY);
        FOR I := 1 STEP 1 UNTIL 10 DO
        BEGIN
            IF VAL.COUNT >= MAXCOUNTS(I) THEN
            BEGIN
                FOR J := 10 STEP -1 UNTIL I + 1 DO
                BEGIN
                    MAXCOUNTS(J) := MAXCOUNTS(J - 1);
                    MAXWORDS(J) :- MAXWORDS(J - 1);
                END;
                MAXCOUNTS(I) := VAL.COUNT;
                MAXWORDS(I) :- KEY;
                GO TO BREAK;
            END;
        END;
    BREAK:
    END;

    COMMENT OUTPUT 10 MOST COMMON WORDS ;

    FOR I := 1 STEP 1 UNTIL 10 DO
    BEGIN
        IF MAXWORDS(I) =/= NONE THEN
        BEGIN
            OUTINT(MAXCOUNTS(I), 10);
            OUTTEXT(" ");
            OUTTEXT(MAXWORDS(I) QUA TEXTHASHKEY.T);
            OUTIMAGE;
        END;
    END;

END

```

{{out}}

```txt

     41089 THE
     19949 OF
     14942 AND
     14608 A
     13951 TO
     11214 IN
      9648 HE
      8621 WAS
      7924 THAT
      6661 IT

6 garbage collection(s) in 0.2 seconds.

```



## UNIX Shell

{{works with|Bash}}
{{works with|zsh}}
This is derived from Doug McIlroy's original 6-line note in the ACM article cited in the task.

```bash
#!/bin/sh
cat ${1} | tr -cs A-Za-z '\n' | tr A-Z a-z | sort | uniq -c | sort -rn | sed ${2}q
```



{{Out}}

```txt

$ ./wordcount.sh 135-0.txt 10 
41089 the
19949 of
14942 and
14608 a
13951 to
11214 in
9648 he
8621 was
7924 that
6661 it

```



## VBA

In order to use it, you have to adapt the PATHFILE Const.


```vb

Option Explicit

Private Const PATHFILE As String = "C:\HOME\VBA\ROSETTA"

Sub Main()
Dim arr
Dim Dict As Object
Dim Book As String, temp As String
Dim T#
T = Timer
   Book = ExtractTxt(PATHFILE & "\les miserables.txt")
   temp = RemovePunctuation(Book)
   temp = UCase(temp)
   arr = Split(temp, " ")
   Set Dict = CreateObject("Scripting.Dictionary")
   FillDictionary Dict, arr
   Erase arr
   SortDictByFreq Dict, arr
   DisplayTheTopMostUsedWords arr, 10

Debug.Print "Words different in this book : " & Dict.Count
Debug.Print "-------------------------"
Debug.Print ""
Debug.Print "Optionally : "
Debug.Print "Frequency of the word MISERABLE : " & DisplayFrequencyOf("MISERABLE", Dict)
Debug.Print "Frequency of the word DISASTER : " & DisplayFrequencyOf("DISASTER", Dict)
Debug.Print "Frequency of the word ROSETTA_CODE : " & DisplayFrequencyOf("ROSETTA_CODE", Dict)
Debug.Print "-------------------------"
Debug.Print "Execution Time : " & Format(Timer - T, "0.000") & " sec."
End Sub

Private Function ExtractTxt(strFile As String) As String
'http://rosettacode.org/wiki/File_input/output#VBA
Dim i As Integer
   i = FreeFile
   Open strFile For Input As #i
       ExtractTxt = Input(LOF(1), #i)
   Close #i
End Function

Private Function RemovePunctuation(strBook As String) As String
Dim T, i As Integer, temp As String
Const PUNCT As String = """,;:!?."
   T = Split(StrConv(PUNCT, vbUnicode), Chr(0))
   temp = strBook
   For i = LBound(T) To UBound(T) - 1
      temp = Replace(temp, T(i), " ")
   Next
   temp = Replace(temp, "--", " ")
   temp = Replace(temp, "...", " ")
   temp = Replace(temp, vbCrLf, " ")
   RemovePunctuation = Replace(temp, "  ", " ")
End Function

Private Sub FillDictionary(d As Object, a As Variant)
Dim L As Long
   For L = LBound(a) To UBound(a)
      If a(L) <> "" Then _
         d(a(L)) = d(a(L)) + 1
   Next
End Sub

Private Sub SortDictByFreq(d As Object, myArr As Variant)
Dim K
Dim L As Long
   ReDim myArr(1 To d.Count, 1 To 2)
   For Each K In d.keys
      L = L + 1
      myArr(L, 1) = K
      myArr(L, 2) = CLng(d(K))
   Next
   SortArray myArr, LBound(myArr), UBound(myArr), 2
End Sub

Private Sub SortArray(a, Le As Long, Ri As Long, Col As Long)
Dim ref As Long, L As Long, r As Long, temp As Variant
   ref = a((Le + Ri) \ 2, Col)
   L = Le
   r = Ri
   Do
         Do While a(L, Col) < ref
            L = L + 1
         Loop
         Do While ref < a(r, Col)
            r = r - 1
         Loop
         If L <= r Then
            temp = a(L, 1)
            a(L, 1) = a(r, 1)
            a(r, 1) = temp
            temp = a(L, 2)
            a(L, 2) = a(r, 2)
            a(r, 2) = temp
            L = L + 1
            r = r - 1
         End If
   Loop While L <= r
   If L < Ri Then SortArray a, L, Ri, Col
   If Le < r Then SortArray a, Le, r, Col
End Sub

Private Sub DisplayTheTopMostUsedWords(arr As Variant, Nb As Long)
Dim L As Long, i As Integer
   i = 1
   Debug.Print "Rank Word    Frequency"
   Debug.Print "
### = ======= ======
"
   For L = UBound(arr) To UBound(arr) - Nb + 1 Step -1
      Debug.Print Left(CStr(i) & "    ", 5) & Left(arr(L, 1) & "       ", 8) & " " & Format(arr(L, 2), "0 000")
      i = i + 1
   Next
End Sub

Private Function DisplayFrequencyOf(Word As String, d As Object) As Long
   If d.Exists(Word) Then _
      DisplayFrequencyOf = d(Word)
End Function
```

{{out}}

```txt
Words different in this book : 25884
-------------------------
Rank Word    Frequency

### = ======= ======

1    THE      40 831
2    OF       19 807
3    AND      14 860
4    A        14 453
5    TO       13 641
6    IN       11 133
7    HE       9 598
8    WAS      8 617
9    THAT     7 807
10   IT       6 517

Optionally : 
Frequency of the word MISERABLE : 35
Frequency of the word DISASTER : 12
Frequency of the word ROSETTA_CODE : 0
-------------------------
Execution Time : 7,785 sec.
```



## zkl


```zkl
fname,count := vm.arglist;	// grab cammand line args

   // words may have leading or trailing "_", ie "the" and "_the"
File(fname).pump(Void,"toLower",  // read the file line by line and hash words
   RegExp("[a-z]+").pump.fp1(Dictionary().incV))  // line-->(word:count,..)
.toList().copy().sort(fcn(a,b){ b[1]<a[1] })[0,count.toInt()] // hash-->list
.pump(String,Void.Xplode,"%s,%s\n".fmt).println();
```

{{out}}

```txt

$ zkl bbb ~/Documents/Les\ Miserables.txt 10
the,41089
of,19949
and,14942
a,14608
to,13951
in,11214
he,9648
was,8621
that,7924
it,6661

```

