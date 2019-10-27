+++
title = "Semordnilap"
description = ""
date = 2019-05-04T13:41:41Z
aliases = []
[extra]
id = 12294
[taxonomies]
categories = []
tags = []
+++

{{task}}

A [[wp:semordnilap|semordnilap]] is a word (or phrase) that spells a different word (or phrase) backward. 
"Semordnilap" is a word that itself is a semordnilap.

Example: ''lager'' and ''regal''


 
;Task
Using only words from <u>[http://wiki.puzzlers.org/pub/wordlists/unixdict.txt this list]</u>, report the total number of unique semordnilap pairs, and print 5 examples. (Note that lager/regal and regal/lager should be counted as one unique pair.)


 
;Related tasks
* [[Palindrome_detection|Palindrome detection]]





## 8th

We're using a map to keep track of what's been seen, and an array to store the results.  We load the "unixdict.txt" as an "asset", meaning a file stored alongside the program code:

```Forth

[] var, results

: processline \ m s --
  clone nip
  tuck s:rev
  m:exists? if 
    results @ rot a:push drop
  else
    swap true m:!
  then ;

{} "unixdict.txt" app:asset >s
' processline s:eachline 

results @ dup a:len . " pairs" . cr
a:shuffle
( a:shift dup .  " is the reverse of " . s:rev . cr ) 5 times bye

```


{{out}}

```txt

158 pairs
trap is the reverse of part
nab is the reverse of ban
la is the reverse of al
ta is the reverse of at
tin is the reverse of nit

```



## Ada


Before tackling the real problem, we specify a package String_Vectors and a class String_Vectors.Vec, to store the list of words in the dictionary:


```Ada
with Ada.Containers.Indefinite_Vectors, Ada.Text_IO;

package String_Vectors is

   package String_Vec is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => String);

   type Vec is new String_Vec.Vector with null record;

   function Read(Filename: String) return Vec;
     -- uses Ada.Text_IO to read words from the given file into a Vec
     -- requirement: each word is written in a single line

   function Is_In(List: Vec;
                  Word: String;
                  Start: Positive; Stop: Natural) return Boolean;
     -- checks if Word is in List(Start .. Stop);
     -- requirement: the words in List are sorted alphabetically
end String_Vectors;
```


The specified class String_Vectors.Vec has been derived from Ada.Containers.Indefinite_Vectors.Vector and provides two additional primitive operations Read and Is_In. Here is the implementation:


```Ada
package body String_Vectors is

   function Is_In(List: Vec;
                  Word: String;
                  Start: Positive; Stop: Natural) return Boolean is
      Middle: Positive;
   begin
      if Start > Stop then
         return False;
      else
         Middle := (Start+Stop) / 2;
            if List.Element(Middle) = Word then
               return True;
            elsif List.Element(Middle) < Word then
               return List.Is_In(Word, Middle+1, Stop);
            else
               return List.Is_In(Word, Start, Middle-1);
            end if;
      end if;
   end Is_In;

   function Read(Filename: String) return Vec is
      package IO renames Ada.Text_IO;
      Persistent_List: IO.File_Type;
      List: Vec;
   begin
      IO.Open(File => Persistent_List, Name => Filename, Mode => IO.In_File);
      while not IO.End_Of_File(Persistent_List) loop
         List.Append(New_Item => IO.Get_Line(Persistent_List));
      end loop;
      IO.Close(Persistent_List);
      return List;
   end Read;

end String_Vectors;
```


This is the main program:


```Ada
with String_Vectors, Ada.Text_IO, Ada.Command_Line;

procedure Semordnilap is

   function Backward(S: String) return String is
   begin
      if S'Length < 2 then
         return S;
      else
         return (S(S'Last) & Backward(S(S'First+1 .. S'Last-1)) & S(S'First));
      end if;
   end Backward;

   W: String_Vectors.Vec := String_Vectors.Read(Ada.Command_Line.Argument(1));

   Semi_Counter: Natural := 0;

begin
   for I in W.First_Index .. W.Last_Index loop
      if W.Element(I) /= Backward(W.Element(I)) and then
        W.Is_In(Backward(W.Element(I)), W.First_Index, I) then
         Semi_Counter := Semi_Counter + 1;
         if Semi_Counter <= 5 then
            Ada.Text_IO.Put_Line(W.Element(I) & " - " & Backward(W.Element(I)));
         end if;
      end if;
   end loop;
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put("pairs found:" & Integer'Image(Semi_Counter));
end Semordnilap;
```


{{out}}

```txt
>./semordnilap unixdict.txt 
ca - ac
dab - bad
diva - avid
dna - and
drab - bard

pairs found: 158
```



## Aime


```aime
integer p, z;
record r;
file f;
text s, t;

f.affix("unixdict.txt");

p = 0;

while (f.line(s) != -1) {
    if (r_o_integer(z, r, t = b_reverse(s))) {
        p += 1;
        if (p <= 5) {
            o_(s, " ", t, "\n");
        }
    }

    r[s] = 0;
}

o_form("Semordnilap pairs: ~\n", p);
```

{{out}}

```txt
ca ac
dab bad
diva avid
dna and
drab bard
Semordnilap pairs: 158
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}}
The Algol 68 G "read" PRAGMA is used to include the associative array code from Associative_array/Iteration. 

```algol68
# find the semordnilaps in a list of words                           #
# use the associative array in the Associate array/iteration task    #
PR read "aArray.a68" PR

# returns text with the characters reversed                          #
OP REVERSE = ( STRING text )STRING:
     BEGIN
         STRING reversed  := text;
         INT    start pos := LWB text;
         FOR end pos FROM UPB reversed BY -1 TO LWB reversed
         DO
             reversed[ end pos ] := text[ start pos ];
             start pos +:= 1
         OD;
         reversed
    END # REVERSE # ;

# read the list of words and store the words in an associative array #
# check for semordnilaps                                             #
IF  FILE input file;
    STRING file name = "unixdict.txt";
    open( input file, file name, stand in channel ) /= 0
THEN
    # failed to open the file #
    print( ( "Unable to open """ + file name + """", newline ) )
ELSE
    # file opened OK #
    BOOL at eof := FALSE;
    # set the EOF handler for the file #
    on logical file end( input file, ( REF FILE f )BOOL:
                                     BEGIN
                                         # note that we reached EOF on the #
                                         # latest read #
                                         at eof := TRUE;
                                         # return TRUE so processing can continue #
                                         TRUE
                                     END
                       );
    REF AARRAY words := INIT LOC AARRAY;
    STRING word;
    INT    semordnilap count := 0;
    WHILE NOT at eof
    DO
        STRING word;
        get( input file, ( word, newline ) );
        STRING reversed word = REVERSE word;
        IF ( words // reversed word ) = ""
        THEN
            # the reversed word isn't in the array                   #
            words // word := reversed word
        ELSE
            # we already have this reversed - we have a semordnilap  #
            semordnilap count +:= 1;
            IF semordnilap count <= 5
            THEN
                print( ( reversed word, " & ", word, newline ) )
           FI
        FI
    OD;
    close( input file );
    print( ( whole( semordnilap count, 0 ), " semordnilaps found", newline ) ) 
FI
```

{{out}}

```txt

ac & ca
bad & dab
avid & diva
and & dna
bard & drab
158 semordnilaps found

```



## AutoHotkey

{{works with|AutoHotkey_L}}

```AutoHotkey
S := [], M := []

FileRead, dict, unixdict.txt
Loop, Parse, dict, `n, `r`n
{
	r := Reverse(A_LoopField)
	if (S[r])
		M.Insert(r " / " A_LoopField)
	else
		S[A_LoopField] := 1
}

Loop, 5
	Out .= "`t" M[A_Index] "`n"

MsgBox, % "5 Examples:`n" Out "`nTotal Pairs:`n`t" M.MaxIndex()

Reverse(s) {
	Loop, Parse, s
		r := A_LoopField . r
	return r
}
```

{{out}}

```txt
5 Examples:
	ac / ca
	bad / dab
	avid / diva
	and / dna
	bard / drab

Total Pairs:
	158
```



## AWK


```AWK

# syntax: GAWK -f SEMORDNILAP.AWK unixdict.txt
{ arr[$0]++ }
END {
    PROCINFO["sorted_in"] = "@ind_str_asc"
    for (word in arr) {
      rword = ""
      for (j=length(word); j>0; j--) {
        rword = rword substr(word,j,1)
      }
      if (word == rword) { continue } # palindrome
      if (rword in arr) {
        if (word in shown || rword in shown) { continue }
        shown[word]++
        shown[rword]++
        if (n++ < 5) { printf("%s %s\n",word,rword) }
      }
    }
    printf("%d words\n",n)
    exit(0)
}

```

{{out}}

```txt

able elba
abut tuba
ac ca
ah ha
al la
158 words

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      INSTALL @lib$+"SORTLIB"
      Sort% = FN_sortinit(0,0)
      
      DIM dict$(26000*2)
      
      REM Load the dictionary, eliminating palindromes:
      dict% = OPENIN("C:\unixdict.txt")
      IF dict%=0 ERROR 100, "No dictionary file"
      index% = 0
      REPEAT
        A$ = GET$#dict%
        B$ = FNreverse(A$)
        IF A$<>B$ THEN
          dict$(index%) = A$
          dict$(index%+1) = B$
          index% += 2
        ENDIF
      UNTIL EOF#dict%
      CLOSE #dict%
      Total% = index%
      
      REM Sort the dictionary:
      C% = Total%
      CALL Sort%, dict$(0)
      
      REM Find semordnilaps:
      pairs% = 0
      examples% = 0
      FOR index% = 0 TO Total%-2
        IF dict$(index%)=dict$(index%+1) THEN
          IF examples%<5 IF LEN(dict$(index%))>4 THEN
            PRINT dict$(index%) " " FNreverse(dict$(index%))
            examples% += 1
          ENDIF
          pairs% += 1
        ENDIF
      NEXT
      
      PRINT "Total number of unique pairs = "; pairs%/2
      END
      
      DEF FNreverse(A$)
      LOCAL I%, L%, P%
      IF A$="" THEN =""
      L% = LENA$ - 1
      P% = !^A$
      FOR I% = 0 TO L% DIV 2
        SWAP P%?I%, L%?(P%-I%)
      NEXT
      = A$
```

{{out}}

```txt

damon nomad
kramer remark
lager regal
leper repel
lever revel
Total number of unique pairs = 158

```



## Bracmat


```Bracmat
( get'("unixdict.txt",STR):?dict
& new$hash:?H
& 0:?p
& ( @( !dict
     :   ?
         ( [!p ?w \n [?p ?
         & (H..insert)$(!w.rev$!w)
         & ~
         )
     )
  |   0:?N
    &   (H..forall)
      $ (
        =
          .     !arg:(?a.?b)
              & !a:<!b
              & (H..find)$!b
              & !N+1:?N:<6
              & out$(!a !b)
            |
        )
    & out$(semordnilap !N dnuoF)
  )
);
```

{{out}}

```txt
tv vt
ir ri
ac ca
eh he
ku uk
semordnilap 158 dnuoF
```



## C



```C>#include <stdio.h

#include <stdlib.h>
#include <alloca.h> /* stdlib.h might not have obliged. */
#include <string.h>

static void reverse(char *s, int len)
{
    int i, j;
    char tmp;

    for (i = 0, j = len - 1; i < len / 2; ++i, --j)
        tmp = s[i], s[i] = s[j], s[j] = tmp;
}

/* Wrap strcmp() for qsort(). */
static int strsort(const void *s1, const void *s2)
{
    return strcmp(*(char *const *) s1, *(char *const *) s2);
}

int main(void)
{
    int i, c, ct = 0, len, sem = 0;
    char **words, **drows, tmp[24];
    FILE *dict = fopen("unixdict.txt", "r");

    /* Determine word count. */
    while ((c = fgetc(dict)) != EOF)
        ct += c == '\n';
    rewind(dict);

    /* Using alloca() is generally discouraged, but we're not doing
     * anything too fancy and the memory gains are significant. */
    words = alloca(ct * sizeof words);
    drows = alloca(ct * sizeof drows);

    for (i = 0; fscanf(dict, "%s%n", tmp, &len) != EOF; ++i) {
        /* Use just enough memory to store the next word. */
        strcpy(words[i] = alloca(len), tmp);

        /* Store it again, then reverse it. */
        strcpy(drows[i] = alloca(len), tmp);
        reverse(drows[i], len - 1);
    }

    fclose(dict);
    qsort(drows, ct, sizeof drows, strsort);

    /* Walk both sorted lists, checking only the words which could
     * possibly be a semordnilap pair for the current reversed word. */
    for (c = i = 0; i < ct; ++i) {
        while (strcmp(drows[i], words[c]) > 0 && c < ct - 1)
            c++;
        /* We found a semordnilap. */
        if (!strcmp(drows[i], words[c])) {
            strcpy(tmp, drows[i]);
            reverse(tmp, strlen(tmp));
            /* Unless it was a palindrome. */
            if (strcmp(drows[i], tmp) > 0 && sem++ < 5)
                printf("%s\t%s\n", drows[i], tmp);
        }
    }

    printf("Semordnilap pairs: %d\n", sem);
    return 0;
}
```


{{out}}

```txt
ca	ac
dab	bad
diva	avid
dna	and
drab	bard
Semordnilap pairs: 158
```



## C sharp


```csharp
using System;
using System.Net;
using System.Collections.Generic;
using System.Linq;
using System.IO;

public class Semordnilap
{
    public static void Main() {
        var results = FindSemordnilaps("http://www.puzzlers.org/pub/wordlists/unixdict.txt").ToList();
        Console.WriteLine(results.Count);
        var random = new Random();
        Console.WriteLine("5 random results:");
        foreach (string s in results.OrderBy(_ => random.Next()).Distinct().Take(5)) Console.WriteLine(s + " " + Reversed(s));
    }
    
    private static IEnumerable<string> FindSemordnilaps(string url) {
        var found = new HashSet<string>();
        foreach (string line in GetLines(url)) {
            string reversed = Reversed(line);
            //Not taking advantage of the fact the input file is sorted
            if (line.CompareTo(reversed) != 0) {
                if (found.Remove(reversed)) yield return reversed;
                else found.Add(line);
            }
        }
    }
    
    private static IEnumerable<string> GetLines(string url) {
        WebRequest request = WebRequest.Create(url);
        using (var reader = new StreamReader(request.GetResponse().GetResponseStream(), true)) {
            while (!reader.EndOfStream) {
                yield return reader.ReadLine();
            }
        }
    }
    
    private static string Reversed(string value) => new string(value.Reverse().ToArray());
}
```

{{out}}

```txt

158
5 random results:
keep peek
lever revel
ix xi
avid diva
gar rag
```



## C++


```cpp>#include <fstream

#include <iostream>
#include <set>
#include <string>

int main()
{
    std::ifstream input("unixdict.txt");
    if (!input) {
        return 1; // couldn't open input file
    }

    std::set<std::string> words; // previous words
    std::string word; // current word
    size_t count = 0; // pair count

    while (input >> word) {
        std::string drow(word.rbegin(), word.rend()); // reverse
        if (words.find(drow) == words.end()) { // pair not found
            words.insert(word);
        } else { // pair found
            if (count < 5) {
                std::cout << word << ' ' << drow << '\n';
            }
            ++count;
        }
    }
    std::cout << "\nSemordnilap pairs: " << count << '\n';
}
```

{{out}}

```txt

ca ac
dab bad
diva avid
dna and
drab bard

Semordnilap pairs: 158

```



## Clojure


```clojure
(ns rosettacode.semordnilaps
  (:require [clojure.string  :as str])
            [clojure.java.io :as io ]))

(def dict-file
  (or (first *command-line-args*) "unixdict.txt"))

(def dict (-> dict-file io/reader line-seq set))

(defn semordnilap? [word] 
  (let [rev (str/reverse word)] 
    (and (not= word rev) (dict rev))))

(def semordnilaps
  (->> dict
       (filter semordnilap?)
       (map #([% (str/reverse %)]))
       (filter (fn [[x y]] (<= (compare x y) 0)))))

(printf "There are %d semordnilaps in %s.  Here are 5:\n" 
  (count semordnilaps) 
  dict-file)

(dorun (->> semordnilaps shuffle (take 5) sort (map println)))
```


{{out}}

```txt
There are 158 semordnilaps in unixdict.txt.  Here are 5:
[bog gob]
[gnaw wang]
[it ti]
[los sol]
[mot tom]
```



## Common Lisp


```lisp
(defun semordnilaps (word-list)
  (let ((word-map (make-hash-table :test 'equal)))
    (loop for word in word-list do
         (setf (gethash word word-map) t))
    (loop for word in word-list
       for rword = (reverse word)
       when (and (string< word rword) (gethash rword word-map))
       collect (cons word rword))))

(defun main ()
  (let ((words
         (semordnilaps
          (with-open-file (s "unixdict.txt")
            (loop for line = (read-line s nil nil)
               until (null line)
               collect (string-right-trim #(#\space #\return #\newline) line))))))
    (format t "Found pairs: ~D" (length words))
    (loop for x from 1 to 5
       for word in words
       do (print word)))
  (values))
```

{{out}}

```txt
* (main)
Found pairs: 158
("able" . "elba")
("abut" . "tuba")
("ac" . "ca")
("ah" . "ha")
("al" . "la")
```



## D


### Simple Imperative Version


```d
void main() {
    import std.stdio, std.file, std.string, std.algorithm;

    bool[string] seenWords;
    size_t pairCount = 0;

    foreach (const word; "unixdict.txt".readText.toLower.splitter) {
        //const drow = word.dup.reverse();
        auto drow = word.dup;
        drow.reverse();
        if (drow in seenWords) {
            if (pairCount++ < 5)
                writeln(word, " ", drow);
        } else
            seenWords[word] = true;
    }

    writeln("\nSemordnilap pairs: ", pairCount);
}
```

{{out}}

```txt
ca ac
dab bad
diva avid
dna and
drab bard

Semordnilap pairs: 158
```



### A More Functional Version


```d
void main() {
    import std.stdio, std.file, std.algorithm, std.string, std.range;

    auto words = "unixdict.txt".readText.split.zip(0.repeat).assocArray;
    auto pairs = zip(words.byKey, words.byKey.map!(w => w.dup.reverse))
                 .filter!(wr => wr[0] < wr[1] && wr[1] in words)
                 .zip(0.repeat).assocArray;
    writeln(pairs.length, "\n", pairs.byKey.take(5));
}
```

{{out}}

```txt
158
[Tuple!(string, char[])("bag", "gab"), Tuple!(string, char[])("pat", "tap"), Tuple!(string, char[])("avis", "siva"), Tuple!(string, char[])("haw", "wah"), Tuple!(string, char[])("rot", "tor")]
```



## EchoLisp

We use the '''words''' library, and the french dictionary delivered with EchoLisp.


```scheme

(lib 'struct)
(lib 'sql)
(lib 'words)

(lib 'dico.fr.no-accent) ;; load dictionary
(string-delimiter "")

;; check reverse r of w is a word
;; take only one pair : r < w
(define (semordnilap? w)
	    (define r (list->string (reverse (string->list w))))
	    (and (word? r) (string<? r w)))
	    
;; to get longest first
(define (string-sort a b) (> (string-length a) (string-length b)))
	    
(define (task)
;; select unique words into the list 'mots'
    (define mots (make-set (words-select #:any null 999999)))
    (define semordnilap 
	    (list-sort string-sort (for/list ((w mots))
	    #:when (semordnilap? w)
	    w )))
   (writeln 'pairs '→ (length semordnilap))
   (writeln 'longest '→ (take semordnilap 5)))

{{out}}
(task)
    pairs       →     345    
    longest     →     (rengager tresser strasse reveler retrace)    


```



## Eiffel

First the programm reads the wordlist into an array. 
Then it mirrors each word and searchs for it across the array using binary search. 

```Eiffel

class
	SEMORDNILAP

create
	make

feature

	make
			--Semordnilaps in 'solution'.
		local
			count, i, middle, upper, lower: INTEGER
			reverse: STRING
		do
			read_wordlist
			create solution.make_empty
			from
				i := 1
			until
				i > word_array.count
			loop
				word_array [i].mirror
				reverse := word_array [i]
				from
					lower := i + 1
					upper := word_array.count
				until
					lower >= upper
				loop
					middle := (upper - lower) // 2 + lower
					if reverse.same_string (word_array [middle]) then
						count := count + 1
						upper := 0
						lower := 1
						solution.force (word_array [i], count)
					elseif reverse.is_less (word_array [middle]) then
						upper := middle - 1
					else
						lower := middle + 1
					end
				end
				if lower < word_array.count and then reverse.same_string (word_array [lower]) then
					count := count + 1
					upper := 0
					lower := 1
					solution.force (word_array [i], count)
				end
				i := i + 1
			end
		end

	solution: ARRAY [STRING]

	original_list: STRING = "unixdict.txt"


feature {NONE}

	read_wordlist
			-- Preprocessed word_array for finding Semordnilaps.
		local
			l_file: PLAIN_TEXT_FILE
			wordlist: LIST [STRING]
		do
			create l_file.make_open_read_write (original_list)
			l_file.read_stream (l_file.count)
			wordlist := l_file.last_string.split ('%N')
			l_file.close
			create word_array.make_empty
			across
				1 |..| wordlist.count as i
			loop
				word_array.force (wordlist.at (i.item), i.item)
			end
		end

	word_array: ARRAY [STRING]

end

```

Test: 

```Eiffel

class
	APPLICATION

create
	make

feature

	make
		local
			test: ARRAY [STRING]
			s: STRING
		do
			create se.make
			test := se.solution
			create sort.sort (test)
			across
				test.subarray (1, 5) as t
			loop
				s := t.item
				io.put_string (t.item + "%T")
				s.mirror
				io.put_string (s)
				io.new_line
			end
			io.put_string ("Total number of semordnilaps: ")
			io.put_integer (test.count)
		end

	se: SEMORDNILAP

	sort: MERGE_SORT [STRING]

end

```

{{out}}

```txt

ca      ac
dab     bad
diva    avid
dna     and
drab    bard
Total number of semordnilaps: 158

```



## Elixir


```elixir
words = File.stream!("unixdict.txt")
        |> Enum.map(&String.strip/1)
        |> Enum.group_by(&min(&1, String.reverse &1))
        |> Map.values
        |> Enum.filter(&(length &1) == 2)
IO.puts "Semordnilap pair: #{length(words)}"
IO.inspect Enum.take(words,5)
```


{{out}}

```txt

Semordnilap pair: 158
[["dab", "bad"], ["drib", "bird"], ["marc", "cram"], ["soma", "amos"],
 ["tab", "bat"]]

```



## Erlang

{{trans|Clojure}}

```erlang
#!/usr/bin/env escript
main([]) -> main(["unixdict.txt"]);

main([DictFile]) -> 
  Dict = sets:from_list(read_lines(DictFile)),
  Semordnilaps = 
    lists:filter(fun([W,R]) -> W < R end,
      lists:map(fun(W) -> [W, lists:reverse(W)] end, 
        semordnilaps(Dict))),
  io:fwrite("There are ~b semordnilaps in ~s~n",
            [length(Semordnilaps), DictFile]),
  lists:map(fun([W,R]) -> io:fwrite("~s/~s~n", [W, R]) end,
            lists:sort(lists:sublist(shuffle(Semordnilaps),1,5))).

read_lines(Filename) when is_list(Filename) ->
  { ok, File } = file:open(Filename, [read]),
  read_lines(File);

read_lines(File) when is_pid(File) ->
   case file:read_line(File) of
    {ok, Data} -> [chop(Data) | read_lines(File)];
    eof        -> []
   end.

is_semordnilap(Word, Dict) -> 
  Rev = lists:reverse(Word),
  sets:is_element(Word, Dict) and sets:is_element(Rev, Dict).

semordnilaps(Dict) -> 
  lists:filter(fun(W) -> is_semordnilap(W, Dict) end, sets:to_list(Dict)).

shuffle(List) -> 
  [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- List])].

chop(L) -> [_|T] = lists:reverse(L), lists:reverse(T).
```


{{out}}

```txt
There are 158 semordnilaps in unixdict.txt
aryl/lyra
caw/wac
cram/marc
dine/enid
dual/laud
```


=={{header|F_Sharp|F#}}==
Using a mutable dictionary.

```fsharp
open System

let seen = new System.Collections.Generic.Dictionary<string,bool>()

let lines = System.IO.File.ReadLines("unixdict.txt")

let sems = seq {
    for word in lines do
        let drow = new String(Array.rev(word.ToCharArray()))
        if fst(seen.TryGetValue(drow)) then yield (drow, word)
        seen.[drow] <- true
        seen.[word] <- true
}

let s = Seq.toList sems
printfn "%d" s.Length
for i in 0 .. 4 do printfn "%A" s.[i]
```

{{out}}

```txt
158
("ac", "ca")
("bad", "dab")
("avid", "diva")
("and", "dna")
("bard", "drab")
```



## Factor


```factor
USING: assocs combinators.short-circuit formatting
io.encodings.utf8 io.files kernel literals locals make
prettyprint random sequences ;
IN: rosetta-code.semordnilap

CONSTANT: words $[ "unixdict.txt" utf8 file-lines ]

: semordnilap? ( str1 str2 -- ? )
    { [ = not ] [ nip words member? ] } 2&& ;

[
    [let
        V{ } clone :> seen words
        [
            dup reverse 2dup
            { [ semordnilap? ] [ drop seen member? not ] } 2&&
            [ 2dup [ seen push ] bi@ ,, ] [ 2drop ] if 
        ] each
    ]
] H{ } make >alist
[ length "%d semordnilap pairs.\n" printf ] [ 5 sample . ] bi
```

{{out}}

```txt

158 semordnilap pairs.
{
    { "pan" "nap" }
    { "lac" "cal" }
    { "tang" "gnat" }
    { "wolf" "flow" }
    { "mac" "cam" }
}

```



## Forth


This code uses a Forth wordlist to contain the dictionary, and uses the Forth-2012 TRAVERSE-WORDLIST to walk through it (a simpler way would be to check for the presence of the reversed word when putting the word into the wordlist).

One interesting issue is how I get each pair only once and exclude
palindromes: I accept only pairs where nt<nt2.  A type checking bigot
will likely argue that nts should not be compared with <, because they
are opaque data types.  But their implementation does not matter for
this check: Whatever bit patterns these two nts get, either it's the
same nt, then nt<nt2 will return false, as desired; and if they are
different, exactly one of nt<nt2 and nt2<nt will return true.

The code uses two Gforth-specific words: EXECUTE-PARSING (implementable in standard Forth, but not easy) for allowing to provide the name of the defined word on the stack; and FIND-NAME-IN to look up the reversed word (could be replaced with a use of the standard SEARCH-WORDLIST, but the code would become a little more complicated).


```forth

wordlist constant dict

: load-dict ( c-addr u -- )
    r/o open-file throw >r
    begin
	pad 1024 r@ read-line throw while
	    pad swap ['] create execute-parsing
    repeat
    drop r> close-file throw ;

: xreverse {: c-addr u -- c-addr2 u :}
    u allocate throw u + c-addr swap over u + >r begin ( from to r:end)
	over r@ u< while
	    over r@ over - x-size dup >r - 2dup r@ cmove
	    swap r> + swap repeat
    r> drop nip u ;

: .example ( c-addr u u1 -- )
    5 < if
	cr 2dup type space 2dup xreverse 2dup type drop free throw then
    2drop ;

: nt-semicheck ( u1 nt -- u2 f )
    dup >r name>string xreverse 2dup dict find-name-in dup if ( u1 c-addr u nt2) 
	r@ < if ( u1 c-addr u ) \ count pairs only once and not palindromes
	    2dup 4 pick .example
	    rot 1+ -rot then
    else
	drop then
    drop free throw r> drop true ;

get-current dict set-current s" unixdict.txt" load-dict set-current

0 ' nt-semicheck dict traverse-wordlist cr .
cr bye

```


{{out}}

```txt

suez zeus
paz zap
way yaw
pay yap
may yam
158 

```



## Fortran

Please read the comments at the beginning of the f90 source to see the compilation instructions and output of 5 random words from a run.  
Note that program Semordnilap opens the file unixdict.txt .  
It does not read from stdin, hence the command line redirection from unixdict.txt is irrelevant.  I haven't bothered to change it.



```fortran

!-*- mode: compilation; default-directory: "/tmp/" -*-
!Compilation started at Sun May 19 21:50:08
!
!a=./F && make $a && $a < unixdict.txt
!f95 -Wall -ffree-form F.F -o F
! 5 of          158 semordnilaps            
!yaw                     
!room                    
!xi                      
!tim                     
!nova                    
!
!
!Compilation finished at Sun May 19 21:50:08
!
!
!
!
!                       unixdict.txt information
! wc -l unixdict.txt                                         #--> 25104                    25 thousand entries
! gawk 'length(a)<length($0){a=$0}END{print a}' unixdict.txt #--> electroencephalography   longest word has 22 characters
! gawk '/[A-Z]/{++a}END{print a}' unixdict.txt               #--> <empty>                  the dictionary is lower case
! sort unixdict.txt | cmp - unixdict.txt                     #--> - unixdict.txt differ: byte 45, line 12
!                                                                                          the dictionary is unsorted
!     mmmmm the dictionary is sorted, according to subroutine bs.  There's something about the ampersands within unixdict.txt I misunderstand.

program Semordnilap
  implicit none
  integer :: i, ios, words, swords
  character(len=24), dimension(32768) :: dictionary, backword
  real, dimension(5) :: harvest
  ! read the dictionary
  open(7,file='unixdict.txt')
  do words = 1, 32768
    read(7, '(a)', iostat = ios) dictionary(words)
    if (ios .ne. 0) exit
  enddo
  close(7)
  if (iachar(dictionary(words)(1:1)) .eq. 0) words = words-1
  ! sort the dictionary
  call bs(dictionary, words)
  !do i = 1, words
  !  write(6,*) dictionary(i)(1:len_trim(dictionary(i))) ! with which we determine the dictionary was ordered
  !enddo
  swords = 0
  do i = 1, words
    call reverse(dictionary(i), backword(swords+1))
    if ((binary_search(dictionary, words, backword(swords+1))) &      !     the reversed word is in the dictionary
      .and. (.not. binary_search(backword, swords, dictionary(i))) &  ! and it's new
      .and. (dictionary(i) .ne. backword(swords+1))) then             ! and it's not a palindrome
      swords = swords + 1
      call bs(backword, swords)
    endif
  enddo
  call random_number(harvest)
  call reverse('spalindromes', backword(swords+1))
  write(6, *) '5 of ', swords, backword(swords+1)
  write(6,'(5(a/))') (backword(1+int(harvest(i)*(swords-2))), i=1,5)

contains

  subroutine reverse(inp, outp)
    character(len=*), intent(in) :: inp
    character(len=*), intent(inout) :: outp
    integer :: k, L
    L = len_trim(inp)
    do k = 1, L
      outp(L+1-k:L+1-k) = inp(k:k)
    enddo
    do k = L+1, len(outp)
      outp(k:k) = ' '
    enddo
  end subroutine reverse

  subroutine bs(a, n) ! ok, despite having claimed that bubble sort should be unceremoniously buried, I'll use it anyway because I expect the dictionary is nearly ordered.  It's also not a terrible sort for less than 5 items.
    ! Please note, I tested bs using unixdict.txt randomized with sort --random .
    character(len=*),dimension(*),intent(inout) :: a
    integer, intent(in) :: n
    integer :: i, j, k
    logical :: done
    character(len=1) :: t
    do i=n-1, 1, -1
      done = .true.
      do j=1, i
        if (a(j+1) .lt. a(j)) then
          done = .false.
          do k = 1, max(len_trim(a(j+1)), len_trim(a(j)))
            t = a(j+1)(k:k)
            a(j+1)(k:k) = a(j)(k:k)
            a(j)(k:k) = t(1:1)
          enddo
        endif
      enddo
      if (done) return
    enddo
  end subroutine bs

  logical function binary_search(source, n, target)
    character(len=*),dimension(*),intent(in) :: source
    character(len=*),intent(in) :: target
    integer, intent(in) :: n
    integer :: a,m,z
    a = 1
    z = n
    do while (a .lt. z)
      m = a + (z - a) / 2
      if (target .lt. source(m)) then
        z = m-1
      else
        if (m .eq. a) exit
        a = m
      endif
    enddo
    binary_search = (target .eq. source(a)) .or. (target .eq. source(z))
  end function binary_search

end program Semordnilap

```



## FreeBASIC


```FreeBASIC
' version 20-06-2015
' compile with: fbc -s console

Function reverse(norm As String) As String

    Dim As String rev
    Dim As Integer i, l = Len(norm) -1

    rev = norm
    For i = 0 To l
        rev[l-i] = norm[i]
    Next

    Return rev

End Function

' ------=< MAIN >=------

Dim As Integer i, j, count, amount, ff = FreeFile
Dim As String in_str, rev, big = " "  ' big needs to start with a space
Dim As String norm(27000), result(270, 2)

Print
Print "Start reading unixdict.txt";

Open "unixdict.txt" For Input As #ff

While Not Eof(ff)                  ' read to end of file

    Line Input #ff, in_str         ' get line = word
    in_str = Trim(in_str)          ' we don't want spaces
    If Len(in_str) > 1 Then        ' if length > 1 then reverse
        rev = reverse(in_str)
        If in_str <> rev Then      ' if in_str is not a palingdrome
            count = count + 1      ' increase counter
            norm(count) = in_str   ' store in the array
            big = big + rev + " "  ' create big string with reversed words
        End If
    End If

Wend

Close #ff

Print " ... Done"
Print : Print "Start looking for semordnilap"

For i = 1 To count
    For j = 1 To amount ' check to avoid the double
        If result(j, 2) = norm(i) Then Continue For, For
    Next
    j = InStr(big, " " + norm(i) + " ")
    If j <> 0 Then                          ' found one
        amount = amount + 1                 ' increase counter
        result(amount,1) = norm(i)          ' store normal word
        result(amount,2) = reverse(norm(i)) ' store reverse word
    End If
Next

Print : Print "Found"; amount; " unique semordnilap pairs"
Print : Print "Display 5 semordnilap pairs"
Print

count = 0
For i = 1 To amount
    If Len(result(i,1)) >= 5 Then
        count = count + 1
        Print result(i, 1), result(i, 2)
        If count >= 5 Then Exit For
    EndIf
Next

Print

' empty keyboard buffer 
While InKey <> "" : Wend
Print : Print "Hit any key to end program"
Sleep
End
```

{{out}}

```txt
Start reading unixdict.txt ... Done

Start looking for semordnilap

Found 158 unique semordnilap pairs

Display 5 semordnilap pairs

damon         nomad
kramer        remark
lager         regal
leper         repel
lever         revel
```



## Go


```go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
    "strings"
)

func main() {
    // read file into memory as one big block
    data, err := ioutil.ReadFile("unixdict.txt")
    if err != nil {
        log.Fatal(err)
    }
    // copy the block, split it up into words
    words := strings.Split(string(data), "\n")
    // optional, free the first block for garbage collection
    data = nil
    // put words in a map, also determine length of longest word
    m := make(map[string]bool)
    longest := 0
    for _, w := range words {
        m[string(w)] = true
        if len(w) > longest {
            longest = len(w)
        }
    }
    // allocate a buffer for reversing words
    r := make([]byte, longest)
    // iterate over word list
    sem := 0
    var five []string
    for _, w := range words {
        // first, delete from map.  this prevents a palindrome from matching
        // itself, and also prevents it's reversal from matching later.
        delete(m, w)
        // use buffer to reverse word
        last := len(w) - 1
        for i := 0; i < len(w); i++ {
            r[i] = w[last-i]
        }
        rs := string(r[:len(w)])
        // see if reversed word is in map, accumulate results
        if m[rs] {
            sem++
            if len(five) < 5 {
                five = append(five, w+"/"+rs)
            }
        }
    }
    // print results
    fmt.Println(sem, "pairs")
    fmt.Println("examples:")
    for _, e := range five {
        fmt.Println("  ", e)
    }
}
```

{{out}}

```txt

158 pairs
examples:
   able/elba
   abut/tuba
   ac/ca
   ah/ha
   al/la

```



## Groovy


```groovy
def semordnilapWords(source) {
    def words = [] as Set
    def semordnilaps = []
    source.eachLine { word ->
        if (words.contains(word.reverse())) semordnilaps << word
        words << word
    }
    semordnilaps
}
```

Test Code

```groovy
def semordnilaps = semordnilapWords(new URL('http://www.puzzlers.org/pub/wordlists/unixdict.txt'))
println "Found ${semordnilaps.size()} semordnilap words"
semordnilaps[0..<5].each { println "$it -> ${it.reverse()}" } 
```

{{out}}

```txt
Found 158 semordnilap words
ca -> ac
dab -> bad
diva -> avid
dna -> and
drab -> bard
```



## Haskell


```haskell
import qualified Data.Set as S

semordnilaps
  :: (Ord a, Foldable t)
  => t [a] -> [[a]]
semordnilaps =
  let f x (s, w)
        | S.member (reverse x) s = (s, x : w)
        | otherwise = (S.insert x s, w)
  in snd . foldr f (S.empty, [])

main :: IO ()
main = do
  s <- readFile "unixdict.txt"
  let l = semordnilaps (lines s)
  print $ length l
  mapM_ (print . ((,) <*> reverse)) $ take 5 (filter ((4 <) . length) l)
```

{{out}}

```txt
158
("damon","nomad")
("kramer","remark")
("lager","regal")
("leper","repel")
("lever","revel")
```


=={{header|Icon}} and {{header|Unicon}}==

The following solution works in both Icon and Unicon:

```unicon
procedure main(a)
    words := set()
    found := 0
    every word := map(!&input) do {
        if member(words, reverse(word)) then {
            if (found +:= 1) <= 5 then write("\t",reverse(word),"/",word)
            }
        else insert(words, word)
        }
    write("\nFound ",found," semordnilap words")
end

```


Sample run with unixdict:

```txt

->smp <unixdict.txt
        ac/ca
        bad/dab
        avid/diva
        and/dna
        bard/drab

Found 158 semordnilap words
->

```



## J


We find all semordnilaps by filtering only words which, when reversed, are a member of the set of dictionary words and are not palindromes. We then find only unique semordnilaps by pairing them with their reversed instance, sorting each pair, and eliminating duplicates pairs:


```j
   isSemordnilap=: |.&.> (~: *. e.) ]
   unixdict=: <;._2 freads 'unixdict.txt'
   #semordnilaps=: ~. /:~"1 (,. |.&.>) (#~ isSemordnilap) unixdict
158
```


We see that there are 158 semordnilaps.

Here's 5 of them, picked arbitrarily:

<lang>   (5?.158) { semordnilaps
┌────┬────┐
│kay │yak │
├────┼────┤
│nat │tan │
├────┼────┤
│avis│siva│
├────┼────┤
│flow│wolf│
├────┼────┤
│caw │wac │
└────┴────┘
```



## Java

{{trans|D}}
{{works with|Java|7+}}

```java5
import java.nio.file.*;
import java.util.*;

public class Semordnilap {

    public static void main(String[] args) throws Exception {
        List<String> lst = Files.readAllLines(Paths.get("unixdict.txt"));
        Set<String> seen = new HashSet<>();
        int count = 0;
        for (String w : lst) {
            w = w.toLowerCase();
            String r = new StringBuilder(w).reverse().toString();
            if (seen.contains(r)) {
                if (count++ < 5)
                    System.out.printf("%-10s %-10s\n", w, r);
            } else seen.add(w);
        }
        System.out.println("\nSemordnilap pairs found: " + count);
    }
}
```


```txt
ca         ac        
dab        bad       
diva       avid      
dna        and       
drab       bard      

Semordnilap pairs found: 158
```



## JavaScript


### Node.js

{{trans|Clojure}}

```javascript
#!/usr/bin/env node
var  fs = require('fs');
var sys = require('sys');

var dictFile = process.argv[2] || "unixdict.txt";

var dict = {};
fs.readFileSync(dictFile)
  .toString()
  .split('\n')
  .forEach(function(word) {
    dict[word] = word.split("").reverse().join("");
  });

function isSemordnilap(word) { return dict[dict[word]]; };

var semordnilaps = []
for (var key in dict) {
  if (isSemordnilap(key)) {
    var rev = dict[key];
    if (key < rev) {
      semordnilaps.push([key,rev]) ;
    }
  }
}

var count = semordnilaps.length;
sys.puts("There are " + count + " semordnilaps in " +
         dictFile + ".  Here are 5:" );

var indices=[]
for (var i=0; i<count; ++i) {
  if (Math.random() < 1/Math.ceil(i/5.0)) {
    indices[i%5] = i 
  }
}
indices.sort()
for (var i=0; i<5; ++i) {
  sys.puts(semordnilaps[indices[i]]);
}
```



### Rhino

{{works with|Rhino|1.7}}


```javascript
#!/usr/bin/env rhino

importPackage (java.io)

var dictFile = arguments[0] || "unixdict.txt";

var reader = new BufferedReader(new FileReader(dictFile));
var dict = {};
var word;
while (word = reader.readLine()) {
  dict[word] = word.split("").reverse().join("");
}

function isSemordnilap(word) { return dict[dict[word]]; };

var semordnilaps = []
for (var key in dict) {
  if (isSemordnilap(key)) {
    var rev = dict[key];
    if (key < rev) {
      semordnilaps.push([key,rev]) ;
    }
  }
}

var count = semordnilaps.length;
print("There are " + count + " semordnilaps in " +
      dictFile + ".  Here are 5:" );
var indices=[]
for (var i=0; i<count; ++i) {
  if (Math.random() < 1/Math.ceil(i/5.0)) {
     indices[i%5] = i 
  }
}
indices.sort()
for (var i=0; i<5; ++i) {
  print(semordnilaps[indices[i]]);
}
```


{{out}}

```txt
There are 158 semordnilaps in unixdict.txt.  Here are 5:
loot,tool
ah,ha
dial,laid
dine,enid
haw,wah
```



### macOS JavaScript for Automation

{{Trans|Python}}

```javascript
(() => {
    'use strict';

    // semordnilap :: [String] -> String
    const semordnilap = xs => {
        const go = ([s, ws], w) =>
            s.has(w.split('').reverse().join('')) ? (
                [s, [w].concat(ws)]
            ) : [s.add(w), ws];
        return xs.reduce(go, [new Set(), []])[1];
    };

    const main = () => {
        
        // xs :: [String]
        const xs = semordnilap(
            lines(readFile('unixdict.txt'))
        );

        console.log(xs.length);
        xs.filter(x => 4 < x.length).forEach(
            x => showLog(...[x, x.split('').reverse().join('')])
        )
    };


    // GENERIC FUNCTIONS ----------------------------

    // lines :: String -> [String]
    const lines = s => s.split(/[\r\n]/);

    // readFile :: FilePath -> IO String
    const readFile = fp => {
        const
            e = $(),
            uw = ObjC.unwrap,
            s = uw(
                $.NSString.stringWithContentsOfFileEncodingError(
                    $(fp)
                    .stringByStandardizingPath,
                    $.NSUTF8StringEncoding,
                    e
                )
            );
        return undefined !== s ? (
            s
        ) : uw(e.localizedDescription);
    };

    // showLog :: a -> IO ()
    const showLog = (...args) =>
        console.log(
            args
            .map(JSON.stringify)
            .join(' -> ')
        );

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
158
"revel" -> "lever"
"repel" -> "leper"
"remark" -> "kramer"
"regal" -> "lager"
"nomad" -> "damon"
```



## jq

{{Works with|jq|1.4}}
The following program illustrates several points about jq:
* jq can be used to process text, as well as JSON;
* for text-processing tasks for which awk is well-suited, jq may be slightly slower;
* jq objects (i.e. JSON objects) can be used to define time-efficient mappings from strings.

Here are some running times on the same machine:
* awk program:
user 0m0.134s; sys 0m0.012s

* time /usr/local/bin/jq -M -s -R -r -f semordnilap.jq  unixdict.txt
user 0m0.440s; sys 0m0.010s

```jq

# Produce a stream
def report:
  split("\n") as $list
  # construct the dictionary:
  | (reduce $list[] as $entry ({}; . + {($entry): 1})) as $dict
  # construct the list of semordnilaps:
  | $list[]
  | select( (explode|reverse|implode) as $rev
            | (. < $rev and $dict[$rev]) );

[report] | (.[0:5][],  "length = \(length)")
```

{{Out}}
 able
 abut
 ac
 ah
 al
 length = 158


## Julia


```julia
raw = readdlm("unixdict.txt",String)[:]
inter = intersect(raw,map(reverse,raw)) #find the matching strings/revstrings
res = String[b == 1 && a != reverse(a) && a < reverse(a) ? a : reverse(a) for a in inter, b in 1:2] #create pairs
res = res[res[:,1] .!= res[:,2],:] #get rid of duplicates, palindromes
```


```txt
julia> length(res[:,1])
158

julia> res[1:5,:]
5x2 String Array:
 "able"  "elba"
 "abut"  "tuba"
 "ac"    "ca"  
 "ah"    "ha"  
 "al"    "la"  
```



## Kotlin

{{trans|D}}

```scala
// version 1.2.0

import java.io.File
 
fun main(args: Array<String>) {
    val words = File("unixdict.txt").readLines().toSet()
    val pairs = words.map { Pair(it, it.reversed()) }
            .filter { it.first < it.second && it.second in words } // avoid dupes+palindromes, find matches
    println("Found ${pairs.size} semordnilap pairs")
    println(pairs.take(5))
}
```


{{out}}

```txt

Found 158 semordnilap pairs
[(able, elba), (abut, tuba), (ac, ca), (ah, ha), (al, la)]

```



## Lasso


```Lasso
local(
	words		= string(include_url('http://www.puzzlers.org/pub/wordlists/unixdict.txt')) -> split('\n'),
	semordnilaps	= array,
	found_size,
	example,
	haveexamples	= false,
	examples	= array
)

#words -> removeall('')

with word in #words do {
	local(reversed = string(#word) -> reverse&)
	if(not(#word == #reversed) and not(#semordnilaps >> #word) and not(#semordnilaps >> #reversed) and #words >> #reversed) => {
		#semordnilaps -> insert(#word = #reversed)
	}
}

#found_size = #semordnilaps -> size

while(not(#haveexamples)) => {
	#example = #semordnilaps -> get(integer_random(#found_size, 1))
	not(#examples >> #example -> name) ? #examples -> insert(#example)
	#examples -> size >= 5 ? #haveexamples = true
}
'Total found: '
#found_size
'<br />'
#examples
```

{{out}}

```txt
Total found: 158
array((dew = wed), (are = era), (den = ned), (oat = tao), (eel = lee))
```



## Liberty BASIC


```lb

print "Loading dictionary."
open "unixdict.txt" for input as #1
while not(eof(#1))
   line input #1, a$
   dict$=dict$+" "+a$
wend
close #1

print "Dictionary loaded."
print "Seaching for semordnilaps."

semo$=" "   'string to hold words with semordnilaps

do
    i=i+1
    w$=word$(dict$,i)
    p$=reverseString$(w$)
    if w$<>p$ then
    p$=" "+p$+" "
    if instr(semo$,p$) = 0 then
        if instr(dict$,p$) then
          pairs=pairs+1
          print w$+" /"+p$
          semo$=semo$+w$+p$
        end if
      end if
    end if
    scan
loop until w$=""

print "Total number of unique semordnilap pairs is ";pairs
wait

Function isPalindrome(string$)
    string$ = Lower$(string$)
    reverseString$ = reverseString$(string$)
    If string$ = reverseString$ Then isPalindrome = 1
End Function

Function reverseString$(string$)
    For i = Len(string$) To 1 Step -1
        reverseString$ = reverseString$ + Mid$(string$, i, 1)
    Next i
End Function

```

{{out}}

```txt

able / elba
leper / repel
lever / revel
moor / room
suez / zeus
tort / trot
Total number of unique semordnilap pairs is 158

```



## Lua


```Lua
#!/usr/bin/env lua
-- allow dictionary file and sample size to be specified on command line
local dictfile = arg[1] or "unixdict.txt"
local sample_size = arg[2] or 5;

-- read dictionary
local f = assert(io.open(dictfile, "r"))
local dict = {}
for line in f:lines() do
  dict[line] = line:reverse()
end
f:close()

-- find the semordnilaps
local semordnilaps = {}
for fwd, rev in pairs(dict) do
  if dict[rev] and fwd < rev then
    table.insert(semordnilaps, {fwd,rev})
  end
end

-- print the report
print("There are " .. #semordnilaps .. " semordnilaps in " .. dictfile .. ".  Here are " .. sample_size .. ":")

math.randomseed( os.time() )
for i = 1, sample_size do
  local j
  repeat 
    j = math.random(1,#semordnilaps)
  until semordnilaps[j]
  local f, r = unpack(semordnilaps[j])
  semordnilaps[j] = nil
  print(f .. " -> " .. r)
end
```


{{out}}

```txt
There are 158 semordnilaps in unixdict.txt.  Here are 5:
deer -> reed
rat -> tar
pus -> sup
meet -> teem
bat -> tab
```



## M2000 Interpreter


```M2000 Interpreter

Module semordnilaps {
      Document d$
      Load.Doc d$, "unixdict.txt"
      Inventory MyDict, Result
      Function s$(a$) {
            m$=a$:k=Len(a$):for i=1 to k {insert i, 1 m$=mid$(a$, k, 1):k--} : =m$
      }
      L=Doc.Par(d$)
      m=Paragraph(d$, 0) 
      If not Forward(d$,m) then exit
      i=1
      While m {
            word$=Paragraph$(d$,(m))
            Print Over $(0, 10), str$(i/L,"##0.00%"), Len(Result) : i++
            If Exist(MyDict, word$) then { if Exist(Result, word$) Then exit
                  Append Result, word$
            } Else.if  len(word$)>1 Then p$=s$(word$):if p$<>word$ Then Append MyDict, p$
      }
      Print
      Print "Semordnilap pairs: ";Len(Result)
      For i=0 to len(Result)-1 step  len(Result) div 5 {
            p$=Eval$(Result, i)
            Print s$(p$);"/";p$
      }
}
semordnilaps

```

{{out}}
<pre style="height:30ex;overflow:scroll">
Semordnilap pairs: 158
ac/ca
nap/pan
cos/soc
loot/tool
way/yaw
</pre >


## Mathematica


```Mathematica
data = Import["http://www.puzzlers.org/pub/wordlists/unixdict.txt", "List"];
result = DeleteDuplicates[ Select[data, MemberQ[data, StringReverse[#]]
  && # =!= StringReverse[#] &], (# ===StringReverse[#2]) &];
Print[Length[result], Take[result, 5]]
```

{{out}}

```txt
158 {able,abut,ac,ah,al}
```



## NetRexx

{{Trans|REXX}}

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

/* REXX ***************************************************************
* 07.09.2012 Walter Pachl
**********************************************************************/
fid = 'unixdict.txt'                   /* the test dictionary        */
ifi = File(fid)
ifr = BufferedReader(FileReader(ifi))
have = ''                              /* words encountered          */
pi = 0                                 /* number of palindromes      */
loop label j_ forever                  /* as long there is input     */
  line = ifr.readLine                  /* read a line (String)       */
  if line = null then leave j_         /* NULL indicates EOF         */
  w = Rexx(line)                       /* each line contains 1 word  */
  If w > '' Then Do                    /* not a blank line           */
    r = w.reverse                      /* reverse it                 */
    If have[r] > '' Then Do            /* was already encountered    */
      pi = pi + 1                      /* increment number of pal's  */
      If pi <= 5 Then                  /* the first 5 are listed     */
        Say have[r] w
      End
    have[w] = w                        /* remember the word          */
    End
  end j_
ifr.close

Say pi 'words in' fid 'have a palindrome' /* total number found      */
return

```

{{out}}

```txt

ac ca 
bad dab 
avid diva 
and dna 
bard drab 
158 words in unixdict.txt have a palindrome

```



## Nim


```nim
import strutils, sequtils, sets, algorithm

proc reverse(s): string =
  result = newString(s.len)
  for i,c in s:
    result[s.high - i] = c

let
  words = readFile("unixdict.txt").strip.splitLines
  wordset = words.toSet
  revs = words.map(reverse)
var pairs = zip(words, revs).filterIt(it[0] < it[1] and it[1] in wordset)

echo "Total number of semordnilaps: ", pairs.len
pairs.sort(proc (x,y): auto = cmp(x[0].len,y[0].len))
echo pairs[pairs.high-4..pairs.high]
```

{{out}}

```txt
Total number of semordnilaps: 158
@[(a: damon, b: nomad), (a: lager, b: regal), (a: leper, b: repel), (a: lever, b: revel), (a: kramer, b: remark)]
```



## OCaml


```ocaml
module StrSet = Set.Make(String)

let str_rev s =
  let len = String.length s in
  let r = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set r i s.[len - 1 - i]
  done;
  Bytes.to_string r

let input_line_opt ic =
  try Some (input_line ic)
  with End_of_file -> close_in ic; None

let () =
  let ic = open_in "unixdict.txt" in
  let rec aux set acc =
    match input_line_opt ic with
    | Some word ->
        let rev = str_rev word in
        if StrSet.mem rev set
        then aux set ((word, rev) :: acc)
        else aux (StrSet.add word set) acc
    | None ->
        (acc)
  in
  let pairs = aux StrSet.empty [] in
  let len = List.length pairs in
  Printf.printf "Semordnilap pairs: %d\n" len;
  Random.self_init ();
  for i = 1 to 5 do
    let (word, rev) = List.nth pairs (Random.int len) in
    Printf.printf " %s %s\n" word rev
  done
```


{{out}}

```txt

Semordnilap pairs: 158
 tar rat
 sera ares
 sub bus
 tic cit
 mid dim

```



## Octave


```octave
a = strsplit(fileread("unixdict.txt"), "\n");
a = intersect(a, cellfun(@fliplr, a, "UniformOutput", false));
a = a(arrayfun(@(i) ismember(fliplr(a{i}), a(i+1:length(a))), 1:length(a)));
length(a)
arrayfun(@(i) printf("%s %s\n", a{i}, fliplr(a{i})), 1:5)
```


'''Output:'''


```txt
ans =  158

able elba
abut tuba
ac ca
ah ha
al la
```



## Oforth



```Oforth
: semordnilap
| w wr wrds |
   ListBuffer new ->wrds
   ListBuffer new
   File new("unixdict.txt") forEach: w [ 
      wrds include(w reverse dup ->wr) ifTrue: [ [wr, w] over add ]
      w wr < ifTrue: [ wrds add(w) ]
      ] ;
```


{{out}}

```txt

>semordnilap dup size println left(5) println
158
[[ac, ca], [bad, dab], [avid, diva], [and, dna], [bard, drab]]

```



## Perl


```perl
while (<>) {
	chomp;
	my $r = reverse;
	$seen{$r}++ and $c++ < 5 and print "$_ $r\n" or $seen{$_}++;
}

print "$c\n"
```



## Perl 6

{{works with|rakudo|2015-10-26}}

```perl6
my $words = set slurp("unixdict.txt").lines;

my @sems = gather for $words.flat -> $word {
    my $drow = $word.key.flip;
    take $drow if $drow ∈ $words and $drow lt $word;
}

say $_ ~ ' ' ~ $_.flip for @sems.pick(5);
```

{{out}}

```txt
abut tuba
avid diva
bard drab
loot tool
part trap
```



## Phix


```Phix
sequence words={}, semilordnaps={}
object word
constant fn = open("demo\\unixdict.txt","r")
 
while 1 do
    word = trim(gets(fn))
    if atom(word) then exit end if
    if find(reverse(word),words) then
        semilordnaps = append(semilordnaps,word)
    end if
    words = append(words,word)
end while
 
close(fn)
 
?length(semilordnaps)
for i=1 to 5 do
    word = semilordnaps[i]
    printf(1,"%s - %s\n",{word,reverse(word)})
end for
```

{{out}}

```txt

158
ca - ac
dab - bad
diva - avid
dna - and
drab - bard

```



## PHP

{{trans|Perl 6}}

```php
<?php
// Read dictionary into array
$dictionary = array_fill_keys(file(
    'http://www.puzzlers.org/pub/wordlists/unixdict.txt',
    FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES
), true);
foreach (array_keys($dictionary) as $word) {
    $reversed_word = strrev($word);
    if (isset($dictionary[$reversed_word]) && $word > $reversed_word)
        $words[$word] = $reversed_word;
}
echo count($words), "\n";
// array_rand() returns keys, not values
foreach (array_rand($words, 5) as $word)
    echo "$word $words[$word]\n";
```

{{out}}

```txt
158
ti it
tide edit
top pot
tram mart
un nu
```



## PicoLisp


```PicoLisp
(let Semordnilap
   (mapcon
      '((Lst)
         (when (member (reverse (car Lst)) (cdr Lst))
            (cons (pack (car Lst))) ) )
      (make (in "unixdict.txt" (while (line) (link @)))) )
   (println (length Semordnilap) (head 5 Semordnilap)) )
```

{{out}}

```txt
158 ("able" "abut" "ac" "ah" "al")
```



## PL/I


```PL/I

find: procedure options (main); /* 20/1/2013 */
   declare word character (20) varying controlled;
   declare dict(*) character (20) varying controlled;
   declare 1 pair controlled,
              2 a character (20) varying, 2 b character (20) varying;
   declare (i, j) fixed binary;
   declare in file;

   open file(in) title ('/UNIXDICT.TXT,type(LF),recsize(100)');
   on endfile (in) go to completed_read;
   do forever;
      allocate word;
      get file (in) edit (word) (L);
   end;

completed_read:
   free word; /* because at the final allocation, no word was stored. */
   allocate dict(allocation(word));
   do i = 1 to hbound(dict,1);
      dict(i) = word; free word;
   end;

   /* Search dictionary for pairs: */
   do i = 1 to hbound(dict,1)-1;
      do j = i+1 to hbound(dict,1);
         if length(dict(i)) = length(dict(j)) then
            do;
               if dict(i) = reverse(dict(j)) then
                  do;
                     allocate pair; pair.a = dict(i); pair.b = dict(j);
                  end;
            end;
      end;
   end;

   put skip list ('There are ' || trim(allocation(pair)) || ' pairs.');

   do while (allocation(pair) > 0);
      put skip edit (pair) (a, col(20), a); free pair;
   end;
end find;

```


```txt

There are 158 pairs. 

```

5 values at random:

```txt

ward               draw
was                saw
wed                dew
wolf               flow
won                now

```



## PowerShell


```PowerShell

function Reverse-String ([string]$String)
{
    [char[]]$output = $String.ToCharArray()
    [Array]::Reverse($output)
    $output -join ""
}

[string]$url = "http://www.puzzlers.org/pub/wordlists/unixdict.txt"
[string]$out = ".\unixdict.txt"

(New-Object System.Net.WebClient).DownloadFile($url, $out)

[string[]]$file = Get-Content -Path $out

[hashtable]$unixDict    = @{}
[hashtable]$semordnilap = @{}

foreach ($line in $file)
{
    if ($line.Length -gt 1)
    {
        $unixDict.Add($line,"")
    }

    [string]$reverseLine = Reverse-String $line

    if ($reverseLine -notmatch $line -and $unixDict.ContainsKey($reverseLine))
    {
        $semordnilap.Add($line,$reverseLine)
    }
}

$semordnilap

"`nSemordnilap count: {0}" -f ($semordnilap.GetEnumerator() | Measure-Object).Count

```


{{Out}}

```txt

Name                           Value
----                           -----
nil                            lin
regal                          lager
tom                            mot
.
.
.
peek                           keep
soma                           amos
nob                            bon

Semordnilap count: 158

```



## Python


### Idiomatic


```python>>>
 with open('unixdict.txt') as f:
	wordset = set(f.read().strip().split())

>>> revlist = (''.join(word[::-1]) for word in wordset)
>>> pairs   = set((wrd, rev) for wrd, rev in zip(wordset, revlist) 
                  if wrd < rev and rev in wordset)
>>> len(pairs)
158
>>> sorted(pairs, key=lambda p: (len(p[0]), p))[-5:]
[('damon', 'nomad'), ('lager', 'regal'), ('leper', 'repel'), ('lever', 'revel'), ('kramer', 'remark')]
>>> 
```


{{trans|Perl 6}}

```python
import os
import random
# Load file and put it to dictionary as set
dictionary = {word.rstrip(os.linesep) for word in open('unixdict.txt')}

# List of results
results = []
for word in dictionary:
    # [::-1] reverses string
    reversed_word = word[::-1]
    if reversed_word in dictionary and word > reversed_word:
        results.append((word, reversed_word))

print(len(results))
for words in random.sample(results, 5):
    print(' '.join(words))
```

{{out}}

```txt
158
nob bon
mac cam
dub bud
viva aviv
nomad damon
```


===As a fold, using reduce===
{{Works with|Python|3.7}}

```python
'''Dictionary words paired by equivalence under reversal'''

from functools import (reduce)
from itertools import (chain)
import urllib.request


# semordnilaps :: [String] -> [String]
def semordnilaps(xs):
    '''The subset of words in a list which
       are paired (by equivalence under reversal)
       with other words in that list.
    '''
    def go(tpl, w):
        (s, ws) = tpl
        if w[::-1] in s:
            return (s, ws + [w])
        else:
            s.add(w)
            return (s, ws)
    return reduce(go, xs, (set(), []))[1]


# TEST ----------------------------------------------------
def main():
    '''Test'''

    url = 'http://wiki.puzzlers.org/pub/wordlists/unixdict.txt'
    ws = semordnilaps(
        urllib.request.urlopen(
            url
        ).read().splitlines()
    )
    print(
        fTable(
            __doc__ + ':\n\n(longest of ' +
            str(len(ws)) + ' in ' + url + ')\n'
        )(snd)(fst)(identity)(
            sorted(
                concatMap(
                    lambda x: (
                        lambda s=x.decode('utf8'): [
                            (s, s[::-1])
                        ] if 4 < len(x) else []
                    )()
                )(ws),
                key=compose(len)(fst),
                reverse=True
            )
        )
    )


# GENERIC -------------------------------------------------

# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    '''A concatenated list over which a function has been mapped.
       The list monad can be derived by using a function f which
       wraps its output in a list,
       (using an empty list to represent computational failure).'''
    return lambda xs: list(
        chain.from_iterable(map(f, xs))
    )


# FORMATTING ----------------------------------------------

# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
                     f -> xs -> tabular string.
    '''
    def go(xShow, fxShow, f, xs):
        ys = [xShow(x) for x in xs]
        w = max(map(len, ys))
        return s + '\n' + '\n'.join(map(
            lambda x, y: y.rjust(w, ' ') + ' -> ' + fxShow(f(x)),
            xs, ys
        ))
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# fst :: (a, b) -> a
def fst(tpl):
    '''First member of a pair.'''
    return tpl[0]


# identity :: a -> a
def identity(x):
    '''The identity function.'''
    return x


# snd :: (a, b) -> b
def snd(tpl):
    '''Second member of a pair.'''
    return tpl[1]


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Dictionary words paired by equivalence under reversal:

(longest of 158 in http://wiki.puzzlers.org/pub/wordlists/unixdict.txt)

kramer -> remark
 damon -> nomad
 lager -> regal
 leper -> repel
 lever -> revel
```



### Lazy generator


Requires the <code>requests</code> library.


```python
import sys
import random
import requests

URL = 'http://wiki.puzzlers.org/pub/wordlists/unixdict.txt'


def find_semordnilaps():
  """
  This generator could just take the `word_generator`
  as an argument and read words from it. That would
  have been both simpler and more efficient, but it
  is implemented this way for the sake of illustration.
  """
  seen = set()
  word = None

  while True:
    word = yield word

    if word not in seen:
      seen.add(word[::-1])
      word = None


def semordnilap_words(word_generator):

  semordnilaps_finder = find_semordnilaps()
  semordnilaps_finder.send(None)

  words = map(semordnilaps_finder.send, word_generator)

  # need to get rid of `None` values for words which are not semordnilaps
  yield from filter(None, words)


def url_lines(url):
  with requests.get(url, stream=True) as req:
    yield from req.iter_lines(decode_unicode=True)


def main(url=URL, num_of_examples=5):

  semordnilaps_generator = semordnilap_words(url_lines(url))

  semordnilaps = list(semordnilaps_generator)

  example_words = random.choices(semordnilaps, k=num_of_examples)
  example_pairs = ((word, word[::-1]) for word in example_words)

  print(('found %(num)s semordnilap usernames:\n'
         '%(examples)s\n'
         '...'
        ) % dict(
          num = len(semordnilaps),
          examples = '\n'.join(str(pair) for pair in example_pairs),
        ))

  return semordnilaps


if __name__ == '__main__':
  main(*sys.argv[1:])

```


{{Out}}

```txt

found 158 semordnilap usernames:
('mug', 'gum')
('revel', 'lever')
('gab', 'bag')
('lime', 'emil')
('lag', 'gal')
...

```



## Racket


```Racket

#lang racket
(define seen (make-hash))
(define semordnilaps '())
(call-with-input-file "/usr/share/dict/words"
  (λ(i) (for ([l (in-lines i)])
          (define r (list->string (reverse (string->list l))))
          (unless (equal? r l)
            (hash-set! seen l #t)
            (when (hash-ref seen r #f)
              (set! semordnilaps (cons (list r l) semordnilaps)))))))
(printf "Total semordnilaps found: ~s\n" (length semordnilaps))
(printf "The five longest ones:\n")
(for ([s (take (sort semordnilaps > #:key (compose1 string-length car)) 5)])
  (apply printf "  ~s ~s\n" s))

```


{{out}}

```txt

Total semordnilaps found: 1961
The five longest ones:
  "desserts" "stressed"
  "dioramas" "samaroid"
  "redrawer" "rewarder"
  "dessert" "tressed"
  "pat-pat" "tap-tap"

```



## REXX


### version 1


```rexx
/* REXX ***************************************************************
* 07.09.2012 Walter Pachl
**********************************************************************/
fid='unixdict.txt'                     /* the test dictionary        */
have.=''                               /* words encountered          */
pi=0                                   /* number of palindromes      */
Do li=1 By 1 While lines(fid)>0        /* as long there is input     */
  w=linein(fid)                        /* read a word                */
  If w>'' Then Do                      /* not a blank line           */
    r=reverse(w)                       /* reverse it                 */
    If have.r>'' Then Do               /* was already encountered    */
      pi=pi+1                          /* increment number of pal's  */
      If pi<=5 Then                    /* the first 5 ale listed     */
        Say have.r w
      End
    have.w=w                           /* remember the word          */
    End
  End
Say pi 'words in' fid 'have a palindrome' /* total number found      */
```

{{out}}

```txt

ac ca
bad dab
avid diva
and dna
bard drab
158 words in unixdict.txt have a palindrome  

```



### version 2

This REXX version makes use of sparse (stemmed) arrays.


The dictionary file wasn't assumed to be in any particular case (upper/lower/mixed).

For instance,   '''DNA'''   <big> &amp; </big>   '''and'''   would be considered palindromes. 

The UNIXDICT dictionary specified to be used ''is'' all lowercase, however, but the REXX 

program assumes that the words may be in any ''case''.  


The order of the words in the dictionary isn't important.

Any blank lines or ''duplicate words'' in the dictionary are ignored (as duplicate words wouldn't make them unique).

Any leading, trailing, or imbedded blanks are also ignored (as well as tab characters or other whitespace).

The palindrome pairs are shown with a comma delimiter in case there're phrases (words with imbedded blanks like Sing Sing).

The (first five) palindrome pairs are shown as they are specified (respective to case) in the dictionary.

```rexx
/*REXX program finds  palindrome pairs in a dictionary  (the default is  UNIXDICT.TXT). */
#=0                                                       /*number palindromes (so far).*/
parse arg iFID .;  if iFID==''  then iFID='UNIXDICT.TXT'  /*Not specified?  Use default.*/
@.=                                                       /*uppercase no─duplicated word*/
    do  while lines(iFID)\==0;  _=space(linein(iFID),0)   /*read a word from dictionary.*/
    parse upper var _ u                                   /*obtain an uppercase version.*/
    if length(_)<2 | @.u\==''  then iterate               /*can't be a unique palindrome*/
    r=reverse(u)                                          /*get the reverse of the word.*/
    if @.r\==''  then do;  #=#+1                          /*find a palindrome pair ?    */
                           if #<6  then say  @.r','   _   /*just show 1st 5 palindromes.*/
                      end                                 /* [↑]  bump palindrome count.*/
    @.u=_                                                 /*define a unique palindrome. */
    end   /*while*/                                       /* [↑]  read the dictionary.  */
say
say "There're "      #      ' unique palindrome pairs in the dictionary file: '       iFID
                                                      /*stick a fork in it, we're done. */
```

'''output'''   when using the default dictionary as the input:

```txt

ac, ca
bad, dab
avid, diva
and, dna
bard, drab

There're  158  unique palindrome pairs in the dictionary file:  UNIXDICT.TXT

```



## Ring


```ring

# Project : Semordnilap

load "stdlib.ring"
nr = 0
num = 0
aList = file2list("C:\Ring\CalmoSoft\unixdict.txt")
for n = 1 to len(aList)
     bool = semordnilap(aList[n])
     if (bool > 0 and nr > n)
        num = num + 1 
        if num % 31 = 0
           see aList[n] + " " + aList[nr] + nl 
        ok
     ok
next
see "Total number of unique pairs = " + num + nl

func semordnilap(aString)
       bString = ""
       for i=len(aString) to 1 step -1
            bString = bString + aString[i]
       next
       nr = find(aList,bString)
       return nr

```

{{out}}

```txt

brag garb
edit tide
it ti
mit tim
suez zeus
Total number of unique pairs = 158

```



## Ruby

Note: An alternative (old fashioned) method of solving this task (not using a Set as done by other solutions) is to produce 2 sorted files and walk through them. This can be done entirly on disk if required, when done in memory it is faster than a set for large samples.--[[User:Nigel Galloway|Nigel Galloway]] 11:12, 17 September 2012 (UTC)

```Ruby
dict = File.readlines("unixdict.txt").collect(&:strip)
i = 0
res = dict.collect(&:reverse).sort.select do |z| 
  i += 1  while z > dict[i] and i < dict.length-1
  z == dict[i] and z < z.reverse
end
puts "There are #{res.length} semordnilaps, of which the following are 5:"
res.take(5).each {|z| puts "#{z}   #{z.reverse}"}
```

{{out}}

```txt

There are 158 semordnilaps, of which the following are 5:
able   elba
abut   tuba
ac   ca
ah   ha
al   la

```


Another way

```Ruby
words = File.readlines("unixdict.txt")
            .group_by{|x| [x.strip!, x.reverse].min}
            .values
            .select{|v| v.size==2}
puts "There are #{words.size} semordnilaps, of which the following are 5:"
words.take(5).each {|a,b| puts "#{a}   #{b}"}
```

output is the same above.


## Scala


```scala
val wordsAll = scala.io.Source.fromURL("http://www.puzzlers.org/pub/wordlists/unixdict.txt").getLines.map(_.toLowerCase).to[IndexedSeq]

/**
 * Given a sequence of lower-case words return a sub-sequence 
 * of matches containing the word and its reverse if the two
 * words are different.
 */
def semordnilap( words:Seq[String] ) : Seq[(String,String)] = {

  ( words.
    zipWithIndex.                        // index will be needed to eliminate duplicate
    filter { 
      case (w,i) => 
        val j = words.indexOf(w.reverse) // eg. (able,62) and (elba,7519) 
        i < j && w != w.reverse          // save the matches which are not palindromes
    }
  ).
  map {
    case (w,i) => (w,w.reverse)          // drop the index
  }
}

val ss = semordnilap(wordsAll)

{
println( ss.size + " matches, including: \n" )
println( ss.take(5).mkString( "\n" ) )
}
```

{{out}}

```txt
158 matches, including:

(able,elba)
(abut,tuba)
(ac,ca)
(ah,ha)
(al,la)

```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "gethttp.s7i";

const func string: reverse (in string: word) is func
  result
    var string: drow is "";
  local
    var integer: index is 0;
  begin
    for index range length(word) downto 1 do
      drow &:= word[index];
    end for;
  end func;

const proc: main is func
  local
    var array string: wordList is 0 times "";
    var set of string: words is (set of string).value;
    var string: word is "";
    var string: drow is "";
    var integer: count is 0;
  begin
    wordList := split(lower(getHttp("www.puzzlers.org/pub/wordlists/unixdict.txt")), "\n");
    for word range wordList do
      drow := reverse(word);
      if drow not in words then
        incl(words, word);
      else
        if count < 5 then
          writeln(word <& " " <& drow);
        end if;
        incr(count);
      end if;
    end for;
    writeln;
    writeln("Semordnilap pairs: " <& count);
  end func;
```


{{out}}

```txt

ca ac
dab bad
diva avid
dna and
drab bard

Semordnilap pairs: 158

```



## Sidef

{{trans|Perl}}

```ruby
var c = 0
var seen = Hash()

ARGF.each { |line|
    line.chomp!
    var r = line.reverse
    ((seen{r} := 0 ++) && (c++ < 5) && say "#{line} #{r}") ->
        || (seen{line} := 0 ++)
}

say c
```

{{out}}

```txt

$ sidef semordnilap.sf < unixdict.txt
ca ac
dab bad
diva avid
dna and
drab bard
158

```



## Stata



```stata
set seed 17760704
import delimited http://www.puzzlers.org/pub/wordlists/unixdict.txt, clear
save temp, replace
replace v1=strreverse(v1)
merge 1:1 v1 using temp, nogen keep(3)
drop if v1>=strreverse(v1)
count
  158
sample 5, count
gen v2=strreverse(v1)
list, noheader noobs
  +-------------+
  | evil   live |
  |  pat    tap |
  |   at     ta |
  |  nit    tin |
  |   ku     uk |
  +-------------+
```



## SuperCollider

{{incorrect|SuperCollider|The number of pairs should be 158.}}

```SuperCollider
(
var text, words, sdrow, semordnilap, selection;
File.use("unixdict.txt".resolveRelative, "r", { |f| x = text = f.readAllString });
words = text.split(Char.nl).collect { |each| each.asSymbol };
sdrow = text.reverse.split(Char.nl).collect { |each| each.asSymbol };
semordnilap = sect(words, sdrow); // converted to symbols so intersection is possible
semordnilap = semordnilap.collect { |each| each.asString };
"There are % in unixdict.txt\n".postf(semordnilap.size);
"For example those, with more than 3 characters:".postln;
selection = semordnilap.select { |each| each.size >= 4 }.scramble.keep(4);
selection.do { |each| "% %\n".postf(each, each.reverse); };
)
```


Answers:

```SuperCollider

There are 405 in unixdict.txt
For example those, with more than 3 characters:
live evil
tram mart
drib bird
eros sore

```


This seems wrong, but perhaps the test file has changed?


## Tcl


```tcl
package require Tcl 8.5
package require http

# Fetch the words
set t [http::geturl http://www.puzzlers.org/pub/wordlists/unixdict.txt]
set wordlist [split [http::data $t] \n]
http::cleanup $t

# Build hash table for speed
foreach word $wordlist {
    set reversed([string reverse $word]) "dummy"
}

# Find where a reversal exists
foreach word $wordlist {
    if {[info exists reversed($word)] && $word ne [string reverse $word]} {
	# Remove to prevent pairs from being printed twice
	unset reversed([string reverse $word])
	# Add to collection of pairs
	set pairs($word/[string reverse $word]) "dummy"
    }
}
set pairlist [array names pairs] ;# NB: pairs are in *arbitrary* order

# Report what we've found
puts "Found [llength $pairlist] reversed pairs"
foreach pair $pairlist {
    puts "Example: $pair"
    if {[incr i]>=5} break
}
```

{{out}}

```txt

Found 158 reversed pairs
Example: lap/pal
Example: jar/raj
Example: ix/xi
Example: eros/sore
Example: bard/drab

```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT,{}
requestdata = REQUEST ("http://www.puzzlers.org/pub/wordlists/unixdict.txt")
DICT semordnilap CREATE 99999
COMPILE
LOOP r=requestdata
rstrings=STRINGS(r," ? ")
rreverse=REVERSE(rstrings)
revstring=EXCHANGE (rreverse,":'':':'::")
group=APPEND (r,revstring)
sort=ALPHA_SORT (group)
DICT semordnilap APPEND/QUIET/COUNT sort,num,cnt,"",""
ENDLOOP
DICT semordnilap UNLOAD wordgroups,num,howmany
get_palins=FILTER_INDEX (howmany,-," 1 ")
size=SIZE(get_palins)
PRINT "unixdict.txt contains ", size, " palindromes"
PRINT " "
palindromes=SELECT (wordgroups,#get_palins)
LOOP n=1,5
take5=SELECT (palindromes,#n)
PRINT n,". ",take5
ENDLOOP
ENDCOMPILE

```

{{out}}

```txt

unixdict.txt contains 158 palindromes

1. able'elba
2. abut'tuba
3. ac'ca
4. ah'ha
5. al'la

```



## VBScript


```vb

Set objFSO = CreateObject("Scripting.FileSystemObject")
Set objInFile = objFSO.OpenTextFile(objFSO.GetParentFolderName(WScript.ScriptFullName) &_
		"\unixdict.txt",1)

Set objUnixDict = CreateObject("Scripting.Dictionary")
Set objSemordnilap = CreateObject("Scripting.Dictionary")

Do Until objInFile.AtEndOfStream
	line = objInFile.ReadLine
	If Len(line) > 1 Then
		objUnixDict.Add line,""
	End If
	reverse_line = StrReverse(line)
	If reverse_line <> line And objUnixDict.Exists(reverse_line) Then
		objSemordnilap.Add line, reverse_line
	End If
Loop	

'Display the first 5 keys.
k = 0
For Each Key In objSemordnilap.Keys
	WScript.StdOut.Write Key & " - " & objSemordnilap.Item(Key)
	WScript.StdOut.WriteLine
	k = k + 1
	If k = 5 Then
		Exit For
	End If
Next

WScript.StdOut.Write "Total Count:  " & objSemordnilap.Count
WScript.StdOut.WriteLine

objInFile.Close
Set objFSO = Nothing
Set objUnixDict = Nothing
Set objSemordnilap = Nothing

```


{{out}}

```txt

ca - ac
dab - bad
diva - avid
dna - and
drab - bard
Total Count:  158

```



## XPL0


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations
string 0;               \use zero-terminated strings
def LF=$0A, CR=$0D, EOF=$1A;

proc RevStr(S);         \Reverse order of characters in a string
char S;
int  I, J, T;
[J:= 0;
while S(J) do J:= J+1;
J:= J-1;
I:= 0;
while I<J do
        [T:= S(I);  S(I):= S(J);  S(J):= T;     \swap
        I:= I+1;  J:= J-1;
        ];
];

func StrEqual(S1, S2);  \Compare strings, return 'true' if equal
char S1, S2;
int  I;
[for I:= 0 to 80-1 do
        [if S1(I) # S2(I) then return false;
         if S1(I) = 0 then return true;
        ];
];

int  C, I, J, SJ, Count;
char Dict, Word(80);
[\Read file on command line redirected as input, i.e: <unixdict.txt
Dict:= GetHp;           \starting address of block of local "heap" memory
I:= 0;                  \ [GetHp does exact same thing as Reserve(0)]
repeat  repeat C:= ChIn(1) until C#LF;  \get chars sans line feeds
        if C = CR then C:= 0;           \replace carriage return with terminator
        Dict(I):= C;  I:= I+1;
until   C = EOF;
SetHp(Dict+I);          \set heap pointer beyond Dict
I:= 0;  Count:= 0;
loop    [J:= 0;         \get word at I
        repeat  C:= Dict(I+J);  Word(J):= C;  J:= J+1;
        until   C=0;
        RevStr(Word);
        J:= J+I;        \set J to following word in Dict
        if Dict(J) = EOF then quit;
        SJ:= J;         \save index to following word
        loop    [if StrEqual(Word, Dict+J) then
                    [Count:= Count+1;
                    if Count <= 5 then
                        [RevStr(Word);  \show some examples
                        Text(0, Word); ChOut(0, ^ ); Text(0, Dict+J); CrLf(0);
                        ];
                    quit;
                    ];
                repeat J:= J+1 until Dict(J) = 0;
                J:= J+1;
                if Dict(J) = EOF then quit;
                ];
        I:= SJ;         \next word
        ];
IntOut(0, Count);  CrLf(0);
]
```


{{out}}

```txt

able elba
abut tuba
ac ca
ah ha
al la
158

```



## zkl


```zkl
var [const] words=  // create hashed unixdict of striped words (word:True, ...)
   File("dict.txt").howza(11).pump(Dictionary().howza(8).add.fp1(True));
ss:=words.pump(List, // push stripped unixdict words through some functions
   fcn(w){ words.holds(w.reverse()) }, Void.Filter,   // filter palindromes
   // create ("word","drow") if "word"<"drow" (ie remove duplicates)
   fcn(w){ r:=w.reverse(); if(w<r) T(w,r) else Void.Skip });

ss.len().println(); //--> 158
ss.shuffle()[0,5].println();
```

{{out}}

```txt

158
L(L("bog","gob"),L("cup","puc"),L("mart","tram"),L("kay","yak"),L("able","elba"))

```



{{omit from|Brlcad}}
{{omit from|GUISS}}
{{omit from|Locomotive Basic}}
{{omit from|Openscad}}
{{omit from|TPP}}
{{omit from|ZX Spectrum Basic}}
