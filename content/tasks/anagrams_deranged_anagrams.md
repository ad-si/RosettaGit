+++
title = "Anagrams/Deranged anagrams"
description = ""
date = 2019-10-06T00:22:59Z
aliases = []
[extra]
id = 9632
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "11l",
  "ada",
  "algol_68",
  "autohotkey",
  "awk",
  "bacon",
  "bbc_basic",
  "bracmat",
  "c",
  "clojure",
  "coffeescript",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "echolisp",
  "eiffel",
  "elixir",
  "erlang",
  "factor",
  "freebasic",
  "gap",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "k",
  "kotlin",
  "lasso",
  "liberty_basic",
  "maple",
  "ocaml",
  "oorexx",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "sidef",
  "simula",
  "tcl",
  "tuscript",
  "unix_shell",
  "ursala",
  "zkl",
]
+++

## Task

Two or more words are said to be [anagrams](/tasks/anagrams) if they have the same characters, but in a different order.

By analogy with [[Permutations/Derangements|derangements]] we define a  ''deranged anagram'' as two words with the same characters, but in which the same character does ''not'' appear in the same position in both words.

{{task heading}}

Use the word list at [http://wiki.puzzlers.org/pub/wordlists/unixdict.txt unixdict] to find and display the longest deranged anagram.

{{task heading|Related tasks}}

* [[Permutations/Derangements]]
* [[Best shuffle]]
{{Related tasks/Word plays}}

<hr>


## 11l

{{trans|Kotlin}}

```11l
F is_not_deranged(s1, s2)
   L(i) 0 .< s1.len
      I s1[i] == s2[i]
         R 1B
   R 0B

Dict[String, Array[String]] anagram
V count = 0
L(word) File(‘unixdict.txt’).read().split("\n")
   V a = sorted(word).join(‘’)
   I a !C anagram
      anagram[a] = [word]
   E
      L(ana) anagram[a]
         I is_not_deranged(ana, word)
            L.break
         L.was_no_break
            anagram[a].append(word)
            count = max(count, word.len)

L(ana) anagram.values()
   I ana.len > 1 & ana[0].len == count
      print(ana)
```

{{out}}

```txt
[excitation, intoxicate]
```



## Ada

{{Works with|Ada 2005}}

```Ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Indefinite_Vectors;
procedure Danagrams is
   package StringVector is new Ada.Containers.Indefinite_Vectors
      (Positive, String);
   procedure StrSort is new Ada.Containers.Generic_Array_Sort
      (Index_Type => Positive,
      Element_Type => Character,
      Array_Type => String);
   function Derange (s1 : String; s2 : String) return Boolean is begin
      for i in s1'Range loop
         if (s1 (i) = s2 (i)) then return False; end if;
      end loop;
      return True;
   end Derange;
   File : File_Type;
   len, foundlen : Positive := 1;
   Vect, SVect : StringVector.Vector;
   index, p1, p2 : StringVector.Extended_Index := 0;
begin
   Open (File, In_File, "unixdict.txt");
   while not End_Of_File (File) loop
      declare str : String := Get_Line (File);
      begin
         len := str'Length;
         if len > foundlen then
            Vect.Append (str);
            StrSort (str);
            index := 0;
            loop --  Loop through anagrams by index in vector of sorted strings
               index := SVect.Find_Index (str, index + 1);
               exit when index = StringVector.No_Index;
               if Derange (Vect.Last_Element, Vect.Element (index)) then
                     p1 := Vect.Last_Index; p2 := index;
                     foundlen := len;
               end if;
            end loop;
            SVect.Append (str);
         end if;
      end;
   end loop;
   Close (File);
   Put_Line (Vect.Element (p1) & " " & Vect.Element (p2));
end Danagrams;
```

{{out}}

```txt
intoxicate excitation
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}} Uses the "read" PRAGMA of Algol 68 G to include the associative array code from the [[Associative_array/Iteration]] task.

```algol68
# find the largest deranged anagrams in a list of words              #
# use the associative array in the Associate array/iteration task    #
PR read "aArray.a68" PR

# returns the length of str                                          #
OP LENGTH = ( STRING str )INT: 1 + ( UPB str - LWB str );

# returns TRUE if a and b are the same length and have no            #
# identical characters at any position,                              #
# FALSE otherwise                                                    #
PRIO ALLDIFFER = 9;
OP   ALLDIFFER = ( STRING a, b )BOOL:
     IF LENGTH a /= LENGTH b
     THEN
        # the two stringa are not the same size                      #
        FALSE
     ELSE
        # the strings are the same length, check the characters      #
        BOOL result := TRUE;
        INT  b pos  := LWB b;
        FOR a pos FROM LWB a TO UPB a WHILE result := ( a[ a pos ] /= b[ b pos ] )
        DO
            b pos +:= 1
        OD;
        result
     FI # ALLDIFFER # ;

# returns text with the characters sorted                            #
OP   SORT = ( STRING text )STRING:
     BEGIN
        STRING sorted := text;
        FOR end pos FROM UPB sorted - 1 BY -1 TO LWB sorted
        WHILE
            BOOL swapped := FALSE;
            FOR pos FROM LWB sorted TO end pos DO
                IF sorted[ pos ] > sorted[ pos + 1 ]
                THEN
                    CHAR  t           := sorted[ pos     ];
                    sorted[ pos     ] := sorted[ pos + 1 ];
                    sorted[ pos + 1 ] := t;
                    swapped           := TRUE
                FI
            OD;
            swapped
        DO SKIP OD;
        sorted
     END # SORTED # ;

# read the list of words and find the longest deranged anagrams      #

CHAR separator = "|"; # character that will separate the anagrams    #

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
    INT    longest derangement := 0;
    STRING longest word        := "<none>";
    STRING longest anagram     := "<none>";
    WHILE NOT at eof
    DO
        STRING word;
        get( input file, ( word, newline ) );
        INT    word length = LENGTH word;
        IF  word length >= longest derangement
        THEN
            # this word is at least long as the longest derangement   #
            # found so far - test it                                  #
            STRING sorted word = SORT word;
            IF ( words // sorted word ) /= ""
            THEN
                # we already have this sorted word - test for         #
                # deranged anagrams                                   #
                # the word list will have a leading separator         #
                # and be followed by one or more words separated by   #
                # the separator                                       #
                STRING word list   := words // sorted word;
                INT    list pos    := LWB word list + 1;
                INT    list max     = UPB word list;
                BOOL   is deranged := FALSE;
                WHILE  list pos < list max
                  AND  NOT is deranged
                DO
                    STRING anagram = word list[ list pos : ( list pos + word length ) - 1 ];
                    IF  is deranged := word ALLDIFFER anagram
                    THEN
                        # have a deranged anagram                     #
                        longest derangement := word length;
                        longest word        := word;
                        longest anagram     := anagram
                    FI;
                    list pos +:= word length + 1
                OD
            FI;
            # add the word to the anagram list                        #
            words // sorted word +:= separator + word
        FI
    OD;
    close( input file );
    print( ( "Longest deranged anagrams: "
           , longest word
           , " and "
           , longest anagram
           , newline
           )
         )
FI
```

{{out}}

```txt

Longest deranged anagrams: intoxicate and excitation

```



## AutoHotkey


```Autohotkey
Time := A_TickCount
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
SetBatchLines   -1
Loop, Read, unixdict.txt
	StrOut .= StrLen(A_LoopReadLine) - 2 . "," . A_LoopReadLine . "`n"
Sort StrOut, N R
Loop, Parse, StrOut, `n, `r
{
	StringSplit, No_Let, A_Loopfield, `,
	if ( old1 = no_let1 )
		string .= old2 "`n"
	if ( old1 != no_let1 )
	{
		string := trim(string old2)
		if ( old2 != "" )
			Loop, Parse, string, `n, `r  ; Specifying `n prior to `r allows both Windows and Unix files to be Parsed.
				line_number := A_Index
		if ( line_number > 1 )
		{
			Loop, Parse, string, `n, `r
			{
				StringSplit, newstr, A_Loopfield, `, ; break the string based on Comma
				Loop, Parse, newstr2
					k .= A_LoopField " "
				Sort k, D%A_Space%
				k := RegExReplace( k, "\s", "" )
				file .= "`r`n" k . "," . newstr1 . "," . newstr2
				k =
			}
			Sort File
			Loop, Parse, File, `n, `r
			{
				if ( A_Loopfield != "" )
				{
					StringSplit, T_C, A_Loopfield, `,
					if ( old = T_C1 )
					{
						Loop, 1
						{
							Loop % T_C2
								if (SubStr(T_C3, A_Index, 1) = SubStr(old3, A_Index, 1))
									break 2
							Time := (A_tickcount - Time)/1000
							MsgBox % T_C3 " " old3 " in " Time . " seconds."
							ExitApp
						}
					}
					old := T_C1, old3 := T_C3
				}
			}
			file =
		}
		string =
	}
	old1 := no_let1, old2 := A_Loopfield
}
```

{{out}}

```txt

intoxicate excitation in 0.844000 seconds.
```



## AWK

{{works with|GNU awk (gawk) 3.1.5}}


```awk

#!/bin/gawk -f
BEGIN{
FS=""
wordcount = 0
maxlength = 0

}
# hash generates the sorted sequence of characters in a word,
# so that the hashes for a pair of anagrams will be the same.
# Example: hash meat = aemt and hash team = aemt
function hash(myword, i,letters,myhash){
  split(myword,letters,"")
  asort(letters)
  for (i=1;i<=length(myword);i++) myhash=myhash letters[i]
  return myhash
}
# deranged checks two anagrems for derangement
function deranged(worda, wordb,  a,b,i,n,len){
  n=0
  len=split(worda,a,"")
  split(wordb,b,"")
  for (i=len; i>=1; i--){
      if (a[i] == b[i]) n = n+1
  }
  return n==0
}

# field separator null makes gawk split input record character by character.
# the split function works the same way
{
  wordcount = wordcount + 1
  fullword[wordcount]=$0
  bylength[length($0)]=bylength[length($0)]  wordcount "|"
  if (length($0) > maxlength) maxlength = length($0)
}

END{
 for (len=maxlength; len>1; len--){
  numwords=split(bylength[len],words,"|")
  split("",hashed)
 split("",anagrams)
  for (i=1;i<=numwords;i++){
#   make lists of anagrams in hashed
      myword = fullword[words[i]]
      myhash = hash(myword)
      hashed[myhash] = hashed[myhash] myword " "
  }
# check anagrams for derangement
 for (myhash in hashed){
     n = split(hashed[myhash],anagrams," ")
     for (i=1; i< n; i++)
         for (j=i+1; j<=n; j++){
             if(deranged(anagrams[i],anagrams[j])) found = found anagrams[i] " " anagrams[j] " "
          }
  }
 if (length(found) > 0 ) print "deranged: " found
  if (length(found) > 0) exit
 }
}

```

On my system, this awk-file is located at /usr/local/bin/deranged,
so it can be invoked with:

```txt

deranged /tmp/unixdict.txt

```


Regular invocation would be:

```txt

gawk -f deranged.awk /tmp/unixdict.txt

```


{{out}}

```txt

deranged: excitation intoxicate

```



## BaCon


```freebasic

DECLARE idx$ ASSOC STRING

FUNCTION Deranged(a$, b$)
    FOR i = 1 TO LEN(a$)
        IF MID$(a$, i, 1) = MID$(b$, i, 1) THEN RETURN FALSE
    NEXT
    RETURN TRUE
END FUNCTION

FOR w$ IN LOAD$(DIRNAME$(ME$) & "/unixdict.txt") STEP NL$
    set$ = EXTRACT$(SORT$(EXPLODE$(w$, 1)), " ")
    idx$(set$) = APPEND$(idx$(set$), 0, w$)
NEXT

FOR w$ IN OBTAIN$(idx$)
    FOR x = 1 TO AMOUNT(idx$(w$))
        FOR y = x+1 TO AMOUNT(idx$(w$))
            IF Deranged(TOKEN$(idx$(w$), x), TOKEN$(idx$(w$), y)) AND LEN(TOKEN$(idx$(w$), x)) > current THEN
                current = LEN(TOKEN$(idx$(w$), x))
                an1$ = TOKEN$(idx$(w$), x)
                an2$ = TOKEN$(idx$(w$), y)
            END IF
        NEXT
    NEXT
NEXT

PRINT "Maximum deranged anagrams: ", an1$, " and ", an2$

PRINT NL$, "Total time: ", TIMER, " msecs.", NL$

```

{{out}}

```txt

Maximum deranged anagrams: excitation and intoxicate

Total time: 75 msecs.

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      INSTALL @lib$+"SORTLIB"
      Sort% = FN_sortinit(0,0)

      DIM dict$(26000), sort$(26000), indx%(26000)

      REM Load the dictionary:
      dict% = OPENIN("C:\unixdict.txt")
      IF dict%=0 ERROR 100, "No dictionary file"
      index% = 0
      REPEAT
        index% += 1
        dict$(index%) = GET$#dict%
        indx%(index%) = index%
      UNTIL EOF#dict%
      CLOSE #dict%
      Total% = index%

      TIME = 0
      REM Sort the letters in each word:
      FOR index% = 1 TO Total%
        sort$(index%) = FNsortstring(dict$(index%))
      NEXT

      REM Sort the sorted words:
      C% = Total%
      CALL Sort%, sort$(1), indx%(1)

      REM Find anagrams and deranged anagrams:
      maxlen% = 0
      maxidx% = 0
      FOR index% = 1 TO Total%-1
        IF sort$(index%) = sort$(index%+1) THEN
          One$ = dict$(indx%(index%))
          Two$ = dict$(indx%(index%+1))
          FOR c% = 1 TO LEN(One$)
            IF MID$(One$,c%,1) = MID$(Two$,c%,1) EXIT FOR
          NEXT
          IF c%>LEN(One$) IF c%>maxlen% maxlen% = c% : maxidx% = index%
        ENDIF
      NEXT

      PRINT "The longest deranged anagrams are '" dict$(indx%(maxidx%));
      PRINT "' and '" dict$(indx%(maxidx%+1)) "'"
      PRINT "(taking " ; TIME/100 " seconds)"
      END

      DEF FNsortstring(A$)
      LOCAL C%, a&()
      C% = LEN(A$)
      DIM a&(C%)
      $$^a&(0) = A$
      CALL Sort%, a&(0)
      = $$^a&(0)
```

{{out}}

```txt

The longest deranged anagrams are 'excitation' and 'intoxicate'
(taking 0.95 seconds)

```



## Bracmat

The file is read into a single string, <code>wordList</code>.
Then, in a while loop, each line is read and, in a nested loop, atomised into single letters.
The letters are added together to create a sorted list that is the letter sum, the 'anagram fingerprint', of the word.
To make sure that even single letter words create a sum of at least two terms, the sum is initialised with the empty string rather than zero.
(Otherwise the words <i>a</i> and <i>aaa</i> later on would get the same fingerprint, the factors <code>1</code> and <code>3</code> being factored out.)

For the word <i>bizarre</i> the letter sum is <code>(+a+b+e+2*r+z+i)</code>.
The letter sum, with the word as the exponent (<code>(+a+b+e+2*r+z+i)^bizarre</code>) is prepended to a list <code>unsorted</code>.
Somewhat later the word <i>brazier</i> also is prepended to the <code>unsorted</code> list.
This word happens to have the same letter sum as <i>bizarre</i>, so these two words must be anagrams of each other. The program brings these two elements together by merge sorting the <code>unsorted</code> list, using Bracmat's Computer Algebra powers to normalise sums and products by sorting and combining like terms or factors.
During the sort, all elements in the the <code>unsorted</code> list are multiplied together, combining factors with the same letter sums by adding their exponents together. So at some stage during sorting, the two elements <code>(+a+b+e+2*r+z+i)^bizarre</code> and <code>(+a+b+e+2*r+z+i)^brazier</code> are brought together in a product <code>(+a+b+e+2*r+z+i)^bizarre*(+a+b+e+2*r+z+i)^brazier</code> which immediately is transformed to the single factor <code>(+a+b+e+2*r+z+i)^(bizarre+brazier)</code>.
In the product of all elements the anagrams are to be found in the exponents consisting of a sum of at least two terms.
To find the longest deranged anagrams, we traverse the product list to find all exponents with multiple words, check that the length of the first word is at least as long as the length of the longest deranged anagram up to now, and check each pair of words for being deranged.
If a pair of deranged anagrams is found with more letters than previously found deranged anagrams, the earlier finds are forgotten. If the new anagrams are the same length, however, they are added to the output.

The Bracmat solution to the similar task [anagrams](/tasks/anagrams) skips the explicit merge sort and instead prepends new factors directly to the product one by one.
Bracmat shuffles each new factor into place to keep the growing product normalized before continuing with the next word from the list.
The result is exactly the same, but the running time becomes much longer.

```bracmat
  get$("unixdict.txt",STR):?wordList
& 1:?product
& :?unsorted
&   whl
  ' ( @(!wordList:(%?word:?letterString) \n ?wordList)
    & :?letterSum
    &   whl
      ' ( @(!letterString:%?letter ?letterString)
        &   (!letter:~#|str$(N !letter))+!letterSum
          : ?letterSum
        )
    & !letterSum^!word !unsorted:?unsorted
    )
& ( mergeSort
  =   newL L first second
    .   !arg:?L
      &   whl
        ' ( !L:% %
          & :?newL
          &   whl
            ' ( !L:%?first %?second ?L
              & !first*!second !newL:?newL
              )
          & !L !newL:?L
          )
      & !L
  )
& mergeSort$!unsorted:?product
& 0:?maxLength:?oldMaxLength
& :?derangedAnagrams
& ( deranged
  =   nextLetter Atail Btail
    .   !arg
      : ( (.)
        |   ( @(?:%@?nextLetter ?Atail)
            . @(?:(%@:~!nextLetter) ?Btail)
            )
          & deranged$(!Atail.!Btail)
        )
  )
& (   !product
    :   ?
      *   ?
        ^ ( %+%
          : @(%:? ([~<!maxLength:[?maxLength))+?
          :   ?
            + %@?anagramA
            + ?
            + %@?anagramB
            + ( ?
              & deranged$(!anagramA.!anagramB)
              &     (!anagramA.!anagramB)
                    (   !maxLength:>!oldMaxLength:?oldMaxLength
                      &
                    | !derangedAnagrams
                    )
                : ?derangedAnagrams
              & ~
              )
          )
      * ?
  | out$!derangedAnagrams
  );
```

{{out}}

```txt
excitation.intoxicate
```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/stat.h>

// Letter lookup by frequency.  This is to reduce word insertion time.
const char *freq = "zqxjkvbpygfwmucldrhsnioate";
int char_to_idx[128];

// Trie structure of sorts
struct word {
	const char *w;
	struct word *next;
};

union node {
	union node *down[10];
	struct word *list[10];
};

int deranged(const char *s1, const char *s2)
{
	int i;
	for (i = 0; s1[i]; i++)
		if (s1[i] == s2[i]) return 0;
	return 1;
}

int count_letters(const char *s, unsigned char *c)
{
	int i, len;
	memset(c, 0, 26);
	for (len = i = 0; s[i]; i++) {
		if (s[i] < 'a' || s[i] > 'z')
			return 0;
		len++, c[char_to_idx[(unsigned char)s[i]]]++;
	}
	return len;
}

const char * insert(union node *root, const char *s, unsigned char *cnt)
{
	int i;
	union node *n;
	struct word *v, *w = 0;

	for (i = 0; i < 25; i++, root = n) {
		if (!(n = root->down[cnt[i]]))
			root->down[cnt[i]] = n = calloc(1, sizeof(union node));
	}

	w = malloc(sizeof(struct word));
	w->w = s;
	w->next = root->list[cnt[25]];
	root->list[cnt[25]] = w;

	for (v = w->next; v; v = v->next) {
		if (deranged(w->w, v->w))
			return v->w;
	}
	return 0;
}

int main(int c, char **v)
{
	int i, j = 0;
	char *words;
	struct stat st;

	int fd = open(c < 2 ? "unixdict.txt" : v[1], O_RDONLY);
	if (fstat(fd, &st) < 0) return 1;

	words = malloc(st.st_size);
	read(fd, words, st.st_size);
	close(fd);

	union node root = {{0}};
	unsigned char cnt[26];
	int best_len = 0;
	const char *b1, *b2;

	for (i = 0; freq[i]; i++)
		char_to_idx[(unsigned char)freq[i]] = i;

	/* count words, change newline to null */
	for (i = j = 0; i < st.st_size; i++) {
		if (words[i] != '\n') continue;
		words[i] = '\0';

		if (i - j > best_len) {
			count_letters(words + j, cnt);
			const char *match = insert(&root, words + j, cnt);

			if (match) {
				best_len = i - j;
				b1 = words + j;
				b2 = match;
			}
		}

		j = ++i;
	}

	if (best_len) printf("longest derangement: %s %s\n", b1, b2);

	return 0;
}
```

{{out}}

```txt

longest derangement: intoxicate excitation

```



## C++


```cpp
#include <algorithm>
#include <fstream>
#include <functional>
#include <iostream>
#include <map>
#include <numeric>
#include <set>
#include <string>

bool is_deranged(const std::string& left, const std::string& right)
{
    return (left.size() == right.size()) &&
        (std::inner_product(left.begin(), left.end(), right.begin(), 0, std::plus<int>(), std::equal_to<char>()) == 0);
}

int main()
{
    std::ifstream input("unixdict.txt");
    if (!input) {
        std::cerr << "can't open input file\n";
        return EXIT_FAILURE;
    }

    typedef std::set<std::string> WordList;
    typedef std::map<std::string, WordList> AnagraMap;
    AnagraMap anagrams;

    std::pair<std::string, std::string> result;
    size_t longest = 0;

    for (std::string value; input >> value; /**/) {
        std::string key(value);
        std::sort(key.begin(), key.end());

        if (longest < value.length()) { // is it a long candidate?
            if (0 < anagrams.count(key)) { // is it an anagram?
                for (const auto& prior : anagrams[key]) {
                    if (is_deranged(prior, value)) { // are they deranged?
                        result = std::make_pair(prior, value);
                        longest = value.length();
                    }
                }
            }
        }
        anagrams[key].insert(value);
    }

    std::cout << result.first << ' ' << result.second << '\n';
    return EXIT_SUCCESS;
}
```

{{out}}

```txt

excitation intoxicate

```



## C#

{{libheader|System}}
{{libheader|System.Collections.Generic}}
{{libheader|System.Linq}}
{{libheader|System.IO}}
{{works with|C sharp|6}}

```c#
public static void Main()
{
    var lookupTable = File.ReadLines("unixdict.txt").ToLookup(line => AnagramKey(line));
    var query = from a in lookupTable
        orderby a.Key.Length descending
        let deranged = FindDeranged(a)
        where deranged != null
        select deranged[0] + " " + deranged[1];
    Console.WriteLine(query.FirstOrDefault());
}

static string AnagramKey(string word) => new string(word.OrderBy(c => c).ToArray());

static string[] FindDeranged(IEnumerable<string> anagrams) => (
    from first in anagrams
    from second in anagrams
    where !second.Equals(first)
        && Enumerable.Range(0, first.Length).All(i => first[i] != second[i])
    select new [] { first, second })
    .FirstOrDefault();

```

{{out}}

```txt

excitation intoxicate

```



## Clojure


```Clojure

(->> (slurp "unixdict.txt") ; words
     (re-seq #"\w+")        ; |
     (group-by sort)        ; anagrams
     vals                   ; |
     (filter second)        ; |
     (remove #(some true? (apply map = %))) ; deranged
     (sort-by #(count (first %)))
     last
     prn)
```

{{out}}

```txt
$ lein exec deranged.clj
["excitation" "intoxicate"]
```



## CoffeeScript

This example was tested with node.js.

```coffeescript
http = require 'http'

is_derangement = (word1, word2) ->
  for c, i in word1
    return false if c == word2[i]
  true

show_longest_derangement = (word_lst) ->
  anagrams = {}
  max_len = 0

  for word in word_lst
    continue if word.length < max_len
    key = word.split('').sort().join('')
    if anagrams[key]
      for prior in anagrams[key]
        if is_derangement(prior, word)
          max_len = word.length
          result = [prior, word]
    else
      anagrams[key] = []
    anagrams[key].push word

  console.log "Longest derangement: #{result.join ' '}"

get_word_list = (process) ->
  options =
    host: "www.puzzlers.org"
    path: "/pub/wordlists/unixdict.txt"

  req = http.request options, (res) ->
    s = ''
    res.on 'data', (chunk) ->
      s += chunk
    res.on 'end', ->
      process s.split '\n'
  req.end()

get_word_list show_longest_derangement
```

{{out}}

```txt

> coffee anagrams.coffee
Longest derangement: excitation intoxicate

```



## Common Lisp


```lisp
(defun read-words (file)
  (with-open-file (stream file)
    (loop with w = "" while w collect (setf w (read-line stream nil)))))

(defun deranged (a b)
  (loop for ac across a for bc across b always (char/= ac bc)))

(defun longest-deranged (file)
  (let ((h (make-hash-table :test #'equal))
	(wordlist (sort (read-words file)
			#'(lambda (x y) (> (length x) (length y))))))
    (loop for w in wordlist do
	  (let* ((ws (sort (copy-seq w) #'char<))
		 (l (gethash ws h)))
	    (loop for w1 in l do
		  (if (deranged w w1)
		    (return-from longest-deranged (list w w1))))
	    (setf (gethash ws h) (cons w l))))))

(format t "~{~A~%~^~}" (longest-deranged "unixdict.txt"))
```

{{out}}

```txt
intoxicate
excitation
```



## D


### Short Version


```d
void main() {
    import std.stdio, std.file, std.algorithm, std.string, std.array;

    string[][dstring] anags;
    foreach (const w; "unixdict.txt".readText.split)
        anags[w.array.sort().release.idup] ~= w;

    anags
    .byValue
    .map!(words => words.cartesianProduct(words)
                   .filter!q{ a[].equal!q{ a != b }})
    .join
    .minPos!q{ a[0].length > b[0].length }[0]
    .writeln;
}
```

{{out}}

```txt
Tuple!(string, string)("intoxicate", "excitation")
```

Runtime: about 0.11 seconds with LDC2 compiler.

Using const(ubytes)[] instead of dstrings gives a runtime of about 0.07 seconds:

```d
    string[][ubyte[]] anags;
    foreach (const w; "unixdict.txt".readText.split)
        anags[w.dup.representation.sort().release.assumeUnique] ~= w;
```



### Faster Version


```d
import std.stdio, std.file, std.algorithm, std.string, std.array,
       std.functional, std.exception;

string[2][] findDeranged(in string[] words) pure nothrow /*@safe*/ {
    // return words
    //        .map!representation
    //        .pairwise
    //        .filter!(ww => ww[].equal!q{ a != b });
    typeof(return) result;
    foreach (immutable i, immutable w1; words)
        foreach (immutable w2; words[i + 1 .. $])
            if (w1.representation.equal!q{ a != b }(w2.representation))
                result ~= [w1, w2];
    return result;
}

void main() /*@safe*/ {
    Appender!(string[])[30] wClasses;
    foreach (const w; "unixdict.txt".readText.splitter)
        wClasses[$ - w.length] ~= w;

    foreach (const ws; wClasses[].map!q{ a.data }.filter!(not!empty)) {
        string[][const ubyte[]] anags; // Assume ASCII input.
        foreach (immutable w; ws)
            anags[w.dup.representation.sort().release.assumeUnique] ~= w;
        auto pairs = anags.byValue.map!findDeranged.joiner;
        if (!pairs.empty)
            return writefln("Longest deranged: %-(%s %)", pairs.front);
    }
}
```

{{out}}

```txt
Longest deranged: excitation intoxicate
```

Runtime: about 0.03 seconds.


## EchoLisp

For a change, we use the french dictionary  included in EchoLisp package.

```scheme

(lib 'hash)
(lib 'struct)
(lib 'sql)
(lib 'words)


(define H (make-hash))

(define (deranged w1 w2)
	(for ((a w1) (b w2))
		#:break (string=? a b) => #f
		#t))

(define (anagrams (normal) (name) (twins))
(for ((w *words*))
	(set! name (word-name w))
	(set! normal (list->string (list-sort string<? (string->list name))))
	(set! twins (or (hash-ref H normal) null))
	#:continue (member name twins)
	#:when  (or (null? twins)  (for/or ((anagram twins)) (deranged name anagram)))
	(hash-set H normal (cons name twins))))


(define (task (lmin 8))
(anagrams)
(for ((lw (hash-values H))) ;; lw = list of words
	#:continue (= (length lw) 1)
	#:continue (< (string-length (first lw)) lmin)
	(set! lmin (string-length (first lw)))
	(write lmin) (for-each write lw)
	(writeln)))

```

{{out}}

```scheme

(lib 'dico.fr.no-accent) ;; 209315 words into *words* table
(task)

[...]
13 tractionnaire contrariaient
13 ressourcement contremesures
13 saintsimonien inseminations
14 tergiversation interrogatives
14 suralimenterai mineralisateur
14 transoceaniens reconnaissante

(lib 'dico.en ) ;;  235886 words
(task)

[...]

12 reaccomplish accomplisher
12 chromatician achromatinic
12 unaccumulate acutenaculum
14 charlatanistic antarchistical
15 megachiropteran cinematographer
17 misconstitutional constitutionalism

```



## Eiffel


```Eiffel

class
	ANAGRAMS_DERANGED

create
	make

feature

	make
			-- Longest deranged anagram.
		local
			deranged_anagrams: LINKED_LIST [STRING]
			count: INTEGER
		do
			read_wordlist
			across
				words as wo
			loop
				deranged_anagrams := check_list_for_deranged (wo.item)
				if not deranged_anagrams.is_empty and deranged_anagrams [1].count > count then
					count := deranged_anagrams [1].count
				end
				wo.item.wipe_out
				wo.item.append (deranged_anagrams)
			end
			across
				words as wo
			loop
				across
					wo.item as w
				loop
					if w.item.count = count then
						io.put_string (w.item + "%T")
						io.new_line
					end
				end
			end
		end

	original_list: STRING = "unixdict.txt"

feature {NONE}

	check_list_for_deranged (list: LINKED_LIST [STRING]): LINKED_LIST [STRING]
			-- Deranged anagrams in 'list'.
		do
			create Result.make
			across
				1 |..| list.count as i
			loop
				across
					(i.item + 1) |..| list.count as j
				loop
					if check_for_deranged (list [i.item], list [j.item]) then
						Result.extend (list [i.item])
						Result.extend (list [j.item])
					end
				end
			end
		end

	check_for_deranged (a, b: STRING): BOOLEAN
			-- Are 'a' and 'b' deranged anagrams?
		local
			n: INTEGER
		do
			across
				1 |..| a.count as i
			loop
				if a [i.item] = b [i.item] then
					n := n + 1
				end
			end
			Result := n = 0
		end

	read_wordlist
			-- Hashtable 'words' with alphabetically sorted Strings used as key.
		local
			l_file: PLAIN_TEXT_FILE
			sorted: STRING
			empty_list: LINKED_LIST [STRING]
		do
			create l_file.make_open_read_write (original_list)
			l_file.read_stream (l_file.count)
			wordlist := l_file.last_string.split ('%N')
			l_file.close
			create words.make (wordlist.count)
			across
				wordlist as w
			loop
				create empty_list.make
				sorted := sort_letters (w.item)
				words.put (empty_list, sorted)
				if attached words.at (sorted) as ana then
					ana.extend (w.item)
				end
			end
		end

	wordlist: LIST [STRING]

	sort_letters (word: STRING): STRING
			--Alphabetically sorted.
		local
			letters: SORTED_TWO_WAY_LIST [STRING]
		do
			create letters.make
			create Result.make_empty
			across
				1 |..| word.count as i
			loop
				letters.extend (word.at (i.item).out)
			end
			across
				letters as s
			loop
				Result.append (s.item)
			end
		end

	words: HASH_TABLE [LINKED_LIST [STRING], STRING]

end

```

{{out}}

```txt

excitation
intoxicate

```



## Elixir

{{trans|Ruby}}

```elixir
defmodule Anagrams do
  def deranged(fname) do
    File.read!(fname)
    |> String.split
    |> Enum.map(fn word -> to_charlist(word) end)
    |> Enum.group_by(fn word -> Enum.sort(word) end)
    |> Enum.filter(fn {_,words} -> length(words) > 1 end)
    |> Enum.sort_by(fn {key,_} -> -length(key) end)
    |> Enum.find(fn {_,words} -> find_derangements(words) end)
  end

  defp find_derangements(words) do
    comb(words,2) |> Enum.find(fn [a,b] -> deranged?(a,b) end)
  end

  defp deranged?(a,b) do
    Enum.zip(a, b) |> Enum.all?(fn {chr_a,chr_b} -> chr_a != chr_b end)
  end

  defp comb(_, 0), do: [[]]
  defp comb([], _), do: []
  defp comb([h|t], m) do
    (for l <- comb(t, m-1), do: [h|l]) ++ comb(t, m)
  end
end

case Anagrams.deranged("/work/unixdict.txt") do
  {_, words} -> IO.puts "Longest derangement anagram: #{inspect words}"
  _          -> IO.puts "derangement anagram: nothing"
end
```


{{out}}

```txt

Longest derangement anagram: ["intoxicate", "excitation"]

```



## Erlang

Using anagrams:fetch/2 from [[Anagrams]] and init_http/0 from [[Rosetta_Code/Find_unimplemented_tasks]]. Exporting words_from_url/1 to [[Ordered_words]].

```Erlang

-module( anagrams_deranged ).
-export( [task/0, words_from_url/1] ).

task() ->
       find_unimplemented_tasks:init_http(),
       Words = words_from_url( "http://www.puzzlers.org/pub/wordlists/unixdict.txt" ),
       Anagram_dict = anagrams:fetch( Words, dict:new() ),
       Deranged_anagrams = deranged_anagrams( Anagram_dict ),
       {_Length, Longest_anagrams} = dict:fold( fun keep_longest/3, {0, []}, Deranged_anagrams ),
       Longest_anagrams.

words_from_url( URL ) ->
	{ok, {{_HTTP, 200, "OK"}, _Headers, Body}} = httpc:request( URL ),
	string:tokens( Body, "\n" ).



deranged_anagrams( Dict ) ->
        Deranged_dict = dict:map( fun deranged_words/2, Dict ),
        dict:filter( fun is_anagram/2, Deranged_dict ).

deranged_words( _Key, [H | T] ) ->
        [{H, X} || X <- T, is_deranged_word(H, X)].

keep_longest( _Key, [{One, _} | _]=New, {Length, Acc} ) ->
        keep_longest_new( erlang:length(One), Length, New, Acc ).

keep_longest_new( New_length, Acc_length, New, _Acc ) when New_length > Acc_length ->
        {New_length, New};
keep_longest_new( New_length, Acc_length, New, Acc ) when New_length =:= Acc_length ->
        {Acc_length, Acc ++ New};
keep_longest_new( _New_length, Acc_length, _New, Acc ) ->
        {Acc_length, Acc}.

is_anagram( _Key, [] ) -> false;
is_anagram( _Key, _Value ) -> true.

is_deranged_word( Word1, Word2 ) ->
        lists:all( fun is_deranged_char/1, lists:zip(Word1, Word2) ).

is_deranged_char( {One, Two} ) -> One =/= Two.

```

{{out}}

```txt

8> anagrams_deranged:task().
[{"excitation","intoxicate"}]

```


=={{header|F_Sharp|F#}}==

```fsharp
open System;

let keyIsSortedWord = Seq.sort >> Seq.toArray >> String
let isDeranged = Seq.forall2 (<>)

let rec pairs acc l = function
| [] -> acc
| x::rtail ->
    pairs (acc @ List.fold (fun acc y -> (y, x)::acc) [] l) (x::l) rtail


[<EntryPoint>]
let main args =
    System.IO.File.ReadAllLines("unixdict.txt")
    |> Seq.groupBy keyIsSortedWord
    |> Seq.fold (fun (len, found) (key, words) ->
        if String.length key < len || Seq.length words < 2 then (len, found)
        else
            let d = List.filter (fun (a, b) -> isDeranged a b) (pairs [] [] (List.ofSeq words))
            if List.length d = 0 then (len, found)
            elif String.length key = len then (len, found @ d)
            else (String.length key, d)
    ) (0, [])
    |> snd
    |> printfn "%A"
    0
```

{{out}}

```txt
[("excitation", "intoxicate")]
```



## Factor


```factor
USING: assocs fry io.encodings.utf8 io.files kernel math
math.combinatorics sequences sorting strings ;
IN: rosettacode.deranged-anagrams

: derangement? ( str1 str2 -- ? ) [ = not ] 2all? ;
: derangements ( seq -- seq )
    2 [ first2 derangement? ] filter-combinations ;

: parse-dict-file ( path -- hash )
    utf8 file-lines
    H{ } clone [
        '[
            [ natural-sort >string ] keep
            _ [ swap suffix  ] with change-at
        ] each
    ] keep ;

: anagrams ( hash -- seq ) [ nip length 1 > ] assoc-filter values ;

: deranged-anagrams ( path -- seq )
    parse-dict-file anagrams [ derangements ] map concat ;

: longest-deranged-anagrams ( path -- anagrams )
    deranged-anagrams [ first length ] sort-with last ;
```


    "unixdict.txt" longest-deranged-anagrams .
    { "excitation" "intoxicate" }


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Type IndexedWord
  As String word
  As Integer index
End Type

' selection sort, quick enough for sorting small number of letters
Sub sortWord(s As String)
  Dim As Integer i, j, m, n = Len(s)
  For i = 0 To n - 2
    m = i
    For j = i + 1 To n - 1
      If s[j] < s[m] Then m = j
    Next j
    If m <> i Then Swap s[i], s[m]
  Next i
End Sub

' quicksort for sorting whole dictionary of IndexedWord instances by sorted word
Sub quicksort(a() As IndexedWord, first As Integer, last As Integer)
  Dim As Integer length = last - first + 1
  If length < 2 Then Return
  Dim pivot As String = a(first + length\ 2).word
  Dim lft As Integer = first
  Dim rgt As Integer = last
  While lft <= rgt
    While a(lft).word < pivot
      lft +=1
    Wend
    While a(rgt).word > pivot
      rgt -= 1
    Wend
    If lft <= rgt Then
       Swap a(lft), a(rgt)
       lft += 1
       rgt -= 1
    End If
  Wend
  quicksort(a(), first, rgt)
  quicksort(a(), lft, last)
End Sub

Function isDeranged(s1 As String, s2 As String) As Boolean
  For i As Integer = 0 To Len(s1) - 1
    If s1[i] = s2[i] Then Return False
  Next
  Return True
End Function

Dim t As Double = timer
Dim As String w()  '' array to hold actual words
Open "undict.txt" For Input As #1
Dim count As Integer = 0
While Not Eof(1)
  count +=1
  Redim Preserve w(1 To count)
  Line Input #1, w(count)
Wend
Close #1

Dim As IndexedWord iw(1 To count) '' array to hold sorted words and their index into w()
Dim word As String
For i As Integer = 1 To count
  word = w(i)
  sortWord(word)
  iw(i).word = word
  iw(i).index = i
Next
quickSort iw(), 1, count  '' sort the IndexedWord array by sorted word

Dim As Integer startIndex = 1, maxLength, ub
Dim As Integer maxIndex()
Dim As IndexedWord iWord = iw(1)
maxLength = 0

For i As Integer = 2 To count
  If iWord.word = iw(i).word Then
    If isDeranged(w(iword.index), w(iw(i).index)) Then
      If startIndex + 1 < i Then Swap iw(i), iw(startIndex + 1)
      If Len(iWord.word) > maxLength Then
        maxLength = Len(iWord.word)
        Erase maxIndex
        ub = 1
        Redim maxIndex(1 To ub)
        maxIndex(ub) = startIndex
        startIndex += 2
        i = startIndex
        iWord = iw(i)
      ElseIf Len(iWord.word) = maxLength Then
        ub += 1
        Redim Preserve maxIndex(1 To ub)
        maxIndex(ub) = startIndex
        startIndex += 2
        i = startIndex
        iWord = iw(i)
      End If
    End If
  ElseIf i = count Then
    Exit For
  Else
    For j As Integer = i To count - 1
      iWord = iw(j)
      If Len(iWord.word) >= maxLength Then
        startIndex = j
        i = startIndex
        Exit For
      End If
    Next
  End If
Next

Print Str(count); " words in the dictionary"
Print "The deranged anagram pair(s) with the greatest length (namely"; maxLength; ") is:"
Print
Dim iws(1 To maxLength) As IndexedWord  '' array to hold each deranged anagram pair
For i As Integer = 1 To UBound(maxIndex)
  For j As Integer = maxIndex(i) To maxIndex(i) + 1
    iws(j - maxIndex(i) + 1) = iw(j)
  Next j
  If iws(1).index > iws(2).index Then swap iws(1), iws(2) '' ensure pair is in correct order
  For j As Integer = 1 To 2
    Print w(iws(j).index); " ";
  Next j
  Print
Next i

Print
Print "Took ";
Print Using "#.###"; timer - t;
Print " seconds on i3 @ 2.13 GHz"

Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

25104 words in the dictionary
The deranged anagram pair(s) with the greatest length (namely 10) is:

excitation intoxicate

Took 0.089 seconds on i3 @ 2.13 GHz

```



## GAP

Using function [[Anagrams#GAP|Anagrams]].

```gap
IsDeranged := function(a, b)
	local i, n;
	for i in [1 .. Size(a)] do
		if a[i] = b[i] then
			return false;
		fi;
	od;
	return true;
end;

# This solution will find all deranged pairs of any length.
Deranged := function(name)
	local sol, ana, u, v;
	sol := [ ];
	ana := Anagrams(name);
	for u in ana do
		for v in Combinations(u, 2) do
			if IsDeranged(v[1], v[2]) then
				Add(sol, v);
			fi;
		od;
	od;
	return sol;
end;

# Now we find all deranged pairs of maximum length
a := Deranged("unixdict.txt");;
n := Maximum(List(a, x -> Size(x[1])));
Filtered(a, x -> Size(x[1]) = n);
# [ [ "excitation", "intoxicate" ] ]
```



## Go


```go
package main
import (
	"fmt"
	"io/ioutil"
	"strings"
	"sort"
)

func deranged(a, b string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range(a) {
		if a[i] == b[i] { return false }
	}
	return true
}

func main() {
	/* read the whole thing in. how big can it be? */
	buf, _ := ioutil.ReadFile("unixdict.txt")
	words := strings.Split(string(buf), "\n")

	m := make(map[string] []string)
	best_len, w1, w2 := 0, "", ""

	for _, w := range(words) {
		// don't bother: too short to beat current record
		if len(w) <= best_len { continue }

		// save strings in map, with sorted string as key
		letters := strings.Split(w, "")
		sort.Strings(letters)
		k := strings.Join(letters, "")

		if _, ok := m[k]; !ok {
			m[k] = []string { w }
			continue
		}

		for _, c := range(m[k]) {
			if deranged(w, c) {
				best_len, w1, w2 = len(w), c, w
				break
			}
		}

		m[k] = append(m[k], w)
	}

	fmt.Println(w1, w2, ": Length", best_len)
}
```

{{out}}

```txt

excitation intoxicate : Length 10

```



## Groovy

Solution:

```groovy
def map = new TreeMap<Integer,Map<String,List<String>>>()

new URL('http://www.puzzlers.org/pub/wordlists/unixdict.txt').eachLine { word ->
    def size = - word.size()
    map[size] = map[size] ?: new TreeMap<String,List<String>>()
    def norm = word.toList().sort().sum()
    map[size][norm] = map[size][norm] ?: []
    map[size][norm] << word
}

def result = map.findResult { negasize, normMap ->
    def size = - negasize
    normMap.findResults { x, anagrams ->
        def n = anagrams.size()
        (0..<(n-1)).findResults { i ->
            ((i+1)..<n).findResult { j ->
                (0..<size).every { k -> anagrams[i][k] != anagrams[j][k] } \
                    ? anagrams[i,j]
                    : null
            }
        }?.flatten() ?: null
    }?.flatten() ?: null
}

if (result) {
    println "Longest deranged anagram pair: ${result}"
} else {
    println 'Deranged anagrams are a MYTH!'
}
```


{{out}}

```txt
Longest deranged anagram pair: [excitation, intoxicate]
```



## Haskell

If the longest deranged anagram includes three or more words we'll only print two of them.  We also correctly handle duplicate words in the input.

```haskell
import Control.Arrow
import Data.List
import Data.Ord
import qualified Data.Map as M
import qualified Data.Set as S

-- Group lists of words based on their "signatures".  A signature is a sorted
-- list of characters.  Handle duplicate input words by storing them in sets.
groupBySig = map (sort &&& S.singleton)

-- Convert groups to lists of equivalent words.
equivs = map (S.toList . snd) . M.toList . M.fromListWith S.union

-- Indicate whether the pair of words differ in all character positions.
isDerangement (a, b) = and $ zipWith (/=) a b

-- Return all pairs of elements, ignoring order.
pairs = concat . unfoldr step
  where step (x:xs) = Just (map ((,) x) xs, xs)
        step []     = Nothing

-- Return all anagram pairs in the input string.
anagrams = concatMap pairs . equivs . groupBySig

-- Return the pair of words making the longest deranged anagram.
maxDerangedAnagram = maxByLen . filter isDerangement . anagrams
  where maxByLen [] = Nothing
        maxByLen xs = Just $ maximumBy (comparing (length . fst)) xs

main :: IO ()
main = do
  input <- getContents
  case maxDerangedAnagram $ words input of
    Nothing     -> putStrLn "No deranged anagrams were found."
    Just (a, b) -> putStrLn $ "Longest deranged anagrams: " ++ a ++ " and " ++ b
```

{{out}}

```txt

Longest deranged anagrams: excitation and intoxicate

```


=={{header|Icon}} and {{header|Unicon}}==
This solution (which works in both languages) does a strict interpretation of the problem and ignores the fact that there may be multiple derangements that are the same length (including ignoring multiple derangements arising from the same set of words that are all anagrams).

```unicon
link strings     # for csort() procedure

procedure main()
    anagrams := table()                     # build lists of anagrams
    every *(word := !&input) > 1 do {
        canon := csort(word)
        /anagrams[canon] := []
        put(anagrams[canon], word)
        }

    longest := 1                            # find a longest derangement
    every *(aList := !anagrams) > 1 do
        if derangement := derange(aList) then
            if longest <:= *derangement[1] then long := derangement

    every writes((!\long||" ")|"\n")        # show longest
end

procedure derange(aList)    # Return a single derangement from this list
    while aWord := get(aList) do return noMatch(aWord, !aList)
end

procedure noMatch(s1,s2)    # Produce pair only if s1,s2 are deranged.
    every i := 1 to *s1 do if s1[i] == s2[i] then fail
    return [s1,s2]
end
```

{{out|Sample run}}

```txt
->dra <unixdict.txt
excitation intoxicate
->
```



## J

This assumes that [http://www.puzzlers.org/pub/wordlists/unixdict.txt unixdict.txt] has been saved in the current directory.

```j
   #words=: 'b' freads 'unixdict.txt'
25104
   #anagrams=: (#~ 1 < #@>) (</.~ /:~&>) words
1303
   #maybederanged=: (#~ (1 -.@e. #@~."1)@|:@:>&>) anagrams
432
   #longest=: (#~ [: (= >./) #@>@{.@>) maybederanged
1
   longest
┌───────────────────────┐
│┌──────────┬──────────┐│
││excitation│intoxicate││
│└──────────┴──────────┘│
└───────────────────────┘
```

Note that anagram sets with more than two members might, hypothetically, have made things more complicated.  By lucky coincidence, this was not an issue.  We could have taken advantage of that coincidence to achieve slight further simplifications.  Perhaps <code>maybederanged=: (#~ (-: ~."1)@|:@:>&>) anagrams</code>

In other words, if we had had to consider whether <code>ascertain/cartesian/sectarian</code> contained a deranged pair, we would have had to break it out into the three pairs it contains.  However, since 'excitation' is a longer word than 'ascertain', we know that this triple cannot contain the longest deranged anagram pair.  And since there are no anagrams longer than 'excitation' which involve more than a single pair, we know that we can ignore this issue entirely.


## Java

{{works with|Java|8}}

```java
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DerangedAnagrams {

    public static void main(String[] args) throws IOException {
        List<String> words = Files.readAllLines(new File("unixdict.txt").toPath());
        printLongestDerangedAnagram(words);
    }

    private static void printLongestDerangedAnagram(List<String> words) {
        words.sort(Comparator.comparingInt(String::length).reversed().thenComparing(String::toString));

        Map<String, ArrayList<String>> map = new HashMap<>();
        for (String word : words) {
            char[] chars = word.toCharArray();
            Arrays.sort(chars);
            String key = String.valueOf(chars);

            List<String> anagrams = map.computeIfAbsent(key, k -> new ArrayList<>());
            for (String anagram : anagrams) {
                if (isDeranged(word, anagram)) {
                    System.out.printf("%s %s%n", anagram, word);
                    return;
                }
            }
            anagrams.add(word);
        }
        System.out.println("no result");
    }

    private static boolean isDeranged(String word1, String word2) {
        for (int i = 0; i < word1.length(); i++) {
            if (word1.charAt(i) == word2.charAt(i)) {
                return false;
            }
        }
        return true;
    }
}
```

{{out}}

```txt
excitation intoxicate
```



## JavaScript



###  Spidermonkey


This example is a little long because
it tries to emphasize generality and clarity over
brevity.


```JavaScript
#!/usr/bin/env js

function main() {
    var wordList = read('unixdict.txt').split(/\s+/);
    var anagrams = findAnagrams(wordList);
    var derangedAnagrams = findDerangedAnagrams(anagrams);
    var longestPair = findLongestDerangedPair(derangedAnagrams);
    print(longestPair.join(' '));

}

function findLongestDerangedPair(danas) {
    var longestLen = danas[0][0].length;
    var longestPair = danas[0];
    for (var i in danas) {
        if (danas[i][0].length > longestLen) {
            longestLen = danas[i][0].length;
            longestPair = danas[i];
        }
    }
    return longestPair;
}

function findDerangedAnagrams(anagrams) {
    var deranged = [];

    function isDeranged(w1, w2) {
        for (var c = 0; c < w1.length; c++) {
            if (w1[c] == w2[c]) {
                return false;
            }
        }
        return true;
    }

    function findDeranged(anas) {
        for (var a = 0; a < anas.length; a++) {
            for (var b = a + 1; b < anas.length; b++) {
                if (isDeranged(anas[a], anas[b])) {
                    deranged.push([anas[a], anas[b]]);
                }
            }
        }
    }

    for (var a in anagrams) {
        var anas = anagrams[a];
        findDeranged(anas);
    }

    return deranged;
}

function findAnagrams(wordList) {
    var anagrams = {};

    for (var wordNum in wordList) {
        var word = wordList[wordNum];
        var key = word.split('').sort().join('');
        if (!(key in anagrams)) {
            anagrams[key] = [];
        }
        anagrams[key].push(word);
    }

    for (var a in anagrams) {
        if (anagrams[a].length < 2) {
            delete(anagrams[a]);
        }
    }

    return anagrams;
}

main();


```


{{out}}
 excitation intoxicate


###  Gecko

Word file is saved locally because browser won't fetch it cross-site.  Tested on Gecko.

```javascript><html><head><title>Intoxication</title></head

<body><pre id='x'>
```

<script type="application/javascript">

function show(t) {
	var l = document.createTextNode(t + '\n');
	document.getElementById('x').appendChild(l);
}

// get words; be ware of cross-site restrictions on XMLHttpRequest
var words = null;
var req = new XMLHttpRequest();
req.open('GET', 'file:///tmp/unixdict.txt', false);
req.send(null);
words = req.responseText.split('\n');

var idx = {};
for (var i = 0; i < words.length; i++) {
	var t = words[i].split('').sort().join('');
	if (idx[t]) idx[t].push(words[i]);
	else	    idx[t] = [words[i]];
}

var best = '';
var best_pair;
for (var i in idx) {
	if (i.length <= best.length) continue;
	if (idx[i].length == 1) continue;

	var a = idx[i], got = null;
	for (var j = 0, l1 = a[j]; j < a.length && !got; j++) {
		for (var k = j + 1, l2 = a[k]; k < a.length && !got; k++)
			for (var m = 0; m < l1.length || !(got = [l2]); m++)
				if (l1[m] == l2[m]) break;
		if (got) got.push(l1);
	}

	if (got) {
		best_pair = got;
		best = got[0];
	}
}

show(best_pair);
</script></body></html>
```


{{Out|Output (in a browser window)}}

```txt
intoxicate,excitation
```



## jq

{{works with|jq|1.5}}

This solution allows for the possibility of more than one answer.


```jq
# Input: an array of strings
# Output: a stream of arrays
def anagrams:
  reduce .[] as $word (
    {table: {}, max: 0};   # state
    ($word | explode | sort | implode) as $hash
    | .table[$hash] += [ $word ]
    | .max   = ([ .max, ( .table[$hash] | length) ] | max ) )
  | .table | .[]  | select(length>1);

# Check whether the input and y are deranged,
# on the assumption that they are anagrams:
def deranged(y):
  explode as $x                             # explode is fast
  | (y | explode) as $y
  | all( range(0;length); $x[.] != $y[.] );

# The task: loop through the anagrams,
# retaining only the best set of deranged anagrams so far.
split("\n") | select(length>0)              # read all the words as an array
| reduce anagrams as $words ([];            # loop through all the anagrams
    reduce $words[] as $v (.;
      reduce ($words - [$v])[] as $w (.;    # $v and $w are distinct members of $words
        if $v|deranged($w)
        then if length == 0 then [$v,$w]
             elif ($v|length) == (.[0]|length) then . + [$v,$w]
             elif ($v|length) >  (.[0]|length) then [$v,$w]
	     else .
	     end
        else .
        end) ) )
| unique
```


'''Invocation and output'''

```txt
    $ jq -M -s -c -R -f program.jq unixdict.txt
    ["excitation","intoxicate"]
```



## Julia



```julia
using Base.isless
# Let's define the less than operator for any two vectors that have the same type:
# This does lexicographic comparison, we use it on vectors of chars in this task.
function Base.isless(t1, t2)
    for (a, b) in zip(t1, t2) # zip only to the shorter length
        if !isequal(a, b)
            return isless(a, b)
        end
    end
    return length(t1) < length(t2)
end

# The sort function of Julia doesn't work on strings, so we write one:
# This returns a sorted vector of the chars of the given string
sortchars(s::AbstractString) = sort(collect(Char, s))

# Custom comparator function for sorting the loaded wordlist
sortanagr(s1::AbstractString, s2::AbstractString) =
    if length(s1) != length(s2) length(s1) < length(s2) else sortchars(s1) < sortchars(s2) end

# Tests if two strings are deranged anagrams, returns a bool:
# in our case s2 is never longer than s1
function deranged(s1::AbstractString, s2::AbstractString)
    # Tests for derangement first
    for (a, b) in zip(s1, s2)
        if a == b return false end
    end
    # s1 and s2 are deranged, but are they anagrams at all?
    return sortchars(s1) == sortchars(s2)
end

# Task starts here, we load the wordlist line by line, strip eol char, and sort the wordlist
# in a way that ensures that longer words come first and anagrams go next to each other
words = sort(open(readlines, "./data/unixdict.txt"), rev = true, lt = sortanagr)

# Now we just look for deranged anagrams in the neighbouring words of the sorted wordlist
for i in 1:length(words)-1
    if deranged(words[i], words[i+1])
        # The first match is guaranteed to be the longest due to the custom sorting
        println("The longest deranged anagrams are $(words[i]) and $(words[i+1])")
        break
    end
end
```


{{out}}

```txt
The longest deranged anagrams are excitation and intoxicate
```



## K


```K
   / anagram clusters
   a:{x g@&1<#:'g:={x@<x}'x}@0:"unixdict.txt";

   / derangements in these clusters
   b@&c=|/c:{#x[0]}'b:a@&{0=+//{x=y}':x}'a
("excitation"
 "intoxicate")
```



## Kotlin


```scala
// version 1.0.6

import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.URL

fun isDeranged(s1: String, s2: String): Boolean {
    return (0 until s1.length).none { s1[it] == s2[it] }
}

fun main(args: Array<String>) {
    val url = URL("http://www.puzzlers.org/pub/wordlists/unixdict.txt")
    val isr = InputStreamReader(url.openStream())
    val reader = BufferedReader(isr)
    val anagrams = mutableMapOf<String, MutableList<String>>()
    var count = 0
    var word = reader.readLine()
    while (word != null) {
        val chars = word.toCharArray()
        chars.sort()
        val key = chars.joinToString("")
        if (!anagrams.containsKey(key)) {
            anagrams.put(key, mutableListOf<String>())
            anagrams[key]!!.add(word)
        }
        else {
            val deranged = anagrams[key]!!.any { isDeranged(it, word) }
            if (deranged) {
                anagrams[key]!!.add(word)
                count = Math.max(count, word.length)
            }
        }
        word = reader.readLine()
    }
    reader.close()
    anagrams.values
        .filter { it.size > 1 && it[0].length == count }
        .forEach { println(it) }
}
```


{{out}}

```txt

[excitation, intoxicate]

```



## Lasso


```Lasso
local(
	anagrams	= map,
	words		= include_url('http://www.puzzlers.org/pub/wordlists/unixdict.txt') -> split('\n'),
	key,
	max		= 0,
	wordsize,
	findings	= array,
	derangedtest	= { // this code snippet is not executed until the variable is invoked. It will return true if the compared words are a deranged anagram
		local(
			w1		= #1,
			w2		= #2,
			testresult	= true
		)

		loop(#w1 -> size) => {
			#w1 -> get(loop_count) == #w2 -> get(loop_count) ? #testresult = false
		}
		return #testresult
	}
)

// find all anagrams
with word in #words do {
	#key = #word -> split('') -> sort& -> join('')
	not(#anagrams >> #key) ? #anagrams -> insert(#key = array)
	#anagrams -> find(#key) -> insert(#word)
}

// step thru each set of anagrams to find deranged ones
with ana in #anagrams
let ana_size = #ana -> size
where #ana_size > 1
do {
	#wordsize = #ana -> first -> size

	if(#wordsize >= #max) => {

		loop(#ana_size - 1) => {
			if(#derangedtest -> detach & invoke(#ana -> get(loop_count), #ana -> get(loop_count + 1))) => {
				// we only care to save the found deranged anagram if it is longer than the previous longest one
				if(#wordsize > #max) => {
					#findings = array(#ana -> get(loop_count) + ', ' + #ana -> get(loop_count + 1))
				else
					#findings -> insert(#ana -> get(loop_count) + ', ' + #ana -> get(loop_count + 1))
				}
				#max = #wordsize
			}
		}

	}
}

#findings -> join('<br />\n')
```


Result -> excitation, intoxicate


## Liberty BASIC


```lb
print "Loading dictionary file."
open "unixdict.txt" for input as #1
a$=input$(#1,lof(#1))
close #1

dim theWord$(30000)
dim ssWord$(30000)

c10$ = chr$(10)
i = 1
print "Creating array of words."
while instr(a$,c10$,i) <> 0
  j     = instr(a$,c10$,i)
  ln    = j - i
  again = 1
  sWord$ = mid$(a$,i,j-i)
  n = n + 1
 theWord$(n) = sWord$

 while again = 1
   again  = 0
   for kk = 1 to len(sWord$) - 1
   if mid$(sWord$,kk,1) > mid$(sWord$,kk +1,1) then
     sWord$ = left$(sWord$,kk-1);mid$(sWord$,kk+1,1);mid$(sWord$,kk,1);mid$(sWord$,kk+2)
     again  = 1
   end if
   next kk
 wend
 ssWord$(n) = sWord$
 i = j + 1
wend
print "Checking for deranged anagrams."
for i = 1 to n
  if len(theWord$(i)) > maxLen then
    for j = 1 to n
      if ssWord$(i) = ssWord$(j) and i <> j then
        cnt = 0
    for k = 1 to len(theWord$(i))
      if mid$(theWord$(i),k,1) = mid$(theWord$(j),k,1) then cnt = cnt + 1
    next k
    if cnt = 0 then
      maxLen = len(theWord$(i))
      maxPtrI = i
      maxPtrJ = j
    end if
      end if
    next j
  end if
next i

print theWord$(maxPtrI);" => ";theWord$(maxPtrJ)
end

```

{{out}}
excitation => intoxicate


## Maple


```Maple

with(StringTools):
dict:=Split([HTTP:-Get("www.puzzlers.org/pub/wordlists/unixdict.txt")][2]):
L:=[seq(select(t->HammingDistance(t,w)=length(w),[Anagrams(w,dict)])[],w=dict)]:
len:=length(ListTools:-FindMaximalElement(L,(a,b)->length(a)<length(b))):
select(w->length(w)=len,L)[];

```

{{out}}

```txt

                        "intoxicate", "excitation"

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica

words=First/@Import["http://www.puzzlers.org/pub/wordlists/unixdict.txt","Table"];
anagramDegrangement=Function[{w1,w2},
	Module[{c1=ToCharacterCode@w1,c2=ToCharacterCode@w2},
	Sort@c1==Sort@c2&&Select[c1-c2,#==0&,1]==={}]];
gs=Select[GatherBy[words,{StringLength@#,Union@ToCharacterCode@#}&],Length@#>=2&];
First@Flatten[Function[ws,Select[Join@@Outer[List,ws,ws,1],anagramDegrangement@@#&]]/@SortBy[gs,-StringLength@First@#&],1]

```

{{out}}

```txt

{"excitation", "intoxicate"}

```


A similar approach using Mathematica 10:

```Mathematica

list = Import["http://www.puzzlers.org/pub/wordlists/unixdict.txt","Lines"];
MaximalBy[
 Select[GatherBy[list, Sort@*Characters],
  Length@# > 1 && And @@ MapThread[UnsameQ, Characters /@ #] &],
 StringLength@*First]

```


{{out}}

```txt

{{"excitation", "intoxicate"}}

```



## OCaml


```ocaml
let sort_chars s =
  let r = String.copy s in
  for i = 0 to (String.length r) - 2 do
    for j = i + 1 to (String.length r) - 1 do
      if r.[i] > r.[j] then begin
        let tmp = r.[i] in
        r.[i] <- r.[j];
        r.[j] <- tmp;
      end
    done
  done;
  (r)

let deranged (s1, s2) =
  let len1 = String.length s1
  and len2 = String.length s2 in
  if len1 <> len2 then false else
  try
    for i = 0 to pred len1 do
      if s1.[i] = s2.[i] then raise Exit
    done;
    true
  with Exit -> false

let pairs_of_list lst =
  let rec aux acc = function
    | [] -> acc
    | x::xs ->
        aux (List.fold_left (fun acc y -> (x,y)::acc) acc xs) xs
  in
  aux [] lst

let () =
  let h = Hashtbl.create 3571 in
  let ic = open_in "unixdict.txt" in
  try while true do
    let word = input_line ic in
    let key = sort_chars word in
    let l =
      try Hashtbl.find h key
      with Not_found -> []
    in
    Hashtbl.replace h key (word::l);
  done with End_of_file ->
    close_in ic;
    let lst =
      Hashtbl.fold (fun _ lw acc ->
        if List.length lw < 2 then acc else lw::acc) h []
    in
    let lst =
      List.fold_left (fun acc anagrams ->
        let pairs = pairs_of_list anagrams in
        (List.filter deranged pairs) @ acc
      ) [] lst
    in
    let res, _ =
      List.fold_left (fun (res, n) (w1, w2) ->
        let len = String.length w1 in
        match Pervasives.compare len n with
        | 0 -> ((w1, w2)::res, n)
        | 1 -> ([w1, w2], len)
        | _ -> (res, n)
      ) ([], 0) lst
    in
    List.iter (fun (w1, w2) -> Printf.printf "%s, %s\n" w1 w2) res
```

{{out}}

```txt
$ ocaml deranged_anagram.ml
intoxicate, excitation
```



## ooRexx


```ooRexx

-- This assumes you've already downloaded the following file and placed it
-- in the current directory: http://www.puzzlers.org/pub/wordlists/unixdict.txt

-- There are several different ways of reading the file.  I chose the
-- supplier method just because I haven't used it yet in any other examples.
source = .stream~new('unixdict.txt')~supplier
-- this holds our mappings of the anagrams.  This is good use for the
-- relation class
anagrams = .relation~new
count = 0    -- this is used to keep track of the maximums

loop while source~available
    word = source~item
    -- this produces a string consisting of the characters in sorted order
    -- Note: the ~~ used to invoke sort makes that message return value be
    -- the target array.  The sort method does not normally have a return value.
    key = word~makearray('')~~sort~tostring("l", "")
    -- add this to our mapping.  This creates multiple entries for each
    -- word that uses the same key
    anagrams[key] = word
    source~next
end

-- now get the set of unique keys
keys = .set~new~~putall(anagrams~allIndexes)
-- the longest count tracker
longest = 0
-- our list of the longest pairs
pairs = .array~new

loop key over keys
    -- don't even bother doing the deranged checks for any key
    -- shorter than our current longest
    if key~length < longest then iterate

    words = anagrams~allAt(key)
    -- singletons aren't anagrams at all
    newCount = words~items
    loop i = 1 to newCount - 1
        word1 = words[i]
        loop j = 1 to newCount
            word2 = words[j]
            -- bitxor will have '00'x in every position where these
            -- strings match.  If found, go around and check the
            -- next one
            if word1~bitxor(word2)~pos('00'x) > 0 then iterate
            -- we have a match
            else do
                if word1~length > longest then do
                    -- throw away anything we've gathered so far
                    pairs~empty
                    longest = word1~length
                end
                pairs~append(.array~of(word1, word2))
            end
        end
    end
end

say "The longest deranged anagrams we found are:"
loop pair over pairs
     say pair[1] pair[2]
end

```

{{out}}

```txt

The longest deranged anagrams we found are:
intoxicate excitation

```



## PARI/GP


```parigp
dict=readstr("unixdict.txt");
len=apply(s->#s, dict);
getLen(L)=my(v=List()); for(i=1,#dict, if(len[i]==L, listput(v, dict[i]))); Vec(v);
letters(s)=vecsort(Vec(s));
getAnagrams(v)=my(u=List(),L=apply(letters,v),t,w); for(i=1,#v-1, w=List(); t=L[i]; for(j=i+1,#v, if(L[j]==t, listput(w, v[j]))); if(#w, listput(u, concat([v[i]], Vec(w))))); Vec(u);
deranged(s1,s2)=s1=Vec(s1);s2=Vec(s2); for(i=1,#s1, if(s1[i]==s2[i], return(0))); 1
getDeranged(v)=my(u=List(),w); for(i=1,#v-1, for(j=i+1,#v, if(deranged(v[i], v[j]), listput(u, [v[i], v[j]])))); Vec(u);
f(n)=my(t=getAnagrams(getLen(n))); if(#t, concat(apply(getDeranged, t)), []);
forstep(n=vecmax(len),1,-1, t=f(n); if(#t, return(t)))
```

{{out}}

```txt
%1 = [["excitation", "intoxicate"]]
```



## Perl


```Perl
sub deranged {                          # only anagrams ever get here
        my @a = split('', shift);       # split word into letters
        my @b = split('', shift);
        for (0 .. $#a) {
                $a[$_] eq $b[$_] and return;
        }
        return 1
}

sub find_deranged {
        for my $i ( 0 .. $#_ ) {
                for my $j ( $i+1 .. $#_ ) {
                        next unless deranged $_[$i], $_[$j];

                        print "length ", length($_[$i]), ": $_[$i] => $_[$j]\n";
                        return 1;
                }
        }
}

my %letter_list;
open my $in, 'unixdict.txt';

local $/ = undef;

for (split(' ', <$in>)) {
        # store anagrams in hash table by letters they contain
        push @{ $letter_list{ join('', sort split('', $_)) } }, $_
}

for (   sort { length($b) <=> length($a) }      # sort by length, descending
        grep { @{ $letter_list{$_} } > 1 }      # take only ones with anagrams
        keys %letter_list               )
{
        # if we find a pair, they are the longested due to the sort before
        last if find_deranged(@{ $letter_list{$_} });
}
```

{{out}}

```txt

length 10: excitation => intoxicate

```



## Perl 6


{{works with|Rakudo|2016.08}}


```perl6
my @anagrams = 'unixdict.txt'.IO.words
    .map(*.comb.cache)             # explode words into lists of characters
    .classify(*.sort.join).values  # group words with the same characters
    .grep(* > 1)                   # only take groups with more than one word
    .sort(-*[0])                   # sort by length of the first word
;

for @anagrams -> @group {
    for @group.combinations(2) -> [@a, @b] {
        if none @a Zeq @b {
            say "{@a.join}   {@b.join}";
            exit;
        }
    }
}
```


{{out}}

```txt
excitation   intoxicate
```



## Phix


```Phix
function deranged(string word1, string word2)
    if length(word1)!=length(word2) then ?9/0 end if -- sanity check
    for i=1 to length(word1) do
        if word1[i]==word2[i] then return 0 end if
    end for
    return 1
end function

integer fn = open("unixdict.txt","r")
sequence words = {}, anagrams = {}, last="", letters
object word
integer maxlen = 1

    while 1 do
        word = trim(gets(fn))
        if atom(word) then exit end if
        if length(word) then
            letters = sort(word)
            words = append(words, {letters, word})
        end if
    end while
    close(fn)

    words = sort(words)
    for i=1 to length(words) do
        {letters,word} = words[i]
        if letters=last then
            anagrams[$] = append(anagrams[$],word)
            anagrams[$][1] = length(word)
        else
            last = letters
            anagrams = append(anagrams,{0,word})
        end if
    end for

    anagrams = sort(anagrams)
    puts(1,"\nLongest deranged anagrams:\n")
    for i=length(anagrams) to 1 by -1 do
        last = anagrams[i]
        if last[1]<maxlen then exit end if
        for j=2 to length(last) do
            for k=j+1 to length(last) do
                if deranged(last[j],last[k]) then
                    puts(1,last[j]&", "&last[k]&"\n")
                    maxlen = last[1]
                end if
            end for
        end for
    end for
```

{{out}}

```txt

Longest deranged anagrams:
excitation, intoxicate

```



## PHP


```PHP
<?php
$words = file(
    'http://www.puzzlers.org/pub/wordlists/unixdict.txt',
    FILE_IGNORE_NEW_LINES
);
$length = 0;

foreach ($words as $word) {
    $chars = str_split($word);
    sort($chars);
    $chars = implode("", $chars);
    $length = strlen($chars);
    $anagrams[$length][$chars][] = $word;
}

krsort($anagrams);

foreach ($anagrams as $anagram) {
    $final_words = array();
    foreach ($anagram as $words) {
        if (count($words) >= 2) {
            $counts = array();
            foreach ($words as $word) {
                $counts[$word] = array($word);
                foreach ($words as $second_word) {
                    for ($i = 0, $length = strlen($word); $i < $length; $i++) {
                        if ($word[$i] === $second_word[$i]) continue 2;
                    }
                    $counts[$word][] = $second_word;
                }
            }
            $max = 0;
            $max_key = '';
            foreach ($counts as $name => $count) {
                if (count($count) > $max) {
                    $max = count($count);
                    $max_key = $name;
                }
            }
            if ($max > 1) {
                $final_words[] = $counts[$max_key];
            }
        }
    }
    if ($final_words) break;
}

foreach ($final_words as $final_word) {
    echo implode(" ", $final_word), "\n";
}
?>
```

{{out}}

```txt

excitation intoxicate

```



## PicoLisp


```PicoLisp
(let Words NIL
   (in "unixdict.txt"
      (while (line)
         (let (Word @  Key (pack (sort (copy @))))
            (if (idx 'Words Key T)
               (push (car @) Word)
               (set Key (list Word)) ) ) ) )
   (maxi '((X) (length (car X)))
      (extract
         '((Key)
            (pick
               '((Lst)
                  (and
                     (find
                        '((L) (not (find = L Lst)))
                        (val Key) )
                     (cons (pack @) (pack Lst)) ) )
               (val Key) ) )
         (idx 'Words) ) ) )
```

{{out}}

```txt
-> ("excitation" . "intoxicate")
```



## PowerShell


```PowerShell

function Test-Deranged ([string[]]$Strings)
{
    $array1 = $Strings[0].ToCharArray()

    for ($i = 1; $i -lt $Strings.Count; $i++)
    {
        $array2 = $Strings[$i].ToCharArray()

        for ($i = 0; $i -lt $array1.Count; $i++)
        {
            if ($array1[$i] -match $array2[$i])
            {
                return $false
            }
        }
    }

    return $true
}


$words = [System.Collections.ArrayList]@()

Get-Content -Path ".\unixdict.txt" |
    ForEach-Object { [void]$words.Add([PSCustomObject]@{Word=$_; SortedWord=(($_.ToCharArray() | Sort-Object) -join "")}) }

[object[]]$anagrams = $words | Group-Object -Property SortedWord | Where-Object -Property Count -GT 1 | Sort-Object {$_.Name.Length}
[string[]]$deranged = ($anagrams | ForEach-Object { if ((Test-Deranged $_.Group.Word)) {$_} } | Select-Object -Last 1).Group.Word

[PSCustomObject]@{
    Length = $deranged[0].Length
    Words  = $deranged
}

```

{{Out}}

```txt

Length Words
------ -----
    10 {excitation, intoxicate}

```



## Prolog

{{Works with|SWI Prolog}}

```Prolog
longest_deranged_anagram :-
	http_open('http://www.puzzlers.org/pub/wordlists/unixdict.txt',In,[]),
	read_file(In, [], Out),
	close(In),
	msort(Out, MOut),
	group_pairs_by_key(MOut, GPL),
	map_list_to_pairs(compute_len, GPL, NGPL),
	predsort(my_compare, NGPL, GPLSort),
	search_derangement(GPLSort).


% order tuples to have longest words first
my_compare(R, N1-(K1-E1), N2-(K2-E2)) :-
	(   N1 < N2 -> R = > ; N1 > N2 -> R = <;
	length(E1, L1),
	length(E2, L2),
	(   L1 < L2 -> R = <; L1 > L2 -> R = >; compare(R, K1, K2))).


compute_len(_-[H|_], Len) :-
	length(H, Len).


% check derangement of anagrams
derangement([], []).
derangement([H1|T1], [H2 | T2]) :-
	H1 \= H2,
	derangement(T1, T2).


search_derangement([_-(_-L) | T]) :-
	length(L, 1), !,
	search_derangement(T).


search_derangement([_-(_-L) | T]) :-
	(   search(L) -> true; search_derangement(T)).

search([]) :- fail.
search([H | T]) :-
	(   search_one(H, T) -> true; search(T)).


search_one(Word, L) :-
	include(derangement(Word), L, [H|_]),
	atom_codes(W, Word),
	atom_codes(W1, H),
	format('Longest deranged anagrams : ~w ~w ~n', [W, W1]).


read_file(In, L, L1) :-
	read_line_to_codes(In, W),
	(   W == end_of_file ->
	       L1 = L
	       ;
	       msort(W, W1),
	       atom_codes(A, W1),
	       read_file(In, [A-W | L], L1)).
```

{{out}}

```txt
 ?- longest_deranged_anagram.
Longest deranged anagrams : excitation intoxicate
true.

```



## PureBasic


```PureBasic
Structure anagram
  word.s
  letters.s
EndStructure

Structure  wordList
  List words.anagram()
EndStructure

#True = 1
#False = 0

Procedure.s sortLetters(*word.Character, wordLength)
  ;returns a string with the letters of a word sorted
  Protected Dim letters.c(wordLength)
  Protected *letAdr = @letters()
  CopyMemoryString(*word, @*letAdr)
  SortArray(letters(), #PB_Sort_Ascending, 0, wordLength - 1)
  ProcedureReturn PeekS(@letters(), wordLength)
EndProcedure

;Compare a list of anagrams for derangement.
Procedure isDeranged(List anagram.s())
  ;If a pair of deranged anagrams is found return #True
  ;and and modify the list to include the pair of deranged anagrams.
  Protected i, length, word.s, *ptrAnagram, isDeranged
  Protected NewList deranged.s()
  FirstElement(anagram())
  length = Len(anagram())
  Repeat
    word = anagram()
    *ptrAnagram = @anagram()

    While NextElement(anagram())
      isDeranged = #True
      For i = 1 To length
        If Mid(word, i, 1) = Mid(anagram(), i, 1)
          isDeranged = #False
          Break ;exit for/next
        EndIf
      Next

      If isDeranged
        AddElement(deranged())
        deranged() = anagram()
        AddElement(deranged())
        deranged() = word
        CopyList(deranged(), anagram())
        ProcedureReturn #True ;deranged anagram found
      EndIf
    Wend
    ChangeCurrentElement(anagram(), *ptrAnagram)
  Until Not NextElement(anagram())

  ProcedureReturn #False ;deranged anagram not found
EndProcedure

If OpenConsole()
  ;word file is assumed to be in the same directory
  If Not ReadFile(0,"unixdict.txt"): End: EndIf

  Define maxWordSize = 0, word.s, length
  Dim wordlists.wordList(maxWordSize)

  ;Read word file and create separate lists of anagrams and their original
  ;words by length.
  While Not Eof(0)
    word = ReadString(0)
    length = Len(word)
    If length > maxWordSize
      maxWordSize = length
      Redim wordlists.wordList(maxWordSize)
    EndIf
    AddElement(wordlists(length)\words())
    wordlists(length)\words()\word = word
    wordlists(length)\words()\letters = sortLetters(@word, length)
  Wend
  CloseFile(0)

  Define offset = OffsetOf(anagram\letters), option = #PB_Sort_Ascending
  Define sortType = #PB_Sort_String
  Define letters.s, foundDeranged
  NewList anagram.s()
  ;start search from largest to smallest
  For length = maxWordSize To 2 Step -1

    If FirstElement(wordlists(length)\words()) ;only examine lists with words
      ;sort words to place anagrams next to each other
      SortStructuredList(wordlists(length)\words(), option, offset, sortType)

      With wordlists(length)\words()
        letters = \letters
        AddElement(anagram()): anagram() = \word

        ;Compose sets of anagrams and check for derangement with remaining
        ;words in current list.
        While NextElement(wordlists(length)\words())
          ;Check for end of a set of anagrams?
          If letters <> \letters

            ;if more than one word in a set of anagrams check for derangement
            If ListSize(anagram()) > 1
              If isDeranged(anagram())
                foundDeranged = #True ;found deranged anagrams, stop processing
                Break 2 ;exit while/wend and for/next
              EndIf
            EndIf

            letters = \letters ;setup for next set of anagrams
            ClearList(anagram())
          EndIf

          AddElement(anagram()): anagram() = \word
        Wend
      EndWith

    EndIf

    ClearList(anagram())
  Next

  ;report results
  If foundDeranged
    Print("Largest 'Deranged' anagrams found are of length ")
    PrintN(Str(length) + ":" + #CRLF$)
    ForEach anagram()
      PrintN("  " + anagram())
    Next
  Else
    PrintN("No 'Deranged' anagrams were found." + #CRLF$)
  EndIf

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

{{out}}

```txt
Largest 'Deranged' anagrams found are of length 10:

  intoxicate
  excitation
```



## Python


```python
import urllib.request
from collections import defaultdict
from itertools import combinations

def getwords(url='http://www.puzzlers.org/pub/wordlists/unixdict.txt'):
    return list(set(urllib.request.urlopen(url).read().decode().split()))

def find_anagrams(words):
    anagram = defaultdict(list) # map sorted chars to anagrams
    for word in words:
        anagram[tuple(sorted(word))].append( word )
    return dict((key, words) for key, words in anagram.items()
                if len(words) > 1)

def is_deranged(words):
    'returns pairs of words that have no character in the same position'
    return [ (word1, word2)
             for word1,word2 in combinations(words, 2)
             if all(ch1 != ch2 for ch1, ch2 in zip(word1, word2)) ]

def largest_deranged_ana(anagrams):
    ordered_anagrams = sorted(anagrams.items(),
                              key=lambda x:(-len(x[0]), x[0]))
    for _, words in ordered_anagrams:
        deranged_pairs = is_deranged(words)
        if deranged_pairs:
            return deranged_pairs
    return []

if __name__ == '__main__':
    words = getwords('http://www.puzzlers.org/pub/wordlists/unixdict.txt')
    print("Word count:", len(words))

    anagrams = find_anagrams(words)
    print("Anagram count:", len(anagrams),"\n")

    print("Longest anagrams with no characters in the same position:")
    print('  ' + '\n  '.join(', '.join(pairs)
                             for pairs in largest_deranged_ana(anagrams)))
```

{{out}}

```txt
Word count: 25104
Anagram count: 1303

Longest anagrams with no characters in the same position:
  excitation, intoxicate
```


### Faster Version


```python
from collections import defaultdict
from itertools import combinations
from pathlib import Path
from typing import (Callable,
                    Dict,
                    Iterable,
                    Iterator,
                    List,
                    Optional,
                    Tuple,
                    TypeVar)

WORDS_FILE = 'unixdict.txt'

T1 = TypeVar('T1')
T2 = TypeVar('T2')


def main():
    words = read_words(Path(WORDS_FILE))
    anagram = longest_deranged_anagram(words)
    if anagram:
        print('The longest deranged anagram is: {}, {}'.format(*anagram))
    else:
        print('No deranged anagrams were found')


def read_words(path: Path) -> Iterator[str]:
    """Yields words from file at specified path"""
    with path.open() as file:
        for word in file:
            yield word.strip()


def longest_deranged_anagram(words: Iterable[str]
                             ) -> Optional[Tuple[str, str]]:
    """
    Returns the longest pair of words
    that have no character in the same position
    """
    words_by_lengths = mapping_by_function(len, words)
    decreasing_lengths = sorted(words_by_lengths, reverse=True)
    for length in decreasing_lengths:
        words = words_by_lengths[length]
        anagrams_by_letters = mapping_by_function(sort_str, words)
        for anagrams in anagrams_by_letters.values():
            deranged_pair = next(deranged_word_pairs(anagrams), None)
            if deranged_pair is not None:
                return deranged_pair
    return None


def mapping_by_function(function: Callable[..., T2],
                        iterable: Iterable[T1]) -> Dict[T2, List[T1]]:
    """
    Constructs a dictionary with keys
    obtained from applying an input function
    to items of an iterable,
    and the values filled from the same iterable
    """
    mapping = defaultdict(list)
    for item in iterable:
        mapping[function(item)].append(item)
    return mapping


def sort_str(string: str) -> str:
    """Sorts input string alphabetically"""
    return ''.join(sorted(string))


def deranged_word_pairs(words: Iterable[str]) -> Iterator[Tuple[str, str]]:
    """Yields deranged words from an input list of words"""
    pairs = combinations(words, 2)  # type: Iterator[Tuple[str, str]]
    yield from filter(is_deranged, pairs)


def is_deranged(word_pair: Tuple[str, str]) -> bool:
    """
    Checks if all corresponding letters are different,
    assuming that words have the same length
    """
    return all(a != b for a, b in zip(*word_pair))


if __name__ == '__main__':
    main()

```

{{out}}

```txt
The longest anagram is: excitation, intoxicate
```



## R


```R
puzzlers.dict <- readLines("http://www.puzzlers.org/pub/wordlists/unixdict.txt")

longest.deranged.anagram <- function(dict=puzzlers.dict) {
  anagram.groups <- function(word.group) {
    sorted <- sapply(lapply(strsplit(word.group,""),sort),paste, collapse="")
    grouped <- tapply(word.group, sorted, force, simplify=FALSE)
    grouped <- grouped[sapply(grouped, length) > 1]
    grouped[order(-nchar(names(grouped)))]
  }

  derangements <- function(anagram.group) {
    pairs <- expand.grid(a = anagram.group, b = anagram.group,
                         stringsAsFactors=FALSE)
    pairs <- subset(pairs, a < b)
    deranged <- with(pairs, mapply(function(a,b) all(a!=b),
                                   strsplit(a,""), strsplit(b,"")))
    pairs[which(deranged),]
  }

  for (anagram.group in anagram.groups(dict)) {
    if (nrow(d <- derangements(anagram.group)) > 0) {
      return(d[1,])
    }
  }
}
```


{{out}}


```R>
 longest.deranged.anagram()
           a          b
3 excitation intoxicate
```



## Racket


```racket
#lang racket
(define word-list-file "data/unixdict.txt")

(define (read-words-into-anagram-keyed-hash)
  (define (anagram-key word) (sort (string->list word) char<?))
  (for/fold ((hsh (hash)))
    ((word (in-lines)))
    (hash-update hsh (anagram-key word) (curry cons word) null)))

(define anagrams-list
  (sort
   (for/list
       ((v (in-hash-values
            (with-input-from-file
                word-list-file
              read-words-into-anagram-keyed-hash)))
        #:when (> (length v) 1)) v)
   > #:key (compose string-length first)))


(define (deranged-anagram-pairs l (acc null))
  (define (deranged-anagram-pair? hd tl)
    (define (first-underanged-char? hd tl)
      (for/first
          (((c h) (in-parallel hd tl))
           #:when (char=? c h)) c))
    (not (first-underanged-char? hd tl)))

  (if (null? l) acc
      (let ((hd (car l)) (tl (cdr l)))
        (deranged-anagram-pairs
         tl
         (append acc (map (lambda (x) (list hd x))
                          (filter (curry deranged-anagram-pair? hd) tl)))))))

;; for*/first give the first set of deranged anagrams (as per the RC problem)
;; for*/list gives a full list of the sets of deranged anagrams (which might be interesting)
(for*/first
    ((anagrams (in-list anagrams-list))
     (daps (in-value (deranged-anagram-pairs anagrams)))
     #:unless (null? daps))
  daps)
```

{{out}}

```txt
'(("intoxicate" "excitation"))
```



## REXX


```rexx
/*REXX program finds the  largest  deranged word  (within an identified dictionary).    */
iFID= 'unixdict.txt';     words=0                /*input file ID; number of words so far*/
wL.=0                                            /*number of words of length  L.  so far*/
      do while lines(iFID)\==0                   /*read each word in the file  (word=X).*/
      x= strip( linein( iFID) )                  /*pick off a word from the input line. */
      L= length(x);       if L<3 then iterate    /*onesies & twosies can't possible win.*/
      words= words + 1                           /*bump the count of  (usable)  words.  */
      #.words= L                                 /*the length of the word found.        */
      @.words= x                                 /*save the word in an array.           */
      wL.L= wL.L+1;       _= wL.L                /*bump counter of words of length  L.  */
      @@.L._= x                                  /*array   of words of length  L.       */
                                  do i=1  while x\=='';  parse var x !.i +1 x;  end  /*i*/
      call eSort L;       z=;     do j=1  for L;         z= z || !.j;           end  /*j*/
      @@s.L._= z                                 /*store the sorted word (letters).     */
      @s.words= @@s.L._                          /*store the sorted length  L  version. */
      end   /*while*/
a.=                                              /*all the  anagrams  for word  X.      */
say copies('─', 30)   words   'usable words in the dictionary file: '     iFID
m= 0;              n.= 0                         /*# anagrams for word  X;   m=max L.   */
       do j=1  for words                         /*process usable words that were found.*/
       Lx= #.j;   if Lx<m  then iterate          /*get length of word; skip if too short*/
       x= @.j;    xs= @s.j                       /*get some vital statistics for  X     */
           do k=1  for wL.Lx                     /*process all the words of length  L.  */
           if xs\== @@s.Lx.k  then iterate       /*is this not a true anagram of  X ?   */
           if x  ==  @@.Lx.k  then iterate       /*skip of processing anagram on itself.*/
                do c=1  for Lx                   /*ensure no character position shared. */
                if substr(@.j, c, 1) == substr(@@.Lx.k, c, 1)  then iterate k
                end   /*c*/                      /* [+]  now compare the rest of chars. */
           n.j= n.j + 1;     a.j= a.j   @@.Lx.k  /*bump counter;  then add ──► anagrams.*/
           m= max(m, Lx)                         /*M  is the maximum length of a word.  */
           end        /*k*/
       end            /*j*/

   do k=1  for words                             /*now, search all words for the maximum*/
   if #.k==m   then if n.k\==0   then if word(a.k, 1) > @.k  then say  @.k  a.k
   end   /*k*/                                   /* [↑]  REXX has no short-circuits.    */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
eSort: procedure expose !.;  parse arg ho 1 h    /*obtain number of elements; also copy.*/
         do while h>1;      h=h % 2;                    do i=1  for ho-h;   j= i;   k= h+i
         do while !.k<!.j;  t=!.j;  !.j=!.k;  !.k=t;   if h>=j  then leave;  j=j-h;  k=k-h
         end   /*while !.k···*/;         end  /*i*/;         end  /*while h>1*/;    return
```

{{out|output|text=   when using the default dictionary:}}

```txt

────────────────────────────── 24945 usable words in the dictionary file:  unixdict.txt
excitation  intoxicate

```



## Ring


```ring

# Project : Anagrams/Deranged anagrams

load "stdlib.ring"
fn1 = "unixdict.txt"

fp = fopen(fn1,"r")
str = fread(fp, getFileSize(fp))
fclose(fp)
strlist = str2list(str)
anagram = newlist(len(strlist), 5)
anag = list(len(strlist))
result = list(len(strlist))
for x = 1 to len(result)
     result[x] = 0
next
for x = 1 to len(anag)
     anag[x] = 0
next
for x = 1 to len(anagram)
      for y = 1 to 5
           anagram[x][y] = 0
      next
next

strbig = 1
for n = 1 to len(strlist)
     for m = 1 to len(strlist)
           sum = 0
           if len(strlist[n]) = len(strlist[m]) and n != m
              for p = 1 to len(strlist[m])
                    temp1 = count(strlist[n], strlist[m][p])
                    temp2 = count(strlist[m], strlist[m][p])
                    if temp1 = temp2
                       sum = sum + 1
                    ok
              next
              if sum = len(strlist[n])
                 anag[n] = anag[n] + 1
                 if anag[n] < 6 and result[n] = 0 and result[m] = 0
                    anagram[n][anag[n]] = strlist[m]
                    if len(strlist[m]) > len(strlist[strbig])
                       strbig = n
                    ok
                      result[m] = 1
                  ok
               ok
            ok
      next
      if anag[n] > 0
         result[n] = 1
      ok
next

flag = 0
for m = 1 to 5
     if anagram[strbig][m] != 0
        if m = 1
           see strlist[strbig] +  " "
           flag = 1
        ok
        see anagram[strbig][m] + " "
     ok
next

func getFileSize fp
       c_filestart = 0
       c_fileend = 2
       fseek(fp,0,c_fileend)
       nfilesize = ftell(fp)
       fseek(fp,0,c_filestart)
       return nfilesize

func count(astring,bstring)
       cnt = 0
       while substr(astring,bstring) > 0
                cnt = cnt + 1
                astring = substr(astring,substr(astring,bstring)+len(string(sum)))
       end
       return cnt

```

Output:

```txt

excitation intoxicate

```



## Ruby


```ruby
def deranged?(a, b)
  a.chars.zip(b.chars).all? {|char_a, char_b| char_a != char_b}
end

def find_derangements(list)
  list.combination(2) {|a,b| return a,b  if deranged?(a,b)}
  nil
end

require 'open-uri'
anagram = open('http://www.puzzlers.org/pub/wordlists/unixdict.txt') do |f|
  f.read.split.group_by {|s| s.each_char.sort}
end

anagram = anagram.select{|k,list| list.size>1}.sort_by{|k,list| -k.size}

anagram.each do |k,list|
  if derangements = find_derangements(list)
    puts "Longest derangement anagram: #{derangements}"
    break
  end
end
```

{{out}}

```txt

Longest derangement anagram: ["excitation", "intoxicate"]

```



## Run BASIC


```runbasic
a$ = httpGet$("http://www.puzzlers.org/pub/wordlists/unixdict.txt")
dim theWord$(30000)
dim ssWord$(30000)

c10$ = chr$(10)
i = 1
while instr(a$,c10$,i) <> 0
  j     = instr(a$,c10$,i)
  ln    = j - i
  again = 1
  sWord$ = mid$(a$,i,j-i)
  n = n + 1
 theWord$(n) = sWord$

 while again = 1
   again  = 0
   for kk = 1 to len(sWord$) - 1
   if mid$(sWord$,kk,1) > mid$(sWord$,kk +1,1) then
     sWord$ = left$(sWord$,kk-1);mid$(sWord$,kk+1,1);mid$(sWord$,kk,1);mid$(sWord$,kk+2)
     again  = 1
   end if
   next kk
 wend
 ssWord$(n) = sWord$
 i = j + 1
wend

for i = 1 to n
  if len(theWord$(i)) > maxLen then
    for j = 1 to n
      if ssWord$(i) = ssWord$(j) and i <> j then
        cnt = 0
	for k = 1 to len(theWord$(i))
	  if mid$(theWord$(i),k,1) = mid$(theWord$(j),k,1) then cnt = cnt + 1
	next k
	if cnt = 0 then
	  maxLen = len(theWord$(i))
	  maxPtrI = i
	  maxPtrJ = j
	end if
      end if
    next j
  end if
next i

print maxLen;" ";theWord$(maxPtrI);" => ";theWord$(maxPtrJ)
end
```

{{out}}

```txt
10 excitation => intoxicate
```



## Rust


```rust
//! Deranged anagrams
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fs::File;
use std::io;
use std::io::BufReader;
use std::io::BufRead;
use std::usize::MAX;

/// Get words from unix dictionary file
pub fn get_words() -> Result<Vec<String>, io::Error> {
    let mut words = vec!();
    // open file
    let f = File::open("data/unixdict.txt")?;
    // read line by line
    let reader = BufReader::new(&f);
    for line in reader.lines() {
        words.push(line?)
    }
    Ok(words)
}

/// Get the longest deranged anagram in the given list of word if any
pub fn longest_deranged(v: &mut Vec<String>) -> Option<(String,String)>{
    // sort by length descending then by alphabetical order
    v.sort_by(|s1, s2| {
        let mut c = s2.len().cmp(&s1.len());
        if c == Ordering::Equal {
            c = s1.cmp(s2);
        }
        c
    });
    // keep all strings keyed by sorted characters (since anagrams have the same list of sorted characters)
    let mut signatures : HashMap<Vec<char>, Vec<&String>> = HashMap::new();
    // save on memory by only keeping in the map strings of the current processed length
    let mut previous_length = MAX;
    for s in v {
        // length change, clear the map
        if s.len()<previous_length {
            signatures.clear();
            previous_length = s.len();
        }
        // generate key as sorted characters
        let mut sorted_chars = s.chars().collect::<Vec<char>>();
        sorted_chars.sort();
        let anagrams = signatures.entry(sorted_chars).or_insert(vec!());
        // find if any anagram (string with the same sorted character vector) is deranged
        if let Some(a) = anagrams.iter().filter(|anagram| is_deranged(anagram, s)).next(){
            return Some(((*a).clone(), s.clone()));
        }
        anagrams.push(s);
    }
    None
}

/// check if two strings do NOT have the same character in the same position
pub fn is_deranged(s1: &String, s2: &String) -> bool {
    // we zip the character iterators and check we find no position with the same two characters
    s1.chars().zip(s2.chars()).filter(|(a,b)| a == b).next().is_none()
}

/// an example main method printing the results
fn main() {
    let r = get_words();
    match r {
        Ok(mut v) => {
            let od = longest_deranged(&mut v);
            match od {
                None => println!("No deranged anagrams found!"),
                Some((s1,s2)) => println!("{} {}",s1,s2),
            }
        },
        Err(e) => panic!("Could not read words: {}",e)
    }
}



```

{{out}}

```txt
excitation intoxicate
```



## Scala


```scala
object DerangedAnagrams {

  /** Returns a map of anagrams keyed by the sorted characters */
  def groupAnagrams(words: Iterable[String]): Map[String, Set[String]] =
    words.foldLeft (Map[String, Set[String]]()) { (map, word) =>
      val sorted = word.sorted
      val entry = map.getOrElse(sorted, Set.empty)
      map + (sorted -> (entry + word))
    }

  /* Returns true if the pair of strings has no positions with the same
   * characters */
  def isDeranged(ss: (String, String)): Boolean =
    ss._1 zip ss._2 forall { case (c1, c2) => c1 != c2 }

  /* Returns pairwise combination of all Strings in the argument Iterable */
  def pairWords(as: Iterable[String]): Iterable[(String, String)] =
    if (as.size < 2) Seq() else (as.tail map (as.head -> _)) ++ pairWords(as.tail)

  /* Returns the contents of the argument URL as an Iterable[String], each
   * String is one line in the file */
  def readLines(url: String): Iterable[String] =
    io.Source.fromURL(url).getLines().toIterable

  val wordsURL = "http://www.puzzlers.org/pub/wordlists/unixdict.txt"

  def main(args: Array[String]): Unit = {
    val anagramMap = groupAnagrams(readLines(wordsURL))
    val derangedPairs = anagramMap.values flatMap (pairWords) filter (isDeranged)
    val (w1, w2) = derangedPairs maxBy (pair => pair._1.length)
    println("Longest deranged pair: "+w1+" and "+w2)
  }

}
```

{{out}}

```txt
Longest deranged pair: excitation and intoxicate
```



## Scheme



```scheme

(import (scheme base)
        (scheme char)
        (scheme cxr)
        (scheme file)
        (scheme write)
        (srfi 1)    ; lists
        (srfi 132)) ; sorting library

;; read in word list, and sort into decreasing length
(define (read-ordered-words)
  (with-input-from-file
    "unixdict.txt"
    (lambda ()
      (do ((line (read-line) (read-line))
           (words '() (cons line words)))
        ((eof-object? line)
         (list-sort (lambda (a b) (> (string-length a) (string-length b)))
                    words))))))

(define (find-deranged-words word-list)
  (define (search words)
    (let loop ((word-chars (let ((chars (map string->list words)))
                             (zip chars
                                  (map (lambda (word) (list-sort char<? word))
                                       chars)))))
      (if (< (length word-chars) 2)
        #f ; failed to find any
        (let ((deranged-word ; seek a deranged version of the first word in word-chars
                (find (lambda (chars)
                        (and (equal? (cadar word-chars) (cadr chars)) ; check it's an anagram?
                             (not (any char=? (caar word-chars) (car chars))))) ; and deranged?
                      word-chars)))
          (if deranged-word ; if we got one, return it with the first word
            (map list->string (list (caar word-chars) (car deranged-word)))
            (loop (cdr word-chars)))))))
  ;
  (let loop ((rem word-list))
    (if (null? rem)
      '()
      (let* ((len (string-length (car rem)))
             (deranged-words (search ; look through group of equal sized words
                               (take-while (lambda (word) (= len (string-length word)))
                                           (cdr rem)))))
        (if deranged-words
          deranged-words
          (loop (drop-while (lambda (word) (= len (string-length word)))
                            (cdr rem))))))))

(display (find-deranged-words (read-ordered-words))) (newline)

```


{{out}}

```txt

(excitation intoxicate)

```



## Sidef


```ruby
func find_deranged(Array a) {
    for i in (^a) {
        for j in (i+1 .. a.end) {
            overlaps(a[i], a[j]) || (
                printf("length %d: %s => %s\n", a[i].len, a[i], a[j])
                return true
            )
        }
    }
    return false
}

func main(File file) {

    file.open_r(\var fh, \var err) ->
        || die "Can't open file `#{file}' for reading: #{err}\n"

    var letter_list = Hash()

    # Store anagrams in hash table by letters they contain
    fh.words.each { |word|
        letter_list{word.sort} := [] << word
    }

    letter_list.keys                        \
         .grep {|k| letter_list{k}.len > 1} \     # take only ones with anagrams
         .sort {|a,b| b.len <=> a.len}      \     # sort by length, descending
         .each {|key|

        # If we find a pair, they are the longested due to the sort before
        find_deranged(letter_list{key}) && break
    }
}

main(%f'/tmp/unixdict.txt')
```

{{out}}

```txt
length 10: excitation => intoxicate
```


## Simula


```simula
! cim --memory-pool-size=512 deranged-anagrams.sim;
BEGIN

   CLASS TEXTVECTOR;
   BEGIN

      CLASS TEXTARRAY(N); INTEGER N;
      BEGIN TEXT ARRAY DATA(1:N);
      END TEXTARRAY;

      PROCEDURE EXPAND(N); INTEGER N;
      BEGIN
         INTEGER I;
         REF(TEXTARRAY) TEMP;
         TEMP :- NEW TEXTARRAY(N);
         FOR I := 1 STEP 1 UNTIL SIZE DO
            TEMP.DATA(I) :- ITEMS.DATA(I);
         ITEMS :- TEMP;
      END EXPAND;

      PROCEDURE APPEND(T); TEXT T;
      BEGIN
         IF SIZE + 1 > CAPACITY THEN
         BEGIN
            CAPACITY := 2 * CAPACITY;
            EXPAND(CAPACITY);
         END;
         SIZE := SIZE + 1;
         ITEMS.DATA(SIZE) :- T;
      END APPEND;

      TEXT PROCEDURE ELEMENT(I); INTEGER I;
      BEGIN
         IF I < 1 OR I > SIZE THEN ERROR("ELEMENT: INDEX OUT OF BOUNDS");
         ELEMENT :- ITEMS.DATA(I);
      END ELEMENT;

      INTEGER PROCEDURE FIND_INDEX(STR,INDEX); TEXT STR; INTEGER INDEX;
      BEGIN
         INTEGER I, FOUND;
         FOUND := -1;
         FOR I := INDEX STEP 1 UNTIL SIZE DO
            IF STR = ELEMENT(I) THEN
            BEGIN
               FOUND := I;
               GOTO L;
            END;
      L: FIND_INDEX := FOUND;
      END FIND_INDEX;

      INTEGER CAPACITY;
      INTEGER SIZE;
      REF(TEXTARRAY) ITEMS;

      CAPACITY := 20;
      SIZE := 0;
      EXPAND(CAPACITY);
   END TEXTVECTOR;

   BOOLEAN PROCEDURE DERANGE(S1,S2); TEXT S1,S2;
   BEGIN
      INTEGER I;
      BOOLEAN RESULT;
      RESULT := TRUE;
      I := 1;
      WHILE RESULT AND I <= S1.LENGTH DO
      BEGIN
         CHARACTER C1, C2;
         S1.SETPOS(I); C1 := S1.GETCHAR;
         S2.SETPOS(I); C2 := S2.GETCHAR;
         IF C1 = C2 THEN
            RESULT := FALSE
         ELSE
            I := I+1;
      END;
      DERANGE := RESULT;
   END DERANGE;

   PROCEDURE STRSORT(STR); NAME STR; TEXT STR;
   BEGIN
      INTEGER N, I;
      FOR N := STR.LENGTH STEP -1 UNTIL 2 DO
         FOR I := 1 STEP 1 UNTIL N-1 DO
         BEGIN
            CHARACTER CI1,CI2;
            STR.SETPOS(I); CI1 := STR.GETCHAR; CI2 := STR.GETCHAR;
            IF CI1 > CI2 THEN
            BEGIN
               STR.SETPOS(I); STR.PUTCHAR(CI2); STR.PUTCHAR(CI1);
            END;
         END;
   END STRSORT;

   REF(INFILE) FILE;
   INTEGER LEN, FOUNDLEN;
   REF(TEXTVECTOR) VECT, SVECT;
   INTEGER INDEX, P1, P2;
   TEXT STR;

   VECT :- NEW TEXTVECTOR;
   SVECT :- NEW TEXTVECTOR;
   FOUNDLEN := 1;
   FILE :- NEW INFILE("unixdict.txt");
   FILE.OPEN(BLANKS(132));
   WHILE NOT FILE.LASTITEM DO
   BEGIN
      STR :- FILE.INTEXT(132).STRIP;
      LEN := STR.LENGTH;
      IF LEN > FOUNDLEN THEN
      BEGIN
         VECT.APPEND(COPY(STR));
         STRSORT(STR);
         INDEX := 0;
         COMMENT Loop through anagrams by index in vector of sorted strings;
         INDEX := SVECT.FIND_INDEX(STR, INDEX + 1);
         WHILE INDEX > 0 DO
         BEGIN
            IF DERANGE(VECT.ELEMENT(VECT.SIZE), VECT.ELEMENT(INDEX)) THEN
            BEGIN
               P1 := VECT.SIZE;
               P2 := INDEX;
               FOUNDLEN := LEN;
            END IF;
            INDEX := SVECT.FIND_INDEX(STR, INDEX + 1);
         END WHILE;
         SVECT.APPEND(STR);
      END IF;
   END WHILE;
   FILE.CLOSE;
   OUTTEXT(VECT.ELEMENT(P1) & " " & VECT.ELEMENT(P2));
   OUTIMAGE;
END

```

{{out}}

```txt
intoxicate excitation

3 garbage collection(s) in 2.9 seconds.

```



## Tcl


```tcl
package require Tcl 8.5
package require http

# Fetch the words
set t [http::geturl "http://www.puzzlers.org/pub/wordlists/unixdict.txt"]
set wordlist [split [http::data $t] \n]
http::cleanup $t

# Group by characters in word
foreach word $wordlist {
    dict lappend w [lsort [split $word ""]] [split $word ""]
}

# Deranged test
proc deranged? {l1 l2} {
    foreach c1 $l1 c2 $l2 {
	if {$c1 eq $c2} {return 0}
    }
    return 1
}

# Get a deranged pair from an anagram set, if one exists
proc getDeranged {words} {
    foreach l1 [lrange $words 0 end-1] {
	foreach l2 [lrange $words 1 end] {
	    if {[deranged? $l1 $l2]} {
		return [list $l1 $l2 1]
	    }
	}
    }
    return {{} {} 0}
}

# Find the max-length deranged anagram
set count 0
set candidates {}
set max 0
dict for {k words} $w {
    incr count [expr {[llength $words] > 1}]
    if {[llength $k] > $max && [lassign [getDeranged $words] l1 l2]} {
	set max [llength $l1]
	lappend candidates [join $l1 ""],[join $l2 ""]
    }
}

# Print out what we found
puts "[llength $wordlist] words"
puts "[dict size $w] potential anagram-groups"
puts "$count real anagram-groups"
foreach pair $candidates {
    puts "considered candidate pairing: $pair"
}
puts "MAXIMAL DERANGED ANAGRAM: LENGTH $max\n\t[lindex $candidates end]"
```

{{out}}

```txt

25105 words
23567 potential anagram-groups
1303 real anagram-groups
considered candidate pairing: abc,cab
considered candidate pairing: abed,bade
considered candidate pairing: abort,bator
considered candidate pairing: afresh,shafer
considered candidate pairing: alberto,latrobe
considered candidate pairing: american,cinerama
considered candidate pairing: ancestral,lancaster
considered candidate pairing: excitation,intoxicate
MAXIMAL DERANGED ANAGRAM: LENGTH 10
	excitation,intoxicate

```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT,{}
requestdata = REQUEST ("http://www.puzzlers.org/pub/wordlists/unixdict.txt")

DICT anagramm CREATE 99999

COMPILE
 LOOP word=requestdata
  -> ? : any character
  charsInWord=STRINGS (word," ? ")
  charString =ALPHA_SORT (charsInWord)

  DICT anagramm LOOKUP charString,num,freq,wordalt,wlalt
  IF (num==0) THEN
   WL=SIZE (charString)
   DICT anagramm APPEND/QUIET/COUNT charString,num,freq,word,wl;" "
  ELSE
   DICT anagramm APPEND/QUIET/COUNT charString,num,freq,word,"";" "
  ENDIF
 ENDLOOP

DICT anagramm UNLOAD charString,all,freq,anagrams,wl

index        =DIGIT_INDEX (wl)
reverseIndex =REVERSE (index)
wl           =INDEX_SORT (wl,reverseIndex)
freq         =INDEX_SORT (freq,reverseIndex)
anagrams     =INDEX_SORT (anagrams,reverseIndex)
charString   =INDEX_SORT (charString,reverseIndex)

LOOP fr=freq,a=anagrams,w=wl
 IF (fr==1) cycle
 asplit=SPLIT (a,": :")
 a1=SELECT (asplit,1,arest)
 a1split=STRINGS (a1," ? ")
 LOOP r=arest
  rsplit=STRINGS (r," ? ")
   LOOP v1=a1split,v2=rsplit
    IF (v1==v2) EXIT,EXIT
   ENDLOOP
    PRINT "Largest deranged anagram (length: ",w,"):"
    PRINT a
   STOP
 ENDLOOP
ENDLOOP
ENDCOMPILE
```

{{out}}

```txt

Largest deranged anagram (length: 10):
excitation intoxicate

```



## UNIX Shell

{{works with|ksh93}}

```bash
function get_words {
    typeset host=www.puzzlers.org
    typeset page=/pub/wordlists/unixdict.txt
    exec 7<>/dev/tcp/$host/80
    print -e -u7 "GET $page HTTP/1.1\r\nhost: $host\r\nConnection: close\r\n\r\n"
    # remove the http header and save the word list
    sed 's/\r$//; 1,/^$/d' <&7 >"$1"
    exec 7<&-
}

function is_deranged {
    typeset -i i
    for ((i=0; i<${#1}; i++)); do
        [[ ${1:i:1} == ${2:i:1} ]] && return 1
    done
    return 0
}

function word2key {
    typeset -a chars=( $(
        for ((i=0; i<${#word}; i++)); do
            echo "${word:i:1}"
        done | sort
    ) )
    typeset IFS=""
    echo "${chars[*]}"
}

[[ -f word.list ]] || get_words word.list

typeset -A words
typeset -i max=0

while IFS= read -r word; do
    key=$(word2key $word)
    if [[ -z "${words["$key"]}" ]]; then
        words["$key"]=$word
    else
        if (( ${#word} > max )); then
            if is_deranged "${words["$key"]}" "$word"; then
                max_deranged=("${words["$key"]}" "$word")
                max=${#word}
            fi
        fi
    fi
done <word.list
echo $max - ${max_deranged[@]}
```

{{out}}

```txt
10 - excitation intoxicate
```



## Ursala

This solution assumes the file <code>unixdict.txt</code> is passed to the compiler as a command line parameter.

```Ursala
#import std

anagrams = |=tK33lrDSL2SL ~=&& ==+ ~~ -<&

deranged = filter not zip; any ==

#cast %sW

main = leql$^&l deranged anagrams unixdict_dot_txt
```

The <code>anagrams</code> function is a little slow as defined above, but can be sped up by at least two orders of magnitude by grouping the words into classes of equal length, and sorting each word once in advance instead of each time a comparison is made as shown below.

```Ursala
anagrams = @NSiXSlK2rSS *= ^(-<&,~&)*; |=rSStFtK33lrDSL2SL ~=@br&& ==@bl
```

We can speed it up by about another factor of 5 by starting from the group of longest words and stopping as soon as a deranged anagram is found instead of generating all anagrams.

```Ursala
#import std

longest_deranged_anagram =

@NSiXSlK2rSS leql-<x&h; @NiX ~&lZrB->l ^\~&rt @rh -+
   ~&a^& ~&plrEkZ?ah/~&ah ~&fatPR,
   ^(-<&,~&)*; |=rSStFtK33lrDSL2SL ~=@br&& ==@bl+-

#cast %sW

main = longest_deranged_anagram unixdict_dot_txt
```

{{out}}

```txt

('excitation','intoxicate')

```



## zkl


```zkl
words:=Dictionary(25000);  //-->Dictionary(sorted word:all anagrams, ...)
File("unixdict.txt").read().pump(Void,'wrap(w){
   w=w.strip(); key:=w.sort(); words[key]=words.find(key,T).append(w);
});

nws:=words.values.pump(List,fcn(ws){ //-->( (len,words), ...)
   if(ws.len()>1){ // two or more anagrams
      r:=List(); n:=ws[0].len(); // length of these anagrams
      foreach idx,w in (ws.enumerate()){
	 foreach w2 in (ws[idx+1,*]){
	    if(Utils.zipWith('!=,w,w2).filter().len()==n)
	       r.write(T(w,w2));
	 }
      }
      if(r) return(r.insert(0,n));
   }
   Void.Skip
});

nws.filter(fcn(nws,max){ nws[0]==max },
   nws.reduce(fcn(p,nws){ p.max(nws[0]) },0) )
.println();
```

{{out}}

```txt

L(L(10,L("excitation","intoxicate")))

```

Replace the center section with the following for smaller code (3 lines shorter!) that is twice as slow:

```zkl
nws:=words.values.pump(List,fcn(ws){ //-->( (len,words), ...)
   if(ws.len()>1){ // two or more anagrams
      n:=ws[0].len(); // length of these anagrams
      r:=Utils.Helpers.permute(ws).filter('wrap(ws2){
            n == Utils.zipWith('!=,ws2.xplode()).filter().len();
	 });
      if(r) return(n,r[0]); // L(L("glove","vogel"))-->L(5,L("glove","vogel"))
   }
   Void.Skip
});
```

