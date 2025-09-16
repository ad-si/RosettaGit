+++
title = "Ordered words"
description = ""
date = 2019-08-31T05:57:34Z
aliases = []
[extra]
id = 8699
[taxonomies]
categories = ["task", "text processing"]
tags = []
languages = [
  "ada",
  "aime",
  "algol_68",
  "apl",
  "autohotkey",
  "awk",
  "bacon",
  "bbc_basic",
  "befunge",
  "bracmat",
  "burlesque",
  "c",
  "clojure",
  "coffeescript",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "e",
  "echolisp",
  "elixir",
  "erlang",
  "euphoria",
  "factor",
  "fantom",
  "fbsl",
  "forth",
  "fortran",
  "freebasic",
  "gambas",
  "go",
  "groovy",
  "haskell",
  "huginn",
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
  "lua",
  "maple",
  "mathematica",
  "netrexx",
  "nim",
  "ocaml",
  "oforth",
  "oorexx",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "racket",
  "red",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "simula",
  "smalltalk",
  "spl",
  "swift",
  "tcl",
  "tuscript",
  "ursala",
  "vba",
  "vbscript",
  "vedit_macro_language",
  "zkl",
]
+++

## Task

An   ''ordered word''   is a word in which the letters appear in alphabetic order.

Examples include   '''abbey'''   and   '''dirt'''.

Find ''and display'' all the ordered words in the dictionary   [https://web.archive.org/web/20180611003215/http://www.puzzlers.org/pub/wordlists/unixdict.txt unixdict.txt]   that have the longest word length.

(Examples that access the dictionary file locally assume that you have downloaded this file yourself.)

The display needs to be shown on this page.

<hr>


## Ada



```Ada

with Ada.Text_IO, Ada.Containers.Indefinite_Vectors;
use Ada.Text_IO;

procedure Ordered_Words is
   package Word_Vectors is new Ada.Containers.Indefinite_Vectors
      (Index_Type => Positive, Element_Type => String);
   use Word_Vectors;
   File : File_Type;
   Ordered_Words : Vector;
   Max_Length : Positive := 1;
begin
   Open (File, In_File, "unixdict.txt");
   while not End_Of_File (File) loop
      declare
         Word : String := Get_Line (File);
      begin
         if (for all i in Word'First..Word'Last-1 => Word (i) <= Word(i+1)) then
            if Word'Length > Max_Length then
               Max_Length := Word'Length;
               Ordered_Words.Clear;
               Ordered_Words.Append (Word);
            elsif Word'Length = Max_Length then
               Ordered_Words.Append (Word);
            end if;
         end if;
      end;
   end loop;
   for Word of Ordered_Words loop
     Put_Line (Word);
   end loop;
   Close (File);
end Ordered_Words;

```


Output:

```txt
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```



## Aime


```aime
integer
ordered(data s)
{
    integer a, c, p;

    a = 1;

    p = -1;
    for (, c in s) {
        if (c < p) {
            a = 0;
            break;
        } else {
            p = c;
        }
    }

    a;
}

integer
main(void)
{
    file f;
    text s;
    index x;

    f.affix("unixdict.txt");

    while (f.line(s) != -1) {
        if (ordered(s)) {
            x.v_list(~s).append(s);
        }
    }

    l_ucall(x.back, o_, 0, "\n");

    return 0;
}
```

```txt
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}}.
The Algol 68 G "read" PRAGMA is used to include the associative array code from [[Associative_array/Iteration]].


Note that as the associative array does not store the elements in sorted order, the output is not sorted.

```algol68
# find the longrst words in a list that have all letters in order    #
# use the associative array in the Associate array/iteration task    #
PR read "aArray.a68" PR

# returns the length of word                                         #
PROC length = ( STRING word )INT: 1 + ( UPB word - LWB word );

# returns text with the characters sorted into ascending order       #
PROC char sort = ( STRING text )STRING:
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
    END # char sort # ;

# read the list of words and store the ordered ones in an associative array #

IF  FILE input file;
    STRING file name = "unixdict.txt";
    open( input file, file name, stand in channel ) /= 0
THEN
    # failed to open the file #
    print( (  "Unable to open """ + file name + """", newline ) )
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
    # store the ordered words and find the longest #
    INT max length   := 0;
    REF AARRAY words := INIT LOC AARRAY;
    STRING word;
    WHILE NOT at eof
    DO
        STRING word;
        get( input file, ( word, newline ) );
        IF char sort( word ) = word
        THEN
            # have an ordered word #
            IF  INT word length := length( word );
                word length > max length
            THEN
                max length := word length
            FI;
            # store the word #
            words // word := ""
        FI
    OD;
    # close the file #
    close( input file );

    print( ( "Maximum length of ordered words: ", whole( max length, -4 ), newline ) );
    # show the ordered words with the maximum length #
    REF AAELEMENT e := FIRST words;
    WHILE e ISNT nil element DO
        IF  max length = length( key OF e )
        THEN
            print( ( key OF e, newline ) )
        FI;
        e := NEXT words
    OD
FI
```

```txt

Maximum length of ordered words:    6
accent
accept
access
abbott
accost
chilly
bellow
effort
billow
almost
choppy
choosy
biopsy
floppy
glossy
knotty
```


## APL

Works in NARS2000 APL; ("Objects" menu -> New, paste this in, save. Then run in the main session manager).

```APL
result←longest_ordered_words file_path

f←file_path ⎕NTIE 0            ⍝ open file
text←⎕NREAD f 'char8'          ⍝ read vector of 8bit chars
⎕NUNTIE f                      ⍝ close file

lines←text⊂⍨~text∊(⎕UCS 10 13) ⍝ split into lines (\r\n)

⍝ filter only words with ordered characters
ordered_words←lines/⍨{(⍳∘≢≡⍋)⍵}¨lines

⍝ find max of word lengths, filter only words with that length
result←ordered_words/⍨lengths=⍨⌈/lengths←≢¨ordered_words

```

The ordered character filter is a train which uses gradeup to say which order you would have to pick the characters, to put them in order. e.g. ⍋ 'zxy' is 2 3 1 because you'd have to pick the second character, then the third, then the first, to put them in order. If they are in order then the result is the integers 1 2 3 .. to the length of the word.

And it generates the integers up to the length of the word, e.g. 1 2 3. and compares if those two arrays are the same.

Then uses that information as a filter against the original word list.

The result is: lengths of each ordered word, which equal the max-reduce of the list of lengths (longest word), used as a filter against the ordered word list.
```txt

      longest_ordered_words 'd:\unixdict.txt'
abbott accent accept access accost almost bellow billow biopsy chilly choosy choppy effort floppy glossy knotty

```


## AutoHotkey

This script uses the ASCII value of each letter to determine its place in the alphabet. Given a dictionary not all in the same case,
StringLower could be used. This script assumes a locally downloaded copy of the dictionary, but UrlDownloadToFile could be used.
The purpose of the GUI is simply to display a field where the user can copy the list. MsgBox could be used, or FileAppend.

```AutoHotkey

MaxLen=0
Loop, Read, UnixDict.txt         ; Assigns A_LoopReadLine to each line of the file
{
    thisword := A_LoopReadLine   ; Just for readability
    blSort := isSorted(thisWord) ; reduce calls to IsSorted to improve performance
    ThisLen := StrLen(ThisWord)  ; reduce calls to StrLen to improve performance
    If (blSort = true and ThisLen = maxlen)
        list .= ", " . thisword
    Else If (blSort = true and ThisLen > maxlen)
    {
        list := thisword
        maxlen := ThisLen
    }
}

IsSorted(word){  ; This function uses the ASCII value of the letter to determine its place in the alphabet.
                           ;        Thankfully, the dictionary is in all lowercase
    lastchar=0
    Loop, parse, word
    {
        if ( Asc(A_LoopField) < lastchar )
            return false
        lastchar := Asc(A_loopField)
    }
    return true
}

GUI, Add, Edit, w300 ReadOnly, %list%
GUI, Show
return ; End Auto-Execute Section

GUIClose:
ExitApp

```

Output:

```txt
abbott, accent, accept, access, accost, almost, bellow, billow, biopsy, chilly, choosy, choppy, effort, floppy, glossy, knotty
```



## AWK


```awk
BEGIN {
	abc = "abcdefghijklmnopqrstuvwxyz"
}

{
	# Check if this line is an ordered word.
	ordered = 1  # true
	left = -1
	for (i = 1; i <= length($0); i++) {
		right = index(abc, substr($0, i, 1))
		if (right == 0 || left > right) {
			ordered = 0  # false
			break
		}
		left = right
	}

	if (ordered) {
		score = length($0)
		if (score > best["score"]) {
			# Reset the list of best ordered words.
			best["score"] = score
			best["count"] = 1
			best[1] = $0
		} else if (score == best["score"]) {
			# Add this word to the list.
			best[++best["count"]] = $0
		}
	}
}

END {
	# Print the list of best ordered words.
	for (i = 1; i <= best["count"]; i++)
		print best[i]
}
```


You must provide <tt>unixdict.txt</tt> as input.


```txt
$ awk -f ordered-words.awk unixdict.txt
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```



## BaCon


```freebasic
'Ordered words - improved version
OPTION COLLAPSE TRUE

list$ = LOAD$("unixdict.txt")

FOR word$ IN list$ STEP NL$

    term$ = EXTRACT$(SORT$(EXPLODE$(word$, 1)), " ")

    IF word$ = term$ THEN
        IF LEN(term$) > MaxLen THEN
            MaxLen = LEN(term$)
            result$ = word$
        ELIF LEN(term$) = MaxLen THEN
            result$ = APPEND$(result$, 0, word$, NL$)
        END IF
    END IF
NEXT

PRINT result$

```


```txt
prompt$ bacon ordered-words
Converting 'ordered-words.bac'... done, 29 lines were processed in 0.004 seconds.
Compiling 'ordered-words.bac'... cc  -c ordered-words.bac.c
cc -o ordered-words ordered-words.bac.o -lbacon -lm
Done, program 'ordered-words' ready.
prompt$ ./ordered-words
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```



## BBC BASIC

An optimisation is that the word isn't checked for being ordered unless it is at least as long as the current maximum.

```bbcbasic
      dict% = OPENIN("unixdict.txt")
      IF dict%=0 ERROR 100, "Failed to open dictionary file"

      max% = 2
      REPEAT
        A$ = GET$#dict%
        IF LENA$ >= max% THEN
          i% = 0
          REPEAT i% += 1
          UNTIL ASCMID$(A$,i%) > ASCMID$(A$,i%+1)
          IF i% = LENA$ THEN
            IF i% > max% max% = i% : list$ = ""
            list$ += A$ + CHR$13 + CHR$10
          ENDIF
        ENDIF
      UNTIL EOF#dict%
      CLOSE #dict%
      PRINT list$
      END
```

'''Output:'''

```txt

abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty

```



## Befunge

The word list is read from stdin, so ideally requires an interpreter that can accept input redirected from a file. It's still possible to enter the test data manually, though - you would just need to mark the end of the list with a blank line.

Memory usage isn't very efficient - the matching list is stored one word per line - so on a standard Befunge-93 interpreter there is an upper limit of 22 matches. This is not a problem for the unix dictionary, which only requires 16, but it's theoretically possible that other data sets could run out of space.


```befunge
00p30p>
_010p120p0>#v0~>>\$::48*\`\"~"`+!>>#v_$:#v_>30g:!#v_1-30p55+0>:30g3+g\1v
>0#v _$^#::\p04:<^+>#1^#\p01:p02*g02!`\g01:<@$ _ ,#!>#:<$<^<!:g03$<_^#!`\g00:+<
^<o>\30g2+p40g1+^0p00p03+1*g03!-g00 < < < < < <:>#$:#$00g#<\#<`#<!#<2#$0g#<*#<_
```


```txt
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```



## Bracmat


```bracmat
  ( orderedWords
  =   bow result longestLength word character
    .     0:?bow
        & :?result
        & 0:?longestLength
        & @( get$(!arg,STR)
           :   ?
               ( [!bow %?word \n [?bow ?
               & @( !word
                  : ( ? %@?character <%@!character ?
                    |   ?
                        (   [!longestLength
                          & !word !result:?result
                        |   [>!longestLength:[?longestLength
                          & !word:?result
                        |
                        )
                    )
                  )
               & ~`
               )
           )
      | !result
  )
& orderedWords$"unixdict.txt"
```


```txt
  knotty
  glossy
  floppy
  effort
  choppy
  choosy
  chilly
  biopsy
  billow
  bellow
  almost
  accost
  access
  accept
  accent
  abbott
```



## Burlesque


```burlesque
ln{so}f[^^{L[}>mL[bx(==)[+(L[)+]f[uN
```


```txt
$ ./burlesque --file ordered_words.brl < unixdict.txt
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```



## C


```cpp
#include <iostream>
#include <string.h>
#include <stdio.h>
#include <assert.h>


#define MAXLEN 100
typedef char TWord[MAXLEN];


typedef struct Node {
    TWord word;
    struct Node *next;
} Node;


int is_ordered_word(const TWord word) {
    assert(word != NULL);
    int i;

    for (i = 0; word[i] != '\0'; i++)
        if (word[i] > word[i + 1] && word[i + 1] != '\0')
            return 0;

    return 1;
}


Node* list_prepend(Node* words_list, const TWord new_word) {
    assert(new_word != NULL);
    Node *new_node = malloc(sizeof(Node));
    if (new_node == NULL)
        exit(EXIT_FAILURE);

    strcpy(new_node->word, new_word);
    new_node->next = words_list;
    return new_node;
}


Node* list_destroy(Node *words_list) {
    while (words_list != NULL) {
        Node *temp = words_list;
        words_list = words_list->next;
        free(temp);
    }

    return words_list;
}


void list_print(Node *words_list) {
    while (words_list != NULL) {
        printf("\n%s", words_list->word);
        words_list = words_list->next;
    }
}


int main() {
    FILE *fp = fopen("unixdict.txt", "r");
    if (fp == NULL)
        return EXIT_FAILURE;

    Node *words = NULL;
    TWord line;
    unsigned int max_len = 0;

    while (fscanf(fp, "%99s\n", line) != EOF) {
        if (strlen(line) > max_len && is_ordered_word(line)) {
            max_len = strlen(line);
            words = list_destroy(words);
            words = list_prepend(words, line);
        } else if (strlen(line) == max_len && is_ordered_word(line)) {
            words = list_prepend(words, line);
        }
    }

    fclose(fp);
    list_print(words);

    return EXIT_SUCCESS;
}
```

Output:

```txt
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```

Alternative version with dynamic array:

```cpp
#include <iostream>
#include <string.h>
#include <stdio.h>
#include <assert.h>


#define MAXLEN 100
typedef char TWord[MAXLEN];


typedef struct WordsArray {
    TWord *words;
    size_t len;
} WordsArray;


int is_ordered_word(const TWord word) {
    assert(word != NULL);
    int i;

    for (i = 0; word[i] != '\0'; i++)
        if (word[i] > word[i + 1] && word[i + 1] != '\0')
            return 0;

    return 1;
}


void array_append(WordsArray *words_array, const TWord new_word) {
    assert(words_array != NULL);
    assert(new_word != NULL);
    assert((words_array->len == 0) == (words_array->words == NULL));

    words_array->len++;
    words_array->words = realloc(words_array->words,
                                 words_array->len * sizeof(words_array->words[0]));
    if (words_array->words == NULL)
        exit(EXIT_FAILURE);
    strcpy(words_array->words[words_array->len-1], new_word);
}


void array_free(WordsArray *words_array) {
    assert(words_array != NULL);
    free(words_array->words);
    words_array->words = NULL;
    words_array->len = 0;
}


void list_print(WordsArray *words_array) {
    assert(words_array != NULL);
    size_t i;
    for (i = 0; i < words_array->len; i++)
        printf("\n%s", words_array->words[i]);
}


int main() {
    FILE *fp = fopen("unixdict.txt", "r");
    if (fp == NULL)
        return EXIT_FAILURE;

    WordsArray words;
    words.len = 0;
    words.words = NULL;

    TWord line;
    line[0] = '\0';
    unsigned int max_len = 0;

    while (fscanf(fp, "%99s\n", line) != EOF) { // 99 = MAXLEN - 1
        if (strlen(line) > max_len && is_ordered_word(line)) {
            max_len = strlen(line);
            array_free(&words);
            array_append(&words, line);
        } else if (strlen(line) == max_len && is_ordered_word(line)) {
            array_append(&words, line);
        }
    }

    fclose(fp);
    list_print(&words);
    array_free(&words);

    return EXIT_SUCCESS;
}
```


### Mmap

Shorter and potentially much faster version with <code>mmap (2)</code>.  No stinky <code>malloc</code> or <code>scanf</code> calls.

```c
#include <stdio.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <err.h>
#include <string.h>

int ordered(char *s, char **end)
{
	int r = 1;
	while (*++s != '\n' && *s != '\r' && *s != '\0')
		if (s[0] < s[-1]) r = 0;

	*end = s;
	return r;
}

int main()
{
	char *buf, *word, *end, *tail;
	struct stat st;
	int longest = 0, len, fd;

	if ((fd = open("unixdict.txt", O_RDONLY)) == -1) err(1, "read error");

	fstat(fd, &st);
	if (!(buf = mmap(0, st.st_size, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0)))
		err(1, "mmap");

	for (word = end = buf; end < buf + st.st_size; word = end) {
		while (*word == '\r' || *word == '\n') word++;
		if (!ordered(word, &end)) continue;
		if ((len = end - word + 1) < longest) continue;
		if (len > longest) {
			tail = buf;  /* longer words found; reset out buffer */
			longest = len;
		}
		/* use the same mmap'd region to store output.  because of MAP_PRIVATE,
		 * change will not go back to file.  mmap is copy on write, and we are using
		 * only the head space to store output, so kernel doesn't need to copy more
		 * than the words we saved--in this case, one page tops.
		 */
		memcpy(tail, word, len);
		tail += len;
		*tail = '\0';
	}
	printf(buf);

	munmap(buf, st.st_size);
	close(fd);
	return 0;
}
```



## C++



```cpp
#include <algorithm>
#include <fstream>
#include <iostream>
#include <iterator>
#include <string>
#include <vector>

bool ordered(const std::string &word)
{
    return std::is_sorted(word.begin(), word.end()); // C++11
}

int main()
{
    std::ifstream infile("unixdict.txt");
    if (!infile) {
        std::cerr << "Can't open word file\n";
        return -1;
    }

    std::vector<std::string> words;
    std::string word;
    int longest = 0;

    while (std::getline(infile, word)) {
        int length = word.length();
        if (length < longest) continue; // don't test short words

        if (ordered(word)) {
            if (longest < length) {
                longest = length; // set new minimum length
                words.clear(); // reset the container
            }
            words.push_back(word);
        }
    }
    std::copy(words.begin(), words.end(), std::ostream_iterator<std::string>(std::cout, "\n"));
}
```

Output:

```txt

abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty

```


## C#

```c#
using System;
using System.Linq;
using System.Net;

static class Program
{
    static void Main(string[] args)
    {
        WebClient client = new WebClient();
        string text = client.DownloadString("http://www.puzzlers.org/pub/wordlists/unixdict.txt");
        string[] words = text.Split(new char[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries);

        var query = from w in words
                    where IsOrderedWord(w)
                    group w by w.Length into ows
                    orderby ows.Key descending
                    select ows;

        Console.WriteLine(string.Join(", ", query.First().ToArray()));
    }

    private static bool IsOrderedWord(string w)
    {
        for (int i = 1; i < w.Length; i++)
            if (w[i] < w[i - 1])
                return false;

        return true;
    }
}
```


Output:

```txt
abbott, accent, accept, access, accost, almost, bellow, billow, biopsy, chilly, choosy, choppy, effort, floppy, glossy, knotty
```




## Clojure



```Clojure
(defn is-sorted? [coll]
  (not-any? pos? (map compare coll (next coll))))

(defn take-while-eqcount [coll]
  (let [n (count (first coll))]
    (take-while #(== n (count %)) coll)))

(with-open [rdr (clojure.java.io/reader "unixdict.txt")]
  (->> rdr
       line-seq
       (filter is-sorted?)
       (sort-by count >)
       take-while-eqcount
       (clojure.string/join ", ")
       println))
```


Output

```Clojure
abbott, accent, accept, access, accost, almost, bellow, billow, biopsy, chilly, choosy, choppy, effort, floppy, glossy, knotty
```



## CoffeeScript


```coffeescript

ordered_word = (word) ->
  for i in [0...word.length - 1]
    return false unless word[i] <= word[i+1]
  true

show_longest_ordered_words = (candidates, dict_file_name) ->
  words = ['']
  for word in candidates
    continue if word.length < words[0].length
    if ordered_word word
      words = [] if word.length > words[0].length
      words.push word
  return if words[0] == '' # we came up empty
  console.log "Longest Ordered Words (source=#{dict_file_name}):"
  for word in words
    console.log word

dict_file_name = 'unixdict.txt'
file_content = require('fs').readFileSync dict_file_name
dict_words = file_content.toString().split '\n'
show_longest_ordered_words dict_words, dict_file_name

```

output
<lang>
> coffee ordered_words.coffee
Longest Ordered Words (source=unixdict.txt):
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty

```



## Common Lisp


```lisp
(defun orderedp (word)
  (reduce (lambda (prev curr)
            (when (char> prev curr) (return-from orderedp nil))
            curr)
          word)
  t)

(defun longest-ordered-words (filename)
  (let ((result nil))
    (with-open-file (s filename)
      (loop
         with greatest-length = 0
         for word = (read-line s nil)
         until (null word)
         do (let ((length (length word)))
              (when (and (>= length greatest-length)
                         (orderedp word))
                (when (> length greatest-length)
                  (setf greatest-length length
                        result nil))
                (push word result)))))
    (nreverse result)))

CL-USER> (longest-ordered-words "unixdict.txt")
("abbott" "accent" "accept" "access" "accost" "almost" "bellow" "billow"
 "biopsy" "chilly" "choosy" "choppy" "effort" "floppy" "glossy" "knotty")

```



## D


### Simple Procedural Version


```d
void main() {
    import std.stdio, std.algorithm, std.range, std.string;

    string[] result;
    size_t maxLen;

    foreach (string word; "unixdict.txt".File.lines) {
        word = word.chomp;
        immutable len = word.walkLength;
        if (len < maxLen || !word.isSorted)
            continue;
        if (len > maxLen) {
            result = [word];
            maxLen = len;
        } else
            result ~= word;
    }

    writefln("%-(%s\n%)", result);
}
```

```txt
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```



### Faster Procedural Version

Faster, same output.

```d
void main() {
    import std.stdio, std.algorithm, std.file, std.range;

    string[] result;
    size_t maxWalkLen;

    foreach (word; "unixdict.txt".readText.splitter) {
        if (word.length >= maxWalkLen && word.isSorted) {
            immutable wlen = word.walkLength;
            if (wlen > maxWalkLen) {
                result.length = 0;
                maxWalkLen = wlen;
            }
            result ~= word.idup;
        }
    }

    writefln("%-(%s\n%)", result);
}
```



### Functional Version

Shorter, same output.

```d
void main() {
    import std.stdio, std.algorithm, std.range, std.file, std.string;

    auto words = "unixdict.txt".readText.split.filter!isSorted;
    immutable maxLen = words.map!q{a.length}.reduce!max;
    writefln("%-(%s\n%)", words.filter!(w => w.length == maxLen));
}
```



### Fastest Memory Mapped Version

Lower level, much faster with large input files (about as fast as the memory mapped C version). The output is the same. It works only with ASCII texts, but unlike the C entry this is portable.
```d
import std.stdio, core.stdc.string, std.mmfile, std.algorithm;

const(char)[] findWord(const char[] s) pure nothrow @safe @nogc {
    size_t wordEnd = 0;
    while (wordEnd < s.length && s[wordEnd] != '\n' && s[wordEnd] != '\r')
        wordEnd++;
    return s[0 .. wordEnd];
}

void main() {
    auto mmf = new MmFile("unixdict.txt", MmFile.Mode.readCopyOnWrite, 0, null);
    auto txt = cast(char[])(mmf[]);
    size_t maxLen = 0, outStart = 0;

    for (size_t wordStart = 0; wordStart < txt.length; ) {
        while (wordStart < txt.length &&
               (txt[wordStart] == '\r' || txt[wordStart] == '\n'))
            wordStart++;
        const word = findWord(txt[wordStart .. $]);
        wordStart += word.length;

        if (word.length < maxLen || !word.isSorted)
            continue;
        if (word.length > maxLen) {
            // Longer ordered word found, reset the out buffer.
            outStart = 0;
            maxLen = word.length;
        }

        // Use the same mmap'd region to store output. Because of
        // Mode.readCopyOnWrite, change will not go back to file.
        // We are using  only the head space to store output, so
        // kernel doesn't need to copy more than the words we saved,
        // in this case, one page tops.
        memcpy(&txt[outStart], word.ptr, word.length);
        outStart += word.length;
        txt[outStart++] = '\n'; // Words separator in out buffer.
    }

    txt[0 .. outStart].write;
}
```



## Delphi


```Delphi

program POrderedWords;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, IdHTTP;

function IsOrdered(const s:string): Boolean;
var
  I: Integer;
begin
  Result := Length(s)<2; // empty or 1 char strings are ordered
  for I := 2 to Length(s) do
    if s[I]<s[I-1] then // can improve using case/localization to order...
      Exit;
  Result := True;
end;

function ProcessDictionary(const AUrl: string): string;
var
  slInput: TStringList;
  I, WordSize: Integer;
begin
  slInput := TStringList.Create;
  try
    with TIdHTTP.Create(nil) do try
      slInput.Text := Get(AUrl);
    finally
      Free;
    end;
    // or use slInput.LoadFromFile('yourfilename') to load from a local file
    WordSize :=0;
    for I := 0 to slInput.Count-1 do begin
      if IsOrdered(slInput[I]) then
        if (Length(slInput[I]) = WordSize) then
          Result := Result + slInput[I] + ' '
        else if (Length(slInput[I]) > WordSize) then begin
          Result := slInput[I] + ' ';
          WordSize := Length(slInput[I]);
        end;
    end;
  finally
    slInput.Free;
  end;
end;

begin
  try
    WriteLn(ProcessDictionary('http://www.puzzlers.org/pub/wordlists/unixdict.txt'));
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

```


Output: dictionary directly processed from the URL


```Delphi

abbott accent accept access accost almost bellow billow biopsy chilly choosy choppy effort floppy glossy knotty

```



## E

```e
pragma.enable("accumulator")

def words := <http://www.puzzlers.org/pub/wordlists/unixdict.txt>.getText().split("\n")
def ordered := accum [] for word ? (word.sort() <=> word) in words { _.with(word) }
def maxLen := accum 0 for word in ordered { _.max(word.size()) }
def maxOrderedWords := accum [] for word ? (word.size() <=> maxLen) in ordered { _.with(word) }
println(" ".rjoin(maxOrderedWords))
```


One-pass procedural algorithm which avoids keeping the entire data set in memory:


```e
def best := [].diverge()
for `@word$\n` ? (word.sort() <=> word) in <http://www.puzzlers.org/pub/wordlists/unixdict.txt> {
  if (best.size() == 0) {
    best.push(word)
  } else if (word.size() > best[0].size()) {
    best(0) := [word] # replace all
  } else if (word.size() <=> best[0].size()) {
    best.push(word)
  }
}
println(" ".rjoin(best.snapshot()))
```


Output: <code>abbott accent accept access accost almost bellow billow biopsy chilly choosy
 choppy effort floppy glossy knotty</code>


## EchoLisp


```scheme

(define (ordered? str)
  (for/and ([i (in-range 1 (string-length str))])
    (string-ci<=? (string-ref str (1- i)) (string-ref str i))))

(define (ordre words)
(define wl 0)
(define s 's)
	(for/fold (len 0) ((w words))
		(set! wl (string-length w))
		#:continue (< wl len)
		#:when (ordered? w)
		#:continue (and (= len wl) (push s w))
		(push (stack s) w) ;; start a new list of length wl
		wl)
	(stack->list s))

;; output
(load 'unixdict)
(ordre (text-parse unixdict))
   → (abbott accent accept access accost almost bellow billow biopsy chilly choosy choppy effort floppy glossy knotty)

;; with the dictionaries provided with EchoLisp
;; french
  → (accentué) ;; ordered, longest, and ... self-reference
;; english
  → (Adelops alloquy beefily begorry billowy egilops)

```




## Elixir


```Elixir
File.read!("unixdict.txt")
|> String.split
|> Enum.filter(fn word -> String.codepoints(word) |> Enum.sort |> Enum.join == word end)
|> Enum.group_by(fn word -> String.length(word) end)
|> Enum.max_by(fn {length,_words} -> length end)
|> elem(1)
|> Enum.sort
|> Enum.each(fn word -> IO.puts word end)
```


```txt

abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty

```



## Erlang


```Erlang

-module( ordered_words ).

-export( [is_ordered/1, task/0] ).

is_ordered( Word ) -> lists:sort( Word ) =:=  Word.

task() ->
    ok = find_unimplemented_tasks:init_http(),
    Ordered_words = [X || X <- words(), is_ordered(X)],
    Sorted_longest_length_first = lists:reverse( sort_with_length( Ordered_words ) ),
    [{Max_length, _Word1} | _T] = Sorted_longest_length_first,
    Longest_length_first = lists:takewhile( fun({Length, _Word2}) -> Length =:= Max_length end, Sorted_longest_length_first ),
    [X || {_Length, X} <- Longest_length_first].



sort_with_length( Words ) ->
    Words_with_length_first = [{erlang:length(X), X} || X <- Words],
    lists:sort( Words_with_length_first ).

words() -> anagrams_deranged:words_from_url( "http://www.puzzlers.org/pub/wordlists/unixdict.txt" ).

```

```txt

2> ordered_words:task().
["knotty","glossy","floppy","effort","choppy","choosy",
 "chilly","biopsy","billow","bellow","almost","accost",
 "access","accept","accent","abbott"]

```



## Euphoria


```euphoria
include misc.e

type ordered(sequence s)
    for i = 1 to length(s)-1 do
        -- assume all items in the sequence are atoms
        if s[i]>s[i+1] then
            return 0
        end if
    end for
    return 1
end type

integer maxlen
sequence words
object word
constant fn = open("unixdict.txt","r")
maxlen = -1

while 1 do
    word = gets(fn)
    if atom(word) then
        exit
    end if
    word = word[1..$-1] -- truncate new-line
    if length(word) >= maxlen and ordered(word) then
        if length(word) > maxlen then
            maxlen = length(word)
            words = {}
        end if
        words = append(words,word)
    end if
end while

close(fn)

pretty_print(1,words,{2})
```


Output:

```txt
{
  "abbott",
  "accent",
  "accept",
  "access",
  "accost",
  "almost",
  "bellow",
  "billow",
  "biopsy",
  "chilly",
  "choosy",
  "choppy",
  "effort",
  "floppy",
  "glossy",
  "knotty"
}
```


=={{header|F Sharp|F#}}==

```fsharp
open System
open System.IO

let longestOrderedWords() =
    let isOrdered = Seq.pairwise >> Seq.forall (fun (a,b) -> a <= b)

    File.ReadLines("unixdict.txt")
    |> Seq.filter isOrdered
    |> Seq.groupBy (fun s -> s.Length)
    |> Seq.sortBy (fst >> (~-))
    |> Seq.head |> snd

longestOrderedWords() |> Seq.iter (printfn "%s")
```


Output:

```txt

abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```



## Factor


```factor

USING: grouping http.client io io.encodings.utf8 io.files
io.files.temp kernel math memoize sequences sequences.extras
unicode.case urls ;
IN: rosetta-code.ordered-words

MEMO: word-list ( -- seq )
    "unixdict.txt" temp-file dup exists? [
        URL" http://puzzlers.org/pub/wordlists/unixdict.txt"
        over download-to
    ] unless utf8 file-lines ;

: ordered-word? ( word -- ? )
    >lower [ <= ] monotonic? ;

: ordered-words-main ( -- )
    word-list [ ordered-word? ] filter
    all-longest [ print ] each ;

```


Output:
```txt
( scratchpad ) USING: ordered-words-main ;
( scratchpad ) main
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```



## Fantom



```fantom

class Main
{
  public static Bool ordered (Str word)
  {
    word.chars.all |Int c, Int i -> Bool|
    {
      (i == (word.size-1) || c <= word.chars[i+1])
    }
  }

  public static Void main ()
  {
    Str[] words := [,]
    File(`unixdict.txt`).eachLine |Str word|
    {
      if (ordered(word))
      {
        if (words.isEmpty || words.first.size < word.size)
        { // reset the list
          words = [word]
        }
        else if (words.size >= 1 && words.first.size == word.size)
        { // add word to existing ones
          words.add (word)
        }
      }
    }
    echo (words.join (" "))
  }
}

```


Output:

```txt
abbott accent accept access accost almost bellow billow biopsy chilly choosy choppy effort floppy glossy knotty

```


## FBSL

Downloads the list from puzzlers.org.

```qbasic
#APPTYPE CONSOLE

FUNCTION RESTfulGET(url)
    DIM %HTTP = CREATEOBJECT("WinHttp.WinHttpRequest.5.1")
    CALLMETHOD(HTTP, ".open %s, %s, %d", "GET", url, FALSE)
    CALLMETHOD(HTTP, ".send")
    RETURN GETVALUE("%s", HTTP, ".ResponseText")
END FUNCTION

DIM $TEXT = RESTfulGET("http://www.puzzlers.org/pub/wordlists/unixdict.txt")
DIM dict[] = Split(TEXT, CHR(10))
DIM max AS INTEGER = UBOUND(dict)
DIM theword AS STRING
DIM words[]
FOR DIM i = 0 TO max
    theWord = dict[i]
    IF isOrdered(theWord) THEN
        words[LEN(theWord)] = words[LEN(theWord)] & " " & theWord
    END IF
NEXT

PRINT words[UBOUND(words)]

PAUSE

FUNCTION isOrdered(s)
    FOR DIM i = 1 TO LEN(s) - 1
        IF s{i} > s{i + 1} THEN
            RETURN FALSE
        END IF
    NEXT
    RETURN TRUE
END FUNCTION

```

Output

```txt
 abbott accent accept access accost almost bellow billow biopsy chilly choosy choppy effort floppy glossy knotty

Press any key to continue...
```



## Forth

This program uses a string stack, which means that all matching words are stored on a stack.
The longest word ends up on the top of the stack.

```Forth

include lib/stmstack.4th               \ include string stack library

: check-word                           ( a n -- a n f)
  2dup bl >r                           \ start off with a space
  begin
    dup                                \ when not end of word
  while
    over c@ r@ >=                      \ check character
  while
    r> drop over c@ >r chop            \ chop character off
  repeat r> drop nip 0=                \ cleanup and set flag
;

: open-file                            ( -- h)
  1 dup argn = abort" Usage: ordered infile"
  args input open error? abort" Cannot open file"
  dup use                              \ return and use the handle
;

: read-file                            ( --)
  0 >r                                 \ begin with zero length
  begin
    refill                             \ EOF detected?
  while
    0 parse dup r@ >=                  \ equal or longer string length?
    if                                 \ check the word and adjust length
      check-word if r> drop dup >r >s else 2drop then
    else                               \ if it checks out, put on the stack
      2drop                            \ otherwise drop the word
    then
  repeat r> drop                       \ clean it up
;

: read-back                            ( --)
  s> dup >r type cr                    \ longest string is on top of stack
  begin s> dup r@ >= while type cr repeat
  2drop r> drop                        \ keep printing until shorter word
;                                      \ has been found

: ordered                              ( --)
  open-file s.clear read-file read-back close
;                                      \ open file, clear the stack, read file
                                       \ read it back and close the file
ordered
```

Since the longest word is on top of the stack, the only thing to be done is to pop
all words from the stack until a shorter word is encountered. Consequently, all
words are listed in reverse order:

```txt
knotty
glossy
floppy
effort
choppy
choosy
chilly
biopsy
billow
bellow
almost
accost
access
accept
accent
abbott
```



## Fortran



```fortran

!***************************************************************************************
 module ordered_module
!***************************************************************************************
 implicit none

	!the dictionary file:
	integer,parameter :: file_unit = 1000
	character(len=*),parameter :: filename = 'unixdict.txt'

	!maximum number of characters in a word:
	integer,parameter :: max_chars = 50

	type word
	  character(len=max_chars) :: str    !the word from the dictionary
	  integer	:: n = 0    !length of this word
	  logical	:: ordered = .false.    !if it is an ordered word
	end type word

	!the dictionary structure:
	type(word),dimension(:),allocatable :: dict

	contains
!***************************************************************************************

	!******************************************************************************
		function count_lines_in_file(fid) result(n_lines)
	!******************************************************************************
		implicit none

		integer             :: n_lines
		integer,intent(in)  :: fid
		character(len=1)    :: tmp
		integer             :: i
		integer             :: ios

		!the file is assumed to be open already.

		rewind(fid)	  !rewind to beginning of the file

		n_lines = 0
		do !read each line until the end of the file.
			read(fid,'(A1)',iostat=ios) tmp
			if (ios < 0) exit      !End of file
			n_lines = n_lines + 1  !row counter
		end do

		rewind(fid)   !rewind to beginning of the file

	!******************************************************************************
		end function count_lines_in_file
	!******************************************************************************

	!******************************************************************************
	 pure elemental function ordered_word(word) result(yn)
	!******************************************************************************
	! turns true if word is an ordered word, false if it is not.
	!******************************************************************************

	 implicit none
	 character(len=*),intent(in) :: word
	 logical :: yn

	 integer :: i

	 yn = .true.
	 do i=1,len_trim(word)-1
	 	if (ichar(word(i+1:i+1))<ichar(word(i:i))) then
	 		yn = .false.
	 		exit
	 	end if
	 end do

	!******************************************************************************
	 end function ordered_word
	!******************************************************************************

!***************************************************************************************
 end module ordered_module
!***************************************************************************************

!****************************************************
 program main
!****************************************************
 use ordered_module
 implicit none

	integer :: i,n,n_max

	!open the dictionary and read in all the words:
	open(unit=file_unit,file=filename)     		!open the file
	n = count_lines_in_file(file_unit)      !count lines in the file
	allocate(dict(n))                       !allocate dictionary structure
	do i=1,n                                !
		read(file_unit,'(A)') dict(i)%str   !each line is a word in the dictionary
		dict(i)%n = len_trim(dict(i)%str)   !save word length
	end do
	close(file_unit)                        !close the file

	!use elemental procedure to get ordered words:
	dict%ordered = ordered_word(dict%str)

	!max length of an ordered word:
	n_max = maxval(dict%n, mask=dict%ordered)

	!write the output:
	do i=1,n
		if (dict(i)%ordered .and. dict(i)%n==n_max) write(*,'(A,A)',advance='NO') trim(dict(i)%str),' '
	end do
	write(*,*) ''

!****************************************************
 end program main
!****************************************************

```


'''Output'''

```txt
abbott accent accept access accost almost bellow billow biopsy chilly choosy choppy effort floppy glossy knotty
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function isOrdered(s As Const String) As Boolean
  If Len(s) <= 1 Then Return True
  For i As Integer = 1 To Len(s) - 1
    If s[i] < s[i - 1] Then Return False
  Next
  Return True
End Function

Dim words() As String
Dim word As String
Dim maxLength As Integer = 0
Dim count As Integer = 0
Open "undict.txt" For Input As #1
While Not Eof(1)
  Line Input #1, word
  If isOrdered(word) Then
    If Len(word) = maxLength Then
      Redim Preserve words(0 To count)
      words(count) = word
      count += 1
    ElseIf Len(word) > maxLength Then
      Erase words
      maxLength = Len(word)
      Redim words(0 To 0)
      words(0) = word
      count = 1
    End If
  End If
Wend

Close #1

Print "The ordered words with the longest length ("; Str(maxLength); ") in undict.txt are :"
Print
For i As Integer = 0 To UBound(words)
  Print words(i)
Next
Print
Print "Press any key to quit"
Sleep
```


```txt

The ordered words with the longest length (6) in undict.txt are :

abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty

```



## Gambas


```gambas
Public Sub Main()
Dim sDict As String = File.Load(User.Home &/ "unixdict.txt")  'Store the 'Dictionary'
Dim sOrdered As New String[]                                  'To store ordered words
Dim sHold As New String[]                                     'General store
Dim sTemp As String                                           'Temp variable
Dim siCount As Short                                          'Counter

For Each sTemp In Split(sDict, gb.NewLine)                    'For each word in the Dictionary
  For siCount = 1 To Len(sTemp)                               'Loop for each letter in the word
    sHold.Add(Mid(sTemp, siCount, 1))                         'Put each letter in sHold array
  Next
  sHold.Sort()                                                'Sort sHold (abbott = abboot, zoom = mooz)
  If sTemp = sHold.Join("") Then sOrdered.Add(sTemp)          'If they are the same (abbott(OK) mooz(not OK)) then add to sOrdered
  sHold.Clear                                                 'Empty sHold
Next

siCount = 0                                                   'Reset siCount

For Each sTemp In sOrdered                                    'For each of the Ordered words
  If Len(sTemp) > siCount Then siCount = Len(sTemp)           'Count the length of the word and keep a record of the longest length
Next

For Each sTemp In sOrdered                                    'For each of the Ordered words
  If Len(sTemp) = siCount Then sHold.Add(sTemp)               'If it is one of the longest add it to sHold
Next

sHold.Sort()                                                  'Sort sHold
Print sHold.Join(gb.NewLine)                                  'Display the result

End
```

Output:

```txt

abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty

```



## Go

Go has strings and Unicode and stuff, but with the dictionary all ASCII
and lower case, strings and Unicode seem overkill.  I just worked with byte slices here, only converting the final result to strings for easy output.

```go
package main

import (
    "bytes"
    "fmt"
    "io/ioutil"
)

func main() {
    // read into memory in one chunk
    b, err := ioutil.ReadFile("unixdict.txt")
    if err != nil {
        fmt.Println(err)
        return
    }
    // split at line ends
    bss := bytes.Split(b, []byte{'\n'})

    // accumulate result
    var longest int
    var list [][]byte
    for _, bs := range bss {
        // don't bother with words shorter than
        // our current longest ordered word
        if len(bs) < longest {
            continue
        }
        // check for ordered property
        var lastLetter byte
        for i := 0; ; i++ {
            if i == len(bs) {
                // end of word.  it's an ordered word.
                // save it and break from loop
                if len(bs) > longest {
                    longest = len(bs)
                    list = list[:0]
                }
                list = append(list, bs)
                break
            }
            // check next letter
            b := bs[i]
            if b < 'a' || b > 'z' {
                continue // not a letter.  ignore.
            }
            if b < lastLetter {
                break // word not ordered.
            }
            // letter passes test
            lastLetter = b
        }
    }
    // print result
    for _, bs := range list {
        fmt.Println(string(bs))
    }
}
```

Output:

```txt

abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty

```



## Groovy

Solution:

```groovy
def isOrdered = { word -> def letters = word as List; letters == ([] + letters).sort() }
assert isOrdered('abbey')
assert !isOrdered('cat')

def dictUrl = new URL('http://www.puzzlers.org/pub/wordlists/unixdict.txt')
def orderedWords = dictUrl.readLines().findAll { isOrdered(it) }
def owMax = orderedWords*.size().max()

orderedWords.findAll { it.size() == owMax }.each { println it }
```


Output:

```txt
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```


## Haskell


```haskell

-- Words are read from the standard input.  We keep in memory only the current
-- set of longest, ordered words.
--
-- Limitation: the locale's collation order is not take into consideration.

isOrdered wws@(_:ws) = and $ zipWith (<=) wws ws

longestOrderedWords = reverse . snd . foldl f (0,[]) . filter isOrdered
  where f (max, acc) w =
          let len = length w in
          case compare len max of
            LT -> (max, acc)
            EQ -> (max, w:acc)
            GT -> (len, [w])

main = do
  str <- getContents
  let ws = longestOrderedWords $ words str
  mapM_ putStrLn ws

```


Output:

```haskell

abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty

```

Alternative version:

```haskell
import Control.Monad (liftM)

isSorted wws@(_ : ws) = and $ zipWith (<=) wws ws

getLines = liftM lines . readFile

main = do
    ls <- getLines "unixdict.txt"
    let ow = filter isSorted ls
    let maxl = foldr max 0 (map length ow)
    print $ filter (\w -> (length w) == maxl) ow
```



## Huginn


```huginn
import Algorithms as algo;
import Mathematics as math;
import Network as net;
import Text as text;

main( argv_ ) {
  url = size( argv_ ) > 1
    ? argv_[1]
    : "http://wiki.puzzlers.org/pub/wordlists/unixdict.txt";
  words = algo.materialize( algo.map( net.get( url ).stream, string.strip ), list );
  ordered = algo.materialize(
    algo.filter(
      words,
      @( word ){ word == ∑( algo.map( algo.sorted( word ), string ) ); }
    ),
    list
  );
  maxLen = algo.reduce( ordered, @( x, y ){ math.max( x, size( y ) ); }, 0 );
  maxOrderedWords = algo.materialize(
    algo.filter( ordered, @[maxLen]( word ){ size( word ) == maxLen; } ),
    list
  );
  print( "{}\n".format( text.join( algo.sorted( maxOrderedWords ), " " ) ) );
  return ( 0 );
}
```


=={{header|Icon}} and {{header|Unicon}}==

```Unicon
link strings

procedure main(A)
   f := open(\A[1]) | stop("Give dictionary file name on command line")
   every (maxLen := 0, maxLen <= *(w := !f), w == csort(w)) do {
      if maxLen <:= *w then maxList := []  #discard any shorter sorted words
      put(maxList, w)
      }
   every write(!\maxList)
end
```


[http://www.cs.arizona.edu/icon/library/src/procs/strings.icn strings provides csort] which sorts the letters within a string

Output:

```txt
->ordered_words unixdict.txt
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
->
```



## Io


```Io
file := File clone openForReading("./unixdict.txt")
words := file readLines
file close

maxLen := 0
orderedWords := list()
words foreach(word,
    if( (word size >= maxLen) and (word == (word asMutable sort)),
        if( word size > maxLen,
            maxLen = word size
            orderedWords empty
        )
        orderedWords append(word)
    )
)

orderedWords join(" ") println
```

```txt
abbott accent accept access accost almost bellow billow biopsy chilly choosy choppy effort floppy glossy knotty

```



## J


```j
   require'web/gethttp'
   dict=: gethttp'http://www.puzzlers.org/pub/wordlists/unixdict.txt'
   oWords=: (#~ ] = /:~L:0) <;._2 dict-.CR
   ;:inv (#~ (= >./)@:(#@>))oWords
abbott accent accept access accost almost bellow billow biopsy chilly choosy choppy effort floppy glossy knotty
```


Recap:

# fetch dictionary (<code>dict</code>)
# break into words, one per line (<code><;._2 dict-.CR</code>)
# find ordered words  (<code>oWords</code>)
# select the longest ordered words (<code>(#~ (= >./)@:(#@>))oWords</code>)
# format for display (using ;:inv)


## Java

This example assumes there is a local copy of the dictionary whose path is given as the first argument to the program.

```java5
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

public class Ordered {

	private static boolean isOrderedWord(String word){
		char[] sortedWord = word.toCharArray();
		Arrays.sort(sortedWord);
		return word.equals(new String(sortedWord));
	}

	public static void main(String[] args) throws IOException{
		List<String> orderedWords = new LinkedList<String>();
		BufferedReader in = new BufferedReader(new FileReader(args[0]));
		while(in.ready()){
			String word = in.readLine();
			if(isOrderedWord(word)) orderedWords.add(word);
		}
		in.close();

		Collections.<String>sort(orderedWords, new Comparator<String>() {
			@Override
			public int compare(String o1, String o2) {
				return new Integer(o2.length()).compareTo(o1.length());
			}
		});

		int maxLen = orderedWords.get(0).length();
		for(String word: orderedWords){
			if(word.length() == maxLen){
				System.out.println(word);
			}else{
				break;
			}
		}
	}
}
```

Output:

```txt
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```



## JavaScript

Using [http://nodejs.org/ node.js]:


```javascript
var fs = require('fs'), print = require('sys').print;
fs.readFile('./unixdict.txt', 'ascii', function (err, data) {
    var is_ordered = function(word){return word.split('').sort().join('') === word;},
        ordered_words = data.split('\n').filter(is_ordered).sort(function(a, b){return a.length - b.length}).reverse(),
        longest = [], curr = len = ordered_words[0].length, lcv = 0;
    while (curr === len){
        longest.push(ordered_words[lcv]);
        curr = ordered_words[++lcv].length;
    };
    print(longest.sort().join(', ') + '\n');
});
```


Output:

```txt
abbott, accent, accept, access, accost, almost, bellow, billow, biopsy, chilly, choosy, choppy, effort, floppy, glossy, knotty
```


Alternative version (also using Node.js):


```javascript
var http = require('http');

http.get({
  host: 'www.puzzlers.org',
  path: '/pub/wordlists/unixdict.txt'
}, function(res) {
  var data = '';
  res.on('data', function(chunk) {
    data += chunk;
  });
  res.on('end', function() {
    var words = data.split('\n');
    var max = 0;
    var ordered = [];
    words.forEach(function(word) {
      if (word.split('').sort().join('') != word) return;
      if (word.length == max) {
        ordered.push(word);
      } else if (word.length > max) {
        ordered = [word];
        max = word.length;
      }
    });
    console.log(ordered.join(', '));
  });
});
```


## jq

```jq
def is_sorted:
  if length <= 1 then true
  else .[0] <= .[1] and (.[1:] | is_sorted)
  end;

def longest_ordered_words:
  # avoid string manipulation:
  def is_ordered: explode | is_sorted;
  map(select(is_ordered))
  | (map(length)|max) as $max
  | map( select(length == $max) );


split("\n")  | longest_ordered_words
```

 ["abbott","accent","accept","access","accost","almost","bellow","billow","biopsy","chilly","choosy","choppy","effort","floppy","glossy","knotty"]


## Julia

'''Built-in function''':

```julia
issorted("abc") # true
```


'''Main''':

```julia
lst = readlines("data/unixdict.txt")
filter!(issorted, lst)
filter!(x -> length(x) == maximum(length, lst), lst)
println.(lst)
```


```txt
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```



## K


```K
    w@&d=|/d:#:'w:d@&&/'{~x<y}':'d:0:"unixdict.txt"
("abbott"
 "accent"
 "accept"
 "access"
 "accost"
 "almost"
 "bellow"
 "billow"
 "biopsy"
 "chilly"
 "choosy"
 "choppy"
 "effort"
 "floppy"
 "glossy"
 "knotty")
```



## Kotlin



```scala
import java.io.File

fun main(args: Array<String>) {
    val file = File("unixdict.txt")
    val result = mutableListOf<String>()

    file.forEachLine {
        if (it.toCharArray().sorted().joinToString(separator = "") == it) {
            result += it
        }
    }

    result.sortByDescending { it.length }
    val max = result[0].length

    for (word in result) {
        if (word.length == max) {
            println(word)
        }
    }
}
```



## Lang5


```lang5
:>
string-index
    "" split
    "&'0123456789abcdefghijklmnopqrstuvwxyz" "" split
    swap index collapse ;
: chars  "" split length swap drop ;
: cr  "\n" . ;
: nip  swap drop ;
: ordered?
    dup grade subscript != '+ reduce if 0 else -1 then ;

: filtering
    [] '_ set
    0 do read
        2dup chars
        <=
        if  dup >string-index ordered?
            if   2dup chars
                 <
                 if   nip dup chars swap
                      [] '_ set
                 then
                 _ swap append '_ set
                 '. .                       # progress dot
            else drop
            then
        else drop
        then
    eof if break then loop

    cr _ . cr
    ;

: ordered-words
    '< 'unixdict.txt open 'fh set
    fh fin filtering fh close ;

ordered-words
```

```txt
[ abbott accent accept access accost almost bellow billow biopsy chilly choosy choppy effort floppy glossy knotty ]
```




## Lasso


```Lasso
local(f = file('unixdict.txt'), words = array, ordered = array, maxleng = 0)
#f->dowithclose => {
	#f->foreachLine => {
		#words->insert(#1)
	}
}
with w in #words
do => {
	local(tosort = #w->asString->values)
	#tosort->sort
	if(#w->asString == #tosort->join('')) => {
		#ordered->insert(#w->asString)
		#w->asString->size > #maxleng ? #maxleng = #w->asString->size
	}
}
with w in #ordered
where #w->size == #maxleng
do => {^ #w + '\r' ^}
```

```txt
abbott accent accept access accost almost bellow billow biopsy chilly choosy choppy effort floppy glossy knotty
```



## Liberty BASIC


```lb

'Ordered wordsFrom Rosetta Code
open "unixdict.txt" for input as #1
'this is not normal DOS/Windows file.
'It LF delimited, not CR LF
'So Line input would not work.

lf$=chr$(10)
curLen=0
wordList$=""
while not(eof(#1))
    a$=inputto$(#1, lf$)
    'now, check word
    flag = 1
    c$ = left$(a$,1)
    for i = 2 to len(a$)
        d$ = mid$(a$,i,1)
        if c$>d$ then flag=0: exit for
        c$=d$
    next
    'ckecked, proceed if ordered word
    if flag then
        if curLen=len(a$) then
            wordList$=wordList$+" "+a$
        else
            if curLen<len(a$) then
                curLen=len(a$)
                wordList$=a$
            end if
        end if
    end if
wend
close #1
print wordList$

```


Output:

```txt
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```



## Lingo

Code ported from Lua solution.

```lingo
-- Contents of unixdict.txt passed as string
on printLongestOrderedWords (words)
  res = []
  maxlen = 0
  _player.itemDelimiter = numtochar(10)
  cnt = words.item.count
  repeat with i = 1 to cnt
    w = words.item[i]
    len = w.length
    ordered = TRUE
    repeat with j = 2 to len
      if chartonum(w.char[j-1])>chartonum(w.char[j]) then
        ordered = FALSE
        exit repeat
      end if
    end repeat
    if ordered then
      if len > maxlen then
        res = [w]
        maxlen = len
      else if len = maxlen then
        res.add(w)
      end if
    end if
  end repeat
  put res
end
```

```txt
-- ["abbott", "accent", "accept", "access", "accost", "almost", "bellow", "billow", "biopsy", "chilly", "choosy", "choppy", "effort", "floppy", "glossy", "knotty"]
```



## Lua


```lua
fp = io.open( "dictionary.txt" )

maxlen = 0
list = {}

for w in fp:lines() do
    ordered = true
    for l = 2, string.len(w) do
	if string.byte( w, l-1 ) > string.byte( w, l ) then
	    ordered = false
  	    break
	end
    end
    if ordered then
	if string.len(w) > maxlen then
	    list = {}
	    list[1] = w
	    maxlen = string.len(w)
	elseif string.len(w) == maxlen then
	    list[#list+1] = w
	end
    end
end

for _, w in pairs(list) do
    print( w )
end

fp:close()
```

Output:

```txt
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```



## Maple


```Maple
lst := StringTools:-Split(Import("http://www.puzzlers.org/pub/wordlists/unixdict.txt"), "\n"):
longest := 0:
words := Array():
i := 1:
for word in lst do
	if StringTools:-IsSorted(word) then
		len := StringTools:-Length(word):
		if len > longest then
			longest := len:
			words := Array():
			words(1) := word:
			i := 2:
		elif len = longest then
			words(i) := word:
			i++:
		end if;
	end if;
end do;
for word in words do print(word); end do;
```

```txt
"abbott"
"accent"
"accept"
"access"
"accost"
"almost"
"bellow"
"billow"
"biopsy"
"chilly"
"choosy"
"choppy"
"effort"
"floppy"
"glossy"
"knotty"
```




## Mathematica


```mathematica
Module[{max,
   data = Select[Import["http://www.puzzlers.org/pub/wordlists/unixdict.txt", "List"],
     OrderedQ[Characters[#]] &]},
  max = Max[StringLength /@ data];
  Select[data, StringLength[#] == max &]]

```


However Mathematica has built in dictionaries for many languages, so here is a more general version...


```Mathematica
maxWords[language_String] :=  Module[{max,data = Select[DictionaryLookup[{language, "*"}],OrderedQ[Characters[#]] &]},
  max = Max[StringLength /@ data];
  Select[data, StringLength[#] == max &]]

```



```txt

findLargestOrderedWord["English"]
{"billowy"}

findLargestOrderedWord["French"]
{"accent", "afflux", "bijoux", "billot", "dehors", "effort"}

findLargestOrderedWord["Spanish"]
{"abenuz", "chillo"}

```


=={{header|MATLAB}} / {{header|Octave}}==


```Matlab
maxlen = 0;
listlen= 0;
fid = fopen('unixdict.txt','r');
while ~feof(fid)
    str = fgetl(fid);
    if any(diff(abs(str))<0) continue; end;

    if length(str)>maxlen,
	list = {str};
	maxlen = length(str);
    elseif length(str)==maxlen,
	list{end+1} = str;
    end;
end
fclose(fid);
printf('%s\n',list{:});
```


Returns:

```txt
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols binary

unixdict = 'unixdict.txt'
do
  wmax = Integer.MIN_VALUE
  dwords = ArrayList()
  inrdr = BufferedReader(FileReader(File(unixdict)))
  loop label ln while inrdr.ready
    dword = Rexx(inrdr.readLine).strip
    if isOrdered(dword) then do
      dwords.add(dword)
      if dword.length > wmax then
        wmax = dword.length
      end
    end ln
  inrdr.close

  witerator = dwords.listIterator
  loop label wd while witerator.hasNext
    dword = Rexx witerator.next
    if dword.length < wmax then do
      witerator.remove
      end
    end wd
  dwords.trimToSize

  say dwords.toString

catch ex = IOException
  ex.printStackTrace
end

return

method isOrdered(dword = String) inheritable static binary returns boolean
  wchars = dword.toCharArray
  Arrays.sort(wchars)
  return dword.equalsIgnoreCase(String(wchars))

```

;Output

```txt

[abbott, accent, accept, access, accost, almost, bellow, billow, biopsy, chilly, choosy, choppy, effort, floppy, glossy, knotty]

```



## Nim


```nim
import httpclient, strutils

proc isSorted(s): bool =
  var last = low(char)
  for c in s:
    if c < last:
      return false
    last = c
  return true

const url = "http://www.puzzlers.org/pub/wordlists/unixdict.txt"
var mx = 0
var words: seq[string] = @[]

for word in getContent(url).split():
  if word.len >= mx and isSorted(word):
    if word.len > mx:
      words = @[]
      mx = word.len
    words.add(word)
echo words.join(" ")
```

Output:

```txt
abbott accent accept access accost almost bellow billow biopsy chilly choosy choppy effort floppy glossy knotty
```



## OCaml



```ocaml
let input_line_opt ic =
  try Some(input_line ic)
  with End_of_file -> None

(* load each line in a list *)
let read_lines ic =
  let rec aux acc =
    match input_line_opt ic with
    | Some line -> aux (line :: acc)
    | None -> (List.rev acc)
  in
  aux []

let char_list_of_string str =
  let lst = ref [] in
  String.iter (fun c -> lst := c :: !lst) str;
  (List.rev !lst)

let is_ordered word =
  let rec aux = function
    | c1::c2::tl ->
        if c1 <= c2
        then aux (c2::tl)
        else false
    | c::[] -> true
    | [] -> true  (* should only occur with an empty string *)
  in
  aux (char_list_of_string word)

let longest_words words =
  let res, _ =
    List.fold_left
      (fun (lst, n) word ->
        let len = String.length word in
        let comp = compare len n in
        match lst, comp with
        | lst, 0  -> ((word::lst), n) (* len = n *)
        | lst, -1 -> (lst, n)         (* len < n *)
        | _, 1    -> ([word], len)    (* len > n *)
        | _ -> assert false
      )
      ([""], 0) words
  in
  (List.rev res)

let () =
  let ic = open_in "unixdict.txt" in
  let words = read_lines ic in
  let lower_words = List.map String.lowercase words in
  let ordered_words = List.filter is_ordered lower_words in
  let longest_ordered_words = longest_words ordered_words in
  List.iter print_endline longest_ordered_words
```


Output:


```txt
$ ocaml ordered_words.ml
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```



## Oforth



```Oforth
: longWords
| w longest l s |
   0 ->longest
   File new("unixdict.txt") forEach: w [
      w size dup ->s longest < ifTrue: [ continue ]
      w sort w == ifFalse: [ continue ]
      s longest > ifTrue: [ s ->longest ListBuffer new ->l ]
      l add(w)
      ] l ;
```


```txt

>longWords .
[abbott, accent, accept, access, accost, almost, bellow, billow, biopsy, chilly, choosy, choppy, effort, floppy, glossy, knotty]

```



## ooRexx

Adapted for ooRexx

```oorexx
/*REXX list (the longest) ordered word(s) from a supplied dictionary. */
iFID= 'UNIXDICT.TXT'
w.=''
mL=0
Do j=1 While lines(iFID)\==0
  x=linein(iFID)
  w=length(x)
  If w>=mL Then Do
    Parse Upper Var x xU 1 z 2
    Do k=2 To w
      _=substr(xU, k, 1)
      If \datatype(_, 'U')  Then Iterate
      If _<z                Then Iterate j
      z=_
      End
    mL=w
    w.w=w.w  x
    End
  End
nn=words(w.mL)
Say nn 'word's(nn) "found (of length" mL')'
Say ''
Do n=1 To nn
  Say word(w.mL, n)
  End
Exit
s: Return left('s',arg(1)>1)
```

Same as REXX'


## PARI/GP


```parigp
ordered(s)=my(v=Vecsmall(s),t=97);for(i=1,#v,if(v[i]>64&&v[i]<91,v[i]+=32);if(v[i]<97||v[i]>122||v[i]<t,return(0),t=v[i]));1
v=select(ordered,readstr("~/unixdict.txt"));
N=vecmax(apply(length,v));
select(s->#s==N, v)
```

```txt
%1 = ["abbott", "accent", "accept", "access", "accost", "almost", "bellow", "billow", "biopsy", "chilly", "choosy", "choppy", "effort", "floppy", "glossy", "knotty"]
```



## Perl


```Perl
#!/usr/bin/perl
use strict;
use warnings;

open(FH, "<", "unixdict.txt") or die "Can't open file!\n";
my @words;
while (<FH>) {
   chomp;
   push @{$words[length]}, $_ if $_ eq join("", sort split(//));
}
close FH;
print "@{$words[-1]}\n";

```

Output:

```txt

abbott accent accept access accost almost bellow billow biopsy chilly choosy choppy effort floppy glossy knotty

```



## Perl 6


Here we assume the dictionary is provided on standard input.

```perl6
say lines.grep({ [le] .comb }).classify(*.chars).max(*.key).value
```


```txt

[abbott accent accept access accost almost bellow billow biopsy chilly choosy choppy effort floppy glossy knotty]

```



## Phix

Copy of [[Ordered_words#Euphoria|Euphoria]]

```Phix
type ordered(sequence s)
    for i=1 to length(s)-1 do
        -- assume all items in the sequence are atoms
        if s[i]>s[i+1] then
            return 0
        end if
    end for
    return 1
end type

integer maxlen
sequence words
object word
constant fn = open("demo\\unixdict.txt","r")
maxlen = -1

while 1 do
    word = gets(fn)
    if atom(word) then
        exit
    end if
    word = trim(word)
    if length(word)>=maxlen and ordered(lower(word)) then
        if length(word)>maxlen then
            maxlen = length(word)
            words = {}
        end if
        words = append(words,word)
    end if
end while

close(fn)

?words
```

```txt

{"abbott","accent","accept","access","accost","almost","bellow","billow","biopsy","chilly","choosy","choppy","effort","floppy","glossy","knotty"}

```



## PicoLisp


```PicoLisp
(in "unixdict.txt"
   (mapc prinl
      (maxi '((L) (length (car L)))
         (by length group
            (filter '((S) (apply <= S))
               (make (while (line) (link @))) ) ) ) ) )
```

Output:

```txt
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```



## PL/I


```pli

order: procedure options (main);               /* 24/11/2011 */
   declare word      character (20) varying;
   declare word_list character (20) varying controlled;
   declare max_length fixed binary;
   declare input file;

   open file (input) title ('/ORDER.DAT,TYPE(TEXT),RECSIZE(100)');

   on endfile (input) go to completed_search;

   max_length = 0;
   do forever;
      get file (input) edit (word) (L);
      if length(word) > max_length then
         do;
            if in_order(word) then
               do;
                  /* Get rid of any stockpiled shorter words. */
                  do while (allocation(word_list) > 0);
                     free word_list;
                  end;
                  /* Add the eligible word to the stockpile. */
                  allocate word_list;
                  word_list = word;
                  max_length = length(word);
               end;
         end;
      else if max_length = length(word) then
         do; /* we have an eligle word of the same (i.e., maximum) length. */
            if in_order(word) then
               do; /* Add it to the stockpile. */
                  allocate word_list;
                  word_list = word;
               end;
         end;
   end;
completed_search:
   put skip list ('There are ' || trim(allocation(word_list)) ||
      ' eligible words of length ' || trim(length(word)) || ':');
   do while (allocation(word_list) > 0);
      put skip list (word_list);
      free word_list;
   end;

/* Check that the letters of the word are in non-decreasing order of rank. */
in_order: procedure (word) returns (bit(1));
   declare word character (*) varying;
   declare i fixed binary;

   do i = 1 to length(word)-1;
      if substr(word, i, 1) > substr(word, i+1, 1) then return ('0'b);
   end;
   return ('1'b);
end in_order;
end order;

```

OUTPUT:

```txt

There are 16 eligible words of length 6:
knotty
glossy
floppy
effort
choppy
choosy
chilly
biopsy
billow
bellow
almost
accost
access
accept
accent
abbott

```



## PowerShell


```PowerShell

$url = 'http://www.puzzlers.org/pub/wordlists/unixdict.txt'

(New-Object System.Net.WebClient).DownloadFile($url, "$env:TEMP\unixdict.txt")

$ordered = Get-Content -Path "$env:TEMP\unixdict.txt" |
    ForEach-Object {if (($_.ToCharArray() | Sort-Object) -join '' -eq $_) {$_}} |
    Group-Object  -Property Length |
    Sort-Object   -Property Name |
    Select-Object -Property @{Name="WordCount" ; Expression={$_.Count}},
                            @{Name="WordLength"; Expression={[int]$_.Name}},
                            @{Name="Words"     ; Expression={$_.Group}} -Last 1

"There are {0} ordered words of the longest word length ({1} characters):`n`n{2}" -f $ordered.WordCount,
                                                                                     $ordered.WordLength,
                                                                                    ($ordered.Words -join ", ")
Remove-Item -Path "$env:TEMP\unixdict.txt" -Force -ErrorAction SilentlyContinue

```


```txt

There are 16 ordered words of the longest word length (6 characters):

abbott, accent, accept, access, accost, almost, bellow, billow, biopsy, chilly, choosy, choppy, effort, floppy, glossy, knotty

```



## Prolog

Works with SWI-Prolog


```Prolog
:- use_module(library( http/http_open )).

ordered_words :-
        % we read the URL of the words
	http_open('http://www.puzzlers.org/pub/wordlists/unixdict.txt',	In, []),
	read_file(In, [], Out),
	close(In),

        % we get a list of pairs key-value where key = Length and value = <list-of-its-codes>
        % this list must be sorted
	msort(Out, MOut),

	group_pairs_by_key(MOut, POut),

       % we sorted this list in decreasing order of the length of values
	predsort(my_compare, POut, [_N-V | _OutSort]),
	maplist(mwritef, V).


mwritef(V) :-
	writef('%s\n', [V]).

read_file(In, L, L1) :-
	read_line_to_codes(In, W),
	(   W == end_of_file ->
               % the file is read
	       L1 = L
	       ;
               % we sort the list of codes of the line
	       % and keep only the "goods word"
	       (   msort(W, W) ->
	           length(W, N), L2 = [N-W | L], (len = 6 -> writef('%s\n', [W]); true)
	       ;
	           L2 = L
	       ),

               % and we have the pair Key-Value in the result list
	       read_file(In, L2, L1)).

% predicate for sorting list of pairs Key-Values
% if the lentgh of values is the same
% we sort the keys in alhabetic order
my_compare(R, K1-_V1, K2-_V2) :-
	(   K1 < K2 -> R = >; K1 > K2 -> R = <; =).

```

Output :

```txt
 ?- ordered_words.
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
true.

```



## PureBasic


```PureBasic
Procedure.s sortLetters(*word.Character, wordLength) ;returns a string with the letters of a word sorted
  Protected Dim letters.c(wordLength)
  Protected *letAdr = @letters()

  CopyMemoryString(*word, @*letAdr)
  SortArray(letters(), #PB_Sort_Ascending, 0, wordLength - 1)
  ProcedureReturn PeekS(@letters(), wordLength)
EndProcedure

Structure orderedWord
  word.s
  length.i
EndStructure

Define filename.s = "unixdict.txt", fileNum = 0,  word.s

If OpenConsole()
  NewList orderedWords.orderedWord()
  If ReadFile(fileNum, filename)
    While Not Eof(fileNum)
      word = ReadString(fileNum)
      If word = sortLetters(@word, Len(word))
        AddElement(orderedWords())
        orderedWords()\word = word
        orderedWords()\length = Len(word)
      EndIf
    Wend
  Else
    MessageRequester("Error", "Unable to find dictionary '" + filename + "'")
    End
  EndIf

  SortStructuredList(orderedWords(), #PB_Sort_Ascending, OffsetOf(orderedWord\word), #PB_String)
  SortStructuredList(orderedWords(), #PB_Sort_Descending, OffsetOf(orderedWord\length), #PB_Integer)
  Define maxLength
  FirstElement(orderedWords())
  maxLength = orderedWords()\length
  ForEach orderedWords()
    If orderedWords()\length = maxLength
      Print(orderedWords()\word + "  ")
    EndIf
  Next

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
abbott  accent  accept  access  accost  almost  bellow  billow  biopsy  chilly
choosy  choppy  effort  floppy  glossy  knotty
```



## Python


### Python: First Solution


```python
import urllib.request

url = 'http://www.puzzlers.org/pub/wordlists/unixdict.txt'
words = urllib.request.urlopen(url).read().decode("utf-8").split()
ordered = [word for word in words if word==''.join(sorted(word))]
maxlen = len(max(ordered, key=len))
maxorderedwords = [word for word in ordered if len(word) == maxlen]
print(' '.join(maxorderedwords))
```



### Python: Alternate Solution using one explicit loop


```python
import urllib.request

mx, url = 0, 'http://www.puzzlers.org/pub/wordlists/unixdict.txt'

for word in urllib.request.urlopen(url).read().decode("utf-8").split():
    lenword = len(word)
    if lenword >= mx and word==''.join(sorted(word)):
        if lenword > mx:
            words, mx = [], lenword
        words.append(word)
print(' '.join(words))
```


'''Sample Output'''

```txt
abbott accent accept access accost almost bellow billow biopsy chilly choosy choppy effort floppy glossy knotty
```



### Python: As a fold

```python
'''The longest ordered words in a list'''

from functools import reduce
import urllib.request


# longestOrds :: [String] -> [String]
def longestOrds(ws):
    '''The longest ordered words in a given list.
    '''
    return reduce(triage, ws, (0, []))[1]


# triage :: (Int, [String]) -> String -> (Int, [String])
def triage(nxs, w):
    '''The maximum length seen for an ordered word,
       and the ordered words of this length seen so far.
    '''
    n, xs = nxs
    lng = len(w)
    return (
        (lng, ([w] if n != lng else xs + [w])) if (
            ordWord(w)
        ) else nxs
    ) if lng >= n else nxs


# ordWord :: String -> Bool
def ordWord(w):
    '''True if the word w is ordered.'''
    return reduce(stillRising, w[1:], (True, w[0]))[0]


# stillRising :: (Bool, Char) -> Char -> (Bool, Char)
def stillRising(bc, x):
    '''A boolean value paired with the current character.
       The boolean is true if no character in the word
       so far has been alphabetically lower than its
       predecessor.
    '''
    b, c = bc
    return ((x >= c) if b else b, x)


# TEST ---
if __name__ == '__main__':
    print(
        '\n'.join(longestOrds(
            urllib.request.urlopen(
                'http://wiki.puzzlers.org/pub/wordlists/unixdict.txt'
            ).read().decode("utf-8").split()
        ))
    )
```

```txt
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```



## Racket



```Racket

#lang racket
(require net/url)

(define dict "http://www.puzzlers.org/pub/wordlists/unixdict.txt")

(define (ordered? str)
  (define lower (string-downcase str))
  (for/and ([i (in-range 1 (string-length str))])
    (char<=? (string-ref lower (sub1 i)) (string-ref lower i))))

(define words (port->lines (get-pure-port (string->url dict))))

(let loop ([len 0] [longs '()] [words words])
  (if (null? words)
    (for-each displayln (reverse longs))
    (let* ([word (car words)] [words (cdr words)]
           [wlen (string-length word)])
      (if (or (< wlen len) (not (ordered? word)))
        (loop len longs words)
        (loop wlen (cons word (if (> wlen len) '() longs)) words)))))

```


Output:

```txt

abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty

```


## Red


```Red
Red []
;; code to read url and save to local file:
;;data: read/binary http://www.puzzlers.org/pub/wordlists/unixdict.txt
;;write %unixdict.txt data

max: [ "" ] ;; init array with one empty string (length 0 )

foreach word read/lines %unixdict.txt [   ;; read local file
  len: either  word = sort copy word [ length? word ] [  -1 ]   ;; check if ordered and get length
  case [
    len  > length?  first max [    max:  reduce [ word ]]       ;; init new block
    len = length? first max   [ append max word   ]
  ]
]
probe max
```

'''Sample Output'''

```txt
["abbott" "accent" "accept" "access" "accost" "almost" "bellow" "billow" "biopsy" "chilly" "choosy" "choppy" "effort" "floppy" "glossy" "knotty"]

```


## REXX

This problem assumes (or implies) an order of letter case, but fortunately, there're no uppercase letters

in the (default) dictionary to see which programs would fail when not recognizing this distinction.


In <code> ASCII</code>,   '''A'''   is less than   '''a''',   while in <code>EBCDIC</code>,   it's the other way around.

The problem can be avoided by first converting the word to a specific case, and then test for ordered

letters in that word (as indeed, this REXX entry does).


Output could be improved by sorting the words in the dictionary, but adding a sort would   ''bulk up''   the program;

but since the word list   (in this case)   is already in alphabetical order, this would-be improvement is

mostly moot.

```rexx
/*REXX program  lists  (the longest)  ordered word(s)  from a  supplied  dictionary.    */
iFID= 'UNIXDICT.TXT'                             /*the filename of the word dictionary. */
@.=                                              /*placeholder array for list of words. */
mL=0                                             /*maximum length of the ordered words. */
call linein iFID, 1, 0                           /*point to the first word in dictionary*/
                                                 /* [↑]  just in case the file is open. */
  do j=1  while lines(iFID)\==0;  x=linein(iFID) /*keep reading until file is exhausted.*/
  w=length(x);     if w<mL  then iterate         /*Word not long enough? Then ignore it.*/
  parse upper var  x  xU  1  z  2                /*get uppercase version of X & 1st char*/
                                                 /* [↓]  handle words of mixed case.    */
        do k=2  to w;         _=substr(xU, k, 1) /*process each letter in uppercase word*/
        if \datatype(_, 'U')  then iterate       /*Is it not a letter?  Then ignore it. */
        if _<z                then iterate j     /*is letter < than the previous letter?*/
        z=_                                      /*we have a newer current letter.      */
        end   /*k*/                              /* [↑]  logic includes  ≥  order.      */
  mL=w                                           /*maybe define a new maximum length.   */
  @.w=@.w  x                                     /*add the original word to a word list.*/
  end   /*j*/                                    /*the 1st DO needs an index for ITERATE*/
              #=words(@.mL)                      /*just a handy─dandy variable to have. */
say # 'word's(#)  "found (of length" mL')';  say /*show the number of words and length. */
        do n=1  for #;   say word(@.mL, n);  end /*display all the words, one to a line.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
s:  if arg(1)==1  then return '';   return "s"   /*a simple pluralizer (merely adds "S")*/
```

'''output'''   when using the default supplied word dictionary:

```txt

16 words found (of length 6)

abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty

```



## Ring


```ring

fn = "C:\Ring\unixdict.txt"

fp = fopen(fn,"r")
str = fread(fp, getFileSize(fp))
str = substr(str, windowsnl(), nl)
clist = str2list(str)
fclose(fp)
dlist = []

for m = 1 to len(clist)
    flag = 1
    for n = 1 to len(clist[m])-1
        if ascii(clist[m][n+1]) < ascii(clist[m][n])
           flag=0 exit ok
    next
    if flag = 1
       add(dlist, clist[m]) ok
next

nr = 0
for m = 1 to len(dlist)
    if len(dlist[m]) > nr
       nr = len(dlist[m]) ok
next

for n = 1 to len(dlist)
    if len(dlist[n]) = nr
       see dlist[n] + nl ok
next

func getFileSize fp
     c_filestart = 0
     c_fileend = 2
     fseek(fp,0,c_fileend)
     nfilesize = ftell(fp)
     fseek(fp,0,c_filestart)
     return nfilesize

```

Output:

```txt

abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty

```



## Ruby


```ruby
require 'open-uri'
ordered_words = open('http://www.puzzlers.org/pub/wordlists/unixdict.txt', 'r').select do |word|
  word.strip!
  word.chars.sort.join == word
end

grouped = ordered_words.group_by &:size
puts grouped[grouped.keys.max]
```


'''Sample Output'''

```txt

abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```

Local version:

```ruby
words = IO.foreach('unixdict.txt').map(&:chomp).select {|word| word.chars.sort.join == word}
puts words.group_by(&:size).sort_by(&:first).last.last
```



## Run BASIC


```runbasic
a$	= httpget$("http://www.puzzlers.org/pub/wordlists/unixdict.txt")
j	= 1
i   	= instr(a$,chr$(10),j)
while i <> 0
 a1$ 	= mid$(a$,j,i-j)
 for k	= 1 to len(a1$) - 1
  if mid$(a1$,k,1) > mid$(a1$,k+1,1) then goto [noWay]
 next k
 maxL	= max(maxL,len(a1$))
if len(a1$) >= maxL then a2$ = a2$ + a1$ + "||"
[noWay]
j	= i + 1
i   	= instr(a$,chr$(10),j)
wend
n	= 1
while  word$(a2$,n,"||") <> ""
 a3$ = word$(a2$,n,"||")
 if len(a3$) = maxL then print a3$
 n = n + 1
wend
```


```txt
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```



## Rust


```rust
const FILE: &'static str = include_str!("./unixdict.txt");

fn is_ordered(s: &str) -> bool {
    let mut prev = '\x00';
    for c in s.to_lowercase().chars() {
        if c < prev {
            return false;
        }
        prev = c;
    }

    return true;
}

fn find_longest_ordered_words(dict: Vec<&str>) -> Vec<&str> {
    let mut result = Vec::new();
    let mut longest_length = 0;

    for s in dict.into_iter() {
        if is_ordered(&s) {
            let n = s.len();
            if n > longest_length {
                longest_length = n;
                result.truncate(0);
            }
            if n == longest_length {
                result.push(s);
            }
        }
    }

    return result;
}

fn main() {
    let lines = FILE.lines().collect();

    let longest_ordered = find_longest_ordered_words(lines);

    for s in longest_ordered.iter() {
        println!("{}", s.to_string());
    }
}
```



## Scala


```scala
val wordsAll = scala.io.Source.fromURL("http://www.puzzlers.org/pub/wordlists/unixdict.txt").getLines.toSeq

/**
 * Given a sequence of words return a sub-sequence of the
 * words that have characters in sorted order.
 */
def orderedWords( words:Seq[String] ) : Seq[(String)] = {

  def isOrdered( s:String ) : Boolean =
    (s.foldLeft( (true,'@') ){
      case ((false,_),_) => return false
      case ((true,prev),c) => ((prev <= c),c)
    })._1

  wordsAll.filter( isOrdered(_) ).toSeq
}

val ww = orderedWords( wordsAll ).sortBy( -_.length )

println( ww.takeWhile( _.length == ww.head.length ).mkString("\n") )
```

```txt

abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty

```


## Scheme

The following implementation uses a <tt>char>=?</tt> procedure that accepts an arbitrary number of arguments. This is allowed, but not required, by R5RS, and is provided by many Scheme implementations. It has been tested under GNU Guile 1.8.8.


```scheme

(define sorted-words
  (let ((port (open-input-file "unixdict.txt")))
    (let loop ((char (read-char port)) (word '()) (result '(())))
      (cond
       ((eof-object? char)
	(reverse (map (lambda (word) (apply string word)) result)))
       ((eq? #\newline char)
	(loop (read-char port) '()
	      (let ((best-length (length (car result))) (word-length (length word)))
		(cond
		 ((or (< word-length best-length) (not (apply char>=? word))) result)
		 ((> word-length best-length) (list (reverse word)))
		 (else (cons (reverse word) result))))))
       (else (loop (read-char port) (cons char word) result))))))

(map (lambda (x)
       (begin
	 (display x)
	 (newline)))
     sorted-words)

```


Output:

```txt
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func boolean: isOrdered (in string: word) is func
  result
    var boolean: ordered is TRUE;
  local
    var integer: index is 0;
  begin
    for index range 1 to pred(length(word)) do
      if word[index] > word[succ(index)] then
        ordered := FALSE;
      end if;
    end for;
  end func;

const proc: write (in array string: wordList) is func
  local
    var string: word is "";
  begin
    for word range wordList do
      writeln(word);
    end for;
  end func;

const proc: main is func
  local
    var file: dictionary is STD_NULL;
    var string: word is "";
    var integer: length is 0;
    var array string: wordList is 0 times "";
  begin
    dictionary := open("unixdict.txt", "r");
    if dictionary <> STD_NULL then
      readln(dictionary, word);
      while not eof(dictionary) do
        if isOrdered(lower(word)) then
          if length(word) > length then
            length := length(word);
            wordList := [] (word);
          elsif length(word) = length then
            wordList &:= word;
          end if;
        end if;
        readln(dictionary, word);
      end while;
      close(dictionary);
    end if;
    write(wordList);
  end func;
```


Output:

```txt

abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty

```



## Sidef


```ruby
var words = [[]]
var file = %f'unixdict.txt'

file.open_r(\var fh, \var err) ->
    || die "Can't open file #{file}: $#{err}"

fh.each { |line|
    line.trim!
    if (line == line.sort) {
        words[line.length] := [] << line
    }
}

say words[-1].join(' ')
```

```txt
abbott accent accept access accost almost bellow billow biopsy chilly choosy choppy effort floppy glossy knotty
```


## Simula


```simula
BEGIN

   BOOLEAN PROCEDURE ISORDERED(W); TEXT W;
   BEGIN
       BOOLEAN B;
       B := TRUE;
       W.SETPOS(1);
       IF W.MORE THEN
       BEGIN
           CHARACTER CURR, LAST;
           CURR := W.GETCHAR;
           WHILE W.MORE AND B DO
           BEGIN
               LAST := CURR;
               CURR := W.GETCHAR;
               B := LAST <= CURR;
           END;
       END;
       ISORDERED := B;
   END;

   REF (INFILE) INF;
   INTEGER LONGEST;
   TEXT W;

   COMMENT FIND LONGEST LENGTH;
   INF :- NEW INFILE("unixdict.txt");
   INF.OPEN(BLANKS(132));
   WHILE NOT INF.LASTITEM DO
   BEGIN
       W :- COPY(INF.IMAGE).STRIP;
       IF ISORDERED(W) THEN
           IF W.LENGTH > LONGEST THEN
               LONGEST := W.LENGTH;
       INF.INIMAGE;
   END;
   INF.CLOSE;

   COMMENT OUTPUT ORDRERED WORDS OF LONGEST LENGTH;
   INF :- NEW INFILE("unixdict.txt");
   INF.OPEN(BLANKS(132));
   WHILE NOT INF.LASTITEM DO
   BEGIN
       W :- COPY(INF.IMAGE).STRIP;
       IF W.LENGTH = LONGEST AND THEN ISORDERED(W) THEN
       BEGIN
           OUTTEXT(W);
           OUTIMAGE;
       END;
       INF.INIMAGE;
   END;
   INF.CLOSE;

END.

```

```txt

abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty

14 garbage collection(s) in 0.0 seconds.

```



## Smalltalk

```smalltalk
|file dict r t|
file := FileStream open: 'unixdict.txt' mode: FileStream read.
dict := Set new.

"load the whole dict into the set before, 'filter' later"
[ file atEnd ] whileFalse: [
  dict add: (file upTo: Character nl) ].

"find those with the sorted letters, and sort them by length"
r := ((dict
       select: [ :w | (w asOrderedCollection sort) = (w asOrderedCollection) ] )
     asSortedCollection: [:a :b| (a size) > (b size) ] ).

"get those that have length = to the max length, and sort alphabetically"
r := (r select: [:w| (w size) = ((r at: 1) size)]) asSortedCollection.

r do: [:e| e displayNl].
```


Output:

```txt
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```



## SPL


```spl
words = #.split(#.readtext("unixdict.txt","ascii"),#.lf)
max = 0
> i, 1..#.size(words,1)
  word = words[i]
  wordb = #.array(word)
  wordbc = #.size(wordb,1)
  > j, 3..wordbc,2
    << wordb[j]<wordb[j-2]
  <
  >> j!>wordbc|wordbc<max
  ? wordbc>max, result = ""
  ? wordbc>max, max = wordbc
  result += word+#.crlf
<
#.output(result)
```

```txt

abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty

```



## Swift



```swift
import Foundation

guard
    let url =  NSURL(string: "http://www.puzzlers.org/pub/wordlists/unixdict.txt"),
    let input = try? NSString(contentsOfURL: url,encoding: NSUTF8StringEncoding) as String
    else { exit(EXIT_FAILURE) }

let words = input.componentsSeparatedByCharactersInSet(NSCharacterSet.newlineCharacterSet())
let group: ([Int: [String]], String) -> [Int: [String]] = {
    var d = $0; let g = d[$1.characters.count] ?? []
    d[$1.characters.count] = g + [$1]
    return d
}
let ordered: ([String], String) -> [String] = {
    guard String($1.characters.sort()) == $1 else { return $0 }
    return $0 + [$1]
}

let groups = words
    .reduce([String](), combine: ordered)
    .reduce([Int: [String]](), combine: group)

guard
    let maxLength = groups.keys.maxElement(),
    let maxLengthGroup = groups[maxLength]
    else { exit(EXIT_FAILURE) }

maxLengthGroup.forEach { print($0) }

```


```txt

abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty

```



## Tcl


```tcl
package require http

# Pick the ordered words (of maximal length) from a list
proc chooseOrderedWords list {
    set len 0
    foreach word $list {
	# Condition to determine whether a word is ordered; are its characters
	# in sorted order?
	if {$word eq [join [lsort [split $word ""]] ""]} {
	    if {[string length $word] > $len} {
		set len [string length $word]
		set orderedOfMaxLen {}
	    }
	    if {[string length $word] == $len} {
		lappend orderedOfMaxLen $word
	    }
	}
    }
    return $orderedOfMaxLen
}

# Get the dictionary and print the ordered words from it
set t [http::geturl "http://www.puzzlers.org/pub/wordlists/unixdict.txt"]
puts [chooseOrderedWords [http::data $t]]
http::cleanup $t
```

Output:

```txt

abbott accent accept access accost almost bellow billow biopsy chilly choosy choppy effort floppy glossy knotty

```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
SET data = REQUEST ("http://www.puzzlers.org/pub/wordlists/unixdict.txt")
DICT orderdwords CREATE 99999
COMPILE
LOOP word=data
 - "<%" = any token
 SET letters=STRINGS (word,":<%:")
 SET wordsignatur= ALPHA_SORT (letters)
 IF (wordsignatur==letters) THEN
  SET wordlength=LENGTH (word)
  DICT orderdwords ADD/COUNT word,num,cnt,wordlength
 ENDIF
ENDLOOP

DICT orderdwords UNLOAD words,num,cnt,wordlength
SET maxlength=MAX_LENGTH (words)
SET rtable=QUOTES (maxlength)
BUILD R_TABLE maxlength = rtable
SET index=FILTER_INDEX (wordlength,maxlength,-)
SET longestwords=SELECT (words,#index)
PRINT num," ordered words - max length is ",maxlength,":"

LOOP n,w=longestwords
SET n=CONCAT (n,"."), n=CENTER(n,4)
PRINT n,w
ENDLOOP
ENDCOMPILE

```

Output:

```txt

422 ordered words - max length is 6:
 1. abbott
 2. accent
 3. accept
 4. access
 5. accost
 6. almost
 7. bellow
 8. billow
 9. biopsy
10. chilly
11. choosy
12. choppy
13. effort
14. floppy
15. glossy
16. knotty

```



## Ursala


```Ursala
#import std

#show+

main = leql@bh$^ eql|= (ordered lleq)*~ unixdict_dot_txt
```

* <code>leql</code> is a binary predicate testing a pair of lists for less or equal length
* <code>eql</code> tests for equal length
* <code>lleq</code> tests for lexically less or equal relatedness between characters or strings
* <code>ordered</code> takes a binary predicate to a predicate operating on a list, which tests whether every pair of consecutive items is related by the binary predicate
* postfix operator <code>*~</code> turns a predicate into a list filtering function, deleting any item not satisfying it
* postfix operator <code>|=</code> turns an equivalance relational predicate into function that partitions a list into a list of equivalence classes
* postfix operator <code>$^</code> turns a predicate into a function that searches a list for the maximum member with respect to it
* postfix operator <code>@bh</code> makes its operand function apply to the pair of heads of a pair of lists
* loading a file <code>unixdict.txt</code> into a list of strings is implied by using its name as an identifier and supplying it to the compiler in a command line parameter

output:

```txt

abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```



## VBA


```VBA

Public Sub orderedwords(fname As String)
 ' find ordered words in dict file that have the longest word length
 ' fname is the name of the input file
 ' the words are printed in the immediate window
 ' this subroutine uses boolean function IsOrdered

Dim word As String          'word to be tested
Dim l As Integer            'length of word
Dim wordlength As Integer   'current longest word length
Dim orderedword() As String 'dynamic array holding the ordered words with the current longest word length
Dim wordsfound As Integer   'length of the array orderedword()

On Error GoTo NotFound      'catch incorrect/missing file name
Open fname For Input As #1
On Error GoTo 0

'initialize
wordsfound = 0
wordlength = 0

'process file line per line
While Not EOF(1)
  Line Input #1, word
  If IsOrdered(word) Then    'found one, is it equal to or longer than current word length?
    l = Len(word)
    If l >= wordlength Then  'yes, so add to list or start a new list
      If l > wordlength Then 'it's longer, we must start a new list
        wordsfound = 1
        wordlength = l
      Else                   'equal length, increase the list size
        wordsfound = wordsfound + 1
      End If
      'add the word to the list
      ReDim Preserve orderedword(wordsfound)
      orderedword(wordsfound) = word
    End If
  End If
Wend
Close #1

'print the list
Debug.Print "Found"; wordsfound; "ordered words of length"; wordlength
For i = 1 To wordsfound
  Debug.Print orderedword(i)
Next
Exit Sub

NotFound:
  debug.print "Error: Cannot find or open file """ & fname & """!"
End Sub



Public Function IsOrdered(someWord As String) As Boolean
'true if letters in word are in ascending (ascii) sequence

Dim l As Integer         'length of someWord
Dim wordLcase As String  'the word in lower case
Dim ascStart As Integer  'ascii code of first char
Dim asc2 As Integer      'ascii code of next char

wordLcase = LCase(someWord)  'convert to lower case
l = Len(someWord)
IsOrdered = True
If l > 0 Then            'this skips empty string - it is considered ordered...
  ascStart = Asc(Left$(wordLcase, 1))
  For i = 2 To l
    asc2 = Asc(Mid$(wordLcase, i, 1))
    If asc2 < ascStart Then 'failure!
      IsOrdered = False
      Exit Function
    End If
    ascStart = asc2
  Next i
End If
End Function

```


Results:

```txt

OrderedWords("unixdict.txt")
Found 16 ordered words of length 6
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty

```



## Vedit macro language


```vedit
File_Open("unixdict.txt", BROWSE)
#1 = 2                                  // length of longest word found
Repeat (ALL) {
    #2 = EOL_Pos-Cur_Pos                // length of this word
    if (#2 >= #1) {
        #3 = 1                          // flag: is ordered word
        Char(1)
        While (!At_EOL) {
            if (Cur_Char < Cur_Char(-1)) {
                #3 = 0                  // not an ordered word
                break
            }
            Char(1)
        }
        if (#3) {                       // ordered word found
            if (#2 > #1) {              // new longer word found
                #1 = #2
                Reg_Empty(10)           // clear list
            }
            BOL Reg_Copy(10,1,APPEND)   // add word to list
        }
    }
    Line(1,ERRBREAK)                    // next word
}
Buf_Quit(OK)                            // close file
Reg_Type(10)                            // display results
```



```txt
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```



## VBScript

Note:  The input file is in the same folder as the script.

```vb
Set objFSO = CreateObject("Scripting.FileSystemObject")
Set infile = objFSO.OpenTextFile(objFSO.GetParentFolderName(WScript.ScriptFullName) & "\" &_
			"unixdict.txt",1)
list = ""
length = 0

Do Until inFile.AtEndOfStream
	line = infile.ReadLine
	If IsOrdered(line) Then
		If Len(line) > length Then
			length = Len(line)
			list = line & vbCrLf
		ElseIf Len(line) = length Then
			list = list & line & vbCrLf
		End If
	End If
Loop

WScript.StdOut.Write list

Function IsOrdered(word)
	IsOrdered = True
	prev_val = 0
	For i = 1 To Len(word)
		If i = 1 Then
			prev_val = Asc(Mid(word,i,1))
		ElseIf Asc(Mid(word,i,1)) >= prev_val Then
			prev_val = Asc(Mid(word,i,1))
		Else
			IsOrdered = False
			Exit For
		End If
	Next
End Function
```


```txt
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty
```



## zkl

One pass, dictionary stays on disk:

```zkl
var words=L(), sz=0;  // some state
fcn isLex(word){ word.reduce(fcn(p,c){ (p<=c) and c or T(Void.Stop,False) }) }
File("dict.txt").pump(Void,fcn(w){
   w=w.strip();   // get rid of newline
   if(isLex(w)){ n:=w.len();
      if(n>sz){ words.clear(w); sz=n }
      else if(n==sz) words.append(w)
   }
})
println("Num words: %d, all size %d\n".fmt(words.len(),sz));
words.pump(Console.println);
```

Or, reading dictionary into memory:

```zkl
fcn isLex(word){ word.reduce(fcn(p,c){ (p<=c) and c or T(Void.Stop,False) }) }
lwords:=File("dict.txt").readln(*).apply("strip").filter(isLex);
max:=lwords.reduce(fcn(n,w){ w.len()>n and w.len() or n },0);
lwords=lwords.filter(fcn(w,n){ w.len()==n },max);
println("Num words: %d, all size %d\n".fmt(lwords.len(),max));
words.pump(Console.println);
```

```txt

Num words: 16, all size 6
abbott
accent
accept
access
accost
almost
bellow
billow
biopsy
chilly
choosy
choppy
effort
floppy
glossy
knotty

```

