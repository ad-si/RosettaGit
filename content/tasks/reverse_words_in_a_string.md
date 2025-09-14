+++
title = "Reverse words in a string"
description = ""
date = 2019-10-18T18:34:24Z
aliases = []
[extra]
id = 17462
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "aime",
  "algol_68",
  "applescript",
  "applesoft_basic",
  "autohotkey",
  "awk",
  "bacon",
  "batch_file",
  "bbc_basic",
  "bracmat",
  "burlesque",
  "c",
  "clojure",
  "cobol",
  "coffeescript",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "echolisp",
  "elena",
  "elixir",
  "elm",
  "emacs_lisp",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "frink",
  "gambas",
  "gema",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "kotlin",
  "liberty_basic",
  "livecode",
  "livescript",
  "logo",
  "lua",
  "maple",
  "mathematica",
  "matlab",
  "maxscript",
  "miniscript",
  "nial",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "powershell",
  "purebasic",
  "python",
  "r",
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
  "smalltalk",
  "sparkling",
  "swift",
  "tailspin",
  "tcl",
  "txr",
  "unix_shell",
  "vba",
  "vbscript",
  "yabasic",
  "zkl",
]
+++

## Task

Reverse the order of all tokens in each of a number of strings and display the result;   the order of characters within a token should not be modified.


;Example:
<big><big><code>Hey you, Bub! </code></big></big>   would be shown reversed as:   <big><big><code> Bub! you, Hey </code></big></big>


Tokens are any non-space characters separated by spaces (formally, white-space);   the visible punctuation form part of the word within which it is located and should not be modified.

You may assume that there are no significant non-visible characters in the input.   Multiple or superfluous spaces may be compressed into a single space.

Some strings have no tokens, so an empty string   (or one just containing spaces)   would be the result.

'''Display''' the strings in order   (1<sup>st</sup>, 2<sup>nd</sup>, 3<sup>rd</sup>, ···),   and one string per line.

(You can consider the ten strings as ten lines, and the tokens as words.)


;Input data

```txt

             (ten lines within the box)
 line
     ╔════════════════════════════════════════╗
   1 ║  ---------- Ice and Fire ------------  ║
   2 ║                                        ║  ◄─── a blank line here.
   3 ║  fire, in end will world the say Some  ║
   4 ║  ice. in say Some                      ║
   5 ║  desire of tasted I've what From       ║
   6 ║  fire. favor who those with hold I     ║
   7 ║                                        ║  ◄─── a blank line here.
   8 ║  ... elided paragraph last ...         ║
   9 ║                                        ║  ◄─── a blank line here.
  10 ║  Frost Robert -----------------------  ║
     ╚════════════════════════════════════════╝

```


;Cf.
* [[Phrase reversals]]





## Ada


===Simple_Parse===

To Split a string into words, we define a Package "Simple_Parse". This package is also used for the Phrase Reversal Task [[http://rosettacode.org/wiki/Phrase_reversals#Ada]].


```Ada
package Simple_Parse is
   -- a very simplistic parser, useful to split a string into words

   function Next_Word(S: String; Point: in out Positive)
		     return String;
   -- a "word" is a sequence of non-space characters
   -- if S(Point .. S'Last) holds at least one word W
   -- then  Next_Word increments Point by len(W) and returns W.
   -- else  Next_Word sets Point to S'Last+1 and returns ""

end Simple_Parse;
```


The implementation of "Simple_Parse":


```Ada
package body Simple_Parse is

   function Next_Word(S: String; Point: in out Positive) return String is
      Start: Positive := Point;
      Stop: Natural;
   begin
      while Start <= S'Last and then S(Start) = ' ' loop
	 Start := Start + 1;
      end loop; -- now S(Start) is the first non-space,
		-- or Start = S'Last+1 if S is empty or space-only
      Stop := Start-1; -- now S(Start .. Stop) = ""
      while Stop < S'Last and then S(Stop+1) /= ' ' loop
	 Stop := Stop + 1;
      end loop; -- now S(Stop+1) is the first sopace after Start
		-- or Stop = S'Last if there is no such space
      Point := Stop+1;
      return S(Start .. Stop);
   end Next_Word;

end Simple_Parse;
```



### Main Program



```Ada
with Ada.Text_IO, Simple_Parse;

procedure Reverse_Words is

   function Reverse_Words(S: String) return String is
      Cursor: Positive := S'First;
      Word: String := Simple_Parse.Next_Word(S, Cursor);
   begin
      if Word = "" then
         return "";
      else
         return Reverse_Words(S(Cursor .. S'Last)) & " " & Word;
      end if;
   end Reverse_Words;

   use Ada.Text_IO;
begin
   while not End_Of_File loop
      Put_Line(Reverse_Words(Get_Line)); -- poem is read from standard input
   end loop;
end Reverse_Words;
```



## Aime


```aime
integer j;
list l, x;
text s, t;

l = list("---------- Ice and Fire ------------",
         "",
         "fire, in end will world the say Some",
         "ice. in say Some",
         "desire of tasted I've what From",
         "fire. favor who those with hold I",
         "",
         "... elided paragraph last ...",
         "",
         "Frost Robert -----------------------");

for (, t in l) {
    file().b_affix(t).list(x, 0);
    for (j, s in x.reverse) {
        o_space(sign(j));
        o_text(s);
    }
    o_newline();
}
```

```txt
------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
```



## ALGOL 68


```algol68
# returns original phrase with the order of the words reversed #
# a word is a sequence of non-blank characters                 #
PROC reverse word order = ( STRING original phrase )STRING:
     BEGIN
        STRING words reversed := "";
        STRING separator      := "";
        INT    start pos      := LWB original phrase;
        WHILE
            # skip leading spaces #
            WHILE IF start pos <= UPB original phrase
                  THEN original phrase[ start pos ] = " "
                  ELSE FALSE
                  FI
            DO start pos +:= 1
            OD;
            start pos <= UPB original phrase
        DO
            # have another word, find it #
            INT end pos := start pos;
            WHILE IF end pos <= UPB original phrase
                  THEN original phrase[ end pos ] /= " "
                  ELSE FALSE
                  FI
            DO end pos +:= 1
            OD;
            ( original phrase[ start pos : end pos - 1 ] + separator ) +=: words reversed;
            separator := " ";
            start pos := end pos + 1
        OD;
        words reversed
     END # reverse word order # ;

# reverse the words in the lines as per the task #
print( ( reverse word order ( "--------- Ice and Fire ------------ " ), newline ) );
print( ( reverse word order ( "                                    " ), newline ) );
print( ( reverse word order ( "fire, in end will world the say Some" ), newline ) );
print( ( reverse word order ( "ice. in say Some                    " ), newline ) );
print( ( reverse word order ( "desire of tasted I've what From     " ), newline ) );
print( ( reverse word order ( "fire. favor who those with hold I   " ), newline ) );
print( ( reverse word order ( "                                    " ), newline ) );
print( ( reverse word order ( "... elided paragraph last ...       " ), newline ) );
print( ( reverse word order ( "                                    " ), newline ) );
print( ( reverse word order ( "Frost Robert -----------------------" ), newline ) )

```

```txt

------------ Fire and Ice ---------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost

```



## AppleScript



```AppleScript
on run

    unlines(map(reverseWords, |lines|("---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I

... elided paragraph last ...

Frost Robert -----------------------")))

end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- reverseWords :: String -> String
on reverseWords(str)
    unwords(|reverse|(|words|(str)))
end reverseWords

-- |reverse| :: [a] -> [a]
on |reverse|(xs)
    if class of xs is text then
        (reverse of characters of xs) as text
    else
        reverse of xs
    end if
end |reverse|

-- |lines| :: Text -> [Text]
on |lines|(str)
    splitOn(linefeed, str)
end |lines|

-- |words| :: Text -> [Text]
on |words|(str)
    splitOn(space, str)
end |words|

-- ulines :: [Text] -> Text
on unlines(lstLines)
    intercalate(linefeed, lstLines)
end unlines

-- unwords :: [Text] -> Text
on unwords(lstWords)
    intercalate(space, lstWords)
end unwords

-- splitOn :: Text -> Text -> [Text]
on splitOn(strDelim, strMain)
    set {dlm, my text item delimiters} to {my text item delimiters, strDelim}
    set lstParts to text items of strMain
    set my text item delimiters to dlm
    lstParts
end splitOn

-- interCalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    strJoined
end intercalate

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to lambda(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property lambda : f
        end script
    end if
end mReturn

```

```txt
------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
```



## Applesoft BASIC


```ApplesoftBasic
100 DATA"---------- ICE AND FIRE ------------"
110 DATA"                                    "
120 DATA"FIRE, IN END WILL WORLD THE SAY SOME"
130 DATA"ICE. IN SAY SOME                    "
140 DATA"DESIRE OF TASTED I'VE WHAT FROM     "
150 DATA"FIRE. FAVOR WHO THOSE WITH HOLD I   "
160 DATA"                                    "
170 DATA"... ELIDED PARAGRAPH LAST ...       "
180 DATA"                                    "
190 DATA"FROST ROBERT -----------------------"

200 FOR L = 1 TO 10
210     READ T$
220     I = LEN(T$)
240     IF I THEN GOSUB 300 : PRINT W$; : IF I THEN PRINT " "; : GOTO 240
250     PRINT
260 NEXT L
270 END

300 W$ = ""
310 FOR I = I TO 1 STEP -1
320     IF MID$(T$, I, 1) = " " THEN NEXT I : RETURN
330 FOR I = I TO 1 STEP -1
340     C$ = MID$(T$, I, 1)
350     IF C$ <> " " THEN  W$ = C$ + W$ : NEXT I
360 RETURN

```



## AutoHotkey


```AutoHotkey
Data := "
(Join`r`n
---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I

... elided paragraph last ...

Frost Robert -----------------------
)"

Loop, Parse, Data, `n, `r
{
	Loop, Parse, A_LoopField, % A_Space
		Line := A_LoopField " " Line
	Output .= Line "`n", Line := ""
}
MsgBox, % RTrim(Output, "`n")
```



## AWK


```AWK

# syntax: GAWK -f REVERSE_WORDS_IN_A_STRING.AWK
BEGIN {
    text[++i] = "---------- Ice and Fire ------------"
    text[++i] = ""
    text[++i] = "fire, in end will world the say Some"
    text[++i] = "ice. in say Some"
    text[++i] = "desire of tasted I've what From"
    text[++i] = "fire. favor who those with hold I"
    text[++i] = ""
    text[++i] = "... elided paragraph last ..."
    text[++i] = ""
    text[++i] = "Frost Robert -----------------------"
    leng = i
    for (i=1; i<=leng; i++) {
      n = split(text[i],arr," ")
      for (j=n; j>0; j--) {
        printf("%s ",arr[j])
      }
      printf("\n")
    }
    exit(0)
}

```

```txt

------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost

```



## BaCon


```qbasic

PRINT REV$("---------- Ice and Fire ------------")
PRINT
PRINT REV$("fire, in end will world the say Some")
PRINT REV$("ice. in say Some                    ")
PRINT REV$("desire of tasted I've what From     ")
PRINT REV$("fire. favor who those with hold I   ")
PRINT
PRINT REV$("... elided paragraph last ...       ")
PRINT
PRINT REV$("Frost Robert -----------------------")

```

Using the REV$ function which takes a sentence as a delimited string where the items are separated by a delimiter (the space character is the default delimiter).
```txt

------------ Fire and Ice ----------

Some say the world will end in fire,
                    Some say in ice.
     From what I've tasted of desire
   I hold with those who favor fire.

       ... last paragraph elided ...

----------------------- Robert Frost

```



## Batch File


```dos
@echo off

::The Main Thing...
cls
echo.
call :reverse "---------- Ice and Fire ------------"
call :reverse
call :reverse "fire, in end will world the say Some"
call :reverse "ice. in say Some"
call :reverse "desire of tasted I've what From"
call :reverse "fire. favor who those with hold I"
call :reverse
call :reverse "... elided paragraph last ..."
call :reverse
call :reverse "Frost Robert -----------------------"
echo.
pause>nul
exit
::/The Main Thing...

::The Function...
:reverse
set reversed=&set word=&set str=%1
:process
for /f "tokens=1,*" %%A in (%str%) do (
	set str=%%B
	set word=%%A
)
set reversed=%word% %reversed%
set str="%str%"
if not %str%=="" goto process

echo.%reversed%
goto :EOF
::/The Function...
```

```txt

------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost


```



## BBC BASIC

```bbcbasic
      PRINT FNreverse("---------- Ice and Fire ------------")\
      \    'FNreverse("")\
      \    'FNreverse("fire, in end will world the say Some")\
      \    'FNreverse("ice. in say Some")\
      \    'FNreverse("desire of tasted I've what From")\
      \    'FNreverse("fire. favor who those with hold I")\
      \    'FNreverse("")\
      \    'FNreverse("... elided paragraph last ...")\
      \    'FNreverse("")\
      \    'FNreverse("Frost Robert -----------------------")
      END

      DEF FNreverse(s$)
      LOCAL sp%
      sp%=INSTR(s$," ")
      IF sp% THEN =FNreverse(MID$(s$,sp%+1))+" "+LEFT$(s$,sp%-1) ELSE =s$
```


```txt
------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
```




## Bracmat


```bracmat
("---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I

... elided paragraph last ...

Frost Robert -----------------------"
  : ?text
& ( reverse
  =   token tokens reversed
    .   :?tokens
      &   whl
        ' ( @( !arg
             : ?token (" "|\t|\r) ?arg
             )
          & !tokens !token:?tokens
          )
      & !tokens !arg:?tokens
      & :?reversed
      &   whl
        ' ( !tokens:%?token %?tokens
          & " " !token !reversed:?reversed
          )
      & !tokens !reversed:?reversed
      & str$!reversed
  )
& :?output
&   whl
  ' ( @(!text:?line \n ?text)
    & !output reverse$!line \n:?output
    )
& !output reverse$!text:?output
& out$str$!output
);
```

```txt
------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
```



## Burlesque


```blsq

blsq ) "It is not raining"wd<-wd
"raining not is It"
blsq ) "ice. in say some"wd<-wd
"some say in ice."

```



## C


```c
#include <stdio.h>
#include <ctype.h>

void rev_print(char *s, int n)
{
        for (; *s && isspace(*s); s++);
        if (*s) {
                char *e;
                for (e = s; *e && !isspace(*e); e++);
                rev_print(e, 0);
                printf("%.*s%s", (int)(e - s), s, " " + n);
        }
        if (n) putchar('\n');
}

int main(void)
{
        char *s[] = {
                "---------- Ice and Fire ------------",
                "                                    ",
                "fire, in end will world the say Some",
                "ice. in say Some                    ",
                "desire of tasted I've what From     ",
                "fire. favor who those with hold I   ",
                "                                    ",
                "... elided paragraph last ...       ",
                "                                    ",
                "Frost Robert -----------------------",
                0
        };
        int i;
        for (i = 0; s[i]; i++) rev_print(s[i], 1);

        return 0;
}
```

Output is the same as everyone else's.


## C++


```cpp

#include <algorithm>
#include <functional>
#include <string>
#include <iostream>
#include <vector>

//code for a C++11 compliant compiler
template <class BidirectionalIterator, class T>
void block_reverse_cpp11(BidirectionalIterator first, BidirectionalIterator last, T const& separator) {
   std::reverse(first, last);
   auto block_last = first;
   do {
      using std::placeholders::_1;
      auto block_first = std::find_if_not(block_last, last,
         std::bind(std::equal_to<T>(),_1, separator));
      block_last = std::find(block_first, last, separator);
      std::reverse(block_first, block_last);
   } while(block_last != last);
}

//code for a C++03 compliant compiler
template <class BidirectionalIterator, class T>
void block_reverse_cpp03(BidirectionalIterator first, BidirectionalIterator last, T const& separator) {
   std::reverse(first, last);
   BidirectionalIterator block_last = first;
   do {
      BidirectionalIterator block_first = std::find_if(block_last, last,
         std::bind2nd(std::not_equal_to<T>(), separator));
      block_last = std::find(block_first, last, separator);
      std::reverse(block_first, block_last);
   } while(block_last != last);
}

int main() {
   std::string str1[] =
    {
        "---------- Ice and Fire ------------",
        "",
        "fire, in end will world the say Some",
        "ice. in say Some",
        "desire of tasted I've what From",
        "fire. favor who those with hold I",
        "",
        "... elided paragraph last ...",
        "",
        "Frost Robert -----------------------"
    };

   std::for_each(begin(str1), end(str1), [](std::string& s){
      block_reverse_cpp11(begin(s), end(s), ' ');
      std::cout << s << std::endl;
   });

   std::for_each(begin(str1), end(str1), [](std::string& s){
      block_reverse_cpp03(begin(s), end(s), ' ');
      std::cout << s << std::endl;
   });

   return 0;
}

```



### Alternate version


```cpp

#include <string>
#include <iostream>
using namespace std;

string invertString( string s )
{
    string st, tmp;
    for( string::iterator it = s.begin(); it != s.end(); it++ )
    {
        if( *it != 32 ) tmp += *it;
        else
        {
            st = " " + tmp + st;
            tmp.clear();
        }
    }
    return tmp + st;
}

int main( int argc, char* argv[] )
{
    string str[] =
    {
        "---------- Ice and Fire ------------",
        "",
        "fire, in end will world the say Some",
        "ice. in say Some",
        "desire of tasted I've what From",
        "fire. favor who those with hold I",
        "",
        "... elided paragraph last ...",
        "",
        "Frost Robert -----------------------"
    };
    for( int i = 0; i < 10; i++ )
        cout << invertString( str[i] ) << "\n";

    cout << "\n";
    return system( "pause" );
}

```


## C#

```c#
using System;

public class ReverseWordsInString
{
    public static void Main(string[] args)
    {
        string text = @"
            ---------- Ice and Fire ------------

            fire, in end will world the say Some
            ice. in say Some
            desire of tasted I've what From
            fire. favor who those with hold I

            ... elided paragraph last ...

            Frost Robert -----------------------
            ";

        foreach (string line in text.Split(Environment.NewLine)) {
            //Splits on any whitespace, not just spaces
            string[] words = line.Split(default(char[]), StringSplitOptions.RemoveEmptyEntries);
            Array.Reverse(words);
            WriteLine(string.Join(" ", words));
        }
    }
}
```



## Clojure


```clojure

(def poem
  "---------- Ice and Fire ------------

   fire, in end will world the say Some
   ice. in say Some
   desire of tasted I've what From
   fire. favor who those with hold I

   ... elided paragraph last ...

   Frost Robert -----------------------")

(dorun
  (map println (map #(apply str (interpose " " (reverse (re-seq #"[^\s]+" %)))) (clojure.string/split poem #"\n"))))

```

Output is the same as everyone else's.


## COBOL


```COBOL

       program-id. rev-word.
       data division.
       working-storage section.
       1 text-block.
        2 pic x(36) value "---------- Ice and Fire ------------".
        2 pic x(36) value "                                    ".
        2 pic x(36) value "fire, in end will world the say Some".
        2 pic x(36) value "ice. in say Some                    ".
        2 pic x(36) value "desire of tasted I've what From     ".
        2 pic x(36) value "fire. favor who those with hold I   ".
        2 pic x(36) value "                                    ".
        2 pic x(36) value "... elided paragraph last ...       ".
        2 pic x(36) value "                                    ".
        2 pic x(36) value "Frost Robert -----------------------".
       1 redefines text-block.
        2 occurs 10.
         3 text-line pic x(36).
       1 text-word.
        2 wk-len binary pic 9(4).
        2 wk-word pic x(36).
       1 word-stack.
        2 occurs 10.
         3 word-entry.
          4 word-len binary pic 9(4).
          4 word pic x(36).
       1 binary.
        2 i pic 9(4).
        2 pos pic 9(4).
        2 word-stack-ptr pic 9(4).

       procedure division.
           perform varying i from 1 by 1
           until i > 10
               perform push-words
               perform pop-words
           end-perform
           stop run
           .

       push-words.
           move 1 to pos
           move 0 to word-stack-ptr
           perform until pos > 36
               unstring text-line (i) delimited by all space
               into wk-word count in wk-len
               pointer pos
               end-unstring
               add 1 to word-stack-ptr
               move text-word to word-entry (word-stack-ptr)
           end-perform
           .

       pop-words.
           perform varying word-stack-ptr from word-stack-ptr
               by -1
           until word-stack-ptr < 1
               move word-entry (word-stack-ptr) to text-word
               display wk-word (1:wk-len) space with no advancing
           end-perform
           display space
           .
       end program rev-word.

```

```txt

------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost

```



## CoffeeScript

```coffeescript
strReversed = '---------- Ice and Fire ------------\n\n
fire, in end will world the say Some\n
ice. in say Some\n
desire of tasted I\'ve what From\n
fire. favor who those with hold I\n\n
... elided paragraph last ...\n\n
Frost Robert -----------------------'

reverseString = (s) ->
  s.split('\n').map((l) -> l.split(/\s/).reverse().join ' ').join '\n'

console.log reverseString(strReversed)
```

As JavaScript.


## Common Lisp


```lisp
(defun split-and-reverse (str)
  (labels
    ((iter (s lst)
       (let ((s2 (string-trim '(#\space) s)))
         (if s2
             (let ((word-end (position #\space s2)))
               (if (and word-end (< (1+ word-end) (length s2)))
                   (iter (subseq s2 (1+ word-end))
                         (cons (subseq s2 0 word-end) lst))
                   (cons s2 lst)))
               lst))))
  (iter str NIL)))

(defparameter *poem*
  "---------- Ice and Fire ------------

   fire, in end will world the say Some
   ice. in say Some
   desire of tasted I've what From
   fire. favor who those with hold I

   ... elided paragraph last ...

   Frost Robert -----------------------")

(with-input-from-string (s *poem*)
  (loop for line = (read-line s NIL)
        while line
        do (format t "~{~a~#[~:; ~]~}~%" (split-and-reverse line))))
```


Output is the same as everyone else's.


## D


```d
void main() {
    import std.stdio, std.string, std.range, std.algorithm;

    immutable text =
"---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I

... elided paragraph last ...

Frost Robert -----------------------";

    writefln("%(%-(%s %)\n%)",
             text.splitLines.map!(r => r.split.retro));
}
```

The output is the same as the Python entry.


## EchoLisp

Using a here-string input :

```scheme

(define S #<<
---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I

... elided paragraph last ...

Frost Robert -----------------------
>>#)

(for-each writeln
    (for/list ((line (string-split S "\n")))
        (string-join (reverse (string-split line " ")) " ")))

```

```txt

------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost

```



## Elena

ELENA 4.1:

```elena
import extensions;
import system'routines;

public program()
{
    var text := new string[]::("---------- Ice and Fire ------------",
                  "",
                  "fire, in end will world the say Some",
                  "ice. in say Some",
                  "desire of tasted I've what From",
                  "fire. favor who those with hold I",
                  "",
                  "... elided paragraph last ...",
                  "",
                  "Frost Robert -----------------------");

    text.forEach:(line)
    {
        line.splitBy:" ".sequenceReverse().forEach:(word)
        {
            console.print(word," ")
        };
        console.writeLine()
    }
}
```



## Elixir


```elixir
defmodule RC do
  def reverse_words(txt) do
    txt |> String.split("\n")       # split lines
        |> Enum.map(&(              # in each line
             &1 |> String.split       # split words
                |> Enum.reverse       # reverse words
                |> Enum.join(" ")))   # rejoin words
        |> Enum.join("\n")          # rejoin lines
  end
end
```

Usage:

```elixir
txt = """
---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I

... elided paragraph last ...

Frost Robert -----------------------
"""

IO.puts RC.reverse_words(txt)
```



## Elm



```elm

reversedPoem =
  String.trim """
    ---------- Ice and Fire ------------

    fire, in end will world the say Some
    ice. in say Some
    desire of tasted I've what From
    fire. favor who those with hold I

    ... elided paragraph last ...

    Frost Robert -----------------------
    """

reverseWords string =
  string |> String.words |> List.reverse |> String.join " "

reverseLinesWords string =
  string |> String.lines |> List.map reverseWords |> String.join "\n"

poem =
  reverseLinesWords reversedPoem

```



## Emacs Lisp



```Emacs Lisp

(defun reverse-words (line)
  (insert
   (format "%s\n"
           (mapconcat 'identity (reverse (split-string line)) " "))))

(defun reverse-lines (lines)
  (mapcar 'reverse-words lines))

(reverse-lines
 '("---------- Ice and Fire ------------"
   ""
   "fire, in end will world the say Some"
   "ice. in say Some"
   "desire of tasted I've what From"
   "fire. favor who those with hold I"
   ""
   "... elided paragraph last ..."
   ""
   "Frost Robert ----------------------- "))

```

'''Output''':

```txt

------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost

```



## Factor


```factor
USING: io sequences splitting ;
IN: rosetta-code.reverse-words

"---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I

... elided paragraph last ...

Frost Robert -----------------------"

"\n" split [ " " split reverse " " join ] map [ print ] each
```

```txt

------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost

```



## Forth

The method shown is based on submissions in comp.lang.forth. Comments have been added for those less familiar with reading Forth. This example makes use of the Forth interpreter/compiler internals by calling PARSE-NAME which is a Forth 2012 word that parses the input stream, ignores leading spaces and delimits at the space character. This method makes clever use of the data stack. Strings are not copied in memory by rather pointers are collected on the stack as PARSE-NAME processes a sentence. This of course leaves the strings in the reversed order so they are simply printed off the data stack.
<lang>create buf 1000 chars allot                    \ string buffer
buf value pp                                   \ pp points to buffer address

: tr        ( caddr u -- )
            dup >r pp swap cmove               \ move string into buffer
            r> pp + to pp  ;                   \ advance pointer by u bytes

: collect   ( -- addr len .. addr[n] len[n])   \ words deposit on data stack
            begin
              parse-name dup                   \ parse input stream, dup the len
            while                              \ while  stack <> 0
              tuck pp >r tr r>  swap
            repeat
            2drop ;                            \ clean up stack

: reverse   ( -- )
            buf to pp                          \ initialize pointer to buffer address
            collect
            depth 2/ 0 ?do  type space  loop   \ type the strings with a trailing space
            cr ;                               \ final new line

reverse ---------- Ice and Fire ------------
reverse
reverse fire, in end will world the say Some
reverse ice. in say Some
reverse desire of tasted I've what From
reverse fire. favor who those with hold I
reverse
reverse ... elided paragraph last ...
reverse
reverse Frost Robert -----------------------
```


'''Output'''
Interpreted above code at the Forth console

```txt
Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
 ok
```



## Fortran

Compiled using G95 on x86 system running Puppy Linux.
Fortran syntax is mostly Fortran 77.


```fortran

 character*40 words
 character*40 reversed
 logical inblank
 ierr=0
 read (5,fmt="(a)",iostat=ierr)words
 do while (ierr.eq.0)
 inblank=.true.
 ipos=1
 do i=40,1,-1
   if(words(i:i).ne.' '.and.inblank) then
     last=i
     inblank=.false.
     end if
    if(.not.inblank.and.words(i:i).eq.' ') then
      reversed(ipos:ipos+last-i)=words(i+1:last)
      ipos=ipos+last-i+1
      inblank=.true.
      end if
     if(.not.inblank.and.i.eq.1) then
       reversed(ipos:ipos+last-1)=words(1:last)
       ipos=ipos+last
       end if
   end do
 print *,words,'=> ',reversed(1:ipos-1)
 read (5,fmt="(a)",iostat=ierr)words
 end do
 end


```


Output from comand: <b>cat frostPoem.txt | reverse</b><p>
where file frostPoem.txt contains the input text.


```txt


 ---------- Ice and Fire -----------     => ----------- Fire and Ice ----------
                                         =>
 fire, in end will world the say Some    => Some say the world will end in fire,
 ice. in say Some                        => Some say in ice.
 desire of tasted I've what From         => From what I've tasted of desire
 fire. favor who those with hold I       => I hold with those who favor fire.
                                         =>
 ... elided paragraph last ...           => ... last paragraph elided ...
                                         =>
 Frost Robert -----------------------    => ----------------------- Robert Frost


```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Sub split (s As String, sepList As String, result() As String, removeEmpty As Boolean = False)
  If s = "" OrElse sepList = "" Then
     Redim result(0)
     result(0) = s
     Return
  End If
  Dim As Integer i, j, count = 0, empty = 0, length
  Dim As Integer position(Len(s) + 1)
  position(0) = 0

  For i = 0 To len(s) - 1
    For j = 0 to Len(sepList) - 1
      If s[i] = sepList[j] Then
        count += 1
        position(count) = i + 1
      End If
    Next j
  Next i

  Redim result(count)
  If count  = 0 Then
    result(0) = s
    Return
  End If

  position(count + 1) = len(s) + 1

  For i = 1 To count + 1
    length = position(i) - position(i - 1) - 1
    result(i - 1 - empty) = Mid(s, position(i - 1) + 1, length)
    If removeEmpty Andalso CBool(length = 0) Then empty += 1
  Next

  If empty > 0 Then Redim Preserve result(count - empty)
End Sub

Dim s As String = "Hey you, Bub!"
Dim a() As String
split(s, " ", a(), true)
Dim reversed As String = ""
For i As Integer = UBound(a) To LBound(a) Step -1
  reversed += a(i)
  If i > LBound(a) Then reversed += " "
Next

Print "Original String = "; s
Print "Reversed String = "; reversed
Print
Print "Press any key to quit"
Sleep
```


```txt

Original String = Hey you, Bub!
Reversed String = Bub! you, Hey

```



## Frink


```frink

lines=split["\n",
"""---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I

 .. elided paragraph last ...

Frost Robert -----------------------"""]

for line = lines
    println[join[" ", reverse[split[%r/\s+/, line]]]]

```



## Gema


```gema>\L<G> <U
=@{$2} $1
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=c81c1bbf94e856035fd382015d208272 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sString As New String[10]                                     'Array for the input text
Dim sLine As New String[]                                         'Array of each word in a line
Dim siCount0, siCount1 As Short                                   'Counters
Dim sOutput, sReverse, sTemp As String                            'Strings

sString[0] = "---------- Ice And Fire ------------"               'Input text
sString[1] = "                                    "
sString[2] = "fire, in end will world the say Some"
sString[3] = "ice. in say Some                    "
sString[4] = "desire of tasted I've what From     "
sString[5] = "fire. favor who those with hold I   "
sString[6] = "                                    "
sString[7] = "... elided paragraph last ...       "
sString[8] = "                                    "
sString[9] = "Frost Robert -----------------------"

For siCount0 = 0 To 9                                             'To work through each line of input text
  If Trim(sString[siCount0]) = "" Then sString[siCount0] = " "    'If the line is all spaces then make it 1 space

  For Each sTemp In Split(Trim(sString[siCount0]), " ")           'Split the trimmed line by spaces
    sLine.Add(sTemp)                                              'Add each word to the sLine array
  Next

  For siCount1 = sLine.max DownTo 0                               'Loop from the last in the sLine array to 0
    sReverse &= sLine[siCount1] & " "                             'Fill sReverse with words reversed, adding a space
  Next

  sOutput &= Trim(sReverse) & gb.NewLine                          'Add the reversed words to sOutput and add a newline
  sReverse = ""                                                   'Clear sReverse
  sLine.Clear                                                     'Clear sLine array
Next

Print sOutput                                                     'Print the output

End
```

Output:

```txt

------------ Fire And Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost

```



## Go


```go
package main

import (
    "fmt"
    "strings"
)

// a number of strings
var n = []string{
    "---------- Ice and Fire ------------",
    "                                    ",
    "fire, in end will world the say Some",
    "ice. in say Some                    ",
    "desire of tasted I've what From     ",
    "fire. favor who those with hold I   ",
    "                                    ",
    "... elided paragraph last ...       ",
    "                                    ",
    "Frost Robert -----------------------",
}

func main() {
    for i, s := range n {
        t := strings.Fields(s) // tokenize
        // reverse
        last := len(t) - 1
        for j, k := range t[:len(t)/2] {
            t[j], t[last-j] = t[last-j], k
        }
        n[i] = strings.Join(t, " ")
    }
    // display result
    for _, t := range n {
        fmt.Println(t)
    }
}
```

```txt

------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost

```



## Groovy


```groovy
def text = new StringBuilder()
    .append('---------- Ice and Fire ------------\n')
    .append('                                    \n')
    .append('fire, in end will world the say Some\n')
    .append('ice. in say Some                    \n')
    .append('desire of tasted I\'ve what From     \n')
    .append('fire. favor who those with hold I   \n')
    .append('                                    \n')
    .append('... elided paragraph last ...       \n')
    .append('                                    \n')
    .append('Frost Robert -----------------------\n').toString()

text.eachLine { line ->
    println "$line   -->   ${line.split(' ').reverse().join(' ')}"
}
```

```txt
---------- Ice and Fire ------------   -->   ------------ Fire and Ice ----------
                                       -->
fire, in end will world the say Some   -->   Some say the world will end in fire,
ice. in say Some                       -->   Some say in ice.
desire of tasted I've what From        -->   From what I've tasted of desire
fire. favor who those with hold I      -->   I hold with those who favor fire.
                                       -->
... elided paragraph last ...          -->   ... last paragraph elided ...
                                       -->
Frost Robert -----------------------   -->   ----------------------- Robert Frost
```



## Haskell


```Haskell

revstr :: String -> String
revstr = unwords . reverse . words -- point-free style
--equivalent:
--revstr s = unwords (reverse (words s))

revtext :: String -> String
revtext = unlines . map revstr . lines -- applies revstr to each line independently

test = revtext "---------- Ice and Fire ------------\n\
        \\n\
        \fire, in end will world the say Some\n\
        \ice. in say Some\n\
        \desire of tasted I've what From\n\
        \fire. favor who those with hold I\n\
        \\n\
        \... elided paragraph last ...\n\
        \\n\
        \Frost Robert -----------------------\n" --multiline string notation requires \ at end and start of lines, and \n to be manually input

```

unwords, reverse, words, unlines, map and lines are built-in functions, all available at GHC's Prelude.
For better visualization, use "putStr test"

=={{header|Icon}} and {{header|Unicon}}==

Works in both languages:

```unicon
procedure main()
    every write(rWords(&input))
end

procedure rWords(f)
    every !f ? {
        every (s := "") := genWords() || s
        suspend s
        }
end

procedure genWords()
    while w := 1(tab(upto(" \t")),tab(many(" \t"))) || " " do suspend w
end
```


{{out}} for test file:

```txt

->rw <rw.in
------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
->

```



## J


Treated interactively:


```J
   ([:;@|.[:<;.1 ' ',]);._2]0 :0
---------- Ice and Fire ------------

  fire, in end will world the say Some
  ice. in say Some
  desire of tasted I've what From
  fire. favor who those with hold I

   ... elided paragraph last ...

  Frost Robert -----------------------
)
 ------------ Fire and Ice ----------

 Some say the world will end in fire,
 Some say in ice.
 From what I've tasted of desire
 I hold with those who favor fire.

 ... last paragraph elided ...

 ----------------------- Robert Frost

```


The verb phrase <code>( [:   ; @ |.   [: < ;. 1  ' ' , ])</code> reverses words in a string. The rest of the implementation has to do with defining the block of text we are working on, and applying this verb phrase to each line of that text.


## Java


```java
public class ReverseWords {

    static final String[] lines = {
        " ----------- Ice and Fire ----------- ",
        "                                      ",
        " fire, in end will world the say Some ",
        " ice. in say Some                     ",
        " desire of tasted I've what From      ",
        " fire. favor who those with hold I    ",
        "                                      ",
        " ... elided paragraph last ...        ",
        " Frost Robert ----------------------- "};

    public static void main(String[] args) {
        for (String line : lines) {
            String[] words = line.split("\\s");
            for (int i = words.length - 1; i >= 0; i--)
                System.out.printf("%s ", words[i]);
            System.out.println();
        }
    }
}
```

```java
package string;

import static java.util.Arrays.stream;

public interface ReverseWords {
  public static final String[] LINES = {
    " ----------- Ice and Fire ----------- ",
    "                                      ",
    " fire, in end will world the say Some ",
    " ice. in say Some                     ",
    " desire of tasted I've what From      ",
    " fire. favor who those with hold I    ",
    "                                      ",
    " ... elided paragraph last ...        ",
    " Frost Robert ----------------------- "
  };

  public static String[] reverseWords(String[] lines) {
    return stream(lines)
      .parallel()
      .map(l -> l.split("\\s"))
      .map(ws -> stream(ws)
        .parallel()
        .map(w -> " " + w)
        .reduce(
          "",
          (w1, w2) -> w2 + w1
        )
      )
      .toArray(String[]::new)
    ;
  }

  public static void main(String... arguments) {
    stream(reverseWords(LINES))
      .forEach(System.out::println)
    ;
  }
}
```


```txt
----------- Fire and Ice -----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
```



## JavaScript


```javascript
var strReversed =
"---------- Ice and Fire ------------\n\
\n\
fire, in end will world the say Some\n\
ice. in say Some\n\
desire of tasted I've what From\n\
fire. favor who those with hold I\n\
\n\
... elided paragraph last ...\n\
\n\
Frost Robert -----------------------";

function reverseString(s) {
  return s.split('\n').map(
    function (line) {
      return line.split(/\s/).reverse().join(' ');
    }
  ).join('\n');
}

console.log(
  reverseString(strReversed)
);
```


Output:

```txt
------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
```



## jq


```jq
split("[ \t\n\r]+") | reverse | join(" ")
```

This solution requires a version of jq with regex support for split.

The following example assumes the above line is in a file named reverse_words.jq and that the input text is in a file named IceAndFire.txt. The -r option instructs jq to read the input file as strings, line by line.
```sh
$ jq -R -r -M -f reverse_words.jq IceAndFire.txt
------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
```



## Jsish

From Javascript entry.

```javascript
var strReversed =
"---------- Ice and Fire ------------\n
fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I
\n... elided paragraph last ...\n
Frost Robert -----------------------";

function reverseString(s) {
    return s.split('\n').map(
      function (line) {
          return line.split().reverse().join(' ');
      }
    ).join('\n');
}

;reverseString('Hey you, Bub!');
;strReversed;
;reverseString(strReversed);

/*
=!EXPECTSTART!=
reverseString('Hey you, Bub!') ==> Bub! you, Hey
strReversed ==> ---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I

... elided paragraph last ...

Frost Robert -----------------------
reverseString(strReversed) ==> ------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
=!EXPECTEND!=
*/
```


```txt

prompt$ jsish -u reverseWords.jsi
[PASS] reverseWords.jsi
```



## Julia


```Julia
revstring (str) = join(reverse(split(str, " ")), " ")
```
```txt
julia> revstring("Hey you, Bub!")
"Bub! you, Hey"

julia> s = IOBuffer(
"---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I

... elided paragraph last ...

Frost Robert -----------------------")

julia>  for line in eachline(s)
          println(revstring(chomp(line)))
        end
------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
```



## Kotlin


```scala
// version 1.0.6

fun reversedWords(s: String) = s.split(" ").reversed().joinToString(" ")

fun main(args: Array<String>) {
    val s = "Hey you, Bub!"
    println(reversedWords(s))
    println()
    val sl = listOf(
        " ---------- Ice and Fire ------------ ",
        "                                      ",
        " fire, in end will world the say Some ",
        " ice. in say Some                     ",
        " desire of tasted I've what From      ",
        " fire. favor who those with hold I    ",
        "                                      ",
        " ... elided paragraph last ...        ",
        "                                      ",
        " Frost Robert ----------------------- "
    )
    sl.forEach { println(reversedWords(it).trimStart()) }
}
```


```txt

Bub! you, Hey

------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost

```



## Liberty BASIC


```lb

for i = 1 to 10
    read string$
    print reverse$(string$)
next
end

function reverse$(string$)
    token$="*"
    while token$<>""
        i=i+1
        token$ = word$(string$, i)
        output$=token$+" "+output$
    wend
    reverse$ = trim$(output$)
end function

data "---------- Ice and Fire ------------"
data ""
data "fire, in end will world the say Some"
data "ice. in say Some"
data "desire of tasted I've what From"
data "fire. favor who those with hold I"
data ""
data "... elided paragraph last ..."
data ""
data "Frost Robert -----------------------"

```

```txt
------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
```



## LiveCode

The input text has been entered into the contents of a text field called "Fieldtxt", add a button and put the following in its mouseUp

```LiveCode
repeat for each line txtln in fld "Fieldtxt"
    repeat with i = the number of words of txtln down to 1
        put word i of txtln & space after txtrev
    end repeat
    put cr after txtrev  -- preserve line
end repeat
put txtrev
```




## LiveScript


```livescript

poem =
    """
    ---------- Ice and Fire ------------

    fire, in end will world the say Some
    ice. in say Some
    desire of tasted I've what From
    fire. favor who those with hold I

    ... elided paragraph last ...

    Frost Robert -----------------------
    """

reverse-words  = (.split ' ') >> (.reverse!) >> (.join ' ')
reverse-string = (.split '\n') >> (.map reverse-words) >> (.join '\n')
reverse-string poem

```



## Logo

This version just reads the words from standard input.


```logo
do.until [
  make "line readlist
  print reverse :line
] [word? :line]
bye
```


Given this input:
```txt
---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I

... elided paragraph last ...

Frost Robert -----------------------
```

it produces this output:

```txt
------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
```



## Lua


See below for original entry and the input string under variable 's'. Here is a significantly shorter program.


```lua

local lines = {}
for line in (s .. "\n"):gmatch("(.-)\n") do
	local this = {}
	for word in line:gmatch("%S+") do
		table.insert(this, 1, word)
	end
	lines[#lines + 1] = table.concat(this, " ")
end
print(table.concat(lines, "\n"))

```




Original response:

(''Note:'' The Wiki's syntax highlighting for Lua does not highlight the following valid string literal correctly, so the listing is split in two parts.)

    s = [[---------- Ice and Fire ------------

    fire, in end will world the say Some
    ice. in say Some
    desire of tasted I've what From
    fire. favor who those with hold I

    ... elided paragraph last ...

    Frost Robert -----------------------
    ]]


```lua
function table.reverse(a)
    local res = {}
    for i = #a, 1, -1 do
        res[#res+1] = a[i]
    end
    return res
end

function splittokens(s)
    local res = {}
    for w in s:gmatch("%S+") do
        res[#res+1] = w
    end
    return res
end

for line, nl in s:gmatch("([^\n]-)(\n)") do
    print(table.concat(table.reverse(splittokens(line)), ' '))
end
```


''Note:'' With the technique used here for splitting <code>s</code> into lines (not part of the task) the last line will be gobbled up if it does not end with a newline.


## Maple


```Maple
while (true) do
	input := readline("input.txt"):
	if input = 0 then break: fi:
	input := StringTools:-Trim(input): # remove leading/trailing space
	input := StringTools:-Join(ListTools:-Reverse(StringTools:-Split(input, " "))," "):
	printf("%s\n", input):
od:
```

```txt
------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
```




## Mathematica


```Mathematica
poem = "---------- Ice and Fire ------------

     fire, in end will world the say Some
     ice. in say Some
     desire of tasted I've what From
     fire. favor who those with hold I

     ... elided paragraph last ...

     Frost Robert -----------------------";
lines = StringSplit[poem, "\n"];
wordArray = StringSplit[#] &   @ lines ;
reversedWordArray = Reverse[#] & /@ wordArray ;
linesWithReversedWords =
  StringJoin[Riffle[#, " "]] & /@ reversedWordArray;
finaloutput = StringJoin[Riffle[#, "\n"]] &  @ linesWithReversedWords
```

```txt
------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
```



## MATLAB


```MATLAB
function testReverseWords
    testStr = {'---------- Ice and Fire ------------' ; ...
        ''                                            ; ...
        'fire, in end will world the say Some'        ; ...
        'ice. in say Some'                            ; ...
        'desire of tasted I''ve what From'            ; ...
        'fire. favor who those with hold I'           ; ...
        ''                                            ; ...
        '... elided paragraph last ...'               ; ...
        ''                                            ; ...
        'Frost Robert -----------------------'        };
    for k = 1:length(testStr)
        fprintf('%s\n', reverseWords(testStr{k}))
    end
end

function strOut = reverseWords(strIn)
    strOut = strtrim(strIn);
    if ~isempty(strOut)
        % Could use strsplit() instead of textscan() in R2013a or later
        words = textscan(strOut, '%s');
        words = words{1};
        strOut = strtrim(sprintf('%s ', words{end:-1:1}));
    end
end
```

```txt
------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
```



## MAXScript


```maxscript

-- MAXScript : Reverse words in a string : N.H. 2019
--
(
text = stringstream "---------- Ice and Fire ------------\n\nfire, in end will world the say Some\nice. in say Some\ndesire of tasted I've what From\nfire. favor who those with hold I\n\n... elided paragraph last ...\n\nFrost Robert -----------------------\n"
clearListener()
seek text 0
while eof text == false do
(
nextLine = (readLine text)
if nextLine == "" then
(
print ""
continue
) -- end of if
revLine = ""
eachWord = filterString nextLine " "
for k = eachWord.count to 1 by -1 do
(
revLine = revLine + eachWord[k]
-- Only add space between words not at the end of line
if k != 1 then revLine = revLine + " "
) -- end of for k
print revLine
) -- end of while eof
)

```

Output to MAXScript Listener:

```txt
"------------ Fire and Ice ----------"
""
"Some say the world will end in fire,"
"Some say in ice."
"From what I've tasted of desire"
"I hold with those who favor fire."
""
"... last paragraph elided ..."
""
"----------------------- Robert Frost"
```



## MiniScript


```MiniScript
lines = ["
### ====================================
",
    "|  ---------- Ice and Fire ------------  |",
    "|                                        |",
    "|  fire, in end will world the say Some  |",
    "|  ice. in say Some                      |",
    "|  desire of tasted I've what From       |",
    "|  fire. favor who those with hold I     |",
    "|                                        |",
    "|  ... elided paragraph last ...         |",
    "|                                        |",
    "|  Frost Robert -----------------------  |",
    "
### ====================================
"]

for line in lines
    oldLine = line.split
    newLine = []
    while oldLine
// the line below line retains the outer box format
        newLine.push oldLine.pop
// alternate format, replace above line with below two lines below to strip all superfluous spaces
//	word = oldLine.pop
//	if word != "" then newLine.push word
    end while
    print newLine.join
end for
```

```txt


### ====================================

|  ------------ Fire and Ice ----------  |
|                                        |
|  Some say the world will end in fire,  |
|                      Some say in ice.  |
|       From what I've tasted of desire  |
|     I hold with those who favor fire.  |
|                                        |
|         ... last paragraph elided ...  |
|                                        |
|  ----------------------- Robert Frost  |

### ====================================


```


=={{header|Modula-2}}==
```modula2

MODULE ReverseWords;

FROM STextIO IMPORT
  WriteString, WriteLn;
FROM Strings IMPORT
  Assign, Concat, Append;

CONST
  NL = CHR(10);
  Sp = ' ';
  Txt = "---------- Ice and Fire -----------" + NL +
    NL +
    "fire, in end will world the say Some" + NL +
    "ice. in say Some" + NL +
    "desire of tasted I've what From" + NL +
    "fire. favor who those with hold I" + NL +
    NL +
    "... elided paragraph last ..." + NL +
    NL +
    "Frost Robert -----------------------" + NL;

TYPE
  String400 = ARRAY [0 .. 399] OF CHAR;

PROCEDURE AddWord(Source: ARRAY OF CHAR; VAR INOUT Destination: ARRAY OF CHAR);
VAR
  R: String400;
BEGIN
  Concat(Source, Sp, R);
  Append(Destination, R);
  Assign(R, Destination);
END AddWord;

VAR
  I: CARDINAL;
  SingleWord, CurrentLine: String400;
  C: CHAR;

BEGIN
  SingleWord := "";
  CurrentLine := "";
  FOR I := 0 TO HIGH(Txt) DO
    C := Txt[I];
    CASE C OF
      Sp:
        AddWord(SingleWord, CurrentLine);
        SingleWord := ""; |
      NL:
        AddWord(SingleWord, CurrentLine);
        WriteString(CurrentLine);
        WriteLn;
        SingleWord := "";
        CurrentLine := ""; |
    ELSE
      Append(C, SingleWord);
    END;
  END;
END ReverseWords.

```



## Nial


```Nial

# Define a function to convert a list of strings to a single string.
join is rest link (' ' eachboth link)

iterate (write join reverse (' ' string_split)) \
  \
  \
  '------------ Eldorado ----------' \
  '' \
  '... here omitted lines ...' \
  '' \
  'Mountains the "Over' \
  'Moon, the Of' \
  'Shadow, the of Valley the Down' \
  'ride," boldly Ride,' \
  'replied,--- shade The' \
  'Eldorado!" for seek you "If' \
  '' \
  'Poe Edgar -----------------------'

```


```txt

---------- Eldorado ------------

... lines omitted here ...

"Over the Mountains
Of the Moon,
Down the Valley of the Shadow,
Ride, boldly ride,"
The shade replied,---
"If you seek for Eldorado!"

----------------------- Edgar Poe

```



## Nim


```nim
import strutils

let text = """---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I

... elided paragraph last ...

Frost Robert -----------------------"""

proc reversed*[T](a: openArray[T], first, last: int): seq[T] =
  result = newSeq[T](last - first + 1)
  var x = first
  var y = last
  while x <= last:
    result[x] = a[y]
    dec(y)
    inc(x)

proc reversed*[T](a: openArray[T]): seq[T] =
  reversed(a, 0, a.high)

for line in text.splitLines():
  echo line.split(' ').reversed().join(" ")
```

```txt
------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
```



## Objeck


```objeck
use Collection;

class Reverselines {
  function : Main(args : String[]) ~ Nil {
    lines := List->New();
    lines->AddBack("---------- Ice and Fire ------------");
    lines->AddBack("");
    lines->AddBack("fire, in end will world the say Some");
    lines->AddBack("ice. in say Some");
    lines->AddBack("desire of tasted I've what From");
    lines->AddBack("fire. favor who those with hold I");
    lines->AddBack("");
    lines->AddBack("... elided paragraph last ...");
    lines->AddBack("");
    lines->AddBack("Frost Robert -----------------------");

    lines->Rewind();
    each(i : lines) {
      words := lines->Get()->As(String)->Split(" ");
      if(words <> Nil) {
        for(j := words->Size() - 1; j > -1; j-=1;) {
          IO.Console->Print(words[j])->Print(" ");
        };
      };
      IO.Console->PrintLine();
      lines->Next();
    };
  }
}
```


```txt
------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
```



## OCaml


```ocaml
#load "str.cma"
let input = ["---------- Ice and Fire ------------";
        "";
        "fire, in end will world the say Some";
        "ice. in say Some";
        "desire of tasted I've what From";
        "fire. favor who those with hold I";
        "";
        "... elided paragraph last ...";
        "";
        "Frost Robert -----------------------"];;

let splitted = List.map (Str.split (Str.regexp " ")) input in
let reversed = List.map List.rev splitted in
let final = List.map (String.concat " ") reversed in
List.iter print_endline final;;
```

Sample usage

```txt
$ ocaml reverse.ml
------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost

```



## Oforth



```Oforth
: revWords(s)
   s words reverse unwords ;

: reverseWords
   "---------- Ice and Fire ------------" revWords println
   "                                    " revWords println
   "fire, in end will world the say Some" revWords println
   "ice. in say Some                    " revWords println
   "desire of tasted I've what From     " revWords println
   "fire. favor who those with hold I   " revWords println
   "                                    " revWords println
   "... elided paragraph last ...       " revWords println
   "                                    " revWords println
   "Frost Robert -----------------------" revWords println ;
```


```txt

>reverseWords
------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
ok

```



## Pascal

Free Pascal 3.0.0

```pascal
program Reverse_words(Output);
{$H+}

const
  nl = chr(10); // Linefeed
  sp = chr(32); // Space
  TXT =
  '---------- Ice and Fire -----------'+nl+
  nl+
  'fire, in end will world the say Some'+nl+
  'ice. in say Some'+nl+
  'desire of tasted I''ve what From'+nl+
  'fire. favor who those with hold I'+nl+
  nl+
  '... elided paragraph last ...'+nl+
  nl+
  'Frost Robert -----------------------'+nl;

var
  I : integer;
  ew, lw : ansistring;
  c : char;

function addW : ansistring;
var r : ansistring = '';
begin
  r := ew + sp + lw;
  ew := '';
  addW := r
end;

begin
  ew := '';
  lw := '';

  for I := 1 to strlen(TXT) do
  begin
    c := TXT[I];
    case c of
      sp : lw := addW;
      nl : begin writeln(addW); lw := '' end;
      else ew := ew + c
    end;
  end;
  readln;
end.
```

```txt
----------- Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
```



## Perl


```perl
print join(" ", reverse split), "\n" for <DATA>;
__DATA__
---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I

... elided paragraph last ...

Frost Robert -----------------------

```



## Perl 6

We'll read input from stdin

```perl6
say ~.words.reverse for lines
```

```txt
------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
```



## Phix


```Phix
constant test="""
---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I

... elided paragraph last ...

Frost Robert -----------------------
"""
sequence lines = split(test,'\n')
for i=1 to length(lines) do
    lines[i] = join(reverse(split(lines[i])))
end for
puts(1,join(lines,"\n"))
```

```txt

------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost

```



## PicoLisp


```PicoLisp

(in "FireIce.txt"
 (until (eof)
  (prinl (glue " " (flip (split (line) " "))))))

```

Same as anybody else.


## PL/I


```PL/I
rev: procedure options (main);          /* 5 May 2014 */
   declare (s, reverse) character (50) varying;
   declare (i, j) fixed binary;
   declare in file;

   open file (in) title ('/REV-WRD.DAT,type(text),recsize(5> Nil) {
        for(j := words->Size() - 1; j > -1; j-=1;) {
          IO.Console->Print(words[j])->Print(" ");
        };
      };
      IO.Console->PrintLine();
      lines->Next();
    };
  }
}0)');

   do j = 1 to 10;
      get file (in) edit (s) (L);
      put skip list (trim(s));

      reverse = '';

      do while (length(s) > 0);
         s = trim(s);
         i = index(s, ' ');
         if i = 0 then
            if s ^= '' then i = length(s)+1;
         if i > 0 then reverse = substr(s, 1, i-1) || ' ' || reverse;
         if length(s) = i then s = ''; else s = substr(s, i);
      end;
      put edit ('---> ', reverse) (col(40), 2 A);
   end;
end rev;
```

```txt

---------- Ice and Fire ------------   ---> ------------ Fire and Ice ----------
                                       --->
fire, in end will world the say Some   ---> Some say the world will end in fire,
ice. in say Some                       ---> Some say in ice.
desire of tasted I've what From        ---> From what I've tasted of desire
fire. favor who those with hold I      ---> I hold with those who favor fire.
                                       --->
... elided paragraph last ...          ---> ... last paragraph elided ...
                                       --->
Frost Robert -----------------------   ---> ----------------------- Robert Frost

```



## PowerShell

```PowerShell

Function Reverse-Words($lines) {
    $lines | foreach {
        $array = $PSItem.Split(' ')
        $array[($array.Count-1)..0] -join ' '
    }
}

$lines =
"---------- Ice and Fire ------------",
"",
"fire, in end will world the say Some",
"ice. in say Some",
"desire of tasted I've what From",
"fire. favor who those with hold I",
"",
"... elided paragraph last ...",
"",
"Frost Robert -----------------------"

Reverse-Words($lines)

```


'''output''' :

```txt

------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost

```



## PureBasic


```purebasic
a$ =    "---------- Ice and Fire ------------" +#CRLF$+
        "                                    " +#CRLF$+
        "fire, in end will world the say Some" +#CRLF$+
        "ice. in say Some                    " +#CRLF$+
        "desire of tasted I've what From     " +#CRLF$+
        "fire. favor who those with hold I   " +#CRLF$+
        "                                    " +#CRLF$+
        "... elided paragraph last ...       " +#CRLF$+
        "                                    " +#CRLF$+
        "Frost Robert -----------------------" +#CRLF$
a$ =    "Hey you, Bub!                       " +#CRLF$+#CRLF$+ a$

OpenConsole()
For p1=1 To CountString(a$,#CRLF$)
  b$=StringField(a$,p1,#CRLF$) : c$=""
  For p2=1 To CountString(b$,Chr(32))+1
    c$=StringField(b$,p2,Chr(32))+Space(1)+c$
  Next
  PrintN(LSet(b$,36,Chr(32))+" ---> "+Trim(c$))
Next
Input()

```

```txt

Hey you, Bub!                        ---> Bub! you, Hey
                                     --->
---------- Ice and Fire ------------ ---> ------------ Fire and Ice ----------
                                     --->
fire, in end will world the say Some ---> Some say the world will end in fire,
ice. in say Some                     ---> Some say in ice.
desire of tasted I've what From      ---> From what I've tasted of desire
fire. favor who those with hold I    ---> I hold with those who favor fire.
                                     --->
... elided paragraph last ...        ---> ... last paragraph elided ...
                                     --->
Frost Robert ----------------------- ---> ----------------------- Robert Frost

```



## PHP


```php

<?php

 function strInv ($string) {

 	$str_inv = '' ;

	 for ($i=0,$s=count($string);$i<$s;$i++){
	 	$str_inv .= implode(' ',array_reverse(explode(' ',$string[$i])));
	 	$str_inv .= '
';
	 }

 	return $str_inv;

 }

 $string[] =  "---------- Ice and Fire ------------";
 $string[] =  "";
 $string[] =  "fire, in end will world the say Some";
 $string[] =  "ice. in say Some";
 $string[] =  "desire of tasted I've what From";
 $string[] =  "fire. favor who those with hold I";
 $string[] =  "";
 $string[] =  "... elided paragraph last ...";
 $string[] =  "";
 $string[] =  "Frost Robert ----------------------- ";


echo strInv($string);
```

'''Output''':


```Shell
------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
```



## Python


```python
 text = '''\
---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I

... elided paragraph last ...

Frost Robert -----------------------'''

for line in text.split('\n'): print(' '.join(line.split()[::-1]))
```


'''Output''':


```Shell
------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
```



## R



```R

whack <- function(s) {
  paste( rev( unlist(strsplit(s, " "))), collapse=' ' ) }

poem <- unlist( strsplit(
'------------ Eldorado ----------

... here omitted lines ...

Mountains the "Over
Moon, the Of
Shadow, the of Valley the Down
ride," boldly Ride,
replied,--- shade The
Eldorado!" for seek you "If

Poe Edgar -----------------------', "\n"))

for (line in poem) cat( whack(line), "\n" )

```


```txt

---------- Eldorado ------------

... lines omitted here ...

"Over the Mountains
Of the Moon,
Down the Valley of the Shadow,
Ride, boldly ride,"
The shade replied,---
"If you seek for Eldorado!"

----------------------- Edgar Poe

```


As a dangerous stunt, let's redefine "<tt>{</tt>".
(Everything that happens in R is a function-call.)


```R

> `{`  <-  function(s) rev(unlist(strsplit(s, " ")))
> {"one two three four five"}
[1] "five"  "four"  "three" "two"   "one"

```


You had better restart your REPL after trying this.


## Racket


```racket
#lang racket/base

(require racket/string)

(define (split-reverse str)
  (string-join
   (reverse
    (string-split str))))

(define poem
  "---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I

... elided paragraph last ...

Frost Robert -----------------------")


(let ([poem-port (open-input-string poem)])
  (let loop ([l (read-line poem-port)])
    (unless (eof-object? l)
      (begin (displayln (split-reverse l))
              (loop (read-line poem-port))))))

```


## Red


```Red
Red []
foreach line
split
{---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I

... elided paragraph last ...

Frost Robert -----------------------} newline [
 print reverse split line " "
]

```


## REXX


### natural order

This REXX version process the words in a natural order (first to last).

```rexx
/*REXX program reverses the order of tokens in a string (but not the letters).*/
@.=;                    @.1  =  "---------- Ice and Fire ------------"
                        @.2  =  ' '
                        @.3  =  "fire, in end will world the say Some"
                        @.4  =  "ice. in say Some"
                        @.5  =  "desire of tasted I've what From"
                        @.6  =  "fire. favor who those with hold I"
                        @.7  =  ' '
                        @.8  =  "... elided paragraph last ..."
                        @.9  =  ' '
                        @.10 =  "Frost Robert -----------------------"

  do j=1  while  @.j\==''              /*process each of the 10 lines of poem.*/
  $=                                   /*nullify the  $  string (the new line)*/
     do k=1  for  words(@.j)           /*process each word in a   @.j  string.*/
     $=word(@.j,k) $                   /*prepend a word to the new line  ($). */
     end   /*k*/                       /* [↑]  we could do this another way.  */

  say $                                /*display the newly constructed line.  */
  end      /*j*/                       /*stick a fork in it,  we're all done. */
```

'''output'''   when using the (internal text) ten lines of input:

```txt

------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost

```



### reverse order

This REXX version process the words in reverse order (last to first).

```rexx
/*REXX program reverses the order of tokens in a string (but not the letters).*/
@.=;                    @.1  =  "---------- Ice and Fire ------------"
                        @.2  =  ' '
                        @.3  =  "fire, in end will world the say Some"
                        @.4  =  "ice. in say Some"
                        @.5  =  "desire of tasted I've what From"
                        @.6  =  "fire. favor who those with hold I"
                        @.7  =  ' '
                        @.8  =  "... elided paragraph last ..."
                        @.9  =  ' '
                        @.10 =  "Frost Robert -----------------------"

  do j=1  while  @.j\==''              /*process each of the 10 lines of poem.*/
  $=                                   /*nullify the  $  string (the new line)*/
     do k=words(@.j)   to 1   by -1    /*process each word in a   @.j  string.*/
     $=$  word(@.j,k)                  /*append a word to the new line  ($).  */
     end   /*k*/                       /* [↑]  process last word to first word*/

  say $                                /*display the newly constructed line.  */
  end      /*j*/                       /*stick a fork in it,  we're all done. */
```

'''output'''   is the same as the 1<sup>st</sup> REXX version.




## Ring


```ring

aList = str2list("
---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I

... elided paragraph last ...

Frost Robert -----------------------
")
aList = str2list(cStr)
for x in aList
   x2 = substr(x," ",nl) alist2 = str2list(x2) aList2 = reverse(aList2)
   for y in aList2 see y + " " next see nl
next

```


Output

```ring

------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...
----------------------- Robert Frost

```



## Ruby


```ruby
puts <<EOS
---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I

... elided paragraph last ...

Frost Robert -----------------------
EOS
  .each_line.map {|line| line.split.reverse.join(' ')}
```


Output the same as everyone else's.



## Run BASIC


```runbasic
for i = 1 to 10
    read string$
   j = 1
   r$ = ""
    while word$(string$,j) <> ""
     r$ = word$(string$,j) + " " + r$
     j = j + 1
    WEND
    print r$
next
end

data "---------- Ice and Fire ------------"
data ""
data "fire, in end will world the say Some"
data "ice. in say Some"
data "desire of tasted I've what From"
data "fire. favor who those with hold I"
data ""
data "... elided paragraph last ..."
data ""
data "Frost Robert -----------------------"
```

Output:

```txt
------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I''ve tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
```



## Rust


```rust
const TEXT: &'static str =
"---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I

... elided paragraph last ...

Frost Robert -----------------------";

fn main() {
    println!("{}",
             TEXT.lines() // Returns iterator over lines
             .map(|line|  // Applies closure to each item in iterator (for each line)
                  line.split_whitespace() // Returns iterator of words
                  .rev() // Reverses iterator of words
                  .collect::<Vec<_>>() // Collects words into Vec<&str>
                  .join(" ")) // Convert vector of words back into line
             .collect::<Vec<_>>() // Collect lines into Vec<String>
             .join("\n")); // Concatenate lines into String
}
```



=={{header|S-lang}}==
<lang S-lang>variable ln, in =
  ["---------- Ice and Fire ------------",
   "fire, in end will world the say Some",
   "ice. in say Some",
   "desire of tasted I've what From",
   "fire. favor who those with hold I",
   "",
   "... elided paragraph last ...",
   "",
   "Frost Robert -----------------------"];

foreach ln (in) {
  ln = strtok(ln, " \t");
  array_reverse(ln);
  () = printf("%s\n", strjoin(ln, " "));
}
```

```txt
------------ Fire and Ice ----------
Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
```



## Scala

```Scala
object ReverseWords extends App {

  """|  ---------- Ice and Fire ------------
     |
     |  fire, in end will world the say Some
     |  ice. in say Some
     |  desire of tasted I've what From
     |  fire. favor who those with hold I
     |
     |  ... elided paragraph last ...
     |
     |  Frost Robert -----------------------  """
    .stripMargin.lines.toList.map{_.split(" ")}.map{_.reverse}
    .map(_.mkString(" "))
    .foreach{println}

}
```


```txt

 ------------ Fire and Ice ----------

 Some say the world will end in fire,
 Some say in ice.
 From what I've tasted of desire
 I hold with those who favor fire.

 ... last paragraph elided ...

 ----------------------- Robert Frost
```



## Scheme


```Scheme

(for-each
  (lambda (s) (print (string-join (reverse (string-split s #/ +/)))))
  (string-split
    "---------- Ice and Fire ------------

    fire, in end will world the say Some
    ice. in say Some
    desire of tasted I've what From
    fire. favor who those with hold I

    ... elided paragraph last ...

    Frost Robert -----------------------"
    #/[ \r]*\n[ \r]*/))

```

<b>Output:</b>

```txt

------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const array string: lines is [] (
    "---------- Ice and Fire ------------",
    "",
    "fire, in end will world the say Some",
    "ice. in say Some",
    "desire of tasted I've what From",
    "fire. favor who those with hold I",
    "",
    "... elided paragraph last ...",
    "",
    "Frost Robert -----------------------");

const proc: main is func
  local
    var string: line is "";
    var array string: words is 0 times "";
    var integer: index is 0;
  begin
    for line range lines do
      words := split(line, ' ');
      for index range length(words) downto 1 do
        write(words[index] <& " ");
      end for;
      writeln;
    end for;
  end func;
```


```txt

------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost

```



## Sidef


```ruby
DATA.each{|line| line.words.reverse.join(" ").say};

__DATA__
---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I

... elided paragraph last ...

Frost Robert -----------------------
```



## Smalltalk


```smalltalk

poem := '---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I''ve what From
fire. favor who those with hold I

... elided paragraph last ...

Frost Robert -----------------------'.

(poem lines collect: [ :line | ((line splitOn: ' ') reverse) joinUsing: ' '  ]) joinUsing: (String cr).

```


```txt

'------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I''ve tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost'

```



## Sparkling


This only considers space as the word separator, not tabs, form feeds or any other sort of whitespace. (This, however, turns out not to be an issue with the example input.)


```sparkling
let lines = split("---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I

... elided paragraph last ...

Frost Robert -----------------------", "\n");

foreach(lines, function(idx, line) {
	let words = split(line, " ");
	let reverseWords = map(words, function(idx) { return words[sizeof words - idx - 1]; });
	foreach(reverseWords, function(idx, word) {
		printf("%s ", word);
	});

	print();
});
```



## Swift


```swift
import Foundation

// convenience extension for better clarity
extension String {
    var lines: [String] {
        get {
            return self.componentsSeparatedByCharactersInSet(NSCharacterSet.newlineCharacterSet())
        }
    }
    var words: [String] {
        get {
            return self.componentsSeparatedByCharactersInSet(NSCharacterSet.whitespaceCharacterSet())
        }
    }
}

let input = "---------- Ice and Fire ------------\n\nfire, in end will world the say Some\nice. in say Some\ndesire of tasted I've what From\nfire. favor who those with hold I\n\n... elided paragraph last ...\n\nFrost Robert -----------------------\n"

let output = input.lines.map { $0.words.reverse().joinWithSeparator(" ") }.joinWithSeparator("\n")

print(output)
```

```txt

------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost

```



## Tailspin


```tailspin

def input: ['---------- Ice and Fire ------------',
                    '',
                    'fire, in end will world the say Some',
                    'ice. in say Some',
                    'desire of tasted I''ve what From',
                    'fire. favor who those with hold I',
                    '',
                    '... elided paragraph last ...',
                    '',
                    'Frost Robert -----------------------']
;

composer words
  [ <word>* ]
  rule word: <~WS> <WS>?
end words

$input... -> '$ -> words -> $(-1..1:-1)...;
' -> !OUT::write

```



## Tcl


```tcl
set lines {
    "---------- Ice and Fire ------------"
    ""
    "fire, in end will world the say Some"
    "ice. in say Some"
    "desire of tasted I've what From"
    "fire. favor who those with hold I"
    ""
    "... elided paragraph last ..."
    ""
    "Frost Robert -----------------------"
}
foreach line $lines {
    puts [join [lreverse [regexp -all -inline {\S+} $line]]]
    # This would also work for data this simple:
    ### puts [lreverse $line]
}
```

```txt

------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost

```

Alternatively…
```tcl
puts [join [lmap line $lines {lreverse $line}] "\n"]
```



## TXR

Run from command line:

```bash>txr reverse.txr verse.txt</lang

'''Solution:'''

```txr
@(collect)
@  (some)
@(coll)@{words /[^ ]+/}@(end)
@  (or)
@(bind words nil)
@  (end)
@(end)
@(set words @(mapcar (fun nreverse) words))
@(output)
@  (repeat)
@(rep)@words @(last)@words@(end)
@  (end)
@(end)

```

New line should be present after the last @(end) terminating vertical definition.
i.e.

```txr
@(end)
[EOF]
```

not

```txr
@(end)[EOF]
```



## UNIX Shell

```bash
while read -a words; do
    for ((i=${#words[@]}-1; i>=0; i--)); do
        printf "%s " "${words[i]}"
    done
    echo
done << END
---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I

... elided paragraph last ...

Frost Robert -----------------------
END
```

Same as above, except change
```bash
read -a
```
 to
```bash
read -A
```



## VBA


```vb

Option Explicit

Sub Main()
Dim Lines(9) As String, i&
    'Input
    Lines(0) = "------------- Ice And Fire -------------"
    Lines(1) = ""
    Lines(2) = "fire, in end will world the say Some"
    Lines(3) = "ice. in say Some"
    Lines(4) = "desire of tasted I've what From"
    Lines(5) = "fire. favor who those with hold I"
    Lines(6) = ""
    Lines(7) = "... elided paragraph last ..."
    Lines(8) = ""
    Lines(9) = "Frost Robert -----------------------"
    'Output
    For i = 0 To 9
        Debug.Print ReverseLine(Lines(i), " ")
    Next
End Sub

Private Function ReverseLine(Line As String, Optional Separat As String) As String
Dim T, R, i&, j&, deb&, fin&
    If Len(Line) = 0 Then
        ReverseLine = vbNullString
    Else
        If Separat = "" Then Separat = " "
        T = Split(Line, Separat)
        ReDim R(UBound(T)): j = LBound(T)
        deb = UBound(T): fin = deb / 2
        For i = deb To fin Step -1
            R(j) = T(i)
            R(i) = T(j)
            j = j + 1
        Next i
        ReverseLine = Join(R, Separat)
    End If
End Function
```

```txt
------------- Fire And Ice -------------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost
```



## VBScript


```vb

Option Explicit

Dim objFSO, objInFile, objOutFile
Dim srcDir, line

Set objFSO = CreateObject("Scripting.FileSystemObject")

srcDir = objFSO.GetParentFolderName(WScript.ScriptFullName) & "\"

Set objInFile = objFSO.OpenTextFile(srcDir & "In.txt",1,False,0)

Set objOutFile = objFSO.OpenTextFile(srcDir & "Out.txt",2,True,0)

Do Until objInFile.AtEndOfStream
	line = objInFile.ReadLine
	If line = "" Then
		objOutFile.WriteLine ""
	Else
		objOutFile.WriteLine Reverse_String(line)
	End If
Loop

Function Reverse_String(s)
	Dim arr, i
	arr = Split(s," ")
	For i = UBound(arr) To LBound(arr) Step -1
		If arr(i) <> "" Then
			If i = UBound(arr) Then
				Reverse_String = Reverse_String & arr(i)
			Else
				Reverse_String = Reverse_String & " " & arr(i)
			End If
		End If
	Next
End Function

objInFile.Close
objOutFile.Close
Set objFSO = Nothing

```


Output written to a file.

```txt

 ------------ Fire and Ice ----------

 Some say the world will end in fire,
 Some say in ice.
 From what I've tasted of desire
 I hold with those who favor fire.

 ... last paragraph elided ...

----------------------- Robert Frost

```



## Yabasic


```Yabasic
data "  ---------- Ice and Fire ------------  "
data "                                        "
data "  fire, in end will world the say Some  "
data "  ice. in say Some                      "
data "  desire of tasted I've what From       "
data "  fire. favor who those with hold I     "
data "                                        "
data "  ... elided paragraph last ...         "
data "                                        "
data "  Frost Robert -----------------------  "
data ""

dim w$(1)

do
    read l$
    if l$ <> "" then
        n = token(l$, w$(), " ")
        for i = n to 1 step -1
            print w$(i), " ";
        next
        print
    else
        break
    end if
loop
```



## zkl


```zkl
text:=Data(0,String,
#<<<
"---------- Ice and Fire ------------

fire, in end will world the say Some
ice. in say Some
desire of tasted I've what From
fire. favor who those with hold I

... elided paragraph last ...

Frost Robert -----------------------");
#<<<

text.pump(11,Data,fcn(s){ // process stripped lines
   s.split(" ").reverse().concat(" ") + "\n" })
   .text.print();
```

```txt

------------ Fire and Ice ----------

Some say the world will end in fire,
Some say in ice.
From what I've tasted of desire
I hold with those who favor fire.

... last paragraph elided ...

----------------------- Robert Frost

```

