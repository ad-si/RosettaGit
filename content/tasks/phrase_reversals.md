+++
title = "Phrase reversals"
description = ""
date = 2019-10-22T03:45:25Z
aliases = []
[extra]
id = 18225
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "algol_68",
  "applescript",
  "autohotkey",
  "awk",
  "bacon",
  "batch_file",
  "bracmat",
  "c",
  "clojure",
  "cobol",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "echolisp",
  "elena",
  "elixir",
  "emacs_lisp",
  "factor",
  "fortran",
  "freebasic",
  "gambas",
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
  "lua",
  "maple",
  "miniscript",
  "mumps",
  "oforth",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "plaintex",
  "powershell",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "seed7",
  "sidef",
  "swift",
  "tcl",
  "unix_shell",
  "vba",
  "vbscript",
  "yabasic",
  "zkl",
]
+++

## Task

Given a string of space separated words containing the following phrase:
  rosetta code phrase reversal

:# Reverse the characters of the string.
:# Reverse the characters of each individual word in the string, maintaining original word order within the string.
:# Reverse the order of each word of the string, maintaining the order of characters in each word.


Show your output here.


## See also

* [[Reverse a string]]
* [[Reverse words in a string]]





## AutoHotkey


```AutoHotKey
var =
(
Rosetta Code Phrase Reversal
)

array := strsplit(var, " ")

loop, % array.maxindex()
	string .= array[array.maxindex() - A_index + 1] . " "

loop, % array.maxindex()
{
	m := array[A_index]
	array2 := strsplit(m, "")
	Loop, % array2.maxindex()
		string2 .= array2[array2.maxindex() - A_index + 1]
	string2 .= " "
}

array := strsplit(string, " " )

loop, % array.maxindex()
{
	m := array[A_index]
	array3 := strsplit(m, "")
	Loop, % array3.maxindex()
		string3 .= array3[array3.maxindex() - A_index + 1]
	string3 .= " "
}

MsgBox % var . "`n" . string3 . "`n" . String . "`n" . string2
ExitApp

esc::ExitApp
```


```txt
Rosetta Code Phrase Reversal
lasreveR esarhP edoC attesoR
Reversal Phrase Code Rosetta
attesoR edoC esarhP lasreveR

```



## Ada


To split a string into words, the package "Simple_Parse" from another task
[[http://rosettacode.org/wiki/Reverse_words_in_a_string#Ada]] is used.


```Ada
<with Ada.Text_IO, Simple_Parse;

procedure Phrase_Reversal is

   function Reverse_String (Item : String) return String is
      Result : String (Item'Range);
   begin
      for I in Item'range loop
         Result (Result'Last - I + Item'First) := Item (I);
      end loop;
      return Result;
   end Reverse_String;

   function Reverse_Words(S: String) return String is
      Cursor: Positive := S'First;
      Word: String := Simple_Parse.Next_Word(S, Cursor);
   begin
      if Cursor > S'Last then -- Word holds the last word
	 return Reverse_String(Word);
      else
	 return Reverse_String(Word) & " " & Reverse_Words(S(Cursor .. S'Last));
      end if;
   end Reverse_Words;

   function Reverse_Order(S: String) return String is
      Cursor: Positive := S'First;
      Word: String := Simple_Parse.Next_Word(S, Cursor);
   begin
      if Cursor > S'Last then -- Word holds the last word
	 return Word;
      else
	 return Reverse_Order(S(Cursor .. S'Last)) & " " & Word;
      end if;
   end Reverse_Order;

   Phrase: String := "rosetta code phrase reversal";
   use Ada.Text_IO;
begin
   Put_Line("0. The original phrase:       """ & Phrase & """");
   Put_Line("1. Reverse the entire phrase: """ & Reverse_String(Phrase) & """");
   Put_Line("2. Reverse words, same order: """ & Reverse_Words(Phrase) & """");
   Put_Line("2. Reverse order, same words: """ & Reverse_Order(Phrase) & """");
end Phrase_Reversal;
```


```txt
0. The original phrase:       "rosetta code phrase reversal"
1. Reverse the entire phrase: "lasrever esarhp edoc attesor"
2. Reverse words, same order: "attesor edoc esarhp lasrever"
2. Reverse order, same words: "reversal phrase code rosetta"
```



## ALGOL 68

```algol68
# reverses the characters in str from start pos to end pos                   #
PROC in place reverse = ( REF STRING str, INT start pos, INT end pos )VOID:
    BEGIN
        INT fpos := start pos, epos := end pos;
        WHILE fpos < epos
        DO
            CHAR c      := str[ fpos ];
            str[ fpos ] := str[ epos ];
            str[ epos ] := c;
            fpos       +:= 1;
            epos       -:= 1
        OD
    END; # in place reverse #

STRING original phrase := "rosetta code phrase reversal";

STRING whole reversed  := original phrase;
in place reverse( whole reversed, LWB whole reversed, UPB whole reversed );

# reverse the individual words #
STRING words reversed := original phrase;
INT start pos         := LWB words reversed;

WHILE
    # skip leading spaces #
    WHILE IF start pos <= UPB words reversed
          THEN words reversed[ start pos ] = " "
          ELSE FALSE
          FI
    DO start pos +:= 1
    OD;
    start pos <= UPB words reversed
DO
    # have another word, find it #
    INT end pos := start pos;
    WHILE IF end pos <= UPB words reversed
          THEN words reversed[ end pos ] /= " "
          ELSE FALSE
          FI
    DO end pos +:= 1
    OD;
    in place reverse( words reversed, start pos, end pos - 1 );
    start pos := end pos + 1
OD;

# reversing the reversed words in the same order as the original will        #
# reverse the order of the words                                             #
STRING order reversed := words reversed;
in place reverse( order reversed, LWB order reversed, UPB order reversed );

print( ( original phrase, ": whole reversed -> ", whole reversed, newline
       , original phrase, ": words reversed -> ", words reversed, newline
       , original phrase, ": order reversed -> ", order reversed, newline
       )
 )
```

```txt

rosetta code phrase reversal: whole reversed -> lasrever esarhp edoc attesor
rosetta code phrase reversal: words reversed -> attesor edoc esarhp lasrever
rosetta code phrase reversal: order reversed -> reversal phrase code rosetta


```



## AppleScript


AppleScript has a very small and patchy library of primitive functions. To accumulate a larger and more coherent library, which includes some higher order functions, we can try to overcome two architectural weaknesses: 1. Built-in functions have a different type from user functions, and 2. user functions are second class properties of (first class) script objects.

Here is a simple illustration of unifying (and elevating) the function type by wrapping the built-in functions in user handlers (perhaps making some of them polymorphic where needed), and also obtaining first class status for ordinary user handler functions by 'lifting' them (for use as arguments in higher order functions) into a first class script object. (This process can be inlined, or abstracted out to an '''mReturn''' or '''mInject''' function).


```AppleScript
-- REVERSED PHRASES, COMPONENT WORDS, AND WORD ORDER ---------------------

-- reverseString, reverseEachWord, reverseWordOrder :: String -> String
on stringReverse(s)
    |reverse|(s)
end stringReverse

on reverseEachWord(s)
    wordLevel(curry(my map)'s |λ|(my |reverse|))'s |λ|(s)
end reverseEachWord

on reverseWordOrder(s)
    wordLevel(my |reverse|)'s |λ|(s)
end reverseWordOrder


-- wordLevel :: ([String] -> [String]) -> String -> String
on wordLevel(f)
    script
        on |λ|(x)
            unwords(mReturn(f)'s |λ|(|words|(x)))
        end |λ|
    end script
end wordLevel


-- TEST ----------------------------------------------------------------------
on run
    unlines(|<*>|({stringReverse, reverseEachWord, reverseWordOrder}, ¬
        {"rosetta code phrase reversal"}))

    -->

    --     "lasrever esarhp edoc attesor
    --      attesor edoc esarhp lasrever
    --      reversal phrase code rosetta"
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- A list of functions applied to a list of arguments
-- (<*> | ap) :: [(a -> b)] -> [a] -> [b]
on |<*>|(fs, xs)
    set {nf, nx} to {length of fs, length of xs}
    set acc to {}
    repeat with i from 1 to nf
        tell mReturn(item i of fs)
            repeat with j from 1 to nx
                set end of acc to |λ|(contents of (item j of xs))
            end repeat
        end tell
    end repeat
    return acc
end |<*>|

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

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

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

-- reverse :: [a] -> [a]
on |reverse|(xs)
    if class of xs is text then
        (reverse of characters of xs) as text
    else
        reverse of xs
    end if
end |reverse|

-- words :: String -> [String]
on |words|(s)
    words of s
end |words|

-- unlines :: [String] -> String
on unlines(lstLines)
    intercalate(linefeed, lstLines)
end unlines

-- unwords :: [String] -> String
on unwords(lstWords)
    intercalate(space, lstWords)
end unwords
```

```txt
"lasrever esarhp edoc attesor
attesor edoc esarhp lasrever
reversal phrase code rosetta"
```



## AWK


```awk
# Usage:  awk -f phrase_revers.awk
function rev(s, del,   n,i,a,r) {
   n = split(s, a, del)
   r = a[1]
   for(i=2; i <= n; i++) {r = a[i] del r }
   return r
}

BEGIN {
  p0 = "Rosetta Code Phrase Reversal"

  fmt = "%-20s: %s\n"
  printf( fmt, "input",               p0 )
  printf( fmt, "string reversed",     rev(p0, "") )
  wr = rev(p0, " ")
  printf( fmt, "word-order reversed", wr )
  printf( fmt, "each word reversed",  rev(wr) )
}
```

```txt

input               : Rosetta Code Phrase Reversal
string reversed     : lasreveR esarhP edoC attesoR
word-order reversed : Reversal Phrase Code Rosetta
each word reversed  : attesoR edoC esarhP lasreveR

```



## BaCon


```qbasic
phrase$ = "rosetta code phrase reversal"

PRINT REVERSE$(phrase$)

PRINT REV$(REVERSE$(phrase$))

PRINT REV$(phrase$)
```

```txt

lasrever esarhp edoc attesor
attesor edoc esarhp lasrever
reversal phrase code rosetta

```



## Batch File


```dos
@echo off
setlocal enabledelayedexpansion
	%
###  The Main Thing...
%
set "inp=Rosetta Code phrase reversal"
call :reverse_string "!inp!" rev1
call :reverse_order "!inp!" rev2
call :reverse_words "!inp!" rev3
cls
echo.Original:       !inp!
echo.Reversed:       !rev1!
echo.Reversed Order: !rev2!
echo.Reversed Words: !rev3!
pause>nul
exit /b 0
	%
###  /The Main Thing...
%

	%
###  Reverse the Order Function
%
:reverse_order
set var1=%2
set %var1%=&set word=&set str1=%1
:process1
for /f "tokens=1,*" %%A in (%str1%) do (set str1=%%B&set word=%%A)
set %var1%=!word! !%var1%!&set str1="!str1!"
if not !str1!=="" goto process1
goto :EOF
	%
###  /Reverse the Order Function
%

	%
###  Reverse the Whole String Function
%
:reverse_string
set var2=%2
set %var2%=&set cnt=0&set str2=%~1
:process2
set char=!str2:~%cnt%,1!&set %var2%=!char!!%var2%!
if not "!char!"=="" set /a cnt+=1&goto process2
goto :EOF
	%
###  /Reverse the Whole String Function
%

	%
###  Reverse each Words Function
%
:reverse_words
set var3=%2
set %var3%=&set word=&set str3=%1
:process3
for /f "tokens=1,*" %%A in (%str3%) do (set str3=%%B&set word=%%A)
call :reverse_string "%word%" revs
set %var3%=!%var3%! !revs!&set str3="!str3!"
if not !str3!=="" goto process3
set %var3%=!%var3%:~1,1000000!
goto :EOF
	%
###  /Reverse each Words Function
%
```

```txt
Original:       Rosetta Code phrase reversal
Reversed:       lasrever esarhp edoC attesoR
Reversed Order: reversal phrase Code Rosetta
Reversed Words: attesoR edoC esarhp lasrever
```



## Bracmat

This example only works correctly with strings only consisting of byte-sized characters.

```bracmat
( "rosetta code phrase reversal":?text
& rev$!text:?output1
& get$(!text,MEM):?words
& :?output2:?output3
&   whl
  ' ( !words:%?word %?words
    & !output2 rev$!word " ":?output2
    & " " !word !output3:?output3
    )
& str$(!output2 rev$!words):?output2
& str$(!words !output3):?output3
&   out
  $ ( str
    $ ("0:\"" !text "\"\n1:\"" !output1 "\"\n2:\"" !output2 "\"\n3:\"" !output3 \"\n)
    )
);
```

Output:

```txt
0:"rosetta code phrase reversal"
1:"lasrever esarhp edoc attesor"
2:"attesor edoc esarhp lasrever"
3:"reversal phrase code rosetta"
```



## C

Working with C strings is often long-winded.

```C

#include <stdio.h>
#include <string.h>

/* The functions used are destructive, so after each call the string needs
 * to be copied over again. One could easily allocate new strings as
 * required, but this way allows the caller to manage memory themselves */

char* reverse_section(char *s, size_t length)
{
    if (length == 0) return s;

    size_t i; char temp;
    for (i = 0; i < length / 2 + 1; ++i)
        temp = s[i], s[i] = s[length - i], s[length - i] = temp;
    return s;
}

char* reverse_words_in_order(char *s, char delim)
{
    if (!strlen(s)) return s;

    size_t i, j;
    for (i = 0; i < strlen(s) - 1; ++i) {
        for (j = 0; s[i + j] != 0 && s[i + j] != delim; ++j)
            ;
        reverse_section(s + i, j - 1);
        s += j;
    }
    return s;
}

char* reverse_string(char *s)
{
    return strlen(s) ? reverse_section(s, strlen(s) - 1) : s;
}

char* reverse_order_of_words(char *s, char delim)
{
    reverse_string(s);
    reverse_words_in_order(s, delim);
    return s;
}

int main(void)
{
    char str[]    = "rosetta code phrase reversal";
    size_t lenstr = sizeof(str) / sizeof(str[0]);
    char scopy[lenstr];
    char delim = ' ';

    /* Original String */
    printf("Original:       \"%s\"\n", str);

    /* Reversed string */
    strncpy(scopy, str, lenstr);
    reverse_string(scopy);
    printf("Reversed:       \"%s\"\n", scopy);

    /* Reversed words in string */
    strncpy(scopy, str, lenstr);
    reverse_words_in_order(scopy, delim);
    printf("Reversed words: \"%s\"\n", scopy);

    /* Reversed order of words in string */
    strncpy(scopy, str, lenstr);
    reverse_order_of_words(scopy, delim);
    printf("Reversed order: \"%s\"\n", scopy);

    return 0;
}

```

```txt

Original:       "rosetta code phrase reversal"
Reversed:       "lasrever esarhp edoc attesor"
Reversed words: "attesor edoc esarhp lasrever"
Reversed order: "reversal phrase code rosetta"

```



## C++


```cpp
#include <iostream>
#include <iostream>
#include <vector>
#include <algorithm>
#include <string>
#include <iterator>
#include <sstream>

int main() {
   std::string s = "rosetta code phrase reversal";
   std::cout << "Input : " << s << '\n'
             << "Input reversed : " << std::string(s.rbegin(), s.rend()) << '\n' ;
   std::istringstream is(s);
   std::vector<std::string> words(std::istream_iterator<std::string>(is), {});
   std::cout << "Each word reversed : " ;
   for(auto w : words)
      std::cout << std::string(w.rbegin(), w.rend()) << ' ';
   std::cout << '\n'
             << "Original word order reversed : " ;
   reverse_copy(words.begin(), words.end(), std::ostream_iterator<std::string>(std::cout, " "));
   std::cout << '\n' ;
}

```

```txt
Input : rosetta code phrase reversal
Input reversed : lasrever esarhp edoc attesor
Each word reversed : attesor edoc esarhp lasrever
Original word order reversed : reversal phrase code rosetta

```



## C#



```c#
using System;
using System.Linq;
namespace ConsoleApplication
{
    class Program
    {
        static void Main(string[] args)
        {
            //Reverse() is an extension method on IEnumerable<char>.
            //The constructor takes a char[], so we have to call ToArray()
            Func<string, string> reverse = s => new string(s.Reverse().ToArray());

            string phrase = "rosetta code phrase reversal";
            //Reverse the string
            Console.WriteLine(reverse(phrase));
            //Reverse each individual word in the string, maintaining original string order.
            Console.WriteLine(string.Join(" ", phrase.Split(' ').Select(word => reverse(word))));
            //Reverse the order of each word of the phrase, maintaining the order of characters in each word.
            Console.WriteLine(string.Join(" ", phrase.Split(' ').Reverse()));
        }
    }
}
```



## Clojure


```clojure
(use '[clojure.string :only (join split)])
(def phrase "rosetta code phrase reversal")
(defn str-reverse [s] (apply str (reverse s)))

; Reverse string
(str-reverse phrase)
; Words reversed
(join " " (map str-reverse (split phrase #" ")))
; Word order reversed
(apply str (interpose " " (reverse (split phrase #" "))))

```

```txt
"lasrever esarhp edoc attesor"
"attesor edoc esarhp lasrever"
"reversal phrase code rosetta"
```



## COBOL


```COBOL

       program-id. phra-rev.
       data division.
       working-storage section.
       1 phrase pic x(28) value "rosetta code phrase reversal".
       1 wk-str pic x(16).
       1 binary.
        2 phrase-len pic 9(4).
        2 pos pic 9(4).
        2 cnt pic 9(4).
       procedure division.
           compute phrase-len = function length (phrase)
           display phrase
           display function reverse (phrase)
           perform display-words
           move function reverse (phrase) to phrase
           perform display-words
           stop run
           .

       display-words.
           move 1 to pos
           perform until pos > phrase-len
               unstring phrase delimited space
               into wk-str count in cnt
               with pointer pos
               end-unstring
               display function reverse (wk-str (1:cnt))
                   with no advancing
               if pos < phrase-len
                   display space with no advancing
               end-if
           end-perform
           display space
           .
       end program phra-rev.

```

```txt

rosetta code phrase reversal
lasrever esarhp edoc attesor
attesor edoc esarhp lasrever
reversal phrase code rosetta

```



## Common Lisp


```lisp

(defun split-string (str)
 "Split a string into space separated words including spaces"
  (do* ((lst nil)
        (i (position-if #'alphanumericp str) (position-if #'alphanumericp str :start j))
        (j (when i (position #\Space str :start i)) (when i (position #\Space str :start i))) )
       ((null j) (nreverse (push (subseq str i nil) lst)))
    (push (subseq str i j) lst)
    (push " " lst) ))


(defun task (str)
  (print (reverse str))
  (let ((lst (split-string str)))
    (print (apply #'concatenate 'string (mapcar #'reverse lst)))
    (print (apply #'concatenate 'string (reverse lst))) )
  nil )


```

```txt
(task "rosetta code phrase reversal")

"lasrever esarhp edoc attesor"
"attesor edoc esarhp lasrever"
"reversal phrase code rosetta"
nil

```



## D

Partially lazy.

```d
void main() @safe {
    import std.stdio, std.range, std.algorithm;

    immutable phrase = "rosetta code phrase reversal";
    phrase.retro.writeln;                          // Reversed string.
    phrase.splitter.map!retro.joiner(" ").writeln; // Words reversed.
    phrase.split.retro.joiner(" ").writeln;        // Word order reversed.
}
```

```txt
lasrever esarhp edoc attesor
attesor edoc esarhp lasrever
reversal phrase code rosetta
```



## EchoLisp


```scheme

(define (string-reverse string)
    (list->string (reverse (string->list string))))

(define (task str)
    (for-each writeln (list
      (string-reverse str)
      (string-join (map string-reverse (string-split str )))
      (string-join (reverse (string-split str ))))))


(task "rosetta code phrase reversal")
    "lasrever esarhp edoc attesor"
    "attesor edoc esarhp lasrever"
    "reversal phrase code rosetta"

```


## Elena

ELENA 4.x :

```elena
import extensions;
import extensions'text;
import system'routines;

public program()
{
    var reverse := (s => s.toArray().sequenceReverse().summarize(new StringWriter()));

    var phrase := "rosetta code phrase reversal";
    console.printLine(phrase);

    //Reverse the string
    console.printLine(reverse(phrase));
    //Reverse each individual word in the string, maintaining original string order.
    console.printLine(phrase.splitBy:" ".selectBy:(s => reverse(s).add(" ")).summarize(new StringWriter()));
    //Reverse the order of each word of the phrase, maintaining the order of characters in each word.
    console.printLine(reverse(phrase.splitBy:" ".selectBy:(s => s + " ")))
}
```

```txt

rosetta code phrase reversal
lasrever esarhp edoc attesor
attesor edoc esarhp lasrever
reversal phrase code rosetta

```



## Elixir


```elixir
str = "rosetta code phrase reversal"

IO.puts String.reverse(str)
IO.puts String.split(str) |> Enum.map(&String.reverse(&1)) |> Enum.join(" ")
IO.puts String.split(str) |> Enum.reverse |> Enum.join(" ")
```


```txt

lasrever esarhp edoc attesor
attesor edoc esarhp lasrever
reversal phrase code rosetta

```



## Emacs Lisp


```Emacs Lisp

(defun reverse-sep (words sep)
  (mapconcat 'identity (reverse (split-string words sep) ) sep) )

(defun reverse-chars (line)
  (reverse-sep line "") )

(defun reverse-words (line)
  (reverse-sep line " ") )

(progn
  (setq line "rosetta code phrase reversal")

  (insert (format "%s\n" (reverse-chars line) ))

    (insert (format "%s\n"
        (mapconcat 'identity (mapcar #'reverse-chars
                                     (split-string line) ) " ") ))

    (insert (format "%s\n" (reverse-words line) )))

```

<b>Output:</b>

```txt

lasrever esarhp edoc attesor
attesor edoc esarhp lasrever
reversal phrase code rosetta

```



## Factor


```factor

USE: splitting

: splitp         ( str -- seq ) " " split ;
: printp         ( seq -- )     " " join print ;
: reverse-string ( str -- )     reverse print ;
: reverse-words  ( str -- )     splitp [ reverse ] map printp ;
: reverse-phrase ( str -- )     splitp reverse printp ;

"rosetta code phrase reversal" [ reverse-string ] [ reverse-words ] [ reverse-phrase ] tri

```

```txt

lasrever esarhp edoc attesor
attesor edoc esarhp lasrever
reversal phrase code rosetta

```



## Fortran

The key here is the ability via F90 to specify an array span, as A(''first'':''last'':''step'') where the ''step'' can be negative... This facility is not available for CHARACTER variables where only TEXT(''first'':''last'') is available - no ''step'' is accommodated and TEXT(''last'':''first'') evokes nothing rather than a backwards order as in Python. However, one can have an array of CHARACTER*1 variables, and as an array they can be rolled bidirectionally. For convenience in initialising such an array, EQUIVALENCE(TEXT,ATXT) means that a normal multi-character text literal can be assigned to TEXT via the DATA statement, rather than having to specify the ATXT elements one at a time.

By identifying the first and last character position of each word in TEXT (or equivalently, their indices in ATXT) and storing them in arrays IST and LST, the i'th word can be fingered via IST(i) to LST(i) and of course a DO-loop can step in either direction.

F90 allows a WHILE-loop, and writing something like
```Fortran
      DO WHILE (L1.LE.L .AND. ATXT(L1).LE." ")
        L1 = L1 + 1
      END DO
```

Would be rather more structured and involve fewer GO TOs and their labels, but alas, modern Fortran specifies that there is no specification as to whether or not both terms of an expression such as (A '''and''' B) will always be evaluated or instead there will be a shortcut: if A is ''false'' then the B term is ignored. And here, the A term checks whether or not L1 is within bounds and if it is not, then the B term should not be evaluated, not just because of the waste of effort but because to do so might involve accessing outside the definition of ATXT. As when the scan chases through the trailing spaces. One could make the array one longer, or rather, <code>L = LEN(TEXT) - 1</code> for this case but that would be messy, require explanation, be easily forgotten, and, typical ad-hoc testing would be unlikely to detect the mistake.

Alternatively, a GO TO can be removed from view by using EXIT in its place:
```Fortran
      DO L1 = L1,L
        IF (ATXT(L1).GT." ") EXIT
      END DO
```
 Except that this relies on the index variable retaining its value  on exiting the loop, either as fingering the first non-blank or, being L + 1. This expectation is frowned upon in some quarters.

Both variants would have to be followed by a test such as <code>IF (L1 .LE. L) THEN</code> to identify whether the start of a word has been found that would require further processing. So, all in all, suck up the GO TOs...
```Fortran
      PROGRAM REVERSER	!Just fooling around.
      CHARACTER*(66) TEXT	!Holds the text. Easily long enough.
      CHARACTER*1 ATXT(66)	!But this is what I play with.
      EQUIVALENCE (TEXT,ATXT)	!Same storage, different access abilities..
      DATA TEXT/"Rosetta Code Phrase Reversal"/	!Easier to specify this for TEXT.
      INTEGER IST(6),LST(6)	!Start and stop positions.
      INTEGER N,L,I		!Counters.
      INTEGER L1,L2		!Fingers for the scan.
      CHARACTER*(*) AS,RW,FW,RO,FO			!Now for some cramming.
      PARAMETER (AS = "Words ordered as supplied")	!So that some statements can fit on a line.
      PARAMETER (RW = "Reversed words, ", FW = "Forward words, ")
      PARAMETER (RO = "reverse order",    FO = "forward order")

Chop the text into words.
      N = 0		!No words found.
      L = LEN(TEXT)	!Multiple trailing spaces - no worries.
      L2 = 0		!Syncopation: where the previous chomp ended.
   10 L1 = L2		!Thus, where a fresh scan should follow.
   11 L1 = L1 + 1		!Advance one.
      IF (L1.GT.L) GO TO 20		!Finished yet?
      IF (ATXT(L1).LE." ") GO TO 11	!No. Skip leading spaces.
      L2 = L1			!Righto, L1 is the first non-blank.
   12 L2 = L2 + 1		!Scan through the non-blanks.
      IF (L2.GT.L) GO TO 13	!Is it safe to look?
      IF (ATXT(L2).GT." ") GO TO 12	!Yes. Speed through non-blanks.
   13 N = N + 1			!Righto, a word is found in TEXT(L1:L2 - 1)
      IST(N) = L1		!So, recall its first character.
      LST(N) = L2 - 1		!And its last.
      IF (L2.LT.L) GO TO 10	!Perhaps more text follows.

Chuck the words around.
   20 WRITE (6,21) N,TEXT	!First, say what has been discovered.
   21 FORMAT (I4," words have been isolated from the text ",A,/)

      WRITE (6,22) AS,    (" ",ATXT(IST(I):LST(I):+1), I = 1,N,+1)
      WRITE (6,22) RW//RO,(" ",ATXT(LST(I):IST(I):-1), I = N,1,-1)
      WRITE (6,22) FW//RO,(" ",ATXT(IST(I):LST(I):+1), I = N,1,-1)
      WRITE (6,22) RW//FO,(" ",ATXT(LST(I):IST(I):-1), I = 1,N,+1)

   22 FORMAT (A36,":",66A1)
      END
```

With F77 such array spans can't be used, but all that is necessary is to supply a second implied DO-loop in the WRITE statement, for example
```Fortran
      WRITE (6,22) RW//RO,(" ",(ATXT(J), J = LST(I),IST(I),-1), I = 1,N,+1)
```


And the output is...

```txt

   4 words have been isolated from the text Rosetta Code Phrase Reversal

           Words ordered as supplied: Rosetta Code Phrase Reversal
       Reversed words, reverse order: lasreveR esarhP edoC attesoR
        Forward words, reverse order: Reversal Phrase Code Rosetta
       Reversed words, forward order: attesoR edoC esarhP lasreveR

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Sub split (s As Const String, sepList As Const String, result() As String)
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
    result(i - 1) = Mid(s, position(i - 1) + 1, length)
  Next
End Sub

Function reverse(s As Const String) As String
  If s = "" Then Return ""
  Dim t As String = s
  Dim length As Integer = Len(t)
  For i As Integer = 0 to length\2 - 1
    Swap t[i], t[length - 1 - i]
  Next
  Return t
End Function

Dim s As String = "rosetta code phrase reversal"
Dim a() As String
Dim sepList As String = " "

Print "Original string => "; s
Print "Reversed string => "; reverse(s)
Print "Reversed words  => ";
split s, sepList, a()
For i As Integer = LBound(a) To UBound(a)
  Print reverse(a(i)); " ";
Next
Print
Print "Reversed order  => ";
For i As Integer = UBound(a) To LBound(a) Step -1
  Print a(i); " ";
Next
Print : Print
Print "Press any key to quit"
Sleep
```


```txt

Original string => rosetta code phrase reversal
Reversed string => lasrever esarhp edoc attesor
Reversed words  => attesor edoc esarhp lasrever
Reversed order  => reversal phrase code rosetta

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=77cb8b3386a0f57524bdbff6634387cd Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sString As String = "rosetta code phrase reversal"  'The string
Dim sNewString, sTemp As String                         'String variables
Dim siCount As Short                                    'Counter
Dim sWord As New String[]                               'Word store

For siCount = Len(sString) DownTo 1                     'Loop backwards through the string
  sNewString &= Mid(sString, sicount, 1)                'Add each character to the new string
Next

Print "Original string = \t" & sString & "\n" &         'Print the original string
  "Reversed string = \t" & sNewString                   'Print the reversed string

sNewString = ""                                         'Reset sNewString

For Each sTemp In Split(sString, " ")                   'Split the original string by the spaces
  sWord.Add(sTemp)                                      'Add each word to sWord array
Next

For siCount = sWord.max DownTo 0                        'Loop backward through each word in sWord
  sNewString &= sWord[siCount] & " "                    'Add each word to to sNewString
Next

Print "Reversed word order = \t" & sNewString           'Print reversed word order

sNewString = ""                                         'Reset sNewString
For Each sTemp In sWord                                 'For each word in sWord
  For siCount = Len(sTemp) DownTo 1                     'Loop backward through the word
    sNewString &= Mid(sTemp, siCount, 1)                'Add the characters to sNewString
  Next
  sNewString &= " "                                     'Add a space at the end of each word
Next

Print "Words reversed = \t" & sNewString                'Print words reversed

End
```

Output:

```txt

Original string =       rosetta code phrase reversal
Reversed string =       lasrever esarhp edoc attesor
Reversed word order =   reversal phrase code rosetta
Words reversed =        attesor edoc esarhp lasrever

```



## Go


### Simple


```go
package main

import (
	"fmt"
	"strings"
)

const phrase = "rosetta code phrase reversal"

func revStr(s string) string {
	rs := make([]rune, len(s))
	i := len(s)
	for _, r := range s {
		i--
		rs[i] = r
	}
	return string(rs[i:])
}

func main() {
	fmt.Println("Reversed:           ", revStr(phrase))

	ws := strings.Fields(phrase)
	for i, w := range ws {
		ws[i] = revStr(w)
	}
	fmt.Println("Words reversed:     ", strings.Join(ws, " "))

	ws = strings.Fields(phrase)
	last := len(ws) - 1
	for i, w := range ws[:len(ws)/2] {
		ws[i], ws[last-i] = ws[last-i], w
	}
	fmt.Println("Word order reversed:", strings.Join(ws, " "))
}
```

```txt

Reversed:            lasrever esarhp edoc attesor
Words reversed:      attesor edoc esarhp lasrever
Word order reversed: reversal phrase code rosetta

```


### Alternative


```go
package main

import (
	"fmt"
	"regexp"
	"sort"
	"strings"
)

const phrase = "rosetta code phrase reversal"

type reversible interface {
	Len() int
	Swap(i, j int)
}

func reverse(p reversible) {
	mid := p.Len() / 2
	last := p.Len() - 1
	for i := 0; i < mid; i++ {
		p.Swap(i, last-i)
	}
}

type runeSlice []rune

func (p runeSlice) Len() int      { return len(p) }
func (p runeSlice) Swap(i, j int) { p[i], p[j] = p[j], p[i] }

func reverseString(s string) string {
	r := runeSlice(s)
	reverse(r)
	return string(r)
}

var rx = regexp.MustCompile(`\S*`)

func reverseWords(s string) string {
	return rx.ReplaceAllStringFunc(s, func(m string) string {
		return reverseString(m)
	})
}

func reverseWordOrder(s string) string {
	l := sort.StringSlice(strings.Fields(s))
	reverse(l)
	return strings.Join(l, " ")
}

func main() {
	fmt.Println("Reversed:           ", reverseString(phrase))
	fmt.Println("Words reversed:     ", reverseWords(phrase))
	fmt.Println("Word order reversed:", reverseWordOrder(phrase))
}
```

```txt

Reversed:            lasrever esarhp edoc attesor
Words reversed:      attesor edoc esarhp lasrever
Word order reversed: reversal phrase code rosetta

```



## Groovy


```groovy
def phaseReverse = { text, closure -> closure(text.split(/ /)).join(' ')}

def text = 'rosetta code phrase reversal'
println "Original:       $text"
println "Reversed:       ${phaseReverse(text) { it.reverse().collect { it.reverse() } } }"
println "Reversed Words: ${phaseReverse(text) { it.collect { it.reverse() } } }"
println "Reversed Order: ${phaseReverse(text) { it.reverse() } }"
```

```txt
Original:       rosetta code phrase reversal
Reversed:       lasrever esarhp edoc attesor
Reversed Words: attesor edoc esarhp lasrever
Reversed Order: reversal phrase code rosetta
```



## Haskell


```haskell
reverseString, reverseEachWord, reverseWordOrder :: String -> String
reverseString = reverse

reverseEachWord = wordLevel (fmap reverse)

reverseWordOrder = wordLevel reverse

wordLevel :: ([String] -> [String]) -> String -> String
wordLevel f = unwords . f . words

main :: IO ()
main =
  (putStrLn . unlines) $
  [reverseString, reverseEachWord, reverseWordOrder] <*>
  ["rosetta code phrase reversal"]
```

```txt
lasrever esarhp edoc attesor
attesor edoc esarhp lasrever
reversal phrase code rosetta
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "ReverseS.bas"
110 LET S$="Rosetta Code Pharse Reversal"
120 PRINT S$
130 PRINT REVERSE$(S$)
140 PRINT REVERSEW$(S$)
150 PRINT REVERSEC$(S$)
160 DEF REVERSE$(S$)
170   LET T$=""
180   FOR I=LEN(S$) TO 1 STEP-1
190     LET T$=T$&S$(I)
200   NEXT
210   LET REVERSE$=T$
220 END DEF
230 DEF REVERSEW$(S$)
240   LET T$="":LET PE=LEN(S$)
250   FOR PS=PE TO 1 STEP-1
260     IF PS=1 OR S$(PS)=" " THEN LET T$=T$&" ":LET T$=T$&LTRIM$(S$(PS:PE)):LET PE=PS-1
270   NEXT
280   LET REVERSEW$=LTRIM$(T$)
290 END DEF
300 DEF REVERSEC$(S$)
310   LET T$="":LET PS=1
320   FOR PE=1 TO LEN(S$)
330     IF PE=LEN(S$) OR S$(PE)=" " THEN LET T$=T$&" ":LET T$=T$&REVERSE$(RTRIM$(S$(PS:PE))):LET PS=PE+1
340   NEXT
350   LET REVERSEC$=LTRIM$(T$)
360 END DEF
```



## J

'''Solution:'''

```j
   getWords=: (' '&splitstring) :. (' '&joinstring)
   reverseString=: |.
   reverseWords=: |.&.>&.getWords
   reverseWordOrder=: |.&.getWords
```

'''Usage:'''

```j
   phrase=: 'rosetta code phrase reversal'
   (reverseWordOrder , reverseWords ,: reverseString) phrase
reversal phrase code rosetta
attesor edoc esarhp lasrever
lasrever esarhp edoc attesor
```



## Java

```java5
import java.util.Arrays;

public class PhraseRev{
	private static String reverse(String x){
		return new StringBuilder(x).reverse().toString();
	}

	private static <T> T[] reverse(T[] x){
		T[] rev = Arrays.copyOf(x, x.length);
		for(int i = x.length - 1; i >= 0; i--){
			rev[x.length - 1 - i] = x[i];
		}
		return rev;
	}

	private static String join(String[] arr, String joinStr){
		StringBuilder joined = new StringBuilder();
		for(int i = 0; i < arr.length; i++){
			joined.append(arr[i]);
			if(i < arr.length - 1) joined.append(joinStr);
		}
		return joined.toString();
	}

	public static void main(String[] args){
		String str = "rosetta code phrase reversal";

		System.out.println("Straight-up reversed: " + reverse(str));
		String[] words = str.split(" ");
		for(int i = 0; i < words.length; i++){
			words[i] = reverse(words[i]);
		}
		System.out.println("Reversed words: " + join(words, " "));
		System.out.println("Reversed word order: " + join(reverse(str.split(" ")), " "));
	}
}
```

```txt
Straight-up reversed: lasrever esarhp edoc attesor
Reversed words: attesor edoc esarhp lasrever
Reversed word order: reversal phrase code rosetta
```



## JavaScript


### ES5


```JavaScript
(function (p) {
  return [
    p.split('').reverse().join(''),

    p.split(' ').map(function (x) {
        return x.split('').reverse().join('');
    }).join(' '),

    p.split(' ').reverse().join(' ')

  ].join('\n');

})('rosetta code phrase reversal');
```

```txt
lasrever esarhp edoc attesor
attesor edoc esarhp lasrever
reversal phrase code rosetta
```



### ES6


```JavaScript
(() => {
    'use strict'

    // reverseString, reverseEachWord, reverseWordOrder :: String -> String
    const
        reverseString = s => reverse(s),

        reverseEachWord = s => wordLevel(map(reverse))(s),

        reverseWordOrder = s => wordLevel(reverse)(s);

    // wordLevel :: ([String] -> [String]) -> String -> String
    const wordLevel = f =>
        x => unwords(f(words(x)));


    // GENERIC FUNCTIONS -----------------------------------------------------

    // A list of functions applied to a list of arguments
    // <*> :: [(a -> b)] -> [a] -> [b]
    const ap = (fs, xs) => //
        [].concat.apply([], fs.map(f => //
            [].concat.apply([], xs.map(x => [f(x)]))));

    // 2 or more arguments
    // curry :: Function -> Function
    const curry = (f, ...args) => {
        const go = xs => xs.length >= f.length ? (f.apply(null, xs)) :
            function () {
                return go(xs.concat(Array.from(arguments)));
            };
        return go([].slice.call(args, 1));
    };

    // map :: (a -> b) -> [a] -> [b]
    const map = curry((f, xs) => xs.map(f));

    // reverse :: [a] -> [a]
    const reverse = curry(xs =>
        typeof xs === 'string' ? (
            xs.split('')
            .reverse()
            .join('')
        ) : xs.slice(0)
        .reverse());

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // unwords :: [String] -> String
    const unwords = xs => xs.join(' ');

    // words :: String -> [String]
    const words = s => s.split(/\s+/);


    // TEST ------------------------------------------------------------------
    return unlines(
        ap([
            reverseString,
            reverseEachWord,
            reverseWordOrder
        ], ["rosetta code phrase reversal"])
    );
})();
```

```txt
lasrever esarhp edoc attesor
attesor edoc esarhp lasrever
reversal phrase code rosetta
```



## jq

```jq
def reverse_string: explode | reverse | implode;

"rosetta code phrase reversal"
| split(" ") as $words
| "0. input:               \(.)",
  "1. string reversed:     \(reverse_string)",
  "2. each word reversed:  \($words | map(reverse_string) | join(" "))",
  "3. word-order reversed: \($words | reverse | join(" "))"
```

 $ jq -r -n -f Phrase_reversals.jq
 0. input:               rosetta code phrase reversal
 1. string reversed:     lasrever esarhp edoc attesor
 2. each word reversed:  attesor edoc esarhp lasrever
 3. word-order reversed: reversal phrase code rosetta


## Julia


```Julia

s = "rosetta code phrase reversal"

println("The original phrase.")
println("   ", s)

println("Reverse the string.")
t = reverse(s)
println("   ", t)

println("Reverse each individual word in the string.")
t = join(map(reverse, split(s, " ")), " ")
println("   ", t)

println("Reverse the order of each word of the phrase.")
t = join(reverse(split(s, " ")), " ")
println("   ", t)

```


```txt

The original phrase.
   rosetta code phrase reversal
Reverse the string.
   lasrever esarhp edoc attesor
Reverse each individual word in the string.
   attesor edoc esarhp lasrever
Reverse the order of each word of the phrase.
   reversal phrase code rosetta

```



## K


```K

/ Rosetta code phrase reversal
/ phraserev.k

reversestr: {|x}
getnxtwd: {c:(&" "~'x); if[c~!0;w::x;:""];w::c[0]#x; x: ((1+c[0]) _ x)}
revwords: {rw:""; while[~(x~""); x: getnxtwd x;rw,:|w;rw,:" "];:-1 _ rw}
revwordorder: {rw:""; while[~(x~""); x: getnxtwd x;rw:" ",rw;rw:w,rw];:-1 _ rw}


```

The output of a session is given below:
```txt

K Console - Enter \ for help

  \l phraserev
  phrase: "rosetta code phrase reversal"
"rosetta code phrase reversal"
  reversestr phrase
"lasrever esarhp edoc attesor"
  revwords phrase
"attesor edoc esarhp lasrever"
  revwordorder phrase
"reversal phrase code rosetta"


```



## Kotlin


```scala
// version 1.0.6

fun reverseEachWord(s: String) = s.split(" ").map { it.reversed() }.joinToString(" ")

fun main(args: Array<String>) {
    val original = "rosetta code phrase reversal"
    val reversed = original.reversed()
    println("Original string => $original")
    println("Reversed string => $reversed")
    println("Reversed words  => ${reverseEachWord(original)}")
    println("Reversed order  => ${reverseEachWord(reversed)}")
}
```


```txt

Original string => rosetta code phrase reversal
Reversed string => lasrever esarhp edoc attesor
Reversed words  => attesor edoc esarhp lasrever
Reversed order  => reversal phrase code rosetta

```



## Lua


```Lua
-- Return a copy of table t in which each string is reversed
function reverseEach (t)
    local rev = {}
    for k, v in pairs(t) do rev[k] = v:reverse() end
    return rev
end

-- Return a reversed copy of table t
function tabReverse (t)
    local revTab = {}
    for i, v in ipairs(t) do revTab[#t - i + 1] = v end
    return revTab
end

-- Split string str into a table on space characters
function wordSplit (str)
    local t = {}
    for word in str:gmatch("%S+") do table.insert(t, word) end
    return t
end

-- Main procedure
local str = "rosetta code phrase reversal"
local tab = wordSplit(str)
print("1. " .. str:reverse())
print("2. " .. table.concat(reverseEach(tab), " "))
print("3. " .. table.concat(tabReverse(tab), " "))
```

```txt
1. lasrever esarhp edoc attesor
2. attesor edoc esarhp lasrever
3. reversal phrase code rosetta
```



## Maple


```Maple
#reverse the string
str := "rosetta code phrase reversal":
print(StringTools:-Reverse(str)):
#reverse each word
lst := convert(StringTools:-Split(str, " "), Array):
for i to numelems(lst) do
	lst[i] := StringTools:-Reverse(lst[i]):
end do:
print(StringTools:-Join(convert(lst,list)," ")):
#reverse word order
print(StringTools:-Join(ListTools:-Reverse(StringTools:-Split(str," ")), " ")):
```

```txt
                 "lasrever esarhp edoc attesor"
                 "attesor edoc esarhp lasrever"
                 "reversal phrase code rosetta"
```



=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
phrase = "Rosetta Code Phrase Reversal";

reverseWords[phrase_String] :=
 StringJoin @@ Riffle[Reverse@StringSplit@phrase, " "]

reverseLetters[phrase_String] :=

 StringJoin @@
  Riffle[Map[StringJoin @@ Reverse[Characters@#] &,
    StringSplit@phrase], " "]

{phrase, reverseWords@phrase, reverseLetters@phrase,
  reverseWords@reverseLetters@phrase} // TableForm
```


```txt

Rosetta Code Phrase Reversal
Reversal Phrase Code Rosetta
attesoR edoC esarhP lasreveR
lasreveR esarhP edoC attesoR

```



## MiniScript


```MiniScript
phrase = "rosetta code phrase reversal"

// general sequence reversal function
reverse = function(seq)
    out = []
    for i in range(seq.len-1, 0)
        out.push seq[i]
    end for
    if seq isa string then return out.join("")
    return out
end function

// 1. Reverse the characters of the string.
print reverse(phrase)

// 2. Reverse the characters of each individual word in the string, maintaining original word order within the string.
words = phrase.split
for i in words.indexes
    words[i] = reverse(words[i])
end for
print words.join

// 3. Reverse the order of each word of the string, maintaining the order of characters in each word.
print reverse(phrase.split).join

```

```txt
lasrever esarhp edoc attesor
attesor edoc esarhp lasrever
reversal phrase code rosetta
```



## MUMPS


```MUMPS
set string="Rosetta Code Phrase Reversal"
set str="",len=$length(string," ")
for i=1:1:len set $piece(str," ",i)=$piece(string," ",len-i+1)
write string,!
write $reverse(string),!
write str,!
write $reverse(str),!
```


```txt
Rosetta Code Phrase Reversal
lasreveR esarhP edoC attesoR
Reversal Phrase Code Rosetta
attesoR edoC esarhP lasreveR

```



## Oforth



```Oforth
"rosetta code phrase reversal" reverse println
"rosetta code phrase reversal" words map(#reverse) unwords println
"rosetta code phrase reversal" words reverse unwords println
```


```txt

lasrever esarhp edoc attesor
attesor edoc esarhp lasrever
reversal phrase code rosetta

```



## Perl



```perl
use feature 'say';
my $s = "rosetta code phrase reversal";

say "0. Input               : ", $s;
say "1. String reversed     : ", scalar reverse $s;
say "2. Each word reversed  : ", join " ", reverse split / /, reverse $s;
say "3. Word-order reversed : ", join " ", reverse split / /,$s;

# Or, using a regex:
say "2. Each word reversed  : ", $s =~ s/[^ ]+/reverse $&/gre;
```


```txt

0. Input               : rosetta code phrase reversal
1. String reversed     : lasrever esarhp edoc attesor
2. Each word reversed  : attesor edoc esarhp lasrever
3. Word-order reversed : reversal phrase code rosetta
2. Each word reversed  : attesor edoc esarhp lasrever

```



## Perl 6


```perl6
my $s = 'rosetta code phrase reversal';

put 'Input               : ', $s;
put 'String reversed     : ', $s.flip;
put 'Each word reversed  : ', $s.words».flip;
put 'Word-order reversed : ', $s.words.reverse;
```

```txt
Input               : rosetta code phrase reversal
String reversed     : lasrever esarhp edoc attesor
Each word reversed  : attesor edoc esarhp lasrever
Word-order reversed : reversal phrase code rosetta
```



## Phix

I have assumed step 2 should be applied to the result of step 1, and step 3 applied to the result of step 2.

Obviously I'm right and everyone else is wrong!

```Phix
constant test="rosetta code phrase reversal"
?reverse(test)
sequence words = split(reverse(test))
for i=1 to length(words) do
    words[i] = reverse(words[i])
end for
?join(words)
?join(reverse(words))
```

```txt

"lasrever esarhp edoc attesor"
"reversal phrase code rosetta"
"rosetta code phrase reversal"

```



## PHP


```php
<?php
// Initialize a variable with the input desired
$strin = "rosetta code phrase reversal";

// Show user what original input was
echo "Input: ".$strin."\n";

// Show the full input reversed
echo "Reversed: ".strrev($strin)."\n";

// reverse the word letters in place
$str_words_reversed = "";
$temp = explode(" ", $strin);
foreach($temp as $word)
	$str_words_reversed .= strrev($word)." ";

// Show the reversed words in place
echo "Words reversed: ".$str_words_reversed."\n";


// reverse the word order while leaving the words in order
$str_word_order_reversed = "";
$temp = explode(" ", $strin);
for($i=(count($temp)-1); $i>=0; $i--)
	$str_word_order_reversed .= $temp[$i]." ";

// Show the reversal of the word order while leaving the words in order
echo "Word order reversed: ".$str_word_order_reversed."\n";

```



```txt
Input: rosetta code phrase reversal
Reversed: lasrever esarhp edoc attesor
Words reversed: attesor edoc esarhp lasrever
Word order reversed: reversal phrase code rosetta
```



## PicoLisp


```PicoLisp
(let (S (chop "rosetta code phrase reversal")  L (split S " "))
   (prinl (reverse S))
   (prinl (glue " " (mapcar reverse L)))
   (prinl (glue " " (reverse L))) )
```

Output:

```txt
lasrever esarhp edoc attesor
attesor edoc esarhp lasrever
reversal phrase code rosetta
```



## PL/I


```PL/I

reverser: procedure options (main);          /* 19 August 2015 */
   declare (phrase, r, word) character (100) varying;
   declare (start, end) fixed binary;

   phrase = 'rosetta code phrase reversal';
   put ('The original phrase is: ' || phrase);

   put skip list ( '1. ' || reverse(phrase) );

   start = 1; r = ''; put skip edit ('2. ') (a);
   do until ( end > length(phrase) );
      end = index(phrase, ' ', start);          /* Find end of the next word.*/
      if end = 0 then end = length(phrase) + 1; /* We're at the last word.   */
      word = substr(phrase, start, end-start);
      put edit ( reverse(word), ' ' ) (a);      /* Append reversed word.     */
      r = word || ' ' || r;                     /* Prepend normal word.      */
      start = end+1;
   end;
   put skip list ('3. ' || r);

end reverser;

```

Output:

```txt

The original phrase is: rosetta code phrase reversal
1. lasrever esarhp edoc attesor
2. attesor edoc esarhp lasrever
3. reversal phrase code rosetta

```



## plainTeX


```tex
\def\afterfi#1#2\fi{#2\fi#1}
\def\RevSingleWord#1{\RevSingleWordi{}#1\RSWA\RSWB\RSWB\RSWB\RSWB\RSWB\RSWB\RSWB\RSWB\RSWA}
\def\RevSingleWordi#1#2#3#4#5#6#7#8#9{\RSWgobtoB#9\RSWend\RSWB\RevSingleWordi{#9#8#7#6#5#4#3#2#1}}
\def\RSWend\RSWB\RevSingleWordi#1#2\RSWA{\RSWgobtoA#1}
\def\RSWgobtoA#1\RSWA{}\def\RSWgobtoB#1\RSWB{}
%---
\def\firstchartonil#1#2\nil{#1}
\def\RevOrderSameWords#1{\RevOrderSameWordsi{}#1 \. \* \* \* \* \* \* \* \* \.}
\def\RevOrderSameWordsi#1#2 #3 #4 #5 #6 #7 #8 #9 {%
	\expandafter\ifx\expandafter\*\firstchartonil#9\nil
	\expandafter\ROSWend\else\expandafter\RevOrderSameWordsi\fi{#9 #8 #7 #6 #5 #4 #3 #2#1}%
}
\def\ROSWend#1#2\.{\ROSWendi#1}
\def\ROSWendi#1\.{\romannumeral-`\-}
%---
\def\ROWquark{\ROWquark}
\def\RevOnlyWords#1{\edef\ROWtemp{\noexpand\RevOnlyWordsi{}#1 \noexpand\ROWquark\space}\ROWtemp}
\def\RevOnlyWordsi#1#2 {%
	\ifx\ROWquark#2\afterfi{\ROWgoblastspace#1\nil}%
	\else\afterfi{\RevOnlyWordsi{#1\RevSingleWord{#2} }}%
	\fi
}
\def\ROWgoblastspace#1 \nil{#1}
%---
\def\RevAll#1{\RevAlli{}#1 \. \* \* \* \* \* \* \* \* \.\:}
\def\RevAlli#1#2 #3 #4 #5 #6 #7 #8 #9 {%
	\expandafter\ifx\expandafter\*\firstchartonil#9\nil
	\expandafter\RAWend\else\expandafter\RevAlli\fi{#9 #8 #7 #6 #5 #4 #3 #2#1}%
}
\def\RAWend#1#2\.{\RAWendi#1}
\def\RAWendi#1\.{\expandafter\RAWendii\romannumeral-`\-}
\def\RAWendii#1\:{\RevOnlyWords{#1}}
%--
\halign{#\hfil: &#\cr
Initial&rosetta code phrase reversal\cr
Reverse all&\RevAll{rosetta code phrase reversal}\cr
Reverse order, same words&\RevOrderSameWords{rosetta code phrase reversal}\cr
Reverse only words&\RevOnlyWords{rosetta code phrase reversal}\cr\crcr}
\bye
```


pdf or dvi output looks like:

```txt
Initial                  : rosetta code phrase reversal
Reverse all              : lasrever esarhp edoc attesor
Reverse order, same words: reversal phrase code rosetta
Reverse only words       : attesor edoc esarhp lasrever

```



## PowerShell


```PowerShell

function reverse($a, $sep = "") {
    if($a.Length -gt 0) {
        $a = $a[($a.Length -1)..0] -join $sep
    }
    $a
}
$line = "rosetta code phrase reversal"
$task1 = reverse $line
$task2 = ($line -split " " | foreach{ reverse $_  }) -join " "
$task3 = reverse ($line -split " ") " "
$task1
$task2
$task3

```

<b>Output:</b>

```txt

lasrever esarhp edoc attesor
attesor edoc esarhp lasrever
reversal phrase code rosetta

```



## PureBasic


```PureBasic
#TEXT="rosetta code phrase reversal"

If OpenConsole("rosetta code phrase reversal")
  Define idx.i=1, txt.s=""

  Print(~"Original:\t\t")
  PrintN(#TEXT)

  Print(~"Reversed:\t\t")
  PrintN(ReverseString(#TEXT))

  Print(~"Reversed words:\t\t")
  txt=StringField(#TEXT,idx," ")
  While Len(txt)
    Print(ReverseString(txt)+" ")
    idx+1
    txt=StringField(#TEXT,idx," ")
  Wend
  PrintN("")

  Print(~"Reversed order:\t\t")
  idx-1
  txt=StringField(#TEXT,idx," ")
  While Len(txt)
    Print(txt+" ")
    If idx>1 : idx-1 : Else : Break : EndIf
    txt=StringField(#TEXT,idx," ")
  Wend

  Input()
EndIf
```

```txt
Original:               rosetta code phrase reversal
Reversed:               lasrever esarhp edoc attesor
Reversed words:         attesor edoc esarhp lasrever
Reversed order:         reversal phrase code rosetta
```



## Python

These examples use the [https://docs.python.org/2/whatsnew/2.3.html#extended-slices extended slicing] notation of <code>[::-1]</code> to reverse strings and lists of strings:

```python>>>
 phrase = "rosetta code phrase reversal"
>>> phrase[::-1]					  # Reversed.
'lasrever esarhp edoc attesor'
>>> ' '.join(word[::-1] for word in phrase.split())	  # Words reversed.
'attesor edoc esarhp lasrever'
>>> ' '.join(phrase.split()[::-1])	                  # Word order reversed.
'reversal phrase code rosetta'
>>>
```



Or, variously composing three reusable abstractions – '''reverse''', '''words''', and '''unwords''':


```python
'''String reversals at different levels.'''


# reversedCharacters :: String -> String
def reversedCharacters(s):
    '''All characters in reversed sequence.'''
    return reverse(s)


# wordsWithReversedCharacters :: String -> String
def wordsWithReversedCharacters(s):
    '''Characters within each word in reversed sequence.'''
    return unwords(map(reverse, words(s)))


# reversedWordOrder :: String -> String
def reversedWordOrder(s):
    '''Sequence of words reversed.'''
    return unwords(reverse(words(s)))


# TESTS -------------------------------------------------
# main :: IO()
def main():
    '''Tests'''

    s = 'rosetta code phrase reversal'
    print(
        tabulated(s + ':\n')(
            lambda f: f.__name__
        )(lambda s: "'" + s + "'")(
            lambda f: f(s)
        )([
            reversedCharacters,
            wordsWithReversedCharacters,
            reversedWordOrder
        ])
    )


# GENERIC -------------------------------------------------


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Function composition.'''
    return lambda f: lambda x: g(f(x))


# reverse :: [a] -> [a]
# reverse :: String -> String
def reverse(xs):
    '''The elements of xs in reverse order.'''
    return xs[::-1] if isinstance(xs, str) else (
        list(reversed(xs))
    )


# tabulated :: String -> (a -> String) ->
#                        (b -> String) ->
#                        (a -> b) -> [a] -> String
def tabulated(s):
    '''Heading -> x display function -> fx display function ->
                f -> value list -> tabular string.'''
    def go(xShow, fxShow, f, xs):
        w = max(map(compose(len)(xShow), xs))
        return s + '\n' + '\n'.join(
            xShow(x).rjust(w, ' ') + ' -> ' + fxShow(f(x)) for x in xs
        )
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# unwords :: [String] -> String
def unwords(xs):
    '''A space-separated string derived from a list of words.'''
    return ' '.join(xs)


# words :: String -> [String]
def words(s):
    '''A list of words delimited by characters
       representing white space.'''
    return s.split()


if __name__ == '__main__':
    main()
```

```txt
rosetta code phrase reversal:

         reversedCharacters -> 'lasrever esarhp edoc attesor'
wordsWithReversedCharacters -> 'attesor edoc esarhp lasrever'
          reversedWordOrder -> 'reversal phrase code rosetta'
```



## Racket



```racket
#lang racket/base
(require
  (only-in srfi/13 string-reverse)
  (only-in racket/string string-split string-join))

(define (phrase-reversal s)
  (list
   (string-reverse s)
   (string-join (map string-reverse (string-split s)))
   (string-join (reverse (string-split s)))))

(for-each displayln (phrase-reversal "rosetta code phrase reversal"))
```


```txt
lasrever esarhp edoc attesor
attesor edoc esarhp lasrever
reversal phrase code rosetta
```



## REXX


### version 1

Working with REXX (strings and words) is trivial.

```rexx
s='rosetta code phrase reversal'
r1=reverse(s)
r2=''
Do i=1 To words(s)
  r2=r2 reverse(word(s,i))
  End
r2=strip(r2)
r3=''
Do i=words(s) To 1 By -1
  r3=r3 word(s,i)
  End
r3=strip(r3)
Say "input               : " s
say "string reversed     : " r1
say "each word reversed  : " r2
say "word-order reversed : " r3
```

```txt
input               :  rosetta code phrase reversal
string reversed     :  lasrever esarhp edoc attesor
each word reversed  :  attesor edoc esarhp lasrever
word-order reversed :  reversal phrase code rosetta
```



### version 2


```rexx
/*REXX program reverses  words and also letters  in a string in various (several) ways. */
parse arg $                                      /*obtain optional arguments from the CL*/
if $=''  then $= "rosetta code phrase reversal"  /*Not specified?  Then use the default.*/
L=;  W=                                          /*initialize two REXX variables to null*/
           do j=1  for words($);   _= word($, j) /*extract each word in the  $  string. */
           L= L reverse(_);        W= _ W        /*reverse letters;  reverse words.     */
           end   /*j*/
say '   the original phrase used: '          $
say '   original phrase reversed: '  reverse($)
say '  reversed individual words: '    strip(L)
say '  reversed words in phrases: '          W   /*stick a fork in it,  we're all done. */
```

```txt

   the original phrase used:  rosetta code phrase reversal
   original phrase reversed:  lasrever esarhp edoc attesor
  reversed individual words:  attesor edoc esarhp lasrever
  reversed words in phrases:  reversal phrase code rosetta

```



## Ring


```ring

aString = "Welcome to the Ring Language"
bString = ""
see reverseString(aString)

func reverseString cString
     for i= len(cString) to 1 step -1
         bString = bString + cString[i]
     next
     return bString

```


Output:

```txt

egaugnaL gniR eht ot emocleW

```



## Ruby


```ruby
str = "rosetta code phrase reversal"

puts str.reverse                          # Reversed string.
puts str.split.map(&:reverse).join(" ")   # Words reversed.
puts str.split.reverse.join(" ")          # Word order reversed.
```


```txt

lasrever esarhp edoc attesor
attesor edoc esarhp lasrever
reversal phrase code rosetta

```



## Scala


```scala
object PhraseReversals extends App {
    val phrase = scala.io.StdIn.readLine
    println(phrase.reverse)
    println(phrase.split(' ').map(_.reverse).mkString(" "))
    println(phrase.split(' ').reverse.mkString(" "))
}
```


```txt

lasrever esarhp edoc attesor
attesor edoc esarhp lasrever
reversal phrase code rosetta

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var string: phrase is "rosetta code phrase reversal";
    var string: word is "";
    var array string: wordList is 0 times "";
  begin
    writeln("The original phrase:" rpad 27 <& phrase);
    writeln("Reverse the entire phrase:" rpad 27 <& reverse(phrase));
    for word range split(phrase, ' ') do
      wordList &:= reverse(word);
    end for;
    writeln("Reverse words, same order:" rpad 27 <& join(wordList, ' '));
    wordList := 0 times "";
    for word range split(phrase, ' ') do
      wordList := [] (word) & wordList;
    end for;
    writeln("Reverse order, same words:" rpad 27 <& join(wordList, ' '));
  end func;
```


```txt

The original phrase:       rosetta code phrase reversal
Reverse the entire phrase: lasrever esarhp edoc attesor
Reverse words, same order: attesor edoc esarhp lasrever
Reverse order, same words: reversal phrase code rosetta

```



## Sidef


```ruby
var str    = "rosetta code phrase reversal";

say str.reverse;                            # reversed string
say str.words.map{.reverse}.join(' ');      # words reversed
say str.words.reverse.join(' ');            # word order reversed
```

```txt
lasrever esarhp edoc attesor
attesor edoc esarhp lasrever
reversal phrase code rosetta
```


## Swift


```swift

func reverseString(s:String)->String{
    var temp = [Character]()
    for i in s.characters{
        temp.append(i)
    }
    var j=s.characters.count-1
    for i in s.characters{
        temp[j]=i
        j-=1
    }
    return String(temp)
}

func reverseWord(s:String)->String{
    var temp = [Character]()
    var result:String=""
    for i in s.characters{
        if i==" "{
            result += "\(reverseString(s:String(temp))) "
            temp=[Character]()
        }
        else {
            temp.append(i)
        }
        if i==s[s.index(before: s.endIndex)]{
            result += (reverseString(s:String(temp)))
        }
    }
    return result
}

func flipString(s:String)->String{
    return reverseWord(s:reverseString(s:s))
}
print(str)
print(reverseString(s:str))
print(reverseWord(s:str))
print(flipString(s:str))

```



## Tcl


```tcl
set s "rosetta code phrase reversal"
# Reverse all characters
puts [string reverse $s]
# Reverse characters in each word
puts [lmap word $s {string reverse $word}]
# Reverse the words but not the characters
puts [lreverse $s]
```

```txt

lasrever esarhp edoc attesor
attesor edoc esarhp lasrever
reversal phrase code rosetta

```



## UNIX Shell


### Version 1

Requires "rev" command.
```sh
s1="rosetta code phrase reversal"
echo "Original string ----------------------> "$s1

echo -n "1.) Reverse the string ---------------> "
echo $s1|rev

echo -n "2.) Reverse characters of each word --> "
echo $s1|tr " " "\n"|rev|tr "\n" " ";echo

echo -n "3.) Reverse word order ---------------> "
word_num=$(echo $s1|wc -w)
while [ $word_num != 0 ];do
echo -n $(echo $s1|cut -d " " -f $word_num);echo -n " "
word_num=$(expr $word_num - 1);done;echo
```


```txt

Original string ----------------------> rosetta code phrase reversal
1.) Reverse the string ---------------> lasrever esarhp edoc attesor
2.) Reverse characters of each word --> attesor edoc esarhp lasrever
3.) Reverse word order ---------------> reversal phrase code rosetta
```



### Version 2

Does not require "rev" command.
```sh
s1="rosetta code phrase reversal"
echo "Original string                     --> "$s1

echo -n "1.) Reverse the string              --> "
length=$(echo $s1|wc -c)
while [ $length != 0 ];do
echo $s1|cut -c$length|tr -d "\n"
length=$(expr $length - 1)
done;echo

echo -n "2.) Reverse characters of each word --> "
word_quantity=$(echo $s1|wc -w)
word_quantity=$(expr $word_quantity + 1)
word_num=1
while [ $word_num != $word_quantity ];do
length=$(echo $s1|cut -d " " -f $word_num|wc -c)
while [ $length != 0 ];do
echo $s1|cut -d " " -f $word_num|cut -c$length|tr -d "\n"
length=$(expr $length - 1);done;echo -n " "
word_num=$(expr $word_num + 1);done;echo

echo -n "3.) Reverse word order              --> "
word_num=$(echo $s1|wc -w)
while [ $word_num != 0 ];do
echo -n $(echo $s1|cut -d " " -f $word_num);echo -n " "
word_num=$(expr $word_num - 1);done;echo
```


```txt
Original string                     --> rosetta code phrase reversal
1.) Reverse the string              --> lasrever esarhp edoc attesor
2.) Reverse characters of each word --> attesor edoc esarhp lasrever
3.) Reverse word order              --> reversal phrase code rosetta
```



## VBA



```vb

Option Explicit

Sub Main_Phrase_Reversals()
Const PHRASE As String = "rosetta code phrase reversal"
    Debug.Print "Original String              : " & PHRASE
    Debug.Print "Reverse String               : " & Reverse_String(PHRASE)
    Debug.Print "Reverse each individual word : " & Reverse_each_individual_word(PHRASE)
    Debug.Print "Reverse order of each word   : " & Reverse_the_order_of_each_word(PHRASE)
End Sub

Function Reverse_String(strPhrase As String) As String
    Reverse_String = StrReverse(strPhrase)
End Function

Function Reverse_each_individual_word(strPhrase As String) As String
Dim Words, i&, strTemp$
    Words = Split(strPhrase, " ")
    For i = 0 To UBound(Words)
        Words(i) = Reverse_String(CStr(Words(i)))
    Next i
    Reverse_each_individual_word = Join(Words, " ")
End Function

Function Reverse_the_order_of_each_word(strPhrase As String) As String
Dim Words, i&, strTemp$

    Words = Split(strPhrase, " ")
    For i = UBound(Words) To 0 Step -1
        strTemp = strTemp & " " & Words(i)
    Next i
    Reverse_the_order_of_each_word = Trim(strTemp)
End Function

```

```txt
Original String              : rosetta code phrase reversal
Reverse String               : lasrever esarhp edoc attesor
Reverse each individual word : attesor edoc esarhp lasrever
Reverse order of each word   : reversal phrase code rosetta
```



## VBScript


```vb

Phrase = "rosetta code phrase reversal"

WScript.StdOut.Write "Original String          : " & Phrase
WScript.StdOut.WriteLine
WScript.StdOut.Write "Reverse String           : " & RevString(Phrase)
WScript.StdOut.WriteLine
WScript.StdOut.Write "Reverse String Each Word : " & RevStringEachWord(Phrase)
WScript.StdOut.WriteLine
WScript.StdOut.Write "Reverse Phrase           : " & RevPhrase(Phrase)
WScript.StdOut.WriteLine

Function RevString(s)
	x = Len(s)
	For i = 1 To Len(s)
		RevString = RevString & Mid(s,x,1)
		x = x - 1
	Next
End Function

Function RevStringEachWord(s)
	arr = Split(s," ")
	For i = 0 To UBound(arr)
		RevStringEachWord = RevStringEachWord & RevString(arr(i))
		If i < UBound(arr) Then
			RevStringEachWord = RevStringEachWord & " "
		End If
	Next
End Function

Function RevPhrase(s)
	arr = Split(s," ")
	For i = UBound(arr) To LBound(arr) Step -1
		RevPhrase = RevPhrase & arr(i)
		If i > LBound(arr) Then
			RevPhrase = RevPhrase & " "
		End If
	Next
End Function

```

```txt

Original String          : rosetta code phrase reversal
Reverse String           : lasrever esarhp edoc attesor
Reverse String Each Word : attesor edoc esarhp lasrever
Reverse Phrase           : reversal phrase code rosetta

```



## Yabasic

```Yabasic
phrase$ = "Rosetta Code Phrase Reversal"

dim word$(1)

n = token(phrase$, word$())

print phrase$

for i = n to 1 step -1
    print reverse$(word$(i)), " ";
next

print

for i = n to 1 step -1
    print word$(i), " ";
next

print

for i = 1 to n
    print reverse$(word$(i)), " ";
next

print

sub reverse$(w$)
    local i, rw$

    for i = len(w$) to 1 step -1
        rw$ = rw$ + mid$(w$, i, 1)
    next

    return rw$
end sub
```



## zkl


```zkl
zkl: var str="rosetta code phrase reversal"
rosetta code phrase reversal

zkl: str.reverse() #1
lasrever esarhp edoc attesor

zkl: str.split().apply("reverse").concat(" ") #2 string to list to string
attesor edoc esarhp lasrever

zkl: str.split().reverse().concat(" ") #3
reversal phrase code rosetta
```

