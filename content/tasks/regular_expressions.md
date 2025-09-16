+++
title = "Regular expressions"
description = ""
date = 2019-09-04T23:42:04Z
aliases = []
[extra]
id = 1691
[taxonomies]
categories = ["task", "Text processing"]
tags = []
languages = [
  "8th",
  "abap",
  "ada",
  "algol_68",
  "applescript",
  "argile",
  "autohotkey",
  "awk",
  "bbc_basic",
  "bracmat",
  "brat",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dart",
  "elixir",
  "emacs_lisp",
  "erlang",
  "factor",
  "forth",
  "frink",
  "gambas",
  "genexus",
  "genie",
  "go",
  "groovy",
  "haskell",
  "hicest",
  "inform_7",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "kotlin",
  "langur",
  "lasso",
  "lua",
  "m2000_interpreter",
  "m4",
  "maple",
  "mathematica",
  "maxscript",
  "mirc_scripting_language",
  "mumps",
  "netrexx",
  "newlisp",
  "nim",
  "objeck",
  "ocaml",
  "ol",
  "oorexx",
  "oxygene",
  "oz",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "powershell",
  "purebasic",
  "python",
  "r",
  "racket",
  "raven",
  "rebol",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "sather",
  "scala",
  "shiny",
  "sidef",
  "slate",
  "smalltalk",
  "snobol4",
  "standard_ml",
  "stata",
  "swift",
  "tcl",
  "toka",
  "txr",
  "unix_shell",
  "vala",
  "vbscript",
  "vedit_macro_language",
  "web_68",
  "zkl",
]
+++

## Task

:*   match a string against a regular expression
:*   substitute part of a string using a regular expression





## 8th


```forth

"haystack" /a./ r:match . cr
"haystack" /a./ "blah" s:replace! . cr

```

```txt

1
hblahstblahk

```


## ABAP



```ABAP

DATA: text TYPE string VALUE 'This is a Test'.

FIND FIRST OCCURRENCE OF REGEX 'is' IN text.
IF sy-subrc = 0.
  cl_demo_output=>write( 'Regex matched' ).
ENDIF.

REPLACE ALL OCCURRENCES OF REGEX '[t|T]est' IN text WITH 'Regex'.

cl_demo_output=>write( text ).
cl_demo_output=>display( ).

```


Output:

```txt

Regex matched

This is a Regex

```



## Ada

There is no Regular Expression library in the Ada Standard,
so I am using one of the libraries provided by gnat/gcc.

```ada
with Ada.Text_IO; with Gnat.Regpat; use Ada.Text_IO;

procedure Regex is

   package Pat renames Gnat.Regpat;

   procedure Search_For_Pattern(Compiled_Expression: Pat.Pattern_Matcher;
                                Search_In: String;
                                First, Last: out Positive;
                                Found: out Boolean) is
      Result: Pat.Match_Array (0 .. 1);
   begin
      Pat.Match(Compiled_Expression, Search_In, Result);
      Found := not Pat."="(Result(1), Pat.No_Match);
      if Found then
         First := Result(1).First;
         Last := Result(1).Last;
      end if;
   end Search_For_Pattern;

   Word_Pattern: constant String := "([a-zA-Z]+)";

   Str:           String:= "I love PATTERN matching!";
   Current_First: Positive := Str'First;
   First, Last:   Positive;
   Found:         Boolean;

begin
   -- first, find all the words in Str
   loop
      Search_For_Pattern(Pat.Compile(Word_Pattern),
                         Str(Current_First .. Str'Last),
                         First, Last, Found);
   exit when not Found;
      Put_Line("<" & Str(First .. Last) & ">");
      Current_First := Last+1;
   end loop;

   -- second, replace "PATTERN" in Str by "pattern"
   Search_For_Pattern(Pat.Compile("(PATTERN)"), Str, First, Last, Found);
   Str := Str(Str'First .. First-1) & "pattern" & Str(Last+1 .. Str'Last);
   Put_Line(Str);
end Regex;
```


```txt
<I>
<love>
<PATTERN>
<matching>
I love pattern matching!
```



## AppleScript

```applescript
try
    find text ".*string$" in "I am a string" with regexp
on error message
    return message
end try

try
    change "original" into "modified" in "I am the original string" with regexp
on error message
    return message
end try
```


```txt


```




## ALGOL 68

The routines ''grep in strings'' and ''sub in string''
are not part of [[ALGOL 68]]'s standard prelude.

<!-- {{does not work with|ALGOL 68|Standard - grep/sub in string are not part of the standard's prelude. }} -->
```algol68
INT match=0, no match=1, out of memory error=2, other error=3;

STRING str := "i am a string";

# Match: #

STRING m := "string$";
INT start, end;
IF grep in string(m, str, start, end) = match THEN printf(($"Ends with """g""""l$, str[start:end])) FI;

# Replace: #

IF sub in string(" a ", " another ",str) = match THEN printf(($gl$, str)) FI;
```

```txt

Ends with "string"
i am another string

```


Standard ALGOL 68 does have an primordial form of pattern matching
called a '''format'''.
This is designed to extract values from input data.
But it can also be used for outputting (and transputting) the original data.

For example:
```algol68
FORMAT pattern = $ddd" "c("cats","dogs")$;
FILE file; STRING book; associate(file, book);
on value error(file, (REF FILE f)BOOL: stop);
on format error(file, (REF FILE f)BOOL: stop);

book := "100 dogs";
STRUCT(INT count, type) dalmatians;

getf(file, (pattern, dalmatians));
print(("Dalmatians: ", dalmatians, new line));
count OF dalmatians +:=1;
printf(($"Gives: "$, pattern, dalmatians, $l$))
```

```txt

Dalmatians:        +100         +2
Gives 101 dogs

```



## Argile


```Argile
use std, regex

(: matching :)
if "some matchable string" =~ /^some" "+[a-z]*" "+string$/
  echo string matches
else
  echo string "doesn't" match

(: replacing :)
let t = strdup "some allocated string"
t =~ s/a/"4"/g
t =~ s/e/"3"/g
t =~ s/i/"1"/g
t =~ s/o/"0"/g
t =~ s/s/$/g
print t
free t

(: flushing regex allocations :)
uninit regex

check mem leak; use dbg (:optional:)
```


(note that it needs to be compiled with argrt library)

```txt

string matches
$0m3 4ll0c4t3d $tr1ng

```



## AutoHotkey


```AutoHotkey
MsgBox % foundpos := RegExMatch("Hello World", "World$")
MsgBox % replaced := RegExReplace("Hello World", "World$", "yourself")
```



## AWK

AWK supports regular expressions, which are typically enclosed using slash symbols at the front and back, and the tilde regular expression binding operator:

```awk
$ awk '{if($0~/[A-Z]/)print "uppercase detected"}'
abc
ABC
uppercase detected
```

As shorthand, a regular expression in the condition part fires if it matches an input line:

```awk
awk '/[A-Z]/{print "uppercase detected"}'
def
DeF
uppercase detected
```

For substitution, the first argument can be a regular expression, while the replacement string is constant (only that '&' in it receives the value of the match):

```awk
$ awk '{gsub(/[A-Z]/,"*");print}'
abCDefG
ab**ef*
$ awk '{gsub(/[A-Z]/,"(&)");print}'
abCDefGH
ab(C)(D)ef(G)(H)
```

This variant matches one or more uppercase letters in one round:

```awk
$ awk '{gsub(/[A-Z]+/,"(&)");print}'
abCDefGH
ab(CD)ef(GH)
```


Regular expression negation can be achieved by combining the regular expression binding operator with a logical not operator, as follows:

if (text !~ /strawberry/) {
  print "Match not found"
}


## BBC BASIC

Uses the [http://people.delphiforums.com/gjc/gnu_regex.html gnu_regex] library.

```bbcbasic
      SYS "LoadLibrary", "gnu_regex.dll" TO gnu_regex%
      IF gnu_regex% = 0 ERROR 100, "Cannot load gnu_regex.dll"
      SYS "GetProcAddress", gnu_regex%, "regcomp" TO regcomp
      SYS "GetProcAddress", gnu_regex%, "regexec" TO regexec

      DIM regmatch{start%, finish%}, buffer% 256

      REM Find all 'words' in a string:
      teststr$ = "I love PATTERN matching!"
      pattern$ = "([a-zA-Z]+)"

      SYS regcomp, buffer%, pattern$, 1 TO result%
      IF result% ERROR 101, "Failed to compile regular expression"

      first% = 1
      REPEAT
        SYS regexec, buffer%, MID$(teststr$, first%), 1, regmatch{}, 0 TO result%
        IF result% = 0 THEN
          s% = regmatch.start%
          f% = regmatch.finish%
          PRINT "<" MID$(teststr$, first%+s%, f%-s%) ">"
          first% += f%
        ENDIF
      UNTIL result%

      REM Replace 'PATTERN' with 'pattern':
      teststr$ = "I love PATTERN matching!"
      pattern$ = "(PATTERN)"

      SYS regcomp, buffer%, pattern$, 1 TO result%
      IF result% ERROR 101, "Failed to compile regular expression"
      SYS regexec, buffer%, teststr$, 1, regmatch{}, 0 TO result%
      IF result% = 0 THEN
        s% = regmatch.start%
        f% = regmatch.finish%
        MID$(teststr$, s%+1, f%-s%) = "pattern"
        PRINT teststr$
      ENDIF

      SYS "FreeLibrary", gnu_regex%
```

```txt

<I>
<love>
<PATTERN>
<matching>
I love pattern matching!

```



## Bracmat

Pattern matching in Bracmat is inspired by pattern matching in Snobol.
It also is quite different from regular expressions:
* Patterns in Bracmat are not greedy
* It is not possible to replace substrings, because values can never be changed
* Patterns always must match all of the subject
* Strings as well as complex data can be subjected to pattern matching

List all rational numbers smaller then 7 hidden in the string "fgsakg789/35768685432fkgha"

```bracmat
@("fesylk789/35768poq2art":? (#<7:?n & out$!n & ~) ?)
```

```txt
789/357
789/3576
789/35768
89/35
89/357
89/3576
89/35768
9/3
9/35
9/357
9/3576
9/35768
3
5
6
2
```


After the last number, the match expression fails.


## Brat


Test


```brat
str = "I am a string"

true? str.match(/string$/)
 { p "Ends with 'string'" }

false? str.match(/^You/)
 { p "Does not start with 'You'" }

```


Substitute


```brat
# Substitute in copy

str2 = str.sub(/ a /, " another ")

p str    # original unchanged
p str2   # prints "I am another string"

# Substitute in place

str.sub!(/ a /, " another ")

p str    # prints "I am another string"

# Substitute with a block

str.sub! /a/
 { match | match.upcase }

p str    # prints "I Am Another string"

```



## C

As far as I can see, POSIX defined function for regex matching, but nothing for substitution. So we must do all the hard work ''by hand''. The complex-appearing code could be turned into a function.


```c
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <regex.h>
#include <string.h>

int main()
{
   regex_t preg;
   regmatch_t substmatch[1];
   const char *tp = "string$";
   const char *t1 = "this is a matching string";
   const char *t2 = "this is not a matching string!";
   const char *ss = "istyfied";

   regcomp(&preg, "string$", REG_EXTENDED);
   printf("'%s' %smatched with '%s'\n", t1,
                                        (regexec(&preg, t1, 0, NULL, 0)==0) ? "" : "did not ", tp);
   printf("'%s' %smatched with '%s'\n", t2,
                                        (regexec(&preg, t2, 0, NULL, 0)==0) ? "" : "did not ", tp);
   regfree(&preg);
   /* change "a[a-z]+" into "istifyed"?*/
   regcomp(&preg, "a[a-z]+", REG_EXTENDED);
   if ( regexec(&preg, t1, 1, substmatch, 0) == 0 )
   {
      //fprintf(stderr, "%d, %d\n", substmatch[0].rm_so, substmatch[0].rm_eo);
      char *ns = malloc(substmatch[0].rm_so + 1 + strlen(ss) +
                        (strlen(t1) - substmatch[0].rm_eo) + 2);
      memcpy(ns, t1, substmatch[0].rm_so+1);
      memcpy(&ns[substmatch[0].rm_so], ss, strlen(ss));
      memcpy(&ns[substmatch[0].rm_so+strlen(ss)], &t1[substmatch[0].rm_eo],
                strlen(&t1[substmatch[0].rm_eo]));
      ns[ substmatch[0].rm_so + strlen(ss) +
          strlen(&t1[substmatch[0].rm_eo]) ] = 0;
      printf("mod string: '%s'\n", ns);
      free(ns);
   } else {
      printf("the string '%s' is the same: no matching!\n", t1);
   }
   regfree(&preg);

   return 0;
}
```



## C++

```cpp
#include <iostream>
#include <string>
#include <iterator>
#include <boost/regex.hpp>

int main()
{
  boost::regex re(".* string$");
  std::string s = "Hi, I am a string";

  // match the complete string
  if (boost::regex_match(s, re))
    std::cout << "The string matches.\n";
  else
    std::cout << "Oops - not found?\n";

  // match a substring
  boost::regex re2(" a.*a");
  boost::smatch match;
  if (boost::regex_search(s, match, re2))
  {
    std::cout << "Matched " << match.length()
              << " characters starting at " << match.position() << ".\n";
    std::cout << "Matched character sequence: \""
              << match.str() << "\"\n";
  }
  else
  {
    std::cout << "Oops - not found?\n";
  }

  // replace a substring
  std::string dest_string;
  boost::regex_replace(std::back_inserter(dest_string),
                       s.begin(), s.end(),
                       re2,
                       "'m now a changed");
  std::cout << dest_string << std::endl;
}
```



## C#


```c#
using System;
using System.Text.RegularExpressions;

class Program {
    static void Main(string[] args) {
        string str = "I am a string";

        if (new Regex("string$").IsMatch(str)) {
            Console.WriteLine("Ends with string.");
        }

        str = new Regex(" a ").Replace(str, " another ");
        Console.WriteLine(str);
    }
}
```



## Clojure


```clojure
(let [s "I am a string"]
  ;; match
  (when (re-find #"string$" s)
    (println "Ends with 'string'."))
  (when-not (re-find #"^You" s)
    (println "Does not start with 'You'."))

  ;; substitute
  (println (clojure.string/replace s " a " " another "))
)
```



## Common Lisp

Uses [http://weitz.de/cl-ppcre/ CL-PPCRE - Portable Perl-compatible regular expressions for Common Lisp].


```lisp
(let ((string "I am a string"))
  (when (cl-ppcre:scan "string$" string)
    (write-line "Ends with string"))
  (unless (cl-ppcre:scan "^You" string )
    (write-line "Does not start with 'You'")))
```


Substitute


```lisp
(let* ((string "I am a string")
       (string (cl-ppcre:regex-replace " a " string " another ")))
  (write-line string))
```


Test and Substitute


```lisp
(let ((string "I am a string"))
  (multiple-value-bind (string matchp)
      (cl-ppcre:regex-replace "\\bam\\b" string "was")
    (when matchp
      (write-line "I was able to find and replace 'am' with 'was'."))))
```



### CLISP regexp engine

[[Clisp]] comes with [http://clisp.org/impnotes/regexp-mod.html built-in regexp matcher].  On a Clisp prompt:

```lisp
[1]> (regexp:match "fox" "quick fox jumps")
#S(REGEXP:MATCH :START 6 :END 9)
```

To find all matches, loop with different <code>:start</code> keyword.

Replacing text can be done with the help of <code>REGEXP:REGEXP-SPLIT</code> function:

```lisp
[2]> (defun regexp-replace (pat repl string)
  (reduce #'(lambda (x y) (string-concat x repl y))
          (regexp:regexp-split pat string)))
REGEXP-REPLACE
[3]> (regexp-replace "x\\b" "-X-" "quick foxx jumps")
"quick fox-X- jumps"
```



## D


```d
void main() {
    import std.stdio, std.regex;

    immutable s = "I am a string";

    // Test.
    if (s.match("string$"))
        "Ends with 'string'.".writeln;

    // Substitute.
    s.replace(" a ".regex, " another ").writeln;
}
```

```txt
Ends with 'string'.
I am another string
```

In std.string there are string functions to perform the same operations more efficiently.


## Dart


```d
RegExp regexp = new RegExp(r'\w+\!');

String capitalize(Match m) => '${m[0].substring(0, m[0].length-1).toUpperCase()}';

void main(){
  String hello = 'hello hello! world world!';
  String hellomodified = hello.replaceAllMapped(regexp, capitalize);
  print(hello);
  print(hellomodified);
}
```

```txt
hello hello! world world!
hello HELLO world WORLD
```



## Elixir

Elixir allows pattern matching using the <code>~r</code> sigil.

```Elixir

str = "This is a string"
if str =~ ~r/string$/, do: IO.inspect "str ends with 'string'"

```

A number of modifiers can be appended to the regular expression; <code>~r/pattern/i</code>, for instance, toggles case insensitivity.

```Elixir

str =~ ~r/this/  # => false
str =~ ~r/this/i # => true

```

Both <code>Regex</code> and <code>String</code> have a <code>replace</code> function.

```Elixir

str1 = ~r/a/ |> Regex.replace(str,"another")
str2 = str1 |> String.replace(~r/another/,"even another")

```

<code>Regex.replace</code> allows for a function to be used as a replacement value. A function can modify the found pattern.

```Elixir

str3 = ~r/another/ |> Regex.replace(str2, fn x -> "#{String.upcase(x)}" end)

```


str ends with 'string'

false

true

"This is another string"

"This is even another string"

"This is even ANOTHER string"


## Emacs Lisp


```Emacs Lisp

(defun match (word str)
   (setq pos (string-match word str) )
  (if pos
      (progn
	(insert (format "%s found at position %d in: %s\n" word pos str) )
	(setq regex (format "^.+%s" word) )
	(setq str (replace-regexp-in-string regex (format "left %s" word) str) )
	(setq regex (format "%s.+$" word) )
	(setq str (replace-regexp-in-string regex (format "%s right" word) str) )
	(insert (format "result: %s\n"  str) ))
    (insert (format "%s not found in: %s\n" word str) )))

(setq str1 "before center after" str2 "before centre after")

(progn
  (match "center" str1)
  (insert "\n")
  (match "center" str2) )

```

<b>Output:</b>

```txt

center found at position 7 in: before center after
result: left center right

center not found in: before centre after

```



## Erlang


```erlang
match() ->
	String = "This is a string",
	case re:run(String, "string$") of
		{match,_} -> io:format("Ends with 'string'~n");
		_ -> ok
	end.

substitute() ->
	String = "This is a string",
	NewString = re:replace(String, " a ", " another ", [{return, list}]),
	io:format("~s~n",[NewString]).
```



=={{header|F_Sharp|F#}}==
```fsharp
open System
open System.Text.RegularExpressions

[<EntryPoint>]
let main argv =
    let str = "I am a string"
    if Regex("string$").IsMatch(str) then Console.WriteLine("Ends with string.")

    let rstr = Regex(" a ").Replace(str, " another ")
    Console.WriteLine(rstr)
    0
```



## Factor


```factor
USING: io kernel prettyprint regexp ;
IN: rosetta-code.regexp

"1000000" R/ 10+/ matches? .     ! Does the entire string match the regexp?
"1001"    R/ 10+/ matches? .
"1001"    R/ 10+/ re-contains? . ! Does the string contain the regexp anywhere?

"blueberry pie" R/ \p{alpha}+berry/ "pumpkin" re-replace print
```

```txt

t
f
t
pumpkin pie

```



## Forth

Test/Match

```forth
include ffl/rgx.fs

\ Create a regular expression variable 'exp' in the dictionary

rgx-create exp

\ Compile an expression

s" Hello (World)" exp rgx-compile [IF]
  .( Regular expression successful compiled.) cr
[THEN]

\ (Case sensitive) match a string with the expression

s" Hello World" exp rgx-cmatch? [IF]
  .( String matches with the expression.) cr
[ELSE]
  .( No match.) cr
[THEN]
```




## Frink

Pattern matching:

```frink

line = "My name is Inigo Montoya."

for [first, last] = line =~ %r/my name is (\w+) (\w+)/ig
{
   println["First name is: $first"]
   println["Last name is: $last"]
}

```


Replacement:  (Replaces in the variable <code>line</code>)

```frink

line =~ %s/Frank/Frink/g

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=5a2a1052f8dff12596fa7f45242d25a9 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sString As String = "Hello world!"

If sString Ends "!" Then Print sString & " ends with !"
If sString Begins "Hel" Then Print sString & " begins with 'Hel'"

sString = Replace(sString, "world", "moon")

Print sString

End
```

Output:

```txt

Hello world! ends with !
Hello world! begins with 'Hel'
Hello moon!

```



## GeneXus

Interesting link: http://wiki.gxtechnical.com/commwiki/servlet/hwiki?Regular+Expressions+%28RegEx%29,<br />
<br />
Replacement:<br />

```genexus
&string = &string.ReplaceRegEx("^\s+|\s+$", "") // it's a trim!
&string = &string.ReplaceRegEx("Another (Match)", "Replacing $1") // Using replace groups
```

Check match:

```genexus
If (&string.IsMatch("regex$"))
    // The string ends with "regex"
EndIf
```

Split RegEx:

```genexus
&stringCollection = &string.SplitRegEx("^\d{2,4}")
```

Matches:

```genexus
&RegExMatchCollection = &string.Matches("(pa)tt(ern)")
For &RegExMatch In &RegExMatchCollection
    &FullMatch = &RegExMatch.Value // &FullMatch contains the full pattern match: "pattern"
    For &matchVarchar In &RegExMatch.Groups
        // &matchVarchar contains group matches: "pa", "ern"
    EndFor
EndFor
```

Flags: <br />
s - Dot matches all (including newline) <br />
m - multiline <br />
i - ignore case <br />
Using Flags Sintax: (?flags)pattern <br />
Example:<br />

```genexus
&string = &string.ReplaceRegEx("(?si)IgnoreCase.+$", "") // Flags s and i
```

Error Handling:

```genexus
&string = "abc"
&RegExMatchCollection = &string.Matches("[z-a]") // invalid pattern: z-a
&errCode = RegEx.GetLastErrCode() // returns 0 if no error and 1 if an error has occured
&errDsc = RegEx.GetLastErrDescription()
```



## Genie


```genie
[indent=4]
/* Regular expressions, in Genie */

init
    var sentence = "This is a sample sentence."
    try
        var re = new Regex("s[ai]mple")

        if re.match(sentence)
            print "matched '%s' in '%s'", re.get_pattern(), sentence

        var offs = 0
        print("replace with 'different': %s",
            re.replace(sentence, sentence.length, offs, "different"))

    except err:RegexError
        print err.message
```


```txt
prompt$ valac regularExpressions.gs
prompt$ ./regularExpressions
matched 's[ai]mple' in 'This is a sample sentence.'
replace with 'different': This is a different sentence.
```



## Go


```go
package main
import "fmt"
import "regexp"

func main() {
  str := "I am the original string"

  // Test
  matched, _ := regexp.MatchString(".*string$", str)
  if matched { fmt.Println("ends with 'string'") }

  // Substitute
  pattern := regexp.MustCompile("original")
  result := pattern.ReplaceAllString(str, "modified")
  fmt.Println(result)
}
```



## Groovy

"Matching" Solution (it's complicated):

```groovy
import java.util.regex.*;

def woodchuck = "How much wood would a woodchuck chuck if a woodchuck could chuck wood?"
def pepper = "Peter Piper picked a peck of pickled peppers"


println "=== Regular-expression String syntax (/string/) ==="
def woodRE = /[Ww]o\w+d/
def piperRE = /[Pp]\w+r/
assert woodRE instanceof String && piperRE instanceof String
assert (/[Ww]o\w+d/ == "[Ww]o\\w+d") && (/[Pp]\w+r/ == "[Pp]\\w+r")
println ([woodRE: woodRE, piperRE: piperRE])
println ()


println "=== Pattern (~) operator ==="
def woodPat = ~/[Ww]o\w+d/
def piperPat = ~piperRE
assert woodPat instanceof Pattern && piperPat instanceof Pattern

def woodList = woodchuck.split().grep(woodPat)
println ([exactTokenMatches: woodList])
println ([exactTokenMatches: pepper.split().grep(piperPat)])
println ()


println "=== Matcher (=~) operator ==="
def wwMatcher = (woodchuck =~ woodRE)
def ppMatcher = (pepper =~ /[Pp]\w+r/)
def wpMatcher = (woodchuck =~ /[Pp]\w+r/)
assert wwMatcher instanceof Matcher && ppMatcher instanceof Matcher
assert wwMatcher.toString() == woodPat.matcher(woodchuck).toString()
assert ppMatcher.toString() == piperPat.matcher(pepper).toString()
assert wpMatcher.toString() == piperPat.matcher(woodchuck).toString()

println ([ substringMatches: wwMatcher.collect { it }])
println ([ substringMatches: ppMatcher.collect { it }])
println ([ substringMatches: wpMatcher.collect { it }])
println ()


println "=== Exact Match (==~) operator ==="
def containsWoodRE = /.*/ + woodRE + /.*/
def containsPiperRE = /.*/ + piperRE + /.*/
def wwMatches = (woodchuck ==~ containsWoodRE)
assert wwMatches instanceof Boolean
def wwNotMatches = ! (woodchuck ==~ woodRE)
def ppMatches = (pepper ==~ containsPiperRE)
def pwNotMatches = ! (pepper ==~ containsWoodRE)
def wpNotMatches = ! (woodchuck ==~ containsPiperRE)
assert wwMatches && wwNotMatches && ppMatches && pwNotMatches && pwNotMatches

println ("'${woodchuck}' ${wwNotMatches ? 'does not' : 'does'} match '${woodRE}' exactly")
println ("'${woodchuck}' ${wwMatches ? 'does' : 'does not'} match '${containsWoodRE}' exactly")
```


```txt
=== Regular-expression String syntax (/string/)===
[woodRE:[Ww]o\w+d, piperRE:[Pp]\w+r]

=== Pattern (~) operator ===
[exactTokenMatches:[wood, would]]
[exactTokenMatches:[Peter, Piper]]

=== Matcher (=~) operator ===
[substringMatches:[wood, would, wood, wood, wood]]
[substringMatches:[Peter, Piper, pepper]]
[substringMatches:[]]

=== Exact Match (==~) operator ===
'How much wood would a woodchuck chuck if a woodchuck could chuck wood?' does not match '[Ww]o\w+d' exactly
'How much wood would a woodchuck chuck if a woodchuck could chuck wood?' does match '.*[Ww]o\w+d.*' exactly
```


Replacement Solution (String.replaceAll()):

```groovy
println woodchuck.replaceAll(/c\w+k/, "CHUCK")
```


```txt
How much wood would a woodCHUCK CHUCK if a woodCHUCK could CHUCK wood?
```


Reusable Replacement Solution (Matcher.replaceAll()):

```groovy
def ck = (woodchuck =~ /c\w+k/)
println (ck.replaceAll("CHUCK"))
println (ck.replaceAll("wind"))
println (ck.replaceAll("pile"))
println (ck.replaceAll("craft"))
println (ck.replaceAll("block"))
println (ck.replaceAll("row"))
println (ck.replaceAll("shed"))
println (ck.replaceAll("man"))
println (ck.replaceAll("work"))
println (ck.replaceAll("pickle"))
```


```txt
How much wood would a woodCHUCK CHUCK if a woodCHUCK could CHUCK wood?
How much wood would a woodwind wind if a woodwind could wind wood?
How much wood would a woodpile pile if a woodpile could pile wood?
How much wood would a woodcraft craft if a woodcraft could craft wood?
How much wood would a woodblock block if a woodblock could block wood?
How much wood would a woodrow row if a woodrow could row wood?
How much wood would a woodshed shed if a woodshed could shed wood?
How much wood would a woodman man if a woodman could man wood?
How much wood would a woodwork work if a woodwork could work wood?
How much wood would a woodpickle pickle if a woodpickle could pickle wood?
```



## Haskell

Test

```haskell
import Text.Regex

str = "I am a string"

case matchRegex (mkRegex ".*string$") str of
  Just _  -> putStrLn $ "ends with 'string'"
  Nothing -> return ()
```


Substitute

```haskell
import Text.Regex

orig = "I am the original string"
result = subRegex (mkRegex "original") orig "modified"
putStrLn $ result
```



## HicEst


```hicest
CHARACTER string*100/ "The quick brown fox jumps over the lazy dog" /
REAL, PARAMETER :: Regex=128, Count=256

characters_a_m = INDEX(string, "[a-m]", Regex+Count) ! counts 16

vocals_changed = EDIT(Text=string, Option=Regex, Right="[aeiou]", RePLaceby='**', DO=LEN(string) ) ! changes 11
WRITE(ClipBoard) string ! Th** q****ck br**wn f**x j**mps **v**r th** l**zy d**g
```


=={{header|Icon}} and {{header|Unicon}}==
Regex includes procedures to provide access to regular expressions within native string scanning and matching expressions.
'ReFind' and 'ReMatch' respectively generate the sequence of beginning and ending positions matched by a regular expression.
Additionally, there is a regular expression pattern compiler 'RePat' and other supporting functions and variables.


```Icon
procedure main()

s := "A simple string"
p := "string$"                  # regular expression

s ? write(image(s),if ReFind(p) then " matches " else " doesn't match ",image(p))

s[j := ReFind(p,s):ReMatch(p,s,j)] := "replacement"
write(image(s))
end

link regexp   # link to IPL regexp
```

[http://www.cs.arizona.edu/icon/library/procs/regexp.htm See regexp].

```txt
"A simple string" matches "string$"
"A simple replacement"
```



## Inform 7

Inform's regex support is similar to Perl's but with some limitations: angle brackets are used instead of square brackets, there is no multiline mode, several control characters and character classes are omitted, and backtracking is slightly less powerful.


```inform7
let T be indexed text;
let T be "A simple string";
if T matches the regular expression ".*string$", say "ends with string.";
replace the regular expression "simple" in T with "replacement";
```



## J


J's regex support is built on top of PCRE.


```j
load'regex'               NB.  Load regex library
str =: 'I am a string'   NB.  String used in examples.
```


Matching:


```j
   '.*string$' rxeq str     NB.  1 is true, 0 is false
1
```


Substitution:


```j
   ('am';'am still') rxrplc str
I am still a string
```


Note: use
```J
   open'regex'
```
 to read the source code for the library.  The comments list 6 main definitions and a dozen utility definitions.


## Java

Test


```java
String str = "I am a string";
if (str.matches(".*string")) { // note: matches() tests if the entire string is a match
  System.out.println("ends with 'string'");
}
```


To match part of a string, or to process matches:

```java
import java.util.regex.*;
Pattern p = Pattern.compile("a*b");
Matcher m = p.matcher(str);
while (m.find()) {
  // use m.group() to extract matches
}
```


Substitute


```java
String orig = "I am the original string";
String result = orig.replaceAll("original", "modified");
// result is now "I am the modified string"
```



## JavaScript

Test/Match

```javascript
var subject = "Hello world!";

// Two different ways to create the RegExp object
// Both examples use the exact same pattern... matching "hello "
var re_PatternToMatch = /Hello (World)/i; // creates a RegExp literal with case-insensitivity
var re_PatternToMatch2 = new RegExp("Hello (World)", "i");

// Test for a match - return a bool
var isMatch = re_PatternToMatch.test(subject);

// Get the match details
//    Returns an array with the match's details
//    matches[0] == "Hello world"
//    matches[1] == "world"
var matches = re_PatternToMatch2.exec(subject);
```


Substitute

```javascript
var subject = "Hello world!";

// Perform a string replacement
//    newSubject == "Replaced!"
var newSubject = subject.replace(re_PatternToMatch, "Replaced");
```



## jq

Recent versions of jq (jq > 1.4) include PCRE regex support using the [http://www.geocities.jp/kosako3/oniguruma/doc/RE.txt Oniguruma library].

'''Test''':

```jq
"I am a string" | test("string$")
```

yields: true

'''Substitutution''':

```jq
"I am a string" | sub(" a "; " another ")
```

yields: "I am another string"

'''Substitution using capture''':

```jq
"abc" | sub( "(?<head>^.)(?<tail>.*)"; "\(.head)-\(.tail)")
```

yields: "a-bc"


## Jsish


```javascript
/* Regular expressions, in Jsish */

var re = /s[ai]mple/;
var sentence = 'This is a sample sentence';

var matches = sentence.match(re);
if (matches.length > 0) printf('%s found in "%s" using %q\n', matches[0], sentence, re);

var replaced = sentence.replace(re, "different");
printf("replaced sentence is: %s\n", replaced);
```


```txt
prompt$ jsish regularExpressions.jsi
sample found in "This is a sample sentence" using "/s[ai]mple/"
replaced sentence is: This is a different sentence
```



## Julia

Julia implements Perl-compatible regular expressions (via the built-in [http://www.pcre.org/ PCRE library]).  To test for a match:

```julia
s = "I am a string"
if ismatch(r"string$", s)
    println("'$s' ends with 'string'")
end
```

To perform replacements:

```julia
s = "I am a string"
s = replace(s, r" (a|an) ", " another ")
```

There are many [http://docs.julialang.org/en/latest/manual/strings/#regular-expressions other features] of Julia's regular-expression support, too numerous to list here.


## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    val s1 = "I am the original string"
    val r1 = Regex("^.*string$")
    if (s1.matches(r1)) println("`$s1` matches `$r1`")
    val r2 = Regex("original")
    val s3 = "replacement"
    val s2 = s1.replace(r2, s3)
    if (s2 != s1) println("`$s2` replaces `$r2` with `$s3`")
}
```


```txt

`I am the original string` matches `^.*string$`
`I am the replacement string` replaces `original` with `replacement`

```



## Langur

Langur uses semi-integreted regex. There are several functions that can be used with regexes, such as match(), replace(), split(), etc.

To match a string, ...

```Langur
if matching(re/abc/, "somestring") { ... }
```


Or...

```Langur
if val (.x, .y) = submatch(re/(abc+).+?(def)/, "somestring") { ... }
```


Substitution does not alter the original string.

```Langur
replace("abcdef", re/abc/, "Y")
# result: "Ydef"
```



## Lasso

Lasso has built in support for regular expressions using ICU regexps.

```Lasso
local(mytext = 'My name is: Stone, Rosetta
My name is: Hippo, Campus
')

local(regexp = regexp(
		-find = `(?m)^My name is: (.*?), (.*?)$`,
		-input = #mytext,
		-replace = `Hello! I am $2 $1`,
		-ignorecase
))


while(#regexp -> find) => {^
	#regexp -> groupcount > 1 ? (#regexp -> matchString(2) -> trim&) + '<br />'
^}

#regexp -> reset(-input = #mytext)
#regexp -> findall

#regexp -> reset(-input = #mytext)
'<br />'
#regexp -> replaceall
```


```txt
Rosetta
Campus
array(My name is: Stone, Rosetta, My name is: Hippo, Campus)
Hello! I am Rosetta Stone Hello! I am Campus Hippo
```



## Lua


In Lua many string manipulation methods use ''patterns'', which offer almost the same fucntionality as regular expressions, but whose syntax differs slightly. The percent sign (<code>%</code>) is generally used instead of a backslash to start a character class or a reference for a match in a substitution.


```lua
test = "My name is Lua."
pattern = ".*name is (%a*).*"

if test:match(pattern) then
    print("Name found.")
end

sub, num_matches = test:gsub(pattern, "Hello, %1!")
print(sub)
```



## M2000 Interpreter

We can use COM objects so we can use VBscript.RegExp

Properties Count and List() are bound with MyMatches using smart pointer. So MyMatches can change object, and Count and List() can operate with new object.

Com objects always have one real pointer. We can't returned id (because at the exit of module or function where we create it pointer get Nothing, so properties have a "broken" pointer, and return of object returns a broken smart pointer). We can pass by reference com objects, or we can use it as Globals (until "mother" module finish). We can use WithEvent if we wish to get events. We have to make functions with object_event name (note that forms event use dot not underscore).

We can use named parameters (for Word)  Try {Method Documents, "add", "", DocumentType:=WdNewWebPage as doc1}

New version using enumerator from object. Including a help SUB for displaying all functions of a COM object.
Enumerators for some COM objects are number of function -4&, we can use this in place of string for the name of property.


```M2000 Interpreter

Module CheckIt {
      declare global ObjRegEx "VBscript.RegExp"
      Function RegEx.Replace$(from$, what$) {
            Method ObjRegEx, "Replace", from$, what$ as response$
            =response$
      }
      Function RegEx.Test(what$) {
            Method ObjRegEx, "Test", what$ as response
            =response
      }
      Print Type$(ObjRegEx)
      With ObjRegEx, "Global", True, "Pattern" as pattern$
      pattern$="Mona Lisa"
      Print RegEx.Test("The Mona Lisa is in the Louvre.")=true
      Print RegEx.Replace$("The Mona Lisa is in the Louvre.", "La Gioconda")
      Pattern$ = " {2,}"
      Print "Myer Ken,  Vice President,  Sales and Services"
      \\ Removing some spaces
      Print RegEx.Replace$("Myer Ken,  Vice President,  Sales and Services", " ")
      pattern$="(\d{3})-(\d{3})-(\d{4})"

      Method ObjRegEx, "Execute", "555-123-4567, 555-943-6717" as MyMatches
      Print Type$(MyMatches)  ' it is a IMatchCollection2
      With MyMatches, "Count" as count, "Item" as List$()
      For i=0 to Count-1 : Print List$(i) : Next i


      Print RegEx.Replace$("555-123-4567, 555-943-6717", "($1) $2-$3")
      Pattern$ = "(\S+), (\S+)"
      Print RegEx.Replace$("Myer, Ken", "$2 $1")
      Method ObjRegEx, "Execute", "Myer, Ken" as MyMatches
      Rem : DisplayFunctions(MyMatches)
      \\ we can use Enumerator
      With MyMatches, "_NewEnum" as New Matches
      Rem : DisplayFunctions(Matches)
      With Matches, "Value" as New item$
      While Matches {
           Print Item$
      }
      \\ Or just using the list$()
      For i=0 to Count-1 : Print List$(i) : Next i
      declare ObjRegEx Nothing
      End
      Sub DisplayFunctions(x)
            Local cc=param(x),  ec=each(cc)
            while ec {
                  Print eval$(ec)   ' print every function/property of object x
            }
      End Sub
}
Checkit



\\ internal has no pattern. There is a like operator (~) for strings which use pattern matching (using VB6 like). We can use Instr() and RInstr() for strings.

Module Internal {
      what$="Mona Lisa"
      Document a$="The Mona Lisa is in the Louvre."
      Find a$, what$
      Read FindWhere
      If FindWhere<>0 then Read parNo, parlocation
      \\ replace in place
      Insert  FindWhere, Len(what$)  a$="La Gioconda"
      Report a$

      n$="The Mona Lisa is in the Louvre, not the Mona Lisa"
      Report Replace$("Mona Lisa", "La Gioconda", n$, 1, 1)  ' replace from start only one
      dim a$()
      a$()=Piece$("Myer, Ken",", ")
      Print a$(1)+", "+a$(0)="Ken, Myer"
}
Internal

```



## M4


```M4
regexp(`GNUs not Unix', `\<[a-z]\w+')
regexp(`GNUs not Unix', `\<[a-z]\(\w+\)', `a \& b \1 c')
```


```txt

5
a not b ot c

```



## Maple


```Maple
#Examples from Maple Help
StringTools:-RegMatch("^ab+bc$", "abbbbc");
StringTools:-RegMatch("^ab+bc$", "abbbbcx");
StringTools:-RegSub("a([bc]*)(c*d)", "abcd", "&-\\1-\\2");
StringTools:-RegSub("(.*)c(anad[ai])(.*)", "Maple is canadian", "\\1C\\2\\3");
```

```txt
true
false
"abcd-bc-d"
"Maple is Canadian"
```



## Mathematica


```Mathematica

StringCases["I am a string with the number 18374 in me",RegularExpression["[0-9]+"]]
StringReplace["I am a string",RegularExpression["I\\sam"] -> "I'm"]

```

The in-notebook output, in order:

```txt

{18374}
I'm a string

```


## MAXScript


```MAXScript

samples = #("Some string 123","Example text 123","string",\
	    "ThisString Will Not Match","A123,333,string","123451")
samples2 = #("I am a string","Me too.")

regex = dotnetobject "System.Text.RegularExpressions.Regex" ".*\bstring*"
regex2 = dotnetobject "System.Text.RegularExpressions.Regex" "\ba\b"

clearlistener()

format "Pattern is : %\n" (regex.toString())

for i in samples do
(
	if regex.ismatch(i) then
	(
		format "The string \"%\" matches the pattern\n" i
	)
	else
	(
		format "The string \"%\" doesn't match the pattern\n" i
	)
)

-- replacement

format "Pattern is : %\n" (regex2.toString())

for i in samples2 do
(
	if regex2.ismatch(i) then
	(
		local replaced = regex2.replace i "another"
		format "The string \"%\" matched the pattern, so it was replaced: \"%\"\n" i replaced
	)
	else
	(
		format "The string \"%\" does not match the pattern\n" i
	)
)

```

```txt

OK
Pattern is : .*\bstring*
OK
The string "Some string 123" matches the pattern
The string "Example text 123" doesn't match the pattern
The string "string" matches the pattern
The string "ThisString Will Not Match" doesn't match the pattern
The string "A123,333,string" matches the pattern
The string "123451" doesn't match the pattern
OK
Pattern is : \ba\b
OK
The string "I am a string" matched the pattern, so it was replaced: "I am another string"
The string "And me too." does not match the pattern
OK
OK

```



## MIRC Scripting Language


```mirc
alias regular_expressions {
  var %string = This is a string
  var %re = string$
  if ($regex(%string,%re) > 0) {
    echo -a Ends with string.
  }
  %re = \ba\b
  if ($regsub(%string,%re,another,%string) > 0) {
    echo -a Result 1: %string
  }
  %re = \b(another)\b
  echo -a Result 2: $regsubex(%string,%re,yet \1)
}
```


```txt

Ends with string.
Result 1: This is another string
Result 2: This is yet another string

```



## MUMPS

<p>MUMPS doesn't have a replacement functionality when using the pattern matching operator, ?. We can mimic it with $PIECE, but $PIECE doesn't work with regular expressions as an operand.</p>
```MUMPS
REGEXP
 NEW HI,W,PATTERN,BOOLEAN
 SET HI="Hello, world!",W="world"
 SET PATTERN=".E1"""_W_""".E"
 SET BOOLEAN=HI?@PATTERN
 WRITE "Source string - '"_HI_"'",!
 WRITE "Partial string - '"_W_"'",!
 WRITE "Pattern string created is - '"_PATTERN_"'",!
 WRITE "Match? ",$SELECT(BOOLEAN:"YES",'BOOLEAN:"No"),!
 ;
 SET BOOLEAN=$FIND(HI,W)
 IF BOOLEAN>0 WRITE $PIECE(HI,W,1)_"string"_$PIECE(HI,W,2)
 QUIT
```

Usage:
```txt

USER>D REGEXP^ROSETTA
Source string - 'Hello, world!'
Partial string - 'world'
Pattern string created is - '.E1"world".E'
Match? YES
Hello, string!
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

import java.util.regex.

st1 = 'Fee, fie, foe, fum, I smell the blood of an Englishman'
rx1 = 'f.e.*?'
sbx = 'foo'

rx1ef = '(?i)'rx1 -- use embedded flag expression == Pattern.CASE_INSENSITIVE

-- using String's matches & replaceAll
mcm = (String st1).matches(rx1ef)
say 'String "'st1'"' 'matches pattern "'rx1ef'":' Boolean(mcm)
say
say 'Replace all occurrences of regex pattern "'rx1ef'" with "'sbx'"'
stx = Rexx
stx = (String st1).replaceAll(rx1ef, sbx)
say 'Input string:  "'st1'"'
say 'Result string: "'stx'"'
say

-- using java.util.regex classes
pt1 = Pattern.compile(rx1, Pattern.CASE_INSENSITIVE)
mc1 = pt1.matcher(st1)
mcm = mc1.matches()
say 'String "'st1'"' 'matches pattern "'pt1.toString()'":' Boolean(mcm)
mc1 = pt1.matcher(st1)
say
say 'Replace all occurrences of regex pattern "'rx1'" with "'sbx'"'
sx1 = Rexx
sx1 = mc1.replaceAll(sbx)
say 'Input string:  "'st1'"'
say 'Result string: "'sx1'"'
say

return

```

```txt

String "Fee, fie, foe, fum, I smell the blood of an Englishman" matches pattern "(?i)f.e.*?": true

Replace all occurrences of regex pattern "(?i)f.e.*?" with "foo"
Input string:  "Fee, fie, foe, fum, I smell the blood of an Englishman"
Result string: "foo, foo, foo, fum, I smell the blood of an Englishman"

String "Fee, fie, foe, fum, I smell the blood of an Englishman" matches pattern "f.e.*?": true

Replace all occurrences of regex pattern "f.e.*?" with "foo"
Input string:  "Fee, fie, foe, fum, I smell the blood of an Englishman"
Result string: "foo, foo, foo, fum, I smell the blood of an Englishman"

```



## NewLISP


```NewLISP
(regex "[bB]+" "AbBBbABbBAAAA") -> ("bBBb" 1 4)
```



## Nim


```nim
import re

var s = "This is a string"

if s.find(re"string$") > -1:
  echo "Ends with string."

s = s.replace(re"\ a\ ", " another ")
echo s
```



## Objeck


```objeck

use RegEx;

bundle Default {
  class RegExTest {
    function : Main(args : String[]) ~ Nil {
      string := "I am a string";
      # exact match
      regex := RegEx->New(".*string");
      if(regex->MatchExact(".*string")) {
        "ends with 'string'"->PrintLine();
      };
      # replace all
      regex := RegEx->New(" a ");
      regex->ReplaceAll(string, " another ")->PrintLine();
    }
  }
}

```


=={{header|Objective-C}}==
Test
```objc
NSString *str = @"I am a string";
NSString *regex = @".*string$";

// Note: the MATCHES operator matches the entire string, necessitating the ".*"
NSPredicate *pred = [NSPredicate predicateWithFormat:@"SELF MATCHES %@", regex];

if ([pred evaluateWithObject:str]) {
    NSLog(@"ends with 'string'");
}
```

Unfortunately this method cannot find the location of the match or do substitution.


### NSRegularExpressionSearch

Test
```objc
NSString *str = @"I am a string";
if ([str rangeOfString:@"string$" options:NSRegularExpressionSearch].location != NSNotFound) {
    NSLog(@"Ends with 'string'");
}
```


Substitute
{{works with|iOS|4.0+}} undocumented

```objc
NSString *orig = @"I am the original string";
NSString *result = [orig stringByReplacingOccurrencesOfString:@"original"
                                                   withString:@"modified"
                                                      options:NSRegularExpressionSearch
                                                        range:NSMakeRange(0, [orig length])];
NSLog(@"%@", result);
```



### NSRegularExpression

Test

```objc
NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:@"string$"
                                                                       options:0
                                                                         error:NULL];
NSString *str = @"I am a string";
if ([regex rangeOfFirstMatchInString:str
                             options:0
                               range:NSMakeRange(0, [str length])
     ].location != NSNotFound) {
    NSLog(@"Ends with 'string'");
}
```


Loop through matches

```objc
for (NSTextCheckingResult *match in [regex matchesInString:str
                                                   options:0
                                                     range:NSMakeRange(0, [str length])
                                     ]) {
    // match.range gives the range of the whole match
    // [match rangeAtIndex:i] gives the range of the i'th capture group (starting from 1)
}
```


Substitute

```objc
NSString *orig = @"I am the original string";
NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:@"original"
                                                                       options:0
                                                                         error:NULL];
NSString *result = [regex stringByReplacingMatchesInString:orig
                                                   options:0
                                                     range:NSMakeRange(0, [orig length])
                                              withTemplate:@"modified"];
NSLog(@"%@", result);
```



## OCaml


###  With the standard library

Test

```ocaml
#load "str.cma";;
let str = "I am a string";;
try
  ignore(Str.search_forward (Str.regexp ".*string$") str 0);
  print_endline "ends with 'string'"
with Not_found -> ()
;;
```


Substitute

```ocaml
#load "str.cma";;
let orig = "I am the original string";;
let result = Str.global_replace (Str.regexp "original") "modified" orig;;
(* result is now "I am the modified string" *)
```



###  Using Pcre

'''Library:''' [http://ocaml.info/home/ocaml_sources.html#pcre-ocaml ocaml-pcre]


```ocaml
let matched pat str =
  try ignore(Pcre.exec ~pat str); (true)
  with Not_found -> (false)
;;

let () =
  Printf.printf "matched = %b\n" (matched "string$" "I am a string");
  Printf.printf "Substitute: %s\n"
    (Pcre.replace ~pat:"original" ~templ:"modified" "I am the original string")
;;
```



## Ol


```scheme

; matching:
(define regex (string->regex "m/aa(bb|cc)dd/"))
(print (regex "aabbddx")) ; => true
(print (regex "aaccddx")) ; => true
(print (regex "aabcddx")) ; => false

; substitute part of a string:
(define regex (string->regex "s/aa(bb|cc)dd/HAHAHA/"))
(print (regex "aabbddx")) ; => HAHAHAx
(print (regex "aaccddx")) ; => HAHAHAx
(print (regex "aabcddx")) ; => false


```



## ooRexx


```ooRexx
/* Rexx */
/* Using the RxRegExp Regular Expression built-in utility class */

st1 = 'Fee, fie, foe, fum, I smell the blood of an Englishman'
rx1 = '[Ff]?e' -- unlike most regex engines, RxRegExp uses '?' instead of '.' to match any single character
sbx = 'foo'

myRE = .RegularExpression~new()
myRE~parse(rx1, MINIMAL)

mcm = myRE~pos(st1)
say 'String "'st1'"' 'matches pattern "'rx1'":' bool2string(mcm > 0)
say

-- The RxRegExp package doesn't provide a replace capability so you must roll your own
st0 = st1
loop label GREP forever
  mcp = myRE~pos(st1)
  if mcp > 0 then do
    mpp = myRE~position
    fnd = st1~substr(mcp, mpp - mcp + 1)
    stx = st1~changestr(fnd, sbx, 1)
    end
  else leave GREP
  st1 = stx
  end GREP
say 'Input string:  "'st0'"'
say 'Result string: "'stx'"'
return
exit

bool2string:
procedure
do
  parse arg bv .
  if bv then bx = 'true'
        else bx = 'false'
  return bx
end
exit

::requires "rxregexp.cls"

```

```txt

String "Fee, fie, foe, fum, I smell the blood of an Englishman" matches pattern "[Ff]?e": true

Input string:  "Fee, fie, foe, fum, I smell the blood of an Englishman"
Result string: "foo, foo, foo, fum, I smell the blood of an Englishman"

```



## Oxygene


```oxygene

// Match and Replace part of a string using a Regular Expression
//
// Nigel Galloway - April 15th., 2012
//
namespace re;

interface

type
  re = class
  public
    class method Main;
  end;

implementation

class method re.Main;
const
  myString = 'I think that I am Nigel';
var
  r: System.Text.RegularExpressions.Regex;
  myResult : String;
begin
  r := new System.Text.RegularExpressions.Regex('(I am)|(you are)');
  Console.WriteLine("{0} contains {1}", myString, r.Match(myString));
  myResult := r.Replace(myString, "you are");
  Console.WriteLine("{0} contains {1}", myResult, r.Match(myResult));
end;

end.

```

Produces:

```txt

I think that I am Nigel contains I am
I think that you are Nigel contains you are

```



## Oz


```oz
declare
  [Regex] = {Module.link ['x-oz://contrib/regex']}
  String = "This is a string"
in
  if {Regex.search "string$" String} \= false then
     {System.showInfo "Ends with string."}
  end
  {System.showInfo {Regex.replace String " a " fun {$ _ _} " another " end}}
```


## Pascal


```pascal

// Match and Replace part of a string using a Regular Expression
//
// Nigel Galloway - April 11th., 2012
//
program RegularExpr;

uses
  RegExpr;

const
  myString = 'I think that I am Nigel';
  myMatch = '(I am)|(you are)';
var
  r : TRegExpr;
  myResult : String;

begin
  r := TRegExpr.Create;
  r.Expression := myMatch;
  write(myString);
  if r.Exec(myString) then writeln(' contains ' + r.Match[0]);
  myResult := r.Replace(myString, 'you are', False);
  write(myResult);
  if r.Exec(myResult) then writeln(' contains ' + r.Match[0]);
end.

```

Produces:

```txt

>RegularExpr
I think that I am Nigel contains I am
I think that you are Nigel contains you are

```



## Perl

Test

```perl
$string = "I am a string";
if ($string =~ /string$/) {
   print "Ends with 'string'\n";
}

if ($string !~ /^You/) {
   print "Does not start with 'You'\n";
}
```



Substitute

```perl
$string = "I am a string";
$string =~ s/ a / another /; # makes "I am a string" into "I am another string"
print $string;
```


In Perl 5.14+, you can return a new substituted string without altering the original string:

```perl
$string = "I am a string";
$string2 = $string =~ s/ a / another /r; # $string2 == "I am another string", $string is unaltered
print $string2;
```



Test and Substitute

```perl
$string = "I am a string";
if ($string =~ s/\bam\b/was/) { # \b is a word border
   print "I was able to find and replace 'am' with 'was'\n";
}
```



Options

```perl
# add the following just after the last / for additional control
# g = globally (match as many as possible)
# i = case-insensitive
# s = treat all of $string as a single line (in case you have line breaks in the content)
# m = multi-line (the expression is run on each line individually)

$string =~ s/i/u/ig; # would change "I am a string" into "u am a strung"
```


Omission of the regular expression binding operators

If regular expression matches are being made against the topic variable, it is possible to omit the regular expression binding operators:


```perl
$_ = "I like banana milkshake.";
if (/banana/) {          # The regular expression binding operator is omitted
  print "Match found\n";
}
```



## Perl 6


```perl6
use v6;
if 'a long string' ~~ /string$/ {
   say "It ends with 'string'";
}

# substitution has a few nifty features

$_ = 'The quick Brown fox';
s:g:samecase/\w+/xxx/;
.say;
# output:
# Xxx xxx Xxx xxx

```



## Phix


```Phix
include builtins\regex.e
string s = "I am a string"
printf(1,"\"%s\" %s with string\n",{s,iff(length(regex(`string$`,s))?"ends":"does not end")})
printf(1,"\"%s\" %s with You\n",{s,iff(length(regex(`^You`,s))?"starts":"does not start")})
?gsub(`[A-Z]`,"abCDefG","*")
?gsub(`[A-Z]`,"abCDefGH","(&)")
?gsub(`[A-Z]+`,"abCDefGH","(&)")
?gsub(`string`,s,"replacement")
s = gsub(`\ba\b`,s,"another") ?s
?gsub(`string`,s,"replacement")
```

```txt

"I am a string" ends with string
"I am a string" does not start with You
"ab**ef*"
"ab(C)(D)ef(G)(H)"
"ab(CD)ef(GH)"
"I am a replacement"
"I am another string"
"I am another replacement"

```



## PHP

```php
$string = 'I am a string';
# Test
if (preg_match('/string$/', $string))
{
    echo "Ends with 'string'\n";
}
# Replace
$string = preg_replace('/\ba\b/', 'another', $string);
echo "Found 'a' and replace it with 'another', resulting in this string: $string\n";
```


```txt
Ends with 'string'
Foud 'a' and replaced it with 'another', resulting in this string: I am another string
```



## PicoLisp


### Calling the C library

PicoLisp doesn't have built-in regex functionality.
It is easy to call the native C library.

```PicoLisp
(let (Pat "a[0-9]z"  String "a7z")
   (use Preg
      (native "@" "regcomp" 'I '(Preg (64 B . 64)) Pat 1)  # Compile regex
      (when (=0 (native "@" "regexec" 'I (cons NIL (64) Preg) String 0 0 0))
         (prinl "String \"" String "\" matches regex \"" Pat "\"") ) ) )
```

```txt
String "a7z" matches pattern "a[0-9]z"
```



### Using Pattern Matching

Regular expressions are static and inflexible.
Another possibility is dynamic pattern matching,
where arbitrary conditions can be programmed.

```PicoLisp
(let String "The number <7> is incremented"
   (use (@A @N @Z)
      (and
         (match '(@A "<" @N ">"  @Z) (chop String))
         (format @N)
         (prinl @A "<" (inc @) ">" @Z) ) ) )
```

```txt
The number <8> is incremented
```



## PowerShell


```powershell
"I am a string" -match '\bstr'       # true
"I am a string" -replace 'a\b','no'  # I am no string
```

By default both the <code>-match</code> and <code>-replace</code> operators are case-insensitive. They can be made case-sensitive by using the <code>-cmatch</code> and <code>-creplace</code> operators.


## PureBasic


```PureBasic
String$        = "<tag>some text consisting of Roman letters spaces and numbers like 12</tag>"
regex$         = "<([a-z]*)>[a-z,A-Z,0-9, ]*</\1>"
regex_replace$ = "letters[a-z,A-Z,0-9, ]*numbers[a-z,A-Z,0-9, ]*"
If CreateRegularExpression(1, regex$) And CreateRegularExpression(2, regex_replace$)
  If MatchRegularExpression(1, String$)
    Debug "Tags correct, and only alphanummeric or space characters between them"
  EndIf
  Debug ReplaceRegularExpression(2, String$, "char stuff")
EndIf
```



## Python


```python
import re

string = "This is a string"

if re.search('string$', string):
    print("Ends with string.")

string = re.sub(" a ", " another ", string)
print(string)
```



## R

First, define some strings.

```R
pattern <- "string"
text1 <- "this is a matching string"
text2 <- "this does not match"
```

Matching with grep.  The indices of the texts containing matches are returned.

```R
grep(pattern, c(text1, text2))  # 1
```

Matching with regexpr.  The positions of the starts of the matches are returned, along with the lengths of the matches.

```R
regexpr(pattern, c(text1, text2))
```

 [1] 20 -1
 attr(,"match.length")
 [1]  6 -1
Replacement

```R
gsub(pattern, "pair of socks", c(text1, text2))
```

 [1] "this is a matching pair of socks" "this does not match"


## Racket



```racket

#lang racket

(define s "I am a string")

(when (regexp-match? #rx"string$" s)
  (displayln "Ends with 'string'."))

(unless (regexp-match? #rx"^You" s)
  (displayln "Does not start with 'You'."))

(displayln (regexp-replace " a " s " another "))

```



## Raven



```raven
'i am a string' as str
```


Match:


```raven
str m/string$/
if  "Ends with 'string'\n" print
```


Replace once:


```raven
str r/ a / another /  print>
```


```raven
str r/ /_/  print
```


Replace all:


```raven
str r/ /_/g  print
```


Replace case insensitive:


```raven
str r/ A / another /i  print>
```


Splitting:


```raven
str s/ />
```



## REBOL


```REBOL
REBOL [
	Title: "Regular Expression Matching"
	URL: http://rosettacode.org/wiki/Regular_expression_matching
]

string: "This is a string."

; REBOL doesn't use a conventional Perl-compatible regular expression
; syntax. Instead, it uses a variant Parsing Expression Grammar with
; the 'parse' function. It's also not limited to just strings. You can
; define complex grammars that actually parse and execute program
; files.

; Here, I provide a rule to 'parse' that specifies searching through
; the string until "string." is found, then the end of the string. If
; the subject string satisfies the rule, the expression will be true.

if parse string [thru "string." end] [
	print "Subject ends with 'string.'"]

; For replacement, I take advantage of the ability to call arbitrary
; code when a pattern is matched -- everything in the parens will be
; executed when 'to " a "' is satisfied. This marks the current string
; location, then removes the offending word and inserts the replacement.

parse string [
	to " a " ; Jump to target.
	mark: (
		remove/part mark 3 ; Remove target.
		mark: insert mark " another " ; Insert replacement.
	)
	:mark ; Pick up where I left off.
]
print [crlf "Parse replacement:" string]

; For what it's worth, the above operation is more conveniently done
; with the 'replace' function:

replace string " another " " a " ; Change string back.
print [crlf "Replacement:" string]
```


```txt
Subject ends with 'string.'

Parse replacement: This is another string.

Replacement: This is a string.
```



## REXX

Rexx does not directly support the use of regular expressions as part of the language.

However, some rexx interpreters offer support for regular expressions via external function libraries or

through implementation specific extensions.


It is also possible to emulate regular expressions through appropriate coding techniques.


All of the following REXX examples are modeled after the '''PERL''' examples.


### testing


```rexx
/*REXX program demonstrates   testing      (modeled after Perl example).*/
$string="I am a string"
                                                  say 'The string is:'  $string
x="string" ;  if right($string,length(x))=x  then say 'It ends with:'  x
y="You"    ;  if left($string,length(y))\=y  then say 'It does not start with:'  y
z="ring"   ;  if pos(z,$string)\==0          then say 'It contains the string:'  z
z="ring"   ;  if wordpos(z,$string)==0       then say 'It does not contain the word:'  z
                                       /*stick a fork in it, we're done.*/
```

```txt

The string is: I am a string
It ends with: string
It does not start with: You
It contains the string: ring
It does not contain the word: ring

```


===substitution   (destructive)===

```rexx
/*REXX program demonstrates  substitution  (modeled after Perl example).*/
$string = "I am a string"
    old = " a "
    new = " another "
say 'The original string is:'   $string
say 'old  word  is:'            old
say 'new  word  is:'            new
$string = changestr(old,$string,new)
say 'The  changed string is:'   $string
                                       /*stick a fork in it, we're done.*/
```

```txt

The original string is: I am a string
old  word  is:  a
new  word  is:  another
The  changed string is: I am another string

```



===substitution   (non-destructive)===

```rexx
/*REXX program shows  non-destructive sub. (modeled after Perl example).*/
$string = "I am a string"
    old = " a "
    new = " another "
say 'The original string is:'  $string
say 'old  word  is:'           old
say 'new  word  is:'           new
$string2 = changestr(old,$string,new)
say 'The original string is:'  $string
say 'The  changed string is:'  $string2
                                       /*stick a fork in it, we're done.*/
```

```txt

The original string is: I am a string
old  word  is:  a
new  word  is:  another
The original string is: I am a string
The  changed string is: I am another string

```



### test and substitute


```rexx
/*REXX program shows  test and substitute  (modeled after Perl example).*/
 $string = "I am a string"
     old = " am "
     new = " was "
say 'The original string is:'  $string
say 'old  word  is:'           old
say 'new  word  is:'           new

if wordpos(old,$string)\==0  then
           do
           $string = changestr(old,$string,new)
           say 'I was able to find and replace ' old " with " new
           end
                                       /*stick a fork in it, we're done.*/
```

```txt

The original string is: I am a string
old  word  is:  am
new  word  is:  was
I was able to find and replace   am   with   was

```

Some older REXXes don't have a   '''changestr'''   BIF,   so one is included here:   ───►   [[CHANGESTR.REX]].





## Ring


```ring

# Project : Regular expressions

text = "I am a text"
if right(text,4) = "text"
   see "'" + text +"' ends with 'text'" + nl
ok
i = substr(text,"am")
text = left(text,i - 1) + "was" + substr(text,i + 2)
see "replace 'am' with 'was' = " + text + nl

```

Output:

```txt

'I am a text' ends with 'text'
replace 'am' with 'was' = I was a text

```



## Ruby

Test

```ruby
str = "I am a string"
p "Ends with 'string'" if str =~ /string$/
p "Does not start with 'You'" unless str =~ /^You/
```


Substitute

```ruby
str.sub(/ a /, ' another ') #=> "I am another string"
# Or:
str[/ a /] = ' another '    #=> "another"
str                         #=> "I am another string"
```


Substitute using block

```ruby
str.gsub(/\bam\b/) { |match| match.upcase } #=> "I AM a string"
```



## Run BASIC


```runbasic
string$ = "I am a string"
if right$(string$,6) = "string" then print "'";string$;"' ends with 'string'"
i = instr(string$,"am")
string$ = left$(string$,i - 1) + "was" + mid$(string$,i + 2)
print "replace 'am' with 'was' = ";string$

```

```txt
'I am a string' ends with 'string'
replace 'am' with 'was' = I was a string
```



## Rust

Note that <code>Regex::new</code> checks for a valid regex and thus returns a <code>Result<Regex, Error></code>.

```Rust
use regex::Regex;

fn main() {
    let s = "I am a string";

    if Regex::new("string$").unwrap().is_match(s) {
        println!("Ends with string.");
    }

    println!("{}", Regex::new(" a ").unwrap().replace(s, " another "));
}
```



## Sather

Sather understands POSIX regular expressions.


```sather
class MAIN is
  -- we need to implement the substitution
  regex_subst(re:REGEXP, s, sb:STR):STR is
    from, to:INT;
    re.match(s, out from, out to);
    if from = -1 then return s; end;
    return s.head(from) + sb + s.tail(s.size - to);
  end;

  main is
    s ::= "I am a string";
    re ::= REGEXP::regexp("string$", true);
    if re.match(s) then
      #OUT + "'" + s + "'" + " ends with 'string'\n";
    end;
    if ~REGEXP::regexp("^You", false).match(s) then
      #OUT + "'" + s + "'" + " does not begin with 'You'\n";
    end;
    #OUT + regex_subst(re, s, "integer") + "\n";
    #OUT + regex_subst(REGEXP::regexp("am +a +st", true), s, "get the ") + "\n";
  end;
end;
```



## Scala

Define

```Scala
val Bottles1 = "(\\d+) bottles of beer".r                                            // syntactic sugar
val Bottles2 = """(\d+) bottles of beer""".r                                         // using triple-quotes to preserve backslashes
val Bottles3 = new scala.util.matching.Regex("(\\d+) bottles of beer")               // standard
val Bottles4 = new scala.util.matching.Regex("""(\d+) bottles of beer""", "bottles") // with named groups
```


Search and replace with string methods:

```scala
"99 bottles of beer" matches "(\\d+) bottles of beer" // the full string must match
"99 bottles of beer" replace ("99", "98") // Single replacement
"99 bottles of beer" replaceAll ("b", "B") // Multiple replacement
```


Search with regex methods:

```scala
"\\d+".r findFirstIn "99 bottles of beer" // returns first partial match, or None
"\\w+".r findAllIn "99 bottles of beer" // returns all partial matches as an iterator
"\\s+".r findPrefixOf "99 bottles of beer" // returns a matching prefix, or None
Bottles4 findFirstMatchIn "99 bottles of beer" // returns a "Match" object, or None
Bottles4 findPrefixMatchOf "99 bottles of beer" // same thing, for prefixes
val bottles = (Bottles4 findFirstMatchIn "99 bottles of beer").get.group("bottles") // Getting a group by name
```


Using pattern matching with regex:

```Scala
val Some(bottles) = Bottles4 findPrefixOf "99 bottles of beer" // throws an exception if the matching fails; full string must match
for {
  line <- """|99 bottles of beer on the wall
             |99 bottles of beer
             |Take one down, pass it around
             |98 bottles of beer on the wall""".stripMargin.lines
} line match {
  case Bottles1(bottles) => println("There are still "+bottles+" bottles.") // full string must match, so this will match only once
  case _ =>
}
for {
  matched <- "(\\w+)".r findAllIn "99 bottles of beer" matchData // matchData converts to an Iterator of Match
} println("Matched from "+matched.start+" to "+matched.end)
```


Replacing with regex:

```Scala
Bottles2 replaceFirstIn ("99 bottles of beer", "98 bottles of beer")
Bottles3 replaceAllIn ("99 bottles of beer", "98 bottles of beer")
```



## Shiny


```shiny
str: 'I am a string'
```


Match text:

```shiny
if str.match ~string$~
    say "Ends with 'string'"
end
```


Replace text:

```shiny
say str.alter ~ a ~ 'another'
```


## Sidef

Simple matching:

```ruby
var str = "I am a string";
if (str =~ /string$/) {
    print "Ends with 'string'\n";
}
```


Global matching:

```ruby
var str = <<'EOF';
    x:Foo
    y:Bar
EOF

while (var m = str=~/(\w+):(\S+)/g) {
    say "#{m[0]} -> #{m[1]}";
}
```


Substitutions:

```ruby
var str = "I am a string";

# Substitute something mached by a regex
str.sub!(/ a /, ' another ');   # "I am a string" => "I am another string"

# Remove something matched by a regex
str -= / \Kanother /i;          # "I am another string" => "I am string"

# Global subtitution with a block
str = str.gsub(/(\w+)/, {|s1| 'x' * s1.len});  # globaly replace any word with 'xxx'

say str;     # prints: 'x xx xxxxxx'
```



## Slate


This library is still in its early stages. There isn't currently a feature to replace a substring.


```slate

 'http://slatelanguage.org/test/page?query' =~ '^(([^:/?#]+)\\:)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?'.

" ==> {'http:'. 'http'. '//slatelanguage.org'. 'slatelanguage.org'. '/test/page'. '?query'. 'query'. Nil} "

```



## Smalltalk


```smalltalk
|re s s1|
re := Regex fromString: '[a-z]+ing'.
s := 'this is a matching string'.
s1 := 'this does not match'.

(s =~ re)
ifMatched: [ :b |
   b match displayNl
].
(s1 =~ re)
ifMatched: [ :b |
   'Strangely matched!' displayNl
]
ifNotMatched: [
   'no match!' displayNl
].

(s replacingRegex: re with: 'modified') displayNl.
```


```smalltalk

|re s s1|
re := 'm[a-z]+ing' asRegex.
s := 'this is a matching string'.

(re search: s) ifTrue: [ 'matches!' ].

s1 := re copy: s replacingMatchesWith: 'modified'.

```



## SNOBOL4


In SNOBOL4, patterns are based not on regular expressions, but are a native datatype which can be constructed, manipulated, concatenated, used in pattern expressions, stored into variables, and so forth.  Patterns can be constructed ahead of time and saved in variables, and those preconstructed patterns can also reference additional pattern and data items which won't be known until actual pattern match time.  Patterns can define calls to functions which will be called during actual pattern matching, and whose outcome can affect how the pattern match continues, which tentative matches will and won't be accepted, and so forth.

SNOBOL4 pattern matching is thus hugely more capable than traditional regular expressions are.  An example of a pattern matching problem that would be prohibitively difficult to create as a regular expression would be to "create a pattern which matches a complete name and international postal mailing address."

SNOBOL4's "raison d'etre" is pattern matching and string manipulation (although it's also strong in data structures too).  The basic statement syntax in SNOBOL4 is:


```snobol4
label   subject  pattern  =  object          :(goto)
```


The basic operation is to evaluate the subject, evaluate the pattern, find the pattern in the subject, evaluate the object, and then replace the portion of the subject matched by the pattern with the evaluated object.  If any of those steps fails (i.e. does not succeed) then execution continues with the goto, as appropriate.

The goto can be unconditional, or can be based on whether the statement succeeded or failed (and that is the basis for all explicit transfers of control in SNOBOL4).  This example finds the string "SNOBOL4" in string variable string1, and replaces it with "new SPITBOL" (SPITBOL is an implementation of SNOBOL4, basically SPITBOL is to SNOBOL4 what Turbo Pascal is to Pascal):


```snobol4
     string1 = "The SNOBOL4 language is designed for string manipulation."
     string1 "SNOBOL4" = "new SPITBOL"                   :s(changed)f(nochange)
```


The following example replaces "diameter is " and a numeric value by "circumference is " and the circumference instead (it also shows creation of a pattern which matches integer or real numeric values, and storing that pattern into a variable... and then using that pattern variable later in a slightly more complicated pattern expression):


```snobol4
     pi = 3.1415926
     dd = "0123456789"
     string1 = "For the first circle, the diameter is 2.5 inches."
     numpat = span(dd) (("." span(dd)) | null)
     string1 "diameter is " numpat . diam = "circumference is " diam * pi
```


Relatively trivial pattern matching and replacements can be attacked very effectively using regular expressions, but regular expressions (while ubiquitous) are a crippling limitation for more complicated pattern matching problems.


## Standard ML

There is no regex support in the Basis Library; however, various implementations have their own support.
Test

```sml
CM.make "$/regexp-lib.cm";
structure RE = RegExpFn (
      structure P = AwkSyntax
      structure E = BackTrackEngine);
val re = RE.compileString "string$";
val string = "I am a string";
case StringCvt.scanString (RE.find re) string
 of NONE => print "match failed\n"
  | SOME match =>
      let
        val {pos, len} = MatchTree.root match
      in
        print ("matched at position " ^ Int.toString pos ^ "\n")
      end;
```



## Stata

See '''[http://www.stata.com/help.cgi?regexm regexm]''', '''regexr''' and '''regexs''' in Stata help.


```stata
scalar s="ars longa vita brevis"

* is there a vowel?
di regexm(s,"[aeiou]")

* replace the first vowel with "?"
di regexr(s,"[aeiou]","?")
```


## Swift



### RegularExpressionSearch

Test

```swift
import Foundation

let str = "I am a string"
if let range = str.rangeOfString("string$", options: .RegularExpressionSearch) {
  println("Ends with 'string'")
}
```


Substitute (undocumented)

```swift
import Foundation

let orig = "I am the original string"
let result = orig.stringByReplacingOccurrencesOfString("original", withString: "modified", options: .RegularExpressionSearch)
println(result)
```



### NSRegularExpression

Test

```swift
import Foundation

if let regex = NSRegularExpression(pattern: "string$", options: nil, error: nil) {
  let str = "I am a string"
  if let result = regex.firstMatchInString(str, options: nil, range: NSRange(location: 0, length: count(str.utf16))) {
    println("Ends with 'string'")
  }
}
```


Loop through matches

```swift
  for x in regex.matchesInString(str, options: nil, range: NSRange(location: 0, length: count(str.utf16))) {
    let match = x as! NSTextCheckingResult
    // match.range gives the range of the whole match
    // match.rangeAtIndex(i) gives the range of the i'th capture group (starting from 1)
  }
```


Substitute

```swift
import Foundation

let orig = "I am the original string"
if let regex = NSRegularExpression(pattern: "original", options: nil, error: nil) {
let result = regex.stringByReplacingMatchesInString(orig, options: nil, range: NSRange(location: 0, length: count(orig.utf16)), withTemplate: "modified")
  println(result)
}
```



## Tcl

Test using <code>regexp</code>:

```tcl
set theString "I am a string"
if {[regexp -- {string$} $theString]} {
    puts "Ends with 'string'"
}

if {![regexp -- {^You} $theString]} {
    puts "Does not start with 'You'"
}
```


Extract substring using <code>regexp</code>

```tcl
set theString "This string has >123< a number in it"
if {[regexp -- {>(\d+)<} $theString -> number]} {
    puts "Contains the number $number"
}
```


Substitute using <code>regsub</code>

```tcl
set theString = "I am   a   string"
puts [regsub -- { +a +} $theString { another }]
```



## Toka


Toka's regular expression library allows for matching, but does not yet provide for replacing elements within strings.


```toka
#! Include the regex library
needs regex

#! The two test strings
" This is a string" is-data test.1
" Another string" is-data test.2

#! Create a new regex named 'expression' which tries
#! to match strings beginning with 'This'.
" ^This" regex: expression

#! An array to store the results of the match
#! (Element 0 = starting offset, Element 1 = ending offset of match)
2 cells is-array match

#! Try both test strings against the expression.
#! try-regex will return a flag.  -1 is TRUE, 0 is FALSE
expression test.1 2 match try-regex .
expression test.2 2 match try-regex .
```



## TXR



### Search and replace: simple


Txr is not designed for sed-like filtering, but here is how to do <code>sed -e 's/dog/cat/g'</code>:


```txr
@(collect)
@(coll :gap 0)@mismatch@{match /dog/}@(end)@suffix
@(output)
@(rep)@{mismatch}cat@(end)@suffix
@(end)
@(end)
```


How it works is that the body of the <code>coll</code> uses a double-variable match:
an unbound variable followed by a regex-match variable.
The meaning of this combination is, "Search for the regular expression, and if successful, then bind all the characters whcih were skipped over by the search to the first variable, and the matching text to the second variable."
So we collect pairs: pieces of mismatching text, and pieces of text which match the regex <code>dog</code>.
At the end, there is usually going to be a piece of text which does not match the body, because it has no match for the regex.
Because <code>:gap 0</code> is specified, the coll construct will terminate when faced with this nonmatching text, rather than skipping it in a vain search for a match, which allows <code>@suffix</code> to take on this trailing text.

To output the substitution, we simply spit out the mismatching texts
followed by the replacement text, and then add the suffix.


### Search and replace: strip comments from C source


Based on the technique of the previous example, here is a query for stripping C comments from a source file, replacing
them by a space.
Here, the "non-greedy" version of the regex Kleene operator is used,
denoted by <code>%</code>.
This allows for a very simple, straightforward regex which correctly matches C comments. The <code>freeform</code> operator allows the entire input stream to be treated as one big line, so this works across multi-line comments.


```txr
@(freeform)
@(coll :gap 0)@notcomment@{comment /[/][*].%[*][/]/}@(end)@tail
@(output)
@(rep)@notcomment @(end)@tail
@(end)
```



### Regexes in TXR Lisp


Parse regex at run time to abstract syntax:


```sh
$ txr -p '(regex-parse "a.*b")'
(compound #\a (0+ wild) #\b)
```


Dynamically compile regex abstract syntax to regex object:


```sh
$ txr -p "(regex-compile '(compound #\a (0+ wild) #\b))"
#<sys:regex: 9c746d0>
```


Search replace with <code>regsub</code>.


```sh
$ txr -p '(regsub #/a+/ "-" "baaaaaad")'
"b-d"
```



## UNIX Shell

[[bash]] and [[ksh]] implement regular expression matching via the
<tt>[[</tt> command's <tt>=~</tt> operator.

[[ksh]] additionally allows regular expression as a flavour of general
pattern matching.


### Matching

```bash
s="I am a string"
if [[ $s =~ str..g$ ]]; then
    echo "the string ends with 'str..g'"
fi
```



### Replacing

Given these values

```bash
s="I am the original string"
re='o.*l'
repl="modified"
```


Can use regular expressions in parameter expansion

```bash
modified=${s/~(E)$re/$repl}
echo "$modified"           # I am the modified string
```


have to break apart the original string to build the modified string.

```bash
if [[ $s =~ $re ]]; then
    submatch=${BASH_REMATCH[0]}
    modified="${s%%$submatch*}$repl${s#*$submatch}"
    echo "$modified"           # I am the modified string
fi
```




## Vala


```vala

void main(){
    string sentence = "This is a sample sentence.";

    Regex a = new Regex("s[ai]mple"); // if using \n type expressions, use triple " for string literals as easy method to escape them

    if (a.match(sentence)){
        stdout.printf("\"%s\" is in \"%s\"!\n", a.get_pattern(), sentence);
    }

    string sentence_replacement = "cat";
    sentence = a.replace(sentence, sentence.length, 0, sentence_replacement);
    stdout.printf("Replaced sentence is: %s\n", sentence);
}

```


```txt

"s[ai]mple" is in "This is a sample sentence."!
Replaced sentence is: This is a cat sentence.

```



## VBScript

Replace white spaces with line breaks.

```vb
text = "I need more coffee!!!"
Set regex = New RegExp
regex.Global = True
regex.Pattern = "\s"
If regex.Test(text) Then
	WScript.StdOut.Write regex.Replace(text,vbCrLf)
Else
	WScript.StdOut.Write "No matching pattern"
End If
```

```txt
I need more coffee!!!
```

```txt

I
need
more
coffee!!!

```



## Vedit macro language

Vedit can perform searches and matching with either regular expressions, pattern matching codes or plain text. These examples use regular expressions.

Match text at cursor location:

```vedit
if (Match(".* string$", REGEXP)==0) {
    Statline_Message("This line ends with 'string'")
}
```


Search for a pattern:

```vedit
if (Search("string$", REGEXP+NOERR)) {
    Statline_Message("'string' at and of line found")
}
```


Replace:

```vedit
Replace(" a ", " another ", REGEXP+NOERR)
```



## Web 68


```web68
@1Introduction.
Web 68 has access to a regular expression module
which can compile regular expressions,
use them for matching strings,
and replace strings with the matched string.

@a@<Compiler prelude@>
BEGIN
@<Declarations@>
@<Logic at the top level@>
END
@<Compiler postlude@>

@ The local compiler requires a special prelude.

@<Compiler prel...@>=
PROGRAM rosettacode regex CONTEXT VOID
USE regex,standard

@ And a special postlude.

@<Compiler post...@>=
FINISH

@1Regular expressions.
Compile a regular expression and match a string using it.

@<Decl...@>=
STRING regexp="string$";
REF REGEX rx=rx compile(regexp);

@ Declare a string for the regular expression to match.

@<Decl...@>=
STRING to match = "This is a string";

@ Define a routine to print the result of matching.

@<Decl...@>=
OP MATCH = (REF REGEX rx,STRING match)STRING:
IF rx match(rx,match,LOC SUBEXP)
THEN "matches"
ELSE "doesn't match"
FI;

@ Check whether the regular expression matches the string.

@<Logic...@>=
print(("String """,regexp,""" ",rx MATCH to match,
       " string """,to match,"""",newline))

@ The end.
This program is processed by tang to produce Algol 68 code which has to be compiled by the a68toc compiler.
It's output is then compiled by gcc to produce a binary program. The script 'ca' provided with the Debian
package algol68toc requires the following command to process this program.
  ca -l mod rosettacoderegex.w68
That's it. The resulting binary will print
'String "string$" matches string "This is a string"'
```



## zkl

The build in RE engine is a basic one.<br/>
Strings are immutable so replacement is creation.

```zkl
var re=RegExp(".*string$");
re.matches("I am a string") //-->True
var s="I am a string thing"
re=RegExp("(string)")  // () means group, ie if you see it, save it
re.search(s,True) //-->True, .search(x,True) means search for a match, ie don't need .*
p,n:=re.matched[0] //.matched-->L(L(7,6),"string")
String(s[0,p],"FOO",s[p+n,*]) //-->"I am a FOO thing"

re.search(s,True); // using .matched clears it
m:=re.matched[1];
s.replace(m,"FOO"); // -->"I am a FOO thing"
```


Using a mutable byte bucket:

```zkl
var s=Data(0,Int,"I am a string thing");
re.search(s,True);
p,n:=re.matched[0];
s[p,n]="FOO";
s.text //-->"I am a FOO thing"
```

