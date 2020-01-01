+++
title = "Strip whitespace from a string/Top and tail"
description = ""
date = 2019-06-07T12:25:10Z
aliases = []
[extra]
id = 9867
[taxonomies]
categories = []
tags = []
+++

{{task|String manipulation}}

;Task:
Demonstrate how to strip leading and trailing whitespace from a string.

The solution should demonstrate how to achieve the following three results:

* String with leading whitespace removed
* String with trailing whitespace removed
* String with both leading and trailing whitespace removed



For the purposes of this task whitespace includes non printable characters such as the space character, the tab character, and other such characters that have no corresponding graphical representation.





## Ada


```Ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
procedure StripDemo is
   str : String := "     Jabberwocky     ";
begin
   Put_Line ("'" & Trim (str, Left) & "'");
   Put_Line ("'" & Trim (str, Right) & "'");
   Put_Line ("'" & Trim (str, Both) & "'");
end StripDemo;
```

{{out}}

```txt

'Jabberwocky     '
'     Jabberwocky'
'Jabberwocky'

```


## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.win32}}

```algol68
# returns "text" with leading non-printing characters removed #
PROC trim leading whitespace = ( STRING text )STRING:
BEGIN

    INT pos := LWB text;

    WHILE
        IF pos > UPB text
        THEN
            FALSE
        ELSE
            text[ pos ] <= " "
        FI
    DO
        pos +:= 1
    OD;

    text[ pos : ]
END; # trim leading whitespace #

# returns "text" with trailing non-printing characters removed #
PROC trim trailing whitespace = ( STRING text )STRING:
BEGIN

    INT pos := UPB text;

    WHILE
        IF pos < LWB text
        THEN
            FALSE
        ELSE
            text[ pos ] <= " "
        FI
    DO
        pos -:= 1
    OD;

    text[ : pos ]
END; # trim trailing whitespace #

# returns "text" with leading and trailing non-printing characters removed #
PROC trim whitespace = ( STRING text )STRING:
BEGIN
    trim trailing whitespace( trim leading whitespace( text ) )
END; # trim whitespace #


main:(
    STRING test = "   leading and trailing spaces surrounded this text   ";

    print( ( "trim  leading: """ + trim leading whitespace ( test ) + """", newline ) );
    print( ( "trim trailing: """ + trim trailing whitespace( test ) + """", newline ) );
    print( ( "trim     both: """ + trim whitespace         ( test ) + """", newline ) )
)
```

{{out}}

```txt

trim  leading: "leading and trailing spaces surrounded this text   "
trim trailing: "   leading and trailing spaces surrounded this text"
trim     both: "leading and trailing spaces surrounded this text"

```


=
## Version 2
=
{{works with|ALGOL 68G|2.8.3}}

A single procedure that trims leading space, trailing space, or both.


```algol68
#
  string_trim
  Trim leading and trailing whitespace from string.

  @param str A string.
  @return A string trimmed of leading and trailing white space.
#
PROC string_trim = (STRING str) STRING: (
    INT i := 1, j := 0;
    WHILE str[i] = blank DO
        i +:= 1
    OD;
    WHILE str[UPB str - j] = blank DO
        j +:= 1
    OD;
    str[i:UPB str - j]
);

test: (
    IF string_trim("   foobar") /= "foobar" THEN
        print(("string_trim('   foobar'): expected 'foobar'; actual: "
              + string_trim("   foobar"), newline)) FI;
    IF string_trim("foobar   ") /= "foobar" THEN
        print(("string_trim('foobar   '): expected 'foobar'; actual: "
              + string_trim("foobar   "), newline)) FI;
    IF string_trim("   foobar   ") /= "foobar" THEN
        print(("string_trim('   foobar   '): expected 'foobar'; actual: "
              + string_trim("   foobar   "), newline)) FI
)
```




## AppleScript

{{Trans|JavaScript}}
{{Trans|Haskell}}

```AppleScript
use framework "Foundation" -- "OS X" Yosemite onwards, for NSRegularExpression

-- STRIP WHITESPACE ----------------------------------------------------------

-- isSpace :: Char -> Bool
on isSpace(c)
    ((length of c) = 1) and regexTest("\\s", c)
end isSpace

-- stripStart :: Text -> Text
on stripStart(s)
    dropWhile(isSpace, s) as text
end stripStart

-- stripEnd :: Text -> Text
on stripEnd(s)
    dropWhileEnd(isSpace, s) as text
end stripEnd

-- strip :: Text -> Text
on strip(s)
    dropAround(isSpace, s) as text
end strip


-- TEST ----------------------------------------------------------------------
on run
    set strText to "  \t\t \n \r    Much Ado About Nothing \t \n \r  "

    script arrowed
        on |λ|(x)
            "-->" & x & "<--"
        end |λ|
    end script

    map(arrowed, [stripStart(strText), stripEnd(strText), strip(strText)])

    --     {"-->Much Ado About Nothing
    --
    --   <--", "-->
    --
    --     Much Ado About Nothing<--", "-->Much Ado About Nothing<--"}
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- dropAround :: (Char -> Bool) -> [a] -> [a]
on dropAround(p, xs)
    dropWhile(p, dropWhileEnd(p, xs))
end dropAround

-- dropWhile :: (a -> Bool) -> [a] -> [a]
on dropWhile(p, xs)
    tell mReturn(p)
        set lng to length of xs
        set i to 1
        repeat while i ≤ lng and |λ|(item i of xs)
            set i to i + 1
        end repeat
    end tell
    if i ≤ lng then
        items i thru lng of xs
    else
        {}
    end if
end dropWhile

-- dropWhileEnd :: (a -> Bool) -> [a] -> [a]
on dropWhileEnd(p, xs)
    tell mReturn(p)
        set i to length of xs
        repeat while i > 0 and |λ|(item i of xs)
            set i to i - 1
        end repeat
    end tell
    if i > 0 then
        items 1 thru i of xs
    else
        {}
    end if
end dropWhileEnd

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

-- regexTest :: RegexPattern -> String -> Bool
on regexTest(strRegex, str)
    set ca to current application
    set oString to ca's NSString's stringWithString:str
    ((ca's NSRegularExpression's regularExpressionWithPattern:strRegex ¬
        options:((ca's NSRegularExpressionAnchorsMatchLines as integer)) ¬
        |error|:(missing value))'s firstMatchInString:oString options:0 ¬
        range:{location:0, |length|:oString's |length|()}) is not missing value
end regexTest
```

{{Out}}

```AppleScript
{"-->Much Ado About Nothing

  <--", "-->

    Much Ado About Nothing<--", "-->Much Ado About Nothing<--"}
```



## AutoHotkey

AutoHotkey_L and v2 contain a [http://www.autohotkey.net/~Lexikos/AutoHotkey_L/docs/commands/Trim.htm Trim function]

```AutoHotkey
string := "   abc   "
MsgBox % clipboard := "<" LTrim(string) ">`n<" RTrim(string) ">`n<" . Trim(string) ">"
```

{{out}}

```txt
<abc   >
<   abc>
<abc>
```



## AWK

*[:graph:] is a POSIX character class for characters that are both printable and visible. The following strips the entire non-[:graph:] character set. A simplified version for the more common task of removing only spaces and tabs follows it.


```awk
function trimleft(str   ,c, out, arr) {
    c = split(str, arr, "")
    for ( i = match(str, /[[:graph:]]/); i <= c; i++)
          out = out arr[i]
    return out
}

function reverse(str    ,n, tmp, j, out) {
    n = split(str, tmp, "")
    for (j = n; j > 0; j--)
        out = out tmp[j]
    return out
}

function trimright(str) {
    return reverse(trimleft(reverse(str)))
}

function trim(str) {
    return trimright(trimleft(str))
}

BEGIN {
    str = " \x0B\t\r\n \xA0 Hellö \xA0\x0B\t\r\n "
    print "string  = |" str "|"
    print "left    = |" trimleft(str) "|"
    print "right   = |" trimright(str) "|"
    print "both    = |" trim(str) "|"
}
```


{{out|Output from 26 May 2015}}

```txt

string  = |

 ▒ Hellö ▒

 |
left    = |Hellö ▒

 |
right   = |

 ▒ Hellö|
both    = |Hellö|

```


Simplified for removing [:blank:] (spaces and tabs) or [:space:] (for [ \t\r\n\v\f] ). This method does not work using [:graph:]


```awk
function trim(str) {
    gsub(/^[[:blank:]]+/,"", str) # Remove leading
    gsub(/[[:blank:]]+$/,"", str) # Remove trailing
    gsub(/^[[:blank:]]+|[[:blank:]]+$/, "", str) # Remove both
    return str;
}
```



## BASIC

{{works with|QBasic}}

```qbasic
 mystring$=ltrim(mystring$)           ' remove leading whitespace
 mystring$=rtrim(mystring$)           ' remove trailing whitespace
 mystring$=ltrim(rtrim(mystring$))    ' remove both leading and trailing whitespace
```



## BBC BASIC


```bbcbasic
      REM Remove leading whitespace:
      WHILE ASC(A$)<=32 A$ = MID$(A$,2) : ENDWHILE

      REM Remove trailing whitespace:
      WHILE ASC(RIGHT$(A$))<=32 A$ = LEFT$(A$) : ENDWHILE

      REM Remove both leading and trailing whitespace:
      WHILE ASC(A$)<=32 A$ = MID$(A$,2) : ENDWHILE
      WHILE ASC(RIGHT$(A$))<=32 A$ = LEFT$(A$) : ENDWHILE
```



## Bracmat

Greedy pattern matching is not Bracmat's strongest field. So instead of a pattern that globs all white space characters, we have a pattern that finds the first non-whitespace character. That character, and the remainder of the subject string, constitute a left trimmed string. To do a right trim, we reverse the string, do a left trim and reverse back.

```bracmat
( ( ltrim
  =   s
    .   @( !arg
         :   ?
             (   ( %@
                 : ~( " "
                    | \a
                    | \b
                    | \n
                    | \r
                    | \t
                    | \v
                    )
                 )
                 ?
             : ?s
             )
         )
      & !s
  )
& (rtrim=.rev$(ltrim$(rev$!arg)))
& (trim=.rev$(ltrim$(rev$(ltrim$!arg))))
& (string="   \a  Hear
	 the sound?
\v

\r
")
& out$(str$("Input:[" !string "]"))
& out$(str$("ltrim:[" ltrim$!string "]"))
& out$(str$("rtrim:[" rtrim$!string "]"))
& out$(str$("trim :[" trim$!string "]"))
&
);
```

Output (Notice the effect of the ancient \a (alarm) and \v (vertical tab)):

```txt
Input:[     Hear
         the sound?
♂


]
ltrim:[Hear
         the sound?
♂


]
rtrim:[     Hear
         the sound?]
trim :[Hear
         the sound?]
```



## Burlesque



```burlesque

blsq ) "  this is a string  "t[
"this is a string  "
blsq ) "  this is a string  "t]
"  this is a string"
blsq ) "  this is a string  "tt
"this is a string"

```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

char *rtrim(const char *s)
{
  while( isspace(*s) || !isprint(*s) ) ++s;
  return strdup(s);
}

char *ltrim(const char *s)
{
  char *r = strdup(s);
  if (r != NULL)
  {
    char *fr = r + strlen(s) - 1;
    while( (isspace(*fr) || !isprint(*fr) || *fr == 0) && fr >= r) --fr;
    *++fr = 0;
  }
  return r;
}

char *trim(const char *s)
{
  char *r = rtrim(s);
  char *f = ltrim(r);
  free(r);
  return f;
}

const char *a = "     this is a string      ";

int main()
{
  char *b = rtrim(a);
  char *c = ltrim(a);
  char *d = trim(a);

  printf("'%s'\n'%s'\n'%s'\n", b, c, d);

  free(b);
  free(c);
  free(d);
  return 0;
}
```



## C++


```cpp
#include <boost/algorithm/string.hpp>
#include <string>
#include <iostream>

int main( ) {
   std::string testphrase( "    There are unwanted blanks here!    " ) ;
   std::string lefttrimmed = boost::trim_left_copy( testphrase ) ;
   std::string righttrimmed = boost::trim_right_copy( testphrase ) ;
   std::cout << "The test phrase is :" << testphrase << "\n" ;
   std::cout << "Trimmed on the left side :" << lefttrimmed << "\n" ;
   std::cout << "Trimmed on the right side :" << righttrimmed << "\n" ;
   boost::trim( testphrase ) ;
   std::cout << "Trimmed on both sides :" <<  testphrase  << "\n" ;
   return 0 ;
}
```

{{out}}
<PRE>The test phrase is :    There are unwanted blanks here!
Trimmed on the left side :There are unwanted blanks here!
Trimmed on the right side :    There are unwanted blanks here!
Trimmed on both sides :There are unwanted blanks here!</PRE>

=={{header|C sharp|C#}}==

```csharp
using System;

public class TrimExample
{
    public static void Main(String[] args)
    {
        const string toTrim = " Trim me ";
        Console.WriteLine(Wrap(toTrim.TrimStart()));
        Console.WriteLine(Wrap(toTrim.TrimEnd()));
        Console.WriteLine(Wrap(toTrim.Trim()));
    }

    private static string Wrap(string s)
    {
        return "'" + s + "'";
    }
}
```

{{out}}

```txt

'Trim me '
' Trim me'
'Trim me'

```



## COBOL

{{works with|GNU Cobol|2.0}}

```cobol
DISPLAY "'" FUNCTION TRIM(str, LEADING) "'"
DISPLAY "'" FUNCTION TRIM(str, TRAILING) "'"
DISPLAY "'" FUNCTION TRIM(str) "'"
```


{{works with|IBM ILE COBOL}}

```cobol
DISPLAY "'" FUNCTION TRIML(str) "'"
DISPLAY "'" FUNCTION TRIMR(str) "'"
DISPLAY "'" FUNCTION TRIM(str) "'"
```



## Clojure


```clojure

(use 'clojure.string)
(triml "   my string   ")
=> "my string   "
(trimr "   my string   ")
=> "   my string"
(trim " \t\r\n my string \t\r\n  ")
=> "my string"

```



## Common Lisp


```lisp
; Common whitespace characters
(defvar *whitespace* '(#\Space #\Newline #\Tab))

(defvar str "   foo bar     baz  ")

(string-trim *whitespace* str)
; -> "foo bar     baz"

(string-left-trim *whitespace* str)
; -> "foo bar     baz  "

(string-right-trim *whitespace* str)
; -> "   foo bar     baz"

; Whitespace characters defined by Unicode for
; implementations which support it (e.g. CLISP, SBCL).
; (see http://www.unicode.org/Public/UCD/latest/ucd/PropList.txt)
(defvar *unicode-whitespace*
  '(#\u0009 #\u000a #\u000b #\u000c #\u000d
    #\u0020 #\u0085 #\u00a0 #\u1680 #\u2000
    #\u2001 #\u2002 #\u2003 #\u2004 #\u2005
    #\u2006 #\u2007 #\u2008 #\u2009 #\u200a
    #\u2028 #\u2029 #\u202f #\u205f #\u3000))

(defvar unicode-str
  (format nil "~C~Cfoo~Cbar~Cbaz~C~C"
          #\u2000 #\u2003 #\u0020 #\u00a0 #\u0009 #\u202f))

(string-trim *unicode-whitespace* unicode-str)
; -> "foo bar baz"

(string-left-trim *unicode-whitespace* unicode-str)
; -> "foo bar baz     "

(string-right-trim *unicode-whitespace* unicode-str)
; -> "  foo bar baz"
```



## Crystal


```Ruby

def strip_whitepace(s)
    puts s.lstrip()
    puts s.rstrip()
    puts s.strip()
end

strip_whitepace("\t hello \t")
# => hello
# =>      hello
# => hello

```



## D


```d
import std.stdio, std.string;

void main() {
    auto s = " \t \r \n String with spaces  \t  \r  \n  ";
    assert(s.stripLeft() == "String with spaces  \t  \r  \n  ");
    assert(s.stripRight() == " \t \r \n String with spaces");
    assert(s.strip() == "String with spaces");
}
```


=={{header|Delphi}}/{{header|Pascal}}==

```Delphi
program StripWhitespace;

{$APPTYPE CONSOLE}

uses SysUtils;

const
  TEST_STRING = '     String with spaces     ';
begin
  Writeln('"' + TEST_STRING + '"');
  Writeln('"' + TrimLeft(TEST_STRING) + '"');
  Writeln('"' + TrimRight(TEST_STRING) + '"');
  Writeln('"' + Trim(TEST_STRING) + '"');
end.
```



## DWScript

{{Trans|Delphi}}

```Delphi
const TEST_STRING = '     String with spaces     ';

PrintLn('"' + TEST_STRING + '"');
PrintLn('"' + TrimLeft(TEST_STRING) + '"');
PrintLn('"' + TrimRight(TEST_STRING) + '"');
PrintLn('"' + Trim(TEST_STRING) + '"');
```



## EchoLisp


```scheme

(define witt
   "                 The limits of my world are the limits of my langage.         ")
(string->html (string-trim witt))
   → "The limits of my world are the limits of my langage."
(string->html (string-trim-left witt))
   → "The limits of my world are the limits of my langage.         "
(string->html (string-trim-right witt))
   → "                 The limits of my world are the limits of my langage."

```


## Elena

ELENA 4.x :

```elena
import extensions;

public program()
{
    var toTrim := " Trim me ";
    console.printLine("'", toTrim.trimLeft(),"'");
    console.printLine("'", toTrim.trimRight(),"'");
    console.printLine("'", toTrim.trim(),"'");
}
```

{{out}}

```txt

'Trim me '
' Trim me'
'Trim me'

```



## Elixir


```elixir
str = "\n \t foo \n\t bar \t \n"
IO.inspect String.strip(str)
IO.inspect String.rstrip(str)
IO.inspect String.lstrip(str)
```

{{out}}

```txt

"foo \n\t bar"
"\n \t foo \n\t bar"
"foo \n\t bar \t \n"

```



## Emacs Lisp


### trim left


```Emacs Lisp

(defun trim-l (str)
  (replace-regexp-in-string "^ +"  "" str) )

(setq str "    left between right  ")
(insert (trim-l str) )


```

<b>Output:</b>

```txt

left between right

```



### trim right


```Emacs Lisp

(defun trim-r (str)
  (replace-regexp-in-string " +$"  "" str) )

(setq str "    left between right  ")
(insert (trim-r str) )

```

<b>Output:</b>

```txt

    left between right

```


### trim version 1


```Emacs Lisp

(defun trim (str)
  (trim-l (trim-r str) ))

(setq str "    left between right  ")
(insert (trim str) )

```

<b>Output:</b>

```txt

left between right

```


### trim version 2


```Emacs Lisp

(defun trim (str)
  (mapconcat 'identity (split-string str)  " ") )

(setq str "    left between right  ")
(insert (trim str) )

```

<b>Output:</b>

```txt

left between right

```



## Erlang


```erlang
% Implemented by Arjun Sunel
1> string:strip("   Hello World!   ", left). %remove leading whitespaces
"Hello World!   "

2> string:strip("   Hello World!   ", right). % remove trailing whitespaces
"   Hello World!"

3> string:strip("   Hello World!   ", both).  % remove both leading and trailing whitespace
"Hello World!"

```



## Euphoria

{{works with|Euphoria|4.0.3, 4.0.0 and later}}

A string (sequence) 'A  B  C' is surrounded by unwanted characters including spaces. The trim() function Trims "all items in the supplied set from both the left end (head/start) and right end (tail/end) of a sequence." It's part of the standard library, std/text.e .

Special characters in a string literal are typed 'escaped' with a '\' followed by one character. Other special characters are written
using escaped hexadecimal , example : \x04 to represent hexadecimal ascii 04 or \u2A7C for 4-digit UTF, or more than two digit ascii characters : \u0127.


```euphoria
include std/console.e
include std/text.e

sequence removables = " \t\n\r\x05\u0234\" "
sequence extraSeq = "  \x05\r \" A  B  C  \n \t\t  \u0234 \r\r \x05   "

extraSeq = trim(extraSeq,removables) --the work is done by the trim function

--only output programming next :
printf(1, "String Trimmed is now: %s \r\n", {extraSeq} ) --print the resulting string to screen

for i = 1 to length(extraSeq) do --loop over each character in the sequence.
    printf(1, "String element %d", i) --to look at more detail,
    printf(1, " : %d\r\n", extraSeq[i])--print integer values(ascii) of the string.
end for

any_key()
```

Output:
```txt

String Trimmed is now: A  B  C
String element 1 : 65
String element 2 : 32
String element 3 : 32
String element 4 : 66
String element 5 : 32
String element 6 : 32
String element 7 : 67
Press Any Key to continue...

```



=={{header|F_Sharp|F#}}==

```fsharp>[<EntryPoint
]
let main args =
    printfn "%A" (args.[0].TrimStart())
    printfn "%A" (args.[0].TrimEnd())
    printfn "%A" (args.[0].Trim())
    0
```


```txt

>rosetta " a string "
"a string "
" a string"
"a string"
```



## Factor


```factor

USING: unicode.categories;
"     test string    " [ blank? ] trim        ! leading and trailing
"     test string    " [ blank? ] trim-head   ! only leading
"     test string    " [ blank? ] trim-tail   ! only trailing

```




## Forth

Modern Forth advocates the use of "stack strings". These are simply the memory address of the text on the stack followed by the length of the string on the stack. The advantage of using stack strings is that string manipulation is done without resorting to copying the strings to temporary memory. This makes string manipulation very fast. The values left on the stack are suitable as input arguments for further processing so function concatenation is possible to create more complex functions.

After the string processing is completed on the stack the program can print or copy back to memory as required.


```forth
: -leading ( addr len -- addr' len' ) \ called "minus-leading"
    begin
      over c@ bl =  \ fetch character at addr, test if blank (space)
    while
     \ cut 1 leading character by incrementing address & decrementing length
      1 /string      \ "cut-string"
   repeat ;
```

The counterpart function "-trailing" is normally part of a standard forth system,
so by concatenating both we create a "STRIP" function.
<lang> : strip ( addr len -- addr' len') -leading -trailing ;
```

Test at the Forth console

```txt
: mystring s"        Trim this string.        "  ;

mstring type        Trim this string.       ok
mstring -leading type Trim this string.        ok
mystring -trailing type        Trim this string. ok
mytring strip type Trim this string. ok
```



## Fortran

The problem is that Fortran does not offer a "string" type, which is to say a store of characters ''plus'' a current length. Leaving aside later Fortrans where one can create a data type and associated operations, one must deal with CHARACTER style items, though they were not available in early Fortran. Thus, <code>CHARACTER*(60) TEXT</code> creates a variable capable of storing sixty characters and there is no "varying" attribute as in pl/i to signify a current length as well. The intrinsic function LEN(TEXT) returns 60, so it is really the "size" function. Functions can return CHARACTER types, but, only of a specified size, fixed at compile time. It is common then to have many trailing spaces in such variables. An expression <code>TEXT(start:stop)</code> means select characters ''start'' to ''stop'' of TEXT.

A function LEN_TRIM(x) may be supplied, returning the index of the last non-blank character. And only of spaces: tabs and suchlike are not blanks. Thus, to remove trailing spaces, <code>TEXT(1:LEN_TRIM(TEXT))</code> would do the trick. There may also be available an intrinsic function TRIM, which removes leading and trailing spaces, thus <code>TRIM(TEXT)</code> and which does so for any length of TEXT and any length of result, unlike user-written functions that have a fixed result length.

Otherwise, one must write integer functions such as ISTNB(x) and LSTNB(x) to return the first and last non-blank character of ''x'' and use them in something like <code>TEXT(ISTNB(TEXT):LSTNB(TEXT))</code> wherever one wants the trimmed content of TEXT. Correspondingly <code>TEXT(ISTNB(TEXT):LEN(TEXT))</code> to omit leading spaces and <code>TEXT(1:LSTNB(TEXT))</code> to omit trailing spaces. Such user-written functions can make their own choices about tabs and any other special characters.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Const whitespace = !" \t\n\v\f\r"

Dim s  As String = !"  \tRosetta Code \v\f\r\n"
Dim s1 As String = LTrim (s, Any whitespace)
Dim s2 As String = RTrim (s, Any whitespace)
Dim s3 As String = Trim  (s, Any whitespace)

' Under Windows console :
' "vertical tab" displays as ♂
' "form feed" displays as ♀
' the other whitespace characters do what it says on the tin

Print "Untrimmed"         , "=> "; s
Print "Left Trimmed"      , "=> "; s1
Print "Right Trimmed"     , "=> "; s2
Print "Fully Trimmed"     , "=> "; s3
Print
Print "Untrimmed"         , "=>  Length = "; Len(s)
Print "Left trimmed"      , "=>  Length = "; Len(s1)
Print "Right trimmed"     , "=>  Length = "; Len(s2)
Print "Fully trimmed"     , "=>  Length = "; Len(s3)
Sleep
```


{{out}}

```txt

Untrimmed     =>        Rosetta Code ♂♀

Left Trimmed  => Rosetta Code ♂♀

Right Trimmed =>        Rosetta Code
Fully Trimmed => Rosetta Code

Untrimmed     =>  Length =  20
Left trimmed  =>  Length =  17
Right trimmed =>  Length =  15
Fully trimmed =>  Length =  12

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=62e56fea0f74819daa3d3a548869fa90 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sString As String = "    Hello world!    "

Print sString & "\tString length = " & Len(sString) & " - Original string"
Print LTrim(sString) & "\tString length = " & Len(LTrim(sString)) & " - String with leading whitespace removed"
Print RTrim(sString) & "\tString length = " & Len(RTrim(sString)) & " - String with trailing whitespace removed"
Print Trim(sString) & "\t\tString length = " & Len(Trim(sString)) & " - String with both leading and trailing whitespace removed"

End
```

Output:

```txt

    Hello world!        String length = 20 - Original string
Hello world!            String length = 16 - String with leading whitespace removed
    Hello world!        String length = 16 - String with trailing whitespace removed
Hello world!            String length = 12 - String with both leading and trailing whitespace removed

```



## Go


```go
package main

import (
    "fmt"
    "strings"
    "unicode"
)

var simple = `
    simple   `

func main() {
    show("original", simple)
    show("leading ws removed", strings.TrimLeftFunc(simple, unicode.IsSpace))
    show("trailing ws removed", strings.TrimRightFunc(simple, unicode.IsSpace))
    // equivalent to strings.TrimFunc(simple, unicode.IsSpace)
    show("both removed", strings.TrimSpace(simple))
}

func show(label, str string) {
    fmt.Printf("%s: |%s| %v\n", label, str, []rune(str))
}
```

Example text is shows a leading linefeed and tab, and three trailing spaces.  The code uses the Unicode definition of whitespace.  Other defintions could be implemented with a custom function given to TrimXFunc.

Output below shows the text surrounded by vertical bars to show the extent of whitespace, followed by a list of the character values in the string, to show exactly what whitespace is present.

```txt

original: |
        simple   | [10 9 115 105 109 112 108 101 32 32 32]
leading ws removed: |simple   | [115 105 109 112 108 101 32 32 32]
trailing ws removed: |
        simple| [10 9 115 105 109 112 108 101]
both removed: |simple| [115 105 109 112 108 101]

```



## Groovy

Solution uses StringUtils class from [http://commons.apache.org/lang/ Apache Commons "Lang" library]:

```groovy
//Grape setup to get library
@Grab('org.apache.commons:commons-lang3:3.0.1')
import static org.apache.commons.lang3.StringUtils.*

def abc = '\r\n\t  abc  \r\n\t'

def printTest = {
    println ('|' + it + '|')
}

println 'Unstripped\n------------'
printTest abc

println '
### ======
\n\nStripped\n------------'
printTest strip(abc)

println '
### ======
\n\nLeft Stripped\n------------'
printTest stripStart(abc, null)

println '
### ======
\n\nRight Stripped\n------------'
printTest stripEnd(abc, null)
println '
### ======
'
```

{{out}}

```txt
Unstripped
------------
|
	  abc
	|

### ======


Stripped
------------
|abc|

### ======


Left Stripped
------------
|abc
	|

### ======


Right Stripped
------------
|
	  abc|

### ======

```



## Haskell


```haskell
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

trimLeft :: String -> String
trimLeft = dropWhile isSpace

trimRight :: String -> String
trimRight = dropWhileEnd isSpace

trim :: String -> String
trim = trimLeft . trimRight
```


=={{header|Icon}} and {{header|Unicon}}==
This solution takes the phrase "other such characters that have no corresponding graphical representation" quite literallly.

```Unicon
procedure main()
    unp := &cset[1+:32]++' \t'++&cset[127:0]   # all 'unprintable' chars
    s := " Hello, people of earth!  	"
    write("Original:      '",s,"'")
    write("leading trim:  '",reverse(trim(reverse(s),unp)),"'")
    write("trailing trim: '",trim(s,unp),"'")
    write("full trim:     '",reverse(trim(reverse(trim(s,unp)),unp)),"'")
end
```

{{out|Sample run}}

```txt
->trim
Original:      ' Hello, people of earth!        '
leading trim:  'Hello, people of earth!         '
trailing trim: ' Hello, people of earth!'
full trim:     'Hello, people of earth!'
->
```



## J

Note: The <code>quote</code> verb is only used to enclose the resulting string in single quotes so the beginning and end of the new string are visible.

```j
   require 'strings'                      NB. the strings library is automatically loaded in versions from J7 on
   quote dlb '  String with spaces   '    NB. delete leading blanks
'String with spaces   '
   quote dtb '  String with spaces   '    NB. delete trailing blanks
'  String with spaces'
   quote dltb '  String with spaces   '   NB. delete leading and trailing blanks
'String with spaces'
```

In addition <code>deb</code> (delete extraneous blanks) will trim both leading and trailing blanks as well as replace consecutive spaces within the string with a single space.

```j
   quote deb '  String   with spaces   '   NB. delete extraneous blanks
'String with spaces'
```

These existing definitions can be easily amended to include whitespace other than spaces if desired.

```j
whpsc=: ' ',TAB                                NB. define whitespace as desired
dlws=: }.~ (e.&whpsc i. 0:)                    NB. delete leading whitespace (spaces and tabs)
dtws=: #~ ([: +./\. -.@:e.&whpsc)              NB. delete trailing whitespace
dltws=: #~ ([: (+./\ *. +./\.) -.@:e.&whpsc)   NB. delete leading & trailing whitespace
dews=: #~ (+. (1: |. (> </\)))@(-.@:e.&whpsc)  NB. delete extraneous whitespace
```



## Java

Left trim and right trim taken from [http://www.fromdev.com/2009/07/playing-with-java-string-trim-basics.html here].
<code>Character.isWhitespace()</code> returns true if the character given is one of the following [[Unicode]] characters: '\u00A0', '\u2007', '\u202F', '\u0009', '\u000A', '\u000B', '\u000C', '\u000D',  '\u001C', '\u001D', '\u001E', or '\u001F'.

```java

public class Trims{
   public static String ltrim(String s){
      int i = 0;
      while (i < s.length() && Character.isWhitespace(s.charAt(i))){
         i++;
      }
      return s.substring(i);
   }

   public static String rtrim(String s){
      int i = s.length() - 1;
      while (i > 0 && Character.isWhitespace(s.charAt(i))){
         i--;
      }
      return s.substring(0, i + 1);
   }

   public static void main(String[] args){
      String s = " \t \r \n String with spaces  \t  \r  \n  ";
      System.out.println(ltrim(s));
      System.out.println(rtrim(s));
      System.out.println(s.trim()); //trims both ends
   }
}
```



## Javascript

{{works with|Node.js}}
{{works with|ECMAScript standard|2015}}

```javascript
{
    let s = " \t String with spaces  \t  ";
    // a future version of ECMAScript will have trimStart().  Some current
    // implementations have trimLeft().
    console.log("original: '" + s + "'");
    console.log("trimmed left: '" + s.replace(/^\s+/,'') + "'");
    // a future version of ECMAScript will have trimEnd().  Some current
    // implementations have trimRight().
    console.log("trimmed right: '" + s.replace(/\s+$/,'') + "'");
    console.log("trimmed both: '" + s.trim() + "'");
 }
```

{{Output}}

```txt
original: ' 	 String with spaces  	  '
trimmed left: 'String with spaces  	  '
trimmed right: ' 	 String with spaces'
trimmed both: 'String with spaces'
```



Or, composing from generic primitives:

{{Trans|Haskell}}

```JavaScript
(() => {
    'use strict';

    // stripStart :: Text -> Text
    let stripStart = s => dropWhile(isSpace, s);

    // stripEnd :: Text -> Text
    let stripEnd = s => dropWhileEnd(isSpace, s);

    // strip :: Text -> Text
    let strip = s => dropAround(isSpace, s);
    // OR: let strip = s => s.trim();



    // GENERIC FUNCTIONS

    // dropAround :: (Char -> Bool) -> Text -> Text
    let dropAround = (p, s) => dropWhile(p, dropWhileEnd(p, s));

    // dropWhile :: (a -> Bool) -> [a] -> [a]
    let dropWhile = (p, xs) => {
        for (var i = 0, lng = xs.length;
            (i < lng) && p(xs[i]); i++) {}
        return xs.slice(i);
    }

    // dropWhileEnd :: (Char -> Bool) -> Text -> Text
    let dropWhileEnd = (p, s) => {
        for (var i = s.length; i-- && p(s[i]);) {}
        return s.slice(0, i + 1);
    }

    // isSpace :: Char -> Bool
    let isSpace = c => /\s/.test(c);

    // show :: a -> String
    let show = x => JSON.stringify(x, null, 2);


    // TEST

    let strText = "  \t\t \n \r    Much Ado About Nothing \t \n \r  ";

    return show([stripStart, stripEnd, strip]
        .map(f => '-->' + f(strText) + '<--'));

})();

```


{{Out}}

```txt
[
  "-->Much Ado About Nothing \t \n \r  <--",
  "-->  \t\t \n \r    Much Ado About Nothing<--",
  "-->Much Ado About Nothing<--"
]
```



## jq

{{Works with|jq|>1.4}}

Recent versions of jq (since July 2014) support PCRE regex operations;
\p{_} character classes are also supported. The following accordingly uses \p{Cc} as that corresponds to ASCII 0x00–0x1F.

Notice that since jq strings are JSON strings, one must, for example, write "\\s" for the regex '\s'.

```jq
def lstrip: sub( "^[\\s\\p{Cc}]+"; "" );

def rstrip: sub( "[\\s\\p{Cc}]+$"; "" );

def strip: lstrip | rstrip;
```

'''Examples''':

```jq
def demo:
  "lstrip: \(lstrip)",
  "rstrip: \(rstrip)",
  "strip: \(strip)" ;

(" \t \r \n String with spaces \t  \r  \n  ",
 "� <- control A",
 "\u0001 \u0002 <- ^A ^B"
)  | demo

```

{{Out}}

```sh
$ jq -n -f Strip_whitespace_top_tail.jq
"lstrip: String with spaces \t  \r  \n  "
"rstrip:  \t \r \n String with spaces"
"strip: String with spaces"
"rstrip: \u0001 <- control A"
"strip: <- control A"
"lstrip: <- ^A ^B"
"rstrip: \u0001 \u0002 <- ^A ^B"
"strip: <- ^A ^B"
```



## Jsish

Using echo-mode unitTest lines. Lines starting with semicolon are echoed and captured during test.


```javascript
#!/usr/bin/env jsish
/* Strip whitespace from string, in Jsi */
var str = ' \n \t String with whitespace \t \n ';
;'Original';
;str;


;'Default trim characters are space, tab, newline, carriage return';
;'trimLeft, remove leading characters';
;str.trimLeft();

;'trimRight, remove trailing characters';
;str.trimRight();

;'trim, removes leading and trailing';
;str.trim();

/*
=!EXPECTSTART!=
'Original'
str ==>
     String with whitespace

'Default trim characters are space, tab, newline, carriage return'
'trimLeft, remove leading characters'
str.trimLeft() ==> String with whitespace

'trimRight, remove trailing characters'
str.trimRight() ==>
     String with whitespace
'trim, removes leading and trailing'
str.trim() ==> String with whitespace
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish -u stripWhitespace.jsi
[PASS] stripWhitespace.jsi

prompt$ jsish --U stripWhitespace.jsi
'Original'
str ==>
         String with whitespace

'Default trim characters are space, tab, newline, carriage return'
'trimLeft, remove leading characters'
str.trimLeft() ==> String with whitespace

'trimRight, remove trailing characters'
str.trimRight() ==>
         String with whitespace
'trim, removes leading and trailing'
str.trim() ==> String with whitespace
```



## Julia

{{trans|Python}}

```julia>julia
 s = " \t \r \n String with spaces  \t  \r  \n  "
" \t \r \n String with spaces  \t  \r  \n  "

julia> lstrip(s)
"String with spaces  \t  \r  \n  "

julia> rstrip(s)
" \t \r \n String with spaces"

julia> strip(s)
"String with spaces"
```



## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    val s = "  \tRosetta Code \r\n"
    println("Untrimmed       => $s")
    println("Left Trimmed    => ${s.trimStart()}")
    println("Right Trimmed   => ${s.trimEnd()}")
    println("Fully Trimmed   => ${s.trim()}")
}
```


{{out}}

```txt

Untrimmed       =>      Rosetta Code

Left Trimmed    => Rosetta Code

Right Trimmed   =>      Rosetta Code
Fully Trimmed   => Rosetta Code

```



## Lasso

Note that the example below uses the retarget operator ("&") to output as the trim method modifies the string in place.

The '>' and '<' strings have been included in this example to demonstrate the whitespace trim.

```Lasso
// String with leading whitespace removed
'>' + ('  \t   Hello')->trim& + '<'

// String with trailing whitespace removed
'>' + ('Hello  \t   ')->trim& + '<'

// String with both leading and trailing whitespace removed
'>' + ('   \t  Hello  \t   ')->trim& + '<'
```



## Liberty BASIC


```lb
a$="   This is a test   "

'LB TRIM$ removes characters with codes 0..31 as well as a space(code 32)
'So these versions of ltrim rtrim remove them too
'a$="   "+chr$(31)+"This is a test"+chr$(31)+"   "

print "Source line"
print ">";a$;"<"
print "Strip left"
print ">";ltrim$(a$);"<"
print "Strip right"
print ">";rtrim$(a$);"<"
print "Strip both"
print ">";trim$(a$);"<"

end

function ltrim$(a$)
    c$=trim$(a$+".")
    ltrim$ = mid$(c$, 1, len(c$)-1)
end function

function rtrim$(a$)
    c$=trim$("."+a$)
    rtrim$ = mid$(c$, 2)
end function


```



## Logtalk

Using atoms for representing strings and assuming an ASCII text encoding:

```logtalk

:- object(whitespace).

    :- public(trim/4).

    trim(String, TrimLeft, TrimRight, TrimBoth) :-
        trim_left(String, TrimLeft),
        trim_right(String, TrimRight),
        trim_right(TrimLeft, TrimBoth).

    trim_left(String, TrimLeft) :-
        atom_codes(String, Codes),
        trim(Codes, TrimCodes),
        atom_codes(TrimLeft, TrimCodes).

    trim_right(String, TrimRight) :-
        atom_codes(String, Codes),
        list::reverse(Codes, ReverseCodes),
        trim(ReverseCodes, ReverseTrimCodes),
        list::reverse(ReverseTrimCodes, TrimCodes),
        atom_codes(TrimRight, TrimCodes).

    trim([], []).
    trim([InCode| InCodes], OutCodes) :-
        (   InCode =< 32 ->
            trim(InCodes, OutCodes)
        ;   OutCodes = [InCode| InCodes]
        ).

:- end_object.

```

Sample output:

```text

| ?- whitespace::trim('\n\t   Rosetta Code  \t\n', TrimLeft, TrimRight, TrimBoth).
TrimLeft = 'Rosetta Code  \t\n',
TrimRight = '\n\t   Rosetta Code',
TrimBoth = 'Rosetta Code'
yes

```



## Lua


```lua
str = " \t \r \n String with spaces  \t  \r  \n  "

print( string.format( "Leading whitespace removed: %s", str:match( "^%s*(.+)" ) ) )
print( string.format( "Trailing whitespace removed: %s", str:match( "(.-)%s*$" ) ) )
print( string.format( "Leading and trailing whitespace removed: %s", str:match( "^%s*(.-)%s*$" ) ) )
```


## M2000 Interpreter


```M2000 Interpreter

Module CheckIt {
	filter$=chr$(0)
	for i=1 to 31:filter$+=chr$(i):next
	a$=chr$(9)+"    There are unwanted blanks here!    "+chr$(9)
	a$=filter$(a$,filter$) ' exclude non printable characters

	\\ string encoded as UTF16LE
	Print Len(a$)=39
	Print(ltrim$(a$))
	Print(rtrim$(a$))
	Print(trim$(a$))
	\\ string encoded as ANSI base to Locale
	oldlocale=Locale
	Locale 1033
	a$=str$(a$)
	Print Len(a$)=19.5  ' unit for length is a word (2 bytes), so 19.5 means  19.5*2 bytes= 39 bytes
	PrintAnsi(ltrim$(a$ as byte))
	PrintAnsi(rtrim$(a$ as byte))
	PrintAnsi(trim$(a$ as byte))
	Locale oldlocale
	Sub Print(a$)
		Print "*"+a$+"*"
	End Sub
	Sub PrintAnsi(a$)
		Print "*"+chr$(a$)+"*"
	End Sub
}
Checkit

```

{{out}}
<pre style="height:30ex;overflow:scroll">
    True
*There are unwanted blanks here!    *
*    There are unwanted blanks here!*
*There are unwanted blanks here!*
    True
*There are unwanted blanks here!    *
*    There are unwanted blanks here!*
*There are unwanted blanks here!*

```



## Maple


```Maple
str := " \t \r \n String with spaces  \t  \r  \n  ";

with(StringTools):

TrimLeft(str);
TrimRight(str);
Trim(str);
```



## Mathematica


```Mathematica
StringTrim[" \n\t    string with spaces   \n   \t   "]
```


=={{header|MATLAB}} / {{header|Octave}}==

```matlab
% remove trailing whitespaces
    str = str(1:find(~isspace(str),1,'last'));
% remove leading whitespaces
    str = str(find(~isspace(str),1):end);

% removes leading and trailing whitespaces, vectorized version
    f = ~isspace(str);
    str = str(find(f,1,'first'):find(f,1,'last');

% a built-in function, removes leading and trailing whitespaces
    str = strtrim(str);
```



## Mercury


```mercury
:- module top_and_tail.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list, string.

main(!IO) :-
   TestPhrase = "\t\r\n String with spaces \t\r\n ",
   io.format("leading ws removed: %s\n", [s(lstrip(TestPhrase))], !IO),
   io.format("trailing ws removed: %s\n", [s(rstrip(TestPhrase))], !IO),
   io.format("both removed: %s\b", [s(strip(TestPhrase))], !IO).
```



## Nemerle


```Nemerle
def str = "\t\n\t   A string with\nwhitespace\n\n\t   ";
WriteLine(str.TrimStart());
WriteLine(str.TrimEnd());
WriteLine(str.Trim());      // both ends at once, of course, internal whitespace is preserved in all 3
```



## NetRexx

NetRexx provides a <tt>strip()</tt> method which can be used to remove all of a single character from the head and/or tail of a string.  To remove ''all'' whitespace characters from a string requires a little more work:

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method stripWhitespace(sstring, soption = 'BOTH') public static
  wsChars = getWhitspaceCharacterString()
  po1 = sstring.verify(wsChars)
  if po1 = 0 then do
    sstring = ''
    end
  else do
    po2 = sstring.length - (sstring.reverse().verify(wsChars) - 1) + 1
    ss = sstring
    parse ss sl =(po1) sm =(po2) st
    if po1 <= 1 then sl = ''
    soption = soption.upper()
    select
      when 'BOTH'.abbrev(soption, 1)     then sstring = sm
      when 'LEADING'.abbrev(soption, 1)  then sstring = sm || st
      when 'TRAILING'.abbrev(soption, 1) then sstring = sl || sm
      otherwise                               sstring = sm
      end
    end
  return sstring

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/**
 * Create an array containing a useful subset of unicode whitespace characters
 *
 * @return an array of unicode whitespace characters
 * @see http://www.fileformat.info/info/unicode/category/index.htm
 */
method getWhitspaceCharacters() public static returns Rexx[]
  wsChars = [ -
    /* LINE SEPARATOR              [Zi] */ '\u2028', /* PARAGRAPH SEPARATOR         [Zp] */ '\u2029', -
    /* SPACE                       [Zs] */ '\u0020', /* NO-BREAK SPACE              [Zs] */ '\u00A0', -
    /* OGHAM SPACE MARK            [Zs] */ '\u1680', /* MONGOLIAN VOWEL SEPARATOR   [Zs] */ '\u180E', -
    /* EN QUAD                     [Zs] */ '\u2000', /* EM QUAD                     [Zs] */ '\u2001', -
    /* EN SPACE                    [Zs] */ '\u2002', /* EM SPACE                    [Zs] */ '\u2003', -
    /* THREE-PER-EM SPACE          [Zs] */ '\u2004', /* FOUR-PER-EM SPACE           [Zs] */ '\u2005', -
    /* SIX-PER-EM SPACE            [Zs] */ '\u2006', /* FIGURE SPACE                [Zs] */ '\u2007', -
    /* PUNCTUATION SPACE           [Zs] */ '\u2008', /* THIN SPACE                  [Zs] */ '\u2009', -
    /* HAIR SPACE                  [Zs] */ '\u200A', /* NARROW NO-BREAK SPACE       [Zs] */ '\u202F', -
    /* MEDIUM MATHEMATICAL SPACE   [Zs] */ '\u3000', /* IDIOGRAPHIC SPACE           [Zs] */ '\u205F', -
    /* BACKSPACE                   [Cc] */ '\u0008', /* CHARACTER TABULATION, HT    [Cc] */ '\u0009', -
    /* LINE FEED (LF)              [Cc] */ '\u000A', /* LINE TABULATION (VT)        [Cc] */ '\u000B', -
    /* FORM FEED (FF)              [Cc] */ '\u000C', /* CARRIAGE RETURN (CR)        [Cc] */ '\u000D', -
    /* INFORMATION SEPARATOR FOUR  [Cc] */ '\u001C', /* INFORMATION SEPARATOR THREE [Cc] */ '\u001D', -
    /* INFORMATION SEPARATOR TWO   [Cc] */ '\u001E', /* INFORMATION SEPARATOR ONE   [Cc] */ '\u001F', -
    /* NEXT LINE (NEL)             [Cc] */ '\u0085', -
    /* ZERO WIDTH SPACE            [Cf] */ '\u200B', /* ZERO WIDTH NO-BREAK SPACE   [Cf] */ '\uFEFF'  -
    ]
  return wsChars

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method getWhitspaceCharacterString() public static returns Rexx
  wsCharstring = ''
  loop wsChar over getWhitspaceCharacters()
    wsCharstring = wsCharstring || wsChar
    end wsChar
  return wsCharstring

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) public static
  sstrings = [ -
    ' \u0020 \u0009 \u000D\r \n \u2029\uFEFF1 String with white space.  \t  \r  \n \u1680  ', -
    ' \t 2 String with white space. \t   ', -
    '3 String with white space. \t', -
    ' \t 4 String with white space.', -
    '5 String with white space.', -
    '\u0020\u0009\u2029\uFEFF\u1680\u2006', -
    '   ', -
    '' -
    ]
  loop sstringO over sstrings
    sstringL = stripWhitespace(sstringO, 'l')
    sstringT = stripWhitespace(sstringO, 't')
    sstringB = stripWhitespace(sstringO)
    say '  Original string  ['sstringO']'
    say '    strip leading  ['sstringL']'
    say '    strip trailing ['sstringT']'
    say '    strip both     ['sstringB']'
    say
    end sstringO

  return

```

'''Output:'''
<pre style="height: 25ex; overflow:scroll;">
  Original string  [
  ﻿1 String with white space.
    ]
    strip leading  [1 String with white space.
    ]
    strip trailing [
 ﻿1 String with white space.]
    strip both     [1 String with white space.]

  Original string  [ 	 2 String with white space. 	   ]
    strip leading  [2 String with white space. 	   ]
    strip trailing [ 	 2 String with white space.]
    strip both     [2 String with white space.]

  Original string  [3 String with white space. 	]
    strip leading  [3 String with white space. 	]
    strip trailing [3 String with white space.]
    strip both     [3 String with white space.]

  Original string  [ 	 4 String with white space.]
    strip leading  [4 String with white space.]
    strip trailing [ 	 4 String with white space.]
    strip both     [4 String with white space.]

  Original string  [5 String with white space.]
    strip leading  [5 String with white space.]
    strip trailing [5 String with white space.]
    strip both     [5 String with white space.]

  Original string  [ ﻿  ]
    strip leading  []
    strip trailing []
    strip both     []

  Original string  [   ]
    strip leading  []
    strip trailing []
    strip both     []

  Original string  []
    strip leading  []
    strip trailing []
    strip both     []

```



## NewLISP


```NewLISP
(setq str "   this is a string   ")

;; trim leading blanks
(trim str " " "")

;; trim trailing blanks
(trim str "" " ")

;; trim both leading and trailing blanks
(trim str)
```



## Nim


```nim
import strutils

let s = " \t \n String with spaces  \t  \n  "
echo "'", s, "'"
echo "'", s.strip(trailing = false), "'"
echo "'", s.strip(leading = false), "'"
echo "'", s.strip(), "'"
```


=={{header|Oberon-2}}==
Oxford Oberon-2

```oberon2

MODULE Trim;
IMPORT Out,Strings,SYSTEM;

CONST
	(* whitespaces *)
	HT = 09X;	VT = 0BX;	FF = 0CX;	GS = 1DX;	US = 1FX;
	LF = 0AX;	CR = 0DX; 	FS = 1CX;	RS = 1EX;	SPC = 20X;

PROCEDURE LTrim(VAR s: ARRAY OF CHAR);
VAR
	j : INTEGER;
BEGIN
	j := 0;
	WHILE (s[j] = HT) OR (s[j] = LF) OR (s[j] = VT) OR (s[j] = CR) OR
		(s[j] = FF) OR (s[j] = FS) OR (s[j] = FS) OR (s[j] = GS) OR
		(s[j] = RS) OR (s[j] = US) OR (s[j] = SPC) DO INC(j) END;
	SYSTEM.MOVE(SYSTEM.ADR(s[j]),SYSTEM.ADR(s[0]),LEN(s) - j);
END LTrim;

PROCEDURE RTrim(VAR s: ARRAY OF CHAR);
VAR
	j : INTEGER;
BEGIN
	j := LEN(s) - 1;
	WHILE (j >= 0) & (s[j] = 0X) DO DEC(j) END;
	WHILE (j >= 0) & ((s[j] = HT) OR (s[j] = LF) OR (s[j] = VT) OR (s[j] = CR) OR
		(s[j] = FF) OR (s[j] = FS) OR (s[j] = FS) OR (s[j] = GS) OR
		(s[j] = RS) OR (s[j] = US) OR (s[j] = SPC)) DO
		s[j] := 0X;
 		DEC(j)
	END
END RTrim;

PROCEDURE Trim(VAR s: ARRAY OF CHAR);
BEGIN
	LTrim(s);
	RTrim(s)
END Trim;

VAR
	s: ARRAY 100 OF CHAR;

BEGIN
	s := "   A AAA";
	Out.Char("[");Out.String(s);Out.String("]=");Out.Char(HT);LTrim(s);Out.Char("[");Out.String(s);Out.Char("]");Out.Ln;
	s := "AAA A    ";
	Out.Char("[");Out.String(s);Out.String("]=");Out.Char(HT);RTrim(s);Out.Char("[");Out.String(s);Out.Char("]");Out.Ln;
	s := "   A AA A   ";
	Out.Char("[");Out.String(s);Out.String("]=");Out.Char(HT);Trim(s);Out.Char("[");Out.String(s);Out.Char("]");Out.Ln;
	s := "    ";
	Out.Char("[");Out.String(s);Out.String("]=");Out.Char(HT);Trim(s);Out.Char("[");Out.String(s);Out.Char("]");Out.Ln;
	s := "    ";
	Out.Char("[");Out.String(s);Out.String("]=");Out.Char(HT);RTrim(s);Out.Char("[");Out.String(s);Out.Char("]");Out.Ln;
	s := "    ";
	Out.Char("[");Out.String(s);Out.String("]=");Out.Char(HT);LTrim(s);Out.Char("[");Out.String(s);Out.Char("]");Out.Ln;
	Out.Char("[");Out.String(s);Out.String("]=");Out.Char(HT);RTrim(s);Out.Char("[");Out.String(s);Out.Char("]");Out.Ln;
	Out.Char("[");Out.String(s);Out.String("]=");Out.Char(HT);LTrim(s);Out.Char("[");Out.String(s);Out.Char("]");Out.Ln;
END Trim.

```

Output:

```txt

[   A AAA]=	[A AAA]
[AAA A    ]=	[AAA A]
[   A AA A   ]=	[A AA A]
[    ]=	[]
[    ]=	[]
[    ]=	[]
[]=	[]
[]=	[]

```

=={{header|Objective-C}}==
{{works with|Cocoa}}
{{works with|GNUstep}}

```objc>#import <Foundation/Foundation.h


@interface NSString (RCExt)
-(NSString *) ltrim;
-(NSString *) rtrim;
-(NSString *) trim;
@end

@implementation NSString (RCExt)
-(NSString *) ltrim
{
  NSInteger i;
  NSCharacterSet *cs = [NSCharacterSet whitespaceAndNewlineCharacterSet];
  for(i = 0; i < [self length]; i++)
  {
    if ( ![cs characterIsMember: [self characterAtIndex: i]] ) break;
  }
  return [self substringFromIndex: i];
}

-(NSString *) rtrim
{
  NSInteger i;
  NSCharacterSet *cs = [NSCharacterSet whitespaceAndNewlineCharacterSet];
  for(i = [self length] -1; i >= 0; i--)
  {
    if ( ![cs characterIsMember: [self characterAtIndex: i]] ) break;
  }
  return [self substringToIndex: (i+1)];
}

-(NSString *) trim
{
  return [self
	   stringByTrimmingCharactersInSet:
	     [NSCharacterSet whitespaceAndNewlineCharacterSet]];
}
@end

int main()
{
  @autoreleasepool {

    NSString *s = @"     this is a string     ";

    NSLog(@"'%@'", s);
    NSLog(@"'%@'", [s ltrim]);
    NSLog(@"'%@'", [s rtrim]);
    NSLog(@"'%@'", [s trim]);

  }
  return 0;
}
```



## OCaml


```ocaml
let left_pos s len =
  let rec aux i =
    if i >= len then None
    else match s.[i] with
    | ' ' | '\n' | '\t' | '\r' -> aux (succ i)
    | _ -> Some i
  in
  aux 0

let right_pos s len =
  let rec aux i =
    if i < 0 then None
    else match s.[i] with
    | ' ' | '\n' | '\t' | '\r' -> aux (pred i)
    | _ -> Some i
  in
  aux (pred len)

let trim s =
  let len = String.length s in
  match left_pos s len, right_pos s len with
  | Some i, Some j -> String.sub s i (j - i + 1)
  | None, None -> ""
  | _ -> assert false

let ltrim s =
  let len = String.length s in
  match left_pos s len with
  | Some i -> String.sub s i (len - i)
  | None -> ""

let rtrim s =
  let len = String.length s in
  match right_pos s len with
  | Some i -> String.sub s 0 (i + 1)
  | None -> ""
```

we put the previous code in a file called "trim.ml", and then we test these functions in the toplevel:

```txt
$ ocaml
# #use "trim.ml" ;;
val left_pos : string -> int -> int option = <fun>
val right_pos : string -> int -> int option = <fun>
val trim : string -> string = <fun>
val ltrim : string -> string = <fun>
val rtrim : string -> string = <fun>
# let s = " \t \r \n String with spaces \t \r \n " ;;
val s : string = " \t \r \n String with spaces \t \r \n "
# trim s ;;
- : string = "String with spaces"
# ltrim s ;;
- : string = "String with spaces \t \r \n "
# rtrim s ;;
- : string = " \t \r \n String with spaces"
```


Since OCaml version 4.00.0 there is a function '''String.trim'''.


## OpenEdge/Progress


```progress
DEF VAR cc AS CHAR INIT "   string with spaces   ".

MESSAGE
   "|" + LEFT-TRIM( cc )  + "|" SKIP
   "|" + RIGHT-TRIM( cc ) + "|" SKIP
   "|" + TRIM( cc )       + "|"
VIEW-AS ALERT-BOX.
```

{{out}}

```txt

---------------------------
Message
---------------------------
|string with spaces   |
|   string with spaces|
|string with spaces|
---------------------------
OK
---------------------------

```



## Perl


```perl
sub ltrim { shift =~ s/^\s+//r }
sub rtrim { shift =~ s/\s+$//r }
sub trim { ltrim rtrim shift }

# Usage:
my $p = "       this is a string      ";
print "'", $p, "'\n";
print "'", trim($p), "'\n";
print "'", ltrim($p), "'\n";
print "'", rtrim($p), "'\n";
```



## Perl 6


```perl6
my $s = "\r\n \t\x2029 Good Stuff \x202F\n";
say $s.trim;
say $s.trim.perl;
say $s.trim-leading.perl;
say $s.trim-trailing.perl;
```


{{Output}}

```txt

Good Stuff
"Good Stuff"
"Good Stuff  \n"
"\r\n \t  Good Stuff"

```



## Phix


```Phix
constant s = "\ttest\n"
?s
?trim_head(s)
?trim_tail(s)
?trim(s)
```

{{out}}

```txt

"\ttest\n"
"test\n"
"\ttest"
"test"

```



## PHP

There is a built-in function that already does this.

```PHP
<?php

/**
 * @author Elad Yosifon
 */

$string = '      this is a string     ';
echo '^'.trim($string) .'$'.PHP_EOL;
echo '^'.ltrim($string).'$'.PHP_EOL;
echo '^'.rtrim($string).'$'.PHP_EOL;

```

{{out}}

```txt

^this is a string$
^this is a string     $
^      this is a string$

```



## PicoLisp


```PicoLisp
(de trimLeft (Str)
   (pack (flip (trim (flip (chop Str))))) )

(de trimRight (Str)
   (pack (trim (chop Str))) )

(de trimBoth (Str)
   (pack (clip (chop Str))) )
```

Test:

```txt
: (trimLeft " ^G ^I trimmed left ^L ")
-> "trimmed left ^L "

: (trimRight " ^G ^I trimmed right ^L ")
-> " ^G ^I trimmed right"

: (trimBoth " ^G ^I trimmed both ^L ")
-> "trimmed both"
```



## PL/I


```pli
put ( trim(text, ' ', '') );          /* trims leading blanks.       */
put ( trim(text, '', ' ') );          /* trims trailing blanks.      */
put ( trim(text) );                   /* trims leading and trailing  */
                                      /* blanks.                     */
```

To remove any white-space character(s) in a portable way:-

```pli
declare whitespace character(33) value
   ((substr(collate(), 1, 32) || ' '));
put ( trim(text, whitespace) );       /* trims leading white space.  */
put ( trim(text, '', whitespace) );   /* trims trailing white space. */
put ( trim(text, whitespace, whitespace) );
                                      /* trims leading and trailing  */
                                      /* white space.                */
```


## PowerShell


```PowerShell

$var = " Hello World "
$var.TrimStart() # String with leading whitespace removed
$var.TrimEnd() # String with trailing whitespace removed
$var.Trim() # String with both leading and trailing whitespace removed

```

<b>Output:</b>

```txt

Hello World
 Hello World
Hello World

```



## Prolog

Works with SWI-Prolog.

```Prolog
strip :-
	In = "    There are unwanted blanks here!    ",
	strip_left(In, OutLeft),
	format('In          : ~s__~n', [In]),
	format('Strip left  : ~s__~n', [OutLeft]),
	strip_right(In, OutRight),
	format('Strip right : ~s__~n', [OutRight]),
	strip(In, Out),
	format('Strip       : ~s__~n', [Out]).


strip_left(In, Out) :-
	strip_action(In, Out, []).

strip_right(In, Out) :-
	reverse(In, RIn),
	strip_left(RIn, ROut),
	reverse(ROut, Out).

strip(In, Out) :-
	strip_left(In, Tmp),
	strip_right(Tmp, Out).

strip_action([X|T]) -->
	{\+code_type(X, graph), !},
	strip_action(T).


strip_action(X) --> X.

```

Output :

```txt
 ?- strip.
In          :     There are unwanted blanks here!    __
Strip left  : There are unwanted blanks here!    __
Strip right :     There are unwanted blanks here!__
Strip       : There are unwanted blanks here!__
true.
```

SWI-Prolog has an integrated version of strip : '''normalize_space(-Out, +In)'''

```txt
 ?- In = '    There are unwanted blanks here!    ', normalize_space(atom(Out), In).
In = '    There are unwanted blanks here!    ',
Out = 'There are unwanted blanks here!'.

```



## PureBasic

Note, if only spaces need to be removed, PureBasic provides commands that do this: <tt>LTrim()</tt>, <tt>RTrim()</tt>, and <tt>Trim()</tt>.  To handle a larger selection of whitespace the following functions meet the task.

```PureBasic
;define the whitespace as desired
#whitespace$ = " " + Chr($9) + Chr($A) + Chr($B) + Chr($C) + Chr($D) + Chr($1C) + Chr($1D) + Chr($1E) + Chr($1F)

Procedure.s myLTrim(source.s)
  Protected i, *ptrChar.Character, length = Len(source)
  *ptrChar = @source
  For i = 1 To length
    If Not FindString(#whitespace$, Chr(*ptrChar\c))
      ProcedureReturn Right(source, length + 1 - i)
    EndIf
    *ptrChar + SizeOf(Character)
  Next
EndProcedure

Procedure.s myRTrim(source.s)
  Protected i, *ptrChar.Character, length = Len(source)
  *ptrChar = @source + (length - 1) * SizeOf(Character)
  For i = length To 1 Step - 1
    If Not FindString(#whitespace$, Chr(*ptrChar\c))
      ProcedureReturn Left(source, i)
    EndIf
    *ptrChar - SizeOf(Character)
  Next
EndProcedure

Procedure.s myTrim(source.s)
  ProcedureReturn myRTrim(myLTrim(source))
EndProcedure

If OpenConsole()
  PrintN(#DQUOTE$ + myLTrim("  Top  ") + #DQUOTE$)
  PrintN(#DQUOTE$ + myRTrim("  Tail  ") + #DQUOTE$)
  PrintN(#DQUOTE$ +  myTrim("  Both  ") + #DQUOTE$)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

{{out}}

```txt
"Top  "
"  Tail"
"Both"
```



## Python


```python>>>
 s = ' \t \r \n String with spaces  \t  \r  \n  '
>>> s
' \t \r \n String with spaces  \t  \r  \n  '
>>> s.lstrip()
'String with spaces  \t  \r  \n  '
>>> s.rstrip()
' \t \r \n String with spaces'
>>> s.strip()
'String with spaces'
>>>
```



## Racket



```Racket

#lang racket

;; Using Racket's `string-trim'

(define str "  \n\t foo  bar   \r\n  ")

;; both sides:
(string-trim str) ; -> "foo  bar"

;; one side:
(string-trim str #:right? #f) ; -> "foo  bar   \r\n  "
(string-trim str #:left? #f)  ; -> "  \n\t foo  bar"

;; can also normalize spaces:
(string-normalize-spaces (string-trim str)) ; -> "foo bar"

```



## Retro


```Retro
"  this is a test   "  ^strings'trimLeft
"  this is a test   "  ^strings'trimRight
"  this is a test   "  ^strings'trimLeft ^strings'trimRight
```



## Red


```Red>>
 trim/head " remove leading white space "
== "remove leading white space "
>> trim/tail " remove trailing white space "
== " remove trailing white space"
>> trim " remove both white spaces "
== "remove both white spaces"
```



## REXX



### version 1


```rexx
/*REXX program demonstrates  how to  strip  leading  and/or  trailing  spaces (blanks). */
yyy="   this is a string that has leading/embedded/trailing blanks,  fur shure.  "
say 'YYY──►'yyy"◄──"                             /*display the original string + fence. */
                      /*white space also includes tabs (VT, HT), among other characters.*/

                      /*all examples in each group are equivalent, only the option's 1st*/
                      /*character is examined.                                          */
noL=strip(yyy,'L')                               /*elide any  leading white space.      */
noL=strip(yyy,"l")                               /*  (the same as the above statement.) */
noL=strip(yyy,'leading')                         /*    "    "   "  "    "       "       */
say 'noL──►'noL"◄──"                             /*display the string with a title+fence*/

noT=strip(yyy,'T')                               /*elide any trailing white space.      */
noT=strip(yyy,"t")                               /*  (the same as the above statement.) */
noT=strip(yyy,'trailing')                        /*    "    "   "  "    "       "       */
say 'noT──►'noT"◄──"                             /*display the string with a title+fence*/

noB=strip(yyy)                                   /*elide leading & trailing white space.*/
noB=strip(yyy,)                                  /*  (the same as the above statement.) */
noB=strip(yyy,'B')                               /*    "    "   "  "    "       "       */
noB=strip(yyy,"b")                               /*    "    "   "  "    "       "       */
noB=strip(yyy,'both')                            /*    "    "   "  "    "       "       */
say 'noB──►'noB"◄──"                             /*display the string with a title+fence*/

                                                 /*elide leading & trailing white space,*/
noX=space(yyy)                                   /* including white space between words.*/
say 'nox──►'noX"◄──"                             /*display the string with a title+fence*/
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''

```txt

YYY──►   this is a string that has leading/embedded/trailing blanks,  fur shure.  ◄──
noL──►this is a string that has leading/embedded/trailing blanks,  fur shure.  ◄──
noT──►   this is a string that has leading/embedded/trailing blanks,  fur shure.◄──
noB──►this is a string that has leading/embedded/trailing blanks,  fur shure.◄──
nox──►this is a string that has leading/embedded/trailing blanks, fur shure.◄──

```



### version 2


```rexx
/* REXX ***************************************************************
* 01.1.2012 Walter Pachl taking care of all non-printable chars
**********************************************************************/
pc='abcdefghijklmnopqrstuvwxyz'
pc=pc||translate(pc)'äöüÄÖÜß1234567890!"§&/()=?*+''#;:_,.-<>^!'
x01='01'x
s=x01||'  Hi  '||x01||' there!  '||x01
say pc                                 /* all printable characters   */
s=x01||'  Hi  '||x01||' there!  '||x01 /* my source string           */
Say 's >'s'<'                          /* show it                    */
p1=verify(s,pc,'M')                    /* find first printable char  */
sl=substr(s,p1)                        /* start with it              */
Say 'sl>'sl'<'
sr=reverse(s)
p2=verify(sr,pc,'M')                   /* find last printable char   */
sr=left(s,length(s)-p2+1)              /* end with it                */
Say 'sr>'sr'<'
sb=substr(s,p1,length(s)-p1-p2+1)      /* remove leading & trailing  */
Say 'sb>'space(sb)'!'                  /* whitespace                 */
sa=translate(s,pc,pc!!xrange('00'x,'FF'x)) /* all nonprintable chars */
                                         /* are translated to blanks */
sa=space(sa)                           /* eliminate them except 1    */
Say 'sa>'sa'<'<'                       /* between words              */
s0=space(sa,0)                         /* remove all whitespace      */
Say 's0>'s0'<'
```

Output:

```txt

abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZäöüÄÖÜß1234567890!"§&/()=?*+'#;:_,.-<>^!
s >�  Hi  � there!  �<
sl>Hi  � there!  �<
sr>�  Hi  � there!<
sb>Hi  � there!<
sa>Hi there!<
s0>Hithere!<

```



## Ring


```ring

aList = "   Welcome to the Ring Programming Language   "
see aList + nl
see trim(aList) + nl

```



## Ruby


```ruby
s = " \t\v\r\n\ffoo bar \t\v\r\n\f"
p s
p s.lstrip    # remove leading whitespaces
p s.rstrip    # remove trailing whitespaces
p s.strip     # remove both leading and trailing whitespace

```

{{out}}

```txt

" \t\v\r\n\ffoo bar \t\v\r\n\f"
"foo bar \t\v\r\n\f"
" \t\v\r\n\ffoo bar"
"foo bar"

```



## Run BASIC


```runbasic
string$ = "  abcdefg  "

print "   Top:";trim$(string$+"|")       ' top    left trim
print "Bottom:";trim$("|"+string$)       ' bottom right trim
print "  Both:";trim$(string$)           ' both   left and right
end
```


```txt
   Top:abcdefg
Bottom:  abcdefg
  Both:abcdefg
```



## Rust


```rust
fn main() {
    let spaces = " \t\n\x0B\x0C\r \u{A0} \u{2000}\u{3000}";
    let string_with_spaces = spaces.to_owned() + "String without spaces" + spaces;

    assert_eq!(string_with_spaces.trim(), "String without spaces");
    assert_eq!(string_with_spaces.trim_left(), "String without spaces".to_owned() + spaces);
    assert_eq!(string_with_spaces.trim_right(), spaces.to_owned() + "String without spaces");
}
```



## Sather


```sather
class MAIN is
    ltrim(s :STR) :STR is
      i ::= 0;
      loop while!(i < s.size);
        if " \t\f\v\n".contains(s[i]) then
           i := i + 1;
        else
           break!;
        end;
      end;
      return s.tail(s.size - i);
    end;

    rtrim(s :STR) :STR is
      i ::= s.size-1;
      loop while!(i >= 0);
        if " \t\f\v\n".contains(s[i]) then
           i := i - 1;
        else
           break!;
        end;
      end;
      return s.head(i+1);
    end;

    trim(s :STR) :STR is
       return ltrim(rtrim(s));
    end;


    main is
      p ::= "     this is a string     ";
      #OUT + ltrim(p).pretty + "\n";
      #OUT + rtrim(p).pretty + "\n";
      #OUT + trim(p).pretty + "\n";
    end;
end;
```



## Scala


```scala
def trimLeft(str: String) = str dropWhile(_.isWhitespace)

def trimRight(str: String) = str take (str.lastIndexWhere(!_.isWhitespace) + 1)

def trimRight2(str: String) = trimLeft(str reverse) reverse

def trim(str: String) = str trim

def testTrim() = {
  val str = "  \u001F  String with spaces \t  \n  \r "
  println("original  : |" + str + "|")
  println("trimLeft  : |" + trimLeft(str) + "|")
  println("trimRight : |" + trimRight(str) + "|")
  println("trimRight2: |" + trimRight2(str) + "|")
  println("trim      : |" + trim(str) + "|")
}
```

{{out}}

```txt
original  : |  �  String with spaces

 |
trimLeft  : |String with spaces

 |
trimRight : |  �  String with spaces|
trimRight2: |  �  String with spaces|
trim      : |String with spaces|
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    const string: testStri is " \t \r \n String with spaces  \t  \r  \n  ";
  begin
    writeln(ltrim(testStri));
    writeln(rtrim(testStri));
    writeln(trim(testStri));
  end func;
```



## Sidef


```ruby
var s = " \t\v\r\n\ffoo bar \t\v\r\n\f";
say s.strip_beg.dump;    # remove leading whitespaces
say s.strip_end.dump;    # remove trailing whitespaces
say s.strip.dump;        # remove both leading and trailing whitespace
```

{{out}}

```txt

"foo bar \t\13\r\n\f"
" \t\13\r\n\ffoo bar"
"foo bar"

```



## Smalltalk

{{works with|GNU Smalltalk}}

```smalltalk
String extend
[
   ltrim [
      ^self replacingRegex: '^\s+' with: ''.
   ]
   rtrim [
      ^self replacingRegex: '\s+$' with: ''.
   ]
   trim [
      ^self ltrim rtrim.
   ]
]

|a|
a := '      this is a string       '.

('"%1"' % {a}) displayNl.
('"%1"' % {a ltrim}) displayNl.
('"%1"' % {a rtrim}) displayNl.
('"%1"' % {a trim}) displayNl.
```



## SNOBOL4


```SNOBOL4
    s1 = s2 = " 	Hello, people of earth!  	"
    s2 = CHAR(3) s2 CHAR(134)
    &ALPHABET TAB(33) . prechars
    &ALPHABET POS(127) RTAB(0) . postchars
    stripchars = " " prechars postchars

* TRIM() removes final spaces and tabs:
    OUTPUT = "Original: >" s1 "<"
    OUTPUT = "With trim() >" REVERSE(TRIM(REVERSE(TRIM(s1)))) "<"

* Remove all non-printing characters:
    OUTPUT = "Original: >" s2 "<"
    s1 POS(0) SPAN(stripchars) =
    OUTPUT = "Leading: >" s1 "<"
    s2 ARB . s2 SPAN(stripchars) RPOS(0)
    OUTPUT = "Trailing: >" s2 "<"
    s2 POS(0) SPAN(stripchars) =
    OUTPUT = "Full trim: >" s2 "<"
END
```

{{out}}

```txt

Original: > 	Hello, people of earth!  	<
With trim() >Hello, people of earth!<
Original: >� 	Hello, people of earth!  	�<
Leading: >Hello, people of earth!  	<
Trailing: >� 	Hello, people of earth!<
Full trim: >Hello, people of earth!<
```



## Stata

See '''[https://www.stata.com/help.cgi?mf_strtrim strtrim]''' in Stata help. Use the equivalent '''[https://www.stata.com/help.cgi?mf_ustrtrim ustrtrim]''' functions with Unicode strings.


```stata
s = "   ars   longa   "
"("+strtrim(s)+")"
  (ars   longa)

"("+strltrim(s)+")"
  (ars   longa   )

"("+strrtrim(s)+")"
  (   ars   longa)

"("+stritrim(s)+")"
  (   ars longa   )
```



## Tcl

Whitespace stripping is done with <code>string trim</code> and related commands:

```tcl
set str "      hello world      "
puts "original: >$str<"
puts "trimmed head: >[string trimleft $str]<"
puts "trimmed tail: >[string trimright $str]<"
puts "trimmed both: >[string trim $str]<"
```

{{out}}

```txt

original: >      hello world      <
trimmed head: >hello world      <
trimmed tail: >      hello world<
trimmed both: >hello world<

```


=={{header|TI-83 BASIC}}==
<lang ti-83b>
PROGRAM:WHITESPC
Input Str1
0→M
Menu("     REMOVE     ","TRAILIN WHTSPC",A,"LEADING WHTSPC",C,"BOTH",B)

Lbl B
1→M

Lbl A
While sub(Str1,length(Str1)-1),1)=" "
sub(Str1,1,length(Str1)-1)→Str1
End

If M=1
Then
Goto C
Else
Goto F
End

Lbl C
While sub(str1,1,1)=" "
sub(Str1,2,length(Str1)-1)→Str1
End

Lbl F
Disp "'"+Str1+"'"

```



## TorqueScript

Remove leading whitespace
  $string = "                 yep                 ";
  $string = LTrim($string);
  echo($string);
Remove trailing whitespace
  $string = "                 yep                 ";
  $string = RTrim($string);
  echo($string);
Remove leading and trailing white spaces
  $string = "                 yep                 ";
  $string = trim($string);
  echo($string);


Output
  "yep                 "
  "                 yep"
  "yep"


## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
str= "      sentence w/whitespace before and after    "
trimmedtop=EXTRACT (str,":<|<> :"|,0)
trimmedtail=EXTRACT (str,0,":<> >|:")
trimmedboth=SQUEEZE(str)
PRINT "string           <|", str," >|"
PRINT "trimmed on top   <|",trimmedtop,">|"
PRINT "trimmed on tail  <|", trimmedtail,">|"
PRINT "trimmed on both  <|", trimmedboth,">|"
```

{{out}}

```txt

string           <|      sentence w/whitespace before and after     >|
trimmed on top   <|sentence w/whitespace before and after    >|
trimmed on tail  <|      sentence w/whitespace before and after>|
trimmed on both  <|sentence w/whitespace before and after>|

```



## TXR



### Pattern Matching Language Exercise

Here, no builtin functions are used, just text pattern matching logic. Two functions are written, conforming to the proper filter convention, and then employed as filters.

```txr
@(define trim_left (in out))
@  (next :list in)
@/[ \t]*/@out
@(end)
@(define trim_right (in out))
@  (local blanks middle)
@  (next :list in)
@  (cases)
@    {blanks /[ \t]*/}@middle@/[\t ]+/
@    (bind out `@blanks@middle`)
@  (or)
@    out
@  (end)
@(end)
@line_of_input
@(output)
trim-left:  [@{line_of_input :filter (:fun trim_left)}]
trim_right: [@{line_of_input :filter (:fun trim_right)}]
trim_both:  [@{line_of_input :filter ((:fun trim_left) (:fun trim_right))}]
@(end)
```

{{out}}

```txt
$ echo "" | txr trim.txr  -
trim-left:  []
trim_right: []
trim_both:  []
$ echo "a" | txr trim.txr  -
trim-left:  [a]
trim_right: [a]
trim_both:  [a]
$ echo " a" | txr trim.txr  -
trim-left:  [a]
trim_right: [ a]
trim_both:  [a]
$ echo " a " | txr trim.txr  -
trim-left:  [a ]
trim_right: [ a]
trim_both:  [a]
$ echo " a b " | txr trim.txr  -
trim-left:  [a b ]
trim_right: [ a b]
trim_both:  [a b]
```



### Using Lisp Primitives


Trimming whitespace from both ends is a builtin:


```sh
$ txr -p '(trim-str " a b ")'
"a b"
```


An unnecessarily cryptic, though educational, left trim:


```sh
$ txr -p '[(do progn (del [@1 0..(match-regex @1 #/\s*/)]) @1) " a b "]'
"a b "
```


Explanation: the basic structure is <code>[function " a b "]</code> where the function is an anonymous lambda generated using the <code>do</code> operator.  The function is applied to the string <code>" a b "</code>.

The structure of the <code>do</code> is <code>(do progn (blah @1) @1)</code> where the forms make references to implicit argument <code>@1</code>, and so the generated lambda has one argument, essentially being: <code>(lambda (arg) (blah arg) arg)</code>: do something with the argument (the string) and then return it.

What is done with the argument is this: <code>(del [@1 0..(match-regex @1 #/\s+/)])</code>. The <code>match-regex</code> function returns the number of characters at the front of the string which match the regex <code>\s*</code>: one or more spaces.  The return value of this is used to express a range <code>0..length</code> which is applied to the string. The syntax <code>(del [str from..to])</code> deletes a range of characters in the string.

Lastly, a pedestrian right trim:


```txrlisp
(defun trim-right (str)
  (for ()
       ((and (> (length str) 0) (chr-isspace [str -1])) str)
       ((del [str -1]))))
(format t "{~a}\n" (trim-right " a a "))
(format t "{~a}\n" (trim-right "  "))
(format t "{~a}\n" (trim-right "a "))
(format t "{~a}\n" (trim-right ""))
```


Output:


```txt
{ a a}
{}
{a}
{}
```


## Ursala


```Ursala
#import std

white      = ==` !| not @iNC %sI
trim_left  = white-~r
trim_right = white~-l
trim_both  = trim_left+ trim_right

#cast %sgUL

main = <.trim_left,trim_right,trim_both> '  string with spaces   '
```

* The <code>white</code> predicate tests an argument for whiteness by either comparing it to a literal space character or testing whether the singleton list containing it is of a string (<code>%s</code>) type.
* The <code>-~</code> postfix operator takes a predicate to a function that takes a string to a pair of strings whose concatenation is the original string and whose left side is the maximal prefix of the original string whose members satisfy the predicate.
* The <code>r</code> suffix on the <code>-~</code> predicate extracts the right side of the pair of strings in the result.
* The <code>~-</code> operator is similar to the <code>-~</code> operator except that is concerned with the maximal suffix whose members satisfy the predicate.
* The <code>l</code> suffix extracts the right side.
{{out}}

```txt
<
   'string with spaces   ',
   '  string with spaces',
   'string with spaces'>
```


## Vala


### Strip Leading White Space


```vala
string s = "   word   ";
string s_chug = s.chug();
```


### Strip Trailing White Space


```vala
string s = "   word   ";
string s_chomp = s.chomp();
```

===Strip Leading & Trailing White Space===

```vala
string s = "   word   ";
string s_strip = s.strip();
```


## VBA


```vb
Public Sub test()
    'LTrim trims leading spaces
    'RTrim trims tailing spaces
    'Trim trims both leading and tailing spaces
    s = " trim "
    Debug.Print """" & s & """"
    Debug.Print """" & LTrim(s) & """"
    Debug.Print """" & RTrim(s) & """"
    Debug.Print """" & WorksheetFunction.trim(s) & """"
    'these functions do not remove tabs or newlines
End Sub
```
{{out}}

```txt
" trim "
"trim "
" trim"
"trim"

```


## VBScript


```vb

Function LeftTrim(s)
	Set regex = New RegExp
	With regex
		.Pattern = "^\s*"
		If .Test(s) Then
			LeftTrim = .Replace(s,"")
		Else
			LeftTrim = s
		End If
	End With
End Function

Function RightTrim(s)
	Set regex = New RegExp
	With regex
		.Pattern = "\s*$"
		If .Test(s) Then
			RightTrim = .Replace(s,"")
		Else
			RightTrim = s
		End If
	End With
End Function

'testing the functions
WScript.StdOut.WriteLine LeftTrim("			   RosettaCode")
WScript.StdOut.WriteLine RightTrim("RosettaCode			  		")
WScript.StdOut.WriteLine LeftTrim(RightTrim("  	RosettaCode			  		"))

```


{{Out}}

```txt

RosettaCode
RosettaCode
RosettaCode

```



## XPL0


```XPL0
code ChOut=8, CrLf=9, Text=12;
string 0;                       \use zero-terminated string convention

func StripLead(S0);             \Strip leading whitespace (<=$20) from string
char S0;
char S1(80);                    \BEWARE: very temporary string space returned
int  I, J, C, Stripped;
[I:= 0;  J:= 0;  Stripped:= false;
repeat  C:= S0(I);  I:= I+1;
        if Stripped ! C>$20 ! C=0 then
            [S1(J):= C;  J:= J+1;  Stripped:= true];
until   C=0;
return S1;
];

func StripTrail(S0);            \Strip trailing whitespace (<=$20) from string
char S0;
char S1(80);                    \BEWARE: very temporary string space returned
int  I, SI;
[I:= 0;
loop    [if S0(I) <= $20 then
             [SI:= I;
             while S0(I) <= $20 do
                [if S0(I) = 0 then [S1(SI):= 0;  return S1];
                S1(I):= S0(I);  I:= I+1;
                ];
             ]
        else [S1(I):= S0(I);  I:= I+1];
        ];
];

char String;
[String:= "  Hello World  ";
Text(0, StripLead(String));  ChOut(0, ^!);  CrLf(0);
Text(0, StripTrail(String));  ChOut(0, ^!);  CrLf(0);
Text(0, StripTrail(StripLead(String)));  ChOut(0, ^!);  CrLf(0);
]
```


Output:

```txt

Hello World  !
  Hello World!
Hello World!

```



## Yabasic


```Yabasic
s$ = "\t test \n"

print "--",ltrim$(s$),"--"
print "--",rtrim$(s$),"--"
print "--",trim$(s$),"--"
```



## zkl

Remove white space from both end of string:

```zkl
"\t\n hoho\n\t\ ".strip() //-->"hoho"
```


```zkl
fcn removeLeadingWS(s){ n:=0;
  try{ while(s[n].isSpace()){ n+=1 } }catch{""}fallthrough{ s[n,*] }
}
removeLeadingWS("\t\n hoho\n\t\ ") //-->"hoho\n\t "
removeLeadingWS("") //-->""
```


```zkl
fcn removeTrailingWS(s){ n:=-1;
  try{ while(s[n].isSpace()){ n-=1 } s[0,n+1] }catch{""}
}
removeTrailingWS("\t\n hoho\n\t\ ") //-->"\t\n hoho"
removeTrailingWS("\t\n \n\t\ ") //-->""
```

