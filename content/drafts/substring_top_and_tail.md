+++
title = "Substring/Top and tail"
description = ""
date = 2019-10-13T00:37:04Z
aliases = []
[extra]
id = 9881
[taxonomies]
categories = []
tags = []
+++

{{task}}[[Category:String manipulation]]
The task is to demonstrate how to remove the first and last characters from a string.

The solution should demonstrate how to obtain the following results:

* String with first character removed
* String with last character removed
* String with both the first and last characters removed



If the program uses UTF-8 or UTF-16, it must work on any valid Unicode code point, whether in the Basic Multilingual Plane or above it.

The program must reference logical characters (code points), not 8-bit code units for UTF-8 or 16-bit code units for UTF-16.

Programs for other encodings (such as 8-bit ASCII, or EUC-JP) are not required to handle all Unicode characters.





## 360 Assembly


```360asm
*        Substring/Top and tail    04/03/2017
SUBSTRTT CSECT
         USING  SUBSTRTT,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
*
         XPRNT  S8,L'S8            print s8
         MVC    S7,S8+1            s7=substr(s8,2,7)
         XPRNT  S7,L'S7            print s7
         MVC    S7,S8              s7=substr(s8,1,7)
         XPRNT  S7,L'S7            print s7
         MVC    S6,S8+1            s6=substr(s8,2,6)
         XPRNT  S6,L'S6            print s6
*
         L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    restore previous context
         XR     R15,R15            rc=0
         BR     R14                exit
S8       DC     CL8'12345678'
S7       DS     CL7
S6       DS     CL6
         YREGS
         END    SUBSTRTT
```

{{out}}

```txt

12345678
2345678
1234567
234567

```



## ACL2


```Lisp
(defun str-rest (str)
   (coerce (rest (coerce str 'list)) 'string))

(defun rdc (xs)
   (if (endp (rest xs))
       nil
       (cons (first xs)
             (rdc (rest xs)))))

(defun str-rdc (str)
   (coerce (rdc (coerce str 'list)) 'string))

(str-rdc "string")
(str-rest "string")
(str-rest (str-rdc "string"))
```



## Ada



```Ada
with Ada.Text_IO;

procedure Remove_Characters is
   S: String := "upraisers";
   use Ada.Text_IO;
begin
   Put_Line("Full String:   """ & S & """");
   Put_Line("Without_First: """ & S(S'First+1 .. S'Last) & """");
   Put_Line("Without_Last:  """ & S(S'First   .. S'Last-1) & """");
   Put_Line("Without_Both:  """ & S(S'First+1 .. S'Last-1) & """");
end Remove_Characters;
```


Output:


```txt
Full String:   "upraisers"
Without_First: "praisers"
Without_Last:  "upraiser"
Without_Both:  "praiser"
```


With UTF8 support in Ada 2012 (Wide_Character of literals is automatic):


```Ada
with Ada.Text_IO;
with Ada.Strings.UTF_Encoding.Wide_Strings;

procedure Remove_Characters
is
   use Ada.Text_IO;
   use Ada.Strings.UTF_Encoding;
   use Ada.Strings.UTF_Encoding.Wide_Strings;

   S : String := "upraisers";
   U : Wide_String := Decode (UTF_8_String'(S));

   function To_String (X : Wide_String)return String
   is
   begin
      return String (UTF_8_String'(Encode (X)));
   end To_String;

begin
   Put_Line
     (To_String
        ("Full String:   """ & U & """"));
   Put_Line
     (To_String
        ("Without_First: """ & U (U'First + 1 .. U'Last) & """"));
   Put_Line
     (To_String
        ("Without_Last:  """ & U (U'First   .. U'Last - 1) & """"));
   Put_Line
     (To_String
        ("Without_Both:  """ & U (U'First + 1 .. U'Last - 1) & """"));

end Remove_Characters;
```


Output:


```txt
Full String:   "upraisers"
Without_First: "praisers"
Without_Last:  "upraiser"
Without_Both:  "praiser"
```



## Aime


```aime
o_text(delete("knights", 0));
o_newline();
o_text(delete("knights", -1));
o_newline();
o_text(delete(delete("knights", 0), -1));
o_newline();
```

{{out}}

```txt
nights
knight
night
```



## Apex


```java

String strOrig = 'brooms';
String str1 = strOrig.substring(1, strOrig.length());
system.debug(str1);
String str2 = strOrig.substring(0, strOrig.length()-1);
system.debug(str2);
String str3 = strOrig.substring(1, strOrig.length()-1);
system.debug(str3);

// Regular Expressions approach
String strOrig = 'brooms';
String str1 = strOrig.replaceAll( '^.', '' );
system.debug(str1);
String str2 = strOrig.replaceAll( '.$', '' ) ;
system.debug(str2);
String str3 = strOrig.replaceAll( '^.|.$', '' );
system.debug(str3);

```


{{out}}

```txt

rooms

broom

room

```



## ALGOL 68

{{trans|AWK}}
{{works with|ALGOL 68|Revision 1 - no extensions to language used.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}

```algol68
#!/usr/local/bin/a68g --script #

STRING str="upraisers";
printf(($gl$,
  str,                      # remove no characters #
  str[LWB str+1:         ], # remove the first character #
  str[         :UPB str-1], # remove the last character #
  str[LWB str+1:UPB str-1], # remove both the first and last character #
  str[LWB str+2:         ], # remove the first 2 characters #
  str[         :UPB str-2], # remove the last 2 characters #
  str[LWB str+1:UPB str-2], # remove 1 before and 2 after #
  str[LWB str+2:UPB str-1], # remove 2 before and one after #
  str[LWB str+2:UPB str-2]  # remove both the first and last 2 characters #
))
```

Output:

```txt

upraisers
praisers
upraiser
praiser
raisers
upraise
praise
raiser
raise

```



## AutoHotkey


```AutoHotkey
myString := "knights"
MsgBox % SubStr(MyString, 2)
MsgBox % SubStr(MyString, 1, StrLen(MyString)-1)
MsgBox % SubStr(MyString, 2, StrLen(MyString)-2)
```


## AWK


```awk
BEGIN {
  mystring="knights"
  print substr(mystring,2)                       # remove the first letter
  print substr(mystring,1,length(mystring)-1)    # remove the last character
  print substr(mystring,2,length(mystring)-2)    # remove both the first and last character
}
```



## BASIC


```basic
10 PRINT FN F$("KNIGHTS"): REM STRIP THE FIRST LETTER
20 PRINT FN L$("SOCKS"): REM STRIP THE LAST LETTER
30 PRINT FN B$("BROOMS"): REM STRIP BOTH THE FIRST AND LAST LETTER
100 END

9000 DEF FN F$(A$)=RIGHT$(A$,LEN(A$)-1)
9010 DEF FN L$(A$)=LEFT$(A$,LEN(A$)-1)
9020 DEF FN B$(A$)=FN L$(FN F$(A$))
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 LET S$="Knights"
110 PRINT S$(2:)
120 PRINT S$(:LEN(S$)-1)
130 PRINT S$(2:LEN(S$)-1)
```


=
## Sinclair ZX81 BASIC
=
Note that strings are indexed from 1.

```basic
10 REM STRING SLICING EXAMPLE
20 LET S$="KNIGHTS"
30 REM WITH FIRST CHARACTER REMOVED:
40 PRINT S$(2 TO )
50 REM WITH LAST CHARACTER REMOVED:
60 PRINT S$( TO LEN S$-1)
70 REM WITH BOTH REMOVED:
80 PRINT S$(2 TO LEN S$-1)
```

{{out}}

```txt
NIGHTS
KNIGHT
NIGHT
```



## Bracmat

Bracmat uses UTF-8 internally. The function <code>utf</code> fails if its argument isn't a valid UTF-8 multibyte string, but in two slightly different ways: an indefinite and a definite way. If the argument does not have the required number of bytes but otherwise seems to be ok, Bracmat's backtacking mechanism lenghtens the argument and then calls <code>utf</code> again. This is repeated until utf either succeeds or definitely fails. The code is far from efficient.


```bracmat
(substringUTF-8=
  @( Δημοτική
   : (%?a&utf$!a) ?"String with first character removed"
   )
& @( Δημοτική
   : ?"String with last character removed" (?z&utf$!z)
   )
& @( Δημοτική
   :   (%?a&utf$!a)
       ?"String with both the first and last characters removed"
       (?z&utf$!z)
   )
&   out
  $ ("String with first character removed:" !"String with first character removed")
&   out
  $ ("String with last character removed:" !"String with last character removed")
&   out
  $ ( "String with both the first and last characters removed:"
      !"String with both the first and last characters removed"
    ));
```



```txt
!substringUTF-8
String with first character removed: ημοτική
String with last character removed: Δημοτικ
String with both the first and last characters removed: ημοτικ
```


If the string is known to consist of 8-byte characters, we can use a simpler method. Essential are the <code>%</code> and <code>@</code> prefixes. The <code>%</code> prefix matches 1 or more elements (bytes, in the case of string pattern matching), while <code>@</code> matches 0 or 1 elements. In combination these prefixes match 1 and only 1 byte.


```bracmat
(substring-8-bit=
  @("8-bit string":%@ ?"String with first character removed")
& @("8-bit string":?"String with last character removed" @)
& @( "8-bit string"
   : %@ ?"String with both the first and last characters removed" @
   )
&   out
  $ ("String with first character removed:" !"String with first character removed")
&   out
  $ ("String with last character removed:" !"String with last character removed")
&   out
  $ ( "String with both the first and last characters removed:"
      !"String with both the first and last characters removed"
    ));
```



```txt
!substring-8-bit
String with first character removed: -bit string
String with last character removed: 8-bit strin
String with both the first and last characters removed: -bit strin
```



## BBC BASIC


```bbcbasic
      s$ = "Rosetta Code"
      PRINT MID$(s$, 2)
      PRINT LEFT$(s$)
      PRINT LEFT$(MID$(s$, 2))
```



## Burlesque


```blsq

blsq ) "RosettaCode"[-
"osettaCode"
blsq ) "RosettaCode"-]
'R
blsq ) "RosettaCode"~]
"RosettaCod"
blsq ) "RosettaCode"[~
'e
blsq ) "RosettaCode"~-
"osettaCod"

```



## C


```c
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

int main( int argc, char ** argv ){
  const char * str_a = "knight";
  const char * str_b = "socks";
  const char * str_c = "brooms";

  char * new_a = malloc( strlen( str_a ) - 1 );
  char * new_b = malloc( strlen( str_b ) - 1 );
  char * new_c = malloc( strlen( str_c ) - 2 );

  strcpy( new_a, str_a + 1 );
  strncpy( new_b, str_b, strlen( str_b ) - 1 );
  strncpy( new_c, str_c + 1, strlen( str_c ) - 2 );

  printf( "%s\n%s\n%s\n", new_a, new_b, new_c );

  free( new_a );
  free( new_b );
  free( new_c );

  return 0;
}
```


Result:

```txt
night
sock
room
```


ANSI C provides little functionality for text manipulation outside of string.h.  While a number of libraries for this purpose have been written, this example uses only ANSI C.


## C++


```cpp
#include <string>
#include <iostream>

int main( ) {
   std::string word( "Premier League" ) ;
   std::cout << "Without first letter: " << word.substr( 1 ) << " !\n" ;
   std::cout << "Without last letter: " << word.substr( 0 , word.length( ) - 1 ) << " !\n" ;
   std::cout << "Without first and last letter: " << word.substr( 1 , word.length( ) - 2 ) << " !\n" ;
   return 0 ;
}
```

Output:
<PRE>Without first letter: remier League !
Without last letter: Premier Leagu !
Without first and last letter: remier Leagu !
</PRE>


## C sharp


```C sharp

using System;

class Program
{
    static void Main(string[] args)
    {
        string testString = "test";
        Console.WriteLine(testString.Substring(1));
        Console.WriteLine(testString.Substring(0, testString.Length - 1));
        Console.WriteLine(testString.Substring(1, testString.Length - 2));
    }
}

```


Result:

```txt
est
tes
es
```



## Clojure


```clojure
; using substring:
user=> (subs "knight" 1)
"night"
user=> (subs "socks" 0 4)
"sock"
user=> (.substring "brooms" 1 5)
"room"

; using rest and drop-last:
user=> (apply str (rest "knight"))
"night"
user=> (apply str (drop-last "socks"))
"sock"
user=> (apply str (rest (drop-last "brooms")))
"room"
```



## COBOL


```COBOL
       identification division.
       program-id. toptail.

       data division.
       working-storage section.
       01 data-field.
          05 value "[this is a test]".

       procedure division.
       sample-main.
       display data-field
      *> Using reference modification, which is (start-position:length)
       display data-field(2:)
       display data-field(1:length of data-field - 1)
       display data-field(2:length of data-field - 2)
       goback.
       end program toptail.
```


{{out}}

```txt
prompt$ cobc -xj toptail.cob
[this is a test]
this is a test]
[this is a test
this is a test
```



## Common Lisp

<code>subseq</code> will signal an error if you provide invalid start or end values.

```lisp>
 (defvar *str* "∀Ꮺ✤Л◒")
*STR*
> (subseq *str* 1) ; remove first character
"Ꮺ✤Л◒"
> (subseq *str* 0 (1- (length *str*))) ; remove last character
"∀Ꮺ✤Л"
> (subseq *str* 1 (1- (length *str*))) ; remove first and last character
"Ꮺ✤Л"
```



## D

Version for ASCII strings or Unicode dstrings:

```d
import std.stdio;

void main() {
    // strip first character
    writeln("knight"[1 .. $]);

    // strip last character
    writeln("socks"[0 .. $ - 1]);

    // strip both first and last characters
    writeln("brooms"[1 .. $ - 1]);
}
```

{{out}}

```txt
night
sock
room
```



## Delphi


```Delphi
program TopAndTail;

{$APPTYPE CONSOLE}

const
  TEST_STRING = '1234567890';
begin
  Writeln(TEST_STRING);                                    // full string
  Writeln(Copy(TEST_STRING, 2, Length(TEST_STRING)));      // first character removed
  Writeln(Copy(TEST_STRING, 1, Length(TEST_STRING) - 1));  // last character removed
  Writeln(Copy(TEST_STRING, 2, Length(TEST_STRING) - 2));  // first and last characters removed

  Readln;
end.
```



## Eero


```objc>#import <Foundation/Foundation.h


int main()
  autoreleasepool

    s := 'knight'
    Log( '%@', s[1 .. s.length-1] )  // strip first character

    s = 'socks'
    Log( '%@', s[0 .. s.length-2] )  // strip last character

    s = 'brooms'
    Log( '%@', s[1 .. s.length-2] )  // strip both first and last characters

    s = 'Δημοτική'
    Log( '%@', s[1 .. s.length-2] )  // strip both first and last characters

  return 0
```


Output:
```txt

2013-09-04 17:08:09.453 a.out[2257:507] night
2013-09-04 17:08:09.454 a.out[2257:507] sock
2013-09-04 17:08:09.454 a.out[2257:507] room
2013-09-04 17:08:09.455 a.out[2257:507] ημοτικ
```


## Elena

ELENA 4.x :

```elena
import extensions;

public program()
{
    var testString := "test";

    console.printLine(testString.Substring(1));
    console.printLine(testString.Substring(0, testString.Length - 1));
    console.printLine(testString.Substring(1, testString.Length - 2))
}
```

{{out}}

```txt

est
tes
es

```



## Elixir


```elixir
iex(1)> str = "abcdefg"
"abcdefg"
iex(2)> String.slice(str, 1..-1)
"bcdefg"
iex(3)> String.slice(str, 0..-2)
"abcdef"
iex(4)> String.slice(str, 1..-2)
"bcdef"
```



## Emacs Lisp


```Emacs Lisp

(progn
  (setq string "top and tail")
  (insert (format "%s\n" string) )
  (setq len (length string) )
  (insert (format "%s\n" (substring string 1) ))
  (insert (format "%s\n" (substring string 0 (1- len) )))
  (insert (format "%s\n" (substring string 1 (1- len) ))))

```

<b>Output:</b>

```txt

top and tail
op and tail
top and tai
op and tai

```



## Erlang


```erlang>1
 Str = "Hello".
"Hello"
2> string:sub_string(Str, 2).                   % To strip the string from the right by 1
"ello"
3> string:sub_string(Str, 1, length(Str)-1).    % To strip the string from the left by 1
"Hell"
4> string:sub_string(Str, 2, length(Str)-1).    % To strip the string from both sides by 1
"ell"
```



## Euphoria


```euphoria
function strip_first(sequence s)
    return s[2..$]
end function

function strip_last(sequence s)
    return s[1..$-1]
end function

function strip_both(sequence s)
    return s[2..$-1]
end function

puts(1, strip_first("knight"))	-- strip first character
puts(1, strip_last("write"))	-- strip last character
puts(1, strip_both("brooms"))	-- strip both first and last characters
```



=={{header|F_Sharp|F#}}==

```fsharp>[<EntryPoint
]
let main args =
    let s = "一二三四五六七八九十"
    printfn "%A" (s.Substring(1))
    printfn "%A" (s.Substring(0, s.Length - 1))
    printfn "%A" (s.Substring(1, s.Length - 2))
    0
```

Output

```txt
"二三四五六七八九十"
"一二三四五六七八九"
"二三四五六七八九"
```



## Factor


```factor
USING: io kernel sequences ;
"Rosetta code" [ rest ] [ but-last ] [ rest but-last ] tri
[ print ] tri@
```

{{out}}

```txt

osetta code
Rosetta cod
osetta cod

```



## Forth

In Forth, strings typically take up two cells on the stack, diagrammed ( c-addr u ), with C-ADDR the address of the string and U its length. Dropping leading and trailing characters then involves simple mathematical operations on the address or length, without mutating or copying the string.


```forth
: hello ( -- c-addr u )
  s" Hello" ;

hello 1 /string type     \ => ello

hello 1- type            \ => hell

hello 1 /string 1- type  \ => ell
```


This works for ASCII, and a slight variation (2 instead of 1 per character) will suffice for BIG5, GB2312, and like, but Unicode-general code can use +X/STRING and X\STRING- from Forth-200x's XCHAR wordset.


## Fortran


```Fortran
program substring

  character(len=5) :: string
  string = "Hello"

  write (*,*) string
  write (*,*) string(2:)
  write (*,*) string( :len(string)-1)
  write (*,*) string(2:len(string)-1)

end program substring
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim s As String  = "panda"
Dim s1 As String = Mid(s, 2)
Dim s2 As String = Left(s, Len(s) - 1)
Dim s3 As String = Mid(s, 2, Len(s) - 2)
Print s
Print s1
Print s2
Print s3
Sleep
```


{{out}}

```txt

panda
anda
pand
and

```



## Go

Go strings are byte arrays that can hold whatever you want them to hold.  Common contents are ASCII and UTF-8.  You use different techniques depending on how you are interpreting the string.  The utf8 package functions shown here allows efficient extraction of first and last runes without decoding the entire string.

```go
package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    // ASCII contents:  Interpreting "characters" as bytes.
    s := "ASCII"
    fmt.Println("String:                ", s)
    fmt.Println("First byte removed:    ", s[1:])
    fmt.Println("Last byte removed:     ", s[:len(s)-1])
    fmt.Println("First and last removed:", s[1:len(s)-1])
    // UTF-8 contents:  "Characters" as runes (unicode code points)
    u := "Δημοτική"
    fmt.Println("String:                ", u)
    _, sizeFirst := utf8.DecodeRuneInString(u)
    fmt.Println("First rune removed:    ", u[sizeFirst:])
    _, sizeLast := utf8.DecodeLastRuneInString(u)
    fmt.Println("Last rune removed:     ", u[:len(u)-sizeLast])
    fmt.Println("First and last removed:", u[sizeFirst:len(u)-sizeLast])
}
```

Output:

```txt

String:                 ASCII
First byte removed:     SCII
Last byte removed:      ASCI
First and last removed: SCI
String:                 Δημοτική
First rune removed:     ημοτική
Last rune removed:      Δημοτικ
First and last removed: ημοτικ

```


=={{header|GW-BASIC}}==

```qbasic
10 A$="knight":B$="socks":C$="brooms"
20 PRINT MID$(A$,2)
30 PRINT LEFT$(B$,LEN(B$)-1)
40 PRINT MID$(C$,2,LEN(C$)-2)
```



## Groovy

Solution:

```groovy
def top  = { it.size() > 1 ? it[0..-2] : '' }
def tail = { it.size() > 1 ? it[1..-1] : '' }
```


Test:

```groovy
def testVal = 'upraisers'
println """
original: ${testVal}
top:      ${top(testVal)}
tail:     ${tail(testVal)}
top&tail: ${tail(top(testVal))}
"""
```


Output:

```txt
original: upraisers
top:      upraiser
tail:     praisers
top&tail: praiser
```



## Haskell


```Haskell
-- We define the functions to return an empty string if the argument is too
-- short for the particular operation.

remFirst, remLast, remBoth :: String -> String

remFirst "" = ""
remFirst cs = tail cs

remLast "" = ""
remLast cs = init cs

remBoth (c:cs) = remLast cs
remBoth  _     = ""

main :: IO ()
main = do
  let s = "Some string."
  mapM_ (\f -> putStrLn . f $ s) [remFirst, remLast, remBoth]
```


Alternative solution with builtin functions:

```Haskell
word = "knights"

main = do
    -- You can drop the first item
    -- using `tail`
    putStrLn (tail word)

    -- The `init` function will drop
    -- the last item
    putStrLn (init word)

    -- We can combine these two to drop
    -- the last and the first characters
    putStrLn (middle word)

-- You can combine functions using `.`,
-- which is pronounced "compose" or "of"
middle = init . tail
```


In short:

```Haskell
main :: IO ()
main = mapM_ print $ [tail, init, init . tail] <*> ["knights"]
```

{{Out}}

```txt
"nights"
"knight"
"night"
```


=={{header|Icon}} and {{header|Unicon}}==
The task is accomplished by sub-stringing.

```Icon
procedure main()
write(s := "knight"," --> ", s[2:0])  # drop 1st char
write(s := "sock"," --> ", s[1:-1])   # drop last
write(s := "brooms"," --> ", s[2:-1]) # drop both
end
```


It could also be accomplished (less clearly) by assigning into the string as below.  Very awkward for both front and back.

```Icon
write(s := "knight"," --> ", s[1] := "", s)  # drop 1st char
```



## J

The monadic primitives <code>}.</code> ([http://www.jsoftware.com/help/dictionary/d531.htm Behead]) and <code>}:</code> ([http://www.jsoftware.com/help/dictionary/d532.htm Curtail]) are useful for this task.

'''Example use:'''


```j
   }. 'knight'      NB. drop first item
night
   }: 'socks'       NB. drop last item
sock
   }: }. 'brooms'   NB. drop first and last items
room
```



## Java

I solve this problem two ways.  First I use substring which is relatively fast for small strings, since it simply grabs the characters within a set of given bounds.  The second uses regular expressions, which have a higher overhead for such short strings.


```Java
public class RM_chars {
  public static void main( String[] args ){
    System.out.println( "knight".substring( 1 ) );
    System.out.println( "socks".substring( 0, 4 ) );
    System.out.println( "brooms".substring( 1, 5 ) );
      // first, do this by selecting a specific substring
      // to exclude the first and last characters

    System.out.println( "knight".replaceAll( "^.", "" ) );
    System.out.println( "socks".replaceAll( ".$", "" ) );
    System.out.println( "brooms".replaceAll( "^.|.$", "" ) );
      // then do this using a regular expressions
  }
}
```


Results:

```txt
night
sock
room
night
sock
room
```



## JavaScript



```javascript
alert("knight".slice(1));       // strip first character
alert("socks".slice(0, -1));    // strip last character
alert("brooms".slice(1, -1));   // strip both first and last characters
```



## jq

jq uses 0-based indexing, so [1:] yields all but the first character, it being understood that data strings in jq are JSON strings. [0:-1], which can be abbreviated to [:-1], yields all but the last character, and so on. Here are some examples:
```jq
"一二三四五六七八九十"[1:]' => "二三四五六七八九十"

"一二三四五六七八九十"[:-1]' => "一二三四五六七八九"

"一二三四五六七八九十"[1:-1]' => "二三四五六七八九"

"a"[1:-1] # => ""

```

Recent versions of jq also have regular expression support, with named captures. This leads to many other possibilities, e.g.
```jq
"abc" | capture( ".(?<monkey>.*)." ).monkey => "b"
```



## Julia


```julia>julia
 "My String"[2:end] # without first character
"y String"

julia> "My String"[1:end-1] # without last character
"My Strin"

julia> "My String"[2:end-1] # without first and last characters
"y Strin"
```



## K

K provides the system function <code>_di</code> to delete  an element at
a specified index. The following code is implemented using this feature.

```K

    s: "1234567890"
"1234567890"
    s _di 0 /Delete 1st character
"234567890"
    s _di -1+#s /Delete last character
"123456789"
    (s _di -1+#s) _di 0 /String with both 1st and last character removed
"23456789"

```

Another way to implement without using the above system function:

```K

    s: "1234567890"
"1234567890"
    1 _ s  /Delete 1st character
"234567890"
    -1 _ s /Delete last character
"123456789"
    1 - -1 _ s /Delete 1st and last character
"23456789"

```



## Kotlin


```scala
// version 1.0.6
fun main(args: Array<String>) {
    val s = "Rosetta"
    println(s.drop(1))
    println(s.dropLast(1))
    println(s.drop(1).dropLast(1))
}
```


{{out}}

```txt

osetta
Rosett
osett

```



## Lasso



```Lasso
local(str = 'The quick grey rhino jumped over the lazy green fox.')

// String with first character removed
string_remove(#str,-startposition=1,-endposition=1)

// String with last character removed
string_remove(#str,-startposition=#str->size,-endposition=#str->size)

// String with both the first and last characters removed
string_remove(string_remove(#str,-startposition=#str->size,-endposition=#str->size),-startposition=1,-endposition=1)
```

{{out}}

```txt
he quick grey rhino jumped over the lazy green fox.
The quick grey rhino jumped over the lazy green fox
he quick grey rhino jumped over the lazy green fox
```



```Lasso
local(mystring = 'ÅÜÄÖカ')

#mystring -> remove(1,1)
#mystring
'<br />'
#mystring -> remove(#mystring -> size,1)
#mystring
'<br />'
#mystring -> remove(1,1)& -> remove(#mystring -> size,1)
#mystring
```

-> ÜÄÖカ

ÜÄÖ

Ä


## Liberty BASIC


```lb
string$ = "Rosetta Code"
Print Mid$(string$, 2)
Print Left$(string$, (Len(string$) - 1))
Print Mid$(string$, 2, (Len(string$) - 2))
```



## LiveCode


```LiveCode
put "pple" into x
answer char 2 to len(x) of x // pple
answer char 1 to -2 of x  // ppl
answer char 2 to -2 of x // ppl
```



## Locomotive Basic



```locobasic
10 a$="knight":b$="socks":c$="brooms"
20 PRINT MID$(a$,2)
30 PRINT LEFT$(b$,LEN(b$)-1)
40 PRINT MID$(c$,2,LEN(c$)-2)
```



## Lasso


```Lasso
local(str = 'The quick grey rhino jumped over the lazy green fox.')

// String with first character removed
string_remove(#str,-startposition=1,-endposition=1)
// > he quick grey rhino jumped over the lazy green fox.

// String with last character removed
string_remove(#str,-startposition=#str->size,-endposition=#str->size)
// > The quick grey rhino jumped over the lazy green fox

// String with both the first and last characters removed
string_remove(string_remove(#str,-startposition=#str->size,-endposition=#str->size),-startposition=1,-endposition=1)
// > he quick grey rhino jumped over the lazy green fox
```



## Logo


```logo
make "s "|My string|
print butfirst :s
print butlast :s
print butfirst butlast :s
```



## Logtalk

Using atoms for representing strings:

```logtalk

:- object(top_and_tail).

    :- public(test/1).
    test(String) :-
        sub_atom(String, 1, _, 0, MinusTop),
        write('String with first character cut: '), write(MinusTop), nl,
        sub_atom(String, 0, _, 1, MinusTail),
        write('String with last character cut: '), write(MinusTail), nl,
        sub_atom(String, 1, _, 1, MinusTopAndTail),
        write('String with first and last characters cut: '), write(MinusTopAndTail), nl.

:- end_object.

```

Sample output:

```text

| ?- top_and_tail::test('Rosetta').
String with first character cut: osetta
String with last character cut: Rosett
String with first and last characters cut: osett
yes

```



## Lua



```lua
print (string.sub("knights",2))    -- remove the first character
print (string.sub("knights",1,-2))    -- remove the last character
print (string.sub("knights",2,-2))    -- remove the first and last characters
```



## Maple

There are several ways to do this.  The first is, I think, the simplest.

```Maple>
 s := "some string":
> s[2..-1];
                              "ome string"

> s[1..-2];
                              "some strin"

> s[2..-2];
                              "ome strin"
```

The same functionality exists in the form of a procedure:

```Maple>
 substring( s, 2 .. -1 );
                              "ome string"

> substring( s, 1 .. -2 );
                              "some strin"

> substring( s, 2 .. -2 );
                              "ome strin"
```

Furthermore, there is a slightly different version in the "StringTools" package:

```Maple>
 use StringTools in
>     SubString( s, 2 .. -1 );
>     SubString( s, 1 .. -1 );
>     SubString( s, 2 .. -2 )
> end use;
                              "ome string"

                             "some string"

                              "ome strin"
```

(The difference between "substring" and "StringTools:-SubString" lies in how each treats a name as input; the former returns a name, while the latter returns a string.)


## Mathematica



```Mathematica
StringDrop["input string",1]
StringDrop["input string",-1]
StringTake["input string",{2,-2}]

```


=={{header|MATLAB}} / {{header|Octave}}==

The following case will not handle UTF-8. However, Matlab supports conversion of utf-8 to utf-16 using native2unicode().

```MATLAB

    % String with first character removed
	str(2:end)
    % String with last character removed
	str(1:end-1)
    % String with both the first and last characters removed
	str(2:end-1)

```



## MiniScript


```MiniScript
test = "This thing"
print test[1:]
print test[:-1]
print test[1:-1]

```

{{out}}

```txt

his thing
This thin
his thin

```



## Neko

Neko strings are mutable, fixed length buffers.  The '''$ssize''' builtin uses the allocated size, not any internal sentinel terminator byte.  With literals, the allocated size is the size of the data between quotes, i.e. no NUL byte appended.

'''$ssub''' ''sub-string'' takes string, position (zero-relative), length arguments.

```ActionScript
/**
 Subtring/Top-Tail in Neko
*/

var data = "[this is a test]"
var len = $ssize(data)

$print(data, "\n")
$print($ssub(data, 1, len - 1), "\n")
$print($ssub(data, 0, len - 1), "\n")
$print($ssub(data, 1, len - 2), "\n")
```


{{out}}

```txt
prompt$ nekoc toptail.neko
prompt$ neko toptail.n
[this is a test]
this is a test]
[this is a test
this is a test
```



## Nemerle


```Nemerle
using System;
using System.Console;

module RemoveChars
{
    Main() : void
    {
        def str = "*A string*";
        def end = str.Remove(str.Length - 1);  // from pos to end
        def beg = str.Remove(0, 1);            // start pos, # of chars to remove
        def both = str.Trim(array['*']);       // with Trim() you need to know what char's you're removing

        WriteLine($"$str -> $beg -> $end -> $both");
    }
}
```



## NetRexx


```netrexx
/**********************************************************************
* 02.08.2013 Walter Pachl  translated from REXX
**********************************************************************/
z = 'abcdefghijk'
l=z.length()
say '                  the original string =' z
If l>=1 Then Do
  Say 'string first        character removed =' z.substr(2)
  say 'string         last character removed =' z.left(l-1)
  End
If l>=2 Then
  Say 'string first & last character removed =' z.substr(2,l-2)
```



## NewLISP


```NewLISP
(let (str "rosetta")
  ;; strip first char
  (println (1 str))
  ;; strip last char
  (println (0 -1 str))
  ;; strip both first and last characters
  (println (1 -1 str)))
```



## Nim


```nim
var s = "The quick μ brown fox"
echo(s.substr(1))
echo(s.substr(0,s.len-2))
echo(s.substr(1,s.len-2))
# using slices
echo(s[1 .. -2])
```

{{out}}

```txt
he quick μ brown fox
The quick μ brown fo
he quick μ brown fo
he quick μ brown fo
```



## Objeck


```objeck

bundle Default {
   class TopTail {
      function : Main(args : System.String[]) ~ Nil {
         string := "test";
         string->SubString(1, string->Size() - 1)->PrintLine();
         string->SubString(string->Size() - 1)->PrintLine();
         string->SubString(1, string->Size() - 2)->PrintLine();
      }
   }
}

```



## OCaml



```ocaml
let strip_first_char str =
  if str = "" then "" else
  String.sub str 1 ((String.length str) - 1)

let strip_last_char str =
  if str = "" then "" else
  String.sub str 0 ((String.length str) - 1)

let strip_both_chars str =
  match String.length str with
  | 0 | 1 | 2 -> ""
  | len -> String.sub str 1 (len - 2)

let () =
  print_endline (strip_first_char "knight");
  print_endline (strip_last_char "socks");
  print_endline (strip_both_chars "brooms");
;;
```



## Oforth



```Oforth
: topAndTail(s)
    s right(s size 1-) println
    s left(s size 1-) println
    s extract(2, s size 1- ) println ;
```


{{out}}

```txt

topAndTail("MyString")
yString
MyStrin
yStrin

```



## PARI/GP


```parigp
df(s)=concat(vecextract(Vec(s),1<<#s-2));
dl(s)=concat(vecextract(Vec(s),1<<(#s-1)-1));
db(s)=concat(vecextract(Vec(s),1<<(#s-1)-2));
```



## Pascal

See [[Substring/Top_and_tail#Delphi | Delphi]]


## Perl



```perl
print substr("knight",1), "\n";        # strip first character
print substr("socks", 0, -1), "\n";    # strip last character
print substr("brooms", 1, -1), "\n";   # strip both first and last characters
```


In perl, we can also remove the last character from a string variable with the chop function:


```perl
$string = 'ouch';
$bits = chop($string);       # The last letter is returned by the chop function
print $bits;        # h
print $string;      # ouc    # See we really did chop the last letter off
```



## Perl 6


Perl 6 provides both functional and method forms of substr. Note that, unlike in Perl 5, offsets from the end do not use negative numbers, but instead require a function expressing the negative offset relative to the length parameter, which is supplied by the operator.  The form <tt>*-1</tt> is just a simple way to write such a function.

We use musical sharps and flats to illustrate that Perl is comfortable with characters from any Unicode plane.


```perl6
my $s = '𝄪♯♮♭𝄫';

print qq:to/END/;
    Original:
    $s

    Remove first character:
    { substr($s, 1) }
    { $s.substr(1) }

    Remove last character:
    { substr($s, 0, *-1) }
    { $s.substr( 0, *-1) }
    { $s.chop }

    Remove first and last characters:
    { substr($s, 1, *-1) }
    { $s.substr(1, *-1) }
    END
```

{{out}}

```txt
Original:
𝄪♯♮♭𝄫

Remove first character:
♯♮♭𝄫
♯♮♭𝄫

Remove last character:
𝄪♯♮♭
𝄪♯♮♭
𝄪♯♮♭

Remove first and last characters:
♯♮♭
♯♮♭
```



## Phix


```Phix
constant s = "(test)"
?s[2..-1]
?s[1..-2]
?s[2..-2]
```

{{out}}

```txt

"test)"
"(test"
"test"

```



## PHP



```php
<?php
echo substr("knight", 1), "\n";       // strip first character
echo substr("socks", 0, -1), "\n";    // strip last character
echo substr("brooms", 1, -1), "\n";   // strip both first and last characters
?>
```



## PicoLisp


```PicoLisp
: (pack (cdr (chop "knight")))         # Remove first character
-> "night"

: (pack (head -1 (chop "socks")))      # Remove last character
-> "sock"

: (pack (cddr (rot (chop "brooms"))))  # Remove first and last characters
-> "room"
```



## PL/I


```PL/I

declare s character (100) varying;
s = 'now is the time to come to the aid of the party';
if length(s) <= 2 then stop;
put skip list ('First character removed=' || substr(s,2) );
put skip list ('Last character removed='  || substr(s, 1, length(s)-1) );
put skip list ('One character from each end removed=' ||
   substr(s, 2, length(s)-2) );

```

OUTPUT:

```txt

First character removed=ow is the time to come to the aid of the party
Last character removed=now is the time to come to the aid of the part
One character from each end removed=ow is the time to come to the aid of the part

```



## PowerShell

{{works with|PowerShell|4.0}}

### First method


```PowerShell

$string = "top and tail"
$string
$string.Substring(1)
$string.Substring(0, $string.Length - 1)
$string.Substring(1, $string.Length - 2)

```


### Second method


```PowerShell

$string = "top and tail"
$string
$string[1..($string.Length - 1)] -join ""
$string[0..($string.Length - 2)] -join ""
$string[1..($string.Length - 2)] -join ""

```

<b>Output:</b>

```txt

top and tail
op and tail
top and tai
op and tai

```



## Prolog

Works with SWI-Prolog.


```Prolog
remove_first_last_chars :-
	L = "Rosetta",
	L = [_|L1],
	remove_last(L, L2),
	remove_last(L1, L3),
	writef('Original string		 : %s\n', [L]),
	writef('Without first char       : %s\n', [L1]),
	writef('Without last char        : %s\n', [L2]),
	writef('Without first/last chars : %s\n', [L3]).

remove_last(L, LR) :-
	append(LR, [_], L).
```

Output :

```txt
 ?- remove_first_last_chars.
Original string          : Rosetta
Without first char       : osetta
Without last char        : Rosett
Without first/last chars : osett
true.

```



## PureBasic


```PureBasic
If OpenConsole()
  PrintN(Right("knight", Len("knight") - 1))  ;strip the first letter
  PrintN(Left("socks", Len("socks")- 1))      ;strip the last letter
  PrintN(Mid("brooms", 2, Len("brooms") - 2)) ;strip both the first and last letter

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
night
sock
room
```



## Python



```python
print "knight"[1:]     # strip first character
print "socks"[:-1]     # strip last character
print "brooms"[1:-1]   # strip both first and last characters
```



Or, composing atomic functional expressions for these slices:

```python
from functools import (reduce)


def main():
    for xs in transpose(
        (chunksOf(3)(
            ap([tail, init, compose(init)(tail)])(
                ['knights', 'socks', 'brooms']
            )
        ))
    ):
        print(xs)


# GENERIC -------------------------------------------------

# tail :: [a] -> [a]
def tail(xs):
    return xs[1:]


# init::[a] - > [a]
def init(xs):
    return xs[:-1]


# ap (<*>) :: [(a -> b)] -> [a] -> [b]
def ap(fs):
    return lambda xs: reduce(
        lambda a, f: a + reduce(
            lambda a, x: a + [f(x)], xs, []
        ), fs, []
    )


# chunksOf :: Int -> [a] -> [[a]]
def chunksOf(n):
    return lambda xs: reduce(
        lambda a, i: a + [xs[i:n + i]],
        range(0, len(xs), n), []
    ) if 0 < n else []


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    return lambda f: lambda x: g(f(x))


# transpose :: [[a]] -> [[a]]
def transpose(xs):
    return list(map(list, zip(*xs)))


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
['nights', 'knight', 'night']
['ocks', 'sock', 'ock']
['rooms', 'broom', 'room']
```



## Racket



```racket

#lang racket

(define str "ストリング")

(substring str 1)
(substring str 0 (sub1 (string-length str)))
(substring str 1 (sub1 (string-length str)))

```


Output:


```txt

"トリング"
"ストリン"
"トリン"

```



## Raven


```Raven
define println use $s
   $s print "\n" print

"0123456789" as $str

define offTheTop use $s
   $s 1 0x7FFFFFFF extract

define offTheTail use $s
   $s 0 -1 extract


$str offTheTop  println
$str offTheTail println
$str offTheTop  offTheTail  println
```

{{out}}

```txt
123456789
012345678
12345678
```



## REXX


### error prone

This REXX version is error prone in that if the string is less than two characters, then the   '''left'''   and/or   '''substr'''   BIFs will fail   (because of an invalid length specified).

```rexx
/*REXX program demonstrates removal of  1st/last/1st-and-last  characters from a string.*/
@ = 'abcdefghijk'
say '                  the original string ='  @
say 'string first        character removed ='  substr(@, 2)
say 'string         last character removed ='  left(@, length(@) -1)
say 'string first & last character removed ='  substr(@, 2, length(@) -2)
                                                 /*stick a fork in it,  we're all done. */
   /* ╔═══════════════════════════════════════════════════════════════════════════════╗
      ║  However, the original string may be null or exactly one byte in length which ║
      ║  will cause the  BIFs to fail because of  either zero  or  a negative length. ║
      ╚═══════════════════════════════════════════════════════════════════════════════╝ */
```

'''output'''

```txt

                  the original string = abcdefghijk
string first        character removed = bcdefghijk
string         last character removed = abcdefghij
string first & last character removed = bcdefghij

```



### robust version

This REXX version correctly handles a string of zero (null) or one byte.

```rexx
/*REXX program demonstrates removal of  1st/last/1st-and-last  characters from a string.*/
@ = 'abcdefghijk'
say '                  the original string ='  @
say 'string first        character removed ='  substr(@, 2)
say 'string         last character removed ='  left(@, max(0, length(@) -1))
say 'string first & last character removed ='  substr(@, 2, max(0, length(@) -2))
exit                                             /*stick a fork in it,  we're all done. */

                              /* [↓]  an easier to read version using a length variable.*/
@ = 'abcdefghijk'
L=length(@)
say '                  the original string ='  @
say 'string first        character removed ='  substr(@, 2)
say 'string         last character removed ='  left(@, max(0, L-1) )
say 'string first & last character removed ='  substr(@, 2, max(0, L-2) )
```

'''output'''   is the same as the 1<sup>st</sup> REXX version.


### faster version

This REXX version is faster   (uses   '''parse'''   instead of multiple BIFs).

```rexx
/*REXX program demonstrates removal of  1st/last/1st-and-last  characters from a string.*/
@ = 'abcdefghijk'
say '                  the original string =' @

parse var @ 2 z
say 'string first        character removed =' z

m=length(@) - 1
parse var @ z +(m)
say 'string         last character removed =' z

n=length(@) - 2
parse var @ 2 z +(n)
if n==0  then z=                                /*handle special case of a length of 2.*/
say 'string first & last character removed =' z /*stick a fork in it,  we're all done. */
```

'''output'''   is the same as the 1<sup>st</sup> REXX version.




## Ring


```ring

aString = "1Welcome to the Ring Programming Language2"
see substr(aString,2,len(aString)-1) + nl +
substr(aString,1,len(aString)-1) + nl +
substr(aString,2,len(aString)-2) + nl

```



## Ruby



```ruby
puts "knight"[1..-1]   # strip first character
puts "socks"[0..-2]    # strip last character
puts "socks".chop      # alternate way to strip last character
puts "brooms"[1..-2]   # strip both first and last characters
puts "与今令"[1..-2]    # => 今
```



## Run BASIC


```runbasic
s$  = "Run BASIC"
print mid$(s$,2)             'strip first
print left$(s$,len(s$) -1)   'strip last
print mid$(s$,2,len(s$) -2)  'strip first and last
```



## Scala

{{libheader|Scala}}

```scala
println("knight".tail)               // strip first character
println("socks".init)         // strip last character
println("brooms".tail.init)   // strip both first and last characters
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    const string: stri is "upraisers";
  begin
    writeln("Full string:   " <& stri);
    writeln("Without first: " <& stri[2 ..]);
    writeln("Without last:  " <& stri[.. pred(length(stri))]);
    writeln("Without both:  " <& stri[2 .. pred(length(stri))]);
  end func;
```


Output:

```txt

Full string:   upraisers
Without first: praisers
Without last:  upraiser
Without both:  praiser

```



## Sidef

Strip any characters:

```ruby
say "knight".substr(1);       # strip first character
say "socks".substr(0, -1);    # strip last character
say "brooms".substr(1, -1);   # strip both first and last characters
say "与今令".substr(1, -1);    # => 今
```

{{out}}

```txt

night
sock
room
今

```


Strip graphemes:

```ruby
var gstr = "J\x{332}o\x{332}s\x{332}e\x{301}\x{332}";
say gstr-/^\X/;                    # strip first grapheme
say gstr-/\X\z/;                   # strip last grapheme
say gstr.sub(/^\X/).sub(/\X\z/);   # strip both first and last graphemes
```

{{out}}

```txt

o̲s̲é̲
J̲o̲s̲
o̲s̲

```



## Smalltalk

{{works with|GNU Smalltalk}}
These all use built-in collection methods that will work with any kind of ordered collection, not just Strings. There is no error checking. They will fail if the string is not at least two characters long.

```smalltalk

s := 'upraisers'.
Transcript show: 'Top: ', s allButLast; nl.
Transcript show: 'Tail: ', s allButFirst; nl.
Transcript show: 'Without both: ', s allButFirst allButLast; nl.
Transcript show: 'Without both using substring method: ', (s copyFrom: 2 to: s size - 1); nl.

```

{{out}}

```txt

Top: upraiser
Tail: praisers
Without both: praiser
Without both using substring method: praiser
```



## SNOBOL4


```snobol4
     "knight" len(1) rem . output       ;* strip first character
     "socks" rtab(1) . output           ;* strip last character
     "brooms" len(1) rtab(1) . output   ;* strip both first and last characters
```



## Standard ML


```sml
- val str = "abcde";
val str = "abcde" : string
- String.substring(str, 1, String.size str - 1);
val it = "bcde" : string
- String.substring(str, 0, String.size str - 1);
val it = "abcd" : string
- String.substring(str, 1, String.size str - 2);
val it = "bcd" : string
```



## Swift

Swift strings are native Unicode strings and do not index through the code points. Swift's <code>String.Index</code> refers to true Unicode characters (Unicode grapheme clusters). Swift standard library has generic functionality that not only works with strings, but also with any type that conforms to relevant protocols. The first method presented here uses generic functions from Swift standard library:


```swift
let txt = "0123456789"
println(dropFirst(txt))
println(dropLast(txt))
println(dropFirst(dropLast(txt)))
```

{{out}}

```txt
123456789
012345678
12345678
```

The other method is slicing by range subscripting:

```swift
let txt = "0123456789"
println(txt[txt.startIndex.successor() ..< txt.endIndex])
println(txt[txt.startIndex             ..< txt.endIndex.predecessor()])
println(txt[txt.startIndex.successor() ..< txt.endIndex.predecessor()])
```

{{out}}

```txt
123456789
012345678
12345678
```

Another way is mutating the string:

```swift
var txt = "0123456789"
txt.removeAtIndex(txt.startIndex)
txt.removeAtIndex(txt.endIndex.predecessor())
```

The above functions return what they remove.
You can also extend String type and define BASIC-style functions:

```swift
extension String {

    /// Ensure positive indexes

    private func positive(index: Int) -> Int {

        if index >= 0 { return index }

        return count(self) + index
    }

    /// Unicode character by zero-based integer (character) `index`
    /// Supports negative character index to count from end. (-1 returns character before last)

    subscript(index: Int) -> Character {

        return self[advance(startIndex, positive(index))]
    }

    /// String slice by character index

    subscript(range: Range<Int>) -> String {

        return self[advance(startIndex, range.startIndex) ..<
                    advance(startIndex, range.endIndex, endIndex)]
    }

    /// Left portion of text to `index`

    func left(index : Int) -> String {

        return self[0 ..< positive(index)]
    }

    /// Right portion of text from `index`

    func right(index : Int) -> String{

        return self[positive(index) ..< count(self)]
    }

    /// From `start` index until `end` index

    func mid(start: Int, _ end: Int) -> String {

        return self[positive(start) ..< positive(end)]
    }

}

let txt = "0123456789"

txt.right(1) // Right part without first character
txt.left(-1) // Left part without last character
txt.mid(1,-1) // Middle part without first and last character
```



## Tcl


```tcl
puts [string range "knight" 1 end];		# strip first character
puts [string range "write" 0 end-1];		# strip last character
puts [string range "brooms" 1 end-1];		# strip both first and last characters
```



## TorqueScript

String with first character removed
  %string = "Moo";
  %string = getSubStr(%string, 1, strLen(%string) - 1);
  echo(%string);
String with last character removed
  %string = "Moo";
  %string = getSubStr(%string, 0, strLen(%string) - 1);
  echo(%string);
String with both the first and last characters removed
  %string = "Moo";
  %string = getSubStr(%string, 1, strLen(%string) - 2);
  echo(%string);


Output:
  oo
  Mo
  o


## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
str="upraisers"
str1=EXTRACT (str,2,0)
str2=EXTRACT (str,0,-1)
str3=EXTRACT (str,2,-1)
PRINT str
PRINT str1
PRINT str2
PRINT str3

```

Output:

```txt

upraisers
praisers
upraiser
upraiser

```



## UNIX Shell


First ''or'' last character:


```bash
str='abcdefg'
echo "${str#?}"   # Remove first char
echo "${str%?}"   # Remove last char
```


First ''and'' last character:

: Only zsh supports nested string manipulation.
:
```bash
echo ${${str#?}%?}   # Remove first & last chars
```

: bash and ksh, use substring expansion, from character index 1 for length of (string length) minus 2
:
```bash
echo "${s:1:${#s}-2}"
```

: POSIX shells like dash, need a temp variable
:
```bash
tmp=${s#?}; tmp=${tmp%?}; echo "$tmp"
```



## Vala


```vala

// declare test string
string s = "Hello, world!";
// remove first letter
string s_first = s[1:s.length];
//remove last letter
string s_last = s[0:s.length - 1];
// remove first and last letters
string s_first_last = s[1:s.length - 1];

```



## VBScript


```VBScript
Function TopNTail(s,mode)
	Select Case mode
		Case "top"
			TopNTail = Mid(s,2,Len(s)-1)
		Case "tail"
			TopNTail = Mid(s,1,Len(s)-1)
		Case "both"
			TopNTail = Mid(s,2,Len(s)-2)
	End Select
End Function

WScript.Echo "Top: UPRAISERS = " & TopNTail("UPRAISERS","top")
WScript.Echo "Tail: UPRAISERS = " & TopNTail("UPRAISERS","tail")
WScript.Echo "Both: UPRAISERS = " & TopNTail("UPRAISERS","both")
```


{{out}}

```txt
Top: UPRAISERS = PRAISERS
Tail: UPRAISERS = UPRAISER
Both: UPRAISERS = PRAISER
```



## XPL0


```XPL0
include c:\cxpl\stdlib;
char S, P;
[S:= "Smiles";
Text(0, S+1);           \first character removed
CrLf(0);
P:= S + StrLen(S) - 2;  \point to last character in string
P(0):= P(0) ! $80;      \set the MSb on the last character
Text(0, S);             \last character removed
CrLf(0);
Text(0, S+1);           \first and last characters removed
CrLf(0);
]
```


Output:

```txt

miles
Smile
mile

```



## zkl


```zkl
"Smiles"[1,*]  //-->miles
"Smiles"[0,-1] //-->Smile
"Smiles"[1,-1] //-->mile
```

[] format is [offset (zero based), length].  * means "to the end", a
negative number means from the end.


## ZX Spectrum Basic



```zxbasic
10 PRINT FN f$("knight"): REM strip the first letter. You can also write PRINT "knight"(2 TO)
20 PRINT FN l$("socks"): REM strip the last letter
30 PRINT FN b$("brooms"): REM strip both the first and last letter
100 STOP

9000 DEF FN f$(a$)=a$(2 TO LEN(a$))
9010 DEF FN l$(a$)=a$(1 TO LEN(a$)-(1 AND (LEN(a$)>=1)))
9020 DEF FN b$(a$)=FN l$(FN f$(a$))
```


{{omit from|GUISS}}
{{omit from|Openscad}}
