+++
title = "Split a character string based on change of character"
description = ""
date = 2019-10-17T21:27:12Z
aliases = []
[extra]
id = 21261
[taxonomies]
categories = []
tags = []
+++

[[Category: String manipulation]]
[[Category:Simple]]
{{task}}

<!--  this problem is also known as  "splitsville"  elsewhere.  !-->
<!--  I imagine this Rosetta Code task will lead to quite a few code-golf solutions.  !-->

;Task:
Split a (character) string into comma (plus a blank) delimited
strings based on a change of character   (left to right).

Show the output here   (use the 1<sup>st</sup> example below).


Blanks should be treated as any other character   (except
they are problematic to display clearly).   The same applies
to commas.


For instance, the string:
 <big><big> gHHH5YY++///\ </big></big>
should be split and show:
 <big><big> g, HHH, 5, YY, ++, ///, \ </big></big>





## Ada


```ada

with Ada.Text_IO;
procedure Split is
  procedure Print_Tokens (s : String) is
    i, j : Integer := s'First;
  begin
    loop
      while j<=s'Last and then s(j)=s(i) loop j := j + 1; end loop;
      if i/=s'first then Ada.Text_IO.Put (", "); end if;
      Ada.Text_IO.Put (s(i..j-1));
      i := j;
      exit when j>s'last;
    end loop;
  end Print_Tokens;
begin
  Print_Tokens ("gHHH5YY+++");
end split;

```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}}

```algol68
BEGIN
    # returns s with ", " added between each change of character #
    PROC split on characters = ( STRING s )STRING:
         IF s = "" THEN
            # empty string #
            ""
         ELSE
            # allow for 3 times as many characters as in the string #
            # this would handle a string of unique characters       #
            [ 3 * ( ( UPB s - LWB s ) + 1 ) ]CHAR result;
            INT  r pos  := LWB result;
            INT  s pos  := LWB s;
            CHAR s char := s[ LWB s ];
            FOR s pos FROM LWB s TO UPB s DO
                IF s char /= s[ s pos ] THEN
                    # change of character - insert ", " #
                    result[ r pos     ] := ",";
                    result[ r pos + 1 ] := " ";
                    r pos +:= 2;
                    s char := s[ s pos ]
                FI;
                result[ r pos ] := s[ s pos ];
                r pos +:= 1
            OD;
            # return the used portion of the result #
            result[ 1 : r pos - 1 ]
         FI ; # split on characters #

    print( ( split on characters( "gHHH5YY++///\" ), newline ) )
END
```

{{out}}

```txt

g, HHH, 5, YY, ++, ///, \

```



## ANSI BASIC


```ansibasic>REM
split
DECLARE EXTERNAL FUNCTION FN_split$

PRINT FN_split$( "gHHH5YY++///\" )
END

EXTERNAL FUNCTION FN_split$( s$ )
LET c$ = s$(1:1)
LET split$ = ""
FOR i = 1 TO LEN(s$)
  LET d$ = s$(i:i)
  IF d$ <> c$ THEN
    LET split$ = split$ & ", "
    LET c$ = d$
  END IF
  LET split$ = split$ & d$
NEXT i
LET FN_split$ = split$
END FUNCTION
```

{{out}}

```txt
g, HHH, 5, YY, ++, ///, \
```



## AppleScript

{{Trans|JavaScript}}

```AppleScript
intercalate(", ", ¬
    map(curry(intercalate)'s |λ|(""), ¬
        group("gHHH5YY++///\\")))

--> "g, HHH, 5, YY, ++, ///, \\"


-- GENERIC FUNCTIONS ----------------------------------------------------------
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
        tell foldl(enGroup, {active:{item 1 of xs}, sofar:{}}, tail(xs))
            if length of (its active) > 0 then
                its sofar & its active
            else
                {}
            end if
        end tell
    else
        {}
    end if
end groupBy

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

-- tail :: [a] -> [a]
on tail(xs)
    if length of xs > 1 then
        items 2 thru -1 of xs
    else
        {}
    end if
end tail
```

{{Out}}

```txt
g, HHH, 5, YY, ++, ///, \
```


## ARM Assembly



{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program splitcar.s   */

/************************************/
/* Constantes                       */
/************************************/
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall

/*********************************/
/* Initialized data              */
/*********************************/
.data
szCarriageReturn:   .asciz "\n"
szString1:          .asciz "gHHH5YY++///\\"
/*   IMPORTANT REMARK for compiler as
The way to get special characters into a string is to escape these characters: precede them
with a backslash ‘\’ character. For example ‘\\’ represents one backslash: the first \ is
an escape which tells as to interpret the second character literally as a backslash (which
prevents as from recognizing the second \ as an escape character).
*/

/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
sBuffer:               .skip  100

/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                                             @ entry of program

    ldr r0,iAdrszString1                          @ input string address
    ldr r1,iAdrsBuffer                            @ output buffer address
    bl split

    ldr r0,iAdrsBuffer
    bl affichageMess                              @ display message
    ldr r0,iAdrszCarriageReturn
    bl affichageMess


100:                                              @ standard end of the program
    mov r0, #0                                    @ return code
    mov r7, #EXIT                                 @ request to exit program
    svc #0                                        @ perform the system call

iAdrszString1:            .int szString1
iAdrszCarriageReturn:     .int szCarriageReturn
iAdrsBuffer:              .int sBuffer

/******************************************************************/
/*     generate value                                  */
/******************************************************************/
/* r0 contains the address of input string  */
/* r1 contains the address of output buffer  */

split:
    push {r1-r5,lr}                           @ save registers
    mov r4,#0                                 @ indice loop input string
    mov r5,#0                                 @ indice buffer
    ldrb r2,[r0,r4]                           @ read first char in reg r2
    cmp r2,#0                                 @ if null -> end
    beq 3f
    strb r2,[r1,r5]                           @ store char in buffer
    add r5,#1                                 @ increment location buffer
1:
    ldrb r3,[r0,r4]                           @read char[r4] in reg r3
    cmp r3,#0                                 @ if null  end
    beq 3f
    cmp r2,r3                                 @ compare two characters
    streqb r3,[r1,r5]                         @ = -> store char in buffer
    beq 2f                                    @ loop

    mov r2,#','                               @ else store comma in buffer
    strb r2,[r1,r5]                           @ store char in buffer
    add r5,#1
    mov r2,#' '                               @ and store space in buffer
    strb r2,[r1,r5]
    add r5,#1
    strb r3,[r1,r5]                           @ and store input char in buffer
    mov r2,r3                                 @ and maj r2 with new char
2:
    add r5,#1                                 @ increment indices
    add r4,#1
    b 1b                                      @ and loop
3:
    strb r3,[r1,r5]                           @ store zero final in buffer
100:
    pop {r1-r5,lr}
    bx lr                                     @ return

/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                          @ save  registres
    mov r2,#0                                      @ counter length
1:                                                 @ loop length calculation
    ldrb r1,[r0,r2]                                @ read octet start position + index
    cmp r1,#0                                      @ if 0 its over
    addne r2,r2,#1                                 @ else add 1 in the length
    bne 1b                                         @ and loop
                                                   @ so here r2 contains the length of the message
    mov r1,r0                                      @ address message in r1
    mov r0,#STDOUT                                 @ code to write to the standard output Linux
    mov r7, #WRITE                                 @ code call system "write"
    svc #0                                         @ call systeme
    pop {r0,r1,r2,r7,lr}                           @ restaur des  2 registres */
    bx lr                                          @ return

output : gg, HHH, 5, YY, ++, ///, \



```



## AWK


```AWK

# syntax: GAWK -f SPLIT_A_CHARACTER_STRING_BASED_ON_CHANGE_OF_CHARACTER.AWK
BEGIN {
    str = "gHHH5YY++///\\"
    printf("old: %s\n",str)
    printf("new: %s\n",split_on_change(str))
    exit(0)
}
function split_on_change(str,  c,i,new_str) {
    new_str = substr(str,1,1)
    for (i=2; i<=length(str); i++) {
      c = substr(str,i,1)
      if (substr(str,i-1,1) != c) {
        new_str = new_str ", "
      }
      new_str = new_str c
    }
    return(new_str)
}

```

{{out}}

```txt

old: gHHH5YY++///\
new: g, HHH, 5, YY, ++, ///, \

```



## BaCon

Literal strings in BaCon are passed to the C compiler as they are; a backslash therefore needs to be escaped.

```freebasic
txt$ = "gHHH5YY++///\\"

c$ = LEFT$(txt$, 1)

FOR x = 1 TO LEN(txt$)
    d$ = MID$(txt$, x, 1)
    IF d$ <> c$ THEN
        PRINT ", ";
        c$ = d$
    END IF
    PRINT d$;
NEXT
```

{{out}}

```txt

g, HHH, 5, YY, ++, ///, \

```



## BBC BASIC


```bbcbasic>REM
split
PRINT FN_split( "gHHH5YY++///\" )
END

DEF FN_split( s$ )
LOCAL c$, split$, d$, i%
c$ = LEFT$( s$, 1 )
split$ = ""
FOR i% = 1 TO LEN s$
  LET d$ = MID$( s$, i%, 1 )
  IF d$ <> c$ THEN
    split$ += ", "
    c$ = d$
  ENDIF
  split$ += d$
NEXT
= split$
```

{{out}}

```txt
g, HHH, 5, YY, ++, ///, \
```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
char *split(char *str);
int main(int argc,char **argv)
{
	char input[13]="gHHH5YY++///\\";
	printf("%s\n",split(input));
}
char *split(char *str)
{
	char last=*str,*result=malloc(3*strlen(str)),*counter=result;
	for (char *c=str;*c;c++) {
		if (*c!=last) {
			strcpy(counter,", ");
			counter+=2;
			last=*c;
		}
		*counter=*c;
		counter++;
	}
	*(counter--)='\0';
	return realloc(result,strlen(result));
}
```

{{out}}

```txt

g, HHH, 5, YY, ++, ///, \

```



## C#


```c#
using System;
using System.Linq;
using System.Collections.Generic;

public class Program
{
    string s = @"gHHH5YY++///\";
    Console.WriteLine(s.RunLengthSplit().Delimit(", "));
}

public static class Extensions
{
    public static IEnumerable<string> RunLengthSplit(this string source) {
        using (var enumerator = source.GetEnumerator()) {
            if (!enumerator.MoveNext()) yield break;
            char previous = enumerator.Current;
            int count = 1;
            while (enumerator.MoveNext()) {
                if (previous == enumerator.Current) {
                    count++;
                } else {
                    yield return new string(Enumerable.Repeat(previous, count).ToArray());
                    previous = enumerator.Current;
                    count = 1;
                }
            }
            yield return new string(Enumerable.Repeat(previous, count).ToArray());
        }
    }

    public static string Delimit<T>(this IEnumerable<T> source, string separator = "") => string.Join(separator ?? "", source);
}
```

{{out}}

```txt

g, HHH, 5, YY, ++, ///, \

```



## C++


```cpp

// Solution for http://rosettacode.org/wiki/Split_a_character_string_based_on_change_of_character
#include<string>
#include<iostream>

auto split(const std::string& input, const std::string& delim){
	std::string res;
	for(auto ch : input){
		if(!res.empty() && ch != res.back())
			res += delim;
		res += ch;
	}
	return res;
}

int main(){
	std::cout << split("gHHH5  ))YY++,,,///\\", ", ") << std::endl;
}
```

{{out}}

```txt
g, HHH, 5,   , )), YY, ++, ,,,, ///, \
```



## Clojure


```clojure
(defn print-cchanges [s]
  (println (clojure.string/join ", " (map first (re-seq #"(.)\1*" s)))))

(print-cchanges "gHHH5YY++///\\")

```

{{out}}

```txt
g, HHH, 5, YY, ++, ///, \
```



## COBOL


```COBOL

       identification division.
       program-id. split-ch.
       data division.
       1 split-str pic x(30) value space.
       88 str-1 value "gHHH5YY++///\".
       88 str-2 value "gHHH5  ))YY++,,,///\".
       1 binary.
        2 ptr pic 9(4) value 1.
        2 str-start pic 9(4) value 1.
        2 delim-len pic 9(4) value 1.
        2 split-str-len pic 9(4) value 0.
        2 trash-9 pic 9(4) value 0.
       1 delim-char pic x value space.
       1 delim-str pic x(6) value space.
       1 trash-x pic x.
       procedure division.
           display "Requested string"
           set str-1 to true
           perform split-init-and-go
           display space
           display "With spaces and commas"
           set str-2 to true
           perform split-init-and-go
           stop run
           .

       split-init-and-go.
           move 1 to ptr
           move 0 to split-str-len
           perform split
           .

       split.
           perform get-split-str-len
           display split-str (1:split-str-len)
           perform until ptr > split-str-len
               move ptr to str-start
               move split-str (ptr:1) to delim-char
               unstring split-str (1:split-str-len)
                   delimited all delim-char
                   into trash-x delimiter delim-str
                   pointer ptr
               end-unstring
               subtract str-start from ptr giving delim-len
               move split-str (str-start:delim-len)
                   to delim-str (1:delim-len)
               display delim-str (1:delim-len) with no advancing
               if ptr <= split-str-len
                   display ", " with no advancing
               end-if
           end-perform
           display space
           .

       get-split-str-len.
           inspect function reverse (split-str) tallying
               trash-9 for leading space
               split-str-len for characters after space
           .

       end program split-ch.

```

{{out}}

```txt

Requested string
gHHH5YY++///\
g, HHH, 5, YY, ++, ///, \

With spaces and commas
gHHH5  ))YY++,,,///\
g, HHH, 5,   , )), YY, ++, ,,,, ///, \

```



## Common Lisp



```lisp
(defun split (string)
  (loop :for prev := nil :then c
     :for c :across string
     :do (format t "~:[~;, ~]~c" (and prev (char/= c prev)) c)))

(split "gHHH5YY++///\\")

```

{{out}}

```txt
g, HHH, 5, YY, ++, ///, \
```


Doing more work that what's being ask, the following solution builds a list of strings then output it:


```lisp
(defun split (string)
  (flet ((make-buffer ()
           (make-array 0 :element-type 'character :adjustable t :fill-pointer t)))
    (loop with buffer = (make-buffer)
          with result
          for prev = nil then c
          for c across string
          when (and prev (char/= c prev))
            do (push buffer result)
               (setf buffer (make-buffer))
          do (vector-push-extend c buffer)
          finally (push buffer result)
                  (format t "~{~A~^, ~}"(nreverse result)))))

(split "gHHH5YY++///\\")
```

{{out}}

```txt
g, HHH, 5, YY, ++, ///, \
```



## D



```D
import std.stdio;

void main() {
    auto source = "gHHH5YY++///\\";

    char prev = source[0];
    foreach(ch; source) {
        if (prev != ch) {
            prev = ch;
            write(", ");
        }
        write(ch);
    }
    writeln();
}
```


{{output}}

```txt
g, HHH, 5, YY, ++, ///, \
```



## Dyalect



```dyalect
func String.smartSplit() {
    var c
    var str = ""
    var last = this.len() - 1

    for n in 0..last {
        if c && this[n] != c {
            str += ", "
        }
        c = this[n]
        str += c
    }

    str
}

print("gHHH5YY++///\\".smartSplit())
```


{{out}}


```txt
g, HHH, 5, YY, ++, ///, \
```



## EasyLang


<lang>a$ = "gHHH5YY++///\"
a$[] = str_split a$
c$ = a$[0]
for i range len a$[]
  if a$[i] <> c$
    b$ &= ", "
    c$ = a$[i]
  .
  b$ &= a$[i]
.
print b$
```

{{out}}

```txt

g, HHH, 5, YY, ++, ///, \

```



## Elixir


```elixir
split = fn str ->
          IO.puts " input string: #{str}"
          String.graphemes(str)
          |> Enum.chunk_by(&(&1))
          |> Enum.map_join(", ", &Enum.join &1)
          |> fn s -> IO.puts "output string: #{s}" end.()
        end

split.("gHHH5YY++///\\")
```


{{out}}

```txt

 input string: gHHH5YY++///\
output string: g, HHH, 5, YY, ++, ///, \

```


=={{header|F_Sharp|F#}}==

```fsharp
open System.Text.RegularExpressions
let splitRuns s = Regex("""(.)\1*""").Matches(s) |> Seq.cast<Match> |> Seq.map (fun m -> m.Value) |> Seq.toList
printfn "%A" (splitRuns """gHHH5YY++///\""")
```

{{out}}

```txt
["g"; "HHH"; "5"; "YY"; "++"; "///"; "\"]
```



## Factor


```factor
USE: splitting.monotonic
"gHHH5YY++///\\"
"aaabbccccdeeff" [ [ = ] monotonic-split ", " join print ] bi@
```

{{out}}

```txt

g, HHH, 5, YY, ++, ///, \
aaa, bb, cccc, d, ee, ff

```



## Forth

{{works with|Gforth|0.7.3}}

```Forth
CREATE A 0 ,
: C@A+   A @ C@  [ 1 CHARS ]L A +! ;
: SPLIT. ( c-addr u --) SWAP A !  A @ C@
   BEGIN OVER WHILE
     C@A+  TUCK  <> IF ." , " THEN
     DUP EMIT  SWAP 1- SWAP
   REPEAT  DROP ;
: TEST   OVER OVER
   ." input: " TYPE CR
   ." split: " SPLIT. CR ;
s" gHHH5YY++///\"        TEST
s" gHHH5  ))YY++,,,///\" TEST
BYE
```

{{out}}

```txt
input: gHHH5YY++///\
split: g, HHH, 5, YY, ++, ///, \
input: gHHH5  ))YY++,,,///\
split: g, HHH, 5,   , )), YY, ++, ,,,, ///, \
```



## Fortran

This is F77 style, except for the <code>END SUBROUTINE SPLATTER</code> which would be just <code>END</code>, which for F90 is also allowable outside of the MODULE protocol. Linking the start/stop markers by giving the same name is helpful, especially when the compiler checks for this. The $ symbol at the end of a FORMAT code sequence is a common F77 extension, meaning "do not finish the line" so that a later output will follow on. This is acceptable to F90 and is less blather than adding the term <code>,ADVANCE = "NO"</code> inside a WRITE statement that would otherwise be required. Output is to I/O unit <code>6</code> which is the modern default for "standard output". The format code is <code>A</code> meaning "any number of characters" rather than <code>A1</code> for "one character" so as to accommodate not just the single character from TEXT but also the two characters of ", " for the splitter between sequences. Alas, there is no provision to change fount or colour for this, to facilitate the reader's attempts to parse the resulting list especially when the text includes commas or spaces of its own. By contrast, with quoted strings, the standard protocol is to double contained quotes.

An alternative method would be to prepare the entire output in a CHARACTER variable then write that, but this means answering the maddening question "how long is a piece of string?" for that variable, though later Fortran has arrangements whereby a text variable is resized to suit on every assignment, as in <code>TEMP = TEMP // more</code> - but this means repeatedly copying the text to the new manifestation of the variable. Still another approach would be to prepare an array of fingers to each split point (as in [[Phrase_reversals#Fortran]]) so that the final output would be a single WRITE using that array, and again, how big must the array be? At most, as big as the number of characters in TEXT. With F90, subroutines can declare arrays of a size determined on entry, with something like <code>INTEGER A(LEN(TEXT))</code>

If the problem were to be solved by writing a "main line" only, there would have to be a declaration of the text variable there but since a subroutine can receive a CHARACTER variable of any size (the actual size is passed as a secret parameter), this can be dodged.

For this example a DO-loop stepping along the text is convenient, but in a larger context it would probably be most useful to work along the text with fingers L1 and L2 marking the start and finish positions of each sequence.
```Fortran
      SUBROUTINE SPLATTER(TEXT)	!Print a comma-separated list. Repeated characters constitute one item.
Can't display the inserted commas in a different colour so as not to look like any commas in TEXT.
       CHARACTER*(*) TEXT	!The text.
       INTEGER L	!A finger.
       CHARACTER*1 C	!A state follower.
        IF (LEN(TEXT).LE.0) RETURN	!Prevent surprises in the following..
        C = TEXT(1:1)			!Syncopation: what went before.
        DO L = 1,LEN(TEXT)	!Step through the text.
          IF (C.NE.TEXT(L:L)) THEN	!A change of character?
            C = TEXT(L:L)			!Yes. This is the new normal.
            WRITE (6,1) ", "			!Set off from what went before. This is not from TEXT.
          END IF			!So much for changes.
          WRITE (6,1) C			!Roll the current character. (=TEXT(L:L))
    1     FORMAT (A,$)			!The $ sez: do not end the line.
        END DO			!On to the next character.
        WRITE (6,1)	!Thus end the line. No output item means that the $ is not reached, so the line is ended.
      END SUBROUTINE SPLATTER	!TEXT with spaces, or worse, commas, will produce an odd-looking list.

      PROGRAM POKE
      CALL SPLATTER("gHHH5YY++///\")	!The example given.
      END
```

Unfortunately, the syntax highlighter has failed to notice the terminating quote character, presumably because the preceding backslash might be an "escape sequence" trigger, a facility ''not'' used in Fortran text ''literals'' except possibly as a later modernist option.

{{Out}}

```txt

g, HHH, 5, YY, ++, ///, \

```



## Go

Treating "character" as a byte:

```go
package main

import (
    "fmt"
    "strings"
)

func main() {
    fmt.Println(scc(`gHHH5YY++///\`))
}

func scc(s string) string {
    if len(s) < 2 {
        return s
    }
    var b strings.Builder
    p := s[0]
    b.WriteByte(p)
    for _, c := range []byte(s[1:]) {
        if c != p {
            b.WriteString(", ")
        }
        b.WriteByte(c)
        p = c
    }
    return b.String()
}
```

{{out}}

```txt

g, HHH, 5, YY, ++, ///, \

```



## Haskell



```Haskell
import Data.List (group, intercalate)

main :: IO ()
main = putStrLn $ intercalate ", " (group "gHHH5YY++///\\")
```


{{Out}}

```txt
g, HHH, 5, YY, ++, ///, \
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 LET S$="gHHH5YY++///\"
110 PRINT S$(1);
120 FOR I=2 TO LEN(S$)
130   IF S$(I)<>S$(I-1) THEN PRINT ", ";
140   PRINT S$(I);
150 NEXT
160 PRINT
```



## J

'''Solution:'''

```j
splitChars=: (1 ,~ 2 ~:/\ ]) <;.2 ]
delimitChars=: ', ' joinstring splitChars
```

'''Example Usage:'''

```j
   delimitChars 'gHHH5YY++///\'
g, HHH, 5, YY, ++, ///, \
```



## Java



```Java
package org.rosettacode;

import java.util.ArrayList;
import java.util.List;


/**
 * This class provides a main method that will, for each arg provided,
 * transform a String into a list of sub-strings, where each contiguous
 * series of characters is made into a String, then the next, and so on,
 * and then it will output them all separated by a comma and a space.
 */
public class SplitStringByCharacterChange {

    public static void main(String... args){
        for (String string : args){

            List<String> resultStrings = splitStringByCharacter(string);
            String output = formatList(resultStrings);
            System.out.println(output);
        }
    }

    /**
     * @param string String - String to split
     * @return List<\String> - substrings of contiguous characters
     */
    public static List<String> splitStringByCharacter(String string){

        List<String> resultStrings = new ArrayList<>();
        StringBuilder currentString = new StringBuilder();

        for (int pointer = 0; pointer < string.length(); pointer++){

            currentString.append(string.charAt(pointer));

            if (pointer == string.length() - 1
                    || currentString.charAt(0) != string.charAt(pointer + 1)) {
                resultStrings.add(currentString.toString());
                currentString = new StringBuilder();
            }
        }

        return resultStrings;
    }

    /**
     * @param list List<\String> - list of strings to format as a comma+space-delimited string
     * @return String
     */
    public static String formatList(List<String> list){

        StringBuilder output = new StringBuilder();

        for (int pointer = 0; pointer < list.size(); pointer++){
            output.append(list.get(pointer));

            if (pointer != list.size() - 1){
                output.append(", ");
            }
        }

        return output.toString();
    }
}
```


{{Out}}

```txt
g, HHH, 5, YY, ++, ///, \
```



## JavaScript


### ES6

{{Trans|Haskell}}

```JavaScript
(() => {
    // GENERIC FUNCTIONS ------------------------------------------------------

    // concat :: [[a]] -> [a] | [String] -> String
    const concat = xs =>
        xs.length > 0 ? (() => {
            const unit = typeof xs[0] === 'string' ? '' : [];
            return unit.concat.apply(unit, xs);
        })() : [];

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
                    active: blnGroup ? a.active.concat([x]) : [x],
                    sofar: blnGroup ? a.sofar : a.sofar.concat([a.active])
                };
            }, {
                active: xs.length > 0 ? [xs[0]] : [],
                sofar: []
            });
        return dct.sofar.concat(dct.active.length > 0 ? [dct.active] : []);
    };

    // intercalate :: String -> [a] -> String
    const intercalate = (s, xs) => xs.join(s);

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // show :: a -> String
    const show = (...x) =>
        JSON.stringify.apply(
            null, x.length > 1 ? [x[0], null, x[1]] : x
        );

    // stringChars :: String -> [Char]
    const stringChars = s => s.split('');


    // TEST -------------------------------------------------------------------
    return show(
        intercalate(', ',
            map(concat, group(stringChars('gHHH5YY++///\\')))
        )
    );

    // -> "g, HHH, 5, YY, ++, ///, \\"
})();
```

{{Out}}

```txt
g, HHH, 5, YY, ++, ///, \
```



## jq


```jq
# input: a string
# output: a stream of runs
def runs:
  def init:
    explode as $s
    | $s[0] as $i
    | (1 | until( $s[.] != $i; .+1));
  if length == 0 then empty
  elif length == 1 then .
  else init as $n | .[0:$n], (.[$n:] | runs)
  end;

"gHHH5YY++///\\" | [runs] | join(", ")
```

{{out}}
Using the -r ("raw output") command-line option of jq:

```txt
g, HHH, 5, YY, ++, ///, \
```



## Jsish

Showing off a little unit testing...

Starting with

```javascript
#!/usr/bin/env jsish
;'Split a string based on change of character, in Jsish';

function splitOnChange(str:string):string {
    if (str.length < 2) return str;
    var last = str[0];
    var result = last;
    for (var pos = 1; pos < str.length; pos++) {
        result += ((last == str[pos]) ? last : ', ' + str[pos]);
        last = str[pos];
    }
    return result;
}
provide('splitOnChange', 1.0);

/* literal backslash needs escaping during initial processing */
;splitOnChange('gHHH5YY++///\\');
;splitOnChange('a');
;splitOnChange('ab');
;splitOnChange('aaa');
;splitOnChange('aaaba');
;splitOnChange('gH HH5YY++//,/\\');
```


Then

```txt

prompt$ jsish -u -update true splitOnChange.jsi
Created splitOnChange.jsi
```


Giving


```javascript
#!/usr/bin/env jsish
;'Split a string based on change of character, in Jsish';

function splitOnChange(str:string):string {
    if (str.length < 2) return str;
    var last = str[0];
    var result = last;
    for (var pos = 1; pos < str.length; pos++) {
        (last == str[pos]) ? result += last : result += ', ' + str[pos];
        last = str[pos];
    }
    return result;
}
provide('splitOnChange', 1.0);

/* literal backslash needs escaping during initial processing */
;splitOnChange('gHHH5YY++///\\');
;splitOnChange('a');
;splitOnChange('ab');
;splitOnChange('aaa');
;splitOnChange('aaaba');
;splitOnChange('gH HH5YY++//,/\\');

/*
=!EXPECTSTART!=
'Split a string based on change of character, in Jsish'
splitOnChange('gHHH5YY++///\') ==> g, HHH, 5, YY, ++, ///, \
splitOnChange('a') ==> a
splitOnChange('ab') ==> a, b
splitOnChange('aaa') ==> aaa
splitOnChange('aaaba') ==> aaa, b, a
splitOnChange('gH HH5YY++//,/\') ==> g, H,  , HH, 5, YY, ++, //, ,, /, \
=!EXPECTEND!=
*/
```


Which tests as:


```txt
prompt$ jsish -u splitOnChange.jsi
[PASS] splitOnChange.jsi
```


And then satisfying the task of showing the one result, using the script as a module:

{{out}}

```txt
prompt$ jsish
Jsish interactive: see 'help [cmd]'.  \ cancels > input.  ctrl-c aborts running script.
# require('splitOnChange');
1
# puts(splitOnChange('gHHH5YY++///\\'));
g, HHH, 5, YY, ++, ///, \
```



## Julia


```julia
# v0.6
using IterTools

str = "gHHH5YY++///\\"
sep = map(join, groupby(identity, str))
println("string: $str\nseparated: ", join(sep, ", "))
```


{{out}}

```txt
string: gHHH5YY++///\
separated: g, HHH, 5, YY, ++, ///, \
```



## Kotlin


```scala
// version 1.0.6

fun splitOnChange(s: String): String {
    if (s.length < 2) return s
    var t = s.take(1)
    for (i in 1 until s.length)
        if (t.last() == s[i]) t += s[i]
        else t += ", " + s[i]
    return t
}

fun main(args: Array<String>) {
    val s = """gHHH5YY++///\"""
    println(splitOnChange(s))
}
```


{{out}}

```txt

g, HHH, 5, YY, ++, ///, \

```



## Lua

Note that the backslash must be quoted as a double backslash as Lua uses C-like escape sequences.

```Lua
function charSplit (inStr)
    local outStr, nextChar = inStr:sub(1, 1)
    for pos = 2, #inStr do
        nextChar = inStr:sub(pos, pos)
        if nextChar ~= outStr:sub(#outStr, #outStr) then
            outStr = outStr .. ", "
        end
        outStr = outStr .. nextChar
    end
    return outStr
end

print(charSplit("gHHH5YY++///\\"))
```

{{out}}

```txt
g, HHH, 5, YY, ++, ///, \
```


'''Alternative:'''
Simply scan difference in reverse order and insert delimiter in place, the loop counter i will not update with length of s.

```lua
function splitdiff(s)
  for i=#s,2,-1 do
    if s:sub(i,i)~=s:sub(i-1,i-1) then
      s = s:sub(1,i-1)..', '.. s:sub(i,-1)
    end
  end
  return s
end
```



## M2000 Interpreter

Stack New open a new stack object as current stack, and keep the old one. After the end of block execution old stack get back as current stack. Data statement push to bottom (we read from top, so using data we get a FIFO type). Letter$ pops a string or raise an error if no string found at the top of stack.


```M2000 Interpreter

Module PrintParts(splitthis$) {
      Def string m$, p$
      Def long c
      Stack New {
            if len(splitthis$)=0 then exit
            For i=1 to len(splitthis$)
                  p$=mid$(splitthis$,i,1)
                  if m$<>p$ then {
                        if c>0 then data string$(m$, c)
                        m$=p$
                        c=1
                  } else c++
            Next i
            if c>0 then data string$(m$, c)
            While stack.size>1 {
                  Print letter$+", ";
            }
            If not empty then Print letter$
      }
}
PrintParts "gHHH5YY++///\"

```




## Maple

Added an additional backlash to escape the \ character at the end.

```Maple
splitChange := proc(str::string)
	local start,i,len;
	start := 1;
	len := StringTools:-Length(str);
	for i from 2 to len do
		if str[i] <> str[start] then
			printf("%s, ",  str[start..i-1]);
			start := i:
		end if;
	end do;
	printf("%s", str[start..len]);
end proc;
splitChange("gHHH5YY++///\\");
```

{{out}}

```txt
g, HHH, 5, YY, ++, ///, \
```



## Mathematica


The backslash (\) must be escaped with another backslash when defining the string.

```Mathematica
StringJoin@@Riffle[StringCases["gHHH5YY++///\\", p : (x_) .. -> p], ", "]
```


{{out}}

```txt
g, HHH, 5, YY, ++, ///, \

```



## MiniScript


```MiniScript
s = "gHHH5YY++///\"
output = []
lastLetter = s[0]
for letter in s
    if letter != lastLetter then output.push ", "
    output.push letter
    lastLetter = letter
end for
print output.join("")
```

{{out}}

```txt
g, HHH, 5, YY, ++, ///, \
```


=={{header|Modula-2}}==

```modula2
MODULE CharacterChange;
FROM Terminal IMPORT Write,WriteString,WriteLn,ReadChar;

PROCEDURE Split(str : ARRAY OF CHAR);
VAR
    i : CARDINAL;
    c : CHAR;
BEGIN
    FOR i:=0 TO HIGH(str) DO
        IF i=0 THEN
            c := str[i]
        ELSIF str[i]#c THEN
            c := str[i];
            WriteLn;
        END;
        Write(c)
    END
END Split;

CONST EX = "gHHH5YY++///\";
BEGIN
    Split(EX);

    ReadChar
END CharacterChange.
```

{{out}}

```txt
g
HHH
5
YY
++
///
\
```



## Nim


{{output?}}


```nim
 proc splitOnDiff(str: string) : string =
  result = ""

  if str.len < 1: return result

  var prevChar : char = str[0]

  for idx in 0 .. < str.len:
    if str[idx] != prevChar:
      result &= ", "
      prevChar = str[idx]

    result &= str[idx]


assert splitOnDiff("""X""") == """X"""
assert splitOnDiff("""XX""") == """XX"""
assert splitOnDiff("""XY""") == """X, Y"""
assert splitOnDiff("""gHHH5YY++///\""") == """g, HHH, 5, YY, ++, ///, \"""

echo splitOnDiff("""gHHH5YY++///\""")
```



## ooRexx


```oorexx
Parse Arg str  .                                  /*obtain optional arguments from the CL*/
If str=='' Then str= 'gHHH5YY++///\'        /*Not specified?  Then use the default.*/
i=1
ol=''
Do Forever
  j=verify(str,substr(str,i,1),'N',i,99)  /* find first character that's different */
  If j=0 Then Do                          /* End of strin reached                  */
    ol=ol||substr(str,i)                  /* the final substring                   */
    Leave
    End
  ol=ol||substr(str,i,j-i)', '            /* add substring and delimiter           */
  i=j
  End
Say ol
```

{{out}}

```txt
g, HHH, 5, YY, ++, ///, \
```



## Perl

{{works with|Perl|5.x}}

```perl
my $str = 'gHHH5YY++///\\';
$str =~ s/((.)\g{-1}*)/$1, /g;
$str =~ s/, $//; # remove trailing ,
print "$str\n";

```

{{out}}

```txt

g, HHH, 5, YY, ++, ///, \

```



## Perl 6

{{works with|Rakudo|2017.05}}


```perl6
sub group-chars ($str) { $str.comb: / (.) $0* / }

# Testing:

for Q[gHHH5YY++///\], Q[fff﻿﻿﻿n⃗n⃗n⃗»»»  ℵℵ☄☄☃☃̂☃🤔🇺🇸🤦‍♂️👨‍👩‍👧‍👦] -> $string {
    put 'Original: ', $string;
    put '   Split: ', group-chars($string).join(', ');
}
```


{{out}}

```txt

Original: gHHH5YY++///\
   Split: g, HHH, 5, YY, ++, ///, \
Original: fff﻿﻿﻿n⃗n⃗n⃗»»»  ℵℵ☄☄☃☃̂☃🤔🇺🇸🤦‍♂️👨‍👩‍👧‍👦
   Split: fff, ﻿﻿﻿, n⃗n⃗n⃗, »»»,   , ℵℵ, ☄☄, ☃, ☃̂, ☃, 🤔, 🇺🇸, 🤦‍♂️, 👨‍👩‍👧‍👦

```


The second test-case is to show that Perl 6 works with strings on the Unicode grapheme level, handles whitespace, combiners, and zero width characters up to Unicode Version 9.0, and multi-byte Emoji characters up to Version 4.0 correctly. (Perl 6 provisionally handles Unicode Versions 10.0 and Emoji Version 5.0 but they aren't released yet so aren't officially supported.) For those of you with browsers unable to display the second string, it consists of:
* {LATIN SMALL LETTER F} x 3
* {ZERO WIDTH NO-BREAK SPACE} x 3
* {LATIN SMALL LETTER N, COMBINING RIGHT ARROW ABOVE} x 3
* {RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK} x 3
* {SPACE} x 2,
* {ALEF SYMBOL} x 2,
* {COMET} x 2,
* {SNOWMAN} x 1,
* {SNOWMAN, COMBINING CIRCUMFLEX ACCENT} x 1
* {SNOWMAN} x 1,
* {THINKING FACE} x 1
* {REGIONAL INDICATOR SYMBOL LETTER U, REGIONAL INDICATOR SYMBOL LETTER S} x 1
* {FACE PALM, ZERO WIDTH JOINER, MALE SIGN, VARIATION SELECTOR-16} x 1
* {MAN, ZERO WIDTH JOINER, WOMAN, ZERO WIDTH JOINER, GIRL, ZERO WIDTH JOINER, BOY} x 1


## Phix


```Phix
function split_on_change(string in)
string out = ""
    if length(in) then
        integer prev = in[1]
        for i=1 to length(in) do
            integer ch = in[i]
            if ch!=prev then
                out &= ", "
                prev = ch
            end if
            out &= ch
        end for
    end if
    return out
end function

puts(1,split_on_change(`gHHH5YY++///\`))
```

{{Out}}

```txt

g, HHH, 5, YY, ++, ///, \

```



## PowerShell

{{trans|BBC BASIC}}

```PowerShell

function Split-String ([string]$String)
{
    [string]$c = $String.Substring(0,1)
    [string]$splitString = $c

    for ($i = 1; $i -lt $String.Length; $i++)
    {
        [string]$d = $String.Substring($i,1)

        if ($d -ne $c)
        {
            $splitString += ", "
            $c = $d
        }

        $splitString += $d
    }

    $splitString
}

```


```PowerShell

Split-String "gHHH5YY++///\"

```

{{Out}}

```txt

g, HHH, 5, YY, ++, ///, \

```



## PureBasic


```purebasic
Procedure splitstring(s$)
  Define *p.Character = @s$,
         c_buf.c = *p\c
  While *p\c
    If *p\c = c_buf
      Print(Chr(c_buf))
    Else
      Print(", ")
      c_buf = *p\c
      Continue
    EndIf
    *p + SizeOf(Character)
  Wend
EndProcedure

If OpenConsole()
  splitstring("gHHH5YY++///\")
  Input()
EndIf
```

{{out}}

```txt
g, HHH, 5, YY, ++, ///, \
```



## Python



### Python3.6+

Using [[https://docs.python.org/3.6/library/itertools.html#itertools.groupby itertools.groupby]].

```python
from itertools import groupby

def splitter(text):
    return ', '.join(''.join(group) for key, group in groupby(text))

if __name__ == '__main__':
    txt = 'gHHH5YY++///\\'      # Note backslash is the Python escape char.
    print(f'Input: {txt}\nSplit: {splitter(txt)}')
```


{{out}}

```txt
Input: gHHH5YY++///\
Split: g, HHH, 5, YY, ++, ///, \
```



### Python: Using zip


```python
def splitterz(text):
    return (''.join(x + ('' if x == nxt else ', ')
            for x, nxt in zip(txt, txt[1:] + txt[-1])))

if __name__ == '__main__':
    txt = 'gHHH5YY++///\\'
    print(splitterz(txt))
```


{{out}}

```txt
g, HHH, 5, YY, ++, ///, \
```



### Python2


```python
import itertools

try: input = raw_input
except: pass

s = input()
groups = []
for _, g in itertools.groupby(s):
    groups.append(''.join(g))
print('      input string:  %s' % s)
print('     output string:  %s' % ', '.join(groups))
```

{{out}}   when using the default input:

```txt

      input string:  gHHH5YY++///\
     output string:  g, HHH, 5, YY, ++, ///, \

```



## Racket

{{trans|Python}}

```racket
#lang racket
(define (split-strings-on-change s)
  (map list->string (group-by values (string->list s) char=?)))

(displayln (string-join (split-strings-on-change #<<<
gHHH5YY++///\
<
                                                 )
                        ", "))
```


{{out}}

```txt
g, HHH, 5, YY, ++, ///, \
```



## REXX


### version 1


```rexx
/*REXX program splits a string based on change of character ───► a comma delimited list.*/
parse arg str                                    /*obtain optional arguments from the CL*/
if str==''  then str= 'gHHH5YY++///\'            /*Not specified?  Then use the default.*/
p=left(str, 1)                                   /*placeholder for the "previous" string*/
$=                                               /*     "       "   "    output      "  */
     do j=1  for length(str);  @=substr(str,j,1) /*obtain a character from the string.  */
     if @\==p  then $=$', '                      /*Not replicated char? Append delimiter*/
     p=@;           $=$ || @                     /*append a character to the  $  string.*/
     end   /*j*/                                 /* [↓]  keep peeling chars until done. */
say '          input string: '      str          /*display the original string & output.*/
say '         output string: '      $            /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default input:}}

```txt

          input string:  gHHH5YY++///\
         output string:  g, HHH, 5, YY, ++, ///, \

```



### version 2


```rexx
/* REXX */
Parse arg str                         /*obtain optional arguments from the CL*/
if str==''  then str= 'gHHH5YY++///\' /*Not specified?  Then use the default.*/
input=str
x=''
cp=''
result=''
Do While str<>''
  Parse Var str c +1 str
  If c==cp Then x=x||c
  Else Do
    If x>>'' Then
      result=result||x', '
    x=c
    End
  cp=c
  End
result=result||x
say '      input string: '    input
say '     output string: '    result
```

{{out]]

```txt
      input string:  gHHH5YY++///\
     output string:  g, HHH, 5, YY, ++, ///, \
```



## Ring


```ring

see split("gHHH5YY++///\")

func split(s )
     c =left (s, 1)
     split = ""
     for i = 1 to len(s)
         d = substr(s, i, 1)
         if d != c
            split = split + ", "
            c = d
         ok
         split = split + d
     next
     return split

```

Output:

```txt

g, HHH, 5, YY, ++, ///, \

```



## Ruby


```ruby
def split(str)
  puts " input string: #{str}"
  s = str.chars.chunk(&:itself).map{|_,a| a.join}.join(", ")
  puts "output string: #{s}"
  s
end

split("gHHH5YY++///\\")
```


{{out}}

```txt

 input string: gHHH5YY++///\
output string: g, HHH, 5, YY, ++, ///, \

```



## Rust


```Rust
fn splitter(string: &str) -> String {
    let chars: Vec<_> = string.chars().collect();
    let mut result = Vec::new();
    let mut last_mismatch = 0;
    for i in 0..chars.len() {
        if chars.len() == 1 {
            return chars[0..1].iter().collect();
        }
        if i > 0 && chars[i-1] != chars[i] {
            let temp_result: String = chars[last_mismatch..i].iter().collect();
            result.push(temp_result);
            last_mismatch = i;
        }
        if i == chars.len() - 1 {
            let temp_result: String = chars[last_mismatch..chars.len()].iter().collect();
            result.push(temp_result);
        }
    }
    result.join(", ")
}

fn main() {
    let test_string = "g";
    println!("input string: {}", test_string);
    println!("output string: {}", splitter(test_string));

    let test_string = "";
    println!("input string: {}", test_string);
    println!("output string: {}", splitter(test_string));

    let test_string = "gHHH5YY++///\\";
    println!("input string: {}", test_string);
    println!("output string: {}", splitter(test_string));
}
```


{{out}}

```txt

input string: g
output string: g
input string:
output string:
input string: gHHH5YY++///\
output string: g, HHH, 5, YY, ++, ///, \

```



## Scala


```Scala
// Split a (character) string into comma (plus a blank) delimited strings
// based on a change of character (left to right).
// See https://rosettacode.org/wiki/Split_a_character_string_based_on_change_of_character#Scala

def runLengthSplit(s: String): String = /// Add a guard letter
  (s + 'X').sliding(2).map(pair => pair.head + (if (pair.head != pair.last) ", " else "")).mkString("")

println(runLengthSplit("""gHHH5YY++///\"""))
```

{{Out}}See it in running in your browser by [https://scalafiddle.io/sf/c4dp8GT/2 ScalaFiddle (JavaScript)]
or by [https://scastie.scala-lang.org/mDoBS77YSG2Z7w5xdAPzcw Scastie (JVM)].


```Scala

def runLengthSplit(s:String):List[String] = {
  def recursiveSplit(acc:List[String], rest:String): List[String] = rest match {
    case "" => acc
    case _ => {
      val (h, t) = rest.span(_ == rest.head)
      recursiveSplit(acc :+ h, t)
    }
  }

  recursiveSplit(Nil, s)
}

val result = runLengthSplit("""gHHH5YY++///\""")
println(result.mkString(","))

```

{{Out}}

```txt

g,HHH,5,YY,++,///,\

```



## Sed


{{output?|Sed}}


```sed

echo 'gHHH5YY++///\' | sed 's/\(.\)\1*/&, /g;s/, $//'

```



## Sidef


```ruby
func group(str) {
    gather {
        while (var match = (str =~ /((.)\g{-1}*)/g)) {
            take(match[0])
        }
    }
}

say group(ARGV[0] \\ 'gHHH5YY++///\\').join(', ')
```

{{out}}

```txt

g, HHH, 5, YY, ++, ///, \

```



## Standard ML


```sml
(*
 * Head-Tail implementation of grouping
 *)
fun group'     ac      nil = [ac]
  | group'     nil (y::ys) = group' [y] ys
  | group' (x::ac) (y::ys) = if x=y then group' (y::x::ac) ys else (x::ac) :: group' [y] ys

fun group xs = group' nil xs

fun groupString str = String.concatWith ", " (map implode (group (explode str)))
```


{{out}}

```txt
- groupString "gHHH5YY++///\\";
val it = "g, HHH, 5, YY, ++, ///, \\" : string
```



## tbas

{{Trans|BBC BASIC}}

```basic
SUB SPLITUNIQUE$(s$)
	DIM c$, d$, split$, i%
	c$ = LEFT$(s$, 1)
	split$ = ""
	FOR i% = 1 TO LEN(s$)
	  d$ = MID$(s$, i%, 1)
	  IF d$ <> c$ THEN
		split$ = split$ + ", "
		c$ = d$
	  END IF
	  split$ = split$ + d$
	NEXT
	RETURN split$
END SUB

PRINT SPLITUNIQUE$("gHHH5YY++///\")
END
```



## Tcl


{{output?}}

This is most concise with regular expressions.  Note well the two steps:  it could be achieved in one very clever regexp, but being that clever is usually a bad idea (for both readability and performance, in this case).


```Tcl
set string "gHHH5YY++///\\"

regsub -all {(.)\1*} $string {\0, } string
regsub {, $} $string {} string
puts $string
```



## VBA



```vb

Option Explicit

Sub Split_string_based_on_change_character()
Dim myArr() As String, T As String

Const STRINPUT As String = "gHHH5YY++///\"
Const SEP As String = ", "

    myArr = Split_Special(STRINPUT)
    T = Join(myArr, SEP)
    Debug.Print Left(T, Len(T) - Len(SEP))
End Sub

Function Split_Special(Ch As String) As String()
'return an array of Strings
Dim tb, i&, st As String, cpt As Long, R() As String

    tb = Split(StrConv(Ch, vbUnicode), Chr(0))
    st = tb(LBound(tb))
    ReDim R(cpt)
    R(cpt) = st
    For i = 1 To UBound(tb)
        If tb(i) = st Then
            R(cpt) = R(cpt) & st
        Else
            st = tb(i)
            cpt = cpt + 1
            ReDim Preserve R(cpt)
            R(cpt) = st
        End If
    Next
    Split_Special = R
End Function

```

{{out}}

```txt
g, HHH, 5, YY, ++, ///, \
```



## XLISP


```lisp
(defun delimit (s)
	(defun delim (old-list new-list current-char)
		(if (null old-list)
			new-list
			(delim (cdr old-list) (append new-list
				(if (not (equal (car old-list) current-char))
					`(#\, #\Space ,(car old-list))
					(cons (car old-list) nil) ) )
			(car old-list) ) ) )
	(list->string (delim (string->list s) '() (car (string->list s)))) )

(display (delimit "gHHH5YY++///\\")) ;; NB. The "\" character needs to be escaped
```

{{out}}

```txt
g, HHH, 5, YY, ++, ///, \
```



## zkl


```zkl
fcn group(str){
   C,out := str[0],Sink(C);
   foreach c in (str[1,*]){ out.write(if(c==C) c else String(", ",C=c)) }
   out.close();
}
group("gHHH5YY++///\\").println();
```

{{out}}

```txt

g, HHH, 5, YY, ++, ///, \

```



## ZX Spectrum Basic


```basic
 10 LET s$="gHHH5YY++///\"
 20 LET c$=s$(1)
 30 LET n$=c$
 40 FOR i=2 TO LEN s$
 50 IF s$(i)<>c$ THEN LET n$=n$+", "
 60 LET n$=n$+s$(i)
 70 LET c$=s$(i)
 80 NEXT i
 90 PRINT n$
```

{{out}}

```txt
g, HHH, 5, YY, ++, ///, \
```

