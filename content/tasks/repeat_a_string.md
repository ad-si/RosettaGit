+++
title = "Repeat a string"
description = ""
date = 2019-10-17T04:53:48Z
aliases = []
[extra]
id = 4913
[taxonomies]
categories = ["task", "String manipulation"]
tags = []
+++

## Task

Take a string and repeat it some number of times.

Example: repeat("ha", 5)   =>   "hahahahaha"

If there is a simpler/more efficient way to repeat a single “character” (i.e. creating a string filled with a certain character), you might want to show that as well (i.e. repeat-char("*", 5) => "*****").





## 4DOS Batch


```4dos
gosub repeat ha 5
echo %@repeat[*,5]
quit

:Repeat [String Times]
    do %Times%
        echos %String%
    enddo
    echo.
return
```

Output shows:

```txt
hahahahaha
*****
```



## 8th


```forth
"ha" 5 s:*
. cr
```

Output shows:

```txt
hahahahaha
```



## ABAP

This works for ABAP Version 7.40 and above


```ABAP

report z_repeat_string.

write repeat( val = `ha`  occ = 5 ).

```


```txt

hahahahaha

```



## ActionScript

ActionScript does not have a built-in way to repeat a string multiple times, but the addition operator can be used to concatenate strings.

In Flex, there is the method [http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/mx/utils/StringUtil.html#repeat%28%29 mx.utils.StringUtil.repeat()].


### Iterative version


```ActionScript
function repeatString(string:String, numTimes:uint):String
{
	var output:String = "";
	for(var i:uint = 0; i < numTimes; i++)
		output += string;
	return output;
}
```



### Recursive version

The following double-and-add method is much faster when repeating a string many times.

```ActionScript
function repeatRecursive(string:String, numTimes:uint):String
{
	if(numTimes == 0) return "";
	if(numTimes & 1) return string + repeatRecursive(string, numTimes - 1);
	var tmp:String = repeatRecursive(string, numTimes/2);
	return tmp + tmp;
}
```



### Flex


```ActionScript
import mx.utils.StringUtil;
trace(StringUtil.repeat("ha", 5));

```

Sample Output:

```txt

hahahahaha

```



## Ada

In [[Ada]] multiplication of an universal integer to string gives the desired result. Here is an example of use:

```Ada
with Ada.Strings.Fixed;  use Ada.Strings.Fixed;
with Ada.Text_IO;        use Ada.Text_IO;

procedure String_Multiplication is
begin
   Put_Line (5 * "ha");
end String_Multiplication;
```

Sample output:

```txt

hahahahaha

```



## Aime


```aime
call_n(5, o_text, "ha");
```



## ALGOL 68


```algol68
print (5 * "ha")

```



## APL

Fill up a string of length 10 with 'ha':

```apl
      10⍴'ha'
hahahahaha
```

Alternatively, define a function:

```apl
      REPEAT←{(⍺×⍴⍵)⍴⍵}
      5 REPEAT 'ha'
hahahahaha
```



## AppleScript


```AppleScript
set str to "ha"
set final_string to ""
repeat 5 times
    set final_string to final_string & str
end repeat
```



For larger numbers of repetitions, however, it proves significantly faster to progressively double a copy of the original string (concatenating it with itself). Intermediate stages of doubling are appended to an accumulator wherever required for binary composition of the target number.

See the technique of 'Egyptian Multiplication' described in the Rhind Mathematical Papyrus at the British Museum.

```AppleScript
replicate(5000, "ha")

-- Repetition by 'Egyptian multiplication' -
-- progressively doubling a list, appending
-- stages of doubling to an accumulator where needed for
-- binary assembly of a target length.

-- replicate :: Int -> String -> String
on replicate(n, s)
    set out to ""
    if n < 1 then return out
    set dbl to s

    repeat while (n > 1)
        if (n mod 2) > 0 then set out to out & dbl
        set n to (n div 2)
        set dbl to (dbl & dbl)
    end repeat
    return out & dbl
end replicate
```



## Applesoft BASIC


```ApplesoftBasic
FOR I = 1 TO 5 : S$ = S$ + "HA" : NEXT

? "X" SPC(20) "X"
```

Output:

```txt
X                    X
```



## ATS


```ATS

//
// How to compile:
// patscc -DATS_MEMALLOC_LIBC -o string_repeat string_repeat.dats
//

#include
"share/atspre_staload.hats"

fun
string_repeat
(
  x: string, n: intGte(0)
) : Strptr1 = res where
{
  val xs =
    list_make_elt<string>(n, x)
  val res = stringlst_concat($UNSAFE.list_vt2t(xs))
  val ((*freed*)) = list_vt_free(xs)
} (* end of [string_repeat] *)

(* ****** ****** *)

implement
main0 () = let
//
val ha5 = string_repeat("ha", 5)
val ((*void*)) = println! ("ha5 = \"", ha5, "\"")
val ((*freed*)) = strptr_free (ha5)
//
in
  // nothing
end // end of [main0]

```



## AutoHotkey


```AutoHotkey
MsgBox % Repeat("ha",5)

Repeat(String,Times)
{
  Loop, %Times%
    Output .= String
  Return Output
}
```



## AutoIt


```AutoIt>#include <String.au3


ConsoleWrite(_StringRepeat("ha", 5) & @CRLF)
```



## AWK


```awk
function repeat( str, n,    rep, i )
{
    for( ; i<n; i++ )
        rep = rep str
    return rep
}

BEGIN {
    print repeat( "ha", 5 )
}
```



## Babel


```babel
main: { "ha" 5 print_repeat }

print_repeat!: { <- { dup << } -> times }
```

Outputs:

```babel>hahahahaha</lang

The '<<' operator prints, 'dup' duplicates the top-of-stack, 'times' does something x number of times. The arrows mean down (<-) and up (->) respectively - it would require a lengthy description to explain what this means, refer to the doc/babel_ref.txt file in the github repo linked from [[Babel]]


## Batch File

Commandline implementation

```dos
@echo off
if "%2" equ "" goto fail
setlocal enabledelayedexpansion
set char=%1
set num=%2
for /l %%i in (1,1,%num%) do set res=!res!%char%
echo %res%
:fail
```


'Function' version

```dos
@echo off
set /p a=Enter string to repeat :
set /p b=Enter how many times to repeat :
set "c=1"
set "d=%b%"
:a
echo %a%
set "c=%c%+=1"
if /i _"%c%"==_"%d%" (exit /b)
goto :a
```


'Function' version 2

```dos
@echo off
@FOR /L %%i in (0,1,9) DO @CALL :REPEAT %%i
@echo That's it!
@FOR /L %%i in (0,1,9) DO @CALL :REPEAT %%i
@echo.
@echo And that!
@GOTO END

:REPEAT
@echo|set /p="*"
@GOTO:EOF

:END
```



## BaCon

To repeat a string:

```qbasic
DOTIMES 5
    s$ = s$ & "ha"
DONE
PRINT s$
```

```txt

hahahahaha

```

To repeat one single character:

```qbasic
PRINT FILL$(5, ASC("x"))
```

```txt

xxxxx

```



## BBC BASIC


```bbcbasic
      PRINT STRING$(5, "ha")
```




## beeswax


```beeswax
          p    <
      p0~1<}~< d@<
_VT@1~>yg~9PKd@M'd;
```



Example:


```txt
julia> beeswax("repeat a string.bswx")
sha
i5
hahahahaha
Program finished!
```

<code>s</code> tells the user that the program expects a string as input.
<code>i</code> tells the user that the program expects an integer as input.


## Befunge


```Befunge>v
                ">:#,_v
>29*+00p>~:"0"-    #v_v $
 v      ^p0p00:-1g00< $            >
 v    p00&p0-1g00+4*65< >00g1-:00p#^_@
```

Input sample:

```txt
ha05
```

Input string has to be zero terminated and less than 18 characters.

Output sample:

```txt
hahahahaha
```



## Bracmat

The code almost explains itself. The repetions are accumulated in a list <code>rep</code>. The <code>str</code> concatenates all elements into a single string, ignoring the white spaces separating the elements.


```bracmat
(repeat=
  string N rep
.   !arg:(?string.?N)
  & !string:?rep
  &   whl
    ' (!N+-1:>0:?N&!string !rep:?rep)
  & str$!rep
);
```



```txt
 repeat$(ha.5)
 hahahahaha
```


=={{header|Brainfuck}}==
Prints "ha" 10 times.  Note that this method only works for a number of repetitions that fit into the cell size.

```bf
+++++ +++++	init first as 10 counter
[-> +++++ +++++<] we add 10 to second each loopround

		Now we want to loop 5 times to follow std
+++++
[-> ++++ . ----- -- . +++<] print h and a each loop

and a newline because I'm kind and it looks good
+++++ +++++ +++ . --- .
```



## Brat


```brat
p "ha" * 5  #Prints "hahahahaha"
```



## Burlesque


```burlesque

blsq ) 'h5?*
"hhhhh"
blsq ) "ha"5.*\[
"hahahahaha"

```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char * string_repeat( int n, const char * s ) {
  size_t slen = strlen(s);
  char * dest = malloc(n*slen+1);

  int i; char * p;
  for ( i=0, p = dest; i < n; ++i, p += slen ) {
    memcpy(p, s, slen);
  }
  *p = '\0';
  return dest;
}

int main() {
  char * result = string_repeat(5, "ha");
  puts(result);
  free(result);
  return 0;
}
```

A variation.

```c
...
char *string_repeat(const char *str, int n)
{
   char *pa, *pb;
   size_t slen = strlen(str);
   char *dest = malloc(n*slen+1);

   pa = dest + (n-1)*slen;
   strcpy(pa, str);
   pb = --pa + slen;
   while (pa>=dest) *pa-- = *pb--;
   return dest;
}
```


To repeat a single character

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char * char_repeat( int n, char c ) {
  char * dest = malloc(n+1);
  memset(dest, c, n);
  dest[n] = '\0';
  return dest;
}

int main() {
  char * result = char_repeat(5, '*');
  puts(result);
  free(result);
  return 0;
}
```


If you use [[GLib]], simply use <code>g_strnfill ( gsize length, gchar fill_char )</code> function.


## C#


```c#
string s = "".PadLeft(5, 'X').Replace("X", "ha");
```

or (with .NET 2+)

```c#
string s = new String('X', 5).Replace("X", "ha");
```

or (with .NET 2+)

```c#
string s = String.Join("ha", new string[5 + 1]);
```

or (with .NET 4+)

```c#
string s = String.Concat(Enumerable.Repeat("ha", 5));
```


To repeat a single character:

```c#
string s = "".PadLeft(5, '*');
```

or (with .NET 2+)

```c#
string s = new String('*', 5);
```



## C++


```cpp
#include <string>
#include <iostream>

std::string repeat( const std::string &word, int times ) {
   std::string result ;
   result.reserve(times*word.length()); // avoid repeated reallocation
   for ( int a = 0 ; a < times ; a++ )
      result += word ;
   return result ;
}

int main( ) {
   std::cout << repeat( "Ha" , 5 ) << std::endl ;
   return 0 ;
}
```


To repeat a single character:

```cpp
#include <string>
#include <iostream>

int main( ) {
   std::cout << std::string( 5, '*' ) << std::endl ;
   return 0 ;
}
```



## Ceylon


```ceylon
shared void repeatAString() {
	print("ha".repeat(5));
}
```



## Clipper

Also works with Harbour Project compiler Harbour 3.0.0 (Rev. 16951)

```visualfoxpro
   Replicate( "Ha", 5 )
```



## Clojure


```lisp
(apply str (repeat 5 "ha"))
```



## COBOL

Virtually a one-liner.

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. REPEAT-PROGRAM.
DATA DIVISION.
WORKING-STORAGE SECTION.
77  HAHA         PIC A(10).
PROCEDURE DIVISION.
    MOVE ALL 'ha' TO HAHA.
    DISPLAY HAHA.
    STOP RUN.
```

```txt
hahahahaha
```



## ColdFusion


```cfm

<cfset word = 'ha'>
<Cfset n = 5>
<Cfoutput>
<Cfloop from="1" to="#n#" index="i">#word#</Cfloop>
</Cfoutput>

```



## Common Lisp


```lisp
(defun repeat-string (n string)
  (with-output-to-string (stream)
    (loop repeat n do (write-string string stream))))
```


A version which allocates the result string in one step:


```lisp
(defun repeat-string (n string
                     &aux
                     (len (length string))
                     (result (make-string (* n len)
                                          :element-type (array-element-type string))))
  (loop repeat n
        for i from 0 by len
        do (setf (subseq result i (+ i len)) string))
  result)
```



For those who love one-liners, even at the expense of readability:

```lisp
(defun repeat-string (n string)
  (format nil "~V@{~a~:*~}" n string))
```




```lisp
(princ (repeat-string 5 "hi"))
```



A single character may be repeated using just the builtin <code>make-string</code>:

```lisp
(make-string 5 :initial-element #\X)
```

produces “XXXXX”.


## Crystal


```Ruby

puts "ha" * 5

```



```Bash

hahahahaha

```



## D

Repeating a string:

```d
import std.stdio, std.array;

void main() {
    writeln("ha".replicate(5));
}
```

Repeating a character with vector operations:

```d
import std.stdio;

void main() {
    char[] chars;     // create the dynamic array
    chars.length = 5; // set the length
    chars[] = '*';    // set all characters in the string to '*'
    writeln(chars);
}
```



## DCL

Not exactly what the task asks for but at least it is something;

```DCL
$ write sys$output f$fao( "!AS!-!AS!-!AS!-!AS!-!AS", "ha" )
$ write sys$output f$fao( "!12*d" )
```

```txt
$ @repeat_a_string_and_then_character
hahahahaha
dddddddddddd
```



## Delphi

Repeat a string

```Delphi

function RepeatString(const s: string; count: cardinal): string;
var
  i: Integer;
begin
  for i := 1 to count do
    Result := Result + s;
end;

Writeln(RepeatString('ha',5));

```


Repeat a character


```Delphi

Writeln( StringOfChar('a',5) );

```


Using recursion


```Delphi

function RepeatStr(const s: string; i: Cardinal): string;
begin
  if i = 0 then
    result := ''
  else
   result := s + RepeatStr(s, i-1)
end;

```


Built in RTL function:


```Delphi>StrUtils.DupeString</lang


=={{header|Déjà Vu}}==

```dejavu
!. concat( rep 5 "ha" )
```

```txt
"hahahahaha"
```



## DWScript

Repeat a string


```Delphi

PrintLn( StringOfString('abc',5) );

```


Repeat a character


```Delphi

PrintLn( StringOfChar('a',5) );

```



## Dyalect


```dyalect
String(values: Array.empty(5, '*'))
```



## E


```e
"ha" * 5
```



## ECL

After version 4.2.2
<lang>IMPORT STD; //Imports the Standard Library

STRING MyBaseString := 'abc';
RepeatedString := STD.Str.Repeat(MyBaseString,3);
RepeatedString;  //returns 'abcabcabc'
```


Before version 4.2.2
<lang>RepeatString(STRING InStr, INTEGER Cnt) := FUNCTION
  rec := {STRING Str};
  ds  := DATASET(Cnt,TRANSFORM(rec,SELF.Str := InStr));
  res := ITERATE(ds,TRANSFORM(rec,SELF.Str := LEFT.Str + RIGHT.Str));
  RETURN Res[Cnt].Str;
END;

RepeatString('ha',3);
RepeatString('Who',2);
```



## Egison


```egison

(S.concat (take 5 (repeat1 "ha")))

```



## Eiffel


```eiffel

 repeat_string(a_string: STRING; times: INTEGER): STRING
 require
   times_positive: times > 0
 do
   Result := a_string.multiply(times)
 end

```


## Elena

ELENA 4.x :

```elena
import system'routines;
import extensions;
import extensions'text;

public program()
{
    var s := new Range(0, 5).selectBy:(x => "ha").summarize(new StringWriter())
}
```



## Elixir


```elixir

String.duplicate("ha", 5)

```



## Emacs Lisp

Going via a list to repeat the desired string,


```lisp
(apply 'concat (make-list 5 "ha"))
```


A single character can be repeated with <code>make-string</code>


```lisp
(make-string 5 ?x)
```


With <code>cl.el</code> the loop macro can repeat and concatenate,


```lisp
(require 'cl)
(loop repeat 5 concat "ha")
```



## Erlang


```erlang
repeat(X,N) ->
    lists:flatten(lists:duplicate(N,X)).
```

This will duplicate a string or character N times to produce a new string.


## ERRE


```ERRE

PROCEDURE REPEAT_STRING(S$,N%->REP$)
   LOCAL I%
   REP$=""
   FOR I%=1 TO N% DO
       REP$=REP$+S$
   END FOR
END PROCEDURE

```

Note: If N% is less than 1, the result is the empty string "".If S$ is a one-character string
you can use the predefined function <code>STRING$</code> as <code>REP$=STRING$(S$,N%)</code>.


## Euphoria


```Euphoria
function repeat_string(object x, integer times)
    sequence out
    if atom(x) then
        return repeat(x,times)
    else
        out = ""
        for n = 1 to times do
            out &= x
        end for
        return out
    end if
end function

puts(1,repeat_string("ha",5) & '\n') -- hahahahaha

puts(1,repeat_string('*',5) & '\n') -- *****
```

Sample Output:

```txt

hahahahaha
*****
```



```Euphoria
-- Here is an alternative method for "Repeat a string"
include std/sequence.e
printf(1,"Here is the repeated string: %s\n", {repeat_pattern("ha",5)})
printf(1,"Here is another: %s\n", {repeat_pattern("*",5)})

```

Sample Output:

```txt

Here is the repeated string: hahahahaha
Here is another: *****
```


=={{header|F_Sharp|F#}}==

```fsharp>
 String.replicate 5 "ha";;
val it : string = "hahahahaha"
```

Or

```fsharp>
 String.Concat( Array.create 5 "ha" );;
val it : string = "hahahahaha"
```



## Factor


```factor
: repeat-string ( str n -- str' ) swap <repetition> concat ;

"ha" 5 repeat-string print
```



## Forth


```forth
: place-n { src len dest n -- }
  0 dest c!
  n 0 ?do src len dest +place loop ;

s" ha" pad 5 place-n
pad count type    \ hahahahaha
```

The same code without the use of locals:

```forth

: place-n ( src len dest n -- )
  swap >r 0 r@ c!
  begin dup while -rot 2dup r@ +place rot 1- repeat
  r> 2drop 2drop ;

s" ha" pad 5 place-n
pad count type    \ hahahahaha
```

Filling a string with a single character is supported by ANS-Forth:

```forth
pad 10 char * fill   \ repeat a single character
pad 10 type    \ **********
```



## Fortran

```fortran
program test_repeat

  write (*, '(a)') repeat ('ha', 5)

end program test_repeat
```

Output:
 hahahahaha


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' A character is essentially a string of length 1 in FB though there is a built-in function, String,
' which creates a string by repeating a character a given number of times.

' To avoid repeated concatenation (a slow operation) when the string to be repeated has a length
' greater than one, we instead create a buffer of the required size and then fill that.

Function repeat(s As String, n As Integer) As String
  If n < 1 Then Return ""
  If n = 1 Then Return s
  Var size = Len(s)
  If size = 0 Then Return s  ' empty string
  If size = 1 Then Return String(n, s[0])  ' repeated single character
  Var buffer = Space(size * n)  'create buffer for size > 1
  For i As Integer = 0 To n - 1
    For j As Integer = 0 To size - 1
      buffer[i * size + j] = s[j]
    Next j
  Next i
  Return buffer
End Function

Print repeat("rosetta", 1)
Print repeat("ha", 5)
Print repeat("*", 5)
Print
Print "Press any key to quit program"
Sleep
```


```txt

rosetta
hahahahaha
*****

```



## Free Pascal


```pascal
strUtils.dupeString('ha', 5)
```

Repetition of a single character:

```pascal
stringOfChar('*', 5)
```

If the repeated character happens to be the space character:

```pascal
space(5)
```



## Frink


```frink

println[repeat["ha", 5]]

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=bdd2a7a0f4f09ff67e9d78b5d9667bdb Click this link to run this code]'''
<lang>Public Sub Main()

Print String$(5, "ha")

End
```


Output = hahahahaha


## GAP


```gap
Concatenation(ListWithIdenticalEntries(10, "BOB "));
"BOB BOB BOB BOB BOB BOB BOB BOB BOB BOB "
```



## Glee


```glee
'*' %% 5
```



```glee
'ha' => Str;
Str# => Len;
1..Len %% (Len * 5) => Idx;
Str [Idx] $;
```



```glee
'ha'=>S[1..(S#)%%(S# *5)]
```



## Go


```go
fmt.Println(strings.Repeat("ha", 5))        // ==> "hahahahaha"
```

There is no special way to repeat a single character, other than to convert the character to a string.  The following works:

```go
fmt.Println(strings.Repeat(string('h'), 5)) // prints hhhhh
```



## Groovy


```groovy
 println 'ha' * 5
```



## Harbour


```visualfoxpro
? Replicate( "Ha", 5 )
```



## Haskell

For a string of finite length:

```haskell
concat $ replicate 5 "ha"
```


Or with list-monad (a bit obscure):

```haskell
[1..5] >> "ha"
```


Or with Control.Applicative:

```haskell
[1..5] *> "ha"
```


For an infinitely long string:

```haskell
cycle "ha"
```


To repeat a single character:

```haskell
replicate 5 '*'
```


Or, unpacking the mechanism of '''replicate''' a little, and using a '''mappend'''-based rep in lieu of the '''cons'''-based '''repeat''', so that we can skip a subsequent '''concat''':

```haskell
repString :: String -> Int -> String
repString s n =
  let rep x = xs
        where
          xs = mappend x xs
  in take (n * length s) (rep s)

main :: IO ()
main = print $ repString "ha" 5
```

```txt
"hahahahaha"
```


As the number of repetitions grows, however, it may become more efficient to repeat by progressive duplication (mappend to self), mappending to an accumulator only where required for binary composition of the target length. (i.e. Rhind Papyrus 'Egyptian' or 'Ethiopian' multiplication):


```haskell
import Data.Tuple (swap)
import Data.List (unfoldr)
import Control.Monad (join)

-- BY RHIND PAPYRUS 'EGYPTIAN' OR 'ETHIOPIAN' MULTIPLICATION ------------------
repString :: Int -> String -> String
repString n s =
  foldr
    (\(d, x) a ->
        if d > 0 -- Is this power of 2 needed for the binary recomposition ?
          then mappend a x
          else a)
    mempty $
  zip
    (unfoldr
       (\h ->
           if h > 0
             then Just $ swap (quotRem h 2) -- Binary decomposition of n
             else Nothing)
       n)
    (iterate (join mappend) s) -- Iterative duplication ( mappend to self )

-- TEST -----------------------------------------------------------------------
main :: IO ()
main = print $ repString 500 "ha"
```



## HicEst


```HicEst
CHARACTER out*20

EDIT(Text=out, Insert="ha", DO=5)
```


=={{header|Icon}} and {{header|Unicon}}==
The procedure <tt>repl</tt> is a supplied function in Icon and Unicon.

```Icon
procedure main(args)
    write(repl(integer(!args) | 5))
end
```

If it weren't, one way to write it is:

```Icon
procedure repl(s, n)
    every (ns := "") ||:= |s\(0 <= n)
    return ns
end
```



## Idris


```Idris
strRepeat : Nat -> String -> String
strRepeat Z s = ""
strRepeat (S n) s = s ++ strRepeat n s

chrRepeat : Nat -> Char -> String
chrRepeat Z c = ""
chrRepeat (S n) c = strCons c $ chrRepeat n c
```



## Inform 7


```inform7
Home is a room.

To decide which indexed text is (T - indexed text) repeated (N - number) times:
	let temp be indexed text;
	repeat with M running from 1 to N:
		let temp be "[temp][T]";
	decide on temp.

When play begins:
	say "ha" repeated 5 times;
	end the story.
```


=={{header|IS-BASIC}}==
<lang IS-BASIC> 10 PRINT STRING$("ha",5)
100 DEF STRING$(S$,N)
105   LET ST$=""
110   FOR I=1 TO N
120     LET ST$=ST$&S$
130   NEXT
140   LET STRING$=ST$
150 END DEF
```



## J


```j
   5 # '*'               NB. repeat each item 5 times
*****
   5 # 'ha'              NB. repeat each item 5 times
hhhhhaaaaa
   5 ((* #) $ ]) 'ha'    NB. repeat array 5 times
hahahahaha
   5 ;@# < 'ha'          NB. boxing is used to treat the array as a whole
hahahahaha
```



## Java

There's no method or operator to do this in Java, so you have to do it yourself.


```java5
public static String repeat(String str, int times) {
    StringBuilder sb = new StringBuilder(str.length() * times);
    for (int i = 0; i < times; i++)
        sb.append(str);
    return sb.toString();
}

public static void main(String[] args) {
    System.out.println(repeat("ha", 5));
}
```


Or even shorter:


```java5
public static String repeat(String str, int times) {
   return new String(new char[times]).replace("\0", str);
}
```


In Apache Commons Lang, there is a [http://commons.apache.org/lang/api-2.6/org/apache/commons/lang/StringUtils.html#repeat%28java.lang.String,%20int%29 StringUtils.repeat()] method.


## JavaScript


### =Extending the String prototype=

This solution creates an empty array of length n+1, then uses the array's join method to effectively concatenate the string n times. Note that extending the prototype of built-in objects is not a good idea if the code is to run in a shared workspace.

```javascript
String.prototype.repeat = function(n) {
    return new Array(1 + (n || 0)).join(this);
}

console.log("ha".repeat(5));  // hahahahaha
```


As of ES6, `repeat` is built in, so this can be written as:


```javascript

console.log("ha".repeat(5));  // hahahahaha
```



### =Repetition by Egyptian multiplication=

For larger numbers of repetitions, however, it proves significantly faster to progressively double a copy of the original string (concatenating it with itself). Intermediate stages of doubling are appended to an accumulator wherever required for binary composition of the target number.

See the technique of 'Egyptian Multiplication' described in the Rhind Mathematical Papyrus at the British Museum.


```javascript
(() => {
    'use strict';

    // replicate :: Int -> String -> String
    const replicate = (n, s) => {
        let v = [s],
            o = [];
        if (n < 1) return o;
        while (n > 1) {
            if (n & 1) o = o + v;
            n >>= 1;
            v = v + v;
        }
        return o.concat(v);
    };


    return replicate(5000, "ha")
})();
```



### =Concat . replicate=

Or, more generically, we could derive '''repeat''' as the composition of '''concat''' and '''replicate'''

```JavaScript
(() => {
    'use strict';

    // repeat :: Int -> String -> String
    const repeat = (n, s) =>
        concat(replicate(n, s));


    // GENERIC FUNCTIONS ------------------------------------------------------

    // concat :: [[a]] -> [a] | [String] -> String
    const concat = xs =>
        xs.length > 0 ? (() => {
            const unit = typeof xs[0] === 'string' ? '' : [];
            return unit.concat.apply(unit, xs);
        })() : [];

    // replicate :: Int -> a -> [a]
    const replicate = (n, x) =>
        Array.from({
            length: n
        }, () => x);


    // TEST -------------------------------------------------------------------
    return repeat(5, 'ha');
})();
```

```txt
hahahahaha
```



## jq


```jq
"a " * 3' # => "a a a "
```


Note that if the integer multiplicand is 0, then the result is null.


## Julia

```julia
@show "ha" ^ 5

# The ^ operator is really just call to the `repeat` function
@show repeat("ha", 5)
```



## K



```k

  ,/5#,"ha"
"hahahahaha"

  5#"*"
"*****"

```



## Kotlin


```scala
fun main(args: Array<String>) {
    println("ha".repeat(5))
}
```

Or more fancy:

```scala
operator fun String.times(n: Int) = this.repeat(n)

fun main(args: Array<String>) = println("ha" * 5)
```



## LabVIEW

I don't know if there is a built-in function for this, but it is easily achieved with a For loop and Concatenate Strings.<br/>
[[file:LabVIEW_Repeat_a_string.png]]


## Lasso


```Lasso
'ha'*5 // hahahahaha
```



```Lasso
loop(5) => {^ 'ha' ^} // hahahahaha
```



## LFE


```lisp

(string:copies '"ha" 5)

```



## Liberty BASIC


```lb
a$ ="ha "
print StringRepeat$( a$, 5)

end

function StringRepeat$( in$, n)
    o$ =""
    for i =1 to n
        o$ =o$ +in$
    next i
    StringRepeat$ =o$
end function
```



## Lingo

*Take a string and repeat it some number of times.

```lingo
on rep (str, n)
  res = ""
  repeat with i = 1 to n
    put str after res
  end repeat
  return res
end
```


```lingo
put rep("ha", 5)
-- "hahahahaha"
```

*If there is a simpler/more efficient way to repeat a single “character”...

```lingo
put bytearray(5, chartonum("*")).readRawString(5)
-- "*****"
```



## LiveCode


```liveCode
on mouseUp
    put repeatString("ha", 5)
end mouseUp

function repeatString str n
    repeat n times
        put str after t
    end repeat
    return t
end repeatString
```



## Logo


```logo
to copies :n :thing [:acc "||]
  if :n = 0 [output :acc]
  output (copies :n-1 :thing combine :acc :thing)
end
```

or using cascade:

```logo
show cascade 5 [combine "ha ?] "||    ; hahahahaha
```


Lhogho doesn't have cascade (yet), nor does it have the initialise a missing parameter capability demonstrated by the [:acc "||] above.


```logo
to copies :n :thing :acc
  if :n = 0 [output :acc]
  output (copies :n-1 :thing combine :acc :thing)
end

print copies 5 "ha "||
```



## Lua


```lua
function repeats(s, n) return n > 0 and s .. repeats(s, n-1) or "" end
```


Or use native string library function

```lua
string.rep(s,n)
```



## Maple

There are many ways to do this in Maple.  First, the "right" (most efficient) way is to use the supplied procedures for this purpose.

```Maple

> use StringTools in
>       Repeat( "abc", 10 ); # repeat an arbitrary string
>       Fill( "x", 20 )      # repeat a character
> end use;
                    "abcabcabcabcabcabcabcabcabcabc"

                         "xxxxxxxxxxxxxxxxxxxx"

```

These next two are essentially the same, but are less efficient (though still linear) because they create a sequence of 10 strings before concatenating them (with the built-in procedure cat) to form the result.

```Maple

> cat( "abc" $ 10 );
                    "abcabcabcabcabcabcabcabcabcabc"

> cat( seq( "abc", i = 1 .. 10 ) );
                    "abcabcabcabcabcabcabcabcabcabc"

```

You ''can'' build up a string in a loop, but this is highly inefficient (quadratic); don't do this.

```Maple

> s := "":
> to 10 do s := cat( s, "abc" ) end: s;
                    "abcabcabcabcabcabcabcabcabcabc"

```

If you need to build up a string incrementally, use a StringBuffer object, which keeps things linear.

Finally, note that strings and characters are not distinct datatypes in Maple; a character is just a string of length one.


## Mathematica


```Mathematica
(* solution 1 *)
rep[n_Integer,s_String]:=Apply[StringJoin,ConstantArray[s,{n}]]

(* solution 2 -- @@ is the infix form of Apply[] *)
rep[n_Integer,s_String]:=StringJoin@@Table[s,{n}]

(* solution 3 -- demonstrating another of the large number of looping constructs available *)
rep[n_Integer,s_String]:=Nest[StringJoin[s, #] &,s,n-1]
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function S = repeat(s , n)
    S = repmat(s , [1,n]) ;
return
```


Note 1: The repetition is returned, not displayed.


Note 2: To repeat a string, use single quotes. Example: S=repeat('ha',5)


## Maxima


```maxima
"$*"(s, n) := apply(sconcat, makelist(s, n))$
infix("$*")$

"abc" $* 5;
/* "abcabcabcabcabc" */
```



## Mercury

Mercury's 'string' module provides an efficient char-repeater.  The following uses string.builder to repeat strings.


```Mercury
:- module repeat.
:- interface.
:- import_module string, char, int.

:- func repeat_char(char, int) = string.
:- func repeat(string, int) = string.

:- implementation.
:- import_module stream, stream.string_writer, string.builder.

repeat_char(C, N) = string.duplicate_char(C, N).

repeat(String, Count) = Repeated :-
        S0 = string.builder.init,
        Repeated = string.builder.to_string(S),
        printn(string.builder.handle, Count, String, S0, S).

:- pred printn(Stream, int, string, State, State)
               <= (stream.writer(Stream, string, State),
                   stream.writer(Stream, character, State)).
:- mode printn(in, in, in, di, uo) is det.
printn(Stream, N, String, !S) :-
        ( N > 0 ->
                print(Stream, String, !S),
                printn(Stream, N - 1, String, !S)
        ; true ).
```



## min

```min
"ha" 5 repeat print
```

```txt

hahahahaha

```



## MiniScript


```MiniScript
str = "Lol"
print str * 5
```

```txt

LolLolLolLolLol

```



## Mirah


```mirah
x = StringBuilder.new

5.times do
    x.append "ha"
end

puts x # ==> "hahahahaha"
```



## Monte


```Monte

var s := "ha " * 5
traceln(s)

```



## MontiLang


```MontiLang
|ha| 5 * PRINT .
```

Or with a loop

```MontiLang
FOR 5
    |ha| OUT .
ENDFOR || PRINT .
```


Or ...


```MontiLang
|ha| FOR 5 OUT ENDFOR . || PRINT .
```



## MUMPS


```MUMPS
RPTSTR(S,N)
 ;Repeat a string S for N times
 NEW I
 FOR I=1:1:N WRITE S
 KILL I
 QUIT
RPTSTR1(S,N) ;Functionally equivalent, but denser to read
 F I=1:1:N W S
 Q

```



This last example uses the [http://docs.intersystems.com/cache20121/csp/docbook/DocBook.UI.Page.cls?KEY=RCOS_fpiece#RCOS_B57001 $PIECE] function.

```MUMPS

 ;Even better (more terse)
 S x="",$P(x,"-",10)="-"
 W x

```



## Neko


```actionscript
/* Repeat a string, in Neko */
var srep = function(s, n) {
    var str = ""
    while n > 0 {
        str += s
        n -= 1
    }
    return str
}

$print(srep("ha", 5), "\n")
```


```txt
prompt$ nekoc srep.neko
prompt$ neko srep
hahahahaha
```



## Nemerle

Any of the methods shown in the [[Repeat_a_string#C.23|C#]] solution would also work for Nemerle, but they're all semantically awkward. This example uses an extension method to wrap one of the awkward techniques in order to clarify the semantics (which is also possible in C#, there's nothing really Nemerle specific here except the syntax).

```Nemerle
using System;
using System.Console;

module StrRep
{
    Repeat(this s : string, n : int) : string
    {
        String('x', n).Replace("x", s)
    }

    Main() : void
    {
        WriteLine("ha".Repeat(5));
        WriteLine("*".Repeat(5));
        WriteLine(String('*', 5)); // repeating single char

    }
}
```



## NetRexx

NetRexx has built in functions to manipulate strings.  The most appropriate for this task is the <code>'''copies()'''</code> function:

```NetRexx
/* NetRexx */

ha5 = 'ha'.copies(5)

```


There are several other built-in functions that can be used to achieve the same result depending on need:


```NetRexx
/* NetRexx */
sampleStr = 'ha' -- string to duplicate
say '   COPIES:' sampleStr.copies(5)
say 'CHANGESTR:' '.....'.changestr('.', sampleStr)

sampleChr = '*' -- character to duplicate
say '     LEFT:' sampleChr.left(5, sampleChr)
say '    RIGHT:' sampleChr.right(5, sampleChr)
say '   CENTRE:' sampleChr.centre(5, sampleChr)
say '  OVERLAY:' sampleChr.overlay(sampleChr, 1, 5, sampleChr)
say '   SUBSTR:' ''.substr(1, 5, sampleChr)
say 'TRANSLATE:' '.....'.translate(sampleChr, '.')

```



## NewLISP


```NewLISP
(dup "ha" 5)
```



## Nim


```nim

import strutils
repeat("ha", 5)

```



## Objeck


```objeck
bundle Default {
  class Repeat {
    function : Main(args : String[]) ~ Nil {
      Repeat("ha", 5)->PrintLine();
    }

    function : Repeat(string : String, max : Int) ~ String {
      repeat : String := String->New();
      for(i := 0; i < max; i += 1;) {
        repeat->Append(string);
      };

      return repeat;
    }
  }
}
```


=={{header|Objective-C}}==
Objective-C allows developers to extend existing an existing class by adding additional methods to the class without needing to subclass. These extensions are called categories. Category methods are available to all instances of the class, as well as any instances of its subclasses.

This task provides us with an opportunity to visit this aspect of the language feature.

We will extend NSString, the de facto Objective-C string class in environments that are either compatible with or descend directly from the OPENSTEP specification, such as GNUstep and Mac OS X, respectively, with a method that accomplishes the described task.


```objc
@interface NSString (RosettaCodeAddition)
- (NSString *) repeatStringByNumberOfTimes: (NSUInteger) times;
@end

@implementation NSString (RosettaCodeAddition)
- (NSString *) repeatStringByNumberOfTimes: (NSUInteger) times {
    return [@"" stringByPaddingToLength:[self length]*times withString:self startingAtIndex:0];
}
@end
```


Now, let's put it to use:

```objc
    // Instantiate an NSString by sending an NSString literal our new
    // -repeatByNumberOfTimes: selector.
    NSString *aString = [@"ha" repeatStringByNumberOfTimes:5];

    // Display the NSString.
    NSLog(@"%@", aString);
```



## OCaml

Since Ocaml 4.02 strings are immutable, as is convenient for a functional language. Mutable strings are now implemented in the module Bytes.

```ocaml
let string_repeat s n =
  let len = Bytes.length s in
  let res = Bytes.create(n * len) in
  for i = 0 to pred n do
    Bytes.blit s 0 res (i * len) len
  done;
  Bytes.to_string res (* not stricly necessary, the bytes type is equivalent to string except mutability *)
;;
```

which gives the signature
```ocaml
 val string_repeat : bytes -> int -> string = <fun>
```


testing in the toplevel:

```ocaml
# string_repeat "Hiuoa" 3 ;;
- : string = "HiuoaHiuoaHiuoa"
```


Alternately create an array initialized to s, and concat:

```ocaml
let string_repeat s n =
  String.concat "" (Array.to_list (Array.make n s))
;;
```


Or:

```ocaml
let string_repeat s n =
  Array.fold_left (^) "" (Array.make n s)
;;
```


To repeat a single character use:

```ocaml
String.make 5 '*'
```



## Oforth


```Oforth
StringBuffer new "abcd" <<n(5)
```



## OpenEdge/Progress

<lang Progress (OpenEdge ABL)>MESSAGE FILL( "ha", 5 ) VIEW-AS ALERT-BOX.
```



## OxygenBasic


```oxygenbasic


'REPEATING A CHARACTER

print string 10,"A" 'result AAAAAAAAAA

'REPEATING A STRING

function RepeatString(string s,sys n) as string
  sys i, le=len s
  if le=0 then exit function
  n*=le
  function=nuls n
  '
  for i=1 to n step le
    mid function,i,s
  next
end function

print RepeatString "ABC",3 'result ABCABCABC

```



## Oz

We have to write a function for this:

```oz
declare
  fun {Repeat Xs N}
     if N > 0 then
        {Append Xs {Repeat Xs N-1}}
     else
        nil
     end
  end
in
  {System.showInfo {Repeat "Ha" 5}}
```



## PARI/GP


### Version #1. Based on recursion.

This solution is recursive and unimaginably bad.  Slightly less bad versions can be designed, but that's not the point: don't use GP for text processing if you can avoid it.  If you really need to, it's easy to create an efficient function in PARI (see [[#C|C]]) and pass that to GP.

```parigp
repeat(s,n)={
  if(n, Str(repeat(s, n-1), s), "")
};
```


<code>concat()</code> joins together a vector of strings, in this case a single string repeated.

```parigp
repeat(s,n)=concat(vector(n,i, s));
```


This solution is recursive and slightly less bad than the others for large n.

```parigp
repeat(s,n)={
  if(n<4, return(concat(vector(n,i, s))));
  if(n%2,
    Str(repeat(Str(s,s),n\2),s)
  ,
    repeat(Str(s,s),n\2)
  );
}
```



### Version #2. Simple loop based.

Basic set of string functions is very handy for presentation purposes. At the same time, it is true that PARI/GP is not an appropriate tool
for the heavy text processing.


```parigp

\\ Repeat a string str the specified number of times ntimes and return composed string.
\\ 3/3/2016 aev
srepeat(str,ntimes)={
my(srez=str,nt=ntimes-1);
if(ntimes<1||#str==0,return(""));
if(ntimes==1,return(str));
for(i=1,nt, srez=concat(srez,str));
return(srez);
}

{
\\ TESTS
print(" *** Testing srepeat:");
print("1.",srepeat("a",5));
print("2.",srepeat("ab",5));
print("3.",srepeat("c",1));
print("4.|",srepeat("d",0),"|");
print("5.|",srepeat("",5),"|");
print1("6."); for(i=1,10000000, srepeat("e",10));
}

```


```txt

 *** Testing srepeat:
1.aaaaa
2.ababababab
3.c
4.||
5.||
6.
(16:00) gp > ##
  ***   last result computed in 1min, 2,939 ms.

```



## Pascal

See [[#Delphi|Delphi]] or [[#Free Pascal|Free Pascal]], as standard Pascal does not know strings of unlimited length.


## Perl


```perl
"ha" x 5
```



## Perl 6


```perl6
print "ha" x 5
```

(Note that the <code>x</code> operator isn't quite the same as in Perl 5: it now only creates strings. To create lists, use <code>xx</code>.)


## Phix


```Phix
?repeat('*',5)
?join(repeat("ha",5),"")
```

```txt

"*****"
"hahahahaha"

```



## PHP


```php
str_repeat("ha", 5)
```



## PicoLisp


```PicoLisp
(pack (need 5 "ha"))
-> "hahahahaha"
```

or:

```PicoLisp
(pack (make (do 5 (link "ha"))))
-> "hahahahaha"
```



## Pike


```pike
"ha"*5;
```



## PL/I


```PL/I

/* To repeat a string a variable number of times: */

s = repeat('ha', 4);

  /* or */

s = copy('ha', 5);

/* To repeat a single character a fixed number of times: */

s = (5)'h';     /* asigns 'hhhhh' to s. */

```



## Plorth


```plorth
"ha" 5 *
```



## PostScript


```PostScript
% the comments show the stack content after the line was executed
% where rcount is the repeat count, "o" is for orignal,
% "f" is for final, and iter is the for loop variable
%
% usage: rcount ostring times -> fstring

/times {
  dup length dup    % rcount ostring olength olength
  4 3 roll          % ostring olength olength rcount
  mul dup string    % ostring olength flength fstring
  4 1 roll          % fstring ostring olength flength
  1 sub 0 3 1 roll  % fstring ostring 0 olength flength_minus_one
  {                 % fstring ostring iter
    1 index 3 index % fstring ostring iter ostring fstring
    3 1 roll        % fstring ostring fstring iter ostring
    putinterval     % fstring ostring
  } for
  pop               % fstring
} def
```



## PowerBASIC


```powerbasic
MSGBOX REPEAT$(5, "ha")
```



## PowerShell


```powershell
"ha" * 5  # ==> "hahahahaha"
```



## Processing


```processing
void setup() {
  String rep = repeat("ha", 5);
  println(rep);
}
String repeat(String str, int times) {
  // make an array of n chars,
  // replace each char with str,
  // and return as a new String
  return new String(new char[times]).replace("\0", str);
}
```



## Prolog


```prolog
%repeat(Str,Num,Res).
repeat(Str,1,Str).
repeat(Str,Num,Res):-
    Num1 is Num-1,
    repeat(Str,Num1,Res1),
    string_concat(Str, Res1, Res).
```



## Pure

str_repeat is defined by pattern-matching: repeating any string 0 times results in the empty string; while
repeating it more than 0 times results in the concatenation of the string and (n-1) further repeats.


```pure>
 str_repeat 0 s = "";
> str_repeat n s = s + (str_repeat (n-1) s) if n>0;
> str_repeat 5 "ha";
"hahahahaha"
>
```


You can define str_repeat using infinite lazy list (stream).


```pure

str_repeat n::int s::string = string $ take n $ cycle (s:[]);

```



## PureBasic


```PureBasic
Procedure.s RepeatString(count, text$=" ")
   Protected i, ret$=""

   For i = 1 To count
      ret$ + text$
   Next
   ProcedureReturn ret$
EndProcedure

Debug RepeatString(5, "ha")
```



## Python


```python
"ha" * 5  # ==> "hahahahaha"
```

"Characters" are just strings of length one.

the other way also works:

```python
5 * "ha"  # ==> "hahahahaha"
```



## R


```ruby
strrep("ha", 5)
```



## Racket


```racket

#lang racket
;; fast
(define (string-repeat n str)
  (string-append* (make-list n str)))
(string-repeat 5 "ha") ; => "hahahahaha"

```


To repeat a single character:

```racket

(make-string 5 #\*) => "*****"

```



## RapidQ


```vb

'For a single char
showmessage String$(10, "-")

'For strings with more than one char
function Repeat$(Expr as string, Count as integer) as string
    dim x as integer
    for x = 1 to Count
        Result = Result + Expr
    next
end function

showmessage Repeat$("ha", 5)

```



## REALbasic


```vb
Function Repeat(s As String, count As Integer) As String
  Dim output As String
  For i As Integer = 0 To count
    output = output + s
  Next
  Return output
End Function

```



## REBOL


```rebol
head insert/dup "" "ha" 5
```



## Red


```Red>>
 str: "Add duplicates to string"
>> insert/dup str "ha" 3
== "hahahaAdd duplicates to string"
>> insert/dup tail str "ha" 3
== "hahahaAdd duplicates to stringhahaha"
```



## Retro


```Retro
with strings'
: repeatString ( $n-$ )
  1- [ dup ] dip [ over prepend ] times nip ;

"ha" 5 repeatString
```



## REXX

Since the REXX language only supports the "character" type, it's not surprising that there are so many ways to skin a cat.

```REXX
/*REXX program to show various ways to repeat a string (or repeat a single char).*/

/*all examples are equivalent, but not created equal.*/

                           /*───────────────────────────────────────────*/
y='ha'
z=copies(y,5)
                           /*───────────────────────────────────────────*/
z=copies( 'ha', 5 )
                           /*───────────────────────────────────────────*/
y='ha'
z=y||y||y||y||y
                           /*───────────────────────────────────────────*/
y='ha'
z=y || y || y || y || y    /*same as previous, but the "big sky" version*/
                           /*───────────────────────────────────────────*/
y='ha'
z=''
       do 5
       z=z||y
       end
                           /*───────────────────────────────────────────*/
y="ha"
z=
       do 5
       z=z||y
       end
                           /*───────────────────────────────────────────*/
y="ha"
z=
       do i=101 to 105
       z=z||y
       end

                           /*───────────────────────────────────────────*/
y='+'
z=left('',5,y)
                           /*───────────────────────────────────────────*/
y='+'
z=right('',5,y)
                           /*───────────────────────────────────────────*/
y='+'
z=substr('',1,5,y)
                           /*───────────────────────────────────────────*/
y='+'
z=center('',5,y)
                           /*───────────────────────────────────────────*/
y='+'
z=centre('',5,y)
                           /*───────────────────────────────────────────*/
y='+'
z=space('',5,y)
                           /*───────────────────────────────────────────*/
y='+'
z=translate('@@@@@',y,"@")
                           /*───────────────────────────────────────────*/
y='abcdef'
z=five(y)
exit

five: procedure expose y; parse arg g
if length(g)>=5*length(y) then return g
return five(y||g)
                           /*───────────────────────────────────────────*/
y='something wicked this way comes.'
z=y||y||y||y||y||y||y||y||y||y||y||y|\y||y||y
z=left(z,5*length(y))
                           /*───────────────────────────────────────────*/
y='+'
z=copies('',5,y)
                           /*───────────────────────────────────────────*/
y='+'
z=lower('',1,5,y)
                           /*───────────────────────────────────────────*/
y='+'
z=lower('',,5,y)
                           /*───────────────────────────────────────────*/
z='+'
z=upper('',1,5,y)
                           /*───────────────────────────────────────────*/
z=upper('',,5,y)
                           /*───────────────────────────────────────────*/

y='charter bus.'
z='*****'
z=changestr('*',z,y)
                           /*───────────────────────────────────────────*/
y='what the hey!'
z=
  do until length(z)==5*length(y)
  z=z||y
  end
                           /*───────────────────────────────────────────*/
y='what the hey!'
z=
  do until length(z)==5*length(y)
  z=insert(z,0,y)
  end
                           /*───────────────────────────────────────────*/
y='yippie ki yay'
z=
   do i=1 by 5 for 5
   z=overlay(y,z,i)
   end
                           /*───────────────────────────────────────────*/
y='+'
z=justify('',5,y)
                           /*───────────────────────────────────────────*/
whatever_this_variable_is_____it_aint_referenced_directly= 'boy oh boy.'
z=; signal me; me:
  do 5
  z=z||strip(subword(sourceline(sigl-1),2),,"'")
  end
                           /*───────────────────────────────────────────*/
y="any more examples & the angry townfolk with pitchforks will burn the castle."
parse value y||y||y||y||y with z

exit                                   /*stick a fork in it, we're done.*/
```

Some older REXXes don't have a '''changestr''' bif, so one is included here ──► [[CHANGESTR.REX]].





## Ring


```ring
 Copy("ha" , 5)  # ==> "hahahahaha"
```




## Ruby


```ruby
"ha" * 5  # ==> "hahahahaha"
```



## Run BASIC


```runbasic
a$ = "ha "
for i = 1 to 5
  a1$ = a1$ + a$
next i
a$ = a1$
print a$
```



## Rust


```rust
std::iter::repeat("ha").take(5).collect::<String>(); // ==> "hahahahaha"
```


Since 1.16:

```rust
"ha".repeat(5); // ==> "hahahahaha"
```



## Scala


```scala
"ha" * 5 // ==> "hahahahaha"
```



## Scheme


```scheme
(define (string-repeat n str)
  (apply string-append (vector->list (make-vector n str))))
```

with SRFI 1:

```scheme
(define (string-repeat n str)
	(fold string-append "" (make-list n str)))
(string-repeat 5 "ha") ==> "hahahahaha"
```


To repeat a single character:

```scheme
(make-string 5 #\*)
```



## Scratch

This example requires making variables named "String", "Count", and "Repeated" first.

[[File:Scratch_Repeat_a_String.png]]


## sed

Number of ampersands indicates number of repetitions.

```sed

$ echo ha | sed 's/.*/&&&&&/'
hahahahaha

```


## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  begin
    writeln("ha" mult 5);
  end func;
```


Output:

```txt

hahahahaha

```



## Sidef


```ruby
'ha' * 5;  # ==> 'hahahahaha'
```



## Sinclair ZX81 BASIC

Works with 1k of RAM. This program defines a subroutine that expects to find a string and a number of times to repeat it; but all it then does is loop and concatenate, so making it a separate subroutine is arguably overkill.

```basic
 10 LET S$="HA"
 20 LET N=5
 30 GOSUB 60
 40 PRINT T$
 50 STOP
 60 LET T$=""
 70 FOR I=1 TO N
 80 LET T$=T$+S$
 90 NEXT I
100 RETURN
```



## Smalltalk


If n is a small constant, then simply concatenating n times will do; for example, n=5::

```smalltalk
v := 'ha'.
v,v,v,v,v
```


By creating a collection of n 'ha', and joining them to a string:


```smalltalk
((1 to: n) collect: [:x | 'ha']) joinUsing: ''.
```

or:{{works with|Smalltalk/X}}
```smalltalk
(Array new:n withAll:'ha') asStringWith:''.
```

By creating a WriteStream, and putting N times the string 'ha' into it:


```smalltalk
ws := '' writeStream.
n timesRepeat: [ws nextPutAll: 'ha'].
ws contents.
```

alternatively:

```smalltalk
(String streamContents:[:ws | n timesRepeat: [ws nextPutAll: 'ha']])
```


all evaluate to:

```txt

hahahahaha

```


A string containing a repeated character is generated with:

```smalltalk
String new:n withAll:$*
```


```smalltalk
(String new:n) atAllPut:$*
```



## SNOBOL4


```snobol4
	output = dupl("ha",5)
end
```



## Sparkling


```sparkling>spn:3
 repeat("na", 8) .. " Batman!"
= nananananananana Batman!
```



## SQL


```sql
select rpad('', 10, 'ha')
```



## SQL PL

```sql pl

VALUES REPEAT('ha', 5);
VALUES RPAD('', 10, 'ha');

```

Output:

```txt

db2 -t
db2 => VALUES REPEAT('ha', 5);

1
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
hahahahaha

  1 record(s) selected.

db2 => VALUES RPAD('', 10, 'ha');

1
----------
hahahahaha

  1 record(s) selected.

```



## Standard ML


```sml
fun string_repeat (s, n) =
  concat (List.tabulate (n, fn _ => s))
;
```


testing in the interpreter:

```sml
- string_repeat ("Hiuoa", 3) ;
val it = "HiuoaHiuoaHiuoa" : string
```


To repeat a single character:

```sml
fun char_repeat (c, n) =
  implode (List.tabulate (n, fn _ => c))
;
```



## Stata


```stata
. scalar a="ha"
. scalar b=a*5
. display b
hahahahaha
```



## Suneido


```Suneido
'ha'.Repeat(5)  -->  "hahahahaha"
'*'.Repeat(5)  -->  "*****"
```



## Swift

Using extensions to do the repetition which makes for an easier syntax when repeating Strings, and using String.extend() to get faster evaluation.


```swift
extension String {
  // Slower version
  func repeatString(n: Int) -> String {
    return Array(count: n, repeatedValue: self).joinWithSeparator("")
  }

  // Faster version
  // benchmarked with a 1000 characters and 100 repeats the fast version is approx 500 000 times faster :-)
  func repeatString2(n:Int) -> String {
    var result = self
    for _ in 1 ..< n {
      result.appendContentsOf(self)   // Note that String.appendContentsOf is up to 10 times faster than "result += self"
    }
    return result
  }
}

print( "ha".repeatString(5) )
print( "he".repeatString2(5) )
```

```txt

hahahahaha
hehehehehe

```


To repeat a single character:

```swift
String(count:5, repeatedValue:"*" as Character)

```


Note that using the String version on a string of 1 Character, or the repeat single Character version is timewise close to the same. No point in using the Character version for efficiency (tested with repeating up to 100 000 times).


###  Bitwise Iterative Version

The following version is an enhanced version of the [http://rosettacode.org/mw/index.php?title=Repeat_a_string#Recursive_version recursive ActionScript], where we're using bit operation along with iterative doubling of the string to get to the correctly repeated version of the text in the most effective manner without recursion.  When benchmarked against the plain iterative version in previous section, this version is marginally better, but only my a very small percentage. The critical factor for making the repeat function effective when using larger strings (1000 characters) and multiple repeats (1000 repeats :-) ) was to to exchange the '+=' with 'String.extend' method.


```swift
extension String {
  func repeatBiterative(count: Int) -> String {
        var reduceCount = count
        var result = ""
        var doubled = self
        while reduceCount != 0 {
            if reduceCount & 1 == 1 {
                result.appendContentsOf(doubled)
            }
            reduceCount >>= 1
            if reduceCount != 0 {
                doubled.appendContentsOf(doubled)
            }
        }
        return result
    }
}

"He".repeatBiterative(5)
```

```txt

"HeHeHeHeHe"

```



## Tailspin


```tailspin

'$:1..5 -> 'ha';' -> !OUT::write

```

```txt
hahahahaha
```



## Tcl


```tcl
string repeat "ha" 5  ;# => hahahahaha
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
repeatstring=REPEAT ("ha",5)

```



## TorqueScript

--[[User:Eepos|Eepos]]

```TorqueScript
function strRep(%str,%int)
{
	for(%i = 0; %i < %int; %i++)
	{
		%rstr = %rstr@%str;
	}

	return %rstr;
}
```

=={{header|Transact-SQL}}==

```tsql
select REPLICATE( 'ha', 5 )
```



## UNIX Shell


### Using printf

```bash
printf "ha"%.0s {1..5}
```


With ksh93 and zsh, the count can vary.

```bash
i=5
printf "ha"%.0s {1..$i}
```


With bash, <code>{1..$i}</code> fails, because brace expansion happens before variable substitution. The fix uses <code>eval</code>.

```bash
i=5
eval "printf 'ha'%.0s {1..$i}"
```


For the general case, one must escape any % or \ characters in the string, because <code>printf</code> would interpret those characters.

```bash
reprint() {
  typeset e="$(sed -e 's,%,%%,g' -e 's,\\,\\\\,g' <<<"$1")"
  eval 'printf "$e"%.0s '"{1..$2}"
}
reprint '%  ha  \' 5
```



###  Using repeat

```bash

len=12; str='='
repeat $len printf "$str"

```


===Using head -c===
<code>head -c</code> is a [[GNU]] extension, so it only works with those systems. (Also, this script can only repeat a single character.)

```sh
width=72; char='='
head -c ${width} < /dev/zero | tr '\0' "$char"
```



## Ursala


```Ursala
#import nat

repeat = ^|DlSL/~& iota

#cast %s

example = repeat('ha',5)
```

output:

```txt
'hahahahaha'
```



## Vala

Repeat a string 5 times:

```vala

string s = "ha";
string copy = "";
for (int x = 0; x < 5; x++)
	copy += s;

```


Fill a string with a char N times:

```vala

string s = string.nfill(5, 'c');

```



## VBA


'''Repeat a string'''


```VBA
Public Function RepeatStr(aString As String, aNumber As Integer) As String
	Dim bString As String, i As Integer
	bString = ""
	For i = 1 To aNumber
		bString = bString & aString
	Next i
	RepeatStr = bString
End Function

Debug.Print RepeatStr("ha", 5)
```

```txt

hahahahaha

```

''Note:'' "String(5, "ha") in VBA produces "hhhhh" (only the first character is repeated)!
<p>An alternative method:

```vba
Public Function RepeatString(stText As String, iQty As Integer) As String
  RepeatString = Replace(String(iQty, "x"), "x", stText)
End Function
```


'''Repeat a character'''


```VBA
Debug.Print String(5, "x")
```

```txt
xxxxx
```



## VBScript

```VBScript

' VBScript has a String() function that can repeat a character a given number of times
' but this only works with single characters (or the 1st char of a string):
WScript.Echo String(10, "123")	' Displays "1111111111"

' To repeat a string of chars, you can use either of the following "hacks"...
WScript.Echo Replace(Space(10), " ", "Ha")
WScript.Echo Replace(String(10, "X"), "X", "Ha")

```



## Vedit macro language


```vedit
Ins_Text("ha", COUNT, 5)
```



## Visual Basic

'''Repeat a string'''


```vb
Public Function StrRepeat(s As String, n As Integer) As String
	Dim r As String, i As Integer
	r = ""
	For i = 1 To n
		r = r & s
	Next i
	StrRepeat = r
End Function

Debug.Print StrRepeat("ha", 5)
```

```txt
hahahahaha
```


An alternative method:

```vb
Public Function StrRepeat(sText As String, n As Integer) As String
	StrRepeat = Replace(String(n, "*"), "*", sText)
End Function
```



'''Repeat a character'''


```VBA
Debug.Print String(5, "x")
```

```txt
xxxxx
```



## Visual Basic .NET


'''Repeat a string'''


```vb

Debug.Print(Replace(Space(5), " ", "Ha"))

```

```txt

HaHaHaHaHa

```



'''Repeat a character'''


```vb

Debug.Print(StrDup(5, "x"))
Debug.Print("".PadRight(5, "x"))
Debug.Print("".PadLeft(5, "x"))

```

```txt

xxxxx
xxxxx
xxxxx

```



## Visual FoxPro

Use the built in function REPLICATE(string, number):

```vfp
? REPLICATE("HO", 3)
```


produces

```txt
HOHOHO
```



## Wart


```wart
def (s * n) :case (string? s)
  with outstring
    repeat n
      pr s

("ha" * 5)
=> "hahahahaha"
```



## Wortel


```wortel
@join "" @rep 5 "ha" ; returns "hahahahaha"
```

As a function:

```wortel
^(\@join "" @rep)
```



## XPL0


```XPL0
cod T=12; int I; for I gets 1,5 do T(0,"ha")
```

```txt
hahahahaha
```



## Yorick


```yorick
array("ha", 5)(sum)
```



## zig

At compile-time:

```zig
const laugh = "ha" ** 5;
```

Note that to achieve this at run-time in zig (in order to avoid hidden overflows) you must manage the memory yourself.

```zig
const std = @import("std");
const warn = std.debug.warn;

const Allocator = std.mem.Allocator;

fn repeat(s: []const u8, times: u16, allocator: *Allocator) ![]u8 {
    const repeated = try allocator.alloc(u8, s.len*times);

    var i: usize = 0;
    while (i < s.len*times) : (i += 1) {
        repeated[i] = s[i % 2];
    }

    return repeated;
}

pub fn main() !void {
    const allocator = std.debug.global_allocator;
    const ex = try repeat("ha", 5, allocator);
    defer allocator.free(ex);
}
```



## zkl

Same as [[#Ruby|Ruby]]

```zkl
"ha" * 5  # --> "hahahahaha"
```




