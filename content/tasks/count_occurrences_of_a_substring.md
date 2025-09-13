+++
title = "Count occurrences of a substring"
description = ""
date = 2019-10-18T05:05:24Z
aliases = []
[extra]
id = 9932
[taxonomies]
categories = ["task", "String manipulation"]
tags = []
+++

## Task

Create a function,   or show a built-in function,   to count the number of non-overlapping occurrences of a substring inside a string.

The function should take two arguments:
:::*   the first argument being the string to search,   and
:::*   the second a substring to be searched for.


It should return an integer count.

```pseudocode
print countSubstring("the three truths","th")
3

// do not count substrings that overlap with previously-counted substrings:
print countSubstring("ababababab","abab")
2
```


The matching should yield the highest number of non-overlapping matches.

In general, this essentially means matching from left-to-right or right-to-left   (see proof on talk page).


## 11l


```11l
print(‘the three truths’.count(‘th’))
print(‘ababababab’.count(‘abab’))
```


```txt

3
2

```



## 360 Assembly

The program uses two ASSIST macros (XDECO,XPRNT) to keep the code as short as possible.

```360asm
*        Count occurrences of a substring  05/07/2016
COUNTSTR CSECT
         USING  COUNTSTR,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         "
         ST     R15,8(R13)         "
         LR     R13,R15            "
         MVC    HAYSTACK,=CL32'the three truths'
         MVC    LENH,=F'17'        lh=17
         MVC    NEEDLE,=CL8'th'    needle='th'
         MVC    LENN,=F'2'         ln=2
         BAL    R14,SHOW           call show
         MVC    HAYSTACK,=CL32'ababababab'
         MVC    LENH,=F'11'        lh=11
         MVC    NEEDLE,=CL8'abab'  needle='abab'
         MVC    LENN,=F'4'         ln=4
         BAL    R14,SHOW           call show
         L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    "
         XR     R15,R15            "
         BR     R14                exit
HAYSTACK DS     CL32               haystack
NEEDLE   DS     CL8                needle
LENH     DS     F                  length(haystack)
LENN     DS     F                  length(needle)
*------- ----   show---------------------------------------------------
SHOW     ST     R14,SAVESHOW       save return address
         BAL    R14,COUNT          count(haystack,needle)
         LR     R11,R0             ic=count(haystack,needle)
         MVC    PG(20),HAYSTACK    output haystack
         MVC    PG+20(5),NEEDLE    output needle
         XDECO  R11,PG+25          output ic
         XPRNT  PG,80              print buffer
         L      R14,SAVESHOW       restore return address
         BR     R14                return to caller
SAVESHOW DS     A                  return address of caller
PG       DC     CL80' '            buffer
*------- ----   count--------------------------------------------------
COUNT    ST     R14,SAVECOUN       save return address
         SR     R7,R7              n=0
         LA     R6,1               istart=1
         L      R10,LENH           lh
         S      R10,LENN           ln
         LA     R10,1(R10)         lh-ln+1
LOOPI    CR     R6,R10             do istart=1 to lh-ln+1
         BH     ELOOPI
         LA     R8,NEEDLE          @needle
         L      R9,LENN            ln
         LA     R4,HAYSTACK-1      @haystack[0]
         AR     R4,R6              +istart
         LR     R5,R9              ln
         CLCL   R4,R8              if substr(haystack,istart,ln)=needle
         BNE    NOTEQ
         LA     R7,1(R7)           n=n+1
         A      R6,LENN            istart=istart+ln
NOTEQ    LA     R6,1(R6)           istart=istart+1
         B      LOOPI
ELOOPI   LR     R0,R7              return(n)
         L      R14,SAVECOUN       restore return address
         BR     R14                return to caller
SAVECOUN DS     A                  return address of caller
*        ----   -------------------------------------------------------
         YREGS
         END    COUNTSTR
```

```txt

the three truths    th              3
ababababab          abab            2

```



## Ada


```Ada
with Ada.Strings.Fixed, Ada.Integer_Text_IO;

procedure Substrings is
begin
   Ada.Integer_Text_IO.Put (Ada.Strings.Fixed.Count (Source  => "the three truths",
                                                     Pattern => "th"));
   Ada.Integer_Text_IO.Put (Ada.Strings.Fixed.Count (Source  => "ababababab",
                                                     Pattern => "abab"));
end Substrings;
```


```txt
          3          2
```



## ALGOL 68

Algol68 has no build in function to do this task, hence the next to create a ''count string in string'' routine.

```algol68
#!/usr/local/bin/a68g --script #

PROC count string in string = (STRING needle, haystack)INT: (
  INT start:=LWB haystack, next, out:=0;
  FOR count WHILE string in string(needle, next, haystack[start:]) DO
    start+:=next+UPB needle-LWB needle;
    out:=count
  OD;
  out
);

printf(($d" "$,
  count string in string("th", "the three truths"),    # expect 3 #
  count string in string("abab", "ababababab"),        # expect 2 #
  count string in string("a*b", "abaabba*bbaba*bbab"), # expect 2 #
  $l$
))
```



```txt

3 2 2

```



## Apex

Apex example for 'Count occurrences of a substring'.

```Apex

String substr = 'ABC';
String str = 'ABCZZZABCYABCABCXXABC';
Integer substrLen = substr.length();
Integer count = 0;
Integer index = str.indexOf(substr);
while (index >= 0) {
    count++;
    str = str.substring(index+substrLen);
    index = str.indexOf(substr);
}
System.debug('Count String : '+count);

```



```txt

Count String : 5

```







## AppleScript


This is a good example of the kind of problem to which standard libraries (a regex library in this case) would offer most languages a simple and immediate solution.
AppleScript, however, for want of various basics like regex and math library functions, can require scripters to draw on the supplementary resources of Bash, using the built-in ''do shell script'' function.

A slightly faster approach to seeking outside library help has, however, become possible since OS X 10.10 added JavaScript as an osalang sibling for AppleScript. Rather than using the resources of Bash, we can more quickly use AppleScript's built in ObjC interface to pass a subproblem over to the more richly endowed JavaScript core of JavaScript for Automation.

Here we use a generic ''evalOSA(language, code)'' function to apply a JavaScript for Automation regex to a pair of AppleScript strings, using OSAKit.


```AppleScript
use framework "OSAKit"

on run
    {countSubstring("the three truths", "th"), ¬
        countSubstring("ababababab", "abab")}
end run

on countSubstring(str, subStr)
    return evalOSA("JavaScript", "var matches = '" & str & "'" & ¬
        ".match(new RegExp('" & subStr & "', 'g'));" & ¬
        "matches ? matches.length : 0") as integer
end countSubstring

-- evalOSA :: ("JavaScript" | "AppleScript") -> String -> String
on evalOSA(strLang, strCode)

    set ca to current application
    set oScript to ca's OSAScript's alloc's initWithSource:strCode ¬
        |language|:(ca's OSALanguage's languageForName:(strLang))

    set {blnCompiled, oError} to oScript's compileAndReturnError:(reference)

    if blnCompiled then
        set {oDesc, oError} to oScript's executeAndReturnError:(reference)
        if (oError is missing value) then return oDesc's stringValue as text
    end if

    return oError's NSLocalizedDescription as text
end evalOSA
```


```txt

{3, 2}

```



## AutoHotkey

While it is simple enough to parse the string,
AutoHotkey has a rather unconventional method which outperforms this.
StringReplace sets the number of replaced strings to ErrorLevel.

```AutoHotkey
MsgBox % countSubstring("the three truths","th") ; 3
MsgBox % countSubstring("ababababab","abab")     ; 2

CountSubstring(fullstring, substring){
   StringReplace, junk, fullstring, %substring%, , UseErrorLevel
   return errorlevel
}
```



## AWK



```AWK
#
# countsubstring(string, pattern)
#   Returns number of occurrences of pattern in string
#   Pattern treated as a literal string (regex characters not expanded)
#
function countsubstring(str, pat,    len, i, c) {
  c = 0
  if( ! (len = length(pat) ) )
    return 0
  while(i = index(str, pat)) {
    str = substr(str, i + len)
    c++
  }
  return c
}
#
# countsubstring_regex(string, regex_pattern)
#   Returns number of occurrences of pattern in string
#   Pattern treated as regex
#
function countsubstring_regex(str, pat,    c) {
  c = 0
  c += gsub(pat, "", str)
  return c
}
BEGIN {
  print countsubstring("[do&d~run?d!run&>run&]", "run&")
  print countsubstring_regex("[do&d~run?d!run&>run&]", "run[&]")
  print countsubstring("the three truths","th")
}
```

```txt
$ awk -f countsubstring.awk
2
2
3

```



## BaCon


```qbasic
FUNCTION Uniq_Tally(text$, part$)
    LOCAL x
    WHILE TALLY(text$, part$)
        INCR x
        text$ = MID$(text$, INSTR(text$, part$)+LEN(part$))
    WEND
    RETURN x
END FUNCTION

PRINT "the three truths - th: ", Uniq_Tally("the three truths", "th")
PRINT "ababababab - abab: ", Uniq_Tally("ababababab", "abab")
```

```txt

the three truths - th: 3
ababababab - abab: 2

```



## BASIC

In FreeBASIC, this needs to be compiled with <code>-lang qb</code> or <code>-lang fblite</code>.


```qbasic
DECLARE FUNCTION countSubstring& (where AS STRING, what AS STRING)

PRINT "the three truths, th:", countSubstring&("the three truths", "th")
PRINT "ababababab, abab:", countSubstring&("ababababab", "abab")

FUNCTION countSubstring& (where AS STRING, what AS STRING)
    DIM c AS LONG, s AS LONG
    s = 1 - LEN(what)
    DO
        s = INSTR(s + LEN(what), where, what)
        IF 0 = s THEN EXIT DO
        c = c + 1
    LOOP
    countSubstring = c
END FUNCTION
```


 the three truths, th:        3
 ababababab, abab:            2

See also: [[#Liberty BASIC|Liberty BASIC]], [[#PowerBASIC|PowerBASIC]], [[#PureBasic|PureBasic]].
=
## Applesoft BASIC
=

```ApplesoftBasic
10 F$ = "TH"
20 S$ = "THE THREE TRUTHS"
30 GOSUB 100"COUNT SUBSTRING
40 PRINT R
50 F$ = "ABAB"
60 S$ = "ABABABABAB"
70 GOSUB 100"COUNT SUBSTRING
80 PRINT R
90 END

100 R = 0
110 F = LEN(F$)
120 S = LEN(S$)
130 IF F > S THEN RETURN
140 IF F = 0 THEN RETURN
150 IF F = S AND F$ = S$ THEN R = 1 : RETURN
160 FOR I = 1 TO S - F
170     IF F$ = MID$(S$, I, F) THEN R = R + 1 : I = I + F - 1
180 NEXT I
190 RETURN
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 INPUT PROMPT "String:    ":TXT$
110 INPUT PROMPT "Substring: ":SUB$
120 PRINT COUNT(LCASE$(TXT$),LCASE$(SUB$))
130 DEF COUNT(TXT$,SUB$)
140   LET N=0:LET PO=1
150   DO
160     LET PO=POS(TXT$,SUB$,PO)
170     IF PO THEN LET N=N+1:LET PO=PO+LEN(SUB$)
180   LOOP UNTIL PO=0
190   LET COUNT=N
200 END DEF
```


=
## Sinclair ZX81 BASIC
=
Works with 1k of RAM.

```basic
 10 LET S$="THE THREE TRUTHS"
 20 LET U$="TH"
 30 GOSUB 100
 40 PRINT N
 50 LET S$="ABABABABAB"
 60 LET U$="ABAB"
 70 GOSUB 100
 80 PRINT N
 90 STOP
100 LET N=0
110 LET I=0
120 LET I=I+1
130 IF I+LEN U$>LEN S$ THEN RETURN
140 IF S$(I TO I+LEN U$-1)<>U$ THEN GOTO 120
150 LET N=N+1
160 LET I=I+LEN U$
170 GOTO 130
```



## Batch File


```dos
@echo off
setlocal enabledelayedexpansion

	::Main
call :countString "the three truths","th"
call :countString "ababababab","abab"
pause>nul
exit /b
	::/Main

	::Procedure
:countString
	set input=%~1
	set cnt=0

	:count_loop
	set trimmed=!input:*%~2=!
	if "!trimmed!"=="!input!" (echo.!cnt!&goto :EOF)
	set input=!trimmed!
	set /a cnt+=1
	goto count_loop
```

```txt
3
2
```


## BBC BASIC


```bbcbasic
      tst$ = "the three truths"
      sub$ = "th"
      PRINT ; FNcountSubstring(tst$, sub$) " """ sub$ """ in """ tst$ """"
      tst$ = "ababababab"
      sub$ = "abab"
      PRINT ; FNcountSubstring(tst$, sub$) " """ sub$ """ in """ tst$ """"
      END

      DEF FNcountSubstring(A$, B$)
      LOCAL I%, N%
      I% = 1 : N% = 0
      REPEAT
        I% = INSTR(A$, B$, I%)
        IF I% THEN N% += 1 : I% += LEN(B$)
      UNTIL I% = 0
      = N%

```

```txt
3 "th" in "the three truths"
2 "abab" in "ababababab"
```



## Bracmat


```bracmat
  ( count-substring
  =   n S s p
    .     0:?n:?p
        & !arg:(?S.?s)
        & @( !S
           :   ?
               ( [!p ? !s [?p ?
               & !n+1:?n
               & ~
               )
           )
      | !n
  )
& out$(count-substring$("the three truths".th))
& out$(count-substring$(ababababab.abab))
& ;
```

```txt
3
2
```



## C


```c
#include <stdio.h>
#include <string.h>

int match(const char *s, const char *p, int overlap)
{
        int c = 0, l = strlen(p);

        while (*s != '\0') {
                if (strncmp(s++, p, l)) continue;
                if (!overlap) s += l - 1;
                c++;
        }
        return c;
}

int main()
{
        printf("%d\n", match("the three truths", "th", 0));
        printf("overlap:%d\n", match("abababababa", "aba", 1));
        printf("not:    %d\n", match("abababababa", "aba", 0));
        return 0;
}
```


Alternate version:

```c
#include <stdio.h>
#include <string.h>

// returns count of non-overlapping occurrences of 'sub' in 'str'
int countSubstring(const char *str, const char *sub)
{
    int length = strlen(sub);
    if (length == 0) return 0;
    int count = 0;
    for (str = strstr(str, sub); str; str = strstr(str + length, sub))
        ++count;
    return count;
}

int main()
{
    printf("%d\n", countSubstring("the three truths", "th"));
    printf("%d\n", countSubstring("ababababab", "abab"));
    printf("%d\n", countSubstring("abaabba*bbaba*bbab", "a*b"));

    return 0;
}
```

```txt

3
2
2

```



## C++


```cpp
#include <iostream>
#include <string>

// returns count of non-overlapping occurrences of 'sub' in 'str'
int countSubstring(const std::string& str, const std::string& sub)
{
    if (sub.length() == 0) return 0;
    int count = 0;
    for (size_t offset = str.find(sub); offset != std::string::npos;
	 offset = str.find(sub, offset + sub.length()))
    {
        ++count;
    }
    return count;
}

int main()
{
    std::cout << countSubstring("the three truths", "th")    << '\n';
    std::cout << countSubstring("ababababab", "abab")        << '\n';
    std::cout << countSubstring("abaabba*bbaba*bbab", "a*b") << '\n';

    return 0;
}
```

```txt

3
2
2

```



## C#


```c sharp
using System;

class SubStringTestClass
{
   public static int CountSubStrings(this string testString, string testSubstring)
   {
        int count = 0;

        if (testString.Contains(testSubstring))
        {
            for (int i = 0; i < testString.Length; i++)
            {
                if (testString.Substring(i).Length >= testSubstring.Length)
                {
                    bool equals = testString.Substring(i, testSubstring.Length).Equals(testSubstring);
                    if (equals)
                    {
                        count++;
                        i += testSubstring.Length - 1;  // Fix: Don't count overlapping matches
                    }
                }
            }
        }
        return count;
   }
}
```


Using C# 6.0's expression-bodied member, null-conditional operator, and coalesce operator features:


```c sharp
using System;
class SubStringTestClass
{
   public static int CountSubStrings(this string testString, string testSubstring) =>
       testString?.Split(new [] { testSubstring }, StringSplitOptions.None)?.Length - 1 ?? 0;
}
```



## Clojure

Use a sequence of regexp matches to count occurrences.

```clojure

(defn count-substring [txt sub]
  (count (re-seq (re-pattern sub) txt)))

```


Use the trick of blank replacement and maths to count occurrences.

```clojure

(defn count-substring1 [txt sub]
  (/ (- (count txt) (count (.replaceAll txt sub "")))
     (count sub)))

```



## COBOL

<code>INSPECT</code> can be used for this task without having to create a function.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. testing.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  occurrences             PIC 99.

       PROCEDURE DIVISION.
           INSPECT "the three truths" TALLYING occurrences FOR ALL "th"
           DISPLAY occurrences

           MOVE 0 TO occurrences
           INSPECT "ababababab" TALLYING occurrences FOR ALL "abab"
           DISPLAY occurrences

           MOVE 0 TO occurrences
           INSPECT "abaabba*bbaba*bbab" TALLYING occurrences
               FOR ALL "a*b"
           DISPLAY occurrences

           GOBACK
           .
```


```txt

03
02
02

```



## CoffeeScript


```coffeescript

countSubstring = (str, substr) ->
  n = 0
  i = 0
  while (pos = str.indexOf(substr, i)) != -1
    n += 1
    i = pos + substr.length
  n

console.log countSubstring "the three truths", "th"
console.log countSubstring "ababababab", "abab"

```



## Common Lisp


```lisp
(defun count-sub (str pat)
  (loop with z = 0 with s = 0 while s do
	(when (setf s (search pat str :start2 s))
	  (incf z) (incf s (length pat)))
	finally (return z)))

(count-sub "ababa" "ab")  ; 2
(count-sub "ababa" "aba") ; 1
```



## D


```d
void main() {
    import std.stdio, std.algorithm;

    "the three truths".count("th").writeln;
    "ababababab".count("abab").writeln;
}
```

```txt
3
2
```



## Delphi


```Delphi
program OccurrencesOfASubstring;

{$APPTYPE CONSOLE}

uses StrUtils;

function CountSubstring(const aString, aSubstring: string): Integer;
var
  lPosition: Integer;
begin
  Result := 0;
  lPosition := PosEx(aSubstring, aString);
  while lPosition <> 0 do
  begin
    Inc(Result);
    lPosition := PosEx(aSubstring, aString, lPosition + Length(aSubstring));
  end;
end;

begin
  Writeln(CountSubstring('the three truths', 'th'));
  Writeln(CountSubstring('ababababab', 'abab'));
end.
```

=={{header|Déjà Vu}}==

```dejavu
!. count "the three truths" "th"
!. count "ababababab" "abab"
```

```txt
3
2
```



## Dyalect



```dyalect
func countSubstring(str, val) {
    var idx = 0
    var count = 0
    while true {
        idx = str.indexOf(val, idx)
        if idx == -1 {
            break
        }
        idx += val.len()
        count += 1
    }
    return count
}

print(countSubstring("the three truths", "th"))
print(countSubstring("ababababab", "abab"))
```


```txt
3
2
```



## EchoLisp


```scheme

;; from Racket
(define count-substring
   (compose length regexp-match*))

(count-substring "aab" "graabaabdfaabgh") ;; substring
    → 3
(count-substring "/ .e/" "Longtemps je me suis couché de bonne heure") ;; regexp
    → 4

```



## Eiffel


```eiffel

class
	APPLICATION
inherit
	ARGUMENTS
create
	make
feature {NONE} -- Initialization
	make
			-- Run application.
		do
			occurance := 0
			from
				index := 1
			until
				index > text.count
			loop
				temp := text.fuzzy_index(search_for, index, 0)
				if
					temp /= 0
				then
					index := temp + search_for.count
					occurance := occurance + 1
				else
					index := text.count + 1
				end
			end
			print(occurance)
		end

	index:INTEGER
	temp:INTEGER
	occurance:INTEGER
	text:STRING = "ababababab"
	search_for:STRING = "abab"
end

```



## Elixir


```elixir
countSubstring = fn(_, "") -> 0
                   (str, sub) -> length(String.split(str, sub)) - 1 end

data = [ {"the three truths", "th"},
         {"ababababab", "abab"},
         {"abaabba*bbaba*bbab", "a*b"},
         {"abaabba*bbaba*bbab", "a"},
         {"abaabba*bbaba*bbab", " "},
         {"abaabba*bbaba*bbab", ""},
         {"", "a"},
         {"", ""} ]

Enum.each(data, fn{str, sub} ->
  IO.puts countSubstring.(str, sub)
end)
```


```txt

3
2
2
7
0
0
0
0

```



## Erlang


```Erlang

%% Count non-overlapping substrings in Erlang for the rosetta code wiki.
%% Implemented by J.W. Luiten

-module(substrings).
-export([main/2]).

%% String and Sub exhausted, count a match and present result
match([], [], _OrigSub, Acc) ->
  Acc+1;

%% String exhausted, present result
match([], _Sub, _OrigSub, Acc) ->
  Acc;

%% Sub exhausted, count a match
match(String, [], Sub, Acc) ->
  match(String, Sub, Sub, Acc+1);

%% First character matches, advance
match([X|MainTail], [X|SubTail], Sub, Acc) ->
  match(MainTail, SubTail, Sub, Acc);

%% First characters do not match. Keep scanning for sub in remainder of string
match([_X|MainTail], [_Y|_SubTail], Sub, Acc)->
  match(MainTail, Sub, Sub, Acc).

main(String, Sub) ->
   match(String, Sub, Sub, 0).
```

Command:
```Erlang
substrings:main("ababababab","abab").
```

```txt

2

```


Alternative using built in functions:

```Erlang

main( String, Sub ) -> erlang:length( binary:split(binary:list_to_bin(String), binary:list_to_bin(Sub), [global]) ) - 1.

```



## Euphoria


```euphoria
function countSubstring(sequence s, sequence sub)
    integer from,count
    count = 0
    from = 1
    while 1 do
        from = match_from(sub,s,from)
        if not from then
            exit
        end if
        from += length(sub)
        count += 1
    end while
    return count
end function

? countSubstring("the three truths","th")
? countSubstring("ababababab","abab")
```


```txt
3
2

```



## EGL

The "remove and count the difference" and "manual loop" methods. Implementation includes protection from empty source and search strings.

```EGL
program CountStrings

    function main()
        SysLib.writeStdout("Remove and Count:");
        SysLib.writeStdout(countSubstring("th", "the three truths"));
        SysLib.writeStdout(countSubstring("abab", "ababababab"));
        SysLib.writeStdout(countSubstring("a*b", "abaabba*bbaba*bbab"));
        SysLib.writeStdout(countSubstring("a", "abaabba*bbaba*bbab"));
        SysLib.writeStdout(countSubstring(" ", "abaabba*bbaba*bbab"));
        SysLib.writeStdout(countSubstring("", "abaabba*bbaba*bbab"));
        SysLib.writeStdout(countSubstring("a", ""));
        SysLib.writeStdout(countSubstring("", ""));

        SysLib.writeStdout("Manual Loop:");
        SysLib.writeStdout(countSubstringWithLoop("th", "the three truths"));
        SysLib.writeStdout(countSubstringWithLoop("abab", "ababababab"));
        SysLib.writeStdout(countSubstringWithLoop("a*b", "abaabba*bbaba*bbab"));
        SysLib.writeStdout(countSubstringWithLoop("a", "abaabba*bbaba*bbab"));
        SysLib.writeStdout(countSubstringWithLoop(" ", "abaabba*bbaba*bbab"));
        SysLib.writeStdout(countSubstringWithLoop("", "abaabba*bbaba*bbab"));
        SysLib.writeStdout(countSubstringWithLoop("a", ""));
        SysLib.writeStdout(countSubstringWithLoop("", ""));
    end

    function countSubstring(substr string in, str string in) returns(int)
        if(str.length() > 0 and substr.length() > 0)
	    return (str.length() - str.replaceStr(subStr, "").length()) / subStr.length();
	else
	    return 0;
	end
    end

    function countSubstringWithLoop(substr string in, str string in) returns(int)
        count int = 0;
        loc, index int = 1;
        strlen int = str.length();
        substrlen int = substr.length();

        if(strlen > 0 and substrlen > 0)
            while(loc != 0 and index <= strlen)
                loc = str.indexOf(substr, index);
                if(loc > 0)
                    count += 1;
                    index = loc + substrlen;
                end
            end
        end
        return count;
    end

end

```

```txt
Remove and Count:
3
2
2
7
0
0
0
0
Manual Loop:
3
2
2
7
0
0
0
0
```



## Factor


```factor
USING: math sequences splitting ;
: occurences ( seq subseq -- n ) split-subseq length 1 - ;
```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Count_occurrences_of_a_substring this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth


```forth
: str-count ( s1 len s2 len -- n )
  2swap 0 >r
  begin 2over search
  while 2over nip /string
        r> 1+ >r
  repeat 2drop 2drop r> ;

s" the three truths" s" th" str-count .    \ 3
s" ababababab" s" abab" str-count .     \ 2
```



## Fortran

```fortran
program Example
  implicit none
  integer :: n

  n = countsubstring("the three truths", "th")
  write(*,*) n
  n = countsubstring("ababababab", "abab")
  write(*,*) n
  n = countsubstring("abaabba*bbaba*bbab", "a*b")
  write(*,*) n

contains

function countsubstring(s1, s2) result(c)
  character(*), intent(in) :: s1, s2
  integer :: c, p, posn

  c = 0
  if(len(s2) == 0) return
  p = 1
  do
    posn = index(s1(p:), s2)
    if(posn == 0) return
    c = c + 1
    p = p + posn + len(s2)
  end do
end function
end program
```

```txt
3
2
2
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function countSubstring(s As String, search As String) As Integer
  If s = "" OrElse search = "" Then Return 0
  Dim As Integer count = 0, length = Len(search)
  For i As Integer = 1 To Len(s)
    If Mid(s, i, length) = Search Then
      count += 1
      i += length - 1
    End If
  Next
  Return count
End Function

Print countSubstring("the three truths","th")
Print countSubstring("ababababab","abab")
Print countSubString("zzzzzzzzzzzzzzz", "z")
Print
Print "Press any key to quit"
Sleep
```


```txt

 3
 2
 15

```


=={{header|F_Sharp|F#}}==
"Remove and count the difference" method, as shown by J, Java, ...

```Fsharp
open System

let countSubstring (where :string) (what : string) =
    match what with
    | "" -> 0 // just a definition; infinity is not an int
    | _ -> (where.Length - where.Replace(what, @"").Length) / what.Length


[<EntryPoint>]
let main argv =
    let show where what =
        printfn @"countSubstring(""%s"", ""%s"") = %d" where what (countSubstring where what)
    show "the three truths" "th"
    show "ababababab" "abab"
    show "abc" ""
    0
```


```txt
countSubstring("the three truths", "th") = 3
countSubstring("ababababab", "abab") = 2
countSubstring("abc", "") = 0
```



## FunL


```funl
import util.Regex

def countSubstring( str, substr ) = Regex( substr ).findAllMatchIn( str ).length()

println( countSubstring("the three truths", "th") )
println( countSubstring("ababababab", "abab") )
```


```txt

3
2

```



## Go

Using strings.Count() method:

```go
package main
import (
        "fmt"
        "strings"
)

func main() {
        fmt.Println(strings.Count("the three truths", "th")) // says: 3
        fmt.Println(strings.Count("ababababab", "abab"))     // says: 2
}
```



## Groovy

Solution, uses the Groovy "find" operator (=~), and the Groovy-extended Matcher property "count":

```groovy
println (('the three truths' =~ /th/).count)
println (('ababababab' =~ /abab/).count)
println (('abaabba*bbaba*bbab' =~ /a*b/).count)
println (('abaabba*bbaba*bbab' =~ /a\*b/).count)
```


```txt
3
2
9
2
```



## Haskell

=== Text-based solution ===

```haskell
import Data.Text hiding (length)

-- Return the number of non-overlapping occurrences of sub in str.
countSubStrs str sub = length $ breakOnAll (pack sub) (pack str)

main = do
  print $ countSubStrs "the three truths" "th"
  print $ countSubStrs "ababababab" "abab"

```

```txt

3
2

```


Alternatively, in a language built around currying, it might make more sense to reverse the suggested order of arguments.

```haskell
import Data.Text hiding (length)

countAll :: String -> String -> Int
countAll needle haystack = length (breakOnAll n h)
  where
    [n, h] = pack <$> [needle, haystack]

main :: IO ()
main =
  print $ countAll "ab" <$> ["ababababab", "abelian absurdity", "babel kebab"]
```


```txt
[5,2,2]
```


=== List-based solution ===
Even though list-based strings are not "the right" way of representing texts, the problem of counting subsequences in a list is generally useful.


```Haskell>count :: Eq a =
 [a] -> [a] -> Int
count []  = error "empty substring"
count sub = go
  where
    go = scan sub . dropWhile (/= head sub)
    scan _ [] = 0
    scan [] xs = 1 + go xs
    scan (x:xs) (y:ys) | x == y    = scan xs ys
                       | otherwise = go ys
```

```txt
λ> count "th" "the three truths"
3
λ> count "abab" "ababababab"
2
λ> count [2,3] [1,2,1,2,3,4,3,2,3,4,3,2]
2
λ> count "123456" $ foldMap show [1..1000000]
7
```


=== List-based solution using Data.List ===
The following solution is almost two times faster than the previous one.


```Haskell
import Data.List (tails, stripPrefix)
import Data.Maybe (catMaybes)

count :: Eq a => [a] -> [a] -> Int
count sub = length . catMaybes . map (stripPrefix sub) . tails
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
every A := ![ ["the three truths","th"], ["ababababab","abab"] ] do
   write("The string ",image(A[2])," occurs as a non-overlapping substring ",
         countSubstring!A , " times in ",image(A[1]))
end

procedure countSubstring(s1,s2) #: return count of non-overlapping substrings
c := 0
s1 ? while tab(find(s2)) do {
   move(*s2)
   c +:= 1
   }
return c
end
```


```txt
The string "th" occurs as a non-overlapping substring 3 times in "the three truths"
The string "abab" occurs as a non-overlapping substring 2 times in "ababababab"
```



## J



```j
require'strings'
countss=: #@] %~ #@[ - [ #@rplc '';~]
```


In other words: find length of original string, replace the string to be counted with the empty string, find the difference in lengths and divide by the length of the string to be counted.

Example use:


```j
   'the three truths' countss 'th'
3
   'ababababab' countss 'abab'
2
```



## Java

The "remove and count the difference" method:

```java
public class CountSubstring {
	public static int countSubstring(String subStr, String str){
		return (str.length() - str.replace(subStr, "").length()) / subStr.length();
	}

	public static void main(String[] args){
		System.out.println(countSubstring("th", "the three truths"));
		System.out.println(countSubstring("abab", "ababababab"));
		System.out.println(countSubstring("a*b", "abaabba*bbaba*bbab"));
	}
}
```

```txt
3
2
2
```


The "split and count" method:

```java
import java.util.regex.Pattern;

public class CountSubstring {
	public static int countSubstring(String subStr, String str){
		// the result of split() will contain one more element than the delimiter
		// the "-1" second argument makes it not discard trailing empty strings
		return str.split(Pattern.quote(subStr), -1).length - 1;
	}

	public static void main(String[] args){
		System.out.println(countSubstring("th", "the three truths"));
		System.out.println(countSubstring("abab", "ababababab"));
		System.out.println(countSubstring("a*b", "abaabba*bbaba*bbab"));
	}
}
```

```txt
3
2
2
```


Manual looping

```java
public class CountSubstring {
	public static int countSubstring(String subStr, String str){
		int count = 0;
		for (int loc = str.indexOf(subStr); loc != -1;
		     loc = str.indexOf(subStr, loc + subStr.length()))
			count++;
		return count;
	}

	public static void main(String[] args){
		System.out.println(countSubstring("th", "the three truths"));
		System.out.println(countSubstring("abab", "ababababab"));
		System.out.println(countSubstring("a*b", "abaabba*bbaba*bbab"));
	}
}
```

```txt
3
2
2
```



## JavaScript

Using regexes:

```javascript
function countSubstring(str, subStr) {
    var matches = str.match(new RegExp(subStr, "g"));
    return matches ? matches.length : 0;
}
```


Using 'split' and ES6 notation:

```javascript
const countSubString = (str, subStr) => str.split(subStr).length - 1;

```



## jq

Using regexes (available in jq versions after June 19, 2014):

```jq

def countSubstring(sub):
  [match(sub; "g")] | length;
```
Example:
```jq

"the three truths" | countSubstring("th")
```



## Julia

'''Built-in Function'''

```txt

matchall(r::Regex, s::String[, overlap::Bool=false]) -> Vector{String}

   Return a vector of the matching substrings from eachmatch.

```


'''Main'''

```Julia

ts = ["the three truths", "ababababab"]
tsub = ["th", "abab"]

println("Test of non-overlapping substring counts.")
for i in 1:length(ts)
    print(ts[i], " (", tsub[i], ") => ")
    println(length(matchall(Regex(tsub[i]), ts[i])))
end
println()
println("Test of overlapping substring counts.")
for i in 1:length(ts)
    print(ts[i], " (", tsub[i], ") => ")
    println(length(matchall(Regex(tsub[i]), ts[i], true)))
end

```


```txt

Test of non-overlapping substring counts.
the three truths (th) => 3
ababababab (abab) => 2

Test of overlapping substring counts.
the three truths (th) => 3
ababababab (abab) => 4

```



## K

The dyadic verb _ss gives the positions of substring y in string x.

```K
  "the three truths" _ss "th"
0 4 13

  #"the three truths" _ss "th"
3

  "ababababab" _ss "abab"
0 4

  #"ababababab" _ss "abab"
2

```



## Kotlin


```scala
// version 1.0.6

fun countSubstring(s: String, sub: String): Int = s.split(sub).size - 1

fun main(args: Array<String>) {
    println(countSubstring("the three truths","th"))
    println(countSubstring("ababababab","abab"))
    println(countSubstring("",""))
}
```


```txt

3
2
1

```



## Lasso


```Lasso
define countSubstring(str::string, substr::string)::integer => {
	local(i = 1, foundpos = -1, found = 0)
	while(#i < #str->size && #foundpos != 0) => {
		protect => {
			handle_error => { #foundpos = 0 }
			#foundpos = #str->find(#substr, -offset=#i)
		}
		if(#foundpos > 0) => {
			#found += 1
			#i = #foundpos + #substr->size
		else
			#i++
		}
	}
	return #found
}
define countSubstring_bothways(str::string, substr::string)::integer => {
	local(found = countSubstring(#str,#substr))
	#str->reverse
	local(found2 = countSubstring(#str,#substr))
	#found > #found2 ? return #found | return #found2
}
countSubstring_bothways('the three truths','th')
//3
countSubstring_bothways('ababababab','abab')
//2
```



## Liberty BASIC


```lb

print countSubstring( "the three truths", "th")
print countSubstring( "ababababab", "abab")
end

function countSubstring( a$, s$)
    c =0
    la =len( a$)
    ls =len( s$)
    for i =1 to la -ls
        if mid$( a$, i, ls) =s$ then c =c +1: i =i +ls -1
    next i
    countSubstring =c
end function

```



## Logtalk

Using atoms for string representation:

```logtalk

:- object(counting).

    :- public(count/3).

    count(String, SubString, Count) :-
        count(String, SubString, 0, Count).

    count(String, SubString, Count0, Count) :-
        (   sub_atom(String, Before, Length, After, SubString) ->
            Count1 is Count0 + 1,
            Start is Before + Length,
            sub_atom(String, Start, After, 0, Rest),
            count(Rest, SubString, Count1, Count)
        ;   Count is Count0
        ).

:- end_object.

```

```text

| ?- counting::count('the three truths', th, N).
N = 3
yes

| ?- counting::count(ababababab, abab, N).
N = 2
yes

```



## Lua

Solution 1:


```Lua
function countSubstring(s1, s2)
    return select(2, s1:gsub(s2, ""))
end

print(countSubstring("the three truths", "th"))
print(countSubstring("ababababab", "abab"))
```


```txt
3
2
```



Solution 2:


```Lua
function countSubstring(s1, s2)
    local count = 0
    for eachMatch in s1:gmatch(s2) do
        count = count + 1
    end
    return count
end

print(countSubstring("the three truths", "th"))
print(countSubstring("ababababab", "abab"))
```


```txt
3
2
```



## Maple


```Maple

f:=proc(s::string,c::string,count::nonnegint) local n;
     n:=StringTools:-Search(c,s);
     if n>0 then 1+procname(s[n+length(c)..],c,count);
     else 0; end if;
end proc:

f("the three truths","th",0);

f("ababababab","abab",0);

```

```txt

                                      3

                                      2

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
StringPosition["the three truths","th",Overlaps->False]//Length
3
StringPosition["ababababab","abab",Overlaps->False]//Length
2
```


=={{header|MATLAB}} / {{header|Octave}}==


```Matlab
  % Count occurrences of a substring without overlap
  length(findstr("ababababab","abab",0))
  length(findstr("the three truths","th",0))

  % Count occurrences of a substring with overlap
  length(findstr("ababababab","abab",1))
```


```txt

>>   % Count occurrences of a substring without overlap
>>   length(findstr("ababababab","abab",0))
ans =  2
>>   length(findstr("the three truths","th",0))
ans =  3
>>   % Count occurrences of a substring with overlap
>>   length(findstr("ababababab","abab",1))
ans =  4
>>
```



## Maxima


```maxima
scount(e, s) := block(
   [n: 0, k: 1],
   while integerp(k: ssearch(e, s, k)) do (n: n + 1, k: k + 1),
   n
)$

scount("na", "banana");
2
```



## MiniScript


```MiniScript
string.count = function(s)
    return self.split(s).len - 1
end function

print "the three truths".count("th")
print "ababababab".count("abab")
```

```txt

3
2

```



## Mirah


```mirah
import java.util.regex.Pattern
import java.util.regex.Matcher

#The "remove and count the difference" method
def count_substring(pattern:string, source:string)
    (source.length() - source.replace(pattern, "").length()) / pattern.length()
end

puts count_substring("th", "the three truths")      # ==> 3
puts count_substring("abab", "ababababab")          # ==> 2
puts count_substring("a*b", "abaabba*bbaba*bbab")   # ==> 2


# The "split and count" method
def count_substring2(pattern:string, source:string)
    # the result of split() will contain one more element than the delimiter
	# the "-1" second argument makes it not discard trailing empty strings
    source.split(Pattern.quote(pattern), -1).length - 1
end

puts count_substring2("th", "the three truths")      # ==> 3
puts count_substring2("abab", "ababababab")          # ==> 2
puts count_substring2("a*b", "abaabba*bbaba*bbab")   # ==> 2


# This method does a match and counts how many times it matches
def count_substring3(pattern:string, source:string)
    result = 0
    Matcher m = Pattern.compile(Pattern.quote(pattern)).matcher(source);
    while (m.find())
        result = result + 1
    end
    result
end

puts count_substring3("th", "the three truths")      # ==> 3
puts count_substring3("abab", "ababababab")          # ==> 2
puts count_substring3("a*b", "abaabba*bbaba*bbab")   # ==> 2

```



## Nemerle

```Nemerle
using System.Console;

module CountSubStrings
{
    CountSubStrings(this text : string, target : string) : int
    {
        match (target) {
            |"" => 0
            |_ => (text.Length - text.Replace(target, "").Length) / target.Length
        }
    }

    Main() : void
    {
        def text1 = "the three truths";
        def target1 = "th";
        def text2 = "ababababab";
        def target2 = "abab";

        WriteLine($"$target1 occurs $(text1.CountSubStrings(target1)) times in $text1");
        WriteLine($"$target2 occurs $(text2.CountSubStrings(target2)) times in $text2");
    }
}
```

```txt
th occurs 3 times in the three truths
abab occurs 2 times in ababababab
```



## NetRexx

NetRexx provides the <tt>''string''.countstr(''needle'')</tt> built-in function:


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method countSubstring(inStr, findStr) public static
  return inStr.countstr(findStr)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method main(args = String[]) public static
  strings = ''
  find = 'FIND'
  ix = 0
  ix = ix + 1; strings[0] = ix; find[0] = ix; strings[ix] = 'the three truths'; strings[ix, find] = 'th'
  ix = ix + 1; strings[0] = ix; find[0] = ix; strings[ix] = 'ababababab';       strings[ix, find] = 'abab'

  loop ix = 1 to strings[0]
    str = strings[ix]
    fnd = strings[ix, find]
    say 'there are' countSubstring(str, fnd) 'occurences of "'fnd'" in "'str'"'
    end ix

  return

```

```txt

there are 3 occurences of "th" in "the three truths"
there are 2 occurences of "abab" in "ababababab"

```



## NewLISP



```NewLISP
; file:   stringcount.lsp
; url:    http://rosettacode.org/wiki/Count_occurrences_of_a_substring
; author: oofoe 2012-01-29

; Obvious (and non-destructive...)

; Note that NewLISP performs an /implicit/ slice on a string or list
; with this form "(start# end# stringorlist)". If the end# is omitted,
; the slice will go to the end of the string. This is handy here to
; keep removing the front part of the string as it gets matched.

(define (scount needle haystack)
  (let ((h (copy haystack)) ; Copy of haystack string.
        (i 0)               ; Cursor.
        (c 0))              ; Count of occurences.

    (while (setq i (find needle h))
      (inc c)
      (setq h ((+ i (length needle)) h)))

    c))                     ; Return count.

; Tricky -- Uses functionality from replace function to find all
; non-overlapping occurrences, replace them, and return the count of
; items replaced in system variable $0.

(define (rcount needle haystack)
  (replace needle haystack "X") $0)

; Test

(define (test f needle haystack)
  (println "Found " (f needle haystack)
           " occurences of '" needle "' in '" haystack "'."))

(dolist (f (list scount rcount))
        (test f "glart" "hinkerpop")
        (test f "abab"  "ababababab")
        (test f "th"    "the three truths")
        (println)
        )

(exit)
```


```txt
Found 0 occurences of 'glart' in 'hinkerpop'.
Found 2 occurences of 'abab' in 'ababababab'.
Found 3 occurences of 'th' in 'the three truths'.

Found 0 occurences of 'glart' in 'hinkerpop'.
Found 2 occurences of 'abab' in 'ababababab'.
Found 3 occurences of 'th' in 'the three truths'.

```



## Nim


```nim
import strutils

proc count(s, sub: string): int =
  var i = 0
  while true:
    i = s.find(sub, i)
    if i < 0:
      break
    i += sub.len # i += 1 for overlapping substrings
    inc result

echo count("the three truths","th")

echo count("ababababab","abab")
```

```txt
3
2
```


=={{header|Objective-C}}==
The "split and count" method:

```objc
@interface NSString (CountSubstrings)
- (NSUInteger)occurrencesOfSubstring:(NSString *)subStr;
@end

@implementation NSString (CountSubstrings)
- (NSUInteger)occurrencesOfSubstring:(NSString *)subStr {
  return [[self componentsSeparatedByString:subStr] count] - 1;
}
@end

int main(int argc, const char *argv[]) {
  @autoreleasepool {

    NSLog(@"%lu", [@"the three truths" occurrencesOfSubstring:@"th"]);
    NSLog(@"%lu", [@"ababababab" occurrencesOfSubstring:@"abab"]);
    NSLog(@"%lu", [@"abaabba*bbaba*bbab" occurrencesOfSubstring:@"a*b"]);

  }
  return 0;
}
```

```txt
3
2
2
```



The "remove and count the difference" method:

```objc
@interface NSString (CountSubstrings)
- (NSUInteger)occurrencesOfSubstring:(NSString *)subStr;
@end

@implementation NSString (CountSubstrings)
- (NSUInteger)occurrencesOfSubstring:(NSString *)subStr {
  return ([self length] - [[self stringByReplacingOccurrencesOfString:subStr withString:@""] length]) / [subStr length];
}
@end

int main(int argc, const char *argv[]) {
  @autoreleasepool {

    NSLog(@"%lu", [@"the three truths" occurrencesOfSubstring:@"th"]);
    NSLog(@"%lu", [@"ababababab" occurrencesOfSubstring:@"abab"]);
    NSLog(@"%lu", [@"abaabba*bbaba*bbab" occurrencesOfSubstring:@"a*b"]);

  }
  return 0;
}
```

```txt
3
2
2
```



Manual looping:

```objc
@interface NSString (CountSubstrings)
- (NSUInteger)occurrencesOfSubstring:(NSString *)subStr;
@end

@implementation NSString (CountSubstrings)
- (NSUInteger)occurrencesOfSubstring:(NSString *)subStr {
  NSUInteger count = 0;
  for (NSRange range = [self rangeOfString:subStr]; range.location != NSNotFound;
       range.location += range.length,
       range = [self rangeOfString:subStr options:0
                             range:NSMakeRange(range.location, [self length] - range.location)])
    count++;
  return count;
}
@end

int main(int argc, const char *argv[]) {
  @autoreleasepool {

    NSLog(@"%lu", [@"the three truths" occurrencesOfSubstring:@"th"]);
    NSLog(@"%lu", [@"ababababab" occurrencesOfSubstring:@"abab"]);
    NSLog(@"%lu", [@"abaabba*bbaba*bbab" occurrencesOfSubstring:@"a*b"]);

  }
  return 0;
}
```

```txt
3
2
2
```



## OCaml



```ocaml
let count_substring str sub =
  let sub_len = String.length sub in
  let len_diff = (String.length str) - sub_len
  and reg = Str.regexp_string sub in
  let rec aux i n =
    if i > len_diff then n else
      try
        let pos = Str.search_forward reg str i in
        aux (pos + sub_len) (succ n)
      with Not_found -> n
  in
  aux 0 0

let () =
  Printf.printf "count 1: %d\n" (count_substring "the three truth" "th");
  Printf.printf "count 2: %d\n" (count_substring "ababababab" "abab");
;;
```



## Oforth



```Oforth

: countSubString(s, sub)
   0 1 while(sub swap s indexOfAllFrom dup notNull) [ sub size +  1 under+ ]
   drop ;
```


```txt

countSubString("the three truths", "th") println
3
countSubString("ababababab", "abab") println
2

```



## ooRexx


```ooRexx

 bag="the three truths"
 x="th"
 say left(bag,30) left(x,15) 'found' bag~countstr(x)

 bag="ababababab"
 x="abab"
 say left(bag,30) left(x,15) 'found' bag~countstr(x)

 -- can be done caselessly too
 bag="abABAbaBab"
 x="abab"
 say left(bag,30) left(x,15) 'found' bag~caselesscountstr(x)

```

<pre style="height:10ex;overflow:scroll">
the three truths               th              found 3
ababababab                     abab            found 2
abABAbaBab                     abab            found 2

```



## PARI/GP


```parigp
subvec(v,u)={
	my(i=1,s);
	while(i+#u<=#v,
		for(j=1,#u,
			if(v[i+j-1]!=u[j], i++; next(2))
		);
		s++;
		i+=#u
	);
	s
};
substr(s1,s2)=subvec(Vec(s1),Vec(s2));
substr("the three truths","th")
substr("ababababab","abab")
```

```txt
%1 = 3
%2 = 2
```



## Pascal

See [[Count_occurrences_of_a_substring#Delphi | Delphi]]


## Perl


```perl
sub countSubstring {
  my $str = shift;
  my $sub = quotemeta(shift);
  my $count = () = $str =~ /$sub/g;
  return $count;
#  or return scalar( () = $str =~ /$sub/g );
}

print countSubstring("the three truths","th"), "\n"; # prints "3"
print countSubstring("ababababab","abab"), "\n"; # prints "2"
```



## Perl 6


```perl6
sub count-substring($big,$little) { +$big.comb: ~$little }

say count-substring("the three truths","th"); # 3
say count-substring("ababababab","abab");     # 4

say count-substring(123123123,12);            # 3
```

The <tt>~</tt> prefix operator converts <tt>$little</tt> to a <tt>Str</tt> if it isn't already, and <tt>.comb</tt> when given a <tt>Str</tt> as an argument returns instances of that substring.  You can think of it as if the argument was a regex that matched the string literally <tt>/$little/</tt>.  Also, prefix <tt>+</tt> forces numeric context in Perl 6 (it's a no-op in Perl 5).  For the built in listy types that is the same as calling <tt>.elems</tt> method.  One other style point: we now tend to prefer hyphenated names over camelCase.


## Phix


```Phix
sequence tests = {{"the three truths","th"},
                  {"ababababab","abab"},
                  {"ababababab","aba"},
                  {"ababababab","ab"},
                  {"ababababab","a"},
                  {"ababababab",""}}
integer start, count
string test, substring
for i=1 to length(tests) do
    start = 1
    count = 0
    {test, substring} = tests[i]
    while 1 do
        start = match(substring,test,start)
        if start=0 then exit end if
        start += length(substring)
        count += 1
    end while
    printf(1,"The string \"%s\" occurs as a non-overlapping substring %d times in \"%s\"\n",{substring,count,test})
end for
```

```txt

The string "th" occurs as a non-overlapping substring 3 times in "the three truths"
The string "abab" occurs as a non-overlapping substring 2 times in "ababababab"
The string "aba" occurs as a non-overlapping substring 2 times in "ababababab"
The string "ab" occurs as a non-overlapping substring 5 times in "ababababab"
The string "a" occurs as a non-overlapping substring 5 times in "ababababab"
The string "" occurs as a non-overlapping substring 0 times in "ababababab"

```



## PHP


```php
<?php
echo substr_count("the three truths", "th"), "\n"; // prints "3"
echo substr_count("ababababab", "abab"), "\n"; // prints "2"
?>
```



## PicoLisp


```PicoLisp
(de countSubstring (Str Sub)
   (let (Cnt 0  H (chop Sub))
      (for (S (chop Str)  S  (cdr S))
         (when (head H S)
            (inc 'Cnt)
            (setq S (map prog2 H S)) ) )
      Cnt ) )
```

Test:

```txt
: (countSubstring "the three truths" "th")
-> 3

: (countSubstring "ababababab" "abab")
-> 2
```



## PL/I


```pli
cnt: procedure options (main);
   declare (i, tally) fixed binary;
   declare (text, key) character (100) varying;

   get edit (text) (L); put skip data (text);
   get edit (key)  (L); put skip data (key);

   tally = 0; i = 1;
   do until (i = 0);
      i = index(text, key, i);
      if i > 0 then do; tally = tally + 1; i = i + length(key); end;
   end;
   put skip list (tally);
end cnt;
```


Output for the two specified strings is as expected.
{{out}} for the following data:

```txt

TEXT='AAAAAAAAAAAAAAA';
KEY='AA';
        7

```



## PowerBASIC

Windows versions of PowerBASIC (at least since PB/Win 7, and possibly earlier) provide the <code>TALLY</code> function, which does exactly what this task requires (count non-overlapping substrings).

[[PB/DOS]] can use the example under [[#BASIC|BASIC]], above.

Note that while this example is marked as working with PB/Win, the <code>PRINT</code> statement would need to be replaced with <code>MSGBOX</code>, or output to a file. (PB/Win does not support console output.)


```powerbasic
FUNCTION PBMAIN () AS LONG
    PRINT "the three truths, th:", TALLY("the three truths", "th")
    PRINT "ababababab, abab:", TALLY("ababababab", "abab")
END FUNCTION
```


 the three truths, th:        3
 ababababab, abab:            2


## PureBasic


```PureBasic
a = CountString("the three truths","th")
b = CountString("ababababab","abab")
; a = 3
; b = 2
```



## PowerShell


```PowerShell

[regex]::Matches("the three truths", "th").count

```

<b>Output:</b>

```txt

3

```



```PowerShell

[regex]::Matches("ababababab","abab").count

```

<b>Output:</b>

```txt

2

```


## Prolog


Using SWI-Prolog's string facilities (this solution is very similar to the Logtalk solution that uses sub_atom/5):


```prolog


count_substring(String, Sub, Total) :-
    count_substring(String, Sub, 0, Total).

count_substring(String, Sub, Count, Total) :-
    ( substring_rest(String, Sub, Rest)
    ->
        succ(Count, NextCount),
        count_substring(Rest, Sub, NextCount, Total)
    ;
        Total = Count
    ).

substring_rest(String, Sub, Rest) :-
    sub_string(String, Before, Length, Remain, Sub),
    DropN is Before + Length,
    sub_string(String, DropN, Remain, 0, Rest).

```


Usage:

```prolog

?- count_substring("the three truths","th",X).
X = 3.

?- count_substring("ababababab","abab",X).
X = 2.

```



## Python


```python>>>
 "the three truths".count("th")
3
>>> "ababababab".count("abab")
2
```



## R


The <code>fixed</code> parameter (and, in <code>stringr</code>, the function of the same name) is used to specify a search for a fixed string. Otherwise, the search pattern is interpreted as a POSIX regular expression. PCRE is also an option: use the <code>perl</code> parameter or function.


```rsplus
count = function(haystack, needle)
   {v = attr(gregexpr(needle, haystack, fixed = T)[[1]], "match.length")
    if (identical(v, -1L)) 0 else length(v)}

print(count("hello", "l"))
```


```rsplus
library(stringr)
print(str_count("hello", fixed("l")))
```



## Racket


```racket

(define count-substring
  (compose length regexp-match*))

```


```racket

> (count-substring "th" "the three truths")
3
> (count-substring "abab" "ababababab")
2

```


## Red


```Red
Red []
;;-----------------------------------
count-sub1: func [hay needle][
;;-----------------------------------
  prin rejoin ["hay: " pad copy hay  20 ",needle: " pad copy needle 6  ",count: " ]
  i: 0
  parse hay [ some [thru needle (i: i + 1)] ]
  print i
]
;;-----------------------------------
count-sub2: func [hay needle][
;;-----------------------------------
  prin rejoin ["hay: " pad copy hay  20 ",needle: " pad copy needle 6  ",count: " ]
  i: 0
  while [hay: find hay needle][
    i: i + 1
    hay:  skip hay length? needle
  ]
  print i
]
count-sub1 "the three truths" "th"
count-sub1 "ababababab" "abab"
print "^/version 2"
count-sub2 "the three truths" "th"
count-sub2 "ababababab" "abab"

```

```txt
hay: the three truths    ,needle: th    ,count: 3
hay: ababababab          ,needle: abab  ,count: 2

version 2
hay: the three truths    ,needle: th    ,count: 3
hay: ababababab          ,needle: abab  ,count: 2
>>
```



## REXX

Some older REXXes don't have the built-in function   '''countstr''',   so one is included within the REXX program as a function.

The   '''countstr'''   subroutine (below) mimics the BIF in newer REXXes   (except for error checking).

Either of the first two strings may be null.

The third argument is optional and is the   ''start position''   to start counting   (the default is   '''1''',   meaning the first character).

If specified, it must be a positive integer   (and it may exceed the length of the 1<sup>st</sup> string).

The third argument was added here to be compatible with the newer REXXes BIF.

No checks are made (in the   '''countstr'''   subroutine) for:
::::*   missing arguments
::::*   too many arguments
::::*   if   '''start'''   is a positive integer (when specified)

```rexx
/*REXX program counts the  occurrences  of a (non─overlapping)  substring  in a string. */
w=.                                                                 /*max. width so far.*/
bag= 'the three truths'    ;      x= "th"       ;        call showResult
bag= 'ababababab'          ;      x= "abab"     ;        call showResult
bag= 'aaaabacad'           ;      x= "aa"       ;        call showResult
bag= 'abaabba*bbaba*bbab'  ;      x= "a*b"      ;        call showResult
bag= 'abaabba*bbaba*bbab'  ;      x= " "        ;        call showResult
bag=                       ;      x= "a"        ;        call showResult
bag=                       ;      x=            ;        call showResult
bag= 'catapultcatalog'     ;      x= "cat"      ;        call showResult
bag= 'aaaaaaaaaaaaaa'      ;      x= "aa"       ;        call showResult
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
countstr:   procedure;  parse arg haystack,needle,start;      if start==''  then start=1
            width=length(needle)
                                  do $=0  until p==0;         p=pos(needle,haystack,start)
                                  start=width + p                    /*prevent overlaps.*/
                                  end   /*$*/
            return $                                                 /*return the count.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
showResult: if w==. then do;                   w=30        /*W:  largest haystack width.*/
                         say center('haystack',w)  center('needle',w%2)  center('count',5)
                         say left('', w, "═")      left('', w%2, "═")    left('', 5, "═")
                         end

            if bag==''  then bag= " (null)"                /*handle displaying of nulls.*/
            if   x==''  then   x= " (null)"                /*   "        "      "   "   */
            say left(bag, w)           left(x, w%2)            center(countstr(bag, x), 5)
            return
```

'''output'''   when using the default (internal) inputs:

```txt

           haystack                needle      count
══════════════════════════════ ═══════════════ ═════
the three truths               th                3
ababababab                     abab              2
aaaabacad                      aa                2
abaabba*bbaba*bbab             a*b               2
abaabba*bbaba*bbab                               0
 (null)                        a                 0
 (null)                         (null)           1
catapultcatalog                cat               2
aaaaaaaaaaaaaa                 aa                7

```



## Ring


```Ring

aString = "Ring Welcome Ring to the Ring Ring Programming Ring Language Ring"
bString = "Ring"
see count(aString,bString)

func count cString,dString
     sum = 0
     while substr(cString,dString) > 0
           sum++
           cString = substr(cString,substr(cString,dString)+len(string(sum)))
     end
     return sum
```


Output:

```txt

6

```



## Ruby


```ruby
def countSubstrings str, subStr
  str.scan(subStr).length
end

p countSubstrings "the three truths", "th"      #=> 3
p countSubstrings "ababababab", "abab"          #=> 2
```


String#scan returns an array of substrings, and Array#length (or Array#size) counts them.


## Run BASIC


```runbasic
print countSubstring("the three truths","th")
print countSubstring("ababababab","abab")

FUNCTION countSubstring(s$,find$)
WHILE instr(s$,find$,i) <> 0
  countSubstring = countSubstring + 1
  i = instr(s$,find$,i) + len(find$)
WEND
END FUNCTION
```

```txt
3
2
```



## Rust


```Rust

fn main() {
    println!("{}","the three truths".matches("th").count());
    println!("{}","ababababab".matches("abab").count());
}

```

```txt

3
2

```


## Scala


### Using Recursion


```scala
import scala.annotation.tailrec
def countSubstring(str1:String, str2:String):Int={
   @tailrec def count(pos:Int, c:Int):Int={
      val idx=str1 indexOf(str2, pos)
      if(idx == -1) c else count(idx+str2.size, c+1)
   }
   count(0,0)
}
```



### Using Sliding


```scala
def countSubstring(str: String, sub: String): Int =
  str.sliding(sub.length).count(_ == sub)
```
<br/>


### Using Regular Expressions


```scala
def countSubstring( str:String, substr:String ) = substr.r.findAllMatchIn(str).length
```

<br/>

```scala
println(countSubstring("ababababab", "abab"))
println(countSubstring("the three truths", "th"))
```

```txt
2
3
```



## Scheme

```Scheme>gosh
 (use gauche.lazy)
#<undef>
gosh> (length (lrxmatch "th" "the three truths"))
3
gosh> (length (lrxmatch "abab" "ababababab"))
2

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func integer: countSubstring (in string: stri, in string: searched) is func
  result
    var integer: count is 0;
  local
    var integer: offset is 0;
  begin
    offset := pos(stri, searched);
    while offset <> 0 do
      incr(count);
      offset := pos(stri, searched, offset + length(searched));
    end while;
  end func;

const proc: main is func
  begin
    writeln(countSubstring("the three truths", "th"));
    writeln(countSubstring("ababababab", "abab"));
  end func;
```


```txt

3
2

```



## Sidef

'''Built-in:'''

```ruby
say "the three truths".count("th");
say "ababababab".count("abab");
```


'''User-created function:'''

```ruby
func countSubstring(s, ss) {
    var re = Regex.new(ss.escape, 'g');      # 'g' for global
    var counter = 0;
    while (s =~ re) { ++counter };
    return counter;
}

say countSubstring("the three truths","th");
say countSubstring("ababababab","abab");
```

```txt

3
2

```



## SNOBOL4


```SNOBOL4

        DEFINE("countSubstring(t,s)")

        OUTPUT = countSubstring("the three truths","th")
        OUTPUT = countSubstring("ababababab","abab")

        :(END)
countSubstring t ARB s = :F(RETURN)
        countSubstring = countSubstring + 1 :(countSubstring)
END
3
2

```



## Standard ML



```sml
fun count_substrings (str, sub) =
  let
    fun aux (str', count) =
      let
        val suff = #2 (Substring.position sub str')
      in
        if Substring.isEmpty suff then
       	  count
        else
          aux (Substring.triml (size sub) suff, count + 1)
      end
  in
    aux (Substring.full str, 0)
  end;

print (Int.toString (count_substrings ("the three truths", "th")) ^ "\n");
print (Int.toString (count_substrings ("ababababab", "abab")) ^ "\n");
print (Int.toString (count_substrings ("abaabba*bbaba*bbab", "a*b")) ^ "\n");
```



## Stata


```stata
function strcount(s, x) {
	n = 0
	k = 1-(i=strlen(x))
	do {
		if (k = ustrpos(s, x, k+i)) n++
	} while(k)
	return(n)
}

strcount("peter piper picked a peck of pickled peppers", "pe")
  5

strcount("ababababab","abab")
  2
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT, {}
occurences=COUNT ("the three truths",  ":th:")
occurences=COUNT ("ababababab",   ":abab:")
occurences=COUNT ("abaabba*bbaba*bbab",":a\*b:")

```

```txt

3
2
2

```



## Tcl

The regular expression engine is ideal for this task, especially as the <tt>***=</tt> prefix makes it interpret the rest of the argument as a literal string to match:

```tcl
proc countSubstrings {haystack needle} {
    regexp -all ***=$needle $haystack
}
puts [countSubstrings "the three truths" "th"]
puts [countSubstrings "ababababab" "abab"]
puts [countSubstrings "abaabba*bbaba*bbab" "a*b"]
```

```txt
3
2
2
```



## TXR



```txr
@(next :args)
@(do (defun count-occurrences (haystack needle)
       (for* ((occurrences 0)
              (old-pos 0)
              (new-pos (search-str haystack needle old-pos nil)))
             (new-pos occurrences)
             ((inc occurrences)
              (set old-pos (+ new-pos (length needle)))
              (set new-pos (search-str haystack needle old-pos nil))))))
@ndl
@hay
@(output)
@(count-occurrences hay ndl) occurrences(s) of @ndl inside @hay
@(end)
```



```txt
$ ./txr count-occurrences.txr "baba" "babababa"
2 occurence(s) of baba inside babababa
$ ./txr count-occurrences.txr "cat" "catapultcatalog"
2 occurence(s) of cat inside catapultcatalog
```



## UNIX Shell

```bash
#!/bin/bash

function countString(){
	input=$1
	cnt=0

	until [ "${input/$2/}" == "$input" ]; do
		input=${input/$2/}
		let cnt+=1
	done
	echo $cnt
}

countString "the three truths" "th"
countString "ababababab" "abab"
```

```txt
3
2
```



## VBA



```VBA
Function CountStringInString(stLookIn As String, stLookFor As String)
    CountStringInString = UBound(Split(stLookIn, stLookFor))
End Function
```



## VBScript


```vb

Function CountSubstring(str,substr)
	CountSubstring = 0
	For i = 1 To Len(str)
		If Len(str) >= Len(substr) Then
			If InStr(i,str,substr) Then
				CountSubstring = CountSubstring + 1
				i = InStr(i,str,substr) + Len(substr) - 1
			End If
		Else
			Exit For
		End If
	Next
End Function

WScript.StdOut.Write CountSubstring("the three truths","th") & vbCrLf
WScript.StdOut.Write CountSubstring("ababababab","abab") & vbCrLf

```


```txt

3
2

```



## Visual Basic .NET


```vbnet
Module Count_Occurrences_of_a_Substring
    Sub Main()
        Console.WriteLine(CountSubstring("the three truths", "th"))
        Console.WriteLine(CountSubstring("ababababab", "abab"))
        Console.WriteLine(CountSubstring("abaabba*bbaba*bbab", "a*b"))
        Console.WriteLine(CountSubstring("abc", ""))
    End Sub

    Function CountSubstring(str As String, substr As String) As Integer
        Dim count As Integer = 0
        If (Len(str) > 0) And (Len(substr) > 0) Then
            Dim p As Integer = InStr(str, substr)
            Do While p <> 0
                p = InStr(p + Len(substr), str, substr)
                count += 1
            Loop
        End If
        Return count
    End Function
End Module
```

```txt
3
2
2
0
```



## Wortel


```wortel
@let {
  c &[s t] #!s.match &(t)g

  [[
    !!c "the three truths" "th"
    !!c "ababababab" "abab"
  ]]
}
```

Returns:
```txt
[3 2]
```



## XPL0


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations
string 0;               \use zero-terminated strings, instead of MSb terminated


func    StrNCmp(A, B, N); \Compare string A to string B up to N bytes long
\This returns:
\       >0 if A > B
\       =0 if A = B
\       <0 if A < B
char    A, B;           \strings to be compared
int     N;              \number of bytes to compare
int     I;
[for I:= 0 to N-1 do
    if A(I) # B(I) then
        return A(I) - B(I);
return 0;               \they're equal
];      \StrNCmp


func    StrLen(Str);    \Return the number of characters in an ASCIIZ string
char    Str;
int     I;
for I:= 0 to -1>>1-1 do
        if Str(I) = 0 then return I;


func    SubStr(A, B);   \Count number of times string B occurs in A
char    A, B;
int     LA, LB, C, I;
[LA:= StrLen(A);  LB:= StrLen(B);
C:= 0;  I:= 0;
while I < LA do
        if StrNCmp(B, A+I, LB) = 0 then [C:= C+1;  I:= I+LB]
        else I:= I+1;
return C;
];


[IntOut(0, SubStr("the three truths", "th"));  CrLf(0);
 IntOut(0, SubStr("ababababab", "abab"));  CrLf(0);
]
```


```txt

3
2

```


```txt

>>   % Count occurrences of a substring without overlap
>>   length(findstr("ababababab","abab",0))
ans =  2
>>   length(findstr("the three truths","th",0))
ans =  3
>>   % Count occurrences of a substring with overlap
>>   length(findstr("ababababab","abab",1))
ans =  4
>>

```



## zkl

Two solutions:

```zkl
fcn countSubstring(s,p){ pn:=p.len(); cnt:=n:=0;
   while(Void!=(n:=s.find(p,n))){cnt+=1; n+=pn}
   cnt
}
```

```zkl
fcn countSubstring(s,p){ (pl:=p.len()) and (s.len()-(s-p).len())/pl }
```

```txt

zkl: println(countSubstring("the three truths","th"))
3
zkl: println(countSubstring("ababababab","abab"))
2
zkl: println(countSubstring("ababababab","v"))
0

```



## ZX Spectrum Basic


```zxbasic
10 LET t$="ABABABABAB": LET p$="ABAB": GO SUB 1000
20 LET t$="THE THREE TRUTHS": LET p$="TH": GO SUB 1000
30 STOP
1000 PRINT t$: LET c=0
1010 LET lp=LEN p$
1020 FOR i=1 TO LEN t$-lp+1
1030 IF (t$(i TO i+lp-1)=p$) THEN LET c=c+1: LET i=i+lp-1
1040 NEXT i
1050 PRINT p$;"=";c''
1060 RETURN
```

