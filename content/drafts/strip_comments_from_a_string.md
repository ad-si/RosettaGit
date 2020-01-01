+++
title = "Strip comments from a string"
description = ""
date = 2019-10-13T00:59:23Z
aliases = []
[extra]
id = 8640
[taxonomies]
categories = []
tags = []
+++

{{clarify task}}
{{task|Basic language learning}}[[Category:String manipulation]]

The task is to remove text that follow any of a set of comment markers, (in these examples either a hash or a semicolon) from a string or input line.


'''Whitespace debacle:'''   There is some confusion about whether to remove any whitespace from the input line.

As of [http://rosettacode.org/mw/index.php?title=Strip_comments_from_a_string&oldid=119409 2 September 2011], at least 8 languages (C, C++, Java, Perl, Python, Ruby, sed, UNIX Shell) were incorrect, out of 36 total languages, because they did not trim whitespace by 29 March 2011 rules. Some other languages might be incorrect for the same reason.

'''Please discuss this issue at [[{{TALKPAGENAME}}]].'''

* From [http://rosettacode.org/mw/index.php?title=Strip_comments_from_a_string&oldid=103978 29 March 2011], this task required that: ''"The comment marker and any whitespace at the beginning or ends of the resultant line should be removed. A line without comments should be trimmed of any leading or trailing whitespace before being produced as a result."'' The task had 28 languages, which did not all meet this new requirement.
* From [http://rosettacode.org/mw/index.php?title=Strip_comments_from_a_string&oldid=103978 28 March 2011], this task required that: ''"Whitespace before the comment marker should be removed."''
* From [http://rosettacode.org/mw/index.php?title=Strip_comments_from_a_string&offset=20101206204307&action=history 30 October 2010], this task did not specify whether or not to remove whitespace.



The following examples will be truncated to either "apples, pears " or "apples, pears".

(This example has flipped between "apples, pears " and "apples, pears" in the past.)


```txt

apples, pears # and bananas
apples, pears ; and bananas

```



;Related task:
*   [[Strip block comments]]





## Ada


```Ada
with Ada.Text_IO;
procedure Program is
   Comment_Characters : String := "#;";
begin
   loop
      declare
	 Line : String := Ada.Text_IO.Get_Line;
      begin
	 exit when Line'Length = 0;
	 Outer_Loop : for I in Line'Range loop
	    for J in Comment_Characters'Range loop
	       if Comment_Characters(J) = Line(I) then
		  Ada.Text_IO.Put_Line(Line(Line'First .. I - 1));
		  exit Outer_Loop;
	       end if;
	    end loop;
	 end loop Outer_Loop;
      end;
   end loop;
end Program;
```



## Aime


```aime
strip_comments(data b)
{
    b.size(b.look(0, ";#")).bf_drop(" \t").bb_drop(" \t");
}

main(void)
{
    for (, text n in list("apples, pears # and bananas", "apples, pears ; and bananas")) {
        o_(strip_comments(n), "\n");
    }

    0;
}
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny].}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}

```algol68
#!/usr/local/bin/a68g --script #

PROC trim comment = (STRING line, CHAR marker)STRING:(
  INT index := UPB line+1;
  char in string(marker, index, line);
  FOR i FROM index-1 BY -1 TO LWB line
  WHILE line[i]=" " DO index := i OD;
  line[:index-1]
);

CHAR q = """";

print((
  q, trim comment("apples, pears # and bananas", "#"), q, new line,
  q, trim comment("apples, pears ; and bananas", ";"), q, new line,
  q, trim comment("apples, pears and bananas  ", ";"), q, new line,
  q, trim comment("    ", ";"), q, new line, # blank string #
  q, trim comment("", ";"), q, new line  # empty string #
))

CO Alternatively Algol68g has available "grep"
;STRING re marker := " *#", line := "apples, pears # and bananas";
  INT index := UPB line;
  grep in string(re marker, line, index, NIL);
  print((q, line[:index-1], q, new line))
END CO
```

Output:

```txt

"apples, pears"
"apples, pears"
"apples, pears and bananas"
""
""

```



## ALGOL W

Leading and trailing spaces are removed from the result, as per the March 29 2011 task version.

```algolw
begin
    % determines the non-comment portion of the string s, startPos and endPos are   %
    % returned set to the beginning and ending character positions (indexed from 0) %
    % of the non-comment text in s. If there is no non-comment text in s, startPos  %
    % will be greater than endPos                                                   %
    % note that in Algol W, strings can be at most 256 characters long              %
    procedure stripComments ( string(256) value s; integer result startPos, endPos ) ;
    begin
        integer MAX_LENGTH;
        MAX_LENGTH := 256;
        startPos   :=  0;
        endPos     := -1;
        % find the first non-blank character in s %
        while startPos < MAX_LENGTH and s( startPos // 1 ) = " " do startPos := startPos + 1;
        if startPos < MAX_LENGTH then begin
            % have a non-blank character in the string %
            if s( startPos // 1 ) not = "#" and s( startPos // 1 ) not = ";" then begin
                % the non-blank character is not a comment delimiter %
                integer cPos;
                cPos := endPos := startPos;
                while cPos < MAX_LENGTH and s( cPos // 1 ) not = "#" and s( cPos // 1 ) not = ";" do begin
                    if s( cPos // 1 ) not = " " then endPos := cPos;
                    cPos := cPos + 1
                end while_not_a_comment
            end if_not_a_comment
        end if_startPos_lt_MAX_LENGTH
    end stripComments ;
    % tests the stripComments procedure                                             %
    procedure testStripComments( string(256) value s ) ;
    begin
        integer startPos, endPos;
        stripComments( s, startPos, endPos );
        write( """" );
        for cPos := startPos until endPos do writeon( s( cPos // 1 ) );
        writeon( """" )
    end testStripComments ;
    begin % test cases - should all print "apples, pears"                           %
        testStripComments( "apples, pears # and bananas" );
        testStripComments( "apples, pears ; and bananas" );
        testStripComments( "apples, pears "              );
        testStripComments( "              apples, pears" )
    end
end.
```

{{out}}

```txt

"apples, pears"
"apples, pears"
"apples, pears"
"apples, pears"

```



## Applesoft BASIC


```ApplesoftBasic
10 LET C$ = ";#"
20 S$(1)="APPLES, PEARS # AND BANANAS"
30 S$(2)="APPLES, PEARS ; AND BANANAS"
40 FOR Q = 1 TO 2
50     LET S$ = S$(Q)
60     GOSUB 100"STRIP COMMENTS
70     PRINT S$
80 NEXT Q
90 END

100 IF S$ = "" THEN RETURN
110 FOR I = 1 TO LEN(S$)
120     LET A$ = MID$(S$, I, 1)
130     FOR J = 1 TO LEN(C$)
140         LET F$ = MID$(C$, J, 1)
150         IF A$ <> F$ THEN NEXT J
160     IF A$ = F$ THEN 200
170 NEXT I
200 LET I = I - 1
210 GOSUB 260"STRIP
220 IF S$ = "" THEN RETURN
230 FOR I = I TO 0 STEP -1
240     LET A$ = MID$(S$, I, 1)
250     IF A$ = " " THEN NEXT I
260 LET S$ = MID$(S$, 1, I)
270 RETURN
```



## AutoHotkey


```AutoHotkey
Delims := "#;"
str := "apples, pears # and bananas"
str2:= "apples, pears, `; and bananas" ; needed to escape the ; since that is AHK's comment marker
msgbox % StripComments(Str,Delims)
msgbox % StripComments(Str2,Delims)
; The % forces expression mode.


StripComments(String1,Delims){
    Loop, parse, delims
    {
        If Instr(String1,A_LoopField)
            EndPosition := InStr(String1,A_LoopField) - 1
        Else
            EndPosition := StrLen(String1)
        StringLeft, String1, String1, EndPosition
    }
    return String1
}
```

Output:

```txt

apples, pears
apples, pears,

```




## AutoIt

It was always said in discussion, the task is not really stripping comments. It's only a truncation.

```AutoIt

Dim $Line1 = "apples, pears # and bananas"
Dim $Line2 = "apples, pears ; and bananas"

_StripAtMarker($Line1)
_StripAtMarker($Line2)

Func _StripAtMarker($_Line, $sMarker='# ;')
	Local $aMarker = StringSplit($sMarker, ' ')
	Local $iPos
	For $i = 1 To $aMarker[0]
		$iPos = StringInStr($_Line, $aMarker[$i])
		If $iPos Then
			ConsoleWrite($_Line & @CRLF)
			ConsoleWrite( StringStripWS( StringLeft($_Line, $iPos -1), 2) & @CRLF)
		EndIf
	Next
EndFunc  ;==>_StripAtMarker

```

Output:

```txt

apples, pears # and bananas
apples, pears
apples, pears ; and bananas
apples, pears

```


Here is a really language-related solution to parse script lines and delete comments.
A comment in line in AutoIt starts with an semicolon.
But it may be possible, that a semicolon is part of a string in a parameter from an function-call/function-headline or in an assignment.
That means: the comment starts with the first semicolon outside a string.


```AutoIt

Dim $aLines[4] = _
[ _
"$a = $b + $c ; Comment line 1", _
"Dim $s1 = 'some text; tiled with semicolon', $s2 = 'another text; also tiled with semicolon' ; Comment line 2 - semicolon as part of assignment", _
"_SomeFunctionCall('string parameter with ;', $anotherParam) ; Comment line 3 - semicolon as part parameter in an function call", _
"Func _AnotherFunction($param1=';', $param2=';', $param3=';') ; Comment line 4 - semicolon as default value in parameter of a function headline" _
]

For $i = 0 To 3
	ConsoleWrite('+> Line ' & $i+1 & ' full:' & @CRLF & '+>' & $aLines[$i] & @CRLF)
	ConsoleWrite('!> without comment:' & @CRLF & '!>' & _LineStripComment($aLines[$i]) & @CRLF & @CRLF)
Next


Func _LineStripComment($_Line)
	; == tile line by all included comment marker
	Local $aPartsWithMarker = StringSplit($_Line, ';')
	Local $sNoComment

	; == if no comment marker: return full line
	If $aPartsWithMarker[0] = 0 Then Return $_Line

	; == check if string in part, if is'nt: following part(s) are comment
	For $i = 1 To $aPartsWithMarker[0]
		If Not StringRegExp($aPartsWithMarker[$i], "('|\x22)") Then
			If $i = 1 Then
				Return StringStripWS($aPartsWithMarker[$i], 2)
			Else
				Return StringStripWS($sNoComment & $aPartsWithMarker[$i], 2)
			EndIf
		Else
			; == check if next leftside string delimiter has uneven count
			Local $iLen = StringLen($aPartsWithMarker[$i])
			Local $fDetectDelim = False, $sStringDelim, $iDelimCount, $sCurr
			For $j = $iLen To 1 Step -1
				$sCurr = StringMid($aPartsWithMarker[$i], $j, 1)
				If Not $fDetectDelim Then
					If $sCurr = "'" Or $sCurr = '"' Then
						$sStringDelim = $sCurr
						$iDelimCount += 1
						$fDetectDelim = True
					EndIf
				Else
					If $sCurr = $sStringDelim Then $iDelimCount += 1
				EndIf
			Next
			If Mod($iDelimCount, 2) Then
				; == uneven count: so it masks the comment marker
				$sNoComment &= $aPartsWithMarker[$i] & ';'
			Else
				; == even count: all following is comment
				Return StringStripWS($sNoComment & $aPartsWithMarker[$i], 2)
			EndIf
		EndIf
	Next
EndFunc  ;==>_LineStripComment

```

Output:

```txt

+> Line 1 full:
+>$a = $b + $c ; Comment line 1
>> without comment:
>>$a = $b + $c

+> Line 2 full:
+>Dim $s1 = 'some text; tiled with semicolon', $s2 = 'another text; also tiled with semicolon' ; Comment line 2 - semicolon as part of assignment
>> without comment:
>>Dim $s1 = 'some text; tiled with semicolon', $s2 = 'another text; also tiled with semicolon'

+> Line 3 full:
+>_SomeFunctionCall('string parameter with ;', $anotherParam) ; Comment line 3 - semicolon as part parameter in an function call
>> without comment:
>>_SomeFunctionCall('string parameter with ;', $anotherParam)

+> Line 4 full:
+>Func _AnotherFunction($param1=';', $param2=';', $param3=';') ; Comment line 4 - semicolon as default value in parameter of a function headline
>> without comment:
>>Func _AnotherFunction($param1=';', $param2=';', $param3=';')

```



## AWK



```AWK
#!/usr/local/bin/awk -f
{
   sub("[ \t]*[#;].*$","",$0);
   print;
}
```



## BBC BASIC


```bbcbasic
      marker$ = "#;"
      PRINT FNstripcomment("apples, pears # and bananas", marker$)
      PRINT FNstripcomment("apples, pears ; and bananas", marker$)
      PRINT FNstripcomment("   apples, pears   ", marker$)
      END

      DEF FNstripcomment(text$, delim$)
      LOCAL I%, D%
      FOR I% = 1 TO LEN(delim$)
        D% = INSTR(text$, MID$(delim$, I%, 1))
        IF D% text$ = LEFT$(text$, D%-1)
      NEXT I%
      WHILE ASC(text$) = 32 text$ = MID$(text$,2) : ENDWHILE
      WHILE LEFT$(text$) = " " text$ = RIGHT$(text$) : ENDWHILE
      = text$
```



## Bracmat


```bracmat
(   "    apples, pears # and bananas
       oranges, mangos ; and a durian"
  : ?text
& :?newText
& ( non-blank
  = %@:~(" "|\t|\r|\n)
  )
& ( cleanUp
  =
    .   @(!arg:?arg ("#"|";") ?)
      & @(rev$!arg:? (!non-blank ?:?arg))
      & @(rev$!arg:? (!non-blank ?:?arg))
      & !arg    {You could write & "[" !arg "]" to prove that the blanks are stripped.}
  )
&   whl
  ' ( @(!text:?line (\r|\n) ?text)
    & !newText \n cleanUp$!line:?newText
    )
& !newText \n cleanUp$!text:?newText
& out$(str$!newText)
);
```

Output:

```txt

apples, pears
oranges, mangos
```



## C


```C>#include<stdio.h


int main()
{
	char ch, str[100];
	int i;

	do{
		printf("\nEnter the string :");
		fgets(str,100,stdin);
		for(i=0;str[i]!=00;i++)
		{
			if(str[i]=='#'||str[i]==';')
			{
				str[i]=00;
				break;
			}
		}
		printf("\nThe modified string is : %s",str);
		printf("\nDo you want to repeat (y/n): ");
		scanf("%c",&ch);
		fflush(stdin);
	}while(ch=='y'||ch=='Y');

	return 0;
}
```

Output:

```txt

Enter the string :apples, pears # and bananas

The modified string is : apples, pears
Do you want to repeat (y/n): y

Enter the string :apples, pears ; and bananas

The modified string is : apples, pears
Do you want to repeat (y/n): n

```



## C++


```cpp
#include <iostream>
#include <string>

std::string strip_white(const std::string& input)
{
   size_t b = input.find_first_not_of(' ');
   if (b == std::string::npos) b = 0;
   return input.substr(b, input.find_last_not_of(' ') + 1 - b);
}

std::string strip_comments(const std::string& input, const std::string& delimiters)
{
   return strip_white(input.substr(0, input.find_first_of(delimiters)));
}

int main( ) {
   std::string input;
   std::string delimiters("#;");
   while ( getline(std::cin, input) && !input.empty() ) {
      std::cout << strip_comments(input, delimiters) << std::endl ;
   }
   return 0;
}
```

Sample output:

```txt

apples, pears # and bananas
apples, pears
apples, pears ; and bananas
apples, pears

```


## C#


```c#

using System.Text.RegularExpressions;

string RemoveComments(string str, string delimiter)
        {
            //regular expression to find a character (delimiter) and
            //      replace it and everything following it with an empty string.
            //.Trim() will remove all beginning and ending white space.
            return Regex.Replace(str, delimiter + ".+", string.Empty).Trim();
        }

```

Sample output:

```txt

Console.WriteLine(RemoveComments("apples, pears # and bananas", "#"));
Console.WriteLine(RemoveComments("apples, pears ; and bananas", ";"));
apples, pears
apples, pears

```



## Clojure


```clojure>
 (apply str (take-while #(not (#{\# \;} %)) "apples # comment"))
"apples "
```



## Common Lisp


```lisp
(defun strip-comments (s cs)
  "Truncate s at the first occurrence of a character in cs."
  (defun comment-char-p (c)
    (some #'(lambda (x) (char= x c)) cs))
  (let ((pos (position-if #'comment-char-p s)))
    (subseq s 0 pos)))
```


{{Out}}
Use the function in combination with '''STRING-TRIM''' to fulfill the task requirements.

```txt
> (string-trim '(#\Space #\Tab) (strip-comments "apples, pears # and bananas" "#;"))

"apples, pears"
> (string-trim '(#\Space #\Tab) (strip-comments "apples, pears ; and bananas" "#;"))

"apples, pears"
> (string-trim '(#\Space #\Tab) (strip-comments "  apples, pears and bananas  " "#;"))

"apples, pears and bananas"
```



## D


```d
import std.stdio, std.regex;

string remove1LineComment(in string s, in string pat=";#") {
    const re = "([^" ~ pat ~ "]*)([" ~ pat ~ `])[^\n\r]*([\n\r]|$)`;
    return s.replace(regex(re, "gm"), "$1$3");
}

void main() {
    const s = "apples, pears # and bananas
apples, pears ; and bananas ";

    writeln(s, "\n====>\n", s.remove1LineComment());
}
```

{{out}}

```txt
apples, pears # and bananas
apples, pears ; and bananas
====>
apples, pears
apples, pears
```



## Delphi


```Delphi
program StripComments;

{$APPTYPE CONSOLE}

uses
  SysUtils;

function DoStripComments(const InString: string; const CommentMarker: Char): string;
begin
  Result := Trim(Copy(InString,1,Pos(CommentMarker,InString)-1));
end;

begin
  Writeln('apples, pears # and bananas --> ' + DoStripComments('apples, pears # and bananas','#'));
  Writeln('');
  Writeln('apples, pears ; and bananas --> ' + DoStripComments('apples, pears ; and bananas',';'));
  Readln;
end.
```



## DWScript


```delphi
function StripComments(s : String) : String;
begin
   var p := FindDelimiter('#;', s);
   if p>0 then
      Result := Trim(Copy(s, 1, p-1))
   else Result := Trim(s);
end;

PrintLn(StripComments('apples, pears # and bananas'));
PrintLn(StripComments('apples, pears ; and bananas'));
```



## Erlang


```Erlang

-module( strip_comments_from_string ).

-export( [task/0] ).

task() ->
    io:fwrite( "~s~n", [keep_until_comment("apples, pears and bananas")] ),
    io:fwrite( "~s~n", [keep_until_comment("apples, pears # and bananas")] ),
    io:fwrite( "~s~n", [keep_until_comment("apples, pears ; and bananas")] ).



keep_until_comment( String ) ->	lists:takewhile( fun not_comment/1, String ).

not_comment( $# ) -> false;
not_comment( $; ) -> false;
not_comment( _ ) -> true.

```

{{out}}

```txt

17> strip_comments_from_string:task().
apples, pears and bananas
apples, pears
apples, pears

```


=={{header|F_Sharp|F#}}==

```fsharp
let stripComments s =
    s
    |> Seq.takeWhile (fun c -> c <> '#' && c <> ';')
    |> Seq.map System.Char.ToString
    |> Seq.fold (+) ""
```



## Factor


```factor
USE: sequences.extras
: strip-comments ( str -- str' )
    [ "#;" member? not ] take-while "" like ;
```



## Fantom

Using a regular expression:

```fantom
class Main
{
  static Str removeComment (Str str)
  {
    regex := Regex <|(;|#)|>
    matcher := regex.matcher (str)
    if (matcher.find)
      return str[0..<matcher.start]
    else
      return str
  }

  public static Void main ()
  {
    echo (removeComment ("String with comment here"))
    echo (removeComment ("String with comment # here"))
    echo (removeComment ("String with comment ; here"))
  }
}
```



## Forth

Modern Forth advocates the use of stack strings. Stack strings are manipulated as an address and a length on the Forth DATA stack. As such they do not require memory copying for many forms of string functions making them fast.  Using stack strings and concatenating functions means Forth does this task with no REGEX required.

NOTES:

1. SCAN is not a standard function but is common in most Forth systems as either a library function or resident.

2. FORTH style factoring means you can strip the comments and strip trailing spaces or not, as you see fit.
Or Concatenate the functions together as we did here.

Tested with Swift Forth on OS/X

<LANG FORTH>\ Rosetta Code Strip Comment

: LASTCHAR ( addr len -- addr len c) 2DUP + 1- C@ ;

: COMMENT? ( char -- ? )  S" #;"  ROT SCAN NIP ; \ is char '#' or ';'

: -COMMENT   ( addr len -- addr len') \ removes # or ; comments
            BEGIN
              LASTCHAR COMMENT? 0=
             WHILE                    \ while not a comment char...
                1-                    \ reduce length by 1
            REPEAT
            1-  ;                     \ remove 1 more (the comment char)

\ -TRAILING is resident in desktop Forth systems like Swift Forth
\ shown here for demonstration
: -TRAILING  ( adr len -- adr len')    \ remove trailing spaces
             BEGIN
               LASTCHAR BL =
             WHILE                     \ while lastchar = blank char
               1-                      \ reduce length by 1
             REPEAT  ;

: COMMENT-STRIP ( addr len -- addr 'len)  -COMMENT  -TRAILING ;</LANG>

Tested at the Forth console


```Forth
S" apples, pears # and bananas"  COMMENT-STRIP TYPE apples, pears ok
S" apples, pears ; and bananas"  COMMENT-STRIP TYPE apples, pears ok
```



## Fortran


```fortran
!****************************************************
 module string_routines
!****************************************************
 implicit none
 private
 public :: strip_comments
 contains
!****************************************************

	 function strip_comments(str,c) result(str2)
	 implicit none
	 character(len=*),intent(in) :: str
	 character(len=1),intent(in) :: c !comment character
	 character(len=len(str)) :: str2

	 integer :: i

	 i = index(str,c)
	 if (i>0) then
		str2 = str(1:i-1)
	 else
		str2 = str
	 end if

	 end function strip_comments

!****************************************************
 end module string_routines
!****************************************************

!****************************************************
 program main
!****************************************************
! Example use of strip_comments function
!****************************************************
 use string_routines, only: strip_comments
 implicit none

 write(*,*) strip_comments('apples, pears # and bananas', '#')
 write(*,*) strip_comments('apples, pears ; and bananas', ';')

!****************************************************
 end program main
!****************************************************
```

output:

```txt

apples, pears
apples, pears

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Sub stripComment(s As String, commentMarkers As String)
  If s = "" Then Return
  Dim i As Integer = Instr(s, Any commentMarkers)
  If i > 0 Then
    s = Left(s, i - 1)
    s = Trim(s) '' removes both leading and trailing whitespace
  End If
End Sub

Dim s(1 To 4) As String = _
{ _
  "apples, pears # and bananas", _
  "apples, pears ; and bananas", _
  "# this is a comment", _
  "  # this is a comment with leading whitespace" _
}

For i As Integer = 1 To 4
  stripComment(s(i), "#;")
  Print s(i), " => Length ="; Len(s(i))
Next

Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

apples, pears  => Length = 13
apples, pears  => Length = 13
               => Length = 0
               => Length = 0

```



## Go


```go
package main

import (
	"fmt"
	"strings"
	"unicode"
)

const commentChars = "#;"

func stripComment(source string) string {
	if cut := strings.IndexAny(source, commentChars); cut >= 0 {
		return strings.TrimRightFunc(source[:cut], unicode.IsSpace)
	}
	return source
}

func main() {
	for _, s := range []string{
		"apples, pears # and bananas",
		"apples, pears ; and bananas",
		"no bananas",
	} {
		fmt.Printf("source:   %q\n", s)
		fmt.Printf("stripped: %q\n", stripComment(s))
	}
}
```

Output:

```txt

source:   "apples, pears # and bananas"
stripped: "apples, pears"
source:   "apples, pears ; and bananas"
stripped: "apples, pears"
source:   "no bananas"
stripped: "no bananas"

```



## Groovy


```groovy
def stripComments = { it.replaceAll(/\s*[#;].*$/, '') }
```

Testing:

```groovy
assert 'apples, pears' == stripComments('apples, pears # and bananas')
assert 'apples, pears' == stripComments('apples, pears ; and bananas')
```



## Haskell


```haskell
ms = ";#"

main = getContents >>=
    mapM_ (putStrLn . takeWhile (`notElem` ms)) . lines
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
# strip_comments:
# return part of string up to first character in 'markers',
# or else the whole string if no comment marker is present
procedure strip_comments (str, markers)
  return str ? tab(upto(markers) | 0)
end

procedure main ()
  write (strip_comments ("apples, pears   and bananas", cset ("#;")))
  write (strip_comments ("apples, pears # and bananas", cset ("#;")))
  write (strip_comments ("apples, pears ; and bananas", cset ("#;")))
end
```


Output:

```txt

apples, pears   and bananas
apples, pears
apples, pears

```



## Inform 7


```inform7
Home is a room.

When play begins:
	strip comments from "apples, pears # and bananas";
	strip comments from "apples, pears ; and bananas";
	end the story.

To strip comments from (T - indexed text):
	say "[T] -> ";
	replace the regular expression "<#;>.*$" in T with "";
	say "[T][line break]".
```


Since square brackets have a special meaning in strings, Inform's regular expression syntax uses angle brackets for character grouping.


## J

'''Solution 1''' (mask & filter):
```j
strip=: dltb@(#~  *./\@:-.@e.&';#')
```

'''Solution 2''' (index & cut):
```j
strip=: dltb@({.~  <./@i.&';#')
```


'''Example''':
```j
   dquote strip '  apples, pears # and bananas'  NB. quote result to show stripped whitespace
"apples, pears"
   strip '  apples, pears ; and bananas'
"apples, pears"
```



## Java


```java
import java.io.*;

public class StripLineComments{
    public static void main( String[] args ){
	if( args.length < 1 ){
	    System.out.println("Usage: java StripLineComments StringToProcess");
	}
	else{
	    String inputFile = args[0];
	    String input = "";
	    try{
		BufferedReader reader = new BufferedReader( new FileReader( inputFile ) );
		String line = "";
		while((line = reader.readLine()) != null){
		    System.out.println( line.split("[#;]")[0] );
		}
	    }
	    catch( Exception e ){
		e.printStackTrace();
	    }
	}
    }
}
```




## JavaScript


### ES5


```javascript
function stripComments(s) {
  var re1 = /^\s+|\s+$/g;  // Strip leading and trailing spaces
  var re2 = /\s*[#;].+$/g; // Strip everything after # or ; to the end of the line, including preceding spaces
  return s.replace(re1,'').replace(re2,'');
}


var s1 = 'apples, pears # and bananas';
var s2 = 'apples, pears ; and bananas';

alert(stripComments(s1) + '\n' + stripComments(s2));

```


A more efficient version that caches the regular expressions in a closure:


```javascript
var stripComments = (function () {
  var re1 = /^\s+|\s+$/g;
  var re2 = /\s*[#;].+$/g;
  return function (s) {
    return s.replace(re1,'').replace(re2,'');
  };
}());

```

A difference with the two versions is that in the first, all declarations are processed before code is executed so the function declaration can be after the code that calls it. However in the second example, the expression creating the function must be executed before the function is available, so it must be before the code that calls it.


### ES6


```javascript
(() => {
    'use strict';

    const main = () => {

        const src = `apples, pears # and bananas
apples, pears ; and bananas`;

        return unlines(
            map(preComment(chars(';#')),
                lines(src)
            )
        );
    };

    // preComment :: [Char] -> String -> String
    const preComment = cs => s =>
        strip(
            takeWhile(
                curry(flip(notElem))(cs),
                s
            )
        );

    // GENERIC FUNCTIONS ------------------------------

    // chars :: String -> [Char]
    const chars = s => s.split('');

    // curry :: ((a, b) -> c) -> a -> b -> c
    const curry = f => a => b => f(a, b);

    // flip :: (a -> b -> c) -> b -> a -> c
    const flip = f => (a, b) => f.apply(null, [b, a]);

    // lines :: String -> [String]
    const lines = s => s.split(/[\r\n]/);

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // notElem :: Eq a => a -> [a] -> Bool
    const notElem = (x, xs) => -1 === xs.indexOf(x);

    // strip :: String -> String
    const strip = s => s.trim();

    // takeWhile :: (a -> Bool) -> [a] -> [a]
    // takeWhile :: (Char -> Bool) -> String -> String
    const takeWhile = (p, xs) => {
        let i = 0;
        const lng = xs.length;
        while ((i < lng) && p(xs[i]))(i = i + 1);
        return xs.slice(0, i);
    };

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
apples, pears
apples, pears
```



## jq

If your version of jq has regex support, the task can be accomplished with the following one-liner:

```jq
sub("[#;].*";"") | sub("^\\s+";"") |  sub("\\s+$";"")
```


Otherwise, we can define strip_comment as a jq filter, as follows. For clarity, the helper functions are presented as top-level functions.


```jq
# define whitespace here as a tab, space, newline, return or form-feed character:
def is_whitespace: . as $in | " \n\r\f\t" | index($in);

def ltrim:
  if .[0:1] | is_whitespace then (.[1:]|ltrim) else . end;

def rtrim:
  if .[length-1:] | is_whitespace then .[0:length-1]|rtrim else . end;

def trim: ltrim | rtrim;

def strip_comment:
  index("#") as $i1 | index(";") as $i2
  | (if $i1 then if $i2 then [$i1, $i2] | min
                 else $i1
                 end
     else $i2
     end ) as $ix
  | if $ix then .[0:$ix] else . end
  | trim;
```


'''Example''':

```jq
" abc ; def # ghi" | strip_comment
```

{{out}}


```sh
"abc"
```



## Julia

<tt>striplinecomment</tt> is designed to be flexible and robust.  By default <tt>#</tt> and <tt>;</tt> are considered comment defining characters, but any characters can be used by passing them as the string <tt>cchars</tt>.  All such characters are escaped in the regular expression used to eliminate comments to allow characters special to the Regex language (e.g. <tt>^</tt>, <tt>$</tt>, <tt>[</tt>) to be used as a comment character.

```Julia

function striplinecomment{T<:String,U<:String}(a::T, cchars::U="#;")
    b = strip(a)
    0 < length(cchars) || return b
    for c in cchars
        r = Regex(@sprintf "\\%c.*" c)
        b = replace(b, r, "")
    end
    strip(b)
end

tests = {"apples, pears # and bananas",
         "apples, pears ; and bananas",
         "  apples, pears & bananas   ",
         " # "}

for t in tests
    s = striplinecomment(t)
    println("Testing \"", t, "\":")
    println("    \"", s, "\"")
end

```


{{out}}

```txt

Testing "apples, pears # and bananas":
    "apples, pears"
Testing "apples, pears ; and bananas":
    "apples, pears"
Testing "  apples, pears & bananas   ":
    "apples, pears & bananas"
Testing " # ":
    ""

```



## Kotlin

For the avoidance of doubt and in line with what the current description of this task appears to be, the following program is designed to remove comments from a string which in Kotlin are:

1. // this is a comment (to the end of the line)

2. /* this is a comment */

3. /* outer comment /* nested comment */ more outer comment */

It then removes whitespace from the beginning and end of the resulting string:

```scala
// version 1.0.6

val r = Regex("""(/\*.*\*/|//.*$)""")

fun stripComments(s: String) = s.replace(r, "").trim()

fun main(args: Array<String>) {
    val strings = arrayOf(
        "apples, pears // and bananas",
        "   apples, pears /* and bananas */",
        "/* oranges */ apples // pears and bananas  ",
        " \toranges /*apples/*, pears*/*/and bananas"
    )
    for (string in strings) println(stripComments(string))
}
```


{{out}}

```txt

apples, pears
apples, pears
apples
oranges and bananas

```



## Liberty BASIC


```lb
string1$ = "apples, pears # and bananas"
string2$ = "pears;, " + chr$(34) + "apples ; " + chr$(34) + " an;d bananas"
commentMarker$ = "; #"
Print parse$(string2$, commentMarker$)
End

Function parse$(string$, commentMarker$)
    For i = 1 To Len(string$)
        charIn$ = Mid$(string$, i, 1)
        If charIn$ = Chr$(34) Then
            inQuotes = Not(inQuotes)
        End If
        If Instr(commentMarker$, charIn$) And (inQuotes = 0) Then Exit For
    next i
    parse$ = Left$(string$, (i - 1))
End Function
```



## Lua


```lua
comment_symbols = ";#"

s1 = "apples, pears # and bananas"
s2 = "apples, pears ; and bananas"

print ( string.match( s1, "[^"..comment_symbols.."]+" ) )
print ( string.match( s2, "[^"..comment_symbols.."]+" ) )
```



## Maple


```Maple>
 use StringTools in map( Trim@Take, [ "\t\t   apples, pears \t# and bananas", " apples, pears ; and bananas  \t" ], "#;" ) end;
                            ["apples, pears", "apples, pears"]
```



## Mathematica


```Mathematica
a = "apples, pears # and bananas
  apples, pears ; and bananas";
b = StringReplace[a, RegularExpression["[ ]+[#;].+[\n]"] -> "\n"];
StringReplace[b, RegularExpression["[ ]+[#;].+$"] -> ""] // FullForm
```


Output:


```txt
"apples, pears\napples, pears"
```


=={{header|MATLAB}} / {{header|Octave}}==

```Matlab
function line = stripcomment(line)
   e = min([find(line=='#',1),find(line==';',1)]);
   if ~isempty(e)
      e = e-1;
      while isspace(line(e)) e = e - 1; end;
      line = line(1:e);
   end;
end;

```

Output:

```txt
>> stripcomment('apples, pears # and bananas\n')
ans = apples, pears
>> stripcomment('apples, pears ; and bananas\n')
ans = apples, pears
```



## MiniScript


```MiniScript
strip = function(test)
    comment = test.indexOf("#")
    if comment == null then comment = test.indexOf(";")
    if comment then test = test[:comment]
    while test[-1] == " "
        test = test - " "
    end while
    return test
end function

print strip("This is a hash test    # a comment") + "."
print strip("This is a semicolon test   ;  a comment") + "."
print strip("This is a no comment test   ") + "."

```

{{out}}

```txt

This is a hash test.
This is a semicolon test.
This is a no comment test.

```



## Nim


```nim
import strutils

proc removeComments(line, sep): string =
  line.split(sep)[0].strip(leading = false)

echo removeComments("apples, pears # and bananas", '#')
echo removeComments("apples, pears ; and bananas", ';')
```



## Objeck


```objeck
use System.IO.File;

class StripComments {
  function : Main(args : String[]) ~ Nil {
    reader : FileReader;
    if(args->Size() = 1) {
      reader := FileReader->New(args[0]);
      line := reader->ReadString();
      while(line <> Nil) {
        index := line->FindLast(';');
        if(index < 0) {
          index := line->FindLast('#');
        };

        if(index > -1) {
          line->SubString(index)->PrintLine();
        };

        line := reader->ReadString();
      };
    };

    leaving {
      if(reader <> Nil) {
        reader->Close();
      };
    };
  }
}
```



## OCaml


```ocaml
let strip_comments str =
  let len = String.length str in
  let rec aux print i =
    if i >= len then () else
    match str.[i] with
    | '#' | ';' ->
        aux false (succ i)
    | '\n' ->
        print_char '\n';
        aux true (succ i)
    | c ->
        if print then print_char c;
        aux print (succ i)
  in
  aux true 0

let () =
  strip_comments "apples, pears # and bananas\n";
  strip_comments "apples, pears ; and bananas\n";
;;
```


or with an imperative style:


```ocaml
let strip_comments =
  let print = ref true in
  String.iter (function
    | ';' | '#' -> print := false
    | '\n' -> print_char '\n'; print := true
    | c -> if !print then print_char c)
```



## Oforth



```Oforth
: stripComments(s, markers)
| firstMarker |
   markers map(#[ s indexOf ]) reduce(#min) ->firstMarker
   s firstMarker ifNotNull: [ left(firstMarker 1 - ) ] strip ;
```


{{out}}

```txt

>stripComments("apples, pears # and bananas", "#") println
apples, pears

>stripComments("apples, pears ;  and bananas # and oranges", "#;") println
apples, pears

```



## Pascal

See [[Strip_comments_from_a_string#Delphi | Delphi]]


## Perl


```perl
while (<>)
  {
    s/[#;].*$//s; # remove comment
    s/^\s+//;     # remove leading whitespace
    s/\s+$//;     # remove trailing whitespace
    print
  }
```



## Perl 6


```perl6
$*IN.slurp.subst(/ \h* <[ # ; ]> \N* /, '', :g).print
```



## Phix

Added a couple of things that can go wrong with something nowhere near sufficiently smart or for that matter language-specific enough.

A line comment inside a block comment (eg " /* left -- right */") could also be very dodgy, and perhaps vice versa.

```Phix
function strip_comments(string s, sequence comments={"#",";"})
    for i=1 to length(comments) do
        integer k = match(comments[i],s)
        if k then
            s = s[1..k-1]
            s = trim_tail(s)
        end if
    end for
    return s
end function

?strip_comments("apples, pears # and bananas")
?strip_comments("apples, pears ; and bananas")
?strip_comments("apples, pears and bananas  ")
?strip_comments("    WS_CAPTION = #00C00000, -- = WS_BORDER+WS_DLGFRAME")
?strip_comments("    WS_CAPTION = #00C00000, -- = WS_BORDER+WS_DLGFRAME",{"--"})
?strip_comments("  title = \"--Title--\"",{"--"})
```

{{out}}

```txt

"apples, pears"
"apples, pears"
"apples, pears and bananas  "
"    WS_CAPTION ="
"    WS_CAPTION = #00C00000,"
"  title = \""

```



## PicoLisp


```PicoLisp
(for Str '("apples, pears # and bananas" "apples, pears ; and bananas")
   (prinl (car (split (chop Str) "#" ";"))) )
```

Output:

```txt

apples, pears
apples, pears

```



## PL/I


```PL/I
k = search(text, '#;');
if k = 0 then put skip list (text);
         else put skip list (substr(text, 1, k-1));
```



## Prolog

{{works with|SWI Prolog}}
This version is implemented as a state automata to strip multiple lines of comments.

```prolog
stripcomment(A,B) :- stripcomment(A,B,a).
stripcomment([A|AL],[A|BL],a) :- \+ A=0';, \+ A=0'# , \+ A=10, \+ A=13 , stripcomment(AL,BL,a).
stripcomment([A|AL],   BL ,a) :-  ( A=0';;    A=0'#), \+ A=10, \+ A=13 , stripcomment(AL,BL,b).
stripcomment([A|AL],   BL ,b) :-                      \+ A=10, \+ A=13 , stripcomment(AL,BL,b).
stripcomment([A|AL],[A|BL],_M):-                       ( A=10;    A=13), stripcomment(AL,BL,a).
stripcomment([],[],_M).
start :-
In = "apples, pears ; and bananas
apples, pears # and bananas",
    stripcomment(In,Out),
    format("~s~n",[Out]).
```

Output:

```txt

?- start.
apples, pears
apples, pears

```

This version uses prolog's pattern matching with two append/3 to strip 1 line.

```prolog
strip_1comment(A,D) :- ((S1=0'#;S1=0';),append(B,[S1|C],A)), \+ ((S2=0'#;S2=0';),append(_X,[S2|_Y],B)) -> B=D; A=D.
```

At the query console:

```txt

?- strip_1comment("apples, pears ; and bananas",O1),format("~s~n",[O1]).
apples, pears
O1 = [97, 112, 112, 108, 101, 115, 44, 32, 112|...] .
?- strip_1comment("apples, pears # and bananas",O1),format("~s~n",[O1]).
apples, pears
O1 = [97, 112, 112, 108, 101, 115, 44, 32, 112|...] .

```



## PureBasic


```PureBasic
Procedure.s Strip_comments(Str$)
  Protected result$=Str$, l, l1, l2
  l1 =FindString(Str$,"#",1)
  l2 =FindString(Str$,";",1)
  ;
  ; See if any comment sign was found, prioritizing '#'
  If l1
    l=l1
  ElseIf l2
    l=l2
  EndIf
  l-1
  If l>0
    result$=Left(Str$,l)
  EndIf
  ProcedureReturn result$
EndProcedure
```

Implementation

```PureBasic
#instring1 ="apples, pears # and bananas"
#instring2 ="apples, pears ; and bananas"

PrintN(Strip_comments(#instring1))
PrintN(Strip_comments(#instring2))
```

Output:
```txt

apples, pears
apples, pears

```



## Python


### Procedural


```python
def remove_comments(line, sep):
    for s in sep:
        i = line.find(s)
        if i >= 0:
            line = line[:i]
    return line.strip()

# test
print remove_comments('apples ; pears # and bananas', ';#')
print remove_comments('apples ; pears # and bananas', '!')

```



### Regular expressions

You could also use a regular expression

```python
import re

m = re.match(r'^([^#]*)#(.*)$', line)
if m:  # The line contains a hash / comment
    line = m.group(1)

```



### Functional


We can specify the line prefixes to retain in terms of '''itertools.takewhile''',
which is defined over strings as well as lists.
{{Works with|Python|3.7}}

```python
'''Comments stripped with itertools.takewhile'''

from itertools import takewhile


# stripComments :: [Char] -> String -> String
def stripComments(cs):
    '''The lines of the input text, with any
       comments (defined as starting with one
       of the characters in cs) stripped out.
    '''
    def go(cs):
        return lambda s: ''.join(
            takewhile(lambda c: c not in cs, s)
        ).strip()
    return lambda txt: '\n'.join(map(
        go(cs),
        txt.splitlines()
    ))


if __name__ == '__main__':
    print(
        stripComments(';#')(
            '''apples, pears # and bananas
               apples, pears ; and bananas
            '''
        )
    )
```

{{Out}}

```txt
apples, pears
apples, pears
```



## R

This is most cleanly accomplished using the <code>stringr</code> package.


```r
strip_comments <- function(str)
{
  if(!require(stringr)) stop("you need to install the stringr package")
  str_trim(str_split_fixed(str, "#|;", 2)[, 1])
}
```


Example usage:


```r
x <-c(
  "apples, pears # and bananas",       # the requested hash test
  "apples, pears ; and bananas",       # the requested semicolon test
  "apples, pears   and bananas",       # without a comment
  " apples, pears # and bananas"       # with preceding spaces
)
strip_comments(x)
```



## Racket



```Racket

#lang at-exp racket

(define comment-start-rx "[;#]")

(define text
  @~a{apples, pears # and bananas
      apples, pears ; and bananas
      })

(define (strip-comments text [rx comment-start-rx])
  (string-join
   (for/list ([line (string-split text "\n")])
     (string-trim line (pregexp (~a "\\s*" rx ".*")) #:left? #f))
   "\n"))

;; Alternatively, do it in a single regexp operation
(define (strip-comments2 text [rx comment-start-rx])
  (regexp-replace* (pregexp (~a "(?m:\\s*" rx ".*)")) text ""))

(strip-comments2 text) ; -> "apples, pears\napples, pears"

```



## Red

Red has an embedded parse engine called Parse.
For more info on the Parse dialect. http://www.red-lang.org/2013/11/041-introducing-parse.html.

```Red

>> parse s: "apples, pears ; and bananas" [to [any space ";"] remove thru end]
== true
>> s
== "apples, pears"

```


But you can also use simple series operations to find where something occurs, clear from that position, and trim leading and trailing spaces.

```Red

s: "apples, pears ; and bananas"
dlms: charset "#;"

trim head clear find s dlms
== "apples, pears"

s: "apples, pears # and bananas"

trim head clear find s dlms
== "apples, pears"

```



## REXX


### version 1

The first REXX subroutine takes advantage of the fact that there are only two single-character delimiters:
::*   <big>#</big>   (hash or pound sign),
::*   <big>;</big>     (a semicolon).
The second and third subroutines take a general approach to the (number of) delimiters,

the third subroutine is more straightforward and reads better.

The fourth subroutine is similar to the third version but more idiomatic.


All four subroutines trim leading   ''and''   trailing blanks after stripping the "comments".

```rexx
/*REXX program  strips  a string  delineated  by a  hash (#)   or   a  semicolon (;).   */
old1= ' apples, pears # and bananas'      ;      say '                 old ───►'old1"◄───"
new1= stripCom1(old1)                     ;      say '     1st version new ───►'new1"◄───"
new2= stripCom2(old1)                     ;      say '     2nd version new ───►'new2"◄───"
new3= stripCom3(old1)                     ;      say '     3rd version new ───►'new3"◄───"
new4= stripCom4(old1)                     ;      say '     4th version new ───►'new4"◄───"
                                                 say  copies('▒', 62)
old2= ' apples, pears ; and bananas'      ;      say '                 old ───►'old2"◄───"
new1= stripCom1(old2)                     ;      say '     1st version new ───►'new1"◄───"
new2= stripCom2(old2)                     ;      say '     2nd version new ───►'new2"◄───"
new3= stripCom3(old2)                     ;      say '     3rd version new ───►'new3"◄───"
new4= stripCom4(old2)                     ;      say '     4th version new ───►'new4"◄───"
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
stripCom1: procedure;   parse arg x              /*obtain the argument (the  X  string).*/
           x=translate(x, '#', ";")              /*translate semicolons to a hash (#).  */
           parse  var  x    x  '#'               /*parse the X string,  ending in hash. */
           return strip(x)                       /*return the stripped shortened string.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
stripCom2: procedure;   parse arg x              /*obtain the argument (the  X  string).*/
           d= ';#'                               /*this is the delimiter list to be used*/
           d1=left(d,1)                          /*get the first character in delimiter.*/
           x=translate(x,copies(d1,length(d)),d) /*translates delimiters ──►  1st delim.*/
           parse  var  x    x  (d1)              /*parse the string,  ending in a hash. */
           return strip(x)                       /*return the stripped shortened string.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
stripCom3: procedure;   parse arg x              /*obtain the argument (the  X  string).*/
           d= ';#'                               /*this is the delimiter list to be used*/
                           do j=1  for length(d) /*process each of the delimiters singly*/
                           _=substr(d,j,1)       /*use only one delimiter at a time.    */
                           parse  var  x  x  (_) /*parse the  X  string for each delim. */
                           end   /*j*/           /* [↑]    (_)  means stop parsing at _ */
           return strip(x)                       /*return the stripped shortened string.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
stripCom4: procedure;   parse arg x              /*obtain the argument (the  X  string).*/
           d= ';#'                               /*this is the delimiter list to be used*/
                    do k=1  for length(d)        /*process each of the delimiters singly*/
                    p=pos(substr(d,k,1), x)      /*see if a delimiter is in the X string*/
                    if p\==0  then x=left(x,p-1) /*shorten the X string by one character*/
                    end   /*k*/                  /* [↑]  If p==0, then char wasn't found*/
           return strip(x)                       /*return the stripped shortened string.*/
```

'''output'''

```txt

                 old ───► apples, pears # and bananas◄───
     1st version new ───►apples, pears◄───
     2nd version new ───►apples, pears◄───
     3rd version new ───►apples, pears◄───
     4th version new ───►apples, pears◄───
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
                 old ───► apples, pears ; and bananas◄───
     1st version new ───►apples, pears◄───
     2nd version new ───►apples, pears◄───
     3rd version new ───►apples, pears◄───
     4th version new ───►apples, pears◄───

```



### version 2


```rexx
Call stripd ' apples, pears # and bananas'
Call stripd ' apples, pears and bananas'
Exit
stripd:
  Parse Arg old
  dlist='#;'                        /* delimiter list             */
  p=verify(old,dlist,'M')          /* find position of delimiter */
  If p>0 Then                       /* delimiter found            */
    new=strip(left(old,p-1))
  Else
    new=strip(old)
  Say '>'old'<'
  Say '>'new'<'
  Return
```



## Ring


```ring

aList = 'apples, pears # and bananas'
see aList + nl
see stripComment(aList) + nl
aList = 'apples, pears // and bananas'
see aList + nl
see stripComment(aList) + nl

func stripComment bList
     nr = substr(bList,"#")
     if nr > 0 cList = substr(bList,1,nr-1) ok
     nr = substr(bList,"//")
     if nr > 0 cList = substr(bList,1,nr-1) ok
     return cList

```



## Ruby


```ruby
class String
  def strip_comment( markers = ['#',';'] )
    re = Regexp.union( markers ) # construct a regular expression which will match any of the markers
    if index = (self =~ re)
      self[0, index].rstrip      # slice the string where the regular expression matches, and return it.
    else
      rstrip
    end
  end
end

p 'apples, pears # and bananas'.strip_comment
str = 'apples, pears ; and bananas'
p str.strip_comment
str = 'apples, pears and bananas '
p str.strip_comment
p str.strip_comment('and')
p " \t \n ;".strip_comment
p "".strip_comment
```

{{out}}

```txt

"apples, pears"
"apples, pears"
"apples, pears and bananas"
"apples, pears"
""
""

```



## Rust


```rust
fn strip_comment<'a>(input: &'a str, markers: &[char]) -> &'a str {
    input
        .find(markers)
        .map(|idx| &input[..idx])
        .unwrap_or(input)
        .trim()
}

fn main() {
    println!("{:?}", strip_comment("apples, pears # and bananas", &['#', ';']));
    println!("{:?}", strip_comment("apples, pears ; and bananas", &['#', ';']));
    println!("{:?}", strip_comment("apples, pears and bananas ", &['#', ';']));
}
```

{{out}}

```txt

"apples, pears"
"apples, pears"
"apples, pears and bananas"

```



## Scala


```scala
object StripComments {
  def stripComments1(s:String, markers:String =";#")=s takeWhile (!markers.contains(_)) trim

  // using regex and pattern matching
  def stripComments2(s:String, markers:String =";#")={
    val R=("(.*?)[" + markers + "].*").r
    (s match {
      case R(line) => line
      case _ => s
    }) trim
  }

  def print(s:String)={
    println("'"+s+"' =>")
    println("   '"+stripComments1(s)+"'")
    println("   '"+stripComments2(s)+"'")
  }

  def main(args: Array[String]): Unit = {
    print("apples, pears # and bananas")
    print("apples, pears ; and bananas")
  }
}
```

Output:

```txt
'apples, pears # and bananas' =>
   'apples, pears'
   'apples, pears'
'apples, pears ; and bananas' =>
   'apples, pears'
   'apples, pears'
```



## Scheme

{{works with|Guile}}

```scheme
(use-modules (ice-9 regex))

(define (strip-comments s)
    (regexp-substitute #f
        (string-match "[ \t\r\n\v\f]*[#;].*" s) 'pre "" 'post))

(display (strip-comments "apples, pears # and bananas"))(newline)
(display (strip-comments "apples, pears ; and bananas"))(newline)
```

Output:
```txt
apples, pears
apples, pears
```



## sed


```bash
#!/bin/sh
# Strip comments
echo "$1" | sed 's/ *[#;].*$//g' | sed 's/^ *//'
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func string: stripComment (in string: line) is func
  result
    var string: lineWithoutComment is "";
  local
    var integer: lineEnd is 0;
    var integer: pos is 0;
  begin
    lineEnd := length(line);
    for pos range 1 to length(line) do
      if line[pos] in {'#', ';'} then
        lineEnd := pred(pos);
        pos := length(line);
      end if;
    end for;
    lineWithoutComment := line[.. lineEnd];
  end func;

const proc: main is func
  local
    var string: stri is "apples, pears # and bananas\n\
                        \apples, pears ; and bananas";
    var string: line is ""
  begin
    writeln(stri);
    writeln("====>");
    for line range split(stri, '\n') do
      writeln(stripComment(line));
    end for;
  end func;
```


Output:

```txt

apples, pears # and bananas
apples, pears ; and bananas
====>
apples, pears
apples, pears

```



## Sidef


```ruby
func strip_comment(s) {
    (s - %r'[#;].*').strip;
}

[" apples, pears # and bananas",
 " apples, pears ; and bananas",
 " apples, pears "].each { |s|
    say strip_comment(s).dump;
};
```

{{out}}

```txt

"apples, pears"
"apples, pears"
"apples, pears"

```



## Tcl


```tcl
proc stripLineComments {inputString {commentChars ";#"}} {
    # Switch the RE engine into line-respecting mode instead of the default whole-string mode
    regsub -all -line "\[$commentChars\].*$" $inputString "" commentStripped
    # Now strip the whitespace
    regsub -all -line {^[ \t\r]*(.*\S)?[ \t\r]*$} $commentStripped {\1}
}
```

Demonstration:

```tcl
# Multi-line string constant
set input "apples, pears # and bananas
apples, pears ; and bananas"
# Do the stripping
puts [stripLineComments $input]
```

Output:

```txt

apples, pears
apples, pears

```

The above code has one issue though; it's notion of a set of characters is very much that of the RE engine. That's possibly desirable, but to handle ''any'' sequence of characters as a set of separators requires a bit more cleverness.

```tcl
proc stripLineComments {inputString {commentChars ";#"}} {
    # Convert the character set into a transformation
    foreach c [split $commentChars ""] {lappend map $c "\uFFFF"}; # *very* rare character!
    # Apply transformation and then use a simpler constant RE to strip
    regsub -all -line {\uFFFF.*$} [string map $map $inputString] "" commentStripped
    # Now strip the whitespace
    regsub -all -line {^[ \t\r]*(.*\S)?[ \t\r]*$} $commentStripped {\1}
}
```

Output in the example is the same as above.


## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
strngcomment=*
DATA apples, pears # and bananas
DATA apples, pears ; and bananas

BUILD S_TABLE comment_char="|#|;|"

LOOP s=strngcomment
x=SPLIT (s,comment_char,string,comment)
PRINT string
ENDLOOP
```

Output:

```txt

apples, pears
apples, pears

```



## UNIX Shell

{{works with|bash}}
{{works with|pdksh}}
Adapted from the Advanced Bash-Scripting Guide, section 10.1 [http://tldp.org/LDP/abs/html/string-manipulation.html Manipulating Strings].

```bash
bash$ a='apples, pears ; and bananas'
bash$ b='apples, pears # and bananas'
bash$ echo ${a%%;*}
apples, pears
bash$ echo ${b%%#*}
apples, pears
bash$
```



## VBScript


```vb

Function strip_comments(s,char)
	If InStr(1,s,char) > 0 Then
		arr = Split(s,char)
		strip_comments = RTrim(arr(0))
	Else
		strip_comments = s
	End If
End Function

WScript.StdOut.WriteLine strip_comments("apples, pears # and bananas","#")
WScript.StdOut.WriteLine strip_comments("apples, pears ; and bananas",";")

```



## zkl


```zkl
fcn strip(text,c){  // if c in text, remove it and following text
   if (Void!=(n:=text.find(c))) text=text[0,n];
   text.strip()		// remove leading and trailing white space
}
fcn stripper(text,a,b,c,etc){ // strip a,b,c,etc from text
   foreach c in (vm.arglist[1,*]){ text=strip(text,c) }
   text
}
```

Or, if you want the all-in-one stripper:

```zkl
fcn stripper(text,a,b,c,etc){
   vm.arglist[1,*].reduce('wrap(text,c){
      if (Void!=(n:=text.find(c))) text[0,n] else text
   },text)
   .strip()
}
```


```zkl
String(">", strip(" apples, pears # and bananas","#"), "<").println();
String(">", stripper(" apples, pears ; and # bananas","#",";"), "<").println();
```

{{out}}

```txt

>apples, pears<
>apples, pears<

```


{{omit from|Bc}}
{{omit from|Openscad}}
{{omit from|PARI/GP|No real capacity for string manipulation}}
