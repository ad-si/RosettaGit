+++
title = "Strip a set of characters from a string"
description = ""
date = 2019-07-26T14:01:39Z
aliases = []
[extra]
id = 9877
[taxonomies]
categories = ["task", "String manipulation"]
tags = []
+++

## Task

Create a function that strips a set of characters from a string.


The function should take two arguments:
:::#   a string to be stripped
:::#   a string containing the set of characters to be stripped



The returned string should contain the first string, stripped of any characters in the second argument:

```pseudocode
 print stripchars("She was a soul stripper. She took my heart!","aei")
Sh ws  soul strppr. Sh took my hrt!
```






## 360 Assembly

The program uses two ASSIST macro (XDECO,XPRNT) to keep the code as short as possible.

```360asm
*        Strip a set of characters from a string  07/07/2016
STRIPCH  CSECT
         USING  STRIPCH,R13        base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         " <-
         ST     R15,8(R13)         " ->
         LR     R13,R15            " addressability
         LA     R1,PARMLIST        parameter list
         BAL    R14,STRIPCHR       c3=stripchr(c1,c2)
         LA     R2,PG              @pg
         LH     R3,C3              length(c3)
         LA     R4,C3+2            @c3
         LR     R5,R3              length(c3)
         MVCL   R2,R4              pg=c3
         XPRNT  PG,80              print buffer
         L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    " restore
         XR     R15,R15            " rc=0
         BR     R14                exit
PARMLIST DC     A(C3)              @c3
         DC     A(C1)              @c1
         DC     A(C2)              @c2
C1       DC     H'43',CL62'She was a soul stripper. She took my heart!'
C2       DC     H'3',CL14'aei'     c2      [varchar(14)]
C3       DS     H,CL62             c3      [varchar(62)]
PG       DC     CL80' '            buffer  [char(80)]
*------- stripchr -----------------------------------------------------
STRIPCHR L      R9,0(R1)           @parm1
         L      R2,4(R1)           @parm2
         L      R3,8(R1)           @parm3
         MVC    PHRASE(64),0(R2)   phrase=parm2
         MVC    REMOVE(16),0(R3)   remove=parm3
         SR     R8,R8              k=0
         LA     R6,1               i=1
LOOPI    CH     R6,PHRASE          do i=1 to length(phrase)
         BH     ELOOPI             "
         LA     R4,PHRASE+1          @phrase
         AR     R4,R6                +i
         MVC    CI(1),0(R4)          ci=substr(phrase,i,1)
         MVI    OK,X'01'             ok='1'B
         LA     R7,1                 j=1
LOOPJ    CH     R7,REMOVE            do j=1 to length(remove)
         BH     ELOOPJ               "
         LA     R4,REMOVE+1            @remove
         AR     R4,R7                  +j
         MVC    CJ,0(R4)               cj=substr(remove,j,1)
         CLC    CI,CJ                  if ci=cj
         BNE    CINECJ                 then
         MVI    OK,X'00'                 ok='0'B
         B      ELOOPJ                   leave j
CINECJ   LA     R7,1(R7)               j=j+1
         B      LOOPJ                end do j
ELOOPJ   CLI    OK,X'01'             if ok
         BNE    NOTOK                then
         LA     R8,1(R8)               k=k+1
         LA     R4,RESULT+1            @result
         AR     R4,R8                  +k
         MVC    0(1,R4),CI             substr(result,k,1)=ci
NOTOK    LA     R6,1(R6)           i=i+1
         B      LOOPI              end do i
ELOOPI   STH    R8,RESULT          length(result)=k
         MVC    0(64,R9),RESULT    return(result)
         BR     R14                return to caller
CI       DS     CL1                ci      [char(1)]
CJ       DS     CL1                cj      [char(1)]
OK       DS     X                  ok      [boolean]
PHRASE   DS     H,CL62             phrase  [varchar(62)]
REMOVE   DS     H,CL14             remove  [varchar(14)]
RESULT   DS     H,CL62             result  [varchar(62)]
*        ----   -------------------------------------------------------
         YREGS
         END    STRIPCH
```

```txt

Sh ws  soul strppr. Sh took my hrt!

```



## Ada


```Ada
with Ada.Text_IO;

procedure Strip_Characters_From_String is

   function Strip(The_String: String; The_Characters: String)
                  return String is
      Keep:   array (Character) of Boolean := (others => True);
      Result: String(The_String'Range);
      Last:   Natural := Result'First-1;
   begin
      for I in The_Characters'Range loop
         Keep(The_Characters(I)) := False;
      end loop;
      for J in The_String'Range loop
         if Keep(The_String(J)) then
            Last := Last+1;
            Result(Last) := The_String(J);
         end if;
      end loop;
      return Result(Result'First .. Last);
   end Strip;

   S: String := "She was a soul stripper. She took my heart!";

begin -- main
   Ada.Text_IO.Put_Line(Strip(S, "aei"));
end Strip_Characters_From_String;
```

```txt
> ./strip_characters_from_string
Sh ws  soul strppr. Sh took my hrt!
```



## Aime


```aime
text
stripchars1(data b, text w)
{
    integer p;

    p = b.look(0, w);
    while (p < ~b) {
        b.delete(p);
        p += b.look(p, w);
    }

     b;
}

text
stripchars2(data b, text w)
{
    b.drop(w);
}

integer
main(void)
{
    o_text(stripchars1("She was a soul stripper. She took my heart!", "aei"));
    o_newline();

    o_text(stripchars2("She was a soul stripper. She took my heart!", "aei"));
    o_newline();

    return 0;
}
```



## ALGOL 68

```algol68
#!/usr/local/bin/a68g --script #

PROC strip chars = (STRING mine, ore)STRING: (
  STRING out := "";
  FOR i FROM LWB mine TO UPB mine DO
    IF NOT char in string(mine[i], LOC INT, ore) THEN
      out +:= mine[i]
    FI
  OD;
  out[@LWB mine]
);

printf(($gl$,stripchars("She was a soul stripper. She took my heart!","aei")))
```

```txt

Sh ws  soul strppr. Sh took my hrt!

```



## ALGOL W


```algolw
begin
    % returns s with the characters in remove removed                         %
    %     as all strings in Algol W are fixed length, the length of remove    %
    %     must be specified in removeLength                                   %
    string(256) procedure stripCharacters( string(256) value s, remove
                                         ; integer     value removeLength
                                         ) ;
    begin
        string(256) resultText;
        integer     tPos;
        resultText := " ";
        tPos := 0;
        for sPos := 0 until 255 do begin
            logical   keepCharacter;
            string(1) c;
            c             := s( sPos // 1 );
            keepCharacter := true;
            for rPos := 0 until removeLength - 1 do begin
                if remove( rPos // 1 ) = c then begin
                    % have a character that should be removed                 %
                    keepCharacter := false;
                    goto endSearch
                end if_have_a_character_to_remove ;
            end for_rPos ;
endSearch:
            if keepCharacter then begin
                resultText( tPos // 1 ) := c;
                tPos                    := tPos + 1
            end if_keepCharacter
        end for_sPos ;
        resultText
    end stripCharacters ;
    % task test case                                                          %
    begin
        string(256) ex, stripped;
        ex       := "She was a soul stripper. She took my heart!";
        stripped := stripCharacters( ex, "aei", 3 );
        write( "text: ",       ex( 0 // 64 ) );
        write( "  ->: ", stripped( 0 // 64 ) )
    end
end.
```

```txt

text: She was a soul stripper. She took my heart!
  ->: Sh ws  soul strppr. Sh took my hrt!

```



## AppleScript



### Using text item delimiters


```AppleScript
stripChar("She was a soul stripper. She took my heart!", "aei")

on stripChar(str, chrs)
    tell AppleScript
        set oldTIDs to text item delimiters
        set text item delimiters to characters of chrs
        set TIs to text items of str
        set text item delimiters to ""
        set str to TIs as string
        set text item delimiters to oldTIDs
    end tell
    return str
end stripChar
```

```txt

"Sh ws  soul strppr. Sh took my hrt!"

```



### By functional composition


### =Without regex=

(Following the Haskell contribution in reversing the argument order to the sequence more probable in a context of potential currying or partial application).


```AppleScript
-- stripChars :: String -> String -> String
on stripChars(strNeedles, strHaystack)
    script notNeedles
        on |λ|(x)
            notElem(x, strNeedles)
        end |λ|
    end script

    intercalate("", filter(notNeedles, strHaystack))
end stripChars


-- TEST ---------------------------------------------------------------------------
on run

    stripChars("aei", "She was a soul stripper. She took my heart!")

    --> "Sh ws  soul strppr. Sh took my hrt!"
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------------------------

-- filter :: (a -> Bool) -> [a] -> [a]
on filter(f, xs)
    tell mReturn(f)
        set lst to {}
        set lng to length of xs
        repeat with i from 1 to lng
            set v to item i of xs
            if |λ|(v, i, xs) then set end of lst to v
        end repeat
        return lst
    end tell
end filter

-- notElem :: Eq a => a -> [a] -> Bool
on notElem(x, xs)
    xs does not contain x
end notElem

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

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
```

```txt
"Sh ws  soul strppr. Sh took my hrt!"
```



### =With regex=

OS X Yosemite onwards – importing the Foundation classes to use  NSRegularExpression


```AppleScript
use framework "Foundation"


-- stripChars :: String -> String -> String
on stripChars(strNeedles, strHaystack)

    intercalate("", splitRegex("[" & strNeedles & "]", strHaystack))

end stripChars


-- TEST ------------------------------------------------------------------------
on run

    stripChars("aei", "She was a soul stripper. She took my heart!")

    --> "Sh ws  soul strppr. Sh took my hrt!"
end run


-- GENERIC FUNCTIONS -----------------------------------------------------------

-- splitRegex :: RegexPattern -> String -> [String]
on splitRegex(strRegex, str)
    set lstMatches to regexMatches(strRegex, str)
    if length of lstMatches > 0 then
        script preceding
            on |λ|(a, x)
                set iFrom to start of a
                set iLocn to (location of x)

                if iLocn > iFrom then
                    set strPart to text (iFrom + 1) thru iLocn of str
                else
                    set strPart to ""
                end if
                {parts:parts of a & strPart, start:iLocn + (length of x) - 1}
            end |λ|
        end script

        set recLast to foldl(preceding, {parts:[], start:0}, lstMatches)

        set iFinal to start of recLast
        if iFinal < length of str then
            parts of recLast & text (iFinal + 1) thru -1 of str
        else
            parts of recLast & ""
        end if
    else
        {str}
    end if
end splitRegex

-- regexMatches :: RegexPattern -> String -> [{location:Int, length:Int}]
on regexMatches(strRegex, str)
    set ca to current application
    set oRgx to ca's NSRegularExpression's regularExpressionWithPattern:strRegex ¬
        options:((ca's NSRegularExpressionAnchorsMatchLines as integer)) |error|:(missing value)
    set oString to ca's NSString's stringWithString:str
    set oMatches to oRgx's matchesInString:oString options:0 range:{location:0, |length|:oString's |length|()}

    set lstMatches to {}
    set lng to count of oMatches
    repeat with i from 1 to lng
        set end of lstMatches to range() of item i of oMatches
    end repeat
    lstMatches
end regexMatches

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

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

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
```

```txt
"Sh ws  soul strppr. Sh took my hrt!"
```



## Applesoft BASIC


```basic
100  LET S$ = "SHE WAS A SOUL STRIPPER. SHE TOOK MY HEART!"
110  LET RM$ = "AEI"
120  GOSUB 200STRIPCHARS
130  PRINT SC$
190  END
200  REM
210  REM STRIPCHARS
220  REM
230  LET SC$ = ""
240  LET SL =  LEN (S$)
250  IF SL = 0 THEN  RETURN
260  FOR SI = 1 TO SL
270  LET SM$ =  MID$ (S$,SI,1)
280  FOR SJ = 1 TO  LEN (RM$)
290  LET SR$ =  MID$ (RM$,SJ,1)
300  LET ST = SR$ <  > SM$
310  IF ST THEN  NEXT SJ
320  IF ST THEN SC$ = SC$ + SM$
330  NEXT SI
340  RETURN

```

```txt
SH WS  SOUL STRPPR. SH TOOK MY HRT!
```



## AutoHotkey


```AutoHotkey
MsgBox % stripchars("She was a soul stripper. She took my heart!","aei")

StripChars(string, charsToStrip){
   Loop Parse, charsToStrip
      StringReplace, string, string, % A_LoopField, , All
   return string
}
```

```txt
Sh ws  soul strppr. Sh took my hrt!
```



## AWK


```AWK
#!/usr/bin/awk -f
BEGIN {
   x = "She was a soul stripper. She took my heart!";
   print x;
   gsub(/[aei]/,"",x);
   print x;
}
```

```txt
She was a soul stripper. She took my heart!
Sh ws  soul strppr. Sh took my hrt!

```



## BASIC

```qbasic
DECLARE FUNCTION stripchars$(src AS STRING, remove AS STRING)

PRINT stripchars$("She was a soul stripper. She took my heart!", "aei")

FUNCTION stripchars$(src AS STRING, remove AS STRING)
    DIM l0 AS LONG, t AS LONG, s AS STRING
    s = src
    FOR l0 = 1 TO LEN(remove)
        DO
            t = INSTR(s, MID$(remove, l0, 1))
            IF t THEN
                s = LEFT$(s, t - 1) + MID$(s, t + 1)
            ELSE
                EXIT DO
            END IF
        LOOP
    NEXT
    stripchars$ = s
END FUNCTION
```

 Sh ws  soul strppr. Sh took my hrt!

==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Stripchr.bas"
110 PRINT STRIPCHARS$("She was a soul stripper. She took my heart!","aei")
120 DEF STRIPCHARS$(SRC$,REMOVE$)
130   LET T$=""
140   FOR I=1 TO LEN(SRC$)
150     LET L=0
160     FOR J=1 TO LEN(REMOVE$)
170       IF SRC$(I)=REMOVE$(J) THEN LET L=1:EXIT FOR
180     NEXT
190     IF L=0 THEN LET T$=T$&SRC$(I)
200   NEXT
210   LET STRIPCHARS$=T$
220 END DEF
```


=
## Sinclair ZX81 BASIC
=
Works with 1k of RAM. Since the ZX81 character set includes neither lower case nor <tt>!</tt>, the test string is not quite identical to the one suggested in the specification.

```basic
 10 LET A$="SHE WAS A SOUL STRIPPER. SHE TOOK MY HEART."
 20 LET B$="AEI"
 30 GOSUB 60
 40 PRINT C$
 50 STOP
 60 LET C$=""
 70 FOR I=1 TO LEN A$
 80 LET J=1
 90 IF A$(I)=B$(J) THEN GOTO 130
100 LET J=J+1
110 IF J<=LEN B$ THEN GOTO 90
120 LET C$=C$+A$(I)
130 NEXT I
140 RETURN
```

```txt
SH WS  SOUL STRPPR. SH TOOK MY HRT.
```



See also: [[#Liberty BASIC|Liberty BASIC]], [[#PureBasic|PureBasic]]


## BBC BASIC


```bbcbasic
      PRINT FNstripchars("She was a soul stripper. She took my heart!", "aei")
      END

      DEF FNstripchars(A$, S$)
      LOCAL I%, C%, C$
      FOR I% = 1 TO LEN(S$)
        C$ = MID$(S$, I%, 1)
        REPEAT
          C% = INSTR(A$, C$)
          IF C% A$ = LEFT$(A$, C%-1) + MID$(A$, C%+1)
        UNTIL C% = 0
      NEXT
      = A$
```

Output:

```txt

Sh ws  soul strppr. Sh took my hrt!

```



## Bracmat

This solution handles Unicode (utf-8) characters. Optimizations are: (1) the <code>chars</code> string is hard-coded into the pattern before the pattern is used in the match expression, (2) the output characters are stacked (cheap) rather than appended (expensive). The result string is obtained by stringizing the stack and reversing. To make multibyte characters survive, they are reversed before being put onto the stack. A problem is that this code is negligent of diacritical marks.

```bracmat
( ( strip
  =   string chars s pat
    .     !arg:(?string.?chars)
        & :?s
        &
            ' ( ?
                ( %
                : [%( utf$!sjt
                    & ( @($chars:? !sjt ?)
                      | rev$!sjt !s:?s
                      )
                    & ~
                    )
                )
                ?
              )
          : (=?pat)
        & @(!string:!pat)
      | rev$(str$!s)
  )
&   out
  $ (strip$("Аппетит приходит во время еды".веп)
);
```

```txt
Атит риходит о рмя ды
```



## Burlesque



```burlesque

blsq ) "She was a soul stripper. She took my heart!"{"aei"\/~[n!}f[
"Sh ws  soul strppr. Sh took my hrt!"

```



## C


```c
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

  /* removes all chars from string */
char *strip_chars(const char *string, const char *chars)
{
  char * newstr = malloc(strlen(string) + 1);
  int counter = 0;

  for ( ; *string; string++) {
    if (!strchr(chars, *string)) {
      newstr[ counter ] = *string;
      ++ counter;
    }
  }

  newstr[counter] = 0;
  return newstr;
}

int main(void)
{
  char *new = strip_chars("She was a soul stripper. She took my heart!", "aei");
  printf("%s\n", new);

  free(new);
  return 0;
}
```

```txt
Sh ws  soul strppr. Sh took my hrt!
```



### With table lookup


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *strip(const char * str, const char *pat)
{
	/*  char replacement is typically done with lookup tables if
	 *  the replacement set can be large: it turns O(m n) into
	 *  O(m + n).
	 *  If same replacement pattern is going to be applied to many
	 *  strings, it's better to build a table beforehand and reuse it.
	 *  If charset is big like unicode, table needs to be implemented
	 *  more efficiently, say using bit field or hash table -- it
	 *  all depends on the application.
	 */
	int i = 0, tbl[128] = {0};
	while (*pat != '\0') tbl[(int)*(pat++)] = 1;

	char *ret = malloc(strlen(str) + 1);
	do {
		if (!tbl[(int)*str])
			ret[i++] = *str;
	} while (*(str++) != '\0');

	/*  assuming realloc is efficient and succeeds; if not, we could
	 *  do a two-pass, count first, alloc and strip second
	 */
	return realloc(ret, i);
}

int main()
{
	char * x = strip("She was a soul stripper. She took my heart!", "aei");
	printf(x);
	free(x);

	return 0;
}
```
Output same as above.


## C++

```cpp
#include <algorithm>
#include <iostream>
#include <string>

std::string stripchars(std::string str, const std::string &chars)
{
    str.erase(
        std::remove_if(str.begin(), str.end(), [&](char c){
            return chars.find(c) != std::string::npos;
        }),
        str.end()
    );
    return str;
}

int main()
{
    std::cout << stripchars("She was a soul stripper. She took my heart!", "aei") << '\n';
    return 0;
}
```

```txt

Sh ws  soul strppr. Sh took my hrt!

```



## C#


```c#
using System;

public static string RemoveCharactersFromString(string testString, string removeChars)
{
    char[] charAry = removeChars.ToCharArray();
    string returnString = testString;
    foreach (char c in charAry)
    {
        while (returnString.IndexOf(c) > -1)
        {
            returnString = returnString.Remove(returnString.IndexOf(c), 1);
        }
    }
    return returnString;
}
```

Usage:

```C sharp

using System;

class Program
{
    static void Main(string[] args)
    {
        string testString = "She was a soul stripper. She took my heart!";
        string removeChars = "aei";
        Console.WriteLine(RemoveCharactersFromString(testString, removeChars));
    }
}

```

```txt
Sh ws  soul strppr. Sh took my hrt!
```

Using <code>Regex</code>:

```C sharp

using System;
using System.Text.RegularExpressions;

private static string RegexRemoveCharactersFromString(string testString, string removeChars)
{
    string pattern = "[" + removeChars + "]";
    return Regex.Replace(testString, pattern, "");
}
```



## Clojure


```Clojure
(defn strip [coll chars]
  (apply str (remove #((set chars) %) coll)))

(strip "She was a soul stripper. She took my heart!" "aei")
;; => "Sh ws  soul strppr. Sh took my hrt!"
```



## COBOL

This function takes the two arguments as specified in the task. However, the result will be returned in the string that had the characters stripped from it, and the string containing the characters to strip must be null-terminated (otherwise, a table would have to be used instead).

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Strip-Chars.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Str-Size  CONSTANT 128.

       LOCAL-STORAGE SECTION.
       01  I       PIC 999.
       01  Str-Pos PIC 999.

       01  Offset  PIC 999.
       01  New-Pos PIC 999.

       01  Str-End PIC 999.

       LINKAGE SECTION.
       01  Str     PIC X(Str-Size).
       01  Chars-To-Replace PIC X(256).

       PROCEDURE DIVISION USING Str BY VALUE Chars-To-Replace.
       Main.
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL Chars-To-Replace (I:1) = X"00"

               MOVE ZERO TO Offset

*              *> Overwrite the characters to remove by left-shifting
*              *> following characters over them.
               PERFORM VARYING Str-Pos FROM 1 BY 1
                       UNTIL Str-Size < Str-Pos
                   IF Str (Str-Pos:1) = Chars-To-Replace (I:1)
                       ADD 1 TO Offset
                   ELSE IF Offset NOT = ZERO
                       COMPUTE New-Pos = Str-Pos - Offset
                       MOVE Str (Str-Pos:1) TO Str (New-Pos:1)
                   END-IF
               END-PERFORM

*              *> Move spaces to characters at the end that have been
*              *> shifted over.
               COMPUTE Str-End = Str-Size - Offset
               MOVE SPACES TO Str (Str-End:Offset)
           END-PERFORM

           GOBACK
           .
```



## ColdFusion


```cfm

<Cfset theString = 'She was a soul stripper. She took my heart!'>
<Cfset theStrip = 'aei'>
<Cfloop from="1" to="#len(theStrip)#" index="i">
  <cfset theString = replace(theString, Mid(theStrip, i, 1), '', 'all')>
</Cfloop>
<Cfoutput>#theString#</Cfoutput>

```



## Common Lisp


```lisp
(defun strip-chars (str chars)
  (remove-if (lambda (ch) (find ch chars)) str))

(strip-chars "She was a soul stripper. She took my heart!" "aei")
;; => "Sh ws  soul strppr. Sh took my hrt!"

;; strip whitespace:
(string-trim
      '(#\Space #\Newline #\Backspace #\Tab
        #\Linefeed #\Page #\Return #\Rubout)
      "  A string   ")
;; => "A string"

```



## D


```d
import std.stdio, std.string;

void main() {
    auto s = "She was a soul stripper. She took my heart!";
    auto ss = "Sh ws  soul strppr. Sh took my hrt!";
    assert(s.removechars("aei") == ss);
}
```



## Delphi


```Delphi
program StripCharacters;

{$APPTYPE CONSOLE}

uses SysUtils;

function StripChars(const aSrc, aCharsToStrip: string): string;
var
  c: Char;
begin
  Result := aSrc;
  for c in aCharsToStrip do
    Result := StringReplace(Result, c, '', [rfReplaceAll, rfIgnoreCase]);
end;

const
  TEST_STRING = 'She was a soul stripper. She took my heart!';
begin
  Writeln(TEST_STRING);
  Writeln(StripChars(TEST_STRING, 'aei'));
end.
```



## EchoLisp


```scheme

;; using regexp /[chars]/g

(define (strip-chars string chars)
  (string-replace string (string-append "/[" chars "]/g") ""))

(strip-chars "She was a soul stripper. She took my heart!" "aei")
    → "Sh ws soul strppr. Sh took my hrt!"

```


## Elena

ELENA 4.x :

```elena
import extensions;
import extensions'text;
import system'routines;

public program()
{
    var testString := "She was a soul stripper. She took my heart!";
    var removeChars := "aei";

    console.printLine(testString.filterBy:(ch => removeChars.indexOf(0, ch) == -1).summarize(new StringWriter()))
}
```

```txt

Sh ws  soul strppr. Sh took my hrt!

```



## Elixir

The easiest solution would be to use replace from the String module, which takes a Regex.

```elixir
str = "She was a soul stripper. She took my heart!"
String.replace(str, ~r/[aei]/, "")
# => Sh ws  soul strppr. Sh took my hrt!
```


To get the desired interface, we just have to dynamically construct the Regex:

```elixir
defmodule RC do
  def stripchars(str, chars) do
    String.replace(str, ~r/[#{chars}]/, "")
  end
end

str = "She was a soul stripper. She took my heart!"
RC.stripchars(str, "aei")
# => Sh ws  soul strppr. Sh took my hrt!
```



## Erlang

The function is created in the shell. A module would be over engineering.

```txt

4> F = fun(To_stripp, Strip_with) -> lists:filter( fun(C) -> not lists:member(C, Strip_with) end, To_stripp ) end.
#Fun<erl_eval.12.111823515>
5> F("She was a soul stripper. She took my heart!", "aei").
"Sh ws  soul strppr. Sh took my hrt!"

```



## Euphoria

The includes use Euphoria 4 standard library files.
A sequence called <tt>originalString</tt> holds the text to be converted.
The <code>puts</code> function is for console output.
The work of this task is done by the <code>transmute</code> function; this function takes parameters separated by commas. Here it uses 3 parameters, up to 5, the other two are optional and aren't put in this time.
The <code>transmute</code> function's usage and examples can be searched for in the official Euphoria 4.0.0+ manual. Euphoria object identifiers (names) are case sensitive but don't need to be in a particular case to be recognized as an object type.

```euphoria
include std\sequence.e
include std\console.e

sequence originalString = "She was a soul stripper. She took my heart!"
puts(1,"Before : " & originalString & "\n")
originalString = transmute(originalString, {{} , "a", "e", "i"}, {{} , "", "", ""})
puts(1,"After : " & originalString & "\n")
any_key()
```

```txt
Before : She was a soul stripper. She took my heart!
After : Sh ws  soul strppr. Sh took my hrt!
Press Any Key to continue...
```


=={{header|F_Sharp|F#}}==

```fsharp
let stripChars text (chars:string) =
    Array.fold (
        fun (s:string) c -> s.Replace(c.ToString(),"")
    ) text (chars.ToCharArray())

[<EntryPoint>]
let main args =
    printfn "%s" (stripChars "She was a soul stripper. She took my heart!" "aei")
    0
```

Output

```txt
Sh ws  soul strppr. Sh took my hrt!
```



## Factor


```factor>without</lang

Example:

```factor
USE: sets
"She was a soul stripper. She took my heart!" "aei" without print
```

```txt

Sh ws  soul strppr. Sh took my hrt!

```



## Forth

Forth is a low level language that is extended to solve your problem.
Here we add APPEND-CHAR to the language and use it built the new string
character by character in a memory buffer called PAD. PAD is a standard Forth word.
SCAN is common in most Forth systems and is typically coded in Forth assembler

```Forth
: append-char ( char str -- ) dup >r  count dup 1+ r> c! + c! ;  \ append char to a counted string
: strippers   ( -- addr len)  s" aeiAEI" ;     \ a string literal returns addr and length

: stripchars ( addr1 len1  addr2 len2 -- PAD len )
        0 PAD c!                               \ clear the PAD buffer
        bounds                                 \ calc loop limits for addr2
        DO
           2dup I C@ ( -- addr1 len1 addr1 len1 char)
           scan nip 0=                         \ scan for char in addr1, test for zero
           IF                                  \ if stack = true (ie. NOT found)
              I c@ PAD append-char             \ fetch addr2 char, append to PAD
           THEN                                \ ...then ... continue the loop
        LOOP
        2drop                                  \ we don't need STRIPPERS now
        PAD count ;                            \ return PAD address and length

```

Test at the forth console

```txt
strippers  s" She was a soul stripper. She took my heart!" stripchars  cr type
Sh ws  soul strppr. Sh took my hrt! ok

```


===Shorter version, using ]] [[ macros===
This shorter version removes creating a new string and prints the "stripped" string immediately.
The macro called '?exit' speeds up the '.stripped' print loop by compiling its code inside the loop.

```Forth
: ?exit ( c1 c2 -- ) ]] = if drop unloop exit then [[ ; immediate
: .stripped ( a u c -- ) -rot bounds ?do dup i c@ ?exit loop emit ;
: stripchars ( a1 u1 a2 u2 -- ) bounds ?do 2dup i c@ .stripped loop 2drop ;

: "aei" s" aei" ;

\ usage: "aei" s" She was a soul stripper. She took my heart!" stripchars
```



## Fortran


```Fortran
elemental subroutine strip(string,set)
  character(len=*), intent(inout) :: string
  character(len=*), intent(in)    :: set
  integer                         :: old, new, stride
  old = 1; new = 1
  do
    stride = scan( string( old : ), set )
    if ( stride > 0 ) then
      string( new : new+stride-2 ) = string( old : old+stride-2 )
      old = old+stride
      new = new+stride-1
    else
      string( new : ) = string( old : )
      return
    end if
  end do
end subroutine strip
```
Note: Since strip is an elemental subroutine, it can be called with arrays of strings as well.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function stripChars(s As Const String, chars As Const String) As String
  If s = "" Then Return ""
  Dim count As Integer = 0
  Dim strip(0 To Len(s) - 1) As Boolean
  For i As Integer = 0 To Len(s) - 1
    For j As Integer = 0 To Len(chars) - 1
      If s[i] = chars[j] Then
        count += 1
        strip(i) = True
        Exit For
      End If
    Next j
  Next i

  Dim buffer As String = Space(Len(s) - count)
  count  = 0
  For i As Integer = 0 To Len(s) - 1
    If Not Strip(i) Then
      buffer[count] = s[i]
      count += 1
    End If
  Next
  Return buffer
End Function

Dim s As String = "She was a soul stripper. She took my heart!"
Dim chars As String = "aei"
Print stripChars(s, chars)
Print
Print "Press any key to quit"
Sleep
```


```txt

Sh ws  soul strppr. Sh took my hrt!

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=f199a8f7a56cf466e4a16c3fc71f6278 Click this link to run this code]'''

```gambas
Public Sub Main()

Print StripChars("She was a soul stripper. She took my heart!", "aei")

End
'_____________________________________________________________________
Public Sub StripChars(sText As String, sRemove As String) As String
Dim siCount As Short

For siCount = 1 To Len(sRemove)
  sText = Replace(sText, Mid(sRemove, siCount, 1), "")
Next

Return sText

End
```

Output:

```txt

Sh ws  soul strppr. Sh took my hrt!

```



## Go


```go
package main

import (
    "fmt"
    "strings"
)

func stripchars(str, chr string) string {
    return strings.Map(func(r rune) rune {
        if strings.IndexRune(chr, r) < 0 {
            return r
        }
        return -1
    }, str)
}

func main() {
    fmt.Println(stripchars("She was a soul stripper. She took my heart!",
        "aei"))
}
```

```txt

Sh ws  soul strppr. Sh took my hrt!

```



## Groovy

Solution:

```groovy
def stripChars = { string, stripChars ->
    def list = string as List
    list.removeAll(stripChars as List)
    list.join()
}
```

Test:

```groovy
println (stripChars('She was a soul stripper. She took my heart!', 'aei'))
```

```txt
Sh ws  soul strppr. Sh took my hrt!
```



## Haskell

I decided to make the string the second argument and the characters the first argument, because it is more likely for someone to partially apply the characters to be stripped (making a function that strips certain characters), than the string.

```haskell
stripChars :: String -> String -> String
stripChars = filter . flip notElem
```

```txt
> stripChars "aei" "She was a soul stripper. She took my heart!"
"Sh ws  soul strppr. Sh took my hrt!"
```


=={{header|Icon}} and {{header|Unicon}}==
The following works in both languages:

```unicon
procedure main(A)
    cs := \A[1] | 'aei'   # argument is set of characters to strip
    every write(stripChars(!&input, cs))  # strip all input lines
end

procedure stripChars(s,cs)
    ns := ""
    s ? while ns ||:= (not pos(0), tab(upto(cs)|0)) do tab(many(cs))
    return ns
end
```

```txt
->strip
She was a soul stripper. She took my heart!
Sh ws  soul strppr. Sh took my hrt!
Aardvarks are ant eaters.
Ardvrks r nt trs.
->strip AEIOUaeiou
Aardvarks are ant eaters.
rdvrks r nt trs.
->
```



## J

'''Solution:'''

The dyadic primitive <code>-.</code> ([http://www.jsoftware.com/help/dictionary/d121.htm Less]) is probably the simplest way to solve this task.
```j
   'She was a soul stripper. She took my heart!' -. 'aei'
Sh ws  soul strppr. Sh took my hrt!
```



## Java


```Java
class StripChars {
    public static String stripChars(String inString, String toStrip) {
        return inString.replaceAll("[" + toStrip + "]", "");
    }

    public static void main(String[] args) {
        String sentence = "She was a soul stripper. She took my heart!";
        String chars = "aei";
        System.out.println("sentence: " + sentence);
        System.out.println("to strip: " + chars);
        System.out.println("stripped: " + stripChars(sentence, chars));
    }
}
```

```txt
sentence: She was a soul stripper. She took my heart!
to strip: aei
stripped: Sh ws  soul strppr. Sh took my hrt!
```



## JavaScript



### ES5



```JavaScript
function stripchars(string, chars) {
  return string.replace(RegExp('['+chars+']','g'), '');
}
```



### ES6


Reversing the order of the arguments, to simplify any currying:


```JavaScript
(() => {
    'use strict';

    // stripChars :: String -> String -> String
    const stripChars = (strNeedles, strHayStack) =>
        strHayStack.replace(RegExp(`[${strNeedles}]`, 'g'), '');

    // GENERIC FUNCTION

    // curry :: ((a, b) -> c) -> a -> b -> c
    const curry = f => a => b => f(a, b);

    // TEST FUNCTION

    const noAEI = curry(stripChars)('aeiAEI');

    // TEST
    return noAEI('She was a soul stripper. She took my heart!');

    // 'Sh ws  soul strppr. Sh took my hrt!'
})();
```


```txt
'Sh ws  soul strppr. Sh took my hrt!'
```


Alternatively, we could also do this without a regex:


```JavaScript
(() => {
    'use strict';

    // stripChars :: String -> String -> String
    const stripChars = (strNeedles, strHayStack) =>
        strHayStack.split('')
        .filter(x => !elem(x, strNeedles))
        .join('');

    // GENERIC FUNCTIONS

    // elem :: Eq a => a -> [a] -> Bool
    const elem = (x, xs) => xs.indexOf(x) !== -1;

    // curry :: ((a, b) -> c) -> a -> b -> c
    const curry = f => a => b => f(a, b);

    // TEST FUNCTION

    const noAEI = curry(stripChars)('aeiAEI');


    // TEST
    return noAEI('She was a soul stripper. She took my heart!');

    // 'Sh ws  soul strppr. Sh took my hrt!'
})();
```


```txt
'Sh ws  soul strppr. Sh took my hrt!'
```



## jq


```jq
def stripchars(string; banish):
  (string | explode) - (banish | explode) | implode;
```

'''Note''': In jq, it would be more idiomatic to define the function as a filter:

```jq
def stripchars(banish):
  explode - (banish | explode) | implode;
```

In this case, we would write:
 "She was a soul stripper. She took my heart!" | stripchars("aei")


## Julia

```julia
stripChar = (s, r) -> replace(s, Regex("[$r]") => "")
```


```txt
> stripChar("She was a soul stripper. She took my heart!", "aei")
Sh ws  soul strppr. Sh took my hrt!
```



## Kotlin


```scala
// version 1.0.6

fun stripChars(s: String, r: String) = s.replace(Regex("[$r]"), "")

fun main(args: Array<String>) {
    println(stripChars("She was a soul stripper. She took my heart!", "aei"))
}
```


```txt

Sh ws  soul strppr. Sh took my hrt!

```



## Lasso


```Lasso
define stripper(in::string,destroy::string) => {
	with toremove in #destroy->values do => {
		#in->replace(#toremove,'')
	}
	return #in
}
stripper('She was a soul stripper. She took my heart!','aei')
```


```txt
Sh ws  soul strppr. Sh took my hrt!
```



## Logo


```logo
to strip :string :chars
  output filter [not substringp ? :chars] :string
end

print strip "She\ was\ a\ soul\ stripper.\ She\ took\ my\ heart! "aei

bye
```

```txt
Sh ws  soul strppr. Sh took my hrt!
```



## Lua


```lua

function stripchars(str, chrs)
  local s = str:gsub("["..chrs:gsub("%W","%%%1").."]", '')
  return s
end

print( stripchars( "She was a soul stripper. She took my heart!", "aei" ) )
--> Sh ws  soul strppr. Sh took my hrt!
print( stripchars( "She was a soul stripper. She took my heart!", "a-z" ) )
--> She ws  soul stripper. She took my hert!

```



## Liberty BASIC


```lb
Print stripchars$("She was a soul stripper. She took my heart!", "aei", 1)
End

Function stripchars$(strip$, chars$, num)
    For i = 1 To Len(strip$)
        If Mid$(strip$, i, 1) <> Mid$(chars$, num, 1) Then
            stripchars$ = (stripchars$ + Mid$(strip$, i, 1))
        End If
    Next i
    If (num <= Len(chars$)) Then stripchars$ = stripchars$(stripchars$, chars$, (num + 1))
End Function
```




## LiveCode


```LiveCode
function stripChars str charlist
    local strstripped
    put str into strstripped
    repeat for each char c in charlist
        replace c with empty in strstripped
    end repeat
    return strstripped
end stripChars
```

Test
```LiveCode
command teststripchars
    put stripchars("She was a soul stripper. She took my heart!","aei")
end teststripchars
```
Output
```txt
Sh ws  soul strppr. Sh took my hrt!
```



## Maple


```Maple
with(StringTools):

Remove(c->Has("aei",c), "She was a soul stripper. She took my heart!");
```

Output:

```txt
                    "Sh ws  soul strppr. Sh took my hrt!"
```



## Mathematica


```Mathematica
stripchars[a_,b_]:=StringReplace[a,(#->"")&/@Characters[b]]
stripchars["She was a soul stripper. She took my heart!","aei"]
->Sh ws  soul strppr. Sh took my hrt!
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function str = stripchars(str, charlist)
  % MATLAB after 2016b: str = erase(str, charlist);
  str(ismember(str, charlist)) = '';
```

```txt
 >> stripchars('She was a soul stripper. She took my heart!','aei')
ans = Sh ws  soul strppr. Sh took my hrt!
```



## Nemerle


```Nemerle
StripChars( text : string, remove : string ) : string
{
    def chuck = Explode(remove);
    Concat( "", Split(text, chuck))
}
```



## NetRexx


```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols

say stripchars("She was a soul stripper. She took my heart!", "aei")

return

method stripchars(haystack, chs) public static

  loop c_ = 1 to chs.length
    needle = chs.substr(c_, 1)
    haystack = haystack.changestr(needle, '')
    end c_

  return haystack
```



## NewLISP


```NewLISP
(let (sentence "She was a soul stripper. She took my heart!")
  (replace "[aei]" sentence "" 0))
```



## Nim


```nim
import strutils

echo "She was a soul stripper. She took my heart!".split({'a','e','i'}).join()

echo "She was a soul stripper. She took my heart!".multiReplace(
  ("a", ""),
  ("e", ""),
  ("i", "")
)

```

```txt
Sh ws  soul strppr. Sh took my hrt!
Sh ws  soul strppr. Sh took my hrt!
```


=={{header|Objective-C}}==
```objc
@interface NSString (StripCharacters)
- (NSString *) stripCharactersInSet: (NSCharacterSet *) chars;
@end

@implementation NSString (StripCharacters)
- (NSString *) stripCharactersInSet: (NSCharacterSet *) chars {
    return [[self componentsSeparatedByCharactersInSet:chars] componentsJoinedByString:@""];
}
@end
```

```objc
    NSString *aString = @"She was a soul stripper. She took my heart!";
    NSCharacterSet* chars = [NSCharacterSet characterSetWithCharactersInString:@"aei"];

    // Display the NSString.
    NSLog(@"%@", [aString stripCharactersInSet:chars]);
```



## OCaml


```ocaml
let stripchars s cs =
  let len = String.length s in
  let res = Bytes.create len in
  let rec aux i j =
    if i >= len
    then Bytes.to_string (Bytes.sub res 0 j)
    else if String.contains cs s.[i] then
      aux (succ i) (j)
    else begin
      Bytes.set res j s.[i];
      aux (succ i) (succ j)
    end
  in
  aux 0 0
```

```txt
# stripchars "She was a soul stripper. She took my heart!" "aei" ;;
- : string = "Sh ws  soul strppr. Sh took my hrt!"
```



## Oforth



```Oforth
String method: stripChars(str)  #[ str include not ] self filter ;

"She was a soul stripper. She took my heart!" stripChars("aei") println
```


```txt

Sh ws  soul strppr. Sh took my hrt!

```



## PARI/GP

GP should not be used for string manipulation.  A good solution to this problem would probably involve <code>system("perl -e</code>...

```parigp
stripchars(s, bad)={
  bad=Set(Vec(Vecsmall(bad)));
  s=Vecsmall(s);
  my(v=[]);
  for(i=1,#s,if(!setsearch(bad,s[i]),v=concat(v,s[i])));
  Strchr(v)
};
stripchars("She was a soul stripper. She took my heart!","aei")
```



## Pascal

See [[Strip_a_set_of_characters_from_a_string#Delphi|Delphi]]


## Perl

Note: this example uses a regular expression character class. Certain characters, like hyphens and brackets, may need to be escaped.

```perl
sub stripchars {
    my ($s, $chars) = @_;
    $s =~ s/[$chars]//g;
    return $s;
}

print stripchars("She was a soul stripper. She took my heart!", "aei"), "\n";
```

```txt

Sh ws  soul strppr. Sh took my hrt!

```

Another good option for stripping characters is to use the <code>tr///</code> operator. This option is very efficient when the set of characters to strip is fixed at compile time, because <code>tr///</code> is specifically designed for transforming and deleting characters. Note that hyphens also have special meaning in this case.

```perl
$str =~ tr/aei//d;
```

Since the characters used for <code>tr///</code> must be fixed at compile time, unfortunately, it requires the use of an <code>eval</code> to do this generally for any set of characters provided at runtime:

```perl
sub stripchars {
    my ($s, $chars) = @_;
    eval("\$s =~ tr/$chars//d;");
    return $s;
}
```



## Perl 6


```perl6
sub strip_chars ( $s, $chars ) {
    return $s.trans( $chars.comb X=> '' );
}

say strip_chars( 'She was a soul stripper. She took my heart!', 'aei' );
```

```txt
Sh ws  soul strppr. Sh took my hrt!
```



## Phix


```Phix
function stripchars(string s, string chars)
    for i=1 to length(chars) do
        s = substitute(s,chars[i..i],"")
    end for
    return s
end function

?stripchars("She was a soul stripper. She took my heart!","aei")
```

```txt

"Sh ws  soul strppr. Sh took my hrt!"

```



## PHP


```php
<?php
function stripchars($s, $chars) {
    return str_replace(str_split($chars), "", $s);
}

echo stripchars("She was a soul stripper. She took my heart!", "aei"), "\n";
?>
```

```txt

Sh ws  soul strppr. Sh took my hrt!

```



## PicoLisp


```PicoLisp
(de strDiff (Str1 Str2)
   (pack (diff (chop Str1) (chop Str2))) )
```

```txt
: (strDiff "She was a soul stripper. She took my heart!" "aei")
-> "Sh ws  soul strppr. Sh took my hrt!"
```



## PL/I


```PL/I
strip_chars: procedure (text, chars) returns (character (100) varying);
   declare text character (*) varying, chars character (*) varying;
   declare out_text character (100);
   declare ch character (1);
   declare (i, j) fixed binary;

   j = 0;
   do i = 1 to length(text);
      ch = substr(text, i, 1);
      if index(chars, ch) = 0 then
         do; j = j + 1; substr(out_text, j, 1) = ch; end;
   end;
   return (substr(out_text, 1, j) );
end strip_chars;
```



## Powershell

Powershell have replace operator that by will replace a regex pattern with a given string:


```Powershell
'She was a soul stripper. She took my heart!' -replace '[aei]', ''
Sh ws  soul strppr. Sh took my hrt!

```




## Prolog

Works with SWI-Prolog and module '''lambda.pl''' written by  '''Ulrich Neumerkel''' found there http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/lambda.pl .

```Prolog
:- use_module(library(lambda)).

stripchars(String, Exclude, Result) :-
	exclude(\X^(member(X, Exclude)), String, Result1),
	string_to_list(Result, Result1).

```

```txt
 ?- stripchars("She was a soul stripper. She took my heart!","aei", R).
R = "Sh ws  soul strppr. Sh took my hrt!".

```



## PureBasic

PureBasic uses a single (for ASCII) or a two-byte (for Unicode) null to signal the end of a string.  Nulls are thus excluded from the allowable characters to strip as they can't be included in a PureBasic string.

```PureBasic
Procedure.s stripChars(source.s,  charsToStrip.s)
  Protected i, *ptrChar.Character, length = Len(source), result.s
  *ptrChar = @source
  For i = 1 To length
    If Not FindString(charsToStrip, Chr(*ptrChar\c))
      result + Chr(*ptrChar\c)
    EndIf
    *ptrChar + SizeOf(Character)
  Next
  ProcedureReturn result
EndProcedure

If OpenConsole()
  PrintN(stripChars("She was a soul stripper. She took my heart!", "aei"))

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

```txt
Sh ws  soul strppr. Sh took my hrt!
```



## Python


### Not using regular expressions

```python>>>
 def stripchars(s, chars):
...     return s.translate(None, chars)
...
>>> stripchars("She was a soul stripper. She took my heart!", "aei")
'Sh ws  soul strppr. Sh took my hrt!'
```

```python>>>
 import string
>>> def stripchars(s, chars):
...     return s.translate(string.maketrans("", ""), chars)
...
>>> stripchars("She was a soul stripper. She took my heart!", "aei")
'Sh ws  soul strppr. Sh took my hrt!'
```

Implemented manually:

```python>>>
 def stripchars(s, chars):
...     return "".join(c for c in s if c not in chars)
...
>>> stripchars("She was a soul stripper. She took my heart!", "aei")
'Sh ws  soul strppr. Sh took my hrt!'
```


### Using regular expressions


```python>>>
 import re
>>> def stripchars(s, chars):
	return re.sub('[%s]+' % re.escape(chars), '', s)

>>> stripchars("She was a soul stripper. She took my heart!", "aei")
'Sh ws  soul strppr. Sh took my hrt!'
>>>
```



## Racket



```Racket

#lang racket

;; Using list operations
(define (stripchars1 text chars)
  (list->string (remove* (string->list chars) (string->list text))))

;; Using a regexp
;; => will be broken if chars have "-" or "]" or "\\"
(define (stripchars2 text chars)
  (regexp-replace* (~a "[" chars "]+") text ""))

```



## Red


```Red

stripchars: func [str chars] [trim/with str chars]
stripchars "She was a soul stripper. She took my heart!" "aei"
```



## REXX


### version 1

In the REXX language, '''strip''' usually means to remove leading and/or trailing characters from a string (most often, blanks).

```rexx
/*REXX program  removes  a list of characters from a string  (the haystack).            */
say stripChars('She was a soul stripper. She took my heart!',   "iea")     /*elide: iea */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
stripChars: procedure;  parse arg haystack, remove
                                do j=1  for length(remove)
                                haystack=changestr( substr( remove, j, 1),  haystack, '')
                                end   /*j*/
            return haystack
```

Some older REXXes don't have a   '''changestr'''   BIF, so one is included here   ───►   [[CHANGESTR.REX]].



```txt

Sh ws  soul strppr. Sh took my hrt!

```



### version 2

Using recursion:

```rexx

/* REXX */
say StripChars('She was a soul stripper. She took my heart!','iea')
exit 0

StripChars: procedure
parse arg strng,remove
removepos=Verify(strng,remove,'MATCH')
if removepos=0 then return strng
parse value strng with strng =(removepos) +1 rest
return strng || StripChars(rest,remove)


```



### version 3

This works on all Rexxes.

(Except for R4 and ROO at the least, there may be others.)

```rexx

/* REXX ***************************************************************
* If source and stripchars don't contain a hex 00 character, this works
* 06.07.2012 Walter Pachl
* 19.06.2013 -"- space(result,0) -> space(result,0,' ')
*                space(result,0) removes WHITESPACE not only blanks
**********************************************************************/
Say 'Sh ws  soul strppr. Sh took my hrt! -- expected'
Say stripchars("She was a soul stripper. She took my heart!","aei")
Exit
stripchars: Parse Arg string,stripchars
result=translate(string,'00'x,' ')      /* turn blanks into '00'x   */
result=translate(result,' ',stripchars) /* turn stripchars into ' ' */
result=space(result,0,' ')              /* remove all blanks        */
Return translate(result,' ','00'x)      /* '00'x back to blanks     */

```



### version 4

 Another neat (?) one
 No x00 restriction and no changestr

```rexx

stripchars: Procedure
  Parse Arg i,s                 /* get input and chars to be removed */
  o=''                          /* initialize result                 */
  Do While i\==''               /* loop through input                */
    Parse Var i c +1 i          /* get one character                 */
    If pos(c,s)=0 Then          /* it's not to be removed            */
      o=o||c                    /* append it to the result           */
    End
  Return o                      /* return the result                 */

```



## Ring


```ring

aList = "She was a soul stripper. She took my heart!"
bList = "aei"
see aList + nl
see stripChars(aList,bList)

func stripChars cList, dList
     for n = 1 to len(dList)
         cList = substr(cList,dList[n],"") + nl
     next
     return cList

```



## Ruby


```ruby
"She was a soul stripper. She took my heart!".delete("aei")  # => "Sh ws  soul strppr. Sh took my hrt!"
```



## Rust

Naive Implementation:

```rust

fn strip_characters(original : &str, to_strip : &str) -> String {
    let mut result = String::new();
    for c in original.chars() {
        if !to_strip.contains(c) {
           result.push(c);
       }
    }
    result
}

```


Functional Implementation:

```rust

fn strip_characters(original : &str, to_strip : &str) -> String {
    original.chars().filter(|&c| !to_strip.contains(c)).collect()
}

```


Either can be executed thusly:

```rust

fn main() {
    println!("{}", strip_characters("She was a soul stripper. She took my heart!", "aei"));
}

```



## SAS

This code will write the resulting string to the log:

```SAS
%let string=She was a soul stripper. She took my heart!;
%let chars=aei;
%let stripped=%sysfunc(compress("&string","&chars"));
%put &stripped;
```


Log:

```SAS
Sh ws  soul strppr. Sh took my hrt!
```



## Scala


```scala
def stripChars(s:String, ch:String)= s filterNot (ch contains _)

stripChars("She was a soul stripper. She took my heart!", "aei")
// => Sh ws  soul strppr. Sh took my hrt!
```



## Scheme


Two approaches are given here.  The first is in plain Scheme, and implements a loop to remove the characters.
The second uses the SRFI libraries to create a character set and delete those characters from the string.


```scheme

(import (scheme base)
        (scheme write)
        (only (srfi 13) string-delete)
        (only (srfi 14) ->char-set))

;; implementation in plain Scheme
(define (strip-chars str chars)
  (let ((char-list (string->list chars)))
    (define (do-strip str-list result)
      (cond ((null? str-list)
             (reverse result))
            ((member (car str-list) char-list char=?)
             (do-strip (cdr str-list) result))
            (else
              (do-strip (cdr str-list) (cons (car str-list) result)))))
    (list->string
      (do-strip (string->list str) '()))))

(display (strip-chars "She was a soul stripper. She took my heart!" "aei"))
(newline)

;; using functions in SRFI 13 and SRFI 14
(define (strip-chars2 str chars)
  (string-delete (->char-set chars) str))

(display (strip-chars2 "She was a soul stripper. She took my heart!" "aei"))
(newline)

```


```txt

Sh ws  soul strppr. Sh took my hrt!
Sh ws  soul strppr. Sh took my hrt!

```



## ScriptBasic


```scriptbasic

str1 = "She was a soul stripper. She took my heart!"
rmv = "aei"
FOR i = 1 TO LEN(rmv)
  str1 = REPLACE(str1, MID(rmv, i, 1), "")
NEXT
PRINT str1,"\n"

```



## Sed

Using echo and piping it through a sed filter:

```bash
#!/bin/bash

strip_char()
{
  echo "$1" | sed "s/[$2]//g"
}
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func string: stripchars (in string: mainStri, in string: charList) is func
  result
    var string: strippedStri is "";
  local
    var char: ch is ' ';
  begin
    strippedStri := mainStri;
    for ch range charList do
      strippedStri := replace(strippedStri, str(ch), "");
    end for;
  end func;

const proc: main is func
  begin
    writeln(stripchars("She was a soul stripper. She took my heart!", "aei"));
  end func;
```

```txt

Sh ws  soul strppr. Sh took my hrt!

```



## Sidef


```ruby
func stripchars(str, char_list) {
    str.tr(char_list, "", "d");
}
```


or:

```ruby
func stripchars(str, char_list) {
    str.chars.grep {|c| !char_list.contains(c)}.join;
}
```


Calling the function:

```ruby
say stripchars("She was a soul stripper. She took my heart!", "aei");
```

```txt
Sh ws  soul strppr. Sh took my hrt!
```



## Standard ML


```sml
fun stripchars (string, chars) = let
  fun aux c =
    if String.isSubstring (str c) chars then
      ""
    else
      str c
in
  String.translate aux string
end
```

```txt
- stripchars ("She was a soul stripper. She took my heart!", "aei") ;
val it = "Sh ws  soul strppr. Sh took my hrt!" : string
```

Alternately:

```sml
fun stripchars (string, chars) =
  String.concat (String.tokens (fn c => String.isSubstring (str c) chars) string)
```

```txt
- stripchars ("She was a soul stripper. She took my heart!", "aei") ;
val it = "Sh ws  soul strppr. Sh took my hrt!" : string
```



## Smalltalk

```smalltalk
| stripChars |
stripChars := [ :string :chars |
	string reject: [ :c | chars includes: c ] ].
stripChars
	value: 'She was a soul stripper. She took my heart!'
	value: 'aei'.

"'Sh ws  soul strppr. Sh took my hrt!'"
```



## SNOBOL4

Note: "strip" is a function, its argument, the label of its first executed line, and its returned value.


```SNOBOL4
      DEFINE("strip(strip,c)")         :(strip_end)
strip strip ANY(c) =                   :S(strip)F(RETURN)
strip_end

      chars = HOST(2, HOST(3))   ;* Get command line argument
      chars = IDENT(chars) "aei"
again line = INPUT                    :F(END)
      OUTPUT = strip(line, chars)     :(again)
END
```

```txt
snobol4 strip.sno aei
She was a soul stripper. She took my heart.
Sh ws  soul strppr. Sh took my hrt.
```



## Swift


```swift
extension String {
  func stripCharactersInSet(chars: [Character]) -> String {
    return String(seq: filter(self) {find(chars, $0) == nil})
  }
}

let aString = "She was a soul stripper. She took my heart!"
let chars: [Character] = ["a", "e", "i"]

println(aString.stripCharactersInSet(chars))
```

```txt
Sh ws  soul strppr. Sh took my hrt!
```



## Tcl


```tcl
proc stripchars {str chars} {
    foreach c [split $chars ""] {set str [string map [list $c ""] $str]}
    return $str
}

set s "She was a soul stripper. She took my heart!"
puts [stripchars $s "aei"]
```



## TorqueScript

This uses a default function.
  $string = "She was a soul stripper. She took my heart!";
  $chars = "aei";
  $newString = stripChars($string, $chars);
  echo($string);
  echo($newString);

Output:

  She was a soul stripper. She took my heart!
  Sh ws  soul strppr. Sh took my hrt!


## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT,{}
string="She was a soul stripper. She took my heart!"
stringstrip=EXCHANGE (string,"_[aei]__")
print string
print stringstrip

```

Output:

```txt

She was a soul stripper. She took my heart!
Sh ws  soul strppr. Sh took my hrt!

```



## TXR

This solution builds up a regular expression in a hygienic way from the set of characters given as a string.
The string is broken into a list, which is used to construct a regex abstract syntax tree for a character set match, using a Lisp quasiquote. This is fed to the regex compiler, which produces an executable machine that is then used with <code>regsub</code>.

On the practical side, some basic structural pattern matching is used to process command line argument list.

Since the partial argument list (the arguments belonging to the TXR script) is a suffix of the full argument list (the complete arguments which include the invoking command and the script name), the classic Lisp function <code>ldiff</code> comes in handy in obtaining just the prefix, for printing the usage:


```txrlisp
(defun strip-chars (str set)
  (let* ((regex-ast ^(set ,*(list-str set)))
         (regex-obj (regex-compile regex-ast)))
    (regsub regex-obj "" str)))

(defun usage ()
  (pprinl `usage: @{(ldiff *full-args* *args*) " "} <string> <set>`)
  (exit 1))

(tree-case *args*
  ((str set extra) (usage))
  ((str set . junk) (pprinl (strip-chars str set)))
  (else (usage)))
```

```txt
$ txr strip-chars-2.tl
usage: txr strip-chars-2.tl <string> <set>
$ txr strip-chars-2.tl "she was a soul stripper. she stole my heart." "aei"
sh ws  soul strppr. sh stol my hrt.
```


Now here is a rewrite of <code>strip-chars</code> which just uses classic Lisp that has been generalized to work over strings, plus the <code>do</code> syntax (a sibling of the <code>op</code> operator) that provides syntactic sugar for a lambda function whose body is an operator or macro form.


```txr
(defun strip-chars (str set)
   (mappend (do if (memq @1 set) (list @1)) str))
```


<code>(do if (memq @1 set) (list @1))</code> is just <code>(lambda (item) (if (memq item set) (list item)))</code>.
<code>mappend</code> happily maps over strings and since the leftmost input sequence is a string, and the return values of the lambda are sequence of characters, <code>mappend</code> produces a string.


## UNIX Shell

One would normally do this using the standard tr(1) command:
```bash
strip_chars() {
  echo "$1" | tr -d "$2"
}
```

But it can also be accomplished with bash's built-in parameter expansions:
```bash
function strip_chars {
  echo "${1//[$2]}"
}
```

Test code:

```bash
 strip_chars "She was a soul stripper.  She took my heart!" aei
```

```txt
Sh ws  soul strppr.  Sh took my hrt!
```



## Ursala

Normally there's no need to define this operation because it's built in.

```Ursala
strip = ~&j

#cast %s

test = strip('she was a soul stripper. she took my heart','aei')
```

```txt

'sh ws  soul strppr. sh took my hrt'

```


## VBA

Pass the optional bSpace parameter True to replace stripped characters with spaces, otherwise replaced with null.


```vb
Function StripChars(stString As String, stStripChars As String, Optional bSpace As Boolean)
Dim i As Integer, stReplace As String
    If bSpace = True Then
        stReplace = " "
    Else
        stReplace = ""
    End If
    For i = 1 To Len(stStripChars)
        stString = Replace(stString, Mid(stStripChars, i, 1), stReplace)
    Next i
    StripChars = stString
End Function
```


```txt
' with bSpace = True:
Sh  w s   soul str pp r. Sh  took my h  rt!

'with bSpace = False / omitted:
Sh ws  soul strppr. Sh took my hrt!
```



## VBScript


```vb

Function stripchars(s1,s2)
	For i = 1 To Len(s1)
		If InStr(s2,Mid(s1,i,1)) Then
			s1 = Replace(s1,Mid(s1,i,1),"")
		End If
	Next
	stripchars = s1
End Function

WScript.StdOut.Write stripchars("She was a soul stripper. She took my heart!","aei")

```


```txt
Sh ws  soul strppr. Sh took my hrt!
```



## zkl


```zkl
println("She was a soul stripper. She took my heart!" - "aei")
//-->Sh ws  soul strppr. Sh took my hrt!
```

