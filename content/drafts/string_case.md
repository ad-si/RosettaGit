+++
title = "String case"
description = ""
date = 2019-10-12T11:18:06Z
aliases = []
[extra]
id = 1933
[taxonomies]
categories = []
tags = []
+++

{{task|String manipulation}}

;Task:
Take the string     '''alphaBETA'''     and demonstrate how to convert it to:
:::*   upper-case     and
:::*   lower-case



Use the default encoding of a string literal or plain ASCII if there is no string literal in your language.

Show any additional case conversion functions   (e.g. swapping case, capitalizing the first letter, etc.)   that may be included in the library of your language.


{{Template:Strings}}





## 360 Assembly

The first version uses a nice thing of EBCDIC coding,
uppercase can be performed with a simple 'OR' with blank character (X'40'), in the same way
lowercase can be performed with a 'AND' with character 191 (X'BF').

```360asm
UCASE    CSECT
         USING  UCASE,R15
         MVC    UC,PG
         MVC    LC,PG
         OC     UC,=16C' '         or  X'40' uppercase
         NC     LC,=16X'BF'        and X'BF' lowercase
         XPRNT  PG,L'PG            print original
         XPRNT  UC,L'UC            print uc
         XPRNT  LC,L'LC            print lc
         BR     R14
PG       DC     CL9'alphaBETA'
UC       DS     CL(L'PG)
LC       DS     CL(L'PG)
         YREGS
         END    UCASE
```

{{out}}

```txt

alphaBETA
ALPHABETA
alphabeta

```

The second version uses the translate operation (TR opcode),
but now EBCDIC coding with alphabetic in 3 sequences, makes things a bit longer to create
translation tables.

```360asm
UCASE    CSECT
         USING  UCASE,R15
         MVC    UC,PG
         MVC    LC,PG
         TR     UC,TABLEU          TR uppercase
         TR     LC,TABLEL          TR lowercase
         XPRNT  PG,L'PG            print original
         XPRNT  UC,L'UC            print uc
         XPRNT  LC,L'LC            print lc
         BR     R14
PG       DC     CL9'alphaBETA'
UC       DS     CL(L'PG)
LC       DS     CL(L'PG)
TABLEU   DC     256AL1(*-TABLEU)
         ORG    TABLEU+C'a'
         DC     C'ABCDEFGHI'
         ORG    TABLEU+C'j'
         DC     C'JKLMNOPQR'
         ORG    TABLEU+C's'
         DC     C'STUVWXYZ'
         ORG
TABLEL   DC     256AL1(*-TABLEL)
         ORG    TABLEL+C'A'
         DC     C'abcdefghi'
         ORG    TABLEL+C'J'
         DC     C'jklmnopqr'
         ORG    TABLEL+C'S'
         DC     C'stuvwxyz'
         ORG
         YREGS
         END    UCASE
```

{{out}}

```txt

alphaBETA
ALPHABETA
alphabeta

```



## 4D


```4d
$string:="alphaBETA"
$uppercase:=Uppercase($string)
$lowercase:=Lowercase($string)
```



## 6502 Assembly


<lang>	.lf  case6502.lst
	.cr  6502
	.tf  case6502.obj,ap1
;------------------------------------------------------
; String Case for the 6502 by barrym95838 2013.04.07
; Thanks to sbprojects.com for a very nice assembler!
; The target for this assembly is an Apple II with
;   mixed-case output capabilities.  Apple IIs like to
;   work in '+128' ascii, so this version leaves bit 7
;   alone, and can be used with either flavor.
; 6502s work best with data structures < 256 bytes;
;   several instructions would have to be added to
;   properly deal with longer strings.
; Tested and verified on AppleWin 1.20.0.0
;------------------------------------------------------
; Constant Section
;
StrPtr	 =   $6		;0-page temp pointer (2 bytes)
Low	 =   $8		;0-page temp low bound
High	 =   $9		;0-page temp high bound
CharOut	 =   $fded	;Specific to the Apple II
BigA	 =   "A"	;'A' for normal ascii
BigZ	 =   "Z"	;'Z'  "    "      "
LittleA	 =   "a"	;'a'  "    "      "
LittleZ	 =   "z"	;'z'  "    "      "
;
### ================================================

	.or  $0f00
;------------------------------------------------------
; The main program
;
main	ldx  #sTest	;Point to the test string
	lda  /sTest
	jsr  puts	;print it to stdout
	jsr  toUpper	;convert to UPPER-case
	jsr  puts	;print it
	jsr  toLower	;convert to lower-case
	jmp  puts	;print it and return to caller
;------------------------------------------------------
toUpper	ldy  #LittleA
	sty  Low	;set up the flip range
	ldy  #LittleZ
	bne  toLow2	;return via toLower's tail
;------------------------------------------------------
toLower	ldy  #BigA
	sty  Low	;set up the flip range
	ldy  #BigZ
toLow2	sty  High
	;		;return via fall-thru to flip
;------------------------------------------------------
; Given a NUL-terminated string at A:X, flip the case
;   of any chars in the range [Low..High], inclusive;
;   only works on the first 256 bytes of a long string
; Uses:  StrPtr, Low, High
; Preserves:  A, X
; Trashes:  Y
;
flip	stx  StrPtr	;init string pointer
	sta  StrPtr+1
	ldy  #0
	pha  		;save A
flip2	lda  (StrPtr),y	;get string char
	beq  flip5	;done if NUL
	cmp  Low
	bcc  flip4	;if Low <= char <= High
	cmp  High
	beq  flip3
	bcs  flip4
flip3	eor  #$20	;  then flip the case
	sta  (StrPtr),y
flip4	iny  		;point to next char
	bne  flip2	;loop up to 255 times
flip5	pla  		;restore A
	rts  		;return
;------------------------------------------------------
; Output NUL-terminated string @ A:X; strings longer
;   than 256 bytes are truncated there
; Uses:  StrPtr
; Preserves:  A, X
; Trashes:  Y
;
puts	stx  StrPtr	;init string pointer
	sta  StrPtr+1
	ldy  #0
	pha  		;save A
puts2	lda  (StrPtr),y	;get string char
	beq  puts3	;done if NUL
	jsr  CharOut	;output the char
	iny  		;point to next char
	bne  puts2	;loop up to 255 times
puts3	pla  		;restore A
	rts  		;return
;------------------------------------------------------
; Test String (in '+128' ascii, Apple II style)
;
sTest	.as	-"Alpha, BETA, gamma, {[(<123@_>)]}."
	.az	-#13
;------------------------------------------------------
	.en
```

Output:

```txt

Alpha, BETA, gamma, {[(<123@_>)]}.
ALPHA, BETA, GAMMA, {[(<123@_>)]}.
alpha, beta, gamma, {[(<123@_>)]}.
```



## ActionScript



```actionscript
var string:String = 'alphaBETA';
var upper:String = string.toUpperCase();
var lower:String = string.toLowerCase();
```



## Ada


```ada
with Ada.Characters.Handling, Ada.Text_IO;
use  Ada.Characters.Handling, Ada.Text_IO;

procedure Upper_Case_String is
   S : constant String := "alphaBETA";
begin
   Put_Line (To_Upper (S));
   Put_Line (To_Lower (S));
end Upper_Case_String;
```



## ALGOL 68

{{trans|C|Note: This specimen retains the original [[#C|C]] coding style.}}
{{works with|ALGOL 68|Revision 1 - no extensions to language used.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}

```algol68
#!/usr/local/bin/a68g --script #

# Demonstrate toupper and tolower for standard ALGOL 68
strings.  This does not work for multibyte character sets. #

INT l2u = ABS "A" - ABS "a";

PROC to upper = (CHAR c)CHAR:
  (ABS "a" > ABS c | c |: ABS c > ABS "z" | c | REPR ( ABS c + l2u ));

PROC to lower = (CHAR c)CHAR:
  (ABS "A" > ABS c | c |: ABS c > ABS "Z" | c | REPR ( ABS c - l2u ));

# Operators can be defined in ALGOL 68 #
OP (CHAR)CHAR TOLOWER = to lower, TOUPPER = to upper;

# upper-cases s in place #
PROC string to upper = (REF STRING s)VOID:
    FOR i FROM LWB s TO UPB s DO s[i] := to upper(s[i]) OD;

# lower-cases s in place #
PROC string to lower = (REF STRING s)VOID:
    FOR i FROM LWB s TO UPB s DO s[i] := to lower(s[i]) OD;

main: (
    STRING t := "alphaBETA";
    string to upper(t);
    printf(($"uppercase: "gl$, t));
    string to lower(t);
    printf(($"lowercase: "gl$, t))
)
```

Output:

```txt

uppercase: ALPHABETA
lowercase: alphabeta

```



## ALGOL W


```algolw
begin
    % algol W doesn't have standard case conversion routines, this is one way %
    % such facilities could be provided                                       %

    % converts text to upper case                                             %
    % assumes the letters are contiguous in the character set (as in ASCII)   %
    % would not work in EBCDIC (as the original algol W implementations used) %
    procedure upCase( string(256) value result text ) ;
        for i := 0 until 255 do begin
            string(1) c;
            c := text( i // 1 );
            if c >= "a" and c <= "z"
            then begin
                text( i // 1 ) := code( decode( "A" )
                                      + ( decode( c ) - decode( "a" ) )
                                      )
            end
        end upCase ;

    % converts text to lower case                                             %
    % assumes the letters are contiguous in the character set (as in ASCII)   %
    % would not work in EBCDIC (as the original algol W implementations used) %
    procedure dnCase( string(256) value result text ) ;
        for i := 0 until 255 do begin
            string(1) c;
            c := text( i // 1 );
            if c >= "A" and c <= "Z"
            then begin
                text( i // 1 ) := code( decode( "a" )
                                      + ( decode( c ) - decode( "A" ) )
                                      )
            end
        end dnCase ;

    string(256) text;
    text := "alphaBETA";
    upCase( text );
    write( text( 0 // 40 ) );
    dnCase( text );
    write( text( 0 // 40 ) );

end.
```

{{out}}

```txt

ALPHABETA
alphabeta

```



## APL

{{works with|APL2}}
       a←'abcdefghijklmnopqrstuvwxyz'
       A←'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

       X←'alphaBETA'

       (a,⎕AV)[(A,⎕AV)⍳'alphaBETA']
 alphabeta
       (A,⎕AV)[(a,⎕AV)⍳'alphaBETA']
 ALPHABETA

{{works with|APL}}
In the following example, puntuation is not covered. It is substituted by '*'.

```apl

    AlphLower←'abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    AlphUpper←'ABCDEFGHIJKLMNOPQRSTUVWXYZ ABCDEFGHIJKLMNOPQRSTUVWXYZ*'
    AlphUpper[AlphLower⍳'I'm using APL!']
I*M USING APL*

    AlphLower←'abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ*'
    AlphUpper←'ABCDEFGHIJKLMNOPQRSTUVWXYZ ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    AlphLower[AlphUpper⍳'I'm using APL!']
i*m using apl*

```

==={{header|APL (Dyalog)}}===
Dyalog APL has a function for case conversion, I-beam with code 819 (mnemonic, looks like 'BIg'). This is Unicode-aware and preserves punctuation; defaults to lowercase, or with left argument 0 does lowercase. Left argument 1 does uppercase:

```APL

      (819⌶) 'I''m using APL!'
i'm using apl!
      1 (819⌶) 'I''m using APL!'
I'M USING APL!

```



## AppleScript

{{Trans|JavaScript}}

AppleScript lacks built in string case functions, but since OS X 10.10 (Yosemite version, Oct 2014) it has been possible to use ObjC Foundation class methods directly in AppleScript code.


```applescript
use framework "Foundation"

-- TEST -----------------------------------------------------------------------
on run

    ap({toLower, toTitle, toUpper}, {"alphaBETA αβγδΕΖΗΘ"})

    --> {"alphabeta αβγδεζηθ", "Alphabeta Αβγδεζηθ", "ALPHABETA ΑΒΓΔΕΖΗΘ"}

end run


-- GENERIC FUNCTIONS ----------------------------------------------------------

-- toLower :: String -> String
on toLower(str)
    set ca to current application
    ((ca's NSString's stringWithString:(str))'s ¬
        lowercaseStringWithLocale:(ca's NSLocale's currentLocale())) as text
end toLower

-- toTitle :: String -> String
on toTitle(str)
    set ca to current application
    ((ca's NSString's stringWithString:(str))'s ¬
        capitalizedStringWithLocale:(ca's NSLocale's currentLocale())) as text
end toTitle

-- toUpper :: String -> String
on toUpper(str)
    set ca to current application
    ((ca's NSString's stringWithString:(str))'s ¬
        uppercaseStringWithLocale:(ca's NSLocale's currentLocale())) as text
end toUpper

-- A list of functions applied to a list of arguments
-- (<*> | ap) :: [(a -> b)] -> [a] -> [b]
on ap(fs, xs)
    set {nf, nx} to {length of fs, length of xs}
    set lst to {}
    repeat with i from 1 to nf
        tell mReturn(item i of fs)
            repeat with j from 1 to nx
                set end of lst to |λ|(contents of (item j of xs))
            end repeat
        end tell
    end repeat
    return lst
end ap

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

{{Out}}

```AppleScript
{"alphabeta αβγδεζηθ", "Alphabeta Αβγδεζηθ", "ALPHABETA ΑΒΓΔΕΖΗΘ"}
```



## Arbre


```arbre
main():
  uppercase('alphaBETA') + '\n' + lowercase('alphaBETA') + '\n' -> io
```

Output:

```txt

ALPHABETA
alphabeta

```



## Arturo



```arturo
str "alphaBETA"

print "uppercase  : " + $(uppercase str)
print "lowercase  : " + $(lowercase str)
print "capitalize : " + $(capitalize str)
```


{{out}}


```txt
uppercase  : ALPHABETA
lowercase  : alphabeta
capitalize : Alphabeta
```



## AutoHotkey



```autohotkey
a := "alphaBETA"
StringLower, b, a ; alphabeta
StringUpper, c, a ; ALPHABETA

StringUpper, d, a, T ; Alphabeta (T = title case) eg "alpha beta gamma" would become "Alpha Beta Gamma"
```



## AutoIt



```autoit
$sString = "alphaBETA"
$sUppercase = StringUpper($sString) ;"ALPHABETA"
$sLowercase = StringLower($sString) ;"alphabeta"
```



## AWK



```awk
BEGIN {
  a = "alphaBETA";
  print toupper(a), tolower(a)
}
```


Capitalize:

```awk
BEGIN {
  a = "alphaBETA";
  print toupper(substr(a, 1, 1)) tolower(substr(a, 2))
}
```



## BASIC

{{works with|QBasic}}

```qbasic
s$ = "alphaBETA"
PRINT UCASE$(s$)
PRINT LCASE$(s$)
```


=
## Applesoft BASIC
=

```ApplesoftBasic
S$ = "alphaBETA"

UP$ = "" : FOR I = 1 TO LEN(S$) : C = ASC(MID$(S$, I, 1)) : UP$ = UP$ + CHR$(C - (C > 96 AND C < 123) * 32) : NEXT I : ? UP$

LO$ = "" : FOR I = 1 TO LEN(S$) : C = ASC(MID$(S$, I, 1)) : LO$ = LO$ + CHR$(C + (C > 64 AND C < 91) * 32) : NEXT I : ? LO$
```


=
## BBC BASIC
=
{{works with|BBC BASIC for Windows}}

```bbcbasic
      INSTALL @lib$+"STRINGLIB"

      original$ = "alphaBETA"
      PRINT "Original:   " original$
      PRINT "Lower case: " FN_lower(original$)
      PRINT "Upper case: " FN_upper(original$)
      PRINT "Title case: " FN_title(original$)
```

Output:

```txt
Original:   alphaBETA
Lower case: alphabeta
Upper case: ALPHABETA
Title case: AlphaBETA
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 INPUT PROMPT "String:     ":TX$
110 PRINT "Lower case: ";LCASE$(TX$)
120 PRINT "Upper case: ";UCASE$(TX$)
```


=
## Liberty BASIC
=

```lb
input$ ="alphaBETA"

print input$
print upper$( input$)
print lower$( input$)

end
```


=
## PureBasic
=

```PureBasic
s$ = "alphaBETA"
upper$ = UCase(s$)  ;uppercase
lower$ = LCase(s$)  ;lowercase
```


=
## Run BASIC
=

```runbasic
a$ ="alphaBETA"

print a$           '=> alphaBETA
print upper$(a$)   '=> ALPHABETA
print lower$(a$)   '=> alphabeta
```


==={{header|TI-83 BASIC}}===
Note: While lowercase letters are built in to every TI-83/4/+/SE calculator, typing in lowercase is disabled by default and you have to hack the calculator to type in lowercase. However, the calculator does not have to be hacked to simply display lowercase output from a program, so on non-hacked calculators this program will only be useful one-way. To get lowercase letters, you have to create a new program with "AsmPrgmFDCB24DEC9" as the text, and then execute it on the homescreen with Asm(prgmYOURPROGRAMNAME). Then press [ALPHA] twice.

=
## Visual Basic .NET
=
{{works with|Visual Basic|2008}}

```vbnet
' Define 's'
Dim s AS String = "alphaBETA"

' Change 's' to Upper Case.
s =  s.ToUpper()

' Change 's' to Lower Case.
s = s.ToLower()
```



```ti83b
:"ABCDEFGHIJKLMNOPQRSTUVWXYZ"→Str9
:"abcdefghijklmnopqrstuvwxyz"→Str0
:Input ">",Str1
:":"+Str1+":"→Str1
:Prompt U
:If U:Then
:For(I,2,length(Str1))
:If inString(Str0,sub(Str1,I,1)) and sub(Str1,I,1)≠":"
:sub(Str1,1,I-1)+sub(Str9,inString(Str0,sub(Str1,I,1)),1)+sub(Str1,I+1,length(Str1)-I)→Str1
:End
:Else
:For(I,2,length(Str1))
:If inString(Str9,sub(Str1,I,1)) and sub(Str1,I,1)≠":"
:sub(Str1,1,I-1)+sub(Str0,inString(Str9,sub(Str1,I,1)),1)+sub(Str1,I+1,length(Str1)-I)→Str1
:End
:End
:sub(Str1,2,length(Str1)-2)→Str1
:Pause Str1
```



## Befunge

{{works with|befungee}}
Converts to uppercase only; lowercase is done in a similar way so I chose not to add it.

```Befunge
"ATEBahpla" > : #v_ 25* , @         >48*-v
                 > :: "`"` \"{"\` * |    > , v
                                    >    ^
            ^                                <
```



## Bracmat

The functions <code>upp$</code> and <code>low$</code> assume that strings are UTF-8 encoded, but if a string is not valid UTF-8, it is assumed the string is ISO-8859-1. Case conversion is not restricted to the Latin alphabet, but extends to all alphabets that have upper and lower case characters.

```bracmat
  "alphaBETA":?s
& out$str$(upp$!s \n low$!s)
```

Output:

```txt
ALPHABETA
alphabeta
```



## Burlesque



```burlesque

blsq ) "alphaBETA"^^zz\/ZZ
"ALPHABETA"
"alphabeta"

```



## C

The <tt>tolower</tt> and <tt>toupper</tt> functions are locale-aware.

```c
/* Demonstrate toupper and tolower for
   standard C strings.
   This does not work for multibyte character sets. */
#include <ctype.h>
#include <stdio.h>

/* upper-cases s in place */
void str_toupper(char *s)
{
    while(*s)
    {
        *s=toupper(*s);
        s++;
    }
}


/* lower-cases s in place */
void str_tolower(char *s)
{
    while(*s)
    {
        *s=tolower(*s);
        s++;
    }
}

int main(int argc, char *argv[])
{
    char t[255]="alphaBETA";
    str_toupper(t);
    printf("uppercase: %s\n", t);
    str_tolower(t);
    printf("lowercase: %s\n", t);
    return 0;
}
```



## C++

{{works with|g++| 3.4.4 (cygming special)}}

{{libheader|STL}}

This method does the transform in-place. Alternate methods might return a new copy or use a stream manipulator.


```cpp
#include <algorithm>
#include <string>
#include <cctype>

/// \brief in-place convert string to upper case
/// \return ref to transformed string
void str_toupper(std::string &str) {
  std::transform(str.begin(),
                 str.end(),
                 str.begin(),
                 (int(*)(int)) std::toupper);
}

/// \brief in-place convert string to lower case
/// \return ref to transformed string
void str_tolower(std::string &str) {
  std::transform(str.begin(),
                 str.end(),
                 str.begin(),
                 (int(*)(int)) std::tolower);
}
```


Here is sample usage code:


```cpp
#include <iostream>
#include <string>

using namespace std;
int main() {
  string foo("_upperCas3Me!!");
  str_toupper(foo);
  cout << foo << endl;
  str_tolower(foo);
  cout << foo << endl;
  return 0;
}
```


=={{header|C sharp|C#}}==


```csharp

class Program
{
    static void Main(string[] args)
    {
        string input;
        Console.Write("Enter a series of letters: ");
        input = Console.ReadLine();
        stringCase(input);
    }

    private static void stringCase(string str)
    {
        char[] chars = str.ToCharArray();
        string newStr = "";

        foreach (char i in chars)
            if (char.IsLower(i))
                newStr += char.ToUpper(i);
            else
                newStr += char.ToLower(i);
        Console.WriteLine("Converted: {0}", newStr);
    }
}
```


Title case is a little different:

```csharp
System.Console.WriteLine(System.Globalization.CultureInfo.CurrentCulture.TextInfo.ToTitleCase("exAmpLe sTrinG"));
```



## Clojure


```lisp
(def string "alphaBETA")
(println (.toUpperCase string))
(println (.toLowerCase string))
```



## CMake


```cmake
string(TOUPPER alphaBETA s)
message(STATUS "Uppercase: ${s}")
string(TOLOWER alphaBETA s)
message(STATUS "Lowercase: ${s}")
```



```txt
-- Uppercase: ALPHABETA
-- Lowercase: alphabeta
```



## COBOL

===Standard-compliant Methods===

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. string-case-85.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  example PIC X(9) VALUE "alphaBETA".

       01  result  PIC X(9).

       PROCEDURE DIVISION.
           DISPLAY "Example: " example

           *> Using the intrinsic functions.
           DISPLAY "Lower-case: " FUNCTION LOWER-CASE(example)

           DISPLAY "Upper-case: " FUNCTION UPPER-CASE(example)

           *> Using INSPECT
           MOVE example TO result
           INSPECT result CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
               TO "abcdefghijklmnopqrstuvwxyz"
           DISPLAY "Lower-case: " result

           MOVE example TO result
           INSPECT result CONVERTING "abcdefghijklmnopqrstuvwxyz"
               TO  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           DISPLAY "Upper-case: " result

           GOBACK
           .
```



### Compiler Extensions


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. string-case-extensions.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  example VALUE "alphaBETA".

       01  result  PIC X(9).

       PROCEDURE DIVISION.
           DISPLAY "Example: " example

           *> ACUCOBOL-GT
           MOVE example TO result
           CALL "C$TOLOWER" USING result, BY VALUE 9
           DISPLAY "Lower-case: " result

           MOVE example TO result
           CALL "C$TOUPPER" USING result, BY VALUE 9
           DISPLAY "Upper-case: " result

           *> Visual COBOL
           MOVE example TO result
           CALL "CBL_TOLOWER" USING result, BY VALUE 9
           DISPLAY "Lower-case: " result

           MOVE example TO result
           CALL "CBL_TOUPPER" USING result BY VALUE 9
           DISPLAY "Upper-case: " result

           GOBACK
           .
```



## ColdFusion


converting a string literal

```coldfusion
<cfset upper = UCase("alphaBETA")>
<cfset lower = LCase("alphaBETA")>
```


converting the value of a variable

```coldfusion
<cfset string = "alphaBETA">
<cfset upper = UCase(string)>
<cfset lower = LCase(string)>
```



## Common Lisp

You can use the ''string-upcase'' function to perform upper casing:


```lisp
CL-USER> (string-upcase "alphaBETA")
"ALPHABETA"
```


and you can do lower casing by using ''string-downcase'':


```lisp
CL-USER> (string-downcase "alphaBETA")
"alphabeta"
```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE AlphaBeta;
IMPORT StdLog,Strings;

PROCEDURE Do*;
VAR
	str,res: ARRAY 128 OF CHAR;
BEGIN
	str := "alphaBETA";
	Strings.ToUpper(str,res);
	StdLog.String("Uppercase:> ");StdLog.String(res);StdLog.Ln;
	Strings.ToLower(str,res);
	StdLog.String("Lowercase:> ");StdLog.String(res);StdLog.Ln
END Do;

END AlphaBeta.

```

Execute: ^Q AlphaBeta.Do<br/>
Output:

```txt

Uppercase:> ALPHABETA
Lowercase:> alphabeta

```


## D


```d
void main() {
    import std.stdio, std.string;

    immutable s = "alphaBETA";
    s.toUpper.writeln;
    s.toLower.writeln;
}
```

{{out}}

```txt
ALPHABETA
alphabeta
```


=={{header|Delphi}}[[Category:Object Pascal]]==

```pascal
writeln(uppercase('alphaBETA'));
writeln(lowercase('alphaBETA'));
```



## DWScript


```delphi
PrintLn(UpperCase('alphaBETA'));
PrintLn(LowerCase('alphaBETA'));
```



## Dyalect



```dyalect
const str = "alphaBETA"

print("Lower case: ", str.lower(), separator: "")
print("Upper case: ", str.upper(), separator: "")
print("Capitalize: ", str.capitalize(), separator: "")
```



## E


```e
["alphaBETA".toUpperCase(),
"alphaBETA".toLowerCase()]
```



## EchoLisp

EchoLisp includes the usual case conversion functions and the '''randcase''' function : random case

```scheme

(string-downcase "alphaBETA")
    → "alphabeta"
(string-upcase "alphaBETA")
    → "ALPHABETA"
(string-titlecase "alphaBETA")
    → "Alphabeta"
(string-randcase "alphaBETA")
    → "alphaBEtA"
(string-randcase "alphaBETA")
    → "AlPHaBeTA"

```



## ECL


```ECL
IMPORT STD; //Imports the Standard Library

STRING MyBaseString := 'alphaBETA';

UpperCased := STD.str.toUpperCase(MyBaseString);
LowerCased := STD.str.ToLowerCase(MyBaseString);
TitleCased := STD.str.ToTitleCase(MyBaseString);

OUTPUT (UpperCased);
OUTPUT (LowerCased);
OUTPUT (TitleCased);
```


## Elena

ELENA 4.x:

```elena
import system'culture;

public program()
{
    string s1 := "alphaBETA";

    // Alternative 1
    console.writeLine(s1.lowerCase());
    console.writeLine(s1.upperCase());

    // Alternative 2
    console.writeLine(s1.toLower(currentLocale));
    console.writeLine(s1.toUpper(currentLocale));
    console.readChar()
}
```



## Elixir

The String module provides the following functions:

```elixir

String.downcase("alphaBETA")
# => alphabeta
String.upcase("alphaBETA")
# => ALPHABETA
String.capitalize("alphaBETA")
# => Alphabeta

```

As with most String functions in Elixir, these are fully compatible with Unicode.

```elixir

String.downcase("αΒ")
# => αβ
String.upcase("αΒ")
# => ΑΒ
String.capitalize("αΒ")
# => Αβ

```



## Elm


```elm
import String exposing (toLower, toUpper)

s = "alphaBETA"

lower = toLower s
upper = toUpper s
```



## Erlang


```erlang
string:to_upper("alphaBETA").
string:to_lower("alphaBETA").
```



## Excel

Take 3 cells, say A1,B1 and C1. In B1 type :


```excel

=LOWER(A1)

```


and in C1 :


```excel

=UPPER(A1)

```


For the stated input in A1, the result will be :

<lang>
alphaBETA	alphabeta	ALPHABETA

```


=={{header|F Sharp|F#}}==

```fsharp

let s = "alphaBETA"
let upper = s.ToUpper()
let lower = s.ToLower()

```



## Factor


```factor
"alphaBETA" >lower  ! "alphabeta"
"alphaBETA" >upper  ! "ALPHABETA"
"alphaBETA" >title  ! "Alphabeta"
"ß" >case-fold      ! "ss"
```




## Falcon



```falcon
printl("alphaBETA".lower())
printl("alphaBETA".upper())
```



## Fantom



```fantom

fansh> a := "alphaBETA"
alphaBETA
fansh> a.upper // convert whole string to upper case
ALPHABETA
fansh> a.lower // convert whole string to lower case
alphabeta
fansh> a.capitalize  // make sure first letter is capital
AlphaBETA
fansh> "BETAalpha".decapitalize  // make sure first letter is not capital
bETAalpha

```



## Forth

ANS Forth does not have words to convert case for either strings or characters. For known alpha-numeric ASCII characters, the following can be used:
 : tolower ( C -- c ) 32 or ;
 : toupper ( c -- C ) 32 invert and ;
 : lower ( addr len -- ) over + swap  do i c@ tolower i c!  loop ;
 : upper ( addr len -- ) over + swap  do i c@ toupper i c!  loop ;

If the character range is unknown, these definitions are better:
 : tolower ( C -- c ) dup [char] A [char] Z 1+ within if 32 + then ;
 : toupper ( c -- C ) dup [char] a [char] z 1+ within if 32 - then ;

{{works with|Win32Forth| 4.2}}

 create s ," alphaBETA"
 s count type
 s count 2dup upper type
 s count 2dup lower type

Output:

 alphaBETA
 ALPHABETA
 alphabeta


## Fortran

{{works with|Fortran|90 and later}}

```fortran
 program example

   implicit none

   character(9) :: teststring = "alphaBETA"

   call To_upper(teststring)
   write(*,*) teststring
   call To_lower(teststring)
   write(*,*) teststring

 contains

   subroutine To_upper(str)
     character(*), intent(in out) :: str
     integer :: i

     do i = 1, len(str)
       select case(str(i:i))
         case("a":"z")
           str(i:i) = achar(iachar(str(i:i))-32)
       end select
     end do
   end subroutine To_upper

   subroutine To_lower(str)
     character(*), intent(in out) :: str
     integer :: i

     do i = 1, len(str)
       select case(str(i:i))
         case("A":"Z")
           str(i:i) = achar(iachar(str(i:i))+32)
       end select
     end do
   end subroutine To_Lower

 end program example
```


Functions could be used instead, especially with later compilers that enable lengths not fixed at compile time, but this involves copying the text about. By contrast, the subroutines alter the text in-place, though if something like <code>Utext = Uppercase(text)</code> is desired so that both versions are available, a subroutine is less convenient.

F90 introduced the intrinsic function  i = I'''A'''CHAR(c), which returns the integer value of the position of character c ''in the'' '''ASCII''' character set, even if the processor's default character set is different, such as perhaps '''EBCDIC'''. Function ACHAR is the inverse. If the bit pattern of the character was not being interpreted as in the ASCII set, this will cause odd results, say on a system using EBCDIC or (on an ASCII-using cpu) for a file originating from an EBCDIC-using system. Some systems offer additional options to the file OPEN statement that enable character conversion between ASCII and EBCDIC, and there may also be options concerning big- and little-endian usage. But much will depend on the format of the data in the file. If the data are a mixture of text, integers, floating-point, ''etc.'' all in binary, there will be no hope for such simple translations.

For converting lower-case text to upper, the following will work both on an ASCII system and an EBCDIC system (or any other encodement), once it is compiled for that system:
```Fortran
      SUBROUTINE UPCASE(TEXT)
       CHARACTER*(*) TEXT
       INTEGER I,C
        DO I = 1,LEN(TEXT)
          C = INDEX("abcdefghijklmnopqrstuvwxyz",TEXT(I:I))
          IF (C.GT.0) TEXT(I:I) = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"(C:C)
        END DO
      END
```

The INDEX function of course returning zero if the character is not found. Converting from upper to lower case is the obvious inverse and it might be worthwhile defining a MODULE with suitable named character constants to avoid repetition - one might hope the compiler will share duplicated constants rather than producing a fresh version every time, but it might not too. The repeated text scanning done by the INDEX function for each character of TEXT will of course be a lot slower. A still-more advanced compiler might be able to take advantage of special translation op-codes, on systems that offer them. If storage space is not at a premium a swifter method would be to create something like <code>CHARACTER*1 XLATUC(0:255)</code> with most entries being equal to their index, except for those corresponding to the lower case letters for which the value is the corresponding upper case letter. Then
```Fortran
      DO I = 1,LEN(TEXT)
        TEXT(I:I) = XLATUC(ICHAR(TEXT(I:I)))
      END DO
```


Note that in EBCDIC the offset is not 32 but 64. Rather than using an undocumented "magic constant" such as 32, one could define <code>PARAMETER (HIC = ICHAR("A") - ICHAR("a"))</code> instead or just place such code in-line and have hope for the compiler. This would also handle the small detail that "A" > "a" in EBCDIC rather than ASCII's "A" < "a". But alas, in EBCDIC the letter codes are ''not'' contiguous (there are many non-letter symbols between "a" and "z" as well as between "A" and "Z"), so the bounds of "A" to "Z" will ''not'' isolate only letters for attack. And it was not just IBM mainframes that used various versions of EBCDIC, so also did Burroughs, among others.

Here a complete example, using functions, and as far as I can tell, will work also with EBCDIC:


```Fortran

module uplow
   implicit none
   character(len=26), parameter, private :: low  = "abcdefghijklmnopqrstuvwxyz"
   character(len=26), parameter, private :: high = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
contains

   function to_upper(s) result(t)
      ! returns upper case of s
      implicit none
      character(len=*), intent(in) :: s
      character(len=len(s))        :: t

      character(len=1), save       :: convtable(0:255)
      logical, save                :: first = .true.
      integer                      :: i

      if(first) then
         do i=0,255
            convtable(i) = char(i)
         enddo
         do i=1,len(low)
            convtable(iachar(low(i:i))) = char(iachar(high(i:i)))
         enddo
         first = .false.
      endif

      t = s

      do i=1,len_trim(s)
         t(i:i) = convtable(iachar(s(i:i)))
      enddo

   end function to_upper

   function to_lower(s) result(t)
      ! returns lower case of s
      implicit none
      character(len=*), intent(in) :: s
      character(len=len(s))        :: t

      character(len=1), save :: convtable(0:255)
      logical, save          :: first = .true.
      integer                :: i

      if(first) then
         do i=0,255
            convtable(i) = char(i)
         enddo
         do i = 1,len(low)
            convtable(iachar(high(i:i))) = char(iachar(low(i:i)))
         enddo
         first = .false.
      endif

      t = s

      do i=1,len_trim(s)
         t(i:i) = convtable(iachar(s(i:i)))
      enddo

   end function to_lower


end module uplow


program doit
   use uplow
   character(len=40) :: s

   s = "abcdxyz ZXYDCBA _!@"
   print *,"original: ",'[',s,']'
   print *,"to_upper: ",'[',to_upper(s),']'
   print *,"to_lower: ",'[',to_lower(s),']'

end program doit

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim s As String = "alphaBETA"
Print UCase(s)
Print LCase(s)
Sleep
```


{{out}}

```txt

ALPHABETA
alphabeta

```



## Frink


```frink

a = "alphaBETA"
println[lc[a]]
println[uc[a]]

```


These functions use Unicode single- and multiple-character mapping tables and thus try to do the right thing with Unicode, possibly making the string longer in some cases:

```frink
uc["Imbiß"] // Last char is \u00df
```


Produces:

```txt
IMBISS
```


As the Unicode standard for casing states, "it is important to note that no casing operations on strings are reversible:"


```frink
lc[ uc["Imbiß"] ]
```


```txt
imbiss
```




## FutureBasic


```futurebasic

include "ConsoleWindow"

dim as Str255 a

a = "alphaBETA"

print a
print ucase$(a)
fn lcase(a)
print a

```


Output:

```txt

alphaBETA
ALPHABETA
alphabeta

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=d45e91bee011314fd126dc53052b5386 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sString As String = "alphaBETA "

Print UCase(sString)
Print LCase(sString)

End
```

Output:

```txt

ALPHABETA
alphabeta

```



## GAP


```gap
LowercaseString("alphaBETA");
UppercaseString("alphaBETA");
```



## GML


```GML
#define cases
{
    x = 'alphaBETA';
    y = string_upper(x); // returns ALPHABETA
    z = string_lower(x); // returns alphabeta
    show_message(y);
    show_message(z);
}
```



## Go

"Title case" in Go's Unicode package does not mean capitalize the first letter of each word, but rather capitalize each letter as if it were the first letter of a word.  The distinction matters in languages such as Croatian with digraphs with a capitialized form that is different from the all-caps form.  ToTitle() converts a string to all title case.

It is Title() on the other hand, that capitalizes the first letter of each word.  It identifies word boundaries and capitalizes first letters, leaving other letters unmodified.  As of Go 1.2 though, the word breaking algorithm is not Unicode compliant.

```go
package main

import (
    "fmt"
    "strings"
    "unicode"
    "unicode/utf8"
)

func main() {
    show("alphaBETA")
    show("alpha BETA")
    // Three digraphs that should render similar to DZ, Lj, and nj.
    show("Ǆǈǌ")
    // Unicode apostrophe in third word.
    show("o'hare O'HARE o’hare don't")
}

func show(s string) {
    fmt.Println("\nstring:         ",
        s, " len:", utf8.RuneCountInString(s), "runes") // DZLjnj
    fmt.Println("All upper case: ", strings.ToUpper(s)) // DZLJNJ
    fmt.Println("All lower case: ", strings.ToLower(s)) // dzljnj
    fmt.Println("All title case: ", strings.ToTitle(s)) // DzLjNj
    fmt.Println("Title words:    ", strings.Title(s))   // Dzljnj
    fmt.Println("Swapping case:  ",                     // DzLjNJ
        strings.Map(unicode.SimpleFold, s))
}
```

Output:

```txt

string:          alphaBETA  len: 9 runes
All upper case:  ALPHABETA
All lower case:  alphabeta
All title case:  ALPHABETA
Title words:     AlphaBETA
Swapping case:   ALPHAbeta

string:          alpha BETA  len: 10 runes
All upper case:  ALPHA BETA
All lower case:  alpha beta
All title case:  ALPHA BETA
Title words:     Alpha BETA
Swapping case:   ALPHA beta

string:          Ǆǈǌ  len: 3 runes
All upper case:  ǄǇǊ
All lower case:  ǆǉǌ
All title case:  ǅǈǋ
Title words:     ǅǈǌ
Swapping case:   ǅǉǊ

string:          o'hare O'HARE o’hare don't  len: 26 runes
All upper case:  O'HARE O'HARE O’HARE DON'T
All lower case:  o'hare o'hare o’hare don't
All title case:  O'HARE O'HARE O’HARE DON'T
Title words:     O'Hare O'HARE O’hare Don'T
Swapping case:   O'HARE o'hare O’HARE DON'T

```



## Groovy


```groovy
def str = 'alphaBETA'

println str.toUpperCase()
println str.toLowerCase()
```


Output:

```txt
ALPHABETA
alphabeta
```



## Haskell


```haskell
import Data.Char

s = "alphaBETA"

lower = map toLower s
upper = map toUpper s
```



## HicEst


```hicest
CHARACTER str = "alphaBETA"
EDIT(Text=str, UpperCase=LEN(str))
EDIT(Text=str, LowerCase=LEN(str))
EDIT(Text=str, UpperCase=1)
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
    write(map("alphaBETA"))
    write(map("alphaBETA",&lcase,&ucase))
end
```



## IDL

 str = "alphaBETA"
 print, str
 print, strupcase(str)
 print, strlowcase(str)


## J

Use standard utilities:

```j
   toupper 'alphaBETA'
ALPHABETA
   tolower 'alphaBETA'
alphabeta
```


or alternative definitions:

```j
upper=: {&((65+i.26) +&32@[} i.256)&.(a.&i.)
lower=: {&((97+i.26) -&32@[} i.256)&.(a.&i.)
```


For example:

```j
   upper 'alphaBETA'
ALPHABETA
   lower 'alphaBETA'
alphabeta
```



## Java


```java
String str = "alphaBETA";
System.out.println(str.toUpperCase());
System.out.println(str.toLowerCase());
//Also works with non-English characters with no modification
System.out.println("äàâáçñßæεбế".toUpperCase());
System.out.println("ÄÀÂÁÇÑSSÆΕБẾ".toLowerCase()); //does not transalate "SS" to "ß"
```


You could also easily create a <tt>swapCase</tt> method using <tt>Character.isLowerCase()</tt>, <tt>Character.isUpperCase()</tt>, and <tt>Character.isLetter()</tt>.


## JavaScript


```javascript
alert( "alphaBETA".toUpperCase() );
alert( "alphaBETA".toLowerCase() );
```


Output:
 ALPHABETA
 alphabeta

{{works with|NJS| 0.2.5}}

```javascript
var string = "alphaBETA";
var uppercase = string.toUpperCase();
var lowercase = string.toLowerCase();
```



## jq

If your version of jq does not have ascii_downcase and ascii_upcase, then you might want to use their definitions:

```jq
# like ruby's downcase - only characters A to Z are affected
def ascii_downcase:
  explode | map( if 65 <= . and . <= 90 then . + 32  else . end) | implode;

# like ruby's upcase - only characters a to z are affected
def ascii_upcase:
  explode | map( if 97 <= . and . <= 122 then . - 32  else . end) | implode;
```

'''Examples''':

```jq
"alphaBETA" | ascii_upcase
#=> "ALPHABETA"

"alphaBETA" | ascii_downcase
#=> "alphabeta"
```



## Jsish


```javascript
var msg = "alphaBETA";
;msg;
;msg.toUpperCase();
;msg.toLowerCase();

;msg.toTitle();
;msg.toLocaleUpperCase();
;msg.toLocaleLowerCase();
```


{{out}}

```txt
prompt$ jsish --U string-case.jsi
msg ==> alphaBETA
msg.toUpperCase() ==> ALPHABETA
msg.toLowerCase() ==> alphabeta
msg.toTitle() ==> Alphabeta
msg.toLocaleUpperCase() ==> ALPHABETA
msg.toLocaleLowerCase() ==> alphabeta
```



## Julia


```julia>julia
 uppercase("alphaBETA")
"ALPHABETA"

julia> lowercase("alphaBETA")
"alphabeta"
```



## K


```k

  s:"alphaBETA"
  upper:{i:_ic x; :[96<i; _ci i-32;_ci i]}'
  lower:{i:_ic x; :[91>i; _ci i+32;_ci i]}'
  upper s
"ALPHABETA"
  lower s
"alphabeta"

```



## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    val s = "alphaBETA"
    println(s.toUpperCase())
    println(s.toLowerCase())
    println(s.capitalize())
    println(s.decapitalize())
}
```


{{out}}

```txt

ALPHABETA
alphabeta
AlphaBETA
alphaBETA

```



## Lasso


```Lasso
// Direct string return
'alphaBETA'->uppercase&
'alphaBETA'->lowercase&

// Assignment and manipulation of variables
local(toupper = 'alphaBETA')
#toupper->uppercase
#toupper

local(tolower = 'alphaBETA')
#tolower->lowercase
#tolower
```



## Lingo

Lingo has no case conversion functions, but for ASCII strings they can e.g. be implemented like this:

```lingo
----------------------------------------
-- Lower to upper case (ASCII only)
-- @param {string} str
-- @return {string}
----------------------------------------
on toUpper (str)
  alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  len = str.length
  repeat with i = 1 to len
    pos = offset(str.char[i], alphabet)
    if pos > 0 then put alphabet.char[pos] into char i of str
  end repeat
  return str
end

----------------------------------------
-- Upper to lower case (ASCII only)
-- @param {string} str
-- @return {string}
----------------------------------------
on toLower (str)
  alphabet = "abcdefghijklmnopqrstuvwxyz"
  len = str.length
  repeat with i = 1 to len
    pos = offset(str.char[i], alphabet)
    if pos > 0 then put alphabet.char[pos] into char i of str
  end repeat
  return str
end
```



```lingo
put toUpper("alphaBETA")
-- "ALPHABETA"

put toLower("alphaBETA")
-- "alphabeta"
```



## LiveCode


```LiveCode
put upper("alphaBETA") && lower("alphaBETA")
ALPHABETA alphabeta
```



## Logo

 print uppercase "alphaBETA  ; ALPHABETA
 print lowercase "alphaBETA  ; alphabeta


## Lua


```Lua
str = "alphaBETA"
print( string.upper(str) )
print( string.lower(str) )
```



## M4


```M4
define(`upcase', `translit(`$*', `a-z', `A-Z')')
define(`downcase', `translit(`$*', `A-Z', `a-z')')

define(`x',`alphaBETA')
upcase(x)
downcase(x)
```



## Maple


```Maple
str := "alphaBETA";
StringTools:-UpperCase(str);
StringTools:-LowerCase(str);
```

produces

```txt
     alphabeta
```


```txt
     ALPHABETA
```



## Mathematica


```Mathematica
str="alphaBETA";
ToUpperCase[str]
ToLowerCase[str]
```

gives:

```txt
 ALPHABETA
 alphabeta
```



## MATLAB


```MATLAB>>
 upper('alphaBETA')

ans =

ALPHABETA

>> lower('alphaBETA')

ans =

alphabeta
```



## Maxima


```Maxima
supcase('alphaBETA');
sdowncase('alphaBETA');
```



## MAXScript

Requires MAX 2008

```maxscript
str = "alphaBETA"
print (toUpper str)
print (toLower str)
```



## Mercury

The functions to_upper/1, to_lower/1, capitalize_first/1 and uncapitalize_first/1
only affect unaccented Latin characters.

<lang>:- module string_case.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list, string.

main(!IO) :-
   S = "alphaBETA",
   io.format("uppercase       : %s\n", [s(to_upper(S))], !IO),
   io.format("lowercase       : %s\n", [s(to_lower(S))], !IO),
   io.format("capitalize first: %s\n", [s(capitalize_first(S))], !IO).
   % We can use uncaptitalize_first/1 to ensure the first character in a
   % string is lower-case.
```



## Metafont

We need to implement it, since it is not already given; the following code works only for ASCII or ASCII based encodings. (It could work anyway also for single byte encodings where letters are contiguous).


```metafont
vardef isbetween(expr a, i, f) =
  if string a:
    if (ASCII(a) >= ASCII(i)) and (ASCII(a) <= ASCII(f)):
      true
    else:
      false
    fi
  else:
    false
  fi enddef;

vardef toupper(expr s) =
  save ?; string ?; ? := ""; d := ASCII"A" - ASCII"a";
  for i = 0 upto length(s)-1:
    if isbetween(substring(i, i+1) of s, "a", "z"):
      ? := ? & char(ASCII(substring(i,i+1) of s) + d)
    else:
      ? := ? & substring(i, i+1) of s
    fi;
  endfor
  ?
enddef;

vardef tolower(expr s) =
  save ?; string ?; ? := ""; d := ASCII"a" - ASCII"A";
  for i = 0 upto length(s)-1:
    if isbetween(substring(i, i+1) of s, "A", "Z"):
      ? := ? & char(ASCII(substring(i,i+1) of s) + d)
    else:
      ? := ? & substring(i, i+1) of s
    fi;
  endfor
  ?
enddef;
```



```metafont
message toupper("alphaBETA");
message tolower("alphaBETA");

end
```



## min

{{works with|min|0.19.3}}

```min
"alphaBETA" uppercase
"alphaBETA" lowercase
"alphaBETA" capitalize
```



## MiniScript


```MiniScript
mixedString = "alphaBETA"
print "Upper Case of " + mixedString + " is " + mixedString.upper
print "Lower Case of " + mixedString + " is " + mixedString.lower
```

{{out}}

```txt

Upper Case of alphaBETA is ALPHABETA
Lower Case of alphaBETA is alphabeta

```



## mIRC Scripting Language


```mirc
echo -ag $upper(alphaBETA)
echo -ag $lower(alphaBETA)
```


=={{header|Modula-3}}==

```modula3
MODULE TextCase EXPORTS Main;

IMPORT IO, Text, ASCII;

PROCEDURE Upper(txt: TEXT): TEXT =
  VAR
    len := Text.Length(txt);
    res := "";
  BEGIN
    FOR i := 0 TO len - 1 DO
      res := Text.Cat(res, Text.FromChar(ASCII.Upper[Text.GetChar(txt, i)]));
    END;
    RETURN res;
  END Upper;

PROCEDURE Lower(txt: TEXT): TEXT =
  VAR
    len := Text.Length(txt);
    res := "";
  BEGIN
    FOR i := 0 TO len - 1 DO
      res := Text.Cat(res, Text.FromChar(ASCII.Lower[Text.GetChar(txt, i)]));
    END;
    RETURN res;
  END Lower;

BEGIN
  IO.Put(Upper("alphaBETA\n"));
  IO.Put(Lower("alphaBETA\n"));
END TextCase.
```

Output:

```txt

ALPHABETA
alphabeta

```


## MUMPS


```MUMPS

STRCASE(S)
 SET UP="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
 SET LO="abcdefghijklmnopqrstuvwxyz"
 WRITE !,"Given: "_S
 WRITE !,"Upper: "_$TRANSLATE(S,LO,UP)
 WRITE !,"Lower: "_$TRANSLATE(S,UP,LO)
 QUIT

```

Output:
```txt

USER>DO STRCASE^ROSETTA("alphaBETA")

Given: alphaBETA
Upper: ALPHABETA
Lower: alphabeta

```



## Nemerle


```Nemerle
using System.Console;
using System.Globalization;

module StringCase
{
    Main() : void
    {
        def alpha = "alphaBETA";
        WriteLine(alpha.ToUpper());
        WriteLine(alpha.ToLower());

        WriteLine(CultureInfo.CurrentCulture.TextInfo.ToTitleCase("exAmpLe sTrinG"));

    }
}
```



## NetRexx


```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols

abc = 'alphaBETA'

say abc.upper
say abc.lower
say abc.upper(1, 1) -- capitalize 1st character

```



## NewLISP


```NewLISP

(upper-case "alphaBETA")
(lower-case "alphaBETA")

```



## Nial


```nial
toupper 'alphaBETA'
=ALPHABETA
tolower 'alphaBETA'
=alphabeta
```



## Nim


```nim
import strutils

var s: string = "alphaBETA_123"
echo s," as upper case: ", toUpper(s)
echo s," as lower case: ", toLower(s)
echo s," as Capitalized: ", capitalize(s)
echo s," as normal case: ", normalize(s)  # remove underscores, toLower
```

{{out}}

```txt
alphaBETA_123 as upper case: ALPHABETA_123
alphaBETA_123 as lower case: alphabeta_123
alphaBETA_123 as Capitalized: AlphaBETA_123
alphaBETA_123 as normal case: alphabeta123
```



## Objeck


```objeck

string := "alphaBETA";
string->ToUpper()->PrintLine();
string->ToLower()->PrintLine();

```


=={{header|Objective-C}}==
{{works with|GNUstep}}
{{works with|Cocoa}}

```objc
NSLog(@"%@", @"alphaBETA".uppercaseString);
NSLog(@"%@", @"alphaBETA".lowercaseString);

NSLog(@"%@", @"foO BAr".capitalizedString); // "Foo Bar"
```



## OCaml


```ocaml
let () =
  let str = "alphaBETA" in
  print_endline (String.uppercase_ascii str); (* ALPHABETA *)
  print_endline (String.lowercase_ascii str); (* alphabeta *)

  print_endline (String.capitalize_ascii str); (* AlphaBETA *)
;;
```



## Octave


```octave
s = "alphaBETA";
slc = tolower(s);
suc = toupper(s);
disp(slc);
disp(suc);
```



## Oforth



```Oforth
"alphaBETA" toUpper
"alphaBETA" toLower
```



## OpenEdge/Progress

<lang Progress (OpenEdge ABL)>caps("alphaBETA")
lc("alphaBETA")

```



## Oz

Convert to upper/lower-case:

```oz
declare
  Str = "alphaBETA"
in
  {System.showInfo {Map Str Char.toUpper}}
  {System.showInfo {Map Str Char.toLower}}
```


Capitalize:

```oz
declare
  [StringX] = {Link ['x-oz://system/String.ozf']}
in
  {System.showInfo {StringX.capitalize "alphaBETA"}} %% prints "AlphaBETA"
```



## Pascal



```pascal

// Uppercase and Lowercase functions for a minimal standard Pascal
// where no library routines for these operations exist
PROGRAM upperlower;

// convert a character to uppercase
FUNCTION uch(ch: CHAR): CHAR;
	BEGIN
		uch := ch;
		IF ch IN ['a'..'z'] THEN
			uch := chr(ord(ch) AND $5F);
	END;

// convert a character to lowercase
FUNCTION lch(ch: CHAR): CHAR;
	BEGIN
		lch := ch;
		IF ch IN ['A'..'Z'] THEN
			lch := chr(ord(ch) OR $20);
	END;

// toggle uper/lower case character
FUNCTION ulch(ch: CHAR): CHAR;
	BEGIN
		ulch := ch;
		IF ch IN ['a'..'z'] THEN ulch := uch(ch);
		IF ch IN ['A'..'Z'] THEN ulch := lch(ch);
	END;

// convert a string to uppercase
FUNCTION ucase(str: STRING): STRING;
	var i: Integer;
	BEGIN
		ucase := '';
		FOR i := 1 TO Length(str) DO
			ucase := ucase + uch(str[i]);
	END;

// convert a string to lowercase
FUNCTION lcase(str: STRING): STRING;
	var i: Integer;
	BEGIN
		lcase := '';
		FOR i := 1 TO Length(str) DO
			lcase := lcase + lch(str[i]);
	END;

// reverse cases in a given string
FUNCTION ulcase(str: STRING): STRING;
	var i: Integer;
	BEGIN
		ulcase := '';
		FOR i := 1 TO Length(str) DO
			ulcase := ulcase + ulch(str[i]);
	END;

VAR
	ab : STRING = 'alphaBETA';

BEGIN
	// demonstration
	Writeln('Original string : ',ab);
	Writeln('Reversed case   : ',ulcase(ab));
	Writeln('Upper case      : ',ucase(ab));
	Writeln('Lower case      : ',lcase(ab));
END.

```


Demonstration:


```txt

Original string : alphaBETA
Reversed case   : ALPHAbeta
Upper case      : ALPHABETA
Lower case      : alphabeta

```



## Peloton

Iterating through the peerset

```sgml
<@ ENU$$$LSTPSTLITLIT>UPP|
[<@ SAYELTLST>...</@>] <@ SAYHLPELTLST>...</@><@ DEFKEYELTLST>__SuperMacro|...</@>
<@ SAY&&&LIT>alphaBETA</@>

</@>
```


Same code in padded-out, variable-length English dialect

```sgml><# ENUMERATION LAMBDA LIST PEERSET LITERAL LITERAL
UPP|
[<# SAY ELEMENT LIST>...</#>] <# SAY HELP ELEMENT LIST>...</#><# DEFINE KEYWORD ELEMENT LIST>__SuperMacro|...</#>
<# SAY SUPERMACRO LITERAL>alphaBETA</#>

</#>
```


Output.

```txt
[FLC] 410400001 Flip case (410400001)  [Transformers^Abstract type transforms^Text transformers^Platform relative encoding and encrypting]
ALPHAbeta

[LOW] 410400002 Lower case (410400002)  [Transformers^Abstract type transforms^Text transformers^Platform relative encoding and encrypting]
alphabeta

[PRP] 410400003 Proper Case (410400003)  [Transformers^Abstract type transforms^Text transformers^Platform relative encoding and encrypting]
Alphabeta

[SNT] 410400004 Sentence case (410400004)  [Transformers^Abstract type transforms^Text transformers^Platform relative encoding and encrypting]
Alphabeta

[UPP] 410400005 Upper case (410400005)  [Transformers^Abstract type transforms^Text transformers^Platform relative encoding and encrypting]
ALPHABETA


```



## Perl

{{works with|Perl|5.x}}

```perl
my $string = "alphaBETA";
print uc($string), "\n"; # => "ALPHABETA"
print lc($string), "\n"; # => "alphabeta"
$string =~ tr/[a-z][A-Z]/[A-Z][a-z]/; print "$string\n"; # => ALPHAbeta

print ucfirst($string), "\n"; # => "AlphaBETA"
print lcfirst("FOObar"), "\n"; # => "fOObar"
```


Also works in Perl 4 if the '''my''' is removed.


## Perl 6

In Perl 6, case modification is implemented as builtin subroutine or method:


```perl6
my $word = "alpha BETA" ;
say uc $word;         # all uppercase (subroutine call)
say $word.uc;         # all uppercase (method call)
# from now on we use only method calls as examples
say $word.lc;         # all lowercase
say $word.tc;         # first letter titlecase
say $word.tclc;       # first letter titlecase, rest lowercase
say $word.wordcase;   # capitalize each word

```

Output:

```txt
ALPHA BETA
alpha beta
Alpha BETA
Alpha beta
Alpha Beta
```



## Phix


```Phix
constant s = "alphaBETA"
?upper(s)
?lower(s)
```

There is also a bespoke convertCase function in demo\Edix\Edix.exw which accepts five operators: LOWER, UPPER, CAPITALISE, SENTENCE, and INVERT, which is obviously not part of the language, and a bit too long, messy, ugly, and unedifying, to bother reproducing here.


## PHP


```php
$str = "alphaBETA";
echo strtoupper($str), "\n"; // ALPHABETA
echo strtolower($str), "\n"; // alphabeta

echo ucfirst($str), "\n"; // AlphaBETA
echo lcfirst("FOObar"), "\n"; // fOObar
echo ucwords("foO baR baZ"), "\n"; // FoO BaR BaZ
echo lcwords("FOo BAr BAz"), "\n"; // fOo bAr bAz
```



## PicoLisp


```PicoLisp
(let Str "alphaBETA"
   (prinl (uppc Str))
   (prinl (lowc Str)) )
```



## PL/I


```pli

declare s character (20) varying initial ('alphaBETA');

put skip list (uppercase(s));
put skip list (lowercase(s));

```



```pli

/* An alternative to the above, which might be used if some */
/* non-standard conversion is required, is shown for        */
/* converting to upper case:                                */
put skip list ( translate(s, 'abcdefghijklmnopqrstuvwxyz',
                             'ABCDEFGHIJKLMNOPQRSTUVWXYZ') );

```



## PL/SQL


```plsql
declare
    vc  VARCHAR2(40) := 'alphaBETA';
    ivc VARCHAR2(40);
    lvc VARCHAR2(40);
    uvc VARCHAR2(40);
begin
    ivc := INITCAP(vc); -- 'Alphabeta'
    lvc := LOWER(vc);   -- 'alphabeta'
    uvc := UPPER(vc);   -- 'ALPHABETA'
end;
```



## Pop11


```pop11
lvars str = 'alphaBETA';
lowertoupper(str) =>
uppertolower(str) =>
```



## Potion


```potion
lowercase = (str) :
   low = ("")
   str length times (i) :
      low append(if (65 <= str(i) ord and str(i) ord <= 90) :
         "abcdefghijklmnopqrstuvwxyz"(str(i) ord - 65)
      . else :
         str(i)
      .)
   .
   low join("")
.
uppercase = (str) :
   upp = ("")
   str length times (i) :
      upp append(if (97 <= str(i) ord and str(i) ord <= 122) :
         "ABCDEFGHIJKLMNOPQRSTUVWXYZ"(str(i) ord - 97)
      . else :
         str(i)
      .)
   .
   upp join("")
.

lowercase("alphaBETA") print
uppercase("alphaBETA") print
```



## Powerbuilder


```powerbuilder
string ls_string
ls_string = 'alphaBETA'
ls_string  = Upper(ls_string)
ls_string  = Lower(ls_string)
```



## PowerShell


```powershell

$string = 'alphaBETA'
$lower  = $string.ToLower()
$upper  = $string.ToUpper()
$title  = (Get-Culture).TextInfo.ToTitleCase($string)

$lower, $upper, $title

```

{{Out}}

```txt

alphabeta
ALPHABETA
Alphabeta

```



## Python


```python
s = "alphaBETA"
print s.upper() # => "ALPHABETA"
print s.lower() # => "alphabeta"

print s.swapcase() # => "ALPHAbeta"

print "fOo bAR".capitalize() # => "Foo bar"
print "fOo bAR".title() # => "Foo Bar"

import string
print string.capwords("fOo bAR") # => "Foo Bar"
```


string.capwords() allows the user to define word separators, and by default behaves slightly differently than title().


```python
print "foo's bar".title()          # => "Foo'S Bar"
print string.capwords("foo's bar") # => "Foo's Bar"
```



## QB64


```QB64

DIM s AS STRING * 9
s = "alphaBETA"
PRINT "The original string: " + s
PRINT ""
PRINT "Translated to lowercase: " + LCASE$(s)
PRINT "Translated to uppercase: " + UCASE$(s)

```



## R


```R
 str <- "alphaBETA"
 toupper(str)
 tolower(str)
```



## Racket


```Racket
#lang racket
(define example "alphaBETA")

(string-upcase example)
;"ALPHABETA"
(string-downcase example)
;"alphabeta"
(string-titlecase example)
;"Alphabeta"
```



## Raven


```raven
'alphaBETA' upper
'alhpaBETA' lower
```



## REBOL


```REBOL
print ["Original: " original: "alphaBETA"]
print ["Uppercase:" uppercase original]
print ["Lowercase:" lowercase original]
```


Output:


```txt
Original:  alphaBETA
Uppercase: ALPHABETA
Lowercase: alphabeta
```



## Red


```Red
str: "alphaBETA"
>> uppercase str
== "ALPHABETA"
>> lowercase str
== "alphabeta"
>> uppercase/part str 5
== "ALPHAbeta"
```



## Retro


```Retro
with strings'
"alphaBETA" toUpper puts
"alphaBETA" toLower puts
```



## REXX


### with TRANSLATE BIF

The following code will execute correctly in   '''ASCII'''   and   '''EBCDIC.

```rexx
abc  = "abcdefghijklmnopqrstuvwxyz"              /*define all  lowercase  Latin letters.*/
abcU = translate(abc)                            /*   "    "   uppercase    "      "    */

x = 'alphaBETA'                                  /*define a string to a REXX variable.  */
y = translate(x)                                 /*uppercase  X  and store it ───►  Y   */
z = translate(x, abc, abcU)                      /*translate uppercase──►lowercase chars*/
```


===with PARSE UPPER &amp; PARSE LOWER statements===
The following code will execute correctly in   '''ASCII'''   and   '''EBCDIC.

```rexx
x = "alphaBETA"                                  /*define a string to a REXX variable.  */
parse upper var x y                              /*uppercase  X  and  store it ───►  Y  */
parse lower var x z                              /*lowercase  X   "     "    " ───►  Z  */

                 /*Some REXXes don't support the  LOWER  option for the  PARSE  command.*/
```


===with UPPER &amp; LOWER BIFs===
The following code will execute correctly in   '''ASCII'''   and   '''EBCDIC.

```rexx
x = 'alphaBETA'                                  /*define a string to a REXX variable.  */
y = upper(x)                                     /*uppercase  X  and  store it ───►  Y  */
z = lower(x)                                     /*lowercase  X   "     "    " ───►  Z  */

         /*Some REXXes don't support the  UPPER  and  LOWER  BIFs  (built-in functions).*/
```



### with UPPER statement

The following code will execute correctly in   '''ASCII'''   and   '''EBCDIC.

```rexx
x = "alphaBETA"                                  /*define a string to a REXX variable.  */
y=x;   upper y                                   /*uppercase  X  and  store it ───►  Y  */
parse lower var x z                              /*lowercase  Y   "     "    " ───►  Z  */

                 /*Some REXXes don't support the  LOWER  option for the  PARSE  command.*/
```



### with capitalized words

The following code will execute correctly in   '''ASCII'''   and   '''EBCDIC.

```rexx
/*REXX program  capitalizes  each word in string, and maintains imbedded blanks.        */
x= "alef bet gimel dalet he vav zayin het tet yod kaf lamed mem nun samekh",
   "ayin pe tzadi qof resh shin  tav."           /*the "old" spelling of Hebrew letters.*/
y= capitalize(x)                                 /*capitalize each word in the string.  */
say x                                            /*display the original string of words.*/
say y                                            /*   "     "    capitalized      words.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
capitalize: procedure;  parse arg z;   $=' 'z    /*prefix    $    string with a  blank. */
            abc = "abcdefghijklmnopqrstuvwxyz"   /*define all  Latin  lowercase letters.*/

                      do j=1  for 26             /*process each letter in the alphabet. */
                      _=' 'substr(abc,j,1); _U=_ /*get a  lowercase  (Latin) letter.    */
                      upper _U                   /* "  "  uppercase     "       "       */
                      $=changestr(_, $, _U)      /*maybe capitalize some word(s).       */
                      end   /*j*/

            return substr($, 2)                  /*return the capitalized words.        */
```

Some older REXXes don't have a   '''changestr'''   BIF, so one is included here   ───►   [[CHANGESTR.REX]].


{{out|output|:}}

```txt

alef bet gimel dalet he vav zayin het tet yod kaf lamed mem nun samekh ayin pe tzadi qof resh shin  tav.
Alef Bet Gimel Dalet He Vav Zayin Het Tet Yod Kaf Lamed Mem Nun Samekh Ayin Pe Tzadi Qof Resh Shin  Tav.

```

Note:   there are many variant spellings of the Hebrew alphabet.





### with case swap

The following code will execute correctly in   '''ASCII'''   and   '''EBCDIC.

```rexx
/*REXX program  swaps the letter case of a string:  lower ──► upper  &  upper ──► lower.*/
abc = "abcdefghijklmnopqrstuvwxyz"               /*define all the  lowercase  letters.  */
abcU = translate(abc)                            /*   "    "   "   uppercase     "      */

x = 'alphaBETA'                                  /*define a string to a REXX variable.  */
y = translate(x,  abc || abcU,  abcU || abc)     /*swap case of  X  and store it ───► Y */
say x
say y
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''

```txt

alphaBETA
ALPHAbeta

```



### version 2


```rexx

x='alphaBETA';                    Say '  x='||x
                                  Say 'three ways to uppercase'
u1=translate(x);                  Say '  u1='u1
u2=upper(x);                      Say '  u2='u2
parse upper var x u3;             Say '  u3='u3

abc ='abcdefghijklmnopqrstuvwxyz'
abcu=translate(abc);              Say 'three ways to lowercase'
l1=translate(x,abc,abcu);         Say '  l1='l1
l2=lower(x);                      Say '  l2='l2
parse lower var x l3;             Say '  l3='l3

```

:Note: Parse options upper and lower not available in every Rexx
:Builtin functions upper and lower not available in every Rexx
:Upper instruction not available in ooRexx

For German input (considering umlaute) these will uppercase them:

```rexx

uppercase: /*courtesy Gerard Schildberger */
 return translate(changestr("ß",translate(arg(1),'ÄÖÜ',"äöü"),'SS'))

uppercase2: Procedure
Parse Arg a
  a=translate(arg(1),'ÄÖÜ',"äöü")     /* translate lowercase umlaute */
  a=changestr("ß",a,'SS')             /* replace ß with SS           */
  return translate(a)                 /* translate lowercase letters */

```

Translation to lowercase is not similarly possible because of 'SS'->'ß' or -> 'ss' ??


Note: Recently an uppercase ß was introduced in Austria. I haven't used it yet.


## Ring


```ring

aString = "WELCOME TO THE ring programming language"
see lower(aString) + nl
see upper(aString) + nl

```



## Ruby


```ruby
"alphaBETA".downcase # => "alphabeta"
"alphaBETA".upcase # => "ALPHABETA"

"alphaBETA".swapcase # => "ALPHAbeta"
"alphaBETA".capitalize # => "Alphabeta"
```


These methods used to affect ASCII letters A-Z and a-z only. From Ruby 2.4 onward however, these methods support Full Unicode case mapping, suitable for most languages, by default. (Options can be specified for Turkic, Lithuanian and ascii)
{{works with|Ruby| 2.4}}

```ruby
'ĥåçýджк'.upcase  # => "ĤÅÇÝДЖК"
```



## Rust

{{works with|Rust| 1.3}}

```rust
fn main() {
    println!("{}", "jalapeño".to_uppercase()); // JALAPEÑO
    println!("{}", "JALAPEÑO".to_lowercase()); // jalapeño
}
```



## Scala


```Scala
val s="alphaBETA"
println(s.toUpperCase)   //-> ALPHABETA
println(s.toLowerCase)   //-> alphabeta
println(s.capitalize)    //-> AlphaBETA
println(s.reverse)       //-> ATEBahpla
```



## Scheme


```scheme
(define s "alphaBETA")
(list->string (map char-upcase (string->list s)))
(list->string (map char-downcase (string->list s)))
```


Using SRFI-13:


```scheme

> (define s "alphaBETA gammaDELTA")
> (string-upcase s)     ;; turn all into upper case
"ALPHABETA GAMMADELTA"
> (string-downcase s)   ;; turn all into lower case
"alphabeta gammadelta"
> (string-titlecase s)  ;; capitalise start of each word
"Alphabeta Gammadelta"

```



## Sed

Piping through sed in bash:


```bash
echo "alphaBETA" | sed 'y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/'
echo "alphaBETA" | sed 'y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/'
```


Other functions:

```bash
# Invert case
echo "alphaBETA" | sed 'y/ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz/abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ/'
```


GNU sed supports special sequences to change case:

```bash

# to uppercase
$ echo alphaBETA | sed 's/.*/\U&/'
ALPHABETA
# to lowercase
$ echo alphaBETA | sed 's/.*/\L&/'
alphabeta

```



## Seed7

 writeln(upper("alphaBETA"));
 writeln(lower("alphaBETA"));


## Sidef


```ruby
say "alphaBETA".lc;             #=> alphabeta
say "alphaBETA".uc;             #=> ALPHABETA
say "alphaBETA".tc;             #=> AlphaBETA
say "alpha BETA".wc;            #=> Alpha Beta
say "alpha BETA".tc;            #=> Alpha BETA
say "alpha BETA".tclc;          #=> Alpha beta
```


## Simula


```Simula
TEXT soup, lower;
soup :- "alphaBETA";
lower :- LOWCASE(COPY(soup)); ! COPY, else soup is changed;
OutText("upper: "); OutText(UPCASE("alphaBETA"));
OutText(", lower: "); OutText(lower);
OutText(", soup: "); OutText(soup); Outimage;
```



## Slate


```slate
'alphaBETA' toLowercase.
'alphaBETA' toUppercase.
```



## Smalltalk


```smalltalk
'ALPHAbeta' asUppercase  "->'ALPHABETA' "
'ALPHAbeta' asLowercase "-> 'alphabeta' "
```

{{works with|Smalltalk/X}}
{{works with|VisualWorks Smalltalk}}

```smalltalk
'alphabeta' asUppercaseFirst "-> 'Alphabeta' "
```

Unicode (notice, that this cannot be done simply with a straight forward "ch := ch -$a + $A" loop):
{{works with|Smalltalk/X}} (may with others too, but I have not verified)

```smalltalk
'ĥåçýджк' asUppercase "->  'ĤÅÇÝДЖК'
```



## SNOBOL4


There are no standard Snobol libraries or case conversion built-ins. But case functions are easy to roll using the character class keywords. Native charset only.


```SNOBOL4
        define('uc(str)') :(uc_end)
uc      uc = replace(str,&lcase,&ucase) :(return)
uc_end

        define('lc(str)') :(lc_end)
lc      lc = replace(str,&ucase,&lcase) :(return)
lc_end

        define('ucfirst(str)ch') :(ucfirst_end)
ucfirst str len(1) . ch = uc(ch)
        ucfirst = str :(return)
ucfirst_end

        define('swapc(str)') :(swapc_end)
swapc   str = replace(str,&ucase &lcase, &lcase &ucase)
        swapc = str :(return)
swapc_end

*       # Test and display
        str = 'alphaBETA'
        output = str
        output = lc(str)
        output = uc(str)
        output = ucfirst(str)
        output = swapc(str)
end
```


An alternative way of constructing the above that groups related functions together in a denser display:


```SNOBOL4
        define('UC(STR)')
        define('LC(STR)')
        define('UCFIRST(STR)')
        define('SWAPC(STR)')                               :(CASES.END)
UC      uc = replace(str,&lcase,&ucase)                    :(RETURN)
LC      lc = replace(str,&ucase,&lcase)                    :(RETURN)
UCFIRST str len(1) . ch = uc(ch) ; ucfirst = str           :(RETURN)
SWAPC   swapc = replace(str, &ucase &lcase, &lcase &ucase) :(RETURN)
CASES.END

*       # Test and display
        str = 'alphaBETA'
        output = str
        output = lc(str)
        output = uc(str)
        output = ucfirst(str)
        output = swapc(str)
END
```


Output:

```txt
alphaBETA
alphabeta
ALPHABETA
AlphaBETA
ALPHAbeta
```



## SQL

{{works with|MS SQL| 2005}}

```sql
declare @s varchar(10)
set @s = 'alphaBETA'
print upper(@s)
print lower(@s)
```



## SQL PL

{{works with|Db2 LUW}}
With SQL only:

```sql pl

values upper('alphaBETA');
values lower('alphaBETA');
values initcap('alphaBETA');
-- Within a SQL query.
select upper('alphaBETA') from sysibm.sysdummy1;

```

Output:

```txt

db2 -t
db2 => values upper('alphaBETA');

1
---------
ALPHABETA

  1 record(s) selected.

db2 => values lower('alphaBETA');

1
---------
alphabeta

  1 record(s) selected.

db2 => values initcap('alphaBETA');

1
---------
Alphabeta

  1 record(s) selected.
db2 => select upper('alphaBETA') from sysibm.sysdummy1;

1
---------
ALPHABETA

  1 record(s) selected.


```



## Standard ML


```sml
val strupr = String.map Char.toUpper;
val strlwr = String.map Char.toLower;
```


Test

 - strupr "alphaBETA";
 <i>val it = "ALPHABETA" : string</i>
 - strlwr "alphaBETA";
 <i>val it = "alphabeta" : string</i>


## Stata


Use '''[https://www.stata.com/help.cgi?f_strupper strupper]''' and '''strlower''' to change case of ASCII characters. Use '''[https://www.stata.com/help.cgi?f_ustrupper ustrupper]''' and '''ustrlower''' to change case of all Unicode letters.


```stata
. scalar s="alphaBETA"
. di strupper(s)
ALPHABETA
. di strlower(s)
alphabeta
```


Notice there may be some difficulties with Unicode characters. In the following, the uppercase '''[https://en.wikipedia.org/wiki/Sigma sigma]''' is correctly converted back to the lowercase variant, but the '''[https://en.wikipedia.org/wiki/Iota_subscript iota subscript]''' is not.


```stata
. scalar a="Ἐν ἀρχῇ ἐποίησεν ὁ θεὸς τὸν οὐρανὸν καὶ τὴν γῆν"
. scalar b=ustrupper(a)
. di b
ἘΝ ἈΡΧΗ͂Ι ἘΠΟΊΗΣΕΝ Ὁ ΘΕῸΣ ΤῸΝ ΟΥ̓ΡΑΝῸΝ ΚΑῚ ΤῊΝ ΓΗ͂Ν
. di ustrlower(b)
ἐν ἀρχῆι ἐποίησεν ὁ θεὸς τὸν οὐρανὸν καὶ τὴν γῆν
```



## Swift


```swift
import Foundation

println("alphaBETA".uppercaseString)
println("alphaBETA".lowercaseString)
println("foO BAr".capitalizedString)
```

{{output}}

```txt

ALPHABETA
alphabeta
Foo Bar

```



## Tcl


```tcl
set string alphaBETA

# three built-in case conversion commands
string toupper $string  ;# ==> ALPHABETA
string tolower $string  ;# ==> alphabeta
string totitle $string  ;# ==> Alphabeta

# not built-in
proc swapcase {s} {
    foreach char [split $s ""] {
        if {$char eq [set CHAR [string toupper $char]]} {
            append new [string tolower $char]
        } else {
            append new $CHAR
        }
    }
    return $new
}
swapcase $string  ;# ==> ALPHAbeta

# better performance, but English alphabet only
proc swapcase_en {s} {
    string map {
        a A b B c C d D e E f F g G h H i I j J k K l L m M n N o O p P q Q r R s S t T u U v V w W x X y Y z Z
        A a B b C c D d E e F f G g H h I i J j K k L l M m N n O o P p Q q R r S s T t U u V v W w X x Y y Z z
    } $s
}

swapcase Père     ;# ==> pÈRE
swapcase_en Père  ;# ==> pèRE
```



## Toka

  needs ctype

  [ i 1 - ] is i
  [ string.getLength 0 [ dup i + c@ toupper over i + c! ] countedLoop ] is string.toUpper
  [ string.getLength 0 [ dup i + c@ tolower over i + c! ] countedLoop ] is string.toLower

  " alphaBETA" string.toUpper type cr
  " alphaBETA" string.toLower type cr


## TorqueScript

  $string = "alphaBETA";
  $upperCase = strUpr($string);
  $lowerCase = strLwr($string);


## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT,{}
string="alphaBETA"
lowercase =EXCHANGE(string," {&a} {-0-} ")
uppercase1=EXCHANGE(string," {&a} {-0+} ")
uppercase2=CAPS    (string)
PRINT lowercase
PRINT uppercase1
PRINT uppercase2

```

Output:

```txt

alphabeta
ALPHABETA
ALPHABETA

```



## UNIX Shell

{{works with|Bourne Shell}}

For System V tr:

```bash
echo alphaBETA | tr '[a-z]' '[A-Z]'     # => ALPHABETA
echo alphaBETA | tr '[A-Z]' '[a-z]'     # => alphabeta
```


For [[BSD]] tr, [[GNU]] tr, or any [[POSIX]] system:

```bash
echo alphaBETA | tr a-z A-Z             # => ALPHABETA
echo alphaBETA | tr A-Z a-z             # => alphabeta
```


System V has a different syntax, and requires square brackets around ranges. Portable scripts can use System V syntax; the other systems handle square brackets as literal characters, translating [ to [ and ] to ], which is harmless.


### Bash

{{works with|bash}}


```bash
s="alphaBETA"
echo ${s^^}     # => ALPHABETA
echo ${s,,}     # => alphabeta
echo ${s^}      # => AlphaBETA
```



### Z Shell

{{works with|zsh}}


```bash
s="alphaBETA"
echo ${s:u}     # => ALPHABETA
echo ${s:l}     # => alphabeta
```



## Ursa


```ursa
out (lower "alphaBETA") endl console
out (upper "alphaBETA") endl console
```



## Ursala

Case conversion functions aren't built in but can be defined using the
reification operator (-:) to construct a function from a list of pairs.

```Ursala
#import std

to_upper = * -:~& ~=`A-~p letters
to_lower = * -:~& ~=`A-~rlp letters

#show+

examples = <to_upper 'alphaBETA',to_lower 'alphaBETA'>
```

output:

```txt
ALPHABETA
alphabeta
```



## Vala


```vala

string s = "alphaBeta";
// stores ALPHABETA to string
string s_upper = s.up();
// stores alphabeta to string
string s_lower = s.down();

```



## VBA


```VB
Function StringCase()
Dim s As String
s = "alphaBETA"
Debug.Print UCase(s)
Debug.Print LCase(s)
Debug.Print WorksheetFunction.Proper(s)
End Function
```


Output:

```txt
ALPHABETA
alphabeta
Alphabeta
```



## VBScript


```vbscript
Dim MyWord
MyWord = UCase("alphaBETA")   ' Returns "ALPHABETA"
MyWord = LCase("alphaBETA")   ' Returns "alphabeta"
```



## Visual Basic

{{works with|Visual Basic|VB6 Standard}}

```vb
Sub Main()
Const TESTSTRING As String = "alphaBETA"
Debug.Print "initial   =                                      " _
   & TESTSTRING
Debug.Print "uppercase =                                      " _
   & UCase(TESTSTRING)
Debug.Print "lowercase =                                      " _
   & LCase(TESTSTRING)
Debug.Print "first letter capitalized =                       " _
   & StrConv(TESTSTRING, vbProperCase)
Debug.Print "length (in characters) =                         " _
   & CStr(Len(TESTSTRING))
Debug.Print "length (in bytes) =                              " _
   & CStr(LenB(TESTSTRING))
Debug.Print "reversed =                                       " _
   & StrReverse(TESTSTRING)
Debug.Print "first position of letter A (case-sensitive) =    " _
   & InStr(1, TESTSTRING, "A", vbBinaryCompare)
Debug.Print "first position of letter A (case-insensitive) =  " _
   & InStr(1, TESTSTRING, "A", vbTextCompare)
Debug.Print "concatenated with '123' =                        " _
   & TESTSTRING & "123"
End Sub
```

{{out}}

```txt
initial   =                                      alphaBETA
uppercase =                                      ALPHABETA
lowercase =                                      alphabeta
first letter capitalized =                       Alphabeta
length (in characters) =                         9
length (in bytes) =                              18
reversed =                                       ATEBahpla
first position of letter A (case-sensitive) =    9
first position of letter A (case-insensitive) =  1
concatenated with '123' =                        alphaBETA123
```



## Vedit macro language


```vedit
#1 = CP
IT("alphaBETA")
Case_Upper_Block(#1, CP)
Case_Lower_Block(#1, CP)
```



## XPL0


```XPL0
string 0;                       \use zero-terminated string convention
include c:\cxpl\stdlib;         \ToUpper, ToLower, and 'code' declarations

proc StrToUpper(S);             \Convert string to uppercase characters
char S;
while S(0) do [S(0):= ToUpper(S(0));  S:=S+1];

proc StrToLower(S);             \Convert string to lowercase characters
char S;
while S(0) do [S(0):= ToLower(S(0));  S:=S+1];

char Str;
[Str:= "alphaBETA";
StrToUpper(Str);
Text(0, Str);  CrLf(0);
StrToLower(Str);
Text(0, Str);  CrLf(0);
]
```


Output:

```txt

ALPHABETA
alphabeta

```



## zkl


```zkl
s:="alphaBETA";
s.toLower(); //--> "alphabeta"
s.toUpper(); //--> "ALPHABETA"
```


{{omit from|Openscad}}
{{omit from|PARI/GP|No real capacity for string manipulation}}

[[Category: String manipulation]]
[[Category: Encodings]]

[[Wikipedia::https://en.wikipedia.org/wiki/Letter_case]]
