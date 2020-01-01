+++
title = "Palindrome detection"
description = ""
date = 2019-10-17T23:51:11Z
aliases = []
[extra]
id = 3209
[taxonomies]
categories = []
tags = []
+++

{{task|Text processing}}
[[Category:Recursion]]
[[Category:String manipulation]]
[[Category:Classic CS problems and programs]]
[[Category:Palindromes]]

A [[wp:Palindrome|palindrome]] is a phrase which reads the same backward and forward.

{{task heading}}

Write a function or program that checks whether a given sequence of characters (or, if you prefer, bytes)
is a palindrome.

'''''For extra credit:'''''
* Support Unicode characters.
* Write a second function (possibly as a wrapper to the first) which detects ''inexact'' palindromes, i.e. phrases that are palindromes if white-space and punctuation is ignored and case-insensitive comparison is used.

{{task heading|Hints}}
* It might be useful for this task to know how to [[Reversing a string|reverse a string]].
* This task's entries might also form the subjects of the task [[Test a function]].

{{task heading|Related tasks}}

{{Related tasks/Word plays}}

<hr>


## 360 Assembly


```360asm
*        Reverse b string          25/06/2018
PALINDRO CSECT
         USING  PALINDRO,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         "
         ST     R15,8(R13)         "
         LR     R13,R15            "
         LA     R8,BB              @b[1]
         LA     R9,AA+L'AA-1       @a[n-1]
         LA     R6,1               i=1
LOOPI    C      R6,=A(L'AA)        do i=1 to length(a)
         BH     ELOOPI             leave i
         MVC    0(1,R8),0(R9)        substr(b,i,1)=substr(a,n-i+1,1)
         LA     R8,1(R8)             @b=@b+1
         BCTR   R9,0                 @a=@a-1
         LA     R6,1(R6)             i=i+1
         B      LOOPI              end do
ELOOPI   XPRNT  AA,L'AA            print a
         CLC    BB,AA              if b=a
         BNE    SKIP
         XPRNT  MSG,L'MSG          then print msg
SKIP     L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    "
         XR     R15,R15            "
         BR     R14                exit
AA       DC     CL32'INGIRUMIMUSNOCTEETCONSUMIMURIGNI'  a
BB       DS     CL(L'AA)           b
MSG      DC     CL23'IT IS A TRUE PALINDROME'
         YREGS
         END    PALINDRO
```

{{out}}

```txt

INGIRUMIMUSNOCTEETCONSUMIMURIGNI
IT IS A TRUE PALINDROME

```



## ACL2


```Lisp
(defun reverse-split-at-r (xs i ys)
  (if (zp i)
      (mv xs ys)
      (reverse-split-at-r (rest xs) (1- i)
                          (cons (first xs) ys))))

(defun reverse-split-at (xs i)
  (reverse-split-at-r xs i nil))

(defun is-palindrome (str)
  (let* ((lngth (length str))
         (idx (floor lngth 2)))
    (mv-let (xs ys)
            (reverse-split-at (coerce str 'list) idx)
            (if (= (mod lngth 2) 1)
                (equal (rest xs) ys)
                (equal xs ys)))))
```



## ActionScript

The following function handles non-ASCII characters properly, since charAt() returns a single Unicode character.

```ActionScript
function isPalindrome(str:String):Boolean
{
	for(var first:uint = 0, second:uint = str.length - 1; first < second; first++, second--)
		if(str.charAt(first) != str.charAt(second)) return false;
	return true;
}
```


## Ada


```ada
function Palindrome (Text : String) return Boolean is
begin
   for Offset in 0..Text'Length / 2 - 1 loop
      if Text (Text'First + Offset) /= Text (Text'Last - Offset) then
         return False;
      end if;
   end loop;
   return True;
end Palindrome;
```


----
Ada 2012 version:

```ada

function Palindrome (Text : String) return Boolean is
(for all i in Text'Range => Text(i)= Text(Text'Last-i+Text'First));

```



## ALGOL 68

{{trans|C}}

{{works with|ALGOL 68|Standard - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386 - except for the '''FORMAT''' and ''printf'' in test}}

```algol68
# Iterative #
PROC palindrome = (STRING s)BOOL:(
   FOR i TO UPB s OVER 2 DO
     IF s[i] /= s[UPB s-i+1] THEN GO TO return false FI
   OD;Power
   else: TRUE EXIT
   return false: FALSE
);

# Recursive #
PROC palindrome r = (STRING s)BOOL:
   IF LWB s >= UPB s THEN TRUE
   ELIF s[LWB s] /= s[UPB s] THEN FALSE
   ELSE palindrome r(s[LWB s+1:UPB s-1])
   FI
;

# Test #
main:
(
   STRING t = "ingirumimusnocteetconsumimurigni";
   FORMAT template = $"sequence """g""" "b("is","isnt")" a palindrome"l$;

   printf((template, t, palindrome(t)));
   printf((template, t, palindrome r(t)))
)
```

{{out}}

```txt

sequence "ingirumimusnocteetconsumimurigni" is a palindrome
sequence "ingirumimusnocteetconsumimurigni" is a palindrome

```


## APL

NARS2000 APL, dynamic function "if the argument matches the reverse of the argument", with Unicode character support:

```APL
      {⍵≡⌽⍵} 'abc'
0
      {⍵≡⌽⍵} '⍋racecar⍋'
1
```

Or in tacit function form, a combination of three functions, right tack (echo), reverse, then the result of each compared with the middle one, match (equals):

```APL
      (⊢≡⌽) 'abc'
0
      (⊢≡⌽) 'nun'
1
```

An inexact version is harder, because uppercase and lowercase with Unicode awareness depends on APL interpreter; NARS2000 has no support for it. Classic case conversion means lookup up the letters in an alphabet of UppercaseLowercase, then mapping those positions into an UppercaseUppercase or LowercaseLowercase array. Remove non-A-Za-z first to get rid of punctuation, and get an inexact dynamic function with just English letter support:

```APL
inexact←{Aa←(⎕A,⎕a) ⋄ (⊢≡⌽)(⎕a,⎕a)[Aa⍳⍵/⍨⍵∊Aa]}
      inexact 'abc,-cbA2z'
0
      inexact 'abc,-cbA2'
1
```

Dyalog APL has a Unicode-aware uppercase/lowercase function (819 I-beam), AFAIK no support for looking up Unicode character classes.

## AppleScript


Using post-Yosemite AppleScript (to pull in lowercaseStringWithLocale from Foundation classes)

```AppleScript
use framework "Foundation"

-- CASE-INSENSITIVE PALINDROME, IGNORING SPACES ? ----------------------------

-- isPalindrome :: String -> Bool
on isPalindrome(s)
    s = intercalate("", reverse of characters of s)
end isPalindrome

-- toSpaceFreeLower :: String -> String
on spaceFreeToLower(s)
    script notSpace
        on |λ|(s)
            s is not space
        end |λ|
    end script

    intercalate("", filter(notSpace, characters of toLower(s)))
end spaceFreeToLower


-- TEST ----------------------------------------------------------------------
on run

    isPalindrome(spaceFreeToLower("In girum imus nocte et consumimur igni"))

    --> true

end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

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

-- toLower :: String -> String
on toLower(str)
    set ca to current application
    ((ca's NSString's stringWithString:(str))'s ¬
        lowercaseStringWithLocale:(ca's NSLocale's currentLocale())) as text
end toLower
```

{{Out}}

```txt
true
```



## Applesoft BASIC


```ApplesoftBasic
100 DATA"MY DOG HAS FLEAS"
110 DATA"MADAM, I'M ADAM."
120 DATA"1 ON 1"
130 DATA"IN GIRUM IMUS NOCTE ET CONSUMIMUR IGNI"
140 DATA"A man, a plan, a canal: Panama!"
150 DATA"KAYAK"
160 DATA"REDDER"
170 DATA"H"
180 DATA""

200 FOR L1 = 1 TO 9
210    READ W$ : GOSUB 300" IS PALINDROME?
220    PRINT CHR$(34); W$; CHR$(34); " IS ";
230    IF NOT PALINDROME THEN PRINT "NOT ";
240    PRINT "A PALINDROME"
250 NEXT
260 END

300 REMIS PALINDROME?
310 PA = 1
320 L = LEN(W$)
330 IF L = 0 THEN RETURN
340 FOR L0 = 1 TO L / 2 + .5
350     PA = MID$(W$, L0, 1) = MID$(W$, L - L0 + 1, 1)
360     IF PALINDROME THEN NEXT L0
370 RETURN
```



## AutoHotkey

Reversing the string:

```AutoHotkey
IsPalindrome(Str){
	Loop, Parse, Str
		ReversedStr := A_LoopField . ReversedStr
	return, (ReversedStr == Str)?"Exact":(RegExReplace(ReversedStr,"\W")=RegExReplace(Str,"\W"))?"Inexact":"False"
}
```



## AutoIt



```AutoIt
;== AutoIt Version: 3.3.8.1

Global $aString[7] = [ _
"In girum imus nocte, et consumimur igni", _  ; inexact palindrome
"Madam, I'm Adam.", _                         ; inexact palindrome
"salàlas", _                                  ; exact palindrome
"radar", _                                    ; exact palindrome
"Lagerregal", _                               ; exact palindrome
"Ein Neger mit Gazelle zagt im Regen nie.", _ ; inexact palindrome
"something wrong"]                            ; no palindrome
Global $sSpace42 = "                                          "

For $i = 0 To 6
	If _IsPalindrome($aString[$i]) Then
		ConsoleWrite('"' & $aString[$i] & '"' & StringLeft($sSpace42, 42-StringLen($aString[$i])) & 'is an exact palindrome.' & @LF)
	Else
		If _IsPalindrome( StringRegExpReplace($aString[$i], '\W', '') ) Then
			ConsoleWrite('"' & $aString[$i] & '"' & StringLeft($sSpace42, 42-StringLen($aString[$i])) & 'is an  inexact palindrome.' & @LF)
		Else
			ConsoleWrite('"' & $aString[$i] & '"' & StringLeft($sSpace42, 42-StringLen($aString[$i])) & 'is not a palindrome.' & @LF)
		EndIf
	EndIf
Next

Func _IsPalindrome($_string)
	Local $iLen = StringLen($_string)
	For $i = 1  To Int($iLen/2)
		If StringMid($_string, $i, 1) <> StringMid($_string, $iLen-($i-1), 1) Then Return False
	Next
	Return True
EndFunc

```

{{out}}

```Text

"In girum imus nocte, et consumimur igni"   is an inexact palindrome.
"Madam, I'm Adam."                          is an inexact palindrome.
"salàlas"                                   is an exact palindrome.
"radar"                                     is an exact palindrome.
"Lagerregal"                                is an exact palindrome.
"Ein Neger mit Gazelle zagt im Regen nie."  is an inexact palindrome.
"something wrong"                           is not a palindrome.

```


--[[User:BugFix|BugFix]] ([[User talk:BugFix|talk]]) 14:26, 13 November 2013 (UTC)


## AWK

'''Non-recursive'''

See [[Reversing a string]].


```awk
function is_palindro(s)
{
  if ( s == reverse(s) ) return 1
  return 0
}
```


'''Recursive'''


```awk
function is_palindro_r(s)
{
  if ( length(s) < 2 ) return 1
  if ( substr(s, 1, 1) != substr(s, length(s), 1) ) return 0
  return is_palindro_r(substr(s, 2, length(s)-2))
}
```


'''Testing'''

```awk
BEGIN {
  pal = "ingirumimusnocteetconsumimurigni"
  print is_palindro(pal)
  print is_palindro_r(pal)
}
```



## BaCon


```freebasic

OPTION COMPARE TRUE

INPUT "Enter your line... ", word$

IF word$ = REVERSE$(word$) THEN
    PRINT "This is an exact palindrome!"
ELIF EXTRACT$(word$, "[[:punct:]]|[[:blank:]]", TRUE) = REVERSE$(EXTRACT$(word$, "[[:punct:]]|[[:blank:]]", TRUE)) THEN
    PRINT "This is an inexact palindrome!"
ELSE
    PRINT "Not a palindrome."
ENDIF

```

{{out}}

```txt

Enter your line... In girum imus nocte, et consumimur igni
This is an inexact palindrome!
Enter your line... Madam, I'm Adam.
This is an inexact palindrome!
Enter your line... radar
This is an exact palindrome!
Enter your line... Something else
Not a palindrome.

```



## BASIC

{{works with|QBasic}}


```qbasic
' OPTION _EXPLICIT ' For QB64. In VB-DOS remove the underscore.

DIM txt$

' Palindrome
CLS
PRINT "This is a palindrome detector program."
PRINT
INPUT "Please, type a word or phrase: ", txt$

IF IsPalindrome(txt$) THEN
  PRINT "Is a palindrome."
ELSE
  PRINT "Is Not a palindrome."
END IF

END


FUNCTION IsPalindrome (AText$)
  ' Var
  DIM CleanTXT$, RvrsTXT$

  CleanTXT$ = CleanText$(AText$)
  RvrsTXT$ = RvrsText$(CleanTXT$)

  IsPalindrome = (CleanTXT$ = RvrsTXT$)

END FUNCTION

FUNCTION CleanText$ (WhichText$)
  ' Var
  DIM i%, j%, c$, NewText$, CpyTxt$, AddIt%, SubsTXT$
  CONST False = 0, True = NOT False

  SubsTXT$ = "AIOUE"
  CpyTxt$ = UCASE$(WhichText$)
  j% = LEN(CpyTxt$)

  FOR i% = 1 TO j%
    c$ = MID$(CpyTxt$, i%, 1)

    ' See if it is a letter. Includes Spanish letters.
    SELECT CASE c$
      CASE "A" TO "Z"
        AddIt% = True
      CASE " ", "¡", "¢", "£"
        c$ = MID$(SubsTXT$, ASC(c$) - 159, 1)
        AddIt% = True
      CASE "‚"
        c$ = "E"
        AddIt% = True
      CASE "¤"
        c$ = "¥"
        AddIt% = True
      CASE ELSE
        AddIt% = False
    END SELECT

    IF AddIt% THEN
      NewText$ = NewText$ + c$
    END IF
  NEXT i%

  CleanText$ = NewText$

END FUNCTION

FUNCTION RvrsText$ (WhichText$)
  ' Var
  DIM i%, c$, NewText$, j%

  j% = LEN(WhichText$)
  FOR i% = 1 TO j%
    NewText$ = MID$(WhichText$, i%, 1) + NewText$
  NEXT i%

  RvrsText$ = NewText$

END FUNCTION
```


{{out}}
This is a palindrome detector program.

Please, type a word or phrase: Madam, I'm Adam.
Is a palindrome.

This is a palindrome detector program.

Please, type a word or phrase: This is just a test.
Is not a palindrome.

==={{header|IS-BASIC}}===
<lang IS-BASIC>
100 PROGRAM "Palindr.bas"
110 LINE INPUT PROMPT "Text: ":TX$
120 PRINT """";TX$;""" is ";
130 IF PALIND(TX$) THEN
140   PRINT "a palindrome."
150 ELSE
160   PRINT "not a palindrome."
170 END IF
180 DEF TRIM$(TX$)
190   LET T$=""
200   FOR I=1 TO LEN(TX$)
210     IF TX$(I)>="A" AND TX$(I)<="Z" THEN LET T$=T$&TX$(I)
220   NEXT
230   LET TRIM$=T$
240 END DEF
250 DEF PALIND(TX$)
260   LET PALIND=-1:LET TX$=TRIM$(UCASE$(TX$))
270   FOR I=1 TO LEN(TX$)/2
280     IF TX$(I)<>TX$(LEN(TX$)-I+1) THEN LET PALIND=0:EXIT FOR
290   NEXT
300 END DEF
```


=
## Sinclair ZX81 BASIC
=

### =Exact palindrome=

The specification suggests, but does not insist, that we reverse the input string and then test for equality; this algorithm is more efficient.

```basic
 10 INPUT S$
 20 FOR I=1 TO LEN S$/2
 30 IF S$(I)<>S$(LEN S$-I+1) THEN GOTO 60
 40 NEXT I
 50 GOTO 70
 60 PRINT "NOT A ";
 70 PRINT "PALINDROME"
```


### =Inexact palindrome=

Add the following lines to convert the program into an inexact-palindrome checker (i.e. one that ignores non-alphabetic characters). The resulting program still works with only 1k of RAM. The ZX81 only supports its own character set, which does not include lower case, so that case-insensitive comparison and <i>a fortiori</i> Unicode are not possible.

```basic
 15 GOSUB 90
 80 STOP
 90 LET T$=""
100 FOR I=1 TO LEN S$
110 IF S$(I)>="A" AND S$(I)<="Z" THEN LET T$=T$+S$(I)
120 NEXT I
130 LET S$=T$
140 RETURN
```


=
## BBC BASIC
=

```bbcbasic
      test$ = "A man, a plan, a canal: Panama!"
      PRINT """" test$ """" ;
      IF FNpalindrome(FNletters(test$)) THEN
        PRINT " is a palindrome"
      ELSE
        PRINT " is not a palindrome"
      ENDIF
      END

      DEF FNpalindrome(A$) = (A$ = FNreverse(A$))

      DEF FNreverse(A$)
      LOCAL B$, P%
      FOR P% = LEN(A$) TO 1 STEP -1
        B$ += MID$(A$,P%,1)
      NEXT
      = B$

      DEF FNletters(A$)
      LOCAL B$, C%, P%
      FOR P% = 1 TO LEN(A$)
        C% = ASC(MID$(A$,P%))
        IF C% > 64 AND C% < 91 OR C% > 96 AND C% < 123 THEN
          B$ += CHR$(C% AND &5F)
        ENDIF
      NEXT
      = B$
```

{{out}}

```txt
"A man, a plan, a canal: Panama!" is a palindrome
```



## Bash


```bash

#! /bin/bash
# very simple way to detect a palindrome in Bash
# output of bash --version -> GNU bash, version 4.4.7(1)-release x86_64 ...

echo "enter a string"
read input

size=${#input}
count=0

while (($count < $size))
do
    array[$count]=${input:$count:1}
    (( count+=1 ))
done

count=0

for ((i=0 ; i < $size; i+=1))
do
    if [ "${array[$i]}" == "${array[$size - $i - 1]}" ]
    then
        (( count += 1 ))
    fi
done

if (( $count == $size ))
then
    echo "$input is a palindrome"
fi

```



## Batch File



```dos
@echo off
setlocal enabledelayedexpansion
set /p string=Your string :
set count=0
:loop
	if "!%string%:~%count%,1!" neq "" (
		set reverse=!%string%:~%count%,1!!reverse!
		set /a count+=1
		goto loop
	)
set palindrome=isn't
if "%string%"=="%reverse%" set palindrome=is
echo %string% %palindrome% a palindrome.
pause
exit
```


Or, recursive (and without setlocal enabledelayedexpansion) (compatible with ReactOS cmd.exe)

```dos
@echo off
set /p testString=Your string (all same case please) :
call :isPalindrome result %testString: =%
if %result%==1 echo %testString% is a palindrome
if %result%==0 echo %testString% isn't a palindrome
pause
goto :eof

:isPalindrome
	set %1=0
	set string=%2
	if "%string:~2,1%"=="" (
		set %1=1
		goto :eof
	)
	if "%string:~0,1%"=="%string:~-1%" (
		call :isPalindrome %1 %string:~1,-1%
	)
	goto :eof
```



## Befunge


{{works with|CCBI|2.1}}

The following code reads a line from stdin and prints "True" if it is a palindrome, or False" otherwise.

```befunge
v_$0:8p>:#v_:18p08g1-08p >:08g`!v
~->p5p ^  0v1p80-1g80vj!-g5g80g5_0'ev
:a^80+1:g8<>8g1+:18pv>0"eslaF">:#,_@
[[relet]]-2010------>003-x   -^"Tru"<
```


{{works with|Befunge|93}}

To check a string, replace "dennis sinned" with your own string.

Note that this has some limits.:
* There '''must''' be a quotation mark immediately after the string, and then nothing but spaces for the rest of that line.
** The '''v''' at the end of that same line '''must''' remain immediately above the '''2'''. (''Very'' important.) The closing quotation mark can be against the '''v''', but can't replace it.
* The potential palindrome can be no longer than 76 characters (which beats the previous version's 11), and ''everything'' (spaces, punctuation, capitalization, etc.) is considered part of the palindrome. (Best to just use lower case letters and ''nothing else''.)


```befunge>v
    "emordnilap a toN",,,,,,,,,,,,,,,,@,,,,,,,,,,,,,,,"Is a palindrome"     <
2^ < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < <
4    ^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v
8  ^_v # ^_v # ^_v # ^_v # ^_v # ^_v # ^_v # ^_v # ^_v # ^_v # ^_v # ^_v # ^_v
*^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v
+ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
>"dennis sinned"                                                               v
 "                                                                             2
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" 0
> ^- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 9
   _^   v_^   v_^   v_^   v_^   v_^   v_^   v_^   v_^   v_^   v_^   v_^   v_^  p
    v_^ # v_^ # v_^ # v_^ # v_^ # v_^ # v_^ # v_^ # v_^ # v_^ # v_^ # v_^ # v_^
      v_^   v_^   v_^   v_^   v_^   v_^   v_^   v_^   v_^   v_^   v_^   v_^
 ^< < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < <
 >09g8p09g1+09pv
 |:            <                                                               <
^<
```



## Bracmat


```bracmat
( ( palindrome
  =   a
    .     @(!arg:(%?a&utf$!a) ?arg !a)
        & palindrome$!arg
      | utf$!arg
  )
& ( desep
  =   x
    .     @(!arg:?x (" "|"-"|",") ?arg)
        & !x desep$!arg
      | !arg
  )
&     "In girum imus nocte et consumimur igni"
      "Я иду с мечем, судия"
      "The quick brown fox"
      "tregða, gón, reiði - er nóg að gert"
      "人人為我,我為人人"
      "가련하시다 사장집 아들딸들아 집장사 다시 하련가"
  : ?candidates
&   whl
  ' ( !candidates:%?candidate ?candidates
    &   out
      $ ( !candidate
          is
          (   palindrome$(low$(str$(desep$!candidate)))
            & indeed
          | not
          )
          a
          palindrome
        )
    )
&
);
```

Output:

```txt
In girum imus nocte et consumimur igni is indeed a palindrome
Я иду с мечем, судия is indeed a palindrome
The quick brown fox is not a palindrome
tregða, gón, reiði - er nóg að gert is indeed a palindrome
人人為我,我為人人 is indeed a palindrome
  가련하시다 사장집 아들딸들아 집장사 다시 하련가
  is
  indeed
  a
  palindrome

```



## Burlesque



```burlesque

zz{ri}f[^^<-==

```



## C


'''Non-recursive'''

This function compares the first char with the last, the second with the one previous the last, and
so on. The first different pair it finds, return 0 (false); if all the pairs were equal, then return 1 (true).
You only need to go up to (the length) / 2 because the second half just re-checks the same stuff as the first half;
and if the length is odd, the middle doesn't need to be checked (so it's okay to do integer division by 2, which rounds down).


```c
#include <string.h>

int palindrome(const char *s)
{
   int i,l;
   l = strlen(s);
   for(i=0; i<l/2; i++)
   {
     if ( s[i] != s[l-i-1] ) return 0;
   }
   return 1;
}
```

More idiomatic version:

```c
int palindrome(const char *s)
{
   const char *t; /* t is a pointer that traverses backwards from the end */
   for (t = s; *t != '\0'; t++) ; t--; /* set t to point to last character */
   while (s < t)
   {
     if ( *s++ != *t-- ) return 0;
   }
   return 1;
}
```


'''Recursive'''

A single char is surely a palindrome; a string is a palindrome if first and last char are the same and the remaining string (the string starting from the second char and ending to the char preceding the last one) is
itself a palindrome.


```c
int palindrome_r(const char *s, int b, int e)
{
   if ( (e - 1) <= b ) return 1;
   if ( s[b] != s[e-1] ) return 0;
   return palindrome_r(s, b+1, e-1);
}
```


'''Testing'''


```c
#include <stdio.h>
#include <string.h>
/* testing */
int main()
{
   const char *t = "ingirumimusnocteetconsumimurigni";
   const char *template = "sequence \"%s\" is%s palindrome\n";
   int l = strlen(t);

   printf(template,
          t, palindrome(t) ? "" : "n't");
   printf(template,
          t, palindrome_r(t, 0, l) ? "" : "n't");
   return 0;
}
```



## C++

The C solutions also work in C++, but C++ allows a simpler one:

```cpp
#include <string>
#include <algorithm>

bool is_palindrome(std::string const& s)
{
  return std::equal(s.begin(), s.end(), s.rbegin());
}
```


Or, checking half is sufficient (on odd-length strings, this will ignore the middle element):

```cpp
#include <string>
#include <algorithm>

bool is_palindrome(std::string const& s)
{
  return std::equal(s.begin(), s.begin()+s.length()/2, s.rbegin());
}
```


## C#

'''Non-recursive'''


```c#
using System;

class Program
{
    static string Reverse(string value)
    {
        char[] chars = value.ToCharArray();
        Array.Reverse(chars);
        return new string(chars);
    }

    static bool IsPalindrome(string value)
    {
        return value == Reverse(value);
    }

    static void Main(string[] args)
    {
        Console.WriteLine(IsPalindrome("ingirumimusnocteetconsumimurigni"));
    }
}
```


'''Using LINQ operators'''

```c#
using System;
using System.Linq;

class Program
{
	static bool IsPalindrome(string text)
	{
		return text == new String(text.Reverse().ToArray());
	}

	static void Main(string[] args)
	{
		Console.WriteLine(IsPalindrome("ingirumimusnocteetconsumimurigni"));
	}
}

```


'''No string reversal'''

Reversing a string is very slow. A much faster way is to simply compare characters.

```c#
using System;

static class Program
{
    //As an extension method (must be declared in a static class)
    static bool IsPalindrome(this string sentence)
    {
        for (int l = 0, r = sentence.Length - 1; l < r; l++, r--)
            if (sentence[l] != sentence[r]) return false;
        return true;
    }

    static void Main(string[] args)
    {
        Console.WriteLine("ingirumimusnocteetconsumimurigni".IsPalindrome());
    }
}
```



## Clojure


```clojure
(defn palindrome? [s]
  (= s (clojure.string/reverse s)))
```


'''lower-level, but somewhat faster'''

```clojure
(defn palindrome? [^String s]
  (loop [front 0 back (dec (.length s))]
    (or (>= front back)
        (and (= (.charAt s front) (.charAt s back))
             (recur (inc front) (dec back)))))
```


'''Test'''

```txt

user=> (palindrome? "amanaplanacanalpanama")
true
user=> (palindrome? "Test 1, 2, 3")
false

```



## COBOL

{{works with|GnuCOBOL}}

```COBOL
       identification division.
       function-id. palindromic-test.

       data division.
       linkage section.
       01 test-text            pic x any length.
       01 result               pic x.
          88 palindromic       value high-value
                               when set to false low-value.

       procedure division using test-text returning result.

       set palindromic to false
       if test-text equal function reverse(test-text) then
           set palindromic to true
       end-if

       goback.
       end function palindromic-test.

```



## CoffeeScript


```coffeescript

    String::isPalindrome = ->
        for i in [0...@length / 2] when @[i] isnt @[@length - (i + 1)]
            return no
        yes

    String::stripped = -> @toLowerCase().replace /\W/gi, ''

    console.log "'#{ str }' : #{ str.stripped().isPalindrome() }" for str in [
        'In girum imus nocte et consumimur igni'
        'A man, a plan, a canal: Panama!'
        'There is no spoon.'
    ]

```


{{out}}
    'In girum imus nocte et consumimur igni' : true
    'A man, a plan, a canal: Panama!' : true
    'There is no spoon.' : false


## Common Lisp


```lisp
(defun palindrome-p (s)
  (string= s (reverse s)))
```


### Alternate solution

I use [https://franz.com/downloads/clp/survey Allegro CL 10.1]


```lisp

;; Project : Palindrome detection

(defun palindrome(x)
          (if (string= x (reverse x))
          (format t "~d" ": palindrome" (format t x))
          (format t "~d" ": not palindrome" (format t x))))
(terpri)
(setq x "radar")
(palindrome x)
(terpri)
(setq x "books")
(palindrome x)
(terpri)

```

Output:

```txt

radar: palindrome
books: not palindrome

```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE BbtPalindrome;
IMPORT StdLog;

PROCEDURE ReverseStr(str: ARRAY OF CHAR): POINTER TO ARRAY OF CHAR;
VAR
	top,middle,i: INTEGER;
	c: CHAR;
	rStr: POINTER TO ARRAY OF CHAR;
BEGIN
	NEW(rStr,LEN(str$) + 1);
	top := LEN(str$) - 1; middle := (top - 1) DIV 2;
	FOR i := 0 TO middle DO
		rStr[i] := str[top - i];
		rStr[top - i] := str[i];
	END;
	IF ODD(LEN(str$)) THEN rStr[middle + 1] := str[middle + 1] END;
	RETURN rStr;
END ReverseStr;

PROCEDURE IsPalindrome(str: ARRAY OF CHAR): BOOLEAN;
BEGIN
	RETURN str = ReverseStr(str)$;
END IsPalindrome;

PROCEDURE Do*;
VAR
	x: CHAR;
BEGIN
	StdLog.String("'salalas' is palindrome?:> ");
	StdLog.Bool(IsPalindrome("salalas"));StdLog.Ln;
	StdLog.String("'madamimadam' is palindrome?:> ");
	StdLog.Bool(IsPalindrome("madamimadam"));StdLog.Ln;
	StdLog.String("'abcbda' is palindrome?:> ");
	StdLog.Bool(IsPalindrome("abcbda"));StdLog.Ln;
END Do;
END BbtPalindrome.

```

Execute: ^Q  BbtPalindrome.Do<br/>
{{out}}

```txt

'salalas' is palindrome?:>  $TRUE
'madamimadam' is palindrome?:>  $TRUE
'abcbda' is palindrome?:>  $FALSE

```



## Crystal


### Declarative


```Ruby

def palindrome(s)
  s == s.reverse
end

```



### Imperative


```Ruby

def palindrome_imperative(s) : Bool
  mid = s.size / 2
  last = s.size - 1
  (0...s.size).each do |i|
    if s[i] != s[last - i]
      return false
    end
  end

  true
end

```


Performance comparison

```Ruby

require "benchmark"
Benchmark.ips do |x|
  x.report("declarative") { palindrome("hannah") }
  x.report("imperative") { palindrome_imperative("hannah")}
end

```



```Bash

declarative  21.96M ( 45.54ns) (± 6.90%)  32 B/op   1.49× slower
 imperative   32.8M ( 30.49ns) (± 3.29%)   0 B/op        fastest

```



## Delphi


```Delphi
uses
  SysUtils, StrUtils;

function IsPalindrome(const aSrcString: string): Boolean;
begin
  Result := SameText(aSrcString, ReverseString(aSrcString));
end;
```



## D

===High-level 32-bit Unicode Version===

```d
import std.traits, std.algorithm;

bool isPalindrome1(C)(in C[] s) pure /*nothrow*/
if (isSomeChar!C) {
    auto s2 = s.dup;
    s2.reverse(); // works on Unicode too, not nothrow.
    return s == s2;
}

void main() {
    alias pali = isPalindrome1;
    assert(pali(""));
    assert(pali("z"));
    assert(pali("aha"));
    assert(pali("sees"));
    assert(!pali("oofoe"));
    assert(pali("deified"));
    assert(!pali("Deified"));
    assert(pali("amanaplanacanalpanama"));
    assert(pali("ingirumimusnocteetconsumimurigni"));
    assert(pali("salÃ las"));
}
```


===Mid-level 32-bit Unicode Version===

```d
import std.traits;

bool isPalindrome2(C)(in C[] s) pure if (isSomeChar!C) {
    dchar[] dstr;
    foreach (dchar c; s) // not nothrow
        dstr ~= c;

    for (int i; i < dstr.length / 2; i++)
        if (dstr[i] != dstr[$ - i - 1])
            return false;
    return true;
}

void main() {
    alias isPalindrome2 pali;
    assert(pali(""));
    assert(pali("z"));
    assert(pali("aha"));
    assert(pali("sees"));
    assert(!pali("oofoe"));
    assert(pali("deified"));
    assert(!pali("Deified"));
    assert(pali("amanaplanacanalpanama"));
    assert(pali("ingirumimusnocteetconsumimurigni"));
    assert(pali("salÃ las"));
}
```


===Low-level 32-bit Unicode Version===

```d
import std.stdio, core.exception, std.traits;

// assume alloca() to be pure for this program
extern(C) pure nothrow void* alloca(in size_t size);

bool isPalindrome3(C)(in C[] s) pure if (isSomeChar!C) {
    auto p = cast(dchar*)alloca(s.length * 4);
    if (p == null)
        // no fallback heap allocation used
        throw new OutOfMemoryError();
    dchar[] dstr = p[0 .. s.length];

    // use std.utf.stride for an even lower level version
    int i = 0;
    foreach (dchar c; s) { // not nothrow
        dstr[i] = c;
        i++;
    }
    dstr = dstr[0 .. i];

    foreach (j; 0 .. dstr.length / 2)
        if (dstr[j] != dstr[$ - j - 1])
            return false;
    return true;
}

void main() {
    alias isPalindrome3 pali;
    assert(pali(""));
    assert(pali("z"));
    assert(pali("aha"));
    assert(pali("sees"));
    assert(!pali("oofoe"));
    assert(pali("deified"));
    assert(!pali("Deified"));
    assert(pali("amanaplanacanalpanama"));
    assert(pali("ingirumimusnocteetconsumimurigni"));
    assert(pali("salÃ las"));
}
```


===Low-level ASCII Version===

```d
bool isPalindrome4(in string str) pure nothrow {
    if (str.length == 0) return true;
    immutable(char)* s = str.ptr;
    immutable(char)* t = &(str[$ - 1]);
    while (s < t)
        if (*s++ != *t--) // ugly
            return false;
    return true;
}

void main() {
    alias isPalindrome4 pali;
    assert(pali(""));
    assert(pali("z"));
    assert(pali("aha"));
    assert(pali("sees"));
    assert(!pali("oofoe"));
    assert(pali("deified"));
    assert(!pali("Deified"));
    assert(pali("amanaplanacanalpanama"));
    assert(pali("ingirumimusnocteetconsumimurigni"));
    //assert(pali("salÃ las"));
}
```



## Dart



```dartlang

bool isPalindrome(String s){
  for(int i = 0; i < s.length/2;i++){
    if(s[i] != s[(s.length-1) -i])
      return false;
  }
  return true;
}

```


=={{header|Déjà Vu}}==


```dejavu
palindrome?:
	local :seq chars
	local :len-seq -- len seq

	for i range 0 / len-seq 2:
		if /= seq! i seq! - len-seq i:
			return false
	true

!. palindrome? "ingirumimusnocteetconsumimurigni"
!. palindrome? "nope"
```

{{out}}

```txt
true
false
```



## Dyalect



```dyalect
func isPalindrom(str) {
    str == str.reverse()
}

print(isPalindrom("ingirumimusnocteetconsumimurigni"))
```



## E


It is only necessarily to scan the first half of the string, <code>upper(0, upper.size() // 2)</code>, and compare each character to the corresponding character from the other end, <code>upper[last - i]</code>.

The for loop syntax is <code>for <var>key pattern</var> => <var>value pattern</var> in <var>collection</var> { ... }</code>, <code>?</code> imposes an additional boolean condition on a pattern (it may be read “''such that''”), and if the pattern does not match in a for loop then the iteration is skipped, so false is returned only if <code>upper[last - i] != c</code>.


```e
def isPalindrome(string :String) {
  def upper := string.toUpperCase()
  def last := upper.size() - 1
  for i => c ? (upper[last - i] != c) in upper(0, upper.size() // 2) {
    return false
  }
  return true
}
```



## EchoLisp


```lisp

;; returns #t or #f
(define (palindrome? string)
(equal? (string->list string) (reverse (string->list string))))

;; to strip spaces, use the following
;;(define (palindrome? string)
;;(let ((string (string-replace string "/\ /" "" "g")))
;;(equal? (string->list string) (reverse (string->list string)))))

```



## Eiffel


```Eiffel

	is_palindrome (a_string: STRING): BOOLEAN
			-- Is `a_string' a palindrome?
		require
			string_attached: a_string /= Void
		local
			l_index, l_count: INTEGER
		do
			from
				Result := True
				l_index := 1
				l_count := a_string.count
			until
				l_index >= l_count - l_index + 1 or not Result
			loop
				Result := (Result and a_string [l_index] = a_string [l_count - l_index + 1])
				l_index := l_index + 1
			end
		end

```



## Ela



```ela
open list string

isPalindrome xs = xs == reverse xs
isPalindrome <| toList "ingirumimusnocteetconsumimurigni"

```


Function <code>reverse</code> is taken from list module and is defined as:


```ela
reverse = foldl (flip (::)) (nil xs)

foldl f z (x::xs) = foldl f (f z x) xs
foldl _ z []      = z

```



## Elixir


```elixir

defmodule PalindromeDetection do
  def is_palindrome(str), do: str == String.reverse(str)
end

```

Note: Because of Elixir's strong Unicode support, this even supports graphemes:

```txt

iex(1)> PalindromeDetection.is_palindrome("salàlas")
true
iex(2)> PalindromeDetection.is_palindrome("as⃝df̅")
false
iex(3)> PalindromeDetection.is_palindrome("as⃝df̅f̅ds⃝a")
true

```


## Elm


```elm
import String exposing (reverse, length)
import Html exposing (Html, Attribute, text, div, input)
import Html.Attributes exposing (placeholder, value, style)
import Html.Events exposing (on, targetValue)
import Html.App exposing (beginnerProgram)

-- The following function (copied from Haskell) satisfies the
-- rosettacode task description.
is_palindrome x = x == reverse x

-- The remainder of the code demonstrates the use of the function
-- in a complete Elm program.
main = beginnerProgram { model = "" , view = view , update = update }

update newStr oldStr = newStr

view : String -> Html String
view candidate =
  div []
    ([ input
        [ placeholder "Enter a string to check."
        , value candidate
        , on "input" targetValue
        , myStyle
        ]
        []
     ] ++
     [ let testResult =
             is_palindrome candidate

           statement =
             if testResult then "PALINDROME!" else "not a palindrome"

       in div [ myStyle] [text statement]
     ])

myStyle : Attribute msg
myStyle =
  style
    [ ("width", "100%")
    , ("height", "20px")
    , ("padding", "5px 0 0 5px")
    , ("font-size", "1em")
    , ("text-align", "left")
    ]
```


Link to live demo: http://dc25.github.io/palindromeDetectionElm/


## Erlang


```Erlang

-module( palindrome ).

-export( [is_palindrome/1, task/0] ).

is_palindrome( String ) -> String =:= lists:reverse(String).

task() ->
	display( "abcba" ),
	display( "abcdef" ),
	Latin = "In girum imus nocte et consumimur igni",
	No_spaces_same_case = lists:append( string:tokens(string:to_lower(Latin), " ") ),
	display( Latin, No_spaces_same_case ).



display( String ) -> io:fwrite( "Is ~p a palindrom? ~p~n", [String, is_palindrome(String)] ).

display( String1, String2 ) -> io:fwrite( "Is ~p a palindrom? ~p~n", [String1, is_palindrome(String2)] ).

```

{{out}}

```txt

22> palindrome:task().
Is "abcba" a palindrom? true
Is "abcdef" a palindrom? false
Is "In girum imus nocte et consumimur igni" a Latin palindrom? true

```



## Euphoria


```euphoria
function isPalindrome(sequence s)
    for i = 1 to length(s)/2 do
        if s[i] != s[$-i+1] then
            return 0
        end if
    end for
    return 1
end function
```


=={{header|F Sharp|F#}}==

```fsharp
let isPalindrome (s: string) =
   let arr = s.ToCharArray()
   arr = Array.rev arr
```


Examples:

```fsharp
isPalindrome "abcba"
val it : bool = true
isPalindrome ("In girum imus nocte et consumimur igni".Replace(" ", "").ToLower());;
val it : bool = true
isPalindrome "abcdef"
val it : bool = false
```



## Factor


```factor
USING: kernel sequences ;
: palindrome? ( str -- ? ) dup reverse = ;
```




## Falcon

'''VBA/Python programmer's approach not sure if it's the most falconic way'''

```falcon

/* created by Aykayayciti Earl Lamont Montgomery
April 9th, 2018 */

function is_palindrome(a)
	a = strUpper(a).replace(" ", "")
	b = a[-1:0]
	return b == a
end

a = "mom"
> is_palindrome(a)

```

{{out}}

```txt

true
[Finished in 1.7s]

```


'''more falconic'''

```falcon

/* created by Aykayayciti Earl Lamont Montgomery
April 9th, 2018 */

b = "mom"
> strUpper(b).replace(" ", "") == strUpper(b[-1:0]) ? "Is a palindrome" : "Is not a palindrome"

```

{{out}}

```txt

Is a palindrome
[Finished in 1.5s]

```



## Fantom



```fantom

class Palindrome
{
  // Function to test if given string is a palindrome
  public static Bool isPalindrome (Str str)
  {
    str == str.reverse
  }

  // Give it a test run
  public static Void main ()
  {
    echo (isPalindrome(""))
    echo (isPalindrome("a"))
    echo (isPalindrome("aa"))
    echo (isPalindrome("aba"))
    echo (isPalindrome("abb"))
    echo (isPalindrome("salàlas"))
    echo (isPalindrome("In girum imus nocte et consumimur igni".lower.replace(" ","")))
  }
}

```



## FBSL



```qbasic
#APPTYPE CONSOLE

FUNCTION stripNonAlpha(BYVAL s AS STRING) AS STRING
	DIM sTemp AS STRING = ""
	DIM c AS STRING
	FOR DIM i = 1 TO LEN(s)
		c = MID(s, i, 1)
		IF INSTR("ABCDEFGHIJKLMNOPQRSTUVWXYZ", c, 0, 1) THEN
			sTemp = stemp & c
		END IF
	NEXT
	RETURN sTemp
END FUNCTION

FUNCTION IsPalindrome(BYVAL s AS STRING) AS INTEGER
	FOR DIM i = 1 TO STRLEN(s) \ 2 ' only check half of the string, as scanning from both ends
		IF s{i} <> s{STRLEN - (i - 1)} THEN RETURN FALSE 'comparison is not case sensitive
	NEXT

	RETURN TRUE
END FUNCTION

PRINT IsPalindrome(stripNonAlpha("A Toyota"))
PRINT IsPalindrome(stripNonAlpha("Madam, I'm Adam"))
PRINT IsPalindrome(stripNonAlpha("the rain in Spain falls mainly on the rooftops"))

PAUSE

```

{{out}}

```txt

 1
 1
 0

```



## Forth


```forth
: first   over c@ ;
: last    >r 2dup + 1- c@ r> swap ;
: palindrome? ( c-addr u -- f )
  begin
    dup 1 <=      if 2drop true  exit then
    first last <> if 2drop false exit then
    1 /string 1-
  again ;

```


FIRST and LAST are once-off words that could be beheaded immediately afterwards.
The version taking advantage of Tail Call Optimization or a properly tail-recursive variant of RECURSE (easily added to any Forth) is very similar.
The horizontal formatting highlights the parallel code - and potential factor;
a library of many string tests like this could have ?SUCCESS and ?FAIL .


'''Below is a separate Forth program that detects palindrome phrases as well as single word palindromes. It was programmed using gforth.'''


```forth

variable temp-addr

: valid-char? ( addr1 u -- f ) ( check for valid character )
    + dup C@ 48 58 within
    over C@ 65 91 within or
    swap C@ 97 123 within or ;

: >upper ( c1 -- c2 )
    dup 97 123 within if 32 - then ;

: strip-input ( addr1 u -- addr2 u ) ( Strip characters, then copy stripped string to temp-addr )
    pad temp-addr !
    temp-addr @ rot rot 0 do dup I 2dup valid-char? if
        + C@ >upper temp-addr @ C! 1 temp-addr +!
        else 2drop
        then loop drop temp-addr @ pad - ;

: get-phrase ( -- addr1 u )
    ." Type a phrase: " here 1024 accept here swap -trailing cr ;

: position-phrase ( addr1 u -- addr1 u addr2 u addr1 addr2 u )
    temp-addr @ over 2over 2over drop swap ;

: reverse-copy ( addr1 addr2 u -- addr1 addr2 )
    0 do over I' 1- I - + over I + 1 cmove loop 2drop ;

: palindrome? ( -- )
    get-phrase strip-input position-phrase reverse-copy compare 0= if
    ." << Valid >> Palindrome."
    else ." << Not >> a Palindrome."
    then cr ;

```


Example:

palindrome?

Type a phrase: A man, a plan, a cat, a ham, a yak, a yam, a hat, a canal-Panama!

<< Valid >> Palindrome.


## Fortran

{{works with|Fortran|90 and later}}

```fortran
program palindro

  implicit none

  character(len=*), parameter :: p = "ingirumimusnocteetconsumimurigni"

  print *, is_palindro_r(p)
  print *, is_palindro_r("anothertest")
  print *, is_palindro2(p)
  print *, is_palindro2("test")
  print *, is_palindro(p)
  print *, is_palindro("last test")

contains
```


'''Non-recursive'''


```fortran
! non-recursive
function is_palindro(t)
  logical :: is_palindro
  character(len=*), intent(in) :: t

  integer :: i, l

  l = len(t)
  is_palindro = .false.
  do i=1, l/2
     if ( t(i:i) /= t(l-i+1:l-i+1) ) return
  end do
  is_palindro = .true.
end function is_palindro

! non-recursive 2
function is_palindro2(t) result(isp)
  logical :: isp
  character(len=*), intent(in) :: t

  character(len=len(t)) :: s
  integer :: i

  forall(i=1:len(t)) s(len(t)-i+1:len(t)-i+1) = t(i:i)
  isp = ( s == t )
end function is_palindro2
```


'''Recursive'''

```fortran
  recursive function is_palindro_r (t) result (isp)

    implicit none
    character (*), intent (in) :: t
    logical :: isp

    isp = len (t) == 0 .or. t (: 1) == t (len (t) :) .and. is_palindro_r (t (2 : len (t) - 1))

  end function is_palindro_r
```



```fortran>end program palindro</lang



## FreeBASIC


```FreeBASIC
' version 20-06-2015
' compile with: fbc -s console "filename".bas

#Ifndef TRUE        ' define true and false for older freebasic versions
    #Define FALSE 0
    #Define TRUE Not FALSE
#EndIf

Function reverse(norm As String) As Integer

    Dim As String rev
    Dim As Integer i, l = Len(norm) -1

    rev = norm
    For i = 0 To l
        rev[l-i] = norm[i]
    Next

    If norm = rev Then
        Return TRUE
    Else
        Return FALSE
    End If

End Function

Function cleanup(in As String, action As String = "") As String
    ' action = "" do nothing, [l|L] = convert to lowercase,
    ' [s|S] = strip spaces,  [p|P] = strip punctuation.
    If action = "" Then Return in

    Dim As Integer i, p_, s_
    Dim As String ch

    action = LCase(action)
    For i = 1 To Len(action)
        ch = Mid(action, i, 1)
        If ch = "l" Then in = LCase(in)
        If ch = "p" Then
            p_ = 1
        ElseIf ch = "s" Then
            s_ = 1
        End If
    Next

    If p_ = 0 And s_ = 0 Then Return in

    Dim As String unwanted, clean

    If s_ = 1 Then unwanted = " "
    If p_ = 1 Then unwanted = unwanted + "`~!@#$%^&*()-=_+[]{}\|;:',.<>/?"

    For i = 1 To Len(in)
        ch = Mid(in, i, 1)
        If InStr(unwanted, ch) = 0 Then clean = clean + ch
    Next

    Return clean

End Function

' ------=< MAIN >=------

Dim As String test = "In girum imus nocte et consumimur igni"
'IIf ( cond, true, false ), true and false must be of the same type (num, string, UDT)
Print
Print "                 reverse(test) = "; IIf(reverse(test) = FALSE, "FALSE", "TRUE")
Print "  reverse(cleanup(test,""l"")) = "; IIf(reverse(cleanup(test,"l")) = FALSE, "FALSE", "TRUE")
Print " reverse(cleanup(test,""ls"")) = "; IIf(reverse(cleanup(test,"ls")) = FALSE, "FALSE", "TRUE")
Print "reverse(cleanup(test,""PLS"")) = "; IIf(reverse(cleanup(test,"PLS")) = FALSE, "FALSE", "TRUE")

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print : Print "Hit any key to end program"
Sleep
End
```

{{out}}

```txt
               reverse(test) = FALSE
  reverse(cleanup(test,"l")) = FALSE
 reverse(cleanup(test,"ls")) = TRUE
reverse(cleanup(test,"PLS")) = TRUE
```



## Frink

This version will even work with upper-plane Unicode characters.  Many languages will not work correctly with upper-plane Unicode characters because they are represented as Unicode "surrogate pairs" which are represented as two characters in a UTF-16 stream.  In addition, Frink uses a ''grapheme''-based reverse, which allows the algorithm below to operate on combined sequences of Unicode characters.

For example, the string "og\u0308o" represents an o, a g with combining diaeresis, followed by the letter o. Or, in other words, "og̈o". Note that while there are four Unicode codepoints, only three "graphemes" are displayed.  Using Frink's smart "reverse" function preserves these combined graphemes and detects them correctly as palindromes.


```frink
isPalindrome[x] := x == reverse[x]

```


Test in Frink with upper-plane Unicode:

```frink
isPalindrome["x\u{1f638}x"]
```


<code>
true
</code>


## GAP


```gap
ZapGremlins := function(s)
  local upper, lower, c, i, n, t;
  upper := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  lower := "abcdefghijklmnopqrstuvwxyz";
  t := [ ];
  i := 1;
  for c in s do
    n := Position(upper, c);
    if n <> fail then
      t[i] := lower[n];
      i := i + 1;
    else
      n := Position(lower, c);
      if n <> fail then
        t[i] := c;
        i := i + 1;
      fi;
    fi;
  od;
  return t;
end;

IsPalindrome := function(s)
  local t;
  t := ZapGremlins(s);
  return t = Reversed(t);
end;
```


## GML


```go

//Setting a var from an argument passed to the script
var str;
str = argument0
//Takes out all spaces/anything that is not a letter or a number and turns uppercase letters to lowercase
str = string_lettersdigits(string_lower(string_replace(str,' ','')));
var inv;
inv = '';
//for loop that reverses the sequence
var i;
for (i = 0; i < string_length(str); i += 1;)
    {
    inv += string_copy(str,string_length(str)-i,1);
    }
//returns true if the sequence is a palindrome else returns false
return (str == inv);

```


Palindrome detection using a [http://rosettacode.org/wiki/Loop/Downward_For#GML Downward For-Loop]


```go


//Remove everything except for letters and digits and convert the string to lowercase. source is what will be compared to str.
var str = string_lower(string_lettersdigits(string_replace(argument0," ",""))), source = "";

//Loop through and store each character of str in source.
for (var i = string_length(str); i > 0; i--) {
    source += string_char_at(str,i);
}

//Return if it is a palindrome.
return source == str;

```



## Go


```go
package pal

func IsPal(s string) bool {
    mid := len(s) / 2
    last := len(s) - 1
    for i := 0; i < mid; i++ {
        if s[i] != s[last-i] {
            return false
        }
    }
    return true
}
```


This version works with Unicode,


```go

func isPalindrome(s string) bool {
	runes := []rune(s)
	numRunes := len(runes) - 1
	for i := 0; i < len(runes)/2; i++ {
		if runes[i] != runes[numRunes-i] {
			return false
		}
	}
	return true
}
```


Or using more slicing,


```go

func isPalindrome(s string) bool {
	runes := []rune(s)
	for len(runes) > 1 {
		if runes[0] != runes[len(runes)-1] {
			return false
		}
		runes = runes[1 : len(runes)-1]
	}
	return true
}
```



## Groovy


### Trivial


Solution:

```groovy
def isPalindrome = { String s ->
    s == s?.reverse()
}
```


Test program:

```groovy
println isPalindrome("")
println isPalindrome("a")
println isPalindrome("abcdefgfedcba")
println isPalindrome("abcdeffedcba")
println isPalindrome("abcedfgfedcb")
```


{{out}}

```txt
true
true
true
true
false
```


This solution assumes nulls are palindromes.

===Non-recursive===

Solution:

```groovy
def isPalindrome = { String s ->
    def n = s.size()
    n < 2 || s[0..<n/2] == s[-1..(-n/2)]
}
```


Test program and output are the same.
This solution does not handle nulls.


### Recursive


Solution follows the [[#C|C palindrome_r]] recursive solution:

```groovy
def isPalindrome
isPalindrome = { String s ->
    def n = s.size()
    n < 2 || (s[0] == s[n-1] && isPalindrome(s[1..<(n-1)]))
}
```


Test program and output are the same.
This solution does not handle nulls.


## Haskell


'''Non-recursive'''

A string is a palindrome if reversing it we obtain the same string.


```haskell
is_palindrome x = x == reverse x
```


Or, applicative and point-free, with some pre-processing of data (shedding white space and upper case):


```haskell
import Data.Char (toLower)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome = (==) <*> reverse

-- Alternatively, comparing just the first half with the reversed latter half
isPal :: Eq a => [a] -> Bool
isPal s =
  let (q, r) = quotRem (length s) 2
  in take q s == reverse (drop (q + r) s)

sample :: String
sample = "In girum imus nocte et consumimur igni"

prepared :: String -> String
prepared = filter (' ' /=) . fmap toLower

main :: IO ()
main = print $ [isPalindrome, isPal] <*> [prepared sample]
```

{{Out}}

```txt
[True,True]
```



'''Recursive'''

See the C palindrome_r code for an explanation of the concept used in this solution.


```haskell
is_palindrome_r x | length x <= 1 = True
                  | head x == last x = is_palindrome_r . tail. init $ x
                  | otherwise = False
```



## HicEst

{{incorrect|HicEst|The stripping of spaces and case conversion should be outside the palindrome detection.}}


```hicest
   result = Palindrome( "In girum imus nocte et consumimur igni" ) ! returns 1
END

FUNCTION Palindrome(string)
   CHARACTER string, CopyOfString

   L = LEN(string)
   ALLOCATE(CopyOfString, L)
   CopyOfString = string
   EDIT(Text=CopyOfString, UpperCase=L)
   L = L - EDIT(Text=CopyOfString, End, Left=' ', Delete, DO=L) ! EDIT returns number of deleted spaces

   DO i = 1, L/2
     Palindrome = CopyOfString(i) == CopyOfString(L - i + 1)
     IF( Palindrome == 0 ) RETURN
   ENDDO
END
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main(arglist)
every writes(s := !arglist) do write( if palindrome(s) then " is " else " is not", " a palindrome.")
end
```


The following simple procedure uses the built-in reverse.  Reverse creates a transient string which will get garbage collected.

```Icon
procedure palindrome(s)  #: return s if s is a palindrome
return s == reverse(s)
end
```

{{libheader|Icon Programming Library}}

Note: The IPL procedure [http://www.cs.arizona.edu/icon/library/src/procs/strings.icn strings] contains a palindrome tester called '''ispal''' that uses reverse and is equivalent to the version of '''palindrome''' above.

This version uses positive and negative sub-scripting and works not only on strings but lists of strings, such as ["ab","ab"] or ["ab","x"] the first list would pass the test but the second wouldn't.

```Icon
procedure palindrome(x)  #: return x if s is x palindrome
local i
every if x[i := 1 to (*x+ 1)/2] ~== x[-i] then fail
return x
end
```



## Ioke


```ioke
Text isPalindrome? = method(self chars == self chars reverse)
```



## J

'''Non-recursive'''

Reverse and match method

```j
isPalin0=: -: |.
```

Example usage

```j
   isPalin0 'ABBA'
1
   isPalin0 -.&' ' tolower 'In girum imus nocte et consumimur igni'
1
```


'''Recursive'''

Tacit and explicit verbs:

```j
isPalin1=: 0:`($:@(}.@}:))@.({.={:)`1:@.(1>:#)

isPalin2=: monad define
 if. 1>:#y do. 1 return. end.
 if. ({.={:)y do. isPalin2 }.}:y else. 0 end.
)
```


Note that while these recursive verbs are bulkier and more complicated, they are also several thousand times more inefficient than isPalin0.


```j
   foo=: foo,|.foo=:2000$a.
   ts=:6!:2,7!:2  NB. time and space required to execute sentence
   ts 'isPalin0 foo'
2.73778e_5 5184
   ts 'isPalin1 foo'
0.0306667 6.0368e6
   ts 'isPalin2 foo'
0.104391 1.37965e7
   'isPalin1 foo' %&ts 'isPalin0 foo'
1599.09 1164.23
   'isPalin2 foo' %&ts 'isPalin0 foo'
3967.53 2627.04
```



## Java


'''Non-Recursive'''


```java
public static boolean pali(String testMe){
	StringBuilder sb = new StringBuilder(testMe);
	return testMe.equals(sb.reverse().toString());
}
```

'''Recursive  (this version does not work correctly with upper-plane Unicode)'''


```java
public static boolean rPali(String testMe){
	if(testMe.length()<=1){
		return true;
	}
	if(!(testMe.charAt(0)+"").equals(testMe.charAt(testMe.length()-1)+"")){
		return false;
	}
	return rPali(testMe.substring(1, testMe.length()-1));
}
```


'''Recursive using indexes (this version does not work correctly with upper-plane Unicode)'''


```java
public static boolean rPali(String testMe){
	int strLen = testMe.length();
	return rPaliHelp(testMe, strLen-1, strLen/2, 0);
}

public static boolean rPaliHelp(String testMe, int strLen, int testLen, int index){
	if(index > testLen){
		return true;
	}
	if(testMe.charAt(index) != testMe.charAt(strLen-index)){
		return false;
	}
	return rPaliHelp(testMe, strLen, testLen, index + 1);
}

```


'''Regular Expression'''
([http://stackoverflow.com/questions/3664881/how-does-this-java-regex-detect-palindromes source])

```java
public static boolean pali(String testMe){
	return testMe.matches("|(?:(.)(?<=(?=^.*?(\\1\\2?)$).*))+(?<=(?=^\\2$).*)");
}
```



## JavaScript


```javascript
function isPalindrome(str) {
  return str === str.split("").reverse().join("");
}

console.log(isPalindrome("ingirumimusnocteetconsumimurigni"));
```



ES6 implementation

```javascript>var isPal = str =
 str === str.split("").reverse().join("");
```


Or, ignoring spaces and variations in case:


```JavaScript
(() => {

    // isPalindrome :: String -> Bool
    const isPalindrome = s => {
        const cs = filter(c => ' ' !== c, s.toLocaleLowerCase());
        return cs.join('') === reverse(cs).join('');
    };


    // TEST -----------------------------------------------
    const main = () =>
        isPalindrome(
            'In girum imus nocte et consumimur igni'
        )

    // GENERIC FUNCTIONS ----------------------------------

    // filter :: (a -> Bool) -> [a] -> [a]
    const filter = (f, xs) => (
        'string' !== typeof xs ? (
            xs
        ) : xs.split('')
    ).filter(f);

    // reverse :: [a] -> [a]
    const reverse = xs =>
        'string' !== typeof xs ? (
            xs.slice(0).reverse()
        ) : xs.split('').reverse().join('');

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
true
```



## jq


```jq
def palindrome: explode as $in | ($in|reverse) == $in;
```

'''Example''':
 "salàlas" | palindrome
{{Out}}
 true


## Jsish


```javascript
/* Palindrome detection, in Jsish */
function isPalindrome(str:string, exact:boolean=true) {
  if (!exact) {
      str = str.toLowerCase();
      str = str.replace(/[ \t,;:!?.]/g, '');
  }
  return str === str.match(/./g).reverse().join('');
}

;isPalindrome('BUB');
;isPalindrome('CUB');
;isPalindrome('Bub');
;isPalindrome('Bub', false);
;isPalindrome('In girum imus nocte et consumimur igni', false);
;isPalindrome('A man, a plan, a canal; Panama!', false);
;isPalindrome('Never odd or even', false);

/*
=!EXPECTSTART!=
isPalindrome('BUB') ==> true
isPalindrome('CUB') ==> false
isPalindrome('Bub') ==> false
isPalindrome('Bub', false) ==> true
isPalindrome('In girum imus nocte et consumimur igni', false) ==> true
isPalindrome('A man, a plan, a canal; Panama!', false) ==> true
isPalindrome('Never odd or even', false) ==> true
=!EXPECTEND!=
*/
```


Most of that code is for testing, using echo mode lines (semicolon in column 1)

{{out}}

```txt
prompt$ jsish --U palindrome.jsi
isPalindrome('BUB') ==> true
isPalindrome('CUB') ==> false
isPalindrome('Bub') ==> false
isPalindrome('Bub', false) ==> true
isPalindrome('In girum imus nocte et consumimur igni', false) ==> true
isPalindrome('A man, a plan, a canal; Panama!', false) ==> true
isPalindrome('Never odd or even', false) ==> true

prompt$ jsish -u palindrome.jsi
[PASS] palindrome.jsi
```



## Julia


```julia
palindrome(s) = s == reverse(s)
```

<b> Non-Recursive </b>

```julia

function palindrome(s)
    len = length(s)
    for i = 1:(len/2)
        if(s[len-i+1]!=s[i])
            return false
        end
    end
    return true
end

```

<b> Recursive </b>

```julia

function palindrome(s)
    len = length(s)
    if(len==0 || len==1)
        return true
    end
    if(s[1] == s[len])
        return palindrome(SubString(s,2,len-1))
    end
    return false
end
```



## k


```k
is_palindrome:{x~|x}
```



## Kotlin


```scala
// version 1.1.2

/* These functions deal automatically with Unicode as all strings are UTF-16 encoded in Kotlin */

fun isExactPalindrome(s: String) = (s == s.reversed())

fun isInexactPalindrome(s: String): Boolean {
    var t = ""
    for (c in s) if (c.isLetterOrDigit()) t += c
    t = t.toLowerCase()
    return t == t.reversed()
}

fun main(args: Array<String>) {
    val candidates = arrayOf("rotor", "rosetta", "step on no pets", "été")
    for (candidate in candidates) {
        println("'$candidate' is ${if (isExactPalindrome(candidate)) "an" else "not an"} exact palindrome")
    }
    println()
    val candidates2 = arrayOf(
         "In girum imus nocte et consumimur igni",
         "Rise to vote, sir",
         "A man, a plan, a canal - Panama!",
         "Ce repère, Perec"  // note: 'è' considered a distinct character from 'e'
    )
    for (candidate in candidates2) {
        println("'$candidate' is ${if (isInexactPalindrome(candidate)) "an" else "not an"} inexact palindrome")
    }
}
```


{{out}}

```txt

'rotor' is an exact palindrome
'rosetta' is not an exact palindrome
'step on no pets' is an exact palindrome
'été' is an exact palindrome

'In girum imus nocte et consumimur igni' is an inexact palindrome
'Rise to vote, sir' is an inexact palindrome
'A man, a plan, a canal - Panama!' is an inexact palindrome
'Ce repère, Perec' is not an inexact palindrome

```



## LabVIEW

{{VI solution|LabVIEW_Palindrome_detection.png}}


## Langur


```Langur
val .ispal = f len(.s) > 0 and .s == s2s .s, len(.s)..1

val .tests = h{
    "": false,
    "z": true,
    "aha": true,
    "αηα": true,
    "αννα": true,
    "αννασ": false,
    "sees": true,
    "seas": false,
    "deified": true,
    "solo": false,
    "solos": true,
    "amanaplanacanalpanama": true,
    "a man a plan a canal panama": false,   # true if we remove spaces
    "ingirumimusnocteetconsumimurigni": true,
}

for .word in sort(keys .tests) {
    val .foundpal = .ispal(.word)
    writeln .word, ": ", .foundpal, if(.foundpal == .tests[.word]: ""; " (FAILED TEST)")
}
```


{{out}}

```txt
: false
a man a plan a canal panama: false
aha: true
amanaplanacanalpanama: true
deified: true
ingirumimusnocteetconsumimurigni: true
seas: false
sees: true
solo: false
solos: true
z: true
αηα: true
αννα: true
αννασ: false
```



## Lasso


```Lasso
define ispalindrome(text::string) => {

	local(_text = string(#text)) // need to make copy to get rid of reference issues

	#_text -> replace(regexp(`(?:$|\W)+`), -ignorecase)

	local(reversed = string(#_text))
	#reversed -> reverse

	return #_text == #reversed
}

ispalindrome('Tätatät') // works with high ascii
ispalindrome('Hello World')

ispalindrome('A man, a plan, a canoe, pasta, heros, rajahs, a coloratura, maps, snipe, percale, macaroni, a gag, a banana bag, a tan, a tag, a banana bag again (or a camel), a crepe, pins, Spam, a rut, a Rolo, cash, a jar, sore hats, a peon, a canal – Panama!')
```

{{out}}

```txt
true
false
true
```



## Liberty BASIC


```lb
print isPalindrome("In girum imus nocte et consumimur igni")
print isPalindrome(removePunctuation$("In girum imus nocte et consumimur igni", "S"))
print isPalindrome(removePunctuation$("In girum imus nocte et consumimur igni", "SC"))

function isPalindrome(string$)
    isPalindrome = 1
    for i = 1 to int(len(string$)/2)
        if mid$(string$, i, 1) <> mid$(string$, len(string$)-i+1, 1) then isPalindrome = 0 : exit function
    next i
end function

function removePunctuation$(string$, remove$)
    'P = remove puctuation.  S = remove spaces   C = remove case
    If instr(upper$(remove$), "C") then string$ = lower$(string$)
    If instr(upper$(remove$), "P") then removeCharacters$ = ",.!'()-&*?<>:;~[]{}"
    If instr(upper$(remove$), "S") then removeCharacters$ = removeCharacters$;" "

    for i = 1 to len(string$)
        if instr(removeCharacters$, mid$(string$, i, 1)) then string$ = left$(string$, i-1);right$(string$, len(string$)-i) : i = i - 1
    next i
    removePunctuation$ = string$
end function
```


{{out}}

```txt

0
0
1

```



## LiveCode

This implementation defaults to exact match, but has an optional parameter to do inexact.
```LiveCode
function palindrome txt exact
    if exact is empty or exact is not false then
        set caseSensitive to true  --default is false
    else
        replace space with empty in txt
        put lower(txt) into txt
    end if
    return txt is reverse(txt)
end palindrome

function reverse str
    repeat with i = the length of str down to 1
        put byte i of str after revstr
    end repeat
    return revstr
end reverse
```



## Logo


```logo
to palindrome? :w
  output equal? :w reverse :w
end
```



## Lua


```lua
function ispalindrome(s) return s == string.reverse(s) end
```



## M4


'''Non-recursive'''
This uses the <code>invert</code> from [[Reversing a string]].

```m4
define(`palindrorev',`ifelse(`$1',invert(`$1'),`yes',`no')')dnl
palindrorev(`ingirumimusnocteetconsumimurigni')
palindrorev(`this is not palindrome')
```


'''Recursive'''

```m4
define(`striptwo',`substr(`$1',1,eval(len(`$1')-2))')dnl
define(`cmplast',`ifelse(`striptwo(`$1')',,`yes',dnl
substr(`$1',0,1),substr(`$1',eval(len(`$1')-1),1),`yes',`no')')dnl
define(`palindro',`dnl
ifelse(eval(len(`$1')<1),1,`yes',cmplast(`$1'),`yes',`palindro(striptwo(`$1'))',`no')')dnl
palindro(`ingirumimusnocteetconsumimurigni')
palindro(`this is not palindrome')
```




## Maple


This uses functions from Maple's built-in <code>StringTools</code> package.


```Maple

with(StringTools):

IsPalindrome("ingirumimusnocteetconsumimurigni");

IsPalindrome("In girum imus nocte et consumimur igni");

IsPalindrome(LowerCase(DeleteSpace("In girum imus nocte et consumimur igni")));

```


{{out}}

```txt

                                    true

                                    false

                                    true

```



## Mathematica

Custom functions:

'''Non-recursive'''


```Mathematica
PalindromeQ[i_String] := StringReverse[i] == i
```


Test numbers:

PalindromeQ[i_Integer] := Reverse[IntegerDigits[i]] == IntegerDigits[i];

{{out|Examples}}

```txt
PalindromeQ["TNT"]
PalindromeRecQ["TNT"]
PalindromeQ["test"]
PalindromeRecQ["test"]
PalindromeQ["deified"]
PalindromeRecQ["deified"]
PalindromeQ["sal&agrave;las"]
PalindromeRecQ["sal&agrave;las"]
PalindromeQ["ingirumimusnocteetconsumimurigni"]
PalindromeRecQ["ingirumimusnocteetconsumimurigni"]
```


Note that the code block doesn't correctly show the &agrave; in sal&agrave;las.
{{out}}

```txt
True
True
False
False
True
True
True
True
True
True
```



## MATLAB


```MATLAB
function trueFalse = isPalindrome(string)

    trueFalse = all(string == fliplr(string)); %See if flipping the string produces the original string

    if not(trueFalse) %If not a palindrome
        string = lower(string); %Lower case everything
        trueFalse = all(string == fliplr(string)); %Test again
    end

    if not(trueFalse) %If still not a palindrome
        string(isspace(string)) = []; %Strip all space characters out
        trueFalse = all(string == fliplr(string)); %Test one last time
    end

end
```


{{out|Sample Usage}}

```MATLAB>>
 isPalindrome('In girum imus nocte et consumimur igni')

ans =

     1

```



## Maxima


```maxima
palindromep(s) := block([t], t: sremove(" ", sdowncase(s)), sequal(t, sreverse(t)))$

palindromep("Sator arepo tenet opera rotas");  /* true */
```



## MAXScript


'''Non-recursive'''


```maxscript
fn isPalindrome s =
(
    local reversed = ""
    for i in s.count to 1 by -1 do reversed += s[i]
    return reversed == s
)
```


'''Recursive'''


```maxscript
fn isPalindrome_r s =
(
    if s.count <= 1 then
    (
        true
    )
    else
    (
        if s[1] != s[s.count] then
        (
            return false
        )
        isPalindrome_r (substring s 2 (s.count-2))
    )
)
```


'''Testing'''


```maxscript
local p = "ingirumimusnocteetconsumimurigni"
format ("'%' is a palindrome? %\n") p (isPalindrome p)
format ("'%' is a palindrome? %\n") p (isPalindrome_r p)
```



## min

{{works with|min|0.19.3}}

```min
(dup reverse ==) :palindrome?
(dup "" split reverse "" join ==) :str-palindrome?

"apple" str-palindrome? puts
"racecar" str-palindrome? puts
(a b c) palindrome? puts
(a b b a) palindrome? puts
```

{{out}}

```txt

false
true
false
true

```



## MiniScript


```MiniScript
isPalindrome = function(s)
    // convert to lowercase, and strip non-letters
    stripped = ""
    for c in s.lower
    	if c >= "a" and c <= "z" then stripped = stripped + c
    end for

    // check palindromity
    mid = floor(stripped.len/2)
    for i in range(0, mid)
    	if stripped[i] != stripped[-i - 1] then return false
    end for
    return true
end function

testStr = "Madam, I'm Adam"
answer = [testStr, "is"]
if not isPalindrome(testStr) then answer.push "NOT"
answer.push "a palindrome"
print answer.join
```

{{out}}

```txt

Madam, I'm Adam is a palindrome

```



## Mirah


```mirah
def reverse(s:string)
    StringBuilder.new(s).reverse.toString()
end

def palindrome?(s:string)
    s.equals(reverse(s))
end

puts palindrome?("anna")        # ==> true
puts palindrome?("Erik")        # ==> false
puts palindrome?("palindroom-moordnilap") # ==> true
puts nil                        # ==> null
```



## ML

=
## mLite
=

```ocaml
fun to_locase s = implode ` map (c_downcase) ` explode s

fun only_alpha s = implode ` filter (fn x = c_alphabetic x) ` explode s

fun is_palin
	( h1 :: t1, h2 :: t2, n = 0 ) 		       = true
|	( h1 :: t1, h2 :: t2, n ) where ( h1 eql h2 )  = is_palin( t1, t2, n - 1)
|	( h1 :: t1, h2 :: t2, n )                      = false
|       (str s) =
		let
			val es = explode ` to_locase ` only_alpha s;
			val res = rev es;
			val k = (len es) div 2
		in
			is_palin (es, res, k)
		end

fun test_is_palin s =
	(print "\""; print s; print "\" is a palindrome: "; print ` is_palin s; println "")

fun test (f, arg, res, ok, notok) = if (f arg eql res) then ("'" @ arg @ "' " @ ok) else ("'" @ arg @ "' " @ notok)

;

println ` test (is_palin, "In girum imus nocte, et consumimur igni", true, "is a palindrome", "is NOT a palindrome");
println ` test (is_palin, "Madam, I'm Adam.", true, "is a palindrome", "is NOT a palindrome");
println ` test (is_palin, "salàlas", true, "is a palindrome", "is NOT a palindrome");
println ` test (is_palin, "radar", true, "is a palindrome", "is NOT a palindrome");
println ` test (is_palin, "Lagerregal", true, "is a palindrome", "is NOT a palindrome");
println ` test (is_palin, "Ein Neger mit Gazelle zagt im Regen nie.", true, "is a palindrome", "is NOT a palindrome");
println ` test (is_palin, "something wrong", true, "is a palindrome", "is NOT a palindrome");
```

Output:

```txt
'In girum imus nocte, et consumimur igni' is a palindrome
'Madam, I'm Adam.' is a palindrome
'salàlas' is a palindrome
'radar' is a palindrome
'Lagerregal' is a palindrome
'Ein Neger mit Gazelle zagt im Regen nie.' is a palindrome
'something wrong' is NOT a palindrome

```


=
## Standard ML
=

```sml

fun palindrome s =
  let val cs = explode s in
    cs = rev cs
  end

```



## MMIX


```mmix
argc     IS $0
argv     IS $1

         LOC Data_Segment
DataSeg  GREG @

          LOC @+1000
ItsPalStr IS @-Data_Segment
          BYTE "It's palindrome",10,0
          LOC @+(8-@)&7
NoPalStr  IS  @-Data_Segment
          BYTE "It is not palindrome",10,0

         LOC #100
         GREG @
% input: $255 points to where the string to be checked is
% returns $255 0 if not palindrome, not zero otherwise
% trashs: $0,$1,$2,$3
% return address $4
DetectPalindrome LOC @
         ADDU $1,$255,0      % $1 = $255
2H       LDB  $0,$1,0        % get byte at $1
         BZ   $0,1F          % if zero, end (length)
         INCL $1,1           % $1++
         JMP  2B             % loop
1H       SUBU $1,$1,1        % ptr last char of string
         ADDU $0,DataSeg,0   % $0 to data seg.
3H       CMP  $3,$1,$255     % is $0 == $255?
         BZ   $3,4F          % then jump
         LDB  $3,$1,0        % otherwise get the byte
         STB  $3,$0,0        % and copy it
         INCL $0,1           % $0++
         SUB  $1,$1,1        % $1--
         JMP  3B
4H       LDB  $3,$1,0
         STB  $3,$0,0        % copy the last byte
% now let us compare reversed string and straight string
         XOR  $0,$0,$0       % index
         ADDU $1,DataSeg,0
6H       LDB  $2,$1,$0       % pick char from rev str
         LDB  $3,$255,$0     % pick char from straight str
         BZ   $3,PaliOk      % finished as palindrome
         CMP  $2,$2,$3       % == ?
         BNZ  $2,5F          % if not, exit
         INCL $0,1           % $0++
         JMP  6B
5H       XOR  $255,$255,$255
         GO   $4,$4,0        % return false
PaliOk   NEG  $255,0,1
         GO   $4,$4,0        % return true

% The Main for testing the function
% run from the command line
% $ mmix ./palindrome.mmo ingirumimusnocteetconsumimurigni
Main     CMP  argc,argc,2    % argc > 2?
         BN   argc,3F        % no -> not enough arg
         ADDU $1,$1,8        % argv+1
         LDOU $255,$1,0      % argv[1]
         GO   $4,DetectPalindrome
         BZ   $255,2F        % if not palindrome, jmp
         SETL $0,ItsPalStr   % pal string
         ADDU $255,DataSeg,$0
         JMP  1F
2H       SETL $0,NoPalStr    % no pal string
         ADDU $255,DataSeg,$0
1H       TRAP 0,Fputs,StdOut % print
3H       XOR  $255,$255,$255
         TRAP 0,Halt,0       % exit(0)
```


=={{header|Modula-2}}==

```modula2
MODULE Palindrome;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,ReadChar;

PROCEDURE IsPalindrome(str : ARRAY OF CHAR) : BOOLEAN;
VAR i,m : INTEGER;
VAR buf : ARRAY[0..63] OF CHAR;
BEGIN
    i := 0;
    m := HIGH(str) - 1;
    WHILE i<m DO
        IF str[i] # str[m-i] THEN
            RETURN FALSE
        END;
        INC(i)
    END;
    RETURN TRUE
END IsPalindrome;

PROCEDURE Print(str : ARRAY OF CHAR);
VAR buf : ARRAY[0..63] OF CHAR;
BEGIN
    FormatString("%s: %b\n", buf, str, IsPalindrome(str));
    WriteString(buf)
END Print;

BEGIN
    Print("");
    Print("z");
    Print("aha");
    Print("sees");
    Print("oofoe");
    Print("deified");
    Print("Deified");
    Print("amanaplanacanalpanama");
    Print("ingirumimusnocteetconsumimurigni");

    ReadChar
END Palindrome.
```


=={{header|Modula-3}}==

```modula3
MODULE Palindrome;

IMPORT Text;

PROCEDURE isPalindrome(string: TEXT): BOOLEAN =
  VAR len := Text.Length(string);
  BEGIN
    FOR i := 0 TO len DIV 2 - 1 DO
      IF Text.GetChar(string, i) # Text.GetChar(string, (len - i - 1)) THEN
        RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END isPalindrome;
END Palindrome.
```



## Nemerle


```Nemerle
using System;
using System.Console;
using Nemerle.Utility.NString; //contains methods Explode() and Implode() which convert string -> list[char] and back

module Palindrome
{
    IsPalindrome( text : string) : bool
    {
        Implode(Explode(text).Reverse()) == text;
    }

    Main() : void
    {
        WriteLine("radar is a palindrome: {0}", IsPalindrome("radar"));
    }
}
```

And a function to remove spaces and punctuation and convert to lowercase

```Nemerle
Clean( text : string ) : string
{
    def sepchars = Explode(",.;:-?!()' ");
    Concat( "", Split(text, sepchars)).ToLower()
}
```



## NetRexx

{{Trans|REXX}}

```netrexx

y='In girum imus nocte et consumimur igni'

-- translation: We walk around in the night and
-- we are burnt by the fire (of love)
say
say 'string = 'y
say

pal=isPal(y)

if pal==0 then say "The string isn't palindromic."
          else say 'The string is palindromic.'

method isPal(x) static
  x=x.upper().space(0)          /* removes all blanks (spaces)          */
                                /*   and translate to uppercase.        */
  return x==x.reverse()         /* returns  1  if exactly equal         */

```



## NewLISP

Works likewise for strings and for lists

```lisp

(define (palindrome? s)
    (setq r s)
    (reverse r) ; Reverse is destructive.
    (= s r))

;; Make ‘reverse’ non-destructive and avoid a global variable
(define (palindrome? s)
    (= s (reverse (copy s))))

```



## Nim


```nim
proc reverse(s: string): string =
  result = newString(s.len)
  for i,c in s:
    result[s.high - i] = c

proc isPalindrome(s: string): bool =
  s == reverse(s)

echo isPalindrome("FoobooF")
```



## Objeck


```objeck

bundle Default {
  class Test {
    function : Main(args : String[]) ~ Nil {
      IsPalindrome("aasa")->PrintLine();
      IsPalindrome("acbca")->PrintLine();
      IsPalindrome("xx")->PrintLine();
    }

    function : native : IsPalindrome(s : String) ~ Bool {
      l := s->Size();
      for(i := 0; i < l / 2; i += 1;) {
        if(s->Get(i) <> s->Get(l - i - 1)) {
          return false;
        };
      };

      return true;
    }
  }
}

```



## OCaml



```ocaml
let is_palindrome s =
    let l = String.length s in
    let rec comp n =
        n = 0 || (s.[l-n] = s.[n-1] && comp (n-1)) in
    comp (l / 2)
```


and here a function to remove the white spaces in the string:


```ocaml
let rem_space str =
  let len = String.length str in
  let res = Bytes.create len in
  let rec aux i j =
    if i >= len
    then (Bytes.sub_string res 0 j)
    else match str.[i] with
      | ' ' | '\n' | '\t' | '\r' ->
        aux (i+1) (j)
      | _ ->
        Bytes.set res j str.[i];
        aux (i+1) (j+1)
  in
  aux 0 0

```


and to make the test case insensitive, just use the function <tt>String.lowercase_ascii</tt>.


## Oforth



```Oforth>String method: isPalindrome  self reverse self == ;</lang



## Octave

'''Recursive'''

```octave
function v = palindro_r(s)
  if ( length(s) == 1 )
    v = true;
    return;
  elseif ( length(s) == 2 )
    v = s(1) == s(2);
    return;
  endif
  if ( s(1) == s(length(s)) )
    v = palindro_r(s(2:length(s)-1));
  else
    v = false;
  endif
endfunction
```


'''Non-recursive'''

```octave
function v = palindro(s)
  v = all( (s == s(length(s):-1:1)) == 1);
endfunction
```


'''Testing'''

```octave
palindro_r("ingirumimusnocteetconsumimurigni")
palindro("satorarepotenetoperarotas")
```



## Ol


```scheme

; simple case - only lowercase letters
(define (palindrome? str)
   (let ((l (string->runes str)))
      (equal? l (reverse l))))

(print (palindrome? "ingirumimusnocteetconsumimurigni"))
; ==> #true
(print (palindrome? "thisisnotapalindrome"))
; ==> #false


; complex case - with ignoring letter case and punctuation
(define (alpha? x)
   (<= #\a x #\z))
(define (lowercase x)
   (if (<= #\A x #\Z)
      (- x (- #\A #\a))
      x))

(define (palindrome? str)
   (let ((l (filter alpha? (map lowercase (string->runes str)))))
      (equal? l (reverse l))))

(print (palindrome? "A man, a plan, a cat, a ham, a yak, a yam, a hat, a canal-Panama!"))
; ==> #true
(print (palindrome? "This is not a palindrome"))
; ==> #false

```



## Oz


```oz
fun {IsPalindrome S}
  {Reverse S} == S
end
```



## PARI/GP


```parigp
ispal(s)={
  s=Vec(s);
  for(i=1,#v\2,
    if(v[i]!=v[#v-i+1],return(0))
  );
  1
};
```


A version for numbers:
{{works with|PARI/GP|2.6.0 and above}}

```parigp
ispal(s)={
  my(d=digits(n));
  for(i=1,#d\2,
    if(d[i]!=d[n+1=i],return(0))
  );
  1
};
```



## Pascal

{{works with|Free Pascal}}

```pascal
program Palindro;

{ RECURSIVE }
function is_palindro_r(s : String) : Boolean;
begin
   if length(s) <= 1 then
      is_palindro_r := true
   else begin
      if s[1] = s[length(s)] then
	 is_palindro_r := is_palindro_r(copy(s, 2, length(s)-2))
      else
	 is_palindro_r := false
   end
end; { is_palindro_r }

{ NON RECURSIVE; see [[Reversing a string]] for "reverse" }
function is_palindro(s : String) : Boolean;
begin
   if s = reverse(s) then
      is_palindro := true
   else
      is_palindro := false
end;
```



```pascal
procedure test_r(s : String; r : Boolean);
begin
   write('"', s, '" is ');
   if ( not r ) then
      write('not ');
   writeln('palindrome')
end;

var
   s1, s2 : String;

begin
   s1 := 'ingirumimusnocteetconsumimurigni';
   s2 := 'in girum imus nocte';
   test_r(s1, is_palindro_r(s1));
   test_r(s2, is_palindro_r(s2));
   test_r(s1, is_palindro(s1));
   test_r(s2, is_palindro(s2))
end.
```



## Perl

There is more than one way to do this.

* '''palindrome''' uses the built-in function <tt>reverse()</tt>.
* '''palindrome_c''' uses iteration; it is a translation of the [[{{PAGENAME}}#C|C solution]].
* '''palindrome_r''' uses recursion.
* '''palindrome_e''' uses a recursive regular expression.

All of these functions take a parameter,
or default to <tt>$_</tt> if there is no parameter.
None of these functions ignore case or strip characters;
if you want do that, you can use <tt>($s = lc $s) =~ s/[\W_]//g</tt>
before you call these functions.


```perl
# Palindrome.pm
package Palindrome;

use strict;
use warnings;

use Exporter 'import';
our @EXPORT = qw(palindrome palindrome_c palindrome_r palindrome_e);

sub palindrome
{
    my $s = (@_ ? shift : $_);
    return $s eq reverse $s;
}

sub palindrome_c
{
    my $s = (@_ ? shift : $_);
    for my $i (0 .. length($s) >> 1)
    {
        return 0 unless substr($s, $i, 1) eq substr($s, -1 - $i, 1);
    }
    return 1;
}

sub palindrome_r
{
    my $s = (@_ ? shift : $_);
    if (length $s <= 1) { return 1; }
    elsif (substr($s, 0, 1) ne substr($s, -1, 1)) { return 0; }
    else { return palindrome_r(substr($s, 1, -1)); }
}

sub palindrome_e
{
    (@_ ? shift : $_) =~ /^(.?|(.)(?1)\2)$/ + 0
}
```


This example shows how to use the functions:


```perl
# pbench.pl
use strict;
use warnings;

use Benchmark qw(cmpthese);
use Palindrome;

printf("%d, %d, %d, %d: %s\n",
       palindrome, palindrome_c, palindrome_r, palindrome_e, $_)
for
    qw/a aa ab abba aBbA abca abba1 1abba
    ingirumimusnocteetconsumimurigni/,
    'ab cc ba',	'ab ccb a';

printf "\n";

my $latin = "ingirumimusnocteetconsumimurigni";
cmpthese(100_000, {
    palindrome => sub { palindrome $latin },
    palindrome_c => sub { palindrome_c $latin },
    palindrome_r => sub { palindrome_r $latin },
    palindrome_e => sub { palindrome_e $latin },
});
```


{{out}} on a machine running Perl 5.10.1 on amd64-openbsd:

```txt
$ perl pbench.pl
1, 1, 1, 1: a
1, 1, 1, 1: aa
0, 0, 0, 0: ab
1, 1, 1, 1: abba
0, 0, 0, 0: aBbA
0, 0, 0, 0: abca
0, 0, 0, 0: abba1
0, 0, 0, 0: 1abba
1, 1, 1, 1: ingirumimusnocteetconsumimurigni
1, 1, 1, 1: ab cc ba
0, 0, 0, 0: ab ccb a

            (warning: too few iterations for a reliable count)
                  Rate palindrome_r palindrome_e palindrome_c   palindrome
palindrome_r   51020/s           --         -50%         -70%         -97%
palindrome_e  102041/s         100%           --         -41%         -94%
palindrome_c  172414/s         238%          69%           --         -90%
palindrome   1666667/s        3167%        1533%         867%           --
```


With this machine, palindrome() ran far faster than the alternatives
(and too fast for a reliable count).
The Perl regular expression engine recursed twice as fast as the Perl interpreter.


## Perl 6


```perl6
subset Palindrom of Str where {
    .flip eq $_ given .comb(/\w+/).join.lc
}

my @tests = q:to/END/.lines;
    A man, a plan, a canal: Panama.
    My dog has fleas
    Madam, I'm Adam.
    1 on 1
    In girum imus nocte et consumimur igni
    END

for @tests { say $_ ~~ Palindrom, "\t", $_ }
```

{{out}}

```txt
True	A man, a plan, a canal: Panama.
False	My dog has fleas
True	Madam, I'm Adam.
False	1 on 1
True	In girum imus nocte et consumimur igni

```



## Phix


```Phix
function is_palindrome(sequence s)
    return s==reverse(s)
end function

?is_palindrome("rotator") -- prints 1
?is_palindrome("tractor") -- prints 0

constant punctuation = " `~!@#$%^&*()-=_+[]{}\\|;:',.<>/?",
         nulls = repeat("",length(punctuation))

function extra_credit(sequence s)
    s = utf8_to_utf32(lower(substitute_all(s,punctuation,nulls)))
    return s==reverse(s)
end function

-- these all print 1 (true)
?extra_credit("Madam, I'm Adam.")
?extra_credit("A man, a plan, a canal: Panama!")
?extra_credit("In girum imus nocte et consumimur igni")
?extra_credit("人人為我,我為人人")
?extra_credit("Я иду с мечем, судия")
?extra_credit("아들딸들아")
?extra_credit("가련하시다 사장집 아들딸들아 집장사 다시 하련가")
?extra_credit("tregða, gón, reiði - er nóg að gert")
```



## PHP


```php
<?php
function is_palindrome($string) {
  return $string == strrev($string);
}
?>
```


Regular expression-based solution ([http://www.polygenelubricants.com/2010/09/matching-palindromes-in-pcre-regex.html source])

```php
<?php
function is_palindrome($string) {
  return preg_match('/^(?:(.)(?=.*(\1(?(2)\2|))$))*.?\2?$/', $string);
}
?>
```



## PicoLisp


```PicoLisp
(de palindrome? (S)
   (= (setq S (chop S)) (reverse S)) )
```

{{out}}

```txt
: (palindrome? "ingirumimusnocteetconsumimurigni")
-> T
```



## Pike


```pike
int main(){
   if(pal("rotator")){
      write("palindrome!\n");
   }
   if(!pal("asdf")){
      write("asdf isn't a palindrome.\n");
   }
}

int pal(string input){
   if( reverse(input) == input ){
      return 1;
   } else {
      return 0;
   }
}
```



## PL/I

To satisfy the revised specification (which contradicts the preceding explanation)
the following trivially solves the problem in PL/I:

```PL/I
is_palindrome = (text = reverse(text));
```


The following solution strips spaces:
<lang>is_palindrome: procedure (text) returns (bit(1));
   declare text character (*) varying;

   text = remove_blanks(text);
   text = lowercase(text);
   return (text = reverse(text));

remove_blanks: procedure (text);
   declare text character (*) varying;
   declare (i, j) fixed binary (31);
   j = 0;
   do i = 1 to length(text);
      if substr(text, i, 1) = ' ' then
         do; j = j + 1; substr(text, j, 1) = substr(text, i, 1); end;
   end;
   return (substr(text, 1, j));
end remove_blanks;
end is_palindrome;
```



## Potion


```Potion
# The readable recursive version
palindrome_i = (s, b, e):
  if (e <= b): true.
  elsif (s ord(b) != s ord(e)): false.
  else: palindrome_i(s, b+1, e-1).
.

palindrome = (s):
  palindrome_i(s, 0, s length - 1).

palindrome(argv(1))
```



## PowerBASIC


The output is identical to the [[#BASIC|QBasic]] version, above.


```powerbasic
FUNCTION isPalindrome (what AS STRING) AS LONG
    DIM whatcopy AS STRING, chk AS STRING, tmp AS STRING * 1, L0 AS LONG

    FOR L0 = 1 TO LEN(what)
        tmp = UCASE$(MID$(what, L0, 1))
        SELECT CASE tmp
            CASE "A" TO "Z"
                whatcopy = whatcopy & tmp
                chk = tmp & chk
            CASE "0" TO "9"
                MSGBOX "Numbers are cheating! (""" & what & """)"
                FUNCTION = 0
                EXIT FUNCTION
        END SELECT
    NEXT

    FUNCTION = ISTRUE((whatcopy) = chk)
END FUNCTION


FUNCTION PBMAIN () AS LONG
    DATA "My dog has fleas", "Madam, I'm Adam.", "1 on 1", "In girum imus nocte et consumimur igni"
    DIM L1 AS LONG, w AS STRING
    FOR L1 = 1 TO DATACOUNT
        w = READ$(L1)
        IF ISTRUE(isPalindrome(w)) THEN
            MSGBOX $DQ & w & """ is a palindrome"
        ELSE
            MSGBOX $DQ & w & """ is not a palindrome"
        END IF
    NEXT
END FUNCTION
```




## PowerShell

An exact version based on reversing the string:

```PowerShell

Function Test-Palindrome( [String] $Text ){
    $CharArray = $Text.ToCharArray()
    [Array]::Reverse($CharArray)
    $Text -eq [string]::join('', $CharArray)
}

```

===PowerShell (Regex Version)===
This version is much faster because it does not manipulate arrays. [This is not clear; the above version was slowed down by using -join instead of [string]::join, and -like instead of -eq. After changing those it is similar, if not faster, than this version].

```PowerShell

function Test-Palindrome
{
  <#
    .SYNOPSIS
        Tests if a string is a palindrome.
    .DESCRIPTION
        Tests if a string is a true palindrome or, optionally, an inexact palindrome.
    .EXAMPLE
        Test-Palindrome -Text "racecar"
    .EXAMPLE
        Test-Palindrome -Text '"Deliver desserts," demanded Nemesis, "emended, named, stressed, reviled."' -Inexact
  #>
    [CmdletBinding()]
    [OutputType([bool])]
    Param
    (
        # The string to test for palindrominity.
        [Parameter(Mandatory=$true)]
        [string]
        $Text,

        # When specified, detects an inexact palindrome.
        [switch]
        $Inexact
    )

    if ($Inexact)
    {
        # Strip all punctuation and spaces
        $Text = [Regex]::Replace("$Text($7&","[^1-9a-zA-Z]","")
    }

    $Text -match "^(?'char'[a-z])+[a-z]?(?:\k'char'(?'-char'))+(?(char)(?!))$"
}

```


```PowerShell

Test-Palindrome -Text 'radar'

```

{{Out}}

```txt

True

```


```PowerShell

Test-Palindrome -Text "In girum imus nocte et consumimur igni."

```

{{Out}}

```txt

False

```


```PowerShell

Test-Palindrome -Text "In girum imus nocte et consumimur igni." -Inexact

```

{{Out}}

```txt

True

```

===PowerShell (Unicode category aware, no string reverse)===
An inexact version can remove punctuation by looking at Unicode categories for each character, either using .Net methods or a regex.

```PowerShell
Function Test-Palindrome {
[CmdletBinding()]
Param(
    [Parameter(ValueFromPipeline)]
    [string[]]$Text
)

process {
    :stringLoop foreach ($T in $Text)
    {
        # Normalize Unicode combining characters,
        # so character á compares the same as (a+combining accent)
        $T = $T.Normalize([Text.NormalizationForm]::FormC)

        # Remove anything from outside the Unicode category
        # "Letter from any language"
        $T = $T -replace '\P{L}', ''

        # Walk from each end of the string inwards,
        # comparing a char at a time.
        # Avoids string copy / reverse overheads.
        $Left, $Right = 0, [math]::Max(0, ($T.Length - 1))
        while ($Left -lt $Right)
        {
            if ($T[$Left] -ne $T[$Right])
            {
                # return early if string is not a palindrome
                [PSCustomObject]@{
                    Text = $T
                    IsPalindrome = $False
                }
                continue stringLoop
            }
            else
            {
                $Left++
                $Right--
            }
        }

        # made it to here, then string is a palindrome
        [PSCustomObject]@{
            Text = $T
            IsPalindrome = $True
        }

    }
}
}
'ánu-ná', 'nowt' | Test-Palindrome
```

{{Out}}

```txt

PS C:\> 'ánu-ná', 'nowt' | Test-Palindrome

Text  IsPalindrome
----  ------------
ánuná         True
now          False

```



## Processing


```processing

void setup(){
	println(isPalindrome(InsertPalindromeHere));
}

boolean isPalindrome(string check){
	char[] letters = new char[check.length];
	string invert = " ";
	string modCheck = " " + check;
	for(int i = 0; i < letters.length; i++){
		letters[i] = check.charAt(i);
	}
	for(int i = letters.length-1; i >= 0; i--){
		invert = invert + letters[i];
	}

	if(invert == modCheck){
		return true;
	} else {
		return false;
	}
}

```


{{out}}

```txt

"true" or "false" depending

```



## Prolog

'''Non-recursive'''

From [http://www2.dcs.hull.ac.uk/NEAT/dnd/AI/prolog/tutorial2.html this tutorial].


```prolog
palindrome(Word) :- name(Word,List), reverse(List,List).
```


'''Recursive'''

{{works with|SWI Prolog}}

```prolog
pali(Str) :- sub_string(Str, 0, 1, _, X), string_concat(Str2, X, Str), string_concat(X, Mid, Str2), pali(Mid).
pali(Str) :- string_length(Str, Len), Len < 2.
```


Changing '''string''' into '''atom''' makes the program run also on GNU Prolog. I.e.

{{works with|GNU Prolog}}


```prolog
pali(Str) :- sub_atom(Str, 0, 1, _, X), atom_concat(Str2, X, Str), atom_concat(X, Mid, Str2), pali(Mid).
pali(Str) :- atom_length(Str, Len), Len < 2.
```


## PureBasic

{{works with|PureBasic|4.41}}

```PureBasic
Procedure IsPalindrome(StringToTest.s)
  If StringToTest=ReverseString(StringToTest)
    ProcedureReturn 1
  Else
    ProcedureReturn 0
  EndIf
EndProcedure
```



## Python

Now that Python 2.7 and Python 3.4 are quite different, We should include the version IMHO.

'''Non-recursive'''

This one uses the ''reversing the string'' technique
(to reverse a string Python can use the odd
but right syntax <tt>string[::-1]</tt>)


```python
def is_palindrome(s):
  return s == s[::-1]
```


'''Non-recursive, Ignoring Punctuation/Case/Spaces'''

A word is a palindrome if the letters are the same forwards as backwards, but the other methods given here will return False for, e.g., an input of "Go hang a salami, I'm a lasagna hog" or "A man, a plan, a canal: Panama." An implementation that traverses the string and ignores case differences, spaces, and non-alpha characters is pretty trivial.


```python
def is_palindrome(s):
  low = 0
  high = len(s) - 1
  while low < high:
    if not s[low].isalpha():
      low += 1
    elif not s[high].isalpha():
      high -= 1
    else:
      if s[low].lower() != s[high].lower():
        return False
      else:
        low += 1
        high -= 1
        return True
```


'''Recursive'''


```python
def is_palindrome_r(s):
  if len(s) <= 1:
    return True
  elif s[0] != s[-1]:
    return False
  else:
    return is_palindrome_r(s[1:-1])
```


Python has short-circuit evaluation of Boolean operations
so a shorter and still easy to understand recursive function is


```python
def is_palindrome_r2(s):
  return not s or s[0] == s[-1] and is_palindrome_r2(s[1:-1])
```


'''Testing'''


```python
def test(f, good, bad):
  assert all(f(x) for x in good)
  assert not any(f(x) for x in bad)
  print '%s passed all %d tests' % (f.__name__, len(good)+len(bad))

pals = ('', 'a', 'aa', 'aba', 'abba')
notpals = ('aA', 'abA', 'abxBa', 'abxxBa')
for ispal in is_palindrome, is_palindrome_r, is_palindrome_r2:
  test(ispal, pals, notpals)
```


''' Palindrome Using Regular Expressions  Python 2.7 '''


```python
def p_loop():
  import re, string
  re1=""       # Beginning of Regex
  re2=""       # End of Regex
  pal=raw_input("Please Enter a word or phrase: ")
  pd = pal.replace(' ','')
  for c in string.punctuation:
     pd = pd.replace(c,"")
  if pal == "" :
    return -1
  c=len(pd)   # Count of chars.
  loops = (c+1)/2
  for x in range(loops):
    re1 = re1 + "(\w)"
    if (c%2 == 1 and x == 0):
       continue
    p = loops - x
    re2 = re2 + "\\" + str(p)
  regex= re1+re2+"$"   # regex is like "(\w)(\w)(\w)\2\1$"
  #print(regex)  # To test regex before re.search
  m = re.search(r'^'+regex,pd,re.IGNORECASE)
  if (m):
     print("\n   "+'"'+pal+'"')
     print("   is a Palindrome\n")
     return 1
  else:
     print("Nope!")
     return 0
```



## R

'''Recursive'''

Note that the recursive method will fail if the string length is too long.
R will assume an infinite recursion if a recursion nests deeper than 5,000.
Options may be set in the environment to increase this to 500,000.

```R
palindro <- function(p) {
  if ( nchar(p) == 1 ) {
    return(TRUE)
  } else if ( nchar(p) == 2 ) {
    return(substr(p,1,1) == substr(p,2,2))
  } else {
    if ( substr(p,1,1) == substr(p, nchar(p), nchar(p)) ) {
      return(palindro(substr(p, 2, nchar(p)-1)))
    } else {
      return(FALSE)
    }
  }
}
```


'''Iterative'''

```R
palindroi <- function(p) {
  for(i in 1:floor(nchar(p)/2) ) {
    r <- nchar(p) - i + 1
    if ( substr(p, i, i) != substr(p, r, r) ) return(FALSE)
  }
  TRUE
}
```


'''Comparative'''

This method is somewhat faster than the other two.

Note that this method incorrectly regards an empty string as not a palindrome.
Please leave this bug in the code, and take a look a the [[Testing_a_Function]] page.

```R
revstring <- function(stringtorev) {
   return(
      paste(
           strsplit(stringtorev,"")[[1]][nchar(stringtorev):1]
           ,collapse="")
           )
}
palindroc <- function(p) {return(revstring(p)==p)}
```


{{out}}

```txt

test <- "ingirumimusnocteetconsumimurigni"
tester <- paste(rep(test,38),collapse="")
> test <- "ingirumimusnocteetconsumimurigni"
> tester <- paste(rep(test,38),collapse="")
> system.time(palindro(tester))
   user  system elapsed
   0.04    0.00    0.04
> system.time(palindroi(tester))
   user  system elapsed
   0.01    0.00    0.02
> system.time(palindroc(tester))
   user  system elapsed
      0       0       0

```



## Racket



```Racket

(define (palindromb str)
  (let* ([lst (string->list (string-downcase str))]
         [slst (remove* '(#\space) lst)])
    (string=? (list->string (reverse slst)) (list->string slst))))

;;example output

> (palindromb "able was i ere i saw elba")
#t
> (palindromb "waht the hey")
#f
> (palindromb "In girum imus nocte et consumimur igni")
#t
>

```



## Rascal

The most simple solution:

```rascal
import String;

public bool palindrome(str text) =  toLowerCase(text) == reverse(text);
```


A solution that handles sentences with spaces and capitals:


```rascal
import String;

public bool palindrome(str text){
	text = replaceAll(toLowerCase(text), " ", "");
	return text == reverse(text);
}

```


Example:

```rascal>rascal
palindrome("In girum imus nocte et consumimur igni")
bool: true
```



## REBOL



```REBOL
REBOL [
    Title: "Palindrome Recognizer"
    URL: http://rosettacode.org/wiki/Palindrome
]

; In order to compete with all the one-liners, the operation is
; compressed: parens force left hand side to evaluate first, where I
; copy the phrase, then uppercase it and assign it to 'p'. Now the
; right hand side is evaluated: p is copied, then reversed in place;
; the comparison is made and implicitely returned.

palindrome?: func [
	phrase [string!] "Potentially palindromatic prose."
	/local p
][(p: uppercase copy phrase) = reverse copy p]

; Teeny Tiny Test Suite

assert: func [code][print [either do code ["  ok"]["FAIL"]  mold code]]

print "Simple palindromes, with an exception for variety:"
repeat phrase ["z" "aha" "sees" "oofoe" "Deified"][
	assert compose [palindrome? (phrase)]]

print [crlf "According to the problem statement, these should fail:"]
assert [palindrome? "A man, a plan, a canal, Panama."] ; Punctuation not ignored.
assert [palindrome? "In girum imus nocte et consumimur igni"] ; Spaces not removed.

; I know we're doing palindromes, not alliteration, but who could resist...?
```


{{out}}

```txt
Simple palindromes, with an exception for variety:
  ok [palindrome? "z"]
  ok [palindrome? "aha"]
  ok [palindrome? "sees"]
FAIL [palindrome? "oofoe"]
  ok [palindrome? "Deified"]

According to the problem statement, these should fail:
FAIL [palindrome? "A man, a plan, a canal, Panama."]
FAIL [palindrome? "In girum imus nocte et consumimur igni"]
```



## Retro



```Retro

:palindrome? (s-f) dup s:hash [ s:reverse s:hash ] dip eq? ;

'ingirumimusnocteetconsumimurigni palindrome? n:put

```



## REXX


### version 1


```REXX
/*REXX pgm checks if phrase is palindromic; ignores the case of the letters.  */
parse arg y                            /*get (optional) phrase from the C.L.  */
if y=''  then y='In girum imus nocte et consumimur igni'    /*[↓] translation.*/
               /*We walk around in the night and we are burnt by the fire (of love).*/
say 'string = ' y
if isTpal(y)  then                   say 'The string is a true palindrome.'
              else if isPal(y)  then say 'The string is an inexact palindrome.'
                                else say "The string isn't palindromic."
exit                                   /*stick a fork in it,  we're all done. */
/*────────────────────────────────────────────────────────────────────────────*/
isTpal:  return reverse(arg(1))==arg(1)
isPal:   return isTpal(translate(space(x,0)))
```

'''output'''

```txt

string =  In girum imus nocte et consumimur igni
The string is an inexact palindrome.

```



### Short version

{{works with|ARexx}}
{{works with|Regina}} (3.8 and later, with options: AREXX_BIFS and AREXX_SEMANTICS)
It should be noted that the   '''COMPRESS'''   function is not a Classic REXX BIF and isn't present in many REXXes.

The   '''SPACE(string,0)'''   BIF can be used instead.

It should also be noted that   '''UPPER'''   BIF is not present in some REXXes.

Use the   '''PARSE UPPER'''   statement or   '''TRANSLATE()'''   BIF instead.

```REXX

/*Check whether a string is a palindrome */
parse pull string
select
	when palindrome(string) then say string 'is an exact palindrome.'
	when palindrome(compress(upper(string))) then say string 'is an inexact palindrome.'
	otherwise say string 'is not palindromic.'
	end
exit 0

palindrome: procedure
parse arg string
return string==reverse(string)

```

{{out}}

```txt

ABBA is an exact palindrome.
In girum imus nocte et consumimur igni is an inexact palindrome.
djdjdj is not palindromic.

```



## Ring


```ring

aString = "radar"
bString = ""
for i=len(aString) to 1 step -1
    bString = bString + aString[i]
next
see aString
if aString = bString see " is a palindrome." + nl
else see " is not a palindrome" + nl ok

```



## Ruby


'''Non-recursive'''


```ruby
def palindrome?(s)
  s == s.reverse
end
```


'''Recursive'''


```ruby
def r_palindrome?(s)
  if s.length <= 1
    true
  elsif s[0] != s[-1]
    false
  else
    r_palindrome?(s[1..-2])
  end
end
```


'''Testing'''
Note that the recursive method is ''much'' slower -- using the 2151 character palindrome by Dan Hoey [http://www2.vo.lu/homepages/phahn/anagrams/panama.htm here], we have:

```ruby
str = "A man, a plan, a caret, [...2110 chars deleted...] a canal--Panama.".downcase.delete('^a-z')
puts palindrome?(str)    # => true
puts r_palindrome?(str)  # => true

require 'benchmark'
Benchmark.bm do |b|
  b.report('iterative') {10000.times {palindrome?(str)}}
  b.report('recursive') {10000.times {r_palindrome?(str)}}
end
```


{{out}}

```txt
true
true
               user     system      total        real
iterative  0.062000   0.000000   0.062000 (  0.055000)
recursive 16.516000   0.000000  16.516000 ( 16.562000)
```



## Run BASIC


```runbasic
data "My dog has fleas", "Madam, I'm Adam.", "1 on 1", "In girum imus nocte et consumimur igni"

for i = 1 to 4
  read w$
  print w$;" is ";isPalindrome$(w$);" Palindrome"
next

FUNCTION isPalindrome$(str$)
for i = 1 to len(str$)
  a$ = upper$(mid$(str$,i,1))
   if (a$ >= "A" and a$ <= "Z") or (a$ >= "0" and a$ <= "9") then b$ = b$ + a$: c$ = a$ + c$
next i
if b$ <> c$ then isPalindrome$ = "not"
```

{{out}}

```txt
My dog has fleas is not Palindrome
Madam, I'm Adam. is  Palindrome
1 on 1 is not Palindrome
In girum imus nocte et consumimur igni is  Palindrome
```



## Rust


```rust

fn is_palindrome(string: &str) -> bool {
    let half_len = string.len()/2;
    string.chars().take(half_len).eq(string.chars().rev().take(half_len))
}

macro_rules! test {
    ( $( $x:tt ),* ) => { $( println!("'{}': {}", $x, is_palindrome($x)); )* };
}

fn main() {
    test!("",
          "a",
          "ada",
          "adad",
          "ingirumimusnocteetconsumimurigni",
          "人人為我,我為人人",
          "Я иду с мечем, судия",
          "아들딸들아",
          "The quick brown fox");
}

```

{{out}}

```txt

'': true
'a': true
'ada': true
'adad': false
'ingirumimusnocteetconsumimurigni': true
'人人為我,我為人人': true
'Я иду с мечем, судия': false
'아들딸들아': true
'The quick brown fox': false

```

The above soluion checks if the codepoints form a pallindrome, but it is perhaps more correct to consider if the graphemes form a pallindrome. This can be accomplished with an external library and a slight modification to <code>is_palindrome</code>.

```rust
extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;
fn is_palindrome(string: &str) -> bool {
    string.graphemes(true).eq(string.graphemes(true).rev())
}
```



## SAS

Description

```SAS

The macro "palindro" has two parameters: string and ignorewhitespace.
  string is the expression to be checked.
  ignorewhitespace, (Y/N), determines whether or not to ignore blanks and punctuation.
This macro was written in SAS 9.2.  If you use a version before SAS 9.1.3,
the compress function options will not work.

```

Code

```SAS

%MACRO palindro(string, ignorewhitespace);
  DATA _NULL_;
    %IF %UPCASE(&ignorewhitespace)=Y %THEN %DO;
/* The arguments of COMPRESS (sp) ignore blanks and puncutation */
/* We take the string and record it in reverse order using the REVERSE function. */
      %LET rev=%SYSFUNC(REVERSE(%SYSFUNC(COMPRESS(&string,,sp))));
      %LET string=%SYSFUNC(COMPRESS(&string.,,sp));
    %END;

    %ELSE %DO;
      %LET rev=%SYSFUNC(REVERSE(&string));
    %END;
    /*%PUT rev=&rev.;*/
    /*%PUT string=&string.;*/

/* Here we determine if the string and its reverse are the same. */
    %IF %UPCASE(&string)=%UPCASE(&rev.) %THEN %DO;
      %PUT TRUE;
    %END;
    %ELSE %DO;
      %PUT FALSE;
    %END;
  RUN;
%MEND;

```

Example macro call and output

```SAS

%palindro("a man, a plan, a canal: panama",y);

TRUE

NOTE: DATA statement used (Total process time):
      real time           0.00 seconds
      cpu time            0.00 seconds

%palindro("a man, a plan, a canal: panama",n);

FALSE

NOTE: DATA statement used (Total process time):
      real time           0.00 seconds
      cpu time            0.00 seconds

```



## Scala

{{libheader|Scala}}
=== Non-recursive, robustified===

```Scala
  def isPalindrome(s: String): Boolean = (s.size >= 2) && s == s.reverse
```


### Bonus: Detect and account for odd space and punctuation


```scala
  def isPalindromeSentence(s: String): Boolean =
    (s.size >= 2) && {
      val p = s.replaceAll("[^\\p{L}]", "").toLowerCase
      p == p.reverse
    }

```



### Recursive


```Scala
import scala.annotation.tailrec

  def isPalindromeRec(s: String) = {
    @tailrec
    def inner(s: String): Boolean =
      (s.length <= 1) || (s.head == s.last) && inner(s.tail.init)

    (s.size >= 2) && inner(s)
  }
```

'''Testing'''

```Scala
  // Testing
  assert(!isPalindrome(""))
  assert(!isPalindrome("z"))
  assert(isPalindrome("amanaplanacanalpanama"))
  assert(!isPalindrome("Test 1,2,3"))
  assert(isPalindrome("1 2 1"))
  assert(!isPalindrome("A man a plan a canal Panama."))

  assert(!isPalindromeSentence(""))
  assert(!isPalindromeSentence("z"))
  assert(isPalindromeSentence("amanaplanacanalpanama"))
  assert(!isPalindromeSentence("Test 1,2,3"))
  assert(isPalindromeSentence("1 2 1"))
  assert(isPalindromeSentence("A man a plan a canal Panama."))

  assert(!isPalindromeRec(""))
  assert(!isPalindromeRec("z"))
  assert(isPalindromeRec("amanaplanacanalpanama"))
  assert(!isPalindromeRec("Test 1,2,3"))
  assert(isPalindromeRec("1 2 1"))
  assert(!isPalindromeRec("A man a plan a canal Panama."))

  println("Successfully completed without errors.")
```



## Scheme

'''Non-recursive'''


```scheme
(define (palindrome? s)
  (let ((chars (string->list s)))
    (equal? chars (reverse chars))))
```


'''Recursive'''

```scheme
(define (palindrome? s)
  (let loop ((i 0)
             (j (- (string-length s) 1)))
    (or (>= i j)
        (and (char=? (string-ref s i) (string-ref s j))
             (loop (+ i 1) (- j 1))))))

;; Or:
(define (palindrome? s)
  (let loop ((s (string->list s))
             (r (reverse (string->list s))))
    (or (null? s)
        (and (char=? (car s) (car r))
             (loop (cdr s) (cdr r))))))

> (palindrome? "ingirumimusnocteetconsumimurigni")
#t
> (palindrome? "This is not a palindrome")
#f
>
```



## Seed7


```seed7
const func boolean: palindrome (in string: stri) is func
  result
    var boolean: isPalindrome is TRUE;
  local
    var integer: index is 0;
    var integer: length is 0;
  begin
    length := length(stri);
    for index range 1 to length div 2 do
      if stri[index] <> stri[length - index + 1] then
        isPalindrome := FALSE;
      end if;
    end for;
  end func;
```


For palindromes where spaces shuld be ignore use:

```seed7
palindrome(replace("in girum imus nocte et consumimur igni", " ", ""))
```



## SequenceL

'''Using the Reverse Library Function'''

```sequencel>import <Utilities/Sequence.sl
;

isPalindrome(string(1)) := equalList(string, reverse(string));
```


'''Version Using an Indexed Function'''

```sequencel
isPalindrome(string(1)) :=
	let
		compares[i] := string[i] = string[size(string) - (i - 1)] foreach i within 1 ... (size(string) / 2);
	in
		all(compares);
```



## Sidef


'''Built-in'''

```ruby
say "noon".is_palindrome;    # true
```


'''Non-recursive'''


```ruby
func palindrome(s) {
    s == s.reverse
}
```


'''Recursive'''


```ruby
func palindrome(s) {
    if (s.len <= 1) {
        true
    }
    elsif (s.first != s.last) {
        false
    }
    else {
        __FUNC__(s.ft(1, -2))
    }
}
```


## Simula


```simula
BEGIN

    BOOLEAN PROCEDURE ISPALINDROME(T); TEXT T;
    BEGIN
        BOOLEAN RESULT;
        INTEGER I, J;
        I := 1;
        J := T.LENGTH;
        RESULT := TRUE;
        WHILE RESULT AND I < J DO
        BEGIN
            CHARACTER L, R;
            T.SETPOS(I); L := T.GETCHAR; I := I + 1;
            T.SETPOS(J); R := T.GETCHAR; J := J - 1;
            RESULT := L = R;
        END;
        ISPALINDROME := RESULT;
    END ISPALINDROME;

    TEXT T;
    FOR T :- "", "A", "AA", "ABA", "SALALAS", "MADAMIMADAM",
             "AB", "AAB", "ABCBDA"
    DO
    BEGIN
        OUTTEXT(IF ISPALINDROME(T) THEN "IS   " ELSE "ISN'T");
        OUTTEXT(" PALINDROME: ");
        OUTCHAR('"');
        OUTTEXT(T);
        OUTCHAR('"');
        OUTIMAGE;
    END;

END.
```

{{out}}

```txt

IS    PALINDROME: ""
IS    PALINDROME: "A"
IS    PALINDROME: "AA"
IS    PALINDROME: "ABA"
IS    PALINDROME: "SALALAS"
IS    PALINDROME: "MADAMIMADAM"
ISN'T PALINDROME: "AB"
ISN'T PALINDROME: "AAB"
ISN'T PALINDROME: "ABCBDA"

```



## Slate

'''Non-Recursive'''

```slate
s@(String traits) isPalindrome
[
  (s lexicographicallyCompare: s reversed) isZero
].
```


'''Recursive'''
Defined on Sequence since we are not using String-specific methods:

```slate
s@(Sequence traits) isPalindrome
[
  s isEmpty
    ifTrue: [True]
    ifFalse: [(s first = s last) /\ [(s sliceFrom: 1 to: s indexLast - 1) isPalindrome]]
].
```


'''Testing'''

```slate
define: #p -> 'ingirumimusnocteetconsumimurigni'.
inform: 'sequence ' ; p ; ' is ' ; (p isPalindrome ifTrue: [''] ifFalse: ['not ']) ; 'a palindrome.'.
```



## Smalltalk


{{works with|Squeak}}

```smalltalk
isPalindrome := [:aString |
	str := (aString select: [:chr| chr isAlphaNumeric]) collect: [:chr | chr asLowercase].
	str = str reversed.
	].

```


{{works with|GNU Smalltalk}}

```smalltalk
String extend [
  palindro [                  "Non-recursive"
    ^ self = (self reverse)
  ]
  palindroR [                 "Recursive"
    (self size) <= 1 ifTrue: [ ^true ]
      ifFalse: [ |o i f| o := self asOrderedCollection.
          i := o removeFirst.
          f := o removeLast.
          i = f ifTrue: [ ^ (o asString) palindroR ]
                ifFalse: [ ^false ]
      ]
  ]
].
```


'''Testing'''


```smalltalk
('hello' palindro) printNl.
('hello' palindroR) printNl.
('ingirumimusnocteetconsumimurigni' palindro) printNl.
('ingirumimusnocteetconsumimurigni' palindroR) printNl.
```


{{works with|VisualWorks Pharo Squeak}}

```smalltalk>SequenceableCollection>
isPalindrome
	^self reverse = self

```



## SNOBOL4



```SNOBOL4
        define('pal(str)') :(pal_end)
pal     str notany(&ucase &lcase) = :s(pal)
        str = replace(str,&ucase,&lcase)
        leq(str,reverse(str)) :s(return)f(freturn)
pal_end

        define('palchk(str)tf') :(palchk_end)
palchk  output = str;
        tf = 'False'; tf = pal(str) 'True'
        output = 'Palindrome: ' tf :(return)
palchk_end

*       # Test and display
        palchk('Able was I ere I saw Elba')
        palchk('In girum imus nocte et consumimur igni')
        palchk('The quick brown fox jumped over the lazy dogs')
end
```


{{out}}

```txt
Able was I ere I saw Elba
Palindrome: True
In girum imus nocte et consumimur igni
Palindrome: True
The quick brown fox jumped over the lazy dogs
Palindrome: False
```



## SQL


```sql
SET @txt = REPLACE('In girum imus nocte et consumimur igni', ' ', '');
SELECT REVERSE(@txt) = @txt;
```


## Swift

{{works with|Swift|1.2}}

```Swift
import Foundation

// Allow for easy character checking
extension String {
    subscript (i: Int) -> String {
        return String(Array(self)[i])
    }
}

func isPalindrome(str:String) -> Bool {
    if (count(str) == 0 || count(str) == 1) {
        return true
    }
    let removeRange = Range<String.Index>(start: advance(str.startIndex, 1), end: advance(str.endIndex, -1))
    if (str[0] == str[count(str) - 1]) {
        return isPalindrome(str.substringWithRange(removeRange))
    }
    return false
}
```


{{works with|Swift|2.0}}

```swift
func isPal(str: String) -> Bool {
  let c = str.characters
  return lazy(c).reverse()
    .startsWith(c[c.startIndex...advance(c.startIndex, c.count / 2)])
}
```



## Tcl


'''Non-recursive'''


```tcl
package require Tcl 8.5
proc palindrome {s} {
    return [expr {$s eq [string reverse $s]}]
}
```


'''Recursive'''


```tcl
proc palindrome_r {s} {
    if {[string length $s] <= 1} {
        return true
    } elseif {[string index $s 0] ne [string index $s end]} {
        return false
    } else {
        return [palindrome_r [string range $s 1 end-1]]
    }
}
```


'''Testing'''


```tcl
set p ingirumimusnocteetconsumimurigni
puts "'$p' is palindrome? [palindrome $p]"
puts "'$p' is palindrome? [palindrome_r $p]"
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
pal  ="ingirumimusnocteetconsumimurigni"
pal_r=TURN(pal)
SELECT pal
CASE $pal_r
PRINT "true"
DEFAULT
PRINT/ERROR "untrue"
ENDSELECT

```

{{out}}

```txt

true

```



## TypeScript


```javascript
const detectNonLetterRegexp=/[^A-ZÀ-ÞЀ-Я]/g;

function stripDiacritics(phrase:string){
    return phrase.normalize('NFD').replace(/[\u0300-\u036f]/g, "")
}

function isPalindrome(phrase:string){
    const TheLetters = stripDiacritics(phrase.toLocaleUpperCase().replace(detectNonLetterRegexp, ''));
    const middlePosition = TheLetters.length/2;
    const leftHalf = TheLetters.substr(0, middlePosition);
    const rightReverseHalf = TheLetters.substr(-middlePosition).split('').reverse().join('');
    return leftHalf == rightReverseHalf;
}

console.log(isPalindrome('Sueño que esto no es un palíndromo'))
console.log(isPalindrome('Dábale arroz a la zorra el abad!'))
console.log(isPalindrome('Я иду с мечем судия'))

```



## UNIX Shell


```bash
if [[ "${text}" == "$(rev <<< "${text}")" ]]; then
   echo "Palindrome"
else
   echo "Not a palindrome"
fi
```



## Ursala


The algorithm is to convert to lower case, and then compare
the intersection of the argument and the set of letters
(declared in the standard library) with its reversal.
This is done using the built in operator suffixes
for intersection (c), identity (i), reversal (x) and equality (E).

```Ursala
#import std

palindrome = ~&cixE\letters+ * -:~& ~=`A-~rlp letters
```

This test programs applies the function to each member of a list of three strings,
of which only the first two are palindromes.

```Ursala
#cast %bL

examples = palindrome* <'abccba','foo ba rra bo of','notone'>
```

{{out}}

```txt
<true,true,false>
```



## Vala

Checks if a word is a palindrome ignoring the case and spaces.

```vala
bool is_palindrome (string str) {
    var tmp = str.casefold ().replace (" ", "");
    return tmp == tmp.reverse ();
}

int main (string[] args) {
    print (is_palindrome (args[1]).to_string () + "\n");
    return 0;
}
```



## VBA

This function uses function Reverse() (or Rreverse()) from [[Reverse a string]],
after first stripping spaces from the string using the built-in function Replace
and converting it to lower case. It can't handle punctuation (yet). Just like the VBScript
version it could also work using StrReverse.


```VBA

Public Function isPalindrome(aString as string) as Boolean
dim tempstring as string
  tempstring = Lcase(Replace(aString, " ", ""))
  isPalindrome = (tempstring = Reverse(tempstring))
End Function

```


{{out|Example}}

```txt

print isPalindrome("In girum imus nocte et consumimur igni")
True

```



## VBScript


### =Implementation=


```vb
function Squish( s1 )
	dim sRes
	sRes = vbNullString
	dim i, c
	for i = 1 to len( s1 )
		c = lcase( mid( s1, i, 1 ))
		if instr( "abcdefghijklmnopqrstuvwxyz0123456789", c ) then
			sRes = sRes & c
		end if
	next
	Squish = sRes
end function

function isPalindrome( s1 )
	dim squished
	squished = Squish( s1 )
	isPalindrome = ( squished = StrReverse( squished ) )
end function
```



### =Invocation=


```vb
wscript.echo isPalindrome( "My dog has fleas")
wscript.echo isPalindrome( "Madam, I'm Adam.")
wscript.echo isPalindrome( "1 on 1")
wscript.echo isPalindrome( "In girum imus nocte et consumimur igni")
```

{{out}}

```txt
0
-1
0
-1
```



## Vedit macro language

This routine checks if current line is a palindrome:


```vedit
:PALINDROME:
EOL #2 = Cur_Col-2
BOL
for (#1 = 0; #1 <= #2/2; #1++) {
    if (CC(#1) != CC(#2-#1)) { Return(0) }
}
Return(1)
```


Testing:


```vedit
Call("PALINDROME")
if (Return_Value) {
    Statline_Message("Yes")
} else {
    Statline_Message("No")
}
Return
```



## Visual Basic .NET

{{trans|VBA}}

```vbnet
Module Module1

    Function IsPalindrome(p As String) As Boolean
        Dim temp = p.ToLower().Replace(" ", "")
        Return StrReverse(temp) = temp
    End Function

    Sub Main()
        Console.WriteLine(IsPalindrome("In girum imus nocte et consumimur igni"))
    End Sub

End Module
```

{{out}}

```txt
True
```



## Wortel


```wortel
@let {
  ; Using a hook
  pal1 @(= @rev)
  ; Function with argument
  pal2 &s = s @rev s
  ; for inexact palindromes
  pal3 ^(@(= @rev) .toLowerCase. &\@replace[&"\s+"g ""])
  [[
    !pal1 "abcba"
    !pal2 "abcbac"
    !pal3 "In girum imus nocte et consumimur igni"
  ]]
}
```

Returns:
```txt
[true false true]
```



## X86 Assembly


```x86asm

; x86_84 Linux nasm
section .text

isPalindrome:
  mov rsi, rax
  mov rdi, rax

  get_end:
    cmp byte [rsi], 0
    je get_result
    inc rsi
    jmp get_end

  get_result:
    mov rax, 0
    dec rsi

    compare:
      mov cl, byte [rdi]
      cmp byte [rsi], cl
      jne not_palindrome
      cmp rsi, rdi
      je palindrome
      inc rdi
      cmp rdi, rsi
      je palindrome
      dec rsi
      jmp compare

  not_palindrome:
    mov rax, 0
    ret
  palindrome:
    mov rax, 1
    ret

```



## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
string 0;                       \use zero-terminated strings

func StrLen(Str);               \Return number of characters in an ASCIIZ string
char Str;
int  I;
for I:= 0 to -1>>1-1 do
        if Str(I) = 0 then return I;

func Palindrome(S);             \Return 'true' if S is a palindrome
char S;
int  L, I;
[L:= StrLen(S);
for I:= 0 to L/2-1 do
        if S(I) # S(L-1-I) then return false;
return true;
]; \Palindrome

int Word, I;
[Word:=
    ["otto", "mary", "ablewasiereisawelba", "ingirumimusnocteetconsumimurigni"];
for I:= 0 to 4-1 do
    [Text(0, if Palindrome(Word(I)) then "yes" else "no");
    CrLf(0);
    ];
]
```


{{out}}

```txt

yes
no
yes
yes

```



## Yorick

Function ''is_palindrome'' meets the task description.
Function ''prep_palindrome'' demonstrates how to convert
an English sentence into a form that can be tested with
is_palindrome (by changing case and stripping non-alphabetical characters).


```yorick
func is_palindrome(str) {
    s = strchar(str)(:-1);
    return allof(s == s(::-1));
}

func prep_palindrome(str) {
    s = strchar(strlower(str));
    w = where(s >= 'a' & s <= 'z');
    return strchar(s(w));
}
```



## zkl


```zkl
fcn pali(text){
   if (text.len()<2) return(False);
   text==text.reverse();
}
fcn pali2(text){ pali((text - " \t\n.,").toLower()) }  // or whatever punctuation is
```

{{out}}

```txt

pali("red rum sir is murder") //--> False
pali("red rum sir is murder" - " ") //-->True, remove spaces
pali2("In girum imus nocte et consumimur igni") //-->True

```

