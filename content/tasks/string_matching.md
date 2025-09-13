+++
title = "String matching"
description = ""
date = 2019-10-19T05:54:56Z
aliases = []
[extra]
id = 8493
[taxonomies]
categories = ["task", "String manipulation"]
tags = []
+++

## Task

Given two strings, demonstrate the following three types of string matching:

::#   Determining if the first string starts with second string
::#   Determining if the first string contains the second string at any location
::#   Determining if the first string ends with the second string



Optional requirements:
::#   Print the location of the match for part 2
::#   Handle multiple occurrences of a string for part 2.





## 360 Assembly


```360asm
*        String matching           04/04/2017
STRMATCH CSECT
         USING  STRMATCH,R15
         XPRNT  SS,L'SS
*
         CLC    SS(L'S1),S1
         BNE    NOT1
         XPRNT  =C'-- STARTS WITH',14
         XPRNT  S1,L'S1
NOT1     EQU    *
*
         CLC    SS+L'SS-L'S2(L'S2),S2
         BNE    NOT2
         XPRNT  =C'-- ENDS WITH',12
         XPRNT  S2,L'S2
NOT2     EQU    *
*
         LA     R0,L'SS-L'S3+1
         LA     R1,SS
LOOP     CLC    0(L'S3,R1),S3
         BNE    NOT3
         XPRNT  =C'-- CONTAINS',11
         XPRNT  S3,L'S3
NOT3     LA     R1,1(R1)
         BCT    R0,LOOP
*
         BR     R14
SS       DC     CL6'ABCDEF'
S1       DC     CL2'AB'
S2       DC     CL2'EF'
S3       DC     CL2'CD'
PG       DC     CL80' '
         YREGS
         END    STRMATCH
```

```txt

ABCDEF
-- STARTS WITH
AB
-- ENDS WITH
EF
-- CONTAINS
CD

```




## Ada


```Ada

with Ada.Strings.Fixed;  use Ada.Strings.Fixed;
with Ada.Text_IO;        use Ada.Text_IO;

procedure Match_Strings is
   S1 : constant String := "abcd";
   S2 : constant String := "abab";
   S3 : constant String := "ab";
begin
   if S1'Length >= S3'Length and then S1 (S1'First..S1'First + S3'Length - 1) = S3 then
      Put_Line (''' & S1 & "' starts with '" & S3 & ''');
   end if;
   if S2'Length >= S3'Length and then S2 (S2'Last - S3'Length + 1..S2'Last) = S3 then
      Put_Line (''' & S2 & "' ends with '" & S3 & ''');
   end if;
   Put_Line (''' & S3 & "' first appears in '" & S1 & "' at" & Integer'Image (Index (S1, S3)));
   Put_Line
   (  ''' & S3 & "' appears in '" & S2 & ''' &
      Integer'Image (Ada.Strings.Fixed.Count (S2, S3)) & " times"
   );
end Match_Strings;

```

```txt

'abcd' starts with 'ab'
'abab' ends with 'ab'
'ab' first appears in 'abcd' at 1
'ab' appears in 'abab' 2 times

```



## Aime


```aime
text t;
data b;

b = "Bangkok";

t = "Bang";

o_form("starts with, embeds, ends with \"~\": ~, ~, ~\n", t, b.seek(t) == 0,
       b.seek(t) != -1,
       b.seek(t) != -1 && b.seek(t) + ~t == ~b);

t = "ok";

o_form("starts with, embeds, ends with \"~\": ~, ~, ~\n", t, b.seek(t) == 0,
       b.seek(t) != -1,
       b.seek(t) != -1 && b.seek(t) + ~t == ~b);

t = "Summer";

o_form("starts with, embeds, ends with \"~\": ~, ~, ~\n", t, b.seek(t) == 0,
       b.seek(t) != -1,
       b.seek(t) != -1 && b.seek(t) + ~t == ~b);
```

```txt
starts with, embeds, ends with "Bang": 1, 1, 0
starts with, embeds, ends with "ok": 0, 1, 1
starts with, embeds, ends with "Summer": 0, 0, 0
```



## ALGOL 68

```algol68
# define some appropriate OPerators #
PRIO STARTSWITH = 5, ENDSWITH = 5;
OP STARTSWITH = (STRING str, prefix)BOOL: # assuming LWB = 1 #
  IF UPB str < UPB prefix THEN FALSE ELSE str[:UPB prefix]=prefix FI;
OP ENDSWITH = (STRING str, suffix)BOOL: # assuming LWB = 1 #
  IF UPB str < UPB suffix THEN FALSE ELSE str[UPB str-UPB suffix+1:]=suffix FI;

INT loc, loc2;

print((
  "abcd" STARTSWITH "ab", # returns TRUE #
  "abcd" ENDSWITH "zn", # returns FALSE #
  string in string("bb",loc,"abab"), # returns FALSE #
  string in string("ab",loc,"abab"), # returns TRUE #
  (string in string("bb",loc,"abab")|loc|-1), # returns -1 #
  (string in string("ab",loc,"abab")|loc|-1), # returns +1 #
  (string in string("ab",loc2,"abab"[loc+1:])|loc+loc2|-1) # returns +3 #
))
```

```txt

TFFT         -1         +1         +3

```



## AppleScript


```AppleScript
set stringA to "I felt happy because I saw the others were happy and because I knew I should feel happy, but I wasn’t really happy."

set string1 to "I felt happy"
set string2 to "I should feel happy"
set string3 to "I wasn't really happy"

-- Determining if the first string starts with second string
stringA starts with string1  --> true

-- Determining if the first string contains the second string at any location
stringA contains string2     --> true

-- Determining if the first string ends with the second string
stringA ends with string3    --> false

-- Print the location of the match for part 2
offset of string2 in stringA --> 69
```

AppleScript doesn't have a builtin means of matching multiple occurrences of a substring, however one can redefine the existing '''offset''' command to add this functionality:

```AppleScript
-- Handle multiple occurrences of a string for part 2
on offset of needle in haystack
	local needle, haystack

	if the needle is not in the haystack then return {}
	set my text item delimiters to the needle
	script
		property N : needle's length
		property t : {1 - N} & haystack's text items
	end script

	tell the result
		repeat with i from 2 to (its t's length) - 1
			set x to item i of its t
			set y to item (i - 1) of its t
			set item i of its t to (its N) + (x's length) + y
		end repeat

		items 2 thru -2 of its t
	end tell
end offset

offset of "happy" in stringA --> {8, 44, 83, 110}
```



or, defining an '''offsets''' function in terms of a more general '''findIndices''':

```applescript
-- offsets :: String -> String -> [Int]
on offsets(needle, haystack)
    script match
        property mx : length of haystack
        property d : (length of needle) - 1
        on |λ|(x, i, xs)
            set z to d + i
            mx ≥ z and needle = text i thru z of xs
        end |λ|
    end script

    findIndices(match, haystack)
end offsets


-- TEST ---------------------------------------------------
on run
    set txt to "I felt happy because I saw the others " & ¬
        "were happy and because I knew I should " & ¬
        "feel happy, but I wasn’t really happy."

    offsets("happy", txt)

    --> {8, 44, 83, 110}
end run


-- GENERIC -------------------------------------------------

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    set lng to length of xs
    set acc to {}
    tell mReturn(f)
        repeat with i from 1 to lng
            set acc to acc & (|λ|(item i of xs, i, xs))
        end repeat
    end tell
    return acc
end concatMap


-- findIndices :: (a -> Bool) -> [a] -> [Int]
-- findIndices :: (String -> Bool) -> String -> [Int]
on findIndices(p, xs)
    script go
        property f : mReturn(p)
        on |λ|(x, i, xs)
            if f's |λ|(x, i, xs) then
                {i}
            else
                {}
            end if
        end |λ|
    end script
    concatMap(go, xs)
end findIndices


-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if script is class of f then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn
```

```txt
{8, 44, 83, 110}
```


## ARM Assembly

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program strMatching.s   */

/* Constantes    */
.equ STDOUT, 1                           @ Linux output console
.equ EXIT,   1                           @ Linux syscall
.equ WRITE,  4                           @ Linux syscall

/* Initialized data */
.data
szMessFound:             .asciz "String found. \n"
szMessNotFound:          .asciz "String not found. \n"
szString:                .asciz "abcdefghijklmnopqrstuvwxyz"
szString2:               .asciz "abc"
szStringStart:           .asciz "abcd"
szStringEnd:             .asciz "xyz"
szStringStart2:          .asciz "abcd"
szStringEnd2:            .asciz "xabc"
szCarriageReturn:        .asciz "\n"

/* UnInitialized data */
.bss

/*  code section */
.text
.global main
main:

    ldr r0,iAdrszString                         @ address input string
    ldr r1,iAdrszStringStart                    @ address search string

    bl searchStringDeb                          @ Determining if the first string starts with second string
    cmp r0,#0
    ble 1f
    ldr r0,iAdrszMessFound                      @ display message
    bl affichageMess
    b 2f
1:
    ldr r0,iAdrszMessNotFound
    bl affichageMess
2:
    ldr r0,iAdrszString                         @ address input string
    ldr r1,iAdrszStringEnd                      @ address search string
    bl searchStringFin                          @ Determining if the first string ends with the second string
    cmp r0,#0
    ble 3f
    ldr r0,iAdrszMessFound                      @ display message
    bl affichageMess
    b 4f
3:
    ldr r0,iAdrszMessNotFound
    bl affichageMess
4:
    ldr r0,iAdrszString2                        @ address input string
    ldr r1,iAdrszStringStart2                   @ address search string

    bl searchStringDeb                          @
    cmp r0,#0
    ble 5f
    ldr r0,iAdrszMessFound                      @ display message
    bl affichageMess
    b 6f
5:
    ldr r0,iAdrszMessNotFound
    bl affichageMess
6:
    ldr r0,iAdrszString2                        @ address input string
    ldr r1,iAdrszStringEnd2                     @ address search string
    bl searchStringFin
    cmp r0,#0
    ble 7f
    ldr r0,iAdrszMessFound                      @ display message
    bl affichageMess
    b 8f
7:
    ldr r0,iAdrszMessNotFound
    bl affichageMess
8:
    ldr r0,iAdrszString                         @ address input string
    ldr r1,iAdrszStringEnd                      @ address search string
    bl searchSubString                          @ Determining if the first string contains the second string at any location
    cmp r0,#0
    ble 9f
    ldr r0,iAdrszMessFound                      @ display message
    bl affichageMess
    b 10f
9:
    ldr r0,iAdrszMessNotFound                   @ display substring result
    bl affichageMess
10:

100:                                            @ standard end of the program
    mov r0, #0                                  @ return code
    mov r7, #EXIT                               @ request to exit program
    svc 0                                       @ perform system call
iAdrszMessFound:          .int szMessFound
iAdrszMessNotFound:       .int szMessNotFound
iAdrszString:             .int szString
iAdrszString2:            .int szString2
iAdrszStringStart:        .int szStringStart
iAdrszStringEnd:          .int szStringEnd
iAdrszStringStart2:       .int szStringStart2
iAdrszStringEnd2:         .int szStringEnd2
iAdrszCarriageReturn:     .int szCarriageReturn
/******************************************************************/
/*     search substring at begin of input string                  */
/******************************************************************/
/* r0 contains the address of the input string */
/* r1 contains the address of substring */
/* r0 returns 1 if find or 0 if not or -1 if error */
searchStringDeb:
    push {r1-r4,lr}                             @ save  registers
    mov r3,#0                                   @ counter byte  string
    ldrb r4,[r1,r3]                             @ load first byte of substring
    cmp r4,#0                                   @ empty string ?
    moveq r0,#-1                                @ error
    beq 100f
1:
    ldrb r2,[r0,r3]                             @ load byte string input
    cmp r2,#0                                   @ zero final ?
    moveq r0,#0                                 @ not find
    beq 100f
    cmp r4,r2                                   @ bytes equals ?
    movne r0,#0                                 @ no not find
    bne 100f
    add r3,#1                                   @ increment counter
    ldrb r4,[r1,r3]                             @ and load next byte of substring
    cmp r4,#0                                   @ zero final ?
    bne 1b                                      @ no -> loop
    mov r0,#1                                   @ yes is ok
100:
    pop {r1-r4,lr}                              @ restaur registers
    bx lr                                       @ return

/******************************************************************/
/*     search substring at end of input string                    */
/******************************************************************/
/* r0 contains the address of the input string */
/* r1 contains the address of substring */
/* r0 returns 1 if find or 0 if not or -1 if error */
searchStringFin:
    push {r1-r5,lr}                             @ save  registers
    mov r3,#0                                   @ counter byte  string
                                                @ search the last character of substring
1:
    ldrb r4,[r1,r3]                             @ load byte of substring
    cmp r4,#0                                   @ zero final ?
    addne r3,#1                                 @ no increment counter
    bne 1b                                      @ and loop
    cmp r3,#0                                   @ empty string ?
    moveq r0,#-1                                @ error
    beq 100f
    sub r3,#1                                   @ index of last byte
    ldrb r4,[r1,r3]                             @ load last byte of substring
                                                @ search the last character of string
    mov r2,#0                                   @ index last character
2:
    ldrb r5,[r0,r2]                             @ load first byte of substring
    cmp r5,#0                                   @ zero final ?
    addne r2,#1                                 @ no -> increment counter
    bne 2b                                      @ and loop
    cmp r2,#0                                   @ empty input string ?
    moveq r0,#0                                 @ yes -> not found
    beq 100f
    sub r2,#1                                   @ index last character
3:
    ldrb r5,[r0,r2]                             @ load byte string input
    cmp r4,r5                                   @ bytes equals ?
    movne r0,#0                                 @ no -> not found
    bne 100f
    subs r3,#1                                  @ decrement counter
    movlt r0,#1                                 @ if zero -> ok found
    blt 100f
    subs r2,#1                                  @ decrement counter input string
    movlt r0,#0                                 @ if zero -> not found
    blt 100f
    ldrb r4,[r1,r3]                             @ load previous byte of substring
    b 3b                                        @ and loop

100:
    pop {r1-r5,lr}                              @ restaur registers
    bx lr                                       @ return

/******************************************************************/
/*   search a substring in the string                            */
/******************************************************************/
/* r0 contains the address of the input string */
/* r1 contains the address of substring */
/* r0 returns index of substring in string or -1 if not found */
searchSubString:
    push {r1-r6,lr}                       @ save registers
    mov r2,#0                             @ counter byte input string
    mov r3,#0                             @ counter byte string
    mov r6,#-1                            @ index found
    ldrb r4,[r1,r3]
1:
    ldrb r5,[r0,r2]                       @ load byte string
    cmp r5,#0                             @ zero final ?
    moveq r0,#-1                          @ yes returns error
    beq 100f
    cmp r5,r4                             @ compare character
    beq 2f
    mov r6,#-1                            @ no equals - > raz index
    mov r3,#0                             @ and raz counter byte
    add r2,#1                             @ and increment counter byte
    b 1b                                  @ and loop
2:                                        @ characters equals
    cmp r6,#-1                            @ first characters equals ?
    moveq r6,r2                           @ yes -> index begin in r6
    add r3,#1                             @ increment counter substring
    ldrb r4,[r1,r3]                       @ and load next byte
    cmp r4,#0                             @ zero final ?
    beq 3f                                @ yes -> end search
    add r2,#1                             @ else increment counter string
    b 1b                                  @ and loop
3:
    mov r0,r6
100:
    pop {r1-r6,lr}                        @ restaur registers
    bx lr

/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                       @ save  registers
    mov r2,#0                                   @ counter length */
1:                                              @ loop length calculation
    ldrb r1,[r0,r2]                             @ read octet start position + index
    cmp r1,#0                                   @ if 0 its over
    addne r2,r2,#1                              @ else add 1 in the length
    bne 1b                                      @ and loop
                                                @ so here r2 contains the length of the message
    mov r1,r0                                   @ address message in r1
    mov r0,#STDOUT                              @ code to write to the standard output Linux
    mov r7, #WRITE                              @ code call system "write"
    svc #0                                      @ call system
    pop {r0,r1,r2,r7,lr}                        @ restaur registers
    bx lr                                       @ return


```



## Arturo



```arturo
print "'abcd' starts with 'ab' = " + $(startsWith "abcd" "ab")

print "'abcd' contains 'bc' = " + $(contains "abcd" "bc")
print "'bc' found in 'abcd' at location = " + $(find "abcd" "bc")

print "'abcd' ends with 'zn' = " + $(endsWith "abcd" "zn")
```


```txt
'abcd' starts with 'ab' = true
'abcd' contains 'bc' = true
'bc' found in 'abcd' at location = 1
'abcd' ends with 'zn' = false
```



## AutoHotkey


```AutoHotkey

String1 = abcd
String2 = abab

If (SubStr(String1,1,StrLen(String2)) = String2)
 MsgBox, "%String1%" starts with "%String2%".
IfInString, String1, %String2%
{
 Position := InStr(String1,String2)
 StringReplace, String1, String1, %String2%, %String2%, UseErrorLevel
 MsgBox, "%String1%" contains "%String2%" at position %Position%`, and appears %ErrorLevel% times.
}
StringRight, TempVar, String1, StrLen(String2)
If TempVar = %String2%
 MsgBox, "%String1%" ends with "%String2%".

```



## AutoIt


```AutoIt
$string1 = "arduinoardblobard"
$string2 = "ard"

; == Determining if the first string starts with second string
If StringLeft($string1, StringLen($string2)) = $string2 Then
	ConsoleWrite("1st string starts with 2nd string." & @CRLF)
Else
	ConsoleWrite("1st string does'nt starts with 2nd string." & @CRLF)
EndIf

; == Determining if the first string contains the second string at any location
; == Print the location of the match for part 2
; == Handle multiple occurrences of a string for part 2
$start = 1
$count = 0
$pos = StringInStr($string1, $string2)
While $pos
	$count += 1
	ConsoleWrite("1st string contains 2nd string at position: " & $pos & @CRLF)
	$pos = StringInStr($string1, $string2, 0, 1, $start + $pos + StringLen($string2))
WEnd
If $count = 0 Then ConsoleWrite("1st string does'nt contain 2nd string." & @CRLF)

; == Determining if the first string ends with the second string
If StringRight($string1, StringLen($string2)) = $string2 Then
	ConsoleWrite("1st string ends with 2nd string." & @CRLF)
Else
	ConsoleWrite("1st string does'nt ends with 2nd string." & @CRLF)
EndIf


```



## AWK


```AWK
#!/usr/bin/awk -f
{
    if ($1 ~ "^"$2) {
	print $1" begins with "$2;
    } else {
	print $1" does not begin with "$2;
    }

    if ($1 ~ $2) {
	print $1" contains "$2;
    } else {
	print $1" does not contain "$2;
    }

    if ($1 ~ $2"$") {
	print $1" ends with "$2;
    } else {
	print $1" does not end with "$2;
    }
}

```




## BASIC

```qbasic
first$ = "qwertyuiop"

'Determining if the first string starts with second string
second$ = "qwerty"
IF LEFT$(first$, LEN(second$)) = second$ THEN
    PRINT "'"; first$; "' starts with '"; second$; "'"
ELSE
    PRINT "'"; first$; "' does not start with '"; second$; "'"
END IF

'Determining if the first string contains the second string at any location
'Print the location of the match for part 2
second$ = "wert"
x = INSTR(first$, second$)
IF x THEN
    PRINT "'"; first$; "' contains '"; second$; "' at position "; x
ELSE
    PRINT "'"; first$; "' does not contain '"; second$; "'"
END IF

' Determining if the first string ends with the second string
second$ = "random garbage"
IF RIGHT$(first$, LEN(second$)) = second$ THEN
    PRINT "'"; first$; "' ends with '"; second$; "'"
ELSE
    PRINT "'"; first$; "' does not end with '"; second$; "'"
END IF


```


 'qwertyuiop' starts with 'qwerty'
 'qwertyuiop' contains 'wert' at position  2
 'qwertyuiop' does not end with 'random garbage'

=
## Applesoft BASIC
=

```ApplesoftBasic
10 A$ = "THIS, THAT, AND THE OTHER THING"
20 S$ = "TH"
30 DEF FN S(P) = MID$(A$, P, LEN(S$)) = S$
40 PRINT A$ : PRINT

110 S$(1) = "STARTS"
120 S$(0) = "DOES NOT START"
130 PRINT S$(FN S(1))" WITH "S$ : PRINT

210 R$ = "" : FOR I = 1 TO LEN(A$) - LEN(S$) : IF FN S(I) THEN R$ = R$ + STR$(I) + " "
220 NEXT I
230 IF LEN(R$) = 0 THEN PRINT "DOES NOT CONTAIN "S$
240 IF LEN(R$) THEN PRINT "CONTAINS "S$" LOCATED AT POSITION "R$
250 PRINT

310 E$(1) = "ENDS"
320 E$(0) =  "DOES NOT END"
330 PRINT E$(FN S(LEN(A$) - LEN(S$) + 1))" WITH "S$
```



## Batch File


```dos
::NOTE #1: This implementation might crash, or might not work properly if
::you put some of the CMD special characters (ex. %,!, etc) inside the strings.
::
::NOTE #2: The comparisons here are case-SENSITIVE.
::NOTE #3: Spaces in strings are considered.

@echo off
setlocal enabledelayedexpansion

::The main things...
set "str1=qwertyuiop"
set "str2=qwerty"
call :str2_lngth
call :matchbegin

set "str1=qweiuoiocghiioyiocxiisfguiioiuygvd"
set "str2=io"
call :str2_lngth
call :matchcontain

set "str1=blablabla"
set "str2=bbla"
call :str2_lngth
call :matchend

echo.
pause
exit /b 0
::/The main things.

::The functions...
:matchbegin
echo.
if "!str1:~0,%length%!"=="!str2!" (
	echo "%str1%" begins with "%str2%".
) else (
	echo "%str1%" does not begin with "%str2%".
)
goto :EOF

:matchcontain
echo.
set curr=0&set exist=0
:scanchrloop
if "!str1:~%curr%,%length%!"=="" (
	if !exist!==0 echo "%str1%" does not contain "%str2%".
	goto :EOF
)
if "!str1:~%curr%,%length%!"=="!str2!" (
	echo "%str1%" contains "%str2%". ^(in Position %curr%^)
	set exist=1
)
set /a curr+=1&goto scanchrloop

:matchend
echo.
if "!str1:~-%length%!"=="!str2!" (
	echo "%str1%" ends with "%str2%".
) else (
	echo "%str1%" does not end with "%str2%".
)
goto :EOF

:str2_lngth
set length=0
:loop
if "!str2:~%length%,1!"=="" goto :EOF
set /a length+=1
goto loop
::/The functions.
```

```txt
"qwertyuiop" begins with "qwerty".

"qweiuoiocghiioyiocxiisfguiioiuygvd" contains "io". (in Position 6)
"qweiuoiocghiioyiocxiisfguiioiuygvd" contains "io". (in Position 12)
"qweiuoiocghiioyiocxiisfguiioiuygvd" contains "io". (in Position 15)
"qweiuoiocghiioyiocxiisfguiioiuygvd" contains "io". (in Position 26)

"blablabla" does not end with "bbla".

Press any key to continue . . .
```




## BBC BASIC


```bbcbasic
      first$ = "The fox jumps over the dog"

      FOR test% = 1 TO 3
        READ second$
        starts% = FN_first_starts_with_second(first$, second$)
        IF starts% PRINT """" first$ """ starts with """ second$ """"
        ends% = FN_first_ends_with_second(first$, second$)
        IF ends% PRINT """" first$ """ ends with """ second$ """"
        where% = FN_first_contains_second_where(first$, second$)
        IF where% PRINT """" first$ """ contains """ second$ """ at position " ; where%
        howmany% = FN_first_contains_second_howmany(first$, second$)
        IF howmany% PRINT """" first$ """ contains """ second$ """ " ; howmany% " time(s)"
      NEXT
      DATA "The", "he", "dog"
      END

      DEF FN_first_starts_with_second(A$, B$)
      = B$ = LEFT$(A$, LEN(B$))

      DEF FN_first_ends_with_second(A$, B$)
      = B$ = RIGHT$(A$, LEN(B$))

      DEF FN_first_contains_second_where(A$, B$)
      = INSTR(A$, B$)

      DEF FN_first_contains_second_howmany(A$, B$)
      LOCAL I%, N% : I% = 0
      REPEAT
        I% = INSTR(A$, B$, I%+1)
        IF I% THEN N% += 1
      UNTIL I% = 0
      = N%

```

```txt
"The fox jumps over the dog" starts with "The"
"The fox jumps over the dog" contains "The" at position 1
"The fox jumps over the dog" contains "The" 1 time(s)
"The fox jumps over the dog" contains "he" at position 2
"The fox jumps over the dog" contains "he" 2 time(s)
"The fox jumps over the dog" ends with "dog"
"The fox jumps over the dog" contains "dog" at position 24
"The fox jumps over the dog" contains "dog" 1 time(s)
```



## Bracmat

Bracmat does pattern matching in expressions <code><i>subject</i>:<i>pattern</i></code> and in strings <code>@(<i>subject</i>:<i>pattern</i>)</code>. The (sub)pattern <code>?</code> is a wild card.

```Bracmat
( (sentence="I want a number such that that number will be even.")
& out$(@(!sentence:I ?) & "sentence starts with 'I'" | "sentence does not start with 'I'")
& out$(@(!sentence:? such ?) & "sentence contains 'such'" | "sentence does not contain 'such'")
& out$(@(!sentence:? "even.") & "sentence ends with 'even.'" | "sentence does not end with 'even.'")
& 0:?N
& ( @(!sentence:? be (? & !N+1:?N & ~))
  | out$str$("sentence contains " !N " occurrences of 'be'")
  )
)
```

In the last line, Bracmat is forced by the always failing node <code>~</code> to backtrack until all occurrences of 'be' are found.
Thereafter the pattern match expression fails.
The interesting part is the side effect: while backtracking,
the accumulator <code>N</code> keeps track of how many are found.

```txt
sentence starts with 'I'
sentence contains 'such'
sentence ends with 'even.'
sentence contains 3 occurrences of 'be'
```



## C

Case sensitive matching:

```C>#include <string.h

#include <stdio.h>

int startsWith(const char* container, const char* target)
{
  size_t clen = strlen(container), tlen = strlen(target);
  if (clen < tlen)
    return 0;
  return strncmp(container, target, tlen) == 0;
}

int endsWith(const char* container, const char* target)
{
  size_t clen = strlen(container), tlen = strlen(target);
  if (clen < tlen)
    return 0;
  return strncmp(container + clen - tlen, target, tlen) == 0;
}

int doesContain(const char* container, const char* target)
{
  return strstr(container, target) != 0;
}

int main(void)
{
  printf("Starts with Test ( Hello,Hell ) : %d\n", startsWith("Hello","Hell"));
  printf("Ends with Test ( Code,ode ) : %d\n", endsWith("Code","ode"));
  printf("Contains Test ( Google,msn ) : %d\n", doesContain("Google","msn"));

  return 0;
}
```

```txt
Starts with Test ( Hello,Hell ) : 1
Ends with Test ( Code,ode ) : 1
Contains Test ( Google,msn ) : 0
```

Code without using string library to demonstrate how char strings are just pointers:

```c
#include <stdio.h>

/* returns 0 if no match, 1 if matched, -1 if matched and at end */
int s_cmp(const char *a, const char *b)
{
        char c1 = 0, c2 = 0;
        while (c1 == c2) {
                c1 = *(a++);
                if ('\0' == (c2 = *(b++)))
                        return c1 == '\0' ? -1 : 1;
        }
        return 0;
}

/* returns times matched */
int s_match(const char *a, const char *b)
{
        int i = 0, count = 0;
        printf("matching `%s' with `%s':\n", a, b);

        while (a[i] != '\0') {
                switch (s_cmp(a + i, b)) {
                case -1:
                        printf("matched: pos %d (at end)\n\n", i);
                        return ++count;
                case 1:
                        printf("matched: pos %d\n", i);
                        ++count;
                        break;
                }
                i++;
        }
        printf("end match\n\n");
        return count;
}

int main()
{
        s_match("A Short String", "ort S");
        s_match("aBaBaBaBa", "aBa");
        s_match("something random", "Rand");

        return 0;
}
```

```txt
matching `A Short String' with `ort S':
matched: pos 4
end match

matching `aBaBaBaBa' with `aBa':
matched: pos 0
matched: pos 2
matched: pos 4
matched: pos 6 (at end)

matching `something random' with `Rand':
end match
```



## C++


```cpp
#include <string>
using namespace std;

string s1="abcd";
string s2="abab";
string s3="ab";
//Beginning
s1.compare(0,s3.size(),s3)==0;
//End
s1.compare(s1.size()-s3.size(),s3.size(),s3)==0;
//Anywhere
s1.find(s2)//returns string::npos
int loc=s2.find(s3)//returns 0
loc=s2.find(s3,loc+1)//returns 2
```


## C#
```c#

class Program
{
	public static void Main (string[] args)
	{
		var value = "abcd".StartsWith("ab");
		value = "abcd".EndsWith("zn"); //returns false
		value = "abab".Contains("bb"); //returns false
		value = "abab".Contains("ab"); //returns true
		int loc = "abab".IndexOf("bb"); //returns -1
		loc = "abab".IndexOf("ab"); //returns 0
		loc = "abab".IndexOf("ab",loc+1); //returns 2
	}
}

```



## Clojure

```clojure
(def evals '((. "abcd" startsWith "ab")
	     (. "abcd" endsWith "zn")
	     (. "abab" contains "bb")
	     (. "abab" contains "ab")
	     (. "abab" indexOf "bb")
	     (let [loc (. "abab" indexOf "ab")]
	       (. "abab" indexOf "ab" (dec loc)))))

user> (for [i evals] [i (eval i)])
([(. "abcd" startsWith "ab") true] [(. "abcd" endsWith "zn") false] [(. "abab" contains "bb") false] [(. "abab" contains "ab") true] [(. "abab" indexOf "bb") -1] [(let [loc (. "abab" indexOf "ab")] (. "abab" indexOf "ab" (dec loc))) 0])
```



## CoffeeScript


This example uses string slices, but a better implementation might use indexOf for slightly better performance.


```coffeescript

matchAt = (s, frag, i) ->
  s[i...i+frag.length] == frag

startsWith = (s, frag) ->
  matchAt s, frag, 0

endsWith = (s, frag) ->
  matchAt s, frag, s.length - frag.length

matchLocations = (s, frag) ->
  (i for i in [0..s.length - frag.length] when matchAt s, frag, i)

console.log startsWith "tacoloco", "taco" # true
console.log startsWith "taco", "tacoloco" # false
console.log startsWith "tacoloco", "talk" # false
console.log endsWith "tacoloco", "loco" # true
console.log endsWith "loco", "tacoloco" # false
console.log endsWith "tacoloco", "yoco" # false
console.log matchLocations "bababab", "bab" # [0,2,4]
console.log matchLocations "xxx", "x" # [0,1,2]

```



## Common Lisp


```lisp

(defun starts-with-p (str1 str2)
  "Determine whether `str1` starts with `str2`"
  (let ((p (search str2 str1)))
    (and p (= 0 p))))

(print (starts-with-p "foobar" "foo")) ; T
(print (starts-with-p "foobar" "bar")) ; NIL

(defun ends-with-p (str1 str2)
  "Determine whether `str1` ends with `str2`"
  (let ((p (mismatch str2 str1 :from-end T)))
    (or (not p) (= 0 p))))

(print (ends-with-p "foobar" "foo")) ; NIL
(print (ends-with-p "foobar" "bar")) ; T

(defun containsp (str1 str2)
  "Determine whether `str1` contains `str2`.
   Instead of just returning T, return a list of starting locations
   for every occurence of `str2` in `str1`"
   (unless (string-equal str2 "")
     (loop for p = (search str2 str1) then (search str2 str1 :start2 (1+ p))
           while p
           collect p)))

(print (containsp "foobar" "oba")) ; (2)
(print (containsp "ababaBa" "ba")) ; (1 3)
(print (containsp "foobar" "x"))   ; NIL

```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE StringMatch;
IMPORT StdLog,Strings;
CONST
	strSize = 1024;
	patSize = 256;

TYPE
	Matcher* = POINTER TO LIMITED RECORD
		str: ARRAY strSize OF CHAR;
		pat: ARRAY patSize OF CHAR;
		pos: INTEGER
	END;

PROCEDURE NewMatcher*(IN str: ARRAY OF CHAR): Matcher;
VAR
	m: Matcher;
BEGIN
	NEW(m);m.str := str$;m.pos:= 0;
	RETURN m
END NewMatcher;

PROCEDURE (m: Matcher) Match*(IN pat: ARRAY OF CHAR): INTEGER,NEW;
VAR
	pos: INTEGER;
BEGIN
	m.pat := pat$;
	pos := m.pos;
	Strings.Find(m.str,m.pat,pos,m.pos);
	RETURN m.pos
END Match;

PROCEDURE (m: Matcher) Next*(): INTEGER, NEW;
VAR
	pos: INTEGER;
BEGIN
	pos := m.pos + LEN(m.pat$);
	Strings.Find(m.str,m.pat,pos,m.pos);
	RETURN m.pos;
END Next;

(* Some Helper functions based on Strings module *)
PROCEDURE StartsWith(IN str: ARRAY OF CHAR;IN pat: ARRAY OF CHAR): BOOLEAN;
VAR
	pos: INTEGER;
BEGIN
	Strings.Find(str,pat,0,pos);
	RETURN pos = 0
END StartsWith;

PROCEDURE Contains(IN str: ARRAY OF CHAR;IN pat: ARRAY OF CHAR; OUT pos: INTEGER): BOOLEAN;
BEGIN
	Strings.Find(str,pat,0,pos);
	RETURN pos >= 0
END Contains;

PROCEDURE EndsWith(IN str: ARRAY OF CHAR;IN pat: ARRAY OF CHAR): BOOLEAN;
VAR
	pos: INTEGER;
BEGIN
	Strings.Find(str,pat,0,pos);
	RETURN pos + LEN(pat$) = LEN(str$)
END EndsWith;

PROCEDURE Do*;
CONST
	aStr = "abcdefghijklmnopqrstuvwxyz";
VAR
	pat: ARRAY 128 OF CHAR;
	res: BOOLEAN;
	at: INTEGER;
	m: Matcher;
BEGIN
	(* StartsWith *)
	pat := "abc";
	StdLog.String(aStr + " startsWith " + pat + " :>");StdLog.Bool(StartsWith(aStr,pat));StdLog.Ln;
	pat := "cba";
	StdLog.String(aStr + " startsWith " + pat + " :>");StdLog.Bool(StartsWith(aStr,pat));StdLog.Ln;
	pat := "def";
	StdLog.String(aStr + " startsWith " + pat + " :>");StdLog.Bool(StartsWith(aStr,pat));StdLog.Ln;
	StdLog.Ln;
	(* Contains *)
	pat := 'def';
	StdLog.String(aStr + " contains " + pat + " :>");StdLog.Bool(Contains(aStr,pat,at));
	StdLog.String(" at: ");StdLog.Int(at);StdLog.Ln;
	pat := 'efd';
	StdLog.String(aStr + " contains " + pat + " :>");StdLog.Bool(Contains(aStr,pat,at));
	StdLog.String(" at: ");StdLog.Int(at);StdLog.Ln;
	pat := 'abc';
	StdLog.String(aStr + " contains " + pat + " :>");StdLog.Bool(Contains(aStr,pat,at));
	StdLog.String(" at: ");StdLog.Int(at);StdLog.Ln;
	pat := 'xyz';
	StdLog.String(aStr + " contains " + pat + " :>");StdLog.Bool(Contains(aStr,pat,at));
	StdLog.String(" at: ");StdLog.Int(at);StdLog.Ln;
	StdLog.Ln;
	(* EndsWith *)
	pat := 'xyz';
	StdLog.String(aStr + " endsWith " + pat + " :>");StdLog.Bool(EndsWith(aStr,pat));StdLog.Ln;
	pat := 'zyx';
	StdLog.String(aStr + " endsWith " + pat + " :>");StdLog.Bool(EndsWith(aStr,pat));StdLog.Ln;
	pat := 'abc';
	StdLog.String(aStr + " endsWith " + pat + " :>");StdLog.Bool(EndsWith(aStr,pat));StdLog.Ln;
	pat:= 'def';
	StdLog.String(aStr + " endsWith " + pat + " :>");StdLog.Bool(EndsWith(aStr,pat));StdLog.Ln;
	StdLog.Ln;

	m := NewMatcher("abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz");
	StdLog.String("Matching 'abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz' against 'abc':> ");
	StdLog.Ln;
	StdLog.String("Match at: ");StdLog.Int(m.Match("abc"));StdLog.Ln;
	StdLog.String("Match at: ");StdLog.Int(m.Next());StdLog.Ln;
	StdLog.String("Match at: ");StdLog.Int(m.Next());StdLog.Ln
END Do;
END StringMatch.

```

Execute: ^Q StringMatching.Do <br/>
```txt

abcdefghijklmnopqrstuvwxyz startsWith abc :> $TRUE
abcdefghijklmnopqrstuvwxyz startsWith cba :> $FALSE
abcdefghijklmnopqrstuvwxyz startsWith def :> $FALSE

abcdefghijklmnopqrstuvwxyz contains def :> $TRUE at:  3
abcdefghijklmnopqrstuvwxyz contains efd :> $FALSE at:  -1
abcdefghijklmnopqrstuvwxyz contains abc :> $TRUE at:  0
abcdefghijklmnopqrstuvwxyz contains xyz :> $TRUE at:  23

abcdefghijklmnopqrstuvwxyz endsWith xyz :> $TRUE
abcdefghijklmnopqrstuvwxyz endsWith zyx :> $FALSE
abcdefghijklmnopqrstuvwxyz endsWith abc :> $FALSE
abcdefghijklmnopqrstuvwxyz endsWith def :> $FALSE

Matching 'abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz' against 'abc':>
Match at:  0
Match at:  26
Match at:  -1

```



## D


```d
void main() {
    import std.stdio;
    import std.algorithm: startsWith, endsWith, find, countUntil;

    "abcd".startsWith("ab").writeln;      // true
    "abcd".endsWith("zn").writeln;        // false
    "abab".find("bb").writeln;            // empty array (no match)
    "abcd".find("bc").writeln;            // "bcd" (substring start
                                           //        at match)
    "abab".countUntil("bb").writeln;      // -1 (no match)
    "abab".countUntil("ba").writeln;      //  1 (index of 1st match)

    // std.algorithm.startsWith also works on arrays and ranges:
    [1, 2, 3].countUntil(3).writeln;      //  2
    [1, 2, 3].countUntil([2, 3]).writeln; //  1
}
```

```txt
true
false

bcd
-1
1
2
1
```


## DCL


```DCL
$ first_string = p1
$ length_of_first_string = f$length( first_string )
$ second_string = p2
$ length_of_second_string = f$length( second_string )
$ offset = f$locate( second_string, first_string )
$ if offset .eq. 0
$ then
$  write sys$output "first string starts with second string"
$ else
$  write sys$output "first string does not start with second string"
$ endif
$ if offset .ne. length_of_first_string
$ then
$  write sys$output "first string contains the second string at location ", offset
$ else
$  write sys$output "first string does not contain the second string at any location"
$ endif
$ temp = f$extract( length_of_first_string - length_of_second_string, length_of_second_string, first_string )
$ if second_string .eqs. temp
$ then
$  write sys$output "first string ends with the second string"
$ else
$  write sys$output "first string does not end with the second string"
$ endif
```

```txt
$ @string_matching efabcdef ef
first string starts with second string
first string contains the second string at location 0
first string ends with the second string
$ @string_matching efabcdef ab
first string does not start with second string
first string contains the second string at location 2
first string does not end with the second string
$ @string_matching efabcdef def
first string does not start with second string
first string contains the second string at location 5
first string ends with the second string
$ @string_matching efabcdef defx
first string does not start with second string
first string does not contain the second string at any location
first string does not end with the second string
```



## Delphi


```Delphi
program CharacterMatching;

{$APPTYPE CONSOLE}

uses StrUtils;

begin
  WriteLn(AnsiStartsText('ab', 'abcd')); // True
  WriteLn(AnsiEndsText('zn', 'abcd')); // False
  WriteLn(AnsiContainsText('abcd', 'bb')); // False
  Writeln(AnsiContainsText('abcd', 'ab')); // True
  WriteLn(Pos('ab', 'abcd')); // 1
end.
```



## Dyalect



```Dyalect
var value = "abcd".startsWith("ab")
value = "abcd".endsWith("zn") //returns false
value = "abab".contains("bb") //returns false
value = "abab".contains("ab") //returns true
var loc = "abab".indexOf("bb") //returns -1
loc = "abab".indexOf("ab") //returns 0
```



## E



```e
def f(string1, string2) {
    println(string1.startsWith(string2))

    var index := 0
    while ((index := string1.startOf(string2, index)) != -1) {
        println(`at $index`)
        index += 1
    }

    println(string1.endsWith(string2))
}
```



## EchoLisp


```lisp

(string-suffix? "nette" "Antoinette") → #t
(string-prefix? "Simon" "Simon & Garfunkel") → #t

(string-match "Antoinette" "net") → #t ;; contains
(string-index "net" "Antoinette") → 5  ;; substring location

```



## Elena

ELENA 4.x :

```elena
import extensions;

public program()
{
    var s := "abcd";

    console.printLine(s," starts with ab: ",s.startingWith:"ab");
    console.printLine(s," starts with cd: ",s.startingWith:"cd");

    console.printLine(s," ends with ab: ",s.endingWith:"ab");
    console.printLine(s," ends with cd: ",s.endingWith:"cd");

    console.printLine(s," contains ab: ",s.containing:"ab");
    console.printLine(s," contains bc: ",s.containing:"bc");
    console.printLine(s," contains cd: ",s.containing:"cd");
    console.printLine(s," contains az: ",s.containing:"az");

    console.printLine(s," index of az: ",s.indexOf(0, "az"));
    console.printLine(s," index of cd: ",s.indexOf(0, "cd"));
    console.printLine(s," index of bc: ",s.indexOf(0, "bc"));
    console.printLine(s," index of ab: ",s.indexOf(0, "ab"));

    console.readChar()
}
```



## Elixir

The String module has functions that cover the base requirements.

```elixir
s1 = "abcd"
s2 = "adab"
s3 = "ab"

String.starts_with?(s1, s3)
# => true
String.starts_with?(s2, s3)
# => false

String.contains?(s1, s3)
# => true
String.contains?(s2, s3)
# => true

String.ends_with?(s1, s3)
# => false
String.ends_with?(s2, s3)
# => true


# Optional requirements:
Regex.run(~r/#{s3}/, s1, return: :index)
# => [{0, 2}]
Regex.run(~r/#{s3}/, s2, return: :index)
# => [{2, 2}]

Regex.scan(~r/#{s3}/, "abcabc", return: :index)
# => [[{0, 2}], [{3, 2}]]
```



## Emacs Lisp


```Emacs Lisp

(defun match (word str)
  (progn

    (setq regex (format "^%s.*$" word) )

    (if (string-match regex str)
	(insert (format "%s found in beginning of: %s\n" word str) )
      (insert (format "%s not found in beginning of: %s\n" word str) ))

    (setq pos (string-match word str) )

    (if pos
	(insert (format "%s found at position %d in: %s\n" word pos str) )
      (insert (format "%s not found in: %s\n" word str) ))

    (setq regex (format "^.*%s$" word) )

    (if (string-match regex str)
	(insert (format "%s found in end of: %s\n" word str) )
      (insert (format "%s not found in end of: %s\n" word str) ))))

(setq string "before center after")

(progn
  (match "center" string)
  (insert "\n")
  (match "before" string)
  (insert "\n")
  (match "after" string) )

```

<b>Output:</b>

```txt

center not found in beginning of: before center after
center found at position 7 in: before center after
center not found in end of: before center after

before found in beginning of: before center after
before found at position 0 in: before center after
before not found in end of: before center after

after not found in beginning of: before center after
after found at position 14 in: before center after
after found in end of: before center after

```



## Erlang


```erlang

-module(character_matching).
-export([starts_with/2,ends_with/2,contains/2]).

%% Both starts_with and ends_with are mappings to 'lists:prefix/2' and
%% 'lists:suffix/2', respectively.

starts_with(S1,S2) ->
    lists:prefix(S2,S1).

ends_with(S1,S2) ->
    lists:suffix(S2,S1).

contains(S1,S2) ->
    contains(S1,S2,1,[]).

%% While S1 is at least as long as S2 we check if S2 is its prefix,
%% storing the result if it is. When S1 is shorter than S2, we return
%% the result. NB: this will work on any arbitrary list in erlang
%% since it makes no distinction between strings and lists.
contains([_|T]=S1,S2,N,Acc) when length(S2) =< length(S1) ->
    case starts_with(S1,S2) of
        true ->
            contains(T,S2,N+1,[N|Acc]);
        false ->
            contains(T,S2,N+1,Acc)
    end;
contains(_S1,_S2,_N,Acc) ->
    Acc.

```


## Euphoria


```euphoria
sequence first, second
integer x

first = "qwertyuiop"

-- Determining if the first string starts with second string
second = "qwerty"
if match(second, first) = 1 then
    printf(1, "'%s' starts with '%s'\n", {first, second})
else
    printf(1, "'%s' does not start with '%s'\n", {first, second})
end if

-- Determining if the first string contains the second string at any location
-- Print the location of the match for part 2
second = "wert"
x = match(second, first)
if x then
    printf(1, "'%s' contains '%s' at position %d\n", {first, second, x})
else
    printf(1, "'%s' does not contain '%s'\n", {first, second})
end if

-- Determining if the first string ends with the second string
second = "uio"
if length(second)<=length(first) and match_from(second, first, length(first)-length(second)+1) then
    printf(1, "'%s' ends with '%s'\n", {first, second})
else
    printf(1, "'%s' does not end with '%s'\n", {first, second})
end if
```


```txt
'qwertyuiop' starts with 'qwerty'
'qwertyuiop' contains 'wert' at position 2
'qwertyuiop' does not end with 'uio'

```


=={{header|F_Sharp|F#}}==

```fsharp>[<EntryPoint
]
let main args =

    let text = "一二三四五六七八九十"
    let starts = "一二"
    let ends = "九十"
    let contains = "五六"
    let notContains = "百"

    printfn "text = %A" text
    printfn "starts with %A: %A" starts (text.StartsWith(starts))
    printfn "starts with %A: %A" ends (text.StartsWith(ends))
    printfn "ends with %A: %A" ends (text.EndsWith(ends))
    printfn "ends with %A: %A" starts (text.EndsWith(starts))
    printfn "contains %A: %A" contains (text.Contains(contains))
    printfn "contains %A: %A" notContains (text.Contains(notContains))
    printfn "substring %A begins at position %d (zero-based)" contains (text.IndexOf(contains))
    let text2 = text + text
    printfn "text = %A" text2
    Seq.unfold (fun (n : int) ->
            let idx = text2.IndexOf(contains, n)
            if idx < 0 then None else Some (idx, idx+1)) 0
    |> Seq.iter (printfn "substring %A begins at position %d (zero-based)" contains)
    0
```

```txt
text = "一二三四五六七八九十"
starts with "一二": true
starts with "九十": false
ends with "九十": true
ends with "一二": false
contains "五六": true
contains "百": false
substring "五六" begins at position 4 (zero-based)
text = "一二三四五六七八九十一二三四五六七八九十"
substring "五六" begins at position 4 (zero-based)
substring "五六" begins at position 14 (zero-based)
```



## Factor

Does <code>cheesecake</code> start with <code>cheese</code>?

```factor
"cheesecake" "cheese" head?   ! t
```

Does <code>cheesecake</code> contain <code>sec</code> at any location?

```factor
"sec" "cheesecake" subseq?   ! t
```

Does <code>cheesecake</code> end with <code>cake</code>?

```factor
"cheesecake" "cake" tail?   ! t
```

Where in <code>cheesecake</code> is the leftmost <code>sec</code>?

```factor
"sec" "cheesecake" subseq-start   ! 4
```

Where in <code>Mississippi</code> are all occurrences of <code>iss</code>?

```factor
USE: regexp
"Mississippi" "iss" <regexp> all-matching-slices [ from>> ] map   ! { 1 4 }
```



## Falcon

'''VBA/Python programmer's approach.  I'm just a junior Falconeer but this code seems falconic''

```falcon

/* created by Aykayayciti Earl Lamont Montgomery
April 9th, 2018 */

s1 = "Naig Ialocin Olracnaig"
s2 = "Naig"

var = s1.startsWith(s2) ? s1 + " starts with " + s2 : s1 + " does not start with " + s2
> var

s2 = "loc"
var = s2 in s1 ? @ "$s1 contains $s2" : @ "$s1 does not contain $s2"
> var

> s1.endsWith(s2) ? @ "s1 ends with $s2" : @ "$s1 does not end with $s2"

```

```txt

Naig Ialocin Olracnaig starts with Naig
Naig Ialocin Olracnaig contains loc
Naig Ialocin Olracnaig does not end with loc
[Finished in 1.2s]

```



## Fantom


Fantom provides several self-explanatory string-matching methods:

* <code>startsWith</code>
* <code>endsWith</code>
* <code>contains</code>
* <code>index</code> (takes an optional index, for the start position)
* <code>indexIgnoreCase</code> (like above, ignoring case for ASCII characters)
* <code>indexr</code> (start search from end of string, with an optional index)
* <code>indexrIgnoreCase</code> (like above, ignoring case for ASCII characters)


```fantom

class Main
{
  public static Void main ()
  {
    string := "Fantom Language"
    echo ("String is: " + string)
    echo ("does string start with 'Fantom'? " + string.startsWith("Fantom"))
    echo ("does string start with 'Language'? " + string.startsWith("Language"))
    echo ("does string contain 'age'? " + string.contains("age"))
    echo ("does string contain 'page'? " + string.contains("page"))
    echo ("does string end with 'Fantom'? " + string.endsWith("Fantom"))
    echo ("does string end with 'Language'? " + string.endsWith("Language"))

    echo ("Location of 'age' is: " + string.index("age"))
    posn := string.index ("an")
    echo ("First location of 'an' is: " + posn)
    posn = string.index ("an", posn+1)
    echo ("Second location of 'an' is: " + posn)
    posn = string.index ("an", posn+1)
    if (posn == null) echo ("No third location of 'an'")
  }
}

```


```txt

String is: Fantom Language
does string start with 'Fantom'? true
does string start with 'Language'? false
does string contain 'age'? true
does string contain 'page'? false
does string end with 'Fantom'? false
does string end with 'Language'? true
Location of 'age' is: 12
First location of 'an' is: 1
Second location of 'an' is: 8
No third location of 'an'

```



## FBSL


```qbasic
#APPTYPE CONSOLE

DIM s = "roko, mat jane do"

IF LEFT(s, 4) = "roko" THEN PRINT STRENC(s), " starts with ", STRENC("roko")
IF INSTR(s, "mat") THEN PRINT STRENC(s), " contains ", STRENC("mat"), " at ", INSTR
IF RIGHT(s, 2) = "do" THEN PRINT STRENC(s), " ends with ", STRENC("do")
PRINT STRENC(s), " contains ", STRENC("o"), " at the following locations:", InstrEx(s, "o")

PAUSE

SUB InstrEx(mane, match)
    INSTR = 0
    WHILE INSTR(mane, match, INSTR + 1): PRINT " ", INSTR;: WEND
END SUB

```

```txt
"roko, mat jane do" starts with "roko"
"roko, mat jane do" contains "mat" at 7
"roko, mat jane do" ends with "do"
"roko, mat jane do" contains "o" at the following locations: 2 4 17

Press any key to continue...
```



## Forth


```forth
: starts-with ( a l a2 l2 -- ? )
  tuck 2>r min 2r> compare 0= ;
: ends-with ( a l a2 l2 -- ? )
  tuck 2>r negate over + 0 max /string 2r> compare 0= ;
\ use SEARCH ( a l a2 l2 -- a3 l3 ? ) for contains
```



## Fortran

Fortran does not offer a string type, but since F77 it has been possible to use a CHARACTER variable, of some specified size, whose size may be accessed via the LEN function. When passed as a parameter, a secret additional parameter specifies its size and so string-like usage is possible. Character matching is case sensitive, and, trailing spaces are ignored so that "xx" and "xx " are deemed equal. The function INDEX(text,target) determines the first index in ''text'' where ''target'' matches, and returns zero if there is no such match. Unfortunately, the function does not allow the specification of a starting position for a search, as to find any second and further matches. One must specify something like <code>INDEX(text(5:),target)</code> to start with position five, and then deal with the resulting offsets needed to relate the result to positions within the parameter. On the other hand, since there is no "length" conjoined to the text such substring selections can be made without copying the text to a work area, unlike the <code>copy(text,start,stop)</code> equivalent of Pascal for example. Some Fortran compilers ''do'' offer a starting point, and also an option to search backwards from the end, but these facilities are not guaranteed. Similarly, INDEX is only made available for CHARACTER searching, even though it could easily be generalised to other types.

A second problem is presented by the possibility that a logical expression such as <code>L.LT.0 .OR. ''etc.''</code> will always or might possibly or in certain constructions but not others be fully evaluated, which is to say that the ''etc'' will be evaluated even though L < 0 is ''true'' so that the result is determined. And in this case, evaluating the ''etc'' will cause trouble because the indexing won't work! To be safe, therefore, a rather lame two-stage test is required - though optimising compilers might well shift code around anyway.

In the case of STARTS, these annoyances can be left to the INDEX function rather than comparing the start of A against B. At the cost of it searching the whole of A if B is not at the start. Otherwise, it would be the mirror of ENDS.


```Fortran

      SUBROUTINE STARTS(A,B)	!Text A starts with text B?
       CHARACTER*(*) A,B
        IF (INDEX(A,B).EQ.1) THEN	!Searches A to find B.
          WRITE (6,*) ">",A,"< starts with >",B,"<"
         ELSE
          WRITE (6,*) ">",A,"< does not start with >",B,"<"
        END IF
      END SUBROUTINE STARTS

      SUBROUTINE HAS(A,B)	!Text B appears somewhere in text A?
       CHARACTER*(*) A,B
       INTEGER L
        L = INDEX(A,B)		!The first position in A where B matches.
        IF (L.LE.0) THEN
          WRITE (6,*) ">",A,"< does not contain >",B,"<"
         ELSE
          WRITE (6,*) ">",A,"< contains a >",B,"<, offset",L
        END IF
      END SUBROUTINE HAS

      SUBROUTINE ENDS(A,B)	!Text A ends with text B.
       CHARACTER*(*) A,B
       INTEGER L
        L = LEN(A) - LEN(B)	!Find the tail end of A that B might match.
        IF (L.LT.0) THEN	!Dare not use an OR, because of full evaluation risks.
          WRITE (6,*) ">",A,"< is too short to end with >",B,"<"	!Might as well have a special message.
        ELSE IF (A(L + 1:L + LEN(B)).NE.B) THEN	!Otherwise, it is safe to look.
          WRITE (6,*) ">",A,"< does not end with >",B,"<"
        ELSE
          WRITE (6,*) ">",A,"< ends with >",B,"<"
        END IF
      END SUBROUTINE ENDS

      CALL STARTS("This","is")
      CALL STARTS("Theory","The")
      CALL HAS("Bananas","an")
      CALL ENDS("Banana","an")
      CALL ENDS("Banana","na")
      CALL ENDS("Brief","Much longer")
      END

```

Output: text strings are bounded by >''etc.''< in case of leading or trailing spaces.

```txt

 >This< does not start with >is<
 >Theory< starts with >The<
 >Bananas< contains a >an<, offset           2
 >Banana< does not end with >an<
 >Banana< ends with >na<
 >Brief< is too short to end with >Much longer<

```


Similar program using modern Fortran style

```Fortran

!-----------------------------------------------------------------------
!Main program string_matching
!-----------------------------------------------------------------------
program    string_matching
   implicit none
   character(len=*), parameter :: fmt= '(I0)'
   write(*,fmt) starts("this","is")
   write(*,fmt) starts("theory","the")
   write(*,fmt) has("bananas","an")
   write(*,fmt) ends("banana","an")
   write(*,fmt) ends("banana","na")
   write(*,fmt) ends("brief","much longer")

 contains
   !     Determining if the first string starts with second string
   function  starts(string1, string2) result(answer)
      implicit none
      character(len=*), intent(in) :: string1
      character(len=*), intent(in) :: string2
      integer :: answer
      answer = 0
      if(len(string2)>len(string1)) return
      if(string1(1:len(string2))==string2) answer = 1
   end function starts
   !     Determining if the first string contains the second string at any location
   function  has(string1, string2) result(answer)
      implicit none
      character(len=*), intent(in) :: string1
      character(len=*), intent(in) :: string2
      character(len=:),allocatable :: temp
      integer :: answer, add
      character(len=*), parameter :: fmt= '(A6,X,I0)'
      answer = 0
      add = 0
      if(len(string2)>len(string1)) return
      answer = index(string1, string2)
      if(answer==0) return
      !     Print the location of the match for part 2
      write(*,fmt) " at ", answer
      !     Handle multiple occurrences of a string for part 2.
      add = answer
      temp = string1(answer+1:)
      do while(answer>0)
         answer = index(temp, string2)
         add = add + answer
         if(answer>0) write(*,fmt) " at ", add
         !          deallocate(temp)
         temp = string1(add+1:) ! auto reallocation
      enddo
      answer = 1
   end function has
   !     Determining if the first string ends with the second string
   function  ends(string1, string2) result(answer)
      implicit none
      character(len=*), intent(in) :: string1
      character(len=*), intent(in) :: string2
      integer :: answer
      answer = 0
      if(len(string2)>len(string1)) return
      if(string1(len(string1)-len(string2)+1:)==string2) answer = 1
   end function ends
end program string_matching

```

Output: false = 0, true = 1 ( + multiple occurrences if applicable)

```txt

0
1
   at  2
   at  4
1
0
1
0

```

In recent standards of Fortran strings as intrinsic first-class type and many intrinsic procedures for strings manipulation have been added.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim As String s1 = "abracadabra"
Dim As String s2 = "abra"
Print "First string  : "; s1
Print "Second string : "; s2
Print
Print "First string begins with second string : "; CBool(s2 = Left(s1, Len(s2)))
Dim As Integer i1 = Instr(s1, s2)
Dim As Integer i2
Print "First string contains second string    : ";
If i1 Then
  Print "at index"; i1;
  i2 = Instr(i1 + Len(s2), s1, s2)
  If i2 Then
    Print " and at index"; i2
  Else
    Print
  End If
Else
  Print "false";
End If
Print "First string ends with second string   : "; CBool(s2 = Right(s1, Len(s2)))
Print
Print "Press any key to quit"
Sleep
```


```txt

First string  : abracadabra
Second string : abra

First string begins with second string : true
First string contains second string    : at index 1 and at index 8
First string ends with second string   : true

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=07bb32f4e8e8f7d81898cf41d4431a2e Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sString1 As String = "Hello world"
Dim sString2 As String = "Hello"

Print sString1 Begins Left(sString2, 5)                             'Determine if the first string starts with second string
If InStr(sString1, sString2) Then Print "True" Else Print "False"   'Determine if the first string contains the second string at any location
Print sString1 Ends Left(sString2, 5)                               'Determine if the first string ends with the second string

End
```

Output:

```txt

True
True
False

```



## GML

```GML
#define charMatch
{
    first = "qwertyuiop";

    // Determining if the first string starts with second string
    second = "qwerty";
    if (string_pos(second, first) > 0) {
        show_message("'" + first + "' starts with '" + second + "'");
    } else {
        show_message("'" + first + "' does not start with '" + second + "'");
    }

    second = "wert"
    // Determining if the first string contains the second string at any location
    // Print the location of the match for part 2
    if (string_pos(second, first) > 0) {
        show_message("'" + first + "' contains '" + second + "' at position " + string(x));
    } else {
        show_message("'" + first + "' does not contain '" + second + "'");
    }
    // Handle multiple occurrences of a string for part 2.
    x = string_count(second, first);
    show_message("'" + first + "' contains " + string(x) + " instances of '" + second + "'");

// Determining if the first string ends with the second string
    second = "random garbage"
    temp = string_copy(first,
                       (string_length(first) - string_length(second)) + 1,
                       string_length(second));
    if (temp == second) {
        show_message("'" + first + "' ends with '" + second + "'");
    } else {
        show_message("'" + first + "' does not end with '" + second + "'");
    }
}
```


{{out}} (in message boxes, 1 per line):
 'qwertyuiop' starts with 'qwerty'
 'qwertyuiop' contains 'wert' at position 0
 'qwertyuiop' contains 1 instances of 'wert'
 'qwertyuiop' does not end with 'random garbage'


## Go


```go
package main

import (
    "fmt"
    "strings"
)

func match(first, second string) {
    fmt.Printf("1. %s starts with %s: %t\n",
        first, second, strings.HasPrefix(first, second))
    i := strings.Index(first, second)
    fmt.Printf("2. %s contains %s: %t,\n", first, second, i >= 0)
    if i >= 0 {
        fmt.Printf("2.1. at location %d,\n", i)
        for start := i+1;; {
            if i = strings.Index(first[start:], second); i < 0 {
                break
            }
            fmt.Printf("2.2. at location %d,\n", start+i)
            start += i+1
        }
        fmt.Println("2.2. and that's all")
    }
    fmt.Printf("3. %s ends with %s: %t\n",
        first, second, strings.HasSuffix(first, second))
}

func main() {
    match("abracadabra", "abr")
}
```

```txt

1. abracadabra starts with abr: true
2. abracadabra contains abr: true,
2.1. at location 0,
2.2. at location 7,
2.2. and that's all
3. abracadabra ends with abr: false

```



## Groovy

Examples:

```groovy
assert "abcd".startsWith("ab")
assert ! "abcd".startsWith("zn")
assert "abcd".endsWith("cd")
assert ! "abcd".endsWith("zn")
assert "abab".contains("ba")
assert ! "abab".contains("bb")


assert "abab".indexOf("bb") == -1 // not found flag
assert "abab".indexOf("ab") == 0

def indicesOf = { string, substring ->
    if (!string) { return [] }
    def indices = [-1]
    while (true) {
        indices << string.indexOf(substring, indices.last()+1)
        if (indices.last() == -1) break
    }
    indices[1..<(indices.size()-1)]
}
assert indicesOf("abab", "ab") == [0, 2]
assert indicesOf("abab", "ba") == [1]
assert indicesOf("abab", "xy") == []
```


All assertions pass, so there is no output.


## Haskell


```haskell>
 import Data.List
> "abc" `isPrefixOf` "abcdefg"
True
> "efg" `isSuffixOf` "abcdefg"
True
> "bcd" `isInfixOf` "abcdefg"
True
> "abc" `isInfixOf` "abcdefg" -- Prefixes and suffixes are also infixes
True
> let infixes a b = findIndices (isPrefixOf a) $ tails b
> infixes "ab" "abcdefabqqab"
[0,6,10]
```


== {{header|Icon}} and {{header|Unicon}} ==

```Icon
procedure main()

   write("Matching s2 :=",image(s2 := "ab")," within s1:= ",image(s1 := "abcdabab"))

   write("Test #1 beginning ",if match(s2,s1) then "matches " else "failed")
   writes("Test #2 all matches at positions [")
      every writes(" ",find(s2,s1)|"]\n")
   write("Test #3 ending ", if s1[0-:*s2] == s2 then "matches" else "fails")

end
```


```txt
Matching s2 :="ab" within s1:= "abcdabab"
Test #1 beginning matches
Test #2 all matches at positions [ 1 5 7 ]
Test #3 ending matches
```



## J


```j
startswith=: ] -: ({.~ #)
contains=: +./@:E.~
endswith=: ] -: ({.~ -@#)
```


Example use:


```j
   'abcd' startswith 'ab'
1
   'abcd' startswith 'cd'
0
   'abcd' endswith 'ab'
0
   'abcd' endswith 'cd'
1
   'abcd' contains 'bb'
0
   'abcd' contains 'ab'
1
   'abcd' contains 'bc'
1
   'abab' contains 'ab'
1
   'abab' I.@E.~ 'ab'       NB. find starting indicies
0 2
```


Note that these verbs contain no constraints restricting them to sequences of characters and so also apply to arrays of type other than character:

```j
   0 1 2 3 startswith 0 1               NB. integer
1
   4.2 5.1 1.3 9 3 contains 1.3 4.2     NB. floating point
0
   4.2 5.1 1.3 4.2 9 3 contains 1.3 4.2
1
```



## Java


```java
"abcd".startsWith("ab") //returns true
"abcd".endsWith("zn") //returns false
"abab".contains("bb") //returns false
"abab".contains("ab") //returns true
int loc = "abab".indexOf("bb") //returns -1
loc = "abab".indexOf("ab") //returns 0
loc = "abab".indexOf("ab",loc+1) //returns 2
```


// -----------------------------------------------------------//
public class JavaApplication6 {

    public static void main(String[] args) {
        String strOne = "complexity";
        String strTwo = "udacity";

        //
        stringMatch(strOne, strTwo);

    }

    public static void stringMatch(String one, String two) {
        boolean match = false;
        if (one.charAt(0) == two.charAt(0)) {
            System.out.println(match = true);   // returns true
        } else {
            System.out.println(match);       // returns false
        }
        for (int i = 0; i < two.length(); i++) {
            int temp = i;
            for (int x = 0; x < one.length(); x++) {
                if (two.charAt(temp) == one.charAt(x)) {
                    System.out.println(match = true);    //returns true
                    i = two.length();
                }
            }
        }
        int num1 = one.length() - 1;
        int num2 = two.length() - 1;
        if (one.charAt(num1) == two.charAt(num2)) {
            System.out.println(match = true);
        } else {
            System.out.println(match = false);
        }
    }
}


## JavaScript



```javascript
var stringA = "tacoloco"
  , stringB = "co"
  , q1, q2, q2multi, m
  , q2matches = []

// stringA starts with stringB
q1 = stringA.substring(0, stringB.length) == stringB

// stringA contains stringB
q2  = stringA.indexOf(stringB)

// multiple matches
q2multi = new RegExp(stringB,'g')

while(m = q2multi.exec(stringA)){
	q2matches.push(m.index)
}

// stringA ends with stringB
q3 = stringA.substr(-stringB.length) == stringB

console.log("1: Does '"+stringA+"' start with '"+stringB+"'? " + ( q1 ? "Yes." : "No."))
console.log("2: Is '"+stringB+"' contained in '"+stringA+"'? " + (~q2 ? "Yes, at index "+q2+"." : "No."))
if (~q2 && q2matches.length > 1){
	console.log("   In fact, it happens "+q2matches.length+" times within '"+stringA+"', at index"+(q2matches.length > 1 ? "es" : "")+" "+q2matches.join(', ')+".")
}
console.log("3: Does '"+stringA+"' end with '"+stringB+"'? "   + ( q3 ? "Yes." : "No."))
```


```txt
1: Does 'tacoloco' start with 'co'? No.
2: Is 'co' contained in 'tacoloco'? Yes, at index 2.
   In fact, it happens 2 times within 'tacoloco', at indexes 2, 6.
3: Does 'tacoloco' end with 'co'? Yes.
```




## jq

Using the builtins of jq 1.4 and later:

```jq
# startswith/1 is boolean:
"abc" | startswith("ab")
#=> true
```



```jq
# index/1 returns the index or null,
# so the jq test "if index(_) then ...." can be used
# without any type conversion.

"abcd" | index( "bc")
#=> 1
```



```jq
# endswith/1 is also boolean:
"abc" | endswith("bc")
#=> true
```


Using the regex functions available in jq 1.5:

```jq
"abc" | test( "^ab")

"abcd" | test("bc")

"abcd" | test("cd$")
```



### Multiple Occurrences

To determine all the indices of one string in another:

```sh
# In jq 1.4 or later:
jq -n '"abcdabcd" | indices("bc")'
[
  1,
  5
]
```


In jq 1.5, the regex function match/1 can also be used:

```sh
$ jq -n '"abcdabcd" | match("bc"; "g") | .offset'
1
5
```




## Julia


```julia

startswith("abcd","ab")            #returns true
findfirst("ab", "abcd")            #returns 1:2, indices range where string was found
endswith("abcd","zn")              #returns false
match(r"ab","abcd") != Nothing     #returns true where 1st arg is regex string
for r in eachmatch(r"ab","abab")
	println(r.offset)
end                                #returns 1, then 3 matching the two starting indices where the substring was found

```



## K


```k
startswith: {:[0<#p:_ss[x;y];~*p;0]}
endswith: {0=(-#y)+(#x)-*_ss[x;y]}
contains: {0<#_ss[x;y]}
```


'''Example:'''


```k
  startswith["abcd";"ab"]
1
  startswith["abcd";"bc"]
0
  endswith["abcd";"cd"]
1
  endswith["abcd";"bc"]
0
  contains["abcdef";"cde"]
1
  contains["abcdef";"bdef"]
0
  _ss["abcdabceabc";"abc"]    / location of matches
0 4 8
```




## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    val s1 = "abracadabra"
    val s2 = "abra"
    println("$s1 begins with $s2 : ${s1.startsWith(s2)}")
    println("$s1 ends with $s2   : ${s1.endsWith(s2)}")
    val b  = s2 in s1
    print("$s1 contains $s2    : $b")
    if (b) println(" at locations ${s1.indexOf(s2) + 1} and ${s1.lastIndexOf(s2) + 1}")
    else println()
}
```


```txt

abracadabra begins with abra : true
abracadabra ends with abra   : true
abracadabra contains abra    : true at locations 1 and 8

```



## LabVIEW

These images solve the task's requirements in order.<br/>{{VI snippet}}<br/>
[[File:LabVIEW_Character_matching_1.png]]<br/>
[[File:LabVIEW_Character_matching_2.png]]<br/>
[[File:LabVIEW_Character_matching_3.png]]



## Lasso



```Lasso
local(
 a = 'a quick brown peanut jumped over a quick brown fox',
 b = 'a quick brown'
)

//Determining if the first string starts with second string
#a->beginswith(#b) // true

//Determining if the first string contains the second string at any location
#a >> #b           // true
#a->contains(#b)   // true

//Determining if the first string ends with the second string
#a->endswith(#b)   // false
```



## Lang5


```lang5
: 2array  2 compress ; : bi*  '_ set dip _ execute ;  : bi@  dup bi* ;
: comb  "" split ;  : concat  "" join ;  : dip  swap '_ set execute _ ;
: first  0 extract swap drop ; : flip  comb reverse concat ;

: contains
    swap 'comb bi@ length do                    # create a matrix.
         1 - "dup 1 1 compress rotate" dip dup 0 == if break then
    loop drop length compress swap drop
    "length 1 -" bi@ rot 0 0 "'2array dip" '2array bi* swap 2array slice reverse
    : concat.(*)  concat ;
    'concat "'concat. apply" bi* eq 1 1 compress index collapse
    length if expand drop else drop 0 then ;    # r: position.
: end-with    'flip bi@ start-with ;
: start-with  'comb bi@ length rot swap iota subscript 'concat bi@ eq ;

"rosettacode" "rosetta" start-with .    # 1
"rosettacode" "taco" contains .         # 5
"rosettacode" "ocat" contains .         # 0
"rosettacode" "edoc" end-with .         # 0
"rosettacode" "code" contains .         # 7
```



## Liberty BASIC


```lb
'1---Determining if the first string starts with second string
st1$="first string"
st2$="first"
if left$(st1$,len(st2$))=st2$ then
    print "First string starts with second string."
end if

'2---Determining if the first string contains the second string at any location
'2.1---Print the location of the match for part 2
st1$="Mississippi"
st2$="i"
if instr(st1$,st2$) then
    print "First string contains second string."
    print "Second string is at location ";instr(st1$,st2$)
end if

'2.2---Handle multiple occurrences of a string for part 2.
pos=instr(st1$,st2$)
while pos
    count=count+1
    pos=instr(st1$,st2$,pos+len(st2$))
wend
print "Number of times second string appears in first string is ";count

'3---Determining if the first string ends with the second string
st1$="first string"
st2$="string"
if right$(st1$,len(st2$))=st2$ then
    print "First string ends with second string."
end if

```



## Lingo


```lingo
a = "Hello world!"
b = "Hello"

-- Determining if the first string starts with second string
put a starts b
-- 1

-- Determining if the first string contains the second string at any location
put a contains b
-- 1

-- Determining if the first string ends with the second string
put a.char[a.length-b.length+1..a.length] = b
-- 0

b = "world!"
put a.char[a.length-b.length+1..a.length] = b
-- 1

-- Print the location of the match for part 2
put offset(b, a)
-- 7
```



## Logo


```logo
to starts.with? :sub :thing
  if empty? :sub [output "true]
  if empty? :thing [output "false]
  if not equal? first :sub first :thing [output "false]
  output starts.with? butfirst :sub butfirst :thing
end

to ends.with? :sub :thing
  if empty? :sub [output "true]
  if empty? :thing [output "false]
  if not equal? last :sub last :thing [output "false]
  output ends.with? butlast :sub butlast :thing
end

show starts.with? "dog "doghouse    ; true
show ends.with? "house "doghouse    ; true
show substring? "gho "doghouse       ; true  (built-in)
```


## Lua


```lua
s1 = "string"
s2 = "str"
s3 = "ing"
s4 = "xyz"

print( "s1 starts with s2: ", string.find( s1, s2 ) == 1 )
print( "s1 starts with s3: ", string.find( s1, s3 ) == 1, "\n" )

print( "s1 contains s3: ", string.find( s1, s3 ) ~= nil )
print( "s1 contains s3: ", string.find( s1, s4 ) ~= nil, "\n" )

print( "s1 ends with s2: ", select( 2, string.find( s1, s2 ) ) == string.len( s1 ) )
print( "s1 ends with s3: ", select( 2, string.find( s1, s3 ) ) == string.len( s1 ) )
```


## M2000 Interpreter


```M2000 Interpreter

Module StringMatch {
      A$="Hello World"
      Print A$ ~ "Hello*"
      Print A$ ~ "*llo*"
      p=Instr(A$, "llo")
      Print p=3
      \\ Handle multiple occurance for "o"
      p=Instr(A$, "o")
      While p > 0 {
            Print "position:";p;{ for "o"}
            p=Instr(A$, "o", p+1)
      }
      Print A$ ~ "*orld"
}
```txt

    True
    True
    True
position:5 for "o"
position:8 for "o"
    True

```

StringMatch

```


## Maple

These facilities are all to be found in the StringTools package in Maple.

```Maple

> with( StringTools ): # bind package exports at the top-level
> s := "dzrIemaWWIMidXYZwGiqkOOn":
> s[1..4]; # pick a prefix
                                 "dzrI"

> IsPrefix( s[ 1 .. 4 ], s ); # check it
                                  true

> s[ -4 .. -1 ]; # pick a suffix
                                 "kOOn"

> IsSuffix( s[ -4 .. -1 ], s ); # check it
                                  true

> p := Search( "XYZ", s ); # find a substring
                                p := 14

> s[ p .. p + 2 ]; # check
                                 "XYZ"

> SearchAll( [ "WWI", "XYZ" ], s ); # search for multiple patterns
                            [8, 1], [14, 2]

> to 3 do s := cat( s, s ) end: length( s ); # build a longer string by repeated doubling
                                  192

> p := SearchAll( "XYZ", s ); # find all occurrences
                p := 14, 38, 62, 86, 110, 134, 158, 182

> {seq}( s[ i .. i + 2 ], i = p ); # check them
                                {"XYZ"}

```

The StringTools package also contains facilities for regular expression matching, but for fixed string patterns, the Search and SearchAll tools are much faster.


## Mathematica


```Mathematica
StartWith[x_, y_] := MemberQ[Flatten[StringPosition[x, y]], 1]
EndWith[x_, y_] :=  MemberQ[Flatten[StringPosition[x, y]], StringLength[x]]
StartWith["XYZaaabXYZaaaaXYZXYZ", "XYZ"]
EndWith["XYZaaabXYZaaaaXYZXYZ", "XYZ"]
StringPosition["XYZaaabXYZaaaaXYZXYZ", "XYZ"]
```

```txt
True
True
```



=={{header|MATLAB}} / {{header|Octave}}==

```Matlab

   % 1. Determining if the first string starts with second string
	strcmp(str1,str2,length(str2))
   % 2. Determining if the first string contains the second string at any location
	~isempty(strfind(s1,s2))
   % 3. Determining if the first string ends with the second string
	( (length(str1)>=length(str2)) && strcmp(str1(end+[1-length(str2):0]),str2) )

   % 1. Print the location of the match for part 2
	disp(strfind(s1,s2))
   % 2. Handle multiple occurrences of a string for part 2.
	ix = strfind(s1,s2);   % ix is a vector containing the starting positions of s2 within s1

```



## MiniScript

We first extend the built-in string class with three new methods, and then demonstrate their use on some sample strings.


```MiniScript
string.startsWith = function(s)
    return self.len >= s.len and s[:s.len] == s
end function

string.endsWith = function(s)
    return self.len >= s.len and s[-s.len:] == s
end function

string.findAll = function(s)
    result = []
    after = null
    while true
        foundPos = self.indexOf(s, after)
        if foundPos == null then return result
        result.push foundPos
        after = foundPos + s.len - 1
    end while
end function

first = "The brown dog jumped jumped and jumped"
second = "jumped"

firstQ = """" + first + """"  // (first string, in quotes)
secondQ = """" + second + """"
doesOrNot = [" does not ", " does "]

print firstQ + doesOrNot[first.startsWith(second)] + "start with " + secondQ
print

found = first.findAll(second)
if not found then
    print firstQ + " does not contain " + secondQ + " anywhere"
else
    for pos in found
        print firstQ + " is found at position " + pos + " in " + secondQ
    end for
end if
print

print firstQ + doesOrNot[first.endsWith(second)] + "end with " + secondQ
```

```txt

"The brown dog jumped jumped and jumped" does start with "jumped"

"The brown dog jumped jumped and jumped" is found at position 14 in "jumped"
"The brown dog jumped jumped and jumped" is found at position 21 in "jumped"
"The brown dog jumped jumped and jumped" is found at position 32 in "jumped"

"The brown dog jumped jumped and jumped" does end with "jumped"

```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols

lipsum = ''
x_ = 0
x_ = x_ + 1; lipsum[0] = x_; lipsum[x_] = 'Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.'
x_ = x_ + 1; lipsum[0] = x_; lipsum[x_] = lipsum[1].reverse

srch = ''
srch[1] = 'Lorem ipsum dolor sit amet'
srch[2] = 'consectetur adipisicing elit'
srch[3] = 'dolore magna aliqua.'

loop j_ = 1 to lipsum[0]
  x1 = lipsum[j_].pos(srch[1])
  x2 = lipsum[j_].pos(srch[2])
  x3 = lipsum[j_].lastpos(srch[3])

  report(x1 = 1, lipsum[j_], srch[1], 'Test string starts with search string:', 'Test string does not start with search string:')
  report(x2 > 0, lipsum[j_], srch[2], 'Search string located in test string at position:' x2,  'Search string not found within test string:')
  report(x3 \= srch[3].length, lipsum[j_], srch[3], 'Test string ends with search string:', 'Test string does not start with search string:')
  end j_

many = ''
x_ = 0
x_ = x_ + 1; many[0] = x_; many[x_] = 'How many times does "many times" occur in this string?'
x_ = x_ + 1; many[0] = x_; many[x_] = 'How often does "many times" occur in this string?'
x_ = x_ + 1; many[0] = x_; many[x_] = 'How often does it occur in this string?'
srch[4] = 'many times'

loop j_ = 1 to many[0]
  o_ = 0
  k_ = 0
  loop label dups until o_ = 0
    o_ = many[j_].pos(srch[4], o_ + 1)
    if o_ \= 0 then k_ = k_ + 1
    end dups
  report(k_ > 0, many[j_], srch[4], 'Number of times search string occurs:' k_, 'Search string not found')
  end j_

method report(state = boolean, ts, ss, testSuccess, testFailure) public static
  if state then say testSuccess
  else          say testFailure
  say '    Test string:' ts
  say '  Search string:' ss
  say

  return

```


----

## NewLISP


```NewLISP
(setq str "abcdefbcghi")

;; test if str starts with "ab"
(starts-with str "ab")

;; find "bc" inside str
(find "bc" str)

;; test if str ends with "ghi"
(ends-with str "ghi")

;; find all positions of pattern inside str
(define (find-all-pos pattern str)
  (let ((idx -1) (pos '()))
    (while (setq idx (find pattern str 0 (+ idx 1)))
      (push idx pos -1))))

(find-all-pos "bc" str)
```



## Nim


```nim
import strutils

var s: string = "The quick brown fox"
if startsWith(s, "The quick"):
   echo("Starts with: The quick")
if endsWith(s, "brown Fox"):
   echo("Ends with: brown fox")
var pos = find(s, " brown ")  # -1 if not found
if contains(s, " brown "):    # showing the contains() proc, but could use if pos!=-1:
   echo('"' & " brown " & '"' & " is located at position: " & $pos)
```

```txt
Starts with: The quick
Ends with: brown fox
" brown " is located at position: 9
```


=={{header|Objective-C}}==

```objc
[@"abcd" hasPrefix:@"ab"] //returns true
[@"abcd" hasSuffix:@"zn"] //returns false
int loc = [@"abab" rangeOfString:@"bb"].location //returns -1
loc = [@"abab" rangeOfString:@"ab"].location //returns 0
loc = [@"abab" rangeOfString:@"ab" options:0 range:NSMakeRange(loc+1, [@"abab" length]-(loc+1))].location //returns 2
```



## Objeck


```objeck

bundle Default {
  class Matching {
    function : Main(args : System.String[]) ~ Nil {
      "abcd"->StartsWith("ab")->PrintLine(); # returns true
      "abcd"->EndsWith("zn")->PrintLine(); # returns false
      ("abab"->Find("bb") <> -1)->PrintLine(); # returns false
      ("abab"->Find("ab") <> -1)->PrintLine(); # returns true
      loc := "abab"->Find("bb"); # returns -1
      loc := "abab"->Find("ab"); # returns 0
      loc := "abab"->Find("ab", loc + 1); # returns 2
    }
  }
}

```



## OCaml



```ocaml
let match1 s1 s2 =
  let len1 = String.length s1
  and len2 = String.length s2 in
  if len1 < len2 then false else
    let sub = String.sub s1 0 len2 in
    (sub = s2)
```


testing in the top-level:

 # match1 "Hello" "Hello World!" ;;
 - : bool = false
 # match1 "Hello World!" "Hello" ;;
 - : bool = true


```ocaml
let match2 s1 s2 =
  let len1 = String.length s1
  and len2 = String.length s2 in
  if len1 < len2 then false else
    let rec aux i =
      if i < 0 then false else
        let sub = String.sub s1 i len2 in
        if (sub = s2) then true else aux (pred i)
    in
    aux (len1 - len2)
```


 # match2 "It's raining, Hello World!" "umbrella" ;;
 - : bool = false
 # match2 "It's raining, Hello World!" "Hello" ;;
 - : bool = true


```ocaml
let match3 s1 s2 =
  let len1 = String.length s1
  and len2 = String.length s2 in
  if len1 < len2 then false else
    let sub = String.sub s1 (len1 - len2) len2 in
    (sub = s2)
```


 # match3 "Hello World" "Hello" ;;
 - : bool = false
 # match3 "Hello World" "World" ;;
 - : bool = true


```ocaml
let match2_loc s1 s2 =
  let len1 = String.length s1
  and len2 = String.length s2 in
  if len1 < len2 then (false, -1) else
    let rec aux i =
      if i < 0 then (false, -1) else
        let sub = String.sub s1 i len2 in
        if (sub = s2) then (true, i) else aux (pred i)
    in
    aux (len1 - len2)
```


 # match2_loc "The sun's shining, Hello World!" "raining" ;;
 - : bool * int = (false, -1)
 # match2_loc "The sun's shining, Hello World!" "shining" ;;
 - : bool * int = (true, 10)


```ocaml
let match2_num s1 s2 =
  let len1 = String.length s1
  and len2 = String.length s2 in
  if len1 < len2 then (false, 0) else
    let rec aux i n =
      if i < 0 then (n <> 0, n) else
        let sub = String.sub s1 i len2 in
        if (sub = s2)
        then aux (pred i) (succ n)
        else aux (pred i) (n)
    in
    aux (len1 - len2) 0
```


 # match2_num "This cloud looks like a camel, \
     that other cloud looks like a llama" "stone" ;;
 - : bool * int = (false, 0)
 # match2_num "This cloud looks like a camel, \
     that other cloud looks like a llama" "cloud" ;;
 - : bool * int = (true, 2)


## Oforth



```Oforth
: stringMatching(s1, s2)
| i |
   s2 isAllAt(s1, 1) ifTrue: [ System.Out s1 << " begins with " << s2 << cr ]
   s2 isAllAt(s1, s1 size s2 size - 1 + ) ifTrue: [ System.Out s1 << " ends with " << s2 << cr ]

   s1 indexOfAll(s2) ->i
   i ifNotNull: [ System.Out s1 << " includes " << s2 << " at position : " << i << cr ]

   "\nAll positions : " println
   1 ->i
   while (s1 indexOfAllFrom(s2, i) dup ->i notNull) [
      System.Out s1 << " includes " << s2 << " at position : " << i << cr
      i s2 size + ->i
      ] ;
```


```txt

> "arduinoardblobard", "ard" stringMatching
arduinoardblobard begins with ard
arduinoardblobard ends with ard
arduinoardblobard includes ard at position : 1

All positions :
arduinoardblobard includes ard at position : 1
arduinoardblobard includes ard at position : 8
arduinoardblobard includes ard at position : 15

```



## OxygenBasic


```oxygenbasic

string s="sdfkjhgsdfkdfgkbopefioqwurti487sdfkrglkjfs9wrtgjglsdfkdkjcnmmb.,msfjflkjsdfk"

string f="sdfk"

string cr=chr(13)+chr(10),tab=chr(9)

string pr="FIND STRING LOCATIONS" cr cr

sys a=0, b=1, count=0, ls=len(s), lf=len(f)

do
  a=instr b,s,f
  if a=0 then exit do
  count++
  if a=1 then  pr+="Begins with keyword" cr
  pr+=count tab a cr
  if a=ls-lf+1 then pr+="Ends with keyword at " a cr
  b=a+1
end do

pr+=cr "Total matches: " count cr

print pr

'FIND STRING LOCATIONS
'
'Begins with keyword
'1	1
'2	8
'3	32
'4	51
'5	73
'Ends with keyword at 73
'
'Total matches: 5

```



## PARI/GP

This meets the first but not the second of the optional requirements.  Note that GP treats any nonzero value as true so the location found by contains() can be ignore if not needed.

```parigp
startsWith(string, prefix)={
  string=Vec(string);
  prefix=Vec(prefix);
  if(#prefix > #string, return(0));
  for(i=1,#prefix,if(prefix[i]!=string[i], return(0)));
  1
};
contains(string, inner)={
  my(good);
  string=Vec(string);
  inner=Vec(inner);
  for(i=0,#string-#inner,
    good=1;
    for(j=1,#inner,
      if(inner[j]!=string[i+j], good=0; break)
    );
    if(good, return(i+1))
  );
  0
};
endsWith(string, suffix)={
  string=Vec(string);
  suffix=Vec(suffix);
  if(#suffix > #string, return(0));
  for(i=1,#suffix,if(prefix[i]!=string[i+#string-#suffix], return(0)));
  1
};
```



## Perl


Using regexes:


```perl
$str1 =~ /^\Q$str2\E/  # true if $str1 starts with $str2
$str1 =~ /\Q$str2\E/   # true if $str1 contains $str2
$str1 =~ /\Q$str2\E$/  # true if $str1 ends with $str2
```


Using <code>index</code>:


```perl
index($str1, $str2) == 0                               # true if $str1 starts with $str2
index($str1, $str2) != -1                              # true if $str1 contains $str2
rindex($str1, $str2) == length($str1) - length($str2)  # true if $str1 ends with $str2
```


Using <code>substr</code>:


```perl
substr($str1, 0, length($str2)) eq $str2  # true if $str1 starts with $str2
substr($str1, - length($str2)) eq $str2   # true if $str1 ends with $str2
```


Bonus task ''(printing all positions where <code>$str2</code> appears in <code>$str1</code>)'':


```perl
print $-[0], "\n" while $str1 =~ /\Q$str2\E/g;  # using a regex
```


```perl
my $i = -1; print $i, "\n" while ($i = index $str1, $str2, $i + 1) != -1;  # using index
```



## Perl 6


Using string methods:


```perl6
$haystack.starts-with($needle)  # True if $haystack starts with $needle
$haystack.contains($needle)     # True if $haystack contains $needle
$haystack.ends-with($needle)    # True if $haystack ends with $needle
```


Using regexes:


```perl6
so $haystack ~~ /^ $needle  /  # True if $haystack starts with $needle
so $haystack ~~ /  $needle  /  # True if $haystack contains $needle
so $haystack ~~ /  $needle $/  # True if $haystack ends with $needle
```


Using <code>substr</code>:


```perl6
substr($haystack, 0, $needle.chars) eq $needle  # True if $haystack starts with $needle
substr($haystack, *-$needle.chars) eq $needle   # True if $haystack ends with $needle
```


Bonus task:


```perl6
$haystack.match($needle, :g)».from;  # List of all positions where $needle appears in $haystack
```



## Phix


```Phix
constant word = "the",                                      -- (also try this with "th"/"he")
         sentence = "the last thing the man said was the"
--       sentence = "thelastthingthemansaidwasthe"          -- (practically the same results)

-- A common, but potentially inefficient idiom for checking for a substring at the start is:
if match(word,sentence)=1 then
    ?"yes(1)"
end if
-- A more efficient method is to test the appropriate slice
if length(sentence)>=length(word)
and sentence[1..length(word)]=word then
    ?"yes(2)"
end if
-- Which is almost identical to checking for a word at the end
if length(sentence)>=length(word)
and sentence[-length(word)..-1]=word then
    ?"yes(3)"
end if
-- Or sometimes you will see this, a tiny bit more efficient:
if length(sentence)>=length(word)
and match(word,sentence,length(sentence)-length(word)+1) then
    ?"yes(4)"
end if
-- Finding all occurences is a snap:
integer r = match(word,sentence)
while r!=0 do
    ?r
    r = match(word,sentence,r+1)
end while
```

```txt

"yes(1)"
"yes(2)"
"yes(3)"
"yes(4)"
1
16
33

```



## PHP


```php
<?php
/**********************************************************************************
* This program gets needle and haystack from the caller (chm.html) (see below)
* and checks for occurrences of the needle in the haystack
* 02.05.2013 Walter Pachl
* Comments or Suggestions welcome
**********************************************************************************/
$haystack = $_POST['haystack']; if ($haystack=='') {$haystack='no haystack given';}
$needle   = $_POST['needle'];   if ($needle=='')   {$needle='no needle given';}

function rexxpos($h,$n) {
  $pos = strpos($h,$n);
  if ($pos === false) { $pos=-1; }
  else                { $pos=$pos+1; }
  return ($pos);
 }

$pos=rexxpos($haystack,$needle);
$tx1 = "";
if ($pos==-1){ $n=0; }  // not found
else         { $n=1; }  // found once (so far)
// Special cases
if ($pos==1){ $tx1="needle found to be the start of the haystack"; }
if ($pos==strlen($haystack)-strlen($needle)+1)
            { $tx1="needle found at end of haystack"; }

if ($n>0) { // look for other occurrences
  $pl=$pos; // list of positions
  $p=$pos;  //
  $x="*************************************";
  $h=$haystack;
  while ($p>0) {
    $h=substr($x,0,$p).substr($h,$p);
    $p=rexxpos($h,$needle);
    if ( $p>0 ) { $n=$n+1; $pl=$pl.", ".$p; }
  }
       if ($n==1) { $txt="needle found once in haystack, position: $pl."; }
  else if ($n==2) { $txt="needle found twice in haystack, position(s): $pl."; }
  else            { $txt="needle found $n times in haystack, position(s): $pl."; }
}
else              { $txt="needle not found in haystack."; }
?>
<html>
  <head>
    <title>Character Matching</title>
    <meta name="author" content="Walter Pachl">
    <meta name="date" content="02.05.2013">
    <style>
      p { font: 120% courier; }
    </style>
  </head>
  <body>
    <p><strong>Haystack: '<?php echo "$haystack" ?>'</strong></p>
    <p><strong>Needle:   '<?php echo "$needle" ?>'</strong></p>
    <p><strong><?php echo "$txt" ?></strong></p>
    <!-- special message: -->
    <p  style="color: red";><strong><?php echo "$tx1" ?></strong></p>
  </body>
</html>
```


```php
<?php
<!DOCTYPE html>
<!--
/************************************************************************
* Here we prompt the user for a haystack and a needle
* We then invoke program chmx.php
* to check for occurrences of the needle in the haystack
* 02.05.2013 Walter Pachl
* Comments or Suggestions welcome
************************************************************************/
-->
<html>
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    <title>Character matching</title>
  </head>
  <body>
    <form id="test" name="test" method="post" action="chmx.php">
    <h1>Character matching</h1>
    <p>Given two strings, demonstrate the following 3 types of matchings:
    <ol style="margin-top:2; margin-bottom:2;">
    <li>Determining if the first string starts with second string
    <li>Determining if the first string contains the second string at any location
    <li>Determining if the first string ends with the second string
    </ol>
    <p>Optional requirements:
    <ol style="margin-top:2; margin-bottom:2;">
    <li>Print the location of the match(es) for part 2
    <li>Handle multiple occurrences of a string for part 2.
    </ol>
    <p style="margin-top:5; margin-bottom:3;">
       <font face="Courier"><strong>Haystack:</strong>
       <strong><input type="text" name="haystack" size="80"></strong></font></p>
    <p style="margin-top:5; margin-bottom:3;">
       <font face="Courier"><strong>Needle:  </strong>
       <strong><input type="text" name="needle" size="80"></strong></font></p>
    <p>Press <input name="Submit" type="submit" class="erfolg" value="CHECK"/>
       to invoke chmx.php.</p>
  </form>
  </body>
</html>
```



## PicoLisp


```PicoLisp
: (pre? "ab" "abcd")
-> "abcd"
: (pre? "xy" "abcd")
-> NIL

: (sub? "bc" "abcd")
-> "abcd"
: (sub? "xy" "abcd")
-> NIL

: (tail (chop "cd") (chop "abcd"))
-> ("c" "d")
: (tail (chop "xy") (chop "abcd"))
-> NIL


(de positions (Pat Str)
   (setq Pat (chop Pat))
   (make
      (for ((I . L) (chop Str) L (cdr L))
         (and (head Pat L) (link I)) ) ) )

: (positions "bc" "abcdabcd")
-> (2 6)
```



## PL/I


```PL/I

/*  Let s be one string, t be the other that might exist within s. */
                                                     /* 8-1-2011 */
k = index(s, t);
if k = 0 then put skip edit (t, ' is nowhere in sight') (a);
else if k = 1 then
   put skip edit (t, '  starts at the beginning of  ', s) (a);
else if k+length(t)-1 = length(s) then
   put skip edit (t, '  is at the end of  ', s) (a);
else put skip edit (t, '  is within  ', s) (a);

if k > 0 then put skip edit (t, '  starts at position  ', k) (a);

```


Optional extra:


```PL/I

/* Handle multiple occurrences. */
   n = 1;
   do forever;
      k = index(s, t, n);
      if k = 0 then
         do;
            if n = 1 then put skip list (t, ' is nowhere in sight');
            stop;
         end;
      else if k = 1 then
         put skip edit ('<', t, '> starts at the beginning of  ', s) (a);
      else if k+length(t)-1 = length(s) then
         put skip edit ('<', t, '> is at the end of  ', s) (a);
      else put skip edit ('<', t, '> is within  ', s) (a);
      n =   k + length(t);

      if k > 0 then
         put skip edit ('<', t, '> starts at position ', trim(k)) (a);
      else stop;
   end;

```



## PureBasic


```PureBasic
Procedure StartsWith(String1$, String2$)
  Protected Result
  If FindString(String1$, String2$, 1) =1 ; E.g Found in possition 1
    Result =CountString(String1$, String2$)
  EndIf
  ProcedureReturn Result
EndProcedure

Procedure EndsWith(String1$, String2$)
  Protected Result, dl=Len(String1$)-Len(String2$)
  If dl>=0 And Right(String1$, Len(String2$))=String2$
    Result =CountString(String1$, String2$)
  EndIf
  ProcedureReturn Result
EndProcedure
```

And a verification

```PureBasic
If OpenConsole()
  PrintN(Str(StartsWith("Rosettacode", "Rosetta")))           ; = 1
  PrintN(Str(StartsWith("Rosettacode", "code")))              ; = 0
  PrintN(Str(StartsWith("eleutherodactylus cruralis", "e")))  ; = 3
  PrintN(Str(EndsWith  ("Rosettacode", "Rosetta")))  ; = 0
  PrintN(Str(EndsWith  ("Rosettacode", "code")))     ; = 1
  PrintN(Str(EndsWith  ("Rosettacode", "e")))        ; = 2

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```


An alternate and more complete solution:

```PureBasic
Procedure startsWith(string1$, string2$)
  ;returns one if string1$ starts with string2$, otherwise returns zero
  If FindString(string1$, string2$, 1) = 1
    ProcedureReturn 1
  EndIf
  ProcedureReturn 0
EndProcedure

Procedure contains(string1$, string2$, location = 0)
  ;returns the location of the next occurrence of string2$ in string1$ starting from location,
  ;or zero if no remaining occurrences of string2$ are found in string1$
  ProcedureReturn FindString(string1$, string2$, location + 1)
EndProcedure

Procedure endsWith(string1$, string2$)
  ;returns one if string1$ ends with string2$, otherwise returns zero
  Protected ls = Len(string2$)
  If Len(string1$) - ls >= 0 And Right(string1$, ls) = string2$
    ProcedureReturn 1
  EndIf
  ProcedureReturn 0
EndProcedure

If OpenConsole()
  PrintN(Str(startsWith("RosettaCode", "Rosetta")))           ; = 1, true
  PrintN(Str(startsWith("RosettaCode", "Code")))              ; = 0, false

  PrintN("")
  PrintN(Str(contains("RosettaCode", "luck")))                ; = 0, no occurrences
  Define location
  Repeat
    location = contains("eleutherodactylus cruralis", "e", location)
    PrintN(Str(location))                                     ;display each occurrence: 1, 3, 7,  & 0 (no more occurrences)
  Until location = 0

  PrintN("")
  PrintN(Str(endsWith  ("RosettaCode", "Rosetta")))            ; = 0, false
  PrintN(Str(endsWith  ("RosettaCode", "Code")))               ; = 1, true

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

```txt
1
0

0
1
3
7
0

0
1
```



## PowerShell


```Powershell

"spicywiener".StartsWith("spicy")
"spicywiener".Contains("icy")
"spicywiener".EndsWith("wiener")
"spicywiener".IndexOf("icy")
[regex]::Matches("spicywiener", "i").count

```

```txt

True
True
True
2
2

```



## Python


```python
"abcd".startswith("ab") #returns True
"abcd".endswith("zn") #returns False
"bb" in "abab" #returns False
"ab" in "abab" #returns True
loc = "abab".find("bb") #returns -1
loc = "abab".find("ab") #returns 0
loc = "abab".find("ab",loc+1) #returns 2
```



## Racket


```racket

#lang racket
(require srfi/13)
(string-prefix? "ab" "abcd")
(string-suffix? "cd" "abcd")
(string-contains "abab" "bb")
(string-contains "abab" "ba")

```

```txt

#t
#t
#f
1

```




## Retro


```Retro
: startsWith? ( $1 $2 - f )
  withLength &swap dip 0 swap ^strings'getSubset compare ;

"abcdefghijkl" "abcde" startsWith?
"abcdefghijkl" "bcd" startsWith?

"abcdefghijkl" "bcd" ^strings'search
"abcdefghijkl" "zmq" ^strings'search

: endsWith? ( $1 $2 - f )
  swap withLength + over getLength - compare ;

"abcdefghijkl" "ijkl" endsWith?
"abcdefghijkl" "abc" endsWith?
```



## REXX

Extra coding was added to take care of using plurals in the last output message.

```rexx
/*REXX program  demonstrates  some  basic   character string   testing  (for matching). */
parse arg A B;                    LB=length(B)   /*obtain A and B from the command line.*/
say 'string  A  = '   A                          /*display string   A   to the terminal.*/
say 'string  B  = '   B                          /*   "       "     B    "  "      "    */
say copies('░', 70)
if left(A, LB)==B  then say  'string  A  starts with string  B'
                   else say  "string  A  doesn't start with string  B"
say                                              /* [↓] another method using COMPARE BIF*/
          /*╔══════════════════════════════════════════════════════════════════════════╗
            ║ if compare(A,B)==LB  then say  'string  A  starts with string  B'        ║
            ║                      else say  "string  A  doesn't start with string  B" ║
            ╚══════════════════════════════════════════════════════════════════════════╝*/
p=pos(B, A)
if p==0  then say  "string  A  doesn't contain string  B"
         else say  'string  A  contains string  B  (starting in position'   p")"
say
if right(A, LB)==b  then say 'string  A  ends with string  B'
                    else say "string  A  doesn't end with string  B"
say
$=;   p=0;                    do  until  p==0;        p=pos(B, A, p+1)
                              if p\==0  then $=$','   p
                              end   /*until*/
$=space(strip($, 'L', ","))                      /*elide extra blanks and leading comma.*/
#=words($)                                       /*obtain number of words in  $  string.*/
if #==0  then say "string  A  doesn't contain string  B"
         else say 'string  A  contains string  B '    #    " time"left('s', #>1),
                  "(at position"left('s',  #>1)   $")"  /*stick a fork in it, we're done*/
```

```txt

string  A  =  Chico_Harpo_Groucho_Zeppo_Gummo
string  B  =  p
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
string  A  doesn't start with string B

string  A  contains string  B  (starting in position 10)

string  A  doesn't end with string B

string  A  contains string  B  3  times (at positions 10, 23, 24)

```

```txt

string  A  =  Chico_Harpo_Groucho_Zeppo_Gummo
string  B  =  Z
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
string  A  doesn't start with string  B

string  A  contains string  B  (starting in position 21)

string  A  doesn't end with string  B

string  A  contains string  B  1  time (at position 21)

```

```txt

string  A  =  Chico_Harpo_Groucho_Zeppo_Gummo
string  B  =  Chi
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
string  A  starts with string  B

string  A  contains string B  (starting in position 1)

string  A  doesn't end with string  B

string  A  contains string  B  1  time (at position 1)

```

```txt

string  A  =  Chico_Harpo_Groucho_Zeppo_Gummo
string  B  =  mmo
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
string  A  doesn't start with string  B

string  A  contains string  B  (starting in position 29)

string  A  ends with string  B

string  A  contains string  B  1  time (at position 29)

```



## Ring


```ring

aString = "Welcome to the Ring Programming Language"
bString = "Ring"
bStringIndex = substr(aString,bString)
if bStringIndex > 0 see "" + bStringIndex + " : " + bString ok

```



## Ruby


```ruby
p 'abcd'.start_with?('ab')  #returns true
p 'abcd'.end_with?('ab')    #returns false
p 'abab'.include?('bb')     #returns false
p 'abab'.include?('ab')     #returns true
p 'abab'['bb']              #returns nil
p 'abab'['ab']              #returns "ab"
p 'abab'.index('bb')        #returns nil
p 'abab'.index('ab')        #returns 0
p 'abab'.index('ab', 1)     #returns 2
p 'abab'.rindex('ab')       #returns 2
```



## Run BASIC


```runbasic
s1$ = "abc def ghi klmnop"
s2$  = "abc"  ' begins with
s3$  = "ef"   ' is in the string
s4$  = "nop"  ' ends with

sn2$ = "abcx"  ' not begins with
sn3$ = "efx"   ' not in the string
sn4$ = "nopx"  ' not ends with

if left$(s1$,len(s2$)) <> s2$ then a$ = "Not "
print "String:";s1$;" does ";a$;"begin with:";s2$

if instr(s1$,s3$) = 0 then a$ = "Not "
print "String:";s1$;" does ";a$;"contain:";s3$

if mid$(s1$,len(s1$) + 1 - len(s4$),len(s4$)) <> s4$ then a$ = "Not "
print "String:";s1$;" does ";a$;"end with:";s4$

' ----------- not -----------------------------
print
if left$(s1$,len(sn2$)) <> sn2$ then a$ = "Not "
print "String:";s1$;" does ";a$;"begin with:";sn2$

if instr(s1$,sn3$) = 0 then a$ = "Not "
print "String:";s1$;" does ";a$;"contain:";sn3$

if mid$(s1$,len(s1$) + 1 - len(sn4$),len(sn4$)) <> sn4$ then a$ = "Not "
print "String:";s1$;" does ";a$;"end with:";sn4$
```

```txt

String:abc def ghi klmnop does begin with:abc
String:abc def ghi klmnop does contain:ef
String:abc def ghi klmnop does end with:nop

String:abc def ghi klmnop does Not begin with:abcx
String:abc def ghi klmnop does Not contain:efx
String:abc def ghi klmnop does Not end with:nopx
```



## Rust


```rust
fn print_match(possible_match: Option<usize>) {
    match possible_match {
        Some(match_pos) => println!("Found match at pos {}", match_pos),
        None => println!("Did not find any matches")
    }
}

fn main() {
    let s1 = "abcd";
    let s2 = "abab";
    let s3 = "ab";

    // Determining if the first string starts with second string
    assert!(s1.starts_with(s3));
    // Determining if the first string contains the second string at any location
    assert!(s1.contains(s3));
    // Print the location of the match
    print_match(s1.find(s3)); // Found match at pos 0
    print_match(s1.find(s2)); // Did not find any matches
    // Determining if the first string ends with the second string
    assert!(s2.ends_with(s3));
}
```



```rust

fn main(){
    let hello = String::from("Hello world");
    println!(" Start with \"he\" {} \n Ends with \"rd\" {}\n Contains \"wi\" {}",
                                                        hello.starts_with("He"),
                                                        hello.ends_with("ld"),
                                                        hello.contains("wi"));
}
```


 Start with "he" true
 Ends with "ld" true
 Contains "wi" false


## Scala


```scala
"abcd".startsWith("ab") //returns true
"abcd".endsWith("zn") //returns false
"abab".contains("bb") //returns false
"abab".contains("ab") //returns true

var loc="abab".indexOf("bb") //returns -1
loc = "abab".indexOf("ab") //returns 0
loc = "abab".indexOf("ab", loc+1) //returns 2
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: position is 0;
  begin
    writeln(startsWith("abcd", "ab")); # write TRUE
    writeln(endsWith("abcd", "zn"));   # write FALSE
    writeln(pos("abab", "bb") <> 0);   # write FALSE
    writeln(pos("abab", "ab") <> 0);   # write TRUE
    writeln(pos("abab", "bb"));        # write 0
    position := pos("abab", "ab");
    writeln(position);                 # position is 1
    position := pos("abab", "ab", succ(position));
    writeln(position);                 # position is 3
  end func;
```


```txt

TRUE
FALSE
FALSE
TRUE
0
1
3

```



## Sidef


```ruby
var first = "abc-abcdef-abcd";
var second = "abc";

say first.begins_with(second);      #=> true
say first.contains(second);         #=> true
say first.ends_with(second);        #=> false

# Get and print the location of the match
say first.index(second);            #=> 0

# Find multiple occurrences of a string
var pos = -1;
while (pos = first.index(second, pos+1) != -1) {
    say "Match at pos: #{pos}";
}
```



## Smalltalk


```smalltalk
a startsWith: b
a includesSubCollection: b
a endsWith: b
a indexOfSubCollection: b
a indexOfSubCollection: b startingAt: pos
```



## SNOBOL4


```SNOBOL4
      s1 = 'abcdabefgab'
      s2 = 'ab'
      s3 = 'xy'
      OUTPUT = ?(s1 ? POS(0) s2)  "1. " s2 " begins " s1
      OUTPUT = ?(s1 ? POS(0) s3)  "1. " s3 " begins " s1  ;# fails

      n = 0
again s1 POS(n) ARB s2 @a                        :F(p3)
      OUTPUT = "2. " s2 " found at position "
+        a - SIZE(s2) " in " s1
      n = a                                      :(again)

p3    OUTPUT = ?(s1 ? s2 RPOS(0)) "3. " s2 " ends " s1
END
```

```txt
1. ab begins abcdabefgab
2. ab found at position 0 in abcdabefgab
2. ab found at position 4 in abcdabefgab
2. ab found at position 9 in abcdabefgab
3. ab ends abcdabefgab
```



## Standard ML


```sml
String.isPrefix "ab" "abcd"; (* returns true *)
String.isSuffix "zn" "abcd"; (* returns false *)
String.isSubstring "bb" "abab"; (* returns false *)
String.isSubstring "ab" "abab"; (* returns true *)
#2 (Substring.base (#2 (Substring.position "bb" (Substring.full "abab")))); (* returns 4 *)
val loc = #2 (Substring.base (#2 (Substring.position "ab" (Substring.full "abab")))); (* returns 0 *)
val loc' = #2 (Substring.base (#2 (Substring.position "ab" (Substring.extract ("abab", loc+1, NONE))))); (* returns 2 *)
```



## Swift


```swift
var str = "Hello, playground"
str.hasPrefix("Hell")           //True
str.hasPrefix("hell")           //False

str.containsString("llo")       //True
str.containsString("xxoo")      //False

str.hasSuffix("playground")     //True
str.hasSuffix("world")          //False
```



## Tailspin


```tailspin

templates find@{s:}
  <'$s;.*'> '$; starts with $s;' !
  <'.*$s;'> '$; ends with $s;' !
  <'.*$s;.*'> '$; contains $s;' !
  <> '$s; cannot be found in $;' !
end find

'abcd' -> find@{s:'ab'} -> !OUT::write
'
' -> !OUT::write
'abcd' -> find@{s:'cd'} -> !OUT::write
'
' -> !OUT::write
'abcd' -> find@{s:'bc'} -> !OUT::write
'
' -> !OUT::write
'abcd' -> find@{s:'e'} -> !OUT::write

```

```txt

abcd starts with ab
abcd ends with cd
abcd contains bc
e cannot be found in abcd

```


In tailspin we don't manipulate strings by character indices but we can still work out the second part.

```tailspin

composer index@{s:}
  @index: 0;
  [<match>* -> (@index: $@index + $; $@index!)]  (<'.*'>)
  rule match: (def pos: [<~'$s;'>? ...] -> $::length + 1; <'(?=$s;).'>) $pos
end index

'ba is found in positions $:'banana' -> index@{s:'ba'}; in banana' -> !OUT::write
'
' -> !OUT::write
'ana is found in positions $:'banana' -> index@{s:'ana'}; in banana' -> !OUT::write
'
' -> !OUT::write
'c is found in positions $:'banana' -> index@{s:'c'}; in banana' -> !OUT::write

```

```txt

ba is found in positions [1] in banana
ana is found in positions [2, 4] in banana
c is found in positions [] in banana

```



## Tcl

In this code, we are looking in various ways for the string in the variable <tt>needle</tt> in the string in the variable <tt>haystack</tt>.

```tcl
set isPrefix    [string equal -length [string length $needle] $haystack $needle]
set isContained [expr {[string first $needle $haystack] >= 0}]
set isSuffix    [string equal $needle [string range $haystack end-[expr {[string length $needle]-1}] end]]
```


Of course, in the cases where the needle is a glob-safe string (i.e., doesn't have any of the characters “<tt>*?[\</tt>” in), this can be written far more conveniently:

```tcl
set isPrefix    [string match  $needle* $haystack]
set isContained [string match *$needle* $haystack]
set isSuffix    [string match *$needle  $haystack]
```


Another powerful technique is to use the regular expression engine in literal string mode:

```tcl
set isContained [regexp ***=$needle $haystack]
```

This can be extended by getting the <code>regexp</code> to return the locations of the matches, enabling the other forms of match to be done:

```tcl
set matchLocations [regexp -indices -all -inline ***=$needle $haystack]
# Each match location is a pair, being the index into the string where the needle started
# to match and the index where the needle finished matching

set isContained [expr {[llength $matchLocations] > 0}]
set isPrefix [expr {[lindex $matchLocations 0 0] == 0}]
set isSuffix [expr {[lindex $matchLocations end 1] == [string length $haystack]-1}]
set firstMatchStart [lindex $matchLocations 0 0]
puts "Found \"$needle\" in \"$haystack\" at $firstMatchStart"
foreach location $matchLocations {
    puts "needle matched at index [lindex $location 0]"
}
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
ASK "string1", string1=""
ASK "string2", string2=""

IF (string1.sw.string2)   THEN
PRINT string1," starts with     ",string2
ELSE
PRINT string1," not starts with ",string2
ENDIF
SET beg=STRING (string1,string2,0,0,0,end)
IF (beg!=0) THEN
PRINT string1," contains        ",string2
PRINT "  starting in position ",beg
PRINT "  ending   in position ",end
ELSE
PRINT string1," not contains    ",string2
ENDIF

IF (string1.ew.string2) THEN
PRINT string1," ends with       ",string2
ELSE
PRINT string1," not ends with   ",string2
ENDIF

```

```txt
string1 >Rosetta Code
string2 >Code
Rosetta Code not starts with Code
Rosetta Code contains        Code
  starting in position 9
  ending   in position 13
Rosetta Code ends with       Code

```



## TXR



### TXR Lisp



```txrlisp
(tree-case *args*
  ((big small)
   (cond
     ((< (length big) (length small))
      (put-line `@big is shorter than @small`))
     ((str= big small)
      (put-line `@big and @small are equal`))
     ((starts-with small big)
      (put-line `@small is a prefix of @big`))
     ((ends-with small big)
      (put-line `@small is a suffix of @big`))
     (t (iflet ((pos (search-str big small)))
          (put-line `@small occurs in @big at position @pos`)
          (put-line `@small does not occur in @big`)))))
  (otherwise
    (put-line `usage: @(ldiff *full-args* *args*) <bigstring> <smallstring>`)))
```

```txt
$ txr cmatch2.tl x
usage: txr cmatch2.tl <bigstring> <smallstring>
$ txr cmatch2.tl x y z
usage: txr cmatch2.tl <bigstring> <smallstring>
$ txr cmatch2.tl catalog cat
cat is a prefix of catalog
$ txr cmatch2.tl catalog log
log is a suffix of catalog
$ txr cmatch2.tl catalog at
at occurs in catalog at position 1
$ txr cmatch2.tl catalog catalogue
catalog is shorter than catalogue
$ txr cmatch2.tl catalog catalog
catalog and catalog are equal
$ txr cmatch2.tl catalog dog
dog does not occur in catalog
```



### Pattern Language



```txr
@line
@(cases)
@  line
@  (output)
second line is the same as first line
@  (end)
@(or)
@  (skip)@line
@  (output)
first line is a suffix of the second line
@  (end)
@(or)
@  line@(skip)
@  (output)
first line is a suffix of the second line
@  (end)
@(or)
@  prefix@line@(skip)
@  (output)
first line is embedded in the second line at position @(length prefix)
@  (end)
@(or)
@  (output)
first line is not found in the second line
@  (end)
@(end)
```


```txt
$ txr cmatch.txr -
123
01234
first line is embedded in the second line at position 1
$ txr cmatch.txr -
123
0123
first line is a suffix of the second line
```



## VBA

```vb
Public Sub string_matching()
    word = "the"                                        '-- (also try this with "th"/"he")
    sentence = "the last thing the man said was the"
    '--       sentence = "thelastthingthemansaidwasthe" '-- (practically the same results)

    '-- A common, but potentially inefficient idiom for checking for a substring at the start is:
    If InStr(1, sentence, word) = 1 Then
        Debug.Print "yes(1)"
    End If
    '-- A more efficient method is to test the appropriate slice
    If Len(sentence) >= Len(word) _
        And Mid(sentence, 1, Len(word)) = word Then
        Debug.Print "yes(2)"
    End If
    '-- Which is almost identical to checking for a word at the end
    If Len(sentence) >= Len(word) _
        And Mid(sentence, Len(sentence) - Len(word) + 1, Len(word)) = word Then
        Debug.Print "yes(3)"
    End If
    '-- Or sometimes you will see this, a tiny bit more efficient:
    If Len(sentence) >= Len(word) _
    And InStr(Len(sentence) - Len(word) + 1, sentence, word) Then
        Debug.Print "yes(4)"
    End If
    '-- Finding all occurences is a snap:
    r = InStr(1, sentence, word)
    Do While r <> 0
        Debug.Print r
        r = InStr(r + 1, sentence, word)
    Loop
End Sub
```
```txt
yes(1)
yes(2)
yes(3)
yes(4)
 1
 16
 33
```


## VBScript


```vb
Function StartsWith(s1,s2)
	StartsWith = False
	If Left(s1,Len(s2)) = s2 Then
		StartsWith = True
	End If
End Function

Function Contains(s1,s2)
	Contains = False
	If InStr(1,s1,s2) Then
		Contains = True & " at positions "
		j = 1
		Do Until InStr(j,s1,s2) = False
			Contains = Contains & InStr(j,s1,s2) & ", "
			If j = 1 Then
				If Len(s2) = 1 Then
					j = j + InStr(j,s1,s2)
				Else
					j = j + (InStr(j,s1,s2) + (Len(s2) - 1))
				End If
			Else
				If Len(s2) = 1 Then
					j = j + ((InStr(j,s1,s2) - j) + 1)
				Else
					j = j + ((InStr(j,s1,s2) - j) + (Len(s2) - 1))
				End If
			End If
		Loop
	End If
End Function

Function EndsWith(s1,s2)
	EndsWith = False
	If Right(s1,Len(s2)) = s2 Then
		EndsWith = True
	End If
End Function

WScript.StdOut.Write "Starts with test, 'foo' in 'foobar': " & StartsWith("foobar","foo")
WScript.StdOut.WriteLine
WScript.StdOut.Write "Contains test, 'o' in 'fooooobar': " & Contains("fooooobar","o")
WScript.StdOut.WriteLine
WScript.StdOut.Write "Ends with test, 'bar' in 'foobar': " & EndsWith("foobar","bar")
```


```txt


Starts with test, 'foo' in 'foobar': True
Contains test, 'o' in 'fooooobar': True at positions 2, 3, 4, 5, 6,
Ends with test, 'bar' in 'foobar': True

```



## Visual Basic

works the same as in VBA, see [[String_matching#VBA]]


## XPL0


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations
string 0;               \use zero-terminated strings

func StrLen(A);         \Return number of characters in a string
char A;
int  I;
for I:= 0 to -1>>1-1 do
    if A(I) = 0 then return I;

func StrFind(A, B);     \Search for string B in string A
\Returns index of first occurrence of string B in A, or -1 if B is not found
char A, B;              \strings to be compared
int  LA, LB, I, J;
[LA:= StrLen(A);
 LB:= StrLen(B);
 for I:= 0 to LA-LB do
    [for J:= 0 to LB-1 do
        if B(J) # A(J+I) then J:= LB+1;
    if J = LB then return I;    \found
    ];
return -1;
];

char Str;  int I, J;
[Str:= "pack my box with";
\       0123456789012345
Text(0, if StrFind(Str, "pack") = 0 then "yes" else "no");  CrLf(0);    \1.
Text(0, if StrFind(Str,  "ack") = 0 then "yes" else "no");  CrLf(0);
I:= StrFind(Str, "x w");
Text(0, if I >= 0 then "yes" else "no");                                \2.
Text(0, ", at offset ");  IntOut(0, I);  CrLf(0);
I:= 0;  J:= 0;          \offsets of space characters
loop    [I:= StrFind(Str+J, " ");
        if I < 0 then quit;
        IntOut(0, I+J); ChOut(0, ^ );
        J:= J+I+1;
        ];
CrLf(0);
Text(0, if StrFind(Str, "X w") >= 0 then "yes" else "no");  CrLf(0);
Text(0, if StrFind(Str, "with") = StrLen(Str)-StrLen("with") then "yes" else "no");  CrLf(0); \3.
Text(0, if StrFind(Str, "x w" ) = StrLen(Str)-StrLen("x w" ) then "yes" else "no");  CrLf(0);
]
```


```txt

yes
no
yes, at offset 10
4 7 11
no
yes
no

```



## zkl


```zkl
fcn f(text,p){ if(text.find(p)==0)println("Yep") else println("Nope") }
f("foobar","foo") //--> Yep
f("foobar","bar") //--> Nope
```


```zkl
fcn f(text,p){ if(Void!=(n:=text.find(p)))println("Contained @",n) else println("Nope") }
f("foobar","ob")   //--> Contained @2
f("foobar","food") //--> Nope
```


```zkl
fcn f(text,p){
   if( Void!=(n:=text.rfind(p)) and n+p.len()==text.len() )
      println("tail gunner") else println("Nope")
}
f("foobar","r"); f("foobar","ar"); //--> tail gunners
f("foobar","ob");  //--> Nope
f("foobarfoobar","bar"); //--> tail gunner
```


