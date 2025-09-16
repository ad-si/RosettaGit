+++
title = "Leap year"
description = ""
date = 2019-10-13T18:22:25Z
aliases = []
[extra]
id = 7782
[taxonomies]
categories = ["task", "Date and time"]
tags = []
languages = [
  "360_assembly",
  "68000_assembly",
  "actionscript",
  "ada",
  "algol_68",
  "algol_w",
  "apl",
  "applescript",
  "arc",
  "autohotkey",
  "autoit",
  "awk",
  "bacon",
  "bash",
  "basic",
  "batch_file",
  "bbc_basic",
  "befunge",
  "bracmat",
  "c",
  "clipper",
  "clojure",
  "cobol",
  "common_lisp",
  "component_pascal",
  "cpp",
  "csharp",
  "d",
  "dart",
  "dwscript",
  "dyalect",
  "ela",
  "elixir",
  "emacs_lisp",
  "erlang",
  "erre",
  "euphoria",
  "excel",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "futurebasic",
  "gambas",
  "gap",
  "genie",
  "go",
  "groovy",
  "harbour",
  "haskell",
  "hy",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "k",
  "kotlin",
  "lasso",
  "liberty_basic",
  "lingo",
  "livecode",
  "llvm",
  "logo",
  "logtalk",
  "lolcode",
  "lua",
  "maple",
  "mathematica",
  "maxima",
  "mercury",
  "miniscript",
  "mips_assembly",
  "mumps",
  "neko",
  "nemerle",
  "netrexx",
  "objeck",
  "ocaml",
  "oforth",
  "oorexx",
  "openedge_progress",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "postscript",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "q",
  "r",
  "racket",
  "raven",
  "rebol",
  "retro",
  "rexx",
  "ring",
  "rpg",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "sinclair_zx81_basic",
  "smalltalk",
  "snobol4",
  "stata",
  "swift",
  "tcl",
  "tuscript",
  "ubasic_4th",
  "unix_shell",
  "ursa",
  "vala",
  "vba",
  "vbscript",
  "vedit_macro_language",
  "visual_basic",
  "visual_basic_dotnet",
  "wdte",
  "wortel",
  "x86_assembly",
  "xlisp",
  "xpl0",
  "yorick",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task

Determine whether a given year is a leap year in the Gregorian calendar.


## See also

* [[wp:Leap year|Leap year (wiki)]]





## 360 Assembly


This is a callable subroutine to determine whether or not a given zoned-decimal 4-digit year is a Leap Year.
Leap years are "evenly divisible" by 4, except those which end in '00' and are not evenly divisible by 400.
The subroutine receives two parameters:
  (1) a 4-digit year (CCYY)
  (2) an 8-byte work area
The value returned in Register 15 (by convention the "return code") indicates whether the year is a Leap Year:
  When R15 = zero, the year is a leap year.
  Otherwise it is not.

```360 Assembly

LPCK CSECT
     USING LPCK,15
     STM  0,12,20(13)   STORE CALLER REGS
     LM   1,2,0(1)      R1 -> CCYY, R2 -> DOUBLE-WORD WORK AREA
     PACK 0(8,2),0(4,1) PACK CCYY INTO WORK AREA
     CVB  0,0(2)        CONVERT TO BINARY (R0 = CCYY)
     SRDL 0,32          R0|R1 = CCYY
     LA   2,100         R2 = 100
     DR   0,2           DIVIDE CCYY BY 100: R0 = YY, R1 = CC
     LTR  0,0           YY = 0? IF CCYY DIV BY 100, LY IFF DIV BY 400
     BZ   A               YES: R0|R1 = CC; CCYY DIV BY 100, TEST CC
     SRDL 0,32            NO: R0|R1 = YY; CCYY NOT DIV BY 100, TEST YY
A    LA   2,4           DIVISOR = 4; DIVIDEND = YY, OR DIV BY 100 CC
     DR   0,2           DIVIDE BY 4: R0 = REMAINDER, R1 = QUOTIENT
     LR   15,0          LOAD REMAINDER: IF 0, THEN LEAP YEAR
     LM   0,12,20(13)   RESTORE REGS
     BR   14
     END

```


Sample invocation from a COBOL program:

WORKING-STORAGE SECTION.
01  FILLER.
    05 YEAR-VALUE PIC 9(4).
    05 WKAREA PIC X(8).

PROCEDURE DIVISION.

    MOVE 1936 TO YEAR-VALUE
    CALL 'LPCK' USING YEAR-VALUE, WKAREA
    PERFORM RESULT-DISPLAY
    MOVE 1900 TO YEAR-VALUE
    CALL 'LPCK' USING YEAR-VALUE, WKAREA
    PERFORM RESULT-DISPLAY
    GOBACK.

RESULT-DISPLAY.

    IF RETURN-CODE = ZERO DISPLAY YEAR-VALUE ' IS A LEAP YEAR'
    ELSE DISPLAY YEAR-VALUE ' IS NOT A LEAP YEAR'.


## 68000 Assembly



```68000 Assembly
;Example
        move.l  #2018,d0
        bsr     leap_year
        addi.l  #28,d1      ; # days in February 2018
        rts
; Leap Year
;
; Input
;   d0=year
;
; Output
;   d1=1 if leap year, 0 if not leap year
;   zero flag clear if leap year, set if not
;
leap_year:
        cmpi.l  #1752,d0
        ble.s   not_leap_year

        move.l  d0,d1
        lsr.l   #1,d1
        bcs.s   not_leap_year
        lsr.l   #1,d1
        bcs.s   not_leap_year

; If we got here, year is divisible by 4.
        move.l  d0,d1
        divu    #100,d1
        swap    d1
        tst.w   d1
        bne.s   is_leap_year

; If we got here, year is divisible by 100.
        move.l  d0,d1
        divu    #400,d1
        swap    d1
        tst.w   d1
        bne.s   not_leap_year

is_leap_year:
        moveq.l #1,d1
        rts
not_leap_year:
        moveq.l #0,d1
        rts

```



## ActionScript


```actionscript
public function isLeapYear(year:int):Boolean {
    if (year % 100 == 0) {
        return (year % 400 == 0);
    }
    return (year % 4 == 0);
}
```



## Ada


```Ada
-- Incomplete code, just a sniplet to do the task. Can be used in any package or method.
-- Adjust the type of Year if you use a different one.
function Is_Leap_Year (Year : Integer) return Boolean is
begin
   if Year rem 100 = 0 then
      return Year rem 400 = 0;
   else
      return Year rem 4 = 0;
   end if;
end Is_Leap_Year;


-- An enhanced, more efficient version:
-- This version only does the 2 bit comparison (rem 4) if false.
-- It then checks rem 16 (a 4 bit comparison), and only if those are not
-- conclusive, calls rem 100, which is the most expensive operation.
-- I failed to be convinced of the accuracy of the algorithm at first,
-- so I rephrased it below.
-- FYI: 400 is evenly divisible by 16 whereas 100,200 and 300 are not. Ergo, the
-- set of integers evenly divisible by 16 and 100 are all evenly divisible by 400.
-- 1. If a year is not divisible by 4 => not a leap year. Skip other checks.
-- 2. If a year is evenly divisible by 16, it is either evenly divisible by 400 or
--    not evenly divisible by 100 => leap year. Skip further checks.
-- 3. If a year evenly divisible by 100 => not a leap year.
-- 4. Otherwise a leap year.

function Is_Leap_Year (Year : Integer) return Boolean is
begin
   return (Year rem 4 = 0) and then ((Year rem 16 = 0) or else (Year rem 100 /= 0));
end Is_Leap_Year;



-- To improve speed a bit more, use with
pragma Inline (Is_Leap_Year);
```



## ALGOL 68

```algol68
MODE YEAR = INT, MONTH = INT, DAY = INT;

PROC year days = (YEAR year)DAY: # Ignore 1752 CE for the moment #
  ( month days(year, 2) = 28 | 365 | 366 );

PROC month days = (YEAR year, MONTH month) DAY:
  ( month | 31,
            28 + ABS (year MOD 4 = 0 AND year MOD 100 /= 0 OR year MOD 400 = 0),
            31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

PROC is leap year = (YEAR year)BOOL: year days(year)=366;

test:(
  []INT test cases = (1900, 1994, 1996, 1997, 2000);
  FOR i TO UPB test cases DO
    YEAR year = test cases[i];
    printf(($g(0)" is "b("","not ")"a leap year."l$, year, is leap year(year)))
  OD
)
```

```txt

1900 is not a leap year.
1994 is not a leap year.
1996 is a leap year.
1997 is not a leap year.
2000 is a leap year.

```


=={{header|Algol-M}}==

```algol

BEGIN

% COMPUTE P MOD Q %
INTEGER FUNCTION MOD(P, Q);
INTEGER P, Q;
BEGIN
  MOD := P - Q * (P / Q);
END;

% RETURN NON-ZERO IF Y IS A LEAP YEAR %
INTEGER FUNCTION ISLEAP(Y);
INTEGER Y;
BEGIN
  IF MOD(Y,4) <> 0 THEN    % QUICK EXIT IN MOST CASES %
     ISLEAP := 0
  ELSE IF MOD(Y,400) = 0 THEN
     ISLEAP := 1
  ELSE IF MOD(Y,100) = 0 THEN
     ISLEAP := 0
  ELSE                     % NON-CENTURY DIVISIBLE BY 4 %
     ISLEAP := 1;
END;

% EXERCISE THE FUNCTION %
INTEGER Y;
WRITE("TEST OF CENTURY YEARS");
FOR Y := 1600 STEP 100 UNTIL 2000 DO
  BEGIN
    IF ISLEAP(Y) <> 0 THEN
      WRITE(Y, " IS A LEAP YEAR")
    ELSE
      WRITE(Y, " IS NOT A LEAP YEAR");
  END;
WRITE("TEST OF CURRENT DECADE");
FOR Y := 2010 STEP 1 UNTIL 2020 DO
  BEGIN
    IF ISLEAP(Y) <> 0 THEN
      WRITE(Y, " IS A LEAP YEAR")
    ELSE
      WRITE(Y, " IS NOT A LEAP YEAR");
  END;

END

```

```txt

TEST OF CENTURY YEARS
  1600 IS A LEAP YEAR
  1700 IS NOT A LEAP YEAR
  1800 IS NOT A LEAP YEAR
  1900 IS NOT A LEAP YEAR
  2000 IS A LEAP YEAR
TEST OF CURRENT DECADE
  2010 IS NOT A LEAP YEAR
  2011 IS NOT A LEAP YEAR
  2012 IS A LEAP YEAR
  2013 IS NOT A LEAP YEAR
  2014 IS NOT A LEAP YEAR
  2015 IS NOT A LEAP YEAR
  2016 IS A LEAP YEAR
  2017 IS NOT A LEAP YEAR
  2018 IS NOT A LEAP YEAR
  2019 IS NOT A LEAP YEAR
  2020 IS A LEAP YEAR

```



## ALGOL W


```algolw
begin
    % returns true if year is a leap year, false otherwise %
    % assumes year is in the Gregorian Calendar            %
    logical procedure isLeapYear ( integer value year ) ;
        year rem 400 = 0 or ( year rem 4 = 0 and year rem 100 not = 0 );

    % some test cases                                      %
    for year := 1899, 1900, 1901, 1902, 1903, 1904, 1905, 1999, 2000, 2001, 2002, 2003, 2004 do begin
        write( i_w := 1, s_w := 0
             , year
             , " is "
             , if isLeapYear( year ) then "" else "not "
             , " a leap year"
             )
    end for_year
end.
```


## APL


```apl

⍝returns 1 if leapyear, 0 otherwise:
∇Z←LEAPYEAR YEAR
Z←(0=4|YEAR)∧(0=400|YEAR)∨~0=100|YEAR
∇

```



## AppleScript


```applescript
on leap_year(y)
    return year mod 4 is equal to 0 and (year mod 100 is not equal to 0 or year mod 400 is equal to 0)
end leap_year

leap_year(1900)
```



## Arc


```arc

(= leap? (fn (year)
  (if (and (is 0 (mod year 4)) (isnt 0 (mod year 100))) year
      (unless (< 0 (+ (mod year 100) (mod year 400))) year))))

```

<b>Output:</b>

```arc

(map [leap? _] '(1900 1904 2000 2019 2020 2100))
;; =>          '(     1904 2000      2020     )

```


## AutoHotkey


```autohotkey
leapyear(year)
{
    if (Mod(year, 100) = 0)
        return (Mod(year, 400) = 0)
    return (Mod(year, 4) = 0)
}

MsgBox, % leapyear(1604)
```

```txt
Returns 1 if year is a leap year
```

or

```autohotkey
IsLeapYear(Year)
{
    return !Mod(Year, 4) && Mod(Year, 100) || !Mod(Year, 400)
}

MsgBox % "The year 1604 was " (IsLeapYear(1604) ? "" : "not ") "a leap year"
```

```txt
The year 1600 was a leap year
The year 1601 was not a leap year
The year 1604 was a leap year
```



## AutoIt


```AutoIt
; AutoIt Version: 3.3.8.1
$Year = 2012
$sNot = " not"

If IsLeapYear($Year) Then $sNot = ""
ConsoleWrite ($Year & " is" & $sNot & " a leap year." & @LF)

Func IsLeapYear($_year)
	Return Not Mod($_year, 4) And (Mod($_year, 100) Or Not Mod($_year, 400))
EndFunc

; == But it exists the standard UDF "Date.au3" with this function: "_IsLeapYear($Year)"

```

```txt

2012 is a leap year.

```

--[[User:BugFix|BugFix]] ([[User talk:BugFix|talk]]) 16:18, 16 November 2013 (UTC)


## AWK


```AWK
function leapyear( year )
{
    if ( year % 100 == 0 )
        return ( year % 400 == 0 )
    else
        return ( year % 4 == 0 )
}
```




## Bash


```Bash
#!/bin/bash
year=$(date +"%Y")
if (( $year % 4 == 0 ))
then
        if (( $year % 400 == 0 ))
        then
                echo "This is a leap year"
        else
                if (( $year % 100 == 0 ))
                then
                        echo "This is not a leap year"
                else
                        echo "This is a leap year"
                fi
        fi
else
        echo "This is not a leap year"
fi

```



## BASIC

Note that the <code>year%</code> function is not needed for most modern BASICs.

```qbasic
DECLARE FUNCTION diy% (y AS INTEGER)
DECLARE FUNCTION isLeapYear% (yr AS INTEGER)
DECLARE FUNCTION year% (date AS STRING)

PRINT isLeapYear(year(DATE$))

FUNCTION diy% (y AS INTEGER)
    IF y MOD 4 THEN
        diy = 365
    ELSEIF y MOD 100 THEN
        diy = 366
    ELSEIF y MOD 400 THEN
        diy = 365
    ELSE
        diy = 366
    END IF
END FUNCTION

FUNCTION isLeapYear% (yr AS INTEGER)
    isLeapYear = (366 = diy(yr))
END FUNCTION

FUNCTION year% (date AS STRING)
    year% = VAL(RIGHT$(date, 4))
END FUNCTION
```


An old-timey solution:

```BASIC
10 DEF FNLY(Y)=(Y/4=INT(Y/4))*((Y/100<>INT(Y/100))+(Y/400=INT(Y/400)))
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Leapyear.bas"
110 FOR I=1990 TO 2020
120   IF LEAPY(I) THEN
130     PRINT I;"is a leap year."
140   ELSE
150     PRINT I;"is not a leap year."
160   END IF
170 NEXT
180 DEF LEAPY(Y)=MOD(Y,4)=0 AND MOD(Y,100) OR MOD(Y,400)=0
```


=
## Sinclair ZX81 BASIC
=
ZX81 BASIC does not support user-defined functions, even the single-expression functions that are provided by many contemporary dialects; so we have to fake it using a subroutine and pass everything in global variables.

```basic
5000 LET L=Y/4=INT (Y/4) AND (Y/100<>INT (Y/100) OR Y/400=INT (Y/400))
5010 RETURN
```

An example showing how to call it:

```basic
10 INPUT Y
20 GOSUB 5000
30 PRINT Y;" IS ";
40 IF NOT L THEN PRINT "NOT ";
50 PRINT "A LEAP YEAR."
60 STOP
```


=
## ZX Spectrum Basic
=

```zxbasic
10 DEF FN l(y)=y/4=INT (y/4) AND y/100<>INT (y/100) OR y/400=INT (y/400)
```


=
## BaCon
=
From the Ada shortcut calculation

```qbasic
' Leap year
FUNCTION leapyear(NUMBER y) TYPE NUMBER
   RETURN IIF(MOD(y, 4) = 0, IIF(MOD(y, 16) = 0, IIF(MOD(y, 100) != 0, TRUE, FALSE), TRUE), FALSE)
END FUNCTION

READ y
WHILE y != 0
    PRINT y, ": ", IIF$(leapyear(y), "", "not a "), "leapyear"
    READ y
WEND

DATA 1600, 1700, 1800, 1900, 1901, 1996, 2000, 2001, 2004, 0
```


```txt
1600: not a leapyear
1700: leapyear
1800: leapyear
1900: leapyear
1901: not a leapyear
1996: leapyear
2000: not a leapyear
2001: not a leapyear
2004: leapyear

```



## Batch File


```dos
@echo off

::The Main Thing...
for %%x in (1900 2046 2012 1600 1800 2031 1952) do (
	call :leap %%x
)
echo.
pause
exit/b
::/The Main Thing...

::The Function...
:leap
set year=%1
set /a op1=%year%%%4
set /a op2=%year%%%100
set /a op3=%year%%%400
if not "%op1%"=="0" (goto :no)
if not "%op2%"=="0" (goto :yes)
if not "%op3%"=="0" (goto :no)
:yes
echo.
echo %year% is a leap year.
goto :EOF
:no
echo.
echo %year% is NOT a leap year.
goto :EOF
::/The Function...
```

```txt
1900 is NOT a leap year.

2046 is NOT a leap year.

2012 is a leap year.

1600 is a leap year.

1800 is NOT a leap year.

2031 is NOT a leap year.

1952 is a leap year.

Press any key to continue . . .
```



## BBC BASIC


```bbcbasic
      REPEAT
        INPUT "Enter a year: " year%
        IF FNleap(year%) THEN
          PRINT ;year% " is a leap year"
        ELSE
          PRINT ;year% " is not a leap year"
        ENDIF
      UNTIL FALSE
      END

      DEF FNleap(yr%)
      = (yr% MOD 4 = 0) AND ((yr% MOD 400 = 0) OR (yr% MOD 100 <> 0))
```



## Befunge

```befunge
0"2("*:3-:1-:2-:"^"-v<
v*%"d"\!%4::,,"is".:<|
>\45*:*%!+#v_ "ton"vv<
v"ear."+550<,,,,*84<$#
>"y pael a ">:#,_$:#@^
```


```txt
1900 is not a leap year.
1994 is not a leap year.
1996 is a leap year.
1997 is not a leap year.
2000 is a leap year.
```



## Bracmat


```bracmat
  ( leap-year
  =
    .     mod$(!arg.100):0
        & `(mod$(!arg.400):0) { The backtick skips the remainder of the OR operation,
                                even if the tested condition fails. }
      | mod$(!arg.4):0
  )
& 1600 1700 1899 1900 2000 2006 2012:?tests
&   whl
  ' ( !tests:%?test ?tests
    & ( leap-year$!test&out$(!test " is a leap year")
      | out$(!test " is not a leap year")
      )
    )
& ;
```

```txt
1600  is a leap year
1700  is not a leap year
1899  is not a leap year
1900  is not a leap year
2000  is a leap year
2006  is not a leap year
2012  is a leap year
```



## C


```c
#include <stdio.h>

int is_leap_year(int year)
{
    return (!(year % 4) && year % 100 || !(year % 400)) ? 1 : 0;
}

int main()
{
    int test_case[] = {1900, 1994, 1996, 1997, 2000}, key, end, year;
    for (key = 0, end = sizeof(test_case)/sizeof(test_case[0]); key < end; ++key) {
        year = test_case[key];
        printf("%d is %sa leap year.\n", year, (is_leap_year(year) == 1 ? "" : "not "));
    }
}
```

```txt

1900 is not a leap year.
1994 is not a leap year.
1996 is a leap year.
1997 is not a leap year.
2000 is a leap year.

```



## C++

Uses C++11. Compile with
 g++ -std=c++11 leap_year.cpp

```cpp
#include <iostream>

bool is_leap_year(int year) {
  return year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
}

int main() {
  for (auto year : {1900, 1994, 1996, 1997, 2000}) {
    std::cout << year << (is_leap_year(year) ? " is" : " is not") << " a leap year.\n";
  }
}
```

```txt

1900 is not a leap year.
1994 is not a leap year.
1996 is a leap year.
1997 is not a leap year.
2000 is a leap year.

```


## C#

```c#
using System;

class Program
{
    static void Main()
    {
        foreach (var year in new[] { 1900, 1994, 1996, DateTime.Now.Year })
        {
            Console.WriteLine("{0} is {1}a leap year.",
                              year,
                              DateTime.IsLeapYear(year) ? string.Empty : "not ");
        }
    }
}
```

```txt
1900 is not a leap year.
1994 is not a leap year.
1996 is a leap year.
2010 is not a leap year.
```




## Clipper


```Clipper
Function IsLeapYear( nYear )
Return Iif( nYear%100 == 0, (nYear%400 == 0), (nYear%4 == 0) )
```



## Clojure


```clojure
(defn leap-year? [y]
  (and (zero? (mod y 4)) (or (pos? (mod y 100)) (zero? (mod y 400)))))
```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. leap-year.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  examples VALUE "19001994199619972000".
           03  year PIC 9(4) OCCURS 5 TIMES
               INDEXED BY year-index.

       01  remainders.
           03 400-rem   PIC 9(4).
           03 100-rem   PIC 9(4).
           03 4-rem     PIC 9(4).

       PROCEDURE DIVISION.
           PERFORM VARYING year-index FROM 1 BY 1 UNTIL 5 < year-index
               MOVE FUNCTION MOD(year (year-index), 400) TO 400-rem
               MOVE FUNCTION MOD(year (year-index), 100) TO 100-rem
               MOVE FUNCTION MOD(year (year-index), 4) TO 4-rem

               IF 400-rem = 0 OR ((100-rem NOT = 0) AND 4-rem = 0)
                   DISPLAY year (year-index) " is a leap year."
               ELSE
                   DISPLAY year (year-index) " is not a leap year."
               END-IF
           END-PERFORM

           GOBACK
           .
```


Using Date Intrinsic Functions

```COBOL

       program-id. leap-yr.
           *> Given a year, where 1601 <= year <= 9999
           *> Determine if the year is a leap year
       data division.
       working-storage section.
       1 input-year pic 9999.
       1 binary.
        2 int-date pic 9(8).
        2 cal-mo-day pic 9(4).
       procedure division.
           display "Enter calendar year (1601 thru 9999): "
               with no advancing
           accept input-year
           if input-year >= 1601 and <= 9999
           then
                   *> if the 60th day of a year is Feb 29
                   *> then the year is a leap year
               compute int-date = function integer-of-day
                   ( input-year * 1000 + 60 )
               compute cal-mo-day = function mod (
                   (function date-of-integer ( int-date )) 10000 )
               display "Year " input-year space with no advancing
               if cal-mo-day = 229
                   display "is a leap year"
               else
                   display "is NOT a leap year"
               end-if
           else
               display "Input date is not within range"
           end-if
           stop run
           .
       end program leap-yr.

```

```txt

Enter calendar year (1601 thru 9999): 2016
Year 2016 is a leap year
Enter calendar year (1601 thru 9999): 2017
Year 2017 is NOT a leap year
Enter calendar year (1601 thru 9999): 2100
Year 2100 is NOT a leap year
Enter calendar year (1601 thru 9999): 2400
Year 2400 is a leap year
Enter calendar year (1601 thru 9999): 3000
Year 3000 is NOT a leap year
Enter calendar year (1601 thru 9999): 4000
Year 4000 is a leap year

```



## Common Lisp


```lisp
(defun leap-year-p (year)
  (destructuring-bind (fh h f)
      (mapcar #'(lambda (n) (zerop (mod year n))) '(400 100 4))
    (or fh (and (not h) f))))
```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE LeapYear;
IMPORT StdLog, Strings, Args;

PROCEDURE IsLeapYear(year: INTEGER): BOOLEAN;
BEGIN
	IF year MOD 4 # 0 THEN
    	RETURN FALSE
	ELSE
		IF year MOD 100 = 0 THEN
			IF year MOD 400  = 0 THEN RETURN TRUE ELSE RETURN FALSE END
		ELSE
			RETURN TRUE
		END
	END
END IsLeapYear;

PROCEDURE Do*;
VAR
	p: Args.Params;
	year,done,i: INTEGER;
BEGIN
	Args.Get(p);
	FOR i := 0 TO p.argc - 1 DO
		Strings.StringToInt(p.args[i],year,done);
		StdLog.Int(year);StdLog.String(":>");StdLog.Bool(IsLeapYear(year));StdLog.Ln
	END;

END Do;
END LeapYear.

```

Execute: ^Q LeapYear.Do 2000 2004 2013~<br/>
```txt

 2000:> $TRUE
 2004:> $TRUE
 2013:> $FALSE

```


## D


```d
import std.algorithm;

bool leapYear(in uint y) pure nothrow {
    return (y % 4) == 0 && (y % 100 || (y % 400) == 0);
}

void main() {
    auto good = [1600, 1660, 1724, 1788, 1848, 1912, 1972, 2032,
                 2092, 2156, 2220, 2280, 2344, 2348];
    auto bad =  [1698, 1699, 1700, 1750, 1800, 1810, 1900, 1901,
                 1973, 2100, 2107, 2200, 2203, 2289];
    assert(filter!leapYear(bad ~ good).equal(good));
}
```



Using the datetime library:

```d
import std.datetime;

void main() {
    assert(yearIsLeapYear(1724));
    assert(!yearIsLeapYear(1973));
    assert(!Date(1900, 1, 1).isLeapYear);
    assert(DateTime(2000, 1, 1).isLeapYear);
}

```


## Dart



```Dart
class Leap {
  bool leapYear(num year) {
    return (year % 400 == 0) || (( year % 100 != 0) && (year % 4 == 0));
  }
}
```



=={{header|Delphi}}/{{header|Pascal}}==
Delphi has standard function IsLeapYear in SysUtils unit.

```Delphi
program TestLeapYear;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  Year: Integer;

begin
  Write('Enter the year: ');
  Readln(Year);
  if IsLeapYear(Year) then
    Writeln(Year, ' is a Leap year')
  else
    Writeln(Year, ' is not a Leap year');
  Readln;
end.
```



## DWScript


```Delphi
function IsLeapYear(y : Integer) : Boolean;
begin
   Result:=    (y mod 4 = 0)
           and (   ((y mod 100) <> 0)
                or ((y mod 400) = 0) );
end;

const good : array [0..13] of Integer =
   [1600,1660,1724,1788,1848,1912,1972,2032,2092,2156,2220,2280,2344,2348];
const bad : array [0..13] of Integer =
   [1698,1699,1700,1750,1800,1810,1900,1901,1973,2100,2107,2200,2203,2289];

var i : Integer;

PrintLn('Checking leap years');
for i in good do
   if not IsLeapYear(i) then PrintLn(i);

PrintLn('Checking non-leap years');
for i in bad do
   if IsLeapYear(i) then PrintLn(i);
```



## Dyalect



```Dyalect
func isLeap(y) {
    if y % 100 == 0 {
        y % 400 == 0
    } else {
        y % 4 == 0
    }
}

print(isLeap(1984))
```


```txt
true
```



## Ela


```ela
isLeap y | y % 100 == 0 = y % 400 == 0
         | else         = y % 4 == 0
```



## Elixir


```elixir
leap_year? = fn(year) -> :calendar.is_leap_year(year) end
IO.inspect for y <- 2000..2020, leap_year?.(y), do: y
```


```txt

[2000, 2004, 2008, 2012, 2016, 2020]

```



## Emacs Lisp

```lisp
(defun leap-year-p (year)
  (apply (lambda (a b c) (or a (and (not b) c)))
	 (mapcar (lambda (n) (zerop (mod year n)))
		 '(400 100 4))))
```



## Erlang


```erlang

-module(gregorian).
-export([leap/1]).

leap( Year ) -> calendar:is_leap_year( Year ).

```



## ERRE


```ERRE
PROGRAM LEAP_YEAR

FUNCTION LEAP(YR%)
     LEAP=(YR% MOD 4=0) AND ((YR% MOD 400=0) OR (YR% MOD 100<>0))
END FUNCTION

BEGIN
     LOOP
        INPUT("Enter a year: ",year%)
        EXIT IF YEAR%=0
        IF LEAP(year%) THEN
            PRINT(year%;" is a leap year")
          ELSE
            PRINT(year%;" is not a leap year")
        END IF
     END LOOP
END PROGRAM
```



## Euphoria


```euphoria
function isLeapYear(integer year)
    return remainder(year,4)=0 and remainder(year,100)!=0 or remainder(year,400)=0
end function
```



## Excel

Take two cells, say A1 and B1, in B1 type in :


```Excel

=IF(OR(NOT(MOD(A1,400)),AND(NOT(MOD(A1,4)),MOD(A1,100))),"Leap Year","Not a Leap Year")

```


```txt

1900	Not a Leap Year
1954	Not a Leap Year
1996	Leap Year
2003	Not a Leap Year
2012	Leap Year

```


=={{header|F_Sharp|F#}}==

```fsharp
let isLeapYear = System.DateTime.IsLeapYear
assert isLeapYear 1996
assert isLeapYear 2000
assert not (isLeapYear 2001)
assert not (isLeapYear 1900)
```



## Factor

Call ''leap-year?'' word from ''calendars'' vocabulary. For example:

```factor
USING: calendar prettyprint ;
2011 leap-year? .
```

Factor uses proleptic Gregorian calendar.


## Forth


```forth
: leap-year? ( y -- ? )
  dup 400 mod 0= if drop true  exit then
  dup 100 mod 0= if drop false exit then
        4 mod 0= ;
```


Or more simply (but always computing three "mod"):

```forth
: leap-year? dup 4 mod 0= over 16 mod 0= rot 25 mod 0= not or and ;
```



## Fortran


```fortran
program leap
 implicit none

 write(*,*) leap_year([1900, 1996, 1997, 2000])

 contains

	pure elemental function leap_year(y) result(is_leap)
	implicit none
	logical :: is_leap
	integer,intent(in) :: y

	is_leap = (mod(y,4)==0 .and. .not. mod(y,100)==0) .or. (mod(y,400)==0)

	end function leap_year

end program leap
```

```txt
  F T F T
```



## FreeBASIC


```FreeBASIC
' version 23-06-2015
' compile with: fbc -s console

#Ifndef TRUE        ' define true and false for older freebasic versions
    #Define FALSE 0
    #Define TRUE Not FALSE
#EndIf

Function leapyear(Year_ As Integer) As Integer

    If (Year_ Mod 4) <> 0 Then Return FALSE
    If (Year_ Mod 100) = 0 AndAlso (Year_ Mod 400) <> 0 Then Return FALSE
    Return TRUE

End Function

' ------=< MAIN >=------

' year is a FreeBASIC keyword
Dim As Integer Year_

For Year_ = 1800 To 2900 Step 100
    Print Year_; IIf(leapyear(Year_), " is a leap year", " is not a leap year")
Next

Print : Print

For Year_ = 2012 To 2031
    Print Year_;
    If leapyear(Year_) = TRUE Then
        Print " = leap",
    Else
        Print " = no",
    End If
    If year_ Mod 4 = 3 Then Print ' lf/cr
Next

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
 1800 is not a leap year
 1900 is not a leap year
 2000 is a leap year
 2100 is not a leap year
 2200 is not a leap year
 2300 is not a leap year
 2400 is a leap year
 2500 is not a leap year
 2600 is not a leap year
 2700 is not a leap year
 2800 is a leap year
 2900 is not a leap year

 2012 = leap   2013 = no     2014 = no     2015 = no
 2016 = leap   2017 = no     2018 = no     2019 = no
 2020 = leap   2021 = no     2022 = no     2023 = no
 2024 = leap   2025 = no     2026 = no     2027 = no
 2028 = leap   2029 = no     2030 = no     2031 = no
```



## FutureBasic


```futurebasic

include "ConsoleWindow"

// In-line C function to generate random number in range
BeginCFunction
long randomInRange( long min, long max ) {
int i = (arc4random()%(max-min+1))+min;
return (long)i;
}
EndC
toolbox fn randomInRange( long min, long max  ) = long

// Leap year test function
local fn LeapYear( year as long ) as Boolean
dim as Boolean result : result = _false

if year mod 400 == 0 then result = _true  : exit fn
if year mod 100 == 0 then result = _false : exit fn
if year mod   4 == 0 then result = _true  : exit fn
if year mod   4 != 0 then result = _false : exit fn
end fn = result

dim as long i, y, knownLeapYear(10)

// Array of known leap years from 1980 through 2020 for control
knownLeapYear(0) = 1980 : knownLeapYear(1)  = 1984 : knownLeapYear(2) = 1988
knownLeapYear(3) = 1992 : knownLeapYear(4)  = 1996 : knownLeapYear(5) = 2000
knownLeapYear(6) = 2004 : knownLeapYear(7)  = 2008 : knownLeapYear(8) = 2012
knownLeapYear(9) = 2016 : knownLeapYear(10) = 2020

print "Known leap years:"
for i = 0 to 9
   if ( fn LeapYear( knownLeapYear(i) ) == _true )
   print knownLeapYear(i); " is a leap year."
else
   print knownLeapYear(i); " is a not leap year."
   end if
next

print

// Random years from 1980 to 2020 to test
print "Check random years:"
for i = 0 to 20
   y = fn randomInRange( 1980, 2020  )
   if ( fn LeapYear( y ) == _true )
      print y; " is a leap year."
   else
      print y; " is a not leap year."
   end if
next

```


Output (results will vary for random years):

```txt

Known leap years:
 1980 is a leap year.
 1984 is a leap year.
 1988 is a leap year.
 1992 is a leap year.
 1996 is a leap year.
 2000 is a leap year.
 2004 is a leap year.
 2008 is a leap year.
 2012 is a leap year.
 2016 is a leap year.

Check random years:
 1998 is a not leap year.
 1987 is a not leap year.
 2015 is a not leap year.
 1998 is a not leap year.
 2020 is a leap year.
 2020 is a leap year.
 2009 is a not leap year.
 2020 is a leap year.
 2018 is a not leap year.
 2013 is a not leap year.
 2003 is a not leap year.
 1994 is a not leap year.
 1989 is a not leap year.
 1999 is a not leap year.
 1984 is a leap year.
 1980 is a leap year.
 1998 is a not leap year.
 2008 is a leap year.
 1983 is a not leap year.
 2007 is a not leap year.
 2004 is a leap year.

```



## Gambas


```gambas
Public Sub Form_Open()
Dim dDate As Date
Dim siYear As Short = InputBox("Enter a year", "Leap year test")
Dim sMessage As String = " is a leap year."

Try dDate = Date(siYear, 02, 29)
If Error Then sMessage = " is not a leap year."

Message(siYear & sMessage)

End
```


Output:

```txt

2016 is a leap year.

```




## GAP


```gap
IsLeapYear := function(n)
  return (n mod 4 = 0) and ((n mod 100 <> 0) or (n mod 400 = 0));
end;

# alternative using built-in function
IsLeapYear := function(n)
  return DaysInYear(n) = 366;
end;
```



## Genie

Dialect conversion from Vala entry.


```genie
[indent=4]
/*
   Leap year, in Genie

   valac leapYear.gs
   ./leapYear
*/
init
    years:array of DateYear = {1900, 1994, 1996, 1997, 2000, 2100}

    for year in years
        status:string = year.is_leap_year() ? "" : "not "
        stdout.printf("%d is %sa leap year.\n", year, status)
```


```txt
prompt$ valac leapYear.gs
prompt$ ./leapYear
1900 is not a leap year.
1994 is not a leap year.
1996 is a leap year.
1997 is not a leap year.
2000 is a leap year.
2100 is not a leap year.
```



## Go


```go
func isLeap(year int) bool {
    return year%400 == 0 || year%4 == 0 && year%100 != 0
}
```



## Groovy

Solution:

```groovy
(1900..2012).findAll {new GregorianCalendar().isLeapYear(it)}.each {println it}
```

<pre style="height:30ex;overflow:scroll;">1904
1908
1912
1916
1920
1924
1928
1932
1936
1940
1944
1948
1952
1956
1960
1964
1968
1972
1976
1980
1984
1988
1992
1996
2000
2004
2008
2012
```


=={{header|GW-BASIC}}==
```qbasic

10  ' Leap year
20  DEF FN ISLEAPYEAR(Y%) = ((Y% MOD 4 = 0) AND (Y% MOD 100 <> 0)) OR (Y% MOD 400 = 0)
95  ' *** Test ***
100 FOR I% = 1 TO 5
110  READ YEAR%
120  PRINT YEAR%; "is ";
130  IF FN ISLEAPYEAR(YEAR%) = 0 THEN PRINT "not "; ELSE PRINT "";
140  PRINT "a leap year."
150 NEXT I%
160 END
200 DATA 1900, 1994, 1996, 1997, 2000

```

```txt

 1900 is not a leap year.
 1994 is not a leap year.
 1996 is a leap year.
 1997 is not a leap year.
 2000 is a leap year.

```



## Harbour


```visualfoxpro
FUNCTION IsLeapYear( nYear )
   RETURN iif( nYear % 100 == 0, nYear % 400 == 0, nYear % 4 == 0 )
```



## Haskell

'''Simple version'''

```haskell
import Data.List
import Control.Monad
import Control.Arrow

leaptext x b | b = show x ++ " is a leap year"
	     | otherwise = show x ++  " is not a leap year"

isleapsf j | 0==j`mod`100 = 0 == j`mod`400
	   | otherwise    = 0 == j`mod`4
```

'''Algorithmic'''

```haskell
isleap = foldl1 ((&&).not).flip map [400, 100, 4]. ((0==).).mod
```

Example using isleap

```haskell
*Main> mapM_ (putStrLn. (ap leaptext isleap)) [1900,1994,1996,1997,2000]
1900 is not a leap year
1994 is not a leap year
1996 is a leap year
1997 is not a leap year
2000 is a leap year
```


'''TDD version'''

```haskell
import Test.HUnit

isLeapYear::Int->Bool
isLeapYear y
  | mod y 400 == 0 = True
  | mod y 100 == 0 = False
  | mod y 4 == 0 = True
  | otherwise = False

tests = TestList[TestCase $ assertEqual "4 is a leap year" True $ isLeapYear 4
                ,TestCase $ assertEqual "1 is not a leap year" False $ isLeapYear 1
                ,TestCase $ assertEqual "64 is a leap year" True $ isLeapYear 64
                ,TestCase $ assertEqual "2000 is a leap year" True $ isLeapYear 2000
                ,TestCase $ assertEqual "1900 is not a leap year" False $ isLeapYear 1900]
```



## Hy


```clojure
(defn leap? [y]
    (and
        (= (% y 4) 0)
        (or
            (!= (% y 100) 0)
            (= (% y 400) 0))))
```


=={{header|Icon}} and {{header|Unicon}}==
Gives leap year status for 2000,1900,2012 and any arguments you give

```Icon
procedure main(arglist)
every y := !([2000,1900,2012]|||arglist) do
  write("The year ",y," is ", leapyear(y) | "not ","a leap year.")
end

procedure leapyear(year)		#: determine if year is leap
   if (numeric(year) % 4 = 0 & year % 100 ~= 0) | (numeric(year) % 400 = 0) then return
end
```



## J


```j
isLeap=: 0 -/@:= 4 100 400 |/ ]
```

Example use:

```j
   isLeap 1900 1996 1997 2000
0 1 0 1
```



## Java

By default, [http://docs.oracle.com/javase/7/docs/api/index.html?java/util/GregorianCalendar.html java.util.GregorianCalendar] switches from Julian calendar to Gregorian calendar at 15 October 1582.
The code below uses both the GregorianCalendar class
and the algorithm from the wiki.
Both values are printed in the output.


```java
import java.util.GregorianCalendar;
import java.text.MessageFormat;

public class Leapyear{
        public static void main(String[] argv){
                int[] yrs = {1800,1900,1994,1998,1999,2000,2001,2004,2100};
                GregorianCalendar cal = new GregorianCalendar();
                for(int year : yrs){
                        System.err.println(MessageFormat.format("The year {0,number,#} is leaper: {1} / {2}.",
                                                                 year, cal.isLeapYear(year), isLeapYear(year)));
                }

        }
        public static boolean isLeapYear(int year){
                return (year % 100 == 0) ? (year % 400 == 0) : (year % 4 == 0);
        }
}


```

```txt
The year 1800 is leaper: false / false.
The year 1900 is leaper: false / false.
The year 1994 is leaper: false / false.
The year 1998 is leaper: false / false.
The year 1999 is leaper: false / false.
The year 2000 is leaper: true / true.
The year 2001 is leaper: false / false.
The year 2004 is leaper: true / true.
The year 2100 is leaper: false / false.
```


```java
import java.time.Year;

public class IsLeap {

    public static void main(String[] args) {
        System.out.println(Year.isLeap(2004));
    }
}

```



## JavaScript


```javascript
var isLeapYear = function (year) { return (year % 100 === 0) ? (year % 400 === 0) : (year % 4 === 0); };
```

Or, by setting the day to the 29th and checking if the day remains

```javascript
// Month values start at 0, so 1 is for February
var isLeapYear = function (year) { return new Date(year, 1, 29).getDate() === 29; };
```



## jq

```jq
def leap:
  . as $y | ($y%4) == 0 and ($y < 1582 or ($y%400) == 0 or ($y%100) != 0);
```

'''Examples''':

```jq
def assert(value; f):
  value as $value
  | ($value|f) | if . then empty else error("assertion violation: \($value) => \(.)") end;

((2400, 2012, 2000, 1600, 1500, 1400) | assert(.; leap)),

((2100, 2014, 1900, 1800, 1700, 1499) | assert(.; leap|not))

```

 $ jq -n -f Leap_year.jq


## Julia

```julia
isleap(yr::Integer) = yr % 4 == 0 && (yr < 1582 || yr % 400 == 0 || yr % 100 != 0)

@assert all(isleap, [2400, 2012, 2000, 1600, 1500, 1400])
@assert !any(isleap, [2100, 2014, 1900, 1800, 1700, 1499])
```



## K


```K
   leapyear:{(+/~x!'4 100 400)!2}

   a@&leapyear'a:1900,1994,1996,1997,2000
1996 2000
```



## Kotlin


```kotlin
fun isLeapYear(year: Int) = year % 400 == 0 || (year % 100 != 0 && year % 4 == 0)
```



## Lasso


```Lasso
define isLeapYear(y::integer) => {
	#y % 400 == 0 ? return true
	#y % 100 == 0 ? return false
	#y % 4 == 0 ? return true
	return false
}

with test in array(2012,2016,1933,1900,1999,2000) do => {^
	isLeapYear(#test)
	'\r'
^}
```


```txt
true
true
false
false
false
true
```



## Liberty BASIC


###  Simple method


```lb
if leap(1996)then
    print "leap"
else
    print "ordinary"
end if
wait

function leap(n)
    leap=date$("2/29/";n)
end function
```



###  Calculated method


```lb
    year = 1908
    select case
        case year mod 400 = 0
            leapYear = 1
        case year mod 4 = 0 and year mod 100 <> 0
            leapYear = 1
        case else
            leapYear = 0
    end select
    if leapYear = 1 then
        print year;" is a leap year."
    else
        print year;" is not a leap year."
    end if
```



## Lingo


```lingo
on isLeapYear (year)
  return date(year, 2, 29).month=2
end
```



## LiveCode


```LiveCode
function isLeapYear year
    return (year MOD 4 is 0) AND ((year MOD 400 is 0) OR (year MOD 100 is not 0))
end isLeapYear

command testLeapYear
    set itemDelimiter to comma
    put  "1900,1994,1996,1997,2000" into years
    repeat for each item y in years
        put y && "is" && isLeapYear(y) && return after tyears
    end repeat
    put tyears
end testLeapYear

1900 is false
1994 is false
1996 is true
1997 is false
2000 is true
```



## LLVM


```llvm
; This is not strictly LLVM, as it uses the C library function "printf".
; LLVM does not provide a way to print values, so the alternative would be
; to just load the string into memory, and that would be boring.

$"EMPTY_STR" = comdat any
$"NOT_STR" = comdat any
$"IS_A_LEAP_YEAR" = comdat any

@main.test_case = private unnamed_addr constant [5 x i32] [i32 1900, i32 1994, i32 1996, i32 1997, i32 2000], align 16
@"EMPTY_STR" = linkonce_odr unnamed_addr constant [1 x i8] zeroinitializer, comdat, align 1
@"NOT_STR" = linkonce_odr unnamed_addr constant [5 x i8] c"not \00", comdat, align 1
@"IS_A_LEAP_YEAR" = linkonce_odr unnamed_addr constant [22 x i8] c"%d is %sa leap year.\0A\00", comdat, align 1

;--- The declaration for the external C printf function.
declare i32 @printf(i8*, ...)

; Function Attrs: noinline nounwind optnone uwtable
define i32 @is_leap_year(i32) #0 {
  %2 = alloca i32, align 4              ;-- allocate a local copy of year
  store i32 %0, i32* %2, align 4        ;-- store a copy of year

  %3 = load i32, i32* %2, align 4       ;-- load the year
  %4 = srem i32 %3, 4                   ;-- year % 4
  %5 = icmp ne i32 %4, 0                ;-- (year % 4) != 0
  br i1 %5, label %c1false, label %c1true

c1true:
  %6 = load i32, i32* %2, align 4       ;-- load the year
  %7 = srem i32 %6, 100                 ;-- year % 100
  %8 = icmp ne i32 %7, 0                ;-- (year % 100) != 0
  br i1 %8, label %c2true, label %c1false

c1false:
  %9 = load i32, i32* %2, align 4       ;-- load the year
  %10 = srem i32 %9, 400                ;-- year % 400
  %11 = icmp ne i32 %10, 0              ;-- (year % 400) != 0
  %12 = xor i1 %11, true
  br label %c2true

c2true:
  %13 = phi i1 [ true, %c1true ], [ %12, %c1false ]
  %14 = zext i1 %13 to i64
  %15 = select i1 %13, i32 1, i32 0
  ret i32 %15
}

; Function Attrs: noinline nounwind optnone uwtable
define i32 @main() #0 {
  %1 = alloca [5 x i32], align 16       ;-- allocate test_case
  %2 = alloca i32, align 4              ;-- allocate key
  %3 = alloca i32, align 4              ;-- allocate end
  %4 = alloca i32, align 4              ;-- allocate year
  %5 = bitcast [5 x i32]* %1 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %5, i8* bitcast ([5 x i32]* @main.test_case to i8*), i64 20, i32 16, i1 false)
  store i32 0, i32* %2, align 4         ;-- store 0 in key
  store i32 5, i32* %3, align 4         ;-- store 5 in end
  br label %loop

loop:
  %6 = load i32, i32* %2, align 4       ;-- load key
  %7 = load i32, i32* %3, align 4       ;-- load end
  %8 = icmp slt i32 %6, %7              ;-- key < end
  br i1 %8, label %loop_body, label %exit

loop_body:
  %9 = load i32, i32* %2, align 4       ;-- load key
  %10 = sext i32 %9 to i64              ;-- sign extend key
  %11 = getelementptr inbounds [5 x i32], [5 x i32]* %1, i64 0, i64 %10
  %12 = load i32, i32* %11, align 4     ;-- load test_case[key]
  store i32 %12, i32* %4, align 4       ;-- store test_case[key] as year

  %13 = load i32, i32* %4, align 4      ;-- load year
  %14 = call i32 @is_leap_year(i32 %13) ;-- is_leap_year(year)
  %15 = icmp eq i32 %14, 1              ;-- is_leap_year(year) == 1
  %16 = zext i1 %15 to i64              ;-- zero extend
  %17 = select i1 %15, i8* getelementptr inbounds ([1 x i8], [1 x i8]* @"EMPTY_STR", i32 0, i32 0), i8* getelementptr inbounds ([5 x i8], [5 x i8]* @"NOT_STR", i32 0, i32 0)

  %18 = load i32, i32* %4, align 4      ;-- load year
  %19 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([22 x i8], [22 x i8]* @"IS_A_LEAP_YEAR", i32 0, i32 0), i32 %18, i8* %17)

  %20 = load i32, i32* %2, align 4      ;-- load key
  %21 = add nsw i32 %20, 1              ;-- increment key
  store i32 %21, i32* %2, align 4       ;-- store key
  br label %loop

exit:
  ret i32 0
}

; Function Attrs: argmemonly nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture writeonly, i8* nocapture readonly, i64, i32, i1) #1

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { argmemonly nounwind }
```

```txt
1900 is not a leap year.
1994 is not a leap year.
1996 is a leap year.
1997 is not a leap year.
2000 is a leap year.
```



## Logo


```logo
to multiple? :n :d
  output equal? 0 modulo :n :d
end
to leapyear? :y
  output ifelse multiple? :y 100 [multiple? :y 400] [multiple? :y 4]
end
```



## Logtalk


```logtalk
leap_year(Year) :-
    (   mod(Year, 4) =:= 0, mod(Year, 100) =\= 0 ->
        true
    ;   mod(Year, 400) =:= 0
    ).
```



## LOLCODE


```lolcode
BTW  Determine if a Gregorian calendar year is leap
HAI 1.3
HOW IZ I Leap YR Year
  BOTH SAEM 0 AN MOD OF Year AN 4
  O RLY?
    YA RLY
      BOTH SAEM 0 AN MOD OF Year AN 100
      O RLY?
        YA RLY
          BOTH SAEM 0 AN MOD OF Year AN 400
          O RLY?
            YA RLY
              FOUND YR WIN
            NO WAI
              FOUND YR FAIL
          OIC
        NO WAI
          FOUND YR WIN
        OIC
    NO WAI
      FOUND YR FAIL
    OIC
IF U SAY SO

I HAS A Yearz ITZ A BUKKIT
Yearz HAS A SRS 0 ITZ 1900
Yearz HAS A SRS 1 ITZ 1904
Yearz HAS A SRS 2 ITZ 1994
Yearz HAS A SRS 3 ITZ 1996
Yearz HAS A SRS 4 ITZ 1997
Yearz HAS A SRS 5 ITZ 2000

IM IN YR Loop UPPIN YR Index WILE DIFFRINT Index AN 6
  I HAS A Yr ITZ Yearz'Z SRS Index
  I HAS A Not
  I IZ Leap YR Yr MKAY
  O RLY?
  YA RLY
   Not R ""
  NO WAI
   Not R " NOT"
  OIC
  VISIBLE Yr " is" Not " a leap year"
IM OUTTA YR Loop

KTHXBYE

```

```txt
1900 is NOT a leap year
1904 is a leap year
1994 is NOT a leap year
1996 is a leap year
1997 is NOT a leap year
2000 is a leap year

```



## Lua


```Lua
function isLeapYear(year)
  return year%4==0 and (year%100~=0 or year%400==0)
end
```



## Maple


```maple
isLeapYear := proc(year)
	if not year mod 4 = 0 or (year mod 100 = 0 and not year mod 400 = 0) then
		return false;
	else
		return true;
	end if;
end proc:
```



## Mathematica

Dates are handled by built-in functions in the Wolfram Language

```Mathematica
LeapYearQ[2002]
```


=={{header|MATLAB}} / {{header|Octave}}==
MATLAB, conveniently, provides a function that returns the last day of an arbitrary month of the calendar given the year. Using the fact that February is 29 days long during a leap year, we can write a one-liner that solves this task.

```MATLAB
function TrueFalse = isLeapYear(year)
    TrueFalse = (eomday(year,2) == 29);
end
```



### Using Logical and modular functions


```matlab
x = ~mod(YEAR, 4) & (mod(YEAR, 100) | ~mod(YEAR, 400))
```



## Maxima


```maxima
leapyearp(year) := is(mod(year, 4) = 0 and
   (mod(year, 100) # 0 or mod(year, 400) = 0))$
```


=={{header|MK-61/52}}==
<lang>П0	1	0	0	/	{x}	x=0	14	ИП0	4
0	0	ПП	18	ИП0	4	ПП	18	/	{x}
x=0	24	1	С/П	0	С/П
```



## Mercury


```mercury
:- pred is_leap_year(int::in) is semidet.

is_leap_year(Year) :-
   ( if Year mod 100 = 0 then Year mod 400 = 0 else Year mod 4 = 0 ).
```


Usage:


```mercury
:- module leap_year.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list, string.

main(!IO) :-
    Years = [1600, 1700, 1899, 1900, 2000, 2006, 2012],
    io.write_list(Years, "", write_year_kind, !IO).

:- pred write_year_kind(int::in, io::di, io::uo) is det.

write_year_kind(Year, !IO) :-
  io.format("%d %s a leap year.\n",
      [i(Year), s(if is_leap_year(Year) then "is" else "is not" )], !IO).
```



## MiniScript


```MiniScript
isLeapYear = function(year)
  return year%4==0 and (year % 100 or not year % 400)
end function
```



## MIPS Assembly

Pass year in a0, returns boolean in v0.

```mips

IsLeap:	andi $a1, $a0, 3 #a0 is year to test
	bnez $a1 NotLeap
	li $a1, 100
	div $a0, $a1
	mfhi $a1
	bnez $a1, Leap
	mflo $a1
	andi $a1, $a1, 3
	bnez $a1, NotLeap
Leap:	li $v0, 1
	jr $ra
NotLeap:li $v0, 0
	jr $ra

```


=={{header|Modula-2}}==

```modula2
MODULE LeapYear;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,ReadChar;

PROCEDURE IsLeapYear(year : INTEGER) : BOOLEAN;
BEGIN
    IF year MOD 100 = 0 THEN
        RETURN year MOD 400 = 0;
    END;
    RETURN year MOD 4 = 0
END IsLeapYear;

PROCEDURE Print(year : INTEGER);
VAR
    buf : ARRAY[0..63] OF CHAR;
    leap : BOOLEAN;
BEGIN
    leap := IsLeapYear(year);
    FormatString("Is %i a leap year? %b\n", buf, year, leap);
    WriteString(buf)
END Print;

BEGIN
    Print(1900);
    Print(1994);
    Print(1996);
    Print(1997);
    Print(2000);
    ReadChar
END LeapYear.
```



## MUMPS


```MUMPS
ILY(X) ;IS IT A LEAP YEAR?
 QUIT ((X#4=0)&(X#100'=0))!((X#100=0)&(X#400=0))
```

Usage:
```txt
USER>W $SELECT($$ILY^ROSETTA(1900):"Yes",1:"No")
No
USER>W $SELECT($$ILY^ROSETTA(2000):"Yes",1:"No")
Yes
USER>W $SELECT($$ILY^ROSETTA(1999):"Yes",1:"No")
No
```



## Neko

Translating from C


```ActionScript
/**
 <doc><h2>Leap year, in Neko</h2></doc>
**/

var leapyear = function(y) return ($not(y % 4) && $istrue(y % 100) || $not(y % 400))

var tests = $array(2000, 1997, 1996, 1994, 1990, 1980, 1900)
var cnt = $asize(tests)
while (cnt -= 1) >= 0 $print(tests[cnt], if leapyear(tests[cnt]) " is" else " is not", " a leapyear", "\n")
```


```txt
prompt$ nekoc leapyear.neko
prompt$ neko leapyear.n
1900 is not a leapyear
1980 is a leapyear
1990 is not a leapyear
1994 is not a leapyear
1996 is a leapyear
1997 is not a leapyear
2000 is a leapyear
```



## Nemerle

Demonstrating implementation as well as use of standard library function.

```Nemerle
using System;
using System.Console;
using Nemerle.Assertions;
using Nemerle.Imperative;

module LeapYear
{
    IsLeapYear(year : int) : bool
      requires year >= 1582 otherwise throw ArgumentOutOfRangeException("year must be in Gregorian calendar.")
      // without the contract enforcement would work for proleptic Gregorian Calendar
      // in that case we might still want to require year > 0
    {
        when (year % 400 == 0) return true;
        when (year % 100 == 0) return false;
        when (year % 4   == 0) return true;
        false


    }

    Main() : void
    {
        WriteLine("2000 is a leap year: {0}", IsLeapYear(2000));
        WriteLine("2100 is a leap year: {0}", IsLeapYear(2100));
        try {
            WriteLine("1500 is a leap year: {0}", IsLeapYear(1500));
        }
        catch {
            |e is ArgumentOutOfRangeException => WriteLine(e.Message)
        }
        WriteLine("1500 is a leap year: {0}", DateTime.IsLeapYear(1500)); // is false, indicating use of proleptic
                                                                          // Gregorian calendar rather than reverting to
                                                                          // Julian calendar
        WriteLine("{0} is a leap year: {1}", DateTime.Now.Year,
                                             DateTime.IsLeapYear(DateTime.Now.Year));
    }
}
```

```txt
2000 is a leap year: True
2100 is a leap year: False
Specified argument was out of the range of valid values.
Parameter name: year must be in Gregorian calendar.
1500 is a leap year: False
2013 is a leap year: False
```



## NetRexx

Demonstrates both a '''Gregorian/proleptic Gregorian''' calendar leap-year algorithm and use of the Java library's <code>GregorianCalendar</code> object to determine which years are leap-years.

Note that the Java library indicates that the year '''1500'''
is a leap-year as the Gregorian calendar wasn't established until 1582.
The Java library implements the Julian calendar for dates
prior to the Gregorian cut-over and leap-year rules in the Julian calendar
are different to those for the Gregorian calendar.

```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols nobinary

years = '1500 1580 1581 1582 1583 1584 1600 1700 1800 1900 1994 1996 1997 2000 2004 2008 2009 2010 2011 2012 2100 2200 2300 2400 2500 2600'
years['l-a'] = ''
years['n-a'] = ''
years['l-j'] = ''
years['n-j'] = ''

loop y_ = 1 to years.words
  year = years.word(y_)
  if isLeapyear(year) then years['l-a'] = years['l-a'] year
                      else years['n-a'] = years['n-a'] year
  if GregorianCalendar().isLeapYear(year) then years['l-j'] = years['l-j'] year
                                          else years['n-j'] = years['n-j'] year
  end y_

years['l-a'] = years['l-a'].strip
years['n-a'] = years['n-a'].strip
years['l-j'] = years['l-j'].strip
years['n-j'] = years['n-j'].strip

say ' Sample years:' years['all'].changestr(' ', ',')
say '     Leap years (algorithmically):' years['l-a'].changestr(' ', ',')
say '     Leap years (Java library)   :' years['l-j'].changestr(' ', ',')
say ' Non-leap years (algorithmically):' years['n-a'].changestr(' ', ',')
say ' Non-leap years (Java library)   :' years['n-j'].changestr(' ', ',')

return

-- algorithmically
method isLeapyear(year = int) public constant binary returns boolean
  select
    when year // 400 = 0 then ly = isTrue
    when year // 100 \= 0 & year // 4 = 0 then ly = isTrue
    otherwise ly = isFalse
    end
  return ly

method isTrue public constant binary returns boolean
  return 1 == 1

method isFalse public constant binary returns boolean
  return \isTrue
```

```txt

 Sample years: 1500,1580,1581,1582,1583,1584,1600,1700,1800,1900,1994,1996,1997,2000,2004,2008,2009,2010,2011,2012,2100,2200,2300,2400,2500,2600
     Leap years (algorithmically): 1580,1584,1600,1996,2000,2004,2008,2012,2400
     Leap years (Java library)   : 1500,1580,1584,1600,1996,2000,2004,2008,2012,2400
 Non-leap years (algorithmically): 1500,1581,1582,1583,1700,1800,1900,1994,1997,2009,2010,2011,2100,2200,2300,2500,2600
 Non-leap years (Java library)   : 1581,1582,1583,1700,1800,1900,1994,1997,2009,2010,2011,2100,2200,2300,2500,2600

```


=={{Header|Nim}}==

```nim
import times
let year = 1980
echo isLeapYear(year)

# or

proc isLeapYear2(year): bool =
  if year mod 100 == 0:
    year mod 400 == 0
  else: year mod 4 == 0

echo isLeapYear2(year)
```

```txt
true
true
```


=={{Header|Oberon-2}}==

```oberon2

PROCEDURE IsLeapYear(year: INTEGER): BOOLEAN;
BEGIN
  IF year MOD 4 # 0 THEN
    RETURN FALSE
  ELSE
    IF year MOD 100 = 0 THEN
      IF year MOD 400  = 0 THEN
	RETURN TRUE
      ELSE
	RETURN FALSE
      END
    ELSE
      RETURN TRUE
    END
 END
END IsLeapYear;

```



## Objeck


```objeck
bundle Default {
  class LeapYear {
    function : Main(args : String[]) ~ Nil {
      test_case := [1900, 1994, 1996, 1997, 2000];
      each(i : test_case) {
        test_case[i]->Print();
        if(IsLeapYear(test_case[i])) {
          " is a leap year."->PrintLine();
        }
        else {
          " is not a leap year."->PrintLine();
        };
      };
    }

    function : native : IsLeapYear(year : Int) ~ Bool {
      if(year % 4 = 0 & year % 100 <> 0) {
        return true;
      }
      else if(year % 400 = 0) {
        return true;
      };

      return false;
    }
  }
}
```



## OCaml


```ocaml
let is_leap_year ~year =
  if (year mod 100) = 0
  then (year mod 400) = 0
  else (year mod 4) = 0
```

Using Unix Time functions:

```ocaml
let is_leap_year ~year =
  let tm =
    Unix.mktime {
      (Unix.gmtime (Unix.time())) with
        Unix.tm_year = (year - 1900);
        tm_mon = 1 (* feb *);
        tm_mday = 29
      }
  in
  (tm.Unix.tm_mday = 29)
```



## Oforth



```Oforth
Date.IsLeapYear(2000)
```



## ooRexx


```ooRexx

::routine isLeapYear
  use arg year
  d = .datetime~new(year, 1, 1)
  return d~isLeapYear

```



## OpenEdge/Progress

The DATE function converts month, day, year integers to a date data type and will set the error status if invalid values are passed.

```progress
FUNCTION isLeapYear RETURNS LOGICAL (
   i_iyear AS INTEGER
):

   DATE( 2, 29, i_iyear ) NO-ERROR.
   RETURN NOT ERROR-STATUS:ERROR.

END FUNCTION. /* isLeapYear */

MESSAGE
   1900 isLeapYear( 1900 ) SKIP
   1994 isLeapYear( 1994 ) SKIP
   1996 isLeapYear( 1996 ) SKIP
   1997 isLeapYear( 1997 ) SKIP
   2000 isLeapYear( 2000 )
VIEW-AS ALERT-BOX.
```



## Oz


```oz
declare
  fun {IsLeapYear Year}
     case Year mod 100 of 0 then
	Year mod 400 == 0
     else
	Year mod 4 == 0
     end
  end
in
  for Y in [1900 1996 1997 2000] do
     if {IsLeapYear Y} then
	{System.showInfo Y#" is a leap year."}
     else
	{System.showInfo Y#" is NOT a leap year."}
     end
  end
```

```txt
1900 is NOT a leap year.
1996 is a leap year.
1997 is NOT a leap year.
2000 is a leap year.
```



## PARI/GP


```parigp
isLeap(n)={
  if(n%400==0, return(1));
  if(n%100==0, return(0));
  n%4==0
};
```


Alternate version:

```parigp
isLeap(n)=!(n%if(n%100,4,400))
```


```parigp
isLeap(n)={
  if(n%4,0,
    n%100,1,
      n%400,0,1
  )
};
```


## Pascal

```pascal
program LeapYear;
uses
  sysutils;//includes isLeapYear

procedure TestYear(y: word);
begin
  if IsLeapYear(y) then
    writeln(y,' is a leap year')
  else
    writeln(y,' is NO leap year');
end;
Begin
  TestYear(1900);
  TestYear(2000);
  TestYear(2100);
  TestYear(1904);
end.
```

Output:

```txt
1900 is NO leap year
2000 is a leap year
2100 is NO leap year
1904 is a leap year
```



## Perl


```Perl
sub isleap {
    my $year = shift;
    if ($year % 100 == 0) {
        return ($year % 400 == 0);
    }
    return ($year % 4 == 0);
}
```


Or more concisely:


```Perl
sub isleap { !($_[0] % 100) ? !($_[0] % 400) : !($_[0] % 4) }
```


Alternatively, using functions/methods from CPAN modules:


```Perl
use Date::Manip;
print Date_LeapYear(2000);

use Date::Manip::Base;
my $dmb = new Date::Manip::Base;
print $dmb->leapyear(2000);

use DateTime;
my $date = DateTime->new(year => 2000);
print $date->is_leap_year();
```



## Perl 6

```perl6
say "$year is a {Date.is-leap-year($year) ?? 'leap' !! 'common'} year."
```

In Rakudo 2010.07, <code>Date.is-leap-year</code> is implemented as

```perl6
multi method is-leap-year($y = $!year) {
    $y %% 4 and not $y %% 100 or $y %% 400
}
```



## Phix

Available as an auto-include, implemented as:

```Phix
global function is_leap_year(integer y)
    return remainder(y,4)=0 and (remainder(y,100)!=0 or remainder(y,400)=0)
end function
```



## PHP


```php
<?php
function isLeapYear($year) {
    if ($year % 100 == 0) {
        return ($year % 400 == 0);
    }
    return ($year % 4 == 0);
}
```

With <code>date('L')</code>:

```php
<?php
function isLeapYear($year) {
    return (date('L', mktime(0, 0, 0, 2, 1, $year)) === '1')
}
```



## PicoLisp


```PicoLisp
(de isLeapYear (Y)
   (bool (date Y 2 29)) )
```

```txt
: (isLeapYear 2010)
-> NIL

: (isLeapYear 2008)
-> T

: (isLeapYear 1600)
-> T

: (isLeapYear 1700)
-> NIL
```



## PL/I


```pli
dcl mod  builtin;
dcl year fixed bin (31);

do year = 1900, 1996 to 2001;
  if mod(year, 4)    = 0 &
    (mod(year, 100) ^= 0 |
     mod(year, 400)  = 0) then
    put skip edit(year, 'is a leap year') (p'9999b', a);
  else
    put skip edit(year, 'is not a leap year') (p'9999b', a);
end;
```


```txt

1900 is not a leap year
1996 is a leap year
1997 is not a leap year
1998 is not a leap year
1999 is not a leap year
2000 is a leap year
2001 is not a leap year

```



## PostScript


```postscript
/isleapyear {
    dup dup
    4 mod 0 eq     % needs to be divisible by 4
    exch
    100 mod 0 ne   % but not by 100
    and
    exch
    400 mod 0 eq   % or by 400
    or
} def
```



## PowerShell


```powershell
$Year = 2016
[System.DateTime]::IsLeapYear( $Year )
```



## Prolog

```Prolog
leap_year(L) :-
	partition(is_leap_year, L, LIn, LOut),
	format('leap years : ~w~n', [LIn]),
	format('not leap years : ~w~n', [LOut]).

is_leap_year(Year) :-
	R4 is Year mod 4,
	R100 is Year mod 100,
	R400 is Year mod 400,
	(   (R4 = 0, R100 \= 0); R400 = 0).
```

```Prolog
 ?- leap_year([1900,1994,1996,1997,2000 ]).
leap years : [1996,2000]
not leap years : [1900,1994,1997]
L = [1900,1994,1996,1997,2000].
```


There is an handy builtin that simplifies a lot, ending up in a simple query:


```Prolog

?- findall(Y, (between(1990,2030,Y),day_of_the_year(date(Y,12,31),366)), L).
L = [1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020, 2024, 2028].

```



## PureBasic


```PureBasic
Procedure isLeapYear(Year)
  If (Year%4=0 And Year%100) Or Year%400=0
    ProcedureReturn #True
  Else
    ProcedureReturn #False
  EndIf
EndProcedure
```



## Python


```python
import calendar
calendar.isleap(year)
```

or

```python
def is_leap_year(year):
    if year % 100 == 0:
        return year % 400 == 0
    return year % 4 == 0
```

Asking for forgiveness instead of permission:

```python
import datetime

def is_leap_year(year):
    try:
        datetime.date(year, 2, 29)
    except ValueError:
        return False
    return True
```



## Q


```q
ly:{((0<>x mod 100) | 0=x mod 400) & 0=x mod 4}    / Return 1b if x is a leap year; 0b otherwise
```



## R


```R
isLeapYear <- function(year) {
    ifelse(year%%100==0, year%%400==0, year%%4==0)
}

for (y in c(1900, 1994, 1996, 1997, 2000)) {
  cat(y, ifelse(isLeapYear(y), "is", "isn't"), "a leap year.\n")
}
```

```txt

1900 isn't a leap year.
1994 isn't a leap year.
1996 is a leap year.
1997 isn't a leap year.
2000 is a leap year.

```



## Racket


```racket
(define (leap-year? y)
  (and (zero? (modulo y 4)) (or (positive? (modulo y 100)) (zero? (modulo y 400)))))
```



## Raven


```Raven
define is_leap_year use $year
    $year 100 % 0 = if
        $year 400 % 0 =
    $year 4 % 0 =
```



## REBOL


```rebol
leap-year?: func [
    {Returns true if the specified year is a leap year; false otherwise.}
    year [date! integer!]
    /local div?
][
    either date? year [year: year/year] [
        if negative? year [throw make error! join [script invalid-arg] year]
    ]
    ; The key numbers are 4, 100, and 400, combined as follows:
    ;   1) If the year is divisible by 4, it’s a leap year.
    ;   2) But, if the year is also divisible by 100, it’s not a leap year.
    ;   3) Double but, if the year is also divisible by 400, it is a leap year.
    div?: func [n] [zero? year // n]
    to logic! any [all [div? 4  not div? 100] div? 400]
]
```



## Retro


```Retro
  : isLeapYear? ( y-f )
    dup 400 mod 0 = [ drop -1 0 ] [ 1 ] if 0; drop
    dup 100 mod 0 = [ drop  0 0 ] [ 1 ] if 0; drop
    4 mod 0 = ;
```

This is provided by the standard '''calendar''' library.


## REXX


### local variables


```rexx
leapyear:  procedure;    parse arg yr
return  yr//400==0  |  (yr//100\==0  &  yr//4==0)
```


===with short-circuit===
The REXX language doesn't support short-circuits, so here is a version that does a short-circuit.

```rexx
leapyear:  procedure;   parse arg yr
if yr//4\==0  then return 0                 /*Not ÷ by 4?    Not a leap year.*/
return  yr//400==0  |  yr//100\==0
```



### no local variables

This version doesn't need a PROCEDURE to hide local variable(s)   [because there aren't any local variables],

but it does invoke the   '''ARG'''   BIF multiple times.

```rexx
leapyear: if arg(1)//4\==0  then return 0
          return arg(1)//400==0  |  arg(1)//100\==0
```



### handles 2 digit year

This REXX version has the proviso that if the year is exactly two digits,

the current century is assumed   (i.e.,   no ''year'' windowing).


If a year below 100 is to be used, the year should have leading zeroes added (to make it four digits).

```rexx
leapyear:  procedure;  parse arg y          /*year could be: Y, YY, YYY, YYYY*/
if y//4\==0      then return 0              /*Not ÷ by 4?    Not a leap year.*/
if length(y)==2  then y=left(date('S'),2)y  /*adjust for a 2─digit  YY  year.*/
return y//100\==0 | y//400==0               /*apply  100 and 400  year rule. */
```



## Ring


```ring

give year
leap = isLeapYear(year)
if leap true see year + " is leap year."
else see year + " is not leap year." ok

Func isLeapYear year
     if (year % 400) = 0 return true
        but (year % 100) = 0 return false
        but (year % 4) = 0 return true
        else return false ok

```



## RPG


```RPG
     C*0N01N02N03Factor1+++OpcdeFactor2+++ResultLenDHHiLoEqComments+++++++
     C           *ENTRY    PLIST
     C                     PARM           YEAR    40       input (year)
     C                     PARM           ISLEAP  1        output (Y/N)
     C*
     C                     MOVE 'N'       ISLEAP
     C           YEAR      CABLE1752      DONE             not Gregorian
     C*
     C           YEAR      DIV  4         RESULT  40
     C                     MVR            REMAIN  40
     C           REMAIN    CABNE0         DONE
     C*
     C* If we got here, year is divisible by 4.
     C           YEAR      DIV  100       RESULT
     C                     MVR            REMAIN
     C           REMAIN    CABNE0         LEAPYR
     C*
     C* If we got here, year is divisible by 100.
     C           YEAR      DIV  400       RESULT
     C                     MVR            REMAIN
     C           REMAIN    CABNE0         DONE
     C*
     C           LEAPYR    TAG
     C                     MOVE 'Y'       ISLEAP
     C*
     C           DONE      TAG
     C                     SETON                     LR
```



## Ruby


```ruby
require 'date'

Date.leap?(year)
```


The leap? method is aliased as gregorian_leap? And yes, there is a julian_leap? method.


## Run BASIC


```runbasic
if date$("02/29/" + mid$(date$("mm/dd/yyyy"),7,4)) then print "leap year" else print "not"
```



## Rust


```rust
fn is_leap(year: i32) -> bool {
    let factor = |x| year % x == 0;
    factor(4) && (!factor(100) || factor(400))
}
```


=={{header|S-BASIC}}==
Since S-BASIC has no MOD operator or function, we have to supply one.

```basic

rem  - compute p mod q
function mod(p, q = integer) = integer
end = p - q * (p/q)

rem - return true (-1) if y is a leap year, otherwise 0
function isleapyear(y = integer) = integer
end = mod(y,4)=0 and mod(y,100)<>0 or mod(y,400)=0

rem - exercise the function
var y = integer

print "Test of century years"
for y = 1600 to 2000 step 100
   if isleapyear(y) then
     print y;" is a leap year"
   else
     print y;" is NOT a leap year"
next y

print "Test of current half-decade"
for y = 2015 to 2020
   if isleapyear(y) then
     print y; " is a leap year"
   else
     print y; " is NOT a leap year"
next y

end

```

```txt

Test of century years
 1600 is a leap year
 1700 is NOT a leap year
 1800 is NOT a leap year
 1900 is NOT a leap year
 2000 is a leap year
Test of current half-decade
 2015 is NOT a leap year
 2016 is a leap year
 2017 is NOT a leap year
 2018 is NOT a leap year
 2019 is NOT a leap year
 2020 is a leap year

```



## Scala

===JDK 7 (not recommended)===
By default, [http://docs.oracle.com/javase/7/docs/api/index.html?java/util/GregorianCalendar.html java.util.GregorianCalendar] switches from Julian calendar to Gregorian calendar at 15 October 1582.


```scala
//use Java's calendar class
new java.util.GregorianCalendar().isLeapYear(year)
```



### JDK 8

Using JSR-310 java.time.

```scala
java.time.LocalDate.ofYearDay(year, 1).isLeapYear()
```



### Implementation


For proleptic Gregorian calendar:


```scala
def isLeapYear(year:Int)=if (year%100==0) year%400==0 else year%4==0;

//or use Java's calendar class
def isLeapYear(year:Int):Boolean = {
  val c = new java.util.GregorianCalendar
  c.setGregorianChange(new java.util.Date(Long.MinValue))
  c.isLeapYear(year)
}
```



## Scheme


```scheme
(define (leap-year? n)
(apply (lambda (a b c) (or a (and (not b) c)))
       (map (lambda (m) (zero? (remainder n m)))
            '(400 100 4))))
```



## Seed7

This function is part of the "time.s7i" library. It returns TRUE if the year is a leap year in the Gregorian calendar.

```seed7
const func boolean: isLeapYear (in integer: year) is
  return (year rem 4 = 0 and year rem 100 <> 0) or year rem 400 = 0;
```

Original source: [http://seed7.sourceforge.net/algorith/date.htm#isLeapYear]


## Sidef


```ruby
func isleap(year) {
    if (year %% 100) {
        return (year %% 400);
    }
    return (year %% 4);
}
```


or a little bit simpler:

```ruby
func isleap(year) { year %% 100 ? (year %% 400) : (year %% 4) };
```



## Smalltalk

Smalltalk has a built-in method named <tt>isLeapYear</tt>:

```smalltalk

Date today isLeapYear.

```



## SNOBOL4

Predicate leap( ) succeeds/fails, returns nil.

```SNOBOL4
        define('leap(yr)')  :(end_leap)
leap    eq(remdr(yr,400),0) :s(return)
        eq(remdr(yr,100),0) :s(freturn)
    	eq(remdr(yr,4),0)   :s(return)f(freturn)
end_leap

*       # Test and display (with ?: kluge)
        test = "output = ('10' ? (*leap(yr) 1 | 0)) ': ' yr"
        yr = '1066'; eval(test)
        yr = '1492'; eval(test)
        yr = '1900'; eval(test)
        yr = '2000'; eval(test)
end
```

```txt
0: 1066
1: 1492
0: 1900
1: 2000
```



## Stata


Given a dataset with a "year" variable, generate a variable "leap" which is 1 for a leap year, 0 otherwise.


```stata
gen leap = mod(year,400)==0 | mod(year,4)==0 & mod(year,100)!=0
```


See also the article '''[https://www.stata.com/support/faqs/data-management/leap-year-indicators/ How do I identify leap years in Stata?]''' by Nicholas J. Cox in Stata FAQ.


## Swift


```Swift
func isLeapYear(year:Int) -> Bool {
   return (year % 100 == 0) ? (year % 400 == 0) : (year % 4 == 0)
}

print(isLeapYear(2000))
print(isLeapYear(2011))
```

```txt
true
false

```



## Tcl

The "classic" modulo comparison:

```tcl
proc isleap1 {year} {
    return [expr {($year % 4 == 0) && (($year % 100 != 0) || ($year % 400 == 0))}]
}
isleap1 1988 ;# => 1
isleap1 1989 ;# => 0
isleap1 1900 ;# => 0
isleap1 2000 ;# => 1
```

Does Feb 29 exist in the given year?  If not a leap year, the clock command will return "03-01".  (This code will switch to the Julian calendar for years before 1582.)

```tcl
proc isleap2 year {
    return [expr {[clock format [clock scan "$year-02-29" -format "%Y-%m-%d"] -format "%m-%d"] eq "02-29"}]
}
isleap2 1988 ;# => 1
isleap2 1989 ;# => 0
isleap2 1900 ;# => 0
isleap2 2000 ;# => 1
```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
LOOP year="1900'1994'1996'1997'2000",txt=""
SET dayoftheweek=DATE(number,29,2,year,number)
IF (dayoftheweek==0) SET txt="not "
PRINT year," is ",txt,"a leap year"
ENDLOOP
```

```txt

1900 is not a leap year
1994 is not a leap year
1996 is a leap year
1997 is not a leap year
2000 is a leap year

```



## uBasic/4tH

<lang>DO
  INPUT "Enter a year: "; y
  IF FUNC(_FNleap(y)) THEN
    PRINT y; " is a leap year"
  ELSE
    PRINT y; " is not a leap year"
  ENDIF
LOOP
END

_FNleap Param (1)
RETURN ((a@ % 4 = 0) * ((a@ % 400 = 0) + (a@ % 100 # 0)))
```


## UNIX Shell

Original Bourne:

```sh
leap() {
  if expr $1 % 4 >/dev/null; then return 1; fi
  if expr $1 % 100 >/dev/null; then return 0; fi
  if expr $1 % 400 >/dev/null; then return 1; fi
  return 0;
}
```


Using GNU date(1):

```sh
leap() {
  date -d "$1-02-29" >/dev/null 2>&1;
}
```


Defining a bash function <tt>is_leap</tt> which accepts a YEAR argument, and uses no IO redirection, nor any extra processes.

```sh
is_leap() {
  local year=$(( 10#${1:?'Missing year'} ))
  (( year % 4 == 0 && ( year % 100 != 0 || year % 400 == 0 ) )) && return 0
  return 1
}
```


Using the cal command: ''(note that this invokes two processes with IO piped between them and is relatively heavyweight compared to the above shell functions: leap and is_leap)''
<!--
-->
<!-- actually, it *is* correct for dates from 1752 and after.  Since the Gregorian calendar didn't exist prior to that date,
it doesn't make sense to hold this task accountable for years prior -->

```sh
leap() {
  cal 02 $1 | grep -q 29
}

```



## Ursa

This program takes a year as a command line argument.

```ursa
decl int year
set year (int args<1>)
if (= (mod year 4) 0)
        if (and (= (mod year 100) 0) (not (= (mod year 400) 0)))
                out year " is not a leap year" endl console
        else
                out year " is a leap year" endl  console
        end if
else
        out year " is not a leap year" endl console
end if
```

Output in Bash:

```txt
$ ursa leapyear.u 1900
1900 is not a leap year
$ ursa leapyear.u 2000
2000 is a leap year
```



## Vala


```Vala
void main() {
    DateYear[] years = {1900, 1994, 1996, 1997, 2000};
    foreach (DateYear year in years) {
        string status = year.is_leap_year() ? "" : "not ";
        print("%d is %sa leap year.\n", (int) year, status);
    }
}
```



## VBA


```vb
Public Function Leap_year(year As Integer) As Boolean
    Leap_year = (Month(DateSerial(year, 2, 29)) = 2)
End Function
```


## VBScript


```vb

Function IsLeapYear(yr)
	IsLeapYear = False
	If yr Mod 4 = 0 And (yr Mod 400 = 0 Or yr Mod 100 <> 0) Then
		IsLeapYear = True
	End If
End Function

'Testing the function.
arr_yr = Array(1900,1972,1997,2000,2001,2004)

For Each yr In arr_yr
	If IsLeapYear(yr) Then
		WScript.StdOut.WriteLine yr & " is leap year."
	Else
		WScript.StdOut.WriteLine yr & " is NOT leap year."
	End If
Next

```


```txt

1900 is NOT leap year.
1972 is leap year.
1997 is NOT leap year.
2000 is leap year.
2001 is NOT leap year.
2004 is leap year.

```



## Vedit macro language


```vedit
while (#1 = Get_Num("Year: ")) {
    #2 = (#1 % 4 == 0) && ((#1 % 100 != 0) || (#1 % 400 == 0))
    if (#2) {
        Message(" is leap year\n")
    } else {
	Message(" is not leap year\n")
    }
}
```


The following version requires Vedit 6.10 or later:

```vedit
while (#1 = Get_Num("Year: ")) {
    if (Is_Leap_Year(#1)) {
        Message(" is leap year\n")
    } else {
	Message(" is not leap year\n")
    }
}
```



## Visual Basic

```vb

Public Function IsLeapYear1(ByVal theYear As Integer) As Boolean
'this function utilizes documented behaviour of the built-in DateSerial function
IsLeapYear1 = (VBA.Day(VBA.DateSerial(theYear, 2, 29)) = 29)
End Function

Public Function IsLeapYear2(ByVal theYear As Integer) As Boolean
'this function uses the well-known formula
IsLeapYear2 = IIf(theYear Mod 100 = 0, theYear Mod 400 = 0, theYear Mod 4 = 0)
End Function

```

Testing:

```vb

Sub Main()
'testing the above functions
Dim i As Integer
  For i = 1750 To 2150
    Debug.Assert IsLeapYear1(i) Eqv IsLeapYear2(i)
  Next i
End Sub

```




## Visual Basic .NET

```vbnet
Module Module1

    Sub Main()
        For Each y In {1900, 1994, 1996, Date.Now.Year}
            Console.WriteLine("{0} is {1}a leap year.", y, If(Date.IsLeapYear(y), String.Empty, "not "))
        Next
    End Sub

End Module
```

```txt
1900 is not a leap year.
1994 is not a leap year.
1996 is a leap year.
2019 is not a leap year.
```



## WDTE


```wdte
let str =>
 import 'strings';

let multiple of n => == (% n of) 0;

let leap year => str.format '{} is{} a leap year.' year (switch year {
  multiple 400 => '';
  multiple 100 => ' not';
  multiple 4 => '';
  default => ' not';
}) -- io.writeln io.stdout;
```



## Wortel


```wortel
@let {
  isLeapYear !?{\~%%1H \~%%4H \~%%4}
  !-isLeapYear @range[1900 2000]
}
```

Returns:

```txt
[1904 1908 1912 1916 1920 1924 1928 1932 1936 1940 1944 1948 1952 1956 1960 1964 1968 1972 1976 1980 1984 1988 1992 1996 2000]
```



## X86 Assembly

Using FASM syntax. Leaf function fits nicely into your program.

```asm
    align 16
; Input year as signed dword in EAX
IsLeapYear:
    test eax,11b
    jz .4
    retn ; 75% : ZF=0, not a leap year
.4:
    mov ecx,100
    cdq
    idiv ecx
    test edx,edx
    jz .100
    cmp edx,edx
    retn ; 24% : ZF=1, leap year
.100:
    test eax,11b
    retn ; 1% : ZF=?, leap year if EAX%400=0
```



## XLISP


```xlisp
(DEFUN LEAP-YEARP (YEAR)
    (AND (= (MOD YEAR 4) 0) (OR (/= (MOD YEAR 100) 0) (= (MOD YEAR 400) 0))))

; Test the function
(DISPLAY (MAPCAR LEAP-YEARP '(1600 1640 1800 1928 1979 1990 2000 2004 2005 2016)))
```

```txt
(#T #T () #T () () #T #T () #T)
```



## XPL0


```XPL0
func LeapYear(Y);       \Return 'true' if Y is a leap year
int Y;
[if rem(Y/100)=0 then return rem(Y/400)=0;
return rem(Y/4)=0;
];
```



## Yorick

This solution is vectorized and can be applied to scalar or array input.

```yorick
func is_leap(y) {
  return ((y % 4 == 0) & (y % 100 != 0)) | (y % 400 == 0);
}
```

Interactive example usage:

```txt
> is_leap(1988)
1
> is_leap([1988,1989,1900,2000])
[1,0,0,1]
```



## zkl


```zkl
Time.Date.isLeapYear(1988) //-->True
T(1988,1989,1900,2000).apply(Time.Date.isLeapYear)
    //-->L(True,False,False,True)
```


