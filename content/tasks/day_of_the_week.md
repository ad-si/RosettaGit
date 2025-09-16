+++
title = "Day of the week"
description = ""
date = 2019-10-18T19:43:06Z
aliases = []
[extra]
id = 3249
[taxonomies]
categories = ["task", "Date and time"]
tags = []
languages = [
  "11l",
  "360_assembly",
  "abap",
  "ada",
  "algol_68",
  "applescript",
  "arc",
  "arturo",
  "autohotkey",
  "autoit",
  "awk",
  "bacon",
  "basic",
  "batch_file",
  "bbc_basic",
  "bc",
  "befunge",
  "bracmat",
  "c",
  "clojure",
  "cobol",
  "coffeescript",
  "coldfusion",
  "common_lisp",
  "component_pascal",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "ecl",
  "elixir",
  "erlang",
  "erre",
  "euphoria",
  "factor",
  "fbsl",
  "forth",
  "fortran",
  "freebasic",
  "gambas",
  "gap",
  "go",
  "groovy",
  "haskell",
  "hicest",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "k",
  "kotlin",
  "lasso",
  "liberty_basic",
  "lingo",
  "livecode",
  "logo",
  "lua",
  "m2000_interpreter",
  "m4",
  "maple",
  "maxima",
  "mumps",
  "netrexx",
  "nim",
  "ocaml",
  "oforth",
  "oorexx",
  "pari_gp",
  "pascal",
  "peloton",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pike",
  "pl_i",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "rebol",
  "red",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "sas",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "sinclair_zx81_basic",
  "smalltalk",
  "sql",
  "standard_ml",
  "stata",
  "suneido",
  "swift",
  "tcl",
  "tuscript",
  "unix_shell",
  "ursala",
  "vba",
  "vbscript",
  "vedit_macro_language",
  "visual_objects",
  "wortel",
  "xpl0",
  "yabasic",
  "zkl",
  "zonnon",
  "zx_spectrum_basic",
]
+++

A company decides that whenever Xmas falls on a Sunday they will give their workers all extra paid holidays so that, together with any public holidays, workers will not have to work the following week (between the 25th of December and the first of January).


## Task

'''In what years between 2008 and 2121 will the 25th of December be a Sunday?'''

Using any standard date handling libraries of your programming language;
compare the dates calculated with the output of other languages to discover any anomalies in the handling of dates which may be due to, for example, overflow in types used to represent dates/times similar to   [[wp:Y2k#See_also|y2k]]   type problems.





## 11l


```11l
print((2008..2121).filter(y -> Time(y, 12, 25).strftime(‘%w’) == ‘0’))
```

```txt

[2011, 2016, 2022, 2033, 2039, 2044, 2050, 2061, 2067, 2072, 2078, 2089, 2095, 2101, 2107, 2112, 2118]

```



## 360 Assembly

The program uses two ASSIST macro (XDECO,XPRNT) to keep the code as short as possible.

```360asm
*        Day of the week           06/07/2016
DOW      CSECT
         USING  DOW,R15            base register
         LA     R6,2008            year=2008
LOOP     C      R6,=F'2121'        do year=2008 to 2121
         BH     ELOOP              .
         LR     R7,R6              y=year
         LA     R8,12              m=12
         LA     R9,25              d=25
         C      R8,=F'3'           if m<3
         BNL    MGE3               then
         LA     R8,12(R8)            m=m+12
         BCTR   R7,0                 y=y-1
MGE3     LR     R10,R7             y
         SRDA   R10,32             .
         D      R10,=F'100'        r=y//100 ; l=y/100
         LR     R3,R8              m
         LA     R3,1(R3)           m+1
         M      R2,=F'26'          *26
         D      R2,=F'10'          /10
         AR     R3,R9              +d
         AR     R3,R10             +r
         LR     R2,R10             r
         SRA    R2,2               /4
         AR     R2,R3              (d+(m+1)*26/10+r+r/4
         LR     R3,R11             l
         SRA    R3,2               /4
         AR     R2,R3              (d+(m+1)*26/10+r+r/4+l/4
         LA     R5,5               5
         MR     R4,R11             *l
         AR     R2,R5              (d+(m+1)*26/10+r+r/4+l/4+5*l)
         SRDA   R2,32              .
         D      R2,=F'7'           w=(d+(m+1)*26/10+r+r/4+l/4+5*l)//7
         C      R2,=F'1'           if w=1  (sunday)
         BNE    WNE1               then
         XDECO  R6,PG                edit year
         XPRNT  PG,12                print year
WNE1     LA     R6,1(R6)           year=year+1
         B      LOOP               next year
ELOOP    BR     R14                exit
PG       DS     CL12               buffer
         YREGS
         END    DOW
```

```txt
        2011
        2016
        2022
        2033
        2039
        2044
        2050
        2061
        2067
        2072
        2078
        2089
        2095
        2101
        2107
        2112
        2118
```



## ABAP


```ABAP
report zday_of_week
data: lv_start type i value 2007,
      lv_n type i value 114,
      lv_date type sy-datum,
      lv_weekday type string,
      lv_day type c,
      lv_year type n length 4.

write 'December 25 is a Sunday in: '.
do lv_n times.
   lv_year = lv_start + sy-index.
   concatenate lv_year '12' '25' into lv_date.
   call function 'DATE_COMPUTE_DAY'
    exporting date = lv_date
    importing day  = lv_day.

   select single langt from t246 into lv_weekday
     where sprsl = sy-langu and
     wotnr = lv_day.

   if lv_weekday eq 'Sunday'.
     write / lv_year.
   endif.
enddo.

```

```txt
December 25 is a Sunday in:
2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118
```


## Action!

Action! does not have a standard library providing a day of week function, therefore an adaptation of [https://en.wikipedia.org/wiki/Determination_of_the_day_of_the_week#Sakamoto.27s_methods Sakamoto's method] to determine the day of week for a given date using integer arithmetic is used.

```Action!
Byte FUNC DayOfWeek(BYTE day, month CARD year BYTE century)
CARD weekday
BYTE ARRAY index=[0 3 2 5 0 3 5 1 4 6 2 4]

IF year < 100  THEN
   year = year + century * 100
FI

IF year < 1753 THEN RETURN(7) FI

IF month < 3 THEN
   year==-1
FI

month = index(month-1)
weekday=year + year/4 - year/100 + year/400 + month + day
weekday = weekday MOD 7
RETURN (weekday)

PROC main()
CARD y
PrintE("December 25 is a Sunday in:")
FOR y = 2008 to 2121
DO
IF DayOfWeek(25, 12, y)=0 THEN
PrintCE(y)
FI
OD
RETURN
```

```txt
December 25 is a Sunday in:
2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118
```




## Ada


```ada
with Ada.Calendar.Formatting;  use Ada.Calendar.Formatting;
with Ada.Text_IO;              use Ada.Text_IO;

procedure Yuletide is
begin
   for Year in 2008..2121 loop
      if Day_Of_Week (Time_Of (Year, 12, 25)) = Sunday then
         Put_Line (Image (Time_Of (Year, 12, 25)));
      end if;
   end loop;
end Yuletide;
```

```txt
2011-12-25 00:00:00
2016-12-25 00:00:00
2022-12-25 00:00:00
2033-12-25 00:00:00
2039-12-25 00:00:00
2044-12-25 00:00:00
2050-12-25 00:00:00
2061-12-25 00:00:00
2067-12-25 00:00:00
2072-12-25 00:00:00
2078-12-25 00:00:00
2089-12-25 00:00:00
2095-12-25 00:00:00
2101-12-25 00:00:00
2107-12-25 00:00:00
2112-12-25 00:00:00
2118-12-25 00:00:00
```



## ALGOL 68

```algol68
# example from: http://www.xs4all.nl/~jmvdveer/algol.html - GPL #
INT sun=0 # , mon=1, tue=2, wed=3, thu=4, fri=5, sat=6 #;

PROC day of week = (INT year, month, day) INT: (
  # Day of the week by Zeller’s Congruence algorithm from 1887 #
  INT y := year, m := month, d := day, c;
  IF m <= 2 THEN
    m +:= 12; y -:= 1
  FI;
  c := y OVER 100;
  y %*:= 100;
  (d - 1 + ((m + 1) * 26) OVER 10 + y + y OVER 4 + c OVER 4 - 2 * c) MOD 7
);

test:(
  print("December 25th is a Sunday in:");
  FOR year FROM 2008 TO 2121 DO
    INT wd = day of week(year, 12, 25);
    IF wd = sun THEN print(whole(year,-5)) FI
  OD;
  new line(stand out)
)

```

```txt

December 25th is a Sunday in: 2011 2016 2022 2033 2039 2044 2050 2061 2067 2072 2078 2089 2095 2101 2107 2112 2118

```


=={{header|ALGOL-M}}==

```algol

BEGIN

% CALCULATE P MOD Q %
INTEGER FUNCTION MOD(P, Q);
INTEGER P, Q;
BEGIN
   MOD := P - Q * (P / Q);
END;

COMMENT
  RETURN DAY OF WEEK (SUN=0, MON=1, ETC.) FOR A GIVEN
  GREGORIAN CALENDAR DATE USING ZELLER'S CONGRUENCE;
INTEGER FUNCTION DAYOFWEEK(MO, DA, YR);
INTEGER MO, DA, YR;
BEGIN
  INTEGER Y, C, Z;
  IF MO < 3 THEN
    BEGIN
      MO := MO + 10;
      YR := YR - 1;
    END
  ELSE MO := MO - 2;
  Y := MOD(YR, 100);
  C := YR / 100;
  Z := (26 * MO - 2) / 10;
  Z := Z + DA + Y + (Y / 4) + (C /4) - 2 * C + 777;
  DAYOFWEEK := MOD(Z, 7);
END;

% MAIN PROGRAM STARTS HERE %
INTEGER YEAR, SUNDAY;
SUNDAY := 0;
WRITE("CHRISTMAS WILL FALL ON A SUNDAY IN THESE YEARS:");
FOR YEAR := 2008 STEP 1 UNTIL 2121 DO
  BEGIN
    IF DAYOFWEEK(12, 25, YEAR) = SUNDAY THEN
       WRITE(YEAR);
  END;

END

```

```txt

CHRISTMAS WILL FALL ON A SUNDAY IN THESE YEARS:
  2011
  2016
  2022
  2033
  2039
  2044
  2050
  2061
  2067
  2072
  2078
  2089
  2095
  2101
  2107
  2112
  2118

```



## AppleScript


```applescript
set ChristmasSundays to {}
set Christmas to (current date)
set month of Christmas to December
set day of Christmas to 25
repeat with year from 2008 to 2121
	set year of Christmas to year
	if weekday of Christmas is Sunday then set end of ChristmasSundays to year
end repeat
ChristmasSundays
```



Or, composing generic functions:

```applescript
on run
    -- xmasOnSunday :: Int -> Bool
    script xmasOnSunday
        on |λ|(y)
            tell (current date)
                set {its year, its month, its day, its time} to {y, 12, 25, 0}
                return its weekday is Sunday
            end tell
        end |λ|
    end script

    filter(xmasOnSunday, enumFromTo(2008, 2121))
end run


-- GENERIC FUNCTIONS ----------------------------------------------------------

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if m ≤ n then
        set lst to {}
        repeat with i from m to n
            set end of lst to i
        end repeat
        return lst
    else
        return {}
    end if
end enumFromTo

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

```AppleScript
{2011, 2016, 2022, 2033, 2039, 2044, 2050, 2061, 2067,
2072, 2078, 2089, 2095, 2101, 2107, 2112, 2118}
```



## Arc


```arc

(= day-names '(Sunday Monday Tuesday Wednesday Thursday Friday Saturday))
(= get-weekday-num (fn (year month day)
   (= helper '(0 3 2 5 0 3 5 1 4 6 2 4))
   (if (< month 3) (= year (- year 1)))
   (mod (+ year (helper (- month 1)) day
        (apply + (map [trunc (/ year _)] '(4 -100 400))))
   7)))
(= get-weekday-name (fn (weekday-num) (day-names weekday-num)))

```

<b>test:</b>

```arc
(up i 2008 2121
  (when (is 0 (get-weekday-num i 12 25))
    (prn i)))

2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118


```



## Arturo



```arturo
sundays $(filter $(range 2008 2121) { $(day &+"-12-25")="sun" })

print sundays
```


```txt
#(2011 2016 2022 2033 2039 2044 2050 2061 2067 2072 2078 2089 2095 2101 2107 2112 2118)
```



## AutoHotkey


```autohotkey
year = 2008
stop = 2121

While year <= stop {
 FormatTime, day,% year 1225, dddd
 If day = Sunday
  out .= year "`n"
 year++
}
MsgBox,% out
```



## AutoIt


```AutoIt
#include <date.au3>

Const $iSunday = 1
For $iYear = 2008 To 2121 Step 1
   If $iSunday = _DateToDayOfWeek($iYear, 12, 25) Then
     ConsoleWrite(StringFormat($iYear & "\n"))
   EndIf
Next
```



## AWK



```AWK

# syntax: GAWK -f DAY_OF_THE_WEEK.AWK
# runtime does not support years > 2037 on my 32-bit Windows XP O/S
BEGIN {
    for (i=2008; i<=2121; i++) {
      x = strftime("%Y/%m/%d %a",mktime(sprintf("%d 12 25 0 0 0",i)))
      if (x ~ /Sun/) { print(x) }
    }
}

```



## BASIC

<b>Works with:</b> FreeBASIC

This program needs the modulo function because there is a bug in the built in modulo function.


```BASIC
Declare Function modulo(x As Double, y As Double) As Double
Declare Function wd(m As Double, d As Double, y As Double) As Integer

Cls
Dim yr As Double
For yr = 2008 To 2121
	If wd(12,25,yr) = 1 Then
		Print "Dec " & 25 & ", " & yr
	EndIf
Next
Sleep

Function modulo(x As Double, y As Double) As Double
	If y = 0 Then
		Return x
	Else
		Return x - y * Int(x / y)
	End If
End Function

Function wd(m As Double, d As Double, y As Double) As Integer
	If m = 1 Or m = 2 Then
		m += 12
		y-= 1
	End If
	Return modulo(365 * y + Fix(y / 4) - Fix(y / 100) + Fix(y / 400) + d  + Fix((153 * m + 8) / 5), 7) + 1
End Function

Dec 25, 2011
Dec 25, 2016
Dec 25, 2022
Dec 25, 2033
Dec 25, 2039
Dec 25, 2044
Dec 25, 2050
Dec 25, 2061
Dec 25, 2067
Dec 25, 2072
Dec 25, 2078
Dec 25, 2089
Dec 25, 2095
Dec 25, 2101
Dec 25, 2107
Dec 25, 2112
Dec 25, 2118
```


=
## BaCon
=

```freebasic
' Sunday Christmas
PRINT "Years with Christmas on a Sunday"
FOR y = 2008 TO 2121
    tv = TIMEVALUE(y, 12, 25, 0, 0, 0)
    IF WEEKDAY$(tv) = "Sunday" THEN PRINT y
NEXT
```


```txt
prompt$ ./sunday-christmas
Years with Christmas on a Sunday
2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118
```


=
## FreeBASIC
=

```FreeBASIC
' version 17-06-2015
' compile with: fbc -s console

Function wd(m As Integer, d As Integer, y As Integer) As Integer
  If m < 3 Then        ' If m = 1 Or m = 2 Then
    m += 12
    y -= 1
  End If
  Return (y + (y \ 4) - (y \ 100) + (y \ 400) + d + ((153 * m + 8) \ 5)) Mod 7
End Function

' ------=< MAIN >=------

For yr As Integer = 2008 To 2121
  If wd(12, 25, yr) = 0 Then
    Print "Dec 25 "; yr
  EndIf
Next

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
Dec 25  2011
Dec 25  2016
Dec 25  2022
Dec 25  2033
Dec 25  2039
Dec 25  2044
Dec 25  2050
Dec 25  2061
Dec 25  2067
Dec 25  2072
Dec 25  2078
Dec 25  2089
Dec 25  2095
Dec 25  2101
Dec 25  2107
Dec 25  2112
Dec 25  2118
```


```FreeBASIC
' version 17-06-2015
' Weekday And DateSerial only works with #Include "vbcompat.bi"
' compile with: fbc -s console

#Include Once "vbcompat.bi"
Dim As Double a

For yr As Integer = 2008 To 2121
  a = DateSerial (yr, 12, 25)
  If Weekday(a) = 1 Then Print Format(a, "dd-mm-yyyy")   ' 1 = sunday, 2 = monday ...
Next

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
25-12-2011
25-12-2016
25-12-2022
25-12-2033
25-12-2039
25-12-2044
25-12-2050
25-12-2061
25-12-2067
25-12-2072
25-12-2078
25-12-2089
25-12-2095
25-12-2101
25-12-2107
25-12-2112
25-12-2118
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Dayweek.bas"
110 PRINT "The years between 2008 and 2121 will the 25th of December be a Sunday:"
120 FOR Y=2008 TO 2121
130   IF DAYWEEK(Y,12,25)=0 THEN PRINT "Dec 25,";Y
140 NEXT
150 DEF DAYWEEK(Y,M,D)
160   LET A=INT((14-M)/12):LET Y=Y-A
170   LET W=D+INT((13*(M+12*A-2)-1)/5)+Y+INT(Y/4)-INT(Y/100)+INT(Y/400)
180   LET DAYWEEK=W-7*INT(W/7)
190 END DEF
```


=
## Sinclair ZX81 BASIC
=
Works with 1k of RAM. Follows the C code quite closely: the only factors that perhaps make it less readable are (a) the absence of a modulo operator and (b) the need for continual calls to <code>INT</code> because we don't have an integer type. The performance is pretty acceptable; seconds rather than minutes.

```zxbasic
 10 LET M=12
 20 LET D=25
 30 FOR Y=2008 TO 2121
 40 GOSUB 80
 50 IF W=0 THEN PRINT Y
 60 NEXT Y
 70 STOP
 80 LET A=INT ((14-M)/12)
 90 LET MM=M+12*A-2
100 LET YY=Y-A
110 LET W=D+INT ((13*MM-1)/5)+YY+INT (YY/4)-INT (YY/100)+INT (YY/400)
120 LET W=W-7*INT (W/7)
130 RETURN
```

```txt
2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118
```



## Batch File


```dos

:: Day of the Week task from Rosetta Code Wiki
:: Batch File Implementation
::
:: In what years between 2008 and 2121 will the 25th of December be a Sunday?
::
:: This implementation uses Zeller's Rule...

@echo off

::Set month code for December
set mon=33

::Set day number
set day=25

for /L %%w in (2008,1,2121) do (
call :check_day %%w
)
pause>nul
exit /b

:check_day
set yr=%1
set /a a=%yr%/100
set /a b=%yr%-(%a%*100)
set /a weekday=(%day%+%mon%+%b%+(%b%/4)+(%a%/4)+(5*%a%))%%7
if %weekday%==1 (
echo Dec 25, %yr% is a Sunday.
)
goto :EOF

```

```txt

Dec 25, 2011 is a Sunday.
Dec 25, 2016 is a Sunday.
Dec 25, 2022 is a Sunday.
Dec 25, 2033 is a Sunday.
Dec 25, 2039 is a Sunday.
Dec 25, 2044 is a Sunday.
Dec 25, 2050 is a Sunday.
Dec 25, 2061 is a Sunday.
Dec 25, 2067 is a Sunday.
Dec 25, 2072 is a Sunday.
Dec 25, 2078 is a Sunday.
Dec 25, 2089 is a Sunday.
Dec 25, 2095 is a Sunday.
Dec 25, 2101 is a Sunday.
Dec 25, 2107 is a Sunday.
Dec 25, 2112 is a Sunday.
Dec 25, 2118 is a Sunday.

```



## BBC BASIC

```bbcbasic
      INSTALL @lib$+"DATELIB"

      FOR year% = 2008 TO 2121
        IF FN_dow(FN_mjd(25, 12, year%)) = 0 THEN
          PRINT "Christmas Day is a Sunday in "; year%
        ENDIF
      NEXT
```



## bc

Because ''bc'' has no date library, this program uses [http://mathforum.org/library/drmath/view/62324.html ''Zeller's rule''], also known as [http://www.merlyn.demon.co.uk/zel-like.htm ''Zeller's congruence''], to calculate day of week.


```bc
scale = 0

/*
 * Returns day of week (0 to 6) for year, month m, day d in proleptic
 * Gregorian calendar. Sunday is 0. Assumes y >= 1, scale = 0.
 */
define w(y, m, d) {
	auto a

	/* Calculate Zeller's congruence. */
	a = (14 - m) / 12
	m += 12 * a
	y -= a
	return ((d + (13 * m + 8) / 5 +			\
		 y + y / 4 - y / 100 + y / 400) % 7)
}

for (y = 2008; y <= 2121; y++) {
	/* If December 25 is a Sunday, print year. */
	if (w(y, 12, 25) == 0) y
}
quit
```



## Befunge

Befunge doesn't have any standard date-handling functionality, so we calculate the day of the week directly using a simple variation of the Zeller rule.


```befunge
8
:"2("*+::::4/+\"d"/-\45v
@_^#`"y": +1$<_v#%7+1+/*:*<
>:#,_>$:.55+,^ >0" ,52 ceD"
```


```txt
Dec 25, 2011
Dec 25, 2016
Dec 25, 2022
Dec 25, 2033
Dec 25, 2039
Dec 25, 2044
Dec 25, 2050
Dec 25, 2061
Dec 25, 2067
Dec 25, 2072
Dec 25, 2078
Dec 25, 2089
Dec 25, 2095
Dec 25, 2101
Dec 25, 2107
Dec 25, 2112
Dec 25, 2118
```



## Bracmat

```bracmat
{ Calculate day of week in proleptic Gregorian calendar. Sunday == 0. }
    ( wday
    =   year month day adjustment mm yy
      .   !arg:(?year,?month,?day)
        & div$(14+-1*!month,12):?adjustment
        & !month+12*!adjustment+-2:?mm
        & !year+-1*!adjustment:?yy
        &   mod
          $ (   !day
              + div$(13*!mm+-1,5)
              + !yy
              + div$(!yy,4)
              + -1*div$(!yy,100)
              + div$(!yy,400)
            , 7
            )
    )
& 2008:?y
&   whl
  ' ( !y:~>2121
    & (   wday$(!y,12,25):0
        & put$(str$(!y "-12-25\n"))
      |
      )
    & 1+!y:?y
    )
& done;
```

```txt
2011-12-25
2016-12-25
2022-12-25
2033-12-25
2039-12-25
2044-12-25
2050-12-25
2061-12-25
2067-12-25
2072-12-25
2078-12-25
2089-12-25
2095-12-25
2101-12-25
2107-12-25
2112-12-25
2118-12-25
```



## C

Because of problems with various C libraries (such as ''time_t'' overflowing during 2038, or strptime() or mktime() not filling in ''tm_wday''), this program uses [http://mathforum.org/library/drmath/view/62324.html Zeller's Rule] to calculate day of week.


```c
#include <stdio.h>

/* Calculate day of week in proleptic Gregorian calendar. Sunday == 0. */
int wday(int year, int month, int day)
{
	int adjustment, mm, yy;

	adjustment = (14 - month) / 12;
	mm = month + 12 * adjustment - 2;
	yy = year - adjustment;
	return (day + (13 * mm - 1) / 5 +
		yy + yy / 4 - yy / 100 + yy / 400) % 7;
}

int main()
{
	int y;

	for (y = 2008; y <= 2121; y++) {
		if (wday(y, 12, 25) == 0) printf("%04d-12-25\n", y);
	}

	return 0;
}
```



## C++


```cpp
#include <boost/date_time/gregorian/gregorian.hpp>
#include <iostream>

int main( ) {
   using namespace boost::gregorian ;

   std::cout
      << "Yuletide holidays must be allowed in the following years:\n" ;
   for ( int i = 2008 ; i < 2121 ; i++ ) {
      greg_year gy = i ;
      date d  ( gy, Dec , 25 ) ;
      if ( d.day_of_week( ) == Sunday ) {
	 std::cout << i << std::endl ;
      }
   }
   std::cout << "\n" ;
   return 0 ;
}
```

```txt

Yuletide holidays must be allowed in the following years:
2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118

```


## C#

```c#
using System;

class Program
{
    static void Main(string[] args)
    {
        for (int i = 2008; i <= 2121; i++)
        {
            DateTime date = new DateTime(i, 12, 25);
            if (date.DayOfWeek == DayOfWeek.Sunday)
            {
                Console.WriteLine(date.ToString("dd MMM yyyy"));
            }
        }
    }
}
```


Using LINQ:

```c#
using System;
using System.Linq;

class Program
{
    static void Main(string[] args)
    {
        string[] days = Enumerable.Range(2008, 2121 - 2007)
            .Select(year => new DateTime(year, 12, 25))
            .Where(day => day.DayOfWeek == DayOfWeek.Sunday)
            .Select(day => day.ToString("dd MMM yyyy")).ToArray();

        foreach (string day in days) Console.WriteLine(day);
    }
}
```
Lambda expressions FTW:

```c#
using System;
using System.Linq;

class Program
{
    static void Main(string[] args)
    {
        Enumerable.Range(2008, 113).ToList()
        .ConvertAll(ent => new DateTime(ent, 12, 25))
        .Where(ent => ent.DayOfWeek.Equals(DayOfWeek.Sunday)).ToList()
        .ForEach(ent => Console.WriteLine(ent.ToString("dd MMM yyyy")));
    }
}
```

```txt
25 Dec 2011
25 Dec 2016
25 Dec 2022
25 Dec 2033
25 Dec 2039
25 Dec 2044
25 Dec 2050
25 Dec 2061
25 Dec 2067
25 Dec 2072
25 Dec 2078
25 Dec 2089
25 Dec 2095
25 Dec 2101
25 Dec 2107
25 Dec 2112
25 Dec 2118
```



## Clojure

Utilizing Java interop


```clojure

(import '(java.util GregorianCalendar))
(defn yuletide [start end]
	    (filter #(= (. (new GregorianCalendar %
			 (. GregorianCalendar DECEMBER) 25) get (. GregorianCalendar DAY_OF_WEEK))
		 (. GregorianCalendar SUNDAY)) (range start (inc end))))

(yuletide 2008 2121)

```


```txt
(2011 2016 2022 2033 2039 2044 2050 2061 2067 2072 2078 2089 2095 2101 2107 2112 2118)
```



## COBOL

Using Date Intrinsic Functions

```COBOL

       program-id. dec25.
       data division.
       working-storage section.
       1 work-date.
        2 yr pic 9(4) value 2008.
        2 mo-da pic 9(4) value 1225. *> Dec 25
       1 wk-date redefines work-date pic 9(8).
       1 binary.
        2 int-date pic 9(8).
        2 dow pic 9(4).
       procedure division.
           perform varying yr from 2008 by 1
           until yr > 2121
               compute int-date = function integer-of-date (wk-date)
               compute dow = function mod ((int-date - 1) 7) + 1
               if dow = 7  *> Sunday = 7 per ISO 8601 and ISO 1989
                   display yr
               end-if
           end-perform
           stop run
           .
       end program dec25.

```

```txt
2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118

```


Without Date Intrinsic Functions


```Cobol

       identification division.
       program-id. dowtest.
       data division.
       working-storage section.
       01  ws-inp-date   pic x(08).
       01  filler redefines ws-inp-date.
         03  ws-inp-year  pic 9(04).
       01  ws-dow        pic 9(05).
       procedure division.
           move '00001225' to ws-inp-date
           perform test before
           varying ws-inp-year from 2008 by +1
           until ws-inp-year > 2121
             call "todow" using
                 by reference ws-inp-date
                 by reference ws-dow
                 if ws-dow = 1 then
                   display 'year=' ws-inp-year
                 end-if
           end-perform
           stop run.

       end program dowtest.

       identification division.
       program-id.  todow.
       environment division.
       input-output section.
       file-control.
       data division.
       file section.
       working-storage section.
       01 tally pic 9(05).
       01  wms-work-area.
         03  wms-year       pic 9(04).
         03  wms-month      pic 9(02).
         03  wms-csys       pic 9(01) value 1.
         03  wms-sum        pic 9(05).
       linkage section.
       01  lkip-date.
         03  lkip-date-year     pic 9(04).
         03  lkip-date-month    pic 9(02).
         03  lkip-date-day      pic 9(02).
       01  lkop-dow             pic 9(05).
         88  lkop-sunday                   value 1.
       procedure division using
           by reference lkip-date
           by reference lkop-dow
           .

           if lkip-date-month < 3
             compute wms-month = lkip-date-month + 12
             compute wms-year  = lkip-date-year - 1
           else
             compute wms-month = lkip-date-month
             compute wms-year  = lkip-date-year
           end-if

          compute wms-sum    =
                          ( lkip-date-day + 2 * wms-month + wms-year
                          + function integer (6 * (wms-month + 1) / 10)
                          + function integer ( wms-year / 4   )
                          - function integer ( wms-year / 100 )
                          + function integer ( wms-year / 400 )
                          + wms-csys )
         compute lkop-dow = function mod (wms-sum, 7) + 1
                          .
       end program todow.

```

```txt
year=2011
year=2016
year=2022
year=2033
year=2039
year=2044
year=2050
year=2061
year=2067
year=2072
year=2078
year=2089
year=2095
year=2101
year=2107
year=2112
year=2118

```



## CoffeeScript


```coffeescript
december = 11 # gotta love Date APIs :)
sunday = 0
for year in [2008..2121]
  xmas = new Date year, december, 25
  console.log year if xmas.getDay() is sunday
```
one-liner:
```coffeescript
console.log year for year in [2008...2121] when new Date(year, 11, 25).getDay() is 0
```

```txt
2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118

```



## ColdFusion


```ColdFusion

<cfloop from = "2008" to = "2121" index = "i">
    <cfset myDate = createDate(i, 12, 25) />
    <cfif dayOfWeek(myDate) eq 1>
        December 25th falls on a Sunday in <cfoutput>#i#</cfoutput><br />
    </cfif>
</cfloop>

```



## Common Lisp


```lisp
(loop for year from 2008 upto 2121
   when (= 6 (multiple-value-bind
                   (second minute hour date month year day-of-week dst-p tz)
                 (decode-universal-time (encode-universal-time 0 0 0 25 12 year))
               (declare (ignore second minute hour date month year dst-p tz))
               day-of-week))
     collect year)
```



```lisp
(loop for year from 2008 upto 2121
   for xmas = (encode-universal-time 0 0 0 25 12 year)
   for day  = (nth-value 6 (decode-universal-time xmas))
   when (= day 6) collect year)
```



## Component Pascal

```oberon2

MODULE DayOfWeek;
IMPORT DevCommanders, TextMappers, Dates, StdLog;

PROCEDURE XmastOnSun(s,e: INTEGER);
VAR
	i: INTEGER;
	d: Dates.Date;
BEGIN
	i := s;d.day := 25;d.month := 12;
	WHILE i < e DO
		d.year := i;
		IF Dates.DayOfWeek(d) = Dates.sunday THEN
			StdLog.Int(i);StdLog.Ln
		END;
		INC(i)
	END
END XmastOnSun;

PROCEDURE Do*;
VAR
	s: TextMappers.Scanner;
	r: ARRAY 2 OF INTEGER;
	i: INTEGER;
BEGIN
	s.ConnectTo(DevCommanders.par.text);
	s.SetPos(DevCommanders.par.beg);
	s.Scan;i := 0;
	WHILE ~s.rider.eot DO
		IF s.type = TextMappers.int THEN
			r[i] := s.int; INC(i)
		END;
		s.Scan
	END;
	XmastOnSun(r[0],r[1]);
END Do;

END DayOfWeek.

```

Execute: ^Q DayOfWeek.Do 2008 2121~
```txt

 2011
 2016
 2022
 2033
 2039
 2044
 2050
 2061
 2067
 2072
 2078
 2089
 2095
 2101
 2107
 2112
 2118

```



## D


```d
void main() {
    import std.stdio, std.range, std.algorithm, std.datetime;

    writeln("Christmas comes on a Sunday in the years:\n",
            iota(2008, 2122)
            .filter!(y => Date(y, 12, 25).dayOfWeek == DayOfWeek.sun));
}
```

```txt
Christmas comes on a Sunday in the years:
[2011, 2016, 2022, 2033, 2039, 2044, 2050, 2061, 2067, 2072, 2078, 2089, 2095, 2101, 2107, 2112, 2118]
```



## Delphi

{{libheader|sysutils}} always in uses clause in Delphi

```delphi
procedure IsXmasSunday(fromyear, toyear: integer);
var
i: integer;
TestDate: TDateTime;
outputyears: string;
begin
outputyears := '';
  for i:= fromyear to toyear do
  begin
    TestDate := EncodeDate(i,12,25);
    if dayofweek(TestDate) = 1 then
    begin
      outputyears := outputyears + inttostr(i) + ' ';
    end;
  end;
  //CONSOLE
  //writeln(outputyears);
  //GUI
  form1.label1.caption := outputyears;
end;
```


Procedure called with year range to test and outputs a space-delimited array of years to a label. There is no error check that fromyear < toyear, but this is easily added.

```txt

2011 2016 2022 2033 2039 2044 2050 2061 2067 2072 2078 2089 2095 2101 2107 2112 2118

```



## ECL


```ECL
//In what years between 2008 and 2121 will the 25th of December be a Sunday?

IMPORT STD;

BaseYear := 2008;
EndYear  := 2121;

ChristmasDay := RECORD
  UNSIGNED1 DayofWeek;
  UNSIGNED2 Year;
END;

ChristmasDay FindDate(INTEGER Ctr) := TRANSFORM
  SELF.DayofWeek := (STD.Date.FromGregorianYMD((BaseYear-1) + Ctr,12,25)) % 7; //0=Sunday
  SELF.Year := (BaseYear-1) + Ctr;
END;

YearDS := DATASET(EndYear-BaseYear,FindDate(COUNTER));
OUTPUT(YearDS(DayofWeek=0),{Year});

/* Outputs:
   2011
   2016
   2022
   2033
   2039
   2044
   2050
   2061
   2067
   2072
   2078
   2089
   2095
   2101
   2107
   2112
   2118
*/

```

This code solves a specific task, but can easily be modified as a generic function to return the DayOfWeek for any day after 1 AD.


## Elixir

```elixir
Enum.each(2008..2121, fn year ->
  wday = Date.from_erl!({year, 12, 25}) |> Date.day_of_week
  if wday==7, do: IO.puts "25 December #{year} is sunday"
end)
```


```txt

25 December 2011 is sunday
25 December 2016 is sunday
25 December 2022 is sunday
25 December 2033 is sunday
25 December 2039 is sunday
25 December 2044 is sunday
25 December 2050 is sunday
25 December 2061 is sunday
25 December 2067 is sunday
25 December 2072 is sunday
25 December 2078 is sunday
25 December 2089 is sunday
25 December 2095 is sunday
25 December 2101 is sunday
25 December 2107 is sunday
25 December 2112 is sunday
25 December 2118 is sunday

```



## Erlang



```erlang
% Implemented by bengt kleberg
-module(yuletide).
-export([main/0, sunday_years/2]).

main() ->
	[io:fwrite("25 December ~p is Sunday~n", [X]) || X <- sunday_years(2008, 2121)].

sunday_years( Start, Stop ) ->
	[X || X <- lists:seq(Start, Stop), is_sunday(calendar:day_of_the_week({X, 12, 25}))].

is_sunday( 7 ) -> true;
is_sunday( _ ) -> false.
```

```txt

25 December 2011 is Sunday
25 December 2016 is Sunday
25 December 2022 is Sunday
25 December 2033 is Sunday
25 December 2039 is Sunday
25 December 2044 is Sunday
25 December 2050 is Sunday
25 December 2061 is Sunday
25 December 2067 is Sunday
25 December 2072 is Sunday
25 December 2078 is Sunday
25 December 2089 is Sunday
25 December 2095 is Sunday
25 December 2101 is Sunday
25 December 2107 is Sunday
25 December 2112 is Sunday
25 December 2118 is Sunday

```



## ERRE


```ERRE

PROGRAM DAY_OF_THE_WEEK

PROCEDURE MODULO(X,Y->RES)
   IF Y=0 THEN
      RES=X
     ELSE
      RES=X-Y*INT(X/Y)
   END IF
END PROCEDURE

PROCEDURE WD(M,D,Y->RES%)
   IF M=1 OR M=2 THEN
     M+=12
     Y-=1
   END IF
   MODULO(365*Y+INT(Y/4)-INT(Y/100)+INT(Y/400)+D+INT((153*M+8)/5),7->RES)
   RES%=RES+1.0
END PROCEDURE

BEGIN
PRINT(CHR$(12);) ! CLS
FOR YR=2008 TO 2121 DO
    WD(12,25,YR->RES%)
    IF RES%=1 THEN  ! day 1 is Sunday......
      PRINT("Dec";25;",";YR)
    END IF
END FOR
GET(K$)
END PROGRAM

```

```txt
Dec 25, 2011
Dec 25, 2016
Dec 25, 2022
Dec 25, 2033
Dec 25, 2039
Dec 25, 2044
Dec 25, 2050
Dec 25, 2061
Dec 25, 2067
Dec 25, 2072
Dec 25, 2078
Dec 25, 2089
Dec 25, 2095
Dec 25, 2101
Dec 25, 2107
Dec 25, 2112
Dec 25, 2118

```



## Euphoria


```Euphoria

--Day of the week task from Rosetta Code wiki
--User:Lnettnay

--In what years between 2008 and 2121 will the 25th of December be a Sunday

include std/datetime.e

datetime dt

for year = 2008 to 2121 do
        dt = new(year, 12, 25)
        if weeks_day(dt) = 1 then -- Sunday = 1
                ? year
        end if
end for

```

```txt

2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118

```



=={{header|F_Sharp|F#}}==

```fsharp
open System

[ 2008 .. 2121 ]
|> List.choose (fun y -> if DateTime(y,12,25).DayOfWeek = DayOfWeek.Sunday then Some(y) else None)
|> printfn "%A"
```

```txt
[2011; 2016; 2022; 2033; 2039; 2044; 2050; 2061; 2067; 2072; 2078; 2089; 2095;
 2101; 2107; 2112; 2118]
```



## Factor


```factor
USING: calendar math.ranges prettyprint sequences ;
2008 2121 [a,b] [ 12 25 <date> sunday? ] filter .
```



## FBSL


```qbasic
#APPTYPE CONSOLE

'In what years between 2008 and 2121 will the 25th of December be a Sunday?
dim date as integer, dayname as string
for dim year = 2008 to 2121
	date = year * 10000 + 1225
	dayname = dateconv(date,"dddd")
	if dayname = "Sunday" then
		print "Christmas Day is on a Sunday in ", year
	end if
next
PAUSE

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Day_of_the_week this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth


Forth has only TIME&DATE, which does not give day of week. Many public Forth Julian date calculators had year-2100 problems, but this algorithm works well.

```forth

\ Zeller's Congruence
: weekday ( d m y -- wd) \ 1 mon..7 sun
  over 3 < if 1- swap 12 + swap then
  100 /mod
  dup 4 / swap 2* -
  swap dup 4 / + +
  swap 1+ 13 5 */ + +
  ( in zeller 0=sat, so -2 to 0= mon, then mod, then 1+ for 1=mon)
  2- 7 mod 1+ ;

: yuletide
  ." December 25 is Sunday in "
  2122 2008 do
    25 12 i weekday
    7 = if i . then
  loop cr ;

```



```forth

cr yuletide
December 25 is Sunday in 2011 2016 2022 2033 2039 2044 2050 2061 2067 2072 2078 2089 2095 2101 2107 2112 2118
 ok

```



To show year-2100 problems with SwiftForth's provided Modified Julian Day support:


```forth
: yuletide
  ." December 25 is Sunday in "
  2122 2008 do
    25 12 i d/m/y
    7 mod 0= if i . then
  loop cr ;

cr yuletide
December 25 is Sunday in 2011 2016 2022 2033 2039 2044 2050 2061 2067 2072 2078 2089 2095 2100 2106 2117
```


In [[4tH]] a library is available which provides the right answer:


```forth
include lib/time.4th

: yuletide
  ." December 25 is Sunday in "
  2122 2008 do
    25 12 i weekday
    6 = if i . then
  loop cr ;

cr yuletide
```


The code is derived from "Collected Algorithms from ACM", Volume 1 Algorithms 1-220.


## Fortran

Based on Forth example

```fortran
PROGRAM YULETIDE

IMPLICIT NONE

INTEGER :: day, year

WRITE(*, "(A)", ADVANCE="NO") "25th of December is a Sunday in"
DO year = 2008, 2121
   day = Day_of_week(25, 12, year)
   IF (day == 1) WRITE(*, "(I5)", ADVANCE="NO") year
END DO

CONTAINS

FUNCTION Day_of_week(d, m, y)
   INTEGER :: Day_of_week, j, k, mm, yy
   INTEGER, INTENT(IN) :: d, m, y

   mm=m
   yy=y
   IF(mm.le.2) THEN
      mm=mm+12
      yy=yy-1
   END IF
   j = yy / 100
   k = MOD(yy, 100)
   Day_of_week = MOD(d + ((mm+1)*26)/10 + k + k/4 + j/4 + 5*j, 7)
END FUNCTION Day_of_week

END PROGRAM YULETIDE
```

```txt

 25th of December is a Sunday in 2011 2016 2022 2033 2039 2044 2050 2061 2067 2072 2078 2089 2095 2101 2107 2112 2118

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=b9b4e9a871e96ea6f1db467fa23669fe Click this link to run this code]'''

```gambas
Public Sub Main()
Dim siCount As Short

For siCount = 2008 To 2121
  If WeekDay(Date(siCount, 12, 25)) = 0 Then Print Format(Date(siCount, 12, 25), "dddd dd mmmm yyyy") & " falls on a Sunday"
Next

End
```

Output:

```txt

Sunday 25 December 2011 falls on a Sunday
Sunday 25 December 2016 falls on a Sunday
Sunday 25 December 2022 falls on a Sunday
Sunday 25 December 2033 falls on a Sunday
Sunday 25 December 2039 falls on a Sunday
Sunday 25 December 2044 falls on a Sunday
Sunday 25 December 2050 falls on a Sunday
Sunday 25 December 2061 falls on a Sunday
Sunday 25 December 2067 falls on a Sunday
Sunday 25 December 2072 falls on a Sunday
Sunday 25 December 2078 falls on a Sunday
Sunday 25 December 2089 falls on a Sunday
Sunday 25 December 2095 falls on a Sunday
Sunday 25 December 2101 falls on a Sunday
Sunday 25 December 2107 falls on a Sunday
Sunday 25 December 2112 falls on a Sunday
Sunday 25 December 2118 falls on a Sunday

```



## GAP


```gap
Filtered([2008 .. 2121], y -> WeekDay([25, 12, y]) = "Sun");
# [ 2011, 2016, 2022, 2033, 2039, 2044, 2050, 2061, 2067, 2072, 2078, 2089, 2095, 2101, 2107, 2112, 2118 ]

# A possible implementation of WeekDayAlt

days := ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"];;

WeekDayAlt := function(args)
   local d, m, y, k;
   d := args[1];
   m := args[2];
   y := args[3];
   if m < 3 then
      m := m + 12;
      y := y - 1;
   fi;
   k := 1 + RemInt(d + QuoInt((m + 1)*26, 10) + y + QuoInt(y, 4)
          + 6*QuoInt(y, 100) + QuoInt(y, 400) + 5, 7);
   return days[k];
end;

Filtered([2008 .. 2121], y -> WeekDayAlt([25, 12, y]) = "Sun");
# [ 2011, 2016, 2022, 2033, 2039, 2044, 2050, 2061, 2067, 2072, 2078, 2089, 2095, 2101, 2107, 2112, 2118 ]
```



## Go


```go
package main

import "fmt"
import "time"

func main() {
    for year := 2008; year <= 2121; year++ {
        if time.Date(year, 12, 25, 0, 0, 0, 0, time.UTC).Weekday() ==
            time.Sunday {
            fmt.Printf("25 December %d is Sunday\n", year)
        }
    }
}
```

```txt

25 December 2011 is Sunday
25 December 2016 is Sunday
25 December 2022 is Sunday
25 December 2033 is Sunday
25 December 2039 is Sunday
25 December 2044 is Sunday
25 December 2050 is Sunday
25 December 2061 is Sunday
25 December 2067 is Sunday
25 December 2072 is Sunday
25 December 2078 is Sunday
25 December 2089 is Sunday
25 December 2095 is Sunday
25 December 2101 is Sunday
25 December 2107 is Sunday
25 December 2112 is Sunday
25 December 2118 is Sunday

```



## Groovy


Solution:

```groovy
def yuletide = { start, stop -> (start..stop).findAll { Date.parse("yyyy-MM-dd", "${it}-12-25").format("EEE") == "Sun" } }
```


Test program:

```groovy
println yuletide(2008, 2121)
```


```txt
[2011, 2016, 2022, 2033, 2039, 2044, 2050, 2061, 2067, 2072, 2078, 2089, 2095, 2101, 2107, 2112, 2118]
```



## Haskell

Using the time library:

```haskell
import Data.Time (fromGregorian)

import Data.Time.Calendar.WeekDate (toWeekDate)

isXmasSunday :: Integer -> Bool
isXmasSunday year =
  let (_, _, wday) = toWeekDate $ fromGregorian year 12 25
  in wday == 7

main :: IO ()
main =
  mapM_
    putStrLn
    [ "25 December " ++ show year ++ " is Sunday"
    | year <- [2008 .. 2121]
    , isXmasSunday year ]
```

```txt

25 December 2011 is Sunday
25 December 2016 is Sunday
25 December 2022 is Sunday
25 December 2033 is Sunday
25 December 2039 is Sunday
25 December 2044 is Sunday
25 December 2050 is Sunday
25 December 2061 is Sunday
25 December 2067 is Sunday
25 December 2072 is Sunday
25 December 2078 is Sunday
25 December 2089 is Sunday
25 December 2095 is Sunday
25 December 2101 is Sunday
25 December 2107 is Sunday
25 December 2112 is Sunday
25 December 2118 is Sunday

```


The built-in System.Time module can overflow at the Unix epoch in 2038:

```haskell
import System.Time

isXmasSunday :: Int -> Bool
isXmasSunday year = ctWDay cal == Sunday
  where
    cal = toUTCTime $ toClockTime cal'
    cal' =
      CalendarTime
      { ctYear = year
      , ctMonth = December
      , ctDay = 25
      , ctHour = 0
      , ctMin = 0
      , ctSec = 0
      , ctPicosec = 0
      , ctWDay = Friday
      , ctYDay = 0
      , ctTZName = ""
      , ctTZ = 0
      , ctIsDST = False
      }

main :: IO ()
main =
  mapM_
    putStrLn
    [ "25 December " ++ show year ++ " is Sunday"
    | year <- [2008 .. 2121]
    , isXmasSunday year ]
```

{{out}} on 32-bit machine:

```txt

25 December 2011 is Sunday
25 December 2016 is Sunday
25 December 2022 is Sunday
25 December 2033 is Sunday
*** Exception: user error (Time.toClockTime: invalid input)

```

but with 64 bit systems, running current versions of GHC:

```txt

25 December 2011 is Sunday
25 December 2016 is Sunday
25 December 2022 is Sunday
25 December 2033 is Sunday
25 December 2039 is Sunday
25 December 2044 is Sunday
25 December 2050 is Sunday
25 December 2061 is Sunday
25 December 2067 is Sunday
25 December 2072 is Sunday
25 December 2078 is Sunday
25 December 2089 is Sunday
25 December 2095 is Sunday
25 December 2101 is Sunday
25 December 2107 is Sunday
25 December 2112 is Sunday
25 December 2118 is Sunday
```



## HicEst


```HicEst
DO year = 1, 1000000
   TIME(Year=year, MOnth=12, Day=25, TO, WeekDay=weekday)
   IF( weekday == 7) WRITE(StatusBar) year
ENDDO

END
```


```txt
No anomalies detected for the first million years :-)
Dec 25 = Sunday in
5 ... 2011 2016 2022 2033 2039 2044 2050 2061 2067
      2072 2078 2089 2095 2101 2107 2112 2118 ... 999994

```


== {{header|Icon}} and {{header|Unicon}} ==


```Icon
link datetime

procedure main()
writes("December 25th is a Sunday in: ")
every writes((dayoweek(25,12,y := 2008 to 2122)=="Sunday",y)," ")
end
```



[http://www.cs.arizona.edu/icon/library/src/procs/datetime.icn datetime provides dayoweek]


```Icon
procedure dayoweek(day, month, year)	#: day of the week
   static d_code, c_code, m_code, ml_code, y, C, M, Y

   initial {
      d_code := ["Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"]

      c_code := table()
      c_code[16] := c_code[20] := 0
      c_code[17] := c_code[21] := 6
      c_code[18] := c_code[22] := 4
      c_code[19] := c_code[23] := 2

      m_code := table()
      m_code[1] := m_code["January"] := 1
      m_code[2] := m_code["February"] := 4
      m_code[3] := m_code["March"] := 4
      m_code[4] := m_code["April"] := 0
      m_code[5] := m_code["May"] := 2
      m_code[6] := m_code["June"] := 5
      m_code[7] := m_code["July"] := 0
      m_code[8] := m_code["August"] := 3
      m_code[9] := m_code["September"] := 6
      m_code[10] := m_code["October"] := 1
      m_code[11] := m_code["November"] := 4
      m_code[12] := m_code["December"] := 6

      ml_code := copy(m_code)
      ml_code[1] := ml_code["January"] := 0
      ml_code[2] := ml_code["February"] := 3
      }

   if year < 1600 then stop("*** can't compute day of week that far back")
   if year > 2299 then stop("*** can't compute day of week that far ahead")

   C := c_code[(year / 100) + 1]
   y := year % 100
   Y := (y / 12) + (y % 12) + ((y % 12) / 4)
   month := integer(month)
   M := if (year % 4) = 0 then ml_code[month] else m_code[month]

   return d_code[(C + Y + M + day) % 7 + 1]

end
```


```txt
December 25th is a Sunday in: 2011 2016 2022 2033 2039 2044 2050 2061 2067 2072 2078 2089 2095 2101 2107 2112 2118
```



## J


```j
   load 'dates'                                    NB. provides verb 'weekday'
   xmasSunday=: #~ 0 = [: weekday 12 25 ,~"1 0 ]   NB. returns years where 25 Dec is a Sunday
   xmasSunday 2008 + i.114                         NB. check years from 2008 to 2121
2011 2016 2022 2033 2039 2044 2050 2061 2067 2072 2078 2089 2095 2101 2107 2112 2118
```



## Java


```java
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

public class Yuletide{
	public static void main(String[] args) {
		for(int i = 2008;i<=2121;i++){
			Calendar cal = new GregorianCalendar(i, Calendar.DECEMBER,
					25);
			if(cal.get(Calendar.DAY_OF_WEEK)==Calendar.SUNDAY){
				System.out.println(cal.getTime());
			}
		}
	}
}
```

```txt
Sun Dec 25 00:00:00 CST 2011
Sun Dec 25 00:00:00 CST 2016
Sun Dec 25 00:00:00 CST 2022
Sun Dec 25 00:00:00 CST 2033
Sun Dec 25 00:00:00 CST 2039
Sun Dec 25 00:00:00 CST 2044
Sun Dec 25 00:00:00 CST 2050
Sun Dec 25 00:00:00 CST 2061
Sun Dec 25 00:00:00 CST 2067
Sun Dec 25 00:00:00 CST 2072
Sun Dec 25 00:00:00 CST 2078
Sun Dec 25 00:00:00 CST 2089
Sun Dec 25 00:00:00 CST 2095
Sun Dec 25 00:00:00 CST 2101
Sun Dec 25 00:00:00 CST 2107
Sun Dec 25 00:00:00 CST 2112
Sun Dec 25 00:00:00 CST 2118
```



## JavaScript



### ES5


### =Iteration=


```javascript
for (var year = 2008; year <= 2121; year++){
    var xmas = new Date(year, 11, 25)
    if ( xmas.getDay() === 0 )
        console.log(year)
}
```

```txt
2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118
```



### =Functional composition=



```JavaScript
(function () {
    'use strict';

    // isXmasSunday :: Integer -> Bool
    function isXmasSunday(year) {
        return (new Date(year, 11, 25))
            .getDay() === 0;
    }

    // range :: Int -> Int -> [Int]
    function range(m, n) {
        return Array.apply(null, Array(n - m + 1))
            .map(function (_, i) {
                return m + i;
            });
    }

    return range(2008, 2121)
        .filter(isXmasSunday);

})();
```


```txt
[2011, 2016, 2022, 2033, 2039, 2044, 2050, 2061, 2067,
2072, 2078, 2089, 2095, 2101, 2107, 2112, 2118]
```



### ES6


```JavaScript
(() => {
    'use strict';

    // main :: IO ()
    const main = () => {
        const
            xs = enumFromTo(2008, 2121)
            .filter(xmasIsSunday);
        return (
            console.log(xs),
            xs
        );
    };


    // xmasIsSunday :: Int -> Bool
    const xmasIsSunday = year =>
        (new Date(year, 11, 25))
        .getDay() === 0;


    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: 1 + n - m
        }, (_, i) => m + i);


    return main();
})();
```

```JavaScript
[2011, 2016, 2022, 2033, 2039, 2044, 2050, 2061, 2067, 2072, 2078, 2089, 2095, 2101, 2107, 2112, 2118]
```



## jq


```jq
# Use Zeller's Congruence to determine the day of the week, given
# year, month and day as integers in the conventional way.
# If iso == "iso" or "ISO", then emit an integer in 1 -- 7 where
# 1 represents Monday, 2 Tuesday, etc;
# otherwise emit 0 for Saturday, 1 for Sunday, etc.
#
def day_of_week(year; month; day; iso):
  if month == 1 or month == 2 then
    [month + 12, year - 1]
  else
    [month, year]
  end
  | day + (13*(.[0] + 1)/5|floor)
    +  (.[1]%100)       + ((.[1]%100)/4|floor)
    +  (.[1]/400|floor) - 2*(.[1]/100|floor)
  | if iso == "iso" or iso == "ISO" then 1 + ((. + 5) % 7)
    else . % 7
    end;
```

'''The task''':

```jq
# Give the results as an array so they can
# readily be presented on a single line:
[range(2008; 2122) | select( day_of_week(.;12;25;0) == 1 )]
```

 $ jq -n -c -f zeller.jq
 [2011,2016,2022,2033,2039,2044,2050,2061,2067,2072,2078,2089,2095,2101,2107,2112,2118]


## Jsish

Jsi does not yet implement the Javascript ''Date'' object.  ''strftime' and ''strptime'' functions are used here instead.


```javascript
/* Day of the week, December 25th on a Sunday */
for (var year = 2008; year <= 2121; year++) {
    var xmas = strptime(year + '/12/25', '%Y/%m/%d');
    var weekDay = strftime(xmas, '%w');
    if (weekDay == 0) puts(year);
}

/*
=!EXPECTSTART!=
2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118
=!EXPECTEND!=
*/
```


```txt
prompt$ jsish -u dayOfTheWeek.jsi
[PASS] dayOfTheWeek.jsi
```



## Julia


```julia
using Dates

lo, hi = 2008, 2121
xmas = collect(Date(lo, 12, 25):Year(1):Date(hi, 12, 25))
filter!(xmas) do dt
    dayofweek(dt) == Dates.Sunday
end

println("Years from $lo to $hi having Christmas on Sunday: ")
foreach(println, year.(xmas))
```


```txt
Years from 2008 to 2121 having Christmas on Sunday:
2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118
```



## K


```K
    wd:{(__jd x)!7}  / Julian day count, Sun=6
    y@&6={wd 1225+x*10000}'y:2008+!114
2011 2016 2022 2033 2039 2044 2050 2061 2067 2072 2078 2089 2095 2101 2107 2112 2118

```



## Kotlin


```scala
// version 1.0.6

import java.util.*

fun main(args: Array<String>) {
    println("Christmas day in the following years falls on a Sunday:\n")
    val calendar = GregorianCalendar(2008, Calendar.DECEMBER, 25)
    for (year in 2008..2121) {
        if (Calendar.SUNDAY == calendar[Calendar.DAY_OF_WEEK]) println(year)
        calendar.add(Calendar.YEAR, 1)
    }
}
```


```txt

Christmas day in the following years falls on a Sunday:

2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118

```



## Lasso


```lasso
loop(-From=2008, -to=2121) => {^
  local(tDate = date('12/25/' + loop_count))
  #tDate->dayOfWeek == 1 ? '\r' + #tDate->format('%D') + ' is a Sunday'
^}
```


```txt
12/25/2011 is a Sunday
12/25/2016 is a Sunday
12/25/2022 is a Sunday
12/25/2033 is a Sunday
12/25/2039 is a Sunday
12/25/2044 is a Sunday
12/25/2050 is a Sunday
12/25/2061 is a Sunday
12/25/2067 is a Sunday
12/25/2072 is a Sunday
12/25/2078 is a Sunday
12/25/2089 is a Sunday
12/25/2095 is a Sunday
12/25/2101 is a Sunday
12/25/2107 is a Sunday
12/25/2112 is a Sunday
12/25/2118 is a Sunday
```



## Liberty BASIC


```lb
    count = 0
    for year = 2008 to 2121
        dateString$="12/25/";year
        dayNumber=date$(dateString$)

        if dayNumber mod 7 = 5 then
            count = count + 1
            print dateString$
        end if

    next year

    print count; " years when Christmas Day falls on a Sunday"
    end
```



## Lingo


```lingo
put "December 25 is a Sunday in:"
refDateObj = date(1905,1,2)
repeat with year = 2008 to 2121
  dateObj = date(year, 12, 25)
  dayOfWeek = ((dateObj - refDateObj) mod 7)+1 -- 1=Monday..7=Sunday
  if dayOfWeek=7 then put year
end repeat
```

```txt

-- "December 25 is a Sunday in:"
-- 2011
-- 2016
-- 2022
-- 2033
-- 2039
-- 2044
-- 2050
-- 2061
-- 2067
-- 2072
-- 2078
-- 2089
-- 2095
-- 2101
-- 2107
-- 2112
-- 2118

```



## LiveCode


```LiveCode
function xmasSunday startDate endDate
    convert the long date to dateitems
    put it into xmasDay
    put 12 into item 2 of xmasDay
    put 25 into item 3 of xmasDay
    repeat with i = startDate to endDate
        put i into item 1 of xmasDay
        convert xmasDay to dateItems
        if item 7 of xmasDay is 1 then put i & comma after xmasYear
    end repeat
    if the last char of xmasYear is comma then delete the last char of xmasYear
    return xmasYear
end xmasSunday
```
Example
```LiveCode
put xmasSunday(2008,2121)
```
Output
```txt
2011,2016,2022,2033,2039,2044,2050,2061,2067,2072,2078,2089,2095,2101,2107,2112,2118
```



## Logo


```Logo
; Determine if a Gregorian calendar year is leap
to leap? :year
  output (and
    equal? 0 modulo :year 4
    not member? modulo :year 400 [100 200 300]
  )
end

; Convert Gregorian calendar date to a simple day count from
; day 1 = January 1, 1 CE
to day_number :year :month :day
  local "elapsed make "elapsed difference :year 1
  output (sum  product 365 :elapsed
              int quotient :elapsed 4
              minus int quotient :elapsed 100
              int quotient :elapsed 400
              int quotient difference product 367 :month 362 12
              ifelse lessequal? :month 2 0 ifelse leap? :year -1 -2
              :day)
end

; Find the day of the week from a day number; 0 = Sunday through 6 = Saturday
to day_of_week :day_number
  output modulo :day_number 7
end

; True if the given day is a Sunday
to sunday? :year :month :day
  output equal? 0 day_of_week day_number :year :month :day
end

; Put it all together to answer the question posed in the problem
print filter [sunday? ? 12 25] iseq 2008 2121
bye
```


```txt
2011 2016 2022 2033 2039 2044 2050 2061 2067 2072 2078 2089 2095 2101 2107 2112 2118
```



## Lua

'''Library:''' [http://luaforge.net/projects/date/ LuaDate]


```Lua
require("date")

for year=2008,2121 do
   if date(year, 12, 25):getweekday() == 1 then
      print(year)
   end
end
```


```txt

2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118

```



###  Without external modules

Same output as above

```Lua
local dTab = {day = 25, month = 12}
for year = 2008, 2121 do
    dTab.year = year
    if os.date("%A", os.time(dTab)) == "Sunday" then
        print(year)
    end
end
```



## M2000 Interpreter

Str$( number, format$) use Visual Basic 6 format

```M2000 Interpreter

Print "December 25 is a Sunday in:"
For Year=2008 to 2121 {
      if  Str$(Date("25/12/"+str$(Year,"")),"w")="1" Then {
            Print Year
      }
}
\\ is the same with this:
Print "December 25 is a Sunday in:"
For Year=2008 to 2121 {
      if  Str$(Date(str$(Year,"")+"-12-25"),"w")="1" Then {
            Print Year
      }
}



```

```txt

2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118

```



## M4


```M4
divert(-1)

define(`for',
   `ifelse($#,0,``$0'',
   `ifelse(eval($2<=$3),1,
   `pushdef(`$1',$2)$4`'popdef(`$1')$0(`$1',incr($2),$3,`$4')')')')

dnl  julian day number corresponding to December 25th of given year
define(`julianxmas',
   `define(`yrssince0',eval($1+4712))`'define(`noOfLpYrs',
      eval((yrssince0+3)/4))`'define(`jd',
      eval(365*yrssince0+noOfLpYrs-10-($1-1501)/100+($1-1201)/400+334+25-1))`'
      ifelse(eval($1%4==0 && ($1%100!=0 || $1%400==0)),1,
         `define(`jd',incr(jd))')`'jd')

divert

for(`yr',2008,2121,
   `ifelse(eval(julianxmas(yr)%7==6),1,`yr ')')
```


```txt

2011 2016 2022 2033 2039 2044 2050 2061 2067 2072 2078 2089 2095 2101 2107 2112
2118

```



## Maple


```Maple
xmas:= proc()
	local i, dt;
	for i from 2008 to 2121 by 1 do
		dt := Date(i, 12, 25);
		if (Calendar:-DayOfWeek(dt) = 1) then
			print(i);
		end if;
	end do;
end proc;

xmas();
```


```txt

2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118

```



=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
Reap[If[DateString[{#,12,25},"DayName"]=="Sunday",Sow[#]]&/@Range[2008,2121]][[2,1]]
```

gives back:

```Mathematica
{2011,2016,2022,2033,2039,2044,2050,2061,2067,2072,2078,2089,2095,2101,2107,2112,2118}
```


=={{header|MATLAB}} / {{header|Octave}}==


```Matlab
  t  = datenum([[2008:2121]',repmat([12,25,0,0,0], 2121-2007, 1)]);
  t  = t(strmatch('Sunday', datestr(t,'dddd')), :);
  datestr(t,'yyyy')

```



```txt
 ans =
  2011
  2016
  2022
  2033
  2039
  2044
  2050
  2061
  2067
  2072
  2078
  2089
  2095
  2101
  2107
  2112
  2118

```



## Maxima


```maxima
weekday(year, month, day) := block([m: month, y: year, k],
   if m < 3 then (m: m + 12, y: y - 1),
   k: 1 + remainder(day + quotient((m + 1)*26, 10) + y + quotient(y, 4)
        + 6*quotient(y, 100) + quotient(y, 400) + 5, 7),
   ['monday, 'tuesday, 'wednesday, 'thurdsday, 'friday, 'saturday, 'sunday][k]
)$

sublist(makelist(i, i, 2008, 2121),
        lambda([y], weekday(y, 12, 25) = 'sunday));
/* [2011, 2016, 2022, 2033, 2039, 2044, 2050, 2061, 2067, 2072, 2078, 2089, 2095, 2101, 2107, 2112, 2118] */
```


=={{header|Modula-3}}==
Modula-3 represents time using a (safe) wrapper around the C time interface.
Consequently, it suffers from the same problem as C.


```modula3
MODULE Yule EXPORTS Main;

IMPORT IO, Fmt, Date, Time;

VAR date: Date.T;
    time: Time.T;

BEGIN
  FOR year := 2008 TO 2121 DO
    date.day := 25;
    date.month := Date.Month.Dec;
    date.year := year;

    TRY
      time := Date.ToTime(date);
    EXCEPT
    | Date.Error =>
      IO.Put(Fmt.Int(year) & " is the last year we can specify\n");
      EXIT;
    END;

    date := Date.FromTime(time);

    IF date.weekDay = Date.WeekDay.Sun THEN
      IO.Put("25th of December " & Fmt.Int(year) & " is Sunday\n");
    END;
  END;
END Yule.
```


```txt

25th of December 2011 is Sunday
25th of December 2016 is Sunday
25th of December 2022 is Sunday
25th of December 2033 is Sunday
2038 is the last year we can specify

```


=={{header|MK-61/52}}==
<lang>П9	7	П7	1	П8	НОП	ИП8	2	2	-
1	0	/	[x]	П6	ИП9	+	1	8	9
9	-	3	6	5	,	2	5	*	[x]
ИП8	ИП6	1	2	*	-	1	4	-	3
0	,	5	9	*	[x]	+	2	9	+
ИП7	+	П4	ИП4	7	/	[x]	7	*	-
x=0	64	ИП9	С/П	ИП9	1	+	П9	БП	06
```


''Input:'' РX: starting year.

''Output:'' the year in which Christmas falls on a Sunday. For example, enter ''2008'', the first result: ''2018'' (''January 7, 2018'' is Sunday).


## MUMPS

```MUMPS

DOWHOLIDAY
 ;In what years between 2008 and 2121 will December 25 be a Sunday?
 ;Uses the VA's public domain routine %DTC (Part of the Kernel) named here DIDTC
 NEW BDT,EDT,CHECK,CHKFOR,LIST,I,X,Y
 ;BDT - the beginning year to check
 ;EDT - the end year to check
 ;BDT and EDT are year offsets from the epoch date 1/1/1700
 ;CHECK - the month and day to look at
 ;CHKFOR - what day of the week to look for
 ;LIST - list of years in which the condition is true
 ;I - the year currently being checked
 ;X - the date in an "internal" format, for input to DOW^DIDTC
 ;Y - the output from DOW^DIDTC
 SET BDT=308,EDT=421,CHECK="1225",CHKFOR=0,LIST=""
 FOR I=BDT:1:EDT SET X=I_CHECK D DOW^DIDTC SET:(Y=0) LIST=$SELECT($LENGTH(LIST):LIST_", ",1:"")_(I+1700)
 IF $LENGTH(LIST)=0 WRITE !,"There are no years that have Christmas on a Sunday in the given range."
 IF $LENGTH(LIST) WRITE !,"The following years have Christmas on a Sunday: ",LIST
 KILL BDT,EDT,CHECK,CHKFOR,LIST,I,X,Y
 QUIT

```
Usage:
```txt
USER>D ^DOW

The following years have Christmas on a Sunday: 2011, 2016, 2022, 2033, 2039, 2044, 2050, 2061, 2067, 2072, 2078, 2089, 2095, 2101, 2107, 2112, 2118

```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

yearRanges = [int 2008, 2121]
searchday = ''
cal = Calendar

loop year = yearRanges[0] to yearRanges[1]
  cal = GregorianCalendar(year, Calendar.DECEMBER, 25)
  dayIndex = cal.get(Calendar.DAY_OF_WEEK)
  if dayIndex = Calendar.SUNDAY then searchday = searchday year
  end year

say 'Between' yearRanges[0] 'and' yearRanges[1]', Christmas day falls on a Sunday on the following years:'
searchday = searchday.strip.changestr(' ', ',')
say '  'searchday

return

```

```txt

Between 2008 and 2121, Christmas day falls on a Sunday on the following years:
  2011,2016,2022,2033,2039,2044,2050,2061,2067,2072,2078,2089,2095,2101,2107,2112,2118

```

===Comparison of Some Common Day-of-Week Algorithms===
The following program exercises some common &quot;Day-0f-Week&quot; algorithms to confirm they all arrive at the same result.

```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

days = 'Monday Tuesday Wednesday Thursday Friday Saturday Sunday'
yearRanges = [int 2008, 2121]

searchday = ''
searchday['index'] = days.wordpos('Sunday')
searchday[0] = 0

algorithmName = ['Java Calendar', 'Zeller[1]', 'Zeller[2]', 'Sakamoto', 'Gauss', 'Keith', 'Babwani']

loop alg = 0 to algorithmName.length - 1
  sd = searchday[0] + 1
  searchday[0] = sd
  searchday['agorithm', sd] = algorithmName[alg]
  loop year = yearRanges[0] to yearRanges[1]
    select case alg
      when 0 then dayIndex = getDaynumJavaLibrary(year, 12, 25)
      when 1 then dayIndex = getDaynumZellersCongruenceMethod1(year, 12, 25)
      when 2 then dayIndex = getDaynumZellersCongruenceMethod2(year, 12, 25)
      when 3 then dayIndex = getDaynumSakamoto(year, 12, 25)
      when 4 then dayIndex = getDaynumGauss(year, 12, 25)
      when 5 then dayIndex = getDaynumKeith(year, 12, 25)
      when 6 then dayIndex = getDaynumBabwani(year, 12, 25)
      otherwise nop
      end
    if dayIndex = searchday['index'] then
      searchday[sd] = searchday[sd] year
    end year
  end alg

-- display results
say 'Between' yearRanges[0] 'and' yearRanges[1]', Christmas day falls on a Sunday in the following years:'
loop r_ = 1 to searchday[0]
  searchday[r_] = searchday[r_].strip.changestr(' ', ',')
  say searchday['agorithm', r_].right(20)':' searchday[r_]
  end r_

return

-- -----------------------------------------------------------------------------
method getDaynumJavaLibrary(Year = int, Month = int, Day = int, iso = Rexx 'Y') public static binary returns int
  -- The day-of-week is an integer value where 1 is Sunday, 2 is Monday, ..., and 7 is Saturday
  -- For an ISO week date Day-of-Week d (1 = Monday to 7 = Sunday), use d = ((h - 1 + 6) mod 7) + 1

  cal = Calendar
  jmNumber = [ -
      Calendar.JANUARY,   Calendar.FEBRUARY, Calendar.MARCH,    Calendar.APRIL    -
    , Calendar.MAY,       Calendar.JUNE,     Calendar.JULY,     Calendar.AUGUST   -
    , Calendar.SEPTEMBER, Calendar.OCTOBER,  Calendar.NOVEMBER, Calendar.DECEMBER -
    ]

  mon = jmNumber[Month - 1]
  cal = GregorianCalendar(Year, mon, Day)
  h   = cal.get(Calendar.DAY_OF_WEEK)

  if 'YES'.abbrev(iso.upper, 1) then w = ((h - 1 + 6) // 7) + 1
                                else w = h

  return w

-- -----------------------------------------------------------------------------
method getDaynumZellersCongruenceMethod1(Year = int, Month = int, Day = int, iso = Rexx 'Y') public static returns int
  -- DayNum results in an integer in the range 0-6 where 0 represents Monday etc.
  -- For an ISO week date add 1

  if Month = 1 | Month = 2 then do
    Month = Month + 12
    Year  = Year - 1
    end

  MonthFactor = 2 * Month + 3 * (Month + 1) % 5
  YearFactor  = Year + Year % 4 - Year % 100 + Year % 400
  DayNum      = (Day + MonthFactor + YearFactor) // 7

  if 'YES'.abbrev(iso.upper, 1) then d = DayNum + 1
                                else d = DayNum

  return d

-- -----------------------------------------------------------------------------
method getDaynumZellersCongruenceMethod2(Year = int, Month = int, Day = int, iso = Rexx 'Y') public static binary returns int
  -- h is the day of the week (0 = Saturday, 1 = Sunday, 2 = Monday, ...)
  -- For an ISO week date Day-of-Week d (1 = Monday to 7 = Sunday), use d = ((h + 5) mod 7) + 1

  if Month < 3 then do
    Month = Month + 12
    Year  = Year - 1
    end
  q = Day
  m = Month
  Y = Year

  h = (q + ((m + 1) * 26 % 10) + Y + (Y % 4) + 6 * (Y % 100) + (Y % 400)) // 7

  if 'YES'.abbrev(iso.upper, 1) then d = ((h + 5) // 7) + 1
                                else d = h

  return d

-- -----------------------------------------------------------------------------
method getDaynumSakamoto(y = int, m = int, d = int, iso = Rexx 'Y') public static binary returns int
  -- h is the day of the week (0 = Sunday, 1 = Monday, 2 = Tuesday...)
  -- For an ISO week date Day-of-Week d (1 = Monday to 7 = Sunday), use d = ((h + 6) mod 7) + 1

  t = [int 0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4]
  y = y - (m < 3)
  h = (y + y % 4 - y % 100 + y % 400 + t[m - 1] + d) // 7

  if 'YES'.abbrev(iso.upper, 1) then d = ((h + 6) // 7) + 1
                                else d = h

  return d

-- -----------------------------------------------------------------------------
method getDaynumGauss(Year = int, Month = int, Day = int, iso = Rexx 'Y') public static binary returns int
  -- W is week day (0 = Sunday, ..., 6 = Saturday)
  -- For an ISO week date Day-of-Week d (1 = Monday to 7 = Sunday), use d = ((h + 6) mod 7) + 1

  Year = Year - (Month < 3)
  k = double Day
  C = double Year % 100
  Y = double Year // 100
  m = double ((Month + 9) // 12) + 1

  W = modulo(int (k + Math.floor(2.6 * m - 0.2) + y + Math.floor(y / 4) + Math.floor(c / 4) - 2 * c), 7)

  if 'YES'.abbrev(iso.upper, 1) then h = ((W + 6) // 7) + 1
                                else h = W

  return h

-- -----------------------------------------------------------------------------
method getDaynumKeith(y = int, m = int, d = int, iso = Rexx 'Y') public constant binary returns int
  -- W is week day (0 = Sunday, ..., 6 = Saturday)
  -- For an ISO week date Day-of-Week d (1 = Monday to 7 = Sunday), use d = ((h + 6) mod 7) + 1

  if m < 3 then do
    d = d + y
    y = y - 1
    end
  else do
    d = d + y - 2
    end

  h = (23 * m % 9 + d + 4 + y % 4 - y % 100 + y % 400) // 7

  if 'YES'.abbrev(iso.upper, 1) then W = ((h + 6) // 7) + 1
                                else W = h

  return W

-- -----------------------------------------------------------------------------
method getDaynumBabwani(Year = int, Month = int, Day = int, iso = Rexx 'Y') public constant binary returns int
  -- return dow = Day of week: 0 = Saturday, 1 = Sunday, ... 6 = Friday
  -- For an ISO week date Day-of-Week W (1 = Monday to 7 = Sunday), use W = ((dow + 5) mod 7) + 1

  y = Year
  m = Month
  d = Day

  dow    = int -- dow stands for day of week
  dowfg  = double
  fmonth = int
  leap   = int

  if ((y // 100 == 0) & (y // 400 \= 0)) then  -- leap function 1 for leap & 0 for non-leap
    leap = 0
  else if (y // 4 == 0) then
    leap = 1
  else
    leap = 0

  fmonth = 3 + (2 - leap) * ((m + 2) % (2 * m)) + (5 * m + m % 9) % 2 -- f(m) formula
  fmonth = fmonth // 7 -- f(m) is brought in range of 0 to 6

  century    = y % 100
  lastdigits = y // 100

  dowfg = 1.25 * lastdigits + fmonth + d - 2 * (century // 4) -- function of weekday for Gregorian
  dow = int dowfg // 7 -- remainder on division by 7

  if 'YES'.abbrev(iso.upper, 1) then W = ((dow + 5) // 7) + 1
                                else W = dow

  return W

-- -----------------------------------------------------------------------------
method modulo(N = int, D = int) inheritable static binary returns int
  return (D + (N // D)) // D

```

```txt

Between 2008 and 2121, Christmas day falls on a Sunday in the following years:
       Java Calendar: 2011,2016,2022,2033,2039,2044,2050,2061,2067,2072,2078,2089,2095,2101,2107,2112,2118
           Zeller[1]: 2011,2016,2022,2033,2039,2044,2050,2061,2067,2072,2078,2089,2095,2101,2107,2112,2118
           Zeller[2]: 2011,2016,2022,2033,2039,2044,2050,2061,2067,2072,2078,2089,2095,2101,2107,2112,2118
            Sakamoto: 2011,2016,2022,2033,2039,2044,2050,2061,2067,2072,2078,2089,2095,2101,2107,2112,2118
               Gauss: 2011,2016,2022,2033,2039,2044,2050,2061,2067,2072,2078,2089,2095,2101,2107,2112,2118
               Keith: 2011,2016,2022,2033,2039,2044,2050,2061,2067,2072,2078,2089,2095,2101,2107,2112,2118
             Babwani: 2011,2016,2022,2033,2039,2044,2050,2061,2067,2072,2078,2089,2095,2101,2107,2112,2118

```



## Nim


```nim
import times

var timeinfo = getLocalTime getTime()
timeinfo.monthday = 25
timeinfo.month = mDec
for year in 2008..2121:
  timeinfo.year = year
  if getLocalTime(timeInfoToTime timeinfo).weekday == dSun:
    stdout.write year," "
```

```txt
2011 2016 2022 2033 2039 2044 2050 2061 2067 2072 2078 2089 2095 2101 2107 2112 2118
```


=={{header|Oberon-2}}==
```oberon2

MODULE DayOfWeek;
IMPORT NPCT:Dates, Out;
VAR
  year: INTEGER;
  date: Dates.Date;
BEGIN
  FOR year := 2008 TO 2121 DO
    date := Dates.NewDate(25,12,year);
    IF date.DayOfWeek() = Dates.sunday THEN
     Out.Int(date.year,4);Out.Ln
    END
  END
END DayOfWeek.

```

```txt

2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118

```

```Oberon2

MODULE DaysOfWeek; (** AUTHOR ""; PURPOSE ""; *)

IMPORT
	Out := KernelLog, Dates;

PROCEDURE Do*;
VAR
	date: Dates.DateTime;
	i,y,w,wd: LONGINT;
BEGIN
	FOR i := 2008 TO 2121 DO
		date.year := i;date.month :=12; date.day := 25;
		date.hour := 0;date.minute := 0; date.second := 0;
		Dates.WeekDate(date,y,w,wd);
		IF wd = 7 THEN Out.Int(i,0);Out.Ln END
	END
END Do;

END DaysOfWeek.

```

```txt

2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118

```


=={{header|Objective-C}}==

```objc
#import <Foundation/Foundation.h


int main()
{
   @autoreleasepool {
      for(NSUInteger i=2008; i<2121; i++)
      {
         NSCalendarDate *d = [[NSCalendarDate alloc]
                              initWithYear: i
                              month: 12
                              day: 25
                              hour: 0 minute: 0 second:0
                              timeZone: [NSTimeZone timeZoneWithAbbreviation:@"CET"] ];
         if ( [d dayOfWeek] == 0 )
         {
            printf("25 Dec %u is Sunday\n", i);
         }
      }

   }
   return 0;
}
```


```txt

25 Dec 2011 is Sunday
25 Dec 2016 is Sunday
25 Dec 2022 is Sunday
25 Dec 2033 is Sunday
25 Dec 2039 is Sunday
25 Dec 2044 is Sunday
25 Dec 2050 is Sunday
25 Dec 2061 is Sunday
25 Dec 2067 is Sunday
25 Dec 2072 is Sunday
25 Dec 2078 is Sunday
25 Dec 2089 is Sunday
25 Dec 2095 is Sunday
25 Dec 2101 is Sunday
25 Dec 2107 is Sunday
25 Dec 2112 is Sunday
25 Dec 2118 is Sunday

```



## OCaml

```ocaml
#load "unix.cma"
open Unix

try
  for i = 2008 to 2121 do
    (* I'm lazy so we'll just borrow the current time
       instead of having to set all the fields explicitly *)
    let mytime = { (localtime (time ())) with
                   tm_year  = i - 1900;
                   tm_mon   = 11;
                   tm_mday  = 25 } in
    try
      let _, mytime = mktime mytime in
        if mytime.tm_wday = 0 then
          Printf.printf "25 December %d is Sunday\n" i
    with e ->
      Printf.printf "%d is the last year we can specify\n" (i-1);
      raise e
  done
with _ -> ()
```


{{out}} of a run on a 32 bit machine

```txt

25 December 2011 is Sunday
25 December 2016 is Sunday
25 December 2022 is Sunday
25 December 2033 is Sunday
2037 is the last year we can specify

```



###  With a dedicated library


Unlike the previous example which only uses the OCaml standard library, here with the OCaml Calendar Library we can go until the year 2121:


```ocaml
open CalendarLib

let list_make_seq first last =
  let rec aux i acc =
    if i < first then acc
    else aux (pred i) (i::acc)
  in
  aux last []

let print_date (year, month, day) =
  Printf.printf "%d-%02d-%02d\n" year month day

let () =
  let years = list_make_seq 2008 2121 in
  let years = List.filter (fun year ->
    Date.day_of_week (Date.make year 12 25) = Date.Sun) years in
  print_endline "December 25 is a Sunday in:";
  List.iter (Printf.printf "%d\n") years
```


```txt
$ ocaml unix.cma str.cma -I +calendar calendarLib.cma xmas_sundays.ml
December 25 is a Sunday in:
2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118
```



## Oforth



```Oforth
import: date
seqFrom(2008, 2121) filter(#[ 12 25 Date newDate dayOfWeek Date.SUNDAY == ]) .
```

```txt

[2011, 2016, 2022, 2033, 2039, 2044, 2050, 2061, 2067, 2072, 2078, 2089, 2095, 2101, 2107, 2112, 2118]

```



## ooRexx


```ooRexx
date = .datetime~new(2008, 12, 25)
lastdate = .datetime~new(2121, 12, 25)

resultList = .array~new -- our collector of years

-- date objects are directly comparable
loop while date <= lastdate
  if date~weekday == 7 then resultList~append(date~year)
  -- step to the next year
  date = date~addYears(1)
end

say "Christmas falls on Sunday in the years" resultList~toString("Line", ", ")
```

```txt
Christmas falls on Sunday in the years 2011, 2016, 2022, 2033, 2039, 2044, 2050, 2061, 2067, 2072, 2078, 2089, 2095, 2101, 2107, 2112, 2118
```



## PARI/GP


```parigp

njd(D) =
{
  my (m, y);

  if (D[2] > 2, y = D[1]; m = D[2]+1, y = D[1]-1; m = D[2]+13);

  (1461*y)\4 + (306001*m)\10000 + D[3] - 694024 + if (100*(100*D[1]+D[2])+D[3] > 15821004, 2 - y\100 + y\400)
}

for (y = 2008, 2121, if (njd([y,12,25]) % 7 == 1, print(y)));

```


Output:

```txt

2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118

```



## Pascal

See [[Day_of_the_week#Delphi | Delphi]]


## Peloton


```sgml
<@ SAI>

	<@ ITEFORLI3>2121|2008|
		<@ LETVARCAP>Christmas Day|25-Dec-<@ SAYVALFOR>...</@></@>
		<@ TSTDOWVARLIT>Christmas Day|1</@>
		<@ IFF>
			<@ SAYCAP>Christmas Day <@ SAYVALFOR>...</@> is a Sunday</@><@ SAYKEY>__Newline</@>
		</@>
	</@>
</@>
```


English dialect variable-length space-padded opcodes

```html
<# suppressimplicitoutput

	<# iterate foriteration literalstring3>2121|2008|
		<# let variable capture>Christmas Day|25-Dec-<# say value foriteration>...</#></#>
		<# test dayofweek variable literal>Christmas Day|1</#>
		<# if>
			<# say capture>Christmas Day <# say value foriteration>...</#> is a Sunday</#><# say keyword>__Newline</#>
		</#>
	</#>

</#>
```


```txt
Christmas Day 2011 is a Sunday
Christmas Day 2016 is a Sunday
Christmas Day 2022 is a Sunday
Christmas Day 2033 is a Sunday
Christmas Day 2039 is a Sunday
Christmas Day 2044 is a Sunday
Christmas Day 2050 is a Sunday
Christmas Day 2061 is a Sunday
Christmas Day 2067 is a Sunday
Christmas Day 2072 is a Sunday
Christmas Day 2078 is a Sunday
Christmas Day 2089 is a Sunday
Christmas Day 2095 is a Sunday
Christmas Day 2101 is a Sunday
Christmas Day 2107 is a Sunday
Christmas Day 2112 is a Sunday
Christmas Day 2118 is a Sunday
```



## Perl



```perl
#! /usr/bin/perl -w

use Time::Local;
use strict;

foreach my $i (2008 .. 2121)
{
  my $time = timelocal(0,0,0,25,11,$i);
  my ($s,$m,$h,$md,$mon,$y,$wd,$yd,$is) = localtime($time);
  if ( $wd == 0 )
  {
    print "25 Dec $i is Sunday\n";
  }
}

exit 0;
```


```txt

25 Dec 2011 is Sunday
25 Dec 2016 is Sunday
25 Dec 2022 is Sunday
25 Dec 2033 is Sunday
Day too big - 25195 > 24855
Sec too small - 25195 < 78352
Sec too big - 25195 > 15247
Cannot handle date (0, 0, 0, 25, 11, 2038) at ./ydate.pl line 8

```


Using the DateTime module from CPAN:

```perl
#! /usr/bin/perl -w

use DateTime;
use strict;

foreach my $i (2008 .. 2121)
{
  my $dt = DateTime->new( year   => $i,
                          month  => 12,
                          day    => 25
                        );
  if ( $dt->day_of_week == 7 )
  {
    print "25 Dec $i is Sunday\n";
  }
}

exit 0;
```

or shorter:

```perl
#! /usr/bin/perl -w

use DateTime;
use strict;

for (2008 .. 2121) {
  print "25 Dec $_ is Sunday\n"
    if DateTime->new(year => $_, month => 12, day => 25)->day_of_week == 7;
}

exit 0;
```

```txt

25 Dec 2011 is Sunday
25 Dec 2016 is Sunday
25 Dec 2022 is Sunday
25 Dec 2033 is Sunday
25 Dec 2039 is Sunday
25 Dec 2044 is Sunday
25 Dec 2050 is Sunday
25 Dec 2061 is Sunday
25 Dec 2067 is Sunday
25 Dec 2072 is Sunday
25 Dec 2078 is Sunday
25 Dec 2089 is Sunday
25 Dec 2095 is Sunday
25 Dec 2101 is Sunday
25 Dec 2107 is Sunday
25 Dec 2112 is Sunday
25 Dec 2118 is Sunday

```


Alternatively in one line using grep (read from right to left):

```perl
#! /usr/bin/perl -w

use DateTime;
use strict;

print join " ", grep { DateTime->new(year => $_, month => 12, day => 25)->day_of_week == 7 } (2008 .. 2121);

0;
```

```txt
2011 2016 2022 2033 2039 2044 2050 2061 2067 2072 2078 2089 2095 2101 2107 2112 2118
```



## Perl 6


As Perl 5, except <code>DateTime</code> is built-in, so you don't need to download a module of that name:


```perl6
say join ' ', grep { Date.new($_, 12, 25).day-of-week == 7 }, 2008 .. 2121;
```



## Phix


```Phix
sequence res = {}
for y=2008 to 2121 do
    if day_of_week(y,12,25)=1 then
        res = append(res,y)
    end if
end for
?res
```

```txt

{2011,2016,2022,2033,2039,2044,2050,2061,2067,2072,2078,2089,2095,2101,2107,2112,2118}

```



## PHP



```php
<?php
for($i=2008; $i<2121; $i++)
{
  $datetime = new DateTime("$i-12-25 00:00:00");
  if ( $datetime->format("w") == 0 )
  {
     echo "25 Dec $i is Sunday\n";
  }
}
?>
```


```txt

25 Dec 2011 is Sunday
25 Dec 2016 is Sunday
25 Dec 2022 is Sunday
25 Dec 2033 is Sunday
25 Dec 2039 is Sunday
25 Dec 2044 is Sunday
25 Dec 2050 is Sunday
25 Dec 2061 is Sunday
25 Dec 2067 is Sunday
25 Dec 2072 is Sunday
25 Dec 2078 is Sunday
25 Dec 2089 is Sunday
25 Dec 2095 is Sunday
25 Dec 2101 is Sunday
25 Dec 2107 is Sunday
25 Dec 2112 is Sunday
25 Dec 2118 is Sunday

```



## PicoLisp


```PicoLisp
(for (Y 2008 (>= 2121 Y) (inc Y))
   (when (= "Sunday" (day (date Y 12 25)))
      (printsp Y) ) )
```

```txt
2011 2016 2022 2033 2039 2044 2050 2061 2067 2072 2078 2089 2095 2101 2107 2112 2118
```



## Pike


```Pike
filter(Calendar.Year(2008)->range(Calendar.Year(2121))->years()->month(12)->day(25), lambda(object day){ return day->week_day()==7; })->year()->format_nice();
```

```txt

 Result: ({ /* 17 elements */
                 "2011",
                 "2016",
                 "2022",
                 "2033",
                 "2039",
                 "2044",
                 "2050",
                 "2061",
                 "2067",
                 "2072",
                 "2078",
                 "2089",
                 "2095",
                 "2101",
                 "2107",
                 "2112",
                 "2118"
             })

```



## PL/I


```PL/I

declare i picture '9999';
do i = 2008 to 2121;
   if weekday(days('25Dec' || i, 'DDMmmYYYY')) = 1 then
      put skip list ('Christmas day ' || i || ' is a Sunday');
end;

```



## PowerShell


```powershell
2008..2121 | Where-Object { (Get-Date $_-12-25).DayOfWeek -eq "Sunday" }
```



### Find Christmas holiday for any day and/or year


```PowerShell

function Get-ChristmasHoliday
{
    [CmdletBinding()]
    [OutputType([PSCustomObject])]
    Param
    (
        [Parameter(Mandatory=$false,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true,
                   Position=0)]
        [ValidateRange(1,9999)]
        [int[]]
        $Year = (Get-Date).Year
    )

    Process
    {
        [datetime]$christmas = Get-Date $Year/12/25

        switch ($christmas.DayOfWeek)
        {
            "Sunday"   {[datetime[]]$dates = 1..5 | ForEach-Object {$christmas.AddDays($_)}}
            "Monday"   {[datetime[]]$dates = $christmas, $christmas.AddDays(1)}
            "Saturday" {[datetime[]]$dates = $christmas.AddDays(-2), $christmas.AddDays(-1)}
            Default    {[datetime[]]$dates = $christmas.AddDays(-1), $christmas}
        }

        $dates | Group-Object  -Property Year |
                 Select-Object -Property @{Name="Year"     ; Expression={$_.Name}},
                                         @{Name="DayOfWeek"; Expression={$christmas.DayOfWeek}},
                                         @{Name="Christmas"; Expression={$christmas.ToString("MM/dd/yyyy")}},
                                         @{Name="DaysOff"  ; Expression={$_.Group | ForEach-Object {$_.ToString("MM/dd/yyyy")}}}
    }
}

```

Satisfy the task requirement:

```PowerShell

2008..2121 | Get-ChristmasHoliday | where DayOfWeek -match Su

```

```txt

Year DayOfWeek Christmas  DaysOff
---- --------- ---------  -------
2011    Sunday 12/25/2011 {12/26/2011, 12/27/2011, 12/28/2011, 12/29/2011...}
2016    Sunday 12/25/2016 {12/26/2016, 12/27/2016, 12/28/2016, 12/29/2016...}
2022    Sunday 12/25/2022 {12/26/2022, 12/27/2022, 12/28/2022, 12/29/2022...}
2033    Sunday 12/25/2033 {12/26/2033, 12/27/2033, 12/28/2033, 12/29/2033...}
2039    Sunday 12/25/2039 {12/26/2039, 12/27/2039, 12/28/2039, 12/29/2039...}
2044    Sunday 12/25/2044 {12/26/2044, 12/27/2044, 12/28/2044, 12/29/2044...}
2050    Sunday 12/25/2050 {12/26/2050, 12/27/2050, 12/28/2050, 12/29/2050...}
2061    Sunday 12/25/2061 {12/26/2061, 12/27/2061, 12/28/2061, 12/29/2061...}
2067    Sunday 12/25/2067 {12/26/2067, 12/27/2067, 12/28/2067, 12/29/2067...}
2072    Sunday 12/25/2072 {12/26/2072, 12/27/2072, 12/28/2072, 12/29/2072...}
2078    Sunday 12/25/2078 {12/26/2078, 12/27/2078, 12/28/2078, 12/29/2078...}
2089    Sunday 12/25/2089 {12/26/2089, 12/27/2089, 12/28/2089, 12/29/2089...}
2095    Sunday 12/25/2095 {12/26/2095, 12/27/2095, 12/28/2095, 12/29/2095...}
2101    Sunday 12/25/2101 {12/26/2101, 12/27/2101, 12/28/2101, 12/29/2101...}
2107    Sunday 12/25/2107 {12/26/2107, 12/27/2107, 12/28/2107, 12/29/2107...}
2112    Sunday 12/25/2112 {12/26/2112, 12/27/2112, 12/28/2112, 12/29/2112...}
2118    Sunday 12/25/2118 {12/26/2118, 12/27/2118, 12/28/2118, 12/29/2118...}

```

Get days off for a random year:

```PowerShell

Get-ChristmasHoliday -Year (2008..2121 | Get-Random)

```

```txt

Year DayOfWeek Christmas  DaysOff
---- --------- ---------  -------
2110  Thursday 12/25/2110 {12/24/2110, 12/25/2110}

```

Get days off for the current year using the '''Year''' property returned by <code>Get-Date</code>:

```PowerShell

(Get-Date | Get-ChristmasHoliday).DaysOff

```

```txt

12/26/2016
12/27/2016
12/28/2016
12/29/2016
12/30/2016

```

Get days off for the current year as <code>[DateTime]</code> objects:

```PowerShell

(Get-Date | Get-ChristmasHoliday).DaysOff | Get-Date

```

```txt

Monday, December 26, 2016 12:00:00 AM
Tuesday, December 27, 2016 12:00:00 AM
Wednesday, December 28, 2016 12:00:00 AM
Thursday, December 29, 2016 12:00:00 AM
Friday, December 30, 2016 12:00:00 AM

```



## Prolog

Works with SWI-Prolog;

```Prolog
main() :-
    christmas_days_falling_on_sunday(2011, 2121, SundayList),
    writeln(SundayList).

christmas_days_falling_on_sunday(StartYear, EndYear, SundayList) :-
    numlist(StartYear, EndYear, YearRangeList),
    include(is_christmas_day_a_sunday, YearRangeList, SundayList).

is_christmas_day_a_sunday(Year) :-
    Date = date(Year, 12, 25),
    day_of_the_week(Date, DayOfTheWeek),
    DayOfTheWeek == 7.

```

```txt
?- main.
[2011,2016,2022,2033,2039,2044,2050,2061,2067,2072,2078,2089,2095,2101,2107,2112,2118]
true.
```



## PureBasic

PureBasic's internal Date() is limited between 1970-01-01 00:00:00  and 2038-01-19 03:14:07

```PureBasic
For i=2008 To 2037
  If DayOfWeek(Date(i,12,25,0,0,0))=0
    PrintN(Str(i))
  EndIf
Next
```



## Python


```python
from calendar import weekday, SUNDAY

[year for year in range(2008, 2122) if weekday(year, 12, 25) == SUNDAY]
```

```txt
[2011, 2016, 2022, 2033, 2039, 2044, 2050, 2061, 2067, 2072, 2078, 2089, 2095, 2101, 2107, 2112, 2118]
```


The function <code>calendar.weekday</code> accepts all dates between 1/1/1 and 9999/12/31, and uses the [https://en.wikipedia.org/wiki/Proleptic_Gregorian_calendar proleptic Gregorian calendar] before adoption of the [https://en.wikipedia.org/wiki/Gregorian_calendar Gregorian calendar] in 1582. There is no gap between 1582/10/4 and 1582/10/15, as can be seen with <code>print(calendar.calendar(1582))</code>.


Or, in terms of datetime:
```python
'''Days of the week'''

from datetime import date
from itertools import islice


# xmasIsSunday :: Int -> Bool
def xmasIsSunday(y):
    '''True if Dec 25 in the given year is a Sunday.'''
    return 6 == date(y, 12, 25).weekday()


# main :: IO ()
def main():
    '''Years between 2008 and 2121 with 25 Dec on a Sunday'''

    xs = list(filter(
        xmasIsSunday,
        enumFromTo(2008)(2121)
    ))
    total = len(xs)
    print(
        fTable(main.__doc__ + ':\n\n' + '(Total ' + str(total) + ')\n')(
            lambda i: str(1 + i)
        )(str)(index(xs))(
            enumFromTo(0)(total - 1)
        )
    )


# GENERIC -------------------------------------------------

# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# index (!!) :: [a] -> Int -> a
def index(xs):
    '''Item at given (zero-based) index.'''
    return lambda n: None if 0 > n else (
        xs[n] if (
            hasattr(xs, "__getitem__")
        ) else next(islice(xs, n, None))
    )


# unlines :: [String] -> String
def unlines(xs):
    '''A single string formed by the intercalation
       of a list of strings with the newline character.
    '''
    return '\n'.join(xs)


#  FORMATTING ---------------------------------------------
# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
                     f -> xs -> tabular string.
    '''
    def go(xShow, fxShow, f, xs):
        ys = [xShow(x) for x in xs]
        w = max(map(len, ys))
        return s + '\n' + '\n'.join(map(
            lambda x, y: y.rjust(w, ' ') + ' -> ' + fxShow(f(x)),
            xs, ys
        ))
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# MAIN --
if __name__ == '__main__':
    main()
```

```txt
Years between 2008 and 2121 with 25 Dec on a Sunday:

(Total 17)

 1 -> 2011
 2 -> 2016
 3 -> 2022
 4 -> 2033
 5 -> 2039
 6 -> 2044
 7 -> 2050
 8 -> 2061
 9 -> 2067
10 -> 2072
11 -> 2078
12 -> 2089
13 -> 2095
14 -> 2101
15 -> 2107
16 -> 2112
17 -> 2118
```



## R


```R
years <- 2008:2121
xmas <- as.POSIXlt(paste0(years, '/12/25'))
years[xmas$wday==0]
# 2011 2016 2022 2033 2039 2044 2050 2061 2067 2072 2078 2089 2095 2101 2107 2112 2118

# Also:
xmas=seq(as.Date("2008/12/25"), as.Date("2121/12/25"), by="year")
as.numeric(format(xmas[weekdays(xmas)== 'Sunday'], "%Y"))

# Still another solution, using ISOdate and weekdays
with(list(years=2008:2121), years[weekdays(ISOdate(years, 12, 25)) == "Sunday"])

# Or with "subset"
subset(data.frame(years=2008:2121), weekdays(ISOdate(years, 12, 25)) == "Sunday")$years

# Simply replace "Sunday" with whatever it's named in your country,
# or set locale first, with
Sys.setlocale(cat="LC_ALL", "en")

# Under MS Windows, write instead
Sys.setlocale("LC_ALL", "English")
```



## Racket



```Racket

#lang racket

(require racket/date)

(define (xmas-on-sunday? year)
  (zero? (date-week-day (seconds->date (find-seconds 0 0 12 25 12 year)))))

(for ([y (in-range 2008 2121)] #:when (xmas-on-sunday? y))
  (displayln y))

```



## REBOL


```REBOL
REBOL [
	Title: "Yuletide Holiday"
	URL: http://rosettacode.org/wiki/Yuletide_Holiday
]

for y 2008 2121 1 [
	d: to-date reduce [y 12 25]
	if 7 = d/weekday [prin [y ""]]
]
```


```txt
2011 2016 2022 2033 2039 2044 2050 2061 2067 2072 2078 2089 2095 2101 2107 2112 2118
```



## Red


```Red
Red []
repeat yy 114 [
  d: to-date reduce [25 12 (2007 + yy )]
  if 7 = d/weekday [ print d ] ;; 7 = sunday
]
;; or
print "version 2"

d: to-date [25 12 2008]
while [d <= 25/12/2121 ] [
  if 7 = d/weekday [
    print rejoin [d/day '. d/month '. d/year ]
  ]
  d/year: d/year + 1
]

```

```txt
25-Dec-2011
25-Dec-2016
25-Dec-2022
25-Dec-2033
25-Dec-2039
25-Dec-2044
25-Dec-2050
25-Dec-2061
25-Dec-2067
25-Dec-2072
25-Dec-2078
25-Dec-2089
25-Dec-2095
25-Dec-2101
25-Dec-2107
25-Dec-2112
25-Dec-2118
version 2
25.12.2011
25.12.2016
25.12.2022
25.12.2033
25.12.2039
25.12.2044
25.12.2050
25.12.2061
25.12.2067
25.12.2072
25.12.2078
25.12.2089
25.12.2095
25.12.2101
25.12.2107
25.12.2112
25.12.2118
>>

```


## REXX


### using DATE weekday

The extended DATE parameters (arguments 2 and 3) are only supported by the newer REXX interpreters.

```rexx
    do year=2008 to 2121
    if date('w', year'1225', 's') == 'Sunday' then say year
    end
```


```txt
2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118
```



### using DATE base

The extended DATE parameters (arguments 2 and 3) are only supported by the newer REXX interpreters.

```rexx
    do year=2008 to 2121
    if date('b', year'1225', 's') // 7 == 6 then say year
    end
```

'''output''' is the same as above


### using DATE iso

Works with Regina REXX only.
Works with ooRexx

The extended DATE parameters (arguments 2 and 3) are only supported by the newer REXX interpreters.

Programming note:   The   '''ISO'''   option of the   '''date'''   BIF is a Regina extension.

Language note:   the  DATE built-in function always returns the day-of-week in English, no matter what the native language is in effect.

```rexx
/*REXX program displays in which  years  12/25  (December 25th)   falls on a  Sunday.   */
parse arg start finish .                         /*get the  START  and  FINISH  years.  */
if  start=='' |  start==","  then  start=2008    /*Not specified?  Then use the default.*/
if finish=='' | finish==","  then finish=2121    /* "       "        "   "   "     "    */

      do y=start  to finish                      /*process all the years specified.     */

      if date('Weekday', y"-12-25", 'ISO')\=='Sunday'  then iterate

   /* if date('w'      , y"-12-25", 'i'  ) ···       (same as above).  */
   /*          ↑↑↑↑↑↑   ↑↑↑↑↑↑↑↑↑↑  ↑↑↑                                */
   /*          option   yyyy-mm-dd  fmt                                */

      say 'December 25th,'    y    "falls on a Sunday."
      end  /*y*/
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''   when using the default input:

```txt

December 25th, 2011 falls on a Sunday.
December 25th, 2016 falls on a Sunday.
December 25th, 2022 falls on a Sunday.
December 25th, 2033 falls on a Sunday.
December 25th, 2039 falls on a Sunday.
December 25th, 2044 falls on a Sunday.
December 25th, 2050 falls on a Sunday.
December 25th, 2061 falls on a Sunday.
December 25th, 2067 falls on a Sunday.
December 25th, 2072 falls on a Sunday.
December 25th, 2078 falls on a Sunday.
December 25th, 2089 falls on a Sunday.
December 25th, 2095 falls on a Sunday.
December 25th, 2101 falls on a Sunday.
December 25th, 2107 falls on a Sunday.
December 25th, 2112 falls on a Sunday.
December 25th, 2118 falls on a Sunday.

```



### old school DOW

This DOW (day-of-week) version will work with any version of a REXX interpreter.

```rexx
/*REXX program (old school) displays in which years 12/25 (Dec. 25th) falls on a Sunday.*/
parse arg start finish .                         /*get the  START  and  FINISH  years.  */
if  start=='' |  start==","  then  start=2008    /*Not specified?  Then use the default.*/
if finish=='' | finish==","  then finish=2121    /* "       "        "   "   "     "    */

      do y=start  to finish                      /*process all the years specified.     */
      if dow(12,25,y)==1  then say 'December 25th,'       y       "falls on a Sunday."
      end   /*y*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
dow: procedure; parse arg m,d,y;                    if m<3  then do;  m=m+12;  y=y-1;  end
     yL=left(y,2);  yr=right(y,2);  w=(d + (m+1)*26%10+yr+yr%4+yL%4+5*yL) // 7
     if w==0  then w=7;   return w               /*Sunday=1,  Monday=2,  ···  Saturday=7*/
```

'''output'''    when using the default input:

```txt

December 25th, 2011 falls on a Sunday.
December 25th, 2016 falls on a Sunday.
December 25th, 2022 falls on a Sunday.
December 25th, 2033 falls on a Sunday.
December 25th, 2039 falls on a Sunday.
December 25th, 2044 falls on a Sunday.
December 25th, 2050 falls on a Sunday.
December 25th, 2061 falls on a Sunday.
December 25th, 2067 falls on a Sunday.
December 25th, 2072 falls on a Sunday.
December 25th, 2078 falls on a Sunday.
December 25th, 2089 falls on a Sunday.
December 25th, 2095 falls on a Sunday.
December 25th, 2101 falls on a Sunday.
December 25th, 2107 falls on a Sunday.
December 25th, 2112 falls on a Sunday.
December 25th, 2118 falls on a Sunday.

```



## Ring


```ring

for n = 2008 to 2121
    if n < 2100 leap = n - 1900 else leap = n - 1904 ok
    m = (((n-1900)%7) + floor(leap/4) + 27) % 7
    if m = 4 see "25 Dec " + n + nl ok
next

```



## Ruby


```ruby
require 'date'

(2008..2121).each {|year| puts "25 Dec #{year}" if Date.new(year, 12, 25).sunday? }
```

```txt

25 Dec 2011
25 Dec 2016
25 Dec 2022
25 Dec 2033
25 Dec 2039
25 Dec 2044
25 Dec 2050
25 Dec 2061
25 Dec 2067
25 Dec 2072
25 Dec 2078
25 Dec 2089
25 Dec 2095
25 Dec 2101
25 Dec 2107
25 Dec 2112
25 Dec 2118

```


Or using the Time class

```ruby
(2008..2121).each {|year| puts "25 Dec #{year}" if Time.local(year, 12, 25).sunday?}
```

```txt

25 Dec 2011
25 Dec 2016
25 Dec 2022
25 Dec 2033
25 Dec 2039
25 Dec 2044
25 Dec 2050
25 Dec 2061
25 Dec 2067
25 Dec 2072
25 Dec 2078
25 Dec 2089
25 Dec 2095
25 Dec 2101
25 Dec 2107
25 Dec 2112
25 Dec 2118

```


(Note: The Time class could not handle dates beyond 2038 prior to Ruby 1.9.2.[https://www.ruby-lang.org/en/news/2010/08/18/ruby-1-9.2-released/])


## Run BASIC


```runbasic
for year = 2008 to 2121
 if val(date$("12-25-";year)) mod 7 = 5 then print "For ";year;"xmas is Sunday"
next year
```

```txt

For 2011 xmas is Sunday
For 2016 xmas is Sunday
For 2022 xmas is Sunday
For 2033 xmas is Sunday
For 2039 xmas is Sunday
For 2044 xmas is Sunday
For 2050 xmas is Sunday
For 2061 xmas is Sunday
For 2067 xmas is Sunday
For 2072 xmas is Sunday
For 2078 xmas is Sunday
For 2089 xmas is Sunday
For 2095 xmas is Sunday
For 2101 xmas is Sunday
For 2107 xmas is Sunday
For 2112 xmas is Sunday
For 2118 xmas is Sunday

```



## Rust


```rust
extern crate chrono;

use chrono::prelude::*;

fn main() {
    let years = (2008..2121).filter(|&y| Local.ymd(y, 12, 25).weekday() == Weekday::Sun).collect::<Vec<i32>>();
    println!("Years = {:?}", years);
}
```


Output:

```txt
Years = [2011, 2016, 2022, 2033, 2039, 2044, 2050, 2061, 2067, 2072, 2078, 2089, 2095, 2101, 2107, 2112, 2118]

```



## SAS


```sas
data _null_;
do y=2008 to 2121;
a=mdy(12,25,y);
if weekday(a)=1 then put y;
end;
run;

/* 2011 2016 2022 2033 2039 2044 2050 2061 2067
   2072 2078 2089 2095 2101 2107 2112 2118 */
```


=={{header|S-BASIC}}==

```BASIC

$constant SUNDAY = 0

rem - compute p mod q
function mod(p, q = integer) = integer
end = p - q * (p/q)

comment
    return day of week (Sun = 0, Mon = 1, etc.) for a
    given Gregorian calendar date using Zeller's congruence
end
function dayofweek (mo, da, yr = integer) = integer
    var y, c, z = integer
    if mo < 3 then
        begin
            mo = mo + 10
            yr = yr - 1
        end
    else mo = mo - 2
    y = mod(yr,100)
    c = int(yr / 100)
    z = int((26 * mo - 2) / 10)
    z = z + da + y + int(y/4) + int(c/4) - 2 * c + 777
    z = mod(z,7)
end = z

rem - main program
var year = integer
print "Christmas will fall on a Sunday in"
for year=2008 to 2121
   if dayofweek(12,25,year) = SUNDAY then
      print year
next year
end

```

```txt
Christmas will fall on a Sunday in
 2011
 2016
 2011
 2033
 2039
 2044
 2050
 2061
 2067
 2072
 2078
 2089
 2095
 2101
 2107
 2112
 2118
```



## Scala

===JDK (discouraged) ===

```scala
import java.util.{ Calendar, GregorianCalendar }
import Calendar.{ DAY_OF_WEEK, DECEMBER, SUNDAY }

object DayOfTheWeek extends App {
  val years = 2008 to 2121

  val yuletide =
    years.filter(year => (new GregorianCalendar(year, DECEMBER, 25)).get(DAY_OF_WEEK) == SUNDAY)

  // If you want a test: (optional)
  assert(yuletide ==
    Seq(2011, 2016, 2022, 2033, 2039, 2044, 2050, 2061,
      2067, 2072, 2078, 2089, 2095, 2101, 2107, 2112, 2118))

  println(yuletide.mkString(
    s"${yuletide.length} Years between ${years.head} and ${years.last}" +
      " including where Christmas is observed on Sunday:\n", ", ", "."))
}
```

===JDK >= 8 (recommended)===

### =Naive programming=


```scala
import java.time.{ DayOfWeek, LocalDate }

object DayOfTheWeek1 extends App {
  val years = 2008 to 2121
  val yuletide = for {
    year <- years
    if LocalDate.of(year, 12, 25).getDayOfWeek() == DayOfWeek.SUNDAY
  } yield year

  println(yuletide.mkString(
    s"${yuletide.count(p => true)} Years between ${years.head} and ${years.last}" +
      " including where Christmas is observed on Sunday:\n", ", ", "."))
}
```


### =Idiomatic programming=


```scala
import java.time.{ DayOfWeek, LocalDate }

object DayOfTheWeek1 extends App {
  val years = 2008 to 2121
  val yuletide =
    years.filter(year => (LocalDate.of(year, 12, 25).getDayOfWeek() == DayOfWeek.SUNDAY))

  // If you want a test: (optional)
  assert(yuletide ==
    Seq(2011, 2016, 2022, 2033, 2039, 2044, 2050, 2061,
      2067, 2072, 2078, 2089, 2095, 2101, 2107, 2112, 2118))

  println(yuletide.mkString(
    s"${yuletide.length} Years between ${years.head} and ${years.last}" +
      " including where Christmas is observed on Sunday:\n", ", ", "."))
}
```


### =Tail recursion=


```scala
import java.time.{ DayOfWeek, LocalDate }
import scala.annotation.tailrec

object DayOfTheWeek3 extends App {
  val years = 2008 to 2121
  val yuletide = {
    @tailrec
    def inner(anni: List[Int], accu: List[Int]): List[Int] = {
      if (anni == Nil) accu
      else inner(anni.tail, accu ++
        (if (LocalDate.of(anni.head, 12, 25).getDayOfWeek() == DayOfWeek.SUNDAY) List(anni.head)
        else Nil))
    }
    inner(years.toList, Nil)
  }

  // If you want a test: (optional)
  assert(yuletide ==
    Seq(2011, 2016, 2022, 2033, 2039, 2044, 2050, 2061,
      2067, 2072, 2078, 2089, 2095, 2101, 2107, 2112, 2118))

  println(yuletide.mkString(
    s"${yuletide.length} Years between ${years.head} and ${years.last}" +
      " including where Christmas is observed on Sunday:\n", ", ", "."))
}
```

```txt
Years between 2008 and 2121 including when Christmas is observed on Sunday:
2011, 2016, 2022, 2033, 2039, 2044, 2050, 2061, 2067, 2072, 2078, 2089, 2095, 2101, 2107, 2112, 2118.
```



## Scheme


```scheme
(define (day-of-week year month day)
(if (< month 3)
    (begin (set! month (+ month 12)) (set! year (- year 1))))
(+ 1
   (remainder (+ 5 day (quotient (* (+ 1 month) 13) 5)
                 year (quotient year 4) (* (quotient year 100) 6) (quotient year 400))
              7)))

(define (task)
(let loop ((y 2121) (v '()))
(if (< y 2008)
    v
    (loop (- y 1)
          (if (= 7 (day-of-week y 12 25))
              (cons y v)
              v)))))

(task)
; (2011 2016 2022 2033 2039 2044 2050 2061 2067 2072 2078 2089 2095 2101 2107 2112 2118)
```



## Seed7

The library [http://seed7.sourceforge.net/libraries/time.htm time.s7i] defines
the function [http://seed7.sourceforge.net/libraries/time.htm#dayOfWeek%28in_time%29 dayOfWeek],
which returns 1 for monday, 2 for tuesday, and so on up to 7 for sunday.

```seed7
$ include "seed7_05.s7i";
  include "time.s7i";

const proc: main is func
  local
    var integer: year is 0;
  begin
    for year range 2008 to 2122 do
      if dayOfWeek(date(year, 12, 25)) = 7 then
        writeln("Christmas comes on a sunday in " <& year);
      end if;
    end for;
  end func;
```

```txt

Christmas comes on a sunday in 2011
Christmas comes on a sunday in 2016
Christmas comes on a sunday in 2022
Christmas comes on a sunday in 2033
Christmas comes on a sunday in 2039
Christmas comes on a sunday in 2044
Christmas comes on a sunday in 2050
Christmas comes on a sunday in 2061
Christmas comes on a sunday in 2067
Christmas comes on a sunday in 2072
Christmas comes on a sunday in 2078
Christmas comes on a sunday in 2089
Christmas comes on a sunday in 2095
Christmas comes on a sunday in 2101
Christmas comes on a sunday in 2107
Christmas comes on a sunday in 2112
Christmas comes on a sunday in 2118

```



## Sidef

```ruby
require('Time::Local')
 
for year in (2008 .. 2121) {
    var time = %S<Time::Local>.timelocal(0,0,0,25,11,year)
    var wd = Time(time).local.wday
    if (wd == 0) {
        say "25 Dec #{year} is Sunday"
    }
}
```

```txt

25 Dec 2011 is Sunday
25 Dec 2016 is Sunday
25 Dec 2022 is Sunday
25 Dec 2033 is Sunday
25 Dec 2039 is Sunday
25 Dec 2044 is Sunday
25 Dec 2050 is Sunday
25 Dec 2061 is Sunday
25 Dec 2067 is Sunday
25 Dec 2072 is Sunday
25 Dec 2078 is Sunday
25 Dec 2089 is Sunday
25 Dec 2095 is Sunday
25 Dec 2101 is Sunday
25 Dec 2107 is Sunday
25 Dec 2112 is Sunday
25 Dec 2118 is Sunday

```



## Smalltalk


```smalltalk
2008 to: 2121 do: [ :year | |date|
     date := Date newDay: 25 monthIndex: 12 year: year.
     date dayName = #Sunday
       ifTrue: [ date displayNl ]
]
```


```txt
25-Dec-2011
25-Dec-2016
25-Dec-2022
25-Dec-2033
25-Dec-2039
25-Dec-2044
25-Dec-2050
25-Dec-2061
25-Dec-2067
25-Dec-2072
25-Dec-2078
25-Dec-2089
25-Dec-2095
25-Dec-2101
25-Dec-2107
25-Dec-2112
25-Dec-2118
```



## SQL


SQL has good support for date functions; care must be taken with NLS settings (globalization support), in the code below the date format language is passed in as an argument to the relevant function.


```sql
select extract(year from dt) as year_with_xmas_on_sunday
from   (
         select  add_months(date '2008-12-25', 12 * (level - 1)) as dt
         from    dual
         connect by level <= 2121 - 2008 + 1
       )

where  to_char(dt, 'Dy', 'nls_date_language=English') = 'Sun'
order  by 1
;
```



```txt

YEAR_WITH_XMAS_ON_SUNDAY
------------------------
                    2011
                    2016
                    2022
                    2033
                    2039
                    2044
                    2050
                    2061
                    2067
                    2072
                    2078
                    2089
                    2095
                    2101
                    2107
                    2112
                    2118

17 rows selected.
```



## Stata


```stata
clear
sca n=2121-2008+1
set obs `=n'
gen year=2007+_n
list if dow(mdy(12,25,year))==0, noobs sep(0)

  +------+
  | year |
  |------|
  | 2011 |
  | 2016 |
  | 2022 |
  | 2033 |
  | 2039 |
  | 2044 |
  | 2050 |
  | 2061 |
  | 2067 |
  | 2072 |
  | 2078 |
  | 2089 |
  | 2095 |
  | 2101 |
  | 2107 |
  | 2112 |
  | 2118 |
  +------+
```



###  Mata



```mata
year=2008::2121
select(year,dow(mdy(12,25,year)):==0)
```



## Suneido


```Suneido
year = 2008
while (year <= 2121)
    {
    if Date('#' $ year $ '1225').WeekDay() is 0
        Print(year)
    ++year
    }
```

```txt
2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118
```


## Standard ML


```sml
(* Call:  yearsOfSundayXmas(2008, 2121)   *)
fun yearsOfSundayXmas(fromYear, toYear) =
  if fromYear>toYear then
    ()
  else
    let
      val d = Date.date {year=fromYear, month=Date.Dec, day=25,
              hour=0, minute=0, second=0,
                      offset=SOME Time.zeroTime}
      val wd = Date.weekDay d
    in
      if wd=Date.Sun then
        (
          print(Int.toString fromYear ^ "\n");
          yearsOfSundayXmas(fromYear+1, toYear)
        )
      else
        yearsOfSundayXmas(fromYear+1, toYear)
    end;
```


```txt
- yearsOfSundayXmas(2008, 2121);
2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118
```





## Swift


```Swift
import Cocoa

var year=2008
let formatter=NSDateFormatter()
formatter.dateFormat = "yyyy-MM-dd"

let gregorian:NSCalendar! = NSCalendar(calendarIdentifier: NSCalendarIdentifierGregorian)
while (year<2122){
    var date:NSDate!=formatter.dateFromString(String(year)+"-12-25")
    var components=gregorian.components(NSCalendarUnit.CalendarUnitWeekday, fromDate: date)
    var dayOfWeek:NSInteger=components.weekday
    if(dayOfWeek==1){
        println(year)
    }
    year++
}
```

```txt
2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118
```



## Tcl

```tcl
package require Tcl 8.5

for {set y 2008} {$y <= 2121} {incr y} {
    if {[clock format [clock scan "$y-12-25" -format {%Y-%m-%d}] -format %w] == 0} {
        puts "xmas $y is a sunday"
    }
}
```

```txt
xmas 2011 is a sunday
xmas 2016 is a sunday
xmas 2022 is a sunday
xmas 2033 is a sunday
xmas 2039 is a sunday
xmas 2044 is a sunday
xmas 2050 is a sunday
xmas 2061 is a sunday
xmas 2067 is a sunday
xmas 2072 is a sunday
xmas 2078 is a sunday
xmas 2089 is a sunday
xmas 2095 is a sunday
xmas 2101 is a sunday
xmas 2107 is a sunday
xmas 2112 is a sunday
xmas 2118 is a sunday
```


=={{header|TI-83 BASIC}}==
'''Works with''' TI-84+/SE only

```ti83b

:For(A,2008,2121
:If dayofWk(A,12,25)=1
:Disp A
:End

```

```txt
2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118
Done
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
PRINT "25th of December will be a Sunday in the following years: "
LOOP year=2008,2121
SET dayofweek = DATE (number,25,12,year,nummer)
IF (dayofweek==7) PRINT year
ENDLOOP

```

```txt

25th of December will be a Sunday in the following years:
2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118

```



## UNIX Shell

Unix commands may use ''time_t'' to count seconds since the [[show the epoch|epoch]]. For systems with 32-bit time, the counter overflows during 19 January 2038. These scripts continue to 2121 and may need a system with 64-bit time, to prevent the overflow.


### With GNU date

This solution uses date -d, which seems to be a [[GNU]] extension, so it only works with those systems.

```bash
#! /bin/bash

for (( i=2008; i<=2121; ++i ))
do
 date -d "$i-12-25"
done  |grep Sun

exit 0
```


The first lines of output (from a GNU/Linux system with 32bit time_t, date version 6.9) are


```bash
Sun Dec 25 00:00:00 CET 2011
Sun Dec 25 00:00:00 CET 2016
Sun Dec 25 00:00:00 CET 2022
Sun Dec 25 00:00:00 CET 2033
date: invalid date `2038-12-25'
```


I.e., starting from year 2038, the <tt>date</tt> command (which uses the glibc library, at least on GNU systems), is not able to recognise the date as a valid one!

''Different machine/OS version (64 bit time_t):''
This is the same command run on RedHat Linux.

```bash
bash-3.00$ date --version
date (coreutils) 5.2.1
Written by David MacKenzie.

Copyright (C) 2004 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
bash-3.00$ uname -a
Linux brslln01 2.6.9-67.ELsmp #1 SMP Wed Nov 7 13:56:44 EST 2007 x86_64 x86_64 x86_64 GNU/Linux
bash-3.00$ for((i=2009; i <= 2121; i++)); do  date -d "$i-12-25" |egrep Sun; done
Sun Dec 25 00:00:00 GMT 2011
Sun Dec 25 00:00:00 GMT 2016
Sun Dec 25 00:00:00 GMT 2022
Sun Dec 25 00:00:00 GMT 2033
Sun Dec 25 00:00:00 GMT 2039
Sun Dec 25 00:00:00 GMT 2044
Sun Dec 25 00:00:00 GMT 2050
Sun Dec 25 00:00:00 GMT 2061
Sun Dec 25 00:00:00 GMT 2067
Sun Dec 25 00:00:00 GMT 2072
Sun Dec 25 00:00:00 GMT 2078
Sun Dec 25 00:00:00 GMT 2089
Sun Dec 25 00:00:00 GMT 2095
Sun Dec 25 00:00:00 GMT 2101
Sun Dec 25 00:00:00 GMT 2107
Sun Dec 25 00:00:00 GMT 2112
Sun Dec 25 00:00:00 GMT 2118
bash-3.00$
```


===With GNU date and GNU seq ({{header|UnixPipes}})===
Like the previous solution, this solution uses date -d, which seems to be a [[GNU]] extension. Output is same as previous solution.


```bash
seq 2008 2121 | xargs -IYEAR -n 1 date +%c -d 'Dec 25 YEAR' | grep Sun
```



### With Unix cal

The <code>cal</code> command is a tradition since Version 6 AT&T UNIX. This solution assumes that <code>cal</code> will always output a calendar in this format.


```txt
$ cal 12 2011
   December 2011
Su Mo Tu We Th Fr Sa
             1  2  3
 4  5  6  7  8  9 10
11 12 13 14 15 16 17
18 19 20 21 22 23 24
25 26 27 28 29 30 31

```


This format always puts Sunday in columns 1 and 2. The solution uses ''tail'' to delete the first 2 lines (month, year, names of days), ''cut'' to extract Sunday's columns, and ''grep'' to check if "25" appears in those columns.

```bash
y=2008
while test $y -lt 2122; do
	cal 12 $y | tail +3 | cut -c1-2 | grep -Fq 25 && echo 25 Dec $y
	y=`expr $y + 1`
done
```


Running this script with [[OpenBSD]], the output is identical to the C# program. OpenBSD ''cal'' accepts any year from 1 to 9999, so 2008 to 2122 is well within range.


### With zsh


```bash
zmodload zsh/datetime
for (( year = 2010; year <= 2121; year++ ));
  if [[ $(strftime '%A' $(strftime -r '%F' $year-12-25)) == Sunday ]] print $year
```


If the system has 32-bit time, this script will malfunction for years >= 2038; it will print no year from 2038 to 2121 (unless today is Sunday, then it prints every year from 2038 to 2121). This happens because ''strftime -r '%F' $year-12-25'' yields -1 for an out-of-range date, and ''strftime '%A' -1'' yields name of today.


## Ursala


A standard library, <code>stt</code>, provides basic date manipulation functions,
and is imported in this example. Unix era times denominated in seconds since
1969 (excluding leap seconds) are represented as natural numbers with
unlimited precision.  Results are valid for the arbitrarily distant
future assuming the Gregorian calendar remains in effect.

The algorithm relies on the <code>string_to_time</code> function converting a date
expressed as a character string to seconds without needing a weekday field in
the input, and the <code>time_to_string</code> function outputting the corresponding
date with the weekday included. The output is then filtered for Sundays.


```Ursala
#import std
#import nat
#import stt

christmases = time_to_string* string_to_time*TS 'Dec 25 0:0:0 '-*@hS %nP* nrange/2008 2121

#show+

sunday_years = ~&zS sep` * =]'Sun'*~ christmases
```

```txt
2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118
```



## VBA


```vb
Option Explicit

Sub MainDayOfTheWeek()
    Debug.Print "Xmas will be a Sunday in : " & XmasSunday(2008, 2121)
End Sub

Private Function XmasSunday(firstYear As Integer, lastYear As Integer) As String
Dim i As Integer, temp$
    For i = firstYear To lastYear
        If Weekday(CDate("25/12/" & i)) = vbSunday Then temp = temp & ", " & i
    Next
    XmasSunday = Mid(temp, 2)
End Function
```


```txt
Xmas will be a Sunday in :  2011, 2016, 2022, 2033, 2039, 2044, 2050, 2061, 2067, 2072, 2078, 2089, 2095, 2101, 2107, 2112, 2118
```



## VBScript


```vb
For year = 2008 To 2121
    If Weekday(DateSerial(year, 12, 25)) = 1 Then
        WScript.Echo year
    End If
Next
```


```txt

2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118

```



## Vedit macro language


```vedit
Buf_Switch(Buf_Free)
for (#3 = 2008; #3 < 2122; #3++) {
    Reg_Set(10, "12/25/")
    Num_Str(#3, 10, LEFT+APPEND)
    if (JDate(@10) % 7 == 0) {
	Num_Ins(#3, NOCR)
    }
}
```


```txt

2011 2016 2022 2033 2039 2044 2050 2061 2067 2072 2078 2089 2095 2101 2107 2112 2118

```



## Visual Objects


```visualfoxpro

local i as dword

for i := 2008 upto 2121
	if DOW(ConDate(i, 12, 25)) = 1
		? AsString(i)
	endif
next i

```


```txt

2011 2016 2022 2033 2039 2044 2050 2061 2067 2072 2078 2089 2095 2101 2107 2112 2118

```



## Wortel


```wortel
!-&y = 0 `.getDay. @new Date[y 11 25] @range[2008 2121]
```

Returns:
```txt
[2011 2016 2022 2033 2039 2044 2050 2061 2067 2072 2078 2089 2095 2101 2107 2112 2118]
```



## XPL0

The original routine in the library only worked correctly between the years
1980 and 2099. It was upgraded with this new routine that handles all dates in
the Gregorian calendar, from 1583 onward. It's based on Zeller's Congruence.


```XPL0
include c:\cxpl\codes;                  \intrinsic 'code' declarations

func    WeekDay(Year, Month, Day);      \Return day of week (0=Sat 1=Sun..6=Fri)
int     Year, Month, Day;
[if Month<=2 then [Month:= Month+12;  Year:= Year-1];
return rem((Day + (Month+1)*26/10 + Year + Year/4 + Year/100*6 + Year/400) / 7);
];      \WeekDay


int     Year;
[for Year:= 2008 to 2121 do
    if WeekDay(Year, 12, 25) = 1 then   \25th of December is a Sunday
        [IntOut(0, Year);  CrLf(0)];
]
```


```txt

2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118

```



## Yabasic

```Yabasic
sub wd(m, d, y)
  If m < 3 Then        // If m = 1 Or m = 2 Then
    m = m + 12
    y = y - 1
  End If
  Return mod((y + int(y / 4) - int(y / 100) + int(y / 400) + d + int((153 * m + 8) / 5)), 7)
End sub

// ------=< MAIN >=------

For yr = 2008 To 2121
  If wd(12, 25, yr) = 0 Then
    Print "Dec 25 ", yr
  EndIf
Next
```



## zkl

ISO dates, monday is 1, sunday is 7

```zkl
var [const] D=Time.Date;
foreach y in ([2008..2121]){
   if (D.Sunday==D.weekDay(y,12,25)) println(y)
}
```

Or, in a more functional manner:

```zkl
var [const] D=Time.Date;
[2008..2121].filter(fcn(y){ D.Sunday==D.weekDay(y,12,25) }).println()
```

```txt

2011
2016
2022
2033
2039
2044
2050
2061
2067
2072
2078
2089
2095
2101
2107
2112
2118

```



## zonnon


```zonnon

module Main;
(*Access to Mono System package *)
import System;

var
	now: System.DateTime;
begin
	now := System.DateTime.Now;
	System.Console.Write(now.ToString("yyyy-MM-dd :"));
	System.Console.WriteLine(now.DayOfWeek);
end Main.

```

```txt

2017-12-05 :Tuesday

```


## ZX Spectrum Basic

```zxbasic
10 CLS
20 FOR y=2008 TO 2121
30 LET year=y: LET m=12: LET d=25: GO SUB 1000
40 IF wd=0 THEN PRINT d;" ";m;" ";y
50 NEXT y
60 STOP
1000 REM week day
1010 IF m=1 OR m=2 THEN LET m=m+12: LET year=year-1
1020 LET wd=FN m(year+INT (year/4)-INT (year/100)+INT (year/400)+d+INT ((153*m+8)/5),7)
1030 RETURN
1100 DEF FN m(a,b)=a-INT (a/b)*b
```

