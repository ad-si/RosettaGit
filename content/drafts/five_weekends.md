+++
title = "Five weekends"
description = ""
date = 2019-10-01T21:43:09Z
aliases = []
[extra]
id = 8562
[taxonomies]
categories = []
tags = []
+++

{{task|Date and time}}
[[Category:Puzzles]]
The month of October in 2010 has five Fridays, five Saturdays, and five Sundays.


;Task:
# Write a program to show all months that have this same characteristic of five full weekends from the year 1900 through 2100 (Gregorian calendar).
# Show the ''number'' of months with this property (there should be 201).
# Show at least the first and last five dates, in order.


'''Algorithm suggestions'''
* Count the number of Fridays, Saturdays, and Sundays in every month.
* Find all of the 31-day months that begin on Friday.


'''Extra credit'''

Count and/or show all of the years which do not have at least one five-weekend month (there should be 29).


;Related tasks
* [[Day of the week]]
* [[Last Friday of each month]]
* [[Find last sunday of each month]]





## 360 Assembly

{{trans|Fortran}}
For maximum compatibility, this program uses only the basic instruction set (S/360)
and two ASSIST macros (XDECO, XPRNT) to keep the code as short as possible.

```360asm
*        Five weekends             31/05/2016
FIVEWEEK CSECT
         USING  FIVEWEEK,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         "
         ST     R15,8(R13)         "
         LR     R13,R15            "
         LM     R10,R11,=AL8(0)    nko=0; nok=0
         LH     R6,Y1              y=y1
LOOPY    CH     R6,Y2              do y=y1 to y2
         BH     ELOOPY
         MVI    YF,X'00'           yf=0
         LA     R7,1               im=1
LOOPIM   C      R7,=F'7'           do im=1 to 7
         BH     ELOOPIM
         LR     R1,R7              im
         SLA    R1,1               *2 (H)
         LH     R2,ML-2(R1)        ml(im)
         ST     R2,M               m=ml(im)
         MVC    D,=F'1'            d=1
         L      R4,M               m
         C      R4,=F'2'           if m<=2
         BH     MSUP2
         L      R8,M               m
         LA     R8,12(R8)          mw=m+12
         LR     R9,R6              y
         BCTR   R9,0               yw=y-1
         B      EMSUP2
MSUP2    L      R8,M               mw=m
         LR     R9,R6              yw=y
EMSUP2   LR     R4,R9              ym
         SRDA   R4,32              .
         D      R4,=F'100'         yw/100
         ST     R5,J               j=yw/100
         ST     R4,K               k=yw//100
         LR     R4,R8              mw
         LA     R4,1(R4)           mw+1
         MH     R4,=H'26'          (mw+1)*26
         SRDA   R4,32              .
         D      R4,=F'10'          (mw+1)*26/10
         LR     R2,R5              "
         A      R2,D               d
         A      R2,K               d+k
         L      R3,K               k
         SRA    R3,2               k/4
         AR     R2,R3              (mw+1)*26/10+k/4
         L      R3,J               j
         SRA    R3,2               j/4
         AR     R2,R3              (mw+1)*26/10+k/4+j/4
         LA     R5,5               5
         M      R4,J               5*j
         AR     R2,R5              (mw+1)*26/10+k/4+j/4
         SRDA   R2,32              .
         D      R2,=F'7'           (d+(mw+1)*26/10+k+k/4+j/4+5*j)/7
         C      R2,=F'6'           if dow=friday
         BNE    NOFRIDAY
         XDECO  R6,XDEC            y
         MVC    PG+0(4),XDEC+8     output y
         LR     R1,R7              im
         MH     R1,=H'3'           *3
         LA     R14,MN-3(R1)       @mn(im)
         MVC    PG+5(3),0(R14)     output mn(im)
         XPRNT  PG,8               print buffer
         LA     R11,1(R11)         nok=nok+1
         MVI    YF,X'01'           yf=1
NOFRIDAY LA     R7,1(R7)           im=im+1
         B      LOOPIM
ELOOPIM  L      R4,YF              yf
         CLI    YF,X'00'           if yf=0
         BNE    EYFNE0
         LA     R10,1(R10)         nko=nko+1
EYFNE0   LA     R6,1(R6)           y=y+1
         B      LOOPY
ELOOPY   XDECO  R11,XDEC           nok
         MVC    PG+0(4),XDEC+8     output nok
         MVC    PG+4(12),=C' occurrences'
         XPRNT  PG,80              print buffer
         XDECO  R10,XDEC           nko
         MVC    PG+0(4),XDEC+8     output nko
         MVC    PG+4(33),=C' years with no five weekend month'
         XPRNT  PG,80              print buffer
         L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    "
         XR     R15,R15            "
         BR     R14                exit
Y1       DC     H'1900'            year start
Y2       DC     H'2100'            year stop
ML       DC     H'1',H'3',H'5',H'7',H'8',H'10',H'12'
MN       DC     C'jan',C'mar',C'may',C'jul',C'aug',C'oct',C'dec'
YF       DS     X                  year flag
M        DS     F                  month
D        DS     F                  day
J        DS     F                  j=yw/100
K        DS     F                  j=mod(yw,100)
PG       DC     CL80'....-'        buffer
XDEC     DS     CL12               temp for XDECO
         YREGS
         END    FIVEWEEK
```

{{out}}

```txt

1901-mar
1902-aug
1903-may
1904-jan
1904-jul
1905-dec
1907-mar
1908-may
...
2094-jan
2094-oct
2095-jul
2097-mar
2098-aug
2099-may
2100-jan
2100-oct
 201 occurrences
  29 years with no five weekend month

```



## Ada


```Ada

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Calendar.Formatting;  use Ada.Calendar;

use Ada.Calendar.Formatting;

procedure Five_Weekends is
   Months : Natural := 0;
begin
   for Year in Year_Number range 1901..2100 loop
      for Month in Month_Number range 1..12 loop
         begin
            if Day_Of_Week (Formatting.Time_Of (Year, Month, 31)) = Sunday then
               Put_Line (Year_Number'Image (Year) & Month_Number'Image (Month));
               Months := Months + 1;
            end if;
         exception
            when Time_Error =>
               null;
         end;
      end loop;
   end loop;
   Put_Line ("Number of months:" & Integer'Image (Months));
end Five_Weekends;

```

{{out|Sample output}}
<pre style="height:30ex;overflow:scroll">
 1901 3
 1902 8
 1903 5
 1904 1
 1904 7
 1905 12
 1907 3
 1908 5
 1909 1
 1909 10
 1910 7
 1911 12
 1912 3
 1913 8
 1914 5
 1915 1
 1915 10
 1916 12
 1918 3
 1919 8
 1920 10
 1921 7
 1922 12
 1924 8
 1925 5
 1926 1
 1926 10
 1927 7
 1929 3
 1930 8
 1931 5
 1932 1
 1932 7
 1933 12
 1935 3
 1936 5
 1937 1
 1937 10
 1938 7
 1939 12
 1940 3
 1941 8
 1942 5
 1943 1
 1943 10
 1944 12
 1946 3
 1947 8
 1948 10
 1949 7
 1950 12
 1952 8
 1953 5
 1954 1
 1954 10
 1955 7
 1957 3
 1958 8
 1959 5
 1960 1
 1960 7
 1961 12
 1963 3
 1964 5
 1965 1
 1965 10
 1966 7
 1967 12
 1968 3
 1969 8
 1970 5
 1971 1
 1971 10
 1972 12
 1974 3
 1975 8
 1976 10
 1977 7
 1978 12
 1980 8
 1981 5
 1982 1
 1982 10
 1983 7
 1985 3
 1986 8
 1987 5
 1988 1
 1988 7
 1989 12
 1991 3
 1992 5
 1993 1
 1993 10
 1994 7
 1995 12
 1996 3
 1997 8
 1998 5
 1999 1
 1999 10
 2000 12
 2002 3
 2003 8
 2004 10
 2005 7
 2006 12
 2008 8
 2009 5
 2010 1
 2010 10
 2011 7
 2013 3
 2014 8
 2015 5
 2016 1
 2016 7
 2017 12
 2019 3
 2020 5
 2021 1
 2021 10
 2022 7
 2023 12
 2024 3
 2025 8
 2026 5
 2027 1
 2027 10
 2028 12
 2030 3
 2031 8
 2032 10
 2033 7
 2034 12
 2036 8
 2037 5
 2038 1
 2038 10
 2039 7
 2041 3
 2042 8
 2043 5
 2044 1
 2044 7
 2045 12
 2047 3
 2048 5
 2049 1
 2049 10
 2050 7
 2051 12
 2052 3
 2053 8
 2054 5
 2055 1
 2055 10
 2056 12
 2058 3
 2059 8
 2060 10
 2061 7
 2062 12
 2064 8
 2065 5
 2066 1
 2066 10
 2067 7
 2069 3
 2070 8
 2071 5
 2072 1
 2072 7
 2073 12
 2075 3
 2076 5
 2077 1
 2077 10
 2078 7
 2079 12
 2080 3
 2081 8
 2082 5
 2083 1
 2083 10
 2084 12
 2086 3
 2087 8
 2088 10
 2089 7
 2090 12
 2092 8
 2093 5
 2094 1
 2094 10
 2095 7
 2097 3
 2098 8
 2099 5
 2100 1
 2100 10
Number of months: 201

```



## AppleScript



### Imperative



```AppleScript
set fiveWeekendMonths to {}
set noFiveWeekendYears to {}

set someDate to current date
set day of someDate to 1

repeat with someYear from 1900 to 2100
  set year of someDate to someYear
  set foundOne to false
  repeat with someMonth in {January, March, May, July, ¬
                            August, October, December}
    set month of someDate to someMonth
    if weekday of someDate is Friday then
        set foundOne to true
        set end of fiveWeekendMonths to ¬
             (someYear as text) & "-" & (someMonth as text)
    end
  end repeat
  if not foundOne then
    set end of noFiveWeekendYears to someYear
  end
end repeat

set text item delimiters to ", "
set monthList to ¬
  (items 1 thru 5 of fiveWeekendMonths as text) & ", ..." & linefeed & ¬
   " ..., " & (items -5 thru end of fiveWeekendMonths as text)

set monthCount to count fiveWeekendMonths
set yearCount to count noFiveWeekendYears

set resultText to ¬
  "Months with five weekends (" & monthCount & "): " & linefeed & ¬
  "      " & monthList & linefeed & linefeed & ¬
  "Years with no such months (" & yearCount & "): "

set y to 1
repeat while y < yearCount
  set final to y+11
  if final > yearCount then
    set final to yearCount
  end
  set resultText to ¬
    resultText & linefeed & ¬
    "      " & (items y through final of noFiveWeekendYears as text)
  set y to y + 12
end
resultText
```


{{out}} (can always add "display dialog resultText" for GUI output):

```txt
Months with five weekends (201):
      1901-March, 1902-August, 1903-May, 1904-January, 1904-July, ...
 ..., 2097-March, 2098-August, 2099-May, 2100-January, 2100-October

Years with no such months (29):
      1900, 1906, 1917, 1923, 1928, 1934, 1945, 1951, 1956, 1962, 1973, 1979
      1984, 1990, 2001, 2007, 2012, 2018, 2029, 2035, 2040, 2046, 2057, 2063
      2068, 2074, 2085, 2091, 2096
```



### Functional


```AppleScript
-- TEST -----------------------------------------------------------------------
on run

    fiveWeekends(1900, 2100)

end run

-- FIVE WEEKENDS --------------------------------------------------------------

-- fiveWeekends :: Int -> Int -> Record
on fiveWeekends(fromYear, toYear)
    set lstYears to enumFromTo(fromYear, toYear)

    -- yearMonthString :: (Int, Int) -> String
    script yearMonthString
        on |λ|(lstYearMonth)
            ((item 1 of lstYearMonth) as string) & " " & ¬
                item (item 2 of lstYearMonth) of ¬
                {"January", "", "March", "", "May", "", ¬
                    "July", "August", "", "October", "", "December"}
        end |λ|
    end script

    -- addLongMonthsOfYear :: [(Int, Int)] -> [(Int, Int)]
    script addLongMonthsOfYear
        on |λ|(lstYearMonth, intYear)

            -- yearMonth :: Int -> (Int, Int)
            script yearMonth
                on |λ|(intMonth)
                    {intYear, intMonth}
                end |λ|
            end script

            lstYearMonth & ¬
                map(yearMonth, my longMonthsStartingFriday(intYear))
        end |λ|
    end script

    -- leanYear :: Int -> Bool
    script leanYear
        on |λ|(intYear)
            0 = length of longMonthsStartingFriday(intYear)
        end |λ|
    end script

    set lstFullMonths to map(yearMonthString, ¬
        foldl(addLongMonthsOfYear, {}, lstYears))

    set lstLeanYears to filter(leanYear, lstYears)

    {{|number|:length of lstFullMonths}, ¬
        {firstFive:(items 1 thru 5 of lstFullMonths)}, ¬
        {lastFive:(items -5 thru -1 of lstFullMonths)}, ¬
        {leanYearCount:length of lstLeanYears}, ¬
        {leanYears:lstLeanYears}}
end fiveWeekends

-- longMonthsStartingFriday :: Int -> [Int]
on longMonthsStartingFriday(intYear)

    --     startIsFriday :: Int -> Bool
    script startIsFriday
        on |λ|(iMonth)
            weekday of calendarDate(intYear, iMonth, 1) is Friday
        end |λ|
    end script

    filter(startIsFriday, [1, 3, 5, 7, 8, 10, 12])
end longMonthsStartingFriday

-- calendarDate :: Int -> Int -> Int -> Date
on calendarDate(intYear, intMonth, intDay)
    tell (current date)
        set {its year, its month, its day, its time} to ¬
            {intYear, intMonth, intDay, 0}
        return it
    end tell
end calendarDate

-- GENERIC FUNCTIONS ----------------------------------------------------------

-- enumFromTo :: Enum a => a -> a -> [a]
on enumFromTo(m, n)
    set {intM, intN} to {fromEnum(m), fromEnum(n)}

    if intM > intN then
        set d to -1
    else
        set d to 1
    end if
    set lst to {}
    if class of m is text then
        repeat with i from intM to intN by d
            set end of lst to chr(i)
        end repeat
    else
        repeat with i from intM to intN by d
            set end of lst to i
        end repeat
    end if
    return lst
end enumFromTo

-- fromEnum :: Enum a => a -> Int
on fromEnum(x)
    set c to class of x
    if c is boolean then
        if x then
            1
        else
            0
        end if
    else if c is text then
        if x ≠ "" then
            id of x
        else
            missing value
        end if
    else
        x as integer
    end if
end fromEnum

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
```

{{Out}}

```AppleScript
{{|number|:201},
{firstFive:{"1901 March", "1902 August", "1903 May", "1904 January", "1904 July"}},
{lastFive:{"2097 March", "2098 August", "2099 May", "2100 January", "2100 October"}},
{leanYearCount:29},
{leanYears:{1900, 1906, 1917, 1923, 1928, 1934, 1945, 1951, 1956, 1962, 1973, 1979,
1984, 1990, 2001, 2007, 2012, 2018, 2029, 2035, 2040, 2046, 2057, 2063, 2068, 2074,
2085, 2091, 2096}}}
```



## AutoHotkey


```AutoHotkey
Year := 1900
End_Year := 2100
31_Day_Months = 01,03,05,07,08,10,12

While Year <= End_Year
  {
    Loop, Parse, 31_Day_Months, CSV
      {
        FormatTime, Day, %Year%%A_LoopField%01, dddd
        IfEqual, Day, Friday
          {
            All_Months_With_5_Weekends .=  A_LoopField . "/" . Year ",    "
            5_Weekend_Count++
            Year_Has_5_Weekend_Month := 1
          }
      }
   IfEqual, Year_Has_5_Weekend_Month, 0
      {
        All_Years_Without_5_Weekend .= Year ",   "
        No_5_Weekend_Count ++
      }
   Year ++
   Year_Has_5_Weekend_Month := 0
  }
; Trim the spaces and comma off the last item.
StringTrimRight, All_Months_With_5_Weekends, All_Months_With_5_Weekends, 5
StringTrimRight, All_Years_Without_5_Weekend, All_Years_Without_5_Weekend, 4
MsgBox,
(
Months with 5 day weekends between 1900 and 2100 : %5_Weekend_Count%
%All_Months_With_5_Weekends%
)
MsgBox,
(
Years with no 5 day weekends between 1900 and 2100 : %No_5_Weekend_Count%
%All_Years_Without_5_Weekend%
)
```



## AutoIt

Call the Funktion with $ret > 1 And $ret < 3 to determine Returned Array
1 - All found Weekend Dates
2 - Years Without 5 Weekend Months
3 - Year and Month with 5 Weekends

```AutoIt

#include <Date.au3>
#include <Array.au3>
$array = Five_weekends(1)
_ArrayDisplay($array)
$array = Five_weekends(2)
_ArrayDisplay($array)
$array = Five_weekends(3)
_ArrayDisplay($array)

Func Five_weekends($ret = 1)
	If $ret < 1 Or $ret > 3 Then Return SetError(1, 0, 0)
	Local $avDateArray[1]
	Local $avYearArray[1]
	Local $avMonthArray[1]
	For $iYear = 1900 To 2100
		Local $checkyear = False
		For $iMonth = 1 To 12
			If _DateDaysInMonth($iYear, $iMonth) <> 31 Then ContinueLoop ; Month has less then 31 Days
			If _DateToDayOfWeek($iYear, $iMonth, "01") <> 6 Then ContinueLoop ;First Day is not a Friday
			_ArrayAdd($avMonthArray, $iYear & "-" & _DateToMonth($iMonth))
			$checkyear = True
			For $s = 1 To 31
				Local $Date = _DateToDayOfWeek($iYear, $iMonth, $s)
				If $Date = 6 Or $Date = 7 Or $Date = 1 Then ; if Date is Friday, Saturday or Sunday
					_ArrayAdd($avDateArray, $iYear & "\" & StringFormat("%02d", $iMonth) & "\" & StringFormat("%02d", $s))
				EndIf
			Next
		Next
		If Not $checkyear Then _ArrayAdd($avYearArray, $iYear)
	Next
	$avDateArray[0] = UBound($avDateArray) - 1
	$avYearArray[0] = UBound($avYearArray) - 1
	$avMonthArray[0] = UBound($avMonthArray) - 1
	If $ret = 1 Then
		Return $avDateArray
	ElseIf $ret = 2 Then
		Return $avYearArray
	ElseIf $ret = 3 Then
		Return $avMonthArray
	EndIf
EndFunc   ;==>Five_weekends

```



## ALGOL 68

{{trans|Fortran|Note: This specimen retains the original [[#Fortran|Fortran]] coding style. [http://rosettacode.org/mw/index.php?title=Five_weekends&action=historysubmit&diff=107850&oldid=107848 diff]}}
{{works with|ALGOL 68|Revision 1 - no extensions to language used.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}

```algol68
five_weekends: BEGIN
  INT m, year, nfives := 0, not5 := 0;
  BOOL no5weekend;

  MODE MONTH = STRUCT(
    INT n,
    [3]CHAR name
  ) # MODE MONTH #;

  []MONTH month = (
    MONTH(13, "Jan"),
    MONTH(3,  "Mar"),
    MONTH(5,  "May"),
    MONTH(7,  "Jul"),
    MONTH(8,  "Aug"),
    MONTH(10, "Oct"),
    MONTH(12, "Dec")
  );

  FOR year FROM 1900 TO 2100 DO
    IF year = 1905 THEN printf($"..."l$) FI;
    no5weekend := TRUE;
    FOR m TO UPB month DO
      IF n OF month(m) = 13 THEN
        IF day_of_week(1, n OF month(m), year-1) = 6 THEN
          IF year<1905 OR year > 2096 THEN printf(($g, 5zl$, name OF month(m), year)) FI;
          nfives +:= 1;
          no5weekend := FALSE
        FI
      ELSE
        IF day_of_week(1, n OF month(m), year) = 6 THEN
          IF year<1905 OR year > 2096 THEN printf(($g, 5zl$, name OF month(m), year)) FI;
          nfives +:= 1;
          no5weekend := FALSE
        FI
      FI
    OD;
    IF no5weekend THEN not5 +:= 1 FI
  OD;

  printf(($g, g(0)l$, "Number of months with five weekends between 1900 and 2100 = ", nfives));
  printf(($g, g(0)l$, "Number of years between 1900 and 2100 with no five weekend months = ", not5));

# contains #

  PROC day_of_week = (INT d, m, y)INT: BEGIN
    INT j, k;
    j := y OVER 100;
    k := y MOD 100;
    (d + (m+1)*26 OVER 10 + k + k OVER 4 + j OVER 4 + 5*j) MOD 7
  END # function day_of_week #;
  SKIP
END # program five_weekends #
```

{{out}}

```txt

Mar 1901
Aug 1902
May 1903
Jan 1904
Jul 1904
...
Mar 2097
Aug 2098
May 2099
Jan 2100
Oct 2100
Number of months with five weekends between 1900 and 2100 = 201
Number of years between 1900 and 2100 with no five weekend months = 29

```



## AWK



```awk

# usage:  awk -f 5weekends.awk  cal.txt

# Filter a file of month-calendars, such as
# ...
##        January 1901
##    Mo Tu We Th Fr Sa Su
##        1  2  3  4  5  6
##     7  8  9 10 11 12 13
##    14 15 16 17 18 19 20
##    21 22 23 24 25 26 27
##    28 29 30 31
# ...
##         March 1901
##    Mo Tu We Th Fr Sa Su
##                 1  2  3
##     4  5  6  7  8  9 10
##    11 12 13 14 15 16 17
##    18 19 20 21 22 23 24
##    25 26 27 28 29 30 31
# ...
# This file is generated by a script for the unix-shell,
# see http://rosettacode.org/wiki/Five_weekends#UNIX_Shell

BEGIN   { print("# Month with 5 weekends:")
          badYears = numW5 = 0;
          lastW5 = -1
        }

0+$2>33 { if( $2>currYear ) {       # calendar-header: month, year
            if( lastW5==numW5 ) {
              badYears++; sep="\n"
              if( badYears % 10 ) { sep=" " }
              bY=bY currYear sep;   # collect years in string
            ##print badYears,":", currYear
            }
            lastW5=numW5
          }
          WE=0; currYear=$2; currMY = $1 " " $2;
        ##print currMY;
          next
        }

/^Mo/   { next }                    # skip lines with weekday-names

        { $0 = substr($0,13) }      # cut inputline, leave  Fr,Sa,Su only

NF>2    { WE++;                     # 3 fields left => complete weekend found
          if( WE>4 ) {
            numW5++; printf("%4d : %s\n", numW5, currMY)
          }
        }

END     { print("# Found", numW5, "month with 5 weekends.")
          print("# Found", badYears, "years with no month having 5 weekends:")
          print(bY)
        }

```


See also: [http://rosettacode.org/wiki/Five_weekends#UNIX_Shell unix-shell] and [http://rosettacode.org/wiki/Calendar#AWK Calendar].

{{out}}

```txt

# Month with 5 weekends:
   1 : March 1901
   2 : August 1902
   3 : May 1903
   4 : January 1904
   5 : July 1904
   6 : December 1905
...
 196 : July 2095
 197 : March 2097
 198 : August 2098
 199 : May 2099
 200 : January 2100
 201 : October 2100
# Found 201 month with 5 weekends.
# Found 29 years with no month having 5 weekends:
1900 1906 1917 1923 1928 1934 1945 1951 1956 1962
1973 1979 1984 1990 2001 2007 2012 2018 2029 2035
2040 2046 2057 2063 2068 2074 2085 2091 2096

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      INSTALL @lib$+"DATELIB"
      DIM Month$(12)
      Month$() = "","January","February","March","April","May","June", \
      \   "July","August","September","October","November","December"

      num% = 0
      FOR year% = 1900 TO 2100
        PRINT ; year% ": " ;
        oldnum% = num%
        FOR month% = 1 TO 12
          IF FN_dim(month%,year%) = 31 IF FN_dow(FN_mjd(1,month%,year%)) = 5 THEN
            num% += 1
            PRINT Month$(month%), ;
          ENDIF
        NEXT
        IF num% = oldnum% PRINT "(none)" ELSE PRINT
      NEXT year%
      PRINT "Total = " ; num%
```

{{out}}

```txt

1900: (none)
1901: March
1902: August
1903: May
1904: January       July
1905: December
1906: (none)
...
2093: May
2094: January       October
2095: July
2096: (none)
2097: March
2098: August
2099: May
2100: January       October
Total = 201

```



## C


```c
#include <stdio.h>
#include <time.h>

static const char *months[] = {"January", "February", "March", "April", "May",
    "June", "July", "August", "September", "October", "November", "December"};
static int long_months[] = {0, 2, 4, 6, 7, 9, 11};

int main() {
    int n = 0, y, i, m;
    struct tm t = {0};
    printf("Months with five weekends:\n");
    for (y = 1900; y <= 2100; y++) {
        for (i = 0; i < 7; i++) {
            m = long_months[i];
            t.tm_year = y-1900;
	    t.tm_mon = m;
	    t.tm_mday = 1;
            if (mktime(&t) == -1) { /* date not supported */
                printf("Error: %d %s\n", y, months[m]);
                continue;
            }
            if (t.tm_wday == 5) { /* Friday */
                printf("  %d %s\n", y, months[m]);
                n++;
            }
        }
    }
    printf("%d total\n", n);
    return 0;
}
```

{{out}} (note that the C library may not be able to handle all dates; the output may vary across systems):

```txt

Error: 1900 January
Error: 1900 March
Error: 1900 May
Error: 1900 July
Error: 1900 August
Error: 1900 October
Error: 1900 December
Error: 1901 January
Error: 1901 March
Error: 1901 May
Error: 1901 July
Error: 1901 August
Error: 1901 October
Error: 1901 December
  1902 August
  1903 May
  1904 January
  1904 July
  1905 December
...
  2097 March
  2098 August
  2099 May
  2100 January
  2100 October
200 total

```


Not your typical method.  Requires <code>ncal</code>.

```c
#include <stdio.h>
#include <string.h>

int check_month(int y, int m)
{
	char buf[1024], *ptr;
	int bytes, *a = &m;

	sprintf(buf, "ncal -m %d -M %d", m, y);
	FILE *fp = popen(buf, "r");
	if (!fp) return -1;

	bytes = fread(buf, 1, 1024, fp);
	fclose(fp);
	buf[bytes] = 0;

#define check_day(x) \
	ptr = strstr(buf, x);\
	if (5 != sscanf(ptr, x" %d %d %d %d %d", a, a, a, a, a)) return 0

	check_day("Fr");
	check_day("Sa");
	check_day("Su");
	return 1;
}

int main()
{
	int y, m, cnt = 0;
	for (y = 1900; y <= 2100; y++) {
		for (m = 1; m <= 12; m++) {
			if (check_month(y, m) <= 0) continue;
			printf("%d-%02d ", y, m);
			if (++cnt % 16 == 0) printf("\n");
		}
	}
	printf("\nTotal: %d\n", cnt);

	return 0;
}
```

{{out}}

```txt

1901-03 1902-08 1903-05 1904-01 1904-07 1905-12 1907-03 1908-05 1909-01 1909-10 1910-07 1911-12 1912-03 1913-08 1914-05 1915-01
.
.
.
2093-05 2094-01 2094-10 2095-07 2097-03 2098-08 2099-05 2100-01 2100-10
Total: 201

```



## C++

{{libheader|Boost}}

```cpp
#include <vector>
#include <boost/date_time/gregorian/gregorian.hpp>
#include <algorithm>
#include <iostream>
#include <iterator>
using namespace boost::gregorian ;

void print( const date &d ) {
   std::cout << d.year( ) << "-" << d.month( ) << "\n" ;
}

int main( ) {
   greg_month longmonths[ ] = {Jan, Mar , May , Jul ,
      Aug , Oct , Dec } ;
   int monthssize = sizeof ( longmonths ) / sizeof (greg_month ) ;
   typedef std::vector<date> DateVector ;
   DateVector weekendmonster ;
   std::vector<unsigned short> years_without_5we_months ;
   for ( unsigned short i = 1900 ; i < 2101 ; i++ ) {
      bool months_found = false ; //does a given year have 5 weekend months ?
      for ( int j = 0 ; j < monthssize ; j++ ) {
	 date d ( i , longmonths[ j ] , 1 ) ;
	 if ( d.day_of_week( ) == Friday ) {  //for the month to have 5 weekends
	    weekendmonster.push_back( d ) ;
	    if ( months_found == false )
	       months_found = true ;
         }
      }
      if ( months_found == false ) {
	 years_without_5we_months.push_back( i ) ;
      }
   }
   std::cout << "Between 1900 and 2100 , there are " << weekendmonster.size( )
      << " months with 5 complete weekends!\n" ;
   std::cout << "Months with 5 complete weekends are:\n" ;
   std::for_each( weekendmonster.begin( ) , weekendmonster.end( ) , print ) ;
   std::cout <<  years_without_5we_months.size( ) << " years had no months with 5 complete weekends!\n" ;
   std::cout << "These are:\n" ;
   std::copy( years_without_5we_months.begin( ) , years_without_5we_months.end( ) ,
	 std::ostream_iterator<unsigned short>( std::cout , "\n" ) ) ;
   std::cout << std::endl ;
   return 0 ;
}
```

{{out|Sample output}}
<pre style="height:30ex;overflow:scroll">
Between 1900 and 2100 , there are 201 months with 5 complete weekends!
Months with 5 complete weekends are:
1901-Mar
1902-Aug
1903-May
1904-Jan
1904-Jul
1905-Dec
1907-Mar
1908-May
1909-Jan
1909-Oct
1910-Jul
1911-Dec
1912-Mar
1913-Aug
1914-May
1915-Jan
1915-Oct
1916-Dec
1918-Mar
1919-Aug
1920-Oct
1921-Jul
1922-Dec
1924-Aug
1925-May
1926-Jan
1926-Oct
1927-Jul
1929-Mar
1930-Aug
1931-May
1932-Jan
1932-Jul
1933-Dec
1935-Mar
1936-May
1937-Jan
1937-Oct
1938-Jul
1939-Dec
1940-Mar
1941-Aug
1942-May
1943-Jan
1943-Oct
1944-Dec
1946-Mar
1947-Aug
1948-Oct
1949-Jul
1950-Dec
1952-Aug
1953-May
1954-Jan
1954-Oct
1955-Jul
1957-Mar
1958-Aug
1959-May
1960-Jan
1960-Jul
1961-Dec
1963-Mar
1964-May
1965-Jan
1965-Oct
1966-Jul
1967-Dec
1968-Mar
1969-Aug
1970-May
1971-Jan
1971-Oct
1972-Dec
1974-Mar
1975-Aug
1976-Oct
1977-Jul
1978-Dec
1980-Aug
1981-May
1982-Jan
1982-Oct
1983-Jul
1985-Mar
1986-Aug
1987-May
1988-Jan
1988-Jul
1989-Dec
1991-Mar
1992-May
1993-Jan
1993-Oct
1994-Jul
1995-Dec
1996-Mar
1997-Aug
1998-May
1999-Jan
1999-Oct
2000-Dec
2002-Mar
2003-Aug
2004-Oct
2005-Jul
2006-Dec
2008-Aug
2009-May
2010-Jan
2010-Oct
2011-Jul
2013-Mar
2014-Aug
2015-May
2016-Jan
2016-Jul
2017-Dec
2019-Mar
2020-May
2021-Jan
2021-Oct
2022-Jul
2023-Dec
2024-Mar
2025-Aug
2026-May
2027-Jan
2027-Oct
2028-Dec
2030-Mar
2031-Aug
2032-Oct
2033-Jul
2034-Dec
2036-Aug
2037-May
2038-Jan
2038-Oct
2039-Jul
2041-Mar
2042-Aug
2043-May
2044-Jan
2044-Jul
2045-Dec
2047-Mar
2048-May
2049-Jan
2049-Oct
2050-Jul
2051-Dec
2052-Mar
2053-Aug
2054-May
2055-Jan
2055-Oct
2056-Dec
2058-Mar
2059-Aug
2060-Oct
2061-Jul
2062-Dec
2064-Aug
2065-May
2066-Jan
2066-Oct
2067-Jul
2069-Mar
2070-Aug
2071-May
2072-Jan
2072-Jul
2073-Dec
2075-Mar
2076-May
2077-Jan
2077-Oct
2078-Jul
2079-Dec
2080-Mar
2081-Aug
2082-May
2083-Jan
2083-Oct
2084-Dec
2086-Mar
2087-Aug
2088-Oct
2089-Jul
2090-Dec
2092-Aug
2093-May
2094-Jan
2094-Oct
2095-Jul
2097-Mar
2098-Aug
2099-May
2100-Jan
2100-Oct
29 years had no months with 5 complete weekends!
These are:
1900
1906
1917
1923
1928
1934
1945
1951
1956
1962
1973
1979
1984
1990
2001
2007
2012
2018
2029
2035
2040
2046
2057
2063
2068
2074
2085
2091
2096

```


=={{header|C sharp|C#}}==

### With iteration


```csharp
using System;

namespace _5_Weekends
{
    class Program
    {
        const int FIRST_YEAR = 1900;
        const int LAST_YEAR = 2100;
        static int[] _31_MONTHS = { 1, 3, 5, 7, 8, 10, 12 };

        static void Main(string[] args)
        {
            int totalNum = 0;
            int totalNo5Weekends = 0;

            for (int year = FIRST_YEAR; year <= LAST_YEAR; year++)
            {
                bool has5Weekends = false;

                foreach (int month in _31_MONTHS)
                {
                    DateTime firstDay = new DateTime(year, month, 1);
                    if (firstDay.DayOfWeek == DayOfWeek.Friday)
                    {
                        totalNum++;
                        has5Weekends = true;
                        Console.WriteLine(firstDay.ToString("yyyy - MMMM"));
                    }
                }

                if (!has5Weekends) totalNo5Weekends++;
            }
            Console.WriteLine("Total 5-weekend months between {0} and {1}: {2}", FIRST_YEAR, LAST_YEAR, totalNum);
            Console.WriteLine("Total number of years with no 5-weekend months {0}", totalNo5Weekends);
        }
    }
}
```

{{out}}

```txt

1901 - March
1902 - August
1903 - May
1904 - January
1904 - July
1905 - December
...
2095 - July
2097 - March
2098 - August
2099 - May
2100 - January
2100 - October
Total 5-weekend months between 1900 and 2100: 201
Total number of years with no 5-weekend months 29

```



### With LINQ


```csharp
using System;
using System.Linq;
using System.Collections.Generic;

public class Program
{
    public static void Main()
    {
        const int startYear = 1900, endYear = 2100;

        var query = (
            from year in startYear.To(endYear)
            from month in 1.To(12)
            where DateTime.DaysInMonth(year, month) == 31
            select new DateTime(year, month, 1) into date
            where date.DayOfWeek == DayOfWeek.Friday
            select date)
            .ToList();

        Console.WriteLine("Count: " + query.Count);
        Console.WriteLine();
        Console.WriteLine("First and last 5:");
        for (int i = 0; i < 5; i++)
            Console.WriteLine(query[i].ToString("MMMM yyyy"));
        Console.WriteLine("...");
        for (int i = query.Count - 5; i < query.Count; i++)
            Console.WriteLine(query[i].ToString("MMMM yyyy"));
        Console.WriteLine();
        Console.WriteLine("Years without 5 weekends:");
        Console.WriteLine(string.Join(" ", startYear.To(endYear).Except(query.Select(dt => dt.Year))));
    }
}

public static class IntExtensions
{
    public static IEnumerable<int> To(this int start, int end) => Enumerable.Range(start, end - start + 1);
}
```

{{out}}

```txt

Count: 201

First and last 5:
March 1901
August 1902
May 1903
January 1904
July 1904
...
March 2097
August 2098
May 2099
January 2100
October 2100

Years without 5 weekends:
1900 1906 1917 1923 1928 1934 1945 1951 1956 1962 1973 1979 1984 1990 2001 2007 2012 2018 2029 2035 2040 2046 2057 2063 2068 2074 2085 2091 2096
```



## Ceylon

<b>module.ceylon</b>

```ceylon

module rosetta.fiveweekends "1.0.0" {
    import ceylon.time "1.2.2";
}

```

<b>run.ceylon</b>

```ceylon

import ceylon.time {
    date,
    Date
}
import ceylon.time.base {
    january,
    december,
    friday,
    Month
}

shared void run() {
    [Date[],Integer[]] result = fiveWeekendsRecursive();

    value fiveWeekendFirstOfMonths = result[0];
    Integer[] yearsWithNoFiveWeekendMonths = result[1];

    print("# five weekend months = ``fiveWeekendFirstOfMonths.size``");
    print("# years without five weekend months = ``yearsWithNoFiveWeekendMonths.size``");
    yearsWithNoFiveWeekendMonths.each(print);
}

[Date[], Integer[]] fiveWeekendsRecursive()
    => fiveWeekendsRecursiveInner{ year = 1900;
                                   month = january;
                                   fiveWeekendFirstOfMonths = [];
                                   yearsWithNoFiveWeekendMonths = []; };

[Date[], Integer[]] fiveWeekendsRecursiveInner(Integer year,
                                               Month month,
                                               Date[] fiveWeekendFirstOfMonths,
                                               Integer[] yearsWithNoFiveWeekendMonths) {
    if (year > 2100) {
        return [fiveWeekendFirstOfMonths,yearsWithNoFiveWeekendMonths];
    }

    Date firstOfMonth = date{ year = year; month = month; day = 1; };

    Boolean isFiveWeekendMonth =
         (month.numberOfDays() == 31 && friday == firstOfMonth.dayOfWeek);

    Boolean hasNoFiveWeekends =
        month == december &&
        ! isFiveWeekendMonth &&
        fiveWeekendFirstOfMonths.filter((date) => date.year == year).size == 0;

    return fiveWeekendsRecursiveInner(if (month == december) then year+1 else year,
                                      if (month == december) then january else month.plusMonths(1),
                                      if (isFiveWeekendMonth)
                                        then fiveWeekendFirstOfMonths.withTrailing(firstOfMonth)
                                        else fiveWeekendFirstOfMonths,
                                      if (hasNoFiveWeekends)
                                        then yearsWithNoFiveWeekendMonths.withTrailing(year)
                                        else yearsWithNoFiveWeekendMonths);
}


```

{{out}}

```txt

# five weekend months = 201
# years without five weekend months = 29
1900
1906
1917
1923
1928
1934
1945
1951
1956
1962
1973
1979
1984
1990
2001
2007
2012
2018
2029
2035
2040
2046
2057
2063
2068
2074
2085
2091
2096


```




## Clojure


```Clojure
(import java.util.GregorianCalendar
	java.text.DateFormatSymbols)

(->> (for [year (range 1900 2101)
	   month [0 2 4 6 7 9 11] ;; 31 day months
	   :let [cal (GregorianCalendar. year month 1)
		 day (.get cal GregorianCalendar/DAY_OF_WEEK)]
	   :when (= day GregorianCalendar/FRIDAY)]
       (println month "-" year))
     count
     (println "Total Months: " ,))
```



## COBOL


```COBOL

       program-id. five-we.
       data division.
       working-storage section.
       1 wk binary.
        2 int-date pic 9(8).
        2 dow pic 9(4).
        2 friday pic 9(4) value 5.
        2 mo-sub pic 9(4).
        2 months-with-5 pic 9(4) value 0.
        2 years-no-5 pic 9(4) value 0.
        2 5-we-flag pic 9(4) value 0.
         88 5-we-true value 1 when false 0.
       1 31-day-mos pic 9(14) value 01030507081012.
       1 31-day-table redefines 31-day-mos.
        2 mo-no occurs 7 pic 99.
       1 cal-date.
        2 yr pic 9(4).
        2 mo pic 9(2).
        2 da pic 9(2) value 1.
       procedure division.
           perform varying yr from 1900 by 1
           until yr > 2100
               set 5-we-true to false
               perform varying mo-sub from 1 by 1
               until mo-sub > 7
                   move mo-no (mo-sub) to mo
                   compute int-date = function
                       integer-of-date (function numval (cal-date))
                   compute dow = function mod
                       ((int-date - 1) 7) + 1
                   if dow = friday
                       perform output-date
                       add 1 to months-with-5
                       set 5-we-true to true
                   end-if
               end-perform
               if not 5-we-true
                   add 1 to years-no-5
               end-if
           end-perform
           perform output-counts
           stop run
           .

       output-counts.
           display "Months with 5 weekends: " months-with-5
           display "Years without 5 weekends: " years-no-5
           .

       output-date.
           display yr "-" mo
           .
       end program five-we.

```

{{out}}

```txt

1901-03
1902-08
1903-05
1904-01
1904-07
. . .
2097-03
2098-08
2099-05
2100-01
2100-10
Months with 5 weekends: 0201
Years without 5 weekends: 0029

```



## CoffeeScript


```coffeescript

startsOnFriday = (month, year) ->
  # 0 is Sunday, 1 is Monday, ... 5 is Friday, 6 is Saturday
  new Date(year, month, 1).getDay() == 5

has31Days = (month, year) ->
  new Date(year, month, 31).getDate() == 31

checkMonths = (year) ->
  month = undefined
  count = 0
  month = 0
  while month < 12
    if startsOnFriday(month, year) and has31Days(month, year)
      count += 1
      console.log year + ' ' + month + ''
    month += 1
  count

fiveWeekends = ->
  startYear = 1900
  endYear = 2100
  year = undefined
  monthTotal = 0
  yearsWithoutFiveWeekends = []
  total = 0
  year = startYear
  while year <= endYear
    monthTotal = checkMonths(year)
    total += monthTotal
    # extra credit
    if monthTotal == 0
      yearsWithoutFiveWeekends.push year
    year += 1
  console.log 'Total number of months: ' + total + ''
  console.log ''
  console.log yearsWithoutFiveWeekends + ''
  console.log 'Years with no five-weekend months: ' + yearsWithoutFiveWeekends.length + ''
  return

fiveWeekends()


```

{{out}}
<lang>
1901 2
1902 7
1903 4
1904 0
1904 6
1905 11
1907 2
1908 4
1909 0
1909 9
1910 6
1911 11
1912 2
1913 7
1914 4
1915 0
1915 9
1916 11
1918 2
1919 7
1920 9
1921 6
1922 11
1924 7
..

Total number of months: 201
1900,1906,1917,1923,1928,1934,1945,1951,1956,1962,1973,1979,1984,1990,2001,2007,2012,2018,2029,2035,2040,2046,2057,2063,2068,2074,2085,2091,2096

Years with no five-weekend months: 29

```





## Common Lisp


```lisp
;; Given a date, get the day of the week.  Adapted from
;; http://lispcookbook.github.io/cl-cookbook/dates_and_times.html

(defun day-of-week (day month year)
  (nth-value
   6
   (decode-universal-time
	(encode-universal-time 0 0 0 day month year 0)
	0)))

(defparameter *long-months* '(1 3 5 7 8 10 12))

(defun sundayp (day month year)
  (= (day-of-week day month year) 6))

(defun ends-on-sunday-p (month year)
  (sundayp 31 month year))

;; We use the "long month that ends on Sunday" rule.
(defun has-five-weekends-p (month year)
  (and (member month *long-months*)
	   (ends-on-sunday-p month year)))

;; For the extra credit problem.
(defun has-at-least-one-five-weekend-month-p (year)
  (let ((flag nil))
	(loop for month in *long-months* do
		 (if (has-five-weekends-p month year)
			 (setf flag t)))
	flag))

(defun solve-it ()
  (let ((good-months '())
		(bad-years 0))
	(loop for year from 1900 to 2100 do
	   ;; First form in the PROGN is for the extra credit.
		 (progn (unless (has-at-least-one-five-weekend-month-p year)
				  (incf bad-years))
				(loop for month in *long-months* do
					 (when (has-five-weekends-p month year)
					   (push (list month year) good-months)))))
	(let ((len (length good-months)))
	  (format t "~A months have five weekends.~%" len)
	  (format t "First 5 months: ~A~%" (subseq good-months (- len 5) len))
	  (format t "Last 5 months: ~A~%" (subseq good-months 0 5))
	  (format t "Years without a five-weekend month: ~A~%" bad-years))))

```


{{out}}

```txt
201 months have five weekends.
First 5 months: ((7 1904) (1 1904) (5 1903) (8 1902) (3 1901))
Last 5 months: ((10 2100) (1 2100) (5 2099) (8 2098) (3 2097))
Years without a five-weekend month: 29
```



## D


```d
import std.stdio, std.datetime, std.algorithm, std.range;

Date[] m5w(in Date start, in Date end) pure /*nothrow*/ {
    typeof(return) res;
    // adjust to 1st day
    for (Date when = Date(start.year, start.month, 1);
         when < end;
         when.add!"months"(1))
        // Such month must have 3+4*7 days and start at friday
        // for 5 FULL weekends.
        if (when.daysInMonth == 31 &&
            when.dayOfWeek == DayOfWeek.fri)
            res ~= when;
    return res;
}

bool noM5wByYear(in int year) pure {
    return m5w(Date(year, 1, 1), Date(year, 12, 31)).empty;
}

void main() {
    immutable m = m5w(Date(1900, 1, 1), Date(2100, 12, 31));
    writeln("There are ", m.length,
            " months of which the first and last five are:");
    foreach (d; m[0 .. 5] ~ m[$ - 5 .. $])
        writeln(d.toSimpleString()[0 .. $ - 3]);

    immutable n = iota(1900, 2101).filter!noM5wByYear().walkLength();
    writefln("\nThere are %d years in the range that do not have " ~
             "months with five weekends.", n);
}
```

{{out}}

```txt
There are 201 months of which the first and last five are:
1901-Mar
1902-Aug
1903-May
1904-Jan
1904-Jul
2097-Mar
2098-Aug
2099-May
2100-Jan
2100-Oct

There are 29 years in the range that do not have months with five weekends.
```


### Simpler Version


```d
void main() {
    import std.stdio, std.datetime, std.traits;

    enum first_year = 1900;
    enum last_year = 2100;

    uint totalNo5Weekends;
    immutable(Date)[] fiveWeekendMonths;
    foreach (immutable year; first_year .. last_year + 1) {
        bool has5Weekends = false;

        foreach (immutable month; EnumMembers!Month) {
            immutable firstDay = Date(year, month, 1);
            if (firstDay.daysInMonth == 31 &&
                firstDay.dayOfWeek == DayOfWeek.fri) {
                has5Weekends = true;
                fiveWeekendMonths ~= firstDay;
            }
        }

        if (!has5Weekends)
            totalNo5Weekends++;
    }

    writefln("Total 5-weekend months between %d and %d: %d",
             first_year, last_year, fiveWeekendMonths.length);
    foreach (immutable date; fiveWeekendMonths[0 .. 5])
        writeln(date.month, ' ', date.year);
    "...".writeln;
    foreach (immutable date; fiveWeekendMonths[$ - 5 .. $])
        writeln(date.month, ' ', date.year);

    writeln("\nTotal number of years with no 5-weekend months: ",
            totalNo5Weekends);
}
```

{{out}}

```txt
Total 5-weekend months between 1900 and 2100: 201
mar 1901
aug 1902
may 1903
jan 1904
jul 1904
...
mar 2097
aug 2098
may 2099
jan 2100
oct 2100

Total number of years with no 5-weekend months: 29
```



## Delphi


```delphi
program FiveWeekends;

{$APPTYPE CONSOLE}

uses SysUtils, DateUtils;

var
  lMonth, lYear: Integer;
  lDate: TDateTime;
  lFiveWeekendCount: Integer;
  lYearsWithout: Integer;
  lFiveWeekendFound: Boolean;
begin
  for lYear := 1900 to 2100 do
  begin
    lFiveWeekendFound := False;
    for lMonth := 1 to 12 do
    begin
      lDate := EncodeDate(lYear, lMonth, 1);
      if (DaysInMonth(lDate) = 31) and (DayOfTheWeek(lDate) = DayFriday) then
      begin
        Writeln(FormatDateTime('mmm yyyy', lDate));
        Inc(lFiveWeekendCount);
        lFiveWeekendFound := True;
      end;
    end;
    if not lFiveWeekendFound then
      Inc(lYearsWithout);
  end;

  Writeln;
  Writeln(Format('Months with 5 weekends: %d', [lFiveWeekendCount]));
  Writeln(Format('Years with no 5 weekend months: %d', [lYearsWithout]));
end.
```



## Elixir


```elixir
defmodule Date do
  @months { "January", "February", "March",     "April",   "May",      "June",
            "July",    "August",   "September", "October", "November", "December" }

  def five_weekends(year) do
    for m <-[1,3,5,7,8,10,12], :calendar.day_of_the_week(year, m, 31) == 7, do: elem(@months, m-1)
  end
end

months = Enum.map(1900..2100, fn year -> {year, Date.five_weekends(year)} end)
{none, months5} = Enum.partition(months, fn {_,m} -> Enum.empty?(m) end)
count = Enum.reduce(months5, 0, fn {year, months}, acc ->
  IO.puts "#{year} : #{Enum.join(months, ", ")}"
  acc + length(months)
end)
IO.puts "Found #{count} month with 5 weekends."
IO.puts "\nFound #{length(none)} years with no month having 5 weekends:"
IO.puts "#{inspect Enum.map(none, fn {y,_}-> y end)}"
```


{{out}}

```txt

1901 : March
1902 : August
1903 : May
1904 : January, July
1905 : December
...
2095 : July
2097 : March
2098 : August
2099 : May
2100 : January, October
Found 201 month with 5 weekends.

Found 29 years with no month having 5 weekends:
[1900, 1906, 1917, 1923, 1928, 1934, 1945, 1951, 1956, 1962, 1973, 1979, 1984, 1990, 2001, 2007, 2012, 2018, 2029, 2035, 2040, 2046, 2057, 2063, 2068, 2074, 2085, 2091, 2096]

```



## Erlang

Two examples, both based on first building a nested list-of-lists like [Year1, Year2, ..., YearN], where each year is a sublist of year and month tuples, like [{YearN, 1}, {YearN, 2}, ..., {YearN, 12}].

First, a pretty compact example intended for use with <b>escript</b>:

```erlang

#!/usr/bin/env escript
%%
%% Calculate number of months with five weekends between years 1900-2100
%%
main(_) ->
  Years = [ [{Y,M} || M <- lists:seq(1,12)] || Y <- lists:seq(1900,2100) ],
  {CountedYears, {Has5W, TotM5W}} = lists:mapfoldl(
    fun(Months, {Has5W, Tot}) ->
      WithFive = [M || M <- Months, has_five(M)],
      CountM5W = length(WithFive),
      {{Months,CountM5W}, {Has5W++WithFive, Tot+CountM5W}}
    end, {[], 0}, Years),
  io:format("There are ~p months with five full weekends.~n"
            "Showing top and bottom 5:~n",
    [TotM5W]),
  lists:map(fun({Y,M}) -> io:format("~p-~p~n", [Y,M]) end,
    lists:sublist(Has5W,1,5) ++ lists:nthtail(TotM5W-5, Has5W)),
  No5W = [Y || {[{Y,_M}|_], 0} <- CountedYears],
  io:format("The following ~p years do NOT have any five-weekend months:~n",
    [length(No5W)]),
  lists:map(fun(Y) -> io:format("~p~n", [Y]) end, No5W).

has_five({Year, Month}) ->
  has_five({Year, Month}, calendar:last_day_of_the_month(Year, Month)).

has_five({Year, Month}, Days) when Days =:= 31 ->
  calendar:day_of_the_week({Year, Month, 1}) =:= 5;
has_five({_Year, _Month}, _DaysNot31) ->
  false.

```

Second, a more verbose Erlang module:

```erlang

-module(five_weekends).

-export([report/0, print_5w_month/1, print_year_with_no_5w_month/1]).

report() ->
  Years = make_nested_period_list(1900, 2100),
  {CountedYears, {All5WMonths, CountOf5WMonths}} = lists:mapfoldl(
    fun(SingleYearSublist, {All5WMonths, CountOf5WMonths}) ->
      MonthsWith5W = [Month || Month <- SingleYearSublist, if_has_5w(Month)],
      CountOf5WMonthsFor1Year = length(MonthsWith5W),
      { % Result of map for this year sublist:
        {SingleYearSublist,CountOf5WMonthsFor1Year},
        % Accumulate total result for our fold:
        {All5WMonths ++ MonthsWith5W, CountOf5WMonths + CountOf5WMonthsFor1Year}
      }
    end, {[], 0}, Years),
  io:format("There are ~p months with five full weekends.~n"
            "Showing top and bottom 5:~n",
    [CountOf5WMonths]),
  lists:map(fun print_5w_month/1, take_nth_first_and_last(5, All5WMonths)),
  YearsWithout5WMonths = find_years_without_5w_months(CountedYears),
  io:format("The following ~p years do NOT have any five-weekend months:~n",
            [length(YearsWithout5WMonths)]),
  lists:map(fun print_year_with_no_5w_month/1, YearsWithout5WMonths).

make_nested_period_list(FromYear, ToYear) ->
  [ make_monthtuple_sublist_for_year(Year) || Year <- lists:seq(FromYear, ToYear) ].

make_monthtuple_sublist_for_year(Year) ->
  [ {Year, Month} || Month <- lists:seq(1,12) ].

if_has_5w({Year, Month}) ->
  if_has_5w({Year, Month}, calendar:last_day_of_the_month(Year, Month)).

if_has_5w({Year, Month}, Days) when Days =:= 31 ->
  calendar:day_of_the_week({Year, Month, 1}) =:= 5;
if_has_5w({_Year, _Month}, _DaysNot31) ->
  false.

print_5w_month({Year, Month}) ->
  io:format("~p-~p~n", [Year, Month]).

print_year_with_no_5w_month(Year) ->
  io:format("~p~n", [Year]).

take_nth_first_and_last(N, List) ->
  Len = length(List),
  lists:sublist(List, 1, N) ++ lists:nthtail(Len - N, List).

find_years_without_5w_months(List) ->
  [Y || {[{Y,_M}|_], 0} <- List].

```

{{out}}

```txt

There are 201 months with five full weekends.
Showing top and bottom 5:
1901-3
1902-8
1903-5
1904-1
1904-7
2097-3
2098-8
2099-5
2100-1
2100-10
The following 29 years do NOT have any five-weekend months:
1900
1906
1917
1923
1928
1934
1945
1951
1956
1962
1973
1979
1984
1990
2001
2007
2012
2018
2029
2035
2040
2046
2057
2063
2068
2074
2085
2091
2096

```



## ERRE


```ERRE

PROGRAM FIVE_WEEKENDS

DIM M$[12]

PROCEDURE MODULO(X,Y->MD)
  IF Y=0 THEN
     MD=X
   ELSE
     MD=X-Y*INT(X/Y)
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
  M$[]=("","JANUARY","FEBRUARY","MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER")
  PRINT(CHR$(12);) ! CLS
  FOR YEAR=1900 TO 2100 DO
       FOREACH MONTH IN (1,3,5,7,8,10,12) DO   ! months with 31 days
           WD(MONTH,1,YEAR->RES%)
           IF RES%=6 THEN  ! day #6 is Friday
              PRINT(YEAR;": ";M$[MONTH])
              CNT%=CNT%+1
!             IF CNT% MOD 20=0 THEN GET(K$) END IF  ! press a key for next page
            END IF
       END FOR
  END FOR
  PRINT("Total =";CNT%)
END PROGRAM

```

{{out}}

```txt

1901: MARCH
1902: AUGUST
1903: MAY
1904: JANUARY
1904: JULY
1905: DECEMBER
...
2093: MAY
2094: JANUARY
2094: OCTOBER
2095: JULY
2097: MARCH
2098: AUGUST
2099: MAY
2100: JANUARY
2100: OCTOBER
Total = 201
```



## Euphoria


```Euphoria

--Five Weekend task from Rosetta Code wiki
--User:Lnettnay

include std/datetime.e

atom numbermonths = 0
sequence longmonths = {1, 3, 5, 7, 8, 10, 12}
sequence yearsmonths = {}
atom none = 0
datetime dt

for year = 1900 to 2100 do
	atom flag = 0
	for month = 1 to length(longmonths) do
		dt = new(year, longmonths[month], 1)
		if weeks_day(dt) = 6 then --Friday is day 6
			flag = 1
			numbermonths += 1
			yearsmonths = append(yearsmonths, {year, longmonths[month]})
		end if
	end for

	if flag = 0 then
		none += 1
	end if
end for

puts(1, "Number of months with five full weekends from 1900 to 2100 = ")
? numbermonths

puts(1, "First five and last five years, months\n")

for count = 1 to 5 do
	? yearsmonths[count]
end for

for count = length(yearsmonths) - 4 to length(yearsmonths) do
	? yearsmonths[count]
end for

puts(1, "Number of years that have no months with five full weekends = ")
? none

```

{{out}}

```txt

Number of months with five full weekends from 1900 to 2100 = 201
First five and last five years, months
{1901,3}
{1902,8}
{1903,5}
{1904,1}
{1904,7}
{2097,3}
{2098,8}
{2099,5}
{2100,1}
{2100,10}
Number of years that have no months with five full weekends = 29

```


=={{header|F_Sharp|F#}}==

```fsharp
open System

[<EntryPoint>]
let main argv =
    let (yearFrom, yearTo) = (1900, 2100)

    let monthsWith5We year =
        [1; 3; 5; 7; 8; 10; 12] |>
        List.filter (fun month -> DateTime(year, month, 1).DayOfWeek = DayOfWeek.Friday)

    let ym5we =
        [yearFrom .. yearTo]
        |> List.map (fun year -> year, (monthsWith5We year))

    let countMonthsWith5We =
        ym5we
        |> List.sumBy (snd >> List.length)

    let countYearsWithout5WeMonths =
        ym5we
        |> List.sumBy (snd >> List.isEmpty >> (function|true->1|_->0))

    let allMonthsWith5we =
        ym5we
        |> List.filter (snd >> List.isEmpty >> not)

    printfn "%d months in the range of years from %d to %d have 5 weekends."
        countMonthsWith5We yearFrom yearTo
    printfn "%d years in the range of years from %d to %d have no month with 5 weekends."
        countYearsWithout5WeMonths yearFrom yearTo
    printfn "Months with 5 weekends: %A ... %A"
        (List.take 5 allMonthsWith5we)
        (List.take 5 (List.skip ((List.length allMonthsWith5we) - 5) allMonthsWith5we))
    0
```

{{out}}

```txt
201 months in the range of years from 1900 to 2100 have 5 weekends.
29 years in the range of years from 1900 to 2100 have no month with 5 weekends.
Months with 5 weekends: [(1901, [3]); (1902, [8]); (1903, [5]); (1904, [1; 7]); (1905, [12])] ... [(2095, [7]); (2097, [3]); (2098, [8]); (2099, [5]); (2100, [1; 10])]
```



## Factor


```factor
USING: calendar calendar.format formatting fry io kernel math
sequences ;
IN: rosetta-code.five-weekends

: timestamps>my ( months -- )
   [ { MONTH bl YYYY nl } formatted 2drop ] each ;

: month-range ( start-year #months -- seq )
    '[ _ <year> _ <iota> ] call [ months time+ ] with map ;

: find-five-weekend-months ( months -- months' )
    [ [ friday? ] [ days-in-month ] bi 31 = and ] filter ;

1900 12 201 * month-range find-five-weekend-months
[ length "%d five-weekend months found.\n" printf ]
[ 5 head  timestamps>my "..." print               ]
[ 5 tail* timestamps>my                           ] tri
```

{{out}}

```txt

201 five-weekend months found.
Mar 1901
Aug 1902
May 1903
Jan 1904
Jul 1904
...
Mar 2097
Aug 2098
May 2099
Jan 2100
Oct 2100

```



## Fortran

{{works with|Fortran|95 and later}}

Using Zeller's congruence

```fortran
program Five_weekends
  implicit none

  integer :: m, year, nfives = 0, not5 = 0
  logical :: no5weekend

  type month
    integer :: n
    character(3) :: name
  end type month

  type(month) :: month31(7)

  month31(1) = month(13, "Jan")
  month31(2) = month(3,  "Mar")
  month31(3) = month(5,  "May")
  month31(4) = month(7,  "Jul")
  month31(5) = month(8,  "Aug")
  month31(6) = month(10, "Oct")
  month31(7) = month(12, "Dec")

  do year = 1900, 2100
    no5weekend = .true.
    do m = 1, size(month31)
      if(month31(m)%n == 13) then
        if(Day_of_week(1, month31(m)%n, year-1) == 6) then
          write(*, "(a3, i5)") month31(m)%name, year
          nfives = nfives + 1
          no5weekend = .false.
        end if
      else
        if(Day_of_week(1, month31(m)%n, year) == 6) then
          write(*,"(a3, i5)") month31(m)%name, year
          nfives = nfives + 1
          no5weekend = .false.
        end if
      end if
    end do
    if(no5weekend) not5 = not5 + 1
  end do

  write(*, "(a, i0)") "Number of months with five weekends between 1900 and 2100 = ", nfives
  write(*, "(a, i0)") "Number of years between 1900 and 2100 with no five weekend months = ", not5

contains

function Day_of_week(d, m, y)
  integer :: Day_of_week
  integer, intent(in) :: d, m, y
  integer :: j, k

  j = y / 100
  k = mod(y, 100)
  Day_of_week = mod(d + (m+1)*26/10 + k + k/4 + j/4 + 5*j, 7)

end function Day_of_week
end program Five_weekends
```

{{out}}

```txt
Mar 1901
Aug 1902
May 1903
Jan 1904
Jul 1904
...
Mar 2097
Aug 2098
May 2099
Jan 2100
Oct 2100
Number of months with five weekends between 1900 and 2100 = 201
Number of years between 1900 and 2100 with no five weekend months = 29
```


## FreeBASIC


```FreeBASIC
' version 23-06-2015
' compile with: fbc -s console

Function wd(m As Integer, d As Integer, y As Integer) As Integer
    ' Zellerish
    ' 0 = Sunday, 1 = Monday, 2 = Tuesday, 3 = Wednesday
    ' 4 = Thursday, 5 = Friday, 6 = Saturday

    If m < 3 Then        ' If m = 1 Or m = 2 Then
        m += 12
        y -= 1
    End If
    Return (y + (y \ 4) - (y \ 100) + (y \ 400) + d + ((153 * m + 8) \ 5)) Mod 7
End Function

' ------=< MAIN >=------
' only months with 31 day can have five weekends
' these months are: January, March, May, July, August, October, December
' in nr: 1, 3, 5, 7, 8, 10, 12
' the 1e day needs to be on a friday (= 5)

Dim As String month_names(1 To 12) => {"January","February","March",_
                               "April","May","June","July","August",_
                        "September","October","November","December"}
Dim As Integer m, yr, total, i, j, yr_without(200)
Dim As String answer

For yr = 1900 To 2100  ' Gregorian calendar
    answer = ""
    For m = 1 To 12 Step 2
        If m = 9 Then m = 8
        If wd(m , 1 , yr) = 5 Then
            answer = answer + month_names(m) + ", "
            total = total + 1
        End If
    Next
    If answer <> "" Then
        Print Using "#### | "; yr;
        Print Left(answer, Len(answer) -2) ' get rid of extra " ,"
    Else
        i = i + 1
        yr_without(i) = yr
    End If
Next

Print
Print "nr of month for 1900 to 2100 that has five weekends";total
Print
Print i;" years don't have months with five weekends"

For j = 1 To i
    Print yr_without(j); " ";
    If j Mod 8 = 0 Then Print
Next
Print

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
1901 | March
1902 | August
1903 | May
1904 | January, July
1905 | December
...
2095 | July
2097 | March
2098 | August
2099 | May
2100 | January, October

nr of month from 1900 to 2100 that has five weekends 201

 29 years don't have months with five weekends
 1900  1906  1917  1923  1928  1934  1945  1951
 1956  1962  1973  1979  1984  1990  2001  2007
 2012  2018  2029  2035  2040  2046  2057  2063
 2068  2074  2085  2091  2096
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=fbeb1b7a06fff3da3f8b262ee5d71390 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim aMonth As Short[] = [1, 3, 5, 7, 8, 10, 12]                       'All 31 day months
Dim aMMStore As New String[]                                          'To store results
Dim siYear, siMonth, siCount As Short                                 'Various variables
Dim dDay As Date                                                      'To store the day to check
Dim sTemp As String                                                   'Temp string

For siYear = 1900 To 2100                                             'Loop through each year
  For siMonth = 0 To 6                                                'Loop through each 31 day month
    dDay = Date(siYear, aMonth[siMonth], 1)                           'Get the date of the 1st of the month
    If WeekDay(dDay) = 5 Then aMMStore.Add(Format(dDay, "mmmm yyyy")) 'If the 1st is a Friday then store the result
  Next
Next

For Each sTemp In aMMStore                                            'For each item in the stored array..
  Inc siCount                                                         'Increase siCount
  If siCount < 6 Then Print aMMStore[siCount]                         'If 1 of the 1st 5 dates then print it
  If siCount = 6 Then Print String$(14, "-")                          'Print a separator
  If siCount > aMMStore.Max - 4 Then Print aMMStore[siCount - 1]      'If 1 of the last 5 dates then print it
Next

Print gb.NewLine & "Total months = " & Str(siCount)                   'Print the number of months found

siCount = 0                                                           'Reset siCount
sTemp = aMMStore.Join(",")                                            'Put all the stored dates in one string joined by commas
aMMStore.Clear                                                        'Clear the store for reuse

For siYear = 1900 To 2100                                             'Loop through each year
   If Not InStr(sTemp, Str(siYear)) Then                              'If the year is not in the stored string then..
    Inc siCount                                                       'Increase siCount (Amount of years that don't have 5 weekend months)
    aMMStore.Add(Str(siYear))                                         'Add to the store
   End If
Next

Print gb.NewLine & "There are " & Str(siCount) &
  " years that do not have at least one five-weekend month"           'Print the amount of years with no 5 weekend months
Print aMMStore.Join(",")                                              'Print the years with no 5 weekend months
End
```

Output:

```txt

August 1902
May 1903
January 1904
July 1904
December 1905
--------------
March 2097
August 2098
May 2099
January 2100
October 2100

Total months = 201

There are 29 years that do not have at least one five-weekend month
1900,1906,1917,1923,1928,1934,1945,1951,1956,1962,1973,1979,1984,1990,2001,2007,2012,2018,2029,2035,2040,2046,2057,2063,2068,2074,2085,2091,2096

```



## GAP


```gap
# return a list of two lists :
# first is the list of months with five weekends between years y1 and y2 (included)
# second is the list of years without such months, in the same interval
FiveWeekends := function(y1, y2)
  local L, yL, badL, d, m, y;
  L := [ ];
  badL := [ ];
  for y in [y1 .. y2] do
    yL := [ ];
    for m in [1, 3, 5, 7, 8, 10, 12] do
      if WeekDay([1, m, y]) = "Fri" then
        d := StringDate([1, m, y]);
        Add(yL, d{[4 .. 11]});
      fi;
    od;
    if Length(yL) = 0 then
      Add(badL, y);
    else
      Append(L, yL);
    fi;
  od;
  return [ L, badL ];
end;

r := FiveWeekends(1900, 2100);;
n := Length(r[1]);
# 201
Length(r[2]);
# 29
r[1]{[1 .. 5]};
# [ "Mar-1901", "Aug-1902", "May-1903", "Jan-1904", "Jul-1904" ]
r[1]{[n-4 .. n]};
# [ "Mar-2097", "Aug-2098", "May-2099", "Jan-2100", "Oct-2100" ]
```


## Go

Using second algorithm suggestion:

```go
package main

import (
    "fmt"
    "time"
)

func main() {
    var n int                                 // for task item 2
    var first, last time.Time                 // for task item 3
    haveNone := make([]int, 0, 29)            // for extra credit
    fmt.Println("Months with five weekends:") // for task item 1
    for year := 1900; year <= 2100; year++ {
        var hasOne bool // for extra credit
        for _, month := range []time.Month{1, 3, 5, 7, 8, 10, 12} {
            t := time.Date(year, month, 1, 0, 0, 0, 0, time.UTC)
            if t.Weekday() == time.Friday {
                // task item 1:  show month
                fmt.Println("  ", t.Format("2006 January"))
                n++
                hasOne = true
                last = t
                if first.IsZero() {
                    first = t
                }
            }
        }
        if !hasOne {
            haveNone = append(haveNone, year)
        }
    }
    fmt.Println(n, "total\n") // task item 2: number of months
    // task item 3
    fmt.Println("First five dates of weekends:")
    for i := 0; i < 5; i++ {
        fmt.Println("  ", first.Format("Monday, January 2, 2006"))
        first = first.Add(7 * 24 * time.Hour)
    }
    fmt.Println("Last five dates of weekends:")
    for i := 0; i < 5; i++ {
        fmt.Println("  ", last.Format("Monday, January 2, 2006"))
        last = last.Add(7 * 24 * time.Hour)
    }
    // extra credit
    fmt.Println("\nYears with no months with five weekends:")
    for _, y := range haveNone {
        fmt.Println("  ", y)
    }
    fmt.Println(len(haveNone), "total")
}
```

{{out}}

```txt

Months with five weekends:
   1901 March
   1902 August
   1903 May
   1904 January
   1904 July
...
   2097 March
   2098 August
   2099 May
   2100 January
   2100 October
201 total

First five dates of weekends:
   Friday, March 1, 1901
   Friday, March 8, 1901
   Friday, March 15, 1901
   Friday, March 22, 1901
   Friday, March 29, 1901
Last five dates of weekends:
   Friday, October 1, 2100
   Friday, October 8, 2100
   Friday, October 15, 2100
   Friday, October 22, 2100
   Friday, October 29, 2100

Years with no months with five weekends:
   1900
   1906
   1917
   1923
   1928
....
   2068
   2074
   2085
   2091
   2096
29 total

```


## Groovy

Solution:

```groovy
enum Day {
    Sun, Mon, Tue, Wed, Thu, Fri, Sat
    static Day valueOf(Date d) { Day.valueOf(d.format('EEE')) }
}

def date = Date.&parse.curry('yyyy-M-dd')
def isLongMonth = { firstDay -> (firstDay + 31).format('dd') == '01'}

def fiveWeekends = { years ->
    years.collect { year ->
        (1..12).collect { month ->
            date("${year}-${month}-01")
        }.findAll { firstDay ->
            isLongMonth(firstDay) && Day.valueOf(firstDay) == Day.Fri
        }
    }.flatten()
}
```


Test:

```groovy
def ym = { it.format('yyyy-MM') }
def years = 1900..2100
def fiveWeekendMonths = fiveWeekends(years)
println "Number of five weekend months: ${fiveWeekendMonths.size()}"
fiveWeekendMonths.each { println (ym(it)) }
```


{{out}}
<pre style="height:30ex;overflow:scroll;">Number of five weekend months: 201
1901-03
1902-08
1903-05
1904-01
1904-07
1905-12
1907-03
1908-05
1909-01
1909-10
1910-07
1911-12
1912-03
1913-08
1914-05
1915-01
1915-10
1916-12
1918-03
1919-08
1920-10
1921-07
1922-12
1924-08
1925-05
1926-01
1926-10
1927-07
1929-03
1930-08
1931-05
1932-01
1932-07
1933-12
1935-03
1936-05
1937-01
1937-10
1938-07
1939-12
1940-03
1941-08
1942-05
1943-01
1943-10
1944-12
1946-03
1947-08
1948-10
1949-07
1950-12
1952-08
1953-05
1954-01
1954-10
1955-07
1957-03
1958-08
1959-05
1960-01
1960-07
1961-12
1963-03
1964-05
1965-01
1965-10
1966-07
1967-12
1968-03
1969-08
1970-05
1971-01
1971-10
1972-12
1974-03
1975-08
1976-10
1977-07
1978-12
1980-08
1981-05
1982-01
1982-10
1983-07
1985-03
1986-08
1987-05
1988-01
1988-07
1989-12
1991-03
1992-05
1993-01
1993-10
1994-07
1995-12
1996-03
1997-08
1998-05
1999-01
1999-10
2000-12
2002-03
2003-08
2004-10
2005-07
2006-12
2008-08
2009-05
2010-01
2010-10
2011-07
2013-03
2014-08
2015-05
2016-01
2016-07
2017-12
2019-03
2020-05
2021-01
2021-10
2022-07
2023-12
2024-03
2025-08
2026-05
2027-01
2027-10
2028-12
2030-03
2031-08
2032-10
2033-07
2034-12
2036-08
2037-05
2038-01
2038-10
2039-07
2041-03
2042-08
2043-05
2044-01
2044-07
2045-12
2047-03
2048-05
2049-01
2049-10
2050-07
2051-12
2052-03
2053-08
2054-05
2055-01
2055-10
2056-12
2058-03
2059-08
2060-10
2061-07
2062-12
2064-08
2065-05
2066-01
2066-10
2067-07
2069-03
2070-08
2071-05
2072-01
2072-07
2073-12
2075-03
2076-05
2077-01
2077-10
2078-07
2079-12
2080-03
2081-08
2082-05
2083-01
2083-10
2084-12
2086-03
2087-08
2088-10
2089-07
2090-12
2092-08
2093-05
2094-01
2094-10
2095-07
2097-03
2098-08
2099-05
2100-01
2100-10
```


'''Extra Credit:'''

Test:

```groovy
def yearsWith = fiveWeekendMonths.collect { it.format('yyyy') as int } as Set
def yearsWithout = (years as Set) - yearsWith
println "\nNumber of years without a five weekend month: ${yearsWithout.size()}"
yearsWithout.each { println it }
```


{{out}}
<pre style="height:30ex;overflow:scroll;">Number of years without a five weekend month: 29
1900
1906
1917
1923
1928
1934
1945
1951
1956
1962
1973
1979
1984
1990
2001
2007
2012
2018
2029
2035
2040
2046
2057
2063
2068
2074
2085
2091
2096
```




## Harbour


```visualfoxpro

PROCEDURE Main()
   LOCAL y, m, d, nFound, cNames, nTot := 0, nNotFives := 0
   LOCAL aFounds := {}

   SET DATE ANSI

   FOR y := 1900 TO 2100
      nFound := 0 ; cNames := ""
      FOR m := 1 TO 12
      d := CtoD( hb_NtoS( y ) +"/" + hb_NtoS( m ) + "/1" )
         IF CDoW( d ) == "Friday"
	    IF DaysInMonth( m ) == 31
	       nFound++
	       cNames += CMonth( d ) + " "
	    ENDIF
         ENDIF
      NEXT
      IF nFound > 0
         AAdd( aFounds, hb_NtoS( y ) + " : " + hb_NtoS( nFound ) + " ( " + Rtrim( cNames ) + " )" )
         nTot += nFound
      ELSE
         nNotFives++
      ENDIF
   NEXT
   ? "Total months with five weekends: " + hb_NtoS( nTot )
   ? "(see bellow the first and last five years/months with five weekends)"
   ?
   AEval( aFounds, { | e, n | Iif( n < 6, Qout( e ), NIL ) } )
   Qout("...")
   AEval( aFounds, { | e, n | Iif( n > Len(aFounds)-5, Qout( e ), NIL ) } )
   ?
   ? "Years with no five weekends months: " + hb_NtoS( nNotFives )

   RETURN
```


{{out}}

```txt

Total months with five weekends: 201
(see bellow the first and last five years/months with five weekends)

1901 : 1 ( March )
1902 : 1 ( August )
1903 : 1 ( May )
1904 : 2 ( January July )
1905 : 1 ( December )
...
2095 : 1 ( July )
2097 : 1 ( March )
2098 : 1 ( August )
2099 : 1 ( May )
2100 : 2 ( January October )

Years with no five weekends months: 29
```



## Haskell


### =Without using date libraries=

Not using any helper libraries, this code profits from Haskell's lazy evaluation.
Knowing that the first day of 1900 was a Monday, we make an infinit list of days and split it in years and months.
To do this, we take and drop days from the head of the list of days.

year 1900 = [Monday, Tuesday, Wednesday...] (365 items)
year 1904 = [Friday, Saturday, Sunday...] (366 items -- leap year)

and also:

year 1900 = [(January, [Monday, Tuesday..]), (February, [Thursday, Friday..]), ...]

Now it is easy to get all months with 31 days that start on Friday.

```Haskell
import Data.List (intercalate)

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday |
    Saturday | Sunday
    deriving (Eq, Show)

-- the whole thing bases upon an infinite list of weeks

daysFrom1_1_1900 :: [DayOfWeek]
daysFrom1_1_1900 = concat $ repeat [Monday, Tuesday, Wednesday,
    Thursday, Friday, Saturday, Sunday]

data Month = January | February | March | April | May | June | July |
    August | September | October | November | December
    deriving (Show)

type Year = Int
type YearCalendar = (Year, [DayOfWeek])
type MonthlyCalendar = (Year, [(Month, [DayOfWeek])])

-- makes groups of 365 or 366 days for each year (infinite list)

yearsFrom :: [DayOfWeek] -> Year -> [YearCalendar]
yearsFrom s i = (i, yeardays) : yearsFrom rest (i + 1)
    where
        yeardays = take (leapOrNot i) s
        yearlen  = length yeardays
        rest    = drop yearlen s
        leapOrNot n = if isLeapYear n then 366 else 365

yearsFrom1900 :: [YearCalendar]
yearsFrom1900 = yearsFrom daysFrom1_1_1900 1900

-- makes groups of days for each month of the year

months :: YearCalendar -> MonthlyCalendar
months (y, d) = (y, [(January, january), (February, february),
    (March, march), (April, april), (May, may), (June, june),
    (July, july), (August, august), (September, september),
    (October, october), (November, november), (December, december)])
    where
        leapOrNot = if isLeapYear y then 29 else 28
        january = take 31 d
        february = take leapOrNot $ drop 31 d
        march = take 31 $ drop (31 + leapOrNot) d
        april = take 30 $ drop (62 + leapOrNot) d
        may = take 31 $ drop (92 + leapOrNot) d
        june = take 30 $ drop (123 + leapOrNot) d
        july = take 31 $ drop (153 + leapOrNot) d
        august = take 31 $ drop (184 + leapOrNot) d
        september = take 30 $ drop (215 + leapOrNot) d
        october = take 31 $ drop (245 + leapOrNot) d
        november = take 30 $ drop (276 + leapOrNot) d
        december = take 31 $ drop (306 + leapOrNot) d

-- see if a year is a leap year

isLeapYear n
    | n `mod` 100 == 0 = n `mod` 400 == 0
    | otherwise = n `mod` 4 == 0

-- make a list of the months of a year that have 5 weekends
-- (they must have 31 days and the first day must be Friday)
-- if the year doesn't contain any 5-weekended months, then
-- return the year and an empty list

whichFiveWeekends :: MonthlyCalendar -> (Year, [Month])
whichFiveWeekends (y, ms) = (y, map (\(m, _) -> m) found) -- extract the months & leave out their days
    where   found = filter (\(m, a@(d:ds)) -> and [length a == 31,
                d == Friday]) ms

-- take all days from 1900 until 2100, grouping them by years, then by
-- months, and calculating whether they have any 5-weekended months
-- or not

calendar :: [MonthlyCalendar]
calendar = map months $ yearsFrom1900

fiveWeekends1900To2100 :: [(Year, [Month])]
fiveWeekends1900To2100 = takeWhile (\(y, _) -> y <= 2100) $
    map whichFiveWeekends calendar

main = do
    -- count the number of years with 5 weekends
    let answer1 = foldl (\c (_, m) -> c + length m) 0 fiveWeekends1900To2100
    -- take only the years with 5-weekended months
        answer2 = filter (\(_, m) -> not $ null m) fiveWeekends1900To2100
    -- take only the years without 5-weekended months
        answer30 = filter (\(_, m) -> null m) fiveWeekends1900To2100
    -- count how many years without 5-weekended months there are
        answer31 = length answer30
    -- show the years without 5-weekended months
        answer32 = intercalate ", " $ map (\(y, m) -> show y) answer30
    putStrLn $ "There are " ++ show answer1 ++ " months with 5 weekends between 1900 and 2100."
    putStrLn "\nThe first ones are:"
    mapM_ (putStrLn . formatMonth) $ take 5 $ answer2
    putStrLn "\nThe last ones are:"
    mapM_ (putStrLn . formatMonth) $ reverse $ take 5 $ reverse answer2
    putStrLn $ "\n" ++ show answer31 ++ " years don't have at least one five-weekened month"
    putStrLn "\nThose are:"
    putStrLn answer32

formatMonth :: (Year, [Month]) -> String
formatMonth (y, m) = show y ++ ": " ++ intercalate ", " [ show x | x <- m ]
```

{{out}}

```txt

There are 201 months with 5 weekends between 1900 and 2100.

The first ones are:
1901: March
1902: August
1903: May
1904: January, July
1905: December

The last ones are:
2095: July
2097: March
2098: August
2099: May
2100: January, October

29 years don't have at least one five-weekended month

Those are:
1900, 1906, 1917, 1923, 1928, 1934, 1945, 1951, 1956, 1962, 1973, 1979, 1984, 1990, 2001, 2007, 2012, 2018, 2029, 2035, 2040, 2046, 2057, 2063, 2068, 2074, 2085, 2091, 2096

```



### =Using Data.Time=


```haskell
import Data.Time (Day, fromGregorian, gregorianMonthLength)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.List.Split (chunksOf)
import Data.List (intercalate)


-- MONTHS WITH FIVE WEEKENDS --------------------------------------------------
fiveFridayMonths :: Integer -> [(Integer, Int)]
fiveFridayMonths y =
  [1 .. 12] >>=
  \m ->
     [ (y, m)
     | isFriday (fromGregorian y m 1)
     , gregorianMonthLength y m == 31 ]

isFriday :: Day -> Bool
isFriday d =
  let (_, _, day) = toWeekDate d
  in day == 5


-- TEST -----------------------------------------------------------------------
main :: IO ()
main = do
  let years = [1900 .. 2100]
      xs = fiveFridayMonths <$> years
      lean =
        concat $
        zipWith
          (\months year ->
              [ year
              | null months ])
          xs
          years
      n = (length . concat) xs
  (putStrLn . intercalate "\n\n")
    [ "How many five-weekend months 1900-2100 ?"
    , '\t' : show n
    , "First five ?"
    , '\t' : show (concat (take 5 xs))
    , "Last five ?"
    , '\t' : show (concat (drop (n - 5) xs))
    , "How many lean years ? (No five-weekend months)"
    , '\t' : show (length lean)
    , "Which years are lean ?"
    , unlines (('\t' :) <$> fmap (unwords . fmap show) (chunksOf 5 lean))
    ]
```

{{Out}}

```txt
How many five-weekend months 1900-2100 ?

    201

First five ?

    [(1901,3),(1902,8),(1903,5),(1904,1),(1904,7)]

Last five ?

    [(2097,3),(2098,8),(2099,5),(2100,10),(2100,1)]

How many lean years ? (No five-weekend months)

    29

Which years are lean ?

    1900 1906 1917 1923 1928
    1934 1945 1951 1956 1962
    1973 1979 1984 1990 2001
    2007 2012 2018 2029 2035
    2040 2046 2057 2063 2068
    2074 2085 2091 2096
```



## Inform 7

Inform 7 has no built-in date functions, so this solution implements date types and a day-of-week function.


```inform7
Calendar is a room.

When play begins:
	let happy month count be 0;
	let sad year count be 0;
	repeat with Y running from Y1900 to Y2100:
		if Y is a sad year, increment the sad year count;
		repeat with M running through months:
			if M of Y is a happy month:
				say "[M] [year number of Y].";
				increment the happy month count;
	say "Found [happy month count] month[s] with five weekends and [sad year count] year[s] with no such months.";
	end the story.

Section - Years

A year is a kind of value. Y1 specifies a year.

To decide which number is year number of (Y - year):
	decide on Y / Y1.

To decide if (N - number) is divisible by (M - number):
	decide on whether or not the remainder after dividing N by M is zero.

Definition: a year (called Y) is a leap year:
	let YN be the year number of Y;
	if YN is divisible by 400, yes;
	if YN is divisible by 100, no;
	if YN is divisible by 4, yes;
	no.

Section - Months

A month is a kind of value. The months are defined by the Table of Months.

Table of Months
month		month number
January		1
February	2
March		3
April		4
May		5
June		6
July		7
August		8
September	9
October		10
November	11
December	12

A month has a number called length. The length of a month is usually 31.
September, April, June, and November have length 30. February has length 28.

To decide which number is number of days in (M - month) of (Y - year):
	let L be the length of M;
	if M is February and Y is a leap year, decide on L + 1;
	otherwise decide on L.

Section - Weekdays

A weekday is a kind of value. The weekdays are defined by the Table of Weekdays.

Table of Weekdays
weekday		weekday number
Saturday	0
Sunday		1
Monday		2
Tuesday		3
Wednesday	4
Thursday	5
Friday		6

To decide which weekday is weekday of the/-- (N - number) of (M - month) of (Y - year):
	let MN be the month number of M;
	let YN be the year number of Y;
	if MN is less than 3:
		increase MN by 12;
		decrease YN by 1;
	let h be given by Zeller's Congruence;
	let WDN be the remainder after dividing h by 7;
	decide on the weekday corresponding to a weekday number of WDN in the Table of Weekdays.

Equation - Zeller's Congruence
	h = N + ((MN + 1)*26)/10 + YN + YN/4 + 6*(YN/100) + YN/400
where h is a number, N is a number, MN is a number, and YN is a number.

To decide which number is number of (W - weekday) days in (M - month) of (Y - year):
	let count be 0;
	repeat with N running from 1 to the number of days in M of Y:
		if W is the weekday of the N of M of Y, increment count;
	decide on count.

Section - Happy Months and Sad Years

To decide if (M - month) of (Y - year) is a happy month:
	if the number of days in M of Y is 31 and the weekday of the 1st of M of Y is Friday, decide yes;
	decide no.

To decide if (Y - year) is a sad year:
	repeat with M running through months:
		if M of Y is a happy month, decide no;
	decide yes.
```


{{out}}

```txt
March 1901.
August 1902.
May 1903.
January 1904.
July 1904.
...
March 2097.
August 2098.
May 2099.
January 2100.
October 2100.
Found 201 months with five weekends and 29 years with no such months.
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
link datetime,printf

procedure main(A)  # five weekends
   printf(  "There are %d months from %d to %d with five full weekends.\n",
            *(L := fiveweekends(s := 1900, f := 2100)), s,f)
   printf("The first and last five such months are:\n")
   every printf("%s\n",L[1 to 5]|"..."|L[-4 to 0])
   printf(  "There are %d years without such months as follows:\n",
            *(M := Bonus(s,f,L)))
   every printf("%s\n",!M)
end

procedure fiveweekends(start,finish)
   L := []        # months years with five weekends FRI-SUN
   every year := start to finish & month := 1 to 12 do
      if month = (2|4|6|9|11) then next
      else if julian(month,1,year) % 7 = 4 then
         put(L,sprintf("%d-%d-1",year,month))
   return L
end

procedure Bonus(start,finish,fwe)
every insert(Y := set(), start to finish)
every insert(F := set(), integer(!fwe ? tab(find("-"))))
return sort(Y--F)
end
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides formatting]
[http://www.cs.arizona.edu/icon/library/src/procs/datetime.icn datetime.icn provides julian]

{{out}}

```txt
There are 201 months from 1900 to 2100 with five full weekends.
The first and last five such months are:
1901-3-1
1902-8-1
1903-5-1
1904-1-1
1904-7-1
...
2098-8-1
2099-5-1
2100-1-1
2100-10-1
There are 29 years without such months as follows:
1900
1906
1917
1923
1928
1934
1945
1951
1956
1962
1973
1979
1984
1990
2001
2007
2012
2018
2029
2035
2040
2046
2057
2063
2068
2074
2085
2091
2096
```




## J


```j
require 'types/datetime numeric'
find5wkdMonths=: verb define
  years=. range 2{. y
  months=. 1 3 5 7 8 10 12
  m5w=. (#~ 0 = weekday) >,{years;months;31   NB. 5 full weekends iff 31st is Sunday(0)
  >'MMM YYYY' fmtDate toDayNo m5w
)
```

'''Usage:'''

```j
   # find5wkdMonths 1900 2100                        NB. number of months found
201
   (5&{. , '...' , _5&{.) find5wkdMonths 1900 2100   NB. First and last 5 months found
Mar 1901
Aug 1902
May 1903
Jan 1904
Jul 1904
...
Mar 2097
Aug 2098
May 2099
Jan 2100
Oct 2100
   # (range -. {:"1@(_ ". find5wkdMonths)) 1900 2100   NB. number of years without 5 weekend months
29
```



## Java

In Java 1.5+ you can add <code>import static java.util.Calendar.*;</code> to the list of imports and remove all other occurrences of <code>Calendar.</code> from the rest of the code (e.g. <code>Calendar.FRIDAY</code> would simply become <code>FRIDAY</code>). It's more portable (and probably more clear) to leave the <code>Calendar.</code>'s in.

```java
import java.util.Calendar;
import java.util.GregorianCalendar;

public class FiveFSS {
    private static boolean[] years = new boolean[201];
    private static int[] month31 = {Calendar.JANUARY, Calendar.MARCH, Calendar.MAY,
        Calendar.JULY, Calendar.AUGUST, Calendar.OCTOBER, Calendar.DECEMBER};

    public static void main(String[] args) {
        StringBuilder months = new StringBuilder();
        int numMonths = 0;
        for (int year = 1900; year <= 2100; year++) {
            for (int month : month31) {
                Calendar date = new GregorianCalendar(year, month, 1);
                if (date.get(Calendar.DAY_OF_WEEK) == Calendar.FRIDAY) {
                    years[year - 1900] = true;
                    numMonths++;
                    //months are 0-indexed in Calendar
                    months.append((date.get(Calendar.MONTH) + 1) + "-" + year +"\n");
                }
            }
        }
        System.out.println("There are "+numMonths+" months with five weekends from 1900 through 2100:");
        System.out.println(months);
        System.out.println("Years with no five-weekend months:");
        for (int year = 1900; year <= 2100; year++) {
            if(!years[year - 1900]){
                System.out.println(year);
            }
        }
    }
}
```

{{out}} (middle results cut out):
<pre style="height:30ex;overflow:scroll"> There are 201 months with five weekends from 1900 through 2100:
3-1901
8-1902
5-1903
1-1904
7-1904
12-1905
3-1907
5-1908
1-1909
10-1909
7-1910
...
12-2090
8-2092
5-2093
1-2094
10-2094
7-2095
3-2097
8-2098
5-2099
1-2100
10-2100

Years with no five-weekend months:
1900
1906
1917
1923
1928
1934
1945
1951
1956
1962
1973
1979
1984
1990
2001
2007
2012
2018
2029
2035
2040
2046
2057
2063
2068
2074
2085
2091
2096
```




## JavaScript


### ES5


### =Imperative=


```javascript
function startsOnFriday(month, year)
{
 // 0 is Sunday, 1 is Monday, ... 5 is Friday, 6 is Saturday
 return new Date(year, month, 1).getDay() === 5;
}
function has31Days(month, year)
{
 return new Date(year, month, 31).getDate() === 31;
}
function checkMonths(year)
{
 var month, count = 0;
 for (month = 0; month < 12; month += 1)
 {
  if (startsOnFriday(month, year) && has31Days(month, year))
  {
   count += 1;
   document.write(year + ' ' + month + '
');
  }
 }
 return count;
}
function fiveWeekends()
{
 var
  startYear = 1900,
  endYear = 2100,
  year,
  monthTotal = 0,
  yearsWithoutFiveWeekends = [],
  total = 0;
 for (year = startYear; year <= endYear; year += 1)
 {
  monthTotal = checkMonths(year);
  total += monthTotal;
  // extra credit
  if (monthTotal === 0)
   yearsWithoutFiveWeekends.push(year);
 }
 document.write('Total number of months: ' + total + '
');
 document.write('
');
 document.write(yearsWithoutFiveWeekends + '
');
 document.write('Years with no five-weekend months: ' + yearsWithoutFiveWeekends.length + '
');
}
fiveWeekends();
```


{{out|Sample output}}

```txt
1901 2
1902 7
1903 4
1904 0
1904 6
...
2097 2
2098 7
2099 4
2100 0
2100 9
Total number of months: 201

1900,1906,1917,1923,1928,1934,1945,1951,1956,1962,1973,1979,1984,1990,2001,2007,2012,2018,2029,2035,2040,2046,2057,2063,2068,2074,2085,2091,2096
Years with no five-weekend months: 29
```



Here is an alternative solution that uses the offset between the first day of every month, generating the same solution but without relying on the Date object.

```javascript
var Months = [
  'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
  'Jul', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec'
];

var leap = 0,
  // Relative offsets between first day of each month
  offset = [3,0,3,2,3,2,3,3,2,3,2,3],

  // Months that contain 31 days
  longMonths = [1,3,5,7,8,10,12],

  startYear = 1900,
  year = startYear,
  endYear = 2100,

  // Jan 1, 1900 starts on a Monday
  day = 1,

  totalPerYear = 0,
  total = 0,
  without = 0;

for (; year < endYear + 1; year++) {
  leap = totalPerYear = 0;

  if (year % 4 === 0) {
    if (year % 100 === 0) {
      if (year % 400 === 0) {
        leap = 1;
      }
    } else {
      leap = 1;
    }
  }

  for (var i = 0; i < offset.length; i++) {
    for (var j = 0; day === 5 && j < longMonths.length; j++) {
      if (i + 1 === longMonths[j]) {
        console.log(year + '-' + Months[i]);
        totalPerYear++;
        total++;
        break;
      }
    }

    // February -- if leap year, then +1 day
    if (i == 1) {
      day = (day + leap) % 7;
    } else {
      day = (day + offset[i]) % 7;
    }
  }

  if (totalPerYear === 0) {
    without++;
  }
}

console.log('Number of months that have five full weekends from 1900 to 2100: ' + total);
console.log('Number of years without any five full weekend months: ' + without);
```

{{out}}

```txt
1901-Mar
1902-Aug
1903-May
1904-Jan
1904-Jul
...
2097-Mar
2098-Aug
2099-May
2100-Jan
2100-Oct
Number of months that have five full weekends from 1900 to 2100: 201
Number of years without any five full weekend months: 29
```



### =Functional=


```JavaScript
(function () {
    'use strict';

    // longMonthsStartingFriday :: Int -> Int
    function longMonthsStartingFriday(y) {
        return [0, 2, 4, 6, 7, 9, 11]
            .filter(function (m) {
                return (new Date(Date.UTC(y, m, 1)))
                    .getDay() === 5;
            });
    }

    // range :: Int -> Int -> [Int]
    function range(m, n) {
        return Array.apply(null, Array(n - m + 1))
            .map(function (x, i) {
                return m + i;
            });
    }

    var lstNames = [
            'January', '', 'March', '', 'May', '',
            'July', 'August', '', 'October', '', 'December'
        ],

        lstYears = range(1900, 2100),

        lstFullMonths = lstYears
        .reduce(function (a, y) {
            var strYear = y.toString();

            return a.concat(
                longMonthsStartingFriday(y)
                .map(function (m) {
                    return strYear + ' ' + lstNames[m];
                })
            );
        }, []),

        lstLeanYears = lstYears
        .filter(function (y) {
            return longMonthsStartingFriday(y)
                .length === 0;
        });

    return JSON.stringify({
            number: lstFullMonths.length,
            firstFive: lstFullMonths.slice(0, 5),
            lastFive: lstFullMonths.slice(-5),
            leanYearCount: lstLeanYears.length
        },
        null, 2
    );
})();
```

{{Out}}

```txt
{
  "number": 201,
  "firstFive": [
    "1901 March",
    "1902 August",
    "1903 May",
    "1904 January",
    "1904 July"
  ],
  "lastFive": [
    "2097 March",
    "2098 August",
    "2099 May",
    "2100 January",
    "2100 October"
  ],
  "leanYearCount": 29
}
```



### ES6


```JavaScript
(() => {
    // longMonthsStartingFriday :: Int -> [Int]
    const longMonthsStartingFriday = y =>
        filter(m => (new Date(Date.UTC(y, m, 1)))
            .getDay() === 5, [0, 2, 4, 6, 7, 9, 11]);

    // Years -> YearMonths
    // fullMonths :: [Int] -> [String]
    const fullMonths = xs =>
        foldl((a, y) => a.concat(
            map(m => `${y.toString()} ${[
                            'January', '', 'March', '', 'May', '',
                            'July', 'August', '', 'October', '', 'December'
                        ][m]}`, longMonthsStartingFriday(y))
        ), [], xs);

    // leanYears :: [Int] -> [Int]
    const leanYears = years =>
        filter(y => longMonthsStartingFriday(y)
            .length === 0, years);

    // GENERIC ----------------------------------------------------------------

    // A list of functions applied to a list of arguments
    // <*> :: [(a -> b)] -> [a] -> [b]
    const ap = (fs, xs) => //
        [].concat.apply([], fs.map(f => //
            [].concat.apply([], xs.map(x => [f(x)]))));

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // filter :: (a -> Bool) -> [a] -> [a]
    const filter = (f, xs) => xs.filter(f);

    // foldl :: (b -> a -> b) -> b -> [a] -> b
    const foldl = (f, a, xs) => xs.reduce(f, a);

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // show :: a -> String
    const show = x => JSON.stringify(x, null, 2);


    // TEST -------------------------------------------------------------------
    const [lstFullMonths, lstLeanYears] = ap(
        [fullMonths, leanYears], [enumFromTo(1900, 2100)]
    );

    return show({
        number: lstFullMonths.length,
        firstFive: lstFullMonths.slice(0, 5),
        lastFive: lstFullMonths.slice(-5),
        leanYearCount: lstLeanYears.length
    });
})();
```

{{Out}}

```txt
{
  "number": 201,
  "firstFive": [
    "1901 March",
    "1902 August",
    "1903 May",
    "1904 January",
    "1904 July"
  ],
  "lastFive": [
    "2097 March",
    "2098 August",
    "2099 May",
    "2100 January",
    "2100 October"
  ],
  "leanYearCount": 29
}
```



## jq

{{ works with|jq|1.4}}

'''Foundations: Zeller's Congruence'''

```jq
 Use Zeller's Congruence to determine the day of the week, given
# year, month and day as integers in the conventional way.
# Emit 0 for Saturday, 1 for Sunday, etc.
#
def day_of_week(year; month; day):
  if month == 1 or month == 2 then
    [month + 12, year - 1]
  else
    [month, year]
  end
  | day + (13*(.[0] + 1)/5|floor)
    +  (.[1]%100)       + ((.[1]%100)/4|floor)
    +  (.[1]/400|floor) - 2*(.[1]/100|floor)
  | . % 7
;
```

'''Nuts and Bolts''':

```jq
def weekday_of_last_day_of_month(year; month):
  def day_before(day): (6+day) % 7;

  if month==12 then day_before( day_of_week(year+1; 1; 1) )
  else day_before( day_of_week( year; month+1; 1 ) )
  end
;

# The only case where the month has 5 weekends is when the last day
# of the month falls on a Sunday and the month has 31 days.
#
def five_weekends(from; to):
  reduce range(from; to) as $year
    ([]; reduce (1,3,5,7,8,10,12) as $month  # months with 31 days
      (.;
       weekday_of_last_day_of_month($year; $month) as $day
       | if $day == 1 then . + [[ $year, $month]] else . end ))
;

# Input [year, month] as conventional integers; print e.g. "Jan 2001"
def pp:
  def month:
    ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"][.-1];
   "\(.[1] | month) \(.[0])"
;
```

'''The Task''':

```jq
five_weekends(1900;2101)
| "There are \(length) months with 5 weekends from 1900 to 2100 inclusive;",
  "the first and last five are as follows:",
  ( .[0: 5][] | pp),
  "...",
  ( .[length-5: ][] | pp),
  "In this period, there are \( [range(1900;2101)] - map( .[0] ) | length ) years which have no five-weekend months."
```

{{out}}

```sh
$ jq -r -n -f Five_Weekends.jq
There are 201 months with 5 weekends from 1900 to 2100 inclusive;
the first and last five are as follows:
Mar 1901
Aug 1902
May 1903
Jan 1904
Jul 1904
...
Mar 2097
Aug 2098
May 2099
Jan 2100
Oct 2100
In this period, there are 29 years which have no five-weekend months.
```



## Julia

{{works with|Julia|0.6}}


```julia
isweekend(dt::Date) = Dates.dayofweek(dt) ∈ (Dates.Friday, Dates.Saturday, Dates.Sunday)

function hasfiveweekend(month::Integer, year::Integer)
    dmin = Date(year, month, 1)
    dmax = dmin + Dates.Day(Dates.daysinmonth(dmin) - 1)
    return count(isweekend, dmin:dmax) ≥ 15
end

months = collect((y, m) for y in 1900:2100, m in 1:12 if hasfiveweekend(m, y))

println("Number of months with 5 full-weekends: $(length(months))")
println("First five such months:")
for (y, m) in months[1:5] println(" - $y-$m") end
println("Last five such months:")
for (y, m) in months[end-4:end] println(" - $y-$m") end

# extra credit
yrs = getindex.(months, 1)
nyrs = 2100 - 1899 - length(unique(yrs))

println("Number of year with not one 5-full-weekend month: $nyrs")
```


{{out}}

```txt
Number of months with 5 full-weekends: 201
First five such months:
 - 1904-1
 - 1909-1
 - 1915-1
 - 1926-1
 - 1932-1
Last five such months:
 - 2062-12
 - 2073-12
 - 2079-12
 - 2084-12
 - 2090-12
Number of year with not one 5-full-weekend month: 29
```



## k


```k

cal_j:(_jd[19000101]+!(-/_jd 21010101 19000101)) / enumerate the calendar
is_we:(cal_j!7) _lin 4 5 6                       / identify friday saturdays and sundays
m:__dj[cal_j]%100                                / label the months
mi:&15=+/'is_we[=m]                              / group by month and sum the weekend days
`0:,"There are ",($#mi)," months with five weekends"
m5:(?m)[mi]
`0:$5#m5
`0:,"..."
`0:$-5#m5
y:1900+!201                                     / enumerate the years in the range
y5:?_ m5%100                                    / label the years of the months
yn5:y@&~y _lin y5                               / find any years not in the 5 weekend month list
`0:,"There are ",($#yn5)," years without any five-weekend months"
`0:,1_,/",",/:$yn5
```

{{out}}

```txt

There are 201 months with five weekends
190103
190208
190305
190401
190407
...
209703
209808
209905
210001
210010
There are 29 years without any five-weekend months
1900,1906,1917,1923,1928,1934,1945,1951,1956,1962,1973,1979,1984,1990,2001,2007,2012,2018,2029,2035,2040,2046,2057,2063,2068,2074,2085,2091,2096
```



## Kotlin


```scala
// version 1.0.6

import java.util.*

fun main(args: Array<String>) {
    val calendar = GregorianCalendar(1900, 0, 1)
    val months31 = arrayOf(1, 3, 5, 7, 8, 10, 12)
    val monthsWithFive = mutableListOf<String>()
    val yearsWithNone  = mutableListOf<Int>()
    for (year in 1900..2100) {
        var countInYear = 0 //  counts months in a given year with 5 weekends
        for (month in 1..12) {
            if ((month in months31) && (Calendar.FRIDAY == calendar[Calendar.DAY_OF_WEEK])) {
                countInYear++
                monthsWithFive.add("%02d".format(month) + "-" + year)
            }
            calendar.add(Calendar.MONTH, 1)
        }
        if (countInYear == 0) yearsWithNone.add(year)
    }
    println("There are ${monthsWithFive.size} months with 5 weekends")
    println("The first 5 are ${monthsWithFive.take(5)}")
    println("The final 5 are ${monthsWithFive.takeLast(5)}")
    println()
    println("There are ${yearsWithNone.size} years with no months which have 5 weekends, namely:")
    println(yearsWithNone)
}
```


{{out}}

```txt

There are 201 months with 5 weekends
The first 5 are [03-1901, 08-1902, 05-1903, 01-1904, 07-1904]
The final 5 are [03-2097, 08-2098, 05-2099, 01-2100, 10-2100]

There are 29 years with no months which have 5 weekends, namely:
[1900, 1906, 1917, 1923, 1928, 1934, 1945, 1951, 1956, 1962, 1973, 1979, 1984, 1990, 2001, 2007, 2012, 2018, 2029, 2035, 2040, 2046, 2057, 2063, 2068, 2074, 2085, 2091, 2096]

```



## Lasso


```Lasso
local(
	months		= array(1, 3, 5, 7, 8, 10, 12),
	fivemonths	= array,
	emptyears	= array,
	checkdate	= date,
	countyear
)

#checkdate -> day = 1

loop(-from = 1900, -to = 2100) => {

	#countyear = false

	#checkdate -> year = loop_count

	with month in #months
	do {
		#checkdate -> month = #month
		if(#checkdate -> dayofweek == 6) => {
			#countyear = true
			#fivemonths -> insert(#checkdate -> format(`YYYY MMM`))
		}
	}

	if(not #countyear) => {
		#emptyears -> insert(loop_count)
	}

}
local(
	monthcount	= #fivemonths -> size,
	output		= 'Total number of months ' + #monthcount + '<br /> Starting five months '
)

loop(5) => {
	#output -> append(#fivemonths -> get(loop_count) + ', ')
}

#output -> append('<br /> Ending five months ')

loop(-from = #monthcount - 5, -to = #monthcount) => {
	#output -> append(#fivemonths -> get(loop_count) + ', ')
}

#output -> append('<br /> Years with no five weekend months ' + #emptyears -> size + '<br />')

with year in #emptyears do {
	#output -> append(#year + ', ')
}

#output
```

{{out|Result}}

```txt
Total number of months 201
Starting five months 1901 Mar, 1902 Aug, 1903 May, 1903 Jan, 1904 Jul,
Ending five months 2095 Jul, 2097 Mar, 2098 Aug, 2099 May, 2099 Jan, 2100 Oct,
Years with no five weekend months 29
1900, 1906, 1917, 1923, 1928, 1934, 1945, 1951, 1956, 1962, 1973, 1979, 1984, 1990, 2001, 2007, 2012, 2018, 2029, 2035, 2040, 2046, 2057, 2063, 2068, 2074, 2085, 2091, 2096,
```



## Lua


```Lua

local months={"JAN","MAR","MAY","JUL","AUG","OCT","DEC"}
local daysPerMonth={31+28,31+30,31+30,31,31+30,31+30,0}

function find5weMonths(year)
  local list={}
  local startday=((year-1)*365+math.floor((year-1)/4)-math.floor((year-1)/100)+math.floor((year-1)/400))%7

  for i,v in ipairs(daysPerMonth) do
    if startday==4 then list[#list+1]=months[i] end
    if i==1 and year%4==0 and year%100~=0 or year%400==0 then
      startday=startday+1
    end
    startday=(startday+v)%7
  end
  return list
end

local cnt_months=0
local cnt_no5we=0

for y=1900,2100 do
  local list=find5weMonths(y)
  cnt_months=cnt_months+#list
  if #list==0 then
    cnt_no5we=cnt_no5we+1
  end
  print(y.." "..#list..": "..table.concat(list,", "))
end
print("Months with 5 weekends: ",cnt_months)
print("Years without 5 weekends in the same month:",cnt_no5we)

```

{{out}}

```txt
1900 0:
1901 1: MAR
1902 1: AUG
1903 1: MAY
1904 2: JAN, JUL
1905 1: DEC
1906 0:
1907 1: MAR
1908 1: MAY
1909 2: JAN, OCT
...
...
...
2090 1: DEC
2091 0:
2092 1: AUG
2093 1: MAY
2094 2: JAN, OCT
2095 1: JUL
2096 0:
2097 1: MAR
2098 1: AUG
2099 1: MAY
2100 2: JAN, OCT
Months with 5 weekends: 	201
Years without 5 weekends in the same month:	29
```



## Maple


```Maple
five_weekends:= proc()
	local i, month, count;
	#Only months with 31 days can possibly satisfy the condition
	local long_months := [1,3,5,7,8,10,12];
	local months := ["January","February","March","April","May","June","July","August","September","October","November","December"];
	count := 0;
	for i from 1900 to 2100 by 1 do
		for month in long_months do
			if Calendar:-DayOfWeek(Date(i, month, 1)) = 6 then
				printf("%d-%s\n", i, months[month]);
				count++;
			end if;
		end do;
	end do;
	printf("%d months have five full weekends.\n", count);
end proc;
five_weekends();
```

{{Out|Output}}

```txt
1901-March
1902-August
1903-May
1904-January
1904-July
1905-December
1907-March
1908-May
1909-January
1909-October
1910-July
1911-December
1912-March
1913-August
1914-May
1915-January
1915-October
1916-December
1918-March
1919-August
1920-October
1921-July
1922-December
1924-August
1925-May
1926-January
1926-October
1927-July
1929-March
1930-August
1931-May
1932-January
1932-July
1933-December
1935-March
1936-May
1937-January
1937-October
1938-July
1939-December
1940-March
1941-August
1942-May
1943-January
1943-October
1944-December
1946-March
1947-August
1948-October
1949-July
1950-December
1952-August
1953-May
1954-January
1954-October
1955-July
1957-March
1958-August
1959-May
1960-January
1960-July
1961-December
1963-March
1964-May
1965-January
1965-October
1966-July
1967-December
1968-March
1969-August
1970-May
1971-January
1971-October
1972-December
1974-March
1975-August
1976-October
1977-July
1978-December
1980-August
1981-May
1982-January
1982-October
1983-July
1985-March
1986-August
1987-May
1988-January
1988-July
1989-December
1991-March
1992-May
1993-January
1993-October
1994-July
1995-December
1996-March
1997-August
1998-May
1999-January
1999-October
2000-December
2002-March
2003-August
2004-October
2005-July
2006-December
2008-August
2009-May
2010-January
2010-October
2011-July
2013-March
2014-August
2015-May
2016-January
2016-July
2017-December
2019-March
2020-May
2021-January
2021-October
2022-July
2023-December
2024-March
2025-August
2026-May
2027-January
2027-October
2028-December
2030-March
2031-August
2032-October
2033-July
2034-December
2036-August
2037-May
2038-January
2038-October
2039-July
2041-March
2042-August
2043-May
2044-January
2044-July
2045-December
2047-March
2048-May
2049-January
2049-October
2050-July
2051-December
2052-March
2053-August
2054-May
2055-January
2055-October
2056-December
2058-March
2059-August
2060-October
2061-July
2062-December
2064-August
2065-May
2066-January
2066-October
2067-July
2069-March
2070-August
2071-May
2072-January
2072-July
2073-December
2075-March
2076-May
2077-January
2077-October
2078-July
2079-December
2080-March
2081-August
2082-May
2083-January
2083-October
2084-December
2086-March
2087-August
2088-October
2089-July
2090-December
2092-August
2093-May
2094-January
2094-October
2095-July
2097-March
2098-August
2099-May
2100-January
2100-October
201 months have five full weekends.
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
years = {1900, 2100}; months = {1 ,3 ,5 ,7 ,8 ,10 ,12};
result = Select[Tuples[{Range@@years, months}], (DateString[# ~ Join ~ 1, "DayNameShort"] == "Fri")&];

Print[result // Length," months with 5 weekends" ];
Print["First months: ", DateString[#,{"MonthName"," ","Year"}]& /@ result[[1 ;; 5]]];
Print["Last months: " , DateString[#,{"MonthName"," ","Year"}]& /@ result[[-5 ;; All]]];
Print[# // Length, " years without 5 weekend months:\n", #] &@
 Complement[Range @@ years, Part[Transpose@result, 1]];
```

{{out}}

```txt
201 months with 5 weekends
First months: {March 1901, August 1902, May 1903, January 1904, July 1904}
Last months:  {March 2097, August 2098, May 2099, January 2100, October 2100}
29 years without 5 weekend months
{1900, 1906, 1917, 1923, 1928, 1934, 1945, 1951, 1956, 1962, 1973, 1979,
 1984, 1990, 2001, 2007, 2012, 2018, 2029, 2035, 2040, 2046, 2057, 2063,
 2068, 2074, 2085, 2091, 2096}
```


=={{header|MATLAB}} / {{header|Octave}}==


```Matlab
longmonth = [1 3 5 7 8 10 12];

i = 1;

for y = 1900:2100
    for m = 1:numel(longmonth)
        [num,name] = weekday(datenum(y,longmonth(m),1));
        if num == 6
            x(i,:) = datestr(datenum(y,longmonth(m),1),'mmm yyyy'); %#ok<SAGROW>
            i = i+1;
        end
    end
end

fprintf('There are %i months with 5 weekends between 1900 and 2100.\n',length(x))

fprintf('\n The first 5 months are:\n')
for j = 1:5
    fprintf('\t %s \n',x(j,:))
end

fprintf('\n The final 5 months are:\n')
for j = length(x)-4:length(x)
    fprintf('\t %s \n',x(j,:))
end
```

{{out}}

```txt
There are 201 months with 5 weekends between 1900 and 2100.

 The first 5 months are:
	 Mar 1901
	 Aug 1902
	 May 1903
	 Jan 1904
	 Jul 1904

 The final 5 months are:
	 Mar 2097
	 Aug 2098
	 May 2099
	 Jan 2100
	 Oct 2100
```



## Maxima


```maxima
left(a, n) := makelist(a[i], i, 1, n)$
right(a, n) := block([m: length(a)], makelist(a[i], i, m - n + 1, m))$

a: [ ]$
for year from 1900 thru 2100 do
   for month in [1, 3, 5, 7, 8, 10, 12] do
      if weekday(year, month, 1) = 'friday then
         a: endcons([year, month], a)$

length(a);
201

left(a, 5);
[[1901,3],[1902,8],[1903,5],[1904,1],[1904,7]]

right(a, 5);
[[2097,3],[2098,8],[2099,5],[2100,1],[2100,10]]
```



## MUMPS

{{libheader|VA Kernel|22.0}}

```MUMPS

FIVE
 ;List and count the months between 1/1900 and 12/2100 that have 5 full weekends
 ;Extra credit - list and count years with no months with five full weekends
 ;Using the test that the 31st of a month is on a Sunday
 ;Uses the VA's public domain routine %DTC (Part of the Kernel) named here DIDTC
 NEW YEAR,MONTH,X,Y,CNTMON,NOT,NOTLIST
 ; YEAR is the year we're testing
 ; MONTH is the month we're testing
 ; X is the date in "internal" format, as an input to DOW^DIDTC
 ; Y is the day of the week (0=Sunday, 1=Monday...) output from DOW^DIDTC
 ; CNTMON is a count of the months that have 5 full weekends
 ; NOT is a flag if there were no months with 5 full weekends yet that year
 ; NOTLIST is a list of years that do not have any months with 5 full weekends
 SET CNTMON=0,NOTLIST=""
 WRITE !!,"The following months have five full weekends:"
 FOR YEAR=200:1:400 DO ;years since 12/31/1700 epoch
 . SET NOT=0
 . FOR MONTH="01","03","05","07","08","10","12" DO
 . . SET X=YEAR_MONTH_"31"
 . . DO DOW^DIDTC
 . . IF (Y=0) DO
 . . . SET NOT=NOT+1,CNTMON=CNTMON+1
 . . . WRITE !,MONTH_"-"_(YEAR+1700)
 . SET:(NOT=0) NOTLIST=NOTLIST_$SELECT($LENGTH(NOTLIST)>1:",",1:"")_(YEAR+1700)
 WRITE !,"For a total of "_CNTMON_" months."
 WRITE !!,"There are "_$LENGTH(NOTLIST,",")_" years with no five full weekends in any month."
 WRITE !,"They are: "_NOTLIST
 KILL YEAR,MONTH,X,Y,CNTMON,NOT,NOTLIST
 QUIT
F ;Same logic as the main entry point, shortened format
 N R,M,X,Y,C,N,L S C=0,L=""
 W !!,"The following months have five full weekends:"
 F R=200:1:400 D
 . S N=0 F M="01","03","05","07","08","10","12" S X=R_M_"31" D DOW^DIDTC I 'Y S N=N+1,C=C+1 W !,M_"-"_(R+1700)
 . S:'N L=L_$S($L(L):",",1:"")_(R+1700)
 W !,"For a total of "_C_" months.",!!,"There are "_$L(L,",")_" years with no five full weekends in any month.",!,"They are: "_L
 Q
```
Usage:
```txt

USER>d ^FIVE

The following months have five full weekends:
03-1901
08-1902
05-1903
01-1904
07-1904
12-1905
. . . . . .
01-2094
10-2094
07-2095
03-2097
08-2098
05-2099
01-2100
10-2100
For a total of 201 months.

There are 29 years with no five full weekends in any month.
They are: 1900,1906,1917,1923,1928,1934,1945,1951,1956,1962,1973,1979,1984,1990,2001,2007,2012,2018,2029,2035,2040,2046,2057,2063,2068,2074,2085,2091,2096
```



## NetRexx



```netrexx

/* NetRexx ************************************************************
* 30.08.2012 Walter Pachl derived from Rexx version 3
*                         omitting dead code left there
**********************************************************************/
options replace format comments java crossref savelog symbols
Numeric digits 20
nr5fwe=0
years_without_5fwe=0
mnl='Jan Mar May Jul Aug Oct Dec'
ml='1 3 5 7 8 10 12'
Loop j=1900 To 2100
  year_has_5fwe=0
  Loop mi=1 To ml.words()
    m=ml.word(mi)
    jd=greg2jul(j,m,1)
    IF jd//7=4 Then Do                 /* 1st m j is a Friday */
      nr5fwe=nr5fwe+1
      year_has_5fwe=1
      If j<=1905 | 2095<=j Then
        Say mnl.word(mi) j 'has 5 full weekends'
      End
    End
    If j=1905 Then Say '...'
    if year_has_5fwe=0 Then years_without_5fwe=years_without_5fwe+1
  End
Say ' '
Say nr5fwe 'occurrences of 5 full weekends in a month'
Say years_without_5fwe 'years without 5 full weekends'
exit

method greg2jul(yy,mm,d) public static returns Rexx
/***********************************************************************
* Converts a Gregorian date to the corresponding Julian day number
* 19891101 Walter Pachl REXXified algorithm published in CACM
*                (Fliegel & vanFlandern, CACM Vol.11 No.10 October 1968)
***********************************************************************/
  numeric digits 12
/***********************************************************************
* The published formula:
* res=d-32075+1461*(yy+4800+(mm-14)%12)%4+,
*     367*(mm-2-((mm-14)%12)*12)%12-3*((yy+4900+(mm-14)%12)%100)%4
***********************************************************************/
  mma=(mm-14)%12
  yya=yy+4800+mma
  result=d-32075+1461*yya%4+367*(mm-2-mma*12)%12-3*((yya+100)%100)%4
    Return result                   /* return the result              */
```

Output: see Rexx version 3


## NewLISP


```newlisp

#!/usr/local/bin/newlisp

(context 'KR)

(define (Kraitchik year month day)
    ; See https://en.wikipedia.org/wiki/Determination_of_the_day_of_the_week#Kraitchik.27s_variation
    ; Function adapted for specific task (not for general usage).
    (if (or (= 1 month) (= 2 month))
        (dec year)
    )
    ;- - - -
    (setf m-table '(_ 1 4 3 6 1 4 6 2 5 0 3 5)) ; - - - First element of list is dummy!
    (setf m (m-table month))
    ;- - - -
    (setf c-table '(0 5 3 1))
    (setf century%4 (mod (int (slice (string year) 0 2)) 4))
    (setf c (c-table century%4))
    ;- - - -
    (setf yy* (slice (string year) -2))
    (if (= "0" (yy* 0))
        (setf yy* (yy* 1))
    )
    (setf yy (int yy*))
    (setf y  (mod (+ (/ yy 4) yy) 7))
    ;- - - -
    (setf dow-table '(6 0 1 2 3 4 5))
    (dow-table (mod (+ day m c y) 7))
)

(context 'MAIN)

(setf Fives 0)
(setf NotFives 0)
(setf Report '())
(setf months-table '((1 "Jan") (3 "Mar") (5 "May") (7 "Jul") (8 "Aug") (10 "Oct") (12 "Dec")))

(for (y 1900 2100)
    (setf FivesFound 0)
    (setf Names "")
    (dolist (m '(1 3 5 7 8 10 12))
        (setf Dow (KR:Kraitchik y m 1))
        (if (= 5 Dow)
            (begin
                (++ FivesFound)
                (setf Names (string Names " " (lookup m months-table)))
            )
        )
    )

    (if (zero? FivesFound)
        (++ NotFives)
        (begin
            (setf Report (append Report (list (list y FivesFound (string "(" Names " )")))))
            (setf Fives (+ Fives FivesFound))
        )
    )
)


;- - - - Display all report data
;(dolist (x Report)
;    (println (x 0) ": " (x 1) " " (x 2))
;)


;- - - - Display only first five and last five records
(dolist (x (slice Report 0 5))
    (println (x 0) ": " (x 1) " " (x 2))
)
(println "...")
(dolist (x (slice Report -5))
    (println (x 0) ": " (x 1) " " (x 2))
)

(println "\nTotal months with five weekends: " Fives)
(println "Years with no five weekends months: " NotFives)
(exit)

```

{{out}}

```txt

1901: 1 ( Mar )
1902: 1 ( Aug )
1903: 1 ( May )
1904: 2 ( Jan Jul )
1905: 1 ( Dec )
...
2095: 1 ( Jul )
2097: 1 ( Mar )
2098: 1 ( Aug )
2099: 1 ( May )
2100: 2 ( Jan Oct )

Total months with five weekends: 201
Years with no five weekends months: 29

```



## Nim


```nim
import times

const LongMonths = {mJan, mMar, mMay, mJul, mAug, mOct, mDec}

var timeinfo = getLocalTime getTime()
timeinfo.monthday = 1

var sumNone = 0
for year in 1900..2100:
  var none = true
  for month in LongMonths:
    timeinfo.year = year
    timeinfo.month = month
    if getLocalTime(timeInfoToTime timeinfo).weekday == dFri:
      echo month," ",year
      none = false
  if none: inc sumNone
echo "Years without a 5 weekend month: ",sumNone
```

{{out}}

```txt
March 1901
August 1902
May 1903
January 1904
July 1904
December 1905
[...]
March 2097
August 2098
May 2099
January 2100
October 2100
Years without a 5 weekend month: 29
```



## OCaml

{{libheader|OCaml Calendar Library}}


```ocaml
open CalendarLib

let list_first_five = function
  | x1 :: x2 :: x3 :: x4 :: x5 :: _ -> [x1; x2; x3; x4; x5]
  | _ -> invalid_arg "list_first_five"

let () =
  let months = ref [] in
  for year = 1900 to 2100 do
    for month = 1 to 12 do
      let we = ref 0 in
      let num_days = Date.days_in_month (Date.make_year_month year month) in
      for day = 1 to num_days - 2 do
        let d0 = Date.day_of_week (Date.make year month day)
        and d1 = Date.day_of_week (Date.make year month (day + 1))
        and d2 = Date.day_of_week (Date.make year month (day + 2)) in
        if (d0, d1, d2) = (Date.Fri, Date.Sat, Date.Sun) then incr we
      done;
      if !we = 5 then months := (year, month) :: !months
    done;
  done;
  Printf.printf "Number of months with 5 weekends: %d\n" (List.length !months);
  print_endline "First and last months between 1900 and 2100:";
  let print_month (year, month) = Printf.printf "%d-%02d\n" year month in
  List.iter print_month (list_first_five (List.rev !months));
  List.iter print_month (List.rev (list_first_five !months));
;;
```


{{out}}

```txt
$ ocaml unix.cma str.cma -I +calendar calendarLib.cma five_we.ml
Number of months with 5 weekends: 201
First and last months between 1900 and 2100:
1901-03
1902-08
1903-05
1904-01
1904-07
2097-03
2098-08
2099-05
2100-01
2100-10
```




## Oforth



```Oforth
: fiveWeekEnd(y1, y2)
| y m |

   ListBuffer new
   y1 y2 for: y [
      Date.JANUARY Date.DECEMBER for: m [
         Date.DaysInMonth(y, m) 31 ==
         [ y, m, 01 ] asDate dayOfWeek Date.FRIDAY == and
            ifTrue: [ [ y, m ] over add ]
         ]
      ]
   dup size println dup left(5) println right(5) println ;
```


{{out}}

```txt

>fiveWeekEnd(1900, 2100)
201
[[1901, 3], [1902, 8], [1903, 5], [1904, 1], [1904, 7]]
[[2097, 3], [2098, 8], [2099, 5], [2100, 1], [2100, 10]]

```



## PARI/GP


```parigp
fiveWeekends()={
	my(day=6);	\\ 0 = Friday; this represents Thursday for March 1, 1900.
	my(ny=[31,30,31,30,31,31,30,31,30,31,31,28],ly=ny,v,s);
	ly[12]=29;
	for(year=1900,2100,
		v=if((year+1)%4,ny,ly);	\\ Works for 1600 to 2398
		for(month=1,12,
			if(v[month] == 31 && !day,
				if(month<11,
					print(year" "month+2)
				,
					print(year+1" 1")
				);
				s++
			);
			day = (day + v[month])%7
		)
	);
	s
};
```



## Pascal

See [[Five_weekends#Delphi | Delphi]]


## Perl


```perl
#!/usr/bin/perl -w
use DateTime ;

my @happymonths ;
my @workhardyears ;
my @longmonths = ( 1 , 3 , 5 , 7 , 8 , 10 , 12 ) ;
my @years = 1900..2100 ;
foreach my $year ( @years ) {
   my $countmonths = 0 ;
   foreach my $month ( @longmonths ) {
      my $dt = DateTime->new( year => $year ,
	                      month => $month ,
			      day   => 1 ) ;
      if ( $dt->day_of_week == 5 ) {
	 $countmonths++ ;
	 my $yearfound = $dt->year ;
	 my $monthfound = $dt->month_name ;
	 push ( @happymonths , "$yearfound  $monthfound" ) ;
      }
   }
   if ( $countmonths == 0 ) {
      push ( @workhardyears, $year ) ;
   }
}
print "There are " . @happymonths . " months with 5 full weekends!\n" ;
print "The first 5 and the last 5 of them are:\n" ;
foreach my $i ( 0..4 ) {
   print "$happymonths[ $i ]\n" ;
}
foreach my $i ( -5..-1 ) {
   print "$happymonths[ $i ]\n" ;
}
print "No long weekends in the following " . @workhardyears . " years:\n" ;
map { print "$_\n" } @workhardyears ;
```

{{out}}

```txt
There are 201 months with 5 full weekends!
The first 5 and the last 5 of them are:
1901  March
1902  August
1903  May
1904  January
1904  July
2097  March
2098  August
2099  May
2100  January
2100  October
No 5 weekends per month in the following 29 years:
1900
1906
1917
1923
1928
1934
1945
1951
1956
1962
1973
1979
1984
1990
2001
2007
2012
2018
2029
2035
2040
2046
2057
2063
2068
2074
2085
2091
2096

```



## Perl 6

{{works with|Rakudo|2015-09-10}}

```perl6
# A month has 5 weekends iff it has 31 days and starts on Friday.

my @years = 1900 .. 2100;
my @has31 = 1, 3, 5, 7, 8, 10, 12;
my @happy = ($_ when *.day-of-week == 5 for (@years X @has31).map(-> ($y, $m) { Date.new: $y, $m, 1 }));

say 'Happy month count:  ', +@happy;
say 'First happy months: ' ~ @happy[^5];
say 'Last  happy months: ' ~ @happy[*-5 .. *];
say 'Dreary years count: ',  @years - @happy».year.squish;
```


{{out}}

```txt
Happy month count:  201
First happy months: 1901-03-01 1902-08-01 1903-05-01 1904-01-01 1904-07-01
Last  happy months: 2097-03-01 2098-08-01 2099-05-01 2100-01-01 2100-10-01
Dreary years count: 29
```



## Phix


```Phix
sequence m31 = {"January",0,"March",0,"May",0,"July","August",0,"October",0,"December"}
integer y,m,
        nmonths = 0
string months
sequence res = {},
         none = {}

for y=1900 to 2100 do
    months = ""
    for m=1 to 12 do
        if string(m31[m])
        and day_of_week(y,m,1)=6 then
            if length(months)!=0 then months &= ", " end if
            months &= m31[m]
            nmonths += 1
        end if
    end for
    if length(months)=0 then
        none = append(none,y)
    else
        res = append(res,sprintf("%d : %s\n",{y,months}))
    end if
end for

printf(1,"Found %d months with five full weekends\n",nmonths)
res[6..-6] = {" ...\n"}
puts(1,join(res,""))
printf(1,"Found %d years with no month having 5 weekends:\n",{length(none)})
none[6..-6] = {".."}
?none
```

{{out}}

```txt

Found 201 months with five full weekends
1901 : March
1902 : August
1903 : May
1904 : January, July
1905 : December
 ...
2095 : July
2097 : March
2098 : August
2099 : May
2100 : January, October
Found 29 years with no month having 5 weekends:
{1900,1906,1917,1923,1928,"..",2068,2074,2085,2091,2096}

```



## PicoLisp


```PicoLisp
(setq Lst
   (make
      (for Y (range 1900 2100)
         (for M (range 1 12)
            (and
               (date Y M 31)
               (= "Friday" (day (date Y M 1)))
               (link (list (get *Mon M) Y)) ) ) ) ) )

(prinl "There are " (length Lst) " months with five weekends:")
(mapc println (head 5 Lst))
(prinl "...")
(mapc println (tail 5 Lst))
(prinl)
(setq Lst (diff (range 1900 2100) (uniq (mapcar cadr Lst))))
(prinl "There are " (length Lst) " years with no five-weekend months:")
(println Lst)
```

{{out}}

```txt
There are 201 months with five weekends:
(Mar 1901)
(Aug 1902)
(May 1903)
(Jan 1904)
(Jul 1904)
...
(Mar 2097)
(Aug 2098)
(May 2099)
(Jan 2100)
(Oct 2100)

There are 29 years with no five-weekend months:
(1900 1906 1917 1923 1928 1934 1945 1951 1956 1962 1973 1979 1984 1990 2001 2007
2012 2018 2029 2035 2040 2046 2057 2063 2068 2074 2085 2091 2096)
```



## Pike


```Pike
int(0..1) weekends(object day)
{
    return (<5,6,7>)[day->week_day()];
}

int(0..1) has5(object month)
{
    return sizeof(filter(month->days(), weekends))==15;
}

object range = Calendar.Year(1900)->distance(Calendar.Year(2101));
array have5 = filter(range->months(), has5);

write("found %d months:\n%{%s\n%}...\n%{%s\n%}",
       sizeof(have5), have5[..4]->format_nice(), have5[<4..]->format_nice());

array rest = range->years() - have5->year();
write("%d years without any 5 weekend month:\n %{%d,%}\n", sizeof(rest), rest->year_no());
```


{{out}}

```txt

 found 201 months:
 March 1901
 August 1902
 May 1903
 January 1904
 July 1904
 ...
 March 2097
 August 2098
 May 2099
 January 2100
 October 2100

 29 years without any 5 weekend month:
 1900,1906,1917,1923,1928,1934,1945,1951,1956,1962,1973,1979,1984,1990,2001,2007,2012,2018,2029,2035,2040,2046,2057,2063,2068,2074,2085,2091,2096,

```



## PL/I


```PL/I

weekends: procedure options (main); /* 28/11/2011 */
   declare tally fixed initial (0);
   declare (d, dend, dn) fixed (10);
   declare (date_start, date_end) picture '99999999';
   declare Leap fixed (1);

   date_start = '01011900';
   do date_start = date_start to '01012100';
      d        = days(date_start, 'DDMMYYYY');
      date_end = date_start + 30110000;
      dend     = days(date_end,   'DDMMYYYY');
      Leap     = dend-d-364;
      do dn = d, d+59+Leap, d+120+Leap, d+181+Leap, d+212+Leap,
                 d+273+Leap, d+334+Leap;
         if weekday(dn) = 6 then
            do;
               put skip list (daystodate(dn, 'MmmYYYY') || ' has 5 weekends' );
               tally = tally + 1;
            end;
      end;
   end;

   put skip list ('Total number of months having 3-day weekends =', tally);

end weekends;

```

{{out}}

```txt

Mar1901 has 5 weekends
Aug1902 has 5 weekends
May1903 has 5 weekends
Jan1904 has 5 weekends
Jul1904 has 5 weekends
Dec1905 has 5 weekends
       .....
Jan2094 has 5 weekends
Oct2094 has 5 weekends
Jul2095 has 5 weekends
Mar2097 has 5 weekends
Aug2098 has 5 weekends
May2099 has 5 weekends
Jan2100 has 5 weekends
Oct2100 has 5 weekends
Total number of months having 3-day weekends =       201

```


Part 2: Years not having any month of 5 weekends:

```txt

weekends: procedure options (main);
   declare tally fixed initial (0);
   declare (d, dend, dn) fixed (10);
   declare (date_start, date_end) picture '99999999';
   declare Leap fixed (1);
   declare Long_weekend bit (1);

   date_start = '01011900';
   do date_start = date_start to '01012100';
      d        = days(date_start, 'DDMMYYYY');
      date_end = date_start + 30110000;
      dend     = days(date_end,   'DDMMYYYY');
      Leap     = dend-d-364;
      long_weekend = '0'b;
      do dn = d, d+59+Leap, d+120+Leap, d+181+Leap, d+212+Leap,
                 d+273+Leap, d+334+Leap;
         if weekday(dn) = 6 then long_weekend = '1'b;
      end;
      if ^long_weekend then
         put skip list (daystodate(dn, 'YYYY') || ' has no month of 5 weekends');
   end;

end weekends;

```

{{out}}

```txt

1900 has no month of 5 weekends
1906 has no month of 5 weekends
1917 has no month of 5 weekends
1923 has no month of 5 weekends
1928 has no month of 5 weekends
1934 has no month of 5 weekends
1945 has no month of 5 weekends
1951 has no month of 5 weekends
1956 has no month of 5 weekends
1962 has no month of 5 weekends
1973 has no month of 5 weekends
1979 has no month of 5 weekends
1984 has no month of 5 weekends
1990 has no month of 5 weekends
2001 has no month of 5 weekends
2007 has no month of 5 weekends
2012 has no month of 5 weekends
2018 has no month of 5 weekends
2029 has no month of 5 weekends
2035 has no month of 5 weekends
2040 has no month of 5 weekends
2046 has no month of 5 weekends
2057 has no month of 5 weekends
2063 has no month of 5 weekends
2068 has no month of 5 weekends
2074 has no month of 5 weekends
2085 has no month of 5 weekends
2091 has no month of 5 weekends
2096 has no month of 5 weekends

```



## PowerShell



```powershell
$fiveWeekends = @()
$yearsWithout = @()
foreach ($y in 1900..2100) {
  $hasFiveWeekendMonth = $FALSE
  foreach ($m in @("01","03","05","07","08",10,12)) {
    if ((Get-Date "$y-$m-1").DayOfWeek -eq "Friday") {
      $fiveWeekends += "$y-$m"
      $hasFiveWeekendMonth = $TRUE
    }
  }
  if ($hasFiveWeekendMonth -eq $FALSE) {
     $yearsWithout += $y
  }
}
Write-Output "Between the years 1900 and 2100, inclusive, there are $($fiveWeekends.count) months with five full weekends:"
Write-Output "$($fiveWeekends[0..4] -join ","),...,$($fiveWeekends[-5..-1] -join ",")"

Write-Output ""
Write-Output "Extra Credit: these $($yearsWithout.count) years have no such month:"
Write-Output ($yearsWithout -join ",")
```


{{Output}}

```txt
Between the years 1900 and 2100, inclusive, there are 201 months with five full weekends:
1901-03,1902-08,1903-05,1904-01,1904-07,...,2097-03,2098-08,2099-05,2100-01,2100-10

Extra Credit: these 29 years have no such month:
1900,1906,1917,1923,1928,1934,1945,1951,1956,1962,1973,1979,1984,1990,2001,2007,2012,2018,2029,2035,2040,2046,2057,2063
,2068,2074,2085,2091,2096
```



## Prolog

Works with SWI-Prolog;

```Prolog
main() :-
    weekends(1900, 2100, FiveWeekendList, RemainderWeekendList),

    length(FiveWeekendList, FiveLen),
    maplist(write, ["Total five weekend months:", FiveLen, '\n']),

    slice(FiveWeekendList, 5, FirstFiveList),
    maplist(write, ["First five {year,month} pairs:", FirstFiveList, '\n']),

    slice(FiveWeekendList, -5, LastFiveList),
    maplist(write, ["Last five {year,month} pairs:", LastFiveList, '\n']),

    maplist(take_year, FiveWeekendList, FiveYearList),
    list_to_set(FiveYearList, FiveYearSet),

    maplist(take_year, RemainderWeekendList, RemainderYearList),
    list_to_set(RemainderYearList, RemainderYearSet),

    subtract(RemainderYearSet, FiveYearSet, NonFiveWeekendSet),
    length(NonFiveWeekendSet, NonFiveWeekendLen),

    maplist(write, ["Total years with no five weekend months:", NonFiveWeekendLen, '\n']),
    writeln(NonFiveWeekendSet).

weekends(StartYear, EndYear, FiveWeekendList, RemainderWeekendList) :-
    numlist(StartYear, EndYear, YearList),
    numlist(1, 12, MonthList),
    pair(YearList, MonthList, YearMonthList),
    partition(has_five_weekends, YearMonthList, FiveWeekendList, RemainderWeekendList).

has_five_weekends({Year, Month}) :-
    long_month(Month),
    starts_on_a_friday(Year, Month).

starts_on_a_friday(Year, Month) :-
    Date = date(Year, Month, 1),
    day_of_the_week(Date, DayOfTheWeek),
    DayOfTheWeek == 5.

take_year({Year, _}, Year).

long_month(1).
long_month(3).
long_month(5).
long_month(7).
long_month(8).
long_month(10).
long_month(12).

% Helpers

% https://stackoverflow.com/a/7739806
pair(L1, L2, Pairs):-findall({A,B}, (member(A, L1), member(B, L2)), Pairs).

slice(_, 0, []).

slice(List, N, NList):-
    N < 0,
    N1 is abs(N),
    last_n_elements(List, N1, NList).

slice(List, N, NList):-
    N > 0,
    first_n_elements(List, N, NList).

first_n_elements(List, N, FirstN):-
    length(FirstN, N),
    append(FirstN, _, List).

last_n_elements(List, N, LastN) :-
    length(LastN, N),
    append(_, LastN, List).

```

{{out}}

```txt
?- main.
Total five weekend months:201
First five {year,month} pairs:[{1901,3},{1902,8},{1903,5},{1904,1},{1904,7}]
Last five {year,month} pairs:[{2097,3},{2098,8},{2099,5},{2100,1},{2100,10}]
Total years with no five weekend months:29
[1900,1906,1917,1923,1928,1934,1945,1951,1956,1962,1973,1979,1984,1990,2001,2007,2012,2018,2029,2035,2040,2046,2057,2063,2068,2074,2085,2091,2096]
true .
```



## PureBasic


```PureBasic
Procedure DateG(year.w, month.b, day)
  ;Returns the number of days before or after the earliest reference date
  ;in PureBasic's Date Library (1 Jan 1970) based on an assumed Gregorian calendar calculation
  Protected days
  days = (year) * 365 + (month - 1) * 31 + day - 1 - 719527 ;DAYS_UNTIL_1970_01_01 = 719527
  If month >= 3
    days - Int(0.4 * month + 2.3)
  Else
    year - 1
  EndIf
  days + Int(year/4) - Int(year/100) + Int(year/400)

  ProcedureReturn days
EndProcedure

Procedure startsOnFriday(year, month)
  ;0 is Sunday, 1 is Monday, ... 5 is Friday, 6 is Saturday
  Protected referenceDay = DayOfWeek(Date(1970, 1, 1, 0, 0, 0)) ;link to the first day in the PureBasic's date library
  Protected resultDay = (((DateG(year, month, 1) + referenceDay) % 7) + 7) % 7
   If resultDay = 5
    ProcedureReturn #True
  EndIf
EndProcedure

Procedure has31Days(month)
  Select month
    Case 1, 3, 5, 7 To 8, 10, 12
      ProcedureReturn #True
  EndSelect
EndProcedure

Procedure checkMonths(year)
  Protected month, count
  For month = 1 To 12
    If startsOnFriday(year, month) And has31Days(month)
      count + 1
      PrintN(Str(year) + " " + Str(month))
    EndIf
  Next
  ProcedureReturn count
EndProcedure

Procedure fiveWeekends()
  Protected startYear = 1900, endYear = 2100, year, monthTotal, total
  NewList yearsWithoutFiveWeekends()

  For year = startYear To endYear
    monthTotal = checkMonths(year)
    total + monthTotal
    ;extra credit
    If monthTotal = 0
      AddElement(yearsWithoutFiveWeekends())
      yearsWithoutFiveWeekends() = year
    EndIf
  Next

   PrintN("Total number of months: " + Str(total) + #CRLF$)
   PrintN("Years with no five-weekend months: " + Str(ListSize(yearsWithoutFiveWeekends())) )
EndProcedure

If OpenConsole()
  fiveWeekends()

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

{{out|Sample output}}

```txt
1901 3
1902 8
1903 5
1904 1
1904 7
.
.(output clipped)
.
2097 3
2098 8
2099 5
2100 1
2100 10
Total number of months: 201

Years with no five-weekend months: 29
```



## Python


```python
from datetime import timedelta, date

DAY     = timedelta(days=1)
START, STOP = date(1900, 1, 1), date(2101, 1, 1)
WEEKEND = {6, 5, 4}     # Sunday is day 6
FMT     = '%Y %m(%B)'

def fiveweekendspermonth(start=START, stop=STOP):
    'Compute months with five weekends between dates'

    when = start
    lastmonth = weekenddays = 0
    fiveweekends = []
    while when < stop:
        year, mon, _mday, _h, _m, _s, wday, _yday, _isdst = when.timetuple()
        if mon != lastmonth:
            if weekenddays >= 15:
                fiveweekends.append(when - DAY)
            weekenddays = 0
            lastmonth = mon
        if wday in WEEKEND:
            weekenddays += 1
        when += DAY
    return fiveweekends

dates = fiveweekendspermonth()
indent = '  '
print('There are %s months of which the first and last five are:' % len(dates))
print(indent +('\n'+indent).join(d.strftime(FMT) for d in dates[:5]))
print(indent +'...')
print(indent +('\n'+indent).join(d.strftime(FMT) for d in dates[-5:]))

print('\nThere are %i years in the range that do not have months with five weekends'
      % len(set(range(START.year, STOP.year)) - {d.year for d in dates}))
```


'''Alternate Algorithm'''

The condition is equivalent to having a thirty-one day month in which the last day of the month is a Sunday.

```python
LONGMONTHS = (1, 3, 5, 7, 8, 10, 12) # Jan Mar May Jul Aug Oct Dec
def fiveweekendspermonth2(start=START, stop=STOP):
    return [date(yr, month, 31)
            for yr in range(START.year, STOP.year)
            for month in LONGMONTHS
            if date(yr, month, 31).timetuple()[6] == 6 # Sunday
            ]

dates2 = fiveweekendspermonth2()
assert dates2 == dates
```


{{out|Sample output}}

```txt
There are 201 months of which the first and last five are:
  1901 03(March)
  1902 08(August)
  1903 05(May)
  1904 01(January)
  1904 07(July)
  ...
  2097 03(March)
  2098 08(August)
  2099 05(May)
  2100 01(January)
  2100 10(October)

There are 29 years in the range that do not have months with five weekends
```



## R


```rsplus
ms = as.Date(sapply(c(1, 3, 5, 7, 8, 10, 12),
    function(month) paste(1900:2100, month, 1, sep = "-")))
ms = format(sort(ms[weekdays(ms) == "Friday"]), "%b %Y")
message("There are ", length(ms), " months with five weekends.")
message("The first five: ", paste(ms[1:5], collapse = ", "))
message("The last five: ", paste(tail(ms, 5), collapse = ", "))
```



## Racket


```racket

#lang racket
(require srfi/19)

(define long-months '(1 3 5 7 8 10 12))
(define days #(sun mon tue wed thu fri sat))

(define (week-day date)
  (vector-ref days (date-week-day date)))

(define (five-weekends-a-month start end)
  (for*/list ([year (in-range start (+ end 1))]
              [month long-months]
              [date (in-value (make-date 0 0 0 0 31 month year 0))]
              #:when (eq? (week-day date) 'sun))
    date))

(define weekends (five-weekends-a-month 1900 2100))
(define count (length weekends))

(displayln (~a "There are " count " weeks with five weekends."))
(displayln "The first five are: ")
(for ([w (take weekends 5)])
  (displayln (date->string w "~b ~Y")))
(displayln "The last five are: ")
(for ([w (drop weekends (- count 5))])
  (displayln (date->string w "~b ~Y")))

```

{{out}}

```txt

There are 201 weeks with five weekends.
The first five are:
Mar 1901
Aug 1902
May 1903
Jan 1904
Jul 1904
The last five are:
Mar 2097
Aug 2098
May 2099
Jan 2100
Oct 2100

```



## REXX


### version for newer REXXes

This version uses the latest enhancements to the   '''DATE'''   built-in function (which some older REXX interpreters don't support).

This version was written in such a way that it counts the number of days-of-the-week for each month, thereby

generalizing the problem without taking shortcuts specific to the task at hand.

Changing the task's requirements to find how many extended weekends (Fr-Sa-Su ''or'' Sa-Su-Mo) there are for

each month would entail changing only a single   '''if'''   statement.

```rexx
/*REXX program  finds  months  that contain  five weekends   (given a date range).      */
month. =31;  month.2=0                           /*month days;   February is skipped.   */
month.4=30;  month.6=30; month.9=30; month.11=30 /*all the  months with  thirty-days.   */
parse arg yStart yStop .                         /*get the  "start"  and  "stop"  years.*/
if yStart=='' | yStart==","  then yStart= 1900   /*Not specified?  Then use the default.*/
if yStop =='' | yStop ==","  then yStop = 2100   /* "      "         "   "   "     "    */
years=yStop - yStart + 1                         /*calculate the number of yrs in range.*/
haps=0                                           /*number of five weekends happenings.  */
!.=0;                 @5w= 'five-weekend months' /*flag if a year has any five-weekends.*/
       do y=yStart to yStop                      /*process the years specified.         */
           do m=1  for 12;    wd.=0              /*process each month and also each year*/
                 do d=1  for month.m;  dat_= y"-"right(m,2,0)'-'right(d,2,0)
                 parse  upper  value   date('W', dat_, "I")    with    ? 3
                 wd.?=wd.?+1                     /*?  is the first two chars of weekday.*/
                 end   /*d*/                     /*WD.su = number of Sundays in a month.*/
           if wd.su\==5 | wd.fr\==5 | wd.sa\==5 then iterate           /*five weekends ?*/
           say 'There are five weekends in'   y   date('M', dat_, "I")
           haps=haps+1;   !.y=1                  /*bump counter; indicate yr has 5 WE's.*/
           end         /*m*/
       end             /*y*/
say
say "There were "  haps ' occurrence's(haps) "of"  @5w 'in year's(years)  yStart"──►"yStop
say; #=0
          do y=yStart  to yStop;   if !.y  then iterate                    /*skip if OK.*/
          #=#+1;        say  'Year '    y    " doesn't have any" @5wem'.'
          end   /*y*/
say
say  "There are "  # ' year's(#) "that haven't any" @5w 'in year's(years) yStart'──►'yStop
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
s: if arg(1)==1  then return arg(3);      return word(arg(2) 's',1)        /*pluralizer.*/
```

'''output'''   when using the default inputs:
<pre style="height:98ex">
There are five weekends in 1901 March
There are five weekends in 1902 August
There are five weekends in 1903 May
There are five weekends in 1904 January
There are five weekends in 1904 July
There are five weekends in 1905 December
There are five weekends in 1907 March
There are five weekends in 1908 May
There are five weekends in 1909 January
There are five weekends in 1909 October
There are five weekends in 1910 July
There are five weekends in 1911 December
There are five weekends in 1912 March
There are five weekends in 1913 August
There are five weekends in 1914 May
There are five weekends in 1915 January
There are five weekends in 1915 October
There are five weekends in 1916 December
There are five weekends in 1918 March
There are five weekends in 1919 August
There are five weekends in 1920 October
There are five weekends in 1921 July
There are five weekends in 1922 December
There are five weekends in 1924 August
There are five weekends in 1925 May
There are five weekends in 1926 January
There are five weekends in 1926 October
There are five weekends in 1927 July
There are five weekends in 1929 March
There are five weekends in 1930 August
There are five weekends in 1931 May
There are five weekends in 1932 January
There are five weekends in 1932 July
There are five weekends in 1933 December
There are five weekends in 1935 March
There are five weekends in 1936 May
There are five weekends in 1937 January
There are five weekends in 1937 October
There are five weekends in 1938 July
There are five weekends in 1939 December
There are five weekends in 1940 March
There are five weekends in 1941 August
There are five weekends in 1942 May
There are five weekends in 1943 January
There are five weekends in 1943 October
There are five weekends in 1944 December
There are five weekends in 1946 March
There are five weekends in 1947 August
There are five weekends in 1948 October
There are five weekends in 1949 July
There are five weekends in 1950 December
There are five weekends in 1952 August
There are five weekends in 1953 May
There are five weekends in 1954 January
There are five weekends in 1954 October
There are five weekends in 1955 July
There are five weekends in 1957 March
There are five weekends in 1958 August
There are five weekends in 1959 May
There are five weekends in 1960 January
There are five weekends in 1960 July
There are five weekends in 1961 December
There are five weekends in 1963 March
There are five weekends in 1964 May
There are five weekends in 1965 January
There are five weekends in 1965 October
There are five weekends in 1966 July
There are five weekends in 1967 December
There are five weekends in 1968 March
There are five weekends in 1969 August
There are five weekends in 1970 May
There are five weekends in 1971 January
There are five weekends in 1971 October
There are five weekends in 1972 December
There are five weekends in 1974 March
There are five weekends in 1975 August
There are five weekends in 1976 October
There are five weekends in 1977 July
There are five weekends in 1978 December
There are five weekends in 1980 August
There are five weekends in 1981 May
There are five weekends in 1982 January
There are five weekends in 1982 October
There are five weekends in 1983 July
There are five weekends in 1985 March
There are five weekends in 1986 August
There are five weekends in 1987 May
There are five weekends in 1988 January
There are five weekends in 1988 July
There are five weekends in 1989 December
There are five weekends in 1991 March
There are five weekends in 1992 May
There are five weekends in 1993 January
There are five weekends in 1993 October
There are five weekends in 1994 July
There are five weekends in 1995 December
There are five weekends in 1996 March
There are five weekends in 1997 August
There are five weekends in 1998 May
There are five weekends in 1999 January
There are five weekends in 1999 October
There are five weekends in 2000 December
There are five weekends in 2002 March
There are five weekends in 2003 August
There are five weekends in 2004 October
There are five weekends in 2005 July
There are five weekends in 2006 December
There are five weekends in 2008 August
There are five weekends in 2009 May
There are five weekends in 2010 January
There are five weekends in 2010 October
There are five weekends in 2011 July
There are five weekends in 2013 March
There are five weekends in 2014 August
There are five weekends in 2015 May
There are five weekends in 2016 January
There are five weekends in 2016 July
There are five weekends in 2017 December
There are five weekends in 2019 March
There are five weekends in 2020 May
There are five weekends in 2021 January
There are five weekends in 2021 October
There are five weekends in 2022 July
There are five weekends in 2023 December
There are five weekends in 2024 March
There are five weekends in 2025 August
There are five weekends in 2026 May
There are five weekends in 2027 January
There are five weekends in 2027 October
There are five weekends in 2028 December
There are five weekends in 2030 March
There are five weekends in 2031 August
There are five weekends in 2032 October
There are five weekends in 2033 July
There are five weekends in 2034 December
There are five weekends in 2036 August
There are five weekends in 2037 May
There are five weekends in 2038 January
There are five weekends in 2038 October
There are five weekends in 2039 July
There are five weekends in 2041 March
There are five weekends in 2042 August
There are five weekends in 2043 May
There are five weekends in 2044 January
There are five weekends in 2044 July
There are five weekends in 2045 December
There are five weekends in 2047 March
There are five weekends in 2048 May
There are five weekends in 2049 January
There are five weekends in 2049 October
There are five weekends in 2050 July
There are five weekends in 2051 December
There are five weekends in 2052 March
There are five weekends in 2053 August
There are five weekends in 2054 May
There are five weekends in 2055 January
There are five weekends in 2055 October
There are five weekends in 2056 December
There are five weekends in 2058 March
There are five weekends in 2059 August
There are five weekends in 2060 October
There are five weekends in 2061 July
There are five weekends in 2062 December
There are five weekends in 2064 August
There are five weekends in 2065 May
There are five weekends in 2066 January
There are five weekends in 2066 October
There are five weekends in 2067 July
There are five weekends in 2069 March
There are five weekends in 2070 August
There are five weekends in 2071 May
There are five weekends in 2072 January
There are five weekends in 2072 July
There are five weekends in 2073 December
There are five weekends in 2075 March
There are five weekends in 2076 May
There are five weekends in 2077 January
There are five weekends in 2077 October
There are five weekends in 2078 July
There are five weekends in 2079 December
There are five weekends in 2080 March
There are five weekends in 2081 August
There are five weekends in 2082 May
There are five weekends in 2083 January
There are five weekends in 2083 October
There are five weekends in 2084 December
There are five weekends in 2086 March
There are five weekends in 2087 August
There are five weekends in 2088 October
There are five weekends in 2089 July
There are five weekends in 2090 December
There are five weekends in 2092 August
There are five weekends in 2093 May
There are five weekends in 2094 January
There are five weekends in 2094 October
There are five weekends in 2095 July
There are five weekends in 2097 March
There are five weekends in 2098 August
There are five weekends in 2099 May
There are five weekends in 2100 January
There are five weekends in 2100 October

There were  201  occurrences of five-weekend months in years 1900──►2100

Year  1900  doesn't have any five-weekend months.
Year  1906  doesn't have any five-weekend months.
Year  1917  doesn't have any five-weekend months.
Year  1923  doesn't have any five-weekend months.
Year  1928  doesn't have any five-weekend months.
Year  1934  doesn't have any five-weekend months.
Year  1945  doesn't have any five-weekend months.
Year  1951  doesn't have any five-weekend months.
Year  1956  doesn't have any five-weekend months.
Year  1962  doesn't have any five-weekend months.
Year  1973  doesn't have any five-weekend months.
Year  1979  doesn't have any five-weekend months.
Year  1984  doesn't have any five-weekend months.
Year  1990  doesn't have any five-weekend months.
Year  2001  doesn't have any five-weekend months.
Year  2007  doesn't have any five-weekend months.
Year  2012  doesn't have any five-weekend months.
Year  2018  doesn't have any five-weekend months.
Year  2029  doesn't have any five-weekend months.
Year  2035  doesn't have any five-weekend months.
Year  2040  doesn't have any five-weekend months.
Year  2046  doesn't have any five-weekend months.
Year  2057  doesn't have any five-weekend months.
Year  2063  doesn't have any five-weekend months.
Year  2068  doesn't have any five-weekend months.
Year  2074  doesn't have any five-weekend months.
Year  2085  doesn't have any five-weekend months.
Year  2091  doesn't have any five-weekend months.
Year  2096  doesn't have any five-weekend months.

There are  29  years that haven't any five─weekend months in years 1900──►2100

```



### version for older REXXes

This version will work with any version of a REXX interpreter.

```rexx
/*REXX program  finds  months  that contain  five weekends   (given a date range).      */
month. =31;  month,2=0                           /*month days;   February is skipped.   */
month.4=30;  month.6=30; month.9=30; month.11=30 /*all the  months with  thirty-days.   */
@months='January February March April May June July August September October November December'
parse arg yStart yStop .                         /*get the  "start"  and  "stop"  years.*/
if yStart=='' | yStart==","  then yStart= 1900   /*Not specified?  Then use the default.*/
if yStop =='' | yStop ==","  then yStop = 2100   /* "      "         "   "   "     "    */
years=yStop - yStart + 1                         /*calculate the number of yrs in range.*/
haps=0                                           /*number of five weekends happenings.  */
!.=0;                 @5w= 'five-weekend months' /*flag if a year has any five-weekends.*/
       do y=yStart to yStop                      /*process the years specified.         */
           do m=1  for 12;    wd.=0              /*process each month and also each year*/
                 do d=1  for month.m
                 ?=dow(m,d,y)                    /*get the  day-of-week  for  mm/dd/yyyy*/
                 wd.?=wd.?+1                     /*?:   1=Sun,  2=Mon, 3=Tue ∙∙∙  7=Sat.*/
                 end   /*d*/
           if wd.1\==5 | wd.6\==5 | wd.7\==5  then iterate            /*not a weekend ? */
           say 'There are five weekends in'   y   word(@months, m)
           haps=haps+1;   !.y=1                  /*bump counter; indicate yr has 5 WE's.*/
           end         /*m*/
      end              /*y*/
say
say  'There were ' haps " occurrence"s(haps) 'of'  @5w "in year"s(years)  yStart'──►'yStop
#=0; say
           do y=yStart  to yStop;  if !.y  then iterate                    /*skip if OK.*/
           #=#+1
           say  'Year '    y    " doesn't have any five-weekend months."
           end   /*y*/
say
say  "There are " # ' year's(#) "that haven't any"  @5w 'in year's(years) yStart'──►'yStop
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
dow:  procedure;  parse arg m,d,y;           if m<3 then  do;  m=m+12;  y=y-1;  end
      yL=left(y,2);    yr=right(y,2);        w=(d+(m+1)*26%10+yr+yr%4+yL%4+5*yL) // 7
      if w==0 then w=7;     return w            /*Sunday=1,  Monday=2, ...  Saturday=7 */
/*──────────────────────────────────────────────────────────────────────────────────────*/
s:    if arg(1)==1  then return arg(3);      return word(arg(2) 's',1)     /*pluralizer.*/
```

'''output'''   is identical to the 1<sup>st</sup> REXX version.





### version short and focussed at the task description


```rexx
/* REXX ***************************************************************
* Short(er) solution focussed at the task's description
* Only 7 months can have 5 full weekends
* and it's enough to test if the 1st day of the month is a Friday
* 30.08.2012 Walter Pachl
**********************************************************************/
Numeric digits 20
nr5fwe=0
years_without_5fwe=0
mnl='Jan Mar May Jul Aug Oct Dec'
ml='1 3 5 7 8 10 12'
Do j=1900 to 2100
  year_has_5fwe=0
  Do mi=1 To words(ml)
    m=word(ml,mi)
    jd=greg2jul(j m 1)
    IF jd//7=4 Then Do              /* 1st m j is a Friday */
      nr5fwe=nr5fwe+1
      year_has_5fwe=1
      If j<=1905 | 2095<=j Then
        Say word(mnl,mi) j 'has 5 full weekends'
      End
    End
    If j=1905 Then Say '...'
    if year_has_5fwe=0 Then years_without_5fwe=years_without_5fwe+1
  End
Say ' '
Say nr5fwe 'occurrences of 5 full weekends in a month'
Say years_without_5fwe 'years without 5 full weekends'
exit

greg2jul: Procedure
/***********************************************************************
* Converts a Gregorian date to the corresponding Julian day number
* 19891101 Walter Pachl REXXified algorithm published in CACM
*                (Fliegel & vanFlandern, CACM Vol.11 No.10 October 1968)
* 19891125 PA copy leapyear test into this to avoid the dependency
***********************************************************************/
  numeric digits 12
  Parse Arg yy mm d
  If mm<1 | 12<mm Then Call err 'month ('mm') not within 1 to 12'
  mdl='31' (28+leapyear(yy)) '31 30 31 30 31 31 30 31 30 31'
  md=word(mdl,mm)
  If d<1 | md<d  Then Call err 'day ('d') not within 1 to' md
/***********************************************************************
* The published formula:
* res=d-32075+1461*(yy+4800+(mm-14)%12)%4+,
*     367*(mm-2-((mm-14)%12)*12)%12-3*((yy+4900+(mm-14)%12)%100)%4
***********************************************************************/
  mma=(mm-14)%12
  yya=yy+4800+mma
  result=d-32075+1461*yya%4+367*(mm-2-mma*12)%12-3*((yya+100)%100)%4
    Return result                   /* return the result              */

leapyear: Return ( (arg(1)//4=0) & (arg(1)//100<>0) ) | (arg(1)//400=0)
```

{{Out}}

```txt

Mar 1901 has 5 full weekends
Aug 1902 has 5 full weekends
May 1903 has 5 full weekends
Jan 1904 has 5 full weekends
Jul 1904 has 5 full weekends
Dec 1905 has 5 full weekends
...
Jul 2095 has 5 full weekends
Mar 2097 has 5 full weekends
Aug 2098 has 5 full weekends
May 2099 has 5 full weekends
Jan 2100 has 5 full weekends
Oct 2100 has 5 full weekends

201 occurrences of 5 full weekends in a month
29 years without 5 full weekends

```



### shorter version


```rexx
/*REXX program  finds  months  that contain  five weekends   (given a date range).      */
month. =31;  month.2=0                           /*month days;   February is skipped.   */
month.4=30;  month.6=30; month.9=30; month.11=30 /*all the  months with  thirty-days.   */
parse arg yStart yStop .                         /*get the  "start"  and  "stop"  years.*/
if yStart=='' | yStart==","  then yStart= 1900   /*Not specified?  Then use the default.*/
if yStop =='' | yStop ==","  then yStop = 2100   /* "      "         "   "   "     "    */
years=yStop - yStart + 1                         /*calculate the number of yrs in range.*/
haps=0                                           /*number of five weekends happenings.  */
!.=0;                 @5w= 'five-weekend months' /*flag if a year has any five-weekends.*/
       do y=yStart to yStop                      /*process the years specified.         */
           do m=1  for 12;    wd.=0              /*process each month and also each year*/
                 do d=1  for month.m;  dat_= y"-"right(m, 2, 0)'-'right(d, 2, 0)
                 parse  upper  value   date('W', dat_, "I")    with    ? 3
                 wd.?=wd.?+1                     /*?:   1=Sun,  2=Mon, 3=Tue ∙∙∙  7=Sat.*/
                 end   /*d*/                     /*WD.su=number of Sundays in the month.*/
           if wd.su\==5 | wd.fr\==5 | wd.sa\==5 then iterate      /*is this a weekend ? */
           say 'There are five weekends in'    y     date('M', dat_, "I")
           haps=haps+1;   !.y=1                  /*bump counter; indicate yr has 5 WE's.*/
           end         /*m*/
       end             /*y*/
say
say  'There were ' haps " occurrence"s(haps) 'of'  @5w "in year"s(years)  yStart'──►'yStop
#=0; say
           do y=yStart  to yStop;  if !.y  then iterate                    /*skip if OK.*/
           #=#+1
           say  'Year '    y    " doesn't have any five-weekend months."
           end   /*y*/
say
say  "There are " # ' year's(#) "that haven't any"  @5w 'in year's(years) yStart'──►'yStop
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''   is identical to the 1<sup>st</sup> REXX version.


### shorter and more focused

This REXX version takes advantage that a month with five full weekends must start on a Friday and have 31 days.

```rexx
/*REXX program  finds  months  that contain  five weekends   (given a date range).      */
month. =31                                       /*days in "all" the months.            */
month.2=0; month.4=0; month.6=0; month.9=0; month.11=0              /*not 31 day months.*/
month.4=30;  month.6=30; month.9=30; month.11=30 /*all the  months with  thirty-days.   */
parse arg yStart yStop .                         /*get the  "start"  and  "stop"  years.*/
if yStart=='' | yStart==","  then yStart= 1900   /*Not specified?  Then use the default.*/
if yStop =='' | yStop ==","  then yStop = 2100   /* "      "         "   "   "     "    */
years=yStop - yStart + 1                         /*calculate the number of yrs in range.*/
haps=0                                           /*number of five weekends happenings.  */
!.=0;                 @5w= 'five-weekend months' /*flag if a year has any five-weekends.*/
       do y=yStart  to yStop                     /*process the years specified.         */
           do m=1  for 12;  if month.m==0  then iterate       /*only test 31-day months.*/
           dat_= y"-"right(m,2,0)'-01'           /*get the date in the desired format.  */
           if left(date('W',dat_,"I"),2)\=='Fr'  then iterate     /*isn't not a Friday? */
           say 'There are five weekends in'    y     date('M', dat_, "I")
           haps=haps+1;   !.y=1                  /*bump counter; indicate yr has 5 WE's.*/
           end   /*m*/
       end      /*y*/
say
say  'There were ' haps " occurrence"s(haps) 'of'  @5w "in year"s(years)  yStart'──►'yStop
#=0; say
           do y=yStart  to yStop;  if !.y  then iterate                    /*skip if OK.*/
           #=#+1
           say  'Year '    y    " doesn't have any five-weekend months."
           end   /*y*/
say
say  "There are " # ' year's(#) "that haven't any"  @5w 'in year's(years) yStart'──►'yStop
```

'''output'''   is identical to the 1<sup>st</sup> REXX version.





## Ring


```ring

sum = 0
month = list(12)
mo = [4,0,0,3,5,1,3,6,2,4,0,2]
mon = [31,28,31,30,31,30,31,31,30,31,30,31]
mont = ["January","February","March","April","May","June",
        "July","August","September","October","November","December"]
for year = 1900 to 2100
    if year < 2100 leap = year - 1900 else leap = year - 1904 ok
    m = ((year-1900)%7) + floor(leap/4) % 7
    oldsum = sum
    for n = 1 to 12
        month[n] = (mo[n] + m) % 7
        x = (month[n] + 1) % 7
        if x = 2 and mon[n] = 31 sum += 1 see "" + year + "-" + mont[n] + nl ok
    next
    if sum = oldsum see "" + year + "-" + "(none)" + nl ok
next
see "Total : " + sum + nl

```

Output:

```txt

1900-(none)
1901-March
1902-August
1903-May
1904-July
1905-December
1906-(none)
...
2093-May
2094-January
2094-October
2095-July
2096-(none)
2097-March
2098-August
2099-May
2100-January
2100-October
Total : 201

```



## Ruby


```ruby
require 'date'
# The only case where the month has 5 weekends is when the last day
# of the month falls on a Sunday and the month has 31 days.

LONG_MONTHS = [1,3,5,7,8,10,12]
YEARS       = (1900..2100).to_a

dates    = YEARS.product(LONG_MONTHS).map{|y, m| Date.new(y,m,31)}.select(&:sunday?)
years_4w = YEARS - dates.map(&:year)

puts "There are #{dates.size} months with 5 weekends from 1900 to 2100:"
puts dates.first(5).map {|d| d.strftime("%b %Y") }, "..."
puts dates.last(5).map {|d| d.strftime("%b %Y") }
puts "There are #{years_4w.size} years without months with 5 weekends:"
puts years_4w.join(", ")
```


'''Output'''

```txt
There are 201 months with 5 weekends from 1900 to 2100:
Mar 1901
Aug 1902
May 1903
Jan 1904
Jul 1904
...
Mar 2097
Aug 2098
May 2099
Jan 2100
Oct 2100
There are 29 years without months with 5 weekends:
1900, 1906, 1917, 1923, 1928, 1934, 1945, 1951, 1956, 1962, 1973, 1979, 1984,
1990, 2001, 2007, 2012, 2018, 2029, 2035, 2040, 2046, 2057, 2063, 2068, 2074,
2085, 2091, 2096
```



## Run BASIC


```runbasic
preYear = 1900
for yyyy      = 1900 to 2100
   for mm     = 1 to 12                ' go thru all 12 months
      dayOne$ = mm;"-01-";yyyy         ' First day of month
      n       = date$(dayOne$)         ' Days since 1700
      dow     = 1 + (n mod 7)          ' Day of Week month begins
      m1      = mm                     '
      n1      = n + 27                 ' find end of month starting with 27th day
      while   m1 = mm                  ' if month changes we have the end of the month
         n1   = n1 + 1
         n$   = date$(n1)
         m1   = val(left$(n$,2))
      wend
      mmDays  = n1 - n                 ' Days in the Month
      if dow  = 4 and mmDays = 31 then ' test for 5 weeks
         count = count + 1
         print using("###",count);" ";yyyy;"-";left$("0";mm,2)
      end if

   next mm
   if preCount = count then
     noCount = noCount + 1             ' count years that have none
     print yyyy;" has none ";noCount
   end if
   preCount = count
next yyyy
```


{{Out}}

```txt
1900 has none 1
1 1901-03
2 1902-08
3 1903-05
4 1904-01
5 1904-07
6 1905-01
1906 has none 2
7 1907-03
........
196 2095-07
2096 has none 29
197 2097-03
198 2098-08
199 2099-05
200 2100-01
201 2100-01
```



## Rust


```Rust
extern crate chrono;
use chrono::prelude::*;

/// Months with 31 days
const LONGMONTHS: [u32; 7] = [1, 3, 5, 7, 8, 10, 12];

/// Get all the tuples (year, month) in wich there is five Fridays, five Saturdays and five Sundays
/// between the years start and end (inclusive).
fn five_weekends(start: i32, end: i32) -> Vec<(i32, u32)> {
    let mut out = vec![];

    for year in start..=end {
        for month in LONGMONTHS.iter() {
            // Five weekends if a 31-days month starts with a Friday.
            if Local.ymd(year, *month, 1).weekday() == Weekday::Fri {
                out.push((year, *month));
            }
        }
    }

    out
}

fn main() {
    let out = five_weekends(1900, 2100);

    let len = out.len();
    println!(
        "There are {} months of which the first and last five are:",
        len
    );
    for (y, m) in &out[..5] {
        println!("\t{} / {}", y, m);
    }
    println!("...");
    for (y, m) in &out[(len - 5..)] {
        println!("\t{} / {}", y, m);
    }
}

#[test]
fn test() {
    let out = five_weekends(1900, 2100);
    assert_eq!(out.len(), 201);
}
```



## Scala


```scala
import java.util.Calendar._
import java.util.GregorianCalendar

import org.scalatest.{FlatSpec, Matchers}

class FiveWeekends extends FlatSpec with Matchers {

  case class YearMonth[T](year: T, month: T)
  implicit class CartesianProd[T](val seq: Seq[T]) {
    def x(other: Seq[T]) = for(s1 <- seq; s2 <- other) yield YearMonth(year=s1,month=s2)
    def -(other: Seq[T]): Seq[T] = seq diff other
  }

  def has5weekends(ym: { val year: Int; val month: Int}) = {
    val date = new GregorianCalendar(ym.year, ym.month-1, 1)
    date.get(DAY_OF_WEEK) == FRIDAY && date.getActualMaximum(DAY_OF_MONTH) == 31
  }

  val expectedFirstFive = Seq(
    YearMonth(1901,3), YearMonth(1902,8), YearMonth(1903,5), YearMonth(1904,1), YearMonth(1904,7))
  val expectedFinalFive = Seq(
    YearMonth(2097,3), YearMonth(2098,8), YearMonth(2099,5), YearMonth(2100,1), YearMonth(2100,10))
  val expectedNon5erYears = Seq(1900, 1906, 1917, 1923, 1928, 1934, 1945, 1951, 1956, 1962,
                                1973, 1979, 1984, 1990, 2001, 2007, 2012, 2018, 2029, 2035,
                                2040, 2046, 2057, 2063, 2068, 2074, 2085, 2091, 2096)

  "Five Weekend Algorithm" should "match specification" in {
    val months = (1900 to 2100) x (1 to 12) filter has5weekends
    months.size shouldBe 201
    months.take(5) shouldBe expectedFirstFive
    months.takeRight(5) shouldBe expectedFinalFive

    (1900 to 2100) - months.map(_.year) shouldBe expectedNon5erYears
  }
}
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "time.s7i";

const proc: main is func
  local
    var integer: months is 0;
    var time: firstDayInMonth is time.value;
  begin
    for firstDayInMonth.year range 1900 to 2100 do
      for firstDayInMonth.month range 1 to 12 do
        if daysInMonth(firstDayInMonth) = 31 and dayOfWeek(firstDayInMonth) = 5 then
          writeln(firstDayInMonth.year <& "-" <& firstDayInMonth.month lpad0 2);
          incr(months);
        end if;
      end for;
    end for;
    writeln("Number of months:" <& months);
  end func;
```


{{Out}}

```txt

1901-03
1902-08
1903-05
1904-01
1904-07
1905-12
...
2095-07
2097-03
2098-08
2099-05
2100-01
2100-10
Number of months:201

```



## Sidef

{{trans|Perl}}

```ruby
require('DateTime');

var happymonths = [];
var workhardyears = [];
var longmonths = [1, 3, 5, 7, 8, 10, 12];

range(1900, 2100).each { |year|
   var countmonths = 0;
   longmonths.each { |month|
        var dt = %s'DateTime'.new(
            year => year,
            month => month,
            day   => 1
        );

        if (dt.day_of_week == 5) {
            countmonths++;
            var yearfound = dt.year;
            var monthfound = dt.month_name;
            happymonths.append(join("  ", yearfound, monthfound));
      }
   }

    if (countmonths == 0) {
        workhardyears.append(year);
    }
}

say "There are #{happymonths.len} months with 5 full weekends!";
say "The first 5 and the last 5 of them are:";
say happymonths.first(5).join("\n");
say happymonths.last(5).join("\n");
say "No long weekends in the following #{workhardyears.len} years:";
say workhardyears.join(",");
```

{{out}}

```txt

There are 201 months with 5 full weekends!
The first 5 and the last 5 of them are:
1901  March
1902  August
1903  May
1904  January
1904  July
2097  March
2098  August
2099  May
2100  January
2100  October
No long weekends in the following 29 years:
1900,1906,1917,1923,1928,1934,1945,1951,1956,1962,1973,1979,1984,1990,2001,2007,2012,2018,2029,2035,2040,2046,2057,2063,2068,2074,2085,2091,2096

```


## Simula

{{trans|FreeBasic}}

```simula
! TRANSLATION OF FREEBASIC VERSION ;
BEGIN

    INTEGER PROCEDURE WD(M, D, Y); INTEGER M, D, Y;
    BEGIN
        ! ZELLERISH
        ! 0 = SUNDAY, 1 = MONDAY, 2 = TUESDAY, 3 = WEDNESDAY
        ! 4 = THURSDAY, 5 = FRIDAY, 6 = SATURDAY
        ;

        IF M < 3 THEN        ! IF M = 1 OR M = 2 THEN ;
        BEGIN
            M := M + 12;
            Y := Y - 1
        END;
        WD := MOD(Y + (Y // 4)
                    - (Y // 100)
                    + (Y // 400)
                    + D + ((153 * M + 8) // 5), 7)
    END WD;

    ! ------=< MAIN >=------
    ! ONLY MONTHS WITH 31 DAY CAN HAVE FIVE WEEKENDS
    ! THESE MONTHS ARE: JANUARY, MARCH, MAY, JULY, AUGUST, OCTOBER, DECEMBER
    ! IN NR: 1, 3, 5, 7, 8, 10, 12
    ! THE 1E DAY NEEDS TO BE ON A FRIDAY (= 5)
    ;

    TEXT PROCEDURE MONTHNAMES(M); INTEGER M;
        MONTHNAMES :- IF M =  1 THEN "JANUARY"
                 ELSE IF M =  2 THEN "FEBRUARY"
                 ELSE IF M =  3 THEN "MARCH"
                 ELSE IF M =  4 THEN "APRIL"
                 ELSE IF M =  5 THEN "MAY"
                 ELSE IF M =  6 THEN "JUNE"
                 ELSE IF M =  7 THEN "JULY"
                 ELSE IF M =  8 THEN "AUGUST"
                 ELSE IF M =  9 THEN "SEPTEMBER"
                 ELSE IF M = 10 THEN "OCTOBER"
                 ELSE IF M = 11 THEN "NOVEMBER"
                 ELSE IF M = 12 THEN "DECEMBER"
                 ELSE NOTEXT;

    INTEGER M, YR, TOTAL, I, J;
    INTEGER ARRAY YR_WITHOUT(1:200);
    TEXT ANSWER;

    FOR YR := 1900 STEP 1 UNTIL 2100 DO  ! GREGORIAN CALENDAR ;
    BEGIN
        ANSWER :- NOTEXT;
        FOR M := 1 STEP 2 UNTIL 12 DO
        BEGIN
            IF M = 9 THEN M := 8;
            IF WD(M, 1, YR) = 5 THEN
            BEGIN
                ANSWER :- ANSWER & MONTHNAMES(M) & ", ";
                TOTAL := TOTAL + 1
            END
        END;
        IF ANSWER =/= NOTEXT THEN
        BEGIN
            OUTIMAGE;
            OUTINT(YR, 4); OUTTEXT(" | ");
            OUTTEXT(ANSWER.SUB(1, ANSWER.LENGTH - 2)) ! GET RID OF EXTRA " ," ;
        END
        ELSE
        BEGIN
            I := I + 1;
            YR_WITHOUT(I) := YR
        END
    END;

    OUTIMAGE;
    OUTTEXT("NR OF MONTH FOR 1900 TO 2100 THAT HAS FIVE WEEKENDS ");
    OUTINT(TOTAL, 0);
    OUTIMAGE;
    OUTIMAGE;
    OUTINT(I, 0);
    OUTTEXT(" YEARS DON'T HAVE MONTHS WITH FIVE WEEKENDS");
    OUTIMAGE;

    FOR J := 1 STEP 1 UNTIL I DO
    BEGIN
        OUTINT(YR_WITHOUT(J), 0); OUTCHAR(' ');
        IF MOD(J, 8) = 0 THEN OUTIMAGE
    END;
    OUTIMAGE

END.

```

{{out}}

```txt


1901 | MARCH
1902 | AUGUST
1903 | MAY
1904 | JANUARY, JULY
1905 | DECEMBER
...
2095 | JULY
2097 | MARCH
2098 | AUGUST
2099 | MAY
2100 | JANUARY, OCTOBER
NR OF MONTH FOR 1900 TO 2100 THAT HAS FIVE WEEKENDS 201

29 YEARS DON'T HAVE MONTHS WITH FIVE WEEKENDS
1900 1906 1917 1923 1928 1934 1945 1951
1956 1962 1973 1979 1984 1990 2001 2007
2012 2018 2029 2035 2040 2046 2057 2063
2068 2074 2085 2091 2096

```



## Stata



```stata
clear
set obs `=tm(2101m1)-tm(1900m1)'
gen month=tm(1900m1)+_n-1
format %tm month
gen day=dofm(month)
keep if dofm(month+1)-day==31 & dow(day)==5
drop day

count
  201

list in f/5, noobs noheader

  +--------+
  | 1901m3 |
  | 1902m8 |
  | 1903m5 |
  | 1904m1 |
  | 1904m7 |
  +--------+

list in -5/l, noobs noheader

  +---------+
  |  2097m3 |
  |  2098m8 |
  |  2099m5 |
  |  2100m1 |
  | 2100m10 |
  +---------+
```



## Tcl


```tcl
package require Tcl 8.5

set months {}
set years {}
for {set year 1900} {$year <= 2100} {incr year} {
    set count [llength $months]
    foreach month {Jan Mar May Jul Aug Oct Dec} {
	set date [clock scan "$month/01/$year" -format "%b/%d/%Y" -locale en_US]
	if {[clock format $date -format %u] == 5} {
	    # Month with 31 days that starts on a Friday => has 5 weekends
	    lappend months "$month $year"
	}
    }
    if {$count == [llength $months]} {
	# No change to number of months; year must've been without
	lappend years $year
    }
}
puts "There are [llength $months] months with five weekends"
puts [join [list {*}[lrange $months 0 4] ... {*}[lrange $months end-4 end]] \n]
puts "There are [llength $years] years without any five-weekend months"
puts [join $years ","]
```

{{Out}}

```txt

There are 201 months with five weekends
Mar 1901
Aug 1902
May 1903
Jan 1904
Jul 1904
...
Mar 2097
Aug 2098
May 2099
Jan 2100
Oct 2100
There are 29 years without any five-weekend months
1900,1906,1917,1923,1928,1934,1945,1951,1956,1962,1973,1979,1984,1990,2001,2007,2012,2018,2029,2035,2040,2046,2057,2063,2068,2074,2085,2091,2096

```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
LOOP year=1900,2100
 LOOP month="1'3'5'7'8'10'12"
 SET dayofweek=DATE (number,1,month,year,nummer)
 IF (dayofweek==5) PRINT year,"-",month
 ENDLOOP
ENDLOOP

```

{{Out}}
<pre style='height:30ex;overflow:scroll'>
1901-3
1902-8
1903-5
1904-1
1904-7
1905-12
1907-3
1908-5
1909-1
1909-10
1910-7
1911-12
1912-3
1913-8
1914-5
1915-1
1915-10
1916-12
1918-3
1919-8
1920-10
1921-7
1922-12
1924-8
1925-5
1926-1
1926-10
1927-7
1929-3
1930-8
1931-5
1932-1
1932-7
1933-12
1935-3
1936-5
1937-1
1937-10
1938-7
1939-12
1940-3
1941-8
1942-5
1943-1
1943-10
1944-12
1946-3
1947-8
1948-10
1949-7
1950-12
1952-8
1953-5
1954-1
1954-10
1955-7
1957-3
1958-8
1959-5
1960-1
1960-7
1961-12
1963-3
1964-5
1965-1
1965-10
1966-7
1967-12
1968-3
1969-8
1970-5
1971-1
1971-10
1972-12
1974-3
1975-8
1976-10
1977-7
1978-12
1980-8
1981-5
1982-1
1982-10
1983-7
1985-3
1986-8
1987-5
1988-1
1988-7
1989-12
1991-3
1992-5
1993-1
1993-10
1994-7
1995-12
1996-3
1997-8
1998-5
1999-1
1999-10
2000-12
2002-3
2003-8
2004-10
2005-7
2006-12
2008-8
2009-5
2010-1
2010-10
2011-7
2013-3
2014-8
2015-5
2016-1
2016-7
2017-12
2019-3
2020-5
2021-1
2021-10
2022-7
2023-12
2024-3
2025-8
2026-5
2027-1
2027-10
2028-12
2030-3
2031-8
2032-10
2033-7
2034-12
2036-8
2037-5
2038-1
2038-10
2039-7
2041-3
2042-8
2043-5
2044-1
2044-7
2045-12
2047-3
2048-5
2049-1
2049-10
2050-7
2051-12
2052-3
2053-8
2054-5
2055-1
2055-10
2056-12
2058-3
2059-8
2060-10
2061-7
2062-12
2064-8
2065-5
2066-1
2066-10
2067-7
2069-3
2070-8
2071-5
2072-1
2072-7
2073-12
2075-3
2076-5
2077-1
2077-10
2078-7
2079-12
2080-3
2081-8
2082-5
2083-1
2083-10
2084-12
2086-3
2087-8
2088-10
2089-7
2090-12
2092-8
2093-5
2094-1
2094-10
2095-7
2097-3
2098-8
2099-5
2100-1
2100-10

```



## uBasic/4tH

{{trans|FreeBASIC}}
<lang>' ------=< MAIN >=------
' only months with 31 day can have five weekends
' these months are: January, March, May, July, August, October, December
' in nr: 1, 3, 5, 7, 8, 10, 12
' the 1e day needs to be on a friday (= 5)

For y = 1900 To 2100  ' Gregorian calendar
    a = 0
    For m = 1 To 12 Step 2
        If m = 9 Then m = 8
        If Func(_wd(m , 1 , y)) = 5 Then
           If a Then
              Print ", ";
           Else
              Print y; " | ";
              a = 1
           EndIf
           GoSub m*10
           t = t + 1
        EndIf
    Next
    If a Then
       Print
    Else
       i = i + 1
       @(i) = y
    EndIf
Next

Print
Print "Number of months from 1900 to 2100 that have five weekends: ";t
Print
Print i;" years don't have months with five weekends:"
Print

For j = 1 To i
    Print @(j); " ";
    If (j % 8) = 0 Then Print
Next

Print
End


_wd Param(3)
    ' Zellerish
    ' 0 = Sunday, 1 = Monday, 2 = Tuesday, 3 = Wednesday
    ' 4 = Thursday, 5 = Friday, 6 = Saturday

    If a@ < 3 Then        ' If a@ = 1 Or a@ = 2 Then
        a@ = a@ + 12
        c@ = c@ - 1
    EndIf
Return ((c@ + (c@ / 4) - (c@ / 100) + (c@ / 400) + b@ + ((153 * a@ + 8) / 5)) % 7)


 10 Print "January"; : Return
 20 Print "February"; : Return
 30 Print "March"; : Return
 40 Print "April"; : Return
 50 Print "May"; : Return
 60 Print "June"; : Return
 70 Print "July"; : Return
 80 Print "August"; : Return
 90 Print "September"; : Return
100 Print "October"; : Return
110 Print "November"; : Return
120 Print "December"; : Return
```

{{out}}

```txt
1901 | March
1902 | August
1903 | May
1904 | January, July
...
2099 | May
2100 | January, October

Number of months from 1900 to 2100 that have five weekends: 201

29 years don't have months with five weekends:

1900 1906 1917 1923 1928 1934 1945 1951
1956 1962 1973 1979 1984 1990 2001 2007
2012 2018 2029 2035 2040 2046 2057 2063
2068 2074 2085 2091 2096

0 OK, 0:888
```



## UNIX Shell

{{works with|Bourne Again SHell|3}}
{{works with|ksh}}

This is a 2-step-solution:
* create a file with 1-month calendars for all the required month and years
* feed this file to an awk-script to look for the months with 5 weekends.


```bash
echo "Creating cal-file..."
echo > cal.txt
for ((y=1900; y <= 2100; y++)); do
  for ((m=1; m <= 12; m++)); do
   #echo $m $y
    cal -m $m $y >> cal.txt
  done
done
ls -la cal.txt

echo "Looking for month with 5 weekends:"
awk -f 5weekends.awk  cal.txt

```


See also: [http://rosettacode.org/wiki/Five_weekends#AWK awk] and [[Calendar]].

Try it out online: [http://www.compileonline.com/execute_ksh_online.php compileonline.com]


## VBA


```vb

Option Explicit

Sub Main()
Dim y As Long, m As Long, t As String, cpt As Long, cptm As Long
    For y = 1900 To 2100
        t = vbNullString
        For m = 1 To 12 Step 2
            If m = 9 Then m = 8
            If Weekday(DateSerial(y, m, 1)) = vbFriday Then
                t = t & ", " & m
                cptm = cptm + 1
            End If
        Next
        If t <> "" Then
            Debug.Print y & t
        Else
            cpt = cpt + 1
        End If
    Next
    Debug.Print "There is " & cptm & " months with five full weekends from the year 1900 through 2100"
    Debug.Print "There is " & cpt & " years which don't have months with five weekends"
End Sub
```

{{out}}

```txt
1901, 3
1902, 8
1903, 5
1904, 1, 7
1905, 12
1907, 3
1908, 5
1909, 1, 10
1910, 7
......
2090, 12
2092, 8
2093, 5
2094, 1, 10
2095, 7
2097, 3
2098, 8
2099, 5
2100, 1, 10
There is 201 months with five full weekends from the year 1900 through 2100
There is 29 years which don't have months with five weekends
```


## VBScript

{{works with|Windows Script Host|*}}

```VBScript

For y = 1900 To 2100
	For m = 1 To 12
		d = DateSerial(y, m + 1, 1) - 1
		If Day(d) = 31 And Weekday(d) = vbSunday Then
			WScript.Echo y & ", " & MonthName(m)
			i = i + 1
		End If
	Next
Next

WScript.Echo vbCrLf & "Total = " & i & " months"

```



## XPL0


```XPL0
include c:\cxpl\codes;                  \intrinsic 'code' declarations


func    WeekDay(Year, Month, Day);      \Return day of week (0=Sat 1=Sun..6=Fri)
int     Year, Month, Day;
[if Month<=2 then [Month:= Month+12;  Year:= Year-1];
return rem((Day + (Month+1)*26/10 + Year + Year/4 + Year/100*6 + Year/400) / 7);
];      \WeekDay


int     MonthTbl, Year, I, C;
[MonthTbl:= [1, 3, 5, 7, 8, 10, 12];            \months with 31 days
C:= 0;
for Year:= 1900 to 2100 do
  for I:= 0 to 6 do                             \for all the 31-day months...
    if WeekDay(Year, MonthTbl(I), 1) = 6 then   \first of month is a Friday
        [C:= C+1;                               \count this year
        if C<=5 or C>201-5 then                 \show first 5 and last 5 years
                [IntOut(0, Year);  ChOut(0, ^ );
                 IntOut(0, MonthTbl(I));  CrLf(0);
                ];
        ];
IntOut(0, C);  CrLf(0);                         \show number of years

\Count and show all years that don't have any 5-weekend months
C:= 0;
for Year:= 1900 to 2100 do
        [for I:= 0 to 6 do                      \for all the 31-day months...
            if WeekDay(Year, MonthTbl(I), 1) = 6 \Friday\ then
                I:= 10;                         \bail out of 'for' loop
        if I<10 then                            \'for' loop completed
                [if (C&$F) = 0 then CrLf(0);    \(format 16 years per line)
                C:= C+1;                        \ without finding a 5-weekend
                IntOut(0, Year);  ChOut(0, ^ ); \ so show the year
                ];
        ];
CrLf(0);  IntOut(0, C);  CrLf(0);               \show number of years
]
```


{{Out}}

```txt

1901 3
1902 8
1903 5
1904 1
1904 7
2097 3
2098 8
2099 5
2100 1
2100 10
201

1900 1906 1917 1923 1928 1934 1945 1951 1956 1962 1973 1979 1984 1990 2001 2007
2012 2018 2029 2035 2040 2046 2057 2063 2068 2074 2085 2091 2096
29

```



## zkl

Months with five weekends:

```zkl
var [const] D=Time.Date, r=L();
foreach y,m in ([1900..2100],[1..12]){
   if (D.daysInMonth(y,m)==31 and 5==D.weekDay(y,m,1)) r.append(String(y,"-",m))
}
```

Alternatively, in a functional style using list comprehensions:

```zkl
r:=[[(y,m); [1900..2100];
            [1..12],{D.daysInMonth(y,m)==31 and 5==D.weekDay(y,m,1)};
            {String(y,"-",m)}]];
```

{{out}}

```txt

r.len() //-->201
r[0,5]  //-->L("1901-3","1902-8","1903-5","1904-1","1904-7")
r[-5,*] //-->L("2097-3","2098-8","2099-5","2100-1","2100-10")

```


Extra credit: Years with no five day weekends:

```zkl
var [const] D=Time.Date, r=L();
foreach y in ([1900..2100]){ yes:=True;
   foreach m in ([1..12]){
      if (D.daysInMonth(y,m)==31 and 5==D.weekDay(y,m,1)) { yes=False; break; }
   }
   if (yes) r.append(y)
}
```

Alternatively:

```zkl
var yes, r=[[(y,m);
   [1900..2100];
   [1..12],{if(m==1)yes=True; if(D.daysInMonth(y,m)==31 and 5==D.weekDay(y,m,1)) yes=False; True},
           {if (m==12) yes else False};
   {y}]]
```

Bit of a sleaze using a global var (yes) to hold state.
First filter: On the first month, reset global, note if month has five weekends, always pass.
Second filter: fail if any five day weekends and ignore all months other than December.
{{out}}

```txt

r.len() //-->29
r       //-->L(1900,1906,1917,1923,1928,1934,1945,1951,1956,1962,1973,1979,1984,1990,2001,2007,2012,2018,2029,2035,...)

```

