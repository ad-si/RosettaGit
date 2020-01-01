+++
title = "Last Friday of each month"
description = ""
date = 2018-08-26T09:31:44Z
aliases = []
[extra]
id = 10782
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[Category:Date and time]]

;Task:
Write a program or a script that returns the date of the last Fridays of each month of a given year.

The year may be given through any simple input method in your language (command line, std in, etc).


Example of an expected output:

```txt
./last_fridays 2012
2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28

```




;Related tasks
* [[Five weekends]]
* [[Day of the week]]
* [[Find the last Sunday of each month]]





## 360 Assembly

The program uses one ASSIST macro (XPRNT) to keep the code as short as possible.

```360asm
*        Last Friday of each month 17/07/2016
LASTFRI  CSECT
         USING  LASTFRI,R13        base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         " <-
         ST     R15,8(R13)         " ->
         LR     R13,R15            " addressability
         L      R4,YEAR            year
         SRDA   R4,32              .
         D      R4,=F'400'         year/400
         LTR    R4,R4              if year//400=0
         BZ     LEAP
         L      R4,YEAR            year
         SRDA   R4,32              .
         D      R4,=F'4'           year/4
         LTR    R4,R4              if year//4=0
         BNZ    NOTLEAP
         L      R4,YEAR            year
         SRDA   R4,32              .
         D      R4,=F'100'         year/400
         LTR    R4,R4              if year//100=0
         BZ     NOTLEAP
LEAP     MVC    DAYS+4(4),=F'29'   days(2)=29
NOTLEAP  L      R8,YEAR            year
         BCTR   R8,0               y=year-1
         LA     R7,44              44
         AR     R7,R8              +y
         LR     R3,R8              y
         SRA    R3,2               y/4
         AR     R7,R3              +y/4
         LR     R4,R8              y
         SRDA   R4,32              .
         D      R4,=F'100'         y/100
         LA     R4,0               .
         M      R4,=F'6'           *6
         AR     R7,R5              +6*(y/100)
         LR     R4,R8              y
         SRDA   R4,32              .
         D      R4,=F'400'         y/100
         AR     R7,R5              k=44+y+y/4+6*(y/100)+y/400
         LA     R6,1               m=1
LOOPM    C      R6,=F'12'          do m=1 to 12
         BH     ELOOPM
         LR     R1,R6              m
         SLA    R1,2               .
         L      R2,DAYS-4(R1)      days(m)
         AR     R7,R2              k=k+days(m)
         LR     R4,R7              k
         SRDA   R4,32              .
         D      R4,=F'7'           k/7
         SR     R2,R4              days(m)-k//7
         LR     R9,R2              d=days(m)-k//7
         L      R1,YEAR            year
         CVD    R1,DW              year: binary to packed
         OI     DW+7,X'0F'           zap sign
         UNPK   PG(4),DW             unpack (ZL4)
         CVD    R6,DW              m : binary to packed
         OI     DW+7,X'0F'           zap sign
         UNPK   PG+5(2),DW           unpack (ZL2)
         CVD    R9,DW              d: binary to packed
         OI     DW+7,X'0F'           zap sign
         UNPK   PG+8(2),DW           unpack (ZL2)
         XPRNT  PG,L'PG            print buffer
         LA     R6,1(R6)           m=m+1
         B      LOOPM
ELOOPM   L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    " restore
         XR     R15,R15            " rc=0
         BR     R14                exit
YEAR     DC     F'2016'            <== input year
DAYS     DC     F'31',F'28',F'31',F'30',F'31',F'30'
         DC     F'31',F'31',F'30',F'31',F'30',F'31'
PG       DC     CL80'YYYY-MM-DD'   buffer
XDEC     DS     CL12               temp
DW       DS     D                  packed (PL8) 15num
         YREGS
         END    LASTFRI
```

{{out}}

```txt

2016-01-29
2016-02-26
2016-03-25
2016-04-29
2016-05-27
2016-06-24
2016-07-29
2016-08-26
2016-09-30
2016-10-28
2016-11-25
2016-12-30

```



## Ada


Uses GNAT. Applicable to any day of the week, cf. [[http://rosettacode.org/wiki/Find_last_sunday_of_each_month#Ada]].


```Ada
with Ada.Text_IO, GNAT.Calendar.Time_IO, Ada.Command_Line,
  Ada.Calendar.Formatting, Ada.Calendar.Arithmetic;

procedure Last_Weekday_In_Month is

   procedure Put_Line(T: Ada.Calendar.Time) is
      use GNAT.Calendar.Time_IO;
   begin
      Ada.Text_IO.Put_Line(Image(Date => T, Picture => ISO_Date));
   end Put_Line;

   use Ada.Calendar, Ada.Calendar.Arithmetic;
   subtype Day_Name is Formatting.Day_Name; use type Formatting.Day_Name;

   T, Selected : Time;
   Weekday: Day_Name  := Day_Name'Value(Ada.Command_Line.Argument (1));
   Year : Year_Number := Integer'Value (Ada.Command_Line.Argument (2));

begin
   for Month in 1 .. 12 loop
      T := Time_Of (Year => Year, Month => Month, Day => 01);
      while Ada.Calendar.Month(T) = Month loop
	 if Formatting.Day_Of_Week (T) = Weekday then
	    Selected := T;
	 end if;
	 T := T + Day_Count(1);
      end loop;
      Put_Line(Selected);
   end loop;
end Last_Weekday_In_Month;
```

{{out}}

```txt
>./last_weekday_in_month friday 2012
2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28
```


=={{Header|AppleScript}}==
{{Trans|JavaScript}}

```AppleScript
-- LAST FRIDAYS OF YEAR ------------------------------------------------------

--  lastFridaysOfYear :: Int -> [Date]
on lastFridaysOfYear(y)

    -- lastWeekDaysOfYear :: Int -> Int -> [Date]
    script lastWeekDaysOfYear
        on |λ|(intYear, iWeekday)

            -- lastWeekDay :: Int -> Int -> Date
            script lastWeekDay
                on |λ|(iLastDay, iMonth)
                    set iYear to intYear

                    calendarDate(iYear, iMonth, iLastDay - ¬
                        (((weekday of calendarDate(iYear, iMonth, iLastDay)) ¬
                            as integer) + (7 - (iWeekday))) mod 7)
                end |λ|
            end script

            map(lastWeekDay, lastDaysOfMonths(intYear))
        end |λ|

        -- isLeapYear :: Int -> Bool
        on isLeapYear(y)
            (0 = y mod 4) and (0 ≠ y mod 100) or (0 = y mod 400)
        end isLeapYear

        -- lastDaysOfMonths :: Int -> [Int]
        on lastDaysOfMonths(y)
            {31, cond(isLeapYear(y), 29, 28), ¬
                31, 30, 31, 30, 31, 31, 30, 31, 30, 31}
        end lastDaysOfMonths
    end script

    lastWeekDaysOfYear's |λ|(y, Friday as integer)
end lastFridaysOfYear


-- TEST ----------------------------------------------------------------------
on run argv

    intercalate(linefeed, ¬
        map(isoRow, ¬
            transpose(map(lastFridaysOfYear, ¬
                apply(cond(class of argv is list and argv ≠ {}, ¬
                    singleYearOrRange, fiveCurrentYears), ¬
                    argIntegers(argv))))))

end run


-- ARGUMENT HANDLING ---------------------------------------------------------

-- Up to two optional command line arguments: [yearFrom], [yearTo]
-- (Default range in absence of arguments:
--  from two years ago, to two years ahead)

-- ~ $ osascript ~/Desktop/lastFridays.scpt
-- ~ $ osascript ~/Desktop/lastFridays.scpt 2013
-- ~ $ osascript ~/Desktop/lastFridays.scpt 2013 2016

-- singleYearOrRange :: [Int] -> [Int]
on singleYearOrRange(argv)
    apply(cond(length of argv > 0, my enumFromTo, my fiveCurrentYears), argv)
end singleYearOrRange

-- fiveCurrentYears :: () -> [Int]
on fiveCurrentYears(_)
    set intThisYear to year of (current date)
    enumFromTo(intThisYear - 2, intThisYear + 2)
end fiveCurrentYears

-- argIntegers :: maybe [String] -> [Int]
on argIntegers(argv)
    -- parseInt :: String -> Int
    script parseInt
        on |λ|(s)
            s as integer
        end |λ|
    end script

    if class of argv is list and argv ≠ {} then
        {map(parseInt, argv)}
    else
        {}
    end if
end argIntegers


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- Dates and date strings ----------------------------------------------------

-- calendarDate :: Int -> Int -> Int -> Date
on calendarDate(intYear, intMonth, intDay)
    tell (current date)
        set {its year, its month, its day, its time} to ¬
            {intYear, intMonth, intDay, 0}
        return it
    end tell
end calendarDate

-- isoDateString :: Date -> String
on isoDateString(dte)
    (((year of dte) as string) & ¬
        "-" & text items -2 thru -1 of ¬
        ("0" & ((month of dte) as integer) as string)) & ¬
        "-" & text items -2 thru -1 of ¬
        ("0" & day of dte)
end isoDateString

-- Testing and tabulation ----------------------------------------------------

-- apply (a -> b) -> a -> b
on apply(f, a)
    mReturn(f)'s |λ|(a)
end apply

-- cond :: Bool -> (a -> b) -> (a -> b) -> (a -> b)
on cond(bool, f, g)
    if bool then
        f
    else
        g
    end if
end cond

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if m > n then
        set d to -1
    else
        set d to 1
    end if
    set lst to {}
    repeat with i from m to n by d
        set end of lst to i
    end repeat
    return lst
end enumFromTo

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to ¬
        {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

-- isoRow :: [Date] -> String
on isoRow(lstDate)
    intercalate(tab, map(my isoDateString, lstDate))
end isoRow

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

-- transpose :: [[a]] -> [[a]]
on transpose(xss)
    script column
        on |λ|(_, iCol)
            script row
                on |λ|(xs)
                    item iCol of xs
                end |λ|
            end script

            map(row, xss)
        end |λ|
    end script

    map(column, item 1 of xss)
end transpose
```

{{Out}}

```txt
2014-01-31    2015-01-30    2016-01-29    2017-01-27    2018-01-26
2014-02-28    2015-02-27    2016-02-26    2017-02-24    2018-02-23
2014-03-28    2015-03-27    2016-03-25    2017-03-31    2018-03-30
2014-04-25    2015-04-24    2016-04-29    2017-04-28    2018-04-27
2014-05-30    2015-05-29    2016-05-27    2017-05-26    2018-05-25
2014-06-27    2015-06-26    2016-06-24    2017-06-30    2018-06-29
2014-07-25    2015-07-31    2016-07-29    2017-07-28    2018-07-27
2014-08-29    2015-08-28    2016-08-26    2017-08-25    2018-08-31
2014-09-26    2015-09-25    2016-09-30    2017-09-29    2018-09-28
2014-10-31    2015-10-30    2016-10-28    2017-10-27    2018-10-26
2014-11-28    2015-11-27    2016-11-25    2017-11-24    2018-11-30
2014-12-26    2015-12-25    2016-12-30    2017-12-29    2018-12-28
```



## AutoHotkey


```AHK
if 1 = ; no parameter passed
{
	InputBox, 1, Last Fridays of year, Enter a year:, , , , , , , , %A_YYYY%
	If ErrorLevel
		ExitApp
}

YYYY = %1% ; retrieve command line parameter
Stmp = %YYYY%0101000000
count= 0

While count < 12
{
	FormatTime, ddd, %stmp%, ddd
	FormatTime, M, %stmp%, M
	If (ddd = "Fri"){
		if (M-1 = count){
			t := stmp
			stmp += 7, days
		}
		else
			 res .= SubStr(t, 1, 4) "-" SubStr(t, 5, 2) "-" SubStr(t, 7, 2) "`n"
			,count++
			,stmp := YYYY . SubStr("0" M, -1) . "01"
	}
	else
		stmp += 1, days
}
MsgBox % res
```

{{out}} for 2012:

```txt
2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28
```



## AutoIt



```AutoIt

#include <Date.au3>

$iYear = InputBox('Last Friday in each month', 'Please input the year:')

_GetLastFridays($iYear)

Func _GetLastFridays($_iYear)
	Local $sResult = 'last fridays in ' & $_iYear & @LF, $iDay
	Local $aDaysInMonth[12] = [31,28,31,30,31,30,31,31,30,31,30,31]
	If _DateIsLeapYear($_iYear) Then $aDaysInMonth[1] = 29
	For $i = 1 To 12
		$iDay = $aDaysInMonth[$i-1]
		While 1
			If _DateToDayOfWeekISO($_iYear, $i, $iDay) = 5 Then
				$sResult &= StringFormat('%4d-%02d-%02d', $_iYear, $i, $iDay) & @LF
				ExitLoop
			EndIf
			$iDay -= 1
		WEnd
	Next
	ConsoleWrite($sResult)
EndFunc  ;==>_GetFridays

```

{{out}}

```txt

last fridays in 2012
2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28

```

--[[User:BugFix|BugFix]] ([[User talk:BugFix|talk]]) 13:27, 15 November 2013 (UTC)


## AWK


```AWK

# syntax: GAWK -f LAST_FRIDAY_OF_EACH_MONTH.AWK year
# converted from Fortran
BEGIN {
    split("31,28,31,30,31,30,31,31,30,31,30,31",daynum_array,",") # days per month in non leap year
    year = ARGV[1]
    if (year % 400 == 0 || (year % 4 == 0 && year % 100 != 0)) {
      daynum_array[2] = 29
    }
    y = year - 1
    k = 44 + y + int(y/4) + int(6*(y/100)) + int(y/400)
    for (m=1; m<=12; m++) {
      k += daynum_array[m]
      d = daynum_array[m] - (k%7)
      printf("%04d-%02d-%02d\n",year,m,d)
    }
    exit(0)
}

```

{{out}}

```txt

2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28

```



## Befunge

{{trans|C}}
The algorithm has been slightly simplified to avoid the additional day adjustment inside the loop, and the year is obtained from stdin rather than via the command line.


```befunge
":raeY",,,,,&>55+,:::45*:*%\"d"%!*\4%+!3v
v2++1**"I"5\+/*:*54\-/"d"\/4::-1::p53+g5<
>:00p5g4-+7%\:0\v>,"-",5g+:55+/68*+,55+%v
^<<_$$vv*86%+55:<^+*86%+55,+*86/+55:-1:<6
>$$^@$<>+\55+/:#^_$>:#,_$"-",\:04-\-00g^8
^<# #"#"##"#"##!`       +76:+1g00,+55,+*<
```


{{out}}

```txt
Year:2012

2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28
```



## C

Doesn't work with Julian calendar (then again, you probably don't need to plan your weekends for middle ages).


```c
#include <stdio.h>
#include <stdlib.h>

int main(int c, char *v[])
{
	int days[] = {31,29,31,30,31,30,31,31,30,31,30,31};
	int m, y, w;

	if (c < 2 || (y = atoi(v[1])) <= 1700) return 1;
 	days[1] -= (y % 4) || (!(y % 100) && (y % 400));
	w = y * 365 + (y - 1) / 4 - (y - 1) / 100 + (y - 1) / 400 + 6;

	for(m = 0; m < 12; m++) {
		w = (w + days[m]) % 7;
		printf("%d-%02d-%d\n", y, m + 1,
			days[m] + (w < 5 ? -2 : 5) - w);
	}

	return 0;
}
```



## C#


```c#
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;

namespace RosettaCode.LastFridaysOfYear
{
    internal static class Program
    {
        private static IEnumerable<DateTime> LastFridaysOfYear(int year)
        {
            for (var month = 1; month <= 12; month++)
            {
                var date = new DateTime(year, month, 1).AddMonths(1).AddDays(-1);
                while (date.DayOfWeek != DayOfWeek.Friday)
                {
                    date = date.AddDays(-1);
                }
                yield return date;
            }
        }

        private static void Main(string[] arguments)
        {
            int year;
            var argument = arguments.FirstOrDefault();
            if (string.IsNullOrEmpty(argument) || !int.TryParse(argument, out year))
            {
                year = DateTime.Today.Year;
            }

            foreach (var date in LastFridaysOfYear(year))
            {
                Console.WriteLine(date.ToString("d", CultureInfo.InvariantCulture));
            }
        }
    }
}
```

{{out}}

```txt
01/27/2012
02/24/2012
03/30/2012
04/27/2012
05/25/2012
06/29/2012
07/27/2012
08/31/2012
09/28/2012
10/26/2012
11/30/2012
12/28/2012
```



## C++

{{libheader|Boost}}
called with <code>./last_fridays 2012</code>

```cpp
#include <boost/date_time/gregorian/gregorian.hpp>
#include <iostream>
#include <cstdlib>

int main( int argc , char* argv[ ] ) {
   using namespace boost::gregorian ;

   greg_month months[ ] = { Jan , Feb , Mar , Apr , May , Jun , Jul ,
      Aug , Sep , Oct , Nov , Dec } ;
   greg_year gy = atoi( argv[ 1 ] ) ;
   for ( int i = 0 ; i < 12 ; i++ ) {
      last_day_of_the_week_in_month lwdm ( Friday , months[ i ] ) ;
      date d = lwdm.get_date( gy ) ;
      std::cout << d << std::endl ;
   }
   return 0 ;
}
```

{{out}}

```txt
2012-Jan-27
2012-Feb-24
2012-Mar-30
2012-Apr-27
2012-May-25
2012-Jun-29
2012-Jul-27
2012-Aug-31
2012-Sep-28
2012-Oct-26
2012-Nov-30
2012-Dec-28

```



## Clojure

{{libheader|clj-time}}


```clojure
(use '[clj-time.core :only [last-day-of-the-month day-of-week minus days]]
     '[clj-time.format :only [unparse formatters]])

(defn last-fridays [year]
  (let [last-days (map #(last-day-of-the-month year %) (range 1 13 1))
        dow (map day-of-week last-days)
        relation (zipmap last-days dow)]
    (map #(minus (key %) (days (mod (+ (val %) 2) 7))) relation)))

(defn last-fridays-formatted [year]
  (sort (map #(unparse (formatters :year-month-day) %) (last-fridays year))))
```


{{out}}

```txt
user=> (pprint (last-fridays-formatted 2012))
("2012-01-27"
 "2012-02-24"
 "2012-03-30"
 "2012-04-27"
 "2012-05-25"
 "2012-06-29"
 "2012-07-27"
 "2012-08-31"
 "2012-09-28"
 "2012-10-26"
 "2012-11-30"
 "2012-12-28")
```



## COBOL


```COBOL

       program-id. last-fri.
       data division.
       working-storage section.
       1 wk-date.
        2 yr pic 9999.
        2 mo pic 99 value 1.
        2 da pic 99 value 1.
       1 rd-date redefines wk-date pic 9(8).
       1 binary.
        2 int-date pic 9(8).
        2 dow pic 9(4).
        2 friday pic 9(4) value 5.
       procedure division.
           display "Enter a calendar year (1601 thru 9999): "
               with no advancing
           accept yr
           if yr >= 1601 and <= 9999
               continue
           else
               display "Invalid year"
               stop run
           end-if
           perform 12 times
               move 1 to da
               add 1 to mo
               if mo > 12              *> to avoid y10k in 9999
                   move 12 to mo
                   move 31 to da
               end-if
               compute int-date = function
                   integer-of-date (rd-date)
               if mo =12 and da = 31   *> to avoid y10k in 9999
                   continue
               else
                   subtract 1 from int-date
               end-if
               compute rd-date = function
                   date-of-integer (int-date)
               compute dow = function mod
                   ((int-date - 1) 7) + 1
               compute dow = function mod ((dow - friday) 7)
               subtract dow from da
               display yr "-" mo "-" da
               add 1 to mo
           end-perform
           stop run
           .
       end program last-fri.

```


{{out}}

```txt

2016-01-29
2016-02-26
2016-03-25
2016-04-29
2016-05-27
2016-06-24
2016-07-29
2016-08-26
2016-09-30
2016-10-28
2016-11-25
2016-12-30

```



## CoffeeScript


```coffeescript

last_friday_of_month = (year, month) ->
  # month is 1-based, JS API is 0-based, then we use
  # non-positive indexes to work backward relative to the
  # first day of the next month
  i = 0
  while true
    last_day = new Date(year, month, i)
    if last_day.getDay() == 5
      return last_day.toDateString()
    i -= 1

print_last_fridays_of_month = (year) ->
  for month in [1..12]
    console.log last_friday_of_month year, month

do ->
  year = parseInt process.argv[2]
  print_last_fridays_of_month year

```

{{out}}
<lang>
> coffee last_friday.coffee 2012
Fri Jan 27 2012
Fri Feb 24 2012
Fri Mar 30 2012
Fri Apr 27 2012
Fri May 25 2012
Fri Jun 29 2012
Fri Jul 27 2012
Fri Aug 31 2012
Fri Sep 28 2012
Fri Oct 26 2012
Fri Nov 30 2012
Fri Dec 28 2012

```



## Common Lisp

{{works with|CLISP}}
The command-line argument processing is the only CLISP-specific code.


```lisp
(defun friday-before (year month day)
 (let*
  ((timestamp (encode-universal-time 0 0 12 day month year))
   (weekday (nth 6 (multiple-value-list (decode-universal-time timestamp))))
   (fri (- timestamp (* (+ (mod (+ weekday 2) 7) 1) 86400))))
    (multiple-value-bind (_ _ _ d m y) (decode-universal-time fri)
     (list y m d))))

(defun last-fridays (year)
  (append (loop for month from 2 to 12 collecting (friday-before year month 1))
          (list (friday-before (1+ year) 1 1))))

(let* ((year (read-from-string (car *args*))))
  (format t "~{~{~a-~2,'0d-~2,'0d~}~%~}" (last-fridays year)))
```


Sample run for the year 2015:
{{Output}}

```txt
2015-01-30
2015-02-27
2015-03-27
2015-04-24
2015-05-29
2015-06-26
2015-07-31
2015-08-28
2015-09-25
2015-10-30
2015-11-27
2015-12-25
```



## D


```d
import std.stdio, std.datetime, std.traits;

void lastFridays(in uint year) {
    auto date = Date(year, 1, 1);
    foreach (_; [EnumMembers!Month]) {
        date.day(date.daysInMonth);
        date.roll!"days"(-(date.dayOfWeek + 2) % 7);
        writeln(date);
        date.add!"months"(1, AllowDayOverflow.no);
    }
}

void main() {
    lastFridays(2012);
}
```


```txt
2012-Jan-27
2012-Feb-24
2012-Mar-30
2012-Apr-27
2012-May-25
2012-Jun-29
2012-Jul-27
2012-Aug-31
2012-Sep-28
2012-Oct-26
2012-Nov-30
2012-Dec-28
```



## Elixir


```elixir
defmodule RC do
  def lastFriday(year) do
    Enum.map(1..12, fn month ->
      lastday = :calendar.last_day_of_the_month(year, month)
      daynum = :calendar.day_of_the_week(year, month, lastday)
      friday = lastday - rem(daynum + 2, 7)
      {year, month, friday}
    end)
  end
end

y = String.to_integer(hd(System.argv))
Enum.each(RC.lastFriday(y), fn {year, month, day} ->
  :io.format "~4b-~2..0w-~2..0w~n", [year, month, day]
end)
```


{{out}}

```txt

2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28

```


## Elm


```elm
import Html exposing (Html, Attribute, text, div, input)
import Html.App exposing (beginnerProgram)
import Html.Attributes exposing (placeholder, value, style)
import Html.Events exposing (onInput)
import String exposing (toInt)
import Maybe exposing (withDefault)
import List exposing (map, map2)
import List.Extra exposing (scanl1)

type Msg = SetYear String

lastFridays : Int -> List Int
lastFridays year =
  let isLeap = (year % 400) == 0 || ( (year % 4) == 0 && (year % 100) /= 0 )
      daysInMonth = [31, if isLeap then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
      y = year-1
  in scanl1 (+) daysInMonth
     |> map2 (\len day -> len - (day + 2 + y + y//4 - y//100 + y//400) % 7) daysInMonth

lastFridayStrings : String -> List String
lastFridayStrings yearString =
  let months= ["January ", "February ", "March ", "April ", "May ", "June ", "July ", "August ", "September ", "October ", "November ", "December "]
      errString = "Only years after 1752 are valid."
  in case toInt yearString of
       Ok year ->
           if (year < 1753)
           then [errString]
           else lastFridays year
                |> map2 (\m d -> m ++ toString d ++ ", " ++ toString year) months
       Err _ ->
           [errString]

view :  String -> Html Msg
view yearString =
  div []
    ([ input
        [ placeholder "Enter a year."
        , value yearString
        , onInput SetYear
        , myStyle
        ]
        []
     ] ++ (lastFridayStrings yearString
           |> map (\date -> div [ myStyle ] [ text date ]) ))

myStyle : Attribute Msg
myStyle =
  style
    [ ("width", "100%")
    , ("height", "20px")
    , ("padding", "5px 0 0 5px")
    , ("font-size", "1em")
    , ("text-align", "left")
    ]

update : Msg -> String -> String
update msg _ =
    case msg of
        SetYear yearString -> yearString


main =
    beginnerProgram
        { model = ""
        , view = view
        , update = update
        }
```

Link to live demo: http://dc25.github.io/lastFridayOfMonthElm/

Sample run for the year 2003; copied and pasted from web-page:
{{out}}

```txt
January 31, 2003
February 28, 2003
March 28, 2003
April 25, 2003
May 30, 2003
June 27, 2003
July 25, 2003
August 29, 2003
September 26, 2003
October 31, 2003
November 28, 2003
December 26, 2003

```



## Erlang


```Erlang

-module( last_date_each_month ).

-export( [monday/1, tuesday/1, wednesday/1, thursday/1, friday/1, saturday/1, sunday/1] ).

monday( Year ) -> last( Year, 1 ).
tuesday( Year ) -> last( Year, 2 ).
wednesday( Year ) -> last( Year, 3 ).
thursday( Year ) -> last( Year, 4 ).
friday( Year ) -> last( Year, 5 ).
saturday( Year ) -> last( Year, 6 ).
sunday( Year ) -> last( Year, 7 ).



last( Year, Week_day ) ->
    Months = lists:seq( 1, 12 ),
    Months_days = [{X, Y} || X <- Months, Y <- lists:seq(calendar:last_day_of_the_month(Year, X), calendar:last_day_of_the_month(Year, X) - 7, -1), calendar:valid_date(Year, X, Y), calendar:day_of_the_week(Year, X, Y) =:= Week_day],
    [{Year, X, proplists:get_value(X, Months_days)} || X <- Months].

```

{{out}}

```txt

32> [io:fwrite("~B-~2.10.0B-~B~n", [Y,M,D]) || {Y,M,D} <- last_date_each_month:friday(2012)].
2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28

```



## Factor

The <code>last-friday-of-month</code> word in the <code>calendar</code> vocabulary does most of the work. This program expects the year as a command line argument.

```factor
USING: calendar calendar.format command-line io kernel math.parser sequences ;
IN: rosetta-code.last-fridays

(command-line) second string>number <year> 12 iota
[ months time+ last-friday-of-month ] with map
[ timestamp>ymd print ] each
```

{{out}}

```txt

>factor last-fridays.factor 2012
2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28

```



## Fortran


Algorithm: compute day of week for last day of month, then subtract just enough to get to the preceding friday. Do this for each month. To simplify computations further, we only need to compute day of week of january 1st (the others are found by adding month lengths). Since day of week need only be known modulo 7, we do not compute modulo at all except once when subtracting.

```fortran
program fridays
   implicit none
   integer :: days(1:12) = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
   integer :: year, k, y, m
   read *, year
   if (mod(year, 400) == 0 .or. (mod(year, 4) == 0 .and. mod(year, 100) /= 0)) days(2) = 29
   y = year - 1
   k = 44 + y + y/4 + 6*(y/100) + y/400
   do m = 1, 12
      k = k + days(m)
      print "(I4,A1,I2.2,A1,I2)", year, '-', m, '-', days(m) - mod(k, 7)
   end do
end program

```



## FreeBASIC


```FreeBasic
' version 23-06-2015
' compile with: fbc -s console

#Ifndef TRUE        ' define true and false for older freebasic versions
    #Define FALSE 0
    #Define TRUE Not FALSE
#EndIf

Function leapyear(Year_ As Integer) As Integer
    ' from the leapyear entry
    If (Year_ Mod 4) <> 0 Then Return FALSE
    If (Year_ Mod 100) = 0 AndAlso (Year_ Mod 400) <> 0 Then Return FALSE
    Return TRUE

End Function

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

Type month_days
    m_name As String
    days As UByte
End Type

Dim As month_days arr(1 To 12)
Data "January",   31, "February", 28, "March",    31, "April",    30
Data "May",       31, "June",     30, "July",     31, "August",   31
Data "September", 30, "October",  31, "November", 30, "December", 31

Dim As Integer yr, d, i, x
Dim As String keypress

For i = 1 To 12
    With arr(i)
        Read .m_name
        Read .days
    End With
Next

Do

    Do
        Print "For what year do you want to find the last Friday of the month"
        Input "any number below 1800 stops program, year in YYYY format";yr
        ' empty input also stops
        If yr < 1800 Then
            End
        Else
            Exit Do
        End If
    Loop

    Print : Print
    Print "Last Friday of the month for"; yr

    For i = 1 To 12
        d = arr(i).days
        If i = 2 AndAlso leapyear(yr) = TRUE Then d = d + 1
        x = wd(i, d, yr)
        If x <> 5 Then d = d - IIf(x > 5, x - 5, x + 2)
        Print d; " "; arr(i).m_name
    Next

    ' empty key buffer
    While InKey <> "" : keypress = InKey : Wend
    Print : Print
    Print "Find last Friday for a other year [Y|y], anything else stops"
    keypress =""
    While keypress = "" : keypress = InKey : Wend
    If LCase(keypress) <> "y" Then Exit Do
    Print : Print

Loop
End
```

{{out}}

```txt
For what year do you want to find the last Friday of the month
any number below 1800 stops program, year in YYYY format? 2017

Last Friday of the month for 2017
 27 January
 24 February
 31 March
 28 April
 26 May
 30 June
 28 July
 25 August
 29 September
 27 October
 24 November
 29 December
```



## Gambas


```gambas
Public Sub Form_Open()
Dim siYear As Short = InputBox("Please input a year", "Last Friday of each month")
Dim siMonth, siDay As Short
Dim dDay As Date

For siMonth = 1 To 12
  For siDay = 31 DownTo 22
    Try dDay = Date(siYear, siMonth, siDay)
    If Error Then Continue
    If WeekDay(dDay) = 5 Then
      Print Format(dDay, "yyyy-mm-dd");;
      Print Space(6) & Format(dDay, "dddd dd mmmm yyyy")
      Break
    End If
  Next
Next

Me.Close

End
```

Output:

```txt

1925-01-30       Friday 30 January 1925
1925-02-27       Friday 27 February 1925
1925-03-27       Friday 27 March 1925
1925-04-24       Friday 24 April 1925
1925-05-29       Friday 29 May 1925
1925-06-26       Friday 26 June 1925
1925-07-31       Friday 31 July 1925
1925-08-28       Friday 28 August 1925
1925-09-25       Friday 25 September 1925
1925-10-30       Friday 30 October 1925
1925-11-27       Friday 27 November 1925
1925-12-25       Friday 25 December 1925

```



## Go


```go
package main

import (
	"fmt"
	"os"
	"strconv"
	"time"
)

func main() {
	y := time.Now().Year()
	if len(os.Args) == 2 {
		if i, err := strconv.Atoi(os.Args[1]); err == nil {
			y = i
		}
	}
	for m := time.January; m <= time.December; m++ {
		d := time.Date(y, m+1, 1, 0, 0, 0, 0, time.UTC).Add(-24 * time.Hour)
		d = d.Add(-time.Duration((d.Weekday()+7-time.Friday)%7) * 24 * time.Hour)
		fmt.Println(d.Format("2006-01-02"))
	}
}
```

{{out}}

```txt

> ./fridays 2012
2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28

```



## Groovy

Solution: Same as [[Find_last_sunday_of_each_month#Groovy|Find last Sunday of each month]]

Test:

```groovy
def ymd = { it.format('yyyy-MM-dd') }
def lastFridays = lastWeekDays.curry(Day.Fri)
lastFridays(args[0] as int).each { println (ymd(it)) }
```


Execution (Cygwin on Windows 7):

```txt
[2273] groovy lastFridays.groovy 2012
```


{{out}}

```txt
2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28
```



## Haskell


```Haskell
import Data.Time.Calendar
       (Day, addDays, showGregorian, fromGregorian, gregorianMonthLength)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.List (transpose, intercalate)

-- [1 .. 7] for [Mon .. Sun]
findWeekDay :: Int -> Day -> Day
findWeekDay dayOfWeek date =
  head
    (filter
       (\x ->
           let (_, _, day) = toWeekDate x
           in day == dayOfWeek)
       ((`addDays` date) <$> [-6 .. 0]))

weekDayDates :: Int -> Integer -> [String]
weekDayDates dayOfWeek year =
  ((showGregorian . findWeekDay dayOfWeek) .
   (fromGregorian year <*> gregorianMonthLength year)) <$>
  [1 .. 12]

main :: IO ()
main =
  mapM_
    putStrLn
    (intercalate "  " <$> transpose (weekDayDates 5 <$> [2012 .. 2017]))
```

{{Out}}

```txt
2012-01-27  2013-01-25  2014-01-31  2015-01-30  2016-01-29  2017-01-27
2012-02-24  2013-02-22  2014-02-28  2015-02-27  2016-02-26  2017-02-24
2012-03-30  2013-03-29  2014-03-28  2015-03-27  2016-03-25  2017-03-31
2012-04-27  2013-04-26  2014-04-25  2015-04-24  2016-04-29  2017-04-28
2012-05-25  2013-05-31  2014-05-30  2015-05-29  2016-05-27  2017-05-26
2012-06-29  2013-06-28  2014-06-27  2015-06-26  2016-06-24  2017-06-30
2012-07-27  2013-07-26  2014-07-25  2015-07-31  2016-07-29  2017-07-28
2012-08-31  2013-08-30  2014-08-29  2015-08-28  2016-08-26  2017-08-25
2012-09-28  2013-09-27  2014-09-26  2015-09-25  2016-09-30  2017-09-29
2012-10-26  2013-10-25  2014-10-31  2015-10-30  2016-10-28  2017-10-27
2012-11-30  2013-11-29  2014-11-28  2015-11-27  2016-11-25  2017-11-24
2012-12-28  2013-12-27  2014-12-26  2015-12-25  2016-12-30  2017-12-29
```


=={{header|Icon}} and {{header|Unicon}}==
This will write the last fridays for every year given as an argument.  There is no error checking on the year.

```Icon
procedure main(A)
every write(lastfridays(!A))
end

procedure lastfridays(year)
every m := 1 to 12 do {
   d := case m of {
      2        : if IsLeapYear(year) then 29 else 28
      4|6|9|11 : 30
      default  : 31
      }                          # last day of month

   z := 0
   j := julian(m,d,year) + 1     # first day of next month
   until (j-:=1)%7 = 4 do z -:=1 # backup to last friday=4
   suspend sprintf("%d-%d-%d",year,m,d+z)
   }
end

link datetime, printf
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides formatting]
[http://www.cs.arizona.edu/icon/library/src/procs/datetime.icn datetime.icn provides julian and IsLeapYear]

{{out}}

```txt
last_fridays.exe 2012
2012-1-27
2012-2-24
2012-3-30
2012-4-27
2012-5-25
2012-6-29
2012-7-27
2012-8-31
2012-9-28
2012-10-26
2012-11-30
2012-12-28
```



## J



```j
require 'dates'
last_fridays=: 12 {. [: ({:/.~ }:"1)@(#~ 5 = weekday)@todate (i.366) + todayno@,&1 1
```


In other words, start from January 1 of the given year, and count forward for 366 days, keeping the Fridays. Then pick the last remaining day within each represented month (which will be a Friday because we only kept the Fridays).  Then pick the first 12 (since on a non-leap year which ends on a Thursday we would get an extra Friday).

Example use:


```j
   last_fridays 2012
2012  1 27
2012  2 24
2012  3 30
2012  4 27
2012  5 25
2012  6 29
2012  7 27
2012  8 31
2012  9 28
2012 10 26
2012 11 30
2012 12 28
```



## Java

{{works with|Java|1.5+}}

```java5
import java.text.*;
import java.util.*;

public class LastFridays {

    public static void main(String[] args) throws Exception {
        int year = Integer.parseInt(args[0]);
        GregorianCalendar c = new GregorianCalendar(year, 0, 1);

        for (String mon : new DateFormatSymbols(Locale.US).getShortMonths()) {
            if (!mon.isEmpty()) {
                int totalDaysOfMonth = c.getActualMaximum(Calendar.DAY_OF_MONTH);
                c.set(Calendar.DAY_OF_MONTH, totalDaysOfMonth);

                int daysToRollBack = (c.get(Calendar.DAY_OF_WEEK) + 1) % 7;

                int day = totalDaysOfMonth - daysToRollBack;
                c.set(Calendar.DAY_OF_MONTH, day);

                System.out.printf("%d %s %d\n", year, mon, day);

                c.set(year, c.get(Calendar.MONTH) + 1, 1);
            }
        }
    }
}
```

{{out}} (for <code>java LastFridays 2012</code>):

```txt
2012 Jan 27
2012 Feb 24
2012 Mar 30
2012 Apr 27
2012 May 25
2012 Jun 29
2012 Jul 27
2012 Aug 31
2012 Sep 28
2012 Oct 26
2012 Nov 30
2012 Dec 28
```



==JavaScript==

### ES5


### =Iteration=

{{works with|Nodejs}}

```javascript
var last_friday_of_month, print_last_fridays_of_month;

last_friday_of_month = function(year, month) {
  var i, last_day;
  i = 0;
  while (true) {
    last_day = new Date(year, month, i);
    if (last_day.getDay() === 5) {
      return last_day.toDateString();
    }
    i -= 1;
  }
};

print_last_fridays_of_month = function(year) {
  var month, results;
  results = [];
  for (month = 1; month <= 12; ++month) {
    results.push(console.log(last_friday_of_month(year, month)));
  }
  return results;
};

(function() {
  var year;
  year = parseInt(process.argv[2]);
  return print_last_fridays_of_month(year);
})();
```

{{Out}}

```txt
>node lastfriday.js  2015
Fri Jan 30 2015
Fri Feb 27 2015
Fri Mar 27 2015
Fri Apr 24 2015
Fri May 29 2015
Fri Jun 26 2015
Fri Jul 31 2015
Fri Aug 28 2015
Fri Sep 25 2015
Fri Oct 30 2015
Fri Nov 27 2015
Fri Dec 25 2015
```




### =Functional composition=


```JavaScript
(function () {
    'use strict';

    // lastFridaysOfYear :: Int -> [Date]
    function lastFridaysOfYear(y) {
        return lastWeekDaysOfYear(y, days.friday);
    }

    // lastWeekDaysOfYear :: Int -> Int -> [Date]
    function lastWeekDaysOfYear(y, iWeekDay) {
        return [
                31,
                0 === y % 4 && 0 !== y % 100 || 0 === y % 400 ? 29 : 28,
                31, 30, 31, 30, 31, 31, 30, 31, 30, 31
            ]
            .map(function (d, m) {
                var dte = new Date(Date.UTC(y, m, d));

                return new Date(Date.UTC(
                    y, m, d - (
                        (dte.getDay() + (7 - iWeekDay)) % 7
                    )
                ));
            });
    }

    // isoDateString :: Date -> String
    function isoDateString(dte) {
        return dte.toISOString()
            .substr(0, 10);
    }

    // range :: Int -> Int -> [Int]
    function range(m, n) {
        return Array.apply(null, Array(n - m + 1))
            .map(function (x, i) {
                return m + i;
            });
    }

    // transpose :: [[a]] -> [[a]]
    function transpose(lst) {
        return lst[0].map(function (_, iCol) {
            return lst.map(function (row) {
                return row[iCol];
            });
        });
    }

    var days = {
        sunday: 0,
        monday: 1,
        tuesday: 2,
        wednesday: 3,
        thursday: 4,
        friday: 5,
        saturday: 6
    }

    // TEST
    return transpose(
            range(2012, 2016)
            .map(lastFridaysOfYear)
        )
        .map(function (row) {
            return row
                .map(isoDateString)
                .join('\t');
        })
        .join('\n');
})();
```


{{Out}}

```txt
2012-01-27	2013-01-25	2014-01-31	2015-01-30	2016-01-29
2012-02-24	2013-02-22	2014-02-28	2015-02-27	2016-02-26
2012-03-30	2013-03-29	2014-03-28	2015-03-27	2016-03-25
2012-04-27	2013-04-26	2014-04-25	2015-04-24	2016-04-29
2012-05-25	2013-05-31	2014-05-30	2015-05-29	2016-05-27
2012-06-29	2013-06-28	2014-06-27	2015-06-26	2016-06-24
2012-07-27	2013-07-26	2014-07-25	2015-07-31	2016-07-29
2012-08-31	2013-08-30	2014-08-29	2015-08-28	2016-08-26
2012-09-28	2013-09-27	2014-09-26	2015-09-25	2016-09-30
2012-10-26	2013-10-25	2014-10-31	2015-10-30	2016-10-28
2012-11-30	2013-11-29	2014-11-28	2015-11-27	2016-11-25
2012-12-28	2013-12-27	2014-12-26	2015-12-25	2016-12-30
```



### ES6



```JavaScript
(() => {
    'use strict'

    // lastWeekDaysOfYear :: Int -> Int -> [Date]
    const lastWeekDaysOfYear = (iWeekDay, y) => [
            31,
            0 === y % 4 && 0 !== y % 100 || 0 === y % 400 ? 29 : 28,
            31, 30, 31, 30, 31, 31, 30, 31, 30, 31
        ]
        .map((d, m) =>
            new Date(Date.UTC(
                y, m, d - ((new Date(Date.UTC(y, m, d))
                    .getDay() + (7 - iWeekDay)) % 7))));

    const days = {
        sunday: 0,
        monday: 1,
        tuesday: 2,
        wednesday: 3,
        thursday: 4,
        friday: 5,
        saturday: 6
    };

    // GENERIC FUNCTIONS

    // curry :: ((a, b) -> c) -> a -> b -> c
    const curry = f => a => b => f(a, b);

    // isoDateString :: Date -> String
    const isoDateString = dte =>
        dte.toISOString()
        .substr(0, 10);

    // range :: Int -> Int -> [Int]
    const range = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // transpose :: [[a]] -> [[a]]
    const transpose = lst =>
        lst[0].map((_, iCol) =>
            lst.map(row => row[iCol]));

    // TEST
    return transpose(
            range(2015, 2019)
            .map(curry(lastWeekDaysOfYear)(days.friday))
        )
        .map(row => row
            .map(isoDateString)
            .join('\t'))
        .join('\n');
})();
```

{{Out}}

```txt
2015-01-30    2016-01-29    2017-01-27    2018-01-26    2019-01-25
2015-02-27    2016-02-26    2017-02-24    2018-02-23    2019-02-22
2015-03-27    2016-03-25    2017-03-31    2018-03-30    2019-03-29
2015-04-24    2016-04-29    2017-04-28    2018-04-27    2019-04-26
2015-05-29    2016-05-27    2017-05-26    2018-05-25    2019-05-31
2015-06-26    2016-06-24    2017-06-30    2018-06-29    2019-06-28
2015-07-31    2016-07-29    2017-07-28    2018-07-27    2019-07-26
2015-08-28    2016-08-26    2017-08-25    2018-08-31    2019-08-30
2015-09-25    2016-09-30    2017-09-29    2018-09-28    2019-09-27
2015-10-30    2016-10-28    2017-10-27    2018-10-26    2019-10-25
2015-11-27    2016-11-25    2017-11-24    2018-11-30    2019-11-29
2015-12-25    2016-12-30    2017-12-29    2018-12-28    2019-12-27
```



## jq

{{ works with|jq|1.4}}
'''Foundations'''

```jq
# In case your jq does not have "until" defined:

def until(cond; next):
  def _until:
    if cond then . else (next|_until) end;
  _until;

# Zeller's Congruence is from [[Day_of_the_week#jq]]

# Use Zeller's Congruence to determine the day of the week, given
# year, month and day as integers in the conventional way.
# If iso == "iso" or "ISO", then emit an integer in 1 -- 7 where
# 1 represents Monday, 2 Tuesday, etc;
# otherwise emit 0 for Saturday, 1 for Sunday, etc.
#
def day_of_week(year; month; day; iso):
  if month == 1 or month == 2 then
    [year - 1, month + 12, day]
  else
    [year, month, day]
  end
  | .[2] + (13*(.[1] + 1)/5|floor)
    +  (.[0]%100)       + ((.[0]%100)/4|floor)
    +  (.[0]/400|floor) - 2*(.[0]/100|floor)
  | if iso == "iso" or iso == "ISO" then 1 + ((. + 5) % 7)
    else . % 7
    end ;
```

'''findLastFridays'''

```jq
# year and month are numbered conventionally
def findLastFriday(year; month):
  def isLeapYear:
    year%4 == 0 and ( year%100!=0 or year%400==0 ) ;
  def days:
    if month == 2 then (if isLeapYear then 29 else 28 end)
    else [31, 28, 31,30,31,30,31,31,30,31,30,31][month-1]
    end;
  year as $year
  | month as $month
  | days
  | until( day_of_week($year; $month; .; null) == 6 ; .-1);

# input: year
def findLastFridays:
  def months:
    ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
  . as $year
  | "YEAR: \(.)",
    (range(0;12) | "\(months[.]) \(findLastFriday($year; .+1))") ;

$year|tonumber|findLastFridays
```

{{out}}

```sh
$ jq --arg year 2012 -n -r -f findLastFridays.jq
YEAR: 2012
January 27
February 24
March 30
April 27
May 25
June 29
July 27
August 31
September 28
October 26
November 30
December 28
```



## Julia


```Julia

isdefined(:Date) || using Dates

const wday = Dates.Fri
const lo = 1
const hi = 12

print("\nThis script will print the last ", Dates.dayname(wday))
println("s of each month of the year given.")
println("(Leave input empty to quit.)")

while true
    print("\nYear> ")
    y = chomp(readline())
    0 < length(y) || break
    y = try
        parseint(y)
    catch
        println("Sorry, but \"", y, "\" does not compute as a year.")
        continue
    end
    println()
    for m in Date(y, lo):Month(1):Date(y, hi)
        println("    ", tolast(m, wday))
    end
end

```


This code uses the <code>Dates</code> module, which is being incorporated into Julian's standard library with the current development version (<tt>0.4</tt>).  I've used <code>isdefined</code> to make this code good for the current stable version (<tt>0.3</tt>) as well as for future releases.  If <code>Dates</code> is not installed on your instance of Julian try <code>Pkg.add("Dates")</code> from the <tt>REPL</tt>.

{{out}}

```txt

This script will print the last Fridays of each month of the year given.
(Leave input empty to quit.)

Year> 2012

    2012-01-27
    2012-02-24
    2012-03-30
    2012-04-27
    2012-05-25
    2012-06-29
    2012-07-27
    2012-08-31
    2012-09-28
    2012-10-26
    2012-11-30
    2012-12-28

Year> this year
Sorry, but "this year" does not compute as a year.

Year>

```



## K


```K

/ List the dates of last Fridays of each month of
/ a given year
/ lastfridt.k

isleap: {(+/~x!' 4 100 400)!2}
wd: {(_jd x)!7}
dom: (31;28;31;30;31;30;31;31;30;31;30;31)
init: {:[isleap x;dom[1]::29;dom[1]::28]}
wdme: {[m;y]; init y; dt:(10000*y)+(100*m)+dom[m-1];jd::(_jd dt);mewd::(wd dt)}
lfd: {[m;y]; wdme[m;y];:[mewd>3;jd::jd+(4-mewd);jd::jd-(3+mewd)];dt:_dj(jd);yy:$(yr:dt%10000);dd:$(d:dt!100);mm:$(mo:((dt-yr*10000)%100));arr::arr,$(yy,"-",(2$mm),"-",(2$dd))}
lfd1: {[y];arr::(); m:1; do[12;lfd[m;y];m+:1]}
main: {[y]; lfd1[y];`0: ,"Dates of last Fridays of ",($y);12 10#arr}


```

The output of a session is given below:

{{out}}

```txt

K Console - Enter \ for help

  \l lastfridt
  main 2012
Dates of last Fridays of 2012
("2012- 1-27"
 "2012- 2-24"
 "2012- 3-30"
 "2012- 4-27"
 "2012- 5-25"
 "2012- 6-29"
 "2012- 7-27"
 "2012- 8-31"
 "2012- 9-28"
 "2012-10-26"
 "2012-11-30"
 "2012-12-28")


```



## Kotlin


```scala
// version 1.0.6

import java.util.*

fun main(args: Array<String>) {
    print("Enter a year : ")
    val year = readLine()!!.toInt()

    println("The last Fridays of each month in $year are as follows:")
    val calendar = GregorianCalendar(year, 0, 31)
    for (month in 1..12) {
        val daysInMonth = calendar.getActualMaximum(Calendar.DAY_OF_MONTH)
        var offset = calendar[Calendar.DAY_OF_WEEK] - Calendar.FRIDAY
        if (offset < 0) offset += 7
        val lastFriday = daysInMonth - offset
        println("$year-" + "%02d-".format(month) + "%02d".format(lastFriday))
        if (month < 12) {
            calendar.add(Calendar.DAY_OF_MONTH, 1)
            calendar.add(Calendar.MONTH, 1)
            calendar.add(Calendar.DAY_OF_MONTH, -1)
        }
    }
}
```


{{out}}

```txt

Enter a year : 2012
The last Fridays of each month in 2012 are as follows:
2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28

```



## Lasso


```Lasso
define isLeapYear(y::integer) => {
	#y % 400 == 0 ? return true
	#y % 100 == 0 ? return false
	#y % 4 == 0 ? return true
	return false
}
define fridays(y::integer) => {
	local(out = array)
	loop(12) => {
		local(last = 28)
		loop_count == 2 && isLeapYear(#y) ? #last = 29
		array(4,6,9,11) >> loop_count ? #last == 30
		#last == 28 && loop_count != 2 ? #last = 31
		local(start = date(-year=#y,-month=loop_count,-day=#last))
		while(#start->dayofweek != 6) => {
			#start->subtract(-day=1)
		}
		#out->insert(#start)
	}
	return #out
}
with f in fridays(2012) do => {^
	#f->format('%Q') + '\r'
^}
```


{{out}}

```txt
2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28
```




## LiveCode


```LiveCode
function lastFriday yyyy
    -- year,month num,day of month,hour in 24-hour time,minute,second,numeric day of week.
    convert the long date to dateitems
    put 1 into item 2 of it
    put 1 into item 3 of it
    put yyyy into item 1 of it
    put it into startDate
    convert startDate to dateItems
    repeat with m = 1 to 12
        put m into item 2 of startDate
        repeat with d = 20 to 31
            put d into item 3 of startDate
            convert startDate to dateItems
            -- 6 is friday
            if item 7 of startDate is 6 and item 1 of startDate is yyyy and item 2 of startDate is m then
                put item 3 of startDate into fridays[item 2 of startDate]
            end if
        end repeat
    end repeat
    combine fridays using cr and space
    sort fridays ascending numeric
    return fridays
end lastFriday
```

Example
```LiveCode
put lastFriday("2012")
```
Output
```LiveCode
1 27
2 24
3 30
4 27
5 25
6 29
7 27
8 31
9 28
10 26
11 30
12 28
```



## Logo


```logo
; Determine if a Gregorian calendar year is leap
to leap? :year
  output (and
    equal? 0 modulo :year 4
    not member? modulo :year 400 [100 200 300]
  )
end

; Convert Gregorian calendar date to a simple day count from
; RD 1 = January 1, 1 CE
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

; Find the day of the week from a day number, 0 = Sunday through 6 = Saturday
to day_of_week :day_number
  output modulo :day_number 7
end

; Find the date of the last Friday of a given month
to last_friday :year :month
  local "zero make "zero day_number :year :month 0
  local "last make "last day_number :year sum 1 :month 0
  local "wday make "wday day_of_week :last
  local "friday make "friday sum :last remainder difference -2 :wday 7
  output difference :friday :zero
end

local "year
make "year ifelse empty? :command.line 2012 :command.line

repeat 12 [
  local "month make "month #
  local "day make "day last_friday :year :month
  if (less? :month 10) [make "month word "0 :month]
  print reduce [(word ?1 "- ?2)] (list :year :month :day)
]
bye
```


{{out}}

```txt
$  logo last_fridays.lg - 2012
2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28
```



## Lua


```lua
function isLeapYear (y)
    return (y % 4 == 0 and y % 100 ~=0) or y % 400 == 0
end

function dayOfWeek (y, m, d)
    local t = os.time({year = y, month = m, day = d})
    return os.date("%A", t)
end

function lastWeekdays (wday, year)
    local monthLength, day = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}
    if isLeapYear(year) then monthLength[2] = 29 end
    for month = 1, 12 do
        day = monthLength[month]
        while dayOfWeek(year, month, day) ~= wday do day = day - 1 end
        print(year .. "-" .. month .. "-" .. day)
    end
end

lastWeekdays("Friday", tonumber(arg[1]))
```

Command line session:

```txt
>lua lastFridays.lua 2012
2012-1-27
2012-2-24
2012-3-30
2012-4-27
2012-5-25
2012-6-29
2012-7-27
2012-8-31
2012-9-28
2012-10-26
2012-11-30
2012-12-28

>
```



## Maple


```Maple
fridays := proc(year)
	local i, dt, change, last_days;
	last_days := [31,28,31,30,31,30,31,31,30,31,30,31];
	if (Calendar:-IsLeapYear(year)) then
		last_days[2] := 28;
	end if;
	for i to 12 do
		dt := Date(year, i, last_days[i]);
		change := 0;
		if not(Calendar:-DayOfWeek(dt) = 6) then
			change := -(Calendar:-DayOfWeek(dt) mod 7)-1;
		end if;
		dt := Calendar:-AdjustDateField(dt, "date", change);
		printf("%d-%d-%d\n", year, Month(dt), DayOfMonth(dt));
	end do;
end proc;

fridays(2012);
```

{{Out|Output}}

```txt
2012-1-27
2012-2-24
2012-3-30
2012-4-27
2012-5-25
2012-6-29
2012-7-27
2012-8-31
2012-9-28
2012-10-26
2012-11-30
2012-12-28
```




## Mathematica


```Mathematica
FridaysOfYear[Y_] :=
 NestWhile[(DaysPlus[#, - 1]) &, #, (DateString[#, "DayName"] != "Friday") &] & /@
  Most@Reverse@NestList [DaysPlus[# /. {x_, y_, X_} -> {x, y, 1}, - 1] &, {Y + 1, 1, 1}, 12]
Column@FridaysOfYear[2012]
```

{{out}}

```txt
{2012,1,27}
{2012,2,24}
{2012,3,30}
{2012,4,27}
{2012,5,25}
{2012,6,29}
{2012,7,27}
{2012,8,31}
{2012,9,28}
{2012,10,26}
{2012,11,30}
{2012,12,28}
```


=={{header|MATLAB}} / {{header|Octave}}==


```Matlab
 function t = last_fridays_of_year(y)
  t1 = datenum([y,1,1,0,0,0]);
  t2 = datenum([y,12,31,0,0,0]);
  t  = datevec(t1:t2);
  t  = t(strmatch('Friday', datestr(t,'dddd')), :);     % find all Fridays
  t  = t([find(diff(t(:,2)) > 0); end], :);     % find Fridays before change of month
  end;

  datestr(last_fridays_of_year(2012),'yyyy-mm-dd')

```


{{out}}

```txt
  ans =
  2012-01-27
  2012-02-24
  2012-03-30
  2012-04-27
  2012-05-25
  2012-06-29
  2012-07-27
  2012-08-31
  2012-09-28
  2012-10-26
  2012-11-30
  2012-12-28
```



## Maxima


```maxima
weekday(year,  month,  day) := block([m: month,  y: year,  k],
   if m < 3 then (m: m + 12,  y: y - 1),
   k: 1 + remainder(day + quotient((m + 1)*26,  10) + y + quotient(y,  4)
        + 6*quotient(y,  100) + quotient(y,  400) + 5,  7),
   ['monday,  'tuesday,  'wednesday,  'thurdsday,  'friday,  'saturday,  'sunday][k]
)$

leapyearp(year) := is(mod(year,  4) = 0 and (mod(year,  100) # 0 or mod(year,  400) = 0))$


lastfridays(year) := block(
   [m: [31,  if leapyearp(year) then 29 else 28,  31,  30,  31,  30,  31,  31,  30,  31,  30,  31],  v: [ ]],
   for month thru 12 do v: endcons(sconcat(year,  "-",  month,  "-",
      lmax(sublist(makelist(i,  i,  1,  m[month]),  lambda([day],  weekday(year,  month,  day) = 'friday)))),  v),
   v
)$

lastfridays(2012);
["2012-1-27", "2012-2-24", "2012-3-30", "2012-4-27", "2012-5-25", "2012-6-29",
"2012-7-27","2012-8-31", "2012-9-28", "2012-10-26", "2012-11-30", "2012-12-28"]
```



## NetRexx

{{trans|Java}}
{{trans|C}}
Implements the algorithms from both the [[#Java|Java]] and [[#C|C]] implementations.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

import java.text.

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method lastFridayByLib(year) public static

  cal = GregorianCalendar(year, 0, 1)

  loop mon over DateFormatSymbols().getShortMonths()
    if \mon.isEmpty() then do
      totalDaysOfMonth = cal.getActualMaximum(Calendar.DAY_OF_MONTH)
      cal.set(Calendar.DAY_OF_MONTH, totalDaysOfMonth)

      daysToRollBack = (cal.get(Calendar.DAY_OF_WEEK) + 1) // 7

      day = totalDaysOfMonth - daysToRollBack
      cal.set(Calendar.DAY_OF_MONTH, day)

      say year.right(4, 0) mon day.right(2, 0)

      cal.set(year, cal.get(Calendar.MONTH) + 1, 1)
      end
    end mon
  return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method lastFridayCalc(year) public static binary signals BadArgumentException

  if year <= 1700 then do
    signal BadArgumentException(year 'is out of range')
    end

  wk  = int
  mth = int
  yr  = int year
  days = [int 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]  -- days in month
  days[1] = days[1] - ((yr // 4) | \(yr // 100) & (yr // 400)) -- adjust for leap year

  wk = yr * 365 + (yr - 1) % 4 - (yr - 1) % 100 + (yr - 1) % 400 + 6 -- week number

  loop mth = 0 to 11
    wk = (wk + days[mth]) // 7
    wx = int
    if wk < 5 then wx = -2
              else wx = 5
    yy = Rexx(yr)
    mm = Rexx(mth + 1)
    dd = Rexx(days[mth] + wx - wk)
    say yy.right(4, 0)'-'mm.right(2, 0)'-'dd.right(2, 0)
    end mth
  return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  do
    parse arg year .
    if year = '' | year = '.' then year = 2012
    dlm = '-'
    dlm = dlm.left(60, dlm)
    say
    say 'Using Java calendar libraries'
    say dlm
    lastFridayByLib(year)
    say
    say 'Calculated'
    say dlm
    lastFridayCalc(year)
  catch ex = Exception
    ex.printStackTrace
  end
  return

```

{{out}}

```txt

Using Java calendar libraries
------------------------------------------------------------
2012 Jan 27
2012 Feb 24
2012 Mar 30
2012 Apr 27
2012 May 25
2012 Jun 29
2012 Jul 27
2012 Aug 31
2012 Sep 28
2012 Oct 26
2012 Nov 30
2012 Dec 28

Calculated
------------------------------------------------------------
2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28

```



## Nim


```nim
import times, os, strutils

var timeinfo = getLocalTime getTime()
timeinfo.year = paramStr(1).parseInt
for month in mJan .. mDec:
  timeinfo.month = month
  for day in countdown(31, 1):
    timeinfo.monthday = day
    let t = getLocalTime(timeInfoToTime timeinfo)
    if t.month == month and t.weekday == dFri:
      echo t.format "yyyy-MM-dd"
      break
```

Sample usage:

```txt
./lastfriday 2012
2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28
```



## OCaml


Using the module [http://caml.inria.fr/pub/docs/manual-ocaml/libref/Unix.html Unix] from the standard OCaml library:


```ocaml
#load "unix.cma"
open Unix

let usage() =
  Printf.eprintf "%s <year>\n" Sys.argv.(0);
  exit 1

let print_date t =
  Printf.printf "%d-%02d-%02d\n" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday

let is_date_ok tm t =
  (tm.tm_year = t.tm_year &&
   tm.tm_mon  = t.tm_mon  &&
   tm.tm_mday = t.tm_mday)

let () =
  let _year =
    try int_of_string Sys.argv.(1)
    with _ -> usage()
  in
  let year = _year - 1900 in
  let fridays = Array.make 12 (Unix.gmtime 0.0) in
  for month = 0 to 11 do
    for day_of_month = 1 to 31 do
      let tm = { (Unix.gmtime 0.0) with
        tm_year = year;
        tm_mon = month;
        tm_mday = day_of_month;
      } in
      let _, t = Unix.mktime tm in
      if is_date_ok tm t  (* check for months that have less than 31 days *)
      && t.tm_wday = 5  (* is a friday *)
      then fridays.(month) <- t
    done;
  done;
  Array.iter print_date fridays
```


{{out}}

```txt
$ ocaml last_fridays.ml 2012
2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28
```



###  With a dedicated library

{{libheader|OCaml Calendar Library}}


```ocaml
open CalendarLib

let usage() =
  Printf.eprintf "%s <year>\n" Sys.argv.(0);
  exit 1

let print_date (year, month, day) =
  Printf.printf "%d-%02d-%02d\n" year month day

let () =
  let year =
    try int_of_string Sys.argv.(1)
    with _ -> usage()
  in
  let fridays = ref [] in
  for month = 1 to 12 do
    let num_days = Date.days_in_month (Date.make_year_month year month) in
    let rec aux day =
      if Date.day_of_week (Date.make year month day) = Date.Fri
      then fridays := (year, month, day) :: !fridays
      else aux (pred day)
    in
    aux num_days
  done;
  List.iter print_date (List.rev !fridays)
```


Run this script with the command:

 ocaml unix.cma str.cma -I +calendar calendarLib.cma last_fridays.ml 2012



## Oforth



```Oforth
import: date

: lastFridays(y)
| m |
   Date.JANUARY Date.DECEMBER for: m [
      Date newDate(y, m, Date.DaysInMonth(y, m))
      while(dup dayOfWeek Date.FRIDAY <>) [ addDays(-1) ]
      println
      ] ;
```


{{out}}

```txt

2012-01-27 00:00:00,000
2012-02-24 00:00:00,000
2012-03-30 00:00:00,000
2012-04-27 00:00:00,000
2012-05-25 00:00:00,000
2012-06-29 00:00:00,000
2012-07-27 00:00:00,000
2012-08-31 00:00:00,000
2012-09-28 00:00:00,000
2012-10-26 00:00:00,000
2012-11-30 00:00:00,000
2012-12-28 00:00:00,000

```



## PARI/GP



```parigp
\\ Normalized Julian Day Number from date
njd(D) =
{
  my (m = D[2], y = D[1]);

  if (D[2] > 2, m++, y--; m += 13);

  (1461 * y) \ 4 + (306001 * m) \ 10000 + D[3] - 694024 + 2 - y \ 100 + y \ 400
}

\\ Date from Normalized Julian Day Number
njdate(J) =
{
  my (a = J + 2415019, b = (4 * a - 7468865) \ 146097, c, d, m, y);

  a += 1 + b - b \ 4 + 1524;
  b = (20 * a - 2442) \ 7305;
  c = (1461 * b) \ 4;
  d = ((a - c) * 10000) \ 306001;
  m = d - 1 - 12 * (d > 13);
  y = b - 4715 - (m > 2);
  d = a - c - (306001 * d) \ 10000;

  [y, m, d]
}

for (m=1, 12, a=njd([2012,m+1,0]); print(njdate(a-(a+1)%7)))
```


Output:
```txt

[2012, 1, 27]
[2012, 2, 24]
[2012, 3, 30]
[2012, 4, 27]
[2012, 5, 25]
[2012, 6, 29]
[2012, 7, 27]
[2012, 8, 31]
[2012, 9, 28]
[2012, 10, 26]
[2012, 11, 30]
[2012, 12, 28]
```



## Perl


```Perl
#!/usr/bin/perl -w
use strict ;
use DateTime ;
use feature qw( say ) ;

foreach my $month ( 1..12 ) {
   my $dt = DateTime->last_day_of_month( year => $ARGV[ 0 ] , month => $month ) ;
   while ( $dt->day_of_week != 5 ) {
      $dt->subtract( days => 1 ) ;
   }
   say $dt->ymd ;
}
```

{{out}}

```txt
2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28

```



## Perl 6


```perl6
sub MAIN (Int $year = Date.today.year) {
    my @fri;
    for Date.new("$year-01-01") .. Date.new("$year-12-31") {
        @fri[.month] = .Str if .day-of-week == 5;
    }
    .say for @fri[1..12];
}
```


Example:

```txt
$ ./lastfri 2038
2038-01-29
2038-02-26
2038-03-26
2038-04-30
2038-05-28
2038-06-25
2038-07-30
2038-08-27
2038-09-24
2038-10-29
2038-11-26
2038-12-31
```


A solution without a result array to store things in:


```perl6
sub MAIN (Int $year = Date.today.year) {
    say ~.value.reverse.first: *.day-of-week == 5
        for classify *.month, Date.new("$year-01-01") .. Date.new("$year-12-31");
}
```


Here, <code>classify</code> sorts the dates into one bin per month (but preserves the order in each bin). We then take the list inside each bin (<code>.value</code>) and find the last (<code>.reverse.first</code>) date which is a Friday.

Another variation where the data flow can be read left to right using feed operators:


```perl6
sub MAIN (Int $year = Date.today.year) {
    .say for Date.new("$year-01-01") .. Date.new("$year-12-31") ==> classify *.month ==>
             map *.value.reverse.first: *.day-of-week == 5
}
```



## Phix


```Phix
include timedate.e

constant FRIDAY=6

procedure showlast(integer dow, integer doy, timedate td)
    td = adjust_timedate(td,timedelta(days:=doy-1))
    integer {year,month,day} = td
    while day_of_week(year,month,day)!=dow do day-=1 end while
    printf(1,"%4d-%02d-%02d\n",{year,month,day})
end procedure

procedure last_day_of_month(integer year, integer dow)
integer doy
timedate first = {year,1,1,0,0,0,0,0}
    -- start by finding the 1st of the next month, less 1
    for i=1 to 11 do
        doy = day_of_year(year,i+1,1)-1
        showlast(dow,doy,first)
    end for
    -- do December separately, as 1st would be next year
    doy = day_of_year(year,12,31)
    showlast(dow,doy,first)
end procedure
last_day_of_month(2012,FRIDAY)
```

{{out}}

```txt

2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28

```




## PHP

PHP is generally used for web apps, so I am not implementing the command-line component of this task.


```PHP
<?php
function last_friday_of_month($year, $month) {
  $day = 0;
  while(True) {
    $last_day = mktime(0, 0, 0, $month+1, $day, $year);
    if (date("w", $last_day) == 5) {
      return date("Y-m-d", $last_day);
    }
    $day -= 1;
  }
}

function print_last_fridays_of_month($year) {
  foreach(range(1, 12) as $month) {
    echo last_friday_of_month($year, $month), "
";
  }
}

date_default_timezone_set("GMT");
$year = 2012;
print_last_fridays_of_month($year);
?>
```


{{out}}

```txt

2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28

```



## PicoLisp


```PicoLisp
(de lastFridays (Y)
   (for M 12
      (prinl
         (dat$
            (find '((D) (= "Friday" (day D)))
               (mapcar '((D) (date Y M D)) `(range 31 22)) )
            "-" ) ) ) )
```

Test:

```PicoLisp
: (lastFridays 2012)
2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28
```



## Pike


```Pike
int(0..1) last_friday(object day)
{
   return day->week_day() == 5 &&
          day->month_day() > day->month()->number_of_days()-7;
}

int main(int argc, array argv)
{
    array days = filter(Calendar.Year((int)argv[1])->months()->days()[*], last_friday);
    write("%{%s\n%}", days->format_ymd());
    return 0;
}
```



## PL/I


```PL/I

Fridays: procedure (year) options (main); /* 8 January 2013 */
   declare year character (4) varying;
   declare start fixed binary (31);
   declare months fixed decimal (2) initial (0);
   declare (current_month, month_one_week_hence) character (2);

   put list ('Last Fridays in each month for the year ' || year || ':' );
   start = days('0101' || year, 'DDMMYYYY');
   /* Find first Friday */
   do while (weekday(start) ^= 6); start = start + 1; end;

   do until (months=12);
      current_month = substr (daystodate(start, 'MMDDYYYY'), 1, 2 );
      month_one_week_hence = substr (daystodate(start+7, 'MMDDYYYY'), 1, 2 );
      if current_month ^= month_one_week_hence then
         do;
            months = months + 1;
            put skip list (daystodate(start, 'DDMmmYYYY'));
         end;
      start = start + 7;
   end;
end Fridays;

```

The command: FRIDAYS /2008 produces:

```txt

Last Fridays in each month for the year 2008:
25Jan2008
29Feb2008
28Mar2008
25Apr2008
30May2008
27Jun2008
25Jul2008
29Aug2008
26Sep2008
31Oct2008
28Nov2008
26Dec2008

```

{{out}} for 2013:

```txt

Last Fridays in each month for the year 2013:
25Jan2013
22Feb2013
29Mar2013
26Apr2013
31May2013
28Jun2013
26Jul2013
30Aug2013
27Sep2013
25Oct2013
29Nov2013
27Dec2013

```



## PowerShell


```PowerShell

function last-dayofweek {
    param(
     [Int][ValidatePattern("[1-9][0-9][0-9][0-9]")]$year,
     [String][validateset('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')]$dayofweek
    )
    $date = (Get-Date -Year $year -Month 1 -Day 1)
    while($date.DayOfWeek -ne $dayofweek) {$date = $date.AddDays(1)}
    while($date.year -eq $year) {
        if($date.Month -ne $date.AddDays(7).Month) {$date.ToString("yyyy-dd-MM")}
        $date = $date.AddDays(7)
    }
}
last-dayofweek 2012 "Friday"

```

<b>Output:</b>

```txt

2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28

```



### Alternate Version

This script finds the first and/or last or all dates of any of the days of week; accepts <code>[Int32]</code> and <code>[DateTime]</code> values for Month and Year parameters; outputs <code>[DateTime]</code> objects by default but has an option to output time strings in various formats.  This script also allows for pipeline input based mainly upon the Month parameter.
This script has a syntax as complex as any PowerShell Cmdlet because it attempts to do everything.

```PowerShell

function Get-Date0fDayOfWeek
{
    [CmdletBinding(DefaultParameterSetName="None")]
    [OutputType([datetime])]
    Param
    (
        [Parameter(Mandatory=$false,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true,
                   Position=0)]
        [ValidateRange(1,12)]
        [int]
        $Month = (Get-Date).Month,

        [Parameter(Mandatory=$false,
                   ValueFromPipelineByPropertyName=$true,
                   Position=1)]
        [ValidateRange(1,9999)]
        [int]
        $Year = (Get-Date).Year,

        [Parameter(Mandatory=$true, ParameterSetName="Sunday")]
        [switch]
        $Sunday,

        [Parameter(Mandatory=$true, ParameterSetName="Monday")]
        [switch]
        $Monday,

        [Parameter(Mandatory=$true, ParameterSetName="Tuesday")]
        [switch]
        $Tuesday,

        [Parameter(Mandatory=$true, ParameterSetName="Wednesday")]
        [switch]
        $Wednesday,

        [Parameter(Mandatory=$true, ParameterSetName="Thursday")]
        [switch]
        $Thursday,

        [Parameter(Mandatory=$true, ParameterSetName="Friday")]
        [switch]
        $Friday,

        [Parameter(Mandatory=$true, ParameterSetName="Saturday")]
        [switch]
        $Saturday,

        [switch]
        $First,

        [switch]
        $Last,

        [switch]
        $AsString,

        [Parameter(Mandatory=$false)]
        [ValidateNotNullOrEmpty()]
        [string]
        $Format = "dd-MMM-yyyy"
    )

    Process
    {
        [datetime[]]$dates = 1..[DateTime]::DaysInMonth($Year,$Month) | ForEach-Object {
            Get-Date -Year $Year -Month $Month -Day $_ -Hour 0 -Minute 0 -Second 0 |
            Where-Object -Property DayOfWeek -Match $PSCmdlet.ParameterSetName
        }

        if ($First -or $Last)
        {
            if ($AsString)
            {
                if ($First) {$dates[0].ToString($Format)}
                if ($Last)  {$dates[-1].ToString($Format)}
            }
            else
            {
                if ($First) {$dates[0]}
                if ($Last)  {$dates[-1]}
            }
        }
        else
        {
            if ($AsString)
            {
                $dates | ForEach-Object {$_.ToString($Format)}
            }
            else
            {
                $dates
            }
        }
    }
}

```

The default is to return <code>[DateTime]</code> objects:

```PowerShell

1..12 | Get-Date0fDayOfWeek -Year 2012 -Last -Friday

```

{{Out}}

```txt

Friday, January 27, 2012 12:00:00 AM
Friday, February 24, 2012 12:00:00 AM
Friday, March 30, 2012 12:00:00 AM
Friday, April 27, 2012 12:00:00 AM
Friday, May 25, 2012 12:00:00 AM
Friday, June 29, 2012 12:00:00 AM
Friday, July 27, 2012 12:00:00 AM
Friday, August 31, 2012 12:00:00 AM
Friday, September 28, 2012 12:00:00 AM
Friday, October 26, 2012 12:00:00 AM
Friday, November 30, 2012 12:00:00 AM
Friday, December 28, 2012 12:00:00 AM

```

Return the <code>[DateTime]</code> objects as strings (using the default string format):

```PowerShell

1..12 | Get-Date0fDayOfWeek -Year 2012 -Last -Friday -AsString

```

{{Out}}

```txt

27-Jan-2012
24-Feb-2012
30-Mar-2012
27-Apr-2012
25-May-2012
29-Jun-2012
27-Jul-2012
31-Aug-2012
28-Sep-2012
26-Oct-2012
30-Nov-2012
28-Dec-2012

```

Return the <code>[DateTime]</code> objects as strings (specifying the string format):

```PowerShell

1..12 | Get-Date0fDayOfWeek -Year 2012 -Last -Friday -AsString -Format yyyy-MM-dd

```

{{Out}}

```txt

2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28

```



## PureBasic


```PureBasic
Procedure LastFridayOfEachMonth(yyyy.i,List lfem.i())
  Define dv.i=ParseDate("%yyyy",Str(yyyy)), mv.i=1
  NewList d.i()
  For d=1 To 365
    dv=AddDate(dv,#PB_Date_Day,1)
    If DayOfWeek(dv)=5
      AddElement(d()) : d()=dv
    EndIf
  Next
  dv=0
  For mv=1 To 12
    ForEach d()
      If dv<d() And Month(d())=mv
        dv=d()
      EndIf
    Next
    AddElement(lfem()) : lfem()=dv
  Next
EndProcedure

NewList lf.i()
Define y.i
OpenConsole("Last Friday of each month")
Print("Input Year [ 1971 < y < 2038 ]: ")
y=Val(Input())
If y>1971 And y<2038
  PrintN("Last Friday of each month...")
  LastFridayOfEachMonth(y,lf())
  ForEach lf()
    PrintN(FormatDate("%dd.%mm.%yyyy",lf()))
  Next
EndIf
Print("...End")
Input()
```

{{out}}

```txt
Input Year [ 1971 < y < 2038 ]: 2017
Last Friday of each month...
27.01.2017
24.02.2017
31.03.2017
28.04.2017
26.05.2017
30.06.2017
28.07.2017
25.08.2017
29.09.2017
27.10.2017
24.11.2017
29.12.2017
...End
```



## Python


```python
import calendar

def last_fridays(year):
    for month in range(1, 13):
        last_friday = max(week[calendar.FRIDAY]
            for week in calendar.monthcalendar(year, month))
        print('{:4d}-{:02d}-{:02d}'.format(year, month, last_friday))
```


{{out}}

```txt
>>> last_fridays(2012)
2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28
```

Another solution

```python
import calendar
c=calendar.Calendar()
fridays={}
year=raw_input("year")
for item in c.yeardatescalendar(int(year)):
    for i1 in item:
        for i2 in i1:
            for i3 in i2:
                if "Fri" in i3.ctime() and year in i3.ctime():
                    month,day=str(i3).rsplit("-",1)
                    fridays[month]=day

for item in sorted((month+"-"+day for month,day in fridays.items()),
                   key=lambda x:int(x.split("-")[1])):
    print item
```


Using reduce


```python
import calendar
c=calendar.Calendar()
fridays={}
year=raw_input("year")
add=list.__add__
for day in reduce(add,reduce(add,reduce(add,c.yeardatescalendar(int(year))))):

    if "Fri" in day.ctime() and year in day.ctime():
        month,day=str(day).rsplit("-",1)
        fridays[month]=day

for item in sorted((month+"-"+day for month,day in fridays.items()),
                   key=lambda x:int(x.split("-")[1])):
    print item
```


using itertools


```python
import calendar
from itertools import chain
f=chain.from_iterable
c=calendar.Calendar()
fridays={}
year=raw_input("year")
add=list.__add__

for day in f(f(f(c.yeardatescalendar(int(year))))):

    if "Fri" in day.ctime() and year in day.ctime():
        month,day=str(day).rsplit("-",1)
        fridays[month]=day

for item in sorted((month+"-"+day for month,day in fridays.items()),
                   key=lambda x:int(x.split("-")[1])):
    print item
```



## R


```rsplus
year = commandArgs(T)
d = as.Date(paste0(year, "-01-01"))
fridays = d + seq(by = 7,
    (5 - as.POSIXlt(d)$wday) %% 7,
    364 + (months(d + 30 + 29) == "February"))
message(paste(collapse = "\n", fridays[tapply(
    seq_along(fridays), as.POSIXlt(fridays)$mon, max)]))
```



## Racket


```racket

#lang racket
(require srfi/19 math)

(define (days-in-month m y)
  (define lengths #(0 31 #f 31 30 31 30 31 31 30 31 30 31))
  (define d (vector-ref lengths m))
  (or d (days-in-feb y)))

(define (leap-year? y)
  (and (divides? 4 y)
       (or (not (divides? 100 y))
           (divides? 400 y))))

(define (days-in-feb y)
  (if (leap-year? y) 29 28))

(define (last-day-in-month m y)
  (make-date 0 0 0 0 (days-in-month m y) m y 0))

(define (week-day date)
  (define days #(sun mon tue wed thu fri sat))
  (vector-ref days (date-week-day date)))

(define (last-fridays y)
  (for/list ([m (in-range 1 13)])
    (prev-friday (last-day-in-month m y))))

(define 24hours (make-time time-duration 0 (* 24 60 60)))

(define (prev-day d)
  (time-utc->date
   (subtract-duration
    (date->time-utc d) 24hours)))

(define (prev-friday d)
  (if (eq? (week-day d) 'fri)
      d
      (prev-friday (prev-day d))))

(for ([d (last-fridays 2012)])
  (displayln (~a (date->string d "~a ~d ~b ~Y"))))

```

{{out}}

```txt

Fri 27 Jan 2012
Fri 24 Feb 2012
Fri 30 Mar 2012
Fri 27 Apr 2012
Fri 25 May 2012
Fri 29 Jun 2012
Fri 27 Jul 2012
Fri 31 Aug 2012
Fri 28 Sep 2012
Fri 26 Oct 2012
Fri 30 Nov 2012
Fri 28 Dec 2012

```



## REBOL

The longer version:

```REBOL
leap-year?:  function [year] [to-logic attempt [to-date reduce [29 2 year]]]

days-in-feb: function [year] [either leap-year? year [29] [28]]

days-in-month: function [month year] [
    do pick [31 (days-in-feb year) 31 30 31 30 31 31 30 31 30 31] month
]

last-day-of-month: function [month year] [
    to-date reduce [year month  days-in-month month year]
]

last-weekday-of-month: function [weekday month year] [
    d: last-day-of-month month year
    while [d/weekday != weekday] [d/day: d/day - 1]
    d
]

last-friday-of-month: function [month year] [last-weekday-of-month 5 month year]

year: to-integer input
repeat month 12 [print last-friday-of-month month year]

```

{{out}}

```txt
rebol last-fridays.reb <<< 2012
27-Jan-2012
24-Feb-2012
30-Mar-2012
27-Apr-2012
25-May-2012
29-Jun-2012
27-Jul-2012
31-Aug-2012
28-Sep-2012
26-Oct-2012
30-Nov-2012
28-Dec-2012

```

A shorter version:

```REBOL
last-fridays-of-year: function [year] [
    collect [
        repeat month 12 [
            d: to-date reduce [1 month year]
            d/month: d/month + 1                      ; start of next month
            until [d/day: d/day - 1  d/weekday = 5]   ; go backwards until find a Friday
            keep d
        ]
    ]
]

foreach friday last-fridays-of-year to-integer input [print friday]

```

NB. See "Find the last Sunday of each month" Rosetta for alternative (even more succinct) solution


## REXX

This REXX program will find the last day-of-week (for any day) of all the months for any year.

It wasn't optimized just to find a particular day-of-week.

The documentation for the  '''lastDOW'''  function   (used in the REXX program below):

```txt

      ╔════════════════════════════════════════════════════════════════════╗
      ║ lastDOW:  procedure to return the date of the  last day─of─week of ║
      ║           any particular month  of any particular year.            ║
      ║                                                                    ║
      ║ The  day─of─week  must be specified (it can be in any case,        ║
      ║ (lower─/mixed─/upper─case)  as an English name of the spelled day  ║
      ║ of the week,   with a minimum length that causes no ambiguity.     ║
      ║ I.E.:   W  for Wednesday,   Sa  for Saturday,   Su  for Sunday ... ║
      ║                                                                    ║
      ║ The month can be specified as an integer   1 ──► 12                ║
      ║    1=January     2=February     3=March     ...     12=December    ║
      ║ or the English  name  of the month,  with a minimum length that    ║
      ║ causes no ambiguity.    I.E.:  Jun  for June,   D  for December.   ║
      ║ If omitted  [or an asterisk(*)],  the current month is used.       ║
      ║                                                                    ║
      ║ The year is specified as an integer or just the last two digits    ║
      ║ (two digit years are assumed to be in the current century,  and    ║
      ║ there is no windowing for a two─digit year).                       ║
      ║ If omitted  [or an asterisk(*)],  the current year is used.        ║
      ║ Years < 100   must be specified with  (at least 2)  leading zeroes.║
      ║                                                                    ║
      ║ Method used:  find the "day number" of the 1st of the next month   ║
      ║ then subtract one  (this gives the "day number" of the last day of ║
      ║ the month,  bypassing the leapday mess).   The last day─of─week is ║
      ║ then obtained straightforwardly,   or  via subtraction.            ║
      ╚════════════════════════════════════════════════════════════════════╝

```


```rexx
/*REXX program displays the dates of the  last Fridays of each month for any given year.*/
parse arg yyyy
                 do j=1  for 12
                 say lastDOW('Friday', j, yyyy)  /*find last Friday for the  Jth  month.*/
                 end  /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
lastDOW: procedure;  arg dow .,mm .,yy .;     parse arg a.1,a.2,a.3  /*DOW = day of week*/
if mm=='' | mm=='*'  then mm=left( date('U'), 2)                     /*use default month*/
if yy=='' | yy=='*'  then yy=left( date('S'), 4)                     /*use default year */
if length(yy)==2     then yy=left( date('S'), 2)yy                   /*append century.  */
                                         /*Note mandatory leading blank in strings below*/
$=" Monday TUesday Wednesday THursday Friday SAturday SUnday"
!=" JAnuary February MARch APril MAY JUNe JULy AUgust September October November December"
upper $ !                                                            /*uppercase strings*/
if dow==''                 then call .er "wasn't specified",1
if arg()>3                 then call .er 'arguments specified',4

  do j=1  for 3                                                      /*any plural args ?*/
  if words(arg(j))>1       then call .er 'is illegal:',j
  end

dw=pos(' 'dow,$)                                                     /*find  day-of-week*/
if dw==0                   then call .er 'is invalid:',1
if dw\==lastpos(' 'dow,$)  then call .er 'is ambigious:',1

if datatype(mm,'M')  then                                            /*is MM alphabetic?*/
  do
  m=pos(' 'mm,!)                                                     /*maybe its good...*/
  if m==0                  then call .er 'is invalid:',1
  if m\==lastpos(' 'mm,!)  then call .er 'is ambigious:',2
  mm=wordpos( word( substr(!, m), 1), !) - 1                         /*now, use true Mon*/
  end

if \datatype(mm,'W')       then call .er "isn't an integer:",2
if \datatype(yy,'W')       then call .er "isn't an integer:",3
if mm<1 | mm>12            then call .er "isn't in range 1──►12:",2
if yy=0                    then call .er "can't be 0 (zero):",3
if yy<0                    then call .er "can't be negative:",3
if yy>9999                 then call .er "can't be > 9999:",3

tdow=wordpos(word(substr($,dw),1),$)-1                               /*target DOW, 0──►6*/
                                                                     /*day# of last dom.*/
_=date('B',right(yy+(mm=12),4)right(mm//12+1,2,0)"01",'S')-1
?=_ // 7                                                             /*calc. DOW,  0──►6*/
if ?\==tdow  then _=_ - ? - 7 + tdow + 7 * (?>tdow)                  /*not DOW?  Adjust.*/
return date('weekday', _, "B")    date(, _, 'B')                     /*return the answer*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
.er: arg ,_;  say;   say '***error*** (in LASTDOW)';   say           /*tell error,  and */
     say word('day-of-week month year excess',arg(2)) arg(1) a._
     say;     exit 13                                                /*... then exit.   */
```

'''output'''   when using the following input:   <tt> 2012 </tt>     or     <tt> 12 </tt>

```txt

Friday 27 Jan 2012
Friday 24 Feb 2012
Friday 30 Mar 2012
Friday 27 Apr 2012
Friday 25 May 2012
Friday 29 Jun 2012
Friday 27 Jul 2012
Friday 31 Aug 2012
Friday 28 Sep 2012
Friday 26 Oct 2012
Friday 30 Nov 2012
Friday 28 Dec 2012

```



## Ring


```ring

see "What year to calculate (yyyy) : "
give year
see "Last Friday in " + year + " are on :" + nl
month = list(12)
mo = [4,0,0,3,5,1,3,6,2,4,0,2]
mon = [31,28,31,30,31,30,31,31,30,31,30,31]
if year < 2100 leap = year - 1900 else leap = year - 1904 ok
m = ((year-1900)%7) + floor(leap/4) % 7
for n = 1 to 12
    month[n] = (mo[n] + m) % 7
next
for n = 1 to 12
     for i = (mon[n] - 6) to mon[n]
         if year%4 = 0 and n<3
            x = (month[n] + i) % 7 - 1
         else x = (month[n] + i) % 7 ok
         if  n < 10 strn = "0" + string(n) else strn = string(n) ok
         if x = 2 see year + "-" + strn + "-" + string(i) + nl ok
     next
next


```

Output:

```txt

What year to calculate (yyyy) : 2012
Last Fridays in 2012 are on :
2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28

```



## Ruby


```ruby
require 'date'

def last_friday(year, month)
  # Last day of month: Date.new interprets a negative number as a relative month/day from the end of year/month.
  d = Date.new(year, month, -1)
  d -= (d.wday - 5) % 7  # Subtract days after Friday.
end

year = Integer(ARGV.shift)
(1..12).each {|month| puts last_friday(year, month)}
```


Friday is <code>d.wday == 5</code>; the expression <code>(d.wday - 5) % 7</code> counts days after Friday.
{{out}}

```txt

2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28

```


Or get the last day of the month and go to the previous day until it's a Friday.

```ruby
require 'date'

def last_friday(year, month)
  d = Date.new(year, month, -1)
  d = d.prev_day until d.friday?
  d
end

```



## Run BASIC


```runbasic
input "Year:";yr
dayOne$ = "01-01-";yr
n1	= date$(dayOne$)
for i = 1 to 12
  n1  = n1 + 26
  m1$ = left$(date$(n1),2)
  while  m1$ = left$(date$(n1),2) ' find end of month
    n1 = n1 + 1
  wend
  n1 = n1 -1
  while (n1 Mod 7) <> 3 	  ' find Friday
    n1 = n1 - 1
  wend
  print date$(n1)		  ' print last Friday's date
next i
```


```txt
Year:?2013
01/25/2013
02/22/2013
03/29/2013
04/26/2013
05/31/2013
06/28/2013
07/26/2013
08/30/2013
09/27/2013
10/25/2013
11/29/2013
12/27/2013
```



## Scala


```scala
import java.util.Calendar
import java.text.SimpleDateFormat

object Fridays {

  def lastFridayOfMonth(year:Int, month:Int)={
    val cal=Calendar.getInstance
    cal.set(Calendar.YEAR, year)
    cal.set(Calendar.MONTH, month)
    cal.set(Calendar.DAY_OF_WEEK, Calendar.FRIDAY)
    cal.set(Calendar.DAY_OF_WEEK_IN_MONTH, -1)
    cal.getTime
  }

  def fridaysOfYear(year:Int)=for(month <- 0 to 11) yield lastFridayOfMonth(year, month)

  def main(args:Array[String]){
    val year=args(0).toInt
    val formatter=new SimpleDateFormat("yyyy-MMM-dd")
    fridaysOfYear(year).foreach{date=>
      println(formatter.format(date))
    }
  }
}
```

{{out}}

```txt
2012-Jan-27
2012-Feb-24
2012-Mrz-30
2012-Apr-27
2012-Mai-25
2012-Jun-29
2012-Jul-27
2012-Aug-31
2012-Sep-28
2012-Okt-26
2012-Nov-30
2012-Dez-28
```



## Seed7

Uses the libraries [http://seed7.sourceforge.net/libraries/time.htm time.s7i] and
[http://seed7.sourceforge.net/libraries/duration.htm duration.s7i].
Applicable to any day of the week, cf. [[http://rosettacode.org/wiki/Find_last_sunday_of_each_month#Seed7]].


```seed7
$ include "seed7_05.s7i";
  include "time.s7i";
  include "duration.s7i";

const proc: main is func
  local
    var integer: weekday is 1; # 1 for monday, 2 for tuesday, and so on up to 7 for sunday.
    var integer: year is 0;
    var integer: month is 1;
    var time: aDate is time.value;
    var time: selected is time.value;
  begin
    if length(argv(PROGRAM)) <> 2 then
      writeln("usage: lastWeekdayInMonth weekday year");
      writeln("  weekday: 1 for monday, 2 for tuesday, and so on up to 7 for sunday.");
    else
      weekday := integer parse (argv(PROGRAM)[1]);
      year := integer parse (argv(PROGRAM)[2]);
      for month range 1 to 12 do
        aDate := date(year, month, 1);
        while aDate.month = month do
          if dayOfWeek(aDate) = weekday then
            selected := aDate;
          end if;
          aDate +:= 1 . DAYS;
        end while;
        writeln(strDate(selected));
      end for;
    end if;
  end func;
```


{{out}} when called with <tt>s7 rosetta/lastWeekdayInMonth 5 2013</tt>:

```txt

2013-01-25
2013-02-22
2013-03-29
2013-04-26
2013-05-31
2013-06-28
2013-07-26
2013-08-30
2013-09-27
2013-10-25
2013-11-29
2013-12-27

```



## Sidef

{{trans|Perl}}

```ruby
require('DateTime')
var (year=2016) = ARGV.map{.to_i}...
 
for month (1..12) {
   var dt = %O<DateTime>.last_day_of_month(year => year, month => month)
   while (dt.day_of_week != 5) {
      dt.subtract(days => 1)
   }
   say dt.ymd
}
```

{{out}}

```txt

$ sidef lastfriday.sf 2012
2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28

```



## Smalltalk



```Smalltalk

Pharo Smalltalk

[ :yr | | firstDay firstFriday |
  firstDay := Date year: yr month: 1 day: 1.
  firstFriday := firstDay addDays: (6 - firstDay dayOfWeek).
  (0 to: 53)
    collect: [ :each | firstFriday addDays: (each * 7) ]
    thenSelect: [ :each |
      (((Date daysInMonth: each monthIndex forYear: yr) - each dayOfMonth) <= 6) and: [	each year = yr ] ] ]

```

{{out}}

```txt

Send value: 2012 to the above block to return an array:
(27 January 2012 24 February 2012 30 March 2012 27 April 2012 25 May 2012 29 June 2012 27 July 2012 31 August 2012 28 September 2012 26 October 2012 30 November 2012 28 December 2012)

```



## SQL


```SQL

select to_char( next_day( last_day( add_months( to_date(
        :yr||'01','yyyymm' ),level-1))-7,'Fri') ,'yyyy-mm-dd Dy') lastfriday
from dual
connect by level <= 12;

```


```txt

LASTFRIDAY
-----------------------
2012-01-27 Fri
2012-02-24 Fri
2012-03-30 Fri
2012-04-27 Fri
2012-05-25 Fri
2012-06-29 Fri
2012-07-27 Fri
2012-08-31 Fri
2012-09-28 Fri
2012-10-26 Fri
2012-11-30 Fri
2012-12-28 Fri

12 rows selected.

```



## Stata


```stata
program last_fridays
	args year
	clear
	qui set obs 12
	gen day=dofm(mofd(mdy(_n,1,`year'))+1)-1
	qui replace day=day-mod(dow(day)-5,7)
	format %td day
	list, noobs noheader sep(6)
end

last_fridays 2012

  +-----------+
  | 27jan2012 |
  | 24feb2012 |
  | 30mar2012 |
  | 27apr2012 |
  | 25may2012 |
  | 29jun2012 |
  |-----------|
  | 27jul2012 |
  | 31aug2012 |
  | 28sep2012 |
  | 26oct2012 |
  | 30nov2012 |
  | 28dec2012 |
  +-----------+
```



## Swift


```Swift

import Foundation

func lastFridays(of year: Int) -> [Date] {

	let calendar = Calendar.current
	var dates = [Date]()

	for month in 2...13 {

		let lastDayOfMonth = DateComponents(calendar: calendar,
		                                    year: year,
		                                    month: month,
		                                    day: 0,
		                                    hour: 12)

		let date = calendar.date(from: lastDayOfMonth)!

		let isFriday = calendar.component(.weekday, from: date) == 6

		if isFriday {

			dates.append(calendar.date(from: lastDayOfMonth)!)

		} else {

			let lastWeekofMonth = calendar.ordinality(of: .weekOfMonth,
			                                          in: .month,
			                                          for: date)!

			let lastWithFriday = lastWeekofMonth - (calendar.component(.weekday, from: date) > 6 ? 0 : 1)

			let lastFridayOfMonth = DateComponents(calendar: calendar,
			                                       year: year,
			                                       month: month - 1,
			                                       hour: 12,
			                                       weekday: 6,
			                                       weekOfMonth: lastWithFriday)

			dates.append(calendar.date(from: lastFridayOfMonth)!)
		}
	}
	return dates
}

var dateFormatter = DateFormatter()
dateFormatter.dateStyle = .short

print(lastFridays(of: 2013).map(dateFormatter.string).joined(separator: "\n"))

```


```txt

1/27/12
2/24/12
3/30/12
4/27/12
5/25/12
6/29/12
7/27/12
8/31/12
9/28/12
10/26/12
11/30/12
12/28/12

```



## Tcl


```tcl
package require Tcl 8.5
set year [lindex $argv 0]
foreach dm {02/1 03/1 04/1 05/1 06/1 07/1 08/1 09/1 10/1 11/1 12/1 12/32} {
    # The [clock scan] code is unhealthily clever; use it for our own evil purposes
    set t [clock scan "last friday" -base [clock scan $dm/$year -gmt 1] -gmt 1]
    # Print the interesting part
    puts [clock format $t -format "%Y-%m-%d" -gmt 1]
}
```

Sample execution:

```txt

$ tclsh8.5 lastfri.tcl 2012
2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28

```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
year=2012
LOOP month=1,12
 LOOP day=31,22,-1
  dayofweek=DATE (number,day,month,year,nummer)
  IF (dayofweek==5) THEN
  PRINT year,"-",month,"-",day
  EXIT
  ENDIF
 ENDLOOP
ENDLOOP

```

{{out}}
<pre style='height:30ex;overflow:scroll'>
2012-1-27
2012-2-24
2012-3-30
2012-4-27
2012-5-25
2012-6-29
2012-7-27
2012-8-31
2012-9-28
2012-10-26
2012-11-30
2012-12-28

```



## UNIX Shell

Using <code>ncal</code>.  Will switch to Julian calender as ncal sees fit, and will not calculate past year 9999 (chances are you'll be too dead by then to worry about weekends anyway).

```bash
#!/bin/sh

if [ -z $1 ]; then exit 1; fi

# weed out multiple erros due to bad year
ncal 1 $1 > /dev/null && \
for m in 01 02 03 04 05 06 07 08 09 10 11 12; do
	echo $1-$m-`ncal $m $1 | grep Fr | sed 's/.* \([0-9]\)/\1/'`
done
```



For systems without ncal:

```sh
#!/bin/sh

# usage: last_fridays [ year]

year=${1:-`date +%Y`}    # default to current year
month=1
while [ 12 -ge $month ]; do
    # Ensure 2 digits: if we try to strip off 2 characters but it still
    # looks the same, that means there was only 1 char, so we'll pad it.
    [ "$month" = "${month%??}" ] && month=0$month

    cal $month $year | awk '{print $6}' | grep . | tail -1 \
        | sed "s@^@$year-$month-@"

    # Strip leading zeros to avoid octal interpretation
    month=$(( 1 + ${month#0} ))
done
```



Using <code>date --date</code> from GNU date??? This code is not portable.


```bash
#!/bin/sh

# Free code, no limit work
# $Id: lastfridays,v 1.1 2011/11/10 00:48:16 gilles Exp gilles $

# usage :
# lastfridays 2012 # prints last fridays of months of year 2012

debug=${debug:-false}
#debug=true

epoch_year_day() {
	#set -x
	x_epoch=`expr ${2:-0} '*' 86400 + 43200`
	date --date="${1:-1970}-01-01 UTC $x_epoch seconds" +%s
}

year_of_epoch() {
	date --date="1970-01-01 UTC ${1:-0} seconds" +%Y
}
day_of_epoch() {
	LC_ALL=C date --date="1970-01-01 UTC ${1:-0} seconds" +%A
}
date_of_epoch() {
	date --date="1970-01-01 UTC ${1:-0} seconds" "+%Y-%m-%d"
}
month_of_epoch() {
	date --date="1970-01-01 UTC ${1:-0} seconds" "+%m"
}

last_fridays() {
	year=${1:-2012}

        next_year=`expr $year + 1`
        $debug && echo "next_year $next_year"

        current_year=$year
        day=0
        previous_month=01

        while test $current_year != $next_year; do

        	$debug && echo "day $day"

        	current_epoch=`epoch_year_day $year $day`
        	$debug && echo "current_epoch $current_epoch"

        	current_year=`year_of_epoch $current_epoch`

        	current_day=`day_of_epoch $current_epoch`
        	$debug && echo "current_day $current_day"

        	test $current_day = 'Friday' && current_friday=`date_of_epoch $current_epoch`
        	$debug && echo "current_friday $current_friday"

        	current_month=`month_of_epoch $current_epoch`
        	$debug && echo "current_month $current_month"

        	# Change of month => previous friday is the last of month
        	test "$previous_month" != "$current_month" \
        		&& echo $previous_friday

        	previous_month=$current_month
        	previous_friday=$current_friday
        	day=`expr $day + 1`
        done
}

# main
last_fridays ${1:-2012}
```


Sample execution:

```txt

lastfridays 2012
2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28

```



## Visual FoxPro


```vfp

*!* OOP implementaion
LOCAL lnYear As Integer, oCalc As fricalc
CLEAR
lnYear = VAL(INPUTBOX("Year", "Year"))
oCalc = NEWOBJECT("fricalc")
oCalc.LastFriday(lnYear)

DEFINE CLASS fricalc As Session
DataSession = 2	&& Private

PROCEDURE Init
*!* These date settings are private to this class
SET DATE YMD
SET CENTURY ON
SET MARK TO "-"
ENDPROC

FUNCTION LastFriday(tnYear As Integer) As VOID
LOCAL i As Integer, ldDate As Date
CLEAR
? "Last Fridays in the year " + TRANSFORM(tnYear)
FOR i = 1 TO 12
	ldDate = DATE(tnYear, i, 1)	&& 1st of month
	ldDate = GOMONTH(ldDate, 1) - 1	&& last day of month
	*!* Use the built in function to return the day of the week
	*!* 6 is Friday
	DO WHILE DOW(ldDate) # 6
		ldDate = ldDate - 1
	ENDDO
	? ldDate
ENDFOR
ENDFUNC

ENDDEFINE

```



## XPL0


```XPL0
include c:\cxpl\codes; \intrinsic 'code' declarations

func WeekDay(Year, Month, Day);   \Return day of week (0=Sun, 1=Mon ... 6=Sat)
int  Year, Month, Day;            \works for years from 1583 onward
[if Month<=2 then [Month:= Month+12;  Year:= Year-1];
return rem((Day-1 + (Month+1)*26/10 + Year + Year/4 + Year/100*6 + Year/400)/7);
];

int Year, Month, LastDay, WD;
[Year:= IntIn(8);               \from command line
for Month:= 1 to 12 do
    [LastDay:= WeekDay(Year, Month+1, 1) - WeekDay(Year, Month, 28);
    if LastDay < 0 then LastDay:= LastDay + 7;
    LastDay:= LastDay + 27;     \ = number of days in Month
    WD:= WeekDay(Year, Month, LastDay);
    WD:= WD - 5;
    if WD < 0 then WD:= WD + 7;
    LastDay:= LastDay - WD;
    IntOut(0, Year);  ChOut(0, ^-);
    if Month < 10 then ChOut(0, ^0);  IntOut(0, Month);  ChOut(0, ^-);
    IntOut(0, LastDay);  CrLf(0);
    ];
]
```


{{out}}

```txt

2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28

```



## zkl

Gregorian calendar

```zkl
var [const] D=Time.Date;
fcn lastDay(y,d){
   [1..12].pump(List,'wrap(m){  // 12 months, closure for y & d
      [D.daysInMonth(y,m)..1,-1].pump(Void,'wrap(_d){  // work backwards
         D.weekDay(y,m,_d) :
         if (_==d) return(Void.Stop,D.toYMDString(y,m,_d))
      })
   })
}
lastDay(2012,D.Friday).concat("\n").println();
```

For each month in year y, count back from the last day in the month
until a Friday is found and print that date.
A pump is a loop over a sequence and Void.Stop stops the pump with a value.
The first parameter to a pump is the sink.
All the imperative loop constructs are available but I didn't feel like using them.
A wrap is a function closure over unknown values in the function,
necessary because functions are not lexically scoped.
{{out}}

```txt

2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28

```

