+++
title = "Find the last Sunday of each month"
description = ""
date = 2019-06-21T14:46:12Z
aliases = []
[extra]
id = 13313
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "360_assembly",
  "ada",
  "autohotkey",
  "awk",
  "batch_file",
  "bbc_basic",
  "befunge",
  "c",
  "clojure",
  "cobol",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "elixir",
  "erlang",
  "factor",
  "fbsl",
  "fortran",
  "freebasic",
  "gambas",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "k",
  "kotlin",
  "lasso",
  "liberty_basic",
  "livecode",
  "lua",
  "maple",
  "nim",
  "ocaml",
  "oforth",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "powershell",
  "purebasic",
  "python",
  "racket",
  "rebol",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "scala",
  "seed7",
  "sidef",
  "smalltalk",
  "stata",
  "swift",
  "tcl",
  "vbscript",
  "zkl",
]
+++

## Task

{{task}}  [[Category:Date and time]]
Write a program or a script that returns the last Sundays of each month of a given year. The year may be given through any simple input method in your language (command line, std in, etc).

Example of an expected output:


```txt
./last_sundays 2013
2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29
```



## Related tasks

* [[Day of the week]]
* [[Five weekends]]
* [[Last Friday of each month]]




## 360 Assembly

The program uses one ASSIST macro (XPRNT) to keep the code as short as possible.

```360asm
*        Last Sunday of each month 31/01/2017
LASTSUND CSECT
         USING  LASTSUND,R13       base register
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
         LA     R7,42              42
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
         AR     R7,R5              k=42+y+y/4+6*(y/100)+y/400
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
YEAR     DC     F'2013'            <== input year
DAYS     DC     F'31',F'28',F'31',F'30',F'31',F'30'
         DC     F'31',F'31',F'30',F'31',F'30',F'31'
PG       DC     CL80'YYYY-MM-DD'   buffer
XDEC     DS     CL12               temp
DW       DS     D                  packed (PL8) 15num
         YREGS
         END    LASTSUND
```

```txt

2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29

```



## Ada


The program from [[http://rosettacode.org/wiki/Last_Friday_of_each_month#Ada]] solves this task, as well.

```txt
>./last_weekday_in_month sunday 2013
2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29
```


=={{Header|AppleScript}}==

```AppleScript
-- LAST SUNDAYS OF YEAR ------------------------------------------------------

--  lastSundaysOfYear :: Int -> [Date]
on lastSundaysOfYear(y)

    -- lastWeekDaysOfYear :: Int -> Int -> [Date]
    script lastWeekDaysOfYear
        on |λ|(intYear, iWeekday)

            -- lastWeekDay :: Int -> Int -> Date
            script lastWeekDay
                on |λ|(iLastDay, iMonth)
                    set iYear to intYear

                    calendarDate(iYear, iMonth, iLastDay - ¬
                        (((weekday of calendarDate(iYear, iMonth, iLastDay)) as integer) + ¬
                            (7 - (iWeekday))) mod 7)
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
            {31, cond(isLeapYear(y), 29, 28), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}
        end lastDaysOfMonths
    end script

    lastWeekDaysOfYear's |λ|(y, Sunday as integer)
end lastSundaysOfYear


-- TEST ----------------------------------------------------------------------
on run argv

    intercalate(linefeed, ¬
        map(isoRow, ¬
            transpose(map(lastSundaysOfYear, ¬
                apply(cond(class of argv is list and argv ≠ {}, ¬
                    singleYearOrRange, fiveCurrentYears), argIntegers(argv))))))

end run

-- ARGUMENT HANDLING ---------------------------------------------------------

-- Up to two optional command line arguments: [yearFrom], [yearTo]
-- (Default range in absence of arguments: from two years ago, to two years ahead)

-- ~ $ osascript ~/Desktop/lastSundays.scpt
-- ~ $ osascript ~/Desktop/lastSundays.scpt 2013
-- ~ $ osascript ~/Desktop/lastSundays.scpt 2013 2016

-- singleYearOrRange :: [Int] -> [Int]
on singleYearOrRange(argv)
    apply(cond(length of argv > 0, my range, my fiveCurrentYears), argv)
end singleYearOrRange

-- fiveCurrentYears :: () -> [Int]
on fiveCurrentYears(_)
    set intThisYear to year of (current date)
    enumFromTo(intThisYear - 2, intThisYear + 2)
end fiveCurrentYears

-- argIntegers :: maybe [String] -> [Int]
on argIntegers(argv)
    if class of argv is list and argv ≠ {} then
        {map(my parseInt, argv)}
    else
        {}
    end if
end argIntegers


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- apply (a -> b) -> a -> b
on apply(f, a)
    mReturn(f)'s |λ|(a)
end apply

-- calendarDate :: Int -> Int -> Int -> Date
on calendarDate(intYear, intMonth, intDay)
    tell (current date)
        set {its year, its month, its day, its time} to ¬
            {intYear, intMonth, intDay, 0}
        return it
    end tell
end calendarDate

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

-- isoDateString :: Date -> String
on isoDateString(dte)
    (((year of dte) as string) & ¬
        "-" & text items -2 thru -1 of ¬
        ("0" & ((month of dte) as integer) as string)) & ¬
        "-" & text items -2 thru -1 of ¬
        ("0" & day of dte)
end isoDateString

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

-- parseInt :: String -> Int
on parseInt(s)
    s as integer
end parseInt

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

```txt
2015-01-25    2016-01-31    2017-01-29    2018-01-28    2019-01-27
2015-02-22    2016-02-28    2017-02-26    2018-02-25    2019-02-24
2015-03-29    2016-03-27    2017-03-26    2018-03-25    2019-03-31
2015-04-26    2016-04-24    2017-04-30    2018-04-29    2019-04-28
2015-05-31    2016-05-29    2017-05-28    2018-05-27    2019-05-26
2015-06-28    2016-06-26    2017-06-25    2018-06-24    2019-06-30
2015-07-26    2016-07-31    2017-07-30    2018-07-29    2019-07-28
2015-08-30    2016-08-28    2017-08-27    2018-08-26    2019-08-25
2015-09-27    2016-09-25    2017-09-24    2018-09-30    2019-09-29
2015-10-25    2016-10-30    2017-10-29    2018-10-28    2019-10-27
2015-11-29    2016-11-27    2017-11-26    2018-11-25    2019-11-24
2015-12-27    2016-12-25    2017-12-31    2018-12-30    2019-12-29
```



## AutoHotkey


```AutoHotkey
InputBox, Year, , Enter a year., , 300, 135
Date := Year . "0101"

while SubStr(Date, 1, 4) = Year {
    FormatTime, WD, % Date, WDay
    if (WD = 1)
        MM := LTrim(SubStr(Date, 5, 2), "0"), Day%MM% := SubStr(Date, 7, 2)
    Date += 1, Days
}

Gui, Font, S10, Courier New
Gui, Add, Text, , % "Last Sundays of " Year ":`n---------------------"

Loop, 12 {
    FormatTime, Month, % Year (A_Index > 9 ? "" : "0") A_Index, MMMM
    Gui, Add, Text, y+1, % Month (StrLen(Month) > 7 ? "" : "`t") "`t" Day%A_Index%
}

Gui, Show
return
```

```txt
Last Sundays of 2013:
---------------------
January		27
February	24
March		31
April		28
May		26
June		30
July		28
August		25
September	29
October		27
November	24
December	29
```


## AWK


```AWK

# syntax: GAWK -f FIND_THE_LAST_SUNDAY_OF_EACH_MONTH.AWK [year]
BEGIN {
    split("31,28,31,30,31,30,31,31,30,31,30,31",daynum_array,",") # days per month in non leap year
    year = (ARGV[1] == "") ? strftime("%Y") : ARGV[1]
    if (year % 400 == 0 || (year % 4 == 0 && year % 100 != 0)) {
      daynum_array[2] = 29
    }
    for (m=1; m<=12; m++) {
      for (d=daynum_array[m]; d>=1; d--) {
        if (strftime("%a",mktime(sprintf("%d %d %d 0 0 0",year,m,d))) == "Sun") {
          printf("%04d-%02d-%02d\n",year,m,d)
          break
        }
      }
    }
    exit(0)
}

```

<p>Output:</p>

```txt

2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29

```



## Batch File

Uses  day of week, last day of month and  leapyear routines

```Batch File

@echo off
setlocal enabledelayedexpansion
set /p yr= Enter year:
echo.
call:monthdays %yr% list
set mm=1
for %%i in (!list!) do (
  call:calcdow !yr! !mm! %%i dow
  set/a lsu=%%i-dow
  set mf=0!mm!
  echo !yr!-!mf:~-2!-!lsu!
  set /a mm+=1
)
pause
exit /b

:monthdays yr &list
setlocal
call:isleap %1 ly
for /L %%i in (1,1,12) do (
  set /a "nn = 30 + ^!(((%%i & 9) + 6) %% 7) + ^!(%%i ^^ 2) * (ly - 2)
  set list=!list! !nn!
)
endlocal & set %2=%list%
exit /b

:calcdow yr mt dy &dow  :: 0=sunday
setlocal
set/a a=(14-%2)/12,yr=%1-a,m=%2+12*a-2,"dow=(%3+yr+yr/4-yr/100+yr/400+31*m/12)%%7"
endlocal & set %~4=%dow%
exit /b

:isleap yr &leap  :: remove ^ if not delayed expansion
set /a "%2=^!(%1%%4)+(^!^!(%1%%100)-^!^!(%1%%400))"
exit /b

```


```txt

Enter year: 2016

2016-01-31
2016-02-28
2016-03-27
2016-04-24
2016-05-29
2016-06-26
2016-07-31
2016-08-28
2016-09-25
2016-10-30
2016-11-27
2016-12-25

```



## BBC BASIC


```bbcbasic

INSTALL @lib$+"DATELIB"

INPUT "What year to calculate (YYYY)? " Year%

PRINT '"Last Sundays in ";Year%;" are on:"
FOR Month%=1 TO 12
  PRINT Year% "-" RIGHT$("0"+STR$Month%,2) "-";FN_dim(Month%,Year%)-FN_dow(FN_mjd(FN_dim(Month%,Year%),Month%,Year%))
NEXT
END

```

```txt

What year to calculate (YYYY)? 2013

Last Sundays in 2013 are on:
      2013-01-27
      2013-02-24
      2013-03-31
      2013-04-28
      2013-05-26
      2013-06-30
      2013-07-28
      2013-08-25
      2013-09-29
      2013-10-27
      2013-11-24
      2013-12-29

```



## Befunge

This is essentially identical to [[Last_Friday_of_each_month#Befunge|Last Friday of each month]] except for the initial day offset.

```befunge
":raeY",,,,,&>55+,:::45*:*%\"d"%!*\4%+!3v
v2++6**"I"5\+/*:*54\-/"d"\/4::-1::p53+g5<
>:00p5g4-+7%\:0\v>,"-",5g+:55+/68*+,55+%v
^<<_$$vv*86%+55:<^+*86%+55,+*86/+55:-1:<6
>$$^@$<>+\55+/:#^_$>:#,_$"-",\:04-\-00g^8
^<# #"#"##"#"##!`       +76:+1g00,+55,+*<
```


```txt
Year:2013

2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29
```



## C

Identical to [[Last_Friday_of_each_month#C|Last Friday of each month]] except for removal of the offset day in the output.


```C

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
        int days[] = {31,29,31,30,31,30,31,31,30,31,30,31};
        int m, y, w;

        if (argc < 2 || (y = atoi(argv[1])) <= 1752) return 1;
        days[1] -= (y % 4) || (!(y % 100) && (y % 400));
        w = y * 365 + 97 * (y - 1) / 400 + 4;

        for(m = 0; m < 12; m++) {
                w = (w + days[m]) % 7;
                printf("%d-%02d-%d\n", y, m + 1,days[m] - w);
        }

        return 0;
}

```



## C++


```cpp

#include <windows.h>
#include <iostream>
#include <string>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
class lastSunday
{
public:
    lastSunday()
    {
	m[0]  = "JANUARY:   "; m[1]  = "FEBRUARY:  "; m[2]  = "MARCH:     "; m[3]  = "APRIL:     ";
	m[4]  = "MAY:       "; m[5]  = "JUNE:      "; m[6]  = "JULY:      "; m[7]  = "AUGUST:    ";
	m[8]  = "SEPTEMBER: "; m[9]  = "OCTOBER:   "; m[10] = "NOVEMBER:  "; m[11] = "DECEMBER:  ";
    }

    void findLastSunday( int y )
    {
	year = y;
	isleapyear();

	int days[] = { 31, isleap ? 29 : 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 },
		d;
	for( int i = 0; i < 12; i++ )
	{
	    d = days[i];
	    while( true )
	    {
		if( !getWeekDay( i, d ) ) break;
		d--;
	    }
	    lastDay[i] = d;
	}

	display();
    }

private:
    void isleapyear()
    {
	isleap = false;
	if( !( year % 4 ) )
	{
	    if( year % 100 ) isleap = true;
	    else if( !( year % 400 ) ) isleap = true;
	}
    }

    void display()
    {
	system( "cls" );
	cout << "  YEAR " << year << endl << "
### =======
" << endl;
	for( int x = 0; x < 12; x++ )
	    cout << m[x] << lastDay[x] << endl;

	cout << endl << endl;
    }

    int getWeekDay( int m, int d )
    {
	int y = year;

	int f = y + d + 3 * m - 1;
	m++;
	if( m < 3 ) y--;
	else f -= int( .4 * m + 2.3 );

	f += int( y / 4 ) - int( ( y / 100 + 1 ) * 0.75 );
	f %= 7;

	return f;
    }

    int lastDay[12], year;
    string m[12];
    bool isleap;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    int y;
    lastSunday ls;

    while( true )
    {
	system( "cls" );
	cout << "Enter the year( yyyy ) --- ( 0 to quit ): ";
	cin >> y;
	if( !y ) return 0;

	ls.findLastSunday( y );

	system( "pause" );
    }
    return 0;
}
//--------------------------------------------------------------------------------------------------

```

```txt

  YEAR 2013

### =======

JANUARY:   27
FEBRUARY:  24
MARCH:     31
APRIL:     28
MAY:       26
JUNE:      30
JULY:      28
AUGUST:    25
SEPTEMBER: 29
OCTOBER:   27
NOVEMBER:  24
DECEMBER:  29

```

Other solution, based on the Boost DateTime library:

```C++
#include <iostream>

#include <boost/date_time/gregorian/gregorian.hpp>
#include <cstdlib>

int main( int argc , char* argv[ ] ) {
   using namespace boost::gregorian ;

   int year =  std::atoi( argv[ 1 ] ) ;
   for ( int i = 1 ; i < 13 ; i++ ) {
      try {
	 date d( year , i , 1  ) ;
	 d = d.end_of_month( ) ;
	 day_iterator d_itr ( d ) ;
	 while ( d_itr->day_of_week( ) != Sunday ) {
	    --d_itr ;
	 }
	 std::cout << to_simple_string ( *d_itr ) << std::endl ;
      } catch ( bad_year by ) {
	  std::cout << "Terminated because of " << by.what( ) << "\n" ;
      }
   }
   return 0 ;
}
```

```txt
2013-Jan-27
2013-Feb-24
2013-Mar-31
2013-Apr-28
2013-May-26
2013-Jun-30
2013-Jul-28
2013-Aug-25
2013-Sep-29
2013-Oct-27
2013-Nov-24
2013-Dec-29

```


## C#

```c#
using System;

namespace LastSundayOfEachMonth
{
    class Program
    {
        static void Main()
        {
            Console.Write("Year to calculate: ");

            string strYear = Console.ReadLine();
            int year = Convert.ToInt32(strYear);

            DateTime date;
            for (int i = 1; i <= 12; i++)
            {
                date = new DateTime(year, i, DateTime.DaysInMonth(year, i), System.Globalization.CultureInfo.CurrentCulture.Calendar);
                while (date.DayOfWeek != DayOfWeek.Sunday)
                {
                    date = date.AddDays(-1);
                }
                Console.WriteLine(date.ToString("yyyy-MM-dd"));
            }
        }
    }
}

```

```txt
Year to calculate: 2013
2013-Jan-27
2013-Feb-24
2013-Mar-31
2013-Apr-28
2013-May-26
2013-Jun-30
2013-Jul-28
2013-Aug-25
2013-Sep-29
2013-Oct-27
2013-Nov-24
2013-Dec-29

```



## Clojure



```clojure
(ns last-sundays.core
  (:require [clj-time.core :as time]
            [clj-time.periodic :refer [periodic-seq]]
            [clj-time.format :as fmt])
  (:import (org.joda.time DateTime DateTimeConstants))
  (:gen-class))

(defn sunday? [t]
  (= (.getDayOfWeek t) (DateTimeConstants/SUNDAY)))

(defn sundays [year]
  (take-while #(= (time/year %) year)
              (filter sunday? (periodic-seq (time/date-time year 1 1) (time/days 1)))))

(defn last-sundays-of-months [year]
  (->> (sundays year)
       (group-by time/month)
       (vals)
       (map (comp first #(sort-by time/day > %)))
       (map #(fmt/unparse (fmt/formatters :year-month-day) %))
       (interpose "\n")
       (apply str)))

(defn -main [& args]
  (println (last-sundays-of-months (Integer. (first args)))))

```

```txt
2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29

```



## COBOL


```COBOL

       program-id. last-sun.
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
        2 sunday pic 9(4) value 7.
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
               compute dow = function mod ((dow - sunday) 7)
               subtract dow from da
               display yr "-" mo "-" da
               add 1 to mo
           end-perform
           stop run
           .
       end program last-sun.

```


```txt

2016-01-31
2016-02-28
2016-03-27
2016-04-24
2016-05-29
2016-06-26
2016-07-31
2016-08-28
2016-09-25
2016-10-30
2016-11-27
2016-12-25

```



## Common Lisp

First, calculate the day of week of the 1st day of next month.  Then figure out how many days you have to go back until the last Sunday of the month.


```lisp
(defun last-sundays (year)
  (loop for month from 1 to 12
        for last-month-p = (= month 12)
        for next-month = (if last-month-p 1 (1+ month))
        for year-of-next-month = (if last-month-p (1+ year) year)
        for 1st-day-next-month = (encode-universal-time 0 0 0 1 next-month year-of-next-month 0)
        for 1st-day-dow = (nth-value 6 (decode-universal-time 1st-day-next-month 0))
        ;; 0: monday, 1: tuesday, ... 6: sunday
        for diff-to-last-sunday = (1+ 1st-day-dow)
        for last-sunday = (- 1st-day-next-month (* diff-to-last-sunday 24 60 60))
        do (multiple-value-bind (second minute hour date month year)
               (decode-universal-time last-sunday 0)
             (declare (ignore second minute hour))
             (format t "~D-~2,'0D-~2,'0D~%" year month date))))

(last-sundays 2013)
```

```txt
2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29
```



## D


```d
void lastSundays(in uint year) {
    import std.stdio, std.datetime;

    foreach (immutable month; 1 .. 13) {
        auto date = Date(year, month, 1);
        date.day(date.daysInMonth);
        date.roll!"days"(-(date.dayOfWeek + 7) % 7);
        date.writeln;
    }
}

void main() {
    lastSundays(2013);
}
```

```txt
2013-Jan-27
2013-Feb-24
2013-Mar-31
2013-Apr-28
2013-May-26
2013-Jun-30
2013-Jul-28
2013-Aug-25
2013-Sep-29
2013-Oct-27
2013-Nov-24
2013-Dec-29
```



## Elixir


```elixir
defmodule RC do
  def lastSunday(year) do
    Enum.map(1..12, fn month ->
      lastday = :calendar.last_day_of_the_month(year, month)
      daynum = :calendar.day_of_the_week(year, month, lastday)
      sunday = lastday - rem(daynum, 7)
      {year, month, sunday}
    end)
  end
end

y = String.to_integer(hd(System.argv))
Enum.each(RC.lastSunday(y), fn {year, month, day} ->
  :io.format "~4b-~2..0w-~2..0w~n", [year, month, day]
end)
```


```txt

C:\Elixir>elixir lastSunday.exs 2013
2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29

```



## Erlang


```Erlang

-module(last_sundays).

-export([in_year/1]).

% calculate all the last sundays in a particular year
in_year(Year) ->
    [lastday(Year, Month, 7) || Month <- lists:seq(1, 12)].

% calculate the date of the last occurrence of a particular weekday
lastday(Year, Month, WeekDay) ->
    Ldm = calendar:last_day_of_the_month(Year, Month),
    Diff = calendar:day_of_the_week(Year, Month, Ldm) rem WeekDay,
    {Year, Month, Ldm - Diff}.


```

```txt

30> [io:fwrite("~B-~2.10.0B-~B~n", [Y,M,D]) || {Y,M,D} <- last_sundays:in_year(2013)].
2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29

```



## FBSL


```qbasic
#APPTYPE CONSOLE

DIM date AS INTEGER, dayname AS STRING
FOR DIM i = 1 TO 12
    FOR DIM j = 31 DOWNTO 1
        date = 20130000 + (i * 100) + j
        IF CHECKDATE(i, j, 2013) THEN
            dayname = DATECONV(date, "dddd")
            IF dayname = "Sunday" THEN
                PRINT 2013, " ", i, " ", j
                EXIT FOR
            END IF
        END IF
    NEXT
NEXT

PAUSE

```

```txt
2013-1-27
2013-2-24
2013-3-31
2013-4-28
2013-5-26
2013-6-30
2013-7-28
2013-8-25
2013-9-29
2013-10-27
2013-11-24
2013-12-29

Press any key to continue...

```


=={{header|F_Sharp|F#}}==
<p>We use a transformation from and to the Julian Day Number, see also PARI/GP or Fortran.</p>
<p>The formulars used here come from [http://calendars.wikia.com/wiki/Julian_day_number] (section "Calculation").</p>

```fsharp
let jdn (year, month, day) =
    let a = (14 - month) / 12
    let y = year + 4800 - a
    let m = month + 12 * a - 3
    day + (153*m+2)/5 + 365*y + y/4 - y/100 + y/400 - 32045

let date_from_jdn jdn =
    let j = jdn + 32044
    let g = j / 146097
    let dg = j % 146097
    let c = (dg / 36524 + 1) * 3 / 4
    let dc = dg - c * 36524
    let b = dc / 1461
    let db = dc % 1461
    let a = (db / 365 + 1) * 3 / 4
    let da = db - a * 365
    let y = g * 400 + c * 100 + b * 4 + a
    let m = (da * 5 + 308) / 153 - 2
    let d = da - (m + 4) * 153 / 5 + 122
    (y - 4800 + (m + 2) / 12, (m + 2) % 12 + 1, d + 1)

[<EntryPoint>]
let main argv =
    let year = System.Int32.Parse(argv.[0])
    [for m in 1..12 do yield jdn (year,m+1,0)]
    |> List.map (fun x -> date_from_jdn (x - (x+1)%7))
    |> List.iter (printfn "%A")
    0
```

```txt
RosettaCode 2016
(2016, 1, 31)
(2016, 2, 28)
(2016, 3, 27)
(2016, 4, 24)
(2016, 5, 29)
(2016, 6, 26)
(2016, 7, 31)
(2016, 8, 28)
(2016, 9, 25)
(2016, 10, 30)
(2016, 11, 27)
(2016, 12, 25)
```



## Factor

This program expects a year passed in via command line argument. In case you are wondering — yes — Factor has a <tt>last-sunday-of-month</tt> word in its calendar vocabulary. This is par for the course when it comes to Factor.

```factor

USING: calendar calendar.format command-line io kernel math math.parser
sequences ;
IN: rosetta-code.last-sunday

: parse-year     ( -- ts )     (command-line) second string>number <year> ;
: print-last-sun ( ts -- )     last-sunday-of-month (timestamp>ymd) nl ;
: inc-month      ( ts -- ts' ) 1 months time+ ;
: process-month  ( ts -- ts' ) dup print-last-sun inc-month ;
: main           ( -- )        parse-year 12 [ process-month ] times drop ;

MAIN: main

```

```txt

>factor last-sunday.factor 2013
2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29

```



## Fortran

Having already functions that calculate a daynumber from a date and the reverse, the notorious routines of H. F. Fliegel and T. C. van Flandern, the problem becomes simple. Their routine calculates the Julian day number but my version adjusts so that day zero is the thirty-first of December 1899, which is a Sunday. Thus, the day of the week is given by MOD(Daynumber,7) with zero for Sunday up to six for Saturday, as per Genesis.

Their routine is not only fast, but flexible, in particular allowing Month + 1 so that DAYNUM(Y,M + 1,0) gives the last day of month M for M = 1 to 12, with no agony over which month has what last day and leap years or not. If W is the day of week desired, all that remains is to use MOD(D - W,7) to determine the offset back to the desired day of the week and make that shift. There is a problem with the MOD function when negative numbers are involved: it may or may not (depending on computer, compiler, language) return a negative result; by adding 7 and taking a MOD again, this variation is suppressed.

The source uses the MODULE protocol for convenience in its usage in larger projects (which contain a calculation for Easter, daylight savings changeover dates, solar azimuth and altitude, etc. which have been removed here), but requires only F77 features, except for the annoyance of the MUNYAD function returning a TYPE(DATEBAG) result to get the three parts. This is an example where a palindromic programming protocol would be so much better. One would write
```Fortran
      D = DAYNUM(Y,M,D)    !Daynumber from date.
      DAYNUM(Y,M,D) = D    !Date parts from a day number.
```

But alas, only pl/i offers palindromic functions, and only for SUBSTR.


```Fortran

      MODULE DATEGNASH
C          Calculate conforming to complex calendarical contortions.
C   Astronomer Simon Newcomb determined that the tropical year of 1900
c contained 31556925.9747 seconds, or 365.24219879 days.
c Subsequent definitions involve "no measurable differences",
c whereas in 45BC (when the Julian calendar was adopted), the year was
c 365.24232 days long, going by modern calculations.
c   The "tropical" year is the time between the same equinoxes, and thus
c contains the effect of the precession of the Earth's axis, which would
c otherwise cause the seasons to likewise precess around the "fixed star"
c year, that being the time for midnight to point in the same direction
c amongst the "fixed" stars. Specifically, the vernal equinox
c (the northern hemisphere's spring equinox: cultural colonialism)
c is meant to hover around the 21'st of March, although it may fall
c within the 19'th or 20'th, which last has been the most popular in
c the 20'th century, until the leap year of 2000 resynchronised
c the civil calendar.
C   By contrast, the computations of astrologers are still based on the
c constellations as oriented in Babylonian times...

c   So, to add .24219879 to 365 days...
c   Adjustment per year   Nett     Discrepancy remaining.
c   +1/4     +.25        +.25      -.00780121  5h 48m 45.98s
c   -1/100   -.01         .24      +.00219879    -11m 14.02s
c   +1/400   +.0025       .2425    -.00030121        -26.02s
c   -1/4000  -.00025      .24225   -.00005121         -4.42s
c
c   The remnant of -.00005121 (meaning that the calendar year is too long)
c amounts to needing to drop one day on 19,527 years, and while this could be
c accommodated nicely enough by -1/20000 to give a calendar year of
c 365.24420 days with a remaining discrepancy of -.00000121 or -.01sec/year,
c there is a problem. The Earth's spin is slowing by a similar amount.
c   Similar to leap years and leap days are the leap seconds that since the
c development of clocks based on atomic oscillations, have been added or
c sometimes removed to keep clock time aligned with astronomical observations.
c This additional confusion is not further considered.

       TYPE DateBag		!Pack three parts into one.
        INTEGER DAY,MONTH,YEAR	!The usual suspects.
       END TYPE DateBag		!Simple enough.

       CHARACTER*9 MONTHNAME(12),DAYNAME(0:6)	!Re-interpretations.
       PARAMETER (MONTHNAME = (/"January","February","March","April",
     1  "May","June","July","August","September","October","November",
     2  "December"/))
       PARAMETER (DAYNAME = (/"Sunday","Monday","Tuesday","Wednesday",
     1  "Thursday","Friday","Saturday"/))	!Index this array with DayNum mod 7.
       CHARACTER*3 MTHNAME(12)		!The standard abbreviations.
       PARAMETER (MTHNAME = (/"JAN","FEB","MAR","APR","MAY","JUN",
     1  "JUL","AUG","SEP","OCT","NOV","DEC"/))

       INTEGER*4 JDAYSHIFT		!INTEGER*2 just isn't enough.
       PARAMETER (JDAYSHIFT = 2415020)	!Thus shall 31/12/1899 give 0, a Sunday, via DAYNUM.
       DOUBLE PRECISION DAYSINYEAR	!A real ache.
       PARAMETER (DAYSINYEAR =  365.24219879D0)	!The "D0" demands DOUBLE PRECISION precision.
       INTEGER*4 SECONDSINDAY			!This has its uses.
       PARAMETER (SECONDSINDAY = 24*60*60)	!86400. Disregarding "leap" seconds.
       INTEGER NOTADAYNUMBER			!Might as well settle on one value.
       PARAMETER (NOTADAYNUMBER = -2147483648)	!And everyone use it.

       PARAMETER NZBASEPLACE = "Mt. Cook Trig, Wellington."	!Name the place.
       DOUBLE PRECISION NZBASELAT,NZBASELONG	!Alas, DATA statements do not allow arithmetic expressions.
       PARAMETER (NZBASELAT = -((59.3 D0/60 + 17)/60 +  41))	!Degrees South, thus negative.
       PARAMETER (NZBASELONG = ((34.65D0/60 + 46)/60 + 174))	!Degrees East are oriented as positive: left to right with N up.
C                                   Seconds  Minutes Degrees.
C   This is the location of the Mt. Cook trigonometrical base point for New Zealand.
C    (It's in the foyer of what used to be the Dominion Museum, Wellington)
C   Determined by Henry Jackson, chief surveyor, in 1870.

       TYPE Terroir	!Collate the attributes of location, as so far needed.
        CHARACTER*28 PLACENAME	!Name the location.
        DOUBLE PRECISION LATITUDE,LONGITUDE	!Locate the location.
        DOUBLE PRECISION ZONETIME	!The time zone of its civil clock, not necessarily a whole hour.
       END TYPE Terroir		!The nature of the climate, soil, etc. is not yet involved.
       TYPE(Terroir) BASE	!Righto, let's have one of them.
       DATA BASE/Terroir("Mt. Cook Trig, Wellington.",	!The compiler bungles if NZBASEPLACE is used here.
     1  NZBASELAT,NZBASELONG,+12.0)/	!Where it's at. +12 hours ahead = +180 degrees Eastward of Greenwhich.
Careful! New Zealand is not centred on longitude 180, but its civil clock's time zone is.
       DOUBLE PRECISION SINBASELAT,COSBASELAT	!Calculated in SOLARDIRECTION and ZAPME.

      CONTAINS			!Let the madness begin.

       INTEGER*4 FUNCTION DAYNUM(YY,M,D)	!Computes (JDayN - JDayShift), not JDayN.
C   Conversion from a Gregorian calendar date to a Julian day number, JDayN.
C   Valid for any Gregorian calendar date producing a Julian day number
C greater than zero, though remember that the Gregorian calendar
C was not used before y1582m10d15 and often, not after that either.
C thus in England (et al) when Wednesday 2'nd September 1752 (Julian style)
C was followed by Thursday the 14'th, occasioning the Eleven Day riots
C because creditors demanded a full month's payment instead of 19/30'ths.
C   The zero of the Julian day number corresponds to the first of January
C 4713BC on the *Julian* calendar's naming scheme, as extended backwards
C with current usage into epochs when it did not exist: the proleptic Julian calendar.
c This function employs the naming scheme of the *Gregorian* calendar,
c and if extended backwards into epochs when it did not exist (thus the
c proleptic Gregorian calendar) it would compute a zero for y-4713m11d24 *if*
c it is supposed there was a year zero between 1BC and 1AD (as is convenient
c for modern mathematics and astronomers and their simple calculations), *but*
c 1BC immediately precedes 1AD without any year zero in between (and is a leap year)
c thus the adjustment below so that the date is y-4714m11d24 or 4714BCm11d24,
c not that this name was in use at the time...
c   Although the Julian calendar (introduced by himself in what we would call 45BC,
c which was what the Romans occasionally called 709AUC) was provoked by the
c "years of confusion" resulting from arbitrary application of the rules
c for the existing Roman calendar, other confusions remain unresolved,
c so precise dating remains uncertain despite apparently precise specifications
c (and much later, Dennis the Short chose wrongly for the birth of Christ)
c and the Roman practice of inclusive reckoning meant that every four years
c was interpreted as every third (by our exclusive reckoning) so that the
c leap years were not as we now interpret them. This was resolved by Augustus
c but exactly when (and what date name is assigned) and whose writings used
c which system at the time of writing is a matter of more confusion,
c and this has continued for centuries.
C   Accordingly, although an algorithm may give a regular sequence of date names,
c that does not mean that those date names were used at the time even if the
c calendar existed then, because the interpretation of the algorithm varied.
c This in turn means that a date given as being on the Julian calendar
c prior to about 10AD is not as definite as it may appear and its alignment
c with the astronomical day number is uncertain even though the calculation
c is quite definite.
c
C   Computationally, year 1 is preceded by year 0, in a smooth progression.
C But there was never a year zero despite what astronomers like to say,
C so the formula's year 0 corresponds to 1BC, year -1 to 2BC, and so on back.
C Thus y-4713 in this counting would be 4714BC on the Gregorian calendar,
C were it to have existed then which it didn't.
C   To conform to the civil usage, the incoming YY, presumed a proper BC (negative)
C and AD (positive) year is converted into the computational counting sequence, Y,
C and used in the formula. If a YY = 0 is (improperly) offered, it will manifest
C as 1AD. Thus YY = -4714 will lead to calculations with Y = -4713.
C Thus, 1BC is a leap year on the proleptic Gregorian calendar.
C   For their convenience, astronomers decreed that a day starts at noon, so that
C in Europe, observations through the night all have the same day number.
C The current Western civil calendar however has the day starting just after midnight
C and that day's number lasts until the following midnight.
C
C   There is no constraint on the values of D, which is just added as it stands.
C This means that if D = 0, the daynumber will be that of the last day of the
C previous month. Likewise, M = 0 or M = 13 will wrap around so that Y,M + 1,0
C will give the last day of month M (whatever its length) as one day before
C the first day of the next month.
C
C   Example: Y = 1970, M = 1, D = 1;  JDAYN = 2440588, a Thursday but MOD(2440588,7) = 3.
C   and with the adjustment JDAYSHIFT, DAYNUM = 25568; mod 7 = 4 and DAYNAME(4) = "Thursday".
C   The Julian Day number 2440588.0 is for NOON that Thursday, 2440588.5 is twelve hours later.
C   And Julian Day number 2440587.625 is for three a.m. Thursday.
C
C   DAYNUM and MUNYAD are the infamous routines of H. F. Fliegel and T.C. van Flandern,
C   presented in Communications of the ACM, Vol. 11, No. 10 (October, 1968).
Carefully typed in again by R.N.McLean (whom God preserve) December XXMMIIX.
C   Though I remain puzzled as to why they used I,J,K for Y,M,D,
C given that the variables were named in the INTEGER statement anyway.
        INTEGER*4 JDAYN		!Without rebasing, this won't fit in INTEGER*2.
        INTEGER YY,Y,M,MM,D	!NB! Full year number, so 1970, not 70.
Caution: integer division in Fortran does not produce fractional results.
C The fractional part is discarded so that 4/3 gives 1 and -4/3 gives -1.
C Thus 4/3 might be Trunc(4/3) or 4 div 3 in other languages. Beware of negative numbers!
         Y = YY		!I can fiddle this copy without damaging the original's value.
         IF (Y.LT.1) Y = Y + 1	!Thus YY = -2=2BC, -1=1BC, +1=1AD, ... becomes Y = -1, 0, 1, ...
         MM = (M - 14)/12	!Calculate once. Note that this is integer division, truncating.
         JDAYN = D - 32075	!This is the proper astronomer's Julian Day Number.
     a    + 1461*(Y + 4800  + MM)/4
     b    +  367*(M - 2     - MM*12)/12
     c    -    3*((Y + 4900 + MM)/100)/4
         DAYNUM = JDAYN - JDAYSHIFT	!Thus, *NOT* the actual *Julian* Day Number.
       END FUNCTION DAYNUM		!But one such that Mod(n,7) gives day names.

Could compute the day of the year somewhat as follows...
c  DN:=D + (61*Month + (Month div 8)) div 2 - 30
c        + if Month > 2 then FebLength - 30 else 0;

       TYPE(DATEBAG) FUNCTION MUNYAD(DAYNUM)	!Oh for palindromic programming!
Conversion from a Julian day number to a Gregorian calendar date. See JDAYN/DAYNUM.
        INTEGER*4 DAYNUM,JDAYN	!Without rebasing, this won't fit in INTEGER*2.
        INTEGER Y,M,D,L,N		!Y will be a full year number: 1950 not 50.
         JDAYN = DAYNUM + JDAYSHIFT	!Revert to a proper Julian day number.
         L = JDAYN + 68569	!Further machinations of H. F. Fliegel and T.C. van Flandern.
         N = 4*L/146097
         L = L - (146097*N + 3)/4
         Y = 4000*(L + 1)/1461001
         L = L - 1461*Y/4 + 31
         M = 80*L/2447
         D = L - 2447*M/80
         L = M/11
         M = M + 2 - 12*L
         Y = 100*(N - 49) + Y + L
         IF (Y.LT.1) Y = Y - 1	!The other side of conformity to BC/AD, as in DAYNUM.
         MUNYAD%YEAR  = Y	!Now place for the world to see.
         MUNYAD%MONTH = M
         MUNYAD%DAY   = D
       END FUNCTION MUNYAD	!A year has 365.2421988 days...

       CHARACTER*10 FUNCTION SLASHDATE(DAYNUM)	!This is relatively innocent.
Caution! The Gregorian calendar did not exist prior to 15/10/1582!
Confine expected operation to four-digit years, since fixed-field sizes are in mind.
Can use this function in WRITE statements with FORMAT, since this function does not use them.
Compilers of lesser merit can concoct code that bungles such double usage otherwise.
        INTEGER*4 DAYNUM	!-32768 to 32767 is just not adequate.
        TYPE(DATEBAG) D		!Though these numbers are more restrained.
        INTEGER N,L		!Workers.
         IF (DAYNUM.EQ.NOTADAYNUMBER) THEN	!Perhaps some work can be dodged.
           SLASHDATE = " Undated!!"	!No proper day number has been placed.
          RETURN		!So give up, rather than show odd results.
         END IF			!So much for confusion.
         D = MUNYAD(DAYNUM)	!Get the pieces.
         IF (D%DAY.GT.9) THEN	!Here we go.
           SLASHDATE(1:1) = CHAR(D%DAY/10 + ICHAR("0"))	!Faster than a table look-up?
          ELSE			!Even if not,
           SLASHDATE(1:1) = " "	!This should be quick.
         END IF			!So much for the tens digit.
         SLASHDATE(2:2) = CHAR(MOD(D%DAY,10) + ICHAR("0"))	!The units digit.
         SLASHDATE(3:3) = "/"	!Enough of the day number. The separator.
         IF (D%MONTH.GT.9) THEN	!Now for the month.
           SLASHDATE(4:4) = CHAR(D%MONTH/10 + ICHAR("0"))	!The tens digit.
          ELSE			!Not so often used. A table beckons...
           SLASHDATE(4:4) = " "	!Some might desire leading zeroes here.
         END IF			!Enough of October, November and December.
         SLASHDATE(5:5) = CHAR(MOD(D%MONTH,10) + ICHAR("0"))	!The units digit.
         SLASHDATE(6:6) = "/"	!Enough of the month number. The separator.
         L = 10			!The year value deserves a loop, it having four digits.
         N = ABS(D%YEAR)	!Should never be zero. 1BC is year -1 and 1AD is year = +1.
    1    SLASHDATE(L:L) = CHAR(MOD(N,10) + ICHAR("0"))	!But if it is, this will place a zero.
         N = N/10		!Drop a power of ten.
         L = L - 1		!Step back for the next digit.
         IF (L.GT.6) GO TO 1	!Thus always four digits, even if they lead with zero.
         IF (N.GT.0) SLASHDATE(7:7) = "?"	!Y > 9999? Might as well do something.
         IF (D%YEAR.LT.0) SLASHDATE(7:7) = "-"	!Years BC? Rather than give no indication.
c         WRITE (SLASHDATE,1) D%DAY,D%MONTH,D%YEAR	!Some compilers will bungle this.
c    1    FORMAT (I2,"/",I2,"/",I4)			!If so, a local variable must be used.
        RETURN			!Enough.		!As when SLASHDATE is invoked in a WRITE statement.
       END FUNCTION SLASHDATE	!Simple enough.
      END MODULE DATEGNASH

      PROGRAM LASTSUNDAY
      USE DATEGNASH
      INTEGER D,W,M,Y
      WRITE (6,1)
    1 FORMAT ("Employs the Gregorian calendar pattern.",/,
     1 "You specify a day of the week, then nominate a year.",/,
     2 "For each month of that year, this calculates the date ",
     3 "of the last such day.",//
     4 "So, what day (0 = Sunday, 6 = Saturday):",$)
      READ (5,*) W
      IF (W.LT.0 .OR. W.GT.6) STOP "Not a good week day number!"

   10 WRITE (6,11)
   11 FORMAT ("What year (non-positive to quit):",$)
      READ (5,*) Y
      IF (Y.LE.0) STOP
      DO M = 1,12
        D = DAYNUM(Y,M + 1,0)	!Zeroth day = last day of previous month.
        D = D - MOD(MOD(D - W,7) + 7,7)	!Protect against MOD(D,7) giving negative values for D < 0.
        WRITE (6,*) SLASHDATE(D)," : ",DAYNAME(MOD(D,7))
      END DO
      GO TO 10
      END

```

And after all that, results:

```txt

Employs the Gregorian calendar pattern.
You specify a day of the week, then nominate a year.
For each month of that year, this calculates the date of the last such day.

So, what day (0 = Sunday, 6 = Saturday):0
What year (non-positive to quit):2013
 27/ 1/2013 : Sunday
 24/ 2/2013 : Sunday
 31/ 3/2013 : Sunday
 28/ 4/2013 : Sunday
 26/ 5/2013 : Sunday
 30/ 6/2013 : Sunday
 28/ 7/2013 : Sunday
 25/ 8/2013 : Sunday
 29/ 9/2013 : Sunday
 27/10/2013 : Sunday
 24/11/2013 : Sunday
 29/12/2013 : Sunday
What year (non-positive to quit):0

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
        Print "For what year do you want to find the last Sunday of the month"
        Input "any number below 1800 stops program, year in YYYY format";yr
        ' empty input also stops
        If yr < 1800 Then
            End
        Else
            Exit Do
        End If
    Loop

    Print : Print
    Print "Last Sunday of the month for"; yr

    For i = 1 To 12
        d = arr(i).days
        If i = 2 AndAlso leapyear(yr) = TRUE Then d = d + 1
        x = wd(i, d, yr)
        d = d - x   ' don't test it just do it
        Print d; " "; arr(i).m_name
    Next

    ' empty key buffer
    While Inkey <> "" : keypress = Inkey : Wend
    Print : Print
    Print "Find last Sunday for a other year [Y|y], anything else stops"
    keypress =""
    While keypress = "" : keypress = Inkey : Wend
    If LCase(keypress) <> "y" Then Exit Do
    Print : Print

Loop
End
```

```txt
For what year do you want to find the last Sunday of the month
any number below 1800 stops program, year in YYYY format? 2017

Last Sunday of the month for 2017
 29 January
 26 February
 26 March
 30 April
 28 May
 25 June
 30 July
 27 August
 24 September
 29 October
 26 November
 31 December
```



## Gambas


```gambas
Public Sub Form_Open()
Dim sYear As String                                                             'To store the year chosen
Dim siDay, siMonth, siWeekday As Short                                          'Day, Month and Weekday

sYear = InputBox("Input year", "Last Sunday of each month")                     'Get the user to enter a year
Print "Last Sundays in " & sYear                                                'Print a heading

For siMonth = 1 To 12                                                           'Loop for each month
  For siDay = 31 DownTo 23                                                      'Count backwards from 31 to 23 (Sunday 23rd February 1930)
    siWeekday = 7                                                               'Set the Weekday to Saturday in case of error in the next line
    Try siWeekday = WeekDay(Date(Val(sYear), siMonth, siDay))                   'TRY and get the Weekday. If there is an error it will be ignored e.g. 31 February
    If siWeekday = 0 Then                                                       'If Weekday is Sunday then..
      Print Format(Date(Val(sYear), siMonth, siDay), "dddd dd mmmm yyyy")       'Print the date
      Break                                                                     'Jump out of this loop
    End If
  Next
Next

End
```

Output:

```txt

Last Sundays in 2013
Sunday 27 January 2013
Sunday 24 February 2013
Sunday 31 March 2013
Sunday 28 April 2013
Sunday 26 May 2013
Sunday 30 June 2013
Sunday 28 July 2013
Sunday 25 August 2013
Sunday 29 September 2013
Sunday 27 October 2013
Sunday 24 November 2013
Sunday 29 December 2013

```



## Go


This is different from the Go code for [[Last Friday of each month]]. It uses the fact that time functions in Go correct for dates: if you enter 32nd of october, Go corrects to 1st of November. So that if 29th of February is corrected to 1st of March, chosen year is not a leap year.


```go
package main

import (
	"fmt"
	"time"
)

func main() {

	var year int
	var t time.Time
	var lastDay = [12]int { 31,29,31,30,31,30,31,31,30,31,30,31 }

	for {
		fmt.Print("Please select a year: ")
		_, err := fmt.Scanf("%d", &year)
		if err != nil {
			fmt.Println(err)
			continue
		} else {
			break
		}
	}

	fmt.Println("Last Sundays of each month of", year)
	fmt.Println("
### ============================
")

	for i := 1;i < 13; i++ {
		j := lastDay[i-1]
		if i == 2 {
			if time.Date(int(year), time.Month(i), j, 0, 0, 0, 0, time.UTC).Month() == time.Date(int(year), time.Month(i), j-1, 0, 0, 0, 0, time.UTC).Month() {
				j = 29
			} else {
				j = 28
			}
		}
		for {
			t = time.Date(int(year), time.Month(i), j, 0, 0, 0, 0, time.UTC)
			if t.Weekday() == 0 {
				fmt.Printf("%s: %d\n", time.Month(i), j)
				break
			}
			j = j - 1
		}
	}
}

```



```txt

Please select a year: 2013
Last Sundays of each month of 2013

### ============================

January: 27
February: 24
March: 31
April: 28
May: 26
June: 30
July: 28
August: 25
September: 29
October: 27
November: 24
December: 29

```



## Groovy

Solution:

```groovy
enum Day {
    Sun, Mon, Tue, Wed, Thu, Fri, Sat
    static Day valueOf(Date d) { Day.valueOf(d.format('EEE')) }
}

def date = Date.&parse.curry('yyyy-MM-dd')
def month = { it.format('MM') }
def days = { year -> (date("${year}-01-01")..<date("${year+1}-01-01")) }
def weekDays = { dayOfWeek, year -> days(year).findAll { Day.valueOf(it) == dayOfWeek } }

def lastWeekDays = { dayOfWeek, year ->
    weekDays(dayOfWeek, year).reverse().inject([:]) { months, sunday ->
        def monthStr = month(sunday)
        !months[monthStr]  ?  months + [(monthStr):sunday]  :  months
    }.values().sort()
}
```


Test:

```groovy
def ymd = { it.format('yyyy-MM-dd') }
def lastSundays = lastWeekDays.curry(Day.Sun)
lastSundays(args[0] as int).each { println (ymd(it)) }
```


Execution (Cygwin on Windows 7):

```txt
[2201] groovy lastSundays.groovy 2013
```


```txt
2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29
```



## Haskell


```Haskell
import Data.Time.Calendar
       (Day, addDays, fromGregorian, gregorianMonthLength, showGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.List (find, intercalate, transpose)
import Data.Maybe (fromJust)

-- [1 .. 7] for [Mon .. Sun]
findWeekDay :: Int -> Day -> Day
findWeekDay dayOfWeek date =
  fromJust $
  find
    (\x ->
        let (_, _, day) = toWeekDate x
        in dayOfWeek == day)
    ((`addDays` date) <$> [-6 .. 0])

weekDayDates :: Int -> Integer -> [String]
weekDayDates dayOfWeek year =
  (showGregorian . findWeekDay dayOfWeek) .
  (fromGregorian year <*> gregorianMonthLength year) <$>
  [1 .. 12]

main :: IO ()
main =
  mapM_
    putStrLn
    (intercalate "  " <$> transpose (weekDayDates 7 <$> [2013 .. 2017]))
```

```txt
2018-01-28  2019-01-27  2020-01-26  2021-01-31  2022-01-30  2023-01-29
2018-02-25  2019-02-24  2020-02-23  2021-02-28  2022-02-27  2023-02-26
2018-03-25  2019-03-31  2020-03-29  2021-03-28  2022-03-27  2023-03-26
2018-04-29  2019-04-28  2020-04-26  2021-04-25  2022-04-24  2023-04-30
2018-05-27  2019-05-26  2020-05-31  2021-05-30  2022-05-29  2023-05-28
2018-06-24  2019-06-30  2020-06-28  2021-06-27  2022-06-26  2023-06-25
2018-07-29  2019-07-28  2020-07-26  2021-07-25  2022-07-31  2023-07-30
2018-08-26  2019-08-25  2020-08-30  2021-08-29  2022-08-28  2023-08-27
2018-09-30  2019-09-29  2020-09-27  2021-09-26  2022-09-25  2023-09-24
2018-10-28  2019-10-27  2020-10-25  2021-10-31  2022-10-30  2023-10-29
2018-11-25  2019-11-24  2020-11-29  2021-11-28  2022-11-27  2023-11-26
2018-12-30  2019-12-29  2020-12-27  2021-12-26  2022-12-25  2023-12-31
```


=={{header|Icon}} and {{header|Unicon}}==

This is a trivial adaptation of the solution to the "Last Friday of each month" task
and works in both languages:

```unicon
procedure main(A)
every write(lastsundays(!A))
end

procedure lastsundays(year)
every m := 1 to 12 do {
   d := case m of {
      2        : if IsLeapYear(year) then 29 else 28
      4|6|9|11 : 30
      default  : 31
      }                          # last day of month

   z := 0
   j := julian(m,d,year) + 1     # first day of next month
   until (j-:=1)%7 = 6 do z -:=1 # backup to last sunday (6)
   suspend sprintf("%d-%d-%d",year,m,d+z)
   }
end

link datetime, printf
```


Sample run:

```txt

->lsem 2013 2014
2013-1-27
2013-2-24
2013-3-31
2013-4-28
2013-5-26
2013-6-30
2013-7-28
2013-8-25
2013-9-29
2013-10-27
2013-11-24
2013-12-29
2014-1-26
2014-2-23
2014-3-30
2014-4-27
2014-5-25
2014-6-29
2014-7-27
2014-8-31
2014-9-28
2014-10-26
2014-11-30
2014-12-28
->

```



## J

Same solution as for [[Last_Friday_of_each_month#J]]

```j
require'dates'
last_sundays=: 12 {. [: ({:/.~ }:"1)@(#~ 0 = weekday)@todate (i.366) + todayno@,&1 1
```


Example use:

```j
   last_sundays 2013
2013  1 27
2013  2 24
2013  3 31
2013  4 28
2013  5 26
2013  6 30
2013  7 28
2013  8 25
2013  9 29
2013 10 27
2013 11 24
2013 12 29
```



## Java


```java
import java.util.Scanner;

public class LastSunday
{
	static final String[] months={"January","February","March","April","May","June","July","August","September","October","November","December"};

	public static int[] findLastSunday(int year)
	{
		boolean isLeap = isLeapYear(year);

		int[] days={31,isLeap?29:28,31,30,31,30,31,31,30,31,30,31};
		int[] lastDay=new int[12];

		for(int m=0;i<12;i++)
		{
			int d;
			for(d=days[m]; getWeekDay(year,m,d)!=0; d--)
				;
			lastDay[m]=d;
		}

		return lastDay;
	}

	private static boolean isLeapYear(int year)
	{
		if(year%4==0)
		{
			if(year%100!=0)
				return true;
			else if (year%400==0)
				return true;
		}
		return false;
	}

	private static int getWeekDay(int y, int m, int d)
	{
		int f=y+d+3*m-1;
		m++;

		if(m<3)
			y--;
		else
			f-=(int)(0.4*m+2.3);

		f+=(int)(y/4)-(int)((y/100+1)*0.75);
		f%=7;

		return f;
	}

	private static void display(int year, int[] lastDay)
	{
		System.out.println("\nYEAR: "+year);
		for(int m=0;i<12;i++)
			System.out.println(months[m]+": "+lastDay[m]);
	}

	public static void main(String[] args) throws Exception
	{
		System.out.print("Enter year: ");
		Scanner s=new Scanner(System.in);

		int y=Integer.parseInt(s.next());

		int[] lastDay = findLastSunday(y);
		display(y, lastDay);

		s.close();
	}
}
```


```txt

Enter year: 2013

YEAR: 2013
January: 27
February: 24
March: 31
April: 28
May: 26
June: 30
July: 28
August: 25
September: 29
October: 27
November: 24
December: 29

```


### Java 8


```Java
import java.time.*;
import java.util.stream.*;
import static java.time.temporal.TemporalAdjusters.*;

public class FindTheLastSundayOfEachMonth {
    public static Stream<LocalDate> lastSundaysOf(int year) {
        return IntStream.rangeClosed(1, 12).mapToObj(month ->
            LocalDate.of(year, month, 1).with(lastDayOfMonth())
            .with(previousOrSame(DayOfWeek.SUNDAY))
        );
    }

    public static java.util.List<LocalDate> listLastSundaysOf(int year) {
        return lastSundaysOf(year).collect(Collectors.toList());
    }

    public static void main(String[] args) throws Exception {
        int year = args.length > 0 ? Integer.parseInt(args[0]) : LocalDate.now().getYear();

        for (LocalDate d : listLastSundaysOf(year)) {
            System.out.println(d);
        };

        String result = lastSundaysOf(2013).map(LocalDate::toString).collect(Collectors.joining("\n"));
        String test = "2013-01-27\n2013-02-24\n2013-03-31\n2013-04-28\n2013-05-26\n2013-06-30\n2013-07-28\n2013-08-25\n2013-09-29\n2013-10-27\n2013-11-24\n2013-12-29";
        if (!test.equals(result)) throw new AssertionError("test failure");
    }

}
```



## JavaScript


### ES5


### =Iteration=


```javascript
function lastSundayOfEachMonths(year) {
	var lastDay = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
	var sundays = [];
	var date, month;
	if (year % 4 === 0 && (year % 100 !== 0 || year % 400 === 0)) {
		lastDay[2] = 29;
	}
	for (date = new Date(), month = 0; month < 12; month += 1) {
		date.setFullYear(year, month, lastDay[month]);
		date.setDate(date.getDate() - date.getDay());
		sundays.push(date.toISOString().substring(0, 10));
	}
	return sundays;
}

console.log(lastSundayOfEachMonths(2013).join('\n'));
```

```txt
2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29
```



### =Functional composition=


```JavaScript
(function () {
    'use strict';

    // lastSundaysOfYear :: Int -> [Date]
    function lastSundaysOfYear(y) {
        return lastWeekDaysOfYear(y, days.sunday);
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
            .map(lastSundaysOfYear)
        )
        .map(function (row) {
            return row
                .map(isoDateString)
                .join('\t');
        })
        .join('\n');

})();
```


```txt
2013-01-27	2014-01-26	2015-01-25	2016-01-31	2017-01-29
2013-02-24	2014-02-23	2015-02-22	2016-02-28	2017-02-26
2013-03-31	2014-03-30	2015-03-29	2016-03-27	2017-03-26
2013-04-28	2014-04-27	2015-04-26	2016-04-24	2017-04-30
2013-05-26	2014-05-25	2015-05-31	2016-05-29	2017-05-28
2013-06-30	2014-06-29	2015-06-28	2016-06-26	2017-06-25
2013-07-28	2014-07-27	2015-07-26	2016-07-31	2017-07-30
2013-08-25	2014-08-31	2015-08-30	2016-08-28	2017-08-27
2013-09-29	2014-09-28	2015-09-27	2016-09-25	2017-09-24
2013-10-27	2014-10-26	2015-10-25	2016-10-30	2017-10-29
2013-11-24	2014-11-30	2015-11-29	2016-11-27	2017-11-26
2013-12-29	2014-12-28	2015-12-27	2016-12-25	2017-12-31
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
            .map(curry(lastWeekDaysOfYear)(days.sunday))
        )
        .map(row => row
            .map(isoDateString)
            .join('\t'))
        .join('\n');
})();
```

```txt
2015-01-25    2016-01-31    2017-01-29    2018-01-28    2019-01-27
2015-02-22    2016-02-28    2017-02-26    2018-02-25    2019-02-24
2015-03-29    2016-03-27    2017-03-26    2018-03-25    2019-03-31
2015-04-26    2016-04-24    2017-04-30    2018-04-29    2019-04-28
2015-05-31    2016-05-29    2017-05-28    2018-05-27    2019-05-26
2015-06-28    2016-06-26    2017-06-25    2018-06-24    2019-06-30
2015-07-26    2016-07-31    2017-07-30    2018-07-29    2019-07-28
2015-08-30    2016-08-28    2017-08-27    2018-08-26    2019-08-25
2015-09-27    2016-09-25    2017-09-24    2018-09-30    2019-09-29
2015-10-25    2016-10-30    2017-10-29    2018-10-28    2019-10-27
2015-11-29    2016-11-27    2017-11-26    2018-11-25    2019-11-24
2015-12-27    2016-12-25    2017-12-31    2018-12-30    2019-12-29
```



## jq

'''Foundations'''

```jq
# In case your jq does not have "until" defined:
def until(cond; next):
  def _until:
    if cond then . else (next|_until) end;
  _until;

# Zeller's Congruence from [[Day_of_the_week#jq]]

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

'''findLastSundays'''

```jq
# year and month are numbered conventionally
def findLastSunday(year; month):
  def isLeapYear:
    year%4 == 0 and ( year%100!=0 or year%400==0 ) ;
  def days:
    if month == 2 then (if isLeapYear then 29 else 28 end)
    else [31, 28, 31,30,31,30,31,31,30,31,30,31][month-1]
    end;
  year as $year
  | month as $month
  | days
  | until( day_of_week($year; $month; .; null) == 1 ; .-1);

# input: year
def findLastSundays:
  def months:
    ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
  . as $year
  | "YEAR: \(.)",
    (range(0;12) | "\(months[.]) \(findLastSunday($year; .+1))") ;

$year|tonumber|findLastSundays

```

'''Example:'''

```sh
$ jq --arg year 2013 -n -r -f findLastSundays.jq
YEAR: 2013
January 27
February 24
March 31
April 28
May 26
June 30
July 28
August 25
September 29
October 27
November 24
December 29
```



## Julia


```Julia

isdefined(:Date) || using Dates

const wday = Dates.Sun
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
        println("Sorry, but that does not compute as a year.")
        continue
    end
    println()
    for m in Date(y, lo):Month(1):Date(y, hi)
        println("    ", tolast(m, wday))
    end
end

```


This code uses the <code>Dates</code> module, which is being incorporated into Julian's standard library with the current development version (<tt>0.4</tt>).  I've used <code>isdefined</code> to make this code good for the current stable version (<tt>0.3</tt>) as well as for future releases.  If <code>Dates</code> is not installed on your instance of Julian try <code>Pkg.add("Dates")</code> from the <tt>REPL</tt>.

```txt

This script will print the last Sundays of each month of the year given.
(Leave input empty to quit.)

Year> 2013

    2013-01-27
    2013-02-24
    2013-03-31
    2013-04-28
    2013-05-26
    2013-06-30
    2013-07-28
    2013-08-25
    2013-09-29
    2013-10-27
    2013-11-24
    2013-12-29

Year> this year
Sorry, but that does not compute as a year.

Year>

```



## K


```K

/ List the dates of last Sundays of each month of
/ a given year
/ lastsundt.k

isleap: {(+/~x!' 4 100 400)!2}
wd: {(_jd x)!7}
dom: (31;28;31;30;31;30;31;31;30;31;30;31)
init: {:[isleap x;dom[1]::29;dom[1]::28]}
wdme: {[m;y]; init y; dt:(10000*y)+(100*m)+dom[m-1];jd::(_jd dt);mewd::(wd dt)}
lsd: {[m;y]; wdme[m;y];:[mewd>5;jd::jd+(6-mewd);jd::jd-(1+mewd)];dt:_dj(jd);yy:$(yr:dt%10000);dd:$(d:dt!100);mm:$(mo:((dt-yr*10000)%100));arr::arr,$(yy,"-",(2$mm),"-",(2$dd))}
lsd1: {[y];arr::(); m:1; do[12;lsd[m;y];m+:1]}
main: {[y]; lsd1[y];`0: ,"Dates of last Sundays of ",($y); 12 10#arr}


```

The output of a session with the above script is given below:
```txt

K Console - Enter \ for help

  \l lastsundt
  main 2013
Dates of last Sundays of 2013
("2013- 1-27"
 "2013- 2-24"
 "2013- 3-31"
 "2013- 4-28"
 "2013- 5-26"
 "2013- 6-30"
 "2013- 7-28"
 "2013- 8-25"
 "2013- 9-29"
 "2013-10-27"
 "2013-11-24"
 "2013-12-29")


```



## Kotlin


```scala
// version 1.0.6

import java.util.*

fun main(args: Array<String>) {
    print("Enter a year : ")
    val year = readLine()!!.toInt()

    println("The last Sundays of each month in $year are as follows:")
    val calendar = GregorianCalendar(year, 0, 31)
    for (month in 1..12) {
        val daysInMonth = calendar.getActualMaximum(Calendar.DAY_OF_MONTH)
        val lastSunday = daysInMonth - (calendar[Calendar.DAY_OF_WEEK] - Calendar.SUNDAY)
        println("$year-" + "%02d-".format(month) + "%02d".format(lastSunday))
        if (month < 12) {
            calendar.add(Calendar.DAY_OF_MONTH, 1)
            calendar.add(Calendar.MONTH, 1)
            calendar.add(Calendar.DAY_OF_MONTH, -1)
        }
    }
}
```

Sample input/output:
```txt

Enter a year : 2013
The last Sundays of each month in 2013 are as follows:
2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29

```



## Lasso


```Lasso
local(
	year	= integer(web_request -> param('year') || 2013),
	date	= date(#year + '-1-1'),
	lastsu	= array,
	lastday
)

with month in generateseries(1,12) do {
	#date -> day = 1
	#date -> month = #month
	#lastday = #date -> month(-days)
	#date -> day = #lastday
	loop(7)	=> {
		if(#date -> dayofweek == 1) => {
			#lastsu -> insert(#date -> format(`dd MMMM`))
			loop_abort
		}
		#date -> day--
	}
}
#lastsu -> join('<br />')
```


```txt
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



## Liberty BASIC

This program goes a step further and provides a function definition, NthDayOfMonth(), that returns the date of any occurrence of a day in any given month and year. For example: The third Monday in February, the last Monday in May, the fourth Thursday in November, etc.

```lb

yyyy=2013: if yyyy<1901 or yyyy>2099 then end
nda$="Lsu"
print "The last Sundays of "; yyyy
for mm=1 to 12
    x=NthDayOfMonth(yyyy, mm, nda$)
    select case mm
        case  1: print "   January ";   x
        case  2: print "   February ";  x
        case  3: print "   March ";     x
        case  4: print "   April ";     x
        case  5: print "   May ";       x
        case  6: print "   June ";      x
        case  7: print "   July ";      x
        case  8: print "   August ";    x
        case  9: print "   September "; x
        case 10: print "   October ";   x
        case 11: print "   November ";  x
        case 12: print "   December ";  x
    end select
next mm
end

function NthDayOfMonth(yyyy, mm, nda$)
' nda$ is a two-part code. The first character, n, denotes
' first, second, third, fourth, and last by 1, 2, 3, 4, or L.
' The last two characters, da, denote the day of the week by
' mo, tu, we, th, fr, sa, or su. For example:
' the nda$ for the second Monday of a month is "2mo";
' the nda$ for the last Thursday of a month is "Lth".
    if yyyy<1900 or yyyy>2099 or mm<1 or mm>12 then
        NthDayOfMonth=0: exit function
    end if
    nda$=lower$(trim$(nda$))
    if len(nda$)<>3 then NthDayOfMonth=0: exit function
    n$=left$(nda$,1): nC$="1234l"
    da$=right$(nda$,2): daC$="tuwethfrsasumotuwethfrsasumo"
    if not(instr(nC$,n$)) or not(instr(daC$,da$)) then
        NthDayOfMonth=0: exit function
    end if
    NthDayOfMonth=1
    mm$=str$(mm): if mm<10 then mm$="0"+mm$
    db$=DayOfDate$(str$(yyyy)+mm$+"01")
    if da$<>db$ then
        x=instr(daC$,db$): y=instr(daC$,da$,x): NthDayOfMonth=1+(y-x)/2
    end if
    dim MD(12)
    MD(1)=31: MD(2)=28: MD(3)=31: MD(4)=30: MD(5)=31: MD(6)=30
    MD(7)=31: MD(8)=31: MD(9)=30: MD(10)=31: MD(11)=30: MD(12)=31
    if yyyy mod 4 = 0 then MD(2)=29
    if n$<>"1" then
        if n$<>"l" then
            NthDayOfMonth=NthDayOfMonth+((val(n$)-1)*7)
        else
            if NthDayOfMonth+27<MD(mm) then
                NthDayOfMonth=NthDayOfMonth+28
            else
                NthDayOfMonth=NthDayOfMonth+21
            end if
        end if
    end if
end function

function DayOfDate$(ObjectDate$) 'yyyymmdd format
    if ObjectDate$="" then 'today
        DaysSince1900 = date$("days")
    else
        DaysSince1900 = date$(mid$(ObjectDate$,5,2)+"/"+right$(ObjectDate$,2)_
                +"/"+left$(ObjectDate$,4))
    end if
    DayOfWeek = DaysSince1900 mod 7
    select case DayOfWeek
        case 0: DayOfDate$="tu"
        case 1: DayOfDate$="we"
        case 2: DayOfDate$="th"
        case 3: DayOfDate$="fr"
        case 4: DayOfDate$="sa"
        case 5: DayOfDate$="su"
        case 6: DayOfDate$="mo"
    end select
end function

```

```txt

The last Sundays of 2013
   January 27
   February 24
   March 31
   April 28
   May 26
   June 30
   July 28
   August 25
   September 29
   October 27
   November 24
   December 29

```



## LiveCode

Abstracted version of "last friday of each month". LiveCode dateItem item 7 is day of week. It is numbered 1 (Sunday) to 7 (Saturday).

```LiveCode
function lastDay yyyy, dayofweek
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
            -- 1 is Sunday through to 7 Saturday
            if item 7 of startDate is dayofweek and item 1 of startDate is yyyy and item 2 of startDate is m then
                put item 3 of startDate into mydays[item 2 of startDate]
            end if
        end repeat
    end repeat
    combine mydays using cr and space
    sort mydays ascending numeric
    return mydays
end lastDay
```
Example
```LiveCode
put lastDay(2013, 1)
```
Output
```txt
1 27
2 24
3 31
4 28
5 26
6 30
7 28
8 25
9 29
10 27
11 24
12 29
```



## Lua


```Lua
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

lastWeekdays("Sunday", tonumber(arg[1]))
```

Command line session:

```txt
>lua lastSundays.lua 2013
2013-1-27
2013-2-24
2013-3-31
2013-4-28
2013-5-26
2013-6-30
2013-7-28
2013-8-25
2013-9-29
2013-10-27
2013-11-24
2013-12-29

>
```



## Maple


```Maple
sundays := proc(year)
	local i, dt, change, last_days;
	last_days := [31,28,31,30,31,30,31,31,30,31,30,31];
	if (Calendar:-IsLeapYear(year)) then
		last_days[2] := 28;
	end if;
	for i to 12 do
		dt := Date(year, i, last_days[i]);
		change := 0;
		if not(Calendar:-DayOfWeek(dt) = 1) then
			change := -Calendar:-DayOfWeek(dt) + 1;
		end if;
		dt := Calendar:-AdjustDateField(dt, "date", change);
		printf("%d-%d-%d\n", year, Month(dt), DayOfMonth(dt));
	end do;
end proc;

sundays(2013);
```

```txt

2013-1-27
2013-2-24
2013-3-31
2013-4-28
2013-5-26
2013-6-30
2013-7-28
2013-8-25
2013-9-29
2013-10-27
2013-11-24
2013-12-29

```

=={{header|Mathematica}} / {{header|Wolfram Language}}==

```mathematica
LastSundays[year_] :=
 Table[Last@
   DayRange[{year, i},
    DatePlus[{year, i}, {{1, "Month"}, {-1, "Day"}}], Sunday], {i,
   12}]
LastSundays[2013]
```

```txt
{{2013, 1, 27}, {2013, 2, 24}, {2013, 3, 31}, {2013, 4, 28}, {2013, 5,
   26}, {2013, 6, 30}, {2013, 7, 28}, {2013, 8, 25}, {2013, 9,
  29}, {2013, 10, 27}, {2013, 11, 24}, {2013, 12, 29}}
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
    if t.month == month and t.weekday == dSun:
      echo t.format "yyyy-MM-dd"
      break
```

Sample usage:

```txt
./lastsunday 2013
2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29
```



## OCaml


```OCaml

let is_leap_year y =
  (* See OCaml solution on Rosetta Code for
     determing if it's a leap year *)
  if (y mod 100) = 0
    then (y mod 400) = 0
    else (y mod 4) = 0;;

let get_days y =
  if is_leap_year y
    then
      [31;29;31;30;31;30;31;31;30;31;30;31]
    else
      [31;28;31;30;31;30;31;31;30;31;30;31];;

let print_date = Printf.printf "%d/%d/%d\n";;

let get_day_of_week y m d =
  let y = if m > 2 then y else y - 1 in
  let c = y / 100 in
  let y = y mod 100 in
  let m_shifted = float_of_int ( ((m + 9) mod 12) + 1) in
  let m_factor = int_of_float (2.6 *. m_shifted -. 0.2) in
  let leap_factor = 5 * (y mod 4) + 3 * (y mod 7) + 5 * (c mod 4) in
  (d + m_factor + leap_factor) mod 7;;

let get_shift y m last_day =
  get_day_of_week y m last_day;;

let print_last_sunday y m =
  let days = get_days y in
  let last_day = List.nth days (m - 1) in
  let last_sunday = last_day - (get_shift y m last_day) in
  print_date y m last_sunday;;

let print_last_sundays y =
  let months = [1;2;3;4;5;6;7;8;9;10;11;12] in
  List.iter (print_last_sunday y) months;;

match (Array.length Sys.argv ) with
  2 -> print_last_sundays( int_of_string (Sys.argv.(1)));
 |_ -> invalid_arg "Please enter a year";

```

Sample usage:

```txt
 ocaml sundays.ml  2013
2013/01/27
2013/02/24
2013/03/31
2013/04/28
2013/05/26
2013/06/30
2013/07/28
2013/08/25
2013/09/29
2013/10/27
2013/11/24
2013/12/29
```



## Oforth


```Oforth
import: date

: lastSunday(y)
| m |
   Date.JANUARY Date.DECEMBER for: m [
      Date newDate(y, m, Date.DaysInMonth(y, m))
      while(dup dayOfWeek Date.SUNDAY <>) [ addDays(-1) ] println
      ] ;
```


```txt

2013-01-27 00:00:00,000
2013-02-24 00:00:00,000
2013-03-31 00:00:00,000
2013-04-28 00:00:00,000
2013-05-26 00:00:00,000
2013-06-30 00:00:00,000
2013-07-28 00:00:00,000
2013-08-25 00:00:00,000
2013-09-29 00:00:00,000
2013-10-27 00:00:00,000
2013-11-24 00:00:00,000
2013-12-29 00:00:00,000

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

for (m=1, 12, a=njd([2013,m+1,0]); print(njdate(a-(a+6)%7)))
```


Output:
```txt

[2013, 1, 27]
[2013, 2, 24]
[2013, 3, 31]
[2013, 4, 28]
[2013, 5, 26]
[2013, 6, 30]
[2013, 7, 28]
[2013, 8, 25]
[2013, 9, 29]
[2013, 10, 27]
[2013, 11, 24]
[2013, 12, 29]

```



## Perl


```Perl
#!/usr/bin/perl
use strict ;
use warnings ;
use DateTime ;

for my $i( 1..12 ) {
   my $date = DateTime->last_day_of_month( year => $ARGV[ 0 ] ,
	 month => $i ) ;
   while ( $date->dow != 7 ) {
      $date = $date->subtract( days => 1 ) ;
   }
   my $ymd = $date->ymd ;
   print "$ymd\n" ;
}
```

```txt
2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29

```



## Perl 6


```perl6
sub MAIN ($year = Date.today.year) {
    for 1..12 -> $month {
        my $month-end = Date.new($year, $month, Date.new($year,$month,1).days-in-month);
        say $month-end - $month-end.day-of-week % 7;
    }
}
```


```txt
2018-01-28
2018-02-25
2018-03-25
2018-04-29
2018-05-27
2018-06-24
2018-07-29
2018-08-26
2018-09-30
2018-10-28
2018-11-25
2018-12-30
```



## Phix


```Phix
include timedate.e

constant SUNDAY=1

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
last_day_of_month(2013,SUNDAY)
```

```txt

2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29

```



## PHP


```PHP
<?php
// Created with PHP 7.0

function printLastSundayOfAllMonth($year)
{
    $months = array(
        'January', 'February', 'March', 'April', 'June', 'July',
        'August', 'September', 'October', 'November', 'December');

    foreach ($months as $month) {
        echo $month . ': ' . date('Y-m-d', strtotime('last sunday of ' . $month . ' ' . $year)) . "\n";
    }
}

printLastSundayOfAllMonth($argv[1]);

```

```txt

January: 2013-01-27
February: 2013-02-24
March: 2013-03-31
April: 2013-04-28
June: 2013-06-30
July: 2013-07-28
August: 2013-08-25
September: 2013-09-29
October: 2013-10-27
November: 2013-11-24
December: 2013-12-29

```



## PicoLisp


```PicoLisp
(de lastSundays (Y)
   (for M 12
      (prinl
         (dat$
            (find '((D) (= "Sunday" (day D)))
               (mapcar '((D) (date Y M D)) `(range 31 22)) )
            "-" ) ) ) )
```

Test:

```PicoLisp
: (lastSundays 2013)
2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29
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
last-dayofweek 2013 "Sunday"

```

<b>Output:</b>

```txt

2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29

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

1..12 | Get-Date0fDayOfWeek -Year 2013 -Last -Sunday

```

```txt

Sunday, January 27, 2013 12:00:00 AM
Sunday, February 24, 2013 12:00:00 AM
Sunday, March 31, 2013 12:00:00 AM
Sunday, April 28, 2013 12:00:00 AM
Sunday, May 26, 2013 12:00:00 AM
Sunday, June 30, 2013 12:00:00 AM
Sunday, July 28, 2013 12:00:00 AM
Sunday, August 25, 2013 12:00:00 AM
Sunday, September 29, 2013 12:00:00 AM
Sunday, October 27, 2013 12:00:00 AM
Sunday, November 24, 2013 12:00:00 AM
Sunday, December 29, 2013 12:00:00 AM

```

Return the <code>[DateTime]</code> objects as strings (using the default string format):

```PowerShell

1..12 | Get-Date0fDayOfWeek -Year 2013 -Last -Sunday -AsString

```

```txt

27-Jan-2013
24-Feb-2013
31-Mar-2013
28-Apr-2013
26-May-2013
30-Jun-2013
28-Jul-2013
25-Aug-2013
29-Sep-2013
27-Oct-2013
24-Nov-2013
29-Dec-2013

```

Return the <code>[DateTime]</code> objects as strings (specifying the string format):

```PowerShell

1..12 | Get-Date0fDayOfWeek -Year 2013 -Last -Sunday -AsString -Format yyyy-MM-dd

```

```txt

2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29

```



## PureBasic


```PureBasic
Procedure LastSundayOfEachMonth(yyyy.i,List lfem.i())
  Define dv.i=ParseDate("%yyyy",Str(yyyy)), mv.i=1
  NewList d.i()
  For d=1 To 365
    dv=AddDate(dv,#PB_Date_Day,1)
    If DayOfWeek(dv)=0
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
OpenConsole("Last Sunday of each month")
Print("Input Year [ 1971 < y < 2038 ]: ")
y=Val(Input())
If y>1971 And y<2038
  PrintN("Last Sunday of each month...")
  LastSundayOfEachMonth(y,lf())
  ForEach lf()
    PrintN(FormatDate("%dd.%mm.%yyyy",lf()))
  Next
EndIf
Print("...End")
Input()
```

```txt
Input Year [ 1971 < y < 2038 ]: 2013
Last Sunday of each month...
27.01.2013
24.02.2013
31.03.2013
28.04.2013
26.05.2013
30.06.2013
28.07.2013
25.08.2013
29.09.2013
27.10.2013
24.11.2013
29.12.2013
...End
```



## Python


```python

import sys
import calendar

year = 2013
if len(sys.argv) > 1:
    try:
        year = int(sys.argv[-1])
    except ValueError:
        pass

for month in range(1, 13):
    last_sunday = max(week[-1] for week in calendar.monthcalendar(year, month))
    print('{}-{}-{:2}'.format(year, calendar.month_abbr[month], last_sunday))

```


<b>Output</b>:
<lang>
2013-Jan-27
2013-Feb-24
2013-Mar-31
2013-Apr-28
2013-May-26
2013-Jun-30
2013-Jul-28
2013-Aug-25
2013-Sep-29
2013-Oct-27
2013-Nov-24
2013-Dec-29

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

(define (last-sundays y)
  (for/list ([m (in-range 1 13)])
    (prev-sunday (last-day-in-month m y))))

(define 24hours (make-time time-duration 0 (* 24 60 60)))

(define (prev-day d)
  (time-utc->date
   (subtract-duration
    (date->time-utc d) 24hours)))

(define (prev-sunday d)
  (if (eq? (week-day d) 'sun)
      d
      (prev-sunday (prev-day d))))

(for ([d (last-sundays 2013)])
  (displayln (~a (date->string d "~a ~d ~b ~Y"))))

```

```txt

Sun 27 Jan 2013
Sun 24 Feb 2013
Sun 31 Mar 2013
Sun 28 Apr 2013
Sun 26 May 2013
Sun 30 Jun 2013
Sun 28 Jul 2013
Sun 25 Aug 2013
Sun 29 Sep 2013
Sun 27 Oct 2013
Sun 24 Nov 2013
Sun 29 Dec 2013

```



## REBOL


```REBOL
#!/usr/bin/env rebol

last-sundays-of-year: function [
    "Return series of last sunday (date!) for each month of the year"
    year [integer!] "which year?"
  ][
    d: to-date reduce [1 1 year]            ; start with first day of year
    collect [
        repeat month 12 [
            d/month: month + 1              ; move to start of next month
            keep d - d/weekday              ; calculate last sunday & keep
        ]
    ]
]

foreach sunday last-sundays-of-year to-integer system/script/args [print sunday]

```

```txt
./last-sundays.reb 2013
27-Jan-2013
24-Feb-2013
31-Mar-2013
28-Apr-2013
26-May-2013
30-Jun-2013
28-Jul-2013
25-Aug-2013
29-Sep-2013
27-Oct-2013
24-Nov-2013
29-Dec-2013

```



## REXX

This REXX example is an exact replication of the Rosetta Code
* ''find last Fridays of each month for any year''
except for the innards of the first '''DO''' loop.


The   '''lastDOW'''   subroutine can be used for any day-of-the-week for any month for any year.

```rexx
/*REXX program displays dates of last Sundays of each month for any year*/
parse arg yyyy
                   do j=1 for  12
                   _ = lastDOW('Sunday', j, yyyy)
                   say right(_,4)'-'right(j,2,0)"-"left(word(_,2),2)
                   end  /*j*/
exit                                   /*stick a fork in it, we're done.*/
/*┌────────────────────────────────────────────────────────────────────┐
  │ lastDOW:  procedure to return the date of the  last day-of-week of │
  │           any particular month  of any particular year.            │
  │                                                                    │
  │ The  day-of-week  must be specified (it can be in any case,        │
  │ (lower-/mixed-/upper-case)  as an English name of the spelled day  │
  │ of the week,   with a minimum length that causes no ambiguity.     │
  │ I.E.:   W  for Wednesday,   Sa  for Saturday,   Su  for Sunday ... │
  │                                                                    │
  │ The month can be specified as an integer   1 ──► 12                │
  │    1=January     2=February     3=March     ...     12=December    │
  │ or the English  name  of the month,  with a minimum length that    │
  │ causes no ambiguity.    I.E.:  Jun  for June,   D  for December.   │
  │ If omitted  [or an asterisk(*)],  the current month is used.       │
  │                                                                    │
  │ The year is specified as an integer or just the last two digits    │
  │ (two digit years are assumed to be in the current century,  and    │
  │ there is no windowing for a two-digit year).                       │
  │ If omitted  [or an asterisk(*)],  the current year is used.        │
  │ Years < 100   must be specified with  (at least 2)  leading zeroes.│
  │                                                                    │
  │ Method used: find the "day number" of the 1st of the next month,   │
  │ then subtract one  (this gives the "day number" of the last day of │
  │ the month,  bypassing the leapday mess).   The last day-of-week is │
  │ then obtained straightforwardly,   or  via subtraction.            │
  └────────────────────────────────────────────────────────────────────┘*/
lastdow: procedure; arg dow .,mm .,yy .              /*DOW = day of week*/
parse arg a.1,a.2,a.3                                /*orig args, errmsg*/
if mm=='' | mm=='*' then mm=left(date('U'),2)        /*use default month*/
if yy=='' | yy=='*' then yy=left(date('S'),4)        /*use default year */
if length(yy)==2 then yy=left(date('S'),2)yy         /*append century.  */
                   /*Note mandatory leading blank in strings below.*/
$=" Monday TUesday Wednesday THursday Friday SAturday SUnday"
!=" JAnuary February MARch APril MAY JUNe JULy AUgust September",
  " October November December"
upper $ !                                            /*uppercase strings*/
if dow==''                 then call .er "wasn't specified",1
if arg()>3                 then call .er 'arguments specified',4

  do j=1 for 3                                       /*any plural args ?*/
  if words(arg(j))>1       then call .er 'is illegal:',j
  end

dw=pos(' 'dow,$)                                     /*find  day-of-week*/
if dw==0                   then call .er 'is invalid:',1
if dw\==lastpos(' 'dow,$)  then call .er 'is ambigious:',1

if datatype(mm,'month') then                         /*if MM is alpha...*/
  do
  m=pos(' 'mm,!)                                     /*maybe its good...*/
  if m==0                  then call .er 'is invalid:',1
  if m\==lastpos(' 'mm,!)  then call .er 'is ambigious:',2
  mm=wordpos(word(substr(!,m),1),!)-1                /*now, use true Mon*/
  end

if \datatype(mm,'W')       then call .er "isn't an integer:",2
if \datatype(yy,'W')       then call .er "isn't an integer:",3
if mm<1 | mm>12            then call .er "isn't in range 1──►12:",2
if yy=0                    then call .er "can't be 0 (zero):",3
if yy<0                    then call .er "can't be negative:",3
if yy>9999                 then call .er "can't be > 9999:",3

tdow=wordpos(word(substr($,dw),1),$)-1               /*target DOW, 0──►6*/
                                                     /*day# of last dom.*/
_=date('B',right(yy+(mm=12),4)right(mm//12+1,2,0)"01",'S')-1
?=_//7                                               /*calc. DOW,  0──►6*/
if ?\==tdow then _=_-?-7+tdow+7*(?>tdow)             /*not DOW?  Adjust.*/
return date('weekday',_,"B") date(,_,'B')            /*return the answer*/

.er: arg ,_;say; say '***error!*** (in LASTDOW)';say /*tell error,  and */
  say word('day-of-week month year excess',arg(2)) arg(1) a._
  say; exit 13                                       /*... then exit.   */
```

{{out}} when using the default input (the current year, 2013):

```txt
2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29
```



## Ring


```ring
see "What year to calculate (yyyy) : "
give year
see "Last Sundays in " + year + " are on :" + nl
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
        x = (month[n] + i) % 7
        if  n < 10 strn = "0" + string(n) else strn = string(n) ok
        if x = 4 see year + "-" + strn + "-" + string(i) + nl ok
    next
next

```

Output:

```txt

What year to calculate (yyyy) : 2013
Last Sundays in 2013 are on :
2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29

```



## Ruby


```ruby
require 'date'

def last_sundays_of_year(year = Date.today.year)
  (1..12).map do |month|
    d = Date.new(year, month, -1) # -1 means "last".
    d - d.wday
  end
end

puts last_sundays_of_year(2013)
```


```txt

2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29

```

Results before the year 1581 may differ from other languages - the Date library takes the Julian reform into account.

## Run BASIC


```runbasic
input "What year to calculate (yyyy) : ";year
print "Last Sundays in ";year;" are:"
dim month(12)
mo$ = "4 0 0 3 5 1 3 6 2 4 0 2"
mon$ = "31 28 31 30 31 30 31 31 30 31 30 31"

if year < 2100 then leap = year - 1900 else leap = year - 1904
m = ((year-1900) mod 7) + int(leap/4) mod 7
for n = 1 to 12
    month(n) = (val(word$(mo$,n)) + m) mod 7
    month(n) = (val(word$(mo$,n)) + m) mod 7
next
for n = 1 to 12
    for i = (val(word$(mon$,n)) - 6) to val(word$(mon$,n))
        x = (month(n) + i) mod 7
        if x = 4 then print year ; "-";right$("0"+str$(n),2);"-" ; i
    next
next
```


```txt

What year to calculate (yyyy) : ?2013
Last Sundays in 2013 are:
2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29
```



## Scala


```Scala
object FindTheLastSundayOfEachMonth extends App {
  import java.util.Calendar._
  val cal = getInstance

  def lastSundaysOf(year: Int) =
    (JANUARY to DECEMBER).map{month =>
      cal.set(year, month + 1, 1) // first day of next month
      (1 to 7).find{_ => cal.add(DAY_OF_MONTH, -1); cal.get(DAY_OF_WEEK) == SUNDAY}
      cal.getTime
    }

  val year = args.headOption.map(_.toInt).getOrElse(cal.get(YEAR))
  val fmt = new java.text.SimpleDateFormat("yyyy-MM-dd")
  println(lastSundaysOf(year).map(fmt.format) mkString "\n")
}
```


### Java 8


```Scala
object FindTheLastSundayOfEachMonth extends App {
  def lastSundaysOf(year: Int) = (1 to 12).map{month =>
    import java.time._; import java.time.temporal.TemporalAdjusters._
    LocalDate.of(year, month, 1).`with`(lastDayOfMonth).`with`(previousOrSame(DayOfWeek.SUNDAY))}

  val year = args.headOption.map(_.toInt).getOrElse(java.time.LocalDate.now.getYear)
  println(lastSundaysOf(year) mkString "\n")
}
```



## Seed7

Uses the libraries [http://seed7.sourceforge.net/libraries/time.htm time.s7i] and
[http://seed7.sourceforge.net/libraries/duration.htm duration.s7i].
Applicable to any day of the week, cf. [[http://rosettacode.org/wiki/Last_Friday_of_each_month#Seed7]].


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


{{out}} when called with <tt>s7 rosetta/lastWeekdayInMonth 7 2013</tt>:

```txt

2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29

```



## Sidef


```ruby
var dt = require('DateTime');
var (year=2016) = ARGV.map{.to_i}...

for i in (1 .. 12) {
    var date = dt.last_day_of_month(
        year  => year,
        month => i
    );

    while (date.dow != 7) {
        date = date.subtract(days => 1);
    }

    say date.ymd;
}
```

```txt

$ sidef last_sunday.sf 2013
2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29

```



## Smalltalk


```smalltalk

Pharo Smalltalk

[ :yr | | firstDay firstSunday |
  firstDay := Date year: yr month: 1 day: 1.
  firstSunday := firstDay addDays: (1 - firstDay dayOfWeek).
    (0 to: 53)
      collect: [ :each | firstSunday addDays: (each * 7) ]
      thenSelect: [ :each |
        (((Date daysInMonth: each monthIndex forYear: yr) - each dayOfMonth) <= 6) and: [ each year = yr ] ] ]

```

```txt

Send value: 2013 to the above block to return an array:
(27 January 2013 24 February 2013 31 March 2013 28 April 2013 26 May 2013 30 June 2013 28 July 2013 25 August 2013 29 September 2013 27 October 2013 24 November 2013 29 December 2013)

```



## Stata


```stata
program last_sundays
	args year
	clear
	qui set obs 12
	gen day=dofm(mofd(mdy(_n,1,`year'))+1)-1
	qui replace day=day-mod(dow(day),7)
	format %td day
	list, noobs noheader sep(6)
end

last_sundays 2013

  +-----------+
  | 27jan2013 |
  | 24feb2013 |
  | 31mar2013 |
  | 28apr2013 |
  | 26may2013 |
  | 30jun2013 |
  |-----------|
  | 28jul2013 |
  | 25aug2013 |
  | 29sep2013 |
  | 27oct2013 |
  | 24nov2013 |
  | 29dec2013 |
  +-----------+
```



## Swift


```Swift
import Foundation

func lastSundays(of year: Int) -> [Date] {

	let calendar = Calendar.current
	var dates = [Date]()

	for month in 1...12 {

		var dateComponents = DateComponents(calendar: calendar,
		                                    year: year,
		                                    month: month + 1,
		                                    day: 0,
		                                    hour: 12)

		let date = calendar.date(from: dateComponents)!
		let weekday = calendar.component(.weekday, from: date)

		if weekday != 1 {
			dateComponents.day! -= weekday - 1
		}

		dates.append(calendar.date(from: dateComponents)!)
	}
	return dates
}

var dateFormatter = DateFormatter()
dateFormatter.dateStyle = .short

print(lastSundays(of: 2013).map(dateFormatter.string).joined(separator: "\n"))
```

```txt

1/27/13
2/24/13
3/31/13
4/28/13
5/26/13
6/30/13
7/28/13
8/25/13
9/29/13
10/27/13
11/24/13
12/29/13

```



## Tcl


```tcl
proc lastSundays {{year ""}} {
    if {$year eq ""} {
	set year [clock format [clock seconds] -gmt 1 -format "%Y"]
    }
    foreach month {2 3 4 5 6 7 8 9 10 11 12 13} {
	set d [clock add [clock scan "$month/1/$year" -gmt 1] -1 day]
	while {[clock format $d -gmt 1 -format "%u"] != 7} {
	    set d [clock add $d -1 day]
	}
	lappend result [clock format $d -gmt 1 -format "%Y-%m-%d"]
    }
    return $result
}
puts [join [lastSundays {*}$argv] "\n"]
```

When called as: “<code>tclsh lastSundays.tcl 2013</code>” (or with the year argument omitted during 2013)

```txt

2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-20
2013-11-24
2013-12-29

```



## VBScript

```VBScript

strYear = WScript.StdIn.ReadLine

For i = 1 To 12
	d = DateSerial(strYear, i + 1, 1) - 1
	WScript.Echo d - Weekday(d) + 1
Next

```



## zkl


The program from [[http://rosettacode.org/wiki/Last_Friday_of_each_month#zkl]] solves this task, as well.

```txt
var [const] D=Time.Date;
lastDay(2013,D.Sunday)
2013-01-27
2013-02-24
2013-03-31
2013-04-28
2013-05-26
2013-06-30
2013-07-28
2013-08-25
2013-09-29
2013-10-27
2013-11-24
2013-12-29
```

