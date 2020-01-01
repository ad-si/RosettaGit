+++
title = "Date format"
description = ""
date = 2019-05-16T00:23:40Z
aliases = []
[extra]
id = 2098
[taxonomies]
categories = []
tags = []
+++

{{task|Text processing}}
{{Clarified-review}}

;Task:
Display the   current date   in the formats of:
:::*    '''2007-11-23'''     and
:::*    '''Sunday, November 23, 2007'''





## 11l


```11l
print(Time().format(‘YYYY-MM-DD’))
print(Time().strftime(‘%A, %B %e, %Y’))
```



## 8th


```forth

d:new
"%Y-%M-%D" over d:format . cr
"%W, %N %D, %Y" over d:format . cr
bye

```



## ABAP


```ABAP

report zdate.
data: lv_month type string,
      lv_weekday type string,
      lv_date type string,
      lv_day type c.

call function 'DATE_COMPUTE_DAY'
  exporting date = sy-datum
  importing day  = lv_day.
select single ltx from t247 into lv_month
  where spras = sy-langu and
  mnr = sy-datum+4(2).

select single langt from t246 into lv_weekday
  where sprsl = sy-langu and
  wotnr = lv_day.

concatenate lv_weekday ', ' lv_month ' ' sy-datum+6(2) ', ' sy-datum(4) into lv_date respecting blanks.
write lv_date.
concatenate sy-datum(4) '-' sy-datum+4(2) '-' sy-datum+6(2) into lv_date.
write / lv_date.

```



## Ada


```ada
with Ada.Calendar;             use Ada.Calendar;
with Ada.Calendar.Formatting;  use Ada.Calendar.Formatting;
with Ada.Text_IO;              use Ada.Text_IO;

procedure Date_Format is
   function Image (Month : Month_Number) return String is
   begin
      case Month is
         when 1  => return "January";
         when 2  => return "February";
         when 3  => return "March";
         when 4  => return "April";
         when 5  => return "May";
         when 6  => return "June";
         when 7  => return "July";
         when 8  => return "August";
         when 9  => return "September";
         when 10 => return "October";
         when 11 => return "November";
         when 12 => return "December";
      end case;
   end Image;
   function Image (Day : Day_Name) return String is
   begin
      case Day is
         when Monday    => return "Monday";
         when Tuesday   => return "Tuesday";
         when Wednesday => return "Wednesday";
         when Thursday  => return "Thursday";
         when Friday    => return "Friday";
         when Saturday  => return "Saturday";
         when Sunday    => return "Sunday";
      end case;
   end Image;
   Today : Time := Clock;
begin
   Put_Line (Image (Today) (1..10));
   Put_Line
   (  Image (Day_Of_Week (Today)) & ", "
   &  Image (Ada.Calendar.Month (Today))
   &  Day_Number'Image (Ada.Calendar.Day (Today)) & ","
   &  Year_Number'Image (Ada.Calendar.Year (Today))
   );
end Date_Format;
```


{{out}}

```txt

2008-10-03
Friday, October 3, 2008

```



## Apex


```java

Datetime dtNow = datetime.now();
String strDt1 = dtNow.format('yyyy-MM-dd');
String strDt2 = dtNow.format('EEEE, MMMM dd, yyyy');
system.debug(strDt1); // "2007-11-10"
system.debug(strDt2); //"Sunday, November 10, 2007"

```


{{out}}

```txt

2016-04-20
Wednesday, April 20, 2016

```



## AppleScript


```applescript
property text item delimiters : {", ", "T"}

set shortdate to text item 1 of (the (current date) as «class isot» as string)

set [w, m, d, y] to the [weekday, month, day, year] of (the current date)
set longdate to {w, [m, space, d], y} as text

shortdate & linefeed & longdate
```


{{out}}

```txt

2018-12-25
Tuesday, December 25, 2018

```



## ALGOL 68

{{works with|ALGOL 68|Standard - using the a68g standard library}}
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}
Note: the '''format''' can be used for both printing ''and'' reading date data.


```algol68
# define the layout of the date/time as provided by the call to local time #
STRUCT ( INT sec, min, hour, mday, mon, year, wday, yday, isdst) tm = (6,5,4,3,2,1,7,~,8);

FORMAT # some useful format declarations #
  ymd repr = $4d,"-"2d,"-"2d$,
  month repr =    $c("January","February","March","April","May","June","July",
                     "August","September","October","November","December")$,
  week day repr = $c("Sunday","Monday","Tuesday","Wednesday",
                     "Thursday","Friday","Saturday")$,
  dmdy repr =     $f(week day repr)", "f(month repr)" "g(-0)", "g(-4)$,

  mon  = $c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")$,
  wday = $c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")$,
  tz   = $c("MSK","MSD")$,
  unix time repr = $f(wday)" "f(mon)z-d," "dd,":"dd,":"dd," "f(tz)" "dddd$;

[]INT now = local time;

printf((ymd repr,       now[year OF tm:mday OF tm], $l$));
printf((dmdy repr,      now[wday OF tm], now[mon OF tm], now[mday OF tm], now[year OF tm], $l$));

printf((unix time repr, now[wday OF tm], now[mon OF tm], now[mday OF tm],
                        now[hour OF tm:sec OF tm], now[isdst OF tm]+1, now[year OF tm], $l$))
```

{{out}}

```txt

2009-04-08
Wednesday, April 8, 2009
Wed Apr  8 18:04:02 MSD 2009

```



## AutoHotkey


```autohotkey
FormatTime, Date1, , yyyy-MM-dd ; "2007-11-10"
FormatTime, Date2, , LongDate   ; "Sunday, November 10, 2007"
MsgBox %Date1% `n %Date2%
```



## AutoIt

This solution uses the locale settings for names of days and months.

```AutoIt

#include <Date.au3>

$iYear = 2007
$iMonth = 11
$iDay = 10

ConsoleWrite(StringFormat('%4d-%02d-%02d', $iYear, $iMonth, $iDay) & @LF)

$iWeekDay = _DateToDayOfWeekISO($iYear, $iMonth, $iDay)
ConsoleWrite(StringFormat('%s, %s %02d, %4d', _GetLongDayLocale($iWeekDay), _GetLongMonthLocale($iMonth), $iDay, $iYear) & @LF)


Func _GetLongDayLocale($_iWeekDay)  ; 1..7 Monday=1
	Local $aDayName[8] = [0, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30]
	Return GetLocaleInfo($aDayName[$_iWeekDay])
EndFunc

Func _GetLongMonthLocale($_iMonth)  ; 1..12 January=1
	Local $aMonthName[13] = [0, 0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F, 0x40, 0x41, 0x42, 0x43]
	Return GetLocaleInfo($aMonthName[$_iMonth])
EndFunc

Func GetLocaleInfo($_LCType)
	Local $ret, $LCID, $sBuffer, $iLen
	$ret = DllCall('kernel32', 'long', 'GetSystemDefaultLCID')
	$LCID = $ret[0]
	$ret = DllCall('kernel32', 'long', 'GetLocaleInfo', 'long', $LCID, 'long', $_LCType, 'str', $sBuffer, 'long', 0)
	$iLen = $ret[0]
	$ret = DllCall('kernel32', 'long', 'GetLocaleInfo', 'long', $LCID, 'long', $_LCType, 'str', $sBuffer, 'long', $iLen)
	Return $ret[3]
EndFunc

```

{{out}}

```txt

2007-11-10
Samstag, November 10, 2007

```

--[[User:BugFix|BugFix]] ([[User talk:BugFix|talk]]) 16:24, 17 November 2013 (UTC)


## AWK

{{works with|Gawk}}

```awk
$ awk 'BEGIN{t=systime();print strftime("%Y-%m-%d",t)"\n"strftime("%A, %B %d, %Y",t)}'
2009-05-15
Friday, May 15, 2009
```



## BaCon


```freebasic
' Date format
n = NOW
PRINT YEAR(n), MONTH(n), DAY(n) FORMAT "%ld-%02ld-%02ld\n"
PRINT WEEKDAY$(n), MONTH$(n), DAY(n), YEAR(n) FORMAT "%s, %s %02ld, %ld\n"
```


{{out}}

```txt
prompt$ ./date-format
2017-02-17
Friday, February 17, 2017
```



## BASIC

{{works with|FreeBASIC}}

```freebasic
#include "vbcompat.bi"

DIM today As Double = Now()

PRINT Format(today, "yyyy-mm-dd")
PRINT Format(today, "dddd, mmmm d, yyyy")
```



## Batch File


```dos

@echo off
setlocal enabledelayedexpansion

:: Define arrays of days/months we'll need
set daynames=Monday Tuesday Wednesday Thursday Friday Saturday Sunday
set monthnames=January February March April May June July August September October November December

:: Separate the output of the 'date /t' command (outputs in the format of "Sun 16/04/2017") into 4 variables
for /f "tokens=1,2,3,4 delims=/ " %%i in ('date /t') do (
  set dayname=%%i
  set day=%%j
  set month=%%k
  set year=%%l
)

:: Crosscheck the first 3 letters of every word in %daynames% to the 3 letter day name found previously
for %%i in (%daynames%) do (
  set tempdayname=%%i
  set comparedayname=!tempdayname:~0,3!
  if "%dayname%"=="!comparedayname!" set fulldayname=%%i
)

:: Variables starting with "0" during the 'set /a' command are treated as octal numbers. To avoid this, if the first character of %month% is "0", it is removed
if "%month:~0,1%"=="0" set monthtemp=%month:~1,1%
set monthcount=0

:: Iterates through %monthnames% and when it reaches the amount of iterations dictated by %month%, sets %monthname% as the current month being iterated through
for %%i in (%monthnames%) do (
  set /a monthcount+=1
  if %monthtemp%==!monthcount! set monthname=%%i
)

echo %year%-%month%-%day%
echo %fulldayname%, %monthname% %day%, %year%
pause>nul

```

{{out}}

```txt

2017-04-16
Sunday,  16, 2017

```



## BBC BASIC


```bbcbasic
      daysow$ = "Sunday    Monday    Tuesday   Wednesday Thursday  Friday    Saturday"
      months$ = "January   February  March     April     May       June      " + \
      \         "July      August    September October   November  December"

      date$ = TIME$
      dayow% = (INSTR(daysow$, MID$(date$,1,3)) + 9) DIV 10
      month% = (INSTR(months$, MID$(date$,8,3)) + 9) DIV 10

      PRINT MID$(date$,12,4) "-" RIGHT$("0"+STR$month%,2) + "-" + MID$(date$,5,2)

      PRINT FNrtrim(MID$(daysow$, dayow%*10-9, 10)) ", " ;
      PRINT FNrtrim(MID$(months$, month%*10-9, 10)) " " ;
      PRINT MID$(date$,5,2) ", " MID$(date$,12,4)
      END

      DEF FNrtrim(A$)
      WHILE RIGHT$(A$) = " " A$ = LEFT$(A$) : ENDWHILE
      = A$
```



## C


```cpp
#include <iostream>
#include <stdio.h>
#include <time.h>
#define MAX_BUF 50

int main(void)
{
  char buf[MAX_BUF];
  time_t seconds = time(NULL);
  struct tm *now = localtime(&seconds);
  const char *months[] = {"January", "February", "March", "April", "May", "June",
                          "July", "August", "September", "October", "November", "December"};

  const char *days[] = {"Sunday", "Monday", "Tuesday", "Wednesday","Thursday","Friday","Saturday"};

  (void) printf("%d-%d-%d\n", now->tm_year + 1900, now->tm_mon + 1, now->tm_mday);
  (void) printf("%s, %s %d, %d\n",days[now->tm_wday], months[now->tm_mon],
               now->tm_mday, now->tm_year + 1900);
  /* using the strftime (the result depends on the locale) */
  (void) strftime(buf, MAX_BUF, "%A, %B %e, %Y", now);
  (void) printf("%s\n", buf);
  return EXIT_SUCCESS;
}
```

{{out}}

```txt

2009-5-13
Wednesday, May 13, 2009
Wednesday, May 13, 2009
```



## C++


```cpp
// Display the current date in the formats of "2007-11-10"
// and "Sunday, November 10, 2007".

#include <vector>
#include <string>
#include <iostream>
#include <ctime>

/** Return the current date in a string, formatted as either ISO-8601
 *  or "Weekday-name, Month-name Day, Year".
 *
 *  The date is initialized when the object is created and will return
 *  the same date for the lifetime of the object.  The date returned
 *  is the date in the local timezone.
 */
class Date
{
    struct tm ltime;

public:
    /// Default constructor.
    Date()
    {
        time_t t = time(0);
        localtime_r(&t, &ltime);
    }

    /** Return the date based on a format string.  The format string is
     *  fed directly into strftime().  See the strftime() documentation
     *  for information on the proper construction of format strings.
     *
     *  @param[in] fmt is a valid strftime() format string.
     *
     *  @return a string containing the formatted date, or a blank string
     *      if the format string was invalid or resulted in a string that
     *      exceeded the internal buffer length.
     */
    std::string getDate(const char* fmt)
    {
        char out[200];
        size_t result = strftime(out, sizeof out, fmt, &ltime);
        return std::string(out, out + result);
    }

    /** Return the date in ISO-8601 date format.
     *
     *  @return a string containing the date in ISO-8601 date format.
     */
    std::string getISODate() {return getDate("%F");}

    /** Return the date formatted as "Weekday-name, Month-name Day, Year".
     *
     *  @return a string containing the date in the specified format.
     */
    std::string getTextDate() {return getDate("%A, %B %d, %Y");}
};

int main()
{
    Date d;
    std::cout << d.getISODate() << std::endl;
    std::cout << d.getTextDate() << std::endl;
    return 0;
}
```

{{out}}

```txt

2009-05-14
Thursday, May 14, 2009

```


## C#

```c#
using System;

namespace RosettaCode.DateFormat
{
    class Program
    {
        static void Main(string[] args)
        {
            DateTime today = DateTime.Now.Date;
            Console.WriteLine(today.ToString("yyyy-MM-dd"));
            Console.WriteLine(today.ToString("dddd, MMMMM d, yyyy"));
        }
    }
}
```



## Clojure


```lisp
(let [now (.getTime (java.util.Calendar/getInstance))
      f1  (java.text.SimpleDateFormat. "yyyy-MM-dd")
      f2  (java.text.SimpleDateFormat. "EEEE, MMMM dd, yyyy")]
  (println (.format f1 now))
  (println (.format f2 now)))
```


{{out}}

```txt
2009-12-06
Sunday, December 06, 2009
```



## COBOL

{{works with|OpenCOBOL}}

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Date-Format.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  Days-Area.
           03  Days-Data.
               05  FILLER PIC X(9) VALUE "Monday".
               05  FILLER PIC X(9) VALUE "Tuesday".
               05  FILLER PIC X(9) VALUE "Wednesday".
               05  FILLER PIC X(9) VALUE "Thursday".
               05  FILLER PIC X(9) VALUE "Friday".
               05  FILLER PIC X(9) VALUE "Saturday".
               05  FILLER PIC X(9) VALUE "Sunday".

           03  Days-Values REDEFINES Days-Data.
               05  Days-Table PIC X(9) OCCURS 7 TIMES.

       01  Months-Area.
           03  Months-Data.
               05  FILLER PIC X(9) VALUE "January".
               05  FILLER PIC X(9) VALUE "February".
               05  FILLER PIC X(9) VALUE "March".
               05  FILLER PIC X(9) VALUE "April".
               05  FILLER PIC X(9) VALUE "May".
               05  FILLER PIC X(9) VALUE "June".
               05  FILLER PIC X(9) VALUE "July".
               05  FILLER PIC X(9) VALUE "August".
               05  FILLER PIC X(9) VALUE "September".
               05  FILLER PIC X(9) VALUE "October".
               05  FILLER PIC X(9) VALUE "November".
               05  FILLER PIC X(9) VALUE "December".

           03  Months-Values REDEFINES Months-Data.
               05  Months-Table PIC X(9) OCCURS 12 TIMES.

       01  Current-Date-Str.
           03  Current-Year     PIC X(4).
           03  Current-Month    PIC X(2).
           03  Current-Day      PIC X(2).

       01  Current-Day-Of-Week  PIC 9.

       PROCEDURE DIVISION.
           MOVE FUNCTION CURRENT-DATE (1:8) TO Current-Date-Str

           DISPLAY Current-Year "-" Current-Month "-" Current-Day

           ACCEPT Current-Day-Of-Week FROM DAY-OF-WEEK
           DISPLAY
               FUNCTION TRIM(
                   Days-Table (FUNCTION NUMVAL(Current-Day-Of-Week)))
               ", "
               FUNCTION TRIM(
                   Months-Table (FUNCTION NUMVAL(Current-Month)))
               " "
               Current-Day
               ", "
               Current-Year
           END-DISPLAY

           GOBACK
           .
```



## CoffeeScript

=== ECMAScript ≥ 5.1 ===
Is supported by at least: Chrome 24, Firefox/Gecko 29, IE 11, Opera 15, and Node.js (at least as of 2015).

```coffeescript

date = new Date

console.log date.toLocaleDateString 'en-GB',
    month:  '2-digit'
    day:    '2-digit'
    year:   'numeric'
.split('/').reverse().join '-'

console.log date.toLocaleDateString 'en-US',
    weekday: 'long'
    month:   'long'
    day:  'numeric'
    year: 'numeric'

```



###  Portable version


```coffeescript

# JS does not have extensive formatting support out of the box.  This code shows
# how you could create a date formatter object.
DateFormatter = ->
  weekdays = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday']
  months = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December']
  pad = (n) ->
    if n < 10
      "0" + n
    else
      n

  brief: (date) ->
    month = 1 + date.getMonth()
    "#{date.getFullYear()}-#{pad month}-#{pad date.getDate()}"

  verbose: (date) ->
    weekday = weekdays[date.getDay()]
    month = months[date.getMonth()]
    day = date.getDate()
    year = date.getFullYear();
    "#{weekday}, #{month} #{day}, #{year}"

formatter = DateFormatter()
date = new Date()
console.log formatter.brief(date)
console.log formatter.verbose(date)

```

{{out}}

```txt

> coffee date_format.coffee
2012-01-14
Saturday, January 14, 2012

```



## ColdFusion


```cfm><cfoutput

    #dateFormat(Now(), "YYYY-MM-DD")#<br />
    #dateFormat(Now(), "DDDD, MMMM DD, YYYY")#
</cfoutput>
```



## Common Lisp


```lisp
(defconstant *day-names*
    #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
(defconstant *month-names*
    #(nil "January" "February" "March" "April" "May" "June" "July"
      "August" "September" "October" "November" "December"))

(multiple-value-bind (sec min hour date month year day daylight-p zone) (get-decoded-time)
    (format t "~4d-~2,'0d-~2,'0d~%" year month date)
    (format t "~a, ~a ~d, ~4d~%"
        (aref *day-names* day) (aref *month-names* month) date year))
```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE DateFormat;
IMPORT StdLog, Dates;

PROCEDURE Do*;
VAR
	d: Dates.Date;
	resp: ARRAY 64 OF CHAR;
BEGIN
	Dates.GetDate(d);
	Dates.DateToString(d,Dates.short,resp);
	StdLog.String(":> " + resp);StdLog.Ln;
	Dates.DateToString(d,Dates.abbreviated,resp);
	StdLog.String(":> " + resp);StdLog.Ln;
	Dates.DateToString(d,Dates.long,resp);
	StdLog.String(":> " + resp);StdLog.Ln;
	Dates.DateToString(d,Dates.plainAbbreviated,resp);
	StdLog.String(":> " + resp);StdLog.Ln;
	Dates.DateToString(d,Dates.plainLong,resp);
	StdLog.String(":> " + resp);StdLog.Ln;
END Do;
END DateFormat.

```

Execute: ^Q DateFormat.Do<br/>
{{out}}
Spanish localization<br/>

```txt

:> 01/09/2013
:> dom, 01 de sep de 2013
:> domingo, 01 de septiembre de 2013
:> 01 de sep de 2013
:> 01 de septiembre de 2013

```



## D

{{works with|D|DMD 1.026}}
{{libheader|Tango}}

```d
module datetimedemo ;

import tango.time.Time ;
import tango.text.locale.Locale ;
import tango.time.chrono.Gregorian ;

import tango.io.Stdout ;

void main() {
    Gregorian g = new Gregorian ;
    Stdout.layout = new Locale; // enable Stdout to handle date/time format
    Time d = g.toTime(2007, 11, 10, 0, 0, 0, 0, g.AD_ERA) ;
    Stdout.format("{:yyy-MM-dd}", d).newline ;
    Stdout.format("{:dddd, MMMM d, yyy}", d).newline ;
    d = g.toTime(2008, 2, 1, 0, 0, 0, 0, g.AD_ERA) ;
    Stdout.format("{:dddd, MMMM d, yyy}", d).newline ;
}
```

{{out}}

```txt
2007-11-10
Saturday, November 10, 2007
Friday, February 1, 2008
```



## Delphi


```Delphi
ShowMessage(FormatDateTime('yyyy-mm-dd', Now) +#13#10+ FormatDateTime('dddd, mmmm dd, yyyy', Now));
```



## Elixir

{{trans|Erlang}}
{{works with|Elixir|1.4}}

```elixir
defmodule Date_format do
  def iso_date, do: Date.utc_today |> Date.to_iso8601

  def iso_date(year, month, day), do: Date.from_erl!({year, month, day}) |> Date.to_iso8601

  def long_date, do: Date.utc_today |> long_date

  def long_date(year, month, day), do: Date.from_erl!({year, month, day}) |> long_date

  @months  Enum.zip(1..12, ~w[January February March April May June July August September October November December])
           |> Map.new
  @weekdays  Enum.zip(1..7, ~w[Monday Tuesday Wednesday Thursday Friday Saturday Sunday])
             |> Map.new
  def long_date(date) do
    weekday = Date.day_of_week(date)
    "#{@weekdays[weekday]}, #{@months[date.month]} #{date.day}, #{date.year}"
  end
end

IO.puts Date_format.iso_date
IO.puts Date_format.long_date
IO.puts Date_format.iso_date(2007,11,10)
IO.puts Date_format.long_date(2007,11,10)
```


{{out}}

```txt

2017-02-26
Sunday, February 26, 2017
2007-11-10
Saturday, November 10, 2007

```



## Emacs Lisp


```Lisp
(format-time-string "%Y-%m-%d")
(format-time-string "%F")  ;; new in Emacs 24
=> "2015-11-08"

(format-time-string "%A, %B %e, %Y")
=> "Sunday, November  8, 2015"

```


<code>%e</code> is blank-padded day number, or <code>%d</code> for zero-padded.  Month and weekday names follow the current locale.  On a POSIX style system this is the usual <code>LC_TIME</code> or <code>LC_ALL</code> environment variables.  GNU Emacs variable <code>system-time-locale</code> can override this if desired.


## Erlang



```Erlang
-module(format_date).
-export([iso_date/0, iso_date/1, iso_date/3, long_date/0, long_date/1, long_date/3]).
-import(calendar,[day_of_the_week/1]).
-import(io,[format/2]).
-import(lists,[append/1]).

iso_date() -> iso_date(date()).
iso_date(Year, Month, Day) -> iso_date({Year, Month, Day}).
iso_date(Date) ->
  format("~4B-~2..0B-~2..0B~n", tuple_to_list(Date)).

long_date() -> long_date(date()).
long_date(Year, Month, Day) -> long_date({Year, Month, Day}).
long_date(Date = {Year, Month, Day}) ->
  Months =   { "January",   "February", "March",    "April",
               "May",       "June",     "July",     "August",
               "September", "October",  "November", "December" },
  Weekdays = { "Monday", "Tuesday",  "Wednesday", "Thursday",
               "Friday", "Saturday", "Sunday" },
  Weekday     = day_of_the_week(Date),
  WeekdayName = element(Weekday, Weekdays),
  MonthName   = element(Month, Months),
  append([WeekdayName, ", ", MonthName, " ", integer_to_list(Day), ", ",
          integer_to_list(Year)]).
```



## Euphoria


```Euphoria
constant days = {"Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"}
constant months = {"January","February","March","April","May","June",
    "July","August","September","October","November","December"}

sequence now

now = date()
now[1] += 1900

printf(1,"%d-%02d-%02d\n",now[1..3])
printf(1,"%s, %s %d, %d\n",{days[now[7]],months[now[2]],now[3],now[1]})
```



## EGL


```EGL

// 2012-09-26
SysLib.writeStdout(StrLib.formatDate(DateTimeLib.currentDate(), "yyyy-MM-dd"));
// Wednesday, September 26, 2012
SysLib.setLocale("en", "US");
SysLib.writeStdout(StrLib.formatDate(DateTimeLib.currentDate(), "EEEE, MMMM dd, yyyy"));

```

{{out}}

```txt

2012-09-26
Wednesday, September 26, 2012

```


=={{header|F_Sharp|F#}}==
"F# Interactive" session:

```fsharp>
 open System;;
> Console.WriteLine( DateTime.Now.ToString("yyyy-MM-dd") );;
2010-08-13
> Console.WriteLine( "{0:D}", DateTime.Now );;
Friday, August 13, 2010
```



## Factor


```factor
USING: formatting calendar io ;

now "%Y-%m-%d" strftime print
now "%A, %B %d, %Y" strftime print
```



## Fantom


Today's date can be retrieved using 'Date.today'.  The 'toLocale' method can be passed formatting information to determine how the date should be represented as a string.  The rules are described at http://fantom.org/doc/sys/Date.html#toLocale


```fantom

fansh> Date.today.toLocale("YYYY-MM-DD")
2011-02-24
fansh> Date.today.toLocale("WWWW, MMMM DD, YYYY")
Thursday, February 24, 2011

```



## Forth


```forth
: .-0 ( n -- n )
  [char] - emit
  dup 10 < if [char] 0 emit then ;

: .short-date
  time&date ( s m h D M Y )
  1 u.r .-0 1 u.r .-0 1 u.r
  drop drop drop ;

: str-table
  create ( n -- ) 0 do , loop
  does>  ( n -- str len ) swap cells + @ count ;

 here ," December"
 here ," November"
 here ," October"
 here ," September"
 here ," August"
 here ," July"
 here ," June"
 here ," May"
 here ," April"
 here ," March"
 here ," February"
 here ," January"
12 str-table months

 here ," Sunday"
 here ," Saturday"
 here ," Friday"
 here ," Thursday"
 here ," Wednesday"
 here ," Tuesday"
 here ," Monday"
7 str-table weekdays

\ Zeller's Congruence
: zeller ( m -- days since March 1 )
  9 + 12 mod 1-   26 10 */ 3 + ;

: weekday ( d m y -- 0..6 )   \ Monday..Sunday
  over 3 < if 1- then
  dup    4 /
  over 100 / -
  over 400 / +  +
  swap zeller + +
  1+ 7 mod ;

: 3dup   dup 2over rot ;

: .long-date
  time&date ( s m h D M Y )
  3dup weekday weekdays type ." , "
  >R 1- months type space 1 u.r ." , " R> .
  drop drop drop ;
```



###  Version 2: Meta Language

Forth is less of a language and more of an extensible toolkit of simple routines. This version attempts to demonstrate using the simple routines to extend Forth. Then using the language extensions and the power of concatenative language to solve the problem. This solution could create numerous date formats as one line definitions now that we have our "date" words defined. Typically these extensions would be saved as a library file.

<LANG FORTH>\ Build up a "language" for date formatting

\ utility words
: UNDER+  ( a b c -- a+c b )  ROT + SWAP ;
: 3DUP    ( a b c -- a b c a b c ) 2 PICK 2 PICK 2 PICK ;
: WRITE$  ( caddr -- ) COUNT TYPE ;  \ print a counted string
: ','     ( -- )  ." , "  ;
: '-'     ( -- )  ." -"   ;

\ day of week calculation
\ "This is an algorithm I've carried with me for 35 years,
\  originally in Assembler and Fortran II."
\  It counts the number of days from March 1, 1900."
\                                    Wil Baden R.I.P.
\ *****************************************************
\ **WARNING** only good until 2078 on 16 bit machine **
\ *****************************************************
DECIMAL
: CDAY    ( dd mm yyyy -- century_day )
      -3 UNDER+  OVER  0<
      IF   12 UNDER+  1-   THEN
      1900 -  1461 4 */   SWAP 306 *  5 +  10 /  + +  ;

: DOW   ( cday -- day_of_week ) 2 + 7 MOD 1+ ; ( 7 is Sunday)

\ formatted number printers
: ##.   ( n -- )  0 <#  # #      #> TYPE ;
: ####. ( n -- )  0 <#  # # # #  #> TYPE ;

\ make some string list words
: LIST[   ( -- ) !CSP  0 C,  ;         \ list starts with 0 bytes, record stack pos.
: ]LIST   ( -- ) 0 C, ALIGN ?CSP ;     \ '0' ends list, check stack

: NEXT$   ( $[1] -- $[2] )    COUNT + ;               \ get next string
: NTH$    ( n list -- $addr ) SWAP 0 DO NEXT$ LOOP ;  \ get nth string

: "       ( -- ) [CHAR] " WORD  C@ CHAR+ ALLOT ;      \ compile text upto "

\ make the lists
CREATE MONTHS
  LIST[ " January" " February"  " March"   " April" " May" " June" " July"
        " August"  " September" " October" " November" " December" ]LIST

CREATE DAYS
  LIST[ " Monday" " Tuesday" " Wednesday"  " Thursday"
        " Friday" " Saturday" " Sunday" ]LIST

\ expose lists as indexable arrays that print the string
: ]MONTH$.  ( n -- )  MONTHS NTH$ WRITE$ ;
: ]DAY$.    ( n -- )  DAYS NTH$ WRITE$ ;


\
### =====================================

\ Rosetta Task Code Begins

\ Rosetta Date Format 1
: Y-M-D.     ( d m y -- )  ####. '-' ##. '-' ##. ;

\ Rosetta Date Format 2
: LONG.DATE ( d m y -- )
    3DUP CDAY DOW ]DAY$. ',' -ROT ]MONTH$. SPACE ##.  ','  ####.  ;</LANG>


Test at the Forth Console
<LANG FORTH> 5 7 2018 Y-M-D. 2018-07-05 ok
  ok
5 7 2018 LONG.DATE Thursday, July 05, 2018 ok
</LANG>


## Fortran

{{works with|Fortran|95 and later}}
The subroutine DATE_AND_TIME does not return day of week information so we have to write our own function for that

```fortran
PROGRAM DATE

  IMPLICIT NONE

  INTEGER :: dateinfo(8), day
  CHARACTER(9) :: month, dayname

  CALL DATE_AND_TIME(VALUES=dateinfo)
  SELECT CASE(dateinfo(2))
    CASE(1)
      month = "January"
    CASE(2)
      month = "February"
    CASE(3)
      month = "March"
    CASE(4)
      month = "April"
    CASE(5)
      month = "May"
    CASE(6)
      month = "June"
    CASE(7)
      month = "July"
    CASE(8)
      month = "August"
    CASE(9)
      month = "September"
    CASE(10)
      month = "October"
    CASE(11)
      month = "November"
    CASE(12)
     month = "December"
  END SELECT

  day = Day_of_week(dateinfo(3), dateinfo(2), dateinfo(1))

  SELECT CASE(day)
    CASE(0)
      dayname = "Saturday"
    CASE(1)
      dayname = "Sunday"
    CASE(2)
      dayname = "Monday"
    CASE(3)
      dayname = "Tuesday"
    CASE(4)
      dayname = "Wednesday"
    CASE(5)
      dayname = "Thursday"
    CASE(6)
      dayname = "Friday"
  END SELECT

  WRITE(*,"(I0,A,I0,A,I0)") dateinfo(1),"-", dateinfo(2),"-", dateinfo(3)
  WRITE(*,"(4(A),I0,A,I0)") trim(dayname), ", ", trim(month), " ", dateinfo(3), ", ", dateinfo(1)

CONTAINS

  FUNCTION Day_of_week(d, m, y)
    INTEGER :: Day_of_week, j, k
    INTEGER, INTENT(IN) :: d, m, y

    j = y / 100
    k = MOD(y, 100)
    Day_of_week = MOD(d + (m+1)*26/10 + k + k/4 + j/4 + 5*j, 7)
  END FUNCTION Day_of_week

END PROGRAM DATE
```

{{out}}
 2008-12-14
 Sunday, December 14, 2008


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

#Include "vbcompat.bi"

Dim d As Long = Now
Print "This example was created on : "; Format(d, "yyyy-mm-dd")
Print "In other words on : "; Format(d, "dddd, mmmm d, yyyy")
Print
Print "Press any key to quit the program"
Sleep
```


{{out}}

```txt

This example was created on : 2016-10-02
In other words on : Sunday, October 2, 2016

```



## Frink


```frink

println[now[] -> ### yyyy-MM-dd ###]
println[now[] -> ### EEEE, MMMM d, yyyy ###]

```



## FunL


```funl
println( format('%tF', $date) )
println( format('%1$tA, %1$tB %1$td, %1$tY', $date) )
```



## Gambas

'''[https://gambas-playground.proko.eu/ You can run this code. Copy the code, click this link, paste it in and press 'Run !']'''

```gambas
Public Sub Main()

Print Format(Now, "yyyy - mm - dd")
Print Format(Now, "dddd, mmmm dd, yyyy")

End
```

Output:

```txt

2017 - 05 - 27
Saturday, May 27, 2017

```



## Go

In an interesting design, you specify your format by providing the format for the date and time 01/02 03:04:05PM '06 -0700

```go
package main

import "time"
import "fmt"

func main() {
    fmt.Println(time.Now().Format("2006-01-02"))
    fmt.Println(time.Now().Format("Monday, January 2, 2006"))
}
```

{{out}}

```txt

2011-12-02
Friday, December 2, 2011

```



## Groovy

Solution:

```groovy
def isoFormat = { date -> date.format("yyyy-MM-dd") }
def longFormat = { date -> date.format("EEEE, MMMM dd, yyyy") }
```


Test Program:

```groovy
def now = new Date()
println isoFormat(now)
println longFormat(now)
```



## Haskell


```haskell
import Data.Time
       (FormatTime, formatTime, defaultTimeLocale, utcToLocalTime,
        getCurrentTimeZone, getCurrentTime)

formats :: FormatTime t => [t -> String]
formats = (formatTime defaultTimeLocale) <$>  ["%F", "%A, %B %d, %Y"]

main :: IO ()
main = do
  t <- pure utcToLocalTime <*> getCurrentTimeZone <*> getCurrentTime
  putStrLn $ unlines (formats <*> pure t)
```

'''Sample output:'''

```txt
2017-06-05
Monday, June 05, 2017
```



## HicEst


```hicest
   CHARACTER string*40

   WRITE(Text=string, Format='UCCYY-MM-DD') 0 ! string: 2010-03-13

   ! the U-format to write date and time uses ',' to separate additional output formats
   ! we therefore use ';' in this example and change it to ',' below:
   WRITE(Text=string,Format='UWWWWWWWWW; MM DD; CCYY') 0 ! string = "Saturday ; 03 13; 2010"
   READ(Text=string) month ! first numeric value = 3 (no literal month name available)
   EDIT(Text='January,February,March,April,May,June,July,August,September,October,November,December', ITeM=month, Parse=cMonth) ! cMonth = "March"
   ! change now string = "Saturday ; 03 13; 2010" to "Saturday, March 13, 2010":
   EDIT(Text=string, Right=' ', Mark1, Right=';', Right=3, Mark2, Delete, Insert=', '//cMonth, Right=';', RePLaceby=',')

 END
```


== {{header|Icon}} and {{header|Unicon}} ==

```Icon
procedure main()
write(map(&date,"/","-"))
write(&dateline ? tab(find(&date[1:5])+4))
end
```


{{out}}

```txt
2011-02-01
Tuesday, February 1, 2011
```



## J

Short format using built in formatting:

```j
   6!:0 'YYYY-MM-DD'
2010-08-19
```


Verb to show custom format:

```j
require 'dates system/packages/misc/datefmt.ijs'
days=:;:'Sunday Monday Tuesday Wednesday Thursday Friday Saturday'
fmtDate=: [:((days ;@{~ weekday),', ',ms0) 3 {.]

   fmtDate 6!:0 ''
Thursday, August 19, 2010
```



## Java


```java
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.text.DateFormatSymbols;
import java.util.Date;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
public class Dates
{
 public static void main(final String[] args)
 {
  Calendar now = new GregorianCalendar(); //months are 0 indexed, dates are 1 indexed
  DateFormatSymbols symbols = new DateFormatSymbols(); //names for our months and weekdays

  //plain numbers way
  System.out.println(now.get(Calendar.YEAR)  + "-" + (now.get(Calendar.MONTH) + 1) + "-" + now.get(Calendar.DATE));

  //words way
  System.out.print(symbols.getWeekdays()[now.get(Calendar.DAY_OF_WEEK)] + ", ");
  System.out.print(symbols.getMonths()[now.get(Calendar.MONTH)] + " ");
  System.out.println(now.get(Calendar.DATE) + ", " + now.get(Calendar.YEAR));

  //using DateFormat
  Date date = new Date();
  DateFormat format1 = new SimpleDateFormat("yyyy-MM-dd");
  System.out.println(format1.format(date));
  DateFormat format2 = new SimpleDateFormat("EEEE, MMMM dd, yyyy");
  System.out.println(format2.format(date));
 }
}
```

Better use a library, see http://sourceforge.net/apps/mediawiki/threeten/index.php?title=ThreeTen


### Java 8 Date Time API


```java

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
public class Dates
{
 public static void main(final String[] args)
 {
  //using DateTimeFormatter
  LocalDate date = LocalDate.now();
  DateTimeFormatter dtFormatter = DateTimeFormatter.ofPattern("yyyy MM dd");

  System.out.println(dtFormatter.format(date));
 }
}

```



## JavaScript

JavaScript does not have any built-in <code>strftime</code>-type functionality.

```javascript
var now = new Date(),
    weekdays = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'],
    months   = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'],
    fmt1 = now.getFullYear() + '-' + (1 + now.getMonth()) + '-' + now.getDate(),
    fmt2 = weekdays[now.getDay()] + ', ' + months[now.getMonth()] + ' ' + now.getDate() + ', ' + now.getFullYear();
console.log(fmt1);
console.log(fmt2);
```


```txt
2010-1-12
Tuesday, January 12, 2010
```



## Joy


```Joy

DEFINE weekdays == [ "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday" ];
       months   == [ "January" "February" "March" "April" "May" "June" "July" "August"
                     "September" "October" "November" "December" ].

time localtime [ [0 at 'd 4 4 format] ["-"] [1 at 'd 2 2 format] ["-"] [2 at 'd 2 2 format] ]
[i] map [putchars] step '\n putch pop.

time localtime [ [8 at pred weekdays of] [", "] [1 at pred months of] [" "] [2 at 'd 1 1 format]
[", "] [0 at 'd 4 4 format] ] [i] map [putchars] step '\n putch pop.

```



## jq

{{works with|jq|with strftime}}

```sh
$ jq -n 'now | (strftime("%Y-%m-%d"), strftime("%A, %B %d, %Y"))'
"2015-07-02"
"Thursday, July 02, 2015"
```


WARNING: prior to July 2, 2015, there was a bug in jq affecting the display of the "day of week" (wday) and the "day of the year" (yday).

## Julia

{{works with|Julia|0.6}}


```julia
ts = Dates.today()

println("Today's date is:")
println("\t$ts")
println("\t", Dates.format(ts, "E, U dd, yyyy"))
```


{{out}}

```txt
Today's date is:
	2018-01-05
	Friday, January 05, 2018
```



## Kotlin


```scala
// version 1.0.6

import java.util.GregorianCalendar

fun main(args: Array<String>) {
    val now = GregorianCalendar()
    println("%tF".format(now))
    println("%tA, %1\$tB %1\$te, %1\$tY".format(now))
}
```


{{out}}

```txt

2017-01-16
Monday, January 16, 2017

```



## Lasso


```Lasso

date('11/10/2007')->format('%Q') // 2007-11-10
date('11/10/2007')->format('EEEE, MMMM d, YYYY') //Saturday, November 10, 2007


```



## Liberty BASIC


```lb
'Display the current date in the formats of "2007-11-10"
d$=date$("yyyy/mm/dd")
print word$(d$,1,"/")+"-"+word$(d$,2,"/")+"-"+word$(d$,3,"/")

'and "Sunday, November 10, 2007".
day$(0)="Tuesday"
day$(1)="Wednesday"
day$(2)="Thursday"
day$(3)="Friday"
day$(4)="Saturday"
day$(5)="Sunday"
day$(6)="Monday"
theDay = date$("days") mod 7
print day$(theDay);", ";date$()

' month in full
year=val(word$(d$,1,"/"))
month=val(word$(d$,2,"/"))
day=val(word$(d$,3,"/"))
weekDay$="Tuesday Wednesday Thursday Friday Saturday Sunday Monday"
monthLong$="January February March April May June July August September October November December"

print word$(weekDay$,theDay+1);", ";word$(monthLong$,month);" ";day;", ";year
```


## Lua


```lua
print( os.date( "%Y-%m-%d" ) )
print( os.date( "%A, %B %d, %Y" ) )
```



## M2000 Interpreter


```M2000 Interpreter

Print str$(today, "yyyy-mm-dd")
Print str$(today, "dddd, mmm, dd, yyyy")

```



## Maple



```Maple

with(StringTools);
FormatTime("%Y-%m-%d")
FormatTime("%A,%B %d, %y")

```




## Mathematica


```Mathematica
DateString[{"Year", "-", "Month", "-", "Day"}]
DateString[{"DayName", ", ", "MonthName", " ", "Day", ", ", "Year"}]
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB>>
 datestr(now,'yyyy-mm-dd')

ans =

2010-06-18

>> datestr(now,'dddd, mmmm dd, yyyy')

ans =

Friday, June 18, 2010
```



## min

{{works with|min|0.19.3}}

```min
("YYYY-MM-dd" "dddd, MMMM dd, YYYY") ('timestamp dip tformat puts!) foreach
```

{{out}}

```txt

2019-04-02
Tuesday, April 02, 2019

```



## mIRC Scripting Language


```mirc
echo -ag $time(yyyy-mm-dd)
echo -ag $time(dddd $+ $chr(44) mmmm dd $+ $chr(44) yyyy)
```



## MUMPS

{{works with|MUMPS|Intersystems' Caché|(all versions)}}
Functions starting with 'Z' or '$Z' are implementation specific.

```MUMPS
DTZ
 WRITE !,"Date format 3: ",$ZDATE($H,3)
 WRITE !,"Or ",$ZDATE($H,12),", ",$ZDATE($H,9)
 QUIT
```

<p>MUMPS contains the integer number of days since December 31, 1840 in the first part of the system variable $HOROLOG.</p>
{{works with|MUMPS|all}}

```MUMPS
DTM(H)
 ;You can pass an integer, but the default is to use today's value
 SET:$DATA(H)=0 H=$HOROLOG
 NEW Y,YR,RD,MC,MO,DA,MN,DN,DOW
 SET MN="January,February,March,April,May,June,July,August,September,October,November,December"
 SET DN="Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday"
 SET MC="31,28,31,30,31,30,31,31,30,31,30,31"
 SET Y=+H\365.25 ;This shouldn't be an approximation in production code
 SET YR=Y+1841 ;Y is the offset from the epoch in years
 SET RD=((+H-(Y*365.25))+1)\1 ;How far are we into the year?
 SET $P(MC,",",2)=$S(((YR#4=0)&(YR#100'=0))!((YR#100=0)&(YR#400=0))=0:28,1:29) ;leap year correction
 SET MO=1,RE=RD FOR  QUIT:RE<=$P(MC,",",MO)  SET RE=RE-$P(MC,",",MO),MO=MO+1
 SET DA=RE+1
 SET DOW=(H#7)+5 ;Fencepost issue - the first piece is 1
 ;add padding as needed
 SET:$L(MO)<2 MO="0"_MO
 SET:$L(DA)<2 DA="0"_DA
 WRITE !,YR,"-",MO,"-",DA
 WRITE !,$P(DN,",",DOW),", ",$P(MN,",",MO)," ",DA,", ",YR
 KILL Y,YR,RD,MC,MO,DA,MN,DN,DOW
 QUIT
```

Demos:
```txt

USER>D DTM^ROSETTA

2010-06-24
Thursday, June 24, 2010
USER>D DTM^ROSETTA(1234)

1844-05-19
Saturday, May 19, 1844
USER>D DTZ^ROSETTA

Date format 3: 2010-06-24
Or Thursday, June 24, 2010

```



## Neko


```ActionScript
/**
 <doc>
 <h2>Date format</h2>
 <p>Neko uses Int32 to store system date/time values.
 And lib C strftime style formatting for converting to string form</p>
 </doc>
*/

var date_now = $loader.loadprim("std@date_now", 0)
var date_format = $loader.loadprim("std@date_format", 2)

var now = date_now()
$print(date_format(now, "%F"), "\n")
$print(date_format(now, "%A, %B %d, %Y"), "\n")
```

{{out}}

```txt
prompt$ nekoc date-format.neko
prompt$ neko date-format.n
2018-11-13
Tuesday, November 13, 2018
```



## NetRexx


```NetRexx

import java.text.SimpleDateFormat
say SimpleDateFormat("yyyy-MM-dd").format(Date())
say SimpleDateFormat("EEEE, MMMM dd, yyyy").format(Date())

```

{{out}}

```txt
2019-02-10
Sonntag, Februar 10, 2019
```



## NewLISP


```NewLISP
; file:   date-format.lsp
; url:    http://rosettacode.org/wiki/Date_format
; author: oofoe 2012-02-01

; The "now" function returns the current time as a list. A time zone
; offset in minutes can be supplied. The example below is for Eastern
; Standard Time. NewLISP's implicit list indexing is used to extract
; the first three elements of the returned list (year, month and day).

(setq n (now (* -5 60)))
(println "short: " (format "%04d-%02d-%02d" (n 0) (n 1) (n 2)))

; The "date-value" function returns the time in seconds from the epoch
; when used without arguments. The "date" function converts the
; seconds into a time representation specified by the format string at
; the end. The offset argument ("0" in this example) specifies a
; time-zone offset in minutes.

(println "short: " (date (date-value) 0 "%Y-%m-%d"))

; The time formatting is supplied by the underlying C library, so
; there may be differences on certain platforms. Particularly, leading
; zeroes in day numbers can be suppressed with "%-d" on Linux and
; FreeBSD, "%e" on OpenBSD, SunOS/Solaris and Mac OS X. Use "%#d" for
; Windows.

(println "long:  " (date (date-value) 0 "%A, %B %#d, %Y"))

; By setting the locale, the date will be displayed appropriately:

(set-locale "japanese")
(println "long:  " (date (date-value) 0 "%A, %B %#d, %Y"))

(exit)
```


{{out}}

```txt
short: 2012-02-01
short: 2012-02-01
long:  Wednesday, February 1, 2012
long:  水曜日, 2月 1, 2012

```



## Nim


```Nim
import times

var t = getTime().getLocalTime()
echo(t.format("yyyy-MM-dd"))
echo(t.format("dddd',' MMMM d',' yyyy"))
```



## Objeck


```objeck

use IO;
use Time;

bundle Default {
  class CurrentDate {
    function : Main(args : String[]) ~ Nil {
      t := Date->New();
      Console->Print(t->GetYear())->Print("-")->Print(t->GetMonth())->Print("-")
        ->PrintLine(t->GetDay());
      Console->Print(t->GetDayName())->Print(", ")->Print(t->GetMonthName())
        ->Print(" ")->Print(t->GetDay())->Print(", ")
        ->PrintLine(t->GetYear());
    }
  }
}

```


{{out}}

```txt

2012-5-22
Sunday, May 22, 2012

```


=={{header|Objective-C}}==

```objc
NSLog(@"%@", [NSDate date]);
NSLog(@"%@", [[NSDate date] descriptionWithCalendarFormat:@"%Y-%m-%d" timeZone:nil locale:nil]);
NSLog(@"%@", [[NSDate date] descriptionWithCalendarFormat:@"%A, %B %d, %Y" timeZone:nil locale:nil]);
```


{{works with|Mac OS X|10.4+}}
{{works with|iOS}}

```objc
NSLog(@"%@", [NSDate date]);
NSDateFormatter *dateFormatter = [[NSDateFormat alloc] init];
[dateFormatter setDateFormat:@"yyyy-MM-dd"];
NSLog(@"%@", [dateFormatter stringFromDate:[NSDate date]]);
[dateFormatter setDateFormat:@"EEEE, MMMM d, yyyy"];
NSLog(@"%@", [dateFormatter stringFromDate:[NSDate date]]);
```



## OCaml



```ocaml
# #load "unix.cma";;
# open Unix;;

# let t = time() ;;
val t : float = 1219997516.

# let gmt = gmtime t ;;
val gmt : Unix.tm =
  {tm_sec = 56; tm_min = 11; tm_hour = 8; tm_mday = 29; tm_mon = 7;
   tm_year = 108; tm_wday = 5; tm_yday = 241; tm_isdst = false}

# Printf.sprintf "%d-%02d-%02d" (1900 + gmt.tm_year) (1 + gmt.tm_mon) gmt.tm_mday ;;
- : string = "2008-08-29"
```



```ocaml
let months = [| "January"; "February"; "March"; "April"; "May"; "June";
      "July"; "August"; "September"; "October"; "November"; "December" |]

let days = [| "Sunday"; "Monday"; "Tuesday";  (* Sunday is 0 *)
      "Wednesday"; "Thursday"; "Friday"; "Saturday" |]

# Printf.sprintf "%s, %s %d, %d"
      days.(gmt.tm_wday)
      months.(gmt.tm_mon)
      gmt.tm_mday
      (1900 + gmt.tm_year) ;;
- : string = "Friday, August 29, 2008"
```



## OxygenBasic


```oxygenbasic

extern lib "kernel32.dll"

'http://msdn.microsoft.com/en-us/library/windows/desktop/ms724950(v=vs.85).aspx

typedef struct _SYSTEMTIME {
  WORD wYear;
  WORD wMonth;
  WORD wDayOfWeek;
  WORD wDay;
  WORD wHour;
  WORD wMinute;
  WORD wSecond;
  WORD wMilliseconds;
} SYSTEMTIME, *PSYSTEMTIME;

void GetSystemTime(SYSTEMTIME*t);
void GetLocalTime(SYSTEMTIME*t);

end extern

SYSTEMTIME t
'GetSystemTime t 'GMT (Greenwich Mean Time)
GetLocalTime t

String WeekDay[7]={"Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"}
String MonthName[12]={"January","February","March","April","May","June","July","August","September","October","November","December"}
String month=str t.wMonth : if len(month)<2 then month="0"+month
String day=str t.wDay : if len(day)<2 then day="0"+day
'
print "" t.wYear "-" month "-" day
print WeekDay[t.wDayOfWeek+1 and 7 ] " " MonthName[t.wMonth and 31] " " t.wDay " " t.wYear

```



## Oz

Getting the current local date is easy, but we have to do the formatting manually.

```oz
declare
  WeekDays = unit(0:"Sunday" "Monday" "Tuesday" "Wednesday"
                  "Thursday" "Friday" "Saturday")
  Months = unit(0:"January" "February" "March" "April"
                "May" "June" "July" "August" "September"
                "October" "November" "December")

  fun {DateISO Time}
     Year = 1900 + Time.year
     Month = Time.mon + 1
  in
     Year#"-"#{Align Month}#"-"#{Align Time.mDay}
  end

  fun {DateLong Time}
     Year = 1900 + Time.year
  in
     WeekDays.(Time.wDay)#", "#Months.(Time.mon)#" "#Time.mDay#", "#Year
  end

  fun {Align Num}
     if Num < 10 then "0"#Num else Num end
  end
in
  {System.showInfo {DateISO {OS.localTime}}}
  {System.showInfo {DateLong {OS.localTime}}}
```



## Pascal

{{works with|Turbo Pascal|5.5}}

```Pascal
program dateform;
uses DOS;

{ Format digit with leading zero }
function lz(w: word): string;
var
  s: string;
begin
  str(w,s);
  if length(s) = 1 then
    s := '0' + s;
  lz := s
end;

function m2s(mon: integer): string;
begin
  case mon of
     1: m2s := 'January';
     2: m2s := 'February';
     3: m2s := 'March';
     4: m2s := 'April';
     5: m2s := 'May';
     6: m2s := 'June';
     7: m2s := 'July';
     8: m2s := 'August';
     9: m2s := 'September';
    10: m2s := 'October';
    11: m2s := 'November';
    12: m2s := 'December'
  end
end;

function d2s(dow: integer): string;
begin
  case dow of
    0: d2s := 'Sunday';
    1: d2s := 'Monday';
    2: d2s := 'Tueday';
    3: d2s := 'Wednesday';
    4: d2s := 'Thursday';
    5: d2s := 'Friday';
    6: d2s := 'Saturday'
  end
end;

var
  yr,mo,dy,dow: word;
  mname,dname: string;

begin
  GetDate(yr,mo,dy,dow);
  writeln(yr,'-',lz(mo),'-',lz(dy));
  mname := m2s(mo); dname := d2s(dow);
  writeln(dname,', ',mname,' ',dy,', ',yr)
end.
```


{{out}}

```txt
2010-07-30
Friday, July 30, 2010
```



## Perl

{{libheader|POSIX}}

```perl
use POSIX;

print strftime('%Y-%m-%d', 0, 0, 0, 10, 10, 107), "\n";
print strftime('%A, %B %d, %Y', 0, 0, 0, 10, 10, 107), "\n";
```


{{out}} with locales '''C''':
 2007-11-10
 Saturday, November 10, 2007

{{out}} with locales '''cs_CZ.UTF-8''':
 2007-11-10
 Sobota, listopad 10, 2007

Actual date:

```perl
use POSIX;

print strftime('%Y-%m-%d', localtime), "\n";
print strftime('%A, %B %d, %Y', localtime), "\n";
```


{{out}} with locales '''C''':
 2008-02-13
 Wednesday, February 13, 2008



## Perl 6

{{libheader|DateTime::Format}}

```perl6
use DateTime::Format;

my $dt = DateTime.now;

say strftime('%Y-%m-%d', $dt);
say strftime('%A, %B %d, %Y', $dt);
```


Rudimentary DateTime operations are built-in,
as the DateTime class itself is built into a Perl 6 compiler:

```perl6
use DateTime::Format;

my $dt = DateTime.now;

say $dt.yyyy-mm-dd;
say strftime('%A, %B %d, %Y', $dt);
```



## Phix


```Phix
include builtins\timedate.e
?format_timedate(date(),"YYYY-MM-DD")
?format_timedate(date(),"Dddd, Mmmm d, YYYY")
```

{{out}}

```txt

"2015-09-20"
"Sunday, September 20, 2015"

```



## PHP

Formatting rules: http://www.php.net/date

```php
<?php
echo date('Y-m-d', time())."\n";
echo date('l, F j, Y', time())."\n";
?>
```



## PicoLisp


```PicoLisp
(let (Date (date)  Lst (date Date))
   (prinl (dat$ Date "-"))             # 2010-02-19
   (prinl                              # Friday, February 19, 2010
      (day Date)
      ", "
      (get *MonFmt (cadr Lst))
      " "
      (caddr Lst)
      ", "
      (car Lst) ) )
```



## Pike


```Pike>
 write("%d-%02d-%02d\n", day->year_no(), day->month_no(), day->month_day());
2011-11-05

> write("%s, %s %s, %s\n", day->week_day_name(), day->month_name(), day->month_day_name(), day->year_name());
Saturday, November 5, 2011

> write(day->format_ymd()+"\n");
2011-11-05

> write(day->format_ext_ymd()+"\n");
Saturday, 5 November 2011
```



## PL/I


### Version 1


```pli
df: proc Options(main);
 declare day_of_week(7) character (9) varying initial(
    'Sunday','Monday','Tuesday','Wednesday',
    'Thursday','Friday','Saturday');
 declare today character (9);

 today = datetime('YYYYMMDD');
 put edit(substr(today,1,4),'-',substr(today,5,2),'-',substr(today,7))
  (A);

 today = datetime('MmmDDYYYY');
 put skip edit(day_of_week(weekday(days())),', ') (A);
 put edit(substr(today,1,3),' ',substr(today,4,2),', ',
          substr(today,6,4))(A);
 end;
```

{{out}}

```txt
2013-11-02
Saturday, Nov 02, 2013
```



### Version 2


```PL/I
 df: proc Options(Main);
 declare day_of_week(7) character(9) varying initial(
    'Sunday','Monday','Tuesday','Wednesday',
    'Thursday','Friday','Saturday');
 declare today character(8);
 declare month(12) character(10) varying initial(
    'January','February','March','April','May','June','July',
    'August','September','October','November','December');

 Put edit(translate('abcd-ef-gh',datetime('YYYYMMDD'),'abcdefgh'))(a);
 today = datetime('MMDDYYYY');

 put skip edit(day_of_week(weekday()),', ') (A);
 put edit(month(substr(today,1,2)),' ',substr(today,3,2),', ',
          substr(today,5,4))(A);
 End;
```

{{out}}

```txt
2013-11-02
Saturday, November 02, 2013
```



## PowerShell


```powershell
"{0:yyyy-MM-dd}" -f (Get-Date)
"{0:dddd, MMMM d, yyyy}" -f (Get-Date)
# or
(Get-Date).ToString("yyyy-MM-dd")
(Get-Date).ToString("dddd, MMMM d, yyyy")
```

''Note:'' The names of months and days follow the currently set locale but otherwise the format is unchanged.


## PureBasic

{{works with|PureBasic|4.41}}

```PureBasic
;Declare Procedures
Declare.s MonthInText()
Declare.s DayInText()

;Output the requested strings
Debug FormatDate("%yyyy-%mm-%dd", Date())
Debug DayInText() + ", " + MonthInText() + FormatDate(" %dd, %yyyy", Date())

;Used procedures
Procedure.s DayInText()
  Protected d$
  Select DayOfWeek(Date())
    Case 1: d$="Monday"
    Case 2: d$="Tuesday"
    Case 3: d$="Wednesday"
    Case 4: d$="Thursday"
    Case 5: d$="Friday"
    Case 6: d$="Saturday"
    Default: d$="Sunday"
  EndSelect
  ProcedureReturn d$
EndProcedure

Procedure.s MonthInText()
  Protected  m$
  Select Month(Date())
    Case 1: m$="January"
    Case 2: m$="February"
    Case 3: m$="March"
    Case 4: m$="April"
    Case 5: m$="May"
    Case 6: m$="June"
    Case 7: m$="July"
    Case 8: m$="August"
    Case 9: m$="September"
    Case 10:m$="October"
    Case 11:m$="November"
    Default:m$="December"
  EndSelect
  ProcedureReturn m$
EndProcedure
```



## Prolog


{{works with|SWI-Prolog|6}}


```prolog

display_date :-
    get_time(Time),
    format_time(atom(Short), '%Y-%M-%d',      Time),
    format_time(atom(Long),  '%A, %B %d, %Y', Time),
    format('~w~n~w~n', [Short, Long]).

```



## Python

Formatting rules: http://docs.python.org/lib/module-time.html (strftime)


```python
import datetime
today = datetime.date.today()
# This one is built in:
print today.isoformat()
# Or use a format string for full flexibility:
print today.strftime('%Y-%m-%d')
```



## R

strftime is short for "string format time".

```rsplus
now <- Sys.time()
strftime(now, "%Y-%m-%d")
strftime(now, "%A, %B %d, %Y")
```



## Racket


Module racket/date is included within #lang racket
However its date to string facilities only provide ISO-8601
(which provides the "2007-11-10" format) but nothing
that strictly provides "<Full day name>, <Full month name> ...".

We therefore need to import SRFI/19 "SRFI 19: Time Data Types and Procedures"
(it's a standard Racket package).

See: http://srfi.schemers.org/srfi-19/srfi-19.html


```racket
#lang racket
(require srfi/19)

;;; The first required format is an ISO-8601 year-month-day format, predefined
;;; as ~1 in date->string
(displayln (date->string (current-date) "~1"))

;;; You should be able to see how each of the components of the following format string
;;; work...
;;; ~d is zero padded day of month:
(displayln (date->string (current-date) "~A, ~B ~d, ~Y"))
;;; ~e is space padded day of month:
(displayln (date->string (current-date) "~A, ~B ~e, ~Y"))
```

both ~d and ~e satisfy the format required, since the "10" part of that date is 2-digit

{{out}}

```txt
2013-04-02
Tuesday, April 02, 2013
Tuesday, April  2, 2013
```



## Raven


```raven>time int as today</lang


Short form:


```raven
today '%Y-%m-%d' date
```


Long form:


```raven
today '%A, %B %d, %Y' date
```



## REBOL


```REBOL
REBOL [
	Title: "Date Formatting"
	URL: http://rosettacode.org/wiki/Date_format
]

; REBOL has no built-in pictured output.

zeropad: func [pad n][
    n: to-string n
    insert/dup n "0" (pad - length? n)
    n
]
d02: func [n][zeropad 2 n]

print now ; Native formatting.

print rejoin [now/year  "-"  d02 now/month  "-"  d02 now/day]

print rejoin [
	pick system/locale/days now/weekday ", "
	pick system/locale/months now/month " "
	now/day ", " now/year
]
```


{{out}}

```txt
6-Dec-2009/10:02:10-5:00
2009-12-06
Sunday, December 6, 2009
```



## REXX

REXX has a number of ways to obtain the various components of the current date (indeed, any date).

It's up to the programmer to choose whatever version of the   '''date'''   BIF that best serves the purpose.

### idiomatic version


```REXX
/*REXX pgm shows current date:  yyyy-mm-dd  &  Dayofweek, Month dd, yyyy*/
   x = date('S')                       /*get current date as  yyyymmdd  */
yyyy = left(x,4)                       /*pick off year         (4 digs).*/
  dd = right(x,2)                      /*pick off day-of-month (2 digs).*/
  mm = substr(x,5,2)                   /*pick off month number (2 digs).*/
say yyyy'-'mm"-"dd                     /*yyyy-mm-dd with leading zeroes.*/

weekday = date('W')                    /*dayofweek (Monday or somesuch).*/
month   = date('M')                    /*Month     (August or somesuch).*/
zdd     = dd+0                         /*remove leading zero from  DD   */
say weekday',' month zdd"," yyyy       /*format date as:  Month dd, yyyy*/
                                       /*stick a fork in it, we're done.*/
```

{{out}}

```txt

2010-09-01
Wednesday, September 1, 2010

```



### compact version


```rexx
/*REXX pgm shows current date:  yyyy-mm-dd  &  Dayofweek, Month dd, yyyy*/
/* ╔══════════════════════════════════════════════════════════════════╗
   ║  function              returns               a specific example  ║
   ║ ───────────   ───────────────────────────   ──────────────────── ║
   ║  date()        dd mon yyyy                      3 Jun 2009       ║
   ║  date('N')     (same as above)                                   ║
   ║  date('B')     days since Jan 1, year 1           735382         ║
   ║  date('C')     days since Jan 1 this century       5264          ║
   ║  date('D')     days since Jan 1, this yr            154          ║
   ║  date('E')     dd/mm/yy                          03/06/09        ║
   ║  date('I')     yyyy-mm-dd                       2009-03-06       ║
   ║  date('J')     yyddd                              2009154        ║
   ║  date('M')     month name                          June          ║
   ║  date('O')     yy/mm/dd                          09/03/06        ║
   ║  date('S')     yyyyddmm                          20090603        ║
   ║  date('T')     seconds since Jan 1st, 1970      1401483686       ║
   ║  date('U')     mm/dd/yy                          03/06/09        ║
   ║  date('W')     day─of─the─week                   Wednesday       ║
   ║ ───────────   ───────────────────────────   ──────────────────── ║
   ║                                                                  ║
   ║  Note:  not all of the above are supported by all REXX versions. ║
   ╚══════════════════════════════════════════════════════════════════╝ */
parse value date('S') with yyyy 5 mm 7 dd   /*get various pieces of date*/
say yyyy'-'mm"-"dd                     /*yyyy-mm-dd with leading zeroes.*/

say  date('W')","   date('M')   word(date(), 1)","   yyyy
                                       /* [↑]  dayofweek  Month dd, yyyy*/
                                       /*stick a fork in it, we're done.*/
```

'''output''' would be the same as the 1<sup>st</sup> version.


### modern version

This version can be used with those REXXes that support the   '''I'''   (ISO) parameter for the   '''date'''   BIF.

```REXX
/*REXX pgm shows current date:  yyyy-mm-dd  &  Dayofweek, Month dd, yyyy*/
say date('I')                          /*yyyy-mm-dd with leading zeroes.*/

say  date('W')","   date('M')   word(date(), 1)","  left(date('S'),4)
                                       /* [↑]  dayofweek  Month dd, yyyy*/
                                       /*stick a fork in it, we're done.*/

```

'''output''' would be the same as the 1<sup>st</sup> version.





## Ring

<lang>
dt = list(21)
dt[1] = "abbreviated weekday name"
dt[2] = "full weekday name"
dt[3] = "abbreviated month name"
dt[4] = "full month name"
dt[5] = "Date & Time"
dt[6] = "Day of the month"
dt[7] = "Hour (24)"
dt[8] = "Hour (12)"
dt[9] = "Day of the year"
dt[10] = "Month of the year"
dt[11] = "Minutes after hour"
dt[12] = "AM or PM"
dt[13] = "Seconds after the hour"
dt[14] = "Week of the year (sun-sat)"
dt[15] = "day of the week"
dt[16] = "date"
dt[17] = "time"
dt[18] = "year of the century"
dt[19] = "year"
dt[20] = "time zone"
dt[21] = "percent sign"

for i=1 to 21
     see dt[i] + " : " + TimeList () [i] + nl
next

```



## Ruby

Formatting rules: [//www.ruby-doc.org/core/Time.html#method-i-strftime Time#strftime]


```ruby
puts Time.now
puts Time.now.strftime('%Y-%m-%d')
puts Time.now.strftime('%F')            # same as %Y-%m-%d (ISO 8601 date formats)
puts Time.now.strftime('%A, %B %d, %Y')
```

{{out}}

```txt

2015-02-08 10:48:45 +0900
2015-02-08
2015-02-08
Sunday, February 08, 2015

```



## Run BASIC


```runbasic
'Display the current date in the formats of "2007-11-10" and "Sunday, November 10, 2007".
print date$("yyyy-mm-dd")
print date$("dddd");", ";                     'return full day of the week (eg. Wednesday
print date$("mmmm");" ";                      'return full month name (eg. March)
print date$("dd, yyyy")                       'return day, year
```

{{out}}

```txt

2012-03-16
Friday, March 16, 2012

```



## Rust

Using <code>chrono 0.4.6</code>

```rust
fn main() {
    let now = chrono::Utc::now();
    println!("{}", now.format("%Y-%m-%d"));
    println!("{}", now.format("%A, %B %d, %Y"));
}
```



## Scala


```scala
val now=new Date()
println("%tF".format(now))
println("%1$tA, %1$tB %1$td, %1$tY".format(now))
```



## Scheme


{{works with|Guile|2.0.13}}


```Scheme
(define short-date
  (lambda (lt)
    (strftime "%Y-%m-%d" (localtime lt))))

(define long-date
  (lambda (lt)
    (strftime "%A, %B %d, %Y" (localtime lt))))

(define main
  (lambda (args)
    ;; Current date
    (let ((dt (car (gettimeofday))))
      ;; Short style
      (display (short-date dt))(newline)
      ;; Long style
      (display (long-date dt))(newline))))


```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "time.s7i";

const proc: main is func
  local
    const array string: months is [] ("January", "February", "March", "April", "May", "June",
        "July", "August", "September", "October", "November", "December");
    const array string: days is [] ("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday");
    var time: now is time.value;
  begin
    now := time(NOW);
    writeln(strDate(now));
    writeln(days[dayOfWeek(now)] <& ", " <& months[now.month] <& " " <& now.day <& ", " <& now.year);
  end func;
```



## Shiny


```shiny
say time.format 'Y-m-d' time.now
say time.format 'l, F j, Y' time.now
```



## Sidef


```ruby
var time = Time.local;
say time.ctime;
say time.strftime("%Y-%m-%d");
say time.strftime("%A, %B %d, %Y");
```

{{out}}

```txt

Fri Oct 17 12:57:02 2014
2014-10-17
Friday, October 17, 2014

```



## Smalltalk

In Smalltalk, one sends a <tt>printFormat</tt> message to a Date object with year, month, and day. For other date strings, one must construct the date string from the parts of a Date object. Of course, you'd probably want to make a method for doing so.


```smalltalk
| d |
d := Date today.
d printFormat: #(3 2 1 $- 1 1 2).
(d weekday asString), ', ', (d monthName), ' ', (d dayOfMonth asString), ', ', (d year asString)
```

{{works with|Smalltalk/X}}

```smalltalk
Date today printOn:Stdout format:'%y-%m-%d'.
Date today printOn:Stdout format:'%(DayName), %(MonthName) %d, %y' language:#en.
```



## SQL

{{works with|Oracle}}

```sql

select to_char(sysdate,'YYYY-MM-DD') date_fmt_1 from dual;

select to_char(sysdate,'fmDay, Month DD, YYYY') date_fmt_2 from dual;

```



```txt

SQL>
DATE_FMT_1
----------
2016-12-12

SQL> SQL>
DATE_FMT_2
--------------------------------------------------------------------------------
Monday, December 12, 2016

```



## Standard ML

Formatting rules: http://www.standardml.org/Basis/date.html#SIG:DATE.fmt:VAL


```sml
print (Date.fmt "%Y-%m-%d" (Date.fromTimeLocal (Time.now ())) ^ "\n");
print (Date.fmt "%A, %B %d, %Y" (Date.fromTimeLocal (Time.now ())) ^ "\n");
```

{{out}}

```txt

2008-02-13
Wednesday, February 13, 2008

```



## Stata


```stata
display %tdCCYY-NN-DD td($S_DATE)
display %tdDayname,_Month_dd,_CCYY td($S_DATE)
```


## Suneido


```Suneido
Date().Format('yyyy-MM-dd')  -->  "2010-03-16"
Date().LongDate() -->  "Tuesday, March 16, 2010"
```



## Swift


```Swift
import Foundation
extension String {
    func toStandardDateWithDateFormat(format: String) -> String {
        let dateFormatter = NSDateFormatter()
        dateFormatter.dateFormat = format
        dateFormatter.dateStyle = .LongStyle
        return dateFormatter.stringFromDate(dateFormatter.dateFromString(self)!)
    }
}

let date = "2015-08-28".toStandardDateWithDateFormat("yyyy-MM-dd")
```


{{out}}

```txt

2015ꈎ8ꆪ28ꑍ（In Sichuan Yi Language Environment）

```



## Tcl


```tcl
set now [clock seconds]
puts [clock format $now -format "%Y-%m-%d"]
puts [clock format $now -format "%A, %B %d, %Y"]
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
SET dayofweek = DATE (today,day,month,year,number)

SET months=*
DATA January
DATA Februari
DATA March
DATA April
DATA Mai
DATA June
DATA July
DATA August
DATA September
DATA October
DATA November
DATA December

SET days="Monday'Tuesday'Wendsday'Thursday'Fryday'Saturday'Sonday"

SET nameofday  =SELECT (days,#dayofweek)
SET nameofmonth=SELECT (months,#month)

SET format1=JOIN (year,"-",month,day)
SET format2=CONCAT (nameofday,", ",nameofmonth," ",day, ", ",year)

PRINT format1
PRINT format2

```

{{out}}

```txt

2011-1-5
Wendsday, January 5, 2011

```



## UNIX Shell


```bash
date +"%Y-%m-%d"
date +"%A, %B %d, %Y"
```


On a new enough system <code>%F</code> is equivalent to <code>%Y-%m-%d</code>

```bash
date +"%F"
```


<!-- ENHANCE-ME: what is new enough for %F ?  For example it doesn't appear in these bits of POSIX,
       http://pubs.opengroup.org/onlinepubs/009604599/utilities/date.html
       http://pubs.opengroup.org/onlinepubs/9699919799/utilities/date.html
-->


## Ursa

{{works with|Cygnus/X Ursa}}
Cygnus/X Ursa can import and call Java classes.

```ursa
cygnus/x ursa v0.78 (default, release 0)
[Oracle Corporation JVM 1.8.0_51 on Mac OS X 10.10.5 x86_64]
> import "java.util.Date"
> import "java.text.SimpleDateFormat"
> decl java.text.SimpleDateFormat sdf
> sdf.applyPattern "yyyy-MM-dd"
> decl java.util.Date d
> out (sdf.format d) endl console
2016-07-23
> sdf.applyPattern "EEEE, MMMM dd, yyyy"
> out (sdf.format d) endl console
Saturday, July 23, 2016
> _
```



## Ursala

The method is to transform a date in standard format returned by the library function, now.

```Ursala
#import std
#import cli

months = ~&p/block3'JanFebMarAprMayJunJulAugSepOctNovDec' block2'010203040506070809101112'

completion =

-:~& ~&pllrTXS/block3'SunMonTueWedThuFriSat'--(~&lS months) -- (
   --','* sep`, 'day,day,sday,nesday,rsday,day,urday',
   sep`, 'uary,ruary,ch,il,,e,y,ust,tember,ober,ember,ember')

text_form    = sep` ; mat` + completion*+ <.~&hy,~&tth,--','@th,~&ttth>
numeric_form = sep` ; mat`-+ <.~&ttth,@tth -: months,~&th>

#show+

main = <.text_form,numeric_form> now0
```

{{out}}

```txt
Wednesday, June 24, 2009
2009-06-24
```




## VBA



```VBA
Function DateFormats()
    Debug.Print Format(Date, "yyyy-mm-dd")
    Debug.Print Format(Date, "dddd, mmmm dd yyyy")
End Function
```



## VBScript


```vb

'YYYY-MM-DD format
WScript.StdOut.WriteLine Year(Date) & "-" & Right("0" & Month(Date),2) & "-" & Right("0" & Day(Date),2)

'Weekday_Name, Month_Name DD, YYYY format
WScript.StdOut.WriteLine FormatDateTime(Now,1)

```


{{Out}}

```txt

2015-08-31
Monday, August 31, 2015

```



## Vedit macro language

Display current date in format "2007-11-10":


```vedit
Date(REVERSE+NOMSG+VALUE, '-')
```


Display current date in format "Sunday, November 10, 2007" (Requires VEDIT 6.2):

```vedit
// Get todays date into #1, #2, #3 and #7
#1 = Date_Day
#2 = Date_Month
#3 = Date_Year
#7 = JDate() % 7              // #7 = weekday

// Convert weekday number (in #7) into word in T-reg 1
if (#7==0) { RS(1,"Sunday") }
if (#7==1) { RS(1,"Monday") }
if (#7==2) { RS(1,"Tuesday") }
if (#7==3) { RS(1,"Wednesday") }
if (#7==4) { RS(1,"Thursday") }
if (#7==5) { RS(1,"Friday") }
if (#7==6) { RS(1,"Saturday") }

// Convert month number (in #2) into word in T-reg 2
if (#2==1)  { RS(2,"January") }
if (#2==2)  { RS(2,"February") }
if (#2==3)  { RS(2,"March") }
if (#2==4)  { RS(2,"April") }
if (#2==5)  { RS(2,"May") }
if (#2==6)  { RS(2,"June") }
if (#2==7)  { RS(2,"July") }
if (#2==8)  { RS(2,"August") }
if (#2==9)  { RS(2,"September") }
if (#2==10) { RS(2,"October") }
if (#2==11) { RS(2,"November") }
if (#2==12) { RS(2,"December") }

// Display the date string
RT(1) M(", ") RT(2) M(" ") NT(#1, LEFT+NOCR) M(",") NT(#3)
```


To insert the date string into edit buffer instead of displaying it, replace the last line with this:


```vedit
RI(1) IT(", ") RI(2) IT(" ") NI(#1, LEFT+NOCR) IT(",") NI(#3)
```



## XPL0


```XPL0
include c:\cxpl\codes;
int CpuReg, Year, Month, Day, DName, MName, WDay;
[CpuReg:= GetReg;       \access CPU registers
CpuReg(0):= $2A00;      \DOS system call
SoftInt($21);
Year:= CpuReg(2);
Month:= CpuReg(3) >> 8;
Day:= CpuReg(3) & $FF;
WDay:= CpuReg(0) & $FF;
IntOut(0, Year);  ChOut(0, ^-);
if Month<10 then ChOut(0, ^0);  IntOut(0, Month);  ChOut(0, ^-);
if Day<10 then ChOut(0, ^0);  IntOut(0, Day);  CrLf(0);
DName:= ["Sunday", "Monday", "Tuesday", "Wednesday",
        "Thursday", "Friday", "Saturday"];
MName:= [0, "January", "February", "March", "April", "May", "June", "July",
        "August", "September", "October", "November", "December"];
Text(0, DName(WDay));  Text(0, ", ");  Text(0, MName(Month));  Text(0, " ");
IntOut(0, Day);  Text(0, ", ");  IntOut(0, Year);  CrLf(0);
]
```


{{out}}

```txt

2012-11-20
Tuesday, November 20, 2012

```



## Yabasic


```Yabasic
dim n$(1)

n = token(date$, n$(), "-")

print n$(4), "-", n$(2), "-", n$(3)
print nDay$(n$(5)), ", ", nMonth$(n$(6)), " ", n$(3), ", ", n$(4)

sub nDay$(n$)
	switch n$
	case "Mon": case "Fri": case "Sun": break
	case "Tue": n$ = n$ + "s" : break
	case "Wed": n$ = n$ + "nes" : break
	case "Thu": n$ = n$ + "rs" : break
	case "Sat": n$ = n$ + "ur" : break
	default: n$ = "none" : break
	end switch

	return n$ + "day"
end sub

sub nMonth$(n$)
	local month$(1), n

	n = token("January, February, March, April, May, June, July, August, September, October, November, December", month$(), ", ")
	n = instr("JanFebMarAprMayJunJulAugSepOctNovDec", n$)

	return month$(int(n/3) + 1)
end sub
```



## zkl


```zkl
"%d-%02d-%02d".fmt(Time.Clock.localTime.xplode()).println()
//--> "2014-02-28" (ISO format)
```

Not quite but close. Even though localTime returns a 7 tuple (and xplode pushes all 7 as call args), fmt only eats what it needs.

```zkl
Time.Date.prettyDay(Time.Clock.localTime.xplode())
//--> "Friday, the 28th of February 2014"
```


```zkl
y,m,d:=Time.Clock.localTime; D:=Time.Date;
"%s, %s %d, %d".fmt(D.dayName(D.weekDay(y,m,d)),
                    D.monthName(m), d,y)
//-->"Friday, February 28, 2014"
```



## zonnon


```zonnon

module Main;
import System;

var
	now: System.DateTime;
begin
	now := System.DateTime.Now;
	System.Console.WriteLine(now.ToString("yyyy-MM-dd");
	System.Console.WriteLine("{0}, {1}",now.DayOfWeek,now.ToString("MMMM dd, yyyy"));
end Main.

```

{{Out}}

```txt

2017-12-05
Tuesday, diciembre 05, 2017

```


{{omit from|ML/I}}
{{omit from|PARI/GP|No access to date/time information}}

[[Category:Date and time]]
