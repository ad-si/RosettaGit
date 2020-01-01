+++
title = "Date manipulation"
description = ""
date = 2019-10-08T09:08:02Z
aliases = []
[extra]
id = 4157
[taxonomies]
categories = []
tags = []
+++

{{task|Text processing}}
[[Category:Date and time]]
{{omit from|ML/I}}
{{omit from|PARI/GP|No real capacity for string manipulation}}

;Task:
Given the date string "March 7 2009 7:30pm EST",

output the time 12 hours later in any human-readable format.

As extra credit, display the resulting time in a time zone different from your own.





## 11l


```11l
V format_str = ‘%B %d %Y %I:%M%p’
print((time:strptime(‘March 7 2009 7:30pm’, format_str)
     + TimeDelta(hours' 12)).strftime(format_str))
```



## Ada

The Ada way: long, type-based, clear, reliable.
Most of the code consists of declarations.
Only standard libraries are required.


```Ada
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Integer_Text_IO;
with Ada.Text_IO;

procedure Date_Manipulation is

   type Month_Name_T is
     (January, February, March, April, May, June,
      July, August, September, October, November, December);

   type Time_Zone_Name_T is (EST, Lisbon);

   type Period_T is (AM, PM);

   package TZ renames Ada.Calendar.Time_Zones;
   use type TZ.Time_Offset;

   Time_Zone_Offset : array (Time_Zone_Name_T) of TZ.Time_Offset :=
     (EST => -5 * 60,
      Lisbon => 0);

   Period_Offset : array (Period_T) of Natural :=
     (AM => 0,
      PM => 12);

   package Month_Name_IO is
      new Ada.Text_IO.Enumeration_IO (Month_Name_T);

   package Time_Zone_Name_IO is
      new Ada.Text_IO.Enumeration_IO (Time_Zone_Name_T);

   package Period_IO is
      new Ada.Text_IO.Enumeration_IO (Period_T);

   package Std renames Ada.Calendar;
   use type Std.Time;

   package Fmt renames Std.Formatting;

   function To_Number (Name : Month_Name_T) return Std.Month_Number is
   begin
      return Std.Month_Number (Month_Name_T'Pos (Name) + 1);
   end;

   function To_Time (S : String) return Std.Time is
      Month : Month_Name_T;
      Day : Std.Day_Number;
      Year : Std.Year_Number;
      Hour : Fmt.Hour_Number;
      Minute : Fmt.Minute_Number;
      Period : Period_T;
      Time_Zone : Time_Zone_Name_T;
      I : Natural;
   begin
      Month_Name_IO.Get
        (From => S, Item => Month, Last => I);
      Ada.Integer_Text_IO.Get
        (From => S (I + 1 .. S'Last), Item => Day, Last => I);
      Ada.Integer_Text_IO.Get
        (From => S (I + 1 .. S'Last), Item => Year, Last => I);
      Ada.Integer_Text_IO.Get
        (From => S (I + 1 .. S'Last), Item => Hour, Last => I);
      Ada.Integer_Text_IO.Get
        (From => S (I + 2 .. S'Last), Item => Minute, Last => I);
         --  here we start 2 chars down to skip the ':'
      Period_IO.Get
        (From => S (I + 1 .. S'Last), Item => Period, Last => I);
      Time_Zone_Name_IO.Get
        (From => S (I + 1 .. S'Last), Item => Time_Zone, Last => I);
      return Fmt.Time_Of
        (Year => Year,
         Month => To_Number (Month),
         Day => Day,
         Hour => Hour + Period_Offset (Period),
         Minute => Minute,
         Second => 0,
         Time_Zone => Time_Zone_Offset (Time_Zone));
   end;

   function Img
     (Date : Std.Time; Zone : Time_Zone_Name_T) return String is
   begin
      return
         Fmt.Image (Date => Date, Time_Zone => Time_Zone_Offset (Zone)) &
         " " & Time_Zone_Name_T'Image (Zone);
   end;

   T1, T2 : Std.Time;
   use Ada.Text_IO;
begin
   T1 := To_Time ("March 7 2009 7:30pm EST");
   T2 := T1 + 12.0 * 60.0 * 60.0;
   Put_Line ("T1 => " & Img (T1, EST) & " = " & Img (T1, Lisbon));
   Put_Line ("T2 => " & Img (T2, EST) & " = " & Img (T2, Lisbon));
end;
```


Result:

```txt

T1 => 2009-03-07 19:30:00 EST = 2009-03-08 00:30:00 LISBON
T2 => 2009-03-08 07:30:00 EST = 2009-03-08 12:30:00 LISBON

```



## AppleScript

AppleScript has a built-in date class and can coerce a string to a date automatically. It also has reserved constants such as <code>hours</code> which are defined in the unit of seconds. There is no built-in support for time zones.

```AppleScript
set x to "March 7 2009 7:30pm EST"
return (date x) + 12 * hours
```


Result is:

```AppleScript
date "Sunday, March 8, 2009 7:30:00 AM"
```



## AutoHotkey


```autohotkey
DateString := "March 7 2009 7:30pm EST"

; split the given string with RegExMatch
Needle := "^(?P<mm>\S*) (?P<d>\S*) (?P<y>\S*) (?P<t>\S*) (?P<tz>\S*)$"
RegExMatch(DateString, Needle, $)

; split the time with RegExMatch
Needle := "^(?P<h>\d+):(?P<min>\d+)(?P<xm>[amp]+)$"
RegExMatch($t, Needle, $)

; convert am/pm to 24h format
$h += ($xm = "am") ? 0 : 12

; knitting YYYYMMDDHH24MI format
_YYYY := $y
_MM   := Get_MonthNr($mm)
_DD   := SubStr("00" $d, -1) ; last 2 chars
_HH24 := SubStr("00" $h, -1) ; last 2 chars
_MI   := $min
YYYYMMDDHH24MI := _YYYY _MM _DD _HH24 _MI

; add 12 hours as requested
EnvAdd, YYYYMMDDHH24MI, 12, Hours
FormatTime, HumanReadable, %YYYYMMDDHH24MI%, d/MMM/yyyy  HH:mm

; add 5 hours to convert to different timezone (GMT)
EnvAdd, YYYYMMDDHH24MI, 5, Hours
FormatTime, HumanReadable_GMT, %YYYYMMDDHH24MI%, d/MMM/yyyy  HH:mm

; output
MsgBox, % "Given: " DateString "`n`n"
        . "12 hours later:`n"
        . "(" $tz "):`t" HumanReadable "h`n"
        . "(GMT):`t" HumanReadable_GMT "h`n"


;---------------------------------------------------------------------------
Get_MonthNr(Month) { ; convert named month to 2-digit number
;---------------------------------------------------------------------------
    If (Month = "January")
        Result := "01"
    Else If (Month = "February")
        Result := "02"
    Else If (Month = "March")
        Result := "03"
    Else If (Month = "April")
        Result := "04"
    Else If (Month = "May")
        Result := "05"
    Else If (Month = "June")
        Result := "06"
    Else If (Month = "July")
        Result := "07"
    Else If (Month = "August")
        Result := "08"
    Else If (Month = "September")
        Result := "09"
    Else If (Month = "October")
        Result := "10"
    Else If (Month = "November")
        Result := "11"
    Else If (Month = "December")
        Result := "12"
    Return, Result
}
```

{{out|Message box shows}}

```txt
Given: March 7 2009 7:30pm EST

12 hours later:
(EST):	8/Mar/2009  07:30h
(GMT):	8/Mar/2009  12:30h
```



## AWK


```AWK

# syntax: GAWK -f DATE_MANIPULATION.AWK
BEGIN {
    fmt = "%a %Y-%m-%d %H:%M:%S %Z" # DAY YYYY-MM-DD HH:MM:SS TZ
    split("March 7 2009 7:30pm EST",arr," ")
    M = (index("JanFebMarAprMayJunJulAugSepOctNovDec",substr(arr[1],1,3)) + 2) / 3
    D = arr[2]
    Y = arr[3]
    hhmm = arr[4]
    hh = substr(hhmm,1,index(hhmm,":")-1) + 0
    mm = substr(hhmm,index(hhmm,":")+1,2) + 0
    if (hh == 12 && hhmm ~ /am/) { hh = 0 }
    else if (hh < 12 && hhmm ~ /pm/) { hh += 12 }
    time = mktime(sprintf("%d %d %d %d %d %d",Y,M,D,hh,mm,0))
    printf("time:    %s\n",strftime(fmt,time))
    time += 12*60*60
    printf("+12 hrs: %s\n",strftime(fmt,time))
    exit(0)
}

```

{{out}}

```txt

time:    Sat 2009-03-07 19:30:00 Eastern Standard Time
+12 hrs: Sun 2009-03-08 08:30:00 Eastern Daylight Time

```



## Batch File


```dos

@echo off

call:Date_Manipulation "March 7 2009 7:30pm EST"
call:Date_Manipulation "February 28 2009 2:28pm EST"
call:Date_Manipulation "February 29 2000 9:52pm EST"
pause>nul
exit /b

:Date_Manipulation
setlocal enabledelayedexpansion

:: These are the arrays we'll be using
set daysinmonth=31 28 31 30 31 30 31 31 30 31 30 31
set namesofmonths=January February March April May June July August September October November December

:: Separate the date given ("%1") into respective variables. Note: For now the "am/pm" is attached to %minutes%
for /f "tokens=1,2,3,4,5,6 delims=: " %%i in ("%~1") do (
  set monthname=%%i
  set day=%%j
  set year=%%k
  set hour=%%l
  set minutes=%%m
  set timezone=%%n
)

:: Separate the am/pm and the minutes value into different variables
set ampm=%minutes:~2,2%
set minutes=%minutes:~0,2%

:: Check if the day needs to be changed based on the status of "am/pm"
if %ampm%==pm (
  set /a day+=1
  set ampm=am
) else (
  set ampm=pm
)

:: Get the number corresponding to the month given
set tempcount=0
for %%i in (%namesofmonths%) do (
  set /a tempcount+=1
  if %monthname%==%%i set monthcount=!tempcount!
)

:: As this step may may be needed to repeat if the month needs to be changed, we add a label here
:getdaysinthemonth
:: Work out how many days are in the current month
set tempcount=0
for %%i in (%daysinmonth%) do (
  set /a tempcount+=1
  if %monthcount%==!tempcount! set daysinthemonth=%%i
)

:: If the month is February, check if it is a leap year. If so, add 1 to the amount of days in the month
if %daysinthemonth%==28 (
  set /a leapyearcheck=%year% %% 4
  if !leapyearcheck!==0 set /a daysinthemonth+=1
)

:: Check if the month needs to be changed based on the current day and how many days there are in the current month
if %day% gtr %daysinthemonth% (
  set /a monthcount+=1
  set day=1
  if !monthcount! gtr 12 (
    set monthcount=1
    set /a year+=1
  )
  goto getdaysinthemonth
)
:: Everything from :getdaysinthemonth will be repeated once if the month needs to be changed

:: This block is only required to change the name of the month for the output, however as you have %monthcount%, this is optional
set tempcount=0
for %%i in (%namesofmonths%) do (
  set /a tempcount+=1
  if %monthcount%==!tempcount! set monthname=%%i
)

echo Original    - %~1
echo Manipulated - %monthname% %day% %year% %hour%:%minutes%%ampm% %timezone%
exit /b

```

The code takes 3 inputs to demonstrate the ability to deal with leap years.

{{in}}

```txt

"March 7 2009 7:30pm EST"
"February 28 2009 2:28pm EST"
"February 29 2000 9:52pm EST"

```


{{out}}

```txt

Original    - March 7 2009 7:30pm EST
Manipulated - March 8 2009 7:30am EST
Original    - February 28 2009 2:28pm EST
Manipulated - March 1 2009 2:28am EST
Original    - February 29 2000 9:52pm EST
Manipulated - March 1 2000 9:52am EST

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      INSTALL @lib$+"DATELIB"

      date$ = "March 7 2009 7:30pm EST"

      mjd% = FN_readdate(date$, "mdy", 0)
      colon% = INSTR(date$, ":")
      hours% = VAL(MID$(date$, colon%-2))
      IF INSTR(date$, "am") IF hours%=12  hours% -= 12
      IF INSTR(date$, "pm") IF hours%<>12 hours% += 12
      mins% = VAL(MID$(date$, colon%+1))

      now% = mjd% * 1440 + hours% * 60 + mins%
      new% = now% + 12 * 60 : REM 12 hours later

      PRINT FNformat(new%, "EST")
      PRINT FNformat(new% + 5 * 60, "GMT")
      PRINT FNformat(new% - 3 * 60, "PST")
      END

      DEF FNformat(datetime%, zone$)
      LOCAL mjd%, hours%, mins%, ampm$
      mjd% = datetime% DIV 1440
      hours% = (datetime% DIV 60) MOD 24
      mins% = datetime% MOD 60

      IF hours% < 12 THEN ampm$ = "am" ELSE ampm$ = "pm"
      IF hours% = 0 hours% += 12
      IF hours% > 12 hours% -= 12

      = FN_date$(mjd%, "MMMM d yyyy") + " " + STR$(hours%) + \
      \ ":" + RIGHT$("0"+STR$(mins%), 2) + ampm$ + " " + zone$
      ENDPROC

```

{{out}}

```txt
March 8 2009 7:30am EST
March 8 2009 12:30pm GMT
March 8 2009 4:30am PST
```



## C

{{works with|POSIX}}

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
  struct tm ts;
  time_t t;
  const char *d = "March 7 2009 7:30pm EST";

  strptime(d, "%B %d %Y %I:%M%p %Z", &ts);
  /* ts.tm_hour += 12; instead of t += 12*60*60
     works too. */
  t = mktime(&ts);
  t += 12*60*60;
  printf("%s", ctime(&t));

  return EXIT_SUCCESS;
}
```


Note: <code>ctime</code> treats the date as local, so that it is like the timezone information were discarded (to see the passage to daylight saving time I must change the date into March 28... no matter the timezone specified)

## C#

```c#
class Program
{
    static void Main(string[] args)
    {
        CultureInfo ci=CultureInfo.CreateSpecificCulture("en-US");
        string dateString = "March 7 2009 7:30pm EST";
        string format = "MMMM d yyyy h:mmtt z";
        DateTime myDateTime = DateTime.ParseExact(dateString.Replace("EST","+6"),format,ci) ;
        DateTime newDateTime = myDateTime.AddHours(12).AddDays(1) ;
        Console.WriteLine(newDateTime.ToString(format).Replace("-5","EST")); //probably not the best way to do this

        Console.ReadLine();
    }
}
```



## C++

{{libheader|Boost}}

compiled with g++ -lboost_date_time

```cpp
#include <string>
#include <iostream>
#include <boost/date_time/local_time/local_time.hpp>
#include <sstream>
#include <boost/date_time/gregorian/gregorian.hpp>
#include <vector>
#include <boost/algorithm/string.hpp>
#include <cstdlib>
#include <locale>


int main( ) {
   std::string datestring ("March 7 2009 7:30pm EST" ) ;
   //we must first parse the date string into a date , a time and a time
   //zone part , to take account of present restrictions in the input facets
   //of the Boost::DateTime library used for this example
   std::vector<std::string> elements ;
   //parsing the date string
   boost::split( elements , datestring , boost::is_any_of( " " ) ) ;
   std::string datepart = elements[ 0 ] + " " + "0" + elements[ 1 ] + " " +
      elements[ 2 ] ; //we must add 0 to avoid trouble with the boost::date_input format strings
   std::string timepart = elements[ 3 ] ;
   std::string timezone = elements[ 4 ] ;
   const char meridians[ ] = { 'a' , 'p' } ;
   //we have to find out if the time is am or pm, to change the hours appropriately
   std::string::size_type found = timepart.find_first_of( meridians, 0 ) ;
   std::string twelve_hour ( timepart.substr( found , 1 ) ) ;
   timepart = timepart.substr( 0 , found ) ; //we chop off am or pm
   elements.clear( ) ;
   boost::split( elements , timepart , boost::is_any_of ( ":" ) ) ;
   long hour = std::atol( (elements.begin( ))->c_str( ) ) ;// hours in the string
   if ( twelve_hour == "p" ) //it's post meridian, we're converting to 24-hour-clock
      hour += 12 ;
   long minute = std::atol( ( elements.begin( ) + 1)->c_str( ) ) ;
   boost::local_time::tz_database tz_db ;
   tz_db.load_from_file( "/home/ulrich/internetpages/date_time_zonespec.csv" ) ;
   //according to the time zone database, this corresponds to one possible EST time zone
   boost::local_time::time_zone_ptr dyc = tz_db.time_zone_from_region( "America/New_York" ) ;
   //this is the string input format to initialize the date field
   boost::gregorian::date_input_facet *f =
      new boost::gregorian::date_input_facet( "%B %d %Y"  ) ;
   std::stringstream ss ;
   ss << datepart ;
   ss.imbue( std::locale( std::locale::classic( ) , f ) ) ;
   boost::gregorian::date d ;
   ss >> d ;
   boost::posix_time::time_duration td (  hour , minute , 0  ) ;
   //that's how we initialize the New York local time , by using date and adding
   //time duration with values coming from parsed date input string
   boost::local_time::local_date_time lt ( d , td ,  dyc ,
	 boost::local_time::local_date_time::NOT_DATE_TIME_ON_ERROR ) ;
   std::cout << "local time: " << lt << '\n' ;
   ss.str( "" ) ;
   ss << lt ;
   //we have to add 12 hours, so a new time duration object is created
   boost::posix_time::time_duration td2 (12 , 0 , 0 , 0 ) ;
   boost::local_time::local_date_time ltlater = lt + td2 ; //local time 12 hours later
   boost::gregorian::date_facet *f2 =
      new boost::gregorian::date_facet( "%B %d %Y , %R %Z" ) ;
   std::cout.imbue( std::locale( std::locale::classic( ) , f2 ) ) ;
   std::cout << "12 hours after " << ss.str( )  << " it is " << ltlater << " !\n" ;
   //what's New York time in the Berlin time zone ?
   boost::local_time::time_zone_ptr bt = tz_db.time_zone_from_region( "Europe/Berlin" ) ;
   std::cout.imbue( std::locale( "de_DE.UTF-8" ) ) ; //choose the output forman appropriate for the time zone
   std::cout << "This corresponds to " << ltlater.local_time_in( bt ) << " in Berlin!\n" ;
   return 0 ;
}

```

{{out}}

```txt

local time: 2009-Mar-07 19:30:00 EST
12 hours after 2009-Mar-07 19:30:00 EST it is 2009-Mar-08 08:30:00 EDT !
This corresponds to 2009-Mär-08 13:30:00 CET in Berlin!


```



## Clojure


```Clojure
(import java.util.Date
	java.text.SimpleDateFormat)

(defn time+12 [s]
  (let [sdf (SimpleDateFormat. "MMMM d yyyy h:mma zzz")]
    (-> (.parse sdf s)
	(.getTime ,)
	(+ , 43200000)
	long
	(Date. ,)
	(->> , (.format sdf ,)))))
```



## COBOL

Tested with GnuCOBOL

Two parts to this example. Following the task spec using POSIX routines, and a more standardized COBOL form.  COBOL 2014 uses ISO8601 date and time formats, and these formats may be more common in COBOL applications.


```cobol
       identification division.
       program-id. date-manipulation.

       environment division.
       configuration section.
       repository.
           function all intrinsic.

       data division.
       working-storage section.
       01 given-date.
          05 filler            value z"March 7 2009 7:30pm EST".
       01 date-spec.
          05 filler            value z"%B %d %Y %I:%M%p %Z".

       01 time-struct.
          05 tm-sec            usage binary-long.
          05 tm-min            usage binary-long.
          05 tm-hour           usage binary-long.
          05 tm-mday           usage binary-long.
          05 tm-mon            usage binary-long.
          05 tm-year           usage binary-long.
          05 tm-wday           usage binary-long.
          05 tm-yday           usage binary-long.
          05 tm-isdst          usage binary-long.
          05 tm-gmtoff         usage binary-c-long.
          05 tm-zone           usage pointer.
       01 scan-index           usage pointer.

       01 time-t               usage binary-c-long.
       01 time-tm              usage pointer.

       01 reform-buffer        pic x(64).
       01 reform-length        usage binary-long.

       01 current-locale       usage pointer.

       01 iso-spec             constant as "YYYY-MM-DDThh:mm:ss+hh:mm".
       01 iso-date             constant as "2009-03-07T19:30:00-05:00".
       01 date-integer         pic 9(9).
       01 time-integer         pic 9(9).

       procedure division.

       call "strptime" using
           by reference given-date
           by reference date-spec
           by reference time-struct
           returning scan-index
           on exception
               display "error calling strptime" upon syserr
       end-call
       display "Given: " given-date

       if scan-index not equal null then
           *> add 12 hours, and reform as local
           call "mktime" using time-struct returning time-t
           add 43200 to time-t
           perform form-datetime

           *> reformat as Pacific time
           set environment "TZ" to "PST8PDT"
           call "tzset" returning omitted
           perform form-datetime

           *> reformat as Greenwich mean
           set environment "TZ" to "GMT"
           call "tzset" returning omitted
           perform form-datetime


           *> reformat for Tokyo time, as seen in Hong Kong
           set environment "TZ" to "Japan"
           call "tzset" returning omitted
           call "setlocale" using by value 6 by content z"en_HK.utf8"
               returning current-locale
               on exception
                   display "error with setlocale" upon syserr
           end-call
           move z"%c" to date-spec
           perform form-datetime
       else
           display "date parse error" upon syserr
       end-if

      *> A more standard COBOL approach, based on ISO8601
       display "Given: " iso-date
       move integer-of-formatted-date(iso-spec, iso-date)
         to date-integer

       move seconds-from-formatted-time(iso-spec, iso-date)
         to time-integer

       add 43200 to time-integer
       if time-integer greater than 86400 then
           subtract 86400 from time-integer
           add 1 to date-integer
       end-if
       display "       " substitute(formatted-datetime(iso-spec
                   date-integer, time-integer, -300), "T", "/")

       goback.

       form-datetime.
       call "localtime" using time-t returning time-tm
       call "strftime" using
           by reference reform-buffer
           by value length(reform-buffer)
           by reference date-spec
           by value time-tm
           returning reform-length
           on exception
               display "error calling strftime" upon syserr
       end-call
       if reform-length > 0 and <= length(reform-buffer) then
           display "       " reform-buffer(1 : reform-length)
       else
           display "date format error" upon syserr
       end-if
       .
       end program date-manipulation.

```


{{out}}

```txt

prompt$ cobc -xj date-manipulation.cob
Given: March 7 2009 7:30pm EST
       March 08 2009 08:30AM EDT
       March 08 2009 05:30AM PDT
       March 08 2009 12:30PM GMT
       Sunday, March 08, 2009 PM09:30:00 JST
Given: 2009-03-07T19:30:00-05:00
       2009-03-08/07:30:00-05:00

```



## D


```d

import std.stdio;
import std.format;
import std.datetime;
import std.algorithm;

enum months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];

void main()
{
  // input
  string date = "March 7 2009 7:30pm EST";

  // parsing date string to integer values
  string month, md, tz;
  int day, year, hour, minute;
  date.formattedRead("%s %d %d %d:%d%s %s", &month, &day, &year, &hour, &minute, &md, &tz);
  int mon = cast (int) months.countUntil(month) + 1;

  // convert to 24-hour
  if (md == "pm")
    hour += 12;

  // create date from integer
  DateTime dt = DateTime(year, mon, day, hour, minute);

  // output
  writeln(dt);
  writeln(dt + 12.hours);

}


```

{{out}}

```txt

2009-Mar-07 19:30:00
2009-Mar-08 07:30:00

```



## Delphi


```Delphi

program DateManipulation;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  DateUtils;

function MonthNumber(aMonth: string): Word;
begin
  //Convert a string value representing the month
  //to its corresponding numerical value
  if aMonth = 'January' then Result:= 1
  else if aMonth = 'February' then Result:= 2
  else if aMonth = 'March' then Result:= 3
  else if aMonth = 'April' then Result:= 4
  else if aMonth = 'May' then Result:= 5
  else if aMonth = 'June' then Result:= 6
  else if aMonth = 'July' then Result:= 7
  else if aMonth = 'August' then Result:= 8
  else if aMonth = 'September' then Result:= 9
  else if aMonth = 'October' then Result:= 10
  else if aMonth = 'November' then Result:= 11
  else if aMonth = 'December' then Result:= 12
  else Result:= 12;
end;

function ParseString(aDateTime: string): TDateTime;
var
  strDay,
  strMonth,
  strYear,
  strTime: string;
  iDay,
  iMonth,
  iYear: Word;
  TimePortion: TDateTime;
begin
  //Decode the month from the given string
  strMonth:= Copy(aDateTime, 1, Pos(' ', aDateTime) - 1);
  Delete(aDateTime, 1, Pos(' ', aDateTime));
  iMonth:= MonthNumber(strMonth);

  //Decode the day from the given string
  strDay:= Copy(aDateTime, 1, Pos(' ', aDateTime) - 1);
  Delete(aDateTime, 1, Pos(' ', aDateTime));
  iDay:= StrToIntDef(strDay, 30);

  //Decode the year from the given string
  strYear:= Copy(aDateTime, 1, Pos(' ', aDateTime) -1);
  Delete(aDateTime, 1, Pos(' ', aDateTime));
  iYear:= StrToIntDef(strYear, 1899);

  //Decode the time value from the given string
  strTime:= Copy(aDateTime, 1, Pos(' ', aDateTime) -1);

  //Encode the date value and assign it to result
  Result:= EncodeDate(iYear, iMonth, iDay);

  //Encode the time value and add it to result
  if TryStrToTime(strTime, TimePortion) then
    Result:= Result + TimePortion;
end;

function Add12Hours(aDateTime: string): string;
var
  tmpDateTime: TDateTime;
begin
  //Adding 12 hours to the given
  //date time string value
  tmpDateTime:= ParseString(aDateTime);
  tmpDateTime:= IncHour(tmpDateTime, 12);

  //Formatting the output
  Result:= FormatDateTime('mm/dd/yyyy hh:mm AM/PM', tmpDateTime);
end;

begin
  Writeln(Add12Hours('March 7 2009 7:30pm EST'));
  Readln;
end.

```


{{out}}
 "03/08/2009 07:30 AM"


## EchoLisp


```scheme

(define my-date (string->date "March 7 2009 7:30 pm EST"))
    → Sun Mar 08 2009 01:30:00 GMT+0100 (CET)

(date-add! my-date (* 12 3600))
    → Sun Mar 08 2009 13:30:00 GMT+0100 (CET)
(string->date my-date)

(date->string my-date)
    → "8/3/2009 13:30:00" ;; human localized, Paris time.

```


----

## Erlang

It is human readable to me.

```Erlang

-module( date_manipulation ).

-export( [task/0] ).

task() ->
	{Date_time, TZ} = date_time_tz_from_string( "March 7 2009 7:30pm EST" ),
	Seconds1 = calendar:datetime_to_gregorian_seconds( Date_time ),
	Seconds2 = calendar:datetime_to_gregorian_seconds( {calendar:gregorian_days_to_date(0), {12, 0, 0}} ),
	Date_time_later = calendar:gregorian_seconds_to_datetime( Seconds1 + Seconds2 ),
	{Date_time_later, TZ}.



date_time_tz_from_string( String ) ->
	[Month, Date, Year, Time, TZ] = string:tokens( String, " " ),
	[Hour, Minute] = string:tokens( Time, ":" ),
	{{date_from_strings(Year, Month, Date), time_from_strings(Hour, Minute)}, TZ}.

date_from_strings( Year, Month, Date ) ->
	{erlang:list_to_integer(Year), date_from_strings_month(Month), erlang:list_to_integer(Date)}.

date_from_strings_month( "January" ) -> 1;
date_from_strings_month( "February" ) -> 2;
date_from_strings_month( "March" ) -> 3;
date_from_strings_month( "April" ) -> 4;
date_from_strings_month( "May" ) -> 5;
date_from_strings_month( "June" ) -> 6;
date_from_strings_month( "July" ) -> 7;
date_from_strings_month( "August" ) -> 8;
date_from_strings_month( "September" ) -> 9;
date_from_strings_month( "October" ) -> 10;
date_from_strings_month( "November" ) -> 11;
date_from_strings_month( "December" ) -> 12.

time_from_strings( Hour, Minute_12hours ) ->
	{ok, [Minute], AM_PM} = io_lib:fread("~d", Minute_12hours ),
	{time_from_strings_hour( Hour, string:to_lower(AM_PM) ), Minute, 0}.

time_from_strings_hour( Hour, "am" ) -> erlang:list_to_integer( Hour );
time_from_strings_hour( Hour, "pm" ) -> erlang:list_to_integer( Hour ) + 12.

```

{{out}}

```txt

24> date_manipulation:task().
{{{2009,3,8},{7,30,0}},"EST"}

```



## Euphoria


```Euphoria

--Date Manipulation task from Rosetta Code wiki
--User:Lnettnay

include std/datetime.e

datetime dt

dt = new(2009, 3, 7, 19, 30)
dt = add(dt, 12, HOURS)
printf(1, "%s EST\n", {format(dt, "%B %d %Y %I:%M %p")})

```

{{out}}

```txt

March 08 2009 07:30 AM EST

```


=={{header|F_Sharp|F#}}==
The .NET framework does not support parsing of time zone identifiers like "EST". We have to use time zone offsets like "-5".


```fsharp
open System

let main() =
  let est = TimeZoneInfo.FindSystemTimeZoneById("Eastern Standard Time")
  let date = DateTime.Parse("March 7 2009 7:30pm -5" )
  let date_est = TimeZoneInfo.ConvertTime( date, est)
  let date2 = date.AddHours(12.0)
  let date2_est = TimeZoneInfo.ConvertTime( date2, est)
  Console.WriteLine( "Original date in local time : {0}", date )
  Console.WriteLine( "Original date in EST        : {0}", date_est )
  Console.WriteLine( "12 hours later in local time: {0}", date2 )
  Console.WriteLine( "12 hours later in EST       : {0}", date2_est )

main()
```


{{out}} (depends on locale settings):

```txt
Original date in local time : 08.03.2009 01:30:00
Original date in EST        : 07.03.2009 19:30:00
12 hours later in local time: 08.03.2009 13:30:00
12 hours later in EST       : 08.03.2009 07:30:00
```



## Factor


```factor
USING: calendar calendar.english calendar.format calendar.parser
combinators io kernel math math.parser sequences splitting
unicode ;
IN: rosetta-code.date-manipulation

! e.g. "7:30pm" -> 19 30
: parse-hm ( str -- hours minutes )
    ":" split first2 [ digit? ] partition
    [ [ string>number ] bi@ ] dip "pm" = [ [ 12 + ] dip ] when ;

! Parse a date in the format "March 7 2009 7:30pm EST"
: parse-date ( str -- timestamp )
    " " split {
        [ first month-names index 1 + ]
        [ second string>number ]
        [ third string>number -rot ]
        [ fourth parse-hm 0 ]
        [ last parse-rfc822-gmt-offset ]
    } cleave <timestamp> ;

"March 7 2009 7:30pm EST" parse-date dup 12 hours time+
[ timestamp>rfc822 print ] bi@
```

{{out}}

```txt

Sat, 7 Mar 2009 19:30:00 -0500
Sun, 8 Mar 2009 07:30:00 -0500

```



## Fantom


In the expression "d + 12hr", the "12hr" defines an instance of the Duration class, interpreting the duration in nanoseconds.


```fantom

fansh> d := DateTime.fromLocale("March 7 2009 7:30pm EST", "MMMM D YYYY h:mmaa zzz")
fansh> d
2009-03-07T19:30:00-05:00 EST
fansh> d + 12hr
2009-03-08T07:30:00-05:00 EST
fansh> (d+12hr).toTimeZone(TimeZone("London")) // the extra credit!
2009-03-08T12:30:00Z London

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Date_manipulation this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

#include "vbcompat.bi"

Sub split (s As String, sepList As String, result() As String, removeEmpty As Boolean = False)
  If s = "" OrElse sepList = "" Then
     Redim result(0)
     result(0) = s
     Return
  End If
  Dim As Integer i, j, count = 0, empty = 0, length
  Dim As Integer position(Len(s) + 1)
  position(0) = 0

  For i = 0 To len(s) - 1
    For j = 0 to Len(sepList) - 1
      If s[i] = sepList[j] Then
        count += 1
        position(count) = i + 1
      End If
    Next j
  Next i

  Redim result(count)
  If count  = 0 Then
    result(0) = s
    Return
  End If

  position(count + 1) = len(s) + 1

  For i = 1 To count + 1
    length = position(i) - position(i - 1) - 1
    result(i - 1 - empty) = Mid(s, position(i - 1) + 1, length)
    If removeEmpty Andalso CBool(length = 0) Then empty += 1
  Next

  If empty > 0 Then Redim Preserve result(count - empty)
End Sub

Function parseDate(dt As String, zone As String) As Double
  Dim result() As String
  split dt, " ", result(), True
  Dim As Long m, d, y, h, mn
  Dim am As Boolean
  Dim index As Integer
  Select Case Lcase(result(0))
    Case "january"    : m = 1
    Case "february"   : m = 2
    Case "march"      : m = 3
    Case "april"      : m = 4
    Case "may"        : m = 5
    Case "june"       : m = 6
    Case "july"       : m = 7
    Case "august"     : m = 8
    Case "september"  : m = 9
    Case "october"    : m = 10
    Case "november"   : m = 11
    Case "december"   : m = 12
  End Select
  d = ValInt(result(1))
  y = ValInt(result(2))
  result(3) = LCase(result(3))
  am = (Right(result(3), 2) = "am")
  index = Instr(result(3), ":")
  h = ValInt(Left(result(3), index - 1))
  If Not am Then
    h = h + 12
    If h = 24 Then h = 12
  End If
  mn = ValInt(Mid(result(3), index + 1, 2))
  zone = result(4)
  Return DateSerial(y, m, d) + TimeSerial(h, mn, 0)
End Function

Dim zone As String
Dim ds As Double = parseDate("March 7 2009 7:30pm EST", zone)
Print "Original Date/Time  : "; Format(ds, "mmmm d yyyy h:nnam/pm ") + "  " + zone
ds = DateAdd("h", 12, ds)
Print "12 hours later      : "; Format(ds, "mmmm d yyyy h:nnam/pm ") + "  " + zone
' add 5 hours to convert EST to UTC
ds = DateAdd("h", 5, ds)
Print "Equiv to Date/Time  : "; Format(ds, "mmmm d yyyy h:nnam/pm ") + " UTC"
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

Original Date/Time  : March 7 2009 7:30pm   EST
12 hours later      : March 8 2009 7:30am   EST
Equiv to Date/Time  : March 8 2009 12:30pm  UTC

```



## Frink

Frink parses a large number of date/time formats, has robust date/time math,
and automatically converts between timezones.
By default, output times are in the user's defined timezone.

```frink

### MMM dd yyyy h:mma ###
d = parseDate["March 7 2009 7:30pm EST"]
println[d + 12 hours -> Eastern]
println[d + 12 hours -> Switzerland]  // Extra credit

```


{{out}}

```txt

AD 2009-03-08 AM 08:30:00.000 (Sun) Eastern Daylight Time
AD 2009-03-08 PM 01:30:00.000 (Sun) Central European Time

```



## FunL


```funl
import time.{TimeZone, Date, SimpleDateFormat, Hour}

pattern = SimpleDateFormat( 'MMMM d yyyy h:mma zzz' )
date = pattern.parse( 'March 7 2009 7:30pm EST' )
later = Date( date.getTime() + 12 Hour )
println( pattern.format(later) )    // Eastern Daylight Time
pattern.setTimeZone( TimeZone.getTimeZone('America/Los_Angeles') )
println( pattern.format(later) )    // U.S. Pacific Time
```


{{out}}


```txt

March 8 2009 8:30AM EDT
March 8 2009 5:30AM PDT

```



## Go


```go
package main

import (
    "fmt"
    "time"
)

const taskDate = "March 7 2009 7:30pm EST"
const taskFormat = "January 2 2006 3:04pm MST"

func main() {
    if etz, err := time.LoadLocation("US/Eastern"); err == nil {
        time.Local = etz
    }
    fmt.Println("Input:             ", taskDate)
    t, err := time.Parse(taskFormat, taskDate)
    if err != nil {
        fmt.Println(err)
        return
    }
    t = t.Add(12 * time.Hour)
    fmt.Println("+12 hrs:           ", t)
    if _, offset := t.Zone(); offset == 0 {
        fmt.Println("No time zone info.")
        return
    }
    atz, err := time.LoadLocation("US/Arizona")
    if err == nil {
        fmt.Println("+12 hrs in Arizona:", t.In(atz))
    }
}
```

{{out}}

```txt

Input:              March 7 2009 7:30pm EST
+12 hrs:            2009-03-08 08:30:00 -0400 EDT
+12 hrs in Arizona: 2009-03-08 05:30:00 -0700 MST

```



## Groovy

Solution:
{{libheader|Joda Time|2.1}}


```groovy
import org.joda.time.*
import java.text.*

def dateString = 'March 7 2009 7:30pm EST'

def sdf = new SimpleDateFormat('MMMM d yyyy h:mma zzz')

DateTime dt = new DateTime(sdf.parse(dateString))

println (dt)
println (dt.plusHours(12))
println (dt.plusHours(12).withZone(DateTimeZone.UTC))
```


{{out}}

```txt
2009-03-07T18:30:00.000-06:00
2009-03-08T07:30:00.000-05:00
2009-03-08T12:30:00.000Z
```



## Haskell



```haskell
import qualified Data.Time.Clock.POSIX as P
import qualified Data.Time.Format as F

-- UTC from EST
main :: IO ()
main = print t2
  where
    t1 =
      F.parseTimeOrError
        True
        F.defaultTimeLocale
        "%B %e %Y %l:%M%P %Z"
        "March 7 2009 7:30pm EST"
    t2 = P.posixSecondsToUTCTime $ 12 * 60 * 60 + P.utcTimeToPOSIXSeconds t1
```

{{Out}}

```txt
2009-03-08 12:30:00 UTC
```



## HicEst


```hicest

   CHARACTER date="March 7 2009 7:30pm EST", am_pm, result*20

   EDIT(Text=date, Parse=cMonth, GetPosition=next)
   month = 1 + EDIT(Text='January,February,March,April,May,June,July,August,September,October,November,December', Right=cMonth, Count=',' )
   READ(Text=date(next:)) day, year, hour, minute, am_pm
   hour = hour + 12*(am_pm == 'p')
   TIME(MOnth=month, Day=day, Year=year, Hour=hour, MInute=minute, TO, Excel=xls_day)
   WRITE(Text=result, Format="UWWW CCYY-MM-DD HH:mm") xls_day + 0.5
                   ! result = "Sun 2009-03-08 07:30"
 END

```


=={{header|Icon}} and {{header|Unicon}}==
This uses the datetime procedures from the Icon Programming Library.  Several supplemental procedures were needed to normalize the date format (as the one used in the task isn't fully compatible with the library), and to better handle time zones (as the library routines don't handle part hour time zones).


```Icon
link datetime

procedure main()
write("input      = ",s := "March 7 2009 7:30pm EST" )
write("+12 hours  = ",SecToTZDateLine(s := TZDateLineToSec(s) + 12*3600,"EST"))
write("           = ",SecToTZDateLine(s,"UTC"))
write("           = ",SecToTZDateLine(s,"NST"))
end

procedure SecToTZDateLine(s,tz)   #: returns dateline + time zone given seconds
return NormalizedDate(SecToDateLine(s+\(_TZdata("table")[\tz|"UTC"]))||" "|| tz)
end

procedure TZDateLineToSec(s)  #: returns seconds given dateline (and time zone)
   return (
      NormalizedDate(s) ? (
         d  := tab(find("am"|"pm")+2),tab(many('\t ,')),
         tz := \_TZdata("table")[tab(0)]
         ),
      DateLineToSec(d) - tz)
end

procedure NormalizedDate(s)  #: returns a consistent dateline
static D,M
initial {
   D := ["Saturday","Sunday","Monday","Tuesday","Wednesday","Thursday","Friday"]
   M := ["January","February","March","April","May","June",
         "July","August","September","October","November","December"]
   }

map(s) ? {                               # parse and build consistent dateline
   ds := 1(x := !D, =map(x)) | ""                                    # Weekday
   ds ||:= 1(", ", tab(many('\t ,')|&pos))
   ds ||:= 1(x := !M, =map(x))                 | fail                # Month
   ds ||:= 1(" ", tab(many('\t ,')|&pos))
   ds ||:= tab(many(&digits))                  | fail                # day
   ds ||:= 1(", ", tab(many('\t ,')))          | fail
   ds ||:= tab(many(&digits))                  | fail                # year
   ds ||:= 1(" ", tab(many('\t ,')))           | fail
   ds ||:= tab(many(&digits))||(=":"||tab(many(&digits))|&null) | fail # time
   ds ||:= 1(" ", tab(many('\t ,')|&pos))
   ds ||:= =("am"|"pm")                        | fail                # halfday
   ds ||:= 1(" ", tab(many('\t ,')|&pos))
   tz := map(=!_TZdata("list"),&lcase,&ucase)
   }

if ds[1] == "," then
   ds := SecToDateLine(DateLineToSec("Sunday"||ds))   # get IPL to fix weekday

return ds ||:=  " " || \tz|"UTC"
end

procedure _TZdata(x)    #: internal return TZ data (demo version incomplete)
static TZ,AZ
initial {
   TZ := table()
   AZ := []
   "UTC/0;ACDT/+10.5;CET/1;EST/-5;NPT/+5.75;NST/-3.5;PST/-8;" ?
      while (  a := tab(find("/")), move(1), o := tab(find(";")), move(1) ) do {
         TZ[map(a)] := TZ[a] := integer(3600*o)
         put(AZ,a,map(a))
         }
      every TZ[&null|""] := TZ["UTC"]
   }
return case x of { "list"  : AZ ; "table" : TZ }
end
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/datetime.icn datetime provides SecToDateLine, and DateLineToSec] these convert between Icon's &dateline format and seconds from a configurable base date (which defaults to the normal 1970 epoch).

{{out}}

```txt
input      = March 7 2009 7:30pm EST
+12 hours  = Sunday, March 8, 2009 7:30 am  EST
           = Sunday, March 8, 2009 12:30 pm  UTC
           = Sunday, March 8, 2009 9:00 am  NST
```



## J


A natural mechanism for representing dates in J is what J's documentation refers to as a "timestamp" -- a list of six numbers in ISO 8601 order (year, month, date, hour, minute, second). An alternate representation uses a single number specifying the number of milliseconds since January 1, 1800.

With that in mind:


```J
require'dates'
months=: <;._2 tolower 0 :0
January
February
March
April
May
June
July
August
September
October
November
December
)

numbers=: _".' '"_`(1 I.@:-e.&(":i.10)@])`]}~
words=: [:;:@tolower' '"_`(I.@(tolower = toupper)@])`]}~
getyear=: >./@numbers
getmonth=: 1 + months <./@i. words
getday=: {.@(numbers -. getyear)
gethour=: (2 { numbers) + 12 * (<'pm') e. words
getminsec=: 2 {. 3}. numbers

getts=: getyear, getmonth, getday, gethour, getminsec
timeadd=: 1&tsrep@+&tsrep
deltaT=: (1 tsrep 0)&([ + -@#@[ {. ])
```


This parser assumes that numeric date information appears to the left of time information, that month name is spelled out in full and that time zone may be ignored. (Alternate date representations are straightforward to implement but turn this into a somewhat open-ended problem).

Note that J's tsrep library routine converts from timestamp to milliseconds and 1 tsrep coverts from milliseconds to timestamp.

Example use:


```J
   (deltaT 12 0 0) timeadd getts 'March 7 2009 7:30pm EST'
2009 3 8 7 30 0
   timestamp (deltaT 12 0 0) timeadd getts 'March 7 2009 7:30pm EST'
08 Mar 2009 07:30:00
   isotimestamp (deltaT 12 0 0) timeadd getts 'March 7 2009 7:30pm EST'
2009-03-08 07:30:00.000
```


Note that the isotimestamp representation uses a space instead of a 'T' to separate date and time.


## Java


```Java
import java.util.Date;
import java.text.SimpleDateFormat;
public class DateManip{
    public static void main(String[] args) throws Exception{
	String dateStr = "March 7 2009 7:30pm EST";

	SimpleDateFormat sdf = new SimpleDateFormat("MMMM d yyyy h:mma zzz");

	Date date = sdf.parse(dateStr);

	date.setTime(date.getTime() + 43200000l);

	System.out.println(sdf.format(date));
    }

}
```

{{out}}

```txt
March 8 2009 8:30AM EDT
```

or using <tt>System.out.println(date);</tt> as the last line:

```txt
Sun Mar 08 08:30:00 EDT 2009
```



## JavaScript


Input: March 7 2009 7:30pm EST

The input string is ambiguous since EST might represent
any one of 3 different world time zones.
Will assume US Eastern Standard Time of UTC -5 hours.

Javascript date objects are always in the local time zone.
If a date and time is provided in a different time zone, it must be dealt with manually as the date object's time zone offset is read only.
Consequently, there may be issues if daylight saving is observed in one location but not the other.

While ECMA-262 Ed 5 specifies a <code>Date.parse</code> method, it is not widely supported (2011) and parsing of strings other than the format specified are implementation dependent. Since the test string doesn't conform to the standard, it must be manually parsed.


```JavaScript
function add12hours(dateString) {

  // Get the parts of the date string
  var parts = dateString.split(/\s+/),
      date  = parts[1],
      month = parts[0],
      year  = parts[2],
      time  = parts[3];

  var hr    = Number(time.split(':')[0]),
      min   = Number(time.split(':')[1].replace(/\D/g,'')),
      ampm  = time && time.match(/[a-z]+$/i)[0],
      zone  = parts[4].toUpperCase();

  var months = ['January','February','March','April','May','June',
                'July','August','September','October','November','December'];
  var zones  = {'EST': 300, 'AEST': -600}; // Minutes to add to zone time to get UTC

  // Convert month name to number, zero indexed. Return if invalid month
  month = months.indexOf(month);
  if (month === -1) { return; }

  // Add 12 hours as specified. Add another 12 if pm for 24hr time
  hr += (ampm.toLowerCase() === 'pm') ? 24 : 12

  // Create a date object in local zone
  var localTime = new Date(year, month, date);
  localTime.setHours(hr, min, 0, 0);

  // Adjust localTime minutes for the time zones so it is now a local date
  // representing the same moment as the source date plus 12 hours
  localTime.setMinutes(localTime.getMinutes() + zones[zone] - localTime.getTimezoneOffset() );
  return localTime;
}

var inputDateString = 'March 7 2009 7:30pm EST';

console.log(
  'Input: ' + inputDateString + '\n' +
  '+12hrs in local time: ' + add12hours(inputDateString)
 );
```



## jq

{{works with|jq|version with mktime}}

```jq
"March 7 2009 7:30pm EST"
| strptime("%B %d %Y %I:%M%p %Z")
| .[3] += 12
| mktime | strftime("%B %d %Y %I:%M%p %Z")
```


{{out}}

```sh
"March 08 2009 07:30AM EST"
```



## Julia


### without TimeZones library


```julia
using Dates

function main()
    dtstr = "March 7 2009 7:30pm" # Base.Dates doesn't handle "EST"
    cleandtstr = replace(dtstr, r"(am|pm)"i, "")
    dtformat = dateformat"U dd yyyy HH:MM"
    dtime = parse(DateTime, cleandtstr, dtformat) +
        Hour(12 * contains(dtstr, r"pm"i)) # add 12h for the pm
    println(Dates.format(dtime + Hour(12), dtformat))
end

main()

```
{{out}}

```txt
March 08 2009 07:30
```


### With TimeZones.jl


```julia
using Dates, TimeZones

function testdateparse()
    tzabbrev = Dict("EST" => "+0500", "CST" => "+0600", "MST" => "+0700",  "PST" => "+0800")
    dtstr = "March 7 2009 7:30pm EST"
    for (k, v) in tzabbrev
        dtstr = replace(dtstr, k => v)
    end
    dtformat = dateformat"U dd yyyy HH:MMp zzzzz"
    dtime = TimeZones.parse(ZonedDateTime, dtstr, dtformat)
    println(Dates.format(dtime, dtformat))
end

testdateparse()

```
{{out}}

```txt
March 07 2009 07:30AM +05:00
```



## Kotlin


```scala
// version 1.0.6

import java.text.SimpleDateFormat
import java.util.*

fun main(args: Array<String>) {
    val dts  = "March 7 2009 7:30pm EST"
    val sdf  = SimpleDateFormat("MMMM d yyyy h:mma z")
    val dt   = sdf.parse(dts)
    val cal  = GregorianCalendar(TimeZone.getTimeZone("EST"))  // stay with EST
    cal.time = dt
    cal.add(Calendar.HOUR_OF_DAY, 12) // add 12 hours
    val fmt = "%tB %1\$td %1\$tY %1\$tl:%1\$tM%1\$tp %1\$tZ"
    println(fmt.format(cal)) // display new time

    // display time now in Mountain Standard Time which is 2 hours earlier than EST
    cal.timeZone = TimeZone.getTimeZone("MST")
    println(fmt.format(cal))
}
```


{{out}}

```txt

March 08 2009 7:30am EST
March 08 2009 5:30am MST

```



## Lasso


```Lasso
local(date) = date('March 7 2009 7:30PM EST',-format='MMMM d yyyy h:mma z')
#date->add(-hour = 24)
#date->timezone = 'GMT'
```


{{out}}

```txt
March 9 2009 12:30AM GMT
```



## Lingo


```lingo
----------------------------------------
-- Returns string representation of given date object in YYYY-MM-DD hh:mm:ii format
-- @param {date} dateObj
-- @returns {string}
----------------------------------------
on dateToDateTimeString (dateObj)
  str = ""
  s = string(dateObj.year)
  if s.length<4 then put "0000".char[1..4-s.length] before s
  put s after str
  s = string(dateObj.month)
  if s.length<2 then s = "0"&s
  put s after str
  s = string(dateObj.day)
  if s.length<2 then put "0" before s
  put s after str
  sec = dateObj.seconds
  s = string(sec / 3600)
  sec = sec mod 3600
  if s.length<2 then put "0" before s
  put s after str
  s = string(sec / 60)
  sec = sec mod 60
  if s.length<2 then put "0" before s
  put s after str
  s = string(sec)
  if s.length<2 then put "0" before s
  put s after str
  put ":" after char 12 of str
  put ":" after char 10 of str
  put " " after char 8 of str
  put "-" after char 6 of str
  put "-" after char 4 of str
  return str
end
```



```lingo
dateStr = "March 7 2009 7:30pm EST"

-- parse string
month = (offset(dateStr.word[1].char[1..3], "JanFebMarAprMayJunJulAugSepOctNovDec")-1)/3 + 1
day = integer(dateStr.word[2])
year = integer(dateStr.word[3])
t = dateStr.word[4]
if t.char[t.length-1..t.length]="pm" then dh = 12
else dh = 0
t = t.char[1..t.length-2]
_player.itemDelimiter = ":"
hour = integer(t.item[1])+dh
minute = integer(t.item[2])
tz = dateStr.word[5] -- unused

-- original date as date object
dateObj = date(year,month,day)
dateObj.seconds = hour*3600 + minute*60

-- add 12 hours
sec = dateObj.seconds + 12*3600
newDateObj = dateObj + sec / 86400
newDateObj.seconds = sec mod 86400

-- show as YYYY-MM-DD hh:mm:ii string
put dateToDateTimeString(newDateObj)
-- "2009-03-08 07:30:00"
```



## Lua

The following solution is quite ugly, but unfortunately there is not anything like 'strptime'-function in Lua.

```lua

str = string.lower( "March 7 2009 7:30pm EST" )

month = string.match( str, "%a+" )
if     month == "january"   then month = 1
elseif month == "february"  then month = 2
elseif month == "march"     then month = 3
elseif month == "april"     then month = 4
elseif month == "may"       then month = 5
elseif month == "june"      then month = 6
elseif month == "july"      then month = 7
elseif month == "august"    then month = 8
elseif month == "september" then month = 9
elseif month == "october"   then month = 10
elseif month == "november"  then month = 11
elseif month == "december"  then month = 12
end

strproc = string.gmatch( str, "%d+" )
day  = strproc()
year = strproc()
hour = strproc()
min  = strproc()

if string.find( str, "pm" ) then hour = hour + 12 end

print( os.date( "%c", os.time{ year=year, month=month, day=day, hour=hour, min=min, sec=0 } + 12 * 3600 ) )

```

{{out}}

```txt
Sun Mar  8 07:30:00 2009
```



## Maple


```Maple
twelve_hours := proc(str)
	local dt, zone;
	local months := ["January","February","March","April","May","June","July","August","September","October","November","December"];
	dt := StringTools:-ParseTime("%B %d %Y %l:%M%p", str);
	zone := StringTools:-RegSplit(" ", str)[-1];
	dt := Date(dt:-year, dt:-month, dt:-monthDay, dt:-hour, dt:-minute, timezone = zone);
	dt := dt + 12 * Unit(hours);
	printf("%s %d %d ", months[Month(dt)], DayOfMonth(dt), Year(dt));
	if (HourOfDay(dt) >= 12) then
		printf("%d:%dpm ", HourOfDay(dt)-12, Minute(dt));
	else
		printf("%d:%dam ", HourOfDay(dt), Minute(dt));
	end if;
	printf(TimeZone(dt));
end proc;

```

{{Out|Usage}}
<lang>twelve_hours("March 7 2009 7:30pm EST");
twelve_hours("March 2 2009 0:10am WET");
twelve_hours("March 2 2009 6:30am AST");
```

{{Out|Output}}

```txt

March 8 2009 7:30am EST
March 2 2009 0:10pm WET
March 2 2009 6:30pm AST

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
dstr = "March 7 2009 7:30pm EST";
DateString[DatePlus[dstr, {12, "Hour"}], {"DayName", " ", "MonthName", " ", "Day", " ", "Year", " ", "Hour24", ":", "Minute", "AMPM"}]
```



## mIRC Scripting Language


```mirc
echo -ag $asctime($calc($ctime(March 7 2009 7:30pm EST)+43200))
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

import java.text.SimpleDateFormat
import java.text.ParseException

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method manipulateDate(sampleDate, dateFmt, dHours = 0) private static
  formatter = SimpleDateFormat(dateFmt)
  msHours = dHours * 60 * 60 * 1000 -- hours in milliseconds
  day = formatter.parse(sampleDate)
  day.setTime(day.getTime() + msHours)
  formatted = formatter.format(day)
  return formatted

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  do
    sampleDate = 'March 7 2009 7:30pm EST'
    dateFmt = "MMMM d yyyy h:mma z"
    say sampleDate
    say manipulateDate(sampleDate, dateFmt, 12)
  catch ex = Exception
    ex.printStackTrace()
  end
  return

```

{{out}}

```txt

March 7 2009 7:30pm EST
March 8 2009 8:30AM EDT

```



## Nim


```nim
import posix, times

var ts: Ttm
discard "March 7 2009 7:30pm EST".strptime("%B %d %Y %I:%M%p %Z", ts)
ts.tmHour += 12
echo ts.mktime
```

{{out}}

```txt
Sun Mar  8 07:30:00 2009
```



## ooRexx


### version 1


```ooRexx

  sampleDate = 'March 7 2009 7:30pm EST'

  Parse var sampleDate month day year time zone
  basedate = .DateTime~fromNormalDate(day month~left(3) year)
  basetime = .DateTime~fromCivilTime(time)
  -- this will give us this in a merged format...now we can add in the
  -- timezone informat
  mergedTime = (basedate + basetime~timeofday)~isoDate
  zone = .TimeZoneDataBase~getTimeZone(zone)

  finalTime = .DateTime~fromIsoDate(mergedTime, zone~datetimeOffset)

  say 'Original date:' finalTime~utcIsoDate
  say 'Result after adding 12 hours:' finalTime~addHours(12)~utcIsoDate
  say 'Result shifted to UTC:' finalTime~toTimeZone(0)~utcIsoDate
  say 'Result shifted to Pacific Standard Time:' finalTime~toTimeZone(.TimeZoneDataBase~getTimeZone('PST')~datetimeOffset)~utcIsoDate
  say 'Result shifted to NepalTime Time:' finalTime~toTimeZone(.TimeZoneDataBase~getTimeZone('NPT')~datetimeOffset)~utcIsoDate

-- a descriptor for timezone information
::class timezone
::method init
  expose code name offset altname region
  use strict arg code, name, offset, altname, region
  code~upper

::attribute code GET
::attribute name GET
::attribute offset GET
::attribute altname GET
::attribute region GET
::attribute datetimeOffset GET
  expose offset
  return offset * 60

-- our database of timezones
::class timezonedatabase
-- initialize the class object.  This occurs when the program is first loaded
::method init class
  expose timezones

  timezones = .directory~new
  -- extract the timezone data which is conveniently stored in a method
  data = self~instanceMethod('TIMEZONEDATA')~source

  loop line over data
    -- skip over the comment delimiters, blank lines, and the 'return'
    -- lines that force the comments to be included in the source
    if line = '/*' | line = '*/' | line = '' | line = 'return' then iterate
    parse var line '{' region '}'
    if region \= '' then do
       zregion = region
       iterate
    end
    else do
       parse var line abbrev . '!' fullname '!' altname . '!' offset .
       timezone = .timezone~new(abbrev, fullname, offset, altname, zregion)
       timezones[timezone~code] = timezone
    end
  end

::method getTimezone class
  expose timezones
  use strict arg code
  return timezones[code~upper]

-- this is a dummy method containing the timezone database data.
-- we'll access the source directly and extract the data held in comments
-- the two return statements force the comment lines to be included in the
-- source rather than processed as part of comments between directives
::method timeZoneData class private
return
/*
{Universal}
UTC  ! Coordinated Universal Time          !        !   0

{Europe}
BST  ! British Summer Time                 !        !  +1
CEST ! Central European Summer Time        !        !  +2
CET  ! Central European Time               !        !  +1
EEST ! Eastern European Summer Time        !        !  +3
EET  ! Eastern European Time               !        !  +2
GMT  ! Greenwich Mean Time                 !        !   0
IST  ! Irish Standard Time                 !        !  +1
KUYT ! Kuybyshev Time                      !        !  +4
MSD  ! Moscow Daylight Time                !        !  +4
MSK  ! Moscow Standard Time                !        !  +3
SAMT ! Samara Time                         !        !  +4
WEST ! Western European Summer Time        !        !  +1
WET  ! Western European Time               !        !   0

{North America}
ADT  ! Atlantic Daylight Time              ! HAA    !  -3
AKDT ! Alaska Daylight Time                ! HAY    !  -8
AKST ! Alaska Standard Time                ! HNY    !  -9
AST  ! Atlantic Standard Time              ! HNA    !  -4
CDT  ! Central Daylight Time               ! HAC    !  -5
CST  ! Central Standard Time               ! HNC    !  -6
EDT  ! Eastern Daylight Time               ! HAE    !  -4
EGST ! Eastern Greenland Summer Time       !        !   0
EGT  ! East Greenland Time                 !        !  -1
EST  ! Eastern Standard Time               ! HNE,ET !  -5
HADT ! Hawaii-Aleutian Daylight Time       !        !  -9
HAST ! Hawaii-Aleutian Standard Time       !        ! -10
MDT  ! Mountain Daylight Time              ! HAR    !  -6
MST  ! Mountain Standard Time              ! HNR    !  -7
NDT  ! Newfoundland Daylight Time          ! HAT    !  -2.5
NST  ! Newfoundland Standard Time          ! HNT    !  -3.5
PDT  ! Pacific Daylight Time               ! HAP    !  -7
PMDT ! Pierre & Miquelon Daylight Time     !        !  -2
PMST ! Pierre & Miquelon Standard Time     !        !  -3
PST  ! Pacific Standard Time               ! HNP,PT !  -8
WGST ! Western Greenland Summer Time       !        !  -2
WGT  ! West Greenland Time                 !        !  -3

{India and Indian Ocean}
IST  ! India Standard Time                 !        !  +5.5
PKT  ! Pakistan Standard Time              !        !  +5
BST  ! Bangladesh Standard Time            !        !  +6 -- Note: collision with British Summer Time
NPT  ! Nepal Time                          !        !  +5.75
BTT  ! Bhutan Time                         !        !  +6
BIOT ! British Indian Ocean Territory Time ! IOT    !  +6
MVT  ! Maldives Time                       !        !  +5
CCT  ! Cocos Islands Time                  !        !  +6.5
TFT  ! French Southern and Antarctic Time  !        !  +5
*/
return

```



### version 2

This example is written using the Open Object Rexx dialect to take advantage of the <code>DateTime</code> built&ndash;in class.

```REXX
/* Rexx */
  sampleDate = 'March 7 2009 7:30pm EST'

  Parse value sampleDate with tm td ty tt tz .
  Parse value time('l', tt, 'c') with hh ':' mm ':' ss '.' us .

  timezones. = ''
  Call initTimezones
  mn = monthNameToNumber(tm)
  zuluOffset = getTZOffset(tz)

  Drop !TZ !MSG
  day.1.!TZ = zuluOffset
  day.1.!MSG = 'Original date:'
  day.1 = .DateTime~new(ty, mn, td, hh, mm, ss, us, day.1.!TZ * 60)
  day.2.!TZ = zuluOffset
  day.2.!MSG = 'Result after adding 12 hours to date:'
  day.2 = day.1~addHours(12)
  day.3.!TZ = getTZOffset('UTC') -- AKA GMT == Greenwich Mean Time
  day.3.!MSG = 'Result shifted to "UTC (Zulu)" time zone:'
  day.3 = day.1~toTimeZone(day.3.!TZ)
  day.4.!TZ = getTZOffset('PST') -- Pacific Standard Time
  day.4.!MSG = 'Result shifted to "Pacific Standard Time" time zone:'
  day.4 = day.2~toTimeZone(day.4.!TZ * 60)
  day.5.!TZ = getTZOffset('NPT') -- Nepal Time
  day.5.!MSG = 'Result shifted to "Nepal Time" time zone:'
  day.5 = day.2~toTimeZone(day.5.!TZ * 60)
  day.0 = 5

  Say 'Manipulate the date string "'sampleDate'" and present results in ISO 8601 timestamp format:'
  Say
  Loop d_ = 1 to day.0
    Say day.d_.!MSG
    Say day.d_~isoDate || getUTCOffset(day.d_.!TZ, 'z')
    Say
    End d_

Return 0

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
isTrue: Procedure; Return (1 == 1)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
isFalse: Procedure; Return \isTrue()

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
monthNameToNumber:
Procedure
Do
  Parse arg tm .

  mnamesList = 'January February March April May June July August September October November December'

  Loop mn = 1 to mnamesList~words
    mnx = mnamesList~word(mn)
    If mnx~upper~abbrev(tm~upper, 3) then Do
      Leave mn
      End
    End mn

  Return mn
End
Exit

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
getTZOffset:
Procedure expose timezones.
Do
  Parse upper arg tz .
  Drop !REGION !FULLNAME !OFFSET !ZNAMEALT

  offset = 0
  Loop z_ = 1 to timezones.0
    If tz = timezones.z_ then Do
      offset = timezones.z_.!OFFSET
      Leave z_
      End
    End z_

  Return offset;
End
Exit

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
getUTCOffset:
Procedure expose timezones.
Do
  Parse arg oh ., zulu .

  oha = abs(oh)
  If oha = 0 & 'ZULU'~abbrev(zulu~upper, 1) then Do
    offset = 'Z'
    End
  else Do
    If oh < 0 then ew = '-'
              else ew = '+'
    om = oha * 60
    oom = om // 60 % 1
    ooh = om % 60
    offset = ew || ooh~right(2, 0) || oom~right(2, 0)
    End

  Return offset
End
Exit

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
initTimezones:
Procedure expose timezones.
Do
  -- Read time zone info from formatted comment block below
  Drop !REGION !FULLNAME !OFFSET !ZNAMEALT
  timezones.0 = 0
  region    = ''
  !datBegin = '__DATA__'
  !datEnd   = '__ENDD__'
  !reading  = isFalse()

  Loop l_ = 1 to sourceline()
    Parse value sourceline(l_) with sl 0 hd +8 .
    If !reading then Do
      If hd = !datEnd then Do
        !reading = isFalse()
        Leave l_
        End
      else Do
        Parse value sl with sl '--' .
        If sl~strip~length = 0 then Iterate l_
        Parse value sl with,
          0 '{' zRegion '}',
          0 zAbbrev . '!' zFullName '!' zAbbrevOther . '!' zUOffset .
          If zRegion~length \= 0 then Do
            region = zRegion
            Iterate l_
            End
          else Do
            z_ = timezones.0 + 1
            timezones.0 = z_
            timezones.z_ = zAbbrev~strip~upper
            timezones.z_.!FULLNAME = zFullName~strip
            timezones.z_.!OFFSET   = zUOffset~format
            timezones.z_.!ZNAMEALT = zAbbrevOther~strip~upper
            timezones.z_.!REGION   = region
            End
        End
      End
    else Do
      If hd = !datBegin then Do
        !reading = isTrue()
        End
      Iterate l_
      End
    End l_

  Return timezones.0
End
Exit
/*
 A "HERE" document, sort of...
 Everything between the __DATA__ and __ENDD__ delimiters will be read into the timezones. stem:

__DATA__
{Universal}
UTC  ! Coordinated Universal Time          !        !   0

{Europe}
BST  ! British Summer Time                 !        !  +1
CEST ! Central European Summer Time        !        !  +2
CET  ! Central European Time               !        !  +1
EEST ! Eastern European Summer Time        !        !  +3
EET  ! Eastern European Time               !        !  +2
GMT  ! Greenwich Mean Time                 !        !   0
IST  ! Irish Standard Time                 !        !  +1
KUYT ! Kuybyshev Time                      !        !  +4
MSD  ! Moscow Daylight Time                !        !  +4
MSK  ! Moscow Standard Time                !        !  +3
SAMT ! Samara Time                         !        !  +4
WEST ! Western European Summer Time        !        !  +1
WET  ! Western European Time               !        !   0

{North America}
ADT  ! Atlantic Daylight Time              ! HAA    !  -3
AKDT ! Alaska Daylight Time                ! HAY    !  -8
AKST ! Alaska Standard Time                ! HNY    !  -9
AST  ! Atlantic Standard Time              ! HNA    !  -4
CDT  ! Central Daylight Time               ! HAC    !  -5
CST  ! Central Standard Time               ! HNC    !  -6
EDT  ! Eastern Daylight Time               ! HAE    !  -4
EGST ! Eastern Greenland Summer Time       !        !   0
EGT  ! East Greenland Time                 !        !  -1
EST  ! Eastern Standard Time               ! HNE,ET !  -5
HADT ! Hawaii-Aleutian Daylight Time       !        !  -9
HAST ! Hawaii-Aleutian Standard Time       !        ! -10
MDT  ! Mountain Daylight Time              ! HAR    !  -6
MST  ! Mountain Standard Time              ! HNR    !  -7
NDT  ! Newfoundland Daylight Time          ! HAT    !  -2.5
NST  ! Newfoundland Standard Time          ! HNT    !  -3.5
PDT  ! Pacific Daylight Time               ! HAP    !  -7
PMDT ! Pierre & Miquelon Daylight Time     !        !  -2
PMST ! Pierre & Miquelon Standard Time     !        !  -3
PST  ! Pacific Standard Time               ! HNP,PT !  -8
WGST ! Western Greenland Summer Time       !        !  -2
WGT  ! West Greenland Time                 !        !  -3

{India and Indian Ocean}
IST  ! India Standard Time                 !        !  +5.5
PKT  ! Pakistan Standard Time              !        !  +5
BST  ! Bangladesh Standard Time            !        !  +6 -- Note: collision with British Summer Time
NPT  ! Nepal Time                          !        !  +5.75
BTT  ! Bhutan Time                         !        !  +6
BIOT ! British Indian Ocean Territory Time ! IOT    !  +6
MVT  ! Maldives Time                       !        !  +5
CCT  ! Cocos Islands Time                  !        !  +6.5
TFT  ! French Southern and Antarctic Time  !        !  +5

__ENDD__
*/

```

{{out}}

```txt

Manipulate the date string "March 7 2009 7:30pm EST" and present results in ISO 8601 timestamp format:

Original date:
2009-03-07T19:30:00.000000-0500

Result after adding 12 hours to date:
2009-03-08T07:30:00.000000-0500

Result shifted to "UTC (Zulu)" time zone:
2009-03-08T00:30:00.000000Z

Result shifted to "Pacific Standard Time" time zone:
2009-03-08T04:30:00.000000-0800

Result shifted to "Nepal Time" time zone:
2009-03-08T18:15:00.000000+0545

```



## Pascal

See [[Date_manipulation#Delphi | Delphi]]


## Perl

We use Mountain Daylight Time for output.


```perl
use DateTime;
use DateTime::Format::Strptime 'strptime';
use feature 'say';

my $input =  'March 7 2009 7:30pm EST';
$input    =~ s{EST}{America/New_York};

say strptime('%b %d %Y %I:%M%p %O', $input)
        ->add(hours => 12)
        ->set_time_zone('America/Edmonton')
        ->format_cldr('MMMM d yyyy h:mma zzz');
```


If we're given an ambiguous timezone like 'EST' for input, we can handle this by changing it to the unambiguous Olson timezone id.  This ensures daylight savings is correctly handled (which is especially tricky here, since March 7/8 is the DST rollover, and times jump ahead skipping an hour)

{{out}}

```txt
March 8 2009 6:30AM MDT
```



## Perl 6

Perl 6 comes with a build-in DateTime type
to support most aspects of standard civic time calculation
that are not dependent on cultural idiosyncracies.

Unfortunately, Perl 6 does not yet have a date parsing module
(mostly due to a reticence to inflict Western cultural imperialism on other cultures...
or maybe just due to laziness), but that just gives us another opportunity to demonstrate the built-in grammar support.


```perl6>my @month = <January February March April May June July August September October November December
;
my %month = flat (@month Z=> ^12), (@month».substr(0,3) Z=> ^12), 'Sept' => 8;

grammar US-DateTime {
    rule TOP { <month> <day>','? <year>','? <time> <tz> }

    token month {
	(\w+)'.'?  { make %month{$0} // die "Bad month name: $0" }
    }

    token day { (\d ** 1..2) { make +$0 } }

    token year { (\d ** 1..4) { make +$0 } }

    token time {
	(\d ** 1..2) ':' (\d ** 2) \h* ( :i <[ap]> \.? m | '' )
	{
	    my $h = $0 % 12;
	    my $m = $1;
	    $h += 12 if $2 and $2.substr(0,1).lc eq 'p';
	    make $h * 60 + $m;
	}
    }

    token tz {  # quick and dirty for this task
        [
        |        EDT  { make -4 }
        | [ EST| CDT] { make -5 }
        | [ CST| MDT] { make -6 }
        | [ MST| PDT] { make -7 }
        | [ PST|AKDT] { make -8 }
        | [AKST|HADT] { make -9 }
        |  HAST
        ]
    }
}

$/ = US-DateTime.parse('March 7 2009 7:30pm EST') or die "Can't parse date";

my $year     = $<year>.ast;
my $month    = $<month>.ast;
my $day      = $<day>.ast;
my $hour     = $<time>.ast div 60;
my $minute   = $<time>.ast mod 60;
my $timezone = $<tz>.ast * 3600;

my $dt = DateTime.new(:$year, :$month, :$day, :$hour, :$minute, :$timezone).in-timezone(0);

$dt = $dt.later(hours => 12);

say "12 hours later, GMT: $dt";
say "12 hours later, PST: $dt.in-timezone(-8 * 3600)";
```

{{out}}

```txt
12 hours later, GMT: 2009-02-08T12:30:00Z
12 hours later, PST: 2009-02-08T04:30:00-0800
```



## Phix


```Phix
include builtins\timedate.e
set_timedate_formats({"Mmmm d yyyy h:mmpm tz"})
timedate td = parse_date_string("March 7 2009 7:30pm EST")
atom twelvehours = timedelta(hours:=12)
td = adjust_timedate(td,twelvehours)
?format_timedate(td)
td = change_timezone(td,"ACDT")  -- extra credit
?format_timedate(td)
td = adjust_timedate(td,timedelta(days:=31*4))
?format_timedate(td)
```

{{out}}

```txt

"March 8 2009 7:30am EST"
"March 8 2009 11:00pm ACDT"
"July 10 2009 10:00pm ACST"

```



## PHP


```php
<?php
$time = new DateTime('March 7 2009 7:30pm EST');
$time->modify('+12 hours');
echo $time->format('c');
?>
```



## PicoLisp


```PicoLisp
(de timePlus12 (Str)
   (use (@Mon @Day @Year @Time @Zone)
      (and
         (match
            '(@Mon " " @Day " " @Year " " @Time " " @Zone)
            (chop Str) )
         (setq @Mon (index (pack @Mon) *MonFmt))
         (setq @Day (format @Day))
         (setq @Year (format @Year))
         (setq @Time
            (case (tail 2 @Time)
               (("a" "m") ($tim (head -2 @Time)))
               (("p" "m") (+ `(time 12 0) ($tim (head -2 @Time))))
               (T ($tim @Time)) ) )
         (let? Date (date @Year @Mon @Day)
            (when (>= (inc '@Time `(time 12 0)) 86400)
               (dec '@Time 86400)
               (inc 'Date) )
            (pack (dat$ Date "-") " " (tim$ @Time T) " " @Zone) ) ) ) )
```



## Pike


```Pike>
 (Calendar.dwim_time("March 7 2009 7:30pm EST")+Calendar.Hour()*12)->set_timezone("CET")->format_ext_time();
Result: "Saturday, 7 March 2009 12:30:00"
```



## PL/I


```PL/I
/* The PL/I date functions handle dates and time in 49             */
/* different formats, but not that particular one.  For any of the */
/* standard formats, the following date manipulation will add */
/* 12 hours to the current date/time. */

seconds = SECS(DATETIME());
seconds = seconds + 12*60*60;
put list (SECSTODATE(seconds));
```



## PowerShell

The .NET framework does not support parsing of time zone identifiers like "EST". We have to use time zone offsets like "-5".

```PowerShell
$date = [DateTime]::Parse("March 7 2009 7:30pm -5" )
write-host $date
write-host $date.AddHours(12)
write-host [TimeZoneInfo]::ConvertTimeBySystemTimeZoneId($date.AddHours(12),"Vladivostok Standard Time")
```

{{out}} (depends on user regional settings):

```txt
domingo, 08 de marzo de 2009 1:30:00
domingo, 08 de marzo de 2009 13:30:00
domingo, 08 de marzo de 2009 23:30:00
```



## PureBasic


```PureBasic

EnableExplicit

Procedure.i ToPBDate(Date$, *zone.String)
  Protected year, month, day, hour, minute
  Protected month$, temp$, time$, pm$, zone$
  month$ = StringField(date$, 1, " ")
  day = Val(StringField(date$, 2, " "))
  year = Val(StringField(date$, 3, " "))
  time$ = StringField(date$, 4, " ")
  zone$ = StringField(date$, 5, " ")

  Select month$
    Case "January"   : month = 1
    Case "February"  : month = 2
    Case "March"     : month = 3
    Case "April"     : month = 4
    Case "May"       : month = 5
    Case "June"      : month = 6
    Case "July"      : month = 7
    Case "August"    : month = 8
    Case "September" : month = 9
    Case "October"   : month = 10
    Case "November"  : month = 11
    Case "December"  : month = 12
  EndSelect

  hour = Val(StringField(time$, 1, ":"))
  temp$ = StringField(time$, 2, ":")
  minute = Val(Left(temp$, 2))
  pm$ = Right(temp$, 2)

  If pm$ = "am"
    If hour = 12 : hour = 0 : EndIf
  Else
    If hour <> 12 : hour + 12 : EndIf
  EndIf

  *zone\s = zone$
  ProcedureReturn Date(year, month, day, hour, minute, 0)
EndProcedure

Procedure.s FromPBDate(Date, zone$)
  Protected year$  = Str(Year(Date))
  Protected month = Month(Date)
  Protected day$ = Str(Day(Date))
  Protected hour = Hour(Date)
  Protected minute = Minute(Date)
  Protected month$, time$, pm$, result$

  Select month
    Case 1  :  month$ = "January"
    Case 2  :  month$ = "February"
    Case 3  :  month$ = "March"
    Case 4  :  month$ = "April"
    Case 5  :  month$ = "May"
    Case 6  :  month$ = "June"
    Case 7  :  month$ = "July"
    Case 8  :  month$ = "August"
    Case 9  :  month$ = "September"
    Case 10 :  month$ = "October"
    Case 11 :  month$ = "November"
    Case 12 :  month$ = "December"
  EndSelect

  If hour > 12
    hour - 12
    pm$ = "pm"
  ElseIf hour = 12
    pm$ = "pm"
  Else
    If hour = 0 : hour = 12 : EndIf
    pm$ = "am"
  EndIf

  time$ = Str(hour) + ":" + RSet(Str(minute), 2, "0") + pm$
  result$ = month$ + " " + day$ + " " + year$ + " " + time$ + " " + zone$
  ProcedureReturn result$
EndProcedure

Define date
Define date1$, date2$
Define zone.String

If OpenConsole()
  date1$ = "March 7 2009 7:30pm EST"
  PrintN("Starting date/time : " + date1$)
  date = ToPBDate(date1$, @zone)
  date = AddDate(date, #PB_Date_Hour, 12); add 12 hours
  date2$ = FromPBDate(date, zone\s)
  PrintN("12 hours later     : " + date2$)
  date = AddDate(date, #PB_Date_Hour, 5); adjust to GMT
  date2$ = FromPBDate(date, "GMT")
  PrintN("Or in GMT timezone : " + date2$)
  PrintN("")
  PrintN("Press any key to close the console")
  Repeat: Delay(10) : Until Inkey() <> ""
  CloseConsole()
EndIf

```


{{out}}

```txt

Starting date/time : March 7 2009 7:30pm EST
12 hours later     : March 8 2009 7:30am EST
Or in GMT timezone : March 8 2009 12:30pm GMT

```



## Python

I don't do anything with timezone here, but it is possible.


```python
import datetime

def mt():
	datime1="March 7 2009 7:30pm EST"
	formatting = "%B %d %Y %I:%M%p "
	datime2 = datime1[:-3]  # format can't handle "EST" for some reason
	tdelta = datetime.timedelta(hours=12)		# twelve hours..
	s3 = datetime.datetime.strptime(datime2, formatting)
	datime2 = s3+tdelta
	print datime2.strftime("%B %d %Y %I:%M%p %Z") + datime1[-3:]

mt()
```



## R


```R
time <- strptime("March 7 2009 7:30pm EST", "%B %d %Y %I:%M%p %Z") # "2009-03-07 19:30:00"
isotime <- ISOdatetime(1900 + time$year, time$mon, time$mday,
   time$hour, time$min, time$sec, "EST")                           # "2009-02-07 19:30:00 EST"
twelvehourslater <- isotime + 12 * 60 * 60                         # "2009-02-08 07:30:00 EST"
timeincentraleurope <- format(isotime, tz="CET", usetz=TRUE)       #"2009-02-08 01:30:00 CET"
```



## Racket

The solution below ignores the time zone.

```racket

#lang racket
(require srfi/19)

(define 12hours (make-time time-duration 0 (* 12 60 60)))

(define (string->time s)
  (define t (date->time-utc (string->date s "~B~e~Y~H~M")))
  (if (regexp-match "pm" s)
      (add-duration t 12hours)
      t))

(date->string
 (time-utc->date
  (add-duration
   (string->time "March 7 2009 7:30pm est" )
   12hours))
 "~a ~d ~b ~Y ~H:~M")

```

{{out}}

```racket

"Sun 08 Mar 2009 07:30"

```



## REBOL


```REBOL
REBOL [
	Title: "Date Manipulation"
	URL: http://rosettacode.org/wiki/Date_Manipulation
]

; Only North American zones here -- feel free to extend for your area.

zones: [
	NST -3:30 NDT -2:30 AST -4:00 ADT -3:00 EST -5:00 EDT -4:00
	CST -6:00 CDT -5:00 MST -7:00 MDT -6:00 PST -8:00 PDT -7:00 AKST -9:00
	AKDT -8:00 HAST -10:00 HADT -9:00]

read-time: func [
	text
	/local m d y t z
][
	parse load text [
		set m word! (m: index? find system/locale/months to-string m)
		set d integer!  set y integer!
		set t time!  set tz word!]
	to-date reduce [y m d t  zones/:tz]
]

print 12:00 + read-time "March 7 2009 7:30pm EST"

```


{{out}}

```txt
8-Mar-2009/7:30-5:00
```



## Red


```Red

d: 07-Mar-2009/19:30 + 12:00
print d
8-Mar-2009/7:30:00
d/timezone: 1
print d
8-Mar-2009/8:30:00+01:00
```



## REXX

This version only works with REXXes that support the   '''date'''   and   '''time'''   extended functions.

```REXX
/*REXX program adds 12 hours to a  given date and time, displaying the before and after.*/
 aDate = 'March 7 2009 7:30pm EST'               /*the original or base date to be used.*/

parse var aDate mon dd yyyy hhmm tz .            /*obtain the various parts and pieces. */

  mins = time('M', hhmm, "C")                    /*get the number minutes past midnight.*/
  mins = mins + (12*60)                          /*add  twelve hours  to the  timestamp.*/
 nMins = mins // 1440                            /*compute number min into same/next day*/
  days = mins %  1440                            /*compute number of days added to dats.*/
aBdays = date('B', dd left(mon,3) yyyy)          /*number of base days since REXX epoch.*/
nBdays = aBdays + days                           /*now,  add the number of days added.  */
 nDate = date(,nBdays, 'B')                      /*calculate the new  date  (maybe).    */
 nTime = time('C', nMins, "M")                   /*    "      "   "   time     "        */

say aDate ' +  12 hours  ───► '  ndate ntime tz  /*display the new timestamp to console.*/
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''

```txt

March 7 2009 7:30pm EST  +  12 hours  ───►  8 Mar 2009 7:30am EST

```



## Ring


```ring

# Project : Date manipulation

load "stdlib.ring"
dateorigin = "March 7 2009 7:30pm EST"
monthname = "January February March April May June July August September October November December"
for i = 1 to 12
     if dateorigin[1] = monthname[i]
        monthnum = i
     ok
next
thedate = str2list(substr(dateorigin, " ", nl))
t = thedate[4]
t1 = substr(t,"pm", "")
t2 = substr(t1,":",".")
t3 = number(t2)
if right(t,2) = "pm"
   t3 = t3+ 12
ok
ap = "pm"
d = "07/03/2009"
if t3 + 12 > 24
  d = adddays("07/03/2009",1)
  ap = "am"
ok
see "Original - " + dateorigin + nl
see "Manipulated - " + d + " " + t1 + ap + nl

```

Output:

```txt

Original - March 7 2009 7:30pm EST
Manipulated - 08/03/2009 7:30am

```



## Ruby


### Time class

The <code>Time</code> package in the standard library adds a <code>parse</code> method to the core <code>Time</code> class.

{{libheader|ActiveSupport}}

```ruby
require 'time'
d = "March 7 2009 7:30pm EST"
t = Time.parse(d)
puts t.rfc2822
puts t.zone

new = t + 12*3600
puts new.rfc2822
puts new.zone

# another timezone
require 'rubygems'
require 'active_support'
zone = ActiveSupport::TimeZone['Beijing']
remote = zone.at(new)
# or, remote = new.in_time_zone('Beijing')
puts remote.rfc2822
puts remote.zone
```

{{out}}

```txt
Sat, 07 Mar 2009 19:30:00 -0500
EST
Sun, 08 Mar 2009 08:30:00 -0400
EDT
Sun, 08 Mar 2009 20:30:00 +0800
CST
```


Using [[:Category:ActiveSupport|ActiveSupport]], we can add 12 hours with any of:

```ruby
new = t + 12.hours
new = t.in(12.hours)
new = t.advance(:hours => 12)
```



### DateTime class


```Ruby
require "date"

puts d1 = DateTime.parse("March 7 2009 7:30pm EST")
# d1 + 1 would add a day, so add half a day:
puts d2 = d1 + 1/2r # 1/2r is a rational; 0.5 would also work
puts d3 = d2.new_offset('+09:00')
```

{{out}}

```txt

2009-03-07T19:30:00-05:00
2009-03-08T07:30:00-05:00
2009-03-08T21:30:00+09:00

```



## Run BASIC


```runbasic
theDate$ = "March 7 2009 7:30pm EST"

monthName$ = "January February March April May June July August September October November December"
for i = 1 to 12
  if word$(theDate$,1) = word$(monthName$,i) then monthNum = i			' turn month name to number
next i
d 	= val(date$(monthNum;"/";word$(theDate$,2);"/";word$(theDate$,3)))	' days since Jan 1 1901
t$	= word$(theDate$,4)				' get time from theDate$
t1$	= word$(t$,1,"pm")				' strip pm
t2$	= word$(t1$,1,":") + "." + word$(t1$,2,":")	' replace : with .
t	= val(t2$)
if right$(t$,2) = "pm" then t = t + 12
ap$	= "pm"
if t + 12 > 24 then
  d	= d + 1			' if over 24 hours add 1 to days since 1/1/1901
  ap$	= "am"
end if
print date$(d);" ";t1$;ap$
```

```txt
03/08/2009 7:30am
```



## Scala


```scala
import java.text.SimpleDateFormat
import java.util.{Calendar, Locale, TimeZone}

object DateManipulation {
  def main(args: Array[String]): Unit = {
    val input="March 7 2009 7:30pm EST"
    val df=new SimpleDateFormat("MMMM d yyyy h:mma z", Locale.ENGLISH)
    val c=Calendar.getInstance()
    c.setTime(df.parse(input))

    c.add(Calendar.HOUR_OF_DAY, 12)
    println(df.format(c.getTime))

    df.setTimeZone(TimeZone.getTimeZone("GMT"))
    println(df.format(c.getTime))
  }
}
```


{{out}}

```txt
March 8 2009 8:30AM EDT
March 8 2009 12:30PM GMT
```



## Seed7

Time zone identifiers like "EST" are ambiguous.
E.g.: "AST" is used to abbreviate both Atlantic Standard Time (UTC-04) and Arab Standard Time (UTC+03).
Therefore parsing of such time zone identifiers is not supported by Seed7.
ISO 8601 defines no time zone designators. Instead ISO 8601 specifies time offsets from UTC.
In the example below EST is replaced with UTC-05.


```seed7
$ include "seed7_05.s7i";
  include "time.s7i";
  include "duration.s7i";

const func time: parseDate (in string: dateStri) is func
  result
    var time: aTime is time.value;
  local
    const array string: monthNames is [] ("January", "February", "March", "April",
        "May", "June", "July", "August", "September", "October", "November", "December");
    var array string: dateParts is 0 times "";
    var integer: month is 0;
    var string: timeStri is "";
  begin
    dateParts := split(dateStri, ' ');
    aTime.year := integer parse (dateParts[3]);
    aTime.month := 1;
    while monthNames[aTime.month] <> dateParts[1] do
      incr(aTime.month);
    end while;
    aTime.day := integer parse (dateParts[2]);
    timeStri := dateParts[4];
    if endsWith(timeStri, "am") then
      aTime.hour := integer parse (timeStri[.. pred(pos(timeStri, ':'))]);
    elsif endsWith(timeStri, "pm") then
      aTime.hour := integer parse (timeStri[.. pred(pos(timeStri, ':'))]) + 12;
    else
      raise RANGE_ERROR;
    end if;
    aTime.minute := integer parse (timeStri[succ(pos(timeStri, ':')) .. length(timeStri) - 2]);
    if dateParts[5] <> "UTC" then
      aTime.timeZone := 60 * integer parse (dateParts[5][4 ..]);
    end if;
  end func;

const proc: main is func
  local
    var time: aTime is time.value;
  begin
    aTime := parseDate("March 7 2009 7:30pm UTC-05");
    writeln("Given:         " <& aTime);
    aTime +:= 1 . DAYS;
    writeln("A day later:   " <& aTime);
    aTime := toUTC(aTime);
    writeln("In UTC:        " <& aTime);
  end func;
```


{{out}}

```txt

Given:         2009-03-07 19:30:00 UTC-5
A day later:   2009-03-08 19:30:00 UTC-5
In UTC:        2009-03-09 00:30:00 UTC

```



## Sidef

{{trans|Perl}}

```ruby
var dt = frequire('DateTime::Format::Strptime')

var input =  'March 7 2009 7:30pm EST'
input.sub!('EST', 'America/New_York')

say dt.strptime('%b %d %Y %I:%M%p %O', input)   \
      .add(hours => 12)                         \
      .set_time_zone('America/Edmonton')        \
      .format_cldr('MMMM d yyyy h:mma zzz')
```

{{out}}

```txt
March 8 2009 6:30AM MDT
```



## Smalltalk

{{works with|GNU Smalltalk}}

The aim of the class DateTimeTZ is to provide the ability to ''understand'' time with "meridian" (PM/AM, even though no checks are done to assure coherency of the format) and to handle timezones despite the locale (which anyway is gently "ignored", or rather unknown in the format of letters, to Date), providing a proper set of informations to the method <code>readFromWithMeridian:andTimeZone:</code>.

The <code>aDict</code> argument must be a dictionary where keys are the abbreviated timezone code (e.g. EST), and values are three-elements array: difference between the timezone and GMT (as Duration), the DateTime when there's passage between using or not using the daylight saving time (year is ignored), and the "direction" (as Duration) of the change. All data must be filled by hand... As example I've put EST (and there's no way to represent the "new" date and time correctly with the new EDT timezone).

The code also fails when adding a duration that "jumps" beyond two DST changes (e.g from EST to EDT and EST again); (it could be partially fixed by considering intervals instead of single date, and adding a fourth element to link to the "new" timezone abbreviation)


```smalltalk
DateTime extend [
  setYear: aNum [ year := aNum ]
].

Object subclass: DateTimeTZ [
  |dateAndTime timeZoneDST timeZoneName timeZoneVar|

  DateTimeTZ class >> new [ ^(super basicNew) ]

  DateTimeTZ class >> readFromWithMeridian: aStream andTimeZone: aDict [
    |me|
    me := self new.
    ^ me initWithMeridian: aStream andTimeZone: aDict
  ]

  initWithMeridian: aStream andTimeZone: aDict [ |s|
    dateAndTime := DateTime readFrom: aStream copy.
    s := aStream collection asString.
    s =~ '[pP][mM]'
      ifMatched: [ :m |
        dateAndTime := dateAndTime + (Duration days: 0 hours: 12 minutes: 0 seconds: 0)
      ].
    aDict keysAndValuesDo: [ :k :v |
      s =~ k
        ifMatched: [ :x |
          dateAndTime := dateAndTime setOffset: (v at: 1).
	  timeZoneDST := (v at: 2) setOffset: (v at: 1).
	  timeZoneVar := (v at: 3).
	  timeZoneDST setYear: (self year). "ignore the year"
          timeZoneName := k
        ]
    ].
    ^ self
  ]

  setYear: aNum [ dateAndTime setYear: aNum ]
  year [ ^ dateAndTime year ]

  timeZoneName [ ^timeZoneName ]

  + aDuration [ |n|
    n := dateAndTime + aDuration.
    (n > timeZoneDST) ifTrue: [ n := n + timeZoneVar ].
    ^ (self copy dateTime: n)
  ]

  dateTime [ ^dateAndTime ]
  dateTime: aDT [ dateAndTime := aDT ]

].
```


Usage example (note: the code is rather rigid, so not all operations possible on DateTime are possible on DateTimeTZ).


```smalltalk
|s abbrDict dt|

s := 'March 7 2009 7:30pm EST'.

"Build a abbreviation -> offset for timezones (example)"
abbrDict := Dictionary new.

abbrDict at: 'EST'
         put: { (Duration days: 0 hours: -5 minutes: 0 seconds: 0).
                (DateTime year: 2009 month: 3 day: 8 hour: 2 minute: 0 second: 0).
		(Duration days: 0 hours: 1 minutes: 0 seconds: 0) }.

dt := DateTimeTZ readFromWithMeridian: (s readStream) andTimeZone: abbrDict.

dt := dt + (Duration days: 0 hours: 12 minutes: 0 seconds: 0).

"let's print it"
('%1 %2 %3 %4:%5%6 %7' %
{
  (dt dateTime) monthName asString.
  (dt dateTime) day.
  (dt dateTime) year.
  (dt dateTime) hour12.
  (dt dateTime) minute.
  (dt dateTime) meridianAbbreviation asString.
  dt timeZoneName.
}) displayNl.

(dt dateTime) asUTC displayNl.
```


{{out}} (note that EST should be EDT):

```txt
March 8 2009 8:30AM EST
 2009-03-08T13:30:00+00:00
```



## SQL

{{works with|Oracle}}

```sql

-- March 7 2009 7:30pm EST

select
TO_TIMESTAMP_TZ(
'March 7 2009 7:30pm EST',
'MONTH DD YYYY HH:MIAM TZR'
)
at time zone 'US/Eastern' orig_dt_time
from dual;

-- 12 hours later DST change

select
(TO_TIMESTAMP_TZ(
'March 7 2009 7:30pm EST',
'MONTH DD YYYY HH:MIAM TZR'
)+
INTERVAL '12' HOUR)
at time zone 'US/Eastern' plus_12_dst
from dual;

-- 12 hours later no DST change
-- Arizona time, always MST

select
(TO_TIMESTAMP_TZ(
'March 7 2009 7:30pm EST',
'MONTH DD YYYY HH:MIAM TZR'
)+
INTERVAL '12' HOUR)
at time zone 'US/Arizona' plus_12_nodst
from dual;

```



```txt

SQL> SQL> SQL> SQL> SQL>   2    3    4    5    6    7
ORIG_DT_TIME
---------------------------------------------------------------------------
07-MAR-09 07.30.00.000000000 PM US/EASTERN

SQL> SQL> SQL> SQL>   2    3    4    5    6    7    8
PLUS_12_DST
---------------------------------------------------------------------------
08-MAR-09 08.30.00.000000000 AM US/EASTERN

SQL> SQL> SQL> SQL> SQL>   2    3    4    5    6    7    8
PLUS_12_NODST
---------------------------------------------------------------------------
08-MAR-09 05.30.00.000000000 AM US/ARIZONA

```



## Tcl

{{works with|Tcl|8.5}}

```tcl
set date "March 7 2009 7:30pm EST"
set epoch [clock scan $date -format "%B %d %Y %I:%M%p %z"]
set later [clock add $epoch 12 hours]
puts [clock format $later] ;# Sun Mar 08 08:30:00 EDT 2009
puts [clock format $later -timezone :Asia/Shanghai] ;# Sun Mar 08 20:30:00 CST 2009
```


Note the transition into daylight savings time in the interval (in the Eastern timezone).


## UNIX Shell

requires GNU date

```bash
epoch=$(date -d 'March 7 2009 7:30pm EST +12 hours' +%s)
date -d @$epoch
TZ=Asia/Shanghai date -d @$epoch
```


{{out}}

```txt
Sun Mar  8 08:30:00 EDT 2009
Sun Mar  8 20:30:00 CST 2009
```



## zkl

The iso8601 library offers additional Time/Date support but using the built in stuff:

```zkl
var Date=Time.Date;
fcn add12h(dt){
   re:=RegExp(0'|(\w+)\s+(\d+)\s+(\d+)\ +(.+)\s|);
   re.search(dt);
   _,M,D,Y,hms:=re.matched;    //"March","7","2009","7:30pm"
   M=Date.monthNames.index(M); //3
   h,m,s:=Date.parseTime(hms); //19,30,0
   dti:=T(Y,M,D, h,m,s).apply("toInt");
   Y,M,D, h,m,s=Date.addHMS(dti,12);
   "%s %d %d %s".fmt(Date.monthNames[M],D,Y,Date.toAMPMString(h,m));
}
```


```zkl
add12h("March 7 2009 7:30pm EST").println();
```

{{out}}

```txt
March 8 2009 07:30AM
```

