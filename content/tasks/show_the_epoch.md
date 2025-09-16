+++
title = "Show the epoch"
description = ""
date = 2018-03-24T01:34:00Z
aliases = []
[extra]
id = 10259
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "abap",
  "ada",
  "awk",
  "bbc_basic",
  "c",
  "clojure",
  "cobol",
  "coffeescript",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dart",
  "delphi",
  "erlang",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "futurebasic",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lasso",
  "limbo",
  "lingo",
  "livecode",
  "lotusscript",
  "lua",
  "mathematica",
  "maxima",
  "netrexx",
  "newlisp",
  "nim",
  "ocaml",
  "oforth",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "powershell",
  "purebasic",
  "python",
  "r",
  "racket",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "seed7",
  "sidef",
  "standard_ml",
  "stata",
  "tcl",
  "tuscript",
  "unix_shell",
  "visual_basic",
  "zkl",
]
+++

## Task

Choose popular date libraries used by your language and show the   [[wp:Epoch_(reference_date)#Computing|epoch]]   those libraries use.

A demonstration is preferable   (e.g. setting the internal representation of the date to 0 ms/ns/etc.,   or another way that will still show the epoch even if it is changed behind the scenes by the implementers),   but text from (with links to) documentation is also acceptable where a demonstration is impossible/impractical.

For consistency's sake, show the date in UTC time where possible.


## Related tasks

*   [[Date format]]





## ABAP


```ABAP
DATA: lv_date TYPE datum.

lv_date = 0.

WRITE: / lv_date.

```

```txt

00.00.0000

```


### Simplified


```ABAP
cl_demo_output=>display( |Result: { CONV datum( 0 ) }| ).

```

```txt

00.00.0000

```



## Ada

In Ada, time is a private type and is implementation defined, for instance, on 64 bit GNAT, time is represented internally as nanoseconds relative to Jan 1, 2150.

However, conversion from unix epoch seconds is also supported and shown below.

```Ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar.Conversions; use Ada.Calendar.Conversions;
procedure ShowEpoch is
   etime : Time := To_Ada_Time (0);
begin
   Put_Line (Image (Date => etime));
end ShowEpoch;
```

```txt
1970-01-01 00:00:00
```



## AWK


```AWK

# syntax: GAWK -f SHOW_THE_EPOCH.AWK
# requires GNU Awk 4.0.1 or later
BEGIN {
    print(strftime("%Y-%m-%d %H:%M:%S",0,1))
    exit(0)
}

```

```txt

1970-01-01 00:00:00

```



## BBC BASIC

```bbcbasic
      INSTALL @lib$+"DATELIB"
      PRINT FN_date$(0, "dd-MMM-yyyy")
```

'''Output:'''

```txt

17-Nov-1858

```



## C


```c
#include <time.h>
#include <stdio.h>

int main() {
    time_t t = 0;
    printf("%s", asctime(gmtime(&t)));
    return 0;
}
```

```txt
Thu Jan  1 00:00:00 1970
```


###  Windows

FileTime, from the Win32 API, uses a different epoch.
```c
#include <windows.h>
#include <stdio.h>
#include <wchar.h>

int
main()
{
	FILETIME ft = {dwLowDateTime: 0, dwHighDateTime: 0};  /* Epoch */
	SYSTEMTIME st;
	wchar_t date[80], time[80];

	/*
	 * Convert FILETIME (which counts 100-nanosecond intervals since
	 * the epoch) to SYSTEMTIME (which has year, month, and so on).
	 *
	 * The time is in UTC, because we never call
	 * SystemTimeToTzSpecificLocalTime() to convert it to local time.
	 */
	FileTimeToSystemTime(&ft, &st);

	/*
	 * Format SYSTEMTIME as a string.
	 */
	if (GetDateFormatW(LOCALE_USER_DEFAULT, DATE_LONGDATE, &st, NULL,
	    date, sizeof date / sizeof date[0]) == 0 ||
	    GetTimeFormatW(LOCALE_USER_DEFAULT, 0, &st, NULL,
	    time, sizeof time / sizeof time[0]) == 0) {
		fwprintf(stderr, L"Error!\n");
		return 1;
	}

	wprintf(L"FileTime epoch is %ls, at %ls (UTC).\n", date, time);
	return 0;
}
```

```txt
FileTime epoch is Monday, January 01, 1601, at 12:00:00 AM (UTC).
```


## C#

```c#
using System;

class Program
{
    static void Main()
    {
        Console.WriteLine(new DateTime());
    }
}
```

```txt
1-1-0001 0:00:00
```



## C++

Doesn't work with MSVC 10 SP1

```cpp
#include <iostream>
#include <chrono>
#include <ctime>
int main()
{
    std::chrono::system_clock::time_point epoch;
    std::time_t t = std::chrono::system_clock::to_time_t(epoch);
    std::cout << std::asctime(std::gmtime(&t)) << '\n';
    return 0;
}
```

```txt
Thu Jan  1 00:00:00 1970
```

```cpp
#include <iostream>
#include <boost/date_time.hpp>
int main()
{
    std::cout << boost::posix_time::ptime( boost::posix_time::min_date_time ) << '\n';
    return 0;
}
```

```txt
1400-Jan-01 00:00:00
```



## Clojure


```clojure
(println (java.util.Date. 0))
```

Output (since Clojure 1.5)
<lang>#inst "1970-01-01T00:00:00.000-00:00"
```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. epoch.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  epoch-date.
           03  year                PIC 9(4).
           03  month               PIC 99.
           03  dday                PIC 99.

       PROCEDURE DIVISION.
           MOVE FUNCTION DATE-OF-INTEGER(1) TO epoch-date

           DISPLAY year "-" month "-" dday

           GOBACK
           .
```


```txt
1601-01-01
```



## CoffeeScript


```coffeescript
console.log new Date(0).toISOString()
```

```txt
Thu, 01 Jan 1970 00:00:00 GMT
```


## Common Lisp


```lisp
(multiple-value-bind (second minute hour day month year) (decode-universal-time 0 0)
 	  (format t "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" year month day hour minute second))
```

```txt
1900-01-01 00:00:00
```



## D

The Date struct of the standard library module "std.datetime" represents a date in the Proleptic Gregorian Calendar ranging from 32,768 B.C. to 32,767 A.D.


## Dart


```dart
main() {
  print(new Date.fromEpoch(0,new TimeZone.utc()));
}
```

```txt
1970-01-01 00:00:00.000Z
```



## Delphi


```Delphi
program ShowEpoch;

{$APPTYPE CONSOLE}

uses SysUtils;

begin
  Writeln(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', 0));
end.
```

```txt
1899-12-30 00:00:00.000
```



## Erlang

Erlang uses 2 3-tuples for time and date manipulation. It is possible to get the current values from the operating system. It is also possible to transform these values to/from gregorian seconds. Those are seconds since the date and time interpreted with the Gregorian calendar extended back to year 0. Perhaps the epoch is the date and time at gregorian seconds 0?


```txt

2> calendar:universal_time().
3> calendar:datetime_to_gregorian_seconds(calendar:universal_time()).
63546278932
4> calendar:gregorian_seconds_to_datetime(63546278932).
11> calendar:gregorian_seconds_to_datetime(0).
```



=={{header|F_Sharp|F#}}==

```fsharp
printfn "%s" ((new System.DateTime()).ToString("u"))
```

```txt
0001-01-01 00:00:00Z
```



## Factor


```factor

IN: USE: calendar calendar.format
IN: 0 micros>timestamp timestamp>ymdhms .
"1970-01-01 00:00:00"

```



## Forth

```forth
include lib/longjday.4th
0 posix>jday .longjday  cr
```

```txt

Thursday, January 1, 1970

```



## Fortran

Fortran offers no standard time-type variables nor library routines whereby a timestamp value is given as say some number of seconds from a particular epoch such as the start of the first of January 1970, etc. Individual systems and individual programmers have their own schemes, for instance counting the first of January 1900 as day one - whereby day zero is a Sunday. F90 introduced a complex subroutine with optional named parameters as well as an eight-element integer array whereby <code>CALL DATE_AND_TIME(IVALS)</code> when invoked returns the computer's current date and time as a four-digit year in IVAL(1), the month of the year in IVAL(2), the day of the month in IVAL(3), the difference in minutes with respect to UTC (ex-GMT) time in IVAL(4), and so on down to milliseconds in IVAL(8). One could then argue that the base epoch, day one for instance, is the first of January year 1 and discuss the proleptic Gregorian calendar.


## FreeBASIC

FreeBASIC's built-in date/time library is based on an object called a 'date serial' which uses an epoch of 0:00 AM on December 30, 1899. This is the same epoch used internally for date/time purposes by COM, Visual Basic, Delphi, Excel, LibreOffice Calc and Google Sheets amongst others.

A DateSerial is a double precision floating point number whose integer part represents the number of days after (or in the case of a negative value, the number of days before) the epoch and the fractional part represents the time on that day. As such, the date/time range covered is virtually unlimited.

Date/time values in FB are always based on the current regional settings and so, if values are needed for other time-zones (or UTC), the appropriate adjustments must be made.

```freebasic
' FB 1.05.0 Win64

#Include "vbcompat.bi"

' The first argument to the Format function is a date serial
' and so the first statement below displays the epoch.

Dim f As String = "mmmm d, yyyy hh:mm:ss"
Print Format( 0 , f)      '' epoch
Print Format( 0.5, f)     '' noon on the same day
Print Format(-0.5, f)     '' noon on the previous day
Print Format(1000000, f)  '' one million days after the epoch
Print Format(-80000, f)   '' eighty thousand days before the epoch
Print
Print "Press any key to quit"
Sleep
```


```txt

December 30, 1899 00:00:00
December 30, 1899 12:00:00
December 29, 1899 12:00:00
November 26, 4637 00:00:00
December 17, 1680 00:00:00

```



## FutureBasic


```futurebasic

include "ConsoleWindow"

print date$
print date$("d MMM yyyy")
print date$("EEE, MMM d, yyyy")
print date$("MMMM d, yyyy ")
print date$("MMMM d, yyyy G")
print "This is day ";date$("D");" of the year"
print
print time$
print time$("hh:mm:ss")
print time$("h:mm a")
print time$("h:mm a zzz")
print
print time$("h:mm a ZZZZ "); date$("MMMM d, yyyy G")

```

Output:

```txt

09/03/16
3 Sep 2016
Sat, Sep 3, 2016
September 3, 2016
September 3, 2016 AD
This is day 247 of the year

23:44:12
11:44:12
11:44 PM
11:44 PM EDT

11:44 PM GMT-04:00 September 3, 2016 AD

```



## Go


```go
package main
import ("fmt"; "time")

func main() {
    fmt.Println(time.Time{})
}
```

This is UNIX format.  The 1 on the end is the full year, not two or four digit year.

```txt

Mon Jan  1 00:00:00 +0000 UTC 1

```



## Groovy

Groovy uses the UNIX epoch.

```groovy
def date = new Date(0)
def format = new java.text.SimpleDateFormat('yyyy-MM-dd\'T\'HH:mm:ss.SSSZ')
format.timeZone = TimeZone.getTimeZone('UTC')
println (format.format(date))
```

```txt
1970-01-01T00:00:00.000+0000
```



## Haskell


### Old time library

The <code>ClockTime</code> type is abstract in Haskell 98, but is defined in GHC.
```haskell
import System.Time

main = putStrLn $ calendarTimeToString $ toUTCTime $ TOD 0 0
```

```txt
Thu Jan  1 00:00:00 UTC 1970
```


### New time library

```haskell
import Data.Time

main = print $ UTCTime (ModifiedJulianDay 0) 0
```

```txt
1858-11-17 00:00:00 UTC
```


=={{header|Icon}} and {{header|Unicon}}==
Date and Time can be accessed via a number of keywords and functions
* The following are available in both Icon and Unicon
** &clock, &date, &dateline, and &time deal with current times and dates
* The following are specific to Unicon
** &now provides the number of seconds since the epoch, Jan 1, 1970 00:00:00
** ctime(integer) takes the number of seconds since the epoch and returns the date and time as a string in the local timezone
** gtime(integer) takes the number of seconds since the epoch and returns the date and time as a string in UTC
** gettimeofday() returns a record with the current time since the epoch in seconds and microseconds
* [http://www.cs.arizona.edu/icon/library/src/procs/datetime.icn datetime routines] use a global variable 'DateBaseYear' which defaults to Jan 1, 1970 00:00:00 but can be set if desired.
* The example below uses only a couple of the datetime procedures

```Unicon
link printf,datetime

procedure main()
  # Unicon
  now := gettimeofday().sec
  if now = &now then printf("&now and gettimeofday().sec are equal\n")
  printf("Now (UTC) %s, (local) %s\n",gtime(now),ctime(now))
  printf("Epoch %s\n",gtime(0))
  # Icon and Unicon
  now := DateToSec(&date) + ClockToSec(&clock)
  printf("Now is also %s and %s\n",SecToDate(now),SecToDateLine(now))
end
```

```txt
&now and gettimeofday().sec are equal
Now (UTC) Tue Aug 09 10:43:23 2011, (local) Tue Aug 09 06:43:23 2011
Epoch Thu Jan 01 00:00:00 1970
Now is also 2011/08/09 and Tuesday, August 9, 2011  6:43 am
```



## J

J does not have an epoch.  J's native representation of date and time is a six element list:  year, month, day, hour, minute, second.  For example:

```j
   6!:0''
2011 8 8 20 25 44.725
```

(August 8, 2011, 8:25:44 pm)

That said, the <code>'dates'</code> library does have an epoch:

```j
   require'dates'
   todate 0
1800 1 1
```



## Java

<code>DateFormat</code> is needed to set the timezone. Printing <code>date</code> alone would show this date in the timezone/locale of the machine that the program is running on. The epoch used in <code>java.util.Date</code> (as well as <code>java.sql.Date</code>, which can be subbed into this example) is actually in GMT, but there isn't a significant difference between that and UTC for lots of applications ([http://download.oracle.com/javase/7/docs/api/java/util/Date.html#getTime() documentation for java.util.Date]).

```java
import java.text.DateFormat;
import java.util.Date;
import java.util.TimeZone;

public class DateTest{
    public static void main(String[] args) {
        Date date = new Date(0);
        DateFormat format = DateFormat.getDateTimeInstance();
        format.setTimeZone(TimeZone.getTimeZone("UTC"));
        System.out.println(format.format(date));
    }
}
```

```txt
Jan 1, 1970 12:00:00 AM
```

On my PC I see

```txt
01.01.1970 00:00:00
```



## JavaScript


```javascript
document.write(new Date(0).toUTCString());
```

```txt
Thu, 01 Jan 1970 00:00:00 GMT
```



## jq


```jq
0 | todate
```

```sh
"1970-01-01T00:00:00Z"
```



## Julia

```julia
using Base.Dates
println("Time zero (the epoch) is $(unix2datetime(0)).")
```


```txt
Time zero (the epoch) is 1970-01-01T00:00:00.
```



## Kotlin

```scala
// version 1.1.2

import java.util.Date
import java.util.TimeZone
import java.text.DateFormat

fun main( args: Array<String>) {
    val epoch = Date(0)
    val format = DateFormat.getDateTimeInstance()
    format.timeZone = TimeZone.getTimeZone("UTC")
    println(format.format(epoch))
}
```


```txt

Jan 1, 1970 12:00:00 AM

```



## Lasso


```Lasso
date(0.00)
date(0)
```


```txt
1969-12-31 19:00:00
1969-12-31 19:00:00
```



## Limbo


```Limbo
implement Epoch;

include "sys.m"; sys: Sys;
include "draw.m";
include "daytime.m"; daytime: Daytime;
	Tm: import daytime;

Epoch: module {
	init: fn(nil: ref Draw->Context, nil: list of string);
};

init(nil: ref Draw->Context, nil: list of string)
{
	sys = load Sys Sys->PATH;
	daytime = load Daytime Daytime->PATH;
	sys->print("%s\n", daytime->text(daytime->gmt(0)));
}
```


Of course, this could also be done by mangling the namespace and forging the current date, locking it to the epoch:


```Limbo
implement Epoch;

include "sys.m"; sys: Sys;
include "draw.m";
include "daytime.m"; daytime: Daytime;
	Tm: import daytime;

Epoch: module {
	init: fn(nil: ref Draw->Context, nil: list of string);
};

init(nil: ref Draw->Context, nil: list of string)
{
	sys = load Sys Sys->PATH;
	daytime = load Daytime Daytime->PATH;

	# Create a file containing a zero:
	fd := sys->open("/tmp/0", Sys->OWRITE);
	if(fd == nil) {
		sys->fprint(sys->fildes(2), "Couldn't open /tmp/0 for writing: %r\n");
		raise "fail:errors";
	}
	sys->fprint(fd, "0");
	fd = nil; # Files with no references are closed immediately.

	# Fork the namespace so as not to disturb the parent
	# process's concept of time:
	sys->pctl(Sys->FORKNS, nil);
	# Bind that file over /dev/time:
	sys->bind("/tmp/0", "/dev/time", Sys->MREPL);

	# Print the "current" date, now the epoch:
	sys->print("%s\n", daytime->text(daytime->gmt(daytime->now())));
}

```


```txt
Thu Jan 01 00:00:00 GMT 1970

```



## Lingo


Lingo's date object is not based on an epoch, but instead on runtime date calculations. A new date object is created by specifying "year, month, day", based on gregorian calendar. In arithmetic context, date objects are casted to "days" (AD), not to seconds or milliseconds (see below).


```Lingo
now = the systemDate
put now
-- date( 2018, 3, 21 )

babylonianDate = date(-1800,1,1)

-- print approx. year difference between "babylonianDate" and now
put (now-babylonianDate)/365.2425
-- 3818.1355
```



## LiveCode

LiveCode uses midnight, January 1, 1970 as the start of the eon

```LiveCode
put 0 into somedate
convert somedate to internet date
put somedate

-- output GMT (localised)
-- Thu, 1 Jan 1970 10:00:00 +1000

```



## LotusScript

Uses LotusScript to calculate difference between current time and epoch start date. This example: a button which prints the result. Of course, change the <code>timeStamp</code> variable to whatever suits your need.


```lotusscript

Sub Click(Source As Button)
  'Create timestamp as of now
  Dim timeStamp As NotesDateTime
  Set timeStamp = New NotesDateTime ( Now )

  'Assign epoch start time to variable
  Dim epochTime As NotesDateTime
  Set epochTime = New NotesDateTime ( "01/01/1970 00:00:00 AM GMT" )  ''' These two commands only to get epoch time.

  'Calculate time difference between both dates
  Dim epochSeconds As Long
  epochSeconds = timeStamp.TimeDifference ( epochTime )

  'Print result
  Print epochSeconds

End Sub

```


Output:

```txt

1445093823

```



## Lua


```Lua
print(os.date("%c", 0))
```

```txt
Thu Jan  1 00:00:00 1970
```



## Mathematica


```Mathematica
DateString[0]
```

->Mon 1 Jan 1900 00:00:00

=={{header|MATLAB}} / {{header|Octave}}==
Matlab and Octave store date/time number in a floating point number counting the days.

```matlab
d = [0,1,2,3.5,-3.5,1000*365,1000*366,now+[-1,0,1]];
for k=1:length(d)
    printf('day %f\t%s\n',d(k),datestr(d(k),0))
    disp(datevec(d(k)))
end;
```

```txt
day 0.000000	31-Dec--001 00:00:00
   -1   12   31    0    0    0
day 1.000000	01-Jan-0000 00:00:00
   0   1   1   0   0   0
day 2.000000	02-Jan-0000 00:00:00
   0   1   2   0   0   0
day 3.500000	03-Jan-0000 12:00:00
    0    1    3   12    0    0
day -3.500000	27-Dec--001 12:00:00
   -1   12   27   12    0    0
day 365000.000000	02-May-0999 00:00:00
   999     5     2     0     0     0
day 366000.000000	27-Jan-1002 00:00:00
   1002      1     27      0      0      0
day 734908.972013	09-Feb-2012 23:19:41
   2012.0000      2.0000      9.0000     23.0000     19.0000     41.9633
day 734909.972013	10-Feb-2012 23:19:41
   2012.0000      2.0000     10.0000     23.0000     19.0000     41.9633
day 734910.972013	11-Feb-2012 23:19:41
   2012.0000      2.0000     11.0000     23.0000     19.0000     41.9633

```


Which is to say, day one is the first of January, year zero - except that there is no year zero: one BC is followed by one AD (or, 1 BCE and 1 CE) and the Gregorian calendar scheme wasn't in use then either.


## Maxima


```maxima
timedate(0);
"1900-01-01 10:00:00+10:00"
```



## NetRexx

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

import java.text.DateFormat

edate = Date(0)
zulu  = DateFormat.getDateTimeInstance()
zulu.setTimeZone(TimeZone.getTimeZone('UTC'))
say zulu.format(edate)
return

```

'''Output:'''

```txt

Jan 1, 1970 12:00:00 AM

```



## NewLISP


```NewLISP
(date 0)
->"Thu Jan 01 01:00:00 1970"
```



## Nim


```nim
import times

echo getGMTime(fromSeconds(0))
```

Output:

```txt
Thu Jan  1 00:00:00 1970
```


=={{header|Objective-C}}==

```objc
#import <Foundation/Foundation.h>


int main(int argc, const char *argv[]) {
  @autoreleasepool {

    NSDate *t = [NSDate dateWithTimeIntervalSinceReferenceDate:0];
    NSDateFormatter *dateFormatter = [[NSDateFormatter alloc] init];
    [dateFormatter setTimeZone:[NSTimeZone timeZoneWithName:@"UTC"]];
    [dateFormatter setDateFormat:@"yyyy-MM-dd HH:mm:ss ZZ"];
    NSLog(@"%@", [dateFormatter stringFromDate:t]);

  }
  return 0;
}
```

```txt
2001-01-01 00:00:00 +0000
```



## OCaml


```ocaml
open Unix

let months = [| "January"; "February"; "March"; "April"; "May"; "June";
      "July"; "August"; "September"; "October"; "November"; "December" |]

let () =
  let t = Unix.gmtime 0.0 in
  Printf.printf "%s %d, %d\n" months.(t.tm_mon) t.tm_mday (1900 + t.tm_year)
```

```txt
$ ocaml unix.cma epoch.ml
January 1, 1970
```



## Oforth



```Oforth
import: date

0 asDateUTC println
```


```txt

1970-01-01 00:00:00,000

```



## PARI/GP

GP has no built-in date or time system.

```parigp
system("date -ur 0")
```


PARI, as usual, has access to the same resources as [[#C|C]].


## Pascal

This works with [[Free_Pascal| Free Pascal]]:

```pascal
Program ShowEpoch;

uses
  SysUtils;

begin
  Writeln(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now));
  Writeln(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', 0));
end.
```

```txt

:> ./SelfDescribingNumber
2011-12-13 00:57:41.378
1899-12-30 00:00:00.000

```



## Perl


```perl
print scalar gmtime 0, "\n";
```

```txt
Thu Jan  1 00:00:00 1970
```



## Perl 6


```perl6
say DateTime.new(0)
```

```txt

1970-01-01T00:00:00Z

```



## Phix

The standard Phix file builtins/datetime.e does not use an epoch, but instead expects absolute values, eg Jan 1st 1970 is {1970,1,1,...}. I suppose the closest we can get is:

```Phix
constant d0 = {0,1,1,0,0,0,1,1}
include builtins\timedate.e
?format_timedate(d0,"YYYY-MM-DD")
?format_timedate(d0,"Dddd, Mmmm d, YYYY")
```


```txt

"0000-01-01"
"Sunday, January 1, 0000"

```

Note that zeroes in DT_MONTH/DT_DAY/DT_DOW/DT_DOY will give it jip.

It only says Sunday because I told it to, plus day_of_week() is meaningless/wrong pre 1752, and blatently broken on 1st Jan 0AD.


## PHP


```php
<?php
echo gmdate('r', 0), "\n";
?>
```

```txt
Thu, 01 Jan 1970 00:00:00 +0000
```



## PicoLisp

The 'date' function in PicoLisp returns a day number, starting first of March of the year zero. Calculated according to the gregorian calendar (despite that that calendar wasn't used in 0 AD yet).

```PicoLisp
: (date 1)
-> (0 3 1)  # Year zero, March 1st
```



## PL/I


```pli
*process source attributes xref;
 epoch: Proc Options(main);
 /*********************************************************************
 * 20.08.2013 Walter Pachl  shows that PL/I uses 15 Oct 1582 as epoch
 * DAYS returns a FIXED BINARY(31,0) value which is the number of days
 * (in Lilian format) corresponding to the date d.
 *********************************************************************/
 Dcl d Char(17);
 Put Edit(datetime(),days(datetime()))
         (Skip,a,f(15));
 d='15821015000000000';
 Put Edit(d         ,days(d))
         (Skip,a,f(15));
 d='15821014000000000';
 Put Edit(d         ,days(d))
         (Skip,a,f(15));
 End;
```

Result:

```txt

20130820072642956         157365
15821015000000000              1
15821014000000000
IBM0512I  ONCODE=2112  X in SECS(X,Y) or DAYS(X,Y) was outside the
          supported range.
   At offset +00000283 in procedure with entry EPOCH

```



## PowerShell

PowerShell uses .NET's <code>DateTime</code> structure and an integer can simply be casted appropriately:

```powershell
[datetime] 0
```

```txt
Monday, January 01, 0001 12:00:00 AM
```



### Three Alternates

<code>Get-Date</code> always returns its '''Kind''' property as Local:

```PowerShell

Get-Date -Year 1 -Month 1 -Day 1 -Hour 0 -Minute 0 -Second 0 -Millisecond 0

```

```txt

Monday, January 01, 0001 12:00:00 AM

```

This approach returns its '''Kind''' property as Unspecified:

```PowerShell

New-Object -TypeName System.DateTime

```

```txt

Monday, January 01, 0001 12:00:00 AM

```

Here you could describe the epoch date's '''Kind''' property as being Utc.
Formatting the output as a list for demonstration:

```PowerShell

New-Object -TypeName System.DateTime -ArgumentList 1, 1, 1, 0, 0, 0, ([DateTimeKind]::Utc) | Format-List

```

```txt

Date        : 1/1/0001 12:00:00 AM
Day         : 1
DayOfWeek   : Monday
DayOfYear   : 1
Hour        : 0
Kind        : Utc
Millisecond : 0
Minute      : 0
Month       : 1
Second      : 0
Ticks       : 0
TimeOfDay   : 00:00:00
Year        : 1
DateTime    : Monday, January 01, 0001 12:00:00 AM

```



## PureBasic


```purebasic
If OpenConsole()
  PrintN(FormatDate("Y = %yyyy  M = %mm  D = %dd, %hh:%ii:%ss", 0))

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

```txt
Y = 1970  M = 01  D = 01, 00:00:00
```



## Python


```python
>>>
 import time
>>> time.asctime(time.gmtime(0))
'Thu Jan  1 00:00:00 1970'
>>>
```



## R


```R>
 epoch <- 0
> class(epoch) <- class(Sys.time())
> format(epoch, "%Y-%m-%d %H:%M:%S %Z")
[1] "1970-01-01 00:00:00 UTC"
```



## Racket


```racket

#lang racket
(require racket/date)
(date->string (seconds->date 0 #f))

```


Output:

```txt

"Thursday, January 1st, 1970"

```



## REXX

The epoch for the REXX language BIF   (<u>B</u>uilt-<u>I</u>n <u>F</u>unction)   '''DATE'''   is:   January 1st, year 1.

```rexx
/*REXX program displays the number of days since the epoch for the DATE function (BIF). */

say '     today is: '  date()                    /*today's is format:     mm MON YYYY   */

days=date('Basedate')                            /*only the first char of option is used*/
say right(days, 40)         " days since the REXX base date of January 1st, year 1"

say ' and today is: '  date(, days, "B")         /*it should still be today (µSec later)*/
                    /*   ↑                ┌───◄─── This BIF (Built-In Function) is only */
                    /*   └─────────◄──────┘        for  newer  versions of  REXX  that  */
                    /*                             support the  2nd and 3rd  arguments. */
```

```txt

     today is:   3 Aug 2012
                                   734717 days since the REXX base date of January 1st, year 1
 and today is:   3 Aug 2012

```



## Ring


```ring

load "guilib.ring"

New qApp {
         win1 = new qMainWindow() {
                setwindowtitle("Using QDateEdit")
                setGeometry(100,100,250,100)
                oDate = new qdateedit(win1) {
                        setGeometry(20,40,220,30)
	                oDate.minimumDate()
                }
                show()
                }
                exec()
                }

```

Output:
[[File:CalmoSoftShowEpoch.jpg]]


## Ruby


```ruby
irb(main):001:0> Time.at(0).utc
=> 1970-01-01 00:00:00 UTC
```



## Rust



```rust
extern crate time;

use time::{at_utc, Timespec};

fn main() {
    let epoch = at_utc(Timespec::new(0, 0));
    println!("{}", epoch.asctime());
}
```

```txt
Thu Jan  1 00:00:00 1970
```



## Run BASIC


```runbasic
eDate$ = date$("01/01/0001")
cDate$ = date$(0) ' 01/01/1901
sDate$ = date$("01/01/1970")
```



## Scala


```scala
import java.util.{Date, TimeZone, Locale}
import java.text.DateFormat

val df=DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.LONG, Locale.ENGLISH)
df.setTimeZone(TimeZone.getTimeZone("UTC"))
println(df.format(new Date(0)))
```

```txt
January 1, 1970 12:00:00 AM UTC
```



## Seed7

The Seed7 library [http://seed7.sourceforge.net/libraries/time.htm time.s7i]
defines the type [http://seed7.sourceforge.net/manual/types.htm#time time],
which describes times and dates. For dates the proleptic Gregorian calendar is used
(which assumes that the Gregorian calendar was even in effect at dates preceding its official introduction).
This convention is used according to ISO 8601, which also defines that positive and
negative years exist and that the year preceding 1 is 0.
Therefore the epoch is the beginning of the year 0.

```seed7
$ include "seed7_05.s7i";
  include "time.s7i";

const proc: main is func
  begin
    writeln(time.value);
  end func;
```

```txt

0000-01-01 00:00:00 UTC

```



## Sidef


```ruby
say Time.new(0).gmtime.ctime;
```

```txt
Thu Jan  1 00:00:00 1970
```



## Standard ML


```sml
- Date.toString (Date.fromTimeUniv Time.zeroTime);
val it = "Thu Jan  1 00:00:00 1970" : string
```



## Stata


```stata
. di %td 0
01jan1960
. di %tc 0
01jan1960 00:00:00
```



## Tcl


```tcl
% clock format 0 -gmt 1
Thu Jan 01 00:00:00 GMT 1970
```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
- epoch
number=1
dayofweeknr=DATE (date,day,month,year,number)
epoch=JOIN(year,"-",month,day)
PRINT "epoch: ", epoch," (daynumber ",number,")"
- today's daynumber
dayofweeknr=DATE (today,day,month,year,number)
date=JOIN (year,"-",month,day)
PRINT "today's date: ", date," (daynumber ", number,")"
```

```txt

epoch: 1-1-1 (daynumber 1)
today's date: 2011-12-14 (daynumber 734487)

```



## UNIX Shell

The nonstandard option <code>date -r</code> takes seconds from the epoch, and prints date and time. See [http://www.openbsd.org/cgi-bin/man.cgi?query=date&apropos=0&sektion=1&manpath=OpenBSD+Current&arch=i386&format=html date(1) manual].
```bash
$ date -ur 0
Thu Jan  1 00:00:00 UTC 1970
```


On systems with GNU date, you can do

```bash

$ TZ=UTC  date --date "$(date +%s) seconds ago"
Thu Jan  1 00:00:00 UTC 1970

```



## Visual Basic


```vb
Sub Main()
    Debug.Print Format(0, "dd mmm yyyy hh:mm")
End Sub
```

 30 Dec 1899 00:00


## zkl

Using the method tickToTock(time_t,useLocalTime) on Linux. tickToTock converts a time_t (seconds since the epoch) to "human" time. False means use UTC (vs local time, the default).

```zkl
zkl: Time.Clock.tickToTock(0,False)
L(1970,1,1,0,0,0) // y,m,d, h,m,s
```




{{omit from|AutoHotkey}} <!-- AHK uses a built-in datetime "library" which uses YYYYMMDDHH24MISS, therefore no epoch. -->
