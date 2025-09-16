+++
title = "System time"
description = ""
date = 2019-08-28T20:14:07Z
aliases = []
[extra]
id = 2482
[taxonomies]
categories = ["task", "Programming environment operations"]
tags = []
languages = [
  "abap",
  "ada",
  "aime",
  "algol_68",
  "applescript",
  "autohotkey",
  "autoit",
  "awk",
  "bacon",
  "basic",
  "batch_file",
  "bbc_basic",
  "c",
  "clojure",
  "cobol",
  "coldfusion",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dcl",
  "delphi",
  "dwscript",
  "e",
  "easylang",
  "elena",
  "elixir",
  "emacs_lisp",
  "erlang",
  "excel",
  "factor",
  "falcon",
  "fantom",
  "forth",
  "fortran",
  "freebasic",
  "gambas",
  "genie",
  "go",
  "groovy",
  "guiss",
  "haskell",
  "hicest",
  "holyc",
  "hoon",
  "idl",
  "io",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "kotlin",
  "lasso",
  "lfe",
  "liberty_basic",
  "lil",
  "lingo",
  "livecode",
  "locomotive_basic",
  "logo",
  "lua",
  "m2000_interpreter",
  "mathematica",
  "maxima",
  "mumps",
  "neko",
  "nemerle",
  "netrexx",
  "newlisp",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "oz",
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
  "q",
  "r",
  "racket",
  "raven",
  "rebol",
  "related_tasks",
  "retro",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "scheme",
  "seed7",
  "setl",
  "sidef",
  "smalltalk",
  "snobol4",
  "spl",
  "sql_pl",
  "standard_ml",
  "stata",
  "swift",
  "tcl",
  "tuscript",
  "unix_shell",
  "ursa",
  "ursala",
  "vala",
  "vba",
  "vbscript",
  "xpl0",
  "yabasic",
  "zkl",
]
+++

## Task

Output the system '''time'''   (any units will do as long as they are noted) either by a [[Execute a System Command|system command]] or one built into the language.

The system time can be used for debugging, network information, random number seeds, or something as simple as program performance.


## Related tasks

*   [[Date format]]


## See also

*   [[wp:System time#Retrieving system time|Retrieving system time (wiki)]]





## ABAP


```aime
REPORT system_time.

WRITE: sy-uzeit.
```



## Ada

The following example displays a date-time stamp. The time zone value is the number of minutes offset from the prime meridian.
```ada
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Text_Io; use Ada.Text_Io;

procedure System_Time is
   Now : Time := Clock;
begin
   Put_line(Image(Date => Now, Time_Zone => -7*60));
end System_Time;
```
 2008-01-23 19:14:19


## Aime


```aime
date d;

d_now(d);

o_form("~-/f2/-/f2/ /f2/:/f2/:/f2/\n", d_year(d), d_y_month(d), d_m_day(d),
       d_d_hour(d), d_h_minute(d), d_m_second(d));
```
 2011-09-04 15:05:08


## ALGOL 68

<!-- {{does not work with|ALGOL 68|Standard - POSIX local time, and utc time extensions to language used}} -->
<!-- {{does not work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386 - test missing transput, and POSIX local time, and utc time extensions to language used}} -->
```algol68
FORMAT time repr = $"year="4d,", month="2d,", day="2d,", hours="2d,",  \
                   minutes="2d,", seconds="2d,", day of week="d,",  \
                   daylight-saving-time flag="dl$;
printf((time repr, local time));
printf((time repr, utc time))
```
 year=2009, month=03, day=12, hours=11, minutes=53, seconds=32, day of week=5, daylight-saving-time flag=0
 year=2009, month=03, day=12, hours=01, minutes=53, seconds=32, day of week=5, daylight-saving-time flag=0


## AppleScript


```AppleScript
display dialog (current date)
```

```txt

 Sunday, August 14, 2011 10:43:57 PM

```



## AutoHotkey


```autohotkey
FormatTime, t
MsgBox,% t
```

```txt

 4:18 PM Saturday, May 30, 2009

```



## AutoIt


```autoit
MsgBox(0,"Time", "Year: "&@YEAR&",Day: " &@MDAY& ",Hours: "& @HOUR & ", Minutes: "& @MIN &", Seconds: "& @SEC)
```

```txt

 Year: 2011,Day: 05,Hour: 09, Minutes: 15, Seconds: 25

```



## AWK

'''One-liner:'''
```awk
$ awk 'BEGIN{print systime(),strftime()}'
```

```txt
1242401632 Fri May 15 17:33:52  2009
```


'''Solution for other awk-versions:'''
```awk

function dos_date(  cmd,d,t,x) { 	# under MS Windows
#   cmd = "DATE /T"
#   cmd | getline d	# Format depends on locale, e.g. MM/DD/YYYY or YYYY-MM-DD
#   close(cmd)        	# close pipe
# ##print d
#   cmd = "TIME /T"
#   cmd | getline t   	# 13:59
#   close(cmd)
# ##print t
#   return d t

    cmd = "echo %DATE% %TIME%"		# this gives better time-resolution
    cmd | getline x   			# 2014-10-31 20:57:36.84
    close(cmd)
    return x
}
BEGIN {
   print "Date and time:", dos_date()
  #print systime(), strftime()		# gawk only
}

```

```txt

2014-10-31 20:57:36.84

```


See also: [[System_time#Batch]], [http://rosettacode.org/wiki/Discordian_date#AWK Discordian_date]


## BaCon


```qbasic
' BaCon time
n = NOW
PRINT n, " seconds since January 1st, 1970"
PRINT YEAR(n), MONTH(n), DAY(n) FORMAT "%04d/%02d/%02d "
PRINT HOUR(n), MINUTE(n), SECOND(n) FORMAT "%02d:%02d:%02d\n"
```

```txt

1503869183 seconds since January 1st, 1970
2017/08/27 17:26:23
```



## BASIC

This shows the system time in seconds since midnight.

```qbasic
PRINT TIMER
```


This shows the time in human-readable format (using a 24-hour clock):

```qbasic
PRINT TIME$
```



## Batch File

There is no way to get a computer-readable date or time representation in batch files.
All output is human-readable and follows the current locale.

Both <tt>date</tt> and <tt>time</tt> have a <tt>/t</tt> argument which makes them output only the value instead of prompting for a new one as well.
```dos
date /t
time /t
```
Furthermore there are two pseudo-variables <tt>%DATE%</tt> and <tt>%TIME%</tt>:
```dos
echo %DATE% %TIME%
```



## BBC BASIC


```bbcbasic
      PRINT TIME$
```
Outputs the date and time.  To output only the time:
```bbcbasic
      PRINT RIGHT$(TIME$,8)
```



## C

This probably isn't the best way to do this, but it works.
It shows system time as "Www Mmm dd hh:mm:ss yyyy", where Www is the weekday, Mmm the month in letters, dd the day of the month, hh:mm:ss the time, and yyyy the year.

```c
#include <time.h>
#include <stdio.h>
#include <stdlib.h>

int main(){
  time_t my_time = time(NULL);
  printf("%s", ctime(&my_time));
  return 0;
}
```



## C++

to be compiled under linux with g++ -lboost_date_time systemtime.cpp -o systemtime( or whatever you like)

```cpp
#include <iostream>
#include <boost/date_time/posix_time/posix_time.hpp>

int main( ) {
   boost::posix_time::ptime t ( boost::posix_time::second_clock::local_time( ) ) ;
   std::cout << to_simple_string( t ) << std::endl ;
   return 0 ;
}
```



### C++ 11


```cpp
#include <chrono>
#include <ctime> //for conversion std::ctime()
#include <iostream>

int main() {
    auto timenow = std::chrono::system_clock::to_time_t(std::chrono::system_clock::now());
    std::cout << std::ctime(&timenow) << std::endl;
}
```

```txt

 Thu Dec 26 13:51:00 2013

```


## C#

```c#
Console.WriteLine(DateTime.Now);
```



## Clojure


```lisp
(import '[java.util Date])
; the current system date time string
(print (new Date))
; the system time as milliseconds since 1970
(print (. (new Date) getTime))
; or
(print (System/currentTimeMillis))
```



## COBOL


```cobol
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-DATE-FIELDS.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR    PIC  9(4).
               10  WS-CURRENT-MONTH   PIC  9(2).
               10  WS-CURRENT-DAY     PIC  9(2).
           05  WS-CURRENT-TIME.
               10  WS-CURRENT-HOUR    PIC  9(2).
               10  WS-CURRENT-MINUTE  PIC  9(2).
               10  WS-CURRENT-SECOND  PIC  9(2).
               10  WS-CURRENT-MS      PIC  9(2).
           05  WS-DIFF-FROM-GMT       PIC S9(4).

       PROCEDURE DIVISION.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
```



## ColdFusion


###  Script Based CFML


```cfm
<cfscript>

  // Date Time
  currentTime = Now();
  writeOutput( currentTime );

  // Epoch
  // Credit for Epoch time should go to Ben Nadel
  // bennadel.com is his blog
  utcDate = dateConvert( "local2utc", currentTime );
  writeOutput( utcDate.getTime() );
</cfscript>
```

```txt

Current DateTime: {ts '2017-06-06 10:36:28'}
Epoch: 1496763388434

```



## Common Lisp


```lisp
(multiple-value-bind (second minute hour day month year) (get-decoded-time)
 	  (format t "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" year month day hour minute second))
```



## D

''Clock.now.span'' in the example below returnes the time-span since 1 Jan 1 A.D.
Days are used in the example,
but lower units are available, with the lowest being nanoseconds (nanos field).

```D
Stdout(Clock.now.span.days / 365).newline;
```


## DCL


```DCL
$ start_time = f$time()
$ wait 0::10
$ end_time = f$time()
$ write sys$output "start time was ", start_time
$ write sys$output "end time was   ", end_time
$ write sys$output "delta time is         ", f$delta_time( start_time, end_time )
```

```txt
$ @system_time
start time was  4-JUN-2015 15:38:09.60
end time was    4-JUN-2015 15:38:19.60
delta time is            0 00:00:10.00
```

The alternative below doesn't facilitate the calculation of the delta time;
```txt
$ show time
   4-JUN-2015 15:33:33
```



## Delphi


```Delphi
lblDateTime.Caption := FormatDateTime('dd mmmm yyyy hh:mm:ss', Now);
```
This populates a label with the date


## DWScript


```delphi
PrintLn(FormatDateTime('dd mmmm yyyy hh:mm:ss', Now));
```



## E


```e
println(timer.now())
```


The value is the number of milliseconds since 1970.


## EasyLang


<lang>secs$ = sys "time"
print sys "time:" & secs$
```



## Elena

ELENA 4.x :

```elena
import extensions;
import system'calendar;

public program()
{
    console.printLine(Date.Now);
}
```

```txt

01.10.2017 18:02:43

```



## Elixir


```elixir
:os.timestamp                   # =>
 {MegaSecs, Secs, MicroSecs}
:erlang.time                    # => {Hour, Minute, Second}
:erlang.date                    # => {Year, Month, Day}
:erlang.localtime               # => {{Year, Month, Day}, {Hour, Minute, Second}}
:erlang.universaltime           # => {{Year, Month, Day}, {Hour, Minute, Second}}

:calendar.local_time            # => {{Year, Month, Day}, {Hour, Minute, Second}}
:calendar.universal_time        # => {{Year, Month, Day}, {Hour, Minute, Second}}
```

It is also guaranteed that subsequent calls to this BIF returns continuously increasing values. Hence, the return value from <code>:erlang.now()</code> can be used to generate unique time-stamps, and if it is called in a tight loop on a fast machine the time of the node can become skewed.

```elixir
:random.seed(:erlang.now)
```



## Emacs Lisp


```Lisp
(message "%s" (current-time-string))
=>
"Wed Oct 14 22:21:05 1987"
```


<code>current-time</code> is the main programmatic interface, with various functions available to format or operate on the time values list it gives.


## Erlang

By default, Erlang timestamps are turned in the {megasecs, secs, microsecs} format.
```erlang
1>
 os:timestamp().
{1250,222584,635452}
```

These can be changed with the calendar module:
```erlang
2>
 calendar:now_to_datetime(os:timestamp()).
3> calendar:now_to_universal_time(os:timestamp()).
4> calendar:now_to_local_time(os:timestamp()).
```
Note that you will often encounter the function <tt>erlang:now/0</tt> giving a time similar to the system time. However, <tt>erlang:now/0</tt> may get delayed if the system time changes suddenly (i.e.: coming back from sleep mode). The delay is in place to make sure <tt>receive</tt> clauses that are millisecond-sensitive do not get false timeouts.


## Excel

NOW() returns the date and time to the minute.
Type in a cell :
```Excel
=NOW()
```

```txt
9-8-2013 17:33
```


=={{header|F_Sharp|F#}}==

```fsharp
printfn "%s" (System.DateTime.Now.ToString("u"))
```

```txt

 2013-09-19 23:56:50Z

```



## Factor


```factor
USE: calendar

now .
```




## Falcon


```falcon

/* Added by Aykayayciti Earl Lamont Montgomery
April 10th, 2018 */

> CurrentTime().toString()

```

```txt

2018-04-10 09:40:35.798
[Finished in 0.2s]

```



## Fantom

<tt>DateTime.now</tt> returns the current time, which can then be formatted into different styles. For example, <tt>toJava</tt> returns the current time in milliseconds since 1 Jan 1970.

<tt>DateTime.nowTicks</tt> returns the number of nanoseconds since 1 Jan 2000 UTC.
```fantom
fansh>
 DateTime.nowTicks
351823905158000000
fansh> DateTime.now
2011-02-24T00:51:47.066Z London
fansh> DateTime.now.toJava
1298508885979
```



## Forth

Forth's only standard access to the system timers is via <tt>DATE&TIME ( -- s m h D M Y )</tt> and <tt>MS ( n -- )</tt> which pauses the program for at least ''n'' milliseconds. Particular Forth implementations give different access to millisecond and microsecond timers:

```forth
[UNDEFINED] MS@ [IF]                                   \ Win32Forth (rolls over daily)
 [DEFINED] ?MS [IF] ( -- ms )
   : ms@ ?MS ;                                         \ iForth
 [ELSE] [DEFINED] cputime [IF] ( -- Dusec )
   : ms@ cputime d+ 1000 um/mod nip ;                  \ gforth: Anton Ertl
 [ELSE] [DEFINED] timer@ [IF] ( -- Dusec )
   : ms@ timer@ >us 1000 um/mod nip ;                  \ bigForth
 [ELSE] [DEFINED] gettimeofday [IF] ( -- usec sec )
   : ms@ gettimeofday 1000 MOD 1000 * SWAP 1000 / + ;  \ PFE
 [ELSE] [DEFINED] counter [IF]
   : ms@ counter ;                                     \ SwiftForth
 [ELSE] [DEFINED] GetTickCount [IF]
   : ms@ GetTickCount ;                                \ VFX Forth
 [ELSE] [DEFINED] MICROSECS [IF]
   : ms@  microsecs 1000 UM/MOD nip ;                  \  MacForth
[THEN] [THEN] [THEN] [THEN] [THEN] [THEN] [THEN]

MS@ .   \ print millisecond counter
```



## Fortran

In ISO Fortran 90 or later, use the SYSTEM_CLOCK intrinsic subroutine:
```fortran
integer :: start, stop, rate
real :: result

! optional 1st integer argument (COUNT) is current raw system clock counter value (not UNIX epoch millis!!)
! optional 2nd integer argument (COUNT_RATE) is clock cycles per second
! optional 3rd integer argument (COUNT_MAX) is maximum clock counter value
call system_clock( start, rate )

result = do_timed_work()

call system_clock( stop )

print *, "elapsed time: ", real(stop - start) / real(rate)
```
In ISO Fortran 95 or later, use the CPU_TIME intrinsic subroutine:
```fortran
real :: start, stop
real :: result

! System clock value interpreted as floating point seconds
call cpu_time( start )

result = do_timed_work()

call cpu_time( stop )

print *, "elapsed time: ", stop - start
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Print Date + " " + Time  '' returns system date/time in format : mm-dd-yyyy hh:mm:ss
Sleep
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=f061cc0cf175496b37bbcd14bd1e7058 Click this link to run this code]'''

```gambas
Public Sub Main()

Print Format(Now, "dddd dd mmmm yyyy hh:nn:ss")

End
```

Output:

```txt

Thursday 08 June 2017 15:58:41

```



## Genie


```genie
[indent=4]

init
    var now = new DateTime.now_local()
    print now.to_string()
```


```txt
prompt$ valac systemTime.gs
prompt$ ./systemTime
2019-06-01T13:07:54-0400
```



## Go


```go
package main

import "time"
import "fmt"

func main() {
    t := time.Now()
    fmt.Println(t)                                    // default format
    fmt.Println(t.Format("Mon Jan  2 15:04:05 2006")) // some custom format
}
```



## Groovy

Solution (based on [[#Java|Java]] solution).
```groovy
def nowMillis = new Date().time
println 'Milliseconds since the start of the UNIX Epoch (Jan 1, 1970) == ' + nowMillis
```

 Milliseconds since the start of the UNIX Epoch (Jan 1, 1970) == 1243395159250


## GUISS


We can only show the clock, but this might not be set to system time:
```guiss
Taskbar>
```



## Haskell


```haskell
import System.Time
       (getClockTime, toCalendarTime, formatCalendarTime)

import System.Locale (defaultTimeLocale)

main :: IO ()
main = do
  ct <- getClockTime
  print ct -- print default format, or
  cal <- toCalendarTime ct
  putStrLn $ formatCalendarTime defaultTimeLocale "%a %b %e %H:%M:%S %Y" cal
```


or with the time library:

```haskell
import Data.Time (getZonedTime, formatTime, defaultTimeLocale)

main :: IO ()
main = do
  zt <- getZonedTime
  print zt -- print default format, or
  putStrLn $ formatTime defaultTimeLocale "%a %b %e %H:%M:%S %Y" zt
```



## HicEst


```HicEst
seconds_since_midnight = TIME() ! msec as fraction

seconds_since_midnight = TIME(Year=yr, Day=day, WeekDay=wday, Gregorian=gday)
                             ! other options e.g. Excel, YYYYMMDD (num or text)
```



## HolyC


```holyc
CDateStruct ds;
Date2Struct(&ds, Now + local_time_offset);

Print("%04d-%02d-%02d %02d:%02d:%02d\n", ds.year, ds.mon, ds.day_of_mon, ds.hour, ds.min, ds.sec);

```



## Hoon

The time of the current system event is made available on the subject in most contexts as <tt>now</tt>.


```Hoon
now>
```
 ~2016.7.24..01.25.02..0f8e

Absolute times have 64-bit subsecond precision, which is used to ensure that no two system events have the same timestamp.


## IDL

The <tt>systime()</tt> function returns system time either as a formatted string (if the argument is zero or absent) or as a double-precision (floating-point) number of seconds (with fractionals) since 1-Jan-1970 otherwise:

  print,systime(0)
     Wed Feb 10 09:41:14 2010
  print,systime(1)
       1.2658237e+009

The default format can also be invoked for a different moment in time by passing a different number of elapsed seconds to the routine as a second argument:

  print,systime(0,1e9)
     Sat Sep 08 17:46:40 2001

Otherwise, the system time can be retrieved in Julian dates:

  print,systime(/julian),format='(F15.7)'
     2455237.9090278

The local time zone is ignored if the <tt>utc</tt> flag is used:

  print,systime()
     Wed Feb 10 09:50:15 2010
  print,systime(/utc)
     Wed Feb 10 17:50:10 2010


## Io


```io
Date now println>
```
 2008-08-26 00:15:52 EDT

=={{header|Icon}} and {{header|Unicon}}==

```Unicon
procedure main()

    write("&time - milliseconds of CPU time = ",&time)
    write("&clock - Time of day as hh:mm:ss (24-hour format) = ",&clock)
    write("&date - current date in yyyy/mm/dd format = ",&date)
    write("&dateline - timestamp with day of the week, date, and current time to the minute = ",&dateline)

    if find("Unicon",&version) then
       write("&now - time in seconds since the epoch = ", &now)  # Unicon only

end
```
 &time - milliseconds of CPU time = 0
&clock - Time of day as hh:mm:ss (24-hour format) = 15:56:14
&date - current date in yyyy/mm/dd format = 2011/06/06
&dateline - timestamp with day of the week, date, and current time to the minute = Monday, June 6, 2011  3:56 pm
&now - time in seconds since the epoch = 1307400974

=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PRINT TIME$
```



## J

The external verb <code>6!:0</code> returns a six-element floating-point array in which the elements correspond to year, month, day, hour, minute, and second. Fractional portion of second is given to thousandths.
```j
   6!:0 ''
2008 1 23 12 52 10.341
```


A formatted string representation of the current time can also be returned:
```j
   6!:0 'YYYY-MM-DD hh:mm:ss.sss'
2009-08-26 10:38:53.171
```



## Java

```java
public class SystemTime{
    public static void main(String[] args){
        System.out.format("%tc%n", System.currentTimeMillis());
    }
}
```
 Mon Jun 21 13:02:19 BST 2010
Or using a <code>Date</code> object:
```java
import java.util.Date;

public class SystemTime{
   public static void main(String[] args){
      Date now = new Date();
      System.out.println(now); // string representation

      System.out.println(now.getTime()); // Unix time (# of milliseconds since Jan 1 1970)
      //System.currentTimeMillis() returns the same value
   }
}
```


Alternately, you can use a <tt>Calendar</tt> object, which allows you to retrieve specific fields of the date.


## JavaScript



```javascript
console.log(new Date()) // => Sat, 28 May 2011 08:22:53 GMT
console.log(Date.now()) // => 1306571005417 // Unix epoch
```



## jq

```sh
$ jq -n 'now | [., todate]'
[
  1437619000.970498,
  "2015-07-23T02:36:40Z"
]
```


"now" reports the number of seconds since the beginning of the [https://en.wikipedia.org/wiki/Unix_epoch  Unix epoch].


## Jsish

Jsish does not implement the standard ECMAScript Date module but provides '''strftime''' and '''strptime'''.


```javascript
console.log(strftime());
```


{{out}} using interactive echo of results

```txt

prompt$ jsish
# strftime();
"2019-02-20 19:43:20"
```



## Julia


```Julia

ts = time()

println("The system time is (in ISO 8601 format):")
println(strftime("    %F %T %Z", ts))

```


```txt

The system time is (in ISO 8601 format):
    2015-04-08 14:19:38 CDT

```



## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    println("%tc".format(System.currentTimeMillis()))
}

```



## Lasso


```Lasso
date->format('%Q %T')
date->asInteger
```
 2013-11-02 11:47:27
 1383407747


## LFE

By default, LFE timestamps are turned in the <code>#(megasecs secs microsecs)</code> format.


```lisp

> (os:timestamp)
#(1423 786308 145798)

```


These can be changed to the more common date/time stamp with the <code>calendar</code> module:


```lisp

> (calendar:now_to_datetime (os:timestamp))
#(#(2015 2 13) #(0 12 18))

```


Locale taken into account:


```lisp

> (calendar:now_to_universal_time (os:timestamp))
#(#(2015 2 13) #(0 13 51))
> (calendar:now_to_local_time (os:timestamp))
#(#(2015 2 12) #(16 14 26))

```


Note that you will often encounter the function <code>erlang:now/0</code> giving a time similar to the system time. However, <code>erlang:now/0</code> may get delayed if the system time changes suddenly (i.e.: coming back from sleep mode). The delay is in place to ensure that <code>receive</code> clauses which are millisecond-sensitive do not get false timeouts.


## Liberty BASIC


```lb
print time$()               'time now as string "16:21:44"
print time$("seconds")      'seconds since midnight as number 32314
print time$("milliseconds") 'milliseconds since midnight as number 33221342
print time$("ms")           'milliseconds since midnight as number 33221342
```



## LIL

The '''system''' command is provided in the lil shell.  On GNU/Linux:

```tcl
print [system date]; print [system date +%s.%N]
```



```txt
Fri Aug 23 20:50:08 EDT 2019

1566607808.225126400
```



## Lingo


```lingo
put time()
-- "03:45"

put date()
-- "01.10.2016"

put the systemdate
-- date( 2016, 10, 1 )

put the systemdate.seconds
-- 13950

-- milliseconds since last boot, due to higher resolution better suited for random number seeding
put _system.milliseconds
-- 41746442
```



## LiveCode


```LiveCode
put the system time>
```



## Locomotive Basic

The variable "time" contains the time elapsed since last system start or reset. Each unit equals 1/300 s.
```locobasic
print time
print time/300;"s since last reboot"
```



## Logo

Other Logo variants might have a built-in command, but UCB Logo must access the Unix shell to get time.
```logo

to time
  output first first shell [date +%s]
end

make "start time
wait 300              ; 60ths of a second
print time - :start   ; 5
```



## Lua


```lua
print(os.date())
```



## M2000 Interpreter



```M2000 Interpreter

print str$(now,"long time"), time$(now)

```




## Mathematica

Different ways of doing this, here are 2 most common:
```Mathematica
Print[DateList[]]
Print[AbsoluteTime[]]
```
DateList will return the list {year,month,day,hour,minute,second} where all of them are integers except for second; that is a float. AbsoluteTime gives the total number of seconds since january 1st 1900 in your time zone.

=={{header|MATLAB}} / {{header|Octave}}==


```MATLAB
 datestr(now)
```

```txt
ans =
13-Aug-2010 12:59:56
```

```MATLAB
 clock>
```
<pre
ans =
2010     8     13      12      59     56.52
```



## Maxima


```maxima
/* Time and date in a formatted string */
timedate();
"2012-08-27 20:26:23+10:00"

/* Time in seconds elapsed since 1900/1/1 0:0:0 */
absolute_real_time();

/* Time in seconds since Maxima was started */
elapsed_real_time();
elapsed_run_time();
```


=={{header|Modula-2}}==
```modula2

MODULE Mytime;

FROM SysClock IMPORT
  GetClock, DateTime;
FROM STextIO IMPORT
  WriteString, WriteLn;
FROM FormatDT IMPORT
  DateTimeToString;

VAR
  CurrentTime: DateTime;
  DateStr, TimeStr: ARRAY [0 .. 20] OF CHAR;
BEGIN
  GetClock(CurrentTime);
  DateTimeToString(CurrentTime, DateStr, TimeStr);
  WriteString("Current time: ");
  WriteString(DateStr);
  WriteString(" ");
  WriteString(TimeStr);
  WriteLn;
END Mytime.

```


=={{header|Modula-3}}==

```modula3
MODULE MyTime EXPORTS Main;

IMPORT IO, FmtTime, Time;

BEGIN
  IO.Put("Current time: " & FmtTime.Long(Time.Now()) & "\n");
END MyTime.
```
 Current time: Tue Dec 30 20:50:07 CST 2008


## MUMPS

System time since midnight (in seconds) is kept in the second part of the system variable $HOROLOG.
```MUMPS
SYSTIME
 NEW PH
 SET PH=$PIECE($HOROLOG,",",2)
 WRITE "The system time is ",PH\3600,":",PH\60#60,":",PH#60
 KILL PH
 QUIT
```
Usage:
```txt
USER>D SYSTIME^ROSETTA
The system time is 22:55:44
```



## Neko


```ActionScript
/**
 <doc>
 <h2>System time</h2>
 <p>Neko uses Int32 to store system date/time values.
 And lib C strftime style formatting for converting to string form</p>
 </doc>
*/

var date_now = $loader.loadprim("std@date_now", 0)
var date_format = $loader.loadprim("std@date_format", 2)

var now = date_now()
$print(now, " ", date_format(now, "%T"), "\n")
```

```txt
prompt$ nekoc system-time.neko
prompt$ neko system-time.n
1542158592 20:23:12
```



## Nemerle


```Nemerle
System.Console.Write(System.DateTime.Now);
```



## NetRexx


```netrexx

import java.text.SimpleDateFormat
say SimpleDateFormat("yyyy-MM-dd-HH.mm.ss.SSS").format(Date())
```



## NewLISP


```NewLISP>
 (date)
"Sun Sep 28 20:17:55 2014"
```



## Nim


```nim
import times

echo(getDateStr())
echo(getClockStr())
echo(getTime())
```
 2013-08-02
 07:34:25
 Fri Aug  2 07:34:25 2013

=={{header|Objective-C}}==

```objc
NSLog(@"%@", [NSDate date]);
```
or (deprecated)
```objc
NSLog(@"%@", [NSCalendarDate calendarDate]);
```



## Objeck


```objeck
function : Time() ~ Nil {
  t := Time->New();
  IO.Console->GetInstance()->Print(t->GetHours())->Print(":")->Print(t->GetMinutes())->Print(":")->PrintLine(t->GetSeconds());
}
```



## OCaml


```ocaml
#load "unix.cma";;
open Unix;;
let {tm_sec = sec;
     tm_min = min;
     tm_hour = hour;
     tm_mday = mday;
     tm_mon = mon;
     tm_year = year;
     tm_wday = wday;
     tm_yday = yday;
     tm_isdst = isdst} = localtime (time ());;
Printf.printf "%04d-%02d-%02d %02d:%02d:%02d\n" (year + 1900) (mon + 1) mday hour min sec;
```



## Oforth


System.tick is used to retrieve a tick from the system in order to compute small intervals of time. It is used by #bench method to compute duration of a runnable (unit is micro second).


```Oforth
System.tick println
#[ #sqrt 1000000 seq map sum println ] bench
```


```txt

707095319487
666667166.458841
259819

```


System.localTime returns number of micro seconds since 01/01/1970:00:00:00
Integer returned by System.localTime allows to create a Date object.


```Oforth
import: date

System.localTime dup println asDate println drop drop
```


```txt

1421095428478000
2015-01-12 21:43:48,478

```



## Oz


```oz
{Show {OS.time}}      %% posix time (seconds since 1970-01-01)
{Show {OS.gmTime}}    %% current UTC as a record
{Show {OS.localTime}} %% current local time as record

%% Also interesting: undocumented module OsTime
%% When did posix time reach 1 billion?
{Show {OsTime.gmtime 1000000000}}
{Show {OsTime.localtime 1000000000}}
```
 1263347902
 time(hour:1 isDst:0 mDay:13 min:58 mon:0 sec:22 wDay:3 yDay:12 year:110)
 time(hour:2 isDst:0 mDay:13 min:58 mon:0 sec:22 wDay:3 yDay:12 year:110)
 time(hour:1 isDst:0 mDay:9 min:46 mon:8 sec:40 wDay:0 yDay:251 year:101)
 time(hour:3 isDst:1 mDay:9 min:46 mon:8 sec:40 wDay:0 yDay:251 year:101)


## PARI/GP

For timing the <code>gettime</code> is usually used, which measures CPU time rather than walltime.  But to get the raw time you'll need a system call


```parigp
system("date")
```


Direct access to the C library <code>time()</code> function can be had by an <code>install()</code>, and it should be faster than running the date program.


```parigp
install(time, "lf");
t = time();
print(t);    \\ integer seconds since the epoch (usually 1 Jan 1970)
```


<code>install()</code> can't give the higher resolution C library time functions like <code>gettimeofday()</code> or <code>clock_gettime()</code> but some further C code could put their results into a Pari integer or rational suitable for <code>install()</code> in GP.


## Pascal

```Pascal
program systime;
uses DOS;

{ Format digit with leading zero }
function lz(w: word): string;
var
  s: string;
begin
  str(w,s);
  if length(s) = 1 then
    s := '0' + s;
  lz := s;
end;

var
  h,m,s,c: word;
  yr,mo,da,dw: word;
  dt: datetime;
  t,ssm: longint;
  regs: registers;

begin

  { Time and Date }
  GetTime(h,m,s,c);
  writeln(lz(h),':',lz(m),':',lz(s),'.',c);
  GetDate(yr,mo,da,dw);
  writeln(yr,'-',lz(mo),'-',lz(da));

  { Turbo Epoch, seconds }
  with dt do begin
    year := yr; month := mo; day := da;
    hour := h; min := m; sec := s;
  end;
  packtime(dt,t);
  writeln(t);

  { Seconds since midnight, PC-BIOS 1Ah }
  regs.ah := 0; Intr($1A,regs);
  ssm := round((regs.cx * 65536 + regs.dx) * (65536 / 1192180));
  writeln(ssm);

end.
```


 23:42:35.9
 2010-07-29
 1023262033
 85427


## Perl

Simple localtime use in scalar context.
```perl
print scalar localtime, "\n";
```
 Thu Jan 24 11:23:30 2008

localtime use in array context.
```perl
($sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst) = localtime;
printf("%04d-%02d-%02d %02d:%02d:%02d\n", $year + 1900, $mon + 1, $mday, $hour, $min, $sec
```

 2008-01-24 11:23:30

The same using DateTime:
```perl
use DateTime;
my $dt = DateTime->now;
my $d = $dt->ymd;
my $t = $dt->hms;
print "$d $t\n";
```
 2010-03-29 19:46:26

localtime use in array context with [[POSIX]] strftime:
```perl
use POSIX qw(strftime);

$now_string = strftime "%a %b %e %H:%M:%S %Y", localtime;
print "$now_string\n";
```
Output (with cs_CZ.UTF-8 locale):

```txt
Čt led 24 11:23:30 2008
```
Using the [http://search.cpan.org/perldoc?DateTime DateTime] module:
```perl
use DateTime;
my $dt = DateTime->now;
say $dt->iso8601();
say $dt->year_with_christian_era();
say $dt->year_with_secular_era();
# etc.
```
Unix epoch:
```perl
print time;
```
 1280473609


## Perl 6


```perl6
say DateTime.now;
dd DateTime.now.Instant;
```

  2015-11-02T13:34:09+01:00
  Instant $var = Instant.new(<2159577143734/1493>)


## Phix


```Phix
include timedate.e
?format_timedate(date(),"Dddd, Mmmm dth, YYYY, h:mm:ss pm")

atom t0 = time()
sleep(0.9)
?time()-t0
```

```txt

"Tuesday, July 12th, 2016, 11:30:07 am"
0.906

```

Note the system clock tick rate mean times can be out by ~0.0155s;
it is not unusual to see total elapsed times of 0,0,0,0.016,0.016,0.016,0.031,0.031,0.031, etc.


## PHP

Seconds since the Unix epoch:
```php
echo time(), "\n";
```


Microseconds since the Unix epoch:
```php
echo microtime(), "\n";
```
Formatted time:
```php
echo date('D M j H:i:s Y'), "\n";  // custom format; see format characters here:
                                   // http://us3.php.net/manual/en/function.date.php
echo date('c'), "\n";  // ISO 8601 format
echo date('r'), "\n";  // RFC 2822 format
echo date(DATE_RSS), "\n";  // can also use one of the predefined formats here:
                            // http://us3.php.net/manual/en/class.datetime.php#datetime.constants.types
```



## PicoLisp


```PicoLisp
(stamp)
```
 -> "2010-02-19 15:14:06


## PL/I


```PL/I
put (datetime()); /* writes out the date and time */
                  /* The time is given to thousandths of a second, */
                  /* in the format hhmiss999 */
put (time());     /* gives the time in the format hhmiss999. */
```



## PowerShell

Using a cmdlet:
```powershell
Get-Date
```
or using .NET classes and properties:
```powershell
[DateTime]::Now
```



## PureBasic


```PureBasic
time=Date()  ; Unix timestamp
time$=FormatDate("%mm.%dd.%yyyy %hh:%ii:%ss",time)

; following value is only reasonable accurate, on Windows it can differ by +/- 20 ms
ms_counter=ElapsedMilliseconds()
; could use API like QueryPerformanceCounter_() on Windows for more accurate values
```



## Python


```python
import time
print time.ctime()
```



## Q

Date & time are accessible via the virtual .z namespace. lowercase names are UTC, uppercase are local:
```q
q).z.D
2010.01.25
q).z.N
0D14:17:45.519682000
q).z.P
2010.01.25D14:17:46.962375000
q).z.T
14:17:47.817
q).z.Z
2010.01.25T14:17:48.711
q).z.z
2010.01.25T19:17:59.445
```



## R

Note that this is output as a standard style string.
```R
Sys.time()
```
 [1] "2009-07-27 15:27:04 PDT"


## Racket


The system time as a date string:

```racket
#lang racket
(require racket/date)
(date->string (current-date))
```


Or, as seconds after midnight UTC, January 1, 1970:
```racket
#lang racket
(current-seconds)
```



## Raven

Note the use of single quotation marks for the date specifier.<lang>time dup print "\n" print int '%a %b %e %H:%M:%S %Y' date
```

 1353352623.511231
 Tue Nov 20 05:17:03 2012


## REBOL


```REBOL
now
print rejoin [now/year "-" now/month "-" now/day " " now/time]
```
 10-Dec-2009/7:43:55-5:00
 2009-12-10 7:43:55


## Retro

Displays the number of seconds since the Unix epoch:

```Retro
time putn>
```



## REXX

Note that REXX only examines the first character of the option and can be in upper or lower case.

```rexx
/*REXX program shows various ways to display the system time, including other options.  */

say '════════════ Normal format of time'
say 'hh:mm:ss        ◄─────────────── hh= is  00 ──► 23'
say 'hh:mm:ss        ◄─────────────── hh= hour   mm= minute   ss= second'
say time()
say time('n')                                    /*    (same as the previous example.)  */
say time('N')                                    /*       "   "  "      "       "       */
say time('Normal')                               /*       "   "  "      "       "       */
say time('nitPick')                              /*       "   "  "      "       "       */

say
say '════════════ Civil format of time'
say 'hh:mmcc         ◄─────────────── hh= is   1 ──► 12'
say 'hh:mmam         ◄─────────────── hh= hour   mm= minute   am= ante meridiem'
say 'hh:mmpm         ◄───────────────                         pm= post meridiem'
say time('C')
say time('civil')                                /*    (same as the previous example.)  */
                                                 /*ante meridiem≡Latin for before midday*/
                                                 /*post    "       "    "   after   "   */
say
say '════════════ long format of time'
say 'hh:mm:ss        ◄─────────────── hh= is   0 ──► 23'
say 'hh:mm:ss.ffffff ◄─────────────── hh= hour   mm= minute   fffff= fractional seconds'
say time('L')
say time('long')                                 /*    (same as the previous example.)  */
say time('long time no see')                     /*       "   "  "      "       "       */

say
say '════════════ complete hours since midnight'
say 'hh              ◄─────────────── hh =  0 ───► 23'
say time('H')
say time('hours')                                /*    (same as the previous example.)  */

say
say '════════════ complete minutes since midnight'
say 'mmmm            ◄─────────────── mmmm =  0 ───► 1439'
say time('M')
say time('minutes')                              /*    (same as the previous example.)  */

say
say '════════════  complete seconds since midnight'
say 'sssss           ◄─────────────── sssss =  0 ───► 86399'
say time('S')
say time('seconds')                              /*    (same as the previous example.)  */
                                                 /*stick a fork in it,  we're all done. */
```

```txt

════════════ Normal format of time
hh:mm:ss        ◄─────────────── hh= is  00 ──► 23
hh:mm:ss        ◄─────────────── hh= hour   mm= minute   ss= second
19:59:40
19:59:40
19:59:40
19:59:40
19:59:40

════════════ Civil format of time
hh:mmcc         ◄─────────────── hh= is   1 ──► 12
hh:mmam         ◄─────────────── hh= hour   mm= minute   am= ante meridiem
hh:mmpm         ◄───────────────                         pm= post meridiem
7:59pm
7:59pm

════════════ long format of time
hh:mm:ss        ◄─────────────── hh= is   0 ──► 23
hh:mm:ss.ffffff ◄─────────────── hh= hour   mm= minute   fffff= fractional seconds
19:59:40.250000
19:59:40.250000
19:59:40.250000

════════════ complete hours since midnight
hh              ◄─────────────── hh =  0 ───► 23
19
19

════════════ complete minutes since midnight
mmmm            ◄─────────────── mmmm =  0 ───► 1439
1199
1199

════════════  complete seconds since midnight
sssss           ◄─────────────── sssss =  0 ───► 86399
71980
71980

```



## Ring


```ring

/* Output:
** Sun                  abbreviated weekday name
** Sunday               full weekday name
** May                  abbreviated month name
** May                  full month name
** 05/24/15 09:58:38    Date & Time
** 24                   Day of the month
** 09                   Hour (24)
** 09                   Hour (12)
** 144                  Day of the year
** 05                   Month of the year
** 58                   Minutes after hour
** AM                   AM or PM
** 38                   Seconds after the hour
** 21                   Week of the year (sun-sat)
** 0                    day of the week
** 05/24/15             date
** 09:58:38             time
** 15                   year of the century
** 2015                 year
** Arab Standard Time   time zone
** %                    percent sign
*/

See TimeList()

```



## Ruby

version 1.9+
```ruby
t = Time.now

# textual
puts t        # => 2013-12-27 18:00:23 +0900

# epoch time
puts t.to_i   # => 1388134823

# epoch time with fractional seconds
puts t.to_f   # => 1388134823.9801579

# epoch time as a rational (more precision):
puts Time.now.to_r  # 1424900671883862959/1000000000

```



## Scala

{{libheader|Scala}}Ad hoc solution as [http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop REPL] scripts:

### JDK < 8


```scala
println(new java.util.Date)
```
 Sun Aug 14 22:47:42 EDT 2011
===JDK >= 8 (recommended)===

```scala
println(java.time.LocalTime.now())
```
 11:32:39.002


## Scheme

```scheme
(use posix)
(seconds->string (current-seconds))
```
 "Sat May 16 21:42:47 2009"


## Seed7


```seed7
$ include "seed7_05.s7i";
  include "time.s7i";

const proc: main is func
  begin
    writeln(time(NOW));
  end func;
```
 2009-12-07 17:09:44.687500 UTC+1


## SETL


```setl
$ Unix time
print(tod);

$ Human readable time and date
print(date);
```


```setl

1447628158908
Sun Nov 15 22:55:58 2015

```



## Sidef


```ruby
# textual
say Time.local.ctime;        # => Thu Mar 19 15:10:41 2015

# epoch time
say Time.sec;                # => 1426770641

# epoch time with fractional seconds
say Time.micro_sec;          # => 1426770641.68409
```



## Smalltalk

```smalltalk
DateTime now displayNl.>
```
 2011-08-10T00:43:36-0-7:00

```smalltalk
DateAndTime now>
```
 2011-08-16T19:40:37-03:00


## SNOBOL4


```SNOBOL4
     OUTPUT = DATE()
END
```
 03/30/2010 20:58:09


## SPL


```spl
hour,min,sec = #.now()
day,month,year = #.today()

#.output(#.str(hour,"00:"),#.str(min,"00:"),#.str(sec,"00.000"))
#.output(day,".",#.str(month,"00"),".",year)
```

```txt

03:15:34.068
24.08.2017

```



## Swift


```Swift
import Foundation

var ⌚️ = NSDate()
println(⌚️)
```
 2014-06-22 20:43:42 +0000


## Standard ML


```sml
print (Date.toString (Date.fromTimeLocal (Time.now ())) ^ "\n")
```



## SQL PL

```sql pl

SELECT CURRENT DATE, CURRENT TIME, CURRENT TIMESTAMP FROM SYSIBM.SYSDUMMY1;

```

Output:

```txt

db2 -t
db2 => SELECT CURRENT DATE, CURRENT TIME, CURRENT TIMESTAMP FROM SYSIBM.SYSDUMMY1;

1          2        3
---------- -------- --------------------------
04/29/2018 14:27:10 2018-04-29-14.27.10.674182

  1 record(s) selected.


```



## Stata



```stata
di c(current_date)
di c(current_time)
```



## Tcl

This uses a timestamp that is a number of seconds since the start of the UNIX Epoch.
```tcl
puts [clock seconds]
```

More readable forms may be created by formatting the time:
```tcl
puts [clock format [clock seconds]]
```


=={{header|TI-89 BASIC}}==

```ti89b
■ getTime()      {13  28  55}
■ getDate()       {2009  8  13}
```
Note that the system clock can be turned off, in which case the value returned will remain constant. <tt>isClkOn()</tt> can be used to check it.


## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
time=time()
PRINT time
```
 2011-04-05 13:45:44


## UNIX Shell


```bash
date       # Thu Dec  3 15:38:06 PST 2009

date +%s   # 1259883518, seconds since the epoch, like C stdlib time(0)
```



## Ursa


```ursa
# outputs time in milliseconds
import "time"
out (time.getcurrent) endl console
```



## Ursala

A library function, now, ignores its argument and returns the system time as a character string.
```Ursala
#import cli

#cast %s

main = now 0
```
 'Fri, 26 Jun 2009 20:31:49 +0100'
This string can be converted to seconds since 1970 (ignoring leap seconds)
by the library function string_to_time.


## Vala


```vala

var now = new DateTime.now_local();
string now_string = now.to_string();
```
 2011-11-12T20:23:24-0800


## VBA


```VB
Debug.Print Now()
```

 12/12/2013 16:16:16


## VBScript


```vb
WScript.Echo Now>
```



## XPL0

The intrinsics GetReg and SoftInt are used to access DOS and BIOS routines. GetReg returns the address of an array where a copy of the processor's hardware registers are stored. Values (such as $2C00 in the example) can be stored into this array. When SoftInt is called, the values in the array are loaded into the processor's registers and the specified interrupt ($21 in the example) is called.
```XPL0
include c:\cxpl\codes;          \include intrinsic 'code' declarations

proc NumOut(N);                 \Output a 2-digit number, including leading zero
int  N;
[if N <= 9 then ChOut(0, ^0);
IntOut(0, N);
]; \NumOut

int Reg;
[Reg:= GetReg;                  \get address of array with copy of CPU registers
Reg(0):= $2C00;                 \call DOS function 2C (hex)
SoftInt($21);                   \DOS calls are interrupt 21 (hex)
NumOut(Reg(2) >> 8);            \the high byte of register CX contains the hours
ChOut(0, ^:);
NumOut(Reg(2) & $00FF);         \the low byte of CX contains the minutes
ChOut(0, ^:);
NumOut(Reg(3) >> 8);            \the high byte of DX contains the seconds
ChOut(0, ^.);
NumOut(Reg(3) & $00FF);         \the low byte of DX contains hundreths
CrLf(0);
]
```
 13:08:26.60


## Yabasic


```Yabasic
print time$
```



## zkl


```zkl
Time.Clock.time //-->seconds since the epoch (C/OS defined)
```


