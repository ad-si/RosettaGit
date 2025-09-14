+++
title = "Discordian date"
description = ""
date = 2019-10-08T18:48:57Z
aliases = []
[extra]
id = 7783
[taxonomies]
categories = ["task", "Date and time"]
tags = []
languages = [
  "ada",
  "awk",
  "basic",
  "batch_file",
  "bbc_basic",
  "befunge",
  "c",
  "clojure",
  "cpp",
  "csharp",
  "d",
  "euphoria",
  "fortran",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jotacode",
  "julia",
  "kotlin",
  "maple",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pike",
  "powerbasic",
  "powershell",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "seed7",
  "sidef",
  "tcl",
  "zkl",
]
+++

## Task

Convert a given date from the   [[wp:Gregorian calendar|Gregorian calendar]]   to the   [[wp:Discordian calendar|Discordian calendar]].





## Ada


discordian.adb:

```Ada
with Ada.Calendar.Arithmetic;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Command_Line;

procedure Discordian is
   use Ada.Calendar;
   use Ada.Strings.Unbounded;
   use Ada.Command_Line;
   package UStr_IO renames Ada.Strings.Unbounded.Text_IO;

   subtype Year_Number is Integer range 3067 .. 3565;
   type Seasons is (Chaos, Discord, Confusion, Bureaucracy, The_Aftermath);
   type Days_Of_Week is (Sweetmorn, Boomtime, Pungenday,
                         Prickle_Prickle, Setting_Orange);
   subtype Day_Number is Integer range 1 .. 73;

   type Discordian_Date is record
      Year        : Year_Number;
      Season      : Seasons;
      Day         : Day_Number;
      Week_Day    : Days_Of_Week;
      Is_Tibs_Day : Boolean := False;
   end record;

   function Week_Day_To_Str(Day : Days_Of_Week) return Unbounded_String is
      s : Unbounded_String;
   begin
      case Day is
         when Sweetmorn       => s := To_Unbounded_String("Sweetmorn");
         when Boomtime        => s := To_Unbounded_String("Boomtime");
         when Pungenday       => s := To_Unbounded_String("Pungenday");
         when Prickle_Prickle => s := To_Unbounded_String("Prickle-Prickle");
         when Setting_Orange  => s := To_Unbounded_String("Setting Orange");
      end case;
      return s;
   end Week_Day_To_Str;

   function Holiday(Season: Seasons) return Unbounded_String is
      s : Unbounded_String;
   begin
      case Season is
         when Chaos         => s := To_Unbounded_String("Chaoflux");
         when Discord       => s := To_Unbounded_String("Discoflux");
         when Confusion     => s := To_Unbounded_String("Confuflux");
         when Bureaucracy   => s := To_Unbounded_String("Bureflux");
         when The_Aftermath => s := To_Unbounded_String("Afflux");
      end case;
      return s;
   end Holiday;

   function Apostle(Season: Seasons) return Unbounded_String is
      s : Unbounded_String;
   begin
      case Season is
         when Chaos         => s := To_Unbounded_String("Mungday");
         when Discord       => s := To_Unbounded_String("Mojoday");
         when Confusion     => s := To_Unbounded_String("Syaday");
         when Bureaucracy   => s := To_Unbounded_String("Zaraday");
         when The_Aftermath => s := To_Unbounded_String("Maladay");
      end case;
      return s;
   end Apostle;

   function Season_To_Str(Season: Seasons) return Unbounded_String is
      s : Unbounded_String;
   begin
      case Season is
         when Chaos         => s := To_Unbounded_String("Chaos");
         when Discord       => s := To_Unbounded_String("Discord");
         when Confusion     => s := To_Unbounded_String("Confusion");
         when Bureaucracy   => s := To_Unbounded_String("Bureaucracy");
         when The_Aftermath => s := To_Unbounded_String("The Aftermath");
      end case;
      return s;
   end Season_To_Str;

   procedure Convert (From : Time; To : out Discordian_Date) is
      use Ada.Calendar.Arithmetic;
      First_Day   : Time;
      Number_Days : Day_Count;
      Leap_Year   : boolean;
   begin
      First_Day   := Time_Of (Year => Year (From), Month => 1, Day => 1);
      Number_Days := From - First_Day;

      To.Year        := Year (Date => From) + 1166;
      To.Is_Tibs_Day := False;
      Leap_Year := False;
      if Year (Date => From) mod 4 = 0 then
          if Year (Date => From) mod 100 = 0 then
              if Year (Date => From) mod 400 = 0 then
                  Leap_Year := True;
              end if;
          else
              Leap_Year := True;
          end if;
      end if;
      if Leap_Year then
         if Number_Days > 59 then
            Number_Days := Number_Days - 1;
         elsif Number_Days = 59 then
            To.Is_Tibs_Day := True;
         end if;
      end if;
      To.Day := Day_Number (Number_Days mod 73 + 1);
      case Number_Days / 73 is
         when 0 => To.Season := Chaos;
         when 1 => To.Season := Discord;
         when 2 => To.Season := Confusion;
         when 3 => To.Season := Bureaucracy;
         when 4 => To.Season := The_Aftermath;
         when others => raise Constraint_Error;
      end case;
      case Number_Days mod 5 is
         when 0 => To.Week_Day := Sweetmorn;
         when 1 => To.Week_Day := Boomtime;
         when 2 => To.Week_Day := Pungenday;
         when 3 => To.Week_Day := Prickle_Prickle;
         when 4 => To.Week_Day := Setting_Orange;
         when others => raise Constraint_Error;
      end case;
   end Convert;

   procedure Put (Item : Discordian_Date) is
   begin
      if Item.Is_Tibs_Day then
         Ada.Text_IO.Put ("St. Tib's Day");
      else
         UStr_IO.Put (Week_Day_To_Str(Item.Week_Day));
         Ada.Text_IO.Put (", day" & Integer'Image (Item.Day));
         Ada.Text_IO.Put (" of ");
         UStr_IO.Put (Season_To_Str (Item.Season));
         if Item.Day = 5 then
            Ada.Text_IO.Put (", ");
            UStr_IO.Put (Apostle(Item.Season));
         elsif Item.Day = 50 then
            Ada.Text_IO.Put (", ");
            UStr_IO.Put (Holiday(Item.Season));
         end if;
      end if;
      Ada.Text_IO.Put (" in the YOLD" & Integer'Image (Item.Year));
      Ada.Text_IO.New_Line;
   end Put;

   Test_Day  : Time;
   Test_DDay : Discordian_Date;
   Year : Integer;
   Month : Integer;
   Day : Integer;
   YYYYMMDD : Integer;
begin

   if Argument_Count = 0 then
      Test_Day := Clock;
      Convert (From => Test_Day, To => Test_DDay);
      Put (Test_DDay);
   end if;

   for Arg in 1..Argument_Count loop

      if Argument(Arg)'Length < 8 then
         Ada.Text_IO.Put("ERROR: Invalid Argument : '" & Argument(Arg) & "'");
         Ada.Text_IO.Put("Input format YYYYMMDD");
         raise Constraint_Error;
      end if;

      begin
         YYYYMMDD := Integer'Value(Argument(Arg));
      exception
         when Constraint_Error =>
            Ada.Text_IO.Put("ERROR: Invalid Argument : '" & Argument(Arg) & "'");
            raise;
      end;

      Day := YYYYMMDD mod 100;
      if Day < Day_Number'First or Day > Day_Number'Last then
         Ada.Text_IO.Put("ERROR: Invalid Day:" & Integer'Image(Day));
         raise Constraint_Error;
      end if;

      Month := ((YYYYMMDD - Day) / 100) mod 100;
      if Month < Month_Number'First or Month > Month_Number'Last then
         Ada.Text_IO.Put("ERROR: Invalid Month:" & Integer'Image(Month));
         raise Constraint_Error;
      end if;

      Year := ((YYYYMMDD - Day - Month * 100) / 10000);
      if Year < 1901 or Year > 2399 then
         Ada.Text_IO.Put("ERROR: Invalid Year:" & Integer'Image(Year));
         raise Constraint_Error;
      end if;

      Test_Day := Time_Of (Year => Year, Month => Month, Day => Day);

      Convert (From => Test_Day, To => Test_DDay);
      Put (Test_DDay);

   end loop;

end Discordian;

```


```txt
$ ./discordian 21001231 20120228 20120229 20120301 20100722 20120902 20121231
Setting Orange, day 73 of The Aftermath in the YOLD 3266
Prickle-Prickle, day 59 of Chaos in the YOLD 3178
St. Tib's Day in the YOLD 3178
Setting Orange, day 60 of Chaos in the YOLD 3178
Pungenday, day 57 of Confusion in the YOLD 3176
Setting Orange, day 26 of Bureaucracy in the YOLD 3178
Setting Orange, day 73 of The Aftermath in the YOLD 3178
```



## AWK


```AWK

# DDATE.AWK - Gregorian to Discordian date contributed by Dan Nielsen
# syntax: GAWK -f DDATE.AWK [YYYYMMDD | YYYY-MM-DD | MM-DD-YYYY | DDMMMYYYY | YYYY] ...
# examples:
#   GAWK -f DDATE.AWK                    today
#   GAWK -f DDATE.AWK 20110722           one date
#   GAWK -f DDATE.AWK 20110722 20120229  two dates
#   GAWK -f DDATE.AWK 2012               yearly calendar
BEGIN {
    split("Chaos,Discord,Confusion,Bureaucracy,The Aftermath",season_arr,",")
    split("Sweetmorn,Boomtime,Pungenday,Prickle-Prickle,Setting Orange",weekday_arr,",")
    split("Mungday,Mojoday,Syaday,Zaraday,Maladay",apostle_holyday_arr,",")
    split("Chaoflux,Discoflux,Confuflux,Bureflux,Afflux",season_holyday_arr,",")
    split("31,28,31,30,31,30,31,31,30,31,30,31",days_in_month,",") # days per month in non leap year
    split("0   31  59  90  120 151 181 212 243 273 304 334",rdt," ") # relative day table
    mmm = "JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC"
#          1   2   3   4   5   6   7   8   9   10  11  12
    if (ARGV[1] == "") { # use current date
      ARGV[ARGC++] = strftime("%Y%m%d") # GAWK only
    # ARGV[ARGC++] = dos_date() # any AWK
    # timetab(arr); ARGV[ARGC++] = sprintf("%04d%02d%02d",arr["YEAR"],arr["MONTH"],arr["DAY"]) # TAWK only
    }
    for (argno=1; argno<=ARGC-1; argno++) { # validate command line arguments
      print("")
      x = toupper(ARGV[argno])
      if (x ~ /^[0-9][0-9][0-9][0-9][01][0-9][0-3][0-9]$/) { # YYYYMMDD
        main(x)
      }
      else if (x ~ /^[0-9][0-9][0-9][0-9]-[01][0-9]-[0-3][0-9]$/) { # YYYY-MM-DD
        gsub(/-/,"",x)
        main(x)
      }
      else if (x ~ /^[01][0-9]-[0-3][0-9]-[0-9][0-9][0-9][0-9]$/) { # MM-DD-YYYY
        main(substr(x,7,4) substr(x,1,2) substr(x,4,2))
      }
      else if (x ~ /^[0-3][0-9](JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)[0-9][0-9][0-9][0-9]$/) { # DDMMMYYYY
        main(sprintf("%04d%02d%02d",substr(x,6,4),int((match(mmm,substr(x,3,3))/4)+1),substr(x,1,2)))
      }
      else if (x ~ /^[0-9][0-9][0-9][0-9]$/) { # YYYY
        yearly_calendar(x)
      }
      else {
        error("begin")
      }
    }
    if (errors == 0) { exit(0) } else { exit(1) }
}
function main(x,  d,dyear,m,season_day,season_nbr,text,weekday_nbr,y,year_day) {
    y = substr(x,1,4) + 0
    m = substr(x,5,2) + 0
    d = substr(x,7,2) + 0
    days_in_month[2] = (leap_year(y) == 1) ? 29 : 28
    if (m < 1 || m > 12 || d < 1 || d > days_in_month[m]+0) {
      error("main")
      return
    }
    year_day = rdt[m] + d
    dyear = y + 1166 # Discordian year
    season_nbr = int((year_day - 1 ) / 73) + 1
    season_day = ((year_day - 1) % 73) + 1
    weekday_nbr = ((year_day - 1 ) % 5) + 1
    if (season_day == 5) {
      text = ", " apostle_holyday_arr[season_nbr]
    }
    else if (season_day == 50) {
      text = ", " season_holyday_arr[season_nbr]
    }
    if (leap_year(y) && m == 2 && d == 29) {
      printf("%04d-%02d-%02d is St. Tib's day, Year of Our Lady of Discord %s\n",y,m,d,dyear)
    }
    else {
      printf("%04d-%02d-%02d is %s, %s %s, Year of Our Lady of Discord %s%s\n",
      y,m,d,weekday_arr[weekday_nbr],season_arr[season_nbr],season_day,dyear,text)
    }
}
function leap_year(y) { # leap year: 0=no, 1=yes
    return (y % 400 == 0 || (y % 4 == 0 && y % 100)) ? 1 : 0
}
function yearly_calendar(y,  d,m) {
    days_in_month[2] = (leap_year(y) == 1) ? 29 : 28
    for (m=1; m<=12; m++) {
      for (d=1; d<=days_in_month[m]; d++) {
        main(sprintf("%04d%02d%02d",y,m,d))
      }
    }
}
function dos_date(  arr,cmd,d,x) { # under Microsoft Windows
# XP - The current date is: MM/DD/YYYY
# 8  - The current date is: DOW MM/DD/YYYY
    cmd = "DATE <NUL"
    cmd | getline x
    close(cmd) # close pipe
    d = arr[split(x,arr," ")]
    return sprintf("%04d%02d%02d",substr(d,7,4),substr(d,1,2),substr(d,4,2))
}
function error(x) {
    printf("error: argument %d is invalid, %s, in %s\n",argno,ARGV[argno],x)
    errors++
}

```

```txt

GAWK -f DDATE.AWK
2011-08-22 is Prickle-Prickle, Bureaucracy 15, Year of Our Lady of Discord 3177

GAWK -f DDATE.AWK 20110722 20120229
2011-07-22 is Pungenday, Confusion 57, Year of Our Lady of Discord 3177
2012-02-29 is St. Tib's day, Year of Our Lady of Discord 3178

```



## BASIC

```qbasic
#INCLUDE "datetime.bi"

DECLARE FUNCTION julian(AS DOUBLE) AS INTEGER

SeasonNames:
DATA "Chaos", "Discord", "Confusion", "Bureaucracy", "The Aftermath"
Weekdays:
DATA "Setting Orange", "Sweetmorn", "Boomtime", "Pungenday", "Prickle-Prickle"
DaysPreceding1stOfMonth:
'   jan feb mar apr may  jun  jul  aug  sep  oct  nov  dec
DATA 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334

DIM dyear AS INTEGER, dseason AS STRING, dday AS INTEGER, dweekday AS STRING
DIM tmpdate AS DOUBLE, jday AS INTEGER, result AS STRING
DIM L0 AS INTEGER

IF LEN(COMMAND$) THEN
    tmpdate = DATEVALUE(COMMAND$)
ELSE
    tmpdate = FIX(NOW())
END IF
dyear = YEAR(tmpdate) + 1166
IF (2 = MONTH(tmpdate)) AND (29 = DAY(tmpdate)) THEN
    result = "Saint Tib's Day, " & STR$(dyear) & " YOLD"
ELSE
    jday = julian(tmpdate)
    RESTORE SeasonNames
    FOR L0 = 1 TO ((jday - 1) \ 73) + 1
    	READ dseason
    NEXT
    dday = (jday MOD 73)
    IF 0 = dday THEN dday = 73
    RESTORE Weekdays
    FOR L0 = 1 TO (jday MOD 5) + 1
        READ dweekday
    NEXT
    result = dweekday & ", " & dseason & " " & TRIM$(STR$(dday)) & ", " & TRIM$(STR$(dyear)) & " YOLD"
END IF

? result
END

FUNCTION julian(d AS DOUBLE) AS INTEGER
    'doesn't account for leap years (not needed for ddate)
    DIM tmp AS INTEGER, L1 AS INTEGER
    RESTORE DaysPreceding1stOfMonth
    FOR L1 = 1 TO MONTH(d)
        READ tmp
    NEXT
    FUNCTION = tmp + DAY(d)
END FUNCTION
```

```txt
"Discordian date.exe" 19-10-2015
Boomtime, Bureaucracy 73, 3181 YOLD
```



## Batch File

```dos
@echo off
goto Parse

 Discordian Date Converter:

 Usage:
	ddate
	ddate /v
	ddate /d isoDate
	ddate /v /d isoDate

:Parse
	shift
	if "%0"==""   goto Prologue
	if "%0"=="/v" set Verbose=1
	if "%0"=="/d" set dateToTest=%1
	if "%0"=="/d" shift
goto Parse

:Prologue
	if "%dateToTest%"=="" set dateToTest=%date%
	for %%a in (GYear GMonth GDay GMonthName GWeekday GDayThisYear) do set %%a=fnord
	for %%a in (DYear DMonth DDay DMonthName DWeekday) do set %%a=MUNG
goto Start

:Start
	for /f "tokens=1,2,3 delims=/:;-. " %%a in ('echo %dateToTest%') do (
		set GYear=%%a
		set GMonth=%%b
		set GDay=%%c
	)
goto GMonthName

:GMonthName
	if %GMonth% EQU  1	set GMonthName=January
	if %GMonth% EQU  2	set GMonthName=February
	if %GMonth% EQU  3	set GMonthName=March
	if %GMonth% EQU  4	set GMonthName=April
	if %GMonth% EQU  5	set GMonthName=May
	if %GMonth% EQU  6	set GMonthName=June
	if %GMonth% EQU  7	set GMonthName=July
	if %GMonth% EQU  8	set GMonthName=August
	if %GMonth% EQU  9	set GMonthName=September
	if %GMonth% EQU 10	set GMonthName=October
	if %GMonth% EQU 11	set GMonthName=November
	if %GMonth% EQU 12	set GMonthName=December
goto GLeap

:GLeap
	set /a CommonYear=GYear %% 4
	set /a CommonCent=GYear %% 100
	set /a CommonQuad=GYear %% 400
	set GLeap=0
	if %CommonYear% EQU 0 set GLeap=1
	if %CommonCent% EQU 0 set GLeap=0
	if %CommonQuad% EQU 0 set GLeap=1
goto GDayThisYear

:GDayThisYear
	set GDayThisYear=%GDay%
	if %GMonth% GTR 11                  set /a GDayThisYear=%GDayThisYear%+30
	if %GMonth% GTR 10                  set /a GDayThisYear=%GDayThisYear%+31
	if %GMonth% GTR  9                  set /a GDayThisYear=%GDayThisYear%+30
	if %GMonth% GTR  8                  set /a GDayThisYear=%GDayThisYear%+31
	if %GMonth% GTR  7                  set /a GDayThisYear=%GDayThisYear%+31
	if %GMonth% GTR  6                  set /a GDayThisYear=%GDayThisYear%+30
	if %GMonth% GTR  5                  set /a GDayThisYear=%GDayThisYear%+31
	if %GMonth% GTR  4                  set /a GDayThisYear=%GDayThisYear%+30
	if %GMonth% GTR  3                  set /a GDayThisYear=%GDayThisYear%+31
	if %GMonth% GTR  2 if %GLeap% EQU 1 set /a GDayThisYear=%GDayThisYear%+29
	if %GMonth% GTR  2 if %GLeap% EQU 0 set /a GDayThisYear=%GDayThisYear%+28
	if %GMonth% GTR  1                  set /a GDayThisYear=%GDayThisYear%+31
goto DYear

:DYear
	set /a DYear=GYear+1166
goto DMonth

:DMonth
	set DMonth=1
	set DDay=%GDayThisYear%
	if %DDay% GTR 73 (
		set /a DDay=%DDay%-73
		set /a DMonth=%DMonth%+1
	)
	if %DDay% GTR 73 (
		set /a DDay=%DDay%-73
		set /a DMonth=%DMonth%+1
	)
	if %DDay% GTR 73 (
		set /a DDay=%DDay%-73
		set /a DMonth=%DMonth%+1
	)
	if %DDay% GTR 73 (
		set /a DDay=%DDay%-73
		set /a DMonth=%DMonth%+1
	)
goto DDay

:DDay
	if %GLeap% EQU 1 (
		if %GDayThisYear% GEQ 61 (
			set /a DDay=%DDay%-1
		)
	)
	if %DDay% EQU 0 (
		set /a DDay=73
		set /a DMonth=%DMonth%-1
	)
goto DMonthName

:DMonthName
	if %DMonth% EQU 1 set DMonthName=Chaos
	if %DMonth% EQU 2 set DMonthName=Discord
	if %DMonth% EQU 3 set DMonthName=Confusion
	if %DMonth% EQU 4 set DMonthName=Bureaucracy
	if %DMonth% EQU 5 set DMonthName=Aftermath
goto DTib

:DTib
	set DTib=0
	if %GDayThisYear% EQU 60 if %GLeap% EQU 1 set DTib=1
	if %GLeap% EQU 1 if %GDayThisYear% GTR 60 set /a GDayThisYear=%GDayThisYear%-1
	set DWeekday=%GDayThisYear%
goto DWeekday

:DWeekday
	if %DWeekday% LEQ 5 goto _DWeekday
	set /a DWeekday=%DWeekday%-5
	goto DWeekday
:_DWeekday
	if %DWeekday% EQU 1 set DWeekday=Sweetmorn
	if %DWeekday% EQU 2 set DWeekday=Boomtime
	if %DWeekday% EQU 3 set DWeekday=Pungenday
	if %DWeekday% EQU 4 set DWeekday=Prickle-Prickle
	if %DWeekday% EQU 5 set DWeekday=Setting Orange
goto GWeekday

:GWeekday
goto GEnding

:GEnding
	set GEnding=th
	for %%a in (1 21 31) do	if %GDay% EQU %%a set GEnding=st
	for %%a in (2 22) do 	if %GDay% EQU %%a set GEnding=nd
	for %%a in (3 23) do	if %GDay% EQU %%a set GEnding=rd
goto DEnding

:DEnding
	set DEnding=th
	for %%a in (1 21 31 41 51 61 71) do if %Dday% EQU %%a set DEnding=st
	for %%a in (2 22 32 42 52 62 72) do if %Dday% EQU %%a set DEnding=nd
	for %%a in (3 23 33 43 53 63 73) do if %Dday% EQU %%a set DEnding=rd
goto Display

:Display
	if "%Verbose%"=="1" goto Display2
	echo.
	if %DTib% EQU 1 (
		echo St. Tib's Day, %DYear%
	) else echo %DWeekday%, %DMonthName% %DDay%%DEnding%, %DYear%
goto Epilogue

:Display2
	echo.
	echo  Gregorian: %GMonthName% %GDay%%GEnding%, %GYear%
	if %DTib% EQU 1 (
		echo Discordian: St. Tib's Day, %DYear%
	) else echo Discordian: %DWeekday%, %DMonthName% %DDay%%DEnding%, %DYear%
goto Epilogue

:Epilogue
set Verbose=
set dateToTest=
echo.

:End

```

```txt

H:\>ddate

Pungenday, Confusion 12th, 3182

H:\>ddate /v

 Gregorian: June 07th, 2016
Discordian: Pungenday, Confusion 12th, 3182

H:\>ddate /d 2016-02-28

Prickle-Prickle, Chaos 59th, 3182

H:\>ddate /v /d 2016-02-28

 Gregorian: February 28th, 2016
Discordian: Prickle-Prickle, Chaos 59th, 3182

H:\>ddate /v /d 2016-02-29

 Gregorian: February 29th, 2016
Discordian: St. Tib's Day, 3182

H:\>ddate /d 2016-02-29

St. Tib's Day, 3182

H:\>

```


This program assumes the date is in ISO order (yyyy-mm-dd) and has leading zeros.  It also assumes the system date is similarly formatted, which is atypical on Windows machines.

Built on CMD.EXE version 5.1.2600.5512 (xpsp.080413-2111).


## BBC BASIC

```bbcbasic
      INSTALL @lib$+"DATELIB"

      PRINT "01/01/2011 -> " FNdiscordian("01/01/2011")
      PRINT "05/01/2011 -> " FNdiscordian("05/01/2011")
      PRINT "28/02/2011 -> " FNdiscordian("28/02/2011")
      PRINT "01/03/2011 -> " FNdiscordian("01/03/2011")
      PRINT "22/07/2011 -> " FNdiscordian("22/07/2011")
      PRINT "31/12/2011 -> " FNdiscordian("31/12/2011")

      PRINT "01/01/2012 -> " FNdiscordian("01/01/2012")
      PRINT "05/01/2012 -> " FNdiscordian("05/01/2012")
      PRINT "28/02/2012 -> " FNdiscordian("28/02/2012")
      PRINT "29/02/2012 -> " FNdiscordian("29/02/2012")
      PRINT "01/03/2012 -> " FNdiscordian("01/03/2012")
      PRINT "22/07/2012 -> " FNdiscordian("22/07/2012")
      PRINT "31/12/2012 -> " FNdiscordian("31/12/2012")
      END

      DEF FNdiscordian(date$)
      LOCAL Season$(), Weekday$(), mjd%, year%, day%
      DIM Season$(4), Weekday$(4)
      Season$() =  "Chaos", "Discord", "Confusion", "Bureaucracy", "The Aftermath"
      Weekday$() = "Sweetmorn", "Boomtime", "Pungenday", "Prickle-Prickle", "Setting Orange"

      mjd% = FN_readdate(date$, "dmy", 2000)
      year% = FN_year(mjd%)

      IF FN_month(mjd%)=2 AND FN_day(mjd%)=29 THEN
        = "St. Tib's Day, YOLD " + STR$(year% + 1166)
      ENDIF

      IF FN_month(mjd%) < 3 THEN
        day% = mjd% - FN_mjd(1, 1, year%)
      ELSE
        day% = mjd% - FN_mjd(1, 3, year%) + 59
      ENDIF
      = Weekday$(day% MOD 5) + ", " + STR$(day% MOD 73 + 1) + " " + \
      \ Season$(day% DIV 73) + ", YOLD " + STR$(year% + 1166)
```

```txt

01/01/2011 -> Sweetmorn, 1 Chaos, YOLD 3177
05/01/2011 -> Setting Orange, 5 Chaos, YOLD 3177
28/02/2011 -> Prickle-Prickle, 59 Chaos, YOLD 3177
01/03/2011 -> Setting Orange, 60 Chaos, YOLD 3177
22/07/2011 -> Pungenday, 57 Confusion, YOLD 3177
31/12/2011 -> Setting Orange, 73 The Aftermath, YOLD 3177
01/01/2012 -> Sweetmorn, 1 Chaos, YOLD 3178
05/01/2012 -> Setting Orange, 5 Chaos, YOLD 3178
28/02/2012 -> Prickle-Prickle, 59 Chaos, YOLD 3178
29/02/2012 -> St. Tib's Day, YOLD 3178
01/03/2012 -> Setting Orange, 60 Chaos, YOLD 3178
22/07/2012 -> Pungenday, 57 Confusion, YOLD 3178
31/12/2012 -> Setting Orange, 73 The Aftermath, YOLD 3178

```



## Befunge

Reads the date to convert from stdin as three separate numeric inputs (year, month, and day).


```befunge
0" :raeY">:#,_&>\" :htnoM">:#,_&>04p" :yaD">:#,_$&>55+,1-:47*v
v"f I".+1%,,,,"Day I":$_:#<0#!4#:p#-4#1g4-#0+#<<_v#!*!-2g40!-<
>"o",,,/:5+*66++:4>g#<:#44#:9#+*#1-#,_$$0 v_v#!< >$ 0 "yaD " v
@,+55.+*+92"j"$_,#!>#:<", in the YOLD"*84 <.>,:^ :"St. Tib's"<
$# #"#"##"#"Chaos$Discord$Confusion$Bureaucracy$The Aftermath$
```


{{out}} (multiple runs)

```txt
Year: 2015
Month: 10
Day: 19

Day 73 of Bureaucracy, in the YOLD 3181

Year: 2012
Month: 2
Day: 29

St. Tib's Day, in the YOLD 3178

Year: 2010
Month: 7
Day: 22

Day 57 of Confusion, in the YOLD 3176
```



## C


For the source code of <code>ddate</code> in util-linux package, see [[http://jubal.westnet.com/hyperdiscordia/ddate.html]].


```cpp
#include <iostream>
#include <stdio.h>
#include <time.h>

#define day_of_week( x ) ((x) == 1 ? "Sweetmorn" :\
                          (x) == 2 ? "Boomtime" :\
                          (x) == 3 ? "Pungenday" :\
                          (x) == 4 ? "Prickle-Prickle" :\
                          "Setting Orange")

#define season( x ) ((x) == 0 ? "Chaos" :\
                    (x) == 1 ? "Discord" :\
                    (x) == 2 ? "Confusion" :\
                    (x) == 3 ? "Bureaucracy" :\
                    "The Aftermath")

#define date( x ) ((x)%73 == 0 ? 73 : (x)%73)

#define leap_year( x ) ((x) % 400 == 0 || (((x) % 4) == 0 && (x) % 100))

char * ddate( int y, int d ){
  int dyear = 1166 + y;
  char * result = malloc( 100 * sizeof( char ) );

  if( leap_year( y ) ){
    if( d == 60 ){
      sprintf( result, "St. Tib's Day, YOLD %d", dyear );
      return result;
    } else if( d >= 60 ){
      -- d;
    }
  }

  sprintf( result, "%s, %s %d, YOLD %d",
           day_of_week(d%5), season(((d%73)==0?d-1:d)/73 ), date( d ), dyear );

  return result;
}


int day_of_year( int y, int m, int d ){
  int month_lengths[ 12 ] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

  for( ; m > 1; m -- ){
    d += month_lengths[ m - 2 ];
    if( m == 3 && leap_year( y ) ){
      ++ d;
    }
  }
  return d;
}


int main( int argc, char * argv[] ){
  time_t now;
  struct tm * now_time;
  int year, doy;

  if( argc == 1 ){
    now = time( NULL );
    now_time = localtime( &now );
    year = now_time->tm_year + 1900; doy = now_time->tm_yday + 1;
  } else if( argc == 4 ){
    year = atoi( argv[ 1 ] ); doy = day_of_year( atoi( argv[ 1 ] ), atoi( argv[ 2 ] ), atoi( argv[ 3 ] ) );
  }

  char * result = ddate( year, doy );
  puts( result );
  free( result );

  return 0;
}
```


Demonstration:


```txt
$ ./ddate 2015 10 19
Boomtime, Bureaucracy 73, YOLD 3181
$ ./ddate 7 1 2011
Sweetmorn, The Aftermath 40, YOLD 1173
$ ./ddate 2011 1 7
Boomtime, Chaos 7, YOLD 3177
$ ./ddate 2012 2 28
Prickle-Prickle, Chaos 59, YOLD 3178
$ ./ddate 2012 2 29
St. Tib's Day, YOLD 3178
$ ./ddate 2012 3 1
Setting Orange, Chaos 60, YOLD 3178
$ ./ddate 2010 7 22
Pungenday, Confusion 57, YOLD 3176
$ ./ddate # today is Jan 4, 2016
Prickle-Prickle, Chaos 4, YOLD 3182
```



## C++


```cpp

#include <iostream>
#include <algorithm>
#include <vector>
#include <sstream>
#include <iterator>
using namespace std;
class myTuple
{
public:
    void set( int a, int b, string c ) { t.first.first = a; t.first.second = b; t.second = c; }
    bool operator == ( pair<int, int> p ) { return p.first == t.first.first && p.second == t.first.second; }
    string second() { return t.second; }
private:
    pair<pair<int, int>, string> t;
};
class discordian
{
public:
    discordian() {
        myTuple t;
        t.set( 5, 1, "Mungday" ); holyday.push_back( t ); t.set( 19, 2, "Chaoflux" ); holyday.push_back( t );
        t.set( 29, 2, "St. Tib's Day" ); holyday.push_back( t ); t.set( 19, 3, "Mojoday" ); holyday.push_back( t );
        t.set( 3, 5, "Discoflux" ); holyday.push_back( t ); t.set( 31, 5, "Syaday" ); holyday.push_back( t );
        t.set( 15, 7, "Confuflux" ); holyday.push_back( t ); t.set( 12, 8, "Zaraday" ); holyday.push_back( t );
        t.set( 26, 9, "Bureflux" ); holyday.push_back( t ); t.set( 24, 10, "Maladay" ); holyday.push_back( t );
        t.set( 8, 12, "Afflux" ); holyday.push_back( t );
        seasons.push_back( "Chaos" ); seasons.push_back( "Discord" ); seasons.push_back( "Confusion" );
        seasons.push_back( "Bureaucracy" ); seasons.push_back( "The Aftermath" );
        wdays.push_back( "Setting Orange" ); wdays.push_back( "Sweetmorn" ); wdays.push_back( "Boomtime" );
        wdays.push_back( "Pungenday" ); wdays.push_back( "Prickle-Prickle" );
    }
    void convert( int d, int m, int y ) {
        if( d == 0 || m == 0 || m > 12 || d > getMaxDay( m, y ) ) {
            cout << "\nThis is not a date!";
            return;
        }
        vector<myTuple>::iterator f = find( holyday.begin(), holyday.end(), make_pair( d, m ) );
        int dd = d, day, wday, sea,  yr = y + 1166;
        for( int x = 1; x < m; x++ )
            dd += getMaxDay( x, 1 );
        day = dd % 73; if( !day ) day = 73;
        wday = dd % 5;
        sea  = ( dd - 1 ) / 73;
        if( d == 29 && m == 2 && isLeap( y ) ) {
            cout << ( *f ).second() << " " << seasons[sea] << ", Year of Our Lady of Discord " << yr;
            return;
        }
        cout << wdays[wday] << " " << seasons[sea] << " " << day;
        if( day > 10 && day < 14 ) cout << "th";
        else switch( day % 10) {
            case 1: cout << "st"; break;
            case 2: cout << "nd"; break;
            case 3: cout << "rd"; break;
            default: cout << "th";
        }
        cout << ", Year of Our Lady of Discord " << yr;
        if( f != holyday.end() ) cout << " - " << ( *f ).second();
    }
private:
    int getMaxDay( int m, int y ) {
        int dd[] = { 0, 31, isLeap( y ) ? 29 : 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }; return dd[m];
    }
    bool isLeap( int y ) {
        bool l = false;
        if( !( y % 4 ) ) {
            if( y % 100 ) l = true;
            else if( !( y % 400 ) ) l = true;
        }
        return l;
    }
    vector<myTuple> holyday; vector<string> seasons, wdays;
};
int main( int argc, char* argv[] ) {
    string date; discordian disc;
    while( true ) {
        cout << "Enter a date (dd mm yyyy) or 0 to quit: "; getline( cin, date ); if( date == "0" ) break;
        if( date.length() == 10 ) {
            istringstream iss( date );
            vector<string> vc;
            copy( istream_iterator<string>( iss ), istream_iterator<string>(), back_inserter<vector<string> >( vc ) );
            disc.convert( atoi( vc[0].c_str() ), atoi( vc[1].c_str() ), atoi( vc[2].c_str() ) );
            cout << "\n\n\n";
        } else cout << "\nIs this a date?!\n\n";
    }
    return 0;
}

```

```txt

Enter a date (dd mm yyyy) or 0 to quit: 19 10 2015
Boomtime, Bureaucracy 73rd, Year of Our Lady of Discord 3181

Enter a date (dd mm yyyy) or 0 to quit: 08 06 2015
Prickle-Prickle, Confusion 13th, Year of Our Lady of Discord 3181

Enter a date (dd mm yyyy) or 0 to quit: 23 06 2015
Prickle-Prickle, Confusion 28th, Year of Our Lady of Discord 3181

Enter a date (dd mm yyyy) or 0 to quit: 29 02 2016
St. Tib's Day, Year of Our Lady of Discord 3182

Enter a date (dd mm yyyy) or 0 to quit: 05 01 2016
Setting Orange, Chaos 5th, Year of Our Lady of Discord 3182 - Mungday

Enter a date (dd mm yyyy) or 0 to quit: 26 08 2016
Pungenday, Bureaucracy 19th, Year of Our Lady of Discord 3182

```



## C#


```c#
using System;

public static class DiscordianDate
{
    static readonly string[] seasons = { "Chaos", "Discord", "Confusion", "Bureaucracy", "The Aftermath" };
    static readonly string[] weekdays = { "Sweetmorn", "Boomtime", "Pungenday", "Prickle-Prickle", "Setting Orange" };
    static readonly string[] apostles = { "Mungday", "Mojoday", "Syaday", "Zaraday", "Maladay" };
    static readonly string[] holidays = { "Chaoflux", "Discoflux", "Confuflux", "Bureflux", "Afflux" };

    public static string Discordian(this DateTime date) {
        string yold = $" in the YOLD {date.Year + 1166}.";
        int dayOfYear = date.DayOfYear;

        if (DateTime.IsLeapYear(date.Year)) {
            if (dayOfYear == 60) return "St. Tib's day" + yold;
            else if (dayOfYear > 60) dayOfYear--;
        }
        dayOfYear--;

        int seasonDay = dayOfYear % 73 + 1;
        int seasonNr = dayOfYear / 73;
        int weekdayNr = dayOfYear % 5;
        string holyday = "";

        if (seasonDay == 5)       holyday = $" Celebrate {apostles[seasonNr]}!";
        else if (seasonDay == 50) holyday = $" Celebrate {holidays[seasonNr]}!";
        return $"{weekdays[weekdayNr]}, day {seasonDay} of {seasons[seasonNr]}{yold}{holyday}";
    }

    public static void Main() {
        foreach (var (day, month, year) in new [] {
            (1, 1, 2010),
            (5, 1, 2010),
            (19, 2, 2011),
            (28, 2, 2012),
            (29, 2, 2012),
            (1, 3, 2012),
            (19, 3, 2013),
            (3, 5, 2014),
            (31, 5, 2015),
            (22, 6, 2016),
            (15, 7, 2016),
            (12, 8, 2017),
            (19, 9, 2018),
            (26, 9, 2018),
            (24, 10, 2019),
            (8, 12, 2020),
            (31, 12, 2020)
        })
        {
            Console.WriteLine($"{day:00}-{month:00}-{year:00} = {new DateTime(year, month, day).Discordian()}");
        }
    }

}
```

```txt

01-01-2010 = Sweetmorn, day 1 of Chaos in the YOLD 3176.
05-01-2010 = Setting Orange, day 5 of Chaos in the YOLD 3176. Celebrate Mungday!
19-02-2011 = Setting Orange, day 50 of Chaos in the YOLD 3177. Celebrate Chaoflux!
28-02-2012 = Prickle-Prickle, day 59 of Chaos in the YOLD 3178.
29-02-2012 = St. Tib's day in the YOLD 3178.
01-03-2012 = Setting Orange, day 60 of Chaos in the YOLD 3178.
19-03-2013 = Pungenday, day 5 of Discord in the YOLD 3179. Celebrate Mojoday!
03-05-2014 = Pungenday, day 50 of Discord in the YOLD 3180. Celebrate Discoflux!
31-05-2015 = Sweetmorn, day 5 of Confusion in the YOLD 3181. Celebrate Syaday!
22-06-2016 = Pungenday, day 27 of Confusion in the YOLD 3182.
15-07-2016 = Sweetmorn, day 50 of Confusion in the YOLD 3182. Celebrate Confuflux!
12-08-2017 = Prickle-Prickle, day 5 of Bureaucracy in the YOLD 3183. Celebrate Zaraday!
19-09-2018 = Boomtime, day 43 of Bureaucracy in the YOLD 3184.
26-09-2018 = Prickle-Prickle, day 50 of Bureaucracy in the YOLD 3184. Celebrate Bureflux!
24-10-2019 = Boomtime, day 5 of The Aftermath in the YOLD 3185. Celebrate Maladay!
08-12-2020 = Boomtime, day 50 of The Aftermath in the YOLD 3186. Celebrate Afflux!
31-12-2020 = Setting Orange, day 73 of The Aftermath in the YOLD 3186.
```



## Clojure


```clojure
(require '[clj-time.core :as tc])

(def seasons ["Chaos" "Discord" "Confusion" "Bureaucracy" "The Aftermath"])
(def weekdays ["Sweetmorn" "Boomtime" "Pungenday" "Prickle-Prickle" "Setting Orange"])
(def year-offset 1166)

(defn leap-year? [year]
  (= 29 (tc/number-of-days-in-the-month year 2)))

(defn discordian-day [day leap]
  (let [offset (if (and leap (>= day 59)) 1 0)
        day-off (- day offset)
        day-num (inc (rem day-off 73))
        season (seasons (quot day-off 73))
        weekday (weekdays (mod day-off 5))]
    (if (and (= day 59) (= offset 1))
      "St. Tib's Day"
      (str weekday ", " season " " day-num))))

(defn discordian-date [year month day]
  (let [day-of-year (dec (.getDayOfYear (tc/date-time year month day)))
        dday (discordian-day day-of-year (leap-year? year))]
    (format "%s, YOLD %s" dday (+ year year-offset))))
```


```txt
user=> (discordian-date 2010 7 22)
"Pungenday, Confusion 57, YOLD 3176"
user=> (discordian-date 2012 2 28)
"Prickle-Prickle, Chaos 59, YOLD 3178"
user=> (discordian-date 2012 2 29)
"St. Tib's Day, YOLD 3178"
user=> (discordian-date 2012 3 1)
"Setting Orange, Chaos 60, YOLD 3178"
user=> (discordian-date 2012 12 31)
"Setting Orange, The Aftermath 73, YOLD 3178"
user=> (discordian-date 2013 12 31)
"Setting Orange, The Aftermath 73, YOLD 3179"
```



## D



```d
import std.stdio, std.datetime, std.conv, std.string;

immutable seasons = ["Chaos", "Discord", "Confusion",
                     "Bureaucracy", "The Aftermath"],
          weekday = ["Sweetmorn", "Boomtime", "Pungenday",
                     "Prickle-Prickle", "Setting Orange"],
          apostle = ["Mungday", "Mojoday", "Syaday",
                     "Zaraday", "Maladay"],
          holiday = ["Chaoflux", "Discoflux", "Confuflux",
                     "Bureflux", "Afflux"];

string discordianDate(in Date date) pure {
    immutable dYear = text(date.year + 1166);

    immutable isLeapYear = date.isLeapYear;
    if (isLeapYear && date.month == 2 && date.day == 29)
        return "St. Tib's Day, in the YOLD " ~ dYear;

    immutable doy = (isLeapYear && date.dayOfYear >= 60) ?
                    date.dayOfYear - 1 :
                    date.dayOfYear;

    immutable dsDay = (doy % 73)==0? 73:(doy % 73); // Season day.
    if (dsDay == 5)
        return apostle[doy / 73] ~ ", in the YOLD " ~ dYear;
    if (dsDay == 50)
        return holiday[doy / 73] ~ ", in the YOLD " ~ dYear;

    immutable dSeas = seasons[(((doy%73)==0)?doy-1:doy) / 73];
    immutable dWday = weekday[(doy - 1) % 5];

    return format("%s, day %s of %s in the YOLD %s",
                  dWday, dsDay, dSeas, dYear);
}

unittest {
    assert(Date(2010, 7, 22).discordianDate ==
           "Pungenday, day 57 of Confusion in the YOLD 3176");
    assert(Date(2012, 2, 28).discordianDate ==
           "Prickle-Prickle, day 59 of Chaos in the YOLD 3178");
    assert(Date(2012, 2, 29).discordianDate ==
           "St. Tib's Day, in the YOLD 3178");
    assert(Date(2012, 3, 1).discordianDate ==
           "Setting Orange, day 60 of Chaos in the YOLD 3178");
    assert(Date(2010, 1, 5).discordianDate ==
           "Mungday, in the YOLD 3176");
    assert(Date(2011, 5, 3).discordianDate ==
           "Discoflux, in the YOLD 3177");
}

void main(string args[]) {
    int yyyymmdd, day, mon, year, sign;
    if (args.length == 1) {
       (cast(Date)Clock.currTime).discordianDate.writeln;
       return;
    }
    foreach (i, arg; args) {
        if (i > 0) {
            //writef("%d: %s: ", i, arg);
            yyyymmdd = to!int(arg);
            if (yyyymmdd < 0) {
                sign = -1;
                yyyymmdd = -yyyymmdd;
            }
            else {
                sign = 1;
            }
            day = yyyymmdd % 100;
            if (day == 0) {
               day = 1;
            }
            mon = ((yyyymmdd - day) / 100) % 100;
            if (mon == 0) {
               mon = 1;
            }
            year = sign * ((yyyymmdd - day - 100*mon) / 10000);
            writefln("%s", Date(year, mon, day).discordianDate);
        }
    }
}
```

```txt
$ ./ddate 20100722 20120228 20120229 20120301 20100105 20110503 20151019 00000101 -11660101
Pungenday, day 57 of Confusion in the YOLD 3176
Prickle-Prickle, day 59 of Chaos in the YOLD 3178
St. Tib's Day, in the YOLD 3178
Setting Orange, day 60 of Chaos in the YOLD 3178
Mungday, in the YOLD 3176
Discoflux, in the YOLD 3177
Boomtime, day 73 of Bureaucracy in the YOLD 3181
Sweetmorn, day 1 of Chaos in the YOLD 1166
Sweetmorn, day 1 of Chaos in the YOLD 0
```



## Euphoria

```euphoria
function isLeapYear(integer year)
    return remainder(year,4)=0 and remainder(year,100)!=0 or remainder(year,400)=0
end function

constant YEAR = 1, MONTH = 2, DAY = 3, DAY_OF_YEAR = 8

constant month_lengths = {31,28,31,30,31,30,31,31,30,31,30,31}
function dayOfYear(sequence Date)
    integer d
    if length(Date) = DAY_OF_YEAR then
        d = Date[DAY_OF_YEAR]
    else
        d = Date[DAY]
        for i = Date[MONTH]-1 to 1 by -1 do
            d += month_lengths[i]
            if i = 2 and isLeapYear(Date[YEAR]) then
                d += 1
            end if
        end for
    end if
    return d
end function

constant seasons = {"Chaos", "Discord", "Confusion", "Bureaucracy", "The Aftermath"}
constant weekday = {"Sweetmorn", "Boomtime", "Pungenday", "Prickle-Prickle", "Setting Orange"}
constant apostle = {"Mungday", "Mojoday", "Syaday", "Zaraday", "Maladay"}
constant holiday = {"Chaoflux", "Discoflux", "Confuflux", "Bureflux", "Afflux"}

function discordianDate(sequence Date)
    sequence dyear, dseas, dwday
    integer  leap, doy, dsday
    dyear = sprintf("%d",Date[YEAR]+1166)
    leap = isLeapYear(Date[YEAR])
    if leap and Date[MONTH] = 2 and Date[DAY] = 29 then
        return "St. Tib's Day, in the YOLD " & dyear
    end if

    doy = dayOfYear(Date)
    if leap and doy >= 60 then
        doy -= 1
    end if

    dsday = remainder(doy,73)
    if dsday = 5 then
        return apostle[doy/73+1] & ", in the YOLD " & dyear
    elsif dsday = 50 then
        return holiday[doy/73+1] & ", in the YOLD " & dyear
    end if

    dseas = seasons[doy/73+1]
    dwday = weekday[remainder(doy-1,5)+1]

    return sprintf("%s, day %d of %s in the YOLD %s", {dwday, dsday, dseas, dyear})
end function

sequence today
today = date()
today[YEAR] += 1900
puts(1, discordianDate(today))
```


=={{header|F Sharp|F#}}==


```fsharp
open System

let seasons = [| "Chaos"; "Discord"; "Confusion"; "Bureaucracy"; "The Aftermath" |]

let ddate (date:DateTime) =
    let dyear = date.Year + 1166
    let leapYear = DateTime.IsLeapYear(date.Year)

    if leapYear && date.Month = 2 && date.Day = 29 then
        sprintf "St. Tib's Day, %i YOLD" dyear
    else
        // compensate for St. Tib's Day
        let dayOfYear = (if leapYear && date.DayOfYear >= 60 then date.DayOfYear - 1 else date.DayOfYear) - 1
        let season, dday = Math.DivRem(dayOfYear, 73)
        sprintf "%s %i, %i YOLD" seasons.[season] (dday+1) dyear

[<EntryPoint>]
let main argv =
    let p = Int32.Parse
    Seq.ofArray("2012-02-28 2012-02-29 2012-03-01 2010-07-22 2015-10-19 2015-10-20".Split())
    |> Seq.map(fun (s:string) ->
        let d = s.Split('-')
        (s, DateTime(p(d.[0]),p(d.[1]),p(d.[2]))))
    |> Seq.iter(fun (s,d) -> printfn "%s is %s" s (ddate d))
    0

```

```txt
2012-02-28 is Chaos 59, 3178 YOLD
2012-02-29 is St. Tib's Day, 3178 YOLD
2012-03-01 is Chaos 60, 3178 YOLD
2010-07-22 is Confusion 57, 3176 YOLD
2015-10-19 is Bureaucracy 73, 3181 YOLD
2015-10-20 is The Aftermath 1, 3181 YOLD
```



## FORTRAN


```FORTRAN

program discordianDate
    implicit none
    ! Declare variables
    character(32) :: arg
    character(15) :: season,day,holyday
    character(80) :: Output,fmt1,fmt2,fmt3
    character(2) :: dayfix,f1,f2,f3,f4
    integer :: i,j,k,daysofyear,dayofweek,seasonnum,yold,dayofseason,t1,t2,t3
    integer,dimension(8) :: values
    integer, dimension(12) :: daysinmonth
    logical  :: isleapyear, isholyday, Pleapyear

    ! Get the current date
    call date_and_time(VALUES=values)
    ! Set some values up to defaults
    daysinmonth = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)
    isleapyear = .false.
    isholyday = .false.
    ! process any command line arguments
    ! using arguments dd mm yyyy
    j = iargc()
    do i = 1, iargc()
      call getarg(i, arg)  ! fetches argument as a character string
      if (j==3) then
        if (i==1) then
          read(arg,'(i2)') values(3)  ! convert to integer
        endif
        if (i==2) then
          read(arg,'(i2)') values(2)  ! convert to integer
        endif
        if (i==3) then
          read(arg,'(i4)') values(1)  ! convert to integer

        endif
      endif
      if (j==2) then  ! arguments dd mm
        if (i==1) then
          read(arg,'(i2)') values(3)  ! convert to integer
        endif
        if (i==2) then
          read(arg,'(i2)') values(2)  ! convert to integer
        endif
      endif
      if (j==1) then ! argument dd
        read(arg,'(i2)') values(3)  ! convert to integer
      endif
    end do

    !Start the number crunching here

    yold = values(1) + 1166
    daysofyear = 0
    if (values(2)>1) then
    do i=1 , values(2)-1 , 1
        daysofyear = daysofyear + daysinmonth(i)
     end do
   end if
    daysofyear = daysofyear + values(3)
    isholyday = .false.
    isleapyear = Pleapyear(yold)
    dayofweek = mod (daysofyear, 5)
    seasonnum = ((daysofyear - 1) / 73) + 1
    dayofseason = daysofyear - ((seasonnum - 1)  * 73)
    k = mod(dayofseason,10)  ! just to get the day number postfix
    select case (k)
      case (1)
         dayfix='st'
      case (2)
         dayfix='nd'
      case (3)
         dayfix='rd'
      case default
         dayfix='th'
    end select
    ! except between 10th and 20th  where we always have 'th'
    if (((dayofseason > 10) .and. (dayofseason < 20)) .eqv. .true.) then
      dayfix = 'th'
    end if
    select case (Seasonnum)
      case (1)
        season ='Choas'
        f4 = '5'
      case (2)
        season ='Discord'
        f4 = '7'
      case (3)
        season ='Confusion'
        f4 = '9'
      case (4)
        season ='Bureaucracy'
        f4 = '10'
      case (5)
        season ='The Aftermath'
        f4 = '13'
    end select
    select case (dayofweek)
       case (0)
         day='Setting Orange'
         f2 = '14'
       case (1)
         day ='Sweetmorn'
         f2 = '9'
       case (2)
         day = 'Boomtime'
         f2 = '8'
       case (3)
         day = 'Pungenday'
         f2 = '9'
       case (4)
         day = 'Prickle-Prickle'
         f2 = '15'
    end select
    ! check for holydays
    select case (dayofseason)
      case (5)
        isholyday = .true.
        select case (seasonnum)
          case (1)
             holyday ='Mungday'
             f1 = '7'
          case (2)
             holyday = 'Mojoday'
             f1 = '7'
          case (3)
            holyday = 'Syaday'
            f1 = '6'
          case (4)
             holyday = 'Zaraday'
             f1 = '7'
          case (5)
            holyday = 'Maladay'
            f1 = '7'
         end select
      Case (50)
        isholyday = .true.
        select case (seasonnum)
          case (1)
            holyday = 'Chaoflux'
            f1 = '8'
          case (2)
            holyday = 'Discoflux'
            f1 = '9'
          case (3)
            holyday = 'Confuflux'
            f1 = '9'
          case (4)
            holyday = 'Bureflux'
            f1 = '8'
          case (5)
            holyday = 'Afflux'
            f1 = '6'
         end select
    end select


    ! Check if it is St. Tibbs day
    if (isleapyear .eqv. .true.) then
      if ((values(2) == 2) .and. (values(3) == 29)) then
         isholyday = .true.
      end if
    end if
    ! Construct our format strings
    f3 = "2"
    if (dayofseason < 10) then
      f3 = "1"
    end if
    fmt1 = "(a,i4)"
    fmt2 = "(A,a" // f1 // ",A,A" // f2 // ",A,I" // f3 // ",A2,A,A" // f4 // ",A,I4)"
    fmt3 = "(A,A" // f2 // ",A,I" // f3 //",A2,A,A" // f4 // ",A,I4)"
    ! print an appropriate line
    if (isholyday .eqv. .true.) then
      if (values(3) == 29) then
         print fmt1,'Celebrate for today is St. Tibbs Day in the YOLD ',yold
       else
         print fmt2, 'Today is ',holyday, ' on ',day,' the ',dayofseason,dayfix,' day of ',season,' in the YOLD ',yold
       end if
     else   ! not a holyday
         print fmt3, 'Today is ',day,' the ',dayofseason,dayfix, ' day of ',season,' in the YOLD ',yold
     end if
    end program discordianDate

    ! Function to check to see if this is a leap year returns true or false!

    function Pleapyear(dloy) result(leaper)
    implicit none
    integer, intent(in) :: dloy
    logical :: leaper
    leaper = .false.
    if (mod((dloy-1166),4) == 0)  then
      leaper = .true.
    end if
    if (mod((dloy-1166),100) == 0) then
        leaper = .false.
        if (mod((dloy-1166),400)==0) then
           leaper = .true.
        end if
    end if
    end function Pleapyear

```


```txt

compiled as ddate.
useage ./ddate
       ./ddate dd mm yyyy

```

```txt

./ddate
Today is Pungenday the 56th day of The Aftermath in the YOLD 3184
./ddate 29 02 2020
Celebrate for today is St. Tibbs Day in the YOLD 3186

```



## Go

A package modeled after the time package in the Go standard library

```go
package ddate

import (
    "strconv"
    "strings"
    "time"
)

// Predefined formats for DiscDate.Format
const (
    DefaultFmt = "Pungenday, Discord 5, 3131 YOLD"
    OldFmt     = `Today is Pungenday, the 5th day of Discord in the YOLD 3131
Celebrate Mojoday`
)

// Formats passed to DiscDate.Format are protypes for formated dates.
// Format replaces occurrences of prototype elements (the constant strings
// listed here) with values corresponding to the date being formatted.
// If the date is St. Tib's Day, the string from the first date element
// through the last is replaced with "St. Tib's Day".
const (
    protoLongSeason  = "Discord"
    protoShortSeason = "Dsc"
    protoLongDay     = "Pungenday"
    protoShortDay    = "PD"
    protoOrdDay      = "5"
    protoCardDay     = "5th"
    protoHolyday     = "Mojoday"
    protoYear        = "3131"
)

var (
    longDay = []string{"Sweetmorn", "Boomtime", "Pungenday",
        "Prickle-Prickle", "Setting Orange"}
    shortDay   = []string{"SM", "BT", "PD", "PP", "SO"}
    longSeason = []string{
        "Chaos", "Discord", "Confusion", "Bureaucracy", "The Aftermath"}
    shortSeason = []string{"Chs", "Dsc", "Cfn", "Bcy", "Afm"}
    holyday     = [][]string{{"Mungday", "Chaoflux"}, {"Mojoday", "Discoflux"},
        {"Syaday", "Confuflux"}, {"Zaraday", "Bureflux"}, {"Maladay", "Afflux"}}
)

type DiscDate struct {
    StTibs bool
    Dayy   int // zero based day of year, meaningless if StTibs is true
    Year   int // gregorian + 1166
}

func New(eris time.Time) DiscDate {
    t := time.Date(eris.Year(), 1, 1, eris.Hour(), eris.Minute(),
        eris.Second(), eris.Nanosecond(), eris.Location())
    bob := int(eris.Sub(t).Hours()) / 24
    raw := eris.Year()
    hastur := DiscDate{Year: raw + 1166}
    if raw%4 == 0 && (raw%100 != 0 || raw%400 == 0) {
        if bob > 59 {
            bob--
        } else if bob == 59 {
            hastur.StTibs = true
            return hastur
        }
    }
    hastur.Dayy = bob
    return hastur
}

func (dd DiscDate) Format(f string) (r string) {
    var st, snarf string
    var dateElement bool
    f6 := func(proto, wibble string) {
        if !dateElement {
            snarf = r
            dateElement = true
        }
        if st > "" {
            r = ""
        } else {
            r += wibble
        }
        f = f[len(proto):]
    }
    f4 := func(proto, wibble string) {
        if dd.StTibs {
            st = "St. Tib's Day"
        }
        f6(proto, wibble)
    }
    season, day := dd.Dayy/73, dd.Dayy%73
    for f > "" {
        switch {
        case strings.HasPrefix(f, protoLongDay):
            f4(protoLongDay, longDay[dd.Dayy%5])
        case strings.HasPrefix(f, protoShortDay):
            f4(protoShortDay, shortDay[dd.Dayy%5])
        case strings.HasPrefix(f, protoCardDay):
            funkychickens := "th"
            if day/10 != 1 {
                switch day % 10 {
                case 0:
                    funkychickens = "st"
                case 1:
                    funkychickens = "nd"
                case 2:
                    funkychickens = "rd"
                }
            }
            f4(protoCardDay, strconv.Itoa(day+1)+funkychickens)
        case strings.HasPrefix(f, protoOrdDay):
            f4(protoOrdDay, strconv.Itoa(day+1))
        case strings.HasPrefix(f, protoLongSeason):
            f6(protoLongSeason, longSeason[season])
        case strings.HasPrefix(f, protoShortSeason):
            f6(protoShortSeason, shortSeason[season])
        case strings.HasPrefix(f, protoHolyday):
            if day == 4 {
                r += holyday[season][0]
            } else if day == 49 {
                r += holyday[season][1]
            }
            f = f[len(protoHolyday):]
        case strings.HasPrefix(f, protoYear):
            r += strconv.Itoa(dd.Year)
            f = f[4:]
        default:
            r += f[:1]
            f = f[1:]
        }
    }
    if st > "" {
        r = snarf + st + r
    }
    return
}
```

Example program using above package

```go
package main

import (
    "ddate"
    "fmt"
    "os"
    "strings"
    "time"
)

func main() {
    pi := 1
    fnord := ddate.DefaultFmt
    if len(os.Args) > 1 {
        switch os.Args[1][0] {
        case '+':
            pi++
            fnord = os.Args[1][1:]
        case '-':
            usage()
        }
    }
    var eris time.Time
    switch len(os.Args) - pi {
    case 0:
        eris = time.Now()
    case 3:
        var err error
        eris, err = time.Parse("2 1 2006", strings.Join(os.Args[pi:pi+3], " "))
        if err != nil {
            fmt.Println(err)
            usage()
        }
    default:
        usage()
    }
    fmt.Println(ddate.New(eris).Format(fnord))
}

func usage() {
    fmt.Fprintf(os.Stderr, "usage: %s [+format] [day month year]\n", os.Args[0])
    os.Exit(1)
}
```

```txt

> ddate
Pungenday, Confusion 62, 3177 YOLD

> ddate 29 2 2012
St. Tib's Day, 3178 YOLD

```



## Haskell


```haskell
import Data.Time (isLeapYear)
import Data.Time.Calendar.MonthDay (monthAndDayToDayOfYear)
import Text.Printf (printf)

type Year = Integer
type Day = Int
type Month = Int

data DDate = DDate Weekday Season Day Year
           | StTibsDay Year deriving (Eq, Ord)

data Season = Chaos
            | Discord
            | Confusion
            | Bureaucracy
            | TheAftermath
            deriving (Show, Enum, Eq, Ord, Bounded)

data Weekday = Sweetmorn
             | Boomtime
             | Pungenday
             | PricklePrickle
             | SettingOrange
             deriving (Show, Enum, Eq, Ord, Bounded)

instance Show DDate where
  show (StTibsDay y) = printf "St. Tib's Day, %d YOLD" y
  show (DDate w s d y) = printf "%s, %s %d, %d YOLD" (show w) (show s) d y

fromYMD :: (Year, Month, Day) -> DDate
fromYMD (y, m, d)
  | leap && dayOfYear == 59 = StTibsDay yold
  | leap && dayOfYear >= 60 = mkDDate $ dayOfYear - 1
  | otherwise               = mkDDate dayOfYear
  where
    yold = y + 1166
    dayOfYear = monthAndDayToDayOfYear leap m d - 1
    leap = isLeapYear y

    mkDDate dayOfYear = DDate weekday season dayOfSeason yold
      where
        weekday = toEnum $ dayOfYear `mod` 5
        season = toEnum $ dayOfYear `div` 73
        dayOfSeason = 1 + dayOfYear `mod` 73
```


Examples:


```haskell
test = mapM_ display dates
  where
    display d = putStr (show d ++ " -> ") >> print (fromYMD d)
    dates = [(2012,2,28)
            ,(2012,2,29)
            ,(2012,3,1)
            ,(2012,3,14)
            ,(2012,3,15)
            ,(2010,9,2)
            ,(2010,12,31)
            ,(2011,1,1)]
```



```txt
λ> test
(2012,2,28) -> PricklePrickle, Chaos 59, 3178 YOLD
(2012,2,29) -> St. Tib's Day, 3178 YOLD
(2012,3,1) -> SettingOrange, Chaos 60, 3178 YOLD
(2012,3,14) -> Pungenday, Chaos 73, 3178 YOLD
(2012,3,15) -> PricklePrickle, Discord 1, 3178 YOLD
(2010,9,2) -> SettingOrange, Bureaucracy 26, 3176 YOLD
(2010,12,31) -> SettingOrange, TheAftermath 73, 3176 YOLD
(2011,1,1) -> Sweetmorn, Chaos 1, 3177 YOLD
```


In GHCi we can also execute shell commands.
* Using Linux utility ddate

```txt
*Main> :! ddate
Today is Setting Orange, the 26th day of Bureaucracy in the YOLD 3176

*Main> :! ddate 29 2 2012
Sint Tibs

*Main> :! ddate 2 9 2010
Setting Orange, Bureaucracy 26, 3176 YOLD
```


=={{header|Icon}} and {{header|Unicon}}==
This version is loosely based on a modified translation of the original ddate.c and like the original the leap year functionality is Julian not Gregorian.

```Icon
link printf

procedure main()
   Demo(2010,1,1)
   Demo(2010,7,22)
   Demo(2012,2,28)
   Demo(2012,2,29)
   Demo(2012,3,1)
   Demo(2010,1,5)
   Demo(2011,5,3)
   Demo(2012,2,28)
   Demo(2012,2,29)
   Demo(2012,3,1)
   Demo(2010,7,22)
   Demo(2012,12,22)
end

procedure Demo(y,m,d)      #: demo display
   printf("%i-%i-%i = %s\n",y,m,d,DiscordianDateString(DiscordianDate(y,m,d)))
end

record DiscordianDateRecord(year,yday,season,sday,holiday)

procedure DiscordianDate(year,month,day)  #: Convert normal date to Discordian
static  cal
initial cal := [31,28,31,30,31,30,31,31,30,31,30,31]

   ddate := DiscordianDateRecord(year+1166)
   every (ddate.yday := day - 1) +:= cal[1 to month-1]   # zero origin
   ddate.sday := ddate.yday

   if ddate.year % 4 = 2 &  month = 2 & day = 29 then
      ddate.holiday := 1   # Note: st tibs is outside of weekdays
   else {
      ddate.season  := (ddate.yday / 73) + 1
      ddate.sday := (ddate.yday % 73) + 1
      ddate.holiday := 1 + ddate.season * case ddate.sday of { 5 : 1; 50 : 2}
   }
   return ddate
end

procedure DiscordianDateString(ddate)   #: format a Discordian Date String
static days,seasons,holidays
initial {
   days := ["Sweetmorn","Boomtime","Pungenday","Prickle-Prickle","Setting Orange"]
   seasons := ["Chaos","Discord","Confusion","Bureaucracy","The Aftermath"]
   holidays := ["St. Tib's Day","Mungday","Chaoflux","Mojoday","Discoflux",
                "Syaday","Confuflux","Zaraday","Bureflux","Maladay","Afflux"]
   }

   return (( holidays[\ddate.holiday] || "," ) |
           ( days[1+ddate.yday%5] || ", day " ||
             ddate.sday || " of " || seasons[ddate.season])) ||
          " in the YOLD " || ddate.year
end
```


```txt
2010-1-1 = Sweetmorn, day 1 of Chaos in the YOLD 3176
2010-7-22 = Pungenday, day 57 of Confusion in the YOLD 3176
2012-2-28 = Prickle-Prickle, day 59 of Chaos in the YOLD 3178
2012-2-29 = St. Tib's Day, in the YOLD 3178
2012-3-1 = Setting Orange, day 60 of Chaos in the YOLD 3178
2010-1-5 = Mungday, in the YOLD 3176
2011-5-3 = Discoflux, in the YOLD 3177
2012-2-28 = Prickle-Prickle, day 59 of Chaos in the YOLD 3178
2012-2-29 = St. Tib's Day, in the YOLD 3178
2012-3-1 = Setting Orange, day 60 of Chaos in the YOLD 3178
2010-7-22 = Pungenday, day 57 of Confusion in the YOLD 3176
2012-12-22 = Sweetmorn, day 64 of The Aftermath in the YOLD 3178
```



## J


```j
require'dates'
leap=: _1j1 * 0 -/@:= 4 100 400 |/ {.@]
bs=: ((#:{.) + 0 j. *@[ * {:@]) +.
disc=: ((1+0 73 bs[ +^:(58<]) -/@todayno@(,: 1 1,~{.)@]) ,~1166+{.@])~ leap
```


Example use:

<lang>   disc 2012 2 28
3178 1 59
   disc 2012 2 29
3178 1 59j1
   disc 2012 3 1
3178 1 60j1
   disc 2012 12 31
3178 5 73j1
   disc 2013 1 1
3179 1 1
   disc 2100 12 31
3266 5 73
   disc 2015 10 19
3181 4 73
   disc 2000 3 13
3166 1 72j1
```


see [[Talk:Discordian_date|talk page]].  But, in essence, this version uses season ordinals with a single imaginary day after the 59th of the first season, on leap years.  This is implemented so that that imaginary day has been passed for all remaining days of a leap year.


## Java



```java
import java.util.Calendar;
import java.util.GregorianCalendar;

public class DiscordianDate {
    final static String[] seasons = {"Chaos", "Discord", "Confusion",
        "Bureaucracy", "The Aftermath"};

    final static String[] weekday = {"Sweetmorn", "Boomtime", "Pungenday",
        "Prickle-Prickle", "Setting Orange"};

    final static String[] apostle = {"Mungday", "Mojoday", "Syaday",
        "Zaraday", "Maladay"};

    final static String[] holiday = {"Chaoflux", "Discoflux", "Confuflux",
        "Bureflux", "Afflux"};

    public static String discordianDate(final GregorianCalendar date) {
        int y = date.get(Calendar.YEAR);
        int yold = y + 1166;
        int dayOfYear = date.get(Calendar.DAY_OF_YEAR);

        if (date.isLeapYear(y)) {
            if (dayOfYear == 60)
                return "St. Tib's Day, in the YOLD " + yold;
            else if (dayOfYear > 60)
                dayOfYear--;
        }

        dayOfYear--;

        int seasonDay = dayOfYear % 73 + 1;
        if (seasonDay == 5)
            return apostle[dayOfYear / 73] + ", in the YOLD " + yold;
        if (seasonDay == 50)
            return holiday[dayOfYear / 73] + ", in the YOLD " + yold;

        String season = seasons[dayOfYear / 73];
        String dayOfWeek = weekday[dayOfYear % 5];

        return String.format("%s, day %s of %s in the YOLD %s",
                dayOfWeek, seasonDay, season, yold);
    }

    public static void main(String[] args) {

        System.out.println(discordianDate(new GregorianCalendar()));

        test(2010, 6, 22, "Pungenday, day 57 of Confusion in the YOLD 3176");
        test(2012, 1, 28, "Prickle-Prickle, day 59 of Chaos in the YOLD 3178");
        test(2012, 1, 29, "St. Tib's Day, in the YOLD 3178");
        test(2012, 2, 1, "Setting Orange, day 60 of Chaos in the YOLD 3178");
        test(2010, 0, 5, "Mungday, in the YOLD 3176");
        test(2011, 4, 3, "Discoflux, in the YOLD 3177");
        test(2015, 9, 19, "Boomtime, day 73 of Bureaucracy in the YOLD 3181");
    }

    private static void test(int y, int m, int d, final String result) {
        assert (discordianDate(new GregorianCalendar(y, m, d)).equals(result));
    }
}
```


```txt
Setting Orange, day 4 of Confusion in the YOLD 3180
```



## JavaScript



```javascript

/**
 * All Hail Discordia! - this script prints Discordian date using system date.
 *
 * lang: JavaScript
 * author: jklu
 * contributors: JamesMcGuigan
 *
 * changelog:
 * - Modified to return same output syntax as unix ddate + module.exports - James McGuigan, 2/Chaos/3183
 *
 * source: https://rosettacode.org/wiki/Discordian_date#JavaScript
 */
var seasons = [
  "Chaos", "Discord", "Confusion",
  "Bureaucracy", "The Aftermath"
];
var weekday = [
  "Sweetmorn", "Boomtime", "Pungenday",
  "Prickle-Prickle", "Setting Orange"
];

var apostle = [
  "Mungday", "Mojoday", "Syaday",
  "Zaraday", "Maladay"
];

var holiday = [
  "Chaoflux", "Discoflux", "Confuflux",
  "Bureflux", "Afflux"
];


Date.prototype.isLeapYear = function() {
  var year = this.getFullYear();
  if( (year & 3) !== 0 ) { return false; }
  return ((year % 100) !== 0 || (year % 400) === 0);
};

// Get Day of Year
Date.prototype.getDOY = function() {
  var dayCount  = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334];
  var mn        = this.getMonth();
  var dn        = this.getDate();
  var dayOfYear = dayCount[mn] + dn;
  if( mn > 1 && this.isLeapYear() ) { dayOfYear++; }
  return dayOfYear;
};

Date.prototype.isToday = function() {
  var today = new Date();
  return this.getDate()     === today.getDate()
      && this.getMonth()    === today.getMonth()
      && this.getFullYear() === today.getFullYear()
  ;
};

function discordianDate(date) {
  if( !date ) { date = new Date(); }

  var y                = date.getFullYear();
  var yold             = y + 1166;
  var dayOfYear        = date.getDOY();
  var celebrateHoliday = null;

  if( date.isLeapYear() ) {
    if( dayOfYear == 60 ) {
      celebrateHoliday = "St. Tib's Day";
    }
    else if( dayOfYear > 60 ) {
      dayOfYear--;
    }
  }
  dayOfYear--;

  var divDay = Math.floor(dayOfYear / 73);

  var seasonDay = (dayOfYear % 73) + 1;
  if( seasonDay == 5 ) {
    celebrateHoliday = apostle[divDay];
  }
  if( seasonDay == 50 ) {
    celebrateHoliday = holiday[divDay];
  }

  var season    = seasons[divDay];
  var dayOfWeek = weekday[dayOfYear % 5];

  var nth = (seasonDay % 10 == 1) ? 'st'
          : (seasonDay % 10 == 2) ? 'nd'
          : (seasonDay % 10 == 3) ? 'rd'
                                  : 'th';

  return "" //(date.isToday() ? "Today is " : '')
         + dayOfWeek
         + ", the " + seasonDay + nth
         + " day of " + season
         + " in the YOLD " + yold
         + (celebrateHoliday ? ". Celebrate " + celebrateHoliday + "!" : '')
    ;
}

function test(y, m, d, result) {
  console.assert((discordianDate(new Date(y, m, d)) == result), [y, m, d, discordianDate(new Date(y, m, d)), result]);
}

// Only run test code if node calls this file directly
if( require.main === module ) {
  console.log(discordianDate(new Date(Date.now())));
  test(2010, 6, 22, "Pungenday, the 57th day of Confusion in the YOLD 3176");
  test(2012, 1, 28, "Prickle-Prickle, the 59th day of Chaos in the YOLD 3178");
  test(2012, 1, 29, "Setting Orange, the 60th day of Chaos in the YOLD 3178. Celebrate St. Tib's Day!");
  test(2012, 2,  1, "Setting Orange, the 60th day of Chaos in the YOLD 3178");
  test(2010, 0,  5, "Setting Orange, the 5th day of Chaos in the YOLD 3176. Celebrate Mungday!");
  test(2011, 4,  3, "Pungenday, the 50th day of Discord in the YOLD 3177. Celebrate Discoflux!");
  test(2015, 9, 19, "Boomtime, the 73rd day of Bureaucracy in the YOLD 3181");
}

module.exports = discordianDate;

```


Example use:


```javascript

console.log(discordianDate(new Date(Date.now())));
"Boomtime, the 2nd day of Chaos in the YOLD 3183"

```


look at [http://www.cs.cmu.edu/~tilt/principia/body.html#applecorps calendar];
learn about [http://jubal.westnet.com/hyperdiscordia/discordian_holydays.html holydays]


## JotaCode


```jotacode
@print(
  "Today is ",
  @let(1,@add(@switch(@time("mon"),
    0,-1,
    1,30,
    2,58,
    3,89,
    4,119,
    5,150,
    6,180,
    7,211,
    8,242,
    9,272,
    10,303,
    11,333)
  ,@time("mday")),@switch(@print(@time("mon"),"/",@time("mday")),
  "1/29","St. Tib's Day",
  @print(@switch(@mod("%1",5),
    0,"Sweetmorn",
    1,"Boomtime",
    2,"Pungenday",
    3,"Prickle-Prickle",
    4,"Setting Orange"),
  ", ",
  @switch(@idiv("%1",73),
    0,"Chaos",
    1,"Discord",
    2,"Confusion",
    3,"Bureaucracy",
    4,"The Aftermath"),
  " ",
  @add(@mod("%1",73),1),
  ", YOLD ",
  @add(@time("year"),3066))
)),".")
```



## Julia


```julia
using Dates

function discordiandate(year::Integer, month::Integer, day::Integer)
    discordianseasons = ["Chaos", "Discord", "Confusion", "Bureaucracy", "The Aftermath"]
    holidays = Dict(
        "Chaos 5" => "Mungday",
        "Chaos 50" => "Chaoflux",
        "Discord 5" => "Mojoday",
        "Discord 50" => "Discoflux",
        "Confusion 5" => "Syaday",
        "Confusion 50" => "Confuflux",
        "Bureaucracy 5" => "Zaraday",
        "Bureaucracy 50" => "Bureflux",
        "The Aftermath 5" => "Maladay",
        "The Aftermath 50" => "Afflux")
    today = Date(year, month, day)
    isleap = isleapyear(year)
    if isleap && month == 2 && day == 29
        rst = "St. Tib's Day, YOLD " * string(year + 1166)
    else
        dy = dayofyear(today)
        if isleap && dy >= 60
            dy -= 1
        end
        rst = string(discordianseasons[div(dy, 73) + 1], " ", rem(dy, 73)) # day
        if haskey(holidays, rst)
            rst *= " ($(holidays[rst]))" # if holiday
        end
        rst *= ", YOLD $(year + 1166)" # year
    end
    return rst
end

@show discordiandate(2017, 08, 15)
@show discordiandate(1996, 02, 29)
@show discordiandate(1996, 02, 19)
```


```txt
discordiandate(2017, 8, 15) = "Bureaucracy 8, YOLD 3183"
discordiandate(1996, 2, 29) = "St. Tib's Day, YOLD 3162"
discordiandate(1996, 2, 19) = "Chaos 50 (Chaoflux), YOLD 3162"
```



## Kotlin

```scala
import java.util.Calendar
import java.util.GregorianCalendar

enum class Season {
    Chaos, Discord, Confusion, Bureaucracy, Aftermath;
    companion object { fun from(i: Int) = values()[i / 73] }
}
enum class Weekday {
    Sweetmorn, Boomtime, Pungenday, Prickle_Prickle, Setting_Orange;
    companion object { fun from(i: Int) = values()[i % 5] }
}
enum class Apostle {
    Mungday, Mojoday, Syaday, Zaraday, Maladay;
    companion object { fun from(i: Int) = values()[i / 73] }
}
enum class Holiday {
    Chaoflux, Discoflux, Confuflux, Bureflux, Afflux;
    companion object { fun from(i: Int) = values()[i / 73] }
}

fun GregorianCalendar.discordianDate(): String {
    val y = get(Calendar.YEAR)
    val yold = y + 1166

    var dayOfYear = get(Calendar.DAY_OF_YEAR)
    if (isLeapYear(y)) {
        if (dayOfYear == 60)
            return "St. Tib's Day, in the YOLD " + yold
        else if (dayOfYear > 60)
            dayOfYear--
    }

    val seasonDay = --dayOfYear % 73 + 1
    return when (seasonDay) {
        5 -> "" + Apostle.from(dayOfYear) + ", in the YOLD " + yold
        50 -> "" + Holiday.from(dayOfYear) + ", in the YOLD " + yold
        else -> "" + Weekday.from(dayOfYear) + ", day " + seasonDay + " of " + Season.from(dayOfYear) + " in the YOLD " + yold
    }
}

internal fun test(y: Int, m: Int, d: Int, result: String) {
    assert(GregorianCalendar(y, m, d).discordianDate() == result)
}

fun main(args: Array<String>) {
    println(GregorianCalendar().discordianDate())

    test(2010, 6, 22, "Pungenday, day 57 of Confusion in the YOLD 3176")
    test(2012, 1, 28, "Prickle-Prickle, day 59 of Chaos in the YOLD 3178")
    test(2012, 1, 29, "St. Tib's Day, in the YOLD 3178")
    test(2012, 2, 1, "Setting Orange, day 60 of Chaos in the YOLD 3178")
    test(2010, 0, 5, "Mungday, in the YOLD 3176")
    test(2011, 4, 3, "Discoflux, in the YOLD 3177")
    test(2015, 9, 19, "Boomtime, day 73 of Bureaucracy in the YOLD 3181")
}
```



## Maple


```maple
convertDiscordian := proc(year, month, day)
	local days31, days30, daysThisYear, i, dYear, dMonth, dDay, seasons, week, dayOfWeek;
	days31 := [1, 3, 5, 7, 8, 10, 12];
	days30 := [4, 6, 9, 11];
	if month < 1 or month >12 then
		error "Invalid month: %1", month;
	end if;
	if (member(month, days31) and day > 31) or (member(month, days30) and day > 30) or (month = 2 and day > 29) or day < 1 then
		error "Invalid date: %1", day;
	end if;
	dYear := year + 1166;
	if month = 2 and day = 29 then
		printf("The date is St. Tib's Day, YOLD %a.\n", dYear);
	else
		seasons := ["Chaos", "Discord", "Confusion", "Bureaucracy", "The Aftermath"];
		week := ["Sweetmorn", "Boomtime", "Pungenday", "Prickle-Prickle", "Setting Orange"];
		daysThisYear := 0;
		for i to month-1 do
			if member(i, days31) then
				daysThisYear := daysThisYear + 31;
			elif member(i, days30) then
				daysThisYear := daysThisYear + 30;
			else
				daysThisYear := daysThisYear + 28;
			end if;
		end do;
		daysThisYear := daysThisYear + day -1;
		dMonth := seasons[trunc((daysThisYear) / 73)+1];
		dDay := daysThisYear mod 73 +1;
		dayOfWeek := week[daysThisYear mod 5 +1];
		printf("The date is %s %s %s, YOLD %a.\n", dayOfWeek, dMonth, convert(dDay, ordinal), dYear);
	end if;
end proc:

convertDiscordian (2016, 1, 1);
convertDiscordian (2016, 2, 29);
convertDiscordian (2016, 12, 31);
```

```txt

The date is Sweetmorn Chaos first, YOLD 3182.
The date is St. Tib's Day, YOLD 3182.
The date is Setting Orange The Aftermath 73rd, YOLD 3182.

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
DiscordianDate[y_, m_, d_] := Module[{year = ToString[y + 1166], month = m, day = d},

  DMONTHS = {"Chaos", "Discord", "Confusion", "Bureaucracy", "The Aftermath"};
  DDAYS = {"Sweetmorn", "Boomtime", "Pungenday", "Prickle-Prickle", "Setting Orange"};
  DayOfYear = DateDifference[{y} ,{y, m, d}] + 1;
  LeapYearQ = (Mod[#, 4]== 0 && (Mod[#, 100] != 0 || Mod[#, 400] == 0))&@ y;

  If [ LeapYearQ && month == 2 && day == 29,
    Print["Today is St. Tib's Day, YOLD ", ]
  ,
    If [ LeapYearQ && DayOfYear >= 60, DayOfYear -= 1 ];
     {season, dday} = {Quotient[DayOfYear, 73], Mod[DayOfYear, 73]};
     Print["Today is ", DDAYS[[Mod[dday,4] + 1]],", ",DMONTHS[[season+1]]," ",dday,", YOLD ",year]
    ];
]
```

```txt
DiscordianDate[2012,2,28]
-> Today is Prickle-Prickle, Chaos 59, YOLD 3178

DiscordianDate[2012,2,29]
-> Today is St. Tib's Day, YOLD 3178

DiscordianDate[2012,3,1]
-> Today is Setting Orange, Chaos 60, YOLD 3178
```



## Pascal


```Pascal

program ddate;
{
This program is free software, it's done it's time
and paid for it's crime. You can copy, edit and use this
software under the terms of the GNU GPL v3 or later.

Copyright Pope Englebert Finklestien,
On this day Boomtime, the 71st day of Confusion in the YOLD 3183

This program will print out the current date in Erisian format as specified in

                  P R I N C I P I A   D I S C O R D I A

If you run it with a date it the command line in european format (dd mm yy) it
will print the equvolent Discordian date. If you omit the year and  month the
current Anerisiean month and year as assumed.


POPE Englebert Finklestien.
}
uses Sysutils;

var
  YY,MM,DD : word;
  YOLD : Boolean;
  Hedgehog: integer;
  Eris: string;
  snub: string;
  chaotica: string;
  midget: string;
  bob: string;


  Anerisiandaysinmonth: array[1..12] of integer = (31,28,31,30,31,30,31,31,30,31,30,31);



procedure anerisiandate;
{ tHIS JUST GETS THE DATE INTO THE  DD,MM,YY VARIABLES }
begin
    DeCodeDate(date,yy,mm,dd);
end;

procedure BORIS;
{ This just tests to see if we are in a leap year }
var
  snafu : boolean;
begin
  snafu := False;
  if (yy mod 4 = 0) then snafu := True;
  if ((yy mod 100 = 0) and (yy mod 400 <> 0)) then snafu := False;
  if ((snafu) and (mm = 2 ) and (dd=29)) then YOLD := True;
end;

function hodgepodge: integer;
{ This returns the total number of days since the year began.
It doesn't bother with leap years at all.
I get a wierd optical illusion looking at the until in this }
var
  fnord : integer;
begin
   Hedgehog := 1;
   hodgepodge := 0;
   fnord :=0;
   if (mm > 1) then repeat
             fnord := fnord + Anerisiandaysinmonth[Hedgehog];
             Hedgehog := Hedgehog +1;
       until Hedgehog = mm;
   fnord := fnord + dd;
   hodgepodge := fnord;
 end;


function treesaregreen(): string;
{Returns the YOLD as a string}

begin
    treesaregreen := IntTOStr(yy+1166);
end;


procedure GRAYFACE;
{This calculates everything, but does not bother much about leap years}
var
   wrestle: integer;
   Thwack: string;
begin
    Hedgehog := hodgepodge;
    wrestle := 0;
    Thwack := 'th';
    {set bob to the name of the holyday or St. Tibs day }
    bob := 'St. Tibs Day';
    if (Hedgehog = 5 )  then bob := 'Mungday';
    if (Hedgehog = 50 ) then bob := 'Chaoflux';
    if (Hedgehog = 78 ) then bob := 'Mojoday';
    if (Hedgehog = 123) then bob := 'Discoflux';
    if (Hedgehog = 151) then bob := 'Syaday';
    if (Hedgehog = 196) then bob := 'Confuflux';
    if (Hedgehog = 224) then bob := 'Zaraday';
    if (Hedgehog = 269) then bob := 'Bureflux';
    if (Hedgehog = 297) then bob := 'Maladay';
    if (Hedgehog = 342) then bob := 'Afflux';
    {Not doing things the usual way
    Lets find the week day and count the number of
	5 day weeks all at the same time}
    while (Hedgehog > 5) do begin
      Hedgehog := Hedgehog -5;
      wrestle := Wrestle + 1;
    end;
    if (Hedgehog = 1) then snub := 'Sweetmorn'  ;
    if (Hedgehog = 2) then snub := 'BoomTime';
    if (Hedgehog = 3) then snub := 'Pungenday';
    if (Hedgehog = 4) then snub := 'Prickle-Prickle';
    if (Hedgehog = 5) then snub := 'Setting Orange';
	{Now to set the Season name}
    chaotica:='The Aftermath';
    if (wrestle <=57) then chaotica := 'Bureaucracy';
    if ((wrestle = 58) and (Hedgehog < 3)) then chaotica := 'Bureaucracy';
    if (wrestle <= 42) then chaotica := 'Confusion';
    if ((wrestle = 43) and (Hedgehog < 5)) then chaotica := 'Confusion';
    if (wrestle <=28) then chaotica := 'Discord';
    if ((wrestle = 29) and (Hedgehog < 2)) then chaotica := 'Discord';
    if (wrestle <=13) then chaotica := 'Chaos';
    if ((wrestle = 14) and (Hedgehog < 4)) then chaotica := 'Chaos';

	{Now all we need the day of the season}
    wrestle := (wrestle*5)+Hedgehog;
    while (wrestle >73) do wrestle := wrestle -73;
	{pick the appropriate day postfix, allready set to th}
    if (wrestle in [1,21,31,41,51,61,71]) then Thwack:='st';
    if (wrestle in [2,22,32,42,52,62,72]) then Thwack:='nd';
    if (wrestle in [3,23,33,43,53,63,73]) then Thwack:='rd';
	{Check to see if it is a holy day, if so bob will have
	the right holyday name already, including St Tibs Day}
    if (wrestle in [5,50]) then YOLD := True;
	{I love this line of code}
    midget := IntToStr(wrestle) + Thwack;
end;


{The main program starts here}
begin
    anerisiandate;
    if (ParamCount >=1) then dd := StrTOInt(ParamStr(1));
    if (ParamCount >=2) then mm := StrToInt(ParamStr(2));
    if (ParamCount =3) then yy := StrToInt(ParamStr(3));
    BORIS;
    GRAYFACE;
	{ The only thing to bother about is holy days and St Tibs day }
    Eris := 'Today is: ' + snub +' the ' + midget +' day of the season of ' + chaotica;
    if (YOLD) then begin
	    Eris := 'Celebrate for today, ' + snub + ' the ' + midget + ' day of ' +chaotica + ' is the holy day of ' + bob;
    end;
	{The only place we deal with St. Tibs Day}
    if ((YOLD) and ((mm=2) and (dd=29))) then Eris := 'Celebrate ' + bob + ' Chaos';
	{This next line applies to all possibilities}
    Eris := Eris + ' YOLD ' + treesaregreen;
    WriteLn(Eris);
end.

```

```txt


 ./ddate
Today is: Setting Orange the 1st day of the season of The Aftermath YOLD 3183
./ddate 12
Celebrate for today, Prickle-Prickle the 5th day of The Aftermath is the holy day of Zaraday YOLD 3183
./ddate 13 5
Today is: Pungenday the 60th day of the season of Confusion YOLD 3183
./ddate 29 2 2020
Celebrate St. Tibs Day Chaos YOLD 3186

```



## Perl

```perl
use 5.010;
use strict;
use warnings;
use Time::Piece ();

my @seasons = (qw< Chaos Discord Confusion Bureaucracy >, 'The Aftermath');
my @week_days = (qw< Sweetmorn Boomtime Pungenday Prickle-Prickle >, 'Setting Orange');

sub ordinal {
	my ($n) = @_;
	return $n . "th" if int($n/10) == 1;
	return $n . ((qw< th st nd rd th th th th th th>)[$n % 10]);
}

sub ddate {
	my $d = Time::Piece->strptime( $_[0], '%Y-%m-%d' );
	my $yold = 'in the YOLD ' . ($d->year + 1166);

	my $day_of_year0 = $d->day_of_year;

	if( $d->is_leap_year ) {
		return "St. Tib's Day, $yold" if $d->mon == 2 and $d->mday == 29;
		$day_of_year0-- if $day_of_year0 >= 60; # Compensate for St. Tib's Day
	}

	my $weekday = $week_days[ $day_of_year0 % @week_days ];
	my $season = $seasons[ $day_of_year0 / 73 ];
	my $season_day = ordinal( $day_of_year0 % 73 + 1 );

	return "$weekday, the $season_day day of $season $yold";
}

say "$_ is " . ddate($_) for qw< 2010-07-22 2012-02-28 2012-02-29 2012-03-01 >;

```

```txt
2010-07-22 is Pungenday, the 57th day of Confusion in the YOLD 3176
2012-02-28 is Prickle-Prickle, the 59th day of Chaos in the YOLD 3178
2012-02-29 is St. Tib's Day, in the YOLD 3178
2012-03-01 is Setting Orange, the 60th day of Chaos in the YOLD 3178

```



## Perl 6

<!-- Hi ‎Grondilu! Keep up the good work! -->

```perl6
my @seasons = << Chaos Discord Confusion Bureaucracy 'The Aftermath' >>;
my @days = << Sweetmorn Boomtime Pungenday Prickle-Prickle 'Setting Orange' >>;
sub ordinal ( Int $n ) { $n ~ ( $n % 100 == 11|12|13
    ?? 'th' !! < th st nd rd th th th th th th >[$n % 10] ) }

sub ddate ( Str $ymd ) {
    my $d = DateTime.new: "{$ymd}T00:00:00Z" or die;

    my $yold = 'in the YOLD ' ~ $d.year + 1166;

    my $day_of_year0 = $d.day-of-year - 1;

    if $d.is-leap-year {
        return "St. Tib's Day, $yold" if $d.month == 2 and $d.day == 29;
        $day_of_year0-- if $day_of_year0 >= 60; # Compensate for St. Tib's Day
    }

    my $weekday    = @days[    $day_of_year0 mod  5     ];
    my $season     = @seasons[ $day_of_year0 div 73     ];
    my $season_day = ordinal(  $day_of_year0 mod 73 + 1 );

    return "$weekday, the $season_day day of $season $yold";
}

say "$_ is {.&ddate}" for < 2010-07-22 2012-02-28 2012-02-29 2012-03-01 >;

```


```txt
2010-07-22 is Pungenday, the 57th day of Confusion in the YOLD 3176
2012-02-28 is Prickle-Prickle, the 59th day of Chaos in the YOLD 3178
2012-02-29 is St. Tib's Day, in the YOLD 3178
2012-03-01 is Setting Orange, the 60th day of Chaos in the YOLD 3178

```



## Phix


```Phix
constant seasons = {"Chaos", "Discord", "Confusion", "Bureaucracy", "The Aftermath"}
constant weekday = {"Sweetmorn", "Boomtime", "Pungenday", "Prickle-Prickle", "Setting Orange"}
constant apostle = {"Mungday", "Mojoday", "Syaday", "Zaraday", "Maladay"}
constant holiday = {"Chaoflux", "Discoflux", "Confuflux", "Bureflux", "Afflux"}

function discordianDate(sequence dt)
string dyear, dseas, dwday
integer  leap, doy, dsday,dseason
integer {y,m,d} = dt
    dyear = sprintf("%d",y+1166)
    leap = isleapyear(y)
    if leap and m=2 and d=29 then
        return "St. Tib's Day, in the YOLD " & dyear
    end if

    doy = day_of_year(y,m,d)-1
    if leap and doy>=60 then
        doy -= 1
    end if

    dsday = remainder(doy,73)+1
    dseason = floor(doy/73+1)
    if dsday=5 then
        return apostle[dseason] & ", in the YOLD " & dyear
    elsif dsday=50 then
        return holiday[dseason] & ", in the YOLD " & dyear
    end if

    dseas = seasons[dseason]
    dwday = weekday[remainder(doy,5)+1]

    return sprintf("%s, day %d of %s in the YOLD %s", {dwday, dsday, dseas, dyear})
end function
```

test code

```Phix
sequence dt = {2015,1,1,0,0,0,0,0}
include timedate.e
atom oneday = timedelta(days:=1)
set_timedate_formats({"DD/MM/YYYY: "})
for i=1 to 5 do
    ?format_timedate(dt)&discordianDate(dt)
    dt = adjust_timedate(dt,oneday*72)
    ?format_timedate(dt)&discordianDate(dt)
    dt = adjust_timedate(dt,oneday)
end for
```

```txt

"01/01/2015: Sweetmorn, day 1 of Chaos in the YOLD 3181"
"14/03/2015: Pungenday, day 73 of Chaos in the YOLD 3181"
"15/03/2015: Prickle-Prickle, day 1 of Discord in the YOLD 3181"
"26/05/2015: Sweetmorn, day 73 of Discord in the YOLD 3181"
"27/05/2015: Boomtime, day 1 of Confusion in the YOLD 3181"
"07/08/2015: Prickle-Prickle, day 73 of Confusion in the YOLD 3181"
"08/08/2015: Setting Orange, day 1 of Bureaucracy in the YOLD 3181"
"19/10/2015: Boomtime, day 73 of Bureaucracy in the YOLD 3181"
"20/10/2015: Pungenday, day 1 of The Aftermath in the YOLD 3181"
"31/12/2015: Setting Orange, day 73 of The Aftermath in the YOLD 3181"

```



## PHP




```PHP

<?php
    $Anerisia = array(31,28,31,30,31,30,31,31,30,31,30,31);
    $MONTHS = array("Choas","Discord","Confusion","Bureacracy","The Aftermath");
    $DAYS = array("Setting Orange","Sweetmorn","BoomTime","Pungenday","Prickle-Prickle");
    $Dsuff = array('th','st','nd','rd','th','th','th','th','th','th');
    $Holy5 = array("Mungday","MojoDay","Syaday","Zaraday","Maladay");
    $Holy50 = array("Chaoflux","Discoflux","Confuflux","Bureflux","Afflux");




    // Get the current system date and assign to some variables
	$edate = explode(" ",date('Y m j L'));
	$usery = $edate[0];
	$userm = $edate[1];
	$userd = $edate[2];
	$IsLeap = $edate[3];

  // If the user supplied us with a date overide the one we got from the system.
  // If you could get the date from users browser via javascript and then call
  // this script with the users date. ddate.php?y=year&m=month&d=day mostly it
  // won't matter but if the server is in a different time zone to the user
  // There will be occasional incorrect results from the users POV.

    if (isset($_GET['y']) && isset($_GET['m']) && isset($_GET['d'])) {
        $usery = $_GET['y'];
        $userm = $_GET['m'];
        $userd = $_GET['d'];
        $IsLeap = 0;
        if (($usery%4 == 0) && ($usery%100 >0)) $IsLeap =1;
        if ($usery%400 == 0) $IsLeap = 1;
    }

 // We need to know the total number of days in the year so far

    $userdays = 0;
    $i = 0;
    while ($i < ($userm-1)) {

        $userdays = $userdays + $Anerisia[$i];
        $i = $i +1;
    }
    $userdays = $userdays + $userd;

    // We can now work out the full discordian date for most dates
    // PHP does not do integer division, so we use 73.2 as a divisor
    // the value 73.2 works, larger values cause an off-by-one on season
    // changes for the later seasons .
    // This is not needed with the mod operator.

    $IsHolyday = 0;
    $dyear = $usery + 1166;
    $dmonth = $MONTHS[$userdays/73.2];
    $dday = $userdays%73;
	if (0 == $dday) $dday = 73;
    $Dname = $DAYS[$userdays%5];
    $Holyday = "St. Tibs Day";
    if ($dday == 5) {
        $Holyday = $Holy5[$userdays/73.2];
        $IsHolyday =1;
    }
    if ($dday == 50) {
        $Holyday = $Holy50[$userdays/73.2];
        $IsHolyday =1;
    }

  if (($IsLeap ==1) && ($userd ==29) and ($userm ==2)) $IsHolyday = 2;

   // work out the suffix to the day number
   $suff = $Dsuff[$dday%10] ;
   if ((11 <= $dday) && (19 >= $dday)) $suff='th';

    // code to display the date ...

 if ($IsHolyday ==2)
      echo "</br>Celeberate ",$Holyday," ",$dmonth," YOLD ",$dyear;
    if ($IsHolyday ==1)
      echo "</br>Celeberate for today ", $Dname , " The ", $dday,"<sup>",$suff,"</sup>", " day of ", $dmonth , " YOLD " , $dyear , " is the holy day of " , $Holyday;
    if ($IsHolyday == 0)
       echo "</br>Today is " , $Dname , " the " , $dday ,"<sup>",$suff, "</sup> day of " , $dmonth , " YOLD " , $dyear;

 ?>

```


```txt
ddate.php #23 November 2011
Today is Boomtime the 35th day of The Aftermath YOLD 3177

ddate.php?y=2012&m=2&y=28
Today is Prickle-Prickle the 59th day of Chaos YOLD 3178

ddate.php?y=2012&m=2&y=29
Celeberate St. Tibs Day, Chaos YOLD 3178

ddate.php?y=2017&m=8&d=12
Celeberate for today Prickle-Prickle The 5th day of Bureacracy YOLD 3183 is the holy day of Zaraday

```



## PicoLisp

```PicoLisp
(de disdate (Year Month Day)
   (let? Date (date Year Month Day)
      (let (Leap (date Year 2 29)  D (- Date (date Year 1 1)))
         (if (and Leap (= 2 Month) (= 29 Day))
            (pack "St. Tib's Day, YOLD " (+ Year 1166))
            (and Leap (>= D 60) (dec 'D))
            (pack
               (get
                  '("Chaos" "Discord" "Confusion" "Bureaucracy" "The Aftermath")
                  (inc (/ D 73)) )
               " "
               (inc (% D 73))
               ", YOLD "
               (+ Year 1166) ) ) ) ) )
```



## Pike

Pike includes a Discordian calendar.
dates can be converted from any calendar to any other.

```Pike>
 Calendar.Discordian.now()->format_ext_ymd();
 Result: "Pungenday, 59 Bureaucracy 3177"
> Calendar.Discordian.Day(Calendar.Day(2011,11,11))->format_ext_ymd();
 Result: "Setting Orange, 23 The Aftermath 3177"
> Calendar.Discordian.Day(Calendar.Badi.Day(168,13,9))->format_ext_ymd();
 Result: "Setting Orange, 23 The Aftermath 3177"
> Calendar.Day((Calendar.Discordian.Month()+1)->day(1));
 Result: Day(Thu 20 Oct 2011)
```



## PowerBASIC


```powerbasic
#COMPILE EXE
#DIM ALL

'change this for systems that use a different character
$DATESEP = "-"

FUNCTION day(date AS STRING) AS LONG
    'date is same format as date$
    DIM tmpdash1 AS LONG, tmpdash2 AS LONG
    tmpdash1 = INSTR(date, $DATESEP)
    tmpdash2 = INSTR(-1, date, $DATESEP)
    FUNCTION = VAL(MID$(date, tmpdash1 + 1, tmpdash2 - tmpdash1 - 1))
END FUNCTION

FUNCTION month(date AS STRING) AS LONG
    'date is same format as date$
    DIM tmpdash AS LONG
    tmpdash = INSTR(date, $DATESEP)
    FUNCTION = VAL(LEFT$(date, tmpdash - 1))
END FUNCTION

FUNCTION year(date AS STRING) AS LONG
    'date is same format as date$
    DIM tmpdash AS LONG
    tmpdash = INSTR(-1, date, $DATESEP)
    FUNCTION = VAL(MID$(date, tmpdash + 1))
END FUNCTION

FUNCTION julian(date AS STRING) AS LONG
    'date is same format as date$
    'doesn't account for leap years (not needed for ddate)
    'days preceding 1st of month
    '   jan feb mar apr may  jun  jul  aug  sep  oct  nov  dec
    DATA 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334
    FUNCTION = VAL(READ$(month(date))) + day(date)
END FUNCTION

FUNCTION PBMAIN () AS LONG
    'Season names
    DATA "Chaos", "Discord", "Confusion", "Bureaucracy", "The Aftermath"
    'Weekdays
    DATA "Setting Orange", "Sweetmorn", "Boomtime", "Pungenday", "Prickle-Prickle"

    DIM dyear AS LONG, dseason AS STRING, dday AS LONG, dweekday AS STRING
    DIM tmpdate AS STRING, jday AS LONG, result AS STRING

    IF LEN(COMMAND$) THEN
        tmpdate = COMMAND$
    ELSE
        tmpdate = DATE$
    END IF
    dyear = year(tmpdate) + 1166
    IF (2 = month(tmpdate)) AND (29 = day(tmpdate)) THEN
        result = "Saint Tib's Day, " & STR$(dyear) & " YOLD"
    ELSE
        jday = julian(tmpdate)
        dseason = READ$((jday \ 73) + 1)
        dday = (jday MOD 73)
        IF 0 = dday THEN dday = 73
        dweekday = READ$((jday MOD 5) + 6)
        result = dweekday & ", " & dseason & " " & TRIM$(STR$(dday)) & ", " & TRIM$(STR$(dyear)) & " YOLD"
    END IF

    ? result
END FUNCTION
```



## PowerShell

```powershell

function ConvertTo-Discordian ( [datetime]$GregorianDate )
{
$DayOfYear = $GregorianDate.DayOfYear
$Year = $GregorianDate.Year + 1166
If ( [datetime]::IsLeapYear( $GregorianDate.Year ) -and $DayOfYear -eq 60 )
    { $Day = "St. Tib's Day" }
Else
    {
    If ( [datetime]::IsLeapYear( $GregorianDate.Year ) -and $DayOfYear -gt 60 )
        { $DayOfYear-- }
    $Weekday = @( 'Sweetmorn', 'Boomtime', 'Pungenday', 'Prickle-Prickle', 'Setting Orange' )[(($DayOfYear - 1 ) % 5 )]
    $Season  = @( 'Chaos', 'Discord', 'Confusion', 'Bureaucracy', 'The Aftermath' )[( [math]::Truncate( ( $DayOfYear - 1 ) / 73 ) )]
    $DayOfSeason = ( $DayOfYear - 1 ) % 73 + 1
    $Day = "$Weekday, $Season $DayOfSeason"
    }
$DiscordianDate = "$Day, $Year YOLD"
return $DiscordianDate
}

ConvertTo-Discordian ([datetime]'1/5/2016')
ConvertTo-Discordian ([datetime]'2/29/2016')
ConvertTo-Discordian ([datetime]'12/8/2016')

```

```txt

Setting Orange, Chaos 5, 3182 YOLD
St. Tib's Day, 3182 YOLD
Boomtime, The Aftermath 50, 3182 YOLD

```



## PureBasic


```PureBasic
Procedure.s Discordian_Date(Y, M, D)
  Protected DoY=DayOfYear(Date(Y,M,D,0,0,0)), Yold$=Str(Y+1166)
  Dim S.s(4)
  S(0)="Chaos": S(1)="Discord": S(2)="Confusion": S(3)="Bureaucracy"
  S(4)="The Aftermath"
  If (Y%4=0 And Y%100) Or Y%400=0
    If M=2 And D=29
      ProcedureReturn "St. Tib's Day, YOLD " + Yold$
    ElseIf DoY>=2*30
      DoY-1
    EndIf
  EndIf
  ProcedureReturn S(DoY/73)+" "+Str(DoY%73)+", Yold "+Yold$
EndProcedure
```



## Python


```python
import datetime, calendar

DISCORDIAN_SEASONS = ["Chaos", "Discord", "Confusion", "Bureaucracy", "The Aftermath"]

def ddate(year, month, day):
    today = datetime.date(year, month, day)
    is_leap_year = calendar.isleap(year)
    if is_leap_year and month == 2 and day == 29:
        return "St. Tib's Day, YOLD " + (year + 1166)

    day_of_year = today.timetuple().tm_yday - 1

    if is_leap_year and day_of_year >= 60:
        day_of_year -= 1 # Compensate for St. Tib's Day

    season, dday = divmod(day_of_year, 73)
    return "%s %d, YOLD %d" % (DISCORDIAN_SEASONS[season], dday + 1, year + 1166)

```



## Racket

```racket
#lang racket/base
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;; Derived from 'D' Implementation
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(require racket/date racket/match)

(define seasons '(Chaos     Discord   Confusion Bureaucracy     |The Aftermath|))
(define weekday '(Sweetmorn Boomtime  Pungenday Prickle-Prickle |Setting Orange|))
(define apostle '(Mungday   Mojoday   Syaday    Zaraday         Maladay))
(define holiday '(Chaoflux  Discoflux Confuflux Bureflux        Afflux))

(define (ymd->date y m d) (seconds->date (find-seconds 0 0 0 d m y)))
(define (leap-year? y) (with-handlers ((exn? (λ (x) #f))) (= 29 (date-day (ymd->date y 2 29)))))

(define (discordian-date d)
  (define leap? (leap-year? (date-year d)))
  (define year-day (match* (leap? (date-year-day d))
                     [(#t (? (λ (D) (>= D 59)) d0)) d0]
                     [(_ d0) (add1 d0)]))

  (define season-day (modulo year-day 73)) ; season day
  (define (list-ref-season l)
    (define season-index (quotient year-day 73))
    (symbol->string (list-ref l season-index)))

  (string-append
   (match* (season-day leap? (date-month d) (date-day d))
     [( _ #t 2 29) "St. Tib's Day,"]
     [((app (match-lambda
              (5 apostle) (50 holiday) (_ #f))
            (and (not #f) special)) _ _  _)
      (string-append (list-ref-season special) ",")]
     [( _  _ _  _)
      (define week-day-name (list-ref weekday (modulo (sub1 year-day) 5)))
      (format "~a, day ~a of ~a" week-day-name season-day (list-ref-season seasons))])
   " in the YOLD " (number->string (+ (date-year d) 1166))))

(displayln (discordian-date (current-date)))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; passing these tests makes me consistent with D implementation
(module+ test
  (require rackunit)
  (define discordian/ymd (compose discordian-date ymd->date))
  (check-equal? (discordian/ymd 2010 7 22) "Pungenday, day 57 of Confusion in the YOLD 3176")
  (check-equal? (discordian/ymd 2012 2 28) "Prickle-Prickle, day 59 of Chaos in the YOLD 3178")
  (check-equal? (discordian/ymd 2012 2 29) "St. Tib's Day, in the YOLD 3178");
  (check-equal? (discordian/ymd 2012 3  1) "Setting Orange, day 60 of Chaos in the YOLD 3178")
  (check-equal? (discordian/ymd 2010 1  5) "Mungday, in the YOLD 3176")
  (check-equal? (discordian/ymd 2011 5  3) "Discoflux, in the YOLD 3177"))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~FIN
```


```txt
Pungenday, day 47 of Confusion in the YOLD 3179
```



## REXX

This REXX version allows the   '''mm'''   or   '''dd'''   to be one or two digits.

Also, the year can be two or four digits, or it may be omitted.

If the Gregorian date is omitted or specified as an asterisk   ('''*'''),   the current date is used.

```rexx
/*REXX program  converts  a   mm/dd/yyyy    Gregorian date   ───►   a  Discordian date. */
 @day.1= 'Sweetness'                             /*define the 1st day─of─Discordian─week*/
 @day.2= 'Boomtime'                              /*   "    "  2nd  "   "      "       " */
 @day.3= 'Pungenday'                             /*   "    "  3rd  "   "      "       " */
 @day.4= 'Prickle-Prickle'                       /*   "    "  4th  "   "      "       " */
 @day.5= 'Setting Orange'                        /*   "    "  5th  "   "      "       " */

@seas.0= "St. Tib's day,"                        /*define the leap─day of Discordian yr.*/
@seas.1= 'Chaos'                                 /*   "   1st season─of─Discordian─year.*/
@seas.2= 'Discord'                               /*   "   2nd    "    "      "       "  */
@seas.3= 'Confusion'                             /*   "   3rd    "    "      "       "  */
@seas.4= 'Bureaucracy'                           /*   "   4th    "    "      "       "  */
@seas.5= 'The Aftermath'                         /*   "   5th    "    "      "       "  */

parse arg  gM  '/'  gD  "/"  gY .                /*obtain the specified Gregorian date. */
if gM=='' | gM=="," | gM=='*'  then parse value  date("U")   with   gM  '/'  gD  "/"  gY .

gY=left( right( date(), 4), 4 - length(Gy) )gY   /*adjust for two─digit year or no year.*/

                                                 /* [↓]  day─of─year,  leapyear adjust. */
doy= date('d', gY || right(gM, 2, 0)right(gD ,2, 0),  "s")   -   (leapyear(gY)   &   gM>2)

dW= doy//5;                if dW==0  then dW=  5 /*compute the Discordian weekday.      */
dS= (doy-1) % 73  +  1                           /*   "     "       "     season.       */
dD= doy//73;               if dD==0  then dD= 73 /*   "     "       "     day─of─month. */
if leapyear(gY)  &  gM==2  &  gD==29 then ds=  0 /*is this St. Tib's day  (leapday) ?   */
if ds==0  then dD=                               /*adjust for the Discordian leap day.  */

say space(@day.dW','  @seas.dS dD","  gY + 1166) /*display Discordian date to terminal. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
leapyear: procedure;   parse arg y               /*obtain a four─digit Gregorian year.  */
          if y//4  \==  0  then return 0         /*Not  ÷  by 4?   Then not a leapyear. */
          return y//100 \== 0  |  y//400 == 0    /*apply the  100  and  400  year rules.*/
```

```txt

  2/28/2012
  2/29/2012
  3/1/2012
  7/22/2010
  9/2/2012
 12/31/2011
 10/19/2015
 12/31/2100

```

```txt

Prickle-Prickle, Chaos 59, 3178
Setting Orange, St. Tib's day, 3178
Setting Orange, Chaos 60, 3178
Pungenday, Confusion 57, 3176
Setting Orange, Bureaucracy 26, 3178
Setting Orange, The Aftermath 73, 3177
Boomtime, Bureaucracy 73, 3181
Setting Orange, The Aftermath 73, 3266

```



## Ruby


```ruby
require 'date'

class DiscordianDate
  SEASON_NAMES = ["Chaos","Discord","Confusion","Bureaucracy","The Aftermath"]
  DAY_NAMES = ["Sweetmorn","Boomtime","Pungenday","Prickle-Prickle","Setting Orange"]
  YEAR_OFFSET = 1166
  DAYS_PER_SEASON = 73
  DAYS_PER_WEEK = 5
  ST_TIBS_DAY_OF_YEAR = 60

  def initialize(year, month, day)
    gregorian_date = Date.new(year, month, day)
    @day_of_year = gregorian_date.yday

    @st_tibs = false
    if gregorian_date.leap?
      if @day_of_year == ST_TIBS_DAY_OF_YEAR
        @st_tibs = true
      elsif @day_of_year > ST_TIBS_DAY_OF_YEAR
        @day_of_year -= 1
      end
    end

    @season, @day = (@day_of_year-1).divmod(DAYS_PER_SEASON)
    @day += 1     #←              ↑ fixes of-by-one error (only visible at season changes)
    @year = gregorian_date.year + YEAR_OFFSET
  end
  attr_reader :year, :day

  def season
    SEASON_NAMES[@season]
  end

  def weekday
    if @st_tibs
      "St. Tib's Day"
    else
      DAY_NAMES[(@day_of_year - 1) % DAYS_PER_WEEK]
    end
  end

  def to_s
    %Q{#{@st_tibs ? "St. Tib's Day" : "%s, %s %d" % [weekday, season, day]}, #{year} YOLD}
  end
end
```


Testing:

```ruby
[[2012, 2, 28], [2012, 2, 29], [2012, 3, 1], [2011, 10, 5], [2015, 10, 19]].each do |date|
  dd = DiscordianDate.new(*date)
  puts "#{"%4d-%02d-%02d" % date} => #{dd}"
end
```

```txt
2012-02-28 => Prickle-Prickle, Chaos 59, 3178 YOLD
2012-02-29 => St. Tib's Day, 3178 YOLD
2012-03-01 => Setting Orange, Chaos 60, 3178 YOLD
2011-10-05 => Pungenday, Bureaucracy 59, 3177 YOLD
2015-10-19 => Boomtime, Bureaucracy 73, 3181 YOLD
```



## Rust

'''Compiler:''' [[Rust]] (1.21.0)
This example is taken in large part from the ddate crate and used with the
permission of the author.


```rust
extern crate chrono;

use chrono::NaiveDate;
use std::str::FromStr;

fn main() {
    let date = std::env::args().nth(1).expect("Please provide a YYYY-MM-DD date.");
    println!("{} is {}", date, NaiveDate::from_str(&date).unwrap().to_poee());
}

// The necessary constants for the seasons, weekdays, and holydays.
const APOSTLES: [&str; 5] = ["Mungday", "Mojoday", "Syaday", "Zaraday", "Maladay"];
const HOLYDAYS: [&str; 5] = ["Chaoflux", "Discoflux", "Confuflux", "Bureflux", "Afflux"];
const SEASONS: [&str; 5] = ["Chaos", "Discord", "Confusion", "Bureaucracy", "The Aftermath"];
const WEEKDAYS: [&str; 5] = ["Sweetmorn", "Boomtime", "Pungenday", "Prickle-Prickle", "Setting Orange"];

// The necessary constants for the conversion.
const APOSTLE_HOLYDAY: usize = 5;
const CURSE_OF_GREYFACE: i32 = 1166;
const SEASON_DAYS: usize = 73;
const SEASON_HOLYDAY: usize = 50;
const ST_TIBS_DAY: usize = 59;
const WEEK_DAYS: usize = 5;

// This extends the `Datelike` trait of Rust's Chrono crate with a method that
// prints any Datelike type as a String.
impl<T: Datelike> DiscordianDate for T {}
pub trait DiscordianDate: Datelike {
    fn to_poee(&self) -> String {
        let day = self.ordinal0() as usize;
        let leap = self.year() % 4 == 0 && self.year() % 100 != 0 || self.year() % 400 == 0;
        let year = self.year() + CURSE_OF_GREYFACE;

        if leap && day == ST_TIBS_DAY { return format!("St. Tib's Day, in the YOLD {}", year); }

        let day_offset = if leap && day > ST_TIBS_DAY { day - 1 } else { day };

        let day_of_season = day_offset % SEASON_DAYS + 1;

        let season = SEASONS[day_offset / SEASON_DAYS];
        let weekday = WEEKDAYS[day_offset % WEEK_DAYS];

        let holiday = if day_of_season == APOSTLE_HOLYDAY {
            format!("\nCelebrate {}", APOSTLES[day_offset / SEASON_DAYS])
        } else if day_of_season == SEASON_HOLYDAY {
            format!("\nCelebrate {}", HOLYDAYS[day_offset / SEASON_DAYS])
        } else {
            String::with_capacity(0)
        };

        format!("{}, the {} day of {} in the YOLD {}{}",
            weekday, ordinalize(day_of_season), season, year, holiday)
    }
}

/// A helper function to ordinalize a numeral.
fn ordinalize(num: usize) -> String {
    let s = format!("{}", num);

    let suffix = if s.ends_with('1') && !s.ends_with("11") {
        "st"
    } else if s.ends_with('2') && !s.ends_with("12") {
        "nd"
    } else if s.ends_with('3') && !s.ends_with("13") {
        "rd"
    } else {
        "th"
    };

    format!("{}{}", s, suffix)
}

```


```txt
$ ./ddate -1166-1-1
-1166-1-1 is Sweetmorn, the 1st day of Chaos in the YOLD 0
$ ./ddate 2000-2-28
2000-2-28 is Prickle-Prickle, the 59th day of Chaos in the YOLD 3166
$ ./ddate -4004-2-29
-4004-2-29 is St. Tib's Day, in the YOLD -2838
$ ./ddate 1066-2-28
1066-2-28 is Prickle-Prickle, the 59th day of Chaos in the YOLD 2232
$ ./ddate 1492-3-1
1492-3-1 is Setting Orange, the 60th day of Chaos in the YOLD 2658
$ ./ddate 1492-9-26
1492-9-26 is Prickle-Prickle, the 50th day of Bureaucracy in the YOLD 2658
Celebrate Bureflux
$ ./ddate 2012-10-24
2012-10-24 is Boomtime, the 5th day of The Aftermath in the YOLD 3178
Celebrate Maladay
$

```



## Scala


```scala
package rosetta

import java.util.GregorianCalendar
import java.util.Calendar

object DDate extends App {
  private val DISCORDIAN_SEASONS = Array("Chaos", "Discord", "Confusion", "Bureaucracy", "The Aftermath")

  // month from 1-12; day from 1-31
  def ddate(year: Int, month: Int, day: Int): String = {
    val date = new GregorianCalendar(year, month - 1, day)
    val dyear = year + 1166

    val isLeapYear = date.isLeapYear(year)
    if (isLeapYear && month == 2 && day == 29) // 2 means February
      "St. Tib's Day " + dyear + " YOLD"
    else {
      var dayOfYear = date.get(Calendar.DAY_OF_YEAR) - 1
      if (isLeapYear && dayOfYear >= 60)
        dayOfYear -= 1 // compensate for St. Tib's Day

      val dday = dayOfYear % 73
      val season = dayOfYear / 73
      "%s %d, %d YOLD".format(DISCORDIAN_SEASONS(season), dday + 1, dyear)
    }
  }
  if (args.length == 3)
    println(ddate(args(2).toInt, args(1).toInt, args(0).toInt))
  else if (args.length == 0) {
    val today = Calendar.getInstance
    println(ddate(today.get(Calendar.YEAR), today.get(Calendar.MONTH)+1, today.get(Calendar.DAY_OF_MONTH)))
  } else
    println("usage: DDate [day month year]")
}
```

Test:

```scala
scala rosetta.DDate 2010 7 22
Confusion 57, 3176 YOLD
scala rosetta.DDate 28 2 2012
Chaos 59, 3178 YOLD
scala rosetta.DDate 29 2 2012
St. Tib's Day 3178 YOLD
scala rosetta.DDate 1 3 2012
Chaos 60, 3178 YOLD
scala rosetta.DDate 19 10 2015
Bureaucracy 73, 3181 YOLD

```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "time.s7i";

const array string: seasons is [0] ("Chaos", "Discord", "Confusion", "Bureaucracy", "The Aftermath");
const array string: weekday is [0] ("Sweetmorn", "Boomtime", "Pungenday", "Prickle-Prickle", "Setting Orange");
const array string: apostle is [0] ("Mungday", "Mojoday", "Syaday", "Zaraday",  "Maladay");
const array string: holiday is [0] ("Chaoflux", "Discoflux", "Confuflux", "Bureflux", "Afflux");

const func string: discordianDate (in time: date) is func
  result
    var string: discordianDate is "";
  local
    var integer: dyear is 0;
    var integer: doy is 0;
    var integer: dsday is 0;
  begin
    dyear := date.year + 1166;
    if isLeapYear(date.year) and date.month = 2 and date.day = 29 then
      discordianDate := "St. Tib's Day, in the YOLD " <& dyear;
    else
      doy := dayOfYear(date);
      if isLeapYear(date.year) and doy >= 60 then
        decr(doy);
      end if;
      dsday := doy rem 73; # season day
      if dsday = 5 then
        discordianDate := apostle[doy div 73] <& ", in the YOLD " <& dyear;
      elsif dsday = 50 then
        discordianDate := holiday[doy div 73] <& ", in the YOLD " <& dyear;
      else
        discordianDate := weekday[pred(doy) rem 5] <&
                          ", day " <& dsday <&
                          " of " <& seasons[doy div 73] <&
                          " in the YOLD " <& dyear;
      end if;
    end if;
  end func;

const proc: main is func
  local
    var time: today is time.value;
  begin
    today := time(NOW);
    writeln(strDate(today) <& " as Discordian date: " <& discordianDate(today));
    if  discordianDate(date(2010, 7, 22)) = "Pungenday, day 57 of Confusion in the YOLD 3176" and
        discordianDate(date(2012, 2, 28)) = "Prickle-Prickle, day 59 of Chaos in the YOLD 3178" and
        discordianDate(date(2012, 2, 29)) = "St. Tib's Day, in the YOLD 3178" and
        discordianDate(date(2012, 3,  1)) = "Setting Orange, day 60 of Chaos in the YOLD 3178" and
        discordianDate(date(2010, 1,  5)) = "Mungday, in the YOLD 3176" and
        discordianDate(date(2011, 5,  3)) = "Discoflux, in the YOLD 3177" then
      writeln("Discordian date computation works.");
    end if;
  end func;
```


```txt

2013-05-12 as Discordian date: Boomtime, day 59 of Discord in the YOLD 3179
Discordian date computation works.

```



## Sidef

```ruby
require('Time::Piece');

var seasons = %w(Chaos Discord Confusion Bureaucracy The\ Aftermath);
var week_days = %w(Sweetmorn Boomtime Pungenday Prickle-Prickle Setting\ Orange);

func ordinal (n) {
    "#{n}" + (n % 100 ~~ [11,12,13] ? 'th'
                                    : <th st nd rd th th th th th th>[n % 10])
}

func ddate(ymd) {
    var d = %s'Time::Piece'.strptime(ymd, '%Y-%m-%d');
    var yold = "in the YOLD #{d.year + 1166}";

    var day_of_year0 = d.day_of_year;

    if (d.is_leap_year) {
        return "St. Tib's Day, #{yold}" if ([d.mon, d.mday] == [2, 29]);
        day_of_year0-- if (day_of_year0 >= 60); # Compensate for St. Tib's Day
    }

    var weekday = week_days[day_of_year0 % week_days.len];
    var season = seasons[day_of_year0 / 73];
    var season_day = ordinal(day_of_year0 % 73 + 1);

    return "#{weekday}, the #{season_day} day of #{season} #{yold}";
}

%w(2010-07-22 2012-02-28 2012-02-29 2012-03-01).each { |ymd|
    say "#{ymd} is #{ddate(ymd)}"
}
```

```txt

2010-07-22 is Pungenday, the 57th day of Confusion in the YOLD 3176
2012-02-28 is Prickle-Prickle, the 59th day of Chaos in the YOLD 3178
2012-02-29 is St. Tib's Day, in the YOLD 3178
2012-03-01 is Setting Orange, the 60th day of Chaos in the YOLD 3178

```



## Tcl


```tcl
package require Tcl 8.5
proc disdate {year month day} {
    # Get the day of the year
    set now [clock scan [format %02d-%02d-%04d $day $month $year] -format %d-%m-%Y]
    scan [clock format $now -format %j] %d doy

    # Handle leap years
    if {!($year%4) && (($year%100) || !($year%400))} {
	if {$doy == 60} {
	    return "St. Tib's Day, [expr {$year + 1166}] YOLD"
	} elseif {$doy > 60} {
	    incr doy -1
	}
    }

    # Main conversion to discordian format now that special cases are handled
    incr doy -1; # Allow div/mod to work right
    set season [lindex {Chaos Discord Confusion Bureaucracy {The Aftermath}} \
	    [expr {$doy / 73}]]
    set dos [expr {$doy % 73 + 1}]
    incr year 1166
    return "$season $dos, $year YOLD"
}
```

Demonstrating:

```tcl
puts [disdate 2010 7 22]; # Today
puts [disdate 2012 2 28]
puts [disdate 2012 2 29]
puts [disdate 2012 3 1]
```

```txt

Confusion 57, 3176 YOLD
Chaos 59, 3178 YOLD
St. Tib's Day, 3178 YOLD
Chaos 60, 3178 YOLD

```



## zkl

```zkl
fcn discordianDate(y,m,d){
   var [const]
   seasons=T("Chaos","Discord","Confusion","Bureaucracy","The Aftermath"),
   weekday=T("Sweetmorn","Boomtime","Pungenday","Prickle-Prickle","Setting Orange"),
   apostle=T("Mungday","Mojoday","Syaday","Zaraday","Maladay"),
   holiday=T("Chaoflux","Discoflux","Confuflux","Bureflux","Afflux"];

   dYear,isLeapYear := y + 1166, Time.Date.isLeapYear(y);
   if(isLeapYear and m==2 and d==29)
      return("St. Tib's Day, in the YOLD " + dYear);

    doy:=Time.Date.nthDayInYear(y,m,d);
    if(isLeapYear and doy>=60) doy-=1;
    dsDay:=(if(doy%73==0) 73 else doy%73); // Season day.
    if(dsDay==5)  return(String(apostle[doy/73],", in the YOLD ",dYear));
    if(dsDay==50) return(String(holiday[doy/73],", in the YOLD ",dYear));

    dSeas:=seasons[(if(doy%73==0) doy-1 else doy)/73];
    dWday:=weekday[(doy - 1)%5];
    "%s, day %s of %s in the YOLD %s".fmt(dWday,dsDay,dSeas,dYear);
}
```


```zkl
foreach y,m,d in (T(T(2010,7,22), T(2012,2,28), T(2012,2,29), T(2012,3,1),
		    T(2010,1, 5), T(2011,5, 3))){
   "%d-%02d-%02d is -->%s".fmt(y,m,d,discordianDate(y,m,d)).println();
}
```

```txt

2010-07-22 is -->Pungenday, day 57 of Confusion in the YOLD 3176
2012-02-28 is -->Prickle-Prickle, day 59 of Chaos in the YOLD 3178
2012-02-29 is -->St. Tib's Day, in the YOLD 3178
2012-03-01 is -->Setting Orange, day 60 of Chaos in the YOLD 3178
2010-01-05 is -->Mungday, in the YOLD 3176
2011-05-03 is -->Discoflux, in the YOLD 3177

```

