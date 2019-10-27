+++
title = "Averages/Mean time of day"
description = ""
date = 2019-10-06T04:57:23Z
aliases = []
[extra]
id = 11986
[taxonomies]
categories = []
tags = []
+++

{{task|Date and time}}

{{task heading}}

A particular activity of bats occurs at these times of the day:
:<tt>23:00:17</tt>, <tt>23:40:20</tt>, <tt>00:12:45</tt>, <tt>00:17:19</tt>

Using the idea that there are twenty-four hours in a day, 
which is analogous to there being 360 degrees in a circle, 
map times of day to and from angles; 
and using the ideas of [[Averages/Mean angle]] 
compute and show the average time of the nocturnal activity 
to an accuracy of one second of time.

{{task heading|See also}}

{{Related tasks/Statistical measures}}

<hr>


## 11l

{{trans|Python}}

```11l
F mean_angle(angles)
   V x = sum(angles.map(a -> cos(radians(a)))) / angles.len
   V y = sum(angles.map(a -> sin(radians(a)))) / angles.len
   R degrees(atan2(y, x))

F mean_time(times)
   V t = (times.map(time -> time.split(‘:’)))
   V seconds = (t.map(hms -> (Float(hms[2]) + Int(hms[1]) * 60 + Int(hms[0]) * 3600)))
   V day = 24 * 60 * 60
   V to_angles = seconds.map(s -> s * 360.0 / @day)
   V mean_as_angle = mean_angle(to_angles)
   V mean_seconds = round(mean_as_angle * day / 360.0)
   I mean_seconds < 0
      mean_seconds += day
   V h = mean_seconds I/ 3600
   V m = mean_seconds % 3600
   V s = m % 60
   m = m I/ 60
   R String(h).zfill(2)‘:’String(m).zfill(2)‘:’String(s).zfill(2)

print(mean_time([‘23:00:17’, ‘23:40:20’, ‘00:12:45’, ‘00:17:19’]))
```

{{out}}

```txt
23:47:43
```



## AutoHotkey

{{works with|AutoHotkey 1.1}}

```AutoHotkey
MsgBox, % "The mean time is: " MeanTime(["23:00:17", "23:40:20", "00:12:45", "00:17:19"])

MeanTime(t, x=0, y=0) {
	static c := ATan(1) / 45
	for k, v in t {
		n := StrSplit(v, ":")
		r := c * (n[1] * 3600 + n[2] * 60 + n[3]) / 240
		x += Cos(r)
		y += Sin(r)
	}
	r := atan2(x, y) / c
	r := (r < 0 ? r + 360 : r) / 15
	h := SubStr("00" Round(r // 1, 0), -1)
	s := SubStr("00" Round(Mod(m := Mod(r, 1) * 60, 1) * 60, 0), -1)
	m := SubStr("00" Round(m // 1, 0), -1)
	return, h ":" m ":" s
}

atan2(x, y) {
   return dllcall("msvcrt\atan2", "Double",y, "Double",x, "CDECL Double")
}
```

{{out}}

```txt
The mean time is: 23:47:43
```



## AWK


```AWK
#!/usr/bin/awk -f
{
    c = atan2(0,-1)/(12*60*60);
    x=0.0; y=0.0;
    for (i=1; i<=NF; i++) {	
        split($i,a,":");
	p = (a[1]*3600+a[2]*60+a[3])*c;
	x += sin(p);
	y += cos(p);
    }
    p = atan2(x,y)/c;	
    if (p<0) p += 24*60*60;
    print strftime("%T",p,1);
}
```


```txt
$ echo 23:00:17, 23:40:20, 00:12:45, 00:17:19 | awk -f mean_time_of_day.awk
23:47:43
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      nTimes% = 4
      DATA 23:00:17, 23:40:20, 00:12:45, 00:17:19
      
      DIM angles(nTimes%-1)
      FOR N% = 0 TO nTimes%-1
        READ tim$
        angles(N%) = FNtimetoangle(tim$)
      NEXT
      PRINT "Mean time is " FNangletotime(FNmeanangle(angles(), nTimes%))
      END
      
      DEF FNtimetoangle(t$)
      LOCAL A%, I%
      REPEAT
        A% = A% * 60 + VAL(t$)
        I% = INSTR(t$, ":")
        t$ = MID$(t$, I%+1)
      UNTIL I% = 0
      = A% / 240 - 180
      
      DEF FNangletotime(a)
      LOCAL A%, I%, t$
      A% = INT((a + 180) * 240 + 0.5)
      FOR I% = 1 TO 3
        t$ = RIGHT$("0" + STR$(A% MOD 60), 2) + ":" + t$
        A% DIV= 60
      NEXT
      = LEFT$(t$)
      
      DEF FNmeanangle(angles(), N%)
      LOCAL I%, addsin, addcos
      FOR I% = 0 TO N%-1
        addsin += SINRADangles(I%)
        addcos += COSRADangles(I%)
      NEXT
      = DEGFNatan2(addsin, addcos)
      
      DEF FNatan2(y,x) : ON ERROR LOCAL = SGN(y)*PI/2
      IF x>0 THEN = ATN(y/x) ELSE IF y>0 THEN = ATN(y/x)+PI ELSE = ATN(y/x)-PI
```

{{out}}

```txt

Mean time is 23:47:43

```



## C


```C>#include<stdlib.h

#include<math.h>
#include<stdio.h>

typedef struct
{
  int hour, minute, second;
} digitime;

double
timeToDegrees (digitime time)
{
  return (360 * time.hour / 24.0 + 360 * time.minute / (24 * 60.0) +
          360 * time.second / (24 * 3600.0));
}

digitime
timeFromDegrees (double angle)
{
  digitime d;
  double totalSeconds = 24 * 60 * 60 * angle / 360;

  d.second = (int) totalSeconds % 60;
  d.minute = ((int) totalSeconds % 3600 - d.second) / 60;
  d.hour = (int) totalSeconds / 3600;

  return d;
}

double
meanAngle (double *angles, int size)
{
  double y_part = 0, x_part = 0;
  int i;

  for (i = 0; i < size; i++)
    {
      x_part += cos (angles[i] * M_PI / 180);
      y_part += sin (angles[i] * M_PI / 180);
    }

  return atan2 (y_part / size, x_part / size) * 180 / M_PI;
}

int
main ()
{
  digitime *set, meanTime;
  int inputs, i;
  double *angleSet, angleMean;

  printf ("Enter number of inputs : ");
  scanf ("%d", &inputs);
  set = malloc (inputs * sizeof (digitime));
  angleSet = malloc (inputs * sizeof (double));
  printf ("\n\nEnter the data separated by a space between each unit : ");

  for (i = 0; i < inputs; i++)
    {
      scanf ("%d:%d:%d", &set[i].hour, &set[i].minute, &set[i].second);
      angleSet[i] = timeToDegrees (set[i]);
    }

  meanTime = timeFromDegrees (360 + meanAngle (angleSet, inputs));

  printf ("\n\nThe mean time is : %d:%d:%d", meanTime.hour, meanTime.minute,
          meanTime.second);
  return 0;
}
```

{{out}}

```txt

Enter number of inputs : 4


Enter the data separated by a space between each unit : 23:00:17 23:40:20 00:12:45 00:17:19


The mean time is : 23:47:43
```

----


## C sharp


```csharp
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RosettaCode
{
    class Program
    {
        static void Main(string[] args)
        {
            Func<TimeSpan, double> TimeToDegrees = (time) => 
                360 * time.Hours / 24.0 +
                360 * time.Minutes / (24 * 60.0) +
                360 * time.Seconds / (24 * 3600.0);
            Func<List<double>, double> MeanAngle = (angles) =>
                {
                    double y_part = 0.0d, x_part = 0.0d;
                    int numItems = angles.Count;

                    for (int i = 0; i < numItems; i++)
                    {
                        x_part += Math.Cos(angles[i] * Math.PI / 180);
                        y_part += Math.Sin(angles[i] * Math.PI / 180);
                    }

                    return Math.Atan2(y_part / numItems, x_part / numItems) * 180 / Math.PI;
                };
            Func<double, TimeSpan> TimeFromDegrees = (angle) =>
                    new TimeSpan(
                        (int)(24 * 60 * 60 * angle / 360) / 3600, 
                        ((int)(24 * 60 * 60 * angle / 360) % 3600 - (int)(24 * 60 * 60 * angle / 360) % 60) / 60, 
                        (int)(24 * 60 * 60 * angle / 360) % 60);
            List<double> digitimes = new List<double>();
            TimeSpan digitime;
            string input;

            Console.WriteLine("Enter times, end with no input: ");
            do
            {
                input = Console.ReadLine();
                if (!(string.IsNullOrWhiteSpace(input)))
                {
                    if (TimeSpan.TryParse(input, out digitime))
                        digitimes.Add(TimeToDegrees(digitime));
                    else
                        Console.WriteLine("Seems this is wrong input: ignoring time");
                }
            } while (!string.IsNullOrWhiteSpace(input));

            if(digitimes.Count() > 0)
                Console.WriteLine("The mean time is : {0}", TimeFromDegrees(360 + MeanAngle(digitimes)));
        }
    }
}

```

{{out}}

```txt

Enter times, end with no input:
23:00:17
23:40:20
00:12:45
00:17:19

The mean time is : 23:47:43

```



## D

{{trans|Python}}

```d
import std.stdio, std.range, std.algorithm, std.complex, std.math,
       std.format, std.conv;

double radians(in double d) pure nothrow @safe @nogc {
    return d * PI / 180;
}

double degrees(in double r) pure nothrow @safe @nogc {
    return r * 180 / PI;
}

double meanAngle(in double[] deg) pure nothrow @safe @nogc {
    return (deg.map!(d => fromPolar(1, d.radians)).sum / deg.length).arg.degrees;
}

string meanTime(in string[] times) pure @safe {
    auto t = times.map!(times => times.split(':').to!(int[3]));
    assert(t.all!(hms => 24.iota.canFind(hms[0]) &&
                         60.iota.canFind(hms[1]) &&
                         60.iota.canFind(hms[2])),
           "Invalid time");

    auto seconds = t.map!(hms => hms[2] + hms[1] * 60 + hms[0] * 3600);
    enum day = 24 * 60 * 60;
    const to_angles = seconds.map!(s => s * 360.0 / day).array;

    immutable mean_as_angle = to_angles.meanAngle;
    auto mean_seconds_fp = mean_as_angle * day / 360.0;
    if (mean_seconds_fp < 0)
        mean_seconds_fp += day;
    immutable mean_seconds = mean_seconds_fp.to!uint;
    immutable h = mean_seconds / 3600;
    immutable m = mean_seconds % 3600;
    return "%02d:%02d:%02d".format(h, m / 60, m % 60);
}

void main() @safe {
    ["23:00:17", "23:40:20", "00:12:45", "00:17:19"].meanTime.writeln;
}
```

{{out}}

```txt
23:47:43
```




## EchoLisp


```scheme

;; string hh:mm:ss to radians
(define (time->radian time) 
    (define-values (h m s) (map string->number (string-split time ":")))
    (+  (* h (/ PI 12)) (* m (/ PI 12 60)) (* s (/ PI 12 3600))))
			
;; radians to string hh:mm;ss
(define (radian->time rad)
	(when (< rad 0) (+= rad (* 2 PI)))
	(define t (round (/ (* 12 3600 rad) PI)))
	(define h (quotient t 3600))
	(define m (quotient (- t (* h 3600)) 60))
	(define s (- t (* 3600 h) (* 60 m)))
	(string-join (map number->string (list h m s)) ":"))
	
(define (mean-time times)
	(radian->time 
	 (angle 
	  (for/sum ((t times)) (make-polar 1 (time->radian t))))))
	
(mean-time '{"23:00:17" "23:40:20" "00:12:45" "00:17:19"})
    →  "23:47:43"

```



## Erlang


```Erlang

-module( mean_time_of_day ).
-export( [from_times/1, task/0] ).

from_times( Times ) ->
	Seconds = [seconds_from_time(X) || X <- Times],
	Degrees = [degrees_from_seconds(X) || X <- Seconds],
	Average = mean_angle:from_degrees( Degrees ),
	time_from_seconds( seconds_from_degrees(Average) ).

task() ->
	Times = ["23:00:17", "23:40:20", "00:12:45", "00:17:19"],
	io:fwrite( "The mean time of  ~p is: ~p~n", [Times, from_times(Times)] ).



degrees_from_seconds( Seconds ) when Seconds < (24 * 3600) -> (Seconds * 360) / (24 * 3600).

seconds_from_degrees( Degrees ) when Degrees < 0 -> seconds_from_degrees( Degrees + 360 );
seconds_from_degrees( Degrees ) when Degrees < 360 -> (Degrees * 24 * 3600) / 360.

seconds_from_time( Time ) ->
	{ok, [Hours, Minutes, Seconds], _Rest} = io_lib:fread( "~d:~d:~d", Time ),
	Hours * 3600 + Minutes * 60 + Seconds.

time_from_seconds( Seconds_float ) ->
	Seconds = erlang:round( Seconds_float ),
	Hours = Seconds div 3600,
	Minutes = (Seconds - (Hours * 3600)) div 60,
	Secs = Seconds - (Hours * 3600) - (Minutes * 60),
	lists:flatten( io_lib:format("~2.10.0B:~2.10.0B:~2.10.0B", [Hours, Minutes, Secs]) ).

```

{{out}}

```txt

17> mean_time_of_day:task().
The mean time of  ["23:00:17","23:40:20","00:12:45","00:17:19"] is: "23:47:43"

```





## Euphoria

{{works with|OpenEuphoria}}

```Euphoria

include std/console.e
include std/math.e
include std/mathcons.e
include std/sequence.e
include std/get.e

function T2D(sequence TimeSeq)
	return (360 * TimeSeq[1] / 24 + 360 * TimeSeq[2] / (24 * 60) +
			360 * TimeSeq[3] / (24 * 3600))
end function

function D2T(atom angle)
	sequence TimeSeq = {0,0,0}
	atom seconds = 24 * 60 * 60 * angle / 360
	
	TimeSeq[3] = mod(seconds,60)
	TimeSeq[2] = (mod(seconds,3600) - TimeSeq[3]) / 60
	TimeSeq[1] = seconds / 3600
	
	return TimeSeq
end function

function MeanAngle(sequence angles)
	atom x = 0, y = 0
	integer l = length(angles)
	
	for i = 1 to length(angles) do
		x += cos(angles[i] * PI / 180)
		y += sin(angles[i] * PI / 180)
	end for
	
	return atan2(y / l, x / l) * 180 / PI
end function

sequence TimeEntry, TimeList = {}, TimeSeq

puts(1,"Enter times.  Enter with no input to end\n")
while 1 do
 TimeEntry = prompt_string("")
 if equal(TimeEntry,"") then  -- no more entries
 	for i = 1 to length(TimeList) do
 		TimeList[i] = split(TimeList[i],":") -- split the times into sequences 
 		for j = 1 to 3 do
 			TimeList[i][j] = defaulted_value(TimeList[i][j],0) -- convert to numerical values
 		end for
 	end for
 	exit
 end if
 TimeList = append(TimeList,TimeEntry)
end while

sequence AngleList = repeat({},length(TimeList))

for i = 1 to length(AngleList) do
	AngleList[i] = T2D(TimeList[i])
end for

sequence MeanTime = D2T(360+MeanAngle(AngleList))

printf(1,"\nMean Time: %d:%d:%d\n",MeanTime)

	
if getc(0) then end if	

```

{{out}}

```txt

Enter Times.  Enter with no input to end.
23:00:17
23:40:20
00:12:45
00:17:19

Mean Time: 23:47:43

```


=={{header|F_Sharp|F#}}==

```fsharp
open System
open System.Numerics

let deg2rad d = d * Math.PI / 180.
let rad2deg r = r * 180. / Math.PI
let makeComplex = fun r ->  Complex.FromPolarCoordinates(1., r)
// 1 msec = 10000 ticks
let time2deg = TimeSpan.Parse >> (fun ts -> ts.Ticks) >> (float) >> (*) (10e-9/24.)
let deg2time = (*) (24. * 10e7) >> (int64) >> TimeSpan

[<EntryPoint>]
let main argv =
    let msg = "Average time for [" + (String.Join("; ",argv)) + "] is"
    argv
    |> Seq.map (time2deg >> deg2rad >> makeComplex)
    |> Seq.fold (fun x y -> Complex.Add(x,y)) Complex.Zero
    |> fun c -> c.Phase |> rad2deg
    |> fun d -> if d < 0. then d + 360. else d
    |> deg2time |> fun t -> t.ToString(@"hh\:mm\:ss")
    |> printfn "%s: %s" msg 
    0
```

{{out}}

```txt
>RosettaCode 23:00:17 23:40:20 00:12:45 00:17:19
Average time for [23:00:17; 23:40:20; 00:12:45; 00:17:19] is: 23:47:43
```



## Factor


```factor
USING: arrays formatting kernel math math.combinators
math.functions math.libm math.parser math.trig qw sequences
splitting ;
IN: rosetta-code.mean-time

CONSTANT: input qw{ 23:00:17 23:40:20 00:12:45 00:17:19 }

: time>deg ( hh:mm:ss -- x )
    ":" split [ string>number ] map first3
    [ 15 * ] [ 1/4 * ] [ 1/240 * ] tri* + + ;

: mean-angle ( seq -- x )
    [ deg>rad ] map [ [ sin ] map-sum ] [ [ cos ] map-sum ]
    [ length ] tri recip [ * ] curry bi@ fatan2 rad>deg ;

: cutf ( x -- str y )
    [ >integer number>string ] [ dup floor - ] bi ;

: mean-time ( seq -- str )
    [ time>deg ] map mean-angle [ 360 + ] when-negative 24 *
    360 / cutf 60 * cutf 60 * round cutf drop 3array ":" join ; 

: mean-time-demo ( -- )
    input dup mean-time "Mean time for %u is %s.\n" printf ;

MAIN: mean-time-demo
```

{{out}}

```txt

Mean time for { "23:00:17" "23:40:20" "00:12:45" "00:17:19" } is 23:47:43.

```



## Fortran

{{works with|gfortran 5.1.0}}

```fortran

program mean_time_of_day
  implicit none
  integer(kind=4), parameter :: dp = kind(0.0d0)

  type time_t
    integer(kind=4) :: hours, minutes, seconds
  end type

  character(len=8), dimension(4), parameter :: times = &
    (/ '23:00:17', '23:40:20', '00:12:45', '00:17:19' /)
  real(kind=dp), dimension(size(times)) :: angles
  real(kind=dp) :: mean

  angles = time_to_angle(str_to_time(times))
  mean = mean_angle(angles)
  if (mean < 0) mean = 360 + mean

  write(*, fmt='(I2.2, '':'', I2.2, '':'', I2.2)') angle_to_time(mean)
contains
  real(kind=dp) function mean_angle(angles)
    real(kind=dp), dimension(:), intent (in) :: angles
    real(kind=dp) :: x, y

    x = sum(sin(radians(angles)))/size(angles)
    y = sum(cos(radians(angles)))/size(angles)

    mean_angle = degrees(atan2(x, y))
  end function

  elemental real(kind=dp) function radians(angle)
    real(kind=dp), intent (in) :: angle
    real(kind=dp), parameter :: pi = 4d0*atan(1d0)
    radians = angle/180*pi
  end function

  elemental real(kind=dp) function degrees(angle)
    real(kind=dp), intent (in) :: angle
    real(kind=dp), parameter :: pi = 4d0*atan(1d0)
    degrees = 180*angle/pi
  end function

  elemental type(time_t) function str_to_time(str)
    character(len=*), intent (in) :: str
    ! Assuming time in format hh:mm:ss
    read(str, fmt='(I2, 1X, I2, 1X, I2)') str_to_time
  end function

  elemental real(kind=dp) function time_to_angle(time) result (res)
    type(time_t), intent (in) :: time

    real(kind=dp) :: seconds
    real(kind=dp), parameter :: seconds_in_day = 24*60*60

    seconds = time%seconds + 60*time%minutes + 60*60*time%hours
    res = 360*seconds/seconds_in_day
  end function

  elemental type(time_t) function angle_to_time(angle)
    real(kind=dp), intent (in) :: angle

    real(kind=dp) :: seconds
    real(kind=dp), parameter :: seconds_in_day = 24*60*60

    seconds = seconds_in_day*angle/360d0
    angle_to_time%hours = int(seconds/60d0/60d0)
    seconds = mod(seconds, 60d0*60d0)
    angle_to_time%minutes = int(seconds/60d0)
    angle_to_time%seconds = mod(seconds, 60d0)
  end function
end program

```

{{out}}

```txt

23:47:43

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Const pi As Double = 3.1415926535897932
 
Function meanAngle(angles() As Double) As Double
  Dim As Integer length = Ubound(angles) - Lbound(angles) + 1
  Dim As Double sinSum = 0.0
  Dim As Double cosSum = 0.0
  For i As Integer = LBound(angles) To UBound(angles)
    sinSum += Sin(angles(i) * pi / 180.0)
    cosSum += Cos(angles(i) * pi / 180.0)    
  Next
  Return Atan2(sinSum / length, cosSum / length) * 180.0 / pi
End Function

' time string assumed to be in format "hh:mm:ss"
Function timeToSecs(t As String) As Integer
  Dim As Integer hours = Val(Left(t, 2))
  Dim As Integer mins =  Val(Mid(t, 4, 2))
  Dim As Integer secs =  Val(Right(t, 2))
  Return 3600 * hours + 60 * mins + secs  
End Function

' 1 second of time = 360/(24 * 3600) = 1/240th degree
Function timeToDegrees(t As String) As Double
  Dim secs As Integer = timeToSecs(t)
  Return secs/240.0
End Function

Function degreesToTime(d As Double) As String
  If d < 0 Then d += 360.0 
  Dim secs As Integer  = d * 240.0
  Dim hours As Integer = secs \ 3600
  Dim mins As Integer  = secs Mod 3600
  secs = mins Mod 60
  mins = mins \ 60
  Dim hBuffer As String = Right("0" + Str(hours), 2)
  Dim mBuffer As String = Right("0" + Str(mins), 2)
  Dim sBuffer As String = Right("0" + Str(secs), 2)
  Return hBuffer + ":" + mBuffer + ":" + sBuffer
End Function

Dim tm(1 To 4) As String = {"23:00:17", "23:40:20", "00:12:45", "00:17:19"}
Dim angles(1 To 4) As Double

For i As Integer = 1 To 4
  angles(i) = timeToDegrees(tm(i))
Next

Dim mean As Double = meanAngle(angles())
Print "Average time is : "; degreesToTime(mean)
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

Average time is : 23:47:43

```



## Go


```go
package main

import (
    "errors"
    "fmt"
    "log"
    "math"
    "time"
)

var inputs = []string{"23:00:17", "23:40:20", "00:12:45", "00:17:19"}

func main() {
    tList := make([]time.Time, len(inputs))
    const clockFmt = "15:04:05"
    var err error
    for i, s := range inputs {
        tList[i], err = time.Parse(clockFmt, s)
        if err != nil {
            log.Fatal(err)
        }
    }
    mean, err := meanTime(tList)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(mean.Format(clockFmt))
}

func meanTime(times []time.Time) (mean time.Time, err error) {
    if len(times) == 0 {
        err = errors.New("meanTime: no times specified")
        return
    }
    var ssum, csum float64
    for _, t := range times {
        h, m, s := t.Clock()
        n := t.Nanosecond()
        fSec := (float64((h*60+m)*60+s) + float64(n)*1e-9)
        sin, cos := math.Sincos(fSec * math.Pi / (12 * 60 * 60))
        ssum += sin
        csum += cos
    }
    if ssum == 0 && csum == 0 {
        err = errors.New("meanTime: mean undefined")
        return
    }
    _, dayFrac := math.Modf(1 + math.Atan2(ssum, csum)/(2*math.Pi))
    return mean.Add(time.Duration(dayFrac * 24 * float64(time.Hour))), nil
}
```

{{out}}

```txt

23:47:43

```



## Groovy

Solution:

```groovy
import static java.lang.Math.*

final format = 'HH:mm:ss',  clock = PI / 12,  millisPerHr = 3600*1000
final tzOffset = new Date(0).timezoneOffset / 60
def parseTime = { time -> (Date.parse(format, time).time / millisPerHr) - tzOffset }
def formatTime = { time -> new Date((time + tzOffset) * millisPerHr as int).format(format) }
def mean = { list, closure -> list.sum(closure)/list.size() }
def meanTime = { ... timeStrings ->
    def times = timeStrings.collect(parseTime)
    formatTime(atan2( mean(times) { sin(it * clock) }, mean(times) { cos(it * clock) }) / clock)
}
```


Test:

```groovy
println (meanTime("23:00:17", "23:40:20", "00:12:45", "00:17:19"))
```


{{out}}

```txt
23:47:43
```



## Haskell


```haskell
import Data.Complex (cis, phase)
import Data.List.Split (splitOn)
import Text.Printf (printf)

timeToRadians :: String -> Float 
timeToRadians time = 
        let hours:minutes:seconds:_ = splitOn ":" time
            s = fromIntegral (read seconds :: Int)
            m = fromIntegral (read minutes :: Int)
            h = fromIntegral (read hours :: Int)
        in  (2*pi)*(h+ (m + s/60.0 )/60.0 )/24.0
   
radiansToTime :: Float -> String
radiansToTime  r =
        let tau = pi*2
            (_,fDay) = properFraction (r / tau) :: (Int, Float)
            fDayPositive = if fDay < 0 then 1.0+fDay else fDay 
            (hours, fHours) = properFraction $ 24.0 * fDayPositive
            (minutes, fMinutes) = properFraction $ 60.0 * fHours
            seconds = 60.0 * fMinutes
        in printf "%0d" (hours::Int) ++ ":" ++ printf "%0d" (minutes::Int) ++ ":" ++ printf "%0.0f" (seconds::Float)
 
meanAngle :: [Float] -> Float
meanAngle = phase . sum . map cis
 
main :: IO ()        
main = putStrLn $ radiansToTime $ meanAngle $ map timeToRadians ["23:00:17", "23:40:20", "00:12:45", "00:17:19"]

```

{{out}}

```txt
23:47:43
```


=={{header|Icon}} and {{header|Unicon}}==

<lang>procedure main(A)
    every put(B := [], ct2a(!A))
    write(ca2t(meanAngle(B)))
end

procedure ct2a(t)
    t ? {s := ((1(move(2),move(1))*60 + 1(move(2),move(1)))*60 + move(2))}
    return (360.0/86400.0) * s
end

procedure ca2t(a)
    while a < 0 do a +:= 360.0
    t := integer((86400.0/360.0)*a + 0.5)
    s := left(1(.t % 60, t /:= 60),2,"0")
    s := left(1(.t % 60, t /:= 60),2,"0")||":"||s
    s := left(t,2,"0")||":"||s
    return s
end

procedure meanAngle(A)
    every (sumSines := 0.0) +:= sin(dtor(!A))
    every (sumCosines := 0.0) +:= cos(dtor(!A))
    return rtod(atan(sumSines/*A,sumCosines/*A))
end
```


Sample run:


```txt

->amtod 23:00:17 23:40:20 00:12:45 00:17:19
23:47:43
->

```



## J

use <code>avgAngleR</code> from [[Averages/Mean angle#J]]

```J
require 'types/datetime'
parseTimes=: ([: _&".;._2 ,&':');._2
secsFromTime=:  24 60 60 #. ]     NB. convert from time to seconds
rft=: 2r86400p1 * secsFromTime    NB. convert from time to radians
meanTime=: 'hh:mm:ss' fmtTime [: secsFromTime [: avgAngleR&.rft parseTimes
```

{{out|Example Use}}

```J
   meanTime '23:00:17 23:40:20 00:12:45 00:17:19 '
23:47:43
```



## Java

{{trans|Kotlin}}

```java
public class MeanTimeOfDay {
    
    static double meanAngle(double[] angles) {
        int len = angles.length;
        double sinSum = 0.0;
        for (int i = 0; i < len; i++) {
            sinSum += Math.sin(angles[i] * Math.PI / 180.0);
        }
 
        double cosSum = 0.0;
        for (int i = 0; i < len; i++) {
            cosSum += Math.cos(angles[i] * Math.PI / 180.0);
        }

        return Math.atan2(sinSum / len, cosSum / len) * 180.0 / Math.PI;
    }

    /* time string assumed to be in format "hh:mm:ss" */
    static int timeToSecs(String t) {
        int hours = Integer.parseInt(t.substring(0, 2));
        int mins  = Integer.parseInt(t.substring(3, 5));
        int secs  = Integer.parseInt(t.substring(6, 8));
        return 3600 * hours + 60 * mins + secs;
    }

    /* 1 second of time = 360/(24 * 3600) = 1/240th degree */
    static double timeToDegrees(String t) {
        return timeToSecs(t) / 240.0;
    }

    static String degreesToTime(double d) {
        if (d < 0.0) d += 360.0;
        int secs  = (int)(d * 240.0);
        int hours = secs / 3600;
        int mins  = secs % 3600;
        secs = mins % 60;
        mins /= 60;
        return String.format("%2d:%2d:%2d", hours, mins, secs);
    }

    public static void main(String[] args) {
        String[] tm = {"23:00:17", "23:40:20", "00:12:45", "00:17:19"};
        double[] angles = new double[4];
        for (int i = 0; i < 4; i++) angles[i] = timeToDegrees(tm[i]);        
        double mean = meanAngle(angles);
        System.out.println("Average time is : " + degreesToTime(mean));
    }
}
```


{{out}}

```txt

Average time is : 23:47:43

```



## Javascript

{{works with|Node.js}}

```Javascript
var args  = process.argv.slice(2);

function time_to_seconds( hms ) {
    var parts = hms.split(':');
    if ( parts[0] < 12 ) {
        parts[0] = 24;
    }
    var seconds = parseInt(parts[0]) * 60 * 60 + parseInt(parts[1]) * 60 + parseInt(parts[2]);
    return seconds;
}
function seconds_to_time( s ) {
    var h = Math.floor( s/(60 * 60) );
    if ( h < 10 ) {
        h = '0' + h;
    }
    s = s % (60 * 60);

    var m = Math.floor( s/60 );
    if ( m < 10 ) {
        m = '0' + m;
    }
    s = s % 60
    if ( s < 10 ) {
        s = '0' + s;
    }
    return h + ':' + m + ':' + s;
}

var sum = 0, count = 0, idx;
for (idx in args) {
    var seconds = time_to_seconds( args[idx] );
    sum += seconds;
    count++;
}

var seconds = Math.floor( sum / count )
console.log( 'Mean time is ', seconds_to_time(seconds));

```

{{out}}

```txt

$ node mean_time.js 23:00:17 23:40:20 00:12:45 00:17:19
Mean time is  23:47:40
```



## jq

{{works with|jq|1.4}}

The "mean time" of two times that differ by 12 hours (e.g. ["00:00:00", "12:00:00"]) is not very well-defined, and is accordingly computed as null here.

```jq
# input: array of "h:m:s"
def mean_time_of_day:
  def pi: 4 * (1|atan);
  def to_radians:   pi * . /(12*60*60);
  def from_radians: (. * 12*60*60) / pi;

  def secs2time:  # produce "hh:mm:ss" string
    def pad: tostring | (2 - length) * "0" + .;
    "\(./60/60 % 24 | pad):\(./60 % 60 | pad):\(. % 60 | pad)";

  def round:
    if . < 0 then -1 * ((- .) | round) | if . == -0 then 0 else . end
    else floor as $x
    | if (. - $x) < 0.5 then $x else $x+1 end
    end;

  map( split(":")
       | map(tonumber)
       | (.[0]*3600 + .[1]*60 + .[2])
       | to_radians )
   | (map(sin) | add) as $y
   | (map(cos) | add) as $x
   | if $x == 0 then (if $y > 3e-14 then pi/2 elif $y < -3e-14 then -(pi/2) else null end)
     else ($y / $x) | atan
     end
   | if . == null then null
     else from_radians
     | if (.<0) then . + (24*60*60) else . end
     | round
     | secs2time
     end ;
```

'''Examples'''

```jq
["0:0:0", "12:0:0" ],
["0:0:0", "24:0:0" ],
["1:0:0",  "1:0:0" ],
["20:0:0", "4:0:0" ],
["20:0:0", "4:0:2" ],
["23:0:0", "23:0:0" ],
["23:00:17", "23:40:20", "00:12:45", "00:17:19"]
| mean_time_of_day
```

{{out}}

```sh
$ jq -r -n -f Mean_time_of_day.jq
null
00:00:00
01:00:00
00:00:00
00:00:01
23:00:00
23:47:43
```



## Julia

{{works with|Julia|0.6}}
{{trans|MATLAB}}

```julia
using Statistics

function meantime(times::Array, dlm::String=":")
    c = π / (12 * 60 * 60)
    a = map(x -> parse.(Int, x), split.(times, dlm))
    ϕ = collect(3600t[1] + 60t[2] + t[3] for t in a)
    d = angle(mean(exp.(c * im * ϕ))) / 2π # days
    if d < 0 d += 1 end
    # Convert to h:m:s
    h = trunc(Int, d * 24)
    m = trunc(Int, d * 24 * 60) - h * 60
    s = trunc(Int, d * 24 * 60 * 60) - h * 60 * 60 - m * 60
    return "$h:$m:$s"
end

times = String["23:00:17", "23:40:20", "00:12:45", "00:17:19"]
mtime = meantime(times)
println("Times:")
println.(times)
println("Mean: $mtime")
```


{{out}}

```txt
Times:
23:00:17
23:40:20
00:12:45
00:17:19
Mean: 23:47:43
```



## Kotlin

{{trans|FreeBASIC}}

```scala
// version 1.0.6

fun meanAngle(angles: DoubleArray): Double {
    val sinSum = angles.sumByDouble {  Math.sin(it * Math.PI / 180.0) }
    val cosSum = angles.sumByDouble {  Math.cos(it * Math.PI / 180.0) }
    return Math.atan2(sinSum / angles.size, cosSum / angles.size) * 180.0 / Math.PI
}

/* time string assumed to be in format "hh:mm:ss" */
fun timeToSecs(t: String): Int {
    val hours = t.slice(0..1).toInt()
    val mins  = t.slice(3..4).toInt()
    val secs  = t.slice(6..7).toInt()
    return 3600 * hours + 60 * mins + secs
}

/* 1 second of time = 360/(24 * 3600) = 1/240th degree */
fun timeToDegrees(t: String): Double = timeToSecs(t) / 240.0

fun degreesToTime(d: Double): String {
    var dd = d
    if (dd < 0.0) dd += 360.0
    var secs  = (dd * 240.0).toInt()
    val hours = secs / 3600
    var mins  = secs % 3600
    secs  = mins % 60
    mins /= 60
    return String.format("%2d:%2d:%2d", hours, mins, secs)
}

fun main(args: Array<String>) {
    val tm = arrayOf("23:00:17", "23:40:20", "00:12:45", "00:17:19")
    val angles = DoubleArray(4) { timeToDegrees(tm[it]) }
    val mean = meanAngle(angles)
    println("Average time is : ${degreesToTime(mean)}")
}
```


{{out}}

```txt

Average time is : 23:47:43

```



## Liberty BASIC


```lb

global pi
pi = acs(-1)

Print "Average of:"
for i = 1 to 4
    read t$ 
    print t$
    a=time2angle(t$)
    ss=ss+sin(a)
    sc=sc+cos(a)
next
a=atan2(ss,sc)
if a<0 then a=a+2*pi
print "is ";angle2time$(a)

end
data "23:00:17", "23:40:20", "00:12:45", "00:17:19"

function nn$(n)
    nn$=right$("0";n, 2)
end function

function angle2time$(a)
    a=int(a/2/pi*24*60*60)
    ss=a mod 60
    a=int(a/60)
    mm=a mod 60
    hh=int(a/60)
    angle2time$=nn$(hh);":";nn$(mm);":";nn$(ss)
end function

function time2angle(time$)
    hh=val(word$(time$,1,":"))
    mm=val(word$(time$,2,":"))
    ss=val(word$(time$,3,":"))
    time2angle=2*pi*(60*(60*hh+mm)+ss)/24/60/60
end function

function atan2(y, x)
    On Error GoTo [DivZero]      'If y is 0 catch division by zero error
        atan2 = (2 * (atn((sqr((x * x) + (y * y)) - x)/ y)))
        exit function
    [DivZero]
        atan2 = (y=0)*(x<0)*pi
End Function

```


{{out}}

```txt

Average of:
23:00:17
23:40:20
00:12:45
00:17:19
is 23:47:43 

```




## Lua


```lua

local times = {"23:00:17","23:40:20","00:12:45","00:17:19"}

-- returns time converted to a radian format
local function timeToAngle(str)
	local h,m,s = str:match("(..):(..):(..)")
	return (h + m / 60 + s / 3600)/12 * math.pi
end

-- computes the mean of the angles inside a list
local function meanAngle(angles)
	local sumSin,sumCos = 0,0
	for k,v in pairs(angles) do
		sumSin = sumSin + math.sin(v)
		sumCos = sumCos + math.cos(v)
	end
	return math.atan2(sumSin,sumCos)
end

-- converts and angle back to a time string
local function angleToTime(angle)
	local abs = angle % (math.pi * 2)
	local time = abs / math.pi * 12
	local h = math.floor(time)
	local m = math.floor(time * 60) % 60
	local s = math.floor(time * 3600) % 60
	return string.format("%02d:%02d:%02d", h, m, s)
end

-- convert times to angles
for k,v in pairs(times) do
	times[k] = timeToAngle(v)
end

print(angleToTime(meanAngle(times)))

```


{{out}}

```txt

23:47:43 

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```mathematica
meanTime[list_] := 
  StringJoin@
   Riffle[ToString /@ 
       Floor@{Mod[24 #, 24], Mod[24*60 #, 60], Mod[24*60*60 #, 60]} &[
     Arg[Mean[
        Exp[FromDigits[ToExpression@StringSplit[#, ":"], 60] & /@ 
            list/(24*60*60) 2 Pi I]]]/(2 Pi)], ":"];
meanTime[{"23:00:17", "23:40:20", "00:12:45", "00:17:19"}]
```

{{Out}}

```txt
23:47:43
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function t = mean_time_of_day(t) 
    c = pi/(12*60*60);
    for k=1:length(t)
	a = sscanf(t{k},'%d:%d:%d');
	phi(k) = (a(1)*3600+a(2)*60+a(3));
    end;
    d = angle(mean(exp(i*phi*c)))/(2*pi); % days 
    if (d<0) d += 1;
    t = datestr(d,"HH:MM:SS");
end; 
```


```txt
mean_time_of_day({'23:00:17', '23:40:20', '00:12:45', '00:17:19'})
ans = 23:47:43

```



## Nim


```nim
import math, complex, strutils, sequtils
 
proc rect(r, phi: float): Complex = (r * cos(phi), sin(phi))
proc phase(c: Complex): float = arctan2(c.im, c.re)
 
proc radians(x: float): float = (x * Pi) / 180.0
proc degrees(x: float): float = (x * 180.0) / Pi
 
proc meanAngle(deg: openArray[float]): float =
  var c: Complex
  for d in deg:
    c += rect(1.0, radians(d))
  degrees(phase(c / float(deg.len)))
 
proc meanTime(times: openArray[string]): string =
  const day = 24 * 60 * 60
  let
    angles = times.map(proc(time: string): float =
      let t = time.split(":")
      (t[2].parseInt + t[1].parseInt * 60 + t[0].parseInt * 3600) * 360 / day)
    ms = (angles.meanAngle * day / 360 + day) mod day
    (h,m,s) = (ms.int div 3600, (ms.int mod 3600) div 60, ms.int mod 60)
 
  align($h, 2, '0') & ":" & align($m, 2, '0') & ":" & align($s, 2, '0')
 
echo meanTime(["23:00:17", "23:40:20", "00:12:45", "00:17:19"])
```

{{out}}

```txt
23:47:43
```


=={{header|Oberon-2}}==
{{works with|oo2c}}

```oberon2

MODULE AvgTimeOfDay;
IMPORT
  M := LRealMath,
  T := NPCT:Tools,
  Out := NPCT:Console;

CONST
  secsDay = 86400;
  secsHour = 3600;
  secsMin = 60;

  toRads = M.pi / 180;

VAR
  h,m,s: LONGINT;
  data: ARRAY 4 OF LONGREAL;

PROCEDURE TimeToDeg(time: STRING): LONGREAL;
VAR
  parts: ARRAY 3 OF STRING;
  h,m,s: LONGREAL;
BEGIN
  T.Split(time,':',parts);
  h := T.StrToInt(parts[0]);
  m := T.StrToInt(parts[1]);
  s := T.StrToInt(parts[2]);
  RETURN (h * secsHour + m * secsMin + s) * 360 / secsDay;
END TimeToDeg;

PROCEDURE DegToTime(d: LONGREAL; VAR h,m,s: LONGINT);
VAR
  ds: LONGREAL;
  PROCEDURE Mod(x,y: LONGREAL): LONGREAL;
  VAR
    c: LONGREAL;
  BEGIN
    c := ENTIER(x / y);
    RETURN x - c * y
  END Mod;
BEGIN
  ds :=  Mod(d,360.0) * secsDay / 360.0;
  h := ENTIER(ds / secsHour);
  m := ENTIER(Mod(ds,secsHour) / secsMin);
  s := ENTIER(Mod(ds,secsMin));
END DegToTime;

PROCEDURE Mean(g: ARRAY OF LONGREAL): LONGREAL;
VAR
  i,l: LONGINT;
  sumSin, sumCos: LONGREAL;
BEGIN
  i := 0;l := LEN(g);sumSin := 0.0;sumCos := 0.0;
  WHILE i < l  DO
    sumSin := sumSin + M.sin(g[i] * toRads);
    sumCos := sumCos + M.cos(g[i] * toRads);  
    INC(i)
  END;
  RETURN M.arctan2(sumSin / l,sumCos / l) * 180 / M.pi;
END Mean;

BEGIN
  data[0] := TimeToDeg("23:00:17");
  data[1] := TimeToDeg("23:40:20");
  data[2] := TimeToDeg("00:12:45");
  data[3] := TimeToDeg("00:17:19");

  DegToTime(Mean(data),h,m,s);
  Out.String(":> ");Out.Int(h,0);Out.Char(':');Out.Int(m,0);Out.Char(':');Out.Int(s,0);Out.Ln

END AvgTimeOfDay.

```

{{Out}}

```txt

:> 23:47:43

```



## OCaml


```ocaml
let pi_twice = 2.0 *. 3.14159_26535_89793_23846_2643
let day = float (24 * 60 * 60)

let rad_of_time t =
  t *. pi_twice /. day
 
let time_of_rad r =
  r *. day /. pi_twice

let mean_angle angles =
  let sum_sin = List.fold_left (fun sum a -> sum +. sin a) 0.0 angles
  and sum_cos = List.fold_left (fun sum a -> sum +. cos a) 0.0 angles in
  atan2 sum_sin sum_cos

let mean_time times =
  let angles = List.map rad_of_time times in
  let t = time_of_rad (mean_angle angles) in
  if t < 0.0 then t +. day else t

let parse_time t =
  Scanf.sscanf t "%d:%d:%d" (fun h m s -> float (s + m * 60 + h * 3600))

let round x = int_of_float (floor (x +. 0.5))

let string_of_time t =
  let t = round t in
  let h = t / 3600 in
  let rem = t mod 3600 in
  let m = rem / 60 in
  let s = rem mod 60 in
  Printf.sprintf "%d:%d:%d" h m s

let () =
  let times = ["23:00:17"; "23:40:20"; "00:12:45"; "00:17:19"] in
  Printf.printf "The mean time of [%s] is: %s\n"
    (String.concat "; " times)
    (string_of_time (mean_time (List.map parse_time times)))
```

{{out}}
 The mean time of [23:00:17; 23:40:20; 00:12:45; 00:17:19] is: 23:47:43


## ooRexx


```oorexx
/* REXX ---------------------------------------------------------------
* 25.06.2014 Walter Pachl
*--------------------------------------------------------------------*/
times='23:00:17 23:40:20 00:12:45 00:17:19'
sum=0
day=86400
x=0
y=0
Do i=1 To words(times)                 /* loop over times            */
  time.i=word(times,i)                 /* pick a time                */
  alpha.i=t2a(time.i)                  /* convert to angle (degrees) */
  /* Say time.i format(alpha.i,6,9) */
  x=x+rxcalcsin(alpha.i)               /* accumulate sines           */
  y=y+rxcalccos(alpha.i)               /* accumulate cosines         */
  End
ww=rxcalcarctan(x/y)                   /* compute average angle      */
ss=ww*86400/360                        /* convert to seconds         */
If ss<0 Then ss=ss+day                 /* avoid negative value       */
m=ss%60                                /* split into hh mm ss        */
s=ss-m*60
h=m%60
m=m-h*60
Say f2(h)':'f2(m)':'f2(s)              /* show the mean time         */
Exit

t2a: Procedure Expose day              /* convert time to angle      */
  Parse Arg hh ':' mm ':' ss
  sec=(hh*60+mm)*60+ss
  If sec>(day/2) Then
    sec=sec-day
  a=360*sec/day
  Return a

f2: return right(format(arg(1),2,0),2,0)

::requires rxmath library
```

{{out}}

```txt
23:47:43
```



## PARI/GP


```parigp
meanAngle(v)=atan(sum(i=1,#v,sin(v[i]))/sum(i=1,#v,cos(v[i])))%(2*Pi)
meanTime(v)=my(x=meanAngle(2*Pi*apply(u->u[1]/24+u[2]/1440+u[3]/86400, v))*12/Pi); [x\1, 60*(x-=x\1)\1, round(60*(60*x-60*x\1))]
meanTime([[23,0,17], [23,40,20], [0,12,45], [0,17,19]])
```

{{out}}

```txt
[23, 47, 43]
```



## Perl

Using the core module <code>Math::Complex</code> to enable use of complex numbers.  The <code>POSIX</code> CPAN module provides the <code>fmod</code> routine for non-integer modulus calculations.
{{trans|Perl 6}}

```Perl
use POSIX 'fmod';
use Math::Complex;
use List::Util qw(sum);
use utf8;

use constant τ => 2 * 3.1415926535;

# time-of-day to radians
sub tod2rad {
    ($h,$m,$s) = split /:/, @_[0];
    (3600*$h + 60*$m + $s) * τ / 86400;
}

# radians to time-of-day
sub rad2tod {
    my $x = $_[0] * 86400 / τ;
    sprintf '%02d:%02d:%02d', fm($x/3600,24), fm($x/60,60), fm($x,60);
}

# float modulus, normalized to positive values
sub fm  {
    my($n,$b) = @_;
    $x = fmod($n,$b);
    $x += $b if $x < 0;
}

sub phase     { arg($_[0]) }  # aka theta
sub cis       { cos($_[0]) + i*sin($_[0]) }
sub mean_time { rad2tod phase sum map { cis tod2rad $_ } @_ }

@times = ("23:00:17", "23:40:20", "00:12:45", "00:17:19");
print mean_time(@times) . " is the mean time of " . join(' ', @times) . "\n";
```

{{out}}

```txt
23:47:43 is the mean time of 23:00:17 23:40:20 00:12:45 00:17:19
```



## Perl 6

{{works with|Rakudo|2015.12}}


```perl6
sub tod2rad($_) { [+](.comb(/\d+/) Z* 3600,60,1) * tau / 86400 }
 
sub rad2tod ($r) {
    my $x = $r * 86400 / tau;
    (($x xx 3 Z/ 3600,60,1) Z% 24,60,60).fmt('%02d',':');
}
 
sub phase ($c) { $c.polar[1] }
 
sub mean-time (@t) { rad2tod phase [+] map { cis tod2rad $_ }, @t }

my @times = ["23:00:17", "23:40:20", "00:12:45", "00:17:19"];
 
say "{ mean-time(@times) } is the mean time of @times[]";
```


{{out}}

```txt

23:47:43 is the mean time of 23:00:17 23:40:20 00:12:45 00:17:19

```



## Phix


```Phix
function atan2(atom y, atom x)
    return 2*arctan((sqrt(power(x,2)+power(y,2))-x)/y)
end function
 
function MeanAngle(sequence angles)
atom x=0, y=0, ai_rad
integer l=length(angles)
 
    for i=1 to l do
        ai_rad = angles[i]*PI/180
        x += cos(ai_rad)
        y += sin(ai_rad)
    end for
    if abs(x)<1e-16 then return "not meaningful" end if
    return atan2(y,x)*180/PI
end function
 
function toSecAngle(integer hours, integer minutes, integer seconds)
    return ((hours*60+minutes)*60+seconds)/(24*60*60)*360
end function

constant Times = {toSecAngle(23,00,17),
                  toSecAngle(23,40,20),
                  toSecAngle(00,12,45),
                  toSecAngle(00,17,19)}

function toHMS(object s)
    if not string(s) then
        if s<0 then s+=360 end if
        s = 24*60*60*s/360
        s = sprintf("%02d:%02d:%02d",{floor(s/3600),floor(remainder(s,3600)/60),remainder(s,60)})
    end if
    return s
end function

printf(1,"Mean Time is %s\n",{toHMS(MeanAngle(Times))})
{} = wait_key()
```

{{out}}

```txt

Mean Time is 23:47:43

```



## PHP


```PHP

<?php
function time2ang($tim) {
        if (!is_string($tim)) return $tim;
        $parts = explode(':',$tim);
        if (count($parts)!=3) return $tim;
        $sec = ($parts[0]*3600)+($parts[1]*60)+$parts[2];
        $ang = 360.0 * ($sec/86400.0);
        return $ang;
}
function ang2time($ang) {
        if (!is_numeric($ang)) return $ang;
        $sec = 86400.0 * $ang / 360.0;
        $parts = array(floor($sec/3600),floor(($sec % 3600)/60),$sec % 60);
        $tim = sprintf('%02d:%02d:%02d',$parts[0],$parts[1],$parts[2]);
        return $tim;
}
function meanang($ang) {
        if (!is_array($ang)) return $ang;
        $sins = 0.0;
        $coss = 0.0;
        foreach($ang as $a) {
                $sins += sin(deg2rad($a));
                $coss += cos(deg2rad($a));
        }
        $avgsin = $sins / (0.0+count($ang));
        $avgcos = $coss / (0.0+count($ang));
        $avgang = rad2deg(atan2($avgsin,$avgcos));
        while ($avgang < 0.0) $avgang += 360.0;
        return $avgang;
}
$bats = array('23:00:17','23:40:20','00:12:45','00:17:19');
$angs = array();
foreach ($bats as $t) $angs[] = time2ang($t);
$ma = meanang($angs);
$result = ang2time($ma);
print "The mean time of day is $result (angle $ma).\n";
?>

```


{{out}}

```txt

The mean time of day is 23:47:43 (angle 356.9306730355).

```



## PicoLisp


```PicoLisp
(load "@lib/math.l")

(de meanTime (Lst)
   (let Tim
      (*/
         (atan2
            (sum '((S) (sin (*/ ($tim S) pi 43200))) Lst)
            (sum '((S) (cos (*/ ($tim S) pi 43200))) Lst) )
         43200 pi )
      (tim$ (% (+ Tim 86400) 86400) T) ) )
```

{{out|Test}}

```PicoLisp
: (meanTime '("23:00:17" "23:40:20" "00:12:45" "00:17:19"))
-> "23:47:43"
```



## PL/I


```pli
*process source attributes xref;
 avt: Proc options(main);
 /*--------------------------------------------------------------------
 * 25.06.2014 Walter Pachl taken from REXX
 *-------------------------------------------------------------------*/
 Dcl (addr,hbound,sin,cos,atan) Builtin;
 Dcl sysprint Print;
 Dcl times(4) Char(8) Init('23:00:17','23:40:20','00:12:45','00:17:19');
 Dcl time Char(8);
 Dcl (alpha,x,y,ss,ww) Dec Float(18) Init(0);
 Dcl day Bin Fixed(31) Init(86400);
 Dcl pi Dec Float(18) Init(3.14159265358979323846);
 Dcl (i,h,m,s) bin Fixed(31) Init(0);
 Do i=1 To hbound(times);              /* loop over times            */
  time=times(i);                       /* pick a time                */
  alpha=t2a(time);                     /* convert to angle (radians) */
  x=x+sin(alpha);                      /* accumulate sines           */
  y=y+cos(alpha);                      /* accumulate cosines         */
  End;
 ww=atan(x/y);                         /* compute average angle      */
 ss=ww*day/(2*pi);                     /* convert to seconds         */
 If ss<0 Then ss=ss+day;               /* avoid negative value       */
 m=ss/60;                              /* split into hh mm ss        */
 s=ss-m*60;
 h=m/60;
 m=m-h*60;
 Put Edit(h,':',m,':',s)(Skip,3(p'99',a));

 t2a: Procedure(t) Returns(Bin Float(18)); /* convert time to angle  */
 Dcl t Char(8);
 Dcl 1 tt Based(addr(t)),
      2 hh Pic'99',
      2 * Char(1),
      2 mm Pic'99',
      2 * Char(1),
      2 ss Pic'99';
 Dcl sec Bin Fixed(31);
 Dcl a   Bin Float(18);
 sec=(hh*60+mm)*60+ss;
 If sec>(day/2) Then
   sec=sec-day;
 a=2*pi*sec/day;
 Return (a);
 End;

 End;
```

{{out}}

```txt
23:47:43
```



## PowerShell


```PowerShell

function Get-MeanTimeOfDay
{
    [CmdletBinding()]
    [OutputType([timespan])]
    Param
    (
        [Parameter(Mandatory=$true,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true)]
        [ValidatePattern("(?:2[0-3]|[01]?[0-9])[:.][0-5]?[0-9][:.][0-5]?[0-9]")]
        [string[]]
        $Time
    )

    Begin
    {
        [double[]]$angles = @()

        function ConvertFrom-Time ([timespan]$Time)
        {
            [double]((360 * $Time.Hours / 24) + (360 * $Time.Minutes / (24 * 60)) + (360 * $Time.Seconds / (24 * 3600)))
        }

        function ConvertTo-Time ([double]$Angle)
        {
            $t = New-TimeSpan -Hours   ([int](24 * 60 * 60 * $Angle / 360) / 3600) `
                              -Minutes (([int](24 * 60 * 60 * $Angle / 360) % 3600 - [int](24 * 60 * 60 * $Angle / 360) % 60) / 60) `
                              -Seconds ([int]((24 * 60 * 60 * $Angle / 360) % 60))

            if ($t.Days -gt 0)
            {
                return ($t - (New-TimeSpan -Hours 1))
            }

            $t
        }

        function Get-MeanAngle ([double[]]$Angles)
        {
            [double]$x,$y = 0

            for ($i = 0; $i -lt $Angles.Count; $i++)
            { 
                $x += [Math]::Cos($Angles[$i] * [Math]::PI / 180)
                $y += [Math]::Sin($Angles[$i] * [Math]::PI / 180)
            }

            $result = [Math]::Atan2(($y / $Angles.Count), ($x / $Angles.Count)) * 180 / [Math]::PI

            if ($result -lt 0)
            {
                return ($result + 360)
            }

            $result
        }
    }
    Process
    {
        $angles += ConvertFrom-Time $_
    }
    End
    {
        ConvertTo-Time (Get-MeanAngle $angles)
    }
}

```


```PowerShell

[timespan]$meanTimeOfDay = "23:00:17","23:40:20","00:12:45","00:17:19" | Get-MeanTimeOfDay
"Mean time is {0}" -f (Get-Date $meanTimeOfDay.ToString()).ToString("hh:mm:ss tt")

```

{{Out}}

```txt

Mean time is 11:47:43 PM

```



## Python


```python
from cmath import rect, phase
from math import radians, degrees


def mean_angle(deg):
    return degrees(phase(sum(rect(1, radians(d)) for d in deg)/len(deg)))

def mean_time(times):
    t = (time.split(':') for time in times)
    seconds = ((float(s) + int(m) * 60 + int(h) * 3600) 
               for h, m, s in t)
    day = 24 * 60 * 60
    to_angles = [s * 360. / day for s in seconds]
    mean_as_angle = mean_angle(to_angles)
    mean_seconds = mean_as_angle * day / 360.
    if mean_seconds < 0:
        mean_seconds += day
    h, m = divmod(mean_seconds, 3600)
    m, s = divmod(m, 60)
    return '%02i:%02i:%02i' % (h, m, s)


if __name__ == '__main__':
    print( mean_time(["23:00:17", "23:40:20", "00:12:45", "00:17:19"]) )
```

{{out}}

```txt
23:47:43
```



## Racket


```racket

#lang racket
(define (mean-angle/radians as)
  (define n (length as))
  (atan (* (/ 1 n) (for/sum ([αj as]) (sin αj)))
        (* (/ 1 n) (for/sum ([αj as]) (cos αj)))))
(define (mean-time times)
  (define secs/day (* 60 60 24))
  (define (time->deg time)
    (/ (for/fold ([sum 0]) ([t (map string->number (string-split time ":"))])
         (+ (* 60 sum) t))
       secs/day 1/360 (/ 180 pi)))
  (define secs
    (modulo (inexact->exact (round (* (mean-angle/radians (map time->deg times))
                                      (/ 180 pi) 1/360 secs/day)))
            secs/day))
  (let loop ([s secs] [ts '()])
    (if (zero? s) (string-join ts ":")
        (let-values ([(q r) (quotient/remainder s 60)])
          (loop q (cons (~r r #:min-width 2 #:pad-string "0") ts))))))
(mean-time '("23:00:17" "23:40:20" "00:12:45" "00:17:19"))

```

{{out}}

```txt
"23:47:43"
```




## REXX


```rexx
/* REXX ---------------------------------------------------------------
* 25.06.2014 Walter Pachl
* taken from ooRexx using my very aged sin/cos/artan functions
*--------------------------------------------------------------------*/
times='23:00:17 23:40:20 00:12:45 00:17:19'
sum=0
day=86400
pi=3.14159265358979323846264
x=0
y=0
Do i=1 To words(times)                 /* loop over times            */
  time.i=word(times,i)                 /* pick a time                */
  alpha.i=t2a(time.i)                  /* convert to angle (radians) */
  /* Say time.i format(alpha.i,6,9) */
  x=x+sin(alpha.i)                     /* accumulate sines           */
  y=y+cos(alpha.i)                     /* accumulate cosines         */
  End
ww=arctan(x/y)                         /* compute average angle      */
ss=ww*86400/(2*pi)                     /* convert to seconds         */
If ss<0 Then ss=ss+day                 /* avoid negative value       */
m=ss%60                                /* split into hh mm ss        */
s=ss-m*60
h=m%60
m=m-h*60
Say f2(h)':'f2(m)':'f2(s)              /* show the mean time         */
Exit

t2a: Procedure Expose day pi           /* convert time to angle      */
  Parse Arg hh ':' mm ':' ss
  sec=(hh*60+mm)*60+ss
  If sec>(day/2) Then
    sec=sec-day
  a=2*pi*sec/day
  Return a

f2: return right(format(arg(1),2,0),2,0)


sin: Procedure Expose pi
  Parse Arg x
  prec=digits()
  Numeric Digits (2*prec)
  Do While x>pi
    x=x-pi
    End
  Do While x<-pi
    x=x+pi
    End
  o=x
  u=1
  r=x
  Do i=3 By 2
    ra=r
    o=-o*x*x
    u=u*i*(i-1)
    r=r+(o/u)
    If r=ra Then Leave
    End
  Numeric Digits prec
  Return r+0

cos: Procedure Expose pi
  Parse Arg x
  prec=digits()
  Numeric Digits (2*prec)
  Numeric Fuzz 3
  o=1
  u=1
  r=1
  Do i=1 By 2
    ra=r
    o=-o*x*x
    u=u*i*(i+1)
    r=r+(o/u)
    If r=ra Then Leave
    End
  Numeric Digits prec
  Return r+0

arctan: Procedure
  Parse Arg x
  prec=digits()
  Numeric Digits (2*prec)
  Numeric Fuzz 3
  o=x
  u=1
  r=x
  k=0
  Do i=3 By 2
    ra=r
    o=-o*x*x
    r=r+(o/i)
    If r=ra Then
      Leave
    k=k+1
    If k//1000=0 Then
      Say i left(r,40) format(abs(o/i),15,5)
    End
  Numeric Digits (prec)
  Return r+0
```

{{out}}

```txt
23:47:43
```



## Ruby

Using the methods at [[http://rosettacode.org/wiki/Averages/Mean_angle#Ruby|Averages/Mean angle]]


```ruby
def time2deg(t)
  raise "invalid time" unless m = t.match(/^(\d\d):(\d\d):(\d\d)$/)
  hh,mm,ss = m[1..3].map {|e| e.to_i}
  raise "invalid time" unless (0..23).include? hh and
                              (0..59).include? mm and
                              (0..59).include? ss
  (hh*3600 + mm*60 + ss) * 360 / 86400.0
end

def deg2time(d)
  sec = (d % 360) * 86400 / 360.0
  "%02d:%02d:%02d" % [sec/3600, (sec%3600)/60, sec%60]
end

def mean_time(times)
  deg2time(mean_angle(times.map {|t| time2deg t}))
end

puts mean_time ["23:00:17", "23:40:20", "00:12:45", "00:17:19"]
```

{{out}}
 23:47:43


## Run BASIC


```runbasic
global pi
pi = acs(-1)
 
Print "Average of:"
for i = 1 to 4
    read t$ 
    print t$
    a  = time2angle(t$)
    ss = ss+sin(a)
    sc = sc+cos(a)
next
a = atan2(ss,sc)
if a < 0 then a = a + 2 * pi
print "is ";angle2time$(a)
end
data "23:00:17", "23:40:20", "00:12:45", "00:17:19"
 
function nn$(n)
    nn$ = right$("0";n, 2)
end function
 
function angle2time$(a)
    a  = int(a / 2 / pi * 24 * 60 * 60)
    ss = a mod 60
    a  = int(a / 60)
    mm=a mod 60
    hh=int(a/60)
    angle2time$=nn$(hh);":";nn$(mm);":";nn$(ss)
end function
 
function time2angle(time$)
    hh=val(word$(time$,1,":"))
    mm=val(word$(time$,2,":"))
    ss=val(word$(time$,3,":"))
    time2angle=2*pi*(60*(60*hh+mm)+ss)/24/60/60
end function
 
function atan2(y, x)
       if y <> 0 then
        atan2 = (2 * (atn((sqr((x * x) + (y * y)) - x)/ y)))
        else
        atan2 = (y=0)*(x<0)*pi
       end if
End Function
```



## Scala

{{libheader|java.time.LocalTime}}
```Scala
import java.time.LocalTime
import scala.compat.Platform

trait MeanAnglesComputation {
  import scala.math.{Pi, atan2, cos, sin}

  def meanAngle(angles: List[Double], convFactor: Double = 180.0 / Pi) = {
    val sums = angles.foldLeft((.0, .0))((r, c) => {
      val rads = c / convFactor
      (r._1 + sin(rads), r._2 + cos(rads))
    })
    val result = atan2(sums._1, sums._2)
    (result + (if (result.signum == -1) 2 * Pi else 0.0)) * convFactor
  }
}

object MeanBatTime extends App with MeanAnglesComputation {
  val dayInSeconds = 60 * 60 * 24

  def times = batTimes.map(t => afterMidnight(t).toDouble)

  def afterMidnight(twentyFourHourTime: String) = {
    val t = LocalTime.parse(twentyFourHourTime)
    (if (t.isBefore(LocalTime.NOON)) dayInSeconds else 0) + LocalTime.parse(twentyFourHourTime).toSecondOfDay
  }

  def batTimes = List("23:00:17", "23:40:20", "00:12:45", "00:17:19")
  assert(LocalTime.MIN.plusSeconds(meanAngle(times, dayInSeconds).round).toString == "23:47:40")
  println(s"Successfully completed without errors. [total ${Platform.currentTime - executionStart} ms]")
}
```



## Scheme

{{libheader|Scheme/SRFIs}}

To be self-contained, this starts with the functions from [[Averages/Mean angle]]


```scheme

(import (scheme base)
        (scheme inexact)
        (scheme read)
        (scheme write)
        (srfi 1))         ; for fold

;; functions copied from "Averages/Mean angle" task
(define (average l)
  (/ (fold + 0 l) (length l)))

(define pi 3.14159265358979323846264338327950288419716939937510582097) 

(define (radians a)
  (* pi 1/180 a))

(define (degrees a)
  (* 180 (/ 1 pi) a))

(define (mean-angle angles)
  (let* ((angles (map radians angles))
         (cosines (map cos angles))
         (sines (map sin angles)))
    (degrees (atan (average sines) (average cosines)))))

;; -- new functions for this task
(define (time->angle time)
  (let* ((time2 ; replaces : with space in string
           (string-map (lambda (c) (if (char=? c #\:) #\space c)) time))
         (string-port (open-input-string time2))
         (hour (read string-port))
         (minutes (read string-port))
         (seconds (read string-port)))
    (/ (* 360 (+ (* hour 3600) (* minutes 60) seconds))
       (* 24 60 60))))

(define (angle->time angle)
  (let* ((nom-angle (if (negative? angle) (+ 360 angle) angle))
         (time (/ (* nom-angle 24 60 60) 360))
         (hour (exact (floor (/ time 3600))))
         (minutes (exact (floor (/ (- time (* 3600 hour)) 60))))
         (seconds (exact (floor (- time (* 3600 hour) (* 60 minutes))))))
    (string-append (number->string hour)
                   ":"
                   (number->string minutes)
                   ":"
                   (number->string seconds))))

(define (mean-time-of-day times)
  (angle->time (mean-angle (map time->angle times))))

(write (mean-time-of-day '("23:00:17" "23:40:20" "00:12:45" "00:17:19")))
(newline)

```


{{out}}

```txt

"23:47:43"

```



## Sidef

{{trans|Ruby}}
Using the '''mean_angle()''' function from: [http://rosettacode.org/wiki/Averages/Mean_angle#Sidef "Averages/Mean angle"]

```ruby
func time2deg(t) {
  (var m = t.match(/^(\d\d):(\d\d):(\d\d)$/)) || die "invalid time"
  var (hh,mm,ss) = m.cap.map{.to_i}...
  ((hh ~~ 24.range) && (mm ~~ 60.range) && (ss ~~ 60.range)) || die "invalid time"
  (hh*3600 + mm*60 + ss) * 360 / 86400
}
 
func deg2time(d) {
  var sec = ((d % 360) * 86400 / 360)
  "%02d:%02d:%02d" % (sec/3600, (sec%3600)/60, sec%60)
}
 
func mean_time(times) {
  deg2time(mean_angle(times.map {|t| time2deg(t)}))
}
 
say mean_time(["23:00:17", "23:40:20", "00:12:45", "00:17:19"])
```

{{out}}

```txt
23:47:43
```


=={{header|SQL}}/{{header|PostgreSQL}}==
{{trans|Python}}

```SQL

--Setup table for testing
CREATE TABLE time_table(times time);
INSERT INTO time_table values ('23:00:17'::time),('23:40:20'::time),('00:12:45'::time),('00:17:19'::time)

--Compute mean time
SELECT to_timestamp((degrees(atan2(AVG(sin),AVG(cos))))* (24*60*60)/360)::time
FROM
	(SELECT
	cos(radians(t*360/(24*60*60))),sin(radians(t*360/(24*60*60)))
	FROM
		(SELECT EXTRACT(epoch from times) t
		FROM time_table) T1
	)T2
```


Output:

```txt

23:47:43.361529

```



## Tcl


```tcl
proc meanTime {times} {
    set secsPerRad [expr {60 * 60 * 12 / atan2(0,-1)}]
    set sumSin [set sumCos 0.0]
    foreach t $times {
	# Convert time to count of seconds from midnight
	scan $t "%02d:%02d:%02d" h m s
	incr s [expr {[incr m [expr {$h * 60}]] * 60}]
	# Feed into averaging
	set sumSin [expr {$sumSin + sin($s / $secsPerRad)}]
	set sumCos [expr {$sumCos + cos($s / $secsPerRad)}]
    }
    # Don't need to divide by counts; atan2() cancels that out
    set a [expr {round(atan2($sumSin, $sumCos) * $secsPerRad)}]
    # Convert back to human-readable
    format "%02d:%02d:%02d" [expr {$a / 60 / 60 % 24}] [expr {$a / 60 % 60}] [expr {$a % 60}]
}

puts [meanTime {23:00:17 23:40:20 00:12:45 00:17:19}]
```

{{out}}
 23:47:43


## XPL0


```XPL0
include c:\cxpl\codes;

proc NumOut(N);                 \Display 2-digit N with leading zero
int  N;
[if N<10 then ChOut(0, ^0);
IntOut(0, N);
];

proc TimeOut(Sec);              \Display real seconds as HH:MM:SS
real Sec;
[NumOut(fix(Sec)/3600);  ChOut(0, ^:);
 NumOut(rem(0)/60);      ChOut(0, ^:);
 NumOut(rem(0));
];

func real HMS2Sec(H, M, S);     \Convert hours, minutes, seconds to real seconds
int  H, M, S;
return float(((H*60 + M)*60) + S);

func real MeanTime(A);          \Return the mean of the given list of times
int  A;
real X, Y, Sec;
int  I;
def Pi  = 3.14159265358979323846;
def S2R = Pi/(12.*60.*60.);     \coefficient to convert seconds to radians
[X:= 0.0;  Y:= 0.0;
for I:= 1 to A(0) do
    [Sec:= HMS2Sec(A(I,0), A(I,1), A(I,2));
    X:= X + Cos(Sec*S2R);
    Y:= Y + Sin(Sec*S2R);
    ];
Sec:= ATan2(Y,X)/S2R;
if Sec < 0.0 then Sec:= Sec + 24.*60.*60.;
return Sec;
];

TimeOut(MeanTime([4, [23,00,17], [23,40,20], [00,12,45], [00,17,19]]))
```

{{out}}

```txt

23:47:43

```



## VBA

Uses Excel and [[Averages/Mean_angle#VBA|mean angle]].

```vb
Public Sub mean_time()
    Dim angles() As Double
    s = [{"23:00:17","23:40:20","00:12:45","00:17:19"}]
    For i = 1 To UBound(s)
        s(i) = 360 * TimeValue(s(i))
    Next i
    Debug.Print Format(mean_angle(s) / 360 + 1, "hh:mm:ss")
End Sub
```
{{out}}

```txt
23:47:43
```



## Yabasic

{{trans|Phix}}

```Yabasic
sub atan2(y, x)
    return 2 * atan((sqrt(x **2 + y ** 2) - x) / y)
end sub
 
sub MeanAngle(angles())
    local x, y, ai_rad, l, i
	
    l = arraysize(angles(), 1)
 
    for i = 1 to l
        ai_rad = angles(i) * PI / 180
        x = x + cos(ai_rad)
        y = y + sin(ai_rad)
    next i
    if abs(x) < 1e-16 return false
    return atan2(y, x) * 180 / PI
end sub
 
sub toSecAngle(hours, minutes, seconds)
    return ((hours * 60 + minutes) * 60 + seconds) / (24 * 60 * 60) * 360
end sub

dim Times(4)

Times(1) = toSecAngle(23,00,17)
Times(2) = toSecAngle(23,40,20)
Times(3) = toSecAngle(00,12,45)
Times(4) = toSecAngle(00,17,19)

sub toHMS$(t)
    local s$
	
    if t then
        if t < 0 t = t + 360
        t = 24 * 60 * 60 * t / 360
        s$ = str$(int(t / 3600), "%02g") + ":" + str$(int(mod(t, 3600) / 60), "%02g") + ":" + str$(int(mod(t, 60)), "%02g")
    else
	s$ = "not meaningful"
    end if
    return s$
end sub

print "Mean Time is ", toHMS$(MeanAngle(Times()))

// Output: Mean Time is 23:47:43

```



## zkl

This solution is a bit greasy, combining the solution 
to task "Averages/Mean angle" and some on-the-fly 
time-to-angle and back conversions.

```zkl
var D=Time.Date;
fcn meanT(t1,t2,etc){
   ts:=vm.arglist.apply(fcn(hms){
     (D.toFloat(hms.split(":").xplode())*15).toRad()
   });
   n:=ts.len(); 
   mt:=(ts.apply("sin").sum(0.0)/n)
       .atan2(ts.apply("cos").sum(0.0)/n)
       .toDeg() /15;
   if(mt<0) mt+=24;  //-0.204622-->23.7954
   D.toHour(mt).concat(":")
}
```

Time.Date.toFloat/toHour convert 24hr HMS to fractional time and back. Multiplying fractional time by 360/24=15 yields angle.
{{out}}

```txt

meanT("23:00:17", "23:40:20", "00:12:45", "00:17:19")
23:47:43

```

