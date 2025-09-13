+++
title = "Nautical bell"
description = ""
date = 2018-12-05T17:49:11Z
aliases = []
[extra]
id = 12953
[taxonomies]
categories = ["task"]
tags = []
+++

[[File:ship'sBell.jpg|650px||right]]
## Task

Write a small program that emulates a [[wp:Ship's bell#Timing_of_duty_periods|nautical bell]] producing a ringing bell pattern at certain times throughout the day.

The bell timing should be in accordance with [[wp:GMT|Greenwich Mean Time]], unless locale dictates otherwise.

It is permissible for the program to [[Run as a daemon or service|daemonize]], or to slave off a scheduler, and it is permissible to use alternative notification methods (such as producing a written notice "Two Bells Gone"), if these are more usual for the system type. 


## Related tasks

* [[Sleep]]





## AppleScript

This version uses local time and speaks the bell rings using OS X's built-in speech synthesizer.


```applescript
repeat
	set {hours:h, minutes:m} to (current date)
	if {0, 30} contains m then
		set bells to (h mod 4) * 2 + (m div 30)
                if bells is 0 then set bells to 4
                set pairs to bells div 2
		repeat pairs times
			say "ding dong" using "Bells"
		end repeat
		if (bells mod 2) is 1 then
			say "dong" using "Bells"
		end if
	end if
	delay 60
end repeat
```



## AutoHotkey


```AutoHotkey
NauticalBell(hh, mm){
	Hr := 0, min := 30, Bells := [], pattern := []
	Loop 8											; genrate 8 patterns
	{
		num := A_Index	, code := ""
		while (num/2 >=1)
			code .= "**  ", num := num-2
		code .= mod(A_Index, 2) ? "*" : ""
		pattern[A_Index] := code
	}
	loop, 48										; 24 hours * 2 for every half an hour
	{
		numBells := !mod(A_Index, 8) ? 8 : mod(A_Index, 8)	, min := 30
		if !Mod(A_Index, 2)
			hr++ , min := 00
		Bells[SubStr("0" hr, -1) ":" min] := numBells
	}
	Bells[00 ":" 00] := Bells[24 ":" 00]	, numBells := Bells[hh ":" mm]
	return {"bells": numBells, "pattern": Pattern[numBells]}
}
```

Example:
```AutoHotkey
res := ""
loop, 24
{
	hr := SubStr("0" A_Index -1, -1)
	Loop 60
	{
		min := SubStr("0" A_Index -1, -1)
		if (min = 0 || min = 30)
			res .= hr ":" min "`t" NauticalBell(hr, min).bells "`t" NauticalBell(hr, min).pattern "`n"
	}
}
MsgBox, 262144, , % "Time`tBells`tPattern`n" res
return
```

Outputs:
```txt
Time	Bells	Pattern
00:00	8	**  **  **  **  
00:30	1	*
01:00	2	**  
01:30	3	**  *
02:00	4	**  **  
02:30	5	**  **  *
03:00	6	**  **  **  
03:30	7	**  **  **  *
04:00	8	**  **  **  **  
04:30	1	*
05:00	2	**  
05:30	3	**  *
06:00	4	**  **  
06:30	5	**  **  *
07:00	6	**  **  **  
07:30	7	**  **  **  *
08:00	8	**  **  **  **  
08:30	1	*
09:00	2	**  
09:30	3	**  *
10:00	4	**  **  
10:30	5	**  **  *
11:00	6	**  **  **  
11:30	7	**  **  **  *
12:00	8	**  **  **  **  
12:30	1	*
13:00	2	**  
13:30	3	**  *
14:00	4	**  **  
14:30	5	**  **  *
15:00	6	**  **  **  
15:30	7	**  **  **  *
16:00	8	**  **  **  **  
16:30	1	*
17:00	2	**  
17:30	3	**  *
18:00	4	**  **  
18:30	5	**  **  *
19:00	6	**  **  **  
19:30	7	**  **  **  *
20:00	8	**  **  **  **  
20:30	1	*
21:00	2	**  
21:30	3	**  *
22:00	4	**  **  
22:30	5	**  **  *
23:00	6	**  **  **  
23:30	7	**  **  **  *
```


Alternatively, you could remove the forever loop and set it up to run every half hour via launchd or cron.

## AWK


```AWK

# syntax: GAWK -f NAUTICAL_BELL.AWK
BEGIN {
#   sleep_cmd = "sleep 55s" # Unix
    sleep_cmd = "TIMEOUT /T 55 >NUL" # MS-Windows
    split("Middle,Morning,Forenoon,Afternoon,Dog,First",watch_arr,",")
    split("One,Two,Three,Four,Five,Six,Seven,Eight",bells_arr,",")
    simulate1day()
    while (1) {
      t = systime()
      h = strftime("%H",t) + 0
      m = strftime("%M",t) + 0
      if (m == 0 || m == 30) {
        nb(h,m)
        while (systime() < t + 5) {}
      }
      system(sleep_cmd)
    }
    exit(0)
}
function nb(h,m,  bells,hhmm,plural,sounds,watch) {
#   hhmm = sprintf("%02d:%02d",h,m)
#   if (hhmm == "00:00") { watch = 6 }
#   else if (hhmm <= "04:00") { watch = 1 }
#   else if (hhmm <= "08:00") { watch = 2 }
#   else if (hhmm <= "12:00") { watch = 3 }
#   else if (hhmm <= "16:00") { watch = 4 }
#   else if (hhmm <= "20:00") { watch = 5 }
#   else { watch = 6}
# determining watch: verbose & readable (above) vs. terse & cryptic (below)
    watch = 60 * h + m
    watch = (watch < 1 ) ? 6 : int((watch - 1) / 240 + 1)
    bells = (h % 4) * 2 + int(m / 30)
    if (bells == 0) { bells = 8 }
    plural = (bells == 1) ? " " : "s"
    sounds = strdup("\x07",bells)
    printf("%02d:%02d %9s watch %5s bell%s  %s\n",h,m,watch_arr[watch],bells_arr[bells],plural,sounds)
}
function simulate1day(   h,m) {
    for (h=0; h<=23; h++) {
      for (m=0; m<=59; m+=30) {
        nb(h,m)
      }
    }
}
function strdup(str,n,  i,new_str) {
    for (i=1; i<=n; i++) {
      new_str = new_str str
    }
    gsub(str str,"& ",new_str)
    return(new_str)
}

```

<p>Output:</p>

```txt

00:00     First watch Eight bells  &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;&#x2407;
00:30    Middle watch   One bell   &#x2407;
01:00    Middle watch   Two bells  &#x2407;&#x2407;
01:30    Middle watch Three bells  &#x2407;&#x2407; &#x2407;
02:00    Middle watch  Four bells  &#x2407;&#x2407; &#x2407;&#x2407;
02:30    Middle watch  Five bells  &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;
03:00    Middle watch   Six bells  &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;&#x2407;
03:30    Middle watch Seven bells  &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;
04:00    Middle watch Eight bells  &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;&#x2407;
04:30   Morning watch   One bell   &#x2407;
05:00   Morning watch   Two bells  &#x2407;&#x2407;
05:30   Morning watch Three bells  &#x2407;&#x2407; &#x2407;
06:00   Morning watch  Four bells  &#x2407;&#x2407; &#x2407;&#x2407;
06:30   Morning watch  Five bells  &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;
07:00   Morning watch   Six bells  &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;&#x2407;
07:30   Morning watch Seven bells  &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;
08:00   Morning watch Eight bells  &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;&#x2407;
08:30  Forenoon watch   One bell   &#x2407;
09:00  Forenoon watch   Two bells  &#x2407;&#x2407;
09:30  Forenoon watch Three bells  &#x2407;&#x2407; &#x2407;
10:00  Forenoon watch  Four bells  &#x2407;&#x2407; &#x2407;&#x2407;
10:30  Forenoon watch  Five bells  &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;
11:00  Forenoon watch   Six bells  &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;&#x2407;
11:30  Forenoon watch Seven bells  &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;
12:00  Forenoon watch Eight bells  &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;&#x2407;
12:30 Afternoon watch   One bell   &#x2407;
13:00 Afternoon watch   Two bells  &#x2407;&#x2407;
13:30 Afternoon watch Three bells  &#x2407;&#x2407; &#x2407;
14:00 Afternoon watch  Four bells  &#x2407;&#x2407; &#x2407;&#x2407;
14:30 Afternoon watch  Five bells  &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;
15:00 Afternoon watch   Six bells  &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;&#x2407;
15:30 Afternoon watch Seven bells  &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;
16:00 Afternoon watch Eight bells  &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;&#x2407;
16:30       Dog watch   One bell   &#x2407;
17:00       Dog watch   Two bells  &#x2407;&#x2407;
17:30       Dog watch Three bells  &#x2407;&#x2407; &#x2407;
18:00       Dog watch  Four bells  &#x2407;&#x2407; &#x2407;&#x2407;
18:30       Dog watch  Five bells  &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;
19:00       Dog watch   Six bells  &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;&#x2407;
19:30       Dog watch Seven bells  &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;
20:00       Dog watch Eight bells  &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;&#x2407;
20:30     First watch   One bell   &#x2407;
21:00     First watch   Two bells  &#x2407;&#x2407;
21:30     First watch Three bells  &#x2407;&#x2407; &#x2407;
22:00     First watch  Four bells  &#x2407;&#x2407; &#x2407;&#x2407;
22:30     First watch  Five bells  &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;
23:00     First watch   Six bells  &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;&#x2407;
23:30     First watch Seven bells  &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;&#x2407; &#x2407;

```



## C


Implementation corrected, sounds the system bell as per nautical standards, sounds bell as per local system time.


```C

#include<unistd.h>
#include<stdio.h>
#include<time.h>

#define SHORTLAG 1000
#define LONGLAG  2000

int main(){
	int i,times,hour,min,sec,min1,min2;
	
	time_t t;
	struct tm* currentTime;
	
	while(1){
		time(&t);
		currentTime = localtime(&t);
		
		hour = currentTime->tm_hour;
		min = currentTime->tm_min;
		sec = currentTime->tm_sec;
		
		hour = 12;
		min = 0;
		sec = 0;
		
		if((min==0 || min==30) && sec==0)
			times = ((hour*60 + min)%240)%8;
		if(times==0){
			times = 8;
		}	

		if(min==0){
			min1 = 0;
			min2 = 0;
		}
		
		else{
			min1 = 3;
			min2 = 0;
		}
		
		if((min==0 || min==30) && sec==0){
			printf("\nIt is now %d:%d%d %s. Sounding the bell %d times.",hour,min1,min2,(hour>11)?"PM":"AM",times);
		
			for(i=1;i<=times;i++){
				printf("\a");
				
				(i%2==0)?sleep(LONGLAG):sleep(SHORTLAG);
			}
		}
	}
	return 0;
}


```



## C++

This version uses local time.

```cpp

#include <iostream>
#include <string>
#include <windows.h>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
class bells
{
public:
    void start()
    {
	watch[0] = "Middle"; watch[1] = "Morning"; watch[2] = "Forenoon"; watch[3] = "Afternoon"; watch[4] = "Dog"; watch[5] =  "First"; 
	count[0] = "One"; count[1] = "Two"; count[2] = "Three"; count[3] = "Four"; count[4] = "Five"; count[5] = "Six"; count[6] = "Seven"; count[7] = "Eight";
	_inst = this; CreateThread( NULL, 0, bell, NULL, 0, NULL );
    }
private:
    static DWORD WINAPI bell( LPVOID p )
    {
	DWORD wait = _inst->waitTime();
	while( true )
	{
	    Sleep( wait );
	    _inst->playBell();
	    wait = _inst->waitTime();
	}
	return 0;
    }

    DWORD waitTime() 
    { 
	GetLocalTime( &st );
	int m = st.wMinute >= 30 ? st.wMinute - 30 : st.wMinute;
	return( 1800000 - ( ( m * 60 + st.wSecond ) * 1000 + st.wMilliseconds ) );
    }

    void playBell()
    {
	GetLocalTime( &st );
	int b = ( 2 * st.wHour + st.wMinute / 30 ) % 8; b = b == 0 ? 8 : b;
	int w = ( 60 * st.wHour + st.wMinute ); 
	if( w < 1 ) w = 5; else w = ( w - 1 ) / 240;
	char hr[32]; wsprintf( hr, "%.2d:%.2d", st.wHour, st.wMinute );

	cout << hr << " - " << watch[w] << " watch - " << count[b - 1] << " Bell";
	if( b > 1 ) cout << "s"; else cout << " "; cout << " Gone." << endl;

	for( int x = 0, c = 1; x < b; x++, c++ )
	{
	    cout << "\7"; Sleep( 500 );
	    if( !( c % 2 ) ) Sleep( 300 );
	}
    }

    SYSTEMTIME st;
    string watch[7], count[8];
    static bells* _inst;
};
//--------------------------------------------------------------------------------------------------
bells* bells::_inst = 0;
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    bells b; b.start();
    while( 1 ); // <- runs forever!
    return 0;
}
//--------------------------------------------------------------------------------------------------

```

Output:

```txt

00:00 - First watch - Eight Bells Gone.
00:30 - Middle watch - One Bell  Gone.
01:00 - Middle watch - Two Bells Gone.
01:30 - Middle watch - Three Bells Gone.
02:00 - Middle watch - Four Bells Gone.
02:30 - Middle watch - Five Bells Gone.
03:00 - Middle watch - Six Bells Gone.
03:30 - Middle watch - Seven Bells Gone.
04:00 - Middle watch - Eight Bells Gone.
04:30 - Morning watch - One Bell  Gone.
05:00 - Morning watch - Two Bells Gone.
05:30 - Morning watch - Three Bells Gone.
06:00 - Morning watch - Four Bells Gone.
06:30 - Morning watch - Five Bells Gone.
07:00 - Morning watch - Six Bells Gone.
07:30 - Morning watch - Seven Bells Gone.
08:00 - Morning watch - Eight Bells Gone.
08:30 - Forenoon watch - One Bell  Gone.
09:00 - Forenoon watch - Two Bells Gone.
09:30 - Forenoon watch - Three Bells Gone.
10:00 - Forenoon watch - Four Bells Gone.
10:30 - Forenoon watch - Five Bells Gone.
11:00 - Forenoon watch - Six Bells Gone.
11:30 - Forenoon watch - Seven Bells Gone.
12:00 - Forenoon watch - Eight Bells Gone.
12:30 - Afternoon watch - One Bell  Gone.
13:00 - Afternoon watch - Two Bells Gone.
13:30 - Afternoon watch - Three Bells Gone.
14:00 - Afternoon watch - Four Bells Gone.
14:30 - Afternoon watch - Five Bells Gone.
15:00 - Afternoon watch - Six Bells Gone.
15:30 - Afternoon watch - Seven Bells Gone.
16:00 - Afternoon watch - Eight Bells Gone.
16:30 - Dog watch - One Bell  Gone.
17:00 - Dog watch - Two Bells Gone.
17:30 - Dog watch - Three Bells Gone.
18:00 - Dog watch - Four Bells Gone.
18:30 - Dog watch - Five Bells Gone.
19:00 - Dog watch - Six Bells Gone.
19:30 - Dog watch - Seven Bells Gone.
20:00 - Dog watch - Eight Bells Gone.
20:30 - First watch - One Bell  Gone.
21:00 - First watch - Two Bells Gone.
21:30 - First watch - Three Bells Gone.
22:00 - First watch - Four Bells Gone.
22:30 - First watch - Five Bells Gone.
23:00 - First watch - Six Bells Gone.
23:30 - First watch - Seven Bells Gone.

```



## D

This code uses local time instead of Greenwich Mean Time.

```d
import std.stdio, core.thread, std.datetime;

class NauticalBell : Thread {
    private shared bool stopped;

    this() {
        super(&run);
    }

    void run() {
        uint numBells;
        auto time = cast(TimeOfDay)Clock.currTime();
        auto next = TimeOfDay();

        void setNextBellTime() {
            next += minutes(30);
            numBells = 1 + (numBells % 8);
        }

        while (next < time)
            setNextBellTime();

        while (!this.stopped) {
            time = cast(TimeOfDay)Clock.currTime();
            if (next.minute == time.minute &&
                    next.hour == time.hour) {
                immutable bells = numBells == 1 ? "bell" : "bells";
                writefln("%s : %d %s", time, numBells, bells);
                setNextBellTime();
            }
            sleep(dur!"msecs"(100));
            yield();
         }
     }

     void stop() {
        this.stopped = true;
     }
}

void main() {
    auto bells = new NauticalBell();
    bells.isDaemon(true);
    bells.start();
    try {
        bells.join();
    } catch (ThreadException e) {
        writeln(e.msg);
    }
}
```

This output is from an actual test run.

```txt

09:30:00 : 3 bells
10:00:00 : 4 bells
10:30:00 : 5 bells
11:00:00 : 6 bells
11:30:00 : 7 bells
12:00:00 : 8 bells
12:30:00 : 1 bell
13:00:00 : 2 bells
13:30:00 : 3 bells
14:00:00 : 4 bells
14:30:00 : 5 bells
15:00:00 : 6 bells
15:30:00 : 7 bells
16:00:00 : 8 bells
16:30:00 : 1 bell
17:00:00 : 2 bells
17:30:00 : 3 bells
18:00:00 : 4 bells
18:30:00 : 5 bells
19:00:00 : 6 bells
19:30:00 : 7 bells
20:00:00 : 8 bells
20:30:00 : 1 bell
21:00:00 : 2 bells
21:30:00 : 3 bells
22:00:00 : 4 bells
22:30:00 : 5 bells
23:00:00 : 6 bells
23:30:00 : 7 bells
00:00:00 : 8 bells
00:30:00 : 1 bell
01:00:00 : 2 bells
01:30:00 : 3 bells
02:00:00 : 4 bells
02:30:00 : 5 bells
03:00:00 : 6 bells
03:30:00 : 7 bells
04:00:00 : 8 bells
04:30:00 : 1 bell
05:00:00 : 2 bells
05:30:00 : 3 bells
06:00:00 : 4 bells
06:30:00 : 5 bells
07:00:00 : 6 bells
07:30:00 : 7 bells
08:00:00 : 8 bells
08:30:00 : 1 bell
09:00:00 : 2 bells
```



## Go

Provided your terminal bell is enabled, this should beep an appropriate number of times before displaying its output. It uses local time.

```go
package main

import (
    "fmt"
    "strings"
    "time"
)

func main() {
    watches := []string{
        "First", "Middle", "Morning", "Forenoon",
        "Afternoon", "Dog", "First",
    }
    for {
        t := time.Now()
        h := t.Hour()
        m := t.Minute()
        s := t.Second()
        if (m == 0 || m == 30) && s == 0 {
            bell := 0
            if m == 30 {
                bell = 1
            }
            bells := (h*2 + bell) % 8
            watch := h/4 + 1
            if bells == 0 {
                bells = 8
                watch--
            }
            sound := strings.Repeat("\a", bells)
            pl := "s"
            if bells == 1 {
                pl = " "
            }
            w := watches[watch] + " watch"
            if watch == 5 {
                if bells < 5 {
                    w = "First " + w
                } else {
                    w = "Last " + w
                }
            }
            fmt.Printf("%s%02d:%02d = %d bell%s : %s\n", sound, h, m, bells, pl, w)
        }
        time.Sleep(1 * time.Second)
    }
}
```


Abbreviated output:

```txt

...
15:30 = 7 bells : Afternoon watch
16:00 = 8 bells : Afternoon watch
16:30 = 1 bell  : First Dog watch
17:00 = 2 bells : First Dog watch
...

```



## Haskell

This solution first creates a general way of scheduling tasks on a time interval, and then schedules a "ringing" task. If used in a terminal it will also produce noise. Local time is used.

```haskell

import Control.Concurrent
import Control.Monad
import Data.Time
import Text.Printf

type Microsecond = Int
type Scheduler = TimeOfDay -> Microsecond

-- Scheduling
--------------

getTime :: TimeZone -> IO TimeOfDay
getTime tz = do
    t  <- getCurrentTime
    return $ localTimeOfDay $ utcToLocalTime tz t

getGMTTime   = getTime utc
getLocalTime = getCurrentTimeZone >>= getTime

-- Returns the difference between 'y' and the closest higher multiple of 'x'
nextInterval x y
    | x > y = x - y
    | mod y x > 0 = x - mod y x
    | otherwise = 0

-- Given a interval in seconds, this function returns time delta in microseconds.
onInterval :: Int -> Scheduler
onInterval interval time = toNext dMS
  where
    toNext = nextInterval (1000000 * interval)
    tDelta = timeOfDayToTime time
    dMS    = truncate $ 1000000 * tDelta

doWithScheduler :: Scheduler -> (Int -> IO ()) -> IO ThreadId
doWithScheduler sched task = forkIO $ forM_ [0..] exec
  where
    exec n = do
        t <- getLocalTime
        threadDelay $ sched t
        task n

-- Output
---------

watchNames = words "Middle Morning Forenoon Afternoon Dog First"
countWords = words "One Two Three Four Five Six Seven Eight"

-- Executes IO action and then waits for n microseconds
postDelay n fn = fn >> threadDelay n

termBell        = putStr "\a"
termBells n     = replicateM_ n $ postDelay 100000 termBell
termBellSeq seq = forM_ seq $ postDelay 500000 . termBells

toNoteGlyph 1 = "♪"
toNoteGlyph 2 = "♫"
toNoteGlyph _ = ""

ringBells :: Int -> IO ()
ringBells n = do
    t <- getLocalTime
    let numBells    = 1 + (mod n 8)
        watch       = watchNames!!(mod (div n 8) 8)
        count       = countWords!!(numBells - 1)
        (twos,ones) = quotRem numBells 2
        pattern     = (replicate twos 2) ++ (replicate ones 1)
        notes       = unwords $ map toNoteGlyph pattern
        plural       = if numBells > 1 then "s" else ""
        strFMT      = show t ++ ": %s watch, %5s bell%s:  " ++ notes ++ "\n"
    printf strFMT watch count plural
    termBellSeq pattern

-- Usage
---------

bellRinger :: IO ThreadId
bellRinger = doWithScheduler (onInterval (30*60)) ringBells


```


```txt

12:30 Afternoon watch,    One bell: 	       ♪
13:00 Afternoon watch,    Two bells: 	♫ 
13:30 Afternoon watch,  Three bells: 	♫ ♪
14:00 Afternoon watch,   Four bells: 	♫ ♫ 
14:30 Afternoon watch,   Five bells:  	♫ ♫ ♪

```



## J

'''Solution''':
```j
require 'strings printf'

WATCH        =:  <;._1 ' Middle Morning Forenoon Afternoon Dog First'
ORDINAL      =:  <;._1 ' One Two Three Four Five Six Seven Eight'
BELL         =:  7{a.  NB. Terminal bell code (\a or ^G)

time         =:  6!:0
sleep        =:  6!:3
print        =:  ucp 1!:2 4:

shipsWatch   =:  verb define
	PREV_MARK =.  _1 _1
	while. do. NB. Loop forever
		now  =.  3 4 { time ''  NB. Current hour & minute
		
		NB. If we just flipped over to a new half-hour mark
		if. (0 30 e.~ {: now) > now -: PREV_MARK do.
			PREV_MARK  =. now
			'allsWell notes'=.callWatch now			
			
			print allsWell
			(ringBell"0~ -@# {. 2|#) notes
			print CRLF
		end.
		
		sleep 15.0
	end.	
)

callWatch    =:  verb define
	'watch bells' =. clock2ship y
	
	NB. Plural for 0~:bells ordinals are origin-1, not origin-0
	NB. (and similarly 1+bells for notes).
	fields=.(0{y);(1{y);(watch{::WATCH);(bells{::ORDINAL);('s'#~0~:bells)
	notes =. ; (0 2#:1+bells) #&.> u:16b266b 16b266a NB. ♫♪
	
	notes ;~ '%02d:%02d %s watch, %s Bell%s Gone: \t' sprintf fields
)
	
clock2ship   =: verb define"1
	NB. Convert from "24 hours of 60 minutes" to 
	NB. "6 watches of 8 bells", and move midnight 
	NB. from index-origin 0 (0 hrs, 0 minutes) 
	NB. index-origin 1 (0 watches, 1 bell).
	6 8 #: 48 | _1 + 24 2 #. (, (30-1)&I.)/ y	
)

ringBell     =:  dyad define
	print BELL,y

	NB. x indicates two rings (0) or just one (1)
	if. 0=x do.
		sleep 0.75
		print BELL
		sleep 0.25
	else.
		sleep 1.0
	end.
	y
)
```


'''Examples''': Invoke <tt>shipsWatch 0</tt>; the output is identical to Perl6's.

'''Notes''': I tested the <tt>clock2ship</tt>, <tt>callWatch</tt>, and <tt>ringBell</tt> functions, but didn't actually have the patience to test <tt>shipsWatch</tt> over a 24-hour period. Use at your own risk (but don't use it to keep watch on your galleon, please).


## Java

This code uses UTC time.
```java
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.TimeZone;

public class NauticalBell extends Thread {

    public static void main(String[] args) {
        NauticalBell bells = new NauticalBell();
        bells.setDaemon(true);
        bells.start();
        try {
            bells.join();
        } catch (InterruptedException e) {
            System.out.println(e);
        }
    }

    @Override
    public void run() {
        DateFormat sdf = new SimpleDateFormat("HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));

        int numBells = 0;
        long time = System.currentTimeMillis();
        long next = time - (time % (24 * 60 * 60 * 1000)); // midnight

        while (next < time) {
            next += 30 * 60 * 1000; // 30 minutes
            numBells = 1 + (numBells % 8);
        }

        while (true) {
            long wait = 100L;
            time = System.currentTimeMillis();
            if (time - next >= 0) {
                String bells = numBells == 1 ? "bell" : "bells";
                String timeString = sdf.format(time);
                System.out.printf("%s : %d %s\n", timeString, numBells, bells);
                next += 30 * 60 * 1000;
                wait = next - time;
                numBells = 1 + (numBells % 8);
            }
            try {
                Thread.sleep(wait);
            } catch (InterruptedException e) {
                return;
            }
        }
    }
}
```


Sample output:


```txt
...
13:00:00 : 2 bells
13:30:00 : 3 bells
14:00:00 : 4 bells
...
```



## Julia


```julia
using Dates

"""
    nauticalbells(DateTime)
    
    Return a string according to the "simpler system" of nautical bells
    listed in the table in Wikipedia at 
    en.wikipedia.org/wiki/Ship%27s_bell#Simpler_system.
    Note the traditional time zone was determined by local sun position
    and so should be local time without daylight savings time.
"""
function nauticalbells(dt::DateTime)
    hr = hour(dt)
    mn = minute(dt)
    if hr in [00, 12, 4, 8, 16, 20] 
        return mn == 00 ? "2 2 2 2" : "1"
    elseif hr in [1, 5, 9, 13, 17, 21]
        return  mn == 00 ? "2" : "2 1"
    elseif hr in [2, 6, 10, 14, 18, 22]
        return mn == 00 ? "2 2" : "2 2 1"
    elseif hr in [3, 7, 11, 15, 19, 23]
        return mn == 00 ? "2 2 2" : "2 2 2 1"
    else
        return "Gong pattern error: time $dt, hour $hr, minutes $mn"
    end
end

function nauticalbelltask()
    untilnextbell = ceil(now(), Dates.Minute(30)) - now()
    delay = untilnextbell.value / 1000
    println("Nautical bell task starting -- next bell in $delay seconds.")
    # The timer wakes its task every half hour. May drift very slightly so restart yearly.
    timer = Timer(delay; interval=1800)
    while true
        wait(timer)
        gong = nauticalbells(now())
        println("Nautical bell gong strikes ", gong)
    end
end

nauticalbelltask()

```
 {{output}} 
```txt

Nautical bell task starting -- next bell in 1201.726 seconds.
Nautical bell gong strikes 2 2 2
Nautical bell gong strikes 2 2 2 1
Nautical bell gong strikes 2 2 2 2
Nautical bell gong strikes 1
Nautical bell gong strikes 2
Nautical bell gong strikes 2 1
Nautical bell gong strikes 2 2
Nautical bell gong strikes 2 2 1
Nautical bell gong strikes 2 2 2
Nautical bell gong strikes 2 2 2 1
Nautical bell gong strikes 2 2 2 2
Nautical bell gong strikes 1
Nautical bell gong strikes 2
Nautical bell gong strikes 2 1
Nautical bell gong strikes 2 2
Nautical bell gong strikes 2 2 1
Nautical bell gong strikes 2 2 2
Nautical bell gong strikes 2 2 2 1
,,,

```



## Kotlin

```scala
// version 1.1.3

import java.text.DateFormat
import java.text.SimpleDateFormat
import java.util.TimeZone

class NauticalBell: Thread() {

    override fun run() {
        val sdf = SimpleDateFormat("HH:mm:ss")
        sdf.timeZone = TimeZone.getTimeZone("UTC")
        var numBells = 0
        var time = System.currentTimeMillis()
        var next = time - (time % (24 * 60 * 60 * 1000)) // midnight

        while (next < time) {
            next += 30 * 60 * 1000 // 30 minutes
            numBells = 1 + (numBells % 8)
        }

        while (true) {
            var wait = 100L
            time = System.currentTimeMillis()
            if ((time - next) >= 0) {
                val bells = if (numBells == 1) "bell" else "bells"
                val timeString = sdf.format(time)
                println("%s : %d %s".format(timeString, numBells, bells))
                next += 30 * 60 * 1000
                wait = next - time
                numBells = 1 + (numBells % 8)
            }
            try {
                Thread.sleep(wait)
            }
            catch (ie: InterruptedException) {
                return
            }
        }
    }
}

fun main(args: Array<String>) {
    val bells = NauticalBell()
    with (bells) {
        setDaemon(true)
        start()
        try {
            join()
        }
        catch (ie: InterruptedException) {
            println(ie.message)
        }
    }
}
```


Sample output:

```txt

....
10:30:00 : 5 bells
11:00:00 : 6 bells
11:30:00 : 7 bells
....

```



## Mathematica

	
Works on version 11.2 ARM, a bug prevents this from working on version 11.3 Win64.


```Mathematica
LocalSubmit[ScheduledTask[
EmitSound[Sound[Table[{
SoundNote["C",750/1000,"TubularBells"],SoundNote[None,500/1000,"TubularBells"]
},Mod[Round[Total[DateList[][[{4,5}]]{2,1/30}]],8,1]]]]
,DateObject[{_,_,_,_,_,30|0}]]]
```



## OoRexx

```oorexx
/*REXX pgm beep's "bells" (using PC speaker) when running (perpetually).*/
  Parse Arg msg
  If msg='?' Then Do
    Say 'Ring a nautical bell'
    Exit
    End
  Signal on Halt                   /* allow a clean way to stop prog.*/
  Do Forever
    Parse Value time() With hh ':' mn ':' ss
    ct=time('C')
    hhmmc=left(right(ct,7,0),5)        /* HH:MM (leading zero).      */
    If msg>'' Then
      Say center(arg(1) ct time(),79)  /* echo arg1 with time ?      */
    If ss==00 & ( mn==00 | mn==30 ) Then Do /*It's time to ring bell */
      dd=dd(hhmmc)                     /* compute number of times    */
      If msg>'' Then
        Say center(dd "bells",79)      /* echo bells?                */
      Do k=1 For dd
        Call beep 650,500
        Call syssleep 1+(k//2==0)
        End
      Call syssleep 60                 /* ensure don't re-peel.      */
      End
    Else
      Call syssleep (60-ss)
    End
/* test
time:
If arg(1)='C' Then
  res='8:30am'
Else
  res='08:30:00'
Return res
*/

dd: Parse Arg hhmmc
Parse Var hhmmc hh +2 ':' mm .
h=hh//4
If h=0 Then
  If mm=00 Then res=8
  Else res=1
Else
  res=2*h+(mm=30)
Return res

halt:
```



## Perl

```perl
use utf8;
binmode STDOUT, ":utf8";
use DateTime;

$| = 1; # to prevent output buffering

my @watch = <Middle Morning Forenoon Afternoon Dog First>;
my @ordinal = <One Two Three Four Five Six Seven Eight>;

my $thishour;
my $thisminute = '';

while () {
    my $utc = DateTime->now( time_zone => 'UTC' );
    if ($utc->minute =~ /^(00|30)$/ and $utc->minute != $thisminute) {
        $thishour   = $utc->hour;
        $thisminute = $utc->minute;
        bell($thishour, $thisminute);
    }
    printf "%s%02d:%02d:%02d", "\r", $utc->hour, $utc->minute, $utc->second;
    sleep(1);
}

sub bell {
    my($hour, $minute) = @_;

    my $bells = (($hour % 4) * 2 + int $minute/30) || 8;
    printf "%s%02d:%02d%9s watch,%6s Bell%s Gone: \t", "\b" x 9, $hour, $minute,
       $watch[(int($hour/4) - (0==($minute + $hour % 4)) + 6) % 6],
       $ordinal[$bells - 1], $bells == 1 ? '' : 's';
    chime($bells);
}

sub chime {
    my($count) = shift;
    for (1..int($count/2)) {
        print "\a♫ "; sleep .25;
        print "\a";   sleep .75;
    }
    if ($count % 2) {
        print "\a♪";  sleep 1;
    }
    print "\n";
}
```

<pre  style="height:35ex">00:30      Middle watch,     One Bell Gone:     ♪
01:00      Middle watch,     Two Bells Gone:    ♫
01:30      Middle watch,   Three Bells Gone:    ♫ ♪
02:00      Middle watch,    Four Bells Gone:    ♫ ♫
02:30      Middle watch,    Five Bells Gone:    ♫ ♫ ♪
03:00      Middle watch,     Six Bells Gone:    ♫ ♫ ♫
03:30      Middle watch,   Seven Bells Gone:    ♫ ♫ ♫ ♪
04:00      Middle watch,   Eight Bells Gone:    ♫ ♫ ♫ ♫
04:30     Morning watch,     One Bell Gone:     ♪
05:00     Morning watch,     Two Bells Gone:    ♫
05:30     Morning watch,   Three Bells Gone:    ♫ ♪
06:00     Morning watch,    Four Bells Gone:    ♫ ♫
06:30     Morning watch,    Five Bells Gone:    ♫ ♫ ♪
07:00     Morning watch,     Six Bells Gone:    ♫ ♫ ♫
07:30     Morning watch,   Seven Bells Gone:    ♫ ♫ ♫ ♪
08:00     Morning watch,   Eight Bells Gone:    ♫ ♫ ♫ ♫
08:30    Forenoon watch,     One Bell Gone:     ♪
09:00    Forenoon watch,     Two Bells Gone:    ♫
09:30    Forenoon watch,   Three Bells Gone:    ♫ ♪
10:00    Forenoon watch,    Four Bells Gone:    ♫ ♫
10:30    Forenoon watch,    Five Bells Gone:    ♫ ♫ ♪
11:00    Forenoon watch,     Six Bells Gone:    ♫ ♫ ♫
11:30    Forenoon watch,   Seven Bells Gone:    ♫ ♫ ♫ ♪
12:00    Forenoon watch,   Eight Bells Gone:    ♫ ♫ ♫ ♫
12:30   Afternoon watch,     One Bell Gone:     ♪
13:00   Afternoon watch,     Two Bells Gone:    ♫
13:30   Afternoon watch,   Three Bells Gone:    ♫ ♪
14:00   Afternoon watch,    Four Bells Gone:    ♫ ♫
14:30   Afternoon watch,    Five Bells Gone:    ♫ ♫ ♪
15:00   Afternoon watch,     Six Bells Gone:    ♫ ♫ ♫
15:30   Afternoon watch,   Seven Bells Gone:    ♫ ♫ ♫ ♪
16:00   Afternoon watch,   Eight Bells Gone:    ♫ ♫ ♫ ♫
16:30         Dog watch,     One Bell Gone:     ♪
17:00         Dog watch,     Two Bells Gone:    ♫
17:30         Dog watch,   Three Bells Gone:    ♫ ♪
18:00         Dog watch,    Four Bells Gone:    ♫ ♫
18:30         Dog watch,    Five Bells Gone:    ♫ ♫ ♪
19:00         Dog watch,     Six Bells Gone:    ♫ ♫ ♫
19:30         Dog watch,   Seven Bells Gone:    ♫ ♫ ♫ ♪
20:00         Dog watch,   Eight Bells Gone:    ♫ ♫ ♫ ♫
20:30       First watch,     One Bell Gone:     ♪
21:00       First watch,     Two Bells Gone:    ♫
21:30       First watch,   Three Bells Gone:    ♫ ♪
22:00       First watch,    Four Bells Gone:    ♫ ♫
22:30       First watch,    Five Bells Gone:    ♫ ♫ ♪
23:00       First watch,     Six Bells Gone:    ♫ ♫ ♫
23:30       First watch,   Seven Bells Gone:    ♫ ♫ ♫ ♪
24:00       First watch,   Eight Bells Gone:    ♫ ♫ ♫ ♫
```



## Perl 6


Perl 6 uses [[wp:Coordinated_Universal_Time|UTC]] (GMT) time internally and by default. This will display the current UTC time and on the half hour, display a graphical representation of the bell. If run in a terminal with the system bell enabled, will also chime the system alarm bell.


```perl6>my @watch = <Middle Morning Forenoon Afternoon Dog First
;
my @ordinal = <One Two Three Four Five Six Seven Eight>;
 
my $thishour;
my $thisminute = '';
 
loop {
    my $utc = DateTime.new(time);
    if $utc.minute ~~ any(0,30) and $utc.minute != $thisminute {
        $thishour   = $utc.hour;
        $thisminute = $utc.minute;
        bell($thishour, $thisminute);
    }
    printf "%s%02d:%02d:%02d", "\r", $utc.hour, $utc.minute, $utc.second;
    sleep(1);
}
 
sub bell ($hour, $minute) {
 
    my $bells = (($hour % 4) * 2 + $minute div 30) || 8;
 
    printf "%s%02d:%02d %9s watch, %6s Bell%s Gone: \t", "\b" x 9, $hour, $minute,
      @watch[($hour div 4 - !?($minute + $hour % 4) + 6) % 6],
      @ordinal[$bells - 1], $bells == 1 ?? '' !! 's';
 
    chime($bells);
 
    sub chime ($count) {
	for 1..$count div 2 {
		print "\a♫ ";
		sleep .25;
		print "\a";
		sleep .75;
	}
	if $count % 2 {
	     print "\a♪";
	     sleep 1;
        }
        print "\n";
    }
}
```

```txt
00:00     First watch,  Eight Bells Gone: 	♫ ♫ ♫ ♫ 
00:30    Middle watch,    One Bell Gone: 	♪
01:00    Middle watch,    Two Bells Gone: 	♫ 
01:30    Middle watch,  Three Bells Gone: 	♫ ♪
02:00    Middle watch,   Four Bells Gone: 	♫ ♫ 
02:30    Middle watch,   Five Bells Gone: 	♫ ♫ ♪
03:00    Middle watch,    Six Bells Gone: 	♫ ♫ ♫ 
03:30    Middle watch,  Seven Bells Gone: 	♫ ♫ ♫ ♪
04:00    Middle watch,  Eight Bells Gone: 	♫ ♫ ♫ ♫ 
04:30   Morning watch,    One Bell Gone: 	♪
05:00   Morning watch,    Two Bells Gone: 	♫ 
05:30   Morning watch,  Three Bells Gone: 	♫ ♪
06:00   Morning watch,   Four Bells Gone: 	♫ ♫ 
06:30   Morning watch,   Five Bells Gone: 	♫ ♫ ♪
07:00   Morning watch,    Six Bells Gone: 	♫ ♫ ♫ 
07:30   Morning watch,  Seven Bells Gone: 	♫ ♫ ♫ ♪
08:00   Morning watch,  Eight Bells Gone: 	♫ ♫ ♫ ♫ 
08:30  Forenoon watch,    One Bell Gone: 	♪
09:00  Forenoon watch,    Two Bells Gone: 	♫ 
09:30  Forenoon watch,  Three Bells Gone: 	♫ ♪
10:00  Forenoon watch,   Four Bells Gone: 	♫ ♫ 
10:30  Forenoon watch,   Five Bells Gone: 	♫ ♫ ♪
11:00  Forenoon watch,    Six Bells Gone: 	♫ ♫ ♫ 
11:30  Forenoon watch,  Seven Bells Gone: 	♫ ♫ ♫ ♪
12:00  Forenoon watch,  Eight Bells Gone: 	♫ ♫ ♫ ♫ 
12:30 Afternoon watch,    One Bell Gone: 	♪
13:00 Afternoon watch,    Two Bells Gone: 	♫ 
13:30 Afternoon watch,  Three Bells Gone: 	♫ ♪
14:00 Afternoon watch,   Four Bells Gone: 	♫ ♫ 
14:30 Afternoon watch,   Five Bells Gone: 	♫ ♫ ♪
15:00 Afternoon watch,    Six Bells Gone: 	♫ ♫ ♫ 
15:30 Afternoon watch,  Seven Bells Gone: 	♫ ♫ ♫ ♪
16:00 Afternoon watch,  Eight Bells Gone: 	♫ ♫ ♫ ♫ 
16:30       Dog watch,    One Bell Gone: 	♪
17:00       Dog watch,    Two Bells Gone: 	♫ 
17:30       Dog watch,  Three Bells Gone: 	♫ ♪
18:00       Dog watch,   Four Bells Gone: 	♫ ♫ 
18:30       Dog watch,   Five Bells Gone: 	♫ ♫ ♪
19:00       Dog watch,    Six Bells Gone: 	♫ ♫ ♫ 
19:30       Dog watch,  Seven Bells Gone: 	♫ ♫ ♫ ♪
20:00       Dog watch,  Eight Bells Gone: 	♫ ♫ ♫ ♫ 
20:30     First watch,    One Bell Gone: 	♪
21:00     First watch,    Two Bells Gone: 	♫ 
21:30     First watch,  Three Bells Gone: 	♫ ♪
22:00     First watch,   Four Bells Gone: 	♫ ♫ 
22:30     First watch,   Five Bells Gone: 	♫ ♫ ♪
23:00     First watch,    Six Bells Gone: 	♫ ♫ ♫ 
23:30     First watch,  Seven Bells Gone: 	♫ ♫ ♫ ♪
```



## Phix

Uses GMT, can easily be switched to local time by simply removing DT_GMT, ie invoking <code>date()</code>
instead of <code>date(DT_GMT)</code>.

Uses a full-length sleep of up to 1800 seconds (half an hour), as it should.

```Phix
constant watches = {"First","Middle","Morning","Forenoon","Afternoon","First dog","Last dog","First"},
         watch_ends = {"00:00", "04:00", "08:00", "12:00", "16:00", "18:00", "20:00", "23:59"},
         bells = {"One","Two","Three","Four","Five","Six","Seven","Eight"},
         ding = "ding!"

procedure nb(integer h,m)
    integer bell = mod(floor((h*60+m)/30),8)
    if bell==0 then bell = 8 end if
    string hm = sprintf("%02d:%02d",{h,m})
    integer watch=1
    while hm>watch_ends[watch] do watch += 1 end while
    string plural = iff(bell==1?" ":"s")
    string dings = ding
    for i=2 to bell do dings &= iff(mod(i,2)?" ":"")&ding end for
    printf(1,"%s %9s watch %5s bell%s  %s\n",
             {hm,watches[watch],bells[bell],plural,dings})
end procedure 

procedure simulate1day()
    for h=0 to 23 do
      for m=0 to 30 by 30 do
        nb(h,m)
      end for
    end for
    nb(0,0) -- (again)
end procedure

simulate1day()

while 1 do
    sequence d = date(DT_GMT)
    integer m = d[DT_SECOND] + mod(d[DT_MINUTE],30)*60
    if m=0 then
        nb(d[DT_HOUR],d[DT_MINUTE])
    end if
    sleep(30*60-m)
end while
```

```txt

00:00     First watch Eight bells  ding!ding! ding!ding! ding!ding! ding!ding!
00:30    Middle watch   One bell   ding!
01:00    Middle watch   Two bells  ding!ding!
01:30    Middle watch Three bells  ding!ding! ding!
02:00    Middle watch  Four bells  ding!ding! ding!ding!
02:30    Middle watch  Five bells  ding!ding! ding!ding! ding!
03:00    Middle watch   Six bells  ding!ding! ding!ding! ding!ding!
03:30    Middle watch Seven bells  ding!ding! ding!ding! ding!ding! ding!
04:00    Middle watch Eight bells  ding!ding! ding!ding! ding!ding! ding!ding!
04:30   Morning watch   One bell   ding!
05:00   Morning watch   Two bells  ding!ding!
05:30   Morning watch Three bells  ding!ding! ding!
06:00   Morning watch  Four bells  ding!ding! ding!ding!
06:30   Morning watch  Five bells  ding!ding! ding!ding! ding!
07:00   Morning watch   Six bells  ding!ding! ding!ding! ding!ding!
07:30   Morning watch Seven bells  ding!ding! ding!ding! ding!ding! ding!
08:00   Morning watch Eight bells  ding!ding! ding!ding! ding!ding! ding!ding!
08:30  Forenoon watch   One bell   ding!
09:00  Forenoon watch   Two bells  ding!ding!
09:30  Forenoon watch Three bells  ding!ding! ding!
10:00  Forenoon watch  Four bells  ding!ding! ding!ding!
10:30  Forenoon watch  Five bells  ding!ding! ding!ding! ding!
11:00  Forenoon watch   Six bells  ding!ding! ding!ding! ding!ding!
11:30  Forenoon watch Seven bells  ding!ding! ding!ding! ding!ding! ding!
12:00  Forenoon watch Eight bells  ding!ding! ding!ding! ding!ding! ding!ding!
12:30 Afternoon watch   One bell   ding!
13:00 Afternoon watch   Two bells  ding!ding!
13:30 Afternoon watch Three bells  ding!ding! ding!
14:00 Afternoon watch  Four bells  ding!ding! ding!ding!
14:30 Afternoon watch  Five bells  ding!ding! ding!ding! ding!
15:00 Afternoon watch   Six bells  ding!ding! ding!ding! ding!ding!
15:30 Afternoon watch Seven bells  ding!ding! ding!ding! ding!ding! ding!
16:00 Afternoon watch Eight bells  ding!ding! ding!ding! ding!ding! ding!ding!
16:30 First dog watch   One bell   ding!
17:00 First dog watch   Two bells  ding!ding!
17:30 First dog watch Three bells  ding!ding! ding!
18:00 First dog watch  Four bells  ding!ding! ding!ding!
18:30  Last dog watch  Five bells  ding!ding! ding!ding! ding!
19:00  Last dog watch   Six bells  ding!ding! ding!ding! ding!ding!
19:30  Last dog watch Seven bells  ding!ding! ding!ding! ding!ding! ding!
20:00  Last dog watch Eight bells  ding!ding! ding!ding! ding!ding! ding!ding!
20:30     First watch   One bell   ding!
21:00     First watch   Two bells  ding!ding!
21:30     First watch Three bells  ding!ding! ding!
22:00     First watch  Four bells  ding!ding! ding!ding!
22:30     First watch  Five bells  ding!ding! ding!ding! ding!
23:00     First watch   Six bells  ding!ding! ding!ding! ding!ding!
23:30     First watch Seven bells  ding!ding! ding!ding! ding!ding! ding!
00:00     First watch Eight bells  ding!ding! ding!ding! ding!ding! ding!ding!
13:00 Afternoon watch   Two bells  ding!ding!
13:30 Afternoon watch Three bells  ding!ding! ding!

```



## PL/I

This program sounds the bell as well as displaying "gong" an appropriate number of times. It operates on local time.

```PL/I

nautical: procedure options (main);      /* 29 October 2013 */
   declare (hour, t, i) fixed binary;

   do until (substr(time(), 3, 4) = '0000'); delay (1000); end;
   hour = substr(time(), 1, 2);

   do while ('1'b);
      do i = 1 to hour;
         put edit ('07'X) (a); put skip edit ('gong ') (a); delay (500);
      end;
      t = substr(time(), 5);
      delay (60*60*1000 - t);
   end;
end nautical;

```



## PowerShell


```PowerShell

function Get-Bell
{
    [CmdletBinding()]
    Param
    (
        [Parameter(Mandatory=$true,
                   Position=0)]
        [ValidateRange(1,12)]
        [int]
        $Hour,

        [Parameter(Mandatory=$true,
                   Position=1)]
        [ValidateSet(0,30)]
        [int]
        $Minute
    )

    $bells = @{
        OneBell    =          1
        TwoBells   =          2
        ThreeBells =       2, 1
        FourBells  =       2, 2
        FiveBells  =    2, 2, 1
        SixBells   =    2, 2, 2
        SevenBells = 2, 2, 2, 1
        EightBells = 2, 2, 2, 2
    }

    filter Invoke-Bell
    {
        if ($_ -eq 1)
        {
            [System.Media.SystemSounds]::Asterisk.Play()
            Write-Host -NoNewline "♪"
        }
        else
        {
            [System.Media.SystemSounds]::Exclamation.Play()
            Write-Host -NoNewline "♪♪  "
        }

        Start-Sleep -Milliseconds 500
    }


    $time = New-TimeSpan -Hours $Hour -Minutes $Minute

    switch ($time.Hours)
    {
         1 {if ($time.Minutes -eq 0) {$bells.TwoBells   | Invoke-Bell} else {$bells.ThreeBells | Invoke-Bell}; break}
         2 {if ($time.Minutes -eq 0) {$bells.FourBells  | Invoke-Bell} else {$bells.FiveBells  | Invoke-Bell}; break}
         3 {if ($time.Minutes -eq 0) {$bells.SixBells   | Invoke-Bell} else {$bells.SevenBells | Invoke-Bell}; break}
         4 {if ($time.Minutes -eq 0) {$bells.EightBells | Invoke-Bell} else {$bells.OneBell    | Invoke-Bell}; break}
         5 {if ($time.Minutes -eq 0) {$bells.TwoBells   | Invoke-Bell} else {$bells.ThreeBells | Invoke-Bell}; break}
         6 {if ($time.Minutes -eq 0) {$bells.FourBells  | Invoke-Bell} else {$bells.FiveBells  | Invoke-Bell}; break}
         7 {if ($time.Minutes -eq 0) {$bells.SixBells   | Invoke-Bell} else {$bells.SevenBells | Invoke-Bell}; break}
         8 {if ($time.Minutes -eq 0) {$bells.EightBells | Invoke-Bell} else {$bells.OneBell    | Invoke-Bell}; break}
         9 {if ($time.Minutes -eq 0) {$bells.TwoBells   | Invoke-Bell} else {$bells.ThreeBells | Invoke-Bell}; break}
        10 {if ($time.Minutes -eq 0) {$bells.FourBells  | Invoke-Bell} else {$bells.FiveBells  | Invoke-Bell}; break}
        11 {if ($time.Minutes -eq 0) {$bells.SixBells   | Invoke-Bell} else {$bells.SevenBells | Invoke-Bell}; break}
        12 {if ($time.Minutes -eq 0) {$bells.EightBells | Invoke-Bell} else {$bells.OneBell    | Invoke-Bell}}
    }

    Write-Host
}

Write-Host "Time Bells`n---- -----`n"

1..12 | ForEach-Object {

    $date = Get-Date -Hour $_ -Minute  0
    Write-Host -NoNewline "$($date.ToString("hh:mm")) "
    Get-Bell -Hour $_ -Minute  0

    $date = $date.AddMinutes(30)
    Write-Host -NoNewline "$($date.ToString("hh:mm")) "
    Get-Bell -Hour $_ -Minute 30
}

```

```txt

Time  Bells
----  -----

01:00 ♪♪  
01:30 ♪♪  ♪
02:00 ♪♪  ♪♪  
02:30 ♪♪  ♪♪  ♪
03:00 ♪♪  ♪♪  ♪♪  
03:30 ♪♪  ♪♪  ♪♪  ♪
04:00 ♪♪  ♪♪  ♪♪  ♪♪  
04:30 ♪
05:00 ♪♪  
05:30 ♪♪  ♪
06:00 ♪♪  ♪♪  
06:30 ♪♪  ♪♪  ♪
07:00 ♪♪  ♪♪  ♪♪  
07:30 ♪♪  ♪♪  ♪♪  ♪
08:00 ♪♪  ♪♪  ♪♪  ♪♪  
08:30 ♪
09:00 ♪♪  
09:30 ♪♪  ♪
10:00 ♪♪  ♪♪  
10:30 ♪♪  ♪♪  ♪
11:00 ♪♪  ♪♪  ♪♪  
11:30 ♪♪  ♪♪  ♪♪  ♪
12:00 ♪♪  ♪♪  ♪♪  ♪♪  
12:30 ♪

```



## Python

As well as typing output to stdout, this program plays a sound for each bell as the ␇ characters are printed (The spaces between the ␇ characters are mirrored as varying delays between each ring).

```python
import time, calendar, sched, winsound

duration = 750      # Bell duration in ms
freq = 1280         # Bell frequency in hertz
bellchar = "\u2407"
watches = 'Middle,Morning,Forenoon,Afternoon,First/Last dog,First'.split(',')

def gap(n=1):
    time.sleep(n * duration / 1000)
off = gap
 
def on(n=1):
    winsound.Beep(freq, n * duration)
 
def bong():
    on(); off(0.5)

def bongs(m):
    for i in range(m):
        print(bellchar, end=' ')
        bong()
        if i % 2:
            print('  ', end='')
            off(0.5)
    print('')
        
scheds =  sched.scheduler(time.time, time.sleep)

def ships_bell(now=None):
    def adjust_to_half_hour(atime):
        atime[4] = (atime[4] // 30) * 30
        atime[5] = 0
        return atime

    debug = now is not None
    rightnow = time.gmtime()
    if not debug:
        now = adjust_to_half_hour( list(rightnow) )
    then = now[::]
    then[4] += 30
    hr, mn = now[3:5]
    watch, b = divmod(int(2 * hr + mn // 30 - 1), 8)
    b += 1
    bells = '%i bell%s' % (b, 's' if b > 1 else ' ')
    if debug:
        print("%02i:%02i, %-20s %s" % (now[3], now[4], watches[watch] + ' watch', bells), end=' ')
    else:
        print("%02i:%02i, %-20s %s" % (rightnow[3], rightnow[4], watches[watch] + ' watch', bells), end=' ')
    bongs(b)
    if not debug:
        scheds.enterabs(calendar.timegm(then), 0, ships_bell)
        #print(time.struct_time(then))
        scheds.run()

def dbg_tester():
    for h in range(24):
        for m in (0, 30):
            if (h,m) == (24,30): break
            ships_bell( [2013, 3, 2, h, m, 15, 5, 61, 0] )
        
    
if __name__ == '__main__':
    ships_bell()
```

```txt
00:00, First watch          8 bells ␇ ␇   ␇ ␇   ␇ ␇   ␇ ␇   
00:30, Middle watch         1 bell  ␇ 
01:00, Middle watch         2 bells ␇ ␇   
01:30, Middle watch         3 bells ␇ ␇   ␇ 
02:00, Middle watch         4 bells ␇ ␇   ␇ ␇   
02:30, Middle watch         5 bells ␇ ␇   ␇ ␇   ␇ 
03:00, Middle watch         6 bells ␇ ␇   ␇ ␇   ␇ ␇   
03:30, Middle watch         7 bells ␇ ␇   ␇ ␇   ␇ ␇   ␇ 
04:00, Middle watch         8 bells ␇ ␇   ␇ ␇   ␇ ␇   ␇ ␇   
04:30, Morning watch        1 bell  ␇ 
05:00, Morning watch        2 bells ␇ ␇   
05:30, Morning watch        3 bells ␇ ␇   ␇ 
06:00, Morning watch        4 bells ␇ ␇   ␇ ␇   
06:30, Morning watch        5 bells ␇ ␇   ␇ ␇   ␇ 
07:00, Morning watch        6 bells ␇ ␇   ␇ ␇   ␇ ␇   
07:30, Morning watch        7 bells ␇ ␇   ␇ ␇   ␇ ␇   ␇ 
08:00, Morning watch        8 bells ␇ ␇   ␇ ␇   ␇ ␇   ␇ ␇   
08:30, Forenoon watch       1 bell  ␇ 
09:00, Forenoon watch       2 bells ␇ ␇   
09:30, Forenoon watch       3 bells ␇ ␇   ␇ 
10:00, Forenoon watch       4 bells ␇ ␇   ␇ ␇   
10:30, Forenoon watch       5 bells ␇ ␇   ␇ ␇   ␇ 
11:00, Forenoon watch       6 bells ␇ ␇   ␇ ␇   ␇ ␇   
11:30, Forenoon watch       7 bells ␇ ␇   ␇ ␇   ␇ ␇   ␇ 
12:00, Forenoon watch       8 bells ␇ ␇   ␇ ␇   ␇ ␇   ␇ ␇   
12:30, Afternoon watch      1 bell  ␇ 
13:00, Afternoon watch      2 bells ␇ ␇   
13:30, Afternoon watch      3 bells ␇ ␇   ␇ 
14:00, Afternoon watch      4 bells ␇ ␇   ␇ ␇   
14:30, Afternoon watch      5 bells ␇ ␇   ␇ ␇   ␇ 
15:00, Afternoon watch      6 bells ␇ ␇   ␇ ␇   ␇ ␇   
15:30, Afternoon watch      7 bells ␇ ␇   ␇ ␇   ␇ ␇   ␇ 
16:00, Afternoon watch      8 bells ␇ ␇   ␇ ␇   ␇ ␇   ␇ ␇   
16:30, First/Last dog watch 1 bell  ␇ 
17:00, First/Last dog watch 2 bells ␇ ␇   
17:30, First/Last dog watch 3 bells ␇ ␇   ␇ 
18:00, First/Last dog watch 4 bells ␇ ␇   ␇ ␇   
18:30, First/Last dog watch 5 bells ␇ ␇   ␇ ␇   ␇ 
19:00, First/Last dog watch 6 bells ␇ ␇   ␇ ␇   ␇ ␇   
19:30, First/Last dog watch 7 bells ␇ ␇   ␇ ␇   ␇ ␇   ␇ 
20:00, First/Last dog watch 8 bells ␇ ␇   ␇ ␇   ␇ ␇   ␇ ␇   
20:30, First watch          1 bell  ␇ 
21:00, First watch          2 bells ␇ ␇   
21:30, First watch          3 bells ␇ ␇   ␇ 
22:00, First watch          4 bells ␇ ␇   ␇ ␇   
22:30, First watch          5 bells ␇ ␇   ␇ ␇   ␇ 
23:00, First watch          6 bells ␇ ␇   ␇ ␇   ␇ ␇   
23:30, First watch          7 bells ␇ ␇   ␇ ␇   ␇ ␇   ␇ 
```



## Racket


This solution uses local time.  It also doesn't poll. If it's printing to a terminal
that displays \a by playing the system bell, it will play the system bell.


```racket
#lang racket

(require racket/date)

(define HALF-HOUR-SECS (* 60 30))

;; given a date, return the seconds corresponding to the beginning
;; of that day (in local time)
(define (beginning-of-date d)
  (find-seconds 0 0 0 (date-day d) (date-month d) (date-year d)))

;; the seconds at the beginning of today:
(define today-secs
  (beginning-of-date
    (seconds->date (current-seconds))))

;; hours -> watch : given an hour, return the watch name
(define (hours->watch hours)
  (cond [(= 0 hours) "first"]
        [(< 0 hours 4.5) "middle"]
        [(< 4 hours 8.5) "morning"]
        [(< 8 hours 12.5) "forenoon"]
        [(< 12 hours 16.5) "afternoon"]
        [(< 16 hours 20.5) "dog"]
        [(< 20 hours 24.5) "first"]))

;; wait until current-seconds is the given number
(define (wait-til secs)
  (sleep (- secs (current-seconds))))

;; display the appropriate message
(define (format-and-print hours bells)
  (define int-hours (floor hours))
  (define minutes (cond [(integer? hours) "00"]
                        [else "30"]))
  (display
   (~a 
    (~a (floor hours) #:min-width 2 #:pad-string "0" 
        #:align 'right)
    ":" minutes ", " bells " bell(s) of the "
    (hours->watch hours) " watch "))
  ;; play the bells, if possible:
  (for ([i bells])
    (display "\a♪")
    (flush-output)
    (cond [(even? i) (sleep 0.5)]
          [(odd? i) (display " ") (sleep 1)]))
  (display "\n"))

;; start the loop:
(for ([s (in-range today-secs +inf.0 HALF-HOUR-SECS)]
      [bells (sequence-tail (in-cycle (in-range 8)) 7)]
      [hours (in-cycle (in-range 0 24 1/2))])
  ;; ignore the ones that have already happened:
  (when (< (current-seconds) s)
    (wait-til s)
    (format-and-print hours (add1 bells))))

```


This might produce the following output: 


```txt

21:00, 2 bell(s) of the first watch �♪�♪ 
21:30, 3 bell(s) of the first watch �♪�♪ �♪
22:00, 4 bell(s) of the first watch �♪�♪ �♪�♪ 
22:30, 5 bell(s) of the first watch �♪�♪ �♪�♪ �♪
23:00, 6 bell(s) of the first watch �♪�♪ �♪�♪ �♪�♪ 
23:30, 7 bell(s) of the first watch �♪�♪ �♪�♪ �♪�♪ �♪
00:00, 8 bell(s) of the first watch �♪�♪ �♪�♪ �♪�♪ �♪�♪ 
00:30, 1 bell(s) of the middle watch �♪
01:00, 2 bell(s) of the middle watch �♪�♪ 

```




## REXX

The local time is used instead of Greenwich mean time.

If any arguments are specified, that text is used as a prefix to the times shown (once a minute). 

Also, the number of bells sounded are shown   (if any arguments are specified).

If no arguments are specified, no times are shown.

In all cases, the PC speaker is used to sound the bells (albeit a poorly sounded bell).

This REXX program makes use of   '''delay'''   BIF,   which delays (sleeps) for a specified amount of seconds.

Some REXXes don't have a   '''delay'''   BIF,   so one is included   <big> [[DELAY.REX|here]]. </big>

Also, some REXXes don't have a   '''sound'''   BIF,   which produces sounds via the PC speaker,
  so one is included   <big> [[SOUND.REX|here]]. </big>

```rexx
/*REXX program sounds "ship's bells"  (using PC speaker)  when executing  (perpetually).*/
echo= ( arg()\==0 )                              /*echo time and bells if any arguments.*/
signal on halt                                   /*allow a clean way to stop the program*/
                   t.1=  '00:30   01:00   01:30   02:00   02:30   03:00   03:30   04:00'
                   t.2=  '04:30   05:00   05:30   06:00   06:30   07:00   07:30   08:00'
                   t.3=  '08:30   09:00   09:30   10:00   10:30   11:00   11:30   12:00'

      do forever;  t=time();   ss=right(t, 2);   mn=substr(t, 4, 2)  /*the current time.*/
      ct=time('C')                               /*[↓]  maybe add leading zero to time. */
      hhmmc=left( right( ct, 7, 0),  5)          /*HH:MM   (maybe with a leading zero). */
      if echo  then say center(arg(1) ct, 79)    /*echo the 1st argument with the time? */
      if ss\==00 & mn\==00 & mn\==30  then do;  call delay  60-ss;   iterate
                                           end   /* [↑]  delay for fraction of a minute.*/
                                                 /* [↓]  the number of bells to peel {$}*/
                     do j=1  for 3  until $\==0;   $=wordpos(hhmmc, t.j)
                     end   /*j*/

      if $\==0 & echo  then say center($ "bells", 79)       /*echo the number of bells? */

                     do k=1 for $;  call sound 650,1;  call delay 1 + (k//2==0)
                     end   /*k*/                 /*[↑]  peel, and then pause for effect.*/
      call delay 60;       if rc\==0  then leave /*ensure we don't re─peel.             */
      end   /*forever*/
halt:                                            /*stick a fork in it,  we're all done. */
```

```txt

                             the time is:   1:48pm
                             the time is:   1:49pm
                             the time is:   1:50pm
                             the time is:   1:51pm
                             the time is:   1:52pm
                             the time is:   1:53pm
                             the time is:   1:54pm
                             the time is:   1:55pm
                             the time is:   1:56pm
                             the time is:   1:57pm
                             the time is:   1:58pm
                             the time is:   1:59pm
                             the time is:   2:00pm
                                    4 bells
                             the time is:   2:01pm
                             the time is:   2:02pm
 ∙
 ∙
 ∙

```



## Ring


```ring

# Project : Nautical bell

m = 0
for n = 0 to 23
     if n = 23
        see "23" + ":30" + " = " + "7 bells" + nl
     else
        m = m + 1
        see "" + n%23 + ":30" + " = " + m + " bells" + nl
     ok
     if n = 23
        see "00" + ":00" + " = " + "8 bells" + nl
     else
        m = m + 1
        see "" + (n%23+1) + ":00" + " = " + m + " bells" + nl
        if m = 8
           m = 0
        ok
     ok
next

```

Output:

```txt

0:30 = 1 bells
1:00 = 2 bells
1:30 = 3 bells
2:00 = 4 bells
2:30 = 5 bells
3:00 = 6 bells
3:30 = 7 bells
4:00 = 8 bells
4:30 = 1 bells
5:00 = 2 bells
5:30 = 3 bells
6:00 = 4 bells
6:30 = 5 bells
7:00 = 6 bells
7:30 = 7 bells
8:00 = 8 bells
8:30 = 1 bells
9:00 = 2 bells
9:30 = 3 bells
10:00 = 4 bells
10:30 = 5 bells
11:00 = 6 bells
11:30 = 7 bells
12:00 = 8 bells
12:30 = 1 bells
13:00 = 2 bells
13:30 = 3 bells
14:00 = 4 bells
14:30 = 5 bells
15:00 = 6 bells
15:30 = 7 bells
16:00 = 8 bells
16:30 = 1 bells
17:00 = 2 bells
17:30 = 3 bells
18:00 = 4 bells
18:30 = 5 bells
19:00 = 6 bells
19:30 = 7 bells
20:00 = 8 bells
20:30 = 1 bells
21:00 = 2 bells
21:30 = 3 bells
22:00 = 4 bells
22:30 = 5 bells
23:00 = 6 bells
23:30 = 7 bells
00:00 = 8 bells

```



## Ruby


```ruby
watches = [ "First", "Middle", "Morning", "Forenoon", "Afternoon", "First dog", "Last dog", "First" ]
watch_ends = [ "00:00", "04:00", "08:00", "12:00", "16:00", "18:00", "20:00", "23:59" ]
words = ["One","Two","Three","Four","Five","Six","Seven","Eight"]
sound = "ding!"

loop do
  time = Time.now
  if time.sec == 0 and time.min % 30 == 0
    num = (time.hour * 60 + time.min) / 30 % 8
    num = 8 if num == 0
    hr_min = time.strftime "%H:%M"
    idx = watch_ends.find_index {|t| hr_min <= t}
    text = "%s - %s watch, %s bell%s gone" % [
        hr_min, 
        watches[idx], 
        words[num-1], 
        num==1 ? "" : "s"
    ]
    bells = (sound * num).gsub(sound + sound) {|dd| dd + ' '}
    puts "%-45s %s" % [text, bells]
  end
  sleep 1
end
```


<pre style="height:64ex;overflow:scroll">
00:00 - First watch, Eight bells gone         ding!ding! ding!ding! ding!ding! ding!ding! 
00:30 - Middle watch, One bell gone           ding!
01:00 - Middle watch, Two bells gone          ding!ding! 
01:30 - Middle watch, Three bells gone        ding!ding! ding!
02:00 - Middle watch, Four bells gone         ding!ding! ding!ding! 
02:30 - Middle watch, Five bells gone         ding!ding! ding!ding! ding!
03:00 - Middle watch, Six bells gone          ding!ding! ding!ding! ding!ding! 
03:30 - Middle watch, Seven bells gone        ding!ding! ding!ding! ding!ding! ding!
04:00 - Middle watch, Eight bells gone        ding!ding! ding!ding! ding!ding! ding!ding! 
04:30 - Morning watch, One bell gone          ding!
05:00 - Morning watch, Two bells gone         ding!ding! 
05:30 - Morning watch, Three bells gone       ding!ding! ding!
06:00 - Morning watch, Four bells gone        ding!ding! ding!ding! 
06:30 - Morning watch, Five bells gone        ding!ding! ding!ding! ding!
07:00 - Morning watch, Six bells gone         ding!ding! ding!ding! ding!ding! 
07:30 - Morning watch, Seven bells gone       ding!ding! ding!ding! ding!ding! ding!
08:00 - Morning watch, Eight bells gone       ding!ding! ding!ding! ding!ding! ding!ding! 
08:30 - Forenoon watch, One bell gone         ding!
09:00 - Forenoon watch, Two bells gone        ding!ding! 
09:30 - Forenoon watch, Three bells gone      ding!ding! ding!
10:00 - Forenoon watch, Four bells gone       ding!ding! ding!ding! 
10:30 - Forenoon watch, Five bells gone       ding!ding! ding!ding! ding!
11:00 - Forenoon watch, Six bells gone        ding!ding! ding!ding! ding!ding! 
11:30 - Forenoon watch, Seven bells gone      ding!ding! ding!ding! ding!ding! ding!
12:00 - Forenoon watch, Eight bells gone      ding!ding! ding!ding! ding!ding! ding!ding! 
12:30 - Afternoon watch, One bell gone        ding!
13:00 - Afternoon watch, Two bells gone       ding!ding! 
13:30 - Afternoon watch, Three bells gone     ding!ding! ding!
14:00 - Afternoon watch, Four bells gone      ding!ding! ding!ding! 
14:30 - Afternoon watch, Five bells gone      ding!ding! ding!ding! ding!
15:00 - Afternoon watch, Six bells gone       ding!ding! ding!ding! ding!ding! 
15:30 - Afternoon watch, Seven bells gone     ding!ding! ding!ding! ding!ding! ding!
16:00 - Afternoon watch, Eight bells gone     ding!ding! ding!ding! ding!ding! ding!ding! 
16:30 - First dog watch, One bell gone        ding!
17:00 - First dog watch, Two bells gone       ding!ding! 
17:30 - First dog watch, Three bells gone     ding!ding! ding!
18:00 - First dog watch, Four bells gone      ding!ding! ding!ding! 
18:30 - Last dog watch, Five bells gone       ding!ding! ding!ding! ding!
19:00 - Last dog watch, Six bells gone        ding!ding! ding!ding! ding!ding! 
19:30 - Last dog watch, Seven bells gone      ding!ding! ding!ding! ding!ding! ding!
20:00 - Last dog watch, Eight bells gone      ding!ding! ding!ding! ding!ding! ding!ding! 
20:30 - First watch, One bell gone            ding!
21:00 - First watch, Two bells gone           ding!ding! 
21:30 - First watch, Three bells gone         ding!ding! ding!
22:00 - First watch, Four bells gone          ding!ding! ding!ding! 
22:30 - First watch, Five bells gone          ding!ding! ding!ding! ding!
23:00 - First watch, Six bells gone           ding!ding! ding!ding! ding!ding! 
23:30 - First watch, Seven bells gone         ding!ding! ding!ding! ding!ding! ding!
00:00 - First watch, Eight bells gone         ding!ding! ding!ding! ding!ding! ding!ding! 

```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "time.s7i";
  include "duration.s7i";
  include "keybd.s7i";

const array string: watch is [] ("Middle", "Morning", "Forenoon", "Afternoon", "Dog", "First");
const array string: bells is [] ("One Bell", "Two Bells", "Three Bells", "Four Bells", "Five Bells", "Six Bells", "Seven Bells", "Eight Bells");

const func time: truncToHalfHour (in time: aTime) is func
  result
    var time: truncatedTime is time.value;
  begin
    truncatedTime := aTime;
    truncatedTime.minute := aTime.minute div 30 * 30;
    truncatedTime.second := 0;
    truncatedTime.micro_second := 0;
  end func;

const proc: main is func
  local
    var time: nextTime is time.value;
    var time: midnight is time.value;
    var integer: minutes is 0;
  begin
    writeln;
    nextTime := truncToHalfHour(time(NOW));
    midnight := truncToDay(nextTime);
    while TRUE do
      nextTime +:= 30 . MINUTES;
      await(nextTime);
      minutes := toMinutes(nextTime - midnight);
      writeln(str_hh_mm(nextTime, ":") <& " - " <&
              watch[succ((minutes - 30) mdiv 240 mod 6)] <& " watch - " <&
              bells[succ((minutes - 30) mdiv 30 mod 8)] <& " Gone.");
      flush(OUT);
    end while;
  end func;
```


```txt

00:00 - First watch - Eight Bells Gone.
00:30 - Middle watch - One Bell Gone.
01:00 - Middle watch - Two Bells Gone.
01:30 - Middle watch - Three Bells Gone.
02:00 - Middle watch - Four Bells Gone.
02:30 - Middle watch - Five Bells Gone.
03:00 - Middle watch - Six Bells Gone.
03:30 - Middle watch - Seven Bells Gone.
04:00 - Middle watch - Eight Bells Gone.
04:30 - Morning watch - One Bell Gone.
05:00 - Morning watch - Two Bells Gone.
05:30 - Morning watch - Three Bells Gone.
06:00 - Morning watch - Four Bells Gone.
06:30 - Morning watch - Five Bells Gone.
07:00 - Morning watch - Six Bells Gone.
07:30 - Morning watch - Seven Bells Gone.
08:00 - Morning watch - Eight Bells Gone.
08:30 - Forenoon watch - One Bell Gone.
09:00 - Forenoon watch - Two Bells Gone.
09:30 - Forenoon watch - Three Bells Gone.
10:00 - Forenoon watch - Four Bells Gone.
10:30 - Forenoon watch - Five Bells Gone.
11:00 - Forenoon watch - Six Bells Gone.
11:30 - Forenoon watch - Seven Bells Gone.
12:00 - Forenoon watch - Eight Bells Gone.
12:30 - Afternoon watch - One Bell Gone.
13:00 - Afternoon watch - Two Bells Gone.
13:30 - Afternoon watch - Three Bells Gone.
14:00 - Afternoon watch - Four Bells Gone.
14:30 - Afternoon watch - Five Bells Gone.
15:00 - Afternoon watch - Six Bells Gone.
15:30 - Afternoon watch - Seven Bells Gone.
16:00 - Afternoon watch - Eight Bells Gone.
16:30 - Dog watch - One Bell Gone.
17:00 - Dog watch - Two Bells Gone.
17:30 - Dog watch - Three Bells Gone.
18:00 - Dog watch - Four Bells Gone.
18:30 - Dog watch - Five Bells Gone.
19:00 - Dog watch - Six Bells Gone.
19:30 - Dog watch - Seven Bells Gone.
20:00 - Dog watch - Eight Bells Gone.
20:30 - First watch - One Bell Gone.
21:00 - First watch - Two Bells Gone.
21:30 - First watch - Three Bells Gone.
22:00 - First watch - Four Bells Gone.
22:30 - First watch - Five Bells Gone.
23:00 - First watch - Six Bells Gone.
23:30 - First watch - Seven Bells Gone.

```



## Tcl

This code was originally based on the [[#Perl 6|Perl 6]] version, but with numerous adaptations, alterations and (some) corrections.

```tcl
# More sophisticated versions are possible, such as playing a bell sample
# using the Snack library.
proc ringTheBell {} {
    puts -nonewline "\a"
}

# The code to convert the (parsed) time into rings of the ship's bell and
# printing of the name of the bell.
proc strikeBell {hour minute} {
    global suppressNormalOutput
    set watches {
	Middle Middle Morning Morning Forenoon Forenoon
	Afternoon Afternoon {First dog} {Last dog} First First
    }
    set cardinals {one two three four five six seven eight}
    set bells [expr {(($hour % 4) * 2 + $minute / 30)}]
    if {!$bells} {set bells 8}
    puts -nonewline [format "%02d:%02d %9s watch, %6s bell%s gone: \t" \
	    $hour $minute [lindex $watches [expr {
		($hour/2 - ($minute==0 && $hour%2==0)) % 12
	    }]] [lindex $cardinals [expr {$bells - 1}]] \
	    [expr {$bells == 1 ? "" : "s"}]]

    # Set up the ringing of the bells to be done asynchronously
    set t 0
    set suppressNormalOutput 1
    for {set i 0} {$i < $bells-1} {incr i 2} {
	after $t {
	    ringTheBell
	    puts -nonewline "\u266b "
	}
	incr t 250
	after $t {
	    ringTheBell
	}
	incr t 750
    }
    if {$bells % 2} {
	after $t {
	    ringTheBell
	    puts -nonewline "\u266a\n"
	    set suppressNormalOutput 0
	}
    } else {
	after $t {
	    puts ""
	    set suppressNormalOutput 0
	}
    }
}

# Main handler; designed to be called every second, which is plenty.
proc nauticalBell {} {
    global last suppressNormalOutput
    scan [clock format [clock seconds] -format "%H:%M" -gmt 1] "%d:%d" h m
    if {$last != $m} {
	set last $m
	if {$m%30 == 0} {
	    strikeBell $h $m
	} elseif {!$suppressNormalOutput} {
	    puts -nonewline [format "%02d:%02d\r" $h $m]
	}
    }
}

# Set things up, using Tcl's event loop to do the processing
proc every {delay script} {
    after $delay [list every $delay $script]
    uplevel #0 $script
}
set last ""
set suppressNormalOutput 0
fconfigure stdout -buffering none
every 1000 nauticalBell
vwait forever;   # Only needed if not running an event loop otherwise
```

```txt

⋮
08:00   Morning watch,  Eight Bells Gone: 	♫ ♫ ♫ ♫ 
⋮
18:00 First dog watch,   Four Bells Gone: 	♫ ♫ 
18:30  Last dog watch,   Five Bells Gone: 	♫ ♫ ♪
⋮

```

