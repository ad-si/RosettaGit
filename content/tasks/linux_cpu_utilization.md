+++
title = "Linux CPU utilization"
description = ""
date = 2019-07-05T13:28:19Z
aliases = []
[extra]
id = 19186
[taxonomies]
categories = ["task"]
tags = []
+++

## Task

Display the current CPU utilization, as a percentage, calculated from <code> /proc/stat</code>.


;Background:
Most Linux kernels provide a virtual   <code>[https://www.kernel.org/doc/Documentation/filesystems/proc.txt /proc]</code>   filesystem, providing an interface to various internal data structures.

One of these internal structures (<code>/proc/stat</code>) includes information on the amount of time (in <code>USER_HZ</code>) spent in various states.   From this information, we can, with a little effort, determine the current level of CPU utilization, as a percent of time spent in any states other than ''idle''.

To do this:
:::#   read the first line of   <code>/proc/stat</code>
:::#   discard the first word of that first line   (it's always <code>cpu</code>)
:::#   sum all of the times found on that first line to get the total time
:::#   divide the fourth column ("idle") by the total time, to get the fraction of time spent being idle
:::#   subtract the previous fraction from 1.0 to get the time spent being   ''not''   idle
:::#   multiple by   '''100'''   to get a percentage



The times in <code>/proc/stat</code> are monotonically increasing, and begin at some point during the kernel's initialization (ie during boot up).   So the steps above will give the total CPU utilization since boot, which may or may not be useful to some.   To get a more real-time utilization, we simply repeat the steps above with some small sleep interval in between, and instead of using the absolute total and idle times, we use use the total time delta and the idle time delta to compute the utilization.

This project is based on   [http://colby.id.au/calculating-cpu-usage-from-proc-stat/ this blog post]   by [[User:Paul|Paul Colby]],   and the   [[#UNIX Shell|Bash]]   version comes from there.





## AWK



```AWK

BEGIN {
  prev_total = 0
  prev_idle = 0
  while (getline < "/proc/stat") {
    close("/proc/stat")
    idle = $5
    total = 0
    for (i=2; i<=NF; i++)
      total += $i
    print (1-(idle-prev_idle)/(total-prev_total))*100"%"
    prev_idle = idle
    prev_total = total
    system("sleep 1")
  }
}

```

Invocation and output:

```txt

>$ awk -f cpu.awk
28.4785%
7.32323%
7.30479%
6.07595%
6.32911%

```

Alternatively, the output can continuously overwrite itself to remain on a single line a la the [[#UNIX Shell|UNIX Shell]] version, by replacing the <code>print</code> line with:

```AWK

    printf "\rCPU: %5.1f%%  \b\b",(1-(idle-prev_idle)/(total-prev_total))*100

```



## C

On Linux, sleep accepts seconds, on Windows, milliseconds. So if you are planning to test this on Windows and then port it to Linux .....The implementation below has been tested on a Linux machine.

```C

#include<stdlib.h>
#include<string.h>
#include<unistd.h>
#include<stdio.h>

int main(int argC,char* argV[])
{
	char str[100];
	const char d[2] = " ";
	char* token;
	int i = 0,times,lag;
	long int sum = 0, idle, lastSum = 0,lastIdle = 0;
	long double idleFraction;

	if(argC!=3)
		printf("Usage : %s <number of times /proc/stat should be polled followed by delay in seconds.>",argV[0]);

	else{
		times = atoi(argV[1]);
		lag = atoi(argV[2]);

		while(times>0){
			FILE* fp = fopen("/proc/stat","r");
	        i = 0;
			fgets(str,100,fp);
			fclose(fp);
			token = strtok(str,d);

			while(token!=NULL){
				token = strtok(NULL,d);
				if(token!=NULL){
					sum += atoi(token);

				if(i==3)
					idle = atoi(token);

				i++;
			}
		}

		printf("\nIdle for : %lf %% of the time.",(1.0 - (idle-lastIdle)*1.0/(sum-lastSum))*100);

		lastIdle = idle;
		lastSum = sum;


		times--;
		sleep(lag);
		}
	}
	return 0;
}

```

Invocation and output :

```txt

/home/aamrun/rosettaCode>./cpuStat 3 1
Idle for : 16.418535 % of the time.
Idle for : 99.999970 % of the time.
Idle for : 99.999971 % of the time.

```



## C++


```cpp
#include <fstream>
#include <iostream>
#include <numeric>
#include <unistd.h>
#include <vector>

std::vector<size_t> get_cpu_times() {
    std::ifstream proc_stat("/proc/stat");
    proc_stat.ignore(5, ' '); // Skip the 'cpu' prefix.
    std::vector<size_t> times;
    for (size_t time; proc_stat >> time; times.push_back(time));
    return times;
}

bool get_cpu_times(size_t &idle_time, size_t &total_time) {
    const std::vector<size_t> cpu_times = get_cpu_times();
    if (cpu_times.size() < 4)
        return false;
    idle_time = cpu_times[3];
    total_time = std::accumulate(cpu_times.begin(), cpu_times.end(), 0);
    return true;
}

int main(int, char *[]) {
    size_t previous_idle_time=0, previous_total_time=0;
    for (size_t idle_time, total_time; get_cpu_times(idle_time, total_time); sleep(1)) {
        const float idle_time_delta = idle_time - previous_idle_time;
        const float total_time_delta = total_time - previous_total_time;
        const float utilization = 100.0 * (1.0 - idle_time_delta / total_time_delta);
        std::cout << utilization << '%' << std::endl;
        previous_idle_time = idle_time;
        previous_total_time = total_time;
    }
}
```

```txt

5.77731%
2.24439%
1.50754%
1.50754%
2.50627%

```


=== {{libheader|Qt}} ===

### = Qt Console Variant 1 =

A very basic [[Qt]]-port of the the [[#C++|C++]] implementation above.

```cpp

#include <QFile>
#include <unistd.h>

int main(int, char *[]) {
    int prevIdleTime = 0, prevTotalTime = 0;
    for (QFile file("/proc/stat"); file.open(QFile::ReadOnly); file.close()) {
        const QList<QByteArray> times = file.readLine().simplified().split(' ').mid(1);
        const int idleTime = times.at(3).toInt();
        int totalTime = 0;
        foreach (const QByteArray &time, times) {
            totalTime += time.toInt();
        }
        qInfo("%5.1f%%", (1 - (1.0*idleTime-prevIdleTime) / (totalTime-prevTotalTime)) * 100.0);
        prevIdleTime = idleTime;
        prevTotalTime = totalTime;
        sleep(1);
    }
}

```

```txt

 31.7%
  7.3%
 10.3%
  1.3%
  4.0%

```



### = Qt Console Variant 2 =

An event-based [[Qt]] console implementation. Note, this version does not rely on any system-dependant headers or functions.

```cpp

#include <QCoreApplication>
#include <QFile>

class CpuUsage : public QObject {
public:
    CpuUsage() : prevIdleTime(0), prevTotalTime(0) { }

protected:
    virtual void timerEvent(QTimerEvent *)
    {
        QFile file("/proc/stat");
        file.open(QFile::ReadOnly);
        const QList<QByteArray> times = file.readLine().simplified().split(' ').mid(1);
        const int idleTime = times.at(3).toInt();
        int totalTime = 0;
        foreach (const QByteArray &time, times) {
            totalTime += time.toInt();
        }
        qInfo("%5.1f%%", (1 - (1.0*idleTime-prevIdleTime) / (totalTime-prevTotalTime)) * 100.0);
        prevIdleTime = idleTime;
        prevTotalTime = totalTime;
    }

private:
    int prevIdleTime;
    int prevTotalTime;
};

int main(int argc, char *argv[]) {
    QCoreApplication app(argc, argv);
    CpuUsage usage;
    usage.startTimer(1000);
    return app.exec();
}

```

```txt

 31.7%
  4.2%
  3.5%
 16.2%
  2.5%
  4.5%

```



### = Qt GUI =

A GUI version, using the [[Qt]] framework.

```cpp

#include <QApplication>
#include <QFile>
#include <QProgressDialog>

class CpuUsage : public QProgressDialog {
public:
    CpuUsage() : prevIdleTime(0), prevTotalTime(0)
    {
        connect(this, &QProgressDialog::canceled, &QApplication::quit);
        setLabelText(tr("CPU utilization"));
        setCancelButtonText(tr("Quit"));
        startTimer(500);
    }

protected:
    virtual void timerEvent(QTimerEvent *)
    {
        QFile file("/proc/stat");
        file.open(QFile::ReadOnly);
        const QList<QByteArray> times = file.readLine().simplified().split(' ').mid(1);
        const int idleTime = times.at(3).toInt();
        int totalTime = 0;
        foreach (const QByteArray &time, times) {
            totalTime += time.toInt();
        }
        setValue((1 - (1.0*idleTime-prevIdleTime) / (totalTime-prevTotalTime)) * 100.0);
        prevIdleTime = idleTime;
        prevTotalTime = totalTime;
    }

private:
    int prevIdleTime;
    int prevTotalTime;
};

int main(int argc, char *argv[]) {
    QApplication app(argc, argv);
    CpuUsage usage;
    usage.show();
    return app.exec();
}

```


[[File:Linux CPU utilization Qt GUI.png]]


## Fortran


```fortran

Program CPUusage
    implicit none
    integer :: ios, i
    integer :: oldidle, oldsum, sumtimes = 0
    real :: percent = 0.
    character(len = 4) lineID ! 'cpu '
    integer, dimension(9) :: times = 0

    write(*, *) 'CPU Usage'
    write(*, *) 'Press Ctrl<C> to end'
    do while (.true.)
        open(unit = 7, file = '/proc/stat', status = 'old', action = 'read', iostat = ios)
        if (ios /= 0) then
            print *, 'Error opening /proc/stat'
            stop
        else
            read(unit = 7, fmt = *, iostat = ios) lineID, (times(i), i = 1, 9)
            close(7)
            if (lineID /= 'cpu ') then
                print *, 'Error reading /proc/stat'
                stop
            end if
            sumtimes = sum(times)
            percent = (1. - real((times(4) - oldidle)) / real((sumtimes - oldsum))) * 100.
            write(*, fmt = '(F6.2,A2)') percent, '%'
            oldidle = times(4)
            oldsum = sumtimes
            call sleep(1)
        end if
    end do
end program CPUusage

```


```txt
CPU Usage
 Press Ctrl<C> to end
  7.51 %
 18.23 %
  4.60 %
  4.53 %
  3.53 %
  2.53 %
```



## Go

```go
package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
    "strconv"
    "strings"
    "time"
)

func main() {
    fmt.Println("CPU usage % at 1 second intervals:\n")
    var prevIdleTime, prevTotalTime uint64
    for i := 0; i < 10; i++ {
        file, err := os.Open("/proc/stat")
        if err != nil {
            log.Fatal(err)
        }
        scanner := bufio.NewScanner(file)
        scanner.Scan()
        firstLine := scanner.Text()[5:] // get rid of cpu plus 2 spaces
        file.Close()
        if err := scanner.Err(); err != nil {
            log.Fatal(err)
        }
        split := strings.Fields(firstLine)
        idleTime, _ := strconv.ParseUint(split[3], 10, 64)
        totalTime := uint64(0)
        for _, s := range split {
            u, _ := strconv.ParseUint(s, 10, 64)
            totalTime += u
        }
        if i > 0 {
            deltaIdleTime := idleTime - prevIdleTime
            deltaTotalTime := totalTime - prevTotalTime
            cpuUsage := (1.0 - float64(deltaIdleTime)/float64(deltaTotalTime)) * 100.0
            fmt.Printf("%d : %6.3f\n", i, cpuUsage)
        }
        prevIdleTime = idleTime
        prevTotalTime = totalTime
        time.Sleep(time.Second)
    }
}
```


Sample output:

```txt

CPU usage % at 1 second intervals:

1 :  5.025
2 :  7.035
3 :  6.030
4 :  3.941
5 :  2.538
6 :  2.010
7 :  2.020
8 :  3.015
9 :  2.000

```



## Haskell


```Haskell
import Data.List ( (!!) )

splitString :: Char -> String -> [String]
splitString c [] = []
splitString c s = let ( item , rest ) = break ( == c ) s
                      ( _ , next ) = break ( /= c ) rest
		  in item : splitString c next

computeUsage :: String -> Double
computeUsage s = (1.0 - ((lineElements !! 3 ) /  sum times)) * 100
   where
      lineElements = map (fromInteger . read ) $ tail $ splitString ' ' s
      times = tail lineElements

main :: IO ( )
main = do
   theTimes <- fmap lines $ readFile "/proc/stat"
   putStr $ show $ computeUsage $ head theTimes
   putStrLn " %"
```


```txt
1.6090321637810434 %

```


=={{header|Icon}} and {{header|Unicon}}==

```unicon
#
# utilization.icn, percentage of running cpu time used versus idle time
#
# tectonics: unicon -s utilization.icn -x
#
procedure main()
    local stats, n, total, newtotal, delta, idle, idled
    stats := gather()
    idle := stats[4]
    total := newtotal := 0
    every n := !stats do total +:= n
    write("Overall utilization:  ", (1 - (real(idle) / total)) * 100, "%")

    idle := stats[4]
    delay(100)

    stats := gather()
    every n := !stats do newtotal +:= n
    delta := newtotal - total
    idled := stats[4] - idle
    write("Interval utilization: ", (1 - (real(idled) / delta)) * 100, "%")
end

procedure gather()
    local f, line, stats

    f := open("/proc/stat", "r") | stop("Cannot open /proc/stat")
    line := read(f)
    close(f)

    stats := []
    line ? {
         tab(upto(' ')) & tab(many(' '))
         while put(stats, tab(upto(' '))) do tab(many(' '))
         put(stats, tab(0))
    }
    return stats
end
```


```txt
prompt$ unicon -s utilization.icn -x
Overall utilization:  4.995469613984859%
Interval utilization: 10.52631578947368%
prompt$ ./utilization
Overall utilization:  4.994924759070784%
Interval utilization: 0.0%
prompt$ ./utilization
Overall utilization:  4.994135168922565%
Interval utilization: 4.878048780487809%
```




## J



```j
cputpct=:3 :0
  if. 0>nc<'PREVCPUTPCT' do. PREVCPUTPCT=:0 end.
  old=. PREVCPUTPCT
  PREVCPUTPCT=:0 { 0.1,~0&".;._2 fread '/proc/stat'
  100*1-(4&{ % +/) PREVCPUTPCT - old
)
```


Example use:


```j
   cputpct''
1.76237
```


Notes: this gives the average non-idle time since the last time this verb was used. If for some reason /proc/stat were not updated between calls, the result would be 100 (percent), which seems appropriate.


## Julia

```julia
function main()
    lastidle = lasttotal = 0
    while true
        ln = readline("/proc/stat")
        fields = parse.(Float64, split(ln)[2:end])
        idle, total = fields[4], sum(fields)
        Δidle, Δtotal = idle - lastidle, total - lasttotal
        lastidle, lasttotal = idle, total
        utilization = 100 * (1 - Δidle / Δtotal)
        @printf "%5.1f%%\r" utilization
        sleep(5)
    end
end

main()
```



## Kotlin


```scala
// version 1.1.3

import java.io.FileReader
import java.io.BufferedReader

fun main(args: Array<String>) {
    println("CPU usage % at 1 second intervals:\n")
    var prevIdleTime = 0L
    var prevTotalTime = 0L
    repeat(10) {
        val br = BufferedReader(FileReader("/proc/stat"))
        val firstLine = br.readLine().drop(5)  // get rid of cpu plus 2 spaces
        br.close()
        val split = firstLine.split(' ')
        val idleTime = split[3].toLong()
        val totalTime = split.map { it.toLong() }.sum()
        if (it > 0) {
            val deltaIdleTime  = idleTime  - prevIdleTime
            val deltaTotalTime = totalTime - prevTotalTime
            val cpuUsage = (1.0 - deltaIdleTime.toDouble() / deltaTotalTime) * 100.0
            println("$it : ${"%6.3f".format(cpuUsage)}")
        }
        prevIdleTime  = idleTime
        prevTotalTime = totalTime
        Thread.sleep(1000)
    }
}
```


Sample output:

```txt

CPU usage % at 1 second intervals:

1 : 13.043
2 :  8.491
3 :  4.478
4 :  3.553
5 :  2.500
6 :  4.975
7 :  1.015
8 :  4.040
9 :  2.525

```



## Perl


```perl
$last_total = 0;
$last_idle  = 0;

while () {
    @cpu = split /\s+/, `head -1 /proc/stat`;
    shift @cpu;
    $this_total  = 0;
    $this_total += $_ for @cpu;
    $delta_total = $this_total - $last_total;
    $this_idle   = $cpu[3]     - $last_idle;
    $delta_idle  = $this_idle  - $last_idle;
    $last_total  = $this_total;
    $last_idle   = $this_idle;
    printf "Utilization: %0.1f%%\n", 100 * (1 - $delta_idle / $delta_total);
    sleep 1;
}
```

```txt
Utilization: 38.6%
Utilization: 96.1%
Utilization: 83.3%
^C

```



## Perl 6


```perl6
my $last-total = 0;
my $last-idle  = 0;

loop {
    my $Δ-total = (my $this-total = [+] my @cpu = "/proc/stat".IO.lines[0].words[1..*]) - $last-total;
    my $Δ-idle  = (my $this-idle  = @cpu[3]) - $last-idle;
    $last-total = $this-total;
    $last-idle  = $this-idle;
    print "\b" x 40, (100 * (1 - $Δ-idle / $Δ-total)).fmt("Utilization: %0.1f%% ");
    sleep(1);
}
```



## Phix

```Phix
integer last_idle = 0, last_total = 0
while true do
    integer fn = open("/proc/stat","r")
    sequence line = split(trim(gets(fn)),no_empty:=true)[2..$]
    close(fn)
    for i=1 to length(line) do
        {{line[i]}} = scanf(line[i],"%d")
    end for
    integer idle = line[4], total = sum(line),
            idle_delta = idle - last_idle,
            total_delta = total - last_total
    last_idle = idle
    last_total = total
    atom utilisation = 100*(1-idle_delta/total_delta)
    printf(1,"%5.1f%%\r",{utilisation})
    sleep(1)
    if get_key()=#1B then exit end if
end while
```



## PicoLisp


```PicoLisp
(scl 8)
(let (Idl 0  Ttl 0)
   (loop
      (use (L I S)
         (in "/proc/stat"
            (read)
            (setq L (make (do 10 (link (read))))) )
         (setq I (get L 4)  S (sum prog L))
         (prinl
            (round
               (*/
                  100.0
                  (-
                     1.0
                     (*/
                        1.0
                        (- I (swap 'Idl I))
                        (- S (swap 'Ttl S)) ) )
                  1.0 )
               1 )
            '% )
         (wait 1000) ) ) )
```



## Python


```python
from __future__ import print_function
from time import sleep


last_idle = last_total = 0
while True:
    with open('/proc/stat') as f:
        fields = [float(column) for column in f.readline().strip().split()[1:]]
    idle, total = fields[3], sum(fields)
    idle_delta, total_delta = idle - last_idle, total - last_total
    last_idle, last_total = idle, total
    utilisation = 100.0 * (1.0 - idle_delta / total_delta)
    print('%5.1f%%' % utilisation, end='\r')
    sleep(5)
```


Lines end in \r which causes old values to be overwritten by new when \r is supported otherwise successive figures appear on separate lines.


```txt
 26.5%
 12.4%
 10.6%
 49.5%
 15.5%
 13.8%
  8.3%
 11.0%
 18.5%
 13.9%
 11.8%
 35.6%
```



## Racket



```racket
#lang racket/base

(require racket/string)

(define (get-stats) ; returns total and idle times as two values
  (define line (call-with-input-file* "/proc/stat" read-line))
  (define numbers (map string->number (cdr (string-split line))))
  (values (apply + numbers) (list-ref numbers 3)))

(define prev-stats #f)

(define (report-cpu-utilization)
  ;; lazy: fixed string instead of keeping the last time
  (define prompt (if prev-stats "last second" "since boot"))
  (define-values [cur-total cur-idle] (get-stats))
  (define prev (or prev-stats '(0 0)))
  (set! prev-stats (list cur-total cur-idle))
  (define total (- cur-total (car prev)))
  (define idle (- cur-idle (cadr prev)))
  (printf "Utilization (~a): ~a%\n" prompt
          (/ (round (* 10000 (- 1 (/ idle total)))) 100.0)))

(let loop ()
  (report-cpu-utilization)
  (sleep 1)
  (loop))

```



## REXX

This REXX version tests to see if the   <big> /proc/stat </big>   file was updated.

```rexx
/*REXX pgm displays current CPU utilization (as a percentage) as per file:  /proc/stat  */
numeric digits 20                                /*ensure enough decimal digits for pgm.*/
parse arg n wait iFID .                          /*obtain optional arguments from the CL*/
if    n=='' |    n=","  then    n= 10            /*Not specified?  Then use the default.*/
if wait=='' | wait=","  then wait=  1            /* "      "         "   "   "     "    */
if iFID=='' | iFID=","  then iFID= '/proc/stat'  /* "      "         "   "   "     "    */
prevTot  = 0;           prevIdle= 0              /*initialize the prevTot and prevIdle. */

  do j=1  for n                                  /*process CPU utilization   N   times. */
  parse value  linein(iFID, 1)  with  .  $       /*read the 1st line and ignore 1st word*/
  tot= 0                                         /*initialize  TOT (total time) to zero.*/
          do k=1  for words($);   @.k= word($,k) /*assign the times to an array (@).    */
          tot= tot + @.k                         /*add all the times from the 1st line. */
          end   /*k*/                            /*@.4  is the idle time for this cycle.*/

  div= tot - prevTot                             /*DIV may be zero if file isn't updated*/
  if div\=0  then say format(((1-(@.4-prevIdle)/div))*100,3,5)"%"    /*display CPU busy.*/
  prevTot= tot;    prevIdle= @.4                 /*set the previous  TOT  and  prevIdle.*/
  call sleep wait                                /*cause this pgm to sleep   WAIT  secs.*/
  end   /*j*/
                                                 /*stick a fork in it,  we're all done. */
```





## Tcl


This is coded a little bit awkwardly in order to correspond neatly to the task description.

<tt>proc stat</tt> fulfills the task description in two ways:

* if called normally, it returns aggregate CPU utilization since boot
* if called within a coroutine, it prints on stdout and repeats every 1s until the coroutine is terminated

A more reusable implementation might take a command as an argument to stat, and allow that command to interrupt the loop with <tt>return false</tt> or even <tt>break</tt>.


```Tcl
# forgive the pun!
proc stat {} {

    set fd [open /proc/stat r]
    set prevtotal 0
    set previdle 0

    while {1} {
        # read the first line of /proc/stat
        set line [gets $fd]
        seek $fd 0
        # discard the first word of that first line (it's always cpu)
        set line [lrange $line 1 end]
        # sum all of the times found on that first line to get the total time
        set total [::tcl::mathop::+ {*}$line]

        # parse each field out of line (we only need $idle)
        lassign $line user nice system idle iowait irq softirq steal guest guest_nice

        # update against previous measurement
        incr idle -$previdle
        incr total -$prevtotal
        incr previdle $idle
        incr prevtotal $total

        # divide the fourth column ("idle") by the total time, to get the fraction of time spent being idle
        set frac [expr {$idle * 1.0 / $total}]
        # subtract the previous fraction from 1.0 to get the time spent being not idle
        set frac [expr {1 - $frac}]
        # multiply by 100 to get a percentage
        set frac [expr {round($frac*100)}]

        if {[info coroutine] eq ""} {
            return $frac ;# if we're called outside a coroutine, return a number
        } else {
            puts [format CPU:%3d%% $frac]  ;# else print output
            yieldto after 1000 [info coroutine]
        }
    }
}

coroutine watchstat stat
```


```txt
CPU: 10%
CPU:  3%
CPU:  1%
```



## UNIX Shell

Example taken, verbatim, from [https://github.com/pcolby/scripts/blob/master/cpu.sh Github].

```bash
#!/bin/bash
# by Paul Colby (http://colby.id.au), no rights reserved ;)

PREV_TOTAL=0
PREV_IDLE=0

while true; do
  # Get the total CPU statistics, discarding the 'cpu ' prefix.
  CPU=(`sed -n 's/^cpu\s//p' /proc/stat`)
  IDLE=${CPU[3]} # Just the idle CPU time.

  # Calculate the total CPU time.
  TOTAL=0
  for VALUE in "${CPU[@]}"; do
    let "TOTAL=$TOTAL+$VALUE"
  done

  # Calculate the CPU usage since we last checked.
  let "DIFF_IDLE=$IDLE-$PREV_IDLE"
  let "DIFF_TOTAL=$TOTAL-$PREV_TOTAL"
  let "DIFF_USAGE=(1000*($DIFF_TOTAL-$DIFF_IDLE)/$DIFF_TOTAL+5)/10"
  echo -en "\rCPU: $DIFF_USAGE%  \b\b"

  # Remember the total and idle CPU times for the next check.
  PREV_TOTAL="$TOTAL"
  PREV_IDLE="$IDLE"

  # Wait before checking again.
  sleep 1
done
```

{{out}}Each successive output overwrites the previous output, so there is only ever one line, but that line keeps updating in-place.

```txt
CPU: 1%
```



## Ursa


```ursa
#
# linux cpu utilization
#

# define variables to hold last idle and last total
decl int last_idle last_total

# loop indefintely
while true
	# read the first line from /proc/stat
	decl string line
	decl file f
	f.open "/proc/stat"
	set line (in string f)

	# get the data from the first line
	decl int i
	decl int<> fields
	fields.clear
	for (set i 2) (< i (size (split (trim line) " "))) (inc i)
		append (int (split (trim line) " ")<i>) fields
	end for

	# get idle stat and total
	decl int idle total
	set idle fields<3>
	set total (int (+ fields))

	# set idle_delta and total_delta
	decl int idle_delta total_delta
	set idle_delta (int (- idle last_idle))
	set total_delta (int (- total last_total))

	# set last_idle and last_total
	set last_idle idle
	set last_total total

	# set percentage utilization
	decl double utilization
	set utilization (* 100 (- 1 (/ idle_delta total_delta)))

	# output percentage
	out utilization "%\r" console

	# close f, it will be reopened at the top of the loop
	f.close

	# sleep 2.5 seconds
	sleep 2500

	# clear the line of output
	for (set i 0)  (< i (+ (size (string utilization)) 1.0)) (inc i)
		out "\b" console
	end for
	for (set i 0) (< i (+ (size (string utilization)) 1.0)) (inc i)
		out " " console
	end for
	out "\r" console
end while
```



## zkl

```zkl
last_idle,last_total:=0,0;
while(True){
   f:=File("/proc/stat");  // open each time to get fresh data
      fields:=f.readln().strip().split()[1,*];
   f.close();
   idle, total := fields[3].toFloat(),fields.sum(0.0);
   idle_delta, total_delta := idle - last_idle, total - last_total;
   last_idle, last_total = idle, total;
   utilisation := 100.0 * (1.0 - idle_delta / total_delta);
   print("%5.1f%%\r".fmt(utilisation)); // stay on this line, overwrite old value
   Atomic.sleep(5);
}
```

Shown on multiple lines, in xterm output stays on one line, overwriting itself.

```txt

 33.3%
  1.2%
  1.5%
  1.1%
  1.6%
  1.1%
  1.8%
^CCntl C noted

```

