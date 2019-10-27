+++
title = "Text processing/Max licenses in use"
description = ""
date = 2018-08-25T13:37:44Z
aliases = []
[extra]
id = 3064
[taxonomies]
categories = []
tags = []
+++

{{task|Text processing}}

A company currently pays a fixed sum for the use of a particular licensed software package.   In determining if it has a good deal it decides to calculate its maximum use of the software from its license management log file.

Assume the software's licensing daemon faithfully records a checkout event when a copy of the software starts and a checkin event when the software finishes to its log file. 

An example of  checkout and  checkin events are:
  License OUT @ 2008/10/03_23:51:05 for job 4974
  ...
  License IN  @ 2008/10/04_00:18:22 for job 4974


;Task:
Save the 10,000 line log file from   [http://rosettacode.org/resources/mlijobs.txt <big> here</big>]   into a local file, then write a program to scan the file extracting both the maximum licenses that were out at any time, and the time(s) at which this occurs.

Mirror of log file available as a zip [https://github.com/thundergnat/rc/blob/master/resouces/mlijobs.zip here] (offsite mirror).





## Ada


```Ada
-- licenselist.adb --
-- run under GPS 4.3-5 (Sidux/Debian)
-- process rosetta.org text_processing/3 example
-- uses linked-list to hold times
with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Strings.Unbounded,
     Ada.Strings.Unbounded.Text_IO,
     Ada.Containers.Doubly_Linked_Lists;
use Ada.Text_IO, Ada.Integer_Text_IO,
    Ada.Strings.Unbounded, Ada.Strings.Unbounded.Text_IO,
    Ada.Containers;

procedure licenselist is

 type logrec is record  -- define a record 'logrec' to place in a list
    logtext : String(1..19);
 end record;

 package dblist is new Doubly_Linked_Lists(logrec);
 use dblist;
       -- declare dblist as a list of logrec's
 licenselog : list;
 logtime    : logrec;
  -- to record the time of max OUT licenses

 infile	: File_Type;		   -- file handle
 str	: Unbounded_String;        -- input string buffer of unknown length
 outcnt, maxoutcnt : integer := 0;
 infilename : string := "license.log";

  procedure trace_times  is
  -- loop thru times list and print
     pntr : cursor := licenselog.first;
  -- pntr is of system type cursor reference to local list 'licenselog'
  begin
       new_line;
       while has_element(pntr) loop
        put(element(pntr).logtext); new_line;
        next(pntr);
       end loop;
  end trace_times;

begin -- main program --
   open ( infile,
         mode=> in_file,
         name=> infilename );

  loop
    exit when End_of_file ( infile );
    str := get_line( infile );
    if index( str, "OUT" ) > 0 then -- test if OUT record
      outcnt := outcnt +1;
    else                            -- else assume IN record
      outcnt := outcnt -1;
    end if;
    if outcnt > maxoutcnt then
         maxoutcnt := outcnt;
	 logtime.logtext := slice(str,15,33); -- date_time field
         licenselog.clear;             -- reset list for new time(s)
         licenselog.append (logtime);  -- put current time into list
    elsif outcnt = maxoutcnt then
         logtime.logtext := slice(str,15,33);  -- date_time field
         licenselog.append (logtime);   -- add current time into list
    end if;  -- have to account for possibility of equal number of OUT's
  end loop;
   put("The max. number of licenses OUT is ");put(maxoutcnt,5); new_line;
   put(" at these times ");

  trace_times;
  close ( infile );
end licenselist;
```

Output:

```txt

The max. number of licenses out is    99
 at these times 
2008/10/03_08:39:34
2008/10/03_08:40:40
[2010-06-06 01:06:07] process terminated successfully (elapsed time: 00.25s)

```


## ALGOL 68

{{trans|C}} note: This specimen retains the original [http://rosettacode.org/mw/index.php?title=Text_processing%2F3&diff=87014&oldid=87011 C] coding style.
{{wont work with|ALGOL 68|Revision 1 - non-standard ''argc'' and ''argv'' routines}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of FORMATted transput}}

```algol68
PROC report = (REF FILE file in)INT: (

  MODE TIME = [19]CHAR;
  STRUCT ([3]CHAR inout, TIME time, INT jobnum) record;
  FORMAT record fmt = $"License "g" @ "g" for job "g(0)l$;

  FLEX[1]TIME max time;

  INT lic out := 0, max out := LWB max time-1, max count := LWB max time-1;
  BOOL file in ended := FALSE;
  on logical file end(file in, (REF FILE file in)BOOL: file in ended := TRUE);
  WHILE
    getf(file in, (record fmt, record));
# WHILE # NOT file in ended DO
    IF inout OF record = "OUT" THEN
      lic out +:= 1
    ELIF lic out > 0 THEN # incase license already "OUT" #
      lic out -:= 1
    FI;

    IF lic out > max out THEN
      max out := lic out;
      max count := LWB max time-1
    FI;
    IF lic out = max out THEN
      IF max count = UPB max time THEN
        [UPB max time*2]TIME new max time;
        new max time[:UPB max time] := max time;
        max time := new max time
        # ;putf(stand error, ($"increasing UPB max time (now it is "g(0)")"l$, UPB max time)); #
      FI;
      max time[max count +:= 1] := time OF record
    FI
  OD;

  printf(($"Maximum simultaneous license use is "g(0)" at the following times:"l$, max out));
  FOR lic out FROM LWB max time TO max count DO
    printf(($gl$, max time[lic out]))
  OD;

  0 EXIT
  exit report error: errno
);

INT errno;

COMMENT
  Usage:
    a68g Text_processing_3.a68 --exit Text_processing_3.dat
    a68g Text_processing_3.a68 < Text_processing_3.dat
END COMMENT

main:
(
  INT argv1 := 4;
  IF argc >= argv1 THEN
    FOR i FROM argv1 TO argc DO
      FILE file in;
      errno := open(file in, argv(i), stand in channel);
      IF errno /= 0 THEN
        putf(stand error, ($"cannot read "gl$, argv(1)));
        exit main error
      ELSE
        report(file in)
      FI;
      close(file in)
    OD
  ELSE
    report(stand in)
  FI;
  exit main error: SKIP
)
```

Output:

```txt

Maximum simultaneous license use is 99 at the following times:
2008/10/03_08:39:34
2008/10/03_08:40:40

```



## APL

{{works with|APL2}}{{trans|J}}

```apl
⍝  Copy/paste file's contents into TXT (easiest), or TXT ← ⎕NREAD
       I  ←  TXT[;8+⎕IO]
       D  ←  TXT[;⎕IO+14+⍳19]
       lu ←  +\ ¯1 * 'OI' ⍳ I
       mx ←  (⎕IO+⍳⍴lu)/⍨lu= max ← ⌈/ lu
       ⎕  ←  'Maximum simultaneous license use is ' , ' at the following times:' ,⍨ ⍕max ⋄ ⎕←D[mx;]
```


```apl
Maximum simultaneous license use is 99 at the following times:
 2008/10/03_08:39:34
 2008/10/03_08:40:40
```


=={{Header|AutoHotkey}}==
{{trans|Python}}

```autohotkey

IfNotExist, mlijobs.txt
  UrlDownloadToFile, http://rosettacode.org/mlijobs.txt, mlijobs.txt

out := 0, max_out := -1, max_times := ""

Loop, Read, mlijobs.txt
{
  If InStr(A_LoopReadLine, "OUT")
    out++
  Else
    out--
  If (out > max_out)
    max_out := out, max_times := ""
  If (out = max_out)
  {
    StringSplit, lineArr, A_LoopReadLine, %A_Space%
    max_times .= lineArr4 . "`n"
  }
}

MsgBox Maximum use is %max_out% at:`n`n%max_times%

```


```txt

Maximum use is 99 at:

2008/10/03_08:39:34
2008/10/03_08:40:40

```



## AWK

to be called with ''awk -f licenses.awk ./mlijobs.txt''

```awk
$2=="OUT" { 
    count = count + 1 
    time = $4 
    if ( count > maxcount ) {
        maxcount = count 
        maxtimes = time
    } else { 
        if ( count == maxcount ) {
            maxtimes = maxtimes " and " time 
        }
    }
}
$2=="IN" { count = count - 1 }
END {print "The biggest number of licenses is " maxcount " at " maxtimes " !"}
```


'''Sample output'''

```txt
The biggest number of licenses is 99 at 2008/10/03_08:39:34 and 2008/10/03_08:40:40 !
```


On a 2.53MHz machine, these timings were obtained using GNU Awk 4.0.2:

 user	0m0.015s
 sys	0m0.008s


## BBC BASIC


```bbcbasic
      max% = 0
      nlicence% = 0
      file% = OPENIN("mlijobs.txt")
      
      WHILE NOT EOF#file%
        a$ = GET$#file%
        stamp$ = MID$(a$, 15, 19)
        IF INSTR(a$, "OUT") THEN
          nlicence% += 1
          IF nlicence% > max% THEN
            max% = nlicence%
            start$ = stamp$
          ENDIF
        ENDIF
        IF INSTR(a$, "IN") THEN
          IF nlicence% = max% THEN
            finish$ = previous$
          ENDIF
          nlicence% -= 1
        ENDIF
        previous$ = stamp$
      ENDWHILE
      CLOSE #file%
      
      PRINT "Maximum licences checked out = " ; max%
      PRINT "From " start$ " to " finish$
      END
```

'''Output:'''

```txt

Maximum licences checked out = 99
From 2008/10/03_08:39:34 to 2008/10/03_08:40:40

```



## Bracmat



```bracmat
(   0:?N:?n
  & :?Ts
  & @( get$("mlijobs.txt",STR)
     :   ?
         ( "e " ?OI " @ " ?T " " ?
         &   (   !OI:OUT
               &   !n+1
                 : ( >!N:?N&!T:?Ts
                   | !N&!Ts !T:?Ts
                   | ?
                   )
             | !n+-1
             )
           : ?n
         & ~
         )
     )
| out$(!N !Ts)
);

```

Output:

```txt
99 2008/10/03_08:39:34 2008/10/03_08:40:40
```



## C



```c>#include <stdio.h

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#define INOUT_LEN 4
#define TIME_LEN 20
#define MAX_MAXOUT 1000

char inout[INOUT_LEN];
char time[TIME_LEN];
uint jobnum;

char maxtime[MAX_MAXOUT][TIME_LEN];

int main(int argc, char **argv)
{
  FILE *in = NULL;
  int l_out = 0, maxout=-1, maxcount=0;

  if ( argc > 1 ) {
    in = fopen(argv[1], "r");
    if ( in == NULL ) {
      fprintf(stderr, "cannot read %s\n", argv[1]);
      exit(1);
    }
  } else {
    in = stdin;
  }

  while( fscanf(in, "License %s @ %s for job %u\n", inout, time, &jobnum) != EOF ) {

    if ( strcmp(inout, "OUT") == 0 )
      l_out++;
    else
      l_out--;

    if ( l_out > maxout ) {
      maxout = l_out;
      maxcount=0; maxtime[0][0] = '\0';
    }
    if ( l_out == maxout ) {
      if ( maxcount < MAX_MAXOUT ) {
	strncpy(maxtime[maxcount], time, TIME_LEN);
	maxcount++;
      } else {
	fprintf(stderr, "increase MAX_MAXOUT (now it is %u)\n", MAX_MAXOUT);
	exit(1);
      }
    }
  }

  printf("Maximum simultaneous license use is %d at the following times:\n", maxout);
  for(l_out=0; l_out < maxcount; l_out++) {
    printf("%s\n", maxtime[l_out]);
  }

  if ( in != stdin ) fclose(in);
  exit(0);
}
```


Using mmap, no extra storage (think this as a search and replace):
```c>#include <stdio.h

#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <err.h>
#include <string.h>

int main()
{
	struct stat s;
	int fd = open("mlijobs.txt", O_RDONLY);
	int cnt, max_cnt, occur;
	char *buf, *ptr;

	if (fd == -1) err(1, "open");
	fstat(fd, &s);
	ptr = buf = mmap(0, s.st_size, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);

	cnt = max_cnt = 0;
	while(ptr - buf < s.st_size - 33) {
		if (!strncmp(ptr, "License OUT", 11) && ++cnt >= max_cnt) {
			if (cnt > max_cnt) {
				max_cnt = cnt;
				occur = 0;
			}
			/* can't sprintf time stamp: might overlap */
			memmove(buf + 26 * occur, ptr + 14, 19);
			sprintf(buf + 26 * occur + 19, "%6d\n", cnt);
			occur++;
		} else if (!strncmp(ptr, "License IN ", 11)) cnt --;

		while (ptr < buf + s.st_size && *ptr++ != '\n');
	}

	printf(buf);
	munmap(buf, s.st_size);
	return close(fd);
}
```
output<lang>2008/10/03_08:39:34    99
2008/10/03_08:40:40    99
```



## C++


```cpp>#include <fstream

#include <iostream>
#include <iterator>
#include <string>
#include <vector>

int main()
{
    const char logfilename[] = "mlijobs.txt";
    std::ifstream logfile(logfilename);

    if (!logfile.is_open())
    {
        std::cerr << "Error opening: " << logfilename << "\n";
        return -1;
    }

    int license = 0, max_license = 0;
    std::vector<std::string> max_timestamp;

    for (std::string logline; std::getline(logfile, logline); )
    {
        std::string action(logline.substr(8,3));

        if (action == "OUT")
        {
            if (++license >= max_license)
            {
                if (license > max_license)
                {
                    max_license = license;
                    max_timestamp.clear();
                }
                max_timestamp.push_back(logline.substr(14, 19));
            }
        }
        else if (action == "IN ")
        {
            --license;
        }
    }

    std::cout << "License count at log end: " << license
        << "\nMaximum simultaneous license: " << max_license
        << "\nMaximum license time(s):\n";

    std::copy(max_timestamp.begin(), max_timestamp.end(),
        std::ostream_iterator<std::string>(std::cout, "\n"));
}
```

Output:
```txt
License count at log end: 0
Maximum simultaneous licenses: 99
Maximum license time(s):
2008/10/03_08:39:34
2008/10/03_08:40:40
```


=={{header|C sharp|C#}}==

```csharp

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace TextProc3
{
    class Program
    {
        static void Main(string[] args)
        {            
            string line;
            int count = 0, maxcount = 0;
            List<string> times = new List<string>();
            System.IO.StreamReader file = new StreamReader("mlijobs.txt");
            while ((line = file.ReadLine()) != null)
            {
                string[] lineelements = line.Split(' ');                
                switch (lineelements[1])
                {
                    case "IN":
                        count--;
                        break;
                    case "OUT":
                        count++;
                        if (count > maxcount)
                        {
                            maxcount = count;
                            times.Clear();
                            times.Add(lineelements[3]);
                        }else if(count == maxcount){
                            times.Add(lineelements[3]);
                        }
                        break;
                }                
            }
            file.Close();
            Console.WriteLine(maxcount);
            foreach (string time in times)
            {
                Console.WriteLine(time);
            }
        }
    }
}

```


```txt

99
2008/10/03_08:39:34
2008/10/03_08:40:40

```



## Clojure



```clojure
(defn delta [entry]
  (case (second (re-find #"\ (.*)\ @" entry))
    "IN " -1
    "OUT" 1
    (throw (Exception. (str "Invalid entry:" entry)))))

(defn t [entry]
  (second (re-find #"@\ (.*)\ f" entry)))

(let [entries (clojure.string/split (slurp "mlijobs.txt") #"\n")
      in-use (reductions + (map delta entries))
      m (apply max in-use)
      times (map #(nth (map t entries) %)
                 (keep-indexed #(when (= m %2) %1) in-use))]
  (println "Maximum simultaneous license use is" m "at the following times:")
  (map println times))
```



```txt

Maximum simultaneous license use is 99 at the following times:
2008/10/03_08:39:34
2008/10/03_08:40:40

```



## COBOL

{{works with|OpenCOBOL}}

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. max-licenses-in-use.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT license-file ASSIGN "mlijobs.txt"
               ORGANIZATION LINE SEQUENTIAL
               FILE STATUS file-status.

       DATA DIVISION.
       FILE SECTION.
       FD  license-file.
       01  license-record.
           03  FILLER                   PIC X(8).
           03  action                   PIC X(3).
               88  license-out          VALUE "OUT".
           03  FILLER                   PIC X(3).
           03  license-timestamp        PIC X(19).
           03  FILLER                   PIC X(13).

       WORKING-STORAGE SECTION.
       01  file-status                  PIC XX.
           88  file-ok                  VALUE "00".

       01  max-licenses-out             PIC 9(6).
       01  num-max-times                PIC 99.
       01  max-license-times-area.
           03  max-timestamps           PIC X(19) OCCURS 1 TO 50 TIMES
               DEPENDING ON num-max-times.
       01  current-licenses-out         PIC 9(6).
       
       01  i                            PIC 99.

       PROCEDURE DIVISION.
       DECLARATIVES.
       license-file-error SECTION.
           USE AFTER ERROR ON license-file.

           DISPLAY "An unexpected error has occurred. Error "
               file-status ". The program will close."
           GOBACK
           .
       END DECLARATIVES.

       main-line.
           OPEN INPUT license-file
           IF NOT file-ok
               DISPLAY "File could not be opened. Error " file-status
                   "."
               GOBACK
           END-IF
           
           PERFORM FOREVER
               READ license-file
                   AT END
                       EXIT PERFORM
               END-READ

               IF license-out
                   ADD 1 TO current-licenses-out

                   EVALUATE TRUE
                       WHEN current-licenses-out > max-licenses-out
                           MOVE 1 TO num-max-times
                           MOVE current-licenses-out TO max-licenses-out
                           MOVE license-timestamp
                               TO max-timestamps (num-max-times)

                       WHEN current-licenses-out = max-licenses-out
                           ADD 1 TO num-max-times
                           MOVE license-timestamp
                               TO max-timestamps (num-max-times)
                   END-EVALUATE
               ELSE
                   SUBTRACT 1 FROM current-licenses-out
               END-IF
           END-PERFORM

           CLOSE license-file

           DISPLAY "License count at log end: " current-licenses-out
           DISPLAY "Maximum simulataneous licenses: " max-licenses-out
           DISPLAY "Time(s):"
           PERFORM VARYING i FROM 1 BY 1 UNTIL num-max-times < i
               DISPLAY max-timestamps (i)
           END-PERFORM

           GOBACK
           .
```



## Common Lisp


{{libheader|CL-PPCRE}}


```lisp
(defun max-licenses (&optional (logfile "mlijobs.txt"))
  (with-open-file (log logfile :direction :input)
    (do ((current-logs 0) (max-logs 0) (max-log-times '())
         (line #1=(read-line log nil nil) #1#))
        ((null line)
         (format t "~&Maximum simultaneous license use is ~w at the ~
                     following time~p: ~{~%  ~a~}."
                 max-logs (length max-log-times) (nreverse max-log-times)))
      (cl-ppcre:register-groups-bind (op time)
          ("License (\\b.*\\b)[ ]{1,2}@ (\\b.*\\b)" line)
        (cond ((string= "OUT" op) (incf current-logs))
              ((string= "IN"  op) (decf current-logs))
              (t (cerror "Ignore it." "Malformed entry ~s." line)))
        (cond ((> current-logs max-logs)
               (setf max-logs current-logs
                     max-log-times (list time)))
              ((= current-logs max-logs)
               (push time max-log-times)))))))
```



```txt
> (max-licenses)
Maximum simultaneous license use is 99 at the following times: 
  2008/10/03_08:39:34
  2008/10/03_08:40:40.
NIL
```



## D


```d
void main() {
    import std.stdio;
    int nOut, maxOut = -1;
    string[] maxTimes;

    foreach (string job; lines(File("mlijobs.txt"))) {
        nOut += (job[8] == 'O') ? 1 : -1;
        if (nOut > maxOut) {
            maxOut = nOut;
            maxTimes = null;
        }
        if (nOut == maxOut)
            maxTimes ~= job[14 .. 33];
    }

    writefln("Maximum simultaneous license use is %d at" ~
             " the following times:\n%( %s\n%)", maxOut, maxTimes);
}
```

{{out}}

```txt
Maximum simultaneous license use is 99 at the following times:
 "2008/10/03_08:39:34"
 "2008/10/03_08:40:40"
```



## E


{{trans|Python}}


```e
var out := 0
var maxOut := 0
var maxTimes := []

def events := ["OUT " => 1, "IN  " => -1]

for line in <file:mlijobs.txt> {
    def `License @{via (events.fetch) delta}@@ @time for job @num$\n` := line

    out += delta
    if (out > maxOut) {
        maxOut := out
        maxTimes := []
    }
    if (out == maxOut) {
        maxTimes with= time
    }
}

println(`Maximum simultaneous license use is $maxOut at the following times:`)
for time in maxTimes {
    println(` $time`)
}
```


## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature

	make
			-- Max Licences used.
		local
			count: INTEGER
			max_count: INTEGER
			date: STRING
		do
			read_list
			create date.make_empty
			across
				data as d
			loop
				if d.item.has_substring ("OUT") then
					count := count + 1
					if count > max_count then
						max_count := count
						date := d.item
					end
				elseif d.item.has_substring ("IN") then
					count := count - 1
				end
			end
			io.put_string ("Max Licences OUT: " + max_count.out)
			io.new_line
			io.put_string ("Date: " + date.substring (15, 33))
		end

	original_list: STRING = "mlijobs.txt"

feature {NONE}

	read_list
			-- Data read into 'data.
		local
			l_file: PLAIN_TEXT_FILE
		do
			create l_file.make_open_read_write (original_list)
			l_file.read_stream (l_file.count)
			data := l_file.last_string.split ('%N')
			l_file.close
		end

	data: LIST [STRING]

end

```

{{out}}

```txt

Max Licences OUT: 99
Date: 2008/10/03_08:39:34

```



## Erlang


```Erlang

-module( text_processing_max_licenses ).

-export( [out_dates_from_file/1, task/0] ).

out_dates_from_file( Name ) ->
	{ok, Binary} = file:read_file( Name ),
	Lines =	binary:split( Binary, <<"\n">>, [global] ),
	{_N, _Date, Dict} = lists:foldl( fun out_dates/2, {0, "", dict:new()}, Lines ),
	[{X, dict:fetch(X, Dict)} || X <- dict:fetch_keys( Dict )].

task() ->
    [{Max, Dates} | _T] = lists:reverse(lists:sort(out_dates_from_file("mlijobs.txt")) ),
    io:fwrite( "Max licenses was ~p at ~p~n", [Max, Dates] ).



out_dates( <<>>, Acc ) -> Acc;
out_dates( Line, {N, Date, Dict} ) ->
	[_License, Direction, <<"@">>, New_date | _T] = [X || X <- binary:split(Line, <<" ">>, [global]), X =/= <<>>],
	New_n = out_dates_n( N, Direction ),
	New_dict = out_dates_dict( N, New_n, Date, Dict ),
	{New_n, New_date, New_dict}.

out_dates_dict( N, New_n, Date, Dict ) when N > New_n -> dict:append( N, Date, Dict );
out_dates_dict( _N, _New_n, _Date, Dict ) -> Dict. 

out_dates_n( N, <<"OUT">> ) -> N + 1;
out_dates_n( N, <<"IN">> ) -> N - 1.

```

{{out}}

```txt

12> text_processing_max_licenses:task().
Max licenses was 99 at [<<"2008/10/03_08:39:34">>,<<"2008/10/03_08:40:40">>]

```



## Euphoria


```euphoria
function split(sequence s, integer c)
    sequence out
    integer first, delim
    out = {}
    first = 1
    while first <= length(s) do
        delim = find_from(c, s, first)
        if delim = 0 then
            delim = length(s) + 1
        end if
        out = append(out, s[first..delim-1])
        first = delim + 1
    end while
    return out
end function

include get.e
function val(sequence s)
    sequence v
    v = value(s)
    return v[2] - v[1] * v[1]
end function

constant fn = open("mlijobs.txt", "r")
integer maxout
sequence jobs, line, maxtime
object x
jobs = {}
maxout = 0
while 1 do
    x = gets(fn)
    if atom(x) then
        exit
    end if
    line = split(x, ' ')
    line[$] = val(line[$])
    if equal(line[2], "OUT") then
        jobs &= line[$]
        if length(jobs) > maxout then
            maxout = length(jobs)
            maxtime = {line[4]}
        elsif length(jobs) = maxout then
            maxtime = append(maxtime, line[4])
        end if
    else
        jobs[find(line[$],jobs)] = jobs[$]
        jobs = jobs[1..$-1]
    end if
end while
close(fn)

printf(1, "Maximum simultaneous license use is %d at the following times:\n", maxout)
for i = 1 to length(maxtime) do
    printf(1, "%s\n", {maxtime[i]})
end for
```


Output:

```txt
Maximum simultaneous license use is 99 at the following times:
2008/10/03_08:39:34
2008/10/03_08:40:40

```



## Factor

Placing the file in resource:work/mlijobs.txt:


```factor
USING: kernel sequences splitting math accessors io.encodings.ascii
io.files math.parser io ;
IN: maxlicenses

TUPLE: maxlicense max-count current-count times ;

<PRIVATE

: <maxlicense> ( -- max ) -1 0 V{ } clone \ maxlicense boa ; inline

: out? ( line -- ? ) [ "OUT" ] dip subseq? ; inline

: line-time ( line -- time ) " " split harvest fourth ; inline

: update-max-count ( max -- max' )
    dup [ current-count>> ] [ max-count>> ] bi >
    [ dup current-count>> >>max-count V{ } clone >>times ] when ;

: (inc-current-count) ( max ? -- max' )
    [ [ 1 + ] change-current-count ]
    [ [ 1 - ] change-current-count ]
    if
    update-max-count ; inline

: inc-current-count ( max ? time -- max' time )
    [ (inc-current-count) ] dip ;

: current-max-equal? ( max -- max ? )
    dup [ current-count>> ] [ max-count>> ] bi = ;

: update-time ( max time -- max' )
    [ current-max-equal? ] dip
    swap
    [ [ suffix ] curry change-times ] [ drop ] if ;
    
: split-line ( line -- ? time ) [ out? ] [ line-time ] bi ;

: process ( max line -- max ) split-line inc-current-count update-time ;

PRIVATE>

: find-max-licenses ( -- max )
    "resource:work/mlijobs.txt" ascii file-lines
    <maxlicense> [ process ] reduce ;

: print-max-licenses ( max -- )
    [ times>> ] [ max-count>> ] bi
    "Maximum simultaneous license use is " write
    number>string write
    " at the following times: " print
    [ print ] each ;
```



```factor
( scratchpad ) [ find-max-licenses print-max-licenses ] time
Maximum simultaneous license use is 99 at the following times: 
2008/10/03_08:39:34
2008/10/03_08:40:40
Running time: 0.16164423 seconds
```



## Forth


```forth

20 constant date-size
create max-dates date-size 100 * allot
variable max-out
variable counter

stdin value input

: process ( addr len -- )
  8 /string
  over 3 s" OUT" compare 0= if
    1 counter +!
    counter @ max-out @ > if
      counter @ max-out !
      drop 5 + date-size max-dates  place
    else counter @ max-out @ = if
      drop 5 + date-size max-dates +place
    else 2drop then then
  else drop 2 s" IN" compare 0= if
    -1 counter +!
  then then ;

: main
  0 max-out !
  0 counter !
  s" mlijobs.txt" r/o open-file throw to input
  begin  pad 80 input read-line throw
  while  pad swap process
  repeat drop
  input close-file throw
  max-out @ . ." max licenses in use @"
  max-dates count type cr ;

main bye

```


== {{header|Fortran}} ==
{{Works with|Fortran|90 and later}}


```fortran

 PROGRAM MAX_LICENSES
   IMPLICIT NONE
 
   INTEGER :: out=0, maxout=0, maxcount=0, err
   CHARACTER(50) :: line
   CHARACTER(19) :: maxtime(100)
 
   OPEN (UNIT=5, FILE="Licenses.txt", STATUS="OLD", IOSTAT=err)
   IF (err > 0) THEN
     WRITE(*,*) "Error opening file Licenses.txt"
     STOP
   END IF
 
   DO 
     READ(5, "(A)", IOSTAT=err) line
     IF (err == -1) EXIT          ! EOF detected
     IF (line(9:9) == "O") THEN
       out = out + 1
     ELSE IF (line(9:9) == "I") THEN
       out = out - 1
     END IF
     IF (out > maxout ) THEN
       maxout = maxout + 1
       maxcount = 1
       maxtime(maxcount) = line(15:33)
     ELSE IF (out == maxout) THEN
       maxcount = maxcount + 1
       maxtime(maxcount) = line(15:33)
     END IF
   END DO
  
   CLOSE(5)
  
   WRITE(*,"(A,I4,A)") "Maximum simultaneous license use is", maxout, " at the following times:"
   WRITE(*,"(A)") maxtime(1:maxcount)
  
 END PROGRAM MAX_LICENSES

```

Output

```txt

 Maximum simultaneous license use is  99 at the following times:
 2008/10/03_08:39:34                                           
 2008/10/03_08:40:40

```


## Gema

Start with ''gema -f licenses.gema mlijobs.txt''

```gema

@set{count;0};@set{max;0}

License OUT \@ * *\n=@incr{count}@testmax{${count},*}
License IN  \@ * *\n=@decr{count}
\Z=@report{${max},${times${max}}}

testmax:*,*=@cmpn{${max};$1;@set{max;$1};;}@append{times${count};$2\n}

report:*,*=Maximum simultaneous license use is * at\n*

```

Output:

```txt

Maximum simultaneous license use is 99 at
2008/10/03_08:39:34
2008/10/03_08:40:40

```



## Go


```go
package main

import (
	"bufio"
	"bytes"
	"fmt"
	"log"
	"os"
)

const (
	filename   = "mlijobs.txt"
	inoutField = 1
	timeField  = 3
	numFields  = 7
)

func main() {
	file, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()
	var ml, out int
	var mlTimes []string
	in := []byte("IN")
	s := bufio.NewScanner(file)
	for s.Scan() {
		f := bytes.Fields(s.Bytes())
		if len(f) != numFields {
			log.Fatal("unexpected format,", len(f), "fields.")
		}
		if bytes.Equal(f[inoutField], in) {
			out--
			if out < 0 {
				log.Fatalf("negative license use at %s", f[timeField])
			}
			continue
		}
		out++
		if out < ml {
			continue
		}

		if out > ml {
			ml = out
			mlTimes = mlTimes[:0]
		}
		mlTimes = append(mlTimes, string(f[timeField]))
	}
	if err = s.Err(); err != nil {
		log.Fatal(err)
	}

	fmt.Println("max licenses:", ml)
	fmt.Println("at:")
	for _, t := range mlTimes {
		fmt.Println(" ", t)
	}
}
```

{{out}}

```txt

max licenses: 99
at:
  2008/10/03_08:39:34
  2008/10/03_08:40:40

```



## Groovy


```groovy
def max = 0
def dates = []
def licenses = [:]
new File('licenseFile.txt').eachLine { line ->
    (line =~ /License (\w+)\s+@ ([\d\/_:]+) for job (\d+)/).each { matcher, action, date, job ->
        switch (action) {
        case 'IN':
            assert licenses[job] != null : "License has not been checked out for $job"
            licenses.remove job
            break
        case 'OUT':
            assert licenses[job] == null : "License has already been checked out for $job"
            licenses[job] = date
            def count = licenses.keySet().size()
            if (count > max) {
                max = count
                dates = [ date ]
            } else if (count == max) {
                dates << date
            }
            break
        default:
            throw new IllegalArgumentException("Unsupported license action $action")
        }
    }
}

println "Maximum Licenses $max"
dates.each { date -> println "  $date" }

```

Output:

```txt

Maximum Licenses 99
  2008/10/03_08:39:34
  2008/10/03_08:40:40

```


== {{header|Haskell}} ==

```haskell

import Data.List

main = do
  f <- readFile "./../Puzzels/Rosetta/inout.txt"
  let (ioo,dt) = unzip. map ((\(_:io:_:t:_)-> (io,t)). words) . lines $ f
      cio = drop 1 . scanl (\c io -> if io == "IN" then pred c else succ c) 0 $ ioo 
      mo = maximum cio
  putStrLn $ "Maximum simultaneous license use is " ++ show mo ++ " at:"
  mapM_ (putStrLn . (dt!!)) . elemIndices mo $ cio

```



## HicEst

{{incorrect|HicEst}}
We open Licenses.txt in [http://www.HicEst.com/MatrixExplorer.htm MatrixExplorer mode] with 3 columns: IN/OUT, date_time, ID_nr. 
This allows to adress single file elements by Licenses(row, column).

```HicEst
CHARACTER Licenses="Licenses.txt"
REAL :: counts(1), Top10(10)

OPEN(FIle=Licenses, fmt='8x,A3,3x,A19,Nb ,', LENgth=lines)

ALLOCATE(counts, lines)
counts(1) = 1
DO line = 2, lines
   counts(line) = counts(line-1) + 1 - 2*(Licenses(line,1)=='IN')
ENDDO

SORT(Vector=counts, Descending=1, Index=Top10)

DO i = 1, LEN(Top10)
  WRITE() counts(Top10(i)), Licenses(Top10(i), 2)
ENDDO

END
```


```txt
99 2008/10/03_08:40:40
99 2008/10/03_08:39:34
98 2008/10/03_08:40:47
98 2008/10/03_08:40:11
98 2008/10/03_08:39:46
98 2008/10/03_08:39:45
98 2008/10/03_08:39:30
97 2008/10/03_20:44:58
97 2008/10/03_08:41:36
97 2008/10/03_08:40:53
```


=={{header|Icon}} and {{header|Unicon}}==

The following solution works in both languages:

```unicon
procedure main(A)
    maxCount := count := 0

    every !&input ?  case tab(upto('@')) of {
        "License OUT ": {
             maxTime := (maxCount <:= (count +:= 1), [])
             put(maxTime, (maxCount = count, ="@ ", tab(find(" for "))))
             }
        "License IN  ": count -:= (count > 0, 1)     # Error check
        }

    write("There were ",maxCount," licenses out at:")
    every write("\t",!maxTime)
end
```


And a run of the program:

```txt
->ml <mlijobs.txt
There were 99 licenses out at:
        2008/10/03_08:39:34
        2008/10/03_08:40:40
->
```



== {{header|J}} ==

```j
   require 'files'
   'I D' =: (8 ; 14+i.19) {"1 &.> <'m' fread 'licenses.txt' NB.  read file as matrix, select columns
   lu    =:  +/\ _1 ^ 'OI' i. I                             NB.  Number of licenses in use at any given time
   mx    =:  (I.@:= >./) lu                                 NB.  Indicies of maxima

   NB.  Output results
   (mx { D) ,~ 'Maximum simultaneous license use is ' , ' at the following times:' ,~ ": {. ,mx { lu
```


```txt

 Maximum simultaneous license use is 99 at the following times:
 2008/10/03_08:39:34                                           
 2008/10/03_08:40:40

```



## Java

{{works with|Java|1.5+}}

```java5
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.LinkedList;

public class License {
  public static void main(String[] args) throws FileNotFoundException, IOException{
    BufferedReader in = new BufferedReader(new FileReader(args[0]));
    int max = Integer.MIN_VALUE;
    LinkedList<String> dates = new LinkedList<String>();
    String line;
    int count = 0;
    while((line = in.readLine()) != null){
      if(line.startsWith("License OUT ")) count++;
      if(line.startsWith("License IN ")) count--;
      if(count > max){
        max = count;
        String date = line.split(" ")[3];
        dates.clear();
        dates.add(date);
      }else if(count == max){
        String date = line.split(" ")[3];
        dates.add(date);
      }
    }
    System.out.println("Max licenses out: "+max);
    System.out.println("At time(s): "+dates);
  }
}
```


```txt
Max licenses out: 99
At time(s): [2008/10/03_08:39:34, 2008/10/03_08:40:40]
```



## JavaScript

{{works with|JScript}} for the file i/o

```javascript
var file_system = new ActiveXObject("Scripting.FileSystemObject");
var fh = file_system.openTextFile('mlijobs.txt', 1); // 1 == open for reading
var in_use = 0, max_in_use = -1, max_in_use_at = [];

while ( ! fh.atEndOfStream) {
    var line = fh.readline();
    if (line.substr(8,3) == "OUT") {
        in_use++;
        if (in_use > max_in_use) {
            max_in_use = in_use;
            max_in_use_at = [ line.split(' ')[3] ];
        }
        else if (in_use == max_in_use)
            max_in_use_at.push( line.split(' ')[3] );
    }
    else if (line.substr(8,2) == "IN") 
        in_use--;
}

fh.close();

WScript.echo("Max licenses out: " + max_in_use + "\n  " + max_in_use_at.join('\n  '));
```


output: 

```txt
Max licenses out: 99
  2008/10/03_08:39:34
  2008/10/03_08:40:40
```


## jq

{{works with|jq|1.4}}

```jq
# Input: an array of strings
def max_licenses_in_use:
  # state: [in_use = 0, max_in_use = -1, max_in_use_at = [] ]
  reduce .[] as $line
    ([0,  -1, [] ];
     ($line|split(" ")) as $line
     | if $line[1] == "OUT" then 
         .[0] += 1                          # in_use++;
         | if   .[0] > .[1]                 # (in_use > max_in_use)
           then .[1] = .[0]                 # max_in_use = in_use
             |  .[2] = [$line[3]]           # max_in_use_at = [$line[3]]
           elif .[0] == .[1]                # (in_use == max_in_use)
             then .[2] += [$line[3]]        # max_in_use_at << $line[3]
           else .
           end
       elif $line[1] == "IN" then .[0] -= 1 # in_use--
       else .
       end )
   | "Max licenses out: \(.[1]) at:\n \(.[2]|join("\n "))" ;

# The file is read in as a single string and so must be split at newlines:
split("\n") | max_licenses_in_use
```

{{Out}}
 $ time /usr/local/bin/jq -M -R -r -s -f Max_licences_in_use.jq mlijobs.txt
 Max licenses out: 99 at:
  2008/10/03_08:39:34
  2008/10/03_08:40:40
 
 real	0m0.163s
 user	0m0.154s
 sys	0m0.005s


## K


```K
  r:m@&a=x:|/a:+\{:[x[8+!3]~"OUT";1;-1]}'m:0:"mlijobs.txt";
  `0:,/"Maximum simultaneous license use is ",$x;
  `0:" at the following times:\n";`0:r[;14+!19]
```


Output:


```K
Maximum simultaneous license use is 99 at the following times:
2008/10/03_08:39:34
2008/10/03_08:40:40
```



## Julia

{{works with|Julia|0.6}}
{{trans|Python}}


```julia
function maximumsimultlicenses(io::IO)
    out, maxout, maxtimes = 0, -1, String[]
    for job in readlines(io)
        out += ifelse(occursin("OUT", job), 1, -1)
        if out > maxout
            maxout = out
            empty!(maxtimes)
        end
        if out == maxout
            push!(maxtimes, split(job)[4])
        end
    end
    return maxout, maxtimes
end

let (maxout, maxtimes) = open(maximumsimultlicenses, "data/mlijobs.txt")
    println("Maximum simultaneous license use is $maxout at the following times: \n - ", join(maxtimes, "\n - "))
end
```


{{out}}

```txt
Maximum simultaneous license use is 99 at the following times:
 - 2008/10/03_08:39:34
 - 2008/10/03_08:40:40
```



## Kotlin

The link for downloading the file, mlijobs.txt, is currently broken so I've created a small file myself to test the program:

```scala
// version 1.1.51

import java.io.File

fun main(args: Array<String>) {
    val filePath = "mlijobs.txt"
    var licenses = 0
    var maxLicenses = 0
    val dates = mutableListOf<String>()
    val f = File(filePath) 

    f.forEachLine { line ->
        if (line.startsWith("License OUT")) {
            licenses++
            if (licenses > maxLicenses) {
                maxLicenses = licenses
                dates.clear() 
                dates.add(line.substring(14, 33))
            }
            else if(licenses == maxLicenses) {
                dates.add(line.substring(14, 33))
            }
        }
        else if (line.startsWith("License IN")) {
            licenses--
        }
    }
    println("Maximum simultaneous license use is $maxLicenses at the following time(s):")
    println(dates.map { "  $it" }.joinToString("\n"))               
}
```


The file used for testing:

```txt

License OUT @ 2008/10/03_23:51:05 for job 4974
License OUT @ 2008/10/03_23:57:00 for job 4975
License IN  @ 2008/10/04_00:00:06 for job 4975
License OUT @ 2008/10/04_00:07:19 for job 4976
License IN  @ 2008/10/04_00:11:32 for job 4976
License IN  @ 2008/10/04_00:18:22 for job 4974

```


{{out}}

```txt

Maximum simultaneous license use is 2 at the following time(s):
  2008/10/03_23:57:00
  2008/10/04_00:07:19

```



## Lua


```lua

filename = "mlijobs.txt"
io.input( filename )

max_out, n_out = 0, 0
occurr_dates = {}

while true do
    line = io.read( "*line" )
    if line == nil then break end

    if string.find( line, "OUT" ) ~= nil then
        n_out = n_out + 1
        if n_out > max_out then
            max_out = n_out
            occurr_dates = {}
            occurr_dates[#occurr_dates+1] = string.match( line, "@ ([%d+%p]+)" )
        elseif n_out == max_out then
            occurr_dates[#occurr_dates+1] = string.match( line, "@ ([%d+%p]+)" )
        end
    else
        n_out = n_out - 1
    end
end

print( "Maximum licenses in use:", max_out )
print( "Occurrences:" )
for i = 1, #occurr_dates do
    print( "", occurr_dates[i] )
end
```

Output:

```txt
Maximum licenses in use: 99
Occurrences:
 	2008/10/03_08:39:34
 	2008/10/03_08:40:40
```


## M2000 Interpreter

Load.Doc load txt in any format of UTF-16LE, UTF16-BE, UTF-8, ANSI, in any line separator CRLF, CR, LF and place text in paragraphs in a document object. A document object return a string containing text (as utf16le using CRLF), but for some specific functions and statements treated as object. So changing paragraphs done with Paragraph$(a$, (m)) where m is by reference with a special syntax for this function (using without m we place order number, but m is a valid paragraph id, which always point to same paragraph until removing it).

In Wine 1.8 can't download.


```M2000 Interpreter

Module Checkit {
      Document a$, max_time$
      Load.doc a$, "mlijobs.txt"
      const dl$=" ", nl$={
      }
      Def long m, out, max_out=-1
      m=Paragraph(a$, 0) 
      If Forward(a$,m) then {
            While m {
                  job$=Paragraph$(a$,(m))
                  out+=If(Piece$(job$,dl$,2)="OUT"->1&, -1&)
                  If out>max_out then max_out=out : Clear max_time$
                  If out=max_out then max_time$=Piece$(job$,dl$,4)+nl$
            }
      }
      Report Format$("Maximum simultaneous license use is {0} at the following times:",max_out)
      Print "    ";  ' left margin
      Report max_time$
}
Checkit

```

Output

```txt
Maximum simultaneous license use is 99 at the following times:
    2008/10/03_08:39:34
    2008/10/03_08:40:40
```



## M4


```M4

divert(-1)
define(`current',0)
define(`max',0)
define(`OUT',`define(`current',incr(current))`'ifelse(eval(current>max),1,
   `define(`max',current)`'divert(-1)`'undivert(1)`'divert(1)',
   `ifelse(current,max,`divert(1)undivert(1)')')')
define(`IN',`define(`current',decr(current))')
define(`for',`divert(-1)')
include(mlijobs.txt))
divert
max
undivert(1)

```


Output:

```txt

99
 @ 2008/10/03_08:39:34  @ 2008/10/03_08:40:40

```



## Mathematica


```Mathematica
LC = 0; LCMax = 0; Scan[
   If[MemberQ[#, "OUT"], LC++; 
      If[LCMax < LC, LCMax = LC; LCMaxtimes = {};]; 
      If[LCMax == LC, AppendTo[LCMaxtimes, #[[4]]]],
   LC--;] &, Import["mlijobs.txt", "Table"]]
Print["The maximum number of licenses used was ", LCMax, ", at ", LCMaxtimes]
```


Output:

```txt
-> The maximum number of licenses used was 99, at {2008/10/03_08:39:34,2008/10/03_08:40:40}
```



## MAXScript


```maxscript
fn licencesInUse =
(
    local logFile = openFile "mlijobs.txt"
    local out = 0
    local maxOut = -1
    local maxTimes = #()
	
    while not EOF logFile do
    (
        line = readLine logFile

        if findString line "OUT" != undefined then
        (
            out += 1
        )
        else
        (
            out -= 1
        )

        if out > maxOut then
        (
            maxOut = out
            maxTimes = #()
        )

        if out == maxOut then
        (
            append maxTimes (filterString line " ")[4]
        )
    )
    format "Maximum simultaneous license use is % at the following times:\n" maxOut 

    for time in maxTimes do
    (
        format "%\n" time
    )

    close logFile
)

licencesInUse()
```

Output

```txt
Maximum simultaneous license use is 99 at the following times:
2008/10/03_08:39:34
2008/10/03_08:40:40
```



## Nim

{{trans|Python}}

```nim
import strutils

var
  curOut = 0
  maxOut = -1
  maxTimes = newSeq[string]()

for job in lines "mlijobs.txt":
  if "OUT" in job: inc curOut else: dec curOut
  if curOut > maxOut:
    maxOut = curOut
    maxTimes = @[]
  if curOut == maxOut:
    maxTimes.add job.split[3]

echo "Maximum simultaneous license use is ",maxOut," at the following times:"
for i in maxTimes: echo "  ",i
```

Output:

```txt
Maximum simultaneous license use is 99 at the following times:
  2008/10/03_08:39:34
  2008/10/03_08:40:40
```



## OCaml



```ocaml
let () =
  let out = ref 0 in
  let max_out = ref(-1) in
  let max_times = ref [] in

  let ic = open_in "mlijobs.txt" in
  try while true do
    let line = input_line ic in
    let io, date, n =
      Scanf.sscanf line
        "License %3[IN OUT] %_c %19[0-9/:_] for job %d"
        (fun io date n -> (io, date, n))
    in
    if io = "OUT" then incr out else decr out;
    if !out > !max_out then
    ( max_out := !out;
      max_times := [date]; )
    else if !out = !max_out then
      max_times := date :: !max_times;
  done
  with End_of_file ->
    close_in ic;
    Printf.printf
      "Maximum simultaneous license use is %d \
       at the following times:\n" !max_out;
    List.iter print_endline !max_times;
;;
```



## Oz

{{trans|Python}}


```oz
declare
  fun {MaxLicenses Filename ?Times}
     InUse = {NewCell 0}
     MaxInUse = {NewCell 0}
     MaxTimes = {NewCell nil}
  in
     for Job in {ReadLines Filename} do
        case {List.take Job 11} of "License OUT" then
  	 InUse := @InUse + 1
  	 if @InUse > @MaxInUse then
  	    MaxInUse := @InUse
  	    MaxTimes := nil
  	 end
  	 if @InUse == @MaxInUse then
	    JobTime = {Nth {String.tokens Job & } 4}
	 in
  	    MaxTimes := JobTime|@MaxTimes
  	 end
        [] "License IN " then
  	 InUse := @InUse - 1
        end
     end
     Times = {Reverse @MaxTimes}
     @MaxInUse
  end

  %% Helper.
  %% Returns a lazy list. So we don't keep the whole logfile in memory...
  fun {ReadLines Filename}
     F = {New class $ from Open.file Open.text end init(name:Filename)}
     fun lazy {ReadNext}
        case {F getS($)} of
           false then nil
        [] Line then
           Line|{ReadNext}
        end
     end
  in
     %% close file when handle becomes unreachable
     {Finalize.register F proc {$ F} {F close} end}
     {ReadNext}
  end

  Times
  MaxInUse = {MaxLicenses "mlijobs.txt" ?Times}
in
  {System.showInfo
   "Maximum simultaneous license use is "#MaxInUse#" at the following times:"}
  {ForAll Times System.showInfo}
```


Output:

```txt

Maximum simultaneous license use is 99 at the following times:
2008/10/03_08:39:34
2008/10/03_08:40:40

```



## PARI/GP


```parigp
license()={
	my(v=externstr("type mlijobs.txt"),u,cur,rec,t);
	for(i=1,#v,
		u=Vec(v[i]);
		if(#u>9 && u[9] == "O",
			if(cur++>rec,
				rec=cur;
				t=[v[i]]
			,
				if(cur == rec,t=concat(t,[v[i]]))
			)
		,
			cur--
		)
	);
	print(apply(s->concat(vecextract(Vec(s),"15..33")), t));
	rec
};
```


```txt
["2008/10/03_08:39:34", "2008/10/03_08:40:40"]
%1 = 99
```



## Perl


```perl
#!/usr/bin/perl -w
use strict;

my $out = 0;
my $max_out = -1;
my @max_times;

open FH, '<mlijobs.txt' or die "Can't open file: $!";
while (<FH>) {
    chomp;
    if (/OUT/) {
        $out++;
    } else {
        $out--;
    }
    if ($out > $max_out) {
        $max_out = $out;
        @max_times = ();
    }
    if ($out == $max_out) {
        push @max_times, (split)[3];
    }
}
close FH;

print "Maximum simultaneous license use is $max_out at the following times:\n";
print "  $_\n" foreach @max_times;
```

Example output:

```txt
Maximum simultaneous license use is 99 at the following times:
  2008/10/03_08:39:34
  2008/10/03_08:40:40

```



## Perl 6

Add some error trapping as recommended by [[Dijkstra]]. Not particularly necessary for this specific example but it doesn't hurt to be proactive.


Redirecting the mlijobs.txt file to STDIN:

```perl6
my %licenses;

%licenses<count max> = 0,0;

for $*IN.lines -> $line {
    my ( $license, $date_time );
    ( *, $license, *, $date_time ) = split /\s+/, $line;
    if $license eq 'OUT' {
        %licenses<count>++;
        if %licenses<count> > %licenses<max> {
            %licenses<max>   = %licenses<count>;
            %licenses<times> = [$date_time];
        }
        elsif %licenses<count> == %licenses<max> {
            %licenses<times>.push($date_time);
        }
    }
    elsif $license eq 'IN' {
        if %licenses<count> == %licenses<max> {
            %licenses<times>[*-1] ~= " through " ~ $date_time;
        }
        %licenses<count>--;
    }
    else {
        # Not a licence OUT or IN event, do nothing
    }
};

my $plural = %licenses<times>.elems == 1 ?? '' !! 's';

say "Maximum concurrent licenses in use: {%licenses<max>}, in the time period{$plural}:";
say join ",\n", %licenses<times>.list;
```


Example output:

```txt

Maximum concurrent licenses in use: 99, in the time periods:
2008/10/03_08:39:34 through 2008/10/03_08:39:45,
2008/10/03_08:40:40 through 2008/10/03_08:40:47

```



## Phix

Modified copy of [[[[Text_processing/Max_licenses_in_use#Euphoria|Euphoria]]]]

```Phix
constant fn = open("mlijobs.txt", "r")
integer maxout = 0, jobnumber
sequence jobs = {}, maxtime, scanres
string inout, jobtime
object line
while 1 do
    line = gets(fn)
    if atom(line) then exit end if
    scanres = scanf(line,"License %s @ %s for job %d\n")
    if length(scanres)!=1 then
        printf(1,"error scanning line: %s\n",{line})
        {} = wait_key()
        abort(0)
    end if
    {{inout,jobtime,jobnumber}} = scanres
    if inout="OUT" then
        jobs &= jobnumber
        if length(jobs)>maxout then
            maxout = length(jobs)
            maxtime = {jobtime}
        elsif length(jobs)=maxout then
            maxtime = append(maxtime, jobtime)
        end if
    else
        jobs[find(jobnumber,jobs)] = jobs[$]
        jobs = jobs[1..$-1]
    end if
end while
close(fn)
 
printf(1, "Maximum simultaneous license use is %d at the following times:\n", maxout)
for i = 1 to length(maxtime) do
    printf(1, "%s\n", {maxtime[i]})
end for
```

{{out}}

```txt

Maximum simultaneous license use is 99 at the following times:
2008/10/03_08:39:34
2008/10/03_08:40:40

```



## PHP


```php
$handle = fopen ("mlijobs.txt", "rb");
$maxcount = 0;
$count = 0;
$times = array();
while (!feof($handle)) {
    $buffer = fgets($handle);
    $op = trim(substr($buffer,8,3));
	switch ($op){
		case 'IN':
			$count--;
		break;
		case 'OUT':
			$count++;
			preg_match('/([\d|\/|_|:]+)/',$buffer,$time);
			if($count>$maxcount){
				$maxcount = $count;
				$times = Array($time[0]);
			}elseif($count == $maxcount){
				$times[] = $time[0];
			}
		break;
	}	
}
fclose ($handle);

echo $maxcount . '
';
for($i=0;$i<count($times);$i++){
	echo $times[$i] . '
';
}
```


```txt

99
2008/10/03_08:39:34
2008/10/03_08:40:40

```



## PL/I


```pli

text3: procedure options (main); /* 19 November 2011 */
   declare line character (80) varying;
   declare (nout, max_nout) fixed;
   declare saveline character (80) varying controlled;
   declare k fixed binary;
   declare in file input;

   open file (in) title ('/TEXT-MAX.DAT,TYPE(TEXT),RECSIZE(80)' );

   on endfile (in) go to finish_up;

   max_nout, nout = 0;
   do forever;
      get file (in) edit (line) (L);
      if substr(line, 9, 4) = 'OUT' then nout = nout+1;
      else if substr(line, 9, 3) = 'IN' then
         nout = nout-1;
      if nout = max_nout then
         do; allocate saveline; saveline = line; end;
      if nout > max_nout then
         do;
            do while (allocation(saveline) > 0); free saveline; end;
            max_nout = nout;
            allocate saveline;
            saveline = line;
         end;
   end;

finish_up:
   put skip list ('The maximum number of licences taken out = ' || max_nout);
   do while (allocation(saveline) > 0);
      k = index(saveline, '@');
      if k > 0 then put skip list ('It occurred at ' || substr(saveline, k+1) );
      free saveline;
   end;
end text3;

```

OUTPUT:

```txt

The maximum number of licences taken out =       99 
It occurred at  2008/10/03_08:40:40 for job 1837 
It occurred at  2008/10/03_08:39:34 for job 1833 

```



## PicoLisp

{{trans|AWK}}

Put the following into an executable file "licenses":

```PicoLisp
#!/usr/bin/picolisp /usr/lib/picolisp/lib.l

(zero Count MaxCount)

(in (opt)
   (while (split (line) " ")
      (case (pack (cadr (setq Line @)))
         (IN
            (dec 'Count) )
         (OUT
            (let Time (cadddr Line)
               (cond
                  ((> (inc 'Count) MaxCount)
                     (setq MaxCount Count  MaxTimes Time) )
                  ((= Count MaxCount)
                     (setq MaxTimes (pack MaxTimes " and " Time)) ) ) ) ) ) ) )

(prinl "The biggest number of licenses is " MaxCount " at " MaxTimes " !")
(bye)
```

Then it can be called as

```txt
$ ./licenses mlijobs.txt
The biggest number of licenses is 99 at 2008/10/03_08:39:34 and 2008/10/03_08:40:40 !
```



## PowerShell


```PowerShell

[int]$count = 0
[int]$maxCount = 0
[datetime[]]$times = @()

$jobs = Get-Content -Path ".\mlijobs.txt" | ForEach-Object {
    [string[]]$fields   = $_.Split(" ",[StringSplitOptions]::RemoveEmptyEntries)
    [datetime]$datetime = Get-Date $fields[3].Replace("_"," ")
    [PSCustomObject]@{
        State = $fields[1]
        Date  = $datetime
        Job   = $fields[6]
    }
}

foreach ($job in $jobs)
{
    switch ($job.State)
    {
        "IN"
        {
            $count--
        }
        "OUT"
        {
            $count++

            if ($count -gt $maxCount)
            {
                $maxCount = $count
                $times = @()
                $times+= $job.Date
            }
            elseif ($count -eq $maxCount)
            {
                $times+= $job.Date
            }
        }
    }
}

[PSCustomObject]@{
    LicensesOut = $maxCount
    StartTime   = $times[0]
    EndTime     = $times[1]
}

```

{{Out}}

```txt

LicensesOut StartTime            EndTime             
----------- ---------            -------             
         99 10/3/2008 8:39:34 AM 10/3/2008 8:40:40 AM

```



## PureBasic


```PureBasic
OpenConsole()

If ReadFile(0, OpenFileRequester("Text processing/3","mlijobs.txt","All files",1))  
  While Not Eof(0)
    currline$=ReadString(0)
    If StringField(currline$,2," ")="OUT"
      counter+1
    Else
      counter-1
    EndIf
    If counter>max  
      max=counter
      maxtime$=StringField(currline$,4," ")
    ElseIf counter=max
      maxtime$+#CRLF$+StringField(currline$,4," ")
    EndIf
  Wend
  PrintN(Str(max)+" license(s) used at ;"+#CRLF$+maxtime$)
  CloseFile(0)            
Else
  PrintN("Failed to open the file.")
EndIf

PrintN(#CRLF$+"Press ENTER to exit"): Input()
CloseConsole()
```


```txt

99 license(s) used at ;
2008/10/03_08:39:34
2008/10/03_08:40:40

Press ENTER to exit

```



## Python


```python
out, max_out, max_times = 0, -1, []
for job in open('mlijobs.txt'):
    out += 1 if "OUT" in job else -1
    if out > max_out:
        max_out, max_times = out, []
    if out == max_out:
        max_times.append(job.split()[3])
        
print("Maximum simultaneous license use is %i at the following times:" % max_out)
print('  ' + '\n  '.join(max_times))
```


{{out}}

```txt
Maximum simultaneous license use is 99 at the following times:
  2008/10/03_08:39:34
  2008/10/03_08:40:40

```



## R


```R

# Read in data, discard useless bits
dfr <- read.table("mlijobs.txt")
dfr <- dfr[,c(2,4)]
# Find most concurrent licences, and when
n.checked.out <- cumsum(ifelse(dfr$V2=="OUT", 1, -1))
times <- strptime(dfr$V4, "%Y/%m/%d_%H:%M:%S")
most.checked.out <- max(n.checked.out)
when.most.checked.out <- times[which(n.checked.out==most.checked.out)]
# As a bonus, plot license use
plot(times, n.checked.out, type="s")

```



## Racket


```racket
#lang racket
;;; reads a licence file on standard input
;;; returns max licences used and list of times this occurred
(define (count-licences)
  (let inner ((ln (read-line)) (in-use 0) (max-in-use 0) (times-list null))
    (if (eof-object? ln) 
        (values max-in-use (reverse times-list))
        (let ((mtch (regexp-match #px"License (IN |OUT) @ (.*) for job.*" ln)))
          (cond
            [(string=? "OUT" (second mtch))
             (let ((in-use+1 (add1 in-use)))
               (cond
                 [(> in-use+1 max-in-use)
                  (inner (read-line) in-use+1 in-use+1 (list (third mtch)))]
                 [(= in-use+1 max-in-use)
                  (inner (read-line) in-use+1 max-in-use (cons (third mtch) times-list))]
                 [else (inner (read-line) in-use+1 max-in-use times-list)]))]
            [(string=? "IN " (second mtch))
             (inner (read-line) (sub1 in-use) max-in-use times-list)]
            [else (inner (read-line) in-use max-in-use times-list)])))))

(define-values (max-used max-used-when)
(with-input-from-file "mlijobs.txt" count-licences))
(printf "Maximum licences in simultaneously used is ~a at the following times:~%"
        max-used)
(for-each displayln max-used-when)

```

Output:

```txt
Maximum licences in simultaneously used is 99 at the following times:
2008/10/03_08:39:34
2008/10/03_08:40:40
```


The following takes advantage of a combination of ''for/fold'' and ''(in-lines)'' (and is possible more in the Racket idiom, rather than just using ''(read-line)'' and ''(regexp-match)'':


```racket
#lang racket
;;; reads a licence file on standard input
;;; returns max licences used and list of times this occurred
(define (count-licences)
  (define (sub-count-licences)
    (for/fold ((in-use 0) (max-in-use 0) (times-list null))
      ((ln (in-lines)))
      (let ((mtch (regexp-match #px"License (IN |OUT) @ (.*) for job.*" ln)))
        (cond
          [(string=? "OUT" (second mtch))
           (let ((in-use+1 (add1 in-use)))
             (cond
               [(> in-use+1 max-in-use) (values in-use+1 in-use+1 (list (third mtch)))]
               [(= in-use+1 max-in-use) (values in-use+1 max-in-use (cons (third mtch) times-list))]
               [else (values in-use+1 max-in-use times-list)]))]
          [(string=? "IN " (second mtch)) (values (sub1 in-use) max-in-use times-list)]
          [else (values in-use max-in-use times-list)]))))
  (let-values (((in-use max-in-use times-list) (sub-count-licences)))
    (values max-in-use (reverse times-list))))

(define-values (max-used max-used-when) (with-input-from-file "mlijobs.txt" count-licences))
(printf "Maximum licences in simultaneously used is ~a at the following times:~%" max-used)
(for-each displayln max-used-when)
```

(Same output)


## REXX


### version 1


```rexx
/*REXX program processes instrument data as read from a time sorted data file.*/
iFID= 'LICENSE.LOG'                    /*the fileID of the   input  file.     */
high=0                                 /*highest number of licenses (so far). */
#=0                                    /*the count of number of licenses out. */
n=0                                    /*the number of  highest licenses out. */
    do recs=0  while lines(iFID)\==0   /* [↓]  read file  until  end─of─file. */
    parse value linein(iFID) with . ? . $       /*get IN│OUT status, job info.*/
    if ?=='IN'  then                   #=#-1    /*decrement the license count.*/
                else if ?=='OUT'  then #=#+1    /*increment  "     "      "   */
    if # >high then do;  n=1;    job.1=$;  end  /*the job info for highest cnt*/
    if #==high then do;  n=n+1;  job.n=$;  end  /* "   "    "   "   equal   " */
    high=max(high,#)                            /*calculate max license count.*/
    end   /*while ···*/

say recs  'records read from the input file: '  iFID
say 'The maximum number of licenses out is '    high        " at:"
say
     do j=1  for n                     /*show what/when max licenses occurred.*/
     say left('',20)  job.j            /*indent the information displayed.    */
     end   /*j*/                       /*stick a fork in it,  we're all done. */
```

'''output'''   when using the default input file:

```txt

10000 records read from the input file:  LICENSE.LOG
The maximum number of licenses out is  99  at:

                     2008/10/03_08:39:34 for job 1833
                     2008/10/03_08:40:40 for job 1837

```


===Version 2 dual-coded for PC and TSO===
It should be noted that almost every REXX interpreter returns a different string for   '''parse source'''   under Microsoft Windows:
::*   '''MSDOS'''                          for   BREXX 
::*   '''PCDOS'''                          for   PC/REXX   and   Personal Rexx for DOS
::*   '''WIN'''             for   Personal Rexx for Windows 
::*   '''WIN32'''                     for   Regina
::*   '''Win32'''                     for   R4   and   ROO 


It is unknown which classic REXX interpreter can be used (under Windows) below, it fails for the above mentioned seven REXX interpreters. 

I am sure it has worked for Regina.
Maybe this is now correct for all the WIN (and xxDOS) situations (pun intended) 

```rexx
/* REXX **************************************************************
* 19.11.2012 Walter Pachl transcribed from PL/I
*                         and dual-coded (for PC (Windows) and TSO)
**********************************************************************/
Parse Source source
call time 'R'
Say source
Parse Upper source system .
If left(system,3)='WIN' |,  /* changed from 'Windows' (I see WIN64 in source) */
   wordpos(system,'MSDOS PCDOS')>0 Then Do  
  fid='mlijobs.txt'
  Do i=1 By 1 While lines(fid)>0
    l.i=linein(fid)
    End
  l.0=i-1
  End
Else Do
  "ALLOC FI(IN) DA(MLIJOBS.TEXT) SHR REUSE"
  'EXECIO * DISKR IN (STEM L. FINIS'
  'FREE FI(IN)'
  End
store.0=0
max_nout=0
nout    =0
cnt=0
cnt.=0
do i=1 To l.0
  line=l.i
  Parse Var line 9 inout +3
  Select
    When inout='OUT' then nout = nout+1;
    When inout='IN'  then nout = nout-1;
    Otherwise Iterate
    End
  cnt.nout=cnt.nout+1
  cnt=cnt+1
  if nout = max_nout then
    Call store line
  if nout > max_nout then Do
    max_nout=nout
    drop store.
    store.0=0
    Call store
    end;
  end;

Say 'The maximum number of licences taken out = ' max_nout
Do i=1 to store.0
  k=pos('@',store.i)
  Say 'It occurred at 'substr(store.i,k+1)
  End
limit=5
Do nout=0 To max_nout
  If nout=limit+1 Then Say '.........'
  If nout<=limit | nout>=max_nout-limit Then
    Say right(cnt.nout,5) right(nout,3)
  End
Say right(cnt,5) 'Jobs'
Say time('E') 'seconds (elapsed)'
Exit

store:
  z=store.0+1
  store.z=line
  store.0=z
  Return
```

{{out}} on Windows

```txt

D:\>rexx maxl
WindowsNT COMMAND D:\maxl.rex
The maximum number of licences taken out =  99
It occurred at  2008/10/03_08:39:34 for job 1833
It occurred at  2008/10/03_08:40:40 for job 1837
    1   0
    2   1
    2   2
    2   3
    2   4
    2   5
.........
   24  94
   22  95
   15  96
    7  97
    5  98
    2  99
10000 Jobs
0.234000 seconds (elapsed) ooRexx

D:\>regina maxl
WIN64 COMMAND D:\maxl.rex
...
10000 Jobs
.177000 seconds (elapsed) Regina
```

{{out}} on TSO

```txt
TSO COMMAND LIC SYS00059 N555555.PRIV.CLIST ? TSO ISPF ?
The maximum number of licences taken out =  99
It occurred at  2008/10/03_08:39:34 for job 1833
It occurred at  2008/10/03_08:40:40 for job 1837
    1   0
    2   1
    2   2
    2   3
    2   4
    2   5
.........
   24  94
   22  95
   15  96
    7  97
    5  98
    2  99
10000 Jobs
0.384154 seconds (elapsed)
```



## Ruby


```ruby
out = 0
max_out = -1
max_times = []
 
File.foreach('mlijobs.txt') do |line|
  out += line.include?("OUT") ? 1 : -1
  if out > max_out
    max_out = out
    max_times = []
  end
  max_times << line.split[3]  if out == max_out
end
 
puts "Maximum simultaneous license use is #{max_out} at the following times:"
max_times.each {|time| puts "  #{time}"}
```


Example output:

```txt
Maximum simultaneous license use is 99 at the following times:
  2008/10/03_08:39:34
  2008/10/03_08:40:40

```


## Run BASIC


```lb
open "c:\data\temp\logFile.txt" for input as #f
while not(eof(#f))
  line input #f, a$
  if word$(a$,2," ") = "IN" then count = count - 1 else count = count + 1
  maxCount = max(maxCount,count)
wend
open "c:\data\temp\logFile.txt" for input as #f
while not(eof(#f))
  line input #f, a$
  if word$(a$,2," ") = "IN" then count = count - 1 else count = count + 1
  if count = maxCount then theDate$ = theDate$ + " | " + word$(a$,4," ") + " Job:";word$(a$,7," ")
wend
print maxCount;" ";theDate$
```



## Scala


### Translated imperative version

Can be seen running in your browser [https://scastie.scala-lang.org/a31Q3P5gQ36VcAre5i4I7g Scastie (remote JVM)].

```Scala
import java.io.{BufferedReader, InputStreamReader}
import java.net.URL

object License0 extends App {
  val url = new URL("https://raw.githubusercontent.com/def-/nim-unsorted/master/mlijobs.txt")
  val in = new BufferedReader(new InputStreamReader(url.openStream()))

  val dates = new collection.mutable.ListBuffer[String]
  var (count: Int, max: Int) = (0, Int.MinValue)
  var line: String = _

  while ( {line = in.readLine; line} != null) {
    if (line.startsWith("License OUT ")) count += 1
    if (line.startsWith("License IN ")) count -= 1 // Redundant test when "OUT" 
    if (count > max) { // Fruitless execution when "License IN " 
      max = count
      val date = line.split(" ")(3)
      dates.clear()
      dates += date
    } else if (count == max) {
      val date = line.split(" ")(3)
      dates += date
    }
  }

  println("Max licenses out: " + max)
  println("At time(s): " + dates.mkString(", "))

}
```



### Smart Imperative Scala with dirty side effects

{{Out}}Best seen running in your browser [https://scastie.scala-lang.org/2bdByPOUSICgRIMx4KJ29A Scastie (remote JVM)].

```Scala
import scala.collection.mutable.ListBuffer

object License1 extends App {
  val src = io.Source.fromURL("https://raw.githubusercontent.com/def-/nim-unsorted/master/mlijobs.txt")

  val dates = new ListBuffer[String]
  var (max, count) = (Int.MinValue, 0)

  src.getLines.foreach { line =>
    def date = line.split(" ")(3)

    if (line.startsWith("License OUT ")) {
      count += 1
      if (count > max) {
        max = count
        dates.clear
      }
      if (count == max) dates += date
    } else if (line.startsWith("License IN ")) count -= 1
  }

  println("Max licenses out: " + max)
  println("At time(s): " + dates.mkString(", "))

}
```

===Clean coded [[functional_programming|FP]] with (tail) recursion, no side effects===
{{Out}}Best seen running in your browser [https://scastie.scala-lang.org/MvwfLtnFTcqBoNHwB4aURg Scastie (remote JVM)].

```Scala
import scala.annotation.tailrec

object License2 extends App {
  type resultTuple = (Int /*max*/, Int /*count*/, List[String] /*dates*/ )

  val src = io.Source.fromURL(
    "https://raw.githubusercontent.com/def-/nim-unsorted/master/mlijobs.txt")
  val iter = src.getLines()
  val (max, count, dates) = loop(Int.MinValue, 0, Nil)

  def lineToResult(tuple: (resultTuple, String)): resultTuple = {
    val ((max, count, dates), line) = tuple

    def date = line.split(" ")(3)

    if (line.startsWith("License OUT ")) {
      if (count + 1 > max) (count + 1, count + 1, List(date))
      else if (count + 1 == max) (max, max, dates :+ date)
      else (max, count + 1, dates)
    } else if (line.startsWith("License IN ")) tuple._1.copy(_2 = count - 1)
    else tuple._1
  }

  @tailrec
  private def loop(tuple: resultTuple): resultTuple = {
    def lineToResult(tuple: (resultTuple, String)): resultTuple = {
      val ((max, count, dates), line) = tuple

      def date = line.split(" ")(3)

      if (line.startsWith("License OUT ")) {
        if (count + 1 > max) (count + 1, count + 1, List(date))
        else if (count + 1 == max) (max, max, dates :+ date)
        else (max, count + 1, dates)
      } else if (line.startsWith("License IN ")) tuple._1.copy(_2 = count - 1)
      else tuple._1
    }

    if (iter.hasNext)
      loop(lineToResult(tuple, iter.next()))
    else tuple
  }

  println("Max licenses out: " + max)
  println("At time(s): " + dates.mkString(", "))

}
```

===Totally [[functional_programming|FP]] with foldLeft===
{{Out}}Best seen running in your browser [https://scastie.scala-lang.org/h8CgsFx8TJGtRy1B5KVSOg Scastie (remote JVM)].

```Scala
object License3 extends App {
  type resultTuple = (Int /*max*/, Int /*count*/, List[String] /*dates*/ )

  val src = io.Source.fromURL("https://raw.githubusercontent.com/def-/nim-unsorted/master/mlijobs.txt")

  val (max, count, dates): resultTuple =
    src.getLines().foldLeft(Int.MinValue, 0, Nil: List[String]) {
      case ((max: Int, count: Int, dates: List[String]), line: String)
        if line.startsWith("License OUT ") =>
        def date = line.split(" ")(3)

        if (count + 1 > max) (count + 1, count + 1, List(date))
        else if (count + 1 == max) (max, max, dates :+ date)
        else (max, count + 1, dates)

      case (resultPart: resultTuple, line: String)
        if line.startsWith("License IN ") =>
        resultPart.copy(_2 = resultPart._2 - 1)

      case (resultPart, _) => resultPart
    }

  println("Max licenses out: " + max)
  println("At time(s): " + dates.mkString(", "))

}
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var file: inFile is STD_NULL;
    var string: line is "";
    var integer: currLicenses is 0;
    var integer: maxLicenses is 0;
    var array string: maxLicenseTimes is 0 times "";
    var string: eventTime is "";
  begin
    inFile := open("mlijobs.txt", "r");
    while hasNext(inFile) do
      line := getln(inFile);
      if line[9 len 3] = "OUT" then
        incr(currLicenses);
        if currLicenses >= maxLicenses then
          if currLicenses > maxLicenses then
            maxLicenses := currLicenses;
            maxLicenseTimes := 0 times "";
          end if;
          maxLicenseTimes &:= line[15 len 19];
        end if;
      elsif currLicenses > 0 then
        decr(currLicenses);
      end if;
    end while;
    close(inFile);
    writeln("Maximum simultaneous license use is " <& maxLicenses <& " at the following times:");
    for eventTime range maxLicenseTimes do
      writeln(eventTime);
    end for;
  end func;
```


Output:

```txt

Maximum simultaneous license use is 99 at the following times:
2008/10/03_08:39:34
2008/10/03_08:40:40

```



## Sidef

{{trans|Perl}}

```ruby
var out = 0
var max_out = -1
var max_times = []

ARGF.each { |line|
    out += (line ~~ /OUT/ ? 1 : -1)
    if (out > max_out) {
        max_out = out
        max_times = []
    }
    if (out == max_out) {
        max_times << line.split(' ')[3]
    }
}

say "Maximum simultaneous license use is #{max_out} at the following times:"
max_times.each {|t| say "  #{t}" }
```

{{out}}

```txt

$ sidef max_licenses.sf < mlijobs.txt
Maximum simultaneous license use is 99 at the following times:
  2008/10/03_08:39:34
  2008/10/03_08:40:40

```



## Tcl


{{trans|Python}}


```tcl
 set out 0
 set max_out -1
 set max_times {}
 
 foreach job [split [read [open "mlijobs.txt" "r"]] "\n"] {
     if {[lindex $job 1] == "OUT"} {
         incr out 
     } { 
         incr out -1
     }   
     if {$out > $max_out} {
         set max_out $out
         set max_times {}
     }   
     if {$out == $max_out} {
         lappend max_times [lindex $job 3]
     }   
 }
 
 puts "Maximum simultaneous license use is $max_out at the following times:"
 foreach t $max_times {
     puts "  $t"
 }
```


Output matches Python

## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
joblog="mlijobs.txt",jobnrout=0
log=FILE (joblog)
DICT jobnrout CREATE
LOOP l=log
jobout=EXTRACT (l,":License :"|,": :")
 IF (jobout=="out") THEN
  time=EXTRACT (l,":@ :"|,": :"), jobnrout=jobnrout+1
  DICT jobnrout APPEND/QUIET jobnrout,num,cnt,time;" "
 ELSE
  jobnrout=jobnrout-1
 ENDIF
ENDLOOP
DICT jobnrout UNLOAD jobnrout,num,cnt,time
DICT jobnrout SIZE maxlicout
times=SELECT (time,#maxlicout)
PRINT "The max. number of licences out is ", maxlicout
PRINT "at these times: ", times

```

Output:

```txt

The max. number of licences out is 99
at these times: 2008/10/03_08:39:34 2008/10/03_08:40:40

```



## Ursala

Four functions are defined. Log lexes the log file, which can be accessed as a pre-declared constant without explicit I/O by being given as a compile-time command line parameter. Scan accumulates running totals of licenses in use. Search identifies the maxima, and format transforms the results to human readable form.


```Ursala

#import std
#import nat

log    = ^(~&hh==`O,~&tth)*FtPS sep` *F mlijobs_dot_txt
scan   = @NiX ~&ar^& ^C/~&alrhr2X ~&arlh?/~&faNlCrtPXPR ~&fabt2R
search = @lSzyCrSPp leql$^&hl@lK2; ^/length@hl ~&rS
format = ^|C\~& --' licenses in use at'@h+ %nP

#show+

main = format search scan log
```

output:

```txt
99 licenses in use at
2008/10/03_08:39:34
2008/10/03_08:40:40

```



## Vedit macro language


```vedit

File_Open("|(PATH_ONLY)\data\mlijobs.txt", BROWSE)

#1 = 0          // Number of licenses active
#2 = 0          // Max number of active licenses found

Repeat(ALL) {
    Search("|{OUT,IN}|W@", ADVANCE+ERRBREAK)
    if (Match_Item == 1) {                      // "OUT"
        #1++
        if (#1 > #2) {                          // new high value
            #2 = #1
            Reg_Empty(10)                       // empty the time list
        }
        if (#1 == #2) {                         // same as high value
            Reg_Copy(10, 1, APPEND)             // store time
        }
    } else {                                    // "IN"
        #1--
    }
}

Message("Maximum simultaneous license use is ")
Num_Type(#2, LEFT+NOCR)
Message(" at the following times:\n")
Reg_Type(10)

Buf_Quit(OK)

```


Output:

```txt

Maximum simultaneous license use is 99 at the following times:
 2008/10/03_08:39:34 for job 1833
 2008/10/03_08:40:40 for job 1837

```



## zkl

{{trans|D}}
The referenced log file is no longer available, so, using the example log:

```zkl
nOut,maxOut,maxTimes:=0,-1,List();
foreach job in (File("mlijobs.txt")){
   _,status,_,date:=job.split();
   nOut+=( if(status.toUpper()=="OUT") 1 else -1 );
   if(nOut>maxOut){ maxOut=nOut; maxTimes.clear(); }
   if(nOut==maxOut) maxTimes.append(date);
}
println(("Maximum simultaneous license use is %d at"
         " the following times:\n %s").fmt(maxOut,maxTimes.concat("\n")));
```

{{out}}

```txt

Maximum simultaneous license use is 1 at the following times:
 2008/10/03_23:51:05

```



{{omit from|GUISS}}
{{omit from|Lilypond}}
{{omit from|Openscad}}
