+++
title = "Text processing/2"
description = ""
date = 2019-05-07T13:54:03Z
aliases = []
[extra]
id = 3111
[taxonomies]
categories = []
tags = []
+++

{{task|Text processing}}

The following task concerns data that came from a pollution monitoring station with twenty-four instruments monitoring twenty-four aspects of pollution in the air. Periodically a record is added to the file, each record being a line of 49 fields separated by white-space, which can be one or more space or tab characters.

The fields (from the left) are:
  DATESTAMP [ VALUEn FLAGn ] * 24
i.e. a datestamp followed by twenty-four repetitions of a floating-point instrument value and that instrument's associated integer flag. Flag values are >= 1 if the instrument is working and < 1 if there is some problem with it, in which case that instrument's value should be ignored.

A sample from the full data file [http://rosettacode.org/resources/readings.zip readings.txt], which is also used in the [[Text processing/1]] task, follows:

Data is no longer available at that link. Zipped mirror available [https://github.com/thundergnat/rc/blob/master/resouces/readings.zip here]

```txt

1991-03-30	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1
1991-03-31	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	20.000	1	20.000	1	20.000	1	35.000	1	50.000	1	60.000	1	40.000	1	30.000	1	30.000	1	30.000	1	25.000	1	20.000	1	20.000	1	20.000	1	20.000	1	20.000	1	35.000	1
1991-03-31	40.000	1	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2
1991-04-01	0.000	-2	13.000	1	16.000	1	21.000	1	24.000	1	22.000	1	20.000	1	18.000	1	29.000	1	44.000	1	50.000	1	43.000	1	38.000	1	27.000	1	27.000	1	24.000	1	23.000	1	18.000	1	12.000	1	13.000	1	14.000	1	15.000	1	13.000	1	10.000	1
1991-04-02	8.000	1	9.000	1	11.000	1	12.000	1	12.000	1	12.000	1	27.000	1	26.000	1	27.000	1	33.000	1	32.000	1	31.000	1	29.000	1	31.000	1	25.000	1	25.000	1	24.000	1	21.000	1	17.000	1	14.000	1	15.000	1	12.000	1	12.000	1	10.000	1
1991-04-03	10.000	1	9.000	1	10.000	1	10.000	1	9.000	1	10.000	1	15.000	1	24.000	1	28.000	1	24.000	1	18.000	1	14.000	1	12.000	1	13.000	1	14.000	1	15.000	1	14.000	1	15.000	1	13.000	1	13.000	1	13.000	1	12.000	1	10.000	1	10.000	1

```


;Task:
# Confirm the general field format of the file.
# Identify any DATESTAMPs that are duplicated.
# Report the number of records that have good readings for all instruments.





## Ada

{{libheader|Simple components for Ada}}

```ada
with Ada.Calendar;           use Ada.Calendar;
with Ada.Text_IO;            use Ada.Text_IO;
with Strings_Edit;           use Strings_Edit;
with Strings_Edit.Floats;    use Strings_Edit.Floats;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

with Generic_Map;

procedure Data_Munging_2 is
   package Time_To_Line is new Generic_Map (Time, Natural);
   use Time_To_Line;
   File    : File_Type;
   Line_No : Natural := 0;
   Count   : Natural := 0;
   Stamps  : Map;
begin
   Open (File, In_File, "readings.txt");
   loop
      declare
         Line    : constant String := Get_Line (File);
         Pointer : Integer := Line'First;
         Flag    : Integer;
         Year, Month, Day : Integer;
         Data    : Float;
         Stamp   : Time;
         Valid   : Boolean := True;
      begin
         Line_No := Line_No + 1;
         Get (Line, Pointer, SpaceAndTab);
         Get (Line, Pointer, Year);
         Get (Line, Pointer, Month);
         Get (Line, Pointer, Day);
         Stamp := Time_Of (Year_Number (Year), Month_Number (-Month), Day_Number (-Day));
         begin
            Add (Stamps, Stamp, Line_No);
         exception
            when Constraint_Error =>
               Put (Image (Year) & Image (Month) & Image (Day) & ": record at " & Image (Line_No));
               Put_Line (" duplicates record at " & Image (Get (Stamps, Stamp)));
         end;
         Get (Line, Pointer, SpaceAndTab);
         for Reading in 1..24 loop
            Get (Line, Pointer, Data);
            Get (Line, Pointer, SpaceAndTab);
            Get (Line, Pointer, Flag);
            Get (Line, Pointer, SpaceAndTab);
            Valid := Valid and then Flag >= 1;
         end loop;
         if Pointer <= Line'Last then
            Put_Line ("Unrecognized tail at " & Image (Line_No) & ':' & Image (Pointer));
         elsif Valid then
            Count := Count + 1;
         end if;
      exception
         when End_Error | Data_Error | Constraint_Error | Time_Error =>
            Put_Line ("Syntax error at " & Image (Line_No) & ':' & Image (Pointer));
      end;
   end loop;
exception
   when End_Error =>
      Close (File);
      Put_Line ("Valid records " & Image (Count) & " of " & Image (Line_No) & " total");
end Data_Munging_2;
```

Sample output

```txt

1990-3-25: record at 85 duplicates record at 84
1991-3-31: record at 456 duplicates record at 455
1992-3-29: record at 820 duplicates record at 819
1993-3-28: record at 1184 duplicates record at 1183
1995-3-26: record at 1911 duplicates record at 1910
Valid records 5017 of 5471 total

```



## Aime


```aime
check_format(list l)
{
    integer i;
    text s;

    if (~l != 49) {
        error("bad field count");
    }

    s = l[0];
    if (match("????-??-??", s)) {
        error("bad date format");
    }
    l[0] = s.delete(7).delete(4).atoi;

    i = 1;
    while (i < 49) {
        atof(l[i]);
        i += 1;
        l[i >> 1] = atoi(l[i]);
        i += 1;
    }

    l.erase(25, -1);
}

main(void)
{
    integer goods, i, v;
    file f;
    list l;
    index x;

    goods = 0;

    f.affix("readings.txt");

    while (f.list(l, 0) != -1) {
        if (!trap(check_format, l)) {
            if ((x[v = lf_x_integer(l)] += 1) != 1) {
                v_form("duplicate ~ line\n", v);
            }

            i = 1;
            l.ucall(min_i, 1, i);
            goods += iclip(0, i, 1);
        }
    }

    o_(goods, " good lines\n");

    0;
}
```

{{out}} (the "reading.txt" needs to be converted to UNIX end-of-line)

```txt
duplicate 19900325 line
duplicate 19910331 line
duplicate 19920329 line
duplicate 19930328 line
duplicate 19950326 line
5017 good lines
```



## AutoHotkey



```autohotkey
; Author: AlephX Aug 17 2011
data = %A_scriptdir%\readings.txt

Loop, Read, %data%
	{
	Lines := A_Index
    StringReplace, dummy, A_LoopReadLine, %A_Tab%,, All UseErrorLevel

    Loop, parse, A_LoopReadLine, %A_Tab%
		{
		wrong := 0
		if A_index = 1
			{
			Date := A_LoopField
			if (Date == OldDate)
				{
				WrongDates = %WrongDates%%OldDate% at %Lines%`n
				TotwrongDates++
				Wrong := 1
				break
				}
			}
		else
			{
			if (A_loopfield/1 < 0)
				{
				Wrong := 1
				break
				}

			}
		}

	if (wrong == 1)
		totwrong++
	else
		valid++

	if (errorlevel <> 48)
		{
		if (wrong == 0)
			{
			totwrong++
			valid--
			}
		unvalidformat++
		}

	olddate := date
	}

msgbox, Duplicate Dates:`n%wrongDates%`nRead Lines: %lines%`nValid Lines: %valid%`nwrong lines: %totwrong%`nDuplicates: %TotWrongDates%`nWrong Formatted: %unvalidformat%`n

```


Sample Output:


```txt

Duplicate Dates:
1990-03-25 at 85
1991-03-31 at 456
1992-03-29 at 820
1993-03-28 at 1184
1995-03-26 at 1911

Read Lines: 5471
Valid Lines: 5129
wrong lines: 342
Duplicates: 5
Wrong Formatted: 0

```



## AWK

A series of AWK one-liners are shown as this is often what is done. If this information were needed repeatedly, (and this is not known), a more permanent shell script might be created that combined multi-line versions of the scripts below.

'''Gradually tie down the format.'''

(In each case offending lines will be printed)

If their are any scientific notation fields then their will be an e in the file:

```awk
bash$ awk '/[eE]/' readings.txt
bash$
```

Quick check on the number of fields:

```awk
bash$ awk 'NF != 49' readings.txt
bash$
```

Full check on the file format using a regular expression:

```awk
bash$ awk '!(/^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]([ \t]+[-]?[0-9]+\.[0-9]+[\t ]+[-]?[0-9]+)+$/ && NF==49)' readings.txt
bash$
```

Full check on the file format as above but using regular expressions allowing intervals (gnu awk):

```awk
bash$ awk  --re-interval '!(/^[0-9]{4}-[0-9]{2}-[0-9]{2}([ \t]+[-]?[0-9]+\.[0-9]+[\t ]+[-]?[0-9]+){24}+$/ )' readings.txt
bash$
```



'''Identify any DATESTAMPs that are duplicated.'''

Accomplished by counting how many times the first field occurs and noting any second occurrences.

```awk
bash$ awk '++count[$1]==2{print $1}' readings.txt
1990-03-25
1991-03-31
1992-03-29
1993-03-28
1995-03-26
bash$
```



'''What number of records have good readings for all instruments.'''

<div style="width:100%;overflow:scroll">

```awk
bash$ awk '{rec++;ok=1; for(i=0;i<24;i++){if($(2*i+3)<1){ok=0}}; recordok += ok} END {print "Total records",rec,"OK records", recordok, "or", recordok/rec*100,"%"}'  readings.txt
Total records 5471 OK records 5017 or 91.7017 %
bash$
```

</div>


## C


```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

typedef struct { const char *s; int ln, bad; } rec_t;
int cmp_rec(const void *aa, const void *bb)
{
	const rec_t *a = aa, *b = bb;
	return a->s == b->s ? 0 : !a->s ? 1 : !b->s ? -1 : strncmp(a->s, b->s, 10);
}

int read_file(const char *fn)
{
	int fd = open(fn, O_RDONLY);
	if (fd == -1) return 0;

	struct stat s;
	fstat(fd, &s);

	char *txt = malloc(s.st_size);
	read(fd, txt, s.st_size);
	close(fd);

	int i, j, lines = 0, k, di, bad;
	for (i = lines = 0; i < s.st_size; i++)
		if (txt[i] == '\n') {
			txt[i] = '\0';
			lines++;
		}

	rec_t *rec = calloc(sizeof(rec_t), lines);
	const char *ptr, *end;
	rec[0].s = txt;
	rec[0].ln = 1;
	for (i = 0; i < lines; i++) {
		if (i + 1 < lines) {
			rec[i + 1].s = rec[i].s + strlen(rec[i].s) + 1;
			rec[i + 1].ln = i + 2;
		}
		if (sscanf(rec[i].s, "%4d-%2d-%2d", &di, &di, &di) != 3) {
			printf("bad line %d: %s\n", i, rec[i].s);
			rec[i].s = 0;
			continue;
		}
		ptr = rec[i].s + 10;

		for (j = k = 0; j < 25; j++) {
			if (!strtod(ptr, (char**)&end) && end == ptr) break;
			k++, ptr = end;
			if (!(di = strtol(ptr, (char**)&end, 10)) && end == ptr) break;
			k++, ptr = end;
			if (di < 1) rec[i].bad = 1;
		}

		if (k != 48) {
			printf("bad format at line %d: %s\n", i, rec[i].s);
			rec[i].s = 0;
		}
	}

	qsort(rec, lines, sizeof(rec_t), cmp_rec);
	for (i = 1, bad = rec[0].bad, j = 0; i < lines && rec[i].s; i++) {
		if (rec[i].bad) bad++;
		if (strncmp(rec[i].s, rec[j].s, 10)) {
			j = i;
		} else
			printf("dup line %d: %.10s\n", rec[i].ln, rec[i].s);
	}

	free(rec);
	free(txt);
	printf("\n%d out %d lines good\n", lines - bad, lines);
	return 0;
}

int main()
{
	read_file("readings.txt");
	return 0;
}
```


{{out}}

```txt

dup line 85: 1990-03-25
dup line 456: 1991-03-31
dup line 820: 1992-03-29
dup line 1184: 1993-03-28
dup line 1911: 1995-03-26

5017 out 5471 lines good

```



## C++

{{libheader|Boost}}

```cpp
#include <boost/regex.hpp>
#include <fstream>
#include <iostream>
#include <vector>
#include <string>
#include <set>
#include <cstdlib>
#include <algorithm>
using namespace std ;

boost::regex e ( "\\s+" ) ;

int main( int argc , char *argv[ ] ) {
   ifstream infile( argv[ 1 ] ) ;
   vector<string> duplicates ;
   set<string> datestamps ; //for the datestamps
   if ( ! infile.is_open( ) ) {
      cerr << "Can't open file " << argv[ 1 ] << '\n' ;
      return 1 ;
   }
   int all_ok = 0  ;//all_ok for lines in the given pattern e
   int pattern_ok = 0 ; //overall field pattern of record is ok
   while ( infile ) {
      string eingabe ;
      getline( infile , eingabe ) ;
      boost::sregex_token_iterator i ( eingabe.begin( ), eingabe.end( ) , e , -1 ), j  ;//we tokenize on empty fields
      vector<string> fields( i, j ) ;
      if ( fields.size( ) == 49 ) //we expect 49 fields in a record
         pattern_ok++ ;
      else
         cout << "Format not ok!\n" ;
      if ( datestamps.insert( fields[ 0 ] ).second ) { //not duplicated
         int howoften = ( fields.size( ) - 1 ) / 2 ;//number of measurement
                                                    //devices and values
         for ( int n = 1 ; atoi( fields[ 2 * n ].c_str( ) ) >= 1 ; n++ ) {
            if ( n == howoften ) {
               all_ok++ ;
               break ;
            }
         }
      }
      else {
         duplicates.push_back( fields[ 0 ] ) ;//first field holds datestamp
      }
   }
   infile.close( ) ;
   cout << "The following " << duplicates.size() << " datestamps were duplicated:\n" ;
   copy( duplicates.begin( ) , duplicates.end( ) ,
         ostream_iterator<string>( cout , "\n" ) ) ;
   cout << all_ok << " records were complete and ok!\n" ;
   return 0 ;
}
```


{{out}}

```txt

Format not ok!
The following 6 datestamps were duplicated:
1990-03-25
1991-03-31
1992-03-29
1993-03-28
1995-03-26
2004-12-31

```


=={{header|C sharp|C#}}==

```csharp
using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.IO;

namespace TextProc2
{
    class Program
    {
        static void Main(string[] args)
        {
            Regex multiWhite = new Regex(@"\s+");
            Regex dateEx = new Regex(@"^\d{4}-\d{2}-\d{2}$");
            Regex valEx = new Regex(@"^\d+\.{1}\d{3}$");
            Regex flagEx = new Regex(@"^[1-9]{1}$");

            int missformcount = 0, totalcount = 0;
            Dictionary<int, string> dates = new Dictionary<int, string>();

            using (StreamReader sr = new StreamReader("readings.txt"))
            {
                string line = sr.ReadLine();
                while (line != null)
                {
                    line = multiWhite.Replace(line, @" ");
                    string[] splitLine = line.Split(' ');
                    if (splitLine.Length != 49)
                        missformcount++;
                    if (!dateEx.IsMatch(splitLine[0]))
                        missformcount++;
                    else
                        dates.Add(totalcount + 1, dateEx.Match(splitLine[0]).ToString());
                    int err = 0;
                    for (int i = 1; i < splitLine.Length; i++)
                    {
                        if (i%2 != 0)
                        {
                            if (!valEx.IsMatch(splitLine[i]))
                                err++;
                        }
                        else
                        {
                            if (!flagEx.IsMatch(splitLine[i]))
                                err++;
                        }
                    }
                    if (err != 0) missformcount++;
                    line = sr.ReadLine();
                    totalcount++;
                }
            }

            int goodEntries = totalcount - missformcount;
            Dictionary<string,List<int>> dateReverse = new Dictionary<string,List<int>>();

            foreach (KeyValuePair<int, string> kvp in dates)
            {
                if (!dateReverse.ContainsKey(kvp.Value))
                    dateReverse[kvp.Value] = new List<int>();
                dateReverse[kvp.Value].Add(kvp.Key);
            }

            Console.WriteLine(goodEntries + " valid Records out of " + totalcount);

            foreach (KeyValuePair<string, List<int>> kvp in dateReverse)
            {
                if (kvp.Value.Count > 1)
                    Console.WriteLine("{0} is duplicated at Lines : {1}", kvp.Key, string.Join(",", kvp.Value));
            }
        }
    }
}
```



```txt

5017 valid Records out of 5471
1990-03-25 is duplicated at Lines : 84,85
1991-03-31 is duplicated at Lines : 455,456
1992-03-29 is duplicated at Lines : 819,820
1993-03-28 is duplicated at Lines : 1183,1184
1995-03-26 is duplicated at Lines : 1910,1911

```



## COBOL

{{works with|OpenCOBOL}}

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. text-processing-2.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT readings ASSIGN Input-File-Path
               ORGANIZATION LINE SEQUENTIAL
               FILE STATUS file-status.

       DATA DIVISION.
       FILE SECTION.
       FD  readings.
       01  reading-record.
           03  date-stamp          PIC X(10).
           03  FILLER              PIC X.
           03  input-data          PIC X(300).

       LOCAL-STORAGE SECTION.
       78  Input-File-Path         VALUE "readings.txt".
       78  Num-Data-Points         VALUE 48.

       01  file-status             PIC XX.

       01  current-line            PIC 9(5).

       01  num-date-stamps-read    PIC 9(5).
       01  read-date-stamps-area.
           03  read-date-stamps    PIC X(10) OCCURS 1 TO 10000 TIMES
                                   DEPENDING ON num-date-stamps-read
                                   INDEXED BY date-stamp-idx.

       01  offset                  PIC 999.
       01  data-len                PIC 999.
       01  data-flag               PIC X.
           88  data-not-found      VALUE "N".

       01  data-field              PIC X(25).

       01  i                       PIC 99.

       01  num-good-readings       PIC 9(5).

       01  reading-flag            PIC X.
           88 bad-reading          VALUE "B".

       01  delim                   PIC X.

       PROCEDURE DIVISION.
       DECLARATIVES.
       readings-error SECTION.
           USE AFTER ERROR ON readings

           DISPLAY "An error occurred while using " Input-File-Path
           DISPLAY "Error code " file-status
           DISPLAY "The program will terminate."

           CLOSE readings
           GOBACK
           .
       END DECLARATIVES.

       main-line.
           OPEN INPUT readings

           *> Process each line of the file.
           PERFORM FOREVER
               READ readings
                   AT END
                       EXIT PERFORM
               END-READ

               ADD 1 TO current-line

               IF reading-record = SPACES
                   DISPLAY "Line " current-line " is blank."
                   EXIT PERFORM CYCLE
               END-IF

               PERFORM check-duplicate-date-stamp

               *> Check there are 24 data pairs and see if all the
               *> readings are ok.
               INITIALIZE offset, reading-flag, data-flag
               PERFORM VARYING i FROM 1 BY 1 UNTIL Num-Data-Points < i
                   PERFORM get-next-field
                   IF data-not-found
                       DISPLAY "Line " current-line " has missing "
                           "fields."
                       SET bad-reading TO TRUE
                       EXIT PERFORM
                   END-IF

                   *> Every other data field is the instrument flag.
                   IF FUNCTION MOD(i, 2) = 0 AND NOT bad-reading
                       IF FUNCTION NUMVAL(data-field) <= 0
                           SET bad-reading TO TRUE
                       END-IF
                   END-IF

                   ADD data-len TO offset
               END-PERFORM

               IF NOT bad-reading
                   ADD 1 TO num-good-readings
               END-IF
           END-PERFORM

           CLOSE readings

           *> Display results.
           DISPLAY SPACE
           DISPLAY current-line " lines read."
           DISPLAY num-good-readings " have good readings for all "
               "instruments."

           GOBACK
           .
       check-duplicate-date-stamp.
           SEARCH read-date-stamps
               AT END
                   ADD 1 TO num-date-stamps-read
                   MOVE date-stamp
                       TO read-date-stamps (num-date-stamps-read)

               WHEN read-date-stamps (date-stamp-idx) = date-stamp
                   DISPLAY "Date " date-stamp " is duplicated at "
                       "line " current-line "."
           END-SEARCH
           .
       get-next-field.
           INSPECT input-data (offset:) TALLYING offset
               FOR LEADING X"09"

           *> The fields are normally delimited by a tab.
           MOVE X"09" TO delim
           PERFORM find-num-chars-before-delim

           *> If the delimiter was not found...
           IF FUNCTION SUM(data-len, offset) > 300
               *> The data may be delimited by a space if it is at the
               *> end of the line.
               MOVE SPACE TO delim
               PERFORM find-num-chars-before-delim

               IF FUNCTION SUM(data-len, offset) > 300
                   SET data-not-found TO TRUE
                   EXIT PARAGRAPH
               END-IF
           END-IF

           IF data-len = 0
               SET data-not-found TO TRUE
               EXIT PARAGRAPH
           END-IF

           MOVE input-data (offset:data-len) TO data-field
           .
       find-num-chars-before-delim.
           INITIALIZE data-len
           INSPECT input-data (offset:) TALLYING data-len
               FOR CHARACTERS BEFORE delim
           .
```


{{out}}

```txt

Date 1990-03-25 is duplicated at line 00084.
Date 1991-03-31 is duplicated at line 00455.
Date 1992-03-29 is duplicated at line 00819.
Date 1993-03-28 is duplicated at line 01183.
Date 1995-03-26 is duplicated at line 01910.

05470 lines read.
05016 have good readings for all instruments.

```



## D


```d
void main() {
    import std.stdio, std.array, std.string, std.regex, std.conv,
           std.algorithm;

    auto rxDate = `^\d\d\d\d-\d\d-\d\d$`.regex;
    // Works but eats lot of RAM in DMD 2.064.
    // auto rxDate = ctRegex!(`^\d\d\d\d-\d\d-\d\d$`);

    int[string] repeatedDates;
    int goodReadings;
    foreach (string line; "readings.txt".File.lines) {
        try {
            auto parts = line.split;
            if (parts.length != 49)
                throw new Exception("Wrong column count");
            if (parts[0].match(rxDate).empty)
                throw new Exception("Date is wrong");
            repeatedDates[parts[0]]++;
            bool noProblem = true;
            for (int i = 1; i < 48; i += 2) {
                if (parts[i + 1].to!int < 1)
                    // don't break loop because it's validation too.
                    noProblem = false;
                if (!parts[i].isNumeric)
                    throw new Exception("Reading is wrong: "~parts[i]);
            }
            if (noProblem)
                goodReadings++;
        } catch(Exception ex) {
            writefln(`Problem in line "%s": %s`, line, ex);
        }
    }

    writefln("Duplicated timestamps: %-(%s, %)",
            repeatedDates.byKey.filter!(k => repeatedDates[k] > 1));
    writeln("Good reading records: ", goodReadings);
}
```

{{out}}

```txt
Duplicated timestamps: 1990-03-25, 1991-03-31, 1992-03-29, 1993-03-28, 1995-03-26
Good reading records: 5017
```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature

	make
			-- Finds double date stamps and wrong formats.
		local
			found: INTEGER
			double: STRING
		do
			read_wordlist
			fill_hash_table
			across
				hash as h
			loop
				if h.key.has_substring ("_double") then
					io.put_string ("Double date stamp: %N")
					double := h.key
					double.remove_tail (7)
					io.put_string (double)
					io.new_line
				end
				if h.item.count /= 24 then
					io.put_string (h.key.out + " has the wrong format. %N")
					found := found + 1
				end
			end
			io.put_string (found.out + " records have not 24 readings.%N")
			good_records
		end

	good_records
			-- Number of records that have flag values > 0 for all readings.
		local
			count, total: INTEGER
			end_date: STRING
		do
			create end_date.make_empty
			across
				hash as h
			loop
				count := 0
				across
					h.item as d
				loop
					if d.item.flag > 0 then
						count := count + 1
					end
				end
				if count = 24 then
					total := total + 1
				end
			end
			io.put_string ("%NGood records: " + total.out + ". %N")
		end

	original_list: STRING = "readings.txt"

	read_wordlist
			--Preprocesses data in 'data'.
		local
			l_file: PLAIN_TEXT_FILE
		do
			create l_file.make_open_read_write (original_list)
			l_file.read_stream (l_file.count)
			data := l_file.last_string.split ('%N')
			l_file.close
		end

	data: LIST [STRING]

	fill_hash_table
			--Fills 'hash' using the date as key.
		local
			by_dates: LIST [STRING]
			date: STRING
			data_tup: TUPLE [val: REAL; flag: INTEGER]
			data_arr: ARRAY [TUPLE [val: REAL; flag: INTEGER]]
			i: INTEGER
		do
			create hash.make (data.count)
			across
				data as d
			loop
				if not d.item.is_empty then
					by_dates := d.item.split ('%T')
					date := by_dates [1]
					by_dates.prune (date)
					create data_tup
					create data_arr.make_empty
					from
						i := 1
					until
						i > by_dates.count - 1
					loop
						data_tup := [by_dates [i].to_real, by_dates [i + 1].to_integer]
						data_arr.force (data_tup, data_arr.count + 1)
						i := i + 2
					end
					hash.put (data_arr, date)
					if not hash.inserted then
						date.append ("_double")
						hash.put (data_arr, date)
					end
				end
			end
		end

	hash: HASH_TABLE [ARRAY [TUPLE [val: REAL; flag: INTEGER]], STRING]

end

```

{{out}}

```txt

Double date stamp:
1990-03-25
Double date stamp:
1991-03-31
Double date stamp:
1992-03-29
Double date stamp:
1993-03-28
Double date stamp:
1995-03-26
0 records have not 24 readings.

Good records: 5017.

```



## Erlang

Uses function from [[Text_processing/1]]. It does some correctness checks for us.

```Erlang

-module( text_processing2 ).

-export( [task/0] ).

task() ->
	Name = "priv/readings.txt",
	try
	File_contents = text_processing:file_contents( Name ),
	[correct_field_format(X) || X<- File_contents],
	{_Previous, Duplicates} = lists:foldl( fun date_duplicates/2, {"", []}, File_contents ),
	io:fwrite( "Duplicates: ~p~n", [Duplicates] ),
	Good = [X || X <- File_contents, is_all_good_readings(X)],
	io:fwrite( "Good readings: ~p~n", [erlang:length(Good)] )

	catch
	_:Error ->
		io:fwrite( "Error: Failed when checking ~s: ~p~n", [Name, Error] )
	end.



correct_field_format( {_Date, Value_flags} ) ->
	Corret_number = value_flag_records(),
	{correct_field_format, Corret_number} = {correct_field_format, erlang:length(Value_flags)}.

date_duplicates( {Date, _Value_flags}, {Date, Acc} ) -> {Date, [Date | Acc]};
date_duplicates( {Date, _Value_flags}, {_Other, Acc} ) -> {Date, Acc}.

is_all_good_readings( {_Date, Value_flags} ) -> value_flag_records() =:= erlang:length( [ok || {_Value, ok} <-  Value_flags] ).

value_flag_records() -> 24.

```

{{out}}

```txt

12> text_processing2:task().
Duplicates: ["1995-03-26","1993-03-28","1992-03-29","1991-03-31","1990-03-25"]
Good readings: 5017

```


=={{header|F Sharp|F#}}==

```fsharp

let file = @"readings.txt"

let dates = HashSet(HashIdentity.Structural)
let mutable ok = 0

do
  for line in System.IO.File.ReadAllLines file do
    match String.split [' '; '\t'] line with
    | [] -> ()
    | date::xys ->
        if dates.Contains date then
          printf "Date %s is duplicated\n" date
        else
          dates.Add date
        let f (b, t) h = not b, if b then int h::t else t
        let _, states = Seq.fold f (false, []) xys
        if Seq.forall (fun s -> s >= 1) states then
          ok <- ok + 1
  printf "%d records were ok\n" ok

```

Prints:

```fsharp

Date 1990-03-25 is duplicated
Date 1991-03-31 is duplicated
Date 1992-03-29 is duplicated
Date 1993-03-28 is duplicated
Date 1995-03-26 is duplicated
5017 records were ok

```


## Fortran

The trouble with the dates rather suggests that they should be checked for correctness in themselves, and that the sequence check should be that each new record advances the date by one day. Daynumber calculations were long ago presented by H. F. Fliegel and T.C. van Flandern, in Communications of the ACM, Vol. 11, No. 10 (October, 1968).

Rather than copy today's data to a PDATA holder so that on the next read the new data may be compared to the old, a two-row array is used, with IT flip-flopping 1,2,1,2,1,2,... Comparison of the data as numerical values rather than text strings means that different texts that evoke the same value will not be regarded as different. If the data format were invalid, there would be horrible messages. There aren't, so ... the values should be read and plotted...


```Fortran

Crunches a set of hourly data. Starts with a date, then 24 pairs of value,indicator for that day, on one line.
      INTEGER Y,M,D		!Year, month, and day.
      INTEGER GOOD(24,2)	!The indicators.
      REAL*8     V(24,2)	!The grist.
      CHARACTER*10 DATE(2)	!Along with the starting date.
      INTEGER IT,TI		!A flipper and its antiflipper.
      INTEGER NV		!Number of entirely good records.
      INTEGER I,NREC,HIC	!Some counters.
      LOGICAL INGOOD		!State flipper for the runs of data.
      INTEGER IN,MSG		!I/O mnemonics.
      CHARACTER*666 ACARD	!Scratchpad, of sufficient length for all expectation.
      IN = 10		!Unit number for the input file.
      MSG = 6		!Output.
      OPEN (IN,FILE="Readings1.txt", FORM="FORMATTED",	!This should be a function.
     1 STATUS ="OLD",ACTION="READ")			!Returning success, or failure.
      NV = 0		!No pure records seen.
      NREC = 0		!No records read.
      HIC = 0		!Provoking no complaints.
      DATE = "snargle"	!No date should look like this!
      IT = 2		!Syncopation for the 1-2 flip flop.
Chew into the file.
   10 READ (IN,11,END=100,ERR=666) L,ACARD(1:MIN(L,LEN(ACARD)))	!With some protection.
      NREC = NREC + 1		!So, a record has been read.
   11 FORMAT (Q,A)		!Obviously, Q ascertains the length of the record being read.
      READ (ACARD,12,END=600,ERR=601) Y,M,D	!The date part is trouble, as always.
   12 FORMAT (I4,2(1X,I2))				!Because there are no delimiters between the parts.
      TI = IT			!Thus finger the previous value.
      IT = 3 - IT		!Flip between 1 and 2.
      DATE(IT) = ACARD(1:10)	!Save the date field.
      READ (ACARD(11:L),*,END=600,ERR=601) (V(I,IT),GOOD(I,IT),I = 1,24)	!But after the date, delimiters abound.
Comparisons. Should really convert the date to a daynumber, check it by reversion, and then check for + 1 day only.
   20 IF (DATE(IT).EQ.DATE(TI)) THEN	!Same date?
        IF (ALL(V(:,IT)   .EQ.V(:,TI)) .AND.	!Yes. What about the data?
     1      ALL(GOOD(:,IT).EQ.GOOD(:,TI))) THEN	!This disregards details of the spacing of the data.
          WRITE (MSG,21) NREC,DATE(IT),"same."	!Also trailing zeroes, spurious + signs, blah blah.
   21     FORMAT ("Record",I8," Duplicate date field (",A,"), data ",A)	!Say it.
         ELSE				!But if they're not all equal,
          WRITE (MSG,21) NREC,DATE(IT),"different!"	!They're different!
        END IF					!So much for comparing the data.
      END IF				!So much for just comparing the date's text.
      IF (ALL(GOOD(:,IT).GT.0)) NV = NV + 1	!A fully healthy record, either way?
      GO TO 10		!More! More! I want more!!

Complaints. Should really distinguish between trouble in the date part and in the data part.
  600 WRITE (MSG,*) '"END" declared - insufficient data?'	!Not enough numbers, presumably.
      GO TO 602				!Reveal the record.
  601 WRITE (MSG,*) '"ERR" declared - improper number format?'	!Ah, but which number?
  602 WRITE (MSG,603) NREC,L,ACARD(1:L)	!Anyway, reveal the uninterpreted record.
  603 FORMAT("Record",I8,", length ",I0," reads ",A)	!Just so.
      HIC = HIC + 1			!This may grow into a habit.
      IF (HIC.LE.12) GO TO 10		!But if not yet, try the next record.
      STOP "Enough distaste."		!Or, give up.
  666 WRITE (MSG,101) NREC,"format error!"	!For A-style data? Should never happen!
      GO TO 900				!But if it does, give up!

Closedown.
  100 WRITE (MSG,101) NREC,"then end-of-file"	!Discovered on the next attempt.
  101 FORMAT ("Record",I8,": ",A)		!A record number plus a remark.
      WRITE (MSG,102) NV	!The overall results.
  102 FORMAT ("  with",I8," having all values good.")	!This should do.
  900 CLOSE(IN)		!Done.
      END	!Spaghetti rules.

```


Output:
 Record      85 Duplicate date field (1990-03-25), data different!
 Record     456 Duplicate date field (1991-03-31), data different!
 Record     820 Duplicate date field (1992-03-29), data different!
 Record    1184 Duplicate date field (1993-03-28), data different!
 Record    1911 Duplicate date field (1995-03-26), data different!
 Record    5471: then end-of-file
   with    5017 having all values good.


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

const (
	filename   = "readings.txt"
	readings   = 24             // per line
	fields     = readings*2 + 1 // per line
	dateFormat = "2006-01-02"
)

func main() {
	file, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()
	var allGood, uniqueGood int
	// map records not only dates seen, but also if an all-good record was
	// seen for the key date.
	m := make(map[time.Time]bool)
	s := bufio.NewScanner(file)
	for s.Scan() {
		f := strings.Fields(s.Text())
		if len(f) != fields {
			log.Fatal("unexpected format,", len(f), "fields.")
		}
		ts, err := time.Parse(dateFormat, f[0])
		if err != nil {
			log.Fatal(err)
		}
		good := true
		for i := 1; i < fields; i += 2 {
			flag, err := strconv.Atoi(f[i+1])
			if err != nil {
				log.Fatal(err)
			}
			if flag > 0 { // value is good
				_, err := strconv.ParseFloat(f[i], 64)
				if err != nil {
					log.Fatal(err)
				}
			} else { // value is bad
				good = false
			}
		}
		if good {
			allGood++
		}
		previouslyGood, seen := m[ts]
		if seen {
			fmt.Println("Duplicate datestamp:", f[0])
		}
		m[ts] = previouslyGood || good
		if !previouslyGood && good {
			uniqueGood++
		}
	}
	if err := s.Err(); err != nil {
		log.Fatal(err)
	}

	fmt.Println("\nData format valid.")
	fmt.Println(allGood, "records with good readings for all instruments.")
	fmt.Println(uniqueGood,
		"unique dates with good readings for all instruments.")
}
```

{{out}}

```txt

Duplicate datestamp: 1990-03-25
Duplicate datestamp: 1991-03-31
Duplicate datestamp: 1992-03-29
Duplicate datestamp: 1993-03-28
Duplicate datestamp: 1995-03-26

Data format valid.
5017 records with good readings for all instruments.
5013 unique dates with good readings for all instruments.

```



## Haskell


```haskell

import Data.List (nub, (\\))

data Record = Record {date :: String, recs :: [(Double, Int)]}

duplicatedDates rs = rs \\ nub rs

goodRecords = filter ((== 24) . length . filter ((>= 1) . snd) . recs)

parseLine l = let ws = words l in Record (head ws) (mapRecords (tail ws))

mapRecords [] = []
mapRecords [_] = error "invalid data"
mapRecords (value:flag:tail) = (read value, read flag) : mapRecords tail

main = do
  inputs <- (map parseLine . lines) `fmap` readFile "readings.txt"
  putStr (unlines ("duplicated dates:": duplicatedDates (map date inputs)))
  putStrLn ("number of good records: " ++ show (length $ goodRecords inputs))

```


this script outputs:
 duplicated dates:
 1990-03-25
 1991-03-31
 1992-03-29
 1993-03-28
 1995-03-26
 number of good records: 5017

=={{header|Icon}} and {{header|Unicon}}==

The following works in both languages.  It assumes there is nothing wrong with
duplicated timestamps that are on well-formed records.


```unicon
procedure main(A)
    dups := set()
    goodRecords := 0
    lastDate := badFile := &null
    f := A[1] | "readings.txt"
    fin := open(f) | stop("Cannot open file '",f,"'")

    while (fields := 0, badReading := &null, line := read(fin)) do {
        line ? {
            ldate := tab(many(&digits ++ '-')) | (badFile := "yes", next)
            if \lastDate == ldate then insert(dups, ldate)
            lastDate := ldate
            while tab(many(' \t')) do {
                (value := real(tab(many(&digits++'-.'))),
                 tab(many(' \t')),
                 flag := integer(tab(many(&digits++'-'))),
                 fields +:= 1) | (badFile := "yes")
                if flag < 1 then badReading := "yes"
                }
            }
        if fields = 24 then goodRecords +:= (/badReading, 1)
        else badFile := "yes"
        }

    if (\badFile) then write(f," has field format issues.")
    write("There are ",goodRecords," records with all good readings.")
    if *dups > 0 then {
        write("The following dates have multiple records:")
        every writes(" ",!sort(dups))
        write()
        }

end
```


Sample run:


```txt

->tp2
There are 5017 records with all good readings.
The following dates have multiple records:
 1990-03-25 1991-03-31 1992-03-29 1993-03-28 1995-03-26
->

```



## J


```j
   require 'tables/dsv dates'
   dat=: TAB readdsv jpath '~temp/readings.txt'
   Dates=: getdate"1 >{."1 dat
   Vals=:  _99 ". >(1 + +: i.24){"1 dat
   Flags=: _99 ". >(2 + +: i.24){"1 dat

   # Dates                      NB. Total # lines
5471
   +/ *./"1 ] 0 = Dates         NB. # lines with invalid date formats
0
   +/ _99 e."1 Vals,.Flags      NB. # lines with invalid value or flag formats
0
   +/ *./"1   [0 < Flags        NB. # lines with only valid flags
5017
   ~. (#~ (i.~ ~: i:~)) Dates   NB. Duplicate dates
1990 3 25
1991 3 31
1992 3 29
1993 3 28
1995 3 26
```



## Java

{{trans|C++}}
{{works with|Java|1.5+}}

```java5
import java.util.*;
import java.util.regex.*;
import java.io.*;

public class DataMunging2 {

    public static final Pattern e = Pattern.compile("\\s+");

    public static void main(String[] args) {
        try {
            BufferedReader infile = new BufferedReader(new FileReader(args[0]));
            List<String> duplicates = new ArrayList<String>();
            Set<String> datestamps = new HashSet<String>(); //for the datestamps

            String eingabe;
            int all_ok = 0;//all_ok for lines in the given pattern e
            while ((eingabe = infile.readLine()) != null) {
                String[] fields = e.split(eingabe); //we tokenize on empty fields
                if (fields.length != 49) //we expect 49 fields in a record
                    System.out.println("Format not ok!");
                if (datestamps.add(fields[0])) { //not duplicated
                    int howoften = (fields.length - 1) / 2 ; //number of measurement
                                                             //devices and values
                    for (int n = 1; Integer.parseInt(fields[2*n]) >= 1; n++) {
                        if (n == howoften) {
                            all_ok++ ;
                            break ;
                        }
                    }
                } else {
                    duplicates.add(fields[0]); //first field holds datestamp
                }
            }
            infile.close();
            System.out.println("The following " + duplicates.size() + " datestamps were duplicated:");
            for (String x : duplicates)
                System.out.println(x);
            System.out.println(all_ok + " records were complete and ok!");
        } catch (IOException e) {
            System.err.println("Can't open file " + args[0]);
            System.exit(1);
        }
    }
}
```

The program produces the following output:

```txt

The following 5 datestamps were duplicated:
1990-03-25
1991-03-31
1992-03-29
1993-03-28
1995-03-26
5013 records were complete and ok!

```



## JavaScript

{{works with|JScript}}

```javascript
// wrap up the counter variables in a closure.
function analyze_func(filename) {
    var dates_seen = {};
    var format_bad = 0;
    var records_all = 0;
    var records_good = 0;
    return function() {
        var fh = new ActiveXObject("Scripting.FileSystemObject").openTextFile(filename, 1); // 1 = for reading
        while ( ! fh.atEndOfStream) {
            records_all ++;
            var allOK = true;
            var line = fh.ReadLine();
            var fields = line.split('\t');
            if (fields.length != 49) {
                format_bad ++;
                continue;
            }

            var date = fields.shift();
            if (has_property(dates_seen, date))
                WScript.echo("duplicate date: " + date);
            else
                dates_seen[date] = 1;

            while (fields.length > 0) {
                var value = parseFloat(fields.shift());
                var flag = parseInt(fields.shift(), 10);
                if (isNaN(value) || isNaN(flag)) {
                    format_bad ++;
                }
                else if (flag <= 0) {
                    allOK = false;
                }
            }
            if (allOK)
                records_good ++;
        }
        fh.close();
        WScript.echo("total records: " + records_all);
        WScript.echo("Wrong format: " + format_bad);
        WScript.echo("records with no bad readings: " + records_good);
    }
}

function has_property(obj, propname) {
    return typeof(obj[propname]) == "undefined" ? false : true;
}

var analyze = analyze_func('readings.txt');
analyze();
```


## jq

{{works with|jq|with regex support}}

For this problem, it is convenient to use jq in a pipeline: the first invocation of jq will convert the text file into a stream of JSON arrays (one array per line):

```sh
$ jq -R '[splits("[ \t]+")]' Text_processing_2.txt
```


The second part of the pipeline performs the task requirements. The following program is used in the second invocation of jq.

'''Generic Utilities'''

```jq
# Given any array, produce an array of [item, count] pairs for each run.
def runs:
  reduce .[] as $item
    ( [];
      if . == [] then [ [ $item, 1] ]
      else  .[length-1] as $last
            | if $last[0] == $item then (.[0:length-1] + [ [$item, $last[1] + 1] ] )
              else . + [[$item, 1]]
              end
      end ) ;

def is_float: test("^[-+]?[0-9]*[.][0-9]*([eE][-+]?[0-9]+)?$");

def is_integral: test("^[-+]?[0-9]+$");

def is_date: test("[12][0-9]{3}-[0-9][0-9]-[0-9][0-9]");
```


'''Validation''':

```jq
# Report line and column numbers using conventional numbering (IO=1).
def validate_line(nr):
  def validate_date:
    if is_date then empty else "field 1 in line \(nr) has an invalid date: \(.)" end;
  def validate_length(n):
    if length == n then empty else "line \(nr) has \(length) fields" end;
  def validate_pair(i):
    ( .[2*i + 1] as $n
      | if ($n | is_float) then empty else "field \(2*i + 2) in line \(nr) is not a float: \($n)" end),
    ( .[2*i + 2] as $n
      | if ($n | is_integral) then empty else "field \(2*i + 3) in line \(nr) is not an integer: \($n)" end);

  (.[0] | validate_date),
  (validate_length(49)),
  (range(0; (length-1) / 2) as $i | validate_pair($i)) ;

def validate_lines:
 . as $in
 | range(0; length) as $i | ($in[$i] | validate_line($i + 1));
```


'''Check for duplicate timestamps'''

```jq
def duplicate_timestamps:
  [.[][0]] | sort | runs | map( select(.[1]>1) );
```


'''Number of valid readings for all instruments''':

```jq
# The following ignores any issues with respect to duplicate dates,
# but does check the validity of the record, including the date format:
def number_of_valid_readings:
  def check:
    . as $in
    | (.[0] | is_date)
      and length == 49
      and all(range(0; 24) | $in[2*. + 1] | is_float)
      and all(range(0; 24) | $in[2*. + 2] | (is_integral and tonumber >= 1) );

   map(select(check)) | length ;
```


'''Generate Report'''

```jq
validate_lines,
"\nChecking for duplicate timestamps:",
duplicate_timestamps,
"\nThere are \(number_of_valid_readings) valid rows altogether."
```

{{out}}
'''Part 1: Simple demonstration'''

To illustrate that the program does report invalid lines, we first use the six lines at the top but mangle the last line.

```sh
$ jq -R  '[splits("[ \t]+")]' Text_processing_2.txt | jq -s -r -f  Text_processing_2.jq
field 1 in line 6 has an invalid date: 991-04-03
line 6 has 47 fields
field 2 in line 6 is not a float: 10000
field 3 in line 6 is not an integer: 1.0
field 47 in line 6 is not an integer: x

Checking for duplicate timestamps:
[
  [
    "1991-03-31",
    2
  ]
]

There are 5 valid rows altogether.
```


'''Part 2: readings.txt'''

```sh
$ jq -R  '[splits("[ \t]+")]' readings.txt | jq -s -r -f  Text_processing_2.jq
Checking for duplicate timestamps:
[
  [
    "1990-03-25",
    2
  ],
  [
    "1991-03-31",
    2
  ],
  [
    "1992-03-29",
    2
  ],
  [
    "1993-03-28",
    2
  ],
  [
    "1995-03-26",
    2
  ]
]

There are 5017 valid rows altogether.
```



## Julia

Refer to the code at https://rosettacode.org/wiki/Text_processing/1#Julia. Add at the end of that code the following:

```Julia

dupdate = df[nonunique(df[:,[:Date]]),:][:Date]
println("The following rows have duplicate DATESTAMP:")
println(df[df[:Date] .== dupdate,:])
println("All values good in these rows:")
println(df[df[:ValidValues] .== 24,:])

```

{{output}}

```txt

The following rows have duplicate DATESTAMP:
2×29 DataFrames.DataFrame
│ Row │ Date                │ Mean    │ ValidValues │ MaximumGap │ GapPosition │ 0:00 │ 1:00 │ 2:00 │ 3:00 │ 4:00 │
├─────┼─────────────────────┼─────────┼─────────────┼────────────┼─────────────┼──────┼──────┼──────┼──────┼──────┤
│ 1   │ 1991-03-31T00:00:00 │ 23.5417 │ 24          │ 0          │ 0           │ 10.0 │ 10.0 │ 10.0 │ 10.0 │ 10.0 │
│ 2   │ 1991-03-31T00:00:00 │ 40.0    │ 1           │ 23         │ 2           │ 40.0 │ NaN  │ NaN  │ NaN  │ NaN  │

│ Row │ 5:00 │ 6:00 │ 7:00 │ 8:00 │ 9:00 │ 10:00 │ 11:00 │ 12:00 │ 13:00 │ 14:00 │ 15:00 │ 16:00 │ 17:00 │ 18:00 │
├─────┼──────┼──────┼──────┼──────┼──────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤
│ 1   │ 10.0 │ 10.0 │ 20.0 │ 20.0 │ 20.0 │ 35.0  │ 50.0  │ 60.0  │ 40.0  │ 30.0  │ 30.0  │ 30.0  │ 25.0  │ 20.0  │
│ 2   │ NaN  │ NaN  │ NaN  │ NaN  │ NaN  │ NaN   │ NaN   │ NaN   │ NaN   │ NaN   │ NaN   │ NaN   │ NaN   │ NaN   │

│ Row │ 19:00 │ 20:00 │ 21:00 │ 22:00 │ 23:00 │
├─────┼───────┼───────┼───────┼───────┼───────┤
│ 1   │ 20.0  │ 20.0  │ 20.0  │ 20.0  │ 35.0  │
│ 2   │ NaN   │ NaN   │ NaN   │ NaN   │ NaN   │
All values good in these rows:
4×29 DataFrames.DataFrame
│ Row │ Date                │ Mean    │ ValidValues │ MaximumGap │ GapPosition │ 0:00 │ 1:00 │ 2:00 │ 3:00 │ 4:00 │
├─────┼─────────────────────┼─────────┼─────────────┼────────────┼─────────────┼──────┼──────┼──────┼──────┼──────┤
│ 1   │ 1991-03-30T00:00:00 │ 10.0    │ 24          │ 0          │ 0           │ 10.0 │ 10.0 │ 10.0 │ 10.0 │ 10.0 │
│ 2   │ 1991-03-31T00:00:00 │ 23.5417 │ 24          │ 0          │ 0           │ 10.0 │ 10.0 │ 10.0 │ 10.0 │ 10.0 │
│ 3   │ 1991-04-02T00:00:00 │ 19.7917 │ 24          │ 0          │ 0           │ 8.0  │ 9.0  │ 11.0 │ 12.0 │ 12.0 │
│ 4   │ 1991-04-03T00:00:00 │ 13.9583 │ 24          │ 0          │ 0           │ 10.0 │ 9.0  │ 10.0 │ 10.0 │ 9.0  │

│ Row │ 5:00 │ 6:00 │ 7:00 │ 8:00 │ 9:00 │ 10:00 │ 11:00 │ 12:00 │ 13:00 │ 14:00 │ 15:00 │ 16:00 │ 17:00 │ 18:00 │
├─────┼──────┼──────┼──────┼──────┼──────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤
│ 1   │ 10.0 │ 10.0 │ 10.0 │ 10.0 │ 10.0 │ 10.0  │ 10.0  │ 10.0  │ 10.0  │ 10.0  │ 10.0  │ 10.0  │ 10.0  │ 10.0  │
│ 2   │ 10.0 │ 10.0 │ 20.0 │ 20.0 │ 20.0 │ 35.0  │ 50.0  │ 60.0  │ 40.0  │ 30.0  │ 30.0  │ 30.0  │ 25.0  │ 20.0  │
│ 3   │ 12.0 │ 27.0 │ 26.0 │ 27.0 │ 33.0 │ 32.0  │ 31.0  │ 29.0  │ 31.0  │ 25.0  │ 25.0  │ 24.0  │ 21.0  │ 17.0  │
│ 4   │ 10.0 │ 15.0 │ 24.0 │ 28.0 │ 24.0 │ 18.0  │ 14.0  │ 12.0  │ 13.0  │ 14.0  │ 15.0  │ 14.0  │ 15.0  │ 13.0  │

│ Row │ 19:00 │ 20:00 │ 21:00 │ 22:00 │ 23:00 │
├─────┼───────┼───────┼───────┼───────┼───────┤
│ 1   │ 10.0  │ 10.0  │ 10.0  │ 10.0  │ 10.0  │
│ 2   │ 20.0  │ 20.0  │ 20.0  │ 20.0  │ 35.0  │
│ 3   │ 14.0  │ 15.0  │ 12.0  │ 12.0  │ 10.0  │
│ 4   │ 13.0  │ 13.0  │ 12.0  │ 10.0  │ 10.0  │

```



## Kotlin


```scala
// version 1.2.31

import java.io.File

fun main(args: Array<String>) {
    val rx = Regex("""\s+""")
    val file = File("readings.txt")
    var count = 0
    var invalid = 0
    var allGood = 0
    var map = mutableMapOf<String, Int>()
    file.forEachLine { line ->
        count++
        val fields = line.split(rx)
        val date = fields[0]
        if (fields.size == 49) {
            if (map.containsKey(date))
                map[date] = map[date]!! + 1
            else
                map.put(date, 1)
            var good = 0
            for (i in 2 until fields.size step 2) {
                if (fields[i].toInt() >= 1) {
                    good++
                }
            }
            if (good == 24) allGood++
        }
        else invalid++
    }

    println("File = ${file.name}")
    println("\nDuplicated dates:")
    for ((k,v) in map) {
        if (v > 1) println("  $k ($v times)")
    }
    println("\nTotal number of records   : $count")
    var percent = invalid.toDouble() / count * 100.0
    println("Number of invalid records : $invalid (${"%5.2f".format(percent)}%)")
    percent = allGood.toDouble() / count * 100.0
    println("Number which are all good : $allGood (${"%5.2f".format(percent)}%)")
}
```


{{out}}

```txt

File = readings.txt

Duplicated dates:
  1990-03-25 (2 times)
  1991-03-31 (2 times)
  1992-03-29 (2 times)
  1993-03-28 (2 times)
  1995-03-26 (2 times)

Total number of records   : 5471
Number of invalid records : 0 ( 0.00%)
Number which are all good : 5017 (91.70%)

```



## Lua


```lua
filename = "readings.txt"
io.input( filename )

dates = {}
duplicated, bad_format = {}, {}
num_good_records, lines_total = 0, 0

while true do
    line = io.read( "*line" )
    if line == nil then break end

    lines_total = lines_total + 1

    date = string.match( line, "%d+%-%d+%-%d+" )
    if dates[date] ~= nil then
        duplicated[#duplicated+1] = date
    end
    dates[date] = 1

    count_pairs, bad_values = 0, false
    for v, w in string.gmatch( line, "%s(%d+[%.%d+]*)%s(%-?%d)" ) do
        count_pairs = count_pairs + 1
        if tonumber(w) <= 0 then
            bad_values = true
        end
    end
    if count_pairs ~= 24 then
        bad_format[#bad_format+1] = date
    end
    if not bad_values then
        num_good_records = num_good_records + 1
    end
end

print( "Lines read:", lines_total )
print( "Valid records: ", num_good_records )
print( "Duplicate dates:" )
for i = 1, #duplicated do
    print( "   ", duplicated[i] )
end
print( "Bad format:" )
for i = 1, #bad_format do
    print( "   ", bad_format[i] )
end
```

Output:

```txt
Lines read:	5471
Valid records: 	5017
Duplicate dates:
   	1990-03-25
   	1991-03-31
   	1992-03-29
   	1993-03-28
   	1995-03-26
Bad format:


```



## Mathematica


```Mathematica
data = Import["Readings.txt","TSV"]; Print["duplicated dates: "];
Select[Tally@data[[;;,1]], #[[2]]>1&][[;;,1]]//Column
Print["number of good records: ", Count[(Times@@#[[3;;All;;2]])& /@ data, 1],
" (out of a total of ", Length[data], ")"]
```



```txt
duplicated dates:
1990-03-25
1991-03-31
1992-03-29
1993-03-28
1995-03-26

number of good records: 5017 (out of a total of 5471)
```


=={{header|MATLAB}} / {{header|Octave}}==


```MATLAB
function [val,count] = readdat(configfile)
% READDAT reads readings.txt file
%
% The value of boolean parameters can be tested with
%    exist(parameter,'var')

if nargin<1,
   filename = 'readings.txt';
end;

fid = fopen(filename);
if fid<0, error('cannot open file %s\n',a); end;
[val,count] = fscanf(fid,'%04d-%02d-%02d %f %d %f %d %f %d %f %d %f %d %f %d %f %d %f %d %f %d %f %d %f %d %f %d %f %d %f %d %f %d %f %d %f %d %f %d %f %d %f %d %f %d %f %d %f %d %f %d \n');
fclose(fid);

count = count/51;

if (count<1) || count~=floor(count),
     error('file has incorrect format\n')
end;

val = reshape(val,51,count)';   % make matrix with 51 rows and count columns, then transpose it.

d = datenum(val(:,1:3));	% compute timestamps

printf('The following records are followed by a duplicate:');
dix = find(diff(d)==0)		% check for to consequtive timestamps with zero difference

printf('number of valid records: %i\n ', sum( all( val(:,5:2:end) >= 1, 2) ) );
```



```txt
>> [val,count]=readdat;
The following records are followed by a duplicate:dix =

     84
    455
    819
   1183
   1910

number of valid records: 5017

```



## Nim


```Nim

import strutils, tables

const NumFields = 49
const DateField = 0
const FlagGoodValue = 1

var badRecords: int # the number of records that have invalid formatted values
var totalRecords: int # the total number of records in the file
var badInstruments: int   # the total number of records that have at least one instrument showing error
var seenDates = newTable[string,bool]() # table that keeps track of what dates we have seen

# ensure we can parse all records as floats (except the date stamp)
proc checkFloats(floats:seq[string]): bool =
  for index in 1..NumFields-1:
    try:
      # we're assuming all instrument flags are floats not integers
      discard parseFloat(floats[index])
    except ValueError:
      return false
  true

# ensure that all sensor flags are ok
proc areAllFlagsOk(instruments: seq[string]): bool =
  #flags start at index 2, and occur every 2 fields
  for index in countup(2,NumFields,2):
    # we're assuming all instrument flags are floats not integers
    var flag = parseFloat(instruments[index])
    if flag < FlagGoodValue: return false
  true


# Note: we're not checking the format of the date stamp

# main
var lines = readFile("readings.txt")
var currentLine: int

for line in lines.splitLines:
  currentLine.inc
  #empty lines don't count as records
  if line.len == 0: continue

  var tokens = line.split({' ','\t'})

  totalRecords.inc

  if tokens.len != NumFields:
    badRecords.inc
    continue

  if not checkFloats(tokens):
    badRecords.inc
    continue

  if not areAllFlagsOk(tokens):
    badInstruments.inc

  if seenDates.hasKeyOrPut(tokens[DateField], true):
    echo tokens[DateField], " duplicated on line ", currentLine

var goodRecords = totalRecords - badRecords
var goodInstruments = goodRecords - badInstruments

echo "Total Records:", totalRecords
echo "Good Records:", goodRecords
echo "Records where all instuments were OK:", goodInstruments

```



## OCaml


```ocaml
#load "str.cma"
open Str

let strip_cr str =
  let last = pred (String.length str) in
  if str.[last] <> '\r' then str else String.sub str 0 last

let map_records =
  let rec aux acc = function
    | value::flag::tail ->
        let e = (float_of_string value, int_of_string flag) in
        aux (e::acc) tail
    | [_] -> invalid_arg "invalid data"
    | [] -> List.rev acc
  in
  aux [] ;;

let duplicated_dates =
  let same_date (d1,_) (d2,_) = (d1 = d2) in
  let date (d,_) = d in
  let rec aux acc = function
    | a::b::tl when same_date a b ->
        aux (date a::acc) tl
    | _::tl ->
        aux acc tl
    | [] ->
        List.rev acc
  in
  aux [] ;;

let record_ok (_,record) =
  let is_ok (_,v) = v >= 1 in
  let sum_ok =
    List.fold_left (fun sum this ->
      if is_ok this then succ sum else sum) 0 record
  in
  sum_ok = 24

let num_good_records =
  List.fold_left  (fun sum record ->
    if record_ok record then succ sum else sum) 0 ;;

let parse_line line =
  let li = split (regexp "[ \t]+") line in
  let records = map_records (List.tl li)
  and date = List.hd li in
  (date, records)

let () =
  let ic = open_in "readings.txt" in
  let rec read_loop acc =
    let line_opt = try Some (strip_cr (input_line ic))
                   with End_of_file -> None
    in
    match line_opt with
      None -> close_in ic; List.rev acc
    | Some line -> read_loop (parse_line line :: acc)
  in
  let inputs = read_loop [] in

  Printf.printf "%d total lines\n" (List.length inputs);

  Printf.printf "duplicated dates:\n";
  let dups = duplicated_dates inputs in
  List.iter print_endline dups;

  Printf.printf "number of good records: %d\n" (num_good_records inputs);
;;
```


this script outputs:

 5471 total lines
 duplicated dates:
 1990-03-25
 1991-03-31
 1992-03-29
 1993-03-28
 1995-03-26
 number of good records: 5017


## Perl


```perl
use List::MoreUtils 'natatime';
use constant FIELDS => 49;

binmode STDIN, ':crlf';
  # Read the newlines properly even if we're not running on
  # Windows.

my ($line, $good_records, %dates) = (0, 0);
while (<>)
   {++$line;
    my @fs = split /\s+/;
    @fs == FIELDS or die "$line: Bad number of fields.\n";
    for (shift @fs)
       {/\d{4}-\d{2}-\d{2}/ or die "$line: Bad date format.\n";
        ++$dates{$_};}
    my $iterator = natatime 2, @fs;
    my $all_flags_okay = 1;
    while ( my ($val, $flag) = $iterator->() )
       {$val =~ /\d+\.\d+/ or die "$line: Bad value format.\n";
        $flag =~ /\A-?\d+/ or die "$line: Bad flag format.\n";
        $flag < 1 and $all_flags_okay = 0;}
    $all_flags_okay and ++$good_records;}

print "Good records: $good_records\n",
   "Repeated timestamps:\n",
   map {"  $_\n"}
   grep {$dates{$_} > 1}
   sort keys %dates;
```


Output:

```txt
Good records: 5017
Repeated timestamps:
  1990-03-25
  1991-03-31
  1992-03-29
  1993-03-28
  1995-03-26
```



## Perl 6

{{trans|Perl}}
{{works with|Rakudo|2018.03}}

This version does validation with a single Perl 6 regex that is much more readable than the typical regex, and arguably expresses the data structure more straightforwardly.
Here we use normal quotes for literals, and <tt>\h</tt> for horizontal whitespace.

Variables like <tt>$good-record</tt> that are going to be autoincremented do not need to be initialized.

The <tt>.push</tt> method on a hash is magical and loses no information; if a duplicate key is found in the pushed pair, an array of values is automatically created of the old value and the new value pushed.  Hence we can easily track all the lines that a particular duplicate occurred at.

The <tt>.all</tt> method does "junctional" logic: it autothreads through comparators as any English speaker would expect.  Junctions can also short-circuit as soon as they find a value that doesn't match, and the evaluation order is up to the computer, so it can be optimized or parallelized.

The final line simply greps out the pairs from the hash whose value is an array with more than 1 element.  (Those values that are not arrays nevertheless have a <tt>.elems</tt> method that always reports <tt>1</tt>.)  The <tt>.pairs</tt> is merely there for clarity; grepping a hash directly has the same effect.
Note that we sort the pairs after we've grepped them, not before; this works fine in Perl 6, sorting on the key and value as primary and secondary keys.  Finally, pairs and arrays provide a default print format that is sufficient without additional formatting in this case.


```perl6
my $good-records;
my $line;
my %dates;

for lines() {
    $line++;
    / ^
    (\d ** 4 '-' \d\d '-' \d\d)
    [ \h+ \d+'.'\d+ \h+ ('-'?\d+) ] ** 24
    $ /
        or note "Bad format at line $line" and next;
    %dates.push: $0 => $line;
    $good-records++ if $1.all >= 1;
}

say "$good-records good records out of $line total";

say 'Repeated timestamps (with line numbers):';
.say for sort %dates.pairs.grep: *.value.elems > 1;
```

Output:

```txt
5017 good records out of 5471 total
Repeated timestamps (with line numbers):
1990-03-25 => [84 85]
1991-03-31 => [455 456]
1992-03-29 => [819 820]
1993-03-28 => [1183 1184]
1995-03-26 => [1910 1911]
```



## Phix


```Phix
sequence lines = read_lines("demo\\rosetta\\readings.txt")

include builtins\timedate.e

integer all_good = 0

string fmt = "%d-%d-%d\t"&join(repeat("%f",48),'\t')
sequence extset = sq_mul(tagset(24),2)  -- {2,4,6,..48}

--The extract routine has been added as a builtin for 0.8.0+. If you don't yet have it, just use this:
--function extract(sequence source, indexes)
--  for i=1 to length(indexes) do
--      indexes[i] = source[indexes[i]]
--  end for
--  return indexes
--end function

for i=1 to length(lines) do
    string li = lines[i]
    sequence r = scanf(li,fmt), this, last
    if length(r)!=1 then
        printf(1,"bad line [%d]:%s\n",{i,li})
    else
        this = r[1][1..3]
        if i>1 and this=last then
            printf(1,"duplicate line for %04d/%02d/%02d\n",last)
        end if
        last = this
        all_good += sum(sq_le(extract(r[1][4..$],extset),0))=0
    end if
end for

printf(1,"Valid records %d of %d total\n",{all_good, length(lines)})
```

{{out}}

```txt

duplicate line for 1990/03/25
duplicate line for 1991/03/31
duplicate line for 1992/03/29
duplicate line for 1993/03/28
duplicate line for 1995/03/26
Valid records 5017 of 5471 total

```



## PHP


```php
$handle = fopen("readings.txt", "rb");
$missformcount = 0;
$totalcount = 0;
$dates = array();
while (!feof($handle)) {
    $buffer = fgets($handle);
	$line = preg_replace('/\s+/',' ',$buffer);
	$line = explode(' ',trim($line));
	$datepattern = '/^\d{4}-\d{2}-\d{2}$/';
	$valpattern = '/^\d+\.{1}\d{3}$/';
	$flagpattern = '/^[1-9]{1}$/';

	if(count($line) != 49) $missformcount++;
	if(!preg_match($datepattern,$line[0],$check)) $missformcount++;
	else $dates[$totalcount+1] = $check[0];

	$errcount = 0;
	for($i=1;$i<count($line);$i++){
		if($i%2!=0){
			if(!preg_match($valpattern,$line[$i],$check)) $errcount++;
		}else{
			if(!preg_match($flagpattern,$line[$i],$check)) $errcount++;
		}
	}
	if($errcount != 0) $missformcount++;
	$totalcount++;
}
fclose ($handle);
$good = $totalcount - $missformcount;
$duplicates = array_diff_key( $dates , array_unique( $dates ));
echo 'Valid records ' . $good . ' of ' . $totalcount . ' total
';
echo 'Duplicates :
';
foreach ($duplicates as $key => $val){
	echo $val . ' at Line : ' . $key . '
';
}
```


```txt
Valid records 5017 of 5471 total
Duplicates :
1990-03-25 at Line : 85
1991-03-31 at Line : 456
1992-03-29 at Line : 820
1993-03-28 at Line : 1184
1995-03-26 at Line : 1911
```



## PL/I


```pli

/* To process readings produced by automatic reading stations. */

check: procedure options (main);
   declare 1 date, 2 (yy, mm, dd) character (2),
           (j1, j2) character (1);
   declare old_date character (6);
   declare line character (330) varying;
   declare R(24) fixed decimal, Machine(24) fixed binary;
   declare (i, k, n, faulty static initial (0)) fixed binary;
   declare input file;

   open file (input) title ('/READINGS.TXT,TYPE(CRLF),RECSIZE(300)');

   on endfile (input) go to done;

   old_date = '';
   k = 0;
   do forever;
      k = k + 1;

      get file (input) edit (line) (L);
      get string(line) edit (yy, j1, mm, j2, dd) (a(4), a(1), a(2), a(1), a(2));

      line = substr(line, 11);

      do i = 1 to length(line);
         if substr(line, i, 1) = '09'x then substr(line, i, 1) = ' ';
      end;
      line = trim(line);
      n = tally(line, ' ') - tally (line, '  ') + 1;

      if n ^= 48 then
         do;
            put skip list ('There are ' || n || ' readings in line ' || k);
         end;

      n = n/2;
      line = line || ' ';

      get string(line) list ((R(i), Machine(i) do i = 1 to n));

      if any(Machine < 1) ^= '0'B then
         faulty = faulty + 1;
      if old_date ^= ' ' then if old_date = string(date) then
         put skip list ('Dates are the same at line' || k);
      old_date = string(date);
   end;
done:
   put skip list ('There were ' || k || ' sets of readings');
   put skip list ('There were ' || faulty || ' faulty readings' );
   put skip list ('There were ' || k-faulty || ' good readings' );
end check;

```



## PicoLisp

Put the following into an executable file "checkReadings":

```PicoLisp
#!/usr/bin/picolisp /usr/lib/picolisp/lib.l

(load "@lib/misc.l")

(in (opt)
   (until (eof)
      (let Lst (split (line) "^I")
         (unless
            (and
               (= 49 (length Lst))     # Check total length
               ($dat (car Lst) "-")    # Check for valid date
               (fully                  # Check data format
                  '((L F)
                     (if F                         # Alternating:
                        (format L 3)               # Number
                        (>= 9 (format L) -9) ) )   # or flag
                  (cdr Lst)
                  '(T NIL .) ) )
            (prinl "Bad line format: " (glue " " Lst))
            (bye 1) ) ) ) )

(bye)
```

Then it can be called as

```txt
$ ./checkReadings readings.txt
```



## PowerShell


```powershell
$dateHash = @{}
$goodLineCount = 0
get-content c:\temp\readings.txt |
    ForEach-Object {
        $line = $_.split(" |`t",2)
        if ($dateHash.containskey($line[0])) {
            $line[0] + " is duplicated"
        } else {
            $dateHash.add($line[0], $line[1])
        }
        $readings = $line[1].split()
        $goodLine = $true
        if ($readings.count -ne 48) { $goodLine = $false; "incorrect line length : $line[0]"  }
        for ($i=0; $i -lt $readings.count; $i++) {
            if ($i % 2 -ne 0) {
                if ([int]$readings[$i] -lt 1) {
                    $goodLine = $false
                }
            }
        }
        if ($goodLine) { $goodLineCount++ }
    }
[string]$goodLineCount + " good lines"

```


Output:

```txt
1990-03-25 is duplicated
1991-03-31 is duplicated
1992-03-29 is duplicated
1993-03-28 is duplicated
1995-03-26 is duplicated
5017
```


An alternative using regular expression syntax:

```powershell

$dateHash = @{}
$goodLineCount = 0
ForEach ($rawLine in ( get-content c:\temp\readings.txt) ){
    $line = $rawLine.split(" |`t",2)
    if ($dateHash.containskey($line[0])) {
        $line[0] + " is duplicated"
    } else {
        $dateHash.add($line[0], $line[1])
    }
    $readings = [regex]::matches($line[1],"\d+\.\d+\s-?\d")
    if ($readings.count -ne 24) { "incorrect number of readings for date " + $line[0] }
    $goodLine = $true
    foreach ($flagMatch in [regex]::matches($line[1],"\d\.\d*\s(?<flag>-?\d)")) {
        if ([int][string]$flagMatch.groups["flag"].value -lt 1) {
            $goodLine = $false
        }
    }
    if ($goodLine) { $goodLineCount++}
}
[string]$goodLineCount + " good lines"

```


Output:

```txt

1990-03-25 is duplicated
1991-03-31 is duplicated
1992-03-29 is duplicated
1993-03-28 is duplicated
1995-03-26 is duplicated
5017 good lines

```


## PureBasic

Using regular expressions.

```PureBasic
Define filename.s = "readings.txt"
#instrumentCount = 24

Enumeration
  #exp_date
  #exp_instruments
  #exp_instrumentStatus
EndEnumeration

Structure duplicate
  date.s
  firstLine.i
  line.i
EndStructure

NewMap dates() ;records line date occurs first
NewList duplicated.duplicate()
NewList syntaxError()
Define goodRecordCount, totalLines, line.s, i
Dim inputDate.s(0)
Dim instruments.s(0)

If ReadFile(0, filename)
  CreateRegularExpression(#exp_date, "\d+-\d+-\d+")
  CreateRegularExpression(#exp_instruments, "(\t|\x20)+(\d+\.\d+)(\t|\x20)+\-?\d")
  CreateRegularExpression(#exp_instrumentStatus, "(\t|\x20)+(\d+\.\d+)(\t|\x20)+")
  Repeat
    line = ReadString(0, #PB_Ascii)
    If line = "": Break: EndIf
    totalLines + 1

    ExtractRegularExpression(#exp_date, line, inputDate())
    If FindMapElement(dates(), inputDate(0))
      AddElement(duplicated())
      duplicated()\date = inputDate(0)
      duplicated()\firstLine = dates()
      duplicated()\line = totalLines
    Else
      dates(inputDate(0)) = totalLines
    EndIf

    ExtractRegularExpression(#exp_instruments, Mid(line, Len(inputDate(0)) + 1), instruments())
    Define pairsCount = ArraySize(instruments()), containsBadValues = #False
    For i =  0 To pairsCount
      If Val(ReplaceRegularExpression(#exp_instrumentStatus, instruments(i), "")) < 1
        containsBadValues = #True
        Break
      EndIf
    Next

    If pairsCount <> #instrumentCount - 1
      AddElement(syntaxError()): syntaxError() = totalLines
    EndIf
    If Not containsBadValues
      goodRecordCount + 1
    EndIf
  ForEver
  CloseFile(0)

  If OpenConsole()
    ForEach duplicated()
      PrintN("Duplicate date: " + duplicated()\date + " occurs on lines " + Str(duplicated()\line) + " and " + Str(duplicated()\firstLine) + ".")
    Next
    ForEach syntaxError()
      PrintN( "Syntax error in line " + Str(syntaxError()))
    Next
    PrintN(#CRLF$ + Str(goodRecordCount) + " of " + Str(totalLines) + " lines read were valid records.")

    Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
    CloseConsole()
  EndIf
EndIf
```

Sample output:

```txt
Duplicate date: 1990-03-25 occurs on lines 85 and 84.
Duplicate date: 1991-03-31 occurs on lines 456 and 455.
Duplicate date: 1992-03-29 occurs on lines 820 and 819.
Duplicate date: 1993-03-28 occurs on lines 1184 and 1183.
Duplicate date: 1995-03-26 occurs on lines 1911 and 1910.

5017 of 5471 lines read were valid records.
```



## Python


```python
import re
import zipfile
import StringIO

def munge2(readings):

   datePat = re.compile(r'\d{4}-\d{2}-\d{2}')
   valuPat = re.compile(r'[-+]?\d+\.\d+')
   statPat = re.compile(r'-?\d+')
   allOk, totalLines = 0, 0
   datestamps = set([])
   for line in readings:
      totalLines += 1
      fields = line.split('\t')
      date = fields[0]
      pairs = [(fields[i],fields[i+1]) for i in range(1,len(fields),2)]

      lineFormatOk = datePat.match(date) and \
         all( valuPat.match(p[0]) for p in pairs ) and \
         all( statPat.match(p[1]) for p in pairs )
      if not lineFormatOk:
         print 'Bad formatting', line
         continue

      if len(pairs)!=24 or any( int(p[1]) < 1 for p in pairs ):
         print 'Missing values', line
         continue

      if date in datestamps:
         print 'Duplicate datestamp', line
         continue
      datestamps.add(date)
      allOk += 1

   print 'Lines with all readings: ', allOk
   print 'Total records: ', totalLines

#zfs = zipfile.ZipFile('readings.zip','r')
#readings = StringIO.StringIO(zfs.read('readings.txt'))
readings = open('readings.txt','r')
munge2(readings)
```

The results indicate 5013 good records, which differs from the Awk implementation. The final few lines of the output are as follows
<pre style="height:10ex;overflow:scroll">
Missing values 2004-12-29	2.900	1	2.700	1	2.800	1	3.300	1	2.900	1	2.300	1	0.000	0	1.700	1	1.900	1	2.300	1	2.600	1	2.900	1	2.600	1	2.600	1	2.600	1	2.700	1	2.300	1	2.200	1	2.100	1	2.000	1	2.100	1	2.100	1	2.300	1	2.400	1

Missing values 2004-12-30	2.400	1	2.600	1	2.600	1	2.600	1	3.000	1	0.000	0	3.300	1	2.600	1	2.900	1	2.400	1	2.300	1	2.900	1	3.500	1	3.700	1	3.600	1	4.000	1	3.400	1	2.400	1	2.500	1	2.600	1	2.600	1	2.800	1	2.400	1	2.200	1

Missing values 2004-12-31	2.400	1	2.500	1	2.500	1	2.400	1	0.000	0	2.400	1	2.400	1	2.400	1	2.200	1	2.400	1	2.500	1	2.000	1	1.700	1	1.400	1	1.500	1	1.900	1	1.700	1	2.000	1	2.000	1	2.200	1	1.700	1	1.500	1	1.800	1	1.800	1

Lines with all readings:  5013
Total records:  5471

```


'''Second Version'''

Modification of the version above to:
* Remove continue statements so it counts as the AWK example does.
* Generate mostly summary information that is easier to compare to other solutions.


```python
import re
import zipfile
import StringIO

def munge2(readings, debug=False):

   datePat = re.compile(r'\d{4}-\d{2}-\d{2}')
   valuPat = re.compile(r'[-+]?\d+\.\d+')
   statPat = re.compile(r'-?\d+')
   totalLines = 0
   dupdate, badform, badlen, badreading = set(), set(), set(), 0
   datestamps = set([])
   for line in readings:
      totalLines += 1
      fields = line.split('\t')
      date = fields[0]
      pairs = [(fields[i],fields[i+1]) for i in range(1,len(fields),2)]

      lineFormatOk = datePat.match(date) and \
         all( valuPat.match(p[0]) for p in pairs ) and \
         all( statPat.match(p[1]) for p in pairs )
      if not lineFormatOk:
         if debug: print 'Bad formatting', line
         badform.add(date)

      if len(pairs)!=24 or any( int(p[1]) < 1 for p in pairs ):
         if debug: print 'Missing values', line
      if len(pairs)!=24: badlen.add(date)
      if any( int(p[1]) < 1 for p in pairs ): badreading += 1

      if date in datestamps:
         if debug: print 'Duplicate datestamp', line
         dupdate.add(date)

      datestamps.add(date)

   print 'Duplicate dates:\n ', '\n  '.join(sorted(dupdate))
   print 'Bad format:\n ', '\n  '.join(sorted(badform))
   print 'Bad number of fields:\n ', '\n  '.join(sorted(badlen))
   print 'Records with good readings: %i = %5.2f%%\n' % (
      totalLines-badreading, (totalLines-badreading)/float(totalLines)*100 )
   print 'Total records: ', totalLines

readings = open('readings.txt','r')
munge2(readings)
```


```txt
bash$  /cygdrive/c/Python26/python  munge2.py
Duplicate dates:
  1990-03-25
  1991-03-31
  1992-03-29
  1993-03-28
  1995-03-26
Bad format:

Bad number of fields:

Records with good readings: 5017 = 91.70%

Total records:  5471
bash$
```



## R


```R
# Read in data from file
dfr <- read.delim("d:/readings.txt", colClasses=c("character", rep(c("numeric", "integer"), 24)))
dates <- strptime(dfr[,1], "%Y-%m-%d")

# Any bad values?
dfr[which(is.na(dfr))]

# Any duplicated dates
dates[duplicated(dates)]

# Number of rows with no bad values
flags <- as.matrix(dfr[,seq(3,49,2)])>0
sum(apply(flags, 1, all))
```



## Racket


```racket
#lang racket
(read-decimal-as-inexact #f)
;; files to read is a sequence, so it could be either a list or vector of files
(define (text-processing/2 files-to-read)
  (define seen-datestamps (make-hash))
  (define (datestamp-seen? ds) (hash-ref seen-datestamps ds #f))
  (define (datestamp-seen! ds pos) (hash-set! seen-datestamps ds pos))

  (define (fold-into-pairs l (acc null))
    (match l ['() (reverse acc)]
      [(list _) (reverse (cons l acc))]
      [(list-rest a b tl) (fold-into-pairs tl (cons (list a b) acc))]))

  (define (match-valid-field line pos)
    (match (string-split line)
      ;; if we don't hit an error, then the file is valid
      ((list-rest (not (pregexp #px"[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}")) _)
       (error 'match-valid-field "invalid format non-datestamp at head: ~a~%" line))

      ;; check for duplicates
      ((list-rest (? datestamp-seen? ds) _)
       (printf "duplicate datestamp: ~a at line: ~a (first seen at: ~a)~%"
               ds pos (datestamp-seen? ds))
       #f)

      ;; register the datestamp as seen, then move on to rest of match
      ((list-rest ds _) (=> next-match-rule) (datestamp-seen! ds pos) (next-match-rule))

      ((list-rest
        _
        (app fold-into-pairs
             (list (list (app string->number (and (? number?) vs))
                         (app string->number (and (? integer?) statuss)))
                   ...)))
       (=> next-match-rule)
       (unless (= (length vs) 24) (next-match-rule))
       (not (for/first ((s statuss) #:unless (positive? s)) #t)))

      ;; if we don't hit an error, then the file is valid
      (else (error 'match-valid-field "bad field format: ~a~%" line))))

  (define (sub-t-p/1)
    (for/sum ((line (in-lines))
              (line-number (in-naturals 1)))
      (if (match-valid-field line line-number) 1 0)))
  (for/sum ((file-name files-to-read))
    (with-input-from-file file-name sub-t-p/1)))

(printf "~a records have good readings for all instruments~%"
        (text-processing/2 (current-command-line-arguments)))
```

Example session:

```txt
$ racket 2.rkt readings/readings.txt
duplicate datestamp: 1990-03-25 at line: 85 (first seen at: 84)
duplicate datestamp: 1991-03-31 at line: 456 (first seen at: 455)
duplicate datestamp: 1992-03-29 at line: 820 (first seen at: 819)
duplicate datestamp: 1993-03-28 at line: 1184 (first seen at: 1183)
duplicate datestamp: 1995-03-26 at line: 1911 (first seen at: 1910)
5013 records have good readings for all instruments
```



## REXX

This REXX program process the file mentioned in "text processing 1" and does further validate on the dates, flags, and data.



Some of the checks performed are:
::*   checks for duplicated date records.
::*   checks for a bad date (YYYY-MM-DD) format, among:
::*   wrong length
::*   year > current year
::*   year < 1970 (to allow for posthumous data)
::*   mm < 1 or mm > 12
::*   dd < 1 or dd > days for the month
::*   yyyy, dd, mm isn't numeric
::*   missing data (or flags)
::*   flag isn't an integer
::*   flag contains a decimal point
::*   data isn't numeric
In addition, all of the presented numbers may have commas inserted.



The program has (negated) code to write the report to a file in addition to the console.

```rexx
/*REXX program to process  instrument data  from a  data file.                */
numeric digits 20                      /*allow for bigger numbers.            */
ifid='READINGS.TXT'                    /*name of the   input  file.           */
ofid='READINGS.OUT'                    /*  "   "  "   output    "             */
grandSum=0                             /*grand sum of the whole file.         */
grandFlg=0                             /*grand number of flagged data.        */
grandOKs=0
Lflag=0                                /*longest period of flagged data.      */
Cflag=0                                /*longest continuous flagged data.     */
oldDate =0                             /*placeholder of penultimate date.     */
w       =16                            /*width of fields when displayed.      */
dupDates=0                             /*count of duplicated timestamps.      */
badFlags=0                             /*count of bad flags  (not integer).   */
badDates=0                             /*count of bad dates  (bad format).    */
badData =0                             /*count of bad data   (not numeric).   */
ignoredR=0                             /*count of ignored records, bad records*/
maxInstruments=24                      /*maximum number of instruments.       */
yyyyCurr=right(date(),4)               /*get the current year (today).        */
monDD.  =31                            /*number of days in every month.       */
                                       /*# days in Feb. is figured on the fly.*/
monDD.4 =30
monDD.6 =30
monDD.9 =30
monDD.11=30

  do records=1  while lines(ifid)\==0  /*read until finished.                 */
  rec=linein(ifid)                     /*read the next record (line).         */
  parse var rec datestamp Idata        /*pick off the the dateStamp and data. */
  if datestamp==oldDate  then do       /*found a duplicate timestamp.         */
                              dupDates=dupDates+1   /*bump the dupDate counter*/
                              call sy datestamp copies('~',30),
                                       'is a duplicate of the',
                                       "previous datestamp."
                              ignoredR=ignoredR+1     /*bump # of ignoredRecs.*/
                              iterate  /*ignore this duplicate record.        */
                              end

  parse var datestamp yyyy '-' mm '-' dd   /*obtain YYYY, MM, and the DD.     */
  monDD.2=28+leapyear(yyyy)            /*how long is February in year  YYYY ? */
                                       /*check for various bad formats.       */
  if verify(yyyy||mm||dd,1234567890)\==0 |,
     length(datestamp)\==10   |,
     length(yyyy)\==4         |,
     length(mm  )\==2         |,
     length(dd  )\==2         |,
     yyyy<1970                |,
     yyyy>yyyyCurr            |,
     mm=0  | dd=0             |,
     mm>12 | dd>monDD.mm  then do
                               badDates=badDates+1
                               call sy datestamp copies('~'),
                                                 'has an illegal format.'
                               ignoredR=ignoredR+1  /*bump number ignoredRecs.*/
                               iterate              /*ignore this bad record. */
                               end
  oldDate=datestamp                    /*save datestamp for the next read.    */
  sum=0
  flg=0
  OKs=0

    do j=1  until Idata=''             /*process the instrument data.         */
    parse var Idata data.j flag.j Idata

    if pos('.',flag.j)\==0 |,          /*does flag have a decimal point  -or- */
       \datatype(flag.j,'W')  then do  /* ··· is the flag not a whole number? */
                                   badFlags=badFlags+1 /*bump badFlags counter*/
                                   call sy datestamp copies('~'),
                                           'instrument' j "has a bad flag:",
                                           flag.j
                                   iterate       /*ignore it and it's data.   */
                                   end

    if \datatype(data.j,'N')  then do  /*is the flag not a whole number?*/
                                   badData=badData+1      /*bump counter.*/
                                   call sy datestamp copies('~'),
                                           'instrument' j "has bad data:",
                                           data.j
                                   iterate       /*ignore it & it's flag.*/
                                   end

    if flag.j>0  then do               /*if good data, ~~~                    */
                      OKs=OKs+1
                      sum=sum+data.j
                      if Cflag>Lflag  then do
                                           Ldate=datestamp
                                           Lflag=Cflag
                                           end
                      Cflag=0
                      end
                 else do               /*flagged data ~~~                     */
                      flg=flg+1
                      Cflag=Cflag+1
                      end
    end   /*j*/

  if j>maxInstruments then do
                           badData=badData+1       /*bump the badData counter.*/
                           call sy datestamp copies('~'),
                                   'too many instrument datum'
                           end

  if OKs\==0  then avg=format(sum/OKs,,3)
              else avg='[n/a]'
  grandOKs=grandOKs+OKs
  _=right(commas(avg),w)
  grandSum=grandSum+sum
  grandFlg=grandFlg+flg
  if flg==0  then  call sy datestamp ' average='_
             else  call sy datestamp ' average='_ '  flagged='right(flg,2)
  end   /*records*/

records=records-1                      /*adjust for reading the  end─of─file. */
if grandOKs\==0  then grandAvg=format(grandsum/grandOKs,,3)
                 else grandAvg='[n/a]'
call sy
call sy copies('=',60)
call sy '      records read:'  right(commas(records ),w)
call sy '   records ignored:'  right(commas(ignoredR),w)
call sy '     grand     sum:'  right(commas(grandSum),w+4)
call sy '     grand average:'  right(commas(grandAvg),w+4)
call sy '     grand OK data:'  right(commas(grandOKs),w)
call sy '     grand flagged:'  right(commas(grandFlg),w)
call sy '   duplicate dates:'  right(commas(dupDates),w)
call sy '         bad dates:'  right(commas(badDates),w)
call sy '         bad  data:'  right(commas(badData ),w)
call sy '         bad flags:'  right(commas(badFlags),w)
if Lflag\==0 then call sy '   longest flagged:' right(commas(LFlag),w) " ending at " Ldate
call sy copies('=',60)
exit                                   /*stick a fork in it,  we're all  done.*/
/*────────────────────────────────────────────────────────────────────────────*/
commas: procedure;  parse arg _;   n=_'.9';    #=123456789;    b=verify(n,#,"M")
        e=verify(n,#'0',,verify(n,#"0.",'M'))-4
           do j=e  to b  by -3;   _=insert(',',_,j);    end  /*j*/;     return _
/*────────────────────────────────────────────────────────────────────────────*/
leapyear: procedure; arg y             /*year could be:  Y,  YY,  YYY, or YYYY*/
if length(y)==2 then y=left(right(date(),4),2)y      /*adjust for   YY   year.*/
if y//4\==0     then return 0          /* not divisible by 4?   Not a leapyear*/
return y//100\==0 | y//400==0          /*apply the 100  and the 400 year rule.*/
/*────────────────────────────────────────────────────────────────────────────*/
sy:     say arg(1);               call lineout ofid,arg(1);             return
```

'''output'''   when using the default input file:
<pre style="height:35ex">
  ∙
  ∙
  ∙
1991-03-31  average=          23.542
1991-03-31 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ is a duplicate of the previous datestamp.
1991-04-01  average=          23.217   flagged= 1
1991-04-02  average=          19.792
1991-04-03  average=          13.958
  ∙
  ∙
  ∙

### ======================================================

      records read:            5,471
   records ignored:                5
     grand     sum:        1,357,152.400
     grand average:               10.496
     grand OK data:          129,306
     grand flagged:            1,878
   duplicate dates:                5
         bad dates:                0
         bad  data:                0
         bad flags:                0
   longest flagged:              589  ending at  1993-03-05

### ======================================================


```



## Ruby


```ruby
require 'set'

def munge2(readings, debug=false)
   datePat = /^\d{4}-\d{2}-\d{2}/
   valuPat = /^[-+]?\d+\.\d+/
   statPat = /^-?\d+/
   totalLines = 0
   dupdate, badform, badlen, badreading = Set[], Set[], Set[], 0
   datestamps = Set[[]]
   for line in readings
      totalLines += 1
      fields = line.split(/\t/)
      date = fields.shift
      pairs = fields.enum_slice(2).to_a

      lineFormatOk = date =~ datePat &&
        pairs.all? { |x,y| x =~ valuPat && y =~ statPat }
      if !lineFormatOk
         puts 'Bad formatting ' + line if debug
         badform << date
      end

      if pairs.length != 24 ||
           pairs.any? { |x,y| y.to_i < 1 }
         puts 'Missing values ' + line if debug
      end
      if pairs.length != 24
         badlen << date
      end
      if pairs.any? { |x,y| y.to_i < 1 }
         badreading += 1
      end

      if datestamps.include?(date)
         puts 'Duplicate datestamp ' + line if debug
         dupdate << date
      end

      datestamps << date
   end

   puts 'Duplicate dates:', dupdate.sort.map { |x| '  ' + x }
   puts 'Bad format:', badform.sort.map { |x| '  ' + x }
   puts 'Bad number of fields:', badlen.sort.map { |x| '  ' + x }
   puts 'Records with good readings: %i = %5.2f%%' % [
      totalLines-badreading, (totalLines-badreading)/totalLines.to_f*100 ]
   puts
   puts 'Total records:  %d' % totalLines
end

open('readings.txt','r') do |readings|
   munge2(readings)
end
```



## Scala

{{works with|Scala|2.8}}

```scala
object DataMunging2 {
  import scala.io.Source
  import scala.collection.immutable.{TreeMap => Map}

  val pattern = """^(\d+-\d+-\d+)""" + """\s+(\d+\.\d+)\s+(-?\d+)""" * 24 + "$" r;

  def main(args: Array[String]) {
    val files = args map (new java.io.File(_)) filter (file => file.isFile && file.canRead)
    val (numFormatErrors, numValidRecords, dateMap) =
      files.iterator.flatMap(file => Source fromFile file getLines ()).
        foldLeft((0, 0, new Map[String, Int] withDefaultValue 0)) {
        case ((nFE, nVR, dM), line) => pattern findFirstMatchIn line map (_.subgroups) match {
          case Some(List(date, rawData @ _*)) =>
            val allValid = (rawData map (_ toDouble) iterator) grouped 2 forall (_.last > 0)
            (nFE, nVR + (if (allValid) 1 else 0), dM(date) += 1)
          case None => (nFE + 1, nVR, dM)
        }
      }

    dateMap foreach {
      case (date, repetitions) if repetitions > 1 => println(date+": "+repetitions+" repetitions")
      case _ =>
    }

    println("""|
               |Valid records: %d
               |Duplicated dates: %d
               |Duplicated records: %d
               |Data format errors: %d
               |Invalid data records: %d
               |Total records: %d""".stripMargin format (
              numValidRecords,
              dateMap filter { case (_, repetitions) => repetitions > 1 } size,
              dateMap.valuesIterable filter (_ > 1) map (_ - 1) sum,
              numFormatErrors,
              dateMap.valuesIterable.sum - numValidRecords,
              dateMap.valuesIterable.sum))
  }
}
```


Sample output:


```txt
1990-03-25: 2 repetitions
1991-03-31: 2 repetitions
1992-03-29: 2 repetitions
1993-03-28: 2 repetitions
1995-03-26: 2 repetitions

Valid records: 5017
Duplicated dates: 5
Duplicated records: 5
Data format errors: 0
Invalid data records: 454
Total records: 5471

```



## Sidef

{{trans|Perl 6}}

```ruby
var good_records = 0;
var dates = Hash();

ARGF.each { |line|
    var m = /^(\d\d\d\d-\d\d-\d\d)((?:\h+\d+\.\d+\h+-?\d+){24})\s*$/.match(line);
    m || (warn "Bad format at line #{$.}"; next);
    dates{m[0]} := 0 ++;
    var i = 0;
    m[1].words.all{|n| i++.is_even || (n.to_num >= 1) } && ++good_records;
}

say "#{good_records} good records out of #{$.} total";
say 'Repeated timestamps:';
say dates.to_a.grep{ .value > 1 }.map { .key }.sort.join("\n");
```

{{out}}

```txt

$ sidef script.sf < readings.txt
5017 good records out of 5471 total
Repeated timestamps:
1990-03-25
1991-03-31
1992-03-29
1993-03-28
1995-03-26

```



## Tcl



```tcl
set data [lrange [split [read [open "readings.txt" "r"]] "\n"] 0 end-1]
set total [llength $data]
set correct $total
set datestamps {}

foreach line $data {
    set formatOk true
    set hasAllMeasurements true

    set date [lindex $line 0]
    if {[llength $line] != 49} { set formatOk false }
    if {![regexp {\d{4}-\d{2}-\d{2}} $date]} { set formatOk false }
    if {[lsearch $datestamps $date] != -1} { puts "Duplicate datestamp: $date" } {lappend datestamps $date}

    foreach {value flag} [lrange $line 1 end] {
        if {$flag < 1} { set hasAllMeasurements false }

        if {![regexp -- {[-+]?\d+\.\d+} $value] || ![regexp -- {-?\d+} $flag]} {set formatOk false}
    }
    if {!$hasAllMeasurements} { incr correct -1 }
    if {!$formatOk} { puts "line \"$line\" has wrong format" }
}

puts "$correct records with good readings = [expr $correct * 100.0 / $total]%"
puts "Total records: $total"
```


```txt
$ tclsh munge2.tcl
Duplicate datestamp: 1990-03-25
Duplicate datestamp: 1991-03-31
Duplicate datestamp: 1992-03-29
Duplicate datestamp: 1993-03-28
Duplicate datestamp: 1995-03-26
5017 records with good readings = 91.7016998721%
Total records: 5471

```


'''Second version'''

To demonstate a different method to iterate over the file, and different ways to verify data types:


```tcl
set total [set good 0]
array set seen {}
set fh [open readings.txt]
while {[gets $fh line] != -1} {
    incr total
    set fields [regexp -inline -all {[^ \t\r\n]+} $line]
    if {[llength $fields] != 49} {
        puts "bad format: not 49 fields on line $total"
        continue
    }
    if { ! [regexp {^(\d{4}-\d\d-\d\d)$} [lindex $fields 0] -> date]} {
        puts "bad format: invalid date on line $total: '$date'"
        continue
    }

    if {[info exists seen($date)]} {
        puts "duplicate date on line $total: $date"
    }
    incr seen($date)

    set line_format_ok true
    set readings_ignored 0
    foreach {value flag} [lrange $fields 1 end] {
        if { ! [string is double -strict $value]} {
            puts "bad format: value not a float on line $total: '$value'"
            set line_format_ok false
        }
        if { ! [string is int -strict $flag]} {
            puts "bad format: flag not an integer on line $total: '$flag'"
            set line_format_ok false
        }
        if {$flag < 1} {incr readings_ignored}
    }
    if {$line_format_ok && $readings_ignored == 0} {incr good}
}
close $fh

puts "total: $total"
puts [format "good:  %d = %5.2f%%" $good [expr {100.0 * $good / $total}]]
```

Results:

```txt
duplicate date on line 85: 1990-03-25
duplicate date on line 456: 1991-03-31
duplicate date on line 820: 1992-03-29
duplicate date on line 1184: 1993-03-28
duplicate date on line 1911: 1995-03-26
total: 5471
good:  5017 = 91.70%
```



## Ursala

compiled and run in a single step, with the input file accessed as a list of strings
pre-declared in readings_dot_txt

```Ursala
#import std
#import nat

readings        = (*F ~&c/;digits+ rlc ==+ ~~ -={` ,9%cOi&,13%cOi&}) readings_dot_txt

valid_format    = all -&length==49,@tK27 all ~&w/`.&& ~&jZ\digits--'-.',@tK28 all ~&jZ\digits--'-'&-

duplicate_dates = :/'duplicated dates:'+ ~&hK2tFhhPS|| -[(none)]-!

good_readings   = --' good readings'@h+ %nP+ length+ *~ @tK28 all ~='0'&& ~&wZ/`-

#show+

main = valid_format?(^C/good_readings duplicate_dates,-[invalid format]-!) readings
```

output:

```txt
5017 good readings
duplicated dates:
1995-03-26
1993-03-28
1992-03-29
1991-03-31
1990-03-25
```



## VBScript


```vb
Set objFSO = CreateObject("Scripting.FileSystemObject")
Set objFile = objFSO.OpenTextFile(objFSO.GetParentFolderName(WScript.ScriptFullName) &_
			"\readings.txt",1)
Set objDateStamp = CreateObject("Scripting.Dictionary")

Total_Records = 0
Valid_Records = 0
Duplicate_TimeStamps = ""

Do Until objFile.AtEndOfStream
	line = objFile.ReadLine
	If line <> "" Then
		token = Split(line,vbTab)
		If objDateStamp.Exists(token(0)) = False Then
			objDateStamp.Add token(0),""
			Total_Records = Total_Records + 1
			If IsValid(token) Then
				Valid_Records = Valid_Records + 1
			End If
		Else
			Duplicate_TimeStamps = Duplicate_TimeStamps & token(0) & vbCrLf
			Total_Records = Total_Records + 1
		End If
	End If
Loop

Function IsValid(arr)
	IsValid = True
	Bad_Readings = 0
	n = 1
	Do While n <= UBound(arr)
		If n + 1 <= UBound(arr) Then
			If CInt(arr(n+1)) < 1 Then
				Bad_Readings = Bad_Readings + 1
			End If
		End If
		n = n + 2
	Loop
	If Bad_Readings > 0 Then
		IsValid = False
	End If
End Function

WScript.StdOut.Write "Total Number of Records = " & Total_Records
WScript.StdOut.WriteLine
WScript.StdOut.Write "Total Valid Records = " & Valid_Records
WScript.StdOut.WriteLine
WScript.StdOut.Write "Duplicate Timestamps:"
WScript.StdOut.WriteLine
WScript.StdOut.Write Duplicate_TimeStamps
WScript.StdOut.WriteLine

objFile.Close
Set objFSO = Nothing
```


{{Out}}

```txt

Total Number of Records = 5471
Total Valid Records = 5013
Duplicate Timestamps:
1990-03-25
1991-03-31
1992-03-29
1993-03-28
1995-03-26

```



## Vedit macro language

This implementation does the following checks:
* Checks for duplicate date fields. Note: duplicates can still be counted as valid records, as in other implementations.
* Checks date format.
* Checks that value fields have 1 or more digits followed by decimal point followed by 3 digits
* Reads flag value and checks if it is positive
* Requires 24 value/flag pairs on each line

```vedit
#50 = Buf_Num           // Current edit buffer (source data)
File_Open("|(PATH_ONLY)\output.txt")
#51 = Buf_Num           // Edit buffer for output file
Buf_Switch(#50)

#11 = #12 = #13 = #14 = #15 = 0
Reg_Set(15, "xxx")

While(!At_EOF) {
    #10 = 0
    #12++

    // Check for repeated date field
    if (Match(@15) == 0) {
        #20 = Cur_Line
        Buf_Switch(#51)   // Output file
        Reg_ins(15) IT(": duplicate record at ") Num_Ins(#20)
        Buf_Switch(#50)   // Input file
        #13++
    }

    // Check format of date field
    if (Match("|d|d|d|d-|d|d-|d|d|w", ADVANCE) != 0) {
        #10 = 1
        #14++
    }
    Reg_Copy_Block(15, BOL_pos, Cur_Pos-1)

    // Check data fields and flags:
    Repeat(24) {
        if ( Match("|d|*.|d|d|d|w", ADVANCE) != 0 || Num_Eval(ADVANCE) < 1) {
            #10 = 1
            #15++
            Break
        }
        Match("|W", ADVANCE)
    }
    if (#10 == 0) { #11++ }             // record was OK
    Line(1, ERRBREAK)
}

Buf_Switch(#51)         // buffer for output data
IN
IT("Valid records:       ") Num_Ins(#11)
IT("Duplicates:          ") Num_Ins(#13)
IT("Date format errors:  ") Num_Ins(#14)
IT("Invalid data records:") Num_Ins(#15)
IT("Total records:       ") Num_Ins(#12)
```

Sample output:

```vedit
1990-03-25: duplicate record at    85
1991-03-31: duplicate record at   456
1992-03-29: duplicate record at   820
1993-03-28: duplicate record at  1184
1995-03-26: duplicate record at  1911

Valid records:        5017
Duplicates:              5
Date format errors:      0
Invalid data records:  454
Total records:        5471
```



## zkl


```zkl
   // the RegExp engine has a low limit on groups so
   // I can't use it to select all fields, only verify them
re:=RegExp(0'|^(\d+-\d+-\d+)| + 0'|\s+\d+\.\d+\s+-*\d+| * 24 + ".+$");
w:=[1..].zip(File("readings.txt"));  //-->lazy (line #,line)
reg datep,N, good=0, dd=0;
foreach n,line in (w){
   N=n;		// since n is local to this scope
   if (not re.search(line)){ println("Line %d: malformed".fmt(n)); continue; }
   date:=line[re.matchedNs[1].xplode()];  // I can group the date field
   if (datep==date){ dd+=1; println("Line %4d: dup date: %s".fmt(n,date)); }
   datep=date;
   if (line.replace("\t"," ").split(" ").filter()[1,*]  // blow fields apart, drop date
         .pump(Void,Void.Read, // get (reading,status)
            fcn(_,s){  // stop on first problem status and return True
               if(s.strip().toInt()<1) T(Void.Stop,True) else False
       })) continue;
   good+=1;
}
println("%d records read, %d duplicate dates, %d valid".fmt(N,dd,good));
```

{{out}}

```txt

Line   85: dup date: 1990-03-25
Line  456: dup date: 1991-03-31
Line  820: dup date: 1992-03-29
Line 1184: dup date: 1993-03-28
Line 1911: dup date: 1995-03-26
5471 records read, 5 duplicate dates, 5017 valid

```



{{omit from|GUISS}}
{{omit from|Openscad}}
{{omit from|PARI/GP|No real capacity for string manipulation}}
