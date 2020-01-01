+++
title = "Text processing/1"
description = ""
date = 2018-11-07T01:15:45Z
aliases = []
[extra]
id = 3104
[taxonomies]
categories = []
tags = []
+++

{{task|Text processing}}{{Template:clarify task}}
Often data is produced by one program, in the wrong format for later use by another program or person. In these situations another program can be written to parse and transform the original data into a format useful to the other. The term "Data Munging" is [http://www.google.co.uk/search?q=%22data+munging%22 often] used in programming circles for this task.

A [http://groups.google.co.uk/group/comp.lang.awk/msg/0ecba3a3fbf247d8?hl=en request] on the comp.lang.awk newsgroup led to a typical data munging task:

```txt
I have to analyse data files that have the following format:
Each row corresponds to 1 day and the field logic is: $1 is the date,
followed by 24 value/flag pairs, representing measurements at 01:00,
02:00 ... 24:00 of the respective day. In short:

<date> <val1> <flag1> <val2> <flag2> ...  <val24> <flag24>

Some test data is available at:
... (nolonger available at original location)

I have to sum up the values (per day and only valid data, i.e. with
flag>0) in order to calculate the mean. That's not too difficult.
However, I also need to know what the "maximum data gap" is, i.e. the
longest period with successive invalid measurements (i.e values with
flag<=0)
```


The data is [http://www.eea.europa.eu/help/eea-help-centre/faqs/how-do-i-obtain-eea-reports free to download and use] and is of this format:

Data is no longer available at that link. Zipped mirror available [https://github.com/thundergnat/rc/blob/master/resouces/readings.zip here] (offsite mirror).
<pre style="overflow:scroll">
1991-03-30	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1
1991-03-31	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	10.000	1	20.000	1	20.000	1	20.000	1	35.000	1	50.000	1	60.000	1	40.000	1	30.000	1	30.000	1	30.000	1	25.000	1	20.000	1	20.000	1	20.000	1	20.000	1	20.000	1	35.000	1
1991-03-31	40.000	1	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2	0.000	-2
1991-04-01	0.000	-2	13.000	1	16.000	1	21.000	1	24.000	1	22.000	1	20.000	1	18.000	1	29.000	1	44.000	1	50.000	1	43.000	1	38.000	1	27.000	1	27.000	1	24.000	1	23.000	1	18.000	1	12.000	1	13.000	1	14.000	1	15.000	1	13.000	1	10.000	1
1991-04-02	8.000	1	9.000	1	11.000	1	12.000	1	12.000	1	12.000	1	27.000	1	26.000	1	27.000	1	33.000	1	32.000	1	31.000	1	29.000	1	31.000	1	25.000	1	25.000	1	24.000	1	21.000	1	17.000	1	14.000	1	15.000	1	12.000	1	12.000	1	10.000	1
1991-04-03	10.000	1	9.000	1	10.000	1	10.000	1	9.000	1	10.000	1	15.000	1	24.000	1	28.000	1	24.000	1	18.000	1	14.000	1	12.000	1	13.000	1	14.000	1	15.000	1	14.000	1	15.000	1	13.000	1	13.000	1	13.000	1	12.000	1	10.000	1	10.000	1

```


Only a sample of the data showing its format is given above. The full example file may be downloaded [http://rosettacode.org/resources/readings.zip here].

Structure your program to show statistics for each line of the file, (similar to the original Python, Perl, and AWK examples below), followed by summary statistics for the file. When showing example output just show a few line statistics and the full end summary.


## Ada

{{libheader|Simple components for Ada}}

```ada
with Ada.Text_IO;            use Ada.Text_IO;
with Strings_Edit;           use Strings_Edit;
with Strings_Edit.Floats;    use Strings_Edit.Floats;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

procedure Data_Munging is
   Syntax_Error : exception;
   type Gap_Data is record
      Count   : Natural := 0;
      Line    : Natural := 0;
      Pointer : Integer;
      Year    : Integer;
      Month   : Integer;
      Day     : Integer;
   end record;
   File    : File_Type;
   Max     : Gap_Data;
   This    : Gap_Data;
   Current : Gap_Data;
   Count   : Natural := 0;
   Sum     : Float   := 0.0;
begin
   Open (File, In_File, "readings.txt");
   loop
      declare
         Line    : constant String := Get_Line (File);
         Pointer : Integer := Line'First;
         Flag    : Integer;
         Data    : Float;
      begin
         Current.Line := Current.Line + 1;
         Get (Line, Pointer, SpaceAndTab);
         Get (Line, Pointer, Current.Year);
         Get (Line, Pointer, Current.Month);
         Get (Line, Pointer, Current.Day);
         while Pointer <= Line'Last loop
            Get (Line, Pointer, SpaceAndTab);
            Current.Pointer := Pointer;
            Get (Line, Pointer, Data);
            Get (Line, Pointer, SpaceAndTab);
            Get (Line, Pointer, Flag);
            if Flag < 0 then
               if This.Count = 0 then
                  This := Current;
               end if;
               This.Count := This.Count + 1;
            else
               if This.Count > 0 and then Max.Count < This.Count then
                  Max := This;
               end if;
               This.Count := 0;
               Count := Count + 1;
               Sum   := Sum + Data;
            end if;
         end loop;
      exception
         when End_Error =>
            raise Syntax_Error;
      end;
   end loop;
exception
   when End_Error =>
      Close (File);
      if This.Count > 0 and then Max.Count < This.Count then
         Max := This;
      end if;
      Put_Line ("Average " & Image (Sum / Float (Count)) & " over " & Image (Count));
      if Max.Count > 0 then
         Put ("Max. " & Image (Max.Count) & " false readings start at ");
         Put (Image (Max.Line) & ':' & Image (Max.Pointer) & " stamped ");
         Put_Line (Image (Max.Year) & Image (Max.Month) & Image (Max.Day));
      end if;
   when others =>
      Close (File);
      Put_Line ("Syntax error at " & Image (Current.Line) & ':' & Image (Max.Pointer));
end Data_Munging;
```

The implementation performs minimal checks. The average is calculated over all valid data. For the maximal chain of consequent invalid data, the source line number, the column number, and the time stamp of the first invalid data is printed.
{{out|Sample output}}

```txt

Average 10.47915 over 129628
Max. 589 false readings start at 1136:20 stamped 1993-2-9

```



## ALGOL 68

{{trans|python}}
<!--{{does not work with|ALGOL 68|Standard - argc and argv are extensions}} -->
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}
<!--{{does not work with|ELLA ALGOL 68|Any (with appropriate job cards) - argc and argv are extensions}} -->

```algol68
INT no data := 0;               # Current run of consecutive flags<0 in lines of file #
INT no data max := -1;          # Max consecutive flags<0 in lines of file #
FLEX[0]STRING no data max line; # ... and line number(s) where it occurs #

REAL tot file := 0;             # Sum of file data #
INT num file := 0;              # Number of file data items with flag>0 #

# CHAR fs = " "; #
INT nf = 24;

INT upb list := nf;
FORMAT list repr = $n(upb list-1)(g", ")g$;

PROC exception = ([]STRING args)VOID:(
  putf(stand error, ($"Exception"$, $", "g$, args, $l$));
  stop
);

PROC raise io error = (STRING message)VOID:exception(("io error", message));

OP +:= = (REF FLEX []STRING rhs, STRING append)REF FLEX[]STRING: (
  HEAP [UPB rhs+1]STRING out rhs;
  out rhs[:UPB rhs] := rhs;
  out rhs[UPB rhs+1] := append;
  rhs := out rhs;
  out rhs
);

INT upb opts = 3; # these are "a68g" "./Data_Munging.a68" & "-" #
[argc - upb opts]STRING in files;
FOR arg TO UPB in files DO in files[arg] := argv(upb opts + arg) OD;

MODE FIELD = STRUCT(REAL data, INT flag);
FORMAT field repr = $2(g)$;

FOR index file TO UPB in files DO
  STRING file name = in files[index file], FILE file;
  IF open(file, file name, stand in channel) NE 0 THEN
    raise io error("Cannot open """+file name+"""") FI;
  on logical file end(file, (REF FILE f)BOOL: logical file end done);
  REAL tot line, INT num line;
  # make term(file, ", ") for CSV data #
  STRING date;
  DO
    tot line := 0;             # sum of line data #
    num line := 0;             # number of line data items with flag>0 #
    # extract field info #
    [nf]FIELD data;
    getf(file, ($10a$, date, field repr, data, $l$));

    FOR key TO UPB data DO
      FIELD field = data[key];
      IF flag OF field<1 THEN
        no data +:= 1
      ELSE
        # check run of data-absent data #
        IF no data max = no data AND no data>0 THEN
          no data max line +:= date FI;
        IF no data max<no data AND no data>0 THEN
          no data max := no data;
          no data max line := date FI;
        # re-initialise run of no data counter #
        no data := 0;
        # gather values for averaging #
        tot line +:= data OF field;
        num line +:= 1
      FI
    OD;

    # totals for the file so far #
    tot file +:= tot line;
    num file +:= num line;

    printf(($"Line: "g"  Reject: "g(-2)"  Accept: "g(-2)"  Line tot: "g(-14, 3)"  Line avg: "g(-14, 3)l$,
          date,
          UPB(data) -num line,
          num line, tot line,
          IF num line>0 THEN tot line/num line ELSE 0 FI))
  OD;
  logical file end done:
    close(file)
OD;

FORMAT plural = $b(" ", "s")$,
       p = $b("", "s")$;

upb list := UPB in files;
printf(($l"File"f(plural)"    = "$, upb list = 1, list repr, in files, $l$,
        $"Total    = "g(-0, 3)l$, tot file,
        $"Readings = "g(-0)l$, num file,
        $"Average  = "g(-0, 3)l$, tot file / num file));

upb list := UPB no data max line;
printf(($l"Maximum run"f(p)" of "g(-0)" consecutive false reading"f(p)" ends at line starting with date"f(p)": "$,
    upb list = 1, no data max, no data max = 0, upb list = 1, list repr, no data max line, $l$))
```

Command:
 $ a68g ./Data_Munging.a68 - data
{{out}}

```txt

Line: 1991-03-30  Reject:  0  Accept: 24  Line tot:        240.000  Line avg:         10.000
Line: 1991-03-31  Reject:  0  Accept: 24  Line tot:        565.000  Line avg:         23.542
Line: 1991-03-31  Reject: 23  Accept:  1  Line tot:         40.000  Line avg:         40.000
Line: 1991-04-01  Reject:  1  Accept: 23  Line tot:        534.000  Line avg:         23.217
Line: 1991-04-02  Reject:  0  Accept: 24  Line tot:        475.000  Line avg:         19.792
Line: 1991-04-03  Reject:  0  Accept: 24  Line tot:        335.000  Line avg:         13.958

File     = data
Total    = 2189.000
Readings = 120
Average  = 18.242

Maximum run of 24 consecutive false readings ends at line starting with date: 1991-04-01

```



## Aime


```aime
integer bads, count, max_bads;
file f;
list l;
real s;
text bad_day, worst_day;

f.stdin;

max_bads = count = bads = s = 0;

while (f.list(l, 0) ^ -1) {
    integer i;

    i = 2;
    while (i < 49) {
        if (0 < atoi(l[i])) {
            count += 1;
            s += atof(l[i - 1]);
            if (max_bads < bads) {
                max_bads = bads;
                worst_day = bad_day;
            }
            bads = 0;
        } else {
            if (!bads) {
                bad_day = l[0];
            }
            bads += 1;
        }
        i += 2;
    }
}

o_form("Averaged /d3/ over ~ readings.\n", s / count, count);
o_("Longest bad run ", max_bads, ", started ", worst_day, ".\n");
```

Run as:

```txt
cat readings.txt | tr -d \\r | aime SOURCE_FILE
```

{{out}}

```txt
Averaged 10.497 over 129403 readings.
Longest bad run 589, started 1993-02-09.
```



## AutoHotkey


```AutoHotkey
# Author AlephX Aug 17 2011

SetFormat, float, 4.2
SetFormat, FloatFast, 4.2

data = %A_scriptdir%\readings.txt
result = %A_scriptdir%\results.txt
totvalid := 0
totsum := 0
totavg:= 0

Loop, Read, %data%, %result%
	{
	sum := 0
	Valid := 0
	Couples := 0
	Lines := A_Index
    Loop, parse, A_LoopReadLine, %A_Tab%
		{
        ;MsgBox, Field number %A_Index% is %A_LoopField%
		if A_index = 1
			{
			Date := A_LoopField
			Counter := 0
			}
		else
			{
			Counter++
			couples := Couples + 0.5
			if Counter = 1
				{
				value := A_LoopField / 1
				}
			else
				{
				if A_loopfield > 0
					{
					Sum := Sum + value
					Valid++

					if (wrong > maxwrong)
						{
						maxwrong := wrong
						lastwrongdate := currwrongdate
						startwrongdate := firstwrongdate
						startoccurrence := firstoccurrence
						lastoccurrence := curroccurrence
						}
					wrong := 0
					}
				else
					{
					wrong++
					currwrongdate := date
					curroccurrence := (A_index-1) / 2
					if (wrong = 1)
						{
						firstwrongdate := date
						firstoccurrence := curroccurrence
						}
					}
				Counter := 0
				}
			}
		}
	avg := sum / valid
	TotValid := Totvalid+valid
	TotSum := Totsum+sum
	FileAppend, Day: %date% sum: %sum% avg: %avg% Readings: %valid%/%couples%`n
	}

Totavg := TotSum / TotValid
FileAppend, `n`nDays %Lines%`nMaximal wrong readings: %maxwrong% from %startwrongdate% at %startoccurrence% to %lastwrongdate% at %lastoccurrence%`n`n, %result%
FileAppend, Valid readings: %TotValid%`nTotal Value: %TotSUm%`nAverage: %TotAvg%, %result%
```

{{out|Sample output}}

```txt
Day: 1990-01-01 sum: 590.00 avg: 26.82 Readings: 22/24.00
Day: 1990-01-02 sum: 410.00 avg: 17.08 Readings: 24/24.00
Day: 1990-01-03 sum: 1415.00 avg: 58.96 Readings: 24/24.00
Day: 1990-01-04 sum: 1800.00 avg: 75.00 Readings: 24/24.00
Day: 1990-01-05 sum: 1130.00 avg: 47.08 Readings: 24/24.00
...
Day: 2004-12-31 sum: 47.30 avg: 2.06 Readings: 23/24.00


Days 5471
Maximal wrong readings: 589 from 1993-02-09 at 2.00 to 1993-03-05 at 14.00

Valid readings: 129403
Total Value: 1358393.40
Average: 10.50
```



## AWK


```awk
BEGIN{
  nodata = 0;             # Current run of consecutive flags<0 in lines of file
  nodata_max=-1;          # Max consecutive flags<0 in lines of file
  nodata_maxline="!";     # ... and line number(s) where it occurs
}
FNR==1 {
  # Accumulate input file names
  if(infiles){
    infiles = infiles "," infiles
  } else {
    infiles = FILENAME
  }
}
{
  tot_line=0;             # sum of line data
  num_line=0;             # number of line data items with flag>0

  # extract field info, skipping initial date field
  for(field=2; field<=NF; field+=2){
    datum=$field;
    flag=$(field+1);
    if(flag<1){
      nodata++
    }else{
      # check run of data-absent fields
      if(nodata_max==nodata && (nodata>0)){
        nodata_maxline=nodata_maxline ", " $1
      }
      if(nodata_max<nodata && (nodata>0)){
        nodata_max=nodata
        nodata_maxline=$1
      }
      # re-initialise run of nodata counter
      nodata=0;
      # gather values for averaging
      tot_line+=datum
      num_line++;
    }
  }

  # totals for the file so far
  tot_file += tot_line
  num_file += num_line

  printf "Line: %11s  Reject: %2i  Accept: %2i  Line_tot: %10.3f  Line_avg: %10.3f\n", \
         $1, ((NF -1)/2) -num_line, num_line, tot_line, (num_line>0)? tot_line/num_line: 0

  # debug prints of original data plus some of the computed values
  #printf "%s  %15.3g  %4i\n", $0, tot_line, num_line
  #printf "%s\n  %15.3f  %4i  %4i  %4i  %s\n", $0, tot_line, num_line,  nodata, nodata_max, nodata_maxline


}

END{
  printf "\n"
  printf "File(s)  = %s\n", infiles
  printf "Total    = %10.3f\n", tot_file
  printf "Readings = %6i\n", num_file
  printf "Average  = %10.3f\n", tot_file / num_file

  printf "\nMaximum run(s) of %i consecutive false readings ends at line starting with date(s): %s\n", nodata_max, nodata_maxline
}
```

{{out|Sample output}}

```txt
bash$ awk -f readings.awk readings.txt | tail
Line:  2004-12-29  Reject:  1  Accept: 23  Line_tot:     56.300  Line_avg:      2.448
Line:  2004-12-30  Reject:  1  Accept: 23  Line_tot:     65.300  Line_avg:      2.839
Line:  2004-12-31  Reject:  1  Accept: 23  Line_tot:     47.300  Line_avg:      2.057

File(s)  = readings.txt
Total    = 1358393.400
Readings = 129403
Average  =     10.497

Maximum run(s) of 589 consecutive false readings ends at line starting with date(s): 1993-03-05
bash$
```



## Batch File


```dos
@echo off
setlocal ENABLEDELAYEDEXPANSION
set maxrun=    0
set maxstart=
set maxend=
set notok=0
set inputfile=%1
for /F "tokens=1,*" %%i in (%inputfile%) do (
    set date=%%i
    call :processline %%j
)

echo\
echo max false: %maxrun%  from %maxstart% until %maxend%

goto :EOF

:processline
set sum=0000
set count=0
set hour=1
:loop
if "%1"=="" goto :result
set num=%1
if "%2"=="1" (
    if "%notok%" NEQ "0" (
        set notok=     !notok!
        if /I "!notok:~-5!" GTR "%maxrun%" (
            set maxrun=!notok:~-5!
            set maxstart=%nok0date% %nok0hour%
            set maxend=%nok1date% %nok1hour%
        )
        set notok=0
    )
    set /a sum+=%num:.=%
    set /a count+=1
) else (
    if "%notok%" EQU "0" (
        set nok0date=%date%
        set nok0hour=%hour%
    ) else (
        set nok1date=%date%
        set nok1hour=%hour%
    )
    set /a notok+=1
)
shift
shift
set /a hour+=1
goto :loop

:result
if "%count%"=="0" (
    set mean=0
) else (
    set /a mean=%sum%/%count%
)
if "%mean%"=="0" set mean=0000
if "%sum%"=="0" set sum=0000
set mean=%mean:~0,-3%.%mean:~-3%
set sum=%sum:~0,-3%.%sum:~-3%
set count=   %count%
set sum=    %sum%
set mean=    %mean%
echo Line: %date% Accept: %count:~-3%  tot: %sum:~-8%  avg: %mean:~-8%

goto :EOF
```

{{out}}

```txt

C:\ >batch-fileparsing.bat readings-2.txt
Line: 1990-01-01 Accept:  22  tot:  590.000  avg:   26.818
Line: 1990-01-02 Accept:  24  tot:  410.000  avg:   17.083
Line: 1990-01-03 Accept:  24  tot: 1415.000  avg:   58.958
Line: 1990-01-04 Accept:  24  tot: 1800.000  avg:   75.000
Line: 1990-01-05 Accept:  24  tot: 1130.000  avg:   47.083
Line: 1990-01-06 Accept:  24  tot: 1820.000  avg:   75.833
...
Line: 1993-12-26 Accept:  24  tot:  195.000  avg:    8.125
Line: 1993-12-27 Accept:  24  tot:  112.000  avg:    4.666
Line: 1993-12-28 Accept:  24  tot:  303.000  avg:   12.625
Line: 1993-12-29 Accept:  24  tot:  339.000  avg:   14.125
Line: 1993-12-30 Accept:  24  tot:  593.000  avg:   24.708
Line: 1993-12-31 Accept:  24  tot:  865.000  avg:   36.041
...
max false:   589  from 1993-02-09 2 until 1993-03-05 14

```



## BBC BASIC


```bbcbasic
      file% = OPENIN("readings.txt")
      IF file% = 0 THEN PRINT "Could not open test data file" : END

      Total = 0
      Count% = 0
      BadMax% = 0
      bad% = 0
      WHILE NOT EOF#file%
        text$ = GET$#file%
        IF text$<>"" THEN
          tab% = INSTR(text$, CHR$(9))
          date$ = LEFT$(text$, tab% - 1)
          acc = 0
          cnt% = 0
          FOR field% = 1 TO 24
            dval = VALMID$(text$, tab%+1)
            tab% = INSTR(text$, CHR$(9), tab%+1)
            flag% = VALMID$(text$, tab%+1)
            tab% = INSTR(text$, CHR$(9), tab%+1)
            IF flag% > 0 THEN
              acc += dval
              cnt% += 1
              bad% = 0
            ELSE
              bad% += 1
              IF bad% > BadMax% BadMax% = bad% : BadDate$ = date$
            ENDIF
          NEXT field%
          @% = &90A
          PRINT "Date: " date$ "  Good = "; cnt%, "  Bad = "; 24-cnt%, ;
          @% = &20308
          IF cnt% THEN PRINT "  Total = " acc "  Mean = " acc / cnt% ;
          PRINT
          Total += acc
          Count% += cnt%
        ENDIF
      ENDWHILE
      CLOSE #file%
      PRINT ' "Grand total = " ; Total
      PRINT "Number of valid readings = " ; STR$(Count%)
      PRINT "Overall mean = " ; Total / Count%
      @% = &90A
      PRINT '"Longest run of bad readings = " ; BadMax% " ending " BadDate$
```

{{out}}

```txt

Date: 1990-01-01  Good = 22     Bad = 2   Total =  590.000  Mean =   26.818
Date: 1990-01-02  Good = 24     Bad = 0   Total =  410.000  Mean =   17.083
Date: 1990-01-03  Good = 24     Bad = 0   Total = 1415.000  Mean =   58.958
Date: 1990-01-04  Good = 24     Bad = 0   Total = 1800.000  Mean =   75.000
Date: 1990-01-05  Good = 24     Bad = 0   Total = 1130.000  Mean =   47.083
Date: 1990-01-06  Good = 24     Bad = 0   Total = 1820.000  Mean =   75.833
Date: 1990-01-07  Good = 24     Bad = 0   Total = 1385.000  Mean =   57.708
....
Date: 2004-12-29  Good = 23     Bad = 1   Total =   56.300  Mean =    2.448
Date: 2004-12-30  Good = 23     Bad = 1   Total =   65.300  Mean =    2.839
Date: 2004-12-31  Good = 23     Bad = 1   Total =   47.300  Mean =    2.057

Grand total = 1358393.402
Number of valid readings = 129403
Overall mean = 10.497

Longest run of bad readings = 589 ending 1993-03-05

```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int badHrs, maxBadHrs;

static double hrsTot = 0.0;
static int rdgsTot = 0;
char bhEndDate[40];

int mungeLine( char *line, int lno, FILE *fout )
{
    char date[40], *tkn;
    int   dHrs, flag, hrs2, hrs;
    double hrsSum;
    int   hrsCnt = 0;
    double avg;

    tkn = strtok(line, ".");
    if (tkn) {
        int n = sscanf(tkn, "%s %d", &date, &hrs2);
        if (n<2) {
            printf("badly formated line - %d %s\n", lno, tkn);
            return 0;
        }
        hrsSum = 0.0;
        while( tkn= strtok(NULL, ".")) {
            n = sscanf(tkn,"%d %d %d", &dHrs, &flag, &hrs);
            if (n>=2) {
                if (flag > 0) {
                    hrsSum += 1.0*hrs2 + .001*dHrs;
                    hrsCnt += 1;
                    if (maxBadHrs < badHrs) {
                        maxBadHrs = badHrs;
                        strcpy(bhEndDate, date);
                    }
                    badHrs = 0;
                }
                else {
                    badHrs += 1;
                }
                hrs2 = hrs;
            }
            else {
                printf("bad file syntax line %d: %s\n",lno, tkn);
            }
        }
        avg = (hrsCnt > 0)? hrsSum/hrsCnt : 0.0;
        fprintf(fout, "%s  Reject: %2d  Accept: %2d  Average: %7.3f\n",
                date, 24-hrsCnt, hrsCnt, hrsSum/hrsCnt);
        hrsTot += hrsSum;
        rdgsTot += hrsCnt;
    }
    return 1;
}

int main()
{
    FILE *infile, *outfile;
    int lineNo = 0;
    char line[512];
    const char *ifilename = "readings.txt";
    outfile = fopen("V0.txt", "w");

    infile = fopen(ifilename, "rb");
    if (!infile) {
        printf("Can't open %s\n", ifilename);
        exit(1);
    }
    while (NULL != fgets(line, 512, infile)) {
        lineNo += 1;
        if (0 == mungeLine(line, lineNo, outfile))
            printf("Bad line at %d",lineNo);
    }
    fclose(infile);

    fprintf(outfile, "File:     %s\n", ifilename);
    fprintf(outfile, "Total:    %.3f\n", hrsTot);
    fprintf(outfile, "Readings: %d\n", rdgsTot);
    fprintf(outfile, "Average:  %.3f\n", hrsTot/rdgsTot);
    fprintf(outfile, "\nMaximum number of consecutive bad readings is %d\n", maxBadHrs);
    fprintf(outfile, "Ends on date %s\n", bhEndDate);
    fclose(outfile);
    return 0;
}
```

{{out|Sample output}}

```txt
1990-01-01  Reject:  2  Accept: 22  Average:  26.818
1990-01-02  Reject:  0  Accept: 24  Average:  17.083
1990-01-03  Reject:  0  Accept: 24  Average:  58.958
1990-01-04  Reject:  0  Accept: 24  Average:  75.000
1990-01-05  Reject:  0  Accept: 24  Average:  47.083
...
File:     readings.txt
Total:    1358393.400
Readings: 129403
Average:  10.497

Maximum number of consecutive bad readings is 589
Ends on date 1993-03-05
```



## C++


```cpp
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <iomanip>
#include <boost/lexical_cast.hpp>
#include <boost/algorithm/string.hpp>

using std::cout;
using std::endl;
const int NumFlags = 24;

int main()
{
    std::fstream file("readings.txt");

    int badCount = 0;
    std::string badDate;
    int badCountMax = 0;
    while(true)
    {
        std::string line;
        getline(file, line);
        if(!file.good())
            break;

        std::vector<std::string> tokens;
        boost::algorithm::split(tokens, line, boost::is_space());

        if(tokens.size() != NumFlags * 2 + 1)
        {
            cout << "Bad input file." << endl;
            return 0;
        }

        double total = 0.0;
        int accepted = 0;
        for(size_t i = 1; i < tokens.size(); i += 2)
        {
            double val = boost::lexical_cast<double>(tokens[i]);
            int flag = boost::lexical_cast<int>(tokens[i+1]);
            if(flag > 0)
            {
                total += val;
                ++accepted;
                badCount = 0;
            }
            else
            {
                ++badCount;
                if(badCount > badCountMax)
                {
                    badCountMax = badCount;
                    badDate = tokens[0];
                }
            }
        }

        cout << tokens[0];
        cout << "  Reject: " << std::setw(2) << (NumFlags - accepted);
        cout << "  Accept: " << std::setw(2) << accepted;
        cout << "  Average: " << std::setprecision(5) << total / accepted << endl;
    }
    cout << endl;
    cout << "Maximum number of consecutive bad readings is " << badCountMax << endl;
    cout << "Ends on date " << badDate << endl;
}
```

{{out}}

```txt
1990-01-01  Reject:  2  Accept: 22  Average: 26.818
1990-01-02  Reject:  0  Accept: 24  Average: 17.083
1990-01-03  Reject:  0  Accept: 24  Average: 58.958
1990-01-04  Reject:  0  Accept: 24  Average: 75
1990-01-05  Reject:  0  Accept: 24  Average: 47.083
...
Maximum number of consecutive bad readings is 589
Ends on date 1993-03-05
```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. data-munging.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT input-file ASSIGN TO INPUT-FILE-PATH
               ORGANIZATION LINE SEQUENTIAL
               FILE STATUS file-status.

       DATA DIVISION.
       FILE SECTION.
       FD  input-file.
       01  input-record.
           03  date-stamp          PIC X(10).
           03  FILLER              PIC X.
           *> Curse whoever decided to use tabs and variable length
           *> data in the file!
           03  input-data-pairs    PIC X(300).

       WORKING-STORAGE SECTION.
       78  INPUT-FILE-PATH         VALUE "readings.txt".

       01  file-status             PIC 99.
           88 file-is-ok           VALUE 0.
           88 end-of-file          VALUE 10.

       01  data-pair.
           03  val                 PIC 9(3)V9(3).
           03  flag                PIC S9.
               88  invalid-flag    VALUE -9 THRU 0.

       01  val-length              PIC 9.
       01  flag-length             PIC 9.
       01  offset                  PIC 99.

       01  day-total               PIC 9(5)V9(3).
       01  grand-total             PIC 9(8)V9(3).
       01  mean-val                PIC 9(8)V9(3).

       01  day-rejected            PIC 9(5).
       01  day-accepted            PIC 9(5).

       01  total-rejected          PIC 9(8).
       01  total-accepted          PIC 9(8).

       01  current-data-gap        PIC 9(8).
       01  max-data-gap            PIC 9(8).
       01  max-data-gap-end        PIC X(10).

       PROCEDURE DIVISION.
       DECLARATIVES.
       *> Terminate the program if an error occurs on input-file.
       input-file-error SECTION.
           USE AFTER STANDARD ERROR ON input-file.

           DISPLAY
               "An error occurred while reading input.txt. "
               "File error: " file-status
               ". The program will terminate."
           END-DISPLAY

           GOBACK
           .

       END DECLARATIVES.

       main-line.
           *> Terminate the program if the file cannot be opened.
           OPEN INPUT input-file
           IF NOT file-is-ok
               DISPLAY "File could not be opened. The program will "
                   "terminate."
               GOBACK
           END-IF

           *> Process the data in the file.
           PERFORM FOREVER
               *> Stop processing if at the end of the file.
               READ input-file
                   AT END
                       EXIT PERFORM
               END-READ

               *> Split the data up and process the value-flag pairs.
               PERFORM UNTIL input-data-pairs = SPACES
                   *> Split off the value-flag pair at the front of the
                   *> record.
                   UNSTRING input-data-pairs DELIMITED BY X"09"
                       INTO val COUNT val-length, flag COUNT flag-length

                   COMPUTE offset = val-length + flag-length + 3
                   MOVE input-data-pairs (offset:) TO input-data-pairs

                   *> Process according to flag.
                   IF NOT invalid-flag
                       ADD val TO day-total, grand-total

                       ADD 1 TO day-accepted, total-accepted

                       IF max-data-gap < current-data-gap
                           MOVE current-data-gap TO max-data-gap
                           MOVE date-stamp TO max-data-gap-end
                       END-IF

                       MOVE ZERO TO current-data-gap
                   ELSE
                       ADD 1 TO current-data-gap, day-rejected,
                           total-rejected
                   END-IF
               END-PERFORM

               *> Display day stats.
               DIVIDE day-total BY day-accepted GIVING mean-val
               DISPLAY
                   date-stamp
                   " Reject: " day-rejected
                   " Accept: " day-accepted
                   " Average: " mean-val
               END-DISPLAY

               INITIALIZE day-rejected, day-accepted, mean-val,
                   day-total
           END-PERFORM

           CLOSE input-file

           *> Display overall stats.
           DISPLAY SPACE
           DISPLAY "File:         " INPUT-FILE-PATH
           DISPLAY "Total:        " grand-total
           DISPLAY "Readings:     " total-accepted

           DIVIDE grand-total BY total-accepted GIVING mean-val
           DISPLAY "Average:      " mean-val

           DISPLAY SPACE
           DISPLAY "Bad readings: " total-rejected
           DISPLAY "Maximum number of consecutive bad readings is "
               max-data-gap
           DISPLAY "Ends on date " max-data-gap-end

           GOBACK
           .
```


{{Out|Example output}}

```txt

1990-01-01 Reject: 00002 Accept: 00022 Average: 00000026.818
1990-01-02 Reject: 00000 Accept: 00024 Average: 00000017.083
1990-01-03 Reject: 00000 Accept: 00024 Average: 00000058.958
...
2004-12-29 Reject: 00001 Accept: 00023 Average: 00000002.447
2004-12-30 Reject: 00001 Accept: 00023 Average: 00000002.839
2004-12-31 Reject: 00001 Accept: 00023 Average: 00000002.056

File:         readings.txt
Total:        01358393.400
Readings:     00129403
Average:      00000010.497

Bad readings: 00001901
Maximum number of consecutive bad readings is 00000589
Ends on date 1993-03-05

```



## Common Lisp


```lisp
(defvar *invalid-count*)
(defvar *max-invalid*)
(defvar *max-invalid-date*)
(defvar *total-sum*)
(defvar *total-valid*)

(defun read-flag (stream date)
  (let ((flag (read stream)))
    (if (plusp flag)
        (setf *invalid-count* 0)
        (when (< *max-invalid* (incf *invalid-count*))
          (setf *max-invalid* *invalid-count*)
          (setf *max-invalid-date* date)))
    flag))

(defun parse-line (line)
  (with-input-from-string (s line)
    (let ((date (make-string 10)))
      (read-sequence date s)
      (cons date (loop repeat 24 collect (list (read s)
                                               (read-flag s date)))))))

(defun analyze-line (line)
  (destructuring-bind (date &rest rest) line
    (let* ((valid (remove-if-not #'plusp rest :key #'second))
           (n (length valid))
           (sum (apply #'+ (mapcar #'rationalize (mapcar #'first valid))))
           (avg (if valid (/ sum n) 0)))
      (incf *total-valid* n)
      (incf *total-sum* sum)
      (format t "Line: ~a  Reject: ~2d  Accept: ~2d  ~
                 Line_tot: ~8,3f  Line_avg: ~7,3f~%"
              date (- 24 n) n sum avg))))

(defun process (pathname)
  (let ((*invalid-count* 0) (*max-invalid* 0) *max-invalid-date*
        (*total-sum* 0) (*total-valid* 0))
    (with-open-file (f pathname)
      (loop for line = (read-line f nil nil)
            while line
            do (analyze-line (parse-line line))))
    (format t "~%File     = ~a" pathname)
    (format t "~&Total    = ~f" *total-sum*)
    (format t "~&Readings = ~a" *total-valid*)
    (format t "~&Average  = ~10,3f~%" (/ *total-sum* *total-valid*))
    (format t "~%Maximum run(s) of ~a consecutive false readings ends at ~
               line starting with date(s): ~a~%"
            *max-invalid* *max-invalid-date*)))
```

{{out|Example output}}

```txt
...
Line: 2004-12-29  Reject:  1  Accept: 23  Line_tot:   56.300  Line_avg:   2.448
Line: 2004-12-30  Reject:  1  Accept: 23  Line_tot:   65.300  Line_avg:   2.839
Line: 2004-12-31  Reject:  1  Accept: 23  Line_tot:   47.300  Line_avg:   2.057

File     = readings.txt
Total    = 1358393.4
Readings = 129403
Average  =     10.497

Maximum run(s) of 589 consecutive false readings ends at line starting with date(s): 1993-03-05
```



## D

{{trans|Python}}

```d
void main(in string[] args) {
    import std.stdio, std.conv, std.string;

    const fileNames = (args.length == 1) ? ["readings.txt"] :
                                           args[1 .. $];

    int noData, noDataMax = -1;
    string[] noDataMaxLine;

    double fileTotal = 0.0;
    int fileValues;

    foreach (const fileName; fileNames) {
        foreach (char[] line; fileName.File.byLine) {
            double lineTotal = 0.0;
            int lineValues;

            // Extract field info.
            const parts = line.split;
            const date = parts[0];
            const fields = parts[1 .. $];
            assert(fields.length % 2 == 0,
                   format("Expected even number of fields, not %d.",
                          fields.length));

            for (int i; i < fields.length; i += 2) {
                immutable value = fields[i].to!double;
                immutable flag = fields[i + 1].to!int;

                if (flag < 1) {
                    noData++;
                    continue;
                }

                // Check run of data-absent fields.
                if (noDataMax == noData && noData > 0)
                    noDataMaxLine ~= date.idup;

                if (noDataMax < noData && noData > 0) {
                    noDataMax = noData;
                    noDataMaxLine.length = 1;
                    noDataMaxLine[0] = date.idup;
                }

                // Re-initialise run of noData counter.
                noData = 0;

                // Gather values for averaging.
                lineTotal += value;
                lineValues++;
            }

            // Totals for the file so far.
            fileTotal += lineTotal;
            fileValues += lineValues;

            writefln("Line: %11s  Reject: %2d  Accept: %2d" ~
                     "  Line_tot: %10.3f  Line_avg: %10.3f",
                     date,
                     fields.length / 2 - lineValues,
                     lineValues,
                     lineTotal,
                     (lineValues > 0) ? lineTotal / lineValues : 0.0);
        }
    }

    writefln("\nFile(s)  = %-(%s, %)", fileNames);
    writefln("Total    = %10.3f", fileTotal);
    writefln("Readings = %6d", fileValues);
    writefln("Average  = %10.3f", fileTotal / fileValues);

    writefln("\nMaximum run(s) of %d consecutive false " ~
             "readings ends at line starting with date(s): %-(%s, %)",
             noDataMax, noDataMaxLine);
}
```

The output matches that of the [[#Python|Python]] version.


## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature

	make
			-- Summary statistics for 'hash'.
		local
			reject, accept, reading_total: INTEGER
			total, average, file_total: REAL
		do
			read_wordlist
			across
				hash as h
			loop
				io.put_string (h.key + "%T")
				reject := 0
				accept := 0
				total := 0
				across
					h.item as data
				loop
					if data.item.flag > 0 then
						accept := accept + 1
						total := total + data.item.val
					else
						reject := reject + 1
					end
				end
				file_total := file_total + total
				reading_total := reading_total + accept
				io.put_string ("accept: " + accept.out + "%Treject: " + reject.out + "%Ttotal: " + total.out + "%T")
				average := total / accept.to_real
				io.put_string ("average: " + average.out + "%N")
			end
			io.put_string ("File total: " + file_total.out + "%N")
			io.put_string ("Readings total: " + reading_total.out + "%N")
			find_longest_gap
		end

	find_longest_gap
			-- Longest gap (flag values <= 0).
		local
			count: INTEGER
			longest_gap: INTEGER
			end_date: STRING
		do
			create end_date.make_empty
			across
				hash as h
			loop
				across
					h.item as data
				loop
					if data.item.flag <= 0 then
						count := count + 1
					else
						if count > longest_gap then
							longest_gap := count
							end_date := h.key
						end
						count := 0
					end
				end
			end
			io.put_string ("%NThe longest gap is " + longest_gap.out + ". It ends at the date stamp " + end_date + ". %N")
		end

	original_list: STRING = "readings.txt"

	read_wordlist
			-- Preprocessed wordlist in 'hash'.
		local
			l_file: PLAIN_TEXT_FILE
			data: LIST [STRING]
			by_dates: LIST [STRING]
			date: STRING
			data_tup: TUPLE [val: REAL; flag: INTEGER]
			data_arr: ARRAY [TUPLE [val: REAL; flag: INTEGER]]
			i: INTEGER
		do
			create l_file.make_open_read_write (original_list)
			l_file.read_stream (l_file.count)
			data := l_file.last_string.split ('%N')
			l_file.close
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
						date.append ("_double_date_stamp")
						hash.put (data_arr, date)
					end
				end
			end
		end

	hash: HASH_TABLE [ARRAY [TUPLE [val: REAL; flag: INTEGER]], STRING]

end

```

{{out}}
Only the last three lines of the per line summary statistics are shown.

```txt

.
.
.
2004-12-29    accept: 23    reject: 1    total: 56.3    average: 2.44
2004-12-30    accept: 23    reject: 1    total: 65.3    average: 2.83
2004-12-31    accept: 23    reject: 1    total: 47.3    average: 2.05

File total: 1.35839e+006
Readings total: 129403

The longest gap is 589. It ends at the date stamp 1993-03-05.

```


```



## Erlang

The function file_contents/1 is used by [[Text_processing/2]]. Please update the user if you make any interface changes.


```Erlang

-module( text_processing ).

-export( [file_contents/1, main/1] ).

-record( acc, {failed={"", 0, 0}, files=[], ok=0, total=0} ).

file_contents( Name ) ->
	{ok, Binary} = file:read_file( Name ),
	[line_contents(X) || X <- binary:split(Binary, <<"\r\n">>, [global]), X =/= <<>>].

main( Files ) ->
      Acc = lists:foldl( fun file/2, #acc{}, Files ),
      {Failed_date, Failed, _Continuation} = Acc#acc.failed,
      io:fwrite( "~nFile(s)=~p~nTotal=~.2f~nReadings=~p~nAverage=~.2f~n~nMaximum run(s) of ~p consecutive false readings ends at line starting with date(s): ~p~n",
        [lists:reverse(Acc#acc.files), Acc#acc.total, Acc#acc.ok, Acc#acc.total / Acc#acc.ok, Failed, Failed_date] ).



file( Name, #acc{files=Files}=Acc ) ->
	try
	Line_contents = file_contents( Name ),
	lists:foldl( fun file_content_line/2, Acc#acc{files=[Name | Files]}, Line_contents )

	catch
	_:Error ->
		io:fwrite( "Error: Failed to read ~s: ~p~n", [Name, Error] ),
		Acc
	end.

file_content_line( {Date, Value_flags}, #acc{failed=Failed, ok=Ok, total=Total}=Acc ) ->
	New_failed = file_content_line_failed( Value_flags, Date, Failed ),
	{Sum, Oks, Average} = file_content_line_oks_0( [X || {X, ok} <- Value_flags] ),
	io:fwrite( "Line=~p\tRejected=~p\tAccepted=~p\tLine total=~.2f\tLine average=~.2f~n", [Date, erlang:length(Value_flags) - Oks, Oks, Sum, Average] ),
	Acc#acc{failed=New_failed, ok=Ok + Oks, total=Total + Sum}.

file_content_line_failed( [], Date, {_Failed_date, Failed, Acc} ) when Acc > Failed ->
        {Date, Acc, Acc};
file_content_line_failed( [], _Date, Failed ) ->
        Failed;
file_content_line_failed( [{_V, error} | T], Date, {Failed_date, Failed, Acc} ) ->
        file_content_line_failed( T,  Date, {Failed_date, Failed, Acc + 1} );
file_content_line_failed( [_H | T],  Date, {_Failed_date, Failed, Acc} ) when Acc > Failed ->
        file_content_line_failed( T, Date, {Date, Acc, 0} );
file_content_line_failed( [_H | T], Date, {Failed_date, Failed, _Acc} ) ->
        file_content_line_failed( T, Date, {Failed_date, Failed, 0} ).

file_content_line_flag( N ) when N > 0 -> ok;
file_content_line_flag( _N ) -> error.

file_content_line_oks_0( [] ) -> {0.0, 0, 0.0};
file_content_line_oks_0( Ok_value_flags ) ->
        Sum = lists:sum( Ok_value_flags ),
        Oks = erlang:length( Ok_value_flags ),
        {Sum, Oks, Sum / Oks}.

file_content_line_value_flag( Binary, {[], Acc} ) ->
        Flag = file_content_line_flag( erlang:list_to_integer(binary:bin_to_list(Binary)) ),
        {[Flag], Acc};
file_content_line_value_flag( Binary, {[Flag], Acc} ) ->
        Value = erlang:list_to_float( binary:bin_to_list(Binary) ),
        {[], [{Value, Flag} | Acc]}.

line_contents( Line ) ->
	[Date_binary | Rest] = binary:split( Line, <<"\t">>, [global] ),
	{_Previous, Value_flags} = lists:foldr( fun file_content_line_value_flag/2, {[], []}, Rest ), % Preserve order
	{binary:bin_to_list( Date_binary ), Value_flags}.

```

{{out}}

```txt

macbook-pro:rosettacode bengt$ escript text_processing.erl readings.txt
Line="1990-01-01"       Rejected=2      Accepted=22     Line total=590.00       Line average=26.82
Line="1990-01-02"       Rejected=0      Accepted=24     Line total=410.00       Line average=17.08
...
Line="2004-12-30"       Rejected=1      Accepted=23     Line total=65.30        Line average=2.84
Line="2004-12-31"       Rejected=1      Accepted=23     Line total=47.30        Line average=2.06

File(s)=["readings.txt"]
Total=1358393.40
Readings=129403
Average=10.50

Maximum run(s) of 589 consecutive false readings ends at line starting with date(s): "1993-03-05"

```



## Forth

{{works with|GNU Forth}}

```forth
\ data munging

\ 1991-03-30[\t10.000\t[-]1]*24

\ 1. mean of valid (flag > 0) values per day and overall
\ 2. length of longest run of invalid values, and when it happened

fvariable day-sum
variable day-n

fvariable total-sum
variable total-n

10 constant date-size    \ yyyy-mm-dd
create cur-date date-size allot

create bad-date date-size allot
variable bad-n

create worst-date date-size allot
variable worst-n

: split ( buf len char -- buf' l2  buf l1 )  \ where buf'[0] = char, l1 = len-l2
  >r 2dup r> scan
  2swap 2 pick - ;

: next-sample ( buf len -- buf' len' fvalue flag )
  #tab split >float   drop    1 /string
  #tab split snumber? drop >r 1 /string r> ;

: ok?  0> ;

: add-sample ( value -- )
  day-sum f@ f+ day-sum f!
  1 day-n +! ;

: add-day
  day-sum f@ total-sum f@ f+ total-sum f!
  day-n @ total-n +! ;

: add-bad-run
  bad-n @ 0= if
    cur-date bad-date date-size move
  then
  1 bad-n +! ;

: check-worst-run
  bad-n @ worst-n @ > if
    bad-n @ worst-n !
    bad-date worst-date date-size move
  then
  0 bad-n ! ;

: hour ( buf len -- buf' len' )
  next-sample ok? if
    add-sample
    check-worst-run
  else
    fdrop
    add-bad-run
  then ;

: .mean ( sum count -- ) 0 d>f f/ f. ;

: day ( line len -- )
  2dup + #tab swap c! 1+			\ append tab for parsing
  #tab split cur-date swap move 1 /string	\ skip date
  0e day-sum f!
  0  day-n !
  24 0 do hour loop 2drop
  cur-date date-size type ."  mean = "
  day-sum f@ day-n @ .mean cr
  add-day ;

stdin value input

: main
  s" input.txt" r/o open-file throw to input
  0e total-sum f!
  0 total-n !
  0 worst-n !
  begin  pad 512 input read-line throw
  while  pad swap day
  repeat
  input close-file throw
  worst-n @ if
    ."  Longest interruption: " worst-n @ .
    ." hours starting " worst-date date-size type cr
  then
  ."  Total mean = "
  total-sum f@ total-n @ .mean cr ;

main bye
```



## Fortran

Aside from formatted I/O, Fotran also offers free-format or "list-directed" I/O that accepts numerical data without many constraints - though complex numbers must be presented as (x,y) style. There are complications when character data are to be read in but this example does not involve that. Unfortunately, although the date part could be considered as three integers (with the hyphens separating the tokens), fortran's free-format scheme requires an actual delimiter, not an implied delimiter. If slashes were to be used, its behaviour is even less helpful, as the / is recognised as terminating the scan of the line! This may well allow comments to be added to data lines, but it makes reading such dates difficult. The free-format rule is that to read N data, input will be read from as many records as necessary until N data have been obtained. Should the last-read record have further data, they will not be seen by the next READ because it will start on a new record.

So, to handle this, the plan becomes to read the record into a CHARACTER variable, read the date part with a FORMAT statement working on the first ten characters, and then read the rest of the input via free-format. If the data field is corrupt (say, a letter in place of a digit) the ERR label will be selected. Similarly, when reading from a CHARACTER variable there is no read-next-record available, so if there is a shortage of data (a number is missing) the END label will be selected. A complaint is printed, a HIC counted, and if HIC is not too large, go back and try for the next record. This means that record count no longer relates to the number of values read, so a count of bad values is maintained as well. In the event, no such troublesome records were encountered in this example. An advantage of reading the record into a scratchpad is that any error messages can quote the text of the record. Similarly, tabs could be converted to spaces, etc. but this isn't needed as the free-format reading allows either, and commas. More generally, a much more comprehensive analysis of the record's text could be made should the need arise, most obviously that the date is a valid one, its hyphens are placed as expected, and the dates are in sequence without gaps. Another useful check is for the appearance of additional text beyond the twenty-four pairs specified - for example, a CRLF has been "lost" and the next record's content follows on the same line. Somehow. I suspect bungles with "cut&paste" hand editing, but the data suppliers never even apologised.

In decades of working with (half-)hourly data on the generation and consumption of electricity, it has been demonstrated that modern data suppliers endlessly manifest an inability to stick with a particular format, even one of their own design, and that the daylight savings changeover days (where there are ''not'' twenty-four hours in a day) surprass their competence annually. Persons from a mainframe background do a much more reliable job than those who have only tinkered with spreadsheets.

Incidentally, a daily average of a set of measurements may be unsuitable when data are missing, as when there is a regular pattern over a day. The N.Z. electricity supply association ruled that in calculating the ratio of daytime to nighttime usage, should there be four or more missing data in a day, then the entire day's data were to be rejected when computing the monthly or quarterly ratio.


```Fortran

Crunches a set of hourly data. Starts with a date, then 24 pairs of value,indicator for that day, on one line.
      INTEGER Y,M,D		!Year, month, and day.
      INTEGER GOOD(24)		!The indicators.
      REAL*8 V(24),VTOT,T	!The grist.
      INTEGER NV,N,NB		!Number of good values overall, and in a day.
      INTEGER I,NREC,HIC	!Some counters.
      INTEGER BI,BN,BBI,BBN	!Stuff to locate the longest run of bad data,
      CHARACTER*10 BDATE,BBDATE	!Along with the starting date.
      LOGICAL INGOOD		!State flipper for the runs of data.
      INTEGER IN,MSG		!I/O mnemonics.
      CHARACTER*666 ACARD	!Scratchpad, of sufficient length for all expectation.
      IN = 10		!Unit number for the input file.
      MSG = 6		!Output.
      OPEN (IN,FILE="Readings1.txt", FORM="FORMATTED",	!This should be a function.
     1 STATUS ="OLD",ACTION="READ")			!Returning success, or failure.
      NB = 0		!No bad values read.
      NV = 0		!Nor good values read.
      VTOT = 0		!Their average is to come.
      NREC = 0		!No records read.
      HIC = 0		!Provoking no complaints.
      INGOOD = .TRUE.	!I start in hope.
      BBN = 0		!And the longest previous bad run is short.
Chew into the file.
   10 READ (IN,11,END=100,ERR=666) L,ACARD(1:MIN(L,LEN(ACARD)))	!With some protection.
      NREC = NREC + 1		!So, a record has been read.
   11 FORMAT (Q,A)		!Obviously, Q ascertains the length of the record being read.
      READ (ACARD,12,END=600,ERR=601) Y,M,D	!The date part is trouble, as always.
   12 FORMAT (I4,2(1X,I2))				!Because there are no delimiters between the parts.
      READ (ACARD(11:L),*,END=600,ERR=601) (V(I),GOOD(I),I = 1,24)	!But after the date, delimiters abound.
Calculations. Could use COUNT(array) and SUM(array), but each requires its own pass through the array.
   20 T = 0		!Start on the day's statistics.
      N = 0		!No values yet.
      DO I = 1,24	!So, scan the cargo and do all the twiddling in one pass..
        IF (GOOD(I).GT.0) THEN	!A good value?
          N = N + 1		!Yes. Count it in.
          T = T + V(I)		!And augment for the average.
          IF (.NOT.INGOOD) THEN	!Had we been ungood?
            INGOOD = .TRUE.	!Yes. But now it changes.
            IF (BN.GT.BBN) THEN	!The run just ending: is it longer?
              BBN = BN		!Yes. Make it the new baddest.
              BBI = BI		!Recalling its start index,
              BBDATE = BDATE	!And its start date.
            END IF		!So much for bigger badness.
          END IF		!Now we're in good data.
         ELSE		!Otherwise, a bad value is upon us.
          IF (INGOOD) THEN	!Were we good?
            INGOOD = .FALSE.	!No longer. A new bad run is starting.
            BDATE = ACARD(1:10)	!Recall the date for this starter.
            BI = I		!And its index.
            BN = 0		!Start the run-length counter.
          END IF		!So much for a fall.
          BN = BN + 1	!Count another bad value.
        END IF		!Good or bad, so much for that value.
      END DO		!On to the next.
Commentary for the day's data..
      IF (N.LE.0) THEN	!I prefer to avoid dividing by zero.
        WRITE (MSG,21) NREC,ACARD(1:10)	!So, no average to report.
   21   FORMAT ("Record",I8," (",A,") has no good data!")	!Just a remark.
       ELSE			!But otherwise,
        WRITE(MSG,22) NREC,ACARD(1:10),N,T/N	!An average is possible.
   22   FORMAT("Record",I8," (",A,")",I3," good, average",F9.3)	!So here it is.
        NB = NB + 24 - N	!Count the bad by implication.
        NV = NV + N		!Count the good directly.
        VTOT = VTOT + T		!Should really sum deviations from a working average.
      END IF			!So much for that line.
      GO TO 10		!More! More! I want more!!

Complaints. Should really distinguish between trouble in the date part and in the data part.
  600 WRITE (MSG,*) '"END" declared - insufficient data?'	!Not enough numbers, presumably.
      GO TO 602				!Reveal the record.
  601 WRITE (MSG,*) '"ERR" declared - improper number format?'	!Ah, but which number?
  602 WRITE (MSG,603) NREC,L,ACARD(1:L)	!Anyway, reveal the uninterpreted record.
  603 FORMAT(" Record ",I0,", length ",I0," reads ",A)	!Just so.
      HIC = HIC + 1			!This may grow into a habit.
      IF (HIC.LE.12) GO TO 10		!But if not yet, try the next record.
      STOP "Enough distaste."		!Or, give up.
  666 WRITE (MSG,101) NREC,"format error!"	!For A-style data? Should never happen!
      GO TO 900				!But if it does, give up!

Closedown.
  100 WRITE (MSG,101) NREC,"then end-of-file"	!Discovered on the next attempt.
  101 FORMAT (" Record ",I0,": ",A)		!A record number plus a remark.
      WRITE (MSG,102) NV,NB,VTOT/NV		!The overall results.
  102 FORMAT (I8," values, ",I0," bad. Average",F9.4)	!This should do.
      IF (BBN.LE.0) THEN		!Now for a special report.
        WRITE (MSG,*) "No bad value presented, so no longest run."	!Unneeded!
       ELSE				!But actually, the example data has some bad values.
        WRITE (MSG,103) BBN,BBI,BBDATE	!And this is for the longest encountered.
  103   FORMAT ("Longest bad run: ",I0,", starting hour ",I0," on ",A)	!Just so.
      END IF			!Enough remarks.
  900 CLOSE(IN)		!Done.
      END	!Spaghetti rules.

```


Output:
 Record       1 (1990-01-01) 22 good, average   26.818
 Record       2 (1990-01-02) 24 good, average   17.083
 Record       3 (1990-01-03) 24 good, average   58.958
 ...etc.
 Record     945 (1992-08-01) has no good data!
 ...etc.
 Record    5471 (2004-12-31) 23 good, average    2.057
  Record 5471: then end-of-file
   129403 values, 1901 bad. Average  10.4974
 Longest bad run: 589, starting hour 2 on 1993-02-09


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
)

const (
	filename = "readings.txt"
	readings = 24             // per line
	fields   = readings*2 + 1 // per line
)

func main() {
	file, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()
	var (
		badRun, maxRun   int
		badDate, maxDate string
		fileSum          float64
		fileAccept       int
	)
	endBadRun := func() {
		if badRun > maxRun {
			maxRun = badRun
			maxDate = badDate
		}
		badRun = 0
	}
	s := bufio.NewScanner(file)
	for s.Scan() {
		f := strings.Fields(s.Text())
		if len(f) != fields {
			log.Fatal("unexpected format,", len(f), "fields.")
		}
		var accept int
		var sum float64
		for i := 1; i < fields; i += 2 {
			flag, err := strconv.Atoi(f[i+1])
			if err != nil {
				log.Fatal(err)
			}
			if flag <= 0 { // value is bad
				if badRun++; badRun == 1 {
					badDate = f[0]
				}
			} else { // value is good
				endBadRun()
				value, err := strconv.ParseFloat(f[i], 64)
				if err != nil {
					log.Fatal(err)
				}
				sum += value
				accept++
			}
		}
		fmt.Printf("Line: %s  Reject %2d  Accept: %2d  Line_tot:%9.3f",
			f[0], readings-accept, accept, sum)
		if accept > 0 {
			fmt.Printf("  Line_avg:%8.3f\n", sum/float64(accept))
		} else {
			fmt.Println()
		}
		fileSum += sum
		fileAccept += accept
	}
	if err := s.Err(); err != nil {
		log.Fatal(err)
	}
	endBadRun()

	fmt.Println("\nFile     =", filename)
	fmt.Printf("Total    = %.3f\n", fileSum)
	fmt.Println("Readings = ", fileAccept)
	if fileAccept > 0 {
		fmt.Printf("Average  =  %.3f\n", fileSum/float64(fileAccept))
	}
	if maxRun == 0 {
		fmt.Println("\nAll data valid.")
	} else {
		fmt.Printf("\nMax data gap = %d, beginning on line %s.\n",
			maxRun, maxDate)
	}
}
```

{{out}}

```txt

...
Line: 2004-12-28  Reject  1  Accept: 23  Line_tot:   77.800  Line_avg:   3.383
Line: 2004-12-29  Reject  1  Accept: 23  Line_tot:   56.300  Line_avg:   2.448
Line: 2004-12-30  Reject  1  Accept: 23  Line_tot:   65.300  Line_avg:   2.839
Line: 2004-12-31  Reject  1  Accept: 23  Line_tot:   47.300  Line_avg:   2.057

File     = readings.txt
Total    = 1358393.400
Readings =  129403
Average  = 10.497

Max data gap = 589, beginning on line 1993-02-09.

```



## Haskell


```Haskell
import Data.List
import Numeric
import Control.Arrow
import Control.Monad
import Text.Printf
import System.Environment
import Data.Function

type Date = String
type Value = Double
type Flag = Bool

readFlg :: String -> Flag
readFlg = (> 0).read

readNum :: String -> Value
readNum = fst.head.readFloat

take2 = takeWhile(not.null).unfoldr (Just.splitAt 2)

parseData :: [String] -> (Date,[(Value,Flag)])
parseData = head &&& map(readNum.head &&& readFlg.last).take2.tail

sumAccs :: (Date,[(Value,Flag)]) -> (Date, ((Value,Int),[Flag]))
sumAccs = second (((sum &&& length).concat.uncurry(zipWith(\v f -> [v|f])) &&& snd).unzip)

maxNAseq :: [Flag] -> [(Int,Int)]
maxNAseq = head.groupBy((==) `on` fst).sortBy(flip compare)
           . concat.uncurry(zipWith(\i (r,b)->[(r,i)|not b]))
           . first(init.scanl(+)0). unzip
           . map ((fst &&& id).(length &&& head)). group

main = do
    file:_ <- getArgs
    f <- readFile file
    let dat :: [(Date,((Value,Int),[Flag]))]
        dat      = map (sumAccs. parseData. words).lines $ f
        summ     = ((sum *** sum). unzip *** maxNAseq.concat). unzip $ map snd dat
        totalFmt = "\nSummary\t\t accept: %d\t total: %.3f \taverage: %6.3f\n\n"
        lineFmt  = "%8s\t accept: %2d\t total: %11.3f \taverage: %6.3f\n"
        maxFmt   =  "Maximum of %d consecutive false readings, starting on line /%s/ and ending on line /%s/\n"
-- output statistics
    putStrLn "\nSome lines:\n"
    mapM_ (\(d,((v,n),_)) -> printf lineFmt d n v (v/fromIntegral n)) $ take 4 $ drop 2200 dat
    (\(t,n) -> printf totalFmt  n t (t/fromIntegral n)) $ fst summ
    mapM_ ((\(l, d1,d2) -> printf maxFmt l d1 d2)
              . (\(a,b)-> (a,(fst.(dat!!).(`div`24))b,(fst.(dat!!).(`div`24))(a+b)))) $ snd summ
```

{{out}}

```Haskell
*Main> :main ["./RC/readings.txt"]
```


```txt
Some lines:

1996-01-11       accept: 24      total:     437.000     average: 18.208
1996-01-12       accept: 24      total:     536.000     average: 22.333
1996-01-13       accept: 24      total:    1062.000     average: 44.250
1996-01-14       accept: 24      total:     787.000     average: 32.792

Summary          accept: 129403  total: 1358393.400     average: 10.497

Maximum of 589 consecutive false readings, starting on line /1993-02-09/ and ending on line /1993-03-05/
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
record badrun(count,fromdate,todate)  # record to track bad runs

procedure main()
return mungetask1("readings1-input.txt","readings1-output.txt")
end

procedure mungetask1(fin,fout)

fin  := open(fin)      | stop("Unable to open input file ",fin)
fout := open(fout,"w") | stop("Unable to open output file ",fout)

F_tot := F_acc := F_rej := 0                       # data set totals
rejmax := badrun(-1)                               # longest reject runs
rejcur := badrun(0)                                # current reject runs

while line := read(fin) do {
   line ? {
      ldate := tab(many(&digits ++ '-'))           # date (poorly checked)
      fields := tot := rej := 0                    # record counters & totals

      while tab(many(' \t')) do {                  # whitespace before every pair
         value := real(tab(many(&digits++'-.')))   | stop("Bad value in ",ldate)
         tab(many(' \t'))
         flag := integer(tab(many(&digits++'-')))  | stop("Bad flag in ",ldate)
         fields +:= 1

         if flag > 0 then {                        # good data, ends a bad run
            if rejcur.count > rejmax.count then rejmax := rejcur
            rejcur := badrun(0)
            tot +:= value
            }
         else {                                    # bad (flagged) data
            if rejcur.count = 0 then rejcur.fromdate := ldate
            rejcur.todate := ldate
            rejcur.count +:= 1
            rej +:= 1
            }
         }
      }
   F_tot +:= tot
   F_acc +:= acc := fields - rej
   F_rej +:= rej
   write(fout,"Line: ",ldate," Reject: ", rej," Accept: ", acc," Line_tot: ",tot," Line_avg: ", if acc > 0 then tot / acc else 0)
   }

write(fout,"\nTotal    = ",F_tot,"\nReadings = ",F_acc,"\nRejects  = ",F_rej,"\nAverage  = ",F_tot / F_acc)
if rejmax.count > 0 then
   write(fout,"Maximum run of bad data was ",rejmax.count," readings from ",rejmax.fromdate," to ",rejmax.todate)
else
   write(fout,"No bad runs of data")
end
```

{{out|Sample output}}

```txt
...
Line: 2004-12-28 Reject: 1 Accept: 23 Line_tot: 77.80000000000001 Line_avg: 3.382608695652174
Line: 2004-12-29 Reject: 1 Accept: 23 Line_tot: 56.3 Line_avg: 2.447826086956522
Line: 2004-12-30 Reject: 1 Accept: 23 Line_tot: 65.3 Line_avg: 2.839130434782609
Line: 2004-12-31 Reject: 1 Accept: 23 Line_tot: 47.3 Line_avg: 2.056521739130435

Total    = 1358393.399999999
Readings = 129403
Rejects  = 1901
Average  = 10.49738723213526
Maximum run of bad data was 589 readings from 1993-02-09 to 1993-03-05
```



## J

'''Solution:'''

```j
  load 'files'
  parseLine=: 10&({. ,&< (_99&".;._1)@:}.)  NB. custom parser
  summarize=: # , +/ , +/ % #               NB. count,sum,mean
  filter=: #~ 0&<                           NB. keep valid measurements

  'Dates dat'=: |: parseLine;._2 CR -.~ fread jpath '~temp/readings.txt'
  Vals=:  (+: i.24){"1 dat
  Flags=: (>: +: i.24){"1 dat
  DailySummary=: Vals summarize@filter"1 Flags
  RunLengths=: ([: #(;.1) 0 , }. *. }:) , 0 >: Flags
   ]MaxRun=: >./ RunLengths
589
   ]StartDates=: Dates {~ (>:@I.@e.&MaxRun (24 <.@%~ +/)@{. ]) RunLengths
1993-03-05
```

'''Formatting Output'''

Define report formatting verbs:

```j
formatDailySumry=: dyad define
  labels=. , ];.2 'Line: Accept: Line_tot: Line_avg: '
  labels , x ,. 7j0 10j3 10j3 ": y
)
formatFileSumry=: dyad define
  labels=. ];.2 'Total: Readings: Average: '
  sumryvals=. (, %/) 1 0{ +/y
  out=. labels ,. 12j3 12j0 12j3 ":&> sumryvals
  'maxrun dates'=. x
  out=. out,LF,'Maximum run(s) of ',(": maxrun),' consecutive false readings ends at line(s) starting with date(s): ',dates
)
```

{{out|Show output}}

```j
   (_4{.Dates) formatDailySumry _4{. DailySummary
Line:     Accept:   Line_tot: Line_avg:
2004-12-28     23    77.800     3.383
2004-12-29     23    56.300     2.448
2004-12-30     23    65.300     2.839
2004-12-31     23    47.300     2.057

   (MaxRun;StartDates) formatFileSumry DailySummary
Total:     1358393.400
Readings:       129403
Average:        10.497

Maximum run(s) of 589 consecutive false readings ends at line(s) starting with date(s): 1993-03-05
```



## Java

{{works with|Java|7}}

```java
import java.io.File;
import java.util.*;
import static java.lang.System.out;

public class TextProcessing1 {

    public static void main(String[] args) throws Exception {
        Locale.setDefault(new Locale("en", "US"));
        Metrics metrics = new Metrics();

        int dataGap = 0;
        String gapBeginDate = null;
        try (Scanner lines = new Scanner(new File("readings.txt"))) {
            while (lines.hasNextLine()) {

                double lineTotal = 0.0;
                int linePairs = 0;
                int lineInvalid = 0;
                String lineDate;

                try (Scanner line = new Scanner(lines.nextLine())) {

                    lineDate = line.next();

                    while (line.hasNext()) {
                        final double value = line.nextDouble();
                        if (line.nextInt() <= 0) {
                            if (dataGap == 0)
                                gapBeginDate = lineDate;
                            dataGap++;
                            lineInvalid++;
                            continue;
                        }
                        lineTotal += value;
                        linePairs++;

                        metrics.addDataGap(dataGap, gapBeginDate, lineDate);
                        dataGap = 0;
                    }
                }
                metrics.addLine(lineTotal, linePairs);
                metrics.lineResult(lineDate, lineInvalid, linePairs, lineTotal);
            }
            metrics.report();
        }
    }

    private static class Metrics {
        private List<String[]> gapDates;
        private int maxDataGap = -1;
        private double total;
        private int pairs;
        private int lineResultCount;

        void addLine(double tot, double prs) {
            total += tot;
            pairs += prs;
        }

        void addDataGap(int gap, String begin, String end) {
            if (gap > 0 && gap >= maxDataGap) {
                if (gap > maxDataGap) {
                    maxDataGap = gap;
                    gapDates = new ArrayList<>();
                }
                gapDates.add(new String[]{begin, end});
            }
        }

        void lineResult(String date, int invalid, int prs, double tot) {
            if (lineResultCount >= 3)
                return;
            out.printf("%10s  out: %2d  in: %2d  tot: %10.3f  avg: %10.3f%n",
                    date, invalid, prs, tot, (prs > 0) ? tot / prs : 0.0);
            lineResultCount++;
        }

        void report() {
            out.printf("%ntotal    = %10.3f%n", total);
            out.printf("readings = %6d%n", pairs);
            out.printf("average  = %010.3f%n", total / pairs);
            out.printf("%nmaximum run(s) of %d invalid measurements: %n",
                    maxDataGap);
            for (String[] dates : gapDates)
                out.printf("begins at %s and ends at %s%n", dates[0], dates[1]);

        }
    }
}
```



```txt
1990-01-01  out:  2  in: 22  tot:    590.000  avg:     26.818
1990-01-02  out:  0  in: 24  tot:    410.000  avg:     17.083
1990-01-03  out:  0  in: 24  tot:   1415.000  avg:     58.958

total    = 1358393.400
readings = 129403
average  = 000010.497

maximum run(s) of 589 invalid measurements:
begins at 1993-02-09 and ends at 1993-03-05
```



## JavaScript

{{works with|JScript}}

```javascript
var filename = 'readings.txt';
var show_lines = 5;
var file_stats = {
    'num_readings': 0,
    'total': 0,
    'reject_run': 0,
    'reject_run_max': 0,
    'reject_run_date': ''
};

var fh = new ActiveXObject("Scripting.FileSystemObject").openTextFile(filename, 1); // 1 = for reading
while ( ! fh.atEndOfStream) {
    var line = fh.ReadLine();
    line_stats(line, (show_lines-- > 0));
}
fh.close();

WScript.echo(
    "\nFile(s)  = " + filename + "\n" +
    "Total    = " + dec3(file_stats.total) + "\n" +
    "Readings = " + file_stats.num_readings + "\n" +
    "Average  = " + dec3(file_stats.total / file_stats.num_readings) + "\n\n" +
    "Maximum run of " + file_stats.reject_run_max +
    " consecutive false readings ends at " + file_stats.reject_run_date
);

function line_stats(line, print_line) {
    var readings = 0;
    var rejects = 0;
    var total = 0;
    var fields = line.split('\t');
    var date = fields.shift();

    while (fields.length > 0) {
        var value = parseFloat(fields.shift());
        var flag = parseInt(fields.shift(), 10);
        readings++;
        if (flag <= 0) {
            rejects++;
            file_stats.reject_run++;
        }
        else {
            total += value;
            if (file_stats.reject_run > file_stats.reject_run_max) {
                file_stats.reject_run_max = file_stats.reject_run;
                file_stats.reject_run_date = date;
            }
            file_stats.reject_run = 0;
        }
    }

    file_stats.num_readings += readings - rejects;
    file_stats.total += total;

    if (print_line) {
        WScript.echo(
            "Line: " + date + "\t" +
            "Reject: " + rejects + "\t" +
            "Accept: " + (readings - rejects) + "\t" +
            "Line_tot: " + dec3(total) + "\t" +
            "Line_avg: " + ((readings == rejects) ? "0.0" : dec3(total / (readings - rejects)))
        );
    }
}

// round a number to 3 decimal places
function dec3(value) {
    return Math.round(value * 1e3) / 1e3;
}
```

{{out}}

```txt
Line: 1990-01-01        Reject: 2       Accept: 22      Line_tot: 590   Line_avg: 26.818
Line: 1990-01-02        Reject: 0       Accept: 24      Line_tot: 410   Line_avg: 17.083
Line: 1990-01-03        Reject: 0       Accept: 24      Line_tot: 1415  Line_avg: 58.958
Line: 1990-01-04        Reject: 0       Accept: 24      Line_tot: 1800  Line_avg: 75
Line: 1990-01-05        Reject: 0       Accept: 24      Line_tot: 1130  Line_avg: 47.083

File(s)  = readings.txt
Total    = 1358393.4
Readings = 129403
Average  = 10.497

Maximum run of 589 consecutive false readings ends at 1993-03-05
```



## jq

{{works with|jq|with foreach}}

This article highlights jq's recently added "foreach" and "inputs" filters,
as they allow the input file to be processed efficiently on a line-by-line basis,
with minimal memory requirements.

The "foreach" syntax is:

```jq
foreach STREAM as $row ( INITIAL; EXPRESSION; VALUE ).
```

The basic idea is that for each $row in STREAM, the value specified by VALUE is emitted.

If we wished only to produce per-line synopses of the "readings.txt"
file, the following pattern could be used:

```jq
foreach (inputs | split("\t")) as $line (INITIAL; EXPRESSION; VALUE)
```

In order to distinguish the single-line synopsis from the whole-file synopsis, we will use the following pattern instead:

```jq
foreach ((inputs | split("\t")), null) as $line (INITIAL; EXPRESSION; VALUE)
```

The "null" is added so that the stream of per-line values can be distinguished from the last value in the stream.

In this section, the whole-file synopsis is focused on the runs of lines having at least one flag<=0.  The maximal length of such runs is computed, and the starting line(s) and date(s) of all such runs are recorded.

One point of interest in the following program is the use of JSON objects to store values.  This allows mnemonic names to be used instead of local variables.

```jq
# Input: { "max":         max_run_length,
#          "starts":      array_of_start_line_values, # of all the maximal runs
#          "start_dates": array_of_start_dates        # of all the maximal runs
#        }
def report:
  (.starts | length) as $l
  | if $l == 1 then
      "There is one maximal run of lines with flag<=0.",
      "The maximal run has length \(.max) and starts at line \(.starts[0]) and has start date \(.start_dates[0])."
    elif $l == 0 then
      "There is no lines with flag<=0."
    else
      "There are \($l) maximal runs of lines with flag<=0.",
      "These runs have length \(.max) and start at the following line numbers:",
      "\(.starts)",
      "The corresponding dates are:",
      "\(.start_dates)"
    end;

# "process" processes "tab-separated string values" on stdin
def process:

  # Given a line in the form of an array [date, datum1, flag2, ...],
  # "synopsis" returns [ number of data items on the line with flag>0, sum, number of data items on the line with flag<=0 ]
  def synopsis: # of a line
    . as $row
    | reduce range(0; (length - 1) / 2) as $i
        ( [0,0,0];
          ($row[1+ (2*$i)] | tonumber) as $datum
          | ($row[2+(2*$i)] | tonumber) as $flag
         | if ($flag>0) then .[0] += 1 | .[1] += $datum else .[2] += 1 end );

  # state: {"line":       line_number # (first line is line 0)
  #         "synopis":    _,      # value returned by "synopsis"
  #         "start":      line_number_of_start_of_current_run,
  #         "start_date": date_of_start_of_current_run,
  #         "length":     length_of_current_run # so far
  #         "max":        max_run_length        # so far
  #         "starts":     array_of_start_values # of all the maximal runs
  #         "start_dates": array_of_start_dates # of all the maximal runs
  #         }
  foreach ((inputs | split("\t")), null) as $line  # null signals END
      # Slots are effectively initialized by default to null
      ( { "line": -1, "length": 0, "max": 0, "starts": [], "start_dates": [] };
        if $line == null then .line = null
	else
	  .line += 1
	  # | debug
          # synopsis returns [number with flag>0, sum, number with flag<=0 ]
          | .synopsis = ($line | synopsis)
          | if .synopsis[2] > 0 then
	      if .start then . else .start = .line | .start_date = $line[0] end
	      | .length += 1
	      | if .max < .length then
	          (.max = .length)
		  | .starts = [ .start ]
		  | .start_dates = [ .start_date ]
	        elif .max == .length then
		  .starts += [ .start ]
		  | .start_dates += [ .start_date ]
	        else .
	        end
	    else .start = null | .length = 0
	    end
	 end;
	.)
  | if .line == null then {max, starts, start_dates} | report
    else .synopsis
    end;

process
```


{{out}}

```sh
$ jq -c -n -R -r -f Text_processing_1.jq readings.txt
[22,590,2]
[24,410,0]
...
[23,47.3,1]
There is one maximal run of lines with flag<=0.
The maximal run has length 93 and starts at line 5378 and has start date 2004-09-30.
```



## Julia


```julia

using DataFrames

function mungdata(filename)
    lines = readlines(filename)
    numlines = length(lines)
    dates = Array{DateTime, 1}(numlines)
    means = zeros(Float64, numlines)
    numvalid = zeros(Int, numlines)
    invalidlength = zeros(Int, numlines)
    invalidpos = zeros(Int, numlines)
    datamatrix = Array{Float64,2}(numlines, 24)
    datamatrix .= NaN
    totalsum = 0.0
    totalgood = 0
    for (linenum,line) in enumerate(lines)
        data = split(line)
        validcount = badlength = 0
        validsum = 0.0
        for i in 2:2:length(data)-1
            if parse(Int, data[i+1]) >= 0
                validsum += (datamatrix[linenum, Int(i/2)] = parse(Float64, data[i]))
                validcount += 1
                badlength = 0
            else
                badlength += 1
                if badlength > invalidlength[linenum]
                    invalidlength[linenum] = badlength
                    invalidpos[linenum] = Int(i/2) - invalidlength[linenum] + 1
                end
            end
        end
        dates[linenum] = DateTime(data[1], "y-m-d")
        means[linenum] = validsum / validcount
        numvalid[linenum] = validcount
        totalsum += validsum
        totalgood += validcount
    end
    dt = DataFrame(Date = dates, Mean = means, ValidValues = numvalid,
                   MaximumGap = invalidlength, GapPosition = invalidpos)
    for i in 1:size(datamatrix)[2]
        dt[Symbol("$(i-1):00")] = datamatrix[:,i]
    end
    dt, totalsum/totalgood
end

datafilename = "data.txt" # this is taken from the example listed on the task, since the actual text file is not available
df, dmean = mungdata(datafilename)
println(df)
println("The overall mean is $dmean")
maxbadline = indmax(df[:MaximumGap])
maxbadval = df[:MaximumGap][maxbadline]
maxbadtime = df[:GapPosition][maxbadline] - 1
maxbaddate = replace("$(df[:Date][maxbadline])", r"T.+$", "")
println("The largest run of bad values is $(maxbadval), on $(maxbaddate) beginning at $(maxbadtime):00 hours.")

```

{{output}}

```txt

6×29 DataFrames.DataFrame
│ Row │ Date                │ Mean    │ ValidValues │ MaximumGap │ GapPosition │ 0:00 │ 1:00 │ 2:00 │ 3:00 │ 4:00 │
├─────┼─────────────────────┼─────────┼─────────────┼────────────┼─────────────┼──────┼──────┼──────┼──────┼──────┤
│ 1   │ 1991-03-30T00:00:00 │ 10.0    │ 24          │ 0          │ 0           │ 10.0 │ 10.0 │ 10.0 │ 10.0 │ 10.0 │
│ 2   │ 1991-03-31T00:00:00 │ 23.5417 │ 24          │ 0          │ 0           │ 10.0 │ 10.0 │ 10.0 │ 10.0 │ 10.0 │
│ 3   │ 1991-03-31T00:00:00 │ 40.0    │ 1           │ 23         │ 2           │ 40.0 │ NaN  │ NaN  │ NaN  │ NaN  │
│ 4   │ 1991-04-01T00:00:00 │ 23.2174 │ 23          │ 1          │ 1           │ NaN  │ 13.0 │ 16.0 │ 21.0 │ 24.0 │
│ 5   │ 1991-04-02T00:00:00 │ 19.7917 │ 24          │ 0          │ 0           │ 8.0  │ 9.0  │ 11.0 │ 12.0 │ 12.0 │
│ 6   │ 1991-04-03T00:00:00 │ 13.9583 │ 24          │ 0          │ 0           │ 10.0 │ 9.0  │ 10.0 │ 10.0 │ 9.0  │

│ Row │ 5:00 │ 6:00 │ 7:00 │ 8:00 │ 9:00 │ 10:00 │ 11:00 │ 12:00 │ 13:00 │ 14:00 │ 15:00 │ 16:00 │ 17:00 │ 18:00 │
├─────┼──────┼──────┼──────┼──────┼──────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤
│ 1   │ 10.0 │ 10.0 │ 10.0 │ 10.0 │ 10.0 │ 10.0  │ 10.0  │ 10.0  │ 10.0  │ 10.0  │ 10.0  │ 10.0  │ 10.0  │ 10.0  │
│ 2   │ 10.0 │ 10.0 │ 20.0 │ 20.0 │ 20.0 │ 35.0  │ 50.0  │ 60.0  │ 40.0  │ 30.0  │ 30.0  │ 30.0  │ 25.0  │ 20.0  │
│ 3   │ NaN  │ NaN  │ NaN  │ NaN  │ NaN  │ NaN   │ NaN   │ NaN   │ NaN   │ NaN   │ NaN   │ NaN   │ NaN   │ NaN   │
│ 4   │ 22.0 │ 20.0 │ 18.0 │ 29.0 │ 44.0 │ 50.0  │ 43.0  │ 38.0  │ 27.0  │ 27.0  │ 24.0  │ 23.0  │ 18.0  │ 12.0  │
│ 5   │ 12.0 │ 27.0 │ 26.0 │ 27.0 │ 33.0 │ 32.0  │ 31.0  │ 29.0  │ 31.0  │ 25.0  │ 25.0  │ 24.0  │ 21.0  │ 17.0  │
│ 6   │ 10.0 │ 15.0 │ 24.0 │ 28.0 │ 24.0 │ 18.0  │ 14.0  │ 12.0  │ 13.0  │ 14.0  │ 15.0  │ 14.0  │ 15.0  │ 13.0  │

│ Row │ 19:00 │ 20:00 │ 21:00 │ 22:00 │ 23:00 │
├─────┼───────┼───────┼───────┼───────┼───────┤
│ 1   │ 10.0  │ 10.0  │ 10.0  │ 10.0  │ 10.0  │
│ 2   │ 20.0  │ 20.0  │ 20.0  │ 20.0  │ 35.0  │
│ 3   │ NaN   │ NaN   │ NaN   │ NaN   │ NaN   │
│ 4   │ 13.0  │ 14.0  │ 15.0  │ 13.0  │ 10.0  │
│ 5   │ 14.0  │ 15.0  │ 12.0  │ 12.0  │ 10.0  │
│ 6   │ 13.0  │ 13.0  │ 12.0  │ 10.0  │ 10.0  │
The overall mean is 18.241666666666667
The largest run of bad values is 23, on 1991-03-31 beginning at 1:00 hours.

```



## Kotlin


```scala
// version 1.2.31

import java.io.File

fun main(args: Array<String>) {
    val rx = Regex("""\s+""")
    val file = File("readings.txt")
    val fmt = "Line:  %s  Reject: %2d  Accept: %2d  Line_tot: %7.3f  Line_avg: %7.3f"
    var grandTotal = 0.0
    var readings = 0
    var date = ""
    var run = 0
    var maxRun = -1
    var finishLine = ""
    file.forEachLine { line ->
        val fields = line.split(rx)
        date = fields[0]
        if (fields.size == 49) {
            var accept = 0
            var total = 0.0
            for (i in 1 until fields.size step 2) {
                if (fields[i + 1].toInt() >= 1) {
                    accept++
                    total += fields[i].toDouble()
                    if (run > maxRun) {
                        maxRun = run
                        finishLine = date
                    }
                    run = 0
                }
                else run++
            }
            grandTotal += total
            readings += accept
            println(fmt.format(date, 24 - accept, accept, total, total / accept))
        }
        else println("Line:  $date does not have 49 fields and has been ignored")
    }

    if (run > maxRun) {
        maxRun = run
        finishLine = date
    }
    val average = grandTotal / readings
    println("\nFile     = ${file.name}")
    println("Total    = ${"%7.3f".format(grandTotal)}")
    println("Readings = $readings")
    println("Average  = ${"%-7.3f".format(average)}")
    println("\nMaximum run of $maxRun consecutive false readings")
    println("ends at line starting with date: $finishLine")
}
```


{{out}}
Abbreviated output:

```txt

Line:  1990-01-01  Reject:  2  Accept: 22  Line_tot: 590.000  Line_avg:  26.818
Line:  1990-01-02  Reject:  0  Accept: 24  Line_tot: 410.000  Line_avg:  17.083
Line:  1990-01-03  Reject:  0  Accept: 24  Line_tot: 1415.000  Line_avg:  58.958
Line:  1990-01-04  Reject:  0  Accept: 24  Line_tot: 1800.000  Line_avg:  75.000
Line:  1990-01-05  Reject:  0  Accept: 24  Line_tot: 1130.000  Line_avg:  47.083
....
Line:  2004-12-27  Reject:  1  Accept: 23  Line_tot:  57.100  Line_avg:   2.483
Line:  2004-12-28  Reject:  1  Accept: 23  Line_tot:  77.800  Line_avg:   3.383
Line:  2004-12-29  Reject:  1  Accept: 23  Line_tot:  56.300  Line_avg:   2.448
Line:  2004-12-30  Reject:  1  Accept: 23  Line_tot:  65.300  Line_avg:   2.839
Line:  2004-12-31  Reject:  1  Accept: 23  Line_tot:  47.300  Line_avg:   2.057

File     = readings.txt
Total    = 1358393.400
Readings = 129403
Average  = 10.497

Maximum run of 589 consecutive false readings
ends at line starting with date: 1993-03-05

```



## Lua


```Lua
filename = "readings.txt"
io.input( filename )

file_sum, file_cnt_data, file_lines = 0, 0, 0
max_rejected, n_rejected = 0, 0
max_rejected_date, rejected_date = "", ""

while true do
    data = io.read("*line")
    if data == nil then break end

    date = string.match( data, "%d+%-%d+%-%d+" )
    if date == nil then break end

    val = {}
    for w in string.gmatch( data, "%s%-*%d+[%.%d]*" ) do
        val[#val+1] = tonumber(w)
    end

    sum, cnt = 0, 0
    for i = 1, #val, 2 do
    	if val[i+1] > 0 then
    	    sum = sum + val[i]
    	    cnt = cnt + 1
    	    n_rejected = 0
    	else
	    if n_rejected == 0 then
	        rejected_date = date
  	    end
    	    n_rejected = n_rejected + 1
    	    if n_rejected > max_rejected then
    	        max_rejected = n_rejected
    	        max_rejected_date = rejected_date
    	    end
    	end
    end

    file_sum = file_sum + sum
    file_cnt_data = file_cnt_data + cnt
    file_lines = file_lines + 1

    print( string.format( "%s:\tRejected: %d\tAccepted: %d\tLine_total: %f\tLine_average: %f", date, #val/2-cnt, cnt, sum, sum/cnt ) )
end

print( string.format( "\nFile:\t  %s", filename ) )
print( string.format( "Total:\t  %f", file_sum ) )
print( string.format( "Readings: %d", file_lines ) )
print( string.format( "Average:  %f", file_sum/file_cnt_data ) )
print( string.format( "Maximum %d consecutive false readings starting at %s.", max_rejected, max_rejected_date ) )
```


```txt
Output:
File:	  readings.txt
Total:	  1358393.400000
Readings: 5471
Average:  10.497387
Maximum 589 consecutive false readings starting at 1993-02-09.
```



## Mathematica


```Mathematica
FileName = "Readings.txt"; data = Import[FileName,"TSV"];

Scan[(a=Position[#[[3;;All;;2]],1];
Print["Line:",#[[1]] ,"\tReject:", 24 - Length[a], "\t Accept:", Length[a], "\tLine_tot:",
Total@Part[#, Flatten[2*a]] , "\tLine_avg:", Total@Part[#, Flatten[2*a]]/Length[a]])&, data]

GlobalSum = Nb = Running = MaxRunRecorded =  0; MaxRunTime = {};
Scan[ For[i = 3, i < 50, i = i + 2,
   If[#[[i]] == 1,
    Running=0; GlobalSum += #[[i-1]]; Nb++;,
    Running ++;  If[MaxRunRecorded < Running, MaxRunRecorded = Running;MaxRunTime={ #[[1]]}; ];
   ]] &, data ]

Print["\nFile(s) : ",FileName,"\nTotal : ",AccountingForm@GlobalSum,"\nReadings : ",Nb,
"\nAverage : ",GlobalSum/Nb,"\n\nMaximum run(s) of ",MaxRunRecorded,
" consecutive false readings ends at line starting with date(s):",MaxRunTime]
```



```txt
Line:1990-01-01	Reject:2	 Accept:22	Line_tot:590.	Line_avg:26.8182
Line:1990-01-02	Reject:0	 Accept:24	Line_tot:410.	Line_avg:17.0833
Line:1990-01-03	Reject:0	 Accept:24	Line_tot:1415.	Line_avg:58.9583
Line:1990-01-04	Reject:0	 Accept:24	Line_tot:1800.	Line_avg:75.
Line:1990-01-05	Reject:0	 Accept:24	Line_tot:1130.	Line_avg:47.0833
....

File(s) : Readings.txt
Total : 1358393.
Readings : 129403
Average : 10.4974

Maximum run(s) of 589 consecutive false readings ends at line starting with date(s):{1993-03-05}
```



## Nim

{{trans|Python}}

```nim
import os, strutils, sequtils

var
  nodata = 0
  nodataMax = -1
  nodataMaxLine:seq[string] = @[]

  totFile = 0.0
  numFile = 0

for filename in commandLineParams():
  var f = open(filename)
  for line in f.lines:
    var
      totLine = 0.0
      numLine = 0
      field = line.split()
      date = field[0]
      data: seq[float] = @[]
      flags: seq[int] = @[]

    for i, f in field[1 .. -1]:
      if i mod 2 == 0: data.add parseFloat(f)
      else: flags.add parseInt(f)

    for datum, flag in items(zip(data, flags)):
      if flag < 1:
        inc nodata
      else:
        if nodataMax == nodata and nodata > 0:
          nodataMaxLine.add date
        if nodataMax < nodata and nodata > 0:
          nodataMax = nodata
          nodataMaxLine = @[date]
        nodata = 0
        totLine += datum
        inc numLine

    totFile += totLine
    numFile += numLine

    echo "Line: $#  Reject: $#  Accept: $#  LineTot: $# LineAvg: $#"
      .format(date, data.len - numLine, numLine,
              formatFloat(totLine, precision = 0), formatFloat(
        (if numLine > 0: totLine / float(numLine) else: 0.0), precision = 0))

echo ""
echo "File(s)   = ", commandLineParams().join(" ")
echo "Total     = ", formatFloat(totFile, precision = 0)
echo "Readings  = ", numFile
echo "Average   = ", formatFloat(totFile / float(numFile), precision = 0)
echo ""
echo "Maximum run(s) of ", nodataMax, " consecutive false readings ends at line starting with date(s): ", nodataMaxLine.join(" ")
```

Output:

```txt
$ ./textproc1 readings.txt|tail
Line: 2004-12-29  Reject: 1  Accept: 23  LineTot: 56.3 LineAvg: 2.44783
Line: 2004-12-30  Reject: 1  Accept: 23  LineTot: 65.3 LineAvg: 2.83913
Line: 2004-12-31  Reject: 1  Accept: 23  LineTot: 47.3 LineAvg: 2.05652

File(s)   = readings.txt
Total     = 1.35839e+06
Readings  = 129403
Average   = 10.4974

Maximum run(s) of 589 consecutive false readings ends at line starting with date(s): 1993-03-05
```



## OCaml


```ocaml
let input_line ic =
  try Some(input_line ic)
  with End_of_file -> None

let fold_input f ini ic =
  let rec fold ac =
    match input_line ic with
    | Some line -> fold (f ac line)
    | None -> ac
  in
  fold ini

let ic = open_in "readings.txt"

let scan line =
  Scanf.sscanf line "%s\
    \t%f\t%d\t%f\t%d\t%f\t%d\t%f\t%d\t%f\t%d\t%f\t%d\
    \t%f\t%d\t%f\t%d\t%f\t%d\t%f\t%d\t%f\t%d\t%f\t%d\
    \t%f\t%d\t%f\t%d\t%f\t%d\t%f\t%d\t%f\t%d\t%f\t%d\
    \t%f\t%d\t%f\t%d\t%f\t%d\t%f\t%d\t%f\t%d\t%f\t%d"
    (fun date
         v1  f1  v2  f2  v3  f3  v4  f4  v5  f5  v6  f6
         v7  f7  v8  f8  v9  f9  v10 f10 v11 f11 v12 f12
         v13 f13 v14 f14 v15 f15 v16 f16 v17 f17 v18 f18
         v19 f19 v20 f20 v21 f21 v22 f22 v23 f23 v24 f24 ->
      (date),
      [ (v1,  f1 ); (v2,  f2 ); (v3,  f3 ); (v4,  f4 ); (v5,  f5 ); (v6,  f6 );
        (v7,  f7 ); (v8,  f8 ); (v9,  f9 ); (v10, f10); (v11, f11); (v12, f12);
        (v13, f13); (v14, f14); (v15, f15); (v16, f16); (v17, f17); (v18, f18);
        (v19, f19); (v20, f20); (v21, f21); (v22, f22); (v23, f23); (v24, f24); ])

let tot_file, num_file, _, nodata_max, nodata_maxline =
  fold_input
    (fun (tot_file, num_file, nodata, nodata_max, nodata_maxline) line ->
       let date, datas = scan line in
       let _datas = List.filter (fun (_, flag) -> flag > 0) datas in
       let ok = List.length _datas in
       let tot = List.fold_left (fun ac (value, _) -> ac +. value) 0.0 _datas in
       let nodata, nodata_max, nodata_maxline =
         List.fold_left
             (fun (nodata, nodata_max, nodata_maxline) (_, flag) ->
                if flag <= 0
                then (succ nodata, nodata_max, nodata_maxline)
                else
                  if nodata_max = nodata && nodata > 0
                  then (0, nodata_max, date::nodata_maxline)
                  else if nodata_max < nodata && nodata > 0
                  then (0, nodata, [date])
                  else (0, nodata_max, nodata_maxline)
             )
             (nodata, nodata_max, nodata_maxline) datas in
       Printf.printf "Line: %s" date;
       Printf.printf "  Reject: %2d  Accept: %2d" (24 - ok) ok;
       Printf.printf "\tLine_tot: %8.3f" tot;
       Printf.printf "\tLine_avg: %8.3f\n" (tot /. float ok);
       (tot_file +. tot, num_file + ok, nodata, nodata_max, nodata_maxline))
    (0.0, 0, 0, 0, [])
    ic ;;

close_in ic ;;

Printf.printf "Total    = %f\n" tot_file;
Printf.printf "Readings = %d\n" num_file;
Printf.printf "Average  = %f\n" (tot_file /. float num_file);
Printf.printf "Maximum run(s) of %d consecutive false readings \
               ends at line starting with date(s): %s\n"
               nodata_max (String.concat ", " nodata_maxline);
```



## Perl

===An AWK-like solution===

```perl
use strict;
use warnings;

my $nodata = 0;               # Current run of consecutive flags<0 in lines of file
my $nodata_max = -1;          # Max consecutive flags<0 in lines of file
my $nodata_maxline = "!";     # ... and line number(s) where it occurs

my $infiles = join ", ", @ARGV;

my $tot_file = 0;
my $num_file = 0;

while (<>) {
  chomp;
  my $tot_line = 0;             # sum of line data
  my $num_line = 0;             # number of line data items with flag>0
  my $rejects  = 0;

  # extract field info, skipping initial date field
  my ($date, @fields) = split;
  while (@fields and my ($datum, $flag) = splice @fields, 0, 2) {
    if ($flag+1 < 2) {
      $nodata++;
      $rejects++;
      next;
    }

    # check run of data-absent fields
    if($nodata_max == $nodata and $nodata > 0){
      $nodata_maxline = "$nodata_maxline, $date";
    }
    if($nodata_max < $nodata and $nodata > 0){
      $nodata_max = $nodata;
      $nodata_maxline = $date;
    }
    # re-initialise run of nodata counter
    $nodata = 0;
    # gather values for averaging
    $tot_line += $datum;
    $num_line++;
  }

  # totals for the file so far
  $tot_file += $tot_line;
  $num_file += $num_line;

  printf "Line: %11s  Reject: %2i  Accept: %2i  Line_tot: %10.3f  Line_avg: %10.3f\n",
         $date, $rejects, $num_line, $tot_line, ($num_line>0)? $tot_line/$num_line: 0;

}

printf "\n";
printf "File(s)  = %s\n", $infiles;
printf "Total    = %10.3f\n", $tot_file;
printf "Readings = %6i\n", $num_file;
printf "Average  = %10.3f\n", $tot_file / $num_file;

printf "\nMaximum run(s) of %i consecutive false readings ends at line starting with date(s): %s\n",
       $nodata_max, $nodata_maxline;
```

{{out|Sample output}}

```txt
bash$ perl -f readings.pl readings.txt | tail
Line:  2004-12-29  Reject:  1  Accept: 23  Line_tot:     56.300  Line_avg:      2.448
Line:  2004-12-30  Reject:  1  Accept: 23  Line_tot:     65.300  Line_avg:      2.839
Line:  2004-12-31  Reject:  1  Accept: 23  Line_tot:     47.300  Line_avg:      2.057

File(s)  = readings.txt
Total    = 1358393.400
Readings = 129403
Average  =     10.497

Maximum run(s) of 589 consecutive false readings ends at line starting with date(s): 1993-03-05
bash$
```


===An object-oriented solution===

```perl
use strict;
use warnings;

use constant RESULT_TEMPLATE => "%-19s = %12.3f / %-6u = %.3f\n";

my $parser = Parser->new;

# parse lines and print results
printf RESULT_TEMPLATE, $parser->parse(split)
    while <>;

$parser->finish;

# print total and summary
printf "\n".RESULT_TEMPLATE."\n", $parser->result;
printf "the maximum of %u consecutive bad values was reached %u time(s)\n",
    $parser->bad_max, scalar $parser->bad_ranges;

# print bad ranges
print for map { '  '.join(' - ', @$_)."\n" } $parser->bad_ranges;

BEGIN {
    package main::Parser;

    sub new {
        my $obj = {
            SUM => 0,
            COUNT => 0,
            CURRENT_DATE => undef,
            BAD_DATE => undef,
            BAD_RANGES => [],
            BAD_MAX => 0,
            BAD_COUNT => 0
        };

        return bless $obj;
    }

    sub _average {
        my ($sum, $count) = @_;
        return ($sum, $count, $count && $sum / $count);
    }

    sub _push_bad_range_if_necessary {
        my ($parser) = @_;
        my ($count, $max) = @$parser{qw(BAD_COUNT BAD_MAX)};

        return if $count < $max;

        if ($count > $max) {
            $parser->{BAD_RANGES} = [];
            $parser->{BAD_MAX} = $count;
        }

        push @{$parser->{BAD_RANGES}}, [ @$parser{qw(BAD_DATE CURRENT_DATE)} ];
    }

    sub _check {
        my ($parser, $flag) = @_;
        if ($flag <= 0) {
            ++$parser->{BAD_COUNT};
            $parser->{BAD_DATE} = $parser->{CURRENT_DATE}
                unless defined $parser->{BAD_DATE};

            return 0;
        }
        else {
            $parser->_push_bad_range_if_necessary;
            $parser->{BAD_COUNT} = 0;
            $parser->{BAD_DATE} = undef;
            return 1;
        }
    }

    sub bad_max {
        my ($parser) = @_;
        return $parser->{BAD_MAX}
    }

    sub bad_ranges {
        my ($parser) = @_;
        return @{$parser->{BAD_RANGES}}
    }

    sub parse {
        my $parser = shift;
        my $date = shift;

        $parser->{CURRENT_DATE} = $date;

        my $sum = 0;
        my $count = 0;

        while (my ($value, $flag) = splice @_, 0, 2) {
            next unless $parser->_check($flag);
            $sum += $value;
            ++$count;
        }

        $parser->{SUM} += $sum;
        $parser->{COUNT} += $count;

        return ("average($date)", _average($sum, $count));
    }

    sub result {
        my ($parser) = @_;
        return ('total-average', _average(@$parser{qw(SUM COUNT)}));
    }

    sub finish {
        my ($parser) = @_;
        $parser->_push_bad_range_if_necessary
    }
}
```

{{out|Sample output}}

```txt
$ perl readings.pl < readings.txt | tail
average(2004-12-27) =       57.100 / 23     = 2.483
average(2004-12-28) =       77.800 / 23     = 3.383
average(2004-12-29) =       56.300 / 23     = 2.448
average(2004-12-30) =       65.300 / 23     = 2.839
average(2004-12-31) =       47.300 / 23     = 2.057

total-average       =  1358393.400 / 129403 = 10.497

the maximum of 589 consecutive bad values was reached 1 time(s)
  1993-02-09 - 1993-03-05

$
```



## Perl 6


```perl6
my @gaps;
my $previous = 'valid';

for $*IN.lines -> $line {
    my ($date, @readings) = split /\s+/, $line;
    my @valid;
    my $hour = 0;
    for @readings -> $reading, $flag {
        if $flag > 0 {
            @valid.push($reading);
            if $previous eq 'invalid' {
                @gaps[*-1]{'end'} = "$date $hour:00";
                $previous = 'valid';
            }
        }
        else
        {
            if $previous eq 'valid' {
                @gaps.push( {start => "$date $hour:00"} );
            }
            @gaps[*-1]{'count'}++;
            $previous = 'invalid';
        }
        $hour++;
    }
    say "$date: { ( +@valid ?? ( ( [+] @valid ) / +@valid ).fmt("%.3f") !! 0 ).fmt("%8s") }",
        " mean from { (+@valid).fmt("%2s") } valid.";
};

my $longest = @gaps.sort({-$^a<count>})[0];

say "Longest period of invalid readings was {$longest<count>} hours,\n",
    "from {$longest<start>} till {$longest<end>}."
```

{{out}}

```txt

1990-01-01:   26.818 mean from 22 valid.
1990-01-02:   17.083 mean from 24 valid.
1990-01-03:   58.958 mean from 24 valid.
1990-01-04:   75.000 mean from 24 valid.
1990-01-05:   47.083 mean from 24 valid.
...
(many lines omitted)
...
2004-12-27:    2.483 mean from 23 valid.
2004-12-28:    3.383 mean from 23 valid.
2004-12-29:    2.448 mean from 23 valid.
2004-12-30:    2.839 mean from 23 valid.
2004-12-31:    2.057 mean from 23 valid.
Longest period of invalid readings was 589 hours,
from 1993-02-09 1:00 till 1993-03-05 14:00.

```



## Phix


```Phix
constant lines = read_lines("demo\\rosetta\\readings.txt")

include builtins\timedate.e

integer count = 0,
        max_count = 0,
        ntot = 0
atom readtot = 0
timedate run_start, max_start

procedure end_bad_run()
    if count then
        if count>max_count then
            max_count = count
            max_start = run_start
        end if
        count = 0
    end if
end procedure

for i=1 to length(lines) do
    sequence oneline = split(lines[i],'\t',no_empty:=true), r
    if length(oneline)!=49 then
        ?"bad line (length!=49)"
    else
        r = parse_date_string(oneline[1],{"YYYY-MM-DD"})
        if not timedate(r) then
            ?{"bad date",oneline[1]}
        else
            timedate td = r
            integer rejects=0, accepts=0
            atom readsum = 0
            for j=2 to 48 by 2 do
                r = scanf(oneline[j],"%f")
                if length(r)!=1 then
                    ?{"error scanning",oneline[j]}
                    rejects += 1
                else
                    atom reading = r[1][1]
                    r = scanf(oneline[j+1],"%d")
                    if length(r)!=1 then
                        ?{"error scanning",oneline[j+1]}
                        rejects += 1
                    else
                        integer flag = r[1][1]
                        if flag<=0 then
                            if count=0 then
                                run_start = td
                            end if
                            count += 1
                            rejects += 1
                        else
                            end_bad_run()
                            accepts += 1
                            readsum += reading
                        end if
                    end if
                end if
            end for
            readtot += readsum
            ntot += accepts
            string average = iff(accepts=0?"N/A":sprintf("%6.3f",readsum/accepts))
            printf(1,"Date: %s, Rejects: %2d, Accepts: %2d, Line total: %7.3f, Average %s\n",
                   {format_timedate(td,"DD/MM/YYYY"),rejects, accepts, readsum, average})
        end if
    end if
end for

printf(1,"Average: %.3f (of %d readings)\n",{readtot/ntot,ntot})
end_bad_run()
if max_count then
    printf(1,"Maximum run of %d consecutive false readings starting: %s\n",
            {max_count,format_timedate(max_start,"DD/MM/YYYY")})
end if
```

{{out}}

```txt

Date: 29/12/2004, Rejects:  1, Accepts: 23, Line total:  56.300, Average  2.448
Date: 30/12/2004, Rejects:  1, Accepts: 23, Line total:  65.300, Average  2.839
Date: 31/12/2004, Rejects:  1, Accepts: 23, Line total:  47.300, Average  2.057
Average: 10.497 (of 129403 readings)
Maximum run of 589 consecutive false readings starting: 09/02/1993

```



## PicoLisp

{{trans|AWK}}
Put the following into an executable file "readings":

```PicoLisp
#!/usr/bin/picolisp /usr/lib/picolisp/lib.l

(let (NoData 0  NoDataMax -1  NoDataMaxline "!"  TotFile 0  NumFile 0)
   (let InFiles
      (glue ","
         (mapcar
            '((File)
               (in File
                  (while (split (line) "^I")
                     (let (Len (length @)  Date (car @)  TotLine 0  NumLine 0)
                        (for (L (cdr @)  L  (cddr L))
                           (if (> 1 (format (cadr L)))
                              (inc 'NoData)
                              (when (gt0 NoData)
                                 (when (= NoDataMax NoData)
                                    (setq NoDataMaxline (pack NoDataMaxline ", " Date)) )
                                 (when (> NoData NoDataMax)
                                    (setq NoDataMax NoData  NoDataMaxline Date) ) )
                              (zero NoData)
                              (inc 'TotLine (format (car L) 3))
                              (inc 'NumLine) ) )
                        (inc 'TotFile TotLine)
                        (inc 'NumFile NumLine)
                        (tab (-7 -12 -7 3 -9 3 -11 11 -11 11)
                           "Line:" Date
                           "Reject:" (- (/ (dec Len) 2) NumLine)
                           "  Accept:" NumLine
                           "  Line_tot:" (format TotLine 3)
                           "  Line_avg:"
                           (and (gt0 NumLine) (format (*/ TotLine @) 3)) ) ) ) )
               File )
            (argv) ) )
      (prinl)
      (prinl "File(s)  = " InFiles)
      (prinl "Total    = " (format TotFile 3))
      (prinl "Readings = " NumFile)
      (prinl "Average  = " (format (*/ TotFile NumFile) 3))
      (prinl)
      (prinl
         "Maximum run(s) of " NoDataMax
         " consecutive false readings ends at line starting with date(s): " NoDataMaxline ) ) )

(bye)
```

Then it can be called as

```txt
$ ./readings readings.txt |tail
Line:  2004-12-29  Reject:  1  Accept: 23  Line_tot:     56.300  Line_avg:      2.448
Line:  2004-12-30  Reject:  1  Accept: 23  Line_tot:     65.300  Line_avg:      2.839
Line:  2004-12-31  Reject:  1  Accept: 23  Line_tot:     47.300  Line_avg:      2.057

File(s)  = readings.txt
Total    = 1358393.400
Readings = 129403
Average  = 10.497

Maximum run(s) of 589 consecutive false readings ends at line starting with date(s): 1993-03-05
$
```



## PL/I


```pli
text1: procedure options (main); /* 13 May 2010 */

   declare line character (2000) varying;
   declare 1 pairs(24),
               2 value fixed (10,4),
               2 flag  fixed;
   declare date character (12) varying;
   declare no_items fixed decimal (10);
   declare (nv, sum, line_no, ndud_values, max_ndud_values) fixed;
   declare (i, k) fixed binary;
   declare in file input;

   open file (in) title ('/TEXT1.DAT,TYPE(TEXT),RECSIZE(2000)' );

   on endfile (in) go to finish_up;

   line_no = 0;
loop:
   do forever;
      get file (in) edit (line) (L);
      /* put skip list (line); */
      line = translate(line, ' ', '09'x);
      line_no = line_no + 1;
      line = trim(line);
      no_items = tally(line, ' ') - tally(line, '  ') + 1;
      if no_items ^= 49 then
         do; put skip list ('There are not 49 items on this line'); iterate loop; end;
      k = index(line, ' '); /* Find the first blank in the line. */
      date = substr(line, 1, k);
      line = substr(line, k) || ' ';
      on conversion go to loop;
      get string (line) list (pairs);
      sum, nv, ndud_values, max_ndud_values = 0;
      do i = 1 to 24;
         if flag(i) > 0 then
            do; sum = sum + value(i); nv = nv + 1;
            ndud_values = 0; /* reset the counter of dud values */
            end;
         else
            do; /* we have a dud reading. */
                ndud_values = ndud_values + 1;
                if ndud_values > max_ndud_values then
                  max_ndud_values = ndud_values;
            end;
      end;
      if nv = 0 then iterate;
      put skip list ('Line ' || trim(line_no) || ' average=', divide(sum, nv, 10,4) );
      if max_ndud_values > 0 then
         put skip list ('Maximum run of dud readings =', max_ndud_values);
   end;

finish_up:

end text1;
```



## PureBasic


```PureBasic
#TASK="Text processing/1"
Define File$, InLine$, Part$, i, Out$, ErrEnds$, Errcnt, ErrMax
Define lsum.d, tsum.d, rejects, val.d, readings

File$=OpenFileRequester(#TASK,"readings.txt","",0)
If OpenConsole() And ReadFile(0,File$)
  While Not Eof(0)
    InLine$=ReadString(0)
    For i=1 To 1+2*24
      Part$=StringField(InLine$,i,#TAB$)
      If i=1        ; Date
        Out$=Part$: lsum=0: rejects=0
      ElseIf i%2=0  ; Recorded value
        val=ValD(Part$)
      Else          ; Status part
        If Val(Part$)>0
          Errcnt=0 : readings+1
          lsum+val : tsum+val
        Else
          rejects+1: Errcnt+1
          If Errcnt>ErrMax
            ErrMax=Errcnt
            ErrEnds$=Out$
          EndIf
        EndIf
      EndIf
    Next i
    Out$+" Rejects: " + Str(rejects)
    Out$+" Accepts: " + Str(24-rejects)
    Out$+" Line_tot: "+ StrD(lsum,3)
    If rejects<24
      Out$+" Line_avg: "+StrD(lsum/(24-rejects),3)
    Else
      Out$+" Line_avg: N/A"
    EndIf
    PrintN("Line: "+Out$)
  Wend
  PrintN(#CRLF$+"File     = "+GetFilePart(File$))
  PrintN("Total    = "+ StrD(tsum,3))
  PrintN("Readings = "+ Str(readings))
  PrintN("Average  = "+ StrD(tsum/readings,3))
  Print(#CRLF$+"Maximum of "+Str(ErrMax))
  PrintN(" consecutive false readings, ends at "+ErrEnds$)
  CloseFile(0)
  ;
  Print("Press ENTER to exit"): Input()
EndIf
```

{{out|Sample output}}

```txt
...
Line: 2004-12-27 Rejects: 1 Accepts: 23 Line_tot: 57.100 Line_avg: 2.483
Line: 2004-12-28 Rejects: 1 Accepts: 23 Line_tot: 77.800 Line_avg: 3.383
Line: 2004-12-29 Rejects: 1 Accepts: 23 Line_tot: 56.300 Line_avg: 2.448
Line: 2004-12-30 Rejects: 1 Accepts: 23 Line_tot: 65.300 Line_avg: 2.839
Line: 2004-12-31 Rejects: 1 Accepts: 23 Line_tot: 47.300 Line_avg: 2.057

File     = readings.txt
Total    = 1358393.400
Readings = 129403
Average  = 10.497

Maximum of 589 consecutive false readings, ends at 1993-03-05
```



## Python


```python
import fileinput
import sys

nodata = 0;             # Current run of consecutive flags<0 in lines of file
nodata_max=-1;          # Max consecutive flags<0 in lines of file
nodata_maxline=[];      # ... and line number(s) where it occurs

tot_file = 0            # Sum of file data
num_file = 0            # Number of file data items with flag>0

infiles = sys.argv[1:]

for line in fileinput.input():
  tot_line=0;             # sum of line data
  num_line=0;             # number of line data items with flag>0

  # extract field info
  field = line.split()
  date  = field[0]
  data  = [float(f) for f in field[1::2]]
  flags = [int(f)   for f in field[2::2]]

  for datum, flag in zip(data, flags):
    if flag<1:
      nodata += 1
    else:
      # check run of data-absent fields
      if nodata_max==nodata and nodata>0:
        nodata_maxline.append(date)
      if nodata_max<nodata and nodata>0:
        nodata_max=nodata
        nodata_maxline=[date]
      # re-initialise run of nodata counter
      nodata=0;
      # gather values for averaging
      tot_line += datum
      num_line += 1

  # totals for the file so far
  tot_file += tot_line
  num_file += num_line

  print "Line: %11s  Reject: %2i  Accept: %2i  Line_tot: %10.3f  Line_avg: %10.3f" % (
        date,
        len(data) -num_line,
        num_line, tot_line,
        tot_line/num_line if (num_line>0) else 0)

print ""
print "File(s)  = %s" % (", ".join(infiles),)
print "Total    = %10.3f" % (tot_file,)
print "Readings = %6i" % (num_file,)
print "Average  = %10.3f" % (tot_file / num_file,)

print "\nMaximum run(s) of %i consecutive false readings ends at line starting with date(s): %s" % (
    nodata_max, ", ".join(nodata_maxline))
```

{{out|Sample output}}

```txt
bash$ /cygdrive/c/Python26/python readings.py readings.txt|tail
Line:  2004-12-29  Reject:  1  Accept: 23  Line_tot:     56.300  Line_avg:      2.448
Line:  2004-12-30  Reject:  1  Accept: 23  Line_tot:     65.300  Line_avg:      2.839
Line:  2004-12-31  Reject:  1  Accept: 23  Line_tot:     47.300  Line_avg:      2.057

File(s)  = readings.txt
Total    = 1358393.400
Readings = 129403
Average  =     10.497

Maximum run(s) of 589 consecutive false readings ends at line starting with date(s): 1993-03-05
bash$
```



## R


```R
#Read in data from file
dfr <- read.delim("readings.txt")
#Calculate daily means
flags <- as.matrix(dfr[,seq(3,49,2)])>0
vals <- as.matrix(dfr[,seq(2,49,2)])
daily.means <- rowSums(ifelse(flags, vals, 0))/rowSums(flags)
#Calculate time between good measurements
times <- strptime(dfr[1,1], "%Y-%m-%d", tz="GMT") + 3600*seq(1,24*nrow(dfr),1)
hours.between.good.measurements <- diff(times[t(flags)])/3600
```



## Racket


```racket
#lang racket
;; Use SRFI 48 to make %n.nf formats convenient.
(require (prefix-in srfi/48: srfi/48)) ; SRFI 48: Intermediate Format Strings

;; Parameter allows us to used exact decimal strings
(read-decimal-as-inexact #f)

;; files to read is a sequence, so it could be either a list or vector of files
(define (text-processing/1 files-to-read)

  (define (print-line-info d r a t)
    (srfi/48:format #t "Line: ~11F  Reject: ~2F  Accept: ~2F  Line_tot: ~10,3F  Line_avg: ~10,3F~%"
                    d r a t (if (zero? a) +nan.0 (/ t a))))

  ;; returns something that can be used as args to an apply
  (define (handle-and-tag-max consecutive-false tag max-consecutive-false max-false-tags)
   (let ((consecutive-false+1 (add1 consecutive-false)))
     (list consecutive-false+1
           (max max-consecutive-false consecutive-false+1)
           (cond ((= consecutive-false+1 max-consecutive-false) (cons tag max-false-tags))
                 ((= consecutive-false max-consecutive-false) (list tag))
                 (else max-false-tags)))))

  (define (sub-t-p/1 N sum consecutive-false max-consecutive-false max-false-tags)
    (for/fold ((N N) (sum sum) (consecutive-false consecutive-false) (max-consecutive-false max-consecutive-false) (max-false-tags max-false-tags))
      ((l (in-lines)))
      (match l
        [(app string-split `(,tag ,(app string->number vs.ss) ...))
         (let get-line-pairs
           ((vs.ss vs.ss) (line-N 0) (reject 0) (line-sum 0) (consecutive-false consecutive-false)
            (max-consecutive-false max-consecutive-false) (max-false-tags max-false-tags))
           (match vs.ss
             ['()
              (print-line-info tag reject line-N line-sum)
              (values (+ N line-N) (+ sum line-sum) consecutive-false max-consecutive-false max-false-tags)]
             [(list-rest v (? positive?) tl)
              (get-line-pairs tl (add1 line-N) reject (+ line-sum v) 0 max-consecutive-false max-false-tags)]
             [(list-rest _ _ tl)
                (apply get-line-pairs tl line-N (add1 reject) line-sum
                       (handle-and-tag-max consecutive-false tag max-consecutive-false max-false-tags))]))]
        (x (fprintf (current-error-port) "mismatch ~s~%" x)
           (values N sum consecutive-false max-consecutive-false max-false-tags)))))

  (for/fold ((N 0) (sum 0) (consecutive-false 0) (max-consecutive-false 0) (max-false-tags null))
    ((f files-to-read))
    (with-input-from-file f
      (lambda () (sub-t-p/1 N sum consecutive-false max-consecutive-false max-false-tags)))))

(let ((files (vector->list (current-command-line-arguments))))
  (let-values (([N sum consecutive-false max-consecutive-false max-false-tags] (text-processing/1 files)))
    (srfi/48:format #t "~%File(s)  = ~a~%Total    = ~10,3F~%Readings = ~6F~%" (string-join files) sum N)
    (unless (zero? N) (srfi/48:format #t "Average  = ~10,3F~%" (/ sum N)))
    (srfi/48:format #t "~%Maximum run(s) of ~a consecutive false readings ends at line starting with date(s): ~a~%"
                    max-consecutive-false (string-join max-false-tags))))
```

{{out|Sample run}}

```txt
$ racket 1.rkt readings/readings.txt | tail
Line:  2004-12-29  Reject:  1  Accept: 23  Line_tot:     56.300  Line_avg:      2.448
Line:  2004-12-30  Reject:  1  Accept: 23  Line_tot:     65.300  Line_avg:      2.839
Line:  2004-12-31  Reject:  1  Accept: 23  Line_tot:     47.300  Line_avg:      2.057

File(s)  = readings/readings.txt
Total    = 1358393.400
Readings = 129403
Average  =     10.497

Maximum run(s) of 589 consecutive false readings ends at line starting with date(s): 1993-03-05
```



## REXX


```rexx
/*REXX program to process  instrument data  from a  data file.                */
numeric digits 20                      /*allow for bigger (precision) numbers.*/
ifid='READINGS.TXT'                    /*the name of the    input    file.    */
ofid='READINGS.OUT'                    /* "    "   "  "     output     "      */
grandSum=0                             /*the grand sum of whole file.         */
grandFlg=0                             /*the grand number of flagged data.    */
grandOKs=0
Lflag=0                                /*the longest period of flagged data.  */
Cflag=0                                /*the longest continous flagged data.  */
w=16                                   /*the width of fields when displayed.  */

  do recs=1  while lines(ifid)\==0     /*keep reading records until finished. */
  rec=linein(ifid)                     /*read the next record (line) of file. */
  parse var rec datestamp Idata        /*pick off the dateStamp and the data. */
  sum=0
  flg=0
  OKs=0

    do j=1  until Idata=''             /*process the  instrument  data.       */
    parse var Idata data.j flag.j Idata

    if flag.j>0 then do                /*process good data ···                */
                     OKs=OKs+1
                     sum=sum+data.j
                     if Cflag>Lflag  then do
                                          Ldate=datestamp
                                          Lflag=Cflag
                                          end
                     Cflag=0
                     end
                else do                /*process flagged data ···             */
                     flg=flg+1
                     Cflag=Cflag+1
                     end
    end   /*j*/

  if OKs\==0  then avg=format(sum/OKs,,3)
              else avg='[n/a]'
  grandOKs=grandOKs+OKs
  _=right(commas(avg),w)
  grandSum=grandSum+sum
  grandFlg=grandFlg+flg
  if flg==0  then call sy datestamp ' average='_
             else call sy datestamp ' average='_ '  flagged='right(flg,2)
  end   /*recs*/

recs=recs-1                            /*adjust for reading the end─of─file.  */
if grandOKs\==0 then Gavg=format(grandSum/grandOKs,,3)
                else Gavg='[n/a]'
call sy
call sy copies('═',60)
call sy '      records read:'   right(commas(recs),     w)
call sy '     grand     sum:'   right(commas(grandSum), w+4)
call sy '     grand average:'   right(commas(Gavg),     w+4)
call sy '     grand OK data:'   right(commas(grandOKs), w)
call sy '     grand flagged:'   right(commas(grandFlg), w)
if Lflag\==0 then call sy '   longest flagged:' right(commas(Lflag),w) " ending at " Ldate
call sy copies('═',60)
exit                                   /*stick a fork in it,  we're all done. */
/*────────────────────────────────────────────────────────────────────────────*/
commas: procedure;  parse arg _;   n=_'.9';    #=123456789;    b=verify(n,#,"M")
        e=verify(n,#'0',,verify(n,#"0.",'M'))-4
           do j=e  to b  by -3;   _=insert(',',_,j);    end  /*j*/;     return _
/*────────────────────────────────────────────────────────────────────────────*/
sy:     say arg(1);               call lineout ofid,arg(1);             return
```

'''output'''   when using the default input file:
<pre style="height:40ex">
   ∙
   ∙
   ∙
1991-10-16  average=           4.167   flagged= 6
1991-10-17  average=          10.867   flagged= 9
1991-10-18  average=           3.083
   ∙
   ∙
   ∙

### ======================================================

      records read:            5,471
     grand     sum:        1,358,393.400
     grand average:               10.497
     grand OK data:          129,403
     grand flagged:            1,901
   longest flagged:              589  ending at  1993-03-05

### ======================================================


```



## Ruby


```ruby
filename = "readings.txt"
total = { "num_readings" => 0, "num_good_readings" => 0, "sum_readings" => 0.0 }
invalid_count = 0
max_invalid_count = 0
invalid_run_end = ""

File.new(filename).each do |line|
  num_readings = 0
  num_good_readings = 0
  sum_readings = 0.0

  fields = line.split
  fields[1..-1].each_slice(2) do |reading, flag|
    num_readings += 1
    if Integer(flag) > 0
      num_good_readings += 1
      sum_readings += Float(reading)
      invalid_count = 0
    else
      invalid_count += 1
      if invalid_count > max_invalid_count
        max_invalid_count = invalid_count
        invalid_run_end = fields[0]
      end
    end
  end

  printf "Line: %11s  Reject: %2d  Accept: %2d  Line_tot: %10.3f  Line_avg: %10.3f\n",
    fields[0], num_readings - num_good_readings, num_good_readings, sum_readings,
    num_good_readings > 0 ? sum_readings/num_good_readings : 0.0

  total["num_readings"] += num_readings
  total["num_good_readings"] += num_good_readings
  total["sum_readings"] += sum_readings
end

puts ""
puts "File(s)  = #{filename}"
printf "Total    = %.3f\n", total['sum_readings']
puts "Readings = #{total['num_good_readings']}"
printf "Average  = %.3f\n", total['sum_readings']/total['num_good_readings']
puts ""
puts "Maximum run(s) of #{max_invalid_count} consecutive false readings ends at #{invalid_run_end}"
```


Alternate implementation:

```ruby
Reading = Struct.new(:date, :value, :flag)

DailyReading = Struct.new(:date, :readings) do
  def good() readings.select(&:flag) end
  def bad() readings.reject(&:flag) end
  def sum() good.map(&:value).inject(0.0) {|sum, val| sum + val } end
  def avg() good.size > 0 ? (sum / good.size) : 0 end
  def print_status
    puts "%11s:  good: %2d  bad: %2d  total: %8.3f  avg: %6.3f" % [date, good.count, bad.count, sum, avg]
    self
  end
end

daily_readings = IO.foreach(ARGV.first).map do |line|
  (date, *parts) = line.chomp.split(/\s/)
  readings = parts.each_slice(2).map {|pair| Reading.new(date, pair.first.to_f, pair.last.to_i > 0)}
  DailyReading.new(date, readings).print_status
end

all_readings = daily_readings.flat_map(&:readings)
good_readings = all_readings.select(&:flag)
all_streaks = all_readings.slice_when {|bef, aft| bef.flag != aft.flag }
worst_streak = all_streaks.reject {|grp| grp.any?(&:flag)}.sort_by(&:size).last

total = good_readings.map(&:value).reduce(:+)
num_readings = good_readings.count
puts
puts "Total: %.3f" % total
puts "Readings: #{num_readings}"
puts "Average  %.3f" % total./(num_readings)
puts
puts "Max run of #{worst_streak.count} consecutive false readings from #{worst_streak.first.date} until #{worst_streak.last.date}"

```



## Scala

{{works with|Scala|2.8}}

A fully functional solution, minus the fact that it uses iterators:

```scala
object DataMunging {
  import scala.io.Source

  def spans[A](list: List[A]) = list.tail.foldLeft(List((list.head, 1))) {
    case ((a, n) :: tail, b) if a == b => (a, n + 1) :: tail
    case (l, b) => (b, 1) :: l
  }

  type Flag = ((Boolean, Int), String)
  type Flags = List[Flag]
  type LineIterator = Iterator[Option[(Double, Int, Flags)]]

  val pattern = """^(\d+-\d+-\d+)""" + """\s+(\d+\.\d+)\s+(-?\d+)""" * 24 + "$" r;

  def linesIterator(file: java.io.File) = Source.fromFile(file).getLines().map(
    pattern findFirstMatchIn _ map (
      _.subgroups match {
        case List(date, rawData @ _*) =>
          val dataset = (rawData map (_ toDouble) iterator) grouped 2 toList;
          val valid = dataset filter (_.last > 0) map (_.head)
          val validSize = valid length;
          val validSum = valid sum;
          val flags = spans(dataset map (_.last > 0)) map ((_, date))
          println("Line: %11s  Reject: %2d  Accept: %2d  Line_tot: %10.3f  Line_avg: %10.3f" format
                  (date, 24 - validSize, validSize, validSum, validSum / validSize))
          (validSum, validSize, flags)
      }
    )
  )

  def totalizeLines(fileIterator: LineIterator) =
    fileIterator.foldLeft(0.0, 0, List[Flag]()) {
      case ((totalSum, totalSize, ((flag, size), date) :: tail), Some((validSum, validSize, flags))) =>
        val ((firstFlag, firstSize), _) = flags.last
        if (firstFlag == flag) {
          (totalSum + validSum, totalSize + validSize, flags.init ::: ((flag, size + firstSize), date) :: tail)
        } else {
          (totalSum + validSum, totalSize + validSize, flags ::: ((flag, size), date) :: tail)
        }
      case ((_, _, Nil), Some(partials)) => partials
      case (totals, None) => totals
    }

  def main(args: Array[String]) {
    val files = args map (new java.io.File(_)) filter (file => file.isFile && file.canRead)
    val lines =  files.iterator flatMap linesIterator
    val (totalSum, totalSize, flags) = totalizeLines(lines)
    val ((_, invalidCount), startDate) = flags.filter(!_._1._1).max
    val report = """|
                    |File(s)  = %s
                    |Total    = %10.3f
                    |Readings = %6d
                    |Average  = %10.3f
                    |
                    |Maximum run(s) of %d consecutive false readings began at %s""".stripMargin
    println(report format (files mkString " ", totalSum, totalSize, totalSum / totalSize, invalidCount, startDate))
  }
}
```

A quick&dirty solution:

```scala
object AltDataMunging {
  def main(args: Array[String]) {
    var totalSum = 0.0
    var totalSize  = 0
    var maxInvalidDate = ""
    var maxInvalidCount = 0
    var invalidDate = ""
    var invalidCount = 0
    val files = args map (new java.io.File(_)) filter (file => file.isFile && file.canRead)

    files.iterator flatMap (file => Source fromFile file getLines ()) map (_.trim split "\\s+") foreach {
      case Array(date, rawData @ _*) =>
        val dataset = (rawData map (_ toDouble) iterator) grouped 2 toList;
        val valid = dataset filter (_.last > 0) map (_.head)
        val flags = spans(dataset map (_.last > 0)) map ((_, date))
        println("Line: %11s  Reject: %2d  Accept: %2d  Line_tot: %10.3f  Line_avg: %10.3f" format
                (date, 24 - valid.size, valid.size, valid.sum, valid.sum / valid.size))
        totalSum += valid.sum
        totalSize += valid.size
        dataset foreach {
          case _ :: flag :: Nil if flag > 0 =>
            if (invalidCount > maxInvalidCount) {
              maxInvalidDate = invalidDate
              maxInvalidCount = invalidCount
            }
            invalidCount = 0
          case _ =>
            if (invalidCount == 0) invalidDate = date
            invalidCount += 1
        }
    }

    val report = """|
                    |File(s)  = %s
                    |Total    = %10.3f
                    |Readings = %6d
                    |Average  = %10.3f
                    |
                    |Maximum run(s) of %d consecutive false readings began at %s""".stripMargin
    println(report format (files mkString " ", totalSum, totalSize, totalSum / totalSize, maxInvalidCount, maxInvalidDate))
  }
}
```

Last few lines of the sample output (either version):

```txt

Line:  2004-12-29  Reject:  1  Accept: 23  Line_tot:     56.300  Line_avg:      2.448
Line:  2004-12-30  Reject:  1  Accept: 23  Line_tot:     65.300  Line_avg:      2.839
Line:  2004-12-31  Reject:  1  Accept: 23  Line_tot:     47.300  Line_avg:      2.057

File(s)  = readings.txt
Total    = 1358393.400
Readings = 129403
Average  =     10.497

Maximum run(s) of 589 consecutive false readings began at 1993-02-09

```

Though it is easier to show when the consecutive false readings ends, if longest run is the last thing in the file, it hasn't really "ended".


## Sidef

{{trans|Perl 6}}

```ruby
var gaps = [];
var previous = :valid;

ARGF.each { |line|
    var (date, *readings) = line.words...;
    var valid = [];
    var hour = 0;
    readings.map{.to_n}.each_slice(2, { |slice|
        var(reading, flag) = slice...;
        if (flag > 0) {
            valid << reading;
            if (previous == :invalid) {
                gaps[-1]{:end} = "#{date} #{hour}:00";
                previous = :valid;
            }
        }
        else {
            if (previous == :valid) {
                gaps << Hash(start => "#{date} #{hour}:00");
            }
            gaps[-1]{:count} := 0 ++;
            previous = :invalid;
        }
        ++hour;
    })
    say ("#{date}: #{ '%8s' % (valid ? ('%.3f' % Math.avg(valid...)) : 0) }",
         " mean from #{ '%2s' % valid.len } valid.");
}

var longest = gaps.sort_by{|a| -a{:count} }.first;

say ("Longest period of invalid readings was #{longest{:count}} hours,\n",
    "from #{longest{:start}} till #{longest{:end}}.");
```

{{out}}

```txt

1991-03-30:   10.000 mean from 24 valid.
1991-03-31:   23.542 mean from 24 valid.
1991-03-31:   40.000 mean from  1 valid.
1991-04-01:   23.217 mean from 23 valid.
1991-04-02:   19.792 mean from 24 valid.
1991-04-03:   13.958 mean from 24 valid.
Longest period of invalid readings was 24 hours,
from 1991-03-31 1:00 till 1991-04-01 1:00.

```

''Output is from the sample of the task.''


## Tcl


```tcl
set max_invalid_run 0
set max_invalid_run_end ""
set tot_file 0
set num_file 0

set linefmt "Line: %11s  Reject: %2d  Accept: %2d  Line_tot: %10.3f  Line_avg: %10.3f"

set filename readings.txt
set fh [open $filename]
while {[gets $fh line] != -1} {
    set tot_line [set count [set num_line 0]]
    set fields [regexp -all -inline {\S+} $line]
    set date [lindex $fields 0]
    foreach {val flag} [lrange $fields 1 end] {
        incr count
        if {$flag > 0} {
            incr num_line
            incr num_file
            set tot_line [expr {$tot_line + $val}]
            set invalid_run_count 0
        } else {
            incr invalid_run_count
            if {$invalid_run_count > $max_invalid_run} {
                set max_invalid_run $invalid_run_count
                set max_invalid_run_end $date
            }
        }
    }
    set tot_file [expr {$tot_file + $tot_line}]
    puts [format $linefmt $date [expr {$count - $num_line}] $num_line $tot_line \
                 [expr {$num_line > 0 ? $tot_line / $num_line : 0}]]
}
close $fh

puts ""
puts "File(s)  = $filename"
puts "Total    = [format %.3f $tot_file]"
puts "Readings = $num_file"
puts "Average  = [format %.3f [expr {$tot_file / $num_file}]]"
puts ""
puts "Maximum run(s) of $max_invalid_run consecutive false readings ends at $max_invalid_run_end"
```



## Ursala

The input file is transformed to a list of assignments of character strings to lists of pairs of floats
and booleans (type <code>%ebXLm</code>) in the parsed data. The same function is used to compute the daily and the
cumulative statistics.

```Ursala
#import std
#import nat
#import flo

parsed_data = ^|A(~&,* ^|/%ep@iNC ~&h==`1)*htK27K28pPCS (sep 9%cOi&)*FyS readings_dot_txt

daily_stats =

* ^|A(~&,@rFlS ^/length ^/plus:-0. ||0.! ~&i&& mean); mat` + <.
   ~&n,
   'accept: '--+ @ml printf/'%7.0f'+ float,
   'total: '--+ @mrl printf/'%10.1f',
   'average: '--+ @mrr printf/'%7.3f'>

long_run =

-+
   ~&i&& ^|TNC('maximum of '--@h+ %nP,' consecutive false readings ending on line '--),
   @nmrSPDSL -&~&,leql$^; ^/length ~&zn&-@hrZPF+ rlc both ~&rZ+-

main = ^T(daily_stats^lrNCT/~& @mSL 'summary   ':,long_run) parsed_data
```

last few lines of output:

```txt

2004-12-29 accept:      23 total:       56.3 average:   2.448
2004-12-30 accept:      23 total:       65.3 average:   2.839
2004-12-31 accept:      23 total:       47.3 average:   2.057
summary    accept:  129403 total:  1358393.4 average:  10.497
maximum of 589 consecutive false readings ending on line 1993-03-05

```



## VBScript


```vb
Set objFSO = CreateObject("Scripting.FileSystemObject")
Set objFile = objFSO.OpenTextFile(objFSO.GetParentFolderName(WScript.ScriptFullName) &_
			"\data.txt",1)

bad_readings_total = 0
good_readings_total = 0
data_gap = 0
start_date = ""
end_date = ""
tmp_datax_gap = 0
tmp_start_date = ""

Do Until objFile.AtEndOfStream
	bad_readings = 0
	good_readings = 0
	line_total = 0
	line = objFile.ReadLine
	token = Split(line,vbTab)
	n = 1
	Do While n <= UBound(token)
		If n + 1 <= UBound(token) Then
			If CInt(token(n+1)) < 1 Then
				bad_readings = bad_readings + 1
				bad_readings_total = bad_readings_total + 1
				'Account for bad readings.
				If tmp_start_date = "" Then
					tmp_start_date = token(0)
				End If
				tmp_data_gap = tmp_data_gap + 1
			Else
				good_readings = good_readings + 1
				line_total = line_total + CInt(token(n))
				good_readings_total = good_readings_total + 1
				'Sum up the bad readings.
				If (tmp_start_date <> "") And (tmp_data_gap > data_gap) Then
					start_date = tmp_start_date
					end_date = token(0)
					data_gap = tmp_data_gap
					tmp_start_date = ""
					tmp_data_gap = 0
				Else
					tmp_start_date = ""
					tmp_data_gap = 0
				End If
			End If
		End If
		n = n + 2
	Loop
	line_avg = line_total/good_readings
	WScript.StdOut.Write "Date: " & token(0) & vbTab &_
		"Bad Reads: " & bad_readings & vbTab &_
		"Good Reads: " & good_readings & vbTab &_
		"Line Total: " & FormatNumber(line_total,3) & vbTab &_
		"Line Avg: " & FormatNumber(line_avg,3)
	WScript.StdOut.WriteLine
Loop
WScript.StdOut.WriteLine
WScript.StdOut.Write "Maximum run of " & data_gap &_
	" consecutive bad readings from " & start_date & " to " &_
	end_date & "."
WScript.StdOut.WriteLine
objFile.Close
Set objFSO = Nothing
```

{{Out}}

```txt

Date: 1991-03-30	Bad Reads: 0	Good Reads: 24	Line Total: 240.000	Line Avg: 10.000
Date: 1991-03-31	Bad Reads: 0	Good Reads: 24	Line Total: 565.000	Line Avg: 23.542
Date: 1991-03-31	Bad Reads: 23	Good Reads: 1	Line Total: 40.000	Line Avg: 40.000
Date: 1991-04-01	Bad Reads: 1	Good Reads: 23	Line Total: 534.000	Line Avg: 23.217
Date: 1991-04-02	Bad Reads: 0	Good Reads: 24	Line Total: 475.000	Line Avg: 19.792
Date: 1991-04-03	Bad Reads: 0	Good Reads: 24	Line Total: 335.000	Line Avg: 13.958

Maximum run of 24 consecutive bad readings from 1991-03-31 to 1991-04-01.

```



## Vedit macro language

{{trans|AWK}}
Vedit does not have floating point data type, so fixed point calculations are used here.

```vedit
#50 = Buf_Num		// Current edit buffer (source data)
File_Open("output.txt")
#51 = Buf_Num		// Edit buffer for output file
Buf_Switch(#50)
#10 = 0			// total sum of file data
#11 = 0			// number of valid data items in file
#12 = 0			// Current run of consecutive flags<0 in lines of file
#13 = -1		// Max consecutive flags<0 in lines of file
Reg_Empty(15)		// ... and date tag(s) at line(s) where it occurs

While(!At_EOF) {
    #20 = 0		// sum of line data
    #21 = 0		// number of line data items with flag>0
    #22 = 0		// number of line data items with flag<0
    Reg_Copy_Block(14, Cur_Pos, Cur_Pos+10)	// date field

    // extract field info, skipping initial date field
    Repeat(ALL) {
	Search("|{|T,|N}", ADVANCE+ERRBREAK)	// next Tab or Newline
	if (Match_Item==2) { Break }		// end of line
	#30 = Num_Eval(ADVANCE) * 1000		// #30 = value
	Char					//  fixed point, 3 decimal digits
	#30 += Num_Eval(ADVANCE+SUPPRESS)
	#31 = Num_Eval(ADVANCE)			// #31 = flag
	if (#31 < 1) {				// not valid field?
	    #12++
	    #22++
	} else {				// valid field
	    // check run of data-absent fields
	    if(#13 == #12 && #12 > 0) {
	        Reg_Set(15, ", ", APPEND)
	        Reg_Set(15, @14, APPEND)
	    }
	    if(#13 < #12 && #12 > 0) {
	        #13 = #12
	        Reg_Set(15, @14)
	    }

	    // re-initialise run of nodata counter
	    #12 = 0
	    // gather values for averaging
	    #20 += #30
	    #21++
	}
    }

    // totals for the file so far
    #10 += #20
    #11 += #21

    Buf_Switch(#51)	// buffer for output data
    IT("Line: ") Reg_Ins(14)
    IT("  Reject:") Num_Ins(#22, COUNT, 3)
    IT("  Accept:") Num_Ins(#21, COUNT, 3)
    IT("  Line tot:") Num_Ins(#20, COUNT, 8) Char(-3) IC('.') EOL
    IT("  Line avg:") Num_Ins((#20+#21/2)/#21, COUNT, 7) Char(-3) IC('.') EOL IN
    Buf_Switch(#50)	// buffer for input data
}

Buf_Switch(#51)		// buffer for output data
IN
IT("Total:   ") Num_Ins(#10, FORCE+NOCR) Char(-3) IC('.') EOL IN
IT("Readings: ") Num_Ins(#11, FORCE)
IT("Average: ") Num_Ins((#10+#11/2)/#11, FORCE+NOCR) Char(-3) IC('.') EOL IN
IN
IT("Maximum run(s) of ") Num_Ins(#13, LEFT+NOCR)
IT(" consecutive false readings ends at line starting with date(s): ") Reg_Ins(15)
IN
```

{{out|Sample output}}

```txt

Line: 2004-12-28  Reject:  1  Accept: 23  Line tot:   77.800  Line avg:   3.383
Line: 2004-12-29  Reject:  1  Accept: 23  Line tot:   56.300  Line avg:   2.448
Line: 2004-12-30  Reject:  1  Accept: 23  Line tot:   65.300  Line avg:   2.839
Line: 2004-12-31  Reject:  1  Accept: 23  Line tot:   47.300  Line avg:   2.057

Total:   1358393.400
Readings:     129403
Average:      10.497

Maximum run(s) of 589 consecutive false readings ends at line starting with date(s): 1993-03-05

```


{{omit from|Openscad}}
{{omit from|PARI/GP|No real capacity for string manipulation}}
