+++
title = "Take notes on the command line"
description = ""
date = 2019-06-10T16:53:20Z
aliases = []
[extra]
id = 6727
[taxonomies]
categories = []
tags = []
+++

{{task|Text processing}} {{selection|Short Circuit|Console Program Basics}}[[Category:Basic language learning]] [[Category:Programming environment operations]]
{{omit from|Lotus 123 Macro Scripting}}
{{omit from|Maxima}}
{{omit from|Openscad}}
{{omit from|ZX Spectrum Basic|Does not support command line parameters}}

Invoking NOTES without commandline arguments displays the current contents of the local NOTES.TXT if it exists.
If NOTES has arguments, the current date and time are appended to the local NOTES.TXT followed by a newline.
Then all the arguments, joined with spaces, prepended with a tab, and appended with a trailing newline, are written to NOTES.TXT.
If NOTES.TXT doesn't already exist in the current directory then a new NOTES.TXT file should be created.


## Ada

{{works with|Ada 2005}}


```Ada
with Ada.Calendar.Formatting;
with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.IO_Exceptions;
with Ada.Text_IO;
procedure Notes is
   Notes_Filename : constant String := "notes.txt";
   Notes_File     : Ada.Text_IO.File_Type;
   Argument_Count : Natural         := Ada.Command_Line.Argument_Count;
begin
   if Argument_Count = 0 then
      begin
         Ada.Text_IO.Open
           (File => Notes_File,
            Mode => Ada.Text_IO.In_File,
            Name => Notes_Filename);
         while not Ada.Text_IO.End_Of_File (File => Notes_File) loop
            Ada.Text_IO.Put_Line (Ada.Text_IO.Get_Line (File => Notes_File));
         end loop;
      exception
         when Ada.IO_Exceptions.Name_Error =>
            null;
      end;
   else
      begin
         Ada.Text_IO.Open
           (File => Notes_File,
            Mode => Ada.Text_IO.Append_File,
            Name => Notes_Filename);
      exception
         when Ada.IO_Exceptions.Name_Error =>
            Ada.Text_IO.Create (File => Notes_File, Name => Notes_Filename);
      end;
      Ada.Text_IO.Put_Line
        (File => Notes_File,
         Item => Ada.Calendar.Formatting.Image (Date => Ada.Calendar.Clock));
      Ada.Text_IO.Put (File => Notes_File, Item => Ada.Characters.Latin_1.HT);
      for I in 1 .. Argument_Count loop
         Ada.Text_IO.Put
           (File => Notes_File,
            Item => Ada.Command_Line.Argument (I));
         if I /= Argument_Count then
            Ada.Text_IO.Put (File => Notes_File, Item => ' ');
         end if;
      end loop;
      Ada.Text_IO.Flush (File => Notes_File);
   end if;
   if Ada.Text_IO.Is_Open (File => Notes_File) then
      Ada.Text_IO.Close (File => Notes_File);
   end if;
end Notes;
```



## Aime


```aime
#! /usr/local/bin/aime -a

if (argc() == 1) {
    file f;
    text s;

    f.affix("NOTES.TXT");

    while (~f.line(s)) {
        o_(s, "\n");
    }
} else {
    date d;
    file f;

    f.open("NOTES.TXT", OPEN_APPEND | OPEN_CREATE | OPEN_WRITEONLY, 0644);

    d.now;

    f.form("/f4/-/f2/-/f2/ /f2/:/f2/:/f2/\n", d.year, d.y_month,
           d.m_day, d.d_hour, d.h_minute, d.m_second);

    for (integer i, text s in 1.args) {
        f.bytes(i ? ' ' : '\t', s);
    }

    f.byte('\n');
}
```



## AppleScript

Requires a modernish version of OS X (Lion or later) on which osascript works as a shebang interpreter.

```applescript
#!/usr/bin/osascript

-- format a number as a string with leading zero if needed
to format(aNumber)
  set resultString to aNumber as text
  if length of resultString < 2
    set resultString to "0" & resultString
  end if
  return resultString
end format

-- join a list with a delimiter
to concatenation of aList given delimiter:aDelimiter
  set tid to AppleScript's text item delimiters
  set AppleScript's text item delimiters to { aDelimiter }
  set resultString to aList as text
  set AppleScript's text item delimiters to tid
  return resultString
end join

-- apply a handler to every item in a list, returning
-- a list of the results
to mapping of aList given function:aHandler
  set resultList to {}
  global h
  set h to aHandler
  repeat with anItem in aList
    set resultList to resultList & h(anItem)
  end repeat
  return resultList
end mapping

-- return an ISO-8601-formatted string representing the current date and time
-- in UTC
to iso8601()
    set { year:y,   month:m,     day:d, ¬
          hours:hr, minutes:min, seconds:sec } to ¬
          (current date) - (time to GMT)
    set ymdList to the mapping of { y, m as integer, d } given function:format
    set ymd to the concatenation of ymdList given delimiter:"-"
    set hmsList to the mapping of { hr, min, sec } given function:format
    set hms to the concatenation of hmsList given delimiter:":"
    set dateTime to the concatenation of {ymd, hms} given delimiter:"T"
    return dateTime & "Z"
end iso8601

to exists(filePath)
  try
    filePath as alias
    return true
  on error
    return false
  end try
end exists

on run argv
  set curDir to (do shell script "pwd")
  set notesFile to POSIX file (curDir & "/NOTES.TXT")

  if (count argv) is 0 then
    if exists(notesFile) then
       set text item delimiters to {linefeed}
       return paragraphs of (read notesFile) as text
    else
       log "No notes here."
       return
    end if
  else
    try
      set fd to open for access notesFile with write permission
      write (iso8601() & linefeed & tab) to fd starting at eof
      set AppleScript's text item delimiters to {" "}
      write ((argv as text) & linefeed) to fd starting at eof
      close access fd
      return true
    on error errMsg number errNum
      try
         close access fd
      end try
      return "unable to open  " & notesFile & ": " & errMsg
    end try
  end if
end run
```



## AutoHotkey


```autohotkey
Notes := "Notes.txt"

If 0 = 0 ; no arguments
{
    If FileExist(Notes) {
        FileRead, Content, %Notes%
        MsgBox, %Content%
    } Else
        MsgBox, %Notes% does not exist
    Goto, EOF
}

; date and time, colon, newline (CRLF), tab
Date := A_DD "/" A_MM "/" A_YYYY
Time := A_Hour ":" A_Min ":" A_Sec "." A_MSec
FileAppend, %Date% %Time%:`r`n%A_Tab%, %Notes%

; command line parameters, trailing newline (CRLF)
Loop, %0%
    FileAppend, % %A_Index% " ", %Notes%
FileAppend, `r`n, %Notes%

EOF:
```



## AWK


```AWK

# syntax: GAWK -f TAKE_NOTES.AWK [notes ... ]
# examples:
#   GAWK -f TAKE_NOTES.AWK Hello world
#   GAWK -f TAKE_NOTES.AWK A "B C" D
#   GAWK -f TAKE_NOTES.AWK
BEGIN {
    log_name = "NOTES.TXT"
    (ARGC == 1) ? show_log() : update_log()
    exit(0)
}
function show_log(  rec) {
    while (getline rec <log_name > 0) {
      printf("%s\n",rec)
    }
}
function update_log(  i,q) {
    print(strftime("%Y-%m-%d %H:%M:%S")) >>log_name
    printf("\t") >>log_name
    for (i=1; i<=ARGC-1; i++) {
      q = (ARGV[i] ~ / /) ? "\"" : ""
      printf("%s%s%s ",q,ARGV[i],q) >>log_name
    }
    printf("\n") >>log_name
}

```

{{out}} from the three example commands:

```txt

2013-09-20 00:35:41
        Hello world
2013-09-20 00:35:41
        A "B C" D

```



## BASIC

{{works with|QuickBASIC|7}}

An implementation of <code>DIR$</code> is all that's required
for this to work with older versions of QB.


```qbasic
IF LEN(COMMAND$) THEN
    OPEN "notes.txt" FOR APPEND AS 1
    PRINT #1, DATE$, TIME$
    PRINT #1, CHR$(9); COMMAND$
    CLOSE
ELSE
    d$ = DIR$("notes.txt")
    IF LEN(d$) THEN
        OPEN d$ FOR INPUT AS 1
        WHILE NOT EOF(1)
            LINE INPUT #1, i$
            PRINT i$
        WEND
        CLOSE
    END IF
END IF
```



## Batch File

{{works with|Windows NT|4}}

```dos
@echo off
if %1@==@ (
    if exist notes.txt more notes.txt
    goto :eof
)
echo %date% %time%:>>notes.txt
echo 	%*>>notes.txt
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
This must be compiled to an EXE and run from the command prompt.

```bbcbasic
      REM!Exefile C:\NOTES.EXE, encrypt, console
      REM!Embed
      LF = 10

      SYS "GetStdHandle", -10 TO @hfile%(1)
      SYS "GetStdHandle", -11 TO @hfile%(2)
      SYS "SetConsoleMode", @hfile%(1), 0
      *INPUT 13
      *OUTPUT 14
      ON ERROR PRINT REPORT$ : QUIT ERR

      notes% = OPENUP(@dir$ + "NOTES.TXT")
      IF notes% = 0 notes% = OPENOUT(@dir$ + "NOTES.TXT")
      IF notes% = 0 PRINT "Cannot open or create NOTES.TXT" : QUIT 1

      IF @cmd$ = "" THEN
        WHILE NOT EOF#notes%
          INPUT #notes%, text$
          IF ASC(text$) = LF text$ = MID$(text$,2)
          PRINT text$
        ENDWHILE
      ELSE
        PTR#notes% = EXT#notes%
        PRINT #notes%,TIME$ : BPUT#notes%,LF
        PRINT #notes%,CHR$(9) + @cmd$ : BPUT#notes%,LF
      ENDIF

      CLOSE #notes%
      QUIT
```



## C


```c
#include <stdio.h>
#include <time.h>

#define note_file "NOTES.TXT"

int main(int argc, char**argv)
{
	FILE *note = 0;
	time_t tm;
	int i;
	char *p;

	if (argc < 2) {
		if ((note = fopen(note_file, "r")))
			while ((i = fgetc(note)) != EOF)
				putchar(i);

	} else if ((note = fopen(note_file, "a"))) {
		tm = time(0);
		p = ctime(&tm);

		/* skip the newline */
		while (*p) fputc(*p != '\n'?*p:'\t', note), p++;

		for (i = 1; i < argc; i++)
			fprintf(note, "%s%c", argv[i], 1 + i - argc ? ' ' : '\n');
	}

	if (note) fclose(note);
	return 0;
}
```



## C++


```cpp
#include <fstream>
#include <iostream>
#include <ctime>
using namespace std;

#define note_file "NOTES.TXT"

int main(int argc, char **argv)
{
	if(argc>1)
	{
		ofstream Notes(note_file, ios::app);
		time_t timer = time(NULL);
		if(Notes.is_open())
		{
			Notes << asctime(localtime(&timer)) << '\t';
			for(int i=1;i<argc;i++)
				Notes << argv[i] << ' ';
			Notes << endl;
			Notes.close();
		}
	}
	else
	{
		ifstream Notes(note_file, ios::in);
		string line;
		if(Notes.is_open())
		{
			while(!Notes.eof())
			{
				getline(Notes, line);
				cout << line << endl;
			}
			Notes.close();
		}
	}
}
```


## C#

```c#
using System;
using System.IO;
using System.Text;

namespace RosettaCode
{
  internal class Program
  {
    private const string FileName = "NOTES.TXT";

    private static void Main(string[] args)
    {
      if (args.Length==0)
      {
        string txt = File.ReadAllText(FileName);
        Console.WriteLine(txt);
      }
      else
      {
        var sb = new StringBuilder();
        sb.Append(DateTime.Now).Append("\n\t");
        foreach (string s in args)
          sb.Append(s).Append(" ");
        sb.Append("\n");

        if (File.Exists(FileName))
          File.AppendAllText(FileName, sb.ToString());
        else
          File.WriteAllText(FileName, sb.ToString());
      }
    }
  }
}
```



## Clojure


```clojure
(ns rosettacode.notes
  (:use [clojure.string :only [join]]))

(defn notes [notes]
  (if (seq notes)
    (spit
     "NOTES.txt"
     (str (java.util.Date.) "\n" "\t"
          (join " " notes) "\n")
     :append true)
    (println (slurp "NOTES.txt"))))

(notes *command-line-args*)
```


{{out}}

```txt

rayne@ubuntu:~/cljprojs/rosettacode$ java -cp 'lib/*' clojure.main src/rosettacode/notes.clj I tawt I taw a puddy cat!
rayne@ubuntu:~/cljprojs/rosettacode$ java -cp 'lib/*' clojure.main src/rosettacode/notes.clj I did! I did!
rayne@ubuntu:~/cljprojs/rosettacode$ cat NOTES.txt
Wed Dec 29 14:09:24 CST 2010
	I tawt I taw a puddy cat!
Wed Dec 29 14:09:28 CST 2010
	I did! I did!
rayne@ubuntu:~/cljprojs/rosettacode$ java -cp 'lib/*' clojure.main src/rosettacode/notes.clj
Wed Dec 29 14:09:24 CST 2010
	I tawt I taw a puddy cat!
Wed Dec 29 14:09:28 CST 2010
	I did! I did!


```



## COBOL

{{works with|OpenCOBOL}}

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NOTES.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL notes ASSIGN TO "NOTES.TXT"
               ORGANIZATION LINE SEQUENTIAL
               FILE STATUS note-status.

       DATA DIVISION.
       FILE SECTION.
       FD  notes.
       01  note-record       PIC X(256).

       LOCAL-STORAGE SECTION.
       01  note-status       PIC 99.
           88  notes-ok      VALUE 0 THRU 9.

       01  date-now.
           03  current-year  PIC 9(4).
           03  current-month PIC 99.
           03  current-day   PIC 99.

       01  time-now.
           03  current-hour  PIC 99.
           03  current-min   PIC 99.
           03  current-sec   PIC 99.

       01  args              PIC X(256).

       PROCEDURE DIVISION.
       DECLARATIVES.
       note-error SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON notes.

           DISPLAY "Error using NOTES.TXT. Error code: " note-status
           .
       END DECLARATIVES.

       main.
           ACCEPT args FROM COMMAND-LINE

*          *> If there are no args, display contents of NOTES.TXT.
           IF args = SPACES
               OPEN INPUT notes

               PERFORM FOREVER
*                  *> READ has no syntax highlighting, but END-READ does.
*                  *> Go figure.
                   READ notes
                       AT END
                           EXIT PERFORM

                       NOT AT END
                           DISPLAY FUNCTION TRIM(note-record)
                   END-READ
               END-PERFORM
           ELSE
               OPEN EXTEND notes

*              *> Write date and time to file.
               ACCEPT date-now FROM DATE YYYYMMDD
               ACCEPT time-now FROM TIME
               STRING current-year "-" current-month "-" current-day
                   " " current-hour ":" current-min ":" current-sec
                   INTO note-record
               WRITE note-record

*              *> Write arguments to file as they were passed.
               STRING X"09", args INTO note-record
               WRITE note-record
           END-IF

           CLOSE notes

           GOBACK
           .
```



## Common Lisp


```lisp
(defparameter *notes* "NOTES.TXT")

(defun format-date-time (stream)
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (format stream "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month date hour minute second)))

(defun notes (args)
  (if args
      (with-open-file (s *notes* :direction :output
                                 :if-exists :append
                                 :if-does-not-exist :create)
        (format-date-time s)
        (format s "~&~A~{~A~^ ~}~%" #\Tab args))
      (with-open-file (s *notes* :if-does-not-exist nil)
        (when s
          (loop for line = (read-line s nil)
                while line
                do (write-line line))))))

(defun main ()
  (notes (uiop:command-line-arguments)))
```



## D


```d
void main(in string[] args) {
    import std.stdio, std.file, std.datetime, std.range;

    immutable filename = "NOTES.TXT";

    if (args.length == 1) {
        if (filename.exists && filename.isFile)
            writefln("%-(%s\n%)", filename.File.byLine);
    } else {
        auto f = File(filename, "a+");
        f.writefln("%s", cast(DateTime)Clock.currTime);
        f.writefln("\t%-(%s %)", args.dropOne);
    }
}
```

{{out}}

```txt
C:\>notes Permission to speak, sir!
C:\>notes They don't like it up 'em, sir, they don't like it up 'em!
C:\>notes Don't panic! DON'T PANIC!
C:\>notes
2013-Mar-01 17:50:38
        Permission to speak, sir!
2013-Mar-01 17:51:00
        They don't like it up 'em, sir, they don't like it up 'em!
2013-Mar-01 17:52:18
        Don't panic! DON'T PANIC!
```



## Delphi


```delphi
program notes;

{$APPTYPE CONSOLE}

uses
  Classes,
  SysUtils,
  IOUtils;

const
  FILENAME = 'NOTES.TXT';
  TAB = #9;

var
  sw: TStreamWriter;
  i : integer;

begin
  if ParamCount = 0 then
  begin
    if TFile.Exists(FILENAME) then
      write(TFile.ReadAllText(FILENAME));
  end
  else
  begin
    if TFile.Exists(FILENAME) then
      sw := TFile.AppendText(FILENAME)
    else
      sw := TFile.CreateText(FILENAME);

    sw.Write(FormatDateTime('yyyy-mm-dd hh:nn',Now));
    sw.Write(TAB);
    for i := 1 to ParamCount do
    begin
      sw.Write(ParamStr(i));
      if i < ParamCount then
        sw.Write(' ');
    end;
    sw.WriteLine;
    sw.Free;
  end;
end.
```



## E



```e
#!/usr/bin/env rune

def f := <file:notes.txt>
def date := makeCommand("date")

switch (interp.getArgs()) {
    match [] {
        if (f.exists()) {
            for line in f { print(line) }
        }
    }
    match noteArgs {
        def w := f.textWriter(true)
        w.print(date()[0], "\t", " ".rjoin(noteArgs), "\n")
        w.close()
    }
}
```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Take_notes do
  @filename "NOTES.TXT"

  def main( [] ), do: display_notes
  def main( arguments ), do: save_notes( arguments )

  def display_notes, do: IO.puts File.read!(@filename)

  def save_notes( arguments ) do
    notes = "#{inspect :calendar.local_time}\n\t" <> Enum.join(arguments, " ")
    File.open!(@filename, [:append], fn(file) -> IO.puts(file, notes) end)
  end
end

Take_notes.main(System.argv)
```



## Erlang


```Erlang

#! /usr/bin/env escript

main( Arguments ) ->
	display_notes( Arguments ),
	save_notes( Arguments ).



display_notes( [] ) -> io:fwrite( "~s", [erlang:binary_to_list(file_contents())] );
display_notes( _Arguments ) -> ok.

file() -> "NOTES.TXT".

file_contents() -> file_contents( file:read_file(file()) ).

file_contents( {ok, Binary} ) -> Binary;
file_contents( {error, _Error} ) -> <<>>.

save_notes( [] ) -> ok;
save_notes( Arguments ) ->
	Date = io_lib:format( "~p~n", [calendar:local_time()] ),
	Notes = [Date ++ "\t" | [io_lib:format( "~s ", [X]) || X <- Arguments]],
	Existing_contents = file_contents(),
	file:write_file( file(), [Existing_contents, Notes, "\n"] ).

```



## Euphoria


```Euphoria
constant cmd = command_line()
constant filename = "notes.txt"
integer fn
object line
sequence date_time

if length(cmd) < 3 then
    fn = open(filename,"r")
    if fn != -1 then
        while 1 do
            line = gets(fn)
            if atom(line) then
                exit
            end if
            puts(1,line)
        end while
        close(fn)
    end if
else
    fn = open(filename,"a") -- if such file doesn't exist it will be created
    date_time = date()
    date_time = date_time[1..6]
    date_time[1] += 1900
    printf(fn,"%d-%02d-%02d %d:%02d:%02d\n",date_time)
    line = "\t"
    for n = 3 to length(cmd) do
        line &= cmd[n] & ' '
    end for
    line[$] = '\n'
    puts(fn,line)
    close(fn)
end if
```


=={{header|F Sharp|F#}}==

```fsharp
open System;;
open System.IO;;

let file_path = "notes.txt";;

let show_notes () =
    try
        printfn "%s" <| File.ReadAllText(file_path)
    with
        _ -> printfn "Take some notes first!";;

let take_note (note : string) =
    let now = DateTime.Now.ToString() in
    let note = sprintf "%s\n\t%s" now note in
    use file_stream = File.AppendText file_path in (* 'use' closes file_stream automatically when control leaves the scope *)
        file_stream.WriteLine note;;

[<EntryPoint>]
let main args =
    match Array.length args with
        | 0 -> show_notes()
        | _ -> take_note <| String.concat " " args;
    0;;
```


Usage:


```txt

> .\TakeNotes.exe
Take some notes first!
> .\TakeNotes.exe some note with multiple words
> .\TakeNotes.exe another note, with coma in it
> .\TakeNotes.exe
2011-10-15 20:36:52
        some note with multiple words
2011-10-15 20:37:22
        another note with coma in it

```



## Factor


```factor
#! /usr/bin/env factor
USING: kernel calendar calendar.format io io.encodings.utf8 io.files
sequences command-line namespaces ;

command-line get [
    "notes.txt" utf8 file-contents print
] [
    " " join "\t" prepend
    "notes.txt" utf8 [
        now timestamp>ymdhms print
        print flush
    ] with-file-appender
] if-empty
```



## Fantom



```fantom

class Notes
{
  public static Void main (Str[] args)
  {
    notesFile := File(`notes.txt`) // the backticks make a URI
    if (args.isEmpty)
    {
      if (notesFile.exists)
      {
        notesFile.eachLine |line| { echo (line) }
      }
    }
    else
    {
      // notice the following uses a block so the 'printLine/close'
      // operations are all applied to the same output stream for notesFile
      notesFile.out(true) // 'true' to append to file
      {
        printLine ( DateTime.now.toLocale("DD-MM-YY hh:mm:ss").toStr )
        printLine ( "\t" + args.join(" ") )
        close
      }
    }
  }
}

```


Sample:


```txt

$ fan notes.fan started notes file
$ fan notes.fan added second set of notes
$ fan notes.fan and a third
$ fan notes.fan
30-01-11 12:30:05
	started notes file
30-01-11 12:30:17
	added second set of notes
30-01-11 12:30:30
	and a third

```



## Forth

The following is SwiftForth-specific, and runs in the Forth environment rather than as a standalone terminal application.  Win32Forth only needs simple replacements of DATE and TIME .  A Unix Forth would come closer to the 'commandline' requirement, as Windows Forths for whatever reason very eagerly discard the Windows console.  Because this runs in the Forth environment, the otherwise acceptable application words are stuffed in a wordlist.  Standard Forth doesn't have the notion of create-it-if-it-doesn't-exist, so OPEN just tries again with CREATE-FILE if OPEN-FILE fails.


```forth
vocabulary note-words
get-current also note-words definitions

\ -- notes.txt
variable file
: open       s" notes.txt" r/w open-file if
             s" notes.txt" r/w create-file throw then file ! ;
: appending  file @ file-size throw file @ reposition-file throw ;
: write      file @ write-file throw ;
: close      file @ close-file throw ;

\ -- SwiftForth console workaround
9 constant TAB
: type ( c-addr u -- )
  bounds ?do
    i c@ dup TAB = if drop 8 spaces else emit then
  loop ;

\ -- dump notes.txt
create buf 4096 allot
: dump ( -- )
  cr begin buf 4096 file @ read-file throw dup while
    buf swap type
  repeat drop ;

\ -- time and date
: time   @time (time) ;
: date   @date (date) ;

\ -- add note
: cr     s\" \n" write ;
: tab    s\" \t" write ;
: space  s"  " write ;
: add-note ( c-addr u -- ) appending
  date write space time write cr
  tab ( note ) write cr ;

set-current

\ -- note
: note ( "note" -- )
  open 0 parse dup if add-note
  else 2drop dump then close ;
previous
```

The following is exactly the same program, but written in 4tH. 4tH has an I/O system which is different from ANS-Forth. The notes file is specified on the commandline.

```forth
\ -- notes.txt
include lib/argopen.4th
include lib/ansfacil.4th

\ -- dump notes.txt
4096 buffer: buf
: dump ( -- )
  input 1 arg-open
  begin buf dup 4096 accept dup while type repeat
  drop drop close ;

\ -- time and date
: :00 <# # # [char] : hold #> type ;
: -00 <# # # [char] - hold #> type ;
: .time 0 .r :00 :00 ;
: .date 0 .r -00 -00 ;

\ -- add note
: add-note ( c-addr u -- )
  output append [+] 1 arg-open -rot
  time&date .date space .time cr
  9 emit type cr close ;

\ -- note
: note ( "note" -- )
  argn 2 < abort" Usage: notes filename"
  refill drop 0 parse dup if add-note else 2drop dump then ;

note
```



## Gambas


```gambas
'Note that the 1st item in 'Args' is the file name as on the command line './CLIOnly.gambas'

Public Sub Main()
Dim sContents As String                                                     'To store the file contents
Dim sArgs As String[] = Args.All                                            'To store all the Command line Arguments

If Not Exist(User.home &/ "NOTES.TXT") Then                                 'If NOTES.TXT doesn't already exist in the current directory then..
  File.Save(User.home &/ "NOTES.TXT", "")                                   'a new NOTES.TXT file should be created.
  Print "New file 'NOTES.TXT' created."                                     'A meassge
Endif

sContents = File.Load(User.home &/ "NOTES.TXT")                             'Get the contents of the file

If Args.count < 2 Then                                                      'If NOTES has arguments (other than the file name)
  Print sContents                                                           'Print the file contents
Else
  sContents &= Format(Now, "dddd dd mmmm, yyyy, hh:nn:ss") & gb.NewLine &   'The current date and time are appended to the local NOTES.TXT followed by a newline and..
     gb.Tab & sArgs.Join(" ") & gb.NewLine                                  'Then all the arguments, joined with spaces, prepended with a tab, and appended with a trailing newline
  Print sContents                                                           'Displays the current contents of the local NOTES.TXT
  File.Save(User.home &/ "NOTES.TXT", sContents)                            'Write contents to NOTES.TXT
Endif

End
```

Output:

```txt

Wednesday 24 May, 2017, 17:55:56
	./CLIOnly.gambas Hello to
Wednesday 24 May, 2017, 17:55:58
	./CLIOnly.gambas Hello to you all

```



## Go


```go
package main

import (
	"fmt"
	"io"
	"os"
	"strings"
	"time"
)

func addNote(fn string, note string) error {
	f, err := os.OpenFile(fn, os.O_RDWR|os.O_APPEND|os.O_CREATE, 0666)
	if err != nil {
		return err
	}
	_, err = fmt.Fprint(f, time.Now().Format(time.RFC1123), "\n\t", note, "\n")
	// To be extra careful with errors from Close():
	if cErr := f.Close(); err == nil {
		err = cErr
	}
	return err
}

func showNotes(w io.Writer, fn string) error {
	f, err := os.Open(fn)
	if err != nil {
		if os.IsNotExist(err) {
			return nil // don't report "no such file"
		}
		return err
	}
	_, err = io.Copy(w, f)
	f.Close()
	return err
}

func main() {
	const fn = "NOTES.TXT"
	var err error
	if len(os.Args) > 1 {
		err = addNote(fn, strings.Join(os.Args[1:], " "))
	} else {
		err = showNotes(os.Stdout, fn)
	}
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
```



## Groovy


```groovy
def notes = new File('./notes.txt')
if (args) {
    notes << "${new Date().format('YYYY-MM-dd HH:mm:ss')}\t${args.join(' ')}\n"
} else {
    println notes.text
}

```



## Haskell


```haskell
import System.Environment (getArgs)
import System.Time (getClockTime)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then catch (readFile "notes.txt" >>= putStr)
               (\_ -> return ())
    else
      do ct <- getClockTime
         appendFile "notes.txt" $ show ct ++ "\n\t" ++ unwords args ++ "\n"
```


{{out}} after a few runs:

```txt

Thu Apr 22 19:01:41 PDT 2010
	Test line 1.
Thu Apr 22 19:01:45 PDT 2010
	Test line 2.

```



## HicEst


```HicEst
SYSTEM(RUN) ! start this script in RUN-mode

CHARACTER notes="Notes.txt", txt*1000

! Remove file name from the global variable $CMD_LINE:
EDIT(Text=$CMD_LINE, Mark1, Right=".hic ", Right=4, Mark2, Delete)

IF($CMD_LINE == ' ') THEN
  READ(FIle=notes, LENgth=Lnotes)
  IF( Lnotes ) THEN
    WINDOW(WINdowhandle=hdl, TItle=notes)
    WRITE(Messagebox="?Y") "Finished ?"
  ENDIF
ELSE
  WRITE(Text=txt, Format="UWWW CCYY-MM-DD HH:mm:SS, A") 0, $CRLF//$TAB//TRIM($CMD_LINE)//$CRLF
  OPEN(FIle=notes, APPend)
  WRITE(FIle=notes, CLoSe=1) txt
ENDIF

ALARM(999) ! quit HicEst immediately
```


```txt
Thu 2010-09-16 18:42:15
	This is line 1

Thu 2010-09-16 18:42:23
	This is line 2
```


=={{header|Icon}} and {{header|Unicon}}==


```Icon

procedure write_out_notes (filename)
  file := open (filename, "rt") | stop ("no notes file yet")
  every write (!file)
end

procedure add_to_notes (filename, strs)
  file := open (filename, "at")  | # append to file if it exists
          open (filename, "cat") | # create the file if not there
          stop ("unable to open " || filename)
  writes (file, ctime(&now) || "\n\t")
  every writes (file, !strs || " ")
  write (file, "")
end

procedure main (args)
  notes_file := "notes.txt"
  if *args = 0
    then write_out_notes (notes_file)
    else add_to_notes (notes_file, args)
end

```


{{out}}

```txt

$ ./take-notes
no notes file yet
$ ./take-notes finished notes program
$ ./take-notes
Thu Jun  2 23:39:49 2011
	finished notes program
$ ./take-notes for Unicon
$ ./take-notes
Thu Jun  2 23:39:49 2011
	finished notes program
Thu Jun  2 23:40:13 2011
	for Unicon

```



## J

'''Solution''':


```j
require 'files strings'

notes=: monad define
 if. #y do.
   now=. LF ,~ 6!:0 'hh:mm:ss DD/MM/YYYY'
   'notes.txt' fappend~ now, LF ,~ TAB, ' ' joinstring y
 elseif. -. _1 -: txt=. fread 'notes.txt' do.
   smoutput txt
 end.
)

notes 2}.ARGV
exit 0
```


Create a Shortcut called <tt>Notes</tt> that calls the script above using the Target:

<tt>"c:\program files\j602\bin\jconsole.exe" path\to\script\notes.ijs</tt>

and Start in: <tt>.\</tt>

'''Example Use:'''

In a Windows CMD session...


```txt
C:\Folder>Notes.lnk "Test line 1"

C:\Folder>Notes.lnk "Test" "line" "2"

C:\Folder>Notes.lnk Test line 3

C:\Folder>Notes.lnk
2010 5 21 11 31 30.389
        Test line 1
2010 5 21 11 31 50.669
        Test line 2
2010 5 21 11 32 14.895
        Test line 3
```



## Java


```java
import java.io.*;
import java.nio.channels.*;
import java.util.Date;

public class TakeNotes {
    public static void main(String[] args) throws IOException {
        if (args.length > 0) {
            PrintStream ps = new PrintStream(new FileOutputStream("notes.txt", true));
            ps.println(new Date());
            ps.print("\t" + args[0]);
            for (int i = 1; i < args.length; i++)
                ps.print(" " + args[i]);
            ps.println();
            ps.close();
        } else {
            FileChannel fc = new FileInputStream("notes.txt").getChannel();
            fc.transferTo(0, fc.size(), Channels.newChannel(System.out));
            fc.close();
        }
    }
}
```



## JavaScript

{{works with|JScript}}

```javascript
var notes = 'NOTES.TXT';

var args = WScript.Arguments;
var fso = new ActiveXObject("Scripting.FileSystemObject");
var ForReading = 1, ForWriting = 2, ForAppending = 8;

if (args.length == 0) {
    if (fso.FileExists(notes)) {
        var f = fso.OpenTextFile(notes, ForReading);
        WScript.Echo(f.ReadAll());
        f.Close();
    }
}
else {
    var f = fso.OpenTextFile(notes, ForAppending, true);
    var d = new Date();
    f.WriteLine(d.toLocaleString());
    f.Write("\t");
    // note that WScript.Arguments is not an array, it is a "collection"
    // it does not have a join() method.
    for (var i = 0; i < args.length; i++) {
        f.Write(args(i) + " ");
    }
    f.WriteLine();
    f.Close();
}
```


```txt
> del NOTES.TXT
> cscript /nologo notes.js
> cscript /nologo notes.js this   is   the   first note
> cscript /nologo notes.js
April 1, 2010 5:18:38 PM
        this is the first note
```



## Julia


```julia
using Dates

const filename = "NOTES.TXT"

if length(ARGS) == 0
    fp = open(filename, "r")
    println(read(fp, String))
else
    fp = open(filename, "a+")
    write(fp, string(DateTime(now()), "\n\t", join(ARGS, " "), "\n"))
end
close(fp)

```



## Kotlin


```scala
// version 1.2.10

import java.io.File
import java.util.Date
import java.text.SimpleDateFormat

fun main(args: Array<String>) {
    val f = File("NOTES.TXT")
    // create file if it doesn't exist already
    f.createNewFile()
    if (args.size == 0) {
        println(f.readText())
    }
    else {
        val df = SimpleDateFormat("yyyy/MM/dd HH:mm:ss")
        val dt = df.format(Date())
        val notes = "$dt\n\t${args.joinToString(" ")}\n"
        f.appendText(notes)
    }
}
```


Sample output:

```txt

$ java -jar NOTES.jar This is the first note.
$ java -jar NOTES.jar
2018/01/27 18:19:06
	This is the first note.


```



## Lasso


```Lasso
#!/usr/bin/lasso9

local(
	arguments	= $argv -> asarray,
	notesfile	= file('notes.txt')
)

#arguments -> removefirst

if(#arguments -> size) => {

	#notesfile -> openappend
	#notesfile -> dowithclose => {
		#notesfile -> writestring(date -> format(`YYYY-MM-dd HH:mm:SS`) + '\n')
		#notesfile -> writestring('\t' + #arguments -> join(', ') + '\n')
	}



else
	#notesfile -> exists ? stdout(#notesfile -> readstring)
}
```


Called with:

```Lasso
./notes "Rosetta was here" Räksmörgås
./notes First Second Last
./notes

```


```txt
2013-11-15 11:43:08
	Rosetta was here, Räksmörgås
2013-11-15 11:43:67
	First, Second, Last

```



## Lua


```lua
filename = "NOTES.TXT"

if #arg == 0 then
    fp = io.open( filename, "r" )
    if fp ~= nil then
        print( fp:read( "*all*" ) )
        fp:close()
    end
else
    fp = io.open( filename, "a+" )

    fp:write( os.date( "%x %X\n" ) )

    fp:write( "\t" )
    for i = 1, #arg do
	fp:write( arg[i], " " )
    end
    fp:write( "\n" )

    fp:close()
end
```



## Mathematica

<lang>If[Length[$CommandLine < 11], str = OpenRead["NOTES.TXT"];
 Print[ReadString[str, EndOfFile]]; Close[str],
 str = OpenAppend["NOTES.TXT"]; WriteLine[str, DateString[]];
 WriteLine[str, "\t" <> StringRiffle[$CommandLine[[11 ;;]]]];
 Close[str]]
```


=={{header|MATLAB}} / {{header|Octave}}==


```Matlab
 function notes(varargin)
    % NOTES can be used for taking notes
    % usage:
    %    notes    displays the content of the file NOTES.TXT
    %    notes arg1 arg2 ...
    %             add the current date, time and arg# to NOTES.TXT
    %

    filename = 'NOTES.TXT';
    if nargin==0
	fid = fopen(filename,'rt');
	if fid<0, return; end;
	while ~feof(fid)
		fprintf('%s\n',fgetl(fid));
	end;
	fclose(fid);
    else
        fid = fopen(filename,'a+');
	if fid<0, error('cannot open %s\n',filename); end;
        fprintf(fid, '%s\n\t%s', datestr(now),varargin{1});
        for k=2:length(varargin)
            fprintf(fid, ', %s', varargin{k});
	end;
	fprintf(fid,'\n');
	fclose(fid);
    end;
```



## Mercury


```mercury
:- module notes.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, time.

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( if Args = [] then print_notes(!IO) else add_note(Args, !IO) ).

:- pred print_notes(io::di, io::uo) is det.

print_notes(!IO) :-
   io.open_input(notes_filename, InputRes, !IO),
   (
        InputRes = ok(Input),
        io.input_stream_foldl_io(Input, io.write_char, WriteRes, !IO),
        (
            WriteRes = ok
        ;
            WriteRes = error(WriteError),
            print_io_error(WriteError, !IO)
        ),
        io.close_input(Input, !IO)
   ;
        InputRes = error(_InputError)
   ).

:- pred add_note(list(string)::in, io::di, io::uo) is det.

add_note(Words, !IO) :-
   io.open_append(notes_filename, OutputRes, !IO),
   (
       OutputRes = ok(Output),
       time(Time, !IO),
       io.write_string(Output, ctime(Time), !IO),
       io.write_char(Output, '\t', !IO),
       io.write_list(Output, Words, " ", io.write_string, !IO),
       io.nl(Output, !IO),
       io.close_output(Output, !IO)
   ;
       OutputRes = error(OutputError),
       print_io_error(OutputError, !IO)
   ).

:- pred print_io_error(io.error::in, io::di, io::uo) is det.

print_io_error(Error, !IO) :-
   io.stderr_stream(Stderr, !IO),
   io.write_string(Stderr, io.error_message(Error), !IO),
   io.set_exit_status(1, !IO).

:- func notes_filename = string.

notes_filename = "NOTES.TXT".

:- end_module notes.
```



## Nim


```nim
import os, times, strutils

if paramCount() == 0:
  try: stdout.write readFile("notes.txt")
  except IOError: discard
else:
  var f = open("notes.txt", fmAppend)
  f.writeln getTime()
  f.writeln "\t", commandLineParams().join(" ")
  f.close()
```

Sample format.txt:

```txt
Mon Jul 14 03:37:20 2014
	Hello World
Mon Jul 14 03:37:45 2014
	Hello World again
```



## OCaml


```ocaml
#! /usr/bin/env ocaml
#load "unix.cma"

let notes_file = "notes.txt"

let take_notes() =
  let gmt = Unix.gmtime (Unix.time()) in
  let date =
    Printf.sprintf "%d-%02d-%02d %02d:%02d:%02d"
      (1900 + gmt.Unix.tm_year) (1 + gmt.Unix.tm_mon) gmt.Unix.tm_mday
      gmt.Unix.tm_hour gmt.Unix.tm_min gmt.Unix.tm_sec
  in
  let oc = open_out_gen [Open_append; Open_creat; Open_text] 0o644 notes_file in
  output_string oc (date ^ "\t");
  output_string oc (String.concat " " (List.tl(Array.to_list Sys.argv)));
  output_string oc "\n";
;;

let dump_notes() =
  if not(Sys.file_exists notes_file)
  then (prerr_endline "no local notes found"; exit 1);
  let ic = open_in notes_file in
  try while true do
    print_endline (input_line ic)
  done with End_of_file ->
    close_in ic

let () =
  if Array.length Sys.argv = 1
  then dump_notes()
  else take_notes()
```



## Oz


```oz
functor
import
   Application
   Open
   OS
   System
define
   fun {TimeStamp}
      N = {OS.localTime}
   in
      (1900+N.year)#"-"#(1+N.mon)#"-"#N.mDay#", "#N.hour#":"#N.min#":"#N.sec
   end

   fun {Join X|Xr Sep}
      {FoldL Xr fun {$ Z X} Z#Sep#X end X}
   end

   case {Application.getArgs plain}
   of nil then
      try
         F = {New Open.file init(name:"notes.txt")}
      in
         {System.printInfo {F read(list:$ size:all)}}
         {F close}
      catch _ then skip end
   [] Args then
      F = {New Open.file init(name:"notes.txt" flags:[write text create append])}
   in
      {F write(vs:{TimeStamp}#"\n")}
      {F write(vs:"\t"#{Join Args " "}#"\n")}
      {F close}
   end
   {Application.exit 0}
end
```



## Pascal

Free Pascal in Delphi mode or Delphi in console mode.


```Pascal

{$mode delphi}
PROGRAM notes;
// Notes: a time-stamped command line notebook
// usage: >notes "note"< or >notes< to display contents
USES Classes, SysUtils;

VAR
	Note  : TStringList;
	Fname : STRING = 'Notes.txt';
	Dtime : STRING;
	Ntext : STRING;
	c     : Cardinal;

BEGIN
	DTime := FormatDateTime('YYYY-MM-DD-hhnn',Now);
	Note  := TStringList.Create;
	WITH Note DO BEGIN
		TRY
			LoadFromFile(Fname);
		EXCEPT
			Add(DTime);
			NText := 'Notes.txt created.';
		END;
		// command line args present:
		// add note with date & time
		IF ParamStr(1) <> '' THEN BEGIN
			NText := ParamStr(1);
			Add(DTime);
			Add(NText);
			SaveToFile(Fname);
		// command line args absent:
		// display contents of notebook
		END ELSE
			FOR c := 0 TO Count-1 DO
				Writeln(Note[c]);
		Free;
	END;
END.

```


{{out|Program usage and output}}

```txt

>notes "Done: coded notes.pas for RC. It was quick."
>notes
2013-09-18-2243
Notes.txt created.
2013-09-18-2252
To do: code notes.pas for RC.
2013-09-18-2253
Done: coded notes.pas for RC. It was quick.

```



## Perl


```Perl
my $file = 'notes.txt';
if ( @ARGV ) {
  open NOTES, '>>', $file or die "Can't append to file $file: $!";
  print NOTES scalar localtime, "\n\t@ARGV\n";
} else {
  open NOTES, '<', $file or die "Can't read file $file: $!";
  print <NOTES>;
}
close NOTES;
```


{{out}} after a few runs:

```txt

Thu Apr 22 19:01:41 2010
	Test line 1.
Thu Apr 22 19:01:45 2010
	Test line 2.

```



## Perl 6



```perl6
my $file = 'notes.txt';

multi MAIN() {
    print slurp($file);
}

multi MAIN(*@note) {
    my $fh = open($file, :a);
    $fh.say: DateTime.now, "\n\t", @note;
    $fh.close;
}
```



## Phix

Copy of [[Take_notes_on_the_command_line#Euphoria|Euphoria]]

```Phix
constant cmd = command_line()
constant filename = "notes.txt"
integer fn
object line
if length(cmd)<3 then
    fn = open(filename,"r")
    if fn!=-1 then
        while 1 do
            line = gets(fn)
            if atom(line) then exit end if
            puts(1,line)
        end while
        close(fn)
    end if
else
    fn = open(filename,"a") -- if such file doesn't exist it will be created
    printf(fn,"%d-%02d-%02d %d:%02d:%02d\n",date())
    printf(fn,"\t%s\n",join(cmd[3..$]))
    close(fn)
end if
```



## PicoLisp


```PicoLisp
#!/usr/bin/picolisp /usr/lib/picolisp/lib.l

(load "@lib/misc.l")
(if (argv)
   (out "+notes.txt" (prinl (stamp) "^J^I" (glue " " @)))
   (and (info "notes.txt") (in "notes.txt" (echo))) )
(bye)
```



## PL/I


```PL/I
NOTES: procedure (text) options (main); /* 8 April 2014 */
   declare text character (100) varying;
   declare note_file file;

   on undefinedfile(note_file) go to file_does_not_exist;
   open file (note_file) title ('/NOTES.TXT,recsize(100),type(text)');
   revert error;

   if text = '' then
      do;
         on endfile (note_file) stop;

         do forever;
            get file (note_file) edit (text) (L);
            put skip list (text);
         end;
      end;
   close file (note_file);
   open file (note_file) output title ('/NOTES.TXT,recsize(100),type(text),append(y)');

   put file (note_file) skip list (DATETIME('DDMmmYYYY'), TIME());
   put file (note_file) skip list (text);
   put file (note_file) skip;

   put skip list ('Appended ' || text || ' to file');

   return;

file_does_not_exist:
   revert undefinedfile (note_file);
   close file (note_file);
   open file (note_file) output title ('/NOTES.TXT,recsize(100),type(text)');
   put file (note_file) skip list (DATETIME('DDMmmYYYY'), TIME());
   put file (note_file) skip list (text);
   put file (note_file) skip;
   put skip list ('The file, NOTES.TXT, has been created');
end NOTES;
```



## PowerShell


```powershell
$notes = "notes.txt"
if (($args).length -eq 0) {
    if(Test-Path $notes) {
        Get-Content $notes
    }
} else {
    Get-Date | Add-Content $notes
    "`t" + $args -join " " | Add-Content $notes
}
```



## PureBasic


```PureBasic
#FileName="notes.txt"
Define argc=CountProgramParameters()
If OpenConsole()
  If argc=0
    If ReadFile(0,#FileName)
      While Eof(0)=0
        PrintN(ReadString(0))                    ; No new notes, so present the old
      Wend
      CloseFile(0)
    EndIf
  Else ; e.g. we have some arguments
    Define d$=FormatDate("%yyyy-%mm-%dd %hh:%ii:%ss",date())
    If OpenFile(0,#FileName)
      Define args$=""
      While argc
        args$+" "+ProgramParameter()             ; Read all arguments
        argc-1
      Wend
      FileSeek(0,Lof(0))                         ; Go to the end of this file
      WriteStringN(0,d$+#CRLF$+#TAB$+args$)      ; Append date & note
      CloseFile(0)
    EndIf
  EndIf
EndIf
```



## Python


```python
import sys, datetime, shutil

if len(sys.argv) == 1:
    try:
        with open('notes.txt', 'r') as f:
            shutil.copyfileobj(f, sys.stdout)
    except IOError:
        pass
else:
    with open('notes.txt', 'a') as f:
        f.write(datetime.datetime.now().isoformat() + '\n')
        f.write("\t%s\n" % ' '.join(sys.argv[1:]))
```


'''Sample notes.txt file'''

After assorted runs:

```txt
2010-04-01T17:06:20.312000
	go for it
2010-04-01T17:08:20.718000
	go for it
```



## PHP


```php

#!/usr/bin/php
<?php
if ($argc > 1)
    file_put_contents(
        'notes.txt',
        date('r')."\n\t".implode(' ', array_slice($argv, 1))."\n",
        FILE_APPEND
    );
else
    @readfile('notes.txt');

```

Note that the error suppression operator (@) is considered bad coding practice.

'''Sample notes.txt file'''


```txt

zls@zls:~$ ./notes hello rosetta code
zls@zls:~$ ./notes todo notes program
zls@zls:~$ ./notes
Tue, 12 Jun 2012 19:27:35 +0200
	hello rosetta code
Tue, 12 Jun 2012 19:27:45 +0200
	todo notes program

```



## R


```R
#!/usr/bin/env Rscript --default-packages=methods

args <- commandArgs(trailingOnly=TRUE)

if (length(args) == 0) {
  conn <- file("notes.txt", 'r')
  cat(readLines(conn), sep="\n")
} else {
  conn <- file("notes.txt", 'a')
  cat(file=conn, date(), "\n\t", paste(args, collapse=" "), "\n", sep="")
}
close(conn)
```



## Racket



```Racket

#!/usr/bin/env racket
#lang racket
(define file "NOTES.TXT")
(require racket/date)
(command-line #:args notes
  (if (null? notes)
    (if (file-exists? file)
      (call-with-input-file* file
        (λ(i) (copy-port i (current-output-port))))
      (raise-user-error 'notes "missing ~a file" file))
    (call-with-output-file* file #:exists 'append
      (λ(o) (fprintf o "~a\n\t~a\n"
                     (date->string (current-date) #t)
                     (string-join notes))))))

```



## REBOL


```REBOL
REBOL [
   Title: "Notes"
   URL: http://rosettacode.org/wiki/Take_notes_on_the_command_line
]

notes: %notes.txt

either any [none? args: system/script/args  empty? args] [
   if exists? notes [print read notes]
] [
   write/binary/append notes rejoin [now lf tab args lf]
]
```


{{out|Sample session}}

```txt
> rebol -w notes.r
> rebol -w notes.r "Test line 1"
> rebol -w notes.r "Test" "line" "2"
> rebol -w notes.r Test line 3
> rebol -w notes.r
4-Oct-2010/21:45:16-5:00
	Test line 1
4-Oct-2010/21:45:27-5:00
	Test line 2
4-Oct-2010/21:45:36-5:00
	Test line 3

```



## REXX


```rexx
/*REXX program  implements the  "NOTES"  command  (append text to a file from the C.L.).*/
timestamp=right(date(),11,0)  time()  date('W')  /*create a (current) date & time stamp.*/
nFID = 'NOTES.TXT'                               /*the  fileID  of the  "notes"  file.  */

if 'f2'x==2  then tab="05"x                      /*this is an EBCDIC system.            */
             else tab="09"x                      /*  "   "  "  ASCII    "               */

if arg()==0  then do  while lines(nFID)          /*No arguments?  Then display the file.*/
                  say linein(Nfid)               /*display  a  line of file ──► screen. */
                  end   /*while*/
             else do
                  call lineout nFID,timestamp    /*append the timestamp to "notes" file.*/
                  call lineout nFID,tab||arg(1)  /*   "    "     text    "    "      "  */
                  end
                                                 /*stick a fork in it,  we're all done. */
```



## Ruby


```ruby
notes = 'NOTES.TXT'
if ARGV.empty?
  File.copy_stream(notes, $stdout) rescue nil
else
  File.open(notes, 'a') {|file| file.puts "%s\n\t%s" % [Time.now, ARGV.join(' ')]}
end
```



## Rust

This uses version 0.4 of the <code>chrono</code> crate

```Rust
extern crate chrono;

use std::fs::OpenOptions;
use std::io::{self, BufReader, BufWriter};
use std::io::prelude::*;
use std::env;

const FILENAME: &str = "NOTES.TXT";

fn show_notes() -> Result<(), io::Error> {
    let file = OpenOptions::new()
        .read(true)
        .create(true) // create the file if not found
        .write(true) // necessary to create the file
        .open(FILENAME)?;
    let mut buf_reader = BufReader::new(file);
    let mut contents = String::new();
    buf_reader.read_to_string(&mut contents)?;
    println!("{}", contents);
    Ok(())
}

fn add_to_notes(note: &str) -> Result<(), io::Error> {
    let file = OpenOptions::new()
        .append(true) // disables overwriting, writes to the end of the file
        .create(true)
        .open(FILENAME)?;
    let mut buf_writer = BufWriter::new(file);

    let date_and_time = chrono::Local::now();
    writeln!(buf_writer, "{}", date_and_time)?;

    writeln!(buf_writer, "\t{}", note)
}

fn main() {
    let note = env::args().skip(1).collect::<Vec<_>>();

    if note.is_empty() {
        show_notes().expect("failed to print NOTES.TXT");
    } else {
        add_to_notes(&note.join(" ")).expect("failed to write to NOTES.TXT");
    }
}
```



## Scala

{{libheader|Scala}}

```scala
import java.io.{ FileNotFoundException, FileOutputStream, PrintStream }
import java.time.LocalDateTime

object TakeNotes extends App {
  val notesFileName = "notes.txt"
  if (args.length > 0) {
    val ps = new PrintStream(new FileOutputStream(notesFileName, true))
    ps.println(LocalDateTime.now() + args.mkString("\n\t", " ", "."))
    ps.close()
  } else try {
    io.Source.fromFile(notesFileName).getLines().foreach(println)
  } catch {
    case e: FileNotFoundException => println(e.getLocalizedMessage())
    case e: Throwable => {
      println("Some other exception type:")
      e.printStackTrace()
    }
  }
}
```



## Scheme

(moved from Racket)
{{works with|Racket}}

```scheme
#lang racket
(require racket/date)
(define *notes* "NOTES.TXT")

(let ([a (vector->list (current-command-line-arguments))])
  (cond
    [(empty? a)
     (with-handlers ([exn:fail? void])
       (call-with-input-file *notes*
         (lambda (fi)
           (copy-port fi (current-output-port)))))
     ]
    [else
     (call-with-output-file *notes*
       (lambda (fo)
         (let ([ln (apply string-append (add-between a " "))]
               [dt (date->string (current-date))])
           (fprintf fo "~a~n\t~a~n" dt ln)))
       #:mode 'text #:exists 'append)
     ]))
```



## Seed7


```seed7
$ include "seed7_05.s7i";
$ include "getf.s7i";
$ include "time.s7i";

const string: noteFileName is "NOTES.TXT";

const proc: main is func
  local
    var file: note is STD_NULL;
  begin
    if length(argv(PROGRAM)) = 0 then
      # write NOTES.TXT
      write(getf(noteFileName));
    else
      # Write date & time to NOTES.TXT, and then arguments
      note := open(noteFileName, "a");
      if note <> STD_NULL then
        writeln(note, truncToSecond(time(NOW)));
        writeln(note, "\t" <& join(argv(PROGRAM), " "));
        close(note);
      end if;
    end if;
  end func;
```



## Sidef


```ruby
var file = %f'notes.txt'

if (ARGV.len > 0) {
    var fh = file.open_a
    fh.say(Time.local.ctime + "\n\t" + ARGV.join(" "))
    fh.close
} else {
    var fh = file.open_r
    fh && fh.each { .say }
}
```

{{out}}

```txt

$ sidef notes.sf Test 1
$ sidef notes.sf Test 2
$ sidef notes.sf
Sat Sep 12 00:19:36 2015
	Test 1
Sat Sep 12 00:19:37 2015
	Test 2

```



## SNOBOL4

{{works with|Macro SNOBOL4 in C}}

```snobol
#! /usr/local/bin/snobol4 -b
	a = 2 ;* skip '-b' parameter
	notefile = "notes.txt"
while 	args = args host(2,a = a + 1) " "		:s(while)
	ident(args)	:f(append)
noparms	input(.notes,io_findunit(),,notefile)		:s(display)f(end)
display	output = notes					:s(display)
	endfile(notes)					:(end)
append	output(.notes,io_findunit(),"A",notefile)	:f(end)
	notes = date()
	notes = char(9) args
end
```



## Swift


```Swift
import Foundation

let args = Process.arguments
let manager = NSFileManager()
let currentPath = manager.currentDirectoryPath
var err:NSError?

// Create file if it doesn't exist
if !manager.fileExistsAtPath(currentPath + "/notes.txt") {
    println("notes.txt doesn't exist")
    manager.createFileAtPath(currentPath + "/notes.txt", contents: nil, attributes: nil)
}

// handler is what is used to write to the file
let handler = NSFileHandle(forUpdatingAtPath: currentPath + "/notes.txt")

// Print the file if there are no args
if args.count == 1 {
    let str = NSString(contentsOfFile: currentPath + "/notes.txt", encoding: NSUTF8StringEncoding, error: &err)
    println(str!)
    exit(0)
}

let time = NSDate()
let format = NSDateFormatter()
let timeData = (format.stringFromDate(time) + "\n").dataUsingEncoding(NSUTF8StringEncoding, allowLossyConversion: false)
format.dateFormat = "yyyy.MM.dd 'at' HH:mm:ss zzz"

// We're writing to the end of the file
handler?.seekToEndOfFile()
handler?.writeData(timeData!)

var str = "\t"
for i in 1..<args.count {
    str += args[i] + " "
}

str += "\n"

let strData = str.dataUsingEncoding(NSUTF8StringEncoding, allowLossyConversion: false)
handler?.writeData(strData!)
```

{{out}}
Example output

```txt

2015.01.22 at 14:19:18 EST
	this is a note
2015.01.22 at 14:19:36 EST
	this is something important

```



## Tcl


```tcl
# Make it easier to change the name of the notes file
set notefile notes.txt

if {$argc} {
    # Write a message to the file
    set msg [clock format [clock seconds]]\n\t[join $argv " "]
    set f [open $notefile a]
    puts $f $msg
    close $f
} else {
    # Print the contents of the file
    catch {
	set f [open $notefile]
	fcopy $f stdout
	close $f
    }
}
```

{{out}} after a few runs:

```txt

Thu Apr 01 19:28:07 BST 2010
	test 1 two three
Thu Apr 01 19:28:49 BST 2010
	TODO: Submit notes.tcl to Rosetta Code

```



## UNIX Shell

Bash version

```bash
#
NOTES=$HOME/notes.txt
if [[ $# -eq 0 ]] ; then
  [[ -r  $NOTES ]] && more $NOTES
else
  date >> $NOTES
  echo "  $*" >> $NOTES
fi
```


"Spoiled Bash kid" version

```bash
N=~/notes.txt;[[ $# -gt 0 ]] && { date ; echo " $*"; exit 0; }  >> $N || [[ -r $N ]] && cat $N
```



Portable version

```bash
NOTES=$HOME/notes.txt
if test "x$*" = "x"
then
        if test -r  $NOTES
        then
                more $NOTES
        fi
else
        date >> $NOTES
        echo "  $*" >> $NOTES
fi
```


{{omit from|Retro}}


## Visual Basic .NET


```vbnet
Imports System.IO

Module Notes
    Function Main(ByVal cmdArgs() As String) As Integer
        Try
            If cmdArgs.Length = 0 Then
                Using sr As New StreamReader("NOTES.TXT")
                    Console.WriteLine(sr.ReadToEnd)
                End Using
            Else
                Using sw As New StreamWriter("NOTES.TXT", True)
                    sw.WriteLine(Date.Now.ToString())
                    sw.WriteLine("{0}{1}", ControlChars.Tab, String.Join(" ", cmdArgs))
                End Using
            End If
        Catch
        End Try
    End Function
End Module
```



## zkl

File notes.zkl:

```zkl
const notesName="NOTES.TXT";
args:=vm.arglist;
if (not args)
   { try{ File(notesName).read(*).text.print(); } catch{println("no file")} }
else{
   f:=File(notesName,"a+");
   f.writeln(Time.Date.ctime(),"\n\t",args.concat(" "));
   f.close();
}
```

{{out}}

```txt

$ zkl notes
no file
$ zkl notes this is a test
$ zkl notes
Tue Apr 8 20:51:16 2014
	this is a test

```

