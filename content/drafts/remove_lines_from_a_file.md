+++
title = "Remove lines from a file"
description = ""
date = 2018-12-08T11:22:33Z
aliases = []
[extra]
id = 10088
[taxonomies]
categories = []
tags = []
+++

{{task}}[[Category:File handling]]

;Task:
Remove a specific line or a number of lines from a file.

This should be implemented as a routine that takes three parameters (filename, starting line, and the number of lines to be removed).

For the purpose of this task, line numbers and the number of lines start at one, so to remove the first two lines from the file <tt>foobar.txt</tt>, the parameters should be: <tt>foobar.txt</tt>, <tt>1</tt>, <tt>2</tt>

Empty lines are considered and should still be counted, and if the specified line is empty, it should still be removed.

An appropriate message should appear if an attempt is made to remove lines beyond the end of the file.





## Ada


```Ada
with Ada.Text_IO, Ada.Directories, Ada.Command_Line, Ada.IO_Exceptions;
use Ada.Text_IO;

procedure Remove_Lines_From_File is
   Temporary: constant String := ".tmp";
begin
   if Ada.Command_Line.Argument_Count /= 3 then
      raise Constraint_Error;
   end if;
   declare
      Filename: String := Ada.Command_Line.Argument(1);
      First: Positive := Integer'Value(Ada.Command_Line.Argument(2));
      Last: Natural := Integer'Value(Ada.Command_Line.Argument(3)) + First - 1;
      Input, Output: File_Type;
      Line_Number: Positive := 1;
   begin
      Open(Input, In_File, Filename); -- open original file for reading
      Create(Output, Out_File, Filename & Temporary); -- write to temp. file
      while not End_Of_File(Input) loop
         declare
            Line: String := Get_Line(Input);
         begin
            if Line_Number < First or else Line_Number > Last then
               Put_Line(Output, Line);
            end if;
         end;
         Line_Number := Line_Number + 1;
      end loop;
      Close(Input);
      Close(Output);
      Ada.Directories.Rename(Old_Name => Filename & Temporary,
                             New_Name => Filename);
   end;
exception
   when Constraint_Error | Ada.IO_Exceptions.Name_Error =>
      Put_Line("usage: " & Ada.Command_Line.Command_Name &
                 " <filename> <first> <length>");
      Put_Line("  opens <filename> for reading and " &
                 "<filename>" & Temporary & " for temporary writing");
      Put_Line("  requires first > 0, length >= 0");
end Remove_Lines_From_File;
```



## ALGOL 68


```algol68
# removes the specified number of lines from a file, starting at start line (numbered from 1) #
# returns TRUE if successful, FALSE otherwise #
PROC remove lines = ( STRING file name, INT start line, INT number of lines )BOOL:
     IF  number of lines < 0 OR start line < 1
     THEN
        # invalid parameters #
        print( ( "number of lines must be >= 0 and start line must be >= 1", newline ) );
        FALSE
     ELIF  FILE temp file;
           create( temp file, stand back channel ) /= 0
     THEN
        # unable to create a temporary output file #
        print( (  "Unable to create temporary file", newline ) );
        FALSE
     ELIF NOT reset possible( temp file )
     THEN
        # rewinding the temporary file is not possible #
        # we would have to get its name ( with "idf( temp file )", close it and re-open it #
        print( ( "Temp file is not rewindable", newline ) );
        FALSE
     ELIF  FILE input file;
           open( input file, file name, stand in channel ) /= 0
     THEN
        # failed to open the file #
        print( (  "Unable to open """ + file name + """", newline ) );
        FALSE
     ELSE
        # files opened OK #
        BOOL at eof := FALSE;
        # set the EOF handler for the original file #
        on logical file end( input file
                           , ( REF FILE f )BOOL:
                             BEGIN
                                 # note that we reached EOF on the latest read #
                                 at eof := TRUE;
                                 # return TRUE so processing can continue #
                                 TRUE
                             END
                           );
        # copy the input file to the temp file #
        WHILE STRING line;
              get( input file, ( line, newline ) );
              NOT at eof
        DO
            put( temp file,    ( line, newline ) )
        OD;
        # copy the temp file back to the input, removing the specified lines #
        close( input file );
        IF   open( input file, file name, stand out channel ) /= 0
        THEN
            # failed to open the original file for output #
            print( ( "Unable to open ", file name, " for output", newline ) );
            FALSE
        ELSE
            # opened OK - copy the temporary file #
            reset( temp file ); # rewinds the input file #
            at eof := FALSE;
            on logical file end( temp file
                               , ( REF FILE f )BOOL:
                                 BEGIN
                                     # note that we reached EOF on the latest read #
                                     at eof := TRUE;
                                     # return TRUE so processing can continue #
                                     TRUE
                                 END
                               );
            INT end line = ( start line - 1 ) + number of lines;
            INT line number := 0;
            WHILE STRING line;
                  get( temp file, ( line, newline ) );
                  NOT at eof
            DO
                # have another line #
                line number +:= 1;
                IF line number < start line OR line number > end line
                THEN
                    put( input file, ( line, newline ) )
                FI
            OD;
            # close the files #
            close(   input file );
            scratch( temp file  );
            IF   line number < start line
            THEN
                # didn't find the start line #
                print( ( "Specified start line (", whole( start line, 0 ), ") not in ", file name, newline ) );
                FALSE
            ELIF line number < end line
            THEN
                # the ending line was not in the file #
                print( ( "Final omitted line not in the file ", file name, newline ) );
                FALSE
            ELSE
                # successful operation #
                TRUE
            FI
        FI
     FI # remove lines # ;

# test the line removal #
BEGIN
    FILE t;
    open( t, "test.txt", stand out channel );
    print( ( "Before...", newline ) );
    FOR i TO 10 DO
        STRING line = whole( i, - ( ( i MOD 5 ) + 3 ) );
        put( t, ( line, newline ) );
        print(  ( line, newline ) )
    OD;
    close( t );
    remove lines( "test.txt", 2, 3 );
    print( ( "After...", newline ) );
    open( t, "test.txt", stand in channel );
    FOR i TO  7 DO
        STRING line;
        get( t, ( line, newline ) );
        print(  ( line, newline ) )
    OD;
    close( t );
    print( ( "----", newline ) )
END
```

{{out}}

```txt

Before...
   1
    2
     3
      4
  5
   6
    7
     8
      9
 10
After...
   1
  5
   6
    7
     8
      9
 10
----

```



## AutoHotkey


```AHK
RemoveLines(filename, startingLine, numOfLines){
       Loop, Read, %filename%
               if ( A_Index < StartingLine )
                       || ( A_Index >= StartingLine + numOfLines )
                               ret .= "`r`n" . A_LoopReadLine
       FileDelete, % FileName
       FileAppend, % SubStr(ret, 3), % FileName
}

SetWorkingDir, % A_ScriptDir
RemoveLines("test.txt", 4, 3)
```

;Output:
with test.txt starting as

```txt
1
2
3
4
5
6
7
8
```

Running the code it is now

```txt
1
2
3
7
8
```



## AWK


```AWK

# syntax: GAWK -f REMOVE_LINES_FROM_A_FILE.AWK
# show files after lines are removed:
#   GAWK "FNR==1{print(FILENAME)};{print(FNR,$0)}" TEST1 TEST2 TEST3
BEGIN {
    build_test_data()
    remove_lines("TEST0",1,1)
    remove_lines("TEST1",3,4)
    remove_lines("TEST2",9,3)
    remove_lines("TEST3",11,1)
    exit(errors+0)
}
function build_test_data(  fn,i,j) { # create 3 files with 10 lines each
    for (i=1; i<=3; i++) {
      fn = "TEST" i
      for (j=1; j<=10; j++) {
        printf("line %d\n",j) >fn
      }
      close(fn)
    }
}
function remove_lines(fn,start,number_of_lines,  arr,fnr,i,n,rec,stop) {
    stop = start + number_of_lines - 1
    while (getline rec <fn > 0) { # read file
      fnr++
      if (fnr < start || fnr > stop) {
        arr[++n] = rec
      }
    }
    close(fn)
    if (fnr == 0) {
      printf("error: file %s not found\n",fn)
      errors = 1
      return
    }
    for (i=1; i<=n; i++) { # write file
      printf("%s\n",arr[i]) >fn
    }
    close(fn)
    if (stop > fnr) {
      printf("error: file %s trying to remove nonexistent lines\n",fn)
      errors = 1
    }
}

```


## BASIC

Compatible with VB-DOS, QBasic, QuickBASIC 4.5, PDS 7.1, QB64

Verbose and with a program loop to test the procedures.

```qbasic

' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' '
' Remove File Lines V1.1                            '
'                                                   '
' Developed by A. David Garza Marín in VB-DOS for   '
' RosettaCode. November 30, 2016.                   '
'                                                   '
' Date      | Change                                '
'-------------------------------------------------- '
' 2016/11/30| Original version                      '
' 2016/12/30| Added functionality to read parameters'
'           | from Command Line                     '
' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' '

'OPTION _EXPLICIT ' For QB45
OPTION EXPLICIT ' For VBDOS, PDS 7.1

' SUBs and FUNCTIONs
DECLARE FUNCTION DeleteLinesFromFile% (WhichFile AS STRING, Start AS LONG, HowMany AS LONG)
DECLARE FUNCTION FileExists% (WhichFile AS STRING)
DECLARE FUNCTION GetDummyFile$ (WhichFile AS STRING)
DECLARE FUNCTION getFileName$ (CommandString AS STRING)
DECLARE FUNCTION getHowManyLines& (CommandLine AS STRING)
DECLARE FUNCTION getStartPoint& (CommandLine AS STRING)
DECLARE FUNCTION ErrorMessage$ (WhichError AS INTEGER)
DECLARE FUNCTION CountLines& (WhichFile AS STRING)

' Var
DIM iOk AS INTEGER, iErr AS INTEGER, lStart AS LONG, lHowMany AS LONG, lSize AS LONG
DIM sFile AS STRING, sCommand AS STRING

' Const
CONST ProgramName = "RemFLine (Remove File Lines) Enhanced V1.1"

' ----------------------------- Main program cycle --------------------------------
CLS
PRINT ProgramName
PRINT
PRINT "This program will remove as many lines of a text file as you state, starting"
PRINT "with the line number you also state. If the starting line number is beyond"
PRINT "total lines in the text file stated, then the process will be aborted. If the"
PRINT "quantity of lines stated to be deleted is beyond the total lines in the text"
PRINT "file, the process also will be aborted. The program will give you a message"
PRINT "if everything ran ok or if any error happened. Includes a function to count"
PRINT "how many lines has the intended file."
' Verifies if parameters are specified
sCommand = COMMAND$
IF sCommand <> "" THEN
  sFile = getFileName$(sCommand)
  lSize = CountLines&(sFile)
  lStart = getStartPoint&(sCommand)  ' Defaults to 1
  lHowMany = getHowManyLines&(sCommand) ' Defaults to 1
ELSE
  PRINT
  INPUT "Please, type the name of the file"; sFile
  sFile = LTRIM$(RTRIM$(sFile))
  IF sFile <> "" THEN
    lSize = CountLines&(sFile)
    IF lSize > 0 THEN
      PRINT "Delete starting on which line (Default=1, Max="; lSize; ")";
      INPUT lStart

      IF lStart = 0 THEN lStart = 1
      IF lStart < lSize THEN
        PRINT "How many lines do you want to remove (Default=1, Max="; (lSize - lStart) + 1; ")";
        INPUT lHowMany
        IF lHowMany = 0 THEN lHowMany = 1
      END IF
    END IF
  END IF
END IF

PRINT
PRINT "Erasing "; lHowMany; "lines from "; sFile; " starting on line"; lStart; "."
IF lSize > 0 THEN
  IF lHowMany + lStart <= lSize THEN
    iOk = DeleteLinesFromFile%(sFile, lStart, lHowMany)
  ELSEIF lHowMany + lStart > lSize THEN
    iOk = 1
  ELSEIF lStart > lSize THEN
    iOk = 2
  END IF
ELSEIF lSize = -1 THEN
  iOk = 3
END IF

IF lSize = -1 THEN
  iOk = 3
ELSEIF lSize = 0 THEN
  iOk = 4  ' The file is not a text file
END IF

IF sFile = "" THEN
  iOk = 5  ' Null file name not allowed
END IF

PRINT
PRINT ErrorMessage$(iOk)
'----------------End of Main program Cycle ----------------

END

FileError:
    iErr = ERR
RESUME NEXT

FUNCTION CountLines& (WhichFile AS STRING)
    ' Var
    DIM iFile AS INTEGER
    DIM l AS LONG, li AS LONG, j AS LONG, lFileSize AS LONG, lLines AS LONG
    DIM sLine AS STRING, strR AS STRING

    ' This function will count how many lines has the file
    IF FileExists%(WhichFile) THEN
        strR = CHR$(13)
        li = 1
        iFile = FREEFILE
        sLine = SPACE$(128)
        lLines = 0
        OPEN WhichFile FOR BINARY AS #iFile
          lFileSize = LOF(iFile)
          DO
            IF (LOC(iFile) + LEN(sLine)) > lFileSize THEN
              sLine = SPACE$(lFileSize - LOC(iFile))
            END IF
            IF LEN(sLine) > 0 THEN
              GET #iFile, , sLine
              GOSUB AnalizeLine
            END IF
          LOOP UNTIL LEN(sLine) < 128
        CLOSE iFile
    ELSE
        lLines = -1
    END IF

    CountLines& = lLines

EXIT FUNCTION

AnalizeLine:
   li = 1
   DO
     l = INSTR(li, sLine, strR)
     IF l > 0 THEN
       lLines = lLines + 1
       li = l + 1
     END IF
   LOOP UNTIL l = 0
RETURN
END FUNCTION

FUNCTION DeleteLinesFromFile% (WhichFile AS STRING, Start AS LONG, HowMany AS LONG)
    ' Var
    DIM lCount AS LONG, iFile AS INTEGER, iFile2 AS INTEGER, lhm AS LONG, iError AS INTEGER
    DIM sLine AS STRING, sDummyFile AS STRING

    IF FileExists%(WhichFile) THEN
        sDummyFile = GetDummyFile$(WhichFile)

        ' It is assumed a text file
        iFile = FREEFILE
        OPEN WhichFile FOR INPUT AS #iFile

        iFile2 = FREEFILE
        OPEN sDummyFile FOR OUTPUT AS #iFile2

        lhm = 0
        DO WHILE NOT EOF(iFile)
            LINE INPUT #iFile, sLine
            lCount = lCount + 1
            IF lCount >= Start AND lhm < HowMany THEN
                lhm = lhm + 1
            ELSE
                PRINT #iFile2, sLine
            END IF
        LOOP

        CLOSE iFile2, iFile

        ' Check if everything went ok or not
        iError = 0
        IF lCount < Start THEN
            iError = 2 ' Full file is shorter than the start line stated,
                       '  process will be aborted.
        ELSEIF lhm < HowMany THEN
            iError = 1 ' File was shorter than lines requested to be removed,
                       '  process will be aborted.
        END IF

        IF iError > 0 THEN
          KILL sDummyFile  ' Process aborted
        ELSE
          KILL WhichFile
          NAME sDummyFile AS WhichFile
        END IF
    ELSE
      iError = 3 ' The file doesn't exist. The process is aborted.
    END IF

    DeleteLinesFromFile% = iError

END FUNCTION

FUNCTION ErrorMessage$ (WhichError AS INTEGER)
    ' Var
    DIM sError AS STRING

    SELECT CASE WhichError
      CASE 0: sError = "Everything went Ok. Lines removed from file."
      CASE 1: sError = "File is shorter than the number of lines stated to remove. Process aborted."
      CASE 2: sError = "Whole file is shorter than the starting point stated. Process aborted."
      CASE 3: sError = "File doesn't exist. Process aborted."
      CASE 4: sError = "The file doesn't seem to be a text file. Process aborted."
      CASE 5: sError = "You need to provide a valid file name, please."
    END SELECT

    ErrorMessage$ = sError
END FUNCTION

FUNCTION FileExists% (WhichFile AS STRING)
    ' Var
    DIM iFile AS INTEGER
    DIM iItExists AS INTEGER
    SHARED iErr AS INTEGER

    ON ERROR GOTO FileError
    iFile = FREEFILE
    iErr = 0
    OPEN WhichFile FOR BINARY AS #iFile
    IF iErr = 0 THEN
        iItExists = LOF(iFile) > 0
        CLOSE #iFile

        IF NOT iItExists THEN
            KILL WhichFile
        END IF
    END IF
    ON ERROR GOTO 0
    FileExists% = iItExists

END FUNCTION

FUNCTION GetDummyFile$ (WhichFile AS STRING)
    ' Var
    DIM i AS INTEGER, j AS INTEGER

    ' Gets the path specified in WhichFile
    i = 1
    DO
        j = INSTR(i, WhichFile, "\")
        IF j > 0 THEN i = j + 1
    LOOP UNTIL j = 0

    GetDummyFile$ = LEFT$(WhichFile, i - 1) + "$dummyf$.tmp"
END FUNCTION

FUNCTION getFileName$ (CommandString AS STRING)
  ' Var
  DIM i AS INTEGER
  DIM sFileName AS STRING

  i = INSTR(CommandString, ",")
  IF i > 0 THEN
    sFileName = LEFT$(CommandString, i - 1)
  ELSEIF LEN(CommandString) > 0 THEN
    sFileName = CommandString
  END IF

  getFileName$ = sFileName
END FUNCTION

FUNCTION getHowManyLines& (CommandLine AS STRING)
  ' Var
  DIM i AS INTEGER, j AS INTEGER
  DIM l AS LONG

  i = INSTR(CommandLine, ",")
  IF i > 0 THEN
    j = INSTR(i + 1, CommandLine, ",")
    IF j = 0 THEN
      l = 1
    ELSE
      l = CLNG(VAL(MID$(CommandLine, j + 1)))
    END IF
  END IF

  getHowManyLines& = l

END FUNCTION

FUNCTION getStartPoint& (CommandLine AS STRING)
  ' Var
  DIM i AS INTEGER, j AS INTEGER
  DIM l AS LONG

  i = INSTR(CommandLine, ",")
  IF i > 0 THEN
    j = INSTR(i + 1, CommandLine, ",")
    IF j = 0 THEN j = LEN(CommandLine)
    l = CLNG(VAL(MID$(CommandLine, i + 1, j - i)))
  ELSE
    i = 1
  END IF

  getStartPoint& = l

END FUNCTION


```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "RemLines.bas"
110 CALL REMOVELINES("fub.txt",5,3)
120 DEF REMOVELINES(NAME$,ST,N)
130   STRING S$*254
140   WHEN EXCEPTION USE OPENERROR
150     LET P=POS(NAME$,".")
160     IF P=0 THEN LET P=LEN(NAME$)+1
170     LET CM$="copy "&NAME$&" "&NAME$(1:P-1)&".bak"
180     EXT CM$
190     OPEN #1:NAME$(1:P-1)&".bak"
200     OPEN #2:NAME$ ACCESS OUTPUT
210   END WHEN
220   WHEN EXCEPTION USE IOERROR
230     LET LIN=0
240     DO
250       LINE INPUT #1:S$
260       LET LIN=LIN+1
270       IF LIN<ST OR LIN>ST+N-1 THEN PRINT #2:S$
280     LOOP
290   END WHEN
300   HANDLER OPENERROR
310     PRINT EXSTRING$(EXTYPE)
320     STOP
330   END HANDLER
340   HANDLER IOERROR
350     IF EXTYPE<>8001 THEN
360       PRINT EXSTRING$(EXLINE)
375       STOP
380     ELSE
390       IF LIN>=ST+N-1 THEN PRINT N;"line(s) removed."
400       IF LIN>=ST AND LIN<ST+N-1 THEN PRINT "Only";LIN-ST+1;"line(s) were removed as not enough lines to remove more."
410       IF LIN<ST THEN PRINT "No lines were removed as starting line was beyond end of file."
420     END IF
430     CLOSE #2
440     CLOSE #1
450   END HANDLER
460 END DEF
```



## C


```c
#include <stdio.h>
#include <stdlib.h> /* for atoi() and malloc() */
#include <string.h> /* for memmove() */

/* Conveniently print to standard error and exit nonzero. */
#define ERROR(fmt, arg) return fprintf(stderr, fmt "\n", arg)

int main(int argc, char **argv)
{
    FILE *fp;
    char *buf;
    size_t sz;
    int start, count, lines = 1;
    int dest = 0, src = 0, pos = -1;

    /* Initialization and sanity checks */
    if (argc != 4)
        ERROR("Usage: %s <file> <start> <count>", argv[0]);

    if ((count = atoi(argv[3])) < 1) /* We're a no-op. */
        return 0;

    if ((start = atoi(argv[2])) < 1)
        ERROR("Error: <start> (%d) must be positive", start);

    if ((fp = fopen(argv[1], "r")) == NULL)
        ERROR("No such file: %s", argv[1]);

    /* Determine filesize and allocate a buffer accordingly. */
    fseek(fp, 0, SEEK_END);
    sz = ftell(fp);
    buf = malloc(sz + 1);
    rewind(fp);

    /* Fill the buffer, count lines, and remember a few important offsets. */
    while ((buf[++pos] = fgetc(fp)) != EOF) {
        if (buf[pos] == '\n') {
            ++lines;
            if (lines == start) dest = pos + 1;
            if (lines == start + count) src = pos + 1;
        }
    }

    /* We've been asked to do something weird; clean up and bail. */
    if (start + count > lines) {
        free(buf);
        fclose(fp);
        ERROR("Error: invalid parameters for file with %d lines", --lines);
    }

    /* Overwrite the lines to be deleted with the survivors. */
    memmove(buf + dest, buf + src, pos - src);

    /* Reopen the file and write back just enough of the buffer. */
    freopen(argv[1], "w", fp);
    fwrite(buf, pos - src + dest, 1, fp);

    free(buf);
    fclose(fp);
    return 0;
}
```



## C sharp

{{works with|C sharp|6}}

```csharp
using System;
using System.IO;
using System.Linq;

public class Rosetta
{
    public static void Main() => RemoveLines("foobar.txt", start: 1, count: 2);

    static void RemoveLines(string filename, int start, int count = 1) =>
        File.WriteAllLines(filename, File.ReadAllLines(filename)
            .Where((line, index) => index < start - 1 || index >= start + count - 1));
}
```



## C++


```cpp
#include <fstream>
#include <iostream>
#include <string>
#include <cstdlib>
#include <list>

void deleteLines( const std::string & , int , int ) ;

int main( int argc, char * argv[ ] ) {
   if ( argc != 4 ) {
      std::cerr << "Error! Invoke with <deletelines filename startline skipnumber>!\n" ;
      return 1 ;
   }
   std::string filename( argv[ 1 ] ) ;
   int startfrom = atoi( argv[ 2 ] ) ;
   int howmany = atoi( argv[ 3 ] ) ;
   deleteLines ( filename , startfrom , howmany ) ;
   return 0 ;
}

void deleteLines( const std::string & filename , int start , int skip ) {
   std::ifstream infile( filename.c_str( ) , std::ios::in ) ;
   if ( infile.is_open( ) ) {
      std::string line ;
      std::list<std::string> filelines ;
      while ( infile ) {
	 getline( infile , line ) ;
	 filelines.push_back( line ) ;
      }
      infile.close( ) ;
      if ( start > filelines.size( ) ) {
	 std::cerr << "Error! Starting to delete lines past the end of the file!\n" ;
	 return ;
      }
      if ( ( start + skip ) > filelines.size( ) ) {
	 std::cerr << "Error! Trying to delete lines past the end of the file!\n" ;
	 return ;
      }
      std::list<std::string>::iterator deletebegin = filelines.begin( ) , deleteend ;
      for ( int i = 1 ; i < start ; i++ )
	 deletebegin++ ;
      deleteend = deletebegin ;
      for( int i = 0 ; i < skip ; i++ )
	 deleteend++ ;
      filelines.erase( deletebegin , deleteend ) ;
      std::ofstream outfile( filename.c_str( ) , std::ios::out | std::ios::trunc ) ;
      if ( outfile.is_open( ) ) {
	 for ( std::list<std::string>::const_iterator sli = filelines.begin( ) ;
	       sli != filelines.end( ) ; sli++ )
	    outfile << *sli << "\n" ;
      }
      outfile.close( ) ;
   }
   else {
      std::cerr << "Error! Could not find file " << filename << " !\n" ;
      return ;
   }
}
```



## Clojure

Simple solution dealing with most of the lines in memory.

```clojure
(require '[clojure.java.io :as jio]
         '[clojure.string :as str])

(defn remove-lines1 [filepath start nskip]
  (let [lines (str/split-lines (slurp filepath))
        new-lines (concat (take (dec start) lines)
                          (drop (+ (dec start) nskip) lines))
        diff (- (count lines) (count new-lines))]
    (when-not (zero? diff)
      (println "WARN: You are trying to remove lines beyond EOF"))
    (with-open [wrt (jio/writer (str filepath ".tmp"))]
      (.write wrt (str/join "\n" new-lines)))
    (.renameTo (jio/file (str filepath ".tmp")) (jio/file filepath))))
```


More complex solution for big file, one line at a time.

```clojure
(require '[clojure.java.io :as jio]
         '[clojure.string :as str])

(defn remove-lines2 [filepath start nskip]
  (with-open [rdr (jio/reader filepath)]
    (with-open [wrt (jio/writer (str filepath ".tmp"))]
      (loop [s start n nskip]
        (if-let [line (.readLine rdr)]
          (cond
            (> s 1)  (do (doto wrt (.write line) (.newLine))
                         (recur (dec s) n))
            (pos? n) (recur s (dec n))
            :else    (do (doto wrt (.write line) (.newLine))
                         (recur s n)))
          (when (pos? n)
            (println "WARN: You are trying to remove lines beyond EOF"))))))
  (.renameTo (jio/file (str filepath ".tmp")) (jio/file filepath)))
```



## Common Lisp


```lisp
(defun remove-lines (filename start num)
  (let ((tmp-filename  (concatenate 'string filename ".tmp"))
	(lines-omitted 0))
    ;; Open a temp file to write the result to
    (with-open-file (out tmp-filename
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      ;; Open the original file for reading
      (with-open-file (in filename)
	(loop
	   for line = (read-line in nil 'eof)
	   for i from 1
	   until (eql line 'eof)
	   ;; Write the line to temp file if it is not in the omitted range
	   do (if (or (< i start)
		      (>= i (+ start num)))
		(write-line line out)
		(setf lines-omitted (1+ lines-omitted))))))
    ;; Swap in the temp file for the original
    (delete-file filename)
    (rename-file tmp-filename filename)
    ;; Warn if line removal went past the end of the file
    (when (< lines-omitted num)
      (warn "End of file reached with only ~d lines removed." lines-omitted))))
```



## D


```d
import std.stdio, std.file, std.string;

void main() {
    deleteLines("deleteline_test.txt", 1, 2);
}

void deleteLines(string name, int start, int num)
in {
    assert(start > 0, "Line counting must start at 1");
} body {
    start--;

    if (!exists(name) || !isFile(name))
        throw new FileException("File not found");

    auto lines = readText(name).splitLines();
    if (lines.length < start + num)
        throw new Exception("Can't delete lines past the end of file!");

    auto f = File(name, "w");
    foreach (int i, line; lines) {
        if (start > i || i >= start + num)
            f.writeln(line);
    }
}
```



## ECL

Implemented for HPCC logical files, not single physical files, since all datasets in HPCC are distributed.

```ECL

IMPORT STD;
RemoveLines(logicalfile, startline, numlines) := FUNCTIONMACRO
  EndLine := startline + numlines - 1;
  RecCnt  := COUNT(logicalfile);
  Res := logicalfile[1..startline-1] + logicalfile[endline+1..];
  RETURN WHEN(IF(RecCnt < EndLine,logicalfile,Res),
              IF(RecCnt < EndLine,STD.System.Log.addWorkunitWarning('Attempted removal past end of file-removal ignored')));
ENDMACRO;

```

And a simple test case to run:

```ECL

MyFile := DATASET(100,TRANSFORM({UNSIGNED1 RecID},SELF.RecID := COUNTER));
RemoveLines(MyFile,3,10);

```



## Elixir


```elixir
defmodule RC do
  def remove_lines(filename, start, number) do
    File.open!(filename, [:read], fn file ->
      remove_lines(file, start, number, IO.read(file, :line))
    end)
  end

  defp remove_lines(_file, 0, 0, :eof), do: :ok
  defp remove_lines(_file, _, _, :eof) do
    IO.puts(:stderr, "Warning: End of file encountered before all lines removed")
  end
  defp remove_lines(file, 0, 0, line) do
    IO.write line
    remove_lines(file, 0, 0, IO.read(file, :line))
  end
  defp remove_lines(file, 0, number, _line) do
    remove_lines(file, 0, number-1, IO.read(file, :line))
  end
  defp remove_lines(file, start, number, line) do
    IO.write line
    remove_lines(file, start-1, number, IO.read(file, :line))
  end
end

[filename, start, number] = System.argv
IO.puts "before:"
IO.puts File.read!(filename)
IO.puts "after:"
RC.remove_lines(filename, String.to_integer(start), String.to_integer(number)
```


{{out}}

```txt

C:\Elixir>elixir remove_lines.exs foobar.txt 1 2
before:
1
 2
  3
   4
    5

after:
1
   4
    5

```



## Erlang


```Erlang

-module( remove_lines ).

-export( [from_file/3, task/0] ).

from_file( Name, Start, How_many ) ->
	{Name, {ok, Binary}} = {Name, file:read_file( Name )},
	Lines = compensate_for_last_newline( lists:reverse([X || X <- binary:split( Binary, <<"\n">>, [global] )]) ),
	{Message, Keep_lines} = keep_lines( Start - 1, How_many, Lines, erlang:length(Lines) - 1 ),
	ok = file:write_file( Name, [binary:list_to_bin([X, <<"\n">>]) || X <- Keep_lines] ),
	io:fwrite( "~s~n", [Message] ).

task() ->
	file:copy( "priv/foobar.txt", "foobar.txt" ),
	from_file( "foobar.txt", 1, 2 ).



compensate_for_last_newline( [<<>> | T] ) -> lists:reverse( T );
compensate_for_last_newline( Reversed_lines ) -> lists:reverse( Reversed_lines ).

keep_lines( Start, _How_many, Lines, Available ) when Start > Available ->
	{"Start > avaliable. Nothing removed.", Lines};
keep_lines( Start, How_many, Lines, Available ) when Start + How_many  > Available ->
	Message = lists:flatten( io_lib:format("Start + How_many > avaliable. Only ~p removed.", [(Start + How_many) - Available]) ),
	{Keeps, _Removes} = lists:split( Start, Lines ),
	{Message, Keeps};
keep_lines( Start, How_many, Lines, _Available ) ->
	{Keeps, Removes} = lists:split( Start, Lines ),
	{"ok", Keeps ++ lists:nthtail( How_many, Removes )}.

```

{{out}}
With "foobar.txt" that looks like this:

```txt

1

  3
   4
    5
     6
      7
       8
        9
         10

```

The resulting contents are:

```txt

  3
   4
    5
     6
      7
       8
        9
         10

```


=={{header|F_Sharp|F#}}==

```fsharp
open System
open System.IO

let cutOut (arr : 'a[]) from n =  // confine syntax highlighting confusion'
    let slicer = fun i -> if i < from || (from + n) <= i then Some(arr.[i-1]) else None
    ((Array.choose slicer [| 1 .. arr.Length |]), from + n - arr.Length > 1)

[<EntryPoint>]
let main argv =
    let nums = Array.choose (System.Int32.TryParse >> function | true, v -> Some v | false, _ -> None) argv.[1..2]
    let lines = File.ReadAllLines(argv.[0])
    let (sliced, tooShort) = cutOut lines nums.[0] nums.[1]
    if tooShort then Console.Error.WriteLine "Not enough lines"
    File.WriteAllLines(argv.[0], sliced)
    0
```

Output

```dos>D:\Projects\Rosetta
for /l %i in (1,1,5) do @echo %i >> foo

D:\Projects\Rosetta>Remove_lines_from_a_file.exe foo 1 2

D:\Projects\Rosetta>type foo
3
4
5

D:\Projects\Rosetta>
```



## Fortran

The proper approach is to copy the source file to an output file with modifications made along the way, then when all has gone well, delete (or better, ''rename'') the source file and change the output file's name to become the original name. Otherwise, data loss is being risked. This name juggling of course invites collisions. Not a problem with Fortran, because the language provides no mechanism for changing the names of files. However, the fallback method is to copy the file to a temporary file (with modifications along the way) and then overwrite the source file from the temporary file... This runs the risk of there being some mishap during the interval when no version of the original file exists, so one could make a copy of the original as well - remembering that file names can't be changed.

As always, there is a problem with the length of a piece of string. The allowance here is 66666 characters. Some filesystems know the length of records, and may make the maximum record length available to enquiry, but others don't know and use instead a marker which might be CR, CRLF, LF or LFCR and even a mixture in the same file. The Fortran programme does not see this with FORMATTED input, just the content of the record, character style. Output, on a windows/DOS system, will always terminate records with CRLF. A null record would be after the first CRLF in the sequence CRLFCRLF, and so on. With UNFORMATTED, the programme must make its own decisions.


```Fortran

      SUBROUTINE CROAK(GASP)	!Something bad has happened.
       CHARACTER*(*) GASP	!As noted.
        WRITE (6,*) "Oh dear. ",GASP	!So, gasp away.
        STOP "++ungood."	!Farewell, cruel world.
      END			!No return from this.

      SUBROUTINE FILEHACK(FNAME,IST,N)
       CHARACTER*(*) FNAME	!Name for the file.
       INTEGER IST		!First record to be omitted.
       INTEGER N		!Number of records to be omitted.
       INTEGER ENUFF,L		!Some lengths.
       PARAMETER (ENUFF = 66666)!Surely?
       CHARACTER*(ENUFF) ALINE	!But not in general...
       INTEGER NREC		!A counter.
       INTEGER F,T		!Mnemonics for file unit numbers.
       PARAMETER (F=66,T=67)	!These should do.
       LOGICAL EXIST
        IF (FNAME.EQ."") CALL CROAK("Blank file name!")
        IF (IST.LE.0)    CALL CROAK("First record must be positive!")
        IF (N.LE.0)      CALL CROAK("Remove count must be positive!")
        INQUIRE(FILE = FNAME, EXIST = EXIST)	!This mishap is frequent, so attend to it.
        IF (.NOT.EXIST) CALL CROAK("Can't find a file called "//FNAME)	!Tough love.
        OPEN (F,FILE=FNAME,STATUS="OLD",ACTION="READ",FORM="FORMATTED")	!Grab the source file.
        OPEN (T,STATUS="SCRATCH",FORM="FORMATTED")	!Request a temporary file.
        NREC = 0		!Number of records read so far.
Copy the desired records to a temporary file.
   10   READ (F,11,END = 20) L,ALINE(1:MIN(L,ENUFF))	!Minimal protection.
   11   FORMAT (Q,A)		!Obviously, Q = # of characters to come, A = their format.
        IF (L.GT.ENUFF) CALL CROAK("Ow! Lengthy record!!")
        NREC = NREC + 1		!If we're here. we've read a record.
        IF (NREC.LT.IST .OR. NREC.GE.IST + N) WRITE (T,12) ALINE(1:L)	!A desired record?
   12   FORMAT (A)		!No character count is explicitly specified.
        GO TO 10		!Keep on thumping.
Convert from input to output...
   20   IF (NREC.LT.IST + N) CALL CROAK("Insufficient records!")	!Finished ignoring records?
        REWIND T		!Not CLOSE! That would discard the file!
        CLOSE(F)		!The source file still exists.
        OPEN (F,FILE=FNAME,FORM="FORMATTED",	!But,
     1   ACTION="WRITE",STATUS="REPLACE")	!This dooms it!
Copy from the temporary file.
   21   READ (T,11,END = 30) L,ALINE(1:L)	!All records are not longer than ALINE.
        WRITE (F,12) ALINE(1:L)			!Out it goes.
        GO TO 21		!Keep on thumping.
Completed.
   30   CLOSE(T)		!Abandon the temporary file.
        CLOSE(F)		!Finished with the source file.
      END		!Done.

      PROGRAM CHOPPER
       CALL FILEHACK("foobar.txt",1,2)
      END

```


When run on file foobar.txt containing
 Unwanted line.
 Nobody wants me either.
 I survive!

The result is file foobar.txt containing
 I survive!

And if run afresh, the file is unharmed and there appears output to the screen:

  Oh dear. Insufficient records!
 ++ungood.


Formulating error messages is tedious, in the absence of a function such as IFMT(n) to be used in <code>CALL CROAK("First record must be positive, not "//IFMT(IST))</code> A more accomplished programme would worry about running out of disc space (signalled by the taking of the END=''label'' option in a WRITE statement) and I/O errors along the way using the ERR=''label'' option, but it is difficult to devise recovery schemes for unexpected errors. Similarly, the OPEN statements are at risk of confronting a file that is available for READ, but not for WRITE. It would help in organising all this if OPEN(...) was a function, but instead one can refer to the recondite IOSTAT error codes, possibly with assistance as with

```Fortran

       CHARACTER*42 FUNCTION ERRORWORDS(IT)	!Look for an explanation. One day, the system may offer coherent messages.
Curious collection of encountered codes. Will they differ on other systems?
Compaq's compiler was taken over by unintel; http://software.intel.com/sites/products/documentation/hpc/compilerpro/en-us/fortran/lin/compiler_f/bldaps_for/common/bldaps_rterrs.htm
contains a schedule of error numbers that matched those I'd found for Compaq, and so some assumptions are added.
Copying all (hundreds!) is excessive; these seem possible for the usage so far made of error diversion.
Compaq's compiler interface ("visual" blah) has a help offering, which can provide error code information.
Compaq messages also appear in http://cens.ioc.ee/local/man/CompaqCompilers/cf/dfuum028.htm#tab_runtime_errors
Combines IOSTAT codes (file open, read etc) with STAT codes (allocate/deallocate) as their numbers are distinct.
Completeness and context remains a problem. Excess brevity means cause and effect can be confused.
        INTEGER IT			!The error code in question.
        INTEGER LASTKNOWN 		!Some codes I know about.
        PARAMETER (LASTKNOWN = 26)	!But only a few, discovered by experiment and mishap.
        TYPE HINT			!For them, I can supply a table.
         INTEGER	CODE		!The code number. (But, different systems..??)
         CHARACTER*42	EXPLICATION	!An explanation. Will it be te answer?
        END TYPE HINT			!Simple enough.
        TYPE(HINT) ERROR(LASTKNOWN)	!So, let's have a collection.
        PARAMETER (ERROR = (/		!With these values.
     1   HINT(-1,"End-of-file at the start of reading!"),	!From examples supplied with the Compaq compiler involving IOSTAT.
     2   HINT( 0,"No worries."),			!Apparently the only standard value.
     3   HINT( 9,"Permissions - read only?"),
     4   HINT(10,"File already exists!"),
     5   HINT(17,"Syntax error in NameList input."),
     6   HINT(18,"Too many values for the recipient."),
     7   HINT(19,"Invalid naming of a variable."),
     8   HINT(24,"Surprise end-of-file during read!"),	!From example source.
     9   HINT(25,"Invalid record number!"),
     o   HINT(29,"File name not found."),
     1   HINT(30,"Unavailable - exclusive use?"),
     2   HINT(32,"Invalid fileunit number!"),
     3   HINT(35,"'Binary' form usage is rejected."),	!From example source.
     4   HINT(36,"Record number for a non-existing record!"),
     5   HINT(37,"No record length has been specified."),
     6   HINT(38,"I/O error during a write!"),
     7   HINT(39,"I/O error during a read!"),
     8   HINT(41,"Insufficient memory available!"),
     9   HINT(43,"Malformed file name."),
     o   HINT(47,"Attempting a write, but read-only is set."),
     1   HINT(66,"Output overflows single record size."),	!This one from experience.
     2   HINT(67,"Input demand exceeds single record size."),	!These two are for unformatted I/O.
     3   HINT(151,"Can't allocate: already allocated!"),	!These different numbers are for memory allocation failures.
     4   HINT(153,"Can't deallocate: not allocated!"),
     5   HINT(173,"The fingered item was not allocated!"),	!Such as an ordinary array that was not allocated.
     6   HINT(179,"Size exceeds addressable memory!")/))
        INTEGER I		!A stepper.
         DO I = LASTKNOWN,1,-1	!So, step through the known codes.
           IF (IT .EQ. ERROR(I).CODE) GO TO 1	!This one?
         END DO			!On to the next.
    1    IF (I.LE.0) THEN	!Fail with I = 0.
           ERRORWORDS = I8FMT(IT)//" is a novel code!"	!Reveal the mysterious number.
          ELSE			!But otherwise, it is found.
           ERRORWORDS = ERROR(I).EXPLICATION	!And these words might even apply.
         END IF			!But on all systems?
       END FUNCTION ERRORWORDS	!Hopefully, helpful.

```


Finally, there is a chance the operating system can be asked to do this, by fragmenting the file in-place into odd-sized pieces. The first piece would be all the records up to the chop, and the second would be all records after resumption. The advantage here is that the rest of the file need no longer be read and written, and files can be large...


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Sub removeLines(fileName As String, startLine As UInteger, numLines As UInteger)
  If startLine = 0 Then
    Print "Starting line must be more than zero"
    Return
  End If
  If numLines = 0 Then
    Print "No lines to remove"
    Return
  End If
  Dim fileNum As Integer = FreeFile
  Open fileName For Input As #fileNum
  If err > 0 Then
    Print "File could not be opened"
    Return
  End If
  Dim tempFileName As String = "temp_" + fileName
  Dim fileNum2 As Integer = FreeFile
  Open tempFileName For Output As #fileNum2
  Dim count As Integer = 0
  Dim ln As String
  Dim endLine As UInteger = startLine + numLines - 1
  While Not Eof(fileNum)
    Input #fileNum, ln
    count += 1
    If count >= startLine AndAlso count <= endLine Then Continue While
    Print #fileNum2, ln
  Wend

  If count < startLine Then
    Print "No lines were removed as starting line was beyond end of file"
    Print
  ElseIf count < endLine Then
    Print "Only "; count - startLine + 1; " line(s) were removed as not enough lines to remove more"
    Print
  Else
    Print Str(numLines); " line(s) were removed"
    Print
  End If
  Close #fileNum : Close #fileNum2
  Kill(fileName)
  Name (tempFileName, fileName)
End Sub

removeLines("foobar.txt", 2, 2)
removeLines("foobar.txt", 5, 2)
removeLines("foobar.txt", 3, 4)
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

2 line(s) were removed

No lines were removed as starting line was beyond end of file

Only 2 line(s) were removed as not enough lines to remove more

```

Contents of foobar.txt before any lines removed:

```txt

foo1
foo2
bar1
bar2
foo3
foo4

```

Contents of foobar.txt after the various removals:

```txt

foo1
bar2

```



## Frink


```frink

removeLines[filename, start, len] :=
{
   lines = array[lines["file:$filename"]]
   modified = lines.removeLen[start-1, len]
   if modified != len
      println["Was only able to remove $modified lines due to end-of-file."]

   w = new Writer[filename]
   for line = lines
      w.println[line]
   w.close[]
}

```



## Gambas


```gambas
sNewFile As String                                                                  'Global string for the 'New file' details

Public Sub Main()
Dim sFileName As String = User.Home &/ "foobar.txt"                                 'File name

sNewFile = DeleteLines(sFileName, 1, 2)                                             'Send the details to the DeleteLine routine 'The parameters should be: foobar.txt, 1, 2'
Print "New file: -" & gb.NewLine & sNewFile                                         'Print details of the changed file
File.Save(sFileName, sNewFile)                                                      'Save the file with the original name

End

Public Sub DeleteLines(sFile As String, siStart As Short, siNum As Short) As String 'DeleteLines routine
Dim sData As New String[]                                                           'To store the existing file data
Dim siCount As Short                                                                'Counter
Dim sTemp, sDel As String                                                           'String variables

For Each sTemp In Split(File.Load(sFile), gb.NewLine)                               'Load the file, split the lines by NewLine
  sData.Add(sTemp)                                                                  'Add to sData
Next

Print "Original file: -"                                                            'Print Title

If siStart + siNum > sData.max Then                                                 'Check an attempt is made to remove lines beyond the end of the file
  Print "Not enough lines in file to carry out request"                             'An appropriate message should appear if so
  Return ""                                                                         'Return nothing
Endif

Dec siStart                                                                         'For the purpose of this task, line numbers start at one (Program starts at 0)

For siCount = siStart To (siStart + siNum) - 1                                      'Loop through the lines to be deleted
  sDel &= Str(siCount) & " "                                                        'Add then to sDel
Next

siCount = -1                                                                        'Reset counter

For Each sTemp In sData                                                             'For each line in the file..
  Inc siCount                                                                       'Increase counter
  Print sTemp                                                                       'Print the line
  If InStr(sDel, Str(siCount) & " ") Then Continue                                  'If the line number is listed in sDel then jump to the end of the loop
  sNewFile &= sTemp & gb.NewLine                                                    'Add the lines not to be removed into sNewFile
Next

Return sNewFile                                                                     'Return the details

End
```

Output:

```txt

Original file: -
abridgment

abroad
abrogate
abrupt
abscess
abscissa
abscissae
absence

New file: -
abroad
abrogate
abrupt
abscess
abscissa
abscissae
absence

```



## Go


```go
package main

import (
    "bytes"
    "errors"
    "fmt"
    "io/ioutil"
    "os"
)

func main() {
    if err := removeLines("foobar.txt", 1, 2); err != nil {
        fmt.Println(err)
    }
}

func removeLines(fn string, start, n int) (err error) {
    if start < 1 {
        return errors.New("invalid request.  line numbers start at 1.")
    }
    if n < 0 {
        return errors.New("invalid request.  negative number to remove.")
    }
    var f *os.File
    if f, err = os.OpenFile(fn, os.O_RDWR, 0); err != nil {
        return
    }
    defer func() {
        if cErr := f.Close(); err == nil {
            err = cErr
        }
    }()
    var b []byte
    if b, err = ioutil.ReadAll(f); err != nil {
        return
    }
    cut, ok := skip(b, start-1)
    if !ok {
        return fmt.Errorf("less than %d lines", start)
    }
    if n == 0 {
        return nil
    }
    tail, ok := skip(cut, n)
    if !ok {
        return fmt.Errorf("less than %d lines after line %d", n, start)
    }
    t := int64(len(b) - len(cut))
    if err = f.Truncate(t); err != nil {
        return
    }
    if len(tail) > 0 {
        _, err = f.WriteAt(tail, t)
    }
    return
}

func skip(b []byte, n int) ([]byte, bool) {
    for ; n > 0; n-- {
        if len(b) == 0 {
            return nil, false
        }
        x := bytes.IndexByte(b, '\n')
        if x < 0 {
            x = len(b)
        } else {
            x++
        }
        b = b[x:]
    }
    return b, true
}
```



## Haskell


```haskell
import System.Environment (getArgs)

main = getArgs >>= (\[a, b, c] ->
            do contents <- fmap lines $ readFile a
               let b1 = read b :: Int
                   c1 = read c :: Int
               putStr $ unlines $ concat [take (b1 - 1) contents, drop c1 $ drop b1 contents]
       )
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main() # remove lines
   removelines("foo.bar",3,2) | stop("Failed to remove lines.")
end

procedure removelines(fn,start,skip)

   f := open(fn,"r") | fail                  # open to read
   every put(F := [],|read(f))               # file to list
   close(f)

   if *F < start-1+skip then fail
   every F[start - 1 + (1 to skip)] := &null # mark delete

   f := open(fn,"w") | fail                  # open to rewrite
   every write(\!F)                          # write non-nulls
   close(f)
   return                                    # done
end
```



## J


```j
removeLines=: 4 :0
  'count start'=. x
  file=. boxxopen y
  lines=. <;.2 fread file
  (;lines {~ <<< (start-1)+i.count) fwrite file
)
```


Thus:

```bash
$ cal >cal.txt
$ cat cal.txt
      July 2011
Su Mo Tu We Th Fr Sa
                1  2
 3  4  5  6  7  8  9
10 11 12 13 14 15 16
17 18 19 20 21 22 23
24 25 26 27 28 29 30
31
```



```j
  2 1 removeLines 'cal.txt'
```



```bash
$ cat cal.txt
                1  2
 3  4  5  6  7  8  9
10 11 12 13 14 15 16
17 18 19 20 21 22 23
24 25 26 27 28 29 30
31
```


Note that this code defines the last character in the file as the line end character.


## Java


```Java

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;

public class RemoveLines
{
	public static void main(String[] args)
	{
		//Enter name of the file here
		String filename="foobar.txt";
		//Enter starting line here
		int startline=1;
		//Enter number of lines here.
		int numlines=2;

		RemoveLines now=new RemoveLines();
		now.delete(filename,startline,numlines);
	}
	void delete(String filename, int startline, int numlines)
	{
		try
		{
			BufferedReader br=new BufferedReader(new FileReader(filename));

			//String buffer to store contents of the file
			StringBuffer sb=new StringBuffer("");

			//Keep track of the line number
			int linenumber=1;
			String line;

			while((line=br.readLine())!=null)
			{
				//Store each valid line in the string buffer
				if(linenumber<startline||linenumber>=startline+numlines)
					sb.append(line+"\n");
				linenumber++;
			}
			if(startline+numlines>linenumber)
				System.out.println("End of file reached.");
			br.close();

			FileWriter fw=new FileWriter(new File(filename));
			//Write entire string buffer into the file
			fw.write(sb.toString());
			fw.close();
		}
		catch (Exception e)
		{
			System.out.println("Something went horribly wrong: "+e.getMessage());
		}
	}
}


```



## jq

In this section, we first present a solution that can be used with jq 1.4; it has the potential disadvantage of requiring that the entire file be read.
The second solution avoids this issue but requires jq 1.5

{{works with|jq|1.4}}

If the following program is in a file named remove.jq,
then to copy lines from a file named INFILE except for NUMBER lines starting with START,
invoke jq as follows:

```sh
jq -s -R -r --arg start START --arg number NUMBER -f remove.jq INFILE
```


For example:

 jq -s -R -r --arg start 1 --arg number 2 -f remove.jq input.txt

```jq
# Counting the first line in the file as line 1,
#  attempt to remove "number" lines from line number "start" onwards:
def remove_lines(start; number):
  (start+number - 1) as $max
  | reduce split("\n")[] as $line
      ( [0, []];
       .[0] += 1
       | .[0] as $i
       | if start <= $i and $i <= $max then . else .[1] += [$line] end)
  | .[0] as $count
  | .[1]
  | join("\n")
  | (if $count < $max then "WARNING: there are only \($count) lines" else empty end), .;

remove_lines($start|tonumber; $number|tonumber)
```


{{works with|jq|1.5}}
jq 1.5 can read textual files in line-by-line mode, as illustrated next.

The command invocation looks like this:

```sh
jq -n -R -r --arg start 1 --arg number 2 -f Remove_lines_from_a_file.jq input.txt
```



```jq
# Counting the first line in the file as line 1, attempt to remove "number" lines from line
# number "start" onwards:
def remove_lines_streaming(start; number):
  (start+number - 1) as $max
  # In the following line, null will serve to signal EOF so that the warning can be emitted.
  | foreach (inputs,null) as $line
      ( 0;
       . += 1;
       if $line == null then # EOF
         if . <= $max then "WARNING: there were only \(.) lines" else empty end
       elif start <= . and . <= $max then empty
       else $line
       end) ;

remove_lines_streaming($start|tonumber; $number|tonumber)
```



## Julia


```Julia
#!/usr/bin/env julia

const prgm = basename(Base.source_path())

if length(ARGS) < 2
    println("usage: ", prgm, " <file> [line]...")
    exit(1)
end

file = ARGS[1]

const numbers = map(x -> begin
    try
        parse(Uint, x)
    catch
        println(prgm, ": ", x, ": not a number")
        exit(1)
    end
end, ARGS[2:end])

f = open(file)
lines = readlines(f)
close(f)

if maximum(numbers) > length(lines)
    println(prgm, ": detected extraneous line number")
    exit(1)
end

deleteat!(lines, sort(unique(numbers)))
f = open(file, "w")
write(f, join(lines))
close(f)
```


Usage:

```txt
usage: removeat.jl <file> [line]...
```


Example:

```txt
./removeat.jl test.txt 5 7
```


test.txt before:

```txt
1
two
3

5        <-- remove me
six
7        <-- remove me

9
ten
11

```


test.txt after:

```txt
1
two
3

six

9
ten
11

```



## Kotlin


```scala
// version 1.1.2

import java.io.File

fun removeLines(fileName: String, startLine: Int, numLines: Int) {
    require(!fileName.isEmpty() && startLine >= 1 && numLines >= 1)
    val f = File(fileName)
    if (!f.exists()) {
        println("$fileName does not exist")
        return
    }
    var lines = f.readLines()
    val size = lines.size
    if (startLine > size) {
        println("The starting line is beyond the length of the file")
        return
    }
    var n = numLines
    if (startLine + numLines - 1 > size) {
        println("Attempting to remove some lines which are beyond the end of the file")
        n = size - startLine + 1
    }
    lines = lines.take(startLine - 1) + lines.drop(startLine + n - 1)
    val text = lines.joinToString(System.lineSeparator())
    f.writeText(text)
}

fun printFile(fileName: String, message: String) {
    require(!fileName.isEmpty())
    val f = File(fileName)
    if (!f.exists()) {
        println("$fileName does not exist")
        return
    }
    println("\nContents of $fileName $message:\n")
    f.forEachLine { println(it) }
}

fun main(args: Array<String>) {
    printFile("input.txt", "before removal")
    removeLines("input.txt", 2, 3)
    printFile("input.txt", "after removal of 3 lines starting from the second")
}
```


{{out}}

```txt

Contents of input.txt before removal:

Line 1
Line 2
Line 3
Line 4
Line 5
Line 6
Line 7
Line 8


Contents of input.txt after removal of 3 lines starting from the second:

Line 1
Line 5
Line 6
Line 7
Line 8

```



## Lasso


```Lasso
#!/usr/bin/lasso9

local(
	orgfilename		= $argv -> second,
	file			= file(#orgfilename),
	regexp			= regexp(-find = `(?m)$`),
	content			= #regexp -> split(-input = #file -> readstring) -> asarray,
	start			= integer($argv -> get(3) || 1),
	range			= integer($argv -> get(4) || 1)
)
stdout(#content)
#file -> copyto(#orgfilename + '.org')

fail_if(#content -> size < (#start + #range), -1, 'Not that many rows in the file')

#content -> remove(#start, #range)

#file = file(#orgfilename)
#file -> opentruncate
#file -> dowithclose => {
	#file -> writestring(#content -> join(''))
}
```

Input:

```txt
1 Here is a row
2 Here is another row
3 This would be yet a row
4 me thinks the count is now four rows
5 Let's make this the last row

```

Call:

```Lasso>./removelines textfile.txt 2 2</lang

Output:

```txt
1 Here is a row
4 me thinks the count is now four rows
5 Let's make this the last row

```



## Liberty BASIC

It's always a bit dangerous to experiment with code that alters files. Un-rem the 'kill' to remove the temp file and the next line so the file is renamed to the original!

```lb

call removeLines "foobar.txt", 1, 2
end

sub removeLines filename$, start, count
    open filename$ for input as #in
    open filename$ + ".tmp" for output as #out
    lineCounter = 1
    firstAfterIgnored = start + count
    while not(eof(#in))
        line input #in, s$
        if lineCounter < start or lineCounter >= firstAfterIgnored then
            print #out, s$
        end if
        lineCounter = lineCounter + 1
    wend
    close #in
    close #out
    'kill filename$
    'name filename$ + ".tmp" as filename$
end sub

```



## Lua


```lua
function remove( filename, starting_line, num_lines )
    local fp = io.open( filename, "r" )
    if fp == nil then return nil end

    content = {}
    i = 1;
    for line in fp:lines() do
        if i < starting_line or i >= starting_line + num_lines then
	    content[#content+1] = line
	end
	i = i + 1
    end

    if i > starting_line and i < starting_line + num_lines then
	print( "Warning: Tried to remove lines after EOF." )
    end

    fp:close()
    fp = io.open( filename, "w+" )

    for i = 1, #content do
	fp:write( string.format( "%s\n", content[i] ) )
    end

    fp:close()
end
```



## Mathematica


```Mathematica
f[doc_String, start_Integer, n_Integer] := Module[{p, newdoc},
  p = Import[doc, "List"];
  If[start + n - 1 <= Length@p,
   p = Drop[p, {start, start + n - 1}];
   newdoc = FileNameJoin[{DirectoryName[doc], FileBaseName[doc] <> "_removed.txt"}];
   Export[newdoc, p, "List"];
   ,
   Print["Too few lines in document. Operation aborted."]
  ]
 ]
```



## NewLISP



```newlisp

(context 'ABC)

(define (remove-lines-from-a-file filename start num)
    (setf new-content "")
    (setf row-counter 0)
    (setf start-delete-row start)
    (setf end-delete-row (+ start num -1))
    (setf file-content (read-file filename))
    (setf max-rows (length (parse file-content "\n" 0)))

    (cond
        ((<= start 0)
            (println "Start line must be >= 1. Value passed: " start))
        ((<= num 0)
            (println "# of lines to remove must be >= 1. Value passed: " num))
        ((> start max-rows)
            (println "Start line must be <= " max-rows ". Value passed: " start))
        ((> end-delete-row max-rows)
            (println "Not so much lines available to be removed. Max " (- max-rows start-delete-row) ". Value passed: " num))
        (true
            (dolist (row (parse file-content "\n" 0))
                (++ row-counter)
                (if (or (< row-counter start-delete-row) (> row-counter end-delete-row))
                    (setf new-content (append new-content row "\n"))
                    )
            )
            (write-file filename new-content)
        )
    )
)

(context 'MAIN)

(ABC:remove-lines-from-a-file "foobar.txt" 8 3)
(exit)

```



## OCaml



```ocaml
let input_line_opt ic =
  try Some (input_line ic)
  with End_of_file -> None

let delete_lines filename start skip =
  if start < 1 || skip < 1 then
    invalid_arg "delete_lines";
  let tmp_file = filename ^ ".tmp" in
  let ic = open_in filename
  and oc = open_out tmp_file in
  let until = start + skip - 1 in
  let rec aux i =
    match input_line_opt ic with
    | Some line ->
        if i < start || i > until
        then (output_string oc line; output_char oc '\n');
        aux (succ i)
    | None ->
        close_in ic;
        close_out oc;
        Sys.remove filename;
        Sys.rename tmp_file filename
  in
  aux 1

let usage () =
  Printf.printf "Usage:\n%s <filename> <startline> <skipnumber>\n" Sys.argv.(0);
  exit 0

let () =
  if Array.length Sys.argv < 4 then usage ();
  delete_lines
    Sys.argv.(1) (int_of_string Sys.argv.(2)) (int_of_string Sys.argv.(3))
```



```txt
$ cal > cal.txt
$ cat cal.txt
    juillet 2011
lu ma me je ve sa di
             1  2  3
 4  5  6  7  8  9 10
11 12 13 14 15 16 17
18 19 20 21 22 23 24
25 26 27 28 29 30 31

$ ocaml deleteLines.ml cal.txt 5 2
$ cat cal.txt
    juillet 2011
lu ma me je ve sa di
             1  2  3
 4  5  6  7  8  9 10
25 26 27 28 29 30 31


```



## Oforth



```oforth
: removeLines(filename, startLine, numLines)
| line b endLine |
   ListBuffer new ->b
   startLine numLines + 1 - ->endLine

   0 File new(filename) forEach: line [
      1+ dup between(startLine, endLine) ifFalse: [ b add(line) continue ]
      numLines 1- ->numLines
      ]
   drop numLines 0 == ifFalse: [ "Error : Removing lines beyond end of file" println return ]

   File new(filename) dup open(File.WRITE) b apply(#[ << dup cr ]) close ;
```



## Pascal


This solution is a bit complicated but the direct use of a TStringList (LoadFromFile, SaveToFile) is not correc ; in fact, it appends CR/LF sometimes, same remark for ReadLn, WriteLn. TFileStream is better. Temp file avoid memory comsumption.

{{works with|Free Pascal|2.6.2}}


```Pascal
program RemLines;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils;

type
  TRLResponse=(rlrOk, rlrEmptyFile, rlrNotEnoughLines);

function RemoveLines(const FileName: String; const From, Count: Integer): TRLResponse;
const
  LineOffs = Length(LineEnding);

var
  TIn, TOut: TFileStream;
  tmpFn, MemBuff, FileBuff: String;
  EndingPos, CharRead, LineNumber: Integer;

  procedure WriteLine(Line: String);
  begin
    if ((From > 1) and (LineNumber = 1)) or ((From = 1) and (LineNumber = (From+Count))) then
      // First line to write, without LineEnding => Line unchanged
    else if ((From = 1) or (From <= LineNumber)) and (LineNumber < (From+Count)) then
      // No line to write
      Line := ''
    else
      // all other cases, write Line preceded (!) by LineEnding
      Line := LineEnding + Line;
    // Write
    if Line <> '' then
      TOut.Write(Line[1], Length(Line));
  End;

begin
  if not FileExists(FileName) then
    raise Exception.CreateFmt('No such file %s', [FileName]);
  if From < 1 then
    raise Exception.Create('First line must be >= 1');

  tmpFn := GetTempFileName(ExtractFilePath(FileName), '');

  TIn := TFileStream.Create(FileName, fmOpenRead);
  try
    TOut := TFileStream.Create(tmpFn, fmCreate);
    try
      FileBuff := StringOfChar(' ', 1024); // Reserve memory in a string
      LineNumber := 0;
      MemBuff := '';
      while True do
      begin
        CharRead := TIn.Read(FileBuff[1], 1024);
        if (CharRead = 0) then
          break; // no more char to process
        MemBuff += Copy(FileBuff, 1, CharRead); // op += is FPC specific
        while True do
        begin
          // LineEnding can contain 1 or 2 chars, depending on the OS
          EndingPos := Pos(LineEnding, MemBuff);
          if EndingPos = 0 then
            break; // EndingLine in the next reading, maybe
          Inc(LineNumber);
          WriteLine(Copy(MemBuff, 1, EndingPos - 1));
          MemBuff := Copy(MemBuff, EndingPos + LineOffs, MaxInt);
          // Loop for another line in MemBuff
        end;
      end;
      Inc(LineNumber);
      WriteLine(MemBuff); // Writes what remains
    finally
      TOut.Free;
    end;
  finally
    TIn.Free;
  end;
  // Temp File replaces the original file.
  if DeleteFile(FileName) then
    RenameFile(tmpFn, FileName)
  else
    raise Exception.Create('Unable to process the file');
  // Response
  if (LineNumber = 0) then
    Result := rlrEmptyFile
  else if (LineNumber < (From+Count-1)) then
    Result := rlrNotEnoughLines
  else
    Result := rlrOk;
End;

var
  FileName: String;

begin
  FileName := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'test.txt';
  try
    case RemoveLines(FileName, 4, 3) of
    rlrOk: WriteLn('Lines deleted');
    rlrEmptyFile: WriteLn(Format('File "%s" is empty!', [FileName]));
    rlrNotEnoughLines: WriteLn('Can''t delete lines past the end of file');
    end
  except
    on E: Exception do
      WriteLn('Error: ' + E.Message);
  end;
  ReadLn;
End.

```


{{out}}

```txt

```



## Perl

The value of deleting certain lines from a file notwithstanding, here's how you'd normally do it in Perl (call with <code>perl rmlines -from=3 -to=10 filename</code>:
```perl
#!/usr/bin/perl -n -s -i
print unless $. >= $from && $. <= $to;
```

If you want a backup file (maybe because deleting lines from a file in place is a pretty silly idea), change the <code>-i</code> in the first line to <code>-i.bak</code>, for example.

```Perl
#!/usr/bin/perl -w
use strict ;
use Tie::File ;

sub deletelines {
   my $arguments = shift ;
   my( $file , $startfrom , $howmany ) = @{$arguments} ;
   tie my @lines , 'Tie::File' , $file or die "Can't find file $file!\n" ;
   my $nrecs = @lines ;
   if ( $startfrom > $nrecs ) {
      print "Error! Starting to delete lines past the end of the file!\n" ;
      return ;
   }
   if ( ( $startfrom + $howmany ) > $nrecs ) {
      print "Error! Trying to delete lines past the end of the file!\n" ;
      return ;
   }
   splice @lines , $startfrom - 1 , $howmany ;
   untie @lines ;
}

if ( @ARGV != 3 ) {
   print "Error! Invoke with deletelines <filename> <start> <skiplines> !\n" ;
   exit( 1 ) ;
}

deletelines( \@ARGV ) ;
```



## Perl 6


```perl6
sub MAIN ($filename, $beg, $len) {
    my @lines = split /^^/, slurp $filename;
    unlink $filename;  # or rename
    splice(@lines,$beg,$len) == $len or warn "Too few lines";
    spurt $filename, @lines;
}
```

{{out}}

```txt
$ cal >foo
$ ./rmlines
 Usage:
   rmlines &lt;filename&gt; &lt;beg&gt; &lt;len&gt;
$ ./rmlines foo 1 2
$ cat foo
  1  2  3  4  5  6  7
  8  9 10 11 12 13 14
 15 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31

```



## Phix


```Phix
procedure remove_lines(string filename, integer start, integer n)
    integer fn = open(filename,'r')
    if fn!=-1 then
        sequence lines = get_text(fn,GT_LF_STRIPPED)
        close(fn)
    end if
    if fn=-1 or n<1 or start<1 or length(lines)<start+n-1 then
        puts(1,"bad parameters!\n")
    else
        lines[start..start+n-1] = {}
        fn = open(filename,'w')
        puts(fn,join(lines,"\n")
        close(fn)
    end if
end procedure
```



## PicoLisp


```PicoLisp
(de deleteLines (File Start Cnt)
   (let L (in File (make (until (eof) (link (line)))))
      (if (> (+ (dec 'Start) Cnt) (length L))
         (quit "Not enough lines")
         (out File
            (mapc prinl (cut Start 'L))
            (mapc prinl (nth L (inc Cnt))) ) ) ) )
```



## PowerBASIC

{{works with|PowerBASIC Console Compiler}}


```powerbasic
#DIM ALL

FUNCTION PBMAIN () AS LONG
    DIM filespec AS STRING, linein AS STRING
    DIM linecount AS LONG, L0 AS LONG, ok AS LONG

    filespec = DIR$(COMMAND$(1))
    WHILE LEN(filespec)
        linecount = 0: ok = 0
        OPEN filespec FOR INPUT AS 1
        OPEN filespec & ".tmp" FOR OUTPUT AS 2
        DO UNTIL EOF(1)
            LINE INPUT #1, linein
            INCR linecount
            IF VAL(COMMAND$(2)) <> linecount THEN
                PRINT #2, linein
            ELSE
                ok = -1
                FOR L0 = 2 TO VAL(COMMAND$(3))
                    IF EOF(1) THEN
                        ok = 1
                        PRINT "Tried to remove beyond the end of "; filespec
                        EXIT DO
                    END IF
                    LINE INPUT #1, linein
                NEXT
            END IF
        LOOP
        IF 0 = ok THEN
            PRINT "Lines to remove are beyond the end of "; filespec
        ELSEIF -1 = ok THEN
            PRINT filespec; ": done"
        END IF
        CLOSE
        filespec = DIR$
    WEND
END FUNCTION
```


Sample output:
 E:\users\Erik\Desktop>rmvlns.exe ?.txt 500 100
 Lines to remove are beyond the end of 0.txt
 Tried to remove beyond the end of 1.txt
 2.txt: done
 3.txt: done
 4.txt: done


## PowerShell

{{works with|PowerShell|4.0}}

```PowerShell

function del-line($file, $start, $end) {
    $i = 0
    $start--
    $end--
    (Get-Content $file) | where{
        ($i -lt $start -or $i -gt $end)
        $i++
    } > $file
    (Get-Content $file)
}
del-line "foobar.txt" 1 2

```



## PureBasic


```PureBasic

; Contents of file 'input.txt' before deletion of lines :
;
; cat
; dog
; giraffe
; lion
; mouse
; pig
; tiger
; zebra

EnableExplicit

#Output$ = "output.txt"; insert path to temporary output file

Procedure RemoveLines(Input$, StartLine, NbLines)
  Protected lineCount = 0
  Protected endline = StartLine + NbLines - 1
  Protected row$

  If Not ReadFile(0, Input$)
    PrintN("Error opening input file")
    ProcedureReturn
  EndIf

  If Not CreateFile(1, #Output$)
    PrintN("Error creating output file")
    CloseFile(0)
    ProcedureReturn
  EndIf

  While Not Eof(0)
    row$ = ReadString(0)
    lineCount + 1
    If lineCount < StartLine Or lineCount > endLine
      WriteStringN(1, row$)
    EndIf
  Wend

  If endLine > lineCount
    PrintN("Attempted to remove lines beyond the end of the file")
    ; but still allow removal of lines (if any) up to the end of the file
  EndIf

  CloseFile(0)
  CloseFile(1)

  If Not DeleteFile(Input$)
    PrintN("Unable to delete input file so output file can be renamed")
    ProcedureReturn
  EndIf

  If Not RenameFile(#Output$, Input$)
    PrintN("Unable to rename output file")
  EndIf

EndProcedure

Define fInput$

If OpenConsole()
  ; delete lines 2,3 amnd 4 of 'input.txt'
  fInput$ = "input.txt"; insert path to input file
  RemoveLines(fInput$, 2, 3)
  PrintN("")
  PrintN("Press any key to close the console")
  Repeat: Delay(10) : Until Inkey() <> ""
  CloseConsole()
EndIf

```


{{out|Output ('input.txt' after removal of lines}}

```txt

cat
mouse
pig
tiger
zebra

```



## Python

Uses the [http://docs.python.org/library/fileinput.html fileinput] standard module.

```python
#!/usr/bin/env python

import fileinput, sys

fname, start, count = sys.argv[1:4]
start, count = int(start), int(count)

for line in fileinput.input(fname, inplace=1, backup='.orig'):
    if start <= fileinput.lineno() < start + count:
        pass
    else:
        print line[:-1]
fileinput.close()
```


{{out}}
There follows a Linux shell session showing the programs use to remove four lines starting from the second, of a file that starts with the digits one to ten on separate lines.
The program keeps the original file with a ,orig prefix.


```txt
paddy@paddy-ThinkPad-T61:~$ seq 1 10 > testfile.txt
paddy@paddy-ThinkPad-T61:~$ ./remove_lines.py testfile.txt 2 4
paddy@paddy-ThinkPad-T61:~$ cat testfile.txt
1
6
7
8
9
10
paddy@paddy-ThinkPad-T61:~$ cat testfile.txt.orig
1
2
3
4
5
6
7
8
9
10
paddy@paddy-ThinkPad-T61:~$
```



## Racket


```Racket

#lang racket
(define (remove-lines file from num)
  (define lines (file->lines file))
  (define-values [pfx rest] (split-at lines (sub1 from)))
  (display-lines-to-file (append pfx (drop rest num)) file #:exists 'replace))

```



## REXX

This example is operating system dependent as this program uses the (DOS)   '''ERASE'''   and   ''' RENAME'''   commands.

This REXX version was tested under two versions Microsoft Windows (in a DOS window).

More error checking could've been added to validate:
::*   a legitimate record line number (and how many lines)
::*   the input file existence
::*   the output file non─existence
::*   authority to read from (and write to) the file (folder)

```rexx
/*REXX program reads and writes a specified file  and  delete(s)  specified record(s).  */
parse arg  iFID   ','   N   ","   many   .       /*input FID, start of delete, how many.*/
if iFID=''  then call er  "no input fileID specified."                           /*oops.*/
if    N=''  then call er  "no start number specified."                           /*oops.*/
if many=''  then many=1                          /*Not specified?  Assume delete 1 line.*/
stop=N+many-1                                    /*calculate  high end  of delete range.*/
oFID=iFID'.$$$'                                  /*temp name (fileID) of the output file*/
#=0                                              /*the count (so far) of records written*/
      do j=1  while  lines(iFID)\==0             /*J  is the record# (line)  being read.*/
      @=linein(iFID)                             /*read a record (line) from input file.*/
      if j>=N & j<=stop  then iterate            /*if it's in the range, then ignore it.*/
      call lineout oFID,@;    #=#+1              /*write record (line);, bump write cnt.*/
      end   /*j*/                                /* [↑]  by ignoring it is to delete it.*/
j=j-1                                            /*adjust J (because of DO loop advance)*/
if j<stop  then say  "The number of lines in file is less than the range given."
$='"'                                            /*handle cases of blanks in the FID(s).*/
'ERASE'   $ || iFID || $                         /*erase the original file.             */
'RENAME'  $ || oFID || $      $ || iFID || $     /*rename "    new      "  to original. */
say 'file '    iFID    " had"    j    'record's(j)", it now has"     #    'record's(w)"."
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
er:   say;      say '***error***';        say;    say arg(1);         say;         exit 13
s:    if arg(1)==1  then return arg(3);   return word(arg(2) 's', 1)       /*pluralizer.*/
```

'''output''' when using the input of:   <tt> foobar.txt,2,4 </tt>

```txt

file  foobar.txt  had 10 records, it now has 6 records.

```

'''file before processing'''

```txt

1
  2
    3
      4
        5
          6
            7
              8
                9
                  ten.

```

'''file after processing'''

```txt

1
          6
            7
              8
                9
                  ten.

```



## Ring


```ring

cStr = read("C:\Ring\bin\filename.txt")
aList = str2list(cStr)
see aList + nl
lineStart = 3
lineCount = 5
num = -1
for n = lineStart to lineStart+lineCount-1
    num += 1
    del(aList,n-num)
next
cStr = list2str(aList)
see cStr + nl
write("C:\Ring\bin\filename.txt",cStr)

```

Input:

```txt

1111111111
2222222222
3333333333
4444444444
5555555555
6666666666
7777777777
8888888888
9999999999

```

Output:

```txt

1111111111
2222222222
8888888888
9999999999

```



```



## Ruby


```ruby
require 'tempfile'

def remove_lines(filename, start, num)
  tmp = Tempfile.open(filename) do |fp|
    File.foreach(filename) do |line|
      if $. >= start and num > 0
        num -= 1
      else
        fp.puts line
      end
    end
    fp
  end
  puts "Warning: End of file encountered before all lines removed" if num > 0
  FileUtils.copy(tmp.path, filename)
  tmp.unlink
end

# Test code
def setup(filename, start, remove)
  puts "remove #{remove} lines starting at line #{start}"
  File.open(filename, "w") {|fh| (1..5).each {|i| fh.puts " "*i + i.to_s}}
  puts "before:", File.read(filename)
end

def teardown(filename)
  puts "after:", File.read(filename)
  puts
  File.unlink(filename)
end

filename = "foobar.txt"
start = 2
[2, 6].each do |remove|
  setup(filename, start, remove)
  remove_lines(filename, start, remove)
  teardown(filename)
end
```


{{out}}

```txt

remove 2 lines starting at line 2
before:
 1
  2
   3
    4
     5
after:
 1
    4
     5

remove 6 lines starting at line 2
before:
 1
  2
   3
    4
     5
Warning: End of file encountered before all lines removed
after:
 1

```



## Run BASIC


```runbasic
fileName$ = "aFile.txt"
startLine = 100
lineCount = 10

open filename$          for input  as #in
open filename$ ; "_tmp" for output as #out

while not(eof(#in))
   lineNum = lineNum + 1
   line input #in, a$
   if lineNum < startLine or lineNum >= startLine + lineCount then print #out, a$
wend
close #in
close #out
```



## Rust


```rust
extern crate rustc_serialize;
extern crate docopt;

use docopt::Docopt;

use std::io::{BufReader,BufRead};
use std::fs::File;

const USAGE: &'static str = "
Usage: rosetta <start> <count> <file>
";

#[derive(Debug, RustcDecodable)]
struct Args {
    arg_start: usize,
    arg_count: usize,
    arg_file: String,
}

fn main() {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());

    let file = BufReader::new(File::open(args.arg_file).unwrap());

    for (i, line) in file.lines().enumerate() {
        let cur = i + 1;

        if cur < args.arg_start || cur >= (args.arg_start + args.arg_count) {
            println!("{}", line.unwrap());
        }
    }
}
```



## Scala


```Scala
object RemoveLinesFromAFile extends App {
  args match {
    case Array(filename, start, num) =>

      import java.nio.file.{Files,Paths}
      val lines = scala.io.Source.fromFile(filename).getLines
      val keep = start.toInt - 1
      val top = lines.take(keep).toList
      val drop = lines.take(num.toInt).toList
      Files.write(Paths.get(filename), scala.collection.JavaConversions.asJavaIterable(top ++ lines))

      if (top.size < keep || drop.size < num.toInt)
        println(s"Too few lines: removed only ${drop.size} of $num lines starting at $start")

    case _ =>
      println("Usage: RemoveLinesFromAFile <filename> <startLine> <numLines>")
  }
}
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "osfiles.s7i";

const proc: removeLines (in string: fileName, in integer: start, in integer: count) is func
  local
    var file: inFile is STD_NULL;
    var file: outFile is STD_NULL;
    var integer: lineNumber is 1;
    var string: line is "";
  begin
    inFile := open(fileName, "r");
    outFile := open(fileName & ".tmp", "w");
    while hasNext(inFile) do
      line := getln(inFile);
      if lineNumber < start or lineNumber >= start + count then
        writeln(outFile, line);
      end if;
      incr(lineNumber);
    end while;
    close(inFile);
    close(outFile);
    removeFile(fileName);
    moveFile(fileName & ".tmp", fileName);
  end func;

const proc: main is func
  begin
    if length(argv(PROGRAM)) = 3 then
      removeLines(argv(PROGRAM)[1], integer parse (argv(PROGRAM)[2]), integer parse (argv(PROGRAM)[3]));
    end if;
  end func;
```



## Sidef


```ruby
func remove_lines(file, beg, len) {
    var lines = file.open_r.lines;
    lines.splice(beg, len).len == len || warn "Too few lines";
    file.open_w.print(lines.join)
}

remove_lines(File(__FILE__), 2, 3);
```



## Stata

The usual way to solve this task in Stata consists in removing lines from the currently loaded dataset. This is done with the '''[http://www.stata.com/help.cgi?drop drop]''' command, or alternately with '''keep'''.


```stata
* drop lines 20 to 30
drop in 20/30

* keep lines 20 to 30, and remove everything else
keep in 20/30
```



## Tcl


```tcl
proc removeLines {fileName startLine count} {
    # Work out range to remove
    set from [expr {$startLine - 1}]
    set to [expr {$startLine + $count - 2}]
    # Read the lines
    set f [open $fileName]
    set lines [split [read $f] "\n"]
    close $f
    # Write the lines back out, without removed range
    set f [open $fileName w]
    puts -nonewline $f [join [lreplace $lines $from $to] "\n"]
    close $f
}
```



## TUSCRIPT


```tuscript

$$! input=testfile,begnr=3,endnr=4
$$ MODE TUSCRIPT
- CREATE inputfile
ERROR/STOP CREATE (input,FDF-o,-std-)
FILE/ERASE $input
 LOOP n=1,9
 content=CONCAT ("line",n)
 DATA {content}
 ENDLOOP
ENDFILE

- CREATE outputfile
output="outputfile"

ERROR/STOP CREATE (output,fdf-o,-std-)
ACCESS q:  READ/RECORDS/utf8 $input  L,line
ACCESS z: WRITE/RECORDS/utf8 $output L,line
PRINT "content: of output-file"
LOOP/9999
READ/NEXT/EXIT q
IF (begnr<=L&&endnr>=L) CYCLE
PRINT line
WRITE z
ENDLOOP
ENDACCESS/PRINT q
ENDACCESS/PRINT z

```

Output:
<pre style='height:30ex;overflow:scroll'>
Start MAKRO   auf: XXXXXX   am: 02.12.11  um: 17:31:50
content: of output-file
line1
line2
line5
line6
line7
line8
line9
****  Eingabe: 9 Sätze von Datei -*TESTFILE
               Satzlänge: 5
****  Ausgabe: 7 Sätze auf Datei -*OUTPUTFILE
               Satzlänge: 5
Ende  MAKRO   auf: XXXXXX   am: 02.12.11  um: 17:31:50

```



## UNIX Shell


```bash
#!/bin/sh
error() {
  echo >&2 "$0: $*"
  exit 1
}

[ $# -ne 3 ] && error "Incorrect number of parameters"

file=$1
start=$2
count=$3
end=`expr $start + $count - 1`

[ -f "$file" ] || error "$file does not exist"

sed "$start,${end}d" "$file" >/tmp/$$ && mv /tmp/$$ "$file"

```


If you have a modern sed on the system, you can use it to do an in-place edit. GNU sed:


```bash

sed -i "$start,${end}d" "$file"

```


BSD/MacOS sed:


```bash

sed -i "" "$start,${end}d" "$file"

```



## VBA

First way : read the text file line by line

```vb
Option Explicit

Sub Main()
   'See Output #1
   RemoveLines "C:\Users\" & Environ("username") & "\Desktop\foobar.txt", 11, 5
   'See Output #2
   RemoveLines "C:\Users\" & Environ("username") & "\Desktop\foobar.txt", 8, 5
   'See Output #3
   RemoveLines "C:\Users\" & Environ("username") & "\Desktop\foobar.txt", 3, 5
End Sub

Private Sub RemoveLines(StrFile As String, StartLine As Long, NumberOfLines As Long)
Dim Nb As Integer, s As String, count As Long, out As String
   Nb = FreeFile
   Open StrFile For Input As #Nb
      While Not EOF(Nb)
         count = count + 1
         Line Input #Nb, s
         If count < StartLine Or count >= StartLine + NumberOfLines Then
            out = out & s & vbCrLf
         End If
      Wend
   Close #Nb
   If StartLine >= count Then
      MsgBox "The file contains only " & count & " lines"
   ElseIf StartLine + NumberOfLines > count Then
      MsgBox "You only can remove " & count - StartLine & " lines"
   Else
      Nb = FreeFile
      Open StrFile For Output As #Nb
         Print #Nb, out
      Close #Nb
   End If
End Sub

```


{{in}}

```txt
1
2
3
4
5
6
7
8
9
10
```


{{out}}Output 1

```txt
The file contains only 10 lines
```

{{out}}
Output 2

```txt
You only can remove 2 lines
```

{{out}}
Output 3

```txt
1
2
8
9
10
```


Second Way : Read the text file at once and work with Arrays

```vb
Option Explicit

Sub Main()
   'See Output First call
   OtherWay "C:\Users\" & Environ("username") & "\Desktop\foobar.txt", 11, 5
   'See Output Second call
   OtherWay "C:\Users\" & Environ("username") & "\Desktop\foobar.txt", 8, 5
   'See Output Third call
   OtherWay "C:\Users\" & Environ("username") & "\Desktop\foobar.txt", 3, 5
End Sub

Private Sub OtherWay(StrFile As String, StartLine As Long, NumberOfLines As Long)
Dim Nb As Integer, s As String, arr, i As Long, out() As String, j As Long
   Nb = FreeFile
   Open StrFile For Input As #Nb
      s = Input(LOF(1), #Nb)
   Close #Nb
   arr = Split(s, Chr(13))
   If StartLine >= UBound(arr) + 1 Then
      MsgBox "First call : " & vbCrLf & "    The file contains only " & UBound(arr) + 1 & " lines"
   ElseIf StartLine + NumberOfLines > UBound(arr) + 1 Then
      MsgBox "Second call : " & vbCrLf & "    You only can remove " & UBound(arr) + 1 - StartLine & " lines"
   Else
      For i = LBound(arr) To UBound(arr)
         If i < StartLine - 1 Or i >= StartLine + NumberOfLines - 1 Then
            ReDim Preserve out(j)
            out(j) = arr(i)
            j = j + 1
         End If
      Next i
      Nb = FreeFile
      Open StrFile For Output As #Nb
         Print #Nb, Join(out, Chr(13))
      Close #Nb
   End If
End Sub
```


{{in}}

```txt
1
2
3
4
5
6
7
8
9
10
```


{{out}}

```txt
First call :
    The file contains only 10 lines
Second call :
    You only can remove 2 lines
Third call :
1
2
8
9
10

```



## VBScript


```vb

Sub remove_lines(filepath,start,number)
	Set objFSO = CreateObject("Scripting.FileSystemObject")
	Set InFile = objFSO.OpenTextFile(filepath,1,False)
	line_count = 1
	discard_count = 1
	out_txt = ""
	Do Until InFile.AtEndOfStream
		line = InFile.ReadLine
		If line_count <> start Then
			If InFile.AtEndOfStream = False Then
				out_txt = out_txt & line & vbCrLf
			Else
				out_txt = out_txt & line
			End If
			line_count = line_count + 1
		Else
			Do Until discard_count = number
				InFile.SkipLine
				discard_count = discard_count + 1
			Loop
		line_count = line_count + 1
		End If
	Loop
	InFile.Close
	Set OutFile = objFSO.OpenTextFile(filepath,2,False)
	OutFile.Write(out_txt)
	OutFile.Close
	Set objFSO = Nothing
End Sub

Call remove_lines("C:\Test.txt",3,4)

```


{{in}}

```txt

1
2
3
4
5
6
7
8
9
10
11
12

```


{{out}}

```txt

1
2
7
8
9
10
11
12

```



## zkl

Two solutions, one in memory and the other read the file line by line and hope the OS buffers. Write to stdout to avoid "what to use as dst" questions.

```zkl
fcn deleteLinesM(fname, start,num){
    blob:=File(fname).read();     // file to memory
    n:=blob.seek(Void,start-1);   // seek to line and remember it
    blob.del(n,blob.seek(Void,num)-n);

    File.stdout.write(blob);
}
deleteLinesM("nn.zkl", 2,5);
```

{{out}}

```txt

fcn deleteLinesM(fname, start,num){
}
deleteLinesM("nn.zkl", 2,5);

```

Line by line:

```zkl
fcn deleteLinesL(fname, start,num){
    if (start < 1) throw(Exception.ValueError);
    f:=File(fname);
    do(start-1) { File.stdout.write(f.readln()) }
    do(num)     { f.readln(); }
    f.pump(File.stdout.write);
}
deleteLinesL("nn.zkl", 2,5);
```

{{out}}

```txt

fcn deleteLinesL(fname, start,num){
}
deleteLinesL("nn.zkl", 2,5);

```



{{omit from|GUISS}}
