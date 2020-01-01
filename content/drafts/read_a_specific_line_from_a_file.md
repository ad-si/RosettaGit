+++
title = "Read a specific line from a file"
description = ""
date = 2019-07-30T15:10:43Z
aliases = []
[extra]
id = 10087
[taxonomies]
categories = []
tags = []
+++

{{task}}
Some languages have special semantics for obtaining a known line number from a file.


;Task:
Demonstrate how to obtain the contents of a specific line within a file.

For the purpose of this task demonstrate how the contents of the seventh line of a file can be obtained, and store it in a variable or in memory (for potential future use within the program if the code were to become embedded). If the file does not contain seven lines, or the seventh line is empty, or too big to be retrieved, output an appropriate message. If no special semantics are available for obtaining the required line, it is permissible to read line by line. Note that empty lines are considered and should still be counted. Note that for functional languages or languages without variables or storage, it is permissible to output the extracted data to standard output.





## Ada


```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Rosetta_Read is
   File : File_Type;
begin
   Open (File => File,
         Mode => In_File,
         Name => "rosetta_read.adb");
   Set_Line (File, To => 7);

   declare
      Line_7 : constant String := Get_Line (File);
   begin
      if Line_7'Length = 0 then
         Put_Line ("Line 7 is empty.");
      else
         Put_Line (Line_7);
      end if;
   end;

   Close (File);
exception
   when End_Error =>
      Put_Line ("The file contains fewer than 7 lines.");
      Close (File);
   when Storage_Error =>
      Put_Line ("Line 7 is too long to load.");
      Close (File);
end Rosetta_Read;
```



## Aime


```aime
void
read_line(text &line, text path, integer n)
{
    file f;

    f.affix(path);

    call_n(n, f_slip, f);

    f.line(line);
}


integer
main(void)
{
    if (1 < argc()) {
        text line;

        read_line(line, argv(1), 6);

        o_("7th line is:\n", line, "\n");
    }

    0;
}
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.win32}}

```algol68
# reads the line with number "number" (counting from 1)       #
# from the file named "file name" and returns the text of the #
# in "line". If an error occurs, the result is FALSE and a    #
# message is returned in "err". If no error occurs, TRUE is   #
# returned                                                    #
PROC read specific line = ( STRING     file name
                          , INT        number        # line 7 #
                          , REF STRING line
                          , REF STRING err
                          )BOOL:
BEGIN

    FILE    input file;

    line := "";
    err  := "";

    IF  open( input file, file name, stand in channel ) /= 0
    THEN
        # failed to open the file #
        err := "Unable to open """ + file name + """";
        FALSE

    ELSE
        # file opened OK #

        BOOL at eof := FALSE;

        # set the EOF handler for the file #
        on logical file end( input file
                           , ( REF FILE f )BOOL:
                             BEGIN
                                 # note that we reached EOF on the #
                                 # latest read #
                                 at eof := TRUE;

                                 # return TRUE so processing can continue #
                                 TRUE
                             END
                           );

         INT    line number := 0;
         STRING text;

         WHILE line number < number
           AND NOT at eof
         DO

             get( input file, ( text, newline ) );
             line number +:= 1

         OD;

         # close the file #
         close( input file );

         # return the line or an error message depending on whether #
         # we got a line with the required number or not            #
         IF line number = number
         THEN
             # got the required line #
             line := text;
             TRUE
         ELSE
             # not enough lines in the file #
             err  := """" + file name + """ is too short";
             FALSE
         FI

    FI

END; # read specific line #


main:(
    # read the seventh line of this source and print it #
    # (or an error message if we can't)                 #

    STRING line;
    STRING err;

    IF read specific line( "read-specific-line.a68", 7, line, err )
    THEN
        # got the line #
        print( ( "line seven is: """ + line + """", newline ) )
    ELSE
        # got an error #
        print( ( "unable to read line: """ + err + """" ) )
    FI

)
```

{{out}}

```txt

line seven is: "                          , INT        number        # line 7 #"

```



## AutoHotkey


```AutoHotkey
FileReadLine, OutputVar, filename.txt, 7
if ErrorLevel
   MsgBox, There was an error reading the 7th line of the file
```



## AWK


```awk
#!/usr/bin/awk -f
#usage: readnthline.awk -v lineno=6 filename
FNR==lineno { storedline=$0; found++ }
END {if(found<1){print "ERROR: Line",lineno,"not found"}
```



## Batch File


```dos

@echo off

for /f "skip=6 tokens=*" %%i in (file.txt) do (
  set line7=%%i
  goto break
)
:break
echo Line 7 is: %line7%
pause>nul

```

{{in}}

```txt

This is line 1.
This is line 2.
This is line 3. This line has special characters !@#$%^&*()
This is line 4. The next line is blank

     This is line 6. This line has trailing spaces.
This is line 7
This is line 8
etc...

```

{{out}}

```txt

Line 7 is: This is line 7

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      filepath$ = @lib$ + "..\licence.txt"
      requiredline% = 7

      file% = OPENIN(filepath$)
      IF file%=0 ERROR 100, "File could not be opened"
      FOR i% = 1 TO requiredline%
        IF EOF#file% ERROR 100, "File contains too few lines"
        INPUT #file%, text$
      NEXT
      CLOSE #file%

      IF ASCtext$=10 text$ = MID$(text$,2)
      PRINT text$
```



## C

Mmap file and search for offsets to certain line number.  Since mapped file really is memory, there's no extra storage procedure once offsets are found.

```c
#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <err.h>

/* following code assumes all file operations succeed. In practice,
 * return codes from open, close, fstat, mmap, munmap all need to be
 * checked for error.
*/
int read_file_line(const char *path, int line_no)
{
	struct stat s;
	char *buf;
	off_t start = -1, end = -1;
	size_t i;
	int ln, fd, ret = 1;

	if (line_no == 1) start = 0;
	else if (line_no < 1){
		warn("line_no too small");
		return 0; /* line_no starts at 1; less is error */
	}

	line_no--; /* back to zero based, easier */

	fd = open(path, O_RDONLY);
	fstat(fd, &s);

	/* Map the whole file.  If the file is huge (up to GBs), OS will swap
	 * pages in and out, and because search for lines goes sequentially
	 * and never accesses more than one page at a time, penalty is low.
	 * If the file is HUGE, such that OS can't find an address space to map
	 * it, we got a real problem.  In practice one would repeatedly map small
	 * chunks, say 1MB at a time, and find the offsets of the line along the
	 * way.  Although, if file is really so huge, the line itself can't be
	 * guaranteed small enough to be "stored in memory", so there.
	 */
	buf = mmap(0, s.st_size, PROT_READ, MAP_PRIVATE, fd, 0);

	/* optional; if the file is large, tell OS to read ahead */
	madvise(buf, s.st_size, MADV_SEQUENTIAL);

	for (i = ln = 0; i < s.st_size && ln <= line_no; i++) {
		if (buf[i] != '\n') continue;

		if (++ln == line_no) start = i + 1;
		else if (ln == line_no + 1) end = i + 1;
	}

	if (start >= s.st_size || start < 0) {
		warn("file does not have line %d", line_no + 1);
		ret = 0;
	} else {
		/* 	do something with the line here, like
		write(STDOUT_FILENO, buf + start, end - start);
			or copy it out, or something
		*/
	}

	munmap(buf, s.st_size);
	close(fd);

	return ret;
}
```



### Alternate Version


This version does not rely on POSIX APIs such as <tt>mmap</tt>, but rather sticks to ANSI C functionality.  This version also works with non-seekable files, so it can be fed by a pipe.  It performs limited but adequate error checking.  That is, <tt>get_nth_line</tt> returns NULL on all failures, and the caller can distinguish EOF, file read error and out of memory by calling <tt>feof()</tt> and <tt>ferror()</tt> on the input file.


```c
#include <stdio.h>
#include <stdlib.h>

#define BUF_SIZE ( 256 )

char *get_nth_line( FILE *f, int line_no )
{
    char   buf[ BUF_SIZE ];
    size_t curr_alloc = BUF_SIZE, curr_ofs = 0;
    char   *line      = malloc( BUF_SIZE );
    int    in_line    = line_no == 1;
    size_t bytes_read;

    /* Illegal to ask for a line before the first one. */
    if ( line_no < 1 )
        return NULL;

    /* Handle out-of-memory by returning NULL */
    if ( !line )
        return NULL;

    /* Scan the file looking for newlines */
    while ( line_no &&
            ( bytes_read = fread( buf, 1, BUF_SIZE, f ) ) > 0 )
    {
        int i;

        for ( i = 0 ; i < bytes_read ; i++ )
        {
            if ( in_line )
            {
                if ( curr_ofs >= curr_alloc )
                {
                    curr_alloc <<= 1;
                    line = realloc( line, curr_alloc );

                    if ( !line )    /* out of memory? */
                        return NULL;
                }
                line[ curr_ofs++ ] = buf[i];
            }

            if ( buf[i] == '\n' )
            {
                line_no--;

                if ( line_no == 1 )
                    in_line = 1;

                if ( line_no == 0 )
                    break;
            }
        }
    }

    /* Didn't find the line? */
    if ( line_no != 0 )
    {
        free( line );
        return NULL;
    }

    /* Resize allocated buffer to what's exactly needed by the string
       and the terminating NUL character.  Note that this code *keeps*
       the terminating newline as part of the string.
     */
    line = realloc( line, curr_ofs + 1 );

    if ( !line ) /* out of memory? */
        return NULL;

    /* Add the terminating NUL. */
    line[ curr_ofs ] = '\0';

    /* Return the line.  Caller is responsible for freeing it. */
    return line;
}


/* Test program.  Prints out the 7th line of input from stdin, if any */
int main( int argc, char *argv[] )
{
    char *line7 = get_nth_line( stdin, 7 );

    if ( line7 )
    {
        printf("The 7th line of input was:\n%s\n", line7 );
        free( line7 );
    } else
    {
        printf("Did not find the 7th line of input.  Reason:  ");
        if ( feof( stdin ) )
            puts("End of file reached.");
        else if ( ferror( stdin ) )
            puts("Error reading input.");
        else
            puts("Out of memory.");
    }

    return 0;
}

```



## C++


```cpp
#include <string>
#include <fstream>
#include <iostream>

int main( ) {
   std::cout << "Which file do you want to look at ?\n" ;
   std::string input ;
   std::getline( std::cin , input ) ;
   std::ifstream infile( input.c_str( ) , std::ios::in ) ;
   std::string file( input ) ;
   std::cout << "Which file line do you want to see ? ( Give a number > 0 ) ?\n" ;
   std::getline( std::cin , input ) ;
   int linenumber = std::stoi( input ) ;
   int lines_read = 0 ;
   std::string line ;
   if ( infile.is_open( ) ) {
      while ( infile ) {
	 getline( infile , line ) ;
	 lines_read++ ;
	 if ( lines_read == linenumber ) {
	    std::cout << line << std::endl ;
	    break ;
	 }
      }
      infile.close( ) ;
      if ( lines_read < linenumber )
	 std::cout << "No " << linenumber << " lines in " << file << " !\n" ;
      return 0 ;
   }
   else {
      std::cerr << "Could not find file " << file << " !\n" ;
      return 1 ;
   }
}
```



## Clojure


```clojure
(defn read-nth-line
  "Read line-number from the given text file. The first line has the number 1."
  [file line-number]
  (with-open [rdr (clojure.java.io/reader file)]
    (nth (line-seq rdr) (dec line-number))))
```


{{out}}

```txt
user=> (read-nth-line "/tmp/test.txt" 7)
"foo"
```



## Common Lisp



```lisp
(defun read-nth-line (file n &aux (line-number 0))
  "Read the nth line from a text file. The first line has the number 1"
  (assert (> n 0) (n))
  (with-open-file (stream file)
    (loop for line = (read-line stream nil nil)
          if (and (null line) (< line-number n))
            do (error "file ~a is too short, just ~a, not ~a lines long"
                      file line-number n)
          do (incf line-number)
          if (and line (= line-number n))
            do (return line))))

```


Example call:
 CL-USER> (read-nth-line "/tmp/test1.text" 7)
 "foo"


## C#



```C sharp
using System;
using System.IO;

namespace GetLine
{
    internal class Program
    {
        private static void Main(string[] args)
        {
            Console.WriteLine(GetLine(args[0], uint.Parse(args[1])));
        }

        private static string GetLine(string path, uint line)
        {
            using (var reader = new StreamReader(path))
            {
                try
                {
                    for (uint i = 0; i <= line; i++)
                    {
                        if (reader.EndOfStream)
                            return string.Format("There {1} less than {0} line{2} in the file.", line,
                                                 ((line == 1) ? "is" : "are"), ((line == 1) ? "" : "s"));

                        if (i == line)
                            return reader.ReadLine();

                        reader.ReadLine();
                    }
                }
                catch (IOException ex)
                {
                    return ex.Message;
                }
                catch (OutOfMemoryException ex)
                {
                    return ex.Message;
                }
            }

            throw new Exception("Something bad happened.");
        }
    }
}
```



## D

simply

```d

void main() {
    import std.stdio, std.file, std.string;
    auto file_lines = readText("input.txt").splitLines();
    //file_lines becomes an array of strings, each line is one element
    writeln((file_lines.length > 6) ? file_lines[6] : "line not found");
}

```


or, line by line

```d
import std.stdio;

void main() {
    int countLines;
    char[] ln;
    auto f = File("linenumber.d", "r");
    foreach (char[] line; f.byLine()) {
        countLines++;
        if (countLines == 7) {
            ln = line;
            break;
        }
    }
    switch(countLines) {
        case 0 : writeln("the file has zero length");
                break;
        case 7 : writeln("line 7: ", (ln.length ? ln : "empty"));
                break;
        default :
                writefln("the file only contains %d lines", countLines);
    }
}
```


```txt

line 7:     foreach (char[] line; f.byLine()) {
```



## Erlang

Using function into_list/1 from [[Read_a_file_line_by_line]].
There is no behaviour specified after printing an error message, so I throw an exception. An alternative would be to continue with a default value?

```Erlang

-module( read_a_specific_line ).

-export( [from_file/2, task/0] ).

from_file( File, N ) -> line_nr( N, read_a_file_line_by_line:into_list(File) ).

task() ->
       Lines = read_a_file_line_by_line:into_list( "read_a_specific_line.erl" ),
       Line_7 = line_nr( 7, Lines ),
       Line_7.



line_nr( N, Lines ) ->
         try
         case lists:nth( N, Lines )
         of "\n" -> erlang:exit( empty_line )
         ; Line -> Line
         end

         catch
         _Type:Error0 ->
                Error = line_nr_error( Error0 ),
                io:fwrite( "Error: ~p~n", [Error] ),
                erlang:exit( Error )

         end.

line_nr_error( function_clause ) -> too_few_lines_in_file;
line_nr_error( Error ) -> Error.

```


{{out}}

```txt

27> read_a_specific_line:task().
"task() ->\n"
28> read_a_specific_line:from_file("read_a_specific_line.erl", 6).
Error: empty_line
** exception exit: empty_line
     in function  read_a_specific_line:line_nr/2 (read_a_specific_line.erl, line 25)
29> read_a_specific_line:from_file("read_a_specific_line.erl", 66).
Error: too_few_lines_in_file
** exception exit: too_few_lines_in_file
     in function  read_a_specific_line:line_nr/2 (read_a_specific_line.erl, line 25)

```



## Factor


```factor
USING: continuations fry io io.encodings.utf8 io.files kernel
math ;
IN: rosetta-code.nth-line

: nth-line ( path encoding n -- str/f )
    [ f ] 3dip '[
        [ _ [ drop readln [ return ] unless* ] times ]
        with-return
    ] with-file-reader ;

: nth-line-demo ( -- )
    "input.txt" utf8 7 nth-line [ "line not found" ] unless*
    print ;

MAIN: nth-line-demo
```



## Fortran

A lot of petty annoyances can arise in the attempt to complete the desired action, and so the function does not simply return ''true'' or ''false'', nor does it return some drab integer code that would require an auxiliary array of explanatory texts somewhere... Instead, it returns a message reporting on its opinion, with an ad-hoc scheme. If the first character is a space, all is well, otherwise a ! indicates some severe problem while a + indicates a partial difficulty. The text of the desired record is returned via a parameter, thus the caller can be the one responsible for deciding how much space to provide for it. F2000 has provision for allocating character strings of the needed length, but there is no attempt to use that here as the key requirement is for the length to be decided during the process of the READ statement.

The example uses F90 only because the MODULE protocol enables usage of a function without having to re-declare its type in every calling routine. Otherwise this is F77 style. Some compilers become confused or raise an error over the manipulation of a function's name as if it were an ordinary variable. In such a case an auxiliary variable can be used with its value assigned to the function name on exit.
```Fortran
      MODULE SAMPLER    !To sample a record from a file.                SAM00100
       CONTAINS                                                         SAM00200
        CHARACTER*20 FUNCTION GETREC(N,F,IS)    !Returns a status.      SAM00300
Careful. Some compilers get confused over the function name's usage.    SAM00400
         INTEGER N              !The desired record number.             SAM00500
         INTEGER F              !Of this file.                          SAM00600
         CHARACTER*(*) IS       !Stashed here.                          SAM00700
         INTEGER I,L            !Assistants.                            SAM00800
          IS = ""               !Clear previous content, even if null...SAM00900
          IF (N.LE.0) THEN      !Start on errors.                       SAM01000
            WRITE (GETREC,1) "!No record",N     !Could never be found.  SAM01100
    1       FORMAT (A,1X,I0)                    !Message, number.       SAM01200
          ELSE IF (F.LE.0) THEN                 !Obviously wrong?       SAM01300
            WRITE (GETREC,1) "!No unit number",F!Positive is valid.     SAM01400
          ELSE IF (LEN(IS).LE.0) THEN           !Space awaits?          SAM01500
            WRITE (GETREC,1) "!String size",LEN(IS)     !Nope.          SAM01600
          ELSE                  !Otherwise, there is hope.              SAM01700
            REWIND (F)          !Clarify the file position.             SAM01800
            DO I = 1,N - 1      !Grind up to the desired record.        SAM01900
              READ (F,2,END=3)  !Ignoring any content.                  SAM02000
            END DO              !Are we there yet?                      SAM02100
            READ (F,2,END = 3) L,IS(1:MIN(L,LEN(IS)))    !At last.      SAM02200
    2       FORMAT (Q,A)        !Q = characters yet unread.             SAM02300
            IF (L.LT.LEN(IS)) IS(L + 1:) = ""   !Clear the tail.        SAM02400
            IF (L.GT.LEN(IS)) THEN              !Now for more silliness.SAM02500
              WRITE (GETREC,1) "+Length",L      !Too long to fit in IS. SAM02600
            ELSE IF (L.LE.0) THEN               !A zero-length record   SAM02700
              WRITE (GETREC,1) "+Null"          !Is not the same        SAM02800
            ELSE IF (IS.EQ."") THEN             !As a record            SAM02900
              WRITE (GETREC,1) "+Blank",L       !Containing spaces.     SAM03000
            ELSE                                !But otherwise,         SAM03100
              WRITE (GETREC,1) " Length",L      !Note the leading space.SAM03200
            END IF              !Righto, we've decided.                 SAM03300
          END IF                !And, no more options.                  SAM03400
          RETURN                !So, done.                              SAM03500
    3     WRITE (GETREC,1) "!End on read",I     !An alternative ending. SAM03600
        END FUNCTION GETREC     !That was interesting.                  SAM03700
      END MODULE SAMPLER        !Just a sample of possibility.          SAM03800
                                                                        SAM03900
      PROGRAM POKE                                                      POK00100
      USE SAMPLER                                                       POK00200
      INTEGER ENUFF     !Some sizes.                                    POK00300
      PARAMETER (ENUFF = 666)   !Sufficient?                            POK00400
      CHARACTER*(ENUFF) STUFF   !Lots of memory these days.             POK00500
      CHARACTER*20 RESULT                                               POK00600
      INTEGER MSG,F     !I/O unit numbers.                              POK00700
      MSG = 6           !Standard output.                               POK00800
      F = 10            !Chooose a unit number.                         POK00900
      WRITE (MSG,*) "      To select record 7 from a disc file."        POK01000
                                                                        POK01100
      WRITE (MSG,*) "As a FORMATTED file."                              POK01200
      OPEN (F,FILE="FileSlurpN.for",STATUS="OLD",ACTION="READ")         POK01300
      RESULT = GETREC(7,F,STUFF)                                        POK01400
      WRITE (MSG,1) "Result",RESULT                                     POK01500
      WRITE (MSG,1) "Record",STUFF                                      POK01600
    1 FORMAT (A,":",A)                                                  POK01700
                                                                        POK01800
      CLOSE (F)                                                         POK01900
      WRITE (MSG,*) "As a random-access unformatted file."              POK02000
      OPEN (F,FILE="FileSlurpN.for",STATUS="OLD",ACTION="READ",         POK02100
     1 ACCESS="DIRECT",FORM="UNFORMATTED",RECL=82)      !Not 80!        POK02200
      STUFF = "Cleared."                                                POK02300
      READ (F,REC = 7,ERR = 666) STUFF(1:80)                            POK02400
      WRITE (MSG,1) "Record",STUFF(1:80)                                POK02500
      STOP                                                              POK02600
  666 WRITE (MSG,*) "Can't get the record!"                             POK02700
      END       !That was easy.                                         POK02800

```

Output:

```txt

       To select record 7 from a disc file.
 As a FORMATTED file.
Result: Length 80
Record:         CHARACTER*(*) IS       !Stashed here.                          SAM00700

 As a random-access unformatted file.
Record:         CHARACTER*(*) IS       !Stashed here.                          SAM00700

```

Fortran's file reading counts a null line as a valid record so fortunately, there is no difficulty there. Trailing spaces appear in IS because of fixed-size CHARACTER variables. A length parameter for the length of the record as read (with possible trailing spaces counted) could easily enough be passed back.


### Random access

An entirely different approach is possible if the file is opened for random access, and has fixed-size records. In such a case, <code>READ (F,REC=7,ERR=666) STUFF</code> would suffice (where STUFF was the right size for the record) and if the record did not exist (being beyond the last record of a short file) then label 666 would be jumped to - without that, a crash results. In the ASCII world, text files have varying-length records so the example file reactivates source line sequence numbers of the Fortran fixed-format style and yet again, the source file highlighter doesn't recognise another foible of Fortran layout. Although there are many conventions (the simplest being digits only), here a text name is crammed in to the sequence field of columns 73-80.

This results in every line being of the same length with an obvious route towards calculating the location of a random record. But, the actual record length is ''not'' 80, because in the ASCII world, plain text files have their records separated by CR, or CRLF, or LFCR, or CR - depending on the system. Experiment shows that this system (Windows XP) uses CRLF, and so the record length is 8'''2'''. But that's not the end of it. The RECL parameter by default is in terms of the default integer size, which is four bytes, and so the record length cannot be specified correctly! Fortunately, the Compaq Visual Fortran compiler has an option to specify that the RECL value is to be in bytes, and, invisibly so here, this has been done...

Obviously, this only works because of the special nature of the file being read. Other systems offer a filesystem that does not regard record sizes or separators as being a part of the record that is read or written, and in such a case, a CR (or CRLF, or whatever) does not appear amongst the data. Some systems escalate to enabling such random access for varying-length records, or access by a key text rather than a record number (so that a key of "SAM00700" might be specified), and acronyms such as ISAM start appearing.

In Fortran 77 there was no verbose REC=''n'' facility, instead one used
```Fortran
READ (F'7) STUFF(1:80)
```
 - that is, an apostrophe even though an at-sign was available - and again the source file highlighting is confused. An interesting alternative was to use the FIND(F'7) statement instead, followed by an ordinary READ (or WRITE) not necessarily specifying the desired record number. The point of this is that the FIND statement would initiate the pre-positioning for the next I/O ''asynchronously'' so that other processing could intervene between it and the READ or WRITE, and in situations more complex than this example, there could be startling changes in performance. If not always positive ones when many files were being accessed on one physical disc drive. Unfortunately, later Fortran extensions have abandoned this statement, while multiprocessing has proliferated.


The GO TO style of handling mishaps in an I/O statement makes a simple structure difficult - note that the reception area ought not be fallen into by normal execution, thus the STOP. Unfortunately, READ and WRITE statements do not return a result that could be tested in an IF-statement or WHILE-loop, but this can be approached with only a little deformation:
```Fortran
      READ (F,REC = 7,ERR = 666, IOSTAT = IOSTAT) STUFF(1:80)
  666 IF (IOSTAT.NE.0) THEN
        WRITE (MSG,*) "Can't get the record: code",IOSTAT
       ELSE
        WRITE (MSG,1) "Record",STUFF(1:80)
      END IF
```

Where IOSTAT is an integer variable (and also a key word) and this formulation means not having to remember which way the assignment goes; it is left to right. The error code numbers are unlikely to be the same across different systems, so experimentation is in order. It would be much nicer to be able to write something like <code>IF (READ (F,REC = 7, IOSTAT = IOSTAT) STUFF(1:80)) THEN ''etc.''</code> or <code>DO WHILE(''etc.'')</code>


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Open "input.txt" For Input As #1
Dim line_ As String
Dim count As Integer = 0
While Not Eof(1)
  Line Input #1, line_  '' read each line
  count += 1
  If count = 7 Then
    line_ = Trim(line_, Any !" \t")   '' remove any leading or trailing spaces or tabs
    If line_ = "" Then
      Print "The 7th line is empty"
    Else
      Print "The 7th line is : "; line_
    End If
    Exit While
  End If
Wend
If count < 7 Then
  Print "There are only"; count; " lines in the file"
End If
Close #1
Print
Print "Press any key to quit"
Sleep
```


=={{header|F_Sharp|F#}}==

```fsharp
open System
open System.IO

[<EntryPoint>]
let main args =
    let n = Int32.Parse(args.[1]) - 1
    use r = new StreamReader(args.[0])
    let lines = Seq.unfold (
                    fun (reader : StreamReader) ->
                    if (reader.EndOfStream) then None
                    else Some(reader.ReadLine(), reader)) r
    let line = Seq.nth n lines  // Seq.nth throws an ArgumentException,
                                // if not not enough lines available
    Console.WriteLine(line)
    0
```



## FutureBasic

Uses FB's native file$ command that opens a dialog window and allows the user to select the file to read.

```futurebasic

include "ConsoleWindow"

dim as long     i : i = 1
dim as Str255   s, lineSeven
dim as CFURLRef url

if ( files$( _CFURLRefOpen, "TEXT", "Select text file", @url ) )
  open "I", 2, @url
  while ( not eof(2) )
    line input #2, s
      if ( i == 7 )
        lineSeven = s
      end if
      i++
  wend
  close 2
end if

if ( lineSeven[0] )
  print lineSeven
else
  print "File did not contain seven lines, or line was empty."
end if

```


Input text file:

```txt

Line 1
Line 2
Line 3
Line 4
Line 5
Line 6
Line 7
Line 8
Line 9
Line 10

```

Output:

```txt

Line 7

```



## Go


```go
package main

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"os"
)

func main() {
	if line, err := rsl("input.txt", 7); err == nil {
		fmt.Println("7th line:")
		fmt.Println(line)
	} else {
		fmt.Println("rsl:", err)
	}
}

func rsl(fn string, n int) (string, error) {
	if n < 1 {
		return "", fmt.Errorf("invalid request: line %d", n)
	}
	f, err := os.Open(fn)
	if err != nil {
		return "", err
	}
	defer f.Close()
	bf := bufio.NewReader(f)
	var line string
	for lnum := 0; lnum < n; lnum++ {
		line, err = bf.ReadString('\n')
		if err == io.EOF {
			switch lnum {
			case 0:
				return "", errors.New("no lines in file")
			case 1:
				return "", errors.New("only 1 line")
			default:
				return "", fmt.Errorf("only %d lines", lnum)
			}
		}
		if err != nil {
			return "", err
		}
	}
	if line == "" {
		return "", fmt.Errorf("line %d empty", n)
	}
	return line, nil
}
```



## Groovy


```groovy
def line = null
new File("lines.txt").eachLine { currentLine, lineNumber ->
    if (lineNumber == 7) {
        line = currentLine
    }
}
println "Line 7 = $line"
```



## Haskell



```Haskell
main :: IO ()
main = do contents <- readFile filename
          case drop 6 $ lines contents of
            []  -> error "File has less than seven lines"
            l:_ -> putStrLn l
  where filename = "testfile"
```


=={{header|Icon}} and {{header|Unicon}}==
The procedure ''readline'' uses repeated alternation (i.e. |read()) to generate the lines of the file one at a time and limitation (i.e. \ n) to limit the generation to n results.  If the file is not large enough ''readline'' will fail.

While it is certainly possible to read at file at specific offsets without reading each line via ''seek'', with files using line feed terminated variable length records something has to read the data to determine the 7th record.  This solution uses a combination of repeated alternation and generation limiting to achieve this.  The counter is simply to discover if there are enough records.


```Icon
procedure main()
write(readline("foo.bar.txt",7)|"failed")
end

procedure readline(f,n)                         # return n'th line of file f
f := open(\f,"r") | fail                        # open file
every  i := n & line := |read(f) \ n do i -:= 1  #  <== here
close(f)
if i = 0 then return line
end
```



## J


```j
readLine=: 4 :0
  (x-1) {:: <;.2 ] 1!:1 boxxopen y
)
```


Thus:

```bash
$ cal 2011 > cal.txt
```



```j
   7 readLine 'cal.txt'
 9 10 11 12 13 14 15  13 14 15 16 17 18 19  13 14 15 16 17 18 19
```


Note that this code assumes that the last character in the file is the line end character, and that the line end character is a part of the line to be retrieved.

'''Tacit alternative'''

```j
require 'files'     NB. required for versions before J701
readLineT=: <:@[ {:: 'b'&freads@]
```

This is not quite equivalent to the code above as it handles cross-platform line-endings and those line end character(s) are removed from the result.


## Java

<tt>example: java -cp . LineNbr7 LineNbr7.java</tt>

<tt>output : line 7: 	public static void main(String[] args) throws Exception {;</tt>

```java
package linenbr7;

import java.io.*;

public class LineNbr7 {

    public static void main(String[] args) throws Exception {
        File f = new File(args[0]);
        if (!f.isFile() || !f.canRead())
            throw new IOException("can't read " + args[0]);

        BufferedReader br = new BufferedReader(new FileReader(f));
        try (LineNumberReader lnr = new LineNumberReader(br)) {
            String line = null;
            int lnum = 0;
            while ((line = lnr.readLine()) != null
                    && (lnum = lnr.getLineNumber()) < 7) {
            }

            switch (lnum) {
                case 0:
                    System.out.println("the file has zero length");
                    break;
                case 7:
                    boolean empty = "".equals(line);
                    System.out.println("line 7: " + (empty ? "empty" : line));
                    break;
                default:
                    System.out.println("the file has only " + lnum + " line(s)");
            }
        }
    }
}
```



## jq

Using jq 1.4, one would have to read the entire file in order to extract a particular line.
Since April 24, 2015, however, the task can be accomplished by only reading the lines up to the desired line number.
We accordingly showcase here these recently added features of jq:
* "inputs" - a builtin which produces a stream
* "foreach" - a control structure for iterating over a stream
*  "break" - for breaking out of a loop

```jq
# Input - a line number to read, counting from 1
# Output - a stream with 0 or 1 items
def read_line:
  . as $in
  | label $top
  | foreach inputs as $line
      (0; .+1; if . == $in then $line, break $top else empty end) ;
```

'''Example:''' Read line number $line (to be provided on the command line), counting from 1

```jq
$line | tonumber
| if . > 0 then read_line
  else "$line (\(.)) should be a non-negative integer"
  end
```

{{out}}

```sh
$ jq -n -r 'range(0;20) | tostring' | jq --arg line 10 -n -R -r -f Read_a_specific_line_from_a_file.jq
9
```



## Julia

The short following snippet of code actually stores all the lines from the file in an array and displays the seventh element of the array, returning an error if there is no such element. Since the array is not referenced, it will be garbage collected when needed. The filehandle is closed upon completion of the task, be it successful or not.

```Julia
open(readlines, "path/to/file")[7]
```

The next function reads n lines in the file and displays the last read if possible, or returns a short message. Here again, the filehandle is automatically closed after the task. Note that the first line is returned if a negative number is given as the line number.

```Julia
function read_nth_lines(stream, num)
  for i = 1:num-1
    readline(stream)
  end
  result = readline(stream)
  print(result != "" ? result : "No such line.")
end
```

{{Out}}

```txt
julia> open(line -> read_nth_lines(line, 7), "path/to/file")
"Hi, I am the content of the seventh line\n"
```



## Kotlin


```scala
// version 1.1.2

import java.io.File

fun main(args: Array<String>) {
    /* The following code reads the whole file into memory
       and so should not be used for large files
       which should instead be read line by line until the
       desired line is reached */

    val lines = File("input.txt").readLines()
    if (lines.size < 7)
        println("There are only ${lines.size} lines in the file")
    else {
        val line7 = lines[6].trim()
        if (line7.isEmpty())
            println("The seventh line is empty")
        else
            println("The seventh line is : $line7")
    }
}

/* Note that 'input.txt' contains the eight lines:
Line 1
Line 2
Line 3
Line 4
Line 5
Line 6
Line 7
Line 8
*/
```


{{out}}

```txt

The seventh line is : Line 7

```



## Lasso


```Lasso
local(f) = file('unixdict.txt')
handle => { #f->close }
local(this_line = string,line = 0)
#f->forEachLine => {
	#line++
	#line == 7 ? #this_line = #1
	#line == 7 ? loop_abort
}
#this_line // 6th, which is the 7th line in the file

```



## Lua


```lua
iter = io.lines 'test.txt'
for i=0, 5 do
    if not iter() then
        error 'Not 7 lines in file'
    end
end

line = iter()
```



## Liberty BASIC

We read the whole file into memory, and use 'word$( string, number, delimiter)'. Line delimiter is assumed to be CRLF, and the file is assumed to exist at the path given.

```lb
fileName$    ="F:\sample.txt"
requiredLine =7

open fileName$ for input as #i
    f$ =input$( #i, lof( #i))
close #i

line7$ =word$( f$, 7, chr$( 13))
if line7$ =chr$( 13) +chr$( 10) or line7$ ="" then notice "Empty line! ( or file has fewer lines)."

print line7$
```




## Maple


```Maple
path := "file.txt":
specificLine := proc(path, num)
	local i, input:
	for i to num do
		input := readline(path):
		if input = 0 then break; end if:
	end do:
	if i = num+1 then printf("Line %d, %s", num, input):
	elif i <= num then printf ("Line number %d is not reached",num): end if:
end proc:
```




## Mathematica


```Mathematica
 If[# != EndOfFile , Print[#]]& @ ReadList["file",  String, 7]
```


=={{header|MATLAB}} / {{header|Octave}}==


```Matlab

  eln = 7;  % extract line number 7
  line = '';
  fid = fopen('foobar.txt','r');
  if (fid < 0)
	printf('Error:could not open file\n')
  else
        n = 0;
	while ~feof(fid),
              n = n + 1;
              if (n ~= eln),
                    fgetl(fid);
              else
                    line = fgetl(fid);
              end
	end;
        fclose(fid);
  end;
  printf('line %i: %s\n',eln,line);

```

<nowiki>Insert non-formatted text here</nowiki>


## MoonScript

{{trans|Lua}}

```MoonScript
iter = io.lines 'test.txt'
for i=0, 5
  error 'Not 7 lines in file' if not iter!

print iter!
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

parse arg inFileName lineNr .

if inFileName = '' | inFileName = '.' then inFileName = './data/input.txt'
if lineNr     = '' | lineNr     = '.' then lineNr     = 7

do
  lineTxt = readLine(inFileName, lineNr)
  say '<textline number="'lineNr.right(5, 0)'">'lineTxt'</textline>'
catch ex = Exception
  ex.printStackTrace()
end

return

--
### =======================================================================

-- NetRexx/Java programs don't have a special mechanism to seek to a specified line number
-- the simple solution is to iterate through file. (Costly for very large files)
method readLine(inFileName, lineNr) public static signals IOException, FileNotFoundException

  lineReader = LineNumberReader(FileReader(File(inFileName)))
  notFound   = isTrue
  lineTxt    = ''
  loop label reading forever
    line = lineReader.readLine()
    select
      when lineReader.getLineNumber() = lineNr then do
        lineTxt  = line
        notFound = isFalse
        leave reading -- terminate I/O loop
        end
      when line = null then do
        leave reading -- terminate I/O loop
        end
      otherwise nop
      end
    finally
      lineReader.close()
    end reading

  if notFound then signal RuntimeException('File' inFileName 'does not contain line' lineNr.right(5))

  return lineTxt

--
### =======================================================================

method isTrue() public static returns boolean
  return 1 == 1
--
### =======================================================================

method isFalse() public static returns boolean
  return \(1 == 1)

```



## Nim



```nim
var
  line: TaintedString
  f = open("test.txt", fmRead)

for x in 0 .. 6:
  try:
    line = readLine f
  except EIO:
    echo "Not 7 lines in file"
```



## OCaml


OCaml does not provide built-in facilities to obtain a particular line from a file. It only provides a function to read one line from a file from the current position in the input channel [http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html#VALinput_line input_line]. We can use this function to get the seventh line from a file, for example as follows:


```ocaml
let input_line_opt ic =
  try Some (input_line ic)
  with End_of_file -> None

let nth_line n filename =
  let ic = open_in filename in
  let rec aux i =
    match input_line_opt ic with
    | Some line ->
        if i = n then begin
          close_in ic;
          (line)
        end else aux (succ i)
    | None ->
        close_in ic;
        failwith "end of file reached"
  in
  aux 1

let () =
  print_endline (nth_line 7 Sys.argv.(1))
```



## PARI/GP

GP is not able to read specific lines, only whole files. For this capability one can use the <code>extern</code>, <code>externstr</code>, or <code>system</code> commands together with, e.g., the [[#AWK|AWK]] solution, or else use the [[#C|C]] solution from within PARI itself.


## Pascal

{{works with|Free_Pascal}}

```pascal
Program FileTruncate;

uses
  SysUtils;

const
  filename = 'test';
  position = 7;

var
  myfile: text;
  line: string;
  counter: integer;

begin
  if not FileExists(filename) then
  begin
    writeln('Error: File does not exist.');
    exit;
  end;

  Assign(myfile, filename);
  Reset(myfile);
  counter := 0;
  Repeat
    if eof(myfile) then
    begin
      writeln('Error: The file "', filename, '" is too short. Cannot read line ', position);
      Close(myfile);
      exit;
    end;
    inc(counter);
    readln(myfile);
  until counter = position - 1;
  readln(myfile, line);
  Close(myfile);
  writeln(line);
end.
```

Output:

```txt

line 7 from file test

```



## Perl


```perl
#!/usr/bin/perl -s
# invoke as <scriptname> -n=7 [input]
while (<>) { $. == $n and print, exit }
die "file too short\n";
```



## Perl 6


```perl6
say lines[6] // die "Short file";
```

Without an argument, the <tt>lines</tt> function reads filenames from the command line, or defaults to standard input. It then returns a lazy list, which we subscript to get the 7th element. Assuming this code is in a program called <tt>line7</tt>:

```txt
$ cal 2011 > cal.txt
$ line7 cal.txt
16 17 18 19 20 21 22  20 21 22 23 24 25 26  20 21 22 23 24 25 26
$
```


This works even on infinite files because lists are lazy:

```txt
$ yes | line7
y
$
```



## Phix

No specific mechanism, but simple enough. If the file is suitably small:

```Phix
integer fn = open("TEST.TXT","r")
sequence lines = get_text(fn,GT_LF_STRIPPED)
close(fn)
if length(lines)>=7 then
    ?lines[7]
else
    ?"no line 7"
end if
```

For bigger files:

```Phix
integer fn = open("TEST.TXT","r")
for i=1 to 6 do
    {} = gets(fn)
end for
?gets(fn)  -- (shows -1 if past eof)
close(fn)
```



## PicoLisp


```PicoLisp
(in "file.txt"
   (do 6 (line))
   (or (line) (quit "No 7 lines")) )
```



## PL/I


```PL/I

declare text character (1000) varying, line_no fixed;

get (line_no);
on endfile (f) begin;
  put skip list ('the specified line does not exist');
  go to next;
end;

get file (f) edit ((text do i = 1 to line_no)) (L);

put skip list (text);
next: ;

```



## PowerShell

{{works with|PowerShell|3.0}}

```Powershell

$file = Get-Content c:\file.txt
if ($file.count -lt 7)
{Write-Warning "The file is too short!"}
else
{
    $file | Where Readcount -eq 7 | set-variable -name Line7
}

```



## Python


Using only builtins (note that <code>enumerate</code> is zero-based):


```python
with open('xxx.txt') as f:
    for i, line in enumerate(f):
        if i == 6:
            break
    else:
        print('Not 7 lines in file')
        line = None
```


Using the <code>islice</code> iterator function from the [https://docs.python.org/3/library/itertools.html#itertools.islice itertools] standard library module, which applies slicing to an iterator and thereby skips over the first six lines:


```python
from itertools import islice

with open('xxx.txt') as f:
    try:
        line = next(islice(f, 6, 7))
    except StopIteration:
        print('Not 7 lines in file')
```


Similar to the Ruby implementation, this will read up to the first 7 lines, returning only the last. Note that the 'readlines' method reads the entire file contents into memory first as opposed to using the file iterator itself which is more performant for large files.

```python

print open('xxx.txt').readlines()[:7][-1]

```



## PureBasic


```purebasic
Structure lineLastRead
  lineRead.i
  line.s
EndStructure

Procedure readNthLine(file, n, *results.lineLastRead)
  *results\lineRead = 0
  While *results\lineRead < n And Not Eof(file)
    *results\line = ReadString(file)
    *results\lineRead + 1
  Wend

  If *results\lineRead = n
    ProcedureReturn 1
  EndIf
EndProcedure

Define filename.s = OpenFileRequester("Choose file to read a line from", "*.*", "All files (*.*)|*.*", 0)
If filename
  Define file = ReadFile(#PB_Any, filename)
  If file
    Define fileReadResults.lineLastRead, lineToRead = 7
    If readNthLine(file, lineToRead, fileReadResults)
      MessageRequester("Results", fileReadResults\line)
    Else
      MessageRequester("Error", "There are less than " + Str(lineToRead) + " lines in file.")
    EndIf
    CloseFile(file)
  Else
    MessageRequester("Error", "Couldn't open file " + filename + ".")
  EndIf
EndIf
```



## R


```R>
 seven <- scan('hw.txt', '', skip = 6, nlines = 1, sep = '\n') # too short
Read 0 items
> seven <- scan('Incoming/quotes.txt', '', skip = 6, nlines = 1, sep = '\n')
Read 1 item

```



## Racket


```Racket

#lang racket

;; simple, but reads the whole file
(define s1 (list-ref (file->lines "some-file") 6))

;; more efficient: read and discard n-1 lines
(define s2
  (call-with-input-file "some-file"
    (λ(i) (for/last ([line (in-lines i)] [n 7]) line))))

```



## REBOL


```rebol

x: pick read/lines request-file/only 7
either x [print x] [print "No seventh line"]

```



## Red


```Red>>
 x: pick read/lines %file.txt 7

case [
    x = none  [print "File has less than seven lines"]
    (length? x) = 0 [print "Line 7 is empty"]
    (length? x) > 0 [print append "Line seven =  " x]
]
```



## REXX


### for newer REXXes


```REXX
/*REXX program reads a specific line from a file  (and displays the length and content).*/
parse arg FID n .                                /*obtain optional arguments from the CL*/
if FID=='' | FID==","  then  FID= 'JUNK.TXT'     /*not specified?  Then use the default.*/
if   n=='' |   n==","  then    n=7               /* "      "         "   "   "      "   */

if lines(FID)==0  then  call ser "wasn't found." /*see if the file    exists  (or not). */

call linein FID, n-1                             /*read the record previous to  N.      */
if lines(FID)==0  then  call ser "doesn't contain"       N        'lines.'
                                                 /* [↑]  any more lines to read in file?*/

$=linein(FID)                                    /*read the   Nth  record in the file.  */

say 'File '  FID  " line "  N  ' has a length of: '         length($)
say 'File '  FID  " line "  N  'contents: '   $  /*display the contents of the Nth line.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
ser:   say;     say '***error!***  File '     FID     " "    arg(1);      say;     exit 13
```



### for older REXXes

Some older REXXes don't support a 2<sup>nd</sup> argument for the   '''linein'''   BIF, so here is an alternative:

```REXX
/*REXX program reads a specific line from a file  (and displays the length and content).*/
parse arg FID n .                                /*obtain optional arguments from the CL*/
if FID=='' | FID==","  then  FID= 'JUNK.TXT'     /*not specified?  Then use the default.*/
if   n=='' |   n==","  then    n=7               /* "      "         "   "   "      "   */

if lines(FID)==0  then  call ser "wasn't found." /*see if the file    exists  (or not). */

  do  n-1
  call linein FID                                /*read all the lines previous to  N.  */
  end   /*n-1*/

if lines(FID)==0  then  call ser "doesn't contain"       N        'lines.'
                                                 /* [↑]  any more lines to read in file?*/

$=linein(FID)                                    /*read the   Nth  record in the file.  */

say 'File '  FID  " line "  N  ' has a length of: '         length($)
say 'File '  FID  " line "  N  'contents: '   $  /*display the contents of the Nth line.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
ser:   say;     say '***error!***  File '     FID     " "    arg(1);      say;     exit 13
```






## Ring


```ring

fp = fopen("C:\Ring\ReadMe.txt","r")
n = 0

r = ""
while isstring(r)
      while n < 8
            r = fgetc(fp)
            if r = char(10) n++ see nl
            else see r ok
      end
end
fclose(fp)

```



## Ruby

The each_line method returns an Enumerator, so no more than seven lines are read.

```ruby
 seventh_line = open("/etc/passwd").each_line.take(7).last

```



## Run BASIC


```runbasic
fileName$    = "f:\sample.txt"
requiredLine = 7
open fileName$ for input as #f

for i = 1 to requiredLine
  if not(eof(#f)) then line input #f, a$
next i
close #f
print a$
end
```



## Rust


```rust
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Error;
use std::path::Path;

fn main() {
    let path = Path::new("file.txt");
    let line_num = 7usize;
    let line = get_line_at(&path, line_num - 1);
    println!("{}", line.unwrap());
}

fn get_line_at(path: &Path, line_num: usize) -> Result<String, Error> {
    let file = File::open(path).expect("File not found or cannot be opened");
    let content = BufReader::new(&file);
    let mut lines = content.lines();
    lines.nth(line_num).expect("No line found at that position")
}
```


Alternate implementation with argument parsing. First argument is the path to the file and is required. Second argument is the line number and is optional. By default the first line will be printed.

```rust
use std::env;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::path::Path;

fn main() {
    if env::args().len() <= 1 {
        println!("At least a path to a file is needed: No file path given");
        return;
    } else {
        let path = &env::args().nth(1).expect("could not parse the path");
        let path = Path::new(&path);
        let mut line_num = 1usize;
        if let Some(arg) = env::args().nth(2) {
            line_num = arg.parse::<usize>().expect("Parsing line number failed");
        }
        print_line_at(&path, line_num);
    }
}

fn print_line_at(path: &Path, line_num: usize) {
    if line_num < 1 {
        panic!("Line number has to be > 0");
    }
    let line_num = line_num - 1;
    let file = File::open(path).expect("File not found or cannot be opened");
    let content = BufReader::new(&file);
    let mut lines = content.lines();
    let line = lines.nth(line_num).expect("No line found at given position");
    println!("{}", line.expect("None line"));
}
```



## Scala


### Discussion

The code will throw a <tt>NoSuchElementException</tt> if the file doesn't have 7 lines.


```scala
val lines = io.Source.fromFile("input.txt").getLines
val seventhLine = lines drop(6) next
```


### Imperative version

Solving the task to the letter, imperative version:


```scala
var lines: Iterator[String] = _
try {
  lines = io.Source.fromFile("input.txt").getLines drop(6)
} catch {
  case exc: java.io.IOException =>
    println("File not found")
}
var seventhLine: String = _
if (lines != null) {
  if (lines.isEmpty) println("too few lines in file")
  else seventhLine = lines next
}
if ("" == seventhLine) println("line is empty")
```

===Functional version (Recommanded)===

```scala
val file = try Left(io.Source.fromFile("input.txt")) catch {
  case exc => Right(exc.getMessage)
}
val seventhLine = (for(f <- file.left;
  line <- f.getLines.toStream.drop(6).headOption.toLeft("too few lines").left) yield
    if (line == "") Right("line is empty") else Left(line)).joinLeft
```



## sed

To print seventh line

```sed

sed -n 7p

```

To print error message if no such line

```sed

sed -n '7{p;h;}; ${x;/^$/s/^/Error: no such line/p}'

```

That is we remember (h) the line, if any, in hold space. At last line ($) we exchange (x) pattern space and hold space. If hold space was empty -- print error message.

## Seed7

The function ''getLine'' skips lines with [http://seed7.sourceforge.net/libraries/file.htm#readln%28inout_file%29 readln]
and reads the requested line with [http://seed7.sourceforge.net/libraries/file.htm#getln%28inout_file%29 getln] afterwards:


```seed7
$ include "seed7_05.s7i";

const func string: getLine (inout file: aFile, in var integer: lineNum) is func
  result
    var string: line is "";
  begin
    while lineNum > 1 and hasNext(aFile) do
      readln(aFile);
      decr(lineNum);
    end while;
    line := getln(aFile);
  end func;

const proc: main is func
  local
    var string: fileName is "input.txt";
    var file: aFile is STD_NULL;
    var string: line is "";
  begin
    aFile := open(fileName, "r");
    if aFile = STD_NULL then
      writeln("Cannot open " <& fileName);
    else
      line := getLine(aFile, 7);
      if eof(aFile) then
        writeln("The file does not have 7 lines");
      else
        writeln("The 7th line of the file is:");
        writeln(line);
      end if;
    end if;
  end func;
```



## Sidef


```ruby
func getNthLine(filename, n) {
  var file = File.new(filename);
  file.open_r.each { |line|
    Num($.) == n && return line;
  }
  warn "file #{file} does not have #{n} lines, only #{$.}\n";
  return nil;
}

var line = getNthLine("/etc/passwd", 7);
print line if defined line;
```



## Smalltalk


```smalltalk

line := (StandardFileStream oldFileNamed: 'test.txt') contents lineNumber: 7.

```



## SPL


```spl
lines = #.readlines("test.txt")
#.output("Seventh line of text:")
? #.size(lines,1)<7
  #.output("is absent")
!
  #.output(lines[7])
.
```



## Stata

See '''[http://www.stata.com/help.cgi?use use]''' in Stata help, to load a dataset or a part of it.


```stata
* Read rows 20 to 30 from somedata.dta
. use somedata in 20/30, clear

* Read rows for which the variable x is positive
. use somedata if x>0, clear
```


If there are not enough lines, an error message is print. It's possible to '''[http://www.stata.com/help.cgi?capture capture]''' the error and do something else:


```stata
capture use somedata in 7, clear
if _rc {
	display "Too few lines"
}
```



## Tcl

This code can deal with very large files with very long lines (up to 1 billion characters in a line should work fine, provided enough memory is available) and will return an empty string when the ''n''th line is empty (as an empty line is still a valid line).

```tcl
proc getNthLineFromFile {filename n} {
    set f [open $filename]
    while {[incr n -1] > 0} {
        if {[gets $f line] < 0} {
            close $f
            error "no such line"
        }
    }
    close $f
    return $line
}

puts [getNthLineFromFile example.txt 7]
```

Where it is necessary to provide very fast access to lines of text, it becomes sensible to create an index file describing the locations of the starts of lines so that the reader code can <code>seek</code> directly to the right location. This is rarely needed, but can occasionally be helpful.


## TorqueScript

  %file = new fileObject();
  %file.openForRead("File/Path.txt");
  $seventhLine = "";

  while(!%file.isEOF())
  {
  	%line++;

  	if(%line == 7)
  	{
  		$seventhLine = %file.readLine();

  		if($seventhLine $= "")
  		{
  			error("Line 7 of the file is blank!");
  		}
  	}
  }

  %file.close();
  %file.delete();

  if(%line < 7)
  {
  	error("The file does not have seven lines!");
  }


## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
file="lines.txt"
ERROR/STOP OPEN (file,READ,-std-)
line2fetch=7
```



## TXR


### From the top

Variable "line" matches and takes eighth line of input:

```txr
@(skip nil 7)
@line
```



### From the bottom

Take the third line from the bottom of the file, if it exists.

```txr
@(skip)
@line
@(skip 1 2)
@(eof)
```

How this works is that the first <code>skip</code> will skip enough lines until the rest of the query successfully matches the input. The rest of the query matches a line, then skips two lines, and matches on EOF.  So <code>@line</code> can only  match at one location: three lines up from the end of the file. If the file doesn't have at least three lines, the query fails.


## UNIX Shell

{{trans|Tcl}}
{{works with|bash}}

```bash
get_nth_line() {
    local file=$1 n=$2 line
    while ((n-- > 0)); do
        if ! IFS= read -r line; then
            echo "No such line $2 in $file"
            return 1
        fi
    done < "$file"
    echo "$line"
}

get_nth_line filename 7
```



## Ursa


```ursa>decl string<
 lines
decl file f
f.open "filename.txt"
set lines (f.readlines)
f.close

if (< (size lines) 7)
        out "the file has less than seven lines" endl console
        stop
end if

out "the seventh line in the file is:" endl endl console
out lines<6> endl console
```



## VBScript


```vb

Function read_line(filepath,n)
	Set objFSO = CreateObject("Scripting.FileSystemObject")
	Set objFile = objFSO.OpenTextFile(filepath,1)
	arrLines = Split(objFile.ReadAll,vbCrLf)
	If UBound(arrLines) >= n-1 Then
		If arrLines(n-1) <> "" Then
			read_line = arrLines(n-1)
		Else
			read_line = "Line " & n & " is null."
		End If
	Else
		read_line = "Line " & n & " does not exist."
	End If
	objFile.Close
	Set objFSO = Nothing
End Function

WScript.Echo read_line("c:\temp\input.txt",7)

```


{{In}}

```txt

1st
22d
3rd
4th
5th
6th
7th
8th

```


{{Out}}

```txt
7th
```



## Vedit macro language

This example reads the 7th line (including newline character(s)) into text register 10.


```vedit
File_Open("example.txt", BROWSE)
Goto_Line(7)
if (Cur_Line < 7) {
    Statline_Message("File contains too few lines")
} else {
    if (At_EOL) {
        Statline_Message("Empty line")
    }
    Reg_Copy(10, 1)
}
Buf_Close(NOMSG)
```


If the file does not exist, the buffer will be empty and you get "File contains too few lines" error.

If the line is too long (more than about 230,000 characters), Vedit displays error message "Block too large for text registers, try clipboard").
This error could be avoided by reading the line to clipboard (which has larger size limit) or by copying the line to another edit buffer using a tmp file (in which case there is no size limit).


## XPL0

Filename.ext must be terminated with an EOF character (hex 1A).
Usage: readline <filename.ext


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations
def MaxLen = 82;        \maximum length of line that can be stored (incl CR+LF)

func ReadLine(N, L);    \Read line N from input file and return it in string L
int  N;  char L;
int  I, C;
[for I:= 1 to N-1 do    \skip to start of specified line
        repeat  C:= ChIn(1);
                if C = $1A\EOF\ then
                        [Text(0, "File only has ");  IntOut(0, I);
                        Text(0, " lines^M^J");  return false];
        until   C = $0A\LF\;
I:= 0;
repeat  C:= ChIn(1);
        if C = $1A\EOF\ then
                [Text(0, "Line is empty (EOF)^M^L");  return false];
        L(I):= C;  I:= I+1;
until   C=$0A\LF\ or I>=MaxLen;
if I >= MaxLen then Text(0, "Line might be truncated^M^J");
if I = 2 then Text(0, "Line is empty^M^J");
L(I-1):= L(I-1) ! $80;          \terminate string
return true;
];

char LineN(MaxLen);
if ReadLine(7, LineN) then Text(0, LineN)
```



## zkl

Many zkl sequence objects contain a readln method, some contain a seek (or equivalent) method. However, File only has readln. If, for some, reason, the nth line can't be read, an exception is thrown.

```zkl
reg line; do(7){line=File.stdin.readln()} println(">>>",line);
```

Or, suck in lines and take the last one:

```zkl
lines:=File.stdin.readln(7); println(">>>",line[-1]);
```



[[Category:File handling]]
