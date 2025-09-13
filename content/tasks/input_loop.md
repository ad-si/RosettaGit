+++
title = "Input loop"
description = ""
date = 2019-10-18T10:52:46Z
aliases = []
[extra]
id = 2598
[taxonomies]
categories = ["task", "Text processing"]
tags = []
+++

{{omit from|TI-89 BASIC}} <!-- No streams other than user input, not really applicable. -->

## Task

Read from a text stream either word-by-word or line-by-line until the stream runs out of data.

The stream will have an unknown amount of data on it.





## Ada

This example reads in a text stream from standard input line by line
and writes the output to standard output.

```ada
with Ada.Text_Io; use Ada.Text_Io;

procedure Read_Stream is
   Line : String(1..10);
   Length : Natural;
begin
   while not End_Of_File loop
      Get_Line(Line, Length); -- read up to 10 characters at a time
      Put(Line(1..Length));
      -- The current line of input data may be longer than the string receiving the data.
      -- If so, the current input file column number will be greater than 0
      -- and the extra data will be unread until the next iteration.
      -- If not, we have read past an end of line marker and col will be 1
      if Col(Current_Input) = 1 then
         New_Line;
      end if;
   end loop;
end Read_Stream;
```



## Aime


```aime
void
read_stream(file f)
{
    text s;

    while (f_line(f, s) != -1) {
        # the read line available as -s-
    }
}
```



## ALGOL 68

For file consisting of just one page - a typical linux/unix file:

```algol68
main:(
  PROC raise logical file end = (REF FILE f) BOOL: ( except logical file end );
  on logical file end(stand in, raise logical file end);
  DO
    print(read string);
    read(new line);
    print(new line)
  OD;
  except logical file end:
    SKIP
)
```

For multi page files, each page is seekable with ''<tt>PROC set = (REF FILE file, INT page, line, char)VOID: ~</tt>''. This allows rudimentary random access where each new page is effectively a new record.

```algol68
main:(
  PROC raise logical file end = (REF FILE f) BOOL: ( except logical file end );
  on logical file end(stand in, raise logical file end);
  DO
    PROC raise page end = (REF FILE f) BOOL: ( except page end );
    on page end(stand in, raise page end);
    DO
      print(read string);
      read(new line);
      print(new line)
    OD;
    except page end:
      read(new page);
      print(new page)
  OD;
  except logical file end:
    SKIP
)
```

The boolean functions ''physical file ended(f)'', ''logical file ended(f)'', ''page ended(f)'' and ''line ended(f)'' are also available to indicate the end of a file, page and line.


## ALGOL W


```algolw
begin
    string(80) line;
    % allow the program to continue after reaching end-of-file %
    % without this, end-of-file would cause a run-time error   %
    ENDFILE := EXCEPTION( false, 1, 0, false, "EOF" );
    % read lines until end of file                             %
    read( line );
    while not XCPNOTED(ENDFILE) do begin
        write( line );
        read(  line )
    end
end.
```



## AmigaE


```amigae
CONST BUFLEN=1024, EOF=-1

PROC consume_input(fh)
  DEF buf[BUFLEN] : STRING, r
  REPEAT
    /* even if the line si longer than BUFLEN,
       ReadStr won't overflow; rather the line is
       "splitted" and the remaining part is read in
       the next ReadStr */
    r := ReadStr(fh, buf)
    IF buf[] OR (r <> EOF)
      -> do something
      WriteF('\s\n',buf)
    ENDIF
  UNTIL r=EOF
ENDPROC

PROC main()
  DEF fh

  fh := Open('basicinputloop.e', OLDFILE)
  IF fh
    consume_input(fh)
    Close(fh)
  ENDIF
ENDPROC
```



## AutoHotkey

This example reads the text of a source file line by line
and writes the output to a destination file.

```AutoHotkey
Loop, Read, Input.txt, Output.txt
{
   FileAppend, %A_LoopReadLine%`n
}
```



## AWK

This just reads lines from stdin and prints them until EOF is read.


```awk
{ print $0 }
```


or, more idiomatic:


```awk>1</lang



## Batch File


```dos

for /f %%i in (file.txt) do if %%i@ neq @ echo %%i

```



## BASIC


=
## Applesoft BASIC
=

```ApplesoftBasic
100 INPUT "FILENAME:";F$
110 D$ = CHR$(4)
120 PRINT D$"VERIFY"F$
130 PRINT D$"OPEN"F$
140 PRINT D$"READ"F$
150 ONERR GOTO 190

160 GET C$
170 PRINT CHR$(0)C$;
180 GOTO 160

190 POKE 216,0
200 IF PEEK(222) <> 5 THEN RESUME
210 PRINT D$"CLOSE"F$
```


=
## BBC BASIC
=
This specifically relates to console input (stdin).

```bbcbasic
      STD_INPUT_HANDLE = -10
      STD_OUTPUT_HANDLE = -11
      SYS "GetStdHandle", STD_INPUT_HANDLE TO @hfile%(1)
      SYS "GetStdHandle", STD_OUTPUT_HANDLE TO @hfile%(2)
      SYS "SetConsoleMode", @hfile%(1), 0
      *INPUT 13
      *OUTPUT 14
      REPEAT
        INPUT A$
        PRINT A$
      UNTIL FALSE
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Type.bas"
110 TEXT 80
120 INPUT PROMPT "File name: ":F$
130 WHEN EXCEPTION USE IOERROR
140   OPEN #1:F$ ACCESS INPUT
150   DO
160     LINE INPUT #1,IF MISSING EXIT DO:F$
170     PRINT F$
180   LOOP
190   CLOSE #1
200 END WHEN
210 HANDLER IOERROR
220   PRINT EXSTRING$(EXTYPE)
230   END
240 END HANDLER
```


Alternate solution:
<lang IS-BASIC>100 PROGRAM "Type.bas"
110 INPUT PROMPT "File name: ":F$
120 WHEN EXCEPTION USE IOERROR
130   OPEN #1:F$
140   COPY FROM #1 TO #0
150   CLOSE #1
160 END WHEN
170 HANDLER IOERROR
180   PRINT EXSTRING$(EXTYPE)
190   CLOSE #1
200 END HANDLER
```



## Bracmat

This example first creates a test file with three lines. It then opens the file in read mode, sets the string of break characters and then reads the file token by token, where tokens are delimeted by break characters. Finally, the file position is set to an invalid value, which closes the file.

```bracmat
( put$("This is
a three line
text","test.txt",NEW)
& fil$("test.txt",r)
& fil$(,STR," \t\r\n")
& 0:?linenr
&   whl
  ' ( fil$:(?line.?breakchar)
    &   put
      $ ( str
        $ ( "breakchar:"
            ( !breakchar:" "&SP
            | !breakchar:\t&"\\t"
            | !breakchar:\r&"\\r"
            | !breakchar:\n&"\\n"
            | !breakchar:&EOF
            )
            ", word "
            (1+!linenr:?linenr)
            ":"
            !line
            \n
          )
        )
    )
& (fil$(,SET,-1)|out$"file closed")
);
```

```txt
breakchar:SP, word 1:This
breakchar:\n, word 2:is
breakchar:SP, word 3:a
breakchar:SP, word 4:three
breakchar:\n, word 5:line
breakchar:EOF, word 6:text
file closed
```



## C

Reads arbitrarily long line each time and return a null-terminated string.
Caller is responsible for freeing the string.

```cpp
#include <iostream>
#include <stdio.h>

char *get_line(FILE* fp)
{
	int len = 0, got = 0, c;
	char *buf = 0;

	while ((c = fgetc(fp)) != EOF) {
		if (got + 1 >= len) {
			len *= 2;
			if (len < 4) len = 4;
			buf = realloc(buf, len);
		}
		buf[got++] = c;
		if (c == '\n') break;
	}
	if (c == EOF && !got) return 0;

	buf[got++] = '\0';
	return buf;
}

int main()
{
	char *s;
	while ((s = get_line(stdin))) {
		printf("%s",s);
		free(s);
	}
	return 0;
}
```



## C++

The following functions send the words resp. lines
to a generic output iterator.

```cpp

#include <istream>
#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
#include <iterator>

// word by word
template<class OutIt>
void read_words(std::istream& is, OutIt dest)
{
  std::string word;
  while (is >> word)
  {
    // send the word to the output iterator
    *dest = word;
  }
}

// line by line:
template<class OutIt>
void read_lines(std::istream& is, OutIt dest)
{
  std::string line;
  while (std::getline(is, line))
  {
    // store the line to the output iterator
    *dest = line;
  }
}

int main()
{
  // 1) sending words from std. in std. out (end with Return)
  read_words(std::cin,
             std::ostream_iterator<std::string>(std::cout, " "));

  // 2) appending lines from std. to vector (end with Ctrl+Z)
  std::vector<std::string> v;
  read_lines(std::cin, std::back_inserter(v));

  return 0;
}


```


An alternate way to read words or lines is to use istream iterators:


```cpp

template<class OutIt>
void read_words(std::istream& is, OutIt dest)
{
    typedef std::istream_iterator<std::string> InIt;
    std::copy(InIt(is), InIt(),
              dest);
}

namespace detail
{
    struct ReadableLine : public std::string
    {
        friend std::istream & operator>>(std::istream & is, ReadableLine & line)
        {
            return std::getline(is, line);
        }
    };
}

template<class OutIt>
void read_lines(std::istream& is, OutIt dest)
{
    typedef std::istream_iterator<detail::ReadableLine> InIt;
    std::copy(InIt(is), InIt(),
              dest);
}

```


## C#


```c#
using System;
using System.IO;

class Program
{
    static void Main(string[] args)
    {
        // For stdin, you could use
        // new StreamReader(Console.OpenStandardInput(), Console.InputEncoding)

        using (var b = new StreamReader("file.txt"))
        {
            string line;
            while ((line = b.ReadLine()) != null)
                Console.WriteLine(line);
        }
    }
}
```



## Clojure


```lisp
(defn basic-input [fname]
  (line-seq (java.io.BufferedReader. (java.io.FileReader. fname))))
```



## COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. input-loop.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT in-stream ASSIGN TO KEYBOARD *> or any other file/stream
               ORGANIZATION LINE SEQUENTIAL
               FILE STATUS in-stream-status.

       DATA DIVISION.
       FILE SECTION.
       FD  in-stream.
       01  stream-line                 PIC X(80).

       WORKING-STORAGE SECTION.
       01  in-stream-status            PIC 99.
           88  end-of-stream           VALUE 10.

       PROCEDURE DIVISION.
           OPEN INPUT in-stream

           PERFORM UNTIL EXIT
               READ in-stream
                   AT END
                       EXIT PERFORM
               END-READ
               DISPLAY stream-line
           END-PERFORM

           CLOSE in-stream
           .
       END PROGRAM input-loop.
```



## Common Lisp


```lisp
(defun basic-input (filename)
    (with-open-file (stream (make-pathname :name filename) :direction :input)
        (loop for line = (read-line stream nil nil)
            while line
            do (format t "~a~%" line))))
```



## D


```d
void main() {
    import std.stdio;

    immutable fileName = "input_loop1.d";

    foreach (const line; fileName.File.byLine) {
        pragma(msg, typeof(line)); // Prints: const(char[])
        // line is a transient slice, so if you need to
        // retain it for later use, you have to .dup or .idup it.
        line.writeln; // Do something with each line.
    }

    // Keeping the line terminators:
    foreach (const line; fileName.File.byLine(KeepTerminator.yes)) {
        // line is a transient slice.
        line.writeln;
    }

    foreach (const string line; fileName.File.lines) {
        // line is a transient slice.
        line.writeln;
    }
}
```


```d
import tango.io.Console;
import tango.text.stream.LineIterator;

void main (char[][] args) {
    foreach (line; new LineIterator!(char)(Cin.input)) {
        // do something with each line
    }
}
```

```d
import tango.io.Console;
import tango.text.stream.SimpleIterator;

void main (char[][] args) {
    foreach (word; new SimpleIterator!(char)(" ", Cin.input)) {
        // do something with each word
    }
}
```


Note that foreach variables 'line' and 'word' are transient slices. If you need to retain them for later use, you should .dup them.


## Delphi


```Delphi
program InputLoop;

{$APPTYPE CONSOLE}

uses SysUtils, Classes;

var
  lReader: TStreamReader; // Introduced in Delphi XE
begin
  lReader := TStreamReader.Create('input.txt', TEncoding.Default);
  try
    while lReader.Peek >= 0 do
      Writeln(lReader.ReadLine);
  finally
    lReader.Free;
  end;
end.
```

=={{header|Déjà Vu}}==

```dejavu
while /= :eof dup !read-line!stdin:
	!print( "Read a line: " !decode!utf-8 swap )
drop
!print "End of file."
```



## EasyLang


<lang>repeat
  l$ = input
  until error = 1
  print l$
.
```



## Eiffel

```Eiffel

note
	description : "{
There are several examples included, including input from a text file,
simple console input and input from standard input explicitly.
See notes in the code for details.

Examples were compile using Eiffel Studio 6.6 with only the default
class libraries.
}"

class APPLICATION

create
	make

feature

	make
		do
			-- These examples show non-console input (a plain text file)
			-- with end-of-input handling.
			read_lines_from_file
			read_words_from_file

			-- These examples use simplified input from 'io', that
			-- handles the details of whether it's stdin or not
			-- They terminate on a line (word) of "q"
			read_lines_from_console_with_termination
			read_words_from_console_with_termination

			-- The next examples show reading stdin explicitly
			-- as if it were a text file.  It expects and end of file
			-- termination and so will loop indefinitely unless reading
			-- from a pipe or your console can send an EOF.
			read_lines_from_stdin
			read_words_from_stdin

			-- These examples use simplified input from 'io', that
			-- handles the details of whether it's stdin or not,
			-- but have no explicit termination
			read_lines_from_console_forever
			read_words_from_console_forever
		end

	--|--------------------------------------------------------------

	read_lines_from_file
			-- Read input from a text file
			-- Echo each line of the file to standard output.
			--
			-- Some language examples omit file open/close operations
			-- but are included here for completeness.  Additional error
			-- checking would be appropriate in production code.
		local
			tf: PLAIN_TEXT_FILE
		do
			print ("Reading lines from a file%N")
			create tf.make ("myfile") -- Create a file object
			tf.open_read -- Open the file in read mode

			-- The actual input loop

			from
			until tf.end_of_file
			loop
				tf.read_line
				print (tf.last_string + "%N")
			end

			tf.close -- Close the file
		end

	--|--------------------------------------------------------------

	read_words_from_file
			-- Read input from a text file
			-- Echo each word of the file to standard output on a
			-- separate line.
			--
			-- Some language examples omit file open/close operations
			-- but are included here for completeness.  Additional error
			-- checking would be appropriate in production code.
		local
			tf: PLAIN_TEXT_FILE
		do
			print ("Reading words from a file%N")
			create tf.make ("myfile") -- Create a file object
			tf.open_read -- Open the file in read mode

			-- The actual input loop

			from
			until tf.end_of_file
			loop
				-- This instruction is the only difference between this
				-- example and the read_lines_from_file example
				tf.read_word
				print (tf.last_string + "%N")
			end

			tf.close -- Close the file
		end

	--|--------------------------------------------------------------

	read_lines_from_console_with_termination
			-- Read lines from console and echo them back to output
			-- until the line contains only the termination key 'q'
			--
			-- 'io' is acquired through inheritance from class ANY,
			-- the top of all inheritance hierarchies.
		local
			the_cows_come_home: BOOLEAN
		do
			print ("Reading lines from console%N")
			from
			until the_cows_come_home
			loop
				io.read_line
				if io.last_string ~ "q" then
					the_cows_come_home := True
					print ("Mooooo!%N")
				else
					print (io.last_string)
					io.new_line
				end
			end
		end

	--|--------------------------------------------------------------

	read_words_from_console_with_termination
			-- Read words from console and echo them back to output, one
			-- word per line, until the line contains only the
			-- termination key 'q'
			--
			-- 'io' is acquired through inheritance from class ANY,
			-- the top of all inheritance hierarchies.
		local
			the_cows_come_home: BOOLEAN
		do
			print ("Reading words from console%N")
			from
			until the_cows_come_home
			loop
				io.read_word
				if io.last_string ~ "q" then
					the_cows_come_home := True
					print ("Mooooo!%N")
				else
					print (io.last_string)
					io.new_line
				end
			end
		end

	--|--------------------------------------------------------------

	read_lines_from_console_forever
			-- Read lines from console and echo them back to output
			-- until the program is terminated externally
			--
			-- 'io' is acquired through inheritance from class ANY,
			-- the top of all inheritance hierarchies.
		do
			print ("Reading lines from console (no termination)%N")
			from
			until False
			loop
				io.read_line
				print (io.last_string + "%N")
			end
		end

	--|--------------------------------------------------------------

	read_words_from_console_forever
			-- Read words from console and echo them back to output, one
			-- word per line until the program is terminated externally
			--
			-- 'io' is acquired through inheritance from class ANY,
			-- the top of all inheritance hierarchies.
		do
			print ("Reading words from console (no termination)%N")
			from
			until False
			loop
				io.read_word
				print (io.last_string + "%N")
			end
		end

	--|--------------------------------------------------------------

	read_lines_from_stdin
			-- Read input from a stream on standard input
			-- Echo each line of the file to standard output.
			-- Note that we treat standard input as if it were a plain
			-- text file
		local
			tf: PLAIN_TEXT_FILE
		do
			print ("Reading lines from stdin (EOF termination)%N")
			tf := io.input

			from
			until tf.end_of_file
			loop
				tf.read_line
				print (tf.last_string + "%N")
			end
		end

	--|--------------------------------------------------------------

	read_words_from_stdin
			-- Read input from a stream on standard input
			-- Echo each word of the file to standard output on a new
			-- line
			-- Note that we treat standard input as if it were a plain
			-- text file
		local
			tf: PLAIN_TEXT_FILE
		do
			print ("Reading words from stdin (EOF termination)%N")
			tf := io.input

			from
			until tf.end_of_file
			loop
				tf.read_line
				print (tf.last_string + "%N")
			end
		end

end

```




## Elena

ELENA 4.x:

Using ReaderEnumerator

```elena
import system'routines;
import system'io;
import extensions'routines;

public program()
{
    ReaderEnumerator.new(File.assign:"file.txt").forEach(printingLn)
}
```

Using loop statement

```elena
import system'io;

public program()
{
    using(var reader := File.assign:"file.txt".textreader())
    {
        while (reader.Available)
        {
           console.writeLine(reader.readLine())
        }
    }
}
```



## Elixir


```elixir
defmodule RC do
  def input_loop(stream) do
    case IO.read(stream, :line) do
      :eof -> :ok
      data -> IO.write data
              input_loop(stream)
    end
  end
end

path = hd(System.argv)
File.open!(path, [:read], fn stream -> RC.input_loop(stream) end)
```



## Erlang


```erlang
% Implemented by Arjun Sunel
-module(read_files).
-export([main/0]).

main() ->
	Read = fun (Filename) -> {ok, Data} = file:read_file(Filename), Data end,
	Lines = string:tokens(binary_to_list(Read("read_files.erl")), "\n"),
	lists:foreach(fun (Y) -> io:format("~s~n", [Y]) end, lists:zipwith(fun(X,_)->X end, Lines, lists:seq(1, length(Lines)))).

```



## ERRE


input from stdio

      LOOP
        INPUT(LINE,A$)
        PRINT(A$)
        EXIT IF <condition> ! condition to be implemented to
                            ! to avoid and endless loop
      END LOOP

reading a text file line by line

      OPEN("I",#1,FILENAME$)
      WHILE NOT EOF(1)
          INPUT(LINE,#1,A$)
          PRINT(A$)
      END WHILE
      CLOSE(1)

Note: with GET(#1) you can read character by character.



## Euphoria

Process text stream line-by-line:

```Euphoria
procedure process_line_by_line(integer fn)
    object line
    while 1 do
        line = gets(fn)
        if atom(line) then
            exit
        end if
        -- process the line
    end while
end procedure
```


=={{header|F Sharp|F#}}==
Using a sequence expression:

```fsharp

let lines_of_file file =
  seq { use stream = System.IO.File.OpenRead file
        use reader = new System.IO.StreamReader(stream)
        while not reader.EndOfStream do
          yield reader.ReadLine() }

```

The file is reopened every time the sequence is traversed and lines are read on-demand so this can handle arbitrarily-large files.


## Factor


```factor
"file.txt" utf8 [ [ process-line ] each-line ] with-file-reader
```



## Fantom


An input stream can be from a string or from a file.  The method <code>eachLine</code> will divide the stream by linebreaks.  The method <code>readStrToken</code> takes two arguments: a maximum size to read, and a function to decide when to stop reading - by default, it stops when it finds a white space.


```fantom

class Main
{
  public static Void main ()
  {
    // example of reading by line
    str := "first\nsecond\nthird\nword"
    inputStream := str.in

    inputStream.eachLine |Str line|
    {
      echo ("Line is: $line")
    }

    // example of reading by word
    str = "first second third word"
    inputStream = str.in

    word := inputStream.readStrToken // reads up to but excluding next space
    while (word != null)
    {
      echo ("Word: $word")
      inputStream.readChar // skip over the preceding space!
      word = inputStream.readStrToken
    }
  }
}

```



## Forth

```forth
4096 constant max-line
: read-lines
  begin  stdin pad max-line read-line throw
  while  pad swap   \ addr len is the line of data, excluding newline
         2drop
  repeat ;
```



## Fortran

The code read line-by-line, but the maximum length of the line is limited (by a parameter)


```fortran
program BasicInputLoop

  implicit none

  integer, parameter        :: in = 50, &
                               linelen = 1000
  integer                   :: ecode
  character(len=linelen)    :: l

  open(in, file="afile.txt", action="read", status="old", iostat=ecode)
  if ( ecode == 0 ) then
     do
        read(in, fmt="(A)", iostat=ecode) l
        if ( ecode /= 0 ) exit
        write(*,*) trim(l)
     end do
     close(in)
  end if

end program BasicInputLoop
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim line_ As String  ' line is a keyword
Open "input.txt" For Input As #1

While Not Eof(1)
  Input #1, line_
  Print line_  ' echo to the console
Wend

Close #1
Print
Print "Press any key to quit"
Sleep
```



## Frink


```frink
while (line = readStdin[]) != undef
   println[line]

```



## gnuplot

The following gnuplot script echoes standard input
to standard output line-by-line until the end of the stream.

```gnuplot
!cat
```

It makes use of the ability of gnuplot to spawn shell commands.
In that sense it might be considered cheating.
Nevertheless, this is a valid gnuplot script
that does meet the requirements of the task description.

It seems impossible to complete this task with just standard gnuplot commands.

## Go

The following reads a line at a time from stdin.

```go
package main

import (
	"bufio"
	"io"
	"log"
	"os"
)

func main() {
	in := bufio.NewReader(os.Stdin)
	for {
		s, err := in.ReadString('\n')
		if err != nil {
			// io.EOF is expected, anything else
			// should be handled/reported
			if err != io.EOF {
				log.Fatal(err)
			}
			break
		}
		// Do something with the line of text
		// in string variable s.
		_ = s
	}
}
```

Or, using <code>bufio.Scanner</code> you can read
line at a time,
word at a time,
byte or Unicode code point at a time,
or by any custom "split function".

```go
package main

import (
	"bufio"
	"log"
	"os"
)

func main() {
	s := bufio.NewScanner(os.Stdin)
	// Select the split function, other ones are available
	// in bufio or you can provide your own.
	s.Split(bufio.ScanWords)
	for s.Scan() {
		// Get and use the next 'token'
		asBytes := s.Bytes() // Bytes does no alloaction
		asString := s.Text() // Text returns a newly allocated string
		_, _ = asBytes, asString
	}
	if err := s.Err(); err != nil {
		// Handle/report any error (EOF will not be reported)
		log.Fatal(err)
	}
}
```



## FutureBasic

Note: This code goes beyond simply specifying the file to open. It includes a dialog window that allows the user to select a text file to read. The entire contents of the file are read in at once, rather than line by line.

```futurebasic

include "ConsoleWindow"

local fn ReadTextFile
dim as CFURLRef      fileRef
dim as Handle        h
dim as CFStringRef   cfStr : cfStr = NULL
dim as long          fileLen

if ( files$( _CFURLRefOpen, "TEXT", "Select text file...", @fileRef ) )
   open "i", 2, fileRef
   fileLen = lof( 2, 1 )
   h = fn NewHandleClear( fileLen )
      if ( h )
         read file 2, [h], fileLen
         close #2
         cfStr = fn CFStringCreateWithBytes( _kCFAllocatorDefault, #[h], fn GetHandleSize(h), _kCFStringEncodingMacRoman, _false )
         fn DisposeH( h )
      end if
else
// User canceled
end if

fn HIViewSetText( sConsoleHITextView, cfStr )
CFRelease( cfStr )
end fn

fn ReadTextFile

```



## Groovy

Solution:

```groovy
def lineMap = [:]
System.in.eachLine { line, i ->
    lineMap[i] = line
}
lineMap.each { println it }
```


```txt
$ groovy -e 'def lineMap = [:]
> System.in.eachLine { line, i ->
>     lineMap[i] = line
> }
> lineMap.each { println it }' <<EOF
>
> Whose woods these are I think I know
> His house is in the village tho'
> He will not see me stopping here
> To watch his woods fill up with snow
> EOF
```


```txt
1=
2=Whose woods these are I think I know
3=His house is in the village tho'
4=He will not see me stopping here
5=To watch his woods fill up with snow
```



## Haskell


The whole contents of a file can be read lazily. The standard functions ''lines'' and ''words'' convert that lazily into the lists of lines resp. words. Usually, one wouldn't use extra routines for that, but just use ''readFile'' and then put 'lines' or ''words'' somewhere in the next processing step.


```haskell
import System.IO

readLines :: Handle -> IO [String]
readLines h = do
  s <- hGetContents h
  return $ lines s

readWords :: Handle -> IO [String]
readWords h = do
  s <- hGetContents h
  return $ words s
```



## HicEst


```HicEst
CHARACTER name='myfile.txt', string*1000

   OPEN(FIle=name, OLD, LENgth=bytes, IOStat=errorcode, ERror=9)

   DO line = 1, bytes  ! loop terminates with end-of-file error at the latest
      READ(FIle=name, IOStat=errorcode, ERror=9) string
      WRITE(StatusBar) string
   ENDDO

 9 WRITE(Messagebox, Name) line, errorcode
```



## i


```i
software {
	loop {
		read()
		errors {
			exit
		}
	}
}
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
link str2toks
# call either words or lines depending on what you want to do.
procedure main()
   words()
end

procedure lines()
   while write(read())
end

procedure words()
   local line
   while line := read() do line ? every write(str2toks())
end
```

See [http://www.cs.arizona.edu/icon/library/src/procs/str2toks.icn str2toks]


## J

Script "read-input-until-eof.ijs":

```J
#!/Applications/j602/bin/jconsole
NB. read input until EOF
((1!:1) 3)(1!:2) 4
exit ''
```

Example:

```J
$ ./read-input-to-eof.ijs <<EOF
> abc
> def
> ghi
> now is the time for all good men ...
> EOF
abc
def
ghi
now is the time for all good men ...
```


## Java

Some people prefer <tt>Scanner</tt> or <tt>BufferedReader</tt>, so a way with each is presented.

```java
import java.io.InputStream;
import java.util.Scanner;

public class InputLoop {
    public static void main(String args[]) {
        // To read from stdin:
        InputStream source = System.in;

        /*
        Or, to read from a file:
        InputStream source = new FileInputStream(filename);

        Or, to read from a network stream:
        InputStream source = socket.getInputStream();
        */

        Scanner in = new Scanner(source);
        while(in.hasNext()){
            String input = in.next(); // Use in.nextLine() for line-by-line reading

            // Process the input here. For example, you could print it out:
            System.out.println(input);
        }
    }
}
```

Or

```java
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;

public class InputLoop {
    public static void main(String args[]) {
        // To read from stdin:
        Reader reader = new InputStreamReader(System.in);

        /*
        Or, to read from a file:
        Reader reader = new FileReader(filename);

        Or, to read from a network stream:
        Reader reader = new InputStreamReader(socket.getInputStream());
        */

        try {
            BufferedReader inp = new BufferedReader(reader);
            while(inp.ready()) {
                int input = inp.read(); // Use in.readLine() for line-by-line

                // Process the input here. For example, you can print it out.
                System.out.println(input);
            }
        }  catch (IOException e) {
            // There was an input error.
        }
    }
}
```



## JavaScript

These implementations of JavaScript define a <code>readline()</code> function, so:

```txt
$ js -e 'while (line = readline()) { do_something_with(line); }' < inputfile
```


As above, this operates on standard input

```javascript
var text_stream = WScript.StdIn;
var i = 0;

while ( ! text_stream.AtEndOfStream ) {
    var line = text_stream.ReadLine();
    // do something with line
    WScript.echo(++i + ": " + line);
}
```



## jq

The jq program for reading and writing is simply the one-character program:

    .

For example, to echo each line of text in a file, one could invoke jq as follows:
```jq
jq -r -R . FILENAME

```


If the input file consists of well-formed JSON entities (including scalars), then the following invocation could be used to "pretty-print" the input:
```jq>jq . FILENAME</lang


Other options, e.g. to emit JSON in compact form, also exist.


## Jsish


```javascript
/* Input loop in Jsish */

var line;
var cs = 0, ls = 0;

while (line = console.input()) { cs += line.length; ls += 1; }

printf("%d lines, %d characters\n", ls, cs);
```


```txt
prompt$ jsish input-loop.jsi <input-loop.jsi
8 lines, 159 characters
```



## Julia

We create a text stream and read the lines from the stream one by one, printing them on screen.
Note that the lines end by a newline, except the last one. The ending newlines are part of the strings returned by the function readline. Once the end of the stream is reached, readline returns an empty string.

```julia
stream = IOBuffer("1\n2\n3\n4\n\n6")

while !eof(stream)
    line = readline(stream)
    println(line)
end
```

```txt
1
2
3
4

6
```



## Kotlin


```scala
// version 1.1

import java.util.*

fun main(args: Array<String>) {
    println("Keep entering text or the word 'quit' to end the program:")
    val sc = Scanner(System.`in`)
    val words = mutableListOf<String>()
    while (true) {
        val input: String = sc.next()
        if (input.trim().toLowerCase() == "quit") {
            if (words.size > 0) println("\nYou entered the following words:\n${words.joinToString("\n")}")
            return
        }
        words.add(input)
    }
}
```

Sample input/output:
```txt

Keep entering text or the word 'quit' to end the program:
The quick brown fox
jumps over the lazy dog
quit

You entered the following words:
The
quick
brown
fox
jumps
over
the
lazy
dog

```



## Lang5


```lang5
: read-lines do read . "\n" . eof if break then loop ;
: ==>contents
    '< swap open 'fh set fh fin read-lines fh close ;

'file.txt ==>contents
```



## Lasso


```Lasso

local(
	myfile		= file('//path/to/file.txt'),
	textresult	= array
)

#myfile -> foreachline => {
	#textresult -> insert(#1)
}

#textresult -> join('<br />')
```


Result:
This is line one
I am the second line
Here is line 3


## Liberty BASIC


```lb

filedialog "Open","*.txt",file$
if file$="" then end
open file$ for input as #f
while not(eof(#f))
    line input #f, t$
    print t$
wend
close #f
end
```



## LIL

From the canread.lil sample that ships with LIL.


```tcl
#
# canread test (note that canread is not available in LIL/FPLIL itself
# but provided by the command line interfaces in main.c/lil.pas)
#
# You can either call this and enter lines directly (use Ctrl+Z/Ctrl+D
# to finish) or a redirect (e.g. lil canread.lil < somefile.txt)
#
# Normally this is how you are supposed to read multiple lines from
# the standard input using the lil executable.  For an alternative way
# that uses error catching via try see the eoferror.lil script.
#

set count 0
while {[canread]} {
    readline
    inc count
}
print $count lines
```


```txt
prompt$ lil canread.lil <canread.lil
19 lines
```



## Logo

There are several words which will return a line of input.
* readline - returns a line as a list of words
* readword - returns a line as a single word, or an empty list if it reached the end of file
* readrawline - returns a line as a single word, with no characters escaped

```logo
while [not eof?] [print readline]
```



## LSL

LSL doesn't have a File System, but it does have [http://wiki.secondlife.com/wiki/Category:LSL_Notecard Notecards] that function as read-only text files, and can be use as configuration files or data sources.

To test it yourself; rez a box on the ground, add the following as a New Script, create a notecard named "Input_Loop_Data_Source.txt", and put what ever data you want in it (in this case I just put a copy of the source code.)

```LSL
string sNOTECARD = "Input_Loop_Data_Source.txt";
default {
	integer iNotecardLine = 0;
	state_entry() {
		llOwnerSay("Reading '"+sNOTECARD+"'");
		llGetNotecardLine(sNOTECARD, iNotecardLine);
	}
	dataserver(key kRequestId, string sData) {
		if(sData==EOF) {
			llOwnerSay("EOF");
		} else {
			llOwnerSay((string)iNotecardLine+": "+sData);
			llGetNotecardLine(sNOTECARD, ++iNotecardLine);
		}
	}
}
```

```txt
Reading 'Input_Loop_Data_Source.txt'
0: string sNOTECARD = "Input_Loop_Data_Source.txt";
1: default {
2: 	integer iNotecardLine = 0;
3: 	state_entry() {
4: 		llOwnerSay("Reading '"+sNOTECARD+"'");
5: 		llGetNotecardLine(sNOTECARD, iNotecardLine);
6: 	}
7: 	dataserver(key kRequestId, string sData) {
8: 		if(sData==EOF) {
9: 			llOwnerSay("EOF");
10: 		} else {
11: 			llOwnerSay((string)iNotecardLine+": "+sData);
12: 			llGetNotecardLine(sNOTECARD, ++iNotecardLine);
13: 		}
14: 	}
15: }
EOF
```



## Lua


```lua
lines = {}
str = io.read()
while str do
    table.insert(lines,str)
    str = io.read()
end
```



###  Via generic for loop

Reads line-by-line via an iterator (from stdin). Substitute <code>io.lines()</code> with <code>io.open(filename, "r"):lines()</code> to read from a file.


```lua
lines = {}

for line in io.lines() do
    table.insert(lines, line) -- add the line to the list of lines
end
```



## M2000 Interpreter


```M2000 Interpreter

Document A$={1st Line
      2nd line
      3rd line
      }
Save.Doc A$, "test.txt", 0   ' 0 for utf16-le
\\ we use Wide for utf16-le \\ without it we open using ANSI
Open "test.txt" For Wide Input Exclusive as #N
      While Not Eof(#N) {
            Line Input #N, ThisLine$
            Print ThisLine$
      }
Close #N
Clear A$
Load.Doc A$, "test.txt"
\\ print proportional text, all lines
Report A$
\\ Print one line, non proportional
\\ using paragraphs
For i=0 to Doc.par(A$)-1
      Print Paragraph$(A$, i)
Next i
\\ List of current variables (in any scope, public only)
List

```



## Maple


```Maple

readinput:=proc(filename)
local line,file;
file:="";
line:=readline(filename);
while line<>0 do
line:=readline(filename);
file:=cat(file,line);
end do;
end proc;

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
stream = OpenRead["file.txt"];
While[a != EndOfFile, Read[stream, Word]];
Close[stream]
```



## MAXScript

this function will read a file line by line.

```MAXScript
fn ReadAFile FileName =
(
	local in_file = openfile FileName
	while not eof in_file do
	(
		--Do stuff in here--
		print (readline in_file)
	)
	close in_file
)
```



## Mercury

<lang>
:- module input_loop.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.stdin_stream(Stdin, !IO),
    io.stdout_stream(Stdout, !IO),
    read_and_print_lines(Stdin, Stdout, !IO).

:- pred read_and_print_lines(io.text_input_stream::in,
    io.text_output_stream::in, io::di, io::uo) is det.

read_and_print_lines(InFile, OutFile, !IO) :-
    io.read_line_as_string(InFile, Result, !IO),
    (
        Result = ok(Line),
        io.write_string(OutFile, Line, !IO),
        read_and_print_lines(InFile, OutFile, !IO)
    ;
        Result = eof
    ;
        Result = error(IOError),
        Msg = io.error_message(IOError),
        io.stderr_stream(Stderr, !IO),
        io.write_string(Stderr, Msg, !IO),
        io.set_exit_status(1, !IO)
    ).

```



## mIRC Scripting Language


```mirc
var %n = 1
while (%n <= $lines(input.txt)) {
  write output.txt $read(input.txt,%n)
  inc %n
}
```



## ML/I

The very nature of ML/I is that its default behaviour
is to copy from input to output until it reaches end of file.

```ML/I></lang


=={{header|Modula-2}}==

```modula2
PROCEDURE ReadName (VAR str : ARRAY OF CHAR);

VAR     n               : CARDINAL;
        ch, endch       : CHAR;

BEGIN
   REPEAT
      InOut.Read (ch);
      Exhausted := InOut.EOF ();
      IF  Exhausted  THEN  RETURN  END
   UNTIL  ch > ' ';             (* Eliminate whitespace         *)
   IF  ch = '['  THEN  endch := ']'  ELSE  endch := ch  END;
   n := 0;
   REPEAT
      InOut.Read (ch);
      Exhausted := InOut.EOF ();
      IF  Exhausted  THEN  RETURN  END;
      IF  n <= HIGH (str)  THEN  str [n] := ch  ELSE  ch := endch  END;
      INC (n)
   UNTIL ch = endch;
   IF  n <= HIGH (str)  THEN  str [n-1] := 0C  END;
   lastCh := ch
END ReadName;
```



=={{header|Modula-3}}==

```modula3
MODULE Output EXPORTS Main;

IMPORT Rd, Wr, Stdio;

VAR buf: TEXT;

<*FATAL ANY*>

BEGIN
  WHILE NOT Rd.EOF(Stdio.stdin) DO
    buf := Rd.GetLine(Stdio.stdin);
    Wr.PutText(Stdio.stdout, buf);
  END;
END Output.
```



## NetRexx


###  Using NetRexx <tt>ASK</tt> Special Variable


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

-- Read from default input stream (console) until end of data
lines = ''
lines[0] = 0
lineNo = 0

loop label ioloop forever
  inLine = ask
  if inLine =  null  then leave ioloop -- stop on EOF (Try Ctrl-D on UNIX-like systems or Ctrl-Z on Windows)
  lineNo = lineNo + 1
  lines[0] = lineNo
  lines[lineNo] = inLine
  end ioloop

loop l_ = 1 to lines[0]
  say l_.right(4)':' lines[l_]
  end l_

return

```



###  Using Java <tt>Scanner</tt>


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

-- Read from default input stream (console) until end of data
lines = ''
lines[0] = 0

inScanner = Scanner(System.in)
loop l_ = 1 while inScanner.hasNext()
  inLine = inScanner.nextLine()
  lines[0] = l_
  lines[l_] = inLine
  end l_
inScanner.close()

loop l_ = 1 to lines[0]
  say l_.right(4)':' lines[l_]
  end l_

return

```



## Nim


```nim
var i = open("input.txt")
for line in i.lines:
  discard # process line
i.close()
```


=={{header|Oberon-2}}==
Works with oo2c Version 2

```oberon2

MODULE InputLoop;
IMPORT
  StdChannels,
  Channel;
VAR
  reader: Channel.Reader;
  writer: Channel.Writer;
  c: CHAR;
BEGIN
  reader := StdChannels.stdin.NewReader();
  writer := StdChannels.stdout.NewWriter();

  reader.ReadByte(c);
  WHILE reader.res = Channel.done DO
    writer.WriteByte(c);
    reader.ReadByte(c)
  END
END InputLoop.

```

Execute: InputLoop &lt; Inputloop.Mod<br/>
Output:

```txt

MODULE InputLoop;
IMPORT
  StdChannels,
  Channel;
VAR
  reader: Channel.Reader;
  writer: Channel.Writer;
  c: CHAR;
BEGIN
  reader := StdChannels.stdin.NewReader();
  writer := StdChannels.stdout.NewWriter();

  reader.ReadByte(c);
  WHILE reader.res = Channel.done DO
    writer.WriteByte(c);
    reader.ReadByte(c)
  END
END InputLoop.

```


## Objeck


```objeck

use IO;

bundle Default {
  class Test {
    function : Main(args : System.String[]) ~ Nil {
      f := FileReader->New("in.txt");
      if(f->IsOpen()) {
        string := f->ReadString();
        while(string->Size() > 0) {
          string->PrintLine();
          string := f->ReadString();
        };
        f->Close();
      };
    }
  }
}

```



## OCaml


```ocaml
let rec read_lines ic =
  try let line = input_line ic in
    line :: read_lines ic
  with End_of_file ->
    []
```


The version above will work for small files, but it is not tail-recursive. <br />
Below will be more scalable:


```ocaml
let read_line ic =
  try Some (input_line ic)
  with End_of_file -> None

let read_lines ic =
  let rec loop acc =
    match read_line ic with
    | Some line -> loop (line :: acc)
    | None -> List.rev acc
  in
  loop []
;;
```


Or with a higher order function:


```ocaml
let read_lines f ic =
  let rec loop () =
    try f(input_line ic); loop()
    with End_of_file -> ()
  in
  loop()

read_lines print_endline (open_in Sys.argv.(1))
```


This function will apply your_function() to every line of input

```ocaml

let rec input_caller () =
  let input = read_line() in
  your_function input ;
  input_caller() ;
;;

let () = input_caller()
```



## Oforth


Reads a file line by line and write each line on standard output :


```Oforth
: readFile(filename)   File new(filename) apply(#println) ;
```



## Oz


```oz
%% Returns a list of lines.
%% Text: an instance of Open.text (a mixin class)
fun {ReadAll Text}
   case {Text getS($)} of false then nil
   [] Line then Line|{ReadAll Text}
   end
end
```



## Pascal



```pascal
{ for stdio }

var

 s : string ;

begin

  repeat

    readln(s);

  until s = "" ;

{ for a file }

var

 f : text ;
 s : string ;

begin

  assignfile(f,'foo');
  reset(f);

  while not eof(f) do
    readln(f,s);

  closefile(f);

end;
```



## Perl

The angle brackets operator ( <tt><...></tt> ) reads one line at a time from a filehandle in scalar context:

```perl
open FH, "< $filename" or die "can't open file: $!";
while (my $line = <FH>) {
    chomp $line; # removes trailing newline
    # process $line
}
close FH or die "can't close file: $!";
```


Or you can get a list of all lines when you use it in list context:

```perl>@lines = <FH>;</lang


Or a simpler program for lines of files entered as command line arguments or standard input:

```perl
while (<>) {
    # $_ contains a line
}
```


Invoking perl with the -p or -n option implies the above loop, executing its code once per input line, with the line stored in $_. -p will print $_ automatically at the end of each iteration, -n will not.


```txt
$ seq 5 | perl -pe '$_ = "Hello $_"'
Hello 1
Hello 2
Hello 3
Hello 4
Hello 5
$ seq 5 | perl -ne 'print "Hello $_"'
Hello 1
Hello 2
Hello 3
Hello 4
Hello 5
```



## Perl 6


In Perl 6, filehandles etc. provide the <code>.lines</code> and <code>.words</code> methods which return lazy lists, and can thus they be iterated using a <code>for</code> loop...

'''Line-by-line''' <small>''(line endings are automatically stripped)''</small>

*From a file:
```perl6
for "filename.txt".IO.lines -> $line {
    ...
}
```

*From standard input:
```perl6
for $*IN.lines -> $line {
    ...
}
```

*From a pipe:
```perl6
for run(«find -iname *.txt», :out).out.lines -> $filename {
    ...
}
```

*From a pipe, with custom line separator <small>''(in this example to handle filenames containing newlines)''</small>:
```perl6
for run(«find -iname *.txt -print0», :nl«\0», :out).out.lines -> $filename {
    ...
}
```


'''Word-by-word'''
*From a file
```perl6
for "filename.txt".IO.words -> $word {
    ...
}
```

*From standard input or a pipe, accordingly.


## Phix

Process text stream line-by-line:

```Phix
procedure process_line_by_line(integer fn)
    object line
    while 1 do
        line = gets(fn)
        if atom(line) then
            exit
        end if
        -- process the line
    end while
end procedure
```



## PHP


```php
$fh = fopen($filename, 'r');
if ($fh) {
    while (!feof($fh)) {
        $line = rtrim(fgets($fh)); # removes trailing newline
        # process $line
    }
    fclose($fh);
}
```


Or you can get an array of all the lines in the file:

```php
$lines = file($filename);
```


Or you can get the entire file as a string:

```php
$contents = file_get_contents($filename);
```



## PicoLisp

This reads all lines in a file, and returns them as a list of lists

```PicoLisp
(in "file.txt"
   (make
      (until (eof)
         (link (line)) ) ) )
```



## PL/I


```pli
declare line character (200) varying;

open file (in) title ('/TEXT.DAT,type(text),recsize(200)' );
on endfile (in) stop;

do forever;
   get file(in) edit (line) (L);
   put skip list (line);
end;
```



## PowerShell


```Powershell
Get-Content c:\file.txt |
    ForEach-Object {
        $_
    }
```

or

```Powershell
ForEach-Object -inputobject (get-content c:\file.txt) {$_}
```



## PureBasic

File objects can be read bytewise, characterwise (ASCII or UNICODE), floatwise, doublewise, integerwise, linewise ...

```PureBasic
If OpenConsole()
  ; file based line wise
  If ReadFile(0, "Text.txt")
    While Eof(0) = 0
      Debug ReadString(0)      ; each line until eof
    Wend
    CloseFile(0)
  EndIf

  ; file based byte wise
  If ReadFile(1, "Text.bin")
    While Eof(1) = 0
      Debug ReadByte(1)      ; each byte until eof
    Wend
    CloseFile(1)
  EndIf
EndIf
```



## Python


To create a Python3 input loop use python's `input()` function.


```python
while(True):
      x = input("What is your age? ")
      print(x)
```


Python file objects can be iterated like lists:


```python
my_file = open(filename, 'r')
try:
    for line in my_file:
        pass # process line, includes newline
finally:
    my_file.close()
```


One can open a new stream for read and have it automatically close when done, with a new "with" statement:

```python
from __future__ import with_statement

with open(filename, 'r') as f:
    for line in f:
        pass # process line, includes newline
```


You can also get lines manually from a file:

```python
line = my_file.readline() # returns a line from the file
lines = my_file.readlines() # returns a list of the rest of the lines from the file
```

This does not mix well with the iteration, however.


When you want to read from stdin, or (multiple) filenames are given on the command line:

```python
import fileinput
for line in fileinput.input():
    pass # process line, includes newline
```

The fileinput module can also do inplace file editing, follow line counts, and the name of the current file being read etc.


## R

Note that read.csv and read.table provide alternatives for files with 'dataset' style contents.

```rsplus
lines <- readLines("file.txt")
```



## Racket


The following prints input lines from standard input to standard output:

```racket

#lang racket
(copy-port (current-input-port) (current-output-port))

```



## REBOL


```REBOL
REBOL [
	Title: "Basic Input Loop"
	URL: http://rosettacode.org/wiki/Basic_input_loop
]

; Slurp the whole file in:
x: read %file.txt

; Bring the file in by lines:
x: read/lines %file.txt

; Read in first 10 lines:
x: read/lines/part %file.txt 10

; Read data a line at a time:
f: open/lines %file.txt
while [not tail? f][
	print f/1
	f: next f ; Advance to next line.
]
close f
```



## REXX


### version 1a

Reading line by line from the standard input using <tt>linein</tt> and <tt>lines</tt> did not work.

```rexx
do while stream(stdin, "State") <> "NOTREADY"
  call charout ,charin(stdin)
end
```



### version 1b

Apparently only lines() does not work

```rexx
Do Until input=''
  input=linein(stdin)
  Call lineout ,input
  End
```



### version 2

```rexx
/* -- AREXX -- */
do until eof(stdin)
  l = readln(stdin)
  say l
end
```



### version 3

Note that if REXX is reading from the default (console) input stream, there is no well-
defined e-o-f (end of file), so to speak.

Therefore, the following two REXX programs use the presence of a null line to indicate e-o-f.

```rexx
/*REXX program reads from the (console) default input stream until null*/
       do  until _==''
       parse pull _
       end   /*until*/                 /*stick a fork in it, we're done.*/
```



### version 4


```rexx
/*REXX program reads from the (console) default input stream until null*/
       do  until _==''
       _= linein()
       end   /*until*/                 /*stick a fork in it, we're done.*/
```



## Ring


```ring

fp = fopen("C:\Ring\ReadMe.txt","r")

r = fgetc(fp)
while isstring(r)
      r = fgetc(fp)
      if r = char(10) see nl
      else see r ok
end
fclose(fp)


```



## Ruby

Ruby input streams are IO objects. One can use IO#each or IO#each_line to iterate lines from a stream.


```ruby
stream = $stdin
stream.each do |line|
  # process line
end
```


IO objects are also Enumerable (like Array or Range), and have methods like Enumerable#map, which call IO#each to loop through lines from a stream.


```ruby
# Create an array of lengths of every line.
ary = stream.map {|line| line.chomp.length}
```


''To open a new stream for reading, see [[Read a file line by line#Ruby]].''



## Run BASIC


```runbasic
open "\testFile.txt" for input as #f
while not(eof(#f))
line input #f, a$
print a$
wend
close #f
```



## Scala

```scala
  scala.io.Source.fromFile("input.txt").getLines().foreach {
    line => ... }
```



## Rust


```Rust
 use std::io::{self, BufReader, Read, BufRead};
use std::fs::File;

fn main() {
    print_by_line(io::stdin())
        .expect("Could not read from stdin");

    File::open("/etc/fstab")
        .and_then(print_by_line)
        .expect("Could not read from file");
}

fn print_by_line<T: Read>(reader: T) -> io::Result<()> {
    let buffer = BufReader::new(reader);
    for line in buffer.lines() {
        println!("{}", line?)
    }
    Ok(())
}
```



## Slate


```slate
(File newNamed: 'README') reader sessionDo: [| :input | input lines do: [| :line | inform: line]].
```



## sed

Sed by default loops over each line and executes its given script on it:


```txt
$ seq 5 | sed ''
1
2
3
4
5
```


The automatic printing can be suppressed with -n,
and performed manually with p:


```txt
$ seq 5 | sed -n p
1
2
3
4
5
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var string: line is "";
  begin
    while hasNext(IN) do
      readln(line);
      writeln("LINE: " <& line);
    end while;
  end func;
```



## Sidef

To read from the standard input, you can use '''STDIN''' as your '''fh'''.

```ruby
var file = File(__FILE__)
file.open_r(\var fh, \var err) || die "#{file}: #{err}"

fh.each { |line|              # iterates the lines of the fh
    line.each_word { |word|   # iterates the words of the line
        say word
    }
}
```



## Smalltalk


```smalltalk
|f|
f := FileStream open: 'afile.txt' mode: FileStream read.
[ f atEnd ] whileFalse: [ (f nextLine) displayNl ] .
```



## SNOBOL4


```snobol
loop  output = input :s(loop)
end
```



## Sparkling


```sparkling
var line;
while (line = getline()) != nil {
    print(line);
}
```



## Tcl


```tcl
set fh [open $filename]
while {[gets $fh line] != -1} {
    # process $line
}
close $fh
```

For “small” files, it is often more common to do this:

```tcl
set fh [open $filename]
set data [read $fh]
close $fh
foreach line [split $data \n] {
    # process line
}
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
file="a.txt"
ERROR/STOP OPEN (file,READ,-std-)
ACCESS source: READ/RECORDS/UTF8 $file s,text
LOOP
    READ/NEXT/EXIT source
    PRINT text
ENDLOOP
ENDACCESS source

```



## UnixPipes

the pipe 'yes XXX' produces a sequence

read by lines:

```bash
yes 'A B C D ' | while read x ; do echo -$x- ; done
```

read by words:

```bash
yes 'A B C D ' | while read -d\  a ; do echo -$a- ; done
```



## UNIX Shell

When there is something to do with the input, here is a loop:

```bash
while read line ; do
  # examine or do something to the text in the "line" variable
  echo "$line"
done
```

The following echoes standard input to standard output line-by-line until the end of the stream.

```bash>cat < /dev/stdin > /dev/stdout</lang

Since <code>cat</code> defaults to reading from standard input and writing to standard output, this can be further simplified to the following.

```bash>cat</lang



## Ursa


```ursa
decl file f
f.open "filename.txt"
while (f.hasnext)
	out (in string f) endl console
end while
```



## Vala


```vala
int main() {
        string? s;
        while((s = stdin.read_line()) != null) {
                stdout.printf("%s\n", s);
        }
        return 0;
}
```



## VBA


```vb
Public Sub test()
    Dim filesystem As Object, stream As Object, line As String
    Set filesystem = CreateObject("Scripting.FileSystemObject")
    Set stream = filesystem.OpenTextFile("D:\test.txt")
    Do While stream.AtEndOfStream <> True
        line = stream.ReadLine
        Debug.Print line
    Loop
    stream.Close
End Sub
```


## VBScript


```vb

filepath = "SPECIFY PATH TO TEXT FILE HERE"

Set objFSO = CreateObject("Scripting.FileSystemObject")
Set objInFile = objFSO.OpenTextFile(filepath,1,False,0)

Do Until objInFile.AtEndOfStream
	line = objInFile.ReadLine
	WScript.StdOut.WriteLine line
Loop

objInFile.Close
Set objFSO = Nothing

```



## Visual Basic .NET


This reads a stream line by line, outputing each line to the screen.


```vbnet
Sub Consume(ByVal stream As IO.StreamReader)
    Dim line = stream.ReadLine
    Do Until line Is Nothing
        Console.WriteLine(line)
        line = stream.ReadLine
    Loop
End Sub
```



## x86 Assembly


'''GAS, 64 bit (Linux)''': Compiled with <code>gcc -nostdlib</code>. Memory maps the file and outputs one line at a time. Try <code>./a.out file</code>, <code>./a.out < file</code>, or <code>./a.out <<< "Heredoc"</code>. It's a little like cat, but less functional.

```x86
#define SYS_WRITE   $1
#define SYS_OPEN    $2
#define SYS_CLOSE   $3
#define SYS_FSTAT   $5
#define SYS_MMAP    $9
#define SYS_MUNMAP  $11
#define SYS_EXIT    $60

// From experiments:
#define FSIZEOFF    48
#define STATSIZE    144

// From Linux source:
#define RDONLY      $00
#define PROT_READ   $0x1
#define MAP_PRIVATE $0x02
#define STDIN       $0
#define STDOUT      $1

.global _start
.text

/* Details: */
/*
    Remember:  %rax(%rdi, %rsi, %rdx, %r10, %r8, %r9)
    - Open a file (get its fd)
        - int fd = open("filename", RDONLY)
    - Get its filesize:
        - fstat(fd, statstruct). 0 if ok. fsize at statstruct+48
    - Then memory map it.
        - void* vmemptr = mmap(vmemptr, fsize, PROT_READ, MAP_PRIVATE, fd, 0)
    - Scan for newlines, print line.
        - Keep going until done. Details at 11.
    - Unmap memory
        - munmap(vmemptr, filesize). 0 if ok.
    - Exit
 */

.macro ERRCHECK code
    cmpq    $\code, %rax
    je      fs_error
.endm

/* Local stack notes:
    0: int fd
    4: void* vmemptr
    12: void* head
    20: void* lookahead
    28: void* end
*/
_start:
    // Open:
    movq    RDONLY, %rsi
    // Filename ptr is on stack currently as argv[1]:
    cmpq    $1, (%rsp)          // if argc is 1, default to stdin
    jnz     open_file
    subq    $36, %rsp           // local stack
    movl    STDIN, (%rsp)
    jmp     fstat

    open_file:
    movq    16(%rsp), %rdi      // argc(8), argv0(8) => rsp+16. filename
    movq    SYS_OPEN, %rax
    syscall
    ERRCHECK    -1
    subq    $36, %rsp           // local stack
    movl    %eax, (%rsp)        // int fd = open(argv[1], RDONLY)

    // fstat to get filesize
    fstat:
    movq    $statstruct, %rsi
    movl    (%rsp), %edi        // fd
    movq    SYS_FSTAT, %rax
    syscall                     // fstat(fd, statstruct)
    ERRCHECK    -1

    // mmap - don't forget to munmap.
    mmap:
    movq    $0, %r9             // offset
    movq    (%rsp), %r8         // fd
    movq    MAP_PRIVATE, %r10
    movq    PROT_READ, %rdx
    movq    filesize, %rsi
    movq    (%rsp), %rdi        // vmemptr
    movq    SYS_MMAP, %rax
    syscall
    ERRCHECK    -1
    movq    %rax, 4(%rsp)       // void* vmemptr = mmap(vmemptr, fsize, PROT_READ, MAP_PRIVATE, fd, 0)

    /* Print lines */
    movq    %rax, 12(%rsp)  // head = vmemptr
    addq    filesize, %rax
    decq    %rax
    movq    %rax, 28(%rsp)  // end = vmemptr+filesize-1
    scan_outer:
        movq    12(%rsp), %rax
        cmpq    28(%rsp), %rax
        jge     cleanup         // if head >= end, done
        movq    %rax, %rbx      // Using rbx as lookahead
        scan_inner:
            cmpq    28(%rsp), %rbx
            jge     writeline       // if lookahead >= end, write the line.
            cmpb    $'\n, (%rbx)
            jz      writeline       // if '\n'==*lookahead, write the line
            incq    %rbx
            jmp     scan_inner
        writeline:
            // write:
            incq    %rbx
            movq    %rbx, %rdx
            subq    12(%rsp), %rdx  // rdx <- lookahead-head
            movq    12(%rsp), %rsi
            movq    STDOUT, %rdi
            movq    SYS_WRITE, %rax
            syscall                 // write(stdout, head, lookahead-head)
            safety:
            movq    %rbx, 12(%rsp)  // head = lookahead.
            jmp     scan_outer

    cleanup:
    // munmap
    movq    filesize, %rsi
    movq    4(%rsp), %rdi
    movq    SYS_MUNMAP, %rax
    syscall                     // munmap(vmemptr, filesize)
    cmpq    $-1, %rax
    je      fs_error
    // close
    movl    (%rsp), %edi
    movq    SYS_CLOSE, %rax
    syscall                     // close(fd)
    ERRCHECK    -1

exit:
    movq    SYS_EXIT, %rax
    xorq    %rdi, %rdi              // The exit code.
    syscall

fs_error:
    movq    SYS_EXIT, %rax
    movq    $-1, %rdi
    syscall                         // exit(-1)

.data
statstruct:     // This struct is 144 bytes. Only want size (+48)
    .zero FSIZEOFF
    filesize:  // 8 bytes.
    .quad   0
    .zero   STATSIZE-FSIZEOFF+8
```



## zkl

Many objects support "stream of" concepts such as lines, characters, chunks.
Some are File, Data (bit bucket), List, Console.
Word by word isn't explicitly supported.
If an object is stream-able, it supports methods like foreach, pump, apply, reduce, etc.

```zkl
foreach line in (File("foo.txt")){...}
List(1,2,3).readln() // here, a "line" is a list element
Utils.Helpers.zipWith(False, // enumerate a file
   fcn(n,line){"%3d: %s".fmt(n,line).print()},[1..],File("cmp.zkl"))
```

