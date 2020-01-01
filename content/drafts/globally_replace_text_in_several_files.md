+++
title = "Globally replace text in several files"
description = ""
date = 2019-04-05T10:18:53Z
aliases = []
[extra]
id = 8642
[taxonomies]
categories = []
tags = []
+++

{{task}}

;Task:
Replace every occurring instance of a piece of text in a group of text files with another one.


For this task we want to replace the text   "'''Goodbye London!'''"   with   "'''Hello New York!'''"   for a list of files.





## Ada



```Ada
with Ada.Strings.Unbounded, Ada.Text_IO, Ada.Command_Line, Ada.Directories;

procedure Global_Replace is

   subtype U_String is Ada.Strings.Unbounded.Unbounded_String;
   function "+"(S: String) return U_String renames
     Ada.Strings.Unbounded.To_Unbounded_String;
   function "-"(U: U_String) return String renames
     Ada.Strings.Unbounded.To_String;

   procedure String_Replace(S: in out U_String; Pattern, Replacement: String) is
      -- example: if S is "Mary had a XX lamb", then String_Replace(S, "X", "little");
      --          will turn S into "Mary had a littlelittle lamb"
      --          and String_Replace(S, "Y", "small"); will not change S

      Index : Natural;
   begin
      loop
         Index := Ada.Strings.Unbounded.Index(Source => S, Pattern => Pattern);
         exit when Index = 0;
         Ada.Strings.Unbounded.Replace_Slice
           (Source => S, Low => Index, High => Index+Pattern'Length-1,
            By => Replacement);
      end loop;
   end String_Replace;

   procedure File_Replace(Filename: String; Pattern, Replacement: String) is
      -- applies String_Rplace to each line in the file with the given Filename
      -- propagates any exceptions, when, e.g., the file does not exist

      I_File, O_File: Ada.Text_IO.File_Type;
      Line: U_String;
      Tmp_Name: String := Filename & ".tmp";
         -- name of temporary file; if that file already exists, it will be overwritten
   begin
      Ada.Text_IO.Open(I_File, Ada.Text_IO.In_File, Filename);
      Ada.Text_IO.Create(O_File, Ada.Text_IO.Out_File, Tmp_Name);
      while not Ada.Text_IO.End_Of_File(I_File) loop
         Line := +Ada.Text_IO.Get_Line(I_File);
         String_Replace(Line, Pattern, Replacement);
         Ada.Text_IO.Put_Line(O_File, -Line);
      end loop;
      Ada.Text_IO.Close(I_File);
      Ada.Text_IO.Close(O_File);
      Ada.Directories.Delete_File(Filename);
      Ada.Directories.Rename(Old_Name => Tmp_Name, New_Name => Filename);
   end File_Replace;

   Pattern:     String := Ada.Command_Line.Argument(1);
   Replacement: String :=  Ada.Command_Line.Argument(2);

begin
   Ada.Text_IO.Put_Line("Replacing """ & Pattern
                          & """ by """ & Replacement & """ in"
                          & Integer'Image(Ada.Command_Line.Argument_Count - 2)
                          & " files.");
   for I in 3 .. Ada.Command_Line.Argument_Count loop
      File_Replace(Ada.Command_Line.Argument(I), Pattern, Replacement);
   end loop;
end Global_Replace;
```


Ouput:

```txt
> ls ?.txt
1.txt  2.txt  x.txt  y.txt

> more 2.txt
This is a text.
"Goodbye London!"
"Goodbye London!"
"Byebye London!" "Byebye London!" "Byebye London!"

> ./global_replace "Goodbye London" "Hello New York" ?.txt
Replacing "Goodbye London" by "Hello New York" in 4 files.

> more 2.txt
This is a text.
"Hello New York!"
"Hello New York!"
"Byebye London!" "Byebye London!" "Byebye London!"
```



## AutoHotkey


```AutoHotkey
SetWorkingDir %A_ScriptDir%      ; Change the working directory to the script's location
listFiles := "a.txt|b.txt|c.txt" ; Define a list of files in the current working directory
loop, Parse, listFiles, |
{
	; The above parses the list based on the | character
	fileread, contents, %A_LoopField% ; Read the file
	fileDelete, %A_LoopField%         ; Delete the file
	stringReplace, contents, contents, Goodbye London!, Hello New York!, All ; replace all occurrences
	fileAppend, %contents%, %A_LoopField% ; Re-create the file with new contents
}

```



## AWK


```AWK

# syntax: GAWK -f GLOBALLY_REPLACE_TEXT_IN_SEVERAL_FILES.AWK filename(s)
BEGIN {
    old_text = "Goodbye London!"
    new_text = "Hello New York!"
}
BEGINFILE {
    nfiles_in++
    text_found = 0
    delete arr
}
{   if (gsub(old_text,new_text,$0) > 0) {
      text_found++
    }
    arr[FNR] = $0
}
ENDFILE {
    if (text_found > 0) {
      nfiles_out++
      close(FILENAME)
      for (i=1; i<=FNR; i++) {
        printf("%s\n",arr[i]) >FILENAME
      }
    }
}
END {
    printf("files: %d read, %d updated\n",nfiles_in,nfiles_out)
    exit(0)
}

```


{{works with|gawk}}

```awk
@include "readfile"
BEGIN {
    while(++i < ARGC)
        print gensub("Goodbye London!","Hello New York!","g", readfile(ARGV[i])) > ARGV[i]
}
```



## BASIC

{{works with|FreeBASIC}}

Pass the files on the command line (i.e. <code>global-replace *.txt</code>).


```qbasic
CONST matchtext = "Goodbye London!"
CONST repltext  = "Hello New York!"
CONST matchlen  = LEN(matchtext)

DIM L0 AS INTEGER, x AS INTEGER, filespec AS STRING, linein AS STRING

L0 = 1
WHILE LEN(COMMAND$(L0))
    filespec = DIR$(COMMAND$(L0))
    WHILE LEN(filespec)
        OPEN filespec FOR BINARY AS 1
            linein = SPACE$(LOF(1))
            GET #1, 1, linein
            DO
                x = INSTR(linein, matchtext)
                IF x THEN
                    linein = LEFT$(linein, x - 1) & repltext & MID$(linein, x + matchlen)
                    ' If matchtext and repltext are of equal length (as in this example)
                    ' then you can replace the above line with this:
                    ' MID$(linein, x) = repltext
                    ' This is somewhat more efficient than having to rebuild the string.
                ELSE
                    EXIT DO
                END IF
            LOOP
        ' If matchtext and repltext are of equal length (as in this example), or repltext
        ' is longer than matchtext, you could just write back to the file while it's open
        ' in BINARY mode, like so:
        ' PUT #1, 1, linein
        ' But since there's no way to reduce the file size via BINARY and PUT, we do this:
        CLOSE
        OPEN filespec FOR OUTPUT AS 1
            PRINT #1, linein;
        CLOSE
        filespec = DIR$
    WEND
    L0 += 1
WEND
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      FindThis$ = "Goodbye London!"
      ReplaceWith$ = "Hello New York!"

      DIM Files$(3)
      Files$() = "C:\test1.txt", "C:\test2.txt", "C:\test3.txt", "C:\test4.txt"

      FOR f% = 0 TO DIM(Files$(),1)
        infile$ = Files$(f%)
        infile% = OPENIN(infile$)
        IF infile%=0 ERROR 100, "Failed to open file " + infile$
        tmpfile$ = @tmp$+"replace.txt"
        tmpfile% = OPENOUT(tmpfile$)

        WHILE NOT EOF#infile%
          INPUT #infile%, a$
          IF ASCa$=10 a$ = MID$(a$,2)
          l% = LEN(FindThis$)
          REPEAT
            here% = INSTR(a$, FindThis$)
            IF here% a$ = LEFT$(a$,here%-1) + ReplaceWith$ + MID$(a$,here%+l%)
          UNTIL here% = 0
          PRINT #tmpfile%, a$ : BPUT #tmpfile%,10
        ENDWHILE
        CLOSE #infile%
        CLOSE #tmpfile%

        OSCLI "DEL """ + infile$ + """"
        OSCLI "REN """ + tmpfile$ + """ """ + infile$ + """"
      NEXT
      END
```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <err.h>
#include <string.h>

char * find_match(const char *buf, const char * buf_end, const char *pat, size_t len)
{
	ptrdiff_t i;
	char *start = buf;
	while (start + len < buf_end) {
		for (i = 0; i < len; i++)
			if (start[i] != pat[i]) break;

		if (i == len) return (char *)start;
		start++;
	}
	return 0;
}

int replace(const char *from, const char *to, const char *fname)
{
#define bail(msg) { warn(msg" '%s'", fname); goto done; }
	struct stat st;
	int ret = 0;
	char *buf = 0, *start, *end;
	size_t len = strlen(from), nlen = strlen(to);
	int fd = open(fname, O_RDWR);

	if (fd == -1) bail("Can't open");
	if (fstat(fd, &st) == -1) bail("Can't stat");
	if (!(buf = malloc(st.st_size))) bail("Can't alloc");
	if (read(fd, buf, st.st_size) != st.st_size) bail("Bad read");

	start = buf;
	end = find_match(start, buf + st.st_size, from, len);
	if (!end) goto done; /* no match found, don't change file */

	ftruncate(fd, 0);
	lseek(fd, 0, 0);
	do {
		write(fd, start, end - start);	/* write content before match */
		write(fd, to, nlen);		/* write replacement of match */
		start = end + len;		/* skip to end of match */
						/* find match again */
		end = find_match(start, buf + st.st_size, from, len);
	} while (end);

	/* write leftover after last match */
	if (start < buf + st.st_size)
		write(fd, start, buf + st.st_size - start);

done:
	if (fd != -1) close(fd);
	if (buf) free(buf);
	return ret;
}

int main()
{
	const char *from = "Goodbye, London!";
	const char *to   = "Hello, New York!";
	const char * files[] = { "test1.txt", "test2.txt", "test3.txt" };
	int i;

	for (i = 0; i < sizeof(files)/sizeof(char*); i++)
		replace(from, to, files[i]);

	return 0;
}
```



## C++


```cpp
#include <fstream>
#include <iterator>
#include <boost/regex.hpp>
#include <string>
#include <iostream>

int main( int argc , char *argv[ ] ) {
   boost::regex to_be_replaced( "Goodbye London\\s*!" ) ;
   std::string replacement( "Hello New York!" ) ;
   for ( int i = 1 ; i < argc ; i++ ) {
      std::ifstream infile ( argv[ i ] ) ;
      if ( infile ) {
	 std::string filetext( (std::istreambuf_iterator<char>( infile )) ,
	       std::istreambuf_iterator<char>( ) ) ;
	 std::string changed ( boost::regex_replace( filetext , to_be_replaced , replacement )) ;
	 infile.close( ) ;
	 std::ofstream outfile( argv[ i ] , std::ios_base::out | std::ios_base::trunc ) ;
	 if ( outfile.is_open( ) ) {
	    outfile << changed ;
	    outfile.close( ) ;
	 }
      }
      else
	 std::cout << "Can't find file " << argv[ i ] << " !\n" ;
   }
   return 0 ;
}
```


Modern C++ version:

```cpp
#include <regex>
#include <fstream>

using namespace std;
using ist = istreambuf_iterator<char>;
using ost = ostreambuf_iterator<char>;

int main(){
    auto from = "Goodbye London!", to = "Hello New York!";
    for(auto filename : {"a.txt", "b.txt", "c.txt"}) {
        ifstream infile {filename};
        string content {ist {infile}, ist{}};
        infile.close();
        ofstream outfile {filename};
        regex_replace(ost {outfile}, begin(content), end(content), regex {from}, to);
    }
    return 0;
}

```



## C sharp


```csharp

using System.Collections.Generic;
using System.IO;

class Program {
    static void Main() {
        var files = new List<string> {
            "test1.txt",
            "test2.txt"
        };
        foreach (string file in files) {
            File.WriteAllText(file, File.ReadAllText(file).Replace("Goodbye London!", "Hello New York!"));
        }
    }
}

```


## Clojure


```clojure
(defn hello-goodbye [& more]
  (doseq [file more]
    (spit file (.replace (slurp file) "Goodbye London!" "Hello New York!"))))
```



## Common Lisp


```Lisp

(defun hello-goodbye (files)
  (labels ((replace-from-file (file)
             (with-open-file (in file)
               (loop for line = (read-line in nil)
                  while line do
                    (loop for index = (search "Goodbye London!" line)
                       while index do
                         (setf (subseq line index) "Hello New York!"))
                  collecting line)))
           (write-lines-to-file (lines file)
             (with-open-file (out file :direction :output :if-exists :overwrite)
               (dolist (line lines)
                 (write-line line out))))
           (replace-in-file (file)
             (write-lines-to-file (replace-from-file file) file)))
    (map nil #'replace-in-file files)))

```



## D

{{works with|D|2}}

```d
import std.file, std.array;

void main() {
    auto from = "Goodbye London!", to = "Hello, New York!";
    foreach (fn; "a.txt b.txt c.txt".split()) {
        write(fn, replace(cast(string)read(fn), from, to));
    }
}
```



## Erlang


```Erlang

-module( globally_replace_text ).

-export( [in_files/3, main/1] ).

in_files( Old, New, Files ) when is_list(Old) ->
        in_files( binary:list_to_bin(Old), binary:list_to_bin(New), Files );
in_files( Old, New, Files ) -> [replace_in_file(Old, New, X, file:read_file(X)) || X <- Files].

main( [Old, New | Files] ) -> in_files( Old, New, Files ).



replace_in_file( Old, New, File, {ok, Binary} ) ->
	replace_in_file_return( File, file:write_file(File, binary:replace(Binary, Old, New, [global])) );
replace_in_file( _Old, _New, File, {error, Error} ) ->
        io:fwrite( "Error: Could not read ~p: ~p~n", [File, Error] ),
        error.

replace_in_file_return( _File, ok ) -> ok;
replace_in_file_return( File, {error, Error} ) ->
	io:fwrite( "Error: Could not write ~p: ~p~n", [File, Error] ),
        error.

```


{{out}}

```txt

macbook-pro:rosettacode bengt$ ls ?.txt
1.txt	2.txt
macbook-pro:rosettacode bengt$ more ?.txt
"Goodbye London!"
"Goodbye London!"
"Byebye London!" "Byebye London!" "Byebye London!"
...skipping...
"Goodbye London!"
"Byebye London!" "Byebye London!" "Byebye London!"
"Goodbye London!"
macbook-pro:rosettacode bengt$ escript globally_replace_text.erl "Goodbye London\!" "Hello New York\!" ?.txt
macbook-pro:rosettacode bengt$ more ?.txt
"Hello New York!"
"Hello New York!"
"Byebye London!" "Byebye London!" "Byebye London!"
...skipping...
"Hello New York!"
"Byebye London!" "Byebye London!" "Byebye London!"
"Hello New York!"

```


=={{header|F_Sharp|F#}}==

```fsharp
open System.IO

[<EntryPoint>]
let main args =
    let textFrom = "Goodbye London!"
    let textTo = "Hello New York!"
    for name in args do
        let content = File.ReadAllText(name)
        let newContent = content.Replace(textFrom, textTo)
        if content <> newContent then
            File.WriteAllText(name, newContent)
    0
```



## Fortran

This is in the style of F77 and solves the usual problem of "how long is a piece of string" by choosing a size that is surely long enough. Thus, CHARACTER*6666 ALINE. Fortran 2003 allows the alteration of the length of a character variable, but, unless there is a facility whereby in something like <code>READ(F,11) ALINE</code> the size of ALINE is adjusted to suit the record being read, this doesn't help much. The Q format code allows discovery of the length of a record as it is being read, and so only the required portion, ALINE(1:L), of an input record is placed and trailing spaces out to 6666 are not supplied nor need they be scanned. Thus an input record that has trailing spaces will have them preserved - unless the text replacement changes spaces...

The search is done by using the supplied INDEX function which alas rarely has an option to specify the starting point via an additional (optional?) parameter. So this must be done via <code>INDEX(ALINE(L1:L),THIS)</code> and then one must carefully consider offsets and the like while counting on fingers and becoming confused. On the other hand, ''it'' handles annoyances such as ALINE(L1:L) being shorter than THIS. The expression ALINE(L1:L) does ''not'' create a new string variable by copying the specified text, it works (or should work!) via offsets into ALINE. Similarly, there is no attempt to concatenate an output string to write in one go as that too would involve copying text about. Though WRITE statements involve no small overhead in themselves. Although if LEN(THIS) = LEN(THAT) as is the case in the example task an alter-in-place could be used and ALINE(1:L) be written out in one go, the more general approach is used of writing text up to the start of a match, writing out the replacement THAT, and scanning beyond the match for the next text until the tail end.

The file to be altered cannot be changed "in-place", as by writing back an altered record even if the text replacement does not involve a change in length because such a facility is not available for text files that are read and written sequentially only. More accomplished file systems may well offer varying-length records with update possible even of longer or shorter new versions but standard Fortran does not demand such facilities. So, the altered content has to be written to a temporary file (or perhaps could be held in a capacious memory) which is then read back to overwrite the original file. It would be safer to rename the original file and write to a new version, but Fortran typically does not have access to any file renaming facilities and the task calls for an overwrite anyway. So, overwrite it is, which is actually a file delete followed by a write.

Once equipped with a subroutine that applies a specified change to a named disc file, there is no difficulty in invoking it for a horde of disc files. A more civilised routine might make reports about the files assaulted and the number of changes, and also be prepared to report various oddities such as a file being available but not for WRITE. It is for this reason that the source file is opened with READWRITE even though it at that stage is only going to be read from.
```Fortran
      SUBROUTINE FILEHACK(FNAME,THIS,THAT)	!Attacks a file!
       CHARACTER*(*) FNAME	!The name of the file, presumed to contain text.
       CHARACTER*(*) THIS	!The text sought in each record.
       CHARACTER*(*) THAT	!Its replacement, should it be found.
       INTEGER F,T		!Mnemonics for file unit numbers.
       PARAMETER (F=66,T=67)	!These should do.
       INTEGER L		!A length
       CHARACTER*6666 ALINE	!Surely sufficient?
       LOGICAL AHIT		!Could count them, but no report is called for.
        INQUIRE(FILE = FNAME, EXIST = AHIT)	!This mishap is frequent, so attend to it.
        IF (.NOT.AHIT) RETURN			!Nothing can be done!
        OPEN (F,FILE=FNAME,STATUS="OLD",ACTION="READWRITE")	!Grab the source file.
        OPEN (T,STATUS="SCRATCH")	!Request a temporary file.
        AHIT = .FALSE.		!None found so far.
Chew through the input, replacing THIS by THAT while writing to the temporary file..
   10   READ (F,11,END = 20) L,ALINE(1:MIN(L,LEN(ALINE)))	!Grab a record.
        IF (L.GT.LEN(ALINE)) STOP "Monster record!"		!Perhaps unmanageable.
   11   FORMAT (Q,A)		!Obviously, Q = length of characters unread in the record.
        L1 = 1			!Start at the start.
   12   L2 = INDEX(ALINE(L1:L),THIS)	!Look from L1 onwards.
        IF (L2.LE.0) THEN		!A hit?
          WRITE (T,13) ALINE(L1:L)	!No. Finish with the remainder of the line.
   13     FORMAT (A)			!Thus finishing the output line.
          GO TO 10			!And try for the next record.
        END IF				!So much for not finding THIS.
   14   L2 = L1 + L2 - 2	!Otherwise, THIS is found, starting at L1.
        WRITE (T,15) ALINE(L1:L2)	!So roll the text up to the match, possibly none.
   15   FORMAT (A,$)			!But not ending the record.
        WRITE (T,15) THAT		!Because THIS is replaced by THAT.
        AHIT = .TRUE.			!And we've found at least one match.
        L1 = L2 + LEN(THIS) + 1		!Finger the first character beyond the matching THIS.
        IF (L - L1 + 1 .GE. LEN(THIS)) GO TO 12	!Might another search succeed?
        WRITE (T,13) ALINE(L1:L)	!Nope. Finish the line with the tail end.
        GO TO 10		!And try for another record.
Copy the temporary file back over the source file. Hope for no mishap and data loss!
   20   IF (AHIT) THEN	!If there were no hits, there is nothing to do.
          CLOSE (F)		!Oh well.
          REWIND T		!Go back to the start.
          OPEN (F,FILE="new"//FNAME,STATUS = "REPLACE",ACTION = "WRITE")	!Overwrite...
   21     READ (T,11,END = 22) L,ALINE(1:MIN(L,LEN(ALINE)))	!Grab a line.
          IF (L.GT.LEN(ALINE)) STOP "Monster changed record!"	!Once you start checking...
          WRITE (F,13) ALINE(1:L) 		!In case LEN(THAT) > LEN(THIS)
          GO TO 21			!Go grab the next line.
        END IF		!So much for the replacement of the file.
   22   CLOSE(T)	!Finished: it will vanish.
        CLOSE(F)	!Hopefully, the buffers will be written.
      END	!So much for that.

      PROGRAM ATTACK
      INTEGER N
      PARAMETER (N = 6)		!More than one, anyway.
      CHARACTER*48 VICTIM(N)	!Alternatively, the file names could be read from a file
      DATA VICTIM/		!Along with the target and replacement texts in each case.
     1 "StaffStory.txt",
     2 "Accounts.dat",
     3 "TravelAgent.txt",
     4 "RemovalFirm.dat",
     5 "Addresses.txt",
     6 "SongLyrics.txt"/	!Invention flags.

      DO I = 1,N	!So, step through the list.
        CALL FILEHACK(VICTIM(I),"Goodbye London!","Hello New York!")	!One by one.
      END DO		!On to the next.

      END
```



## Go


```go
package main

import (
    "bytes"
    "io/ioutil"
    "log"
    "os"
)

func main() {
    gRepNFiles("Goodbye London!", "Hello New York!", []string{
        "a.txt",
        "b.txt",
        "c.txt",
    })
}

func gRepNFiles(olds, news string, files []string) {
    oldb := []byte(olds)
    newb := []byte(news)
    for _, fn := range files {
        if err := gRepFile(oldb, newb, fn); err != nil {
            log.Println(err)
        }
    }
}

func gRepFile(oldb, newb []byte, fn string) (err error) {
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
    if bytes.Index(b, oldb) < 0 {
        return
    }
    r := bytes.Replace(b, oldb, newb, -1)
    if err = f.Truncate(0); err != nil {
        return
    }
    _, err = f.WriteAt(r, 0)
    return
}
```


=={{Header|Haskell}}==
The module Data.List provides some useful functions: tails (constructs substrings dropping elements from the head of the list), isPrefixOf (checks if a string matches the beginning of another one) and elemIndices (gets the list indices of all elements matching a value).
This code doesn't rewrite the files, it just returns the changes made to the contents of the files.

```Haskell
import Data.List (tails, elemIndices, isPrefixOf)

replace :: String -> String -> String -> String
replace [] _ xs = xs
replace _ [] xs = xs
replace _ _ []  = []
replace a b xs  = replAll
    where
        -- make substrings, dropping one element each time
        xtails = tails xs
        -- what substrings begin with the string to replace?
        -- get their indices
        matches = elemIndices True $ map (isPrefixOf a) xtails
        -- replace one occurrence
        repl ys n = take n ys ++ b ++ drop (n + length b) ys
        -- replace all occurrences consecutively
        replAll = foldl repl xs matches

replaceInFiles a1 a2 files = do
    f <- mapM readFile files
    return $ map (replace a1 a2) f

```

This other version is more effective because it processes the string more lazily, replacing the text as it consumes the input string (the previous version was stricter because of "matches" traversing the whole list; that would force the whole string into memory, which could cause the system to run out of memory with large text files).

```Haskell
replace _ _ [] = []
replace a b xx@(x:xs) =
    if isPrefixOf a xx
    then b ++ replace a b (drop (length a) xx)
    else x : replace a b xs

```

'''Example:'''

```txt

File "t1.txt" contains: "Goodbye London! This is text 1."
File "t2.txt" contains: "This is text 2. Goodbye London! Now we repeat: Goodbye London! And that's all."

replaceInFiles "Goodbye London!" "Hello New York!" ["t1.txt", "t2.txt"]

```

'''Output:'''

```txt

["Hello New York! This is text 1.\n","This is text 2. Hello New York! Now we repeat: Hello New York! And that's all.\n"]

```


=={{header|Icon}} and {{header|Unicon}}==
This example uses the Unicon stat function.  It can be rewritten for Icon to aggregate the file in a reads loop.

```Icon
procedure main()
globalrepl("Goodbye London","Hello New York","a.txt","b.txt") # variable args for files
end

procedure globalrepl(old,new,files[])

every fn := !files do
   if s := reads(f := open(fn,"bu"),stat(f).size) then {
      writes(seek(f,1),replace(s,old,new))
      close(f)
      }
   else write(&errout,"Unable to open ",fn)
end

link strings # for replace
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/fprocs.htm#strings.icn strings.icn provides replace.]


## J


If <code>files</code> is a variable with the desired list of file names:


```j
require'strings'
(1!:2~rplc&('Goodbye London!';'Hello New York!')@(1!:1))"0 files
```



## Java

Minimalistic version, assumes default encoding.
{{works with|Java|7}}

```java
import java.io.*;
import java.nio.file.*;

public class GloballyReplaceText {

    public static void main(String[] args) throws IOException {

        for (String fn : new String[]{"test1.txt", "test2.txt"}) {
            String s = new String(Files.readAllBytes(Paths.get(fn)));
            s = s.replace("Goodbye London!", "Hello New York!");
            try (FileWriter fw = new FileWriter(fn)) {
                fw.write(s);
            }
        }
    }
}
```



## jq

{{works with|jq|1.5}}
jq delegates filename manipulation to the shell. For simplicity, in the following we assume the availability of `sponge` to simplify the mechanics of editing a file "in-place".

```bash
for file
do
  jq -Rr 'gsub($from; $to)' --arg from 'Goodbye London!' --arg to 'Hello New York!' "$file" |
    sponge "$file"
done
```


The jq filter used above is `gsub/2`, which however is designed for regular expressions. Here is a string-oriented alternative:

```jq
def gsubst($from; $to):
  ($from | length) as $len
  | def g:
      index($from) as $ix
      | if $ix then .[:$ix]  + $to +  (.[($ix+$len):] | g) else . end;
     g;
```



## Jsish

'''For the demo, the code does not overwrite the samples, but creates a .new file.'''

```javascript
/* Global replace in Jsish */
if (console.args.length == 0) {
    console.args.push('-');
}

/* For each file, globally replace "Goodbye London!" with "Hello New York!" */
var fn, data, changed;
for (fn of console.args) {
    /* No args, or an argument of - uses "stdin" (a special Channel name) */
    if (fn == 'stdin') fn = './stdin';
    if (fn == '-') fn = 'stdin';
    try {
        data = File.read(fn);
        /* Jsi supports the m multiline regexp flag */
        changed = data.replace(/Goodbye London!/gm, 'Hello New York!');
        if (changed != data) {
            if (fn == 'stdin') fn = 'stdout'; else fn += '.new';
            var cnt = File.write(fn, changed);
            puts(fn + ":" + cnt, 'updated');
        }
    } catch(err) { puts(err, 'processing', fn); }
}
```


To meet the task specification, change


```javascript
if (fn == 'stdin') fn = 'stdout'; else fn += '.new';
```


to


```javascript
if (fn == 'stdin') fn = 'stdout';
```


removing the else clause. The code will then overwrite originals.
{{out}}

```txt
prompt$ cat one
Goodbye London! it was the slice.
Goodbye London, should still be here
And a little more Goodbye London! New York!

prompt$ jsish global-replace.jsi - <one
Hello New York! it was the slice.
Goodbye London, should still be here
And a little more Hello New York! New York!
stdout:115 updated
```



## Julia

We will use Julia's built-in Perl-compatible [http://docs.julialang.org/en/latest/manual/strings/#regular-expressions regular-expressions].  Although we could read in the files line by line, it is simpler and probably faster to just read the whole file into memory (as text files are likely to fit into memory on modern computers).

```julia
filenames = ["f1.txt", "f2.txt"]
for filename in filenames
   txt = read(filename, String)
   open(filename, "w") do f
      write(f, replace(txt, "Goodbye London!" => "Hello New York!"))
   end
end
```



## Kotlin


```scala
// version 1.2.0

import java.io.File

fun main(args: Array<String>) {
    val files = arrayOf("file1.txt", "file2.txt")
    for (file in files) {
        val f = File(file)
        var text = f.readText()
        println(text)
        text = text.replace("Goodbye London!", "Hello New York!")
        f.writeText(text)
        println(f.readText())
    }
}
```


{{out}}

```txt

File1 contains "Goodbye London!"

File1 contains "Hello New York!"

File2 contains "Goodbye London!"

File2 contains "Hello New York!"

```



## Lasso


```Lasso
#!/usr/bin/lasso9

local(files = array('f1.txt', 'f2.txt'))

with filename in #files
let file = file(#filename)
let content = #file -> readbytes
do {
	#file -> dowithclose => {
		#content -> replace('Goodbye London!', 'Hello New York!')
		#file -> opentruncate
		#file -> writebytes(#content)
	}
}
```



## Liberty BASIC


```lb

nomainwin

file$( 1) ="data1.txt"
file$( 2) ="data2.txt"
file$( 3) ="data3.txt"


for i =1 to 3
    open file$( i) for input as #i
        orig$ =input$( #i, lof( #i))
    close #i

    dummy$ =FindReplace$( orig$, "Goodbye London!", "Hello New York!", 1)

    open "RC" +file$( i) for output as #o
        #o dummy$;
    close #o
next i

end

function FindReplace$( FindReplace$, find$, replace$, replaceAll)
'   Target string, string to find, string to replace it with, flag 0/1 for 'replace all occurrences'.
    if ( ( FindReplace$ <>"") and ( find$ <>"") ) then
        fLen =len( find$)
        rLen =len( replace$)
        do
            fPos =instr( FindReplace$, find$, fPos)
            if not( fPos) then exit function
            pre$            =left$( FindReplace$, fPos -1)
            post$           =mid$( FindReplace$, fPos +fLen)
            FindReplace$    =pre$ +replace$ +post$
            fPos            =fPos +( rLen -fLen) +1
        loop while (replaceAll)
    end if
end function

```



## Lua


```lua
filenames = { "f1.txt", "f2.txt" }

for _, fn in pairs( filenames ) do
    fp = io.open( fn, "r" )
    str = fp:read( "*all" )
    str = string.gsub( str, "Goodbye London!", "Hello New York!" )
    fp:close()

    fp = io.open( fn, "w+" )
    fp:write( str )
    fp:close()
end
```



=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
listOfFiles = {"a.txt", "b.txt", "c.txt"};
Do[
 filename = listOfFiles[[i]];
 filetext = Import[filename, "Text"];
 filetext = StringReplace[filetext, "Goodbye London!" -> "Hello New York!"];
 Export[filename, filetext, "Text"]
 , {i, 1, Length[listOfFiles]}]
```

File b.txt before the code is run:

```txt
second file for the Globally replace text in several files problem.
Goodbye London!
Goodbye London!
Bye bye London!
Bye bye London! Bye bye London!
```

File b.txt after the code is run:

```txt
second file for the Globally replace text in several files problem.
Hello New York!
Hello New York!
Bye bye London!
Bye bye London! Bye bye London!

```



## Nim


```nim
import strutils

var fr = "Goodbye London!"
var to = "Hello, New York!"

for fn in ["a.txt", "b.txt", "c.txt"]:
  fn.writeFile fn.readFile.replace(fr, to)
```



## OpenEdge/Progress


```progress
FUNCTION replaceText RETURNS LOGICAL (
   i_cfile_list   AS CHAR,
   i_cfrom        AS CHAR,
   i_cto          AS CHAR
):

   DEF VAR ii     AS INT.
   DEF VAR lcfile AS LONGCHAR.

   DO ii = 1 TO NUM-ENTRIES( i_cfile_list ):
      COPY-LOB FROM FILE ENTRY( ii, i_cfile_list ) TO lcfile.
      lcfile = REPLACE( lcfile, i_cfrom, i_cto ).
      COPY-LOB FROM lcfile TO FILE ENTRY( ii, i_cfile_list ).
   END.

END FUNCTION. /* replaceText */

replaceText(
   "a.txt,b.txt,c.txt",
   "Goodbye London!",
   "Hello New York!"
).
```



## Pascal

{{works with|Free_Pascal}}

```pascal
Program StringReplace;

uses
  Classes, StrUtils;

const
  fileName: array[1..3] of string = ('a.txt', 'b.txt', 'c.txt');
  matchText = 'Goodbye London!';
  replaceText = 'Hello New York!';

var
  AllText: TStringlist;
  i, j: integer;

begin
  for j := low(fileName) to high(fileName) do
  begin
   AllText := TStringlist.Create;
   AllText.LoadFromFile(fileName[j]);
   for i := 0 to AllText.Count-1 do
     AllText.Strings[i] := AnsiReplaceStr(AllText.Strings[i], matchText, replaceText);
   AllText.SaveToFile(fileName[j]);
   AllText.Destroy;
  end;
end.
```



## Perl


```bash
perl -pi -e "s/Goodbye London\!/Hello New York\!/g;" a.txt b.txt c.txt
```



## Perl 6


Current Perl 6 implementations do not yet support the -i flag for editing files in place, so we roll our own (rather unsafe) version:


```perl6
slurp($_).subst('Goodbye London!', 'Hello New York!', :g) ==> spurt($_)
    for <a.txt b.txt c.txt>;
```



## Phix

ctrace.out was just a file that happened to be handy, obviously you'd have to provide your own file list.

as hinted, you could probably improve on the error handling.

get_text is deliberately limited to 1GB, for larger files use a temporary file, a loop of gets/puts, and delete_file/rename_file at the end.

```Phix
procedure global_replace(string s, string r, sequence file_list)
    for i=1 to length(file_list) do
        string filename = file_list[i]
        integer fn = open(filename,"rb")
        if fn=-1 then ?9/0 end if   -- message/retry?
        string text = get_text(fn)
        close(fn)
        text = substitute(text,s,r)
        fn = open(filename,"wb")
        puts(fn,text)
        close(fn)
    end for
end procedure

sequence file_list = {"ctrace.out"}
global_replace("Goodbye London!", "Hello New York!", file_list)
```



## PicoLisp


```PicoLisp
(for File '(a.txt b.txt c.txt)
   (call 'mv File (tmp File))
   (out File
      (in (tmp File)
         (while (echo "Goodbye London!")
            (prin "Hello New York!") ) ) ) )
```



## PowerBASIC

{{trans|BASIC}}

```powerbasic
$matchtext = "Goodbye London!"
$repltext  = "Hello New York!"

FUNCTION PBMAIN () AS LONG
    DIM L0 AS INTEGER, filespec AS STRING, linein AS STRING

    L0 = 1
    WHILE LEN(COMMAND$(L0))
        filespec = DIR$(COMMAND$(L0))
        WHILE LEN(filespec)
            OPEN filespec FOR BINARY AS 1
                linein = SPACE$(LOF(1))
                GET #1, 1, linein
                ' No need to jump through FB's hoops here...
                REPLACE $matchtext WITH $repltext IN linein
                PUT #1, 1, linein
                SETEOF #1
            CLOSE
            filespec = DIR$
        WEND
        INCR L0
    WEND
END FUNCTION
```



## PowerShell


```PowerShell

$listfiles = @('file1.txt','file2.txt')
$old = 'Goodbye London!'
$new = 'Hello New York!'
foreach($file in $listfiles) {
    (Get-Content $file).Replace($old,$new) | Set-Content $file
}

```



## PureBasic


```PureBasic
Procedure GRTISF(List File$(), Find$, Replace$)
  Protected Line$, Out$, OutFile$, i
  ForEach File$()
    fsize=FileSize(File$())
    If fsize<=0: Continue: EndIf
    If ReadFile(0, File$())
      i=0
      ;
      ; generate a temporary file in a safe way
      Repeat
        file$=GetTemporaryDirectory()+base$+"_"+Str(i)+".tmp"
        i+1
      Until FileSize(file$)=-1
      i=CreateFile(FileID, file$)
      If i
        ; Copy the infile to the outfile while replacing any needed text
        While Not Eof(0)
          Line$=ReadString(0)
          Out$=ReplaceString(Line$,Find$,Replace$)
          WriteString(1,Out$)
        Wend
        CloseFile(1)
      EndIf
      CloseFile(0)
      If i
        ; If we made a new file, copy it back.
        CopyFile(file$, File$())
        DeleteFile(file$)
      EndIf
    EndIf
  Next
EndProcedure
```

Implementation

```txt
NewList Xyz$()
AddElement(Xyz$()): Xyz$()="C:\\a.txt"
AddElement(Xyz$()): Xyz$()="C:\\b.txt"
AddElement(Xyz$()): Xyz$()="D:\\c.txt"

GRTISF(Xyz$(), "Goodbye London", "Hello New York")
```



## Python

From [http://docs.python.org/library/fileinput.html Python docs]. (Note: in-place editing does not work for MS-DOS 8+3 filesystems.).


```python
import fileinput

for line in fileinput.input(inplace=True):
    print(line.replace('Goodbye London!', 'Hello New York!'), end='')

```



## Racket

Code wrapped in a convenient script:

```racket

#!/usr/bin/env racket
#lang racket

(define from-string #f)
(define to-string #f)

(command-line
 #:once-each
 [("-f") from "Text to remove" (set! from-string from)]
 [("-t") to "Text to put instead" (set! to-string to)]
 #:args files
 (unless from-string (error "No `from' string specified"))
 (unless to-string   (error "No `to' string specified"))
 (when (null? files) (error "No files given"))
 (define from-rx (regexp (regexp-quote from-string)))
 (for ([file files])
   (printf "Editing ~a..." file) (flush-output)
   (define text1 (file->string file))
   (define text2 (regexp-replace* from-rx text1 to-string))
   (if (equal? text1 text2)
     (printf " no change\n")
     (begin (display-to-file text2 file #:exists 'replace)
            (printf " modified copy saved in place\n")))))

```

Sample run:

```txt

$ ./replace -h
replace [ <option> ... ] [<files>] ...
 where <option> is one of
  -f <from> : Text to remove
  -t <to> : Text to put instead
  --help, -h : Show this help
  -- : Do not treat any remaining argument as a switch (at this level)
 Multiple single-letter switches can be combined after one `-'; for
  example: `-h-' is the same as `-h --'
$ ./replace -f "Goodbye London!" "Hello New York!" file*
Editing file1... no change
Editing file2... modified copy saved in place
Editing file3... modified copy saved in place

```




## Red


```Red>>
 f: request-file
>> str: read f
>> replace/all str "Goodbye London!" "Hello New York!"
>> write f str
```


## REXX


### version 1

This example works under "DOS" and/or "DOS" under Microsoft Windows.


File names that contain blanks should have their blanks replaced with commas.

```rexx
/*REXX program  reads  the  files specified  and  globally replaces  a string.          */
old= "Goodbye London!"                           /*the  old text     to be replaced.    */
new= "Hello New York!"                           /* "   new   "   used for replacement. */
parse  arg  fileList                             /*obtain required list of files from CL*/
files=words(fileList)                            /*the number of files in the file list.*/

   do f=1  for files;    fn=translate(word(fileList,f),,',');     say;   say
   say '──────── file is being read: '    fn    " ("f   'out of'   files   "files)."
   call linein fn,1,0                            /*position the file for input.         */
   changes=0                                     /*the number of changes in file so far.*/
             do rec=0  while lines(fn)\==0       /*read a file   (if it exists).        */
             @.rec=linein(fn)                    /*read a record (line)  from the file. */
             if pos(old, @.rec)==0  then iterate /*Anything to change?   No, then skip. */
             changes=changes + 1                 /*flag that file contents have changed.*/
             @.rec=changestr(old, @.rec, new)    /*change the @.rec record, old ──► new.*/
             end   /*rec*/

   say '──────── file has been read: '         fn", with "      rec      'records.'
   if changes==0  then do;  say '──────── file  not  changed: '   fn;   iterate;   end
   call lineout fn,,1                            /*position file for output at 1st line.*/
   say '──────── file being changed: '   fn

       do r=0  for rec;     call lineout fn, @.r /*re─write the contents of the file.   */
       end   /*r*/

   say '──────── file was   changed: '   fn    " with"   changes   'lines changed.'
   end   /*f*/                                   /*stick a fork in it,  we're all done. */
```

Some older REXXes don't have a   '''changestr'''   BIF,   so one is included here   ──►    [[CHANGESTR.REX]].



'''output'''   when using the input (list of files) of:   <tt> one.txt   two.txt </tt>

```txt

──────── file is being read:  one.txt  (1 out of 2 files).
──────── file has been read:  one.txt, with  13 records.
──────── file being changed:  one.txt
──────── file was   changed:  one.txt  with 2 lines changed.


──────── file is being read:  two.txt  (2 out of 2 files).
──────── file has been read:  two.txt, with  59 records.
──────── file being changed:  two.txt
──────── file was   changed:  two.txt  with 6 lines changed.

```



### Version 2

 considering file ids that contain blanks

```rexx
/* REXX ***************************************************************
* Copy all files *.txt to *.rpl
* replacing all occurrences of old by new
* Execute in the directory containing the files to be processed
* 16.01.2013 Walter Pachl
*                         ...if file names contain blanks
**********************************************************************/
Parse Arg a
If a='?' Then Do
  Do i=2 To 5
    Say substr(sourceline(i),3)
    End
  Exit
  End
'dir *.rpl'
Say 'May I erase *.rpl?'
Parse Upper Pull answer
If answer='Y' | answer='J' Then
  'erase *.rpl'
Else Do
  Say 'Giving up..'
  Exit
  End
old='Goodbye London!'
new='Hello New York!'
dir='dir.dir'
'dir *.* >' dir
Do While lines(dir)>0
  Parse Value linein(dir) With 37 f
  Select
    When f='' |,
         left(f,1)='.' |,
         pos(' Bytes',f)>0 Then Iterate
    When right(f,4)='.txt' Then
      Call replace
    Otherwise
      Say left(f,50) 'not eligible for replacing'
    End
  End
Exit

replace:
/* REXX ***************************************************************
* Copy a file fn.txt to fn.rpl
* replacing all occurrences of old by new
**********************************************************************/
oid=fn(f)'.rpl'
cnt.=0
Do ii=1 By 1 While lines(f)>0
  l=linein(f)
  ol=repl(l,new,old)
  Call lineout oid,ol
  End
Call lineout f
Call lineout oid
Select
  When cnt.0changes=0 Then Do
    'erase' oid
    Say left(f,50) 'no changes'
    End
  When cnt.0changes=1 Then
    Say left(f,50) '1 change'
  Otherwise
    Say left(f '->' oid,50) cnt.0changes 'changes'
  End
Return

fn: Procedure
/* REXX ***************************************************************
* Get the file name of a file id
**********************************************************************/
parse Arg fid
Parse Var fid fn '.' ft
Return fn

repl: Procedure Expose cnt.
/* REXX ***************************************************************
* Replace an old string by a new one
**********************************************************************/
  Parse Arg s,new,old
  ol=''
  Do Until p=0
    p=pos(old,s)
    If p>0 Then Do
      ol=ol||left(s,p-1)||new
      s=substr(s,p+length(old))
      cnt.0changes=cnt.0changes+1
      End
    Else
      ol=ol||s
    End
  Return ol
```

Sample output:

```txt

 Datenträger in Laufwerk Z: ist H
 Volumeseriennummer: FE17-3A89

 Verzeichnis von Z:\glr

16.01.2013  09:55               283 input.rpl
16.01.2013  09:55               352 input2.rpl
16.01.2013  09:55               283 input file.rpl
               3 Datei(en),            918 Bytes
               0 Verzeichnis(se),  3.993.468.928 Bytes frei
May I erase *.rpl?
  ----> here I entered y
input.txt -> input.rpl                             1 change
input.nix                                          not eligible for replacing
input2.txt -> input2.rpl                           4 changes
dir.dir                                            not eligible for replacing
input file.txt -> input file.rpl                   1 change
input3.txt                                         no changes

```



## Ring


```ring

filenames = ["ReadMe.txt", "ReadMe2.txt"]

for fn in filenames
    fp = fopen(fn,"r")
    str = fread(fp,getFileSize(fp))
    str = substr(str, "Greetings", "Hello")
    fclose(fp)

    fp = fopen(fn,"w")
    fwrite(fp, str)
    fclose(fp)
next

func getFileSize fp
     C_FILESTART = 0
     C_FILEEND = 2
     fseek(fp,0,C_FILEEND)
     nFileSize = ftell(fp)
     fseek(fp,0,C_FILESTART)
     return nFileSize

```



## Ruby

Like Perl:

```txt

ruby -pi -e "gsub('Goodbye London!', 'Hello New York!')" a.txt b.txt c.txt

```



## Run BASIC


```runbasic
file$(1) ="data1.txt"
file$(2) ="data2.txt"
file$(3) ="data3.txt"

for i = 1 to 3
    open file$(i) for input as #in
        fileBefore$ = input$( #in, lof( #in))
    close #in

    fileAfter$ = strRep$(fileBefore$, "Goodbye London!", "Hello New York!")
    open "new_" +  file$(i) for output as #out
        print #out,fileAfter$;
    close #out
next i
end

' --------------------------------
' string replace - rep str with
' --------------------------------
FUNCTION strRep$(str$,rep$,with$)
ln  = len(rep$)
ln1 = ln - 1
i   = 1
while i <= len(str$)
    if mid$(str$,i,ln) = rep$ then
        strRep$ = strRep$ + with$
        i = i + ln1
    else
        strRep$ = strRep$ + mid$(str$,i,1)
    end if
i = i + 1
WEND
END FUNCTION
```



## Scala


```Scala
import java.io.{File, PrintWriter}

object GloballyReplaceText extends App {

  val (charsetName, fileNames) = ("UTF8", Seq("file1.txt", "file2.txt"))
  for (fileHandle <- fileNames.map(new File(_)))
    new PrintWriter(fileHandle, charsetName) {
      print(scala.io.Source.fromFile(fileHandle, charsetName).mkString
        .replace("Goodbye London!", "Hello New York!"))
      close()
    }

}
```


## Sed

{{works with|GNU Sed}}

```bash
sed -i 's/Goodbye London!/Hello New York!/g' a.txt b.txt c.txt
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "getf.s7i";

const proc: main is func
  local
    var string: fileName is "";
    var string: content is "";
  begin
    for fileName range [] ("a.txt", "b.txt", "c.txt") do
      content := getf(fileName);
      content := replace(content, "Goodbye London!", "Hello New York!");
      putf(fileName, content);
    end for;
  end func;
```



## Sidef


```ruby
var names = %w(
                a.txt
                b.txt
                c.txt
              )
 
names.map{ File(_) }.each { |file|
    say file.edit { |line|
        line.gsub("Goodbye London!", "Hello New York!")
    }
}
```



## Tcl

{{tcllib|fileutil}}

```tcl
package require Tcl 8.5
package require fileutil

# Parameters to the replacement
set from "Goodbye London!"
set to "Hello New York!"
# Which files to replace
set fileList [list a.txt b.txt c.txt]

# Make a command fragment that performs the replacement on a supplied string
set replacementCmd [list string map [list $from $to]]
# Apply the replacement to the contents of each file
foreach filename $fileList {
    fileutil::updateInPlace $filename $replacementCmd
}
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
files="a.txt'b.txt'c.txt"

BUILD S_TABLE search = ":Goodbye London!:"

LOOP file=files
 ERROR/STOP OPEN (file,WRITE,-std-)
 ERROR/STOP CREATE ("scratch",FDF-o,-std-)
  ACCESS q: READ/STREAM/RECORDS/UTF8 $file s,aken+text/search+eken
  ACCESS s: WRITE/ERASE/STREAM/UTF8 "scratch" s,aken+text+eken
   LOOP
    READ/EXIT q
    IF (text.ct.search) SET text="Hello New York!"
    WRITE/ADJUST s
   ENDLOOP
  ENDACCESS/PRINT q
  ENDACCESS/PRINT s
 ERROR/STOP COPY ("scratch",file)
 ERROR/STOP CLOSE (file)
ENDLOOP
ERROR/STOP DELETE ("scratch")

```



## TXR



### Extraction Language



```txr
@(next :args)
@(repeat)
@file
@(next `@file`)
@(freeform)
@(coll :gap 0)@notmatch@{match /Goodbye, London!/}@(end)@*tail@/\n/
@(output `@file.tmp`)
@(rep)@{notmatch}Hello, New York!@(end)@tail
@(end)
@(do @(rename-path `@file.tmp` file))
@(end)
```

Run:

```txt
$ cat foo.txt
aaaGoodbye, London!aaa
Goodbye, London!
$ cat bar.txt
aaaGoodbye, London!aaa
Goodbye, London!
$ txr replace-files.txr foo.txt bar.txt
$ cat foo.txt
aaaHello, New York!aaa
Hello, New York!
$ cat bar.txt
aaaHello, New York!aaa
Hello, New York!
```


Run, with no directory permissions:


```txt
$ chmod a-w .
$ txr replace-files.txr foo.txt bar.txt
txr: unhandled exception of type file_error:
txr: could not open foo.txt.tmp (error 13/Permission denied)

```



### TXR Lisp



```txrlisp
(each ((fname *args*))
  (let* ((infile (open-file fname))
         (outfile (open-file `@fname.tmp` "w"))
         (content (get-string infile))
         (edited (regsub #/Goodbye, London/ "Hello, New York" content)))
    (put-string edited outfile)
    (rename-path `@fname.tmp` fname)))
```



## UNIX Shell

{{works with|bash}}

```bash
replace() {
    local search=$1 replace=$2
    local file lines line
    shift 2
    for file in "$@"; do
        lines=()
        while IFS= read -r line; do
            lines+=( "${line//$search/$replace}" )
        done < "$file"
        printf "%s\n" "${lines[@]}" > "$file"
    done
}
replace "Goodbye London!" "Hello New York!" a.txt b.txt c.txt
```


{{works with|ksh93}}

```bash
function replace {
    typeset search=$1 replace=$2
    typeset file lines line
    shift 2
    for file in "$@"; do
        lines=()
        while IFS= read -r line; do
            lines+=( "${line//$search/$replace}" )
        done < "$file"
        printf "%s\n" "${lines[@]}" > "$file"
    done
}
replace "Goodbye London!" "Hello New York!" a.txt b.txt c.txt
```




## VBScript

{{works with|Windows Script Host|*}}

```VBScript

Const ForReading = 1
Const ForWriting = 2

strFiles = Array("test1.txt", "test2.txt", "test3.txt")

With CreateObject("Scripting.FileSystemObject")
	For i = 0 To UBound(strFiles)
		strText = .OpenTextFile(strFiles(i), ForReading).ReadAll()
		With .OpenTextFile(strFiles(i), ForWriting)
			.Write Replace(strText, "Goodbye London!", "Hello New York!")
			.Close
		End With
	Next
End With

```



## Vedit macro language

The list of files is in file "files.lst" which is expected to be in current directory.

```vedit
File_Open("files.lst")          // list of files to process
#20 = Reg_Free                  // text register for filename

While(!At_EOF) {
    Reg_Copy_Block(#20, Cur_Pos, EOL_Pos)
    File_Open(@(#20))
    Replace("Goodbye London!", "Hello New York!", BEGIN+ALL+NOERR)
    Buf_Close(NOMSG)
    Line(1, ERRBREAK)
}

Reg_Empty(#20)                  // Cleanup
Buf_Quit(OK)
```



## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
string 0;                       \use zero-terminated strings

func StrLen(A);                 \Return number of characters in an ASCIIZ string
char A;
int  I;
for I:= 0 to -1>>1-1 do
    if A(I) = 0 then return I;

func StrFind(A, B);             \Search for ASCIIZ string A in string B
\Returns address of first occurrence of string A in B, or zero if A is not found
char A, B;                      \strings to be compared
int  LA, LB, I, J;
[LA:= StrLen(A);
LB:= StrLen(B);
for I:= 0 to LB-LA do
    [for J:= 0 to LA-1 do
        if A(J) # B(J+I) then J:= LA+1;
    if J = LA then return B+I;  \found
    ];
return 0;
];

proc ReplaceText(FileName);     \replace text in specified file
char FileName;
char Str(1_000_000), Hello, Bye, Pointer;
int  Handle, I, C;
[Handle:= FOpen(FileName, 0);   \get handle for input file
FSet(Handle, ^I);               \set device 3 input to file handle
OpenI(3);                       \initialize buffer pointers
I:= 0;
repeat  C:= ChIn(3);            \read file into memory
        Str(I):= C;
        I:= I+1;
until   C = $1A; \EOF
FClose(Handle);                 \release handle

Hello:= "Hello New York!";      \replacement text
Bye:=   "Goodbye London!";
Pointer:= StrFind(Bye, Str);
if Pointer \#0\ then            \overwrite (both strings are same length)
   for I:= 0 to 15-1 do Pointer(I):= Hello(I);

Handle:= FOpen(FileName, 1);    \get handle for output file
FSet(Handle, ^O);               \set device 3 output to file handle
OpenO(3);
I:= 0;
repeat  C:= Str(I);             \write file from memory
        I:= I+1;
        ChOut(3, C);
until   C = $1A; \EOF
Close(3);                       \flush output buffer
FClose(Handle);                 \release handle
];

int File, I;
[File:= ["Alpha.txt", "Beta.txt", "Gamma.txt", "Delta.txt"];
for I:= 0 to 4-1 do ReplaceText(File(I));
]
```



## zkl


```zkl
fcn sed(data,src,dst){
   srcSz:=src.len(); dstSz:=dst.len(); md5:=Utils.MD5.calc(data);
   n:=0; while(Void!=(n:=data.find(src,n)))
           { data.del(n,srcSz); data.insert(n,dst); n+= dstSz; }
   return(md5!=Utils.MD5.calc(data)); // changed?
}
fcn sedFile(fname,src,dst){
   f:=File(fname,"r"); data:=f.read(); f.close();
   if(sed(data,"Goodbye London!", "Hello New York!"))
      { f:=File(fname,"w"); f.write(data); f.close(); }
}
```

This is a read file/blast it/write if changed. You could also do it line by line.

```zkl
vm.arglist.apply2(sedFile);
$ zkl bbb foo.txt bar.txt
```

The apply2 method doesn't return anything, it is a side effects method.
You could also easily thread this (by using sedFile.launch or sedFile.strand depending on if you wanted a true thread or a co-op thread). I didn't because I didn't want to bother with checking for duplicate files or file locking.
vm.arglist, when passed to "main" (the constructor of the file being run) is a copy of argv that has been pruned.

{{omit from|HTML}}
{{omit from|PARI/GP|No real capacity for string manipulation}}

[[Category:Text processing]]
[[Category:File handling]]
