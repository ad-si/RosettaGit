+++
title = "FASTA format"
description = ""
date = 2019-07-19T13:28:05Z
aliases = []
[extra]
id = 13244
[taxonomies]
categories = ["task"]
tags = []
+++

In [[wp:bioinformatics|bioinformatics]], long character strings are often encoded in a format called [[wp:FASTA format|FASTA]].

A FASTA file can contain several strings, each identified by a name marked by a <big><big><code>&gt;</code></big></big> (greater than) character at the beginning of the line.


## Task

Write a program that reads a FASTA file such as:

```txt

>Rosetta_Example_1
THERECANBENOSPACE
>Rosetta_Example_2
THERECANBESEVERAL
LINESBUTTHEYALLMUST
BECONCATENATED

```

```txt

Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED

```

Note that a high-quality implementation will not hold the entire file in memory at once; real FASTA files can be multiple gigabytes in size.





## Ada


The simple solution just reads the file (from standard input) line by line and directly writes it to the standard output.


```Ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Simple_FASTA is

   Current: Character;

begin
   Get(Current);
   if Current /= '>' then
      raise Constraint_Error with "'>' expected";
   end if;
   while not End_Of_File loop -- read name and string
      Put(Get_Line & ": "); -- read name and write directly to output
      Read_String:
      loop
	 exit Read_String when End_Of_File; -- end of input
	 Get(Current);
	 if Current = '>' then -- next name
	    New_Line;
	    exit Read_String;
	 else
	    Put(Current & Get_Line);
	    -- read part of string and write directly to output
	 end if;
      end loop Read_String;
   end loop;

end Simple_FASTA;
```


```txt
./simple_fasta < test.txt
Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED
```



This is a boringly simple text transformation.

The following more complex solution reads the entire file into a map and then prints the data stored in the map. The output is exactly the same. as for the simple text transformation. ''"Note that a high-quality implementation will not hold the entire file in memory at once; real FASTA files can be multiple gigabytes in size."'' When processing FASTA files, one may use the input step by step to uptdate an internal data structure and, at the end, to output the answer to a given question. For the task at hand the required output is about the same as the input, thus we store the entire input. For another task, we would not store the entire file. If the task where, e.g., to count the number of characters for each string, we would store (name, number) pairs in our data structure.



```Ada
with Ada.Text_IO, Ada.Containers.Indefinite_Ordered_Maps; use Ada.Text_IO;

procedure FASTA is
   package Maps is new  Ada.Containers.Indefinite_Ordered_Maps
     (Element_Type => String, Key_Type => String);
   Map: Maps.Map; -- Map holds the full file (as pairs of name and value)

   function Get_Value(Previous: String := "") return String is
      Current: Character;
   begin
      if End_Of_File then
	 return Previous; -- file ends
      else
	 Get(Current); -- read first character
	 if Current = '>' then -- ah, a new name begins
	    return Previous; -- the string read so far is the value
	 else -- the entire line is part of the value
	    return Get_Value(Previous & Current & Get_Line);
	 end if;
      end if;
   end Get_Value;

   procedure Print_Pair(Position: Maps.Cursor) is
   begin
      Put_Line(Maps.Key(Position) & ": " & Maps.Element(Position));
      -- Maps.Key(X) is the name and Maps.Element(X) is the value at X
   end Print_Pair;

   Skip_This: String := Get_Value;
   -- consumes the entire file, until the first line starting with '>'.
   -- the string Skip_This should be empty, but we don't verify this

begin
   while not End_Of_File loop -- read the file into Map
      declare
	 Name: String := Get_Line;
	   -- reads all characters in the line, except for the first ">"
	 Value: String := Get_Value;
      begin
	 Map.Insert(Key => Name, New_Item => Value);
	 -- adds the pair (Name, Value) to Map
      end;
   end loop;

   Map.Iterate(Process => Print_Pair'Access); -- print Map
end FASTA;
```



## Aime



```aime
file f;
text n, s;

f.affix(argv(1));

while (f.line(s) ^ -1) {
    if (s[0] == '>') {
        o_(n, s, ": ");
        n = "\n";
    } else {
        o_(s);
    }
}

o_(n);
```

```txt
>Rosetta_Example_1: THERECANBENOSPACE
>Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED
```



## ALGOL W


```algolw
begin
    % reads FASTA format data from standard input and write the results to standard output %
    % only handles the ">" line start                                                      %
    string(256) line;
    % allow the program to continue after reaching end-of-file %
    ENDFILE := EXCEPTION( false, 1, 0, false, "EOF" );
    % handle the input %
    readcard( line );
    while not XCPNOTED(ENDFILE) do begin
        % strings are fixed length in Algol W - we need to find the line lengh with trailing spaces removed %
        integer len;
        len := 255;
        while len > 0 and line( len // 1 ) = " " do len := len - 1;
        if len > 0 then begin % non-empty line %
            integer pos;
            pos := 0;
            if line( 0 // 1 ) = ">" then begin % header line %
                write();
                pos := 1;
            end if_header_line ;
            for cPos := pos until len do writeon( line( cPos // 1 ) );
            if line( 0 // 1 ) = ">" then writeon( ": " )
        end if_non_empty_line ;
        readcard( line );
    end while_not_eof
end.
```

```txt

Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED

```



## AutoHotkey


```AutoHotkey
Data =
(
>Rosetta_Example_1
THERECANBENOSPACE
>Rosetta_Example_2
THERECANBESEVERAL
LINESBUTTHEYALLMUST
BECONCATENATED
)

Data := RegExReplace(RegExReplace(Data, ">\V+\K\v+", ": "), "\v+(?!>)")
Gui, add, Edit, w700,  % Data
Gui, show
return
```

```txt
>Rosetta_Example_1: THERECANBENOSPACE
>Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED
```



## AWK


```AWK

# syntax: GAWK -f FASTA_FORMAT.AWK filename
# stop processing each file when an error is encountered
{   if (FNR == 1) {
      header_found = 0
      if ($0 !~ /^[;>]/) {
        error("record is not valid")
        nextfile
      }
    }
    if ($0 ~ /^;/) { next } # comment begins with a ";"
    if ($0 ~ /^>/) { # header
      if (header_found > 0) {
        printf("\n") # EOL for previous sequence
      }
      printf("%s: ",substr($0,2))
      header_found = 1
      next
    }
    if ($0 ~ /[ \t]/) { next } # ignore records with whitespace
    if ($0 ~ /\*$/) { # sequence may end with an "*"
      if (header_found > 0) {
        printf("%s\n",substr($0,1,length($0)-1))
        header_found = 0
        next
      }
      else {
        error("end of sequence found but header is missing")
        nextfile
      }
    }
    if (header_found > 0) {
      printf("%s",$0)
    }
    else {
      error("header not found")
      nextfile
    }
}
ENDFILE {
    if (header_found > 0) {
      printf("\n")
    }
}
END {
    exit (errors == 0) ? 0 : 1
}
function error(message) {
    printf("error: FILENAME=%s, FNR=%d, %s, %s\n",FILENAME,FNR,message,$0) >"con"
    errors++
    return
}

```

```txt

Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED

```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void main()
{
	FILE * fp;
	char * line = NULL;
	size_t len = 0;
	ssize_t read;

	fp = fopen("fasta.txt", "r");
	if (fp == NULL)
		exit(EXIT_FAILURE);

	int state = 0;
	while ((read = getline(&line, &len, fp)) != -1) {
		/* Delete trailing newline */
		if (line[read - 1] == '\n')
			line[read - 1] = 0;
		/* Handle comment lines*/
		if (line[0] == '>') {
			if (state == 1)
				printf("\n");
			printf("%s: ", line+1);
			state = 1;
		} else {
			/* Print everything else */
			printf("%s", line);
		}
	}
	printf("\n");

	fclose(fp);
	if (line)
		free(line);
	exit(EXIT_SUCCESS);
}
```

```txt
Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED
```



## C++


```cpp
#include <iostream>
#include <fstream>

int main( int argc, char **argv ){
    if( argc <= 1 ){
        std::cerr << "Usage: "<<argv[0]<<" [infile]" << std::endl;
        return -1;
    }

    std::ifstream input(argv[1]);
    if(!input.good()){
        std::cerr << "Error opening '"<<argv[1]<<"'. Bailing out." << std::endl;
        return -1;
    }

    std::string line, name, content;
    while( std::getline( input, line ).good() ){
        if( line.empty() || line[0] == '>' ){ // Identifier marker
            if( !name.empty() ){ // Print out what we read from the last entry
                std::cout << name << " : " << content << std::endl;
                name.clear();
            }
            if( !line.empty() ){
                name = line.substr(1);
            }
            content.clear();
        } else if( !name.empty() ){
            if( line.find(' ') != std::string::npos ){ // Invalid sequence--no spaces allowed
                name.clear();
                content.clear();
            } else {
                content += line;
            }
        }
    }
    if( !name.empty() ){ // Print out what we read from the last entry
        std::cout << name << " : " << content << std::endl;
    }

    return 0;
}
```


```txt
Rosetta_Example_1 : THERECANBENOSPACE
Rosetta_Example_2 : THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED

```


## C#


```c#
using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

class Program
{
    public class FastaEntry
    {
        public string Name { get; set; }
        public StringBuilder Sequence { get; set; }
    }

    static IEnumerable<FastaEntry> ParseFasta(StreamReader fastaFile)
    {
        FastaEntry f = null;
        string line;
        while ((line = fastaFile.ReadLine()) != null)
        {
            // ignore comment lines
            if (line.StartsWith(";"))
                continue;

            if (line.StartsWith(">"))
            {
                if (f != null)
                    yield return f;
                f = new FastaEntry { Name = line.Substring(1), Sequence = new StringBuilder() };
            }
            else if (f != null)
                f.Sequence.Append(line);
        }
        yield return f;
    }

    static void Main(string[] args)
    {
        try
        {
            using (var fastaFile = new StreamReader("fasta.txt"))
            {
                foreach (FastaEntry f in ParseFasta(fastaFile))
                    Console.WriteLine("{0}: {1}", f.Name, f.Sequence);
            }
        }
        catch (FileNotFoundException e)
        {
            Console.WriteLine(e);
        }
        Console.ReadLine();
    }
}
```



## Clojure


```clojure
(defn fasta [pathname]
  (with-open [r (clojure.java.io/reader pathname)]
    (doseq [line (line-seq r)]
      (if (= (first line) \>)
          (print (format "%n%s: " (subs line 1)))
        (print line)))))
```



## Common Lisp


```lisp
(defun fasta (pathname)
  (with-open-file (s pathname)
    (loop for line = (read-line s nil)
          while line
          do (if (char= #\> (char line 0))
                 (format t "~&~A: " (subseq line 1))
                 (princ line))
          finally (fresh-line))))
```



## D


```d
import std.stdio, std.string;

void main() {
    immutable fileName = "fasta_format_data.fasta";

    bool first = true;

    foreach (const line; fileName.File.byLine) {
        if (line[0] == '>') {
            if (first) {
                first = false;
            } else {
                writeln;
            }

            write(line[1 .. $].strip, ": ");
        } else {
            line.strip.write;
        }
    }

    writeln;
}
```

```txt
Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED
```



## FreeBASIC

This program sticks to the task as described in the heading and doesn't allow for any of the (apparently) obsolete
practices described in the Wikipedia article :

```freebasic
' FB 1.05.0 Win64

Function checkNoSpaces(s As String) As Boolean
  For i As UInteger = 0 To Len(s) - 1
    If s[i] = 32 OrElse s[i] = 9 Then Return False  '' check for spaces or tabs
  Next
  Return True
End Function

Open "input.fasta" For Input As # 1

Dim As String ln, seq
Dim first As Boolean = True

While Not Eof(1)
  Line Input #1, ln
  If Left(ln, 1) = ">"  Then
    If Not first Then Print
    Print Mid(ln, 2); ": ";
    If first Then first = False
  ElseIf first Then
    Print: Print "Error : File does not begin with '>'";
    Exit While
  Else
    If checkNoSpaces(ln) Then
      Print ln;
    Else
      Print : Print "Error : Sequence contains space(s)";
      Exit While
    End If
  End If
Wend

Close #1

Print : Print
Print "Press any key to quit"
Sleep
```


```txt

Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED

```



## Gambas


```gambas
Public Sub Main()
Dim sList As String = File.Load("../FASTA")
Dim sTemp, sOutput As String

For Each sTemp In Split(sList, gb.NewLine)
  If sTemp Begins ">" Then
    If sOutput Then Print sOutput
    sOutput = Right(sTemp, -1) & ": "
  Else
    sOutput &= sTemp
  Endif
Next

Print sOutput

End
```

Output:

```txt

Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED

```



## Go


```go
package main

import (
        "bufio"
        "fmt"
        "os"
)

func main() {
        f, err := os.Open("rc.fasta")
        if err != nil {
                fmt.Println(err)
                return
        }
        defer f.Close()
        s := bufio.NewScanner(f)
        headerFound := false
        for s.Scan() {
                line := s.Text()
                switch {
                case line == "":
                        continue
                case line[0] != '>':
                        if !headerFound {
                                fmt.Println("missing header")
                                return
                        }
                        fmt.Print(line)
                case headerFound:
                        fmt.Println()
                        fallthrough
                default:
                        fmt.Printf("%s: ", line[1:])
                        headerFound = true
                }
        }
        if headerFound {
                fmt.Println()
        }
        if err := s.Err(); err != nil {
                fmt.Println(err)
        }
}
```

```txt

Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED

```



## Haskell

We pass the file path as an argument to the parseFasta function, which only does the file loading and result printing.

'''The first way'''

We parse FASTA by hand (generally not a recommended approach). We use the fact that groupBy walks the list from the head and groups the items by a predicate; here we first concatenate all the fasta strings and then pair those with each respective name.


```haskell
import Data.List ( groupBy )

parseFasta :: FilePath -> IO ()
parseFasta fileName = do
  file <- readFile fileName
  let pairedFasta = readFasta $ lines file
  mapM_ (\(name, code) -> putStrLn $ name ++ ": " ++ code) pairedFasta

readFasta :: [String] -> [(String, String)]
readFasta = pair . map concat . groupBy (\x y -> notName x && notName y)
 where
  notName :: String -> Bool
  notName = (/=) '>' . head

  pair :: [String] -> [(String, String)]
  pair []           = []
  pair (x : y : xs) = (drop 1 x, y) : pair xs
```


```txt
Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED
```


'''The second way'''

We parse FASTA using parser combinators. Normally you'd use something like Trifecta or Parsec, but here we use ReadP, because it is simple and also included in ghc by default. With other parsing libraries the code would be almost the same.


```haskell
import Text.ParserCombinators.ReadP
import Control.Applicative ( (<|>) )
import Data.Char ( isAlpha, isAlphaNum )

parseFasta :: FilePath -> IO ()
parseFasta fileName = do
  file <- readFile fileName
  let pairs = fst . last . readP_to_S readFasta $ file
  mapM_ (\(name, code) -> putStrLn $ name ++ ": " ++ code) pairs


readFasta :: ReadP [(String, String)]
readFasta = many pair <* eof
 where
  pair    = (,) <$> name <*> code
  name    = char '>' *> many (satisfy isAlphaNum <|> char '_') <* newline
  code    = concat <$> many (many (satisfy isAlpha) <* newline)
  newline = char '\n'
```


```txt
Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED
```



## J

Needs chunking to handle huge files.

```j
require 'strings'  NB. not needed for J versions greater than 6.
parseFasta=: ((': ' ,~ LF&taketo) , (LF -.~ LF&takeafter));._1
```

'''Example Usage'''

```j
   Fafile=: noun define
>Rosetta_Example_1
THERECANBENOSPACE
>Rosetta_Example_2
THERECANBESEVERAL
LINESBUTTHEYALLMUST
BECONCATENATED
)
   parseFasta Fafile
Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED
```



## Java

```java
import java.io.*;
import java.util.Scanner;

public class ReadFastaFile {

    public static void main(String[] args) throws FileNotFoundException {

        boolean first = true;

        try (Scanner sc = new Scanner(new File("test.fasta"))) {
            while (sc.hasNextLine()) {
                String line = sc.nextLine().trim();
                if (line.charAt(0) == '>') {
                    if (first)
                        first = false;
                    else
                        System.out.println();
                    System.out.printf("%s: ", line.substring(1));
                } else {
                    System.out.print(line);
                }
            }
        }
        System.out.println();
    }
}
```



```txt
Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED
Rosetta_Example_3: THISISFASTA
```



## jq

The following implementation uses "foreach" and "inputs"
so that very large input files can be processed with minimal space requirements:
in each cycle, only as many lines are read as are required to compose an output line.

Notice that an additional ">" must be provided to "foreach" to ensure the final block of lines of the input file are properly assembled.

```jq

def fasta:
  foreach (inputs, ">") as $line
    # state: [accumulator, print ]
    ( [null, null];
      if $line[0:1] == ">" then [($line[1:] + ": "), .[0]]
      else [ (.[0] + $line), false]
      end;
      if .[1] then .[1] else empty end )
    ;

fasta
```

```sh
$ jq -n -R -r -f FASTA_format.jq < FASTA_format.fasta
Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED
```



## Julia

```julia
for line in eachline("data/fasta.txt")
    if startswith(line, '>')
        print(STDOUT, "\n$(line[2:end]): ")
    else
        print(STDOUT, "$line")
    end
end
```



## Kotlin

```scala
// version 1.1.2

import java.util.Scanner
import java.io.File

fun checkNoSpaces(s: String) = ' ' !in s && '\t' !in s

fun main(args: Array<String>) {
    var first = true
    val sc = Scanner(File("input.fasta"))
    while (sc.hasNextLine()) {
        val line = sc.nextLine()
        if (line[0] == '>') {
            if (!first) println()
            print("${line.substring(1)}: ")
            if (first) first = false
        }
        else if (first) {
            println("Error : File does not begin with '>'")
            break
        }
        else if (checkNoSpaces(line))
            print(line)
        else {
            println("\nError : Sequence contains space(s)")
            break
        }
    }
    sc.close()
}
```


```txt

Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED

```



## Lua


```lua
local file = io.open("input.txt","r")
local data = file:read("*a")
file:close()

local output = {}
local key = nil

-- iterate through lines
for line in data:gmatch("(.-)\r?\n") do
	if line:match("%s") then
		error("line contained space")
	elseif line:sub(1,1) == ">" then
		key = line:sub(2)
		-- if key already exists, append to the previous input
		output[key] = output[key] or ""
	elseif key ~= nil then
		output[key] = output[key] .. line
	end
end

-- print result
for k,v in pairs(output) do
	print(k..": "..v)
end
```


```txt

Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED

```



## OCaml

I keep it simple by sticking to the description of the FASTA format described in the task.

The program reads and processes the input one line at a time, and directly prints out the chunk of data available. The long strings are not concatenated in memory but just examined and processed as necessary: either printed out as is in the case of part of a sequence, or formatted in the case of the name (what I call the label), and managing the new lines where needed.
```ocaml

(* This program reads from the standard input and writes to standard output.
 * Examples of use:
 *    $ ocaml fasta.ml < fasta_file.txt
 *    $ ocaml fasta.ml < fasta_file.txt > my_result.txt
 *
 * The FASTA file is assumed to have a specific format, where the first line
 * contains a label in the form of '>blablabla', i.e. with a '>' as the first
 * character.
 *)

let labelstart = '>'

let is_label s = s.[0] = labelstart
let get_label s = String.sub s 1 (String.length s - 1)

let read_in channel = input_line channel |> String.trim

let print_fasta chan =
  let rec doloop currlabel line =
    if is_label line then begin
        if currlabel <> "" then print_newline ();
        let newlabel = get_label line in
        print_string (newlabel ^ ": ");
        doloop newlabel (read_in chan)
    end
    else begin
        print_string line;
        doloop currlabel (read_in chan)
    end
  in
  try
    match read_in chan with
    | line when is_label line -> doloop "" line
    | _ -> failwith "Badly formatted FASTA file?"
  with
    End_of_file -> print_newline ()


let () =
  print_fasta stdin

```

 Rosetta_Example_1: THERECANBENOSPACE
 Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED


## M2000 Interpreter

Spaghetti code, using Goto, but works using partially reading of an input stream, with no known size of each reading (supposed data transmitted). We make an object as a FASTA_MACHINE, and run it. Object produce events, so we have some functions for services. These functions called as subs, but we have to use New if we want to shadow any same named variable. (subs always include the New (a Read New) so we didn't use there). If there no modules variables with same names as for arguments for these functions then we can exclude New. All these functions have same scope as the module where they belong.

We can use ";" for comments, ">" for title. We can input one char, or many, in each input packet. Linefeed by default is CRLF. Whitespaces are spaces, nbsp, and tabs.



```M2000 Interpreter

Module CheckIt {
      Class FASTA_MACHINE {
            Events "GetBuffer", "header", "DataLine", "Quit"
      Public:
            Module Run {
                  Const lineFeed$=chr$(13)+chr$(10)
                  Const WhiteSpace$=" "+chr$(9)+chrcode$(160)
                  Def long state=1, idstate=1
                  Def boolean Quit=False
                  Def Buf$, waste$, Packet$
            GetNextPacket:
                        Call Event "Quit", &Quit
                        If Quit then exit
                        Call Event "GetBuffer", &Packet$
                        Buf$+=Packet$
                        If len(Buf$)=0 Then exit
                        On State Goto GetStartIdentifier, GetIdentifier, GetStartData, GetData, GetStartIdentifier2
                        exit
            GetStartIdentifier:
                        waste$=rightpart$(Buf$, ">")
            GetStartIdentifier2:
                        If len(waste$)=0 Then waste$=rightpart$(Buf$, ";") : idstate=2
                        If len(waste$)=0 Then idstate=1 : Goto GetNextPacket ' we have to read more
                        buf$=waste$
                        state=2
            GetIdentifier:
                        If Len(Buf$)=len(lineFeed$) then {
                              if buf$<>lineFeed$ then Goto GetNextPacket
                              waste$=""
                        } Else {
                              if instr(buf$, lineFeed$)=0 then Goto GetNextPacket
                              waste$=rightpart$(Buf$, lineFeed$)
                          }
                        If idstate=2 Then {
                            idstate=1
                            \\ it's a comment, drop it
                            state=1
                            Goto GetNextPacket
                        } Else Call Event "header", filter$(leftpart$(Buf$,lineFeed$), WhiteSpace$)
                        Buf$=waste$
                        State=3
            GetStartData:
                        while left$(buf$, 2)=lineFeed$ {buf$=Mid$(buf$,3)}
                        waste$=Leftpart$(Buf$, lineFeed$)
                        If len(waste$)=0 Then  Goto GetNextPacket ' we have to read more
                        waste$=Filter$(waste$,WhiteSpace$)
                        Call Event "DataLine", leftpart$(Buf$,lineFeed$)
                        Buf$=Rightpart$(Buf$,lineFeed$)
                        state=4
            GetData:
                        while left$(buf$, 2)=lineFeed$ {buf$=Mid$(buf$,3)}
                        waste$=Leftpart$(Buf$, lineFeed$)
                        If len(waste$)=0 Then  Goto GetNextPacket ' we have to read more
                        If Left$(waste$,1)=";" Then wast$="": state=5 : Goto GetStartIdentifier2
                        If Left$(waste$,1)=">" Then state=1 : Goto GetStartIdentifier
                        waste$=Filter$(waste$,WhiteSpace$)
                        Call Event "DataLine", waste$
                        Buf$=Rightpart$(Buf$,lineFeed$)
                        Goto GetNextPacket
            }
      }
      Group WithEvents K=FASTA_MACHINE()
      Document Final$, Inp$

      \\ In documents, "="" used for append data. Final$="append this"
      Const NewLine$=chr$(13)+chr$(10)
      Const Center=2
      \\ Event's Functions
      Function K_GetBuffer (New &a$) {
            Input "IN:", a$
            inp$=a$+NewLine$
            while right$(a$, 1)="\" {
                  Input "IN:", b$
                  inp$=b$+NewLine$
                  if b$="" then b$="n"
                  a$+=b$
            }
            a$= replace$("\N","\n", a$)
            a$= replace$("\n",NewLine$, a$)
      }
      Function K_header (New a$) {
            iF Doc.Len(Final$)=0 then {
                  Final$=a$+": "
            } Else Final$=Newline$+a$+": "
      }
      Function K_DataLine (New a$) {
            Final$=a$
      }
      Function K_Quit (New &q) {
            q=keypress(1)
      }
      Cls , 0
      Report Center, "FASTA Format"
      Report "Simulate input channel in packets (\n for new line). Use empty input to exit after new line, or press left mouse button and Enter to quit. Use ; to write comments. Use > to open a title"
      Cls, row  ' scroll from current row
      K.Run
      Cls
      Report Center, "Input File"
      Report Inp$
      Report Center, "Output File"
      Report Final$
}
checkit

```



## Mathematica

Mathematica has built-in support for FASTA files and strings

```Mathematica
ImportString[">Rosetta_Example_1
 THERECANBENOSPACE
 >Rosetta_Example_2
 THERECANBESEVERAL
 LINESBUTTHEYALLMUST
 BECONCATENATED
 ", "FASTA"]
```

```txt
{"THERECANBENOSPACE", "THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED"}
```



## Nim


```Nim

import strutils

let input = """>Rosetta_Example_1
    THERECANBENOSPACE
    >Rosetta_Example_2
    THERECANBESEVERAL
    LINESBUTTHEYALLMUST
    BECONCATENATED""".unindent

proc fasta*(input: string) =
    var row = ""
    for line in input.splitLines:
        if line.startsWith(">"):
            if row != "": echo row
            row = line[1..^1] & ": "
        else:
            row &= line.strip
    echo row

fasta(input)

```

```txt

Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED

```



## Pascal


```Pascal

program FASTA_Format;
// FPC 3.0.2
var InF,
    OutF: Text;
    ch: char;
    First: Boolean=True;
    InDef: Boolean=False;

begin
  Assign(InF,'');
  Reset(InF);
  Assign(OutF,'');
  Rewrite(OutF);
  While Not Eof(InF) do
  begin
    Read(InF,ch);
    Case Ch of
      '>': begin
            if Not(First) then
              Write(OutF,#13#10)
            else
              First:=False;
            InDef:=true;
          end;
      #13: Begin
               if InDef then
               begin
                 InDef:=false;
                 Write(OutF,': ');
               end;
               Ch:=#0;
             end;
      #10: ch:=#0;
      else Write(OutF,Ch);
    end;
  end;
  Close(OutF);
  Close(InF);
end.

```


FASTA_Format < test.fst

```txt

Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED

```



## Perl


```perl
my $fasta_example = <<'END_FASTA_EXAMPLE';
>Rosetta_Example_1
THERECANBENOSPACE
>Rosetta_Example_2
THERECANBESEVERAL
LINESBUTTHEYALLMUST
BECONCATENATED
END_FASTA_EXAMPLE

my $num_newlines = 0;
while ( < $fasta_example > ) {
	if (/\A\>(.*)/) {
		print "\n" x $num_newlines, $1, ': ';
	}
	else {
		$num_newlines = 1;
		print;
	}
}
```

```txt

Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED

```



## Perl 6


```perl6
grammar FASTA {

    rule TOP    { <entry>+ }
    rule entry  { \> <title> <sequence> }
    token title { <.alnum>+ }
    token sequence { ( <.alnum>+ )+ % \n { make $0.join } }

}

FASTA.parse: q:to /§/;
>Rosetta_Example_1
THERECANBENOSPACE
>Rosetta_Example_2
THERECANBESEVERAL
LINESBUTTHEYALLMUST
BECONCATENATED
§

for $/<entry>[] {
    say ~.<title>, " : ", .<sequence>.made;
}
```

```txt
Rosetta_Example_1 : THERECANBENOSPACE
Rosetta_Example_2 : THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED
```



## Phix


```Phix
bool first = true
integer fn = open("fasta.txt","r")
if fn=-1 then ?9/0 end if
while true do
    object line = trim(gets(fn))
    if atom(line) then puts(1,"\n") exit end if
    if length(line) then
        if line[1]=='>' then
            if not first then puts(1,"\n") end if
            printf(1,"%s: ",{line[2..$]})
            first = false
        elsif first then
            printf(1,"Error : File does not begin with '>'\n")
            exit
        elsif not find_any(" \t",line) then
            puts(1,line)
        else
            printf(1,"\nError : Sequence contains space(s)\n")
            exit
        end if
    end if
end while
close(fn)
```

```txt

Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED

```



## PicoLisp


```PicoLisp
(de fasta (F)
   (in F
      (while (from ">")
         (prin (line T) ": ")
         (until (or (= ">" (peek)) (eof))
            (prin (line T)) )
         (prinl) ) ) )
(fasta "fasta.dat")
```

```txt

Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED

```



## PowerShell

When working with a real file, the content of the <code>$file</code> variable would be: <code>Get-Content -Path .\FASTA_file.txt -ReadCount 1000</code>.  The <code>-ReadCount</code> parameter value for large files is unknown, yet sure to be a value between 1,000 and 10,000 depending upon the length of file and length of the records in the file.  Experimentation is the only way to know the optimum value.
```PowerShell

$file = @'
>Rosetta_Example_1
THERECANBENOSPACE
>Rosetta_Example_2
THERECANBESEVERAL
LINESBUTTHEYALLMUST
BECONCATENATED
'@

$lines = $file.Replace("`n","~").Split(">").ForEach({$_.TrimEnd("~").Split("`n",2,[StringSplitOptions]::RemoveEmptyEntries)})

$output = New-Object -TypeName PSObject

foreach ($line in $lines)
{
    $name, $value = $line.Split("~",2).ForEach({$_.Replace("~","")})

    $output | Add-Member -MemberType NoteProperty -Name $name -Value $value
}

$output | Format-List

```

```txt

Rosetta_Example_1 : THERECANBENOSPACE
Rosetta_Example_2 : THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED

```



### Version 3.0 Or Less


```PowerShell

$file = @'
>Rosetta_Example_1
THERECANBENOSPACE
>Rosetta_Example_2
THERECANBESEVERAL
LINESBUTTHEYALLMUST
BECONCATENATED
'@

$lines = $file.Replace("`n","~").Split(">") | ForEach-Object {$_.TrimEnd("~").Split("`n",2,[StringSplitOptions]::RemoveEmptyEntries)}

$output = New-Object -TypeName PSObject

foreach ($line in $lines)
{
    $name, $value = $line.Split("~",2) | ForEach-Object {$_.Replace("~","")}

    $output | Add-Member -MemberType NoteProperty -Name $name -Value $value
}

$output | Format-List

```

```txt

Rosetta_Example_1 : THERECANBENOSPACE
Rosetta_Example_2 : THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED

```



## PureBasic


```PureBasic
EnableExplicit
Define Hdl_File.i,
       Frm_File.i,
       c.c,
       header.b

Hdl_File=ReadFile(#PB_Any,"c:\code_pb\rosettacode\data\FASTA_TEST.txt")
If Not IsFile(Hdl_File) : End -1 : EndIf
Frm_File=ReadStringFormat(Hdl_File)

If OpenConsole("FASTA format")
  While Not Eof(Hdl_File)
    c=ReadCharacter(Hdl_File,Frm_File)
    Select c
      Case '>'
        header=#True
        PrintN("")
      Case #LF, #CR
        If header
          Print(": ")
          header=#False
        EndIf
      Default
        Print(Chr(c))
    EndSelect
  Wend
  CloseFile(Hdl_File)
  Input()
EndIf
```

```txt
Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED
```



## Python

I use a string to mimic an input file.
If it was an input file, then the file is read line-by-line
and I use a generator expression yielding key, value pairs
as soon as they are read, keeping the minimum in memory.

```python
import io

FASTA='''\
>Rosetta_Example_1
THERECANBENOSPACE
>Rosetta_Example_2
THERECANBESEVERAL
LINESBUTTHEYALLMUST
BECONCATENATED'''

infile = io.StringIO(FASTA)

def fasta_parse(infile):
    key = ''
    for line in infile:
        if line.startswith('>'):
            if key:
                yield key, val
            key, val = line[1:].rstrip().split()[0], ''
        elif key:
            val += line.rstrip()
    if key:
        yield key, val

print('\n'.join('%s: %s' % keyval for keyval in fasta_parse(infile)))
```


```txt
Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED
```


## R


```rsplus

library("seqinr")

data <- c(">Rosetta_Example_1","THERECANBENOSPACE",">Rosetta_Example_2","THERECANBESEVERAL","LINESBUTTHEYALLMUST","BECONCATENATED")
fname <- "rosettacode.fasta"
f <- file(fname,"w+")
writeLines(data,f)
close(f)

fasta <- read.fasta(file = fname, as.string = TRUE, seqtype = "AA")
for (aline in fasta) {
  cat(attr(aline, 'Annot'), ":", aline, "\n")
}

```

```txt

>Rosetta_Example_1 : THERECANBENOSPACE
>Rosetta_Example_2 : THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED

```



## Racket


```racket

#lang racket
(let loop ([m #t])
  (when m
    (when (regexp-try-match #rx"^>" (current-input-port))
      (unless (eq? #t m) (newline))
      (printf "~a: " (read-line)))
    (loop (regexp-match #rx"\n" (current-input-port) 0 #f
                        (current-output-port)))))
(newline)

```



## REXX

Neither REXX version reads the entire file into memory at one time;   lines are processed as they are read (one line at a time).

### version 1

This REXX version correctly processes the examples shown.

```rexx
/*REXX program reads a (bio-informational)  FASTA  file  and  displays the contents.    */
parse arg iFID .                                 /*iFID:  the input file to be read.    */
if iFID==''  then iFID='FASTA.IN'                /*Not specified?  Then use the default.*/
name=                                            /*the name of an output file (so far). */
$=                                               /*the value of the output file's stuff.*/
     do  while  lines(iFID)\==0                  /*process the  FASTA  file contents.   */
     x=strip( linein(iFID), 'T')                 /*read a line (a record) from the file,*/
                                                 /*───────── and strip trailing blanks. */
     if left(x, 1)=='>'  then do
                              if $\==''  then say name':'  $
                              name=substr(x,  2)
                              $=
                              end
                         else $=$ || x
     end   /*j*/                                 /* [↓]  show output of last file used. */
if $\==''  then say name':'  $                   /*stick a fork in it,  we're all done. */
```

```txt

Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED

```



### version 2

This REXX version handles   (see the ''talk'' page):
::*   blank lines
::*   sequences that end in an asterisk   ['''*''']
::*   sequences that contain blanks, tabs, and other whitespace
::*   sequence names that are identified with a semicolon   [''';''']

```rexx
/*REXX program reads a  (bio-informational)  FASTA  file  and  displays the contents.   */
parse arg iFID .                                 /*iFID:  the input file to be read.    */
if iFID==''  then iFID='FASTA2.IN'               /*Not specified?  Then use the default.*/
name=                                            /*the name of an output file (so far). */
$=                                               /*the value of the output file's stuff.*/
     do  while  lines(iFID)\==0                  /*process the  FASTA  file contents.   */
     x=strip( linein(iFID), 'T')                 /*read a line (a record) from the file,*/
                                                 /*───────── and strip trailing blanks. */
     if x==''  then iterate                      /*If the line is all blank, ignore it. */
     if left(x, 1)==';'  then do
                              if name=='' then name=substr(x,2)
                              say x
                              iterate
                              end
     if left(x, 1)=='>'  then do
                              if $\==''  then say name':'  $
                              name=substr(x, 2)
                              $=
                              end
                         else $=space($ || translate(x, , '*'),  0)
     end   /*j*/                                 /* [↓]  show output of last file used. */
if $\==''  then say name':'  $                   /*stick a fork in it,  we're all done. */
```


```txt

'''input:'''   The   '''FASTA2.IN'''   file is shown below:

```txt

;LCBO - Prolactin precursor - Bovine
; a sample sequence in FASTA format
MDSKGSSQKGSRLLLLLVVSNLLLCQGVVSTPVCPNGPGNCQVSLRDLFDRAVMVSHYIHDLSS
EMFNEFDKRYAQGKGFITMALNSCHTSSLPTPEDKEQAQQTHHEVLMSLILGLLRSWNDPLYHL
VTEVRGMKGAPDAILSRAIEIEEENKRLLEGMEMIFGQVIPGAKETEPYPVWSGLPSLQTKDED
ARYSAFYNLLHCLRRDSSKIDTYLKLLNCRIIYNNNC*

>MCHU - Calmodulin - Human, rabbit, bovine, rat, and chicken
ADQLTEEQIAEFKEAFSLFDKDGDGTITTKELGTVMRSLGQNPTEAELQDMINEVDADGNGTID
FPEFLTMMARKMKDTDSEEEIREAFRVFDKDGNGYISAAELRHVMTNLGEKLTDEEVDEMIREA
DIDGDGQVNYEEFVQMMTAK*

>gi|5524211|gb|AAD44166.1| cytochrome b [Elephas maximus maximus]
LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV
EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG
LLILILLLLLLALLSPDMLGDPDNHMPADPLNTPLHIKPEWYFLFAYAILRSVPNKLGGVLALFLSIVIL
GLMPFLHTSKHRSMMLRPLSQALFWTLTMDLLTLTWIGSQPVEYPYTIIGQMASILYFSIILAFLPIAGX
IENY

```

```txt

;LCBO - Prolactin precursor - Bovine
; a sample sequence in FASTA format
LCBO - Prolactin precursor - Bovine: MDSKGSSQKGSRLLLLLVVSNLLLCQGVVSTPVCPNGPGNCQVSLRDLFDRAVMVSHYIHDLSSEMFNEFDKRYAQGKGFITMALNSCHTSSLPTPEDKEQAQQTHHEVLMSLILGLLRSWNDPLYHLVTEVRGMKGAPDAILSRAIEIEEENKRLLEGMEMIFGQVIPGAKETEPYPVWSGLPSLQTKDEDARYSAFYNLLHCLRRDSSKIDTYLKLLNCRIIYNNNC
MCHU - Calmodulin - Human, rabbit, bovine, rat, and chicken: ADQLTEEQIAEFKEAFSLFDKDGDGTITTKELGTVMRSLGQNPTEAELQDMINEVDADGNGTIDFPEFLTMMARKMKDTDSEEEIREAFRVFDKDGNGYISAAELRHVMTNLGEKLTDEEVDEMIREADIDGDGQVNYEEFVQMMTAK
gi|5524211|gb|AAD44166.1| cytochrome b [Elephas maximus maximus]: LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLVEWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLGLLILILLLLLLALLSPDMLGDPDNHMPADPLNTPLHIKPEWYFLFAYAILRSVPNKLGGVLALFLSIVILGLMPFLHTSKHRSMMLRPLSQALFWTLTMDLLTLTWIGSQPVEYPYTIIGQMASILYFSIILAFLPIAGXIENY

```



## Ring


```ring

# Project : FAST format

a = ">Rosetta_Example_1
THERECANBENOSPACE
>Rosetta_Example_2
THERECANBESEVERAL
LINESBUTTHEYALLMUST
BECONCATENATED"

i = 1
while i <= len(a)
      if substr(a,i,17) = ">Rosetta_Example_"
         see nl
         see substr(a,i,18) + ": " + nl
         i = i + 17
      else
         if ascii(substr(a,i,1)) > 20
            see a[i]
         ok
      ok
      i = i + 1
end

```

Output:

```txt

>Rosetta_Example_1: THERECANBENOSPACE
>Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED

```



## Ruby


```ruby
def fasta_format(strings)
  out, text = [], ""
  strings.split("\n").each do |line|
    if line[0] == '>'
      out << text unless text.empty?
      text = line[1..-1] + ": "
    else
      text << line
    end
  end
  out << text unless text.empty?
end

data = <<'EOS'
>Rosetta_Example_1
THERECANBENOSPACE
>Rosetta_Example_2
THERECANBESEVERAL
LINESBUTTHEYALLMUST
BECONCATENATED
EOS

puts fasta_format(data)
```


```txt

Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED

```



## Run BASIC


```runbasic
a$ = ">Rosetta_Example_1
THERECANBENOSPACE
>Rosetta_Example_2
THERECANBESEVERAL
LINESBUTTHEYALLMUST
BECONCATENATED"

i = 1
while i <= len(a$)
  if mid$(a$,i,17) = ">Rosetta_Example_" then
    print
    print mid$(a$,i,18);": ";
    i = i + 17
   else
    if asc(mid$(a$,i,1)) > 20 then print mid$(a$,i,1);
  end if
  i = i + 1
wend
```

```txt
>Rosetta_Example_1: THERECANBENOSPACE
>Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED
```



## Rust


This example is implemented using an [https://doc.rust-lang.org/book/iterators.html iterator] to reduce memory requirements and encourage code reuse.


```rust

use std::env;
use std::io::{BufReader, Lines};
use std::io::prelude::*;
use std::fs::File;

fn main() {
    let args: Vec<String> = env::args().collect();
    let f = File::open(&args[1]).unwrap();
    for line in FastaIter::new(f) {
        println!("{}", line);
    }
}

struct FastaIter<T> {
    buffer_lines: Lines<BufReader<T>>,
    current_name: Option<String>,
    current_sequence: String
}

impl<T: Read> FastaIter<T> {
    fn new(file: T) -> FastaIter<T> {
        FastaIter { buffer_lines: BufReader::new(file).lines(),
                    current_name: None,
                    current_sequence: String::new() }
    }
}

impl<T: Read> Iterator for FastaIter<T> {
    type Item = String;

    fn next(&mut self) -> Option<String> {
        while let Some(l) = self.buffer_lines.next() {
            let line = l.unwrap();
            if line.starts_with(">") {
                if self.current_name.is_some() {
                    let mut res = String::new();
                    res.push_str(self.current_name.as_ref().unwrap());
                    res.push_str(": ");
                    res.push_str(&self.current_sequence);
                    self.current_name = Some(String::from(&line[1..]));
                    self.current_sequence.clear();
                    return Some(res);
                } else {
                    self.current_name = Some(String::from(&line[1..]));
                    self.current_sequence.clear();
                }
                continue;
            }
            self.current_sequence.push_str(line.trim());
        }
        if self.current_name.is_some() {
            let mut res = String::new();
            res.push_str(self.current_name.as_ref().unwrap());
            res.push_str(": ");
            res.push_str(&self.current_sequence);
            self.current_name = None;
            self.current_sequence.clear();
            self.current_sequence.shrink_to_fit();
            return Some(res);
        }
        None
    }
}

```

```txt
Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED
```



## Scala


```scala
import java.io.File
import java.util.Scanner

object ReadFastaFile extends App {
  val sc = new Scanner(new File("test.fasta"))
  var first = true

  while (sc.hasNextLine) {
    val line = sc.nextLine.trim
    if (line.charAt(0) == '>') {
      if (first) first = false
      else println()
      printf("%s: ", line.substring(1))
    }
    else print(line)
  }

  println("~~~+~~~")
}
```


## Scheme


```scheme
(import (scheme base)
        (scheme file)
        (scheme write))

(with-input-from-file ; reads text from named file, one line at a time
  "fasta.txt"
  (lambda ()
    (do ((first-line? #t #f)
         (line (read-line) (read-line)))
      ((eof-object? line) (newline))
      (cond ((char=? #\> (string-ref line 0)) ; found a name
             (unless first-line? ; no newline on first name
               (newline))
             (display (string-copy line 1)) (display ": "))
            (else ; display the string directly
              (display line))))))
```

```txt
Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var file: fastaFile is STD_NULL;
    var string: line is "";
    var boolean: first is TRUE;
  begin
    fastaFile := open("fasta_format.in", "r");
    if fastaFile <> STD_NULL then
      while hasNext(fastaFile) do
        line := getln(fastaFile);
        if startsWith(line, ">") then
          if first then
            first := FALSE;
          else
            writeln;
          end if;
          write(line[2 ..] <& ": ");
        else
          write(line);
        end if;
      end while;
      close(fastaFile);
    end if;
    writeln;
  end func;
```


```txt

Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED

```



## Sidef

```ruby
func fasta_format(strings) {
    var out = []
    var text = ''
    for line in (strings.lines) {
        if (line.begins_with('>')) {
            text.len && (out << text)
            text = line.substr(1)+': '
        }
        else {
            text += line
        }
    }
    text.len && (out << text)
    return out
}

fasta_format(DATA.slurp).each { .say }

__DATA__
>Rosetta_Example_1
THERECANBENOSPACE
>Rosetta_Example_2
THERECANBESEVERAL
LINESBUTTHEYALLMUST
BECONCATENATED
```

```txt

Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED

```



## Tcl


```tcl
proc fastaReader {filename} {
    set f [open $filename]
    set sep ""
    while {[gets $f line] >= 0} {
	if {[string match >* $line]} {
	    puts -nonewline "$sep[string range $line 1 end]: "
	    set sep "\n"
	} else {
	    puts -nonewline $line
	}
    }
    puts ""
    close $f
}

fastaReader ./rosettacode.fas
```

```txt

Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED

```



## zkl


```zkl
fcn fasta(data){ // a lazy cruise through a FASTA file
   fcn(w){      // one string at a time, -->False garbage at front of file
      line:=w.next().strip();
      if(line[0]==">") w.pump(line[1,*]+": ",'wrap(l){
         if(l[0]==">") { w.push(l); Void.Stop } else l.strip()
      })
   }.fp(data.walker()) : Utils.Helpers.wap(_);
}
```

*This assumes that white space at front or end of string is extraneous (excepting ">" lines).
*Lazy, works for objects that support iterating over lines (ie most).
*The fasta function returns an iterator that wraps a function taking an iterator. Uh, yeah. An initial iterator (Walker) is used to get lines, hold state and do push back when read the start of the next string. The function sucks up one string (using the iterator). The wrapping iterator (wap) traps the exception when the function waltzes off the end of the data and provides API for foreach (etc).
FASTA file:

```zkl
foreach l in (fasta(File("fasta.txt"))) { println(l) }
```

FASTA data blob:

```zkl
data:=Data(0,String,
   ">Rosetta_Example_1\nTHERECANBENOSPACE\n"
   ">Rosetta_Example_2\nTHERECANBESEVERAL\nLINESBUTTHEYALLMUST\n"
     "BECONCATENATED");
foreach l in (fasta(data)) { println(l) }
```

```txt

Rosetta_Example_1: THERECANBENOSPACE
Rosetta_Example_2: THERECANBESEVERALLINESBUTTHEYALLMUSTBECONCATENATED

```

