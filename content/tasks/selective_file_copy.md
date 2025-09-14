+++
title = "Selective File Copy"
description = ""
date = 2019-08-30T10:26:00Z
aliases = []
[extra]
id = 19065
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "algol_68",
  "awk",
  "cobol",
  "fortran",
  "go",
  "java",
  "kotlin",
  "netrexx",
  "oorexx",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "racket",
  "rexx",
  "zkl",
]
+++

Copy part of input records to an output file.

Show how file processing as known from PL/I or COBOL can be implemented in the
language of your choice.

Here, a file is not 'just' a sequence of bytes or lines but a sequence of ''records'' (structured data). The structure is usually described by declarations contained in an ''INCLUDE'' file (PL/I) or ''COPY BOOK'' (COBOL).

The ''by name'' assignment is a little extra available in PL/I. 

Data conversions may be necessary (as shown here for data element ''c'' in the Go listing).


## ALGOL 68

Using formatted transput to process similar files to the COBOL sample.

```algol68
MODE INPUTRECORD     = STRUCT( STRING field a, STRING field b, INT field c, CHAR sign of field c, STRING field d );
MODE OUTPUTRECORD    = STRUCT( STRING field a,                 INT field c,                       STRING field x );

FORMAT input format  = $ 5a, 5a,   4d, 1a, 5a l $;
FORMAT output format = $ 5a,     3z-d,     5a l $;

IF  FILE input file;
    STRING file name = "input.txt";
    open( input file, file name, stand in channel ) /= 0
THEN
    # failed to open the file #
    print( (  "Unable to open """ + file name + """", newline ) )
ELIF FILE output file;
     STRING output name = "output.txt";
     open( output file, output name, stand out channel ) /= 0
THEN
    # failed to open the otput file #
    print( (  "Unable to open """ + output name + """", newline ) );
    close( input file )
ELSE
    # files opened OK #
    BOOL at eof := FALSE;
    # set the EOF handler for the file #
    on logical file end( input file, ( REF FILE f )BOOL:
                                     BEGIN
                                         # note that we reached EOF on the #
                                         # latest read #
                                         at eof := TRUE;
                                         # return TRUE so processing can continue #
                                         TRUE
                                     END
                       );
    WHILE INPUTRECORD in record;
          getf( input file, ( input format,  in record ) );
          NOT at eof
    DO
        IF sign of field c OF in record = "-" THEN field c OF in record *:= -1 FI;
        OUTPUTRECORD out record;
        field a OF out record := field a OF in record;
        field c OF out record := field c OF in record;
        field x OF out record := "XXXXX";
        putf( output file, ( output format, out record ) )
    OD;
    # close the file #
    close( output file );
    close( input file  )
FI
```


Input file:

```txt

A    bbbbB0001+d2345
AA   bbbBB0002+1d345
AAA  bbBBB0003+12d45
AAAA bBBBB0001-123d5
AAAAABBBBB0002-1234d

```

```txt

A        1XXXXX
AA       2XXXXX
AAA      3XXXXX
AAAA    -1XXXXX
AAAAA   -2XXXXX

```



## AWK

<!-- http://ideone.com/oPMkNK -->

There is no delimiter between input-fields, so we extract the fields from the whole input-record $0 as substrings using substr(). 

Output and formatting is done using printf().


```awk
# usage:  gawk -f sfcopy.awk input.txt

BEGIN {
    print "# Selective File Copy:"
}

{
  # print NR ": <" $0 ">"  ## debug: print input

    F1 = substr($0,  1,5)
    F2 = substr($0, 11,4)
    F3 = substr($0, 15,1)
    if(F3=="-") {F2 = 0-F2}
    F9 = "xxxxx"
  
    printf("%-5s%5d%s\n", F1, F2, F9)
}

END {
    print "# Done."
}

```


```txt

# Selective File Copy:
A        1xxxxx
AA       2xxxxx
AAA      3xxxxx
AAAA    -1xxxxx
AAAAA   -2xxxxx
# Done.

```



## COBOL

Seeing as this task mentioned COBOL, here is a sample, but there is no JSON
involved, just a simple data record layout and a MOVE CORRESPONDING.

This is GnuCOBOL on a GNU/Linux system, so let's start with a text file, field A
through D, 5 bytes each, 21 character lines, counting the newline, converted
into a more COBOLish fixed length form of 20 bytes per record with no newlines.


```txt

prompt$ cat selective-input-file.txt
A    B    0001+D
AA   BB   0002+DD
AAA  BBB  0003+DDD
AAAA BBBB 0004-DDDD
AAAAABBBBB0005-DDDDD

prompt$ $ dd if=selective-input-file.txt of=selective-input-file cbs=20 conv=block
0+1 records in
0+1 records out
100 bytes (100 B) copied, 0.000451344 s, 222 kB/s

```


There are no newlines in the SEQUENTIAL access file now; just 5, 20 byte
records.  The numeric field-c is explained below.

'''copy books'''
In the selective-copy.cob example, instead of some COPY directives with input
and output record layout copy books, an inline REPLACE directive is used
instead, to keep the source file encapsulated.  The data between the "=="
markers after the REPLACE BY phrase would normally be saved in a separate file,
and read in during compile with '''COPY filename'''.  Those copy books could
then be used by any source module that would need the record layout. In this
example, the :INPUT-RECORD: and :OUTPUT-RECORD: pseudo-text markers are where
the COPY directives would be placed.

Far more complicated record hierarchies can be created with COBOL data level
groupings than the one demonstrated here.  Every larger level number becomes a
group to all the ''higher in the hierarchy'' lower valued level numbers.  Each
new level can be treated as entire groups of fields, or separately down at the
highest numbered elementary items.  Levels from 01 to 49 can be used to create
data record layouts, with subscripted repeats (arrays) allowed at almost all
levels.  In this example only levels 01, and 05 are used.


```COBOL

       01 ws-input-record.
       :INPUT-RECORD:

```
 will become


```COBOL

       01 ws-input-record.
          05 field-a    pic x(5).
          05 field-b    pic x(5).
          05 field-c    pic s9(5).
          05 field-d    pic x(5).

```
 after the REPLACE preprocessor directive is finished.

And finally, the example selective-copy.cob

```COBOL

      *> Tectonics:
      *>   cobc -xj selective-copy.cob
      *>   cobc -xjd -DSHOWING selective-copy.cob
      *> ***************************************************************
       identification division.
       program-id. selective-copy.

       environment division.
       configuration section.
       repository.
           function all intrinsic.

       input-output section.
       file-control.
           select input-file
           assign to input-filename
           organization is sequential
           status is input-status.

           select output-file
           assign to output-filename
           organization is sequential
           status is output-status.

      *> emulate a COPY book, with an inline REPLACE

       REPLACE ==:INPUT-RECORD:== BY
       ==
          05 field-a    pic x(5).
          05 field-b    pic x(5).
          05 field-c    pic s9(4) sign is trailing separate.
          05 field-d    pic x(5).
       ==

       ==:OUTPUT-RECORD:== BY
       ==
          05 field-a    pic x(5).
          05 field-c    pic ----9.
          05 field-x    pic x(5).
       ==.

       data division.
       file section.
       fd input-file.
       01 fd-input-record.
       :INPUT-RECORD:

       fd output-file.
       01 fd-output-record.
       :OUTPUT-RECORD:

       working-storage section.
       01 input-filename.
          05 filler            value "selective-input-file".
       01 input-status         pic xx.
          88 ok-input          values '00' thru '09'.
          88 eof-input         value '10'.
       01 ws-input-record.
       :INPUT-RECORD:

       01 output-filename.
          05 filler            value "selective-output-file".
       01 output-status        pic xx.
          88 ok-output         values '00' thru '09'.
          88 eof-output        value '10'.
       01 ws-output-record.
       :OUTPUT-RECORD:

       77 file-action          pic x(11).

       77 math pic s9(5).

      *> ***************************************************************
       procedure division.
       main-routine.
       perform open-files

       perform read-input-file
       perform until eof-input
       >>IF SHOWING IS DEFINED
           display "input  :" ws-input-record ":"
       >>END-IF
           move corresponding ws-input-record to ws-output-record
           move "XXXXX" to field-x in ws-output-record
           perform write-output-record
           perform read-input-file
       end-perform
 
       perform close-files
       goback.
       
      *> ***************************************************************
       open-files.
       open input input-file
       move "open input" to file-action
       perform check-input-file

       open output output-file
       move "open output" to file-action
       perform check-output-file
       .

      *> **********************
       read-input-file.
       read input-file into ws-input-record
       move "reading" to file-action
       perform check-input-with-eof
       .

      *> **********************
       write-output-record.
       write fd-output-record from ws-output-record
       move "writing" to file-action
       perform check-output-file
       >>IF SHOWING IS DEFINED
           display "output :" ws-output-record ":"
       >>END-IF
       .

      *> **********************
       close-files.
       close input-file output-file
       perform check-input-with-eof
       perform check-output-file
       .

      *> **********************
       check-input-file.
       if not ok-input then
           perform input-file-error
       end-if
       .

      *> **********************
       check-input-with-eof.
       if not ok-input and not eof-input then
           perform input-file-error
       end-if
       .

      *> **********************
       input-file-error.
       display "error " file-action space input-filename
               space input-status upon syserr
       move 1 to return-code
       goback
       .

      *> **********************
       check-output-file.
       if not ok-output then
           display "error " file-action space output-filename
                   space output-status upon syserr
           move 1 to return-code
           goback
       end-if
       .

       end program selective-copy.
```


The output file has no newlines, so normal '''cat''' type commands are not of
much use, so we turn to the '''dd''' command again, this time with an
''unblock'' conversion.

```txt

prompt$ cobc -xj selective-copy.cob
prompt$ dd if=selective-output-file cbs=15 conv=unblock
A        1XXXXX
AA       2XXXXX
AAA      3XXXXX
AAAA    -4XXXXX
AAAAA   -5XXXXX
0+1 records in
0+1 records out
80 bytes (80 B) copied, 0.000173387 s, 461 kB/s

```


80 bytes output, 5 records of 15 bytes each, with a newline added to each of
the 5 records.  The '''dd''' option ''status=none'' will turn off the
statistics report, making the output more '''cat''' like, just showing the
records as lines of data on standard out.

Another wrinkle, is the numeric values in field-c.  Normally COBOL will store
USAGE DISPLAY numerics using one byte per digit, with an ''overpunch'' for the
sign of a value, when saving to or reading from disk or memory.  A sign bit is
or'ed into one of the digits, either first or last, depending on platform and
compile time options.  This can be overridden in source code with SIGN IS
LEADING (or TRAILING) SEPARATE. It was set to TRAILING SEPARATE in this
example. Making for a 4 digit number with a sign field, for the 5 character
field.

The output record then converts this field-c input form to a more human
friendly NUMERIC-EDITED format. This version of field-c is no longer NUMERIC
in a disk/memory sense, as the spaces in the field are not numerical values.
Spaces are not digits in COBOL raw form, and are not assumed to be zeroes.
''Financial institutions seem to like it that way''.

An alternative would be COMPUTATIONAL (or USAGE BINARY) storage format, in
which case the values would not look like text at all. Just as would happen
when C saves an '''int''' value directly to disk or memory.  During terminal
output, with a DISPLAY statement, the numbers look like text, but the memory
representation would be computational bit patterns.


## Fortran

In principle the contents of a file can be arbitrarily complex, with the structure really only comprehensible to the program that writes (and maybe reads) it especially if numbers are written in binary format whereby any bit sequence may appear - just consider the content of a "database" file. Thus, a separate program that selectively reads some data from such a file is faced with an arbitrarily difficult problem. However, if what is written is in text format only, various possible conventions might be followed, and in particular, Fortran offers the NAMELIST protocol, as exemplified below:
```Fortran
      PROGRAM TEST	!Define some data aggregates, then write and read them.
      TYPE PLACE
       INTEGER		N
       CHARACTER*28	PLACENAME
       REAL		ALTITUDE
       COMPLEX		LATLONG
      END TYPE PLACE
      TYPE HOME
       CHARACTER*28	PLACENAME
       COMPLEX		LATLONG
      END TYPE HOME
      TYPE (PLACE) HERE,THERE	!I'll have two.
      TYPE (HOME) IS		!And one of this.
      NAMELIST /STUFF/ HERE,THERE,IS	!A collection.
      INTEGER F	!An I/O unit number.
      F = 10	!This will do.

Create an example file to show its format.
      OPEN(F,FILE="Test.txt",STATUS="REPLACE",ACTION="WRITE",	!First, prepare a recipient file.
     1 DELIM="QUOTE")	!CHARACTER variables will be enquoted.
      HERE = PLACE(1,"Mt. Cook trig.",20.0,(-41.29980555,174.77629))	!Base position for surveying.
      THERE = HERE		!Copy one data aggregate to another.
      THERE.N = 2		!Differentiate.
      IS.PLACENAME = "Taipou."	!Initialise another,
      IS.LATLONG = (0,0)	!Piecemeal.
      WRITE (F,STUFF)		!Write the lot in one go.
      CLOSE (F)			!Finished with output.
      HERE = PLACE(0,"",0,(0,0))!Scrub the variables.
      THERE = HERE
      IS = HOME("",(0,0))
Can now read from the file.
      OPEN(F,FILE="Test.txt",STATUS="OLD",ACTION="READ",	!Get it back.
     1 DELIM="QUOTE")	
      READ (F,STUFF)			!Read who knows what.
      IS.PLACENAME = HERE.PLACENAME	!Copy some particular portion.
      IS.LATLONG = HERE.LATLONG		!Piecemeal, as no common structure was defined.
      WRITE (6,*) IS			!Reveal the result.
      END
```

This produces a file in a standard format. All output starts with a space in column one (in case this output were to be sent to a lineprinter, for which that would be the carriage control character), and begins the block of output with a special line that gives the name of the NAMELIST prefixed with an ampersand, and ends it with a slash. Each line starts with the name of a variable followed by an equals, then comes its value in the same style as would be used in free-form output, using a field large enough for its largest value. Some extra spaces are supplied before the = according to name length to improve readability. The whole line looks much as if it were an assignment statement in Fortran. There is special provision for listing the values of arrays which normally are shown one after another in the standard order. A run of equal values will be shown with a repetition count as in ... ,6*T, ... for six "true" values in a row, alas not using an @ symbol. CHARACTER variables will be shown with all their characters and even if they are all blank, they will not be shown as "" though that is acceptable for input. There is a default record length of 133 (again for lineprinters) and a variable may require multiple lines; CHARACTER variables split across lines include the content of the first character of the next line, but otherwise the line break is after a comma. Structure names alas use % instead of a full stop, but a full stop may be accepted as input. Before F90, such data aggregates were not available, so only ordinary variables could be named.

```txt

 &STUFF
 HERE%N       =           1,
 HERE%PLACENAME       = "Mt. Cook trig.              ",
 HERE%ALTITUDE        =   20.00000    ,
 HERE%LATLONG = (-41.29980,174.7763),
 THERE%N       =           2,
 THERE%PLACENAME       = "Mt. Cook trig.              ",
 THERE%ALTITUDE        =   20.00000    ,
 THERE%LATLONG = (-41.29980,174.7763),
 IS%PLACENAME       = "Taipou.                     ",
 IS%LATLONG = (0.0000000E+00,0.0000000E+00)
 /

```

This is suitable for input also. Variables can be named in any order, overwriting any earlier appearances. Not all need be named, also, the same variable may appear in multiple NAMELIST lists. Those named in the NAMELIST but not named in the specific input will not be changed by the READ statement. A <code>READ(F,STUFF)</code> will skip through input blocks until it finds an &STUFF, thus an input file may contain stuff for different NAMELIST collections - thereby directly meeting the specification for a "selective file copy" in its own way. For arrays (and CHARACTER variables) still further selectivity is available whereby only a portion may be altered as in <code>A(6) = 3</code>, and if values for an array are being listed (as with <code>6*T</code> above) then a sequence <code>6*</code> means that the corresponding six values of the array are to remain unchanged. Thus, given an array MANY in the NAMELIST, an assignment <code>MANY(2:13) = 1,2,3,4,6*,11,12</code> would leave MANY(6:11) unchanged, thus demonstrating another sort of selectivity. On the other hand the use of * precludes arithmetic expressions (just as with the DATA statement) - values must be actual constants.

If input is being obtained live from the keyboard, some Fortrans allow you to enter " ?" (note the leading space) to be advised of the name of the NAMELIST being sought and " =?" will list the current values of the NAMELIST collection, after which you may make your selection... 

This style of input is useful when a large number of different parameters are to be read in, and on a given occasion, many of them may be omitted. It is thus more flexible than having some defined format for all of them, or, reading them all in free-format. Good mnemonics for the names will be very helpful.

To revert to the specific task, obviously the input could contain only values for variables of interest, or, if values for all variables are supplied (as in the example), after the READ only those of interest would be copied to other variables to be going on with. In the example, because Fortran offers no <code>IS = HERE, BY NAME</code> facility so that only the elements of HERE that have the same name as elements of IS (in perhaps a different order) will be copied (as in pl/i and others), the copy must be done element-by-element. If however the desired elements had been grouped into a substructure (say, called LOCATION) and both type PLACE and type HOME had used that substructure, then a single assignment could have been used, as when HERE is copied to THERE.

Output:
```txt
 Mt. Cook trig.               (-41.29980,174.7763)
```



## Go

JSON is popular these days for structured data and Go has support for this kind of selective copy in the JSON reader.  The reader also supports custom conversions on fields.  The common idiom for record construction and field initialization is shown.

```go
package main

import (
    "encoding/json"
    "log"
    "os"
    "bytes"
    "errors"
    "strings"
)

// structure of test file, generated and then used as input.
type s1 struct {
    A string
    B string
    C int
    D string
}

// structure of output file
type s2 struct {
    A string
    C intString // it's a string, but unmarshals from a JSON int.
    X string
}

type intString string

func (i *intString) UnmarshalJSON(b []byte) error {
    if len(b) == 0 || bytes.IndexByte([]byte("0123456789-"), b[0]) < 0 {
        return errors.New("Unmarshal intString expected JSON number")
    }
    *i = intString(b)
    return nil
}

// "constructor" initializes X
func NewS2() *s2 {
    return &s2{X: "XXXXX"}
}

func main() {
    // generate test file
    o1, err := os.Create("o1.json")
    if err != nil {
        log.Fatal(err)
    }
    e := json.NewEncoder(o1)
    for i := 1; i <= 5; i++ {
        err := e.Encode(s1{
            strings.Repeat("A", i),
            strings.Repeat("B", i),
            i,
            strings.Repeat("D", i),
        })
        if err != nil {
            log.Fatal(err)
        }
    }
    o1.Close()

    // reopen the test file, also open output file
    in, err := os.Open("o1.json")
    if err != nil {
        log.Fatal(err)
    }
    out, err := os.Create("out.json")
    if err != nil {
        log.Fatal(err)
    }
    // copy input to output, streaming
    d := json.NewDecoder(in)
    e = json.NewEncoder(out)
    for d.More() {
        // a little different than the PL/I example.  PL/I reads into s1, then
        // does the selective copy in memory.  The Go JSON reader can read the
        // s1 formated JSON directly into the s2 Go struct without needing any
        // intermediate s1 struct.
        s := NewS2()
        if err = d.Decode(s); err != nil {
            log.Fatal(err)
        }
        if err = e.Encode(s); err != nil {
            log.Fatal(err)
        }
    }
}
```

```txt

{"A":"A","B":"B","C":1,"D":"D"}
{"A":"AA","B":"BB","C":2,"D":"DD"}
{"A":"AAA","B":"BBB","C":3,"D":"DDD"}
{"A":"AAAA","B":"BBBB","C":4,"D":"DDDD"}
{"A":"AAAAA","B":"BBBBB","C":5,"D":"DDDDD"}

```

```txt

{"A":"A","C":"1","X":"XXXXX"}
{"A":"AA","C":"2","X":"XXXXX"}
{"A":"AAA","C":"3","X":"XXXXX"}
{"A":"AAAA","C":"4","X":"XXXXX"}
{"A":"AAAAA","C":"5","X":"XXXXX"}

```



## Java

With a little help from my friens

```java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.File;
import java.io.IOException;
import java.util.Scanner;

class CopysJ {

  public static void main(String[] args) {
    String ddname_IN  = "copys.in.txt";
    String ddname_OUT = "copys.out.txt";
    if (args.length >= 1) { ddname_IN  = args[0].length() > 0 ? args[0] : ddname_IN; }
    if (args.length >= 2) { ddname_OUT = args[1].length() > 0 ? args[1] : ddname_OUT; }

    File dd_IN = new File(ddname_IN);
    File dd_OUT = new File(ddname_OUT);

    try (
      Scanner scanner_IN = new Scanner(dd_IN);
      BufferedWriter writer_OUT = new BufferedWriter(new FileWriter(dd_OUT))
      ) {
      String a;
      String b;
      String c;
      String d;
      String c1;
      String x = "XXXXX";
      String data_IN;
      String data_OUT;
      int ib;

      while (scanner_IN.hasNextLine()) {
        data_IN = scanner_IN.nextLine();
        ib = 0;
        a = data_IN.substring(ib, ib += 5);
        b = data_IN.substring(ib, ib += 5);
        c = data_IN.substring(ib, ib += 4);
        c1=Integer.toHexString(new Byte((c.getBytes())[0]).intValue());
        if (c1.length()<2) { c1="0" + c1; }
        data_OUT = a + c1 + x;
        writer_OUT.write(data_OUT);
        writer_OUT.newLine();
        System.out.println(data_IN);
        System.out.println(data_OUT);
        System.out.println();
      }
    }
    catch (IOException ex) {
      ex.printStackTrace();
    }
    return;
  }
}
```



## Kotlin


```scala
// version 1.1.51

import java.io.File

fun process(line: String): String {
   with (line) {
       val a = substring(0, 5) 
       val n = (substring(14, 15) + substring(10, 14)).toInt() 
       return String.format("%s%5dXXXXX", a, n)
   }
}

fun main(args: Array<String>) {
    val out = File("selective_output.txt")
    val pw = out.printWriter()
    File("selective_input.txt").forEachLine { pw.println(process(it)) }
    pw.close()
    // check it worked
    println(out.readText())
}
```


Contents of selective_input.txt:

```txt

A    bbbbB0001+d2345
AA   bbbBB0002+1d345
AAA  bbBBB0003+12d45
AAAA bBBBB0001-123d5
AAAAABBBBB0002-1234d

```


```txt

A        1XXXXX
AA       2XXXXX
AAA      3XXXXX
AAAA    -1XXXXX
AAAAA   -2XXXXX

```



## NetRexx

with a little help from a friend

```netrexx
/* NetRexx */
-- nrc -keepasjava -savelog copys
options replace format comments java crossref symbols nobinary

parse arg ddname_IN ddname_OUT .
do
  if ddname_IN.length = 0 then ddname_IN = 'copys.in.txt'
  if ddname_OUT.length = 0 then ddname_OUT = 'copys.out.txt'

  dd_IN = File(ddname_IN)
  dd_OUT = File(ddname_OUT)
  scanner_IN = Scanner(dd_IN)
  writer_OUT = BufferedWriter(FileWriter(dd_OUT))

  x = 'XXXXX'
  loop while scanner_IN.hasNextLine()
    data_IN = scanner_IN.nextLine()
    parse data_IN a +5 . /* b */ +5 c +4 . /* d */
    cc=c.left(1).c2x
    data_OUT = a || cc.right(2,0) || x
    writer_OUT.write(data_OUT)
    writer_OUT.newLine()
    end
catch ex = IOException
  ex.printStackTrace()
finally
  do
    if scanner_IN \= null then scanner_IN.close()
    if writer_OUT \= null then writer_OUT.close()
  catch ex = IOException
    ex.printStackTrace()
  end
end
```



## ooRexx


```oorexx
/* REXX */
infile ="in.txt"
outfile="out.txt"

s1=.copys~new(infile,outfile)
loop i=1 to 5
  s1~~input~~output
end
s1~close       -- close streams (files)
'type' outfile

::class copys
::attribute a
::attribute b
::attribute c
::attribute d

::method init     -- constructor
  expose instream outstream
  parse arg infile, outfile
  instream =.stream~new(infile)~~open
  outstream=.stream~new(outfile)~~open("replace")

::method input    -- read an input line
  expose instream a b c d
  parse value instream~linein with a +5 b +5 c +5 d +5

::method output   -- write an output line
  expose outstream a c
  outstream~lineout(a || c~c2x~left(2)'XXXXX')

::method close    -- close files
  expose instream outstream
  instream~close
  outstream~close
```

```txt
AA   01XXXXX
AAA  02XXXXX
AAAA 03XXXXX
AAAAA04XXXXX
AAAAA05XXXXX
```
   


## Perl

```perl
my %F = ( # arbitrary and made up record format
    'field a' => { offset => 0,  length => 5, type => 'Str' },
    'field b' => { offset => 5,  length => 5, type => 'Str' },
    'field c' => { offset => 10, length => 4, type => 'Bit' },
    'field d' => { offset => 14, length => 1, type => 'Str' },
    'field e' => { offset => 15, length => 5, type => 'Str' }
);

$record_length += $F{$_}{'length'} for keys %F;

open $fh, '<', 'sfc.dat' || die;
while ($n=sysread($fh, $record, $record_length)) {
    last if $n < $record_length;
    for $k (sort keys %F) {
        if ($F{$k}{type} eq 'Str') {
            printf "$k : %s ", $v = substr $record, $F{$k}{offset}, $F{$k}{length};
            $h{$k} = $v;
        } elsif ($F{$k}{type} eq 'Bit') {
            printf "$k : %d ", $v = substr $record, $F{$k}{offset}, $F{$k}{length};
            $h{$k} = pack("B8",'0011'.$v);;
        }
    }
    print "\n";
    push @result, sprintf( "%-5s%s%01d%5s", $h{'field a'}, $h{'field d'}, $h{'field c'}, 'xxxxx' );
}
print "\n" . join "\n", @result;

```

```txt
field a : A     field b : bbbbB field c : 1 field d : + field e : d2345
field a : A     field b : bbbbB field c : 0001 field d : + field e : d2345
field a : AA    field b : bbbBB field c : 0010 field d : + field e : 1d345
field a : AAA   field b : bbBBB field c : 0011 field d : + field e : 12d45
field a : AAAA  field b : bBBBB field c : 0100 field d : - field e : 123d5
field a : AAAAA field b : BBBBB field c : 0101 field d : - field e : 1234d

A    +1xxxxx
AA   +2xxxxx
AAA  +3xxxxx
AAAA -4xxxxx
AAAAA-5xxxxx
```



## Perl 6

I have no idea how PL/I or COBOL store records and little enthusiasm to research it. If the task author can't be bothered to spell out what the format should look like, then I have no compunction about just making something up out of whole cloth. In the absence of better guidance I am going to make a binary encoded data file format with fixed sized fields consisting of a mix of ISO-8859-1 encoded text and raw binary (hex) encoded integers.

This is WAY more complicated than it could be. Could achieve the same effect in one or two lines of code, but this explicitly shows some of the possible mechanics.

Since the sfc.dat file is binary encoded, I can't include it here easily as text so [https://github.com/thundergnat/rc/blob/master/sfc.dat here is a link to an online copy] instead. 


```perl6
my @format = ( # arbitrary and made up record format
    'field a' => { offset => 0,  length => 5, type => 'Str' },
    'field b' => { offset => 5,  length => 5, type => 'Str' },
    'field c' => { offset => 10, length => 4, type => 'Int' },
    'field d' => { offset => 14, length => 1, type => 'Str' },
    'field e' => { offset => 15, length => 5, type => 'Str' }
);

my $record-length = @format[*]».value».<length>.sum;

my $in = './sfc.dat'.IO.open :r :bin;

say "Input data as read from $in:";
my @records;
@records.push: get-record($in, $record-length) until $in.eof;
.perl.say for @records;

# not going to bother to actually write out to a file, if you really want to,
# supply a file handle to a local file
say "\nOutput:";
my $outfile = $*OUT; # or some other filename, whatever.

for @records -> $r {
    $outfile.printf( "%-5s%s%08x%5s\n", flat $r.{'field a','field d','field c'}, 'xxxxx' );
}

sub get-record($fh, $bytes) {
    my $record = $fh.read($bytes);
    return ().Slip unless $record.elems == $bytes;
    my %r = @format.map: {
        .key => do given $_.value.<type> -> $type
        {
            when $type eq 'Str' { $record.subbuf($_.value.<offset>, $_.value.<length>).decode }
            when $type eq 'Int' { sum $record.subbuf($_.value.<offset>, $_.value.<length>) Z+< (24,16,8,0) }
            default             { $record.subbuf($_.value.<offset>, $_.value.<length>) } # Buf
        }
    }
}
```

```txt
Input data as read from ./sfc.dat:
${"field a" => "A    ", "field b" => "bbbbB", "field c" => 1, "field d" => "+", "field e" => "d2345"}
${"field a" => "AA   ", "field b" => "bbbBB", "field c" => 2, "field d" => "+", "field e" => "1d345"}
${"field a" => "AAA  ", "field b" => "bbBBB", "field c" => 3, "field d" => "+", "field e" => "12d45"}
${"field a" => "AAAA ", "field b" => "bBBBB", "field c" => 4, "field d" => "-", "field e" => "123d5"}
${"field a" => "AAAAA", "field b" => "BBBBB", "field c" => 3729368837, "field d" => "-", "field e" => "1234d"}

Output:
A    +00000001xxxxx
AA   +00000002xxxxx
AAA  +00000003xxxxx
AAAA -00000004xxxxx
AAAAA-de49a705xxxxx
```



## Phix

```Phix
constant data = """
A    bbbbB0001+d2345
AA   bbbBB0002+1d345
AAA  bbBBB0003+12d45
AAAA bBBBB0001-123d5
AAAAABBBBB0002-1234d
"""
integer fn = open("in.txt","w")
puts(fn,data)
close(fn)

integer fin = open("in.txt","r"),
        fout = open("out.txt","w")
object line
while 1 do
    line = gets(fn)
    if atom(line) then exit end if
    sequence res = scanf(line[15]&line[11..14],"%d")
    integer d = iff(length(res)=1?res[1][1]:0)
    string s = sprintf("%s%5dXXXXX\n",{line[1..5],d})
    printf(1,s)
    printf(fout,s)
end while
close(fin)
close(fout)
```

```txt

A        1XXXXX
AA       2XXXXX
AAA      3XXXXX
AAAA    -1XXXXX
AAAAA   -2XXXXX

```



## PicoLisp


```PicoLisp
(de record (F)
   (in F
      (until (eof)
         (tab (5 5 5)
            (make (do 5 (link (char))))
            (and
               (do 5
                  (char) )
               (till "+-")
               (format (conc (cons (peek)) @)) )
            "XXXXX" )
         (line) ) ) )

(record "selective.in")
```

```txt
A        1XXXXX
AA       2XXXXX
AAA      3XXXXX
AAAA    -1XXXXX
AAAAA   -2XXXXX
```



## PL/I


```pli
*process source attributes xref or(!);
 copys: Proc Options(Main);
 Dcl 1 s1 unal,
      2 a Char(5),
      2 b Char(5),
      2 c Bin Fixed(31),
      2 d Char(5);
 Dcl 1 s2,
      2 a Char(5),
      2 c Pic'99',
      2 x Char(5) Init('XXXXX');
 Dcl o1  Record Output;   /* create a test file */
 Dcl in  Record Input;
 Dcl out Record Output;
 Do i=1 To 5;
   s1.a=repeat('A',i);
   s1.b=repeat('B',i);
   s1.c=i;
   s1.d=repeat('D',i);
   Write File(o1) From(s1);
   End;
 Close File(o1);

 On Endfile(in) Goto eoj;
 Do i=1 By 1;             /* copy parts of the test file    */
   Read File(in) Into(s1);
   s2=s1, by name;        /* only fields a and c are copied */
   Write File(out) From(s2);
   End;
 eoj:
 End;

```

```txt
AA   01XXXXX
AAA  02XXXXX
AAAA 03XXXXX
AAAAA04XXXXX
AAAAA05XXXXX
```



## Racket

```racket
#lang racket

(define (read+write)
  (for ([line (in-lines)])
    (define a (substring line 0 5))
    (define n (string->number (string-append (substring line 14 15)
                                             (substring line 10 14))))
    (printf "~a~aXXXXX\n" a (~a n #:min-width 5 #:align 'right))))

(with-output-to-file "selective-output.txt" #:mode 'text #:exists 'replace
  (thunk (with-input-from-file "selective-input.txt" read+write)))
```


```txt

A    bbbbB0001+d2345
AA   bbbBB0002+1d345
AAA  bbBBB0003+12d45
AAAA bBBBB0001-123d5
AAAAABBBBB0002-1234d

```


```txt

A        1XXXXX
AA       2XXXXX
AAA      3XXXXX
AAAA    -1XXXXX
AAAAA   -2XXXXX

```




## REXX

```rexx
in='in.txt'
out='out.txt'; 'erase' out
Do While lines(in)>0
  l=linein(in)
  Parse Var l a +5 b +5 c +4 d +5
  chex=c2x(c)
  cpic=left(chex,2)
  call lineout out,a||cpic||'XXXXX'
  End
Call lineout in
Call lineout out
'type' out
```

Using the test file produced by PL/I.
The data conversion used for c is not very general! 

```txt
AA   01XXXXX
AAA  02XXXXX
AAAA 03XXXXX
AAAAA04XXXXX
AAAAA05XXXXX
```



## zkl

Taking a clue from ALGOL 68 and Awk.

```zkl
File("in.txt").pump(File("out.txt","w"),fcn(line){
   //        012345678901234567890...
   //        A    bbbbB0001+d2345
   // fields:1    2    3   45, field 4 is +|-
   fld1,fld3,fld4,fld5 := line[0,5], line[10,4],line[14], line[15,*].strip();
   if(fld4=="-") fld3=-fld3.toInt();
   "%-5s%5d%s\n".fmt(fld1,fld3,"X"*fld5.len())
})
```

```txt

$ zkl bbb
$ cat out.txt 
A        1XXXXX
AA       2XXXXX
AAA      3XXXXX
AAAA    -1XXXXX
AAAAA   -2XXXXX

```

