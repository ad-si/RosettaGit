+++
title = "Strip block comments"
description = ""
date = 2019-01-03T19:49:45Z
aliases = []
[extra]
id = 8644
[taxonomies]
categories = []
tags = []
+++

{{task|Text processing}}
[[Category:String manipulation]]

A block comment begins with a   ''beginning delimiter''   and ends with a   ''ending delimiter'',   including the delimiters.   These delimiters are often multi-character sequences.


;Task:
Strip block comments from program text (of a programming language much like classic [[C]]). 

Your demos should at least handle simple, non-nested and multi-line block comment delimiters.  

The block comment delimiters are the two-character sequence:
:::*     <big><big> '''/*''' </big></big>     (beginning delimiter)
:::*     <big><big> '''*/''' </big></big>     (ending delimiter)


Sample text for stripping:

```txt

  /**
   * Some comments
   * longer comments here that we can parse.
   *
   * Rahoo 
   */
   function subroutine() {
    a = /* inline comment */ b + c ;
   }
   /*/ <-- tricky comments */

   /**
    * Another comment.
    */
    function something() {
    }

```


;Extra credit:
Ensure that the stripping code is not hard-coded to the particular delimiters described above, but instead allows the caller to specify them.   (If your language supports them,   [[Optional parameters|optional parameters]]   may be useful for this.)


;Related task:
*   [[Strip comments from a string]]





## Ada

strip.adb:

```Ada
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Command_Line;

procedure Strip is
   use Ada.Strings.Unbounded;
   procedure Print_Usage is
   begin
      Ada.Text_IO.Put_Line ("Usage:");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("   strip <file> [<opening> [<closing>]]");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("      file: file to strip");
      Ada.Text_IO.Put_Line ("      opening: string for opening comment");
      Ada.Text_IO.Put_Line ("      closing: string for closing comment");
      Ada.Text_IO.New_Line;
   end Print_Usage;

   Opening_Pattern : Unbounded_String := To_Unbounded_String ("/*");
   Closing_Pattern : Unbounded_String := To_Unbounded_String ("*/");
   Inside_Comment  : Boolean          := False;

   function Strip_Comments (From : String) return String is
      use Ada.Strings.Fixed;
      Opening_Index : Natural;
      Closing_Index : Natural;
      Start_Index   : Natural := From'First;
   begin
      if Inside_Comment then
         Start_Index :=
            Index (Source => From, Pattern => To_String (Closing_Pattern));
         if Start_Index < From'First then
            return "";
         end if;
         Inside_Comment := False;
         Start_Index    := Start_Index + Length (Closing_Pattern);
      end if;
      Opening_Index :=
         Index
           (Source  => From,
            Pattern => To_String (Opening_Pattern),
            From    => Start_Index);
      if Opening_Index < From'First then
         return From (Start_Index .. From'Last);
      else
         Closing_Index :=
            Index
              (Source  => From,
               Pattern => To_String (Closing_Pattern),
               From    => Opening_Index + Length (Opening_Pattern));
         if Closing_Index > 0 then
            return From (Start_Index .. Opening_Index - 1) &
                   Strip_Comments
                      (From (
               Closing_Index + Length (Closing_Pattern) .. From'Last));
         else
            Inside_Comment := True;
            return From (Start_Index .. Opening_Index - 1);
         end if;
      end if;
   end Strip_Comments;

   File : Ada.Text_IO.File_Type;
begin
   if Ada.Command_Line.Argument_Count < 1
     or else Ada.Command_Line.Argument_Count > 3
   then
      Print_Usage;
      return;
   end if;
   if Ada.Command_Line.Argument_Count > 1 then
      Opening_Pattern := To_Unbounded_String (Ada.Command_Line.Argument (2));
      if Ada.Command_Line.Argument_Count > 2 then
         Closing_Pattern :=
            To_Unbounded_String (Ada.Command_Line.Argument (3));
      else
         Closing_Pattern := Opening_Pattern;
      end if;
   end if;
   Ada.Text_IO.Open
     (File => File,
      Mode => Ada.Text_IO.In_File,
      Name => Ada.Command_Line.Argument (1));
   while not Ada.Text_IO.End_Of_File (File => File) loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line (File);
      begin
         Ada.Text_IO.Put_Line (Strip_Comments (Line));
      end;
   end loop;
   Ada.Text_IO.Close (File => File);
end Strip;
```

output:

```txt
  





   function subroutine() {
    a =  b + c ;
   }
   

   


    function something() {
    }
```



## ALGOL W

Handles non-nested block comments, the start and end delimiters are specified as parameters. Comments inside string-literals are retained. The string quote and escape characters are specified as parameters.

```algolw
begin
    % strips block comments from a source                                     %
    % the source is read from standard input and the result written to        %
    % standard output, The comment start text is in cStart and the ending     %
    % is in cEnd.                                                             %
    % As strings are fixed length in Algol W, the first space in cStart/cEnd  %
    % is assumed to terminate the delimiter, i.e. the comment start/end       %
    % delimiters cannot contain spaces.                                       %
    % If non-blank, quote1 and quote2 are the string quote characters.        %
    % If escape is non-blank it indicates that quotes can be embedded in      %
    % string literals by preceding them with escape (as in C, java, etc.).    %
    procedure stripBlockComments( string(32) value cStart, cEnd
                                ; string(1)  value quote1, quote2, escape
                                ) ;
    begin
        integer     columnNumber, lineWidth;
        string(256) line;
        string(1)   currChar;
        string(1)   newlineChar;
        % gets the next source character                                      %
        procedure nextChar ;
        begin
            if      columnNumber = lineWidth then begin
                currChar     := newlineChar;
                columnNumber := columnNumber + 1
                end
            else if columnNumber > lineWidth then begin
                readcard( line );
                lineWidth := 256;
                while lineWidth > 0 and line( lineWidth - 1 // 1 ) = " " do lineWidth := lineWidth - 1;
                columnNumber := 1;
                currChar     := line( 0 // 1 )
                end
            else begin
                currChar     := line( columnNumber // 1 );
                columnNumber := columnNumber + 1
            end
        end nextChar ;
        % copy the current character and get the next                         %
        procedure copyAndNext ;
        begin
            if currChar = newlineChar then write()
                                      else writeon( currChar );
            nextChar
        end copyAndNext ;
        % skips the current character and gets the next                       %
        procedure skipAndNext ;
        begin
            if currChar not = newlineChar then currChar := " ";
            copyAndNext
        end skipAndNext ;
        % handle a string literal                                             %
        procedure stringLiteral( string(1) value quote, escape ) ;
        begin
            copyAndNext;
            while currChar not = quote and not XCPNOTED(ENDFILE) do begin
                if escape <> " " and currChar = escape then copyAndNext;
                if not XCPNOTED(ENDFILE) then copyAndNext
            end while_have_more_string ;
            if currChar = quote then copyAndNext
        end stringLiteral ;
        % returns true if the line continues with the specified text          %
        %         false if not.                                               %
        logical procedure remainingLineStartsWith ( string(32) value text ) ;
        begin
            logical   haveText;
            integer   lPos, wPos;
            haveText := currChar = text( 0 // 1 );
            lPos     := columnNumber;
            wPos     := 1;
            while haveText and wPos <= 32 and text( wPos // 1 ) not = " " do begin
                if lPos >= lineWidth then begin
                    % past the end of the line                                %
                    haveText := false
                    end
                else begin
                    % still have text on the line                             %
                    haveText := line( lPos // 1 ) = text( wPos // 1 );
                    wPos     := wPos + 1;
                    lPos     := lPos + 1;
                end if_past_end_of_line_
            end while_have_text_and_more_text ;
            haveText
        end remainingLineStartsWith ;
        % skips the number of leading non-blank characters in the delimiter   %
        procedure skipDelimiter( string(32) value delimiter ) ;
        begin
            integer dPos;
            dPos := 0;
            while dPos < 32 and not XCPNOTED(ENDFILE) and delimiter( dPos // 1 ) not = " " do begin
               dPos := dPos + 1;
               skipAndNext
           end while_not_at_end_of_delimiter
        end skipDelimiter ;
        newlineChar  := code( 10 );
        lineWidth    := 0;
        columnNumber := lineWidth + 1;
        currChar     := " ";
        % allow the program to continue after reaching end-of-file %
        ENDFILE      := EXCEPTION( false, 1, 0, false, "EOF" );
        % get the first source character                                      %
        nextChar;
        % strip the comments                                                  %
        while not XCPNOTED(ENDFILE) do begin
            if      currChar = " "    then copyAndNext
            else if remainingLineStartsWith( cStart ) then begin
                % have a comment                                              %
                skipDelimiter( cStart );
                while not remainingLineStartsWith( cEnd ) and not XCPNOTED(ENDFILE) do skipAndNext;
                skipDelimiter( cEnd )
                end
            else if currChar = quote1 then stringLiteral( quote1, escape )
            else if currChar = quote2 then stringLiteral( quote2, escape )
            else                           copyAndNext
        end while_not_at_eof
    end stripBlockComments ;
    % text stripBlockComments for C-style source                              %
    stripBlockComments( "/*", "*/", """", "'", "\" )
end.
```

{{out}}

```txt

     
                  
                                            
    
          
     
   function subroutine() {
    a =                      b + c ;
   }
                             
       
                      
      
    function something() {
    }
 

```



## AutoHotkey


```AutoHotkey
code =
(
 /**
   * Some comments
   * longer comments here that we can parse.
   *
   * Rahoo 
   */
   function subroutine() {
    a = /* inline comment */ b + c ;
   }
   /*/ <-- tricky comments */

   /**
    * Another comment.
    */
    function something() {
    }
)
;Open-Close Comment delimiters
	openC:="/*"
	closeC:="*/"
;Make it "Regex-Safe"
	openC:=RegExReplace(openC,"(\*|\^|\?|\\|\+|\.|\!|\{|\}|\[|\]|\$|\|)","\$0")
	closeC:=RegExReplace(closeC,"(\*|\^|\?|\\|\+|\.|\!|\{|\}|\[|\]|\$|\|)","\$0")
;Display final result
	MsgBox % sCode := RegExReplace(code,"s)(" . openC . ").*?(" . closeC . ")")
```


```txt


   function subroutine() {
    a =  b + c ;
   }
   

   
    function something() {
    }
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      infile$ = "C:\sample.c"
      outfile$ = "C:\stripped.c"
      
      PROCstripblockcomments(infile$, outfile$, "/*", "*/")
      END
      
      DEF PROCstripblockcomments(infile$, outfile$, start$, finish$)
      LOCAL infile%, outfile%, comment%, test%, A$
      
      infile% = OPENIN(infile$)
      IF infile%=0 ERROR 100, "Could not open input file"
      outfile% = OPENOUT(outfile$)
      IF outfile%=0 ERROR 100, "Could not open output file"
      
      WHILE NOT EOF#infile%
        A$ = GET$#infile% TO 10
        REPEAT
          IF comment% THEN
            test% = INSTR(A$, finish$)
            IF test% THEN
              A$ = MID$(A$, test% + LEN(finish$))
              comment% = FALSE
            ENDIF
          ELSE
            test% = INSTR(A$, start$)
            IF test% THEN
              BPUT#outfile%, LEFT$(A$, test%-1);
              A$ = MID$(A$, test% + LEN(start$))
              comment% = TRUE
            ENDIF
          ENDIF
        UNTIL test%=0
        IF NOT comment% BPUT#outfile%, A$
      ENDWHILE
      
      CLOSE #infile%
      CLOSE #outfile%
      ENDPROC
```

Output file:

```txt

  
   function subroutine() {
    a =  b + c ;
   }
   

   
    function something() {
    }

```



## C


```C>#include <stdio.h

#include <string.h>
#include <stdlib.h>

const char *ca = "/*", *cb = "*/";
int al = 2, bl = 2;

char *loadfile(const char *fn) {
    FILE *f = fopen(fn, "rb");
    int l;
    char *s;

    if (f != NULL) {
	fseek(f, 0, SEEK_END);
	l = ftell(f);
	s = malloc(l+1);
	rewind(f);
	if (s)
	    fread(s, 1, l, f);
	fclose(f);
    }
    return s;
}

void stripcomments(char *s) {
    char *a, *b;
    int len = strlen(s) + 1;

    while ((a = strstr(s, ca)) != NULL) {
	b = strstr(a+al, cb);
	if (b == NULL)
	    break;
	b += bl;
	memmove(a, b, len-(b-a));
    }
}

int main(int argc, char **argv) {
    const char *fn = "input.txt";
    char *s;

    if (argc >= 2)
	fn = argv[1];
    s = loadfile(fn);
    if (argc == 4) {
	al = strlen(ca = argv[2]);
	bl = strlen(cb = argv[3]);
    }
    stripcomments(s);
    puts(s);
    free(s);
    return 0;
}
```

;Usage:
Specify an input file via the first command line argument, and optionally specify comment opening and closing delimiters with the next two args, or defaults of /* and */ are assumed.
;Output:

```txt


   function subroutine() {
    a =  b + c ;
   }



    function something() {
    }


```



## C++


```cpp>#include <string

#include <iostream> 
#include <iterator>
#include <fstream>
#include <boost/regex.hpp>

int main( ) {
    std::ifstream codeFile( "samplecode.txt" ) ;
    if ( codeFile ) {
       boost::regex commentre( "/\\*.*?\\*/" ) ;//comment start and end, and as few characters in between as possible
       std::string my_erase( "" ) ;             //erase them
       std::string stripped ;
       std::string code( (std::istreambuf_iterator<char>( codeFile ) ) ,
	     std::istreambuf_iterator<char>( ) ) ;
       codeFile.close( ) ;
       stripped = boost::regex_replace( code , commentre , my_erase ) ;
       std::cout << "Code unstripped:\n" << stripped << std::endl ;
       return 0 ;
    }
    else {
       std::cout << "Could not find code file!" << std::endl ;
       return 1 ;
    }
}
```

Output:

```txt

Code unstripped:

   function subroutine() {
    a =  b + c ;
   }
   

   
    function something() {
    }

```


=={{header|C sharp|C#}}==

```Csharp
using System;

    class Program
    {
        private static string BlockCommentStrip(string commentStart, string commentEnd, string sampleText)
        {
            while (sampleText.IndexOf(commentStart) > -1 && sampleText.IndexOf(commentEnd, sampleText.IndexOf(commentStart) + commentStart.Length) > -1)
            {
                int start = sampleText.IndexOf(commentStart);
                int end = sampleText.IndexOf(commentEnd, start + commentStart.Length);
                sampleText = sampleText.Remove(
                    start,
                    (end + commentEnd.Length) - start
                    );
            }
            return sampleText;
        }
    }
```



## Clojure


```Clojure
(defn comment-strip [txt & args]
  (let [args (conj {:delim ["/*" "*/"]} (apply hash-map args)) ; This is the standard way of doing keyword/optional arguments in Clojure
	[opener closer] (:delim args)]
    (loop [out "", txt txt, delim-count 0] ; delim-count is needed to handle nested comments
      (let [[hdtxt resttxt] (split-at (count opener) txt)] ; This splits "/* blah blah */" into hdtxt="/*" and restxt="blah blah */"	
	(printf "hdtxt=%8s resttxt=%8s out=%8s txt=%16s delim-count=%s\n" (apply str hdtxt) (apply str resttxt) out (apply str txt) delim-count)
	(cond
	 (empty? hdtxt)    (str out (apply str txt))
	 (= (apply str hdtxt) opener) (recur out resttxt (inc delim-count))
	 (= (apply str hdtxt) closer) (recur out resttxt (dec delim-count))
	 (= delim-count 0)(recur (str out (first txt)) (rest txt) delim-count)
	 true             (recur out (rest txt) delim-count))))))
```


```txt
user> (comment-strip "This /* is */ some /* /* /* */ funny */ */ text")
hdtxt=      Th resttxt=is /* is */ some /* /* /* */ funny */ */ text out=         txt=This /* is */ some /* /* /* */ funny */ */ text delim-count=0
hdtxt=      hi resttxt=s /* is */ some /* /* /* */ funny */ */ text out=       T txt=his /* is */ some /* /* /* */ funny */ */ text delim-count=0
hdtxt=      is resttxt= /* is */ some /* /* /* */ funny */ */ text out=      Th txt=is /* is */ some /* /* /* */ funny */ */ text delim-count=0
hdtxt=      s  resttxt=/* is */ some /* /* /* */ funny */ */ text out=     Thi txt=s /* is */ some /* /* /* */ funny */ */ text delim-count=0
hdtxt=       / resttxt=* is */ some /* /* /* */ funny */ */ text out=    This txt= /* is */ some /* /* /* */ funny */ */ text delim-count=0
hdtxt=      /* resttxt= is */ some /* /* /* */ funny */ */ text out=   This  txt=/* is */ some /* /* /* */ funny */ */ text delim-count=0
hdtxt=       i resttxt=s */ some /* /* /* */ funny */ */ text out=   This  txt= is */ some /* /* /* */ funny */ */ text delim-count=1
hdtxt=      is resttxt= */ some /* /* /* */ funny */ */ text out=   This  txt=is */ some /* /* /* */ funny */ */ text delim-count=1
hdtxt=      s  resttxt=*/ some /* /* /* */ funny */ */ text out=   This  txt=s */ some /* /* /* */ funny */ */ text delim-count=1
hdtxt=       * resttxt=/ some /* /* /* */ funny */ */ text out=   This  txt= */ some /* /* /* */ funny */ */ text delim-count=1
hdtxt=      */ resttxt= some /* /* /* */ funny */ */ text out=   This  txt=*/ some /* /* /* */ funny */ */ text delim-count=1
hdtxt=       s resttxt=ome /* /* /* */ funny */ */ text out=   This  txt= some /* /* /* */ funny */ */ text delim-count=0
hdtxt=      so resttxt=me /* /* /* */ funny */ */ text out=  This   txt=some /* /* /* */ funny */ */ text delim-count=0
hdtxt=      om resttxt=e /* /* /* */ funny */ */ text out= This  s txt=ome /* /* /* */ funny */ */ text delim-count=0
hdtxt=      me resttxt= /* /* /* */ funny */ */ text out=This  so txt=me /* /* /* */ funny */ */ text delim-count=0
hdtxt=      e  resttxt=/* /* /* */ funny */ */ text out=This  som txt=e /* /* /* */ funny */ */ text delim-count=0
hdtxt=       / resttxt=* /* /* */ funny */ */ text out=This  some txt= /* /* /* */ funny */ */ text delim-count=0
hdtxt=      /* resttxt= /* /* */ funny */ */ text out=This  some  txt=/* /* /* */ funny */ */ text delim-count=0
hdtxt=       / resttxt=* /* */ funny */ */ text out=This  some  txt= /* /* */ funny */ */ text delim-count=1
hdtxt=      /* resttxt= /* */ funny */ */ text out=This  some  txt=/* /* */ funny */ */ text delim-count=1
hdtxt=       / resttxt=* */ funny */ */ text out=This  some  txt= /* */ funny */ */ text delim-count=2
hdtxt=      /* resttxt= */ funny */ */ text out=This  some  txt=/* */ funny */ */ text delim-count=2
hdtxt=       * resttxt=/ funny */ */ text out=This  some  txt= */ funny */ */ text delim-count=3
hdtxt=      */ resttxt= funny */ */ text out=This  some  txt=*/ funny */ */ text delim-count=3
hdtxt=       f resttxt=unny */ */ text out=This  some  txt= funny */ */ text delim-count=2
hdtxt=      fu resttxt=nny */ */ text out=This  some  txt=funny */ */ text delim-count=2
hdtxt=      un resttxt=ny */ */ text out=This  some  txt= unny */ */ text delim-count=2
hdtxt=      nn resttxt=y */ */ text out=This  some  txt=  nny */ */ text delim-count=2
hdtxt=      ny resttxt= */ */ text out=This  some  txt=   ny */ */ text delim-count=2
hdtxt=      y  resttxt=*/ */ text out=This  some  txt=    y */ */ text delim-count=2
hdtxt=       * resttxt=/ */ text out=This  some  txt=      */ */ text delim-count=2
hdtxt=      */ resttxt= */ text out=This  some  txt=      */ */ text delim-count=2
hdtxt=       * resttxt=  / text out=This  some  txt=         */ text delim-count=1
hdtxt=      */ resttxt=    text out=This  some  txt=         */ text delim-count=1
hdtxt=       t resttxt=     ext out=This  some  txt=            text delim-count=0
hdtxt=      te resttxt=      xt out=This  some   txt=            text delim-count=0
hdtxt=      ex resttxt=       t out=This  some  t txt=             ext delim-count=0
hdtxt=      xt resttxt=         out=This  some  te txt=              xt delim-count=0
hdtxt=       t resttxt=         out=This  some  tex txt=               t delim-count=0
hdtxt=         resttxt=         out=This  some  text txt=                 delim-count=0
"This  some  text"
```



## D


```d
import std.algorithm, std.regex;

string[2] separateComments(in string txt,
                           in string cpat0, in string cpat1) {
    int[2] plen; // to handle /*/
    int i, j; // cursors
    bool inside; // is inside comment?

    // pre-compute regex here if desired
    //auto r0 = regex(cpat0);
    //auto r1 = regex(cpat1);
    //enum rct = ctRegex!(r"\n|\r");

    bool advCursor() {
        auto mo = match(txt[i .. $], inside ? cpat1 : cpat0);
        if (mo.empty)
            return false;
        plen[inside] = max(0, plen[inside], mo.front[0].length);
        j = i + mo.pre.length; // got comment head
        if (inside)
            j += mo.front[0].length; // or comment tail

        // special adjust for \n\r
        if (!match(mo.front[0], r"\n|\r").empty)
            j--;
        return true;
    }

    string[2] result;
    while (true) {
        if (!advCursor())
            break;
        result[inside] ~= txt[i .. j]; // save slice of result

        // handle /*/ pattern
        if (inside && (j - i < plen[0] + plen[1])) {
            i = j;
            if (!advCursor())
                break;
            result[inside] ~= txt[i .. j]; // save result again
        }

        i = j; // advance cursor
        inside = !inside; // toggle search type
    }

    if (inside)
        throw new Exception("Mismatched Comment");
    result[inside] ~= txt[i .. $]; // save rest(non-comment)
    return result;
}


void main() {
    import std.stdio;

    static void showResults(in string e, in string[2] pair) {
        writeln("===Original text:\n", e);
        writeln("\n\n===Text without comments:\n", pair[0]);
        writeln("\n\n===The stripped comments:\n", pair[1]);
    }

    // First example ------------------------------
    immutable ex1 = `  /**
   * Some comments
   * longer comments here that we can parse.
   *
   * Rahoo
   */
   function subroutine() {
    a = /* inline comment */ b + c ;
   }
   /*/ <-- tricky comments */

   /**
    * Another comment.
    */
    function something() {
    }`;

    showResults(ex1, separateComments(ex1, `/\*`, `\*/`));

    // Second example ------------------------------
    writeln("\n");
    immutable ex2 = "apples, pears # and bananas
apples, pears; and bananas ";  // test for line comment

    showResults(ex2, separateComments(ex2, `#|;`, `[\n\r]|$`));
}
```

{{out}}

```txt
===Original text:
  /**
   * Some comments
   * longer comments here that we can parse.
   *
   * Rahoo
   */
   function subroutine() {
    a = /* inline comment */ b + c ;
   }
   /*/ <-- tricky comments */

   /**
    * Another comment.
    */
    function something() {
    }


===Text without comments:
  
   function subroutine() {
    a =  b + c ;
   }
   

   
    function something() {
    }


===The stripped comments:
/**
   * Some comments
   * longer comments here that we can parse.
   *
   * Rahoo
   *//* inline comment *//*/ <-- tricky comments *//**
    * Another comment.
    */


===Original text:
apples, pears # and bananas
apples, pears; and bananas 


===Text without comments:
apples, pears 
apples, pears


===The stripped comments:
# and bananas; and bananas 
```




## Fortran

As ever, there arises the question "How long is a piece of string?" as having once abandoned decks of cards, there is no longer a definite upper bound for the length of a record. So, as ever, the classic response of "surely big enough", here 6666 characters. F90 enables the creation of a protocol for varying-length strings, and F2000 formalises this, but, there is no provision for reading a record of input into a variable that is made large enough for the record just being read, as is almost the case for pl/1 - where the receiving variable would be declared <code>ACARD CHARACTER(6666) VARYING</code> and the READ statement sets the length according to what has been read - but only up to the pre-specified upper limit.

So, a reversion to F77 style (which introduced CHARACTER variables) and so, not employing the MODULE protocol of F90 to share information - COMMON statements instead. Though not purely F77 as there is the PARAMETER statement, and the usage of I0 format. The Q format code was a common extension to F77, and reports on the number of characters as yet unread in the input record. This allows the receiving variable to be sized to fit, as in ACARD(1:LC) except that to prevent overflow, LC appears via MIN(LC,LOTS). If this was not done, then the receiving variable would be padded to the end with spaces at a cost in cpu time to supply, and then to scan backwards to find the last non-blank. This would also trim off any trailing spaces that were in the input record.

If the text delimiters were single characters only, similar to text literals, it would be easy: scan the text character by character and change state accordingly, though there would be complications if say two quote characters in a row were to signify a single internal quote. A DO-loop would do for the scan. But with multi-character delimiters the scan would have to lurch over a match, and fiddling the index variable of a DO-loop is frowned upon. So instead, slog it out. And be annoyed afresh by the indeterminacy of boolean expression evaluation of the form (A '''and''' B) or (A '''or''' B) in the context where the test is (''safe'' '''and''' ''test'') because the test might provoke an out-of-bounds fault if not safely within bounds. Like, THIS is being tested against the text in ACARD, but it must not compare beyond the end of the text in ACARD.

The removal of delimited text is taken literally: an incoming card's content might be entirely within a block comment and so be entirely rejected; if so, a null line results in ALINE, and it is ''not'' written to the output file. In other words, lines of block comment are not preserved as blank lines, nor as null lines, they are not there. Only if a line contains text outside of a block comment will it survive. Outside delimited block comments, spaces are just as valid as any other symbol, and are preserved. So, for example, it is <code>a =  b + c ;</code> not <code>a = b + c ;</code> - two spaces after the = sign. Similarly, trailing spaces on a line survive - though I have UltraEdit set to trim trailing spaces and it is not clear whether the example source is to be regarded as containing them or not.

The presence of the delimiters is determined without context, for instance irrespective of whether or not they are inside quoted strings. They cannot be split across lines and recognised, even though in Fortran itself such splitting is permissible. If this process is applied to its own source file, then the only change is to produce <code>CALL UNBLOCK("")</code>, which could be avoided if the statement were to be <code>CALL UNBLOCK("/"//"*","*/")</code> so that the starting delimiter would not be self-identifying. The ending delimiter could be treated in the same way if there was fear that a block comment might have been started earlier in the source file.

A feature of Fortran's character comparison is that trailing spaces are ignored, so that "x  " and "x " and "x" are all deemed equal. Unfortunate choices of starting and ending delimiter texts can be made if they contain characters in common.


```Fortran

      SUBROUTINE UNBLOCK(THIS,THAT)	!Removes block comments bounded by THIS and THAT.
Copies from file INF to file OUT, record by record, except skipping null output records.
       CHARACTER*(*) THIS,THAT	!Starting and ending markers.
       INTEGER LOTS			!How long is a piece of string?
       PARAMETER (LOTS = 6666)		!This should do.
       CHARACTER*(LOTS) ACARD,ALINE	!Scratchpads.
       INTEGER LC,LL,L		!Lengths.
       INTEGER L1,L2		!Scan fingers.
       INTEGER NC,NL		!Might as well count records read and written.
       LOGICAL BLAH		!A state: in or out of a block comment.
       INTEGER MSG,KBD,INF,OUT		!I/O unit numbers.
       COMMON /IODEV/MSG,KBD,INF,OUT	!Thus.
        NC = 0		!No cards read in.
        NL = 0		!No lines written out.
        BLAH = .FALSE.	!And we're not within a comment.
Chug through the input.
   10   READ(INF,11,END = 100) LC,ACARD(1:MIN(LC,LOTS))	!Yum.
   11   FORMAT (Q,A)		!Sez: how much remains (Q), then, characters (A).
        NC = NC + 1		!A card has been read.
        IF (LC.GT.LOTS) THEN	!Paranoia.
          WRITE (MSG,12) NC,LC,LOTS	!Scream.
   12     FORMAT ("Record ",I0," has length ",I0,"! My limit is ",I0)
          LC = LOTS			!Stay calm, and carry on.
        END IF			!None of this should happen.
Chew through ACARD according to mood.
        LL = 0		!No output yet.
        L2 = 0		!Syncopation. Where the previous sniff ended.
   20   L1 = L2 + 1	!The start of what we're looking at.
        IF (L1.LE.LC) THEN	!Anything left?
          L2 = L1		!Yes. This is the probe.
          IF (BLAH) THEN	!So, what's our mood?
   21       IF (L2 + LEN(THAT) - 1 .LE. LC) THEN	!We're skipping stuff.
              IF (ACARD(L2:L2 + LEN(THAT) - 1).EQ.THAT) THEN	!An ender yet?
                BLAH = .FALSE.		!Yes!
                L2 = L2 + LEN(THAT) - 1	!Finger its final character.
                GO TO 20		!And start a new advance.
              END IF		!But if that wasn't an ender,
              L2 = L2 + 1	!Advance one.
              GO TO 21		!And try again.
            END IF	!By here, insufficient text remains to match THAT, so we're finished with ACARD.
           ELSE		!Otherwise, if we're not in a comment, we're looking at grist.
   22       IF (L2 + LEN(THIS) - 1 .LE. LC) THEN	!Enough text to match a comment starter?
              IF (ACARD(L2:L2 + LEN(THIS) - 1).EQ.THIS) THEN	!Yes. Does it?
                BLAH = .TRUE.		!Yes!
                L = L2 - L1		!Recalling where this state started.
                ALINE(LL + 1:LL + L) = ACARD(L1:L2 - 1)	!Copy the non-BLAH text.
                LL = LL + L		!L2 fingers the first of THIS.
                L2 = L2 + LEN(THIS) - 1	!Finger the last matching THIS.
                GO TO 20		!And resume.
              END IF		!But if that wasn't a comment starter,
              L2 = L2 + 1	!Advance one.
              GO TO 22		!And try again.
            END IF	!But if there remains insufficient to match THIS
            L = LC - L1 + 1	!Then the remainder of the line is grist.
            ALINE(LL + 1:LL + L) = ACARD(L1:LC)	!So grab it.
            LL = LL + L		!And count it in.
          END IF	!By here, we're finished witrh ACARD.
        END IF	!So much for ACARD.
Cast forth some output.
        IF (LL.GT.0) THEN	!If there is any.
          WRITE (OUT,23) ALINE(1:LL)	!There is.
   23     FORMAT (">",A,"<") 		!Just text, but with added bounds.
          NL = NL + 1			!Count a line.
        END IF        		!So much for output.
        GO TO 10	!Perhaps there is some more input.
Completed.
  100   WRITE (MSG,101) NC,NL	!Be polite.
  101   FORMAT (I0," read, ",I0," written.")
      END       !No attention to context, such as quoted strings.

      PROGRAM TEST
      INTEGER MSG,KBD,INF,OUT
      COMMON /IODEV/MSG,KBD,INF,OUT
      KBD = 5
      MSG = 6
      INF = 10
      OUT = 11
      OPEN (INF,FILE="Source.txt",STATUS="OLD",ACTION="READ")
      OPEN (OUT,FILE="Src.txt",STATUS="REPLACE",ACTION="WRITE")

      CALL UNBLOCK("/*","*/")

      END	!All open files are closed on exit..

```


Output: the report is "16 read, 8 written." And in the output file appears...

```txt

>  <
>   function subroutine() {<
>    a =  b + c ;<
>   }<
>   <
>   <
>    function something() {<
>    }<

```

Where for expository purposes the > ... < mark the bounds of the surviving text, thus showing surviving spaces.
 
Once one has an axe in one's hands, everything looks like a tree. A slight variation produces the following stump:

```txt

>      <
>     !No attention to context, such as quoted strings.<
>      PROGRAM TEST<
>      INTEGER MSG,KBD,INF,OUT<
>      COMMON /IODEV/MSG,KBD,INF,OUT<
>      KBD = 5<
>      MSG = 6<
>      INF = 10<
>      OUT = 11<
>      OPEN (INF,FILE="Laconic.for",STATUS="OLD",ACTION="READ")<
>      OPEN (OUT,FILE="Src.txt",STATUS="REPLACE",ACTION="WRITE")<
>      CALL UNBLOCK("")<
>      END	!All open files are closed on exit..<

```

Where the source statement is <code>CALL UNBLOCK("SUBROUTINE","END  ")</code> Note that if the ending delimiter were to be "END" there would be trouble. While "end" in the commentary would be missed because I use capitals for Fortran source but normal for commentary, there are plenty of other "END" sequences. Using "END " still would not work because of END IF, but "END  " does work - once I added a comment on the line so that the line doesn't end with "END", and, used spaces rather than a tab after the END.

F90 allows the syntax END SUBROUTINE UNBLOCK (and insists on it within a MODULE) but F77 does not, otherwise the statement could have been <code>CALL UNBLOCK("SUBROUTINE","END SUBROUTINE")</code> which would be rather more structured.

=={{header|F_Sharp|F#}}==
Using .NET's regex counter feature to match nested comments.
If comments here are nested, they have to be correctly balanced.

```fsharp
open System
open System.Text.RegularExpressions

let balancedComments opening closing =
    new Regex(
        String.Format("""
{0}                       # An outer opening delimiter
    (?>                   # efficiency: no backtracking here
        {0} (?<LEVEL>)    # An opening delimiter, one level down
        | 
        {1} (?<-LEVEL>)   # A closing delimiter, one level up
        |
        (?! {0} | {1} ) . # With negative lookahead: Anything but delimiters
    )*                    # As many times as we see these
    (?(LEVEL)(?!))        # Fail, unless on level 0 here
{1}                       # Outer closing delimiter
""", Regex.Escape(opening), Regex.Escape(closing)),
        RegexOptions.IgnorePatternWhitespace ||| RegexOptions.Singleline)

[<EntryPoint>]
let main args =
    let sample = """
    /**
    * Some comments
    * longer comments here that we can parse.
    *
    * Rahoo 
    */
    function subroutine() {
    a = /* inline comment */ b + c ;
    }
    /*/ <-- tricky comments */

    /**
    * Another comment.
    * /* nested balanced
    */ */
    function something() {
    }
    """
    let balancedC = balancedComments "/*" "*/"
    printfn "%s" (balancedC.Replace(sample, ""))
    0
```

Output

```txt


    function subroutine() {
    a =  b + c ;
    }



    function something() {
    }

```



## Go

For the extra credit:  No optional parameters in Go, but documented below is an efficient technique for letting the caller specify the delimiters.

```go
package main

import (
    "fmt"
    "strings"
)

// idiomatic to name a function newX that allocates an object, initializes it,
// and returns it ready to use.  the object in this case is a closure.
func newStripper(start, end string) func(string) string {
    // default to c-style block comments
    if start == "" || end == "" {
        start, end = "/*", "*/"
    }
    // closes on variables start, end.
    return func(source string) string {
        for {
            cs := strings.Index(source, start)
            if cs < 0 {
                break
            }
            ce := strings.Index(source[cs+2:], end)
            if ce < 0 {
                break
            }
            source = source[:cs] + source[cs+ce+4:]
        }
        return source
    }
}

func main() {
    // idiomatic is that zero values indicate to use meaningful defaults
    stripC := newStripper("", "")

    // strip function now defined and can be called any number of times
    // without respecifying delimiters
    fmt.Println(stripC(`  /**
   * Some comments
   * longer comments here that we can parse.
   *
   * Rahoo
   */
   function subroutine() {
    a = /* inline comment */ b + c ;
   }
   /*/ <-- tricky comments */

   /**
    * Another comment.
    */
    function something() {
    }`))
}
```



## Groovy


```groovy
def code = """
  /**
   * Some comments
   * longer comments here that we can parse.
   *
   * Rahoo
   */
   function subroutine() {
    a = /* inline comment */ b + c ;
   }
   /*/ <-- tricky comments */

   /**
    * Another comment.
    */
    function something() {
    }
"""

println ((code =~ "(?:/\\*(?:[^*]|(?:\\*+[^*/]))*\\*+/)|(?://.*)").replaceAll(''))
```



## Haskell

THE FOLLOWING SOLUTION IS WRONG, as it does not take string literals into account.  For example:

```Haskell
test = "This {- is not the beginning of a block comment" -- Do your homework properly -}
```

Comment delimiters can be changed by calling stripComments with different start and end parameters.

```Haskell
import Data.List

stripComments :: String -> String -> String -> String
stripComments start end = notComment
    where notComment :: String -> String
          notComment "" = ""
          notComment xs
            | start `isPrefixOf` xs = inComment $ drop (length start) xs
            | otherwise             = head xs:(notComment $ tail xs)
          inComment :: String -> String
          inComment "" = ""
          inComment xs
            | end `isPrefixOf` xs = notComment $ drop (length end) xs
            | otherwise           = inComment $ tail xs

main = interact (stripComments "/*" "*/")
```

Output:

```txt

   function subroutine() {
    a =  b + c ;
   }
   

   
    function something() {
    }


```


=={{header|Icon}} and {{header|Unicon}}==
If one is willing to concede that the program file will fit in memory, then the following code works:

```Icon
procedure main()
   every (unstripped := "") ||:= !&input || "\n"   # Load file as one string
   write(stripBlockComment(unstripped,"/*","*/"))
end

procedure stripBlockComment(s1,s2,s3)  #: strip comments between s2-s3 from s1
   result := ""
   s1 ? {
      while result ||:= tab(find(s2)) do {
         move(*s2)
         tab(find(s3)|0)   # or end of string 
         move(*s3)
         }
      return result || tab(0)
      }
end
```

Otherwise, the following handles an arbitrary length input:

```Icon
procedure main()
   every writes(stripBlockComment(!&input,"/*","*/"))
end
 
procedure stripBlockComment(s,s2,s3)
    static inC          # non-null when inside comment
    (s||"\n") ?  while not pos(0) do {
            if /inC then 
                if inC := 1(tab(find(s2))\1, move(*s2)) then suspend inC
                else return tab(0)
            else if (tab(find(s3))\1,move(*s3)) then inC := &null
            else fail
            }
end
```



## J


```j
strip=:#~1 0 _1*./@:(|."0 1)2>4{"1(5;(0,"0~".;._2]0 :0);'/*'i.a.)&;:
  1 0 0
  0 2 0
  2 3 2
  0 2 2
)
```

Example data:

```j
example=: 0 :0
  /**
   * Some comments
   * longer comments here that we can parse.
   *
   * Rahoo 
   */
   function subroutine() {
    a = /* inline comment */ b + c ;
   }
   /*/ <-- tricky comments */

   /**
    * Another comment.
    */
    function something() {
    }
)
```

Example use:

```j
   strip example
  
   function subroutine() {
    a =  b + c ;
   }
   

   
    function something() {
    }
```

Here is a version which allows the delimiters to be passed as an optional left argument as a pair of strings:

```j
stripp=:3 :0
  ('/*';'*/') stripp y
:
  'open close'=. x
  marks=. (+./(-i._1+#open,close)|."0 1 open E. y) - close E.&.|. y
  y #~  -. (+._1&|.) (1 <. 0 >. +)/\.&.|. marks
)
```



## Java


```java
import java.io.*;

public class StripBlockComments{
    public static String readFile(String filename) {
	BufferedReader reader = new BufferedReader(new FileReader(filename));
	try {
	    StringBuilder fileContents = new StringBuilder();
	    char[] buffer = new char[4096];
	    while (reader.read(buffer, 0, 4096) > 0) {
		fileContents.append(buffer);
	    }
	    return fileContents.toString();
	} finally {
	    reader.close();
	}
    }

    public static String stripComments(String beginToken, String endToken,
				       String input) {
	StringBuilder output = new StringBuilder();
	while (true) {
	    int begin = input.indexOf(beginToken);
	    int end = input.indexOf(endToken, begin+beginToken.length());
	    if (begin == -1 || end == -1) {
		output.append(input);
		return output.toString();
	    }
	    output.append(input.substring(0, begin));
	    input = input.substring(end + endToken.length());
	}
    }

    public static void main(String[] args) {
	if (args.length < 3) {
	    System.out.println("Usage: BeginToken EndToken FileToProcess");
	    System.exit(1);
	}

	String begin = args[0];
	String end = args[1];
	String input = args[2];

	try {
	    System.out.println(stripComments(begin, end, readFile(input)));
	} catch (Exception e) {
	    e.printStackTrace();
	    System.exit(1);
	}
    }
}
```



## jq

''Note: A version of jq with <tt>gsub/3</tt> is required to compile the function defined in this section.'' 

The filter <tt>strip_block_comments/2</tt> as defined here does not attempt to recognize comments-within-comments.

```jq
def strip_block_comments(open; close):
  def deregex:
    reduce ("\\\\", "\\*", "\\^", "\\?", "\\+", "\\.", 
            "\\!", "\\{", "\\}", "\\[", "\\]", "\\$", "\\|" ) as $c
      (.; gsub($c; $c));
  # "?" => reluctant, "m" => multiline
  gsub( (open|deregex) + ".*?" + (close|deregex); ""; "m") ;

strip_block_comments("/*"; "*/")
```

'''Invocation''':
 $ jq -s -R -r -f Strip_block_comments.jq sample_text_for_stripping.txt


## Julia

{{works with|Julia|0.6}}
{{trans|Python}}

```julia
function _stripcomments(txt::AbstractString, dlm::Tuple{String,String})
    "Strips first nest of block comments"

    dlml, dlmr = dlm
    indx = searchindex(txt, dlml)
    if indx > 0
        out = IOBuffer()
        write(out, txt[1:indx-1])
        txt = txt[indx+length(dlml):end]
        txt = _stripcomments(txt, dlm)
        indx = searchindex(txt, dlmr)
        @assert(indx > 0, "cannot find a closer delimiter \"$dlmr\" in $txt")
        write(out, txt[indx+length(dlmr):end])
    else
        out = txt
    end
    return String(out)
end

function stripcomments(txt::AbstractString, dlm::Tuple{String,String}=("/*", "*/"))
    "Strips nests of block comments"

    dlml, dlmr = dlm
    while contains(txt, dlml)
        txt = _stripcomments(txt, dlm)
    end

    return txt
end

function main()
    println("\nNON-NESTED BLOCK COMMENT EXAMPLE:")
    smpl = """
/**
* Some comments
* longer comments here that we can parse.
*
* Rahoo
*/
function subroutine() {
a = /* inline comment */ b + c ;
}
/*/ <-- tricky comments */

/**
* Another comment.
*/
function something() {
}
"""
    println(stripcomments(smpl))

    println("\nNESTED BLOCK COMMENT EXAMPLE:")
    smpl = """
/**
* Some comments
* longer comments here that we can parse.
*
* Rahoo
*//*
function subroutine() {
a = /* inline comment */ b + c ;
}
/*/ <-- tricky comments */
*/
/**
* Another comment.
*/
function something() {
}
"""
    println(stripcomments(smpl))
end

main()
```


{{out}}

```txt

NON-NESTED BLOCK COMMENT EXAMPLE:

function subroutine() {
a =  b + c ;
}



function something() {
}


NESTED BLOCK COMMENT EXAMPLE:


function something() {
}

```



## Kotlin


```scala
// version 1.1.4-3

val sample = """
/**
   * Some comments
   * longer comments here that we can parse.
   *
   * Rahoo 
   */
   function subroutine() {
    a = /* inline comment */ b + c ;
   }
   /*/ <-- tricky comments */

   /**
    * Another comment.
    */
    function something() {
    }
"""

val sample2 = """
``{
   ` Some comments
   ` longer comments here that we can parse.
   `
   ` Rahoo 
   ``}
   function subroutine2() {
    d = ``{ inline comment ``} e + f ;
   }
   ``{ / <-- tricky comments ``}

   ``{
    ` Another comment.
    ``}
    function something2() {
    }
"""

fun stripBlockComments(text: String, del1: String = "/*", del2: String = "*/"): String {
    val d1 = Regex.escape(del1)
    val d2 = Regex.escape(del2)
    val r = Regex("""(?s)$d1.*?$d2""") 
    return text.replace(r, "")
}

fun main(args: Array<String>) {
    println(stripBlockComments(sample))
    println(stripBlockComments(sample2, "``{", "``}"))
}
```


{{out}}

```txt


   function subroutine() {
    a =  b + c ;
   }
   

   
    function something() {
    }



   function subroutine2() {
    d =  e + f ;
   }
   

   
    function something2() {
    }


```



## Liberty BASIC


```lb
global CRLF$
CRLF$ =chr$( 13) +chr$( 10)

sample$ ="  /**"+CRLF$+_
"   * Some comments"+CRLF$+_
"   * longer comments here that we can parse."+CRLF$+_
"   *"+CRLF$+_
"   * Rahoo "+CRLF$+_
"   */"+CRLF$+_
"   function subroutine() {"+CRLF$+_
"    a = /* inline comment */ b + c ;"+CRLF$+_
"   }"+CRLF$+_
"   /*/ <-- tricky comments */"+CRLF$+_
""+CRLF$+_
"   /**"+CRLF$+_
"    * Another comment."+CRLF$+_
"    */"+CRLF$+_
"    function something() {"+CRLF$+_
"    }"+CRLF$

startDelim$  ="/*"
finishDelim$ ="*/"

print "________________________________"
print sample$
print "________________________________"
print blockStripped$( sample$, startDelim$, finishDelim$)
print "________________________________"

end

function blockStripped$( in$, s$, f$)
    for i =1 to len( in$) -len( s$)
        if mid$( in$, i, len( s$)) =s$ then
            i =i +len( s$)
            do
                if mid$( in$, i, 2) =CRLF$ then blockStripped$ =blockStripped$ +CRLF$
                i =i +1
            loop until ( mid$( in$, i, len( f$)) =f$) or ( i =len( in$) -len( f$))
            i =i +len( f$) -1
        else
            blockStripped$ =blockStripped$ +mid$( in$, i, 1)
        end if
    next i
end function
```


```txt






function subroutine() {
a = b + c ;
}





function something() {
}

 

```



## Lua

It is assumed, that the code is in the file "Text1.txt".

```lua
filename = "Text1.txt"

fp = io.open( filename, "r" )
str = fp:read( "*all" )
fp:close()

stripped = string.gsub( str, "/%*.-%*/", "" )
print( stripped )
```



## Mathematica


```Mathematica
StringReplace[a,"/*"~~Shortest[___]~~"*/" -> ""]

-> 
   function subroutine() {
    a =  b + c ;
   }
   

   
    function something() {
    }
```


=={{header|MATLAB}} / {{header|Octave}}==

```Matlab
function str = stripblockcomment(str,startmarker,endmarker) 
   while(1) 
      ix1 = strfind(str, startmarker);
      if isempty(ix1) return; end;
      ix2 = strfind(str(ix1+length(startmarker):end),endmarker);
      if isempty(ix2) 
         str = str(1:ix1(1)-1);
         return;
      else
         str = [str(1:ix1(1)-1),str(ix1(1)+ix2(1)+length(endmarker)+1:end)];
      end; 
   end;	
end;
```

Output:

```txt

>>t = '  /**\n   * Some comments\n   * longer comments here that we can parse.\n   *\n   * Rahoo \n   */\n   function subroutine() {\n    a = /* inline comment */ b + c ;\n   }\n   /*/ <-- tricky comments */\n\n   /**\n    * Another comment.\n    */\n    function something() {\n    }\n'
>>printf(t);
>>printf('
### =======
\n');
>>printf(stripblockcomment(t));
  /**
   * Some comments
   * longer comments here that we can parse.
   *
   * Rahoo 
   */
   function subroutine() {
    a = /* inline comment */ b + c ;
   }
   /*/ <-- tricky comments */

   /**
    * Another comment.
    */
    function something() {
    }

### =========

  
   function subroutine() {
    a =  b + c ;
   }
   

   
    function something() {
    }



```



## Nim

{{trans|Python}}

```nim
import strutils

proc commentStripper(txt; delim: tuple[l,r: string] = ("/*", "*/")): string =
  let i = txt.find(delim.l)
  if i < 0:
    return txt

  result = if i > 0: txt[0 .. <i] else: ""
  let tmp = commentStripper(txt[i+delim.l.len .. txt.high])
  let j = tmp.find(delim.r)
  assert j >= 0
  result &= tmp[j+delim.r.len .. tmp.high]

echo "NON-NESTED BLOCK COMMENT EXAMPLE:"
echo commentStripper("""/**
   * Some comments
   * longer comments here that we can parse.
   *
   * Rahoo 
   */
   function subroutine() {
    a = /* inline comment */ b + c ;
   }
   /*/ <-- tricky comments */
 
   /**
    * Another comment.
    */
    function something() {
    }""")

echo "\nNESTED BLOCK COMMENT EXAMPLE:"
echo commentStripper("""  /**
   * Some comments
   * longer comments here that we can parse.
   *
   * Rahoo 
   *//*
   function subroutine() {
    a = /* inline comment */ b + c ;
   }
   /*/ <-- tricky comments */
   */
   /**
    * Another comment.
    */
    function something() {
    }""")
```

Output:

```txt
NON-NESTED BLOCK COMMENT EXAMPLE:

   function subroutine() {
    a =  b + c ;
   }
   
 
   
    function something() {
    }

NESTED BLOCK COMMENT EXAMPLE:
  
   
    function something() {
    }
```



## Perl


```Perl
#!/usr/bin/perl -w 
use strict ;
use warnings ;

open( FH , "<" , "samplecode.txt" ) or die "Can't open file!$!\n" ;
my $code = "" ;
{
   local $/ ;
   $code = <FH> ; #slurp mode
}
close FH ;
$code =~ s,/\*.*?\*/,,sg ;
print $code . "\n" ;
```

Output:

```txt

function subroutine() {
    a =  b + c ;
   }
   

   
    function something() {
    }

```



## Perl 6


```perl6
sample().split(/ '/*' .+? '*/' /).print;

sub sample {
'   /**
    * Some comments
    * longer comments here that we can parse.
    *
    * Rahoo
    */
    function subroutine() {
     a = /* inline comment */ b + c ;
    }
    /*/ <-- tricky comments */

    /**
     * Another comment.
     */
    function something() {
    }
'}
```

Output:

```txt
   
    function subroutine() {
     a =  b + c ;
    }
    

    
    function something() {
    }

```



## Phix


```Phix
constant test = """
  /**
   * Some comments
   * longer comments here that we can parse.
   *
   * Rahoo 
   */
   function subroutine() {
    a = /* inline comment */ b + c ;
   }
   /*/ <-- tricky comments */

   /**
    * Another comment.
    */
    function something() {
    }
"""

function strip_comments(string text, startc="/*", endc="*/")
    while 1 do
        integer startp = match(startc,text)
        if startp=0 then exit end if
        integer endp = match(endc,text,startp+length(startc))
        if endp=0 then
            puts(1,"error, aborting...")
            abort(0)
        end if
        text[startp..endp+length(endc)-1] = ""
    end while
    return text
end function

puts(1,strip_comments(test))
```

{{out}}

```txt


   function subroutine() {
    a =  b + c ;
   }



    function something() {
    }

```



## PHP


```PHP

function strip_block_comments( $test_string ) {
	$pattern = "/^.*?(\K\/\*.*?\*\/)|^.*?(\K\/\*.*?^.*\*\/)$/mXus";
	return preg_replace( $pattern, '', $test_string );
}

echo "Result: '" . strip_block_comments( "
/**
 * Some comments
 * longer comments here that we can parse.
 *
 * Rahoo 
 */
 function subroutine() {
  a = /* inline comment */ b + c ;
 }
 /*/ <-- tricky comments */

 /**
  * Another comment.
  */
  function something() {
  }
" ) . "'";

```


{{out}}

```txt

Result: '

 function subroutine() {
  a =  b + c ;
 }



  function something() {
  }
'

```



## PicoLisp


```PicoLisp
(in "sample.txt"
   (while (echo "/*")
      (out "/dev/null" (echo "*/")) ) )
```

Output:

```txt


   function subroutine() {
    a =  b + c ;
   }
   

   
    function something() {
    }
```



## PL/I


```PL/I
/* A program to remove comments from text. */
strip: procedure options (main);                   /* 8/1/2011 */
   declare text character (80) varying;
   declare (j, k) fixed binary;

   on endfile (sysin) stop;

   do forever;
      get edit (text) (L);
      do until (k = 0);
         k = index(text, '/*');
         if k > 0 then /* we have a start of comment. */
            do;
               /* Look for end of comment. */
               j = index(text, '*/', k+2);
               if j > 0 then
                  do;
                     text = substr(text, 1, k-1) ||
                            substr(text, j+2, length(text)-(j+2)+1);
                  end;
               else
                  do; /* The comment continues onto the next line. */
                     put skip list ( substr(text, 1, k-1) );
more:                get edit (text) (L);
                     j = index(text, '*/');
                     if j = 0 then do; put skip; go to more; end;
                     text = substr(text, j+2, length(text) - (j+2) + 1);
                  end;
            end;
      end;
      put skip list (text);
   end;

end strip;
```



## PureBasic

Solution using regular expressions.  A procedure to stripBlocks() procedure is defined that will strip comments between any two delimeters.

```PureBasic
Procedure.s escapeChars(text.s)
  Static specialChars.s = "[\^$.|?*+()"
  Protected output.s, nextChar.s, i, countChar = Len(text)
  For i = 1 To countChar
    nextChar = Mid(text, i, 1)
    If FindString(specialChars, nextChar, 1)
      output + "\" + nextChar
    Else
      output + nextChar
    EndIf 
  Next
  ProcedureReturn output
EndProcedure

Procedure.s stripBlocks(text.s, first.s, last.s)
  Protected delimter_1.s = escapeChars(first), delimter_2.s = escapeChars(last)
  Protected expNum = CreateRegularExpression(#PB_Any, delimter_1 + ".*?" + delimter_2, #PB_RegularExpression_DotAll)
  Protected output.s = ReplaceRegularExpression(expNum, text, "")
  FreeRegularExpression(expNum)
  ProcedureReturn output
EndProcedure

Define source.s
source.s = "  /**" + #CRLF$
source.s + "   * Some comments" + #CRLF$
source.s + "   * longer comments here that we can parse." + #CRLF$
source.s + "   *" + #CRLF$
source.s + "   * Rahoo " + #CRLF$
source.s + "   */" + #CRLF$
source.s + "   function subroutine() {" + #CRLF$
source.s + "    a = /* inline comment */ b + c ;" + #CRLF$
source.s + "   }" + #CRLF$
source.s + "   /*/ <-- tricky comments */" + #CRLF$
source.s + "" + #CRLF$
source.s + "   /**" + #CRLF$
source.s + "    * Another comment." + #CRLF$
source.s + "    */" + #CRLF$
source.s + "    function something() {" + #CRLF$
source.s + "    }" + #CRLF$

If OpenConsole()
  PrintN("--- source ---")
  PrintN(source)
  PrintN("--- source with block comments between '/*' and '*/' removed ---")
  PrintN(stripBlocks(source, "/*", "*/"))
  PrintN("--- source with block comments between '*' and '*' removed ---")
  PrintN(stripBlocks(source, "*", "*"))
   
  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
--- source ---
  /**
   * Some comments
   * longer comments here that we can parse.
   *
   * Rahoo
   */
   function subroutine() {
    a = /* inline comment */ b + c ;
   }
   /*/ <-- tricky comments */

   /**
    * Another comment.
    */
    function something() {
    }

--- source with block comments between '/*' and '*/' removed ---

   function subroutine() {
    a =  b + c ;
   }



    function something() {
    }

--- source with block comments between '*' and '*' removed ---
  /
    longer comments here that we can parse.
    Rahoo
    inline comment / <-- tricky comments  Another comment.
    */
    function something() {
    }


```



## Python

The code has comment delimeters as an argument and will also strip ''nested'' block comments.

```python
def _commentstripper(txt, delim):
    'Strips first nest of block comments'
    
    deliml, delimr = delim
    out = ''
    if deliml in txt:
        indx = txt.index(deliml)
        out += txt[:indx]
        txt = txt[indx+len(deliml):]
        txt = _commentstripper(txt, delim)
        assert delimr in txt, 'Cannot find closing comment delimiter in ' + txt
        indx = txt.index(delimr)
        out += txt[(indx+len(delimr)):]
    else:
        out = txt
    return out

def commentstripper(txt, delim=('/*', '*/')):
    'Strips nests of block comments'
    
    deliml, delimr = delim
    while deliml in txt:
        txt = _commentstripper(txt, delim)
    return txt
```

;Tests and sample output

```python
def test():
    print('\nNON-NESTED BLOCK COMMENT EXAMPLE:')
    sample = '''  /**
   * Some comments
   * longer comments here that we can parse.
   *
   * Rahoo 
   */
   function subroutine() {
    a = /* inline comment */ b + c ;
   }
   /*/ <-- tricky comments */

   /**
    * Another comment.
    */
    function something() {
    }'''
    print(commentstripper(sample))

    print('\nNESTED BLOCK COMMENT EXAMPLE:')
    sample = '''  /**
   * Some comments
   * longer comments here that we can parse.
   *
   * Rahoo 
   *//*
   function subroutine() {
    a = /* inline comment */ b + c ;
   }
   /*/ <-- tricky comments */
   */
   /**
    * Another comment.
    */
    function something() {
    }'''
    print(commentstripper(sample))
    
if __name__ == '__main__':
    test()
```



```txt

NON-NESTED BLOCK COMMENT EXAMPLE:
  
   function subroutine() {
    a =  b + c ;
   }
   

   
    function something() {
    }

NESTED BLOCK COMMENT EXAMPLE:
  
   
    function something() {
    }
```



## Racket



```Racket

#lang at-exp racket

;; default delimiters (strings -- not regexps)
(define comment-start-str "/*")
(define comment-end-str "*/")

(define (strip-comments text [rx1 comment-start-str] [rx2 comment-end-str])
  (regexp-replace* (~a (regexp-quote rx1) ".*?" (regexp-quote rx2))
                   text ""))

((compose1 displayln strip-comments)
 @~a{/**
      * Some comments
      * longer comments here that we can parse.
      *
      * Rahoo
      */
      function subroutine() {
       a = /* inline comment */ b + c ;
      }
      /*/ <-- tricky comments */

      /**
       * Another comment.
       */
       function something() {
       }
    })

```


(Outputs the expected text...)


## REXX


```rexx
/* REXX ***************************************************************
* Split comments
* This program ignores comment delimiters within literal strings
* such as, e.g., in b = "--' O'Connor's widow --";
* it does not (yet) take care of -- comments (ignore rest of line)
* also it does not take care of say 667/*yuppers*/77 (REXX specialty)
*   courtesy GS discussion!
* 12.07.2013 Walter Pachl
**********************************************************************/
fid='in.txt'                           /* input text                 */
oic='oc.txt'; 'erase' oic              /* will contain comments      */
oip='op.txt'; 'erase' oip              /* will contain program parts */
oim='om.txt'; 'erase' oim              /* oc.txt merged with op.txt  */
cmt=0                                  /* comment nesting            */
str=''                                 /* ' or " when in a string    */
Do ri=1 By 1 While lines(fid)>0        /* loop over input            */
  l=linein(fid)                        /* an input line              */
  oc=''                                /* initialize line for oc.txt */
  op=''                                /* initialize line for op.txt */
  i=1                                  /* start at first character   */
  Do While i<=length(l)                /* loop through input line    */
    If cmt=0 Then Do                   /* we are not in a comment    */
      If str<>'' Then Do               /* we are in a string         */
        If substr(l,i,1)=str Then Do   /* string character           */
          If substr(l,i+1,1)=str Then Do /* another one              */
            Call app 'P',substr(l,i,2) /* add '' or "" to op         */
            i=i+2                      /* increase input pointer     */
            Iterate                    /* proceed in input line      */
            End
          Else Do                      /* end of literal string      */
            Call app 'P',substr(l,i,1) /* add ' or " to op           */
            str=' '                    /* no longer in string        */
            i=i+1                      /* increase input pointer     */
            Iterate                    /* proceed in input line      */
            End
          End
        End
      End
    Select
      When str='' &,                   /* not in a string            */
           substr(l,i,2)='/*' Then Do  /* start of comment           */
        cmt=cmt+1                      /* increase commenr nesting   */
        Call app 'C','/*'              /* copy to oc                 */
        i=i+2                          /* increase input pointer     */
        End
      When cmt=0 Then Do               /* not in a comment           */
        If str=' ' Then Do             /* not in a string            */
          If pos(substr(l,i,1),'''"')>0 Then /* string delimiter     */
            str=substr(l,i,1)          /* remember that              */
          End
        Call app 'P',substr(l,i,1)     /* copy to op                 */
        i=i+1                          /* increase input pointer     */
        End
      When substr(l,i,2)='*/' Then Do  /* end of comment             */
        cmt=cmt-1                      /* decrement nesting depth    */
        Call app 'C','*/'              /* copy to oc                 */
        i=i+2                          /* increase input pointer     */
        End
      Otherwise Do                     /* any other character        */
        Call app 'C',substr(l,i,1)     /* copy to oc                 */
        i=i+1                          /* increase input pointer     */
        End
      End
    End
  Call oc                              /* Write line oc              */
  Call op                              /* Write line op              */
  End
Call lineout oic                       /* Close File oic             */
Call lineout oip                       /* Close File oip             */

Do ri=1 To ri-1                        /* merge program with comments*/
  op=linein(oip)
  oc=linein(oic)
  Do i=1 To length(oc)
    If substr(oc,i,1)<>'' Then
      op=overlay(substr(oc,i,1),op,i,1)
    End
  Call lineout oim,op
  End
Call lineout oic
Call lineout oip
Call lineout oim
Exit

app: Parse Arg which,string
/* add str to oc or op                                               */
/* and corresponding blanks to the other (op or oc)                  */
If which='C' Then Do
  oc=oc||string
  op=op||copies(' ',length(string))
  End
Else Do
  op=op||string
  oc=oc||copies(' ',length(string))
  End
Return

oc: Return lineout(oic,oc)
op: Return lineout(oip,op)
```

Input:

```txt

/**
   * Some comments
   * longer comments here that we can parse.
   *
   * Rahoo
   */
   function subroutine() {
    a = /* inline comment */ b + c ;
    b = "*/' O'Connor's widow /*";
   }
   /*/ <-- tricky comments */

   /**
    * Another comment.
    */
    function something() {
    }

```

Program:

```txt







   function subroutine() {
    a =                      b + c ;
    b = "*/' O'Connor's widow /*";
   }





    function something() {
    }


```

Comments:

```txt

/**
   * Some comments
   * longer comments here that we can parse.
   *
   * Rahoo
   */

        /* inline comment */


   /*/ <-- tricky comments */

   /**
    * Another comment.
    */

       

```




## Ring


```ring

example = "123/*456*/abc/*def*/789"
  
example2 = example
nr = 1
while nr = 1
      n1 = substr(example2,"/*")
      n2 = substr(example2,"*/") 
      if n1 > 0 and n2 > 0
         example3 = substr(example2,n1,n2-n1+2) 
         example2 = substr(example2,example3,"")
      else nr = 0 ok
end
see example2 + nl

```



## Ruby


```ruby
def remove_comments!(str, comment_start='/*', comment_end='*/')
  while start_idx = str.index(comment_start) 
    end_idx = str.index(comment_end, start_idx + comment_start.length) + comment_end.length - 1
    str[start_idx .. end_idx] = "" 
  end
  str
end

def remove_comments(str, comment_start='/*', comment_end='*/')
  remove_comments!(str.dup, comment_start, comment_end)
end

example = <<END_OF_STRING
  /**
   * Some comments
   * longer comments here that we can parse.
   *
   * Rahoo 
   */
   function subroutine() {
    a = /* inline comment */ b + c ;
   }
   /*/ <-- tricky comments */

   /**
    * Another comment.
    */
    function something() {
    }
END_OF_STRING

puts remove_comments example
```

outputs

```txt
  
   function subroutine() {
    a =  b + c ;
   }
   

   
    function something() {
    }
```


## Scala


```Scala
import java.util.regex.Pattern.quote
def strip1(x: String, s: String = "/*", e: String = "*/") =
  x.replaceAll("(?s)"+quote(s)+".*?"+quote(e), "")
```


```Scala
def strip2(x: String, s: String = "/*", e: String = "*/"): String = {
  val a = x indexOf s
  val b = x indexOf (e, a + s.length)
  if (a == -1 || b == -1) x
  else strip2(x.take(a) + x.drop(b + e.length), s, e)
}
```


```Scala
def strip3(x: String, s: String = "/*", e: String = "*/"): String = x.indexOf(s) match {
  case -1 => x
  case i => x.indexOf(e, i + s.length) match {
    case -1 => x
    case j => strip2(x.take(i) + x.drop(j + e.length), s, e)
  }
}
```



## Seed7

The function [http://seed7.sourceforge.net/libraries/string.htm#replace2%28in_string,in_string,__in_string,in_string%29 replace2]
can be used to replace unnested comments.


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    const string: stri is "\
        \  /**\n\
        \   * Some comments\n\
        \   * longer comments here that we can parse.\n\
        \   *\n\
        \   * Rahoo\n\
        \   */\n\
        \   function subroutine() {\n\
        \    a = /* inline comment */ b + c ;\n\
        \   }\n\
        \   /*/ <-- tricky comments */\n\
        \\n\
        \   /**\n\
        \    * Another comment.\n\
        \    */\n\
        \    function something() {\n\
        \    }";
  begin
    writeln(replace2(stri, "/*", "*/", " "));
  end func;
```

Output:

```txt

   
   function subroutine() {
    a =   b + c ;
   }
    

    
    function something() {
    }

```



## Sidef

For extra credit, it allows the caller to redefine the delimiters.

```ruby
func strip_block_comments(code, beg='/*', end='*/') {
    var re = Regex.new(beg.escape + '.*?' + end.escape, 's');
    code.gsub(re, '');
}

say strip_block_comments(ARGF.slurp);
```



## Tcl


```tcl
proc stripBlockComment {string {openDelimiter "/*"} {closeDelimiter "*/"}} {
    # Convert the delimiters to REs by backslashing all non-alnum characters
    set openAsRE [regsub -all {\W} $openDelimiter {\\&}]
    set closeAsRE [regsub -all {\W} $closeDelimiter {\\&}]

    # Now remove the blocks using a dynamic non-greedy regular expression
    regsub -all "$openAsRE.*?$closeAsRE" $string ""
}
```

Demonstration code:

```tcl
puts [stripBlockComment "  /**
   * Some comments
   * longer comments here that we can parse.
   *
   * Rahoo 
   */
   function subroutine() {
    a = /* inline comment */ b + c ;
   }
   /*/ <-- tricky comments */

   /**
    * Another comment.
    */
    function something() {
    }
"]
```

Output:

```txt

  
   function subroutine() {
    a =  b + c ;
   }
   

   
    function something() {
    }


```


## TUSCRIPT


```tuscript

$$ MODE DATA
$$ script=*
  /**
   * Some comments
   * longer comments here that we can parse.
   *
   * Rahoo
   */
   function subroutine() {
    a = /* inline comment */ b + c ;
   }
   /*/ <-- tricky comments  */

   /**
    * Another comment.
    */
    function something() {
    }
$$ MODE TUSCRIPT
ERROR/STOP CREATE ("testfile",SEQ-E,-std-)
ERROR/STOP CREATE ("destfile",SEQ-E,-std-)
FILE "testfile" = script
BUILD S_TABLE commentbeg=":/*:"
BUILD S_TABLE commentend=":*/:"

ACCESS t: READ/STREAM "testfile" s.z/u,a/commentbeg+t+e/commentend,typ
ACCESS d: WRITE/STREAM "destfile" s.z/u,a+t+e
LOOP
READ/EXIT t
IF (typ==3) CYCLE
t=SQUEEZE(t)
WRITE/ADJUST d
ENDLOOP
ENDACCESS/PRINT t
ENDACCESS/PRINT d
d=FILE("destfile")
TRACE *d

```

Output:

```txt

TRACE *    38    -*TUSTEP.EDT
d            = *
           1 =
           2 = function subroutine() { a =
           3 = b + c ; }
           4 =
           5 = function something() { }

```



## VBA

Stripping block comments might look easy ...

```vb
'strip block comment NESTED comments
'multi line comments
'and what if there are string literals with these delimeters?
'------------------------
'delimeters for Block Comment can be specified, exactly two characters each
'Three states: Block_Comment, String_Literal, Other_Text
'Globals:
Dim t As String 'target string
Dim s() As Byte 'source array
Dim j As Integer 'index into the source string s, converted to byte array
Dim SourceLength As Integer 'of a base 0 array, so last byte is SourceLength - 1
Dim flag As Boolean
Private Sub Block_Comment(sOpBC As String, sClBC As String)
    'inside a block comment, expecting close block comment delimeter
    flag = False
    Do While j < SourceLength - 2
        Select Case s(j)
            Case Asc(Left(sOpBC, 1))
                If s(j + 1) = Asc(Right(sOpBC, 1)) Then
                    'open block NESTED comment delimeter found
                    j = j + 2
                    Block_Comment sOpBC, sClBC
                End If
            Case Asc(Left(sClBC, 1))
                If s(j + 1) = Asc(Right(sClBC, 1)) Then
                    'close block comment delimeter found
                    flag = True
                    j = j + 2
                    Exit Do
                End If
                'just a lone star
            Case Else
        End Select
        j = j + 1
    Loop
    If Not flag Then MsgBox "Error, missing close block comment delimeter"
End Sub
Private Sub String_Literal()
    'inside as string literal, expecting double quote as delimeter
    flag = False
    Do While j < SourceLength - 2
        If s(j) = Asc("""") Then
            If s(j + 1) = Asc("""") Then
                'found a double quote within a string literal
                t = t + Chr(s(j))
                j = j + 1
            Else
                'close string literal delimeter found
                flag = True
                t = t + Chr(s(j))
                j = j + 1
                Exit Do
            End If
        End If
        t = t + Chr(s(j))
        j = j + 1
    Loop
    If Not flag Then MsgBox "Error, missing closing string delimeter"
End Sub
Private Sub Other_Text(Optional sOpBC As String = "/*", Optional sClBC As String = "*/")
    If Len(sOpBC) <> 2 Then
        MsgBox "Error, open block comment delimeter must be 2" & _
        " characters long, got " & Len(sOpBC) & " characters"
        Exit Sub
    End If
    If Len(sClBC) <> 2 Then
        MsgBox "Error, close block comment delimeter must be 2" & _
        " characters long, got " & Len(sClBC) & " characters"
        Exit Sub
    End If
    Do While j < SourceLength - 1
        Select Case s(j)
            Case Asc(""""):
                t = t + Chr(s(j))
                j = j + 1
                String_Literal
            Case Asc(Left(sOpBC, 1))
                If s(j + 1) = Asc(Right(sOpBC, 1)) Then
                    'open block comment delimeter found
                    j = j + 2
                    Block_Comment sOpBC, sClBC
                Else
                    t = t + Chr(s(j))
                    j = j + 1
                End If
            Case Else
                t = t + Chr(s(j))
                j = j + 1
        End Select
    Loop
    If j = SourceLength - 1 Then t = t + Chr(s(j))
End Sub
Public Sub strip_block_comment()
    Dim n As String
    n = n & "/**" & vbCrLf
    n = n & "* Some comments /*NESTED COMMENT*/" & vbCrLf
    n = n & "* longer comments here that we can parse." & vbCrLf
    n = n & "*" & vbCrLf
    n = n & "* Rahoo" & vbCrLf
    n = n & "*/" & vbCrLf
    n = n & "mystring = ""This is the """"/*"""" open comment block mark.""" & vbCrLf
    'VBA converts two double quotes in a row within a string literal to a single double quote
    'see the output below. Quadruple double quotes become two double quotes within the string
    'to represent a single double quote within a string.
    n = n & "function subroutine() {" & vbCrLf
    n = n & "a = /* inline comment */ b + c ;" & vbCrLf
    n = n & "}" & vbCrLf
    n = n & "/*/ <-- tricky /*NESTED*/ comments */" & vbCrLf
    n = n & "" & vbCrLf
    n = n & "/**" & vbCrLf
    n = n & "* Another comment." & vbCrLf
    n = n & "*/" & vbCrLf
    n = n & "function something() {" & vbCrLf
    n = n & "}"
    s = StrConv(n, vbFromUnicode)
    j = 0
    t = ""
    SourceLength = Len(n)
    Other_Text 'The open and close delimeters for block comment are optional ;)
    Debug.Print "Original text:"
    Debug.Print String$(60, "-")
    Debug.Print n & vbCrLf
    Debug.Print "Text after deleting comment blocks, preserving string literals:"
    Debug.Print String$(60, "-")
    Debug.Print t
End Sub
```
{{out}}
```txt
Original text:
------------------------------------------------------------
/**
* Some comments /*NESTED COMMENT*/
* longer comments here that we can parse.
*
* Rahoo
*/
mystring = "This is the ""/*"" open comment block mark."
function subroutine() {
a = /* inline comment */ b + c ;
}
/*/ <-- tricky /*NESTED*/ comments */

/**
* Another comment.
*/
function something() {
}

Text after deleting comment blocks, preserving string literals:
------------------------------------------------------------

mystring = "This is the ""/*"" open comment block mark."
function subroutine() {
a =  b + c ;
}



function something() {
}
```



## zkl


```zkl
fcn stripper(text, a="/*", b="*/"){
   while(xy:=text.span(a,b,True)){ x,y:=xy; text=text[0,x] + text[x+y,*] }
   text
}
```

The span method takes two tokens and matches the shortest or longest balanced match (if True). It assumes there are no escape characters (such as \ or ""). So we just repeatedly strip out the longest balanced comments until there aren't any left (span returns the empty list). If a comment was unbalanced, span  would fail but this code doesn't check that and just assumes no more matches.
{{out}}
The input (from the task description) is in a file because I'm too lazy to type it in:

```txt


 function subroutine() {
  a =  b + c ;
 }
 

 
  function something() {
  }

```


{{omit from|Bc}}
{{omit from|GUISS|This can only be done, if the editor has such a facility}}
{{omit from|Openscad}}
{{omit from|PARI/GP|No real capacity for string manipulation}}
