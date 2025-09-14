+++
title = "Create an HTML table"
description = ""
date = 2019-10-15T08:58:27Z
aliases = []
[extra]
id = 9338
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "360_assembly",
  "ada",
  "agena",
  "algol_68",
  "arturo",
  "autohotkey",
  "awk",
  "batch_file",
  "bbc_basic",
  "bracmat",
  "c",
  "clojure",
  "coffeescript",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "echolisp",
  "elixir",
  "erlang",
  "euphoria",
  "forth",
  "fortran",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lambdatalk",
  "lasso",
  "liberty_basic",
  "lingo",
  "lua",
  "m2000_interpreter",
  "netrexx",
  "newlisp",
  "nim",
  "objeck",
  "ocaml",
  "oz",
  "pari_gp",
  "pascal",
  "peloton",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "racket",
  "rascal",
  "red",
  "retro",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "standard_ml",
  "stata",
  "tcl",
  "tuscript",
  "unix_shell",
  "ursa",
  "vba",
  "vbscript",
  "visual_basic_.net",
  "xslt",
  "zkl",
]
+++

## Task

{{task}}[[Category:HTML]]
Create an HTML table.
* The table body should have at least three rows of three columns.
* Each of these three columns should be labelled "X", "Y", and "Z".
* An extra column should be added at either the extreme left or the extreme right of the table that has no heading, but is filled with sequential row numbers.
* The rows of the "X", "Y", and "Z" columns should be filled with random or sequential integers having 4 digits or less.
* The numbers should be aligned in the same fashion for all columns.





## 360 Assembly

```360asm
*        Create an HTML table      19/02/2017
CREHTML  CSECT
         USING  CREHTML,R13
         B      72(R15)
         DC     17F'0'
         STM    R14,R12,12(R13)
         ST     R13,4(R15)
         ST     R15,8(R13)
         LR     R13,R15            end of prolog
         LA     R8,RND
         XPRNT  PGBODY,64          <html><head></head><body>
         XPRNT  PGTAB,64           <table border=1 ... cellspacing=0>
         SR     R6,R6              row=0
       DO WHILE=(C,R6,LE,NROWS)    do row=0 to nrows
       IF LTR,R6,Z,R6 THEN           if row=0
         XPRNT  PGTRTH,64              <tr><th></th>
       ELSE     ,                    else
         XDECO  R6,XDEC                edit row
         MVC    PGTR+8(1),XDEC+11      output row heading
         XPRNT  PGTR,64                <tr><th>.</th>
       ENDIF    ,                    endif
         LA     R7,1                 col=1
       DO WHILE=(C,R7,LE,NCOLS)      do col=1 to ncols
       IF LTR,R6,Z,R6 THEN             if row=0
         LR     R1,R7                    col
         LA     R4,TCAR-1(R1)            tcar(col)
         MVC    PGTH+4(1),0(R4)          output heading
         XPRNT  PGTH,64                  <th>.</th>
       ELSE     ,                      else
         L      R2,0(R8)                 value
         XDECO  R2,XDEC                  edit value
         MVC    PGTD+18(4),XDEC+8        output cell value
         XPRNT  PGTD,64                  <td align="right">....</td>
         LA     R8,4(R8)                 next value
       ENDIF    ,                      endif
         LA     R7,1(R7)               col++
       ENDDO    ,                    enddo col
         XPRNT  PGETR,64             </tr>
         LA     R6,1(R6)             row++
       ENDDO    ,                  enddo row
         XPRNT  PGETAB,64          </table>
         XPRNT  PGEBODY,64         </body></html>
         L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)
         XR     R15,R15
         BR     R14                exit
NROWS    DC     F'4'               number of rows
NCOLS    DC     F'3'               number of columns
TCAR     DC     CL3'XYZ'
RND      DC     F'7055',F'5334',F'5795',F'2895',F'3019',F'7747'
         DC     F'140',F'7607',F'8144',F'7090',F'475',F'4140'
PGBODY   DC     CL64'<html><head></head><body>'
PGTAB    DC     CL64'<table border=1 cellpadding=10 cellspacing=0>'
PGTRTH   DC     CL64'<tr><th></th>'
PGTH     DC     CL64'<th>.</th>'
PGETR    DC     CL64'</tr>'
PGTR     DC     CL64'<tr><th>.</th>'
PGTD     DC     CL64'<td align="right">....</td>'
PGETAB   DC     CL64'</table>'
PGEBODY  DC     CL64'</body></html>'
XDEC     DS     CL12
         YREGS
         END    CREHTML
```

```txt

<html><head></head><body>
<table border=1 cellpadding=10 cellspacing=0>
<tr><th></th>
<th>X</th>
<th>Y</th>
<th>Z</th>
</tr>
<tr><th>1</th>
<td align="right">7055</td>
<td align="right">5334</td>
<td align="right">5795</td>
</tr>
<tr><th>2</th>
<td align="right">2895</td>
<td align="right">3019</td>
<td align="right">7747</td>
</tr>
<tr><th>3</th>
<td align="right"> 140</td>
<td align="right">7607</td>
<td align="right">8144</td>
</tr>
<tr><th>4</th>
<td align="right">7090</td>
<td align="right"> 475</td>
<td align="right">4140</td>
</tr>
</table>
</body></html>

```

<table border=1 cellpadding=10 cellspacing=0>
<tr><th></th>
<th>X</th>
<th>Y</th>
<th>Z</th>
</tr>
<tr><th>1</th>
<td align="right">7055</td>
<td align="right">5334</td>
<td align="right">5795</td>
</tr>
<tr><th>2</th>
<td align="right">2895</td>
<td align="right">3019</td>
<td align="right">7747</td>
</tr>
<tr><th>3</th>
<td align="right"> 140</td>
<td align="right">7607</td>
<td align="right">8144</td>
</tr>
<tr><th>4</th>
<td align="right">7090</td>
<td align="right"> 475</td>
<td align="right">4140</td>
</tr>
</table>


## Ada


We define a generic package to output HTML tables:


```Ada
with Ada.Strings.Unbounded;

generic
   type Item_Type is private;
   with function To_String(Item: Item_Type) return String is <>;
   with procedure Put(S: String) is <>;
   with procedure Put_Line(Line: String) is <>;
package HTML_Table is

   subtype U_String is Ada.Strings.Unbounded.Unbounded_String;
   function Convert(S: String) return U_String renames
     Ada.Strings.Unbounded.To_Unbounded_String;

   type Item_Array is array(Positive range <>, Positive range <>) of Item_Type;
   type Header_Array is array(Positive range <>) of U_String;

   procedure Print(Items: Item_Array; Column_Heads: Header_Array);

end HTML_Table;
```


The implementation of the package:


```Ada
package body HTML_Table is

   procedure Print(Items: Item_Array; Column_Heads: Header_Array) is

      function Blanks(N: Natural) return String is
         -- indention for better readable HTML
      begin
         if N=0 then
            return "";
         else
            return " " & Blanks(N-1);
         end if;
      end Blanks;

      procedure Print_Row(Row_Number: Positive) is
      begin
         Put(Blanks(4) & "<tr><td>" & Positive'Image(Row_Number) & "</td>");
         for I in Items'Range(2) loop
            Put("<td>" & To_String(Items(Row_Number, I)) & "</td>");
                end loop;
            Put_Line("</tr>");
      end Print_Row;

      procedure Print_Body is
      begin
         Put_Line(Blanks(2)&"<tbody align = ""right"">");
         for I in Items'Range(1) loop
            Print_Row(I);
         end loop;
         Put_Line(Blanks(2)&"</tbody>");
      end Print_Body;

      procedure Print_Header is
         function To_Str(U: U_String) return String renames
           Ada.Strings.Unbounded.To_String;
      begin
         Put_Line(Blanks(2) & "<thead align = ""right"">");
         Put(Blanks(4) & "<tr><th></th>");
         for I in Column_Heads'Range loop
            Put("<td>" & To_Str(Column_Heads(I)) & "</td>");
         end loop;
         Put_Line("</tr>");
         Put_Line(Blanks(2) & "</thead>");
      end Print_Header;

   begin
      if Items'Length(2) /= Column_Heads'Length then
         raise Constraint_Error with "number of headers /= number of columns";
      end if;
      Put_Line("<table>");
      Print_Header;
      Print_Body;
      Put_Line("</table>");
   end Print;

end HTML_Table;
```


Here is the main program, using an instance of HTML_Table:


```Ada
with Ada.Text_IO, Ada.Numerics.Discrete_Random, HTML_Table;

procedure Test_HTML_Table is

   -- define the Item_Type and the random generator
   type  Four_Digits is mod 10_000;
   package Rand is new Ada.Numerics.Discrete_Random(Four_Digits);
   Gen: Rand.Generator;

   -- now we instantiate the generic package HTML_Table
   package T is new HTML_Table
     (Item_Type => Four_Digits,
      To_String => Four_Digits'Image,
      Put       => Ada.Text_IO.Put,
      Put_Line  => Ada.Text_IO.Put_Line);

   -- define the object that will the values that the table contains
   The_Table: T.Item_Array(1 .. 4, 1..3);

begin
   -- fill The_Table with random values
   Rand.Reset(Gen);
   for Rows in The_Table'Range(1) loop
      for Cols in The_Table'Range(2) loop
         The_Table(Rows, Cols) := Rand.Random(Gen);
      end loop;
   end loop;

   -- output The_Table
   T.Print(Items        => The_Table,
           Column_Heads => (T.Convert("X"), T.Convert("Y"), T.Convert("Z")));
end Test_HTML_Table;
```



Each time you run the program, you get different random values for the table. {{out}}

```html5><table

  <thead align = "right">
    <tr><th></th><td>X</td><td>Y</td><td>Z</td></tr>
  </thead>
  <tbody align = "right">
    <tr><td> 1</td><td> 7255</td><td> 3014</td><td> 9436</td></tr>
    <tr><td> 2</td><td> 554</td><td> 3314</td><td> 8765</td></tr>
    <tr><td> 3</td><td> 4832</td><td> 129</td><td> 2048</td></tr>
    <tr><td> 4</td><td> 31</td><td> 6897</td><td> 8265</td></tr>
  </tbody>
</table>
```


```txt
        X    Y    Z
   1 7255 3014 9436
   2  554 3314 8765
   3 4832  129 2048
   4   31 6897 8265

```



## Agena

Tested with Agena 2.9.5 Win32

```agena
notNumbered     := 0; # possible values for html table row numbering
numberedLeft    := 1; #    "        "    "    "    "    "     "
numberedRight   := 2; #    "        "    "    "    "    "     "

alignCentre     := 0; # possible values for html table column alignment
alignLeft       := 1; #    "        "    "    "    "     "        "
alignRight      := 2; #    "        "    "    "    "     "        "

# write an html table to a file
writeHtmlTable := proc( fh, t :: table ) is
    local align := "align='";
    case t.columnAlignment
      of alignLeft  then align := align & "left'"
      of alignRight then align := align & "right'"
    else                 align := align &  "center'"
    esac;
    local put       := proc( text    :: string ) is io.write( fh, text & "\n" ) end;
    local thElement := proc( content :: string ) is put( "<th " & align & ">" & content & "</th>" ) end;
    local tdElement := proc( content           ) is put( "<td " & align & ">" & content & "</td>" ) end;
    # table element
    put( "<table"
       & " cellspacing='" & t.cellSpacing & "'"
       &  " colspacing='" & t.colSpacing  & "'"
       &      " border='" & t.border      & "'"
       & ">"
       );
    # table headings
    put( "<tr>" );
    if t.rowNumbering =  numberedLeft then thElement( "" ) fi;
    for col to size t.headings do thElement( t.headings[ col ] ) od;
    if t.rowNumbering = numberedRight then thElement( "" ) fi;
    put( "</tr>" );
    # table rows
    for row to size t.data do
        put( "<tr>" );
        if t.rowNumbering =  numberedLeft then thElement( row & "" ) fi;
        for col to size t.data[ row ] do tdElement( t.data[ row, col ] ) od;
        if t.rowNumbering = numberedRight then thElement( row & "" ) fi;
        put( "</tr>" )
    od;
    # end of table
    put( "</table>" )
end ;

# create an html table and print it to standard output
scope
    local t := [];
    t.cellSpacing, t.colSpacing := 0, 0;
    t.border          := 1;
    t.columnAlignment := alignRight;
    t.rowNumbering    := numberedLeft;
    t.headings        := [ "A", "B", "C" ];
    t.data            := [ [ 1001, 1002, 1003 ], [ 21, 22, 23 ], [ 201, 202, 203 ] ];
    writeHtmlTable( io.stdout, t )
epocs
```

<table cellspacing='0' colspacing='0' border='1'>
<tr>
<th align='right'></th>
<th align='right'>A</th>
<th align='right'>B</th>
<th align='right'>C</th>
</tr>
<tr>
<th align='right'>1</th>
<td align='right'>1001</td>
<td align='right'>1002</td>
<td align='right'>1003</td>
</tr>
<tr>
<th align='right'>2</th>
<td align='right'>21</td>
<td align='right'>22</td>
<td align='right'>23</td>
</tr>
<tr>
<th align='right'>3</th>
<td align='right'>201</td>
<td align='right'>202</td>
<td align='right'>203</td>
</tr>
</table>


## ALGOL 68


```algol68
INT not numbered     = 0; # possible values for HTMLTABLE row numbering    #
INT numbered left    = 1; #    "        "    "      "      "      "        #
INT numbered right   = 2; #    "        "    "      "      "      "        #

INT align centre     = 0; # possible values for HTMLTABLE column alignment #
INT align left       = 1; #   "        "    "      "         "       "     #
INT align right      = 2; #   "        "    "      "         "       "     #

# allowable content for the HTML table - extend the UNION and TOSTRING     #
#                                        operator to add additional modes  #
MODE HTMLTABLEDATA   = UNION( INT, REAL, STRING );
OP TOSTRING          = ( HTMLTABLEDATA content )STRING:
    CASE content
      IN ( INT    i ): whole( i, 0 )
       , ( REAL   r ): fixed( r, 0, 0 )
       , ( STRING s ): s
     OUT "Unsupported HTMLTABLEDATA content"
    ESAC;
# MODE to hold an html table                                               #
MODE HTMLTABLE       = STRUCT( FLEX[    0 ]STRING        headings
                             , FLEX[ 0, 0 ]HTMLTABLEDATA data
                             , INT   row numbering
                             , INT   column alignment
                             , INT   cell spacing
                             , INT   col spacing
                             , INT   border
                             );
# write an html table to a file                                            #
PROC write html table = ( REF FILE f, HTMLTABLE t )VOID:
BEGIN
    STRING align = "align="""
                 + CASE column alignment OF t IN "left", "right" OUT "center" ESAC
                 + """";
    PROC th element = ( REF FILE f, HTMLTABLE t, STRING content )VOID:
        put( f, ( "<th " + align + ">" + content + "</th>", newline ) );
    PROC td element = ( REF FILE f, HTMLTABLE t, HTMLTABLEDATA content )VOID:
        put( f, ( "<td " + align + ">" + TOSTRING content + "</td>", newline ) );

    # table element #
    put( f, ( "<table"
            + " cellspacing=""" + whole( cell spacing OF t, 0 ) + """"
            +  " colspacing=""" + whole( col spacing  OF t, 0 ) + """"
            +      " border=""" + whole( border       OF t, 0 ) + """"
            + ">"
            , newline
            )
       );
    # table headings                                                       #
    put( f, ( "<tr>", newline ) );
    IF row numbering OF t =  numbered left THEN th element( f, t, "" ) FI;
    FOR col FROM LWB headings OF t TO UPB headings OF t DO
        th element( f, t, ( headings OF t )[ col ] )
    OD;
    IF row numbering OF t = numbered right THEN th element( f, t, "" ) FI;
    put( f, ( "</tr>", newline ) );
    # table rows                                                           #
    FOR row FROM 1 LWB data OF t TO 1 UPB data OF t DO
        put( f, ( "<tr>", newline ) );
        IF row numbering OF t =  numbered left THEN th element( f, t, whole( row, 0 ) ) FI;
        FOR col FROM 2 LWB data OF t TO 2 UPB data OF t DO
            td element( f, t, ( data OF t )[ row, col ] )
        OD;
        IF row numbering OF t = numbered right THEN th element( f, t, whole( row, 0 ) ) FI;
        put( f, ( "</tr>", newline ) )
    OD;
    # end of table                                                         #
    put( f, ( "</table>", newline ) )
END # write html table # ;

# create an HTMLTABLE and print it to standard output                      #
HTMLTABLE t;
cell spacing     OF t := col spacing OF t := 0;
border           OF t := 1;
column alignment OF t := align right;
row numbering    OF t := numbered left;
headings         OF t := ( "A", "B", "C" );
data             OF t := ( ( 1001, 1002, 1003 ), ( 21, 22, 23 ), ( 201, 202, 203 ) );
write html table( stand out, t )
```

<table cellspacing="0" colspacing="0" border="1">
<tr>
<th align="right"></th>
<th align="right">A</th>
<th align="right">B</th>
<th align="right">C</th>
</tr>
<tr>
<th align="right">1</th>
<td align="right">1001</td>
<td align="right">1002</td>
<td align="right">1003</td>
</tr>
<tr>
<th align="right">2</th>
<td align="right">21</td>
<td align="right">22</td>
<td align="right">23</td>
</tr>
<tr>
<th align="right">3</th>
<td align="right">201</td>
<td align="right">202</td>
<td align="right">203</td>
</tr>
</table>


## Arturo



```arturo
use ~web

table @tbl #{
	row #{
		cell ""
		cell "X" #{ :header true }
		cell "Y" #{ :header true }
		cell "Z" #{ :header true }
	}

	join $(map $(range 0 3) {
		r $(row #{
			cell $(toString &) #{ :header true }
			cell $(toString|random 0 1000)
			cell $(toString|random 0 1000)
			cell $(toString|random 0 1000)
		})
		r.render
	}) ""
}

print $(tbl.render)
```


```html
<table id='tbl'>
   <tr>
      <td></td>
      <th>X</th>
      <th>Y</th>
      <th>Z</th>
   </tr>
   <tr>
      <th>0</th>
      <td>85</td>
      <td>785</td>
      <td>85</td>
   </tr>
   <tr>
      <th>1</th>
      <td>80</td>
      <td>156</td>
      <td>37</td>
   </tr>
   <tr>
      <th>2</th>
      <td>457</td>
      <td>960</td>
      <td>814</td>
   </tr>
   <tr>
      <th>3</th>
      <td>360</td>
      <td>154</td>
      <td>433</td>
   </tr>
</table>
```


<table id='tbl'>
   <tr>
      <td></td>
      <th>X</th>
      <th>Y</th>
      <th>Z</th>
   </tr>
   <tr>
      <th>0</th>
      <td>85</td>
      <td>785</td>
      <td>85</td>
   </tr>
   <tr>
      <th>1</th>
      <td>80</td>
      <td>156</td>
      <td>37</td>
   </tr>
   <tr>
      <th>2</th>
      <td>457</td>
      <td>960</td>
      <td>814</td>
   </tr>
   <tr>
      <th>3</th>
      <td>360</td>
      <td>154</td>
      <td>433</td>
   </tr>
</table>


## AutoHotkey

```AutoHotkey
out = <table style="text-align:center; border: 1px solid"><th></th><th>X</th><th>Y</th><th>Z</th><tr>
Loop 4
    out .= "`r`n<tr><th>" A_Index "</th><td>" Rand() "</td><td>" Rand() "</td><td>" Rand() "</tr>"
out .= "`r`n</table>"
MsgBox % clipboard := out

Rand(u=1000){
    Random, n, 1, % u
    return n
}
```

<table style="text-align:center; border: 1px solid"><th></th><th>X</th><th>Y</th><th>Z</th><tr>
<tr><th>1</th><td>289</td><td>556</td><td>43</tr>
<tr><th>2</th><td>102</td><td>100</td><td>971</tr>
<tr><th>3</th><td>582</td><td>295</td><td>264</tr>
<tr><th>4</th><td>396</td><td>762</td><td>633</tr>
</table>


## AWK



```AWK
#!/usr/bin/awk -f
BEGIN {
   print "<table>\n  <thead align = \"right\">"
   printf "    <tr><th></th><td>X</td><td>Y</td><td>Z</td></tr>\n  </thead>\n  <tbody align = \"right\">\n"
   for (i=1; i<=10; i++) {
       printf "    <tr><td>%2i</td><td>%5i</td><td>%5i</td><td>%5i</td></tr>\n",i, 10*i, 100*i, 1000*i-1
   }
   print "  </tbody>\n</table>\n"
}
```

```txt
<table>
  <thead align = "right">
    <tr><th></th><td>X</td><td>Y</td><td>Z</td></tr>
  </thead>
  <tbody align = "right">
    <tr><td> 1</td><td>   10</td><td>  100</td><td>  999</td></tr>
    <tr><td> 2</td><td>   20</td><td>  200</td><td> 1999</td></tr>
    <tr><td> 3</td><td>   30</td><td>  300</td><td> 2999</td></tr>
    <tr><td> 4</td><td>   40</td><td>  400</td><td> 3999</td></tr>
    <tr><td> 5</td><td>   50</td><td>  500</td><td> 4999</td></tr>
    <tr><td> 6</td><td>   60</td><td>  600</td><td> 5999</td></tr>
    <tr><td> 7</td><td>   70</td><td>  700</td><td> 6999</td></tr>
    <tr><td> 8</td><td>   80</td><td>  800</td><td> 7999</td></tr>
    <tr><td> 9</td><td>   90</td><td>  900</td><td> 8999</td></tr>
    <tr><td>10</td><td>  100</td><td> 1000</td><td> 9999</td></tr>
  </tbody>
</table>

```



## Batch File

This will overwrite any file by the name of "table.html" in the current directory.

```dos

@echo off
setlocal enabledelayedexpansion

:: It's easier and neater to create the variables holding the random 4 digit numbers ahead of time
for /l %%i in (1,1,12) do set /a rand%%i=!random! %% 9999

:: The command output of everything within the brackets is sent to the file "table.html", overwriting anything already in there
(
  echo ^<html^>^<head^>^</head^>^<body^>
  echo ^<table border=1 cellpadding=10 cellspacing=0^>
  echo ^<tr^>^<th^>^</th^>
  echo ^<th^>X^</th^>
  echo ^<th^>Y^</th^>
  echo ^<th^>Z^</th^>
  echo ^</tr^>
  echo ^<tr^>^<th^>1^</th^>
  echo ^<td align="right"^>%rand1%^</td^>
  echo ^<td align="right"^>%rand2%^</td^>
  echo ^<td align="right"^>%rand3%^</td^>
  echo ^</tr^>
  echo ^<tr^>^<th^>2^</th^>
  echo ^<td align="right"^>%rand4%^</td^>
  echo ^<td align="right"^>%rand5%^</td^>
  echo ^<td align="right"^>%rand6%^</td^>
  echo ^</tr^>
  echo ^<tr^>^<th^>3^</th^>
  echo ^<td align="right"^>%rand7%^</td^>
  echo ^<td align="right"^>%rand8%^</td^>
  echo ^<td align="right"^>%rand9%^</td^>
  echo ^</tr^>
  echo ^<tr^>^<th^>4^</th^>
  echo ^<td align="right"^>%rand10%^</td^>
  echo ^<td align="right"^>%rand11%^</td^>
  echo ^<td align="right"^>%rand12%^</td^>
  echo ^</tr^>
  echo ^</table^>
  echo ^</body^>^</html^>
) > table.html
start table.html

```

```txt

<html><head></head><body>
<table border=1 cellpadding=10 cellspacing=0>
<tr><th></th>
<th>X</th>
<th>Y</th>
<th>Z</th>
</tr>
<tr><th>1</th>
<td align="right">6608</td>
<td align="right">3993</td>
<td align="right">4797</td>
</tr>
<tr><th>2</th>
<td align="right">498</td>
<td align="right">4475</td>
<td align="right">2218</td>
</tr>
<tr><th>3</th>
<td align="right">3742</td>
<td align="right">9842</td>
<td align="right">1729</td>
</tr>
<tr><th>4</th>
<td align="right">9251</td>
<td align="right">478</td>
<td align="right">3945</td>
</tr>
</table>
</body></html>

```


<table border=1 cellpadding=10 cellspacing=0>
<tr><th></th>
<th>X</th>
<th>Y</th>
<th>Z</th>
</tr>
<tr><th>1</th>
<td align="right">6608</td>
<td align="right">3993</td>
<td align="right">4797</td>
</tr>
<tr><th>2</th>
<td align="right">498</td>
<td align="right">4475</td>
<td align="right">2218</td>
</tr>
<tr><th>3</th>
<td align="right">3742</td>
<td align="right">9842</td>
<td align="right">1729</td>
</tr>
<tr><th>4</th>
<td align="right">9251</td>
<td align="right">478</td>
<td align="right">3945</td>
</tr>
</table>



## BBC BASIC

Uses BBC BASIC's *spool command to create a file.

```bbcbasic
      ncols% = 3
      nrows% = 4

      *spool temp.htm

      PRINT "<html><head></head><body>"
      PRINT "<table border=1 cellpadding=10 cellspacing=0>"

      FOR row% = 0 TO nrows%
        IF row% = 0 THEN
          PRINT "<tr><th></th>" ;
        ELSE
          PRINT "<tr><th>" ; row% "</th>" ;
        ENDIF
        FOR col% = 1 TO ncols%
          IF row% = 0 THEN
            PRINT "<th>" CHR$(87 + col%) "</th>" ;
          ELSE
            PRINT "<td align=""right"">" ; RND(9999) "</td>" ;
          ENDIF
        NEXT col%
        PRINT "</tr>"
      NEXT row%

      PRINT "</table>"
      PRINT "</body></html>"

      *spool

      SYS "ShellExecute", @hwnd%, 0, "temp.htm", 0, 0, 1

```

```txt
&lt;html&gt;&lt;head&gt;&lt;/head&gt;&lt;body&gt;
&lt;table border=1 cellpadding=10 cellspacing=0&gt;
&lt;tr&gt;&lt;th&gt;&lt;/th&gt;&lt;th&gt;X&lt;/th&gt;&lt;th&gt;Y&lt;/th&gt;&lt;th&gt;Z&lt;/th&gt;&lt;/tr&gt;
&lt;tr&gt;&lt;th&gt;1&lt;/th&gt;&lt;td align="right"&gt;2791&lt;/td&gt;&lt;td align="right"&gt;8011&lt;/td&gt;&lt;td align="right"&gt;9582&lt;/td&gt;
&lt;/tr&gt;
&lt;tr&gt;&lt;th&gt;2&lt;/th&gt;&lt;td align="right"&gt;6793&lt;/td&gt;&lt;td align="right"&gt;6863&lt;/td&gt;&lt;td align="right"&gt;4790&lt;/td&gt;
&lt;/tr&gt;
&lt;tr&gt;&lt;th&gt;3&lt;/th&gt;&lt;td align="right"&gt;8064&lt;/td&gt;&lt;td align="right"&gt;2626&lt;/td&gt;&lt;td align="right"&gt;3917&lt;/td&gt;
&lt;/tr&gt;
&lt;tr&gt;&lt;th&gt;4&lt;/th&gt;&lt;td align="right"&gt;2660&lt;/td&gt;&lt;td align="right"&gt;8776&lt;/td&gt;&lt;td align="right"&gt;6805&lt;/td&gt;
&lt;/tr&gt;
&lt;/table&gt;
&lt;/body&gt;&lt;/html&gt;
```

[[File:HTMLtable_BBC.gif]]


## Bracmat

To make this interesting, the table is created as a Bracmat structure and then converted to XML by a library. This has the advantage that one doesn't have to worry about HTML entities and encoding issues. The contents of the cells are generated by a function that is instantiated with a starting number when the function makeTable is called. Notice the absence of loops.

```bracmat
(  ( makeTable
  =     headTexts
        minRowNr
        maxRowNr
        headCells
        cells
        rows
        Generator
        Table
    .   get$"xmlio.bra"             { A library that converts from Bracmat format to XML or HTML }
      & !arg:(?headTexts.?minRowNr.?maxRowNr.?Generator)
      & ( headCells
        =   cellText
          .     !arg:%?cellText ?arg
              & (th.,!cellText) headCells$!arg
            |
        )
      & ( cells
        =   cellText cellTexts numberGenerator
          .       !arg
                : (%?cellText ?cellTexts.(=?numberGenerator))
              &   (td.,numberGenerator$)
                  cells$(!cellTexts.'$numberGenerator)
            |
        )
      & ( rows
        =   headTexts rowNr maxRowNr Generator
          .     !arg:(?headTexts.?rowNr.?maxRowNr.?Generator)
              & !rowNr:~>!maxRowNr
              &   ( tr
                  .
                    ,   (td.,!rowNr)
                        cells$(!headTexts.!Generator)
                  )
                  \n
                  rows$(!headTexts.!rowNr+1.!maxRowNr.!Generator)
            |
        )
      &   ( table
          .
            ,   ( thead
                .   (align.right)
                  , \n (tr.,(th.," ") headCells$!headTexts)
                )
                \n
                ( tbody
                .   (align.right)
                  ,   \n
                        rows
                      $ (!headTexts.!minRowNr.!maxRowNr.!Generator)
                )
          )
        : ?Table
      & str$((XMLIO.convert)$!Table)      { Call library function to create HTML }
  )
&   makeTable
  $ ( X Y Z                               { Column headers }
    . 1                                   { Lowest row number }
    . 4                                   { Highest row number }
    .                                     { Function that generates numbers 9, 10, ...}
      ' ( cnt
        .   (cnt=$(new$(==8)))            { This creates an object 'cnt' with scope as a local function variable that survives between calls. }
          & !(cnt.)+1:?(cnt.)
        )
    )
)
```

```txt
&lt;table&gt;&lt;thead align="right"&gt;
&lt;tr&gt;&lt;th&gt; &lt;/th&gt;&lt;th&gt;X&lt;/th&gt;&lt;th&gt;Y&lt;/th&gt;&lt;th&gt;Z&lt;/th&gt;&lt;/tr&gt;&lt;/thead&gt;
&lt;tbody align="right"&gt;
&lt;tr&gt;&lt;td&gt;1&lt;/td&gt;&lt;td&gt;9&lt;/td&gt;&lt;td&gt;10&lt;/td&gt;&lt;td&gt;11&lt;/td&gt;&lt;/tr&gt;
&lt;tr&gt;&lt;td&gt;2&lt;/td&gt;&lt;td&gt;12&lt;/td&gt;&lt;td&gt;13&lt;/td&gt;&lt;td&gt;14&lt;/td&gt;&lt;/tr&gt;
&lt;tr&gt;&lt;td&gt;3&lt;/td&gt;&lt;td&gt;15&lt;/td&gt;&lt;td&gt;16&lt;/td&gt;&lt;td&gt;17&lt;/td&gt;&lt;/tr&gt;
&lt;tr&gt;&lt;td&gt;4&lt;/td&gt;&lt;td&gt;18&lt;/td&gt;&lt;td&gt;19&lt;/td&gt;&lt;td&gt;20&lt;/td&gt;&lt;/tr&gt;
&lt;/tbody&gt;&lt;/table&gt;
```



## C


```c
#include <stdio.h>
#include <stdlib.h>

int main()
{
	int i;
	printf("<table style=\"text-align:center; border: 1px solid\"><th></th>"
		"<th>X</th><th>Y</th><th>Z</th>");
	for (i = 0; i < 4; i++) {
		printf("<tr><th>%d</th><td>%d</td><td>%d</td><td>%d</td></tr>", i,
			rand() % 10000, rand() % 10000, rand() % 10000);
	}
	printf("</table>");

	return 0;
}
```

{{out}} (wiki doesn't like tbody/thead tags):<table style="text-align:center; border: 1px solid"><th></th><th>X</th><th>Y</th><th>Z</th><tr><th>0</th><td>2777</td><td>886</td><td>9383</td></tr><tr><th>1</th><td>8335</td><td>7793</td><td>6915</td></tr><tr><th>2</th><td>6649</td><td>492</td><td>5386</td></tr><tr><th>3</th><td>27</td><td>2362</td><td>1421</td></tr></table>


## C++


```cpp
#include <fstream>
#include <boost/array.hpp>
#include <string>
#include <cstdlib>
#include <ctime>
#include <sstream>

void makeGap( int gap , std::string & text ) {
   for ( int i = 0 ; i < gap ; i++ )
      text.append( " " ) ;
}

int main( ) {
   boost::array<char , 3> chars = { 'X' , 'Y' , 'Z' } ;
   int headgap = 3 ;
   int bodygap = 3 ;
   int tablegap = 6 ;
   int rowgap = 9 ;
   std::string tabletext( "<html>\n" ) ;
   makeGap( headgap , tabletext ) ;
   tabletext += "<head></head>\n" ;
   makeGap( bodygap , tabletext ) ;
   tabletext += "<body>\n" ;
   makeGap( tablegap , tabletext ) ;
   tabletext += "<table>\n" ;
   makeGap( tablegap + 1 , tabletext ) ;
   tabletext += "<thead align=\"right\">\n" ;
   makeGap( tablegap, tabletext ) ;
   tabletext += "<tr><th></th>" ;
   for ( int i = 0 ; i < 3 ; i++ ) {
      tabletext += "<td>" ;
      tabletext += *(chars.begin( ) + i ) ;
      tabletext += "</td>" ;
   }
   tabletext += "</tr>\n" ;
   makeGap( tablegap + 1 , tabletext ) ;
   tabletext += "</thead>" ;
   makeGap( tablegap + 1 , tabletext ) ;
   tabletext += "<tbody align=\"right\">\n" ;
   srand( time( 0 ) ) ;
   for ( int row = 0 ; row < 5 ; row++ ) {
      makeGap( rowgap , tabletext ) ;
      std::ostringstream oss ;
      tabletext += "<tr><td>" ;
      oss << row ;
      tabletext += oss.str( ) ;
      for ( int col = 0 ; col < 3 ; col++ ) {
	 oss.str( "" ) ;
	 int randnumber = rand( ) % 10000 ;
	 oss << randnumber ;
	 tabletext += "<td>" ;
	 tabletext.append( oss.str( ) ) ;
	 tabletext += "</td>" ;
      }
      tabletext += "</tr>\n" ;
   }
   makeGap( tablegap + 1 , tabletext ) ;
   tabletext += "</tbody>\n" ;
   makeGap( tablegap , tabletext ) ;
   tabletext += "</table>\n" ;
   makeGap( bodygap , tabletext ) ;
   tabletext += "</body>\n" ;
   tabletext += "</html>\n" ;
   std::ofstream htmltable( "testtable.html" , std::ios::out | std::ios::trunc ) ;
   htmltable << tabletext ;
   htmltable.close( ) ;
   return 0 ;
}
```

{{out}} ( of testtable.html ):
<LANG html5><html>
   <head></head>
   <body>
      <table>
       <thead align="right">
      <tr><th></th><td>X</td><td>Y</td><td>Z</td></tr>
       </thead>
       <tbody align="right">
         <tr><td>0<td>1274</td><td>6847</td><td>352</td></tr>
         <tr><td>1<td>846</td><td>6577</td><td>4612</td></tr>
         <tr><td>2<td>7543</td><td>1644</td><td>8143</td></tr>
         <tr><td>3<td>4928</td><td>5714</td><td>8186</td></tr>
         <tr><td>4<td>3436</td><td>7493</td><td>9344</td></tr>
       </tbody>
      </table>
   </body>
</html>
</LANG>

## C#


```c#
using System;
using System.Text;

namespace prog
{
	class MainClass
	{
		public static void Main (string[] args)
		{
			StringBuilder s = new StringBuilder();
			Random rnd = new Random();

			s.AppendLine("<table>");
			s.AppendLine("<thead align = \"right\">");
			s.Append("<tr><th></th>");
			for(int i=0; i<3; i++)
				s.Append("<td>" + "XYZ"[i] + "</td>");
			s.AppendLine("</tr>");
			s.AppendLine("</thead>");
			s.AppendLine("<tbody align = \"right\">");
			for( int i=0; i<3; i++ )
			{
				s.Append("<tr><td>"+i+"</td>");
				for( int j=0; j<3; j++ )
					s.Append("<td>"+rnd.Next(10000)+"</td>");
				s.AppendLine("</tr>");
			}
			s.AppendLine("</tbody>");
			s.AppendLine("</table>");

			Console.WriteLine( s );
		}
	}
}
```



###  More modern version


```c#
using System;
using System.Text;
using System.Xml;

namespace N
{
	public class T
	{
		public static void Main()
		{
			var headers = new [] { "", "X", "Y", "Z" };

			var cols = headers.Select(name =>
				new XElement(
					"th",
					name,
					new XAttribute("text-align", "center")
				)
			);

			var rows = Enumerable.Range(0, 4).Select(ri =>
				new XElement(
					"tr",
					new XElement("td", ri),
					Enumerable.Range(0, 4).Select(ci =>
						new XElement(
							"td",
							ci,
							new XAttribute("text-align", "center")
						)
					)
				)
			);

			var xml = new XElement(
				"table",
				new XElement(
					"thead",
					new XElement("tr",    cols),
					new XElement("tbody", rows)
				)
			);

			Console.WriteLine(xml.ToString());
		}
	}
}

```


## Clojure

Using Hiccup (https://github.com/weavejester/hiccup):

```clojure
(ns rosettacode.html-table
  (:use 'hiccup.core))

(defn <tr> [el sq]
  [:tr (map vector (cycle [el]) sq)])

(html
  [:table
    (<tr> :th ["" \X \Y \Z])
    (for [n (range 1 4)]
      (->> #(rand-int 10000) (repeatedly 3) (cons n) (<tr> :td)))])
```


## CoffeeScript


```coffeescript

# This is one of many ways to create a table.  CoffeeScript plays nice
# with any templating solution built for JavaScript, and of course you
# can build tables in the browser using DOM APIs.  This approach is just
# brute force string manipulation.

table = (header_row, rows) ->
  """
  <table>
  #{header_row}
  #{rows.join '\n'}
  </table>
  """

tr = (cells) -> "<tr>#{cells.join ''}</tr>"
th = (s) -> "<th align='right'>#{s}</th>"
td = (s) -> "<td align='right'>#{s}</td>"
rand_n = -> Math.floor Math.random() * 10000

header_cols = ['', 'X', 'Y', 'Z']
header_row = tr (th s for s in header_cols)

rows = []
for i in [1..5]
  rows.push tr [
    th(i)
    td rand_n()
    td rand_n()
    td rand_n()
  ]

html = table header_row, rows
console.log html

```


<table>
<tr><th align='right'></th><th align='right'>X</th><th align='right'>Y</th><th align='right'>Z</th></tr>
<tr><th align='right'>1</th><td align='right'>1575</td><td align='right'>7445</td><td align='right'>4544</td></tr>
<tr><th align='right'>2</th><td align='right'>3827</td><td align='right'>5029</td><td align='right'>235</td></tr>
<tr><th align='right'>3</th><td align='right'>10</td><td align='right'>9363</td><td align='right'>4381</td></tr>
<tr><th align='right'>4</th><td align='right'>297</td><td align='right'>2463</td><td align='right'>5745</td></tr>
<tr><th align='right'>5</th><td align='right'>4063</td><td align='right'>6726</td><td align='right'>1093</td></tr>
</table>


## Common Lisp

Using Closure-HTML (https://common-lisp.net/project/closure/closure-html/) installed by the code itself via QuickLisp (http://quicklisp.org/).


```Common Lisp

(ql:quickload :closure-html)
(use-package :closure-html)
(serialize-lhtml
 `(table nil
	 (tr nil ,@(mapcar (lambda (x)
			     (list 'th nil x))
			   '("" "X" "Y" "Z")))
	 ,@(loop for i from 1 to 4
	      collect `(tr nil
			   (th nil ,(format nil "~a" i))
			   ,@(loop repeat 3 collect `(td nil ,(format nil "~a" (random 10000)))))))
 (make-string-sink))

```


<TABLE><TR><TH></TH><TH>X</TH><TH>Y</TH><TH>Z</TH></TR><TR><TH>1</TH><TD>1189</TD><TD>4560</TD><TD>4983</TD></TR><TR><TH>2</TH><TD>7739</TD><TD>9597</TD><TD>5737</TD></TR><TR><TH>3</TH><TD>3403</TD><TD>8767</TD><TD>5852</TD></TR><TR><TH>4</TH><TD>8081</TD><TD>2238</TD><TD>177</TD></TR></TABLE>


## D

```d
void main() {
  import std.stdio, std.random;

  writeln(`<table style="text-align:center; border: 1px solid">`);
  writeln("<th></th><th>X</th><th>Y</th><th>Z</th>");
  foreach (immutable i; 0 .. 4)
    writefln("<tr><th>%d</th><td>%d</td><td>%d</td><td>%d</td></tr>",
             i, uniform(0,1000), uniform(0,1000), uniform(0,1000));
  writeln("</table>");
}
```

<table style="text-align:center; border: 1px solid">
<th></th><th>X</th><th>Y</th><th>Z</th>
<tr><th>0</th><td>524</td><td>739</td><td>847</td></tr>
<tr><th>1</th><td>13</td><td>813</td><td>782</td></tr>
<tr><th>2</th><td>926</td><td>580</td><td>663</td></tr>
<tr><th>3</th><td>309</td><td>816</td><td>750</td></tr>
</table>


## Delphi


```Delphi
program CreateHTMLTable;

{$APPTYPE CONSOLE}

uses SysUtils;

function AddTableRow(aRowNo: Integer): string;
begin
  Result := Format('  <tr><td>%d</td><td>%d</td><td>%d</td><td>%d</td></tr>',
    [aRowNo, Random(10000), Random(10000), Random(10000)]);
end;

var
  i: Integer;
begin
  Randomize;
  Writeln('<table>');
  Writeln('  <tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>');
  for i := 1 to 4 do
    Writeln(AddTableRow(i));
  Writeln('</table>');
  Readln;
end.
```


```html5><table

  <tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>
  <tr><td>1</td><td>7371</td><td>2659</td><td>1393</td></tr>
  <tr><td>2</td><td>6710</td><td>5025</td><td>5203</td></tr>
  <tr><td>3</td><td>1316</td><td>1599</td><td>2086</td></tr>
  <tr><td>4</td><td>4785</td><td>6612</td><td>5042</td></tr>
</table>
```



## EchoLisp


```scheme

;; styles -
(style 'td "text-align:right")
(style 'table "border-spacing: 10px;border:1px solid red")
(style 'th "color:blue;")

;; generic html5 builder
;; pushes <tag style=..> (proc content) </tag>
(define  (emit-tag tag html-proc content )
		 (if (style tag)
		 	(push html (format "<%s style='%a'>" tag (style tag)))
		 	(push html (format "<%s>" tag )))
		 (html-proc content)
		 (push html (format "</%s> " tag )))

;; html procs : 1 tag, 1 proc
(define (h-raw content)
		(push html (format "%s" content)))
(define (h-header headers)
		(for ((h headers)) (emit-tag 'th h-raw h)))
(define (h-row row)
		(for ((item row)) (emit-tag 'td h-raw item)))
(define (h-table table )
	(emit-tag 'tr h-header (first table))
	;; add row-num  i at head of row
	(for ((i 1000)(row (rest table))) (emit-tag 'tr h-row (cons i row))))


```

```scheme

(define my-table '(("" X Y Z) (-1111 111 11) (22 -222 2222) (4422 0  42) (33 333 3333) (6666 666 66)))

(stack (define html 'html)) ;; stack of html elements
(emit-tag 'table h-table my-table)
(string-join (stack->list html) " ")

```


<table style='border-spacing: 10px;border:1px solid red'> <table style='border-spacing: 10px;border:1px solid red'> <tr style='undefined'> <th style='color:blue;'> </th> <th style='color:blue;'> X </th> <th style='color:blue;'> Y </th> <th style='color:blue;'> Z </th> </tr> <tr style='undefined'> <td style='text-align:right'> 0 </td> <td style='text-align:right'> -1111 </td> <td style='text-align:right'> 111 </td> <td style='text-align:right'> 11 </td> </tr> <tr style='undefined'> <td style='text-align:right'> 1 </td> <td style='text-align:right'> 22 </td> <td style='text-align:right'> -222 </td> <td style='text-align:right'> 2222 </td> </tr> <tr style='undefined'> <td style='text-align:right'> 2 </td> <td style='text-align:right'> 4422 </td> <td style='text-align:right'> 0 </td> <td style='text-align:right'> 42 </td> </tr> <tr style='undefined'> <td style='text-align:right'> 3 </td> <td style='text-align:right'> 33 </td> <td style='text-align:right'> 333 </td> <td style='text-align:right'> 3333 </td> </tr> <tr style='undefined'> <td style='text-align:right'> 4 </td> <td style='text-align:right'> 6666 </td> <td style='text-align:right'> 666 </td> <td style='text-align:right'> 66 </td> </tr> </table>


## Elixir



```Elixir
defmodule Table do
  defp put_rows(n) do
    Enum.map_join(1..n, fn i ->
      "<tr align=right><th>#{i}</th>" <>
      Enum.map_join(1..3, fn _ ->
        "<td>#{:rand.uniform(2000)}</td>"
      end) <> "</tr>\n"
    end)
  end

  def create_table(n\\3) do
    "<table border=1>\n" <>
    "<th></th><th>X</th><th>Y</th><th>Z</th>\n" <>
    put_rows(n) <>
    "</table>"
  end
end

IO.puts Table.create_table
```


```html5><table border=1

<th></th><th>X</th><th>Y</th><th>Z</th>
<tr align=right><th>1</th><td>1362</td><td>1289</td><td>357</td></tr>
<tr align=right><th>2</th><td>1161</td><td>1345</td><td>1176</td></tr>
<tr align=right><th>3</th><td>391</td><td>62</td><td>609</td></tr>
</table>
```


<table border=1>
<th></th><th>X</th><th>Y</th><th>Z</th>
<tr align=right><th>1</th><td>1362</td><td>1289</td><td>357</td></tr>
<tr align=right><th>2</th><td>1161</td><td>1345</td><td>1176</td></tr>
<tr align=right><th>3</th><td>391</td><td>62</td><td>609</td></tr>
</table>


## Erlang

Both external_format/1 and html_table/3 are used by [[CSV_to_HTML_translation]]. Keep backwards compatibility when changing or change both.

```Erlang

-module( create_html_table ).

-export( [external_format/1, html_table/3, task/0] ).

external_format( XML ) -> remove_quoutes( lists:flatten(xmerl:export_simple_content([XML], xmerl_xml)) ).

html_table( Table_options, Headers, Contents ) ->
	Header = html_table_header( Headers ),
	Records = [html_table_record(X) || X <- Contents],
	{table, Table_options, [Header | Records]}.

task() ->
	Headers = [" ", "X", "Y", "Z"],
	Contents = [[erlang:integer_to_list(X), random(), random(), random()] || X <- lists:seq(1, 3)],
	external_format( html_table([{border, 1}, {cellpadding, 10}], Headers, Contents) ).



html_table_header( Items ) -> {tr, [], [{th, [], [X]} || X <- Items]}.

html_table_record( Items ) -> {tr, [], [{td, [], [X]} || X <- Items]}.

random() -> erlang:integer_to_list( random:uniform(1000) ).

remove_quoutes( String ) -> lists:flatten( string:tokens(String, "\"") ).

```

<table border=1 cellpadding=10><tr><th> </th><th>X</th><th>Y</th><th>Z</th></tr><tr><td>1</td><td>6</td><td>563</td><td>476</td></tr><tr><td>2</td><td>401</td><td>310</td><td>59</td></tr><tr><td>3</td><td>579</td><td>990</td><td>331</td></tr></table>


## Euphoria


```euphoria
puts(1,"<table>\n")
puts(1,"  <tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>\n")
for i = 1 to 3 do
    printf(1,"  <tr><td>%d</td>",i)
    for j = 1 to 3 do
        printf(1,"<td>%d</td>",rand(10000))
    end for
    puts(1,"</tr>\n")
end for
puts(1,"</table>")
```


```html5><table

  <tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>
  <tr><td>1</td><td>7978</td><td>7376</td><td>2382</td></tr>
  <tr><td>2</td><td>3632</td><td>1947</td><td>8900</td></tr>
  <tr><td>3</td><td>4098</td><td>1563</td><td>2762</td></tr>
</table>
```



=={{header|F_Sharp|F#}}==

```fsharp
open System.Xml

type XmlDocument with
    member this.Add element =
        this.AppendChild element
    member this.Element name =
        this.CreateElement(name) :> XmlNode
    member this.Element (name, (attr : (string * string) list)) =
        let node = this.CreateElement(name)
        for a in attr do
            node.SetAttribute (fst a, snd a)
        node
    member this.Element (name, (text : string)) =
        let node = this.CreateElement(name)
        node.AppendChild(this.Text text) |> ignore
        node
    member this.Text text =
        this.CreateTextNode(text)
    end

type XmlNode with
    member this.Add element =
        this.AppendChild element
    end

let head = [""; "X"; "Y"; "Z"]

let xd = new XmlDocument()
let html = xd.Add (xd.Element("html"))
html.Add(xd.Element("head"))
    .Add(xd.Element("title", "RosettaCode: Create_an_HTML_table"))
let table = html.Add(xd.Element("body")).Add(xd.Element("table", [("style", "text-align:right")]))
let tr1 = table.Add(xd.Element("tr"))
for th in head do
    tr1.Add(xd.Element("th", th)) |> ignore
for i in [1; 2; 3] do
    let tr = table.Add(xd.Element("tr"))
    tr.Add(xd.Element("th", i.ToString())) |> ignore
    for j in [1; 2; 3] do
        tr.Add(xd.Element("td", ((i-1)*3+j+1000).ToString())) |> ignore

let xw = new XmlTextWriter(System.Console.Out)
xw.Formatting <- Formatting.Indented
xd.WriteContentTo(xw)
```

(table part only)
    <table style="text-align:right">
      <tr>
        <th>
        </th>
        <th>X</th>
        <th>Y</th>
        <th>Z</th>
      </tr>
      <tr>
        <th>1</th>
        <td>1001</td>
        <td>1002</td>
        <td>1003</td>
      </tr>
      <tr>
        <th>2</th>
        <td>1004</td>
        <td>1005</td>
        <td>1006</td>
      </tr>
      <tr>
        <th>3</th>
        <td>1007</td>
        <td>1008</td>
        <td>1009</td>
      </tr>
    </table>

## Forth

Printing out a group of HTML text strings is not difficult for Forth or most computer languages.
Another way to do this in Forth is to extend Forth with new "words" that output the HTML tags.
The HTML words will output HTML code when they are interpreted by Forth. In this example the extended Forth code looks at lot like HTML. :-)  The biggest difference is that Forth requires 1 space minimum between each tag.

Although this example is not a complete HTML interpreter, with these few extensions we can demonstrate mixing HTML tags with Forth code to generate the specified random numbers in-line with HTML. We could even use Forth to compile tags together and make HTML generating sub-routines like NETREXX does.


```Forth
include random.hsf

\ parser routines
:  totag
               [char] < PARSE pad place              \ parse input up to '<' char
                -1 >in +!                            \ move the interpreter pointer back 1 char
                pad count type  ;

: '"'        [char] " emit ;
: '"..'      '"'  space ;                            \ output a quote char with trailing space

: toquote                                            \ parse input to " then print as quoted text
              '"' [char] " PARSE pad place
              pad count type '"..' ;

: >          [char] > emit space  ;                  \ output the '>' with trailing space

\ Create some HTML extensions to the Forth interpreter
: <table>         ." <table>" cr ;          : </table>   ." </table>"  cr ;
: <table          ." <table " ;
: style="         ." style="  toquote ;
: align="         ." align="  toquote ;
: border="        ." border=" toquote ;
: width="         ." width="  toquote ;
: cellspacing="   ." cellspacing="  toquote ;
: colspacing="    ." colspacing="   toquote ;

: <tr>       ." <tr>" cr ;                  : </tr>      ." </tr>"  cr ;
: <td>       ." <td> "  totag  ;            : </td>      ." </td>"  cr ;
: <td        ." <td " ;
: <thead>    ." <thead>" ;                  : </thead>   ." </thead>" ;
: <th>       ." <th>" ;                     : </th>      ." </th>"  cr ;
: <th        ." <th "  ;
: <tbody     ." <tbody " ;                  : </tbody>   ." </tbody> " ;
: <caption>  cr ." <caption>"  totag  ;     : </caption> ." </caption>"  cr ;

\ Write the source code that generates HTML in our EXTENDED FORTH
cr
<table border=" 1" width=" 30%" >
<caption> This table was created with FORTH HTML tags</caption>
<tr>
<th align=" right" >       </th>
<th align=" right" > ." A" </th>
<th align=" right" > ." B" </th>
<th align=" right" > ." C" </th>
</tr>
<tr>
<th align=" right" >        1 . </th>
<td align=" right" > 1000 RND . </td>
<td align=" right" > 1000 RND . </td>
<td align=" right" > 1000 RND . </td>
</tr>
<tr>
<th align=" right" >        2 . </th>
<td align=" right" > 1000 RND . </td>
<td align=" right" > 1000 RND . </td>
<td align=" right" > 1000 RND . </td>
</tr>
<tr>
<th align=" right" >        3 . </th>
<td align=" right" > 1000 RND . </td>
<td align=" right" > 1000 RND . </td>
<td align=" right" > 1000 RND . </td>
</tr>
</table>

```

```txt

<table border="1" width="30%" >
<caption>This table was created with FORTH HTML tags</caption>
<tr>
<th align="right" > </th>
<th align="right" > A</th>
<th align="right" > B</th>
<th align="right" > C</th>
</tr>
<tr>
<th align="right" > 1 </th>
<td align="right" > 338 </td>
<td align="right" > 102 </td>
<td align="right" > 113 </td>
</tr>
<tr>
<th align="right" > 2 </th>
<td align="right" > 430 </td>
<td align="right" > 319 </td>
<td align="right" > 731 </td>
</tr>
<tr>
<th align="right" > 3 </th>
<td align="right" > 333 </td>
<td align="right" > 592 </td>
<td align="right" > 83 </td>
</tr>
</table>
```


Result
<table border="1" width="30%" >
<caption>This table was created with FORTH HTML tags</caption>
<tr>
<th align="right" > </th>
<th align="right" > A</th>
<th align="right" > B</th>
<th align="right" > C</th>
</tr>
<tr>
<th align="right" > 1 </th>
<td align="right" > 338 </td>
<td align="right" > 102 </td>
<td align="right" > 113 </td>
</tr>
<tr>
<th align="right" > 2 </th>
<td align="right" > 430 </td>
<td align="right" > 319 </td>
<td align="right" > 731 </td>
</tr>
<tr>
<th align="right" > 3 </th>
<td align="right" > 333 </td>
<td align="right" > 592 </td>
<td align="right" > 83 </td>
</tr>
</table>


## Fortran


### Origin

The task could be achieved by a series of direct WRITE statements, with some simple loop to generate N lines for the table.
This would not show anything beyond the usage of WRITE statements, possibly obfuscated by cunning FORMAT sequences.
Instead, what follows is a trimmed version of some routines that were used for writing HTML tables of various shapes, with perhaps dozens of columns and hundreds of rows.
The idea was that module HTMLSTUFF would contain an attempt at organised assistance for the task.
The system would produce output tables according to the type of the output file: space-aligned or, if .csv then comma-separated, or if .html then it used the HTML table protocol.
In-line code was used for the details of the body of the tables because their cells involved different types and layout (and even decoration, as with RED ... DER for negative correlations) but here the task requires only HTML and that each row show only integers so it seemed worthwhile to devise a subroutine for writing a row of integers and likewise for a row of texts. Thus, the caller need not worry over the multitude of details involved in preparing a well-formed HTML file, just invoke some subroutines with a few parameters. This is done in the mainline.
The specification is for columns headed "X", "Y", and "Z" but a proper table would have more useful annotations.

These routines were written to wallow in a much larger context.
For instance, each subprogram would start with a call to SUBIN naming the subprogram, and just before exit each would invoke SUBOUT.
A stack of active names was thereby maintained, and was available for use by subroutine ECART (to produce trace output, annotated via the name stack with the name of the routine requesting that output), and subroutine STATE that accepted a text declaring the activity about to be started (surprisingly useful when checking and documenting the source), and if there was a disaster, subroutine CROAK reported its message and what was being attempted at each level back to the start. Similarly, (almost) all output to the screen was echoed to file TRAIL and after every 6666 lines written to the screen, there was an opportunity to stop a runaway output, and details for the I/O unit variables and associated arrays were in named COMMON via file cIOunits.for. Most of this has been commented out.
There was also an attempt at dealing with annoying differences in glyphs between keyboard input, screen display, and the display rendered by text editors.
This had to be extended for HTML output as certain character codes also evoked an unsuitable glyph and others interfered with the interpretation of text versus instructions.
The first part of the source therefore is a wad of support routines, starting with a text <code>I AM</code> (see the painting by Colin McCahon) that names the programme, and another that is filled with the user's code identity.
These are used in the metadata for the HTML.

Since the HTML text that results was going to be inspected by eye to catch blunders there was a lot of attention to layout, in particular the use of indenting as each level was tracked by INDEEP, and the content of a row was to appear on one (possibly long) line.
Browsers often pass over hiccoughs and still present a correct-looking display, thus it is useful to employ HTML-checking routines.
I have considered adding various checks to the routines in HTMLSTUFF such as that when a level was closed, its interior levels had also been closed off, but that would mean even more cogitation.
Avoiding endless text shuffling was another objective, thus rather than concatenate pieces and write the result, routine HTML3 has three parts, a preamble, a body, and a tail to generate a complete line.
Further, for layout assistance, if the preamble was not empty then it would be written on a new line and indented according to INDEEP, and if the tail was not empty then it would be written and the line ended.
It is tempting to prepare a similar routine for presenting a single item, say <code>HTML3TERM("&lt;th&gt;",TEXT(I)(1:L),"</th>")</code> rather than preparing a compound string via <code>"&lt;th&gt;"//TEXT(I)(1:L)//"</th>"</code> first. Even more direct would be to use WRITE statements straight to the output file, employing suitable FORMAT statements, but in the larger context it was more useful to avoid this since with all output in one place via subroutine WRITE, mishaps could be attended to in one place only.


### Source


```Fortran

      MODULE PARAMETERS	!Assorted oddities that assorted routines pick and choose from.
       CHARACTER*5 I AM		!Assuage finicky compilers.
       PARAMETER (IAM = "Gnash")	!I AM!
       INTEGER		LUSERCODE	!One day, I'll get around to devising some string protocol.
       CHARACTER*28	USERCODE		!I'm not too sure how long this can be.
       DATA		USERCODE,LUSERCODE/"",0/!Especially before I have a text.
      END MODULE PARAMETERS

      MODULE ASSISTANCE
      CONTAINS	!Assorted routines that seem to be of general use but don't seem worth isolating..
      Subroutine Croak(Gasp)	!A dying message, when horror is suddenly encountered.
Casts out some final words and STOP, relying on the SubInOut stuff to have been used.
Cut down from the full version of April MMI, that employed the SubIN and SubOUT protocol..
       Character*(*) Gasp	!The last gasp.
       COMMON KBD,MSG
       WRITE (MSG,1) GASP
    1  FORMAT ("Oh dear! ",A)
       STOP "I STOP now. Farewell..."	!Whatever pit I was in, I'm gone.
      End Subroutine Croak	!That's it.

       INTEGER FUNCTION LSTNB(TEXT)  !Sigh. Last Not Blank.
Concocted yet again by R.N.McLean (whom God preserve) December MM.
Code checking reveals that the Compaq compiler generates a copy of the string and then finds the length of that when using the latter-day intrinsic LEN_TRIM. Madness!
Can't   DO WHILE (L.GT.0 .AND. TEXT(L:L).LE.' ')	!Control chars. regarded as spaces.
Curse the morons who think it good that the compiler MIGHT evaluate logical expressions fully.
Crude GO TO rather than a DO-loop, because compilers use a loop counter as well as updating the index variable.
Comparison runs of GNASH showed a saving of ~3% in its mass-data reading through the avoidance of DO in LSTNB alone.
Crappy code for character comparison of varying lengths is avoided by using ICHAR which is for single characters only.
Checking the indexing of CHARACTER variables for bounds evoked astounding stupidities, such as calculating the length of TEXT(L:L) by subtracting L from L!
Comparison runs of GNASH showed a saving of ~25-30% in its mass data scanning for this, involving all its two-dozen or so single-character comparisons, not just in LSTNB.
        CHARACTER*(*),INTENT(IN):: TEXT	!The bumf. If there must be copy-in, at least there need not be copy back.
        INTEGER L		!The length of the bumf.
         L = LEN(TEXT)		!So, what is it?
    1    IF (L.LE.0) GO TO 2	!Are we there yet?
         IF (ICHAR(TEXT(L:L)).GT.ICHAR(" ")) GO TO 2	!Control chars are regarded as spaces also.
         L = L - 1		!Step back one.
         GO TO 1		!And try again.
    2    LSTNB = L		!The last non-blank, possibly zero.
        RETURN			!Unsafe to use LSTNB as a variable.
       END FUNCTION LSTNB	!Compilers can bungle it.
       CHARACTER*2 FUNCTION I2FMT(N)	!These are all the same.
        INTEGER*4 N			!But, the compiler doesn't offer generalisations.
         IF (N.LT.0) THEN	!Negative numbers cop a sign.
           IF (N.LT.-9) THEN	!But there's not much room left.
             I2FMT = "-!"	!So this means 'overflow'.
            ELSE			!Otherwise, room for one negative digit.
             I2FMT = "-"//CHAR(ICHAR("0") - N)	!Thus. Presume adjacent character codes, etc.
           END IF		!So much for negative numbers.
         ELSE IF (N.LT.10) THEN	!Single digit positive?
           I2FMT = " " //CHAR(ICHAR("0") + N)	!Yes. This.
         ELSE IF (N.LT.100) THEN	!Two digit positive?
           I2FMT = CHAR(N/10      + ICHAR("0"))	!Yes.
     1            //CHAR(MOD(N,10) + ICHAR("0")) !These.
         ELSE			!Otherwise,
           I2FMT = "+!" 	!Positive overflow.
         END IF			!So much for that.
       END FUNCTION I2FMT	!No WRITE and FORMAT unlimbering.
       CHARACTER*8 FUNCTION I8FMT(N)	!Oh for proper strings.
        INTEGER*4 N
        CHARACTER*8 HIC
         WRITE (HIC,1) N
    1    FORMAT (I8)
         I8FMT = HIC
       END FUNCTION I8FMT
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
         CHARACTER*42	EXPLICATION	!An explanation. Will it be the answer?
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
      END MODULE ASSISTANCE

      MODULE LOGORRHOEA
       CONTAINS
        SUBROUTINE ECART(TEXT)	!Produces trace output with many auxiliary details.
         CHARACTER*(*) TEXT	!The text to be annotated.
         COMMON KBD,MSG		!I/O units.
          WRITE (MSG,1) TEXT	!Just roll the text.
    1     FORMAT ("Trace: ",A)	!Lacks the names of the invoking routine, and that which invoked it.
        END SUBROUTINE ECART
       SUBROUTINE WRITE(OUT,TEXT,ON)	!We get here in the end. Cast forth some pearls.
C   Once upon a time, there was just confusion between ASCII and EBCDIC character codes and glyphs,
c after many variant collections caused annoyance. Now I see that modern computing has introduced
c many new variations, so that one text editor may display glyphs differing from those displayed
c by another editor and also different from those displayed when a programme writes to the screen
c in "teletype" mode, which is to say, employing the character/glyph combination of the moment.
c And in particular, decimal points and degree symbols differ and annoyance has grown.
c   So, on re-arranging SAY to not send output to multiple distinations depending on the value of OUT,
c except for the special output to MSG that is echoed to TRAIL, it became less messy to make an assault
c on the text that goes to MSG, but after it was sent to TRAIL. I would have preferred to fiddle the
c "code page" for text output that determines what glyph to show for which code, but not only
c is it unclear how to do this, even if facilities were available, I suspect that the screen display
c software only loads the mysterious code page during startup.
c   This fiddling means that any write to MSG should be done last, and writes of text literals
c should not include characters that will be fiddled, as text literals may be protected against change.
C   Somewhere along the way, the cent character (¢) has disappeared. Perhaps it will return in "unicode".
        USE ASSISTANCE		!But might still have difficulty.
        INTEGER OUT		!The destination.
        CHARACTER*(*) TEXT	!The message. Possibly damaged. Any trailing spaces will be sent forth.
        LOGICAL ON		!Whether to terminate the line... TRUE sez that someone will be carrying on.
        INTEGER IOSTAT		!Furrytran gibberish.
c        INCLUDE "cIOUnits.for"	!I/O unit numbers.
        COMMON KBD,MSG
c        INTEGER*2,SAVE:: I Be	!Self-identification.
c         CALL SUBIN("Write",I Be)	!Hullo!
         IF (OUT.LE.0) GO TO 999	!Goodbye?
c         IF (IOGOOD(OUT)) THEN	!Is this one in good heart?
c           IF (IOCOUNT(OUT).LE.0 .AND. OUT.NE.MSG) THEN	!Is it attached to a file?
c             IF (IONAME(OUT).EQ."") IONAME(OUT) = "Anome"	!"No name".
c     1         //I2FMT(OUT)//".txt"	!Clutch at straws.
c             IF (.NOT.OPEN(OUT,IONAME(OUT),"REPLACE","WRITE")) THEN	!Just in time?
c               IOGOOD(OUT) = .FALSE.	!No! Strangle further usage.
c               GO TO 999		!Can't write, so give up!
c             END IF			!It might be better to hit the WRITE and fail.
c           END IF		!We should be ready now.
c           IF (OUT.EQ.MSG .AND. SCRAGTEXTOUT) CALL SCRAG(TEXT)	!Output to the screen is recoded for the screen.
           IF (ON) THEN		!Now for the actual output at last. This is annoying.
             WRITE (OUT,1,ERR = 666,IOSTAT = IOSTAT) TEXT	!Splurt.
    1        FORMAT (A,$)	!Don't move on to a new line. (The "$"! Is it not obvious?)
c             IOPART(OUT) = IOPART(OUT) + 1	!Thus count a part-line in case someone fusses.
            ELSE		!But mostly, write and advance.
             WRITE (OUT,2,ERR = 666,IOSTAT = IOSTAT) TEXT	!Splurt.
    2        FORMAT (A)		!*-style "free" format chops at 80 or some such.
           END IF		!So much for last-moment dithering.
c           IOCOUNT(OUT) = IOCOUNT(OUT) + 1	!Count another write (whole or part) so as to be not zero..
c         END IF			!So much for active passages.
c  999    CALL SUBOUT("Write")	!I am closing.
  999   RETURN			!Done.
Confusions.
  666    IF (OUT.NE.MSG) CALL CROAK("Can't write to unit "//I2FMT(OUT)	!Why not?
c     1    //" (file "//IONAME(OUT)(1:LSTNB(IONAME(OUT)))	!Possibly, no more disc space! In which case, this may fail also!
     2    //") message "//ERRORWORDS(IOSTAT)			!Hopefully, helpful.
     3    //" length "//I8FMT(LEN(TEXT))//", this: "//TEXT)	!The instigation.
        STOP "Constipation!"	!Just so.
       END SUBROUTINE WRITE	!The moving hand having writ, moves on.

       SUBROUTINE SAY(OUT,TEXT)	!And maybe a copy to the trail file as well.
        USE PARAMETERS		!Odds and ends.
        USE ASSISTANCE		!Just a little.
        INTEGER OUT		!The orifice.
        CHARACTER*(*) TEXT	!The blather. Can be modified if to MSG and certain characters are found.
        CHARACTER*120 IS	!For a snatched question.
        INTEGER L		!A finger.
c        INCLUDE "cIOUnits.for"	!I/O unit numbers.
        COMMON KBD,MSG
c        INTEGER*2,SAVE:: I Be	!Self-identification.
c         CALL SUBIN("Say",I Be)	!Me do be Me, I say!
Chop off trailing spaces.
         L = LEN(TEXT)		!What I say may be rather brief.
    1    IF (L.GT.0) THEN	!So, is there a last character to look at?
           IF (ICHAR(TEXT(L:L)).LE.ICHAR(" ")) THEN	!Yes. Is it boring?
             L = L - 1			!Yes! Trim it!
             GO TO 1			!And check afresh.
           END IF		!A DO-loop has overhead with its iteration count as well.
         END IF			!Function LEN_TRIM copies the text first!!
Contemplate the disposition of TEXT(1:L)
c         IF (OUT.NE.MSG) THEN	!Normal stuff?
           CALL WRITE(OUT,TEXT(1:L),.FALSE.)	!Roll.
c          ELSE			!Echo what goes to MSG to the TRAIL file.
c           CALL WRITE(TRAIL,TEXT(1:L),.FALSE.)	!Thus.
c           CALL WRITE(  MSG,TEXT(1:L),.FALSE.)	!Splot to the screen.
c           IF (.NOT.BLABBERMOUTH) THEN		!Do we know restraint?
c             IF (IOCOUNT(MSG).GT.BURP) THEN	!Yes. Consider it.
c               WRITE (MSG,100) IOCOUNT(MSG)	!Alas, triggered. So remark on quantity,
c  100          FORMAT (//I9," lines! Your spirit might flag."	!Hint. (Not copied to the TRAIL file)
c     1          /," Type quit to set GIVEOVER to TRUE, with hope for "
c     2           ,"a speedy palliation,",
c     3          /,"   or QUIT to abandon everything, here, now",
c     4          /,"   or blabber to abandon further restraint,",
c     5          /,"   or anything else to carry on:")
c               IS = REPLY("QUIT, quit, blabber or continue")	!And ask.
c               IF (IS.EQ."QUIT") CALL CROAK("Enough of this!")	!No UPDATE, nothing.
c               CALL UPCASE(IS)		!Now we're past the nice distinction, simplify.
c               IF (IS.EQ."QUIT") GIVEOVER = .TRUE.	!Signal to those who listen.
c               IF (IS.EQ."BLABBER") BLABBERMOUTH = .TRUE.	!Well?
c               IF (GIVEOVER) WRITE (MSG,101)			!Announce hope.
c  101          FORMAT ("Let's hope that the babbler notices...")	!Like, IF (GIVEOVER) GO TO ...
c               IF (.NOT.GIVEOVER) WRITE (MSG,102)	!Alternatively, firm resolve.
c  102          FORMAT("Onwards with renewed vigour!")	!Fight the good fight.
c               BURP = IOCOUNT(MSG) + ENOUGH	!The next pause to come.
c             END IF			!So much for last-moment restraint.
c           END IF			!So much for restraint.
c         END IF			!So much for selection.
c         CALL SUBOUT("Say")	!I am merely the messenger.
       END SUBROUTINE SAY	!Enough said.
       SUBROUTINE SAYON(OUT,TEXT)	!Roll to the screen and to the trail file as well.
C This differs by not ending the line so that further output can be appended to it.
        USE ASSISTANCE
        INTEGER OUT		!The orifice.
        CHARACTER*(*) TEXT	!The blather.
        INTEGER L		!A finger.
c        INCLUDE "cIOUnits.for"	!I/O unit numbers.
        COMMON KBD,MSG
c        INTEGER*2,SAVE:: I Be	!Self-identification.
c         CALL SUBIN("SayOn",I Be)	!Me do be another. Me, I say on!
         L = LEN(TEXT)			!How much say I on?
    1    IF (L.GT.0) THEN		!I say on anything?
           IF (ICHAR(TEXT(L:L)).LE.ICHAR(" ")) THEN	!I end it spaceish?
             L = L - 1				!Yes. Trim such.
             GO TO 1				!And look afresh.
           END IF			!So much for trailing off.
         END IF			!Continue with L fingering the last non-blank.
c         IF (OUT.EQ.MSG) CALL WRITE(TRAIL,TEXT(1:L),.TRUE.)	!Writes to the screen go also to the TRAIL.
                         CALL WRITE(  OUT,TEXT(1:L),.TRUE.)	!It is said, and more is expected.
c         CALL SUBOUT("SayOn")	!I am merely the messenger.
       END SUBROUTINE SAYON	!And further messages impend.

      END MODULE LOGORRHOEA

      MODULE HTMLSTUFF	!Assists with the production of decorated output.
Can't say I think much of the scheme. How about <+blah> ... <-blah> rather than the assymetric <blah> ... </blah>?
Cack-handed comment format as well...
       USE PARAMETERS	!To ascertain who I AM.
       USE ASSISTANCE	!To get at LSTNB.
       USE LOGORRHOEA	!To get at SAYON and SAY.
       INTEGER INDEEP,HOLE	!I keep track of some details.
       PRIVATE INDEEP,HOLE	!Amongst myselves.
       DATA INDEEP,HOLE/0,0/	!Initially, I'm not doing anything.
Choose amongst output formats.
       INTEGER LASTFILETYPENAME		!Certain file types are recognised.
       PARAMETER (LASTFILETYPENAME = 2)	!Thus, three options.
       INTEGER OUTTYPE,OUTTXT,OUTCSV,OUTHTML	!The recognition.
       CHARACTER*5 OUTSTYLE,FILETYPENAME(0:LASTFILETYPENAME)	!Via the tail end of a file name.
       PARAMETER (FILETYPENAME = (/".txt",".CSV",".HTML"/))	!Thusly. Note that WHATFILETYPE will not recognise ".txt" directly.
       PARAMETER (OUTTXT = 0,OUTCSV = 1,OUTHTML = 2)	!Mnemonics.
       DATA OUTSTYLE/""/	!So OUTTYPE = OUTTXT. But if an output file is specified, its file type will be inspected.
       TYPE HTMLMNEMONIC	!I might as well get systematic, as these are global names.
        CHARACTER* 9 COMMAH		!This looks like a comma
        CHARACTER* 9 COMMAD		!And in another context, so does this.
        CHARACTER* 6 SPACE		!Some spaces are to be atomic.
        CHARACTER*18 RED		!Decoration and
        CHARACTER* 7 DER		!noitaroceD.
       END TYPE HTMLMNEMONIC	!That's enough for now.
       TYPE(HTMLMNEMONIC) HTMLA	!I'll have one set, please.
       PARAMETER (HTMLA = HTMLMNEMONIC(	!With these values.
     1  "</th><th>",			!But .html has its variants. For a heading.
     2  "</td><td>",			!For a table datum.
     3  " ",			!A space that is not to be split.
     4  '<font color="red">',		!Dabble in decoration.
     5  '</font>'))			!Grrrr. A font is for baptismal water.
      CONTAINS	!Mysterious assistants.
       SUBROUTINE HTML(TEXT)	!Rolls some text, with suitable indentation.
        CHARACTER*(*) TEXT	!The text.
c        INCLUDE "cIOUnits.for"	!I/O unit numbers.
         IF (LEN(TEXT).LE.0) RETURN	!Possibly boring.
         IF (INDEEP.GT.0) THEN		!Some indenting desired?
           CALL WRITE(HOLE,REPEAT(" ",INDEEP),.TRUE.)	!Yep. SAYON trims trailing spaces.
c           IF (HOLE.EQ.MSG) CALL WRITE(TRAIL,REPEAT(" ",INDEEP),.TRUE.)	!So I must copy.
         END IF			!Enough indenting.
         CALL SAY(HOLE,TEXT)	!Say the piece and end the line.
       END SUBROUTINE HTML	!Maintain stacks? Check entry/exit matching?

       SUBROUTINE HTML3(HEAD,BUMF,TAIL)	!Rolls some text, with suitable indentation.
Checks the BUMF for decimal points only. HTMLALINE handles text to HTML for troublesome characters, replacing them with special names for the desired glyph.
Confusion might arise, if & is in BUMF and is not to be converted. "&amp;" vs "&so on"; similar worries with < and >.
        CHARACTER*(*) HEAD	!If not "", the start of the line, with indentation supplied.
        CHARACTER*(*) BUMF	!The main body of the text.
        CHARACTER*(*) TAIL	!If not "", this is for the end of the line.
        INTEGER LB,L1,L2	!A length and some fingers for scanning.
        CHARACTER*1 MUMBLE	!These symbols may not be presented properly.
        CHARACTER*8 MUTTER	!But these encodements may be interpreted as desired.
        PARAMETER (MUMBLE = "·")	!I want certain glyphs, but encodement varies.
        PARAMETER (MUTTER = "&middot;")	!As does recognition.
c        INCLUDE "cIOUnits.for"	!I/O unit numbers.
        COMMON KBD,MSG
Commence with a new line?
         IF (HEAD.NE."") THEN	!Is a line to be started? (Spaces are equivalent to "" as well)
           IF (INDEEP.GT.0) THEN	!Some indentation is good.
             CALL WRITE(HOLE,REPEAT(" ",INDEEP),.TRUE.)	!Yep. SAYON trims trailing spaces.
c             IF (HOLE.EQ.MSG) CALL WRITE(TRAIL,	!So I must copy for the log.
c     1        REPEAT(" ",INDEEP),.TRUE.)	!Hopefully, not generated a second time.
            ELSE			!The accountancy may be bungled.
             CALL ECART("HTML huh? InDeep="//I8FMT(INDEEP))	!So, complain.
           END IF			!Also, REPEAT has misbehaved.
           CALL SAYON(HOLE,HEAD)	!Thus a suitable indentation.
         END IF			!So much for a starter.
Cast forth the bumf. Any trailing spaces will be dropped by SAYON.
         LB = LEN(BUMF)		!How much bumf? Trailing spaces will be rolled.
         L1 = 1			!Waiting to be sent.
         L2 = 0			!Syncopation.
    1    L2 = L2 + 1		!Advance to the next character to be inspected..
         IF (L2.GT.LB) GO TO 2	!Is there another?
         IF (ICHAR(BUMF(L2:L2)).NE.ICHAR(MUMBLE)) GO TO 1	!Yes. Advance through the untroublesome.
         IF (L1.LT.L2) THEN		!A hit. Have any untroubled ones been passed?
           CALL WRITE(HOLE,BUMF(L1:L2 - 1),.TRUE.)	!Yes. Send them forth.
c           IF (HOLE.EQ.MSG) CALL WRITE(TRAIL,BUMF(L1:L2 - 1),.TRUE.)	!With any trailing spaces included.
         END IF				!Now to do something in place of BUMF(L2)
         L1 = L2 + 1			!Moving the marker past it, like.
         CALL SAYON(HOLE,MUTTER)	!The replacement for BUMF(L2 as was).
         GO TO 1		!Continue scanning.
    2    IF (L2.GT.L1) THEN	!Any tail end, but not ending the output line.
           CALL WRITE(HOLE,BUMF(L1:L2 - 1),.TRUE.)	!Yes. Away it goes.
c           IF (HOLE.EQ.MSG) CALL WRITE(TRAIL,BUMF(L1:L2 - 1),.TRUE.)	!And logged.
         END IF				!So much for the bumf.
Consider ending the line.
    3    IF (TAIL.NE."") CALL SAY(HOLE,TAIL)	!Enough!
       END SUBROUTINE HTML3	!Maintain stacks? Check entry/exit matching?

       SUBROUTINE HTMLSTART(OUT,TITLE,DESC)	!Roll forth some gibberish.
        INTEGER OUT		!The mouthpiece, mentioned once only at the start, and remembered for future use.
        CHARACTER*(*) TITLE	!This should be brief.
        CHARACTER*(*) DESC	!This a little less brief.
        CHARACTER*(*) METAH	!Some repetition.
        PARAMETER (METAH = '<Meta Name="')	!The syntax is dubious.
        CHARACTER*8 D		!YYYYMMDD
        CHARACTER*10 T		!HHMMSS.FFF
         HOLE = OUT		!Keep a local copy to save on parameters.
         INDEEP = 0		!We start.
         CALL HTML('<!DOCTYPE HTML PUBLIC "'	!Before we begin, we wave hands.
     1    //'-//W3C//DTD HTML 4.01 Transitional//EN"'	!Otherwise "nowrap" is objected to, as in http://validator.w3.org/check
     2    //' "http://www.w3.org/TR/html4/loose.dtd">')	!Endless blather.
         CALL HTML('<HTML lang="en-NZ">')	!  H E R E   W E   G O !
         INDEEP = 1				!Its content.
         CALL HTML("<Head>")			!And the first decoration begins.
         INDEEP = 2				!Its content.
         CALL HTML("<Title>"//I AM//" "	!This appears in the web page tag.
     1    // TITLE(1:LSTNB(TITLE)) //"</Title>")!So it should be short.
         CALL HTML('<Meta http-equiv="Content-Type"'	!Crazed gibberish.
     1    //' content="text/html; charset=utf-8">') 		!But said to be worthy.
         CALL HTML(METAH//'Description" Content="'//DESC//'">')	!Hopefully, helpful.
         CALL HTML(METAH//'Generator"   Content="'//I AM//'">')	!I said.
         CALL DATE_AND_TIME(DATE = D,TIME = T)			!Not assignments, but attachments.
         CALL HTML(METAH//'Created"     Content="'		!Convert the timestamp
     1    //D(1:4)//"-"//D(5:6)//"-"//D(7:8)			!Into an international standard.
     2    //" "//T(1:2)//":"//T(3:4)//":"//T(5:10)//'">')	!For date and time.
         IF (LUSERCODE.GT.0) CALL HTML(METAH			!Possibly, the user's code is known.
     1    //'Author"      Content="'//USERCODE(1:LUSERCODE)	!If so, reveal.
     2    //'"> <!-- User code as reported by GetLog.-->')	!Disclaiming responsibility...
         INDEEP = 1		!Finishing the content of the header.
         CALL HTML("</Head>")	!Enough of that.
         CALL HTML("<BODY>")	!A fresh line seems polite.
         INDEEP = 2		!Its content follows..
       END SUBROUTINE HTMLSTART	!Others will follow on. Hopefully, correctly.
       SUBROUTINE HTMLSTOP	!And hopefully, this will be a good closure.
Could be more sophisticated and track the stack via INDEEP+- and names, to enable a desperate close-off if INDEEP is not 2.
         IF (INDEEP.NE.2) CALL ECART("Misclosure! InDeep not 2 but"	!But,
     1    //I8FMT(INDEEP))	!It may not be.
         INDEEP = 1		!Retreat to the first level.
         CALL HTML("</BODY>")	!End the "body".
         INDEEP = 0		!Retreat to the start level.
         CALL HTML("</HTML>")	!End the whole thing.
       END SUBROUTINE HTMLSTOP	!Ah...

       SUBROUTINE HTMLTSTART(B,SUMMARY)	!Start a table.
        INTEGER B		!Border thickness.
        CHARACTER*(*) SUMMARY	!Some well-chosen words.
         CALL HTML("<Table border="//I2FMT(B)	!Just so. Text digits, or, digits in text?
     1    //' summary="'//SUMMARY//'">')	!Not displayed, but potentially used by non-display agencies...
         INDEEP = INDEEP + 1		!Another level dug.
       END SUBROUTINE HTMLTSTART!That part was easy.
       SUBROUTINE HTMLTSTOP	!And the ending is easy too.
         INDEEP = INDEEP - 1		!Withdraw a level.
         CALL HTML("</Table>")		!Hopefully, aligning.
       END SUBROUTINE HTMLTSTOP	!The bounds are easy.

       SUBROUTINE HTMLTHEADSTART	!Start a table's heading.
         CALL HTML("<tHead>")		!Thus.
         INDEEP = INDEEP + 1		!Dig deeper.
       END SUBROUTINE HTMLTHEADSTART	!Content should follow.
       SUBROUTINE HTMLTHEADSTOP		!And now, enough.
         INDEEP = INDEEP - 1		!Retreat a level.
         CALL HTML("</tHead>")		!And end the head.
       END SUBROUTINE HTMLTHEADSTOP	!At the neck of the body?

       SUBROUTINE HTMLTHEAD(N,TEXT)	!Cast forth a whole-span table heading.
        INTEGER N		!The count of columns to be spanned.
        CHARACTER*(*) TEXT	!A brief description to place there.
         CALL HTML3("<tr><th colspan=",I8FMT(N)//' align="center">',"")	!Start the specification.
         CALL HTML3("",TEXT(1:LSTNB(TEXT)),"</th></tr>")	!This text, possibly verbose.
       END SUBROUTINE HTMLTHEAD		!Thus, all contained on one line.

       SUBROUTINE HTMLTBODYSTART	!Start on the table body.
         CALL HTML('<tBody> <!--Profuse "align" usage '	!Simple, but I'm unhappy.
     1    //'for all cells can be factored out to "row" '	!Alas, so far as I can make out.
     2    //'but not to "body"-->')		!And I don't think much of the "comment" formalism, either.
         INDEEP = INDEEP + 1	!Anyway, we're ready with the alignment.
       END SUBROUTINE HTMLTBODYSTART	!Others will provide the body.
       SUBROUTINE HTMLTBODYSTOP	!And, they've had enough.
         INDEEP = INDEEP - 1		!So, up out of the hole.
         CALL HTML("</tBody>")		!Take a breath.
       END SUBROUTINE HTMLTBODYSTOP	!And wander off.
       SUBROUTINE HTMLTROWTEXT(TEXT,N)	!Roll a row of column headings.
        CHARACTER*(*) TEXT(:)	!The headings.
        INTEGER N		!Their number.
        INTEGER I,L		!Assistants.
         CALL HTML3("<tr>","","")	!Start a row of headings-to-come, and don't end the line.
         DO I = 1,N			!Step through the headings.
           L = LSTNB(TEXT(I))		!Trailing spaces are to be ignored.
           IF (L.LE.0) THEN		!Thus discovering blank texts.
             CALL HTML3("","<th> </th>","")	!This prevents the cell being collapsed.
            ELSE				!But for those with text,
             CALL HTML3("","<th>"//TEXT(I)(1:L)//"</th>","")	!Roll it.
           END IF			!So much for that text.
         END DO				!On to the next.
         CALL HTML3("","","</tr>")	!Finish the row, and thus the line.
       END SUBROUTINE HTMLTROWTEXT	!So much for texts.
       SUBROUTINE HTMLTROWINTEGER(V,N)	!Now for all integers.
        INTEGER V(:)	!The integers.
        INTEGER N	!Their number.
        INTEGER I	!A stepper.
         CALL HTML3('<tr align="right">',"","")	!Start a row of entries.
         DO I = 1,N			!Work through the row's values.
           CALL HTML3("","<td>"//I8FMT(V(I))//"</td>","")	!One by one.
         END DO				!On to the next.
         CALL HTML3("","","</tr>")	!Finish the row, and thus the line.
       END SUBROUTINE HTMLTROWINTEGER	!All the same type is not troublesome.
      END MODULE HTMLSTUFF	!Enough already.

      PROGRAM MAKETABLE
      USE PARAMETERS
      USE ASSISTANCE
      USE HTMLSTUFF
      INTEGER KBD,MSG
      INTEGER NCOLS		!The usage of V must conform to this!
      PARAMETER (NCOLS = 4)	!Specified number of columns.
      CHARACTER*3 COLNAME(NCOLS)	!And they have names.
      PARAMETER (COLNAME = (/"","X","Y","Z"/))	!As specified.
      INTEGER V(NCOLS)		!A scratchpad for a line's worth.
      COMMON KBD,MSG		!I/O units.
      KBD = 5			!Keyboard.
      MSG = 6			!Screen.
      CALL GETLOG(USERCODE)		!Who has poked me into life?
      LUSERCODE = LSTNB(USERCODE)	!Ah, text gnashing.

      CALL HTMLSTART(MSG,"Powers","Table of integer powers")		!Output to the screen will do.
       CALL HTMLTSTART(1,"Successive powers of successive integers")	!Start the table.
        CALL HTMLTHEADSTART						!The table heading.
         CALL HTMLTHEAD(NCOLS,"Successive powers")			!A full-width heading.
         CALL HTMLTROWTEXT(COLNAME,NCOLS)				!Headings for each column.
        CALL HTMLTHEADSTOP						!So much for the heading.
        CALL HTMLTBODYSTART						!Now for the content.
        DO I = 1,10		!This should be enough.
          V(1) = I		!The unheaded row number.
          V(2) = I**2		!Its square.
          V(3) = I**3		!Cube.
          V(4) = I**4		!Fourth power.
          CALL HTMLTROWINTEGER(V,NCOLS)					!Show a row's worth..
        END DO			!On to the next line.
        CALL HTMLTBODYSTOP						!No more content.
       CALL HTMLTSTOP							!End the table.
      CALL HTMLSTOP
      END

```


### Output

As yet another demonstration of the vagaries of text interpretation, notice that the blank column heading, carefully converted to nbsp; (no leading ampersand here!) does ''not'' appear when the page is displayed, despite the "pre-formatted" hint.

```txt

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<HTML lang="en-NZ">
 <Head>
  <Title>Gnash Powers</Title>
  <Meta http-equiv="Content-Type" content="text/html; charset=utf-8">
  <Meta Name="Description" Content="Table of integer powers">
  <Meta Name="Generator"   Content="Gnash">
  <Meta Name="Created"     Content="2016-02-09 01:07:48.359">
  <Meta Name="Author"      Content="Nicky"> <!-- User code as reported by GetLog.-->
 </Head>
 <BODY>
  <Table border= 1 summary="Successive powers of successive integers">
   <tHead>
    <tr><th colspan=       4 align="center">Successive powers</th></tr>
    <tr><th> </th><th>X</th><th>Y</th><th>Z</th></tr>
   </tHead>
   <tBody> <!--Profuse "align" usage for all cells can be factored out to "row" but not to "body"-->
    <tr align="right"><td>       1</td><td>       1</td><td>       1</td><td>       1</td></tr>
    <tr align="right"><td>       2</td><td>       4</td><td>       8</td><td>      16</td></tr>
    <tr align="right"><td>       3</td><td>       9</td><td>      27</td><td>      81</td></tr>
    <tr align="right"><td>       4</td><td>      16</td><td>      64</td><td>     256</td></tr>
    <tr align="right"><td>       5</td><td>      25</td><td>     125</td><td>     625</td></tr>
    <tr align="right"><td>       6</td><td>      36</td><td>     216</td><td>    1296</td></tr>
    <tr align="right"><td>       7</td><td>      49</td><td>     343</td><td>    2401</td></tr>
    <tr align="right"><td>       8</td><td>      64</td><td>     512</td><td>    4096</td></tr>
    <tr align="right"><td>       9</td><td>      81</td><td>     729</td><td>    6561</td></tr>
    <tr align="right"><td>      10</td><td>     100</td><td>    1000</td><td>   10000</td></tr>
   </tBody>
  </Table>
 </BODY>
</HTML>

```


### =And without the HTML containment=

  <Table border= 1 summary="Successive powers of successive integers">
   <tHead>
    <tr><th colspan=       4 align="center">Successive powers</th></tr>
    <tr><th> </th><th>X</th><th>Y</th><th>Z</th></tr>
   </tHead>
   <tBody> <!--Profuse "align" usage for all cells can be factored out to "row" but not to "body"-->
    <tr align="right"><td>       1</td><td>       1</td><td>       1</td><td>       1</td></tr>
    <tr align="right"><td>       2</td><td>       4</td><td>       8</td><td>      16</td></tr>
    <tr align="right"><td>       3</td><td>       9</td><td>      27</td><td>      81</td></tr>
    <tr align="right"><td>       4</td><td>      16</td><td>      64</td><td>     256</td></tr>
    <tr align="right"><td>       5</td><td>      25</td><td>     125</td><td>     625</td></tr>
    <tr align="right"><td>       6</td><td>      36</td><td>     216</td><td>    1296</td></tr>
    <tr align="right"><td>       7</td><td>      49</td><td>     343</td><td>    2401</td></tr>
    <tr align="right"><td>       8</td><td>      64</td><td>     512</td><td>    4096</td></tr>
    <tr align="right"><td>       9</td><td>      81</td><td>     729</td><td>    6561</td></tr>
    <tr align="right"><td>      10</td><td>     100</td><td>    1000</td><td>   10000</td></tr>
   </tBody>
  </Table>


## Go

html/template is a package in the standard library.

```go
package main

import (
    "fmt"
    "html/template"
    "os"
)

type row struct {
    X, Y, Z int
}

var tmpl = `<table>
    <tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>
{{range $ix, $row := .}}    <tr><td>{{$ix}}</td>
        <td>{{$row.X}}</td>
        <td>{{$row.Y}}</td>
        <td>{{$row.Z}}</td></tr>
{{end}}</table>
`

func main() {
    // create template
    ct := template.Must(template.New("").Parse(tmpl))

    // make up data
    data := make([]row, 4)
    for r := range data {
        data[r] = row{r*3, r*3+1, r*3+2}
    }

    // apply template to data
    if err := ct.Execute(os.Stdout, data); err != nil {
        fmt.Println(err)
    }
}
```

```html5><table

    <tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>
    <tr><td>0</td><td>0</td><td>1</td><td>2</td></tr>
    <tr><td>1</td><td>3</td><td>4</td><td>5</td></tr>
    <tr><td>2</td><td>6</td><td>7</td><td>8</td></tr>
    <tr><td>3</td><td>9</td><td>10</td><td>11</td></tr>
</table>
```



## Groovy


```groovy
import groovy.xml.MarkupBuilder

def createTable(columns, rowCount) {
    def writer = new StringWriter()
    new MarkupBuilder(writer).table(style: 'border:1px solid;text-align:center;') {
        tr {
            th()
            columns.each { title -> th(title)}
        }
        (1..rowCount).each { row ->
            tr {
                td(row)
                columns.each { td((Math.random() * 9999) as int ) }
            }
        }
    }
    writer.toString()
}

println createTable(['X', 'Y', 'Z'], 3)
```

<table style='border:1px solid;text-align:center;'>
  <tr>
    <th />
    <th>X</th>
    <th>Y</th>
    <th>Z</th>
  </tr>
  <tr>
    <td>1</td>
    <td>6106</td>
    <td>9898</td>
    <td>1584</td>
  </tr>
  <tr>
    <td>2</td>
    <td>1641</td>
    <td>9387</td>
    <td>3858</td>
  </tr>
  <tr>
    <td>3</td>
    <td>8970</td>
    <td>4843</td>
    <td>681</td>
  </tr>
</table>


## Haskell

```haskell
import Data.List (unfoldr)
import Control.Monad (forM_)

import qualified Text.Blaze.Html5 as B
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

import System.Random (RandomGen, getStdGen, randomRs, split)

makeTable
  :: RandomGen g
  => [String] -> Int -> g -> B.Html
makeTable headings nRows gen =
  B.table $
  do B.thead $ B.tr $ forM_ (B.toHtml <$> headings) B.th
     B.tbody $
       forM_
         (zip [1 .. nRows] $ unfoldr (Just . split) gen)
         (\(x, g) ->
             B.tr $
             forM_
               (take (length headings) (x : randomRs (1000, 9999) g))
               (B.td . B.toHtml))

main :: IO ()
main = do
  g <- getStdGen
  putStrLn $ renderHtml $ makeTable ["", "X", "Y", "Z"] 3 g
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
printf("<table>\n    <tr><th></th><th>X</th><th>Y</th><th>Z</th>")
every r := 1 to 4 do {
   printf("</tr>\n    <tr><td>%d</td>",r)
   every 1 to 3 do printf("<td>%d</td>",?9999)  # random 4 digit numbers per cell
   }
printf("</tr>\n</table>\n")
end

link printf
```


[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides printf]

```html5><table

    <tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>
    <tr><td>1</td><td>3129</td><td>3294</td><td>7013</td></tr>
    <tr><td>2</td><td>5045</td><td>169</td><td>5761</td></tr>
    <tr><td>3</td><td>7001</td><td>963</td><td>4183</td></tr>
    <tr><td>4</td><td>1695</td><td>1158</td><td>1240</td></tr>
</table>
```



## J


We can define:


```j
ele=:4 :0
  nm=. x-.LF
  lf=. x-.nm
  ;('<',nm,'>') ,L:0 y ,L:0 '</',nm,'>',lf
)

hTbl=:4 :0
  rows=. 'td' <@ele"1 ":&.>y
  'table' ele ('tr',LF) <@ele ('th' ele x); rows
)
```


With these definitions:


```j
   ('';;:'X Y Z') hTbl ":&.>(i.5),.i.5 3
<table><tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>
<tr><td>0</td><td>0</td><td>1</td><td>2</td></tr>
<tr><td>1</td><td>3</td><td>4</td><td>5</td></tr>
<tr><td>2</td><td>6</td><td>7</td><td>8</td></tr>
<tr><td>3</td><td>9</td><td>10</td><td>11</td></tr>
<tr><td>4</td><td>12</td><td>13</td><td>14</td></tr>
</table>
```


Or, if running under jhs:


```j
jhtml ('';;:'X Y Z') hTbl ":&.>(i.5),.i.5 3
```


to display the table inline, as html.


## Java

This example assumes the header row is the first row in the given array and does not add row numbers. They will need to be added by the programmer when constructing the array.

```java5
public class HTML {

	public static String array2HTML(Object[][] array){
		StringBuilder html = new StringBuilder(
				"<table>");
		for(Object elem:array[0]){
			html.append("<th>" + elem.toString() + "</th>");
		}
		for(int i = 1; i < array.length; i++){
			Object[] row = array[i];
			html.append("<tr>");
			for(Object elem:row){
				html.append("<td>" + elem.toString() + "</td>");
			}
			html.append("</tr>");
		}
		html.append("</table>");
		return html.toString();
	}

	public static void main(String[] args){
		Object[][] ints = {{"","X","Y","Z"},{1,1,2,3},{2,4,5,6},{3,7,8,9},{4,10,11,12}};
		System.out.println(array2HTML(ints));
	}
}
```

```html5><table><th></th><th>X</th><th>Y</th><th>Z</th><tr><td>1</td><td>1</td><td>2</td><td>3</td></tr><tr><td>2</td><td>4</td><td>5</td><td>6</td></tr><tr><td>3</td><td>7</td><td>8</td><td>9</td></tr><tr><td>4</td><td>10</td><td>11</td><td>12</td></tr></table></lang


## JavaScript


### Iterative


```JavaScript><html><head><title>Table maker</title
<script type="application/javascript">

// normally, don't do this: at least name it something other than "a"
Node.prototype.a = function (e) { this.appendChild(e); return this }

function ce(tag, txt) {
	var x = document.createElement(tag);
	x.textContent = (txt === undefined) ? '' : txt;
	return x;
}

function make_table(cols, rows) {
	var tbl = ce('table', ''), tr = ce('tr'), th;

	tbl.a(tr.a(ce('th')));

	var z = 'Z'.charCodeAt(0);
	for (var l = z - cols + 1; l <= z; l++)
		tr.a(ce('th', String.fromCharCode(l)));

	for (var r = 1; r <= rows; r++) {
		tbl.a(tr = ce('tr').a(ce('th', r)));
		for (var c = 0; c < cols; c++)
			tr.a(ce('td', Math.floor(Math.random() * 10000)));
	}

	document.body
		.a(ce('style',
			'td, th {border: 1px solid #696;' +
			'padding:.4ex} td {text-align: right }' +
			'table { border-collapse: collapse}'))
		.a(tbl);
}
</script></head>
<body><script>make_table(5, 4)</script></body></html>
```



### Functional

Alternatively we could:
:#Separate the data definition from the HTML generation, and
:#fold a more general HTML rendering function over a data tree.


```JavaScript
(() => {
    'use strict';

    // HTML ---------------------------------------------

    // treeHTML :: tree
    //      {tag :: String, text :: String, kvs :: Dict}
    //      -> String
    const treeHTML = tree =>
        foldTree(
            (x, xs) => `<${x.tag + attribString(x.kvs)}>` + (
                'text' in x ? (
                    x.text
                ) : '\n'
            ) + concat(xs) + `</${x.tag}>\n`)(
            tree
        );

    // attribString :: Dict -> String
    const attribString = dct =>
        dct ? (
            ' ' + Object.keys(dct)
            .reduce(
                (a, k) => a + k + '="' + dct[k] + '" ', ''
            ).trim()
        ) : '';

    // TEST ---------------------------------------------
    const main = () => {
        const
            tableStyle = {
                style: "width:25%; border:2px solid silver;"
            },
            trStyle = {
                style: "border:1px solid silver;text-align:right;"
            },
            strCaption = 'Table generated by JS';

        const
            n = 3,
            colNames = take(n)(enumFrom('A')),
            dataRows = map(
                x => Tuple(x)(map(randomRInt(100)(9999))(
                    colNames
                )))(take(n)(enumFrom(1)));

        const
            // TABLE AS TREE STRUCTURE  -----------------
            tableTree = Node({
                    tag: 'table',
                    kvs: tableStyle
                },
                append([
                    Node({
                        tag: 'caption',
                        text: 'Table source generated by JS'
                    }),
                    // HEADER ROW -----------------------
                    Node({
                            tag: 'tr',
                        },
                        map(k => Node({
                            tag: 'th',
                            kvs: {
                                style: "text-align:right;"
                            },
                            text: k
                        }))(cons('')(colNames))
                    )
                    // DATA ROWS ------------------------
                ])(map(tpl => Node({
                    tag: 'tr',
                    kvs: trStyle
                }, cons(
                    Node({
                        tag: 'th',
                        text: fst(tpl)
                    }))(
                    map(v => Node({
                        tag: 'td',
                        text: v.toString()
                    }))(snd(tpl))
                )))(dataRows))
            );

        // Return a value and/or apply console.log to it.
        // (JS embeddings vary in their IO channels)
        const strHTML = treeHTML(tableTree);
        return (
            console.log(strHTML)
            //strHTML
        );
    };


    // GENERIC FUNCTIONS --------------------------------

    // Node :: a -> [Tree a] -> Tree a
    const Node = (v, xs) => ({
        type: 'Node',
        root: v,
        nest: xs || []
    });

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = a => b => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // append (++) :: [a] -> [a] -> [a]
    // append (++) :: String -> String -> String
    const append = xs => ys => xs.concat(ys);

    // chr :: Int -> Char
    const chr = String.fromCodePoint;

    // concat :: [[a]] -> [a]
    // concat :: [String] -> String
    const concat = xs =>
        0 < xs.length ? (() => {
            const unit = 'string' !== typeof xs[0] ? (
                []
            ) : '';
            return unit.concat.apply(unit, xs);
        })() : [];

    // cons :: a -> [a] -> [a]
    const cons = x => xs => [x].concat(xs);

    // enumFrom :: a -> [a]
    function* enumFrom(x) {
        let v = x;
        while (true) {
            yield v;
            v = succ(v);
        }
    }

    // enumFromToChar :: Char -> Char -> [Char]
    const enumFromToChar = m => n => {
        const [intM, intN] = [m, n].map(
            x => x.charCodeAt(0)
        );
        return Array.from({
            length: Math.floor(intN - intM) + 1
        }, (_, i) => String.fromCodePoint(intM + i));
    };

    // foldTree :: (a -> [b] -> b) -> Tree a -> b
    const foldTree = f => tree => {
        const go = node =>
            f(node.root, node.nest.map(go));
        return go(tree);
    };

    // fst :: (a, b) -> a
    const fst = tpl => tpl[0];

    // isChar :: a -> Bool
    const isChar = x =>
        ('string' === typeof x) && (1 === x.length);

    // map :: (a -> b) -> [a] -> [b]
    const map = f => xs =>
        (Array.isArray(xs) ? (
            xs
        ) : xs.split('')).map(f);

    // ord :: Char -> Int
    const ord = c => c.codePointAt(0);

    // randomRInt :: Int -> Int -> () -> Int
    const randomRInt = low => high => () =>
        low + Math.floor(
            (Math.random() * ((high - low) + 1))
        );

    // snd :: (a, b) -> b
    const snd = tpl => tpl[1];

    // succ :: Enum a => a -> a
    const succ = x =>
        isChar(x) ? (
            chr(1 + ord(x))
        ) : isNaN(x) ? (
            undefined
        ) : 1 + x;

    // take :: Int -> [a] -> [a]
    // take :: Int -> String -> String
    const take = n => xs =>
        'GeneratorFunction' !== xs.constructor.constructor.name ? (
            xs.slice(0, n)
        ) : [].concat.apply([], Array.from({
            length: n
        }, () => {
            const x = xs.next();
            return x.done ? [] : [x.value];
        }));

    // MAIN ---
    return main();
})();
```

<table style="width:25%; border:2px solid silver;">
<caption>Table source generated by JS</caption>
<tr>
<th style="text-align:right;"></th>
<th style="text-align:right;">A</th>
<th style="text-align:right;">B</th>
<th style="text-align:right;">C</th>
</tr>
<tr style="border:1px solid silver;text-align:right;">
<th>1</th>
<td>8464</td>
<td>1650</td>
<td>8275</td>
</tr>
<tr style="border:1px solid silver;text-align:right;">
<th>2</th>
<td>547</td>
<td>9794</td>
<td>8690</td>
</tr>
<tr style="border:1px solid silver;text-align:right;">
<th>3</th>
<td>3702</td>
<td>1170</td>
<td>9004</td>
</tr>
</table>


Raw HTML output:


```txt
<table style="width:25%; border:2px solid silver;">
<caption>Table source generated by JS</caption>
<tr>
<th style="text-align:right;"></th>
<th style="text-align:right;">A</th>
<th style="text-align:right;">B</th>
<th style="text-align:right;">C</th>
</tr>
<tr style="border:1px solid silver;text-align:right;">
<th>1</th>
<td>8464</td>
<td>1650</td>
<td>8275</td>
</tr>
<tr style="border:1px solid silver;text-align:right;">
<th>2</th>
<td>547</td>
<td>9794</td>
<td>8690</td>
</tr>
<tr style="border:1px solid silver;text-align:right;">
<th>3</th>
<td>3702</td>
<td>1170</td>
<td>9004</td>
</tr>
</table>

```



## jq

```jq
def html_row:
  "<tr>",
  "  \(.[] | "<td>\(.)</td>")",
  "</tr>";

def html_header:
  "<thead align = 'right'>",
  "  \(html_row)",
  "</thead>";

 def html_table(header):
  "<table>",
  "  \(header | html_header)",
  "  <tbody align = 'right'>",
  "    \(.[] | html_row)",
  "  </tbody",
  "</table>";

# Prepend the sequence number
def html_table_with_sequence(header):
  length as $length
  | . as $in
  | [range(0;length) | [.+1] + $in[.]] |  html_table(header);
```


'''Example'''

```jq
def data:
  [ [4,5,6],
    [41, 51, 61],
    [401, 501, 601] ];

# The first column has no header
data | html_table_with_sequence( ["", "X", "Y", "Z"] )
```

```sh
$ jq -r -n -f Create_an_HTML_table.jq
<table>
  <thead align = 'right'>
    <tr>
      <td></td>
      <td>X</td>
      <td>Y</td>
      <td>Z</td>
    </tr>
  </thead>
  <tbody align = 'right'>
    <tr>
      <td>1</td>
      <td>4</td>
      <td>5</td>
      <td>6</td>
    </tr>
    <tr>
      <td>2</td>
      <td>41</td>
      <td>51</td>
      <td>61</td>
    </tr>
    <tr>
      <td>3</td>
      <td>401</td>
      <td>501</td>
      <td>601</td>
    </tr>
  </tbody
</table>
```



## Julia


```julia
function tag(x::Pair, attr::Pair...)
    t, b = x
    attrstr = join(" $n=\"$p\"" for (n, p) in attr)
    return "<$t$attrstr>$b</$t>"
end

colnames = split(",X,Y,Z", ',')

header = join(tag(:th => txt) for txt in colnames) * "\n"
rows   = collect(tag(:tr => join(tag(:td => i, :style => "font-weight: bold;") * join(tag(:td => rand(1000:9999)) for j in 1:3))) for i in 1:6)
body   = "\n" * join(rows, '\n') * "\n"
table  = tag(:table => string('\n', header, body, '\n'), :style => "width: 60%")
println(table)
```


<table style="width: 60%">
<th></th><th>X</th><th>Y</th><th>Z</th>

<tr><td style="font-weight: bold;">1</td><td>5399</td><td>5770</td><td>3362</td></tr>
<tr><td style="font-weight: bold;">2</td><td>4564</td><td>1577</td><td>5428</td></tr>
<tr><td style="font-weight: bold;">3</td><td>6257</td><td>7290</td><td>6138</td></tr>
<tr><td style="font-weight: bold;">4</td><td>3912</td><td>5163</td><td>2451</td></tr>
<tr><td style="font-weight: bold;">5</td><td>1426</td><td>1874</td><td>3944</td></tr>
<tr><td style="font-weight: bold;">6</td><td>3896</td><td>9827</td><td>7006</td></tr>

</table>


'''The raw html:'''

```html5
<table style="width: 60%">
<th></th><th>X</th><th>Y</th><th>Z</th>

<tr><td style="font-weight: bold;">1</td><td>5399</td><td>5770</td><td>3362</td></tr>
<tr><td style="font-weight: bold;">2</td><td>4564</td><td>1577</td><td>5428</td></tr>
<tr><td style="font-weight: bold;">3</td><td>6257</td><td>7290</td><td>6138</td></tr>
<tr><td style="font-weight: bold;">4</td><td>3912</td><td>5163</td><td>2451</td></tr>
<tr><td style="font-weight: bold;">5</td><td>1426</td><td>1874</td><td>3944</td></tr>
<tr><td style="font-weight: bold;">6</td><td>3896</td><td>9827</td><td>7006</td></tr>

</table>
```



## Kotlin


```scala
// version 1.1.3

import java.util.Random

fun main(args: Array<String>) {
    val r = Random()
    val sb = StringBuilder()
    val i = "   "  // indent
    with (sb) {
        append("<html>\n<head>\n")
        append("<style>\n")
        append("table, th, td  { border: 1px solid black; }\n")
        append("th, td { text-align: right; }\n")
        append("</style>\n</head>\n<body>\n")
        append("<table style=\"width:60%\">\n")
        append("$i<thead>\n")
        append("$i$i<tr><th></th>")
        for (c in 'X'..'Z') append("<th>$c</th>")
        append("</tr>\n")
        append("$i</thead>\n")
        append("$i<tbody>\n")
        val f = "$i$i<tr><td>%d</td><td>%d</td><td>%d</td><td>%d</td></tr>\n"
        for (j in 1..4) {
            append(f.format(j, r.nextInt(10000), r.nextInt(10000), r.nextInt(10000)))
        }
        append("$i</tbody>\n")
        append("</table>\n")
        append("</body>\n</html>")
    }
    println(sb.toString())
}
```


Sample output:

```HTML5><html

<head>
<style>
table, th, td  { border: 1px solid black; }
th, td { text-align: right; }
</style>
</head>
<body>
<table style="width:60%">
   <thead>
      <tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>
   </thead>
   <tbody>
      <tr><td>1</td><td>1444</td><td>3451</td><td>2568</td></tr>
      <tr><td>2</td><td>876</td><td>3866</td><td>824</td></tr>
      <tr><td>3</td><td>2710</td><td>3845</td><td>8089</td></tr>
      <tr><td>4</td><td>6480</td><td>7885</td><td>9246</td></tr>
   </tbody>
</table>
</body>
</html>
```



## Lambdatalk


Lambdatalk outputs standard HTML/CSS code sent to the web browser who does the job.


```scheme

{table
 {@ style="background:#ffe; width:50%;"}
 {tr {@ style="text-align:right; font:bold 1.0em arial;"}
     {td } {td X} {td Y} {td Z}}
 {map {lambda {:i}
       {tr {td {b :i}}
           {map {lambda {_}
            {td {@ style="text-align:right; font:italic 1.0em courier;"}
                {floor {* {random} 10000}} }}
      {serie 1 3}}}}
      {serie 1 3}}}

```


<table style="background:#ffe; width:50%;"><tr style="text-align:right; font:bold 1.0em arial;"><td></td> <td>X</td> <td>Y</td> <td>Z</td></tr> <tr><td><b>1</b></td> <td style="text-align:right; font:italic 1.0em courier;">4220</td> <td style="text-align:right; font:italic 1.0em courier;">7476</td> <td style="text-align:right; font:italic 1.0em courier;">4414</td></tr> <tr><td><b>2</b></td> <td style="text-align:right; font:italic 1.0em courier;">6</td> <td style="text-align:right; font:italic 1.0em courier;">5335</td> <td style="text-align:right; font:italic 1.0em courier;">1381</td></tr> <tr><td><b>3</b></td> <td style="text-align:right; font:italic 1.0em courier;">8549</td> <td style="text-align:right; font:italic 1.0em courier;">1598</td> <td style="text-align:right; font:italic 1.0em courier;">5380</td></tr></table>


## Lasso


```Lasso>define rand4dig =
 integer_random(9999, 1)

local(
	output = '<table border=2 cellpadding=5  cellspace=0>\n<tr>'
)

with el in ('&#160;,X,Y,Z') -> split(',') do {
	#output -> append('<th>' + #el + '</th>')
}
#output -> append('</tr>\n')

loop(5) => {
	#output -> append('<tr>\n<td style="font-weight: bold;">' + loop_count + '</td>')
	loop(3) => {
		#output -> append('<td>' + rand4dig + '</td>')
	}
	#output -> append('</tr>\n')
}
#output -> append('</table>\n')

#output
```

<table border=2 cellpadding=5  cellspace=0>
<tr><th>&#160;</th><th>X</th><th>Y</th><th>Z</th></tr>
<tr>
<td style="font-weight: bold;">1</td><td>5991</td><td>9892</td><td>6754</td></tr>
<tr>
<td style="font-weight: bold;">2</td><td>8441</td><td>3816</td><td>322</td></tr>
<tr>
<td style="font-weight: bold;">3</td><td>8976</td><td>4175</td><td>202</td></tr>
<tr>
<td style="font-weight: bold;">4</td><td>6705</td><td>8225</td><td>8069</td></tr>
<tr>
<td style="font-weight: bold;">5</td><td>7558</td><td>496</td><td>2577</td></tr>
</table>


```txt
<table border=2 cellpadding=5  cellspace=0>
<tr><th>&#160;</th><th>X</th><th>Y</th><th>Z</th></tr>
<tr>
<td style="font-weight: bold;">1</td><td>5991</td><td>9892</td><td>6754</td></tr>
<tr>
<td style="font-weight: bold;">2</td><td>8441</td><td>3816</td><td>322</td></tr>
<tr>
<td style="font-weight: bold;">3</td><td>8976</td><td>4175</td><td>202</td></tr>
<tr>
<td style="font-weight: bold;">4</td><td>6705</td><td>8225</td><td>8069</td></tr>
<tr>
<td style="font-weight: bold;">5</td><td>7558</td><td>496</td><td>2577</td></tr>
</table>
```



## Liberty BASIC

This creates and saves an html file, then calls web browser to display it.


A time delay is needed to allow this, then the file is deleted.

```lb

    nomainwin

    quote$ =chr$( 34)

    html$  ="<html><head></head><body>"

    html$ =html$ +"<table border =" +quote$ +"6"+ quote$ +" solid rules =none ; cellspacing =" +quote$ +"10" +quote$ +"> <th> </th> <th> X </th> <th> Y </th> <th> Z </th>"

    for i =1 to 4
        d1$ =str$( i)
        d2$ =str$( int( 10000 *rnd( 1)))
        d3$ =str$( int( 10000 *rnd( 1)))
        d4$ =str$( int( 10000 *rnd( 1)))
        html$ =html$ +"<tr align ="; quote$; "right"; quote$; "> <th>"; d1$; " </th> <td>" +d2$ +" </td> <td>" +d3$ +" </td> <td>" +d4$ +" </td> </tr>"
    next i

    html$ =html$ +"</table>"

    html$ =html$ +"</body></html>"

    open "table.html" for output as #o
        #o html$;
    close #o

    address$ ="table.html"
    run "explorer.exe "; address$

    timer 5000, [on]
    wait
    [on]
    timer 0

    kill "table.html"

    wait

sub quit w$
    close #w$
    end
end sub

```



## Lingo


```lingo
on htmlTable (data)
  str = "<table>"

  -- table head
  put "<thead><tr><th> </th>" after str
  repeat with cell in data[1]
    put "<th>"&cell&"</th>" after str
  end repeat
  put "</tr></thead>" after str

  -- table body
  put "<tbody>" after str
  cnt = data.count
  repeat with i = 2 to cnt
    put "<tr><td>"&(i-1)&"</td>" after str
    repeat with cell in data[i]
      put "<td>"&cell&"</td>" after str
    end repeat
    put "</tr>" after str
  end repeat
  put "</tbody>" after str

  put "</table>" after str
  return str
end
```



```lingo
tableData = [\
  ["X", "Y", "Z"],\
  ["1", "2", "3"],\
  ["4", "5", "6"],\
  ["7", "8", "9"]\
]

htmlCode = htmlTable(tableData)

-- render the result in a text member (which only supports simple/ancient HTML)
m = new(#text)
m.text = "<html><body>"&htmlCode&"</body></html>"
```



## Lua

This function is written so as to take arbitrary table data.  Its argument is a (Lua) table of (Lua) tables, where each sub-table is one row of the HTML table.  The first row is assumed to be the column headings.

```Lua
function htmlTable (data)
    local html = "<table>\n<tr>\n<th></th>\n"
    for _, heading in pairs(data[1]) do
        html = html .. "<th>" .. heading .. "</th>" .. "\n"
    end
    html = html .. "</tr>\n"
    for row = 2, #data do
        html = html .. "<tr>\n<th>" .. row - 1 .. "</th>\n"
        for _, field in pairs(data[row]) do
            html = html .. "<td>" .. field .. "</td>\n"
        end
        html = html .. "</tr>\n"
    end
    return html .. "</table>"
end

local tableData = {
    {"X", "Y", "Z"},
    {"1", "2", "3"},
    {"4", "5", "6"},
    {"7", "8", "9"}
}

print(htmlTable(tableData))
```

```txt
<table>
<tr>
<th></th>
<th>X</th>
<th>Y</th>
<th>Z</th>
</tr>
<tr>
<th>1</th>
<td>1</td>
<td>2</td>
<td>3</td>
</tr>
<tr>
<th>2</th>
<td>4</td>
<td>5</td>
<td>6</td>
</tr>
<tr>
<th>3</th>
<td>7</td>
<td>8</td>
<td>9</td>
</tr>
</table>
```


## M2000 Interpreter


```qbasic

MODULE HtmlTable {
	tag$=LAMBDA$ (a$)-> {
		=LAMBDA$ a$ -> {
			IF ISNUM THEN w$=STR$(NUMBER,0) ELSE w$=LETTER$
			READ ? part$
			="<"+a$+IF$(LEN(part$)>0->" "+part$,"")+">"+w$+"</"+a$+">"+CHR$(13)+CHR$(10)
		}
	}
	INVENTORY Fun
	STACK NEW {
		DATA "html", "head", "body", "table", "tr", "th", "td"
		WHILE NOT EMPTY
			OVER ' duplicate top of stack
			APPEND Fun, LETTER$:=tag$(LETTER$)
		END WHILE
	}
	DEF body0$="",body$=""
	STACK NEW {
		DATA "", "X", "Y", "Z"
		FOR i=1 TO 4
			body0$+=Fun$("th")(LETTER$)
			z$=""
			FOR j=1 TO 3 : z$+=Fun$("td")(RANDOM(0, 9999), {align="right"}) : NEXT j
			body$+=Fun$("tr")(Fun$("th")(i)+z$)
		NEXT i
	}
	table$=fun$("table")(fun$("tr")(body0$)+body$,"border=1 cellpadding=10 cellspacing=0")
	DOCUMENT final$="<!DOCTYPE html>"+CHR$(13)+CHR$(10)
	final$=fun$("html")(fun$("head")("")+fun$("body")(table$), {lang="en"})
	file$="c:\doc.html"
	REPORT final$
	CLIPBOARD final$
	SAVE.DOC final$, file$
	WIN file$  ' execute, no wait
}
HtmlTable

```


<pre style="height:30ex;overflow:scroll">
<!DOCTYPE html>
<html lang="en"><head></head>
<body><table border=1 cellpadding=10 cellspacing=0><tr><th></th>
<th>X</th>
<th>Y</th>
<th>Z</th>
</tr>
<tr><th>1</th>
<td align="right">9007</td>
<td align="right">1425</td>
<td align="right">4897</td>
</tr>
<tr><th>2</th>
<td align="right">1795</td>
<td align="right">6858</td>
<td align="right">9682</td>
</tr>
<tr><th>3</th>
<td align="right">2233</td>
<td align="right">8270</td>
<td align="right">5221</td>
</tr>
<tr><th>4</th>
<td align="right">6619</td>
<td align="right">3688</td>
<td align="right">4052</td>
</tr>
</table>
</body>
</html>
</pre >

=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
x := RandomInteger[10];
Print["<table>", "\n","<tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>"]
Scan[Print["<tr><td>", #, "</td><td>", x, "</td><td>", x, "</td><td>","</td></tr>"] & , Range[3]]
Print["</table>"]
```


```txt
<table>
<tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>
<tr><td>1</td><td>6</td><td>10</td><td></td></tr>
<tr><td>2</td><td>1</td><td>10</td><td></td></tr>
<tr><td>3</td><td>6</td><td>7</td><td></td></tr>
</table>
```


=={{header|MATLAB}} / {{header|Octave}}==


```matlab
function htmltable(fid,table,Label)
   fprintf(fid,'<table>\n  <thead align = "right">\n');
   if nargin<3,
       fprintf(fid,'    <tr><th></th><td>X</td><td>Y</td><td>Z</td></tr>\n  </thead>\n  <tbody align = "right">\n');
   else
       fprintf(fid,'    <tr><th></th>');
       fprintf(fid,'<td>%s</td>',Label{:});
       fprintf(fid,'</tr>\n  </thead>\n  <tbody align = "right">\n');
   end;
   fprintf(fid,'    <tr><td>%2i</td><td>%5i</td><td>%5i</td><td>%5i</td></tr>\n', [1:size(table,1);table']);
   fprintf(fid,'  </tbody>\n</table>\n');
end
```


```txt
>> htmltable(1,ceil(rand(5,3)*10000))
<table>
  <thead align = "right">
    <tr><th></th><td>X</td><td>Y</td><td>Z</td></tr>
  </thead>
  <tbody align = "right">
    <tr><td> 1</td><td> 6639</td><td> 1110</td><td>  296</td></tr>
    <tr><td> 2</td><td> 4864</td><td> 1252</td><td> 8412</td></tr>
    <tr><td> 3</td><td> 3800</td><td> 4556</td><td> 3752</td></tr>
    <tr><td> 4</td><td> 5728</td><td> 6897</td><td> 2157</td></tr>
    <tr><td> 5</td><td> 2272</td><td> 8503</td><td> 7021</td></tr>
  </tbody>
</table>
```


=={{header|Modula-2}}==

```modula2
MODULE testCGI;

FROM  InOut       IMPORT  WriteCard, WriteLn, WriteString, WriteBf;
FROM  Arguments   IMPORT  ArgTable, GetEnv;
FROM  Strings     IMPORT  Assign, Length, String;

VAR  EnvVars             : ArgTable;

PROCEDURE ReadEnvVar;

VAR   Value          : String;
      i              : CARDINAL;

BEGIN
   WriteString ('<table border="1" cellpadding="4" width="80%" align="center">');
   WriteString ('<tr><th>Index</th><th>Length</th><th>Content</th></tr>');
   i := 0;
   LOOP
      IF  EnvVars^ [i] = NIL  THEN  EXIT  END;
      Assign (Value, EnvVars^ [i]^);
      WriteString ('<tr><td align="center">');
      WriteCard (i, 2);
      WriteString ('</td><td align="center">');
      WriteCard (Length (Value), 3);
      WriteString ('</td><td>');    WriteString (Value);
      WriteString ("</td></tr>");
      WriteLn;
      INC (i)
   END;
   WriteString("</table>");
END ReadEnvVar;

BEGIN
   GetEnv (EnvVars);
   WriteString ('Content-type:text/html');
   WriteLn;
   WriteLn;
   WriteString ('<html><head>');
   WriteString ('<title>CGI with the Mocka Modula-2 compiler</title>');
   WriteString ('</head><body>');
   WriteLn;
   WriteString ('<center><h2>CGI environment passed along by your browser</h2></center><p>');
   ReadEnvVar;
   WriteString ('</body></html>');
   WriteLn;
   WriteBf
END testCGI.
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

-- create some test data.  Put the data in a Rexx indexed string
maxI = 1000
rng = Random()
xyz = ''
xyz[0] = 1; xyz[1] = '. X Y Z' -- use a dot to indicate an empty cell
loop r_ = 1 for 5
  ra = r_ rng.nextInt(maxI) rng.nextInt(maxI) rng.nextInt(maxI)
  xyz[0] = r_ + 1; xyz[r_ + 1] = ra
  end r_

-- build an HTML string
html = htmlHeader()
html = html || htmlTable(xyz)
html = html || htmlFooter()

-- display HTML at standard output device
say html

return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- HTML boilerplate header
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method htmlHeader() public static returns Rexx
  html = '<?xml version="1.0" encoding="UTF-8"?>\n' -
      || '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">\n' -
      || '<html xmlns="http://www.w3.org/1999/xhtml" lang="en-US" xml:lang="en-US">\n' -
      || '<head>\n' -
      || '<meta http-equiv="Content-type" content="text/html;charset=UTF-8"/>\n' -
      || '<title>RCreateHTMLTable</title>\n' -
      || '<style type="text/css">\n' -
      || '<!--\n' -
      || '/* <![DATA[ */\n' -
      || 'body {font-family: "Lucida Grande", "Geneva", "Verdana", "Helvetica Neue", "Helvetica", "DejaVu Sans", "Arial", sans-serif;}\n' -
      || 'table, th, td {table-layout: fixed; border: 1px solid black; border-collapse: collapse; padding: 0.25em; font-size: 85%;}\n' -
      || 'th, td {width: 6em;}\n' -
      || 'th {color: white; background-color: green;}\n' -
      || 'td {text-align: right;}\n' -
      || 'p.classname {\n' -
      || '  font-size: inherit;\n' -
      || '}\n' -
      || '/* ]] */\n' -
      || '//-->\n' -
      || '</style>\n' -
      || '</head>\n' -
      || '<body>\n' -
      || '<h1>Rosetta Code &ndash; NetRexx Sample Output</h2>\n' -
      || '<h2><a href="http://rosettacode.org/wiki/Create_an_HTML_table">Create an HTML table</a></h2>\n' -
      || ''

  return html

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- HTML footer
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method htmlFooter() public static returns Rexx
  html = '</body>\n' -
      || '</html>\n' -
      || ''
  return html

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Create the table
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method htmlTable(rows, caption = '') public static returns Rexx
  html = '<table>\n'
  if caption.length() > 0 then do
    html = html -
        || '<caption>'caption'</caption>\n' -
        || '<thead>\n' -
        || ''
    end
  html = html -
      || htmlCsvTableRow(rows[1], 'th')'\n' -
      || '</thead>\n' -
      || '<tbody>\n' -
      || ''
  loop r_ = 2 to rows[0]
    html = html -
        || htmlCsvTableRow(rows[r_])
    end r_
  html = html -
      || '</tbody>\n' -
      || '</table>\n' -
      || ''
  return html

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Add a row to the table
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method htmlCsvTableRow(row, tag = 'td', sep = ' ', emptyCell = '.') public static returns Rexx
  if tag = null then tag = 'td'
  row = row.strip('t')
  -- replace HTML special characters with symbol entities
  row = row.changestr('&', '&amp;') -- need to do this one first to avoid double translation
  row = row.changestr('"', '&quot;')
  row = row.changestr("'", '&apos;')
  row = row.changestr('<', '&lt;')
  row = row.changestr('>', '&gt;')
  elmts = ''
  elmts[0] = 0
  e_ = 0
  loop while row.length() > 0
    parse row elmt (sep) row
    if elmt == emptyCell then elmt = ' ' -- replace empy cells with non-breaking spaces
    e_ = e_ + 1; elmts[0] = e_; elmts[e_] = elmt
    end
  html = '<tr>\n' -
      || ''
  loop e_ = 1 to elmts[0]
    html = html -
        || '<'tag'>'elmts[e_]'</'tag'>\n' -
        || ''
    end e_
  html = html -
      || '</tr>\n' -
      || ''
  return html


```

<div style="height:80ex;overflow:scroll;">

```html5
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en-US" xml:lang="en-US">
<head>
<meta http-equiv="Content-type" content="text/html;charset=UTF-8"/>
<title>RCreateHTMLTable</title>
<style type="text/css">
<!--
/* <![DATA[ */
body {font-family: "Lucida Grande", "Geneva", "Verdana", "Helvetica Neue", "Helvetica", "DejaVu Sans", "Arial", sans-serif;}
table, th, td {table-layout: fixed; border: 1px solid black; border-collapse: collapse; padding: 0.25em; font-size: 85%;}
th, td {width: 6em;}
th {color: white; background-color: green;}
td {text-align: right;}
p.classname {
  font-size: inherit;
}
/* ]] */
//-->
</style>
</head>
<body>
<h1>Rosetta Code &ndash; NetRexx Sample Output</h2>
<h2><a href="http://rosettacode.org/wiki/Create_an_HTML_table">Create an HTML table</a></h2>
<table>
<tr>
<th> </th>
<th>X</th>
<th>Y</th>
<th>Z</th>
</tr>

</thead>
<tbody>
<tr>
<td>1</td>
<td>626</td>
<td>128</td>
<td>386</td>
</tr>
<tr>
<td>2</td>
<td>985</td>
<td>568</td>
<td>636</td>
</tr>
<tr>
<td>3</td>
<td>639</td>
<td>622</td>
<td>591</td>
</tr>
<tr>
<td>4</td>
<td>843</td>
<td>268</td>
<td>436</td>
</tr>
<tr>
<td>5</td>
<td>132</td>
<td>526</td>
<td>251</td>
</tr>
</tbody>
</table>
</body>
</html>

```

</div>
[[File:RCreateHTMLTable.png]]


## NewLISP


```NewLISP
; file:   html-table.lsp
; url:    http://rosettacode.org/wiki/Create_an_HTML_table
; author: oofoe 2012-01-29

(seed (time-of-day)) ; Initialize random number generator.

; The "tab" variable tracks the HTML indent. "pad" composes a line
; with the appropriate indent and a terminal newline.

(setq tab 0)
(define (pad text) (string (dup "  " tab) text "\n"))

; NewLISP allows almost any character in an identifier, so I can name
; my functions after the HTML elements they invoke. This one formats a
; single table data cell.

(define (<td> text) (pad (string "<td>" text "</td>")))

; "<tr>" will accept either a number of arguments, each one to be
; formatted as a table cell, or a single list argument, which is
; broken into table cells. For convenience, I format each list item
; with the "<td>" function so I can feed it raw lists.

(define (<tr>)
  (let ((data (args))
        (s    (pad "<tr>")))

    (if (list? (data 0)) (setq data (data 0)))

    (inc tab)
    (dolist (el data) (extend s (<td> el)))
    (dec tab)

    (extend s (pad "</tr>"))
    s))

; By defining "<table>" as a macro, I ensure that the rows won't be
; evaluated until I've got the table started, which preserves the
; formatting.

(define-macro (<table>)
  (let ((s (pad "<table>")))
    (inc tab) (doargs (row) (extend s (eval row)))  (dec tab)
    (extend s (pad "</table>"))
    s
    ))

; Test

(print (<table> (<tr> "" "X" "Y" "Z")
                (<tr> (cons 0 (rand 1000 3)))
                (<tr> (cons 1 (rand 1000 3)))
                (<tr> (cons 2 (rand 1000 3)))
                ))

(exit)
```


```txt
<table>
  <tr>
    <td></td>
    <td>X</td>
    <td>Y</td>
    <td>Z</td>
  </tr>
  <tr>
    <td>0</td>
    <td>289</td>
    <td>824</td>
    <td>462</td>
  </tr>
  <tr>
    <td>1</td>
    <td>49</td>
    <td>600</td>
    <td>84</td>
  </tr>
  <tr>
    <td>2</td>
    <td>511</td>
    <td>219</td>
    <td>742</td>
  </tr>
</table>
```


Table:

<table>
  <tr>
    <td></td>
    <td>X</td>
    <td>Y</td>
    <td>Z</td>
  </tr>
  <tr>
    <td>0</td>
    <td>289</td>
    <td>824</td>
    <td>462</td>
  </tr>
  <tr>
    <td>1</td>
    <td>49</td>
    <td>600</td>
    <td>84</td>
  </tr>
  <tr>
    <td>2</td>
    <td>511</td>
    <td>219</td>
    <td>742</td>
  </tr>
</table>


## Nim


```nim
import math, htmlgen
randomize()

template randTD(): expr = td($random(1000..9999))
proc randTR(x): auto =
  tr(td($x, style="font-weight: bold"), randTD, randTD, randTD)

echo table(
  tr(th"", th"X", th"Y", th"Z"),
  randTR 1,
  randTR 2,
  randTR 3,
  randTR 4,
  randTR 5)
```

<table><tr><th></th><th>X</th><th>Y</th><th>Z</th></tr><tr><td style="font-weight: bold">1</td><td>5084</td><td>7059</td><td>8308</td></tr><tr><td style="font-weight: bold">2</td><td>6185</td><td>3549</td><td>8831</td></tr><tr><td style="font-weight: bold">3</td><td>8063</td><td>5561</td><td>1675</td></tr><tr><td style="font-weight: bold">4</td><td>2777</td><td>1769</td><td>6570</td></tr><tr><td style="font-weight: bold">5</td><td>5465</td><td>9508</td><td>4775</td></tr></table>

Raw output:
<pre html5><table><tr><th></th><th>X</th><th>Y</th><th>Z</th></tr><tr><td style="font-weight: bold">1</td><td>5084</td><td>7059</td><td>8308</td></tr><tr><td style="font-weight: bold">2</td><td>6185</td><td>3549</td><td>8831</td></tr><tr><td style="font-weight: bold">3</td><td>8063</td><td>5561</td><td>1675</td></tr><tr><td style="font-weight: bold">4</td><td>2777</td><td>1769</td><td>6570</td></tr><tr><td style="font-weight: bold">5</td><td>5465</td><td>9508</td><td>4775</td></tr></table>
```



## Objeck


```objeck

﻿class CreateTable {
  function : Main(args : String[]) ~ Nil {
    s := String->New();

    s->Append("<table>");
    s->Append("<thead align = \"right\">");
    s->Append("<tr><th></th>");
    td := "XYZ";
    for(i:=0; i<3; i+=1;) {
      s->Append("<td>");
      s->Append(td->Get(i));
      s->Append("</td>");
    };
    s->Append("</tr>");
    s->Append("</thead>");
    s->Append("<tbody align = \"right\">");
    for(i:=0; i<3; i+=1;) {
      s->Append("<tr><td>");
      s->Append(i);
      s->Append("</td>");
      for(j:=0; j<3; j+=1;) {
        s->Append("<td>");
        s->Append((Float->Random() * 10000)->As(Int));
        s->Append("</td>");
      };
      s->Append("</tr>");
    };
    s->Append("</tbody>");
    s->Append("</table>");

    s->PrintLine();
  }
}

```



## OCaml


A simple <code>printf</code> method:


```ocaml
let () =
  let buf = Buffer.create 1 in
  let s = Buffer.add_string buf in
  Random.self_init();
  s "<table>";
  s "<thead align='right'>";
  s "<tr><th></th>";
  List.iter (fun v ->
    s ("<td>" ^ v ^ "</td>")
  ) ["X"; "Y"; "Z"];
  s "</tr>";
  s "</thead>";
  s "<tbody align='right'>";
  for i = 0 to pred 3 do
    s ("<tr><td>" ^ string_of_int i ^ "</td>");
      for j = 0 to pred 3 do
        s ("<td>" ^ string_of_int (Random.int 1000) ^ "</td>");
      done;
    s "</tr>";
  done;
  s "</tbody>";
  s "</table>";
  print_endline (Buffer.contents buf)
```



###  With a dedicated library


Using the library [http://theorie.physik.uni-wuerzburg.de/~ohl/xhtml/ ocaml-xhtml]. With this library the validity of the pages is guaranteed by the OCaml type system.


```ocaml
open XHTML.M_01_01

let _td s = td [pcdata s]
let _th s = th [pcdata s]

let my_table =
  table ~a:[a_border 1]
    (tr
      (_th "") [
      (_th "X");
      (_th "Y");
      (_th "Z")]
    )
  [
    (tr
      (_td "1") [
      (_td "aa");
      (_td "bb");
      (_td "cc")]
    );
    (tr
      (_td "2") [
      (_td "dd");
      (_td "ee");
      (_td "ff")]
    );
  ]

let my_page =
  html
    (head (title (pcdata "My Page")) [])
    (body
      [ h1 [pcdata "My Table"];
        my_table;
      ]
    )

let () =
  pretty_print ~width:80 print_string my_page
```



###  TyXml


The library [http://ocsigen.org/tyxml/ TyXml] contains a module for XHTML that provides the same interface than the previous <code>ocaml-xhtml</code> library.


```ocaml
#use "topfind"
#require "tyxml"

module X = Xhtml.M_01_01  (* XHTML 1.1 *)
module P = Xhtml.P_01_01

let make_table () =
  let td1 = X.td [X.pcdata "1"] in
  let td2 = X.td [X.pcdata "2"] in
  let td3 = X.td [X.pcdata "3"] in
  let my_tr = X.tr td1 [td2; td3] in
  let my_table = X.table my_tr [] in
  (my_table)

let () =
  let my_title = X.title (X.pcdata "My Page") in
  let my_head = X.head my_title [] in
  let my_h1 = X.h1 [X.pcdata "My Table"] in

  let my_table = make_table () in

  let my_body = X.body [my_h1; my_table] in
  let my_html = X.html my_head my_body in
  P.print print_endline my_html;
;;
```


The previous function <code>make_table ()</code> produces a simple table, we can replace it by the function below to output a more complex table with <code>thead</code> and <code>tbody</code>:


```ocaml
let make_table () =
  let br = X.a_border 1 in
  let th s = X.th [X.pcdata s] in
  let td s = X.td [X.pcdata s] in
  let my_thead = X.thead (X.tr (th "") [th "X"; th "Y"; th "Z"]) [] in
  let my_tr1 = X.tr (td "1") [td "AAA"; td "BBB"; td "CCC"] in
  let my_tr2 = X.tr (td "2") [td "DDD"; td "EEE"; td "FFF"] in
  let my_tr3 = X.tr (td "3") [td "GGG"; td "HHH"; td "III"] in
  let my_tbody = X.tbody my_tr1 [my_tr2; my_tr3] in
  let my_table = X.tablex ~thead:my_thead ~a:[br] my_tbody [] in
  (my_table)
```



## Oz

As a complete web application, using the [https://github.com/wmeyer/roads/wiki "Roads"] web programming library. Connect your browser to http://localhost:8080/table after starting the program.

```oz
declare

[Roads] = {Module.link ['x-ozlib://wmeyer/roads/Roads.ozf']}

fun {Table Session}
   html(
      head(title("Show a table with row and column headings")
	   style(type:"text/css"
		 css(td 'text-align':center)
		))
      body(
	 {TagFromList table
	  tr(th th("X") th("Y") th("Z"))
	  |
	  {CreateRows 3 5}
	 }))
end

fun {CreateRows NumCols NumRows}
   {List.map {List.number 1 NumRows 1}
    fun {$ Row}
       {TagFromList tr
	td( {Int.toString Row} )
	|
	{List.map {List.number 1 NumCols 1}
	 fun {$ Col}
	    SequentialNumber = (Row-1)*NumCols + Col
	 in
	    td( {Int.toString SequentialNumber} )
	 end
	}}
    end
   }
end

TagFromList = List.toTuple

in

{Roads.registerFunction table Table}
{Roads.run}
```



## PARI/GP


```parigp
html(n=3)={
  print("<table>\n<tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>");
  for(i=1,n,
    print1("<tr><td>"i"</td>");
    for(j=1,3,print1("<td>"random(9999)"</td>"));
    print("</tr>")
  );
  print("</table>")
};
```

```txt
<table>
<tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>
<tr><td>1</td><td>6055</td><td>6794</td><td>6034</td></tr>
<tr><td>2</td><td>8930</td><td>1992</td><td>7087</td></tr>
<tr><td>3</td><td>9592</td><td>5836</td><td>7980</td></tr>
</table>
```


Note also the built-in <code>printtex</code> command, which allows the analogous task to be written as

```parigp
printtex(matrix(4,4,i,j,if(i==1,if(j==1,"",Strchr(86+j)),if(j==1,i,random(9999)))))
```



## Pascal

See [[Create_an_HTML_table#Delphi | Delphi]]


## Peloton

Opcodes of interest: SDC -- simple document; R!I -- ranged random integer


```sgml><@ SDCLIT

	<@ DTBLIT>
		<@ DTRLITLIT>
			<@ DTDLITLIT>|[style]background-color:white</@>
			<@ DTD>X</@>
			<@ DTD>Y</@>
			<@ DTD>Z</@>|[style]width:100%; background-color:brown;color:white; text-align:center</@>
		<@ ITEFORLIT>10|
			<@ DTRLITCAP>
			<@ DTDPOSFORLIT>...|[style]background-color:Brown; color:white; text-align:right</@>
			<@ DTDCAPLIT><@ SAYR!ILI2>1|9999</@>|[style]width:50;text-align:right</@>
			<@ DTDCAPLIT><@ SAYR!ILI2>1|9999</@>|[style]width:50;text-align:right</@>
			<@ DTDCAPLIT><@ SAYR!ILI2>1|9999</@>|[style]width:50;text-align:right</@>
			|[style]background-color:white;color:black</@>
		</@>
	</@>
|Number Table</@>
```



## Perl


```Perl
my @heading = qw(X Y Z);
my $rows = 5;
print   '<table><thead><td>',
        (map { "<th>$_</th>" } @heading),
        "</thead><tbody>";

for (1 .. $rows) {
        print   "<tr><th>$_</th>",
                (map { "<td>".int(rand(10000))."</td>" } @heading),
                "</tr>";
}

print   "</tbody></table>";
```

Note that this is a rather inane way (because of the inane task specification) of generating structured document.  For serious work, one should use a module such as XML or HTML for well-formedness instead of this ad hoc method.


## Perl 6


The below example is kind of boring, and laughably simple. For more interesting/complicated examples see:

[[Show_Ascii_table#Perl_6]] - ''(simple)''

[[Mayan_numerals#Perl_6]] - ''(heavy styling)''

[[Rosetta_Code/Count_examples/Full_list]] - ''(multi-column sortable)''

[[Rosetta_Code/List_authors_of_task_descriptions/Full_list]] - ''(complex nested tables)''

''Note: the above examples are outputting wikitable formatting, not HTML directly. It's pretty much a one-for-one shorthand notation though and the principles and process are the same.


This is certainly not the only or best way to generate HTML tables using Perl 6; just an example of one possible method.


```perl6>my @header =  <  X Y Z
;
my $rows = 5;

sub tag ($tag, $string, $param?) { return "<$tag" ~ ($param ?? " $param" !! '') ~ ">$string" ~ "</$tag>" };

my $table = tag('tr', ( tag('th', $_) for @header));

for 1 .. $rows -> $row {
    $table ~=  tag('tr', ( tag('td', $row, 'align="right"')
    ~ (tag('td', (^10000).pick, 'align="right"') for 1..^@header)));
}

say tag('table', $table, 'cellspacing=4 style="text-align:right; border: 1px solid;"');
```


<table cellspacing=4 style="text-align:right; border: 1px solid;"><tr><th> </th> <th>X</th> <th>Y</th> <th>Z</th></tr><tr><td align="right">1</td><td align="right">2179</td> <td align="right">4778</td> <td align="right">2717</td></tr><tr><td align="right">2</td><td align="right">2160</td> <td align="right">1592</td> <td align="right">4348</td></tr><tr><td align="right">3</td><td align="right">4511</td> <td align="right">540</td> <td align="right">7187</td></tr><tr><td align="right">4</td><td align="right">3484</td> <td align="right">5882</td> <td align="right">1273</td></tr><tr><td align="right">5</td><td align="right">1310</td> <td align="right">4017</td> <td align="right">410</td></tr></table>


## Phix


```Phix
puts(1,"<table border=2>\n")
puts(1,"  <tr><th></th>")
for j=1 to 3 do
    printf(1,"<th>%s</th>",'W'+j)
end for
puts(1,"</tr>\n")
for i=1 to 3 do
    printf(1,"  <tr><td>%d</td>",i)
    for j=1 to 3 do
        printf(1,"<td>%d</td>",rand(10000))
    end for
    puts(1,"</tr>\n")
end for
puts(1,"</table>")
```


<table border=2>
  <tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>
  <tr><td>1</td><td>3287</td><td>6480</td><td>6510</td></tr>
  <tr><td>2</td><td>8500</td><td>1908</td><td>5352</td></tr>
  <tr><td>3</td><td>3287</td><td>6600</td><td>3953</td></tr>
</table>

'''The raw HTML'''

```html5><table border=2

<table>
  <tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>
  <tr><td>1</td><td>3287</td><td>6480</td><td>6510</td></tr>
  <tr><td>2</td><td>8500</td><td>1908</td><td>5352</td></tr>
  <tr><td>3</td><td>3287</td><td>6600</td><td>3953</td></tr>
</table>
```



## PHP



###  normal style


```PHP
<?php
/**
 * @author Elad Yosifon
 * @desc HTML Table - normal style
 */
$cols = array('', 'X', 'Y', 'Z');
$rows = 3;

$html = '<html><body><table><colgroup>';
foreach($cols as $col)
{
	$html .= '<col style="text-align: left;" />';
}
unset($col);
$html .= '</colgroup><thead><tr>';

foreach($cols as $col)
{
	$html .= "<td>{$col}</td>";
}
unset($col);

$html .= '</tr></thead><tbody>';
for($r = 1; $r <= $rows; $r++)
{
	$html .= '<tr>';
	foreach($cols as $key => $col)
	{
		$html .= '<td>' . (($key > 0) ? rand(1, 9999) : $r) . '</td>';
	}
	unset($col);
	$html .= '</tr>';
}
$html .= '</tbody></table></body></html>';

echo $html;

```



###  template engine style


```PHP
<?php
/**
 * @author Elad Yosifon
 * @desc HTML Table - template engine style
 */
$cols = array('', 'X', 'Y', 'Z');
$rows = 3;
?>
<html>
<body>
<table>
	<colgroup>
	<?php foreach($cols as $col):?>
		<col style="text-align: left;" />
	<?php endforeach; unset($col) ?>
	</colgroup>
	<thead>
		<tr>
			<?php foreach($cols as $col): ?>
			<td><?php echo $col?></td>
			<?php endforeach; unset($col)?>
		</tr>
	</thead>
	<tbody>
		<?php for($r = 1; $r <= $rows; $r++): ?>
		<tr>
			<?php foreach($cols as $key => $col): ?>
			<td><?php echo ($key > 0) ? rand(1, 9999) : $r ?></td>
			<?php endforeach; unset($col) ?>
		</tr>
		<?php endfor; ?>
	</tbody>
</table>
</body>
</html>

```



## PicoLisp


```PicoLisp
(load "@lib/xhtml.l")

(<table> NIL NIL '(NIL (NIL "X") (NIL "Y") (NIL "Z"))
   (for N 3
      (<row> NIL N 124 456 789) ) )
```



## PL/I


```PL/I

/* Create an HTML table.  6/2011  */

create: procedure options (main);


create_table: procedure (headings, table_contents);

   declare headings(*) character (10) varying;
   declare table_contents(*, *) fixed;
   declare (i, row, col) fixed;

   put skip edit ('<table>') (a);
   /* Headings. */
   put skip edit ('<tr><th></th> ') (a);
                               /* For an empty column heading */
   do i = 1 to hbound(headings);
      put edit ('<th>', headings(i), '</th> ' ) (a);
   end;
   put edit ('</tr>') (a);

   /* Table contents. */

   do row = 1 to hbound(table_contents, 1);
      /* row number */
      put skip edit ('<tr><td>', row, '</td> ') (a);
      /* row contents */
      do col = 1 to hbound(table_contents, 2);
         put edit ('<td>', table_contents(row, col), '</td> ' ) (a);
      end;
      put edit ('</tr>') (a);
   end;
   put skip edit ('</table>' ) (a);
end create_table;

   declare headings (3) character (1) static initial ('X', 'Y', 'Z');

   declare table_contents(3, 3) fixed static initial (
      4, -3, 8,
      7, 2, -6,
      11, 1, 15);

   call create_table (headings, table_contents);

end create;
```



## PowerShell


```PowerShell

# Converts Microsoft .NET Framework objects into HTML that can be displayed in a Web browser.
ConvertTo-Html -inputobject (Get-Date)

# Create a PowerShell object using a HashTable
$object = [PSCustomObject]@{
        'A'=(Get-Random -Minimum 0 -Maximum 10);
        'B'=(Get-Random -Minimum 0 -Maximum 10);
        'C'=(Get-Random -Minimum 0 -Maximum 10)}

$object | ConvertTo-Html

```

```PowerShell

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<title>HTML TABLE</title>
</head><body>
<table>
<colgroup><col/><col/><col/><col/><col/><col/><col/><col/><col/><col/><col/><col/><col/><col/><col/></colgroup>
<tr><th>DisplayHint</th><th>DateTime</th><th>Date</th><th>Day</th><th>DayOfWeek</th><th>DayOfYear</th><th>Hour</th><th>Kind</th><th>Milliseco
nd</th><th>Minute</th><th>Month</th><th>Second</th><th>Ticks</th><th>TimeOfDay</th><th>Year</th></tr>
<tr><td>DateTime</td><td>Sunday, October 26, 2014 2:32:31 PM</td><td>10/26/2014 12:00:00 AM</td><td>26</td><td>Sunday</td><td>299</td><td>14<
/td><td>Local</td><td>563</td><td>32</td><td>10</td><td>31</td><td>635499307515634638</td><td>14:32:31.5634638</td><td>2014</td></tr>
</table>
</body></html>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<title>HTML TABLE</title>
</head><body>
<table>
<colgroup><col/><col/><col/></colgroup>
<tr><th>C</th><th>B</th><th>A</th></tr>
<tr><td>8</td><td>7</td><td>3</td></tr>
</table>
</body></html>

```


When raw results are exported to Out-File cmdlet like this:

```PowerShell

$object | ConvertTo-Html | Out-File -FilePath $env:temp\test.html ; invoke-item $env:temp\test.html

```


<table>
<tr><th>A</th><th>B</th><th>C</th></tr>
<tr><td>5</td><td>8</td><td>1</td></tr>
</table>


## Prolog


```Prolog

:- use_module(library(http/html_write)).

theader([]) --> []. theader([H|T])  --> html(th(H)), theader(T).
trows([],_) --> []. trows([R|T], N) --> html(tr([td(N),\trow(R)])), { N1 is N + 1 }, trows(T, N1).
trow([])    --> []. trow([E|T])     --> html(td(E)), trow(T).

table :-
	Header = ['X','Y','Z'],
	Rows = [
		[7055,5334,5795],
		[2895,3019,7747],
		[140,7607,8144],
		[7090,475,4140]
	],
	phrase(html(table([tr(\theader(Header)), \trows(Rows,1)])),  Out, []),
	print_html(Out).
```



```html5><table

<tr><th>X</th><th>Y</th><th>Z</th></tr>
<tr><td>1</td><td>7055</td><td>5334</td><td>5795</td></tr>
<tr><td>2</td><td>2895</td><td>3019</td><td>7747</td></tr>
<tr><td>3</td><td>140</td><td>7607</td><td>8144</td></tr>
<tr><td>4</td><td>7090</td><td>475</td><td>4140</td></tr>
</table>
```



## Python


```python

import random

def rand9999():
    return random.randint(1000, 9999)

def tag(attr='', **kwargs):
    for tag, txt in kwargs.items():
        return '<{tag}{attr}>{txt}</{tag}>'.format(**locals())

if __name__ == '__main__':
    header = tag(tr=''.join(tag(th=txt) for txt in ',X,Y,Z'.split(','))) + '\n'
    rows = '\n'.join(tag(tr=''.join(tag(' style="font-weight: bold;"', td=i)
                                    + ''.join(tag(td=rand9999())
                                              for j in range(3))))
                     for i in range(1, 6))
    table = tag(table='\n' + header + rows + '\n')
    print(table)
```


<table>
<tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>
<tr><td style="font-weight: bold;">1</td><td>6040</td><td>4697</td><td>7055</td></tr>
<tr><td style="font-weight: bold;">2</td><td>2525</td><td>5468</td><td>6901</td></tr>
<tr><td style="font-weight: bold;">3</td><td>8851</td><td>3727</td><td>8379</td></tr>
<tr><td style="font-weight: bold;">4</td><td>5313</td><td>4396</td><td>1765</td></tr>
<tr><td style="font-weight: bold;">5</td><td>4013</td><td>5924</td><td>6082</td></tr>
</table>

'''The raw HTML'''

```html5><table

<tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>
<tr><td style="font-weight: bold;">1</td><td>6040</td><td>4697</td><td>7055</td></tr>
<tr><td style="font-weight: bold;">2</td><td>2525</td><td>5468</td><td>6901</td></tr>
<tr><td style="font-weight: bold;">3</td><td>8851</td><td>3727</td><td>8379</td></tr>
<tr><td style="font-weight: bold;">4</td><td>5313</td><td>4396</td><td>1765</td></tr>
<tr><td style="font-weight: bold;">5</td><td>4013</td><td>5924</td><td>6082</td></tr>
</table>
```



Or, folding a general HTML render function over a tree of data:

```Python
from functools import (reduce)
import itertools
import random


# HTML RENDERING ----------------------------------------

# treeHTML :: tree
#      {tag :: String, text :: String, kvs :: Dict}
#      -> HTML String
def treeHTML(tree):
    return foldTree(
        lambda x: lambda xs: (
            f"<{x['tag'] + attribString(x)}>" + (
                str(x['text']) if 'text' in x else '\n'
            ) + ''.join(xs) + f"</{x['tag']}>\n"
        )
    )(tree)


# attribString :: Dict -> String
def attribString(dct):
    kvs = dct['kvs'] if 'kvs' in dct else None
    return ' ' + reduce(
        lambda a, k: a + k + '="' + kvs[k] + '" ',
        kvs.keys(), ''
    ).strip() if kvs else ''


# HTML TABLE FROM GENERATED DATA ------------------------


def main():
    # Number of columns and rows to generate.
    n = 3

    # Table details -------------------------------------
    strCaption = 'Table generated with Python'
    colNames = take(n)(enumFrom('A'))
    dataRows = map(
        lambda x: (x, map(
            lambda _: random.randint(100, 9999),
            colNames
        )), take(n)(enumFrom(1)))
    tableStyle = {
        'style': "width:25%; border:2px solid silver;"
    }
    trStyle = {
        'style': "border:1px solid silver;text-align:right;"
    }

    # TREE STRUCTURE OF TABLE ---------------------------
    tableTree = Node({'tag': 'table', 'kvs': tableStyle})([
        Node({
            'tag': 'caption',
            'text': strCaption
        })([]),

        # HEADER ROW --------------------------------
        (Node({'tag': 'tr'})(
            Node({
                'tag': 'th',
                'kvs': {'style': 'text-align:right;'},
                'text': k
            })([]) for k in ([''] + colNames)
        ))
    ] +
        # DATA ROWS ---------------------------------
        list(Node({'tag': 'tr', 'kvs': trStyle})(
            [Node({'tag': 'th', 'text': tpl[0]})([])] +
            list(Node(
                {'tag': 'td', 'text': str(v)})([]) for v in tpl[1]
            )
        ) for tpl in dataRows)
    )

    print(
        treeHTML(tableTree)
        # dataRows
    )


# GENERIC -----------------------------------------------

# Node :: a -> [Tree a] -> Tree a
def Node(v):
    return lambda xs: {'type': 'Node', 'root': v, 'nest': xs}


# enumFrom :: Enum a => a -> [a]
def enumFrom(x):
    return itertools.count(x) if type(x) is int else (
        map(chr, itertools.count(ord(x)))
    )


# foldTree :: (a -> [b] -> b) -> Tree a -> b
def foldTree(f):
    def go(node):
        return f(node['root'])(
            list(map(go, node['nest']))
        )
    return lambda tree: go(tree)


# take :: Int -> [a] -> [a]
# take :: Int -> String -> String
def take(n):
    return lambda xs: (
        xs[0:n]
        if isinstance(xs, list)
        else list(itertools.islice(xs, n))
    )


if __name__ == '__main__':
    main()
```


<table style="width:25%; border:2px solid silver;">
<caption>Table generated with Python</caption>
<tr>
<th style="text-align:right;"></th>
<th style="text-align:right;">A</th>
<th style="text-align:right;">B</th>
<th style="text-align:right;">C</th>
</tr>
<tr style="border:1px solid silver;text-align:right;">
<th>1</th>
<td>7469</td>
<td>7407</td>
<td>6448</td>
</tr>
<tr style="border:1px solid silver;text-align:right;">
<th>2</th>
<td>4135</td>
<td>3299</td>
<td>6586</td>
</tr>
<tr style="border:1px solid silver;text-align:right;">
<th>3</th>
<td>8264</td>
<td>6951</td>
<td>8882</td>
</tr>
</table>


Raw HTML output

```html5
<table style="width:25%; border:2px solid silver;">
<caption>Table generated with Python</caption>
<tr>
<th style="text-align:right;"></th>
<th style="text-align:right;">A</th>
<th style="text-align:right;">B</th>
<th style="text-align:right;">C</th>
</tr>
<tr style="border:1px solid silver;text-align:right;">
<th>1</th>
<td>7469</td>
<td>7407</td>
<td>6448</td>
</tr>
<tr style="border:1px solid silver;text-align:right;">
<th>2</th>
<td>4135</td>
<td>3299</td>
<td>6586</td>
</tr>
<tr style="border:1px solid silver;text-align:right;">
<th>3</th>
<td>8264</td>
<td>6951</td>
<td>8882</td>
</tr>
</table>
```



## PureBasic

<font face="Courier New">Create an HTML table



</font>


```PureBasic

Title.s="Create an HTML table"

head.s=""
head.s+"<html><head><title>"+Title.s+"</title></head><body>"+chr(13)+chr(10)

tablehead.s
tablehead.s+"<table border=1 cellpadding=10 cellspacing=0>"+chr(13)+chr(10)
tablehead.s+"<tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>"+chr(13)+chr(10)

index=0

tablebody.s=""
for row=1 to 4
index+1
tablebody.s+"<tr><th>"+str(index)+"</th>"
for col=1 to 3
tablebody.s+"<td align="+chr(34)+"right"+chr(34)+">"+str(Random(9999,1))+"</td>"
next
tablebody.s+"</tr>"+chr(13)+chr(10)
next

tablefoot.s=""
tablefoot.s+"</table>"+chr(13)+chr(10)

foot.s=""
foot.s+"</body></html>"+chr(13)+chr(10)

FileName.s="Create_an_HTML_table.html"
If CreateFile(0,FileName.s)
    WriteString(0,head.s)
    WriteString(0,tablehead.s)
    WriteString(0,tablebody.s)
    WriteString(0,tablefoot.s)
    WriteString(0,foot.s)
    CloseFile(0)
    Else
    Debug "Not WriteString :"+FileName.s
EndIf

; RunProgram(FileName.s)

```


<table border=1 cellpadding=10 cellspacing=0>
<tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>
<tr><th>1</th><td align="right">6638</td><td align="right">5838</td><td align="right">5360</td></tr>
<tr><th>2</th><td align="right">2995</td><td align="right">3856</td><td align="right">3093</td></tr>
<tr><th>3</th><td align="right">37</td><td align="right">7644</td><td align="right">812</td></tr>
<tr><th>4</th><td align="right">4428</td><td align="right">1100</td><td align="right">3721</td></tr>
</table>


'''The raw HTML'''


```html5

<html><head><title>Create an HTML table</title></head><body>
<table border=1 cellpadding=10 cellspacing=0>
<tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>
<tr><th>1</th><td align="right">6638</td><td align="right">5838</td><td align="right">5360</td></tr>
<tr><th>2</th><td align="right">2995</td><td align="right">3856</td><td align="right">3093</td></tr>
<tr><th>3</th><td align="right">37</td><td align="right">7644</td><td align="right">812</td></tr>
<tr><th>4</th><td align="right">4428</td><td align="right">1100</td><td align="right">3721</td></tr>
</table>
</body></html>

```



## Racket



```racket

#lang racket

(require xml)

(define xexpr
  `(html
    (head)
    (body
     (table
      (tr (td) (td "X") (td "Y") (td "Z"))
      ,@(for/list ([i (in-range 1 4)])
          `(tr (td ,(~a i))
               (td ,(~a (random 10000)))
               (td ,(~a (random 10000)))
               (td ,(~a (random 10000)))))))))

(display-xml/content (xexpr->xml xexpr))

```



## Rascal


```rascal
import IO;
import util::Math;

str html(str title, str content) = item("html", item("title", title) + item("body", content));
str item(str op, str content) = "\<<op>\><content>\</<op>\>";
str table(str content) = item("table border=\"0\"", content);
str tr(str content) = item("tr", content);
str td(str content) = item("td", content);

public str generateTable(int rows){
	int i(){return arbInt(10000);};
	rows = (tr(td("")+td("X")+td("Y")+td("Z"))
			| it + tr(td("<x>")+td("<i()>")+td("<i()>")+td("<i()>"))
			| x <- [1..rows]);
	writeFile(|file:///location|,
	           html("Rosetta Code Table", table(rows)));
	return "written";
}
```


This will result in a simple html file. For example:


```rascal>rascal
generateTable(10)
str: "written"
```


```html><html><title>Rosetta Code Table</title><body
<table border="0"><tr><td></td><td>X</td><td>Y</td><td>Z</td></tr><tr><td>1</td><td>253</td><td>3988</td><td>3208</td></tr><tr><td>2</td><td>315</td><td>2014</td><td>47</td></tr><tr><td>3</td><td>749</td><td>3379</td><td>1076</td></tr><tr><td>4</td><td>241</td><td>3211</td><td>1848</td></tr><tr><td>5</td><td>1</td><td>1605</td><td>6469</td></tr><tr><td>6</td><td>599</td><td>1243</td><td>1189</td></tr><tr><td>7</td><td>741</td><td>4709</td><td>2854</td></tr><tr><td>8</td><td>918</td><td>482</td><td>7160</td></tr><tr><td>9</td><td>451</td><td>572</td><td>6229</td></tr><tr><td>10</td><td>955</td><td>7970</td><td>9684</td></tr></table border="0"></body></html>
```


[[File:Tableoutput.JPG]]


## Red


```red


Create an HTML table

The table body should have at least three rows of three columns.
Each of these three columns should be labelled "X", "Y", and "Z".
An extra column should be added at either the extreme left or the extreme right
of the table that has no heading, but is filled with sequential row numbers.
The rows of the "X", "Y", and "Z" columns should be filled with random or
sequential integers having 4 digits or less.
The numbers should be aligned in the same fashion for all columns.

Red [
    Problem: %http://www.rosettacode.org/wiki/Create_an_HTML_table
    Code: %https://github.com/metaperl/red-rosetta/blob/master/html-table.r
    Acknowledgements: "@endo64 @toomsav"
]

result: func[][simple-tag "table" trs]

trs: func [][rejoin [
        first-tr
        rand-tr 1
        rand-tr 2
        rand-tr 3
        rand-tr 4
        rand-tr 5
    ]]

table-data: func [][999 + (random 9000)]
rand-td: func [][simple-tag "td" table-data]
rand-tr: func [i][rejoin [
        simple-tag "tr"
            rejoin [(simple-tag "td" i) rand-td rand-td rand-td]
    ]]
first-tr: func[][rejoin [
        simple-tag "tr" rejoin [
            simple-tag "th" ""
            simple-tag "th" "X"
            simple-tag "th" "Y"
            simple-tag "th" "Z"
            ]
    ]]

simple-tag: func [tag contents /attr a][rejoin
    ["<" tag (either attr [rejoin [" " a/1 "=" {"} a/2 {"}]][]) ">"
        newline contents newline "</" tag ">"]]


```



## Retro

Using the '''casket::html'''' library which allows creation of HTML using quotes and combinators:


```Retro
needs casket::html'
with casket::html'

: rnd ( -$ ) random 1000 mod toString ;

[ [ [     ] td [ "x" ] td [ "y" ] td [ "z" ] td ] tr
  [ [ "1" ] td [ rnd ] td [ rnd ] td [ rnd ] td ] tr
  [ [ "2" ] td [ rnd ] td [ rnd ] td [ rnd ] td ] tr
  [ [ "3" ] td [ rnd ] td [ rnd ] td [ rnd ] td ] tr
  [ [ "4" ] td [ rnd ] td [ rnd ] td [ rnd ] td ] tr
  [ [ "5" ] td [ rnd ] td [ rnd ] td [ rnd ] td ] tr
  [ [ "6" ] td [ rnd ] td [ rnd ] td [ rnd ] td ] tr
] table
```



## REXX

The   LINEOUTs   (writes to a file for the various HTML tags)   were broken up into separate pieces which

makes reading the file easier and also helps in debugging,   but they could've been combined into a

single   '''lineout'''   for succinctness.

```rexx
/*REXX program  creates (and displays)  an  HTML table of five rows  and  three columns.*/
parse arg rows .                                 /*obtain optional argument from the CL.*/
if rows=='' | rows==","   then rows=5            /*no ROWS specified?  Then use default.*/
      cols = 3                                   /*specify three columns for the table. */
   maxRand = 9999                                /*4-digit numbers, allows negative nums*/
headerInfo = 'X Y Z'                             /*specifify column header information. */
      oFID = 'a_table.html'                      /*name of the  output  file.           */
         w = 0                                   /*number of writes to the output file. */

call wrt  "<html>"
call wrt  "<head></head>"
call wrt  "<body>"
call wrt  "<table border=5  cellpadding=20  cellspace=0>"

  do r=0  to rows                                /* [↓]  handle row  0 as being special.*/
  if r==0  then call wrt  "<tr><th></th>"        /*when it's     the zeroth row.        */
           else call wrt  "<tr><th>"  r  "</th>" /*  "    "  not  "    "     "          */

      do c=1  for cols                           /* [↓]  for row 0,  add the header info*/
      if r==0  then call wrt  "<th>"   word(headerInfo,c)   "</th>"
               else call wrt  "<td align=right>"    rnd()   "</td>"
      end   /*c*/
  end       /*r*/

call wrt  "</table>"
call wrt  "</body>"
call wrt  "</html>"
say;         say  w    ' records were written to the output file: '   oFID
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
rnd: return right(random(0,maxRand*2)-maxRand,5) /*RANDOM doesn't generate negative ints*/
wrt: call lineout oFID,arg(1);   say '══►'  arg(1);   w=w+1;    return          /*write.*/
```

```txt

══► <html>
══► <head></head>
══► <body>
══► <table border=5  cellpadding=20  cellspace=0>
══► <tr><th></th>
══► <th> X </th>
══► <th> Y </th>
══► <th> Z </th>
══► <tr><th> 1 </th>
══► <td align=right> -9529 </td>
══► <td align=right>  2911 </td>
══► <td align=right> -9855 </td>
══► <tr><th> 2 </th>
══► <td align=right>  8487 </td>
══► <td align=right>  1589 </td>
══► <td align=right>  1795 </td>
══► <tr><th> 3 </th>
══► <td align=right> -9795 </td>
══► <td align=right> -8478 </td>
══► <td align=right> -8716 </td>
══► <tr><th> 4 </th>
══► <td align=right> -1737 </td>
══► <td align=right> -6547 </td>
══► <td align=right>  6988 </td>
══► <tr><th> 5 </th>
══► <td align=right> -5600 </td>
══► <td align=right> -2626 </td>
══► <td align=right> -6062 </td>
══► </table>
══► </body>
══► </html>

31  records were written to the output file:  a_table.html

```


```txt

<html>
<head></head>
<body>
<table border=5  cellpadding=20  cellspace=0>
<tr><th></th>
<th> X </th>
<th> Y </th>
<th> Z </th>
<tr><th> 1 </th>
<td align=right> -1517 </td>
<td align=right>  5513 </td>
<td align=right> -7697 </td>
<tr><th> 2 </th>
<td align=right>  8373 </td>
<td align=right>   142 </td>
<td align=right> -3641 </td>
<tr><th> 3 </th>
<td align=right> -3971 </td>
<td align=right>  -717 </td>
<td align=right>  5390 </td>
<tr><th> 4 </th>
<td align=right>  9727 </td>
<td align=right> -2023 </td>
<td align=right> -2536 </td>
<tr><th> 5 </th>
<td align=right> -6093 </td>
<td align=right> -7179 </td>
<td align=right>   642 </td>
</table>
</body>
</html>

```


<table border=5  cellpadding=20  cellspace=0>
<tr><th></th>
<th> X </th>
<th> Y </th>
<th> Z </th>
<tr><th> 1 </th>
<td align=right> -1517 </td>
<td align=right>  5513 </td>
<td align=right> -7697 </td>
<tr><th> 2 </th>
<td align=right>  8373 </td>
<td align=right>   142 </td>
<td align=right> -3641 </td>
<tr><th> 3 </th>
<td align=right> -3971 </td>
<td align=right>  -717 </td>
<td align=right>  5390 </td>
<tr><th> 4 </th>
<td align=right>  9727 </td>
<td align=right> -2023 </td>
<td align=right> -2536 </td>
<tr><th> 5 </th>
<td align=right> -6093 </td>
<td align=right> -7179 </td>
<td align=right>   642 </td>
</table>





## Ring


```ring

# Project: Create an HTML table

load "stdlib.ring"

str = ""
ncols = 3
nrows = 4

str = str + "<html><head></head><body>" + windowsnl()
str = str + "<table border=1 cellpadding=10 cellspacing=0>" + windowsnl()

for row = 0 to nrows
     if row = 0
        str = str + "<tr><th></th>"
    else
        str = str + "<tr><th>" + row + "</th>"
    ok
    for col = 1 to ncols
         if row = 0
            str = str + "<th>" + char(87 + col) + "</th>"
         else
            str = str + "<td align=" + '"right"' + ">" + random(9999) + "</td>"
         ok
    next
    str = str + windowsnl() + "</tr>" +windowsnl()
next

str = str + "</table>" + windowsnl()
str = str + "</body></html>" + windowsnl()

remove("temp.htm")
write("temp.htm",str)
see str + nl
systemcmd("temp.htm")

```

Output:

```txt

<html><head></head><body>
<table border=1 cellpadding=10 cellspacing=0>
<tr><th></th><th>X</th><th>Y</th><th>Z</th>
</tr>
<tr><th>1</th><td align="right">9151</td><td align="right">1873</td><td align="right">8011</td>
</tr>
<tr><th>2</th><td align="right">9034</td><td align="right">4211</td><td align="right">7883</td>
</tr>
<tr><th>3</th><td align="right">5983</td><td align="right">3772</td><td align="right">501</td>
</tr>
<tr><th>4</th><td align="right">3468</td><td align="right">3931</td><td align="right">3891</td>
</tr>
</table>
</body></html>

```


Output image:

[https://www.dropbox.com/s/1c8svjprod2tqyu/CalmoSoftHTTP.jpg?dl=0 Create an HTML table]


## Ruby


Pure Ruby solution:


```ruby

def r
  rand(10000)
end

STDOUT << "".tap do |html|
  html << "<table>"
  [
    ['X', 'Y', 'Z'],
    [r ,r ,r],
    [r ,r ,r],
    [r ,r ,r],
    [r ,r ,r]

  ].each_with_index do |row, index|
    html << "<tr>"
    html << "<td>#{index > 0 ? index : nil }</td>"
    html << row.map { |e| "<td>#{e}</td>"}.join
    html << "</tr>"
  end

  html << "</table>"
end

```



This creates a plain HTML table, without any CSS to draw borders or to set column widths.

```ruby
def r; rand(10000); end
table = [["", "X", "Y", "Z"],
         [ 1,   r,   r,   r],
         [ 2,   r,   r,   r],
         [ 3,   r,   r,   r]]

require 'rexml/document'

xtable = REXML::Element.new("table")
table.each do |row|
  xrow = REXML::Element.new("tr", xtable)
  row.each do |cell|
    xcell = REXML::Element.new("td", xrow)
    REXML::Text.new(cell.to_s, false, xcell)
  end
end

formatter = REXML::Formatters::Pretty.new
formatter.compact = true
formatter.write(xtable, $stdout)
```


```html5><table

  <tr>
    <td></td>
    <td>X</td>
    <td>Y</td>
    <td>Z</td>
  </tr>
  <tr>
    <td>1</td>
    <td>1358</td>
    <td>6488</td>
    <td>6434</td>
  </tr>
  <tr>
    <td>2</td>
    <td>2477</td>
    <td>6493</td>
    <td>1330</td>
  </tr>
  <tr>
    <td>3</td>
    <td>240</td>
    <td>3038</td>
    <td>9849</td>
  </tr>
</table>
```


<table>
  <tr>
    <td></td>
    <td>X</td>
    <td>Y</td>
    <td>Z</td>
  </tr>
  <tr>
    <td>1</td>
    <td>1358</td>
    <td>6488</td>
    <td>6434</td>
  </tr>
  <tr>
    <td>2</td>
    <td>2477</td>
    <td>6493</td>
    <td>1330</td>
  </tr>
  <tr>
    <td>3</td>
    <td>240</td>
    <td>3038</td>
    <td>9849</td>
  </tr>
</table>


## Run BASIC


```RunBasic
html "<table border=1><tr align=center><td>Row</td><td>X</td><td>Y</td><td>Z</td></tr>"
for i = 1 to 5
  html "<tr align=right>"
  for j = 1 to 4
    if j = 1 then html "<td>";i;"</td>" else html "<td>";i;j;"</td>"
  next j
html "</tr>"
next i
html "</table>"
```


<table border=1><tr align=center><td>Row</td><td>X</td><td>Y</td><td>Z</td></tr>
<tr align=right><td>1</td><td>12</td><td>13</td><td>14</td></tr>
<tr align=right><td>2</td><td>22</td><td>23</td><td>24</td></tr>
<tr align=right><td>3</td><td>32</td><td>33</td><td>34</td></tr>
<tr align=right><td>4</td><td>42</td><td>43</td><td>44</td></tr>
<tr align=right><td>5</td><td>52</td><td>53</td><td>54</td></tr>
</table>


## Rust

```rust
extern crate rand;

use rand::Rng;

fn random_cell<R: Rng>(rng: &mut R) -> u32 {
    // Anything between 0 and 10_000 (exclusive) has 4 digits or fewer. Using `gen_range::<u32>`
    // is faster for smaller RNGs.  Because the parameters are constant, the compiler can do all
    // the range construction at compile time, removing the need for
    // `rand::distributions::range::Range`
    rng.gen_range(0, 10_000)
}

fn main() {
    let mut rng = rand::thread_rng(); // Cache the RNG for reuse

    println!("<table><thead><tr><th></th><td>X</td><td>Y</td><td>Z</td></tr></thead>");

    for row in 0..3 {
        let x = random_cell(&mut rng);
        let y = random_cell(&mut rng);
        let z = random_cell(&mut rng);
        println!("<tr><th>{}</th><td>{}</td><td>{}</td><td>{}</td></tr>", row, x, y, z);
    }

    println!("</table>");
}
```


```html5><table><thead><tr><th></th><td>X</td><td>Y</td><td>Z</td></tr></thead

<tr><th>0</th><td>7101</td><td>9111</td><td>3446</td></tr>
<tr><th>1</th><td>426</td><td>9518</td><td>611</td></tr>
<tr><th>2</th><td>9693</td><td>419</td><td>4878</td></tr>
</table>
```



## Scala

{{libheader|org.scala-lang.xml}}Scala has built-in support for XML, so you can freely mix XML literals into your Scala source code. This is nice, because instead of using strings to represent XML, you create XML literals that the compiler can understand and verify. This approach lets you easily generate dynamic XML by interweaving Scala code and XML in the same expressions.
```scala
object TableGenerator extends App {
  val data = List(List("X", "Y", "Z"), List(11, 12, 13), List(12, 22, 23), List(13, 32, 33))

  def generateTable(data: List[List[Any]]) = {
    <table>
      {data.zipWithIndex.map { case (row, rownum) => (if (rownum == 0) Nil else rownum) +: row}.
      map(row => <tr>
      {row.map(cell =>
        <td>
          {cell}
        </td>)}
    </tr>)}
    </table>
  }

  println(generateTable(data))
}
```



## Scheme

```scheme
(define table #(
                #("" "X" "Y" "Z")
                #(1 1 2 3)
                #(2 4 5 6)
                #(3 7 8 9)))

(display "<table>")
(do ((r 0 (+ r 1))) ((eq? r (vector-length table)))
        (display "<tr>")
        (do ((c 0 (+ c 1))) ((eq? c (vector-length (vector-ref table r))))
                (if (eq? r 0)
                        (display "<th>"))
                (if (> r 0)
                        (display "<td>"))
                (display (vector-ref (vector-ref table r) c))
                (if (eq? r 0)
                        (display "</th>"))
                (if (> r 0)
                        (display "</td>")))
        (display "</tr>"))
(display "</table>")
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: line is 0;
    var integer: column is 0;
  begin
    writeln("<table style=\"text-align:center; border: 1px solid\">");
    writeln("<tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>");
    for line range 1 to 3 do
      write("<tr><th>" <& line <& "</th>");
      for column range 1 to 3 do
        write("<td>" <& rand(0, 9999) <& "</td>");
      end for;
      writeln("</tr>");
    end for;
    writeln("</table>")
  end func;
```


```txt

<table style="text-align:center; border: 1px solid">
<tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>
<tr><th>1</th><td>9682</td><td>2439</td><td>7698</td></tr>
<tr><th>2</th><td>2958</td><td>4336</td><td>8340</td></tr>
<tr><th>3</th><td>6245</td><td>6544</td><td>457</td></tr>
</table>

```


Output viewed with a browser:
<table style="text-align:center; border: 1px solid">
<tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>
<tr><th>1</th><td>9682</td><td>2439</td><td>7698</td></tr>
<tr><th>2</th><td>2958</td><td>4336</td><td>8340</td></tr>
<tr><th>3</th><td>6245</td><td>6544</td><td>457</td></tr>
</table>


## Sidef


```ruby
class HTML {
    method _attr(Hash h) {
        h.keys.sort.map {|k| %Q' #{k}="#{h{k}}"' }.join('')
    }

    method _tag(Hash h, name, value) {
        "<#{name}" + self._attr(h) + '>' + value + "</#{name}>"
    }

    method table(Hash h, *data) { self._tag(h, 'table', data.join('')) }
    method table(*data)         { self.table(Hash(), data...) }
}

class Table < HTML {
    method th(Hash h, value) { self._tag(h, 'th', value) }
    method th(value)         { self.th(Hash(), value) }

    method tr(Hash h, *rows) { self._tag(h, 'tr', rows.join('')) }
    method tr(*rows)         { self.tr(Hash(), rows...) }

    method td(Hash h, value) { self._tag(h, 'td', value) }
    method td(value)         { self.td(Hash(), value) }
}

var header = %w(  X Y Z);
var rows = 5;

var html = HTML.new;
var table = Table.new;

say html.table(
    # attributes
    Hash(
        cellspacing => 4,
        style => "text-align:right; border: 1px solid;"
     ),

    # header
    table.tr(header.map{|elem| table.th(elem)}...),

    # rows
    (1..rows).map { |i|
        table.tr(
            table.td(:(align => 'right'), i),
            (header.len - 1).of {
                table.td(Hash(align => 'right'), 10000.rand.int)
            }...
        )
    }...
);
```


```txt

<table cellspacing="4" style="text-align:right; border: 1px solid;">
<tr><th> </th><th>X</th><th>Y</th><th>Z</th></tr>
<tr>
  <td align="right">1</td>
  <td align="right">2308</td>
  <td align="right">6448</td>
  <td align="right">2614</td>
</tr>
<tr>
  <td align="right">2</td>
  <td align="right">8830</td>
  <td align="right">553</td>
  <td align="right">5647</td>
</tr>
<tr>
  <td align="right">3</td>
  <td align="right">9636</td>
  <td align="right">5922</td>
  <td align="right">6384</td>
</tr>
<tr>
  <td align="right">4</td>
  <td align="right">9122</td>
  <td align="right">4832</td>
  <td align="right">8813</td>
</tr>
<tr>
  <td align="right">5</td>
  <td align="right">3331</td>
  <td align="right">5528</td>
  <td align="right">701</td>
</tr>
</table>

```

(tidied afterwards)


## Standard ML

This implemention is more generic, as it can produce arbitrary tables.
The procedure <code>mkHtmlTable</code> gets:
* a list of rows and columns (both of polymorph types)
* two procedures, which transforms the rows and columns into strings (they are necessarily, because of the polymorphism.)
* the ''values'' procedure, witch generates, given the row and column, the string to place



```sml
(*
 * val mkHtmlTable : ('a list * 'b list) -> ('a -> string * 'b -> string)
 * 			-> (('a * 'b) -> string) -> string
 * The int list is list of colums, the function returns the values
 * at a given colum and row.
 * returns the HTML code of the generated table.
 *)
fun mkHtmlTable (columns, rows) (rowToStr, colToStr) values =
	let
	  val text = ref "<table border=1 cellpadding=10 cellspacing=0>\n<tr><td></td>"
	in
	  (* Add headers *)
	  map (fn colum => text := !text ^ "<th>" ^ (colToStr colum) ^ "</th>") columns;

	  text := !text ^ "</tr>\n";
	  (* Add data rows *)
	  map (fn row =>
		(* row name *)
	  	(text := !text ^ "<tr><th>" ^ (rowToStr row) ^ "</th>";
		(* data *)
		 map (fn col => text := !text ^ "<td>" ^ (values (row, col)) ^ "</td>") columns;
		 text := !text ^ "</tr>\n")
	      ) rows;
	  !text ^ "</table>"
	end

fun mkHtmlWithBody (title, body) = "<html>\n<head>\n<title>" ^ title ^ "</title>\n</head>\n<body>\n" ^ body ^ "\n</body>\n</html>\n"

fun samplePage () = mkHtmlWithBody ("Sample Page",
			mkHtmlTable ([1.0,2.0,3.0,4.0,5.0], [1.0,2.0,3.0,4.0])
			            (Real.toString, Real.toString)
				    (fn (a, b) => Real.toString (Math.pow (a, b))))

val _ = print (samplePage ())
```


```txt

<html>
<head>
<title>Sample Page</title>
</head>
<body>
<table border=1 cellpadding=10 cellspacing=0>
<tr><td></td><th>1</th><th>2</th><th>3</th><th>4</th><th>5</th></tr>
<tr><th>1</th><td>1</td><td>1</td><td>1</td><td>1</td><td>1</td></tr>
<tr><th>2</th><td>2</td><td>4</td><td>8</td><td>16</td><td>32</td></tr>
<tr><th>3</th><td>3</td><td>9</td><td>27</td><td>81</td><td>243</td></tr>
<tr><th>4</th><td>4</td><td>16</td><td>64</td><td>256</td><td>1024</td></tr>
</table>
</body>
</html

```



## Stata

First, a program to write a Stata matrix to an HTML file.


```stata
program mat2html
local nr = rowsof(`1')
local nc = colsof(`1')
local rn `: rownames `1''
local cn `: colnames `1''
tempname f
qui file open `f' using `2', write text replace
file write `f' "<!doctype html>" _n
file write `f' "<html>" _n
file write `f' "<head>" _n
file write `f' `"<meta charset="UTF-8">"' _n
file write `f' "</head>" _n
file write `f' "<body>" _n
file write `f' `"<table border="1">"' _n
* write column names
file write `f' "<tr>" _n
file write `f' "<td></td>" _n
forv j = 1/`nc' {
	local s `: word `j' of `cn''
	file write `f' `"<td>`s'</td>"' _n
}
file write `f' "</tr>" _n
* write row names & data
forv i = 1/`nr' {
	file write `f' "<tr>" _n
	local s `: word `i' of `rn''
	file write `f' `"<td>`s'</td>"' _n
	forv j = 1/`nc' {
		file write `f' `"<td>`=el(`1',`i',`j')'</td>"' _n
	}
	file write `f' "</tr>" _n
}
file write `f' "</table>" _n
file write `f' "</body>" _n
file write `f' "</html>" _n
file close `f'
end
```


An example:


```stata
matrix a=2,9,4\7,5,3\6,1,8
matrix rownames a = A B C
matrix colnames a = X Y Z
mat2html a magic.html
```



## Tcl


```tcl
# Make ourselves a very simple templating lib; just two commands
proc TAG {name args} {
    set body [lindex $args end]
    set result "<$name"
    foreach {t v} [lrange $args 0 end-1] {
	append result " $t=\"" $v "\""
    }
    append result ">" [string trim [uplevel 1 [list subst $body]]] "</$name>"
}
proc FOREACH {var lst str} {
    upvar 1 $var v
    set result {}
    set s [list subst $str]
    foreach v $lst {append result [string trim [uplevel 1 $s]]}
    return $result
}

# Build the data we're displaying
set titles {"" "X" "Y" "Z"}
set data {}
for {set x 0} {$x < 4} {incr x} {
    # Inspired by the Go solution, but with extra arbitrary digits to show 4-char wide values
    lappend data [list \
	    [expr {$x+1}] [expr {$x*3010}] [expr {$x*3+1298}] [expr {$x*2579+2182}]]
}

# Write the table to standard out
puts [TAG table border 1 {
    [TAG tr bgcolor #f0f0f0 {
	[FOREACH head $titles {
	    [TAG th {$head}]
	}]
    }]
    [FOREACH row $data {
	[TAG tr bgcolor #ffffff {
	    [FOREACH col $row {
		[TAG td align right {$col}]
	    }]
	}]
    }]
}]
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
tablefile="table.html"
ERROR/STOP CREATE (tablefile,FDF-o,-std-)
ACCESS d: WRITE/ERASE/RECORDS/utf8 $tablefile s,tablecontent
tablecontent=*
WRITE d "<!DOCTYPE html system>"
WRITE d "<html><head><title>create html table</title></head>"
WRITE d "<body><table><thead align='right'>"
WRITE d "<tr><th> </th><th>x</th><th>y</th><th>z</th></tr>"
WRITE d "</thead>"
WRITE d "<tbody align='right'>"
LOOP n=1,5
x=RANDOM_NUMBERS (1,9999,1)
y=RANDOM_NUMBERS (1,9999,1)
z=RANDOM_NUMBERS (1,9999,1)
WRITE d "<tr><td>{n}</td><td>{x}</td><td>{y}</td><td>{z}</td></tr>"
ENDLOOP
WRITE d "</tbody></table></body></html>"
ENDACCESS d
BROWSE $tablefile

```

<pre style='height:30ex;overflow:scroll'>
<!DOCTYPE html system>
<html><head><title>create html table</title></head>
<body><table><thead align='right'>
<tr><th> </th><th>x</th><th>y</th><th>z</th></tr>
</thead>
<tbody align='right'>
<tr><td>1</td><td>268</td><td>2409</td><td>8627</td></tr>
<tr><td>2</td><td>2095</td><td>1455</td><td>269</td></tr>

<tr><td>3</td><td>3763</td><td>9225</td><td>1957</td></tr>
<tr><td>4</td><td>1304</td><td>9434</td><td>2208</td></tr>
<tr><td>5</td><td>3547</td><td>4051</td><td>4859</td></tr>
</tbody></table></body></html>

```



## UNIX Shell

```bash
function emit_table {
    nameref d=$1
    typeset -i idx=0
    echo "<table>"
    emit_row th "" "${d[idx++][@]}"
    for (( ; idx<${#d[@]}; idx++ )); do
        emit_row td $idx "${d[idx][@]}"
    done
    echo "</table>"
}

function emit_row {
    typeset tag=$1; shift
    typeset row="<tr>"
    for elem; do
        row+=$(printf "<%s>%s</%s>" "$tag" "$elem" "${tag## *}")
    done
    row+="</tr>"
    echo "$row"
}

function addrow {
    nameref d=$1
    typeset n=${#d[@]}
    typeset -i i
    for ((i=0; i<$2; i++)); do
        d[n][i]=$(( $RANDOM % 10000 ))
    done
}

n=3
typeset -a data
data[0]=("X" "Y" "Z")
for i in {1..4}; do
    addrow data $n
done

emit_table data
```

```txt
<table>
<tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>
<tr><td>1</td><td>4988</td><td>1296</td><td>475</td></tr>
<tr><td>2</td><td>6823</td><td>533</td><td>7530</td></tr>
<tr><td>3</td><td>9975</td><td>257</td><td>6030</td></tr>
<tr><td>4</td><td>475</td><td>1720</td><td>9629</td></tr>
</table>
```

<table>
<tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>
<tr><td>1</td><td>4988</td><td>1296</td><td>475</td></tr>
<tr><td>2</td><td>6823</td><td>533</td><td>7530</td></tr>
<tr><td>3</td><td>9975</td><td>257</td><td>6030</td></tr>
<tr><td>4</td><td>475</td><td>1720</td><td>9629</td></tr>
</table>


## Ursa

This program outputs the HTML table to the console.

```ursa
decl ursa.util.random random

out "<table>" endl console

# generate header
out "<tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>" endl console

# generate five rows
decl int i
for (set i 1) (< i 6) (inc i)
        out "<tr><td style=\"font-weight: bold;\">" i "</td>" console
        out "<td>" (int (+ 1000 (random.getint 8999))) "</td>" console
        out "<td>" (int (+ 1000 (random.getint 8999))) "</td>" console
        out "<td>" (int (+ 1000 (random.getint 8999))) "</td>" console
        out "</tr>" endl console
end for

out "</table>" endl console
```



## VBA


```VBA

Public Sub BuildHTMLTable()
'simple HTML table, represented as a string matrix "cells"
Const nRows = 6
Const nCols = 4
Dim cells(1 To nRows, 1 To nCols) As String
Dim HTML As String 'the HTML table
Dim temp As String
Dim attr As String

' fill table
' first row with titles
cells(1, 1) = ""
cells(1, 2) = "X"
cells(1, 3) = "Y"
cells(1, 4) = "Z"
'next rows with index & random numbers
For i = 2 To nRows
  cells(i, 1) = Format$(i - 1)
  For j = 2 To nCols
    cells(i, j) = Format$(Int(Rnd() * 10000))
  Next j
Next i

'build the HTML
HTML = ""
For i = 1 To nRows
  temp = ""
  'first row as header row
  If i = 1 Then attr = "th" Else attr = "td"
  For j = 1 To nCols
    temp = temp & HTMLWrap(cells(i, j), attr)
  Next j
  HTML = HTML & HTMLWrap(temp, "tr")
Next i
HTML = HTMLWrap(HTML, "table", "style=""text-align:center; border: 1px solid""")
Debug.Print HTML
End Sub

Public Function HTMLWrap(s As String, tag As String, ParamArray attributes()) As String
  'returns string s wrapped in HTML tag with optional "attribute=value" strings
  'ex.: HTMLWrap("Link text", "a", "href=""http://www.somesite.org""")
  'returns: <a href="http://www.somesite.org">Link text</a>

  Dim sOpenTag As String
  Dim sClosingTag As String

  sOpenTag = "<" & tag
  For Each attr In attributes
    sOpenTag = sOpenTag & " " & attr
  Next
  sOpenTag = sOpenTag & ">"
  sClosingTag = "</" & tag & ">"
  HTMLWrap = sOpenTag & s & sClosingTag
End Function

```


Subroutine BuildHTMLTable builds the HTML code as one big string.
Sample output of call to BuildHMTLTable:


```txt

<table style="text-align:center; border: 1px solid"><tr><th></th><th>X</th><th>Y</th><th>Z</th></tr><tr><td>1</td><td>1906</td><td>6840</td><td>7474</td></tr><tr><td>2</td><td>6139</td><td>7821</td><td>1617</td></tr><tr><td>3</td><td>8077</td><td>2026</td><td>9567</td></tr><tr><td>4</td><td>658</td><td>615</td><td>7931</td></tr><tr><td>5</td><td>3796</td><td>4635</td><td>1195</td></tr></table>

```

which corresponds to:

<table style="text-align:center; border: 1px solid"><tr><th></th><th>X</th><th>Y</th><th>Z</th></tr><tr><td>1</td><td>1906</td><td>6840</td><td>7474</td></tr><tr><td>2</td><td>6139</td><td>7821</td><td>1617</td></tr><tr><td>3</td><td>8077</td><td>2026</td><td>9567</td></tr><tr><td>4</td><td>658</td><td>615</td><td>7931</td></tr><tr><td>5</td><td>3796</td><td>4635</td><td>1195</td></tr></table>


## VBScript


```vb

Set objFSO = CreateObject("Scripting.FileSystemObject")

'Open the input csv file for reading. The file is in the same folder as the script.
Set objInFile = objFSO.OpenTextFile(objFSO.GetParentFolderName(WScript.ScriptFullName) &_
	"\in.csv",1)

'Create the output html file.
Set objOutHTML = objFSO.OpenTextFile(objFSO.GetParentFolderName(WScript.ScriptFullName) &_
	"\out.html",2,True)

'Write the html opening tags.
objOutHTML.Write "<html><head></head><body>" & vbCrLf

'Declare table properties.
objOutHTML.Write "<table border=1 cellpadding=10 cellspacing=0>" & vbCrLf

'Write column headers.
objOutHTML.Write "<tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>" & vbCrLf

'Go through each line of the input csv file and write to the html output file.
n = 1
Do Until objInFile.AtEndOfStream
	line = objInFile.ReadLine
	If Len(line) > 0 Then
		token = Split(line,",")
		objOutHTML.Write "<tr align=""right""><td>" & n & "</td>"
		For i = 0 To UBound(token)
			objOutHTML.Write "<td>" & token(i) & "</td>"
		Next
		objOutHTML.Write "</tr>" & vbCrLf
	End If
	n = n + 1
Loop

'Write the html closing tags.
objOutHTML.Write "</table></body></html>"

objInFile.Close
objOutHTML.Close
Set objFSO = Nothing

```


```txt

8,1490,9436
555,3300,9766
4982,456,9076
3672,6667,6578

```


```txt

<html><head></head><body>
<table border=1 cellpadding=10 cellspacing=0>
<tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>
<tr align="right"><td>1</td><td>8</td><td>1490</td><td>9436</td></tr>
<tr align="right"><td>2</td><td>555</td><td>3300</td><td>9766</td></tr>
<tr align="right"><td>3</td><td>4982</td><td>456</td><td>9076</td></tr>
<tr align="right"><td>4</td><td>3672</td><td>6667</td><td>6578</td></tr>
</table></body></html>

```

Corresponds to:
<table border=1 cellpadding=10 cellspacing=0>
<tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>
<tr align="right"><td>1</td><td>8</td><td>1490</td><td>9436</td></tr>
<tr align="right"><td>2</td><td>555</td><td>3300</td><td>9766</td></tr>
<tr align="right"><td>3</td><td>4982</td><td>456</td><td>9076</td></tr>
<tr align="right"><td>4</td><td>3672</td><td>6667</td><td>6578</td></tr>
</table>


## Visual Basic .NET

VB.NET has XML literals with embedded expressions (<%= ... %>) similar to [[#Scala|Scala]]. This example additionally uses LINQ query syntax.

```vbnet
Module Program
    Sub Main()
        Const ROWS = 3
        Const COLS = 3

        Dim rand As New Random(0)
        Dim getNumber = Function() rand.Next(10000)

        Dim result =
<table cellspacing="4" style="text-align:right; border:1px solid;">
    <tr>
        <th></th>
        <th>X</th>
        <th>Y</th>
        <th>Z</th>
    </tr>
    <%= From c In Enumerable.Range(1, COLS) Select
        <tr>
            <th><%= c %></th>
            <%= From r In Enumerable.Range(1, ROWS) Select
                <td><%= getNumber() %></td>
            %>
        </tr>
    %>
</table>

        Console.WriteLine(result)
    End Sub
End Module
```


<table cellspacing="4" style="text-align:right; border:1px solid;">
  <tr>
    <th></th>
    <th>X</th>
    <th>Y</th>
    <th>Z</th>
  </tr>
  <tr>
    <th>1</th>
    <td>7262</td>
    <td>8173</td>
    <td>7680</td>
  </tr>
  <tr>
    <th>2</th>
    <td>5581</td>
    <td>2060</td>
    <td>5588</td>
  </tr>
  <tr>
    <th>3</th>
    <td>9060</td>
    <td>4421</td>
    <td>9775</td>
  </tr>
</table>

```txt

<table cellspacing="4" style="text-align:right; border:1px solid;">
  <tr>
    <th></th>
    <th>X</th>
    <th>Y</th>
    <th>Z</th>
  </tr>
  <tr>
    <th>1</th>
    <td>7262</td>
    <td>8173</td>
    <td>7680</td>
  </tr>
  <tr>
    <th>2</th>
    <td>5581</td>
    <td>2060</td>
    <td>5588</td>
  </tr>
  <tr>
    <th>3</th>
    <td>9060</td>
    <td>4421</td>
    <td>9775</td>
  </tr>
</table>

```



### PHP translation

Just for fun, this attempts to emulate the PHP template engine implementation as much as possible.

Since embedded expressions must contain expressions, anonymous iterator functions that are immediately called (notice the parentheses after End Function) are used to enable control constructs.

```vbnet
Module Program
    Sub Main()
        Dim rand As Func(Of Integer, Integer) = AddressOf New Random(0).Next

        Dim cols = {"", "X", "Y", "Z"}
        Dim rows = 3

        Dim result =
<html>
    <body>
        <table>
            <colgroup>
                <%= Iterator Function()
                        For Each col In cols
                            Yield <col style="text-align: left;" />
                        Next
                    End Function() %>
            </colgroup>
            <thead>
                <tr>
                    <%= Iterator Function()
                            For Each col In cols
                                Yield <td><%= col %></td>
                            Next
                        End Function() %>
                </tr>
            </thead>
            <tbody>
                <%= Iterator Function()
                        For r = 1 To rows
                            Yield _
                                <tr>
                                    <%= Iterator Function()
                                            For key = 0 To cols.Length - 1
                                                Dim col = cols(key)
                                                Yield <td><%= If(key > 0, rand(10000), r) %></td>
                                            Next
                                        End Function() %>
                                </tr>
                        Next
                    End Function() %>
            </tbody>
        </table>
    </body>
</html>

        Console.WriteLine(result)
    End Sub
End Module
```


```txt

<html>
  <body>
    <table>
      <colgroup>
        <col style="text-align: left;" />
        <col style="text-align: left;" />
        <col style="text-align: left;" />
        <col style="text-align: left;" />
      </colgroup>
      <thead>
        <tr>
          <th></th>
          <th>X</th>
          <th>Y</th>
          <th>Z</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td>1</td>
          <td>7262</td>
          <td>8173</td>
          <td>7680</td>
        </tr>
        <tr>
          <td>2</td>
          <td>5581</td>
          <td>2060</td>
          <td>5588</td>
        </tr>
        <tr>
          <td>3</td>
          <td>9060</td>
          <td>4421</td>
          <td>9775</td>
        </tr>
      </tbody>
    </table>
  </body>
</html>

```


A more idiomatic version that uses LINQ instead of loops:

```vbnet
Module Program
    Sub Main()
        Dim rand As Func(Of Integer, Integer) = AddressOf New Random(0).Next

        Dim cols = {"", "X", "Y", "Z"}
        Dim rows = 3

        Dim result =
<html>
    <body>
        <table>
            <colgroup>
                <%= cols.Select(Function(__) <col style="text-align: left;"/>) %>
            </colgroup>
            <thead>
                <tr>
                    <%= cols.Select(Function(col) <td><%= col %></td>) %>
                </tr>
            </thead>
            <tbody>
                <%= Enumerable.Range(1, rows).Select(
                    Function(r) _
                        <tr>
                            <%= cols.Select(
                                    Function(col, key) <td><%= If(key > 0, rand(10000), r) %></td>)
                            %>
                        </tr>)
                %>
            </tbody>
        </table>
    </body>
</html>

        Console.WriteLine(result)
    End Sub
End Module
```



## XSLT



```xml
<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <xsl:output method="html" version="4.01" indent="yes"/>

    <!-- Most XSLT processors have some way to supply a different value for this parameter -->
    <xsl:param name="column-count" select="3"/>

    <xsl:template match="/">
        <html>
            <head>
                <title>Rosetta Code: Create an HTML table (XSLT)</title>
            </head>
            <body>
                <xsl:apply-templates/>
            </body>
        </html>
        <xsl:variable name="values" select="/*/*"/>
    </xsl:template>

    <!--
        Rendering HTML from XSLT is so basic as to be trivial. The trickier part of this transform is taking the
        single-column list of numbers in the input and folding it into multiple columns. A common strategy is to only
        apply templates to every Nth value in the list, but then to have that template pull in the skipped values to
        form a row.
    -->

    <xsl:template match="/numbers">
        <table>
            <tr>
                <th/>
                <th>X</th>
                <th>Y</th>
                <th>Z</th>
            </tr>
            <!--
                Here, we have the template applied to every Nth input element rather than every element. In XSLT,
                indices are 1-based, so the start index of every row mod N is 1.
            -->
            <xsl:apply-templates select="number[position() mod $column-count = 1]"/>
        </table>
    </xsl:template>

    <xsl:template match="number">
        <tr>
            <th>
                <xsl:value-of select="position()"/>
            </th>
            <!--
                Here, we compensate for the skipping by including the skipped values in the processing for this value.
            -->
            <xsl:for-each select=". | following-sibling::number[position() &lt; $column-count]">
                <td>
                    <xsl:value-of select="."/>
                </td>
            </xsl:for-each>
        </tr>
    </xsl:template>
</xsl:stylesheet>
```


XSLT does not have a standard PRNG facility, so a list of numbers is given as input.

Sample input:


```xml
<?xml version="1.0" encoding="UTF-8"?>
<numbers>
    <number>1578</number>
    <number>4828</number>
    <number>1154</number>
    <number>4950</number>
    <number>6497</number>
    <number>2355</number>
    <number>9341</number>
    <number>1927</number>
    <number>8720</number>
    <number>4490</number>
    <number>1218</number>
    <number>6675</number>
    <number>8181</number>
    <number>1403</number>
    <number>4637</number>
</numbers>
```


```html
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/1999/REC-html401-19991224/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Rosetta Code: Create an HTML table (XSLT)</title>
</head>
<body><table>
<tr>
<th></th>
<th>X</th>
<th>Y</th>
<th>Z</th>
</tr>
<tr>
<th>1</th>
<td>1578</td>
<td>4828</td>
<td>1154</td>
</tr>
<tr>
<th>2</th>
<td>4950</td>
<td>6497</td>
<td>2355</td>
</tr>
<tr>
<th>3</th>
<td>9341</td>
<td>1927</td>
<td>8720</td>
</tr>
<tr>
<th>4</th>
<td>4490</td>
<td>1218</td>
<td>6675</td>
</tr>
<tr>
<th>5</th>
<td>8181</td>
<td>1403</td>
<td>4637</td>
</tr>
</table></body>
</html>
```


<table>
<tr>
<th></th>
<th>X</th>
<th>Y</th>
<th>Z</th>
</tr>
<tr>
<th>1</th>
<td>1578</td>
<td>4828</td>
<td>1154</td>
</tr>
<tr>
<th>2</th>
<td>4950</td>
<td>6497</td>
<td>2355</td>
</tr>
<tr>
<th>3</th>
<td>9341</td>
<td>1927</td>
<td>8720</td>
</tr>
<tr>
<th>4</th>
<td>4490</td>
<td>1218</td>
<td>6675</td>
</tr>
<tr>
<th>5</th>
<td>8181</td>
<td>1403</td>
<td>4637</td>
</tr>
</table>


## zkl


```zkl
table:=0'|<table style="text-align:center; border: 1px solid">|
         "<th></th><th>X</th><th>Y</th><th>Z</th><tr>";
table=Sink(table);
foreach n in ([1..3]){
   table.write("\n   <tr><th>",n,"</th>");
   foreach n in (3){ table.write("<td>",(0).random(10000),"</td>"); }
   table.write("</tr>");
}
table.write("\n</table>\n").close().print();
```

<table style="text-align:center; border: 1px solid"><th></th><th>X</th><th>Y</th><th>Z</th><tr>
   <tr><th>1</th><td>9342</td><td>6394</td><td>1339</td></tr>
   <tr><th>2</th><td>7963</td><td>4514</td><td>1342</td></tr>
   <tr><th>3</th><td>1937</td><td>8288</td><td>7358</td></tr>
</table>
```txt

<table style="text-align:center; border: 1px solid"><th></th><th>X</th><th>Y</th><th>Z</th><tr>
   <tr><th>1</th><td>9342</td><td>6394</td><td>1339</td></tr>
   <tr><th>2</th><td>7963</td><td>4514</td><td>1342</td></tr>
   <tr><th>3</th><td>1937</td><td>8288</td><td>7358</td></tr>
</table>

```

