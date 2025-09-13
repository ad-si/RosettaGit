+++
title = "CSV to HTML translation"
description = ""
date = 2019-10-08T02:59:02Z
aliases = []
[extra]
id = 8707
[taxonomies]
categories = ["task"]
tags = []
+++

Consider a simplified CSV format where all rows are separated by a newline
and all columns are separated by commas.

No commas are allowed as field data, but the data may contain
other characters and character sequences that would
normally be   ''escaped''   when converted to HTML


## Task

Create a function that takes a string representation of the CSV data
and returns a text string of an HTML table representing the CSV data.

Use the following  data as the CSV text to convert, and show your output.
: Character,Speech
: The multitude,The messiah! Show us the messiah!
: Brians mother,<nowiki><angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry></nowiki>
: The multitude,Who are you?
: Brians mother,I'm his mother; that's who!
: The multitude,Behold his mother! Behold his mother!


;Extra credit:
''Optionally'' allow special formatting for the first row of the table as if it is the tables header row
(via <nowiki><thead></nowiki> preferably; CSS if you must).





## 11l

```11l
V input_csv = ‘Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!’

print("<table>\n<tr><td>", end' ‘’)

L(c) input_csv
   print(S c {
            "\n"{"</td></tr>\n<tr><td>"}
            ‘,’ {‘</td><td>’}
            ‘<’ {‘&lt;’}
            ‘>’ {‘&gt;’}
            ‘&’ {‘&amp;’}
            E   {c}
   }, end' ‘’)

print("</td></tr>\n</table>")
```

```html5><table

<tr><td>Character</td><td>Speech</td></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
```



## Ada

csv2html.adb:

```Ada
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Templates_Parser;

procedure Csv2Html is
   use type Templates_Parser.Vector_Tag;

   Chars : Templates_Parser.Vector_Tag;
   Speeches : Templates_Parser.Vector_Tag;

   CSV_File : Ada.Text_IO.File_Type;
begin
   -- read the csv data
   Ada.Text_IO.Open (File => CSV_File,
                     Mode => Ada.Text_IO.In_File,
                     Name => "data.csv");

   -- fill the tags
   while not Ada.Text_IO.End_Of_File (CSV_File) loop
      declare
         Whole_Line : String := Ada.Text_IO.Get_Line (CSV_File);
         Comma_Pos : Natural := Ada.Strings.Fixed.Index (Whole_Line, ",");
      begin
         Chars := Chars & Whole_Line (Whole_Line'First .. Comma_Pos - 1);
         Speeches := Speeches & Whole_Line (Comma_Pos + 1 .. Whole_Line'Last);
      end;
   end loop;

   Ada.Text_IO.Close (CSV_File);

   -- build translation table and output html
   declare
      Translations : constant Templates_Parser.Translate_Table :=
        (1 => Templates_Parser.Assoc ("CHAR", Chars),
         2 => Templates_Parser.Assoc ("SPEECH", Speeches));
   begin
      Ada.Text_IO.Put_Line
        (Templates_Parser.Parse ("table.tmplt", Translations));
   end;
end Csv2Html;
```


table.tmplt:

```html5><table

@@TABLE@@
   <tr>
      <td>@_WEB_ESCAPE:CHAR_@</td>
      <td>@_WEB_ESCAPE:SPEECH_@</td>
   </tr>
@@END_TABLE@@
</table>
```


```html5><table

   <tr>
      <td>Character</td>
      <td>Speech</td>
   </tr>
   <tr>
      <td>The multitude</td>
      <td>The messiah! Show us the messiah!</td>
   </tr>
   <tr>
      <td>Brians mother</td>
      <td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td>
   </tr>
   <tr>
      <td>The multitude</td>
      <td>Who are you?</td>
   </tr>
   <tr>
      <td>Brians mother</td>
      <td>I'm his mother; that's who!</td>
   </tr>
   <tr>
      <td>The multitude</td>
      <td>Behold his mother! Behold his mother!</td>
   </tr>
</table>
```


## ALGOL 68

```algol68
#!/usr/local/bin/a68g --script #

[6]STRING rows := []STRING(
    "Character,Speech",
    "The multitude,The messiah! Show us the messiah!",
    "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>",
    "The multitude,Who are you?",
    "Brians mother,I'm his mother; that's who!",
    "The multitude,Behold his mother! Behold his mother!"
);

[max abs char]STRING encoded; FOR i TO UPB encoded DO encoded[i]:=REPR i OD;
# encoded[ABS""""] := "&quot;"; optional #
  encoded[ABS "&"] := "&amp;";
  encoded[ABS "<"] := "&lt;";
# encoded[ABS ">"] := "&gt;"; optional #

OP ENCODE = (STRING s)STRING: (
  STRING out := "";
  FOR i TO UPB s DO out+:= encoded[ABS s[i]] OD;
  out
);

PROC head = (STRING title)VOID: (
  printf((
    $"<HEAD>"l$,
      $"<TITLE>"g"</TITLE>"l$, title,
      $"<STYLE type=""text/css"">"l$,
        $"TD {background-color:#ddddff; }"l$,
        $"thead TD {background-color:#ddffdd; text-align:center; }"l$,
      $"</STYLE>"l$,
    $"</HEAD>"l$
  ))
);

# define HTML tags using Algol68's "reverent" block structuring #
PROC html = VOID: print(("<HTML>", new line)),
       body = VOID: print(("<BODY>", new line)),
         table = VOID: print(("<TABLE>", new line)),
           table row = VOID: print(("<TR>")),
             th = (STRING s)VOID: printf(($"<TH>"g"</TH>"$, s)),
             td = (STRING s)VOID: printf(($"<TD>"g"</TD>"$, s)),
           elbat row = VOID: print(("</TR>", new line)),
         elbat = VOID: print(("</TABLE>", new line)),
       ydob = VOID: print(("</BODY>", new line)),
     lmth = VOID: print(("</HTML>", new line));

FILE row input; STRING row; CHAR ifs = ",";
associate(row input, row); make term(row input, ifs);

html;
  head("CSV to HTML translation - Extra Credit");
  body;
    table;
      FOR nr TO UPB rows DO
        row := rows[nr];
        table row;
          on logical file end(row input, (REF FILE row input)BOOL: row end);
          FOR nf DO
            STRING field; get(row input,field);
            (nr=1|th|td)(ENCODE field);
            get(row input, space)
          OD;
          row end: reset(row input);
        elbat row
      OD;
    elbat;
  ydob;
lmth
```

```html5><HTML

<HEAD>
<TITLE>CSV to HTML translation - Extra Credit</TITLE>
<STYLE type="text/css">
TD {background-color:#ddddff; }
thead TD {background-color:#ddffdd; text-align:center; }
</STYLE>
</HEAD>
<BODY>
<TABLE>
<TR><TH>Character</TH><TH>Speech</TH></TR>
<TR><TD>The multitude</TD><TD>The messiah! Show us the messiah!</TD></TR>
<TR><TD>Brians mother</TD><TD>&lt;angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry></TD></TR>
<TR><TD>The multitude</TD><TD>Who are you?</TD></TR>
<TR><TD>Brians mother</TD><TD>I'm his mother; that's who!</TD></TR>
<TR><TD>The multitude</TD><TD>Behold his mother! Behold his mother!</TD></TR>
</TABLE>
</BODY>
</HTML>
```



## AutoHotkey

Very basic implementation

```AutoHotkey
CSVData =
(
Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!
)
TableData := "<table>"
Loop Parse, CSVData,`n
{
   TableData .= "`n  <tr>"
   Loop Parse, A_LoopField, CSV
      TableData .= "<td>" HTMLEncode(A_LoopField) "</td>"
   TableData .= "</tr>"
}
TableData .= "`n</table>"
HTMLEncode(str){
   static rep := "&amp;<lt;>gt;""quot"
   Loop Parse, rep,;
      StringReplace, str, str, % SubStr(A_LoopField, 1, 1), % "&" . SubStr(A_LoopField, 2) . ";", All
   return str
}
MsgBox % clipboard := TableData
```

```txt
<table>
  <tr><td>Character</td><td>Speech</td></tr>
  <tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
  <tr><td>Brians mother</td><td>&amp;lt;angry&amp;gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&amp;lt;/angry&amp;gt;</td></tr>
  <tr><td>The multitude</td><td>Who are you?</td></tr>
  <tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
  <tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
```
(note the output has been modified slightly since this webpage is html.)


## AutoIt


```AutoIt

	Local $ascarray[4] = [34,38,60,62]
	$String = "Character,Speech" & @CRLF
	$String &= "The multitude,The messiah! Show us the messiah!" & @CRLF
	$String &= "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>" & @CRLF
	$String &= "The multitude,Who are you?" & @CRLF
	$String &= "Brians mother,I'm his mother; that's who!" & @CRLF
	$String &= "The multitude,Behold his mother! Behold his mother!"
	For $i = 0 To UBound($ascarray) -1
		$String = Stringreplace($String, chr($ascarray[$i]), "&#"&$ascarray[$i]&";")
	Next
	$newstring = "<table>" & @CRLF
	$crlfsplit = StringSplit($String, @CRLF, 1)
	For $i = 1 To $crlfsplit[0]
		If $i = 1 Then $newstring &= "<thead>" & @CRLF
		$newstring &= "<tr>" & @CRLF
		$komsplit = StringSplit($crlfsplit[$i], ",")
		For $k = 1 To $komsplit[0]
			If $i = 1 Then
				$newstring &= "<th>" &$komsplit[$k] & "</th>" & @CRLF
			Else
				$newstring &= "<td>" &$komsplit[$k] & "</td>" & @CRLF
			EndIf
		Next
		$newstring &= "</tr>" & @CRLF
		If $i = 1 Then $newstring &= "</thead>" & @CRLF
	Next
	$newstring &= "</table>"
	ConsoleWrite('@@ Debug(' & @ScriptLineNumber & ') : $newstring = ' & $newstring & @crlf & '>Error code: ' & @error & @crlf) ;### Debug Console

```


```txt
<table>
<thead>
<tr>
<th>Character</th>
<th>Speech</th>
</tr>
</thead>
<tr>
<td>The multitude</td>
<td>The messiah! Show us the messiah!</td>
</tr>
<tr>
<td>Brians mother</td>
<td>&#60;angry&#62;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&#60;/angry&#62;</td>
</tr>
<tr>
<td>The multitude</td>
<td>Who are you?</td>
</tr>
<tr>
<td>Brians mother</td>
<td>I'm his mother; that's who!</td>
</tr>
<tr>
<td>The multitude</td>
<td>Behold his mother! Behold his mother!</td>
</tr>
</table>
```





## ANTLR


### Java


```java

//  Create an HTML Table from comma seperated values
//  Nigel Galloway - June 2nd., 2013
grammar csv2html;
dialog : {System.out.println("<HTML><Table>");}header body+{System.out.println("</Table></HTML>");} ;
header : {System.out.println("<THEAD align=\"center\"><TR bgcolor=\"blue\">");}row{System.out.println("</TR></THEAD");};
body   : {System.out.println("<TBODY><TR>");}row{System.out.println("</TR></TBODY");};
row    : field ',' field '\r'? '\n';
field  : Field{System.out.println("<TD>" + $Field.text.replace("<","&lt;").replace(">","&gt;") + "</TD>");};
Field  : ~[,\n\r]+;

```



## AWK

Includes extra credit.<br />
File csv2html.awk

```awk
#!/usr/bin/awk -f
BEGIN {
        FS=","
        print "<table>"
}

{
        gsub(/</, "\\&lt;")
        gsub(/>/, "\\&gt;")
        gsub(/&/, "\\&gt;")
        print "\t<tr>"
        for(f = 1; f <= NF; f++)  {
                if(NR == 1 && header) {
                        printf "\t\t<th>%s</th>\n", $f
                }
                else printf "\t\t<td>%s</td>\n", $f
        }
        print "\t</tr>"
}

END {
        print "</table>"
}

```



```txt
$ awk -f csv2html.awk input.csv
```


```html5><table

        <tr>
                <td>Character</td>
                <td>Speech</td>
        </tr>
        <tr>
                <td>The multitude</td>
                <td>The messiah! Show us the messiah!</td>
        </tr>
        <tr>
                <td>Brians mother</td>
                <td>&gt;lt;angry&gt;gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&gt;lt;/angry&gt;gt;</td>
        </tr>
        <tr>
                <td>The multitude</td>
                <td>Who are you?</td>
        </tr>
        <tr>
                <td>Brians mother</td>
                <td>I'm his mother; that's who!</td>
        </tr>
        <tr>
                <td>The multitude</td>
                <td>Behold his mother! Behold his mother!</td>
        </tr>
</table>
```


Extra credit:


```txt
$ awk -v header=1 -f csv2html.awk input.csv
```


```html5><table

        <tr>
                <th>Character</th>
                <th>Speech</th>
        </tr>
        <tr>
                <td>The multitude</td>
                <td>The messiah! Show us the messiah!</td>
        </tr>
        <tr>
                <td>Brians mother</td>
                <td>&gt;lt;angry&gt;gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&gt;lt;/angry&gt;gt;</td>
        </tr>
        <tr>
                <td>The multitude</td>
                <td>Who are you?</td>
        </tr>
        <tr>
                <td>Brians mother</td>
                <td>I'm his mother; that's who!</td>
        </tr>
        <tr>
                <td>The multitude</td>
                <td>Behold his mother! Behold his mother!</td>
        </tr>
</table>
```



## Batch File


```dos
::Batch Files are terrifying when it comes to string processing.
::But well, a decent implementation!
@echo off

REM Below is the CSV data to be converted.
REM Exactly three colons must be put before the actual line.

:::Character,Speech
:::The multitude,The messiah! Show us the messiah!
:::Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
:::The multitude,Who are you?
:::Brians mother,I'm his mother; that's who!
:::The multitude,Behold his mother! Behold his mother!

setlocal disabledelayedexpansion
echo ^<table^>
for /f "delims=" %%A in ('findstr "^:::" "%~f0"') do (
   set "var=%%A"
   setlocal enabledelayedexpansion
      REM The next command removes the three colons...
      set "var=!var:~3!"

      REM The following commands to the substitions per line...
      set "var=!var:&=&amp;!"
      set "var=!var:<=&lt;!"
      set "var=!var:>=&gt;!"
      set "var=!var:,=</td><td>!"

      echo ^<tr^>^<td^>!var!^</td^>^</tr^>
   endlocal
)
echo ^</table^>
```

```html5><table

<tr><td>Character</td><td>Speech</td></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
```



## BBC BASIC

```bbcbasic
      DATA "Character,Speech"
      DATA "The multitude,The messiah! Show us the messiah!"
      DATA "Brian's mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>"
      DATA "The multitude,Who are you?"
      DATA "Brian's mother,I'm his mother; that's who!"
      DATA "The multitude,Behold his mother! Behold his mother!"
      DATA "***"

      *SPOOL CSVtoHTML.htm
      PRINT "<HTML>"
      PRINT "<HEAD>"
      PRINT "</HEAD>"
      PRINT "<BODY>"
      PRINT "<table border=1 cellpadding =10 cellspacing=0>"

      header% = TRUE
      REPEAT
        READ csv$
        IF csv$ = "***" THEN EXIT REPEAT

        IF header% PRINT "<tr><th>"; ELSE PRINT "<tr><td>";
        FOR i% = 1 TO LEN(csv$)
          c$ = MID$(csv$, i%, 1)
          CASE c$ OF
            WHEN ",": IF header% PRINT "</th><th>"; ELSE PRINT "</td><td>";
            WHEN "<": PRINT "&lt;";
            WHEN ">": PRINT "&gt;";
            WHEN "&": PRINT "&amp;";
            OTHERWISE: PRINT c$;
          ENDCASE
        NEXT i%
        IF header% PRINT "</th></tr>" ELSE PRINT "</td></tr>"

        header% = FALSE
      UNTIL FALSE

      PRINT "</table>"
      PRINT "</BODY>"
      PRINT "</HTML>"
      *spool

      SYS "ShellExecute", @hwnd%, 0, "CSVtoHTML.htm", 0, 0, 1

```

```txt
<HTML>
<HEAD>
</HEAD>
<BODY>
<table border=1 cellpadding =10 cellspacing=0>
<tr><th>Character</th><th>Speech</th></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brian's mother</td><td>&amp;lt;angry&amp;gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&amp;lt;/angry&amp;gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brian's mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
</BODY>
</HTML>
```


[[File:CSVHTML_BBC.gif]]


## Bracmat


### Extra credit solution using pattern matching

This is not the most concise solution, but it is relatively efficient. To collect the lines we use a pattern that matches a line starting from position <code>[!p</code>. Each time a line is matched, <code>p</code> is updated, the two found elements are collected and the pattern is forced to fail, so the pattern matcher finds the next line. The found rows are collected in reverse order, because prepending to a list is faster than appending. When all lines are read, the collected lines are reversed, interspersed with newline characters. Finally the predefined function toML is used to create HTML.

```bracmat
( ( CSVtoHTML
  =   p q Character Speech swor rows row
    .   0:?p
      & :?swor:?rows
      & ( @( !arg
           :   ?
               ( [!p ?Character "," ?Speech \n [?q ?
               & !q:?p
               &     (tr.,(th.,!Character) (th.,!Speech))
                     !swor
                 : ?swor
               & ~
               )
           )
        |     whl
            ' ( !swor:%?row %?swor
              & !row \n !rows:?rows
              )
          &   toML
            $ (table.,(thead.,!swor) \n (tbody.,!rows))
        )
  )
&   CSVtoHTML
  $ "Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!
"
)

```

Output:

```html><table><thead><tr><th>Character</th><th>Speech</th></tr></thead

<tbody><tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</tbody></table>
```



### Simple solution NOT using pattern matching

Newer versions of Bracmat have the built in function <code>vap</code> that splits an input string in
single characters or that splits it everywhere where a given separator character occurs. Each single character or slab of characters is
passed to a function. The values returned from this function become the elements in a list that is returned from the <code>vap</code> function. For example, <code>vap$(upp.Википедию)</code> "vaporizes" the word Википедию into characters and produces the list of
uppercased characters В И К И П Е Д И Ю. Instead of the name of a function we can choose to just give <code>vap</code> the definition of an anonymous function, like so: <code>vap$((=.!arg:~и|).Википедию)</code>. This returns a list of the characters in the word Википедию, except for the и character: В к п е д ю.

In the code below, we use <code>vap</code> with a third argument, a splitting separator character. The outer call to <code>vap</code> splits a text into rows. An embedded call to <code>vap</code> splits each row into cell elements. This code is very efficient.

```Bracmat
( ( Csv2Html
  =
    .   toML
      $ ( table
        .
          ,   vap
            $ ( (
                = .tr.,vap$((=.td.,!arg).!arg.",")
                )
              . !arg
              . \n
              )
        )
  )
&   Csv2Html
  $ "Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!
"
)
```



### Extra credit solution


```Bracmat
( ( Csv2Html
  =
    .   toML
      $ ( table
        .
          ,       vap
                $ ( (=..vap$((=.,!arg).!arg.","))
                  . !arg
                  . \n
                  )
              : (.%?header) ?body
            &   ( thead
                .
                  ,   (tr.,map$((=.th.!arg).!header))
                      \n
                )
                ( tbody
                .
                  ,   map
                    $ ( (
                        =
                          .   !arg:(.?arg)
                            &   (tr.,map$((=.td.!arg).!arg))
                                \n
                        )
                      . !body
                      )
                )
        )
  )
&   Csv2Html
  $ "Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!
"
)
```


Output:

```html

<table><thead><tr><th>Character</th><th>Speech</th></tr>
</thead><tbody><tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
<tr><td /></tr>
</tbody></table>
```



## Befunge

The most practical solution for Befunge was to read the CSV from stdin, so this implementation works best with command line interpreters that can accept redirected input. That said, it is still possible test with many of the GUI and online interpretors just by entering the input manually.

Note that right angle brackets are deliberately not escaped, since that is not strictly necessary for the markup to be valid.


```Befunge
<v_>#!,#:<     "<table>"     \0 +55
 v >0>::65*1+`\"~"`!*#v_4-5v  >
v>#^~^<v"<tr><td>" <  \v-1/<>">elb"
<^     >:#,_$10    |!:<>\#v_ vv"ta"
v-",":\-"&":\-"<":\<>5#05#<v+ >"/"v
>#v_$$$0">dt<>dt/<"vv"tr>"+<5 v"<"<
>^>\#v_$$0";pma&"  v>"/<>d"v5 v , <
$    > \#v_$0";tl&"v v"</t"<0 > : |
^_>#!,#:<>#<0#<\#<<< >:#,_$#^_v@ $<
```

```html5><table

<tr><td>Character</td><td>Speech</td></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry></td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
```



## C



```c
#include <stdio.h>

const char *input =
	"Character,Speech\n"
	"The multitude,The messiah! Show us the messiah!\n"
	"Brians mother,<angry>Now you listen here! He's not the messiah; "
		"he's a very naughty boy! Now go away!</angry>\n"
	"The multitude,Who are you?\n"
	"Brians mother,I'm his mother; that's who!\n"
	"The multitude,Behold his mother! Behold his mother!";

int main()
{
	const char *s;
	printf("<table>\n<tr><td>");
	for (s = input; *s; s++) {
		switch(*s) {
		case '\n': printf("</td></tr>\n<tr><td>"); break;
		case ',':  printf("</td><td>"); break;
		case '<':  printf("&lt;"); break;
		case '>':  printf("&gt;"); break;
		case '&':  printf("&amp;"); break;
		default:   putchar(*s);
		}
	}
	puts("</td></tr>\n</table>");

	return 0;
}
```


```txt
$ gcc -Wall -W -ansi -pedantic csv.c -o csv
$ ./csv
```



```html5><table

<tr><td>Character</td><td>Speech</td></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
```



## C++


```cpp
#include <string>
#include <boost/regex.hpp>
#include <iostream>

std::string csvToHTML( const std::string & ) ;

int main( ) {
   std::string text = "Character,Speech\n"
                            "The multitude,The messiah! Show us the messiah!\n"
			    "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n"
	                    "The multitude,Who are you?\n"
		            "Brians mother,I'm his mother; that's who!\n"
		            "The multitude,Behold his mother! Behold his mother!\n" ;
  std::cout << csvToHTML( text ) ;
  return 0 ;
}

std::string csvToHTML( const std::string & csvtext ) {
   //the order of the regexes and the replacements is decisive!
   std::string regexes[ 5 ] = { "<" , ">" , "^(.+?)\\b" , "," , "\n" } ;
   const char* replacements [ 5 ] = { "&lt;" , "&gt;" , "    <TR><TD>$1" , "</TD><TD>", "</TD></TR>\n"  } ;
   boost::regex e1( regexes[ 0 ] ) ;
   std::string tabletext = boost::regex_replace( csvtext , e1 ,
     replacements[ 0 ] , boost::match_default | boost::format_all ) ;
   for ( int i = 1 ; i < 5 ; i++ ) {
      e1.assign( regexes[ i ] ) ;
      tabletext = boost::regex_replace( tabletext , e1 , replacements[ i ] , boost::match_default | boost::format_all ) ;
   }
   tabletext = std::string( "<TABLE>\n" ) + tabletext ;
   tabletext.append( "</TABLE>\n" ) ;
   return tabletext ;
}
```

```html5

<TABLE>
    <TR><TD>Character</TD><TD>Speech</TD></TR>
    <TR><TD>The multitude</TD><TD>The messiah! Show us the messiah!</TD></TR>
    <TR><TD>Brians mother</TD><TD>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</TD></TR>
    <TR><TD>The multitude</TD><TD>Who are you?</TD></TR>
    <TR><TD>Brians mother</TD><TD>I'm his mother; that's who!</TD></TR>
    <TR><TD>The multitude</TD><TD>Behold his mother! Behold his mother!</TD></TR>
</TABLE>

```



## C#



### Simple Solution



```C sharp

using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;

    class Program
    {
        private static string ConvertCsvToHtmlTable(string csvText)
        {
            //split the CSV, assume no commas or line breaks in text
            List<List<string>> splitString = new List<List<string>>();
            List<string> lineSplit = csvText.Split('\n').ToList();
            foreach (string line in lineSplit)
            {
                splitString.Add(line.Split(',').ToList());
            }

            //encode text safely, and create table
            string tableResult = "<table>";
            foreach(List<string> splitLine in splitString)
            {
                tableResult += "<tr>";
                foreach(string splitText in splitLine)
                {
                    tableResult += "<td>" + WebUtility.HtmlEncode(splitText) + "</td>";
                }
                tableResult += "</tr>";
            }
            tableResult += "</table>";
            return tableResult;
        }
    }

```


{{out}} when using the text suggested:

```html5

<table><tr><td>Character</td><td>Speech</td></tr><tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr><tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He&#39;s not the messiah; he&#39;s a very naughty boy! Now go away!&lt;/angry&gt;</td></tr><tr><td>The multitude</td><td>Who are you?</td></tr><tr><td>Brians mother</td><td>I&#39;m his mother; that&#39;s who!</td></tr><tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr></table>

```



### Extra Credit Solution



```C sharp
using System;
using System.Linq;
using System.Net;

namespace CsvToHtml
{
    class Program
    {
        static void Main(string[] args)
        {
            string csv =
                @"Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!";

            Console.Write(ConvertCsvToHtmlTable(csv, true));
        }

        private static string ConvertCsvToHtmlTable(string csvText, bool formatHeaders)
        {
            var rows =
                (from text in csvText.Split(new[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries) /* Split the string by newline,
                                                                                                          * removing any empty rows. */
                 select text.Split(',')).ToArray(); // Split each row by comma.

            string output = "<table>"; // Initialize the output with the value of "<table>".

            for (int index = 0; index < rows.Length; index++) // Iterate through each row.
            {
                var row = rows[index];
                var tag = (index == 0 && formatHeaders) ? "th" : "td"; /* Check if this is the first row, and if to format headers.
                                                                        * If so, then set the tags as table headers.
                                                                        * Otherwise, set the tags as table data. */

                output += "\r\n\t<tr>"; // Add table row tag to output string.

                // Add escaped cell data with proper tags to output string for each cell in row.
                output = row.Aggregate(output,
                                       (current, cell) =>
                                       current +
                                       string.Format("\r\n\t\t<{0}>{1}</{0}>", tag, WebUtility.HtmlEncode(cell)));

                output += "\r\n\t</tr>"; // Add closing table row tag to output string.
            }

            output += "\r\n</table>"; // Add closing table tag to output string.

            return output;
        }
    }
}
```


```html5><table

	<tr>
		<th>Character</th>
		<th>Speech</th>
	</tr>
	<tr>
		<td>The multitude</td>
		<td>The messiah! Show us the messiah!</td>
	</tr>
	<tr>
		<td>Brians mother</td>
		<td>&lt;angry&gt;Now you listen here! He&#39;s not the messiah; he&#39;s a very naughty boy! Now go away!&lt;/angry&gt;</td>
	</tr>
	<tr>
		<td>The multitude</td>
		<td>Who are you?</td>
	</tr>
	<tr>
		<td>Brians mother</td>
		<td>I&#39;m his mother; that&#39;s who!</td>
	</tr>
	<tr>
		<td>The multitude</td>
		<td>Behold his mother! Behold his mother!</td>
	</tr>
</table>
```



## CoffeeScript


```coffeescript
String::__defineGetter__ 'escaped', () ->
	this.replace(/&/g, '&amp;')
	    .replace(/</g, '&lt;')
	    .replace(/>/g, '&gt;')
	    .replace(/"/g, '&quot;') // rosettacode doesn't like "

text = '''
Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!
'''

lines = (line.split ',' for line in text.split /[\n\r]+/g)

header = lines.shift()

console.log """
<table cellspacing="0">
	<thead>
		<th scope="col">#{header[0]}</th>
		<th scope="col">#{header[1]}</th>
	</thead>
	<tbody>
"""

for line in lines
	[character, speech] = line
	console.log """
		<th scope="row">#{character}</th>
		<td>#{speech.escaped}</td>
	"""

console.log """
	</tbody>
</table>
	"""
```


```html5
<table cellspacing="0">
  <thead>
    <th scope="col">Character</th>
    <th scope="col">Speech</th>
  </thead>
  <tbody>
    <th scope="row">The multitude</th>
    <td>The messiah! Show us the messiah!</td>
    <th scope="row">Brians mother</th>
    <td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td>
    <th scope="row">The multitude</th>
    <td>Who are you?</td>
    <th scope="row">Brians mother</th>
    <td>I'm his mother; that's who!</td>
    <th scope="row">The multitude</th>
    <td>Behold his mother! Behold his mother!</td>
  </tbody>
</table>
```




## Clojure


We assume the presence of a file, but the input could come from anywhere.


```csv

Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!

```



```clojure

(require 'clojure.string)

(def escapes
     {\< "&lt;", \> "&gt;", \& "&amp;"})

(defn escape
      [content]
      (clojure.string/escape content escapes))

(defn tr
      [cells]
      (format "<tr>%s</tr>"
              (apply str (map #(str "<td>" (escape %) "</td>") cells))))

;; turn a seq of seq of cells into a string.
(defn to-html
      [tbl]
      (format "<table><tbody>%s</tbody></thead>"
              (apply str (map tr tbl))))

;; Read from a string to a seq of seq of cells.
(defn from-csv
      [text]
      (map #(clojure.string/split % #",")
            (clojure.string/split-lines text)))

(defn -main
      []
      (let [lines (line-seq (java.io.BufferedReader. *in*))
            tbl (map #(clojure.string/split % #",") lines)]
           (println (to-html tbl)))

```


```html

<table><tbody><tr><td>Character</td><td>Speech</td></tr><tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr><tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr><tr><td>The multitude</td><td>Who are you?</td></tr><tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr><tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr></tbody></thead>

```



## Common Lisp



```lisp
(defvar *csv* "Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!")

(defun split-string (string delim-char)
  (let ((result '()))
    (do* ((start 0 (1+ end))
	  (end (position delim-char string)
	       (position delim-char string :start start)))
	 ((not end) (reverse (cons (subseq string start) result)))
      (push (subseq string start end) result))))

;;; HTML escape code modified from:
;;; http://www.gigamonkeys.com/book/practical-an-html-generation-library-the-interpreter.html

(defun escape-char (char)
  (case char
    (#\& "&amp;")
    (#\< "&lt;")
    (#\> "&gt;")
    (t (format nil "&#~d;" (char-code char)))))

(defun escape (in)
  (let ((to-escape "<>&"))
    (flet ((needs-escape-p (char) (find char to-escape)))
      (with-output-to-string (out)
	(loop for start = 0 then (1+ pos)
	   for pos = (position-if #'needs-escape-p in :start start)
	   do (write-sequence in out :start start :end pos)
	   when pos do (write-sequence (escape-char (char in pos)) out)
	   while pos)))))

(defun html-row (values headerp)
  (let ((tag (if headerp "th" "td")))
    (with-output-to-string (out)
      (write-string "<tr>" out)
      (dolist (val values)
	(format out "<~A>~A</~A>" tag (escape val) tag))
      (write-string "</tr>" out))))

(defun csv->html (csv)
  (let* ((lines (split-string csv #\Newline))
	 (cols  (split-string (first lines) #\,))
	 (rows  (mapcar (lambda (row) (split-string row #\,)) (rest lines))))
    (with-output-to-string (html)
      (format html "<table>~C" #\Newline)
      (format html "~C~A~C" #\Tab (html-row cols t) #\Newline)
      (dolist (row rows)
	(format html "~C~A~C" #\Tab (html-row row nil) #\Newline))
      (write-string "</table>" html))))
```



```txt
CL-USER> (csv->html *csv*)
```


```html5><table

	<tr><th>Character</th><th>Speech</th></tr>
	<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
	<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
	<tr><td>The multitude</td><td>Who are you?</td></tr>
	<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
	<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
```



## D

```d
void main() {
    import std.stdio;

    immutable input =
        "Character,Speech\n" ~
        "The multitude,The messiah! Show us the messiah!\n" ~
        "Brians mother,<angry>Now you listen here! He's not the messiah; " ~
            "he's a very naughty boy! Now go away!</angry>\n" ~
        "The multitude,Who are you?\n" ~
        "Brians mother,I'm his mother; that's who!\n" ~
        "The multitude,Behold his mother! Behold his mother!";

    "<html>\n<head><meta charset=\"utf-8\"></head>\n<body>\n\n".write;
    "<table border=\"1\" cellpadding=\"5\" cellspacing=\"0\">\n<thead>\n  <tr><td>".write;

    bool theadDone = false;

    foreach (immutable c; input) {
        switch(c) {
            case '\n':
                if (theadDone) {
                    "</td></tr>\n  <tr><td>".write;
                } else {
                    "</td></tr>\n</thead>\n<tbody>\n  <tr><td>".write;
                    theadDone = true;
                }
                break;
            case ',':  "</td><td>".write; break;
            case '<':  "&lt;".write;      break;
            case '>':  "&gt;".write;      break;
            case '&':  "&amp;".write;     break;
            default:   c.write;           break;
        }
    }

    "</td></tr>\n</tbody>\n</table>\n\n</body></html>".write;
}
```

```html5><html

<head><meta charset="utf-8"></head>
<body>

<table border="1" cellpadding="5" cellspacing="0">
<thead>
  <tr><td>Character</td><td>Speech</td></tr>
</thead>
<tbody>
  <tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
  <tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
  <tr><td>The multitude</td><td>Who are you?</td></tr>
  <tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
  <tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</tbody>
</table>

</body>
</html>
```



## Delphi


This solution solves both the basic and extra credit tasks.


```Delphi
program csv2html;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes;

const
  // Carriage Return/Line Feed
  CRLF    = #13#10;

  // The CSV data
  csvData =
  'Character,Speech'+CRLF+
  'The multitude,The messiah! Show us the messiah!'+CRLF+
  'Brians mother,<angry>Now you listen here! He''s not the messiah; he''s a very naughty boy! Now go away!</angry>'+CRLF+
  'The multitude,Who are you?'+CRLF+
  'Brians mother,I''m his mother; that''s who!'+CRLF+
  'The multitude,Behold his mother! Behold his mother!';

  // HTML header
  htmlHead =
  '<!DOCTYPE html'+CRLF+
  'PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"'+CRLF+
  '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">'+CRLF+
  '<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">'+CRLF+
  '<head>'+CRLF+
  '<meta http-equiv="content-type" content="text/html; charset=ISO-8859-1" />'+CRLF+
  '<title>CSV-to-HTML Conversion</title>'+CRLF+
  '<style type="text/css">'+CRLF+
  'body {font-family:verdana,helvetica,sans-serif;font-size:100%}'+CRLF+
  'table {width:70%;border:0;font-size:80%;margin:auto}'+CRLF+
  'th,td {padding:4px}'+CRLF+
  'th {text-align:left;background-color:#eee}'+CRLF+
  'th.c {width:15%}'+CRLF+
  'td.c {width:15%}'+CRLF+
  '</style>'+CRLF+
  '</head>'+CRLF+
  '<body>'+CRLF;

  // HTML footer
  htmlFoot =
  '</body>'+CRLF+
  '</html>';

{ Function to split a string into a list using a given delimiter }
procedure SplitString(S, Delim: string; Rslt: TStrings);
var
  i: integer;
  fld: string;
begin
  fld := '';

  for i := Length(S) downto 1 do
    begin
      if S[i] = Delim then
        begin
          Rslt.Insert(0,fld);
          fld := '';
        end
        else
         fld := S[i]+fld;
    end;

  if (fld <> '') then
      Rslt.Insert(0,fld);
end;

{ Simple CSV parser with option to specify that the first row is a header row }
procedure ParseCSV(const csvIn: string; htmlOut: TStrings; FirstRowIsHeader: Boolean = True);
const
  rowstart      = '<tr><td class="c">';
  rowend        = '</td></tr>';
  cellendstart  = '</td><td class="s">';
  hcellendstart = '</th><th class="s">';
  hrowstart     = '<tr><th class="c">';
  hrowend       = '</th></tr>';
var
  tmp,pieces: TStrings;
  i: Integer;
begin
  // HTML header
  htmlOut.Text := htmlHead + CRLF + CRLF;

  // Start the HTML table
  htmlOut.Text := htmlOut.Text + '<table summary="csv2table conversion">'  + CRLF;

  // Create stringlist
  tmp := TStringList.Create;
  try
    // Assign CSV data to stringlist and fix occurences of '<' and '>'
    tmp.Text := StringReplace(csvIn,'<','&lt;',[rfReplaceAll]);
    tmp.Text := StringReplace(tmp.Text,'>','&gt;',[rfReplaceAll]);

    // Create stringlist to hold the parts of the split data
    pieces := TStringList.Create;
    try

      // Loop through the CSV rows
      for i := 0 to Pred(tmp.Count) do
        begin
          // Split the current row
          SplitString(tmp[i],',',pieces);

          // Check if first row and FirstRowIsHeader flag set
          if (i = 0) and FirstRowIsHeader then

            // Render HTML
            htmlOut.Text := htmlOut.Text + hrowstart + pieces[0] + hcellendstart + pieces[1] + hrowend + CRLF
            else
            htmlOut.Text := htmlOut.Text + rowstart + pieces[0] + cellendstart + pieces[1] + rowend + CRLF;

        end;

      // Finish the HTML table and end the HTML page
      htmlOut.Text := htmlOut.Text + '</table>' + CRLF + htmlFoot;

    finally
      pieces.Free;
    end;

  finally
    tmp.Free;
  end;
end;

var
  HTML: TStrings;

begin
  // Create stringlist to hold HTML output
  HTML := TStringList.Create;
  try
    Writeln('Basic:');
    Writeln('');

    // Load and parse the CSV data
    ParseCSV(csvData,HTML,False);

    // Output the HTML to the console
    Writeln(HTML.Text);

    // Save the HTML to a file (in application's folder)
    HTML.SaveToFile('csv2html_basic.html');

    Writeln('');
    Writeln('
### ===============================
');
    Writeln('');

    HTML.Clear;

    Writeln('Extra Credit:');
    Writeln('');

    // Load and parse the CSV data
    ParseCSV(csvData,HTML,True);

    // Output the HTML to the console
    Writeln(HTML.Text);

    // Save the HTML to a file (in application's folder)
    HTML.SaveToFile('csv2html_extra.html');
    Writeln('');
    Writeln('
### ===============================
');

  finally
    HTML.Free;
  end;

  // Keep console window open
  Readln;
end.
```


```html5
<!DOCTYPE html
PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">
<head>
<meta http-equiv="content-type" content="text/html; charset=ISO-8859-1" />
<title>CSV-to-HTML Conversion</title>
<style type="text/css">
body {font-family:verdana,helvetica,sans-serif;font-size:100%}
table {width:70%;border:0;font-size:80%;margin:auto}
th,td {padding:4px}
th {text-align:left;background-color:#eee}
th.c {width:15%}
td.c {width:15%}
</style>
</head>
<body>


<table summary="csv2table conversion">
<tr><td class="c">Character</td><td class="s">Speech</td></tr>
<tr><td class="c">The multitude</td><td class="s">The messiah! Show us the messiah!</td></tr>
<tr><td class="c">Brians mother</td><td class="s">&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td class="c">The multitude</td><td class="s">Who are you?</td></tr>
<tr><td class="c">Brians mother</td><td class="s">I'm his mother; that's who!</td></tr>
<tr><td class="c">The multitude</td><td class="s">Behold his mother! Behold his mother!</td></tr>
</table>
</body>
</html>
```


```html5
<!DOCTYPE html
PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">
<head>
<meta http-equiv="content-type" content="text/html; charset=ISO-8859-1" />
<title>CSV-to-HTML Conversion</title>
<style type="text/css">
body {font-family:verdana,helvetica,sans-serif;font-size:100%}
table {width:70%;border:0;font-size:80%;margin:auto}
th,td {padding:4px}
th {text-align:left;background-color:#eee}
th.c {width:15%}
td.c {width:15%}
</style>
</head>
<body>


<table summary="csv2table conversion">
<tr><th class="c">Character</th><th class="s">Speech</th></tr>
<tr><td class="c">The multitude</td><td class="s">The messiah! Show us the messiah!</td></tr>
<tr><td class="c">Brians mother</td><td class="s">&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td class="c">The multitude</td><td class="s">Who are you?</td></tr>
<tr><td class="c">Brians mother</td><td class="s">I'm his mother; that's who!</td></tr>
<tr><td class="c">The multitude</td><td class="s">Behold his mother! Behold his mother!</td></tr>
</table>
</body>
</html>
```



## EchoLisp


```scheme

;; CSV -> LISTS
(define (csv->row line)  (string-split line ","))
(define (csv->table csv) (map  csv->row (string-split csv "\n")))

;; LISTS->HTML
(define html 'html)
(define  (emit-tag tag  html-proc content )
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
	(for ((row (rest table))) (emit-tag 'tr h-row row)))

(define (html-dump) (string-join (stack->list html) " "))

;; STYLES
(style 'td "text-align:left")
(style 'table "border-spacing: 10px;border:28px ridge orange") ;; special biblical border
(style 'th "color:blue;")

```

```scheme

;; changed <angry> to <b> to show that html tags inside text are correctly transmitted.
(define MontyPython #<<
    Character,Speech
    The multitude,The messiah! Show us the messiah!
    Brians mother,<b>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</b>
    The multitude,Who are you?
    Brians mother,I'm his mother; that's who!
    The multitude,Behold his mother! Behold his mother!
>>#)

(define (task speech)
	(define table (csv->table speech))
	(stack html)
	(emit-tag 'table h-table table)
	(html-dump))

(task MontyPython)

```


<table style='border-spacing: 10px;border:28px ridge orange'> <tr> <th style='color:blue;'> Character </th> <th style='color:blue;'> Speech </th> </tr> <tr> <td style='text-align:left'> The multitude </td> <td style='text-align:left'> The messiah! Show us the messiah! </td> </tr> <tr> <td style='text-align:left'> Brians mother </td> <td style='text-align:left'> <b>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</b> </td> </tr> <tr> <td style='text-align:left'> The multitude </td> <td style='text-align:left'> Who are you? </td> </tr> <tr> <td style='text-align:left'> Brians mother </td> <td style='text-align:left'> I'm his mother; that's who! </td> </tr> <tr> <td style='text-align:left'> The multitude </td> <td style='text-align:left'> Behold his mother! Behold his mother! </td> </tr> </table>



## Erlang

Using functions from [[Create_an_HTML_table]]

```Erlang

-module( csv_to_html ).

-export( [table_translation/1, task/0] ).

table_translation( CSV ) ->
	[Headers | Contents] = [string:tokens(X, ",") || X <- string:tokens( CSV, "\n")],
	Table = create_html_table:html_table( [{border, "1"}, {cellpadding, "10"}], Headers, Contents ),
	create_html_table:external_format( Table ).

task() -> table_translation( csv() ).



csv() ->
"Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!".

```


<table border=1 cellpadding=10><tr><th>Character</th><th>Speech</th></tr><tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr><tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr><tr><td>The multitude</td><td>Who are you?</td></tr><tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr><tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr></table>


## Euphoria

```euphoria
constant input = "Character,Speech\n" &
    "The multitude,The messiah! Show us the messiah!\n" &
    "Brians mother,<angry>Now you listen here! He's not the messiah; " &
        "he's a very naughty boy! Now go away!</angry>\n" &
    "The multitude,Who are you?\n" &
    "Brians mother,I'm his mother; that's who!\n" &
    "The multitude,Behold his mother! Behold his mother!"

puts(1,"<table>\n<tr><td>")
for i = 1 to length(input) do
    switch input[i] do
        case '\n' then puts(1,"</td></tr>\n<tr><td>")
        case ','  then puts(1,"</td><td>")
        case '<'  then puts(1,"&lt;")
        case '>'  then puts(1,"&gt;")
        case '&'  then puts(1,"&amp;")
        case else puts(1,input[i])
    end switch
end for
puts(1,"</td></tr>\n</table>")
```


```html5><table

<tr><td>Character</td><td>Speech</td></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
```


=={{header|F_Sharp|F#}}==
Use .NET XmlWriter.
Stylesheet styling is applied only when command line option <tt>-h</tt> ist given.

```fsharp
open System
open System.Text
open System.Xml

let data = """
Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!
"""

let csv =
    Array.map
        (fun (line : string) -> line.Split(','))
        (data.Trim().Split([|'\n';'\r'|],StringSplitOptions.RemoveEmptyEntries))


[<EntryPoint>]
let main argv =
    let style = argv.Length > 0 && argv.[0] = "-h"
    Console.OutputEncoding <- UTF8Encoding()
    let xs = XmlWriterSettings()
    xs.Indent <- true   // be friendly to humans
    use x = XmlWriter.Create(Console.Out, xs)
    x.WriteStartDocument()
    x.WriteDocType("HTML", null, null, null)    // HTML5
    x.WriteStartElement("html")
    x.WriteStartElement("head")
    x.WriteElementString("title", "Rosettacode - CSV to HTML translation")
    if style then
        x.WriteStartElement("style"); x.WriteAttributeString("type", "text/css")
        x.WriteString("""
            table { border-collapse: collapse; }
            td, th { border: 1px solid black; padding: .25em}
            th { background-color: #EEE; }
            tbody th { font-weight: normal; font-size: 85%; }
        """)
        x.WriteEndElement() // style
    x.WriteEndElement() // head
    x.WriteStartElement("body")
    x.WriteStartElement("table")
    x.WriteStartElement("thead"); x.WriteStartElement("tr")
    for part in csv.[0] do x.WriteElementString("th", part)
    x.WriteEndElement(); x.WriteEndElement() // tr thead
    x.WriteStartElement("tbody")
    for line in csv.[1..] do
        x.WriteStartElement("tr")
        x.WriteElementString("th", line.[0])
        x.WriteElementString("td", line.[1])
        x.WriteEndElement() // tr
    x.Close()
    0
```

{{out}} (stylesheet version)
<div style="font-size:70%">

```html5
<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE HTML >
<html>
  <head>
    <title>Rosettacode - CSV to HTML translation</title>
    <style type="text/css">
            table { border-collapse: collapse; }
            td, th { border: 1px solid black; padding: .25em}
            th { background-color: #EEE; }
            tbody th { font-weight: normal; font-size: 85%; }
        </style>
  </head>
  <body>
    <table>
      <thead>
        <tr>
          <th>Character</th>
          <th>Speech</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>The multitude</th>
          <td>The messiah! Show us the messiah!</td>
        </tr>
        <tr>
          <th>Brians mother</th>
          <td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td>
        </tr>
        <tr>
          <th>The multitude</th>
          <td>Who are you?</td>
        </tr>
        <tr>
          <th>Brians mother</th>
          <td>I'm his mother; that's who!</td>
        </tr>
        <tr>
          <th>The multitude</th>
          <td>Behold his mother! Behold his mother!</td>
        </tr>
      </tbody>
    </table>
  </body>
</html>
```
</div>


## Factor


```factor
USING: combinators csv io kernel sequences strings ;
IN: rosetta-code.csv-to-html

CONSTANT: input

"Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!"

: escape-chars ( seq -- seq' )
    [
        {
            { CHAR: & [ "&amp;"  ] }
            { CHAR: ' [ "&apos;" ] }
            { CHAR: < [ "&lt;"   ] }
            { CHAR: > [ "&gt;"   ] }
            [ 1string ]
        } case
    ] { } map-as concat ;

: tag ( str tag -- <tag>str</tag> )
    [ "<" ">" surround ] [ "</" ">" surround ] bi surround ;

: csv>table ( seq -- str )
    [ [ "td" tag ] map concat "tr" tag "  " prepend ] map
    { "<table>" } prepend { "</table>" } append "\n" join ;

input escape-chars string>csv csv>table print
```

```html5><table

  <tr><td>Character</td><td>Speech</td></tr>
  <tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
  <tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He&apos;s not the messiah; he&apos;s a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
  <tr><td>The multitude</td><td>Who are you?</td></tr>
  <tr><td>Brians mother</td><td>I&apos;m his mother; that&apos;s who!</td></tr>
  <tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
```



## Forth


```forth
: BEGIN-COLUMN ." <td>" ;
: END-COLUMN   ." </td>" ;

: BEGIN-ROW ." <tr>" BEGIN-COLUMN ;
: END-ROW END-COLUMN ." </tr>" CR ;

: CSV2HTML
	." <table>" CR BEGIN-ROW
	BEGIN KEY DUP #EOF <> WHILE
		CASE
			      10 OF END-ROW BEGIN-ROW ENDOF
			[CHAR] , OF END-COLUMN BEGIN-COLUMN ENDOF
	                [CHAR] < OF ." &lt;" ENDOF
			[CHAR] > OF ." &gt;" ENDOF
			[CHAR] & OF ." &amp;" ENDOF
			DUP EMIT
		ENDCASE
	REPEAT
	END-ROW ." </table>" CR
;

CSV2HTML BYE
```


```html5><table

<tr><td>Character</td><td>Speech</td></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
```



## Fortran

The plan here is to scan each line for commas to find the splitters between texts, then write the texts out with suitable td and /td swaddling. Text presented as a quoted string (that could thereby contain a comma that is not a delimiter) is explicitly not recognised and apostrophes are apostrophes. Similarly, no attempt is made to recognise characters within the texts that might cause trouble, either because they will not elicit the same glyph when rendered from .html, or because they trigger interpretative action rather than be passed through as-is. Thus the lurking "angry" markings in the texts are not given special conversion. Further, leading and trailing spaces in each text are not trimmed off as the .html rendition process ignores them anyway. To remove them would seem to be easy enough via the special intrinsic function TRIM (available in F90) were it not that it trims off only ''trailing'' spaces. Otherwise one could mess with arrays such as IST and LST to finger the first and last character position of each text and deal patiently with the complications attendant on entirely blank or null strings when scanning the input line to determine the values.

The source is only just F90. It uses an array with a specified lower bound (of zero) - with older Fortran you could play with EQUIVALENCE or else diligently remember the offset. More difficult is the arrangement for ascertaining the length of a record as it is being read, with protection against overflowing the scratchpad should it be longer than is allowed for. This requires the Q format code, and was a common extension to F77. As usual, in the absence of help from the filesystem, the length of the longest record could only be determined through reading the entire file, and for this example it is not worth the annoyance. In the absence of the Q format feature, the input record would be read into ALINE with possibly many trailing spaces added to fill out the scratchpad; more time would be lost in scanning backwards to find the last non-blank. Should a record be longer than the scratchpad then its tail end would be lost and no remark made.  Some systems (such as Snobol) are able to read a record and prepare a text variable of the correct size for that record, whatever its length (up to some integer limit such as 65535, or even more) but Fortran is not one such.

The key statement is the compound WRITE statement and its associated FORMAT, and for non-furrytranners these proceedings may be obscure. A WRITE statement accepts a list of items to be written in the usual manner, and an item is usually the name of a variable or an expression (such as 2*X + 1), but it can also be an implied DO-list. In this case the item is enclosed in brackets, and within them starts with an item list followed by a comma and a DO-expression, as in <code>(a,b,c, I = 1,N)</code> where ''a'', ''b'' and ''c'' constitute a three-element list of items - any of which could themselves be an implied DO-list if nested lists were desired. Thus, by one means or another, the WRITE statement produces a sequence of values (of various types) to be sent forth. In association with the list of entries in the nominated FORMAT statement: processing proceeds in parallel through both lists in the manner of coroutines, not sequentially. It is possible with a READ statement that a value just read adjusts a part of a FORMAT statement not yet encountered during the processing of the READ statement. For instance FORMAT 13 could have <NCOLS> instead of 666 as its repeat count, but this is an extension to F77 and avoided here to stick with older abilities except where inconvenient.

Thus, the entire line for a table row can be written in one go, starting with the tr then many repetitions of (WOT,''text'', WOT), with each starting and ending WOT appropriately encased by &lt; and &gt; and / characters supplied by the FORMAT. This repeated pattern is unsuitable for starting the line (so the tr is provided by the FORMAT, with indentation) because then it would be out of step, but happily, after the last text is rolled, the required /tr can be sent to use the format codes that would normally be used for the start of the next text. Humm. If there ''were'' to be 666 texts to roll, this will exhaust the FORMAT statement and it will write the current line out and start a fresh one, resuming at the rightmost ( in the FORMAT specification. This rule does not always prove convenient so I'd prefer the vertical bar usage of musical notation to mark the resumption location. Interpreters of html do not care about layout but humans do, so, just in case, the repeat count should be 667 (or <NCOLS + 1>), or, if <NCOLS> were used, there could follow a <code>,A</code> in the FORMAT, or, the "/tr" could be removed from the WRITE and appended to end the FORMAT just as at its start, but enough.

The check for a comma is not <code>ALINE(I:I).EQ.","</code> because in other work this usage has been found to evoke astoundingly bad code, notably that ''both'' appearances of "I" are checked as being within bounds, and, the length is calculated by subtracting the "first" (I) from the "last" (also I) at run time! At least by the COMPAQ F90/95 compiler. By contrast, the ICHAR usage, which can only be for a single character, lacks this madness and far superior speed results. Not important in this example, but it explains why this puzzling usage appeared in a prog. at the Culham Science Centre in source from an IBM mainframe.

```Fortran

      SUBROUTINE CSVTEXT2HTML(FNAME,HEADED)	!Does not recognise quoted strings.
Converts without checking field counts, or noting special characters.
       CHARACTER*(*) FNAME	!Names the input file.
       LOGICAL HEADED		!Perhaps its first line is to be a heading.
       INTEGER MANY		!How long is a piece of string?
       PARAMETER (MANY=666)	!This should suffice.
       CHARACTER*(MANY) ALINE	!A scratchpad for the input.
       INTEGER MARK(0:MANY + 1)	!Fingers the commas on a line.
       INTEGER I,L,N		!Assistants.
       CHARACTER*2 WOT(2)	!I don't see why a "table datum" could not be for either.
       PARAMETER (WOT = (/"th","td"/))	!A table heding or a table datum
       INTEGER IT		!But, one must select appropriately.
       INTEGER KBD,MSG,IN		!A selection.
       COMMON /IOUNITS/ KBD,MSG,IN	!The caller thus avoids collisions.
        OPEN(IN,FILE=FNAME,STATUS="OLD",ACTION="READ",ERR=661)	!Go for the file.
        WRITE (MSG,1)			!Start the blather.
    1   FORMAT ("<Table border=1>")	!By stating that a table follows.
        MARK(0) = 0		!Syncopation for the comma fingers.
        N = 0			!No records read.

   10   READ (IN,11,END = 20) L,ALINE(1:MIN(L,MANY))	!Carefully acquire some text.
   11   FORMAT (Q,A)		!Q = number of characters yet to read, A = characters.
        N = N + 1		!So, a record has been read.
        IF (L.GT.MANY) THEN	!Perhaps it is rather long?
          WRITE (MSG,12) N,L,MANY	!Alas!
   12     FORMAT ("Line ",I0," has length ",I0,"! My limit is ",I0)	!Squawk/
          L = MANY			!The limit actually read.
        END IF			!So much for paranoia.
        IF (N.EQ.1 .AND. HEADED) THEN	!Is the first line to be treated specially?
          WRITE (MSG,*) "<tHead>"	!Yep. Nominate a heading.
          IT = 1			!And select "th" rather than "td".
         ELSE				!But mostly,
          IT = 2			!Just another row for the table.
        END IF			!So much for the first line.
        NCOLS = 0		!No commas have been seen.
        DO I = 1,L		!So scan the text for them.
          IF (ICHAR(ALINE(I:I)).EQ.ICHAR(",")) THEN	!Here?
            NCOLS = NCOLS + 1		!Yes!
            MARK(NCOLS) = I		!The texts are between commas.
          END IF			!So much for that character.
        END DO			!On to the next.
        NCOLS = NCOLS + 1	!This is why the + 1 for the size of MARK.
        MARK(NCOLS) = L + 1	!End-of-line is as if a comma was one further along.
        WRITE (MSG,13)		!Now roll all the texts.
     1   (WOT(IT),				!This starting a cell,
     2    ALINE(MARK(I - 1) + 1:MARK(I) - 1),	!This being the text between the commas,
     3    WOT(IT),				!And this ending each cell.
     4    I = 1,NCOLS),			!For this number of columns.
     5   "/tr"			!And this ends the row.
   13   FORMAT (" <tr>",666("<",A,">",A,"</",A,">"))	!How long is a piece of string?
        IF (N.EQ.1 .AND. HEADED) WRITE (MSG,*) "</tHead>"	!Finish the possible header.
        GO TO 10		!And try for another record.

   20   CLOSE (IN)		!Finished with input.
        WRITE (MSG,21)		!And finished with output.
   21   FORMAT ("</Table>")	!This writes starting at column one.
       RETURN			!Done!
Confusions.
  661   WRITE (MSG,*) "Can't open file ",FNAME	!Alas.
      END			!So much for the conversion.

      INTEGER KBD,MSG,IN
      COMMON /IOUNITS/ KBD,MSG,IN
      KBD = 5	!Standard input.
      MSG = 6	!Standard output.
      IN = 10	!Some unspecial number.

      CALL CSVTEXT2HTML("Text.csv",.FALSE.)	!The first line is not special.
      WRITE (MSG,*)
      CALL CSVTEXT2HTML("Text.csv",.TRUE.)	!The first line is a heading.
      END

```

```txt

<Table border=1>
 <tr><td>Character</td><td>Speech</td></tr>
 <tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
 <tr><td>Brian's mother</td><td><angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry></td></tr>
 <tr><td>The multitude</td><td>Who are you?</td></tr>
 <tr><td>Brian's mother</td><td>I'm his mother; that's who!</td></tr>
 <tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</Table>

<Table border=1>
 <tHead>
 <tr><th>Character</th><th>Speech</th></tr>
 </tHead>
 <tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
 <tr><td>Brian's mother</td><td><angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry></td></tr>
 <tr><td>The multitude</td><td>Who are you?</td></tr>
 <tr><td>Brian's mother</td><td>I'm his mother; that's who!</td></tr>
 <tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>

```


And interpreted, first with no special heading:
<Table border=1>
 <tr><td>Character</td><td>Speech</td></tr>
 <tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
 <tr><td>Brian's mother</td><td><angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry></td></tr>
 <tr><td>The multitude</td><td>Who are you?</td></tr>
 <tr><td>Brian's mother</td><td>I'm his mother; that's who!</td></tr>
 <tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</Table>

<Table border=1>
 <tHead>
 <tr><th>Character</th><th>Speech</th></tr>
 </tHead>
 <tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
 <tr><td>Brian's mother</td><td><angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry></td></tr>
 <tr><td>The multitude</td><td>Who are you?</td></tr>
 <tr><td>Brian's mother</td><td>I'm his mother; that's who!</td></tr>
 <tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</Table>


## Go


```go
package main

import (
    "bytes"
    "encoding/csv"
    "fmt"
    "html/template"
    "strings"
)

var c = `Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!`

func main() {
    if h, err := csvToHtml(c); err != nil {
        fmt.Println(err)
    } else {
        fmt.Print(h)
    }
}

func csvToHtml(c string) (string, error) {
    data, err := csv.NewReader(bytes.NewBufferString(c)).ReadAll()
    if err != nil {
        return "", err
    }
    var b strings.Builder
    err = template.Must(template.New("").Parse(`<table>
{{range .}}    <tr>{{range .}}<td>{{.}}</td>{{end}}</tr>
{{end}}</table>
`)).Execute(&b, data)
    return b.String(), err
}
```

Extra credit version accepts -h command line option to do the special formatting for the heading line.

```go
package main

import (
    "bytes"
    "encoding/csv"
    "flag"
    "fmt"
    "html/template"
    "strings"
)

var csvStr = `Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!`

func main() {
    headings := flag.Bool("h", false, "format first row as column headings")
    flag.Parse()
    if html, err := csvToHtml(csvStr, *headings); err != nil {
        fmt.Println(err)
    } else {
        fmt.Print(html)
    }
}

func csvToHtml(csvStr string, headings bool) (string, error) {
    data, err := csv.NewReader(bytes.NewBufferString(csvStr)).ReadAll()
    if err != nil {
        return "", err
    }
    tStr := tPlain
    if headings {
        tStr = tHeadings
    }
    var b strings.Builder
    err = template.Must(template.New("").Parse(tStr)).Execute(&b, data)
    return b.String(), err
}

const (
    tPlain = `<table>
{{range .}}    <tr>{{range .}}<td>{{.}}</td>{{end}}</tr>
{{end}}</table>
`
    tHeadings = `<table>{{if .}}
      <tr>{{range .}}<td>{{.}}</td>{{end}}</tr>{{else}}   <thead>
      <tr>{{range .}}<th>{{.}}</th>{{end}}</tr>
   </thead>
   <tbody>{{end}}{{end}}
   </tbody>{{end}}
</table>
`
)
```

Extra credit version with -h

```html><table

   <thead>
      <tr><th>Character</th><th>Speech</th></tr>
   </thead>
   <tbody>
      <tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
      <tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He&#39;s not the messiah; he&#39;s a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
      <tr><td>The multitude</td><td>Who are you?</td></tr>
      <tr><td>Brians mother</td><td>I&#39;m his mother; that&#39;s who!</td></tr>
      <tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
   </tbody>
</table>
```

Basic version, or extra credit version without -h

```html><table

    <tr><td>Character</td><td>Speech</td></tr>
    <tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
    <tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He&#39;s not the messiah; he&#39;s a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
    <tr><td>The multitude</td><td>Who are you?</td></tr>
    <tr><td>Brians mother</td><td>I&#39;m his mother; that&#39;s who!</td></tr>
    <tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
```



## Groovy



### Solution #1: Nested GStrings

Brute force solution using nested GStrings. It solves both the basic and extra credit tasks.

```groovy
def formatCell = { cell ->
    "<td>${cell.replaceAll('&','&amp;').replaceAll('<','&lt;')}</td>"
}

def formatRow = { row ->
    """<tr>${row.split(',').collect { cell -> formatCell(cell) }.join('')}</tr>
"""
}

def formatTable = { csv, header=false ->
    def rows = csv.split('\n').collect { row -> formatRow(row) }
    header \
        ? """
<table>
<thead>
${rows[0]}</thead>
<tbody>
${rows[1..-1].join('')}</tbody>
</table>
""" \
        : """
<table>
${rows.join('')}</table>
"""
}

def formatPage = { title, csv, header=false ->
"""<html>
<head>
<title>${title}</title>
<style type="text/css">
td {background-color:#ddddff; }
thead td {background-color:#ddffdd; text-align:center; }
</style>
</head>
<body>${formatTable(csv, header)}</body>
</html>"""
}
```


'''Test:'''

```groovy
def csv = '''Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!'''

println 'Basic:'
println '-----------------------------------------'
println (formatPage('Basic', csv))
println '-----------------------------------------'
println()
println()
println 'Extra Credit:'
println '-----------------------------------------'
println (formatPage('Extra Credit', csv, true))
println '-----------------------------------------'
```


<div style="height:30ex;overflow:scroll;">
```html5><html

<head>
<title>Basic</title>
<style type="text/css">
td {background-color:#ddddff; }
thead td {background-color:#ddffdd; text-align:center; }
</style>
</head>
<body>
<table>
<tr><td>Character</td><td>Speech</td></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry></td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
</body>
</html>
```
</div>
[[File:Groovy-csv-to-html-basic.jpg]]
Appearance as rendered in Google Chrome.

<div style="height:30ex;overflow:scroll;">
```html5><html

<head>
<title>Extra Credit</title>
<style type="text/css">
td {background-color:#ddddff; }
thead td {background-color:#ddffdd; text-align:center; }
</style>
</head>
<body>
<table>
<thead>
<tr><td>Character</td><td>Speech</td></tr>
</thead>
<tbody>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry></td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</tbody>
</table>
</body>
</html>
```
</div>
[[File:Groovy-csv-to-html-extra.jpg]]
Appearance as rendered in Google Chrome.


### Solution #2: MarkupBuilder

A much cleaner solution using the Groovy XML MarkupBuilder class. It solves both the basic and extra credit tasks.

```groovy
import groovy.xml.MarkupBuilder

def formatRow = { doc, row ->
    doc.tr { row.each { cell -> td { mkp.yield(cell) } } }
}

def formatPage = { titleString, csv, header=false ->
    def writer = new StringWriter()
    def doc = new MarkupBuilder(writer)
    def rows = csv.split('\n').collect { row -> row.split(',') }
    doc.html {
        head {
            title (titleString)
            style (type:"text/css") {
                mkp.yield('''
                    td {background-color:#ddddff; }
                    thead td {background-color:#ddffdd; text-align:center; }
                ''')
            }
        }
        body {
            table {
                header && thead { formatRow(doc, rows[0]) }
                header && tbody { rows[1..-1].each { formatRow(doc, it) } }
                header || rows.each { formatRow(doc, it) }
            }
        }
    }
    writer.toString()
}
```


'''Test:'''<br/>
The interface is the same for both solutions, so we just reuse the same test as before.

<div style="height:30ex;overflow:scroll;">
```html5><html

  <head>
    <title>Basic</title>
    <style type='text/css'>
                    td {background-color:#ddddff; }
                    thead td {background-color:#ddffdd; text-align:center; }
                </style>
  </head>
  <body>
    <table>
      <tr>
        <td>Character</td>
        <td>Speech</td>
      </tr>
      <tr>
        <td>The multitude</td>
        <td>The messiah! Show us the messiah!</td>
      </tr>
      <tr>
        <td>Brians mother</td>
        <td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td>
      </tr>
      <tr>
        <td>The multitude</td>
        <td>Who are you?</td>
      </tr>
      <tr>
        <td>Brians mother</td>
        <td>I'm his mother; that's who!</td>
      </tr>
      <tr>
        <td>The multitude</td>
        <td>Behold his mother! Behold his mother!</td>
      </tr>
    </table>
  </body>
</html>
```
</div>

The HTML for this solution looks superficially different than that from the GString solution, but the appearance as rendered in Google Chrome is identical.

'''Extra Credit output:'''
<div style="height:30ex;overflow:scroll;">
```html5><html

  <head>
    <title>Extra Credit</title>
    <style type='text/css'>
                    td {background-color:#ddddff; }
                    thead td {background-color:#ddffdd; text-align:center; }
                </style>
  </head>
  <body>
    <table>
      <thead>
        <tr>
          <td>Character</td>
          <td>Speech</td>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td>The multitude</td>
          <td>The messiah! Show us the messiah!</td>
        </tr>
        <tr>
          <td>Brians mother</td>
          <td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td>
        </tr>
        <tr>
          <td>The multitude</td>
          <td>Who are you?</td>
        </tr>
        <tr>
          <td>Brians mother</td>
          <td>I'm his mother; that's who!</td>
        </tr>
        <tr>
          <td>The multitude</td>
          <td>Behold his mother! Behold his mother!</td>
        </tr>
      </tbody>
    </table>
  </body>
</html>
```
</div>

The HTML for this solution looks superficially different than that from the GString solution, but the appearance as rendered in Google Chrome is identical.


## Haskell

'''Simple solution'''

```haskell
--import Data.List.Split (splitOn)    -- if the import is available
splitOn :: Char -> String -> [String] -- otherwise
splitOn delim = foldr (\x rest ->
                        if x == delim then "" : rest
                        else (x:head rest):tail rest) [""]

htmlEscape :: String -> String
htmlEscape =  concatMap escapeChar
              where escapeChar '<' = "&lt;"
                    escapeChar '>' = "&gt;"
                    escapeChar '&' = "&amp;"
                    escapeChar '"' = "&quot;" --"
                    escapeChar c   = [c]

toHtmlRow :: [String] -> String
toHtmlRow []   = "<tr></tr>"
toHtmlRow cols = let htmlColumns = concatMap toHtmlCol cols
                  in "<tr>\n" ++ htmlColumns  ++ "</tr>"
               where toHtmlCol x = "  <td>" ++ htmlEscape x ++ "</td>\n"

csvToTable :: String -> String
csvToTable csv = let rows = map (splitOn ',') $ lines csv
                     html = unlines $ map toHtmlRow rows
                  in "<table>\n" ++ html ++ "</table>"

main = interact csvToTable
```


'''Compact version'''

```haskell
import Data.List (unfoldr)
split p = unfoldr (\s -> case dropWhile p s of [] -> Nothing
                                               ss -> Just $ break p ss)

main = interact (\csv -> "<table>\n" ++
    (unlines $ map ((\cols -> "<tr>\n" ++
        (concatMap (\x -> "  <td>" ++ concatMap (\c ->
            case c of {'<' -> "&lt;"; '>' -> "&gt;";
			'&' -> "&amp;"; '"' -> "&quot;"; _ -> [c]}) x
        ++ "</td>\n") cols)
    ++ "</tr>") . split (==',')) $ lines csv) ++ "</table>")
```


;Output:
<div style="height:30ex;overflow:scroll;">
```html5><table

<tr>
  <td>Character</td>
  <td>Speech</td>
</tr>
<tr>
  <td>The multitude</td>
  <td>The messiah! Show us the messiah!</td>
</tr>
<tr>
  <td>Brians mother</td>
  <td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td>
</tr>
<tr>
  <td>The multitude</td>
  <td>Who are you?</td>
</tr>
<tr>
  <td>Brians mother</td>
  <td>I'm his mother; that's who!</td>
</tr>
<tr>
  <td>The multitude</td>
  <td>Behold his mother! Behold his mother!</td>
</tr>
</table>
```
</div>

=={{header|Icon}} and {{header|Unicon}}==
This solution for the extra credit works in both Icon and Unicon.
The simple CSV is read from standard input and written to standard output.
The presence/abscend of "-heading" in the argument list sets the variable thead
to the procedure writes or a 1 (for more on this see [[Icon%2BUnicon/Intro#Conjunction.2C_yielding_a_different_result|Introduction to Icon/Unicon - Conjunction yielding different results]]).


```Icon
procedure main(arglist)
    pchar := &letters ++ &digits ++ '!?;. '  # printable chars

    write("<TABLE>")
    firstHead := (!arglist == "-heading")
    tHead := write
    while row := trim(read()) do {
       if \firstHead then write("   <THEAD>") else tHead("   <TBODY>")
       writes("       <TR><TD>")
       while *row > 0 do
         row ?:= ( (=",",writes("</TD><TD>")) |
                         writes( tab(many(pchar)) |
                         ("&#" || ord(move(1))) ),   tab(0))
       write("</TD></TR>")
       if (\firstHead) := &null then write("    </THEAD>\n    <TBODY>")
       tHead := 1
       }
    write("   </TBODY>")
    write("</TABLE>")
end
```


```html5><TABLE

   <THEAD>
       <TR><TD>Character</TD><TD>Speech</TD></TR>
    </THEAD>
    <TBODY>
       <TR><TD>The multitude</TD><TD>The messiah! Show us the messiah!</TD></TR>
       <TR><TD>Brians mother</TD><TD>&#60angry&#62Now you listen here! He&#39s not the messiah; he&#39s a very naughty boy! Now go away!&#60&#47angry&#62</TD></TR>
       <TR><TD>The multitude</TD><TD>Who are you?</TD></TR>
       <TR><TD>Brians mother</TD><TD>I&#39m his mother; that&#39s who!</TD></TR>
       <TR><TD>The multitude</TD><TD>Behold his mother! Behold his mother!</TD></TR>
   </TBODY>
</TABLE>
```



## J

'''Solution (extra credit)'''

```j
require 'strings tables/csv'
encodeHTML=: ('&';'&amp;';'<';'&lt;';'>';'&gt;')&stringreplace

tag=: adverb define
  'starttag endtag'=.m
  (,&.>/)"1 (starttag , ,&endtag) L:0 y
)

markupCells=:    ('<td>';'</td>') tag
markupHdrCells=: ('<th>';'</th>') tag
markupRows=:     ('<tr>';'</tr>',LF) tag
markupTable=:    (('<table>',LF);'</table>') tag

makeHTMLtablefromCSV=: verb define
  0 makeHTMLtablefromCSV y             NB. default left arg is 0 (no header row)
:
  t=. fixcsv encodeHTML y
  if. x do. t=. (markupHdrCells@{. , markupCells@}.) t
      else. t=. markupCells t
  end.
  ;markupTable markupRows t
)
```


For those interested, equivalent tacit versions of <code>tag</code> and <code>makeHTMLtablefromCSV</code> are:

```j
tag=: adverb def '[: (,&.>/)"1 m&(0&{::@[ , 1&{::@[ ,~ ]) L:0@]'
makeHTMLtablefromCSV6=: 0&$: : ([: ; markupTable@markupRows@([ markupCells`(markupHdrCells@{. , markupCells@}.)@.[ fixcsv@encodeHTML))
```


'''Example'''

```j
   CSVstrng=: noun define
Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!
)
   1 makeHTMLtablefromCSV CSVstrng
```

```html5><table

<tr><th>Character</th><th>Speech</th></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
```



## Java

'''Solution including simple and extra credit version'''

<tt>for simple solution  : java -cp . Csv2Html < text.csv</tt>

<tt>for extended solution: java -cp . Csv2Html header < text.csv</tt>


```java
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintStream;

class Csv2Html {

	public static String escapeChars(String lineIn) {
		StringBuilder sb = new StringBuilder();
		int lineLength = lineIn.length();
		for (int i = 0; i < lineLength; i++) {
			char c = lineIn.charAt(i);
			switch (c) {
				case '"':
					sb.append("&quot;");
					break;
				case '&':
					sb.append("&amp;");
					break;
				case '\'':
					sb.append("&apos;");
					break;
				case '<':
					sb.append("&lt;");
					break;
				case '>':
					sb.append("&gt;");
					break;
				default: sb.append(c);
			}
		}
		return sb.toString();
	}

	public static void tableHeader(PrintStream ps, String[] columns) {
		ps.print("<tr>");
		for (int i = 0; i < columns.length; i++) {
			ps.print("<th>");
			ps.print(columns[i]);
			ps.print("</th>");
		}
		ps.println("</tr>");
	}

	public static void tableRow(PrintStream ps, String[] columns) {
		ps.print("<tr>");
		for (int i = 0; i < columns.length; i++) {
			ps.print("<td>");
			ps.print(columns[i]);
			ps.print("</td>");
		}
		ps.println("</tr>");
	}

	public static void main(String[] args) throws Exception {
		boolean withTableHeader = (args.length != 0);

		InputStreamReader isr = new InputStreamReader(System.in);
		BufferedReader br = new BufferedReader(isr);
		PrintStream stdout = System.out;

		stdout.println("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">");
		stdout.println("<html xmlns=\"http://www.w3.org/1999/xhtml\">");
		stdout.println("<head><meta http-equiv=\"Content-type\" content=\"text/html;charset=UTF-8\"/>");
		stdout.println("<title>Csv2Html</title>");
		stdout.println("<style type=\"text/css\">");
		stdout.println("body{background-color:#FFF;color:#000;font-family:OpenSans,sans-serif;font-size:10px;}");
		stdout.println("table{border:0.2em solid #2F6FAB;border-collapse:collapse;}");
		stdout.println("th{border:0.15em solid #2F6FAB;padding:0.5em;background-color:#E9E9E9;}");
		stdout.println("td{border:0.1em solid #2F6FAB;padding:0.5em;background-color:#F9F9F9;}</style>");
		stdout.println("</head><body><h1>Csv2Html</h1>");

		stdout.println("<table>");
		String stdinLine;
		boolean firstLine = true;
		while ((stdinLine = br.readLine()) != null) {
			String[] columns = escapeChars(stdinLine).split(",");
			if (withTableHeader == true && firstLine == true) {
				tableHeader(stdout, columns);
				firstLine = false;
			} else {
				tableRow(stdout, columns);
			}
		}
		stdout.println("</table></body></html>");
	}
}

```


```html5
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head><meta http-equiv="Content-type" content="text/html;charset=UTF-8"/>
<title>Csv2Html</title>
<style type="text/css">
body{background-color:#FFF;color:#000;font-family:OpenSans,sans-serif;font-size:10px;}
table{border:0.2em solid #2F6FAB;border-collapse:collapse;}
th{border:0.15em solid #2F6FAB;padding:0.5em;background-color:#E9E9E9;}
td{border:0.1em solid #2F6FAB;padding:0.5em;background-color:#F9F9F9;}</style>
</head><body><h1>Csv2Html</h1>
<table>
<tr><td>Character</td><td>Speech</td></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He&apos;s not the messiah; he&apos;s a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I&apos;m his mother; that&apos;s who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table></body></html>
```


```html5
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head><meta http-equiv="Content-type" content="text/html;charset=UTF-8"/>
<title>Csv2Html</title>
<style type="text/css">
body{background-color:#FFF;color:#000;font-family:OpenSans,sans-serif;font-size:10px;}
table{border:0.2em solid #2F6FAB;border-collapse:collapse;}
th{border:0.15em solid #2F6FAB;padding:0.5em;background-color:#E9E9E9;}
td{border:0.1em solid #2F6FAB;padding:0.5em;background-color:#F9F9F9;}</style>
</head><body><h1>Csv2Html</h1>
<table>
<tr><th>Character</th><th>Speech</th></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He&apos;s not the messiah; he&apos;s a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I&apos;m his mother; that&apos;s who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table></body></html>
```



## JavaScript


```JavaScript
var csv = "Character,Speech\n" +
	   "The multitude,The messiah! Show us the messiah!\n" +
	   "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n" +
	   "The multitude,Who are you?\n" +
	   "Brians mother,I'm his mother; that's who!\n" +
	   "The multitude,Behold his mother! Behold his mother!";

var lines = csv.replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .split(/[\n\r]/)
    .map(function(line) { return line.split(',')})
    .map(function(row) {return '\t\t<tr><td>' + row[0] + '</td><td>' + row[1] + '</td></tr>';});

console.log('<table>\n\t<thead>\n'      + lines[0] +
            '\n\t</thead>\n\t<tbody>\n' + lines.slice(1).join('\n') +
            '\t</tbody>\n</table>');


```



```html5><table

  <thead>
    <tr><td>Character</td><td>Speech</td></tr>
  </thead>
  <tbody>
    <tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
    <tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
    <tr><td>The multitude</td><td>Who are you?</td></tr>
    <tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
    <tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
  </tbody>
</table>
```


```html5><table

	<thead>
		<tr><td>Character</td><td>Speech</td></tr>
	</thead>
	<tbody>
		<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
		<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
		<tr><td>The multitude</td><td>Who are you?</td></tr>
		<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
		<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>	</tbody>
</table>
```



## jq

We will assume the input is in a file named csv2html.csv, and that the jq program as given below is in a file named csv2html.jqn.  To simplify things, we will invoke the jq processor twice -- the first invocation simply converts the text input into a sequence of JSON strings:

```jq
jq -R . csv2html.csv | jq -r -s -f csv2html.jq

```

```jq
def headerrow2html:
  ["  <thead> <tr>"]
  + (split(",") | map("    <th>\(@html)</th>"))
  + [ "  </tr> </thead>" ]
;

def row2html:
  ["  <tr>"]
  + (split(",") | map("    <td>\(@html)</td>"))
  + [ "  </tr>" ]
;

def csv2html:
  def rows: reduce .[] as $row
    ([]; . + ($row | row2html));
  ["<table>"]
  + (.[0]  | headerrow2html)
  + (.[1:] | rows)
  + [ "</table>"]
;

csv2html | .[]
```



### Output


```html5><table

  <thead> <tr>
    <th>Character</th>
    <th>Speech </th>
  </tr> </thead>
  <tr>
    <td>The multitude</td>
    <td>The messiah! Show us the messiah! </td>
  </tr>
  <tr>
    <td>Brians mother</td>
    <td>&lt;angry&gt;Now you listen here! He&apos;s not the messiah; he&apos;s a very naughty boy! Now go away!&lt;/angry&gt; </td>
  </tr>
  <tr>
    <td>The multitude</td>
    <td>Who are you? </td>
  </tr>
  <tr>
    <td>Brians mother</td>
    <td>I&apos;m his mother; that&apos;s who! </td>
  </tr>
  <tr>
    <td>The multitude</td>
    <td>Behold his mother! Behold his mother! </td>
  </tr>
</table>
```



## Jsish

From Javascript entry.


```javascript
/* CSV to HTML, in Jsish */
var csv = "Character,Speech\n" +
           "The multitude,The messiah! Show us the messiah!\n" +
           "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n" +
           "The multitude,Who are you?\n" +
           "Brians mother,I'm his mother; that's who!\n" +
           "The multitude,Behold his mother! Behold his mother!";

var lines = csv.replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .split('\n')
    .map(function(line) { return line.split(','); })
    .map(function(row) { return '\t\t<tr><td>' + row[0] + '</td><td>' + row[1] + '</td></tr>'; });

if (Interp.conf('unitTest')) {
    puts('<table>\n\t<thead>\n' + lines[0] + '\n\t</thead>\n\t<tbody>\n'
         + lines.slice(1).join('\n') + '\t</tbody>\n</table>');
}

/*
=!EXPECTSTART!=
<table>
        <thead>
                <tr><td>Character</td><td>Speech</td></tr>
        </thead>
        <tbody>
                <tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
                <tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
                <tr><td>The multitude</td><td>Who are you?</td></tr>
                <tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
                <tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>   </tbody>
</table>
=!EXPECTEND!=
*/
```


```txt
prompt$ jsish -u csvToHTML.jsi
[PASS] csvToHTML.jsi
```



## Julia


```Julia
using DataFrames, CSV

using CSV, DataFrames

function csv2html(fname; header::Bool=false)
    csv = CSV.read(fname)
    @assert(size(csv, 2) > 0)
    str = """
<html>

<head>
    <style type="text/css">
        body {
            margin: 2em;
        }
        h1 {
            text-align: center;
        }
        table {
            border-spacing: 0;
            box-shadow: 0 0 0.25em #888;
            margin: auto;
        }
        table,
        tr,
        th,
        td {
            border-collapse: collapse;
        }
        th {
            color: white;
            background-color: rgb(43, 53, 59);
        }
        th,
        td {
            padding: 0.5em;
        }
        table tr:nth-child(even) td {
            background-color: rgba(218, 224, 229, 0.850);
        }
    </style>
</head>

<body>
    <h1>csv2html Example</h1>
    <table>
        <tr>
"""
    tags = header ? ("<th>", "</th>") : ("<td>", "</td>")

    for i=1:size(csv, 2)
        str *= "            " * tags[1] * csv[1, i] * tags[2] * "\n"
    end

    str *= " "^8 * "</tr>\n"

    for i=2:size(csv, 1)
        str *= "        <tr>\n"

        for j=1:size(csv, 2)
            str *= "            " * "<td>" * csv[i, j] * "</td>\n"
        end

        str *= "        </tr>\n"
    end

    str * "    </table>\n</body>\n\n</html>\n"
end

print(csv2html("input.csv", header=true))

```
```html5><html


<head>
    <style type="text/css">
        body {
            margin: 2em;
        }
        h1 {
            text-align: center;
        }
        table {
            border-spacing: 0;
            box-shadow: 0 0 0.25em #888;
            margin: auto;
        }
        table,
        tr,
        th,
        td {
            border-collapse: collapse;
        }
        th {
            color: white;
            background-color: rgb(43, 53, 59);
        }
        th,
        td {
            padding: 0.5em;
        }
        table tr:nth-child(even) td {
            background-color: rgba(218, 224, 229, 0.850);
        }
    </style>
</head>

<body>
    <h1>csv2html Example</h1>
    <table>
        <tr>
            <th>Character</th>
            <th>Speech</th>
        </tr>
        <tr>
            <td>The multitude</td>
            <td>The messiah! Show us the messiah!</td>
        </tr>
        <tr>
            <td>Brians mother</td>
            <td>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</td>
        </tr>
        <tr>
            <td>The multitude</td>
            <td>Who are you?</td>
        </tr>
        <tr>
            <td>Brians mother</td>
            <td>I'm his mother; that's who!</td>
        </tr>
        <tr>
            <td>The multitude</td>
            <td>Behold his mother! Behold his mother!</td>
        </tr>
    </table>
</body>
</html>
```



## Kotlin


```scala
// version 1.1.3

val csv =
    "Character,Speech\n" +
    "The multitude,The messiah! Show us the messiah!\n" +
    "Brians mother,<angry>Now you listen here! He's not the messiah; " +
    "he's a very naughty boy! Now go away!</angry>\n" +
    "The multitude,Who are you?\n" +
    "Brians mother,I'm his mother; that's who!\n" +
    "The multitude,Behold his mother! Behold his mother!"

fun main(args: Array<String>) {
    val i = "   "  // indent
    val sb = StringBuilder("<table>\n$i<tr>\n$i$i<td>")
    for (c in csv) {
        sb.append( when (c) {
            '\n' -> "</td>\n$i</tr>\n$i<tr>\n$i$i<td>"
            ','  -> "</td>\n$i$i<td>"
            '&'  -> "&amp;"
            '\'' -> "&apos;"
            '<'  -> "&lt;"
            '>'  -> "&gt;"
            else -> c.toString()
        })
    }
    sb.append("</td>\n$i</tr>\n</table>")
    println(sb.toString())
    println()

    // now using first row as a table header
    sb.setLength(0)
    sb.append("<table>\n$i<thead>\n$i$i<tr>\n$i$i$i<td>")
    val hLength = csv.indexOf('\n') + 1  // find length of first row including CR
    for (c in csv.take(hLength)) {
        sb.append( when (c) {
            '\n' -> "</td>\n$i$i</tr>\n$i</thead>\n$i<tbody>\n$i$i<tr>\n$i$i$i<td>"
            ','  -> "</td>\n$i$i$i<td>"
            else -> c.toString()
        })
    }
    for (c in csv.drop(hLength)) {
        sb.append( when (c) {
            '\n' -> "</td>\n$i$i</tr>\n$i$i<tr>\n$i$i$i<td>"
            ','  -> "</td>\n$i$i$i<td>"
            '&'  -> "&amp;"
            '\'' -> "&apos;"
            '<'  -> "&lt;"
            '>'  -> "&gt;"
            else -> c.toString()
        })
    }
    sb.append("</td>\n$i$i</tr>\n$i</tbody>\n</table>")
    println(sb.toString())
}
```


```html5><table

   <tr>
      <td>Character</td>
      <td>Speech</td>
   </tr>
   <tr>
      <td>The multitude</td>
      <td>The messiah! Show us the messiah!</td>
   </tr>
   <tr>
      <td>Brians mother</td>
      <td>&lt;angry&gt;Now you listen here! He&apos;s not the messiah; he&apos;s a very naughty boy! Now go away!&lt;/angry&gt;</td>
   </tr>
   <tr>
      <td>The multitude</td>
      <td>Who are you?</td>
   </tr>
   <tr>
      <td>Brians mother</td>
      <td>I&apos;m his mother; that&apos;s who!</td>
   </tr>
   <tr>
      <td>The multitude</td>
      <td>Behold his mother! Behold his mother!</td>
   </tr>
</table>

<table>
   <thead>
      <tr>
         <td>Character</td>
         <td>Speech</td>
      </tr>
   </thead>
   <tbody>
      <tr>
         <td>The multitude</td>
         <td>The messiah! Show us the messiah!</td>
      </tr>
      <tr>
         <td>Brians mother</td>
         <td>&lt;angry&gt;Now you listen here! He&apos;s not the messiah; he&apos;s a very naughty boy! Now go away!&lt;/angry&gt;</td>
      </tr>
      <tr>
         <td>The multitude</td>
         <td>Who are you?</td>
      </tr>
      <tr>
         <td>Brians mother</td>
         <td>I&apos;m his mother; that&apos;s who!</td>
      </tr>
      <tr>
         <td>The multitude</td>
         <td>Behold his mother! Behold his mother!</td>
      </tr>
   </tbody>
</table>

```



## Liberty BASIC


```lb

    newline$ ="|"
    '   No escape behaviour, so can't refer to '/n'.
    '   Generally imported csv would have separator CR LF; easily converted first if needed

    csv$ ="Character,Speech"                                                                                        +newline$+_
    "The multitude,The messiah! Show us the messiah!"                                                               +newline$+_
    "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>" +newline$+_
    "The multitude,Who are you?"                                                                                    +newline$+_
    "Brians mother,I'm his mother; that's who!"                                                                     +newline$+_
    "The multitude,Behold his mother! Behold his mother!"

    print "<HTML>"
    print "<HEAD>"
    print "</HEAD>"
    print "<BODY>"
    print "<center><H1>CSV to HTML translation </H1></center>"
    print "<table border=1 cellpadding =10>"
    print "<tr><td>"

    for i =1 to len( csv$)
        c$ =mid$( csv$, i, 1)
        select case c$
        case "|": print "</td></tr>": print "<tr><td>"
        case ",":  print "</td><td>";
        case "<":  print "&"+"lt;";
        case ">":  print "&"+"gt;";
        case "&":  print "&"+"amp;";
        case else:   print c$;
        end select
    next i

    print "</td></tr>"
    print "</table>"
    print "</BODY>"
    print "</HTML>"
    end

```

```txt

<HTML>
<HEAD>
</HEAD>
<BODY>
<center><H1>CSV to HTML translation </H1></center>
<table border=1 cellpadding =10>
<tr><td>
Character</td><td>Speech</td></tr>
<tr><td>
The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>
Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>
The multitude</td><td>Who are you?</td></tr>
<tr><td>
Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>
The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
</BODY>
</HTML>

```

 Rendered output is available at http://www.diga.me.uk/csvhtml.gif


## Lua


```lua
FS = ","            -- field separator

csv = [[
Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!
]]

csv = csv:gsub( "<", "&lt;" )
csv = csv:gsub( ">", "&gr;" )

html = { "<table>" }
for line in string.gmatch( csv, "(.-\n)" ) do
    str = "<tr>"
    for field in string.gmatch( line, "(.-)["..FS.."?\n?]" ) do
        str = str .. "<td>" .. field .. "</td>"
    end
    str = str .. "</tr>"
    html[#html+1] = str;
end
html[#html+1] = "</table>"

for _, line in pairs(html) do
    print(line)
end
```



```html5><table

<tr><td>Character</td><td>Speech</td></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gr;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gr;</td></tr>
<tr><td>The multitude</td><td>Who are you</td><td></td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
```




## Maple


```maple
 #A translation of the C code posted
html_table := proc(str)
	local char;
     printf("<table>\n<tr><td>");
     for char in str do
         if char = "\n" then
         	printf("</td></tr>\n<tr><td>")
         elif char = "," then
          printf("</td><td>")
         elif char = "<" then
          printf("&lt;")
         elif char = ">" then
          printf("&gt;")
         elif char = "&" then
         	printf("&amp;")
         else
          printf(char)
         end if;
    end do;
    printf("</td></tr>\n</table>");
end proc;

html_table("Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!");

```

```txt

<table>
<tr><td>Character</td><td>Speech</td></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>

```




=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
a="Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah;he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother;that's who!
The multitude,Behold his mother! Behold his mother!";
(*Naive*)
StringJoin["<table>\n",Map[StringJoin["<tr><td>",#,"</td></tr>\n"]&,
StringSplit[StringReplace[a,{","->"</td><td>","<"->"&lt;",">"->"&gt;"}],"\n"]]
,"</table>"]
(*Extra*)
StringJoin["<table>\n",StringJoin["<tr><th>",#,"</th></tr>\n"]&[
StringSplit[StringReplace[a,{","->"</td><td>","<"->"&lt;",">"->"&gt;"}],"\n"]//First]
,Map[StringJoin["<tr><td>",#,"</td></tr>\n"]&,
StringSplit[StringReplace[a,{","->"</td><td>","<"->"&lt;",">"->"&gt;"}],"\n"]//Rest]
,"</table>"]

```

```txt

<table>
<tr><td>Character</td><td>Speech</td></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah;he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother;that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>

```

```txt

<table>
<tr><th>Character</td><td>Speech</th></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah;he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother;that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
```



## MATLAB


Many specialized MATLAB file IO functions are painfully slow,
and with large files it is often better to use a lower-level functions such as fread.
Here we use fileread, which does a little bit of error handling and calls fread, specifying the input data as text.
From this text string, we can easily convert any special html characters,
split it into a cell array, and print it out specifying format.


```MATLAB

inputString = fileread(csvFileName);
   % using multiple regular expressions to clear up special chars
htmlFriendly = regexprep(regexprep(regexprep(regexprep(inputString,...
      '&','&amp;'),...
      '"','&quot;'),...
      '<','&lt;'),...
      '>','&gt;');
   % split string into cell array
tableValues = regexp(regexp(htmlFriendly,'(\r\n|\r|\n)','split')',',','split');
   %%% print in html format %%%
   % <Extra Credit> first line gets treated as header
fprintf(1,['<table>\n\t<tr>'  sprintf('\n\t\t<th>%s</th>',tableValues{1,:}{:})])
   % print remaining lines of csv as html table (rows 2:end in cell array of csv values)
cellfun(@(x)fprintf(1,['\n\t<tr>'  sprintf('\n\t\t<td>%s</td>',x{:}) '\n\t</tr>']),tableValues(2:end))
fprintf(1,'\n</table>')

```

As a single line:

```MATLAB

fprintf(1,['<table>\n\t<tr>\n\t\t<th>',regexprep(regexprep(regexprep(regexprep(regexprep(regexprep(regexprep(regexprep(fileread(cvsFileName),'&','&amp;'),'"','&quot;'),'<','&lt;'),'>','&gt;'),'(?<=(^[^\n\r]*)),','</th>\n\t\t<th>'),'(?<!>)(\r\n|\r|\n)','</td>\n\t<tr>\n\t</tr>\n\t\t<td>'),'</td>\n','</th>\n','once'),',','</td>\n\t\t<td>'),'</td>\n\t</tr>\n</table>\n'])

```


```html5

<table>
	<tr>
		<th>Character</th>
		<th>Speech</th>
	<tr>
		<td>The multitude</td>
		<td>The messiah! Show us the messiah!</td>
	</tr>
	<tr>
		<td>Brians mother</td>
		<td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td>
	</tr>
	<tr>
		<td>The multitude</td>
		<td>Who are you?</td>
	</tr>
	<tr>
		<td>Brians mother</td>
		<td>I'm his mother; that's who!</td>
	</tr>
	<tr>
		<td>The multitude</td>
		<td>Behold his mother! Behold his mother!</td>
	</tr>
</table>

```



## Maxima


```Maxima
infile: "input.csv";
outfile: "table.html";
instream: openr(infile);
outstream: openw(outfile);

printf(outstream, "<TABLE border=\"1\">~%");
nr: 0;
while (line: readline(instream))#false do (
  nr: nr + 1,
  line: ssubst("&lt;", "<", line),
  line: ssubst("&gt;", ">", line),
  value_list: map(lambda([f], strim(" ", f)), split(line, ",")),
  if nr=1 then printf(outstream, "  <THEAD bgcolor=\"yellow\">") else  printf(outstream, "  <TBODY bgcolor=\"orange\">"),
  printf(outstream, "<TR>"),
  for value in value_list do printf(outstream, "<TD>~a</TD>", value),
  printf(outstream, "</TR>"),
  if nr=1 then printf(outstream, "</THEAD>~%") else  printf(outstream, "</TBODY>~%"));
printf(outstream, "</TABLE>~%");

close(instream);
close(outstream);
```



```html5
<TABLE border="1">
  <THEAD bgcolor="yellow"><TR><TD>Character</TD><TD>Speech</TD></TR></THEAD>
  <TBODY bgcolor="orange"><TR><TD>The multitude</TD><TD>The messiah! Show us the messiah!</TD></TR></TBODY>
  <TBODY bgcolor="orange"><TR><TD>Brians mother</TD><TD>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</TD></TR></TBODY>
  <TBODY bgcolor="orange"><TR><TD>The multitude</TD><TD>Who are you?</TD></TR></TBODY>
  <TBODY bgcolor="orange"><TR><TD>Brians mother</TD><TD>I'm his mother; that's who!</TD></TR></TBODY>
  <TBODY bgcolor="orange"><TR><TD>The multitude</TD><TD>Behold his mother! Behold his mother!</TD></TR></TBODY>
</TABLE>
```



## ML/I

This example carries out the special formatting for extra credit. This is a macro rather than a function, though, due to the nature of ML/I.

### Input


```ML/I
MCSKIP "WITH" NL
"" CSV to HTML
"" assumes macros on input stream 1, terminal on stream 2
MCSKIP MT,[]
MCSKIP SL WITH ~
MCINS %.
"" C1=th before header output, td afterwards
MCCVAR 1,2
MCSET C1=[th]
"" HTML escapes
MCDEF < AS [[&lt;]]
MCDEF > AS [[&gt;]]
MCDEF & AS [[&amp;]]
"" Main line processing
MCDEF SL N1 OPT , N1 OR NL ALL
AS [[   <tr>]
MCSET T2=1
%L1.MCGO L2 IF T2 GR T1
      [<]%C1.[>]%AT2.[</]%C1.[>]
MCSET T2=T2+1
MCGO L1
%L2.[   </tr>]
MCSET C1=[td]
]
[<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN">
<html>
<head>
<title>HTML converted from CSV</title>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
<style type="text/css"><!--
th {
        font-weight:bold;
        text-align:left
}
table,td,th {
        border:1px solid;
        border-collapse:collapse
}
td,th {
        padding:10px
}
//-->
</style>
</head>

<body>
<table>]
MCSET S1=1
~MCSET S10=2
~MCSET S1=0
[</table>
</body>
</html>
]
```


### Output


```ML/I
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN">
<html>
<head>
<title>HTML converted from CSV</title>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
<style type="text/css"><!--
th {
        font-weight:bold;
        text-align:left
}
table,td,th {
        border:1px solid;
        border-collapse:collapse
}
td,th {
        padding:10px
}
//-->
</style>
</head>

<body>
<table>
   <tr>
      <th>Character</th>
      <th>Speech</th>
   </tr>
   <tr>
      <td>The multitude</td>
      <td>The messiah! Show us the messiah!</td>
   </tr>
   <tr>
      <td>Brians mother</td>
      <td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a
          very naughty boy! Now go away!&lt;/angry&gt;</td>
   </tr>
   <tr>
      <td>The multitude</td>
      <td>Who are you?</td>
   </tr>
   <tr>
      <td>Brians mother</td>
      <td>I'm his mother; that's who!</td>
   </tr>
   <tr>
      <td>The multitude</td>
      <td>Behold his mother! Behold his mother!</td>
   </tr>
</table>
</body>
</html>
```



## Nanoquery


```nanoquery
// a method that converts a csv row into a html table row as a string
def toHtmlRow($record, $tag)
	$htmlrow = "\t<tr>\n"

	// loop through the values in the current csv row
	for ($i = 1) ($i <= len($record)) ($i = $i+1)
		$htmlrow = ($htmlrow + "\t\t<" + $tag + ">" + ($record ~ $i) + "</" + $tag + ">\n")
	end for

	return ($htmlrow + "\t</tr>\n")
end def

// get the name of the csv file then open it
print "filename: "
input $fname
open $fname

// allocate a string to hold the table
$htmltable = "<table>\n"

// add the column names to the table (#0 returns column names as a record object
$htmltable = ($htmltable + toHtmlRow(#0, "th"))

// add all other rows to the table
for ($i = 1) ($i < $dbsize) ($i = $i+1)
	$htmltable = ($htmltable + toHtmlRow(#$i, "td"))
end for

// close the html table
$htmltable = $htmltable+"</table>"

println $htmltable
```



## NetRexx

Uses the [[NetRexx]] solution for [[Read_a_file_line_by_line#Using_Java_Scanner|Read a file line by line]] to read the CSV file into the program.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

parse arg inFileName .
if inFileName = '' | inFileName = '.' then inFileName = './data/Brian.csv'
csv = RREadFileLineByLine01.scanFile(inFileName)

header = htmlHeader()
pre    = htmlCsvText(csv, inFileName)
table  = htmlCsvTable(csv, inFileName)
footer = htmlFooter()

say header
say pre
say table
say footer

return

method htmlHeader() public static returns Rexx
  html = '<?xml version="1.0" encoding="UTF-8"?>\n' -
      || '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">\n' -
      || '<html xmlns="http://www.w3.org/1999/xhtml" lang="en-US" xml:lang="en-US">\n' -
      || '<head>\n' -
      || '<meta http-equiv="Content-type" content="text/html;charset=UTF-8"/>\n' -
      || '<title>RCsv2Html</title>\n' -
      || '<style type="text/css">\n' -
      || '<!--\n' -
      || '/* <![DATA[ */\n' -
      || 'body {\n' -
      || '  font-family: "Verdana", "Geneva", "Helvetica Neue", "Helvetica", "DejaVu Sans", "Arial", sans-serif;\n' -
      || '}\n' -
      || 'table, th, td {\n' -
      || '  border: 1px solid black;\n' -
      || '  border-collapse: collapse;\n' -
      || '  padding: 0.25em;\n' -
      || '  font-size: 85%;\n' -
      || '}\n' -
      || 'th {\n' -
      || '  color: white;\n' -
      || '  background-color: green;\n' -
      || '}\n' -
      || 'p.classname {\n' -
      || '  font-size: inherit;\n' -
      || '}\n' -
      || '/* ]] */\n' -
      || '//-->\n' -
      || '</style>\n' -
      || '</head>\n' -
      || '<body>\n' -
      || '<h1>Rosetta Code &ndash; NetRexx Sample Output</h2>\n' -
      || '<h2><a href="http://rosettacode.org/wiki/CSV_to_HTML_translation">CSV to HTML translation</a></h2>\n' -
      || ''

  return html

method htmlFooter() public static returns Rexx
  html = '</body>\n' -
      || '</html>\n' -
      || ''
  return html

method htmlCsvText(csv, fileName = '.') public static returns Rexx
  html = '<h3>Contents of CSV <code>'fileName'</code></h3>\n' -
      || '
```txt
\n' -
      || ''
  loop row = 1 to csv[0]
    html = html || csv[row]'\n'
    end row
  html = html -
      || '
```
\n' -
      || ''
  return html

method htmlCsvTable(csv, fileName = '.') public static returns Rexx
  html = '<table>\n' -
      || '<caption>Translation of CSV <code>'fileName'</code></caption>\n' -
      || '<thead>\n' -
      || ''
  html = html -
      || htmlCsvTableRow(csv[1], 'th')'\n' -
      || '</thead>\n' -
      || '<tbody>\n' -
      || ''
  loop r_ = 2 to csv[0]
    html = html -
        || htmlCsvTableRow(csv[r_])'\n' -
        || ''
    end r_
  html = html -
      || '</tbody>\n' -
      || '</table>\n' -
      || ''
  return html

method htmlCsvTableRow(row, tag = 'td') public static returns Rexx
  row = row.strip('t')
  row = row.changestr('&', '&amp;') -- need to do this one first to avoid double translation
  row = row.changestr('"', '&quot;')
  row = row.changestr("'", '&apos;')
  row = row.changestr('<', '&lt;')
  row = row.changestr('>', '&gt;')
  elmts = ''
  elmts[0] = 0
  e_ = 0
  loop while row.length() > 0
    parse row elmt ',' row
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

'''Output:'''
<div style="height:30ex;overflow:scroll;">

```html5
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en-US" xml:lang="en-US">
<head>
<meta http-equiv="Content-type" content="text/html;charset=UTF-8"/>
<title>RCsv2Html</title>
<style type="text/css">
<!--
/* <![DATA[ */
body {
  font-family: "Verdana", "Geneva", "Helvetica Neue", "Helvetica", "DejaVu Sans", "Arial", sans-serif;
}
table, th, td {
  border: 1px solid black;
  border-collapse: collapse;
  padding: 0.25em;
  font-size: 85%;
}
th {
  color: white;
  background-color: green;
}
p.classname {
  font-size: inherit;
}
/* ]] */
//-->
</style>
</head>
<body>
<h1>Rosetta Code &ndash; NetRexx Sample Output</h2>
<h2><a href="http://rosettacode.org/wiki/CSV_to_HTML_translation">CSV to HTML translation</a></h2>

<h3>Contents of CSV <code>./data/Brian.csv</code></h3>

```txt

Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!

```


<table>
<caption>Contents of CSV <code>./data/Brian.csv</code></caption>
<thead>
<tr>
<th>Character</th>
<th>Speech</th>
</tr>

</thead>
<tbody>
<tr>
<td>The multitude</td>
<td>The messiah! Show us the messiah!</td>
</tr>

<tr>
<td>Brians mother</td>
<td>&lt;angry&gt;Now you listen here! He&apos;s not the messiah; he&apos;s a very naughty boy! Now go away!&lt;/angry&gt;</td>
</tr>

<tr>
<td>The multitude</td>
<td>Who are you?</td>
</tr>

<tr>
<td>Brians mother</td>
<td>I&apos;m his mother; that&apos;s who!</td>
</tr>

<tr>
<td>The multitude</td>
<td>Behold his mother! Behold his mother!</td>
</tr>

</tbody>
</table>

</body>
</html>

```

</div>

'''Rendered Output:'''

[[File:RCsv2HtmlNetRexx.png]]


## Nim

```nim
import cgi, strutils

const csvtext = """Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!"""

proc row2tr(row): string =
  result = "<tr>"
  let cols = xmlEncode(row).split(",")
  for col in cols:
    result.add "<td>"&col&"</td>"
  result.add "</tr>"

proc csv2html(txt): string =
  result = "<table summary=\"csv2html program output\">\n"
  for row in txt.splitLines():
    result.add "  <tbody>"&row2tr(row)&"</tbody>\n"
  result.add "</table>"

echo csv2html(csvtext)
```

 {{Out}}

```txt
<table summary="csv2html program output">
  <tbody><tr><td>Character</td><td>Speech</td></tr></tbody>
  <tbody><tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr></tbody>
  <tbody><tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr></tbody>
  <tbody><tr><td>The multitude</td><td>Who are you?</td></tr></tbody>
  <tbody><tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr></tbody>
  <tbody><tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr></tbody>
</table>
```


=={{header|Oberon-2}}==
```oberon2

MODULE CSV2HTML;
IMPORT
  Object,
  IO,
  IO:FileChannel,
  IO:TextRider,
  SB := ADT:StringBuffer,
  NPCT:Tools,
  NPCT:CGI:Utils,
  Ex := Exception,
  Out;
VAR
  fileChannel: FileChannel.Channel;
  rd: TextRider.Reader;
  line: ARRAY 1024 OF CHAR;
  table: SB.StringBuffer;
  parts: ARRAY 2 OF STRING;

  PROCEDURE DoTableHeader(sb: SB.StringBuffer;parts: ARRAY OF STRING);
  BEGIN
    sb.Append("<tr><th>"+Utils.EscapeHTML(parts[0])+"</th><th>"+Utils.EscapeHTML(parts[1])+"</th></tr>");
    sb.AppendLn
  END DoTableHeader;

  PROCEDURE DoTableRow(sb: SB.StringBuffer;parts: ARRAY OF STRING);
  BEGIN
    sb.Append("<tr><td>"+Utils.EscapeHTML(parts[0])+"</td><td>"+Utils.EscapeHTML(parts[1])+"</td></tr>");
    sb.AppendLn
  END DoTableRow;

  PROCEDURE DoTable(sb: SB.StringBuffer): STRING;
  VAR
    aux: SB.StringBuffer;
  BEGIN
     aux := SB.New("<table>");aux.AppendLn;
     RETURN aux.ToString() + sb.ToString() + "</table>";
  END DoTable;

BEGIN
  TRY
    fileChannel := FileChannel.OpenUnbuffered("script.csv",{FileChannel.read});
  CATCH Ex.Exception(ex):
    Out.Object(ex.GetMessage());Out.Ln;
    HALT(1)
  END;
  rd := TextRider.ConnectReader(fileChannel);
  (* Extract headers *)
  TRY
    rd.ReadLine(line);
    table := NEW(SB.StringBuffer,2048);
    Tools.Split(Object.NewLatin1(line),",",parts);
    DoTableHeader(table,parts);
  CATCH IO.Error(ex):
    Out.Object(ex.Name() + ": " + ex.GetMessage());Out.Ln;
    HALT(2)
  END;

  (* Extract data *)
  LOOP
    TRY
      rd.ReadLine(line);
      IF (line[0] # 0X)THEN (* skip empty lines *)
        Tools.Split(Object.NewLatin1(line),",",parts);
        DoTableRow(table,parts)
      END
    CATCH IO.Error(ex):
      EXIT
    END
  END;
  Out.Object(DoTable(table));Out.Ln;
  fileChannel.Close()
END CSV2HTML.

```

```txt

<table>
<tr><th>Character</th><th>Speech</th></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He&apos;s not the messiah; he&apos;s a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I&apos;m his mother; that&apos;s who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>

```



## Objeck

```objeck
use System.IO.File;
use Data.CSV;

class CsvToHtml {
  function : Main(args : String[]) ~ Nil {
    if(args->Size() = 1) {
      table := CsvTable->New(FileReader->ReadFile(args[0]));
      if(table->IsParsed()) {
        buffer := "<html><body><table>";
        Header(table->GetHeaders(), buffer);
        for(i := 1; i < table->Size(); i += 1;) {
          Data(table->Get(i), buffer);
        };
        buffer += "</table></body></html>";
        buffer->PrintLine();
      };
    };
  }

  function : Header(row : CsvRow, buffer : String) ~ Nil {
    buffer += "<tr>";
    each(i : row) {
      buffer += "<th>";
      buffer += Encode(row->Get(i));
      buffer += "</th>";
    };
    buffer += "</tr>";
  }

  function : Data(row : CsvRow, buffer : String) ~ Nil {
    buffer += "<tr>";
    each(i : row) {
      buffer += "<td>";
      buffer += Encode(row->Get(i));
      buffer += "</td>";
    };
    buffer += "</tr>";
  }

  function : Encode(in : String) ~ String {
    out := "";

    each(i : in) {
      c := in->Get(i);
      select(c) {
        label '&': {
          out->Append("&amp;");

        }

        label '\'': {
          out->Append("&apos;");
        }

        label '<': {
          out->Append("&lt;");
        }

        label '>': {
          out->Append("&gt;");
        }

        other: {
          out->Append(c);
        }
      };
    };

    return out;
  }
}

```


Output:

```txt

<html><body><table><tr><th>Character</th><th>Speech</th></tr><tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr><tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He&apos;s not the messiah; he&apos;s a very naughty boy! Now go away!&lt;/angry&gt;</td></tr><tr><td>The multitude</td><td>Who are you?</td></tr><tr><td>Brians mother</td><td>I&apos;m his mother; that&apos;s who!</td></tr><tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr></table></body></html>

```



## OCaml


### Simple solution


OCaml possesses a [http://forge.ocamlcore.org/projects/csv/ CSV module]
but we do not use it hereafter because the CSV data does not contain comas.


```ocaml
open Printf

let csv_data = "\
Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; \
              he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!"

(* General HTML escape *)
let escape =
  let html_escapes = Str.regexp "\\([^A-Za-z0-9 ;!?'/]\\)" in
  let esc s = sprintf "&#%04d;" (Char.code s.[Str.group_beginning 1]) in
  Str.global_substitute html_escapes esc

let nl = Str.regexp "\n\r?"
let coma = Str.regexp ","

let list_of_csv csv =
  List.map (fun l -> Str.split coma l) (Str.split nl csv)

let print_html_table segments =
  printf "<table>\n";
  List.iter (fun line ->
    printf "<tr>";
    List.iter (fun c -> printf "<td>%s</td>" (escape c)) line;
    printf "</tr>\n";
  ) segments;
  printf "</table>\n";
;;

let () =
  print_html_table (list_of_csv csv_data)
```


 {{Out|Sample html output}}

```html5><table

<tr><td>Character</td><td>Speech</td></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&#0060;angry&#0062;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&#0060;/angry&#0062;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
```



### Extra credit solution



```ocaml
open Printf

let csv_data = "\
Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; \
              he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!"

(* General HTML escape *)
let escape =
  let html_escapes = Str.regexp "\\([^A-Za-z0-9 ;!?'/]\\)" in
  let esc s = sprintf "&#%04d;" (Char.code s.[Str.group_beginning 1]) in
  Str.global_substitute html_escapes esc

let nl = Str.regexp "\n\r?"
let coma = Str.regexp ","

let list_of_csv csv =
  List.map (fun l -> Str.split coma l) (Str.split nl csv)

let print_html_table segments =
  let print_row line =
    printf "<tr>";
    List.iter (fun c -> printf "<td>%s</td>" (escape c)) line;
    printf "</tr>\n" in
  printf "<html>
  <head>
    <style type=\"text/css\">
      td {background-color:#ddddff; }
      thead td {background-color:#ddffdd; text-align:center; }
    </style>
  </head>";
  printf "<table>\n<thead>";
  print_row (List.hd segments);
  printf "</thead><tbody>\n";
  List.iter print_row (List.tl segments);
  printf "</tbody>\n</table>\n</html>";
;;

let () =
  print_html_table (list_of_csv csv_data)
```


```html5><html

  <head>
    <style type="text/css">
      td {background-color:#ddddff; }
      thead td {background-color:#ddffdd; text-align:center; }
    </style>
  </head><table>
<thead><tr><td>Character</td><td>Speech</td></tr>
</thead><tbody>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&#0060;angry&#0062;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&#0060;/angry&#0062;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</tbody>
</table>
</html>
```



## OpenEdge/Progress

<lang Progress (OpenEdge ABL)>
FUNCTION csvToHtml RETURNS CHARACTER (
   i_lhas_header  AS LOGICAL,
   i_cinput       AS CHARACTER
):

   DEFINE VARIABLE coutput AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE irow    AS INTEGER     NO-UNDO.
   DEFINE VARIABLE icolumn AS INTEGER     NO-UNDO.
   DEFINE VARIABLE crow    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE ccell   AS CHARACTER   NO-UNDO.

   coutput = "<html>~n~t<table>".

   DO irow = 1 TO NUM-ENTRIES( i_cinput, "~n":U ):

      coutput = coutput + "~n~t~t<tr>".

      crow = ENTRY( irow, i_cinput, "~n":U ).

      DO icolumn = 1 TO NUM-ENTRIES( crow ):
         ccell = ENTRY( icolumn, crow ).

         coutput = coutput + "~n~t~t~t" + IF i_lhas_header AND irow = 1 THEN "<th>" ELSE "<td>".
         coutput = coutput + REPLACE( REPLACE( REPLACE( ccell, "&", "&amp;" ), "<", "&lt;" ), ">", "&gt;" ).
         coutput = coutput + IF i_lhas_header AND irow = 1 THEN "</th>" ELSE "</td>".

      END.

      coutput = coutput + "~n~t~t</tr>".

   END.

   coutput = coutput + "~n~t</table>~n</html>".

   RETURN coutput.

END FUNCTION. /* csvToHtml */

MESSAGE
   csvToHtml(
      TRUE,
      "Character,Speech" + "~n" +
      "The multitude,The messiah! Show us the messiah!" + "~n" +
      "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>" + "~n" +
      "The multitude,Who are you?" + "~n" +
      "Brians mother,I'm his mother; that's who!" + "~n" +
      "The multitude,Behold his mother! Behold his mother!"
   )
VIEW-AS ALERT-BOX.
```

```html><html

	<table>
		<tr>
			<th>Character</th>
			<th>Speech</th>
		</tr>
		<tr>
			<td>The multitude</td>
			<td>The messiah! Show us the messiah!</td>
		</tr>
		<tr>
			<td>Brians mother</td>
			<td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td>
		</tr>
		<tr>
			<td>The multitude</td>
			<td>Who are you?</td>
		</tr>
		<tr>
			<td>Brians mother</td>
			<td>I'm his mother; that's who!</td>
		</tr>
		<tr>
			<td>The multitude</td>
			<td>Behold his mother! Behold his mother!</td>
		</tr>
	</table>
</html>
```



## Perl


Provide the CSV data as standard input. With a command-line argument, the first row will use <code><th></code> instead of <code><td></code>.


```perl
use HTML::Entities;

sub row {
    my $elem = shift;
    my @cells = map {"<$elem>$_</$elem>"} split ',', shift;
    print '<tr>', @cells, "</tr>\n";
}

my ($first, @rest) = map
    {my $x = $_; chomp $x; encode_entities $x}
    <STDIN>;
print "<table>\n";
row @ARGV ? 'th' : 'td', $first;
row 'td', $_ foreach @rest;
print "</table>\n";
```


```html5><table

<tr><th>Character</th><th>Speech</th></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He&#39;s not the messiah; he&#39;s a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I&#39;m his mother; that&#39;s who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
```



## Perl 6

A very lispy solution:

```perl6
my $str = "Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!";

# comment the next line out, if you want to read from standard input instead of the hard-coded $str above
# my $str = $*IN.slurp;

my &escape = *.trans(« & < > » => « &amp; &lt; &gt; »); # a function with one argument that escapes the entities
my &tag    = {"<$^tag>"~$^what~"</$^tag>"};

printf
'<!DOCTYPE html>
<html>
<head><title>Some Text</title></head>
<body><table>
%s
</table></body></html>
', [~]                                # concatenation reduction ('a', 'b', 'c') → 'abc'
(escape($str).split(/\n/)             # escape the string and split at newline
  ==> map -> $line {tag 'tr',         # feed that into a map, that map function will tag as 'tr, and has an argument called $line
    ([~] $line.split(/','/)\          # split $line at ',',
                                      # that / at the end is just an unspace, you can omit it, but then you have to delete
                                      # all whitespace  and comments between split(…) and .map
      .map({tag 'td', $^cell}))})\    # map those cells as td
		       .join("\n");   # append a newline for nicer output
```


```html
<!DOCTYPE html>
<html>
<head><title>Some Text</title></head>
<body><table>
<tr><td>Character</td><td>Speech</td></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table></body></html>
```



## Phix

Copy of [[CSV_to_HTML_translation#Euphoria|Euphoria]]

```Phix
constant input = "Character,Speech\n" &
    "The multitude,The messiah! Show us the messiah!\n" &
    "Brians mother,<angry>Now you listen here! He's not the messiah; " &
        "he's a very naughty boy! Now go away!</angry>\n" &
    "The multitude,Who are you?\n" &
    "Brians mother,I'm his mother; that's who!\n" &
    "The multitude,Behold his mother! Behold his mother!"

puts(1,"<table>\n<tr><td>")
for i = 1 to length(input) do
    switch input[i] do
        case '\n' then puts(1,"</td></tr>\n<tr><td>")
        case ','  then puts(1,"</td><td>")
        case '<'  then puts(1,"&lt;")
        case '>'  then puts(1,"&gt;")
        case '&'  then puts(1,"&amp;")
        case else puts(1,input[i])
    end switch
end for
puts(1,"</td></tr>\n</table>")
```

```html5><table

<tr><td>Character</td><td>Speech</td></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
```



## PHP


### Simple Solution

<Lang php>
<?php
$csv = <<<EOT
Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!
EOT;

function convert($csv)
{
  $out = [];
  array_map(function($ln) use(&$out) {
      $ln    = htmlentities($ln);
      $out[] = count($out) == 0
        ? '<thead><tr><th>'.implode('</th><th>',explode(',',$ln))."</th></tr></thead>\n"
        : '<tr><td>'.implode('</td><td>',explode(',',$ln))."</td></tr>\n";
    }, explode("\n",$csv));
  return '<table>'.implode('',$out).'</table>';
}

echo convert($csv);

```



## PicoLisp


### Simple solution


```PicoLisp
(load "@lib/http.l")

(in "text.csv"
   (<table> 'myStyle NIL NIL
      (prinl)
      (while (split (line) ",")
         (<row> NIL (ht:Prin (pack (car @))) (ht:Prin (pack (cadr @))))
         (prinl) ) ) )
```

```html5
<table class="myStyle">
<tr><td>Character</td><td>Speech</td></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
```



### Extra credit solution


```PicoLisp
(load "@lib/http.l")

(in "text.csv"
   (when (split (line) ",")
      (<table> 'myStyle NIL (mapcar '((S) (list NIL (pack S))) @)
         (prinl)
         (while (split (line) ",")
            (<row> NIL (ht:Prin (pack (car @))) (ht:Prin (pack (cadr @))))
            (prinl) ) ) ) )
```

```html5
<table class="myStyle"><tr><th>Character</th><th>Speech</th></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
```



## PowerShell


### Simple solution


```Powershell

Import-Csv -Path .\csv_html_test.csv | ConvertTo-Html -Fragment | Out-File .\csv_html_test.html

```

```html5><table

<colgroup>
<col/>
<col/>
</colgroup>
<tr><th>Character</th><th>Speech </th></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah! </td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt; </td></tr>

<tr><td>The multitude</td><td>Who are you? </td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who! </td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother! </td></tr>
</table>
```


### Extra credit solution


```Powershell

$htmlformat  = '<title>Csv to Html</title>'
$htmlformat += '<style type="text/css">'
$htmlformat += 'BODY{background-color:#663300;color:#FFCC00;font-family:Arial Narrow,sans-serif;font-size:17px;}'
$htmlformat += 'TABLE{border-width: 3px;border-style: solid;border-color: black;border-collapse: collapse;}'
$htmlformat += 'TH{border-width: 1px;padding: 3px;border-style: solid;border-color: black;background-color:#663333}'
$htmlformat += 'TD{border-width: 1px;padding: 8px;border-style: solid;border-color: black;background-color:#660033}'
$htmlformat += '</style>'
Import-Csv -Path .\csv_html_test.csv | ConvertTo-Html -Head $htmlformat -Body '<h1>Csv to Html</h1>' | Out-File .\csv_html_test.html
Invoke-Expression .\csv_html_test.html

```

```html5

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<title>Csv to Html</title><style type="text/css">BODY{background-color:#663300;color:#FFCC00;font-family:Arial Narrow,sans-serif;font-size:17px;}TABLE{border-width: 3px;border-style: solid;border-color: black;border-collapse: collapse;}TH{border-width: 1px;padding: 3px;border-style: solid;border-color: black;background-color:#663333}TD{border-width: 1px;padding: 8px;border-style: solid;border-color: black;background-color:#660033}</style>
</head><body>
<h1>Csv to Html</h1>
<table>
<colgroup>
<col/>
<col/>
</colgroup>
<tr><th>Character</th><th>Speech </th></tr>

<tr><td>The multitude</td><td>The messiah! Show us the messiah! </td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt; </td></tr>
<tr><td>The multitude</td><td>Who are you? </td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who! </td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother! </td></tr>

</table>
</body></html>

```



## Prolog

Uses DCG.
### Simple solution


```Prolog
csv_html :-
	L = "Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!",

	csv_html(L, Out, []),
	string_to_list(Str, Out),
	writeln(Str).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% simple HTML
%
csv_html(L) -->
	"<TABLE>\n",
	csv_tr(L),
	"</TABLE>".

csv_tr([]) --> [].

csv_tr(L) -->
	"<TR>\n",
	csv_td(L, S),
	"\n</TR>\n",
	csv_tr(S).

csv_td(L, S) -->
	"<TD>",
	csv_td_in(L, S),
	"</TD>".

csv_td_in([], []) --> [].

csv_td_in([10|L], L) --> [].

csv_td_in([44|L], S) -->
	"</TD><TD>",
	csv_td_in(L,S).

csv_td_in([60|T], S) -->
	"&lt;",
	csv_td_in(T, S).

csv_td_in([62|T], S) -->
	"&gt;",
	csv_td_in(T, S).

csv_td_in([H|T], S) -->
	[H],
	csv_td_in(T, S).


```

```html5><TABLE

<TR>
<TD>Character</TD><TD>Speech</TD>
</TR>
<TR>
<TD>The multitude</TD><TD>The messiah! Show us the messiah!</TD>
</TR>
<TR>
<TD>Brians mother</TD><TD>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!>&lt;/angry&gt;</TD>
</TR>
<TR>
<TD>The multitude</TD><TD>Who are you?</TD>
</TR>
<TR>
<TD>Brians mother</TD><TD>I'm his mother; that's who!</TD>
</TR>
<TR>
<TD>The multitude</TD><TD>Behold his mother! Behold his mother!</TD>
</TR>
</TABLE>

```


### Extra credit solution


```Prolog
csv_html_plus :-
	L =
"Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!",

	csv_html_plus(L, Out1, []),
	string_to_list(Str1, Out1),
	writeln(Str1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HTML +
%
csv_html_plus(L) -->
	"<TABLE>\n",
	csv_head(L, R),
	csv_body(R),
	"</TABLE>".


csv_head(L, R) -->
	"<THEAD>\n",
	csv_head_tr(L, R),
	"</THEAD>\n".


csv_head_tr(L, R) -->
	"<TR>\n",
	csv_head_th(L, R),
	"\n</TR>\n".


csv_head_th(L, S) -->
	"<TH style='color:#000; background:#FF0;'>",
	csv_head_th_in(L, S),
	"</TH>".

csv_head_th_in([], []) --> [].

csv_head_th_in([10|L], L) --> [].

csv_head_th_in([44|L], S) -->
	"</TH><TH style='color:#000; background:#FF0;'>",
	csv_head_th_in(L,S).

csv_head_th_in([H|T], S) -->
	[H],
	csv_head_th_in(T, S).


csv_body(L) -->
	"<TBODY>\n",
	csv_body_tr(L),
	"</TBODY>\n".

csv_body_tr([]) --> [].

csv_body_tr(L) -->
	"<TR>\n",
	csv_body_td(L, S),
	"\n</TR>\n",
	csv_body_tr(S).

csv_body_td(L, S) -->
	"<TD  style='color:#000; background:#8FF; border:1px #000 solid; padding:0.6em;'>",
	csv_body_td_in(L, S),
	"</TD>".

csv_body_td_in([], []) --> [].

csv_body_td_in([10|L], L) --> [].

csv_body_td_in([44|L], S) -->
	"</TD><TD  style='color:#000; background:#8FF; border:1px #000 solid; padding:0.6em;'>",
	csv_body_td_in(L,S).

csv_body_td_in([60|T], S) -->
	"&lt;",
	csv_body_td_in(T, S).

csv_body_td_in([62|T], S) -->
	"&gt;",
	csv_body_td_in(T, S).

csv_body_td_in([H|T], S) -->
	[H],
	csv_body_td_in(T, S).

```

```html5><TABLE

<THEAD>
<TR>
<TH style='color:#000; background:#FF0;'>Character</TH><TH style='color:#000; background:#FF0;'>Speech</TH>
</TR>
</THEAD>
<TBODY>
<TR>
<TD  style='color:#000; background:#8FF; border:1px #000 solid; padding:0.6em;'>The multitude</TD><TD  style='color:#000; background:#8FF; border:1px #000 solid; padding:0.6em;'>The messiah! Show us the messiah!</TD>
</TR>
<TR>
<TD  style='color:#000; background:#8FF; border:1px #000 solid; padding:0.6em;'>Brians mother</TD><TD  style='color:#000; background:#8FF; border:1px #000 solid; padding:0.6em;'>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!>&lt;/angry&gt;</TD>
</TR>
<TR>
<TD  style='color:#000; background:#8FF; border:1px #000 solid; padding:0.6em;'>The multitude</TD><TD  style='color:#000; background:#8FF; border:1px #000 solid; padding:0.6em;'>Who are you?</TD>
</TR>
<TR>
<TD  style='color:#000; background:#8FF; border:1px #000 solid; padding:0.6em;'>Brians mother</TD><TD  style='color:#000; background:#8FF; border:1px #000 solid; padding:0.6em;'>I'm his mother; that's who!</TD>
</TR>
<TR>
<TD  style='color:#000; background:#8FF; border:1px #000 solid; padding:0.6em;'>The multitude</TD><TD  style='color:#000; background:#8FF; border:1px #000 solid; padding:0.6em;'>Behold his mother! Behold his mother!</TD>
</TR>
</TBODY>
</TABLE>

```



### HTML outputs rendered in firefox browser

<div style="overflow: auto;">
[[File:Prolog_csv_to_html-1.png|500px|thumb|none]]
</div>


## Python

(Note: rendered versions of all three outputs are shown at the foot of this section).

### Simple solution


```python
csvtxt = '''\
Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!\
'''

from cgi import escape

def _row2tr(row, attr=None):
    cols = escape(row).split(',')
    return ('<TR>'
            + ''.join('<TD>%s</TD>' % data for data in cols)
            + '</TR>')

def csv2html(txt):
    htmltxt = '<TABLE summary="csv2html program output">\n'
    for rownum, row in enumerate(txt.split('\n')):
        htmlrow = _row2tr(row)
        htmlrow = '  <TBODY>%s</TBODY>\n' % htmlrow
        htmltxt += htmlrow
    htmltxt += '</TABLE>\n'
    return htmltxt

htmltxt = csv2html(csvtxt)
print(htmltxt)
```


'''Sample HTML output'''


```html5
<TABLE summary="csv2html program output">
  <TBODY><TR><TD>Character</TD><TD>Speech</TD></TR></TBODY>
  <TBODY><TR><TD>The multitude</TD><TD>The messiah! Show us the messiah!</TD></TR></TBODY>
  <TBODY><TR><TD>Brians mother</TD><TD>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</TD></TR></TBODY>
  <TBODY><TR><TD>The multitude</TD><TD>Who are you?</TD></TR></TBODY>
  <TBODY><TR><TD>Brians mother</TD><TD>I'm his mother; that's who!</TD></TR></TBODY>
  <TBODY><TR><TD>The multitude</TD><TD>Behold his mother! Behold his mother!</TD></TR></TBODY>
</TABLE>
```



### Extra credit solution


```python
def _row2trextra(row, attr=None):
    cols = escape(row).split(',')
    attr_tr = attr.get('TR', '')
    attr_td = attr.get('TD', '')
    return (('<TR%s>' % attr_tr)
            + ''.join('<TD%s>%s</TD>' % (attr_td, data) for data in cols)
            + '</TR>')

def csv2htmlextra(txt, header=True, attr=None):
    ' attr is a dictionary mapping tags to attributes to add to that tag'

    attr_table = attr.get('TABLE', '')
    attr_thead = attr.get('THEAD', '')
    attr_tbody = attr.get('TBODY', '')
    htmltxt = '<TABLE%s>\n' % attr_table
    for rownum, row in enumerate(txt.split('\n')):
        htmlrow = _row2trextra(row, attr)
        rowclass = ('THEAD%s' % attr_thead) if (header and rownum == 0) else ('TBODY%s' % attr_tbody)
        htmlrow = '  <%s>%s</%s>\n' % (rowclass, htmlrow, rowclass[:5])
        htmltxt += htmlrow
    htmltxt += '</TABLE>\n'
    return htmltxt

htmltxt = csv2htmlextra(csvtxt, True,
                        dict(TABLE=' border="1" summary="csv2html extra program output"',
                             THEAD=' bgcolor="yellow"',
                             TBODY=' bgcolor="orange"'
                             )
                        )
print(htmltxt)
```


'''Sample HTML output'''
<!--
The raw HTML would not render correctly through the wiki interface but shows a suitably coloured table with cell borders.
-->

```html5
<TABLE border="1" summary="csv2html extra program output">
  <THEAD bgcolor="yellow"><TR><TD>Character</TD><TD>Speech</TD></TR></THEAD>
  <TBODY bgcolor="orange"><TR><TD>The multitude</TD><TD>The messiah! Show us the messiah!</TD></TR></TBODY>
  <TBODY bgcolor="orange"><TR><TD>Brians mother</TD><TD>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</TD></TR></TBODY>
  <TBODY bgcolor="orange"><TR><TD>The multitude</TD><TD>Who are you?</TD></TR></TBODY>
  <TBODY bgcolor="orange"><TR><TD>Brians mother</TD><TD>I'm his mother; that's who!</TD></TR></TBODY>
  <TBODY bgcolor="orange"><TR><TD>The multitude</TD><TD>Behold his mother! Behold his mother!</TD></TR></TBODY>
</TABLE>
```



### Robust solution


This solution uses the CSV parser and HTML-capable XML serializer included in the Python standard library to produce the same fancy table as in the "extra credit" version.

While not strictly necessary for the very constrained input given in Rosetta Code, using readily available high-level APIs is idiomatic for Python, makes bugs easier to catch, and this also demonstrates the kind of "safe to use with more general inputs" code that is good to get in the habit of reaching for when dealing with real-world inputs and outputs.

(eg. Suppose an earlier stage in the pipeline is using a proper CSV-generating library but is running in a locale that uses commas for decimal separators and has a bug that causes it to unexpectedly start feeding pretty-printed floating-point in. Using a proper CSV reader minimizes the potential harm and helps the program to raise errors in the most elucidating place.)

Since the version of ElementTree in the standard library does not support pretty-printing, the output it produces is minified. Unlike the "extra credit" version, this doesn't put each <code>&lt;TR&gt;</code> element in its own <code>&lt;TBODY&gt;</code>.


```python
from csv import DictReader
from xml.etree import ElementTree as ET

def csv2html_robust(txt, header=True, attr=None):
    # Use DictReader because, despite what the docs say, reader() doesn't
    # return an object with .fieldnames
    # (DictReader expects an iterable that returns lines, so split on \n)
    reader = DictReader(txt.split('\n'))

    table = ET.Element("TABLE", **attr.get('TABLE', {}))
    thead_tr = ET.SubElement(
        ET.SubElement(table, "THEAD", **attr.get('THEAD', {})),
        "TR")
    tbody = ET.SubElement(table, "TBODY", **attr.get('TBODY', {}))

    if header:
        for name in reader.fieldnames:
            ET.SubElement(thead_tr, "TD").text = name

    for row in reader:
        tr_elem = ET.SubElement(tbody, "TR", **attr.get('TR', {}))

        # Use reader.fieldnames to query `row` in the correct order.
        # (`row` isn't an OrderedDict prior to Python 3.6)
        for field in reader.fieldnames:
            td_elem = ET.SubElement(tr_elem, "TD", **attr.get('TD', {}))
            td_elem.text = row[field]

    return ET.tostring(table, method='html')

htmltxt = csv2html_robust(csvtxt, True, {
    'TABLE': {'border': "1", 'summary': "csv2html extra program output"},
    'THEAD': {'bgcolor': "yellow"},
    'TBODY': {'bgcolor': "orange"}
})

print(htmltxt.decode('utf8'))
```


'''Sample HTML output'''

<!--
The only difference between this and The output is semantically identical to the "extra credit" version, but whitespace has been collapsed as if it had been run through a minifier.
-->

```html5
<TABLE border="1" summary="csv2html extra program output"><THEAD bgcolor="yellow"><TR><TD>Character</TD><TD>Speech</TD></TR></THEAD><TBODY bgcolor="orange"><TR><TD>The multitude</TD><TD>The messiah! Show us the messiah!</TD></TR><TR><TD>Brians mother</TD><TD>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</TD></TR><TR><TD>The multitude</TD><TD>Who are you?</TD></TR><TR><TD>Brians mother</TD><TD>I'm his mother; that's who!</TD></TR><TR><TD>The multitude</TD><TD>Behold his mother! Behold his mother!</TD></TR></TBODY></TABLE>
```



### HTML outputs rendered in firefox browser

[[File:Csv2html.PNG|500px|thumb|none]]


## Racket


Uses X-exprs:

```racket
#lang racket

(define e.g.-CSV
  (string-join
   '("Character,Speech"
     "The multitude,The messiah! Show us the messiah!"
     "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>"
     "The multitude,Who are you?"
     "Brians mother,I'm his mother; that's who!"
     "The multitude,Behold his mother! Behold his mother!")
   "\n"))

(define (CSV-lines->HTML-table csv)
  (define csv-rows
    (regexp-split "\n" csv))
  (define csv-row-cells
    (map (lambda (row) (regexp-split "," row)) csv-rows))
  (define (cell-data->HTML-data data)
    `(td () ,data))
  (define (row-data->HTML-row CSV-row)
    `(tr () ,@(map cell-data->HTML-data CSV-row) "\n"))
  `(table
    (thead
     ,(row-data->HTML-row (car csv-row-cells)))
    (tbody ,@(map row-data->HTML-row (cdr csv-row-cells)))))

(require xml)
(display (xexpr->string (CSV-lines->HTML-table e.g.-CSV)))
```


'''Sample HTML output:'''


```html5><table><thead><tr><td>Character</td><td>Speech</td

</tr></thead><tbody><tr><td>The multitude</td><td>The messiah! Show us the messiah!</td>
</tr><tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td>
</tr><tr><td>The multitude</td><td>Who are you?</td>
</tr><tr><td>Brians mother</td><td>I'm his mother; that's who!</td>
</tr><tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td>
</tr></tbody></table>
```


## Red


```Red
Red []

csv: {Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!}

add2html: func [ bl ] [append html rejoin bl ]  ;; helper function to add block data to html string

;;----------------------------------------------------------------------
csv2html: func ["function to generate string with html table from csv data file"
;;----------------------------------------------------------------------
    s [string!] "input .csv data"
][
arr: split s newline    ;; generate array (series) from string
html: copy "<table border=1>^/" ;; init html string

forall arr  [  ;; i use forall here so that i can test for head? of series ...
  either head? arr [ append html "<tr bgcolor=wheat>"]
                      [ append html "<tr>"]
  replace/all first arr "<" "&lt;"    ;; escape "<" and ">" characters
  replace/all first arr ">" "&gt;"
  foreach col split first arr "," [
      either head? arr [
        add2html ['<th> col '</th>]
      ][
        add2html ['<td> col '</td>]
      ]
  ]
  add2html ['</tr> newline]
]
return add2html ['</table>]
]
;;----------------------------------------------------------------------

print csv2html csv                 ;; call function
write %data.html csv2html csv   ;; write to file

```

output

```txt

<table border=1>
<tr bgcolor=wheat><th>Character</th><th>Speech</th></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>

```

'''Sample HTML output'''
<table border=1>
<tr bgcolor=wheat><th>Character</th><th>Speech</th></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>

## Retro


```Retro
remapping off
"Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!" remapping on
keepString constant CSV

: display  ( c- )
  [ ', = ] [ drop "</td><td>" puts ] when
  [ 10 = ] [ drop "</td></tr>\n<tr><td>" puts ] when
  [ '< = ] [ drop "&lt;"  puts ] when
  [ '> = ] [ drop "&gt;"  puts ] when
  [ '& = ] [ drop "&amp;" puts ] when
  putc ;

: displayHTML  ( $- )
  "<table>\n<tr><td>" puts
  [ @ display ] ^types'STRING each@
  "</td></tr>\n</table>" puts ;

CSV displayHTML
```


```html5><table

<tr><td>Character</td><td>Speech</td></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
```



## REXX

The rendered output was verified using Firefox Aurora with:
:::*   file:///c:/output.html
:::*   file:///c:/outputh.html

```rexx
/*REXX program converts   CSV  ───►  HTML  table   representing  the  CSV  data.        */
arg header_ .                                    /*obtain an uppercase version of args. */
wantsHdr= (header_=='HEADER')                    /*is the arg (low/upp/mix case)=HEADER?*/
                                                 /* [↑]  determine if user wants a hdr. */
                  iFID= 'CSV_HTML.TXT'           /*the   input  fileID  to be used.     */
if wantsHdr  then oFID= 'OUTPUTH.HTML'           /*the  output  fileID  with     header.*/
             else oFID= 'OUTPUT.HTML'            /* "      "      "     without     "   */

    do rows=0  while  lines(iFID)\==0            /*read the rows from a (text/txt) file.*/
    row.rows= strip( linein(iFID) )
    end   /*rows*/

convFrom= '&      <     >     "'                 /*special characters to be converted.  */
convTo  = '&amp;  &lt;  &gt;  &quot;'            /*display what they are converted into.*/

call write , '<html>'
call write , '<table border=4 cellpadding=9 cellspacing=1>'

  do j=0 for rows;    call write 5, '<tr>'
                           tx= 'td'
  if wantsHdr & j==0  then tx= 'th'              /*if user wants a header, then oblige. */

       do  while  row.j\=='';          parse var row.j yyy "," row.j
           do k=1  for words(convFrom)
           yyy=changestr( word( convFrom, k), yyy, word( convTo, k))
           end   /*k*/
       call write 10, '<'tx">"yyy'</'tx">"
       end       /*forever*/
  end            /*j*/

call write 5, '<tr>'
call write  , '</table>'
call write  , '</html>'
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
write:   call lineout  oFID,  left('', 0 || arg(1) )arg(2);                         return
```

Some older REXXes don't have a   '''changestr'''   BIF, so one is included here   ──►   [[CHANGESTR.REX]].


'''output'''

```txt

<html>
<table border=4 cellpadding=9 cellspacing=1>
     <tr>
          <td>Character</td>
          <td>Speech</td>
     <tr>
          <td>The multitude</td>
          <td>The messiah! Show us the messiah!</td>
     <tr>
          <td>Brians mother</td>
          <td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td>
     <tr>
          <td>The multitude</td>
          <td>Who are you?</td>
     <tr>
          <td>Brians mother</td>
          <td>I'm his mother; that's who!</td>
     <tr>
          <td>The multitude</td>
          <td>Behold his mother! Behold his mother!</td>
     <tr>
</table>
</html>

```

'''rendered output'''


<table border=4 cellpadding=9 cellspacing=1>
     <tr>
          <td>Character</td>
          <td>Speech</td>
     <tr>
          <td>The multitude</td>
          <td>The messiah! Show us the messiah!</td>
     <tr>
          <td>Brians mother</td>
          <td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td>
     <tr>
          <td>The multitude</td>
          <td>Who are you?</td>
     <tr>
          <td>Brians mother</td>
          <td>I'm his mother; that's who!</td>
     <tr>
          <td>The multitude</td>
          <td>Behold his mother! Behold his mother!</td>
     <tr>
</table>


'''output'''   (extra credit solution)   when the first argument is HEADER in upper/lower/mixed case (with/without leading/trailing blanks).

```txt

<html>
<table border=4 cellpadding=9 cellspacing=1>
     <tr>
          <th>Character</th>
          <th>Speech</th>
     <tr>
          <td>The multitude</td>
          <td>The messiah! Show us the messiah!</td>
     <tr>
          <td>Brians mother</td>
          <td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td>
     <tr>
          <td>The multitude</td>
          <td>Who are you?</td>
     <tr>
          <td>Brians mother</td>
          <td>I'm his mother; that's who!</td>
     <tr>
          <td>The multitude</td>
          <td>Behold his mother! Behold his mother!</td>
     <tr>
</table>
</html>

```



'''rendered output'''


<table border=4 cellpadding=9 cellspacing=1>
     <tr>
          <th>Character</th>
          <th>Speech</th>
     <tr>
          <td>The multitude</td>
          <td>The messiah! Show us the messiah!</td>
     <tr>
          <td>Brians mother</td>
          <td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td>
     <tr>
          <td>The multitude</td>
          <td>Who are you?</td>
     <tr>
          <td>Brians mother</td>
          <td>I'm his mother; that's who!</td>
     <tr>
          <td>The multitude</td>
          <td>Behold his mother! Behold his mother!</td>
     <tr>
</table>





## Ruby

The extra credit version has one extra line compared to the non-extra credit version.
To output a header, simply add "header" to the command line:
  ruby csv2html.rb header
I/O is done through standard input/output.

```ruby
require 'cgi'

puts '<table summary="csv2html program output">'

def row2html str, wrap = "td"
  "<tr>" +
    str.split(",").map { |cell| "<#{wrap}>#{CGI.escapeHTML cell}</#{wrap}>" }.join +
  "</tr>"
end

puts row2html gets.chomp, "th" if ARGV.delete "header"

while str = gets
  puts row2html str.chomp
end

puts "</table>"
```

```html5
<table summary="csv2html program output">
<tr><th>Character</th><th>Speech</th></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
```



## Run BASIC

The extra credit version has 2 extra lines of code to get the heading.

```rnbasic
csv$ = "Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!"

k	= instr(csv$,",")                                 ' 2 exra lines to get heading
csv$  = left$(csv$,k - 1) + "</th><th> + mid$(csv$,k + 1)

csv$ = strRep$(csv$,",","</td><td>")
html "<table border=1><TR bgcolor=wheat align=center><th>";strRep$(csv$,chr$(13),"</td></tr><tr><td>");"</td></tr></table"
wait
' --------------------------------
' string replace rep str with
' --------------------------------
FUNCTION strRep$(strRep$,rep$,with$)
ln	= len(rep$)
k	= instr(strRep$,rep$)
while k
	strRep$	= left$(strRep$,k - 1) + with$ + mid$(strRep$,k + ln)
	k	= instr(strRep$,rep$)
WEND
END FUNCTION
```

<table border=1><TR bgcolor=wheat align=center><th>Character</th><th>Speech</th></TR>
<TR><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td><angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry></td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr></table>


## Rust

```rust
static INPUT : &'static str  =
"Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!";

fn main() {
    print!("<table>\n<tr><td>");
    for c in INPUT.chars() {
        match c {
            '\n' => print!("</td></tr>\n<tr><td>"),
            ','  => print!("</td><td>"),
            '<'  => print!("&lt;"),
            '>'  => print!("&gt;"),
            '&'  => print!("&amp;"),
            _    => print!("{}", c)
        }
    }
    println!("</td></tr>\n</table>");
}

```

```html5><table

<tr><td>Character</td><td>Speech</td></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
```



## Scala

Scala has built-in support for XML, so you can freely mix XML literals into your Scala source code. This is nice, because instead of using strings to represent XML, you create XML literals that the compiler can understand and verify. This approach lets you easily generate dynamic XML by interweaving Scala code and XML in the same expressions.
```scala
object CsvToHTML extends App {
  val header = <head>
    <title>CsvToHTML</title>
    <style type="text/css">
      td {{background-color:#ddddff; }} thead td {{background-color:#ddffdd; text-align:center; }}
    </style>
  </head>
  val csv =
    """Character,Speech
      |The multitude,The messiah! Show us the messiah!
      |Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
      |The multitude,Who are you?
      |Brians mother,I'm his mother; that's who!
      |The multitude,Behold his mother! Behold his mother!""".stripMargin

  def csv2html(csv: String, withHead: Boolean) = {

    def processRow(text: String) = <tr>
      {text.split(',').map(s => <td>
        {s}
      </td>)}
    </tr>

    val (first :: rest) = csv.lines.toList // Separate the header and the rest

    def tableHead = if (withHead)
      <thead>
        {processRow(first)}
      </thead>
    else processRow(first)

    <html>
      {header}<body>
      <table>
        {tableHead}{rest.map(processRow)}
      </table>
    </body>
    </html>
  }

  println(csv2html(csv, true))
}
```
```html><html

      <head>
    <title>CsvToHTML</title>
    <style type="text/css">
      td {background-color:#ddddff; } thead td {background-color:#ddffdd; text-align:center; }
    </style>
  </head><body>
      <table>
        <thead>
        <tr>
      <td>
        Character
      </td><td>
        Speech
      </td>
    </tr>
      </thead><tr>
      <td>
        The multitude
      </td><td>
        The messiah! Show us the messiah!
      </td>
    </tr><tr>
      <td>
        Brians mother
      </td><td>
        &lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;
      </td>
    </tr><tr>
      <td>
        The multitude
      </td><td>
        Who are you?
      </td>
    </tr><tr>
      <td>
        Brians mother
      </td><td>
        I'm his mother; that's who!
      </td>
    </tr><tr>
      <td>
        The multitude
      </td><td>
        Behold his mother! Behold his mother!
      </td>
    </tr>
      </table>
    </body>
    </html>
```



## Sed


File csv2html.sed

```sed
#!/bin/sed -f

s|<|\&lt;|g
s|>|\&gt;|g
s|^| <tr>\n  <td>|
s|,|</td>\n  <td>|
s|$|</td>\n </tr>|
1s|^|<table>\n|
$s|$|\n</table>|
```



```txt
$ sed -f csv2html.sed input.csv
```



```html5><table

 <tr>
  <td>Character</td>
  <td>Speech</td>
 </tr>
 <tr>
  <td>The multitude</td>
  <td>The messiah! Show us the messiah!</td>
 </tr>
 <tr>
  <td>Brians mother</td>
  <td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td>
 </tr>
 <tr>
  <td>The multitude</td>
  <td>Who are you?</td>
 </tr>
 <tr>
  <td>Brians mother</td>
  <td>I'm his mother; that's who!</td>
 </tr>
 <tr>
  <td>The multitude</td>
  <td>Behold his mother! Behold his mother!</td>
 </tr>
</table>
```



## Seed7

The library [http://seed7.sourceforge.net/libraries/html_ent.htm html_ent.s7i] defines the
function [http://seed7.sourceforge.net/libraries/html_ent.htm#encodeHtmlContent%28in_string%29 encodeHtmlContent],
which replaces characters with HTML entities. E.g.: '<' is replaced by ''&amp;lt;''.

```seed7
$ include "seed7_05.s7i";
  include "html_ent.s7i";

const string: csvData is "\
    \Character,Speech\n\
    \The multitude,The messiah! Show us the messiah!\n\
    \Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n\
    \The multitude,Who are you?\n\
    \Brians mother,I'm his mother; that's who!\n\
    \The multitude,Behold his mother! Behold his mother!\n";

const proc: main is func
  local
    var string: line is "";
    var string: column is "";
    const array [boolean] string: columnStartTag is [boolean] ("<td>", "<th>");
    const array [boolean] string: columnEndTag is [boolean] ("</td>", "</th>");
    var boolean: firstLine is TRUE;
  begin
    writeln("<table>");
    for line range split(csvData, '\n') do
      write("<tr>");
      for column range split(line, ',') do
        write(columnStartTag[firstLine] <& encodeHtmlContent(column) <& columnEndTag[firstLine]);
      end for;
      writeln("</tr>");
      firstLine := FALSE;
    end for;
    writeln("</table>");
  end func;
```


```html5

<table>
<tr><th>Character</th><th>Speech</th></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry></td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
<tr><td></td></tr>
</table>

```


{{Out}} viewed with a browser:
<table>
<tr><th>Character</th><th>Speech</th></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry></td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
<tr><td></td></tr>
</table>


## Sidef


```ruby
func escape(str) { str.trans(« & < > », « &amp; &lt; &gt; ») }
func tag(t, d)   { "<#{t}>#{d}</#{t}>" }

func csv2html(str) {

    var template = <<-'EOT'
    <!DOCTYPE html>
    <html>
    <head><title>Some Text</title></head>
    <body><table>
    %s
    </table></body></html>
    EOT

    template.sprintf(escape(str).lines.map{ |line|
            tag('tr', line.split(',').map{|cell| tag('td', cell) }.join)
        }.join("\n")
    )
}

var str = <<'EOT';
Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!
EOT

print csv2html(str)
```

```html5
<!DOCTYPE html>
<html>
<head><title>Some Text</title></head>
<body><table>
<tr><td>Character</td><td>Speech</td></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table></body></html>
```



## Tcl

```tcl
package require Tcl 8.5
package require csv
package require html
package require struct::queue

set csvData "Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!"

struct::queue rows
foreach line [split $csvData "\n"] {
    csv::split2queue rows $line
}
html::init
puts [subst {
    [html::openTag table {summary="csv2html program output"}]
    [html::while {[rows size]} {
	[html::row {*}[html::quoteFormValue [rows get]]]
    }]
    [html::closeTag]
}]
```


Extra credit version:

```tcl
package require Tcl 8.5
package require csv
package require html
package require struct::queue

set csvData "Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!"

html::init {
    table.border 1
    table.summary "csv2html program output"
    tr.bgcolor orange
}

# Helpers; the html package is a little primitive otherwise
proc table {contents {opts ""}} {
    set out [html::openTag table $opts]
    append out [uplevel 1 [list subst $contents]]
    append out [html::closeTag]
}
proc tr {list {ropt ""}} {
    set out [html::openTag tr $ropt]
    foreach x $list {append out [html::cell "" $x td]}
    append out [html::closeTag]
}

# Parse the CSV data
struct::queue rows
foreach line [split $csvData "\n"] {
    csv::split2queue rows $line
}

# Generate the output
puts [subst {
    [table {
	[tr [html::quoteFormValue [rows get]] {bgcolor="yellow"}]
	[html::while {[rows size]} {
	    [tr [html::quoteFormValue [rows get]]]
	}]
    }]
}]
```

```html5
<table border="1" summary="csv2html program output">
    <tr bgcolor="yellow"><td>Character</td><td>Speech</td></tr>
    <tr bgcolor="orange"><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
    <tr bgcolor="orange"><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He&#39;s not the messiah; he&#39;s a very naughty boy! Now go away! &lt;/angry&gt;</td></tr>
    <tr bgcolor="orange"><td>The multitude</td><td>Who are you?</td></tr>
    <tr bgcolor="orange"><td>Brians mother</td><td>I&#39;m his mother; that&#39;s who!</td></tr>
    <tr bgcolor="orange"><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
MODE DATA
$$ csv=*
Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!
$$ htmlhead=*
<!DOCTYPE html system>
<html>
<head>
<title>Life of Brian</title>
<style type="text/css">
th {background-color:orange}
td {background-color:yellow}
</style></head><body><table>
$$ BUILD X_TABLE txt2html=*
 << &lt;
 >> &gt;
$$ MODE TUSCRIPT
file="brian.html"
ERROR/STOP CREATE (file,FDF-o,-std-)
csv=EXCHANGE (csv,txt2html)
x=SPLIT (csv,":,:",row1,row2)
ACCESS html: WRITE/ERASE/RECORDS/UTF8 $file s,html
WRITE html htmlhead
LOOP n,td1=row1,td2=row2
IF (n==1) THEN
row=CONCAT ("<tr><th>",td1,"</th><th>",td2,"</th></tr>")
ELSE
row=CONCAT ("<tr><td>",td1,"</td><td>",td2,"</td></tr>")
ENDIF
WRITE html row
ENDLOOP
WRITE html "</table></body></html>"
ENDACCESS/PRINT html

```

=== Output (source code) ===

```html5
<!DOCTYPE html system>
<html>
<head>
<title>Life of Brian</title>
<style type="text/css">
th {background-color:orange}
td {background-color:yellow}
</style></head><body><table>
<tr><th>Character</th><th>Speech</th></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table></body></html>

```

=== Output (rendered) ===
[[file:tuscript_csv2html.png|500px|thumb|none|rendered by browser ]]



## TXR



### =Simple=



```txr
@(collect)
@char,@speech
@(end)
@(output :filter :to_html)
<table>
@  (repeat)
  <tr>
     <td>@char</td>
     <td>@speech</td>
  </tr>
@  (end)
</table>
@(end)
```


```txt
$ txr csv.txr  csv.txt
```



```html5><table

  <tr>
     <td>Character</td>
     <td>Speech</td>
  </tr>
  <tr>
     <td>The multitude</td>
     <td>The messiah! Show us the messiah!</td>
  </tr>
  <tr>
     <td>Brians mother</td>
     <td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td>
  </tr>
  <tr>
     <td>The multitude</td>
     <td>Who are you?</td>
  </tr>
  <tr>
     <td>Brians mother</td>
     <td>I'm his mother; that's who!</td>
  </tr>
  <tr>
     <td>The multitude</td>
     <td>Behold his mother! Behold his mother!</td>
  </tr>
</table>
```



### =With Styling=



```txr
@(collect)
@char,@speech
@(end)
@(output :filter :to_html)
<style type="text/css">
tr.odd td {
  background-color: #CC9999; color: black;
}
tr.even td {
  background-color: #9999CC; color: black;
}
th {
  background-color: #99CC99; color: black;
}
</style>
<table>
@  (repeat :counter row)
  <tr class="@(if (evenp row) 'even 'odd)">
     <td>@char</td>
     <td>@speech</td>
  </tr>
@  (first)
  <tr>
     <th>@char</th>
     <th>@speech</th>
  </tr>
@  (end)
</table>
@(end)
```


```txt
$ txr csv2.txr  csv.txt
```


```html5
<style type="text/css">
tr.odd td {
  background-color: #CC9999; color: black;
}
tr.even td {
  background-color: #9999CC; color: black;
}
th {
  background-color: #99CC99; color: black;
}
</style>
<table>
  <tr>
     <th>Character</th>
     <th>Speech</th>
  </tr>
  <tr class="odd">
     <td>The multitude</td>
     <td>The messiah! Show us the messiah!</td>
  </tr>
  <tr class="even">
     <td>Brians mother</td>
     <td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td>
  </tr>
  <tr class="odd">
     <td>The multitude</td>
     <td>Who are you?</td>
  </tr>
  <tr class="even">
     <td>Brians mother</td>
     <td>I'm his mother; that's who!</td>
  </tr>
  <tr class="odd">
     <td>The multitude</td>
     <td>Behold his mother! Behold his mother!</td>
  </tr>
</table>
```



## UNIX Shell

```bash
csv2html() {
    IFS=,
    echo "<table>"

    echo "<thead>"
    read -r speaker text
    htmlrow "$speaker" "$text" th
    echo "</thead>"

    echo "<tbody>"
    while read -r speaker text; do
        htmlrow "$speaker" "$text"
    done
    echo "</tbody>"
    echo "</table>"
}

htmlrow() {
    cell=${3:-td}
    printf "<tr><%s>%s</%s><%s>%s</%s></tr>\n" \
        "$cell" "$(escape_html "$1")" "$cell" \
        "$cell" "$(escape_html "$2")" "$cell"
}

escape_html() {
    str=${1//\&/&amp;}
    str=${str//</&lt;}
    str=${str//>/&gt;}
    echo "$str"
}

html=$(
    csv2html <<-END
	Character,Speech
	The multitude,The messiah! Show us the messiah!
	Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
	The multitude,Who are you?
	Brians mother,I'm his mother; that's who!
	The multitude,Behold his mother! Behold his mother!
	END
)
echo "$html"
```


```html5><table

<thead>
<tr><th>Character</th><th>Speech</th></tr>
</thead>
<tbody>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</tbody>
</table>
```



## VBA

```vb
Public Sub CSV_TO_HTML()
    input_ = "Character,Speech\n" & _
        "The multitude,The messiah! Show us the messiah!\n" & _
        "Brians mother,<angry>Now you listen here! He's not the messiah; " & _
            "he's a very naughty boy! Now go away!</angry>\n" & _
        "The multitude,Who are you?\n" & _
        "Brians mother,I'm his mother; that's who!\n" & _
        "The multitude,Behold his mother! Behold his mother!"

    Debug.Print "<table>" & vbCrLf & "<tr><td>"
    For i = 1 To Len(input_)
        Select Case Mid(input_, i, 1)
            Case "\"
                If Mid(input_, i + 1, 1) = "n" Then
                    Debug.Print "</td></tr>" & vbCrLf & "<tr><td>";
                    i = i + 1
                Else
                    Debug.Print Mid(input_, i, 1);
                End If
            Case ",": Debug.Print "</td><td>";
            Case "<": Debug.Print "&lt;";
            Case ">": Debug.Print "&gt;";
            Case "&": Debug.Print "&amp;";
            Case Else: Debug.Print Mid(input_, i, 1);
        End Select
    Next i
    Debug.Print "</td></tr>" & vbCrLf & "</table>"
End Sub
```
```html5><table

<tr><td>
Character</td><td>Speech</td></tr>
<tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>
<tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>
<tr><td>The multitude</td><td>Who are you?</td></tr>
<tr><td>Brians mother</td><td>I'm his mother; that's who!</td></tr>
<tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
```


## VBScript


```vb

Set objfso = CreateObject("Scripting.FileSystemObject")

parent_folder = objfso.GetParentFolderName(WScript.ScriptFullName) & "\"

Set objcsv = objfso.OpenTextFile(parent_folder & "in.csv",1,False)
Set objhtml = objfso.OpenTextFile(paren_folder & "out.html",2,True)

objhtml.Write(csv_to_html(objcsv.ReadAll))

objcsv.Close
objhtml.Close
Set objfso = Nothing

Function csv_to_html(s)
	row = Split(s,vbCrLf)
	'write the header
	tmp = "<html><head><head/><body><table border=1 cellpadding=10 cellspacing=0>"
	For i = 0 To UBound(row)
		field = Split(row(i),",")
		If i = 0 Then
			tmp = tmp & "<tr><th>" & replace_chars(field(0)) & "</th><th>" & replace_chars(field(1)) & "</th><tr>"
		Else
			tmp = tmp & "<tr><td>" & replace_chars(field(0)) & "</td><td>" & replace_chars(field(1)) & "</td><tr>"
		End If
	Next
	'write the footer
	tmp = tmp & "</table></body></html>"
	csv_to_html = tmp
End Function

Function replace_chars(s)
	replace_chars = Replace(Replace(s,"<","&lt;"),">","&gt;")
End Function

```


Format derived from BBC BASIC output.
<table border=1 cellpadding=10 cellspacing=0><tr><th>Character</th><th>Speech</th><tr><tr><td>The multitude</td><td>The messiah! Show us the messiah!</td><tr><tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td><tr><tr><td>The multitude</td><td>Who are you?</td><tr><tr><td>Brians mother</td><td>I'm his mother; that's who!</td><tr><tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td><tr></table>


## Vedit macro language


This solution converts the table in-place in current edit buffer.
If a block is highlighted, only the block is converted. If no block highlighted, the entire file is converted.


```vedit
if (BB < 0) {           // block not set
    BB(0)               // convert entire file
    BE(File_Size)
}

// Convert special characters into entities
Replace_Block("&","&amp;", BB, BE, BEGIN+ALL+NOERR)
Replace_Block("<","&lt;", BB, BE, BEGIN+ALL+NOERR)
Replace_Block(">","&gt;", BB, BE, BEGIN+ALL+NOERR)

// Convert CSV into HTML table
Goto_Pos(BB)
IT('<table>') IN
#80 = Cur_Pos
Goto_Pos(BE)
IT("</table>")
#81 = Cur_Line
IN
Goto_Pos(#80)
while (Cur_Line < #81) {
    IT("    <tr><td>")
    Replace_Block(",","</td><td>",Cur_Pos,EOL_Pos,ALL+NOERR)
    EOL
    IT("</td></tr>")
    Line(1)
}
BB(Clear)
```


```html5><table

    <tr><td>Character</td><td>Speech </td></tr>
    <tr><td>The multitude</td><td>The messiah! Show us the messiah! </td></tr>
    <tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt; </td></tr>
    <tr><td>The multitude</td><td>Who are you? </td></tr>
    <tr><td>Brians mother</td><td>I'm his mother; that's who! </td></tr>
    <tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>
</table>
```



## Visual Basic .NET

Uses XML literals and the TextFieldParser class of the VB runtime, which can parse delimited or fixed-width files.

The optional first command-line argument denotes whether to use <thead /> for the first row. The optional second argument specifies the path of the CSV file. If no second argument is given, the program reads from the console until stop characters are encountered.

TextFieldParser is designed to work with files and so makes heavy use of peeking, which results in buggy behavior when signaling end-of-file using the console. The most reliable way seems to be alternately pressing enter and Ctrl+Z after the last character of the last line of data.


```vbnet
Imports Microsoft.VisualBasic.FileIO

Module Program
    Sub Main(args As String())
        Dim parser As TextFieldParser
        Try
            If args.Length > 1 Then
                parser = My.Computer.FileSystem.OpenTextFieldParser(args(1), ",")
            Else
                parser = New TextFieldParser(Console.In) With {.Delimiters = {","}}
            End If

            Dim getLines =
            Iterator Function() As IEnumerable(Of String())
                Do Until parser.EndOfData
                    Yield parser.ReadFields()
                Loop
            End Function

            Dim result = CSVTOHTML(getLines(), If(args.Length > 0, Boolean.Parse(args(0)), False))

            Console.WriteLine(result)
        Finally
            If parser IsNot Nothing Then parser.Dispose()
        End Try
    End Sub

    Function CSVTOHTML(lines As IEnumerable(Of IEnumerable(Of String)), useTHead As Boolean) As XElement
        Dim getRow = Function(row As IEnumerable(Of String)) From field In row Select <td><%= field %></td>

        CSVTOHTML =
<table>
    <%= From l In lines.Select(
            Function(line, i)
                If useTHead AndAlso i = 0 Then
                    Return <thead><%= getRow(line) %></thead>
                Else
                    Return <tr><%= getRow(line) %></tr>
                End If
            End Function) %>
</table>
    End Function
End Module
```


```html5
Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!
^Z
^Z
<table>
  <thead>
    <td>Character</td>
    <td>Speech</td>
  </thead>
  <tr>
    <td>The multitude</td>
    <td>The messiah! Show us the messiah!</td>
  </tr>
  <tr>
    <td>Brians mother</td>
    <td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry&gt;</td>
  </tr>
  <tr>
    <td>The multitude</td>
    <td>Who are you?</td>
  </tr>
  <tr>
    <td>Brians mother</td>
    <td>I'm his mother; that's who!</td>
  </tr>
  <tr>
    <td>The multitude</td>
    <td>Behold his mother! Behold his mother!</td>
  </tr>
</table>
```



## XSLT 2.0


<h3>Setup</h3>
# Download to a local copy of the [http://pascaliburnus.seanbdurkin.id.au/index.php?/archives/17-A-Generalised-and-Comprehensive-Solution-to-CSV-to-XML-and-XML-to-CSV-Transformations.html#extended csv-to-xml.xslt library] style-sheet listed in the linked blog entry. Alternatively a copy of the style-sheet on this wiki can be found here: [[csv-to-xml.xslt]]
# Pass as the URI of the input csv as a parameter (named url-of-csv) to your XSLT 2.0 processor

 <lang><xsl:stylesheet
   xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
   xmlns:xcsvt="http://www.seanbdurkin.id.au/xslt/csv-to-xml.xslt"
   xmlns:xs="http://www.w3.org/2001/XMLSchema"
   xmlns:xcsv="http://www.seanbdurkin.id.au/xslt/xcsv.xsd"
   version="2.0"
   exclude-result-prefixes="xsl xs xcsvt xcsv">
 <xsl:import href="csv-to-xml.xslt" />
 <xsl:output indent="yes" encoding="UTF-8" method="html" doctype-system="about:legacy-compat" />
 <xsl:import-schema schema-location="http://www.seanbdurkin.id.au/xslt/xcsv.xsd"
                    use-when="system-property('xsl:is-schema-aware')='yes'" />
 <xsl:param name="url-of-csv" as="xs:string" select="'roseta.csv'" />


 <xsl:variable name="phase-1-result">
   <xsl:call-template name="xcsvt:main" />
 </xsl:variable>

 <xsl:template match="/">
   <html lang="en">
     <head><title>CSV to HTML translation - Extra Credit</title></head>
     <body>
       <xsl:apply-templates select="$phase-1-result" mode="phase-2" />
     </body>
   </html>
 </xsl:template>

 <xsl:template match="xcsv:comma-separated-single-line-values" mode="phase-2">
  <table>
    <xsl:apply-templates mode="phase-2" />
  </table>
 </xsl:template>

 <xsl:template match="xcsv:row[1]" mode="phase-2">
  <th>
    <xsl:apply-templates mode="phase-2" />
  </th>
 </xsl:template>

 <xsl:template match="xcsv:row" mode="phase-2">
  <tr>
    <xsl:apply-templates mode="phase-2" />
  </tr>
 </xsl:template>

 <xsl:template match="xcsv:value" mode="phase-2">
  <td>
    <xsl:apply-templates mode="phase-2" />
  </td>
 </xsl:template>

 <xsl:template match="xcsv:notice" mode="phase-2" />

 </xsl:stylesheet>
```


This style-sheet, when applied to the task input, yields...


```html5
<!DOCTYPE html
  SYSTEM "about:legacy-compat">
<html lang="en">
   <head>
      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
      <title>CSV to HTML translation - Extra Credit</title>
   </head>
   <body>
      <table>
         <th>
            <td>Character</td>
            <td>Speech</td>
         </th>
         <tr>
            <td>The multitude</td>
            <td>The messiah! Show us the messiah!</td>
         </tr>
         <tr>
            <td>Brians mother</td>
            <td>&lt;angry&gt;Now you listen here! He's not the messiah; he's a very naughty boy! Now go
               away!&lt;/angry&gt;
            </td>
         </tr>
         <tr>
            <td>The multitude</td>
            <td>Who are you?</td>
         </tr>
         <tr>
            <td>Brians mother</td>
            <td>I'm his mother; that's who!</td>
         </tr>
         <tr>
            <td>The multitude</td>
            <td>Behold his mother! Behold his mother!</td>
         </tr>
      </table>
   </body>
</html>
```



## zkl


```zkl
csvData:=Data(0,Int,"Character,Speech\n"
"The multitude,The messiah! Show us the messiah!\n"
"Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n"
"The multitude,Who are you\n"
"Brians mother,I'm his mother; that's who!\n"
"The multitude,Behold his mother! Behold his mother!");

html:=csvData.pump("<table>\n",fcn(line){
   line.replace("&","&amp;").replace("<","&lt;") // <angry/> --> &lt;angry/>
   .split(",")
   .pump("<tr>\n","strip",String.fpM("101","  <td>","</td>\n"))+"</tr>\n"
}) + "</table>";
html.println();
```

The pump method writes or appends to a sink, in this case a string (eg "<table/>"). The fpM method is function/partial application and takes a string (ones and zeros) to indicate the positional input parameters. So String.fpM("101","a","c")("B") creates a new string "aBc".
```html5><table

<tr>
  <td>Character</td>
  <td>Speech</td>
</tr>
<tr>
  <td>The multitude</td>
  <td>The messiah! Show us the messiah!</td>
</tr>
<tr>
  <td>Brians mother</td>
  <td>&lt;angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!&lt;/angry></td>
</tr>
<tr>
  <td>The multitude</td>
  <td>Who are you</td>
</tr>
<tr>
  <td>Brians mother</td>
  <td>I'm his mother; that's who!</td>
</tr>
<tr>
  <td>The multitude</td>
  <td>Behold his mother! Behold his mother!</td>
</tr>
</table>
```

