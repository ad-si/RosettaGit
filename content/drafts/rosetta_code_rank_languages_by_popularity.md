+++
title = "Rosetta Code/Rank languages by popularity"
description = ""
date = 2019-10-20T21:29:58Z
aliases = []
[extra]
id = 3315
[taxonomies]
categories = []
tags = []
+++

[[Category:Networking and Web Interaction]]
[[Category:Sorting]]
[[Category:Rosetta Code related]]

{{task|Text processing}}

;Task:
Sort the most popular computer programming languages based in number of members in Rosetta Code categories.


Sample output on 9 October 2019 at 15:31 +02:

```txt

Rank:  1 (1,183 entries) Go
Rank:  2 (1,121 entries) Phix
Rank:  3 (1,111 entries) Perl 6
Rank:  4 (1,108 entries) Julia
Rank:  5 (1,093 entries) Python
Rank:  6 (1,062 entries) Perl
Rank:  7 (1,057 entries) Kotlin
Rank:  8 (1,046 entries) Racket
Rank:  9   (971 entries) C
Rank: 10   (964 entries) Zkl
...
```



;Notes:
*   Each language typically demonstrates one or two methods of accessing the data:
:::*   with web scraping   (via http://www.rosettacode.org/mw/index.php?title=Special:Categories&limit=5000)
:::*   with the API method   (examples below for [[Rosetta Code/Rank languages by popularity#AWK|Awk]], [[Rosetta Code/Rank languages by popularity#Perl|Perl]], [[Rosetta Code/Rank languages by popularity#Ruby|Ruby]], [[Rosetta Code/Rank languages by popularity#Tcl|Tcl]], etc).
*   The scraping and API solutions can be separate subsections, see the [[Rosetta Code/Rank languages by popularity#Tcl|Tcl example]].
*   Filtering wrong results is optional.   You can check against [[Special:MostLinkedCategories]] (if using web scraping)
::If you use the API, and do elect to filter, you may check your results against [[Rosetta_Code/Rank_languages_by_popularity/Full_list|this  complete, accurate, sortable, wikitable listing]] of all '''{{PAGESINCAT:Programming Languages}}''' [[:Category:Programming Languages|programming languages]], updated periodically, ''typically weekly''.
*   A complete ranked listing of all   '''727'''   languages (from the REXX example) is included here   ──►   [[RC_POP.OUT]].





## Ada

{{libheader|AWS}}

```ada
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Ada.Containers.Ordered_Sets;
with Ada.Strings.Less_Case_Insensitive;

with AWS.Client;
with AWS.Response;

procedure Test is

   use Ada.Strings;

   function "+" (S : String) return Unbounded_String renames To_Unbounded_String;

   type A_Language_Count is
      record
         Count    : Integer := 0;
         Language : Unbounded_String;
      end record;

   function "=" (L, R : A_Language_Count) return Boolean is
   begin
      return L.Language = R.Language;
   end "=";

   function "<" (L, R : A_Language_Count) return Boolean is
   begin
      -- Sort by 'Count' and then by Language name
      return L.Count < R.Count
        or else (L.Count = R.Count
                 and then Less_Case_Insensitive (Left  => To_String (L.Language),
                                                 Right => To_String (R.Language)));
   end "<";

   package Sets is new Ada.Containers.Ordered_Sets (A_Language_Count);
   use Sets;

   Counts : Set;

   procedure Find_Counts (S : String) is
      Title_Str : constant String  := "title=""Category:";
      End_A_Str : constant String  := "</a> (";

      Title_At   : constant Natural := Index (S, Title_Str);
   begin
      if Title_At /= 0 then
         declare
            Bracket_At : constant Natural := Index (S (Title_At   + Title_Str'Length .. S'Last), ">");
            End_A_At   : constant Natural := Index (S (Bracket_At + 1                .. S'Last), End_A_Str);
            Space_At   : constant Natural := Index (S (End_A_At   + End_A_Str'Length .. S'Last), " ");
            Count      : constant Natural := Natural'Value (S (End_A_At + End_A_Str'Length .. Space_At - 1));
            Language   : constant String  :=                S (Title_At + Title_Str'Length .. Bracket_At - 2);
         begin
            if Bracket_At /= 0 and then End_A_At /= 0 and then Space_At /= 0 then
               begin
                  Counts.Insert (New_Item => (Count, +Language));
               exception
                  when Constraint_Error =>
                     Put_Line (Standard_Error, "Warning: repeated language: " & Language);
                     -- Ignore repeated results.
                     null;
               end;
            end if;
            -- Recursively parse the string for languages and counts
            Find_Counts (S (Space_At + 1 .. S'Last));
         end;
      end if;

   end Find_Counts;

   Place : Natural := 1;

   procedure Display (C : Cursor) is
   begin
      Put (Place, Width => 1);             Put (". ");
      Put (Element (C).Count, Width => 1); Put (" - ");
      Put_Line (To_String (Element (C).Language));
      Place := Place + 1;
   end Display;

   Http_Source : constant AWS.Response.Data :=
     AWS.Client.Get ("http://rosettacode.org/mw/index.php?title=Special:Categories&limit=5000");
begin
   Find_Counts (AWS.Response.Message_Body (Http_Source));
   Counts.Reverse_Iterate (Display'Access);
end Test;

```



## ALGOL 68


### ALGOL68: using web scraping

<!-- {{does not work with|ALGOL 68|Standard - extensions to language used}} -->
{{works with|ALGOL 68G|mk8+ for Unix and Linux - tested with release mk15-0.8b.fc9.i386 - uses non-standard library routines ''http content'' and'' grep in string''.}}
<!-- {{does not work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386 - ''http content'' and'' grep in string'' not available in any library ... yet}} -->
<!-- # the ''good page'' routine was extracted from ALGOL 68G's manual (GPL code) # -->
Note: the routine ''http content'' is currently not available on Win32 systems.

{{incorrect|ALGOL 68|
 ---among others, Tcl (the top dog) is missing.
}}


```algol68
PROC good page = (REF STRING page) BOOL:
     IF grep in string("^HTTP/[0-9.]* 200", page, NIL, NIL) = 0
     THEN TRUE
     ELSE IF INT start, end;
              grep in string("^HTTP/[0-9.]* [0-9]+ [a-zA-Z ]*", page,
                             start, end) = 0
          THEN print (page[start : end])
          ELSE print ("unknown error retrieving page")
          FI;
          FALSE
     FI;

MODE LISTOFSTRING = STRUCT(REF LINK first, last, INT upb);
MODE LINK = STRUCT(STRING value, REF LINK next);

PRIO LISTINIT = 1;
OP LISTINIT = (REF LISTOFSTRING new, REF LINK first)REF LISTOFSTRING: (
  new := (first, first, (first IS REF LINK(NIL) | 0 | 1 ));
  new
);

OP +:=  = (REF LISTOFSTRING list, []CHAR item)VOID: (
  HEAP LINK new := (STRING(item), REF LINK(NIL));
  IF first OF list IS REF LINK(NIL) THEN
    first OF list := new
  ELSE
    next OF last OF list := new
  FI;
  last OF list := new;
  upb OF list +:= 1
);

OP UPB = (LISTOFSTRING list)INT: upb OF list;

OP ARRAYOFSTRING = (LISTOFSTRING list)[]STRING:(
  [UPB list]STRING out;
  REF LINK this := first OF list;
  FOR i TO UPB list DO out[i] := value OF this; this := next OF this OD;
  out
);

INT match=0, no match=1, out of memory error=2, other error=3;

PROC re split = (STRING re split, REF STRING beetles)[]STRING:(
    LISTOFSTRING out := (NIL, NIL, 0); # LISTINIT REF LINK NIL; #
    INT start := 1, pos, end;
    WHILE grep in string(re split, beetles[start:], pos, end) = match DO
      out +:= beetles[start:start+pos-2];
      out +:= beetles[start+pos-1:start+end-1];
      start +:= end
    OD;
    IF start > UPB beetles THEN
      out +:= beetles[start:]
    FI;
    ARRAYOFSTRING(out)
  );


IF STRING reply;
   INT rc =
      http content (reply, "www.rosettacode.org", "http://www.rosettacode.org/w/index.php?title=Special:Categories&limit=500", 0);
   rc /= 0 OR NOT good page (reply)
THEN print (("Error:",strerror (rc)))
ELSE
  STRING  # hack: HTML should be parsed by an official HTML parsing library #
    re html tag = "<[^>]*>",
    re a href category = "^<a href=""/wiki/Category:.*"" title=",
    re members = "([1-9][0-9]* members)";

  MODE STATISTIC = STRUCT(INT members, STRING category);
  FLEX[0]STATISTIC stats;

  OP +:=  = (REF FLEX[]STATISTIC in out, STATISTIC item)VOID:(
      [LWB in out: UPB in out+1]STATISTIC new;
      new[LWB in out: UPB in out]:=in out;
      new[UPB new]:=item;
      in out := new
    );

# hack: needs to be manually maintained #
  STRING re ignore ="Programming Tasks|WikiStubs|Maintenance/OmitCategoriesCreated|"+
                    "Unimplemented tasks by language|Programming Languages|"+
                    "Solutions by Programming Language|Implementations|"+
                    "Solutions by Library|Encyclopedia|Language users|"+
                    "Solutions by Programming Task|Basic language learning|"+
                    "RCTemplates|Language Implementations";

  FORMAT category fmt = $"<a href=""/wiki/Category:"g""" title=""Category:"g""""$;
  STRING encoded category, category;
  FORMAT members fmt = $" ("g" members)"$;
  INT members;

  FLEX[0]STRING tokens := re split(re html tag, reply);
  FOR token index TO UPB tokens DO
    STRING token := tokens[token index];
    FILE file;
    IF grep in string(re a href category, token, NIL, NIL) = match THEN
      associate(file, token);
      make term(file,"""");
      getf(file, (category fmt, encoded category, category));
      close(file)
    ELIF grep in string(re members, token, NIL, NIL) = match THEN
      IF grep in string(re ignore, category, NIL, NIL) /= match THEN
        associate(file, token);
        getf(file, (members fmt, members));
        stats +:= STATISTIC(members, category);
        close(file)
      FI
    FI
  OD;

  OP < = (STATISTIC a,b)BOOL:
    members OF a < members OF b;

  MODE SORTSTRUCT = STATISTIC;
  PR READ "prelude/sort.a68" PR;

  stats := in place shell sort reverse(stats);

  INT max = 10;
  FOR i TO (UPB stats > max | max | UPB stats) DO
    printf(($g(-0)". "g(-0)" - "gl$,i,stats[i]))
  OD
FI
```

{{out|Sample output}}

```txt

1. 233 - Python
2. 222 - Ada
3. 203 - OCaml
4. 203 - C
5. 201 - Perl
6. 193 - Haskell
7. 182 - Java
8. 179 - D
9. 178 - ALGOL 68
10. 160 - Ruby

```



### ALGOL 68:using the API


```algol68

CHAR line feed = REPR 10, carriage return = REPR 13;
STRING crlf = carriage return + line feed;
STRING domain = "rosettacode.org",
       page = "/mw/api.php?format=xml&action=query&generator=categorymembers&gcmtitle=Category:Programming%20Languages&gcmlimit=500&prop=categoryinfo";

# concatenate tuples #
OP + = ([]STRING a, b) []STRING:
   BEGIN
      [⌈a + ⌈b] STRING c;
      c[:⌈a] := a;
      c[⌈a+1:] := b;
      c
   END;

# count occurrances of string in string #
PROC count = (STRING sub, str) INT :
   BEGIN
      INT count := 0;
      IF UPB str ≥ UPB sub AND UPB str ≥ 1 THEN
	 INT p := 1; INT p0;
	 WHILE p + UPB sub - 1 <= UPB str ANDF (p0 := p; string in string (sub, p, str[p0:])) DO
            count +:= 1;
	    p +:= p0 + UPB sub - 1
	 OD
      FI;
      count
   END;

# split string into tuple #
PROC split = (STRING str, sep) FLEX[]STRING :
   BEGIN
      INT seplen = UPB sep, strlen = UPB str;
      INT cnt := 0, start := 1;
      INT p;
      [count (sep, str) + 1] STRING list;
      WHILE start ≤ strlen - (seplen - 1)
	 ANDF string in string (sep, p, str[start:]) DO
	    p +:= start - 1;
	    list[cnt +:= 1] := str[start:p-1];
	    start := p + seplen
      OD;
      IF cnt = 0 THEN list[cnt +:= 1] := str
      ELIF start ≤ strlen THEN list[cnt +:= 1] := str[start:]
      ELIF start = strlen + 1 AND seplen ≥ 1 THEN list[cnt +:= 1] := ""
      FI;
      list
   END;

# reverse strings in a TUPLE #
OP REVERSE = ([]STRING org) []STRING :
   BEGIN
      [UPB org]STRING new;
      FOR i TO UPB org DO
	 new[UPB org - (i - 1)] := org[i]
      OD;
      new
   END;

# convert unsigned number to INT #
OP TOINT = (STRING str) INT:
   BEGIN
      INT p := 1, len := UPB str;
      WHILE p ≤ len ANDF is space (str[p]) DO p +:= 1 OD;
      IF str[1] = "-" OR str[1] = "+" THEN
	 p +:= 1
      FI;
      INT n := 0;
      WHILE p ≤ len ANDF is space (str[p]) DO p +:= 1 OD;
      FOR i FROM p TO len WHILE is digit (str[i]) DO
	 n := n × 10 + ABS str[i] - ABS "0"
      OD;
      n
   END;

# pad to fixed width #
PROC field = (UNION (STRING,INT) x, INT w) STRING:
   BEGIN
      STRING s = (x | (INT i): whole (i,0), (STRING t): t);
      (w >= UPB s | " " * (w - UPB s)) + s
   END;

PROC get web page = (STRING host, path) STRING:
   BEGIN
      STRING reply;
      INT rc;
      # 'http content' sometimes fails with interrupted system call, so we loop until succeeding #
      WHILE
	 # 'http content' makes requests that are not accepted by rosettacode.org, so therefore the hack #
	 STRING hack = " HTTP/1.0" + crlf +
	               "Host: rosettacode.org" + crlf +
                       "User-Agent: rank_languages_by_popularity";
         rc := http content (reply, host, path + hack, 0);
	 rc = 4
         DO SKIP
      OD;
      IF rc = 0 AND grep in string ("^HTTP/[0-9.]+ 200", reply, NIL, NIL) = 0 THEN
	 INT p;
	 IF string in string (crlf + crlf, p, reply) THEN
	    STRING headers = reply[:p],
	           body = reply[p+4:];
	    body
	 ELSE
	    ""
	 FI
      ELSE
	 print (strerror (rc)); ""
      FI
   END;

# the main program rank languages by popularity starts here #
STRING gcmcontinue;
FLEX[0]STRING lines;

# get through API in chunks of 500 #
WHILE
   STRING body = get web page (domain, page + (gcmcontinue /= "" | "&gcmcontinue=" + gcmcontinue));
   INT b, e;
   gcmcontinue := (grep in string ("gcmcontinue=""([^""]+)", body, b, e) = 0 | body[b+13:e-1] | "");
   # split the XML into lines on </page> #
   lines := lines + split (body, "</page>");
   gcmcontinue /= "" DO SKIP
OD;

# Each line is one language,
  go through them and rewrite them to something we can sort #
FOR i TO UPB lines DO
   STRING line = lines[i];
   STRING title;
   INT pages := 0;
   INT b, e;
   # the two fields we are intrested in are title="Category:xxx", and pages="999" #
   IF grep in string ("title=""Category:[^""]+""", line, b, e) = 0 THEN
      title := line[b+16:e-1]
   FI;
   IF grep in string ("pages=""[0-9]+""", line, b, e) = 0 THEN
      pages := TOINT line[b+7:e-1]
   FI;
   lines[i] := field (pages, 6) + " " + title
OD;

lines := REVERSE SORT lines;

INT rank := 1;
BOOL tied := FALSE, lasttied := FALSE;
print ((new line, whole (UPB lines, 0), " languages", new line, new line));
FOR i TO UPB lines DO
   INT entries = TOINT lines[i][:6];
   STRING lang = lines[i][8:];
   IF entries > 0 THEN
      tied := i < UPB lines ANDF lines[i][:6] = lines[i+1][:6];
      print (("rank: ", field (rank,3), "  ", (tied OR lasttied | "[tied]" | " "*6),
              field ("(" + whole (entries,0) + " " + (entries = 1 | "entry)" | "entries)"), 20),
              "  ", lang, new line));
      IF NOT tied THEN rank +:= 1 FI;
      lasttied := tied
   FI
OD
```

{{out|Sample output top 10}}

```txt


572 languages

rank:   1               (883 entries)  Tcl
rank:   2               (875 entries)  Racket
rank:   3               (837 entries)  Python
rank:   4               (800 entries)  J
rank:   5               (772 entries)  Ruby
rank:   6               (763 entries)  Perl 6
rank:   7               (756 entries)  C
rank:   8               (742 entries)  Go
rank:   9               (737 entries)  D
rank:  10               (707 entries)  Perl


```



## AutoHotkey


```autohotkey
MembsUrl = http://rosettacode.org/mw/index.php?title=Special:Categories&limit=5000
ValidUrl = http://rosettacode.org/wiki/Category:Programming_Languages
WebRequest := ComObjCreate("WinHttp.WinHttpRequest.5.1")

; Get the webpages
WebRequest.Open("GET", MembsUrl),WebRequest.Send()
MembsPage := WebRequest.ResponseText
WebRequest.Open("GET", ValidUrl),WebRequest.Send()
ValidPage := WebRequest.ResponseText

; Replace special characters
StringReplace, MembsPage, MembsPage, ΜC++, µC++, All
StringReplace, MembsPage, MembsPage, MK-61/52, MK-61/52, All
StringReplace, ValidPage, ValidPage, ΜC++, µC++, All
StringReplace, ValidPage, ValidPage, MK-61/52, MK-61/52, All

ValidREx := "s)href=""([^""]+)"" title=""Category:([^""]+)"">(?=.*</table>)"
MembsREx := "title=""Category:(.+?)"">.+?\((\d+) members?\)"

; Iterate through all matches for valid languages
ValidLangs := [], FoundPos := 0
While FoundPos := RegExMatch(ValidPage, ValidREx, Match, FoundPos+1)
	ValidLangs[Match2] := Match1

; Iterate through all matches for categories with members
MembsLangs := [], Dupes := [], Detected := 0, FoundPos := 0
While FoundPos := RegExMatch(MembsPage, MembsREx, Match, FoundPos+1)
{
	; If it isn't a valid language or is a duplicate, skip it
	if !ValidLangs.HasKey(Match1) || Dupes.HasKey(Match1)
		continue

	Dupes.Insert(Match1, true)
	Detected++

	; Initialize this member count
	if !IsObject(MembsLangs[Match2])
		MembsLangs[Match2] := [Match1]
	else
		MembsLangs[Match2].Insert(Match1)
}

; Sort the languages with the highest member count first
Sorted := []
for Members, Languages in MembsLangs
	Sorted.Insert(1, [Members, Languages])

; Initialize the GUI
Gui, New, HwndGuiHwnd
Gui, Add, Text, w300 Center, %Detected% languages detected
Gui, Add, Edit, w300 vSearchText gSearch, Filter languages
Gui, Add, ListView, w300 r20 Grid gOpen vMyListView, Rank|Members|Category

; Populate the list view
LV_ModifyCol(1, "Integer"), LV_ModifyCol(2, "Integer"), LV_ModifyCol(3, 186)
for Rank, Languages in Sorted
	for Key, Language in Languages[2]
		LV_Add("", Rank, Languages[1], Language)

Gui, Show,, Rosetta Code
return

Open:
if (A_GuiEvent == "DoubleClick")
{
	LV_GetText(Language, A_EventInfo, 3)
	Run, % "http://rosettacode.org" ValidLangs[Language]
}
return

Search:
GuiControlGet, SearchText
GuiControl, -Redraw, MyListView

LV_Delete()
for Rank, Languages in Sorted
	for Key, Language in Languages[2]
		if InStr(Language, SearchText)
			LV_Add("", Rank, Languages[1], Language)

GuiControl, +Redraw, MyListView
return

GuiClose:
ExitApp
return
```



## AWK

{{works with|Gawk}}

### By using the API

This is the third solution. The [http://rosettacode.org/mw/index.php?title=Rosetta_Code/Rank_languages_by_popularity&oldid=102962#AWK first solution] used web scraping with an external program '''ns''' for networking. The [http://rosettacode.org/mw/index.php?title=Rosetta_Code/Rank_languages_by_popularity&oldid=204081 second solution] used the Rosetta Code API instead of web scraping, but continued use of '''ns''' which for unknown reasons didn't work correctly. This solution uses native gawk networking to connect to the API at 500 items per request ("gmcontinue").


```awk
function join(array, start, end, sep,    result, i) {
        result = array[start]
        for (i = start + 1; i <= end; i++)
            result = result sep array[i]
        return result
}

function trim(str) {
        gsub(/^[[:blank:]]+|[[:blank:]\n]+$/, "", str)
        return str
}

function http2var(      site,path,server,j,output) {

        RS = ORS = "\r\n"

        site = "rosettacode.org"
        path = "/mw/api.php" \
            "?action=query" \
            "&generator=categorymembers" \
            "&gcmtitle=Category:Programming%20Languages" \
            "&gcmlimit=500" \
            (gcmcontinue "" ? "&gcmcontinue=" gcmcontinue : "") \
            "&prop=categoryinfo" \
            "&format=txt"

        server = "/inet/tcp/0/" site "/80"
        print "GET " path " HTTP/1.0" |& server
        print "Host: " site |& server
        print "" |& server
        while ((server |& getline) > 0) {
            if($0 != 0) {
                j++
                output[j] = $0
            }
        }
        close(server)
        if(length(output) == 0)
            return -1
        else
            return join(output, 1, j, "\n")
}

function parse(webpage  ,c,a,i,b,e,pages) {

       # Check for API continue code ie. a new page of results available
        match(webpage, "gcmcontinue[]] =>[^)]+[^)]", a)
        if(a[0] != "") {
            split(a[0], b, ">")
            gcmcontinue = trim(b[2])
        } else gcmcontinue = ""

        c = split(webpage, a, "[[][0-9]{1,7}[]]")

        while(i++ < c) {
            if(match(a[i], /[pages]/)) {
                match(a[i], "pages[]] =>[^[]+[^[]", b)
                split(b[0], e, ">")
                pages = trim(e[2]) + 0
            } else pages = 0
            if(match(a[i], /[title]/)) {
                match(a[i], "title[]] =>[^[]+[^[]", b)
                split(b[0], e, ":")
                e[2] = trim(e[2])
                if ( substr(e[2], length(e[2]), 1) == ")" )
                    e[2] = trim( substr(e[2], 1, length(e[2]) - 1) )
                if(length(e[2]) > 0)
                    G[e[2]] = pages
            }
        }
}

BEGIN {

        parse( http2var() )     # First 500
        while ( gcmcontinue != "" )
            parse( http2var() ) # Next 500, etc

        # https://www.gnu.org/software/gawk/manual/html_node/Controlling-Scanning.html
        PROCINFO["sorted_in"] = "@val_type_desc"
        for ( language in G )
            print ++i ". " language " - " G[language]

}
```

{{out|Output from 26 May 2015}}

```txt
1. Tcl - 867
2. Racket - 863
3. Python - 828
4. J - 777
5. Ruby - 769
6. Perl 6 - 755
7. C - 751
...
570. NQP - 0
571. AspectC++ - 0
572. Cilk - 0
573. PL/M - 0
574. Agda2 - 0

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
Note that language names differing only in their case are merged.

```bbcbasic
      INSTALL @lib$+"SORTLIB"
      SortUp% = FN_sortinit(0,0)   : REM Ascending
      SortDown% = FN_sortinit(1,0) : REM Descending

      VDU 23,22,640;512;8,16,16,128+8 : REM Enable UTF-8 support
      DIM lang$(1000), tasks%(1000)
      NORM_IGNORECASE = 1

      SYS "LoadLibrary", "URLMON.DLL" TO urlmon%
      SYS "GetProcAddress", urlmon%, "URLDownloadToFileA" TO UDTF

      PRINT "Downloading languages list..."
      url$ = "http://rosettacode.org/wiki/Category:Programming_Languages"
      file$ = @tmp$ + "languages.htm"
      SYS UDTF, 0, url$, file$, 0, 0 TO fail%
      IF fail% ERROR 100, "File download failed (languages)"

      file% = OPENIN(file$)
      index% = 0
      WHILE NOT EOF#file%
        REPEAT
          a$ = GET$#file%
          IF INSTR(a$, "<a href=""/wiki/Category") = 0 EXIT REPEAT
          i% = INSTR(a$, "</a>")
          IF i% = 0 EXIT REPEAT
          j% = i%
          REPEAT i% -= 1 : UNTIL MID$(a$,i%,1) = ">" OR i% = 0
          IF i% = 0 EXIT REPEAT
          lang$(index%) = MID$(a$, i%+1, j%-i%-1)
          IF lang$(index%) <> "Languages" index% += 1
        UNTIL TRUE
      ENDWHILE
      CLOSE #file%

      C% = index%
      CALL SortUp%, lang$(0)

      PRINT "Downloading categories list..."
      url$ = "http://www.rosettacode.org/w/index.php"
      url$ += "?title=Special:Categories&limit=5000"
      file$ = @tmp$ + "categories.htm"
      SYS UDTF, 0, url$, file$, 0, 0 TO fail%
      IF fail% ERROR 100, "File download failed (categories)"

      file% = OPENIN(file$)
      WHILE NOT EOF#file%
        REPEAT
          a$ = GET$#file%
          i% = INSTR(a$, "member")
          IF i% = 0 EXIT REPEAT
          REPEAT i% -= 1 : UNTIL MID$(a$,i%,1) = "(" OR i% = 0
          IF i% = 0 EXIT REPEAT
          tasks% = VAL(MID$(a$, i%+1))
          IF tasks% = 0 EXIT REPEAT
          REPEAT i% -= 1 : UNTIL MID$(a$,i%,1) = "<" OR i% = 0
          IF i% = 0 EXIT REPEAT
          j% = i%
          REPEAT i% -= 1 : UNTIL MID$(a$,i%,1) = ">" OR i% = 0
          IF i% = 0 EXIT REPEAT
          k% = FNwhere(lang$(), MID$(a$, i%+1, j%-i%-1), index%-1)
          IF k% <> -1 tasks%(k%) += tasks%
        UNTIL TRUE
      ENDWHILE
      CLOSE #file%

      CALL SortDown%, tasks%(0), lang$(0)

      VDU 14
      @% = 3 : REM Column width
      PRINT "List of languages as of " TIME$
      FOR i% = 0 TO index%-1
        IF tasks%(i%) = 0 EXIT FOR
        PRINT  i%+1 ". " tasks%(i%) " - " lang$(i%)
      NEXT
      END

      DEF FNwhere(a$(), S$, T%)
      LOCAL B%, C%, H%
      H% = 2
      WHILE H%<T% H% *= 2:ENDWHILE
      H% /= 2
      REPEAT
        IF (B%+H%)<=T% THEN
          SYS "CompareString", 0, NORM_IGNORECASE, S$, -1, a$(B%+H%), -1 TO C%
          IF C% >= 2 B% += H%
        ENDIF
        H% /= 2
      UNTIL H%=0
      SYS "CompareString", 0, NORM_IGNORECASE, S$, -1, a$(B%), -1 TO C%
      IF C% = 2 THEN = B% ELSE = -1
```

'''Output:'''

```txt

Downloading languages list...
Downloading categories list...
List of languages as of Sat.17 Nov 2012,00:21:11
  1. 682 - Tcl
  2. 638 - Python
  3. 626 - PicoLisp
  4. 622 - C
  5. 592 - J
  6. 581 - Go
  7. 570 - Ruby
  8. 553 - Ada
  9. 515 - Perl
 10. 514 - D
 11. 507 - Haskell
 12. 490 - Perl 6
 13. 489 - BBC BASIC
 14. 477 - Java
 15. 473 - Mathematica
 16. 469 - PureBasic
 17. 469 - OCaml
 18. 459 - Unicon
 19. 438 - REXX
 20. 428 - Icon
......
461.   1 - ScriptBasic
462.   1 - Qore
463.   1 - Opa
464.   1 - Nickle
465.   1 - Neko
466.   1 - Neat
467.   1 - MEL
468.   1 - MAPPER
469.   1 - Kotlin
470.   1 - Chapel

```



## Bracmat


```bracmat
  ( get-page
  =   url type
    .   !arg:(?url.?type)
      & sys$(str$("wget -q -O wget.out \"" !url \"))
      & get$("wget.out",!type)                     { Type can be JSN, X ML, HT ML or just ML. }
  )
& ( get-langs
  =   arr lang
    .   :?arr
      & !arg:? (.h2.) ?arg (h2.?) ?      { Only analyse part of page between the h2 elements. }
      &   whl
        ' ( !arg
          :   ?
              ( a
              .   ?
                  ( title
                  .   @(?:"Category:" ?):?lang
                    & !lang !arr:?arr
                  )
                  ?
              )
              ?arg
          )
      & !arr
  )
& ( get-cats
  =   page langs list count pat li A Z
    .   !arg:(?page.?langs)
      & 0:?list
      &   whl
        ' ( !langs:%?lang ?langs
          &                                { Use macro substitution to create a fast pattern. }
              ' ( ?
                  (a.? (title.$lang) ?)           { $lang is replaced by the actual language. }
                  ?
                  (.a.)
                  @(?:? #?count " " ?)
                )
            : (=?pat)
          &       (       !page
                        :   ?A
                            ( (li.) ?li (.li.) ?Z
                            & !li:!pat
                            )
                      & !A !Z:?page     { Remove found item from page. (Not necessary at all.)}
                      & !count
                    | 0                                       { The language has no examples. }
                  .
                  )
                \L !lang             { Bracmat normalizes a\Lx+b\Ly+a\Lz to a\L(x*z)+b\Ly, so }
              + !list                { it's easy to collect categories with the same count.   }
            : ?list
          )
      & !list
  )
&     get-cats
    $ (   get-page
        $ ( "http://www.rosettacode.org/w/index.php?title=Special:Categories&limit=5000"
          . HT,ML
          )
      .   get-langs
        $ ( get-page
          $ ( "http://rosettacode.org/wiki/Category:Programming_Languages"
            . HT ML
            )
          )
      )
  : ?cats
& :?list
&   whl
  ' ( !cats:(?count.)\L?tiedcats+?cats
    & :?ties
    &   whl
      ' ( !tiedcats:@(?:"Category:" ?name)*?tiedcats
        & !ties !name:?ties
        )
    & (!count.!ties) !list:?list
    )
& 1:?rank
&   whl
  ' ( !rank:?tiedRank
    & !list:(?count.?ties) ?list
    &   whl
      ' ( !ties:%?name ?ties
        & @(!tiedRank:? [?len)                  { We want some padding for the highest ranks. }
        & @("   ":? [!len ?sp)                  { Skip blanks up to the length of the rank.   }
        & out$(str$(!sp !tiedRank ". " !count " - " !name))
        & 1+!rank:?rank
        )
    )
& ;
```

Output:

```txt
  1. 816 - Tcl
  2. 771 - Racket
  3. 760 - Python
  4. 708 - C
  5. 705 - Perl 6
  6. 700 - J
  7. 695 - Ruby
  8. 690 - D
  9. 656 - Go
 10. 643 - PicoLisp
 11. 639 - Perl
 12. 622 - REXX
 13. 602 - Ada
 14. 591 - Mathematica
 15. 588 - Haskell
 16. 574 - AutoHotkey
 17. 559 - Unicon
 18. 543 - Java
 19. 526 - BBC BASIC
 20. 510 - Icon
 21. 500 - C++
...
498. 1 - Vox
498. 1 - XPath 2.0
498. 1 - Xanadu
523. 0 - Clarion
523. 0 - EhBASIC
523. 0 - Epigram
523. 0 - FLORA-2
523. 0 - Florid
523. 0 - Jcon
523. 0 - LLP
523. 0 - Lolli
523. 0 - Lygon
523. 0 - Monte
523. 0 - ObjectIcon
523. 0 - RPGIV
523. 0 - Rubylog
523. 0 - Star
523. 0 - True BASIC
523. 0 - X10
523. 0 - XS
523. 0 - Ya
```



## C

{{incorrect|C|Compiles without error but "Segmentation fault" when run. Tested on Cygwin, SuSE Linux and Arch Linux (Manjaro)}}

{{improve|C|This solution uses an external program '''wget''' for networking, but it could use '''Library: libcurl''' (see [[Web scraping#C]]) for example. Also this solution scrapes Special:Categories &limit 5000 which will break if the HTML style changes or the number of languages exceeds 5000. It could use the MediWiki API to get the language names and pages in a single call, in blocks of 500 until complete with no upper limit. See the Awk example. If you make an API-based version please retain the web-scrapping version in its own sub-section (following the lead of [[Rosetta_Code/Rank_languages_by_popularity#Tcl|TCL on this page]]). }}

Ghetto parser
```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const char * lang_url = "http://www.rosettacode.org/w/api.php?action=query&"
		"list=categorymembers&cmtitle=Category:Programming_Languages&"
		"cmlimit=500&format=json";
const char * cat_url = "http://www.rosettacode.org/w/index.php?title=Special:Categories&limit=5000";

#define BLOCK 1024
char *get_page(const char *url)
{
	char cmd[1024];
	char *ptr, *buf;
	int bytes_read = 1, len = 0;
	sprintf(cmd, "wget -q \"%s\" -O -", url);
	FILE *fp = popen(cmd, "r");
	if (!fp) return 0;
	for (ptr = buf = 0; bytes_read > 0; ) {
		buf = realloc(buf, 1 + (len += BLOCK));
		if (!ptr) ptr = buf;
		bytes_read = fread(ptr, 1, BLOCK, fp);
		if (bytes_read <= 0) break;
		ptr += bytes_read;
	}
	*++ptr = '\0';
	return buf;
}

char ** get_langs(char *buf, int *l)
{
	char **arr = 0;
	for (*l = 0; (buf = strstr(buf, "Category:")) && (buf += 9); ++*l)
		for (	(*l)[arr = realloc(arr, sizeof(char*)*(1 + *l))] = buf;
			*buf != '"' || (*buf++ = 0);
			buf++);

	return arr;
}

typedef struct { const char *name; int count; } cnt_t;
cnt_t * get_cats(char *buf, char ** langs, int len, int *ret_len)
{
	char str[1024], *found;
	cnt_t *list = 0;
	int i, llen = 0;
	for (i = 0; i < len; i++) {
		sprintf(str, "/wiki/Category:%s", langs[i]);
		if (!(found = strstr(buf, str))) continue;
		buf = found + strlen(str);

		if (!(found = strstr(buf, "</a> ("))) continue;
		list = realloc(list, sizeof(cnt_t) * ++llen);
		list[llen - 1].name = langs[i];
		list[llen - 1].count = strtol(found + 6, 0, 10);
	}
	*ret_len = llen;
	return list;
}

int _scmp(const void *a, const void *b)
{
	int x = ((const cnt_t*)a)->count, y = ((const cnt_t*)b)->count;
	return x < y ? -1 : x > y;
}

int main()
{
	int len, clen;
	char ** langs = get_langs(get_page(lang_url), &len);
	cnt_t *cats = get_cats(get_page(cat_url), langs, len, &clen);
	qsort(cats, clen, sizeof(cnt_t), _scmp);
	while (--clen >= 0)
		printf("%4d %s\n", cats[clen].count, cats[clen].name);

	return 0;
}
```

{{out}}

```txt
 563 Tcl
 529 PicoLisp
 522 Python
 504 C
 500 J
 442 Go
 440 Ruby
 435 Ada
 430 PureBasic
 427 Perl
...
```



{{libheader|libcurl}}
Using cJSON.

Compiled with gcc -lcurl -lm cJSON.c lang_rank.c

Usage: rank [number]

Outputs the first [number] languages in the list, default to 10. Use -1 to display all the languages.

```c

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <curl/curl.h>
#include "cJSON.h"
char *URL_BASE = "http://www.rosettacode.org/mw/api.php?format=json&action=query&generator=categorymembers&gcmtitle=Category:Programming%20Languages&gcmlimit=500&prop=categoryinfo&rawcontinue";
char *URL_BASE_CONT = "http://www.rosettacode.org/mw/api.php?format=json&action=query&generator=categorymembers&gcmtitle=Category:Programming%20Languages&gcmlimit=500&prop=categoryinfo&gcmcontinue=";

typedef struct mem {
	char *text;
	size_t size;
} mem;

typedef struct page {
	char *name;
	int num;
} page;

size_t write_callback(void *ptr, size_t size, size_t nmemb, void *userdata);
void curl_request(CURL *curl, char *url, mem *response);
char *build_url(char *cont);
char *get_cont(cJSON *json);
void sort_arrays(page *pages, int *s);
cJSON *parse_json(cJSON *json);
page *fill_arrays(page *pages, int *s, cJSON *json);

int main(int argc, char *argv[]) {
	curl_global_init(CURL_GLOBAL_ALL);
	CURL *curl = curl_easy_init();
	char *cont = NULL;
	page *pages = malloc(1);
	int till = 10;
	int *npag = malloc(sizeof(int));
	*npag = 0;
	if (argc>1) till = atoi(argv[1]);
	do {
		mem *response = calloc(1, sizeof(mem));
		char *url = build_url(cont);
		if (cont) free(cont);
		curl_request(curl, url, response);
		cJSON *json = cJSON_Parse(response->text);
		cont = get_cont(json);
		cJSON *json_pages = parse_json(json);
		pages = fill_arrays(pages, npag, json_pages);
		cJSON_Delete(json);
		free(url);
		free(response->text);
		free(response);
	} while (cont);
	sort_arrays(pages, npag);
	if (till>*npag||till<-1) till=10;
	if (till==-1) till=*npag;
	for (int i = 0;i<till;i++) {
		printf("#%d: %s, %d tasks\n", i+1, pages[i].name, pages[i].num);
	}
	for (int i = 0;i<*npag;i++) {
		free(pages[i].name);
	}
	free(pages);
	free(npag);
	curl_easy_cleanup(curl);
	curl_global_cleanup();
	return 0;
}
size_t write_callback(void *ptr, size_t size, size_t nmemb, void *userdata) {
	mem *response = userdata;
	response->text = realloc(response->text, response->size+size*nmemb+1);
	memcpy(&(response->text[response->size]), ptr, size*nmemb);
	response->size += size*nmemb;
	response->text[response->size] = '\0';
	return size*nmemb;
}
void curl_request(CURL *curl, char *url, mem *response) {
	curl_easy_setopt(curl, CURLOPT_URL, url);
	curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
	curl_easy_setopt(curl, CURLOPT_WRITEDATA, response);
	curl_easy_perform(curl);
}
char *build_url(char *cont) {
	char *url;
	if (cont) {
		int size = strlen(URL_BASE_CONT)+strlen(cont)+1;
		url = calloc(1, size);
		strncpy(url, URL_BASE_CONT, strlen(URL_BASE_CONT));
		strcat(url, cont);
	} else {
		url = malloc(strlen(URL_BASE)+1);
		strcpy(url, URL_BASE);
	}
	return url;
}
cJSON *parse_json(cJSON *json) {
	cJSON *pages;
	if (json) {
		pages = cJSON_GetObjectItem(json, "query");
		pages = cJSON_GetObjectItem(pages, "pages");
		pages = pages->child;
	}
	return pages;
}
char *get_cont(cJSON *json) {
	cJSON *jcont = cJSON_GetObjectItem(json, "query-continue");
	if (jcont && jcont->child->child) {
		char *cont = malloc(strlen(jcont->child->child->valuestring)+1);
		strcpy(cont, jcont->child->child->valuestring);
		return cont;
	} else {
		return NULL;
	}
}
page *fill_arrays(page *pag, int *i, cJSON *json) {
	cJSON *cur_page = json;
	page *pages = pag;
	do {
		pages = realloc(pages, *i*sizeof(page)+sizeof(page));
		if (json->child) {
			int size = strlen(cur_page->child->next->next->valuestring)-9;
			char *lang = malloc(size+1);
			strcpy(lang, cur_page->child->next->next->valuestring+9);
			pages[*i].name = lang;
		} else {
			pages[*i].name = "no name";
		}
		int task = cur_page->child->next->next->next?cur_page->child->next->next->next->child->valueint:0;
		pages[*i].num = task;
		*i = *i+1;
		cur_page = cur_page->next;
	} while (cur_page->next);
	return pages;
}
void sort_arrays(page *pages, int *size) {
	int sorted = 0;
	do {
		sorted = 1;
		for (int i = 0;i<*size-1;i++) {
			if (pages[i].num<pages[i+1].num) {
				sorted = 0;
				int a = pages[i+1].num;
				pages[i+1].num = pages[i].num;
				pages[i].num = a;
				char *s = pages[i+1].name;
				pages[i+1].name = pages[i].name;
				pages[i].name = s;
			}
		}
	} while (sorted!=1);
}

```

{{out}}

```txt

1. Racket: 907 tasks
2. Tcl: 899 tasks
3. Python: 872 tasks
4. J: 848 tasks
5. Perl 6: 813 tasks
6. Ruby: 796 tasks
7. C: 777 tasks
8. Java: 764 tasks
9. Go: 759 tasks
10. D: 749 tasks

```


## C#
Sorting only programming languages.

```c#
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Text.RegularExpressions;

class Program
{
    static void Main(string[] args)
    {
        string get1 = new WebClient().DownloadString("http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Languages&cmlimit=500&format=json");
        string get2 = new WebClient().DownloadString("http://www.rosettacode.org/w/index.php?title=Special:Categories&limit=5000");

        ArrayList langs = new ArrayList();
        Dictionary<string, int> qtdmbr = new Dictionary<string, int>();

        MatchCollection match1 = new Regex("\"title\":\"Category:(.+?)\"").Matches(get1);
        MatchCollection match2 = new Regex("title=\"Category:(.+?)\">.+?</a>[^(]*\\((\\d+) members\\)").Matches(get2);

        foreach (Match lang in match1) langs.Add(lang.Groups[1].Value);

        foreach (Match match in match2)
        {
            if (langs.Contains(match.Groups[1].Value))
            {
                qtdmbr.Add(match.Groups[1].Value, Int32.Parse(match.Groups[2].Value));
            }
        }

        string[] test = qtdmbr.OrderByDescending(x => x.Value).Select(x => String.Format("{0,3} - {1}", x.Value, x.Key)).ToArray();

        int count = 1;

        foreach (string i in test)
        {
            Console.WriteLine("{0,3}. {1}", count, i);
            count++;
        }
    }
}
```

{{out|Output (as of May 30, 2010)}}
  1. 397 - Tcl
  2. 368 - Python
  3. 350 - Ruby
  4. 333 - J
  5. 332 - C
  6. 322 - Haskell
  7. 322 - OCaml
  8. 302 - Perl
  9. 290 - Common Lisp
 10. 289 - AutoHotkey
     . . .
===Object-oriented solution===

```c#
using System;
using System.Net;
using System.Linq;
using System.Text.RegularExpressions;
using System.Collections.Generic;

class Category {
    private string _title;
    private int _members;

    public Category(string title, int members) {
        _title = title;
        _members = members;
    }

    public string Title {
        get {
            return _title;
        }
    }

    public int Members {
        get {
            return _members;
        }
    }
}

class Program {
    static void Main(string[] args) {
        string get1 = new WebClient().DownloadString("http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Languages&cmlimit=500&format=json");
        string get2 = new WebClient().DownloadString("http://www.rosettacode.org/w/index.php?title=Special:Categories&limit=5000");

        MatchCollection match1 = new Regex("\"title\":\"Category:(.+?)\"").Matches(get1);
        MatchCollection match2 = new Regex("title=\"Category:(.+?)\">.+?</a>[^(]*\\((\\d+) members\\)").Matches(get2);

        string[] valids = match1.Cast<Match>().Select(x => x.Groups[1].Value).ToArray();
        List<Category> langs = new List<Category>();

        foreach (Match match in match2) {
            string category = match.Groups[1].Value;
            int members = Int32.Parse(match.Groups[2].Value);

            if (valids.Contains(category)) langs.Add(new Category(category, members));
        }

        langs = langs.OrderByDescending(x => x.Members).ToList();
        int count = 1;

        foreach (Category i in langs) {
            Console.WriteLine("{0,3}. {1,3} - {2}", count, i.Members, i.Title);
            count++;
        }
    }
}
```



## C++

{{libheader|Boost}}
using g++ under Linux with <tt>g++ -lboost_thread -lboost_system -lboost_regex</tt>:

```cpp
#include <string>
#include <boost/regex.hpp>
#include <boost/asio.hpp>
#include <vector>
#include <utility>
#include <iostream>
#include <sstream>
#include <cstdlib>
#include <algorithm>
#include <iomanip>

struct Sort { //sorting programming languages according to frequency
   bool operator( ) ( const std::pair<std::string,int> & a , const std::pair<std::string,int> & b )
      const {
	 return a.second > b.second ;
      }
} ;

int main( ) {
   try {
      //setting up an io service , with templated subelements for resolver and query
      boost::asio::io_service io_service ;
      boost::asio::ip::tcp::resolver resolver ( io_service ) ;
      boost::asio::ip::tcp::resolver::query query ( "rosettacode.org" , "http" ) ;
      boost::asio::ip::tcp::resolver::iterator endpoint_iterator = resolver.resolve( query ) ;
      boost::asio::ip::tcp::resolver::iterator end ;
      boost::asio::ip::tcp::socket socket( io_service ) ;
      boost::system::error_code error = boost::asio::error::host_not_found ;
      //looking for an endpoint the socket will be able to connect to
      while ( error && endpoint_iterator != end ) {
	 socket.close( ) ;
	 socket.connect( *endpoint_iterator++ , error ) ;
      }
      if ( error )
	 throw boost::system::system_error ( error ) ;
      //we send a request
      boost::asio::streambuf request ;
      std::ostream request_stream( &request ) ;
      request_stream << "GET " << "/mw/index.php?title=Special:Categories&limit=5000" << " HTTP/1.0\r\n" ;
      request_stream << "Host: " << "rosettacode.org" << "\r\n" ;
      request_stream << "Accept: */*\r\n" ;
      request_stream << "Connection: close\r\n\r\n" ;
      //send the request
      boost::asio::write( socket , request ) ;
      //we receive the response analyzing every line and storing the programming language
      boost::asio::streambuf response ;
      std::istream response_stream ( &response ) ;
      boost::asio::read_until( socket , response , "\r\n\r\n" ) ;
      boost::regex e( "<li><a href=\"[^<>]+?\">([a-zA-Z\\+#1-9]+?)</a>\\s?\\((\\d+) members\\)</li>" ) ;
      //using the wrong regex produces incorrect sorting!!
      std::ostringstream line ;
      std::vector<std::pair<std::string , int> > languages ; //holds language and number of examples
      boost::smatch matches ;
      while ( boost::asio::read( socket , response , boost::asio::transfer_at_least( 1 ) , error ) ) {
	 line << &response ;
	 if ( boost::regex_search( line.str( ) , matches , e ) ) {
	    std::string lang( matches[2].first , matches[2].second ) ;
	    int zahl = atoi ( lang.c_str( ) ) ;
	    languages.push_back( std::make_pair( matches[ 1 ] , zahl ) ) ;
	 }
	 line.str( "") ;//we have to erase the string buffer for the next read
      }
      if ( error != boost::asio::error::eof )
	 throw boost::system::system_error( error ) ;
      //we sort the vector entries , see the struct above
      std::sort( languages.begin( ) , languages.end( ) , Sort( ) ) ;
      int n = 1 ;
      for ( std::vector<std::pair<std::string , int> >::const_iterator spi = languages.begin( ) ;
	    spi != languages.end( ) ; ++spi ) {
	 std::cout << std::setw( 3 ) << std::right << n << '.' << std::setw( 4 ) << std::right <<
	 spi->second   << " - " << spi->first << '\n' ;
	 n++ ;
      }
   } catch ( std::exception &ex ) {
      std::cout << "Exception: " << ex.what( ) << '\n' ;
   }
   return 0 ;
}
```

{{out|Sample output (just the "top ten")}}
  1. 367 - Tcl
  2. 334 - Python
  3. 319 - Ruby
  4. 286 - C
  5. 277 - Perl
  6. 272 - OCaml
  7. 264 - Ada
  8. 241 - E
  9. 239 - AutoHotkey
 10. 193 - Forth

=={{header|Caché ObjectScript}}==


```cos
Class Utils.Net.RosettaCode [ Abstract ]
{

ClassMethod GetTopLanguages(pHost As %String = "", pPath As %String = "", pTop As %Integer = 10) As %Status
{
	// check input parameters
	If $Match(pHost, "^([a-zA-Z0-9]([a-zA-Z0-9\-]{0,61}[a-zA-Z0-9])?\.)+[a-zA-Z]{2,6}$")=0 {
		Quit $$$ERROR($$$GeneralError, "Invalid host name.")
	}

	// create http request and get page
	Set req=##class(%Net.HttpRequest).%New()
	Set req.Server=pHost
	Do req.Get(pPath)

	// create xml stream with doc type
	Set xml=##class(%Stream.GlobalCharacter).%New()
	Set sc=xml.WriteLine("<!DOCTYPE doc_type [")
	Set sc=xml.WriteLine($Char(9)_"<!ENTITY nbsp '&#160;'>")
	Set sc=xml.WriteLine($Char(9)_"<!ENTITY amp '&#38;'>")
	Set sc=xml.WriteLine("]>")

	// copy xhtml stream to xml stream
	Set xhtml=req.HttpResponse.Data
	Set xhtml.LineTerminator=$Char(10)
	While 'xhtml.AtEnd {
		Set line=xhtml.ReadLine()
		If line["!DOCTYPE" Continue
		If line["<g:plusone></g:plusone>" {
			Continue
			Set line="<g:plusone xmlns:g='http://base.google.com/ns/1.0'></g:plusone>"
		}
		Set sc=xml.WriteLine(line)
	}

	// create an instance of an %XML.XPATH.Document
	Set sc=##class(%XML.XPATH.Document).CreateFromStream(xml, .xdoc)
	If $$$ISERR(sc) Quit sc

	// evaluate following 'XPath' expression
	Set sc=xdoc.EvaluateExpression("//div[@id='bodyContent']//li", "a[contains(@href, '/Category:')]/ancestor::li", .res)

	// iterate through list elements
	Set array=##class(%ArrayOfDataTypes).%New()
	Do {
		Set dom=res.GetNext(.key)
		If '$IsObject(dom) Quit

		// get language name and members
		Set lang=""
		While dom.Read() {
			If 'dom.HasValue Continue
			If lang="" {
				If $Locate(dom.Value, "User|Tasks|Omit|attention|operations|Solutions by") Quit
				Set lang=dom.Value Continue
			}
			If dom.Value["members" {
				Set members=+$ZStrip(dom.Value, "<>P")
				Set list=array.GetAt(members)
				Set $List(list, $ListLength(list)+1)=lang
				Set sc=array.SetAt(list, members)
				Quit
			}
		}
	} While key'=""
	If array.Count()=0 Quit $$$ERROR($$$GeneralError, "No languages found.")

	// show top entries
	Write "Top "_pTop_" Languages (as at "_$ZDate($HoroLog, 2)_"):", !
	For count=1:1:pTop {
		Set members=array.GetPrevious(.key)
		If key="" Quit
		Write $Justify(count, 3), ". ", key, " - ", $ListToString(members, ", "), !
	}

	// finished
	Quit $$$OK
}

}
```

{{out|Example}}

```txt

USER>Do ##class(Utils.Net.RosettaCode).GetTopLanguages("www.rosettacode.org", "/mw/index.php?title=Special:Categories&limit=5000")
Top 10 Languages (as at 21 Apr 2013):
  1. 728 - Tcl
  2. 668 - Python
  3. 654 - C
  4. 630 - J
  5. 626 - PicoLisp
  6. 595 - D
  7. 590 - Ruby
  8. 589 - Go
  9. 576 - Perl 6
 10. 567 - Ada

```



## D

With dmd you need compile like "<tt>dmd rosetta_popularity.d -L-lphobos2 -L-lcurl</tt>".

```d
void main() {
    import std.stdio, std.algorithm, std.conv, std.array, std.regex,
           std.typecons, std.net.curl;

    immutable r1 = `"title":"Category:([^"]+)"`;
    const languages = get("www.rosettacode.org/w/api.php?action=query"~
                          "&list=categorymembers&cmtitle=Category:Pro"~
                          "gramming_Languages&cmlimit=500&format=json")
                      .matchAll(r1).map!q{ a[1].dup }.array;

    auto pairs = get("www.rosettacode.org/w/index.php?" ~
                      "title=Special:Categories&limit=5000")
                  .matchAll(`title="Category:([^"]+)">[^<]+` ~
                            `</a>[^(]+\((\d+) members\)`)
                  .filter!(m => languages.canFind(m[1]))
                  .map!(m => tuple(m[2].to!uint, m[1].dup));

    foreach (i, res; pairs.array.sort!q{a > b}.release)
        writefln("%3d. %3d - %s", i + 1, res[]);
}
```

{{out|Sample output (top twenty as of 2013-01-24)}}

```txt
  1. 717 - Tcl
  2. 663 - Python
  3. 643 - C
  4. 626 - PicoLisp
  5. 622 - J
  6. 587 - Go
  7. 587 - Ruby
  8. 585 - D
  9. 568 - Perl 6
 10. 564 - Ada
 11. 554 - Mathematica
 12. 535 - Perl
 13. 532 - Haskell
 14. 514 - BBC BASIC
 15. 505 - REXX
 16. 491 - Java
 17. 478 - OCaml
 18. 469 - PureBasic
 19. 462 - Unicon
 20. 430 - AutoHotkey
```



## Erlang


```Erlang

-module( rank_languages_by_popularity ).

-export( [task/0] ).

-record( print_fold, {place=0, place_step=1, previous_count=0} ).

task() ->
	ok = find_unimplemented_tasks:init(),
	Category_programming_languages = find_unimplemented_tasks:rosetta_code_list_of( "Programming_Languages" ),
	Programming_languages = [X || "Category:" ++ X <- Category_programming_languages],
	{ok, {{_HTTP,200,"OK"}, _Headers, Body}} = httpc:request( "http://rosettacode.org/mw/index.php?title=Special:Categories&limit=5000" ),
	Count_categories = lists:sort( [{Y, X} || {X, Y} <- category_counts(Body, []), lists:member(X, Programming_languages)] ),
	lists:foldr( fun place_count_category_write/2, #print_fold{}, Count_categories ).



category_counts( "", [[] | Acc] ) -> Acc;
category_counts( String, Acc ) ->
	{Begin, End} = category_count_begin_end( String ),
	{Category_count, String_continuation} = category_count_extract( String, Begin, End ),
	category_counts( String_continuation, [Category_count | Acc] ).

category_count_begin_end( String ) ->
	Begin = string:str( String, "/wiki/Category:" ),
	End = string:str( string:substr(String, Begin), " member" ),
	category_count_begin_end( Begin, End, erlang:length(" member") ).

category_count_begin_end( _Begin, 0, _End_length ) -> {0, 0};
category_count_begin_end( Begin, End, End_length ) ->
	{Begin, Begin + End + End_length}.

category_count_extract( _String, 0, _End ) -> {[], ""};
category_count_extract( String, Begin, End ) ->
	Category_count = category_count_extract( string:substr(String, Begin, End - Begin) ),
	{Category_count, string:substr( String, End + 1 )}.

category_count_extract( "/wiki/Category:" ++ T ) ->
	Category_member = string:tokens( T, " " ),
	Category = category_count_extract_category( Category_member ),
	Member = category_count_extract_count( lists:reverse(Category_member) ),
	{Category, Member}.

category_count_extract_category( [Category | _T] ) ->
	lists:map( fun category_count_extract_category_map/1, string:strip(Category, right, $") ).

category_count_extract_category_map( $_ ) -> $\s;
category_count_extract_category_map( Character ) -> Character.

category_count_extract_count( ["member" ++ _, "(" ++ N | _T] ) -> erlang:list_to_integer( N );
category_count_extract_count( _T ) -> 0.

place_count_category_write( {Count, Category}, Acc ) ->
	Print_fold = place_count_category_write( Count, Acc ),
	io:fwrite("~p. ~p - ~p~n", [Print_fold#print_fold.place, Count, Category] ),
	Print_fold;

place_count_category_write( Count, #print_fold{place_step=Place_step, previous_count=Count}=Print_fold ) ->
	Print_fold#print_fold{place_step=Place_step + 1};
place_count_category_write( Count, #print_fold{place=Place, place_step=Place_step} ) ->
	#print_fold{place=Place + Place_step, previous_count=Count}.

```

{{out|Sample output (top/last ten as of 2013-05-27)}}

```txt

1. 741 - "Tcl"
2. 676 - "Python"
3. 660 - "C"
4. 638 - "J"
5. 627 - "PicoLisp"
6. 609 - "Perl 6"
6. 609 - "D"
8. 607 - "Racket"
9. 592 - "Ruby"
10. 589 - "Go"
...
454. 1 - "Opa"
454. 1 - "Nickle"
454. 1 - "NewtonScript"
454. 1 - "Neko"
454. 1 - "Neat"
454. 1 - "MEL"
454. 1 - "MAPPER"
454. 1 - "LiveScript"
454. 1 - "Kotlin"
454. 1 - "Jacquard Loom"

```


=={{header|F_Sharp|F#}}==

```fsharp
open System
open System.Text.RegularExpressions

[<EntryPoint>]
let main argv =
    let rosettacodeSpecialCategoriesAddress =
        "http://www.rosettacode.org/mw/index.php?title=Special:Categories&limit=5000"
    let rosettacodeProgrammingLaguagesAddress =
        "http://rosettacode.org/wiki/Category:Programming_Languages"

    let getWebContent (url :string)  =
        using (new System.Net.WebClient()) (fun x -> x.DownloadString url)

    let regexForTitleCategoryFollowedOptionallyByMembercount =
        new Regex("""
            title="Category: (?<Name> [^"]* ) ">    # capture the name of the category
            (                   # group begin for optional part
                [^(]*           # ignore up to next open paren (on this line)
                \(              # verbatim open paren
                    (?<Number>
                        \d+     # a number (= some digits)
                    )
                    \s+         # whitespace
                    member(s?)  # verbatim text members (maybe singular)
                \)              # verbatim closing paren
            )?                  # end of optional part
            """, // " <- Make syntax highlighting happy
            RegexOptions.IgnorePatternWhitespace ||| RegexOptions.ExplicitCapture)
    let matchesForTitleCategoryFollowedOptionallyByMembercount str =
        regexForTitleCategoryFollowedOptionallyByMembercount.Matches(str)

    let languages =
        matchesForTitleCategoryFollowedOptionallyByMembercount
            (getWebContent rosettacodeProgrammingLaguagesAddress)
        |> Seq.cast
        |> Seq.map (fun (m: Match) -> (m.Groups.Item("Name").Value, true))
        |> Map.ofSeq

    let entriesWithCount =
        let parse str = match Int32.TryParse(str) with | (true, n) -> n | (false, _) -> -1
        matchesForTitleCategoryFollowedOptionallyByMembercount
            (getWebContent rosettacodeSpecialCategoriesAddress)
        |> Seq.cast
        |> Seq.map (fun (m: Match) ->
            (m.Groups.Item("Name").Value, parse (m.Groups.Item("Number").Value)))
        |> Seq.filter (fun p -> (snd p) > 0 &&  Map.containsKey (fst p) languages)
        |> Seq.sortBy (fun x -> -(snd x))


    Seq.iter2 (fun i x -> printfn "%4d. %s" i x)
        (seq { 1 .. 20 })
        (entriesWithCount |> Seq.map (fun x -> sprintf "%3d - %s" (snd x) (fst x)))
    0
```

Showing top 20 as of 2013-04-02

```txt

   1. 721 - Tcl
   2. 665 - Python
   3. 647 - C
   4. 626 - PicoLisp
   5. 622 - J
   6. 588 - Go
   7. 588 - Ruby
   8. 585 - D
   9. 569 - Perl 6
  10. 565 - Ada
  11. 555 - Mathematica
  12. 535 - Perl
  13. 533 - Haskell
  14. 514 - BBC BASIC
  15. 505 - REXX
  16. 491 - Java
  17. 480 - OCaml
  18. 469 - PureBasic
  19. 462 - Unicon
  20. 430 - AutoHotkey
```



## Go


```go
package main

import (
	"encoding/xml"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"net/url"
	"regexp"
	"sort"
	"strconv"
	"strings"
)

var baseQuery = "http://rosettacode.org/mw/api.php?action=query" +
	"&format=xml&list=categorymembers&cmlimit=500"

func req(u string, foundCm func(string)) string {
	resp, err := http.Get(u)
	if err != nil {
		log.Fatal(err) // connection or request fail
	}
	defer resp.Body.Close()
	for p := xml.NewDecoder(resp.Body); ; {
		t, err := p.RawToken()
		switch s, ok := t.(xml.StartElement); {
		case err == io.EOF:
			return ""
		case err != nil:
			log.Fatal(err)
		case !ok:
			continue
		case s.Name.Local == "cm":
			for _, a := range s.Attr {
				if a.Name.Local == "title" {
					foundCm(a.Value)
				}
			}
		case s.Name.Local == "categorymembers" && len(s.Attr) > 0 &&
			s.Attr[0].Name.Local == "cmcontinue":
			return url.QueryEscape(s.Attr[0].Value)
		}
	}
	return ""
}

// satisfy sort interface (reverse sorting)
type pop struct {
	string
	int
}
type popList []pop

func (pl popList) Len() int      { return len(pl) }
func (pl popList) Swap(i, j int) { pl[i], pl[j] = pl[j], pl[i] }
func (pl popList) Less(i, j int) bool {
	switch d := pl[i].int - pl[j].int; {
	case d > 0:
		return true
	case d < 0:
		return false
	}
	return pl[i].string < pl[j].string
}

func main() {
	// get languages, store in a map
	langMap := make(map[string]bool)
	storeLang := func(cm string) {
		if strings.HasPrefix(cm, "Category:") {
			cm = cm[9:]
		}
		langMap[cm] = true
	}
	languageQuery := baseQuery + "&cmtitle=Category:Programming_Languages"
	continueAt := req(languageQuery, storeLang)
	for continueAt != "" {
		continueAt = req(languageQuery+"&cmcontinue="+continueAt, storeLang)
	}
	// allocate slice for sorting
	s := make(popList, 0, len(langMap))

	// get big list of categories
	resp, err := http.Get("http://rosettacode.org/mw/index.php" +
		"?title=Special:Categories&limit=5000")
	if err != nil {
		log.Fatal(err)
	}
	page, err := ioutil.ReadAll(resp.Body)
	resp.Body.Close()

	// split out fields of interest and populate sortable slice
	rx := regexp.MustCompile("<li><a.*>(.*)</a>.*[(]([0-9]+) member")
	for _, sm := range rx.FindAllSubmatch(page, -1) {
		ls := string(sm[1])
		if langMap[ls] {
			if n, err := strconv.Atoi(string(sm[2])); err == nil {
				s = append(s, pop{ls, n})
			}
		}
	}

	// output
	sort.Sort(s)
	lastCnt, lastIdx := -1, 1
	for i, lang := range s {
		if lang.int != lastCnt {
			lastCnt = lang.int
			lastIdx = i + 1
		}
		fmt.Printf("%3d. %3d - %s\n", lastIdx, lang.int, lang.string)
	}
}
```

{{out|Output on 11 Aug 2014}}

```txt

  1. 832 - Tcl
  2. 783 - Racket
  3. 774 - Python
  4. 733 - Perl 6
  5. 729 - J
…
506.   1 - Supernova
506.   1 - TestML
506.   1 - Vox
506.   1 - XPath 2.0
506.   1 - Xanadu

```

(All the final entries are tied for spot 506, there are 530 lines.)


## Groovy


```groovy
def html = new URL('http://rosettacode.org/mw/index.php?title=Special:Categories&limit=5000').getText([
        connectTimeout:500,
        readTimeout:15000,
        requestProperties: [ 'User-Agent': 'Firefox/2.0.0.4']])
def count = [:]
(html =~ '<li><a[^>]+>([^<]+)</a>[^(]*[(](\\d+) member[s]*[)]</li>').each { match, language, members ->
    count[language] = (members as int)
}
count.sort { v1, v2 -> v2.value <=> v1.value }.eachWithIndex { value, index -> println "${index + 1} $value" }
```

Output:

```txt
1 Tcl=766
2 Racket=726
3 Python=712
4 Programming Tasks=695
5 C=681
6 Perl 6=649
...
48 Groovy=323
```



## Haskell



### Haskell: Using the API



```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit (simpleHttp)
import Data.List (sortBy, groupBy)
import Data.Function (on)
import Data.Map (Map, toList)

-- Record representing a single language.
data Language =
    Language {
        name      :: String,
        quantity  :: Int
    } deriving (Show)

-- Make Language an instance of FromJSON for parsing of query response.
instance FromJSON Language where
    parseJSON (Object p) = do
        categoryInfo <- p .:? "categoryinfo"

        let quantity = case categoryInfo of
                           Just ob -> ob .: "size"
                           Nothing -> return 0

            name = p .: "title"

        Language <$> name <*> quantity

-- Record representing entire response to query.
-- Contains collection of languages and optional continuation string.
data Report =
    Report {
        continue    :: Maybe String,
        languages   :: Map String Language
    } deriving (Show)

-- Make Report an instance of FromJSON for parsing of query response.
instance FromJSON Report where
    parseJSON (Object p) = do
        querycontinue <- p .:? "query-continue"

        let continue
                = case querycontinue of
                      Just ob -> fmap Just $
                                     (ob .: "categorymembers") >>=
                                     (   .: "gcmcontinue")
                      Nothing -> return Nothing

            languages = (p .: "query") >>= (.: "pages")

        Report <$> continue <*> languages

-- Pretty print a single language
showLanguage :: Int -> Bool -> Language -> IO ()
showLanguage rank tie (Language languageName languageQuantity) =
    let rankStr = show rank
    in putStrLn $ rankStr ++ "." ++
                      replicate (4 - length rankStr) ' ' ++
                      (if tie then " (tie)" else "      ") ++
                      " " ++ drop 9 languageName ++
                      " - " ++ show languageQuantity

-- Pretty print languages with common rank
showRanking :: (Int,  [Language]) -> IO ()
showRanking (ranking, languages) =
    mapM_ (showLanguage ranking $ length languages > 1) languages

-- Sort and group languages by rank, then pretty print them.
showLanguages :: [Language] -> IO ()
showLanguages allLanguages =
    mapM_ showRanking $
          zip [1..] $
          groupBy ((==) `on` quantity) $
          sortBy (flip compare `on` quantity) allLanguages

-- Mediawiki api style query to send to rosettacode.org
queryStr = "http://rosettacode.org/mw/api.php?" ++
           "format=json" ++
           "&action=query" ++
           "&generator=categorymembers" ++
           "&gcmtitle=Category:Programming%20Languages" ++
           "&gcmlimit=100" ++
           "&prop=categoryinfo"

-- Issue query to get a list of Language descriptions
runQuery :: [Language] -> String -> IO ()
runQuery ls query = do
    Just (Report continue langs) <- decode <$> simpleHttp query
    let accLanguages = ls ++ map snd (toList langs)

    case continue of
        -- If there is no continue string we are done so display the accumulated languages.
        Nothing -> showLanguages accLanguages

        -- If there is a continue string, recursively continue the query.
        Just continueStr -> do
            let continueQueryStr = queryStr ++ "&gcmcontinue=" ++ urlEncode continueStr
            runQuery accLanguages continueQueryStr

main :: IO ()
main = runQuery [] queryStr
```

{{out}}
(As of 2015-07-29.) Here we show only the top 30.

```txt
1.          Tcl - 887
2.          Racket - 877
3.          Python - 853
4.          J - 795
5.          Ruby - 775
6.          Perl 6 - 766
7.          C - 757
8.          Go - 746
9.          D - 740
10.         Perl - 710
11.         REXX - 697
12.         PicoLisp - 692
13.         Haskell - 682
14.         Mathematica - 675
15.         Java - 652
16.         Zkl - 634
17.         Ada - 623
18.         AutoHotkey - 591
19.         Unicon - 581
20.         C++ - 562
21.         Common Lisp - 551
22.         Scala - 548
23.         BBC BASIC - 532
24.         Icon - 523
25.         C sharp - 516
26.         OCaml - 508
27.         Nim - 502
28.   (tie) Clojure - 485
28.   (tie) PureBasic - 485
29.         Erlang - 455
30.         PARI/GP - 441
```



### Haskell: Using web scraping


Scraping the languages and categories pages.

```haskell
import Network.Browser
import Network.HTTP
import Network.URI
import Data.List
import Data.Maybe
import Text.XML.Light
import Control.Arrow
import Data.Ord

getRespons url = do
    rsp <- Network.Browser.browse $ do
      setAllowRedirects True
      setOutHandler $ const (return ())     -- quiet
      request $ getRequest url
    return $ rspBody $ snd rsp


mostPopLang = do
  rsp <-getRespons $ "http://www.rosettacode.org/w/api.php?action=query&list=" ++
		    "categorymembers&cmtitle=Category:Programming_Languages&cmlimit=500&format=xml"
  mbrs <- getRespons "http://www.rosettacode.org/w/index.php?title=Special:Categories&limit=5000"
  let xmls = onlyElems $ parseXML rsp
      langs = concatMap (map ((\\"Category:"). fromJust.findAttr (unqual "title")). filterElementsName (== unqual "cm")) xmls

  let catMbr = second (read.takeWhile(/=' '). drop 6). break (=='<'). drop 1. dropWhile(/='>') . drop 5
      catNmbs :: [(String, Int)]
      catNmbs = map catMbr $ filter (isPrefixOf "<li>") $ lines mbrs
      printFmt (n,(l,m)) = putStrLn $ take 6 (show n ++ ".     ") ++ (show m) ++ "  " ++ l
      toMaybe (a,b) =
	case b of
	  Just x -> Just (a,x)
	  _ -> Nothing

  mapM_ printFmt $  zip [1..] $ sortBy (flip (comparing snd))
    $ mapMaybe (toMaybe. (id &&& flip lookup catNmbs)) langs
```

{{out|First 20}}

```txt
*Main> mostPopLang
1.    421  Tcl
2.    392  Python
3.    365  PicoLisp
4.    363  J
5.    360  Ruby
6.    354  C
7.    344  Haskell
8.    337  OCaml
9.    316  Perl
10.   308  PureBasic
11.   302  AutoHotkey
12.   299  Common Lisp
13.   295  D
14.   295  Java
15.   293  Ada
16.   278  Oz
17.   260  R
18.   259  C sharp
19.   257  C++
20.   255  ALGOL 68
```



## HicEst


```hicest
CHARACTER cats*50000, catlist*50000, sortedCat*50000, sample*100
DIMENSION RankNr(1)

  READ(ClipBoard) cats
  catlist = ' '
  pos = 1 ! find language entries like    * 100 doors (2 members)
  nr = 0
  ! after next '*' find next "name" = '100 doors' and next "(...)" = '(2 members)' :
1 EDIT(Text=cats, SetPos=pos, Right='*', R, Mark1, R='(', Left, M2, Parse=name, R=2, P=members, GetPos=pos)
  IF(pos > 0) THEN
      READ(Text=members) count
      IF(count > 0) THEN
          nr = nr + 1
          WRITE(Text=catlist, Format='i4, 1x, 2a', APPend) count, name, ';'
      ENDIF
      GOTO 1 ! no WHILE in HicEst
  ENDIF      ! catlist is now = "   1  ... User ;   2  100 doors ;   3  3D ;   8  4D ; ..."

  ALLOCATE(RankNr, nr)
  EDIT(Text=catlist, SePaRators=';', Option=1+4, SorTtoIndex=RankNr) ! case (1) and back (4)

  sortedCat = ' ' ! get the sorted list in the sequence of RankNr:
  ok = 0
  DO i = 1, nr
    EDIT(Text=catlist, SePaRators=';', ITeM=RankNr(i), CoPyto=sample)
    discard = EDIT(Text=sample, LeXicon='user,attention,solutions,tasks,program,language,implementation,')
    IF(discard == 0) THEN ! removes many of the non-language entries
        ok = ok + 1
        WRITE(Text=sortedCat, APPend, Format='F5.0, 2A') ok, TRIM(sample), $CRLF
    ENDIF
  ENDDO
  DLG(Text=sortedCat, Format=$CRLF)
END
```


```hicest
2010-04-24 18:31
Top 10 entries (not all are languages)
   1. 394  Tcl
   2. 363  Python
   3. 346  Ruby
   4. 328  J
   5. 319  C
   6. 317  OCaml
   7. 315  Haskell
   8. 298  Perl
   9. 288  WikiStubs
  10. 281  Common Lisp
```


=={{header|Icon}} and {{header|Unicon}}==

### By using the API

The following solution only works in Unicon.


```unicon
$define RCLANGS "http://rosettacode.org/mw/api.php?format=xml&action=query&generator=categorymembers&gcmtitle=Category:Programming%20Languages&gcmlimit=500&prop=categoryinfo"
$define RCUA    "User-Agent: Unicon Rosetta 0.1"
$define RCXUA   "X-Unicon: http://unicon.org/"

link strings
link hexcvt

procedure main()
    cnt := create seq()
    last := -1
    every pair := !reverse(sort(langs := tallyPages(),2)) do {
        n := if last ~=:= pair[2] then @cnt else (@cnt,"")
        write(right(n,4),": ",left(pair[1],30,". "),right(pair[2],10,". "))
        }
    write(*langs, " languages")
end

# Generate page counts for each language
procedure tallyPages(url)
    /url := RCLANGS
    counts := table()
    continue := ""
    while \(txt := ReadURL(url||continue)) do {
        txt ? {
            if tab(find("gcmcontinue=")) then {
                continue := "&"||tab(upto('"'))
                move(1)
                continue ||:= tab(upto('"'))
                }
            else continue := ""
            while tab(find("<page ") & find(s := "title=\"Category:")+*s) do {
                lang := tab(upto('"'))
                tab(find(s := "pages=\"")+*s)
                counts[lang] := numeric(tab(upto('"')))
                }
            if continue == "" then return counts
            }
        }
end

procedure ReadURL(url)                 #: read URL into string
    page := open(url,"m",RCUA,RCXUA) | stop("Unable to open ",url)
    text := ""
    if page["Status-Code"] < 300 then while text ||:= reads(page,-1)
    else write(&errout,image(url),": ",
                       page["Status-Code"]," ",page["Reason-Phrase"])
    close(page)
    return text
end
```


Abridged output (top 26 languages as of July 30, 2016):

```txt

   1: Racket. . . . . . . . . . . . . . . .904
   2: Tcl . . . . . . . . . . . . . . . . .894
   3: Python. . . . . . . . . . . . . . . .867
   4: J . . . . . . . . . . . . . . . . . .852
   5: Perl 6. . . . . . . . . . . . . . . .824
   6: Ruby. . . . . . . . . . . . . . . . .796
   7: C . . . . . . . . . . . . . . . . . .777
   8: Go. . . . . . . . . . . . . . . . . .769
   9: Java. . . . . . . . . . . . . . . . .764
  10: D . . . . . . . . . . . . . . . . . .747
  11: REXX. . . . . . . . . . . . . . . . .743
  12: Perl. . . . . . . . . . . . . . . . .736
  13: Haskell . . . . . . . . . . . . . . .712
    : Zkl . . . . . . . . . . . . . . . . .712
  15: PicoLisp. . . . . . . . . . . . . . .695
  16: Mathematica . . . . . . . . . . . . .686
  17: Sidef . . . . . . . . . . . . . . . .632
  18: Ada . . . . . . . . . . . . . . . . .626
  19: C++ . . . . . . . . . . . . . . . . .619
  20: AutoHotkey. . . . . . . . . . . . . .603
  21: Unicon. . . . . . . . . . . . . . . .578
  22: Common Lisp . . . . . . . . . . . . .577
  23: Scala . . . . . . . . . . . . . . . .561
  24: BBC BASIC . . . . . . . . . . . . . .535
  25: C sharp . . . . . . . . . . . . . . .529
  26: Icon. . . . . . . . . . . . . . . . .520
...
604 languages

```


== {{header|J}} ==
{{works with|J|6.02 (32 bit only)}}
'''Solution''':
```j
require 'web/gethttp xml/sax/x2j regex'

x2jclass 'rcPopLang'

rx         =:  (<0 1) {:: (2#a:) ,~ rxmatches rxfrom ]

'Popular Languages' x2jDefn
   /                             :=  langs  : langs =: 0 2 $ a:
   html/body/div/div/div/ul/li   :=  langs =: langs ,^:(a:~:{.@[)~ lang ;  ' \((\d+) members?\)' rx y
   html/body/div/div/div/ul/li/a :=  lang  =: '^\s*((?:.(?!User|Tasks|Omit|attention|operations|by))+)\s*$' rx y
)

cocurrent'base'

sortTab    =.  \: __ ". [: ;:^:_1: {:"1
formatTab  =:  [: ;:^:_1: [: (20 A. (<'-') , |. , [: ('.' <"1@:,.~ ":) 1 + 1 i.@,~ 1{$)&.|: sortTab f.

rcPopLangs =:  formatTab@:process_rcPopLang_@:gethttp
```

'''Example''':
```j
   10 {. rcPopLangs 'http://www.rosettacode.org/w/index.php?title=Special:Categories&limit=2000'
   1. 687 - Tcl
   2. 646 - Python
   3. 637 - C
   4. 626 - PicoLisp
   5. 612 - J
   6. 587 - Go
   7. 556 - Ada
   8. 550 - D
   9. 549 - Mathematica
  10. 526 - Perl
```

'''Notes''': See some [[Talk:Sort most popular programming languages#J|notes on the J solution]].

== {{header|Java}} ==
Tested with Java 1.7. Uses the api.

```java
import  java.net.URL;
import  java.net.URLConnection;
import  java.io.*;
import  java.util.*;

public class GetRCLanguages
{
    // Custom sort Comparator for sorting the language list
    // assumes the first character is the page count and the rest is the language name
    private static class LanguageComparator implements Comparator<String>
    {
        public int compare( String a, String b )
        {
            // as we "know" we will be comparaing languages, we will assume the Strings have the appropriate format
            int result = ( b.charAt( 0 ) - a.charAt( 0 ) );
            if( result == 0 )
            {
                // the counts are the same - compare the names
                result = a.compareTo( b );
            } // if result == 0
        return result;
        } // compare
    } // LanguageComparator

    // get the string following marker in text
    private static String after( String text, int marker )
    {
        String result = "";
        int    pos    = text.indexOf( marker );
        if( pos >= 0 )
        {
            // the marker is in the string
            result = text.substring( pos + 1 );
        } // if pos >= 0
    return result;
    } // after

    // read and parse the content of path
    // results returned in gcmcontinue and languageList
    public static void parseContent( String path
                                   , String[] gcmcontinue
                                   , ArrayList<String> languageList
                                   )
    {
        try
        {

            URL            url = new URL( path );
            URLConnection  rc  = url.openConnection();
            // Rosetta Code objects to the default Java user agant so use a blank one
            rc.setRequestProperty( "User-Agent", "" );
            BufferedReader bfr = new BufferedReader( new InputStreamReader( rc.getInputStream() ) );

            gcmcontinue[0]      = "";
            String languageName = "?";
            String line         = bfr.readLine();
            while( line != null )
            {
                line = line.trim();
                if     ( line.startsWith( "[title]" ) )
                {
                    // have a programming language - should look like "[title] => Category:languageName"
                    languageName = after( line, ':' ).trim();
                }
                else if( line.startsWith( "[pages]" ) )
                {
                    // number of pages the language has (probably)
                    String pageCount = after( line, '>' ).trim();
                    if( pageCount.compareTo( "Array" ) != 0 )
                    {
                        // haven't got "[pages] => Array" - must be a number of pages
                        languageList.add( ( (char) Integer.parseInt( pageCount ) ) + languageName );
                        languageName = "?";
                    } // if [pageCount.compareTo( "Array" ) != 0
                }
                else if( line.startsWith( "[gcmcontinue]" ) )
                {
                    // have an indication of wether there is more data or not
                    gcmcontinue[0] = after( line, '>' ).trim();
                } // if various line starts
                line = bfr.readLine();
            } // while line != null
            bfr.close();
        }
        catch( Exception e )
        {
            e.printStackTrace();
        } // try-catch
    } // parseContent

    public static void main( String[] args )
    {
        // get the languages
        ArrayList<String> languageList = new ArrayList<String>( 1000 );
        String[]          gcmcontinue  = new String[1];
        gcmcontinue[0]                 = "";
        do
        {
            String path = ( "http://www.rosettacode.org/mw/api.php?action=query"
                          + "&generator=categorymembers"
                          + "&gcmtitle=Category:Programming%20Languages"
                          + "&gcmlimit=500"
                          + ( gcmcontinue[0].compareTo( "" ) == 0 ? "" : ( "&gcmcontinue=" + gcmcontinue[0] ) )
                          + "&prop=categoryinfo"
                          + "&format=txt"
                          );
            parseContent( path, gcmcontinue, languageList );
        }
        while( gcmcontinue[0].compareTo( "" ) != 0 );
        // sort the languages
        String[] languages = languageList.toArray(new String[]{});
        Arrays.sort( languages, new LanguageComparator() );
        // print the languages
        int    lastTie    = -1;
        int    lastCount  = -1;
        for( int lPos = 0; lPos < languages.length; lPos ++ )
        {
            int    count = (int) ( languages[ lPos ].charAt( 0 ) );
            System.out.format( "%4d: %4d: %s\n"
                             , 1 + ( count == lastCount ? lastTie : lPos )
                             , count
                             , languages[ lPos ].substring( 1 )
                             );
            if( count != lastCount )
            {
                lastTie   = lPos;
                lastCount = count;
            } // if count != lastCount
        } // for lPos
    } // main
} // GetRCLanguages
```

{{out}}
Top 10 languages as at 27th August 2015

```txt

   1:  883: Tcl
   2:  875: Racket
   3:  837: Python
   4:  799: J
   5:  772: Ruby
   6:  763: Perl 6
   7:  756: C
   8:  742: Go
   9:  737: D
  10:  707: Perl
  ...

```


== {{header|jq}} ==
{{works with|jq|1.4}}
The following solution matches the languages listed on the Category:Programming_Languages page with the statistics given on the Special:Categories page, making an adjustment for the number of irrelevant subcategories.

jq 1.4  cannot retrieve documents over the web and has no support for regular expressions, but is intended to work seamlessly with other command line tools, so the  following solution is presented in the form of a bash script that uses curl for retrieval, and grep and sed for screen scraping.

```sh
#!/bin/bash

# produce lines of the form: [ "language", n ]
function categories {
  curl -Ss 'http://rosettacode.org/mw/index.php?title=Special:Categories&limit=5000' |\
    grep "/wiki/Category:" | grep member | grep -v '(.*(' |\
    grep -v ' User</a>' |\
    sed -e 's/.*title="Category://' -e 's/member.*//'  |\
    sed 's:^\([^"]*\)"[^(]*(\(.*\):["\1", \2]:'
}

# produce lines of the form: "language"
function languages {
  curl -Ss 'http://rosettacode.org/wiki/Category:Programming_Languages' |\
    sed '/Pages in category "Programming Languages"/,$d' |\
    grep '<li><a href="/wiki/Category:' | fgrep title= |\
    sed 's/.*Category:\([^"]*\)".*/"\1"/'
}

categories |\
  /usr/local/bin/jq --argfile languages <(languages) -s -r '

  # input: array of [score, _] sorted by score
  # output: array of [ranking, score, _]
  def ranking:
    reduce .[] as $x
      ([];   # array of [count, rank, score, _]
       if length == 0 then [[1, 1] + $x]
       else .[length - 1] as $previous
       | if $x[0] == $previous[2]
         then . + [ [$previous[0] + 1, $previous[1]] + $x ]
         else . + [ [$previous[0] + 1, $previous[0] + 1] + $x ]
         end
       end)
    | [ .[] | .[1:] ];

    # Every language page has three category pages that should be excluded
    (reduce .[] as $pair
      ({};
       ($pair[1] as $n | if $n > 3 then . + {($pair[0]): ($n - 3)} else . end ))) as $freq
  | [ $languages[] | select($freq[.] != null) |  [$freq[.], .]]
  | sort
  | reverse
  | ranking[]
  | "\(.[0]).  \(.[1]) - \(.[2])" '
```

{{out}}

```sh
# First ten and last ten lines as of May 27, 2015
$ pop.sh
1.  868 - Tcl
2.  863 - Racket
3.  842 - Python
4.  778 - J
5.  769 - Ruby
6.  756 - Perl 6
7.  752 - C
8.  736 - D
9.  735 - Go
10.  700 - Perl
...
386.  1 - FP
386.  1 - ElastiC
386.  1 - ESQL
386.  1 - Clipper/XBase++
386.  1 - Bori
386.  1 - Biferno
386.  1 - AspectJ
386.  1 - Algae
386.  1 - 80386 Assembly
386.  1 - 68000 Assembly
```



## Julia


```julia
using HTTP

try
    response = HTTP.request("GET", "http://rosettacode.org/mw/index.php?title=Special:Categories&limit=5000")
    langcount = Dict{String, Int}()
    for mat in eachmatch(r"<li><a href[^\>]+>([^\<]+)</a>[^1-9]+(\d+)[^\w]+member.?.?</li>", String(response.body))
        if match(r"^Programming", mat.captures[1]) == nothing
            langcount[mat.captures[1]] = parse(Int, mat.captures[2])
        end
    end
    langs = sort(collect(keys(langcount)), lt=(x, y)->langcount[x]<langcount[y], rev=true)
    for (i, lang) in enumerate(langs)
        println("Language $lang can be ranked #$i at $(langcount[lang]).")
    end
catch y
    println("HTTP request failed: $y.")
    exit()
end

```
{{out}

```txt

Language Phix can be ranked #1 at 995.
Language Racket can be ranked #2 at 989.
Language Perl can be ranked #3 at 969.
Language Julia can be ranked #4 at 968.
Language C can be ranked #5 at 944.
Language Tcl can be ranked #6 at 930.
Language Zkl can be ranked #7 at 919.
Language J can be ranked #8 at 905.
Language Java can be ranked #9 at 900.
Language REXX can be ranked #10 at 892.
Language D can be ranked #11 at 874.
Language Ruby can be ranked #12 at 869.
Language Haskell can be ranked #13 at 853.
Language Scala can be ranked #14 at 792.
Language Sidef can be ranked #15 at 788.
Language PicoLisp can be ranked #16 at 775.
Language C sharp can be ranked #17 at 763.
Language Mathematica can be ranked #18 at 743.
Language C++ can be ranked #19 at 738.
Language Common Lisp can be ranked #20 at 667.
Language Ada can be ranked #21 at 656.
Language AutoHotkey can be ranked #22 at 628.
Language JavaScript can be ranked #23 at 619.
Language Lua can be ranked #24 at 618.
Language WikiStubs can be ranked #25 at 614.
...

```



### Julia: Using web scraping

{{trans|Python}}

```Julia

using HTTP, Dates
response = HTTP.request("GET", "http://rosettacode.org/wiki/Category:Programming_Languages")
languages = Set(m.captures[1] for m in eachmatch(r"title=\"Category:(.*?)\">",String(response.body)))
response = HTTP.request("GET", "http://rosettacode.org/mw/index.php?title=Special:Categories&limit=5000")
response = replace(String(response.body),"," => "")
reg = r"<li><a[^>]+>([^<]+)</a>[^(]*[\(](\d+) member[s]?[)]</li>"
ms = eachmatch(reg,response)
members  = [[1,parse(Int,m.captures[2]),m.captures[1]] for m in ms]
filter!(x -> x[3] in languages, members)
sort!(members, by = x -> (-x[2],x[3]))
for i in 2:length(members)
    if members[i-1][2] == members[i][2]
        members[i][1] = members[i-1][1]
    else
        members[i][1] = i
    end
end
println("Sample output on ", Dates.day(now()), " ", Dates.monthname(now()), " ", Dates.year(now()), ":\n")
for (rank,entries,name) in members[1:10]
    println("Rank: ", lpad(rank,4), lpad(" ($entries entries) ",16), name)
end

```

{{out}}

```txt

Sample output on 22 July 2019:

Rank:    1 (1154 entries) Go
Rank:    2 (1089 entries) Perl 6
Rank:    3 (1070 entries) Julia
Rank:    4 (1066 entries) Python
Rank:    5 (1062 entries) Phix
Rank:    6 (1045 entries) Kotlin
Rank:    7 (1026 entries) Perl
Rank:    8  (991 entries) Racket
Rank:    9  (952 entries) C
Rank:   10  (945 entries) J

```



## Kotlin

{{trans|Java}}

```scala
import java.net.URL
import java.io.*

object Popularity {
    /** Gets language data. */
    fun ofLanguages(): List<String> {
        val languages = mutableListOf<String>()
        var gcm = ""
        do {
            val path = url + (if (gcm == "") "" else "&gcmcontinue=" + gcm) + "&prop=categoryinfo" + "&format=txt"
            try {
                val rc = URL(path).openConnection() // URL completed, connection opened
                // Rosetta Code objects to the default Java user agent so use a blank one
                rc.setRequestProperty("User-Agent", "")
                val bfr = BufferedReader(InputStreamReader(rc.inputStream))
                try {
                    gcm = ""
                    var languageName = "?"
                    var line: String? = bfr.readLine()
                    while (line != null) {
                        line = line.trim { it <= ' ' }
                        if (line.startsWith("[title]")) {
                            // have a programming language - should look like "[title] => Category:languageName"
                            languageName = line[':']
                        } else if (line.startsWith("[pages]")) {
                            // number of pages the language has (probably)
                            val pageCount = line['>']
                            if (pageCount != "Array") {
                                // haven't got "[pages] => Array" - must be a number of pages
                                languages += pageCount.toInt().toChar() + languageName
                                languageName = "?"
                            }
                        } else if (line.startsWith("[gcmcontinue]"))
                            gcm = line['>']  // have an indication of whether there is more data or not
                        line = bfr.readLine()
                    }
                } finally {
                    bfr.close()
                }
            } catch (e: Exception) {
                e.printStackTrace()
            }
        } while (gcm != "")

        return languages.sortedWith(LanguageComparator)
    }

    /** Custom sort Comparator for sorting the language list.
     * Assumes the first character is the page count and the rest is the language name. */
    internal object LanguageComparator : java.util.Comparator<String> {
        override fun compare(a: String, b: String): Int {
            // as we "know" we will be comparing languages, we will assume the Strings have the appropriate format
            var r = b.first() - a.first()
            return if (r == 0) a.compareTo(b) else r
            // r == 0: the counts are the same - compare the names
        }
    }

    /** Gets the string following marker in text. */
    private operator fun String.get(c: Char) = substringAfter(c).trim { it <= ' ' }

    private val url = "http://www.rosettacode.org/mw/api.php?action=query" +
            "&generator=categorymembers" + "&gcmtitle=Category:Programming%20Languages" +
            "&gcmlimit=500"
}

fun main(args: Array<String>) {
    // read/sort/print the languages (CSV format):
    var lastTie = -1
    var lastCount = -1
    Popularity.ofLanguages().forEachIndexed { i, lang ->
        val count = lang.first().toInt()
        if (count == lastCount)
            println("%12s%s".format("", lang.substring(1)))
        else {
            println("%4d, %4d, %s".format(1 + if (count == lastCount) lastTie else i, count, lang.substring(1)))
            lastTie = i
            lastCount = count
        }
    }
}
```

{{out}}

```txt
   1,  901, Racket
   2,  893, Tcl
   3,  851, Python
   4,  826, J
   5,  796, Perl 6
   ...
 135,   70, Kotlin
   ...
```



## Lasso


```Lasso>
```txt
<code
[
sys_listtraits !>> 'xml_tree_trait' ? include('xml_tree.lasso')
local(lang = array)
local(f = curl('http://rosettacode.org/mw/index.php?title=Special:Categories&limit=5000')->result->asString)
local(ff) = xml_tree(#f)
local(lis = #ff->body->div(3)->div(3)->div(3)->div->ul->getnodes)
with li in #lis do => {
	local(title = #li->a->attribute('title'))
	#title->removeLeading('Category:')
	local(num = #li->asString->split('(')->last)
	#num->removeTrailing(')')
	#num->removeTrailing('members')
	#num->removeTrailing('member')
	#num->trim
	#num = integer(#num)
	#lang->insert(#title = #num)
}
local(c = 1)
with l in #lang
order by #l->second descending
do => {^
	#c++
	'. '+#l->second + '  - ' + #l->first+'\r'
^}
]</code>
```

```


{{out}}

```txt
1. 759  - Tcl
2. 724  - Racket
3. 707  - Python
4. 692  - Programming Tasks
5. 672  - C
6. 643  - J
7. 635  - Perl 6
8. 632  - D
9. 627  - PicoLisp
10. 617  - Ruby
...
```



## M2000 Interpreter

Based on BBC BASIC idea, and using M2000 Rinstr() and objects as Inventories, Document, and Microsoft.XMLHTTP for downloading (async use). Results also copy to Clipboard. We can use Msxml2.ServerXMLHTTP (code is the same, only name of object change).

Update: Numbers above 999 get a comma (,) so we have to drop this using Filter$()


```M2000 Interpreter

Module RankLanguages {
      Const Part1$="<a href="+""""+ "/wiki/Category", Part2$="member"
      Const langHttp$="http://rosettacode.org/wiki/Category:Programming_Languages"
      Const categoriesHttp$="http://www.rosettacode.org/mw/index.php?title=Special:Categories&limit=5000"
      Def long m, i,j, tasks, counter, limit, T, t1
      Def string LastLang$, job$

      Document final$, languages$, categories$
      httpGet$=lambda$  (url$, timeout=1000)->{
            Declare htmldoc "Msxml2.ServerXMLHTTP"
            With htmldoc , "readyState" as ready
            Report "Download:"+url$
            Method htmldoc "open","get", url$, True
            Method htmldoc "send"
            Profiler
            While Ready<>4 {
                  Wait 20
                  Print Over format$("Wait: {0:3} sec", timecount/1000)
                  If timecount>timeout then Exit
            }
            If ready=4 Then  With  htmldoc, "responseText" as ready$ : =ready$
            Declare htmldoc Nothing
            print
      }

      languages$=httpGet$(langHttp$, 30000)
      If Doc.Len(languages$)=0 then  Error "File download failed (languages)"

      Inventory Lang

      m=Paragraph(languages$, 0)
      If Forward(languages$,m) then {
            While m {
                  job$=Paragraph$(languages$,(m))
                  If Instr(job$, part1$) Else Continue
                  i = Instr(job$, "</a>")
                  If i Else Continue   ' same as If i=0 Then Continue
                  j = i
                   i=Rinstr(job$, ">", -i)
                  If i Else Continue
                  LastLang$=MID$(job$, i+1, j-i-1)
                  if Instr(job$, "Category:"+lastlang$) then Append lang, lastlang$:=0  :  Print Over format$("Languages: {0}", len(lang))
            }
      }
      Print
      Document categories$=httpGet$(categoriesHttp$, 30000)
      If Doc.Len(categories$)=0 then  Error "File download failed (categories)"
      limit=Doc.Par(categories$)
      If limit<Len(Lang) then Error "Invalid data"
      Refresh
      set slow
      m=Paragraph(categories$, 0)
      counter=0
      If Forward(categories$,m) then {
            While m {
                  job$=Paragraph$(categories$,(m))
                  counter++
                  Print Over format$("{0:2:-6}%", counter/limit*100)
                  i=Instr(job$, part2$)
                  If  i Else Continue
                  i=Rinstr(job$, "(", -i)
                  If  i Else Continue
                  tasks=Val(Filter$(Mid$(job$, i+1),","))
                  If tasks Else Continue
                  i=Rinstr(job$, "<", -i)
                  If i Else Continue
                  j = i
                  i=Rinstr(job$, ">", -i)
                  If i Else Continue
                  LastLang$=MID$(job$, i+1, j-i-1)
                  If Exist(Lang, LastLang$) Then {
                         Return Lang, LastLang$:=Lang(LastLang$)+tasks
                  }
            }
      }
      Print
      \\ this type of inventory can get same keys
      \\ also has stable sort
      Report "Make Inventory list by Task"
      Inventory queue ByTask
      t1=Len(Lang)
      T=Each(Lang)
      While T {
            Append ByTask, Eval(T):=Eval$(T!)
            Print Over format$("Complete: {0} of {1}", T^+1, t1 )
      }
      Print
      Report "Sort by task (stable sort, sort keys as numbers)"
      Sort descending ByTask as number
      Report "Make List"
      T=Each(ByTask)
      final$="Sample output on "+Date$(Today, 1033, "long date")+{:

      }
      While T {
            final$=format$("rank:{0::-4}. {1:-5} entries - {2}", T^+1, Eval$(T!), Eval$(T))+{
            }
      }
      Report "Copy to Clipboard"
      clipboard final$
      \\ present to console with 3/4 fill lines then stop for space bar or mouse click to continue
      Report final$
}
RankLanguages

```


{{out}}
<pre style="height:30ex;overflow:scroll">
Sample output on Saturday, June 22, 2019:

rank:   1.  1149 entries - Go
rank:   2.  1084 entries - Perl 6
rank:   3.  1061 entries - Python
rank:   4.  1045 entries - Kotlin
rank:   5.  1044 entries - Julia
rank:   6.  1033 entries - Phix
rank:   7.  1026 entries - Perl
rank:   8.   991 entries - Racket
rank:   9.   951 entries - C
rank:  10.   944 entries - J
rank:  11.   938 entries - Zkl
rank:  12.   930 entries - Tcl
rank:  13.   910 entries - REXX
rank:  14.   909 entries - Java
rank:  15.   883 entries - Ruby
rank:  16.   881 entries - D
rank:  17.   869 entries - Haskell
rank:  18.   814 entries - Sidef
rank:  19.   801 entries - Scala
rank:  20.   779 entries - C sharp
rank:  21.   775 entries - PicoLisp
rank:  22.   772 entries - C++
rank:  23.   748 entries - Mathematica
rank:  24.   678 entries - Common Lisp
rank:  25.   668 entries - Ada
rank:  26.   639 entries - JavaScript
rank:  27.   633 entries - Lua
rank:  28.   628 entries - AutoHotkey
rank:  29.   611 entries - Ring
rank:  30.   595 entries - Factor
rank:  31.   594 entries - Clojure
rank:  32.   590 entries - Unicon
rank:  33.   566 entries - ALGOL 68
rank:  34.   563 entries - Nim
rank:  35.   563 entries - PureBasic
rank:  36.   560 entries - BBC BASIC
rank:  37.   559 entries - Fortran
rank:  38.   556 entries - OCaml
rank:  39.   540 entries - PARI/GP
rank:  40.   538 entries - F Sharp
rank:  41.   532 entries - Icon
rank:  42.   517 entries - Elixir
rank:  43.   497 entries - FreeBASIC
rank:  44.   495 entries - Erlang
rank:  45.   485 entries - Rust
rank:  46.   473 entries - PowerShell
rank:  47.   462 entries - Jq
rank:  48.   456 entries - Pascal
rank:  49.   453 entries - AWK
rank:  50.   450 entries - Forth
rank:  51.   448 entries - Seed7
rank:  52.   435 entries - R
rank:  53.   414 entries - Groovy
rank:  54.   403 entries - PL/I
rank:  55.   401 entries - PHP
rank:  56.   383 entries - VBA
rank:  57.   361 entries - Scheme
rank:  58.   359 entries - MATLAB
rank:  59.   353 entries - Swift
rank:  60.   346 entries - M2000 Interpreter
rank:  61.   343 entries - Maple
rank:  62.   337 entries - Liberty BASIC
rank:  63.   314 entries - Run BASIC
rank:  64.   309 entries - Prolog

..........
rank: 685.     1 entries - Jacquard Loom
rank: 686.     1 entries - Kamailio Script
rank: 687.     1 entries - Lambda Prolog
rank: 688.     1 entries - LibreOffice Basic
rank: 689.     1 entries - MAPPER
rank: 690.     1 entries - MEL
rank: 691.     1 entries - MiniZinc
rank: 692.     1 entries - Mond
rank: 693.     1 entries - Monkey
rank: 694.     1 entries - NASL
rank: 695.     1 entries - Neat
rank: 696.     1 entries - NewtonScript
rank: 697.     1 entries - Nickle
rank: 698.     1 entries - Nix
rank: 699.     1 entries - Opa
rank: 700.     1 entries - Pare
rank: 701.     1 entries - Qore
rank: 702.     1 entries - Rapira
rank: 703.     1 entries - RPGIV
rank: 704.     1 entries - Setl4
rank: 705.     1 entries - Soar
rank: 706.     1 entries - SoneKing Assembly
rank: 707.     1 entries - Supernova
rank: 708.     1 entries - SuperTalk
rank: 709.     1 entries - Terra
rank: 710.     1 entries - TestML
rank: 711.     1 entries - WebAssembly
rank: 712.     1 entries - Wollok
rank: 713.     1 entries - Xanadu
rank: 714.     1 entries - Ya
rank: 715.     1 entries - МiniZinc
rank: 716.     0 entries - AngelScript
rank: 717.     0 entries - Binary Lambda Calculus
rank: 718.     0 entries - EhBASIC
rank: 719.     0 entries - Epigram
rank: 720.     0 entries - FLORA-2
rank: 721.     0 entries - Florid
rank: 722.     0 entries - Gerbil
rank: 723.     0 entries - LC2200 Assembly
rank: 724.     0 entries - Leon
rank: 725.     0 entries - Livecode
rank: 726.     0 entries - LLP
rank: 727.     0 entries - Loglan82
rank: 728.     0 entries - Lolli
rank: 729.     0 entries - Lygon
rank: 730.     0 entries - ObjectIcon
rank: 731.     0 entries - PL/B
rank: 732.     0 entries - Plan
rank: 733.     0 entries - Reduce
rank: 734.     0 entries - Rubylog
rank: 735.     0 entries - S BASIC
rank: 736.     0 entries - SimpleLang
rank: 737.     0 entries - Star
rank: 738.     0 entries - X10
rank: 739.     0 entries - XS
</pre >


## Maple


```Maple
count_sizes := proc(arr_name,arr_pop,i,lst)
	local index := i;
	local language;
	for language in lst do
		language := language[1]:
		arr_name(index) := txt["query"]["pages"][language]["title"][10..]:
		if(assigned(txt["query"]["pages"][language]["categoryinfo"]["size"])) then
			arr_pop(index) := txt["query"]["pages"][language]["categoryinfo"]["size"]:
		else:
			arr_pop(index) := 0:
		end if:
		index++:
	end do:
	return index:
end proc:

txt := JSON:-ParseFile("http://rosettacode.org/mw/api.php?action=query&generator=categorymembers&gcmtitle=Category:Programming%20Languages&gcmlimit=350&prop=categoryinfo&format=json"):
arr_name := Array():
arr_pop := Array():
i := count_sizes(arr_name, arr_pop, 1, [indices(txt["query"]["pages"])]):
while (assigned(txt["continue"]["gcmcontinue"])) do
	continue := txt["continue"]["gcmcontinue"]:
	txt := JSON:-ParseFile(cat("http://rosettacode.org/mw/api.php?action=query&generator=categorymembers&gcmtitle=Category:Programming%20Languages&gcmlimit=350&prop=categoryinfo&format=json", "&continue=", txt["continue"]["continue"], "&gcmcontinue=", txt["continue"]["gcmcontinue"])):
	i:=count_sizes(arr_name,arr_pop,i,[indices(txt["query"]["pages"])]):
end do:
arr_name:= arr_name[sort(arr_pop,output=permutation)]:
arr_pop := sort(arr_pop, output=sorted):
i := i-1:
for x from i to 1 by -1 do
	printf("rank %d    %d examples    %s\n", i-x+1, arr_pop[x], arr_name[x]):
end do:
```

{{Out|Output}}

```txt
#10:30 AM 10/05/2018
rank 1    1002 examples   Kotlin
rank 2    977 examples    Racket
rank 3    977 examples    Python
rank 4    949 examples    Perl 6
rank 5    918 examples    Tcl
rank 6    898 examples    C
rank 7    891 examples    J
rank 8    879 examples    Zkl
rank 9    870 examples    Java
rank 10   860 examples    D
...
rank 680    0 examples    Epigram
rank 681    0 examples    LLP
rank 682    0 examples    Lolli
rank 683    0 examples    Leon
rank 684    0 examples    Florid
rank 685    0 examples    Loglan82
```



## Mathematica


```Mathematica
Languages = Flatten[Import["http://rosettacode.org/wiki/Category:Programming_Languages","Data"][[1,1]]];
Languages = Most@StringReplace[Languages, {" " -> "_", "+" -> "%2B"}];
b = {#, If[#  ===  {}, 0, #[[1]]]&@( StringCases[Import["http://rosettacode.org/wiki/Category:"<>#,"Plaintext"],
   "category, out of " ~~ x:NumberString ~~ " total" ->x])} &/@ Languages;
For[i = 1, i < Length@b , i++ , Print[i, ". ", #[[2]], " - ", #[[1]] ]&@ Part[Reverse@SortBy[b, Last], i]]
```

{{out|Output : As of 29 February 2012}}

```txt

1. 637 - Tcl
2. 576 - C
3. 558 - J
4. 538 - Go
5. 485 - Ada
6. 456 - D
7. 450 - Haskell
8. 441 - Mathematica
9. 432 - Java
10. 425 - Icon
...
```



## Nim


```nim
import httpclient, json, re, strutils, algorithm

const
  langSite = "http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Languages&cmlimit=500&format=json"
  catSize = "http://www.rosettacode.org/w/index.php?title=Special:Categories&limit=5000"
let regex = re"title=""Category:(.*?)"">.+?</a>.*\((.*) members\)"

var langs: seq[string] = @[]
for l in parseJson(getContent(langSite))["query"]["categorymembers"]:
  langs.add(l["title"].str.split("Category:")[1])

var ranks: seq[tuple[lang: string, count: int]] = @[]
for line in getContent(catSize).findAll(regex):
  let lang = line.replacef(regex, "$1")
  if lang in langs:
    let count = parseInt(line.replacef(regex, "$2").strip())
    ranks.add((lang, count))

ranks.sort(proc (x, y): int = cmp[int](y.count, x.count))
for i, l in ranks:
  echo align($(i+1), 3), align($l.count, 5), " - ", l.lang
```

Output:

```txt
  1  833 - Tcl
  2  781 - Racket
  3  770 - Python
  4  730 - Perl 6
  5  725 - J
  6  712 - C
  7  708 - Ruby
  8  698 - D
  9  674 - Go
 10  656 - Perl
...
```



## Objeck


```objeck
use HTTP;
use RegEx;
use XML;
use Collection;

class RosettaRank {
  function : Main(args : String[]) ~ Nil {
    langs_xml := "";
    client := HttpClient->New();
    in := client->Get("http://rosettacode.org/mw/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Languages&cmlimit=5000&format=xml");
    each(i : in) {
      langs_xml += in->Get(i)->As(String);
    };

    langs := StringSet->New();
    parser := XmlParser->New(langs_xml);
    if(parser->Parse()) {
      # get first item
      results := parser->FindElements("/api/query/categorymembers/cm");
      each(i : results) {
        element := results->Get(i)->As(XmlElement);
        name := element->GetAttribute("title")->GetValue();
        offset := name->Find(':');
        if(offset > -1) {
          lang := name->SubString(offset + 1, name->Size() - offset - 1);
          langs->Insert(lang->ReplaceAll("&#x20;", " "));
        };
      };
    };

    langs_counts := IntMap->New();
    client := HttpClient->New();
    html := client->Get("http://rosettacode.org/mw/index.php?title=Special:Categories&limit=5000");
    each(i : html) {
      lines := html->Get(i)->As(String);
      html_elements := lines->Split("\n");
      each(j : html_elements) {
        element := html_elements[j];
        name : String; count : String;
        regex := RegEx->New("<li><a href=\"(\\w|\\s|/|\\?|\\&|;|:|#)+\"\\stitle=\"Category:(\\w|\\s|#)+\">");
        found := regex->FindFirst(element);
        if(found <> Nil) {
          group1 := found->Size();
          regex := RegEx->New("(\\w|\\s)+");
          found := regex->Match(element, group1);
          if(found <> Nil & found->Size() > 0) {
            name := found;
            # skip over some junk characters
            group2 := group1 + found->Size() + 10;
            regex := RegEx->New("\\s\\(");
            found := regex->Match(element, group2);
            if(found <> Nil) {
              group3 := group2 + found->Size();
              regex := RegEx->New("\\d+");
              found := regex->Match(element, group3);
              if(found <> Nil & found->Size() > 0) {
                count := found;
              };
            };
          };
        };

        if(name <> Nil & count <> Nil) {
          if(langs->Has(name)) {
            langs_counts->Insert(count->ToInt(), name);
          };
          name := Nil; count := Nil;
        };
      };
    };

    keys := langs_counts->GetKeys();
    count := 1;
    for(i := keys->Size() - 1; i >= 0; i -=1;) {
      key := keys->Get(i);
      IO.Console->Print(count)->Print(". ")->Print(key)->Print(" - ")->PrintLine(langs_counts->Find(key)->As(String));
      count += 1;
    };
  }
}
```



```txt
1. 849 - Tcl
2. 826 - Racket
3. 821 - Python
4. 759 - J
5. 757 - Ruby
6. 751 - Perl 6
7. 743 - C
8. 736 - D
9. 728 - Go
10. 696 - Perl
...
```



## ooRexx

{{trans|REXX}}

```oorexx
/* REXX ---------------------------------------------------------------
* Create a list of Rosetta Code languages showing the number of tasks
* This program's logic is basically that of the REXX program
* rearranged to my taste and utilizing the array class of ooRexx
* which offers a neat way of sorting as desired, see :CLASS mycmp below
* For the input to this program open these links:
*  http://rosettacode.org/wiki/Category:Programming_Languages
*  http://www.rosettacode.org/mw/index.php?title=Special:Categories&limit=5000
* and save the pages as LAN.txt and CAT.txt, respectively
* Output: RC_POP.txt list of languages sorted by popularity
* If test=1, additionally:
*         RC_LNG.txt list of languages alphabetically sorted
*         RC_TRN.txt list language name translations (debug)
*--------------------------------------------------------------------*/
test=1
name.='??'
l.=0
safe=''
x00='00'x
linfid='RC_LAN.txt'
linfid='LAN.txt'                       /* language file              */
cinfid='CAT.txt'                       /* category file              */
oid='RC_POP.txt'; 'erase' oid
If test Then Do
  tid='RC.TRN.txt'; 'erase' tid
  tia='RC_LNG.txt'; 'erase' tia
  End
Call init

call read_lang                         /* process language file      */

Call read_cat                          /* process category file      */

Call ot words(lang_list) 'possible languages'
Call ot words(lang_listr) 'relevant languages'
llrn=words(lang_listr)

If test Then
  Call no_member

a=.array~new                           /* create array object        */
cnt.=0
Do i=1 By 1 While lang_listr<>''
  Parse Var lang_listr ddu0 lang_listr
  ddu=translate(ddu0,' ',x00)
  a[i]=right(mem.ddu,3) name.ddu       /* fill array element         */
  z=mem.ddu                            /* number of members          */
  cnt.z=cnt.z+1                        /* number of such languages   */
  End
n=i-1                                  /* number of output lines     */

a~sortWith(.mycmp~new)                 /* sort the array elements    */
                                       /* see :CLASS mycmp below     */
/*---------------------------------------------------------------------
* and now create the output
*--------------------------------------------------------------------*/
Call o ' '
Call o center('timestamp: ' date() time('Civil'),79,'-')
Call o ' '
Call o right(lrecs,9) 'records read from file: ' linfid
Call o right(crecs,9) 'records read from file: ' cinfid
Call o right(llrn,9) 'relevant languages'
Call o ' '

rank=0
rank.=0
Do i=1 To n
  rank=rank+1
  Parse Value a[i] With o . 6 lang
  ol='          rank: 'right(rank,3)'               '||,
                    '('right(o,3) 'entries)  'lang
  If cnt.o>1 Then Do
    If rank.o=0 Then
      rank.o=rank
    ol=overlay(right(rank.o,3),ol,17)
    ol=overlay('[tied]',ol,22)
    End
  Call o ol
  End

Call o ' '
Call o center('+ end of list +',72)
Say 'Output in' oid

If test Then Do
  b=.array~new                         /* create array object        */
  cnt.=0
  Do i=1 By 1 While lang_list<>''
    Parse Var lang_list ddu0 lang_list
    ddu=translate(ddu0,' ',x00)
    b[i]=right(mem.ddu,3) name.ddu       /* fill array element         */
    End
  n=i-1                                  /* number of output lines     */

  b~sortWith(.alpha~new)               /* sort the array elements    */
  Call oa n 'languages'
  Do i=1 To n
    Call oa b[i]
    End
  Say 'Sorted list of languages in' tia
  End
Exit

o:
  Return lineout(oid,arg(1))
ot:
  If test Then Call lineout tid,arg(1)
  Return
oa:
  If test Then Call lineout tia,arg(1)
  Return

read_lang:
/*---------------------------------------------------------------------
* Read the language page to determine the list of possible languages
* Output: l.lang>0  for all languages found
*         name.lang original name of uppercased language name
*         lang_list list of uppercased language names
*         lrecs     number of records read from language file
*--------------------------------------------------------------------*/
  l.=0
  name.='??'
  lang_list=''
  Do lrecs=0 While lines(linfid)\==0
    l=linein(linfid)                   /* read from language file    */
    l=translate(l,' ','9'x)            /* turn tabs to blanks        */
    dd=space(l)                        /* remove extra blanks        */
    ddu=translate(dd)
    If pos('AUTOMATED ADMINISTRATION',ddu)>0 Then /* ignore this     */
      Iterate
    If pos('RETRIEVED FROM',ddu)\==0 Then /* this indicates the end  */
      Leave
    If dd=='' Then                     /* ignore all blank lines.    */
      Iterate
    If left(dd,1)\=='*' Then           /* ignore lines w/o *         */
      Iterate
    ddo=fix_lang(dd)                   /* replace odd language names */
    If ddo<>dd Then Do                 /* show those that we found   */
      Call ot ' ' dd
      Call ot '>' ddo
      dd=ddo
      End
    Parse Var dd '*' dd "<"            /* extract the language name  */
    ddu=strip(translate(dd))           /* translate to uppercase     */
    If name.ddu='??' Then
      name.ddu=dd                      /* remember 1st original name */
    l.ddu=l.ddu+1
    ddu_=translate(ddu,x00,' ')
    If wordpos(ddu_,lang_list)=0 Then
      lang_list=lang_list ddu_
    End
  Return

read_cat:
/*---------------------------------------------------------------------
* Read the category page to get language names and number of members
* Output: mem.ddu   number of members for (uppercase) Language ddu
*         lang_listr  the list of relevant languages
*--------------------------------------------------------------------*/
  mem.=0
  lang_listr=''
  Do crecs=0 While lines(cinfid)\==0
    l=get_cat(cinfid)                  /* read from category file    */
    l=translate(l,' ','9'x)            /* turn tabs to blanks        */
    dd=space(l)                        /* remove extra blanks        */
    If dd=='' Then                     /* ignore all blank lines.    */
      Iterate
    ddu=translate(dd)
    ddo=fix_lang(dd)                   /* replace odd language names */
    If ddo<>dd Then Do                 /* show those that we found   */
      Call ot ' ' dd
      Call ot '>' ddo
      dd=ddo
      End
    du=translate(dd)                   /* get an uppercase version.  */
    If pos('RETRIEVED FROM',du)\==0 Then  /* this indicates the end  */
      Leave
    Parse Var dd dd '<' "(" mems .     /* extract the language name  */
                                       /* and the number of members  */
    dd=space(substr(dd,3))
    _=translate(dd)
    If \l._ Then                       /* not a known language       */
      Iterate                          /* ignore                     */
    if pos(',', mems)\==0  then
      mems=changestr(",", mems, '')    /* remove commas.             */
    If\datatype(mems,'W') Then         /* not a number of members    */
      Iterate                          /* ignore                     */
    ddu=space(translate(dd))
    mem.ddu=mem.ddu+mems               /* set o add number of members*/
    Call memory ddu                    /* list of relevant languages */
    End
  Return

get_cat:
/*---------------------------------------------------------------------
* get a (logical) line from the category file
* These two lines
*   * Lotus 123 Macro Scripting
*     </wiki/Category:Lotus_123_Macro_Scripting>â€â€Ž (3 members)
* are returned as one line:
*-> * Lotus 123 Macro Scripting     </wiki/Cate  ... (3 members)
* we need language name and number of members in one line
*--------------------------------------------------------------------*/
  Parse Arg fid
  If safe<>'' Then
    ol=safe
  Else Do
    If lines(fid)=0 Then
      Return ''
    ol=linein(fid)
    safe=''
    End
  If left(ol,3)='  *' Then Do
    Do Until left(r,3)=='  *' | lines(fid)=0
      r=linein(fid)
      If left(r,3)=='  *' Then Do
        safe=r
        Return ol
        End
      Else
        ol=ol r
      End
    End
  Return ol

memory:
  ddu0=translate(ddu,x00,' ')
  If wordpos(ddu0,lang_listr)=0 Then
    lang_listr=lang_listr ddu0
  Return

fix_lang: Procedure Expose old. new.
  Parse Arg s
  Do k=1 While old.k\==''         /* translate Unicode variations.  */
    If pos(old.k,s)\==0 Then
      s=changestr(old.k,s,new.k)
    End
  Return s

init:
  old.=''
  old.1='UC++'        /* '55432B2B'X                                 */
  new.1="µC++"        /* old      UC++ --?ASCII-8: µC++              */
  old.2='ÐœÐš-61/52'  /* 'D09CD09A2D36312F3532'X                     */
  new.2='MK-61/52'    /* somewhere a mistranslated: MK-              */
  old.3='DÃ©jÃ  Vu'   /* '44C3A96AC3A0205675'X                       */
  new.3='Déjà Vu'     /* Unicode +¬j+á --?ASCII-8: Déjá              */
  old.4='CachÃ©'      /* '43616368C3A9'X                             */
  new.4='Caché'       /* Unicode ach+¬ --?ASCII-8: Caché             */
  old.5='ÎœC++'       /* 'CE9C432B2B'X                               */
  new.5="MC++"        /* Unicode +£C++ --?ASCII-8: µC++              */
  /*-----------------------------------------------------------------*/
  Call ot 'Language replacements:'
  Do ii=1 To 5
    Call ot ' ' left(old.ii,10) left(c2x(old.ii),20) '->' new.ii
    End
  Call ot ' '
  Return

no_member: Procedure Expose lang_list lang_listr tid x00 test
/*---------------------------------------------------------------------
* show languages found in language file that are not referred to
* in the category file
*--------------------------------------------------------------------*/
  ll =wordsort(lang_list )             /* languages in language file */
  llr=wordsort(lang_listr)             /* languages in category file */
  Parse Var ll l1 ll
  Parse Var llr l2 llr
  nn.=0
  Do Forever
    Select
      When l1=l2 Then Do
        If l1='' Then                  /* both lists empty           */
          Leave
        Parse Var ll l1 ll             /* get the next language      */
        Parse Var llr l2 llr           /* -"-                        */
        End
      When l1<l2 Then Do               /* in language file           */
                                       /* and not in category file   */
        z=nn.0+1
        nn.z='   'translate(l1,' ',x00) /* show in test output       */
        nn.0=z
        Parse Var ll l1 ll             /* next from language file    */
        End
      Otherwise Do
        Call ot '?? 'translate(l2,' ',x00) /* show in test output    */
        Say 'In category file but not in language file:'
        Say '?? 'translate(l2,' ',x00)
        Say 'Hit enter to proceed'
        Pull .
        Parse Var llr l2 llr           /* next from category file    */
        End
      End
    End
  Call ot nn.0 'Languages without members:' /* heading                    */
  Do ii=1 To nn.0
    Call ot nn.ii
    End
  Return

::CLASS mycmp MIXINCLASS Comparator
::METHOD compare
/**********************************************************************
* smaller number is considered higher
* numbers equal: higher language considered higher
* otherwise return lower
**********************************************************************/
  Parse Upper Arg a,b
  Parse Var a na +4 ta
  Parse Var b nb +4 tb
  Select
    When na<<nb THEN res=1
    When na==nb Then Do
      If ta<<tb Then res=-1
                Else res=1
      End
    Otherwise        res=-1
    End
  Return res


::CLASS alpha MIXINCLASS Comparator
::METHOD compare
/**********************************************************************
* higher language considered higher
* otherwise return lower
**********************************************************************/
  Parse Upper Arg a,b
  Parse Var a na +4 ta
  Parse Var b nb +4 tb
  If ta<<tb Then res=-1
            Else res=1
  Return res
```


Output (the beginning):

```txt

------------------------timestamp:  24 Nov 2013 9:48am-------------------------

      706 records read from file:  LAN.txt
     2204 records read from file:  CAT.txt
      502 relevant languages

          rank:   1               (760 entries)  Tcl
          rank:   2               (724 entries)  Racket
          rank:   3               (708 entries)  Python
          rank:   4               (681 entries)  C

```

Note: If MC++ and µC++ are the same, they should/could be added together to get 501 languages.


## Oz

{{libheader|OzHttpClient}}
Using web scraping. Does not filter non-language categories.

```oz
declare
  [HTTPClient] = {Module.link ['x-ozlib://mesaros/net/HTTPClient.ozf']}
  [Regex] = {Module.link ['x-oz://contrib/regex']}

  fun {GetPage RawUrl}
     Client = {New HTTPClient.urlGET init(inPrms(toFile:false toStrm:true) _)}
     Url = {VirtualString.toString RawUrl}
     OutParams
     HttpResponseParams
  in
     {Client getService(Url ?OutParams ?HttpResponseParams)}
     {Client closeAll(true)}
     OutParams.sOut
  end

  fun {GetCategories Doc}
     {Map {Regex.allMatches "<li><a[^>]+>([^<]+)</a> \\(([0-9]+) member" Doc}
      fun {$ Match}
	 Category = {Regex.group 1 Match Doc}
	 Count = {String.toInt {ByteString.toString {Regex.group 2 Match Doc}}}
      in
	 Category#Count
      end
     }
  end

  Url = "http://www.rosettacode.org/mw/index.php?title=Special:Categories&limit=5000"

  {System.showInfo "Retrieving..."}
  Doc = {GetPage Url}

  {System.showInfo "Parsing..."}
  Cs = {GetCategories Doc}
in
  for
     Cat#Count in {Sort Cs fun {$ _#C1 _#C2} C1 > C2 end}
     I in 1..20
  do
     {System.showInfo I#". "#Count#" - "#Cat}
  end
```

{{out}}

```txt

1. 371 - Tcl
2. 369 - Programming Tasks
3. 338 - Python
4. 324 - Ruby
5. 306 - Haskell
...
17. 225 - Oz
18. 214 - C++
19. 209 - JavaScript
20. 208 - ALGOL 68

```



## Perl


### By using the API


```perl
use 5.010;
use MediaWiki::API;

my $api =
  MediaWiki::API->new( { api_url => 'http://rosettacode.org/mw/api.php' } );

my @languages;
my $gcmcontinue;
while (1) {
    my $apih = $api->api(
        {
            action      => 'query',
            generator   => 'categorymembers',
            gcmtitle    => 'Category:Programming Languages',
            gcmlimit    => 250,
            prop        => 'categoryinfo',
            gcmcontinue => $gcmcontinue
        }
    );
    push @languages, values %{ $apih->{'query'}{'pages'} };

    last if not $gcmcontinue = $apih->{'continue'}{'gcmcontinue'};
}

for (@languages) {
    $_->{'title'} =~ s/Category://;
    $_->{'categoryinfo'}{'size'} //= 0;
}

my @sorted_languages =
  reverse sort { $a->{'categoryinfo'}{'size'} <=> $b->{'categoryinfo'}{'size'} }
  @languages;

binmode STDOUT, ':encoding(utf8)';
my $n = 1;
for (@sorted_languages) {
    printf "%3d. %20s - %3d\n", $n++, $_->{'title'},
      $_->{'categoryinfo'}{'size'};
}

```

{{out}}

```txt
  1.                   Go - 1177
  2.                 Phix - 1116
  3.               Perl 6 - 1107
  4.                Julia - 1103
  5.               Python - 1080
  6.               Kotlin - 1053
  7.               Racket - 1045
  8.                 Perl - 1045
  9.                    C - 969
 10.                  Zkl - 960
...
```



## Perl 6



### Perl 6: Using the API

Note that this counts '''only''' the tasks. It does not include other non-task categories in the counts yielding more realistic, non-inflated numbers. Perl 6 is unicode aware and handles non-ASCII names natively. This does not attempt to 'unify' different language names that are the same behind the scenes as a result of Rosettacodes'   capitalization peculiarities. (E.G. μC++, UC++ & ΜC++)


```perl6
use HTTP::UserAgent;
use URI::Escape;
use JSON::Fast;
use Sort::Naturally;

my $client = HTTP::UserAgent.new;

my $url = 'http://rosettacode.org/mw';

my $tablefile = './RC_Popularity.txt';

my %cat = (
    'Programming_Tasks' => 'Task',
    'Draft_Programming_Tasks' => 'Draft'
);
my %tasks;

for %cat.keys.sort -> $cat {
    mediawiki-query(
        $url, 'pages',
        :generator<categorymembers>,
        :gcmtitle("Category:$cat"),
        :gcmlimit<350>,
        :rawcontinue(),
        :prop<title>
    ).map({ %tasks{%cat{$cat}}++ });
}

my %counts =
    mediawiki-query(
        $url, 'pages',
        :generator<categorymembers>,
        :gcmtitle<Category:Programming Languages>,
        :gcmlimit<350>,
        :rawcontinue(),
        :prop<categoryinfo>
    )
    .map({
        my $title = .<title>.subst(/^'Category:'/, '');
        my $tasks = (.<categoryinfo><pages> || 0);
        my $categories = (.<categoryinfo><subcats> || 0);
        my $total = (.<categoryinfo><size> || 0);
       $title  => [$tasks ,$categories, $total]
   });

my $out = open($tablefile, :w)  or die "$!\n";

# Add table boilerplate and header
$out.say:
    "\{|class=\"wikitable sortable\"\n",
    "|+ As of { Date.today } :: {+%counts} Languages\n",
    '!Rank!!Language!!Task
Entries!!Tasks
done %!!Non-task
Subcate-
gories!!Total
Categories'
;

my @bg = <#fff; #ccc;>;
my $ff = 0;
my $rank = 1;
my $ties = 0;

# Get sorted unique task counts
for %counts.values»[0].unique.sort: -* -> $count {
    $ff++;
    # Get list of tasks with this count
    my @these = %counts.grep( *.value[0] == $count )».keys.sort: *.&naturally;

    for @these {
        $ties++;
        $out.say:
          "|- style=\"background-color: { @bg[$ff % 2] }\"\n"~
          "|$rank\n"~
          "|[[:Category:$_|]]\n"~
          "|$count\n"~
          "|{(100 * $count/%tasks<Draft Task>.sum).round(.01)} %\n"~
          "|{%counts{$_}[1]}\n"~
          "|{%counts{$_}[2]}"
    }
    $rank += $ties;
    $ties = 0;
}
$out.say( "|}" );
$out.say('=' x 5, " query, download & processing: {(now - INIT now).round(.01)} seconds ", '=' x 5);
$out.close;

sub mediawiki-query ($site, $type, *%query) {
    my $url = "$site/api.php?" ~ uri-query-string(
        :action<query>, :format<json>, :formatversion<2>, |%query);
    my $continue = '';

    gather loop {
        my $response = $client.get("$url&$continue");
        my $data = from-json($response.content);
        take $_ for $data.<query>.{$type}.values;
        $continue = uri-query-string |($data.<query-continue>{*}».hash.hash or last);
    }
}

sub uri-query-string (*%fields) {
    join '&', %fields.map: { "{.key}={uri-escape .value}" }
}
```


{{out|Abridged output}} See [[Rosetta_Code/Rank_languages_by_popularity/Full_list|full output here]].
{|class="wikitable sortable"
|+ As of 2019-02-10 :: 715 Languages
!Rank!!Language!!Task
Entries!!Tasks
done %!!Non-task
Subcate-
gories!!Total
Categories
|- style="background-color: #ccc;"
|1
|[[:Category:Go|Go]]
|1100
|96.15 %
|4
|1104
|- style="background-color: #fff;"
|2
|[[:Category:Perl 6|Perl 6]]
|1046
|91.43 %
|4
|1050
|- style="background-color: #ccc;"
|3
|[[:Category:Kotlin|Kotlin]]
|1029
|89.95 %
|1
|1030
|- style="background-color: #fff;"
|4
|[[:Category:Python|Python]]
|1017
|88.9 %
|17
|1034
|- style="background-color: #ccc;"
|5
|[[:Category:Phix|Phix]]
|991
|86.63 %
|3
|994
|- style="background-color: #fff;"
|6
|[[:Category:Racket|Racket]]
|986
|86.19 %
|3
|989
|- style="background-color: #ccc;"
|7
|[[:Category:Perl|Perl]]
|963
|84.18 %
|5
|968
|- style="background-color: #fff;"
|8
|[[:Category:Julia|Julia]]
|958
|83.74 %
|3
|961
|- style="background-color: #ccc;"
|9
|[[:Category:C|C]]
|940
|82.17 %
|3
|943
|- style="background-color: #fff;"
|10
|[[:Category:Tcl|Tcl]]
|925
|80.86 %
|4
|929
|- style="background-color: #ccc;"
|11
|[[:Category:Zkl|Zkl]]
|918
|80.24 %
|1
|919
|- style="background-color: #fff;"
|12
|[[:Category:J|J]]
|901
|78.76 %
|3
|904
|- style="background-color: #ccc;"
|13
|[[:Category:Java|Java]]
|897
|78.41 %
|3
|900
|- style="background-color: #fff;"
|14
|[[:Category:REXX|REXX]]
|886
|77.45 %
|4
|890
|- style="background-color: #ccc;"
|15
|[[:Category:D|D]]
|871
|76.14 %
|3
|874
|- style="background-color: #fff;"
|16
|[[:Category:Ruby|Ruby]]
|866
|75.7 %
|3
|869
|- style="background-color: #ccc;"
|17
|[[:Category:Haskell|Haskell]]
|849
|74.21 %
|3
|852
|- style="background-color: #fff;"
|18
|[[:Category:Scala|Scala]]
|789
|68.97 %
|3
|792
|- style="background-color: #ccc;"
|19
|[[:Category:Sidef|Sidef]]
|787
|68.79 %
|1
|788
|- style="background-color: #fff;"
|20
|[[:Category:PicoLisp|PicoLisp]]
|772
|67.48 %
|3
|775
|}


### Perl 6: Using web scraping


Scraping the languages and categories pages.  Perl 6 automatically handles Unicode names correctly.

```perl6
my $languages =  qx{wget -O - 'http://rosettacode.org/wiki/Category:Programming_Languages'};
my $categories = qx{wget -O - 'http://www.rosettacode.org/mw/index.php?title=Special:Categories&limit=5000'};

my @lines = $languages.lines;
shift @lines until @lines[0] ~~ / '<h2>Subcategories</h2>' /;
my \languages = set gather for @lines {
    last if / '/bodycontent' /;
    take ~$0 if
        / '<li><a href="/wiki/Category:' .*? '" title="Category:' .*? '">' (.*?) '</a></li>' /;
}

@lines = $categories.lines;
my @results = sort -*.[0], gather for @lines {
    take [+$1.subst(',', ''), ~$0] if
        / '<li><a href="/wiki/Category:' .*? '" title="Category:' .*? '">'
        (.*?) <?{ ~$0 ∈ languages }>
        '</a>' .*? '(' (<[, 0..9]>+) ' member' /;
}

for @results.kv -> $i, @l {
    printf "%d:\t%4d - %s\n", $i+1, |@l;
}
```

{{out}}
(As of 2018-10-28) Here we show only the top 10.

```txt
1:	1036 - Go
2:	1029 - Kotlin
3:	1014 - Python
4:	 998 - Perl 6
5:	 980 - Racket
6:	 929 - Tcl
7:	 926 - C
8:	 910 - Perl
9:	 904 - Zkl
10:	 901 - J
```



## Phix

The distributed version also has an output_html option.

```Phix
-- demo\rosetta\Rank_Languages.exw
include builtins\timedate.e
include builtins\libcurl.e

constant output_users = false,
         limit = 20,    -- 0 to list all
         refresh_cache = timedelta(days:=1),    -- 0 for always
         languages = "http://rosettacode.org/wiki/Category:Programming_Languages",
         categories = "http://www.rosettacode.org/mw/index.php?title=Special:Categories&limit=5000"

function open_download(string filename, url)
    bool refetch = true
    if file_exists(filename) then
        -- use existing file if <= refresh_cache (1 day) old
        sequence last_mod = get_file_date(filename)     -- (0.8.1+)
        atom delta = timedate_diff(last_mod,date())
        refetch = (delta>refresh_cache)
    end if
    if refetch then
        printf(1,"Downloading %s...\n",{filename})
        CURLcode res = curl_easy_get_file(url,"",filename) -- (no proxy)
        if res!=CURLE_OK then
            string error = sprintf("%d",res)
            if res=CURLE_COULDNT_RESOLVE_HOST then
                error &= " [CURLE_COULDNT_RESOLVE_HOST]"
            end if
            printf(1, "Error %s downloading file\n", error)
            {} = wait_key()
            abort(0)
        end if
    end if
    return get_text(filename)
end function

constant cat_title = "title=\"Category:"

function extract_names()
sequence results = {} -- {rank,count,name}

    -- 1) extract languages from eg title="Category:Phix"
    sequence language_names = {}
    string langs = open_download("languages.htm",languages),
           language_name
    langs = langs[1..match("<div class=\"printfooter\">",langs)-1]
    integer start = match("<h2>Subcategories</h2>",langs), k
    while true do
        k = match(cat_title,langs,start)
        if k=0 then exit end if
        k += length(cat_title)
        start = find('"',langs,k)
        language_name = langs[k..start-1]
        language_names = append(language_names,language_name)
    end while

    -- 2) extract results from eg title="Category:Phix">Phix</a>?? (997 members)</li>
    --     but obviously only when we have found that language in the phase above.
    --     (note there is / ignore some wierd uncode-like stuff after the </a>...)
    string cats = open_download("categories.htm",categories)
    start = 1
    while true do
        k = match(cat_title,cats,start)
        if k=0 then exit end if
        k += length(cat_title)
        start = find('"',cats,k)
        language_name = cats[k..start-1]
        start = match("</a>",cats,start)+4
        if output_users then
            if length(language_name)>5
            and language_name[-5..-1] = " User" then
                language_name = language_name[1..-6]
            else
                language_name = ""
            end if
        end if
        if length(language_name)
        and find(language_name,language_names) then
            while not find(cats[start],"(<") do start += 1 end while -- (ignore)
            string members = cats[start..find('<',cats,start+1)]
            members = substitute(members,",","")
            sequence res = scanf(members,"(%d member%s)<")
            results = append(results,{0,res[1][1],language_name})
        end if
    end while
    results = sort_columns(results,{-2,3}) -- (descending 2nd column, then asc 3rd)

    --3) assign rank
    integer count, prev = 0, rank
    for i=1 to length(results) do
        count = results[i][2]
        if count!=prev then rank = i end if
        prev = count
        results[i][1] = rank
    end for

    return results
end function

procedure show(sequence results)
    for i=1 to iff(limit?limit:length(results)) do
        printf(1,"%3d: %,d - %s\n",results[i])
    end for
end procedure

show(extract_names())
```

{{out}}
As of July 31st, 2019

```txt

Downloading rc_cache\languages.htm...
Downloading rc_cache\categories.htm...
  1: 1,156 - Go
  2: 1,092 - Phix
  3: 1,090 - Perl 6
  4: 1,074 - Julia
  5: 1,066 - Python
  6: 1,046 - Kotlin
  7: 1,026 - Perl
  8: 991 - Racket
  9: 953 - C
 10: 945 - J
 11: 943 - Zkl
 12: 930 - Tcl
 13: 915 - REXX
 14: 910 - Java
 15: 897 - Ruby
 16: 892 - D
 17: 872 - Haskell
 18: 816 - Sidef
 19: 814 - Scala
 20: 786 - C sharp

```



## PicoLisp


```PicoLisp
(load "@lib/http.l")

(for (I . X)
   (flip
      (sort
         (make
            (client "rosettacode.org" 80
               "mw/index.php?title=Special:Categories&limit=5000"
               (while (from "<li><a href=\"/wiki/Category:")
                  (let Cat (till "\"")
                     (from "(")
                     (when (format (till " " T))
                        (link (cons @ (ht:Pack Cat))) ) ) ) ) ) ) )
   (prinl (align 3 I) ". " (car X) " - " (cdr X)) )
```

{{out|Output (dec15)}}

```txt
  1. 903 - Racket
  2. 897 - Tcl
  3. 863 - Python
  4. 821 - J
  5. 796 - Perl_6
  6. 780 - Programming_Tasks
  7. 778 - Ruby
  8. 767 - C
  9. 751 - Go
 10. 745 - D
...
```



## PowerShell

{{trans|C#}}

```PowerShell

$get1 = (New-Object Net.WebClient).DownloadString("http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Languages&cmlimit=700&format=json")
$get2 = (New-Object Net.WebClient).DownloadString("http://www.rosettacode.org/w/index.php?title=Special:Categories&limit=5000")
$match1 = [regex]::matches($get1, "`"title`":`"Category:(.+?)`"")
$match2 = [regex]::matches($get2, "title=`"Category:([^`"]+?)`">[^<]+?</a>[^\(]*\((\d+) members\)")
$r = 1
$langs = $match1 | foreach { $_.Groups[1].Value.Replace("\","") }
$res = $match2 | sort -Descending {[Int]$($_.Groups[2].Value)} | foreach {
    if ($langs.Contains($_.Groups[1].Value))
    {
        [pscustomobject]@{
            Rank = "$r"
            Members =  "$($_.Groups[2].Value)"
            Language = "$($_.Groups[1].Value)"
        }
        $r++
    }
}
1..30 | foreach{
    [pscustomobject]@{
        "Rank 1..30" = "$($_)"
        "Members 1..30" =  "$($res[$_-1].Members)"
        "Language 1..30" = "$($res[$_-1].Language)"
        "Rank 31..60" = "$($_+30)"
        "Members 31..60" =  "$($res[$_+30].Members)"
        "Language 31..60" = "$($res[$_+30].Language)"
    }
}| Format-Table -AutoSize

```

<b>Output: August 04, 2015</b>

```txt

Rank 1..30 Members 1..30 Language 1..30 Rank 31..60 Members 31..60 Language 31..60
---------- ------------- -------------- ----------- -------------- ---------------
1          887           Tcl            31          405            Seed7
2          877           Racket         32          397            Julia
3          853           Python         33          389            PL/I
4          798           J              34          387            Fortran
5          775           Ruby           35          386            ALGOL 68
6          766           Perl 6         36          376            Lua
7          758           C              37          369            Pascal
8          746           Go             38          367            R
9          740           D              39          364            Groovy
10         710           Perl           40          363            F Sharp
11         701           REXX           41          363            Forth
12         692           PicoLisp       42          358            PHP
13         682           Haskell        43          342            AWK
14         675           Mathematica    44          340            Sidef
15         652           Java           45          335            MATLAB
16         623           Ada            46          325            Liberty BASIC
17         591           AutoHotkey     47          297            Octave
18         562           C++            48          287            Factor
19         551           Common Lisp    49          286            Scheme
20         548           Scala          50          285            NetRexx
21         532           BBC BASIC      51          284            Oforth
22         523           Icon           52          280            Oz
23         516           C sharp        53          274            Run BASIC
24         508           OCaml          54          272            E
25         502           Nim            55          271            Bracmat
26         488           PureBasic      56          268            PowerShell
27         487           Clojure        57          263            Prolog
28         455           Erlang         58          260            Lasso
29         441           PARI/GP        59          249            Delphi
30         434           JavaScript     60          239            Smalltalk

```


### PowerShell: Using web scraping

{{trans|Python}}

```PowerShell

$response = (New-Object Net.WebClient).DownloadString("http://rosettacode.org/wiki/Category:Programming_Languages")
$languages = [regex]::matches($response,'title="Category:(.*?)">') | foreach {$_.Groups[1].Value}

$response = [Net.WebClient]::new().DownloadString("http://rosettacode.org/mw/index.php?title=Special:Categories&limit=5000")
$response = [regex]::Replace($response,'(\d+),(\d+)','$1$2')

$members  = [regex]::matches($response,'<li><a[^>]+>([^<]+)</a>[^(]*[(](\d+) member[s]?[)]</li>') | foreach { [pscustomobject]@{
            Members =  [Int]($_.Groups[2].Value)
            Language = [String]($_.Groups[1].Value)
        }} | where {$languages.Contains($_.Language)} | sort -Descending Members

Get-Date -UFormat "Sample output on %d %B %Y at %R %Z"
$members | Select-Object -First 10 | foreach -Begin {$r, $rank, $count = 0, 0,-1} {
    $r++
    if ($count -ne $_.Members) {$rank = $r}
    $count = $_.Members
    $x = $_.Members.ToString("N0",[System.Globalization.CultureInfo]::CreateSpecificCulture('en-US'))
    $entry = "($x entries)"
    [String]::Format("Rank: {0,2} {1,15} {2}",$rank,$entry,$_.Language)
}

```

{{out}}

```txt

Sample output on 13 septembre 2019 at 12:17 +02
Rank:  1 (1,177 entries) Go
Rank:  2 (1,116 entries) Phix
Rank:  3 (1,107 entries) Perl 6
Rank:  4 (1,104 entries) Julia
Rank:  5 (1,080 entries) Python
Rank:  6 (1,053 entries) Kotlin
Rank:  7 (1,048 entries) Perl
Rank:  8 (1,045 entries) Racket
Rank:  9   (970 entries) C
Rank: 10   (960 entries) Zkl

```



## PureBasic


### Using MediaWiki API method


```purebasic
Procedure handleError(value, msg.s)
  If value = 0
    MessageRequester("Error", msg)
    End
  EndIf
EndProcedure

Structure languageInfo
  name.s
  pageCount.i
EndStructure

#JSON_web_data = 0 ;ID# for our parsed JSON web data object

Define NewList languages.languageInfo()

Define blah.s, object_val, allPages_mem, title_mem, page_mem, categoryInfo_mem, continue_mem
Define url$, title$, currentPage$, language$, langPageCount, gcmcontinue$, *bufferPtr

handleError(InitNetwork(), "Unable to initialize network functions.")

Repeat
  url$ = "http://www.rosettacode.org/mw/api.php?action=query" +
         "&generator=categorymembers&gcmtitle=Category:Programming%20Languages" +
         "&gcmlimit=500" + "&gcmcontinue=" + gcmcontinue$ +
         "&prop=categoryinfo&format=json"

  *bufferPtr = ReceiveHTTPMemory(url$)
  handleError(*bufferPtr, "Unable to receive web page data.")
  If CatchJSON(#JSON_web_data, *bufferPtr, MemorySize(*bufferPtr)) = 0
    MessageRequester("Error", JSONErrorMessage() + " at position " +
                              JSONErrorPosition() + " in line " +
                              JSONErrorLine() + " of JSON web Data")
    End
  EndIf
  FreeMemory(*bufferPtr)

  object_val = JSONValue(#JSON_web_data)
  allPages_mem = GetJSONMember(GetJSONMember(object_val, "query"), "pages")
  If ExamineJSONMembers(allPages_mem)
    While NextJSONMember(allPages_mem)
      page_mem = JSONMemberValue(allPages_mem)
      title_mem = GetJSONMember(page_mem, "title")
      If title_mem
        title$ = GetJSONString(title_mem)
        If Left(title$, 9) = "Category:"
          language$ = Mid(title$, 10)
          categoryInfo_mem = GetJSONMember(page_mem, "categoryinfo")
          If categoryInfo_mem
            langPageCount = GetJSONInteger(GetJSONMember(categoryInfo_mem, "pages"))
          Else
            langPageCount = 0
          EndIf

          AddElement(languages())
          languages()\name = language$
          languages()\pageCount = langPageCount
        EndIf
      EndIf
    Wend
  EndIf

  ;check for continue
  continue_mem = GetJSONMember(object_val, "continue")
  If continue_mem
    gcmcontinue$ = GetJSONString(GetJSONMember(continue_mem, "gcmcontinue"))
  Else
    gcmcontinue$ = ""
  EndIf

  FreeJSON(#JSON_web_data)
Until gcmcontinue$ = ""

;all data has been aquired, now process and display it
SortStructuredList(languages(), #PB_Sort_Descending, OffsetOf(languageInfo\pageCount), #PB_Integer)

If OpenConsole()

  If ListSize(languages())
    Define i, *startOfGroupPtr.languageInfo, *lastElementPtr, groupSize, rank
    Define outputSize = 100, outputLine

    PrintN(Str(ListSize(languages())) + " languages." + #CRLF$)
    LastElement(languages())
    *lastElementPtr = @languages() ;pointer to last element
    FirstElement(languages())
    *startOfGroupPtr = @languages() ;pointer to first element
    groupSize = 1
    rank = 1

    While NextElement(languages())
      If languages()\pageCount <> *startOfGroupPtr\pageCount Or *lastElementPtr = @languages()
        ;display a group of languages at the same rank
        ChangeCurrentElement(languages(), *startOfGroupPtr)
        For i = 1 To groupSize
          ;display output in groups to allow viewing of all entries
          If outputLine = 0
            PrintN(" Rank   Tasks  Language")
            PrintN(" ------ -----  --------")
          EndIf

          PrintN(RSet(Str(rank), 6) + ".  " +
                 RSet(Str(languages()\pageCount), 4) + "  " +
                 languages()\name)

          outputLine + 1
          If outputLine >= outputSize
            Print(#CRLF$ + #CRLF$ + "Press ENTER to continue" + #CRLF$): Input()
            outputLine = 0
          EndIf

          NextElement(languages())
        Next

        rank + groupSize
        groupSize = 1
        *startOfGroupPtr = @languages()
      Else
        groupSize + 1
      EndIf
    Wend
  Else
    PrintN("No language categories found.")
  EndIf

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
608 languages.

 Rank   Tasks  Language
 ------ -----  --------
     1.   904  Racket
     2.   894  Tcl
     3.   852  Python
     4.   836  J
     5.   803  Perl 6
     6.   787  Ruby
     7.   772  C
     8.   752  Go
     9.   743  D
    10.   728  Perl
    11.   726  REXX
    12.   702  Haskell
    13.   697  Java
    14.   696  PicoLisp
    15.   693  Zkl
    16.   687  Mathematica
    17.   625  Ada
    18.   603  C++
    18.   603  Sidef
    18.   603  AutoHotkey
    21.   578  Unicon
    22.   574  Common Lisp
    23.   558  Scala
    24.   530  BBC BASIC
    25.   527  C sharp
    26.   520  Icon
    27.   507  OCaml
    28.   503  Nim
    29.   497  PureBasic
.....
   514.     0  Rubylog
   514.     0  Refal
   514.     0  QuakeC
   514.     0  Mirelle
   514.     0  .QL
   514.     0  M680x0
   514.     0  Datalog
   514.     0  Cilk++
   514.     0  Cecil
   514.     0  AspectC++
   514.     0  8080 Assembly
   514.     0  OpenC++
   514.     0  AMPL
   514.     0  UserRPL
   514.     0  TeLa
   514.     0  UScript
   514.     0  TAL
   514.     0  Zonnon
   514.     0  Superbase BASIC
   514.     0  VAX Assembly
   514.     0  Star
   514.     0  VRML
   514.     0  WML
   514.     0  X10
   514.     0  XS
   514.     0  XBase
   514.     0  UC++
   514.     0  True BASIC
   514.     0  Thistle

```



### Using web scraping method


```purebasic

;Uses a web scraping method.

;It is limited to only retrieving 5000 language categories and the counts contain
;some slight inaccuracies.

Structure Language
  count.i
  Name.s
EndStructure

Dim Row.Language(5000)

Procedure handleError(value, msg.s)
  If value = 0
    MessageRequester("Error", msg)
    End
  EndIf
EndProcedure

handleError(InitNetwork(), "Unable to initialize network functions.")
; Lines have been split to fit RC's 80 char preferences
ignore$ = "Basic language learning Encyclopedia Implementations " +
          "Language Implementations Language users " +
          "Maintenance/OmitCategoriesCreated Programming Languages " +
          "Programming Tasks RCTemplates Solutions by Library Solutions by " +
          "Programming Language Solutions by Programming Task Unimplemented " +
          "tasks by language WikiStubs Examples needing attention " +
          "Impl needed"

url$ = "http://www.rosettacode.org/mw/index.php?" +
     "title=Special:Categories&limit=5000"

ReceiveHTTPFile(url$, "special.htm")
ReadFile(0, "special.htm", #PB_UTF8)
While Not Eof(0)
  i + 1
  x1$ =  ReadString(0)
  x2$ = Mid(x1$, FindString(x1$, "member", 1) - 4 , 3)
  Row(i)\count = Val(Trim(RemoveString(x2$, "(")))

  x3$ = Mid(x1$, FindString(x1$, Chr(34) + ">", 1) + 2, 30)
  Row(i)\Name = Left(x3$, FindString(x3$, "<", 1) - 1)
  If FindString(ignore$, Row(i)\Name, 1) Or Row(i)\Name = ""
    Row(i)\count = 0
  EndIf
Wend



offset=OffsetOf(Language\count)
SortStructuredArray(Row(), #PB_Sort_Descending, offset, #PB_Integer)
OpenConsole()
For i = 0 To 29
  PrintN( Str(i + 1) + ". " + Str(Row(i)\count) + " - " + Row(i)\Name)
Next
Input()
```

Sample output:

```txt
1. 907 - Racket
2. 898 - Tcl
3. 868 - Python
4. 839 - J
5. 807 - Perl 6
6. 790 - Ruby
7. 756 - Go
8. 746 - D
9. 732 - Perl
10. 729 - REXX
11. 705 - Haskell
12. 700 - Java
13. 699 - PicoLisp
14. 694 - Zkl
15. 690 - Mathematica
16. 629 - Ada
17. 606 - C++
18. 606 - AutoHotkey
19. 604 - Sidef
20. 581 - Unicon
21. 577 - Common Lisp
22. 561 - Scala
23. 533 - BBC BASIC
24. 530 - C sharp
25. 523 - Icon
26. 510 - OCaml
27. 504 - Nim
28. 500 - PureBasic
29. 494 - Clojure
30. 491 - JavaScript
```



## Python



### Python: Using web scraping

Using <code>requests</code> library.


```python
import requests
import re

response = requests.get("http://rosettacode.org/wiki/Category:Programming_Languages").text
languages = re.findall('title="Category:(.*?)">',response)[:-3] # strip last 3

response = requests.get("http://rosettacode.org/mw/index.php?title=Special:Categories&limit=5000").text
response = re.sub('(\d+),(\d+)',r'\1'+r'\2',response)           # strip ',' from popular languages above 999 members
members  = re.findall('<li><a[^>]+>([^<]+)</a>[^(]*[(](\\d+) member[s]*[)]</li>',response) # find language and members

cnt = 0
for (language, members) in sorted(members, key=lambda x: -int(x[1])):
    if language in languages:
        cnt += 1
        print("{:4d} {:4d} - {}".format(cnt, int(members), language))
    if cnt >= 15:    # show only top 15 languages
        break


```

{{out|Output (as of Jan 26, 2019)}}
   1 1095 - Go
   2 1043 - Perl 6
   3 1032 - Python
   4 1030 - Kotlin
   5  994 - Phix
   6  988 - Racket
   7  957 - Perl
   8  941 - C
   9  941 - Julia
  10  929 - Tcl
  11  916 - Zkl
  12  903 - J
  13  895 - Java
  14  886 - REXX
  15  874 - D


### Python: Using MediaWiki API method


```python

import requests
import operator
import re

api_url    = 'http://rosettacode.org/mw/api.php'
languages  = {}

parameters = {
    'format':       'json',
    'action':       'query',
    'generator':    'categorymembers',
    'gcmtitle':     'Category:Programming Languages',
    'gcmlimit':     '200',
    'gcmcontinue':  '',
    'continue':     '',
    'prop':         'categoryinfo'
}

while(True):
    response = requests.get(api_url, params=parameters).json()
    for k,v in response['query']['pages'].items():
        if 'title' in v and 'categoryinfo' in v:
          languages[v['title']]=v['categoryinfo']['size']
    if 'continue' in response:
        gcmcontinue = response['continue']['gcmcontinue']
#        print(gcmcontinue)
        parameters.update({'gcmcontinue': gcmcontinue})
    else:
        break

# report top 15 languages
for i, (language, size) in enumerate(sorted(languages.items(), key=operator.itemgetter(1), reverse=True)[:15]):
    print("{:4d} {:4d} - {}".format(i+1, size, re.sub('Category:','',language))) # strip Category: from language



```

{{out|Output (as of Oct 06, 2019)}}
   1 1183 - Go
   2 1121 - Phix
   3 1110 - Perl 6
   4 1108 - Julia
   5 1093 - Python
   6 1060 - Perl
   7 1057 - Kotlin
   8 1046 - Racket
   9  971 - C
  10  964 - Zkl
  11  945 - J
  12  941 - REXX
  13  931 - Tcl
  14  911 - Java
  15  904 - Ruby


## R

{{incorrect|R|I believe you need to use <tt>continue gcmcontinue</tt> to get complete results.}}

```rsplus

library(rvest)
library(plyr)
library(dplyr)
options(stringsAsFactors=FALSE)

langUrl <- "http://rosettacode.org/mw/api.php?format=xml&action=query&generator=categorymembers&gcmtitle=Category:Programming%20Languages&prop=categoryinfo&gcmlimit=5000"
langs <- html(langUrl) %>%
  html_nodes('page')

ff <- function(xml_node) {
  language <- xml_node %>% html_attr("title")
  language <- sub("^Category:", "", language)
  npages <- xml_node %>% html_nodes('categoryinfo') %>%
    html_attr("pages")
  c(language, npages)
}
tbl <- ldply(sapply(langs, ff), rbind)
names(tbl) <- c("language", "n")
tbl %>%
  mutate(n=as.integer(n)) %>%
  arrange(desc(n)) %>%
  head

```

{{out|Output (as of March, 23, 2019)}}

```txt

  language    n
1       Go 1114
2   Perl 6 1059
3   Kotlin 1029
4     Phix  993
5    Julia  992
6     Perl  978

```



## Racket


{{trans|Python}}


```racket
#lang racket

(require racket/hash
         net/url
         json)

(define limit 15)
(define (replacer cat) (regexp-replace #rx"^Category:(.*?)$" cat "\\1"))
(define category "Category:Programming Languages")
(define entries "entries")

(define api-url (string->url "http://rosettacode.org/mw/api.php"))
(define (make-complete-url gcmcontinue)
  (struct-copy url api-url
               [query `([format . "json"]
                        [action . "query"]
                        [generator . "categorymembers"]
                        [gcmtitle . ,category]
                        [gcmlimit . "200"]
                        [gcmcontinue . ,gcmcontinue]
                        [continue . ""]
                        [prop . "categoryinfo"])]))

(define @ hash-ref)

(define table (make-hash))

(let loop ([gcmcontinue ""])
  (define resp (read-json (get-pure-port (make-complete-url gcmcontinue))))
  (hash-union! table
               (for/hash ([(k v) (in-hash (@ (@ resp 'query) 'pages))])
                 (values (@ v 'title #f) (@ (@ v 'categoryinfo (hash)) 'size 0))))
  (cond [(@ resp 'continue #f) => (λ (c) (loop (@ c 'gcmcontinue)))]))

(for/fold ([prev #f] [rank #f] #:result (void))
          ([item (in-list (sort (hash->list table) > #:key cdr))] [i (in-range limit)])
  (match-define (cons cat size) item)
  (define this-rank (if (equal? prev size) rank (add1 i)))
  (printf "Rank: ~a ~a ~a\n"
          (~a this-rank #:align 'right #:min-width 2)
          (~a (format "(~a ~a)" size entries) #:align 'right #:min-width 14)
          (replacer cat))
  (values size this-rank))
```


Output on September 4, 2019:


```txt

Rank:  1 (1176 entries) Go
Rank:  2 (1112 entries) Phix
Rank:  3 (1107 entries) Perl 6
Rank:  4 (1099 entries) Julia
Rank:  5 (1079 entries) Python
Rank:  6 (1053 entries) Kotlin
Rank:  7 (1038 entries) Racket
Rank:  8 (1037 entries) Perl
Rank:  9  (968 entries) C
Rank: 10  (960 entries) Zkl
Rank: 11  (945 entries) J
Rank: 12  (935 entries) REXX
Rank: 13  (930 entries) Tcl
Rank: 14  (910 entries) Java
Rank: 15  (904 entries) Ruby

```


Recent, occasionally (hourly-ish) updated output also available at:
http://www.timb.net/popular-languages.html.


## Red

proccesses only languages with more than 25 entries to keep the list short

```Red
Red []

data: read http://www.rosettacode.org/mw/index.php?title=Special:Categories&limit=5000
lb: make block! 500
;;data: read %data.html ;; for testing save html and use flat file
arr: split data newline

k: "Category:"
;; exclude list:
exrule: [thru ["programming"
                | "users"
                | "Implementations"
                | "Solutions by "
                | "Members"
                | "WikipediaSourced"
                | "Typing/Strong"
                | "Impl needed"
                ]
            to end
          ]

foreach line arr [
  unless find line k [continue]
  parse line [ thru k thru ">" copy lang to "<" to end ] ;; extract/parse language
  if 20 < length? lang [continue]
  if parse lang [exrule] [continue] ;; exclude invalid
  cnt: 0
  ;; parse number of entries
  parse line [thru "</a>" thru "(" copy cnt  to " member" (cnt: to-integer cnt ) to end]
  if cnt > 25 [  append  lb reduce [to-string lang cnt]   ] ;; only process lang with > 25 entries
]

lb:  sort/skip/compare lb 2 2   ;; sort series by entries

print reduce [ "Rank Entries Language" ]    ;; header

last: 0
rank: 0

lb: tail lb   ;; process the series backwards

until [
  lb: skip lb -2
  cnt: second lb
  if cnt <> last [
    rank: rank + 1
  ]
  print rejoin [ pad/left rank 4 "." pad/left cnt 5 " - " first lb  ]
  last: cnt
  head? lb  ;; until head reached
]

```

Output: (30.12.2017)

```txt

Rank Entries Language
   1.  961 - Racket
   2.  959 - Python
   3.  926 - Perl 6
   4.  918 - Tcl
   5.  900 - Kotlin
   6.  883 - J
   7.  875 - C
   8.  859 - Zkl
   9.  846 - Ruby
  10.  833 - D
  11.  828 - Go
  12.  812 - REXX
  13.  809 - Haskell
  14.  802 - Java
  15.  785 - Perl
  16.  768 - PicoLisp
  17.  733 - Sidef
  18.  723 - Mathematica
  19.  702 - Julia
  20.  683 - Phix
  21.  668 - C++
  22.  648 - Ada
  23.  647 - Common Lisp
  24.  637 - C sharp
  25.  621 - AutoHotkey

```


## REXX

(Native) REXX doesn't support web-page reading, so the mentioned   ''Rosetta Code categories''   and

''Rosetta Code Languages''   were downloaded to local files.


This program reads the   ''Languages''   file and uses the contents of that file for a validation of

the   ''categories''   file.   This essentially is a perfect filter for the   ''Rosetta Code categories''   file.

The   '''catHeap'''   variable in the REXX program is just a long string of all the records in the web-file of the

Rosetta Code categories, with a special character ('''sep''') that separates each language entry (name).

The mechanism is to use a (sparse) stemmed array which holds only the names of languages which

(for most REXXes) uses a hashing algorithm to locate the entry   (which is very fast).

Programming note:   (REXX doesn't handle Unicode characters)   some special cases that are translated:
:::*   '''╬£C++'''                                   translated into   '''µC++'''          [Greek micro]
:::*   '''╨£╨Ü-61/52'''                                                       translated into   '''MK-61/52'''   [Cyrillic   '''MK-61/52'''])
:::*   '''??-61/52'''                                          translated into   '''MK-61/52'''   [Cyrillic   '''MK-61/52'''])
:::*   '''D├⌐j├á Vu'''                                                   translated into   '''Déjà Vu'''
:::*   '''Cach├⌐'''                                            translated into   '''Caché'''
:::*   '''F┼ìrmul├ª'''                                                   translated into   '''Fôrmulæ'''
:::*   '''α«ëα«»α«┐α«░α»ì/Uyir'''                                                                                               translated into   '''Uyir'''
:::*   '''╨£iniZinc'''                                              translated into   '''MiniZinc'''
(The 3<sup>rd</sup> entry is most likely caused by the inability to render unsupported glyphs when it was first used to add that language.)

Note that this REXX example properly ranks tied languages.

This version now sorts the tied languages by language name (thanks to Walter Pachl's suggestion).

### REXX program


```rexx
/*REXX program reads two files  and  displays a  ranked list  of Rosetta Code languages.*/
sep= '█';  L.=0;  #.=0;  u.=0;  catHeap=;  old.= /*assign some REXX variable defaults.  */
term= 1                                          /*show some early messages to terminal.*/
parse arg catFID lanFID outFID .                 /*obtain optional arguments from the CL*/
if catFID==''  then catFID = "RC_POP.CAT"        /*Not specified?  Then use the default.*/
if lanFID==''  then lanFID = "RC_POP.LAN"        /* "      "         "   "   "     "    */
if outFID==''  then outFID = "RC_POP.OUT"        /* "      "         "   "   "     "    */
call tell center('timestamp: '  date()  time("Civil"),79,'═'), 2, 1   /*timestamp,title.*/
langs= 0;      call reader 'languages'           /*assign languages  ───►  L.ααα        */
               call reader 'categories'          /*append categories ───► catHeap       */
#= 0                                             /*the number of categories  (so far).  */
     do j=1  until  catHeap==''                  /*process the heap of categories.      */
     parse var catHeap cat.j  (sep)  catHeap     /*get a category from the  catHeap.    */
     parse var cat.j   cat.j  '<'    "("  mems . /*untangle the strange─looking string. */
     cat.j= space(cat.j);    ?= cat.j;   upper ? /*remove any superfluous blanks.       */
     if ?=='' | \L.?          then iterate       /*it's blank  or  it's not a language. */
     if pos(',', mems)\==0    then mems= space( translate(mems,,","), 0)  /*elide commas*/
     if \datatype(mems, 'W')  then iterate       /*is the "members" number not numeric? */
     #.0= #.0 + mems                             /*bump the number of  members  found.  */
     if u.?\==0  then do;                 do f=1  for #  until ?==@u.f;    end  /*f*/
                      #.f= #.f + mems; iterate j /*languages that're in different cases.*/
                      end                        /* [↑]  handle any possible duplicates.*/
     u.?= u.? + 1;          #= # + 1             /*bump a couple of counters.           */
     #.#= #.# + mems;     @.#= cat.j;    @u.#= ? /*bump the counter;  assign it (upper).*/
     end   /*j*/
!.=                                              /*array holds indication of TIED langs.*/
call tell right(commas(#),    9) '(total) number of languages detected in the category file'
call tell right(commas(langs),9) '   "       "    "     "         "     "  "  language   "'
call tell right(commas(#.0),  9) '(total) number of entries (solutions) detected', , 1
term= 0                                          /*don't show any more messages to term.*/
call eSort #,0                                   /*sort the languages along with number.*/
tied=                                            /*add true rank (tR) ───► the entries. */
r=0;           do j=#  by -1  for #;     r= r+1;   tR= r;   !tR.j= r;   jp= j+1;   jm= j-1
               if tied==''  then pR= r;  tied=   /*handle when language rank is untied. */
               if #.j==#.jp | #.j==#.jm  then do;    !.j= '[tied]';     tied= !.j;     end
               if #.j==#.jp              then do;    tR= pR;            !tR.j= pR;     end
                                         else pR= r
               end   /*j*/
call eSort #,1                                   /*sort the languages along with entries*/
w= length( commas(#) )                           /* [↓]  show by ascending rank of lang.*/
rank= 0
          do t=#  by -1  for #;   rank= rank + 1 /*bump rank of a programming language. */
          call tell   right('rank:'    right(commas(!tR.t), w),  20-1)      right(!.t, 7),
                      right('('commas(#.t)  left("entr"s(#.t, 'ies', "y")')', 9), 20)  @.t
          end   /*#*/                        /* [↑]   S(···)   pluralizes a word.   */

call tell left('', 27)        '☼  end─of─list.  ☼', 1, 2             /*the bottom title.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas: parse arg _;   do jc=length(_)-3  to 1  by -3; _= insert(",",_,jc); end;  return _
s:      if arg(1)==1  then return arg(3);      return word(arg(2) 's',1)   /*pluralizer.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
eSort: procedure expose #. @. !tr.;    arg N,p2;     h= N      /*sort: number of members*/
            do     while  h>1;         h= h % 2                /*halve number of records*/
              do i=1  for N-h;         j= i;         k= h + i  /*sort this part of list.*/
              if p2  then do  while !tR.k==!tR.j  &  @.k>@.j   /*this uses a hard swap ↓*/
                          @= @.j;  #= !tR.j;  @.j= @.k;  !tR.j= !tR.k;   @.k= @;  !tR.k= #
                          if h>=j  then leave;               j= j - h;     k= k - h
                          end   /*while !tR.k==···*/
                     else do  while #.k<#.j                    /*this uses a hard swap ↓*/
                          @= @.j;  #= #.j;    @.j= @.k;    #.j= #.k;     @.k= @;    #.k= #
                          if h>=j  then leave;               j= j - h;     k= k - h
                          end   /*while #.k<#.j*/
              end               /*i*/           /*hard swaps needed for embedded blanks.*/
            end                 /*while h>1*/;            return
/*──────────────────────────────────────────────────────────────────────────────────────*/
reader: arg which 2;        igAst= 1             /*ARG uppers WHICH, obtain the 1st char*/
        if which=='L'  then inFID= lanFID        /*use this fileID for the  languages.  */
        if which=='C'  then inFID= catFID        /* "    "     "    "   "   categories. */
        Formulae = 'F┼ìrmul├ª'                   /*Unicode (in text)  name for  Fôrmulæ */
            Uyir = 'α«ëα«»α«┐α«░α»ì/Uyir'        /*Unicode (in text)  name for  Uyir    */
        old.1= '╬£C++'     ;  new.1= "µC++"      /*Unicode ╬£C++  ───► ASCII─8: µC++    */
        old.2= 'UC++'      ;  new.2= "µC++"      /*old      UC++  ───► ASCII─8: µC++    */
        old.3= '╨£╨Ü-'     ;  new.3= "MK-"       /*Unicode ╨£╨Ü─  ───► ASCII-8: MK-     */
        old.4= 'D├⌐j├á'    ;  new.4= "Déjà"      /*Unicode ├⌐j├á  ───► ASCII─8: Déjà    */
        old.5= 'Cach├⌐'    ;  new.5= "Caché"     /*Unicode Cach├⌐ ───► ASCII─8: Caché   */
        old.6= '??-61/52'  ;  new.6= "MK-61/52"  /*somewhere past, a mis─translated: MK-*/
        old.7=  Formulae   ;  new.7= 'Fôrmulæ'   /*Unicode        ───► ASCII─8: Fôrmulæ */
        old.8= '╨£iniZinc' ;  new.8= 'MiniZinc'  /*Unicode        ───► ASCII─8: MiniZinc*/
        old.9=  Uyir       ;  new.9= 'Uyir'      /*Unicode        ───► ASCII─8: Uyir    */

          do recs=0   while  lines(inFID) \== 0  /*read a file, a single line at a time.*/
          $= translate( linein(inFID), , '9'x)   /*handle any stray  TAB  ('09'x) chars.*/
          $$= space($);  if $$==''  then iterate /*ignore all blank lines in the file(s)*/
                  do v=1  while old.v \== ''     /*translate Unicode variations of langs*/
                  if pos(old.v, $$) \==0  then $$= changestr(old.v, $$, new.v)
                  end   /*v*/                    /* [↑]  handle different lang spellings*/
          if igAst  then do;  igAst= pos(' * ',$)==0;   if igAst  then iterate;     end
          if pos('RETRIEVED FROM', translate($$))\==0  then leave /*pseudo End─Of─Data?.*/
          if which=='L'  then do
                              if left($$, 1)\=='*'  then iterate  /*lang not legitimate?*/
                              parse upper var   $$   '*'  $$  "<";          $$= space($$)
                              if $$==''  then iterate /*recs*/;           L.$$= 1
                              langs= langs+1     /*bump the number of languages found.  */
                              iterate            /*iterates the      DO  recs     loop. */
                              end                /* [↓]  pick off the language name.    */
          if left($$, 1)=='*'  then $$= sep  ||  space( substr($$, 2) )
          catHeap= catHeap  $$                   /*append to the catHeap (CATegory) heap*/
          end   /*recs*/

        call  tell   right( commas(recs), 9)       'records read from file: '        inFID
        return
/*──────────────────────────────────────────────────────────────────────────────────────*/
tell:             do '0'arg(2);   call lineout outFID," "     ; if term then say ;     end
                                  call lineout outFID,arg(1)  ; if term then say arg(1)
                  do '0'arg(3);   call lineout outFID," "     ; if term then say ;     end
        return      /*show BEFORE blank lines (if any), message, show AFTER blank lines.*/
```



### all ranked 727 languages

The '''output'''   for this REXX (RC_POP.REX) program is included here   ──►   [[RC_POP.OUT]].


[See the talk page about some programming languages using different cases (lower/upper/mixed) for the language names,   as well as those that use Unicode characters in the name.]





## Ring


```ring

# Project: Rosetta Code/Rank languages by popularity

load "stdlib.ring"
ros= download("http://rosettacode.org/wiki/Category:Programming_Languages")
pos = 1
totalros = 0
rosname = ""
rosnameold = ""
rostitle = ""
roslist = []
for n = 1 to len(ros)
        nr = searchstring(ros,'<li><a href="/wiki/',pos)
        if nr = 0
           exit
        else
           pos = nr + 1
        ok
        nr = searchname(nr)
        nr = searchtitle(nr)
next
roslist = sortfirst(roslist)
roslist = reverse(roslist)

see nl
for n = 1 to len(roslist)
     see "rank: " + n + " (" + roslist[n][1] + " entries) " + roslist[n][2] + nl
next

func searchstring(str,substr,n)
       newstr=right(str,len(str)-n+1)
       nr = substr(newstr, substr)
       if nr = 0
          return 0
       else
          return n + nr -1
       ok

func count(cstring,dstring)
       sum = 0
       while substr(cstring,dstring) > 0
               sum = sum + 1
               cstring = substr(cstring,substr(cstring,dstring)+len(string(sum)))
       end
       return sum

func searchname(sn)
       nr2 = searchstring(ros,"/wiki/Category:",sn)
       nr3 = searchstring(ros,"title=",sn)
       nr4 = searchstring(ros,'">',sn)
       nr5 = searchstring(ros,"</a></li>",sn)
       rosname = substr(ros,nr2+15,nr3-nr2-17)
       rosnameold = substr(ros,nr4+2,nr5-nr4-2)
       return sn

func searchtitle(sn)
        rostitle = "rosettacode.org/wiki/Category:" + rosname
        rostitle = download(rostitle)
        nr2 = 0
        roscount = count(rostitle,"The following")
        if roscount > 0
           rp = 1
           for rc = 1 to roscount
                nr2 = searchstring(rostitle,"The following",rp)
                rp = nr2 + 1
           next
        ok
        nr3 = searchstring(rostitle,"pages are in this category",nr2)
        if nr2 > 0 and nr3 > 0
           rosnr = substr(rostitle,nr2+14,nr3-nr2-15)
           rosnr = substr(rosnr,",","")
           add(roslist,[rosnr,rosnameold])
        ok
        return sn

func sortfirst(alist)
        for n = 1 to len(alist) - 1
             for m = n + 1 to len(alist)
                  if alist[m][1] < alist[n][1]
                     swap(alist,m,n)
                  ok
                  if alist[m][1] = alist[n][1] and strcmp(alist[m][2],alist[n][2]) > 0
                     swap(alist,m,n)
                  ok
             next
        next
        return alist

```

Output:

```txt

rank: 1 (1010 entries) Kotlin
rank: 2 (977 entries) Racket
rank: 3 (968 entries) Python
rank: 4 (962 entries) Perl 6
rank: 5 (931 entries) Go
rank: 6 (914 entries) Tcl
rank: 7 (905 entries) C
rank: 8 (888 entries) J
rank: 9 (882 entries) Zkl
rank: 10 (875 entries) Java
......
rank: 30 (580 entries) Ring
......
rank: 523 (2 entries) TechBASIC
rank: 524 (2 entries) Thyrd
rank: 525 (2 entries) ToffeeScript
rank: 526 (2 entries) Viua VM assembly
rank: 527 (2 entries) XL
rank: 528 (2 entries) XProc
rank: 529 (2 entries) XSLT 1.0
rank: 530 (2 entries) XTalk
rank: 531 (2 entries) XUL
rank: 532 (2 entries) ZPL

```



## Ruby


### By using the API

{{works with|Ruby|1.8.7}}
Now that there are more than 500 categories, the URL given in the task description is insufficient. I use the RC API to grab the categories, and then count the members of each category.

Uses the <code>RosettaCode</code> module from [[Count programming examples#Ruby]]

```ruby
require 'rosettacode'

langs = []
RosettaCode.category_members("Programming Languages") {|lang| langs << lang}

# API has trouble with long titles= values.
# To prevent skipping languages, use short slices of 20 titles.
langcount = {}
langs.each_slice(20) do |sublist|
  url = RosettaCode.get_api_url({
    "action" => "query",
    "prop" => "categoryinfo",
    "format" => "xml",
    "titles" => sublist.join("|"),
  })

  doc = REXML::Document.new open(url)
  REXML::XPath.each(doc, "//page") do |page|
    lang = page.attribute("title").value
    info = REXML::XPath.first(page, "categoryinfo")
    langcount[lang] = info.nil? ? 0 : info.attribute("pages").value.to_i
  end
end

puts Time.now
puts "There are #{langcount.length} languages"
puts "the top 25:"
langcount.sort_by {|key,val| val}.reverse[0,25].each_with_index do |(lang, count), i|
  puts "#{i+1}. #{count} - #{lang.sub(/Category:/, '')}"
end
```

{{out|Results}}
<pre style="height: 40ex; overflow: scroll">2010-07-08 14:52:46 -0500
There are 306 languages
the top 25:
1. 399 - Tcl
2. 370 - Python
3. 352 - Ruby
4. 338 - J
5. 337 - C
6. 333 - PicoLisp
7. 322 - OCaml
8. 322 - Haskell
9. 299 - Perl
10. 299 - AutoHotkey
11. 288 - Common Lisp
12. 280 - Java
13. 275 - Ada
14. 270 - D
15. 267 - Oz
16. 253 - R
17. 252 - PureBasic
18. 245 - E
19. 243 - C++
20. 241 - C sharp
21. 239 - ALGOL 68
22. 236 - JavaScript
23. 221 - Forth
24. 207 - Clojure
25. 201 - Fortran
```



## Run BASIC


```runbasic
sqliteconnect #mem, ":memory:"  ' make memory DB
#mem execute("CREATE TABLE stats(lang,cnt)")
a$	= httpGet$("http://rosettacode.org/wiki/Category:Programming_Languages")
aa$	= httpGet$("http://www.rosettacode.org/mw/index.php?title=Special:Categories&limit=5000")
i	= instr(a$,"/wiki/Category:")
while i > 0 and lang$ <> "Languages"
	j	= instr(a$,"""",i)
	lang$	= mid$(a$,i+15,j - i-15)
	ii	= instr(aa$,"Category:";lang$;"""")
	jj	= instr(aa$,"(",ii)
	kk	= instr(aa$," ",jj+1)
	if ii = 0 then cnt = 0 else cnt = val(mid$(aa$,jj+1,kk-jj))
	k	= instr(lang$,"%")     ' convert hex values to characters
	while k > 0
		lang$	= left$(lang$,k-1) + chr$(hexdec(mid$(lang$,k+1,2))) + mid$(lang$,k+3)
		k	= instr(lang$,"%")
	wend
	#mem execute("insert into stats values ('";lang$;"',";cnt;")")
	i	= instr(a$,"/wiki/Category:",i+10)
wend
html "<table border=2>"
#mem execute("SELECT * FROM stats ORDER BY cnt desc") ' order list by count descending
WHILE   #mem hasanswer()
 #row = #mem #nextrow()
 rank = rank + 1
 html "<TR><TD align=right>";rank;"</td><td>";#row lang$();"</td><td align=right>";#row cnt();"</td></tr>"
WEND
html "</table>"
```

<table border=2>
<TR><TD align=right>1</td><td>Tcl</td><td align=right>687</td></tr>
<TR><TD align=right>2</td><td>Python</td><td align=right>650</td></tr>
<TR><TD align=right>3</td><td>C</td><td align=right>638</td></tr>
<TR><TD align=right>4</td><td>PicoLisp</td><td align=right>626</td></tr>
<TR><TD align=right>5</td><td>J</td><td align=right>619</td></tr>
<TR><TD align=right>6</td><td>Go</td><td align=right>587</td></tr>
<TR><TD align=right>7</td><td>Ruby</td><td align=right>581</td></tr>
<TR><TD align=right>8</td><td>D</td><td align=right>571</td></tr>
<TR><TD align=right>9</td><td>Ada</td><td align=right>559</td></tr>
<TR><TD align=right>10</td><td>Mathematica</td><td align=right>551</td></tr>
<TR><TD align=right>11</td><td>Perl</td><td align=right>528</td></tr>
<TR><TD align=right>12</td><td>Perl_6</td><td align=right>528</td></tr>
<TR><TD align=right>13</td><td>Haskell</td><td align=right>526</td></tr>
<TR><TD align=right>14</td><td>BBC_BASIC</td><td align=right>513</td></tr>
<TR><TD align=right>15</td><td>REXX</td><td align=right>491</td></tr>
<TR><TD align=right>16</td><td>Java</td><td align=right>488</td></tr>
<TR><TD align=right>17</td><td>OCaml</td><td align=right>477</td></tr>
<TR><TD align=right>18</td><td>PureBasic</td><td align=right>469</td></tr>
<TR><TD align=right>19</td><td>Unicon</td><td align=right>462</td></tr>
<TR><TD align=right>20</td><td>AutoHotkey</td><td align=right>430</td></tr>
<TR><TD align=right>21</td><td>Icon</td><td align=right>429</td></tr>
<TR><TD align=right>22</td><td>Common_Lisp</td><td align=right>424</td></tr>
<TR><TD align=right>23</td><td>C_sharp</td><td align=right>416</td></tr>
<TR><TD align=right>24</td><td>C++</td><td align=right>400</td></tr>
<TR><TD align=right>25</td><td>JavaScript</td><td align=right>359</td></tr>
<TR><TD align=right>26</td><td>Scala</td><td align=right>339</td></tr>
<TR><TD align=right>27</td><td>PARI/GP</td><td align=right>338</td></tr>
<TR><TD align=right>28</td><td>Clojure</td><td align=right>335</td></tr>
<TR><TD align=right>29</td><td>R</td><td align=right>322</td></tr>
<TR><TD align=right>n</td><td>...</td><td align=right>...</td></tr></table>


## Scala


### Parallel internet querying

Entry point @ object <code>GhettoParserPar</code>

```scala
import akka.actor.{Actor, ActorSystem, Props}
import scala.collection.immutable.TreeSet
import scala.xml.XML

// Reports a list with all languages recorded in the Wiki

private object Acquisition {
  val (endPoint, prefix) = ("http://rosettacode.org/mw/api.php", "Category:")
  val (maxPlaces, correction) = (50, 2)

  def convertPathArgsToURL(endPoint: String, pathArgs: Map[String, String]) = {
    pathArgs.map(argPair => argPair._1 + "=" + argPair._2)
      .mkString(endPoint + (if (pathArgs.nonEmpty) "?" else ""), "&", "")
  }

  /* The categories include a page for the language and a count of the pages
   * linked therein, this count is the data we need to scrape.
   * Reports a list with language, count pair recorded in the Wiki
   * All strings starts with the prefixes "Category:"
   */
  def mineCatos = {
    val endPoint = "http://rosettacode.org/mw/index.php"
    Concurrent.logInfo("Acquisition of categories started.")
    val categories =
      (XML.load(convertPathArgsToURL(endPoint,
        Map("title" -> "Special:Categories", "limit" -> "5000"))) \\ "ul" \ "li")
        .withFilter(p => (p \ "a" \ "@title").text.startsWith(prefix))
        .map // Create a tuple pair, eg. ("Category:Erlang", 195)
        { cat =>
          ((cat \ "a" \ "@title").text, // Takes the sibling of "a" and extracts the number
            "[0-9]+".r.findFirstIn(cat.child.drop(1).text).getOrElse("0").toInt)
        }
    Concurrent.logInfo(s"Got ${categories.size} categories..")
    categories
  }

  // The languages
  // All strings starts with the prefixes "Category:"
  def mineLangs = {
    Concurrent.logInfo("Acquisition of languages started...")
    def getLangs(first: Boolean = true, continue: String = ""): TreeSet[String] = (first, continue) match {
      case (false, "") => TreeSet[String]()
      case _ => {
        val xml = XML.load(convertPathArgsToURL(endPoint, Map(
            "action" -> "query",
            "list" -> "categorymembers",
            "cmtitle" -> (prefix + "Programming_Languages"),
            "cmlimit" -> "500",
            "rawcontinue" -> "",
            "format" -> "xml",
            "cmcontinue" -> continue)))
        getLangs(false, (xml \\ "query-continue" \ "categorymembers" \ "@cmcontinue").text) ++ (xml \\ "categorymembers" \ "cm").map(c => (c \ "@title").text)
      }
    }
    val languages = getLangs()
    Concurrent.logInfo(s"Got ${languages.size} languages..")
    languages
  }

  def joinRosettaCodeWithLanguage(catos: Seq[(String, Int)],
                                  langs: TreeSet[String]) =
    for {
      cato <- catos //Clean up the tuple pairs, eg ("Category:Erlang", 195) becomes ("Erlang", 192)
      if langs.contains(cato._1)
    } yield (cato._1.drop(prefix.length), cato._2 - correction max 0) // Correct count

  def printScrape(languages: TreeSet[String], category: Seq[(String, Int)]) {

    val join = joinRosettaCodeWithLanguage(category, languages)
    val total = join.foldLeft(0)(_ + _._2)

    Concurrent.logInfo("Data processed")

    println(f"\nTop$maxPlaces%3d Rosetta Code Languages by Popularity as ${new java.util.Date}%tF:\n")
    (join.groupBy(_._2).toSeq.sortBy(-_._1).take(maxPlaces) :+ (0, Seq(("...", 0))))
      .zipWithIndex // Group the ex aequo
      .foreach {
        case ((score, langs), rank) =>
          println(f"${rank + 1}%2d. $score%3d - ${langs.map(_._1).mkString(", ")}")
      }

    println(s"\nCross section yields ${join.size} languages, total of $total solutions")
    println(s"Resulting average is ${total / join.size} solutions per language")
  }

  def printScrape(): Unit = printScrape(mineLangs, mineCatos)
} // object Acquisition

private object Concurrent extends AppCommons {
  var (category: Option[Seq[(String, Int)]], language: Option[TreeSet[String]]) = (None, None)

  class Worker extends Actor {
    def receive = {
      case 'Catalogue => sender ! Acquisition.mineCatos
      case 'Language => sender ! Acquisition.mineLangs
    }
  }

  class Listener extends Actor {
    // Create and signal the worker actors
    context.actorOf(Props[Worker], "worker0") ! 'Catalogue
    context.actorOf(Props[Worker], "worker1") ! 'Language

    def printCompleteScape() =
      if (category.isDefined && language.isDefined) {
        Acquisition.printScrape(language.get, category.get)
        context.system.shutdown()
        appEnd()
      }

    def receive = {
      case content: TreeSet[String] =>
        language = Some(content)
        printCompleteScape()
      case content: Seq[(String, Int)] =>
        category = Some(content)
        printCompleteScape()
      case whatever => logInfo(whatever.toString)
    } // def receive
  }
} // object Concurrent

trait AppCommons {
  val execStart = System.currentTimeMillis()
  System.setProperty("http.agent", "*")

  def logInfo(info: String) {
    println(f"[Info][${System.currentTimeMillis() - execStart}%5d ms]" + info)
  }

  def appEnd() { logInfo("Run succesfully completed") }
}

// Main entry for sequential version (slower)
object GhettoParserSeq extends App with AppCommons {
  Concurrent.logInfo("Sequential version started")
  Acquisition.printScrape()
  appEnd()
}

// Entry for parallel version (faster)
object GhettoParserPar extends App {
  Concurrent.logInfo("Parallel version started")
  ActorSystem("Main").actorOf(Props[Concurrent.Listener])
}
```



### Sequential internet querying

The same code above but as entry point object <code>GhettoParserSeq</code>
{{Out|Output for both solutions}} but parallel run chosen. Notice the synchronous start time.

```txt
[Info][    0 ms]Sequential version started
[Info][    3 ms]Acquisition of languages started...
[Info][ 1458 ms]Got 642 languages..
[Info][ 1458 ms]Acquisition of categories started.
[Info][19385 ms]Got 2647 categories..
[Info][19389 ms]Data processed

Top 50 Rosetta Code Languages by Popularity as 2016-11-11:

 1. 907 - Racket
 2. 896 - Python, Tcl
 3. 859 - J
 4. 846 - Perl 6
 5. 810 - Ruby
 6. 807 - Zkl
 7. 795 - C
 8. 774 - Java
 9. 773 - Go
10. 754 - Haskell
11. 753 - Perl
12. 751 - REXX
13. 750 - D
14. 729 - PicoLisp
15. 689 - Mathematica
16. 674 - Sidef
17. 634 - C++
18. 631 - Ada
19. 609 - AutoHotkey
20. 592 - Common Lisp
21. 583 - Unicon
22. 569 - Scala
23. 560 - C sharp
24. 546 - BBC BASIC
25. 526 - Icon
26. 522 - Clojure
27. 520 - JavaScript
28. 519 - OCaml
29. 517 - PureBasic
30. 513 - PARI/GP
31. 510 - Lua
32. 509 - Nim
33. 497 - ALGOL 68
34. 491 - Elixir
35. 488 - Fortran
36. 474 - Erlang
37. 430 - PowerShell
38. 428 - Julia
39. 414 - Jq
40. 413 - F Sharp
41. 409 - Phix
42. 407 - Pascal
43. 405 - Forth, Seed7
44. 398 - PL/I
45. 383 - R
46. 382 - PHP
47. 377 - Groovy
48. 370 - AWK
49. 340 - MATLAB
50. 333 - Liberty BASIC
51.   0 - ...

Cross section yields 619 languages, total of 50835 solutions
Resulting average is 82 solutions per language
[Info][19413 ms]Run succesfully completed
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "gethttp.s7i";
  include "scanstri.s7i";

const type: popularityHash is hash [string] integer;
const type: rankingHash is hash [integer] array string;

const func array string: getLangs (in string: buffer) is func
  result
    var array string: langs is 0 times "";
  local
    var integer: pos is 0;
  begin
    pos := pos(buffer, "Category:");
    while pos <> 0 do
      pos +:= 9;
      langs &:= buffer[pos .. pred(pos(buffer, '"', pos))];
      pos := pos(buffer, "Category:", pos);
    end while;
  end func;

const proc: main is func
  local
    var string: categories is "";
    var popularityHash: popularity is popularityHash.value;
    var rankingHash: ranking is rankingHash.value;
    var array integer: numList is 0 times 0;
    var string: lang is "";
    var integer: pos is 0;
    var string: numStri is "";
    var integer: listIdx is 0;
    var integer: index is 0;
    var integer: place is 1;
  begin
    categories := getHttp("www.rosettacode.org/w/index.php?title=Special:Categories&limit=5000");
    for lang range getLangs(getHttp("rosettacode.org/mw/api.php?action=query&list=categorymembers&\
                                    \cmtitle=Category:Programming_Languages&cmlimit=500&format=json")) do
      pos := pos(categories, "title=\"Category:" & lang);
      if pos <> 0 then
        pos := pos(categories, "</a>", succ(pos));
        if pos <> 0 then
          pos := pos(categories, "(", succ(pos));
          if pos <> 0 then
            numStri := categories[succ(pos) len 10];
            popularity @:= [lang] integer parse getDigits(numStri);
          end if;
        end if;
      end if;
    end for;
    ranking := flip(popularity);
    numList := sort(keys(ranking));
    for listIdx range maxIdx(numList) downto minIdx(numList) do
      for key index range ranking[numList[listIdx]] do
        writeln(place lpad 3 <& ". " <& numList[listIdx] <& " - " <& ranking[numList[listIdx]][index]);
      end for;
      place +:= length(ranking[numList[listIdx]]);
    end for;
  end func;
```


List of languages as of 2014-01-15:

```txt

  1. 772 - Tcl
  2. 733 - Racket
  3. 718 - Python
  4. 685 - C
  5. 657 - Perl 6
  6. 654 - J
  7. 645 - D
  8. 643 - Ruby
  9. 636 - PicoLisp
 10. 624 - Go
 11. 610 - Perl
 12. 579 - REXX
 13. 578 - Ada
 14. 567 - Mathematica
 14. 567 - Haskell
 16. 532 - Unicon
 17. 526 - Java
 18. 523 - BBC BASIC
 19. 489 - OCaml
 20. 487 - Icon
 21. 477 - C++
 22. 470 - PureBasic
 23. 448 - Common Lisp
 24. 439 - Erlang
 24. 439 - C sharp
 26. 437 - AutoHotkey
 27. 406 - Scala
 28. 382 - JavaScript
 29. 375 - Clojure
 30. 356 - Seed7
...

```



## Sidef

{{trans|Perl}}

```ruby
require('MediaWiki::API')

var api = %O<MediaWiki::API>.new(
    Hash(api_url => 'http://rosettacode.org/mw/api.php')
)

var languages = []
var gcmcontinue
loop {
    var apih = api.api(
        Hash(
            action      => 'query',
            generator   => 'categorymembers',
            gcmtitle    => 'Category:Programming Languages',
            gcmlimit    => 250,
            prop        => 'categoryinfo',
            gcmcontinue => gcmcontinue,
        )
    )

    languages.append(apih{:query}{:pages}.values...)
    gcmcontinue = apih{:continue}{:gcmcontinue}
    gcmcontinue || break
}

languages.each { |lang|
    lang{:title} -= /^Category:/
    lang{:categoryinfo}{:size} := 0
}

var sorted_languages = languages.sort_by { |lang|
    -lang{:categoryinfo}{:size}
}

sorted_languages.each_kv { |i, lang|
    printf("%3d. %20s - %3d\n", i+1, lang{:title}, lang{:categoryinfo}{:size})
}
```

{{out}}

```txt

  1.               Racket - 938
  2.               Python - 930
  3.                  Tcl - 904
  4.               Perl 6 - 877
  5.                    J - 863
  6.                  Zkl - 834
  7.                 Ruby - 830
  8.                    C - 805
  9.                   Go - 793
 10.              Haskell - 785
 11.                 Java - 783
 12.                 REXX - 776
 13.                 Perl - 765
 14.                    D - 753
 15.             PicoLisp - 731
 16.          Mathematica - 702
 17.                Sidef - 696
...

```



## SNOBOL4

{{Works with|SNOBOL4|CSNOBOL4 CVS test}}

```SNOBOL4
-include "url.sno"
        http.recl = "K,32767"  ;* Read next 32767 characters
                               ;*   of very long lines.

        rclangs = "http://rosettacode.org/mw/api.php?"
+          "format=xml&action=query&generator=categorymembers&"
+          "gcmtitle=Category:Programming%20Languages&"
+          "gcmlimit=500&prop=categoryinfo"

        languagepat = arb "<page" arb 'title="Category:'
+          break('"') . lang arb 'pages="' break('"') . count

        langtable = table(500, 20)

        url.open(.fin, rclangs, http.recl)          :s(read)
        output = "Cannot open rosettacode site."    :(end)

read    line = line fin                             :f(done)
get     line languagepat =                          :f(read)
        langtable<lang> = langtable<lang> + count   :(get)

done    langarray = rsort(langtable,2)              :s(write)
        output = "No languages found."              :(end)

write   n = n + 1
        output = lpad(n ". ", 5) lpad(langarray<n, 2>, 4)
+          " - " langarray<n,1>                     :s(write)

        url.close(.fin)
end
```


'''Abridged output (top 30 languages as of August 25, 2015):'''


```txt
  1.  875 - Racket
  2.  837 - Python
  3.  799 - J
  4.  772 - Ruby
  5.  763 - Perl 6
  6.  756 - C
  7.  742 - Go
  8.  737 - D
  9.  707 - Perl
 10.  699 - REXX
 11.  689 - PicoLisp
 12.  683 - Haskell
 13.  672 - Mathematica
 14.  650 - Java
 15.  620 - Ada
 16.  588 - AutoHotkey
 17.  563 - C++
 18.  553 - Common Lisp
 19.  546 - Scala
 20.  529 - BBC BASIC
 21.  520 - Icon
 22.  513 - C sharp
 23.  505 - OCaml
 24.  501 - Nim
 25.  487 - PureBasic
 26.  485 - Clojure
 27.  455 - Epigram
 28.  443 - PARI/GP
 29.  431 - JavaScript
 30.  410 - Jq
```



## Stata


First we build the database:


```stata
copy "http://rosettacode.org/wiki/Category:Programming_Languages" lang.html, replace
import delimited lang.html, delim("@") enc("utf-8") clear
keep if ustrpos(v1,"/wiki/Category:")
gen i = ustrpos(v1,"title=")
gen j = ustrpos(v1,char(34),i+1)
gen k = ustrpos(v1,char(34),j+1)
gen s = usubstr(v1,j,k-j+1)
keep if usubstr(s,2,9)=="Category:"
gen lang=usubstr(s,11,ustrlen(s)-11)
keep lang
save lang, replace

copy "http://rosettacode.org/mw/index.php?title=Special:Categories&limit=5000" categ.html, replace
import delimited categ.html, delim("@") enc("utf-8") clear
keep if ustrpos(v1,"/wiki/Category:") & ustrpos(v1,"member")
gen i = ustrpos(v1,"title=")
gen j = ustrpos(v1,char(34),i+1)
gen k = ustrpos(v1,char(34),j+1)
gen s = usubstr(v1,j,k-j+1)
keep if usubstr(s,2,9)=="Category:"
gen lang=usubstr(s,11,ustrlen(s)-11)
drop i j k s
gen i = ustrrpos(v1,"(")
gen j = ustrrpos(v1,")")
gen s = usubstr(v1,i,j-i+1)
gen k = ustrpos(s," ")
gen t = usubstr(s,2,k-1)
destring t, gen(count)
drop v1 i j k s t
merge 1:1 lang using lang, keep(2 3) nogen
replace count=0 if missing(count)
gsort -count lang
gen rank=1
replace rank=rank[_n-1]+(count[_n]!=count[_n-1]) in 2/l
save tasks, replace
```


Now some results, as of 2017-12-03:


```stata
* Total number of entries
qui sum n
di r(sum)
57211

* Number of languages
count
671

* Number of languages with at least one entry
count if count
650

* First 10 languages
list in 1/10, noobs

  +-----------------------+
  |   lang   count   rank |
  |-----------------------|
  | Racket     961      1 |
  | Python     958      2 |
  | Perl 6     925      3 |
  |    Tcl     918      4 |
  |      J     883      5 |
  |-----------------------|
  |      C     874      6 |
  | Kotlin     868      7 |
  |    Zkl     857      8 |
  |   Ruby     845      9 |
  |     Go     828     10 |
  +-----------------------+
```



## Tcl


### By web scraping


```tcl
package require Tcl 8.5
package require http

set response [http::geturl http://rosettacode.org/mw/index.php?title=Special:Categories&limit=8000]

array set ignore {
    "Basic language learning"           1
    "Encyclopedia"                      1
    "Implementations"                   1
    "Language Implementations"          1
    "Language users"                    1
    "Maintenance/OmitCategoriesCreated" 1
    "Programming Languages"             1
    "Programming Tasks"                 1
    "RCTemplates"                       1
    "Solutions by Library"              1
    "Solutions by Programming Language" 1
    "Solutions by Programming Task"     1
    "Unimplemented tasks by language"   1
    "WikiStubs"                         1
    "Examples needing attention"	1
    "Impl needed"			1
}
# need substring filter
proc filterLang {n} {
    return [expr {[string first "User" $n] > 0}]
}
# (sorry the 1 double quote in the regexp kills highlighting)
foreach line [split [http::data $response] \n] {
    if {[regexp {title..Category:([^"]+).* \((\d+) members\)} $line -> lang num]} {
        if {![info exists ignore($lang)] && ![filterLang $lang]} {
            lappend langs [list $num $lang]
        }
    }
}

foreach entry [lsort -integer -index 0 -decreasing $langs] {
    lassign $entry num lang
    puts [format "%d. %d - %s" [incr i] $num $lang]
}
```

{{out|Output on 29 July 2015 (top 20 entries only)}}

```txt
1. 887 - Tcl
2. 877 - Racket
3. 853 - Python
4. 795 - J
5. 775 - Ruby
6. 766 - Perl 6
7. 757 - C
8. 746 - Go
9. 740 - D
10. 710 - Perl
11. 697 - REXX
12. 692 - PicoLisp
13. 682 - Haskell
14. 675 - Mathematica
15. 652 - Java
16. 634 - Zkl
17. 623 - Ada
18. 591 - AutoHotkey
19. 581 - Unicon
20. 562 - C++
……
```



### By using the API

{{trans|Ruby}}
{{works with|Tcl|8.5}}
{{libheader|tDOM}}


```tcl
package require Tcl 8.5
package require http
package require tdom

namespace eval rc {
    ### Utility function that handles the low-level querying ###
    proc rcq {q xp vn b} {
	upvar 1 $vn v
	dict set q action "query"
        # Loop to pick up all results out of a category query
	while 1 {
	    set url "http://rosettacode.org/mw/api.php?[http::formatQuery {*}$q]"
	    puts -nonewline stderr .		;# Indicate query progress...
	    set token [http::geturl $url]
	    set doc [dom parse [http::data $token]]
	    http::cleanup $token

	    # Spoon out the DOM nodes that the caller wanted
	    foreach v [$doc selectNodes $xp] {
		uplevel 1 $b
	    }

	    # See if we want to go round the loop again
	    set next [$doc selectNodes "//query-continue/categorymembers"]
	    if {![llength $next]} break
	    dict set q cmcontinue [[lindex $next 0] getAttribute "cmcontinue"]
	}
    }

    ### API function: Iterate over the members of a category ###
    proc members {page varName script} {
	upvar 1 $varName var
	set query [dict create cmtitle "Category:$page" {*}{
	    list "categorymembers"
	    format "xml"
	    cmlimit "500"
	}]
	rcq $query "//cm" item {
	    # Tell the caller's script about the item
	    set var [$item getAttribute "title"]
	    uplevel 1 $script
	}
    }

    ### API function: Count the members of a list of categories ###
    proc count {cats catVar countVar script} {
	upvar 1 $catVar cat $countVar count
	set query [dict create prop "categoryinfo" format "xml"]
	for {set n 0} {$n<[llength $cats]} {incr n 20} {    ;# limit fetch to 20 at a time
	    dict set query titles [join [lrange $cats $n $n+19] |]
	    rcq $query "//page" item {
		# Get title and count
		set cat [$item getAttribute "title"]
		set info [$item getElementsByTagName "categoryinfo"]
		if {[llength $info]} {
		    set count [[lindex $info 0] getAttribute "pages"]
		} else {
		    set count 0
		}
		# Let the caller's script figure out what to do with them
		uplevel 1 $script
	    }
	}
    }

    ### Assemble the bits into a whole API ###
    namespace export members count
    namespace ensemble create
}

# Get the list of programming languages
rc members "Programming Languages" lang {
    lappend langs $lang
}
puts stderr ""			;# Because of the progress dots...
puts "There are [llength $langs] languages"

# Get the count of solutions for each, stripping "Category:" prefix
rc count $langs l c {
    dict set langcounts [regsub {^Category:} $l {}] $c
}
puts stderr ""			;# Because of the progress dots...

# Print the output
puts "Here are the top fifteen:"
set langcounts [lsort -stride 2 -index 1 -integer -decreasing $langcounts]
set i 0
foreach {lang count} $langcounts {
    puts [format "%1\$3d. %3\$3d - %2\$s" [incr n] $lang $count]
    if {[incr i]>=15} break
}

# --- generate a full report, similar in format to REXX example
proc popreport {langcounts} {
    set bycount {}
    foreach {lang count} $langcounts {
	dict lappend bycount $count $lang
    }
    set bycount [lsort -stride 2 -integer -decreasing $bycount]
    set rank 1
    foreach {count langs} $bycount {
	set tied [expr {[llength $langs] > 1 ? "\[tied\]" : ""}]
	foreach lang $langs {
	    puts [format {%15s:%4d  %-12s %12s  %s} rank $rank $tied "($count entries)" $lang]
	}
	incr rank [llength $langs]
    }
}

popreport $langcounts
```


{{out}}

```txt
..
There are 582 languages
..............................
Here are the top fifteen:
  1. 882 - Tcl
  2. 874 - Racket
  3. 837 - Python
  4. 790 - J
  5. 772 - Ruby
  6. 762 - Perl 6
  7. 754 - C
  8. 739 - Go
  9. 736 - D
 10. 706 - Perl
 11. 694 - REXX
 12. 689 - PicoLisp
 13. 679 - Haskell
 14. 672 - Mathematica
 15. 649 - Java
```


Full output is included at [[Rosetta_Code/Rank_languages_by_popularity/Tcl_API_full_output]].


## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
remotedata = REQUEST ("http://www.rosettacode.org/mw/index.php?title=Special:Categories&limit=5000")
allmembers=allnames=""
COMPILE
LOOP d=remotedata
IF (d.sw."<li>") THEN
 name=EXTRACT (d,":<<a<><%>>:"|,":<</a>>:")
 IF (name.eq."Language users") CYCLE
 IF (name.sw."Unimplemented tasks") CYCLE
 IF (name.sw."Programming") CYCLE
 IF (name.sw."Solutions") CYCLE
 IF (name.sw."Garbage") CYCLE
 IF (name.sw."Typing") CYCLE
 IF (name.sw."BASIC LANG") CYCLE
 IF (name.ew."USER") CYCLE
 IF (name.ew."tasks") CYCLE
 IF (name.ew."attention") CYCLE
 IF (name.ew."related") CYCLE
 IF (name.ct."*omit*") CYCLE
 IF (name.ct.":*Categor*:") CYCLE
 IF (name.ct.":WikiSTUBS:") CYCLE
 IF (name.ct.":Impl needed:") CYCLE
 IF (name.ct.":Implementations:") CYCLE
 IF (name.ct.":':") name = EXCHANGE (name,":'::")
 members = STRINGS (d,":><1<>>/><<> member:")
 IF (members!="") THEN
  allmembers=APPEND (allmembers,members)
  allnames  =APPEND (allnames,name)
 ENDIF
ENDIF
ENDLOOP
index      = DIGIT_INDEX (allmembers)
index      = REVERSE (index)
allmembers = INDEX_SORT  (allmembers,index)
allnames   = INDEX_SORT  (allnames,  index)
ERROR/STOP CREATE ("list",SEQ-E,-std-)
time=time(),balt=nalt=""
FILE "list" = time
LOOP n, a=allnames,b=allmembers
 IF (b==balt) THEN
   nr=nalt
 ELSE
   nalt=nr=n
 ENDIF
 content=concat (nr,". ",a," --- ",b)
 FILE "list" = CONTENT
 balt=b
ENDLOOP
ENDCOMPILE
```

{{out}}
<pre style='height:30ex;overflow:scroll'>
2011-01-24 14:05:27
1. Tcl --- 472 member
2. PicoLisp --- 441 member
3. Python --- 432 member
4. J --- 414 member
5. C --- 394 member
6. Ruby --- 385 member
7. PureBasic --- 371 member
8. Haskell --- 369 member
9. Ada --- 364 member
10. OCaml --- 352 member
11. Perl --- 339 member
11. D --- 339 member
13. Java --- 325 member
14. AutoHotkey --- 308 member
15. Common Lisp --- 305 membe
16. C sharp --- 301 member
17. C++ --- 285 member
18. Oz --- 278 member
19. Clojure --- 272 member
20. R --- 266 member
21. ALGOL 68 --- 262 member
22. Perl 6 --- 258 member
23. JavaScript --- 257 member
24. E --- 254 member
25. REXX --- 253 member
26. Fortran --- 251 member
27. Forth --- 248 member
28. Lua --- 238 member
29. PHP --- 215 member
30. Unicon --- 211 member
31. Icon --- 210 member
32. Factor --- 207 member
33. Scala --- 197 member
34. PL/I --- 193 member
34. Go --- 193 member
36. Scheme --- 186 member

```



## UnixPipes


{{libheader|curl}}

```bash
curl 'http://rosettacode.org/mw/index.php?title=Special:Categories&limit=5000' |
sed -nre 's/^<li.*title="Category:([^"(]+)".*\(([0-9]+) members\).*/\2 - \1/p' |
sort -nr | awk '{printf "%2d. %s\n",NR,$0}'
```



## VBScript

Uses the API. Instead of displaying it on the command prompt, the records of the languages are saved on a text file "OutVBRC.txt" encoded with Unicode.

```vb
    '''''''''''''''''''''''''''''''''''''''''''''
    ' Rosetta Code/Rank Languages by Popularity '
    '          VBScript Implementation          '
    '...........................................'

'API Links (From C Code)
URL1 = "http://www.rosettacode.org/mw/api.php?format=json&action=query&generator=categorymembers&gcmtitle=Category:Programming%20Languages&gcmlimit=500&prop=categoryinfo&rawcontinue"
URL2 = "http://www.rosettacode.org/mw/api.php?format=json&action=query&generator=categorymembers&gcmtitle=Category:Programming%20Languages&gcmlimit=500&prop=categoryinfo&gcmcontinue="

'Get Contents of the API from the Web...
Function ScrapeGoat(link)
    On Error Resume Next
    ScrapeGoat = ""
    Err.Clear
    Set objHttp = CreateObject("Msxml2.ServerXMLHTTP")
    objHttp.Open "GET", link, False
    objHttp.Send
    If objHttp.Status = 200 And Err = 0 Then ScrapeGoat = objHttp.ResponseText
    Set objHttp = Nothing
End Function

'HACK: Setup HTML for help of my partner/competitor that is better than me, JavaScript...
Set HTML = CreateObject("HtmlFile")
Set HTMLWindow = HTML.ParentWindow


    ''''''''''''''''''''
    ' Main code begins '
    '..................'

On Error Resume Next

isComplete = 0    ' 1 -> Complete Already
cntLoop = 0       ' Counts Number of Loops Done
Set outputData = CreateObject("Scripting.Dictionary")

Do
    'Scrape Data From API
    If cntLoop = 0 Then strData = ScrapeGoat(URL1) Else strData = ScrapeGoat(URL2 & gcmCont)
    If Len(strData) = 0 Then
        Set HTML = Nothing
        WScript.StdErr.WriteLine "Processing of data stopped because API query failed."
        WScript.Quit(1)
    End If

    'Parse JSON HACK
    HTMLWindow.ExecScript "var json = " & strData, "JavaScript"
    Set ObjJS = HTMLWindow.json

    Err.Clear    'Test if Query is Complete Already
    batchCompl = ObjJS.BatchComplete
    If Err.Number = 438 Then
        'Query not yet complete. Get gcmContinue instead.
        gcmCont = ObjJS.[Query-Continue].CategoryMembers.gcmContinue
    Else
        isComplete = 1    'Yes!
    End If

    'HACK #2: Put all language page ids into a JS array to be accessed by VBScript
    HTMLWindow.ExecScript "var langs=new Array(); for(var lang in json.query.pages){langs.push(lang);}" & _
                          "var nums=langs.length;", "JavaScript"
    Set arrLangs = HTMLWindow.langs
    arrLength = HTMLWindow.nums

    For i = 0 to arrLength - 1
        BuffStr = "ObjJS.Query.Pages.[" & Eval("arrLangs.[" & i & "]") & "]"
        EachStr = Eval(BuffStr & ".title")

        Err.Clear
        CntLang =  Eval(BuffStr & ".CategoryInfo.Pages")
        If InStr(EachStr, "Category:") = 1 And Err.Number = 0 Then
            outputData.Add Replace(EachStr, "Category:", "", 1, 1), CntLang
        End If
    Next

    cntLoop = cntLoop + 1
Loop While isComplete = 0
'The outputData now contains the data we need. We should now sort and print it!

'Make a 2D array with copy of outputData
arrRelease = Array()
ReDim arrRelease(UBound(outputData.Keys), 1)

outKeys = outputData.Keys
outItem = outputData.Items
For i = 0 To UBound(outKeys)
    arrRelease(i, 0) = outKeys(i)
    arrRelease(i, 1) = outItem(i)
Next

'Bubble Sort (Greatest to Least Number of Examples)
For i = 0 to UBound(arrRelease, 1)
    For j = 0 to UBound(arrRelease, 1) - 1
        If arrRelease(j, 1) < arrRelease(j + 1, 1) Then
            temp1 = arrRelease(j + 1, 0)
            temp2 = arrRelease(j + 1, 1)
            arrRelease(j + 1, 0) = arrRelease(j, 0)
            arrRelease(j + 1, 1) = arrRelease(j, 1)
            arrRelease(j, 0) = temp1
            arrRelease(j, 1) = temp2
        End If
    Next
Next

'Save contents to file instead to support Unicode Names
Set objFSO = CreateObject("Scripting.FileSystemObject")
Set txtOut = objFSO.CreateTextFile(".\OutVBRC.txt", True, True)

txtOut.WriteLine "As of " & Now & ", RC has " & UBound(arrRelease) + 1 & " languages."
txtOut.WriteLine ""
For i = 0 to UBound(arrRelease)
    txtOut.WriteLine arrRelease(i, 1) & " Examples - " & arrRelease(i, 0)
Next

'Successfully Done :)
Set HTML = Nothing
Set objFSO = Nothing
WScript.Quit(0)
```

{{Out|Some Parts of Output as of December 31, 2016}}

```txt
As of 12/31/2016 11:52:05 PM, RC has 624 languages.

917 Examples - Racket
906 Examples - Python
894 Examples - Tcl
859 Examples - J
853 Examples - Perl 6
819 Examples - Zkl
813 Examples - Ruby
796 Examples - C
776 Examples - Java
774 Examples - Go
766 Examples - Haskell
760 Examples - REXX
755 Examples - Perl
750 Examples - D

.
.
.

0 Examples - SheerPower 4GL
0 Examples - Script Basic
0 Examples - VRML
0 Examples - Thistle
0 Examples - UserRPL
0 Examples - WML
0 Examples - VAX Assembly
```



## zkl

Using cURL and YAJL (yet anothe JSON library) libraries.

This solution using the API as the web scraping URL is pretty slow.
I'm using JSON as format=txt has been deprecated.

```zkl
var [const] CURL=Import("zklCurl"), YAJL=Import("zklYAJL")[0];

fcn getLangCounts(language){ // -->( (count,lang), ...)
   continueValue,tasks,curl := "",List(), CURL();  // "nm\0nm\0...."
   do{	// eg 5 times
      page:=curl.get(("http://rosettacode.org/mw/api.php?"
         "format=json"
	 "&action=query"
	 "&generator=categorymembers"
	 "&gcmtitle=Category:Programming%%20Languages"
	 "&gcmlimit=500"
	 "&prop=categoryinfo"
	 "&rawcontinue"		// remove warning
	 "&gcmcontinue=%s")
	 .fmt(continueValue));
      page=page[0].del(0,page[1]);  // get rid of HTML header
      json:=YAJL().write(page).close();

      json["query"]["pages"].howza(9).pump(tasks,fcn(d){ #dictionary values,only
         // { title:Category:AWK,categoryinfo:{ pages:398,size:401,... },... }
         // Gotta take care of no categoryinfo case
	 count:=d.find("categoryinfo",Dictionary).find("size",0);  // or pages?
	 if(count<300) return(Void.Skip);  // prune
         T(count,d["title"].del(0,9));     // "Category:RPL" --> "RPL"
      });

      if(continueValue=json.find("query-continue"))
      	// subcat|524558580a52455858|4331 or void
	 continueValue=continueValue["categorymembers"]["gcmcontinue"];
   }while(continueValue);
   tasks
}

langCounts:=getLangCounts()   .sort(fcn(a,b){ a[0]>b[0] });	// reverse sort

println("Most popular Rosetta Code languages as of ",Time.Date.prettyDay());
foreach n,name in ([1..15].zip(langCounts))
   { println("%2d: %3d %s".fmt(n,name.xplode())) }
```

{{out}}

```txt

Most popular Rosetta Code languages as of Saturday, the 23rd of September 2017
 1: 957 Racket
 2: 953 Python
 3: 918 Tcl
 4: 904 Perl 6
 5: 877 J
 6: 854 Zkl
 7: 840 Ruby
 8: 828 C
 9: 812 Go
10: 805 Haskell
11: 801 REXX
12: 800 Kotlin
13: 788 Java
14: 776 Perl
15: 755 D

```



{{omit from|Batch File}}
{{omit from|Brainfuck}}
{{omit from|Lilypond}}
{{omit from|Maxima}}
{{omit from|PARI/GP}}
{{omit from|PostScript}}
{{omit from|TI-83 BASIC|Does not have network access.}}
{{omit from|TI-89 BASIC|Does not have network access.}}
{{omit from|Yorick|Does not have network access.}}
{{omit from|ZX Spectrum Basic|Does not have network access.}}
