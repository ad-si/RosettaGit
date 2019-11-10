+++
title = "Rosetta Code/Count examples"
description = ""
date = 2019-07-24T15:18:40Z
aliases = []
[extra]
id = 3363
[taxonomies]
categories = []
tags = []
+++

{{task|Rosetta Code related}}Find the total number of programming examples for each [[:Category:Programming Tasks|task]] and the total for all tasks.

Essentially, count the number of occurrences of <tt>=={{header|</tt> on each task page.

Output:


```txt
100 doors: 20 examples.
99 Bottles of Beer: 29 examples.
Abstract type: 10 examples.

Total: X examples.
```


For a full output, updated periodically, see [[Rosetta Code/Count examples/Full list]].

You'll need to use the Media Wiki API, which you can find out about locally, [http://rosettacode.org/mw/api.php here], or in Media Wiki's API documentation at, [http://www.mediawiki.org/wiki/API_Query API:Query]


## Ada

{{libheader|AWS}}
Parsing XML file with XMLAda from AdaCore

```Ada
with Aws.Client, Aws.Messages, Aws.Response, Aws.Resources, Aws.Url;
with Dom.Readers, Dom.Core, Dom.Core.Documents, Dom.Core.Nodes, Dom.Core.Attrs;
with Input_Sources.Strings, Unicode, Unicode.Ces.Utf8;
with Ada.Strings.Unbounded, Ada.Strings.Fixed, Ada.Text_IO, Ada.Command_Line;
with Ada.Containers.Vectors;

use  Aws.Client, Aws.Messages, Aws.Response, Aws.Resources, Aws.Url;
use Dom.Readers, Dom.Core, Dom.Core.Documents, Dom.Core.Nodes, Dom.Core.Attrs;
use Aws, Ada.Strings.Unbounded, Ada.Strings.Fixed, Input_Sources.Strings;
use Ada.Text_IO, Ada.Command_Line;

procedure Count_Examples is

   package Members_Vectors is new Ada.Containers.Vectors (
      Index_Type => Positive,
      Element_Type => Unbounded_String);
   use Members_Vectors;

   Exemples      : Vector;
   Nbr_Lg, Total : Natural := 0;

   procedure Get_Vector (Category : in String; Mbr_Vector : in out Vector) is
      Reader  : Tree_Reader;
      Doc     : Document;
      List    : Node_List;
      N       : Node;
      A       : Attr;
      Page    : Aws.Response.Data;
      Uri_Xml : constant String :=
         "http://rosettacode.org/mw/api.php?action=query&list=categorymembers"
         &
         "&format=xml&cmlimit=500&cmtitle=Category:";
   begin
      Page := Client.Get (Uri_Xml & Category);
      if Response.Status_Code (Page) not  in Messages.Success then
         raise Client.Connection_Error;
      end if;
      declare
         Xml    : constant String := Message_Body (Page);
         Source : String_Input;
      begin
         Open
           (Xml'Unrestricted_Access,
            Unicode.Ces.Utf8.Utf8_Encoding,
            Source);
         Parse (Reader, Source);
         Close (Source);
      end;
      Doc  := Get_Tree (Reader);
      List := Get_Elements_By_Tag_Name (Doc, "cm");
      for Index in 1 .. Length (List) loop
         N := Item (List, Index - 1);
         A := Get_Named_Item (Attributes (N), "title");
         Append (Mbr_Vector, To_Unbounded_String (Value (A)));
      end loop;
      Free (List);
      Free (Reader);
   end Get_Vector;

   function Scan_Page (Title : String) return Natural is
      Page                      : Aws.Response.Data;
      File                      : Aws.Resources.File_Type;
      Buffer                    : String (1 .. 1024);
      Languages, Position, Last : Natural := 0;
   begin
      Page :=
         Client.Get
           ("http://rosettacode.org/mw/index.php?title=" &
            Aws.Url.Encode (Title) &
            "&action=raw");
      Response.Message_Body (Page, File);
      while not End_Of_File (File) loop
         Resources.Get_Line (File, Buffer, Last);
         Position :=
            Index
              (Source  => Buffer (Buffer'First .. Last),
               Pattern => "=={{header|");
         if Position > 0 then
            Languages := Languages + 1;
         end if;
      end loop;
      Close (File);
      return Languages;
   end Scan_Page;

begin
   Get_Vector ("Programming_Tasks", Exemples);

   for I in First_Index (Exemples) .. Last_Index (Exemples) loop
      declare
         Title : constant String :=
            To_String (Members_Vectors.Element (Exemples, I));
      begin
         Nbr_Lg := Scan_Page (Title);
         Total  := Total + Nbr_Lg;
         Put_Line (Title & " :" & Integer'Image (Nbr_Lg) & " exemples.");
      end;
   end loop;

   Put_Line ("Total :" & Integer'Image (Total) & " exemples.");
end Count_Examples;

```

Output :

```txt

100 doors : 107 exemples.
24 game : 30 exemples.
....
Yahoo! search interface : 10 exemples.
Zig-zag matrix : 49 exemples.
Total : 17238 exemples.
```



## AutoHotkey


```AutoHotkey
UrlDownloadToFile
  , http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml
  , tasks.xml
FileRead, tasks, tasks.xml
pos = 0
quote = "  ; "
regtitle := "<cm.*?title=" . quote . "(.*?)" . quote
While, pos := RegExMatch(tasks, regtitle, title, pos + 1)
{
  UrlDownloadToFile
    , % "http://www.rosettacode.org/w/index.php?title=" . title1 . "&action=raw"
    , task.xml
  FileRead, task, task.xml
  RegExReplace(task, "\{\{header\|", "", count)
  current :=  title1 . ": " . count . " examples.`n"
  output .= current
  TrayTip, current, % current
}
MsgBox % output
Return
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      VDU 23,22,640;512;8,16,16,128+8 : REM Enable UTF-8 support

      SYS "LoadLibrary", "URLMON.DLL" TO urlmon%
      SYS "GetProcAddress", urlmon%, "URLDownloadToFileA" TO UDTF
      special$ = "+()'"

      url$ = "http://www.rosettacode.org/w/api.php?action=query" + \
      \      "&list=categorymembers&cmtitle=Category:Programming_Tasks" + \
      \      "&cmlimit=500&format=xml"
      file$ = @tmp$ + "tasks.xml"
      SYS UDTF, 0, url$, file$, 0, 0 TO fail%
      IF fail% ERROR 100, "File download failed (tasks)"

      Total% = 0
      file% = OPENIN(file$)
      WHILE NOT EOF#file%
        a$ = GET$#file%
        i% = 0
        REPEAT
          i% = INSTR(a$, "title=", i%+1)
          IF i% THEN
            j% = INSTR(a$, ">", i%)
            title$ = MID$(a$, i%+7, j%-i%-10)
            REM Replace HTML codes:
            REPEAT
              k% = INSTR(title$, "&")
              IF k% THEN
                l% = INSTR(title$, ";", k%)
                title$ = LEFT$(title$,k%-1) + \
                \        FNhtml(MID$(title$,k%,l%-k%+1)) + MID$(title$,l%+1)
              ENDIF
            UNTIL k% = 0
            t$ = title$
            REM Substitute characters not allowed in a URL:
            FOR s% = 1 TO LEN(special$)
              REPEAT
                s$ = MID$(special$, s%, 1)
                k% = INSTR(t$, s$)
                IF k% t$ = LEFT$(t$,k%-1) + "%" + STR$~ASCs$ + MID$(t$,k%+1)
              UNTIL k% = 0
            NEXT
            url$ = "http://www.rosettacode.org/w/index.php?title=" + t$ + \
            \      "&action=raw"
            file$ = @tmp$ + "title.htm"
            SYS UDTF, 0, url$, file$, 0, 0 TO fail%
            IF fail% ERROR 100, "File download failed " + t$
            examples% = 0
            task% = OPENIN(file$)
            WHILE NOT EOF#task%
              IF INSTR(GET$#task%, "=={{header|") examples% += 1
            ENDWHILE
            CLOSE #task%
            Total% += examples%
            PRINT title$ ": " ; examples% " examples."
          ENDIF
        UNTIL i% = 0
        i% = INSTR(a$, "cmcontinue=")
        IF i% THEN
          CLOSE #file%
          j% = INSTR(a$, """", i%+1)
          k% = INSTR(a$, """", j%+1)
          url$ = "http://www.rosettacode.org/w/api.php?action=query" + \
          \      "&list=categorymembers&cmtitle=Category:Programming_Tasks" + \
          \      "&cmlimit=500&format=xml&cmcontinue=" + MID$(a$,j%+1,k%-j%)
          REPEAT
            i% = INSTR(url$, "|")
            IF i% url$ = LEFT$(url$,i%-1) + "%7C" + MID$(url$,i%+1)
          UNTIL i% = 0
          file$ = @tmp$ + "tasks.xml"
          SYS UDTF, 0, url$, file$, 0, 0 TO fail%
          IF fail% ERROR 100, "File download failed (continue)"
          file% = OPENIN(file$)
        ENDIF
      ENDWHILE
      CLOSE #file%
      PRINT ' "Total: " ; Total% " examples."
      END

      DEF FNhtml(h$)
      IF LEFT$(h$,2) = "&#" THEN = CHR$(VALMID$(h$,3))
      CASE h$ OF
        WHEN "&quot;": = """"
      ENDCASE
      = h$
```

'''Sample output:'''

```txt

100 doors: 154 examples.
24 game: 53 examples.
24 game/Solve: 30 examples.
99 Bottles of Beer: 181 examples.
A+B: 124 examples.
Abstract type: 49 examples.
Accumulator factory: 65 examples.
Ackermann function: 126 examples.
Active Directory/Connect: 12 examples.
Active Directory/Search for a user: 13 examples.
......
XML/DOM serialization: 33 examples.
XML/Input: 50 examples.
XML/Output: 41 examples.
XML/XPath: 33 examples.
Y combinator: 51 examples.
Yahoo! search interface: 13 examples.
Yin and yang: 36 examples.
Zebra puzzle: 11 examples.
Zeckendorf number representation: 14 examples.
Zig-zag matrix: 64 examples.

Total: 27004 examples.

```



## Bracmat


```bracmat
( ( get-page
  =
    .   sys$(str$("wget -q -O wget.out \"" !arg \"))
      & get$("wget.out",HT ML)
  )
&   get-page$"http://rosettacode.org/wiki/Category:Programming_Tasks"
  : ? (table.?) ?tasklist (.table.) ?
& 0:?list
&   whl
  ' ( !tasklist
    :   ?
        ( a
        .   (href.@(?:"/wiki/" ?href)) (title.?title)
          &   get-page$(str$("http://rosettacode.org/wiki/" !href))
            : ?task
          & 0:?cnt
          &   whl
            ' (   !task
                :   ?
                    (   (span.(class.mw-headline) (id.?))
                        ?span
                        (.span.)
                        ?task
                    &   !span
                      :   ?
                          ( a
                          .   (href.@(?:"/wiki/Category:" ?))
                              (title.@(?:"Category:" ?))
                          )
                          @
                          (.a.)
                          ?
                    )
              & 1+!cnt:?cnt
              )
          & (!cnt.!title)+!list:?list
        )
        ?tasklist
    )
& lst$(list,taskfreq,NEW)
)
```

Output (in file <code>tasqfreq</code>):

```txt
list=
  (2."OLE Automation")
+ (3."Canny edge detector")
+ ( 3
  . "Continued fraction/Arithmetic/G(matrix NG, Contined Fraction N1, Contined Fraction N2)"
  )
+ (4."Colour pinstripe/Printer")
+ (4."Vogel's approximation method")
+ (5."Catmullâ€“Clark subdivision surface")
+ (5."Percolation/Bond percolation")
+ (5.Pinstripe/Printer)
+ (5."Zeckendorf arithmetic")
+ (6."Continued fraction/Arithmetic/G(matrix NG, Contined Fraction N)")
+ (6."Percolation/Mean cluster density")
+ (7."Bitmap/PPM conversion through a pipe")
+ (7."Deconvolution/2D+")
+ (7."K-d tree")
  ....
+ (125."Greatest element of a list")
+ (127."Averages/Arithmetic mean")
+ (131.Arrays)
+ (131."Increment a numerical string")
+ (132."Greatest common divisor")
+ (133.Loops/While)
+ (134."Conditional structures")
+ (136.Arithmetic/Integer)
+ (137.Loops/For)
+ (145.Loops/Infinite)
+ (147."Ackermann function")
+ (148."Reverse a string")
+ (152."A+B")
+ (152."Function definition")
+ (160."Empty program")
+ (163."Fibonacci sequence")
+ (164.Factorial)
+ (182.FizzBuzz)
+ (187."100 doors")
+ (188.Comments)
+ (216."99 Bottles of Beer")
+ (269."Hello world/Text");

```


=={{header|C sharp|C#}}==

Object-oriented solution.


```csharp
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using System.Net;

class Task {
    private string _task;
    private int _examples;

    public Task(string task, int examples) {
        _task = task;
        _examples = examples;
    }

    public string Name {
        get { return _task; }
    }

    public int Examples {
        get { return _examples; }
    }

    public override string ToString() {
        return String.Format("{0}: {1} examples.", this._task, this._examples);
    }
}

class Program {
    static List<string> GetTitlesFromCategory(string category, WebClient wc) {
        string content = wc.DownloadString(
            String.Format("http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:{0}&cmlimit=500&format=json", category)
        );

        return new Regex("\"title\":\"(.+?)\"").Matches(content).Cast<Match>().Select(x => x.Groups[1].Value.Replace("\\/", "/")).ToList();
    }

    static string GetSourceCodeFromPage(string page, WebClient wc) {
        return wc.DownloadString(
            String.Format("http://www.rosettacode.org/w/index.php?title={0}&action=raw", page)
        );
    }

    static void Main(string[] args) {
        WebClient wc = new WebClient();
        List<Task> tasks = new List<Task>();
        List<string> tasknames = GetTitlesFromCategory("Programming_Tasks", wc);

        foreach (string task in tasknames) {
            string content = GetSourceCodeFromPage(task, wc);
            int count = new Regex("=={{header", RegexOptions.IgnoreCase).Matches(content).Count;
            Task t = new Task(task, count);

            Console.WriteLine(t);
            tasks.Add(t);
        }

        Console.WriteLine("\nTotal: {0} examples.", tasks.Select(x => x.Examples).Sum());
    }
}
```



## Clojure


```clojure
(ns count-examples
  (:import [java.net URLEncoder])
  (:use [clojure.contrib.http.agent :only (http-agent string)]
        [clojure.contrib.json :only (read-json)]
        [clojure.contrib.string :only (join)]))

(defn url-encode [v] (URLEncoder/encode (str v) "utf-8"))

(defn rosettacode-get [path params]
  (let [param-string (join "&" (for [[n v] params] (str (name n) "=" (url-encode v))))]
    (string (http-agent (format "http://www.rosettacode.org/w/%s?%s" path param-string)))))

(defn rosettacode-query [params]
  (read-json (rosettacode-get "api.php" (merge {:action "query" :format "json"} params))))

(defn list-cm
  ([params] (list-cm params nil))
  ([params continue]
     (let [cm-params (merge {:list "categorymembers"} params (or continue {}))
           result (rosettacode-query cm-params)]
       (concat (-> result (:query) (:categorymembers))
               (if-let [cmcontinue (-> result (:query-continue) (:categorymembers))]
                 (list-cm params cmcontinue))))))

(defn programming-tasks []
  (let [result (list-cm {:cmtitle "Category:Programming_Tasks" :cmlimit 50})]
    (map #(:title %) result)))

(defn task-count [task]
  [task (count
         (re-seq #"==\{\{header"
                 (rosettacode-get "index.php" {:action "raw" :title task})))])

(defn print-result []
  (let [task-counts (map task-count (programming-tasks))]
    (doseq [[task count] task-counts]
      (println (str task ":") count)
      (flush))
    (println "Total: " (reduce #(+ %1 (second %2)) 0 task-counts))))

```


```clojure
count-examples> (print-result)
100 doors: 73
24 game: 18
24 game/Solve: 14
99 Bottles of Beer: 89
Abstract type: 27
Accumulator factory: 23
Ackermann function: 73
Active Directory/Connect: 4
Active Directory/Search for a user: 3
Active object: 14
Add a variable to a class instance at runtime: 21
Address of a variable: 20
...
Total:  11216
nil

```



## D

{{works with|Tango}}

```D

import tango.io.Stdout;
import tango.net.http.HttpClient;
import tango.net.http.HttpHeaders;
import tango.text.xml.Document;
import tango.text.Util;

alias HttpHeader.ContentLength CL;

auto url = "http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml";
void main()
{
    auto client = new HttpClient (HttpClient.Get, url);
    client.open();
    char[] mainData, tmp;
    int total, i;

    void cat(void[] content) { tmp ~= cast(char[]) content; }

    if (client.isResponseOK) {
        client.read(&cat, client.getResponseHeaders.getInt(CL));
        mainData = tmp;
        tmp = null;

        auto doc = new Document!(char);
        doc.parse(mainData);
        foreach (n; doc.query.descendant("cm").attribute("title")) {
            auto subClient = new HttpClient(HttpClient.Get,
                    "http://www.rosettacode.org/w/index.php?title=" ~
                    replace(n.value.dup, ' ', '_') ~ "&action=raw");
            subClient.open();
            if (! subClient.isResponseOK) {
                Stderr (client.getResponse);
                 break;
            }
            subClient.read(&cat, subClient.getResponseHeaders.getInt(CL));
            foreach (segment; patterns(cast(char[])tmp, "=={{header|")) i++;
            --i;
            if (i) --i;
            Stdout.formatln ("{0,-40} - {}", n.value, i);
            total += i;
            tmp = null;
            i = 0;
        }
        Stdout("total examples: ", total).newline;
    } else {
        Stderr (client.getResponse);
    }
}

```



## EGL

{{works with|EDT}}
[[File:Catcount.PNG|thumb|right|EGL: Graphical client implementation]]
A graphical implementation with a grid showing the number of implementations for each Rosetta Code task as well as total task and implementation counts. Uses MediaWiki API service call to fetch tasks/categories in a JSON format and meets API data limit and continuation requirements to consume 100% of the items.

User Interface: RosettaCodeHandler.egl

```EGL
package com.eglexamples.client;

import org.eclipse.edt.rui.widgets.*;

handler RosettaCodeHandler type RUIhandler{initialUI =[ui], title = "Rosetta Code Tasks and Counts"}

    ui GridLayout{columns = 3, rows = 4, cellPadding = 4, children = [ b1, dg1, l1, l2, l3, l4 ]};

    b1 Button{ layoutData = new GridLayoutData{ row = 1, column = 1 }, text = "Go!", onClick ::= b1_onClick };
    l1 TextLabel{ layoutData = new GridLayoutData{ row = 1, column = 2 }, text = "Total Tasks:" };
    l2 TextLabel{ layoutData = new GridLayoutData{ row = 1, column = 3 }, text = "0" };

    l3 TextLabel{ layoutData = new GridLayoutData{ row = 2, column = 2 }, text = "Total Implementations:" };
    l4 TextLabel{ layoutData = new GridLayoutData{ row = 2, column = 3 }, text = "0" };

    dg1 DataGrid{ layoutData = new GridLayoutData{ row = 3, column = 1, horizontalSpan = 3 },
    	pageSize = 10, showScrollbar = true,
	columns = [ new DataGridColumn{name = "title", displayName = "Task", width=220},
		    new DataGridColumn{name = "count", displayName = "Count", width=100} ] };

    cmcontinue string?;
    title string?;
    allTasks Task[];

    restBindingTasks IHttp? = new HttpRest{
        restType = eglx.rest.ServiceType.TrueRest,
   	request.uri = "http://rosettacode.org/mw/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=1&format=json"};

    restBindingPageDetail IHttp? = new HttpRest{
        restType = eglx.rest.ServiceType.TrueRest,
   	request.uri = "http://rosettacode.org/mw/index.php"};

    function b1_onClick(event Event in)
	call ProxyFunctions.listTasks("") using restBindingTasks
    	    returning to listTasksCallBack onException exceptionHandler;
    end

    function listTasksCallBack(retResult RosettaCodeJSON in)
	title = retResult.query.categorymembers[1].title;
	cmcontinue = retResult.queryContinue.categorymembers.cmcontinue;

	call ProxyFunctions.fetchPageDetail(title) using restBindingPageDetail
 	    returning to pageDetailCallBack onException exceptionHandler;
    end

    function pageDetailCallBack(pageResults string in)
   	count int = countSubstring("=={{header", pageResults);
   	allTasks.appendElement(new Task { title = title, count = count });
	l2.text = l2.text as int + 1;
	l4.text = l4.text as int + count;

   	if(cmcontinue != null)
	    call ProxyFunctions.listTasks(cmcontinue) using restBindingTasks
		returning to listTasksCallBack onException exceptionHandler;
	else
	    dg1.data = allTasks as any[];
	end
    end

    function countSubstring(substr string in, str string in) returns(int)
	if(str.length() > 0 and substr.length() > 0)
	    return (str.length() - str.replaceStr(subStr, "").length()) / subStr.length();
	else
	    return 0;
	end
    end

   function exceptionHandler(exp AnyException in)
   end

end

record Task
	title string;
	count int;
end
```


Service Interface: ProxyFunctions.egl

```EGL
package com.eglexamples.client;

library ProxyFunctions

    function listTasks(continueLocation String in) returns (RosettaCodeJSON) {
	    @Rest{method = _GET, uriTemplate = "&cmcontinue={continueLocation}",
	    requestFormat = None, responseFormat = JSON}
    }
    end

    function fetchPageDetail(title String in) returns (String) {
	    @Rest{method = _GET, uriTemplate = "?title={title}&action=raw",
	    requestFormat = None, responseFormat = None}
    }
    end

end

record RosettaCodeJSON
    query Query;
    queryContinue QueryContinue{JSONName = "query-continue"};
end

record Query
    categorymembers Categorymembers[];
end

record Categorymembers
    cmcontinue string?;
    pageid int?;
    ns int?;
    title string?;
end

record QueryContinue
    categorymembers Categorymembers;
end
```




## Erlang


{{libheader|xmerl}}


```erlang

-module(rosseta_examples).
-include_lib("xmerl/include/xmerl.hrl").

-export([main/0]).

main() ->
   application:start(inets),
   Titles = read_titles(empty),
   Result = lists:foldl(fun(Title,Acc) -> Acc + calculate_one(Title) end, 0, Titles),
   io:format("Total: ~p examples.\n",[Result]),
   application:stop(inets).

read_titles(CurrentContinue) ->
   URL0 = "http://rosettacode.org/mw/api.php?" ++
         "action=query&list=categorymembers&cmtitle=Category:Programming_Tasks" ++
         "&cmlimit=500&format=xml",
   URL =
      case CurrentContinue of
         empty -> URL0;
         _ -> URL0 ++ "&cmcontinue=" ++ CurrentContinue
      end,
   {ok,Answer} = httpc:request(URL),
   {Document,_} = xmerl_scan:string(lists:last(tuple_to_list(Answer))),
   Continue =
      [Value || #xmlAttribute{value = Value} <- xmerl_xpath:string("//@cmcontinue", Document)],
   Titles =
     [Value || #xmlAttribute{value = Value} <- xmerl_xpath:string("//@title", Document)],
   case Continue of
      []->
         Titles;
      [ContValue | _] ->
         Titles ++ read_titles(ContValue)
   end.

calculate_one(Title0) ->
   Title = replace_chars(Title0),
   URL = "http://www.rosettacode.org/w/index.php?title=" ++
         Title ++ "&action=raw",
   case httpc:request(URL) of
      {ok,Result} ->
            {match,Found} =
               re:run(lists:last(tuple_to_list(Result)), "\n=={{header(|)", [global]),
            io:format("~ts: ~p examples.\n",[Title0,length(Found)]),
            length(Found);
      {error,socket_closed_remotely} ->
         io:format("Socket closed remotely. Retry.\n"),
         calculate_one(Title0)
   end.

replace_chars(String) ->
   replace_chars(String,[]).

replace_chars([$ | T],Acc) ->
   replace_chars(T, [$_| Acc]);
replace_chars([$+| T],Acc) ->
   replace_chars(T, lists:reverse("%2B") ++ Acc);
replace_chars([8211| T],Acc) ->
   replace_chars(T, lists:reverse("%E2%80%93") ++ Acc);
replace_chars([Other| T],Acc) ->
   replace_chars(T, [Other| Acc]);
replace_chars([],Acc) ->
   lists:reverse(Acc).

```



Outputs:

```txt

> rosseta_examples:main().
100 doors: 165 examples.
24 game: 56 examples.
24 game/Solve: 33 examples.
...
Zebra puzzle: 12 examples.
Zeckendorf number representation: 18 examples.
Zig-zag matrix: 65 examples.
Total: 28629 examples.

```


=={{header|F Sharp|F#}}==

Using asynchronous workflows to perform downloads concurrently:


```fsharp
#r "System.Xml.Linq.dll"

let uri1 = "http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml"
let uri2 task = sprintf "http://www.rosettacode.org/w/index.php?title=%s&action=raw" task

[|for xml in (System.Xml.Linq.XDocument.Load uri1).Root.Descendants() do
    for attrib in xml.Attributes() do
      if attrib.Name.LocalName = "title" then
        yield async {
          let uri = uri2 (attrib.Value.Replace(" ", "_") |> System.Web.HttpUtility.UrlEncode)
          use client = new System.Net.WebClient()
          let! html = client.AsyncDownloadString(System.Uri uri)
          let sols' = html.Split([|"{{header|"|], System.StringSplitOptions.None).Length - 1
          lock stdout (fun () -> printfn "%s: %d examples" attrib.Value sols')
          return sols' }|]
|> Async.Parallel
|> Async.RunSynchronously
|> fun xs -> printfn "Total: %d examples" (Seq.sum xs)
```


This is 21&#215; faster than the python thanks to the concurrency.

## Factor

Runs in about a minute. The number of threads is limited to 10 avoid cloudfare's protection mechanism.


```factor
USING: arrays assocs concurrency.combinators
concurrency.semaphores formatting hashtables http.client io
json.reader kernel math math.parser sequences splitting
urls.encoding ;
IN: rosetta-code.count-examples

CONSTANT: list-url "http://rosettacode.org/mw/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&cmprop=title&format=json"

: titles ( query -- titles )
  "query" of "categorymembers" of [ "title" of ] map ;
: continued-url ( query -- url/f )
  "query-continue" of "categorymembers" of
  [ assoc>query list-url swap "&" glue ] [ f ] if* ;

: (all-programming-titles) ( titles url -- titles' url' )
  http-get nip json> [ titles append ] [ continued-url ] bi
  [ (all-programming-titles) ] [ f ] if* ;
: all-programming-titles ( -- titles ) { } list-url (all-programming-titles) drop ;

CONSTANT: content-base-url "http://rosettacode.org/mw/index.php?title=&action=raw"
: content-url ( title -- url )
  " " "_" replace
  "title" associate assoc>query
  content-base-url swap "&" glue ;

: occurences ( seq subseq -- n ) split-subseq length 1 - ;
: count-examples ( title -- n )
  content-url http-get nip "=={{header|" occurences ;

: print-task ( title n -- ) "%s: %d examples.\n" printf ;
: print-total ( assoc -- ) values sum "Total: %d examples.\n" printf ;
: fetch-counts ( titles -- assoc )
  10 <semaphore> [
    [ dup count-examples 2array ] with-semaphore
  ] curry parallel-map ;

: print-counts ( titles -- )
  [ [ print-task ] assoc-each nl ] [ print-total ] bi ;
: rosetta-examples ( -- )
  all-programming-titles fetch-counts print-counts ;

MAIN: rosetta-examples
```

Outputs:


```txt
100 doors: 169 examples.
24 game: 58 examples.
...
Zeckendorf number representation: 22 examples.
Zig-zag matrix: 66 examples.

Total: 30745 examples.
```


## Go


```go
package main

import (
    "bytes"
    "encoding/xml"
    "fmt"
    "io"
    "io/ioutil"
    "net/http"
    "net/url"
    "strings"
)

func req(u string, foundCm func(string)) string {
    resp, err := http.Get(u)
    if err != nil {
        fmt.Println(err) // connection or request fail
        return ""
    }
    defer resp.Body.Close()
    for p := xml.NewDecoder(resp.Body); ; {
        t, err := p.RawToken()
        switch s, ok := t.(xml.StartElement); {
        case err == io.EOF:
            return ""
        case err != nil:
            fmt.Println(err)
            return ""
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

func main() {
    taskQuery := "http://rosettacode.org/mw/api.php?action=query" +
        "&format=xml&list=categorymembers&cmlimit=500" +
        "&cmtitle=Category:Programming_Tasks"
    continueAt := req(taskQuery, count)
    for continueAt > "" {
        continueAt = req(taskQuery+"&cmcontinue="+continueAt, count)
    }
    fmt.Printf("Total: %d examples.\n", total)
}

var marker = []byte("=={{header|")
var total int

func count(cm string) {
    taskFmt := "http://rosettacode.org/mw/index.php?title=%s&action=raw"
    taskEsc := url.QueryEscape(strings.Replace(cm, " ", "_", -1))
    resp, err := http.Get(fmt.Sprintf(taskFmt, taskEsc))
    var page []byte
    if err == nil {
        page, err = ioutil.ReadAll(resp.Body)
        resp.Body.Close()
    }
    if err != nil {
        fmt.Println(err)
        return
    }
    examples := bytes.Count(page, marker)
    fmt.Printf("%s: %d\n", cm, examples)
    total += examples
}
```

{{out|Output: (May 25, 2011)}}

```txt

...
Y combinator: 40
Yahoo! search interface: 10
Yin and yang: 18
Zig-zag matrix: 50
Total: 18290 examples.

```



## Haskell

{{libheader|HTTP XML}} from [http://hackage.haskell.org/packages/hackage.html HackageDB]


```haskell
import Network.Browser
import Network.HTTP
import Network.URI
import Data.List
import Data.Maybe
import Text.XML.Light
import Control.Arrow

justifyR w = foldl ((.return).(++).tail) (replicate w ' ')
showFormatted t n = t ++ ": " ++ justifyR 4 (show n)

getRespons url = do
    rsp <- Network.Browser.browse $ do
      setAllowRedirects True
      setOutHandler $ const (return ())     -- quiet
      request $ getRequest url
    return $ rspBody $ snd rsp

getNumbOfExampels p = do
  let pg = intercalate "_" $ words p
  rsp <- getRespons $ "http://www.rosettacode.org/w/index.php?title=" ++ pg ++ "&action=raw"
  let taskPage = rsp
      countEx = length $ filter (=="=={{header|") $ takeWhile(not.null) $ unfoldr (Just. (take 11 &&& drop 1)) taskPage
  return countEx

progTaskExamples = do
  rsp <- getRespons "http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml"

  let xmls = onlyElems $ parseXML $ rsp
      tasks = concatMap (map (fromJust.findAttr (unqual "title")). filterElementsName (== unqual "cm")) xmls

  taskxx <- mapM getNumbOfExampels tasks
  let ns = taskxx
      tot = sum ns

  mapM_ putStrLn $ zipWith showFormatted tasks ns
  putStrLn $ ("Total: " ++) $ show tot
```

some output:

```haskell
*Main> progTaskExamples
100 doors:   56
24 game:   11
24 game Player:    9
99 Bottles of Beer:   73
Abstract type:   23
Ackermann Function:   61
Active object:    9
...
Total: 9156
```


==Icon and {{header|Unicon}}==
The following code uses features exclusive to Unicon.  This version handles all tasks, not just the first 500.


```Unicon
$define RCINDEX "http://rosettacode.org/mw/api.php?format=xml&action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500"
$define RCTASK  "http://rosettacode.org/mw/index.php?action=raw&title="
$define RCUA    "User-Agent: Unicon Rosetta 0.1"
$define RCXUA   "X-Unicon: http://unicon.org/"
$define TASKTOT "* Total Tasks *"
$define TOTTOT  "* Total Headers*"

link strings
link hexcvt

procedure main(A)   # simple single threaded read all at once implementation
    Tasks := table(0)
    every task := taskNames() do {
       Tasks[TASKTOT] +:= 1                            # count tasks
       every lang := languages(task) do {              # count languages
          Tasks[task] +:= 1
          Tasks[TOTTOT] +:= 1
          }
       }
    every insert(O := set(),key(Tasks))                # extract & sort keys
    O := put(sort(O--set(TOTTOT,TASKTOT)),TASKTOT,TOTTOT)  # move totals to end
    every write(k := !O, " : ", Tasks[k]," examples.") # report
end

# Generate task names
procedure taskNames()
    continue := ""
    while \(txt := ReadURL(RCINDEX||continue)) do {
        txt ? {
            while tab(find("<cm ") & find(s :="title=\"")+*s) do
                suspend tab(find("\""))\1
            if tab(find("cmcontinue=")) then {
                continue := "&"||tab(upto(' \t'))
                }
            else break
            }
        }
end

# Generate language headers in a task
procedure languages(task)
    static WS
    initial WS := ' \t'
    page := ReadURL(RCTASK||CleanURI(task))
    page ? while (tab(find("\n==")),tab(many(WS))|"",tab(find("{{"))) do {
               header := tab(find("=="))
               header ? {
                   while tab(find("{{header|")) do {
                       suspend 2(="{{header|",tab(find("}}")))\1
                       }
                   }
               }
end

procedure CleanURI(u)                  #: clean up a URI
    static tr,dxml                     # xml & http translation
    initial {
       tr := table()
       every c := !string(~(&digits++&letters++'-_.!~*()/\'`')) do
          tr[c] := "%"||hexstring(ord(c),2)
       every /tr[c := !string(&cset)] := c
       tr[" "] := "_"                                      # wiki convention
       every push(dxml := [],"&#"||right(ord(c := !"&<>'\""),3,"0")||";",c)
       }

    dxml[1] := u                       # insert URI as 1st arg
    u := replacem!dxml                 # de-xml it
    every (c := "") ||:= tr[!u]        # reencode everything
    c := replace(c,"%3E","'")          # Hack to put single quotes back in
    c := replace(c,"%26quot%3B","\"")  # Hack to put double quotes back in
    return c
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


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/strings.icn strings provides replacem]
[http://www.cs.arizona.edu/icon/library/src/procs/strings.icn  hexcvt provides hexstring]

Sample Output for July 6, 2013 (abridged):

```txt

100 doors : 171 examples.
24 game : 60 examples.
24 game/Solve : 37 examples.
9 billion names of God the integer : 12 examples.
99 Bottles of Beer : 199 examples.
A+B : 137 examples.
Abstract type : 54 examples.
Accumulator factory : 67 examples.
Ackermann function : 137 examples.
...
Y combinator : 56 examples.
Yahoo! search interface : 18 examples.
Yin and yang : 39 examples.
Zebra puzzle : 12 examples.
Zeckendorf arithmetic : 3 examples.
Zeckendorf number representation : 21 examples.
Zig-zag matrix : 67 examples.
* Total Tasks * : 676 examples.
* Total Headers* : 31146 examples.

```



## J

'''Solution:'''

Using <code>getCategoryMembers</code> from [[Find unimplemented tasks#J|Find unimplemented tasks]].

```j
require 'web/gethttp'

getAllTaskSolnCounts=: monad define
  tasks=.  getCategoryMembers 'Programming_Tasks'
  counts=. getTaskSolnCounts &> tasks
  tasks;counts
)

getTaskSolnCounts=: monad define
  makeuri=. 'http://www.rosettacode.org/w/index.php?title=' , ,&'&action=raw'
  wikidata=. gethttp makeuri urlencode y
  ([: +/ '{{header|'&E.) wikidata
)

formatSolnCounts=: monad define
  'tasks counts'=. y
  tasks=. tasks , &.>':'
  res=. ;:^:_1 tasks ,. (8!:0 counts) ,. <'examples.'
  res , 'Total examples: ' , ": +/counts
)
```


'''Example Usage:'''

```j
   formatSolnCounts getAllTaskSolnCounts ''
100 doors: 61 examples.
24 game: 15 examples.
24 game Player: 11 examples.
99 Bottles of Beer: 76 examples.
...
```



## Java

{{works with|Java|1.5+}}

```java5

import java.util.ArrayList;
import ScreenScrape;

public class CountProgramExamples {
    private static final String baseURL = "http://rosettacode.org/wiki/";
    private static final String rootURL = "http://www.rosettacode.org/w/"
        + "api.php?action=query&list=categorymembers"
        + "&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml";
    private static final String taskBegin = "title=\"";
    private static final String taskEnd = "\"";
    private static final String exmplBegin = "<span class=\"tocnumber\">";
    private static final String exmplEnd = "</span>";
    private static final String editBegin = "<span class=\"editsection\">";

    /**
     * @param args
     */
    public static void main(String[] args) {
        int exTotal = 0;
        try {
            // Get root query results
            ArrayList<String> tasks = new ArrayList<String>();
            ScreenScrape ss = new ScreenScrape();
            String rootPage = ss.read(rootURL);
            while (rootPage.contains(taskBegin)) {
                rootPage = rootPage.substring(rootPage.indexOf(taskBegin)
                    + taskBegin.length());
                String title = rootPage.substring(0, rootPage.indexOf(taskEnd));
                if (!title.contains("Category:")) {
                    tasks.add(title);
                }
                rootPage = rootPage.substring(rootPage.indexOf(taskEnd));
            }
            // Loop through each task and print count
            for (String task : tasks) {
                String title = task.replaceAll("&#039;", "'");
                String taskPage = ss.read(baseURL + title.replaceAll(" ", "_"));
                int exSubTot;
                if (taskPage.contains(exmplBegin)) {
                    int startPos = taskPage.lastIndexOf(exmplBegin)
                        + exmplBegin.length();
                    String countStr = taskPage.substring(startPos,
                        taskPage.indexOf(exmplEnd, startPos));
                    exSubTot = Integer
                        .parseInt(countStr.contains(".") ? countStr
                            .substring(0, countStr.indexOf("."))
                            : countStr);
                } else {
                    exSubTot = 0;
                    while (taskPage.contains(editBegin)) {
                        taskPage = taskPage.substring(taskPage
                            .indexOf(editBegin) + editBegin.length());
                        exSubTot++;
                    }
                }
                exTotal += exSubTot;
                System.out.println(title + ": " + exSubTot + " examples.");
            }
            // Print total
            System.out.println("\nTotal: " + exTotal + " examples.");
        } catch (Exception e) {
            System.out.println(title);
            System.out.println(startPos + ":"
                + taskPage.indexOf(exmplEnd, startPos));
            System.out.println(taskPage);
            e.printStackTrace(System.out);
        }
    }
}

```

[[Count programming examples/Java/ScreenScrape|ScreenScrape class]]




## jq

jq does not duplicate the functionality of `curl` but works seamlessly with it,
as illustrated by the following bash script.  Note in particular the use of jq's
`@uri` filter in the bash function `titles`.


```bash
#!/bin/bash

# Produce lines of the form: URI TITLE
function titles {
    local uri="http://www.rosettacode.org/mw/api.php?action=query&list=categorymembers"
    uri+="&cmtitle=Category:Programming_Tasks&cmlimit=5000&format=json"
    curl -Ss "$uri" |
      jq -r '.query.categorymembers[] | .title | "\(@uri) \(.)"'
}

# Syntax: count URI
function count {
    local uri="$1"
    curl -Ss "http://rosettacode.org/mw/index.php?title=${uri}&action=raw" |
      jq -R -n 'reduce (inputs|select(test("=={{header\\|"))) as $x(0; .+1)'
}

local n=0 i
while read uri title
do
    i=$(count "$uri")
    echo "$title: $i examples."
    n=$((n + i))
done < <(titles)
echo Total: $n examples.
```


{{out}}


```txt
100 doors: 252 examples.
15 Puzzle Game: 36 examples.
2048: 24 examples.
...
Order two numerical lists: 65 examples.
Ordered Partitions: 28 examples.
Ordered words: 85 examples.
Palindrome detection: 136 examples.
Total: 32416 examples.
```



## Julia

Output by page is too long, so summaries only output shown.

```julia
using HTTP, JSON, Dates

rosorg = "http://rosettacode.org"
qURI = "/mw/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=json"
qdURI = "/mw/api.php?action=query&list=categorymembers&cmtitle=Category:Draft_Programming_Tasks&cmlimit=500&format=json"
sqURI = rosorg * "/wiki/"
topages(js, v) = for d in js["query"]["categorymembers"] push!(v, sqURI * replace(d["title"], " " => "_")) end

function getpages(uri)
    wikipages = Vector{String}()
    response = HTTP.request("GET", rosorg * uri)
    if response.status == 200
        fromjson = JSON.parse(String(response.body))
        topages(fromjson, wikipages)
        while haskey(fromjson, "continue")
            cmcont, cont = fromjson["continue"]["cmcontinue"], fromjson["continue"]["continue"]
            response = HTTP.request("GET", rosorg * uri * "&cmcontinue=$cmcont&continue=$cont")
            fromjson = JSON.parse(String(response.body))
            topages(fromjson, wikipages)
        end
    end
    wikipages
end

function processtaskpages(wpages, verbose=false)
    totalexamples = 0
    for pag in wpages
        response = HTTP.request("GET", pag)
        if response.status == 200
            n = length(collect(eachmatch(r"span class=\"mw-headline\"", String(response.body))))
            if verbose
                println("Wiki page $pag => $n examples.")
            end
            totalexamples += n
        end
    end
    println("Total of $totalexamples on $(length(wpages)) task pages.\n")
end


println("Programming examples at $(DateTime(now())):")
qURI |> getpages |> processtaskpages

println("Draft programming tasks:")
qdURI |> getpages |> processtaskpages

```
{{out}}

```txt

Programming examples at 2019-02-16T21:04:15.583:
Total of 66388 on 928 task pages.

Draft programming tasks:
Total of 3385 on 216 task pages.

```



## Lasso


```Lasso
local(root = json_deserialize(curl('http://rosettacode.org/mw/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=10&format=json')->result))
local(tasks = array, title = string, urltitle = string, thiscount = 0, totalex = 0)
with i in #root->find('query')->find('categorymembers') do => {^
	#thiscount = 0
	#title = #i->find('title')
	#urltitle = #i->find('title')
	#urltitle->replace(' ','_')

	#title+': '
	local(src = curl('http://rosettacode.org/mw/index.php?title='+#urltitle->asBytes->encodeurl+'&action=raw')->result->asString)
	#thiscount = (#src->split('=={{header|'))->size - 1
	#thiscount < 0 ? #thiscount = 0
	#thiscount + ' examples.'
	#totalex += #thiscount
	'\r'
^}
'Total: '+#totalex+' examples.'
```


{{out}}
Collecting 10 results:

```txt
100_doors: 176 examples.
24_game: 58 examples.
24_game/Solve: 35 examples.
9_billion_names_of_God_the_integer: 16 examples.
99_Bottles_of_Beer: 210 examples.
A+B: 142 examples.
Abstract_type: 54 examples.
Accumulator_factory: 70 examples.
Ackermann_function: 143 examples.
Active_Directory/Connect: 15 examples.
Total: 919 examples.

```



## LiveCode

1. Add a button to a stack, put the following into the mouseUp

2. Add a text field called "tasks"

n.b. The list of tasks is limited to 10 for demo purposes
```LiveCode
on mouseUp
    put empty into fld "taskurls"
    put URL "http://rosettacode.org/mw/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=10&format=xml" into  apixml
    put revXMLCreateTree(apixml,true,true,false) into pDocID
    put "/api/query/categorymembers/cm" into pXPathExpression
    repeat for each line xmlnode in revXMLEvaluateXPath(pDocID, pXPathExpression)
        put revXMLAttribute(pDocID,xmlnode,"title") into pgTitle
        put revXMLAttribute(pDocID,xmlnode,"pageid") into pageId
        put "http://www.rosettacode.org/w/index.php?title=" & urlEncode(pgTitle) & "&action=raw" into taskURL
        put URL taskURL into taskPage
        filter lines of taskPage with "=={{header|*"
        put the number of lines of taskPage into taskTotal
        put pgTitle & comma & taskTotal & cr after fld "tasks"
        add taskTotal to allTaskTotal
    end repeat
    put "Total" & comma & allTaskTotal after fld "tasks"
end mouseUp
```



## Maple


```Maple
ConvertUTF8 := proc( str )
        local i, tempstring, uniindex;
       try
           tempstring := str;
           uniindex := [StringTools:-SearchAll("\u",str)];
           if uniindex <> [] then
               for i in uniindex  do
                   tempstring := StringTools:-Substitute(tempstring, str[i..i+5], UTF8:-unicode(str[i+2..i+5]));
               end do:
           end if;
           return tempstring;
       catch:
           return str;
       end try;
   end proc:
print_examples := proc(lst)
	local task, count, url, headers, item;
	for task in lst do
		count := 0:
		url := cat("http://www.rosettacode.org/mw/index.php?title=", StringTools:-Encode(StringTools:-SubstituteAll(task["title"], " ", "_"), 'percent'), "&action=raw"):
		headers := [StringTools:-SearchAll("=={{header|",URL:-Get(url))]:
		for item in headers do
			count++:
		end do:
		printf("%s has %d examples\n",ConvertUTF8(task["title"]), count);
	end do:
end proc:


x := JSON:-ParseFile("http://rosettacode.org/mw/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=20&format=json"):
print_examples(x["query"]["categorymembers"]);
while(assigned(x["continue"]["cmcontinue"])) do
	continue := x["continue"]["cmcontinue"]:
	more_tasks:= cat("http://rosettacode.org/mw/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=20&format=json", "&continue=", x["continue"]["continue"], "&cmcontinue=", x["continue"]["cmcontinue"]):
	x := JSON:-ParseFile(more_tasks):
	print_examples(x["query"]["categorymembers"]);
end do:

```

{{Out|Output}}

```txt

100 doors has 267 examples
15 Puzzle Game has 42 examples
15 puzzle solver has 4 examples
2048 has 34 examples
24 game has 88 examples
24 game/Solve has 54 examples
4-rings or 4-squares puzzle has 33 examples
9 billion names of God the integer has 42 examples
99 Bottles of Beer has 291 examples
A+B has 224 examples
ABC Problem has 104 examples
Abstract type has 77 examples
Abundant, deficient and perfect number classifications has 69 examples
Accumulator factory has 97 examples
Ackermann function has 194 examples
Active Directory/Connect has 24 examples
Active Directory/Search for a user has 16 examples
Active object has 37 examples
Add a variable to a class instance at runtime has 51 examples
Address of a variable has 69 examples
AKS test for primes has 55 examples
Align columns has 97 examples
Aliquot sequence classifications has 32 examples
Almost prime has 58 examples
Amb has 35 examples
Amicable pairs has 69 examples
Anagrams has 99 examples
Anagrams/Deranged anagrams has 63 examples
Angle difference between two bearings has 24 examples
Animate a pendulum has 55 examples
Animation has 61 examples
Anonymous recursion has 86 examples
Append a record to the end of a text file has 36 examples
Apply a callback to an array has 144 examples
Apply a digital filter (direct form II transposed) has 14 examples
Arbitrary-precision integers (included) has 84 examples
Archimedean spiral has 40 examples
Arena storage pool has 25 examples
Arithmetic evaluation has 51 examples
Arithmetic-geometric mean has 93 examples
Arithmetic-geometric mean/Calculate Pi has 23 examples
Arithmetic/Complex has 101 examples
Arithmetic/Integer has 176 examples
Arithmetic/Rational has 59 examples
Array concatenation has 159 examples
Array length has 124 examples
Arrays has 188 examples
Assertions has 96 examples
Associative array/Creation has 128 examples
Associative array/Iteration has 107 examples
Atomic updates has 38 examples
Average loop length has 34 examples
Averages/Arithmetic mean has 168 examples
Averages/Mean angle has 63 examples
Averages/Mean time of day has 50 examples
Averages/Median has 103 examples
Averages/Mode has 76 examples
Averages/Pythagorean means has 96 examples
Averages/Root mean square has 111 examples
Averages/Simple moving average has 78 examples
AVL tree has 18 examples
Babbage problem has 80 examples
Balanced brackets has 112 examples
Balanced ternary has 34 examples
Barnsley fern has 39 examples
Benford's law has 58 examples
Bernoulli numbers has 43 examples
...
Variable size/Set has 37 examples
Variable-length quantity has 35 examples
Variables has 109 examples
Variadic function has 94 examples
Vector products has 77 examples
Verify distribution uniformity/Chi-squared test has 21 examples
Verify distribution uniformity/Naive has 40 examples
Video display modes has 13 examples
Vigenère cipher has 0 examples
Vigenère cipher/Cryptanalysis has 0 examples
Visualize a tree has 35 examples
Vogel's approximation method has 12 examples
Voronoi diagram has 32 examples
Walk a directory/Non-recursively has 84 examples
Walk a directory/Recursively has 75 examples
Water collected between towers has 28 examples
Web scraping has 75 examples
Window creation has 81 examples
Window creation/X11 has 23 examples
Window management has 19 examples
Wireworld has 47 examples
Word search has 9 examples
Word wrap has 63 examples
World Cup group stage has 14 examples
Write entire file has 53 examples
Write float arrays to a text file has 64 examples
Write language name in 3D ASCII has 59 examples
Write to Windows event log has 23 examples
Xiaolin Wu's line algorithm has 25 examples
XML/DOM serialization has 45 examples
XML/Input has 68 examples
XML/Output has 55 examples
XML/XPath has 52 examples
Y combinator has 81 examples
Yahoo! search interface has 19 examples
Yin and yang has 53 examples
Zebra puzzle has 39 examples
Zeckendorf arithmetic has 10 examples
Zeckendorf number representation has 57 examples
Zero to the zero power has 94 examples
Zhang-Suen thinning algorithm has 26 examples
Zig-zag matrix has 92 examples

```



## Mathematica


```Mathematica
TaskList = Flatten[
   Import["http://rosettacode.org/wiki/Category:Programming_Tasks", "Data"][[1, 1]]];

Print["Task \"", StringReplace[#, "_" -> " "], "\" has ",
  Length@Select[Import["http://rosettacode.org/wiki/" <> #, "Data"][[1,2]],
  StringFreeQ[#, __ ~~ "Programming Task" | __ ~~ "Omit"]& ], " example(s)"]&
  ~Map~ StringReplace[TaskList, " " -> "_"]
```

returns:

```txt
Task "100 doors" has 143 example(s)
Task "24 game" has 55 example(s)
Task "24 game/Solve" has 35 example(s)
...
```


=={{header|MATLAB}} / {{header|Octave}}==

The function count_examples() need to be saved in a file count_examples.m and its directory need to be included in the path.

```MATLAB
  function c = count_examples(url)
    c = 0;
    [s, success] = urlread (url);
    if ~success, return; end;
    c = length(strfind(s,'<h2><span class='));
  end;

  % script
  s   = urlread ('http://rosettacode.org/wiki/Category:Programming_Tasks');
  pat = '<li><a href="/wiki/';
  ix  = strfind(s,pat)+length(pat)-6;
  for k = 1:length(ix);
     % look through all tasks
     e = find(s(ix(k):end)==34,1)-2;
     t = s(ix(k)+[0:e]);    % task
     c = count_examples(['http://rosettacode.org',t]);
     printf('Task "%s" has %i examples.\n',t(7:end), c);
  end;
```


Output:

```txt

  Task "100_doors" has 137 examples.
  Task "24_game" has 45 examples.
  Task "24_game/Solve" has 28 examples.
  Task "99_Bottles_of_Beer" has 156 examples.
  Task "A%2BB" has 105 examples.
  ...
```



## Nim


```nim
import httpclient, strutils, xmldom, xmldomparser, cgi

proc count(s, sub): int =
  var i = 0
  while true:
    i = s.find(sub, i)
    if i < 0:
      break
    inc i
    inc result

const
  mainSite = "http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml"
  subSite = "http://www.rosettacode.org/w/index.php?title=$#&action=raw"

var sum = 0

for i in getContent(mainSite).loadXML().getElementsByTagName("cm"):
  let t = PElement(i).getAttribute("title").replace(" ", "_")
  let c = getContent(subSite % encodeUrl(t)).toLower().count("{{header|")
  echo t.replace("_", " "),": ",c," examples."
  sum += c

echo "\nTotal: ",sum," examples."
```

Output:

```txt
100 doors: 194 examples.
24 game: 68 examples.
24 game/Solve: 40 examples.
9 billion names of God the integer: 20 examples.
99 Bottles of Beer: 225 examples.
A+B: 159 examples.
ABC Problem: 42 examples.
Abstract type: 60 examples.
Accumulator factory: 78 examples.
Ackermann function: 151 examples.
Active Directory/Connect: 16 examples.
[...]
```



## Objeck


```objeck
use HTTP;
use XML;

class RosettaCount {
  function : Main(args : String[]) ~ Nil {
    taks_xml := HttpGet("http://rosettacode.org/mw/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml");
    parser := XmlParser->New(taks_xml);
    if(parser->Parse()) {
      task_names := parser->FindElements("/api/query/categorymembers/cm");
      if(task_names <> Nil) {
        each(i : task_names) {
          task_name := task_names->Get(i)->As(XmlElement)->GetAttribute("title")->GetValue();
          task_url := "http://rosettacode.org/mw/index.php?title=";
          task_url->Append(task_name);
          task_url->Append("&action=raw");

          task := HttpGet(task_url);
          counts := task->FindAll("=={{header|");
          if(counts->Size() > 0) {
            IO.Console->Print(UrlUtility->Decode(task_name))->Print(": ")->PrintLine(counts->Size());
          };
        };
      };
    };
  }

  function : HttpGet(url : String) ~ String {
    xml := "";

    client := HttpClient->New();
    lines := client->Get(url);
    each(i : lines) {
      xml->Append(lines->Get(i)->As(String));
    };

    return xml;
  }
}
```


Output:
```txt
Amb: 28
Anagrams: 71
Animation: 42
Arithmetic/Complex: 80
Arithmetic/Integer: 136
Arithmetic/Rational: 45
Arrays: 131
Assertions: 75
Averages/Median: 82
Averages/Mode: 61
Bitmap: 58
Bitmap/Histogram: 22
Calendar: 30
Catamorphism: 27
Classes: 77
Collections: 59
...
```



## OCaml


{{libheader|ocamlnet}}

{{libheader|xml-light}}

execute with:
 ocaml str.cma unix.cma  -I +pcre pcre.cma  -I +netsys netsys.cma  -I +equeue equeue.cma \
   -I +netstring netstring.cma  -I +netclient netclient.cma  -I +xml-light xml-light.cma  countex.ml

or with the '''findlib''' package one can compile with:
 ocamlfind opt -linkpkg -package str,unix,xml-light,netclient  countex.ml -o countex.opt


```ocaml
open Http_client.Convenience


let repl_quote s =
  let reg = Str.regexp_string "&#039;" in
  (Str.global_replace reg "%27" s)

let repl_space s =
  let s = String.copy s in
  for i = 0 to pred(String.length s) do
    if s.[i] = ' ' then s.[i] <- '_'
  done;
  (s)
(* or in OCaml 4.00+:
   let repl_space = String.map (fun c -> if c = ' ' then '_' else c)
*)

let count_ex s =
  let pat = Str.regexp_string "=={{header|" in
  let rec aux n p =
    try
      let p = Str.search_forward pat s p in
      aux (n+1) (p+1)
    with Not_found -> (n)
  in
  aux 0 0

let get_child child xml =
  let child =
    List.find
      (function Xml.Element (tag,_,_) when tag = child -> true | _ -> false) xml
  in
  Xml.children child

let () =
  let url = "http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&\
               cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml" in

  let xml = Xml.parse_string (http_get url) in

  let total = ref 0 in
  at_exit (fun () -> Printf.printf "\n Total: %d\n" !total);

  let f = function
  | Xml.Element ("cm", attrs, _) ->
      (try
        let _title = List.assoc "title" attrs in
        let title = repl_quote (repl_space _title) in
        let url = "http://www.rosettacode.org/w/index.php?title="^ title ^"&action=raw" in
        let n = count_ex (http_get url) in
        Printf.printf "%s: %d\n%!" _title n;
        total := n + !total;
      with Http_client.Http_error (404, _) -> ())
  | _ -> ()
  in

  match xml with
  | Xml.Element ("api", _, ch) ->
      let query = get_child "query" ch in
      let catmb = get_child "categorymembers" query in
      List.iter f catmb
  | _ -> ()
```


outputs:


```txt

100 doors: 56
24 game: 11
24 game Player: 9
99 Bottles of Beer: 73
Abstract type: 23
Ackermann Function: 61

...

XML Reading: 22
XML and XPath: 18
Xiaolin Wu&#039;s line algorithm: 3
Y combinator: 23
Yuletide Holiday: 32
Zig Zag: 29

 Total: 9106

```



## Oz

{{libheader|OzHttpClient}}


```oz
declare
  [HTTPClient] = {Module.link ['x-ozlib://mesaros/net/HTTPClient.ozf']}
  [XMLParser] = {Module.link ['x-oz://system/xml/Parser.ozf']}
  [StringX] = {Module.link ['x-oz://system/String.ozf']}
  [Regex] = {Module.link ['x-oz://contrib/regex']}

  AllTasksUrl = "http://rosettacode.org/mw/api.php?action=query&list="#
  "categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml"

  proc {Main}
     AllTasks = {Parse {GetPage AllTasksUrl}}
     TaskTitles = {GetTitles AllTasks}
     Total = {NewCell 0}
  in
     for Task in TaskTitles do
        TaskPage = {GetPage {TaskUrl Task}}
        RE = {Regex.compile "{{header\\|" [extended newline icase]}
        NumMatches = {Length {Regex.allMatches RE TaskPage}}
     in
        {System.showInfo Task#": "#NumMatches#" examples."}
        Total := @Total + NumMatches
     end
     {System.showInfo "Total: "#@Total#" examples."}
  end

  fun {TaskUrl Task}
     "http://rosettacode.org/mw/index.php?"#
     "title="#{PercentEncode {StringX.replace Task " " "_"}}#
     "&action=raw"
  end

  %% GetPage
  local
     Client = {New HTTPClient.urlGET init(inPrms(toFile:false toStrm:true) _)}
  in
     fun {GetPage RawUrl}
        Url = {VirtualString.toString RawUrl}
        OutParams
        HttpResponseParams
     in
        {Client getService(Url ?OutParams ?HttpResponseParams)}
        OutParams.sOut
     end
  end

  %% Parse
  local
     Parser = {New XMLParser.parser init}
  in
     fun {Parse Xs} {Parser parseVS(Xs $)} end
  end

  fun {GetTitles Doc}
     CMs = Doc.2.1.children.1.children.1.children
     fun {Attributes element(attributes:As ...)} As end
     fun {IsTitle attribute(name:N ...)} N == title end
  in
     {Map {Filter {Flatten {Map CMs Attributes}} IsTitle}
      fun {$ A} {Atom.toString A.value} end}
  end

  fun {PercentEncode Xs}
     case Xs of nil then nil
     [] X|Xr then
        if {Char.isDigit X} orelse {Member X [&- &_ &.  &~]}
           orelse X >= &a andthen X =< &z
           orelse X >= &z andthen X =< &Z then
           X|{PercentEncode Xr}
        else
           {Append &%|{ToHex2 X} {PercentEncode Xr}}
        end
     end
  end

  fun {ToHex2 X}
     [{ToHex1 X div 16} {ToHex1 X mod 16}]
  end

  fun {ToHex1 X}
     if X >= 0 andthen X =< 9 then &0 + X
     elseif X >= 10 andthen X =< 15 then &A + X - 10
     end
  end
in
  {Main}
```


Example output:

```txt

100 doors: 86 examples.
24 game: 22 examples.
24 game/Solve: 15 examples.
99 Bottles of Beer: 108 examples.
A+B: 59 examples.
...
Xiaolin Wu's line algorithm: 5 examples.
Y combinator: 29 examples.
Yahoo! Search: 10 examples.
Zig-zag matrix: 43 examples.
Total: 14099 examples.

```



## Perl

{{libheader|HTTP:Tiny}}

```Perl
use HTTP::Tiny;

my $site = "http://rosettacode.org";
my $list_url = "/mw/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml";

my $response = HTTP::Tiny->new->get("$site$list_url");
for ($response->{content} =~ /cm.*?title="(.*?)"/g) {
    (my $slug = $_) =~ tr/ /_/;
    my $response = HTTP::Tiny->new->get("$site/wiki/$slug");
    my $count = () = $response->{content} =~ /toclevel-1/g;
    print "$_: $count examples\n";
}
```


{{libheader|Mojolicious}}

```Perl
use v5.10;
use Mojo::UserAgent;

my $site = "http://rosettacode.org";
my $list_url = "/mw/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml";

my $ua = Mojo::UserAgent->new;
$ua->get("$site$list_url")->res->dom->find('cm')->each(sub {
    (my $slug = $_->{title}) =~ tr/ /_/;
    my $count = $ua->get("$site/wiki/$slug")->res->dom->find("#toc .toclevel-1")->size;
    say "$_->{title}: $count examples";
});
```



## Perl 6

{{works with|Rakudo|2018.12}}
Retrieves counts for both Tasks and Draft Tasks. Save / Display results as a sortable wikitable rather than a static list. Click on a column header to sort on that column. To do a secondary sort, hold down the shift key and click on a second column header. Tasks have a gray (default) background, Draft Tasks have a yellow background.

For a full output, see [[Rosetta Code/Count examples/Full list]]

```perl6
use HTTP::UserAgent;
use URI::Escape;
use JSON::Fast;

unit sub MAIN ( Str $no-fetch = '' );

# Friendlier descriptions for task categories
my %cat = (
    'Programming_Tasks' => 'Task',
    'Draft_Programming_Tasks' => 'Draft'
);

my $client = HTTP::UserAgent.new;

my $url = 'http://rosettacode.org/mw';

my $hashfile  = './RC_Task_count.json';
my $tablefile = './RC_Task_count.txt';

my %tasks;

# clear screen
run($*DISTRO.is-win ?? 'cls' !! 'clear');

my %counts =
    mediawiki-query(
        $url, 'pages',
        :generator<categorymembers>,
        :gcmtitle<Category:Programming Languages>,
        :gcmlimit<350>,
        :rawcontinue(),
        :prop<categoryinfo>
    )
    .map({ .<title>.subst(/^'Category:'/, '') => .<categoryinfo><pages> || 0 });

my @top10 = %counts.sort( -*.value ).head(10).map: *.key;

# dump a copy to STDOUT, mostly for debugging purposes
say '
```txt
Top ten programming languages by number of task examples completed:';
say "{('①' .. '⑩')[$_]} {@top10[$_]}" for ^@top10;
say "
```
\n";

unless $no-fetch {

    note 'Retrieving task information...';

    mkdir('./pages') unless './pages'.IO.e;

    for %cat.keys.sort -> $cat {
        mediawiki-query(
            $url, 'pages',
            :generator<categorymembers>,
            :gcmtitle("Category:$cat"),
            :gcmlimit<350>,
            :rawcontinue(),
            :prop<title>
        ).map({
            my $page =
              $client.get("{ $url }/index.php?title={ uri-escape .<title> }&action=raw").content;
            "./pages/{ uri-escape .<title>.subst(/' '/, '_', :g) } ".IO.spurt($page);
            my $lc = $page.lc;
            my $count = +$lc.comb(/ ^^'==' <-[\n=]>* '{{header|' <-[}]>+? '}}==' \h* $$ /);
            %tasks{.<title>} = {'cat' => %cat{$cat}, :$count};
            %tasks{.<title>}<top10> = (^@top10).map( {
                $lc.contains("\n==\{\{header|{@top10[$_].lc}}}==") ??
                ('①' .. '⑩')[$_] !! ' '
            } ).join;
            print clear, 1 + $++, ' ', %cat{$cat}, ' ', .<title>;
        })
    }

    print clear;

    note "\nTask information saved to local file: {$hashfile.IO.absolute}";
    $hashfile.IO.spurt(%tasks.&to-json);

}

# Load information from local file
%tasks = $hashfile.IO.e ?? $hashfile.IO.slurp.&from-json !! ( );

# Convert saved task info to a table
note "\nBuilding table...";
my $count    = +%tasks;
my $taskcnt  = +%tasks.grep: *.value.<cat> eq %cat<Programming_Tasks>;
my $draftcnt = $count - $taskcnt;
my $total    = sum %tasks{*}»<count>;

# Dump table to a file
my $out = open($tablefile, :w)  or die "$!\n";

$out.say: '
```txt
Top ten programming languages by number of task examples completed:';
$out.say: "{('①' .. '⑩')[$_]} {@top10[$_]}" for ^@top10;
$out.say: "
```
\n\n<div style=\"height:40em;overflow:scroll;\">";

# Add table boilerplate and caption
$out.say:
    '{|class="wikitable sortable"', "\n",
    "|+ As of { DateTime.new(time) } :: Tasks: { $taskcnt } ::<span style=\"background-color:#ffd\"> Draft Tasks:",
    "{ $draftcnt } </span>:: Total Tasks: { $count } :: Total Examples: { $total }\n",
    "!Count!!Task!!{('①' .. '⑩').join('!!')}"
;

# Sort tasks by count then add row
for %tasks.sort: { [-.value<count>, .key] } -> $task {
    $out.say:
      ( $task.value<cat> eq 'Draft'
        ?? "|- style=\"background-color: #ffc\"\n"
        !! "|-\n"
      ),
      "| { $task.value<count> }\n",
      ( $task.key ~~ /\d/
        ?? "|data-sort-value=\"{ $task.key.&naturally }\"| [[{uri-escape $task.key}|{$task.key}]]\n"
        !! "| [[{uri-escape $task.key}|{$task.key}]]\n"
      ),
      "|{ $task.value<top10>.comb.join('||') }"
}

$out.say( "|}\n</div>" );
$out.close;

note "Table file saved as: {$tablefile.IO.absolute}";

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

sub uri-query-string (*%fields) { %fields.map({ "{.key}={uri-escape .value}" }).join("&") }

sub naturally ($a) { $a.lc.subst(/(\d+)/, ->$/ {0~(65+$0.chars).chr~$0},:g) }

sub clear { "\r" ~ ' ' x 116 ~ "\r" }
```


{{out|Abridged output}}

```txt
Top ten programming language by number of task examples completed:
① Go
② Perl 6
③ Kotlin
④ Python
⑤ Phix
⑥ Racket
⑦ Perl
⑧ C
⑨ Julia
⑩ Tcl

```


<div style="height:40em;overflow:scroll;">
{|class="wikitable sortable"
|+ As of 2019-01-21T22:41:28Z :: Tasks: 924 ::<span style="background-color:#ffd"> Draft Tasks:215 </span>:: Total Tasks: 1139 :: Total Examples: 59984
!Count!!Task!!①!!②!!③!!④!!⑤!!⑥!!⑦!!⑧!!⑨!!⑩
|-
| 405
| [[Hello%20world%2FText|Hello world/Text]]
|①||②||③||④||⑤||⑥||⑦||⑧||⑨||⑩
|-
| 283
|data-sort-value="0C99 bottles of beer"| [[99%20Bottles%20of%20Beer|99 Bottles of Beer]]
|①||②||③||④||⑤||⑥||⑦||⑧||⑨||⑩
|-
| 276
| [[FizzBuzz|FizzBuzz]]
|①||②||③||④||⑤||⑥||⑦||⑧||⑨||⑩
|-
| 273
|data-sort-value="0D100 doors"| [[100%20doors|100 doors]]
|①||②||③||④||⑤||⑥||⑦||⑧||⑨||⑩
|-
| 251
| [[Fibonacci%20sequence|Fibonacci sequence]]
|①||②||③||④||⑤||⑥||⑦||⑧||⑨||⑩
|-
| 246
| [[Comments|Comments]]
|①||②||③||④||⑤||⑥||⑦||⑧||⑨||⑩
|-
| 239
| [[Factorial|Factorial]]
|①||②||③||④||⑤||⑥||⑦||⑧||⑨||⑩
|-
| 223
| [[Empty%20program|Empty program]]
|①||②||③||④||⑤||⑥||⑦||⑧||⑨||⑩
|-
| 222
| [[A%2BB|A+B]]
|①||②||③||④||⑤||⑥||⑦||⑧||⑨||⑩
|-
| 214
| [[Function%20definition|Function definition]]
|①||②||③||④||⑤||⑥||⑦||⑧||⑨||⑩
|}
</div>


## Phix

Counts no of "{{header|" (nb not "=={{header|") via web api (but gets tasks via scraping).

Since downloading all the pages can be very slow, this uses a cache.

Limiting (notdone) by "Phix" fairly obviously speeds it up tenfold :-)

```Phix
-- demo\rosetta\Count_examples.exw
constant include_drafts = true,
         sort_by_count = false,
--       notlang = "Phix" -- or "" (ie a zero length string) for all
         notlang = ""

integer lp = 0
procedure progress(string msg, sequence args = {})
    if length(args) then msg = sprintf(msg,args) end if
    integer lm = length(msg)
    if lm<lp then msg[$..$] = repeat(' ',lp-lm)&msg[$] end if
    puts(1,msg)
    lp = iff(msg[$]='\r'?lm:0)
end procedure

include builtins\timedate.e
integer refresh_cache = timedelta(days:=365) -- 0 for always
--integer refresh_cache = timedelta(days:=1) -- 0 for always

include builtins\libcurl.e
atom curl = NULL
atom pErrorBuffer

function write_callback(atom pData, integer size, integer nmemb, integer fn)
    integer bytes_written = size * nmemb
    puts(fn,peek({pData,bytes_written}))
    return bytes_written
end function
constant write_cb = call_back({'+', routine_id("write_callback")})

function open_download(string filename, url)
    bool refetch = true
    if get_file_type("rc_cache")!=FILETYPE_DIRECTORY then
        if not create_directory("rc_cache") then
            crash("cannot create rc_cache directory")
        end if
    end if
    filename = join_path({"rc_cache",filename})
    if file_exists(filename) then
        -- use existing file if <= refresh_cache (365 days) old
        sequence last_mod = get_file_date(filename)     -- (0.8.1+)
        atom delta = timedate_diff(last_mod,date())
        refetch = (delta>refresh_cache) or get_file_size(filename)=0
    else
        string directory = get_file_path(filename)
        if get_file_type(directory)!=FILETYPE_DIRECTORY then
            if not create_directory(directory,make_parent:=true) then
                crash("cannot create %s directory",{directory})
            end if
        end if
    end if
    object text
    if not refetch then
        text = trim(get_text(filename))
        refetch = (not sequence(text)) or (length(text)<10)
    end if
    if refetch then
        progress("Downloading %s...\r",{filename})
        if curl=NULL then
            curl_global_init()
            curl = curl_easy_init()
            pErrorBuffer = allocate(CURL_ERROR_SIZE)
            curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, pErrorBuffer)
            curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_cb)
        end if
        url = substitute(url,"%3A",":")
        url = substitute(url,"%2A","*")
        curl_easy_setopt(curl, CURLOPT_URL, url)
        integer fn = open(filename,"wb")
        if fn=-1 then ?9/0 end if
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fn)
        while true do
            CURLcode res = curl_easy_perform(curl)
            if res=CURLE_OK then exit end if
            string error = sprintf("%d",res)
            if res=CURLE_COULDNT_RESOLVE_HOST then
                error &= " [CURLE_COULDNT_RESOLVE_HOST]"
            end if
            progress("Error %s downloading file, retry?(Y/N):",{error})
            if lower(wait_key())!='y' then abort(0) end if
            printf(1,"Y\n")
        end while
        close(fn)
        refresh_cache += timedelta(days:=1) -- did I mention it is slow?
        text = get_text(filename)
    end if
    return text
end function

function open_category(string filename)
    return open_download(filename&".htm","http://rosettacode.org/wiki/Category:"&filename)
end function

function dewiki(string s)
    -- extract tasks from eg `<li><a href="/wiki/100_doors"`
    sequence tasks = {}
    integer start = 1, finish = match(`<div class="printfooter">`,s)
    s = s[1..finish-1]
    while true do
        start = match("<li><a href=\"/wiki/",s,start)
        if start=0 then exit end if
        start += length("<li><a href=\"/wiki/")
        finish = find('"',s,start)
        string task = s[start..finish-1]
        task = substitute_all(task,{"*",":"},{"%2A","%3A"})
        tasks = append(tasks,task)
        start = finish+1
    end while
    return tasks
end function

constant {hex,ascii} = columnize({{"%2A","*"},
                                  {"%3A",":"},
                                  {"%27","'"},
                                  {"%2B","+"},
                                  {"%22","\""},
                                  {"%E2%80%93","-"},
                                  {"%E2%80%99","'"},
                                  {"%C3%A8","e"},
                                  {"%C3%A9","e"}})

function html_clean(string s)
    return substitute_all(s,hex,ascii)
end function

function count_tasks()
    -- note this lot use web scraping (as cribbed from a similar task) ...
    sequence tasks = dewiki(open_category("Programming_Tasks"))
    if include_drafts then
        tasks &= dewiki(open_category("Draft_Programming_Tasks"))
        tasks = sort(tasks)
    end if
    if length(notlang) then
        -- filter already done in specified language
        string langurl = "http://rosettacode.org/wiki/Category:"&notlang
        sequence done = dewiki(open_download(notlang&".htm",langurl))
        integer k = 0
        for i=1 to length(tasks) do
            if not find(tasks[i],done) then
                k += 1
                tasks[k] = tasks[i]
            end if
        end for
        tasks = tasks[1..k]
        done = {}
    end if
    progress("%d tasks found\n",{length(tasks)})
    -- ... whereas the individual tasks use the web api instead (3x smaller/faster)
    integer total_count = 0
    sequence task_counts = repeat(0,length(tasks))
    for i=1 to length(tasks) do
        string ti = tasks[i],
               url = sprintf("http://rosettacode.org/mw/index.php?title=%s&action=raw",{ti}),
               contents = open_download(ti&".raw",url),
               prev = "", this
        integer count = 0, start = 1
        while true do
            start = match(`{{header|`,contents,start)
            if start=0 then exit end if
            --
            -- skip duplicates/we also have to cope with eg
            --
## Python
                                    \
            -- ==={{header|Python}} Original===                          } count
            -- ==={{header|Python}} Succinct===                          }  once
            -- ==={{header|Python}} Recursive ===                       /
            -- =={{header|Mathematica}} / {{header|Wolfram Language}}== \
            -- =={{header|Icon}} and {{header|Unicon}}==                 } count
            -- == {{header|Icon}} and {{header|Unicon}} ==              /   both
            -- == {{header|Java}}==
            -- etc. Note however that this /does/ count eg
            -- =
## Applesoft BASIC
=                         \
            -- =
## BASIC256
=                                 } count
            -- =
## Commodore BASIC
=                          }  'em
            -- ==={{header|IS-BASIC}}===                                 }  all
            -- =
## Sinclair ZX81 BASIC
=                     /
            --
            this = contents[start..match(`}}`,contents,start+1)]
            if this!=prev then
                count += 1
            end if
            prev = this
            start += length(`{{header|`)
        end while
        if sort_by_count then
            task_counts[i] = count
        elsif length(notlang) or i<=2 or i>=length(tasks)-1 or mod(i,200)=0 then
            progress("%s %d\n",{html_clean(ti),count})
        end if
        total_count += count
        if get_key()=#1B then progress("escape keyed\n") exit end if
    end for
    if curl!=NULL then
        curl_easy_cleanup(curl)
        free(pErrorBuffer)
        curl = NULL
        pErrorBuffer = NULL
    end if
    if sort_by_count then
        sequence tags = custom_sort(task_counts,tagset(length(tasks)))
        for i=length(tags) to 1 by -1 do
            integer ti = tags[i]
            progress("%s %d\n",{html_clean(tasks[ti]),task_counts[ti]})
        end for
    end if
    return total_count
end function

progress("Total: %d\n",{count_tasks()})
```

{{out}} (as of 24/7/19, showing first two, every 200th, and last two)

```txt

1175 tasks found
100_doors 294
15_Puzzle_Game 55
Compiler/virtual_machine_interpreter 14
General_FizzBuzz 51
Maximum_triangle_path_sum 46
Random_number_generator_(included) 89
String_concatenation 155
Zhang-Suen_thinning_algorithm 29
Zig-zag_matrix 99
Total: 64405

```



## PicoLisp


```PicoLisp
(load "@lib/http.l")

(client "rosettacode.org" 80
   "mw/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml"
   (while (from " title=\"")
      (let Task (till "\"")
         (client "rosettacode.org" 80 (pack "wiki/" (replace Task " " "_"))
            (let Cnt 0
               (while (from "<span class=\"tocnumber\">")
                  (unless (sub? "." (till "<" T))
                     (inc 'Cnt) ) )
               (out NIL (prinl (ht:Pack Task) ": " Cnt)) ) ) ) ) )
```

Output (05may10):

```txt
100 doors: 79
24 game: 21
24 game/Solve: 15
99 Bottles of Beer: 95
A+B: 37
Abstract type: 29
...
```



## PureBasic


```PureBasic
Procedure handleError(value, msg.s)
  If value = 0
    MessageRequester("Error", msg)
    End
  EndIf
EndProcedure

handleError(InitNetwork(), "Unable to initialize network functions.")
If OpenConsole()
  Define url$, x1$, y1$, title$, unescapedTitle$, encodedURL$
  Define x2, i, j, totalExamples, totalTasks
  url$ = "http://www.rosettacode.org/mw/api.php?action=query" +
         "&list=categorymembers&cmtitle=Category:Programming_Tasks" +
         "&cmlimit=500&format=xml"

  Repeat
    handleError(ReceiveHTTPFile(url$, "tasks.xml"), "Unable to access tasks URL.")

    handleError(ReadFile(0, "tasks.xml"), "Unable to read 'task.xml' file.")
    x1$ =  ReadString(0)
    CloseFile(0)

    Repeat
      x2 = FindString(x1$, "title=", x2 + 1)
      If x2
        title$ = Mid(x1$, x2 + 7, 99)
        title$ = Left(title$, FindString(title$, ">", 1) - 4)
        unescapedTitle$ = UnescapeString(ReplaceString(title$, "&#039;", "&apos;"), #PB_String_EscapeXML)
        encodedURL$ = URLEncoder("http://www.rosettacode.org/mw/index.php?title=" + unescapedTitle$ + "&action=raw")
        If ReceiveHTTPFile(encodedURL$, "task.xml")
          ReadFile(0, "task.xml")
          While Not Eof(0)
            y1$ =  ReadString(0)
            If FindString(y1$, "=={{header|", 1, #PB_String_NoCase)
              totalExamples + 1
            EndIf
          Wend
          CloseFile(0)

          PrintN(unescapedTitle$ +": " + Str(totalExamples) + " examples")

          totalTasks + totalExamples
          totalExamples = 0
        EndIf
      EndIf
    Until x2 = 0

    ;check for additional pages of tasks
    x2 = FindString(x1$, "cmcontinue=")
    If x2
      i = FindString(x1$, #DQUOTE$, x2 + 1)
      j = FindString(x1$, #DQUOTE$, i + 1)
      url$ = URLEncoder("http://www.rosettacode.org/mw/api.php?action=query" +
                        "&list=categorymembers&cmtitle=Category:Programming_Tasks" +
                        "&cmlimit=500&format=xml&cmcontinue=" + Mid(x1$, i + 1, j - i))
    Else
      Break ;all done
    EndIf
  ForEver

  PrintN("Total: " + Str(totalTasks) + " examples")
  Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
100 doors: 224 examples
24 game: 78 examples
24 game/Solve: 46 examples
9 billion names of God the integer: 33 examples
99 Bottles of Beer: 253 examples
A+B: 187 examples
......
Zeckendorf arithmetic: 6 examples
Zeckendorf number representation: 37 examples
Zero to the zero power: 71 examples
Zhang-Suen thinning algorithm: 18 examples
Zig-zag matrix: 83 examples
Total: 44140 examples
```



## Python



```python
import urllib, xml.dom.minidom

x = urllib.urlopen("http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml")

tasks = []
for i in xml.dom.minidom.parseString(x.read()).getElementsByTagName("cm"):
    t = i.getAttribute('title').replace(" ", "_")
    y = urllib.urlopen("http://www.rosettacode.org/w/index.php?title=%s&action=raw" % t.encode('utf-8'))
    tasks.append( y.read().lower().count("{{header|") )
    print t.replace("_", " ") + ": %d examples." % tasks[-1]

print "\nTotal: %d examples." % sum(tasks)
```



## R

{{libheader|XML (R)}}

{{libheader|RCurl}}

```R

library(XML)
library(RCurl)
doc <- xmlInternalTreeParse("http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml")
nodes <- getNodeSet(doc,"//cm")
titles = as.character( sapply(nodes, xmlGetAttr, "title") )
headers <- list()
counts <- list()
for (i in 1:length(titles)){
	headers[[i]] <- getURL( paste("http://rosettacode.org/mw/index.php?title=", gsub(" ", "_", titles[i]), "&action=raw", sep="") )
	counts[[i]] <- strsplit(headers[[i]],split=" ")[[1]]
	counts[[i]] <- grep("\\{\\{header", counts[[i]])
	cat(titles[i], ":", length(counts[[i]]), "examples\n")
}
cat("Total: ", length(unlist(counts)), "examples\n")

```



## Racket



```racket

#lang racket

(require net/url net/uri-codec json (only-in racket/dict [dict-ref ref]))

(define (RC-get verb params)
  ((compose1 get-pure-port string->url format)
   "http://rosettacode.org/mw/~a.php?~a" verb (alist->form-urlencoded params)))

(define (get-category catname)
  (let loop ([c #f])
    (define t
      ((compose1 read-json RC-get) 'api
       `([action . "query"] [format . "json"]
         [list . "categorymembers"] [cmtitle . ,(format "Category:~a" catname)]
         [cmcontinue . ,(and c (ref c 'cmcontinue))] [cmlimit . "500"])))
    (define (c-m key) (ref (ref t key '()) 'categorymembers #f))
    (append (for/list ([page (c-m 'query)]) (ref page 'title))
            (cond [(c-m 'query-continue) => loop] [else '()]))))

(printf "Total: ~a\n"
  (for/sum ([task (get-category 'Programming_Tasks)])
    (define s ((compose1 length regexp-match-positions*)
               #rx"=={{" (RC-get 'index `([action . "raw"] [title . ,task]))))
    (printf "~a: ~a\n" task s)
    s))

```



## Ring


```ring

# Project: Rosetta Code/Count examples

load "stdlib.ring"
ros= download("http://rosettacode.org/wiki/Category:Programming_Tasks")
pos = 1
num = 0
totalros = 0
rosname = ""
rostitle = ""
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
see nl
see "Total: " + totalros + " examples." + nl

func searchstring(str,substr,n)
       newstr=right(str,len(str)-n+1)
       nr = substr(newstr, substr)
       if nr = 0
          return 0
       else
          return n + nr -1
       ok

func searchname(sn)
       nr2 = searchstring(ros,'">',sn)
       nr3 = searchstring(ros,"</a></li>",sn)
       rosname = substr(ros,nr2+2,nr3-nr2-2)
       return sn

func searchtitle(sn)
        st = searchstring(ros,"title=",sn)
        rostitle = substr(ros,sn+19,st-sn-21)
        rostitle = "rosettacode.org/wiki/" + rostitle
        rostitle = download(rostitle)
        sum = count(rostitle,"Edit section:")
        num = num + 1
        see "" + num + ". " + rosname + ": " + sum + " examples." + nl
        totalros = totalros + sum
        return sn

func count(cstring,dstring)
       sum = 0
       while substr(cstring,dstring) > 0
               sum = sum + 1
              cstring = substr(cstring,substr(cstring,dstring)+len(string(sum)))
       end
       return sum

```

Output:

```txt

1. 100 doors: 331 examples.
2. 15 Puzzle Game: 48 examples.
3. 15 puzzle solver: 18 examples.
4. 2048: 40 examples.
5. 24 game: 95 examples.
6. 24 game/Solve: 59 examples.
7. 4-rings or 4-squares puzzle: 42 examples.
8. 9 billion names of God the integer: 54 examples.
9. 99 Bottles of Beer: 393 examples.
10. A+B: 250 examples.
......
872. XML/XPath: 54 examples.
873. Y combinator: 88 examples.
874. Yahoo! search interface: 20 examples.
875. Yin and yang: 59 examples.
876. Zebra puzzle: 58 examples.
877. Zeckendorf arithmetic: 11 examples.
878. Zeckendorf number representation: 64 examples.
879. Zero to the zero power: 100 examples.
880. Zhang-Suen thinning algorithm: 26 examples.
881. Zig-zag matrix: 112 examples.

Total: 62677 examples.

```



## Ruby

{{libheader|REXML}}
First, a RosettaCode module, saved as <tt>rosettacode.rb</tt>:

```ruby
require 'open-uri'
require 'rexml/document'

module RosettaCode

  URL_ROOT = "http://rosettacode.org/mw"

  def self.get_url(page, query)
    begin
      # Ruby 1.9.2
      pstr = URI.encode_www_form_component(page)
      qstr = URI.encode_www_form(query)
    rescue NoMethodError
      require 'cgi'
      pstr = CGI.escape(page)
      qstr = query.map {|k,v|
        "%s=%s" % [CGI.escape(k.to_s), CGI.escape(v.to_s)]}.join("&")
    end
    url = "#{URL_ROOT}/#{pstr}?#{qstr}"
    p url if $DEBUG
    url
  end

  def self.get_api_url(query)
    get_url "api.php", query
  end

  def self.category_members(category)
    query = {
      "action" => "query",
      "list" => "categorymembers",
      "cmtitle" => "Category:#{category}",
      "format" => "xml",
      "cmlimit" => 500,
    }
    while true
      url = get_api_url query
      doc = REXML::Document.new open(url)

      REXML::XPath.each(doc, "//cm") do |task|
        yield task.attribute("title").value
      end

      continue = REXML::XPath.first(doc, "//query-continue")
      break if continue.nil?
      cm = REXML::XPath.first(continue, "categorymembers")
      query["cmcontinue"] = cm.attribute("cmcontinue").value
    end
  end

end
```


Then, we implement the task with:

```ruby
require 'rosettacode'

total_examples = 0

RosettaCode.category_members("Programming_Tasks") do |task|
  url = RosettaCode.get_url("index.php", {"action" => "raw", "title" => task})
  examples = open(url).read.scan("=={{header").length
  puts "#{task}: #{examples}"
  total_examples += examples
end

puts
puts "Total: #{total_examples}"
```



## Rust


```rust
extern crate reqwest;
extern crate url;
extern crate rustc_serialize;

use std::io::Read;
use self::url::Url;
use rustc_serialize::json::{self, Json};

pub struct Task {
    page_id: u64,
    pub title: String,
}

#[derive(Debug)]
enum ParseError {
    /// Something went wrong with the HTTP request to the API.
    Http(reqwest::Error),

    /// There was a problem parsing the API response into JSON.
    Json(json::ParserError),

    /// Unexpected JSON format from response
    UnexpectedFormat,
}
impl From<json::ParserError> for ParseError {
    fn from(error: json::ParserError) -> Self {
        ParseError::Json(error)
    }
}

impl From<reqwest::Error> for ParseError {
    fn from(error: reqwest::Error) -> Self {
        ParseError::Http(error)
    }
}


fn construct_query_category(category: &str) -> Url {
    let mut base_url = Url::parse("http://rosettacode.org/mw/api.php").unwrap();
    let cat = format!("Category:{}", category);
    let query_pairs = vec![("action", "query"),
                           ("format", "json"),
                           ("list", "categorymembers"),
                           ("cmlimit", "500"),
                           ("cmtitle", &cat),
                           ("continue", "")];
    base_url.query_pairs_mut().extend_pairs(query_pairs.into_iter());
    base_url
}

fn construct_query_task_content(task_id: &str) -> Url {
    let mut base_url = Url::parse("http://rosettacode.org/mw/api.php").unwrap();
    let mut query_pairs =
        vec![("action", "query"), ("format", "json"), ("prop", "revisions"), ("rvprop", "content")];
    query_pairs.push(("pageids", task_id));
    base_url.query_pairs_mut().extend_pairs(query_pairs.into_iter());
    base_url
}

fn query_api(url: Url) -> Result<Json, ParseError> {
    let mut response = try!(reqwest::get(url.as_str()));
    // Build JSON
    let mut body = String::new();
    response.read_to_string(&mut body).unwrap();

    Ok(try!(Json::from_str(&body)))
}

fn parse_all_tasks(reply: &Json) -> Result<Vec<Task>, ParseError> {
    let json_to_task = |json: &Json| -> Result<Task, ParseError> {
        let page_id: u64 = try!(json.find("pageid")
            .and_then(|id| id.as_u64())
            .ok_or(ParseError::UnexpectedFormat));
        let title: &str = try!(json.find("title")
            .and_then(|title| title.as_string())
            .ok_or(ParseError::UnexpectedFormat));

        Ok(Task {
            page_id: page_id,
            title: title.to_owned(),
        })
    };
    let tasks_json = try!(reply.find_path(&["query", "categorymembers"])
        .and_then(|tasks| tasks.as_array())
        .ok_or(ParseError::UnexpectedFormat));

    // Convert into own type
    tasks_json.iter().map(json_to_task).collect()
}
fn count_number_examples(task: &Json, task_id: u64) -> Result<u32, ParseError> {
    let revisions =
        try!(task.find_path(&["query", "pages", task_id.to_string().as_str(), "revisions"])
            .and_then(|content| content.as_array())
            .ok_or(ParseError::UnexpectedFormat));
    let content = try!(revisions[0]
        .find("*")
        .and_then(|content| content.as_string())
        .ok_or(ParseError::UnexpectedFormat));
    Ok(content.split("=={{header").count() as u32)
}

pub fn query_all_tasks() -> Vec<Task> {
    let query = construct_query_category("Programming_Tasks");
    let json: Json = query_api(query).unwrap();
    parse_all_tasks(&json).unwrap()
}

pub fn query_a_task(task: &Task) -> u32 {
    let query = construct_query_task_content(&task.page_id.to_string());
    let json: Json = query_api(query).unwrap();
    count_number_examples(&json, task.page_id).unwrap()
}
```


The function is then run using the following:

```rust

extern crate count_examples;
fn main() {
    let all_tasks = count_examples::query_all_tasks();
    for task in &all_tasks {
        let count = count_examples::query_a_task(task);
        println!("Task: {} has {} examples", task.title, count);
    }
}
```



## Run BASIC


```runbasic
html "<table border=1><tr bgcolor=wheat align=center><td>Num</td><td>Task</td><td>Examples</td></tr>"

a$	= httpGet$("http://rosettacode.org/wiki/Category:Programming_Tasks")
a$	= word$(a$,1,"</table></div>")
i	= instr(a$,"<a href=""/wiki/")
i	= instr(a$,"<a href=""/wiki/",i+1)
while i > 0
  count	= count + 1
  i	= instr(a$,"<a href=""/wiki/",i+1)
  j	= instr(a$,">",i+5)
  a1$	= mid$(a$,i+15,j-i)
  taskId$ = word$(a1$,1,"""")
  task$   = word$(a1$,3,"""")
  url$	= "http://rosettacode.org/wiki/";taskId$
  a2$	= httpGet$(url$)
  ii	= instr(a2$,"<span class=""tocnumber"">")
  jj	= 0
  while ii > 0
    jj	= ii
    ii	= instr(a2$,"<span class=""tocnumber"">",ii+10)
  wend
  if jj = 0 then
    examp = 0
   else
    kk	= instr(a2$,"<",jj+24)
    examp = int(val(mid$(a2$,jj+24,kk-jj-24)))
  end if
  html "<tr><td align=right>";count;"</td><td>";task$;"</td><td align=right>";examp;"</td></tr>"
  totExamp = totExamp + examp
wend
html "<tr bgcolor=wheat><td>**</td><td>** Total **</td><td align=right>";totExamp;"</td></tr></table>"
end
```

<table border=1><tr bgcolor=wheat align=center><td>Num</td><td>Task</td><td>Examples</td></tr>
<tr><td align=right>1</td><td>100 doors</td><td align=right>165</td></tr>
<tr><td align=right>2</td><td>24 game</td><td align=right>56</td></tr>
<tr><td align=right>3</td><td>24 game/Solve</td><td align=right>34</td></tr>
<tr><td align=right>4</td><td>99 Bottles of Beer</td><td align=right>192</td></tr>
<tr><td align=right>5</td><td>A+B</td><td align=right>129</td></tr>
<tr><td align=right>6</td><td>Abstract type</td><td align=right>51</td></tr>
<tr><td align=right>7</td><td>Accumulator factory</td><td align=right>65</td></tr>
<tr><td align=right>8</td><td>Ackermann function</td><td align=right>132</td></tr>
<tr><td align=right>9</td><td>Active Directory/Connect</td><td align=right>13</td></tr>
<tr><td align=right>10</td><td>Active Directory/Search for a user</td><td align=right>13</td></tr>
<tr><td align=right>11</td><td>Active object</td><td align=right>22</td></tr>
<tr><td align=right>12</td><td>Add a variable to a class instance at runtime</td><td align=right>37</td></tr>
<tr><td align=right>...</td><td>...</td><td align=right>...</td></tr>
<tr><td align=right>655</td><td>Y combinator</td><td align=right>53</td></tr>
<tr><td align=right>656</td><td>Yahoo! search interface</td><td align=right>16</td></tr>
<tr><td align=right>657</td><td>Yin and yang</td><td align=right>38</td></tr>
<tr><td align=right>658</td><td>Zebra puzzle</td><td align=right>12</td></tr>
<tr><td align=right>659</td><td>Zeckendorf number representation</td><td align=right>18</td></tr>
<tr><td align=right>660</td><td>Zig-zag matrix</td><td align=right>65</td></tr>
<tr bgcolor=wheat><td>**</td><td>** Total **</td><td align=right>28611</td></tr></table>


## Scala

{{libheader|Scala}}
```Scala
import scala.language.postfixOps

object TaskCount extends App {
  import java.net.{ URL, URLEncoder }
  import scala.io.Source.fromURL

  System.setProperty("http.agent", "*")
  val allTasksURL =
    "http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml"
  val allTasks = xml.parsing.XhtmlParser(fromURL(new URL(allTasksURL)))

  val regexExpr = "(?i)==\\{\\{header\\|".r

  def oneTaskURL(title: String) = {
    println(s"Check $title")
    "http://www.rosettacode.org/w/index.php?title=%s&action=raw" format URLEncoder.encode(title.replace(' ', '_'), "UTF-8")
  }

  def count(title: String) = regexExpr findAllIn fromURL(new URL(oneTaskURL(title)))(io.Codec.UTF8).mkString length

  val counts = for (task <- allTasks \\ "cm" \\ "@title" map (_.text)) yield scala.actors.Futures.future((task, count(task)))

  counts map (_.apply) map Function.tupled("%s: %d examples." format (_, _)) foreach println
  println("\nTotal: %d examples." format (counts map (_.apply._2) sum))
}
```



## Sidef

{{trans|Perl}}

```ruby
var lwp = require('LWP::UserAgent').new(agent => 'Mozilla/5.0');

var site = 'http://rosettacode.org';
var list_url = '/mw/api.php?action=query&list=categorymembers&'+
               'cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml';

var content = lwp.get(site + list_url).decoded_content;

while (var m = content.match(/cm.*?title="(.*?)"/g)) {
    (var slug = m[0]).gsub!(' ', '_');
    var count = lwp.get("#{site}/wiki/#{slug}").decoded_content.count(/toclevel-1/g);
    say "#{m[0]}: #{count} examples";
}
```

{{out}}

```txt

100 doors: 278 examples
15 Puzzle Game: 50 examples
15 puzzle solver: 10 examples
2048: 41 examples
24 game: 91 examples
24 game/Solve: 56 examples
4-rings or 4-squares puzzle: 44 examples
9 billion names of God the integer: 48 examples
99 Bottles of Beer: 287 examples
A+B: 228 examples
...

```



## Tcl

{{tcllib|json}}

```tcl
package require Tcl 8.5
package require http
package require json

fconfigure stdout -buffering none

proc get_tasks {category} {
    set start [clock milliseconds]
    puts -nonewline "getting $category members..."
    set base_url http://www.rosettacode.org/w/api.php
    set query {action query list categorymembers cmtitle Category:%s format json cmlimit 500}
    set this_query [dict create {*}[split [format $query $category]]]
    set tasks [list]

    while {1} {
        set url [join [list $base_url [http::formatQuery {*}$this_query]] ?]
        set response [http::geturl $url]
        if {[set s [http::status $response]] ne "ok" || [http::ncode $response] != 200} {
            error "Oops: url=$url\nstatus=$s\nhttp code=[http::code $response]"
        }
        set data [json::json2dict [http::data $response]]
        http::cleanup $response

        # add tasks to list
        foreach task [dict get $data query categorymembers] {
            lappend tasks [dict get [dict create {*}$task] title]
        }

        if {[catch {dict get $data query-continue categorymembers cmcontinue} continue_task] != 0} {
            # no more continuations, we're done
            break
        }
        dict set this_query cmcontinue $continue_task
    }
    puts " found [llength $tasks] tasks in [expr {[clock milliseconds] - $start}] milliseconds"
    return $tasks
}

# This proc can be replaced by a single regexp command:
#     set count [regexp -all "***=$needle" $haystack]
# However this proc is more efficient -- we're dealing with plain strings only.
proc count_substrings {needle haystack} {
    set count 0
    set idx 0
    while {[set idx [string first $needle $haystack $idx]] != -1} {
        incr count
        incr idx
    }
    return $count
}

set total 0
foreach task [get_tasks Programming_Tasks] {
    set url [format "http://www.rosettacode.org/w/index.php?title=%s&action=raw" [string map {{ } _} $task]]
    set response [http::geturl $url]
    if {[set s [http::status $response]] ne "ok" || [http::ncode $response] != 200} {
        error "Oops: url=$url\nstatus=$s\nhttp code=[http::code $response]"
    }
    set count [count_substrings "\{\{header|" [http::data $response]]
    puts [format "%3d examples in %s" $count $task]
    http::cleanup $response
    incr total $count
}

puts "\nTotal: $total examples"
```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
url="http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml"
data=REQUEST (url)

BUILD S_TABLE beg=*
DATA :title=":
BUILD S_TABLE end=*
DATA :":

titles=EXTRACT (data,beg|,end,1,0,"~~")
titles=SPLIT (titles,":~~:")
sz_titles=SIZE (titles)

BUILD R_TABLE header=":==\{\{header|:"
all=*

ERROR/STOP CREATE ("tasks",seq-e,-std-)

COMPILE
LOOP title=titles
ask=*
ask      =SET_VALUE(ask,"title",title)
ask      =SET_VALUE(ask,"action","raw")
ask      =ENCODE (ask,cgi)
http     ="http://www.rosettacode.org/mw/index.php"
url      =CONCAT (http,"?",ask)
data     =REQUEST (url)
header   =FILTER_INDEX (data,header,-)
sz_header=SIZE(header)
line     =CONCAT (title,"=",sz_header," members")
FILE "tasks" = line
all      =APPEND(all,sz_header)
ENDLOOP

ENDCOMPILE
all =JOIN(all),sum=SUM(all),time=time()
line=CONCAT (time,": ", sz_titles, " Programing Tasks: ", sum, " solutions")

FILE "tasks" = line

```

Output in file "tasks":
<pre style='height:30ex;overflow:scroll'>
100 doors=104 members
24 game=27 members
24 game/Solve=21 members
99 Bottles of Beer=124 members
A+B=76 members
Abstract type=35 members
Accumulator factory=44 members
...
XML/Input=39 members
XML/Output=32 members
XML/XPath=24 members
Xiaolin Wu&#039;s line algorithm=0 members
Y combinator=33 members
Yahoo! search interface=10 members
Zig-zag matrix=46 members
2011-01-15 03:41:30: 455 Programing Tasks: 16009 solutions

```



## zkl

Uses shared libraries YAJL and cURL and handles "continue" responses.

```zkl
var [const] YAJL=Import("zklYAJL")[0], CURL=Import("zklCurl");

fcn getTasks(language){
   continueValue,tasks:="",Data(0,String);  // "nm\0nm\0...."
   do{
      page:=CURL().get(("http://rosettacode.org/mw/api.php?"
         "action=query&cmlimit=500"
	 "&format=json"
	 "&list=categorymembers"
	 "&cmtitle=Category:%s"
	 "&cmcontinue=%s").fmt(language,continueValue));
      page=page[0].del(0,page[1]);  // get rid of HTML header
      json:=YAJL().write(page).close();
      json["query"]["categorymembers"].pump(tasks,T("get","title"));
      continueValue=json.find("continue") //continue:-||,cmcontinue:page|954|19)
          .toList().apply("concat","=").concat("&");
   }while(continueValue);
   tasks
}
re:=RegExp(0'!\s+==\s*{{\s*header\s*|!);  // == {{ header | zkl
foreach task in (getTasks("Programming_Tasks")){
   page:=CURL().get(
      "http://www.rosettacode.org/mw/index.php?title=%s&action=raw"
      .fmt(CURL.urlEncode(task)));
   page=page[0].del(0,page[1]);  // get rid of HTML header
   cnt,n:=0,0;  while(re.search(page,True,n)){ cnt+=1; n=re.matched[0].sum(0); }
   "%4d: %s".fmt(cnt,task).println();
}
```

{{out}}

```txt

 229: 100 doors
  15: 15 Puzzle Game
...
 257: 99 Bottles of Beer
 199: A+B
...
  28: Calendar - for "REAL" programmers
...
   9: Stream Merge
...
  78: Zero to the zero power
  19: Zhang-Suen thinning algorithm
  83: Zig-zag matrix

```



{{omit from|Batch File}}
{{omit from|Brainfuck}}
{{omit from|Brlcad}}
{{omit from|GUISS}}
{{omit from|Lilypond}}
{{omit from|Maxima}}
{{omit from|Openscad}}
{{omit from|PostScript}}
{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have network access. -->
{{omit from|Yorick|Does not have network access.}}
{{omit from|ZX Spectrum Basic|Does not have network access.}}
