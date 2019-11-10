+++
title = "Rosetta Code/Find unimplemented tasks"
description = ""
date = 2019-05-10T14:25:51Z
aliases = []
[extra]
id = 3357
[taxonomies]
categories = []
tags = []
+++

{{task|Rosetta Code related}}

;Task:
Given the name of a language on Rosetta Code, find all tasks which are not implemented in that language.


Note: Implementations should allow for fetching more data than can be returned in one request to Rosetta Code.

You'll need to use the Media Wiki API, which you can find out about locally, [http://rosettacode.org/mw/api.php here], or in Media Wiki's API documentation at, [http://www.mediawiki.org/wiki/API_Query API:Query]





## Ada

{{libheader|AWS}}
Parsing XML with XMLAda from Adacore

```Ada
with Aws.Client, Aws.Messages, Aws.Response, Aws.Resources, Aws.Url;
with Dom.Readers, Dom.Core, Dom.Core.Documents, Dom.Core.Nodes, Dom.Core.Attrs;
with Input_Sources.Strings, Unicode, Unicode.Ces.Utf8;
with Ada.Strings.Unbounded, Ada.Text_IO, Ada.Command_Line;
with Ada.Containers.Vectors;

use  Aws.Client, Aws.Messages, Aws.Response, Aws.Resources, Aws.Url;
use Dom.Readers, Dom.Core, Dom.Core.Documents, Dom.Core.Nodes, Dom.Core.Attrs;
use Aws, Ada.Strings.Unbounded, Input_Sources.Strings;
use Ada.Text_IO, Ada.Command_Line;

procedure Not_Coded is

   package Members_Vectors is new Ada.Containers.Vectors (
      Index_Type => Positive,
      Element_Type => Unbounded_String);
   use Members_Vectors;

   All_Tasks, Language_Members : Vector;

   procedure Get_Vector (Category : in String; Mbr_Vector : in out Vector) is
      Reader : Tree_Reader;
      Doc    : Document;
      List   : Node_List;
      N      : Node;
      A      : Attr;
      Page   : Aws.Response.Data;
      S      : Messages.Status_Code;

      -- Query has cmlimit value of 100, so we need 5 calls to
      -- retrieve the complete list of Programming_category
      Uri_Xml    : constant String  :=
         "http://rosettacode.org/mw/api.php?action=query&list=categorymembers"
         &
         "&format=xml&cmlimit=100&cmtitle=Category:";
      Cmcontinue : Unbounded_String := Null_Unbounded_String;
   begin
      loop
         Page :=
            Aws.Client.Get (Uri_Xml & Category & (To_String (Cmcontinue)));
         S    := Response.Status_Code (Page);
         if S not  in Messages.Success then
            Put_Line
              ("Unable to retrieve data => Status Code :" &
               Image (S) &
               " Reason :" &
               Reason_Phrase (S));
            raise Connection_Error;
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
            Members_Vectors.Append
              (Mbr_Vector,
               To_Unbounded_String (Value (A)));
         end loop;
         Free (List);
         List := Get_Elements_By_Tag_Name (Doc, "query-continue");
         if Length (List) = 0 then
            -- we are done
            Free (List);
            Free (Reader);
            exit;
         end if;
         N          := First_Child (Item (List, 0));
         A          := Get_Named_Item (Attributes (N), "cmcontinue");
         Cmcontinue :=
            To_Unbounded_String
              ("&cmcontinue=" & Aws.Url.Encode (Value (A)));
         Free (List);
         Free (Reader);
      end loop;
   end Get_Vector;

   procedure Quick_Diff (From : in out Vector; Substract : in Vector) is
      Beginning, Position : Extended_Index;
   begin
      -- take adavantage that both lists are already sorted
      Beginning := First_Index (From);
      for I in First_Index (Substract) .. Last_Index (Substract) loop
         Position :=
            Find_Index
              (Container => From,
               Item      => Members_Vectors.Element (Substract, I),
               Index     => Beginning);
         if not (Position = No_Index) then
            Delete (From, Position);
            Beginning := Position;
         end if;
      end loop;
   end Quick_Diff;

begin
   if Argument_Count = 0 then
      Put_Line ("Can't process : No language given!");
      return;
   else
      Get_Vector (Argument (1), Language_Members);
   end if;

   Get_Vector ("Programming_Tasks", All_Tasks);
   Quick_Diff (All_Tasks, Language_Members);

   for I in First_Index (All_Tasks) .. Last_Index (All_Tasks) loop
      Put_Line (To_String (Members_Vectors.Element (All_Tasks, I)));
   end loop;
   Put_Line
     ("Numbers of tasks not implemented :=" &
      Integer'Image (Last_Index ((All_Tasks))));
end Not_Coded;
```


## AutoHotkey


The GUI overkill version with comments.

```AutoHotkey

#NoEnv ; do not resolve environment variables (speed)
#SingleInstance force ; allow only one instance
SetBatchLines, -1 ; set to highest script speed
SetControlDelay, 0 ; set delay between control commands to lowest

; additional label, for clarity only
AutoExec:
  Gui, Add, DDL, x10 y10 w270 r20 vlngStr ; add drop-down list
  Gui, Add, Button, x290 y10 gShowLng, Show Unimplemented ; add button
  Gui, Add, ListView, x10 y36 w400 h300 vcatLst gOnLV, Unimplemented ; add listview
  Gui, Add, StatusBar, vsbStr, ; add statusbar
  Gui, Show, , RosettaCode unimplemented list ; show the gui
  allLng := getList("lng") ; get a list of all available languages
  selectStr = Select language... ; set selection string for ddl
  GuiControl, , LngStr, |%selectStr%||%allLng% ; set the ddl to new contents
  SB_SetIcon("user32.dll", 5) ; change statusbar icon
  SB_SetText("Done loading languages. Select from menu.") ; change statusbar text
Return ; end of the autoexec section. Script waits for input.

; subroutine for language list in ddl update
ShowLng:
  Gui, Submit, NoHide ; refresh all gui variables
  If (lngStr != selectStr) ; if the current selected language is not the selection string
  {
    SB_SetIcon("user32.dll", 2) ; set statusbar icon to exclamation
    SB_SetText("Loading unimplemented tasks... Please wait.") ; set statusbar text
    allTsk := getList("tsk") ; get a list of all tasks
    allThis := getList(lngStr) ; get a list of all done tasks for this language
    Loop, Parse, allTsk, | ; parse the list of all tasks
    {
      If !InStr(allThis,A_LoopField) ; if the current task is not found in the list of all tasks
        notImpl .= A_LoopField . "|" ; add the current field to the notImpl variable
      allTskCnt := A_Index ; save the index for final count
    }
    StringTrimRight, notImpl, notImpl, 1 ; remove last delimiter
    LV_Delete() ; emty the listview
    GuiControl, -Redraw, catLst ; set the listview to not redraw while adding (speed)
    Loop, Parse, notImpl, | ; parse the list of not implemented tasks
    {
      LV_Add("", A_LoopField) ; add them to the listview, field by field
      notImplCnt := A_Index ; save the index for final count
    }
    GuiControl, +Redraw, catLst ; set the listview back to normal
    SB_SetIcon("user32.dll", 5) ; change statusbar icon to information
    SB_SetText("There are " . notImplCnt . " of " . allTskCnt
      . " tasks unimplemented. Double-click task to open in browser.") ; set the statusbar text
    notImpl = ; empty the notImpl variable
    notImplCnt = 0 ; set the count back to 0
  }
Return

; subroutine for action on listview
OnLV:
  If (A_GuiEvent = "DoubleClick") ; if the listview was double-clicked
  {
    LV_GetText(rowTxt, A_EventInfo) ; get the text of the clicked row
    rowTxt := Enc_Uri(rowTxt) ; uri-encode the text
    StringReplace, rowTxt, rowTxt, `%20, _, All ; replace all space-encodings with underscores
    Run % "http://rosettacode.org/wiki/" . rowTxt ; run the resulting link in the default browser
  }
Return

; generic function to gather list
getList(category)
{
  fileName = temp.xml ; set temp-file name
  If (category = "lng") ; if the category received is lng
  {
    category = Programming_Languages ; set the category
    fileName = lng.xml ; set temp-file name
    IfExist, %fileName% ; if the file already exists
    {
      FileGetTime, modTime, %fileName% ; get the last modified time
      EnvSub, modTime, %A_Now%, days ; get the difference between now and last modified time in days
      If (modTime < 3) ; if the result is less than 3
        Goto, GetFileNoDL ; go to function-internal subroutine to parse
    }
    SB_SetIcon("user32.dll", 2) ; set statusbar icon
    SB_SetText("Loading languages... Please wait.") ; set statusbar text
  }
  If (category = "tsk") ; if the category received is tsk
    category = Programming_Tasks ; set the category

  getFile: ; function-internal subroutine for getting a file and parsing it
    ; construct the url
    url := "http://www.rosettacode.org/w/api.php?action=query&list="
          . "categorymembers&cmtitle=Category:" . category
          . "&cmlimit=500&format=xml" . doContinue
    UrlDownloadToFile, %url%, %fileName% ; download the url

  getFileNoDL: ; function-internal subroutine for parsing a file
    FileRead, data, %fileName% ; read file into variable
    pos = 1 ; set while-loop counter
    rxp = title="(.*?)" ; set regular expression
    While (pos := RegExMatch(data, rxp, title, pos)) ; get the contents of the title fields one-by-one
    {
      If InStr(title1, "Category:") ; if the contents contain Category:
        StringTrimLeft, title1, title1, 9 ; remove that from the contents
      StringReplace, title1, title1, |, `|, All ; escape all containing delimiters
      title1 := Dec_XML(UTF82Ansi(title1)) ; convert to ansi first and then decode html entities
      allTitles .= title1 . "|" ; add this title to list
      pos++ ; increment counter
    }
    rxp = cmcontinue="(.*?)" ; set regular expression
    If RegExMatch(data, rxp, next) ; when found
    {
      doContinue = &cmcontinue=%next1% ; set continuation string to add to url
      Goto getFile ; go to function-internal subroutine to redownload and parse
    }
    StringTrimRight, allTitles, allTitles, 1 ; remove last delimiter from result
  Return allTitles ; return result
}

; function to convert html entities to ansi equivalents
Dec_XML(str)
{
   Loop
      If RegexMatch(str, "S)(&#(\d+);)", dec)
         StringReplace, str, str, %dec1%, % Chr(dec2), All
      Else If   RegexMatch(str, "Si)(&#x([\da-f]+);)", hex)
         StringReplace, str, str, %hex1%, % Chr("0x" . hex2), All
      Else
         Break
   StringReplace, str, str,  , %A_Space%, All
   StringReplace, str, str, &quot;, ", All
   StringReplace, str, str, &apos;, ', All
   StringReplace, str, str, &lt;,   <, All
   StringReplace, str, str, &gt;,   >, All
   StringReplace, str, str, &amp;,  &, All
   return, str
}

; function to uri-encode input string
Enc_Uri(str)
{
   f = %A_FormatInteger%
   SetFormat, Integer, Hex
   If RegExMatch(str, "^\w+:/{0,2}", pr)
      StringTrimLeft, str, str, StrLen(pr)
   StringReplace, str, str, `%, `%25, All
   Loop
      If RegExMatch(str, "i)[^\w\.~%/:]", char)
         StringReplace, str, str, %char%, % "%" . SubStr(Asc(char),3), All
      Else Break
   SetFormat, Integer, %f%
   Return, pr . str
}

; function to convert unicode to ansi text
UTF82Ansi(zString)
{
  Ansi2Unicode(zString, wString, 65001)
  Unicode2Ansi(wString, sString, 0)
  Return sString
}

; helper function for unicode to ansi function
Ansi2Unicode(ByRef sString, ByRef wString, CP = 0)
{
  nSize := DllCall("MultiByteToWideChar", "Uint", CP
    , "Uint", 0, "Uint", &sString, "int", -1
    , "Uint", 0, "int", 0)
  VarSetCapacity(wString, nSize * 2)
  DllCall("MultiByteToWideChar"
    , "Uint", CP, "Uint", 0, "Uint", &sString, "int", -1
    , "Uint", &wString, "int", nSize)
}

; helper function for unicode to ansi function
Unicode2Ansi(ByRef wString, ByRef sString, CP = 0)
{
  nSize := DllCall("WideCharToMultiByte"
    , "Uint", CP, "Uint", 0, "Uint", &wString, "int", -1
    , "Uint", 0, "int", 0, "Uint", 0, "Uint", 0)
  VarSetCapacity(sString, nSize)
  DllCall("WideCharToMultiByte"
    , "Uint", CP, "Uint", 0, "Uint", &wString, "int", -1
    , "str", sString, "int", nSize, "Uint", 0, "Uint", 0)
}

; subroutine called when user closes the gui
GuiClose:
  ExitApp ; exit the script
Return

```


### Output

Loads a list of all languages. Pick the language you would like to see the unimplemented tasks of, press the button, double click the selected task to launch in default browser. It will download the languages to file and redownload if older than 3 days (to save unnecessary bandwith).

[[Image:Ahk_find_unimplemented.png‎]]

=={{header|C sharp|C#}}==
Using JSON (not parsed, just Regex.)

To help demonstrate paging, the cmlimit parameter has been omitted from the search query so that 10 rows are returned by default


```csharp
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using System.Net;

class Program {
    static List<string> GetTitlesFromCategory(string category) {
        string searchQueryFormat = "http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:{0}&format=json{1}";
        List<string> results = new List<string>();
        string cmcontinue = string.Empty;

        do {
            string cmContinueKeyValue;

            //append continue variable as needed
            if (cmcontinue.Length > 0)
                cmContinueKeyValue = String.Format("&cmcontinue={0}", cmcontinue);
            else
                cmContinueKeyValue = String.Empty;

            //execute query
            string query = String.Format(searchQueryFormat, category, cmContinueKeyValue);
            string content = new WebClient().DownloadString(query);

            results.AddRange(new Regex("\"title\":\"(.+?)\"").Matches(content).Cast<Match>().Select(x => x.Groups[1].Value));

            //detect if more results are available
            cmcontinue = Regex.Match(content, @"{""cmcontinue"":""([^""]+)""}", RegexOptions.IgnoreCase).Groups["1"].Value;
        } while (cmcontinue.Length > 0);

        return results;
    }

    static string[] GetUnimplementedTasksFromLanguage(string language) {
        List<string> alltasks = GetTitlesFromCategory("Programming_Tasks");
        List<string> lang = GetTitlesFromCategory(language);

        return alltasks.Where(x => !lang.Contains(x)).ToArray();
    }

    static void Main(string[] args) {
        string[] unimpl = GetUnimplementedTasksFromLanguage(args[0]);

        foreach (string i in unimpl) Console.WriteLine(i);
    }
}
```


== {{header|Clojure}} ==
This uses a couple of core libraries, and a Java method for URL encoding.

```clojure
(require
  '[clojure.xml :as xml]
  '[clojure.set :as set]
  '[clojure.string :as string])
(import '[java.net URLEncoder])
```


The ''titles-cont'' function fetches and parses an XML response, and walks over it extracting titles. It also extracts the ''cmcontinue'' value, if present. Returns a pair ''[titles,cmcontinue]''.

```clojure
(defn titles-cont [url]
  (let [docseq (-> url xml/parse xml-seq)]
    ((juxt #(filter string? %), #(-> (filter map? %) first :cmcontinue))
      (for [{:keys [tag attrs content]} docseq :when (#{:cm :query-continue} tag)]
        (if (= tag :cm)
          (attrs :title)
          (-> content first :attrs))))))
```


The ''get-titles'' function has 1- and 2-argument versions. The former takes the name of a category, composes the appropriate URL query, and calls the 2-argument version. The 2-argument version gets a title list (with possible ''cmcontinue'' value), chaining further calls as necessary into the lazy sequence result.

```clojure
(defn urlencode [s] (URLEncoder/encode s "UTF-8"))
(defn param [p v] (str p (urlencode v)))

(defn get-titles
  ([category]
    (let [urlbase "http://www.rosettacode.org/w/api.php"
          params ["action=query"
                  "list=categorymembers"
                  "format=xml"
                  "cmlimit=200"
                  (param "cmtitle=Category:" category)]
          url (str urlbase "?" (string/join "&" params)]
      (get-titles url nil)))
  ([url continue-at]
    (let [continue-param (if continue-at (param "&cmcontinue=" continue-at))
          [titles continue] (titles-cont (str url continue-param))]
      (if continue
        (lazy-cat titles (get-titles url continue))
        titles))))
```

The ''unimplemented'' function gets a set of all titles and of language-implemented titles and returns the difference. It uses ''future'' in order to do the necessary URL requests in parallel.

```clojure
(defn unimplemented [lang-name]
  (let [title-set #(future (apply sorted-set (get-titles %)))
        all-titles (title-set "Programming_Tasks")
        lang-titles (title-set lang-name)]
    (seq (set/difference @all-titles @lang-titles))))

(let [titles (unimplemented "Clojure")]
  (doseq [title titles] (println title))
  (println "count: " (count titles)))
(shutdown-agents)
```


== {{header|E}} ==

Using JSON.


```e
#!/usr/bin/env rune

# NOTE: This program will not work in released E, because TermL is an
# imperfect superset of JSON in that version: it does not accept "\/".
# If you build E from the latest source in SVN then it will work.
#
# Usage: rosettacode-cat-subtract.e [
```e
]
#
# Prints a list of tasks which have not been completed in the language.
# If unspecified, the default language is E.

pragma.syntax("0.9")
pragma.enable("accumulator")

def termParser := <import:org.quasiliteral.term.makeTermParser>
def jURLEncoder := <unsafe:java.net.makeURLEncoder>

def urlEncode(text) {
  return jURLEncoder.encode(text, "UTF-8")
}

/** Convert JSON-as-term-tree to the corresponding natural E data structures. */
def jsonTermToData(term) {
    switch (term) {

        # JSON object to E map
        match term`{@assocs*}` {
            return accum [].asMap() for term`@{key :String}: @valueJson` in assocs {
                _.with(key, jsonTermToData(valueJson))
            }
        }

        # JSON array to E list
        match term`[@elements*]` {
            return accum [] for elem in elements { _.with(jsonTermToData(elem)) }
        }

        # Literals just need to be coerced
        match lit :any[String, int, float64] {
            return lit
        }

        # Doesn't support true/false/null, but we don't need that for this application.
    }
}

def fetchCategoryAccum(name, membersSoFar :Set, extraArgs) {
    stderr.println(`Fetching Category:$name $extraArgs...`)

    def categoryAPIResource := <http>[`//rosettacode.org/w/api.php?` +
        `action=query&list=categorymembers&cmtitle=Category:${urlEncode(name)}&` +
        `format=json&cmlimit=500&cmprop=title$extraArgs`]

    def members :=
      when (def responseJSON := categoryAPIResource <- getTwine()) -> {
        # stderr.println(`Fetched Category:$name $extraArgs, parsing...`)
        def response := jsonTermToData(termParser(responseJSON))

        # stderr.println(`Parsed Category:$name $extraArgs response, extracting data...`)
        def [
          "query" => ["categorymembers" => records],
          "query-continue" => continueData := null
        ] := response
        def members := accum membersSoFar for record in records { _.with(record["title"]) }

        switch (continueData) {
          match ==null {
            stderr.println(`Got all ${members.size()} for Category:$name.`)
            members
          }
          match ["categorymembers" => ["cmcontinue" => continueParam]] {
            stderr.println(`Fetched ${members.size()} members for Category:$name...`)
            fetchCategoryAccum(name, members, `&cmcontinue=` + urlEncode(continueParam))
          }
        }
    } catch p { throw(p) }

    return members
}

def fetchCategory(name) {
  return fetchCategoryAccum(name, [].asSet(), "")
}

# Interpret program arguments
def lang := switch (interp.getArgs()) {
  match [lang] { lang }
  match [] { "E" }
}

# Fetch categories
when (def allTasks := fetchCategory("Programming_Tasks"),
      def doneTasks := fetchCategory(lang),
      def omitTasks := fetchCategory(lang + "/Omit")
     ) -> {

    # Compute difference and report
    def notDoneTasks := allTasks &! (doneTasks | omitTasks)
    println()
    println("\n".rjoin(notDoneTasks.getElements().sort()))

} catch p {
    # Whoops, something went wrong
    stderr.println(`$p${p.eStack()}`)
}
```


## Erlang

init_http/0 is used by many tasks. rosetta_code_list_of/1 is used by [[Rosetta_Code/Rank_languages_by_popularity]]

```Erlang

-module( find_unimplemented_tasks ).
-include_lib( "xmerl/include/xmerl.hrl" ).

-export( [init_http/0, per_language/1, rosetta_code_list_of/1] ).

init_http() ->
	application:start( inets ).

per_language( Language ) ->
	ok = init_http(),
	Tasks = rosetta_code_list_of( "Programming_Tasks" ),
	Uninplemented = Tasks -- rosetta_code_list_of( Language ),
	io:fwrite( "Unimplemented total: ~p~n", [erlang:length(Uninplemented)] ),
	[io:fwrite("~p~n", [X]) || X <- Uninplemented].

rosetta_code_list_of( Category ) ->
	URL = "http://rosettacode.org/mw/api.php?action=query&list=categorymembers&cmlimit=500&format=xml&cmtitle=Category:"
	++ Category,
	title_contents( URL, "", [] ).



title_contents( URL, Continue, Acc ) ->
	{ok, {{_HTTP,200,"OK"}, _Headers, Body}} = httpc:request( URL ++ Continue ),
	{XML, _} = xmerl_scan:string( Body ),
	News = xml_selection( "title", XML ),
	New_continue = title_contents_url_continue( xml_selection("cmcontinue", XML) ),
	title_contents_continue( URL, New_continue, Acc ++ News ).

title_contents_continue( _URL, "", Acc ) -> Acc;
title_contents_continue( URL, Continue, Acc ) -> title_contents( URL, Continue, Acc ).

title_contents_url_continue( [] ) -> "";
title_contents_url_continue( [Continue | _] ) -> "&cmcontinue=" ++ Continue.

xml_selection( Selection, XML ) ->
	[lists:map( fun xml_8211/1, X) || #xmlAttribute{value=X} <- xmerl_xpath:string("//@" ++ Selection, XML)].

xml_8211( 8211 ) -> $-;
xml_8211( 924 ) -> $\s;
xml_8211( 1050 ) -> $\s;
xml_8211( 1052 ) -> $\s;
xml_8211( Character ) -> Character.

```

{{out}}

```txt

25> find_unimplemented_tasks:per_language("Erlang").
Unimplemented total: 307
"24 game/Solve"
"Abstract type"
"Active Directory/Search for a user"
"Add a variable to a class instance at runtime"
"Address of a variable"
.
.
.
"Yahoo! search interface"
"Yin and yang"
"Zeckendorf arithmetic"
"Zeckendorf number representation"
"Zhang-Suen thinning algorithm"

```



## Go

XML, stepping through the elements.

```go
package main

import (
    "encoding/xml"
    "fmt"
    "io"
    "net/http"
    "net/url"
)

const language = "Go"

var baseQuery = "http://rosettacode.org/mw/api.php?action=query" +
    "&format=xml&list=categorymembers&cmlimit=100"

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
    // get language members, store in a map
    langMap := make(map[string]bool)
    storeLang := func(cm string) { langMap[cm] = true }
    languageQuery := baseQuery + "&cmtitle=Category:" + language
    continueAt := req(languageQuery, storeLang)
    for continueAt > "" {
        continueAt = req(languageQuery+"&cmcontinue="+continueAt, storeLang)
    }

    // a quick check to avoid long output
    if len(langMap) == 0 {
        fmt.Println("no tasks implemented for", language)
        return
    }

    // get tasks, print as we go along
    printUnImp := func(cm string) {
        if !langMap[cm] {
            fmt.Println(cm)
        }
    }
    taskQuery := baseQuery + "&cmtitle=Category:Programming_Tasks"
    continueAt = req(taskQuery, printUnImp)
    for continueAt > "" {
        continueAt = req(taskQuery+"&cmcontinue="+continueAt, printUnImp)
    }
}
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
import Data.Char

getRespons url = do
  rsp <- Network.Browser.browse $ do
    setAllowRedirects True
    setOutHandler $ const (return ())   -- quiet
    request $ getRequest url
  return $ rspBody $ snd rsp

replaceWithSpace c = (\x -> if c==x then ' ' else x)

encl = chr 34

unimpTasks lang = do
  allTasks <- getRespons "http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml"
  impl <-  getRespons ( "http://rosettacode.org/json/isb/" ++ lang ++ ".json")
  let langxx = map (map(replaceWithSpace '_')) $ filter (/=",") $ words $ map (replaceWithSpace encl ) $ init $ drop 1 impl
      xml = onlyElems $ parseXML allTasks
      allxx = concatMap (map (fromJust.findAttr (unqual "title")). filterElementsName (== unqual "cm")) xml
  mapM_ putStrLn $ sort $ allxx \\ langxx
```


With only standard libraries

```haskell
import Network.HTTP
import Data.Text (splitOn, pack, unpack)
import Data.List

getTasks :: String -> IO [String]
getTasks url = do
  resp <- simpleHTTP (getRequest url)
  body <- getResponseBody resp
  return $ tail $ map (unpack . head . splitOn  (pack "\"}")) $ splitOn (pack "\"title\":\"") (pack body)

main :: String -> IO ()
main lang = do
  implTasks <- getTasks $ "http://rosettacode.org/mw/api.php?action=query&list=categorymembers&cmtitle=Category:" ++ lang ++ "&format=json&cmlimit=500"
  allTasks <- getTasks "http://rosettacode.org/mw/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&format=json&cmlimit=500"
  mapM_ putStrLn $ allTasks \\ implTasks
```


=={{header|Icon}} and {{header|Unicon}}==

The following code only works in Unicon.


```unicon
$define RCINDEX "http://rosettacode.org/mw/api.php?format=xml&action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500"
$define RCTASK  "http://rosettacode.org/mw/index.php?action=raw&title="
$define RCUA    "User-Agent: Unicon Rosetta 0.1"
$define RCXUA   "X-Unicon: http://unicon.org/"
$define TASKTOT "* Total Tasks *"
$define TOTTOT  "* Total Headers*"

link strings
link hexcvt

procedure main(A)   # simple single threaded read all at once implementation
    lang := \A[1] | "Unicon"
    write("Unimplemented tasks for ",lang," as of ",&date)
    every task := taskNames() do {
       if lang == languages(task) then next
       write(task)
       }
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


Sample output (abridged):

```txt

Unimplemented tasks for Unicon as of 2013/07/06:

### ============================================

9_billion_names_of_God_the_integer
Active_Directory/Connect
Active_Directory/Search_for_a_user
Active_object
Address_of_a_variable
Arena_storage_pool
Atomic_updates
Balanced_ternary
...
XML/Input
XML/Output
XML/XPath
Y combinator
Zebra puzzle
Zeckendorf arithmetic
Zeckendorf number representation

```



## J

'''Solution:'''

```j
require 'strings web/gethttp'

findUnimpTasks=: ('Programming_Tasks' -.&getCategoryMembers ,&'/Omit') ([ #~ -.@e.) getCategoryMembers

getTagContents=: dyad define
  'starttag endtag'=. x
  ('\' -.~ endtag&taketo)&.>@(starttag&E. <@((#starttag)&}.);.1 ]) y
)

NB. RosettaCode Utilities
parseTitles=: ('"title":"';'"')&getTagContents
parseCMcontinue=:('"cmcontinue":"';'"')&getTagContents
getCMcontquery=: ('&cmcontinue=' , urlencode)^:(0 < #)@>@parseCMcontinue

getCategoryMembers=: monad define
  buildqry=. 'action=query&list=categorymembers&cmtitle=Category:' , ,&'&cmlimit=500&format=json'
  url=.'http://www.rosettacode.org/w/api.php'
  uri=. url ,'?', buildqry urlencode y
  catmbrs=. qrycont=. ''
  whilst. #qrycont=. getCMcontquery jsondat do.
    jsondat=. gethttp uri , qrycont
    catmbrs=. catmbrs, parseTitles jsondat
  end.
  catmbrs
)
```


'''Example Usage:'''

```j
   4{. findUnimpTasks 'J'     NB. get first 4 unimplemented tasks for J
+-------------+--------------+----------------+------------------------------+
|Active object|Atomic updates|Basic input loop|Call foreign language function|
+-------------+--------------+----------------+------------------------------+
```



## JavaScript


The isolation of in-browser JavaScript from system resources, and the association of JavaScript code files with particular (and already loaded) documents may mean that JavaScript is not the right tool for the broader case envisaged by this task.

For the narrower context of a browser in which the 'not implemented' page has been fetched for a particular language, we can however, evaluate an XPath expression in JavaScript to generate an array of titles for the unimplemented tasks.


```JavaScript
(function (strXPath) {
  var xr = document.evaluate(
      strXPath,
      document,
      null, 0, 0
    ),

    oNode = xr.iterateNext(),
    lstTasks = [];

  while (oNode) {
    lstTasks.push(oNode.title);
    oNode = xr.iterateNext();
  }

  return [
    lstTasks.length + " items found in " + document.title,
    ''
  ].concat(lstTasks).join('\n')

})(
  '//*[@id="mw-content-text"]/div[2]/table/tbody/tr/td/ul/li/a'
);
```


Output begins with:


```txt
107 items found in Reports:Tasks not implemented in Haskell - Rosetta Code

Active Directory/Connect
Active Directory/Search for a user
Bitmap/Bézier curves/Cubic
Bitmap/PPM conversion through a pipe
Bitmap/Read an image through a pipe
Call a function in a shared library
Call an object method
Canny edge detector
Casting out nines
Catmull–Clark subdivision surface
Color of a screen pixel
Colour bars/Display
Colour pinstripe/Display
Colour pinstripe/Printer
Compare sorting algorithms' performance
Conjugate transpose
Create a file on magnetic tape
Death Star
Deconvolution/2D+
Delegates
Executable library
Extreme floating point values
FTP
Fibonacci word/fractal
Find limit of recursion
...
```



## Julia


```julia
using HTTP, JSON

const baseuri = "http://www.rosettacode.org/mw/api.php?action=query&list=categorymembers&cmtitle=Category:"
const enduri = "&cmlimit=500&format=json"
queries(x) = baseuri * HTTP.Strings.escapehtml(x) * enduri

function getimplemented(query)
    tasksdone = Vector{String}()
    request = query
    while (resp = HTTP.request("GET", request)).status == 200
        fromjson = JSON.parse(String(resp.body))
        for d in fromjson["query"]["categorymembers"]
            if haskey(d, "title")
                push!(tasksdone, d["title"])
            end
        end
        if haskey(fromjson, "continue")
            cmcont, cont = fromjson["continue"]["cmcontinue"], fromjson["continue"]["continue"]
            request = query * "&cmcontinue=$cmcont&continue=$cont"
        else
            break
        end
    end
    tasksdone
end

alltasks = getimplemented(queries("Programming_Tasks"))

showunimp(x) = (println("\nUnimplemented in $x:"); for t in setdiff(alltasks,
    getimplemented(queries(x))) println(t) end)

showunimp("Julia")
showunimp("C++")

```
{{out}}

```txt

Unimplemented in Julia:
Active Directory/Connect
Active Directory/Search for a user
AVL tree
Catmull–Clark subdivision surface
Colour pinstripe/Printer
Create a file on magnetic tape
Deconvolution/2D+
Joystick position
Machine code
OLE Automation
OpenGL
Pattern matching
Pinstripe/Printer
Play recorded sounds
Rosetta Code/Find unimplemented tasks
Simulate input/Keyboard
SOAP
Speech synthesis
Suffixation of decimal numbers
Superpermutation minimisation
Sutherland-Hodgman polygon clipping
Use another language to call a function
Yahoo! search interface

Unimplemented in C++:
15 puzzle solver
Abbreviations, automatic
Abbreviations, easy
Abbreviations, simple
Add a variable to a class instance at runtime
Balanced ternary
Brace expansion
Break OO privacy
Calendar - for "REAL" programmers
Call a foreign-language function
Call an object method
Casting out nines
Check Machin-like formulas
Church Numerals
Colour pinstripe/Printer
Commatizing numbers
Constrained genericity
Cuban primes
Define a primitive data type
Dynamic variable names
Eban numbers
Eertree
Egyptian fractions
Factorial base numbers indexing permutations of a collection
First-class functions/Use numbers analogously
Fixed length records
Four is magic
Four is the number of letters in the ...
Hash join
History variables
HTTPS/Client-authenticated
Hunt The Wumpus
Idiomatically determine all the characters that can be used for symbols
Idiomatically determine all the lowercase and uppercase letters
Imaginary base numbers
Index finite lists of positive integers
Inheritance/Multiple
Interactive programming
Joystick position
Knuth's power tree
Law of cosines - triples
Lucky and even lucky numbers
Lychrel numbers
Mayan numerals
Mind boggling card trick
Nested templated data
Nonoblock
Nonogram solver
Object serialization
OLE Automation
OpenWebNet Password
Order disjoint list items
Partition an integer X into N primes
Pattern matching
Pig the dice game
Pig the dice game/Player
Pinstripe/Printer
Play recorded sounds
Ramer-Douglas-Peucker line simplification
Range consolidation
Reflection/Get source
Reflection/List methods
Reflection/List properties
Respond to an unknown method call
RIPEMD-160
Rosetta Code/Count examples
Rosetta Code/Find bare lang tags
Rosetta Code/Find unimplemented tasks
Rosetta Code/Fix code tags
Runtime evaluation
Runtime evaluation/In an environment
Safe primes and unsafe primes
Send an unknown method call
Solve a Holy Knight's tour
Solve a Hopido puzzle
Solve a Numbrix puzzle
Solve the no connection puzzle
Sort a list of object identifiers
Spelling of ordinal numbers
Strong and weak primes
Suffixation of decimal numbers
Sum and Product Puzzle
Textonyms
Topic variable
Twelve statements
URL parser
Video display modes
Word search
World Cup group stage
Yahoo! search interface
Zeckendorf arithmetic

```



## Lua

{{works with|Lua|5.1 - 5.3}}
{{libheader|lua-requests}}


```lua
local requests = require('requests')
local lang = arg[1]

local function merge_tables(existing, from_req)
  local result = existing

  for _, v in ipairs(from_req) do
      result[v.title] = true
  end

  return result
end

local function get_task_list(category)
  local url = 'http://www.rosettacode.org/mw/api.php'
  local query = {
    action = 'query',
    list = 'categorymembers',
    cmtitle = string.format('Category:%s', category),
    cmlimit = 500,
    cmtype = 'page',
    format = 'json'
  }
  local categories = {}

  while true do
    local resp = assert(requests.get({ url, params = query }).json())

    categories = merge_tables(categories, resp.query.categorymembers)

    if resp.continue then
      query.cmcontinue = resp.continue.cmcontinue
    else
      break
    end
  end

  return categories
end

local function get_open_tasks(lang)
  if not lang then error('Language missing!') end
  local all_tasks = get_task_list('Programming_Tasks')
  local lang_tasks = get_task_list(lang)
  local task_list = {}

  for t, _ in pairs(all_tasks) do
    if not lang_tasks[t] then
      table.insert(task_list, t)
    end
  end

  table.sort(task_list)
  return task_list
end

local open_tasks = get_open_tasks(lang)

io.write(string.format('%s has %d unimplemented programming tasks: \n', lang, #open_tasks))
for _, t in ipairs(open_tasks) do
  io.write(string.format('  %s\n', t))
end
```

{{out}}

```txt

Python has 24 unimplemented programming tasks:
  15 puzzle solver
  Bitmap/PPM conversion through a pipe
  Bitmap/Read an image through a pipe
                 ...

```



## Maple


```Maple
#Example with the Maple Language
lan := "Maple":
x := URL:-Get(cat("http://rosettacode.org/wiki/Reports:Tasks_not_implemented_in_", StringTools:-SubstituteAll(lan, " ", "_")), output = content):
x := StringTools:-StringSplit(x, "<h2><span class=\"mw-headline\" id=\"Not_implemented\">Not implemented</span>")[2]:
x := StringTools:-StringSplit(x, "<span class=\"mw-headline\" id=\"Draft_tasks_without_implementation\">Draft tasks without implementation</span>")[1]:
x := StringTools:-StringSplit(x,"<li><a href=\"/wiki/")[2..]:
for problem in x do
	printf("%s\n", StringTools:-SubstituteAll(StringTools:-Decode(StringTools:-StringSplit(problem, "\" title=")[1], 'percent'), "_", " "));
end do:
```

{{Out|Output}}

```txt
#10:09 AM 10/05/2018
15 Puzzle Game
15 puzzle solver
24 game/Solve
4-rings or 4-squares puzzle
9 billion names of God the integer
AVL tree
...
Xiaolin Wu's line algorithm
Yahoo! search interface
Yin and yang
Zebra puzzle
Zeckendorf arithmetic
Zeckendorf number representation
Zhang-Suen thinning algorithm
Zig-zag matrix

```



## Mathematica

Two implementations are considered, the first will use the API to get all the programming tasks, then the ones implemented for a certain language, and take the difference. The other will use the Tasks not implemented page and strip the html.

```Mathematica
ClearAll[ImportAll]
ImportAll[lang_String] :=
 Module[{data, continue, cmcontinue, extra, xml, next},
  data = {};
  continue = True;
  cmcontinue = "";
  While[continue,
   extra = If[cmcontinue =!= "", "&cmcontinue=" <> cmcontinue, ""];
   xml = Import["http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:" <> lang <> "&cmlimit=500&format=xml" <> extra, "XML"];
   AppendTo[data, Cases[xml, XMLElement["cm", _, {}], \[Infinity]]];
   next = Flatten[Cases[xml, {"cmcontinue" -> _}, \[Infinity]]];
   If[Length[next] > 0,
    next = First[next];
    cmcontinue = "cmcontinue" /. next;
    ,
    continue = False;
    ];
   ];
  Cases[Flatten[data], HoldPattern["title" -> x_] :> x, \[Infinity]]
  ]
Complement[ImportAll["Programming_Tasks"], ImportAll["Mathematica"]]
```

which outputs a list of items not implemented:
{{out}}

```txt
{Abstract type,Active Directory/Connect,Active Directory/Search for a user,Address of a variable,<<139>>,Write to Windows event log,Xiaolin Wu's line algorithm,Zeckendorf arithmetic}
```

Another method is by getting the Tasks not implemented page:

```Mathematica
url = "http://rosettacode.org/wiki/Reports:Tasks_not_implemented_in_Mathematica";
html = Import[url, "XMLObject"];
pos = Position[html, XMLElement["div", {"class" -> "mw-content-ltr", "dir" -> "ltr", "lang" -> "en"}, ___]];
pos = First[pos];
data = Extract[html, pos];
pos = Position[data, XMLElement["li", {}, {XMLElement["a", {"shape" -> "rect", "href" -> _, "title" -> x_}, {x_}]}]];
data = Extract[data, pos];
data = data[[All, -1, -1, 2]];
data = {"title", "href"} /. # & /@ %;
data[[All, 2]] = "http://rosettacode.org" <> # & /@ data[[All, 2]];
newb = data[[All, 1]];
data = Hyperlink @@@ data;
data // Column
```

{{out}}

```txt
Append a record to the end of a text file
Arithmetic-geometric mean/Calculate Pi
Atomic updates
Bitcoin/address validation
Bitmap/PPM conversion through a pipe
Bitmap/Read an image through a pipe
...
```



## Nim


```nim
import httpclient, strutils, xmldom, xmldomparser, cgi, os

proc findrc(category: string): seq[string] =
  var
    name = "http://www.rosettacode.org/mw/api.php?action=query&list=categorymembers&cmtitle=Category:$#&cmlimit=500&format=xml" % encodeUrl(category)
    cmcontinue = @[""]
    titles = newSeq[string]()

  while true:
    var x = getContent(name&cmcontinue[0]).loadXML()
    for i in x.getElementsByTagName("cm"):
      titles.add PElement(i).getAttribute("title")

    cmcontinue = @[]
    for i in x.getElementsByTagName("categorymembers"):
      let u = PElement(i).getAttribute("cmcontinue")
      if u != nil: cmcontinue.add encodeUrl(u)

    if cmcontinue.len > 0:
      cmcontinue[0] = "&cmcontinue=" & cmcontinue[0]
    else:
      break

  return titles

proc chooselang(): string =
  if paramCount() < 1:
    return "Nim"
  else:
    return paramStr(1)


let alltasks = findrc("Programming_Tasks")
let lang = chooselang().findrc()

for i in alltasks:
  if i notin lang:
    echo $i
```

Usage:

```txt
./unimplementedtasks Nim
24 game
24 game/Solve
9 billion names of God the integer
Abstract type
Active Directory/Connect
Active Directory/Search for a user
Active object
Add a variable to a class instance at runtime
AKS test for primes
[...]
```



## Oz

{{libheader|OzHttpClient}}

By parsing XML and using an XPath-like mechanism:

```oz
declare
  [HTTPClient] = {Link ['x-ozlib://mesaros/net/HTTPClient.ozf']}
  [XMLParser] = {Link ['x-oz://system/xml/Parser.ozf']}

  fun {FindUnimplementedTasks Language}
     AllTasks = {FindCategory "Programming Tasks"}
     LangTasks = {FindCategory Language}
  in
     {ListDiff AllTasks LangTasks}
  end

  fun {FindCategory Cat}
     CatUrl = "http://www.rosettacode.org/mw/api.php?action=query"
     #"&list=categorymembers"
     #"&cmtitle=Category:"#{PercentEncode Cat}
     #"&cmlimit=500&format=xml"

     fun {Loop CMContinue}
        [_ Doc] = {Parse {GetPage CatUrl#CMContinue}}
        Titles = {XPath Doc
                  [api query categorymembers cm {Attribute title}]}
     in
        case {XPath Doc
              [api 'query-continue' categorymembers {Attribute cmcontinue}]}
        of nil then Titles
        [] [NewCMContinueAtom] then
           NewCMContinue = {PercentEncode {Atom.toString NewCMContinueAtom}}
        in
           {Append Titles
            {Loop "&cmcontinue="#NewCMContinue}}
        end
     end
  in
     {Loop nil}
  end


  %% XPath emulation
  fun {XPath Doc Path}
     P|Pr = Path
  in
     Doc.name = P %% assert
     {FoldL Pr XPathStep [Doc]}
  end

  Nothing = {NewName}
  fun {NotNothing X} X \= Nothing end

  fun {XPathStep Elements P}
     if {Atom.is P} then
        {FilteredChildren Elements P}
     elseif {Procedure.is P} then
        {Filter {Map Elements P} NotNothing}
     end
  end

  %% A flat list of all Type-children of all Elements.
  fun {FilteredChildren Elements Type}
     {Flatten
      {Map Elements
       fun {$ E}
          {Filter E.children
           fun {$ X}
              case X of element(name:!Type ...) then true
              else false
              end
           end}
       end}}
  end

  fun {Attribute Attr}
     fun {$ Element}
        case {Filter Element.attributes fun {$ A} A.name == Attr end}
        of [A] then A.value
        else Nothing
        end
     end
  end


  %% GetPage
  Client = {New HTTPClient.urlGET init(inPrms(toFile:false toStrm:true) _)}
  fun {GetPage RawUrl}
     Url = {VirtualString.toString RawUrl}
     OutParams
  in
     {Client getService(Url ?OutParams ?_)}
     OutParams.sOut
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


  %% Parse
  local
     Parser = {New XMLParser.parser init}
  in
     fun {Parse Xs} {Parser parseVS(Xs $)} end
  end

  fun {ListDiff Xs Ys}
     {FoldL Ys List.subtract Xs}
  end
in
  %% show tasks not implemented in Oz
  {ForAll {FindUnimplementedTasks "Oz"} System.showInfo}
```



## Perl


```perl
use LWP::UserAgent;

my $ua = LWP::UserAgent->new;
$ua->agent('');

sub enc { join '', map {sprintf '%%%02x', ord} split //, shift }
sub get { $ua->request( HTTP::Request->new( GET => shift))->content }

sub tasks {
    my($category) = shift;
    my $fmt = 'http://www.rosettacode.org/mw/api.php?' .
              'action=query&generator=categorymembers&gcmtitle=Category:%s&gcmlimit=500&format=json&rawcontinue=';
    my @tasks;
    my $json = get(sprintf $fmt, $category);
    while (1) {
        push @tasks, $json =~ /"title":"(.+?)"\}/g;
        $json =~ /"gcmcontinue":"(.+?)"\}/ or last;
        $json = get(sprintf $fmt . '&gcmcontinue=%s', $category, enc $1);
    }
    @tasks;
}

my %language = map {$_, 1} tasks shift || 'perl';
$language{$_} or print "$_\n" foreach tasks('Programming_Tasks'), tasks('Draft_Programming_Tasks');
```


'''See also:''' [[User:ImplSearchBot/Code]]


## Perl 6

{{works with|Rakudo|2018.04.1}}

```perl6
use HTTP::UserAgent;
use URI::Escape;
use JSON::Fast;
use Sort::Naturally;

unit sub MAIN( Str :$lang = 'Perl_6' );

my $client = HTTP::UserAgent.new;
my $url = 'http://rosettacode.org/mw';

my @total;
my @impl;

@total.append: .&get-cat for 'Programming_Tasks', 'Draft_Programming_Tasks';
@impl = get-cat $lang;

say "Unimplemented tasks in $lang:";
.say for (@total (-) @impl).keys.sort: *.&naturally;

sub get-cat ($category) {
    flat mediawiki-query(
        $url, 'pages',
        :generator<categorymembers>,
        :gcmtitle("Category:$category"),
        :gcmlimit<350>,
        :rawcontinue(),
        :prop<title>
    ).map({ .<title> });
}

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
```



## Phix

Note that [[Rosetta_Code/Tasks_without_examples#Phix]] with summary=true and output_html=false
does much that same via web scraping, whereas this uses the web api.
{{trans|Go}}

```Phix
-- demo\rosetta\Find_unimplemented_tasks.exw
constant language = "Phix",
         base_query = "http://rosettacode.org/mw/api.php?action=query"&
                      "&format=xml&list=categorymembers&cmlimit=100"

include builtins\libcurl.e
atom curl = NULL
atom pErrorBuffer
include builtins\xml.e

function req(string url, integer rid)
    if curl=NULL then
        curl_global_init()
        curl = curl_easy_init()
        pErrorBuffer = allocate(CURL_ERROR_SIZE)
        curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, pErrorBuffer)
    end if
    curl_easy_setopt(curl, CURLOPT_URL, url)
    object res = curl_easy_perform_ex(curl)
    if integer(res) then
        string error = sprintf("%d [%s]",{res,peek_string(pErrorBuffer)})
        crash("Download error: %s\n",{error})
    end if
    if not string(res) then ?9/0 end if
    object xml = xml_parse(res)[XML_CONTENTS]
    res = xml_get_nodes(xml,"continue")
    res = iff(res=={}?"":xml_get_attribute(res[1],"cmcontinue"))
    xml = xml_get_nodes(xml,"query")[1]
    xml = xml_get_nodes(xml,"categorymembers")[1]
    xml = xml_get_nodes(xml,"cm")
    for i=1 to length(xml) do
        call_proc(rid,{xml_get_attribute(xml[i],"title")})
    end for
    return res
end function

sequence languages = {}
procedure store_lang(string language)
    languages = append(languages,language)
end procedure
constant r_store_lang = routine_id("store_lang")

integer unimplemented_count = 0
procedure print_unimplemented(string language)
    if not find(language,languages) then
        printf(1,"%s\n",{language})
        unimplemented_count += 1
    end if
end procedure
constant r_print = routine_id("print_unimplemented")

procedure main()
    -- get and store language members
    string lang_query := base_query & "&cmtitle=Category:" & language,
           continue_at := req(lang_query, r_store_lang)
    while continue_at!="" do
        continue_at = req(lang_query&"&cmcontinue="&continue_at, r_store_lang)
    end while

    -- a quick check to avoid long output
    if length(languages)==0 then
        printf(1,"no tasks implemented for %s\n", {language})
        return
    end if

    -- get tasks, print as we go along
    string task_query := base_query & "&cmtitle=Category:Programming_Tasks"
    continue_at = req(task_query, r_print)
    while continue_at!="" do
        continue_at = req(task_query&"&cmcontinue="&continue_at, r_print)
    end while
    printf(1,"%d unimplemented tasks found for %s.\n",{unimplemented_count,language})
end procedure
main()
```

{{out}}

```txt

Abstract type
Active Directory/Connect
Active Directory/Search for a user
...
Yahoo! search interface
78 unimplemented tasks found for Phix.

```



## PicoLisp


```PicoLisp
(load "@lib/http.l" "@lib/xm.l")

(de rosettaCategory (Cat)
   (let (Cont NIL  Xml)
      (make
         (loop
            (client "rosettacode.org" 80
               (pack
                  "mw/api.php?action=query&list=categorymembers&cmtitle=Category:"
                  Cat
                  "&cmlimit=200&format=xml"
                  Cont )
               (while (line))
               (setq Xml (and (xml?) (xml))) )
            (NIL Xml)
            (for M (body Xml 'query 'categorymembers)
               (link (attr M 'title)) )
            (NIL (attr Xml 'query-continue' categorymembers 'cmcontinue))
            (setq Cont (pack "&cmcontinue=" @)) ) ) ) )

(de unimplemented (Task)
   (diff
      (rosettaCategory "Programming_Tasks")
      (rosettaCategory Task) ) )
```



## PowerShell

I don't think this script follows the spirit of the task, but it works.

```PowerShell

function Find-UnimplementedTask
{
    [CmdletBinding()]
    [OutputType([string[]])]
    Param
    (
        [Parameter(Mandatory=$true,
                   Position=0)]
        [string]
        $Language
    )

    $url = "http://rosettacode.org/wiki/Reports:Tasks_not_implemented_in_$Language"
    $web = Invoke-WebRequest $url
    $unimplemented = 1

    [string[]](($web.AllElements |
        Where-Object {$_.class -eq "mw-content-ltr"})[$unimplemented].outerText -split "`r`n" |
        Select-String -Pattern "[^0-9A-Z]$" -CaseSensitive)
}

```


```PowerShell

$tasks = Find-UnimplementedTask -Language PowerShell

$tasks[0..5],".`n.`n.",$tasks[-6..-1]
Write-Host "`nTotal unimplemented Tasks: $($tasks.Count)"

```

{{Out}}

```txt

15 Puzzle Game
2048
24 game/Solve
4-rings or 4-squares puzzle
9 billion names of God the integer
AKS test for primes
.
.
.
Xiaolin Wu's line algorithm
Yahoo! search interface
Yin and yang
Zebra puzzle
Zeckendorf arithmetic
Zhang-Suen thinning algorithm

Total unimplemented Tasks: 388

```


== {{header|Python}} ==
==={{libheader|mwclient}}===

```python
"""
Given the name of a language on Rosetta Code,
finds all tasks which are not implemented in that language.
"""
from operator import attrgetter
from typing import Iterator

import mwclient

URL = 'www.rosettacode.org'
API_PATH = '/mw/'


def unimplemented_tasks(language: str,
                        *,
                        url: str,
                        api_path: str) -> Iterator[str]:
    """Yields all unimplemented tasks for a specified language"""
    site = mwclient.Site(url, path=api_path)
    all_tasks = site.categories['Programming Tasks']
    language_tasks = site.categories[language]
    name = attrgetter('name')
    all_tasks_names = map(name, all_tasks)
    language_tasks_names = set(map(name, language_tasks))
    for task in all_tasks_names:
        if task not in language_tasks_names:
            yield task


if __name__ == '__main__':
    tasks = unimplemented_tasks('Python', url=URL, api_path=API_PATH)
    print(*tasks, sep='\n')

```


==={{libheader|Requests}}===

```python
"""
Given the name of a language on Rosetta Code,
finds all tasks which are not implemented in that language.
"""
from functools import partial
from operator import itemgetter
from typing import (Dict,
                    Iterator,
                    Union)

import requests

URL = 'http://www.rosettacode.org/mw/api.php'
REQUEST_PARAMETERS = dict(action='query',
                          list='categorymembers',
                          cmlimit=500,
                          rawcontinue=True,
                          format='json')


def unimplemented_tasks(language: str,
                        *,
                        url: str,
                        request_params: Dict[str, Union[str, int, bool]]
                        ) -> Iterator[str]:
    """Yields all unimplemented tasks for a specified language"""
    with requests.Session() as session:
        tasks = partial(tasks_titles,
                        session=session,
                        url=url,
                        **request_params)
        all_tasks = tasks(cmtitle='Category:Programming_Tasks')
        language_tasks = set(tasks(cmtitle=f'Category:{language}'))
        for task in all_tasks:
            if task not in language_tasks:
                yield task


def tasks_titles(*,
                 session: requests.Session,
                 url: str,
                 **params: Union[str, int, bool]) -> Iterator[str]:
    """Yields tasks names for a specified category"""
    while True:
        request = session.get(url, params=params)
        data = request.json()
        yield from map(itemgetter('title'), data['query']['categorymembers'])
        query_continue = data.get('query-continue')
        if query_continue is None:
            return
        params.update(query_continue['categorymembers'])


if __name__ == '__main__':
    tasks = unimplemented_tasks('Python',
                                url=URL,
                                request_params=REQUEST_PARAMETERS)
    print(*tasks, sep='\n')

```


==={{libheader|urllib}}===


```python
import xml.dom.minidom
import urllib, sys

def findrc(category):
    name = "http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:%s&cmlimit=500&format=xml" % urllib.quote(category)
    cmcontinue, titles = '', []
    while True:
        u = urllib.urlopen(name + cmcontinue)
        xmldata = u.read()
        u.close()
        x = xml.dom.minidom.parseString(xmldata)
        titles += [i.getAttribute("title") for i in x.getElementsByTagName("cm")]
        cmcontinue = filter( None,
                             (urllib.quote(i.getAttribute("cmcontinue"))
                              for i in x.getElementsByTagName("categorymembers")) )
        if cmcontinue:
            cmcontinue = '&cmcontinue=' + cmcontinue[0]
        else:
            break
    return titles

alltasks = findrc("Programming_Tasks")
lang = findrc(sys.argv[1])

for i in [i for i in alltasks if i not in lang]:
    print i
```



## R

{{libheader|XML (R)}}

```R
library(XML)
find.unimplemented.tasks <- function(lang="R"){
	PT <- xmlInternalTreeParse( paste("http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml",sep="") )
	PT.nodes <- getNodeSet(PT,"//cm")
	PT.titles = as.character( sapply(PT.nodes, xmlGetAttr, "title") )
	language <- xmlInternalTreeParse( paste("http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:",
	lang, "&cmlimit=500&format=xml",sep="") )
	lang.nodes <- getNodeSet(language,"//cm")
	lang.titles = as.character( sapply(lang.nodes, xmlGetAttr, "title") )
	unimplemented <- setdiff(PT.titles, lang.titles)
	unimplemented
}
# Usage
find.unimplemented.tasks(lang="Python")
langs <- c("R","python","perl")
sapply(langs, find.unimplemented.tasks) # fetching data for multiple languages
```



## Racket


```Racket

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

;; The above is the required "library" code, same as the "Rosetta
;; Code/Count" entry

(define (show-unimplemented lang)
  (for-each displayln (remove* (get-category lang)
                               (get-category 'Programming_Tasks))))

(show-unimplemented 'Racket) ; see all of the Racket entries

```



## Ring


```ring

# Project: Rosetta Code/Find unimplemented tasks

load "stdlib.ring"
ros= download("http://rosettacode.org/wiki/Category:Programming_Tasks")
lang = "Ring"
pos = 1
num = 0
totalros = 0
rosname = ""
rostitle = ""
see "Tasks not implemented in " + lang + " language:" + nl
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
        s = substr(rostitle,lang)
        if s = 0
           num = num + 1
           totalros = totalros + 1
           see "" + num + ". " + rosname + nl
        ok
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

Tasks not implemented in Ring language:
1. 15 puzzle solver
2. 2048
3. 24 game/Solve
4. 9 billion names of God the integer
5. Abstract type
6. Active Directory/Search for a user
7. Address of a variable
8. AKS test for primes
9. Align columns
10. Amb
......
343. Xiaolin Wu's line algorithm
344. XML/DOM serialization
345. XML/Input
346. XML/Output
347. XML/XPath
348. Y combinator
349. Yahoo! search interface
350. Yin and yang
351. Zeckendorf arithmetic
352. Zhang-Suen thinning algorithm

Total: 352 examples.

```



## Ruby

Finds tasks that are either not implemented or omitted. Sorts by task creation time.

Uses the <code>RosettaCode</code> module from [[Count programming examples#Ruby]]

```ruby
require 'rosettacode'
require 'time'

module RosettaCode
  def self.get_unimplemented(lang)
    programming_tasks = []
    category_members("Programming_Tasks") {|task| programming_tasks << task}

    lang_tasks = []
    category_members(lang) {|task| lang_tasks << task}

    lang_tasks_omit = []
    category_members("#{lang}/Omit") {|task| lang_tasks_omit << task}

    [programming_tasks - lang_tasks, lang_tasks_omit]
  end

  def self.created_time(title)
    url = get_api_url({
      "action" => "query",
      "titles" => title,
      "format" => "xml",
      "rvlimit" => 500,
      "prop" => "revisions",
      "rvprop" => "timestamp"
    })
    doc = REXML::Document.new open(url)
    REXML::XPath.each(doc, "//rev").collect do |node|
      Time.parse( node.attribute("timestamp").value )
    end.min
  end

end

puts Time.now
lang = ARGV[0] || "Ruby"
unimplemented, omitted = RosettaCode.get_unimplemented(lang)
unimplemented.collect {|title| [title, RosettaCode.created_time(title)]} .
              sort_by {|e| e[1]} .
              each do |title, date|
                puts "%s %6s %s" % [
                  date.strftime("%Y-%m-%d"),
                  omitted.include?(title) ? "[omit]" : "" ,
                  title
                ]
              end

```


Output for Ruby
<pre style="height: 32ex; overflow: scroll">2010-04-05 11:08:07 -0500
2007-01-14        Table creation
2007-01-25 [omit] Pointers and references
2007-02-04        SQL-based authentication
2007-02-09        Metered concurrency
2007-02-27 [omit] Address of a variable
2007-02-27 [omit] Variable size/Set
2007-02-27        Variable size/Get
2007-06-08        OpenGL
2007-06-08        HTTPS/Authenticated
2007-11-06        Pattern matching
2007-11-08 [omit] Parametric polymorphism
2007-11-13        Special characters
2007-12-11        Arithmetic evaluation
2007-12-21 [omit] Proof
2007-12-24        Plot coordinate pairs
2007-12-24        Polynomial regression
2007-12-24        Compare sorting algorithms' performance
2008-03-27        Dragon curve
2008-03-28        Formal power series
2008-09-11        RCRPG
2008-11-02        Active object
2008-11-13        Window creation/X11
2008-12-05 [omit] Constrained genericity
2009-02-17        Rendezvous
2009-03-22        Arena storage pool
2009-05-23        Ray-casting algorithm
2009-05-26 [omit] Memory allocation
2009-05-26        Simulate input/Mouse
2009-05-27        Mouse position
2009-05-27        Keyboard macros
2009-05-27        Window management
2009-05-27        Color of a screen pixel
2009-06-01        HTTPS/Client-authenticated
2009-06-02        Simulate input/Keyboard
2009-06-08 [omit] Create an object at a given address
2009-06-10        Events
2009-06-10        Scope modifiers
2009-08-09        Verify distribution uniformity/Chi-squared test
2009-08-11        Call a function from a foreign language
2009-08-12        Safe addition
2009-12-05        Rate counter
2010-01-02        Loading Animated 3D Data
2010-01-06        Catmull–Clark subdivision surface
2010-01-21        Hough transform
2010-01-25        Compile-time calculation
2010-02-14        Knapsack problem/Bounded
2010-02-21        Deconvolution/1D
2010-02-23        Deconvolution/2D+
2010-02-24        Knapsack problem/Continuous
2010-03-23        Sutherland-Hodgman polygon clipping
2010-03-23        Find Common Directory Path
```



## Run BASIC

Find tasks not implemented.
Select Language from DropDown and click [GO] or [Exit].

```runbasic
WordWrap$ = "style='white-space: pre-wrap;white-space: -moz-pre-wrap;white-space: -pre-wrap;white-space: -o-pre-wrap;word-wrap: break-word'"
a$	= httpGet$("http://rosettacode.org/wiki/Category:Programming_Languages")
a$	= word$(a$,2,"mw-subcategories")
a$	= word$(a$,1,"</table>")
i	= instr(a$,"/wiki/Category:")
html "<B>   Select language from list<BR>"
html "<SELECT name='lang'>"
while i <> 0
  j	= instr(a$,""" title=""Category:",i)
  lang$	= mid$(a$,i+15,j-i-15)
  k	= instr(a$,""">",j + 18)
  langName$ = mid$(a$,j + 18,k-(j+18))
  count	= count + 1
  html  "<option value='";lang$;"'>";langName$;"</option>"
  i	= instr(a$,"/wiki/Category:",k)
wend
html "</select>"
html "<p>Number of Languages:";count;"<BR>        "
button #go,"GO", [go]
html "        "
button #ex, "Exit", [exit]
wait

[go]
cls
lang$	= #request get$("lang")
h$	= "http://rosettacode.org/wiki/Reports:Tasks_not_implemented_in_";lang$
a$	= httpGet$(h$)
a$	= word$(a$,3,"mw-content-ltr")
html "<table border=1><tr>"
i	= instr(a$,"<a href=""/wiki/")
while i > 0
  i	= instr(a$,"title=""",i)
  j	= instr(a$,""">",i+7)
  if c mod 4	= 0 then html "</tr><tr ";WordWrap$;">"
  c	= c + 1
  html "<td>";mid$(a$,i+7,j-(i+7));"</td>"
  i	= instr(a$,"<a href=""/wiki/",i+7)
wend
html "</tr></table>"
print "Total unImplemented Tasks:";c
[exit]
end
```

<table border=1>
<tr></tr>
<tr style='white-space: pre-wrap;white-space: -moz-pre-wrap;white-space: -pre-wrap;white-space: -o-pre-wrap;word-wrap: break-word'><td>24 game/Solve</td><td>Abstract type</td><td>Accumulator factory</td><td>Active Directory/Connect</td><td>Active Directory/Search for a user</td></tr>
<tr style='white-space: pre-wrap;white-space: -moz-pre-wrap;white-space: -pre-wrap;white-space: -o-pre-wrap;word-wrap: break-word'><td>Arena storage pool</td><td>Arithmetic/Rational</td><td>Arithmetic evaluation</td><td>Average loop length</td><td>Averages/Mean angle</td></tr>
<tr style='white-space: pre-wrap;white-space: -moz-pre-wrap;white-space: -pre-wrap;white-space: -o-pre-wrap;word-wrap: break-word'><td>Averages/Mean time of day</td><td>Bitcoin/address validation</td><td>Bitmap/PPM conversion through a pipe</td><td>Bitmap/Read an image through a pipe</td><td>Break OO privacy</td></tr>
<tr style='white-space: pre-wrap;white-space: -moz-pre-wrap;white-space: -pre-wrap;white-space: -o-pre-wrap;word-wrap: break-word'><td>CRC-32</td><td>CSV to HTML translation</td><td>Calendar</td><td>Calendar - for &quot;real&quot; programmers</td><td>Call a function</td></tr>
<tr style='white-space: pre-wrap;white-space: -moz-pre-wrap;white-space: -pre-wrap;white-space: -o-pre-wrap;word-wrap: break-word'><td>Call an object method</td><td>Canny edge detector</td><td>Carmichael 3 strong pseudoprimes, or Miller Rabin's nemesis</td><td>Catmull–Clark subdivision surface</td><td>Chat server</td></tr>
<tr style='white-space: pre-wrap;white-space: -moz-pre-wrap;white-space: -pre-wrap;white-space: -o-pre-wrap;word-wrap: break-word'><td>Check Machin-like formulas</td><td>Cholesky decomposition</td><td>Colour pinstripe/Printer</td><td>Compare sorting algorithms' performance</td><td>Constrained genericity</td></tr>
<tr style='white-space: pre-wrap;white-space: -moz-pre-wrap;white-space: -pre-wrap;white-space: -o-pre-wrap;word-wrap: break-word'><td>Continued fraction</td><td>Continued fraction arithmetic/Continued fraction r2cf(Rational N)</td><td>Continued fraction arithmetic/G(matrix NG, Contined Fraction N)</td><td>Continued fraction arithmetic/G(matrix NG, Contined Fraction N1, Contined Fraction N2)</td><td>Count the coins</td></tr>
<tr style='white-space: pre-wrap;white-space: -moz-pre-wrap;white-space: -pre-wrap;white-space: -o-pre-wrap;word-wrap: break-word'><td>Zebra puzzle</td><td>Zeckendorf number representation</td></tr></table>
Total unImplemented Tasks:177<br />


## Scala


Add to `build.sbt`

      libraryDependencies ++= Seq(
      "org.json4s"%%"json4s-native"%"3.6.0",
      "com.softwaremill.sttp"%%"core"%"1.5.11",
      "com.softwaremill.sttp"%%"json4s"%"1.5.11")


```scala

import com.softwaremill.sttp.json4s._
import com.softwaremill.sttp.quick._

implicit val serialization = org.json4s.native.Serialization
import org.json4s.DefaultFormats
implicit val formats = DefaultFormats

case class Task(pageid: Int, title: String)
case class Category(categorymembers: List[Task])
case class Query(query: Category)

List("Programming Tasks", "Scala")
  .map { title =>
    sttp
      .get(uri"http://www.rosettacode.org/mw/api.php?action=query&list=categorymembers&cmtitle=Category:${title}&cmlimit=1000&format=json")
      .header("User-Agent", mozillaUserAgent)
      .response(asJson[Query])
      .send()
      .body
  }
  .map {
    case Right(r) => r.query.categorymembers.toSet
    case Left(s) => Set.empty[Task]
  }
  .foldRight(Set.empty[Task])((acc: Set[Task], ele: Set[Task]) => acc -- ele)


```



## Tcl

First, find all members of the Programming_Tasks category,
then find all members of the <code>$lang</code> category.
The difference is the list of unimplemented tasks.

{{tcllib|json}}{{tcllib|struct::set}}

```tcl
package require Tcl 8.5
package require http
package require json
package require struct::set

fconfigure stdout -buffering none

# Initialize a cache of lookups
array set cache {}
proc log msg {
    #puts -nonewline $msg
}

proc get_tasks {category} {
    global cache
    if {[info exists cache($category)]} {
	return $cache($category)
    }
    set base_url http://www.rosettacode.org/mw/api.php
    set query {
	action	query
	list	categorymembers
	cmtitle	Category:%s
	format	json
	cmlimit	500
    }
    set query [list {*}$query]; # remove excess whitespace
    set this_query [dict create {*}[split [format $query $category]]]
    set tasks [list]

    while {1} {
        set url [join [list $base_url [http::formatQuery {*}$this_query]] ?]
        while 1 {
            set response [http::geturl $url]
	    # Process redirects
            if {[http::ncode $response] == 301} {
                set newurl [dict get [http::meta $response] Location]
                if {[string match http://* $newurl]} {
                    set url $newurl
                } else {
                    set url [regexp -inline {http://[^/]+} $url]
                    append url $newurl
                }
                continue
            }
	    # Check for oopsies!
            if {
		[set s [http::status $response]] ne "ok"
		|| [http::ncode $response] != 200
	    } then {
                error "Oops: url=$url\nstatus=$s\nhttp code=[http::code $response]"
            }
            break
        }

	# Get the data out of the message
        set data [json::json2dict [http::data $response]]
        http::cleanup $response

        # add tasks to list
        foreach task [dict get $data query categorymembers] {
            lappend tasks [dict get [dict create {*}$task] title]
        }

        if {[catch {
	    dict get $data query-continue categorymembers cmcontinue
	} continue_task]} then {
            # no more continuations, we're done
            break
        }
        dict set this_query cmcontinue $continue_task
    }
    return [set cache($category) $tasks]
}

proc get_unimplemented {lang} {
    set tasks [get_tasks Programming_Tasks]
    set collected [get_tasks Collection_Members]
    set doneTasks [get_tasks $lang]
    set omittedTasks [get_tasks $lang/Omit]

    # Map generic collection task categories to specific ones
    set tasks [regsub -all {Category:(\S+)} $tasks "\\1/$lang"]

    set collectOfLang [struct::set intersect $collected $doneTasks]
    set ignorable [struct::set union $doneTasks $omittedTasks $collectOfLang]
    set unimplemented [struct::set difference $tasks $ignorable]

    puts "\n$lang has [llength $unimplemented] unimplemented programming tasks:"
    if {[llength $unimplemented]} {
	puts "  [join [lsort $unimplemented] "\n  "]"
    }
}

catch {console show}
catch {wm withdraw .}
foreach lang {Perl Python Ruby Tcl} {
    get_unimplemented $lang
}
```



## zkl

Uses shared libraries YAJL and cURL.

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

allTasks:=getTasks.future("Programming_Tasks");  // thread
```


```zkl
language:="zkl";
tasks:=getTasks(language);
langTasks:=Dictionary(); tasks.pump(Void,langTasks.add.fp1(Void));
unimplementedTasks:=allTasks.filter('wrap(nm){ (not langTasks.holds(nm)) });
println("Found %d unimplemented tasks for %s:"
      .fmt(unimplementedTasks.len(1),language));
unimplementedTasks.pump(Console.println);
```

{{out}}

```txt

Found 166 unimplemented tasks for zkl:
15 Puzzle Game
2048
Address of a variable
Animate a pendulum
...
Zeckendorf arithmetic
Zhang-Suen thinning algorithm

```



{{omit from|Batch File}}
{{omit from|Brainfuck}}
{{omit from|Lilypond}}
{{omit from|Lotus 123 Macro Scripting}}
{{omit from|Maxima}}
{{omit from|PostScript}}
{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have network access. -->
{{omit from|Yorick|Does not have network access.}}
{{omit from|ZX Spectrum Basic|Does not have network access.}}

[[Category:Rosetta Code related]]
