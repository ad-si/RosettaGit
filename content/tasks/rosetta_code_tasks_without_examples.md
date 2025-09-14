+++
title = "Rosetta Code/Tasks without examples"
description = ""
date = 2019-05-10T00:10:17Z
aliases = []
[extra]
id = 19035
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "go",
  "perl",
  "perl_6",
  "phix",
  "vbscript",
  "zkl",
]
+++

{{draft task}}A RosettaCode contributor is going on a long journey. To ease the boredom of a long flight he takes his favourite programming language manual and a list of RosettaCode tasks, but there's a catch: the tasks have to be description only and not include all the solutions already supplied by other programmers.

Task: Scrape http://rosettacode.org/wiki/Category:Programming_Tasks for all the tasks. Traverse the links. Extract the text or html between a tag with a class of "infobox" and the beginning of the table with the id of "toc".


## Go


```go
package main

import (
    "fmt"
    "html"
    "io/ioutil"
    "net/http"
    "regexp"
    "strings"
    "time"
)

func main() {    
    ex := `<li><a href="/wiki/(.*?)"`
    re := regexp.MustCompile(ex)
    page := "http://rosettacode.org/wiki/Category:Programming_Tasks"
    resp, _ := http.Get(page)
    body, _ := ioutil.ReadAll(resp.Body)
    matches := re.FindAllStringSubmatch(string(body), -1)
    resp.Body.Close()
    tasks := make([]string, len(matches))
    for i, match := range matches {
        tasks[i] = match[1]
    }
    const base = "http://rosettacode.org/wiki/"
    const limit = 3 // number of tasks to print out
    ex = `(?s)using any language you may know.</div>(.*?)<div id="toc"`
    ex2 := `</?[^>]*>` // to remove all tags including links
    re = regexp.MustCompile(ex)
    re2 := regexp.MustCompile(ex2)
    for i, task := range tasks {
        page = base + task
        resp, _ = http.Get(page)
        body, _ = ioutil.ReadAll(resp.Body)
        match := re.FindStringSubmatch(string(body))
        resp.Body.Close()
        text := html.UnescapeString(re2.ReplaceAllLiteralString(match[1], ""))
        fmt.Println(strings.Replace(task, "_", " ", -1), "\n", text)
        if i == limit-1 {
            break
        }
        time.Sleep(5 * time.Second) // wait 5 seconds before processing next task
    }
}
```


Text rather than HTML:

```txt

100 doors 
 
There are 100 doors in a row that are all initially closed.
You make 100 passes by the doors.
The first time through, visit every door and  toggle  the door  (if the door is closed,  open it;   if it is open,  close it).
The second time, only visit every 2nd door   (door #2, #4, #6, ...),   and toggle it.
The third time, visit every 3rd door   (door #3, #6, #9, ...), etc,   until you only visit the 100th door.


Task

Answer the question:   what state are the doors in after the last pass?   Which are open, which are closed?

Alternate:
As noted in this page's   discussion page,   the only doors that remain open are those whose numbers are perfect squares.
Opening only those doors is an   optimization   that may also be expressed;
however, as should be obvious, this defeats the intent of comparing implementations across programming languages.



15 Puzzle Game 
 
 


Task

Implement the Fifteen Puzzle Game.

The   15-puzzle   is also known as:

   Fifteen Puzzle
   Gem Puzzle
   Boss Puzzle
   Game of Fifteen
   Mystic Square
   14-15 Puzzle
   and many others.


Related Tasks

   15 Puzzle Solver
   16 Puzzle Game



15 puzzle solver 
 
Your task is to write a program that finds a solution in the fewest moves possible single moves to a random Fifteen Puzzle Game.
For this task you will be using the following puzzle:

15 14  1  6
 9 11  4 12
 0 10  7  3
13  8  5  2


Solution: 1  2  3  4
 5  6  7  8
 9 10 11 12
13 14 15  0

The output must show the moves' directions, like so: left, left, left, down, right... and so on.
There are two solutions, of fifty-two moves:
rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd
rrruldluuldrurdddluulurrrdlddruldluurddlulurruldrrdd
see: Pretty Print of Optimal Solution
Finding either one, or both is an acceptable result.

Extra credit.
Solve the following problem:

  0 12  9 13
 15 11 10 14
  3  7  2  5
  4  8  6  1



Related Task

 15 puzzle game
 A* search algorithm

```



## Perl

Slice and dice the HTML. Output is as for the other examples.

```perl
use strict;
use warnings;

use LWP::UserAgent;
my $ua = LWP::UserAgent->new;

# get list of task titles
my $html  = $ua->request( HTTP::Request->new( GET => 'http://rosettacode.org/wiki/Category:Programming_Tasks'))->content;
my @tasks = $html =~ m#<li><a href="/wiki/(.*?)"#g;

# download tasks, and extract task descriptions
for my $title (@tasks) {
    my $html = $ua->request( HTTP::Request->new( GET => "http://rosettacode.org/wiki/$title" ))->content;
    my($task_at_hand) = $html =~ m#using any language you may know.</div>(.*?)<div id="toc"#s;
    print "$title\n$task_at_hand\n\n";
    sleep 10; # so you have time to read each task...
}
```



## Perl 6

```perl6
use HTTP::UserAgent;
use Gumbo;

my $ua = HTTP::UserAgent.new;
my $taskfile = './RC_tasks.html';

# Get list of Tasks
say "Updating Programming_Tasks list...";
my $page   = "http://rosettacode.org/wiki/Category:Programming_Tasks";
my $html   = $ua.get($page).content;
my $xmldoc = parse-html($html, :TAG<div>, :id<mw-pages>);
my @tasks  = parse-html($xmldoc[0].Str, :TAG<li>).Str.comb( /'/wiki/' <-["]>+ / )».substr(6); #"
my $f      = open("./RC_Programming_Tasks.txt", :w)  or die "$!\n";
note "Writing Programming_Tasks file...";
$f.print( @tasks.join("\n") );
$f.close;

sleep .5;

for 'Programming_Tasks' -> $category
{ # Scrape info from each page.

    note "Loading $category file...";
    note "Retreiving tasks...";
    my @entries = "./RC_{$category}.txt".IO.slurp.lines;

    for @entries -> $title {
        note $title;

        # Get the raw page
        my $html = $ua.get: "http://rosettacode.org/wiki/{$title}";

        # Filter out the actual task description
        $html.content ~~ m|'<div id="mw-content-text" lang="en" dir="ltr" class="mw-content-ltr"><div'
                            .+? 'using any language you may know.</div>' (.+?) '<div id="toc"'|;

        my $task = cleanup $0.Str;

        # save to a file
        my $fh = $taskfile.IO.open :a;

        $fh.put: "<hr>\n     $title\n<hr>\n$task";

        $fh.close;

        sleep 3; # Don't pound the server
    }
}

sub cleanup ( $string ) {
    $string.subst( /^.+ '</div>'/, '' )
}
```

<div style="border-style: groove; margin: 50px; padding: 25px;">
<hr>
     100_doors
<hr>

<p>There are 100 doors in a row that are all initially closed.
</p><p>You make 100 <a href="/wiki/Rosetta_Code:Multiple_passes" title="Rosetta Code:Multiple passes">passes</a> by the doors.
</p><p>The first time through, visit every door and &#160;<i>toggle</i>&#160; the door &#160;(if the door is closed, &#160;open it; &#160; if it is open,&#160; close it).
</p><p>The second time, only visit every 2<sup>nd</sup> door &#160; (door #2, #4, #6, ...), &#160; and toggle it.
</p><p>The third time, visit every 3<sup>rd</sup> door &#160; (door #3, #6, #9, ...), etc, &#160; until you only visit the 100<sup>th</sup> door.
</p><p><br />
</p>
<dl><dt>Task</dt>
<dd></dd></dl>
<p>Answer the question: &#160; what state are the doors in after the last pass? &#160; Which are open, which are closed?
</p><p><br />
<b><a href="/wiki/Rosetta_Code:Extra_credit" title="Rosetta Code:Extra credit">Alternate</a>:</b>
As noted in this page's &#160; <a href="/wiki/Talk:100_doors" title="Talk:100 doors">discussion page</a>, &#160; the only doors that remain open are those whose numbers are perfect squares.
</p><p>Opening only those doors is an &#160; <a href="/wiki/Rosetta_Code:Optimization" title="Rosetta Code:Optimization">optimization</a> &#160; that may also be expressed;
however, as should be obvious, this defeats the intent of comparing implementations across programming languages.
<br /><br />
</p>

<hr>
     15_Puzzle_Game
<hr>

<dl><dt>Task</dt>
<dd></dd></dl>
<p>Implement the <a href="http://en.wikipedia.org/wiki/15_puzzle" class="extiw" title="wp:15 puzzle">Fifteen Puzzle Game</a>.
<br /><br />
</p>
<dl><dt>Related Task</dt>
<dd></dd></dl>
<ul><li> <a href="/wiki/15_puzzle_solver" title="15 puzzle solver">15 Puzzle Solver</a></li></ul>
<p><br /><br />
</p>

<hr>
     15_puzzle_solver
<hr>

<p>Your task is to write a program that finds a solution in the fewest single moves (no multimoves) possible to a random <a href="http://en.wikipedia.org/wiki/15_puzzle" class="extiw" title="wp:15 puzzle">Fifteen Puzzle Game</a>.<br />
For this task you will be using the following puzzle:<br />
</p>

```txt
15 14  1  6
 9 11  4 12
 0 10  7  3
13  8  5  2
```

<p><br />
</p>
Solution:
```txt
 1  2  3  4
 5  6  7  8
 9 10 11 12
13 14 15  0

```

<p>The output must show the moves' directions, like so: left, left, left, down, right... and so on.<br />
There are 2 solutions with 52 moves:<br />
rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd<br />
rrruldluuldrurdddluulurrrdlddruldluurddlulurruldrrdd<br />
finding either one, or both is an acceptable result.<br />
see: <a rel="nofollow" class="external text" href="http://www.rosettacode.org/wiki/15_puzzle_solver/Optimal_solution">Pretty Print of Optimal Solution</a>
</p>
<dl><dt>Extra credit.</dt></dl>
<p>Solve the following problem:
</p>

```txt
  0 12  9 13
 15 11 10 14
  3  7  2  5
  4  8  6  1

```

<p><br />
</p>
<dl><dt>Related Task</dt>
<dd></dd></dl>
<ul><li> <a href="/wiki/15_Puzzle_Game" title="15 Puzzle Game">15 puzzle game</a></li></ul>
<p><br /><br /><b>...and so on... </b>
</p>
</div>



## Phix

Since downloading all the pages can be very slow, this uses a cache. Limiting by "Phix" fairly obviously speeds it up tenfold :-)

Output similar to zkl, I assume the first four constants are self-explanatory.

```Phix
-- demo\rosetta\Tasks_without_examples.exw
constant output_html = true,
         include_drafts = true,
         summary = false,
         notlang = "Phix" -- "" for all

include builtins\timedate.e
integer refresh_cache = timedelta(days:=30) -- 0 for always

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
        -- use existing file if <= refresh_cache (30+ days) old
        sequence last_mod = get_file_date(filename)     -- (0.8.1+)
        atom delta = timedate_diff(last_mod,date())
        refetch = (delta>refresh_cache)
    else
        string directory = get_file_path(filename)
        if get_file_type(directory)!=FILETYPE_DIRECTORY then
            if not create_directory(directory,make_parent:=true) then
                crash("cannot create %s directory",{directory})
            end if
        end if
    end if
    if refetch then
        printf(1,"Downloading %s...\n",{filename})
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
        CURLcode res = curl_easy_perform(curl)
        if res!=CURLE_OK then
            string error = sprintf("%d",res)
            if res=CURLE_COULDNT_RESOLVE_HOST then
                error &= " [CURLE_COULDNT_RESOLVE_HOST]"
            end if
            printf(1, "Error %s downloading file\n", error)
            {} = wait_key()
            abort(0)
        end if  
        close(fn)
        refresh_cache += timedelta(days:=1) -- did I mention it is slow?
    end if
    return get_text(filename)
end function

function open_category(string filename)
    return open_download(filename&".htm","http://rosettacode.org/wiki/Category:"&filename)
end function

function dewiki(string s)
    sequence tasks = {}
    integer start = 1, finish = match(`<div class="printfooter">`,s)
    s = s[1..finish-1]
    while true do
        start = match("<li><a href=\"/wiki/",s,start)
        if start=0 then exit end if
        start += length("<li><a href=\"/wiki/")
        finish = find('"',s,start)
        string task = s[start..finish-1]
        task = substitute(task,"*","%2A")
        task = substitute(task,":","%3A")
        tasks = append(tasks,task)
        start = finish+1
    end while
    return tasks
end function

function extract_tasks()
    -- extract tasks from eg `<li><a href="/wiki/100_doors"`
    sequence tasks = dewiki(open_category("Programming_Tasks"))
    if include_drafts then
        tasks &= dewiki(open_category("Draft_Programming_Tasks"))
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
    if not summary then
        -- replace with contents
        for i=1 to length(tasks) do
            string ti = tasks[i],
                   url = sprintf("http://rosettacode.org/wiki/%s",{ti}),
                   contents = open_download(ti&".htm",url)
            integer start = match(`</div>`,contents,match(`<div class="infobox"`,contents))+length(`</div>`)
            integer finish = match(`<div id="toc"`,contents,start)-1
            -- ... but draft tasks with too few languages have no toc:
            if finish=-1 then finish = match(`<h2>`,contents,start)-1 end if
            -- ... and if no languages at all, use the footer:
            if finish=-1 then finish = match(`</div><div class="printfooter">`,contents,start)-1 end if
            if finish=-1 then ?9/0 end if
            contents = contents[start..finish]
            ti = substitute(ti,"_"," ")
            if not match("<b>"&ti&"</b>",contents) then
                -- (ps: I refuse to panic over the occasional replicated header...)
                contents = sprintf("<h3>%s</h3>%s",{ti,contents})
            end if
            tasks[i] = contents
            if get_key()=#1B then exit end if
        end for
    end if
    if curl!=NULL then
        curl_easy_cleanup(curl)
        free(pErrorBuffer)
        curl = NULL
        pErrorBuffer = NULL
    end if
    return tasks
end function

function html_clean(string ri)
    ri = substitute(ri,"%3A",":")
    ri = substitute(ri,"%E2%80%93","-")
    ri = substitute(ri,"%E2%80%99","'")
    ri = substitute(ri,"%27","'")
    ri = substitute(ri,"%2B","+")
    ri = substitute(ri,"%C3%A8","e")
    ri = substitute(ri,"%C3%A9","e")
    ri = substitute(ri,"%22","\"")
    ri = substitute(ri,"%2A","*")
    return ri
end function

constant html_header = """
<!DOCTYPE html>
<html lang="en">
 <head>
  <meta charset="utf-8" />
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
  <title>Rosettacode Tasks without examples</title>
 </head>
 <body>
  <h2>Rosettacode Tasks without examples</h2>
  Generated %s, %d entries


""",
         html_footer = """
 </body>
</html>
"""

sequence results = extract_tasks()
if output_html then
    integer fn = open("Tasks_Without_Examples.html","w")
    printf(fn,html_header,{format_timedate(date()),length(results)})
    for i=1 to length(results) do
        printf(fn,"%s
",html_clean(results[i]))
    end for
    puts(fn,html_footer)
    close(fn)
else
    for i=1 to length(results) do
        printf(1,"%s\n",html_clean(results[i]))
    end for
end if

?"done"
{} = wait_key()
```



## VBScript

Uses Internet Explorer (v9 and above) to traverse the DOM in the list page and subsequent task pages. Echoes title and href to the console. Outputs files containing html. 

Could output text by retrieving innerText instead of innerHTML in Slurp().

```vbscript
Option Explicit

Dim oIE : Set oIE = CreateObject("InternetExplorer.Application")

Dim oFSO : Set oFSO = CreateObject("Scripting.FileSystemObject")

Dim oRE : Set oRE = New RegExp
oRE.Pattern = "class=[" & Chr(34) & "'](.*?)[" & Chr(34) & "']" 
oRE.IgnoreCase = True

oIE.Navigate "http://rosettacode.org/wiki/Category:Programming_Tasks"
While oIE.Busy
	WScript.Sleep 100
Wend

Dim oDoc : Set oDoc = oIE.Document
Dim oDict : Set oDict = CreateObject("Scripting.Dictionary")

' build a dictionary of anchors
Dim oAnchor
Dim oAnchors : Set oAnchors = oDoc.getElementById("mw-pages").getElementsByTagName("a")
For Each oAnchor In oAnchors
	oDict.Add oAnchor.innerText, oAnchor.href
Next

'traverse the dictionary of anchors
Dim aKeys : aKeys = oDict.Keys()
Dim aKey
For Each aKey In aKeys
	Slurp aKey, oDict(aKey)
Next

oIE.Quit

Function Slurp(sTitle, sHref)
	WScript.Echo sTitle, sHref
	
	oIE.Navigate sHref
	While oIE.Busy
		WScript.Sleep 100
	Wend
	
	Dim oDoc : Set oDoc = oIE.Document
	
	Dim oStart : Set oStart = oDoc.getElementsByClassName("infobox")(0)
	Dim oCursor : Set oCursor = oStart
	Dim sDescription : sDescription = vbNullString
	Dim oThere 
	Dim iErr
	
	Do While oCursor.tagName <> "TABLE" And oCursor.id <> "toc"
		Set oThere = oCursor
		sDescription = sDescription & oThere.innerHTML & vbNewLine
		On Error Resume Next 
		Set oCursor = oCursor.nextElementSibling
		iErr = Err.Number
		On Error Goto 0
		If iErr <> 0 Then
			Exit Do
		End If
	Loop
	
	dim sTitle2
	sTitle2 = Replace(sTitle,"/","_")
	sTitle2 = Replace(sTitle2,Chr(34),"'")
	sTitle2 = Replace(sTitle2,"*",".")
	
	Dim oHandle : Set oHandle = oFSO.CreateTextFile(sTitle2 & ".htm", True, True)
	oHandle.Write "<a href='" & sHref & "'>" & sTitle & "</a>

"
	oHandle.Write sDescription
	oHandle.Close
	
	oIE.Stop

End Function

```



## zkl

This is a bit of a twist on the task: Programmer wants to know which tasks
have not been implimeted by their favorite language.

Uses libraries cURL and YAJL (yet another json library).

```zkl
var [const] CURL=Import("zklCurl"), YAJL=Import("zklYAJL")[0];

fcn getTasks(language){
   continueValue,tasks,curl := "",Data(0,String), CURL();  // "nm\0nm\0...."
   do{	// eg 5 times
      page:=curl.get(("http://rosettacode.org/mw/api.php?"
         "action=query&cmlimit=500"
	 "&format=json"
	 "&list=categorymembers"
	 "&cmtitle=Category:%s"
	 "&cmcontinue=%s").fmt(language,continueValue),Void);
      page=page[0];  // get HTML body
      json:=YAJL().write(page).close();
      json["query"]["categorymembers"].pump(tasks,T("get","title"));
      continueValue=json.find("continue") #{continue : -||,cmcontinue:page|954|19)}
          .toList()	# ( ("continue","-||"), ("cmcontinue","page|954|19") )
	  .apply("concat","=").concat("&"); # continue=-||&cmcontinue=page|954|19
   }while(continueValue);
   tasks
}

var allTasks  =getTasks.future("Programming_Tasks");	    // thread
var draftTasks=getTasks.future("Draft_Programming_Tasks");  // thread
var tasks     =getTasks.future(lang);		  	    // thread

fcn newTasks{
   langTasks:=tasks.pump(Dictionary().add.fp1(Void)); // { "Semordnilap":Void }
   unimplementedTasks:=allTasks.filter('!(langTasks.holds))
             .extend(draftTasks.filter('!(langTasks.holds)));
#if 0
   unimplementedTasks.pump(List()).sort().pump(Console.println);
#else  // this next part is very very slow, useless, can't find a wikimedia api
       // and the amount of information is pretty overwhelming
   curl:=CURL();
   foreach task in (unimplementedTasks.pump(List()).sort()){
      // some task names have a / in them, can't url encode: "Active Directory/Connect"
      page:="https://rosettacode.org/wiki/" + task.replace(" ","_");
      page:=curl.get(page)[0];  // sloooow
      s,s := page.find("\"infobox\""), page.find("<p>",s);
      if(not (e:=page.find("<div id=\"toc\"",s))) e:=page.find("</p>",s);
      println("<h2>%s</h2>\n%s\n<hr>".fmt(task,page[s,e-s].text));
   }
#endif
}

newTasks();
```

```txt

$ zkl rs_tasks_without.zkl 
15 Puzzle Game
15 puzzle solver
2048
ASCII art diagram converter
AVL tree
Ackermann function
...

```


<div style="border-style: groove; margin: 50px; padding: 25px;">
15 Puzzle Game
<p>Implement the <a href="http://en.wikipedia.org/wiki/15_puzzle" class="extiw" title="wp:15 puzzle">Fifteen Puzzle Game</a>.
<br /><br />
</p>
<dl><dt>Related Task</dt>
<dd></dd></dl>
<ul><li> <a href="/wiki/15_puzzle_solver" title="15 puzzle solver">15 Puzzle Solver</a></li></ul>
<p><br /><br />
</p>

<hr>
15 puzzle solver
<p>Your task is to write a program that finds a solution in the fewest moves possible single moves to a random <a href="http://en.wikipedia.org/wiki/15_puzzle" class="extiw" title="wp:15 puzzle">Fifteen Puzzle Game</a>.<br />
For this task you will be using the following puzzle:<br />
</p>
15 14  1  6
 9 11  4 12
 0 10  7  3
13  8  5  2
<p><br />
</p>
Solution:
 1  2  3  4
 5  6  7  8
 9 10 11 12
13 14 15  0

<p>The output must show the moves' directions, like so: left, left, left, down, right... and so on.<br />
There are 2 solutions with 52 moves:<br />
rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd<br />
rrruldluuldrurdddluulurrrdlddruldluurddlulurruldrrdd<br />
finding either one, or both is an acceptable result.<br />
see: <a rel="nofollow" class="external text" href="http://www.rosettacode.org/wiki/15_puzzle_solver/Optimal_solution">Pretty Print of Optimal Solution</a>
</p>
<dl><dt>Extra credit.</dt></dl>
<p>Solve the following problem:
</p>
  0 12  9 13
 15 11 10 14
  3  7  2  5
  4  8  6  1

<p><br />
</p>
<dl><dt>Related Task</dt>
<dd></dd></dl>
<ul><li> <a href="/wiki/15_Puzzle_Game" title="15 Puzzle Game">15 puzzle game</a></li>
<li> <a href="/wiki/A*_search_algorithm" title="A* search algorithm">A* search algorithm</a></li></ul>
<p><br /><br />
</p>

<hr>
2048
<p>The rules are that on each turn the player must choose a direction (up, down, left or right) and all tiles move as far as possible in that direction, some more than others. Two adjacent tiles (in that direction only) with matching numbers combine into one bearing the sum of those numbers. A move is valid when at least one tile can be moved, if only by combination. A new tile with the value of 2 is spawned at the end of each turn at a randomly chosen empty square, if there is one. To win the player must create a tile with the number 2048. The player lo 

ses if no valid moves are possible.
</p><p>The name comes from the popular open-source implementation of this game mechanic, <a rel="nofollow" class="external text" href="https://gabrielecirulli.github.io/2048/">2048</a>.
</p><p>Requirements:
</p>

<b>and so on</b>
