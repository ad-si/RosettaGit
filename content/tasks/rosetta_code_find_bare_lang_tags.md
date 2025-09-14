+++
title = "Rosetta Code/Find bare lang tags"
description = ""
date = 2019-07-26T11:23:23Z
aliases = []
[extra]
id = 10059
[taxonomies]
categories = ["task", "Rosetta Code related"]
tags = []
languages = [
  "autohotkey",
  "c",
  "erlang",
  "go",
  "haskell",
  "julia",
  "maple",
  "mathematica",
  "perl",
  "perl_6",
  "phix",
  "racket",
  "ruby",
  "scala",
  "tcl",
  "zkl",
]
+++

## Task

Find all   <big><big> <nowiki><lang></nowiki> </big></big>   tags without a language specified in the text of a page.

Display counts by language section:


```txt

<nowiki>Description

<lang>Pseudocode
```



## C


```C
printf("Hello world!\n");
```



## Perl

<lang>print "Hello world!\n"
```
</nowiki>

```


should display something like

```txt

2 bare language tags.

1 in perl
1 in no language

```



;Extra credit:
Allow multiple files to be read.   Summarize all results by language:

```txt

<nowiki>5 bare language tags.

2 in c ([[Foo]], [[Bar]])
1 in perl ([[Foo]])
2 in no language ([[Baz]])</nowiki>

```



;Extra extra credit:
Use the   [http://rosettacode.org/mw/api.php Media Wiki API]   to test actual RC tasks.





## AutoHotkey

''This code has no syntax highlighting, because Rosetta Code's highlighter fails with code that contains literal <nowiki>
```
</nowiki> tags.''

Stole RegEx Needle from Perl

```txt
<nowiki>task =
(
Description

<lang>Pseudocode
```



## C


```C
printf("Hello world!\n");
```



## Perl

<lang>print "Hello world!\n"
```

)
lang := "no language", out := Object(lang, 0), total := 0
Loop Parse, task, `r`n
	If RegExMatch(A_LoopField, "==\s*{{\s*header\s*\|\s*([^\s\}]+)\s*}}\s*==", $)
		lang := $1, out[lang] := 0
	else if InStr(A_LoopField, "<lang>")
		out[lang]++
For lang, num in Out
	If num
		total++, str .= "`n" num " in " lang
MsgBox % clipboard := total " bare lang tags.`n" . str</nowiki>
```

Output:

```txt
2 bare lang tags.

1 in no language
1 in Perl
```



## Erlang


```Erlang

-module( find_bare_lang_tags ).

-export( [task/0] ).

task() ->
	{ok, Binary} = file:read_file( "priv/find_bare_lang_tags_1" ),
	Lines = string:tokens( erlang:binary_to_list(Binary), "\n" ),
	{_Lang, Dict} = lists:foldl( fun count_empty_lang/2, {"no language", dict:new()}, Lines ),
	Count_langs = [{dict:fetch(X, Dict), X} || X <- dict:fetch_keys(Dict)],
	io:fwrite( "~p bare language tags.~n", [lists:sum([X || {X, _Y} <- Count_langs])] ),
	[io:fwrite( "~p in ~p~n", [X, Y] ) || {X, Y} <- Count_langs].



count_empty_lang( Line, {Lang, Dict} ) ->
	Empty_lang = string:str( Line, "<lang>" ),
	New_dict = dict_update_counter( Empty_lang, Lang, Dict ),
	New_lang = new_lang( string:str( Line,"=={{header|" ), Line, Lang ),
	{New_lang, New_dict}.

dict_update_counter( 0, _Lang, Dict ) -> Dict;
dict_update_counter( _Start, Lang, Dict ) -> dict:update_counter( Lang, 1, Dict ).

new_lang( 0, _Line, Lang ) -> Lang;
new_lang( _Start, Line, _Lang ) ->
	Start = string:str( Line, "|" ),
	Stop = string:rstr( Line, "}}==" ),
	string:sub_string( Line, Start+1, Stop-1 ).

```

```txt

60>  find_bare_lang_tags:task().
2 bare language tags.
1 in "no language"
1 in "Perl"

```



## Go


```go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
    "os"
    "regexp"
    "strings"
)

type header struct {
    start, end int
    lang       string
}

type data struct {
    count int
    names *[]string
}

func newData(count int, name string) *data {
    return &data{count, &[]string{name}}
}

var bmap = make(map[string]*data)

func add2bmap(lang, name string) {
    pd := bmap[lang]
    if pd != nil {
        pd.count++
        *pd.names = append(*pd.names, name)
    } else {
        bmap[lang] = newData(1, name)
    }
}

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func main() {
    expr := `==\s*{{\s*header\s*\|\s*([^\s\}]+)\s*}}\s*==`
    expr2 := fmt.Sprintf("<%s>.*?</%s>", "lang", "lang")
    r := regexp.MustCompile(expr)
    r2 := regexp.MustCompile(expr2)
    fileNames := []string{"example.txt", "example2.txt", "example3.txt"}
    for _, fileName := range fileNames {
        f, err := os.Open(fileName)
        check(err)
        b, err := ioutil.ReadAll(f)
        check(err)
        f.Close()
        text := string(b)
        fmt.Printf("Contents of %s:\n\n%s\n\n", fileName, text)
        m := r.FindAllStringIndex(text, -1)
        headers := make([]header, len(m))
        if len(m) > 0 {
            for i, p := range m {
                headers[i] = header{p[0], p[1] - 1, ""}
            }
            m2 := r.FindAllStringSubmatch(text, -1)
            for i, s := range m2 {
                headers[i].lang = strings.ToLower(s[1])
            }
        }
        last := len(headers) - 1
        if last == -1 { // if there are no headers in the file add a dummy one
            headers = append(headers, header{-1, -1, "no language"})
            last = 0
        }
        m3 := r2.FindAllStringIndex(text, -1)
        for _, p := range m3 {
            if p[1] < headers[0].start {
                add2bmap("no language", fileName)
            } else if p[0] > headers[last].end {
                add2bmap(headers[last].lang, fileName)
            } else {
                for i := 0; i < last; i++ {
                    if p[0] > headers[i].end && p[0] < headers[i+1].start {
                        add2bmap(headers[i].lang, fileName)
                        break
                    }
                }
            }
        }
    }
    fmt.Println("Results:\n")
    count := 0
    for _, v := range bmap {
        count += v.count
    }
    fmt.Printf(" %d bare language tags.\n\n", count)
    for k, v := range bmap {
        fmt.Printf("  %d in %-11s %v\n", v.count, k, *v.names)
    }
}
```


```txt

Contents of example.txt:

Description

<lang>Pseudocode
```



## C


```C
printf("Hello world!\n");
```



## Perl

<lang>print "Hello world!\n"
```



Contents of example2.txt:


## C

<lang>printf("Hello again world!\n");
```



## Perl


```perl
print "Hello again world!\n"
```



Contents of example3.txt:

<lang>Some more pseudocode
```



## C

<lang>printf("Hello once again world!\n");
```



Results:

 5 bare language tags.

  2 in no language [example.txt example3.txt]
  1 in perl        [example.txt]
  2 in c           [example2.txt example3.txt]

```



## Haskell

There are actually many different Regex packages available for Haskell.  For this example, I chose TDFA, a very fast POSIX ERE engine.  To change engines, simply change the import statement.  If you use a Perl-style RE engine, you'll have to modify the expressions slightly.

This solution can be compiled into a program that will either take space-delimited list of files as its argument, or take input from STDIN if no arguments are provided.  Additionally, if you specify the -w flag in the first argument, it will take a list of Rosetta Code wiki pages and search them. Note that the page names must be as they appear in your URL bar -- underscores in place of spaces.


```Haskell
import System.Environment
import Network.HTTP
import Text.Printf
import Text.Regex.TDFA
import Data.List
import Data.Array
import qualified Data.Map as Map

{-| Takes a string and cuts out the text matched in the MatchText array.  -}
splitByMatches :: String -> [MatchText String] -> [String]
splitByMatches str matches  =  foldr splitHead [str] matches
    where splitHead match acc  =  before:after:(tail acc)
            where before  =  take (matchOffset).head$ acc
                  after  =  drop (matchOffset + matchLen).head$ acc
                  matchOffset  =  fst.snd.(!0)$ match
                  matchLen  =  snd.snd.(!0)$ match

{-| Takes a string and counts the number of time a valid, but bare, lang tag
    appears.  It does not attempt to ignore valid tags inside lang blocks.  -}
countBareLangTags :: String -> Int
countBareLangTags  =  matchCount (makeRegex "<lang[[:space:]]*>" :: Regex)

{-| Takes a string and counts the number of bare lang tags per section of the
    text.  All tags before the first section are put into the key "".  -}
countByLanguage :: String -> Map.Map String Int
countByLanguage str  =  Map.fromList.filter ((>0).snd)$ zip langs counts
    where counts  =  map countBareLangTags.splitByMatches str$ allMatches
          langs  =  "":(map (fst.(!1)) allMatches)
          allMatches  =  matchAllText (makeRegex headerRegex :: Regex) str
          headerRegex  =  "==[[:space:]]*{{[[:space:]]*header[[:space:]]*\\|[[:space:]]*([^ }]*)[[:space:]]*}}[^=]*=="

main = do
    args <- getArgs
    (contents, files) <- if length args == 0 then do
        -- If there aren't arguments, read from stdin
            content  <-  getContents
            return ([content],[""])
        else if length args == 1 then do
        -- If there's only one argument, read the file, but don't display
        -- the filename in the results.
            content  <-  readFile (head args)
            return ([content],[""])
        else if (args !! 0) == "-w" then do
        -- If there's more than one argument and the first one is the -w option,
        -- use the rest of the arguments as page titles and load them from the wiki.
            contents  <-  mapM getPageContent.tail$ args
            return (contents, if length args > 2 then tail args else [""])
        else do
        -- Otherwise, read all the files and display their file names.
            contents  <-  mapM readFile args
            return (contents, args)
    let tagsPerLang  =  map countByLanguage contents
    let tagsWithFiles  =  zipWith addFileToTags files tagsPerLang
    let combinedFiles  =  Map.unionsWith combine tagsWithFiles
    printBareTags combinedFiles
        where addFileToTags file  =  Map.map (flip (,) [file])
              combine cur next  =  (fst cur + fst next, snd cur ++ snd next)

printBareTags :: Map.Map String (Int,[String]) -> IO ()
printBareTags tags  =  do
    let numBare  =  Map.foldr ((+).fst) 0 tags
    printf "%d bare language tags:\n\n" numBare
    mapM_ (\(lang,(count,files)) ->
        printf "%d in %s%s\n" count
                              (if lang == "" then "no language" else lang)
                              (filesString files)
        ) (Map.toAscList tags)

filesString :: [String] -> String
filesString []  =  ""
filesString ("":rest)  =  filesString rest
filesString files  =  " ("++listString files++")"
    where listString [file]  =  "[["++file++"]]"
          listString (file:files)  =  "[["++file++"]], "++listString files

getPageContent :: String -> IO String
getPageContent title  =  do
    response  <-  simpleHTTP.getRequest$ url
    getResponseBody response
        where url  =  "http://rosettacode.org/mw/index.php?action=raw&title="++title
```


Here are the input files I used to test:


```txt
<nowiki>
example1.wiki
-------------------------------------------------------------
Description

<lang>Pseudocode
```



## C


```C
printf("Hello world!\n");
```





## Perl

<lang>print "Hello world!\n"
```

</nowiki>
```


```txt
<nowiki>
example2.wiki
-------------------------------------------------------------
Description

<lang>Pseudocode
```



## C

<lang>printf("Hello world!\n");
```



## Perl

<lang>print "Hello world!\n"
```


```Perl
print "Goodbye world!\n"
```



## Haskell

<lang>hubris lang = "I'm so much better than a "++lang++" programmer because I program in Haskell."
```

</nowiki>
```


And the output:


```txt
<nowiki>
6 bare language tags:

2 in no language ([[example1.wiki]], [[example2.wiki]])
1 in C ([[example2.wiki]])
1 in Haskell ([[example2.wiki]])
2 in Perl ([[example1.wiki]], [[example2.wiki]])
</nowiki>
```


Additionally, I tested with [[100_doors]] and [[Huffman_coding]]. The following resulted:

```txt

5 bare language tags:

1 in no language ([[100_doors]])
1 in C ([[Huffman_coding]])
1 in CoffeeScript ([[Huffman_coding]])
1 in Perl ([[Huffman_coding]])
1 in PostScript ([[100_doors]])

```


==Icon and {{header|Unicon}}==

The following is a Unicon-specific solution.

```unicon
import Utils		# To get the FindFirst class

procedure main()
    keys := ["{{header|","<lang>"]
    lang := "No language"
    tags := table(0)
    total := 0

    ff := FindFirst(keys)
    f := reads(&input, -1)

    f ? while tab(ff.locate()) do {
        if "{{header|" == 1(ff.getMatch(), ff.moveMatch()) then lang := map(tab(upto("}}")))
        else (tags[lang] +:= 1, total +:= 1)
        }

    write(total," bare language tags:\n")
    every pair := !sort(tags) do write(pair[2]," in ",pair[1])
end
```


Sample run using example given in problem statement:

```txt

->rcfblt <rcfblt.in
2 bare language tags:

1 in No language
1 in perl
->

```



## Julia


```julia
using Gumbo, AbstractTrees, HTTP, JSON, Dates

rosorg = "http://rosettacode.org"
qURI = "/mw/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=json"
qdURI = "/mw/api.php?action=query&list=categorymembers&cmtitle=Category:Draft_Programming_Tasks&cmlimit=500&format=json"
sqURI = "http://www.rosettacode.org/mw/index.php?title="

function topages(js, v)
    for d in js["query"]["categorymembers"]
        push!(v, sqURI * HTTP.Strings.escapehtml(replace(d["title"], " " => "_")) * "&action=raw")
    end
end

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
    totalcount = 0
    langcount = Dict{String, Int}()
    for pag in wpages
        count = 0
        checked = 0
        try
            response = HTTP.request("GET", pag)
            if response.status == 200
                doc = parsehtml(String(response.body))
                lasttext = ""
                for elem in StatelessBFS(doc.root)
                    if typeof(elem) != HTMLText
                        if tag(elem) == :lang
                            if isempty(attrs(elem))
                                count += 1
                                if lasttext != ""
                                    if verbose
                                        println("Missing lang attibute for lang $lasttext")
                                    end
                                    if !haskey(langcount, lasttext)
                                        langcount[lasttext] = 1
                                    else
                                        langcount[lasttext] += 1
                                    end
                                end
                            else
                                checked += 1
                            end
                        end
                    else
                        m = match(r"header\|(.+)}}==", text(elem))
                        lasttext = (m == nothing) ? "" : m.captures[1]
                    end
                end
            end
        catch y
            if verbose
                println("Page $pag is not loaded or found: $y.")
            end
            continue
        end
        if count > 0 && verbose
            println("Page $pag had $count bare lang tags.")
        end
        totalcount += count
    end
    println("Total bare tags: $totalcount.")
    for k in sort(collect(keys(langcount)))
        println("Total bare <lang> for language $k: $(langcount[k])")
    end
end

println("Programming examples at $(DateTime(now())):")
qURI |> getpages |> processtaskpages

println("\nDraft programming tasks:")
qdURI |> getpages |> processtaskpages

```
```txt

Programming examples at 2019-02-19T06:33:49.951:
Total bare tags: 1044.
Total bare <lang> for language 360 Assembly: 2
Total bare <lang> for language 6502 Assembly: 1
Total bare <lang> for language 6800 Assembly: 2
Total bare <lang> for language ALGOL 60: 1
Total bare <lang> for language ALGOL 68: 1
Total bare <lang> for language ALGOL-M: 1
Total bare <lang> for language APL: 1
Total bare <lang> for language ATS: 3
Total bare <lang> for language Aime: 1
Total bare <lang> for language AutoIt: 1
Total bare <lang> for language BASIC256: 2
Total bare <lang> for language BBC BASIC: 1
Total bare <lang> for language Batch File: 2
Total bare <lang> for language Bracmat: 3
Total bare <lang> for language Burlesque: 1
Total bare <lang> for language C: 1
Total bare <lang> for language C sharp|C#: 1
Total bare <lang> for language COBOL: 2
Total bare <lang> for language Caché ObjectScript: 2
Total bare <lang> for language Ceylon: 1
Total bare <lang> for language Chapel: 1
Total bare <lang> for language ChucK: 11
Total bare <lang> for language CoffeeScript: 1
Total bare <lang> for language Cubescript: 1
Total bare <lang> for language DCL: 3
Total bare <lang> for language DEC BASIC-PLUS: 1
Total bare <lang> for language Dart: 3
Total bare <lang> for language Delphi: 1
Total bare <lang> for language ECL: 6
Total bare <lang> for language ERRE: 23
Total bare <lang> for language EchoLisp: 1
Total bare <lang> for language Erlang: 2
Total bare <lang> for language Euler Math Toolbox: 6
Total bare <lang> for language Euphoria: 1
Total bare <lang> for language Excel: 2
Total bare <lang> for language Factor: 16
Total bare <lang> for language Forth: 16
Total bare <lang> for language Fortran: 2
Total bare <lang> for language FreeBASIC: 1
Total bare <lang> for language Futhark: 1
Total bare <lang> for language FutureBasic: 2
Total bare <lang> for language GAP: 1
Total bare <lang> for language GFA Basic: 10
Total bare <lang> for language Gambas: 1
Total bare <lang> for language Haskell: 2
Total bare <lang> for language Icon}} and {{header|Unicon: 4
Total bare <lang> for language Idris: 1
Total bare <lang> for language Io: 2
Total bare <lang> for language J: 1
Total bare <lang> for language Java: 1
Total bare <lang> for language JavaScript: 2
Total bare <lang> for language Julia: 2
Total bare <lang> for language K: 1
Total bare <lang> for language LOLCODE: 1
Total bare <lang> for language Latitude: 1
Total bare <lang> for language Liberty BASIC: 1
Total bare <lang> for language Limbo: 1
Total bare <lang> for language Lua: 1
Total bare <lang> for language M2000 Interpreter: 2
Total bare <lang> for language M4: 1
Total bare <lang> for language MUMPS: 1
Total bare <lang> for language Maple: 6
Total bare <lang> for language Mathematica: 14
Total bare <lang> for language Mathematica}} / {{header|Wolfram Language: 8
Total bare <lang> for language Mathprog: 3
Total bare <lang> for language Maxima: 3
Total bare <lang> for language Mercury: 20
Total bare <lang> for language Modula-2: 1
Total bare <lang> for language N/t/roff: 1
Total bare <lang> for language Nim: 2
Total bare <lang> for language Octave: 1
Total bare <lang> for language PARI/GP: 1
Total bare <lang> for language PHL: 1
Total bare <lang> for language PL/I: 15
Total bare <lang> for language Perl: 6
Total bare <lang> for language Perl 6: 1
Total bare <lang> for language PicoLisp: 4
Total bare <lang> for language PostScript: 13
Total bare <lang> for language ProDOS: 2
Total bare <lang> for language QB64: 1
Total bare <lang> for language R: 4
Total bare <lang> for language REXX: 1
Total bare <lang> for language Racket: 6
Total bare <lang> for language Raven: 1
Total bare <lang> for language Ring: 5
Total bare <lang> for language Rust: 4
Total bare <lang> for language SAS: 1
Total bare <lang> for language Scala: 2
Total bare <lang> for language Scheme: 3
Total bare <lang> for language Scilab: 41
Total bare <lang> for language Simula: 1
Total bare <lang> for language Stata: 5
Total bare <lang> for language Swift: 5
Total bare <lang> for language TI-83 BASIC: 1
Total bare <lang> for language TI-89 BASIC: 1
Total bare <lang> for language Trith: 1
Total bare <lang> for language UNIX Shell: 1
Total bare <lang> for language Unicon: 2
Total bare <lang> for language Ursa: 1
Total bare <lang> for language Visual Basic .NET: 1
Total bare <lang> for language Viua VM assembly: 1
Total bare <lang> for language Wart: 1
Total bare <lang> for language XSLT: 1
Total bare <lang> for language XSLT 2.0: 2
Total bare <lang> for language ooRexx: 2
Total bare <lang> for language smart BASIC: 1
Total bare <lang> for language uBasic/4tH: 72
Total bare <lang> for language x86 Assembly: 1
Total bare <lang> for language zkl: 2
Total bare <lang> for language zonnon: 1
Total bare <lang> for language MK-61/52: 62

Draft programming tasks:
Total bare tags: 30.
Total bare <lang> for language 1C: 1
Total bare <lang> for language AppleScript: 1
Total bare <lang> for language CoffeeScript: 1
Total bare <lang> for language Dart: 1
Total bare <lang> for language Factor: 2
Total bare <lang> for language Forth: 2
Total bare <lang> for language Glagol: 1
Total bare <lang> for language M2000 Interpreter: 1
Total bare <lang> for language Mathematica: 1
Total bare <lang> for language Racket: 1
Total bare <lang> for language uBasic/4tH: 1
Total bare <lang> for language MK-61/52: 2

```



## Maple


```Maple
#Did not count the tasks where languages tasks are properly closed
add_lan := proc(language, n, existence, languages, pos)
	if (assigned(existence[language])) then
		existence[language] += n:
		return pos;
	else
		existence[language] := n:
		languages(pos) := language:
		return pos+1;
	end if;
end proc:
count_tags := proc(tasks, pos)
	local task, url, txt, header_tags, close_tags, close_len, header_len, occurence, i, pos_copy;
	pos_copy := pos:
	for task in tasks do
		url := cat("http://www.rosettacode.org/mw/index.php?title=", StringTools:-Encode(StringTools:-SubstituteAll(task["title"], " ", "_"), 'percent'), "&action=raw"):
		txt := URL:-Get(url):
		header_tags := [StringTools:-SearchAll("=={{header|", txt)]:
		close_tags := [StringTools:-SearchAll("}}==",txt)]:
		close_len := numelems(close_tags):
		header_len := numelems(header_tags):
		if header_len = 0 then
			break;
		end if;
		if (not header_len = close_len) then
			printf("%s is not counted since some language tags are not properly closed.\n", task["title"]);
			break;
		end if;
		occurence := numelems([StringTools:-SearchAll("<lang>", txt[1..header_tags[1]])]):
		if occurence > 0 then
			pos_copy := add_lan("no languages", occurence, existence, languages, pos_copy):
		end if:
		if close_len > 1 then
			for i from 2 to close_len do
				occurence := numelems([StringTools:-SearchAll("<lang>", txt[header_tags[i-1]..header_tags[i]])]):
				if occurence > 0 then
					pos_copy := add_lan(txt[header_tags[i-1]+11..close_tags[i-1]-1], occurence, existence, languages, pos_copy):
				end if:
			end do:
			occurence := numelems([StringTools:-SearchAll("<lang>", txt[header_tags[-1]..])]):
			if occurence > 0 then
				pos_copy := add_lan(txt[header_tags[-1]+11..close_tags[-1]-1], occurence, existence, languages, pos_copy):
			end if:
		end if:
	end do:
	return pos_copy:
end proc:

existence := table():
languages := Array():
pos := 1:
#go through every task
x := JSON:-ParseFile("http://rosettacode.org/mw/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=10&format=json"):
pos := count_tags(x["query"]["categorymembers"], pos):
while(assigned(x["continue"]["cmcontinue"])) do
	continue := x["continue"]["cmcontinue"]:
	more_tasks:= cat("http://rosettacode.org/mw/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=10&format=json", "&continue=", x["continue"]["continue"], "&cmcontinue=", x["continue"]["cmcontinue"]):
	x := JSON:-ParseFile(more_tasks):
	pos := count_tags(x["query"]["categorymembers"], pos):
end do:
#Prints out the table
total := 0:
for lan in languages do
	total += existence[lan]:
	printf("There are %d bare lang tags in %s\n", existence[lan], lan);
end do:
printf("Total number %d", total);

```

```txt
15 Puzzle Game is not counted since some language tags are not properly closed.
Abstract type is not counted since some language tags are not properly closed.
Almost prime is not counted since some language tags are not properly closed.
...
Zig-zag matrix is not counted since some language tags are not properly closed.
There are 1 bare lang tags in 4DOS Batch
There are 6 bare lang tags in Caché ObjectScript
There are 8 bare lang tags in PostScript
There are 34 bare lang tags in Scilab
There are 35 bare lang tags in uBasic/4tH
There are 1 bare lang tags in Ursa
There are 3 bare lang tags in PicoLisp
There are 15 bare lang tags in CoffeeScript
There are 29 bare lang tags in MK-61/52
There are 2 bare lang tags in APL
There are 10 bare lang tags in ERRE
There are 4 bare lang tags in Excel
There are 2 bare lang tags in LiveCode
There are 9 bare lang tags in Mercury
There are 3 bare lang tags in ECL
There are 3 bare lang tags in Maxima
There are 19 bare lang tags in PL/I
There are 4 bare lang tags in Ring
There are 1 bare lang tags in zonnon
There are 13 bare lang tags in Forth
There are 9 bare lang tags in J
There are 1 bare lang tags in Unicon
There are 3 bare lang tags in Java
There are 23 bare lang tags in C
There are 11 bare lang tags in Common Lisp
There are 3 bare lang tags in factor
There are 3 bare lang tags in Racket
There are 4 bare lang tags in N/t/roff
There are 3 bare lang tags in UNIX Shell
There are 3 bare lang tags in Scheme
There are 2 bare lang tags in Korn Shell
There are 4 bare lang tags in Fortran
There are 10 bare lang tags in C sharp|C#
There are 3 bare lang tags in Io
There are 2 bare lang tags in Erlang
There are 1 bare lang tags in F#
There are 2 bare lang tags in Bracmat
There are 3 bare lang tags in Rust
There are 1 bare lang tags in FreeBASIC
There are 1 bare lang tags in Gri
There are 2 bare lang tags in Simula
There are 1 bare lang tags in smart BASIC
There are 7 bare lang tags in Mathematica}} / {{header|Wolfram Language
There are 1 bare lang tags in Aime
There are 2 bare lang tags in GFA Basic
There are 1 bare lang tags in Visual Basic .NET
There are 3 bare lang tags in Perl 6
There are 3 bare lang tags in Swift
There are 1 bare lang tags in no languages
There are 2 bare lang tags in Maple
There are 1 bare lang tags in M4
There are 1 bare lang tags in FutureBasic
There are 1 bare lang tags in Potion
There are 2 bare lang tags in PowerShell
There are 2 bare lang tags in QB64
There are 2 bare lang tags in Batch File
There are 8 bare lang tags in ChucK
There are 8 bare lang tags in Euler Math Toolbox
There are 1 bare lang tags in gnuplot
There are 2 bare lang tags in ooRexx
There are 7 bare lang tags in Mathprog
There are 1 bare lang tags in PHP
There are 5 bare lang tags in Perl
There are 4 bare lang tags in Python
There are 1 bare lang tags in Haskell
There are 1 bare lang tags in jq
There are 7 bare lang tags in Mathematica
There are 2 bare lang tags in DCL
There are 1 bare lang tags in R
There are 5 bare lang tags in XSLT
There are 2 bare lang tags in Clojure
There are 1 bare lang tags in REXX
There are 3 bare lang tags in XSLT 2.0
There are 1 bare lang tags in Sinclair ZX81 BASIC
There are 2 bare lang tags in Stata
There are 1 bare lang tags in Wart
There are 1 bare lang tags in BBC BASIC
There are 1 bare lang tags in Euphoria
There are 1 bare lang tags in 6800 Assembly
There are 2 bare lang tags in Dart
There are 3 bare lang tags in Factor
There are 1 bare lang tags in F Sharp
There are 1 bare lang tags in zkl
There are 1 bare lang tags in Chapel
There are 1 bare lang tags in Ceylon
There are 1 bare lang tags in PARI/GP
There are 1 bare lang tags in C++
There are 1 bare lang tags in JavaScript
There are 1 bare lang tags in Powershell
There are 2 bare lang tags in Delphi
There are 1 bare lang tags in Gambas
There are 1 bare lang tags in ACL2
There are 1 bare lang tags in TypeScript
There are 1 bare lang tags in AutoHotkey
There are 1 bare lang tags in Elixir
There are 1 bare lang tags in Raven
There are 1 bare lang tags in 360 Assembly
There are 1 bare lang tags in LOLCODE
There are 1 bare lang tags in COBOL
Total number 416

```



## Mathematica


```Mathematica
tasks[page_: ""] :=
  Module[{res =
     Import["http://rosettacode.org/mw/api.php?format=xml&action=\
query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=\
500" <> page, "XML"]},
   If[MemberQ[res[[2, 3]], XMLElement["query-continue", __]],
    Join[res[[2, 3, 1, 3, 1, 3, All, 2, 3, 2]],
     tasks["&cmcontinue=" <> res[[2, 3, 2, 3, 1, 2, 1, 2]]]],
    res[[2, 3, 1, 3, 1, 3, All, 2, 3, 2]]]];
bareTags = # -> (# -> StringCount[#2, "<lang>"] &) @@@
      Partition[
       Prepend[StringSplit[
          Import["http://rosettacode.org/wiki?action=raw&title=" <>
            URLEncode[#], "Text"],
          Shortest["=={{header|" ~~ x__ ~~ "}}=="] :> x],
         "no language"] //. {a___,
          multi_String?StringContainsQ["}}" ~~ ___ ~~ "{{header|"],
          bare_Integer, b___} :> {a, StringSplit[multi, "}"][[1]],
          bare, StringSplit[multi, "|"][[-1]], bare, b}, 2] & /@
   tasks[];
Print[IntegerString[Total[Flatten[bareTags[[All, 2, All, 2]]]]] <>
   " bare language tags.\n"];
langCounts =
  Normal[Total /@
    GroupBy[Flatten[bareTags[[All, 2]]], Keys -> Values]];
Print[IntegerString[#2] <> " in " <> # <> " ([[" <>
     StringRiffle[
      Keys[Select[bareTags,
        Function[task, MemberQ[task[[2]], # -> _Integer?Positive]]]],
      "]], [["] <> "]])"] & @@@
  Select[SortBy[langCounts, Keys], #[[2]] > 0 &];
```

This script looks for all of the pages in [[:Category:Programming Tasks]], downloads them, and gets the bare tags per language. Then, it gets the total of all of them, prints it, and sums up the bare tags by language. Note that it doesn't check if the tags are in a block or even closed, so it picks up false positives (especially on this page.)
This the output on May 21, 2015.

```txt
686 bare language tags.

1 in 360 Assembly ([[FizzBuzz]])
1 in 4DOS Batch ([[100 doors]])
71 in MK-61/52 ([[Arithmetic-geometric mean]], [[Arithmetic-geometric mean/Calculate Pi]], [[Arithmetic/Complex]], [[Arithmetic/Integer]], [[Averages/Arithmetic mean]], [[Averages/Root mean square]], [[Balanced ternary]], [[Circles of given radius through two points]], [[Combinations and permutations]], [[Conditional structures]], [[Convert decimal number to rational]], [[Count in octal]], [[Day of the week]], [[Dot product]], [[Empty program]], [[Ethiopian multiplication]], [[Euler method]], [[Evaluate binomial coefficients]], [[Even or odd]], [[Execute a Markov algorithm]], [[Exponentiation operator]], [[Fibonacci sequence]], [[Find limit of recursion]], [[Greatest element of a list]], [[Haversine formula]], [[Higher-order functions]], [[Holidays related to Easter]], [[Horizontal sundial calculations]], [[Horner's rule for polynomial evaluation]], [[Integer comparison]], [[Integer sequence]], [[Jump anywhere]], [[Leap year]], [[Least common multiple]], [[Loops/Break]], [[Loops/Do-while]], [[Loops/Downward for]], [[Loops/For with a specified step]], [[Loops/Infinite]], [[Loops/While]], [[Main step of GOST 28147-89]], [[Middle three digits]], [[Modular inverse]], [[Monte Carlo methods]], [[Multifactorial]], [[Multiplication tables]], [[Nth root]], [[Pick random element]], [[Polynomial regression]], [[Primality by trial division]], [[Program termination]], [[Random numbers]], [[Real constants and functions]], [[Roots of a quadratic function]], [[Roots of unity]], [[Sequence of non-squares]], [[Standard deviation]], [[Sum and product of an array]], [[Sum digits of an integer]], [[Sum multiples of 3 and 5]], [[Sum of squares]], [[Ternary logic]], [[Towers of Hanoi]], [[Vector products]], [[Voronoi diagram]], [[Zero to the zero power]])
2 in 6502 Assembly ([[FizzBuzz]], [[String case]])
2 in 6800 Assembly ([[Hello world/Text]], [[Loops/Infinite]])
1 in ABAP ([[FizzBuzz]])
1 in ACL2 ([[Sorting algorithms/Quicksort]])
1 in Ada ([[Hofstadter Figure-Figure sequences]])
1 in ALGOL 60 ([[Trabb Pardo\[Dash]Knuth algorithm]])
1 in APL ([[Arithmetic/Complex]])
3 in AutoHotkey ([[Long multiplication]], [[Rosetta Code/Find bare lang tags]], [[Sorting algorithms/Strand sort]])
1 in AutoIt ([[Letter frequency]])
1 in BASIC256 ([[Greyscale bars/Display]])
2 in Batch File ([[Function definition]], [[Loops/For]])
1 in BBC BASIC ([[Loops/For]])
4 in Bracmat ([[Command-line arguments]], [[Greatest element of a list]], [[Integer sequence]], [[Runtime evaluation/In an environment]])
1 in Burlesque ([[Shell one-liner]])
46 in C ([[Atomic updates]], [[Balanced brackets]], [[Best shuffle]], [[Bulls and cows/Player]], [[Closures/Value capture]], [[Constrained random points on a circle]], [[Convert decimal number to rational]], [[Count the coins]], [[Cut a rectangle]], [[Deconvolution/2D+]], [[Distributed programming]], [[Entropy]], [[Euler method]], [[Evolutionary algorithm]], [[Execute a Markov algorithm]], [[Find common directory path]], [[First-class functions]], [[Fork]], [[Inverted index]], [[Last letter-first letter]], [[Longest string challenge]], [[Map range]], [[Multisplit]], [[Natural sorting]], [[Partial function application]], [[Permutation test]], [[Priority queue]], [[Probabilistic choice]], [[Pythagorean triples]], [[Roots of a quadratic function]], [[Rosetta Code/Find bare lang tags]], [[S-Expressions]], [[Self-describing numbers]], [[Sorting algorithms/Sleep sort]], [[Sorting algorithms/Strand sort]], [[Stem-and-leaf plot]], [[Strip control codes and extended characters from a string]], [[Text processing/Max licenses in use]], [[Thiele's interpolation formula]], [[Topological sort]], [[Truncatable primes]], [[Variable-length quantity]], [[Write float arrays to a text file]])
2 in Chapel ([[Associative array/Creation]], [[Numerical integration]])
15 in ChucK ([[Call an object method]], [[Factorial]], [[Function definition]], [[Hello world/Text]], [[Include a file]], [[Integer comparison]], [[Integer sequence]], [[Loops/Do-while]], [[Loops/Infinite]], [[Loops/While]], [[String concatenation]], [[Variables]])
8 in Clojure ([[Hello world/Newbie]], [[Hofstadter Q sequence]], [[Knuth's algorithm S]], [[Longest increasing subsequence]], [[One of n lines in a file]], [[Show the epoch]])
3 in COBOL ([[Hello world/Newbie]])
32 in CoffeeScript ([[Align columns]], [[Balanced brackets]], [[Bitwise operations]], [[Case-sensitivity of identifiers]], [[Constrained random points on a circle]], [[CRC-32]], [[Date format]], [[Delegates]], [[Flatten a list]], [[Integer sequence]], [[Inverted index]], [[Knight's tour]], [[Knuth's algorithm S]], [[Langton's ant]], [[Last Friday of each month]], [[Mutual recursion]], [[Nth root]], [[Numerical integration]], [[Ordered words]], [[Partial function application]], [[Power set]], [[Priority queue]], [[Problem of Apollonius]], [[Queue/Usage]], [[Roots of a function]], [[S-Expressions]], [[Sorting algorithms/Permutation sort]], [[Sorting algorithms/Sleep sort]], [[Spiral matrix]], [[Tree traversal]], [[Truncatable primes]], [[URL decoding]])
17 in Common Lisp ([[Balanced ternary]], [[Cut a rectangle]], [[Equilibrium index]], [[Evolutionary algorithm]], [[Execute a Markov algorithm]], [[Hofstadter Figure-Figure sequences]], [[Knuth's algorithm S]], [[Last letter-first letter]], [[One of n lines in a file]], [[Permutation test]], [[Pig the dice game/Player]], [[Pythagorean triples]], [[Rate counter]], [[Self-describing numbers]], [[Self-referential sequence]], [[Sorting algorithms/Strand sort]], [[Thiele's interpolation formula]])
1 in C sharp ([[Break OO privacy]])
16 in C sharp|C# ([[Closures/Value capture]], [[Color of a screen pixel]], [[Conway's Game of Life]], [[Equilibrium index]], [[Heronian triangles]], [[Infinity]], [[Inverted index]], [[Non-decimal radices/Input]], [[Numerical integration]], [[Pi]], [[Polymorphic copy]], [[Roots of a quadratic function]], [[Short-circuit evaluation]], [[Stack traces]], [[Variable-length quantity]])
2 in Cubescript ([[Caesar cipher]], [[FizzBuzz]])
1 in Dart ([[Mandelbrot set]])
2 in ÐÐ-61/52 ([[Ackermann function]], [[Sum of a series]])
1 in DEC BASIC-PLUS ([[Mandelbrot set]])
2 in Delphi ([[Polymorphic copy]], [[XML/XPath]])
1 in E ([[Shell one-liner]])
7 in ECL ([[Array concatenation]], [[Assertions]], [[CSV data manipulation]], [[Higher-order functions]], [[Repeat a string]], [[Return multiple values]])
1 in Eiffel ([[Hamming numbers]])
1 in Elixir ([[Sparkline in unicode]])
4 in Erlang ([[Apply a callback to an array]], [[Comma quibbling]], [[Fibonacci n-step number sequences]], [[Rosetta Code/Find bare lang tags]])
26 in ERRE ([[Address of a variable]], [[Apply a callback to an array]], [[Arithmetic-geometric mean]], [[Arithmetic/Complex]], [[Arithmetic/Integer]], [[Array concatenation]], [[Averages/Median]], [[Averages/Pythagorean means]], [[Averages/Root mean square]], [[Catalan numbers]], [[Circles of given radius through two points]], [[Create a two-dimensional array at runtime]], [[Empty program]], [[Evaluate binomial coefficients]], [[Execute a system command]], [[Greatest subsequential sum]], [[Increment a numerical string]], [[Integer sequence]], [[Jump anywhere]], [[Look-and-say sequence]], [[N-queens problem]], [[Random numbers]], [[Roots of a quadratic function]], [[Roots of unity]], [[Semiprime]])
10 in Euler Math Toolbox ([[Bitmap/Flood fill]], [[Grayscale image]], [[Greatest common divisor]], [[Greatest element of a list]], [[Identity matrix]], [[Resistor mesh]], [[Verify distribution uniformity/Naive]])
1 in Euphoria ([[Loops/Foreach]])
10 in Excel ([[A+B]], [[Arithmetic/Complex]], [[Averages/Median]], [[Boolean values]], [[Least common multiple]], [[Roman numerals/Encode]], [[String case]], [[String concatenation]], [[Sum of squares]], [[Temperature conversion]])
1 in F# ([[Comma quibbling]])
5 in Factor ([[Inverted index]], [[Mutual recursion]], [[Probabilistic choice]], [[Singly-linked list/Element definition]])
2 in Forth ([[Generate lower case ASCII alphabet]], [[String append]])
9 in Fortran ([[Bitwise operations]], [[Command-line arguments]], [[Dutch national flag problem]], [[Evolutionary algorithm]], [[Pascal matrix generation]], [[Price fraction]], [[Rot-13]], [[Sieve of Eratosthenes]])
1 in GAP ([[Sequence of non-squares]])
1 in gnuplot ([[Greatest common divisor]])
3 in Go ([[Address of a variable]], [[Bitmap/Midpoint circle algorithm]], [[Call a function]])
1 in Gri ([[Comments]])
9 in Haskell ([[Empty directory]], [[Hello world/Newbie]], [[Hough transform]], [[Rosetta Code/Find bare lang tags]])
4 in Icon}} and {{header|Unicon ([[Arithmetic-geometric mean]], [[Averages/Mean time of day]], [[Averages/Median]], [[Left factorials]])
24 in J ([[Calendar - for "REAL" programmers]], [[Discordian date]], [[First-class functions]], [[First-class functions/Use numbers analogously]], [[Galton box animation]], [[Guess the number]], [[Guess the number/With feedback]], [[Guess the number/With feedback (player)]], [[GUI enabling/disabling of controls]], [[JSON]], [[Morse code]], [[Narcissist]], [[Numeric error propagation]], [[Permutation test]], [[Pig the dice game]], [[Pythagorean triples]], [[Quaternion type]], [[Semordnilap]], [[Sorting algorithms/Bead sort]], [[XML/Output]], [[Yin and yang]])
5 in Java ([[A+B]], [[Balanced brackets]], [[Heronian triangles]], [[Parse an IP Address]], [[Y combinator]])
2 in JavaScript ([[Best shuffle]], [[Heronian triangles]])
2 in Julia ([[Show the epoch]], [[Sorting algorithms/Counting sort]])
2 in Korn Shell ([[Bitmap/Bresenham's line algorithm]])
1 in Lasso ([[Take notes on the command line]])
1 in Liberty BASIC ([[LZW compression]])
1 in Limbo ([[Hailstone sequence]])
1 in Logo ([[Dutch national flag problem]])
1 in LOLCODE ([[The Twelve Days of Christmas]])
2 in Lua ([[First-class functions]], [[Guess the number/With feedback (player)]])
1 in M4 ([[Dragon curve]])
1 in Maple ([[Case-sensitivity of identifiers]])
31 in Mathematica ([[Check Machin-like formulas]], [[Compile-time calculation]], [[Execute Brainfuck]], [[Execute HQ9+]], [[Find largest left truncatable prime in a given base]], [[Find limit of recursion]], [[Four bit adder]], [[Guess the number/With feedback (player)]], [[GUI component interaction]], [[Introspection]], [[K-means++ clustering]], [[LZW compression]], [[Number names]], [[Number reversal game]], [[Permutations by swapping]], [[Pig the dice game]], [[Rate counter]], [[Rep-string]], [[RSA code]], [[Send an unknown method call]], [[SHA-256]], [[Sudoku]], [[Take notes on the command line]], [[Tic-tac-toe]], [[Word wrap]], [[Yahoo! search interface]])
9 in Mathprog ([[Averages/Arithmetic mean]], [[Greatest subsequential sum]], [[Knapsack problem/Continuous]], [[Knight's tour]])
5 in Maxima ([[A+B]], [[Array concatenation]], [[Circles of given radius through two points]], [[Execute a system command]], [[Set]])
1 in MAXScript ([[Bulls and cows]])
21 in Mercury ([[A+B]], [[Arithmetic/Integer]], [[Command-line arguments]], [[Concurrent computing]], [[Count in octal]], [[Execute a system command]], [[Formatted numeric output]], [[Hello world/Standard error]], [[Input loop]], [[Loop over multiple arrays simultaneously]], [[Loops/Downward for]], [[Loops/For]], [[Mutual recursion]], [[OpenGL]], [[Real constants and functions]], [[SEDOLs]], [[Sort an integer array]], [[String case]], [[Sum of squares]], [[Tokenize a string]], [[Vector products]])
1 in Modula-2 ([[Hello world/Web server]])
2 in MUMPS ([[Copy a string]], [[Integer comparison]])
1 in Nemerle ([[Short-circuit evaluation]])
3 in Nim ([[LZW compression]], [[Metaprogramming]])
2 in no language ([[Rosetta Code/Find bare lang tags]])
1 in OCaml ([[99 Bottles of Beer]])
2 in Octave ([[Quaternion type]])
2 in ooRexx ([[Greatest element of a list]], [[String comparison]])
3 in Order ([[First class environments]], [[Hailstone sequence]], [[Luhn test of credit card numbers]])
1 in OS/8 BASIC ([[Mandelbrot set]])
1 in PARI/GP ([[Conjugate transpose]])
18 in Perl ([[Box the compass]], [[Extend your language]], [[History variables]], [[Linear congruential generator]], [[Narcissist]], [[Numeric error propagation]], [[Parse an IP Address]], [[Priority queue]], [[Queue/Usage]], [[Rosetta Code/Find bare lang tags]], [[Set of real numbers]])
13 in Perl 6 ([[Concurrent computing]], [[Constrained genericity]], [[Create a two-dimensional array at runtime]], [[Dutch national flag problem]], [[Empty program]], [[Hofstadter-Conway $10,000 sequence]], [[Morse code]], [[Numerical integration]], [[Queue/Usage]], [[Rosetta Code/Find bare lang tags]], [[Write language name in 3D ASCII]])
1 in PHL ([[String concatenation]])
2 in PHP ([[Greatest subsequential sum]], [[Power set]])
5 in PicoLisp ([[ABC Problem]], [[AKS test for primes]], [[Generate lower case ASCII alphabet]], [[Left factorials]], [[Zeckendorf arithmetic]])
38 in PL/I ([[Array concatenation]], [[Assertions]], [[Bitmap/Bresenham's line algorithm]], [[Bitmap/Flood fill]], [[Call a foreign-language function]], [[Closest-pair problem]], [[Compile-time calculation]], [[Convert decimal number to rational]], [[Create a two-dimensional array at runtime]], [[Exceptions]], [[Formatted numeric output]], [[Generate lower case ASCII alphabet]], [[Handle a signal]], [[Hofstadter Figure-Figure sequences]], [[Hofstadter Q sequence]], [[Holidays related to Easter]], [[Image noise]], [[Linear congruential generator]], [[Multifactorial]], [[Multiplication tables]], [[N'th]], [[Narcissistic decimal number]], [[Palindrome detection]], [[Parsing/Shunting-yard algorithm]], [[Pascal's triangle]], [[Queue/Usage]], [[Range extraction]], [[Sequence of non-squares]], [[Sorting algorithms/Pancake sort]], [[Sorting algorithms/Selection sort]], [[Truncate a file]], [[Van der Corput sequence]], [[Zhang-Suen thinning algorithm]])
15 in PostScript ([[100 doors]], [[Arithmetic/Complex]], [[Arrays]], [[Averages/Arithmetic mean]], [[Averages/Pythagorean means]], [[Comments]], [[Flatten a list]], [[Higher-order functions]], [[Mutual recursion]], [[Sequence of non-squares]], [[String length]], [[Sum and product of an array]], [[Sum of a series]], [[Sum of squares]])
3 in PowerShell ([[Find common directory path]], [[Find limit of recursion]], [[Heronian triangles]])
2 in ProDOS ([[Dynamic variable names]], [[Menu]])
7 in Python ([[History variables]], [[Hofstadter Figure-Figure sequences]], [[Odd word problem]], [[Pythagorean triples]], [[Quine]], [[Sorting algorithms/Strand sort]], [[Strip control codes and extended characters from a string]])
6 in R ([[Find the missing permutation]], [[Heronian triangles]], [[Knapsack problem/0-1]], [[Number reversal game]], [[Power set]])
5 in Racket ([[Bernoulli numbers]], [[First-class functions]], [[Flipping bits game]], [[Heronian triangles]], [[Pi]])
1 in Raven ([[System time]])
8 in REXX ([[24 game]], [[Bitmap/Bresenham's line algorithm]], [[Forward difference]], [[Handle a signal]], [[Haversine formula]], [[Non-decimal radices/Convert]], [[Program termination]], [[Variables]])
2 in Ruby ([[FizzBuzz]], [[Rosetta Code/Find bare lang tags]])
2 in Rust ([[Command-line arguments]], [[Fibonacci n-step number sequences]])
1 in Scala ([[Rosetta Code/Find bare lang tags]])
15 in Scheme ([[Averages/Pythagorean means]], [[Bitmap]], [[Bitwise operations]], [[Conditional structures]], [[Function composition]], [[Horner's rule for polynomial evaluation]], [[Interactive programming]], [[Pascal's triangle]], [[Reduced row echelon form]], [[Sieve of Eratosthenes]])
27 in Scilab ([[Ackermann function]], [[AKS test for primes]], [[Catalan numbers/Pascal's triangle]], [[Comments]], [[Factorial]], [[Fibonacci sequence]], [[Gamma function]], [[Hailstone sequence]], [[Loops/Break]], [[Loops/Continue]], [[Loops/Do-while]], [[Loops/Downward for]], [[Loops/For]], [[Loops/For with a specified step]], [[Loops/Foreach]], [[Loops/Infinite]], [[Loops/N plus one half]], [[Loops/Nested]], [[Loops/While]], [[Lucas-Lehmer test]], [[Monty Hall problem]], [[Multiplication tables]], [[Sorting algorithms/Bubble sort]], [[Spiral matrix]], [[Standard deviation]], [[String concatenation]])
1 in Simula ([[Comments]])
5 in Swift ([[Create a two-dimensional array at runtime]], [[Guess the number/With feedback (player)]], [[Repeat a string]], [[Sierpinski triangle]], [[Sum of a series]])
1 in Tcl ([[Rosetta Code/Find bare lang tags]])
19 in uBasic/4tH ([[AKS test for primes]], [[Box the compass]], [[Calendar]], [[Chinese remainder theorem]], [[Dinesman's multiple-dwelling problem]], [[Hamming numbers]], [[Linear congruential generator]], [[Mandelbrot set]], [[N'th]], [[Pascal's triangle]], [[Primality by trial division]], [[Rock-paper-scissors]], [[Roman numerals/Encode]], [[Sierpinski triangle]], [[Stem-and-leaf plot]], [[Subtractive generator]], [[Twelve statements]], [[Zeckendorf number representation]])
2 in UNIX Shell ([[Factors of an integer]], [[Simple database]])
1 in Wart ([[Loops/Downward for]])
1 in XSLT ([[N-queens problem]])
6 in XSLT 2.0 ([[CSV to HTML translation]], [[Longest string challenge]], [[Stable marriage problem]])
4 in XSLT 3.0 ([[Knight's tour]])
5 in zkl ([[Carmichael 3 strong pseudoprimes]], [[Hamming numbers]], [[Introspection]], [[Lucas-Lehmer test]], [[Non-continuous subsequences]])
```



## Perl

This is a simple implementation that does not attempt either extra credit.

```perl
my $lang = 'no language';
my $total = 0;
my %blanks = ();
while (<>) {
  if (m/<lang>/) {
    if (exists $blanks{lc $lang}) {
      $blanks{lc $lang}++
    } else {
      $blanks{lc $lang} = 1
    }
    $total++
  } elsif (m/==\s*\{\{\s*header\s*\|\s*([^\s\}]+)\s*\}\}\s*==/) {
    $lang = lc $1
  }
}

if ($total) {
	print "$total bare language tag" . ($total > 1 ? 's' : '') . ".\n\n";
	while ( my ($k, $v) = each(%blanks) ) {
		print "$k in $v\n"
	}
}
```



## Perl 6

The only tricky thing here is the use of the <tt>ms</tt> form of match, short for <tt>m:sigspace</tt>.  This causes whitespace in the regex to be considered "significant", that is, it matches optional whitespace at those positions, as if you'd put <tt>\s*</tt> there.  Of course, the regexes themselves are in Perl 6 syntax, which is quite different from Perl 5 regex syntax (and arguably much cleaner). Regex syntax is perhaps the area in which Perl 6 diverges most from Perl 5.

```perl6
my $lang = '(no language)';
my $total = 0;
my %blanks;

for lines() {
  when / '<lang>' / {
    %blanks{$lang}++;
    $total++;
  }
  when ms/ '==' '{{' 'header' '|' ( <-[}]>+? ) '}}' '==' / {
    $lang = $0.lc;
  }
}

say "$total bare language tag{ 's' if $total != 1 }\n";
say .value, ' in ', .key for %blanks.sort;
```

```txt
2 bare language tags

1 in (no language)
1 in perl
```



## Phix

Both extra credits. Would probably benefit from excluding &lt;pre&gt;&lt;/pre&gt; sections first.

```Phix
-- demo\rosetta\Find_bare_lang_tags.exw
--
--  Finds/counts no of "<lang>" as opposed to eg "
```Phix
" tags.
--  Since downloading all the pages can be very slow, this uses a cache.
--
constant include_drafts = true,
         sort_by_task = false,
         sort_by_lang = not sort_by_task    -- (one or t'other)

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
        if task!="Rosetta_Code/Find_bare_lang_tags" then -- not this one!
            tasks = append(tasks,task)
        end if
--      if length(tasks)>10 then exit end if    -- (debug aid)
        start = finish+1
    end while
    return tasks
end function

constant {html,ascii} = columnize({{"%2A","*"},
                                   {"%3A",":"},
                                   {"%27","'"},
                                   {"%2B","+"},
                                   {"%22","\""},
                                   {"%E2%80%93","-"},
                                   {"%E2%80%99","'"},
                                   {"%C3%A8","e"},
                                   {"%C3%A9","e"}})

function html_clean(string s)
    return substitute_all(s,html,ascii)
end function

constant {utf8,ansi} = columnize({{x"E28093","-"},
                                  {x"E28099","'"},
                                  {x"C3A8","e"},
                                  {x"C3A9","e"},
                                  {x"D09A","K"},
                                  {x"D09C","M"}})

function utf8_clean(string s)
    return substitute_all(s,utf8,ansi)
end function

function multi_lang(sequence s)
-- Convert eg {"Algol","Algol","C","C","C"} to "Algol[2],C[3]"
    integer i = 1, j = 2
    while i<length(s) do
        if s[i]=s[j] then
            while j<length(s) and s[i]=s[j+1] do j+=1 end while
            s[i..j] = {sprintf("%s[%d]",{s[i],j-i+1})}
        end if
        i += 1
        j = i+1
    end while
    return join(s,",")
end function

function multi_task(sequence s, tasks)
-- Similar to multi_lang() but with task[indexes]
    integer i = 1, j = 2
    while i<=length(s) do
        integer si = s[i]
        string tsi = html_clean(tasks[si])
        if j<=length(s) and si=s[j] then
            while j<length(s) and si=s[j+1] do j+=1 end while
            s[i..j] = {sprintf("%s[%d]",{tsi,j-i+1})}
        else
            s[i] = tsi
        end if
        i += 1
        j = i+1
    end while
    if length(s)>8 then s[4..-4] = {"..."} end if
    return join(s,",")
end function

function find_bare_lang_tags()
    -- note this lot use web scraping (as cribbed from a similar task) ...
    sequence tasks = dewiki(open_category("Programming_Tasks"))
    if include_drafts then
        tasks &= dewiki(open_category("Draft_Programming_Tasks"))
        tasks = sort(tasks)
    end if
    -- ... whereas the individual tasks use the web api instead (3x smaller/faster)
    integer total_count = 0,
            lt = length(tasks),
            kept = 0
    progress("%d tasks found\n",{lt})
    sequence task_langs = {},
             task_counts = iff(sort_by_task?repeat(0,lt):{}),
             task_things = iff(sort_by_task?repeat({},lt):{})
    for i=1 to length(tasks) do
        string ti = tasks[i],
               url = sprintf("http://rosettacode.org/mw/index.php?title=%s&action=raw",{ti}),
               contents = open_download(ti&".raw",url),
               this
        integer count = 0, start = 1, header
        while true do
            start = match(`<lang>`,contents,start)
            if start=0 then exit end if
            -- look backward for the nearest header
            header = rmatch(`{{header|`,contents,start)
            if header=0 then
--              this = ""
                this = "no language"
            else
                header += length(`{{header|`)
                this = utf8_clean(contents[header..match(`}}`,contents,header)-1])
            end if
            if sort_by_lang then
                integer k = find(this,task_langs)
                if k=0 then
                    task_langs = append(task_langs,this)
                    task_things = append(task_things,{i})
                    task_counts = append(task_counts,1)
                else
                    task_things[k] = append(task_things[k],i)
                    task_counts[k] += 1
                end if
            else
                task_things[i] = append(task_things[i],this)
            end if
            count += 1
            start += length(`<lang>`)
        end while
        if count!=0 then
            if sort_by_task then
                task_counts[i] = count
            end if
            kept += 1
        end if
        progress("%d tasks kept, %d to go\r",{kept,lt-i})
        total_count += count
        if get_key()=#1B then progress("escape keyed\n") exit end if
    end for
    if curl!=NULL then
        curl_easy_cleanup(curl)
        free(pErrorBuffer)
        curl = NULL
        pErrorBuffer = NULL
    end if
    progress("%d tasks with bare lang tags\n",{kept})
    sequence tags = custom_sort(task_counts,tagset(length(task_counts)))
    for i=length(tags) to 1 by -1 do
        integer ti = tags[i],
                tc = task_counts[ti]
        if tc=0 then exit end if
--if tc>5 then
        if sort_by_task then
            progress("%s %d (%s)\n",{html_clean(tasks[ti]),tc,multi_lang(task_things[ti])})
        else -- (sort_by_count)
            progress("%s %d (%s)\n",{task_langs[ti],tc,multi_task(task_things[ti],tasks)})
        end if
--end if
    end for
    return total_count
end function

progress("Total: %d\n",{find_bare_lang_tags()})
```

as of 26/7/19, sort_by_task:

```txt

1174 tasks found
505 tasks with bare lang tags
Hello_world/Newbie 13 (Clojure[3],COBOL[3],Haskell[4],JavaScript[3])
Variables 10 (Cache ObjectScript[4],ChucK[3],Forth,ooRexx,uBasic/4tH)
Knight's_tour 9 (CoffeeScript,Mathprog[4],XSLT[4])
Comments 8 (EasyLang,FreeBASIC,Gri,PostScript,Scilab,Simula[2],smart BASIC)
100_doors 8 (4DOS Batch,Cache ObjectScript[2],EasyLang,PostScript,Scilab,uBasic/4tH,Ursa)
...
Total: 1094

```

as of 26/7/19, sort_by_lang:

```txt

1174 tasks found
505 tasks with bare lang tags
uBasic/4tH 83 (100_doors,99_Bottles_of_Beer,AKS_test_for_primes,...,Zeckendorf_number_representation,Zero_to_the_zero_power,Zig-zag_matrix)
EasyLang 81 (100_doors,15_Puzzle_Game,A+B,...,Tic-tac-toe,Towers_of_Hanoi,User_input/Text)
MK-61/52 75 (Ackermann_function,Arithmetic-geometric_mean,Arithmetic-geometric_mean/Calculate_Pi,...,Vector_products,Voronoi_diagram,Zero_to_the_zero_power)
Scilab 58 (100_doors,15_Puzzle_Game,AKS_test_for_primes,...,Welch's_t-test,Yin_and_yang,Zig-zag_matrix)
C 48 (Atomic_updates,Balanced_brackets,Best_shuffle,...,UTF-8_encode_and_decode,Variable-length_quantity,Write_float_arrays_to_a_text_file)
...
Smalltalk 1 (Address_of_a_variable)
smart BASIC 1 (Comments)
4DOS Batch 1 (100_doors)
Total: 1094

```



## Racket

Note that this follows the task, but the output is completely bogus since the actual <tt>&lt;lang&gt;</tt> tags that it finds are in <tt>&lt;pre&gt;</tt> and in code...


```racket

#lang racket

(require net/url net/uri-codec json)

(define (get-text page)
  (define ((get k) x) (dict-ref x k))
  ((compose1 (get '*) car (get 'revisions) cdar hash->list (get 'pages)
             (get 'query) read-json get-pure-port string->url format)
   "http://rosettacode.org/mw/api.php?~a"
   (alist->form-urlencoded
    `([titles . ,page] [prop . "revisions"] [rvprop . "content"]
      [format . "json"] [action . "query"]))))

(define (find-bare-tags page)
  (define in (open-input-string (get-text page)))
  (define rx
    ((compose1 pregexp string-append)
     "<\\s*lang\\s*>|"
     "==\\s*\\{\\{\\s*header\\s*\\|\\s*([^{}]*?)\\s*\\}\\}\\s*=="))
  (let loop ([lang "no language"] [bare '()])
    (match (regexp-match rx in)
      [(list _ #f) (loop lang (dict-update bare lang add1 0))]
      [(list _ lang) (loop lang bare)]
      [#f (if (null? bare) (printf "no bare language tags\n")
              (begin (printf "~a bare language tags\n" (apply + (map cdr bare)))
                     (for ([b bare]) (printf "  ~a in ~a\n" (cdr b) (car b)))))])))

(find-bare-tags "Rosetta Code/Find bare lang tags")

```

```txt

8 bare language tags
  2 in no language
  4 in Perl
  1 in AutoHotkey
  1 in Tcl

```


===More-extra credit===
Add the following code at the bottom, run, watch results.

```racket

(define (get-category cat)
  (let loop ([c #f])
    (define t
      ((compose1 read-json get-pure-port string->url format)
       "http://rosettacode.org/mw/api.php?~a"
       (alist->form-urlencoded
        `([list . "categorymembers"] [cmtitle . ,(format "Category:~a" cat)]
          [cmcontinue . ,(and c (dict-ref c 'cmcontinue))]
          [cmlimit . "500"] [format . "json"] [action . "query"]))))
    (define (c-m key) (dict-ref (dict-ref t key '()) 'categorymembers #f))
    (append (for/list ([page (c-m 'query)]) (dict-ref page 'title))
            (cond [(c-m 'query-continue) => loop] [else '()]))))

(for ([page (get-category "Programming Tasks")])
  (printf "Page: ~a " page)
  (find-bare-tags page))

```



## Ruby

Quoting from the FAQ: "If you just want the raw wikitext without any other information whatsoever, it's best to use index.php's action=raw mode instead of the API"

```Ruby
require "open-uri"
require "cgi"

tasks  = ["Greatest_common_divisor", "Greatest_element_of_a_list", "Greatest_subsequential_sum"]
part_uri  = "http://rosettacode.org/wiki?action=raw&title="
Report = Struct.new(:count, :tasks)
result = Hash.new{|h,k| h[k] = Report.new(0, [])}

tasks.each do |task|
  puts "processing #{task}"
  current_lang = "no language"
  open(part_uri + CGI.escape(task)).each_line do |line|
    current_lang = Regexp.last_match["lang"] if /==\{\{header\|(?<lang>.+)\}\}==/ =~ line
    num_no_langs = line.scan(/<lang\s*>/).size
    if num_no_langs > 0 then
      result[current_lang].count += num_no_langs
      result[current_lang].tasks << task
    end
  end
end

puts "\n#{result.values.map(&:count).inject(&:+)} bare language tags.\n\n"
result.each{|k,v| puts "#{v.count} in #{k} (#{v.tasks})"}
```

```txt

processing Greatest_common_divisor
processing Greatest_element_of_a_list
processing Greatest_subsequential_sum

10 bare language tags.

2 in Euler Math Toolbox (["Greatest_common_divisor", "Greatest_element_of_a_list"])
1 in gnuplot (["Greatest_common_divisor"])
1 in Bracmat (["Greatest_element_of_a_list"])
2 in MK-61/52 (["Greatest_element_of_a_list", "Greatest_element_of_a_list"])
1 in ooRexx (["Greatest_element_of_a_list"])
2 in Mathprog (["Greatest_subsequential_sum", "Greatest_subsequential_sum"])
1 in PHP (["Greatest_subsequential_sum"])

```


## Scala

To analyse RosettaCode pages, invoke Java with <code>-Dhttp.agent=Anything</code> to work around CloudFlare blocking Java from accessing the RosettaCode site.

```Scala
// Map lines to a list of Option(heading -> task) for each bare lang tag found.
val headerFormat = "==[{]+header[|]([^}]*)[}]+==".r
val langFormat = "<lang([^>]*)>".r
def mapped(lines: Seq[String], taskName: String = "") = {
  var heading = ""
  for (line <- lines;
     head = headerFormat.findFirstMatchIn(line).map(_ group 1);
     lang = langFormat.findFirstMatchIn(line).map(_ group 1)) yield {
    if (head.isDefined) heading = head.get
    lang.map(_.trim).filter(_ == "").map(_ => heading -> taskName)
  }
}
// Group results as a Map(heading -> task1, task2, ...)
def reduced(results: Seq[Option[(String,String)]]) =
  results.flatten.groupBy(_._1).mapValues(_.unzip._2)

// Format each heading as "tasklist.size in heading (tasklist)"
def format(results: Map[String,Seq[String]]) = results.map{case (heading, tasks) =>
  val h = if (heading.length > 0) heading else "no langauge"
  val hmsg = s"${tasks.size} in $h"
  val t = tasks.filterNot(_ == "")
  val tmsg = if (t.isEmpty) "" else t.distinct.mkString(" (", ",", ")")
  hmsg + tmsg
}
def count(results: Map[String,Seq[String]]) = results.values.map(_.size).sum

// Single and multi-source support
case class BareLangFinder(source: scala.io.Source, taskName: String = "") {
  def map = mapped(source.getLines.toSeq, taskName)
  def mapReduce = reduced(map)
  def summary = format(mapReduce) mkString "\n"
}
def mapReduce(inputs: Seq[BareLangFinder]) = reduced(inputs.flatMap(_.map))
```

'''Examples:'''

```txt
val test = """
Description

<lang>Pseudocode
```



## C


```C
printf("Hello world!\n");
```



## Perl

<lang>print "Hello world!\n"
```

"""

println(BareLangFinder(scala.io.Source.fromString(test)).summary)

//  System.setProperty("http.agent", "RosettaCode/1.0")
val tasks = List("Greatest_common_divisor", "Greatest_element_of_a_list", "Greatest_subsequential_sum")
val inputs = for (task <- tasks; url = "http://rosettacode.org/wiki?action=raw&title=" + task)
yield BareLangFinder(scala.io.Source.fromURL(url), task)
val bare = mapReduce(inputs)
println
println(s"${count(bare)} bare language tags in ${tasks.size} tasks:")
println(format(bare) mkString "\n")
```

```txt
1 in Perl
1 in no langauge

10 bare language tags in 3 tasks:
2 in Mathprog (Greatest_subsequential_sum)
1 in gnuplot (Greatest_common_divisor)
2 in MK-61/52 (Greatest_element_of_a_list)
1 in Bracmat (Greatest_element_of_a_list)
1 in PHP (Greatest_subsequential_sum)
2 in Euler Math Toolbox (Greatest_common_divisor,Greatest_element_of_a_list)
1 in ooRexx (Greatest_element_of_a_list)
```



## Tcl

For all the extra credit (note, takes a substantial amount of time due to number of HTTP requests):
```tcl
package require Tcl 8.5
package require http
package require json
package require textutil::split
package require uri

proc getUrlWithRedirect {base args} {
    set url $base?[http::formatQuery {*}$args]
    while 1 {
	set t [http::geturl $url]
	if {[http::status $t] ne "ok"} {
	    error "Oops: url=$url\nstatus=$s\nhttp code=[http::code $token]"
	}
	if {[string match 2?? [http::ncode $t]]} {
	    return $t
	}
	# OK, but not 200? Must be a redirect...
	set url [uri::resolve $url [dict get [http::meta $t] Location]]
	http::cleanup $t
    }
}

proc get_tasks {category} {
    global cache
    if {[info exists cache($category)]} {
	return $cache($category)
    }
    set query [dict create cmtitle Category:$category]
    set tasks [list]

    while {1} {
	set response [getUrlWithRedirect http://rosettacode.org/mw/api.php \
		action query list categorymembers format json cmlimit 500 {*}$query]

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
        dict set query cmcontinue $continue_task
    }
    return [set cache($category) $tasks]
}
proc getTaskContent task {
    set token [getUrlWithRedirect http://rosettacode.org/mw/index.php \
	    title $task action raw]
    set content [http::data $token]
    http::cleanup $token
    return $content
}

proc init {} {
    global total count found
    set total 0
    array set count {}
    array set found {}
}
proc findBareTags {pageName pageContent} {
    global total count found
    set t {{}}
    lappend t {*}[textutil::split::splitx $pageContent \
	    {==\s*\{\{\s*header\s*\|\s*([^{}]+?)\s*\}\}\s*==}]
    foreach {sectionName sectionText} $t {
	set n [regexp -all {<lang>} $sectionText]
	if {!$n} continue
	incr count($sectionName) $n
	lappend found($sectionName) $pageName
	incr total $n
    }
}
proc printResults {} {
    global total count found
    puts "$total bare language tags."
    if {$total} {
	puts ""
	if {[info exists found()]} {
	    puts "$count() in task descriptions\
		    (\[\[[join $found() {]], [[}]\]\])"
	    unset found()
	}
	foreach sectionName [lsort -dictionary [array names found]] {
	    puts "$count($sectionName) in $sectionName\
		    (\[\[[join $found($sectionName) {]], [[}]\]\])"
	}
    }
}

init
set tasks [get_tasks Programming_Tasks]
#puts stderr "querying over [llength $tasks] tasks..."
foreach task [get_tasks Programming_Tasks] {
    #puts stderr "$task..."
    findBareTags $task [getTaskContent $task]
}
printResults
```



## zkl

Uses shared library cURL.

```zkl
var [const] CURL=Import("zklCurl"),
   partURI="http://rosettacode.org/wiki?action=raw&title=%s",
   langRE=RegExp(0'!\s*==\s*{{\s*header\s*|(.+)}}!),  // == {{ header | zkl }}
   emptyRE=RegExp(0'!<lang\s*>!);

fcn findEmptyTags(a,b,c,etc){  // -->[lang:(task,task...)]
   results:=Dictionary();
   foreach task in (vm.arglist){
      println("processing ",task);
      currentLang:="";
      page:=CURL().get(partURI.fmt(CURL.urlEncode(task)));
      foreach line in (page[0]){
	 if(langRE.search(line,True)){
	    lang:=langRE.matched[1].strip();
	    if(lang) currentLang=lang;
	 }
	 if(emptyRE.matches(line,True)) results.appendV(currentLang,task);
      }
   }
   results
}
```


```zkl
results:=findEmptyTags("Greatest_common_divisor", "Greatest_element_of_a_list",
		      "Greatest_subsequential_sum");
println("\n%d bare language tags:".fmt(results.values.apply("len").sum(0)));
foreach lang in (results.keys.sort()){
   tasks:=results[lang];
   println("%d in %s: %s".fmt(tasks.len(),lang,tasks.concat(",")));
}
```

```txt

processing Greatest_common_divisor
processing Greatest_element_of_a_list
processing Greatest_subsequential_sum

14 bare language tags:
1 in Bracmat: Greatest_element_of_a_list
1 in ERRE: Greatest_subsequential_sum
2 in Euler Math Toolbox: Greatest_common_divisor,Greatest_element_of_a_list
2 in Mathprog: Greatest_subsequential_sum,Greatest_subsequential_sum
1 in PHP: Greatest_subsequential_sum
1 in Ring: Greatest_common_divisor
1 in gnuplot: Greatest_common_divisor
1 in ooRexx: Greatest_element_of_a_list
2 in uBasic/4tH: Greatest_common_divisor,Greatest_element_of_a_list
2 in MK-61/52: Greatest_element_of_a_list,Greatest_element_of_a_list

```



