+++
title = "Retrieve and search chat history"
description = ""
date = 2019-07-29T00:17:11Z
aliases = []
[extra]
id = 21365
[taxonomies]
categories = ["task", "Programming environment operations"]
tags = []
languages = [
  "c",
  "elixir",
  "go",
  "julia",
  "mathematica",
  "perl_6",
  "phix",
  "racket",
  "ruby",
  "scala",
  "smalltalk",
  "tcl",
  "zkl",
]
+++

## Task

'''Summary:''' Find and print the mentions of a given string in the recent chat logs from a chatroom. Only use your programming language's standard library.

'''Details:''' 

The Tcl Chatroom is an online chatroom. Its conversations are logged. It's useful to know if someone has mentioned you or your project in the chatroom recently. You can find this out by searching the chat logs. The logs are publicly available at http://tclers.tk/conferences/tcl/. One log file corresponds to the messages from one day in [[wp:Time in Germany|Germany's current time zone]]. Each chat log file has the name <tt>YYYY-MM-DD.tcl</tt> where <tt>YYYY</tt> is the year, <tt>MM</tt> is the month and <tt>DD</tt> the day. The logs store one message per line. The messages themselves are human-readable and their internal structure doesn't matter.

Retrieve the chat logs from the last 10 days via [[HTTP]]. Find the lines that include a particular [[substring]] and print them in the following format:


```txt
<log file URL>
------
<matching line 1>
<matching line 2>
...
<matching line N>
------


```


The substring will be given to your program as a command line argument.

You need to account for the possible time zone difference between the client running your program and the chat log writer on the server to not miss any mentions. (For example, if you generated the log file URLs naively based on the local date, you could miss mentions if it was already April 5th for the logger but only April 4th for the client.) What this means in practice is that you should either generate the URLs in the time zone <tt>Europe/Berlin</tt> or, if your language can not do that, add an extra day (today + 1) to the range of dates you check, but then make sure to not print parts of a "not found" page by accident if a log file doesn't exist yet.

The code should be contained in a single-file script, with no "project" or "dependency" file (e.g., no <tt>requirements.txt</tt> for Python). It should only use a given programming language's standard library to accomplish this task and not rely on the user having installed any third-party packages.

If your language does not have an HTTP client in the standard library, you can speak raw HTTP 1.0 to the server. If it can't parse command line arguments in a standalone script, read the string to look for from the standard input.


## C

Starts from current date, prints out lines containing matching substring and also if the string is not found at all in the log of that particular day and also if the log of a day cannot be read for any reason, requires [https://curl.haxx.se/libcurl/ libcurl]

```C

#include<curl/curl.h>
#include<string.h>
#include<stdio.h>

#define MAX_LEN 1000

void searchChatLogs(char* searchString){
	char* baseURL = "http://tclers.tk/conferences/tcl/";
	time_t t;
	struct tm* currentDate;
	char dateString[30],dateStringFile[30],lineData[MAX_LEN],targetURL[100];
	int i,flag;
	FILE *fp;
	
	CURL *curl;
	CURLcode res;
	
	time(&t);
	currentDate = localtime(&t);
	
	strftime(dateString, 30, "%Y-%m-%d", currentDate);
	printf("Today is : %s",dateString);
	
	if((curl = curl_easy_init())!=NULL){
		for(i=0;i<=10;i++){
			
		flag = 0;
		sprintf(targetURL,"%s%s.tcl",baseURL,dateString);
		
		strcpy(dateStringFile,dateString);
		
		printf("\nRetrieving chat logs from %s\n",targetURL);
		
		if((fp = fopen("nul","w"))==0){
			printf("Cant's read from %s",targetURL);
		}
		else{
			curl_easy_setopt(curl, CURLOPT_URL, targetURL);
		curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
		
		res = curl_easy_perform(curl);
		
		if(res == CURLE_OK){
			while(fgets(lineData,MAX_LEN,fp)!=NULL){
				if(strstr(lineData,searchString)!=NULL){
					flag = 1;
					fputs(lineData,stdout);
				}
			}
			
			if(flag==0)
				printf("\nNo matching lines found.");
		}
		fflush(fp);
		fclose(fp);
		}
		
		currentDate->tm_mday--;
		mktime(currentDate);
		strftime(dateString, 30, "%Y-%m-%d", currentDate);	
			
	}
	curl_easy_cleanup(curl);
	
	}
}

int main(int argC,char* argV[])
{
	if(argC!=2)
		printf("Usage : %s <followed by search string, enclosed by \" if it contains spaces>",argV[0]);
	else
		searchChatLogs(argV[1]);
	return 0;
}

```

Invocation and some output, actual output can be huge :

```txt

C:\rosettaCode>searchChatLogs.exe available
Today is : 2017-10-18
Retrieving chat logs from http://tclers.tk/conferences/tcl/2017-10-18.tcl
m 2017-10-18T08:21:17Z {} {AvL_42 has become available}
m 2017-10-18T08:50:17Z {} {de has become available}
m 2017-10-18T09:42:20Z {} {rmax_ has become available}
m 2017-10-18T10:23:54Z {} {dburns has become available}
m 2017-10-18T10:35:37Z {} {lyro has become available}
m 2017-10-18T10:38:48Z {} {rmax has become available}

```



## Elixir


```elixir
#! /usr/bin/env elixir
defmodule Mentions do
  def get(url) do
    {:ok, {{_, 200, _}, _, body}} =
      url
      |> String.to_charlist()
      |> :httpc.request()
    data = List.to_string(body)
    if Regex.match?(~r|<!Doctype HTML.*<Title>URL Not Found</Title>|s, data) do
      {:error, "log file not found"}
    else
      {:ok, data}
    end
  end

  def perg(haystack, needle) do
    haystack
    |> String.split("\n")
    |> Enum.filter(fn x -> String.contains?(x, needle) end)
  end

  def generate_url(n) do
    date_str =
      DateTime.utc_now()
      |> DateTime.to_unix()
      |> (fn x -> x + 60*60*24*n end).()
      |> DateTime.from_unix!()
      |> (fn %{year: y, month: m, day: d} ->
        :io_lib.format("~B-~2..0B-~2..0B", [y, m, d])
      end).()
    "http://tclers.tk/conferences/tcl/#{date_str}.tcl"
  end
end

[needle] = System.argv()
:application.start(:inets)
back = 10
# Elixir does not come standard with time zone definitions, so we add an extra
# day to account for the possible difference between the local and the server
# time.
for i <- -back..1 do
  url = Mentions.generate_url(i)
  with {:ok, haystack} <- Mentions.get(url),
       # If the result is a non-empty list...
       [h | t] <-  Mentions.perg(haystack, needle) do
    IO.puts("#{url}\n------\n#{Enum.join([h | t], "\n")}\n------\n")
  end
end
```


=={{header|F_Sharp|F#}}==

```fsharp
#!/usr/bin/env fsharpi
let server_tz =
    try
        // CLR on Windows
        System.TimeZoneInfo.FindSystemTimeZoneById("W. Europe Standard Time")
    with
        // Mono
        :? System.TimeZoneNotFoundException ->
            System.TimeZoneInfo.FindSystemTimeZoneById("Europe/Berlin")

let get url =        
    let req = System.Net.WebRequest.Create(System.Uri(url)) 
    use resp = req.GetResponse()
    use stream = resp.GetResponseStream() 
    use reader = new System.IO.StreamReader(stream) 
    reader.ReadToEnd()

let grep needle (haystack : string) =
    haystack.Split('\n')
    |> Array.toList
    |> List.filter (fun x -> x.Contains(needle))

let genUrl n =
    let day = System.DateTime.UtcNow.AddDays(float n)
    let server_dt = System.TimeZoneInfo.ConvertTimeFromUtc(day, server_tz)
    let timestamp = server_dt.ToString("yyyy-MM-dd")
    sprintf "http://tclers.tk/conferences/tcl/%s.tcl" timestamp

let _ =
    match fsi.CommandLineArgs with
    | [|_; needle|] ->
        let back = 10
        for i in -back .. 0 do
            let url = genUrl i
            let found = url |> get |> grep needle |> String.concat "\n"
            if found <> "" then printfn "%s\n------\n%s\n------\n" url found
            else ()
    | x ->
        printfn "Usage: %s literal" (Array.get x 0)
        System.Environment.Exit(1)
```



## Go


```go
package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"strings"
	"time"
)

func get(url string) (res string, err error) {
	resp, err := http.Get(url)
	if err != nil {
		return "", err
	}
	buf, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}
	return string(buf), nil
}

func grep(needle string, haystack string) (res []string) {
	for _, line := range strings.Split(haystack, "\n") {
		if strings.Contains(line, needle) {
			res = append(res, line)
		}
	}
	return res
}

func genUrl(i int, loc *time.Location) string {
	date := time.Now().In(loc).AddDate(0, 0, i)
	return date.Format("http://tclers.tk/conferences/tcl/2006-01-02.tcl")
}

func main() {
	needle := os.Args[1]
	back := -10
	serverLoc, err := time.LoadLocation("Europe/Berlin")
	if err != nil {
		log.Fatal(err)
	}
	for i := back; i <= 0; i++ {
		url := genUrl(i, serverLoc)
		contents, err := get(url)
		if err != nil {
			log.Fatal(err)
		}
		found := grep(needle, contents)
		if len(found) > 0 {
			fmt.Printf("%v\n------\n", url)
			for _, line := range found {
				fmt.Printf("%v\n", line)
			}
			fmt.Printf("------\n\n")
		}
	}
}
```



## Julia


```julia
using Dates, TimeZones, HTTP, Printf

function geturlbyday(n)
    d = DateTime(now(tz"Europe/Berlin")) + Day(n)  # n should be <= 0
    uri = @sprintf("http://tclers.tk/conferences/tcl/%04d-%02d-%02d.tcl",
        year(d), month(d), day(d))
    return uri, String(HTTP.request("GET", uri).body)
end

function searchlogs(text = ARGS[1], daysback = 9)
    for n in -daysback:0
        fname, searchtext = geturlbyday(n)
        println("$fname\n------")
        for line in split(searchtext, "\n")
            if findfirst(text, line) != nothing
                println(line)
            end
        end
        println("------\n")
    end
end

if length(ARGS) != 1
    println("Usage: type search phrase in quotes as an argument.")
else
    searchlogs()
end

```




## Mathematica


```Mathematica
matchFrom[url_String, str_String] := Select[StringSplit[Import[url, "String"], "\n"], StringMatchQ[str]]

getLogLinks[n_] := 
 Select[Import["http://tclers.tk/conferences/tcl/", "Hyperlinks"], 
     First[
     StringCases[#1, "tcl/" ~~ date__ ~~ ".tcl" :> DateDifference[DateObject[URLDecode[date], TimeZone -> "Europe/Berlin"], Now]]] <= 
         Quantity[n, "Days"] & ]

searchLogs[str_String] := Block[{data},
  Map[
   (data = matchFrom[#, str];
     If[Length[data] > 0, 
      Print /@ Join[{#, "-----"}, data, {"----\n"}]]) &, 
   getLogLinks[10]]]

searchLogs["*lazy*"];
```

```txt
http://tclers.tk/conferences/tcl/2017%2d09%2d25.tcl
-----
m 2017-09-25T14:38:02Z hypnotoad {I'm lazy and the old implementation was called "zvfs", and there a lot of kit builders who are looking for that command}

----

```



## Perl 6

No dependencies or third party libraries huh? How about no modules, no requires, no libraries, no imports at all. Implemented using a bare compiler, strictly with built-ins. (Which makes it kind-of verbose, but thems the trade-offs.)


```perl6
my $needle = @*ARGS.shift // '';
my @haystack;

# 10 days before today, Zulu time
my $begin = DateTime.new(time).utc.earlier(:10days);
say "         Executed at: ", DateTime.new(time).utc;
say "Begin searching from: $begin";

# Today - 10 days through today
for $begin.Date .. DateTime.now.utc.Date -> $date {

    # connect to server, use a raw socket
    my $http = IO::Socket::INET.new(:host('tclers.tk'), :port(80));

    # request file
    $http.print: "GET /conferences/tcl/{$date}.tcl HTTP/1.0\n\n";

    # retrieve file
    my @page = $http.lines;

    # remove header
    @page.splice(0, 8);

    # concatenate multi-line entries to a single line
    while @page {
        if @page[0].substr(0, 13) ~~ m/^'m '\d\d\d\d'-'\d\d'-'\d\d'T'/ {
            @haystack.push: @page.shift;
        }
        else {
            @haystack.tail ~= '␤' ~ @page.shift;
        }
    }

    # close socket
    $http.close;
}

# ignore times before 10 days ago
@haystack.shift while @haystack[0].substr(2, 22) lt $begin.Str;

# print the first and last line of the haystack
say "First and last lines of the haystack:";
.say for |@haystack[0, *-1];
say "Needle: ", $needle;
say  '-' x 79;

# find and print needle lines
.say if .contains( $needle ) for @haystack;
```

Sample output using a needle of 'github.com'

```txt
         Executed at: 2017-05-05T02:13:55Z
Begin searching from: 2017-04-25T02:13:55Z
First and last lines of the haystack:
m 2017-04-25T02:25:17Z ijchain {*** TakinOver leaves}
m 2017-05-05T02:14:52Z {} {rmax has become available}
Needle: github.com
-------------------------------------------------------------------------------
m 2017-04-28T07:33:59Z ijchain {<Napier> https://github.com/Dash-OS/tcl-modules/blob/master/react-0.5.tm}
m 2017-04-28T07:35:40Z ijchain {<Napier> https://github.com/Dash-OS/tcl-modules/blob/master/react/reducer-0.5.tm}
m 2017-04-28T08:25:39Z ijchain {<Napier> https://github.com/Dash-OS/tcl-modules/blob/master/examples/react.md}
m 2017-04-28T08:45:01Z ijchain {<Napier> https://github.com/Dash-OS/tcl-modules/blob/master/examples/react.md}
m 2017-04-28T09:21:22Z ijchain {<Napier> decorators damnit! :-P https://github.com/Dash-OS/tcl-modules/blob/master/decorator-1.0.tm}
m 2017-04-28T15:42:28Z jima https://gist.github.com/antirez/6ca04dd191bdb82aad9fb241013e88a8
m 2017-04-28T16:20:22Z ijchain {<dbohdan> Look at what I've just made: https://github.com/dbohdan/ptjd}
m 2017-04-29T05:48:13Z ijchain {<dbohdan> The brackets, rather than braces, are there because the [catch] is used for metaprogramming: https://github.com/dbohdan/ptjd/blob/c0a77ecfb34c619e30ec7c5e9f448879d41282e2/tests.tcl#L48}
m 2017-04-29T14:49:08Z ijchain {<dbohdan> If you want CI for Windows builds, it should possible to set it up relatively easily with AppVeyor and the Git mirror at https://github.com/tcltk/tcl}
m 2017-04-29T14:50:58Z ijchain {<dbohdan> Here's an example: https://github.com/dbohdan/picol/blob/trunk/appveyor.yml}
m 2017-04-29T20:33:25Z ijchain {<Napier> https://github.com/pfultz2/Cloak/wiki/C-Preprocessor-tricks,-tips,-and-idioms}
m 2017-04-30T08:20:05Z ijchain {<bairui> AvL_42: fwiw, I like and use Apprentice: https://github.com/romainl/Apprentice}
m 2017-04-30T21:35:22Z ijchain {<Napier> https://github.com/Dash-OS/tcl-modules#cswitch-flags-----expr-script-}
m 2017-05-01T04:16:00Z ijchain {<bairui> avl42: https://github.com/dahu/DiffLine  --  see if that helps with your next long-line vimdiff. DISCLAIMER: It *shouldn't* eat your hard-drive, but buyer beware, check the code and test it on unimportant stuff first.}
m 2017-05-01T07:08:26Z ijchain {<dbohdan> Do take a look at this one, though: https://github.com/dbohdan/sqawk/blob/master/lib/tabulate.tcl#L74}
m 2017-05-01T07:09:28Z ijchain {<dbohdan> And at https://github.com/dbohdan/jimhttp/blob/master/arguments.tcl}
m 2017-05-01T08:14:05Z ijchain {<dbohdan> I've reimplemented part of tcltest recently (with pretty colors!), and I'm pretty with this solution: https://github.com/dbohdan/ptjd/blob/master/tests.tcl#L49}
m 2017-05-03T14:57:24Z ijchain {<dbohdan> I think Tcl is okay to pretty good for compilery things if you already know how to do them. For instance, I found writing this tokenizer in Tcl a breeze: https://github.com/dbohdan/jimhttp/blob/master/json.tcl#L380}
m 2017-05-04T16:19:55Z stu {rkeene, files Makefile.in, itzev and spoto.conf are spotoconf: https://github.com/aryler/Tclarc4random/}
m 2017-05-04T20:26:28Z dgp https://github.com/flightaware/Tcl-bounties/issues/25
```



## Phix

```Phix
include builtins\libcurl.e
atom curl = NULL

function download(string url)
    if curl=NULL then
        curl_global_init()
        curl = curl_easy_init()
    end if
    curl_easy_setopt(curl, CURLOPT_URL, url)
    object res = curl_easy_perform_ex(curl)
    if integer(res) then
        printf(1,"libcurl error %d (%s)\n",{res,curl_easy_strerror(res)})
        return ""
    end if
    return res
end function

function grep(string needle, haystack)
    sequence lines = split(haystack,"\n"),
             res = {}
    for i=1 to length(lines) do
        if match(needle,lines[i]) then
            res = append(res,lines[i])
        end if
    end for
    if res={} then res = {"no occurences"} end if
    return res
end function
 
include builtins\timedate.e

function gen_url(integer i, string timezone)
    timedate td = set_timezone(date(),timezone)
    td = adjust_timedate(td,timedelta(days:=i))
    return format_timedate(td,"'http://tclers.tk/conferences/tcl/'YYYY-MM-DD'.tcl'")
end function
 
sequence cl = command_line()
string needle = "github"
integer days = 10
if length(cl)>=3 then
    needle := cl[3]
    if length(cl)>=4 then
        days := to_integer(cl[4])
        if days=0 or length(cl)>=5 then ?9/0 end if
    end if
end if
for i=-days to 0 do
    string url := gen_url(i, "CEST"),
           contents = download(url)
    if contents="" then exit end if
    ?url
    printf(1,"%s\n",join(grep(needle,contents),"\n"))
end for
```

(manually wrapped)

```txt

"http://tclers.tk/conferences/tcl/2019-07-19.tcl"
m 2019-07-19T09:56:50Z ijchain {<sebres> I thought it expects to migrate the project to github apps, is itn't?}
m 2019-07-19T13:21:18Z ijchain {<sebres> as for git-bash / travis issue - opened now as ticket 
                                (https://github.com/git-for-windows/git/issues/2267)}
"http://tclers.tk/conferences/tcl/2019-07-20.tcl"
no occurences
"http://tclers.tk/conferences/tcl/2019-07-21.tcl"
m 2019-07-21T18:15:16Z ijchain {<rkeene> I think this is the original basis for Nikit, not sure if it's still used: 
                                https://github.com/stevehav/nikit}
m 2019-07-21T19:23:51Z ijchain {<_abc_> Anyway, https://github.com/stevehav/nikit/blob/master/wiki/mkup.tcl uses 
                                regexp type parsing, not a full character/line parser}
m 2019-07-21T19:27:05Z jima https://github.com/tobijk/caius/blob/master/lib/markdown/markdown.tcl
"http://tclers.tk/conferences/tcl/2019-07-22.tcl"
m 2019-07-22T16:15:17Z ijchain {-½aku-+ Note for the githubbers - https://www.githubstatus.com/ - Incident in progress}
m 2019-07-22T16:27:54Z ijchain {<sebres> rkeene: ... a conclusion that Buchm++ller "isn't senior enough" was surely 
                                "great", also after (only) 1 hour of interview... the thing is: I know many devs able 
                                to build binary tree in 15 minutes, but they all ware not senior enough to me... but 
                                Buchm++ller?! (it is so simple - before you do an interview with someone, to take a 
                                    look at his code on github and co).}
m 2019-07-22T16:31:21Z ijchain {<sebres> well... my tcl-code is undiscoverable at github, because someone (;aku;) had 
                                a decision to follow drh's advice :)}
m 2019-07-22T17:14:46Z ijchain {<_abc_> https://github.com/wduquette/tcl-markdown/tree/master/docs/man1 this}
m 2019-07-22T17:15:14Z ijchain {<_abc_> this seems to be the right manpage, but wrong version, 1.1 vs 1.0 
                                https://github.com/wduquette/tcl-markdown/tree/master/docs/man1}
"http://tclers.tk/conferences/tcl/2019-07-23.tcl"
no occurences
"http://tclers.tk/conferences/tcl/2019-07-24.tcl"
m 2019-07-24T16:20:25Z ijchain {<chrstphrchvz> (Equivalent feature in GitHub: https://github.com/tcltk/tk/compare/bug-38dc27bd1d . 
                                The changes I want to look at haven't been mirrored yet, though.)}
"http://tclers.tk/conferences/tcl/2019-07-25.tcl"
no occurences
"http://tclers.tk/conferences/tcl/2019-07-26.tcl"
m 2019-07-26T10:01:56Z ijchain {<dbohdan> There is a nice collection of basic multilingual plane spinners in 
                                https://github.com/clj-commons/spinner/blob/master/src/spinner/core.clj}
"http://tclers.tk/conferences/tcl/2019-07-27.tcl"
no occurences
"http://tclers.tk/conferences/tcl/2019-07-28.tcl"
m 2019-07-27T23:03:59Z ijchain {-½aku-+ cmdr usages - lspace @ https://core.tcl-lang.org/akupries/lspace/doc/micro/doc/1st.intro.md, 
                                fx @ https://core.tcl-lang.org/akupries/fx/index, stackato-cli @ https://github.com/hpcloud/stackato-cli}
"http://tclers.tk/conferences/tcl/2019-07-29.tcl"
no occurences

```


=={{header|Python|Python 3}}==

```python
#! /usr/bin/env python3
import datetime
import re
import urllib.request
import sys

def get(url):
    with urllib.request.urlopen(url) as response:
       html = response.read().decode('utf-8')
    if re.match(r'<!Doctype HTML[\s\S]*<Title>URL Not Found</Title>', html):
        return None
    return html

def main():
    template = 'http://tclers.tk/conferences/tcl/%Y-%m-%d.tcl'
    today = datetime.datetime.utcnow()
    back = 10
    needle = sys.argv[1]
    # Since Python does not come standard with time zone definitions, add an
    # extra day to account for the possible difference between the local and the
    # server time.
    for i in range(-back, 2):
        day = today + datetime.timedelta(days=i)
        url = day.strftime(template)
        haystack = get(url)
        if haystack:
            mentions = [x for x in haystack.split('\n') if needle in x]
            if mentions:
                print('{}\n------\n{}\n------\n'
                          .format(url, '\n'.join(mentions)))

main()
```



## Racket

Retrieves logs from 9 days ago until today and only outputs results if there are matches for the day.  Setting the time zone to that of Germany works on Linux and may work on MacOS but has not been tested.  Setting the time zone on your machine to Europe/Berlin before running this code will take care of the issue no matter what operating system you're using.


```scheme
#lang racket
(require net/url)
(require racket/date)

;; generate archive url from specified number of days in the past
(define (generate-url days-ago)
  (putenv "TZ" "Europe/Berlin") ; this works for Linux
  (let* [(today (current-date))
         (past (seconds->date (- (date->seconds today) (* days-ago 60 60 24))))
         (date-str (string-append
                    (~r (date-year past) #:min-width 4 #:pad-string "0") ; 4 digit year
                    "-"
                    (~r (date-month past) #:min-width 2 #:pad-string "0") ; 2 digit month
                    "-"
                    (~r (date-day past) #:min-width 2 #:pad-string "0"))) ; 2 digit day
         (url (string-append "http://tclers.tk/conferences/tcl/" date-str ".tcl"))]
    url))

;; retrieve content of url as a list of strings
(define (get-content-of-url url-string)
  (let [(st (open-output-string))]
    (copy-port (get-pure-port (string->url url-string)
                              #:redirections 100)
               st)
    (string-split  (get-output-string st) "\n"))) ; divide on line breaks

;; from a list of strings, return a list of those containing the search string
(define (get-matches lines search-string)
  (define (internal-get-matches lines search-string results)
    (cond
      ((empty? lines) results)
      (else (internal-get-matches (cdr lines)
                                  search-string
                                  (if (string-contains? (car lines) search-string)
                                      (append results (list (car lines)))
                                      results)))))
  (internal-get-matches lines search-string (list)))

;; display last 10 days worth of archives that contain matches to the search string
(define (display-matches-for-last-10-days search-string)
  ;; get archives from 9 days ago until today
  (for/list ([i (range 9 -1 -1)])
    (let* ([url (generate-url i)]
           [matches (get-matches (get-content-of-url url) search-string)])
      (cond [(not (empty? matches))
             (begin
               (display url)(newline)
               (display "------\n")
               (for/list ([line matches]) (display line)(newline))
               (display "------\n"))]))))


;; use the first command line argument as the search string
;; display usage info if no search string is provided
(cond ((= 0 (vector-length (current-command-line-arguments))) (display "USAGE: chat_history <search term>\n"))
      (else (display-matches-for-last-10-days (vector-ref (current-command-line-arguments) 0))))
```

Sample output using a search term of 'github.com'

```txt
http://tclers.tk/conferences/tcl/2018-01-02.tcl
------
m 2018-01-02T14:22:41Z ijchain {<auriocus> Here is a small C++ "wrapper" which I wrote myself to simplify writing commands which create commands, i.e. "classes" https://github.com/auriocus/AsynCA/blob/master/generic/tclclass.h}
m 2018-01-02T16:02:31Z ijchain {<Napier> https://github.com/clemahieu/raiblocks}
m 2018-01-02T17:07:10Z ijchain {<mr_calvin> grrr … https://github.com/flightaware/TclProDebug}
m 2018-01-02T22:45:09Z kbk {brono, there has been and will be again. It was never maintained by the Tcl team, and fell by the wayside when ActiveState had a corporate change of direction. Follow https://github.com/flightaware/Tcl-bounties/issues/25}
------
http://tclers.tk/conferences/tcl/2018-01-03.tcl
------
m 2018-01-03T19:53:16Z ijchain {<rkeene> With git, not so much -- github.com does it, most other git hosters won't, and git-archive doesn't}
------
http://tclers.tk/conferences/tcl/2018-01-04.tcl
------
m 2018-01-04T01:53:48Z ijchain {<qih> @stevel My Readme.md, hopefully I'll have the Tcl program pushed tomorrow https://github.com/QeyeH/medtrack}
m 2018-01-04T04:39:44Z stevel {antirez' linenoise might be useful too - and BSD licensed - https://github.com/antirez/linenoise}
m 2018-01-04T10:05:50Z ijchain {<Setok> Not sure if tdom is still actively supported, as there doesn't seem to have been a release for years, but just in case, I identified one issue: https://github.com/tDOM/tdom/issues/26}
m 2018-01-04T11:05:24Z de {Setok: The not updated tdom mirror on github is marked as that.   https://github.com/tDOM/tdom, almost at the top, directly under the tabs: "This is a currently not updated mirror of the tDOM sources. See http://core.tcl.tk/tdom/timeline"}
------
```



## Ruby


```ruby
#! /usr/bin/env ruby
require 'net/http'
require 'time'

def gen_url(i)
  day = Time.now + i*60*60*24
  # Set the time zone in which to format the time, per
  # https://coderwall.com/p/c7l82a/create-a-time-in-a-specific-timezone-in-ruby
  old_tz = ENV['TZ']
  ENV['TZ'] = 'Europe/Berlin'
  url = day.strftime('http://tclers.tk/conferences/tcl/%Y-%m-%d.tcl')
  ENV['TZ'] = old_tz
  url
end

def main
  back = 10
  needle = ARGV[0]
  (-back..0).each do |i|
    url = gen_url(i)
    haystack = Net::HTTP.get(URI(url)).split("\n")
    mentions = haystack.select { |x| x.include? needle }
    if !mentions.empty?
      puts "#{url}\n------\n#{mentions.join("\n")}\n------\n"
    end
  end
end

main
```



## Scala


```scala
import java.net.Socket
import java.net.URL
import java.time
import java.time.format
import java.time.ZoneId
import java.util.Scanner
import scala.collection.JavaConverters._

def get(rawUrl: String): List[String] = {
    val url = new URL(rawUrl)
    val port = if (url.getPort > -1) url.getPort else 80
    val sock = new Socket(url.getHost, port)
    sock.getOutputStream.write(
        s"GET /${url.getPath()} HTTP/1.0\n\n".getBytes("UTF-8")
    )
    new Scanner(sock.getInputStream).useDelimiter("\n").asScala.toList
}

def genUrl(n: Long) = {
    val date = java.time.ZonedDateTime
        .now(ZoneId.of("Europe/Berlin"))
        .plusDays(n)
        .format(java.time.format.DateTimeFormatter.ISO_LOCAL_DATE)
    s"http://tclers.tk/conferences/tcl/$date.tcl"
}

val back = 10
val literal = args(0) 
for (i <- -back to 0) {
    val url = genUrl(i)
    print(get(url).filter(_.contains(literal)) match {
        case List() => ""
        case x => s"$url\n------\n${x.mkString("\n")}\n------\n\n"
    })
}
```



## Smalltalk

The "!" syntax is not part of Smalltalk -- that's the " File Out" format.
Save as a " .st"  file, load into a Pharo 7 image (Tools -> File Browser -> Select the file -> FileIn), navigate using the System Browser to the RosettaCode package to read it more comfortably, save the image, and run on the OS shell with "pharo -headless [image] searchHistory term".


```smalltalk

CommandLineHandler subclass: #ChatHistorySearchCommandLineHandler
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RosettaCode'!

!ChatHistorySearchCommandLineHandler methodsFor: 'activation' stamp: 'EduardoPadoan 1/27/2019 15:18'!
activate
	self searchHistoryFor: self arguments first.
	self quit! !


!ChatHistorySearchCommandLineHandler methodsFor: 'commands' stamp: 'EduardoPadoan 1/27/2019 17:01'!
searchHistoryFor: aString
	| today startDate |
	
	"XXX Doesn't account for DST"
	today := DateAndTime now offset: 1 hours. 
	startDate := today - 10 days.

	startDate to: today by: 1 days do: [ :targetDate |
		| url response |

		url := String streamContents: [ :aStream |
			aStream nextPutAll: 'http://tclers.tk/conferences/tcl/'.
			targetDate printYMDOn: aStream.
			aStream nextPutAll: '.tcl'.
		].

		response := ZnEasy get: url.

		response contents asString linesDo: [ :line |
			(line asLowercase includesSubstring: aString asLowercase) ifTrue: [ 
				self stdout print: line; lf.
			]
		]
	]! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

ChatHistorySearchCommandLineHandler class
	instanceVariableNames: ''!

!ChatHistorySearchCommandLineHandler class methodsFor: 'accessing' stamp: 'EduardoPadoan 1/27/2019 14:53'!
description
	^ 'Look for a pattern in the TCL chat history'! !

!ChatHistorySearchCommandLineHandler class methodsFor: 'accessing' stamp: 'EduardoPadoan 1/27/2019 14:50'!
commandName
	^ 'searchHistory'! !


```


## Tcl



### Tcl 8.5+


```tcl
#! /usr/bin/env tclsh
package require http

proc get url {
    set r [::http::geturl $url]
    set content [::http::data $r]
    ::http::cleanup $r
    return $content
}

proc grep {needle haystack} {
    lsearch -all \
            -inline \
            -glob \
            [split $haystack \n] \
            *[string map {* \\* ? \\? \\ \\\\ [ \\[ ] \\]} $needle]*
}

proc main argv {
    lassign $argv needle
    set urlTemplate http://tclers.tk/conferences/tcl/%Y-%m-%d.tcl
    set back 10
    set now [clock seconds]
    for {set i -$back} {$i <= 0} {incr i} {
        set date [clock add $now $i days]
        set url [clock format $date \
                              -format $urlTemplate \
                              -timezone :Europe/Berlin]
        set found [grep $needle [get $url]]
        if {$found ne {}} {
            puts $url\n------\n[join $found \n]\n------\n
        }
    }
}

main $argv
```



### Jim Tcl


```tcl
#! /usr/bin/env jimsh
proc get url {
    if {![regexp {http://([a-z.]+)(:[0-9]+)?(/.*)} $url _ host port path]} {
        error "can't parse URL \"$url\""
    }
    if {$port eq {}} { set port 80 }
    set ch [socket stream $host:$port]
    puts -nonewline $ch "GET /$path HTTP/1.0\n\n"
    set content [read $ch]
    if {[regexp {^HTTP[^<]+<!Doctype HTML.*<Title>URL Not Found</Title>} \
                $content]} {
        error {log file not found}
    }
    close $ch
    return $content
}

proc grep {needle haystack} {
    lsearch -all \
            -inline \
            -glob \
            [split $haystack \n] \
            *[string map {* \\* ? \\? \\ \\\\ [ \\[ ] \\]} $needle]*
}

proc main argv {
    lassign $argv needle
    set urlTemplate http://tclers.tk/conferences/tcl/%Y-%m-%d.tcl
    set back 10
    set now [clock seconds]
    # Jim Tcl doesn't support time zones, so we add an extra day to account for
    # the possible difference between the local and the server time.
    for {set i -$back} {$i <= 1} {incr i} {
        set date [expr {$now + $i*60*60*24}]
        set url [clock format $date -format $urlTemplate]
        catch {
            set found [grep $needle [get $url]]
            if {$found ne {}} {
                puts $url\n------\n[join $found \n]\n------\n
            }
        }
    }
}

main $argv
```



## zkl


```zkl
#<<<#
http://tclers.tk/conferences/tcl/:
2017-04-03.tcl      30610 bytes  Apr 03, 2017 21:55:37
2017-04-04.tcl      67996 bytes  Apr 04, 2017 21:57:01
...

Contents (eg 2017-01-19.tcl):
m 2017-01-19T23:01:02Z ijchain {*** Johannes13__ leaves}
m 2017-01-19T23:15:37Z ijchain {*** fahadash leaves}
m 2017-01-19T23:27:00Z ijchain {*** Buster leaves}
...
#<<<#

var [const] CURL=Import.lib("zklCurl")();	// libCurl instance

template:="http://tclers.tk/conferences/tcl/%4d-%02d-%02d.tcl";
ymd     :=Time.Clock.UTC[0,3];	// now, (y,m,d)
back    :=10;			// days in the past
needle  :=vm.nthArg(0);		// search string
foreach d in ([-back+1..0]){	// we want day -9,-8,-7..0 (today)
   date :=Time.Date.subYMD(ymd, 0,0,-d);   // date minus days
   url  :=template.fmt(date.xplode());
   haystack:=CURL.get(url);	// (request bytes, header length)
   haystack=haystack[0].del(0,haystack[1]);	// remove HTML header
   mentions:=haystack.filter("find",needle);	// search lines
   if(mentions) println("%s\n------\n%s------\n".fmt(url,mentions.text));
}
```

While zkl supports TCP natively and talking simple HTTP is easy, 
Curl is way easier and fully supports the protocol.
```txt

$ zkl bbb suchenwi
http://tclers.tk/conferences/tcl/2017-04-24.tcl
------
m 2017-04-24T05:33:53Z {} {suchenwi has become available}
m 2017-04-24T06:38:31Z suchenwi {Hi Arjen - and bye. off to donuts}
m 2017-04-24T06:55:57Z {} {suchenwi has left}
...
------

http://tclers.tk/conferences/tcl/2017-04-30.tcl
...
------
http://tclers.tk/conferences/tcl/2017-05-01.tcl

------
...
http://tclers.tk/conferences/tcl/2017-05-03.tcl
------
m 2017-05-03T16:19:54Z {} {suchenwi has become available}
m 2017-05-03T16:20:40Z suchenwi {/me waves}
m 2017-05-03T16:21:57Z suchenwi {I'm on countdown at work: 17 work days to go...
...

```

