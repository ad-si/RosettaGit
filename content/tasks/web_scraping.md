+++
title = "Web scraping"
description = ""
date = 2019-10-12T12:31:25Z
aliases = []
[extra]
id = 2994
[taxonomies]
categories = ["Networking and Web Interaction", "task"]
tags = []
languages = [
  "8th",
  "ada",
  "auto_hotkey",
  "awk",
  "algol_68",
  "app_inventor",
  "arturo",
  "bbc_basic",
  "c",
  "c_plus_plus",
  "c_sharp",
  "cache_objectscript",
  "ceylon",
  "clojure",
  "coffee_script",
  "common_lisp",
  "d",
  "delphi",
  "e",
  "erlang",
  "f_sharp",
  "factor",
  "forth",
  "funl",
  "gambas",
  "go",
  "groovy",
  "haskell",
  "icon",
  "unicon",
  "j",
  "java",
  "java_script",
  "jq",
  "julia",
  "kotlin",
  "lasso",
  "liberty_basic",
  "lua",
  "m2000_interpreter",
  "maple",
  "mathematica",
  "matlab",
  "octave",
  "mirc_scripting_language",
  "microsoft_small_basic",
  "netrexx",
  "nim",
  "objeck",
  "ocaml",
  "oo_rexx",
  "oz",
  "peloton",
  "perl",
  "perl_6",
  "phix",
  "php",
  "pico_lisp",
  "power_shell",
  "pure_basic",
  "python",
  "r",
  "racket",
  "rebol",
  "run_basic",
  "ruby",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "tcl",
  "tuscript",
  "toffee_script",
  "txr",
  "unix_shell",
  "ursala",
  "vba",
  "vbscript",
  "visual_basic_dot_net",
  "xidel",
  "zkl"
]
+++

## Task

## Task
Create a program that downloads the time from this URL:   [http://tycho.usno.navy.mil/cgi-bin/timer.pl http://tycho.usno.navy.mil/cgi-bin/timer.pl]   and then prints the current UTC time by extracting just the UTC time from the web page's [[HTML]].

<!-- As of March 2014, the page is available
{{task|Networking and Web Interaction}}

The page http://tycho.usno.navy.mil/cgi-bin/timer.pl is no longer available since July 2011.
The relevant part of that page source looked like this:

```txt

...

<TITLE>What time is it?</TITLE>
<H2> US Naval Observatory Master Clock Time</H2> <H3><PRE>
<BR>Jul. 27, 22:57:22 UTC   Universal Time
<BR>Jul. 27, 06:57:22 PM EDT  Eastern Time
<BR>Jul. 27, 05:57:22 PM CDT  Central Time
<BR>Jul. 27, 04:57:22 PM MDT  Mountain Time
<BR>Jul. 27, 03:57:22 PM PDT  Pacific Time
<BR>Jul. 27, 02:57:22 PM AKDT Alaska Time
<BR>Jul. 27, 12:57:22 PM HAST Hawaii-Aleutian Time

...

```

End of comment -->

If possible, only use libraries that come at no ''extra'' monetary cost with the programming language and that are widely available and popular such as [http://www.cpan.org/ CPAN] for Perl or [[Boost]] for C++.





## 8th


```forth
\ Web-scrape sample: get UTC time from the US Naval Observatory:
: read-url \ -- s
 "http://tycho.usno.navy.mil/cgi-bin/timer.pl" net:get
 not if "Could not connect" throw then
 >s ;

: get-time
  read-url
  /<BR>.*?(\d{2}:\d{2}:\d{2})\sUTC/
  tuck r:match if
    1 r:@ . cr
  then ;

get-time bye

```

Output

```txt
14:08:20
```



## Ada

{{libheader|AWS}}

```Ada
with AWS.Client, AWS.Response, AWS.Resources, AWS.Messages;
with Ada.Text_IO, Ada.Strings.Fixed;
use  Ada, AWS, AWS.Resources, AWS.Messages;

procedure Get_UTC_Time is

   Page           : Response.Data;
   File           : Resources.File_Type;
   Buffer         : String (1 .. 1024);
   Position, Last : Natural := 0;
   S              : Messages.Status_Code;
begin
   Page := Client.Get ("http://tycho.usno.navy.mil/cgi-bin/timer.pl");
   S    := Response.Status_Code (Page);
   if S not  in Success then
      Text_IO.Put_Line
        ("Unable to retrieve data => Status Code :" & Image (S) &
         " Reason :" & Reason_Phrase (S));
      return;
   end if;

   Response.Message_Body (Page, File);
   while not End_Of_File (File) loop
      Resources.Get_Line (File, Buffer, Last);
      Position :=
         Strings.Fixed.Index
           (Source  => Buffer (Buffer'First .. Last),
            Pattern => "UTC");
      if Position > 0 then
         Text_IO.Put_Line (Buffer (5 .. Position + 2));
         return;
      end if;
   end loop;
end Get_UTC_Time;
```



## AutoHotkey


```AutoHotkey
UrlDownloadToFile, http://tycho.usno.navy.mil/cgi-bin/timer.pl, time.html
FileRead, timefile, time.html
pos := InStr(timefile, "UTC")
msgbox % time := SubStr(timefile, pos - 9, 8)
```



## AWK


This is inspired by [http://www.gnu.org/software/gawk/manual/gawkinet/html_node/GETURL.html#GETURL GETURL] example in the manual for gawk.

<tt>
```awk
#! /usr/bin/awk -f

BEGIN {
  purl = "/inet/tcp/0/tycho.usno.navy.mil/80"
  ORS = RS = "\r\n\r\n"
  print "GET /cgi-bin/timer.pl HTTP/1.0" |& purl
  purl |& getline header
  while ( (purl |& getline ) > 0 )
  {
     split($0, a, "\n")
     for(i=1; i <= length(a); i++)
     {
        if ( a[i] ~ /UTC/ )
        {
          sub(/^<BR>/, "", a[i])
          printf "%s\n", a[i]
        }
     }
  }
  close(purl)
}
```
</tt>


## ALGOL 68

{{works with|ALGOL 68|Revision 1 - however ''grep in string'', ''http content'' and ''str error'' are from a non-standard library}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of ''grep in string'' and ''http content''}}

```algol68
STRING
   domain="tycho.usno.navy.mil",
   page="cgi-bin/timer.pl";

STRING # search for the needle in the haystack #
   needle = "UTC",
   hay stack = "http://"+domain+"/"+page,

   re success="^HTTP/[0-9.]* 200",
   re result description="^HTTP/[0-9.]* [0-9]+ [a-zA-Z ]*",
   re doctype ="\s\s<![Dd][Oo][Cc][Tt][Yy][Pp][Ee] [^>]+>\s+";

PROC raise error = (STRING msg)VOID: ( put(stand error, (msg, new line)); stop);

PROC is html page = (REF STRING page) BOOL: (
     BOOL out=grep in string(re success, page, NIL, NIL) = 0;
     IF INT start, end;
        grep in string(re result description, page, start, end) = 0
     THEN
        page:=page[end+1:];
        IF grep in string(re doctype, page, start, end) = 0
        THEN page:=page[start+2:]
        ELSE raise error("unknown format retrieving page")
        FI
     ELSE raise error("unknown error retrieving page")
     FI;
     out
);

STRING reply;
INT rc = http content (reply, domain, haystack, 0);
IF rc = 0 AND is html page (reply)
THEN
  STRING line; FILE freply; associate(freply, reply);
  on logical file end(freply, (REF FILE freply)BOOL: (done; SKIP));
  DO
    get(freply,(line, new line));
    IF string in string(needle, NIL, line) THEN print((line, new line)) FI
  OD;
  done: SKIP
ELSE raise error (strerror (rc))
FI
```

{{out}} Sample

```txt

<BR>Sep. 26, 21:51:17 UTC               Universal Time

```



## App Inventor

App Inventor has a Web component that contains code blocks which simplify Web scraping.

It also has powerful text and list processing language blocks that simplify text scraping.

This is how the code would look if it could be typed:


```dos

when ScrapeButton.Click do
  set ScrapeWeb.Url to SourceTextBox.Text
  call ScrapeWeb.Get

when ScrapeWeb.GotText url,responseCode,responseType,responseContent do
  initialize local Left to split at first text (text: get responseContent, at: PreTextBox.Text)
  initialize local Right to "" in
    set Right to select list item (list: get Left, index: 2)
    set ResultLabel.Text to select list item (list: split at first (text:get Right, at: PostTextBox.Text), index: 1)

```


[https://lh5.googleusercontent.com/-lFRRPzsi5N0/UvBM9E_gZMI/AAAAAAAAKBQ/AuqDPGlwXNg/s1600/composite.png A picture of the graphical program]/


## Arturo



```arturo
use ~net

source $(download "http://tycho.usno.navy.mil/cgi-bin/timer.pl")
result $(matches source "/(\d{2}:\d{2}:\d{2}) UTC/")

utcTime result.0.1

print utcTime
```


{{out}


```txt
12:30:15
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
Note that the URL cache is cleared so the code works correctly if run more than once.

```bbcbasic
      SYS "LoadLibrary", "URLMON.DLL" TO urlmon%
      SYS "GetProcAddress", urlmon%, "URLDownloadToFileA" TO UDTF%
      SYS "LoadLibrary", "WININET.DLL" TO wininet%
      SYS "GetProcAddress", wininet%, "DeleteUrlCacheEntryA" TO DUCE%

      url$ = "http://tycho.usno.navy.mil/cgi-bin/timer.pl"
      file$ = @tmp$+"navytime.txt"

      SYS DUCE%, url$
      SYS UDTF%, 0, url$, file$, 0, 0 TO result%
      IF result% ERROR 100, "Download failed"

      file% = OPENIN(file$)
      REPEAT
        text$ = GET$#file%
        IF INSTR(text$, "UTC") PRINT MID$(text$, 5)
      UNTIL EOF#file%
      CLOSE #file%
```



## C

{{works with|POSIX|.1-2001}}

{{libheader|libcurl}}

There is no proper error handling.


```c
#include <stdio.h>
#include <string.h>
#include <curl/curl.h>
#include <sys/types.h>
#include <regex.h>

#define BUFSIZE 16384

size_t lr = 0;

size_t filterit(void *ptr, size_t size, size_t nmemb, void *stream)
{
  if ( (lr + size*nmemb) > BUFSIZE ) return BUFSIZE;
  memcpy(stream+lr, ptr, size*nmemb);
  lr += size*nmemb;
  return size*nmemb;
}

int main()
{
  CURL *curlHandle;
  char buffer[BUFSIZE];
  regmatch_t amatch;
  regex_t cregex;

  curlHandle = curl_easy_init();
  curl_easy_setopt(curlHandle, CURLOPT_URL, "http://tycho.usno.navy.mil/cgi-bin/timer.pl");
  curl_easy_setopt(curlHandle, CURLOPT_FOLLOWLOCATION, 1);
  curl_easy_setopt(curlHandle, CURLOPT_WRITEFUNCTION, filterit);
  curl_easy_setopt(curlHandle, CURLOPT_WRITEDATA, buffer);
  int success = curl_easy_perform(curlHandle);
  curl_easy_cleanup(curlHandle);

  buffer[lr] = 0;

  regcomp(&cregex, " UTC", REG_NEWLINE);
  regexec(&cregex, buffer, 1, &amatch, 0);
  int bi = amatch.rm_so;
  while ( bi-- > 0 )
    if ( memcmp(&buffer[bi], "<BR>", 4) == 0 ) break;

  buffer[amatch.rm_eo] = 0;

  printf("%s\n", &buffer[bi+4]);

  regfree(&cregex);
  return 0;
}
```



## C++

{{libheader|boost}}
{{works with|Visual Studio| 2010 Express Edition with boost-1.46.1 from boostpro.com}}
{{works with|gcc|4.5.2 with boost-1.46.1, compiled with -lboost_regex -lboost_system -lboost_thread}}

```cpp
#include <iostream>
#include <string>
#include <boost/asio.hpp>
#include <boost/regex.hpp>
int main()
{
    boost::asio::ip::tcp::iostream s("tycho.usno.navy.mil", "http");
    if(!s)
        std::cout << "Could not connect to tycho.usno.navy.mil\n";
    s  << "GET /cgi-bin/timer.pl HTTP/1.0\r\n"
       << "Host: tycho.usno.navy.mil\r\n"
       << "Accept: */*\r\n"
       << "Connection: close\r\n\r\n" ;
    for(std::string line; getline(s, line); )
    {
        boost::smatch matches;
        if(regex_search(line, matches, boost::regex("<BR>(.+\\s+UTC)") ) )
        {
            std::cout << matches[1] << '\n';
            break;
        }
    }
}
```


## C#

```c#
class Program
    {
        static void Main(string[] args)
        {
            WebClient wc = new WebClient();
            Stream myStream = wc.OpenRead("http://tycho.usno.navy.mil/cgi-bin/timer.pl");
            string html = "";
            using (StreamReader sr = new StreamReader(myStream))
            {
                while (sr.Peek() >= 0)
                {
                    html = sr.ReadLine();
                    if (html.Contains("UTC"))
                    {
                        break;
                    }
                }

            }
            Console.WriteLine(html.Remove(0, 4));

            Console.ReadLine();
        }
    }

```


=={{header|Caché ObjectScript}}==


```cos

Class Utils.Net [ Abstract ]
{

ClassMethod ExtractHTMLData(pHost As %String = "", pPath As %String = "", pRegEx As %String = "", Output list As %List) As %Status
{
  // implement error handling
  Try {

    // some initialisation
    Set list="", sc=$$$OK

    // check input parameters
    If $Match(pHost, "^([a-zA-Z0-9]([a-zA-Z0-9\-]{0,61}[a-zA-Z0-9])?\.)+[a-zA-Z]{2,6}$")=0 {
      Set sc=$$$ERROR($$$GeneralError, "Invalid host name.")
      Quit
    }

    // create http request and get page
    Set req=##class(%Net.HttpRequest).%New()
    Set req.Server=pHost
    Do req.Get(pPath)

    // check for success
    If $Extract(req.HttpResponse.StatusCode)'=2 {
      Set sc=$$$ERROR($$$GeneralError, "Page not loaded.")
      Quit
    }

    // read http response stream
    Set html=req.HttpResponse.Data
    Set html.LineTerminator=$Char(10)
    Set sc=html.Rewind()

    // read http response stream
    While 'html.AtEnd {
      Set line=html.ReadLine(, .sc, .eol)
      Set pos=$Locate(line, pRegEx)
      If pos {
        Set parse=$Piece($Extract(line, pos, *), $Char(9))
        Set slot=$ListLength(list)+1
        Set $List(list, slot)=parse
      }
    }

  } Catch err {

    // an error has occurred
    If err.Name="<REGULAR EXPRESSION>" {
      Set sc=$$$ERROR($$$GeneralError, "Invalid regular expression.")
    } Else {
      Set sc=$$$ERROR($$$CacheError, $ZError)
    }

  }

  // return status
  Quit sc
}

}

```

{{out|Examples}}

```txt

USER>Do ##class(Utils.Net).ExtractHTMLData("tycho.usno.navy.mil", "/cgi-bin/timer.pl", "[A-Za-z\.]{3,5} \d{1,2}, \d{1,2}:\d{2}:\d{2} UTC", .list)
USER>Write $List(list)
Mar. 29, 20:45:27 UTC

USER>Do ##class(Utils.Net).ExtractHTMLData("tycho.usno.navy.mil", "/cgi-bin/timer.pl", "[A-Za-z\.]{3,5} \d{1,2}, \d{1,2}:\d{2}:\d{2}", .list)
USER>Write $ListToString(list, $Char(13,10))
Mar. 29, 20:47:42 UTC
Mar. 29, 04:47:42 PM EDT
Mar. 29, 03:47:42 PM CDT
Mar. 29, 02:47:42 PM MDT
Mar. 29, 01:47:42 PM PDT
Mar. 29, 12:47:42 PM AKDT
Mar. 29, 10:47:42 AM HAST

```



## Ceylon

Don't forget to import ceylon.uri and ceylon.http.client in your module.ceylon file.

```ceylon
import ceylon.uri {
    parse
}
import ceylon.http.client {
    get
}

shared void run() {

    // apparently the cgi link is deprecated?
    value oldUri = "http://tycho.usno.navy.mil/cgi-bin/timer.pl";
    value newUri = "http://tycho.usno.navy.mil/timer.pl";

    value contents = downloadContents(newUri);
    value time = extractTime(contents);
    print(time else "nothing found");
}

String downloadContents(String uriString) {
    value uri = parse(uriString);
    value request = get(uri);
    value response = request.execute();
    return response.contents;
}

String? extractTime(String contents) =>
        contents
        .lines
        .filter((String element) => element.contains("UTC"))
        .first
        ?.substring(4, 21);
```



## Clojure

Clojure 1.2:


```clojure

(second (re-find #" (\d{1,2}:\d{1,2}:\d{1,2}) UTC" (slurp "http://tycho.usno.navy.mil/cgi-bin/timer.pl")))

```



## CoffeeScript

{{works with|node.js}}


```coffeescript

http = require 'http'

CONFIG =
  host: 'tycho.usno.navy.mil'
  path: '/cgi-bin/timer.pl'

# Web scraping code tends to be brittle, and this is no exception.
# The tycho time page does not use highly structured markup, so
# we do a real dirty scrape.
scrape_tycho_ust_time = (text) ->
  for line in text.split '\n'
    matches = line.match /(.*:\d\d UTC)/
    if matches
      console.log matches[0].replace '<BR>', ''
      return
  throw Error("unscrapable page!")

# This is low-level-ish code to get data from a URL. It's
# pretty general purpose, so you'd normally tuck this away
# in a library (or use somebody else's library).
wget = (host, path, cb) ->
  options =
    host: host
    path: path
    headers:
      "Cache-Control": "max-age=0"

  req = http.request options, (res) ->
    s = ''
    res.on 'data', (chunk) ->
      s += chunk
    res.on 'end', ->
      cb s
  req.end()

# Do our web scrape
do ->
  wget CONFIG.host, CONFIG.path, (data) ->
    scrape_tycho_ust_time data

```

{{out}}

```txt

> coffee web_scrape.coffee
Jan. 09, 19:19:07 UTC

```



## Common Lisp


{{libheader|cl-ppcre}}
{{libheader|DRAKMA}}


```lisp>BOA
 (let* ((url "http://tycho.usno.navy.mil/cgi-bin/timer.pl")
            (regexp (load-time-value
                     (cl-ppcre:create-scanner "(?m)^.{4}(.+? UTC)")))
            (data (drakma:http-request url)))
       (multiple-value-bind (start end start-regs end-regs)
           (cl-ppcre:scan regexp data)
         (declare (ignore end))
         (when start
           (subseq data (aref start-regs 0) (aref end-regs 0)))))
"Aug. 12, 04:29:51 UTC"
```


Another Common Lisp solution

```lisp
CL-USER> (cl-ppcre:do-matches-as-strings
             (m ".*<BR>(.*)UTC.*"
                (drakma:http-request "http://tycho.usno.navy.mil/cgi-bin/timer.pl"))
           (print (cl-ppcre:regex-replace "<BR>(.*UTC).*" m "\\1")))
"Jul. 13, 06:32:01 UTC"
```



## D


```d
void main() {
    import std.stdio, std.string, std.net.curl, std.algorithm;

    foreach (line; "http://tycho.usno.navy.mil/cgi-bin/timer.pl".byLine)
        if (line.canFind(" UTC"))
            line[4 .. $].writeln;
}
```



## Delphi


{{works with|Delphi 7}}

There are a number of ways to do this with Delphi using any one of a number of free/open source TCP/IP component suites such as, for example, ICS, Synapse and Indy (which ships with Delphi anyway).  However, I thought it would be interesting to do this using the Winsock API direct.


```Delphi


program WebScrape;

{$APPTYPE CONSOLE}

{.$DEFINE DEBUG}

uses
  Classes,
  Winsock;


{ Function to connect to host, send HTTP request and retrieve response }
function DoHTTPGET(const hostName: PAnsiChar; const resource: PAnsiChar; HTTPResponse: TStrings): Boolean;
const
  Port: integer = 80;
  CRLF = #13#10; // carriage return/line feed
var
  WSAData: TWSAData;
  Sock: TSocket;
  SockAddrIn: TSockAddrIn;
  IPAddress: PHostEnt;
  bytesIn: integer;
  inBuffer: array [0..1023] of char;
  Req: string;
begin
  Result := False;
  HTTPResponse.Clear;

  { Initialise use of the Windows Sockets DLL.
    Older Windows versions support Winsock 1.1 whilst newer Windows
    include Winsock 2 but support 1.1.  Therefore, we'll specify
    version 1.1 ($101) as being the highest version of Windows Sockets
    that we can use to provide greatest flexibility.
    WSAData receives details of the Windows Sockets implementation }
  Winsock.WSAStartUp($101, WSAData);
  try

    { Create a socket for TCP/IP usage passing in
      Address family spec: AF_INET (TCP, UDP, etc.)
      Type specification: SOCK_STREAM
      Protocol: IPPROTO_TCP (TCP) }
    Sock := WinSock.Socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    try

      // Check we have a valid socket
      if (Sock <> INVALID_SOCKET) then
        begin
          // Populate socket address structure
          with SockAddrIn do
            begin
              // Address family specification
              sin_family := AF_INET;
              // Port
              sin_port := htons(Port);
              // Address
              sin_addr.s_addr := inet_addr(hostName);
            end;

          if (SockAddrIn.sin_addr.s_addr = INADDR_NONE) then
            begin
              { As we're using a domain name instead of an
                IP Address, we need to resolve the domain name }
              IPAddress := Winsock.gethostbyname(hostName);

              // Quit if we didn't get an IP Address
              if (IPAddress = nil) then
                Exit;

              // Update the structure with the IP Address
              SockAddrIn.sin_addr.s_addr := PLongint(IPAddress^.h_addr_list^)^;
            end;

          // Try to connect to host
          if (Winsock.connect(Sock, SockAddrIn, SizeOf(SockAddrIn)) <> SOCKET_ERROR) then
            begin
              // OK - Connected

              // Compose our request
              // Each line of the request must be terminated with a carriage return/line feed

              {  The First line specifies method (e.g. GET, POST), path to required resource,
                 and the HTTP version being used.  These three fields are space separated. }
               Req := 'GET '+resource+' HTTP/1.1' + CRLF +

                     // Host: is the only Required header in HTTP 1.1
                     'Host: '+hostName + CRLF +

                     { Persistent connections are the default in HTTP 1.1 but, as we don't want
                       or need one for this exercise, we must include the "Connection: close"
                       header in our request }
                     'Connection: close' + CRLF +

                     CRLF; // Request must end with an empty line!

              // Try to send the request to the host
              if (Winsock.send(Sock,Req[1],Length(Req),0) <> SOCKET_ERROR) then
                begin
                  // Initialise incoming data buffer (i.e. fill array with nulls)
                  FillChar(inBuffer,SizeOf(inBuffer),#0);
                  // Loop until nothing left to read
                  repeat
                    // Read incoming data from socket
                    bytesIn := Winsock.recv(Sock, inBuffer, SizeOf(inBuffer), 0);
                    // Assign buffer to Stringlist
                    HTTPResponse.Text := HTTPResponse.Text + Copy(string(inBuffer),1,bytesIn);
                  until
                    (bytesIn <= 0) or (bytesIn = SOCKET_ERROR);

                  { Our list of response strings should
                    contain at least 1 line }
                  Result := HTTPResponse.Count > 0;
                end;

             end;

        end;

    finally
      // Close our socket
      Winsock.closesocket(Sock);
    end;

  finally
    { This causes our application to deregister itself from this
      Windows Sockets implementation and allows the implementation
      to free any resources allocated on our behalf. }
    Winsock.WSACleanup;
  end;

end;

{ Simple function to locate and return the UTC time from the
  request sent to http://tycho.usno.navy.mil/cgi-bin/timer.pl
  The HTTPResponse parameter contains both the HTTP Headers and
  the HTML served up by the requested resource. }
function ParseResponse(HTTPResponse: TStrings): string;
var
  i: Integer;
begin
  Result := '';

  { Check first line for server response code
    We want something like this: HTTP/1.1 200 OK }
  if Pos('200',HTTPResponse[0]) > 0 then
    begin
      for i := 0 to Pred(HTTPResponse.Count) do
        begin
          { The line we're looking for is something like this:
            <BR>May. 04. 21:55:19 UTC Universal Time }

          // Check each line
          if Pos('UTC',HTTPResponse[i]) > 0 then
            begin
              Result := Copy(HTTPResponse[i],5,Pos('UTC',HTTPResponse[i])-1);
              Break;
            end;

        end;
    end
    else
    Result := 'HTTP Error: '+HTTPResponse[0];
end;


const
  host: PAnsiChar = 'tycho.usno.navy.mil';
  res : PAnsiChar = '/cgi-bin/timer.pl';


var
  Response: TStrings;

begin
  { A TStringList is a TStrings descendant class
    that is used to store and manipulate a list
    of strings.

    Instantiate a stringlist class to
    hold the results of our HTTP GET }
  Response := TStringList.Create;
  try
    // Try an HTTP GET request
    if DoHTTPGET(host,res,Response) then
      begin
        {$IFDEF DEBUG}
        { Write the entire response to
          the console window }
        Writeln(Response.text);
        {$ELSE}
        { Parse the response and write the
          result to the console window }
        Writeln(ParseResponse(Response));
        {$ENDIF DEBUG}
      end
      else
      Writeln('Error retrieving data');

  finally
    Response.Free;
  end;

  // Keep console window open
  Readln;


end.


```



Example using Indy's IdHTTP component.


```Delphi
program ReadUTCTime;

{$APPTYPE CONSOLE}

uses SysUtils, Classes, IdHTTP;

var
  s: string;
  lHTTP: TIdHTTP;
  lReader: TStringReader;
begin
  lHTTP := TIdHTTP.Create(nil);
  try
    lReader := TStringReader.Create(lHTTP.Get('http://tycho.usno.navy.mil/cgi-bin/timer.pl'));
    while lReader.Peek > 0 do
    begin
      s := lReader.ReadLine;
      if Pos('UTC', s) > 0 then
      begin
        Writeln(s);
        Break;
      end;
    end;
  finally
    lHTTP.Free;
    lReader.Free;
  end;
end.
```



## E



```e
interp.waitAtTop(when (def html := <http://tycho.usno.navy.mil/cgi-bin/timer.pl>.getText()) -> {
    def rx`(?s).*>(@time.*? UTC).*` := html
    println(time)
})
```



## Erlang


Using regular expressions:

```erlang
-module(scraping).
-export([main/0]).
-define(Url, "http://tycho.usno.navy.mil/cgi-bin/timer.pl").
-define(Match, "<BR>(.+ UTC)").

main() ->
  inets:start(),
  {ok, {_Status, _Header, HTML}} = httpc:request(?Url),
  {match, [Time]} = re:run(HTML, ?Match, [{capture, all_but_first, binary}]),
  io:format("~s~n",[Time]).
```


=={{header|F_Sharp|F#}}==
This code is asynchronous - it will not block any threads while it waits on a response from the remote server.

```fsharp

open System
open System.Net
open System.Text.RegularExpressions

async {
    use wc = new WebClient()
    let! html = wc.AsyncDownloadString(Uri("http://tycho.usno.navy.mil/cgi-bin/timer.pl"))
    return Regex.Match(html, @"<BR>(.+ UTC)").Groups.[1].Value
}
|> Async.RunSynchronously
|> printfn "%s"

```



## Factor


```factor
USING: http.client io sequences ;

"http://tycho.usno.navy.mil/cgi-bin/timer.pl" http-get nip
[ "UTC" swap start [ 9 - ] [ 1 - ] bi ] keep subseq print
```



## Forth

{{works with|GNU Forth|0.7.0}}

```forth
include unix/socket.fs

: extract-time ( addr len type len -- time len )
  dup >r
  search 0= abort" that time not present!"
  dup >r
  begin -1 /string  over 1- c@ [char] > = until       \ seek back to <BR> at start of line
  r> - r> + ;

s" tycho.usno.navy.mil" 80 open-socket
dup s\" GET /cgi-bin/timer.pl HTTP/1.0\n\n" rot write-socket
dup pad 4096 read-socket
s\" \r\n\r\n" search 0= abort" can't find headers!"   \ skip headers
s" UTC" extract-time type cr
close-socket
```



## FunL


```funl
import io.Source

case Source.fromURL( 'http://tycho.usno.navy.mil/cgi-bin/timer.pl', 'UTF-8' ).getLines().find( ('Eastern' in) ) of
  Some( time ) -> println( time.substring(4) )
  None         -> error( 'Easter time not found' )
```


{{out}}


```txt

Jul. 24, 01:38:23 AM EDT        Eastern Time

```



## Gambas


```gambas
Public Sub Main()
Dim sWeb, sTemp, sOutput As String                                          'Variables

Shell "wget -O /tmp/web http://tycho.usno.navy.mil/cgi-bin/timer.pl" Wait   'Use 'wget' to save the web file in /tmp/

sWeb = File.Load("/tmp/web")                                                'Open file and store in sWeb

For Each sTemp In Split(sWeb, gb.NewLine)                                   'Split the file by NewLines..
  If InStr(sTemp, "UTC") Then                                               'If the line contains "UTC" then..
    sOutPut = sTemp                                                         'Extract the line into sOutput
    Break                                                                   'Get out of here
  End If
Next

Print Mid(sOutput, 5)                                                       'Print the result without the '<BR>' tag

End
```

Output:

```txt

Jun. 10, 15:22:25 UTC           Universal Time

```



## Go


```go
package main

import (
    "bytes"
    "encoding/xml"
    "fmt"
    "io"
    "net/http"
    "regexp"
    "time"
)

func main() {
    resp, err := http.Get("http://tycho.usno.navy.mil/cgi-bin/timer.pl")
    if err != nil {
        fmt.Println(err) // connection or request fail
        return
    }
    defer resp.Body.Close()
    var us string
    var ux int
    utc := []byte("UTC")
    for p := xml.NewDecoder(resp.Body); ; {
        t, err := p.RawToken()
        switch err {
        case nil:
        case io.EOF:
            fmt.Println("UTC not found")
            return
        default:
            fmt.Println(err) // read or parse fail
            return
        }
        if ub, ok := t.(xml.CharData); ok {
            if ux = bytes.Index(ub, utc); ux != -1 {
                // success: found a line with the string "UTC"
                us = string([]byte(ub))
                break
            }
        }
    }
    // first thing to try: parsing the expected date format
    if t, err := time.Parse("Jan. 2, 15:04:05 UTC", us[:ux+3]); err == nil {
        fmt.Println("parsed UTC:", t.Format("January 2, 15:04:05"))
        return
    }
    // fallback: search for anything looking like a time and print that
    tx := regexp.MustCompile("[0-2]?[0-9]:[0-5][0-9]:[0-6][0-9]")
    if justTime := tx.FindString(us); justTime > "" {
        fmt.Println("found UTC:", justTime)
        return
    }
    // last resort: just print the whole element containing "UTC" and hope
    // there is a human readable time in there somewhere.
    fmt.Println(us)
}
```

{{out}}

```txt

parsed UTC: May 23, 00:44:13

```



## Groovy


```groovy
def time = "unknown"
def text = new URL('http://tycho.usno.navy.mil/cgi-bin/timer.pl').eachLine { line ->
    def matcher = (line =~ "<BR>(.+) UTC")
    if (matcher.find()) {
        time = matcher[0][1]
    }
}
println "UTC Time was '$time'"
```

{{out}}

```txt
UTC Time was 'Feb. 26, 11:02:30'
```



## Haskell

Using package HTTP-4000.0.8 from [http://hackage.haskell.org/packages/hackage.html HackgageDB]

```Haskell
import Data.List
import Network.HTTP (simpleHTTP, getResponseBody, getRequest)

tyd = "http://tycho.usno.navy.mil/cgi-bin/timer.pl"

readUTC = simpleHTTP (getRequest tyd)>>=
            fmap ((!!2).head.dropWhile ("UTC"`notElem`).map words.lines). getResponseBody>>=putStrLn
```

Usage in GHCi:

```Haskell
*Main> readUTC
08:30:23
```


== Icon and Unicon ==
=
## Icon
=
Icon has capability to read web pages using the external function cfunc.  The Unicon messaging extensions are more succinct.
=
## Unicon
=

```Unicon
procedure main()
m := open(url := "http://tycho.usno.navy.mil/cgi-bin/timer.pl","m") | stop("Unable to open ",url)
every (p := "") ||:= |read(m)                                                    # read the page into a single string
close(m)

map(p) ? ( tab(find("
")), ="
", write("UTC time=",p[&pos:find(" utc")]))  # scrape and show
end
```



## J


```j
   require 'web/gethttp'

   _8{. ' UTC' taketo gethttp 'http://tycho.usno.navy.mil/cgi-bin/timer.pl'
04:32:44
```


The <code>[[J:Addons/web/gethttp|web/gethttp]]</code> addon uses Wget on Linux or Windows (J ships with Wget on Windows) and cURL on the Mac.

(A sockets solution is also possible.  But, while basic HTTP support is trivial to implement, a full standards compliant implementation and can involve a lot of code to deal with rare corner cases, and the time required to complete a web request is often significantly longer than the time to invoke an external program.  This would imply a fair bit of maintenance and testing overhead to deal with issues which rarely matter, if a direct sockets implementation were used.)


## Java


```java
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;


public class WebTime{
  public static void main(String[] args){
    try{
      URL address = new URL(
          "http://tycho.usno.navy.mil/cgi-bin/timer.pl");
      URLConnection conn = address.openConnection();
      BufferedReader in = new BufferedReader(
          new InputStreamReader(conn.getInputStream()));
      String line;
      while(!(line = in.readLine()).contains("UTC"));
      System.out.println(line.substring(4));
    }catch(IOException e){
      System.err.println("error connecting to server.");
      e.printStackTrace();
    }
  }
}
```



## JavaScript

Due to browser cross-origin restrictions, this script will probably not work in other domains.


```javascript
var req = new XMLHttpRequest();
req.onload = function () {
  var re = /[JFMASOND].+ UTC/; //beginning of month name to 'UTC'
  console.log(this.responseText.match(re)[0]);
};
req.open('GET', 'http://tycho.usno.navy.mil/cgi-bin/timer.pl', true);
req.send();
```



## jq

{{works with|jq|1.4}}

Currently jq does not have built-in curl support, but jq is intended to work seamlessly with other command-line utilities, so we present a simple solution to the problem in the form of a three-line script:

```sh
#!/bin/bash
  curl -Ss 'http://tycho.usno.navy.mil/cgi-bin/timer.pl' |\
    jq -R -r 'if index(" UTC") then .[4:] else empty end'
```

{{out}}

```sh
$ ./Web_scraping.jq
Apr. 21, 05:19:32 UTC   Universal Time
```



## Julia

I'm using the <code>Requests.jl</code> package for this solution.  Note, I used a slightly different URL after finding that the one specified in the task description is deprecated (though it still works).

```Julia
using Requests

function getusnotime()
    const url = "http://tycho.usno.navy.mil/timer.pl"
    s = try
        get(url)
        catch err
        @sprintf "get(%s)\n   => %s" url err
    end
    isa(s, Requests.Response) || return (s, false)
    t = match(r"(?<=<BR>)(.*?UTC)", readstring(s))
    isa(t, RegexMatch) || return (@sprintf("raw html:\n %s", readstring(s)), false)
    return (t.match, true)
end

(t, issuccess) = getusnotime();

if issuccess
    println("The USNO time is ", t)
else
    println("Failed to fetch UNSO time:\n", t)
end

```


{{out}}

```txt

The USNO time is Apr. 20, 17:54:54 UTC

```


'''Checks of Failure Detection'''

By breaking the USNO URL

```txt

Failed to fetch UNSO time:
get(http://sycho.usno.navy.mil/timer.pl)
   => getaddrinfo callback: unknown node or service (EAI_NONAME)

```


By breaking the regular expression (using <code>&lt;BR&gt;(.*UTd)</code>)

```html

Failed to fetch UNSO time:
raw html:
 <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 Final"//EN>
<html>
<body>
<TITLE>What time is it?</TITLE>
<H2> US Naval Observatory Master Clock Time</H2> <H3><PRE>
<BR>Apr. 20, 17:55:31 UTC               Universal Time
<BR>Apr. 20, 01:55:31 PM EDT            Eastern Time
<BR>Apr. 20, 12:55:31 PM CDT            Central Time
<BR>Apr. 20, 11:55:31 AM MDT            Mountain Time
<BR>Apr. 20, 10:55:31 AM PDT            Pacific Time
<BR>Apr. 20, 09:55:31 AM AKDT   Alaska Time
<BR>Apr. 20, 07:55:31 AM HAST   Hawaii-Aleutian Time
</PRE></H3><P><A HREF="http://www.usno.navy.mil"> US Naval Observatory</A>

</body></html>

```



## Kotlin


```scala
// version 1.1.3

import java.net.URL
import java.io.InputStreamReader
import java.util.Scanner

fun main(args: Array<String>) {
    val url = URL("http://tycho.usno.navy.mil/cgi-bin/timer.pl")
    val isr = InputStreamReader(url.openStream())
    val sc = Scanner(isr)
    while (sc.hasNextLine()) {
        val line = sc.nextLine()
        if ("UTC" in line) {
            println(line.drop(4).take(17))
            break
        }
    }
    sc.close()
}
```


Sample output:

```txt

Aug. 20, 22:38:26

```



## Lasso


```Lasso
/* have to be used
local(raw_htmlstring = '<TITLE>What time is it?</TITLE>
<H2> US Naval Observatory Master Clock Time</H2> <H3><PRE>
<BR>Jul. 27, 22:57:22 UTC   Universal Time
<BR>Jul. 27, 06:57:22 PM EDT  Eastern Time
<BR>Jul. 27, 05:57:22 PM CDT  Central Time
<BR>Jul. 27, 04:57:22 PM MDT  Mountain Time
<BR>Jul. 27, 03:57:22 PM PDT  Pacific Time
<BR>Jul. 27, 02:57:22 PM AKDT Alaska Time
<BR>Jul. 27, 12:57:22 PM HAST Hawaii-Aleutian Time
</PRE></H3>
')
*/

// should be used
local(raw_htmlstring = string(include_url('http://tycho.usno.navy.mil/cgi-bin/timer.pl')))

local(
  reg_exp = regexp(-find = `
(.*?) UTC`, -input = #raw_htmlstring, -ignorecase),
  datepart_txt =  #reg_exp -> find ? #reg_exp -> matchstring(1) | string
)

#datepart_txt
'<br />'
// added bonus showing how parsed string can be converted to date object
local(mydate = date(#datepart_txt, -format = `MMM'.' dd',' HH:mm:ss`))
#mydate -> format(`YYYY-MM-dd HH:mm:ss`)
```

Result:
Jul. 27, 22:57:22

2013-07-27 22:57:22


## Liberty BASIC


```lb
if DownloadToFile("http://tycho.usno.navy.mil/cgi-bin/timer.pl", DefaultDir$ + "\timer.htm") = 0 then
        open DefaultDir$ + "\timer.htm" for input as #f
                html$ = lower$(input$(#f, LOF(#f)))
        close #f

        a= instr( html$, "utc" )-1
        print "UTC";mid$( html$, a-9,9)

    end if

end

function DownloadToFile(urlfile$, localfile$)
    open "URLmon" for dll as #url
    calldll #url, "URLDownloadToFileA",_
    0 as long,_         'null
    urlfile$ as ptr,_   'url to download
    localfile$ as ptr,_ 'save file name
    0 as long,_         'reserved, must be 0
    0 as long,_         'callback address, can be 0
    DownloadToFile as ulong  '0=success
    close #url
end function
```


Because Liberty has to do web operations in ways like this, calling Windows DLLs, there is the [[#Run BASIC|Run BASIC]] variant of LB, in which the task becomes a one-liner.


## Lua

{{libheader|LuaSocket}}
The web page is split on the HTML line break tags.  Each line is checked for the required time zone code.  Once it is found, we return the instance within that line of three numbers separated by colons - I.E. the time.

```Lua

local http = require("socket.http")    -- Debian package is 'lua-socket'

function scrapeTime (pageAddress, timeZone)
    local page = http.request(pageAddress)
    if not page then return "Cannot connect" end
    for line in page:gmatch("[^<BR>]*") do
        if line:match(timeZone) then
            return line:match("%d+:%d+:%d+")
        end
    end
end

local url = "http://tycho.usno.navy.mil/cgi-bin/timer.pl"
print(scrapeTime(url, "UTC"))


```

The task description states "just the UTC time" but of course we could return the whole line including the zone name and date if required.


## M2000 Interpreter


```M2000 Interpreter

Module Web_scraping {
	Print "Web scraping"
	function GetTime$(a$, what$="UTC") {
		document a$ ' change string to document
		find a$, what$  ' place data to stack
		Read find_pos
		if find_pos>0 then
			read par_order, par_pos
			b$=paragraph$(a$, par_order)
			k=instr(b$,">")
			if k>0 then if k<par_pos then b$=mid$(b$,k+1) :par_pos-=k
			k=rinstr(b$,"<")
			if k>0 then if k>par_pos then b$=Left(b$,k-1)
			=b$
		end if
	}
	declare msxml2 "MSXML2.XMLHTTP.6.0"
	rem print type$(msxml2)="IXMLHTTPRequest"
	Url$ = "http://tycho.usno.navy.mil/cgi-bin/timer.pl"
	try ok {
		method msxml2, "Open", "GET", url$, false
		method msxml2,"Send"
		with msxml2,"responseText" as txt$
		Print GetTime$(txt$)
	}
	If error or not ok then Print Error$
	declare msxml2 nothing
}
Web_scraping

```



## Maple


```Maple
text := URL:-Get("http://tycho.usno.navy.mil/cgi-bin/timer.pl"):
printf(StringTools:-StringSplit(text,"<BR>")[2]);
```

{{Out|Output}}

```txt
May. 16, 20:17:28 UTC		Universal Time
```



## Mathematica


```mathematica

test = StringSplit[Import["http://tycho.usno.navy.mil/cgi-bin/timer.pl"], "\n"];
Extract[test, Flatten@Position[StringFreeQ[test, "UTC"], False]]

```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
s  = urlread('http://tycho.usno.navy.mil/cgi-bin/timer.pl');
ix = [findstr(s,'<BR>'), length(s)+1];
for k = 2:length(ix)
     tok = s(ix(k-1)+4:ix(k)-1);
     if findstr(tok,'UTC')
  disp(tok);
     end;
end;
```



## mIRC Scripting Language


```mirc
alias utc {
  sockclose UTC
  sockopen UTC tycho.usno.navy.mil 80
}

on *:SOCKOPEN:UTC: {
  sockwrite -n UTC GET /cgi-bin/timer.pl HTTP/1.1
  sockwrite -n UTC Host: tycho.usno.navy.mil
  sockwrite UTC $crlf
}

on *:SOCKREAD:UTC: {
  sockread %UTC
  while ($sockbr) {
    if (<BR>*Universal Time iswm %UTC) {
      echo -ag $remove(%UTC,<BR>,$chr(9),Universal Time)
      unset %UTC
      sockclose UTC
      return
    }
    sockread %UTC
  }
}
```





## Microsoft Small Basic


```vb

'Entered by AykayayCiti -- Earl L. Montgomery
url_name = "http://tycho.usno.navy.mil/cgi-bin/timer.pl"
url_data = Network.GetWebPageContents(url_name)
find = "UTC"
' the length from the UTC to the time is -18 so we need
' to subtract from the UTC position
pos = Text.GetIndexOf(url_data,find)-18
result = Text.GetSubText(url_data,pos,(18+3)) 'plus 3 to add the UTC
TextWindow.WriteLine(result)

'you can eleminate a line of code by putting the
' GetIndexOf insde the GetSubText
'result2 = Text.GetSubText(url_data,Text.GetIndexOf(url_data,find)-18,(18+3))
'TextWindow.WriteLine(result2)
```

{{out}}

```txt

Mar. 19, 04:19:34 UTC
Press any key to continue...

```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

parse arg full_short .
if 'FULL'.abbrev(full_short.upper(), 1) then
  dateFull = isTrue()
else
  dateFull = isFalse()
do
  timeURL = java.net.URL('http://tycho.usno.navy.mil/cgi-bin/timer.pl')
  conn = timeURL.openConnection()
  ibr = BufferedReader(InputStreamReader(conn.getInputStream()))
  line = Rexx
  loop label readLoop while ibr.ready()
    line = ibr.readLine()
    if line = null then leave readLoop
    line = line.translate(' ', '\t')
    if line.wordpos('UTC') > 0 then do
      parse line . '>'       udatetime 'UTC' . -
            0    . '>' . ',' utime     'UTC' .
      if dateFull then
        say udatetime.strip() 'UTC'
      else
        say utime.strip()
      leave readLoop
      end
    end readLoop
  ibr.close()
catch ex = IOException
  ex.printStackTrace()
end

method isTrue() public constant returns boolean
  return 1 == 1
method isFalse() public constant returns boolean
  return \isTrue()

```


{{out}}

```txt

04:29:09

```

or with an argument that matches 'FULL':

```txt

Jul. 18, 04:29:09 UTC

```



## Nim


```nim
import httpclient, strutils

for line in getContent("http://tycho.usno.navy.mil/cgi-bin/timer.pl").splitLines:
  if " UTC" in line:
    echo line[4..line.high]
```



## Objeck


```objeck

use Net;
use IO;
use Structure;

bundle Default {
  class Scrape {
    function : Main(args : String[]) ~ Nil {
      client := HttpClient->New();
      lines := client->Get("http://tycho.usno.navy.mil/cgi-bin/timer.pl", 80);

       i := 0;
       found := false;
      while(found <> true & i < lines->Size()) {
         line := lines->Get(i)->As(String);
        index := line->Find("UTC");
        if(index > -1) {
          time := line->SubString(index - 9, 9)->Trim();
          time->PrintLine();
          found := true;
        };
        i += 1;
      };
    }
  }
}

```



## OCaml



```ocaml
let () =
  let _,_, page_content = make_request ~url:Sys.argv.(1) ~kind:GET () in

  let lines = Str.split (Str.regexp "\n") page_content in
  let str =
    List.find
      (fun line ->
        try ignore(Str.search_forward (Str.regexp "UTC") line 0); true
        with Not_found -> false)
      lines
  in
  let str = Str.global_replace (Str.regexp "<BR>") "" str in
  print_endline str;
;;
```


There are libraries for this, but it's rather interesting to see how to use a socket to achieve this, so see the implementation of the above function <tt>make_request</tt> on [[Web_Scraping/OCaml|this page]].


## ooRexx


This uses the RexxcURL wrapper for libcURL
As an alternative the supplied rxSock socket library could be used


```ooRexx

/* load the RexxcURL library */
Call RxFuncAdd 'CurlLoadFuncs', 'rexxcurl', 'CurlLoadFuncs'
Call CurlLoadFuncs

url = "http://tycho.usno.navy.mil/cgi-bin/timer.pl"

/* get a curl session */
curl = CurlInit()
if curl \= ''
then do
   call CurlSetopt curl, 'URL', Url
   if curlerror.intcode \= 0 then exit
   call curlSetopt curl, 'OUTSTEM', 'stem.'
   if curlerror.intcode \= 0 then exit
   call CurlPerform curl

   /* content is in a stem - lets get it all in a string */
   content = stem.~allItems~makestring('l')
   /* now parse out utc time */
   parse var content content 'Universal Time' .
   utcTime = content~substr(content~lastpos('<BR>') + 4)
   say utcTime
end
```



## Oz


```oz
declare
  [Regex] = {Module.link ['x-oz://contrib/regex']}

  fun {GetPage Url}
     F = {New Open.file init(url:Url)}
     Contents = {F read(list:$ size:all)}
  in
     {F close}
     Contents
  end

  fun {GetDateString Doc}
     case {Regex.search "<BR>([A-Za-z0-9:., ]+ UTC)" Doc}
     of match(1:S#E ...) then {List.take {List.drop Doc S} E-S+1}
     end
  end

  Url = "http://tycho.usno.navy.mil/cgi-bin/timer.pl"
in
  {System.showInfo {GetDateString {GetPage Url}}}
```



## Peloton

English dialect, short form, using integrated Rexx pattern matcher:

```html><@ DEFAREPRS>Rexx Parse</@

<@ DEFPRSLIT>Rexx Parse|'<BR>' UTCtime 'UTC'</@>
<@ LETVARURL>timer|http://tycho.usno.navy.mil/cgi-bin/timer.pl</@>
<@ ACTRPNPRSVAR>Rexx Parse|timer</@>
<@ SAYVAR>UTCtime</@>
```


English dialect, padded variable-length form:

```html><# DEFINE WORKAREA PARSEVALUES>Rexx Parse</#

<# DEFINE PARSEVALUES LITERAL>Rexx Parse|'<BR>' UTCtime 'UTC'</#>
<# LET VARIABLE URLSOURCE>timer|http://tycho.usno.navy.mil/cgi-bin/timer.pl</#>
<# ACT REPLACEBYPATTERN PARSEVALUES VARIABLE>Rexx Parse|timer</#>
<# SAY VARIABLE>UTCtime</#>
```


English dialect, padded short form, using string functions AFT and BEF:

```html><@ SAY AFT BEF URL LIT LIT LIT
http://tycho.usno.navy.mil/cgi-bin/timer.pl| UTC|<BR></@>
```



## Perl

{{libheader|LWP}}

```perl
use LWP::Simple;

my $url = 'http://tycho.usno.navy.mil/cgi-bin/timer.pl';
get($url) =~ /<BR>(.+? UTC)/
    and print "$1\n";
```



## Perl 6


```perl6
use HTTP::Client; # https://github.com/supernovus/perl6-http-client/
my $site = "http://tycho.usno.navy.mil/cgi-bin/timer.pl";
HTTP::Client.new.get($site).content.match(/'<BR>'( .+? <ws> UTC )/)[0].say
```


Note that the string between '<' and '>' refers to regex tokens, so to match a literal '&lt;BR&gt;' you need to quote it, while <ws> refers to the built-in token whitespace.
Also, whitespace is ignored by default in Perl 6 regexes.


## Phix


```Phix
-- demo\rosetta\web_scrape.exw
include builtins\libcurl.e
include builtins\timedate.e

object res = curl_easy_perform_ex("https://tycho.usno.navy.mil/cgi-bin/timer.pl")
if string(res) then
    res = split(res,'\n')
    for i=1 to length(res) do
        integer k = match("UTC",res[i])
        if k then
            res = res[i][5..k-2]
            exit
        end if
    end for
    ?res
    if string(res) then
        timedate td = parse_date_string(res, {"Mmm. d, hh:mm:ss"})
        td[DT_YEAR] = date()[DT_YEAR]
        ?format_timedate(td,"h:mpm Dddd ddth Mmmm")
        ?format_timedate(date(),"h:mpm Dddd ddth Mmmm")
    end if
else
    ?{"some error",res,curl_easy_strerror(res)}
end if
```

{{out}}

```txt

"Apr. 26, 12:38:18"
"12:38pm Friday April 26th"
"1:38pm Friday April 26th"

```

The last line differs because it is British Summer Time here.

Note that since that webpage has no year, td[DT_YEAR] will be 0, and without setting it as shown the weekday would also be wrong.


## PHP

By iterating over each line:


```PHP
<?

$contents = file('http://tycho.usno.navy.mil/cgi-bin/timer.pl');
foreach ($contents as $line){
  if (($pos = strpos($line, ' UTC')) === false) continue;
  echo subStr($line, 4, $pos - 4); //Prints something like "Dec. 06, 16:18:03"
  break;
}
```


By [[regular expressions]] ({{works with|PHP|4.3.0}}):


```PHP
<?

echo preg_replace(
  "/^.*<BR>(.*) UTC.*$/su",
  "\\1",
  file_get_contents('http://tycho.usno.navy.mil/cgi-bin/timer.pl')
);

```



## PicoLisp


```PicoLisp
(load "@lib/http.l")

(client "tycho.usno.navy.mil" 80 "cgi-bin/timer.pl"
   (when (from "<BR>")
      (pack (trim (till "U"))) ) )
```

{{out}}

```txt
-> "Feb. 19, 18:11:37"
```



## PowerShell


```powershell
$wc = New-Object Net.WebClient
$html = $wc.DownloadString('http://tycho.usno.navy.mil/cgi-bin/timer.pl')
$html -match ', (.*) UTC' | Out-Null
Write-Host $Matches[1]
```



### fyi

.NET provides a property named '''UtcNow''':

```PowerShell

[System.DateTime]::UtcNow

```

{{Out}}

```txt

Wednesday, December 14, 2016 2:06:25 AM

```

I am currently in the Pacific timezone:

```PowerShell

[System.DateTime]::Now

```

{{Out}}

```txt

Tuesday, December 13, 2016 6:06:25 PM

```



## PureBasic


```Purebasic
URLDownloadToFile_( #Null, "http://tycho.usno.navy.mil/cgi-bin/timer.pl", "timer.htm", 0, #Null)
ReadFile(0, "timer.htm")
While Not Eof(0)    :    Text$ + ReadString(0)    :    Wend
MessageRequester("Time", Mid(Text$, FindString(Text$, "UTC", 1) - 9 , 8))
```



## Python


```python
import urllib
page = urllib.urlopen('http://tycho.usno.navy.mil/cgi-bin/timer.pl')
for line in page:
    if ' UTC' in line:
        print line.strip()[4:]
        break
page.close()
```

{{out}}

```txt
Aug. 12, 15:22:08 UTC           Universal Time
```



## R


There are two ways of scraping data from webpages.  You can either convert the page into an array of strings containing each line of HTML and use regular expressions to locate the useful data, or you can parse the HTML and use xPath to locate them.  The first method is quicker for simpler pages, but may become more difficult for more complicated ones.


###  Regex method


Read the page as lines, find the line containing the string "UTC", then extract the portion of that string that is the date.


```R

all_lines <- readLines("http://tycho.usno.navy.mil/cgi-bin/timer.pl")
utc_line <- grep("UTC", all_lines, value = TRUE)
matched <- regexpr("(\\w{3}.*UTC)", utc_line)
utc_time_str <- substring(line, matched, matched + attr(matched, "match.length") - 1L)

```


The last three lines can be made simpler by using {{libheader|stringr}}


```R

library(stringr)
utc_line <- all_lines[str_detect(all_lines, "UTC")]
utc_time_str <- str_extract(utc_line, "\\w{3}.*UTC")

```


Finally, the date and time must be parsed and printed in the desired format.


```R

utc_time <- strptime(utc_time_str, "%b. %d, %H:%M:%S UTC")
strftime(utc_time, "%A, %d %B %Y, %H:%M:%S")

```


 Friday, 13 May 2011, 15:12:20


###  Parsing method


{{libheader|RCurl}}
{{libheader|XML}}

First, retrieve the web page. See [[HTTP_Request]] for more options with this.


```R

library(RCurl)
web_page <- getURL("http://tycho.usno.navy.mil/cgi-bin/timer.pl")

```


Now parse the html code into a tree and retrieve the pre node that contains interesting bit.  Without xPath, the syntax is quite clunky.


```R

library(XML)
page_tree <- htmlTreeParse(webpage)
times_node <- page_tree$children$html$children$body$children$h3$children$pre$children
times_node <- times_node[names(times_node) == "text"]
time_lines <- sapply(times_node, function(x) x$value)

```


Here, xPath simplifies things a little bit.


```R

page_tree <- htmlTreeParse(web_page, useInternalNodes = TRUE)
times_node <- xpathSApply(page_tree, "//pre")[[1]]
times_node <- times_node[names(times_node) == "text"]
time_lines <- sapply(times_node, function(x) as(x, "character"))

```


Either way, the solution proceeds from here as in the regex method.

```R

utc_line <- time_lines[str_detect(time_lines, "UTC")]
#etc.

```



## Racket



```Racket

#lang racket
(require net/url)
((compose1 car (curry regexp-match #rx"[^ <>][^<>]+ UTC")
           port->string get-pure-port string->url)
 "https://tycho.usno.navy.mil/cgi-bin/timer.pl")

```



## REBOL


```REBOL
REBOL [
  Title: "Web Scraping"
  URL: http://rosettacode.org/wiki/Web_Scraping
]

; Notice that REBOL understands unquoted URL's:

service: http://tycho.usno.navy.mil/cgi-bin/timer.pl

; The 'read' function can read from any data scheme that REBOL knows
; about, which includes web URLs. NOTE: Depending on your security
; settings, REBOL may ask you for permission to contact the service.

html: read service

; I parse the HTML to find the first
 (note the unquoted HTML tag
; -- REBOL understands those too), then copy the current time from
; there to the "UTC" terminator.

; I have the "to end" in the parse rule so the parse will succeed.
; Not strictly necessary once I've got the time, but good practice.

parse html [thru
 copy current thru "UTC" to end]

print ["Current UTC time:" current]
```



## Run BASIC


```runbasic
print word$(word$(httpget$("http://tycho.usno.navy.mil/cgi-bin/timer.pl"),1,"UTC"),2,"<BR>")
```

{{out}}

```txt
May. 09, 16:13:44
```



## Ruby


A verbose example for comparison


```ruby
require "open-uri"

open('http://tycho.usno.navy.mil/cgi-bin/timer.pl') do |p|
  p.each_line do |line|
    if line =~ /UTC/
      puts line.match(/ (\d{1,2}:\d{1,2}:\d{1,2}) /)
      break
    end
  end
end

```


A more concise example


```ruby
require 'open-uri'
puts URI.parse('http://tycho.usno.navy.mil/cgi-bin/timer.pl').read.match(/ (\d{1,2}:\d{1,2}:\d{1,2}) UTC/)[1]

```



## Scala



```scala

import scala.io.Source

object WebTime extends Application {
  val text = Source.fromURL("http://tycho.usno.navy.mil/cgi-bin/timer.pl")
  val utc = text.getLines.find(_.contains("UTC"))
  utc match {
    case Some(s) => println(s.substring(4))
    case _ => println("error")
  }
}

```



## Scheme

{{works with|Guile}}

```scheme
; Use the regular expression module to parse the url
(use-modules (ice-9 regex) (ice-9 rdelim))

; Variable to store result
(define time "")

; Set the url and parse the hostname, port, and path into variables
(define url "http://tycho.usno.navy.mil/cgi-bin/timer.pl")
(define r (make-regexp "^(http://)?([^:/]+)(:)?(([0-9])+)?(/.*)?" regexp/icase))
(define host (match:substring (regexp-exec r url) 2))
(define port (match:substring (regexp-exec r url) 4))
(define path (match:substring (regexp-exec r url) 6))

; Set port to 80 if it wasn't set above and convert from a string to a number
(if (eq? port #f) (define port "80"))
(define port (string->number port))

; Connect to remote host on specified port
(let ((s (socket PF_INET SOCK_STREAM 0)))
        (connect s AF_INET (car (hostent:addr-list (gethostbyname host))) port)

; Send a HTTP request for the specified path
        (display "GET " s)
        (display path s)
        (display " HTTP/1.0\r\n\r\n" s)

        (set! r (make-regexp "<BR>(.+? UTC)"))
        (do ((line (read-line s) (read-line s))) ((eof-object? line))
                (if (regexp-match? (regexp-exec r line))
                        (set! time (match:substring (regexp-exec r line) 1)))))

; Display result
(display time)
(newline)
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "gethttp.s7i";

const proc: main is func
  local
    var string: pageWithTime is "";
    var integer: posOfUTC is 0;
    var integer: posOfBR is 0;
    var string: timeStri is "";
  begin
    pageWithTime := getHttp("tycho.usno.navy.mil/cgi-bin/timer.pl");
    posOfUTC := pos(pageWithTime, "UTC");
    if posOfUTC <> 0 then
      posOfBR := rpos(pageWithTime, "<BR>", posOfUTC);
      if posOfBR <> 0 then
        timeStri := pageWithTime[posOfBR + 4 .. pred(posOfUTC)];
        writeln(timeStri);
      end if;
    end if;
  end func;
```



## Sidef


```ruby
var ua = frequire('LWP::Simple');
var url = 'http://tycho.usno.navy.mil/cgi-bin/timer.pl';
var match = /<BR>(.+? UTC)/.match(ua.get(url));
say match[0] if match;
```

{{out}}

```txt

Oct. 27, 00:20:50 UTC

```




## Tcl


### <tt>http</tt> and regular expressions


```tcl
package require http

set request [http::geturl "http://tycho.usno.navy.mil/cgi-bin/timer.pl"]
if {[regexp -line {<BR>(.* UTC)} [http::data $request] --> utc]} {
    puts $utc
}
```


===curl(1) and list operations===
Considering the web resource returns tabular data wrapped in a <tt>&lt;PRE&gt;</tt> tag, you can use Tcl's list processing commands to process its contents.

```tcl
set data [exec curl -s http://tycho.usno.navy.mil/cgi-bin/timer.pl]
puts [lrange [lsearch -glob -inline [split $data <BR>] *UTC*] 0 3]
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
SET time = REQUEST ("http://tycho.usno.navy.mil/cgi-bin/timer.pl")
SET utc  = FILTER  (time,":*UTC*:",-)

```




## ToffeeScript


```coffeescript
e, page = require('request').get! 'http://tycho.usno.navy.mil/cgi-bin/timer.pl'
l = line for line in page.body.split('\n') when line.indexOf('UTC')>0
console.log l.substr(4,l.length-20)
```



## TXR



### = Robust =


Large amounts of the document are matched (in fact the entire thing!), rather than blindly looking for some small amount of context.

If the web page changes too much, the query will fail to match. TXR will print the word "false" and terminate with a failed exit status. This is preferrable to finding a false positive match and printing a wrong result. (E.g. any random garbage that happened to be in a line of HTML accidentally containing the string UTC).


```txr
@(next @(open-command "wget -c http://tycho.usno.navy.mil/cgi-bin/timer.pl -O - 2> /dev/null"))
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 Final"//EN>
<html>
<body>
<TITLE>What time is it?</TITLE>
<H2> US Naval Observatory Master Clock Time</H2> <H3><PRE>
@(collect :vars (MO DD HH MM SS (PM "  ") TZ TZNAME))
<BR>@MO. @DD, @HH:@MM:@SS @(maybe)@{PM /PM/} @(end)@TZ@/\t+/@TZNAME
@  (until)
</PRE>@/.*/
@(end)
</PRE></H3><P><A HREF="http://www.usno.navy.mil"> US Naval Observatory</A>

</body></html>
@(output)
@  (repeat)
@MO-@DD @HH:@MM:@SS @PM @TZ
@  (end)
@(end)
```


Sample run:
```txt
$ txr navytime.txr
Nov-22 22:49:41    UTC
Nov-22 05:49:41 PM EST
Nov-22 04:49:41 PM CST
Nov-22 03:49:41 PM MST
Nov-22 02:49:41 PM PST
Nov-22 01:49:41 PM AKST
Nov-22 12:49:41 PM HAST
```


Get just the UTC time:


```txt
$ txr -DTZ=UTC navytime.txr
Nov-22 22:50:16    UTC
```



### = Naive =


Skip stuff until a line beginning with <code>&lt;BR&gt;</code> has some stuff before "UTC", and capture that stuff:


```txr
@(next @(open-command "wget -c http://tycho.usno.navy.mil/cgi-bin/timer.pl -O - 2> /dev/null"))
@(skip)
<BR>@time@\ UTC@(skip)
@(output)
@time
@(end)
```



## UNIX Shell

This solution uses 'curl' and the standard POSIX command 'sed'.


```bash
#!/bin/sh
curl -s http://tycho.usno.navy.mil/cgi-bin/timer.pl |
   sed -ne 's/^<BR>\(.* UTC\).*$/\1/p'
```


This solution uses tcsh, wget and awk


```tcsh
#!/usr/bin/tcsh -f
set page = `wget -q -O- "http://tycho.usno.navy.mil/cgi-bin/timer.pl"`
echo `awk -v s="${page[22]}" 'BEGIN{print substr(s,5,length(s))}'` ${page[23]} ${page[24]}
```



## Ursala

This works by launching the wget command in a separate process and capturing its output.
The program is compiled to an executable command.

```Ursala
#import std
#import cli

#executable ('parameterized','')

whatime =

<.file$[contents: --<''>]>+ -+
   @hm skip/*4+ ~=(9%cOi&)-~l*+ *~ ~&K3/'UTC',
   (ask bash)/0+ -[wget -O - http://tycho.usno.navy.mil/cgi-bin/timer.pl]-!+-
```

Here is a bash session.

```txt
$ whatime
Jun. 26, 20:49:52 UTC
```


{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have network access. -->


## VBA

Note For this example I altered the VBScript

```vb
Rem add Microsoft VBScript Regular Expression X.X to your Tools References

Function GetUTC() As String
    Url = "http://tycho.usno.navy.mil/cgi-bin/timer.pl"
    With CreateObject("MSXML2.XMLHTTP.6.0")
        .Open "GET", Url, False
        .send
        arrt = Split(.responseText, vbLf)
    End With
    For Each t In arrt
        If InStr(t, "UTC") Then
            GetUTC = StripHttpTags(t)

            Exit For
        End If
    Next
End Function

Function StripHttpTags(s)
    With New RegExp
        .Global = True
        .Pattern = "\<.+?\>"
        If .Test(s) Then
            StripHttpTags = .Replace(s, "")
        Else
            StripHttpTags = s
        End If
    End With
End Function

Sub getTime()
    Rem starting point
    Dim ReturnValue As String
    ReturnValue = GetUTC
    Rem debug.print can be removed
    Debug.Print ReturnValue
    MsgBox (ReturnValue)
End Sub
```



{{Out}}

```txt

Run getTime Subroutine

Mar. 05, 00:57:37 UTC       Universal Time

```



## VBScript


```vb
Function GetUTC() As String
    Url = "http://tycho.usno.navy.mil/cgi-bin/timer.pl"
    With CreateObject("MSXML2.XMLHTTP.6.0")
        .Open "GET", Url, False
        .send
        arrt = Split(.responseText, vbLf)
    End With
    For Each t In arrt
        If InStr(t, "UTC") Then
            GetUTC = StripHttpTags(t)

            Exit For
        End If
    Next
End Function

Function StripHttpTags(s)
    With New RegExp
        .Global = True
        .Pattern = "\<.+?\>"
        If .Test(s) Then
            StripHttpTags = .Replace(s, "")
        Else
            StripHttpTags = s
        End If
    End With
End Function

WScript.StdOut.Write GetUTC
WScript.StdOut.WriteLine
```


{{Out}}

```txt

Run getTime Subroutine

Apr. 21, 21:02:03 UTC          Universal Time

```





## Visual Basic .NET

New, .NET way with StringReader:

```vbnet
Imports System.Net
Imports System.IO
        Dim client As WebClient = New WebClient()
        Dim content As String = client.DownloadString("http://tycho.usno.navy.mil/cgi-bin/timer.pl")
        Dim sr As New StringReader(content)
        While sr.peek <> -1
            Dim s As String = sr.ReadLine
            If s.Contains("UTC") Then
                Dim time As String() = s.Substring(4).Split(vbTab)
                Console.WriteLine(time(0))
            End If
        End While
```


Alternative, old fashioned way using VB "Split" function:

```vbnet
Imports System.Net
        Dim client As WebClient = New WebClient()
        Dim content As String = client.DownloadString("http://tycho.usno.navy.mil/cgi-bin/timer.pl")
        Dim lines() As String = Split(content, vbLf) 'may need vbCrLf
        For Each line In lines
            If line.Contains("UTC") Then
                Dim time As String() = line.Substring(4).Split(vbTab)
                Console.WriteLine(time(0))
            End If
        Next
```



## Xidel

http://videlibri.sourceforge.net/xidel.html

```sh
#!/bin/bash
xidel -s "https://tycho.usno.navy.mil/cgi-bin/timer.pl" -e 'extract(//h3,"([\d:]+) UTC",1)'
```

{{out}}

```sh>20:43:05</lang



## zkl


```zkl
const HOST="tycho.usno.navy.mil", PORT=80, dir="/cgi-bin/timer.pl";
get:="GET %s HTTP/1.0\r\nHost: %s:%s\r\n\r\n".fmt(dir,HOST,PORT);
server:=Network.TCPClientSocket.connectTo(HOST,PORT);
server.write(get);       // send request to web serer
data:=server.read(True); // read data from web server

data.seek(data.find("UTC")); // middle of line
c:=data.seek(Void,0);       // start of line
line:=data[c,data.seek(Void,1)-c].text;
line.print(); // the HTML UTC line

re:=RegExp(0'|.*(\d\d:\d\d:\d\d)|);  // get time
re.search(line);
re.matched[1].println();
```

{{out}}

```txt

<BR>Mar. 18, 06:18:31 UTC   Universal Time
06:18:31

```
