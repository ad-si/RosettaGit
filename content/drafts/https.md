+++
title = "HTTPS"
description = ""
date = 2019-04-29T22:00:44Z
aliases = []
[extra]
id = 4289
[taxonomies]
categories = []
tags = []
+++

{{task|Programming environment operations}}
[[Category:Networking and Web Interaction]]

;Task:
Print an HTTPS URL's content to the console. Checking the host certificate for validity is recommended.

The client should not authenticate itself to the server — the webpage https://sourceforge.net/ supports that access policy — as that is the subject of other [[HTTPS request with authentication|tasks]].

Readers may wish to contrast with the [[HTTP Request]] task, and also the task on [[HTTPS request with authentication]].





## Ada

{{libheader|AWS}}
Exactly the same as the HTTP task, assuming you compiled AWS with openssl support.

```ada

with AWS.Client;
with AWS.Response;
with Ada.Text_IO; use Ada.Text_IO;
procedure GetHttps is
begin
   Put_Line (AWS.Response.Message_Body (AWS.Client.Get (
      URL => "https://sourceforge.net/")));
end GetHttps;

```



## AutoHotkey

{{libheader|wininet}}

```AutoHotkey

URL      := "https://sourceforge.net/"
WININET_Init()
msgbox % html := UrlGetContents(URL)
WININET_UnInit()
return
#include urlgetcontents.ahk
#include wininet.ahk

```



## BaCon

This code requires BaCon 3.8.2 or later.

```freebasic
OPTION TLS TRUE
website$ = "www.google.com"

OPEN website$ & ":443" FOR NETWORK AS mynet

SEND "GET / HTTP/1.1\r\nHost: " & website$ & "\r\n\r\n" TO mynet
WHILE WAIT(mynet, 1000)
    RECEIVE dat$ FROM mynet
    total$ = total$ & dat$
    IF REGEX(dat$, "\r\n\r\n$") THEN BREAK           : ' Quit receiving data when end indicator was reached
WEND

CLOSE NETWORK mynet

PRINT REPLACE$(total$, "\r\n[0-9a-fA-F]+\r\n", "\r\n", TRUE) : ' Remove chunk indicators from HTML data

```



## Batch File


```batch

:: Must have curl.exe
curl.exe -k -s -L https://sourceforge.net/

```



## C

{{libheader|libcurl}}

```c

#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

int
main(void)
{
        CURL *curl;
        char buffer[CURL_ERROR_SIZE];

        if ((curl = curl_easy_init()) != NULL) {
                curl_easy_setopt(curl, CURLOPT_URL, "https://sourceforge.net/");
                curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1);
                curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, buffer);
                if (curl_easy_perform(curl) != CURLE_OK) {
                        fprintf(stderr, "%s\n", buffer);
                        return EXIT_FAILURE;
                }
                curl_easy_cleanup(curl);
        }
        return EXIT_SUCCESS;
}

```


=={{header|C sharp|C#}}==
{{works with|C sharp|3.0}}


```csharp

using System;
using System.Net;

class Program
{
    static void Main(string[] args)
    {
        var client = new WebClient();
        var data = client.DownloadString("https://www.google.com");

        Console.WriteLine(data);
    }
}

```


This does not work for urls requiring a secure (SSL) connection.


## Clojure

Using the duck-streams as a convenient wrapper for Java's networking classes, grabbing the contents of an HTTPS URL is as easy as:


```clojure

(use '[clojure.contrib.duck-streams :only (slurp*)])
(print (slurp* "https://sourceforge.net"))

```


The usual Java mechanisms can be used to manage acceptance of SSL certificates if required.

{{works with|Clojure|1.2}}

```clojure

(print (slurp "https://sourceforge.net"))

```



## Common Lisp

{{libheader|DRAKMA}}

First grabbing the entire body as a string, and then by pulling from a stream.  This is the same code as in [[HTTP Request]]; <code>drakma:http-request</code> supports SSL.


```lisp

(defun wget-drakma-string (url &optional (out *standard-output*))
  "Grab the body as a string, and write it to out."
  (write-string (drakma:http-request url) out))

(defun wget-drakma-stream (url &optional (out *standard-output*))
  "Grab the body as a stream, and write it to out."
  (loop with body = (drakma:http-request url :want-stream t)
        for line = (read-line body nil nil)
        while line do (write-line line)
        finally (close body)))

;; Use
(wget-drakma-stream "https://sourceforge.net")

```



## Delphi

{{libheader|OpenSSL}}

```Delphi

program ShowHTTPS;

{$APPTYPE CONSOLE}

uses IdHttp, IdSSLOpenSSL;

var
  s: string;
  lHTTP: TIdHTTP;
begin
  lHTTP := TIdHTTP.Create(nil);
  try
    lHTTP.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(lHTTP);
    lHTTP.HandleRedirects := True;
    s := lHTTP.Get('https://sourceforge.net/');
    Writeln(s);
  finally
    lHTTP.Free;
  end;
end.

```



## EchoLisp

'''file->string''' usage: the server must allow cross-domain access, or a browser add-on like cors-everywhere must be installed to bypass cross-domain checking.

```scheme

;; asynchronous call back definition
(define (success name text) (writeln 'Loaded name) (writeln text))
;;
(file->string success "https:/sourceforge.net")

```



## Erlang


### Synchronous


```erlang

-module(main).
-export([main/1]).

main([Url|[]]) ->
   inets:start(),
   ssl:start(),
   case http:request(get, {URL, []}, [{ssl,[{verify,0}]}], []) of
       {ok, {_V, _H, Body}} -> io:fwrite("~p~n",[Body]);
       {error, Res} -> io:fwrite("~p~n", [Res])
   end.

```



### Asynchronous


```erlang

-module(main).
-export([main/1]).

main([Url|[]]) ->
   inets:start(),
   ssl:start(),
   http:request(get, {Url, [] }, [{ssl,[{verify,0}]}], [{sync, false}]),
   receive
       {http, {_ReqId, Res}} -> io:fwrite("~p~n",[Res]);
       _Any -> io:fwrite("Error: ~p~n",[_Any])
       after 10000 -> io:fwrite("Timed out.~n",[])
   end.

```


Using it

```erlang

|escript ./req.erl https://sourceforge.net/

```



## Fortran

There is no such network library for communicating with a HTTPS server in fortran. Use appropriate tools (eg. simple node.js snippet)

```fortran

program https_example
   implicit none
   character (len=:), allocatable :: code
   character (len=:), allocatable :: command
   logical:: waitForProcess

   ! execute Node.js code
   code = "var https = require('https'); &
   https.get('https://sourceforge.net/', function(res) {&
   console.log('statusCode: ', res.statusCode);&
   console.log('Is authorized:' + res.socket.authorized);&
   console.log(res.socket.getPeerCertificate());&
   res.on('data', function(d) {process.stdout.write(d);});});"

   command = 'node -e "' // code // '"'
   call execute_command_line (command, wait=waitForProcess)
end program https_example

```


=={{header|F_Sharp|F#}}==
The underlying .NET classes handle secure web connections the same way they manage insecure connections.

```fsharp

#light
let wget (url : string) =
    let c = new System.Net.WebClient()
    c.DownloadString(url)

```



## Frink


```Frink

print[read["https://sourceforge.net/"]

```



## Go


```go

package main

import (
    "io"
    "log"
    "net/http"
    "os"
)

func main() {
    r, err := http.Get("https://sourceforge.net/")
    if err != nil {
        log.Fatalln(err)
    }
    io.Copy(os.Stdout, r.Body)
}

```



## Groovy


```groovy

new URL("https://sourceforge.net").eachLine { println it }

```



## Haskell

{{libheader|http-conduit}}
{{Works with|GHC|7.4.1}}

This is just the example from [http://hackage.haskell.org/packages/archive/http-conduit/1.8.5.1/doc/html/Network-HTTP-Conduit.html Network.HTTP.Conduit], with the http URL replaced with an https one, since [http://hackage.haskell.org/package/http-conduit http-conduit] natively supports https without needing any additional work.


```haskell
#!/usr/bin/runhaskell

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import Network (withSocketsDo)

main = withSocketsDo
    $ simpleHttp "https://sourceforge.net/" >>= L.putStr
```


==Icon and {{header|Unicon}}==

```unicon
# Requires Unicon version 13
procedure main(arglist)
    url := (\arglist[1] | "https://sourceforge.net/")
    w := open(url, "m-") | stop("Cannot open " || url)
    while write(read(w))
    close(w)
end
```


{{out}}

```txt
prompt$ unicon -s https.icn -x | head -n2
<!doctype html>
<!-- Server: sfs-consume-15 -->

```



## Ioke

{{trans|Java}}

```ioke

connection = URL new("https://sourceforge.net") openConnection
scanner = Scanner new(connection getInputStream)

while(scanner hasNext,
  scanner next println
)

```



## J

Using <tt>gethttp</tt> from [[Web Scraping#J|Web Scraping]]


```j

   #page=: gethttp'https://sourceforge.net'
0
   #page=: '--no-check-certificate' gethttp'https://sourceforge.net'
900

```


(We can not load the example page using https unless we disable certificate checking.  The numbers are the number of characters retrieved.)


## Java

Additional certificate information is available through the [http://java.sun.com/javase/6/docs/api/javax/net/ssl/HttpsURLConnection.html javax.net.ssl.HttpsURLConnection] interface.

```Java

URL url = new URL("https://sourceforge.net");
HttpsURLConnection connection = (HttpsURLConnection) url.openConnection();
Scanner scanner = new Scanner(connection.getInputStream());

while (scanner.hasNext()) {
    System.out.println(scanner.next());
}

```



## JavaScript


###  Modern Browsers


```JavaScript

fetch("https://sourceforge.net")
    .then(function(response) {
        return response.text();
     })
    .then(function(content) {
        console.log(content)
    })
    .catch(function (err){
         console.error(err)
    });

```



###  Node.js


```JavaScript

const https = require('https');

https.get("https://sourceforge.net", (resp) => {
  let content = '';

  // A chunk of data has been recieved.
  resp.on('data', (chunk) => {
    content += chunk;
  });

  // The whole response has been received. Print out the result.
  resp.on('end', () => {
    console.log(content);
  });

}).on("error", (err) => {
  console.error("Error: " + err.message);
});

```



## Julia


```julia
# v0.6.0

using Requests

str = readstring(get("https://sourceforge.net/"))
```



## Kotlin



```scala
// version 1.1.2
import java.net.URL
import javax.net.ssl.HttpsURLConnection
import java.io.InputStreamReader
import java.util.Scanner

fun main(args: Array<String>) {
    val url = URL("https://en.wikipedia.org/wiki/Main_Page")
    val connection = url.openConnection() as HttpsURLConnection
    val isr = InputStreamReader(connection.inputStream)
    val sc = Scanner(isr)
    while (sc.hasNextLine()) println(sc.nextLine())
    sc.close()
}
```



## Lasso


```Lasso
local(x = curl('https://sourceforge.net'))
local(y = #x->result)
#y->asString
```


If a site with an invalid SSL Cert is encountered the curl type throws the following error:
{{out}}

```txt
FAILURE: 60 Peer certificate cannot be authenticated with given CA certificates
```



## Lingo


*Windows:
{{libheader|Curl Xtra}}

```lingo
ch = xtra("Curl").new()
CURLOPT_URL = 10002
ch.setOption(CURLOPT_URL, "https://sourceforge.net")
res = ch.exec(1)
if integerP(res) then
  put "Error:" && curl_error(res)
else
  put "Result:" && res.readRawString(res.length)
end if
-- "Result: <!doctype html> ..."
```


*Mac OS X:
{{libheader|Shell Xtra}}

```lingo
sx = xtra("Shell").new()
put sx.shell_cmd("curl https://sourceforge.net")
```



## LiveCode

Blocking version
```LiveCode
libURLSetSSLVerification true  --check cert
get URL "https://sourceforge.net/"
```

Non-blocking version, execute getWebResource

```LiveCode
on myUrlDownloadFinished
    get URL "https://sourceforge.net/" -- this will now fetch a locally cached copy
    put it
end myUrlDownloadFinished

command getWebResource
    libURLFollowHttpRedirects true
    libURLSetSSLVerification true  --check cert
    load URL "https://sourceforge.net/" with message "myUrlDownloadFinished"
end getWebResource
```



## LSL

Virtually identical to the HTTP Task.

To test it yourself; rez a box on the ground, and add the following as a New Script.

```LSL
string sURL = "https://SourceForge.Net/";
key kHttpRequestId;
default {
	state_entry() {
		kHttpRequestId = llHTTPRequest(sURL, [], "");
	}
	http_response(key kRequestId, integer iStatus, list lMetaData, string sBody) {
		if(kRequestId==kHttpRequestId) {
			llOwnerSay("Status="+(string)iStatus);
			integer x = 0;
			for(x=0 ; x<llGetListLength(lMetaData) ; x++) {
				llOwnerSay("llList2String(lMetaData, "+(string)x+")="+llList2String(lMetaData, x));
			}
			list lBody = llParseString2List(sBody, ["\n"], []);
			for(x=0 ; x<llGetListLength(lBody) ; x++) {
				llOwnerSay("llList2String(lBody, "+(string)x+")="+llList2String(lBody, x));
			}
		}
	}
}
```

Output:

```txt
Status=200
llList2String(lMetaData, 0)=0
llList2String(lMetaData, 1)=2048
llList2String(lBody, 0)=<!doctype html>
llList2String(lBody, 1)=<!-- Server: sfs-consume-7 -->
llList2String(lBody, 2)=<!--[if lt IE 7 ]> <html lang="en" class="no-js ie6" > <![endif]-->
llList2String(lBody, 3)=<!--[if IE 7 ]>    <html lang="en" class="no-js ie7" > <![endif]-->
llList2String(lBody, 4)=<!--[if IE 8 ]>    <html lang="en" class="no-js ie8" > <![endif]-->
llList2String(lBody, 5)=<!--[if IE 9 ]>    <html lang="en" class="no-js ie9" > <![endif]-->
llList2String(lBody, 6)=<!--[if (gt IE 9)|!(IE)]>--> <html lang="en" class="no-js"> <!--<![endif]-->
llList2String(lBody, 7)=    <head>
llList2String(lBody, 8)=        <meta charset="utf-8">
llList2String(lBody, 9)=
llList2String(lBody, 10)=        <meta id="webtracker" name="webtracker" content='{"event_id": "ea71f064-ca28-11e1-98cc-0019b9f0e8fc"}'>
llList2String(lBody, 11)=        <meta name="description" content="Free, secure and fast downloads from the largest Open Source applications and software directory - SourceForge.net">
llList2String(lBody, 12)=        <meta name="keywords" content="Open Source, Open Source Software, Development, Community, Source Code, Secure,  Downloads, Free Software">
llList2String(lBody, 13)=<meta name="msvalidate.01" content="0279349BB9CF7ACA882F86F29C50D3EA" />
llList2String(lBody, 14)=        <meta name="viewport" content="width=device-width, initial-scale=1.0">
llList2String(lBody, 15)=        <title>SourceForge - Download, Develop and Publish Free Open Source Software</title>
llList2String(lBody, 16)=        <link rel="shortcut icon" href="http://a.fsdn.com/con/img/sftheme/favicon.ico">
...   ...   ...   ...   ...   ...   ...   ...   ...   ...   ...   ...   ...   ...
```



## Lua

{{works with|Lua|5.1 - 5.3}}
{{libheader|lua-http}}

```lua

local request = require('http.request')
local headers, stream = request.new_from_uri("https://sourceforge.net/"):go()
local body = stream:get_body_as_string()
local status = headers:get(':status')
io.write(string.format('Status: %d\nBody: %s\n', status, body)

```

HTTPS requests can be also done with the much smaller libraries like [[LuaSec]] or [[lua-requests]], but it currently don't support redirects, which is why I used [[lua-http]] in this example.


## Maple


```Maple

content := URL:-Get( "https://www.google.ca/" );

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Straight forward "Import" task. More complicated secure web access can be done using J/Link; essentially a link to Java API.

```Mathematica

content=Import["https://sourceforge.net", "HTML"]

```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
s=urlread('https://sourceforge.net/')
```



## Nemerle

This example is essentially identical to the [[HTTP]] task because the <tt>WebClient</tt> object can be used with http:, https:, ftp: and file: uri's.

```Nemerle
using System;
using System.Console;
using System.Net;
using System.IO;

module HTTP
{
    Main() : void
    {
        def wc = WebClient();
        def myStream = wc.OpenRead(https://sourceforge.com);
        def sr = StreamReader(myStream);

        WriteLine(sr.ReadToEnd());
        myStream.Close()
    }
}
```



## NewLISP


```newlisp
(! "curl https://sourceforge.net")
```



## Nim

{{libheader|OpenSSL}}
Compile with <code>nim c -d:ssl httpsClient.nim</code>:

```nim
import httpclient

var client = newHttpClient()
echo client.getContent("https://sourceforge.net")
```



## Objeck


```objeck

use HTTP;

class HttpsTest {
  function : Main(args : String[]) ~ Nil {
    client := HttpsClient->New();
    lines := client->Get("https://sourceforge.net");
    each(i : lines) {
      lines->Get(i)->As(String)->PrintLine();
    };
  }
}

```



## Pascal

{{works with|Free Pascal}}
Using [http://wiki.freepascal.org/fphttpclient fphttpclient]

```pascal
{$mode objfpc}{$H+}
uses fphttpclient;

var
  s: string;
  hc: tfphttpclient;

begin
  hc := tfphttpclient.create(nil);
  try
    s := hc.get('https://www.example.com')
  finally
    hc.free
  end;
  writeln(s)
end.
```



## Perl

{{libheader|LWP}}

```perl

use strict;
use LWP::UserAgent;

my $url = 'https://www.rosettacode.org';
my $response = LWP::UserAgent->new->get( $url );

$response->is_success or die "Failed to GET '$url': ", $response->status_line;

print $response->as_string;

```



## Perl 6

{{works with|Rakudo|2017.09}}
There are several modules that provide HTTPS capability. WWW and HTTP::UserAgent are probably the most popular right now, but others exist.


```perl6
use WWW;
say get 'https://sourceforge.net/';
```

or

```perl6
use HTTP::UserAgent;
say HTTP::UserAgent.new.get('https://sourceforge.net/').content;
```



## Phix

{{libheader|libcurl}}
Exactly the same as the [[HTTP#Phix]] task.

```Phix
include builtins\libcurl.e
curl_global_init()
atom curl = curl_easy_init()
curl_easy_setopt(curl, CURLOPT_URL, "https://sourceforge.net/")
object res = curl_easy_perform_ex(curl)
curl_easy_cleanup(curl)
curl_global_cleanup()

puts(1,res)
```



## PHP


```php

echo file_get_contents('https://sourceforge.net');

```



## PicoLisp

PicoLisp has no functionality for communicating with a HTTPS server
(only for the other direction), but it is easy to use an external tool

```PicoLisp

(in '(curl "https://sourceforge.net")  # Open a pipe to 'curl'
   (out NIL (echo)) )                  # Echo to standard output

```



## Pike


```pike

int main() {
    write("%s\n", Protocols.HTTP.get_url_data("https://sourceforge.net"));
}

```



## PowerShell


```powershell

$wc = New-Object Net.WebClient
$wc.DownloadString('https://sourceforge.net')

```


If the certificate could not be validated (untrusted, self-signed, expired), then an Exception is thrown with the message ''“The underlying connection was closed: Could not establish trust relationship for the SSL/TLS secure channel.”'' so certificate validation is done automatically by the method.


## Python

Python's '''urllib.request''' library, ('''urllib2''' in Python2.x) has support for SSL if the interpreter's underlying ''httplib'' libraries were compiled with SSL support.  By default this will be the enabled for default Python installations on most platforms.

Python 3.x:

```Python

from urllib.request import urlopen
print(urlopen('https://sourceforge.net/').read())

```


(Python 2.x)

```Python

from urllib2 import urlopen
print urlopen('https://sourceforge.net/').read()

```



{{libheader|Requests}}
{{works with|Python|2.7, 3.4–3.7}}
From the Requests documentation: ''"Requests verifies SSL certificates for HTTPS requests, just like a web browser. By default, SSL verification is enabled, and Requests will throw a SSLError if it’s unable to verify the certificate"''

```Python

import requests
print(requests.get("https://sourceforge.net").text)

```



## R

{{libheader|RCurl}}
{{libheader|XML}}

The basic idea is to use getURL (as with [[HTTP_Request]]), but with some extra parameters.

```R
library(RCurl)
webpage <- getURL("https://sourceforge.net/", .opts=list(followlocation=TRUE, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE))
```

In this case, the webpage output contains unprocessed characters, e.g. \" instead of " and \\ instead of \, so we need to process the markup.


```R

wp <- readLines(tc <- textConnection(webpage))
close(tc)

```


Finally, we parse the HTML and find the interesting bit.


```R

pagetree <- htmlTreeParse(wp)
pagetree$children$html

```



## Racket


```Racket

#lang racket
(require net/url)
(copy-port (get-pure-port (string->url "https://www.google.com")
                          #:redirections 100)
           (current-output-port))

```



## REALbasic

REALBasic provides an HTTPSecureSocket class for handling HTTPS connections. The 'Get' method of the HTTPSecureSocket is overloaded and can download data to a file or return data as a string, in both cases an optional timeout argument can be passed.


```REALbasic

      Dim sock As New HTTPSecureSocket
      Print(sock.Get("https://sourceforge.net", 10))  //set the timeout period to 10 seconds.

```



## Ring


```ring

cStr= download("http://sourceforge.net/")
see cStr + nl

```



## RLaB

See [[HTTP#RLaB]]


## Ruby

This solution doesn't use the <code>open-uri</code> convenience package that the [[HTTP Request#Ruby]] solution uses: the <code>Net::HTTP</code> object must be told to use SSL before the session is started.


```ruby

require 'net/https'
require 'uri'
require 'pp'

uri = URI.parse('https://sourceforge.net')
http = Net::HTTP.new(uri.host,uri.port)
http.use_ssl = true
http.verify_mode = OpenSSL::SSL::VERIFY_NONE

http.start do
  content = http.get(uri)
  p [content.code, content.message]
  pp content.to_hash
  puts content.body
end

```


outputs

```txt

["302", "Found"]
{"location"=>["http://sourceforge.net/"],
 "content-type"=>["text/html; charset=UTF-8"],
 "connection"=>["close"],
 "server"=>["nginx/0.7.60"],
 "date"=>["Sun, 30 Aug 2009 20:20:07 GMT"],
 "content-length"=>["229"],
 "set-cookie"=>
  ["sf.consume=89f65c6fadd222338b2f3de6f8e8a17b2c8f67c2gAJ9cQEoVQhfZXhwaXJlc3ECY2RhdGV0aW1lCmRhdGV0aW1lCnEDVQoH9gETAw4HAAAAhVJxBFUDX2lkcQVVIDEyOWI2MmVkOWMwMWYxYWZiYzE5Y2JhYzcwZDMxYTE4cQZVDl9hY2Nlc3NlZF90aW1lcQdHQdKmt73UN21VDl9jcmVhdGlvbl90aW1lcQhHQdKmt73UN2V1Lg==; expires=Tue, 19-Jan-2038 03:14:07 GMT; Path=/"]}
<html>
 <head>
  <title>302 Found</title>
 </head>
 <body>
  <h1>302 Found</h1>
  The resource was found at <a href="http://sourceforge.net/">http://sourceforge.net/</a>;
you should be redirected automatically.


 </body>
</html>

```



## Scala

{{libheader|Scala}}

```scala
import scala.io.Source

object HttpsTest extends App {
  System.setProperty("http.agent", "*")

  Source.fromURL("https://sourceforge.net").getLines.foreach(println)
}
```



## Seed7

The library [http://seed7.sourceforge.net/libraries/gethttps.htm gethttps.s7i] defines the function
[http://seed7.sourceforge.net/libraries/gethttps.htm#getHttps%28in_string%29 getHttps] which uses the
HTTPS protocol go get a file.


```seed7
$ include "seed7_05.s7i";
  include "gethttps.s7i";
  include "utf8.s7i";

const proc: main is func
  begin
    writeln(STD_UTF8_OUT, getHttps("sourceforge.net"));
  end func;
```



## Sidef


```ruby
var lwp = require('LWP::UserAgent');    # LWP::Protocol::https is needed
var url = 'https://rosettacode.org';

var ua = lwp.new(
    agent    => 'Mozilla/5.0',
    ssl_opts => Hash.new(verify_hostname => 1),
);

var resp = ua.get(url);
resp.is_success || die "Failed to GET #{url.dump}: #{resp.status_line}";
print resp.decoded_content;
```



## Swift


```Swift
import Foundation

// With https
let request = NSURLRequest(URL: NSURL(string: "https://sourceforge.net")!)

NSURLConnection.sendAsynchronousRequest(request, queue: NSOperationQueue()) {res, data, err in // callback

    // data is binary
    if (data != nil) {
        let string = NSString(data: data!, encoding: NSUTF8StringEncoding)
        println(string)
    }
}

CFRunLoopRun() // dispatch
```



## Tcl

Though Tcl's built-in <code>http</code> package does not understand SSL, it does support the registration of external handlers to accommodate additional protocols. This allows the use of the [http://tls.sourceforge.net/ Tls] package to supply the missing functionality with only a single line to complete the registration.


```tcl

package require http
package require tls

# Tell the http package what to do with “https:” URLs.
#
# First argument is the protocol name, second the default port, and
# third the connection builder command
http::register "https" 443 ::tls::socket

# Make a secure connection, which is almost identical to normal
# connections except for the different protocol in the URL.
set token [http::geturl "https://sourceforge.net/"]

# Now as for conventional use of the “http” package
puts [http::data $token]
http::cleanup $token

```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
SET DATEN = REQUEST ("https://sourceforge.net")
*{daten}

```



## UNIX Shell


```bash

curl -k -s -L https://sourceforge.net/

```



## VBScript

{{Libheader|Microsoft.XmlHTTP}}

Based on code at [http://itknowledgeexchange.techtarget.com/vbscript-systems-administrator/how-to-retrieve-html-web-pages-with-vbscript-via-the-microsoftxmlhttp-object/ How to retrieve HTML web pages with VBScript via the Microsoft.XmlHttp object]

```vb

Option Explicit

Const sURL="https://sourceforge.net/"

Dim oHTTP
Set oHTTP = CreateObject("Microsoft.XmlHTTP")

On Error Resume Next
oHTTP.Open "GET", sURL, False
oHTTP.Send ""
If Err.Number = 0 Then
     WScript.Echo oHTTP.responseText
Else
     Wscript.Echo "error " & Err.Number & ": " & Err.Description
End If

Set oHTTP = Nothing

```



## Visual Basic

{{Libheader|Microsoft.WinHttp}}
{{works with|Visual Basic|5}}
{{works with|Visual Basic|6}}
{{works with|VBA|Access 97}}
{{works with|VBA|6.5}}
{{works with|VBA|7.1}}

```vb
Sub Main()
Dim HttpReq As WinHttp.WinHttpRequest
'  in the "references" dialog of the IDE, check
'  "Microsoft WinHTTP Services, version 5.1" (winhttp.dll)
Const HTTPREQUEST_PROXYSETTING_PROXY As Long = 2
#Const USE_PROXY = 1
  Set HttpReq = New WinHttp.WinHttpRequest
  HttpReq.Open "GET", "https://groups.google.com/robots.txt"
#If USE_PROXY Then
  HttpReq.SetProxy HTTPREQUEST_PROXYSETTING_PROXY, "my_proxy:80"
#End If
  HttpReq.SetTimeouts 1000, 1000, 1000, 1000
  HttpReq.Send
  Debug.Print HttpReq.ResponseText
End Sub
```



## Visual Basic .NET


```vbnet

Imports System.Net

Dim client As WebClient = New WebClient()
Dim content As String = client.DownloadString("https://sourceforge.net")
Console.WriteLine(content)

```



## zkl

Using the cURL library to do the heavy lifting:

```zkl
zkl: var ZC=Import("zklCurl")
zkl: var data=ZC().get("https://sourceforge.net")
L(Data(36,265),826,0)
```

get returns the text of the response along with two counts: the bytes of header in front of the html code and the byte count of stuff after the end of the page. So, if you wanted to look at the header:

```txt
zkl: data[0][0,data[1]).text
HTTP/1.1 200 OK
Server: nginx
Date: Sun, 23 Mar 2014 07:36:51 GMT
Content-Type: text/html; charset=utf-8
Connection: close
...
```

or some of the html:

```txt
zkl: data[0][data[1],200).text
<!doctype html>
<!-- Server: sfs-consume-8 -->

<!--[if lt IE 7 ]> <html lang="en" class="no-js ie6"> <![endif]-->
<!--[if IE 7 ]>    <html lang="en" class="no-js ie7"> <![endif]-->
<!--[if IE 8 ]>
```




{{omit from|Applesoft BASIC|No TCP/IP network support on Apple II}}
{{omit from|Brainfuck}}
{{omit from|Commodore BASIC|Does not have network access}}
{{omit from|Inform 7|Does not have network access.}}
{{omit from|Integer BASIC|No TCP/IP network support on Apple II}}
{{omit from|Locomotive Basic|Does not have network access.}}
{{omit from|Lotus 123 Macro Scripting}}
{{omit from|M4}}
{{omit from|Maxima}}
{{omit from|ML/I}}
{{omit from|Openscad}}
{{omit from|PARI/GP}}
{{omit from|PostScript}}
{{omit from|Retro|Does not have network access.}}
{{omit from|SQL PL|Does not have network access}}
{{omit from|TI-83 BASIC|Does not have network access.}}
{{omit from|TI-89 BASIC|Does not have network access.}}
{{omit from|Unlambda|Does not have network access.}}
{{omit from|Yorick|Does not have network access.}}
{{omit from|ZX Spectrum Basic|Does not have network access.}}
