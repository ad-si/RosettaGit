+++
title = "HTTPS/Authenticated"
description = ""
date = 2019-04-08T18:25:37Z
aliases = []
[extra]
id = 2074
[taxonomies]
categories = []
tags = []
+++

{{task|Networking and Web Interaction}}

The goal of this task is to demonstrate [[HTTPS request]]s with authentication.
Implementations of this task should not use client certificates for this: that is the subject of [[Client-Authenticated HTTPS Request|another task]].

## AutoHotkey

{{libheader|iweb}}
{{libheader|COM}}

```AutoHotkey
iWeb_Init()
pwb := iWeb_newGui(0, 0, 1000, 800)
iWeb_nav(pwb, "http://www.facebook.com/login.php?ref=pf")
iWeb_Term()
iWeb_complete(pwb)
inputbox, email, email
inputbox, pass, password
iWeb_setDomObj(pwb,"Email",email)
iWeb_setDomObj(pwb,"pass",pass)
iWeb_clickDomObj(pwb, "login")
return

#Include iweb.ahk
#Include COM.ahk
#Include COMinvokeDeep.ahk
```


## C

{{libheader|libcurl}}

```c
#include <stdio.h>
#include <stdlib.h>
#include "curl/curl.h"

int
main(void)
{
        CURL *curl;
        char buffer[CURL_ERROR_SIZE];

        if ((curl = curl_easy_init()) != NULL) {
                curl_easy_setopt(curl, CURLOPT_URL, "https://user:password@secure.example.com/");
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

        // credentials of current user:
        client.Credentials = CredentialCache.DefaultCredentials;
        // or specify credentials manually:
        client.Credentials = new NetworkCredential("User", "Password");

        var data = client.DownloadString("https://example.com");

        Console.WriteLine(data);
    }
}

```


## Clojure

{{libheader|clj-http}}


```clojure
(clj-http.client/get "https://somedomain.com"
                     {:basic-auth ["user" "pass"]})
```



## Delphi


```Delphi
program ShowHTTPSAuthenticated;

{$APPTYPE CONSOLE}

uses IdHttp, IdSSLOpenSSL;

var
  s: string;
  lHTTP: TIdHTTP;
  lIOHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  ReportMemoryLeaksOnShutdown := True;
  lHTTP := TIdHTTP.Create(nil);
  lIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    lHTTP.Request.Username := 'USERNAME';
    lHTTP.Request.Password := 'PASSWD';
    lHTTP.IOHandler := lIOHandler;
    lHTTP.HandleRedirects := True;
    s := lHTTP.Get('https://SomeSecureSite.net/');
    Writeln(s);
  finally
    lHTTP.Free;
    lIOHandler.Free;
  end;
end.
```



## Go

The task solution is really the client program, but to test it I wrote a server and created a custom certificate.  I won't describe the certificate, but this is the server:

```go
package main

import (
    "encoding/base64"
    "io"
    "log"
    "net/http"
    "strings"
)

const userPass = "rosetta:code"
const unauth = http.StatusUnauthorized

func hw(w http.ResponseWriter, req *http.Request) {
    auth := req.Header.Get("Authorization")
    if !strings.HasPrefix(auth, "Basic ") {
        log.Print("Invalid authorization:", auth)
        http.Error(w, http.StatusText(unauth), unauth)
        return
    }
    up, err := base64.StdEncoding.DecodeString(auth[6:])
    if err != nil {
        log.Print("authorization decode error:", err)
        http.Error(w, http.StatusText(unauth), unauth)
        return
    }
    if string(up) != userPass {
        log.Print("invalid username:password:", string(up))
        http.Error(w, http.StatusText(unauth), unauth)
        return
    }
    io.WriteString(w, "Goodbye, World!")
}

func main() {
    http.HandleFunc("/", hw)
    log.Fatal(http.ListenAndServeTLS(":8080", "cert.pem", "key.pem", nil))
}
```

It is a "Hello world" server, but over TLS and with basic authentication required on the Get.  Errors are logged to aid client debugging.

The client:

```go
package main

import (
    "crypto/tls"
    "crypto/x509"
    "fmt"
    "io/ioutil"
    "log"
    "net/http"
)

const (
    userid   = "rosetta"
    password = "code"
)

func main() {
    // Use custom certificate for testing.  Not exactly required by task.
    b, err := ioutil.ReadFile("cert.pem")
    if err != nil {
        log.Fatal(err)
    }
    pool := x509.NewCertPool()
    if ok := pool.AppendCertsFromPEM(b); !ok {
        log.Fatal("Failed to append cert")
    }
    tc := &tls.Config{RootCAs: pool}
    tr := &http.Transport{TLSClientConfig: tc}
    client := &http.Client{Transport: tr}
    req, err := http.NewRequest("GET", "https://127.0.0.1:8080", nil)
    if err != nil {
        log.Fatal(err)
    }

    // This one line implements the authentication required for the task.
    req.SetBasicAuth(userid, password)

    // Make request and show output.
    resp, err := client.Do(req)
    if err != nil {
        log.Fatal(err)
    }
    b, err = ioutil.ReadAll(resp.Body)
    resp.Body.Close()
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(string(b))
}
```



## Haskell


Example uses the [https://hackage.haskell.org/package/req <tt>req</tt>] and [https://hackage.haskell.org/package/aeson <tt>aeson</tt>] packages:


```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Aeson (Value)
import           Data.Default.Class (def)
import           Network.HTTP.Req
                    ( (/:)
                    , GET(..)
                    , NoReqBody(..)
                    , basicAuth
                    , https
                    , jsonResponse
                    , req
                    , responseBody
                    , runReq
                    )

main :: IO ()
main = do
    response <- runReq def $ req
            GET
            (https "httpbin.org" /: "basic-auth" /: "someuser" /: "somepassword")
            NoReqBody
            jsonResponse
            (basicAuth "someuser" "somepassword")
    print (responseBody response :: Value)
```



## Julia

An example using HTTP (see the source for HTTP.jl for the code below ) to access and play a song:

```Julia

using HTTP, HTTP.IOExtras, JSON, MusicProcessing
HTTP.open("POST", "http://music.com/play") do io
    write(io, JSON.json([
        "auth" => "12345XXXX",
        "song_id" => 7,
    ]))
    r = startread(io)
    @show r.status
    while !eof(io)
        bytes = readavailable(io)
        play(bytes)
    end
end

```




## Kotlin


```scala
// version 1.2.0

import java.net.Authenticator
import java.net.PasswordAuthentication
import javax.net.ssl.HttpsURLConnection
import java.net.URL
import java.io.InputStreamReader
import java.io.BufferedReader

object PasswordAuthenticator : Authenticator() {
    override fun getPasswordAuthentication() =
        PasswordAuthentication ("username", "password".toCharArray())
}

fun main(args: Array<String>) {
    val url = URL("https://somehost.com")
    val con = url.openConnection() as HttpsURLConnection
    Authenticator.setDefault(PasswordAuthenticator)
    con.allowUserInteraction = true
    con.connect()
    val isr = InputStreamReader(con.inputStream)
    val br = BufferedReader(isr)
    while (true) {
        val line = br.readLine()
        if (line == null) break
        println(line)
    }
}
```



## Lasso


```Lasso
local(username = 'hello',password = 'world')
local(x = curl('https://sourceforge.net'))
#x->set(CURLOPT_USERPWD, #username + ':' + #password)
local(y = #x->result)
#y->asString
```



## LiveCode

HTTP Basic Auth as part of url

```LiveCode
command getAuthWebResource
    libURLFollowHttpRedirects true
    libURLSetSSLVerification true
    put URL "https://user:passwd@example.basicauth.com/" into response
    put response
end getAuthWebResource
```


You can also set the headers for the basic auth requests

```LiveCode
command getAuthWebResource
    libURLFollowHttpRedirects true
    libURLSetSSLVerification true
    set the httpHeaders to "Authorization: Basic " && base64Encode("user:passwd")
    put URL "https://example.basicauth.com" into response
    put response
end getAuthWebResource
```



## Lua

{{works with|Lua|5.1 - 5.3}}
{{libheader|lua-requests}}


```lua

local requests = require('requests')
local auth = requests.HTTPBasicAuth('admin', 'admin')
local resp, e = requests.get({
  url = 'https://httpbin.org/basic-auth/admin/admin',
  auth = auth
})
io.write(string.format('Status: %d', resp.status_code))

```


{{out}}

```txt

Status: 200

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
a = RunThrough["curl -u JohnDoe:Password https://www.example.com", 1]
For[ i=0, i < Length[a] , i++, SomeFunction[a]]
```



## Perl

{{libheader|LWP}}

```perl
use LWP::UserAgent qw();
my $ua = LWP::UserAgent->new;
my $netloc = 'http://www.buddhism-dict.net/cgi-bin/xpr-dealt.pl:80';
$ua->credentials(
   $netloc,
   'CJK-E and Buddhist Dictionaries', # basic realm
   'guest',  # user
   '',       # empty pw
);
my $response = $ua->get($netloc);

use WWW::Mechanize qw();
my $mech = WWW::Mechanize->new;
$mech->get('https://login.yahoo.com/');
$mech->submit_form(with_fields => {
    login         => 'XXXXXX',
    passwd        => 'YYYYYY',
    '.persistent' => 'y',  # tick checkbox
});
```



## Perl 6

{{works with|Rakudo|2017.09}}
Used here to connect to my local wireless router to a page that is password protected. Obviously not going to be generally publicly accessible but should be easily adaptable to other sites / devices.


```perl6
use HTTP::UserAgent;

my $username = 'username'; # my username
my $password = 'password'; # my password
my $address  = 'http://192.168.1.1/Status_Router.asp'; # my local wireless router

my $ua = HTTP::UserAgent.new;
$ua.auth( $username, $password );
my $response = $ua.get: $address;
say $response.is-success ?? $response.content !! $response.status-line;
```



## Phix

{{libheader|libcurl}}
Exactly the same as the [[HTTP#Phix]] task.
You can of course use curl_easy_setopt(curl,CURLOPT_USERPWD,"user:password") rather than embed that in the url.

```Phix
include builtins\libcurl.e
curl_global_init()
atom curl = curl_easy_init()
curl_easy_setopt(curl, CURLOPT_URL, "https://user:password@example.com/")
object res = curl_easy_perform_ex(curl)
curl_easy_cleanup(curl)
curl_global_cleanup()

puts(1,res)
```



## PicoLisp


```PicoLisp
(let (User "Bill"  Pass "T0p5ecRet"  Url "https://www.example.com")
   (in (list 'curl "-u" (pack User ': Pass) Url)
      (while (line)
         (doSomeProcessingWithLine @) ) ) )
```



## Python

{{works with|Python|2.4 and 2.6}}

'''Note:''' You should install '''''mechanize''''' to run code below. Visit: http://wwwsearch.sourceforge.net/mechanize/


```python
#!/usr/bin/python
# -*- coding: utf-8 -*-

from mechanize import Browser

USER_AGENT = "Mozilla/5.0 (X11; U; Linux i686; tr-TR; rv:1.8.1.9) Gecko/20071102 Pardus/2007 Firefox/2.0.0.9"

br = Browser()
br.addheaders = [("User-agent", USER_AGENT)]

# remove comment if you get debug output
# br.set_debug_redirects(True)
# br.set_debug_responses(True)
# br.set_debug_http(True)

br.open("https://www.facebook.com")

br.select_form("loginform")
br['email'] = "xxxxxxx@xxxxx.com"
br['pass'] = "xxxxxxxxx"
br['persistent'] = ["1"]

response = br.submit()
print response.read()
```


{{libheader|Requests}}
{{works with|Python|2.7, 3.4–3.7}}

```python
import requests

username = "user"
password = "pass"
url = "https://www.example.com"

response = requests.get(url, auth=(username, password)

print(response.text)
```



## Racket


```racket

#lang racket

(require net/url net/url-connect openssl)

(module+ main
  (parameterize ([current-https-protocol (ssl-make-client-context)])
    (ssl-set-verify! (current-https-protocol) #t)

    ;; When this is #f, we correctly get an exception:
    ;; error:14090086:SSL routines:SSL3_GET_SERVER_CERTIFICATE:certificate verify failed
    (when #t
      (ssl-load-verify-source! (current-https-protocol)
                               '(directory
                                 ;; This location works on Debian 6;
                                 ;; adjust as needed for your platform.
                                 "/etc/ssl/certs"
                                 )))

    (for ([l (in-port read-line (get-pure-port (string->url "https://www.google.com/")))])
      (displayln  l))))

```



## Ruby


```ruby
require 'uri'
require 'net/http'

uri = URI.parse('https://www.example.com')
response = Net::HTTP.start(uri.host, uri.port, use_ssl: true) do |http|
  request = Net::HTTP::Get.new uri
  request.basic_auth('username', 'password')
  http.request request
end
```



## Run BASIC


```runbasic
html "
<CENTER><TABLE CELLPADDING=0 CELLSPACING=0 border=1 bgcolor=wheat>
<TR><TD colspan=2 bgcolor=tan align=center>LOGIN</TD></TR>
<TR><TD align=right>UserName</TD><TD>"
 TEXTBOX #userName, ""
html "</TR></TD><TR><TD align=right>Password:</TD><TD>"
PasswordBox  #passWord, ""

html "</TD></TR><TD align=center colspan=2>"
button #si, "Signin", [doSignin]
html "   "
button #ex, "Exit", [exit]
html "</TD></TR></TABLE>"
WAIT

[doSignin]
loginUserName$    = trim$(#userName contents$())
loginPassWord$    = trim$(#passWord contents$())
if (loginUserName$ = "admin" and loginPassWord$ = "admin" then
   print "Login ok"
  else
   print "invalid User or Pass"
   cls
   goto [loop]
end if

print Platform$    ' OS where Run BASIC is being hosted
print UserInfo$    ' Information about the user's web browser
print UserAddress$ ' IP address of the user

[exit]
end
```

[[File:ClientAuthorizationRunBasic.png]]


## Scala


```Scala
import java.net.{Authenticator, PasswordAuthentication, URL}

import javax.net.ssl.HttpsURLConnection

import scala.io.BufferedSource


object Authenticated extends App {

  val con: HttpsURLConnection =
    new URL("https://somehost.com").openConnection().asInstanceOf[HttpsURLConnection]

  object PasswordAuthenticator extends Authenticator {
    override def getPasswordAuthentication =
      new PasswordAuthentication("username", "password".toCharArray)
  }

  Authenticator.setDefault(PasswordAuthenticator)
  con.setAllowUserInteraction(true)
  con.connect()

  new BufferedSource(con.getInputStream).getLines.foreach(println(_))

}
```


## Sidef


```ruby
require('WWW::Mechanize')

var mech = %s'WWW::Mechanize'.new(
    cookie_jar => Hash.new,
    agent => 'Mozilla/5.0',
)

mech.get('https://login.yahoo.com/')
mech.submit_form(
    form_id => 'mbr-login-form',   # form id
    fields => Hash.new(
        'login'  => 'XXXXXX',
        'passwd' => 'YYYYYY',
))
```



## Tcl

{{works with|Tcl|8.6}} for the <code>binary encode</code> subcommand, otherwise uses
{{tcllib|base64}}
Uses the [http://tls.sourceforge.net Tls] package.

```Tcl
package require http
package require tls
http::register https 443 ::tls::socket

# Generate the authentication
set user theUser
set pass thePassword
dict set auth Authenticate "Basic [binary encode base64 ${user}:${pass}]"

# Make a secure authenticated connection
set token [http::geturl https://secure.example.com/ -headers $auth]

# Now as for conventional use of the “http” package
set data [http::data $token]
http::cleanup $token
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
  HttpReq.Open "GET", "https://www.abc.com/xyz/index.html"
  HttpReq.SetCredentials "<username>", "<password>", 0&
#If USE_PROXY Then
  HttpReq.SetProxy HTTPREQUEST_PROXYSETTING_PROXY, "10.167.1.1:80"
#End If
  HttpReq.SetTimeouts 1000, 1000, 1000, 1000
  HttpReq.Send
  Debug.Print HttpReq.ResponseText
End Sub
```



## zkl

Using cURL to do the heavy lifting, get a XML list of computers connected to my router.

```zkl
zkl: var ZC=Import("zklCurl")
zkl: var data=ZC().get("http://usr:pw@192.168.1.1/computer_list.xml")
L(Data(1,049),121,0)
zkl: data[0][121,*].text
```

{{out}}

```txt

<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<computers>
<ip_address0>192.168.1.100</ip_address0><host_name0>core-shot
...

```


{{omit from|Batch File|Does not have network access.}}
{{omit from|Brainfuck}}
{{omit from|Commodore BASIC|Does not have network access}}
{{omit from|Inform 7|Does not have network access.}}
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
