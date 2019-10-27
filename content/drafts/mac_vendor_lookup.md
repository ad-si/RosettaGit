+++
title = "MAC Vendor Lookup"
description = ""
date = 2019-08-29T04:13:20Z
aliases = []
[extra]
id = 21272
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[Category:Network Tools]]

Every connected device around the world comes with a unique Media Access Control address, or a  [https://en.wikipedia.org/wiki/MAC_address MAC address]. A common task a network administrator may come across is being able to identify a network device's manufacturer when given only a MAC address.

;Basic Task:
The task is to interface with one (or numerous) APIs that exist on the internet and retrieve the device manufacturer based on a supplied MAC address.

A MAC address that does not return a valid result should return the String "N/A". A error related to the network connectivity or the API should return a null result.


## AutoHotkey


```AutoHotkey
macLookup(MAC){
	WebRequest := ComObjCreate("WinHttp.WinHttpRequest.5.1")
	WebRequest.Open("GET", "http://api.macvendors.com/" MAC)
	WebRequest.Send()
	return WebRequest.ResponseText
}
```

Examples:
```AutoHotkey
MsgBox % macLookup("00-14-22-01-23-45")
```

Outputs:
```txt
Dell Inc.
```



## BaCon

This code requires BaCon 3.8.2 or higher.

```qbasic
OPTION TLS TRUE
website$ = "api.macvendors.com"
mac$ = "b0:52:16:d0:3c:fb"
OPEN website$ & ":443" FOR NETWORK AS mynet
SEND "GET /" & mac$ & " HTTP/1.1\r\nHost: " & website$ & "\r\n\r\n" TO mynet
RECEIVE info$ FROM mynet
CLOSE NETWORK mynet
PRINT TOKEN$(info$, 2, "\r\n\r\n")
```

{{out}}

```txt
Hon Hai Precision Ind. Co.,Ltd.
```



## C

Takes MAC address as input, prints usage on incorrect invocation, requires [https://curl.haxx.se/libcurl/ libcurl]

```C

#include <curl/curl.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

/* Length of http://api.macvendors.com/ */
#define FIXED_LENGTH 16

struct MemoryStruct {
  char *memory;
  size_t size;
};
 
static size_t WriteMemoryCallback(void *contents, size_t size, size_t nmemb, void *userp)
{
  size_t realsize = size * nmemb;
  struct MemoryStruct *mem = (struct MemoryStruct *)userp;
 
  mem->memory = realloc(mem->memory, mem->size + realsize + 1);
 
  memcpy(&(mem->memory[mem->size]), contents, realsize);
  mem->size += realsize;
  mem->memory[mem->size] = 0;
 
  return realsize;
}

void checkResponse(char* str){
	char ref[] = "Vendor not found";
	int len = strlen(str),flag = 1,i;
	
	if(len<16)
		fputs(str,stdout);
	else{
		for(i=0;i<len && i<16;i++)
			flag = flag && (ref[i]==str[i]);
		
		flag==1?fputs("N/A",stdout):fputs(str,stdout);
	}
}

int main(int argC,char* argV[])
{
		if(argC!=2)
			printf("Usage : %s <MAC address>",argV[0]);
		else{
			CURL *curl;
			int len = strlen(argV[1]);
			char* str = (char*)malloc((FIXED_LENGTH + len)*sizeof(char));
			struct MemoryStruct chunk;
			CURLcode res;
 
			chunk.memory = malloc(1);
			chunk.size = 0;  
 
        if ((curl = curl_easy_init()) != NULL) {
				sprintf(str,"http://api.macvendors.com/%s",argV[1]);

                                curl_easy_setopt(curl, CURLOPT_URL, str);
				curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);
				curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&chunk);
				
				free(str);
				
				res = curl_easy_perform(curl);
				
                if (res == CURLE_OK) {
				checkResponse(chunk.memory);
                                return EXIT_SUCCESS;
                }
				
                curl_easy_cleanup(curl);
			}
		}
        
        return EXIT_FAILURE;
}

```

Invocation and output :

```txt

C:\rosettaCode>macLookUp 00-11-22-33-44-55-66
CIMSYS Inc
C:\rosettaCode>macLookUp 10-11-22-33-44-55-66
N/A

```


=={{header|C sharp|C#}}==


```csharp
using System;
using System.Net;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task<string> LookupMac(string MacAddress)
    {
        var uri = new Uri("http://api.macvendors.com/" + WebUtility.UrlEncode(MacAddress));
        using (var wc = new HttpClient())
            return await wc.GetStringAsync(uri);
    }
    static void Main(string[] args)
    {
        foreach (var mac in new string[] { "88:53:2E:67:07:BE", "FC:FB:FB:01:FA:21", "D4:F4:6F:C9:EF:8D" })
            Console.WriteLine(mac + "\t" + LookupMac(mac).Result);
        Console.ReadLine();
    }
}
```


{{out}}


```txt

88:53:2E:67:07:BE       Intel Corporate
FC:FB:FB:01:FA:21       Cisco Systems, Inc
D4:F4:6F:C9:EF:8D       Apple, Inc.

```



## Common Lisp


```Common Lisp
(quicklisp:quickload :Drakma)            ; or load it in another way

(defun mac-vendor (mac)
  (check-type mac string "A MAC address as a string")
  (multiple-value-bind (vendor status)
    (drakma:http-request (format nil "http://api.macvendors.com/~a" mac))
    (if (= 200 status)
      (format t "~%Vendor is ~a" vendor)
      (error "~%Not a MAC address: ~a" mac))))

```




## Free Pascal


```pascal
program MACVendorLookup;

uses
  fphttpclient;

var
  res: String;
begin
  if paramCount > 0 then begin

    With TFPHttpClient.Create(Nil) do
    try
      allowRedirect := true;
      try
        res := Get('http://api.macvendors.com/' + ParamStr(1));
        writeLn(res);
      except
        writeLn('N/A');
      end;
    finally
      Free;
    end;
  end;
end.
```


{{out}}

```txt

./MACVendorLookup 10-11-22-33-44-55-66
N/A

./MACVendorLookup 00-11-22-33-44-55-66
CIMSYS Inc

```



## Go


```go
package main

import (
	"net/http"
	"fmt"
	"io/ioutil"
)

func macLookUp(mac string) (res string){
	resp, _ := http.Get("http://api.macvendors.com/" + mac)
	body, _ := ioutil.ReadAll(resp.Body)
	res = string(body)
	return
}

func main()  {
	fmt.Println(macLookUp("FC-A1-3E"))
	fmt.Println(macLookUp("FC:FB:FB:01:FA:21"))
	fmt.Println(macLookUp("BC:5F:F4"))
}

```

{{Out}}

```txt
Samsung Electronics Co.,Ltd
Cisco Systems, Inc
ASRock Incorporation

```



## Haskell

{{Works with|GHC|8.0.2}}
{{libheader|http-client}}


```haskell
#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package bytestring
  --package http-client
  --package http-types
-}

{-# LANGUAGE MultiWayIf #-}

import Control.Exception (try)
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString, unpack)
import Network.HTTP.Client
  (Manager, parseRequest, httpLbs, responseStatus, responseBody,
   newManager, defaultManagerSettings, Response, HttpException)
import Network.HTTP.Types.Status (statusIsSuccessful, notFound404)
import System.Environment (getArgs)
import Text.Printf (printf)

fetchURL :: Manager
         -> String
         -> IO (Either HttpException (Response L8.ByteString))
fetchURL mgr url = try $ do
  req <- parseRequest url
  httpLbs req mgr

lookupMac :: Manager -> String -> IO String
lookupMac mgr mac = do
  eth <- fetchURL mgr $ "http://api.macvendors.com/" ++ mac
  return $ case eth of
             Left _ -> "null"
             Right resp -> let body = responseBody resp
                               status = responseStatus resp
                           in if | status == notFound404 -> "N/A"
                                 | not (statusIsSuccessful status) -> "null"
                                 | otherwise -> L8.unpack body

main :: IO ()
main = do
  args <- getArgs
  mgr <- newManager defaultManagerSettings
  forM_ args $ \mac -> do
    putStr $ printf "%-17s" mac ++ " = "
    vendor <- lookupMac mgr mac
    putStrLn vendor
```


{{Out}}

```txt
$ ./RosettaMac.hs 00:15:ed:f0:00:00 ff:ff:ff:ff:ff:ff 88:53:2E:67:07:BE FC:FB:FB:01:FA:21 D4:F4:6F:C9:EF:8D banana
00:15:ed:f0:00:00 = Fulcrum Microsystems, Inc.
ff:ff:ff:ff:ff:ff = N/A
88:53:2E:67:07:BE = Intel Corporate
FC:FB:FB:01:FA:21 = Cisco Systems, Inc
D4:F4:6F:C9:EF:8D = Apple, Inc.
banana            = N/A
```



## J

'''Solution

```j
require 'web/gethttp'
lookupMACvendor=: [: gethttp 'http://api.macvendors.com/'&,
```

'''Example Usage

```j
   addr=: '88:53:2E:67:07:BE';'FC:FB:FB:01:FA:21';'D4:F4:6F:C9:EF:8D';'23:45:67'
   (,&'   ' , lookupMACvendor)&> addr
88:53:2E:67:07:BE   Intel Corporate   
FC:FB:FB:01:FA:21   Cisco Systems, Inc
D4:F4:6F:C9:EF:8D   Apple, Inc.       
23:45:67   Vendor not found
```



## Java


```java
package com.jamesdonnell.MACVendor;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

/** MAC Vendor Lookup class.
 * www.JamesDonnell.com
 * @author James A. Donnell Jr. */
public class Lookup {
	/** Base URL for API. The API from www.macvendors.com was chosen. */
	private static final String baseURL = "http://api.macvendors.com/";

	/** Performs lookup on MAC address(es) supplied in arguments.
	 * @param args MAC address(es) to lookup. */
	public static void main(String[] args) {
		for (String arguments : args)
			System.out.println(arguments + ": " + get(arguments));
	}

	/** Performs lookup on supplied MAC address.
	 * @param macAddress MAC address to lookup.
	 * @return Manufacturer of MAC address. */
	private static String get(String macAddress) {
		try {
			StringBuilder result = new StringBuilder();
			URL url = new URL(baseURL + macAddress);
			HttpURLConnection conn = (HttpURLConnection) url.openConnection();
			conn.setRequestMethod("GET");
			BufferedReader rd = new BufferedReader(new InputStreamReader(conn.getInputStream()));
			String line;
			while ((line = rd.readLine()) != null) {
				result.append(line);
			}
			rd.close();
			return result.toString();
		} catch (FileNotFoundException e) {
			// MAC not found
			return "N/A";
		} catch (IOException e) {
			// Error during lookup, either network or API.
			return null;
		}
	}
}
```



## Julia


```julia
# v0.6.0

using Requests

function getvendor(addr::String)
    try
        get("http://api.macvendors.com/$addr") |> readstring
    catch e
        nothing
    end
end

for addr in ["88:53:2E:67:07:BE", "FC:FB:FB:01:FA:21", "D4:F4:6F:C9:EF:8D", "23:45:67"]
    println("$addr -> ", getvendor(addr))
end
```


{{out}}

```txt
88:53:2E:67:07:BE -> Intel Corporate
FC:FB:FB:01:FA:21 -> Cisco Systems, Inc
D4:F4:6F:C9:EF:8D -> Apple, Inc.
23:45:67 -> Vendor not found
```



## Kotlin


```scala
// version 1.1.2

import java.net.URL

fun lookupVendor(mac: String) = URL("http://api.macvendors.com/" + mac).readText()

fun main(args: Array<String>) {
    val macs = arrayOf("FC-A1-3E", "FC:FB:FB:01:FA:21", "88:53:2E:67:07:BE", "D4:F4:6F:C9:EF:8D")
    for (mac in macs) println(lookupVendor(mac))
}
```


{{out}}

```txt

Samsung Electronics Co.,Ltd
Cisco Systems, Inc
Intel Corporate
Apple, Inc.

```



## Lua


```lua
-- Requires LuaSocket extension by Lua
-- Created by James A. Donnell Jr.
-- www.JamesDonnell.com

local baseURL = "http://api.macvendors.com/"

local function lookup(macAddress)
	http = require "socket.http"
	result, statuscode, content = http.request(baseURL .. macAddress)
	return result
end

local macAddress = "FC-A1-3E-2A-1C-33"
print(lookup(macAddress))
```


## M2000 Interpreter


```M2000 Interpreter

Module Checkit {
      httpGet$=lambda$  (url$, timeout=500)->{
            declare htmldoc "WinHttp.WinHttpRequest.5.1"
            Method htmldoc "SetTimeouts", timeout, timeout, timeout, timeout
            Method htmldoc "open","GET", url$, false
            Method htmldoc "setRequestHeader","Content-Type", "application/x-www-form-urlencoded"
            Method htmldoc "send"
            With  htmldoc, "responseText" as ready$ 
            res$=trim$(ready$)
            if left$(res$,1)="{" then 
                  ="N/A"
            else
                   =res$
            end if
            declare htmldoc nothing
      }
       Urls=("88:53:2E:67:07:BE", "FC:FB:FB:01:FA:21", "D4:F4:6F:C9:EF:8D", "23:45:67")
       url=each(URLs)
       While Url {
             Print Array$(URL), httpGet$("http://api.macvendors.com/"+Array$(URL))
             Wait 20
      }
}
Checkit

```


## Mathematica


```Mathematica
macLookup[mac_String] := Quiet[Check[Import["http://api.macvendors.com/" <> mac], "N/A"]]
```


Examples:
```Mathematica
macLookup["00-14-22-01-23-45"]
```

Outputs:
```txt
Dell Inc.
```



## Nim



```Nim
import httpclient

for mac in ["FC-A1-3E", "FC:FB:FB:01:FA:21", "BC:5F:F4"]:
  echo newHttpClient().getContent("http://api.macvendors.com/"&mac)
```


{{out}}

```txt
Samsung Electronics Co.,Ltd
Cisco Systems, Inc
ASRock Incorporation

```



## Perl


```perl
#!/usr/bin/env perl -T
use 5.018_002;
use warnings;
use LWP;

our $VERSION = 1.000_000;

my $ua = LWP::UserAgent->new;

my @macs = (
    'FC-A1-3EFC:FB:FB:01:FA:21', '00,0d,4b',
    'Rhubarb',                   '00-14-22-01-23-45',
    '10:dd:b1',                  'D4:F4:6F:C9:EF:8D',
    'FC-A1-3E',                  '88:53:2E:67:07:BE',
    '23:45:67',                  'FC:FB:FB:01:FA:21',
    'BC:5F:F4',
);

for my $mac (@macs) {
    my $vendor = get_mac_vendor($mac);
    if ($vendor) {
        say "$mac = $vendor";
    }
}

sub get_mac_vendor {
    my $s = shift;

    my $req = HTTP::Request->new( GET => "http://api.macvendors.com/$s" );
    my $res = $ua->request($req);

    # A error related to the network connectivity or the API should
    # return a null result.
    if ( $res->is_error ) {
        return;
    }

    # A MAC address that does not return a valid result should
    # return the String "N/A".
    if (  !$res->content
        or $res->content eq 'Vendor not found' )
    {
        return 'N/A';
    }

    return $res->content;
}

# IEEE 802:
#  Six groups of two hexadecimal digits separated by hyphens or colons,
#    like 01-23-45-67-89-ab or 01:23:45:67:89:ab
#  Three groups of four hexadecimal digits separated by dots (.),
#    like 0123.4567.89ab
#sub validmac {
#    my $s = shift;
#
#    my $hex    = qr{ [A-Fa-f[:digit:]] }xms;
#    my $hex2ws = qr{ [-:] $hex{2} }xms;
#
#    if (   $s =~ m{\A $hex{2} $hex2ws{5} \z}xms
#        or $s =~ m{\A $hex{4} [.] $hex{4}  [.] $hex{4} \z}xms )
#    {
#        return 'true';
#    }
#    return;
#}
```


{{out}}

```txt
FC-A1-3EFC:FB:FB:01:FA:21 = Samsung Electronics Co.,Ltd
00,0d,4b = Roku, Inc.
00-14-22-01-23-45 = Dell Inc.
10:dd:b1 = Apple, Inc.
D4:F4:6F:C9:EF:8D = Apple, Inc.
FC-A1-3E = Samsung Electronics Co.,Ltd
88:53:2E:67:07:BE = Intel Corporate
FC:FB:FB:01:FA:21 = Cisco Systems, Inc
BC:5F:F4 = ASRock Incorporation
```



## Perl 6

{{works with|Rakudo|2018.03}}
Apparently there is some rate limiting on place now, sleep a bit between requests.


```perl6
use HTTP::UserAgent;
 
my $ua = HTTP::UserAgent.new;
 
$ua.timeout = 10; # seconds
 
my $server = 'http://api.macvendors.com/';
 
sub lookup ($mac) {
    my $response = $ua.get: "$server+$mac";
    sleep 1;
    return $response.is-success ?? $response.content !! 'N/A';
 
    CATCH {             # Normally you would report some information about what
        default { Nil } # went wrong, but the task specifies to ignore errors.
    }
}
 
for <
BC:5F:F4
FC-A1-3E
10:dd:b1
00:0d:4b
23:45:67
> -> $mac { say lookup $mac }
```

{{out}}

```txt
ASRock Incorporation
Samsung Electronics Co.,Ltd
Apple, Inc.
Roku, Inc.
N/A

```



## Phix


```Phix
string test = "00-11-22-33-44-55-66"     -- CIMSYS Inc
--string test = "10-11-22-33-44-55-66"      -- N/A
include builtins/libcurl.e
curl_global_init()
atom curl = curl_easy_init()
string url = sprintf("http://api.macvendors.com/%s",{test})
curl_easy_setopt(curl, CURLOPT_URL, url)
object res = curl_easy_perform_ex(curl)
if string(res) then
    if res="Vendor not found"
    or res=`{"errors":{"detail":"Not Found"}}` then
        res = "N/A"
    end if
    ?res
else
    ?{"error",res}
end if
curl_easy_cleanup(curl)
curl_global_cleanup()
```

{{out}}

```txt

CIMSYS Inc

```



## PicoLisp


```PicoLisp
(load "@lib/http.l")

(de maclookup (M)
   (client "api.macvendors.com" 80
      M
      (while (line))
      (line T) ) )
(test
   "Intel Corporate"
   (maclookup "88:53:2E:67:07:BE") )
(test
   "Apple, Inc."
   (maclookup "D4:F4:6F:C9:EF:8D") )
```



## Python


```python
import requests

for addr in ['88:53:2E:67:07:BE', 'FC:FB:FB:01:FA:21',
        'D4:F4:6F:C9:EF:8D', '23:45:67']:
    vendor = requests.get('http://api.macvendors.com/' + addr).text
    print(addr, vendor)
```

{{out}}

```txt
88:53:2E:67:07:BE Intel Corporate
FC:FB:FB:01:FA:21 Cisco Systems, Inc
D4:F4:6F:C9:EF:8D Apple, Inc.
23:45:67 Vendor not found

```



## Racket


```racket
#lang racket

(require net/url)

(define (lookup-MAC-address addr)
  (port->string
   (get-pure-port
    (url "http" #f "api.macvendors.com" #f #t (list (path/param addr null)) null #f))))

(module+ test
(for ((i (in-list '("88:53:2E:67:07:BE"
                    "FC:FB:FB:01:FA:21"
                    "D4:F4:6F:C9:EF:8D"))))
  (printf "~a\t~a~%" i (lookup-MAC-address i))))
```


{{out}}

```txt
88:53:2E:67:07:BE	Intel Corporate
FC:FB:FB:01:FA:21	Cisco Systems, Inc
D4:F4:6F:C9:EF:8D	Apple, Inc.
```



## Ring


```ring

# Project: MAC Vendor Lookup

load "stdlib.ring"
macs = ["FC-A1-3E","FC:FB:FB:01:FA:21","88:53:2E:67:07:BE","D4:F4:6F:C9:EF:8D"]
for mac = 1 to len(macs)
     lookupvendor(macs[mac])
next

func lookupvendor(mac)
       url = download("api.macvendors.com/" + mac)
       see url + nl

```

Output:

```txt

Samsung Electronics Co.,Ltd
Cisco Systems, Inc
Intel Corporate
Apple, Inc.

```



## Ruby


```ruby
require 'net/http'

arr = ['88:53:2E:67:07:BE', 'FC:FB:FB:01:FA:21', 'D4:F4:6F:C9:EF:8D', '23:45:67']

arr.each do |addr|
  vendor = Net::HTTP.get('api.macvendors.com', "/#{addr}/") rescue nil
  puts "#{addr}  #{vendor}" 
end
```

{{out}}

```txt
88:53:2E:67:07:BE  Intel Corporate
FC:FB:FB:01:FA:21  Cisco Systems, Inc
D4:F4:6F:C9:EF:8D  Apple, Inc.
23:45:67  Vendor not found

```




## Rust


```rust
extern crate reqwest;

use std::{thread, time};

fn get_vendor(mac: &str) -> Option<String> {
    let mut url = String::from("http://api.macvendors.com/");
    url.push_str(mac);
    let url_ref = &url;
    match reqwest::get(url_ref) {
        Ok(mut res) => match res.text() {
            Ok(text) => {
                if text.contains("Not Found") {
                    Some("N/A".to_string())
                } else {
                    Some(text)
                }
            }
            Err(e) => {
                println!("{:?}", e);
                None
            }
        },
        Err(e) => {
            println!("{:?}", e);
            None
        }
    }
}

fn main() {
    let duration = time::Duration::from_millis(1000);
    match get_vendor("88:53:2E:67:07:BE") {
        None => println!("Error!"),
        Some(text) => println!("{}", text),
    }
    thread::sleep(duration);
    match get_vendor("FC:FB:FB:01:FA:21") {
        None => println!("Error!"),
        Some(text) => println!("{}", text),
    }
    thread::sleep(duration);
    match get_vendor("FC-A1-3E") {
        None => println!("Error!"),
        Some(text) => println!("{}", text),
    }
    thread::sleep(duration);
    match get_vendor("abcdefg") {
        None => println!("Error!"),
        Some(text) => println!("{}", text),
    }
}

```

Output:

```txt

Intel Corporate
Cisco Systems, Inc
Samsung Electronics Co.,Ltd
N/A

```


## Scala


```scala
object LookUp extends App {
  val macs = Seq("FC-A1-3E", "FC:FB:FB:01:FA:21", "88:53:2E:67:07:BE", "D4:F4:6F:C9:EF:8D")

  def lookupVendor(mac: String) =
    scala.io.Source.fromURL("""http://api.macvendors.com/""" + mac, "UTF-8").mkString

  macs.foreach(mac => println(lookupVendor(mac)))
}
```


## Tcl


```Tcl
package require http

# finally is a bit like go's defer
proc finally args {
    tailcall trace add variable :#finally#: unset [list apply [list args $args]]
}

# basic wrapper for http::geturl
proc geturl {url} {
    set tok [::http::geturl $url]
    finally ::http::cleanup $tok
    ::http::data $tok
}   
proc maclookup {mac} {
    geturl http://api.macvendors.com/$mac
}

foreach mac {00-14-22-01-23-45 88:53:2E:67:07:BE} {
    puts "$mac\t[maclookup $mac]"
}
```

     
{{out}}

```txt
00-14-22-01-23-45       Dell Inc.
88:53:2E:67:07:BE       Intel Corporate
```



## zkl

{{trans|Lua}}
Uses libcurl (the multiprotocol file transfer library) to do the web query

```zkl
var [const] CURL=Import("zklCurl");   // libcurl
const MAC_VENDORS="http://api.macvendors.com/";

fcn lookUp(macAddress){
   httpAddr:=MAC_VENDORS + macAddress;
   vender:=CURL().get(httpAddr); //-->(Data,bytes of header,bytes of trailer)
   vender=vender[0].del(0,vender[1]);  // remove HTTP header
   vender.text;		// Data --> String (Data is a byte bucket)
}
```


```zkl
lookUp("FC-A1-3E-2A-1C-33").println();
lookUp("4c:72:b9:56:fe:bc").println();
lookUp("foobar").println();
```

{{out}}

```txt

Samsung Electronics Co.,Ltd
PEGATRON CORPORATION
Vendor not found

```

