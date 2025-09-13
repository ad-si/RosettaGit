+++
title = "HTTP"
description = ""
date = 2019-10-15T10:22:18Z
aliases = []
[extra]
id = 3055
[taxonomies]
categories = ["task", "Programming environment operations"]
tags = []
+++

## Task

Access and print a [[wp:Uniform Resource Locator|URL's]] content (the located resource) to the console.

There is a separate task for [[HTTPS Request]]s.





## 8th


```forth

"http://www.rosettacode.org" net:get drop >s .

```



## ABAP

This works for ABAP Version 7.40 and above

```ABAP

report z_http.

cl_http_client=>create_by_url(
  exporting
    url                = `http://rosettacode.org/robots.txt`
  importing
    client             = data(http_client)
  exceptions
    argument_not_found = 1
    plugin_not_active  = 2
    internal_error     = 3
    others             = 4 ).

if sy-subrc <> 0.
  data(error_message) = switch string( sy-subrc
    when 1 then `argument_not_found`
    when 2 then `plugin_not_active`
    when 3 then `internal_error`
    when 4 then `other error` ).

  write error_message.
  exit.
endif.

data(rest_http_client) = cast if_rest_client( new cl_rest_http_client( http_client ) ).

rest_http_client->get( ).

data(response_string) = rest_http_client->get_response_entity( )->get_string_data( ).

split response_string at cl_abap_char_utilities=>newline into table data(output_table).

loop at output_table assigning field-symbol(<output_line>).
  write / <output_line>.
endloop.

```


```txt

User-agent: *
Allow: /mw/images/
Allow: /mw/skins/
Allow: /mw/title.png
Allow: /mw/resources/
Disallow: /w/
Disallow: /mw/
Disallow: /wiki/Special:

```



## ActionScript


```actionscript

package
{
    import flash.display.Sprite;
    import flash.events.Event;
    import flash.net.*;

    public class RequestExample extends Sprite
    {
        public function RequestExample()
        {
            var loader:URLLoader = new URLLoader();
            loader.addEventListener(Event.COMPLETE, loadComplete);
            loader.load(new URLRequest("http://www.rosettacode.org"));
        }

        private function loadComplete(evt:Event):void
        {
            trace(evt.target.data);
        }
    }
}

```



## Ada

```ada

with Ada.Text_IO; use Ada.Text_IO;

with AWS.Client;
with AWS.Response;

procedure HTTP_Request is
begin
   Put_Line (AWS.Response.Message_Body (AWS.Client.Get (URL => "http://www.rosettacode.org")));
end HTTP_Request;

```



## ALGOL 68

```algol68

STRING domain="rosettacode.org";
STRING page="wiki/Main_Page";

STRING re success="^HTTP/[0-9.]* 200";
STRING re result description="^HTTP/[0-9.]* [0-9]+ [a-zA-Z ]*";
STRING re doctype ="\s\s<!DOCTYPE html PUBLIC ""[^>]+"">\s+";

PROC html page = (REF STRING page) BOOL: (
     BOOL out=grep in string(re success, page, NIL, NIL) = 0;
     IF INT start, end;
        grep in string(re result description, page, start, end) = 0
     THEN
        page:=page[end+1:];
        IF grep in string(re doctype, page, start, end) = 0
        THEN page:=page[start+2:]
        ELSE print ("unknown format retrieving page")
        FI
     ELSE print ("unknown error retrieving page")
     FI;
     out
);

IF STRING reply;
   INT rc =
      http content (reply, domain, "http://"+domain+"/"+page, 0);
   rc = 0 AND html page (reply)
THEN print (reply)
ELSE print (strerror (rc))
FI

```



## Arturo



```arturo
use ~net

print $(download "http://google.com")
```


```txt
<!doctype html><html itemscope="" itemtype="http://schema.org/WebPage" lang="es"><head><meta content="Google.es permite acceder a la información mundial en castellano, catalán, gallego, euskara e inglés." name="description"><meta content="noodp" name="robots"><meta content="text/html; charset=UTF-8" http-equiv="Content-Type"><meta content="/images/branding/googleg/1x/googleg_standard_color_128dp.png" itemprop="image"><title>Google</title><script nonce="mEe5oG98axwLddedgOh1JA==">(function(){window.google={kEI:'lp2lXbjlCJGKauK8o9AB',kEXPI:'0,18167,1335579,5663,730,224,510,18,228,819,1535,1617,378,206,1017,53,173,1163,798,10,50,211,452,319,19,96,161,89,193,122,766,81,176,221,1130704,1197793,230,302939,26305,1294,12383,4855,32692,15247,867,12163,16521,363,3320,5505,2436,5948,1119,2,579,727,2431,1362,4323,4967,774,2250,4744,3118,6196,1719,1808,1976,2044,8909,5071,226,897,1119,38,920,2090,2975,2736,49,2606,315,91,2,632,3240,4191,1571,2303,2883,19,319,235,884,904,101,2024,1,370,2778,917,261,731,509,777,7,2796,887,80,601,11,14,1279,2212,202,37,286,5,1252,327,513,324,193,1466,8,48,1

[output truncated]

```



## AutoHotkey


```AutoHotkey

UrlDownloadToFile, http://rosettacode.org, url.html
Run, cmd /k type url.html

```



## AWK

```awk
BEGIN {
  site="en.wikipedia.org"
  path="/wiki/"
  name="Rosetta_Code"

  server = "/inet/tcp/0/" site "/80"
  print "GET " path name " HTTP/1.0" |& server
  print "Host: " site |& server
  print "\r\n\r\n" |& server

  while ( (server |& getline fish) > 0 ) {
    if ( ++scale == 1 )
      ship = fish
    else
      ship = ship "\n" fish
  }
  close(server)

  print ship
}
```



## BaCon


```qbasic
'
' Read and display a website
'
IF AMOUNT(ARGUMENT$) = 1 THEN
    website$ = "www.basic-converter.org"
ELSE
    website$ = TOKEN$(ARGUMENT$, 2)
ENDIF

OPEN website$ & ":80" FOR NETWORK AS mynet
SEND "GET / HTTP/1.1\r\nHost: " & website$ & "\r\n\r\n" TO mynet
REPEAT
    RECEIVE dat$ FROM mynet
    total$ = total$ & dat$
UNTIL ISFALSE(WAIT(mynet, 500))
CLOSE NETWORK mynet
PRINT total$

```



## Batch File


```batch

curl.exe -s -L http://rosettacode.org/

```



## BBC BASIC

```bbcbasic
      SYS "LoadLibrary", "URLMON.DLL" TO urlmon%
      SYS "GetProcAddress", urlmon%, "URLDownloadToFileA" TO URLDownloadToFile

      url$ = "http://www.bbcbasic.co.uk/aboutus.html"
      file$ = @tmp$ + "rosetta.tmp"
      SYS URLDownloadToFile, 0, url$, file$, 0, 0 TO fail%
      IF fail% ERROR 100, "File download failed"

      OSCLI "TYPE """ + file$ + """"
```



## Biferno

simple one-liner using httpExt and quick print $

```Biferno
$httpExt.ExecRemote("www.tabasoft.it")
```



## C

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
                curl_easy_setopt(curl, CURLOPT_URL, "http://www.rosettacode.org/");
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



## C++


```cpp

#include <winsock2.h>
#include <ws2tcpip.h>
#include <iostream>

int main() {
	WSADATA wsaData;
	WSAStartup( MAKEWORD( 2, 2 ), &wsaData );

	addrinfo *result = NULL;
	addrinfo hints;

	ZeroMemory( &hints, sizeof( hints ) );
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_protocol = IPPROTO_TCP;

	getaddrinfo( "74.125.45.100", "80", &hints, &result ); // http://www.google.com

	SOCKET s = socket( result->ai_family, result->ai_socktype, result->ai_protocol );

	connect( s, result->ai_addr, (int)result->ai_addrlen );

	freeaddrinfo( result );

	send( s, "GET / HTTP/1.0\n\n", 16, 0 );

	char buffer[512];
	int bytes;

	do {
		bytes = recv( s, buffer, 512, 0 );

		if ( bytes > 0 )
			std::cout.write(buffer, bytes);
	} while ( bytes > 0 );

	return 0;
}

```


```cpp

#include <Web/Web.h>

using namespace Upp;

CONSOLE_APP_MAIN
{
	Cout() << HttpClient("www.rosettacode.org").ExecuteRedirect();
}

```



## C#


```c#

using System;
using System.Text;
using System.Net;

class Program
{
    static void Main(string[] args)
    {
        WebClient wc = new WebClient();
        string content = wc.DownloadString("http://www.google.com");
        Console.WriteLine(content);
    }
}

```


=={{header|Caché ObjectScript}}==


```txt

USER>Set HttpRequest=##class(%Net.HttpRequest).%New()
USER>Set HttpRequest.Server="checkip.dyndns.org"
USER>Do HttpRequest.Get("/")
USER>Do HttpRequest.HttpResponse.Data.OutputToDevice()

```




## Clojure

Using the Java API:

```clojure

(defn get-http [url]
  (let [sc (java.util.Scanner.
	    (.openStream (java.net.URL. url)))]
    (while (.hasNext sc)
      (println (.nextLine sc)))))
(get-http "http://www.rosettacode.org")

```


Using <code>clojure.contrib.http.agent</code>:

```clojure

(ns example
  (:use [clojure.contrib.http.agent :only (string http-agent)]))

(println (string (http-agent "http://www.rosettacode.org/")))

```


```clojure

(print (slurp "http://www.rosettacode.org/"))

```



## COBOL


Tested with GnuCOBOL


```cobol>COBOL  >
SOURCE FORMAT IS FIXED
       identification division.
       program-id. curl-rosetta.

       environment division.
       configuration section.
       repository.
           function read-url
           function all intrinsic.

       data division.
       working-storage section.

       copy "gccurlsym.cpy".

       01 web-page             pic x(16777216).
       01 curl-status          usage binary-long.

       01 cli                  pic x(7) external.
          88 helping           values "-h", "-help", "help", spaces.
          88 displaying        value "display".
          88 summarizing       value "summary".

      *> ***************************************************************
       procedure division.
       accept cli from command-line
       if helping then
           display "./curl-rosetta [help|display|summary]"
           goback
       end-if

      *>
      *> Read a web resource into fixed ram.
      *>   Caller is in charge of sizing the buffer,
      *>     (or getting trickier with the write callback)
      *> Pass URL and working-storage variable,
      *>   get back libcURL error code or 0 for success

       move read-url("http://www.rosettacode.org", web-page)
         to curl-status

       perform check
       perform show

       goback.
      *> ***************************************************************

      *> Now tesing the result, relying on the gccurlsym
      *>   GnuCOBOL Curl Symbol copy book
       check.
       if curl-status not equal zero then
           display
               curl-status " "
               CURLEMSG(curl-status) upon syserr
       end-if
       .

      *> And display the page
       show.
       if summarizing then
           display "Length: " stored-char-length(web-page)
       end-if
       if displaying then
           display trim(web-page trailing) with no advancing
       end-if
       .

       REPLACE ALSO ==:EXCEPTION-HANDLERS:== BY
       ==
      *> informational warnings and abends
       soft-exception.
         display space upon syserr
         display "--Exception Report-- " upon syserr
         display "Time of exception:   " current-date upon syserr
         display "Module:              " module-id upon syserr
         display "Module-path:         " module-path upon syserr
         display "Module-source:       " module-source upon syserr
         display "Exception-file:      " exception-file upon syserr
         display "Exception-status:    " exception-status upon syserr
         display "Exception-location:  " exception-location upon syserr
         display "Exception-statement: " exception-statement upon syserr
       .

       hard-exception.
           perform soft-exception
           stop run returning 127
       .
       ==.

       end program curl-rosetta.
      *> ***************************************************************

      *> ***************************************************************
      *>
      *> The function hiding all the curl details
      *>
      *> Purpose:   Call libcURL and read into memory
      *> ***************************************************************
       identification division.
       function-id. read-url.

       environment division.
       configuration section.
       repository.
           function all intrinsic.

       data division.
       working-storage section.

       copy "gccurlsym.cpy".

       replace also ==:CALL-EXCEPTION:== by
       ==
           on exception
               perform hard-exception
       ==.

       01 curl-handle          usage pointer.
       01 callback-handle      usage procedure-pointer.
       01 memory-block.
          05 memory-address    usage pointer sync.
          05 memory-size       usage binary-long sync.
          05 running-total     usage binary-long sync.
       01 curl-result          usage binary-long.

       01 cli                  pic x(7) external.
          88 helping           values "-h", "-help", "help", spaces.
          88 displaying        value "display".
          88 summarizing       value "summary".

       linkage section.
       01 url                  pic x any length.
       01 buffer               pic x any length.
       01 curl-status          usage binary-long.

      *> ***************************************************************
       procedure division using url buffer returning curl-status.
       if displaying or summarizing then
           display "Read: " url upon syserr
       end-if

      *> initialize libcurl, hint at missing library if need be
       call "curl_global_init" using by value CURL_GLOBAL_ALL
           on exception
               display
                   "need libcurl, link with -lcurl" upon syserr
               stop run returning 1
       end-call

      *> initialize handle
       call "curl_easy_init" returning curl-handle
           :CALL-EXCEPTION:
       end-call
       if curl-handle equal NULL then
           display "no curl handle" upon syserr
           stop run returning 1
       end-if

      *> Set the URL
       call "curl_easy_setopt" using
           by value curl-handle
           by value CURLOPT_URL
           by reference concatenate(trim(url trailing), x"00")
           :CALL-EXCEPTION:
       end-call

      *> follow all redirects
       call "curl_easy_setopt" using
           by value curl-handle
           by value CURLOPT_FOLLOWLOCATION
           by value 1
           :CALL-EXCEPTION:
       end-call

      *> set the call back to write to memory
       set callback-handle to address of entry "curl-write-callback"
       call "curl_easy_setopt" using
           by value curl-handle
           by value CURLOPT_WRITEFUNCTION
           by value callback-handle
           :CALL-EXCEPTION:
       end-call

      *> set the curl handle data handling structure
       set memory-address to address of buffer
       move length(buffer) to memory-size
       move 1 to running-total

       call "curl_easy_setopt" using
           by value curl-handle
           by value CURLOPT_WRITEDATA
           by value address of memory-block
           :CALL-EXCEPTION:
       end-call

      *> some servers demand an agent
       call "curl_easy_setopt" using
           by value curl-handle
           by value CURLOPT_USERAGENT
           by reference concatenate("libcurl-agent/1.0", x"00")
           :CALL-EXCEPTION:
       end-call

      *> let curl do all the hard work
       call "curl_easy_perform" using
           by value curl-handle
           returning curl-result
           :CALL-EXCEPTION:
       end-call

      *> the call back will handle filling ram, return the result code
       move curl-result to curl-status

      *> curl clean up, more important if testing cookies
       call "curl_easy_cleanup" using
           by value curl-handle
           returning omitted
           :CALL-EXCEPTION:
       end-call

       goback.

       :EXCEPTION-HANDLERS:

       end function read-url.
      *> ***************************************************************

      *> ***************************************************************
      *> Supporting libcurl callback
       identification division.
       program-id. curl-write-callback.

       environment division.
       configuration section.
       repository.
           function all intrinsic.

       data division.
       working-storage section.
       01 real-size            usage binary-long.

      *> libcURL will pass a pointer to this structure in the callback
       01 memory-block         based.
          05 memory-address    usage pointer sync.
          05 memory-size       usage binary-long sync.
          05 running-total     usage binary-long sync.

       01 content-buffer       pic x(65536) based.
       01 web-space            pic x(16777216) based.
       01 left-over            usage binary-long.

       linkage section.
       01 contents             usage pointer.
       01 element-size         usage binary-long.
       01 element-count        usage binary-long.
       01 memory-structure     usage pointer.

      *> ***************************************************************
       procedure division
           using
              by value contents
              by value element-size
              by value element-count
              by value memory-structure
          returning real-size.

       set address of memory-block to memory-structure
       compute real-size = element-size * element-count end-compute

      *> Fence off the end of buffer
       compute
           left-over = memory-size - running-total
       end-compute
       if left-over > 0 and < real-size then
           move left-over to real-size
       end-if

      *> if there is more buffer, and data not zero length
       if (left-over > 0) and (real-size > 1) then
           set address of content-buffer to contents
           set address of web-space to memory-address

           move content-buffer(1:real-size)
             to web-space(running-total:real-size)

           add real-size to running-total
       else
           display "curl buffer sizing problem" upon syserr
       end-if

       goback.
       end program curl-write-callback.
```


and a copybook


```cobol
      *> manifest constants for libcurl
      *> Usage: COPY occurlsym  inside data division
      *>  Taken from include/curl/curl.h 2013-12-19

      *> Functional enums
       01 CURL_MAX_HTTP_HEADER CONSTANT AS     102400.

       78 CURL_GLOBAL_ALL                      VALUE 3.

       78 CURLOPT_FOLLOWLOCATION               VALUE 52.
       78 CURLOPT_WRITEDATA                    VALUE 10001.
       78 CURLOPT_URL                          VALUE 10002.
       78 CURLOPT_USERAGENT                    VALUE 10018.
       78 CURLOPT_WRITEFUNCTION                VALUE 20011.
       78 CURLOPT_COOKIEFILE                   VALUE 10031.
       78 CURLOPT_COOKIEJAR                    VALUE 10082.
       78 CURLOPT_COOKIELIST                   VALUE 10135.

      *> Informationals
       78 CURLINFO_COOKIELIST                  VALUE 4194332.

      *> Result codes
       78 CURLE_OK                             VALUE 0.
      *> Error codes
       78 CURLE_UNSUPPORTED_PROTOCOL           VALUE 1.
       78 CURLE_FAILED_INIT                    VALUE 2.
       78 CURLE_URL_MALFORMAT                  VALUE 3.
       78 CURLE_OBSOLETE4                      VALUE 4.
       78 CURLE_COULDNT_RESOLVE_PROXY          VALUE 5.
       78 CURLE_COULDNT_RESOLVE_HOST           VALUE 6.
       78 CURLE_COULDNT_CONNECT                VALUE 7.
       78 CURLE_FTP_WEIRD_SERVER_REPLY         VALUE 8.
       78 CURLE_REMOTE_ACCESS_DENIED           VALUE 9.
       78 CURLE_OBSOLETE10                     VALUE 10.
       78 CURLE_FTP_WEIRD_PASS_REPLY           VALUE 11.
       78 CURLE_OBSOLETE12                     VALUE 12.
       78 CURLE_FTP_WEIRD_PASV_REPLY           VALUE 13.
       78 CURLE_FTP_WEIRD_227_FORMAT           VALUE 14.
       78 CURLE_FTP_CANT_GET_HOST              VALUE 15.
       78 CURLE_OBSOLETE16                     VALUE 16.
       78 CURLE_FTP_COULDNT_SET_TYPE           VALUE 17.
       78 CURLE_PARTIAL_FILE                   VALUE 18.
       78 CURLE_FTP_COULDNT_RETR_FILE          VALUE 19.
       78 CURLE_OBSOLETE20                     VALUE 20.
       78 CURLE_QUOTE_ERROR                    VALUE 21.
       78 CURLE_HTTP_RETURNED_ERROR            VALUE 22.
       78 CURLE_WRITE_ERROR                    VALUE 23.
       78 CURLE_OBSOLETE24                     VALUE 24.
       78 CURLE_UPLOAD_FAILED                  VALUE 25.
       78 CURLE_READ_ERROR                     VALUE 26.
       78 CURLE_OUT_OF_MEMORY                  VALUE 27.
       78 CURLE_OPERATION_TIMEDOUT             VALUE 28.
       78 CURLE_OBSOLETE29                     VALUE 29.
       78 CURLE_FTP_PORT_FAILED                VALUE 30.
       78 CURLE_FTP_COULDNT_USE_REST           VALUE 31.
       78 CURLE_OBSOLETE32                     VALUE 32.
       78 CURLE_RANGE_ERROR                    VALUE 33.
       78 CURLE_HTTP_POST_ERROR                VALUE 34.
       78 CURLE_SSL_CONNECT_ERROR              VALUE 35.
       78 CURLE_BAD_DOWNLOAD_RESUME            VALUE 36.
       78 CURLE_FILE_COULDNT_READ_FILE         VALUE 37.
       78 CURLE_LDAP_CANNOT_BIND               VALUE 38.
       78 CURLE_LDAP_SEARCH_FAILED             VALUE 39.
       78 CURLE_OBSOLETE40                     VALUE 40.
       78 CURLE_FUNCTION_NOT_FOUND             VALUE 41.
       78 CURLE_ABORTED_BY_CALLBACK            VALUE 42.
       78 CURLE_BAD_FUNCTION_ARGUMENT          VALUE 43.
       78 CURLE_OBSOLETE44                     VALUE 44.
       78 CURLE_INTERFACE_FAILED               VALUE 45.
       78 CURLE_OBSOLETE46                     VALUE 46.
       78 CURLE_TOO_MANY_REDIRECTS             VALUE 47.
       78 CURLE_UNKNOWN_TELNET_OPTION          VALUE 48.
       78 CURLE_TELNET_OPTION_SYNTAX           VALUE 49.
       78 CURLE_OBSOLETE50                     VALUE 50.
       78 CURLE_PEER_FAILED_VERIFICATION       VALUE 51.
       78 CURLE_GOT_NOTHING                    VALUE 52.
       78 CURLE_SSL_ENGINE_NOTFOUND            VALUE 53.
       78 CURLE_SSL_ENGINE_SETFAILED           VALUE 54.
       78 CURLE_SEND_ERROR                     VALUE 55.
       78 CURLE_RECV_ERROR                     VALUE 56.
       78 CURLE_OBSOLETE57                     VALUE 57.
       78 CURLE_SSL_CERTPROBLEM                VALUE 58.
       78 CURLE_SSL_CIPHER                     VALUE 59.
       78 CURLE_SSL_CACERT                     VALUE 60.
       78 CURLE_BAD_CONTENT_ENCODING           VALUE 61.
       78 CURLE_LDAP_INVALID_URL               VALUE 62.
       78 CURLE_FILESIZE_EXCEEDED              VALUE 63.
       78 CURLE_USE_SSL_FAILED                 VALUE 64.
       78 CURLE_SEND_FAIL_REWIND               VALUE 65.
       78 CURLE_SSL_ENGINE_INITFAILED          VALUE 66.
       78 CURLE_LOGIN_DENIED                   VALUE 67.
       78 CURLE_TFTP_NOTFOUND                  VALUE 68.
       78 CURLE_TFTP_PERM                      VALUE 69.
       78 CURLE_REMOTE_DISK_FULL               VALUE 70.
       78 CURLE_TFTP_ILLEGAL                   VALUE 71.
       78 CURLE_TFTP_UNKNOWNID                 VALUE 72.
       78 CURLE_REMOTE_FILE_EXISTS             VALUE 73.
       78 CURLE_TFTP_NOSUCHUSER                VALUE 74.
       78 CURLE_CONV_FAILED                    VALUE 75.
       78 CURLE_CONV_REQD                      VALUE 76.
       78 CURLE_SSL_CACERT_BADFILE             VALUE 77.
       78 CURLE_REMOTE_FILE_NOT_FOUND          VALUE 78.
       78 CURLE_SSH                            VALUE 79.
       78 CURLE_SSL_SHUTDOWN_FAILED            VALUE 80.
       78 CURLE_AGAIN                          VALUE 81.

      *> Error strings
       01 LIBCURL_ERRORS.
          02 CURLEVALUES.
             03 FILLER PIC X(30) VALUE "CURLE_UNSUPPORTED_PROTOCOL    ".
             03 FILLER PIC X(30) VALUE "CURLE_FAILED_INIT             ".
             03 FILLER PIC X(30) VALUE "CURLE_URL_MALFORMAT           ".
             03 FILLER PIC X(30) VALUE "CURLE_OBSOLETE4               ".
             03 FILLER PIC X(30) VALUE "CURLE_COULDNT_RESOLVE_PROXY   ".
             03 FILLER PIC X(30) VALUE "CURLE_COULDNT_RESOLVE_HOST    ".
             03 FILLER PIC X(30) VALUE "CURLE_COULDNT_CONNECT         ".
             03 FILLER PIC X(30) VALUE "CURLE_FTP_WEIRD_SERVER_REPLY  ".
             03 FILLER PIC X(30) VALUE "CURLE_REMOTE_ACCESS_DENIED    ".
             03 FILLER PIC X(30) VALUE "CURLE_OBSOLETE10              ".
             03 FILLER PIC X(30) VALUE "CURLE_FTP_WEIRD_PASS_REPLY    ".
             03 FILLER PIC X(30) VALUE "CURLE_OBSOLETE12              ".
             03 FILLER PIC X(30) VALUE "CURLE_FTP_WEIRD_PASV_REPLY    ".
             03 FILLER PIC X(30) VALUE "CURLE_FTP_WEIRD_227_FORMAT    ".
             03 FILLER PIC X(30) VALUE "CURLE_FTP_CANT_GET_HOST       ".
             03 FILLER PIC X(30) VALUE "CURLE_OBSOLETE16              ".
             03 FILLER PIC X(30) VALUE "CURLE_FTP_COULDNT_SET_TYPE    ".
             03 FILLER PIC X(30) VALUE "CURLE_PARTIAL_FILE            ".
             03 FILLER PIC X(30) VALUE "CURLE_FTP_COULDNT_RETR_FILE   ".
             03 FILLER PIC X(30) VALUE "CURLE_OBSOLETE20              ".
             03 FILLER PIC X(30) VALUE "CURLE_QUOTE_ERROR             ".
             03 FILLER PIC X(30) VALUE "CURLE_HTTP_RETURNED_ERROR     ".
             03 FILLER PIC X(30) VALUE "CURLE_WRITE_ERROR             ".
             03 FILLER PIC X(30) VALUE "CURLE_OBSOLETE24              ".
             03 FILLER PIC X(30) VALUE "CURLE_UPLOAD_FAILED           ".
             03 FILLER PIC X(30) VALUE "CURLE_READ_ERROR              ".
             03 FILLER PIC X(30) VALUE "CURLE_OUT_OF_MEMORY           ".
             03 FILLER PIC X(30) VALUE "CURLE_OPERATION_TIMEDOUT      ".
             03 FILLER PIC X(30) VALUE "CURLE_OBSOLETE29              ".
             03 FILLER PIC X(30) VALUE "CURLE_FTP_PORT_FAILED         ".
             03 FILLER PIC X(30) VALUE "CURLE_FTP_COULDNT_USE_REST    ".
             03 FILLER PIC X(30) VALUE "CURLE_OBSOLETE32              ".
             03 FILLER PIC X(30) VALUE "CURLE_RANGE_ERROR             ".
             03 FILLER PIC X(30) VALUE "CURLE_HTTP_POST_ERROR         ".
             03 FILLER PIC X(30) VALUE "CURLE_SSL_CONNECT_ERROR       ".
             03 FILLER PIC X(30) VALUE "CURLE_BAD_DOWNLOAD_RESUME     ".
             03 FILLER PIC X(30) VALUE "CURLE_FILE_COULDNT_READ_FILE  ".
             03 FILLER PIC X(30) VALUE "CURLE_LDAP_CANNOT_BIND        ".
             03 FILLER PIC X(30) VALUE "CURLE_LDAP_SEARCH_FAILED      ".
             03 FILLER PIC X(30) VALUE "CURLE_OBSOLETE40              ".
             03 FILLER PIC X(30) VALUE "CURLE_FUNCTION_NOT_FOUND      ".
             03 FILLER PIC X(30) VALUE "CURLE_ABORTED_BY_CALLBACK     ".
             03 FILLER PIC X(30) VALUE "CURLE_BAD_FUNCTION_ARGUMENT   ".
             03 FILLER PIC X(30) VALUE "CURLE_OBSOLETE44              ".
             03 FILLER PIC X(30) VALUE "CURLE_INTERFACE_FAILED        ".
             03 FILLER PIC X(30) VALUE "CURLE_OBSOLETE46              ".
             03 FILLER PIC X(30) VALUE "CURLE_TOO_MANY_REDIRECTS      ".
             03 FILLER PIC X(30) VALUE "CURLE_UNKNOWN_TELNET_OPTION   ".
             03 FILLER PIC X(30) VALUE "CURLE_TELNET_OPTION_SYNTAX    ".
             03 FILLER PIC X(30) VALUE "CURLE_OBSOLETE50              ".
             03 FILLER PIC X(30) VALUE "CURLE_PEER_FAILED_VERIFICATION".
             03 FILLER PIC X(30) VALUE "CURLE_GOT_NOTHING             ".
             03 FILLER PIC X(30) VALUE "CURLE_SSL_ENGINE_NOTFOUND     ".
             03 FILLER PIC X(30) VALUE "CURLE_SSL_ENGINE_SETFAILED    ".
             03 FILLER PIC X(30) VALUE "CURLE_SEND_ERROR              ".
             03 FILLER PIC X(30) VALUE "CURLE_RECV_ERROR              ".
             03 FILLER PIC X(30) VALUE "CURLE_OBSOLETE57              ".
             03 FILLER PIC X(30) VALUE "CURLE_SSL_CERTPROBLEM         ".
             03 FILLER PIC X(30) VALUE "CURLE_SSL_CIPHER              ".
             03 FILLER PIC X(30) VALUE "CURLE_SSL_CACERT              ".
             03 FILLER PIC X(30) VALUE "CURLE_BAD_CONTENT_ENCODING    ".
             03 FILLER PIC X(30) VALUE "CURLE_LDAP_INVALID_URL        ".
             03 FILLER PIC X(30) VALUE "CURLE_FILESIZE_EXCEEDED       ".
             03 FILLER PIC X(30) VALUE "CURLE_USE_SSL_FAILED          ".
             03 FILLER PIC X(30) VALUE "CURLE_SEND_FAIL_REWIND        ".
             03 FILLER PIC X(30) VALUE "CURLE_SSL_ENGINE_INITFAILED   ".
             03 FILLER PIC X(30) VALUE "CURLE_LOGIN_DENIED            ".
             03 FILLER PIC X(30) VALUE "CURLE_TFTP_NOTFOUND           ".
             03 FILLER PIC X(30) VALUE "CURLE_TFTP_PERM               ".
             03 FILLER PIC X(30) VALUE "CURLE_REMOTE_DISK_FULL        ".
             03 FILLER PIC X(30) VALUE "CURLE_TFTP_ILLEGAL            ".
             03 FILLER PIC X(30) VALUE "CURLE_TFTP_UNKNOWNID          ".
             03 FILLER PIC X(30) VALUE "CURLE_REMOTE_FILE_EXISTS      ".
             03 FILLER PIC X(30) VALUE "CURLE_TFTP_NOSUCHUSER         ".
             03 FILLER PIC X(30) VALUE "CURLE_CONV_FAILED             ".
             03 FILLER PIC X(30) VALUE "CURLE_CONV_REQD               ".
             03 FILLER PIC X(30) VALUE "CURLE_SSL_CACERT_BADFILE      ".
             03 FILLER PIC X(30) VALUE "CURLE_REMOTE_FILE_NOT_FOUND   ".
             03 FILLER PIC X(30) VALUE "CURLE_SSH                     ".
             03 FILLER PIC X(30) VALUE "CURLE_SSL_SHUTDOWN_FAILED     ".
             03 FILLER PIC X(30) VALUE "CURLE_AGAIN                   ".
       01 FILLER REDEFINES LIBCURL_ERRORS.
          02 CURLEMSG OCCURS 81 TIMES PIC X(30).
```


```txt
prompt$ ./curl-rosetta summary
Read: http://www.rosettacode.org
Length: 000024043

prompt$ ./curl-rosetta display
Read: http://www.rosettacode.org
<!DOCTYPE html>
<html lang="en" dir="ltr" class="client-nojs">
<head>
...
```



## ColdFusion


```coldfusion

  <cfhttp url="http://www.rosettacode.org" result="result">
  <cfoutput>#result.FileContent#</cfoutput>

```



## Common Lisp

CLISP provides an extension function to read http sources. Other implementations may do this differently.
```lisp

(defun wget-clisp (url)
    (ext:with-http-input (stream url)
        (loop for line = (read-line stream nil nil)
            while line
            do (format t "~a~%" line))))

```


First grabbing the entire body as a string, and then by pulling from a stream (as in the CLISP example).


```lisp

(defun wget-drakma-string (url &optional (out *standard-output*))
  "Grab the body as a string, and write it to out."
  (write-string (drakma:http-request url) out))

(defun wget-drakma-stream (url &optional (out *standard-output*))
  "Grab the body as a stream, and write it to out."
  (loop with body = (drakma:http-request url :want-stream t)
        for line = (read-line body nil nil)
        while line do (write-line line out)
        finally (close body)))

```



## Crystal


```crystal

require "http/client"

HTTP::Client.get("http://google.com")

```


## D

```D

void main() {
  import std.stdio, std.net.curl;
  writeln(get("http://google.com"));
}

```


```D

import tango.io.Console;
import tango.net.http.HttpGet;

void main() {
  Cout.stream.copy( (new HttpGet("http://google.com")).open );
}

```


Or more operating directly on the socket:


```D

import tango.io.Console;
import tango.net.InternetAddress;
import tango.net.device.Socket;

void main() {
  auto site = new Socket;
  site.connect (new InternetAddress("google.com",80)).write ("GET / HTTP/1.0\n\n");

  Cout.stream.copy (site);
}

```



## Dart

Using the stand-alone VM:

```d
import 'dart:io';
void main(){
  var url = 'http://rosettacode.org';
  var client = new HttpClient();
  client.getUrl(Uri.parse(url))
        .then((HttpClientRequest request)   => request.close())
        .then((HttpClientResponse response) => response.pipe(stdout));
}
```



## Delphi

Simple example using the free Synapse TCP/IP library [http://www.ararat.cz/synapse/doku.php/download]


```Delphi

program HTTP;

{$APPTYPE CONSOLE}

{$DEFINE DEBUG}

uses
  Classes,
  httpsend; // Synapse httpsend class

var
  Response: TStrings;
  HTTPObj: THTTPSend;

begin
  HTTPObj := THTTPSend.Create;
  try
    { Stringlist object to capture HTML returned
      from URL }
    Response := TStringList.Create;
    try
      if HTTPObj.HTTPMethod('GET','http://www.mgis.uk.com') then
        begin
          { Load HTTP Document into Stringlist }
          Response.LoadFromStream(HTTPObj.Document);
          { Write the response to the console window }
          Writeln(Response.Text);
        end
        else
        Writeln('Error retrieving data');

    finally
      Response.Free;
    end;

  finally
    HTTPObj.Free;
  end;

  // Keep console window open
  Readln;

end.

```



Using Indy:


```Delphi

program ShowHTTP;

{$APPTYPE CONSOLE}

uses IdHttp;

var
  s: string;
  lHTTP: TIdHTTP;
begin
  lHTTP := TIdHTTP.Create(nil);
  try
    lHTTP.HandleRedirects := True;
    s := lHTTP.Get('http://www.rosettacode.org');
    Writeln(s);
  finally
    lHTTP.Free;
  end;
end.

```



## Dragon


```dragon
select "http"
select "std"

http("http://www.rosettacode.org", ::echo)


```



## E



```e

when (def t := <http://www.rosettacode.org> <- getText()) -> {
    println(t)
}

```



## EchoLisp

'''file->string''' usage: the server must allow cross-domain access, or a browser add-on like cors-everywhere must be installed to bypass cross-domain checking.

```scheme

;; asynchronous call back definition
(define (success name text) (writeln 'Loaded name) (writeln text))
;;
(file->string success "http://www.google.com")

```



## Emacs Lisp

<code>url.el</code> can download HTTP.  <code>url-retrieve-synchronously</code> returns a buffer containing headers and body.  Caller kills the buffer when no longer required.


```Lisp
(with-current-buffer
    (url-retrieve-synchronously "http://www.rosettacode.org")
  (goto-char (point-min))
  (search-forward "\n\n" nil t)  ;; skip headers
  (prin1 (buffer-substring (point) (point-max)))
  (kill-buffer (current-buffer)))
```



## Erlang


### Synchronous


```erlang

-module(main).
-export([main/1]).

main([Url|[]]) ->
   inets:start(),
   case http:request(Url) of
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
   http:request(get, {Url, [] }, [], [{sync, false}]),
   receive
       {http, {_ReqId, Res}} -> io:fwrite("~p~n",[Res]);
       _Any -> io:fwrite("Error: ~p~n",[_Any])
       after 10000 -> io:fwrite("Timed out.~n",[])
   end.

```


Using it

```erlang

|escript ./req.erl http://www.rosettacode.org

```


=={{header|F_Sharp|F#}}==
In F# we can just use the .NET library to do this so its the same as the [[C_sharp|C#]] example.


```fsharp

let wget (url : string) =
    use c = new System.Net.WebClient()
    c.DownloadString(url)

printfn "%s" (wget "http://www.rosettacode.org/")

```


However unlike C#, F# can use an asynchronous workflow to avoid blocking any threads while waiting for a response from the server. To asynchronously download three url's at once...


```fsharp

open System.Net
open System.IO

let wgetAsync url =
    async { let request = WebRequest.Create (url:string)
            use! response = request.AsyncGetResponse()
            use responseStream = response.GetResponseStream()
            use reader = new StreamReader(responseStream)
            return reader.ReadToEnd() }

let urls = ["http://www.rosettacode.org/"; "http://www.yahoo.com/"; "http://www.google.com/"]
let content = urls
              |> List.map wgetAsync
              |> Async.Parallel
              |> Async.RunSynchronously
```



## Factor


```factor
USE: http.client
"http://www.rosettacode.org" http-get nip print

```



## Forth

This works at the socket level, returning both the HTTP headers and page contents.

```forth

include unix/socket.fs

s" localhost" 80 open-socket
dup s\" GET / HTTP/1.0\n\n" rot write-socket
dup pad 8092 read-socket  type
close-socket

```



## friendly interactive shell

```fishshell
curl -s -L http://rosettacode.org/
```



```fishshell
lynx -source http://rosettacode.org/
```



```fishshell
wget -O - -q http://rosettacode.org/
```



```fishshell
lftp -c "cat http://rosettacode.org/"
```


```fishshell
ftp -o - http://rosettacode.org ^ /dev/null
```



## Frink

Frink's <CODE>read[<I>URL</I>]</CODE> function works with any URL type supported by your Java Virtual Machine, and returns the results as a single string.

```frink

print[read["http://frinklang.org/"]]

```



## Gastona


```gastona
#listix#

   <main>
      LOOP, TEXT FILE, http://www.rosettacode.org
          , BODY, @<value>

```



## GML

'''Any Event'''

```gml
get = http_get("http://www.rosettacode.org/");
```


'''HTTP Event'''

```gml
if (ds_map_find_value(async_load,"id") == get)
    {
    show_message_async(ds_map_find_value(async_load,"result"));
    }
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
    r, err := http.Get("http://rosettacode.org/robots.txt")
    if err != nil {
        log.Fatalln(err)
    }
    io.Copy(os.Stdout, r.Body)
}

```


Output:

```txt

User-agent: *
Allow: /mw/images/
Allow: /mw/skins/
Allow: /mw/title.png
Disallow: /w/
Disallow: /mw/
Disallow: /wiki/Special:

```



## Groovy


```groovy

new URL("http://www.rosettacode.org").eachLine { println it }

```



## GUISS


It would be more appropriate to paste to notepad:


```guiss
Start,Programs,Applications,Mozilla Firefox,Inputbox:address bar>www.rosettacode.org,Button:Go,
Click:Area:browser window,Type:[Control A],[Control C],Start,Programs,Accessories,Notepad,
Menu:Edit,Paste
```



## Halon


```halon
echo http("http://www.rosettacode.org");
```



## Haskell

Using {{libheader|HTTP}} from [http://hackage.haskell.org/packages/hackage.html HackageDB]


```haskell

import Network.Browser
import Network.HTTP
import Network.URI

main = do
    rsp <- Network.Browser.browse $ do
        setAllowRedirects True
        setOutHandler $ const (return ())
        request $ getRequest "http://www.rosettacode.org/"
    putStrLn $ rspBody $ snd rsp

```


== Icon and Unicon ==
=
## Icon
=

```icon

link cfunc
procedure main(arglist)
   get(arglist[1])
end

procedure get(url)
   local f, host, port, path
   url ? {
         ="http://" | ="HTTP://"
         host := tab(upto(':/') | 0)
         if not (=":" & (port := integer(tab(upto('/'))))) then port := 80
         if pos(0) then path := "/" else path := tab(0)
   }
   write(host)
   write(path)
   f := tconnect(host, port) | stop("Unable to connect")
   writes(f, "GET ", path | "/" ," HTTP/1.0\r\n\r\n")
   while write(read(f))
end

```


Using it

```icon

|icon req.icn http://www.rosettacode.org

```


=
## Unicon
=
Unicon provides improved socket and messaging support without the need for the external function ''cfunc'':

```unicon

procedure main(arglist)
m := open(arglist[1],"m")
while write(read(m))
end

```



## J

Using <tt>gethttp</tt> from [[Web Scraping#J|Web Scraping]]


```j
require'web/gethttp'
gethttp 'http://www.rosettacode.org'

```



## Java


```java5
import java.util.Scanner;
import java.net.URL;

public class Main {
    public static void main(String[] args) throws Exception {
        Scanner sc = new Scanner(new URL("http://www.rosettacode.org").openStream());
        while (sc.hasNext())
            System.out.println(sc.nextLine());
    }
}

```


```java5

import org.apache.commons.io.IOUtils;
import java.net.URL;

public class Main {
    public static void main(String[] args) throws Exception {
        IOUtils.copy(new URL("http://rosettacode.org").openStream(),System.out);
    }
}
```



## JavaScript



### Browser


```JavaScript
var req = new XMLHttpRequest();
req.onload = function() {
  console.log(this.responseText);
};

req.open('get', 'http://rosettacode.org', true);
req.send()
```


Using fetch API:

```JavaScript

fetch('http://rosettacode.org').then(function(response) {
  return response.text();
}).then(function(myText) {
  console.log(myText);
});

```


As a repeatable function:


```JavaScript
/**
 * @name _http
 * @description Generic API Client using XMLHttpRequest
 * @param {string} url The URI/URL to connect to
 * @param {string} method The HTTP method to invoke- GET, POST, etc
 * @param {function} callback Once the HTTP request has completed, responseText is passed into this function for execution
 * @param {object} params Query Parameters in a JavaScript Object (Optional)
 *
 */
function _http(url, method, callback, params) {
    var xhr,
        reqUrl;

    xhr = new XMLHttpRequest();
    xhr.onreadystatechange = function xhrProc() {
        if (xhr.readyState == 4 && xhr.status == 200) {
            callback(xhr.responseText);
        }
    };


    /** If Query Parameters are present, handle them... */
    if (typeof params === 'undefined') {
        reqUrl = url;
    } else {
        switch (method) {
            case 'GET':
                reqUrl = url + procQueryParams(params);
                break;
            case 'POST':
                reqUrl = url;
                break;
            default:
        }
    }


    /** Send the HTTP Request */
    if (reqUrl) {
        xhr.open(method, reqUrl, true);
        xhr.setRequestHeader("Accept", "application/json");

        if (method === 'POST') {
            xhr.send(params);
        } else {
            xhr.send();
        }
    }


    /**
     * @name procQueryParams
     * @description Return function that converts Query Parameters from a JavaScript Object to a proper URL encoded string
     * @param {object} params Query Parameters in a JavaScript Object
     *
     */
    function procQueryParams(params) {
        return "?" + Object
            .keys(params)
            .map(function (key) {
                return key + "=" + encodeURIComponent(params[key])
            })
            .join("&")
    }
}
```


Using jQuery:


```JavaScript
$.get('http://rosettacode.org', function(data) {
  console.log(data);
};
```



### Node.js


With Node.js, using only the included http module.


```javascript
const http = require('http');

http.get('http://rosettacode.org', (resp) => {

  let data = '';

  // A chunk of data has been recieved.
  resp.on('data', (chunk) => {
    data += chunk;
  });

  // The whole response has been received. Print out the result.
  resp.on('end', () => {
    console.log("Data:", data);
  });

}).on("error", (err) => {
  console.log("Error: " + err.message);
});
```



## Jsish

Based on Jsi_Wget that ships with Jsish.


```javascript
#!/usr/bin/env jsish
function httpGet(fileargs:array|string, conf:object=void) {

    var options = { // Web client for downloading files from url
        headers     : [],           // Header fields to send.
        nowait      : false,        // Just return object: caller will call update.
        onDone      : null,         // Callback when done.
        wsdebug     : 0             // WebSockets debug level.
    };

    var self = {
        address     : '',
        done        : false,
        path        : '',
        port        : -1,
        post        : '',           // Post file upload (UNIMPL).
        scheme      : 'http',       // Url scheme
        protocol    : 'get',
        url         : null,
        response    : ''
    };

    parseOpts(self, options, conf);

    if (self.port === -1)
        self.port = 80;

    function WsRecv(ws:userobj, id:number, str:string) {
        LogDebug("LEN: "+str.length);
        LogTrace("DATA", str);
        self.response += str;
    }

    function WsClose(ws:userobj|null, id:number) {
        LogDebug("CLOSE");
        self.done = true;
        if (self.onDone)
            self.onDone(id);
    }

    function main() {
        if (self.Debug)
            debugger;
        if (typeof(fileargs) === 'string')
            fileargs = [fileargs];
        if (!fileargs || fileargs.length !== 1)
            throw("expected a url arg");
        self.url = fileargs[0];
        var m = self.url.match(/^([a-zA-Z]+):\/\/([^\/]*+)(.*)$/);
        if (!m)
            throw('invalid url: '+self.url);
        self.scheme = m[1];
        self.address = m[2];
        self.path = m[3];
        var as = self.address.split(':');
        if (as.length==2) {
            self.port = parseInt(as[1]);
            self.address = as[0];
        } else  if (as.length != 1)
            throw('bad port in address: '+self.address);
        if (self.path=='')
            self.path = '/index.html';
        if (self.post.length)
            self.protocol = 'post';

        var wsopts = {
            client:true,
            onRecv:WsRecv,
            onClose:WsClose,
            debug:self.wsdebug,
            rootdir:self.path,
            port:self.port,
            address:self.address,
            protocol:self.protocol,
            clientHost:self.address
        };
        if (self.post.length)
            wsopts.post = self.post;
        if (self.headers.length)
            wsopts.headers = self.headers;
        if (self.scheme === 'https') {
            if (!Interp.conf('hasOpenSSL'))
                puts('SSL is not compiled in: falling back to http:');
            else {
                if (self.port === 80)
                    wsopts.port = 441;
                wsopts.use_ssl = true;
            }
        }
        LogDebug("Starting:", conf, wsopts);
        self.ws = new WebSocket( wsopts );
        if (self.nowait)
            return self;
        while (!self.done) {
            update(200);
            LogTrace("UPDATE");
        }
        delete self.ws;
        return self.response;
    }

    return main();
}

provide(httpGet, "0.60");

if (isMain())
    runModule(httpGet);
```


```txt
prompt$ jsish
# require('httpGet')
0.6
# var page = httpGet('http://rosettacode.org/robots.txt')
variable
# page
"User-agent: *
Allow: /mw/images/
Allow: /mw/skins/
Allow: /mw/title.png
Allow: /mw/resources/
Disallow: /w/
Disallow: /mw/
Disallow: /wiki/Special:

"
```



## Julia


```Julia
readurl(url) = open(readlines, download(url))

readurl("http://rosettacode.org/index.html")
```



## Kotlin


```scala
// version 1.1.2

import java.net.URL
import java.io.InputStreamReader
import java.util.Scanner

fun main(args: Array<String>) {
    val url = URL("http://www.puzzlers.org/pub/wordlists/unixdict.txt")
    val isr = InputStreamReader(url.openStream())
    val sc = Scanner(isr)
    while (sc.hasNextLine()) println(sc.nextLine())
    sc.close()
}
```



## Lasso

incude_url is a wrapper for Lasso's curl datatype, however it can be achieved in several ways.

```Lasso
// using include_url wrapper:
include_url('http://rosettacode.org/index.html')

// one line curl
curl('http://rosettacode.org/index')->result->asString

// using curl for more complex operations and feedback
local(x = curl('http://rosettacode.org/index'))
local(y = #x->result)
#y->asString
```



## LFE



### Synchronous


```lisp
(: inets start)
(case (: httpc request '"http://lfe.github.io")
  ((tuple 'ok result)
    (: io format '"Result: ~p" (list result)))
  ((tuple 'error reason)
    (: io format '"Error: ~p~n" (list reason))))

```



### Asynchronous


```lisp
(: inets start)
(let* ((method 'get)
       (url '"http://lfe.github.io")
       (headers ())
       (request-data (tuple url headers))
       (http-options ())
       (request-options (list (tuple 'sync 'false))))
  (: httpc request method request-data http-options request-options)
  (receive
    ((tuple 'http (tuple request-id (tuple 'error reason)))
     (: io format '"Error: ~p~n" (list reason)))
    ((tuple 'http (tuple request-id result))
     (: io format '"Result: ~p~n" (list result))))))

```



## Liberty BASIC

Uses a dll call and a timer to allow time to receive the file.

```lb

result = DownloadToFile( "http://rosettacode.org/wiki/Main_Page", "in.html")
timer 2000, [on]
wait
[on]
timer 0
if result <> 0 then print "Error downloading."

end

Function DownloadToFile( urlfile$, localfile$)
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



## Lingo

HTTP requests based on Director's native HTTP facilities - i.e. without using a 3rd party plugin ("Xtra") - are asynchronous. A simple implementation of a HTTP GET request might look like this:

Parent script "SimpleHttpGet":

```lingo
property _netID
property _cbHandler
property _cbTarget

----------------------------------------
-- Simple HTTP GET request
-- @param {string} url
-- @param {symbol} cbHandler
-- @param {object} [cbTarget=_movie]
----------------------------------------
on new (me, url, cbHandler, cbTarget)
  if voidP(cbTarget) then cbTarget = _movie
  me._netID = getNetText(url)
  me._cbHandler = cbHandler
  me._cbTarget = cbTarget
  _movie.actorList.add(me)
  return me
end

----------------------------------------
-- @callback
----------------------------------------
on stepFrame (me)
  if netDone(me._netID) then
    res = netTextResult(me._netID)
    err = netError(me._netID)
    _movie.actorList.deleteOne(me)
    call(me._cbHandler, me._cbTarget, res, err)
  end if
end
```


In some movie script:

```lingo
----------------------------------------
--
----------------------------------------
on getAdobeHomePage ()
  script("SimpleHttpGet").new("http://www.adobe.com/", #printResult)
end

----------------------------------------
-- @callback
----------------------------------------
on printResult (res, err)
  if err="OK" then
    put res
  else
    put "Network Error:" && err
  end if
end
```


Executed in the "Message Window" (=Director's interactive Lingo console):

```lingo
getAdobeHomePage()
-- "<!doctype html>
...
```



## LiveCode

Without a callback handler the get URL method will block until complete

```LiveCode
put true into libURLFollowHttpRedirects
get URL "http://httpbin.org/html"
put it
```

Non-blocking version

```LiveCode
on myUrlDownloadFinished
   answer "Download Complete" with "Okay"
end myUrlDownloadFinished

command getWebResource
    load URL "http://httpbin.org/html" with message "myUrlDownloadFinished"
end getWebResource
```



## LSL

To test it yourself; rez a box on the ground, and add the following as a New Script.

```LSL
string sURL = "http://www.RosettaCode.Org";
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
llList2String(lBody, 0)=<!DOCTYPE html>
llList2String(lBody, 1)=<html lang="en" dir="ltr" class="client-nojs">
llList2String(lBody, 2)=<head>
llList2String(lBody, 3)=<title>Rosetta Code</title>
llList2String(lBody, 4)=<meta charset="UTF-8" />
llList2String(lBody, 5)=<meta name="generator" content="MediaWiki 1.18.0" />
llList2String(lBody, 6)=<link rel="shortcut icon" href="/favicon.ico" />
llList2String(lBody, 7)=<link rel="search" type="application/opensearchdescription+xml" href="/mw/opensearch_desc.php" title="Rosetta Code (en)" />
llList2String(lBody, 8)=<link rel="EditURI" type="application/rsd+xml" href="http://rosettacode.org/mw/api.php?action=rsd" />
llList2String(lBody, 9)=<link rel="copyright" href="http://www.gnu.org/licenses/fdl-1.2.html" />
llList2String(lBody, 10)=<link rel="alternate" type="application/atom+xml" title="Rosetta Code Atom feed" href="/mw/index.php?title=Special:RecentChanges&amp;feed=atom" />
llList2String(lBody, 11)=<link rel="stylesheet" href="/mw/load.php?debug=false&amp;lang=en&amp;modules=mediawiki.legacy.commonPrint%2Cshared%7Cskins.vector&amp;only=styles&amp;skin=vector&amp;*" />
llList2String(lBody, 12)=<meta name="ResourceLoaderDynamicStyles" content="" />
llList2String(lBody, 13)=<link rel="stylesheet" href="/mw/load.php?debug=false&amp;lang=en&amp;modules=site&amp;only=styles&amp;skin=vector&amp;*" />
llList2String(lBody, 14)=<style>a:lang(ar),a:lang(ckb),a:lang(fa),a:lang(kk-arab),a:lang(mzn),a:lang(ps),a:lang(ur){text-decoration:none}a.new,#quickbar a.new{color:#ba0000}
...   ...   ...   ...   ...   ...   ...   ...   ...   ...   ...   ...   ...   ...

```



## Lua

```Lua

local http = require("socket.http")
local url = require("socket.url")
local page = http.request('http://www.google.com/m/search?q=' .. url.escape("lua"))
print(page)

```



## M2000 Interpreter

We use Async read from Microsoft.XMLHTTP
So we use Threads (duration is in millisecond)
M2000 can use COM objects, using Declare, Method and With statements.
Using With statement we can make objects properties like ReadyState as variables
(some of them as read only)


```M2000 Interpreter

Module CheckIt  {
      Declare  xml "Microsoft.XMLHTTP"
      const testUrl$ = "http://www.rosettacode.org"
      With  xml, "readyState" as ReadyState
      Method xml "Open", "Get", testUrl$, True  ' True means Async
      Method xml "send"
      \\ We set a thread to count time
      k=0
      Thread {
            k++
      }  as TimeOut interval 100
      \\ In main thread we can check ReadyState and Mouse button
      Task.Main 100 {
            Print ReadyState
            If ReadyState=4 then exit
            if k>20 then exit   ' 20*100= 2 sec
            if mouse then exit ' exit if mouse click
      }
      \\ So now we can read
      if ReadyState=4 then {
            With  xml, "responseText" AS AA$
            \\ break AA$ to lines
            Document BB$=AA$
            \\ using line breaks as CRLF
            Report BB$
      }
      Declare xml Nothing
}
CheckIt

```



## Maple

In Maple 18 or later:

```Maple

content := URL:-Get( "http://www.google.com/" );

```


In Maple 17 or earlier:

```Maple

content := HTTP:-Get( "http://www.google.com/" );

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica

Print[Import["http://www.google.com/webhp?complete=1&hl=en", "Source"]]

```


=={{header|MATLAB}} / {{header|Octave}}==
[http://www.mathworks.com/help/matlab/ref/urlread.html urlread] is MATLAB's function for making URL requests.
The documentation for Octave is available here [http://octave.sourceforge.net/octave/function/urlread.html urlread].

In this example we initiate an HTTP request for a single random number from [http://www.random.org random.org]:

```MATLAB

>> random = urlread('http://www.random.org/integers/?num=1&min=1&max=100&col=1&base=10&format=plain&rnd=new')

random =

61

```


It is possible to make more complicated requests, specifically "GET" and "POST," which is explained in the [http://www.mathworks.com/help/matlab/ref/urlread.html documentation].


## MIRC Scripting Language

See [[HTTP/MIRC Scripting Language]]


## Nemerle


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
        def myStream = wc.OpenRead("http://rosettacode.org");
        def sr = StreamReader(myStream);

        WriteLine(sr.ReadToEnd());
        myStream.Close()
    }
}
```



## NetRexx

An implementation of the [[#Java|Java]] version shown above; demonstrating NetRexx's ability to exploit the rich Java SDK.


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

import java.util.Scanner
import java.net.URL

do
  rosettaUrl = "http://www.rosettacode.org"
  sc = Scanner(URL(rosettaUrl).openStream)
  loop while sc.hasNext
    say sc.nextLine
  end
catch ex = Exception
  ex.printStackTrace
end

return
```



## NewLisp


```NewLisp

(get-url "http://www.rosettacode.org")

```



## Nim


```nim
import httpclient

var client = newHttpClient()
echo client.getContent "http://rosettacode.org"
```



## Objeck


```objeck
use HTTP;
use Collection;

class HttpTest {
  function : Main(args : String[]) ~ Nil {
    lines := HttpClient->New()->Get("http://rosettacode.org");
    each(i : lines) {
      lines->Get(i)->As(String)->PrintLine();
    };
  }
}
```


=={{header|Objective-C}}==

```objc>#import <Foundation/Foundation.h


int main (int argc, const char * argv[]) {
    @autoreleasepool {

        NSError        *error;
        NSURLResponse *response;
        NSData *data = [NSURLConnection sendSynchronousRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:@"http://rosettacode.org"]]
                                                returningResponse:&response
                                                            error:&error];

        NSLog(@"%@", [[NSString alloc] initWithData:data
                                              encoding:NSUTF8StringEncoding]);

    }
    return 0;
}
```



## OCaml


```ocaml

let () =
  let url = "http://www.rosettacode.org" in
  let _,_, page_content = make_request ~url ~kind:GET () in
  print_endline page_content;
;;

```


The source code of the function ''make_request'' is [[Web_Scraping/OCaml|here]].


## ooRexx

Got this from a friend. Needs bsf4oorexx from sourceforge.

Note that rosettacode.org (as used by java and NetRexx) does not permit this access!

```oorexx
url=.bsf~new("java.net.URL","http://teletext.orf.at")
sc =.bsf~new("java.util.Scanner",url~openStream)
loop while sc~hasNext
  say sc~nextLine
  End
::requires BSF.CLS   -- get Java camouflaging support
```

massaged to avoid problems.

```txt
<-!DOCTYPE HTML-
..
-/html-
```



## Oz

When creating a file object, it is possible to specify an URL instead of a filename:

```oz

declare
  fun {GetPage Url}
     F = {New Open.file init(url:Url)}
     Contents = {F read(list:$ size:all)}
  in
     {F close}
     Contents
  end
in
  {System.showInfo {GetPage "http://www.rosettacode.org"}}

```


If you need more fine-grained control of the request, you could use a custom library:

```oz

declare
  [HTTPClient] = {Module.link ['x-ozlib://mesaros/net/HTTPClient.ozf']}

  fun {GetPage Url}
     Client = {New HTTPClient.urlGET
	       init(inPrms(toFile:false toStrm:true)
		    httpReqPrms
		   )}
     OutParams
     HttpResponseParams
  in
     {Client getService(Url ?OutParams ?HttpResponseParams)}
     {Client closeAll(true)}
     OutParams.sOut
  end
in
  {System.showInfo {GetPage "http://www.rosettacode.org"}}

```



## Pascal

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
    s := hc.get('http://www.example.com')
  finally
    hc.free
  end;
  writeln(s)
end.
```



```pascal
program http;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

{$DEFINE DEBUG}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, httpsend; // Synapse httpsend class
{$R *.res}

var
  Response: TStrings;
  HTTPObj: THTTPSend;

begin
  HTTPObj := THTTPSend.Create;
  try
    { Stringlist object to capture HTML returned
      from URL }
    Response := TStringList.Create;
    try
      if HTTPObj.HTTPMethod('GET','http://wiki.lazarus.freepascal.org/Synapse') then
        begin
          { Load HTTP Document into Stringlist }
          Response.LoadFromStream(HTTPObj.Document);
          { Write the response to the console window }
          Writeln(Response.Text);
        end
        else
        Writeln('Error retrieving data');

    finally
      Response.Free;
    end;

  finally
    HTTPObj.Free;
  end;

  // Keep console window open
  Readln;

end.
```



## Peloton

English dialect, short form:

```sgml

<@ SAYURLLIT>http://rosettacode.org/wiki/Main_Page</@>

```


English dialect, padded variable-length form:

```sgml

<# SAY URLSOURCE LITERAL>http://rosettacode.org/wiki/Main_Page</#>

```



## Perl



### Core example

This sample is nearly identical to the LWP sample except that it uses HTTP::Tiny which was added to the core libraries in [[Perl/5.14]].


```perl
use strict; use warnings;
require 5.014; # check HTTP::Tiny part of core
use HTTP::Tiny;

print( HTTP::Tiny->new()->get( 'http://rosettacode.org')->{content} );
```



### Library examples


===={{libheader|LWP}}====
Classic LWP sample.


```perl
use LWP::Simple qw/get $ua/;
$ua->agent(undef) ; # cloudflare blocks default LWP agent
print( get("http://www.rosettacode.org") );
```


or with more error-checking


```perl
use strict;
use LWP::UserAgent;

my $url = 'http://www.rosettacode.org';
my $response = LWP::UserAgent->new->get( $url );

$response->is_success or die "Failed to GET '$url': ", $response->status_line;

print $response->as_string
```



## Perl 6

```perl6

use v6;
# Using LWP::Simple from: git://github.com/cosimo/perl6-lwp-simple.git
use LWP::Simple;

print LWP::Simple.get("http://www.rosettacode.org");

```


or, without LWP::Simple:


```perl6

use v6;

my $socket = IO::Socket::INET.new(host => "www.rosettacode.org",
				  port => 80,);
$socket.print("GET / HTTP/1.0\r\n\r\n");
print $socket.recv();
$socket.close;

```



## Phix

Note that curl_easy_get_file() is better suited to multi-megabyte downloads than curl_easy_perform_ex().

```Phix
include builtins\libcurl.e
curl_global_init()
atom curl = curl_easy_init()
curl_easy_setopt(curl, CURLOPT_URL, "http://rosettacode.org/robots.txt")
object res = curl_easy_perform_ex(curl)
curl_easy_cleanup(curl)
curl_global_cleanup()

puts(1,res)
```

```txt

User-agent: *
Allow: /mw/images/
Allow: /mw/skins/
Allow: /mw/title.png
Allow: /mw/resources/
Disallow: /w/
Disallow: /mw/
Disallow: /wiki/Special:

```



## PHP


```php

readfile("http://www.rosettacode.org");

```



## PicoLisp


```PicoLisp

(load "@lib/http.l")

(client "rosettacode.org" 80 NIL       # Connect to rosettacode
   (out NIL (echo)) )                  # Echo to standard output

```



## Pike


```pike

write("%s",Protocols.HTTP.get_url_data("http://www.rosettacode.org"));

```



## PowerShell


```powershell

$wc = New-Object Net.WebClient
$wc.DownloadString('http://www.rosettacode.org')

```



## Prolog

Works with SWI-Prolog and library http/http_open. (Extract from the doc).


```Prolog

:- use_module(library( http/http_open )).

http :-
	http_open('http://www.rosettacode.org/',In, []),
	copy_stream_data(In, user_output),
	close(In).

```



## PureBasic


```PureBasic

InitNetwork()
OpenConsole()

tmpdir$   = GetTemporaryDirectory()
filename$ = tmpdir$ + "PB_tempfile" + Str(Random(200000)) + ".html"

If ReceiveHTTPFile("http://rosettacode.org/wiki/Main_Page", filename$)
  If ReadFile(1, filename$)
    Repeat
      PrintN(ReadString(1))
    Until Eof(1)
    Input()
    ; to prevent console from closing if on windows
    CloseFile(1)
  EndIf
  DeleteFile(filename$)
EndIf

```


Another solution using general networking commands

```PureBasic

InitNetwork()
OpenConsole()
id = OpenNetworkConnection("rosettacode.org", 80)
SendNetworkString(id, "GET /wiki/Main_Page HTTP/1.1" + Chr(10) + "Host: rosettacode.org" + Chr(10) + Chr(10))
Repeat
  If NetworkClientEvent(id) = 2
    a$ = Space(1000)
    ReceiveNetworkData(id, @a$, 1000)
    out$ + a$
  EndIf
Until FindString(out$, "</html>", 0)
PrintN(out$)
; next line only to prevent console from closing on Windows
Input()

```


Of course you could use wget too.


## Python


;Python 3:
Using the [http://docs.python.org/py3k/library/urllib.request.html urllib.request] module.

```python

import urllib.request
print(urllib.request.urlopen("http://rosettacode.org").read())

```


Using a more low-level [https://docs.python.org/3/library/http.client.html http.client] library.

```python

from http.client import HTTPConnection
conn = HTTPConnection("example.com")
# If you need to use set_tunnel, do so here.
conn.request("GET", "/")
# Alternatively, you can use connect(), followed by the putrequest, putheader and endheaders functions.
result = conn.getresponse()
r1 = result.read() # This retrieves the entire contents.

```


;Python 2:
Using the [http://docs.python.org/library/urllib.html urllib] library.

```python

import urllib
print urllib.urlopen("http://rosettacode.org").read()

```


Using the [http://docs.python.org/library/urllib2.html urllib2] library.

```python

import urllib2
print urllib2.urlopen("http://rosettacode.org").read()

```



```Python

import requests
print(requests.get("http://rosettacode.org").text)

```



## R

First, retrieve the webpage.


```R

library(RCurl)
webpage <- getURL("http://rosettacode.org")

#If you are linking to a page that no longer exists and need to follow the redirect, use followlocation=TRUE
webpage <- getURL("http://www.rosettacode.org", .opts=list(followlocation=TRUE))

#If you are behind a proxy server, you will need to use something like:
webpage <- getURL("http://rosettacode.org",
   .opts=list(proxy="123.123.123.123", proxyusername="domain\\username", proxypassword="mypassword", proxyport=8080))
#Don't forget that backslashes in your username or password need to be escaped!

```


Now parse the html code into a tree and print the html.


```R

library(XML)
pagetree <- htmlTreeParse(webpage )
pagetree$children$html

```



## Racket


```Racket

#lang racket
(require net/url)
(copy-port (get-pure-port (string->url "http://www.rosettacode.org")
                          #:redirections 100)
           (current-output-port))

```



## REALbasic

REALBasic provides an HTTPSocket class for handling HTTP connections. The 'Get' method of the HTTPSocket is overloaded and can download data to a file or return data as a string, in both cases a timeout argument can be passed.

```REALbasic

  Dim sock As New HTTPSocket
  Print(sock.Get("http://www.rosettacode.org", 10))  //set the timeout period to 10 seconds.

```



## REBOL


```REBOL

print read http://rosettacode.org

```



## REXX


This script takes an URL as an argument and displays the content on the terminal.  It uses the external program `curl` to perform both the acquisition of the data and the display.


```Rexx
/* ft=rexx */
/* GET2.RX - Display contents of an URL on the terminal. */
/* Usage: rexx get.rx http://rosettacode.org             */
parse arg url .
'curl' url
```


A simple change to the script will redirect the output to an internal variable for internal processing.  (Our "internal processing" in this case is to display it.)


```Rexx
/* ft=rexx */
/* GET2.RX - Display contents of an URL on the terminal. */
/* Usage: rexx get2.rx http://rosettacode.org            */
parse arg url .
address system 'curl' url with output stem stuff.
do i = 1 to stuff.0
  say stuff.i
end
```


Another simple change redirects the output to another external program like a shell pipe.


```Rexx
/* ft=rexx */
/* GET3.RX - Display contents of an URL on the terminal. */
/* Usage: rexx get3.rx http://rosettacode.org            */
parse arg url .
address system 'curl' url with output fifo ''
address system 'more' with input fifo ''
```



## RLaB

RLaB supports HTTP/FTP through its Read/Write facilities, which are organized around the concept
of Universal Resource Locator (URL),
:''protocol://address''

RLaB accepts the following values for ''protocol'':
:1. ''file'' or omitted, for generic text files or files in native binary format (partially compatible with ''matlab'' binary format);

:2. ''h5'' or ''hdf5'' for files that use Hierarhical Data Format 5 (HDF5) version 1.8.0, and later. Here ''protocol'' can be omitted while ''address'' has to end with ''.h5'' (file extension);

:3. ''http'', ''https'', or ''ftp'' for accessing the data and files on web- and ftp-servers;

:4. ''tcp'', for accessing sockets over tcp/ip protocol;

:5. ''serial'', for accessing serial port on Un*x type systems.

For these URLs RLaB provides an internal book-keeping: It keeps track of
the open ones and, say, upon quitting, closes them and releases the internal
resources it allocated for managing them.

For accessing URLs on world wide web RLaB implements the library cURL (libcurl) [http://curl.haxx.se] and its "easy" interface.

This said, this is how one would download financial data for Pfeizer from
Yahoo [http://ichart.finance.yahoo.com/table.csv?s=PFE&a=00&b=4&c=1982&d=00&e=10&f=2010&g=d&ignore=.csv].


```RLaB

// get cvs data from Yahoo for Pfeizer (PFE)
url="http://ichart.finance.yahoo.com/table.csv?s=PFE&a=00&b=4&c=1982&d=00&e=10&f=2010&g=d&ignore=.csv";

opt = <<>>;
// opt.CURLOPT_PROXY     = "your.proxy.here";
// opt.CURLOPT_PROXYPORT = YOURPROXYPORT;
// opt.CURLOPT_PROXYTYPE = "http";
open(url, opt);
x = readm(url);
close (url);

```



## Ring


```ring

See download("http://rosettacode.org")

```



## Ruby

The simple way loads the entire content into memory, then prints it.


```ruby

require 'open-uri'

print open("http://rosettacode.org") {|f| f.read}

```


If the content might be large, the better way uses FileUtils.copy_stream.


```ruby

require 'fileutils'
require 'open-uri'

open("http://rosettacode.org/") {|f| FileUtils.copy_stream(f, $stdout)}

```


## Run BASIC


```runbasic
print httpget$("http://rosettacode.org/wiki/Main_Page")
```



## Rust

Cargo.toml

```toml

[dependencies]
hyper = "0.6"

```

src/main.rs

```rust

//cargo-deps: hyper="0.6"
// The above line can be used with cargo-script which makes cargo's dependency handling more convenient for small programs
extern crate hyper;

use std::io::Read;
use hyper::client::Client;

fn main() {
    let client = Client::new();
    let mut resp = client.get("http://rosettacode.org").send().unwrap();
    let mut body = String::new();
    resp.read_to_string(&mut body).unwrap();
    println!("{}", body);
}

```



## Scala

```scala
import scala.io.Source

object HttpTest extends App {
  System.setProperty("http.agent", "*")

  Source.fromURL("http://www.rosettacode.org").getLines.foreach(println)
}
```



## Scheme

```scheme

; Use the regular expression module to parse the url (included with Guile)
(use-modules (ice-9 regex))

; Set the url and parse the hostname, port, and path into variables
(define url "http://www.rosettacode.org/wiki/HTTP")
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

; Display the received HTML
        (do ((c (read-char s) (read-char s))) ((eof-object? c))
                (display c)))

```

Using the [http://api.call-cc.org/doc/http-client http-client] library, this is trivial.

```scheme

(use http-client)
(print
  (with-input-from-request "http://google.com/"
                           #f read-string))

```



## Seed7

The [http://seed7.sourceforge.net/libraries/gethttp.htm gethttp.s7i] library
contains the function [http://seed7.sourceforge.net/libraries/gethttp.htm#getHttp%28in_string%29 getHttp],
which gets data specified by an URL using the HTTP protocol.


```seed7

$ include "seed7_05.s7i";
  include "gethttp.s7i";

const proc: main is func
  begin
    writeln(getHttp("www.rosettacode.org"));
  end func;
```



## Sidef

Sidef can load and use Perl modules:

```ruby
func get(url) {
    var lwp = (
        try   { require('LWP::UserAgent') }
        catch { warn "'LWP::UserAgent' is not installed!"; return nil }
    )
    var ua = lwp.new(agent => 'Mozilla/5.0')
    if (var resp = ua.get(url); resp.is_success) {
        return resp.decoded_content
    }
    return nil
}

print get("http://rosettacode.org")
```



## SNOBOL4

```snobol
-include "tcp.sno"
	tcp.open(.conn,'www.rosettacode.org','http')	:s(cont1)
	terminal = "cannot open"	:(end)
cont1	conn = "GET http://rosettacode.org/wiki/Main_Page HTTP/1.0" char(10) char(10)
while	output = conn	:s(while)
	tcp.close(.conn)
end

```



## Smalltalk

```smalltalk

Transcript show: 'http://rosettacode.org' asUrl retrieveContents contentStream.

```


## Swift


```Swift
import Foundation

let request = NSURLRequest(URL: NSURL(string: "http://rosettacode.org/")!)

// Using trailing closure
NSURLConnection.sendAsynchronousRequest(request, queue: NSOperationQueue()) {res, data, err in

    // data is binary
    if (data != nil) {
        let string = NSString(data: data!, encoding: NSUTF8StringEncoding)
        println(string)
    }
}

CFRunLoopRun() // dispatch
```



## Tcl

Note that the <code>http</code> package is distributed as part of Tcl.


```tcl

package require http
set request [http::geturl "http://www.rosettacode.org"]
puts [http::data $request]
http::cleanup $request
```


## TSE SAL


```TSE SAL


DLL "<urlmon.dll>"
 INTEGER PROC FNUrlGetSourceApiI(
  INTEGER lpunknown,
  STRING urlS : CSTRVAL,
  STRING filenameS : CSTRVAL,
  INTEGER dword,
  INTEGER tlpbindstatuscallback
) : "URLDownloadToFileA"
END

// library: url: get: source <description></description> <version control></version control> <version>1.0.0.0.3</version> (filenamemacro=geturgso.s) [kn, ri, su, 13-04-2008 05:12:53]
PROC PROCUrlGetSource( STRING urlS, STRING filenameS )
 FNUrlGetSourceApiI( 0, urlS, filenameS, 0, 0 )
END

PROC Main()
STRING s1[255] = "http://www.google.com/index.html"
STRING s2[255] = "c:\temp\ddd.txt"
IF ( NOT ( Ask( "url: get: source: urlS = ", s1, _EDIT_HISTORY_ ) ) AND ( Length( s1 ) > 0 ) ) RETURN() ENDIF
IF ( NOT ( AskFilename( "url: get: source: filenameS = ", s2, _DEFAULT_, _EDIT_HISTORY_ ) ) AND ( Length( s2 ) > 0 ) ) RETURN() ENDIF
 PROCUrlGetSource( s1, s2 )
 EditFile( s2 )
END


```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
SET DATEN = REQUEST ("http://www.rosettacode.org")
*{daten}

```



## UNIX Shell


```bash
curl -s -L http://rosettacode.org/
```



```bash
lynx -source http://rosettacode.org/
```



```bash
wget -O - -q http://rosettacode.org/
```



```bash
lftp -c "cat http://rosettacode.org/"
```


```bash
ftp -o - http://rosettacode.org 2>/dev/null
```



## VBScript

Based on code at [http://itknowledgeexchange.techtarget.com/vbscript-systems-administrator/how-to-retrieve-html-web-pages-with-vbscript-via-the-microsoftxmlhttp-object/ How to retrieve HTML web pages with VBScript via the Microsoft.XmlHttp object]

```vb

Option Explicit

Const sURL="http://rosettacode.org/"

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

```vb
Sub Main()
Dim HttpReq As WinHttp.WinHttpRequest
'  in the "references" dialog of the IDE, check
'  "Microsoft WinHTTP Services, version 5.1" (winhttp.dll)
Const HTTPREQUEST_PROXYSETTING_PROXY As Long = 2
#Const USE_PROXY = 1
  Set HttpReq = New WinHttp.WinHttpRequest
  HttpReq.Open "GET", "http://rosettacode.org/robots.txt"
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
Dim content As String = client.DownloadString("http://www.google.com")
Console.WriteLine(content)

```



## zkl

File htmlGet.zkl. This uses HTTP/1.0 Protocol to avoid chunked data. Or use cURL (see https example).

```zkl
url := ask(0,"URL: ");

host := url;
dir  := "/";
port := 80;
if (n := url.find("/"))  { dir  = url[n,*];    host = url[0,n];  }
if (n := host.find(":")) { port = host[n+1,*]; host = host[0,n]; }

get := "GET %s HTTP/1.0\r\nHost: %s:%s\r\n\r\n".fmt(dir,host,port.toInt());
println("-->",get);
server := Network.TCPClientSocket.connectTo(host,port);
server.write(get);
data := server.read(True);
println(data.text);
```

zkl htmlGet.zkl rosettacode.org/wiki/HTTP
```txt

-->GET /wiki/HTTP HTTP/1.0
Host: rosettacode.org:80


HTTP/1.1 200 OK
Server: cloudflare-nginx
Date: Tue, 11 Mar 2014 08:31:43 GMT
Content-Type: text/html; charset=UTF-8
Connection: close
Set-Cookie:XXX
23:50:00 GMT; path=/; domain=.rosettacode.org; HttpOnly
X-Powered-By: PHP/5.3.3-7+squeeze18
X-Content-Type-Options: nosniff
Content-Language: en
ETag: W/"rosettacode:pcache:idhash:3055-0!1!0!!en!2--20140227082903"
Vary: Accept-Encoding,Cookie
Cache-Control: s-maxage=86400, must-revalidate, max-age=0
Last-Modified: Thu, 27 Feb 2014 08:29:03 GMT
Age: 86011
X-Cache: HIT from prgmr2.rosettacode.org
X-Cache-Lookup: HIT from prgmr2.rosettacode.org:80
Via: 1.0 prgmr2.rosettacode.org (squid/3.1.6)
CF-RAY: 109665b7e92a012c-SJC

<!DOCTYPE html>
<html lang="en" dir="ltr" class="client-nojs">
<head>
<title>HTTP - Rosetta Code</title>
...

```



## Zsh


```zsh

zmodload zsh/net/tcp
ztcp example.com 80
fd=$REPLY
print -l -u $fd -- 'GET / HTTP/1.1' 'Host: example.com' ''
while read -u $fd -r -e -t 1; do; :; done
ztcp -c $fd

```


