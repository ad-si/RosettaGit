+++
title = "Hello world/Web server"
description = ""
date = 2019-10-06T10:01:50Z
aliases = []
[extra]
id = 10000
[taxonomies]
categories = []
tags = []
+++

{{task|Networking and Web Interaction}} [[Category:Web]]
{{omit from|GUISS}}
{{omit from|Locomotive Basic|No sockets}}
{{omit from|Lotus 123 Macro Scripting}}
{{omit from|Maxima}}
{{omit from|ML/I|No sockets}}
{{omit from|Retro}}
{{omit from|SQL PL|It does not listen any port different to the database server and it does not has daemons - No sockets}}
{{omit from|TI-83 BASIC}}
{{omit from|ZX Spectrum Basic|No sockets}}

The browser is the new [[GUI]] !


;Task:
Serve our standard text   <big><big><code>Goodbye, World!</code></big></big>   to   http://localhost:8080/   so that it can be viewed with a web browser.

The provided solution must start or implement a server that accepts multiple client connections and serves text as requested.

Note that starting a web browser or opening a new window with this URL
is not part of the task.

Additionally, it is permissible to serve the provided page as a plain text file (there is no requirement to serve properly formatted [[HTML]] here).

The browser will generally do the right thing with simple text like this.





## Ada

{{libheader|AWS}}
Uses many defaults, such as 5 max simultaneous connections.

```ada

with AWS; use AWS;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with Ada.Text_IO; use Ada.Text_IO;
procedure HelloHTTP is
   function CB (Request : Status.Data) return Response.Data is
      pragma Unreferenced (Request);
   begin
      return Response.Build ("text/html", "Hello world!");
   end CB;
   TheServer : Server.HTTP;
   ch : Character;
begin
   Server.Start (TheServer, "Rosettacode",
      Callback => CB'Unrestricted_Access, Port => 8080);
   Put_Line ("Press any key to quit."); Get_Immediate (ch);
   Server.Shutdown (TheServer);
end HelloHTTP;

```



## Aime

Goodbye, world! with random colors and socket polling:

```aime
void
serve(dispatch w, file s, list colors)
{
    file i, o;
    date d;

    accept(i, o, s, 0);
    f_(o, "HTTP/1.1 200 OK\n"
"Content-Type: text/html; charset=UTF-8\n\n"
"<!DOCTYPE html><html><head><title>Bye-bye baby bye-bye</title>"
"<style>body { background-color: #111 }"
"h1 { font-size:4cm; text-align: center; color: black;"
" text-shadow: 0 0 2mm ", colors[drand(3)], "}</style></head>"
"<body><h1>Goodbye, world!</h1></body></html>\n");

    # chrome won't show the page if we close right away.  we'll close in 2s.
    d.now;
    d.offset(2, 0);
    w_schedule(w, d, f_close, i);
}


integer
main(void)
{
    dispatch w;
    file s;

    tcpip_listen(s, 8080, 0);
    w_watch(w, s, serve, w, s, list("blue", "green", "red", "yellow"));
    w_press(w);

    return 0;
}
```

Or simply:

```aime
file i, o, s;

tcpip_listen(s, 8080, 0);
while (1) {
    accept(i, o, s, 0);
    f_(o, "HTTP/1.1 200 OK\n"
"Content-Type: text/html; charset=UTF-8\n\n"
"<!DOCTYPE html><html><head><title>baby bye-bye</title></head>"
"<body><h1>Goodbye, world!</h1></body></html>\n");
    f_flush(o);
}
```



## AntLang

In plain AntLang:

```AntLang
serv: httprun[8080; {"HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nGoodbye, World!"}]
```


Using ALC:

```AntLang
load["handlers.ant"]
serv: httprun[8080; {httpwrap["Goodbye, World!"]}]
```


To close the server:

```AntLang
kill[serv]
```



## AutoIt


```AutoIt

TCPStartup()
$socket = TCPListen("0.0.0.0",8080)
$string = "Goodbye, World!"
While 1
   Do
	  $newConnection = TCPAccept($socket)
	  Sleep(1)
   Until $newConnection <> -1
   $content = TCPRecv($newConnection, 2048)
   If StringLen($content) > 0 Then
	  TCPSend($newConnection, Binary("HTTP/1.1 200 OK" & @CRLF))
	  TCPSend($newConnection, Binary("Content-Type: text/html" & @CRLF))
	  TCPSend($newConnection, Binary("Content-Length: "& BinaryLen($string) & @CRLF & @CRLF))
	  TCPSend($newConnection, $string)
   EndIf
   TCPCloseSocket($newConnection)
WEnd

```



## AWK

With GNU AWK (gawk) a simple web server can be implemented.

The example is taken from
[http://www.gnu.org/software/gawk/manual/gawkinet/gawkinet.html#Primitive-Service]
(Documentation is licensed under GNU Free Documentation License, Version 1.3)

```AWK
#!/usr/bin/gawk -f
     BEGIN {
       RS = ORS = "\r\n"
       HttpService = "/inet/tcp/8080/0/0"
       Hello = "<HTML><HEAD>" \
               "<TITLE>A Famous Greeting</TITLE></HEAD>" \
               "<BODY><H1>Hello, world</H1></BODY></HTML>"
       Len = length(Hello) + length(ORS)
       print "HTTP/1.0 200 OK"          |& HttpService
       print "Content-Length: " Len ORS |& HttpService
       print Hello                      |& HttpService
       while ((HttpService |& getline) > 0)
          continue;
       close(HttpService)
     }
```



## BaCon


```qbasic
' Define HTTP constants
CONST New$ = CHR$(13) & NL$
CONST Sep$ = CHR$(13) & NL$ & CHR$(13) & NL$
CONST Msg$ = "<html><head>BaCon web greeting</head><body><h2>Goodbye, World!</h2></body></html>"

' Get our IP
Ip$ = "localhost"
PRINT "Connect from browser '", Ip$, ":8080'."

' Ignore child signals to avoid zombie processes
SIGNAL SIG_IGN, SIGCHLD

' Keep receiving requests
WHILE TRUE

    ' Open listening port
    OPEN Ip$ & ":8080" FOR SERVER AS mynet

    ' Incoming connection -> create background process
    spawn = FORK

    ' We are in the child
    IF spawn = 0 THEN

        ' Get the request
        REPEAT
            RECEIVE dat$ FROM mynet
            PRINT dat$
        UNTIL RIGHT$(dat$, 4) = Sep$

        ' Reply that we're OK
        SEND "HTTP/1.1 200 Ok" & New$ & "Content-Length: " & STR$(LEN(Msg$)) & Sep$ & Msg$ TO mynet

        ' Close connection
        CLOSE SERVER mynet

        ' End this process
        END

        ' We are in the parent
    ELIF spawn > 0 THEN

        ' Close connection in parent
        CLOSE SERVER mynet

    ENDIF
WEND

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
This explicitly supports multiple concurrent connections.

```bbcbasic
      INSTALL @lib$+"SOCKLIB"
      PROC_initsockets

      maxSess% = 8
      DIM sock%(maxSess%-1), rcvd$(maxSess%-1), Buffer% 255

      ON ERROR PRINT REPORT$ : PROC_exitsockets : END
      ON CLOSE PROC_exitsockets : QUIT

      port$ = "8080"
      host$ = FN_gethostname
      PRINT "Host name is " host$

      listen% = FN_tcplisten(host$, port$)
      PRINT "Listening on port ";port$

      REPEAT
        socket% = FN_check_connection(listen%)
        IF socket% THEN
          FOR i% = 0 TO maxSess%-1
            IF sock%(i%) = 0 THEN
              sock%(i%) = socket%
              rcvd$(i%) = ""
              PRINT "Connection on socket "; sock%(i%) " opened"
              EXIT FOR
            ENDIF
          NEXT i%
          listen% = FN_tcplisten(host$, port$)
        ENDIF

        FOR i% = 0 TO maxSess%-1
          IF sock%(i%) THEN
            res% = FN_readsocket(sock%(i%), Buffer%, 256)
            IF res% >= 0 THEN
              Buffer%?res% = 0
              rcvd$(i%) += $$Buffer%
              IF LEFT$(rcvd$(i%),4) = "GET " AND ( \
              \ RIGHT$(rcvd$(i%),4) = CHR$13+CHR$10+CHR$13+CHR$10 OR \
              \ RIGHT$(rcvd$(i%),4) = CHR$10+CHR$13+CHR$10+CHR$13 OR \
              \ RIGHT$(rcvd$(i%),2) = CHR$10+CHR$10 ) THEN
                rcvd$(i%) = ""
                IF FN_writelinesocket(sock%(i%), "HTTP/1.0 200 OK")
                IF FN_writelinesocket(sock%(i%), "Content-type: text/html")
                IF FN_writelinesocket(sock%(i%), "")
                IF FN_writelinesocket(sock%(i%), "<html><head><title>Hello World!</title></head>")
                IF FN_writelinesocket(sock%(i%), "<body><h1>Hello World!</h1>")
                IF FN_writelinesocket(sock%(i%), "</body></html>")
                PROC_closesocket(sock%(i%))
                PRINT "Connection on socket " ; sock%(i%) " closed (local)"
                sock%(i%) = 0
              ENDIF
            ELSE
              PROC_closesocket(sock%(i%))
              PRINT "Connection on socket " ; sock%(i%) " closed (remote)"
              sock%(i%) = 0
            ENDIF
          ENDIF
        NEXT i%

        WAIT 0
      UNTIL FALSE
      END
```



## C

This is, um, slightly longer than what other languages would be.

```c
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <err.h>

char response[] = "HTTP/1.1 200 OK\r\n"
"Content-Type: text/html; charset=UTF-8\r\n\r\n"
"<!DOCTYPE html><html><head><title>Bye-bye baby bye-bye</title>"
"<style>body { background-color: #111 }"
"h1 { font-size:4cm; text-align: center; color: black;"
" text-shadow: 0 0 2mm red}</style></head>"
"<body><h1>Goodbye, world!</h1></body></html>\r\n";

int main()
{
  int one = 1, client_fd;
  struct sockaddr_in svr_addr, cli_addr;
  socklen_t sin_len = sizeof(cli_addr);

  int sock = socket(AF_INET, SOCK_STREAM, 0);
  if (sock < 0)
    err(1, "can't open socket");

  setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(int));

  int port = 8080;
  svr_addr.sin_family = AF_INET;
  svr_addr.sin_addr.s_addr = INADDR_ANY;
  svr_addr.sin_port = htons(port);

  if (bind(sock, (struct sockaddr *) &svr_addr, sizeof(svr_addr)) == -1) {
    close(sock);
    err(1, "Can't bind");
  }

  listen(sock, 5);
  while (1) {
    client_fd = accept(sock, (struct sockaddr *) &cli_addr, &sin_len);
    printf("got connection\n");

    if (client_fd == -1) {
      perror("Can't accept");
      continue;
    }

    write(client_fd, response, sizeof(response) - 1); /*-1:'\0'*/
    close(client_fd);
  }
}
```



## C++

C version compiles as C++ (known for G++ on Linux)
## C#

```c#
using System.Text;
using System.Net.Sockets;
using System.Net;

namespace WebServer
{
    class GoodByeWorld
    {
        static void Main(string[] args)
        {
            const string msg = "<html>\n<body>\nGoodbye, world!\n</body>\n</html>\n";
            const int port = 8080;
            bool serverRunning = true;

            TcpListener tcpListener = new TcpListener(IPAddress.Any, port);
            tcpListener.Start();

            while (serverRunning)
            {
                Socket socketConnection = tcpListener.AcceptSocket();
                byte[] bMsg = Encoding.ASCII.GetBytes(msg.ToCharArray(), 0, (int)msg.Length);
                socketConnection.Send(bMsg);
                socketConnection.Disconnect(true);
            }
        }
    }
}
```

{{works with|NancyFX}}
```c#
namespace Webserver
{
  using System;
  using Nancy;
  using Nancy.Hosting.Self;

  public class HelloWorldModule : NancyModule
  {
    public HelloWorldModule()
    {
      this.Get["/"] = parameters => "Goodbye, world!";
    }

    public static void Main()
    {
      var uri = new Uri("http://localhost:8080");
      using (var host = new NancyHost(uri))
      {
        host.Start();
        Console.WriteLine("Web server is now running!");
        Console.WriteLine("Press 'Enter' to exit.");
        Console.ReadLine();
      }
    }
  }
}
```



## Clojure

Taken from: [https://github.com/weavejester/compojure/wiki/Getting-Started Compojure's Getting Started doc].

<tt>> lein new compojure goodbye-world</tt>

File: src/goodbye_world/handler.clj

```clojure
(ns goodbye-world.handler
  (:require [compojure.core :refer :all]
            [compojure.handler :as handler]
            [compojure.route :as route]))

(defroutes app-routes
  (GET "/" [] "Goodbye, World!")
  (route/resources "/")
  (route/not-found "Not Found"))

(def app
  (handler/site app-routes))
```


To start up the server on port 8080, run the following from the project's root:

<tt>> lein ring server-headless 8080</tt>


## Common Lisp


Here's the example with a pre-built server:


```lisp
(ql:quickload :hunchentoot)
(defpackage :hello-web (:use :cl :hunchentoot))
(in-package :hello-web)

(define-easy-handler (hello :uri "/") () "Goodbye, World!")

(defparameter *server* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8080)))
```


----

Here's an example of doing everything manually


```lisp
(ql:quickload :usocket)
(defpackage :hello-web-manual (:use :cl :usocket))
(in-package :hello-web-manual)

(defun crlf (&optional (stream *standard-output*))
  (write-char #\return stream)
  (write-char #\linefeed stream)
  (values))

(defun ln (string &optional (stream *standard-output*))
  (write-string string stream)
  (crlf stream))

(defun read-all (stream)
  (loop for char = (read-char-no-hang stream nil :eof)
     until (or (null char) (eq char :eof)) collect char into msg
     finally (return (values msg char))))

(defun serve (port &optional (log-stream *standard-output*))
  (let ((connections (list (socket-listen "127.0.0.1" port :reuse-address t))))
    (unwind-protect
	 (loop
	    (loop for ready in (wait-for-input connections :ready-only t)
	       do (if (typep ready 'stream-server-usocket)
		      (push (socket-accept ready) connections)
		      (let* ((stream (socket-stream ready)))
			(read-all stream)
			(format log-stream "Got message...~%")
			(mapc (lambda (line) (ln line stream))
			      (list "HTTP/1.1 200 OK"
				    "Content-Type: text/plain; charset=UTF-8"
				    ""
				    "Hello world!"))
			(socket-close ready)
			(setf connections (remove ready connections))))))
      (loop for c in connections do (loop while (socket-close c))))))

(serve 8080)
```



## Crystal



```crystal

require "http/server"

server = HTTP::Server.new(8080) do |context|
context.response.print "Goodbye World"
end

server.listen

```



## D

Using sockets only, also shows use of heredoc syntax, std.array.replace,
and casting to bool to satisfy the while conditional.


```D

import std.socket, std.array;

ushort port = 8080;

void main() {
  Socket listener = new TcpSocket;
  listener.bind(new InternetAddress(port));
  listener.listen(10);

  Socket currSock;

  while(cast(bool)(currSock = listener.accept())) {
    currSock.sendTo(replace(q"EOF
HTTP/1.1 200 OK
Content-Type: text/html; charset=UTF-8

<html>
  <head><title>Hello, world!</title></head>
  <body>Hello, world!</body>
</html>
EOF", "\n", "\r\n"));
    currSock.close();
  }
}

```



## Dart


```d
import 'dart:io';

main() async {
  var server = await HttpServer.bind('127.0.0.1', 8080);

  await for (HttpRequest request in server) {
    request.response
      ..write('Hello, world')
      ..close();
  }
}
```



## Delphi


```Delphi
program HelloWorldWebServer;

{$APPTYPE CONSOLE}

uses SysUtils, IdContext, IdCustomHTTPServer, IdHTTPServer;

type
  TWebServer = class
  private
    FHTTPServer: TIdHTTPServer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure HTTPServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  end;

constructor TWebServer.Create;
begin
  FHTTPServer := TIdHTTPServer.Create(nil);
  FHTTPServer.DefaultPort := 8080;
  FHTTPServer.OnCommandGet := HTTPServerCommandGet;
  FHTTPServer.Active := True;
end;

destructor TWebServer.Destroy;
begin
  FHTTPServer.Active := False;
  FHTTPServer.Free;
  inherited Destroy;
end;

procedure TWebServer.HTTPServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  AResponseInfo.ContentText := 'Goodbye, World!';
end;

var
  lWebServer: TWebServer;
begin
  lWebServer := TWebServer.Create;
  try
    Writeln('Delphi Hello world/Web server ');
    Writeln('Press Enter to quit');
    Readln;
  finally
    lWebServer.Free;
  end;
end.
```


=={{header|Dylan.NET|Dylan.NET}}==

```Dylan.NET

//compile with dylan.NET 11.5.1.2 or later!!
#refstdasm "mscorlib.dll"
#refstdasm "System.dll"

import System.Text
import System.Net.Sockets
import System.Net

assembly helloweb exe
ver 1.1.0.0

namespace WebServer

    class public GoodByeWorld

        method public static void main(var args as string[])

            var msg as string = c"<html>\n<body>\nGoodbye, world!\n</body>\n</html>\n"
            var port as integer = 8080
            var serverRunning as boolean = true

            var tcpListener as TcpListener = new TcpListener(IPAddress::Any, port)
            tcpListener::Start()

            do while serverRunning
                var socketConnection as Socket = tcpListener::AcceptSocket()
                var bMsg as byte[] = Encoding::get_ASCII()::GetBytes(msg::ToCharArray(), 0, msg::get_Length())
                socketConnection::Send(bMsg)
                socketConnection::Disconnect(true)
            end do

        end method

    end class

end namespace

```



## Erlang

Using builtin HTTP server with call back to do/1.
It only lasts 30 seconds (30000 milliseconds), then it is stopped.
I fail to see how a longer time will serve any purpose.


```Erlang

-module( hello_world_web_server ).

-export( [do/1, httpd_start/2, httpd_stop/1, task/0] ).

do( _Data ) ->
  {proceed, [{response,{200,"Goodbye, World!"}}]}.

httpd_start( Port, Module ) ->
  Arguments = [{bind_address, "localhost"}, {port, Port}, {ipfamily, inet},
    {modules, [Module]},
    {server_name,erlang:atom_to_list(Module)}, {server_root,"."}, {document_root,"."}],
  {ok, Pid} = inets:start( httpd, Arguments, stand_alone ),
  Pid.

httpd_stop( Pid ) ->
  inets:stop( stand_alone, Pid ).

task() ->
  Pid = httpd_start( 8080, ?MODULE ),
  timer:sleep( 30000 ),
  httpd_stop( Pid ).

```



## Fantom


```fantom
using web
using wisp

const class HelloMod : WebMod // provides the content
{
  override Void onGet ()
  {
    res.headers["Content-Type"] = "text/plain; charset=utf-8"
    res.out.print ("Goodbye, World!")
  }
}

class HelloWeb
{
  Void main ()
  {
    WispService // creates the web service
    {
      port = 8080
      root = HelloMod()
    }.start

    while (true) {} // stay running
  }
}
```



## Fortran

There is no network library in fortran. Use C interoperability and some C compatible library or just start node.js simple web server:

```fortran

program http_example
   implicit none
   character (len=:), allocatable :: code
   character (len=:), allocatable :: command
   logical :: waitForProcess

   ! Execute a Node.js code
   code = "const http = require('http'); http.createServer((req, res) => &
   {res.end('Hello World from a Node.js server started from Fortran!')}).listen(8080);"

   command = 'node -e "' // code // '"'
   call execute_command_line (command, wait=waitForProcess)

end program http_example

```



## Free Pascal


```pascal
program HelloWorldServer;
{$mode objfpc}{$H+}
uses
  Classes, fphttpserver;

Type
  TTestHTTPServer = Class(TFPHTTPServer)
  public
    procedure HandleRequest(Var ARequest: TFPHTTPConnectionRequest;
                            Var AResponse : TFPHTTPConnectionResponse); override;
  end;

Var
  Serv : TTestHTTPServer;

procedure TTestHTTPServer.HandleRequest(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
Var
  F : TStringStream;
begin
  F:=TStringStream.Create('Hello,World!');
  try
    AResponse.ContentLength:=F.Size;
    AResponse.ContentStream:=F;
    AResponse.SendContent;
    AResponse.ContentStream:=Nil;
  finally
    F.Free;
  end;
end;

begin
  Serv:=TTestHTTPServer.Create(Nil);
  try
    Serv.Threaded:=False;
    Serv.Port:=8080;
    Serv.AcceptIdleTimeout:=1000;
    Serv.Active:=True;
  finally
    Serv.Free;
  end;
end.
```



## FunL


```funl
native java.io.PrintWriter
native java.net.ServerSocket

val port = 8080
val listener = ServerSocket( port )

printf( 'Listening at port %1$d\n', port )

forever
  socket = listener.accept()
  PrintWriter( socket.getOutputStream(), true ).println( 'hello world' )
  socket.shutdownOutput()
  socket.close()
```



## Gastona

A minimal graphical user interface, a console, is included to allow
following the server activity and also being able to quit it by closing the window.
But it is not stritctly needed, just the unit #listix# would do the job.

```gastona
#javaj#

   <frames> oConsole

#listix#

   <main>
      MICOHTTP, START, myServer, 8080

   <GET />
      //<html><body>
      //  Goodbye world!
      //</body></html>

```



## Genie


```genie
/**
 * Based on https://wiki.gnome.org/Projects/Genie/GIONetworkingSample
 * Based on an example of Jezra Lickter http://hoof.jezra.net/snip/nV
 *
 * valac --pkg gio-2.0 --pkg gee-0.8 webserver.gs
 * ./webserver
 */
[indent=8]
uses
        GLib
        Gee

init
        var ws = new WebServer()
        ws.run()

struct Request
        full_request : string
        path : string
        query : string

struct Response
        status_code : string
        content_type : string
        data : string

class WebServer

        def run()
                port : uint16 = 8080
                tss : ThreadedSocketService = new ThreadedSocketService(100)
                ia : InetAddress = new InetAddress.any(SocketFamily.IPV4)
                isaddr : InetSocketAddress = new InetSocketAddress(ia, port)
                try
                        tss.add_address(isaddr, SocketType.STREAM, SocketProtocol.TCP, null, null);
                except e : Error
                        stderr.printf("%s\n", e.message)
                        return
                // add connection handler
                tss.run.connect( connection_handler )

                ml : MainLoop = new MainLoop()
                tss.start()
                stdout.printf("Serving on port %d\n", port)
                ml.run()

        def connection_handler ( conn : SocketConnection ) : bool
                first_line : string = ""
                size : size_t = 0;
                request : Request = Request()
                dis : DataInputStream = new DataInputStream (conn.input_stream)
                dos : DataOutputStream = new DataOutputStream (conn.output_stream)
                try
                        first_line = dis.read_line(out size)
                        // here you could analyze request information
                        var parts = first_line.split(" ");
                        if parts.length > 1 do request.full_request = parts[1]
                except e : Error
                        stderr.printf("%s\n", e.message)
                response : Response = Response()
                response.status_code = "HTTP/1.1 200 OK\n"
                response.content_type = "text/html"
                response.data = "<html><body><h1>Goodbye, World!</h1></body></html>"
                serve_response ( response, dos )
                return false

        def serve_response ( response : Response, dos : DataOutputStream )
                try
                        dos.put_string (response.status_code)
                        dos.put_string ("Server: Genie Socket\n")
                        dos.put_string("Content-Type: %s\n".printf(response.content_type))
                        dos.put_string("Content-Length: %d\n".printf(response.data.length))
                        dos.put_string("\n");//this is the end of the return headers
                        dos.put_string(response.data)
                except e : Error
                        stderr.printf("%s\n", e.message)
```


{{out}}

```txt
prompt$ valac --pkg gio-2.0 --pkg gee-0.8 webserver.gs
prompt$ ./webserver
Serving on port 8080
```

Showing

```txt
prompt$ curl http://localhost:8080
<html><body><h1>Goodbye, World!</h1></body></html>
```



## Go


```go
package main

import (
  "fmt"
  "log"
  "net/http"
)

func main() {
  http.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
    fmt.Fprintln(w, "Goodbye, World!")
  })
  log.Fatal(http.ListenAndServe(":8080", nil))
}

```



## Haskell


Lightweightly concurrent "hello world" web server
using the [http://www.yesodweb.com/book/conduits conduit] stack:


```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Char8 ()
import Data.Conduit ( ($$), yield )
import Data.Conduit.Network ( ServerSettings(..), runTCPServer )

main :: IO ()
main = runTCPServer (ServerSettings 8080 "127.0.0.1") $ const (yield response $$)
  where response = "HTTP/1.0 200 OK\nContent-Length: 16\n\nGoodbye, World!\n"
```


Or using only "standard" features ([http://hackage.haskell.org/package/base base], [http://hackage.haskell.org/package/bytestring bytestring] and [http://hackage.haskell.org/package/network network] from the [http://hackage.haskell.org/platform/ Haskell Platform]):


```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Char8 ()
import Network hiding ( accept )
import Network.Socket ( accept )
import Network.Socket.ByteString ( sendAll )
import Control.Monad ( forever )
import Control.Exception ( bracket, finally )
import Control.Concurrent ( forkIO )

main :: IO ()
main = bracket (listenOn $ PortNumber 8080) sClose loop where
  loop s = forever $ forkIO . request . fst =<< accept s
  request c = sendAll c response `finally` sClose c
  response = "HTTP/1.0 200 OK\nContent-Length: 16\n\nGoodbye, World!\n"
```


Both works like this:

 $ curl http://localhost:8080/
 Goodbye, World!
 # httperf --port=8080 --num-conns=10000
 Request rate: 4549.5 req/s (0.2 ms/req)
 # httperf --port=8080 --num-conns=10000 --num-calls=2
 Request rate: 8202.5 req/s (0.1 ms/req)
 Errors: total 10000 client-timo 0 socket-timo 0 connrefused 0 connreset 10000

Or using warp ([http://hackage.haskell.org/package/warp warp] [https://wiki.haskell.org/Web/Servers#Warp warp example] [http://aosabook.org/en/posa/warp.html about warp]):


```haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import Data.Monoid

main = do
    let port = 8080
    putStrLn $ "Listening on port " ++ show port
    run port app

app req respond = respond $
    case pathInfo req of
        x -> index x

index x = responseBuilder status200 [("Content-Type", "text/plain")] $ mconcat $ map copyByteString
    [ "Hello World!\n" ]
```


Work like this:
 $ curl http://localhost:8080/
 Hello World!
 #httperf --server localhost --port 8080 --num-conns 10000 --num-calls 100
 Request rate: 43565.8 req/s (0.0 ms/req)
 #httperf --server localhost --port 8080 --num-conns 100000 --num-calls 200
 Request rate: 43902.9 req/s (0.0 ms/req)

without any errors

Comparing to [http://www.nginx.org/ nginx]:

 # httperf --num-conns=10000
 Request rate: 3613.2 req/s (0.3 ms/req)
 # httperf --num-conns=10000 --num-calls=10
 Request rate: 9952.6 req/s (0.1 ms/req)
 # httperf --num-conns=10000 --num-calls=100
 Request rate: 12341.0 req/s (0.1 ms/req)

which serve without any errors.


## Io



```io

WebRequest := Object clone do(
    handleSocket := method(aSocket,
      aSocket streamWrite("Goodbye, World!")
      aSocket close
    )
)

WebServer := Server clone do(
    setPort(8080)
    handleSocket := method(aSocket,
        WebRequest clone asyncSend(handleSocket(aSocket))
    )
)

WebServer start

```



## J

If the desire is to use the browser as a gui, the easiest thing to do
would be to [http://www.jsoftware.com/stable.htm download] [http://www.jsoftware.com/docs/help701/user/relhigh.htm j7], edit the jhs script to start on port 8080,
start jhs, visit http://127.0.0.1:8080/jijx then enter the text:

```j
'Goodbye, World!'
```

This will compute the desired result and display it (actually, it will be displayed twice since the original string will also be displayed).
This would be even simpler if you could just use the default jhs port (65001)...
Alternatively, a jhs form could be used (but this would not have the exact url structure specified).

However, if the desire is to implement the task exactly,
any of approaches at [[j:JWebServer]] might be used.

For example, here is a web server which ignores the client's request
and always returns Goodbye, World:

```j
hello=: verb define
 8080 hello y NB. try to use port 8080 by default
:
 port=: x
 require 'socket'
 coinsert 'jsocket'
 sdclose ; sdcheck sdgetsockets ''
 server=: {. ; sdcheck sdsocket ''
 sdcheck sdbind server; AF_INET; ''; port
 sdcheck sdlisten server, 1
 while. 1 do.
  while.
    server e. ready=: >{. sdcheck sdselect (sdcheck sdgetsockets ''),'';'';<1e3
  do.
    sdcheck sdaccept server
  end.
  for_socket. ready do.
   request=: ; sdcheck sdrecv socket, 65536 0
   sdcheck (socket responseFor request) sdsend socket, 0
   sdcheck sdclose socket
  end.
 end.
)

responseFor=: dyad define
 'HTTP/1.0 200 OK',CRLF,'Content-Type: text/plain',CRLF,CRLF,'Goodbye, World!',CRLF
)
```

To deploy this server, once it has been defined, run

```j
hello''
```

This version works because reasonable http requests fit in a single tcp packet.
(And note that the server waits for one tcp packet before responding.)
If parsing of the request is desired, one of the more complicated implementations at [[j:JWebServer]] should be used instead (but that's not really relevant for this task, except perhaps to require complete headers before responding, with broken browsers which send multiple tcp packets for the request).


## Java

Multiple requests will be served in the order that they reach the server,
with a queue size limit of 50 waiting requests imposed by default in the <code>ServerSocket</code> class (may be changed by adding a second positive integer argument to the <code>ServerSocket</code> constructor).

```java
import java.io.IOException;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;

public class HelloWorld{
  public static void main(String[] args) throws IOException{
    ServerSocket listener = new ServerSocket(8080);
    while(true){
      Socket sock = listener.accept();
      new PrintWriter(sock.getOutputStream(), true).
                println("Goodbye, World!");
      sock.close();
    }
  }
}
```



## JavaScript


{{works with|Node.js}}


```javascript
var http = require('http');

http.createServer(function (req, res) {
    res.writeHead(200, {'Content-Type': 'text/plain'});
    res.end('Goodbye, World!\n');
}).listen(8080, '127.0.0.1');
```


It scales:

 $ curl http://localhost:8080/
 Goodbye, World!
 # httperf --port=8080 --num-conns=10000
 Request rate: 1813.1 req/s (0.6 ms/req)
 # httperf --port=8080 --num-conns=10000 --num-calls=10
 Request rate: 4869.1 req/s (0.2 ms/req)
 # httperf --port=8080 --num-conns=10000 --num-calls=100
 Request rate: 5689.0 req/s (0.2 ms/req)

with no errors.


## Jsish

One wrinkle in this sample use of the Jsi_Websrv module that ships with Jsi.  A request needs ''page'' and not just the port.

{{out}}

```txt
prompt$ jsish
# require('Jsi_Websrv');
1.01
# Jsi_Websrv('', {server:true, port:8080, pageStr:'Goodbye, World!'});

...other terminal...

prompt$ curl http://localhost:8080/page
Goodbye, World!prompt$ curl http://localhost:8080/page
Goodbye, World!
```



## Julia

Requires the HttpServer package to have previously been installed with 'Pkg.add("HttpServer")'

```Julia
using HttpServer
server = Server() do req, res
    "Goodbye, World!"
end
run(server, 8080)

```



## Kotlin

{{trans|Java}}

```scala
import java.io.PrintWriter
import java.net.ServerSocket

fun main(args: Array<String>) {
    val listener = ServerSocket(8080)
    while(true) {
        val sock = listener.accept()
        PrintWriter(sock.outputStream, true).println("Goodbye, World!")
        sock.close()
    }
}
```



## Lasso

While Lasso has a built-in webserver you can use,
here's how you can create a basic multi-threaded webserver
of your own to complete this request:

```lasso
local(server) = net_tcp
handle => { #server->close }
#server->bind(8080) & listen & forEachAccept => {
   local(con) = #1

   split_thread => {
      handle => { #con->close }
      local(request) = ''
      // Read in the request in chunks until you have it all
      {
         #request->append(#con->readSomeBytes(8096))
         not #request->contains('\r\n\r\n')? currentCapture->restart
      }()

      local(response) = 'HTTP/1.1 200 OK\r\n\
            Content-Type: text/html; charset=UTF-8\r\n\r\n\
            Goodbye, World!'
      #con->writeBytes(bytes(#response))
   }
}
```



## Liberty BASIC

This is difficult, although possible, in Liberty BASIC,
but it's close relative ''Run BASIC'' is designed for serving webpages easily.
The task becomes simply ..

```lb
print "hello world!"
```



## Mathematica


```Mathematica
listener =
 SocketListen["127.0.0.1:8080",
  Function[{assoc},
   With[{client = assoc["SourceSocket"]},
    WriteString[client,
     "HTTP/1.0 200 OK\nContent-Length: 16\n\nGoodbye, World!\n"];
    Close[client]]]]
SystemOpen["http://127.0.0.1:8080"]
```

Clean up:

```Mathematica
DeleteObject[listener];
Close[listener["Socket"]]
```


=={{header|Modula-2}}==
This is a CGI executable:
<lang>
MODULE access;

FROM  InOut   IMPORT  WriteString, WriteLn;

BEGIN
   WriteString ("Content-Type : text/plain");
   WriteLn;
   WriteLn;
   WriteString ("Hello web wide world.");
   WriteLn
END access.

```



## NetRexx

{{Trans|Java}}

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

class RHelloWorldWebServer public

  properties public constant
    isTrue = boolean (1 == 1)
    isFalse = boolean (1 \== 1)
    greeting1 = "Goodbye, World!"
    greeting2 = '' -
      || 'HTTP/1.1 200 OK\r\n' -
      || 'Content-Type: text/html; charset=UTF-8\r\n\r\n' -
      || '<?xml version="1.0" encoding="UTF-8"?>\r\n' -
      || '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">\r\n' -
      || '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">\r\n' -
      || '<header>\r\n' -
      || '<title>Hello</title>\r\n' -
      || '<style type="text/css">body {font-family: sans-serif;}</style>\r\n' -
      || '</header>\r\n' -
      || '<body>\r\n' -
      || '<h2 style="text-align: center;">' || greeting1 || '</h2>\r\n' -
      || '</body>\r\n' -
      || '</html>\r\n' -
      || ''

  properties static inheritable
    terminate = isFalse -- TODO: provide a less draconian means to terminate the listener loop

  method main(args = String[]) public static signals IOException
    listener = ServerSocket(8080)
    loop label listener forever
      if terminate then leave listener
      sock = listener.accept()
      PrintWriter(sock.getOutputStream(), isTrue).println(greeting2)
      sock.close()
      end listener
    return

```



## Nim


```nim
import asynchttpserver, asyncdispatch

proc cb(req: Request) {.async.} =
  await req.respond(Http200, "Hello, World!")

asyncCheck newAsyncHttpServer().serve(Port(8080), cb)
runForever()

```



## Objeck


```objeck

use Net;
use Concurrency;

bundle Default {
  class GoodByeWorld {
    function : Main(args : String[]) ~ Nil {
      server := TCPSocketServer->New(8080);
      if(server->Listen(5)) {
        while(true) {
          client := server->Accept();
          client->WriteString("<html>\n<body>\nGoodbye, world!\n</body>\n</html>\n");
          client->Close();
        };
      };
      server->Close();
    }
  }
}

```



## OCaml

This code is derived from this [http://ocamlunix.forge.ocamlcore.org/sockets.html#htoc54 ocaml-unix documentation].

```ocaml
let try_finalise f x finally y =
  let res = try f x with e -> finally y; raise e in
  finally y;
  res

let rec restart_on_EINTR f x =
  try f x with Unix.Unix_error (Unix.EINTR, _, _) -> restart_on_EINTR f x

let double_fork_treatment server service (client_descr, _ as client) =
  let treat () =
    match Unix.fork () with
    | 0 ->
        if Unix.fork () <> 0 then exit 0;
        Unix.close server; service client; exit 0
    | k ->
        ignore (restart_on_EINTR (Unix.waitpid []) k)
  in
  try_finalise treat () Unix.close client_descr

let install_tcp_server_socket addr =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  try
    Unix.bind s addr;
    Unix.listen s 10;
    s
  with e -> Unix.close s; raise e

let tcp_server treat_connection addr =
  ignore (Sys.signal Sys.sigpipe Sys.Signal_ignore);
  let server_sock = install_tcp_server_socket addr in
  while true do
    let client = restart_on_EINTR Unix.accept server_sock in
    treat_connection server_sock client
  done

let server () =
  let port = 8080 in
  let host = (Unix.gethostbyname (Unix.gethostname())).Unix.h_addr_list.(0) in
  let addr = Unix.ADDR_INET (host, port) in
  let treat sock (client_sock, client_addr as client) =
    let service (s, _) =
      let response = "\
        HTTP/1.1 200 OK\r\n\
        Content-Type: text/html; charset=UTF-8\r\n\r\n\
        <html><head><title>Goodbye, world!</title>\
        <style>body { background-color: #0FF }\
        h1 { font-size:3em; color: black; }</style></head>\
        <body><h1>Goodbye, world!</h1></body></html>\r\n"
      in
      Unix.write s response 0 (String.length response);
    in
    double_fork_treatment sock service client
  in
  tcp_server treat addr

let _ =
  Unix.handle_unix_error server ()
```



## Ol

This sample sends 200 OK on any request and echoes the request headers.

```ol
(import (lib http))

(http:run 8080 (lambda (fd request headers send close)
   (send "HTTP/1.0 200 OK\n"
         "Connection: close\n"
         "Content-Type: text/html; charset=UTF-8\n"
         "Server: " (car *version*) "/" (cdr *version*)
         "\n\n"

         "<h1>Goodbye, World!</h1>"
         (ref request 1) ": " (ref request 2)
         "<hr><small>" headers "</small>")
   (close #t)
))

```



## Opa

From [http://doc.opalang.org/index.html#_a_first_peek_at_opa Opa documentation]:

```ocaml
server = one_page_server("Hello", -> <>Goodbye, world</>)
```

Compile and run:

```bash
opa file.opa --
```



## Panda

Using the command line client. Listen to port 8080. For each request a request object is returned, we ignore this and just use it to send message which will be the response.


```panda
8080.port.listen.say("Hello world!")
```



## Perl


```Perl
use Socket;

my $port = 8080;
my $protocol = getprotobyname( "tcp" );

socket( SOCK, PF_INET, SOCK_STREAM, $protocol ) or die "couldn't open a socket: $!";
  # PF_INET to indicate that this socket will connect to the internet domain
  # SOCK_STREAM indicates a TCP stream, SOCK_DGRAM would indicate UDP communication

setsockopt( SOCK, SOL_SOCKET, SO_REUSEADDR, 1 ) or die "couldn't set socket options: $!";
  # SOL_SOCKET to indicate that we are setting an option on the socket instead of the protocol
  # mark the socket reusable

bind( SOCK, sockaddr_in($port, INADDR_ANY) ) or die "couldn't bind socket to port $port: $!";
  # bind our socket to $port, allowing any IP to connect

listen( SOCK, SOMAXCONN ) or die "couldn't listen to port $port: $!";
  # start listening for incoming connections

while( accept(CLIENT, SOCK) ){
  print CLIENT "HTTP/1.1 200 OK\r\n" .
               "Content-Type: text/html; charset=UTF-8\r\n\r\n" .
               "<html><head><title>Goodbye, world!</title></head><body>Goodbye, world!</body></html>\r\n";
  close CLIENT;
}
```

Various modules exist for using sockets, including the popular IO::Socket
which provides a simpler and more friendly OO interface for the socket layer.
Here is the solution using this module:
{{libheader|IO::Socket::INET}}

```Perl
use IO::Socket::INET;

my $sock = new IO::Socket::INET ( LocalAddr => "127.0.0.1:8080",
                                  Listen    => 1,
                                  Reuse     => 1, ) or die "Could not create socket: $!";

while( my $client = $sock->accept() ){
  print $client "HTTP/1.1 200 OK\r\n" .
                "Content-Type: text/html; charset=UTF-8\r\n\r\n" .
                "<html><head><title>Goodbye, world!</title></head><body>Goodbye, world!</body></html>\r\n";
  close $client;
}
```


Using Perl's glue power, provide a suicide note
with visitor counter via netcat:

```Perl
while (++(our $vn)) {
  open NC, "|-", qw(nc -l -p 8080 -q 1);
  print NC "HTTP/1.0 200 OK\xd\xa",
      "Content-type: text/plain; charset=utf-8\xd\xa\xd\xa",
      "Goodbye, World! (hello, visitor No. $vn!)\xd\xa";
}
```


Here's another solution using Plack (may be found on CPAN):

```Perl
use Plack::Runner;
my $app = sub {
    return [ 200,
       [ 'Content-Type' => 'text/html; charset=UTF-8' ],
       [ '<html><head><title>Goodbye, world!</title></head><body>Goodbye, world!</body></html>' ]
     ]
};
my $runner = Plack::Runner->new;
$runner->parse_options('--host' => 'localhost', '--port' => 8080);
$runner->run($app);
```


When using plackup, then this may be compressed to one line:

```perl
my $app = sub { return [ 200, [ 'Content-Type' => 'text/html; charset=UTF-8' ], [ '<html><head><title>Goodbye, world!</title></head><body>Goodbye, world!</body></html>' ] ] };
```

Use
```Shell
plackup --host localhost --port 8080 script.psgi
```
 to start the webserver.


## Perl 6

{{works with|Rakudo}}

```perl6
my $listen = IO::Socket::INET.new(:listen, :localhost<localhost>, :localport(8080));
loop {
    my $conn = $listen.accept;
    my $req =  $conn.get ;
    $conn.print: "HTTP/1.0 200 OK\r\nContent-Type: text/plain; charset=UTF-8\r\n\r\nGoodbye, World!\r\n";
    $conn.close;
}
```

Async:

```perl6
react {
    whenever IO::Socket::Async.listen('0.0.0.0', 8080) -> $conn {
        whenever $conn.Supply.lines -> $line {
            $conn.print: "HTTP/1.0 200 OK\r\nContent-Type: text/plain; charset=UTF-8\r\n\r\nGoodbye, World!\r\n";
            $conn.close;
        }
    }
}
```



## Phix

Windows only for now (should be relatively straightforward to get it working on linux)

Output as C, code is however a translation of a FASM example.

```Phix
-- demo\rosetta\SimpleHttpServer.exw
include builtins\sockets.e      -- added for 0.8.1 (not yet documented)

constant MAX_QUEUE      = 100,
         ESCAPE         = #1B,
         BUFFER_SIZE    = 2048,
         buffer         = allocate(BUFFER_SIZE),
         sock_addr      = new_sock_addr(AF_INET, 8080, NULL),
         peerAddr       = new_sock_addr(),

response = substitute("""
HTTP/1.1 200 OK
Content-Type: text/html; charset=UTF-8

<!DOCTYPE html>
<html>
 <head>
  <title>Bye-bye baby bye-bye</title>
  <style>
   body { background-color: #111 }
   h1 { font-size:4cm; text-align: center; color: black;
        text-shadow: 0 0 2mm red}
  </style>
 </head>
 <body>
  <h1>Goodbye, world!</h1>
 </body>
</html>

""","\n","\r\n")

atom sock = socket(AF_INET,SOCK_STREAM,NULL)
bind(sock,sock_addr)
listen(sock,MAX_QUEUE)
while get_key()!=ESCAPE do
    atom peer = accept(sock,peerAddr),
         ip = get_sin_addr(sock_addr)
    integer len = recv(peer,buffer,BUFFER_SIZE,0)
    string request = peek({buffer,len})
    printf(1,"Client IP: %s\n%s\n",{ip_to_string(ip),request})
    if length(request)>3 and request[1..4]="GET " then
        poke(buffer,response)
        send(peer,buffer,length(response),0)
    end if
end while
```



## PicoLisp

Contents of the file "goodbye.l":

```PicoLisp
(html 0 "Bye" NIL NIL
   "Goodbye, World!" )
```

Start server:

```txt
$ pil @lib/http.l @lib/xhtml.l -'server 8080 "goodbye.l"' -wait
```



## Prolog

{{works with|SWI Prolog}}
{{works with|YAP}}


```Prolog
% The following modules are used in the main module to start a server.
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

% The following module is used in every module that describes a page.
:- use_module(library(http/html_write)).

% Main entry point: starts the server on port 8080.
server :- http_server(http_dispatch, [port(8080)]).

% Defines the handler for the root URI /.
:- http_handler('/', say_goodbye, []).

% Defines the actual page content.
% In this case we're returning a page with the title "Howdy" and the content,
% wrapped in <h1></h1> tags, "Goodbye, World!".
say_goodbye(_Request) :- reply_html_page([title('Howdy')],
                                   [h1('Goodbye, World!')]).
```



## PureBasic


```PureBasic
If InitNetwork() = 0
  MessageRequester("Error", "Can't initialize the network !")
  End
EndIf

Port = 8080

If CreateNetworkServer(0, Port)
  Repeat
    Delay(1)
    SEvent = NetworkServerEvent()
    If SEvent
      ClientID = EventClient()
      Select SEvent
        Case #PB_NetworkEvent_Data
          SendNetworkData(ClientID,@"Goodbye, World!",Len("Goodbye, World!"))
          CloseNetworkConnection(ClientID)
      EndSelect
    EndIf
  ForEver
Else
  MessageRequester("Error", "Can't create the server (port in use ?).")
EndIf
```



## PHP


```PHP
<?php
                     // AF_INET6 for IPv6  // IP
$socket = socket_create(AF_INET, SOCK_STREAM, 0) or die('Failed to create socket!');
                  // '127.0.0.1' to limit only to localhost // Port
socket_bind($socket, 0,                                        8080);
socket_listen($socket);

$msg = '<html><head><title>Goodbye, world!</title></head><body>Goodbye, world!</body></html>';

for (;;) {
    // @ is used to stop PHP from spamming with error messages if there is no connection
    if ($client = @socket_accept($socket)) {
        socket_write($client, "HTTP/1.1 200 OK\r\n" .
               "Content-length: " . strlen($msg) . "\r\n" .
               "Content-Type: text/html; charset=UTF-8\r\n\r\n" .
               $msg);
    }
    else usleep(100000); // limits CPU usage by sleeping after doing every request
}
?>
```



## Python

Using the <code>wsgiref.simple_server</code> module (Python < 3.2).


```Python
from wsgiref.simple_server import make_server

def app(environ, start_response):
    start_response('200 OK', [('Content-Type','text/html')])
    yield b"<h1>Goodbye, World!</h1>"

server = make_server('127.0.0.1', 8080, app)
server.serve_forever()
```


Using the <code>http.server</code> module (Python 3).


```Python
import threading

from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer


class HelloHTTPRequestHandler(BaseHTTPRequestHandler):

  message = 'Hello World! 今日は'

  def do_GET(self):
    self.send_response(200)
    self.send_header('Content-type', 'text/html; charset=UTF-8')
    self.end_headers()
    self.wfile.write(self.message.encode('utf-8'))
    self.close_connection = True


def serve(addr, port):
  with ThreadingHTTPServer((addr, port), HelloHTTPRequestHandler) as server:
    server.serve_forever(poll_interval=None)


if __name__ == '__main__':

  addr, port = ('localhost', 80)

  threading.Thread(target=serve, args=(addr, port), daemon=True).start()

  try:
    while True:
      # handle Ctrl+C
      input()

  except KeyboardInterrupt:
    pass

```



## R


```rsplus

library(httpuv)

runServer("0.0.0.0", 5000,
	list(
		call = function(req) {
		  list(status = 200L,	headers = list('Content-Type' = 'text/html'), body = "Hello world!")
	        }
	)
)



```



## Racket



```racket

#lang racket
(require web-server/servlet web-server/servlet-env)
(define (start req) (response/xexpr "Goodbye, World!"))
(serve/servlet start #:port 8080 #:servlet-path "/")

```



## REALbasic



```vb

Class HTTPSock
Inherits TCPSocket
  Event Sub DataAvailable()
    Dim headers As New InternetHeaders
    headers.AppendHeader("Content-Length", Str(LenB("Goodbye, World!")))
    headers.AppendHeader("Content-Type", "text/plain")
    headers.AppendHeader("Content-Encoding", "identity")
    headers.AppendHeader("Connection", "close")
    Dim data As String = "HTTP/1.1 200 OK" + EndOfLine.Windows + headers.Source + EndOfLine.Windows + EndOfLine.Windows + "Goodbye, World!"
    Me.Write(data)
    Me.Close
  End Sub
End Class

Class HTTPServ
Inherits ServerSocket
  Event Sub AddSocket() As TCPSocket
    Return New HTTPSock
  End Sub
End Class

Class App
Inherits Application
  Event Sub Run(Args() As String)
    Dim sock As New HTTPServ
    sock.Port = 8080
    sock.Listen()
    While True
      App.DoEvents
    Wend
  End Sub
End Class

```



## REXX

Based on the UNIX Shell entry.  Works with Regina, tested on GNU/Linux.  Requires netcat as nc.


```rexx
/* HTTP hello server */
response.1 = 'HTTP/1.1 200 OK' || '0D0A'X,
          || 'Connection: close' || '0D0A'X,
          || 'Content-Type: text/html' || '0D0A0D0A'X
response.2 = '<!DOCTYPE html>' || '0A'X,
          || '<html><head><title>Hello, Rosetta</title></head>' || '0A'X,
          || '<body><h2>Goodbye, World!</h2></body>' || '0A'X,
          || '<!-- Shout out from the Rosetta Code programming chrestomathy --></html>' || '0A'X

DO FOREVER
    ADDRESS SYSTEM 'nc -l 8080' WITH INPUT STEM response.
END
```



## Ring


```ring

Load "guilib.ring"

cResponse = "HTTP/1.1 200 OK\r\n" +
               "Content-Type: text/html\r\n\r\n" +
               "<html><head><title>Goodbye, world!</title></head>" +
               "<body>Goodbye, world!</body></html>"

cResponse = substr(cResponse,"\r\n",char(13)+char(10))

new qApp {
	oServer = new Server { Server() }
	exec()
}

Class Server

        win1 lineedit1
        oTcpServer oTcpClient
        cOutput = ""

        func server

                win1 = new qwidget()

                lineedit1 = new qtextedit(win1) {
                        setGeometry(150,50,200,300)
                }

                win1 {
                        setwindowtitle("Server")
                        setgeometry(450,100,400,400)
                        show()
                }

                oTcpServer = new qTcpServer(win1) {
                        setNewConnectionEvent("oServer.pNewConnection()")
                        oHostAddress = new qHostAddress()
                        oHostAddress.SetAddress("127.0.0.1")
                        listen(oHostAddress,8080)
                }
                cOutput = "Server Started" + nl +
                           "listen to port 8080" + nl

                lineedit1.settext(cOutput)

        Func pNewConnection

                oTcpClient = oTcpServer.nextPendingConnection()
                while not oTcpClient.waitForReadyRead(100) end
                cOutput += "Accept Connection" + nl
                lineedit1.settext(cOutput)
                oTcpClient {
                        write(cResponse,len(cResponse))
                        flush()
                        waitforbyteswritten(300000)
                        close()
                }

```



## Ruby

Using the WEBrick module from Ruby's standard library.

```ruby
require 'webrick'
server = WEBrick::HTTPServer.new(:Port => 8080)
server.mount_proc('/') {|request, response| response.body = "Goodbye, World!"}
trap("INT") {server.shutdown}
server.start
```


Same code without <code>trap</code>, in a single statement using <code>tap</code>.

```Ruby
require 'webrick'
WEBrick::HTTPServer.new(:Port => 80).tap {|srv|
    srv.mount_proc('/') {|request, response| response.body = "Goodbye, World!"}
}.start
```


Using the [http://www.sinatrarb.com/ sinatra] gem:

```ruby
require 'sinatra'
get("/") { "Goodbye, World!" }
```



## Run BASIC


```runbasic
html "Hello World!"
```



## Rust

Basically no error handling. This web server will simply panic if there is any sort of error.

```rust
use std::net::{Shutdown, TcpListener};
use std::thread;
use std::io::Write;

const RESPONSE: &'static [u8] = b"HTTP/1.1 200 OK\r
Content-Type: text/html; charset=UTF-8\r\n\r
<!DOCTYPE html><html><head><title>Bye-bye baby bye-bye</title>
<style>body { background-color: #111 }
h1 { font-size:4cm; text-align: center; color: black;
text-shadow: 0 0 2mm red}</style></head>
<body><h1>Goodbye, world!</h1></body></html>\r";


fn main() {
    let listener = TcpListener::bind("127.0.0.1:8080").unwrap();

    for stream in listener.incoming() {
        thread::spawn(move || {
            let mut stream = stream.unwrap();
            match stream.write(RESPONSE) {
                Ok(_) => println!("Response sent!"),
                Err(e) => println!("Failed sending response: {}!", e),
            }
            stream.shutdown(Shutdown::Write).unwrap();
        });
    }
}

```



## Scala

{{libheader|Scala}}
{{Trans|Java}}It shows that Scala can simply embed XML fragments.

```Scala
import java.io.PrintWriter
import java.net.ServerSocket

object HelloWorld extends App {

  val text =
    <HTML>
      <HEAD>
        <TITLE>Hello world </TITLE>
      </HEAD>
      <BODY LANG="en-US" BGCOLOR="#e6e6ff" DIR="LTR">
        <P ALIGN="CENTER"> <FONT FACE="Arial, sans-serif" SIZE="6">Goodbye, World!</FONT> </P>
      </BODY>
    </HTML>
val port = 8080
  val listener = new ServerSocket(port)
   printf("Listening at port %1$d", port)

  while (true) {
    val sock = listener.accept()
    new PrintWriter(sock.getOutputStream(), true).println(text)
    sock.close()
  }
}
```



## Salmon


```Salmon
use "http.salm" : "http.si";

/* Don't do any logging. */
procedure log(...) { };

simple_http_server(8080, procedure(header, connection)
  { respond_text(connection, "Goodbye, World!"); });
```



## Seed7

The code below was inspired by the example code for the function [http://seed7.sourceforge.net/libraries/listener.htm#openInetListener%28in_integer%29 openInetListener].


```Seed7
$ include "seed7_05.s7i";
  include "listener.s7i";

const proc: main is func
  local
    var listener: aListener is listener.value;
    var file: sock is STD_NULL;
  begin
    aListener := openInetListener(8080);
    listen(aListener, 10);
    while TRUE do
      sock := accept(aListener);
      write(sock, "HTTP/1.1 200 OK\r\n\
                  \Content-Type: text/html; charset=UTF-8\r\n\
                  \\r\n\
                  \<html><body>Hello, world!</body></html>\n");
      close(sock);
    end while;
  end func;
```



## Sidef


Using the low-level ''Socket'' object:

```ruby
var port = 8080;
var protocol = Socket.getprotobyname("tcp");
 
var sock = (Socket.open(Socket.PF_INET, Socket.SOCK_STREAM, protocol) || die "couldn't open a socket: #{$!}");
  # PF_INET to indicate that this socket will connect to the internet domain
  # SOCK_STREAM indicates a TCP stream, SOCK_DGRAM would indicate UDP communication
 
sock.setsockopt(Socket.SOL_SOCKET, Socket.SO_REUSEADDR, 1) || die "couldn't set socket options: #{$!}";
  # SOL_SOCKET to indicate that we are setting an option on the socket instead of the protocol
  # mark the socket reusable
 
sock.bind(Socket.sockaddr_in(port, Socket.INADDR_ANY)) || die "couldn't bind socket to port #{port}: #{$!}";
  # bind our socket to $port, allowing any IP to connect
 
sock.listen(Socket.SOMAXCONN) || die "couldn't listen to port #{port}: #{$!}";
  # start listening for incoming connections
 
while (var client = sock.accept) {
  client.print ("HTTP/1.1 200 OK\r\n" +
               "Content-Type: text/html; charset=UTF-8\r\n\r\n" +
               "<html><head><title>Goodbye, world!</title></head>" +
               "<body>Goodbye, world!</body></html>\r\n");
  client.close;
}
```


A more friendly interface, using the ''IO::Socket::INET'' library:

```ruby
var inet = require('IO::Socket::INET');

var sock = inet.new( LocalAddr => "127.0.0.1:8080",
                     Listen    => 1,
                     Reuse     => 1,
            );

while (var client = sock.accept) {
    client.print ("HTTP/1.1 200 OK\r\n" +
                "Content-Type: text/html; charset=UTF-8\r\n\r\n" +
                "<html><head><title>Goodbye, world!</title></head>" +
                "<body>Goodbye, world!</body></html>\r\n");
    client.close;
}
```



## Smalltalk

{{works with|Smalltalk/X}}
starting server:

```smalltalk
Smalltalk loadPackage:'stx:goodies/webServer'. "usually already loaded"
|myServer service|

myServer := HTTPServer startServerOnPort:8082.
service := HTTPPluggableActionService new.
service
    register:[:request |
        self halt: 'debugging'.
        request reply:'<HTML><BODY><H1>Hello World</H1></BODY></HTML>'
    ]
    as:'hello'.
service linkNames:#('/' ).
service registerServiceOn: myServer.
myServer start.
```

Be aware that the above is an ad-hoc minimal scripting example.
Normally, a service subclass is used and
response handlers are defined as methods of it (not as action blocks).
Also, services and HTML generation is usually done using a framework
(at least DOM-based, but usually a higher level toolkit).
Especially take a look at smalltalk frameworks like Aida, Seaside, VisualWave etc.


### Pharo Smalltalk

Pharo ships with the Zinc HTTP Component frameworks
that includes a ZnServer class.
Here's the simplest solution to start a web server:

```smalltalk

(ZnServer startDefaultOn: 1701)
   onRequestRespond: [ :request |
      ZnResponse ok: (ZnEntity text: 'Hello World!') ].

```


To stop the server, use the following:

```smalltalk

ZnServer stopDefault.

```



## Tcl


### Tcl 8.x

This version is adapted from [http://wiki.tcl.tk/28414 the Tcler's Wiki].

```tcl
proc accept {chan addr port} {
    while {[gets $chan] ne ""} {}
    puts $chan "HTTP/1.1 200 OK\nConnection: close\nContent-Type: text/plain\n"
    puts $chan "Goodbye, World!"
    close $chan
}
socket -server accept 8080
vwait forever
```



### Jim Tcl

Jim is a small footprint reimplementation of Tcl with modern features.

```tcl
set s [socket stream.server 127.0.0.1:8080]
$s readable {
    set client [$s accept]
    $client puts "HTTP/1.1 200 OK\nConnection: close\nContent-Type: text/plain\n"
    $client puts "Hello, World!\n"
    $client close
}
vwait done
```



## UNIX Shell



```bash
while true; do { echo -e 'HTTP/1.1 200 OK\r\n'; echo 'Hello, World!'; } | nc -l 8080; done
```



## Wart



```python
with server_socket socket :port 4000
  accepting client :from socket
    making stdout outfile+fd.client
      prn "HTTP/1.0 200 OK"
      prn "Content-type: text/plain"
      prn ""
      prn "Hello, world!"
```




## zkl

A threaded web server that returns "Goodbye, World!" for every request

```zkl
const PORT=8080;
const SERVLET_THREADS=4;

    // A class to process requests from clients (eg browsers)
    // in a thread. Requests are received via a pipe, which feeds
    // all Servlet threads.
class Servlet{
   fcn init(jobPipe){ self.launch(jobPipe); }
   fcn liftoff(jobPipe){
      while(1){    // read request, write response, repeat
         socket:=jobPipe.read();
         if(socket.wait(60) != 1)	// what is Chrome doing?
            { socket.close(); continue; }
         if (request:=socket.read())
	    try{ processRequest(request,socket); } catch{}
      }
   }
   fcn splashdown(h,e){ println("Servlet died before its time"); }
}

fcn processRequest(request,socket){
   response:=responseHeader();
   response+="Goodbye, World!";
   socket.write(response); socket.close();    // no Keep-Alive
}

fcn responseHeader(status=200,reason="OK"){
  String(
   "HTTP/1.0 ",status," ",reason,"\r\n",
   Time.Date.httpDate(),"\r\n"
   "Server: ZTWS (zkl)\r\n"
   "Connection: close\r\n"
   "Content-Type: text/html; charset=UTF-8\r\n"
   "\r\n")
}

      //////////////////// Start the server ///////////////////////
var jobPipe=Thread.Pipe();    // a queue of requests
do(SERVLET_THREADS){ Servlet(jobPipe) }  // start threads

    // Create the HTTP server listen socket
    // Sits here forever passing client HTTP connects to Servlets
serverSocket:=Network.TCPServerSocket.open(PORT);
println("HTTP server started at http://",
    serverSocket.hostname, ":", serverSocket.port);
serverSocket.listen(jobPipe);
```

