+++
title = "Sockets"
description = ""
date = 2019-10-18T19:35:34Z
aliases = []
[extra]
id = 2322
[taxonomies]
categories = []
tags = []
+++

{{task|Networking and Web Interaction}}
For this exercise a program is open a socket to localhost on port 256 and send the message "hello socket world" before closing the socket.

Catching any exceptions or errors is not required.





## Ada

{{libheader|GNAT RTL}}

```ada
with GNAT.Sockets;  use GNAT.Sockets;

procedure Socket_Send is
   Client : Socket_Type;
begin
   Initialize;
   Create_Socket  (Socket => Client);
   Connect_Socket (Socket => Client,
                   Server => (Family => Family_Inet,
                              Addr   => Inet_Addr ("127.0.0.1"),
                              Port   => 256));
   String'Write (Stream (Client), "hello socket world");
   Close_Socket (Client);
end Socket_Send;
```



## Aime


```aime
file i, o;
tcpip_connect(i, o, "127.0.0.1", 256, 0);
i.text("hello socket world");
```



## AutoHotkey

modified from
[http://www.autohotkey.com/forum/topic13829.html script] by zed gecko.

```autohotkey
Network_Port = 256
Network_Address = 127.0.0.1
NewData := false
DataReceived =
GoSub, Connection_Init
SendData(socket,"hello socket world")
return

Connection_Init:
OnExit, ExitSub

socket := ConnectToAddress(Network_Address, Network_Port)
if socket = -1
    ExitApp

Process, Exist
DetectHiddenWindows On
ScriptMainWindowId := WinExist("ahk_class AutoHotkey ahk_pid " . ErrorLevel)
DetectHiddenWindows Off

NotificationMsg = 0x5556
OnMessage(NotificationMsg, "ReceiveData")

FD_READ = 1
FD_CLOSE = 32
if DllCall("Ws2_32\WSAAsyncSelect", "UInt", socket, "UInt", ScriptMainWindowId, "UInt", NotificationMsg, "Int", FD_READ|FD_CLOSE)
{
    MsgBox % "WSAAsyncSelect() indicated Winsock error " . DllCall("Ws2_32\WSAGetLastError")
    ExitApp
}

return


ConnectToAddress(IPAddress, Port)
{
    VarSetCapacity(wsaData, 32)
    result := DllCall("Ws2_32\WSAStartup", "UShort", 0x0002, "UInt", &wsaData)
    if ErrorLevel
    {
        MsgBox WSAStartup() could not be called due to error %ErrorLevel%. Winsock 2.0 or higher is required.
        return -1
    }
    if result
    {
        MsgBox % "WSAStartup() indicated Winsock error " . DllCall("Ws2_32\WSAGetLastError")
        return -1
    }
    AF_INET = 2
    SOCK_STREAM = 1
    IPPROTO_TCP = 6
    socket := DllCall("Ws2_32\socket", "Int", AF_INET, "Int", SOCK_STREAM, "Int", IPPROTO_TCP)
    if socket = -1
    {
        MsgBox % "socket() indicated Winsock error " . DllCall("Ws2_32\WSAGetLastError")
        return -1
    }
    SizeOfSocketAddress = 16
    VarSetCapacity(SocketAddress, SizeOfSocketAddress)
    InsertInteger(2, SocketAddress, 0, AF_INET)
    InsertInteger(DllCall("Ws2_32\htons", "UShort", Port), SocketAddress, 2, 2)
    InsertInteger(DllCall("Ws2_32\inet_addr", "Str", IPAddress), SocketAddress, 4, 4)
    if DllCall("Ws2_32\connect", "UInt", socket, "UInt", &SocketAddress, "Int", SizeOfSocketAddress)
    {
        MsgBox % "connect() indicated Winsock error " . DllCall("Ws2_32\WSAGetLastError") . "?"
        return -1
    }
    return socket
}

ReceiveData(wParam, lParam)
{
    global DataReceived
    global NewData
    socket := wParam
    ReceivedDataSize = 4096
    Loop
    {
        VarSetCapacity(ReceivedData, ReceivedDataSize, 0)
        ReceivedDataLength := DllCall("Ws2_32\recv", "UInt", socket, "Str", ReceivedData, "Int", ReceivedDataSize, "Int", 0)
        if ReceivedDataLength = 0
            ExitApp
        if ReceivedDataLength = -1
        {
            WinsockError := DllCall("Ws2_32\WSAGetLastError")
            if WinsockError = 10035
            {
                DataReceived = %TempDataReceived%
                NewData := true
                return 1
            }
            if WinsockError <> 10054
                MsgBox % "recv() indicated Winsock error " . WinsockError
            ExitApp
        }
        if (A_Index = 1)
            TempDataReceived =
        TempDataReceived = %TempDataReceived%%ReceivedData%
    }
    return 1
}

SendData(wParam,SendData)
{
    socket := wParam
    SendDataSize := VarSetCapacity(SendData)
    SendDataSize += 1
    sendret := DllCall("Ws2_32\send", "UInt", socket, "Str", SendData, "Int", SendDatasize, "Int", 0)
}

InsertInteger(pInteger, ByRef pDest, pOffset = 0, pSize = 4)
{
    Loop %pSize%
        DllCall("RtlFillMemory", "UInt", &pDest + pOffset + A_Index-1, "UInt", 1, "UChar", pInteger >> 8*(A_Index-1) & 0xFF)
}

ReceiveProcedure:
    if NewData
        GuiControl, , ReceivedText, %DataReceived%
    NewData := false
Return

ExitSub:
DllCall("Ws2_32\WSACleanup")
ExitApp
```



## AutoIt



```AutoIt
Func _HelloWorldSocket()
	TCPStartup()
	$Socket = TCPConnect("127.0.0.1", 256)
	TCPSend($Socket, "Hello World")
	TCPCloseSocket($Socket)
	TCPShutdown()
EndFunc
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      INSTALL @lib$+"SOCKLIB"
      PROC_initsockets

      socket% = FN_tcpconnect("localhost", "256")
      IF socket% <=0 ERROR 100, "Failed to open socket"

      REM Don't use FN_writesocket since an error is expected
      msg$ = "hello socket world"
      SYS `send`, socket%, !^msg$, LEN(msg$), 0 TO result%

      PROC_closesocket(socket%)
      PROC_exitsockets
```



## C

{{works with|POSIX|.1-2001}}

{{works with|gcc|4.2.2}}

With little changes it could work on MS Windows (without Cygwin) too. But I don't know exactly how. I have tested it using <code>nc -l -p 256</code>.


```c
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

const char *msg = "hello socket world";

int main()
{
   int i, sock, len, slen;

   struct addrinfo hints, *addrs;
   memset(&hints, 0, sizeof(struct addrinfo));
   hints.ai_family = AF_UNSPEC;
   hints.ai_socktype = SOCK_STREAM;

   if (0 == getaddrinfo("localhost", "256", &hints, &addrs))
   {
       sock = socket(addrs->ai_family, addrs->ai_socktype, addrs->ai_protocol);
       if ( sock >= 0 )
       {
           if ( connect(sock, addrs->ai_addr, addrs->ai_addrlen) >= 0 )
           {
               const char *pm = msg;
               do
               {
                  len = strlen(pm);
                  slen = send(sock, pm, len, 0);
                  pm += slen;
               } while ((0 <= slen) && (slen < len));
           }
           close(sock);
       }
       freeaddrinfo(addrs);
   }
}
```



## C++

I have tested it using <code>nc -vlp 4321</code>.


```cpp
//compile with g++ main.cpp -lboost_system -pthread

#include <boost/asio.hpp>

int main()
{
  boost::asio::io_context io_context;
  boost::asio::ip::tcp::socket sock(io_context);
  boost::asio::ip::tcp::resolver resolver(io_context);
  boost::asio::ip::tcp::resolver::query query("localhost", "4321");

  boost::asio::connect(sock, resolver.resolve(query));
  boost::asio::write(sock, boost::asio::buffer("Hello world socket\r\n"));

  return 0;
}
```


## C#


```c#
using System;
using System.IO;
using System.Net.Sockets;

class Program {
    static void Main(string[] args) {
        TcpClient tcp = new TcpClient("localhost", 256);
        StreamWriter writer = new StreamWriter(tcp.GetStream());

        writer.Write("hello socket world");
        writer.Flush();

        tcp.Close();
    }
}
```


Clean Socket alternative:


```c#
using System.Text;
using System.Net.Sockets;

namespace SocketClient
{
    class Program
    {
        static void Main(string[] args)
        {
            var sock = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
            sock.Connect("127.0.0.1", 1000);
            sock.Send(Encoding.ASCII.GetBytes("Hell, world!"));
            sock.Close();
        }
    }
}
```



## Cind



```cind

var libsocket = @("lib","socket");
// connect
int socket = libsocket.connect("localhost",256);
// send data
{
   sheet data = (sheet)"hello socket world";
   int datalen = data.size();
   int was = libsocket.send(socket,data,0,datalen);
   // assuming here that all data has been sent (if not, send them in some loop)
}
// close socket
libsocket.close(socket);

```




## Clojure


```clojure
(ns socket-example
  (:import (java.net Socket)
           (java.io PrintWriter)))

(defn send-data [host msg]
  (with-open [sock (Socket. host 256)
              printer (PrintWriter. (.getOutputStream sock))]
    (.println printer msg)))

(send-data "localhost" "hello socket world")
```



## Common Lisp


{{libheader|usocket}}


```lisp
CL-USER> (usocket:with-client-socket (socket stream "localhost" 256)
           (write-line "hello socket world" stream)
           (values))
; No value
```



```lisp
aurora ~% sudo nc -l -p 256
hello socket world
```



## D


```d
module socket ;
import std.stdio ;
import std.socket ;
version(Win32) {
  // For Win32 systems, need to link with ws2_32.lib.
  pragma(lib, "ws2_32.lib") ;
}
void main() {
  long res;
  auto socket = new Socket(AddressFamily.INET, SocketType.STREAM) ;
  socket.connect(new InternetAddress("localhost",256)) ;
  res = socket.send(cast(void[])"hello socket world") ;
  writefln("Socket %d bytes sent.", res) ;
  socket.close() ;
}
```



## Delphi


```Delphi
program Sockets;

{$APPTYPE CONSOLE}

uses IdTCPClient;

var
  lTCPClient: TIdTCPClient;
begin
  lTCPClient := TIdTCPClient.Create(nil);
  try
    lTCPClient.Host := '127.0.0.1';
    lTCPClient.Port := 256;
    lTCPClient.Connect;
    lTCPClient.IOHandler.WriteLn('hello socket world');
  finally
    lTCPClient.Free;
  end;
end.
```


## Elena

ELENA 4.1 :

```elena
import system'net;
import system'text;
import extensions'text;
import system'io;

public program()
{
    var socket := new Socket(AF_INET,SOCK_STREAM,IPPROTO_TCP);
    socket.connect("127.0.0.1",256);
    var s := "hello socket world";
    socket.write(AnsiEncoder.toByteArray(0, s.Length, s));
    socket.close()
}
```



## Elixir


```Elixir

defmodule Sockets do
  require Logger

  def send_message(port, message) do
    {:ok, socket} = :gen_tcp.connect('localhost', port, [])
    :gen_tcp.send(socket, message)
  end
end

Sockets.send_message(256, "hello socket world")

```



## Emacs Lisp


Emacs treats network connections as sub-processes.  <code>make-network-process</code> is the low-level socket creation,


```Lisp
(let ((proc (make-network-process :name "my sock"
                                  :host 'local    ;; or hostname string
                                  :service 256)))
  (process-send-string proc "hello socket world")
  (delete-process proc))
```



## Erlang


```erlang
-module(socket).
-export([start/0]).

start() ->
    {ok, Sock} = gen_tcp:connect("localhost", 256,
                                 [binary, {packet, 0}]),
    ok = gen_tcp:send(Sock, "hello socket world"),
    ok = gen_tcp:close(Sock).

```



## Factor


```factor
"localhost" 256 <inet> utf8 [ "hello socket world" print ] with-client
```



## Fantom



```fantom

using inet

class Socket
{
  public static Void main ()
  {
    sock := TcpSocket()
    sock.connect(IpAddr("localhost"), 256)

    sock.out.printLine("hello socket world")
    sock.out.flush
    sock.close
  }
}

```



## Forth

{{works with|GNU Forth|0.7.0}}

```forth
include unix/socket.fs

s" localhost" 256 open-socket

dup s" hello socket world" rot write-socket

close-socket
```



## Go


```go
package main

import (
    "fmt"
    "net"
)

func main() {
    conn, err := net.Dial("tcp", "localhost:256")
    if err != nil {
        fmt.Println(err)
        return
    }
    defer conn.Close()
    _, err = conn.Write([]byte("hello socket world"))
    if err != nil {
        fmt.Println(err)
    }
}
```

{{out | Test with nc}}

```txt

$ sudo nc -l 256 & go run sock.go
[2] 19754
hello socket world[2]+  Done                    sudo nc -l 256
$

```



## Groovy


```groovy
s = new java.net.Socket("localhost", 256)
s << "hello socket world"
s.close()
```



## Haskell


```haskell
import Network

main = withSocketsDo $ sendTo "localhost" (PortNumber $ toEnum 256) "hello socket world"
```


== Icon and Unicon ==
=
## Icon
=

```icon
link cfunc
procedure main ()
   hello("localhost", 1024)
end

procedure hello (host, port)
   write(tconnect(host, port) | stop("unable to connect to", host, ":", port) ,  "hello socket world")
end
```

Note:  Socket support in native Icon is limited and requires the external helper function cfunc.
=
## Unicon
=
Unicon integrated TCP/IP networking and messaging.

```unicon
procedure main(arglist)       #: usage socket port hostname or socket port
    hello(arglist[2]|"",arglist[1])
end

procedure hello(host,port)
   local s
   /host := ""
   host ||:= ":"
   host ||:= 0 < 65536 > port | runerr(103,port)
   if s := open(host,"n") then {
      write(s, "hello socket world.")
      close(s)
      }
   else  stop("Unable to connect to ",host,":",port)
   return
end
```



## IDL



```idl
socket, unit, 'localhost',256,/get_lun
printf,unit,"hello socket world"
close, unit
```


"Well-known" port numbers (under 1024 -- such as 256) can also be specified by name (in this case 'RAP').

If there is no listener on this port, this will hang for a while before timing out.


## J

<code>sdcheck</code> raises assertions if anything goes wrong:


```j
   coinsert'jsocket' [ require 'socket'             NB.  Sockets library
   socket =.  >{.sdcheck sdsocket''                 NB.  Open a socket
   host   =. sdcheck sdgethostbyname 'localhost'    NB.  Resolve host
   sdcheck sdconnect socket ; host ,< 256           NB.  Create connection to port 256
   sdcheck 'hello socket world' sdsend socket , 0   NB.  Send msg
```



## Java


```java
import java.io.IOException;
import java.net.*;
public class SocketSend {
  public static void main(String args[]) throws IOException {
    sendData("localhost", "hello socket world");
  }

  public static void sendData(String host, String msg) throws IOException {
    Socket sock = new Socket( host, 256 );
    sock.getOutputStream().write(msg.getBytes());
    sock.getOutputStream().flush();
    sock.close();
  }
}
```

Encapsulating the <code>Socket</code>'s <code>OutputStream</code> in a <code>PrintStream</code> (for data) or <code>PrintWriter</code> (for text) may be easier in more complex programs for their auto-flush abilities, encoding management, and their overloaded <code>print</code> and <code>println</code> methods. The <code>write</code> method from the original <code>OutputStream</code> will still be available.


## Jsish


```javascript
#!/usr/bin/env jsish
function sockets() {
    var sock = new Socket({client:true, port:256, noAsync:true, udp:true});
    sock.send(-1, 'hello socket world');
    sock.close();
}

;sockets();

/*
=!EXPECTSTART!=
sockets() ==> undefined
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish -u sockets.jsi
[PASS] sockets.jsi
```



## Julia


```Julia

socket = connect("localhost",256)
write(socket, "hello socket world")
close(socket)

```



## Kotlin


```scala
// version 1.2.21

import java.net.Socket

fun main(args: Array<String>) {
    val sock = Socket("localhost", 256)
    sock.use {
        it.outputStream.write("hello socket world".toByteArray())
    }
}
```



## Lasso



```Lasso
local(net) = net_tcp
#net->connect('127.0.0.1',256)
#net->dowithclose => {
    #net->writestring('Hello World')
}
```




## Lua

{{libheader|LuaSocket}}

```lua
socket = require "socket"
host, port = "127.0.0.1", 256

sid = socket.udp()
sid:sendto( "hello socket world", host, port )
sid:close()
```



## Myrddin


```myrddin
use std

const main = {
	match std.dial("tcp!localhost!256")
	| `std.Ok fd:
		std.write(fd, "hello socket world")
		std.close(fd)
	| `std.Err err:
		std.fatal("could not open fd: {}\n", err)
	;;
}
```



## Neko


```ActionScript
/**
 Sockets in Neko
 Tectonics:
   nekoc sockets.neko
   sudo nc -vulp 256 & sudo neko sockets
*/

var socket_init = $loader.loadprim("std@socket_init", 0);
var socket_new = $loader.loadprim("std@socket_new", 1);
var host_resolve = $loader.loadprim("std@host_resolve", 1);
var socket_connect = $loader.loadprim("std@socket_connect", 3);
var socket_write = $loader.loadprim("std@socket_write", 2);
var socket_close = $loader.loadprim("std@socket_close", 1);

/* Initialize Neko socket API */
socket_init();

/* true; UDP, false; TCP */
var socket = socket_new(true);

var c = socket_connect(socket, host_resolve("localhost"), 256);
socket_write(socket, "hello socket world");

socket_close(socket);
```


For testing on port 256, root powers required
{{out}}

```txt
prompt$ nekoc sockets.neko
prompt$ sudo nc -vulp 256 & sudo neko sockets
[1] 4475
Ncat: Version 7.60 ( https://nmap.org/ncat )
Ncat: Listening on :::256
Ncat: Listening on 0.0.0.0:256
hello socket world
prompt$ [1]+  Stopped                 sudo nc -vulp 256
```



## Nemerle


```nemerle
using System.Text;
using System.Net.Sockets;

module Program
{
    Main() : void
    {
        def sock = Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        sock.Connect("127.0.0.1", 1000);
        _ = sock.Send(Encoding.ASCII.GetBytes("Hell, world!"));
        sock.Close();
    }
}
```



## NetRexx

{{trans|Java}}
<
```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary
import java.net.

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  parse arg host':'port':'message
  if host    = '' then host    = 'localhost'
  if port    = '' then port    = 256
  if message = '' then message = 'hello socket world'
  sendToSocket(host, port, message)
  return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method sendToSocket(host, port, message) public static
  do
    sokt = Socket(host, port)
    soks = sokt.getOutputStream()
    soks.write((String message).getBytes())
    soks.flush()
    sokt.close()
  catch ix = IOException
    ix.printStackTrace()
  end
  return

```



## NewLISP


```newlisp

(set 'socket (net-connect "localhost" 256))
(net-send socket "hello socket world")
(net-close socket)
(exit)

```



## Nim


```nim
import net

var s = newSocket()
s.connect("localhost", Port(256))
s.send("Hello Socket World")
s.close()
```



## Objeck


```objeck

use Net;

bundle Default {
  class Socket {
    function : Main(args : String[]) ~ Nil {
      socket := TCPSocket->New("localhost", 256);
      if(socket->IsOpen()) {
        socket->WriteString("hello socket world");
        socket->Close();
      }
    }
  }
}

```


=={{header|Objective-C}}==
(untested)

```objc
// declare the class to conform to NSStreamDelegate protocol

// in some method
NSOutputStream *oStream;
[NSStream getStreamsToHost:[NSHost hostWithName:@"localhost"] port:256 inputStream:NULL outputStream:&oStream];
[oStream setDelegate:self];
[oStream scheduleInRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
[oStream open];


// later, in the same class:
- (void)stream:(NSStream *)aStream handleEvent:(NSStreamEvent)streamEvent {
    NSOutputStream *oStream = (NSOutputStream *)aStream;
    if (streamEvent == NSStreamEventHasBytesAvailable) {
        NSString *str = @"hello socket world";
        const char *rawstring = [str UTF8String];
        [oStream write:rawstring maxLength:strlen(rawstring)];
        [oStream close];
    }
}
```



## OCaml



```ocaml
open Unix

let init_socket addr port =
  let inet_addr = (gethostbyname addr).h_addr_list.(0) in
  let sockaddr = ADDR_INET (inet_addr, port) in
  let sock = socket PF_INET SOCK_STREAM 0 in
  connect sock sockaddr;
  (* convert the file descriptor into high-level channels: *)
  let outchan = out_channel_of_descr sock in
  let inchan = in_channel_of_descr sock in
  (inchan, outchan)
```



```ocaml
let () =
  let ic, oc = init_socket "localhost" 256 in
  output_string oc "hello socket world";
;;
```



## Oz


```oz
declare
  Socket = {New Open.socket init}
in
  {Socket connect(port:256)}
  {Socket write(vs:"hello socket world")}
  {Socket close}
```



## Pascal

See [[Sockets#Delphi | Delphi]]


## Perl


```perl
use Socket;

$host = gethostbyname('localhost');
$in = sockaddr_in(256, $host);
$proto = getprotobyname('tcp');
socket(Socket_Handle, AF_INET, SOCK_STREAM, $proto);
connect(Socket_Handle, $in);
send(Socket_Handle, 'hello socket world', 0, $in);
close(Socket_Handle);
```


Object oriented version.

```perl
use Socket::Class;

$sock = Socket::Class->new(
  'remote_port' => 256,
) || die Socket::Class->error;
$sock->send('hello socket world');
$sock->free;
```



## Perl 6

{{Works with|rakudo|2016.03}}
Will fail with a connect error if there is not a socket server of some kind available on the specified host and port.

```perl6
my $host = '127.0.0.1';
my $port = 256;

my $client = IO::Socket::INET.new(:$host, :$port);
$client.print( 'hello socket world' );
$client.close;
```



## PHP


```PHP
$socket = fsockopen('localhost', 256);
fputs($socket, 'hello socket world');
fclose($socket);
```



## PicoLisp


```PicoLisp
(when (connect "localhost" 256)
   (out @ (prinl "hello socket world"))
   (close @) )
```



## Pike


```pike
import Stdio;

int main(){
   object con = File();
   con->connect("127.0.0.1",256);
   con->write("hello socket world");
   con->close();
}
```



## Prolog

This works with Gnu Prolog. Other implementations will have different predicates.

```Prolog
start(Port) :- socket('AF_INET',Socket),
               socket_connect(Socket, 'AF_INET'(localhost,Port), Input, Output),
               write(Output, 'hello socket world'),
               flush_output(Output),
               close(Output),
               close(Input).
```


## PureBasic


```Purebasic
InitNetwork()
ConnectionID = OpenNetworkConnection("localhost", 256)
SendNetworkString(ConnectionID, "hello socket world")
CloseNetworkConnection(ConnectionID)
```



## Python


```python
import socket
sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.connect(("localhost", 256))
sock.sendall("hello socket world")
sock.close()
```



## R


```rsplus

s <- make.socket(port = 256)
write.socket(s, "hello socket world")
close.socket(s)

```



## Racket


```racket
#lang racket
(let-values ([(in out) (tcp-connect "localhost" 256)])
  (display "hello socket world\n" out)
  (close-output-port out))
```



## Rhope

{{works with|Rhope|alpha 1}}

```rhope
Socket Send(0,0)
|:
    [New@Net Client["localhost",256]]Put String["hello socket world"]
:|
```

The connection is automatically closed when the object is freed.


## Ring


```ring

Load "guilib.ring"

new qApp {
        oClient = new Client { client() }
        exec()
}

Class Client

        win1 oTcpSocket

        func client

                win1 = new qwidget()

                new qpushbutton(win1) {
                        setgeometry(50,50,100,30)
                        settext("connect")
                        setclickevent("oClient.Connect()")
                }

                win1 {
                        setwindowtitle("client")
                        setgeometry(10,100,400,400)
                        show()
                }

        func connect
                oTcpSocket = new qTcpSocket(win1) {
                        setconnectedevent("oClient.pConnected()")
                        connecttohost("127.0.0.1",256,3,0)
                        waitforconnected(5000)
                }

        func pConnected
                cStr = "hello socket world"
                write(cStr,len(cStr))
                flush()
                waitforbyteswritten(300000)
                close()

```



## Ruby


```ruby
require 'socket'
sock = TCPSocket.open("localhost", 256)
sock.write("hello socket world")
sock.close
```



## Rust

{{works with|Rust 1.0 stable}}

```rust
use std::io::prelude::*;
use std::net::TcpStream;

fn main() {
    // Open a tcp socket connecting to 127.0.0.1:256, no error handling (unwrap)
    let mut my_stream = TcpStream::connect("127.0.0.1:256").unwrap();

    // Write 'hello socket world' to the stream, ignoring the result of write
    let _ = my_stream.write(b"hello socket world");

} // <- my_stream's drop function gets called, which closes the socket
```



## Scala

{{libheader|Scala}}

```Scala
import java.net.Socket

object sendSocketData {

  def sendData(host: String, msg: String) {
    val sock = new Socket(host, 256)
    sock.getOutputStream().write(msg.getBytes())
    sock.getOutputStream().flush()
    sock.close()
  }

  sendData("localhost", "hello socket world")
}
```



## Scheme

{{works with|Guile|1.8.8}}{{works with|Chicken Scheme|4.6.0}}

```scheme
(let ((s (socket PF_INET SOCK_STREAM 0)))
    (connect s AF_INET (inet-pton AF_INET "127.0.0.1") 256)
    (display "hello socket world" s))
```



## Seed7

The library [http://seed7.sourceforge.net/libraries/socket.htm socket.s7i]
defines the function [http://seed7.sourceforge.net/libraries/socket.htm#openInetSocket%28in_integer%29 openInetSocket],
which returns a connected internet socket file at a port at localhost.


```seed7
$ include "seed7_05.s7i";
  include "socket.s7i";

const proc: main is func
  local
    var file: sock is STD_NULL;
  begin
    sock := openInetSocket(256);
    writeln(sock, "hello socket world");
    close(sock);
  end func;
```



## Sidef


```ruby
var host = Socket.gethostbyname('localhost');
var in = Socket.sockaddr_in(256, host);
var proto = Socket.getprotobyname('tcp');

var sock = Socket.open(Socket.AF_INET, Socket.SOCK_STREAM, proto);
sock.connect(in);
sock.send('hello socket world', 0, in);
sock.close;
```



## Slate


This uses fairly verbose and low level messages. This will probably be simplified in the future.


```slate
[ | socket |
  [ | addr stream |
    addr: (Net SocketAddress newOn: '127.0.0.1:256').
    socket: (Net Socket newFor: addr domain type: Net Socket Types Stream protocol: Net Socket Protocols Default).
    socket connectTo: addr.
    stream: (Net SocketStream newOn: socket).
    stream nextPutAll: ('hello socket world' as: ByteArray).
    stream flush
  ] ensure: [socket close]
] do.
```



## Smalltalk

{{works with|GNU Smalltalk}}

This is taken from [http://sblinn.jottit.com/GNU_Smalltalk_SimpleEcho_TCP_Server here] with few modification to fit the task better.


```smalltalk
PackageLoader fileInPackage: 'TCP'!

Object subclass: #HelloSocket
  instanceVariableNames: 'ss'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'SimpleEcho'!

!HelloSocket class methodsFor: 'instance creation'!

port: anInteger
  | ses |
  ses := super new.
  ses init: anInteger.
  ^ses
!!

!HelloSocket methodsFor: 'instance initialization'!

init: anInteger
  ss := (TCP.ServerSocket port: anInteger).
  ^self
!!

!HelloSocket methodsFor: 'running'!

run
  | s |
  [
    ss waitForConnection.
    s := (ss accept).
    [self handleSocket: s] fork
  ] repeat
!!

!HelloSocket methodsFor: 'handling'!

handleSocket: s
    | msg |/**
 Sockets in Neko
 Tectonics:
   nekoc sockets.neko
   sudo nc -vulp 256 & sudo neko sockets
*/

var socket_init = $loader.loadprim("std@socket_init", 0);
var socket_new = $loader.loadprim("std@socket_new", 1);
var host_resolve = $loader.loadprim("std@host_resolve", 1);
var socket_connect = $loader.loadprim("std@socket_connect", 3);
var socket_write = $loader.loadprim("std@socket_write", 2);
var socket_close = $loader.loadprim("std@socket_close", 1);

/* Initialize Neko socket API */
socket_init();

/* true; UDP, false; TCP */
var socket = socket_new(true);

var c = socket_connect(socket, host_resolve("localhost"), 1256);
socket_write(socket, "hello socket world");

socket_close(socket);
    msg := 'hello socket world'.
    msg displayOn: s.
    (String with: (Character value: 10)) displayOn: s.
    s flush
!!

Smalltalk at: #helloServer put: (HelloSocket port: 2560).

helloServer run.
```



## Symsyn


```symsyn

 '127.0.0.1' $addr
 connect $addr 256 sok
 'hello socket world' [sok]
 close sok

```



## Tcl


```tcl
set io [socket localhost 256]
puts -nonewline $io "hello socket world"
close $io
```



## Toka


```toka
needs sockets

#! A simple abstraction layer that makes writing trivial servers easy
value| server.socket server.connection server.action |
[ ( n- )   pBind to server.socket ] is server.setSocket
[ ( - )    server.socket pAccept to server.connection ] is server.acceptConnection
[ ( - )    server.connection pClose drop ] is server.closeConnection
[ ( $- )   >r server.connection r> string.getLength pWrite drop ] is server.send
[ ( an- )  server.connection -rot pRead drop ] is server.recieve
[ ( qn- )  swap to server.action server.setSocket
  [ server.acceptConnection server.action invoke server.closeConnection TRUE ] whileTrue ] is server.start

#! The actual server
[ " hello socket world" server.send ] 256 server.start
```



## TXR



```txrlisp
(let* ((server (first (getaddrinfo "localhost" 256)))
       (sock (open-socket server.family sock-stream)))
  (sock-connect sock server)
  (put-string "hello socket world"))
```



## UNIX Shell

Using netcat:

{{libheader|nc}}

```bash
echo "hello socket world" | nc localhost 256
```


When the connection fails, <code>nc</code> exits 1. To see an error message, use <code>nc -v localhost 256</code>.


## Ursa


```ursa
decl port p
p.connect "localhost" 256
out "hello socket world" endl p
p.close
```



## Visual Basic .NET



```vbnet
Imports System
Imports System.IO
Imports System.Net.Sockets

Public Class Program
    Public Shared Sub Main(ByVal args As String[])
        Dim tcp As New TcpClient("localhost", 256)
        Dim writer As New StreamWriter(tcp.GetStream())

        writer.Write("hello socket world")
        writer.Flush()

        tcp.Close()
    End Sub
End Class
```



## X86 Assembly

{{works with|nasm|Linux}}

```asm

;using sockets on linux with the 0x80 inturrprets.
;
;assemble
;  nasm -o socket.o -f elf32 -g socket.asm
;link
;  ld -o socket socket.o
;
;
;Just some assigns for better readability

%assign SOCK_STREAM         1
%assign AF_INET             2
%assign SYS_socketcall      102
%assign SYS_SOCKET          1
%assign SYS_CONNECT         3
%assign SYS_SEND            9
%assign SYS_RECV            10

section .text
  global _start

;--------------------------------------------------
;Functions to make things easier. :]
;--------------------------------------------------
_socket:
  mov [cArray+0], dword AF_INET
  mov [cArray+4], dword SOCK_STREAM
  mov [cArray+8], dword 0
  mov eax, SYS_socketcall
  mov ebx, SYS_SOCKET
  mov ecx, cArray
  int 0x80
  ret

_connect:
  call _socket
  mov dword [sock], eax
  mov dx, si
  mov byte [edi+3], dl
  mov byte [edi+2], dh
  mov [cArray+0], eax     ;sock;
  mov [cArray+4], edi     ;&sockaddr_in;
  mov edx, 16
  mov [cArray+8], edx   ;sizeof(sockaddr_in);
  mov eax, SYS_socketcall
  mov ebx, SYS_CONNECT
  mov ecx, cArray
  int 0x80
  ret

_send:
  mov edx, [sock]
  mov [sArray+0],edx
  mov [sArray+4],eax
  mov [sArray+8],ecx
  mov [sArray+12], dword 0
  mov eax, SYS_socketcall
  mov ebx, SYS_SEND
  mov ecx, sArray
  int 0x80
  ret

_exit:
  push 0x1
  mov eax, 1
  push eax
  int 0x80

_print:
  mov ebx, 1
  mov eax, 4
  int 0x80
  ret
;--------------------------------------------------
;Main code body
;--------------------------------------------------

_start:
  mov esi, szIp
  mov edi, sockaddr_in
  xor eax,eax
  xor ecx,ecx
  xor edx,edx
  .cc:
    xor   ebx,ebx
  .c:
    lodsb
    inc   edx
    sub   al,'0'
    jb   .next
    imul ebx,byte 10
    add   ebx,eax
    jmp   short .c
  .next:
    mov   [edi+ecx+4],bl
    inc   ecx
    cmp   ecx,byte 4
    jne   .cc

  mov word [edi], AF_INET
  mov esi, szPort
  xor eax,eax
  xor ebx,ebx
  .nextstr1:
    lodsb
    test al,al
    jz .ret1
    sub   al,'0'
    imul ebx,10
    add   ebx,eax
    jmp   .nextstr1
  .ret1:
    xchg ebx,eax
    mov [sport], eax

  mov si, [sport]
  call _connect
  cmp eax, 0
  jnz short _fail
  mov eax, msg
  mov ecx, msglen
  call _send
  call _exit

_fail:
  mov edx, cerrlen
  mov ecx, cerrmsg
  call _print
  call _exit


_recverr:
  call _exit
_dced:
  call _exit

section .data
cerrmsg      db 'failed to connect :(',0xa
cerrlen      equ $-cerrmsg
msg          db 'Hello socket world!',0xa
msglen       equ $-msg

szIp         db '127.0.0.1',0
szPort       db '256',0

section .bss
sock         resd 1
;general 'array' for syscall_socketcall argument arg.
cArray       resd 1
             resd 1
	     resd 1
             resd 1

;send 'array'.
sArray      resd 1
            resd 1
            resd 1
            resd 1
;duh?
sockaddr_in resb 16
;..
sport       resb 2
buff        resb 1024

```


{{works with|MASM}}

Operates in non-blocking mode.

```asm

.586
.model flat,stdcall
option casemap:none

include /masm32/include/windows.inc
include /masm32/include/user32.inc
include /masm32/include/kernel32.inc
include /masm32/include/ws2_32.inc

includelib /masm32/lib/user32.lib
includelib /masm32/lib/kernel32.lib
includelib /masm32/lib/ws2_32.lib

WinMain proto :DWORD,:DWORD,:DWORD,:DWORD


.data
   	ClassName 	db "MainWinClass",0
   	AppName  	db "Async Sockets",0
   	szSockStr	db "Hello socket world!",13,10,0
   	szIp		db "127.0.0.1",0
   	port		dd 256

   	wsa			WSADATA <>
   	sa			sockaddr_in <>

.data?
   	hInstance 	dd ?
   	CommandLine dd ?
   	sock		dd ?

.const
WM_SOCK			equ WM_USER+100

.code
start:
	invoke WSAStartup, 200h, addr wsa
	.if eax!=NULL
		invoke ExitProcess, eax
	.else
		invoke GetModuleHandle, NULL
		mov    hInstance,eax
		invoke GetCommandLine
		mov    CommandLine,eax
		invoke WinMain, hInstance,NULL,CommandLine, SW_SHOWDEFAULT
		invoke ExitProcess,eax
	.endif

WinMain proc hInst:HINSTANCE,hPrevInst:HINSTANCE,CmdLine:LPSTR,CmdShow:DWORD
	LOCAL wc:WNDCLASSEX
	LOCAL msg:MSG
	LOCAL hwnd:HWND

	mov   wc.cbSize,SIZEOF WNDCLASSEX
	mov   wc.style, CS_HREDRAW or CS_VREDRAW
	mov   wc.lpfnWndProc, OFFSET WndProc
	mov   wc.cbClsExtra,NULL
	mov   wc.cbWndExtra,NULL
	push  hInstance
	pop   wc.hInstance
	mov   wc.hbrBackground,COLOR_BTNFACE+1
	mov   wc.lpszMenuName,NULL
	mov   wc.lpszClassName,OFFSET ClassName

	invoke LoadIcon,NULL,IDI_APPLICATION
	mov   wc.hIcon,eax
	mov   wc.hIconSm,eax

	invoke LoadCursor,NULL,IDC_ARROW
	mov   wc.hCursor,eax

	invoke RegisterClassEx, addr wc
	INVOKE CreateWindowEx,NULL,ADDR ClassName,ADDR AppName,\
           WS_OVERLAPPEDWINDOW,CW_USEDEFAULT,\
           CW_USEDEFAULT,CW_USEDEFAULT,CW_USEDEFAULT,NULL,NULL,\
           hInst,NULL
	mov   hwnd,eax

	invoke ShowWindow, hwnd,SW_SHOWNORMAL
	invoke UpdateWindow, hwnd

	.WHILE TRUE
		invoke GetMessage, ADDR msg,NULL,0,0
		.BREAK .IF (!eax)
		invoke TranslateMessage, ADDR msg
		invoke DispatchMessage, ADDR msg
	.ENDW

	mov     eax,msg.wParam
	ret
WinMain endp

WndProc proc hWnd:HWND, uMsg:UINT, wParam:WPARAM, lParam:LPARAM

	.IF uMsg==WM_DESTROY
		invoke PostQuitMessage,NULL
	.ELSEIF uMsg==WM_CREATE
		invoke socket, AF_INET,SOCK_STREAM, 0
		.if eax==INVALID_SOCKET
			;error
		.endif
		mov sock, eax
		invoke WSAAsyncSelect, sock, hWnd, WM_SOCK, FD_CONNECT or FD_CLOSE
		.if eax==INVALID_SOCKET
			;error!
		.endif
		mov sa.sin_family, AF_INET
		invoke inet_addr, addr szIp
		mov sa.sin_addr, eax
		invoke htons, port
		mov sa.sin_port, ax
		invoke connect, sock, addr sa, sizeof sa
		.if eax==SOCKET_ERROR
			invoke WSAGetLastError
			.if eax!=WSAEWOULDBLOCK
				;real error.
			.endif
		.endif
	.elseif uMsg==WM_SOCK
		mov edx, lParam
		.if dx==FD_CONNECT
			shr edx, 16
			.if dx==NULL
				invoke lstrlen, addr szSockStr
				invoke send, sock, addr szSockStr, eax, 0
			.else
				;error
			.endif
		.elseif dx==FD_CLOSE
			shr edx, 16
			.if dx==NULL
				invoke SendMessage, hWnd, WM_DESTROY, 0, 0
			.endif
		.endif
	.ELSE
		invoke DefWindowProc,hWnd,uMsg,wParam,lParam
		ret
	.ENDIF

	xor eax,eax
	ret
WndProc endp


end start

```


{{works with|MASM}}

This example works in blocking mode.

```asm

.586
.model flat,stdcall
option casemap:none

include /masm32/include/windows.inc
include /masm32/include/user32.inc
include /masm32/include/kernel32.inc
include /masm32/include/ws2_32.inc

includelib /masm32/lib/user32.lib
includelib /masm32/lib/kernel32.lib
includelib /masm32/lib/ws2_32.lib

WinMain proto :DWORD,:DWORD,:DWORD,:DWORD


.data
   	ClassName 	db "MainWinClass",0
   	AppName  	db "Blocking Sockets",0
   	szSockStr	db "Hello socket world!",13,10,0
   	szIp		db "127.0.0.1",0
   	port		dd 256

   	wsa			WSADATA <>
   	sa			sockaddr_in <>

.data?
   	hInstance 	dd ?
   	CommandLine dd ?
   	sock		dd ?

.code
start:
	invoke WSAStartup, 200h, addr wsa
	.if eax!=NULL
		invoke ExitProcess, eax
	.else
		invoke GetModuleHandle, NULL
		mov    hInstance,eax
		invoke GetCommandLine
		mov    CommandLine,eax
		invoke WinMain, hInstance,NULL,CommandLine, SW_SHOWDEFAULT
		invoke ExitProcess,eax
	.endif

WinMain proc hInst:HINSTANCE,hPrevInst:HINSTANCE,CmdLine:LPSTR,CmdShow:DWORD
	LOCAL wc:WNDCLASSEX
	LOCAL msg:MSG
	LOCAL hwnd:HWND

	mov   wc.cbSize,SIZEOF WNDCLASSEX
	mov   wc.style, CS_HREDRAW or CS_VREDRAW
	mov   wc.lpfnWndProc, OFFSET WndProc
	mov   wc.cbClsExtra,NULL
	mov   wc.cbWndExtra,NULL
	push  hInstance
	pop   wc.hInstance
	mov   wc.hbrBackground,COLOR_BTNFACE+1
	mov   wc.lpszMenuName,NULL
	mov   wc.lpszClassName,OFFSET ClassName

	invoke LoadIcon,NULL,IDI_APPLICATION
	mov   wc.hIcon,eax
	mov   wc.hIconSm,eax

	invoke LoadCursor,NULL,IDC_ARROW
	mov   wc.hCursor,eax

	invoke RegisterClassEx, addr wc
	INVOKE CreateWindowEx,NULL,ADDR ClassName,ADDR AppName,\
           WS_OVERLAPPEDWINDOW,CW_USEDEFAULT,\
           CW_USEDEFAULT,CW_USEDEFAULT,CW_USEDEFAULT,NULL,NULL,\
           hInst,NULL
	mov   hwnd,eax

	invoke ShowWindow, hwnd,SW_SHOWNORMAL
	invoke UpdateWindow, hwnd

	.WHILE TRUE
		invoke GetMessage, ADDR msg,NULL,0,0
		.BREAK .IF (!eax)
		invoke TranslateMessage, ADDR msg
		invoke DispatchMessage, ADDR msg
	.ENDW

	mov     eax,msg.wParam
	ret
WinMain endp

WndProc proc hWnd:HWND, uMsg:UINT, wParam:WPARAM, lParam:LPARAM

	.IF uMsg==WM_DESTROY
		invoke PostQuitMessage,NULL
	.ELSEIF uMsg==WM_CREATE
		invoke socket, AF_INET,SOCK_STREAM, 0
		.if eax==INVALID_SOCKET
			;error
		.endif
		mov sock, eax
		mov sa.sin_family, AF_INET
		invoke inet_addr, addr szIp
		mov sa.sin_addr, eax
		invoke htons, port
		mov sa.sin_port, ax
		invoke connect, sock, addr sa, sizeof sa
		invoke lstrlen, addr szSockStr
		invoke send, sock, addr szSockStr, eax, 0
	.ELSE
		invoke DefWindowProc,hWnd,uMsg,wParam,lParam
		ret
	.ENDIF

	xor eax,eax
	ret
WndProc endp


end start

```



## zkl


```zkl
var s=Network.TCPClientSocket.connectTo("localhost",256);
s.write("hello socket world");  //-->18
s.close();
```



## Zsh


```zsh
zmodload zsh/net/tcp
ztcp localhost 256
print hello socket world >&$REPLY
```


{{omit from|ACL2}}
{{omit from|Applesoft BASIC|No TCP/IP support on Apple II}}
{{omit from|Batch File|Does not have network access.}}
{{omit from|GUISS}}
{{omit from|Integer BASIC|No TCP/IP support on Apple II}}
{{omit from|Lotus 123 Macro Scripting}}
{{omit from|Locomotive Basic|Does not have network access.}}
{{omit from|Mathematica}}
{{omit from|Maxima}}
{{omit from|ML/I}}
{{omit from|PARI/GP}}
{{omit from|Retro|No socket support}}
{{omit from|TI-83 BASIC|Does not have network access.}}
{{omit from|TI-89 BASIC|Does not have network access.}}
{{omit from|Yorick|Does not have network access.}}
{{omit from|ZX Spectrum Basic|Does not have network access.}}
