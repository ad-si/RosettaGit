+++
title = "Distributed programming"
description = ""
date = 2018-11-13T05:36:38Z
aliases = []
[extra]
id = 2102
[taxonomies]
categories = ["task", "Networking and Web Interaction"]
tags = []
+++

## Task

Write two programs (or one program with two modes) which run on networked computers, and send some messages between them.

The protocol used may be language-specific or not, and should be '''suitable for general distributed programming'''; that is, the ''protocol'' should be generic (not designed just for the particular example application), readily capable of handling the independent communications of many different components of a single application, and the transferring of arbitrary data structures natural for the language.

This task is intended to demonstrate high-level communication facilities beyond just creating [[sockets]].


## Ada

Ada defines facilities for distributed systems in its standard (Annex E, also called DSA).

This example works with PolyORB and the GNAT GPL 2010 compiler from AdaCore.

server.ads:

```Ada
package Server is
   pragma Remote_Call_Interface;
   procedure Foo;
   function Bar return Natural;
end Server;
```


server.adb:

```Ada
package body Server is
   Count : Natural := 0;

   procedure Foo is
   begin
      Count := Count + 1;
   end Foo;

   function Bar return Natural is
   begin
      return Count;
   end Bar;
end Server;
```


client.adb:

```Ada
with Server;
with Ada.Text_IO;

procedure Client is
begin
   Ada.Text_IO.Put_Line ("Calling Foo...");
   Server.Foo;
   Ada.Text_IO.Put_Line ("Calling Bar: " & Integer'Image (Server.Bar));
end Client;
```


required config (dsa.cfg):

```Ada
configuration DSA is
   pragma Starter (None);

   -- Server
   Server_Partition : Partition := (Server);
   procedure Run_Server is in Server_Partition;

   -- Client
   Client_Partition : Partition;
   for Client_Partition'Termination use Local_Termination;
   procedure Client;
   for Client_Partition'Main use Client;
end DSA;
```


compilation:

```txt
$po_gnatdist dsa.cfg
[...]
 ------------------------------
 ---- Configuration report ----
 ------------------------------
Configuration :
   Name        : dsa
   Main        : run_server
   Starter     : none

Partition server_partition
   Main        : run_server
   Units       :
             - server (rci)
             - run_server (normal)
             - polyorb.dsa_p.partitions (rci, from PCS)

   Environment variables :
             - "POLYORB_DSA_NAME_SERVICE"

Partition client_partition
   Main        : client
   Termination : local
   Units       :
             - client (normal)

   Environment variables :
             - "POLYORB_DSA_NAME_SERVICE"

 -------------------------------
[...]
```


preparation (run PolyORB name service):

```txt
$ po_ioc_naming
POLYORB_CORBA_NAME_SERVICE=IOR:010000002b00000049444[...]
POLYORB_CORBA_NAME_SERVICE=corbaloc:iiop:1.2@10.200.[...]
```


You have to set the environment variable POLYORB_DSA_NAME_SERVICE to one of the two values given by po_ioc_naming for the server/client partitions.

running server:

```txt
$ ./server_partition
```


running client:

```txt
$ ./client_partition
Calling Foo...
Calling Bar:  1
$ ./client_partition
Calling Foo...
Calling Bar:  2
```



## AutoHotkey

See [[Distributed program/AutoHotkey]].


## C

Using PVM [[http://www.csm.ornl.gov/pvm/pvm_home.html]
This program is in a sense both a server and a client, depending on if its task is spawned with a command-line argument: if yes, it spawns another task of the same executible on the parallel virtual machine and waits for it to transmit data; if no, it transmits data and is done.

```c
#include <stdio.h>
#include <stdlib.h>
#include <pvm3.h>

int main(int c, char **v)
{
	int tids[10];
	int parent, spawn;
	int i_data, i2;
	double f_data;

	if (c > 1) {
		spawn = pvm_spawn("/tmp/a.out", 0, PvmTaskDefault, 0, 1, tids);
		if (spawn <= 0) {
			printf("Can't spawn task\n");
			return 1;
		}

		printf("Spawning successful\n");

		/* pvm_recv(task_id, msgtag).  msgtag identifies what kind of data it is,
 		 * for here: 1 = (int, double), 2 = (int, int)
		 * The receiving order is intentionally swapped, just to show.
		 * task_id = -1 means "receive from any task"
		 */
		pvm_recv(-1, 2);
		pvm_unpackf("%d %d", &i_data, &i2);
		printf("got msg type 2: %d %d\n", i_data, i2);

		pvm_recv(-1, 1);
		pvm_unpackf("%d %lf", &i_data, &f_data);
		printf("got msg type 1: %d %f\n", i_data, f_data);
	} else {
		parent = pvm_parent();

		pvm_initsend(PvmDataDefault);
		i_data = rand();
		f_data = (double)rand() / RAND_MAX;
		pvm_packf("%d %lf", i_data, f_data);
		pvm_send(parent, 1);	/* send msg type 1 */

		pvm_initsend(PvmDataDefault);
		i2 = rand();
		pvm_packf("%d %d", i_data, i2);
		pvm_send(parent, 2);	/* send msg type 2 */
	}

	pvm_exit();
	return 0;
}
```
Running it: (on PVM console, exe is /tmp/a.out)<lang>pvm> spawn -> /tmp/a.out 1
spawn -> /tmp/a.out 1
[2]
1 successful
t40028
pvm> [2:t40029] EOF
[2:t40028] Spawning successful
[2:t40028] got msg type 2: 1804289383 1681692777
[2:t40028] got msg type 1: 1804289383 0.394383
[2:t40028] EOF
[2] finished
```


## C#
Start the program with "server" parameter to start the server, and "client" to start the client. The client will send data to the server and receive a response. The server will wait for data, display the data received, and send a response.


```c#

using System;
using System.IO;
using System.Net;
using System.Net.Sockets;
using System.Runtime.Serialization.Formatters.Binary;
using System.Threading.Tasks;

using static System.Console;

class DistributedProgramming
{
    const int Port = 555;

    async static Task RunClient()
    {
        WriteLine("Connecting");
        var client = new TcpClient();
        await client.ConnectAsync("localhost", Port);

        using (var stream = client.GetStream())
        {
            WriteLine("Sending loot");
            var data = Serialize(new SampleData());
            await stream.WriteAsync(data, 0, data.Length);

            WriteLine("Receiving thanks");
            var buffer = new byte[80000];
            var bytesRead = await stream.ReadAsync(buffer, 0, buffer.Length);
            var thanks = (string)Deserialize(buffer, bytesRead);
            WriteLine(thanks);
        }

        client.Close();
    }

    async static Task RunServer()
    {
        WriteLine("Listening");
        var listener = new TcpListener(IPAddress.Any, Port);
        listener.Start();
        var client = await listener.AcceptTcpClientAsync();

        using (var stream = client.GetStream())
        {
            WriteLine("Receiving loot");
            var buffer = new byte[80000];
            var bytesRead = await stream.ReadAsync(buffer, 0, buffer.Length);
            var data = (SampleData)Deserialize(buffer, bytesRead);
            WriteLine($"{data.Loot} at {data.Latitude}, {data.Longitude}");

            WriteLine("Sending thanks");
            var thanks = Serialize("Thanks!");
            await stream.WriteAsync(thanks, 0, thanks.Length);
        }

        client.Close();
        listener.Stop();
        Write("Press a key");
        ReadKey();
    }

    static byte[] Serialize(object data)
    {
        using (var mem = new MemoryStream())
        {
            new BinaryFormatter().Serialize(mem, data);
            return mem.ToArray();
        }
    }

    static object Deserialize(byte[] data, int length)
    {
        using (var mem = new MemoryStream(data, 0, length))
        {
            return new BinaryFormatter().Deserialize(mem);
        }
    }

    static void Main(string[] args)
    {
        if (args.Length == 0) return;

        switch (args[0])
        {
            case "client": RunClient().Wait(); break;
            case "server": RunServer().Wait(); break;
        }
    }
}

[Serializable]
class SampleData
{
    public decimal Latitude = 44.33190m;
    public decimal Longitude = 114.84129m;
    public string Loot = "140 tonnes of jade";
}

```



## D

Uses the <b>rpc</b> library:
https://github.com/adamdruppe/misc-stuff-including-D-programming-language-web-stuff/blob/master/rpc.d

This library is not standard, so this code (by Adam D. Ruppe) could and should be rewritten using more standard means.

```d
import arsd.rpc;

struct S1 {
    int number;
    string name;
}

struct S2 {
    string name;
    int number;
}

interface ExampleNetworkFunctions {
    string sayHello(string name);
    int add(in int a, in int b) const pure nothrow;
    S2 structTest(S1);
    void die();
}

// The server must implement the interface.
class ExampleServer : ExampleNetworkFunctions {
    override string sayHello(string name) {
        return "Hello, " ~ name;
    }

    override int add(in int a, in int b) const pure nothrow {
        return a + b;
    }

    override S2 structTest(S1 a) {
        return S2(a.name, a.number);
    }

    override void die() {
        throw new Exception("death requested");
    }

    mixin NetworkServer!ExampleNetworkFunctions;
}

class Client {
    mixin NetworkClient!ExampleNetworkFunctions;
}

void main(in string[] args) {
    import std.stdio;

    if (args.length > 1) {
        auto client = new Client("localhost", 5005);
        // These work like the interface above, but instead of
        // returning the value, they take callbacks for success (where
        // the arg is the retval) and failure (the arg is the
        // exception).
        client.sayHello("whoa", (a) { writeln(a); }, null);
        client.add(1,2, (a){ writeln(a); }, null);
        client.add(10,20, (a){ writeln(a); }, null);
        client.structTest(S1(20, "cool!"),
                          (a){ writeln(a.name, " -- ", a.number); },
                          null);
        client.die(delegate(){ writeln("shouldn't happen"); },
                   delegate(a){ writeln(a); });
        client.eventLoop;
    } else {
        auto server = new ExampleServer(5005);
        server.eventLoop;
    }
}
```



## E

'''Protocol:''' Pluribus

This service cannot be used except by clients which know the URL designating it, messages are encrypted, and the client authenticates the server. However, it is vulnerable to denial-of-service by any client knowing the URL.


###  Server


(The protocol is symmetric; this program is the server only in that it is the one which is started first and exports an object.)


```E
def storage := [].diverge()

def logService {
  to log(line :String) {
    storage.push([timer.now(), line])
  }
  to search(substring :String) {
    var matches := []
    for [time, line] ? (line.startOf(substring) != -1) in storage {
      matches with= [time, line]
    }
    return matches
  }
}

introducer.onTheAir()
def sturdyRef := makeSturdyRef.temp(logService)
println(<captp>.sturdyToURI(sturdyRef))
interp.blockAtTop()
```


This will print the URL of the service and run it until aborted.


###  Client


The URL provided by the server is given as the argument to this program.


```E
def [uri] := interp.getArgs()
introducer.onTheAir()
def sturdyRef := <captp>.sturdyFromURI(uri)
def logService := sturdyRef.getRcvr()

logService <- log("foot")
logService <- log("shoe")

println("Searching...")
when (def result := logService <- search("foo")) -> {
  for [time, line] in result {
    println(`At $time: $line`)
  }
}
```



## Erlang

The protocol is erlang's own

###  Server

srv.erl


```erlang
-module(srv).
-export([start/0, wait/0]).

start() ->
   net_kernel:start([srv,shortnames]),
   erlang:set_cookie(node(), rosetta),
   Pid = spawn(srv,wait,[]),
   register(srv,Pid),
   io:fwrite("~p ready~n",[node(Pid)]),
   ok.

wait() ->
   receive
       {echo, Pid, Any} ->
           io:fwrite("-> ~p from ~p~n", [Any, node(Pid)]),
           Pid ! {hello, Any},
           wait();
       Any -> io:fwrite("Error ~p~n", [Any])
   end.
```



###  Client

client.erl


```erlang
-module(client).
-export([start/0, wait/0]).

start() ->
   net_kernel:start([client,shortnames]),
   erlang:set_cookie(node(), rosetta),
   {ok,[[Srv]]} = init:get_argument(server),
   io:fwrite("connecting to ~p~n", [Srv]),
   {srv, list_to_atom(Srv)} ! {echo,self(), hi},
   wait(),
   ok.

wait() ->
   receive
       {hello, Any} -> io:fwrite("Received ~p~n", [Any]);
       Any -> io:fwrite("Error ~p~n", [Any])
   end.
```


running it (*comes later)
 |erlc srv.erl
 |erl -run srv start -noshell
  srv@agneyam ready
 *-> hi from client@agneyam

 |erlc client.erl
 |erl -run client start -run init stop -noshell -server srv@agneyam
  connecting to "srv@agneyam"
  Received hi


## Factor

The protocol is the one provided by Factor (concurrency.distributed, concurrency.messaging)

Example summary:

- A server node is listening for messages made of natural data types and structures, and simply prettyprint them.

- A client node is sending such data structure: an array of one string and one hashtable (with one key/value pair).


### Server


```factor
USING: concurrency.distributed concurrency.messaging threads io.sockets io.servers ;
QUALIFIED: concurrency.messaging
: prettyprint-message ( -- ) concurrency.messaging:receive . flush prettyprint-message ;
[ prettyprint-message ] "logger" spawn dup name>> register-remote-thread
"127.0.0.1" 9000 <inet4> <node-server> start-server
```


Note: we are using QUALIFIED: with the concurrency.messaging vocabulary because the "receive" word is defined in io.sockets vocabulary too. If someone have a cleaner way to handle this.


### Client


```factor
USING: concurrency.distributed io.sockets ;
QUALIFIED: concurrency.messaging
{ "Hello Remote Factor!" H{ { "key1" "value1" } } }
"127.0.0.1" 9000 <inet4> "logger" <remote-thread> concurrency.messaging:send
```


How to Run:

- Copy/Paste the server code in an instance of Factor Listener

- Copy/Paste the client code in another instance of Factor Listener.

The server node should prettyprint the data structure send by the client: { "Hello Remote Factor!" H{ { "key1" "value1" } } }


## Go


### Standard library net/rpc

Package net/rpc in the Go standard library serializes data with the Go-native "gob" type.  The example here sends only a single floating point number, but the package will send any user-defined data type, including of course structs with multiple fields.

'''Server:'''

```go
package main

import (
    "errors"
    "log"
    "net"
    "net/http"
    "net/rpc"
)

type TaxComputer float64

func (taxRate TaxComputer) Tax(x float64, r *float64) error {
    if x < 0 {
        return errors.New("Negative values not allowed")
    }
    *r = x * float64(taxRate)
    return nil
}

func main() {
    c := TaxComputer(.05)
    rpc.Register(c)
    rpc.HandleHTTP()
    listener, err := net.Listen("tcp", ":1234")
    if err != nil {
        log.Fatal(err)
    }
    http.Serve(listener, nil)
}
```

'''Client:'''

```go
package main

import (
    "fmt"
    "log"
    "net/rpc"
)

func main() {
    client, err := rpc.DialHTTP("tcp", "localhost:1234")
    if err != nil {
        fmt.Println(err)
        return
    }

    amount := 3.
    var tax float64
    err = client.Call("TaxComputer.Tax", amount, &tax)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("Tax on %.2f: %.2f\n", amount, tax)
}
```

```txt

Tax on 3.00: 0.15

```


### gRPC

See http://www.grpc.io/

The default serialization for gRPC is "protocol buffers."  gRPC uses a .proto file to define an interface for the client and server.  The .proto has its own syntax, independent of client and server implementation languages.  Server and client programs here are Go however.

'''.proto:'''

```proto
syntax = "proto3";

service TaxComputer {
  rpc Tax(Amount) returns (Amount) {}
}

message Amount {
  int32 cents = 1;
}
```

'''Server:'''

```go
package main

import (
    "errors"
    "net"

    "golang.org/x/net/context"
    "google.golang.org/grpc"
    "google.golang.org/grpc/grpclog"

    "taxcomputer"
)

type taxServer struct {
    rate float64
}

func (s *taxServer) Tax(ctx context.Context,
    amt *taxcomputer.Amount) (*taxcomputer.Amount, error) {
    if amt.Cents < 0 {
        return nil, errors.New("Negative amounts not allowed")
    }
    return &taxcomputer.Amount{int32(float64(amt.Cents)*s.rate + .5)}, nil
}

func main() {
    listener, err := net.Listen("tcp", ":1234")
    if err != nil {
        grpclog.Fatalf(err.Error())
    }
    grpcServer := grpc.NewServer()
    taxcomputer.RegisterTaxComputerServer(grpcServer, &taxServer{.05})
    grpcServer.Serve(listener)
}
```

'''Client:'''

```go
package main

import (
    "fmt"

    "golang.org/x/net/context"
    "google.golang.org/grpc"
    "google.golang.org/grpc/grpclog"

    "taxcomputer"
)

func main() {
    conn, err := grpc.Dial("localhost:1234", grpc.WithInsecure())
    if err != nil {
        grpclog.Fatalf(err.Error())
    }
    defer conn.Close()
    client := taxcomputer.NewTaxComputerClient(conn)
    amt := &taxcomputer.Amount{300}
    tax, err := client.Tax(context.Background(), amt)
    if err != nil {
        grpclog.Fatalf(err.Error())
    }
    fmt.Println("Tax on", amt.Cents, "cents is", tax.Cents, "cents")
}
```

```txt

Tax on 300 cents is 15 cents

```



### Apache Thrift

See https://thrift.apache.org/

'''.thrift'''

Like gRPC, Thrift requires a language independent interface definition file:

```thrift
service TaxService {
   i32 tax(1: i32 amt)
}
```

'''Server:'''

```go
package main

import (
    "errors"
    "log"

    "git.apache.org/thrift.git/lib/go/thrift"

    "gen-go/tax"
)

type taxHandler float64

func (r taxHandler) Tax(amt int32) (int32, error) {
    if amt < 0 {
        return 0, errors.New("Negative amounts not allowed")
    }
    return int32(float64(amt)*float64(r) + .5), nil
}

func main() {
    transport, err := thrift.NewTServerSocket("localhost:3141")
    if err != nil {
        log.Fatal(err)
    }
    transFac := thrift.NewTTransportFactory()
    protoFac := thrift.NewTCompactProtocolFactory()
    proc := tax.NewTaxServiceProcessor(taxHandler(.05))
    s := thrift.NewTSimpleServer4(proc, transport, transFac, protoFac)
    if err := s.Serve(); err != nil {
        log.Fatal(err)
    }
}
```

'''Client:'''

```go
package main

import (
    "fmt"
    "log"

    "git.apache.org/thrift.git/lib/go/thrift"

    "gen-go/tax"
)

func main() {
    transport, err := thrift.NewTSocket("localhost:3141")
    if err != nil {
        log.Fatal(err)
    }
    if err := transport.Open(); err != nil {
        log.Fatal(err)
    }
    protoFac := thrift.NewTCompactProtocolFactory()
    client := tax.NewTaxServiceClientFactory(transport, protoFac)
    amt := int32(300)
    t, err := client.Tax(amt)
    if err != nil {
        log.Print(err)
    } else {
        fmt.Println("tax on", amt, "is", t)
    }
    transport.Close()
}
```

```txt

tax on 300 is 15

```



## Haskell

See:

* http://www.haskell.org/haskellwiki/HaXR#Server
* http://www.haskell.org/haskellwiki/HaXR#Client

Check license:
http://www.haskell.org/haskellwiki/HaskellWiki:Copyrights


## JavaScript


### Server



```javascript
var net = require('net')

var server = net.createServer(function (c){
  c.write('hello\r\n')
  c.pipe(c) // echo messages back
})

server.listen(3000, 'localhost')

```



### Client


```javascript
var net = require('net')

conn = net.createConnection(3000, '192.168.1.x')

conn.on('connect', function(){
	console.log('connected')
	conn.write('test')
})

conn.on('data', function(msg){
	console.log(msg.toString())
})
```



## Julia

Julia was designed with distributed conmputing. in particular cluster computing, as a primary use target.
If a group of CPUs, including multiple cores on a single machine or a cluster running with paswordless ssh login, is used,
the following can be set up as an example:

```julia
# From Julia 1.0's online docs. File countheads.jl available to all machines:

function count_heads(n)
    c::Int = 0
    for i = 1:n
        c += rand(Bool)
    end
    c
end
```

We then run the following on the primary client:

```julia

using Distributed
@everywhere include_string(Main, $(read("count_heads.jl", String)), "count_heads.jl")

a = @spawn count_heads(100000000) # runs on an available processor
b = @spawn count_heads(100000000) # runs on another available processor

println(fetch(a)+fetch(b)) # total heads of 200 million coin flips, half on each CPU

```
 {{output}}
```txt

100001564

```



## LFE


The protocol used is the one native to Erlang (and thus native to LFE, Lisp Flavored Erlang).

These examples are done completely in the LFE REPL.


### Server


In one terminal window, start up the REPL


```bash

$ ./bin/lfe
Erlang/OTP 17 [erts-6.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

LFE Shell V6.2 (abort with ^G)
>

```


And then enter the following code


```lisp

> (defun get-server-name ()
    (list_to_atom (++ "exampleserver@" (element 2 (inet:gethostname)))))

> (defun start ()
    (net_kernel:start `(,(get-server-name) shortnames))
    (erlang:set_cookie (node) 'rosettaexample)
    (let ((pid (spawn #'listen/0)))
      (register 'serverref pid)
      (io:format "~p ready~n" (list (node pid)))
      'ok))

> (defun listen ()
    (receive
      (`#(echo ,pid ,data)
        (io:format "Got ~p from ~p~n" (list data (node pid)))
        (! pid `#(hello ,data))
        (listen))
      (x
        (io:format "Unexpected pattern: ~p~n" `(,x)))))

```



### Client


In another terminal window, start up another LFE REPL and ender the following code:


```lisp

> (defun get-server-name ()
    (list_to_atom (++ "exampleserver@" (element 2 (inet:gethostname)))))

> (defun send (data)
    (net_kernel:start '(exampleclient shortnames))
    (erlang:set_cookie (node) 'rosettaexample)
    (io:format "connecting to ~p~n" `(,(get-server-name)))
    (! `#(serverref ,(get-server-name)) `#(echo ,(self) ,data))
    (receive
      (`#(hello ,data)
        (io:format "Received ~p~n" `(,data)))
      (x
        (io:format "Unexpected pattern: ~p~n" (list x))))
    'ok)

```


To use this code, simply start the server in the server terminal:


```lisp

> (start)
exampleserver@yourhostname ready
ok
(exampleserver@yourhostname)>

```


Send some messages from the client terminal:


```lisp

> (send "hi there")
connecting to exampleserver@yourhostname
Received "hi there"
ok
(exampleclient@yourhostname)> (send 42)
connecting to exampleserver@yourhostname
Received 42
ok
(exampleclient@yourhostname)> (send #(key value))
connecting to exampleserver@yourhostname
Received {key,value}
ok
(exampleclient@yourhostname)>

```


And check out the results back in the server terminal window:


```lisp

Got "hi there" from exampleclient@yourhostname
Got 42 from exampleclient@yourhostname
Got {key,value} from exampleclient@yourhostname

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
The following sends a request for a random number to be generated on each of two nodes, these are then transmitted back to be assembled into an array with two elements. Omitting the first line, will cause the program to be run on all configured remote computers.

```Mathematica
LaunchKernels[2];
ParallelEvaluate[RandomReal[]]

```



## Nim

```nim
import os, nanomsg

proc sendMsg(s, msg) =
  echo "SENDING \"",msg,"\""
  let bytes = s.send(msg.cstring, msg.len + 1, 0)
  assert bytes == msg.len + 1

proc recvMsg(s) =
  var buf: cstring
  let bytes = s.recv(addr buf, MSG, 0)
  if bytes > 0:
    echo "RECEIVED \"",buf,"\""
    discard freemsg buf

proc sendRecv(s, msg) =
  var to: cint = 100
  discard s.setSockOpt(SOL_SOCKET, RCVTIMEO, addr to, sizeof to)
  while true:
    s.recvMsg
    sleep 1000
    s.sendMsg msg

proc node0(url: string) =
  var s = socket(AF_SP, nanomsg.PAIR)
  assert s >= 0
  let res = s.bindd url
  assert res >= 0
  s.sendRecv "node0"
  discard s.shutdown 0

proc node1(url: string) =
  var s = socket(AF_SP, nanomsg.PAIR)
  assert s >= 0
  let res = s.connect url
  assert res >= 0
  s.sendRecv "node1"
  discard s.shutdown 0

if paramStr(1) == "node0":
  node0 paramStr(2)
elif paramStr(1) == "node1":
  node1 paramStr(2)
```

Usage:

```txt
./pair node0 tcp://127.0.0.1:25000
./pair node1 tcp://127.0.0.1:25000
```


=={{header|Objective-C}}==
Distributed Objects are ''natural'' to Objective-C, and OpenStep and derivated framework offers an easy way of ''using'' remote objects as if it were local. The client must only know the protocol the remote object support. For the rest, calling a remote object's method or local object's method is transparent.

### Server

The server ''vending'' the object with the name <tt>DistributedAction</tt>

<tt>ActionObjectProtocol.h</tt>

```objc>#import <Foundation/Foundation.h

// our protocol allows "sending" "strings", but we can implement
// everything we could for a "local" object
@protocol ActionObjectProtocol
- (NSString *)sendMessage: (NSString *)msg;
@end
```


<tt>ActionObject.h</tt>

```objc>#import <Foundation/Foundation.h

#import "ActionObjectProtocol.h"

@interface ActionObject : NSObject <ActionObjectProtocol>
  // we do not have much for this example!
@end
```


<tt>ActionObject.m</tt>

```objc>#import <Foundation/Foundation.h

#import "ActionObject.h"

@implementation ActionObject
-(NSString *)sendMessage: (NSString *)msg
{
  NSLog(@"client sending message %@", msg);
  return @"server answers ...";
}
@end
```


<tt>server.m</tt>

```objc>#import <Foundation/Foundation.h

#import "ActionObject.h"

int main (void)
{
  @autoreleasepool {

    ActionObject *action = [[ActionObject alloc] init];

    NSSocketPort *port = (NSSocketPort *)[NSSocketPort port];
    // initWithTCPPort: 1234 and other methods are not supported yet
    // by GNUstep
    NSConnection *connect = [NSConnection
  	      connectionWithReceivePort: port
  	      sendPort: port]; // or sendPort: nil

    [connect setRootObject: action];

    /* "vend" the object ActionObject as DistributedAction; on GNUstep
       the Name Server that allows the resolution of the registered name
       is bound to port 538 */
    if (![connect registerName:@"DistributedAction"
  	       withNameServer: [NSSocketPortNameServer sharedInstance] ])
    {
      NSLog(@"can't register the server DistributedAction");
      exit(EXIT_FAILURE);
    }

    NSLog(@"waiting for messages...");

    [[NSRunLoop currentRunLoop] run];

  }
  return 0;
}
```



### Client

<tt>client.m</tt>

```objc>#import <Foundation/Foundation.h

#import "ActionObjectProtocol.h"

int main(void)
{
  @autoreleasepool {

    id <ActionObjectProtocol> action = (id <ActionObjectProtocol>)
      [NSConnection
        rootProxyForConnectionWithRegisteredName: @"DistributedAction"
        host: @"localhost"
        usingNameServer: [NSSocketPortNameServer sharedInstance] ];

    if (action == nil)
    {
      NSLog(@"can't connect to the server");
      exit(EXIT_FAILURE);
    }

    NSArray *args = [[NSProcessInfo processInfo] arguments];

    if ([args count] == 1)
    {
      NSLog(@"specify a message");
      exit(EXIT_FAILURE);
    }

    NSString *msg = args[1];

    // "send" (call the selector "sendMessage:" of the (remote) object
    // action) the first argument's text as msg, store the message "sent
    // back" and then show it in the log
    NSString *backmsg = [action sendMessage: msg];
    NSLog("%@", backmsg);

  }
  return 0;
}
```



## OCaml

Minimalistic distributed logger with synchronous channels using the join calculus on top of OCaml.


###  Server


```ocaml
open Printf

let create_logger () =
  def log(text) & logs(l) =
      printf "Logged: %s\n%!" text;
      logs((text, Unix.gettimeofday ())::l) & reply to log

   or search(text) & logs(l) =
      logs(l) & reply List.filter (fun (line, _) -> line = text) l to search
  in
    spawn logs([]);
    (log, search)

def wait() & finished() = reply to wait

let register name service = Join.Ns.register Join.Ns.here name service

let () =
  let log, search = create_logger () in
    register "log" log;
    register "search" search;
    Join.Site.listen (Unix.ADDR_INET (Join.Site.get_local_addr(), 12345));
    wait ()
```



###  Client



```ocaml
open Printf

let ns_there = Join.Ns.there (Unix.ADDR_INET (Join.Site.get_local_addr(), 12345))

let lookup name = Join.Ns.lookup ns_there name

let log : string -> unit = lookup "log"
let search : string -> (string * float) list = lookup "search"

let find txt =
  printf "Looking for %s...\n" txt;
  List.iter (fun (line, time) ->
               printf "Found: '%s' at t = %f\n%!" (String.escaped line) time)
    (search txt)

let () =
  log "bar";
  find "foo";
  log "foo";
  log "shoe";
  find "foo"
```



## Oz

We show a program that starts a server on a remote machine, exchanges two messages with that server and finally shuts it down.


```oz
declare
  functor ServerCode
  export
     port:Prt
  define
     Stream
     Prt = {NewPort ?Stream}
     thread
	for Request#Reply in Stream do
	   case Request
	   of echo(Data)        then Reply = Data
	   [] compute(Function) then Reply = {Function}
	   end
	end
     end
  end

  %% create the server on some machine
  %% (just change "localhost" to some machine
  %% that you can use with a passwordless rsh login
  %% and that has the same Mozart version installed)
  RM = {New Remote.manager init(host:localhost)}

  %% execute the code encapsulated in the ServerCode functor
  Server = {RM apply(ServerCode $)}

  %% Shortcut: send a message to Server and receive a reply
  fun {Send X}
     {Port.sendRecv Server.port X}
  end
in
  %% echo
  {System.showInfo "Echo reply: "#{Send echo(hello)}}

  %% compute
  {System.showInfo "Result of computation: "#
   {Send compute(fun {$} 8 div 4 end)}}

  %% shut down server
  {RM close}
```



## Perl

Using Data::Dumper and Safe to transmit arbitrary data structures as serialized text between hosts.  Same code works as both sender and receiver.

```Perl
use Data::Dumper;
use IO::Socket::INET;
use Safe;

sub get_data {
	my $sock = new IO::Socket::INET
		LocalHost =>	"localhost",
		LocalPort =>	"10000",
		Proto =>	"tcp",
		Listen =>	1,
		Reuse =>	1;
	unless ($sock) { die "Socket creation failure" }
	my $cli = $sock->accept();

	# of course someone may be tempted to send you 'system("rm -rf /")',
	# to be safe(r), use Safe::
	my $safe = new Safe;
	my $x = $safe->reval(join("", <$cli>));
	close $cli;
	close $sock;
	return $x;
}

sub send_data {
	my $host = shift;
	my $data = shift;
	my $sock = new IO::Socket::INET
		PeerAddr =>	"$host:10000",
		Proto =>	"tcp",
		Reuse =>	1;

	unless ($sock) { die "Socket creation failure" }

	print $sock Data::Dumper->Dump([$data]);
	close $sock;
}

if (@ARGV) {
	my $x = get_data();
	print "Got data\n", Data::Dumper->Dump([$x]);
} else {
	send_data('some_host', { a=>100, b=>[1 .. 10] });
}
```


## Perl 6

Server listens for JSON encoded messages. It processes requests for set|get|dump. 'set' stores a message, 'get' returns message, 'dump' returns all stored messages. Optional parameters for ip address and port.

Server.p6:

```txt
./server.p6 --usage
Usage:
  server.p6 [--server=<Any>] [--port=<Any>]
```


```perl6
#!/usr/bin/env perl6
use JSON::Fast ;
sub MAIN( :$server='0.0.0.0' , :$port=3333 ) {
  my %db ;
  react {
    whenever IO::Socket::Async.listen( $server , $port ) -> $conn {
        whenever $conn.Supply.lines -> $line {
            my %response = 'status' => '' ;
            my $msg = from-json $line ;
            say $msg.perl ;
            given $msg{"function"} {
                when 'set' {
                    %db{ $msg<topic> } = $msg<message> ;
                    %response<status> = 'ok' ;
                }
                when 'get' {
                    %response<topic> = $msg<topic> ;
                    %response<message> = %db{ $msg<topic> } ;
                    %response<status> = 'ok' ;
                }
                when 'dump' {
                    %response = %db ;
                }
                when 'delete' {
                    %db{ $msg<topic> }:delete;
                    %response<status> = 'ok' ;
                }
            }
            $conn.print( to-json(%response, :!pretty) ~ "\n" ) ;
            LAST { $conn.close ; }
            QUIT { default { $conn.close ; say "oh no, $_";}}
            CATCH { default { say .^name, ': ', .Str ,  " handled in $?LINE";}}
        }
    }
  }
}
```

client.p6:

```txt
Usage:
  client.p6 [--server=<Any>] [--port=<Any>] [--json=<Any>] set <topic> [<message>]
  client.p6 [--server=<Any>] [--port=<Any>] get <topic>
  client.p6 [--server=<Any>] [--port=<Any>] dump
```


```perl6
#!/usr/bin/env perl6
use JSON::Fast ;
multi MAIN('set', $topic,  $message='', :$server='localhost', :$port='3333', :$json='') {
    my %msg = function => 'set' , topic=> $topic , message=> $message ;
    %msg{"message"} = from-json( $json ) if $json ;
    sendmsg( %msg , $server, $port) ;
}
multi MAIN('get', $topic, :$server='localhost', :$port='3333') {
    my %msg = function => 'get' , topic=> $topic ;
    sendmsg( %msg , $server, $port) ;
}
multi MAIN('delete', $topic, :$server='localhost', :$port='3333') {
    my %msg = function => 'delete' , topic=> $topic ;
    sendmsg( %msg , $server, $port) ;
}
multi MAIN('dump', :$server='localhost', :$port='3333') {
    my %msg = function => 'dump'  ;
    sendmsg( %msg , $server, $port) ;
}
sub sendmsg( %msg , $server, $port){
    my $conn = await IO::Socket::Async.connect( $server , $port );
    $conn.print: to-json( %msg,:!pretty)~"\n";
    react {
        whenever $conn.Supply -> $data {
            print $data;
            $conn.close;
        }
    }
}
```

examples:

```txt
echo '{"function":"set","topic":"push","message":["perl5","perl6","rakudo"]}' | nc localhost 3333

./client.p6 set version perl6
{"status": "ok"}
./client.p6 get version
{"status": "ok","topic": "version","message": "perl6"}
./client.p6 --json='["one","two","three"]' set mylist
{"status": "ok"}
./client.p6 dump
{"push": ["perl5","perl6","rakudo"],"version": "perl6","mylist": ["one","two","three"]}
./client.p6 delete version
{"status": "ok"}

server output:
${:function("set"), :message($["perl5", "perl6", "rakudo"]), :topic("push")}
${:function("set"), :message("perl6"), :topic("version")}
${:function("get"), :topic("version")}
${:function("set"), :message($["one", "two", "three"]), :topic("mylist")}
${:function("dump")}
${:function("delete"), :topic("version")}

```



## PicoLisp


### Server


```PicoLisp
(task (port 12321)                     # Background server task
   (let? Sock (accept @)
      (unless (fork)                   # Handle request in child process
         (in Sock
            (while (rd)                # Handle requests
               (out Sock
                  (pr (eval @)) ) ) )  # Evaluate and send reply
         (bye) )                       # Exit child process
      (close Sock) ) )                 # Close socket in parent process
```


### Client


```PicoLisp
(let? Sock (connect "localhost" 12321)
   (out Sock (pr '*Pid))               # Query PID from server
   (println 'PID (in Sock (rd)))       # Receive and print reply
   (out Sock (pr '(* 3 4)))            # Request some calculation
   (println 'Result (in Sock (rd)))    # Print result
   (close Sock) )                      # Close connection to server
```

Output:

```txt
PID 18372
Result 12
```



## Python

=== XML-RPC ===
'''Protocol:''' XML-RPC


### = Server =


```python
#!/usr/bin/env python
# -*- coding: utf-8 -*-

import SimpleXMLRPCServer

class MyHandlerInstance:
    def echo(self, data):
        '''Method for returning data got from client'''
        return 'Server responded: %s' % data

    def div(self, num1, num2):
        '''Method for divide 2 numbers'''
        return num1/num2

def foo_function():
    '''A function (not an instance method)'''
    return True

HOST = "localhost"
PORT = 8000

server = SimpleXMLRPCServer.SimpleXMLRPCServer((HOST, PORT))

# register built-in system.* functions.
server.register_introspection_functions()

# register our instance
server.register_instance(MyHandlerInstance())

# register our function as well
server.register_function(foo_function)

try:
    # serve forever
    server.serve_forever()
except KeyboardInterrupt:
    print 'Exiting...'
    server.server_close()
```



### = Client =


```python
#!/usr/bin/env python
# -*- coding: utf-8 -*-

import xmlrpclib

HOST = "localhost"
PORT = 8000

rpc = xmlrpclib.ServerProxy("http://%s:%d" % (HOST, PORT))

# print what functions does server support
print 'Server supports these functions:',
print ' '.join(rpc.system.listMethods())

# echo something
rpc.echo("We sent this data to server")

# div numbers
print 'Server says: 8 / 4 is: %d' % rpc.div(8, 4)

# control if foo_function returns True
if rpc.foo_function():
    print 'Server says: foo_function returned True'
```



### HTTP

'''Protocol:''' HTTP


### = Server =


```python
#!/usr/bin/python
# -*- coding: utf-8 -*-

import BaseHTTPServer

HOST = "localhost"
PORT = 8000

# we just want to write own class, we replace do_GET method. This could be extended, I just added basics
# see; http://docs.python.org/lib/module-BaseHTTPServer.html
class MyHTTPHandler(BaseHTTPServer.BaseHTTPRequestHandler):
    def do_GET(self):
        # send 200 (OK) message
        self.send_response(200)
        # send header
        self.send_header("Content-type", "text/html")
        self.end_headers()

        # send context
        self.wfile.write("<html><head><title>Our Web Title</title></head>")
        self.wfile.write("<body><p>This is our body. You wanted to visit <b>%s</b> page</p></body>" % self.path)
        self.wfile.write("</html>")

if __name__ == '__main__':
    server = BaseHTTPServer.HTTPServer((HOST, PORT), MyHTTPHandler)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print 'Exiting...'
        server.server_close()
```



### = Client =


```python
#!/usr/bin/python
# -*- coding: utf-8 -*-

import httplib

HOST = "localhost"
PORT = 8000

conn = httplib.HTTPConnection(HOST, PORT)
conn.request("GET", "/somefile")

response = conn.getresponse()
print 'Server Status: %d' % response.status

print 'Server Message: %s' % response.read()
```


===Socket, Pickle format===

'''Protocol:''' raw socket / pickle format

This example builds a very basic RPC mechanism on top of sockets and the [http://docs.python.org/library/pickle.html#module-pickle pickle module]. Please note that the pickle module is not secure - a malicious client can build malformed data to execute arbitrary code on the server. If untrusted clients can access the server, the [http://docs.python.org/library/json.html json module] could be used as a substitute, but we lose the ability to transfer arbitrary Python objects that way.


### = Server =


```python
#!/usr/bin/python
# -*- coding: utf-8 -*-

import SocketServer
import pickle

HOST = "localhost"
PORT = 8000

class RPCServer(SocketServer.ThreadingMixIn, SocketServer.TCPServer):
    # The object_to_proxy member should be set to the object we want
    # methods called on. Unfortunately, we can't do this in the constructor
    # because the constructor should not be overridden in TCPServer...

    daemon_threads = True

class RPCHandler(SocketServer.StreamRequestHandler):
    def handle(self):
        in_channel = pickle.Unpickler(self.rfile)
        out_channel = pickle.Pickler(self.wfile, protocol=2)
        while True:
            try:
                name, args, kwargs = in_channel.load()
                print 'got %s %s %s' % (name, args, kwargs)
            except EOFError:
                # EOF means we're done with this request.
                # Catching this exception to detect EOF is a bit hackish,
                # but will work for a quick demo like this
                break
            try:
                method = getattr(self.server.object_to_proxy, name)
                result = method(*args, **kwargs)
            except Exception, e:
                out_channel.dump(('Error',e))
            else:
                out_channel.dump(('OK',result))

class MyHandlerInstance(object):
    def echo(self, data):
        '''Method for returning data got from client'''
        return 'Server responded: %s' % data

    def div(self, dividend, divisor):
        '''Method to divide 2 numbers'''
        return dividend/divisor

    def is_computer_on(self):
        return True

if __name__ == '__main__':
    rpcserver = RPCServer((HOST, PORT), RPCHandler)
    rpcserver.object_to_proxy = MyHandlerInstance()
    try:
        rpcserver.serve_forever()
    except KeyboardInterrupt:
        print 'Exiting...'
        rpcserver.server_close()

```



### = Client =


```python
#!/usr/bin/python
# -*- coding: utf-8 -*-

import socket
import pickle

HOST = "localhost"
PORT = 8000

class RPCClient(object):
    def __init__(self, host, port):
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.connect((host, port))
        self.rfile = self.socket.makefile('rb')
        self.wfile = self.socket.makefile('wb')
        self.in_channel = pickle.Unpickler(self.rfile)
        self.out_channel = pickle.Pickler(self.wfile, protocol=2)

    def _close(self):
        self.socket.close()
        self.rfile.close()
        self.wfile.close()

    # Make calling remote methods easy by overriding attribute access.
    # Accessing any attribute on our instances will give a proxy method that
    # calls the method with the same name on the remote machine.
    def __getattr__(self, name):
        def proxy(*args, **kwargs):
            self.out_channel.dump((name, args, kwargs))
            self.wfile.flush() # to make sure the server won't wait forever
            status, result = self.in_channel.load()
            if status == 'OK':
                return result
            else:
                raise result

        return proxy

if __name__ == '__main__':
    # connect to server and send data
    rpcclient = RPCClient(HOST, PORT)

    print 'Testing the echo() method:'
    print rpcclient.echo('Hello world!')
    print
    print 'Calculating 42/2 on the remote machine:'
    print rpcclient.div(42, 2)
    print
    print 'is_computer_on on the remote machine returns:'
    print rpcclient.is_computer_on()
    print
    print 'Testing keyword args:'
    print '42/2 is:', rpcclient.div(divisor=2, dividend=42)
    rpcclient._close()
    del rpcclient
```



### Pyro

'''Note:''' You should install Pyro (http://pyro.sourceforge.net) first and run '''pyro-ns''' binary to run code below.


### = Server =


```python
#!/usr/bin/python
# -*- coding: utf-8 -*-

import Pyro.core
import Pyro.naming

# create instance that will return upper case
class StringInstance(Pyro.core.ObjBase):
    def makeUpper(self, data):
        return data.upper()

class MathInstance(Pyro.core.ObjBase):
    def div(self, num1, num2):
        return num1/num2

if __name__ == '__main__':
    server = Pyro.core.Daemon()
    name_server = Pyro.naming.NameServerLocator().getNS()
    server.useNameServer(name_server)
    server.connect(StringInstance(), 'string')
    server.connect(MathInstance(), 'math')
    try:
        server.requestLoop()
    except KeyboardInterrupt:
        print 'Exiting...'
        server.shutdown()
```



### = Client =


```python
#!/usr/bin/python
# -*- coding: utf-8 -*-

import Pyro.core

DATA = "my name is eren"
NUM1 = 10
NUM2 = 5

string = Pyro.core.getProxyForURI("PYRONAME://string")
math = Pyro.core.getProxyForURI("PYRONAME://math")

print 'We sent: %s' % DATA
print 'Server responded: %s\n' % string.makeUpper(DATA)

print 'We sent two numbers to divide: %d and %d' % (NUM1, NUM2)
print 'Server responded the result: %s' % math.div(NUM1, NUM2)
```



###  Spread

'''Note:''' You should install Spread (http://www.spread.org) and its python bindings (http://www.python.org/other/spread/)


### = Server =

You don't need any code for server. You should start "spread" daemon by typing "spread -c /etc/spread.conf -n localhost". If you want more configuration, look at /etc/spread.conf.

After starting daemon, if you want to make sure that it is running, enter '''spuser -s 4803''' command where 4803 is your port set in spread.conf, you will see prompt, type '''j user''', you should see something like this message: ''Received REGULAR membership for group test with 3 members, where I am member 2''

==== Client (Listener) ====

```python
#!/usr/bin/python
# -*- coding: utf-8 -*-

import spread

PORT = '4803'

# connect spread daemon
conn = spread.connect(PORT)
# join the room
conn.join('test')

print 'Waiting for messages... If you want to stop this script, please stop spread daemon'
while True:
    recv = conn.receive()
    if hasattr(recv, 'sender') and hasattr(recv, 'message'):
        print 'Sender: %s' % recv.sender
        print 'Message: %s' % recv.message
```


==== Client (Sender) ====

```python
#!/usr/bin/python
# -*- coding: utf-8 -*-

import spread

PORT = '4803'

conn = spread.connect(PORT)
conn.join('test')

conn.multicast(spread.RELIABLE_MESS, 'test', 'hello, this is message sent from python')
conn.disconnect()
```



## Racket

Server and client in the same piece of code, running a useless (fib 42) computation, four times, on four hosts (which all happen to be "localhost", but that can change, of course).


```racket

#lang racket/base
(require racket/place/distributed racket/place)

(define (fib n)
  (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))

(provide work)
(define (work)
  (place ch
    (place-channel-put ch (fib (place-channel-get ch)))))

(module+ main
  (define places
    (for/list ([host '("localhost" "localhost" "localhost" "localhost")]
               [port (in-naturals 12345)])
      (define-values [node place]
        (spawn-node-supervise-place-at host #:listen-port port #:thunk #t
                                       (quote-module-path "..") 'work))
      place))
  (message-router
   (after-seconds 1
     (for ([p places]) (*channel-put p 42))
     (printf "Results: ~s\n" (map *channel-get places))
     (exit))))

```



## Ruby

Uses the distributed Ruby (dRuby) from the standard library. The "druby:" protocol uses TCP/IP sockets for communication.

'''Server'''

```ruby
require 'drb/drb'

# The URI for the server to connect to
URI="druby://localhost:8787"

class TimeServer

  def get_current_time
    return Time.now
  end

end

# The object that handles requests on the server
FRONT_OBJECT = TimeServer.new

$SAFE = 1   # disable eval() and friends

DRb.start_service(URI, FRONT_OBJECT)
# Wait for the drb server thread to finish before exiting.
DRb.thread.join
```


'''Client'''

```ruby
require 'drb/drb'

# The URI to connect to
SERVER_URI = "druby://localhost:8787"

# Start a local DRbServer to handle callbacks.
#
# Not necessary for this small example, but will be required
# as soon as we pass a non-marshallable object as an argument
# to a dRuby call.
DRb.start_service

timeserver = DRbObject.new_with_uri(SERVER_URI)
puts timeserver.get_current_time
```



## Tcl

A rudimentary IRC Server

```tcl
proc main {} {
    global connections
    set connections [dict create]
    socket -server handleConnection 12345
    vwait dummyVar ;# enter the event loop
}

proc handleConnection {channel clientaddr clientport} {
    global connections
    dict set connections $channel address "$clientaddr:$clientport"
    fconfigure $channel -buffering line
    fileevent $channel readable [list handleMessage $channel]
}

proc handleMessage {channel} {
    global connections
    if {[gets $channel line] == -1} {
        disconnect $channel
    } else {
        if {[string index [string trimleft $line] 0] eq "/"} {
            set words [lassign [split [string trim $line]] command]
            handleCommand $command $words $channel
        } else {
            echo $line $channel
        }
    }
}

proc disconnect {channel} {
    global connections
    dict unset connections $channel
    fileevent $channel readable ""
    close $channel
}

proc handleCommand {command words channel} {
    global connections
    switch -exact -- [string tolower $command] {
        /nick {
            dict set connections $channel nick [lindex $words 0]
        }
        /quit {
            echo bye $channel
            disconnect $channel
        }
        default {
            puts $channel "\"$command\" not implemented"
        }
    }
}

proc echo {message senderchannel} {
    global connections
    foreach channel [dict keys $connections] {
        if {$channel ne $senderchannel} {
            set time [clock format [clock seconds] -format "%T"]
            set nick [dict get $connections $channel nick]
            puts $channel [format "\[%s\] %s: %s" $time $nick $message]
        }
    }
}

main
```

Client

```tcl
proc main {} {
    global argv argc
    if {$argc != 2} {
        error "usage: [info script] serveraddress serverport"
    }
    connect {*}$argv
    vwait dummyVar
}

proc connect {addr port} {
    global sock
    set sock [socket $addr $port]
    fconfigure $sock -buffering line
    fileevent $sock readable getFromServer
    fileevent stdin readable sendToServer
}

proc getFromServer {} {
    global sock
    if {[gets $sock line] == -1} {
        puts "disconnected..."
        exit
    } else {
        puts $line
    }
}

proc sendToServer {} {
    global sock
    set msg [string trim [gets stdin]]
    if {[string length $msg] > 0} {
        puts $sock $msg
    }
}

main
```



## UnixPipes

Uses netcat and a buffer to cycle the server shell's stdout back to netcat's stdin.


### Server

```bash>:
/tmp/buffer
tail -f /tmp/buffer | nc -l 127.0.0.1 1234 | sh >/tmp/buffer 2>&1
```


Limitations:

* The server can accept only one connection (but continues to run, not exit, after this connection dies).
* With some systems, <code>tail -f</code> might be slow to notice changes to /tmp/buffer.


### Client


```bash>nc 127.0.0.1 1234</lang


Now you can enter commands in the client terminal and get the output back through the same connection.

