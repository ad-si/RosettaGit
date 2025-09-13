+++
title = "Chat server"
description = ""
date = 2019-09-01T17:02:00Z
aliases = []
[extra]
id = 8970
[taxonomies]
categories = ["Networking and Web Interaction", "task"]
tags = []
+++

## Task

### Task:
Write a server for a minimal text based chat.

People should be able to connect via ‘telnet’, sign on with a nickname, and type messages which will then be seen by all other connected users. Arrivals and departures of chat members should generate appropriate notification messages.





## Ada

{{libheader|AdaSockets}}


```Ada
with Ada.Containers.Vectors;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Sockets;          use Sockets;

procedure Chat_Server is

   package Client_Vectors is new Ada.Containers.Vectors
     (Element_Type => Socket_FD, Index_Type => Positive);
   All_Clients : Client_Vectors.Vector;

   procedure Write (S : String) is
      procedure Output (Position : Client_Vectors.Cursor) is
         Sock : Socket_FD := Client_Vectors.Element (Position);
      begin
         Put_Line (Sock, S);
      end Output;
   begin
      All_Clients.Iterate (Output'Access);
   end Write;

   task type Client_Task is
      entry Start (FD : Socket_FD);
   end Client_Task;

   task body Client_Task is
      Sock    : Socket_FD;
      Sock_ID : Positive;
      Name    : Unbounded_String;
   begin
      select
         accept Start (FD : Socket_FD) do
            Sock := FD;
         end Start;
      or
         terminate;
      end select;

      while Name = Null_Unbounded_String loop
         Put (Sock, "Enter Name:");
         Name := To_Unbounded_String (Get_Line (Sock));
      end loop;
      Write (To_String (Name) & " joined.");
      All_Clients.Append (Sock);
      Sock_ID := All_Clients.Find_Index (Sock);
      loop
         declare
            Input : String := Get_Line (Sock);
         begin
            Write (To_String (Name) & ": " & Input);
         end;
      end loop;
   exception
      when Connection_Closed =>
         Put_Line ("Connection closed");
         Shutdown (Sock, Both);
         All_Clients.Delete (Sock_ID);
         Write (To_String (Name) & " left.");
   end Client_Task;

   Accepting_Socket : Socket_FD;
   Incoming_Socket  : Socket_FD;

   type Client_Access is access Client_Task;
   Dummy : Client_Access;
begin
   if Argument_Count /= 1 then
      Raise_Exception (Constraint_Error'Identity,
                       "Usage: " & Command_Name & " port");
   end if;
   Socket (Accepting_Socket, PF_INET, SOCK_STREAM);
   Setsockopt (Accepting_Socket, SOL_SOCKET, SO_REUSEADDR, 1);
   Bind (Accepting_Socket, Positive'Value (Argument (1)));
   Listen (Accepting_Socket);
   loop
      Put_Line ("Waiting for new connection");
      Accept_Socket (Accepting_Socket, Incoming_Socket);
      Put_Line ("New connection acknowledged");

      Dummy := new Client_Task;
      Dummy.Start (Incoming_Socket);
   end loop;
end Chat_Server;
```



## C

C has no built-in networking functions, but the POSIX library does provide some low-level networking functions.  The functions of interest relating to sockets include ''bind'', ''listen'', ''select'', ''accept'', ''read'', ''write'' and ''close''.

The example below was compiled on Cygwin, and accepts PuTTY connections under the RAW protocol.

A glitch occurs if a connection is made using the Telnet protocol - user names are preceded by garbled text.


```c
#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <netinet/in.h>
#include <netinet/ip.h>

int tsocket;
struct sockaddr_in tsockinfo;
fd_set status, current;
void ClientText(int handle, char *buf, int buf_len);

struct client
{
    char buffer[4096];
    int pos;
    char name[32];
} *connections[FD_SETSIZE];

void AddConnection(int handle)
{
    connections[handle] = malloc(sizeof(struct client));
    connections[handle]->buffer[0] = '\0';
    connections[handle]->pos = 0;
    connections[handle]->name[0] = '\0';
}

void CloseConnection(int handle)
{
    char buf[512];
    int j;

    FD_CLR(handle, &status);

    if (connections[handle]->name[0])
    {
        sprintf(buf, "* Disconnected: %s\r\n", connections[handle]->name);

        for (j = 0; j < FD_SETSIZE; j++)
        {
            if (handle != j && j != tsocket && FD_ISSET(j, &status))
            {
                if (write(j, buf, strlen(buf)) < 0)
                {
                    CloseConnection(j);
                }
            }
        }
    } else
    {
        printf ("-- Connection %d disconnected\n", handle);
    }
    if (connections[handle])
    {
        free(connections[handle]);
    }
    close(handle);
}

void strip(char *buf)
{
    char *x;

    x = strchr(buf, '\n');
    if (x) { *x='\0'; }
    x = strchr(buf, '\r');
    if (x) { *x='\0'; }
}

int RelayText(int handle)
{
    char *begin, *end;
    int ret = 0;
    begin = connections[handle]->buffer;
    if (connections[handle]->pos == 4000)
    {
        if (begin[3999] != '\n')
            begin[4000] = '\0';
        else {
            begin[4000] = '\n';
            begin[4001] = '\0';
        }
    } else {
        begin[connections[handle]->pos] = '\0';
    }

    end = strchr(begin, '\n');
    while (end != NULL)
    {
        char output[8000];
        output[0] = '\0';
        if (!connections[handle]->name[0])
        {
            strncpy(connections[handle]->name, begin, 31);
            connections[handle]->name[31] = '\0';

            strip(connections[handle]->name);
            sprintf(output, "* Connected: %s\r\n", connections[handle]->name);
            ret = 1;
        } else
        {
            sprintf(output, "%s: %.*s\r\n", connections[handle]->name,
                    end-begin, begin);
            ret = 1;
        }

        if (output[0])
        {
            int j;
            for (j = 0; j < FD_SETSIZE; j++)
            {
                if (handle != j && j != tsocket && FD_ISSET(j, &status))
                {
                    if (write(j, output, strlen(output)) < 0)
                    {
                        CloseConnection(j);
                    }
                }
            }
        }
        begin = end+1;
        end = strchr(begin, '\n');
    }

    strcpy(connections[handle]->buffer, begin);
    connections[handle]->pos -= begin - connections[handle]->buffer;
    return ret;
}

void ClientText(int handle, char *buf, int buf_len)
{
    int i, j;
    if (!connections[handle])
        return;
    j = connections[handle]->pos;

    for (i = 0; i < buf_len; ++i, ++j)
    {
        connections[handle]->buffer[j] = buf[i];

        if (j == 4000)
        {
            while (RelayText(handle));
            j = connections[handle]->pos;
        }
    }
    connections[handle]->pos = j;

    while (RelayText(handle));
}


int ChatLoop()
{
    int i, j;
    FD_ZERO(&status);

    FD_SET(tsocket, &status);
    FD_SET(0, &status);

    while(1)
    {
        current = status;
        if (select(FD_SETSIZE, &current, NULL, NULL, NULL)==-1)
        {
            perror("Select");
            return 0;
        }
        for (i = 0; i < FD_SETSIZE; ++i)
        {
            if (FD_ISSET(i, &current))
            {
                if (i == tsocket)
                {
                    struct sockaddr_in cliinfo;
                    socklen_t addrlen = sizeof(cliinfo);
                    int handle;
                    handle = accept(tsocket, &cliinfo, &addrlen);
                    if (handle == -1)
                    {
                        perror ("Couldn't accept connection");
                    } else if (handle > FD_SETSIZE)
                    {
                        printf ("Unable to accept new connection.\n");
                        close(handle);
                    }
                    else
                    {
                        if (write(handle, "Enter name: ", 12) >= 0)
                        {
                            printf("-- New connection %d from %s:%hu\n",
                                handle,
                                inet_ntoa (cliinfo.sin_addr),
                                ntohs(cliinfo.sin_port));
                            FD_SET(handle, &status);

                            AddConnection(handle);
                        }
                    }
                } /* Read data, relay to others. */
                else
                {
                    char buf[512];
                    int b;

                    b = read(i, buf, 500);
                    if (b <= 0)
                    {
                        CloseConnection(i);
                    }
                    else
                    {
                        ClientText(i, buf, b);
                    }
                }
            }
        }
    } /* While 1 */
}

int main (int argc, char*argv[])
{
    tsocket = socket(PF_INET, SOCK_STREAM, 0);

    tsockinfo.sin_family = AF_INET;
    tsockinfo.sin_port = htons(7070);
    if (argc > 1)
    {
        tsockinfo.sin_port = htons(atoi(argv[1]));
    }
    tsockinfo.sin_addr.s_addr = htonl(INADDR_ANY);
    printf ("Socket %d on port %hu\n", tsocket, ntohs(tsockinfo.sin_port));

    if (bind(tsocket, &tsockinfo, sizeof(tsockinfo)) == -1)
    {
        perror("Couldn't bind socket");
        return -1;
    }

    if (listen(tsocket, 10) == -1)
    {
        perror("Couldn't listen to port");
    }

    ChatLoop();

    return 0;
}
```


## C#

```c#
using System;
using System.Collections.Generic;
using System.Net.Sockets;
using System.Text;
using System.Threading;

namespace ChatServer {
    class State {
        private TcpClient client;
        private StringBuilder sb = new StringBuilder();

        public string Name { get; }

        public State(string name, TcpClient client) {
            Name = name;
            this.client = client;
        }

        public void Add(byte b) {
            sb.Append((char)b);
        }

        public void Send(string text) {
            var bytes = Encoding.ASCII.GetBytes(string.Format("{0}\r\n", text));
            client.GetStream().Write(bytes, 0, bytes.Length);
        }
    }

    class Program {
        static TcpListener listen;
        static Thread serverthread;
        static Dictionary<int, State> connections = new Dictionary<int, State>();

        static void Main(string[] args) {
            listen = new TcpListener(System.Net.IPAddress.Parse("127.0.0.1"), 4004);
            serverthread = new Thread(new ThreadStart(DoListen));
            serverthread.Start();
        }

        private static void DoListen() {
            // Listen
            listen.Start();
            Console.WriteLine("Server: Started server");

            while (true) {
                Console.WriteLine("Server: Waiting...");
                TcpClient client = listen.AcceptTcpClient();
                Console.WriteLine("Server: Waited");

                // New thread with client
                Thread clientThread = new Thread(new ParameterizedThreadStart(DoClient));
                clientThread.Start(client);
            }
        }

        private static void DoClient(object client) {
            // Read data
            TcpClient tClient = (TcpClient)client;

            Console.WriteLine("Client (Thread: {0}): Connected!", Thread.CurrentThread.ManagedThreadId);
            byte[] bytes = Encoding.ASCII.GetBytes("Enter name: ");
            tClient.GetStream().Write(bytes, 0, bytes.Length);

            string name = string.Empty;
            bool done = false;
            do {
                if (!tClient.Connected) {
                    Console.WriteLine("Client (Thread: {0}): Terminated!", Thread.CurrentThread.ManagedThreadId);
                    tClient.Close();
                    Thread.CurrentThread.Abort();       // Kill thread.
                }

                name = Receive(tClient);
                done = true;

                if (done) {
                    foreach (var cl in connections) {
                        var state = cl.Value;
                        if (state.Name == name) {
                            bytes = Encoding.ASCII.GetBytes("Name already registered. Please enter your name: ");
                            tClient.GetStream().Write(bytes, 0, bytes.Length);
                            done = false;
                        }
                    }
                }
            } while (!done);

            connections.Add(Thread.CurrentThread.ManagedThreadId, new State(name, tClient));
            Console.WriteLine("\tTotal connections: {0}", connections.Count);
            Broadcast(string.Format("+++ {0} arrived +++", name));

            do {
                string text = Receive(tClient);
                if (text == "/quit") {
                    Broadcast(string.Format("Connection from {0} closed.", name));
                    connections.Remove(Thread.CurrentThread.ManagedThreadId);
                    Console.WriteLine("\tTotal connections: {0}", connections.Count);
                    break;
                }

                if (!tClient.Connected) {
                    break;
                }
                Broadcast(string.Format("{0}> {1}", name, text));
            } while (true);

            Console.WriteLine("Client (Thread: {0}): Terminated!", Thread.CurrentThread.ManagedThreadId);
            tClient.Close();
            Thread.CurrentThread.Abort();
        }

        private static string Receive(TcpClient client) {
            StringBuilder sb = new StringBuilder();
            do {
                if (client.Available > 0) {
                    while (client.Available > 0) {
                        char ch = (char)client.GetStream().ReadByte();
                        if (ch == '\r') {
                            //ignore
                            continue;
                        }
                        if (ch == '\n') {
                            return sb.ToString();
                        }
                        sb.Append(ch);
                    }
                }

                // Pause
                Thread.Sleep(100);
            } while (true);
        }

        private static void Broadcast(string text) {
            Console.WriteLine(text);
            foreach (var oClient in connections) {
                if (oClient.Key != Thread.CurrentThread.ManagedThreadId) {
                    State state = oClient.Value;
                    state.Send(text);
                }
            }
        }
    }
}
```



## CoffeeScript

This is ported from the JavaScript version.  The tool js2coffee got me a mostly working version, and then I manually converted JS-style classes to CS "classic-style class" syntax.


```coffeescript

net = require("net")
sys = require("sys")
EventEmitter = require("events").EventEmitter

isNicknameLegal = (nickname) ->
  return false unless nickname.replace(/[A-Za-z0-9]*/, "") is ""
  for used_nick of @chatters
    return false if used_nick is nickname
  true

class ChatServer
  constructor: ->
    @chatters = {}
    @server = net.createServer @handleConnection
    @server.listen 1212, "localhost"

  handleConnection: (connection) =>
    console.log "Incoming connection from " + connection.remoteAddress
    connection.setEncoding "utf8"
    chatter = new Chatter(connection, this)
    chatter.on "chat", @handleChat
    chatter.on "join", @handleJoin
    chatter.on "leave", @handleLeave

  handleChat: (chatter, message) =>
    @sendToEveryChatterExcept chatter, chatter.nickname + ": " + message

  handleJoin: (chatter) =>
    console.log chatter.nickname + " has joined the chat."
    @sendToEveryChatter chatter.nickname + " has joined the chat."
    @addChatter chatter

  handleLeave: (chatter) =>
    console.log chatter.nickname + " has left the chat."
    @removeChatter chatter
    @sendToEveryChatter chatter.nickname + " has left the chat."

  addChatter: (chatter) =>
    @chatters[chatter.nickname] = chatter

  removeChatter: (chatter) =>
    delete @chatters[chatter.nickname]

  sendToEveryChatter: (data) =>
    for nickname of @chatters
      @chatters[nickname].send data

  sendToEveryChatterExcept: (chatter, data) =>
    for nickname of @chatters
      @chatters[nickname].send data  unless nickname is chatter.nickname


class Chatter extends EventEmitter
  constructor: (socket, server) ->
    EventEmitter.call this
    @socket = socket
    @server = server
    @nickname = ""
    @lineBuffer = new SocketLineBuffer(socket)
    @lineBuffer.on "line", @handleNickname
    @socket.on "close", @handleDisconnect
    @send "Welcome! What is your nickname?"

  handleNickname: (nickname) =>
    if isNicknameLegal(nickname)
      @nickname = nickname
      @lineBuffer.removeAllListeners "line"
      @lineBuffer.on "line", @handleChat
      @send "Welcome to the chat, " + nickname + "!"
      @emit "join", this
    else
      @send "Sorry, but that nickname is not legal or is already in use!"
      @send "What is your nickname?"

  handleChat: (line) =>
    @emit "chat", this, line

  handleDisconnect: =>
    @emit "leave", this

  send: (data) =>
    @socket.write data + "\r\n"


class SocketLineBuffer extends EventEmitter
  constructor: (socket) ->
    EventEmitter.call this
    @socket = socket
    @buffer = ""
    @socket.on "data", @handleData

  handleData: (data) =>
    console.log "Handling data", data
    i = 0

    while i < data.length
      char = data.charAt(i)
      @buffer += char
      if char is "\n"
        @buffer = @buffer.replace("\r\n", "")
        @buffer = @buffer.replace("\n", "")
        @emit "line", @buffer
        console.log "incoming line: #{@buffer}"
        @buffer = ""
      i++

server = new ChatServer()

```



## D


```d

import std.getopt;
import std.socket;
import std.stdio;
import std.string;

struct client {
    int pos;
    char[] name;
    char[] buffer;
    Socket socket;
}

void broadcast(client[] connections, size_t self, const char[] message) {
    writeln(message);
    for (size_t i = 0; i < connections.length; i++) {
        if (i == self) continue;

        connections[i].socket.send(message);
        connections[i].socket.send("\r\n");
    }
}

bool registerClient(client[] connections, size_t self) {
    for (size_t i = 0; i < connections.length; i++) {
        if (i == self) continue;

        if (icmp(connections[i].name, connections[self].name) == 0) {
            return false;
        }
    }

    return true;
}

void main(string[] args) {
    ushort port = 4004;

    auto helpInformation = getopt
    (
        args,
        "port|p", "The port to listen to chat clients on [default is 4004]", &port
    );

    if (helpInformation.helpWanted) {
        defaultGetoptPrinter("A simple chat server based on a task in rosettacode.", helpInformation.options);
        return;
    }

    auto listener = new TcpSocket();
    assert(listener.isAlive);
    listener.blocking = false;
    listener.bind(new InternetAddress(port));
    listener.listen(10);
    writeln("Listening on port: ", port);

    enum MAX_CONNECTIONS = 60;
    auto socketSet = new SocketSet(MAX_CONNECTIONS + 1);
    client[] connections;

    while(true) {
        socketSet.add(listener);

        foreach (con; connections) {
            socketSet.add(con.socket);
        }

        Socket.select(socketSet, null, null);

        for (size_t i = 0; i < connections.length; i++) {
            if (socketSet.isSet(connections[i].socket)) {
                char[1024] buf;
                auto datLength = connections[i].socket.receive(buf[]);

                if (datLength == Socket.ERROR) {
                    writeln("Connection error.");
                } else if (datLength != 0) {
                    if (buf[0] == '\n' || buf[0] == '\r') {
                        if (connections[i].buffer == "/quit") {
                            connections[i].socket.close();
                            if (connections[i].name.length > 0) {
                                writeln("Connection from ", connections[i].name, " closed.");
                            } else {
                                writeln("Connection from ", connections[i].socket.remoteAddress(), " closed.");
                            }

                            connections[i] = connections[$-1];
                            connections.length--;
                            i--;

                            writeln("\tTotal connections: ", connections.length);
                            continue;
                        } else if (connections[i].name.length == 0) {
                            connections[i].buffer = strip(connections[i].buffer);
                            if (connections[i].buffer.length > 0) {
                                connections[i].name = connections[i].buffer;
                                if (registerClient(connections, i)) {
                                    connections.broadcast(i, "+++ " ~ connections[i].name ~ " arrived +++");
                                } else {
                                    connections[i].socket.send("Name already registered. Please enter your name: ");
                                    connections[i].name.length = 0;
                                }
                            } else {
                                connections[i].socket.send("A name is required. Please enter your name: ");
                            }
                        } else {
                            connections.broadcast(i, connections[i].name ~ "> " ~ connections[i].buffer);
                        }
                        connections[i].buffer.length = 0;
                    } else {
                        connections[i].buffer ~= buf[0..datLength];
                    }
                } else {
                    try {
                        if (connections[i].name.length > 0) {
                            writeln("Connection from ", connections[i].name, " closed.");
                        } else {
                            writeln("Connection from ", connections[i].socket.remoteAddress(), " closed.");
                        }
                    } catch (SocketException) {
                        writeln("Connection closed.");
                    }
                }
            }
        }

        if (socketSet.isSet(listener)) {
            Socket sn = null;
            scope(failure) {
                writeln("Error accepting");

                if (sn) {
                    sn.close();
                }
            }
            sn = listener.accept();
            assert(sn.isAlive);
            assert(listener.isAlive);

            if (connections.length < MAX_CONNECTIONS) {
                client newclient;

                writeln("Connection from ", sn.remoteAddress(), " established.");
                sn.send("Enter name: ");

                newclient.socket = sn;
                connections ~= newclient;

                writeln("\tTotal connections: ", connections.length);
            } else {
                writeln("Rejected connection from ", sn.remoteAddress(), "; too many connections.");
                sn.close();
                assert(!sn.isAlive);
                assert(listener.isAlive);
            }
        }

        socketSet.reset();
    }
}

```



## Erlang


```erlang

-module(chat).

-export([start/0, start/1]).

-record(client, {name=none, socket=none}).

start() -> start(8080).
start(Port) ->
    register(server, spawn(fun() -> server() end)),
    {ok, LSocket} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
    accept(LSocket).

% main loop for message dispatcher
server() -> server([]).
server(Clients) ->
    receive
        {join, Client=#client{name = Name, socket = Socket}} ->
            self() ! {say, Socket, "has joined." ++ [10, 13]},
            server(Clients ++ [Client]);
        {leave, Socket} ->
            {value, #client{name = Name}, List} = lists:keytake(Socket, 3, Clients),
            self() ! {say, none, Message = "has left."},
            server(List);
        {say, Socket, Data} ->
            {value, #client{name = From}, List} = lists:keytake(Socket, 3, Clients),
            Message = From ++ " : " ++ Data,
            lists:map(fun(#client{socket = S}) ->
                    gen_tcp:send(S, Message)
                end, List)
    end,
    server(Clients).

% accepts connections then spawns the client handler
accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> connecting(Socket) end),
    accept(LSocket).

% when client is first connect send prompt for user name
connecting(Socket) ->
    gen_tcp:send(Socket, "What is your name? "),
    case listen(Socket) of
        {ok, N} ->
            Name = binary_to_list(N),
            server ! {join, #client{name =  lists:sublist(Name, 1, length(Name) - 2), socket = Socket} },
            client(Socket);
        _ -> ok
    end.

% main client loop that listens for data
client(Socket) ->
    case listen(Socket) of
        {ok, Data} ->
            server ! {say, Socket, binary_to_list(Data)},
            client(Socket);
        _ -> server ! {leave, Socket}
    end.

% utility function that listens for data on a socket
listen(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        Response -> Response
    end.

```



## Go

This example uses the Go idiom of [http://blog.golang.org/share-memory-by-communicating ''Do not communicate by sharing memory; instead, share memory by communicating'']; there are no explicit locks used, instead Go channels are used to safely synchronize where required.

A similar exercise of a chat roulette (different in that messages only have to be written to a single partner rather than broadcast, this simplifies the code greatly) was the topic of a [http://talks.golang.org/2012/chat.slide#1 2012 Go Talk].

This example handles the case of one specific client "falling behind" by relying on the underlying TCP stack to do a reasonable job of buffering. Once that buffer fills, a write to the that client's connection will time out and the connection will dropped. Other minor improvements would include enabling TCP keep alives, handling temporary errors from accept, and better logging. Not ideal, but it should be good enough for this example.


```go
package main

import (
	"bufio"
	"flag"
	"fmt"
	"log"
	"net"
	"strings"
	"time"
)

func main() {
	log.SetPrefix("chat: ")
	addr := flag.String("addr", "localhost:4000", "listen address")
	flag.Parse()
	log.Fatal(ListenAndServe(*addr))
}

// A Server represents a chat server that accepts incoming connections.
type Server struct {
	add  chan *conn  // To add a connection
	rem  chan string // To remove a connection by name
	msg  chan string // To send a message to all connections
	stop chan bool   // To stop early
}

// ListenAndServe listens on the TCP network address addr for
// new chat client connections.
func ListenAndServe(addr string) error {
	ln, err := net.Listen("tcp", addr)
	if err != nil {
		return err
	}
	log.Println("Listening for connections on", addr)
	defer ln.Close()
	s := &Server{
		add:  make(chan *conn),
		rem:  make(chan string),
		msg:  make(chan string),
		stop: make(chan bool),
	}
	go s.handleConns()
	for {
		// TODO use AcceptTCP() so that we can get a TCPConn on which
		// we can call SetKeepAlive() and SetKeepAlivePeriod()
		rwc, err := ln.Accept()
		if err != nil {
			// TODO Could handle err.(net.Error).Temporary()
			// here by adding a backoff delay.
			close(s.stop)
			return err
		}
		log.Println("New connection from", rwc.RemoteAddr())
		go newConn(s, rwc).welcome()
	}
}

// handleConns is run as a go routine to handle adding and removal of
// chat client connections as well as broadcasting messages to them.
func (s *Server) handleConns() {
	// We define the `conns` map here rather than within Server,
	// and we use local function literals rather than methods to be
	// extra sure that the only place that touches this map is this
	// method. In this way we forgo any explicit locking needed as
	// we're the only go routine that can see or modify this.
	conns := make(map[string]*conn)

	var dropConn func(string)
	writeAll := func(str string) {
		log.Printf("Broadcast: %q", str)
		// TODO handle blocked connections
		for name, c := range conns {
			c.SetWriteDeadline(time.Now().Add(500 * time.Millisecond))
			_, err := c.Write([]byte(str))
			if err != nil {
				log.Printf("Error writing to %q: %v", name, err)
				c.Close()
				delete(conns, name)
				// Defer all the disconnect messages until after
				// we've closed all currently problematic conns.
				defer dropConn(name)
			}
		}
	}

	dropConn = func(name string) {
		if c, ok := conns[name]; ok {
			log.Printf("Closing connection with %q from %v",
				name, c.RemoteAddr())
			c.Close()
			delete(conns, name)
		} else {
			log.Printf("Dropped connection with %q", name)
		}
		str := fmt.Sprintf("--- %q disconnected ---\n", name)
		writeAll(str)
	}

	defer func() {
		writeAll("Server stopping!\n")
		for _, c := range conns {
			c.Close()
		}
	}()

	for {
		select {
		case c := <-s.add:
			if _, exists := conns[c.name]; exists {
				fmt.Fprintf(c, "Name %q is not available\n", c.name)
				go c.welcome()
				continue
			}
			str := fmt.Sprintf("+++ %q connected +++\n", c.name)
			writeAll(str)
			conns[c.name] = c
			go c.readloop()
		case str := <-s.msg:
			writeAll(str)
		case name := <-s.rem:
			dropConn(name)
		case <-s.stop:
			return
		}
	}
}

// A conn represents the server side of a single chat connection.
// Note we embed the bufio.Reader and net.Conn (and specifically in
// that order) so that a conn gets the appropriate methods from each
// to be a full io.ReadWriteCloser.
type conn struct {
	*bufio.Reader         // buffered input
	net.Conn              // raw connection
	server        *Server // the Server on which the connection arrived
	name          string
}

func newConn(s *Server, rwc net.Conn) *conn {
	return &conn{
		Reader: bufio.NewReader(rwc),
		Conn:   rwc,
		server: s,
	}
}

// welcome requests a name from the client before attempting to add the
// named connect to the set handled by the server.
func (c *conn) welcome() {
	var err error
	for c.name = ""; c.name == ""; {
		fmt.Fprint(c, "Enter your name: ")
		c.name, err = c.ReadString('\n')
		if err != nil {
			log.Printf("Reading name from %v: %v", c.RemoteAddr(), err)
			c.Close()
			return
		}
		c.name = strings.TrimSpace(c.name)
	}
	// The server will take this *conn and do a final check
	// on the name, possibly starting c.welcome() again.
	c.server.add <- c
}

// readloop is started as a go routine by the server once the initial
// welcome phase has completed successfully. It reads single lines from
// the client and passes them to the server for broadcast to all chat
// clients (including us).
// Once done, we ask the server to remove (and close) our connection.
func (c *conn) readloop() {
	for {
		msg, err := c.ReadString('\n')
		if err != nil {
			break
		}
		//msg = strings.TrimSpace(msg)
		c.server.msg <- c.name + "> " + msg
	}
	c.server.rem <- c.name
}
```



## Groovy

{{trans|Java}}

```groovy
class ChatServer implements Runnable {
    private int port = 0
    private List<Client> clientList = new ArrayList<>()

    ChatServer(int port) {
        this.port = port
    }

    @SuppressWarnings("GroovyInfiniteLoopStatement")
    @Override
    void run() {
        try {
            ServerSocket serverSocket = new ServerSocket(port)
            while (true) {
                Socket socket = serverSocket.accept()
                new Thread(new Client(socket)).start()
            }
        } catch (Exception e) {
            e.printStackTrace()
        }
    }

    private synchronized boolean registerClient(Client client) {
        for (Client other : clientList) {
            if (other.clientName.equalsIgnoreCase(client.clientName)) {
                return false
            }
        }
        clientList.add(client)
        return true
    }

    private void deRegisterClient(Client client) {
        boolean wasRegistered
        synchronized (this) {
            wasRegistered = clientList.remove(client)
        }
        if (wasRegistered) {
            broadcast(client, "--- " + client.clientName + " left ---")
        }
    }

    private synchronized String getOnlineListCSV() {
        StringBuilder sb = new StringBuilder()
        sb.append(clientList.size()).append(" user(s) online: ")
        def it = clientList.iterator()
        if (it.hasNext()) {
            sb.append(it.next().clientName)
        }
        while (it.hasNext()) {
            sb.append(", ")
            sb.append(it.next().clientName)
        }
        return sb.toString()
    }

    private void broadcast(Client fromClient, String msg) {
        // Copy client list (don't want to hold lock while doing IO)
        List<Client> clients
        synchronized (this) {
            clients = new ArrayList<>(this.clientList)
        }
        for (Client client : clients) {
            if (client == fromClient) {
                continue
            }
            try {
                client.write(msg + "\r\n")
            } catch (Exception ignored) {
                // empty
            }
        }
    }

    class Client implements Runnable {
        private Socket socket = null
        private Writer output = null
        private String clientName = null

        Client(Socket socket) {
            this.socket = socket
        }

        @Override
        void run() {
            try {
                socket.setSendBufferSize(16384)
                socket.setTcpNoDelay(true)
                BufferedReader input = new BufferedReader(new InputStreamReader(socket.getInputStream()))
                output = new OutputStreamWriter(socket.getOutputStream())
                write("Please enter your name: ")
                String line
                while (null != (line = input.readLine())) {
                    if (null == clientName) {
                        line = line.trim()
                        if (line.isEmpty()) {
                            write("A name is required. Please enter your name: ")
                            continue
                        }
                        clientName = line
                        if (!registerClient(this)) {
                            clientName = null
                            write("Name already registered. Please enter your name: ")
                            continue
                        }
                        write(getOnlineListCSV() + "\r\n")
                        broadcast(this, "+++ " + clientName + " arrived +++")
                        continue
                    }
                    if (line.equalsIgnoreCase("/quit")) {
                        return
                    }
                    broadcast(this, clientName + "> " + line)
                }
            } catch (Exception ignored) {
                // empty
            } finally {
                deRegisterClient(this)
                output = null
                try {
                    socket.close()
                } catch (Exception ignored) {
                    // empty
                }
                socket = null
            }
        }

        void write(String msg) {
            output.write(msg)
            output.flush()
        }

        @Override
        boolean equals(client) {
            return (null != client) && (client instanceof Client) && (null != clientName) && clientName == client.clientName
        }

        @Override
        int hashCode() {
            int result
            result = (socket != null ? socket.hashCode() : 0)
            result = 31 * result + (output != null ? output.hashCode() : 0)
            result = 31 * result + (clientName != null ? clientName.hashCode() : 0)
            return result
        }
    }

    static void main(String[] args) {
        int port = 4004
        if (args.length > 0) {
            port = Integer.parseInt(args[0])
        }
        new ChatServer(port).run()
    }
}
```



## Haskell


```haskell
{-# LANGUAGE OverloadedStrings #-}
import Network
import System.IO
import Control.Concurrent
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad.Reader
import Control.Monad.Error
import Control.Exception
import Data.Monoid
import Control.Applicative

type ServerApp = ReaderT ThreadData IO
data Speaker = Server | Client Text
data ThreadData = ThreadData { threadHandle :: Handle
                             , userTableMV :: MVar (Map Text Handle)}

echoLocal = liftIO . T.putStrLn
echoRemote = echoMessage . (">> "<>)
echoMessage msg = viewHandle >>= \h -> liftIO $ T.hPutStrLn h msg
getRemoteLine = viewHandle >>= liftIO . T.hGetLine
putMVarT = (liftIO.) . putMVar
takeMVarT = liftIO . takeMVar
readMVarT = liftIO . readMVar
modifyUserTable fn = viewUsers >>= \mv ->
                     liftIO $ modifyMVar_ mv (return . fn)
viewHandle = threadHandle <$> ask
viewUsers = userTableMV <$> ask

userChat :: ServerApp ()
userChat = do
    name <- addUser
    echoLocal name
    h <- viewHandle
    (flip catchError) (\_ -> removeUser name) $
      do echoLocal $ "Accepted " <> name
         forever $ getRemoteLine >>= broadcast (Client name)

removeUser :: Text -> ServerApp ()
removeUser name = do
    echoLocal $ "Exception with " <> name <> ", removing from userTable"
    broadcast Server $ name <> " has left the server"
    modifyUserTable (M.delete name)

addUser :: ServerApp Text
addUser = do
    h <- viewHandle
    usersMV <- viewUsers
    echoRemote "Enter username"
    name <- T.filter (/='\r') <$> getRemoteLine
    userTable <- takeMVarT usersMV
    if name `M.member` userTable
      then do echoRemote "Username already exists!"
              putMVarT usersMV userTable
              addUser
      else do putMVarT usersMV (M.insert name h userTable)
              broadcast Server $ name <> " has joined the server"
              echoRemote "Welcome to the server!\n>> Other users:"
              readMVarT usersMV >>=
                  mapM_ (echoRemote . ("*" <>) . fst)
                . filter ((/=name). fst) . M.toList
              return name

broadcast :: Speaker -> Text -> ServerApp ()
broadcast user msg =
    viewUsers >>= readMVarT >>= mapM_ (f . snd) . fn . M.toList
  where f h = liftIO $ T.hPutStrLn h $ nm <> msg
        (fn, nm) = case user of
                    Server -> (id, ">> ")
                    Client t -> (filter ((/=t) . fst), t <> "> ")

clientLoop socket users = do
    (h, _, _) <- accept socket
    hSetBuffering h LineBuffering
    forkIO $ runReaderT userChat (ThreadData h users)
    clientLoop socket users

main = do
    server <- listenOn $ PortNumber 5002
    T.putStrLn "Server started"
    newMVar (M.empty) >>= clientLoop server

```


==Icon and {{header|Unicon}}==

This is Unicon-specific:

```unicon
global mlck, nCons, cons

procedure main()
    mlck := mutex()
    nCons := 0
    cons := mutex(set())
    while f := open(":12321","na") do {
        handle_client(f)
        critical mlck: if nCons <= 0 then close(f)
        }
end

procedure handle_client(f)
    critical mlck: nCons +:= 1
    thread {
        select(f,1000) & {
            writes(f, "Name? ")
            nick := (read(f) ? tab(upto('\n\r')))
            every write(!cons, nick," has joined.")
            insert(cons, f)
            while s := read(f) do every write(!cons, nick,": ",s)
            }
        delete(cons, f)
        every write(!cons, nick," has left.")
        critical mlck: nCons -:= 1
        }
end
```



## Java


Broadcasting of messages is done by the thread that received the message, so a bad client could potentially disrupt the server. The output buffer is set to 16K in an attempt to alleviate possible symptoms, but I'm not sure if it's effective. Server does not allow duplicate client names, and lists all users online after a successful connection. Client can type "/quit" to quit.

I think ideally, NIO would be used to select() sockets available/ready for I/O, to eliminate the possibility of a bad connection disrupting the server, but this increases the complexity.


```java
import java.io.*;
import java.net.*;
import java.util.*;

public class ChatServer implements Runnable
{
  private int port = 0;
  private List<Client> clients = new ArrayList<Client>();

  public ChatServer(int port)
  {  this.port = port;  }

  public void run()
  {
    try
    {
      ServerSocket ss = new ServerSocket(port);
      while (true)
      {
        Socket s = ss.accept();
        new Thread(new Client(s)).start();
      }
    }
    catch (Exception e)
    {  e.printStackTrace();  }
  }

  private synchronized boolean registerClient(Client client)
  {
    for (Client otherClient : clients)
      if (otherClient.clientName.equalsIgnoreCase(client.clientName))
        return false;
    clients.add(client);
    return true;
  }

  private void deregisterClient(Client client)
  {
    boolean wasRegistered = false;
    synchronized (this)
    {  wasRegistered = clients.remove(client);  }
    if (wasRegistered)
      broadcast(client, "--- " + client.clientName + " left ---");
  }

  private synchronized String getOnlineListCSV()
  {
    StringBuilder sb = new StringBuilder();
    sb.append(clients.size()).append(" user(s) online: ");
    for (int i = 0; i < clients.size(); i++)
      sb.append((i > 0) ? ", " : "").append(clients.get(i).clientName);
    return sb.toString();
  }

  private void broadcast(Client fromClient, String msg)
  {
    // Copy client list (don't want to hold lock while doing IO)
    List<Client> clients = null;
    synchronized (this)
    {  clients = new ArrayList<Client>(this.clients);  }
    for (Client client : clients)
    {
      if (client.equals(fromClient))
        continue;
      try
      {  client.write(msg + "\r\n");  }
      catch (Exception e)
      {  }
    }
  }

  public class Client implements Runnable
  {
    private Socket socket = null;
    private Writer output = null;
    private String clientName = null;

    public Client(Socket socket)
    {
      this.socket = socket;
    }

    public void run()
    {
      try
      {
        socket.setSendBufferSize(16384);
        socket.setTcpNoDelay(true);
        BufferedReader input = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        output = new OutputStreamWriter(socket.getOutputStream());
        write("Please enter your name: ");
        String line = null;
        while ((line = input.readLine()) != null)
        {
          if (clientName == null)
          {
            line = line.trim();
            if (line.isEmpty())
            {
              write("A name is required. Please enter your name: ");
              continue;
            }
            clientName = line;
            if (!registerClient(this))
            {
              clientName = null;
              write("Name already registered. Please enter your name: ");
              continue;
            }
            write(getOnlineListCSV() + "\r\n");
            broadcast(this, "+++ " + clientName + " arrived +++");
            continue;
          }
          if (line.equalsIgnoreCase("/quit"))
            return;
          broadcast(this, clientName + "> " + line);
        }
      }
      catch (Exception e)
      {  }
      finally
      {
        deregisterClient(this);
        output = null;
        try
        {  socket.close();  }
        catch (Exception e)
        {  }
        socket = null;
      }
    }

    public void write(String msg) throws IOException
    {
      output.write(msg);
      output.flush();
    }

    public boolean equals(Client client)
    {
      return (client != null) && (client instanceof Client) && (clientName != null) && (client.clientName != null) && clientName.equals(client.clientName);
    }
  }

  public static void main(String[] args)
  {
    int port = 4004;
    if (args.length > 0)
      port = Integer.parseInt(args[0]);
    new ChatServer(port).run();
  }
}

```



## JavaScript

{{works with|Node.js}}

```javascript
var net = require("net");
var sys = require("sys");
var EventEmitter = require("events").EventEmitter;

/*******************************************************************************
 * ChatServer
 *
 * Manages connections, users, and chat messages.
 ******************************************************************************/

function ChatServer() {
  this.chatters = {};
  this.server   = net.createServer(this.handleConnection.bind(this));
  this.server.listen(1212, "localhost");
}

ChatServer.prototype.isNicknameLegal = function(nickname) {
  // A nickname may contain letters or numbers only,
  // and may only be used once.
  if(nickname.replace(/[A-Za-z0-9]*/, '') != "") {
    return false
  }
  for(used_nick in this.chatters) {
    if(used_nick == nickname) {
      return false;
    }
  }
  return true;
};

ChatServer.prototype.handleConnection = function(connection) {
  console.log("Incoming connection from " + connection.remoteAddress);
  connection.setEncoding("utf8");

  var chatter = new Chatter(connection, this);
  chatter.on("chat", this.handleChat.bind(this));
  chatter.on("join", this.handleJoin.bind(this));
  chatter.on("leave", this.handleLeave.bind(this));
};

ChatServer.prototype.handleChat = function(chatter, message) {
  this.sendToEveryChatterExcept(chatter, chatter.nickname + ": " + message);
};

ChatServer.prototype.handleJoin = function(chatter) {
  console.log(chatter.nickname + " has joined the chat.");
  this.sendToEveryChatter(chatter.nickname + " has joined the chat.");
  this.addChatter(chatter);
};

ChatServer.prototype.handleLeave = function(chatter) {
  console.log(chatter.nickname + " has left the chat.");
  this.removeChatter(chatter);
  this.sendToEveryChatter(chatter.nickname + " has left the chat.");
};

ChatServer.prototype.addChatter = function(chatter) {
  this.chatters[chatter.nickname] = chatter;
};

ChatServer.prototype.removeChatter = function(chatter) {
  delete this.chatters[chatter.nickname];
};

ChatServer.prototype.sendToEveryChatter = function(data) {
  for(nickname in this.chatters) {
    this.chatters[nickname].send(data);
  }
};

ChatServer.prototype.sendToEveryChatterExcept = function(chatter, data) {
  for(nickname in this.chatters) {
    if(nickname != chatter.nickname) {
      this.chatters[nickname].send(data);
    }
  }
};

/*******************************************************************************
 * Chatter
 *
 * Represents a single user/connection in the chat server.
 ******************************************************************************/

function Chatter(socket, server) {
  EventEmitter.call(this);

  this.socket     = socket;
  this.server     = server;
  this.nickname   = "";
  this.lineBuffer = new SocketLineBuffer(socket);

  this.lineBuffer.on("line", this.handleNickname.bind(this));
  this.socket.on("close", this.handleDisconnect.bind(this));

  this.send("Welcome! What is your nickname?");
};

sys.inherits(Chatter, EventEmitter);

Chatter.prototype.handleNickname = function(nickname) {
  if(server.isNicknameLegal(nickname)) {
    this.nickname = nickname;
    this.lineBuffer.removeAllListeners("line");
    this.lineBuffer.on("line", this.handleChat.bind(this));
    this.send("Welcome to the chat, " + nickname + "!");
    this.emit("join", this);
  } else {
    this.send("Sorry, but that nickname is not legal or is already in use!");
    this.send("What is your nickname?");
  }
};

Chatter.prototype.handleChat = function(line) {
  this.emit("chat", this, line);
};

Chatter.prototype.handleDisconnect = function() {
  this.emit("leave", this);
};

Chatter.prototype.send = function(data) {
  this.socket.write(data + "\r\n");
};

/*******************************************************************************
 * SocketLineBuffer
 *
 * Listens for and buffers incoming data on a socket and emits a 'line' event
 * whenever a complete line is detected.
 ******************************************************************************/

function SocketLineBuffer(socket) {
  EventEmitter.call(this);

  this.socket = socket;
  this.buffer = "";

  this.socket.on("data", this.handleData.bind(this));
};

sys.inherits(SocketLineBuffer, EventEmitter);

SocketLineBuffer.prototype.handleData = function(data) {
  for(var i = 0; i < data.length; i++) {
    var char = data.charAt(i);
    this.buffer += char;
    if(char == "\n") {
      this.buffer = this.buffer.replace("\r\n", "");
      this.buffer = this.buffer.replace("\n", "");
      this.emit("line", this.buffer);
      this.buffer = "";
    }
  }
};

// Start the server!
server = new ChatServer();
```



## Julia

Modified to fit the Rosetta Code task from example code for the WebSockets module written by Leah Hanson.
To test, start the code and use a browser to connect to localhost:8000.

```julia

using HttpServer
using WebSockets

const connections = Dict{Int,WebSocket}()
const usernames   = Dict{Int,String}()

function decodeMessage( msg )
    String(copy(msg))
end


wsh = WebSocketHandler() do req, client
    global connections
    @show connections[client.id] = client
    println("req is $req")
    notifyonline = "Connection from user number $(client.id) is now online."
    for (k,v) in connections
        if k != client.id
            try
                write(v, notifyonline)
            catch
                continue
            end
        end
    end
    while true
        try
            msg = read(client)
        catch
            telloffline = "User $(usernames[client.id]) disconnected."
            println(telloffline, "(The client id was $(client.id).)")
            delete!(connections, client.id)
            if haskey(usernames, client.id)
                delete!(usernames, client.id)
            end
            for (k,v) in connections
                try
                    write(v, telloffline)
                catch
                    continue
                end
            end
            return
        end
        msg = decodeMessage(msg)
        if startswith(msg, "setusername:")
            println("SETTING USERNAME: $msg")
            usernames[client.id] = msg[13:end]
            notifyusername = "User number $(client.id) chose $(usernames[client.id]) as name handle."
            for (k,v) in connections
                try
                    write(v, notifyusername)
                catch
                    println("Caught exception writing to user $k")
                    continue
                end
            end
        end
        if startswith(msg, "say:")
            println("EMITTING MESSAGE: $msg")
            for (k,v) in connections
                if k != client.id
                    try
                        write(v, (usernames[client.id] * ": " * msg[5:end]))
                    catch
                        println("Caught exception writing to user $k")
                        continue
                    end
                end
            end
        end
    end
end

onepage = readstring(Pkg.dir("WebSockets","examples","chat-client.html"))
httph = HttpHandler() do req::Request, res::Response
  Response(onepage)
end

server = Server(httph, wsh)
println("Chat server listening on 8000...")
run(server,8000)

```



## Kotlin

{{trans|Java}}

```scala
import java.io.BufferedReader
import java.io.IOException
import java.io.InputStreamReader
import java.io.OutputStreamWriter
import java.io.Writer
import java.net.ServerSocket
import java.net.Socket
import java.util.ArrayList
import java.util.Collections

class ChatServer private constructor(private val port: Int) : Runnable {
    private val clients = ArrayList<Client>()

    private val onlineListCSV: String
        @Synchronized get() {
            val sb = StringBuilder()
            sb.append(clients.size).append(" user(s) online: ")
            for (i in clients.indices) {
                sb.append(if (i > 0) ", " else "").append(clients[i].clientName)
            }
            return sb.toString()
        }

    override fun run() {
        try {
            val ss = ServerSocket(port)
            while (true) {
                val s = ss.accept()
                Thread(Client(s)).start()
            }
        } catch (e: Exception) {
            e.printStackTrace()
        }
    }

    @Synchronized
    private fun registerClient(client: Client): Boolean {
        for (otherClient in clients) {
            if (otherClient.clientName!!.equals(client.clientName!!, ignoreCase = true)) {
                return false
            }
        }
        clients.add(client)
        return true
    }

    private fun deRegisterClient(client: Client) {
        var wasRegistered = false
        synchronized(this) {
            wasRegistered = clients.remove(client)
        }
        if (wasRegistered) {
            broadcast(client, "--- " + client.clientName + " left ---")
        }
    }

    private fun broadcast(fromClient: Client, msg: String) {
        // Copy client list (don't want to hold lock while doing IO)
        var clients: List<Client> = Collections.emptyList()
        synchronized(this) {
            clients = ArrayList(this.clients)
        }
        for (client in clients) {
            if (client.equals(fromClient)) {
                continue
            }
            try {
                client.write(msg + "\r\n")
            } catch (e: Exception) {
                e.printStackTrace()
            }

        }
    }

    inner class Client internal constructor(private var socket: Socket?) : Runnable {
        private var output: Writer? = null
        var clientName: String? = null

        override fun run() {
            try {
                socket!!.sendBufferSize = 16384
                socket!!.tcpNoDelay = true
                val input = BufferedReader(InputStreamReader(socket!!.getInputStream()))
                output = OutputStreamWriter(socket!!.getOutputStream())
                write("Please enter your name: ")
                var line: String
                while (true) {
                    line = input.readLine()
                    if (null == line) {
                        break
                    }
                    if (clientName == null) {
                        line = line.trim { it <= ' ' }
                        if (line.isEmpty()) {
                            write("A name is required. Please enter your name: ")
                            continue
                        }
                        clientName = line
                        if (!registerClient(this)) {
                            clientName = null
                            write("Name already registered. Please enter your name: ")
                            continue
                        }
                        write(onlineListCSV + "\r\n")
                        broadcast(this, "+++ $clientName arrived +++")
                        continue
                    }
                    if (line.equals("/quit", ignoreCase = true)) {
                        return
                    }
                    broadcast(this, "$clientName> $line")
                }
            } catch (e: Exception) {
                e.printStackTrace()
            } finally {
                deRegisterClient(this)
                output = null
                try {
                    socket!!.close()
                } catch (e: Exception) {
                    e.printStackTrace()
                }

                socket = null
            }
        }

        @Throws(IOException::class)
        internal fun write(msg: String) {
            output!!.write(msg)
            output!!.flush()
        }

        internal fun equals(client: Client?): Boolean {
            return (client != null
                && clientName != null
                && client.clientName != null
                && clientName == client.clientName)
        }
    }

    companion object {
        @JvmStatic
        fun main(args: Array<String>) {
            var port = 4004
            if (args.isNotEmpty()) {
                port = Integer.parseInt(args[0])
            }
            ChatServer(port).run()
        }
    }
}
```



## Nim


```nim
import asyncnet, asyncdispatch

type
  Client = tuple
    socket: AsyncSocket
    name: string
    connected: bool

var clients {.threadvar.}: seq[Client]

proc sendOthers(client: Client, line: string) {.async.} =
  for c in clients:
    if c != client and c.connected:
      await c.socket.send(line & "\c\L")

proc processClient(socket: AsyncSocket) {.async.} =
  await socket.send("Please enter your name: ")
  var client: Client = (socket, await socket.recvLine(), true)

  clients.add(client)
  asyncCheck client.sendOthers("+++ " & client.name & " arrived +++")

  while true:
    let line = await client.socket.recvLine()
    if line == "":
      asyncCheck client.sendOthers("--- " & client.name & " leaves ---")
      client.connected = false
      return
    asyncCheck client.sendOthers(client.name & "> " & line)

proc serve() {.async.} =
  clients = @[]
  var server = newAsyncSocket()
  server.bindAddr(Port(4004))
  server.listen()

  while true:
    let socket = await server.accept()
    asyncCheck processClient(socket)

asyncCheck serve()
runForever()
```



## Objeck


```objeck

use System.IO.Net;
use System.Concurrency;
use Collection;

bundle Default {
  class ChatServer {
    @clients : StringMap;
    @clients_mutex : ThreadMutex;

    New() {
      @clients := StringMap->New();
      @clients_mutex := ThreadMutex->New("clients_mutex");
    }

    method : ValidLogin(login_name : String, clients : StringMap) ~ Bool {
      if(clients->Has(login_name)) {
        return false;
      };

      return true;
    }

    function : Main(args : String[]) ~ Nil {
      chat_server := ChatServer->New();
      chat_server->Run();
    }

    method : public : Broadcast(message : String, sender : Client) ~ Nil {
      client_array : Vector;
      critical(@clients_mutex) {
        client_array := @clients->GetValues();
      };
      each(i : client_array) {
        client := client_array->Get(i)->As(Client);
        if(client <> sender) {
          client->Send(message);
        };
      };
    }

    method : public : Disconnect(sender : Client) ~ Nil {
      send_name := sender->GetName();
      Broadcast("+++ {$send_name} has left +++", sender);
      critical(@clients_mutex) {
        @clients->Remove(sender->GetName());
      };
      sender->Close();
    }

    method : public : Run() ~ Nil {
      server := TCPSocketServer->New(4661);
      if(server->Listen(5)) {
        while(true) {
          client_sock := server->Accept();
          critical(@clients_mutex) {
            client_sock->WriteString("login: ");
            login_name := client_sock->ReadString();
            if(ValidLogin(login_name, @clients)) {
              client := Client->New(login_name, client_sock, @self);
              @clients->Insert(client->GetName(), client);
              client->Execute(Nil);
            }
            else {
              client_sock->WriteString("+++ login in use +++\r\n");
              client_sock->Close();
            };
          };
        };
      };
      server->Close();
    }
  }

  class Client from Thread {
    @client_sock : TCPSocket;
    @server : ChatServer;

    New(login_name : String, client_sock : TCPSocket, server : ChatServer) {
      Parent(login_name);
      @client_sock := client_sock;
      @server := server;
    }

    method : public : Close() ~ Nil {
      @client_sock->Close();
    }

    method : public : Send(message : String) ~ Nil {
      if(@client_sock->IsOpen() & message->Size() > 0) {
        @client_sock->WriteString("{$message}\r\n");
      }
      else {
        @server->Disconnect(@self);
      };
    }

    method : public : Run(param : Base) ~ Nil {
      client_name := GetName();
      @server->Broadcast("+++ {$client_name} has arrived +++", @self);

      message := @client_sock->ReadString();
      while(message->Size() > 0 & message->Equals("/quit") = false) {
        @server->Broadcast("{$client_name}> {$message}", @self);
        message := @client_sock->ReadString();
      };
      @server->Disconnect(@self);
    }
  }
}

```



## Perl

{{trans|Python}}

```perl
use 5.010;
use strict;
use warnings;

use threads;
use threads::shared;

use IO::Socket::INET;
use Time::HiRes qw(sleep ualarm);

my $HOST = "localhost";
my $PORT = 4004;

my @open;
my %users : shared;

sub broadcast {
    my ($id, $message) = @_;
    print "$message\n";
    foreach my $i (keys %users) {
        if ($i != $id) {
            $open[$i]->send("$message\n");
        }
    }
}

sub sign_in {
    my ($conn) = @_;

    state $id = 0;

    threads->new(
        sub {
            while (1) {
                $conn->send("Please enter your name: ");
                $conn->recv(my $name, 1024, 0);

                if (defined $name) {
                    $name = unpack('A*', $name);

                    if (exists $users{$name}) {
                        $conn->send("Name entered is already in use.\n");
                    }
                    elsif ($name ne '') {
                        $users{$id} = $name;
                        broadcast($id, "+++ $name arrived +++");
                        last;
                    }
                }
            }
        }
    );

    ++$id;
    push @open, $conn;
}

my $server = IO::Socket::INET->new(
                                   Timeout   => 0,
                                   LocalPort => $PORT,
                                   Proto     => "tcp",
                                   LocalAddr => $HOST,
                                   Blocking  => 0,
                                   Listen    => 1,
                                   Reuse     => 1,
                                  );

local $| = 1;
print "Listening on $HOST:$PORT\n";

while (1) {
    my ($conn) = $server->accept;

    if (defined($conn)) {
        sign_in($conn);
    }

    foreach my $i (keys %users) {

        my $conn = $open[$i];
        my $message;

        eval {
            local $SIG{ALRM} = sub { die "alarm\n" };
            ualarm(500);
            $conn->recv($message, 1024, 0);
            ualarm(0);
        };

        if ($@ eq "alarm\n") {
            next;
        }

        if (defined($message)) {
            if ($message ne '') {
                $message = unpack('A*', $message);
                broadcast($i, "$users{$i}> $message");
            }
            else {
                broadcast($i, "--- $users{$i} leaves ---");
                delete $users{$i};
                undef $open[$i];
            }
        }
    }

    sleep(0.1);
}
```



## Perl 6


<div style="display:inline-block">{{trans|Python}}</div> (or at least started out that way)
{{works with|Rakudo|2016.07}}

```perl6
#!/usr/bin/env perl6

react {
    my %connections;

    whenever IO::Socket::Async.listen('localhost', 4004) -> $conn {
        my $name;

        $conn.print: "Please enter your name: ";

        whenever $conn.Supply.lines -> $message {
            if !$name {
                if %connections{$message} {
                    $conn.print: "Name already taken, choose another one: ";
                }
                else {
                    $name = $message;
                    %connections{$name} = $conn;
                    broadcast "+++ %s arrived +++", $name;
                }
            }
            else {
                broadcast "%s> %s", $name, $message;
            }
            LAST {
                broadcast "--- %s left ---", $name;
                %connections{$name}:delete;
                $conn.close ;
            }
            QUIT {
                 default {
                     say "oh no, $_";
                }
            }
        }
    }

    sub broadcast ($format, $from, *@message) {
        my $text = sprintf $format, $from, |@message;
        say $text;
        for %connections.kv -> $name, $conn {
            $conn.print: "$text\n" if $name ne $from;
        }
    }
}
```


Notes:
* It operates asynchronously (using <tt>IO::Socket::Async</tt>), so a slow connection to one client won't affect other clients.
* It accepts messages encoded in UTF-8.
* It tokenizes the message streams at newline boundaries (using the <tt>Supply.lines</tt> method), which I think makes the most sense for a chat application.


## PicoLisp


```PicoLisp
#!/usr/bin/picolisp /usr/lib/picolisp/lib.l

(de chat Lst
   (out *Sock
      (mapc prin Lst)
      (prinl) ) )

(setq *Port (port 4004))

(loop
   (setq *Sock (listen *Port))
   (NIL (fork) (close *Port))
   (close *Sock) )

(out *Sock
   (prin "Please enter your name: ")
   (flush) )
(in *Sock (setq *Name (line T)))

(tell 'chat "+++ " *Name " arrived +++")

(task *Sock
   (in @
      (ifn (eof)
         (tell 'chat *Name "> " (line T))
         (tell 'chat "--- " *Name " left ---")
         (bye) ) ) )
(wait)
```

After starting the above script, connect to the chat server from two terminals:

```txt
           Terminal 1            |           Terminal 2
---------------------------------+---------------------------------
$ telnet localhost 4004          |
Trying ::1...                    |
Trying 127.0.0.1...              |
Connected to localhost.          |
Escape character is '^]'.        |
Please enter your name: Ben      |
                                 | $ telnet localhost 4004
                                 | Trying ::1...
                                 | Trying 127.0.0.1...
                                 | Connected to localhost.
                                 | Escape character is '^]'.
                                 | Please enter your name: Tom
+++ Tom arrived +++              |
Hi Tom                           |
                                 | Ben> Hi Tom
                                 | Hi Ben
Tom> Hi Ben                      |
                                 | How are you?
Tom> How are you?                |
Thanks, fine!                    |
                                 | Ben> Thanks, fine!
                                 | See you!
Tom> See you!                    |
                                 | ^]
                                 | telnet> quit
--- Tom left ---                 |
                                 | Connection closed.
                                 | $
```



## Prolog

Works with Swi-Prolog as of Jan 2019.

This version will load the server automatically on port 5000, adapt to your needs.

```prolog
:- initialization chat_server(5000).

chat_server(Port) :-
      tcp_socket(Socket),
      tcp_bind(Socket, Port),
      tcp_listen(Socket, 5),
      tcp_open_socket(Socket, AcceptFd, _),
      dispatch(AcceptFd).

dispatch(AcceptFd) :-
        tcp_accept(AcceptFd, Socket, _),
        thread_create(process_client(Socket, _), _, [detached(true)]),
        dispatch(AcceptFd).

process_client(Socket, _) :-
        setup_call_cleanup(
            tcp_open_socket(Socket, Str),
            handle_connection(Str),
            close(Str)).

% a connection was made, get the username and add the streams so the
% client can be broadcast to.
handle_connection(Str) :-
      send_msg(Str, msg_welcome, []),
      repeat,
      send_msg(Str, msg_username, []),
      read_line_to_string(Str, Name),
      connect_user(Name, Str), !.

% connections are stored here
:- dynamic(connected/2).

connect_user(Name, Str) :-
      connected(Name, _),
      send_msg(Str, msg_username_taken, []),
      fail.
connect_user(Name, Str) :-
      \+ connected(Name, _),
      send_msg(Str, msg_welcome_name, Name),

      % make sure that the connection is removed when the client leaves.
      setup_call_cleanup(
          assert(connected(Name, Str)),
          (
              broadcast(Name, msg_joined, Name),
              chat_loop(Name, Str), !,
              broadcast(Name, msg_left, Name)
          ),
          retractall(connected(Name, _))
      ).

% wait for a line to be sent then broadcast to the rest of the clients
% finish this goal when the client disconnects (end of stream)
chat_loop(Name, Str) :-
      read_line_to_string(Str, S),
      dif(S, end_of_file),
      broadcast(Name, msg_by_user, [Name, S]),
      chat_loop(Name, Str).
chat_loop(_, Str) :- at_end_of_stream(Str).

% send a message to all connected clients except Name (the sender)
broadcast(Name, Msg, Params) :-
    forall(
        (connected(N, Str), dif(N, Name)),
        (send_msg(Str, Msg, Params), send_msg(Str, msg_new_line, []))
    ).

send_msg(St, MsgConst, Params) :-
      call(MsgConst, Msg),
      format(St, Msg, Params),
      flush_output(St).

% constants for the various message types that are sent
msg_welcome('Welcome to Chatalot\n\r').
msg_username('Please enter your nickname: ').
msg_welcome_name('Welcome ~p\n\r').
msg_joined(' -- "~w" has joined the chat --').
msg_left(' -- "~w" has left the chat. --').
msg_username_taken('That username is already taken, choose another\n\r').
msg_new_line('\n\r').
msg_by_user('~w> ~w').
```



## Python


```python
#!/usr/bin/env python

import socket
import thread
import time

HOST = ""
PORT = 4004

def accept(conn):
    """
    Call the inner func in a thread so as not to block. Wait for a
    name to be entered from the given connection. Once a name is
    entered, set the connection to non-blocking and add the user to
    the users dict.
    """
    def threaded():
        while True:
            conn.send("Please enter your name: ")
            try:
                name = conn.recv(1024).strip()
            except socket.error:
                continue
            if name in users:
                conn.send("Name entered is already in use.\n")
            elif name:
                conn.setblocking(False)
                users[name] = conn
                broadcast(name, "+++ %s arrived +++" % name)
                break
    thread.start_new_thread(threaded, ())

def broadcast(name, message):
    """
    Send a message to all users from the given name.
    """
    print message
    for to_name, conn in users.items():
        if to_name != name:
            try:
                conn.send(message + "\n")
            except socket.error:
                pass

# Set up the server socket.
server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
server.setblocking(False)
server.bind((HOST, PORT))
server.listen(1)
print "Listening on %s" % ("%s:%s" % server.getsockname())

# Main event loop.
users = {}
while True:
    try:
        # Accept new connections.
        while True:
            try:
                conn, addr = server.accept()
            except socket.error:
                break
            accept(conn)
        # Read from connections.
        for name, conn in users.items():
            try:
                message = conn.recv(1024)
            except socket.error:
                continue
            if not message:
                # Empty string is given on disconnect.
                del users[name]
                broadcast(name, "--- %s leaves ---" % name)
            else:
                broadcast(name, "%s> %s" % (name, message.strip()))
        time.sleep(.1)
    except (SystemExit, KeyboardInterrupt):
        break
```



## Racket


This is a very basic chat server, but it does everything that is needed for this task.


```racket

#lang racket

(define outs (list (current-output-port)))
(define ((tell-all who o) line)
  (for ([c outs] #:unless (eq? o c)) (displayln (~a who ": " line) c)))

(define ((client i o))
  (define nick (begin (display "Nick: " o) (read-line i)))
  (define tell (tell-all nick o))
  (let loop ([line "(joined)"])
    (if (eof-object? line)
      (begin (tell "(left)") (set! outs (remq o outs)) (close-output-port o))
      (begin (tell line) (loop (read-line i))))))

(define (chat-server listener)
  (define-values [i o] (tcp-accept listener))
  (for ([p (list i o)]) (file-stream-buffer-mode p 'none))
  (thread (client i o)) (set! outs (cons o outs)) (chat-server listener))

(void (thread (λ() (chat-server (tcp-listen 12321)))))
((client (current-input-port) (current-output-port)))

```



## Ruby


```Ruby
require 'gserver'

class ChatServer < GServer
  def initialize *args
    super

    #Keep a list for broadcasting messages
    @chatters = []

    #We'll need this for thread safety
    @mutex = Mutex.new
  end

  #Send message out to everyone but sender
  def broadcast message, sender = nil
    #Need to use \r\n for our Windows friends
    message = message.strip << "\r\n"

    #Mutex for safety - GServer uses threads
    @mutex.synchronize do
      @chatters.each do |chatter|
        begin
          chatter.print message unless chatter == sender
        rescue
          @chatters.delete chatter
        end
      end
    end
  end

  #Handle each connection
  def serve io
    io.print 'Name: '
    name = io.gets

    #They might disconnect
    return if name.nil?

    name.strip!

    broadcast "--+ #{name} has joined +--"

    #Add to our list of connections
    @mutex.synchronize do
      @chatters << io
    end

    #Get and broadcast input until connection returns nil
    loop do
      message = io.gets

      if message
        broadcast "#{name}> #{message}", io
      else
        break
      end
    end

    broadcast "--+ #{name} has left +--"
  end
end

#Start up the server on port 7000
#Accept connections for any IP address
#Allow up to 100 connections
#Send information to stderr
#Turn on informational messages
ChatServer.new(7000, '0.0.0.0', 100, $stderr, true).start.join

```



## Tcl

{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6

# Write a message to everyone except the sender of the message
proc writeEveryoneElse {sender message} {
    dict for {who ch} $::cmap {
	if {$who ne $sender} {
	    puts $ch $message
	}
    }
}

# How to read a line (up to 256 chars long) in a coroutine
proc cgets {ch var} {
    upvar 1 $var v
    while {[gets $ch v] < 0} {
	if {[eof $ch] || [chan pending input $ch] > 256} {
	    return false
	}
	yield
    }
    return true
}

# The chatting, as seen by one user
proc chat {ch addr port} {
    ### CONNECTION CODE ###
    #Log "connection from ${addr}:${port} on channel $ch"
    fconfigure $ch -buffering none -blocking 0 -encoding utf-8
    fileevent $ch readable [info coroutine]
    global cmap
    try {

	### GET THE NICKNAME OF THE USER ###
	puts -nonewline $ch "Please enter your name: "
	if {![cgets $ch name]} {
	    return
	}
	#Log "Mapping ${addr}:${port} to ${name} on channel $ch"
	dict set cmap $name $ch
	writeEveryoneElse $name "+++ $name arrived +++"

	### MAIN CHAT LOOP ###
	while {[cgets $ch line]} {
	    writeEveryoneElse $name "$name> $line"
	}

    } finally {
	### DISCONNECTION CODE ###
	if {[info exists name]} {
	    writeEveryoneElse $name "--- $name left ---"
	    dict unset cmap $name
	}
	close $ch
	#Log "disconnection from ${addr}:${port} on channel $ch"
    }
}

# Service the socket by making corouines running [chat]
socket -server {coroutine c[incr count] chat} 4004
set ::cmap {};		# Dictionary mapping nicks to channels
vwait forever;		# Run event loop
```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Imports System.Net.Sockets
Imports System.Text
Imports System.Threading

Module Module1

    Class State
        Private ReadOnly client As TcpClient
        Private ReadOnly sb As New StringBuilder

        Public Sub New(name As String, client As TcpClient)
            Me.Name = name
            Me.client = client
        End Sub

        Public ReadOnly Property Name As String

        Public Sub Send(text As String)
            Dim bytes = Encoding.ASCII.GetBytes(String.Format("{0}" & vbCrLf, text))
            client.GetStream().Write(bytes, 0, bytes.Length)
        End Sub
    End Class

    ReadOnly connections As New Dictionary(Of Integer, State)
    Dim listen As TcpListener
    Dim serverThread As Thread

    Sub Main()
        listen = New TcpListener(Net.IPAddress.Parse("127.0.0.1"), 4004)
        serverThread = New Thread(New ThreadStart(AddressOf DoListen))
        serverThread.Start()
    End Sub

    Private Sub DoListen()
        listen.Start()
        Console.WriteLine("Server: Started server")

        Do
            Console.Write("Server: Waiting...")
            Dim client = listen.AcceptTcpClient()
            Console.WriteLine(" Connected")

            ' New thread with client
            Dim clientThread As New Thread(New ParameterizedThreadStart(AddressOf DoClient))

            clientThread.Start(client)
        Loop
    End Sub

    Private Sub DoClient(client As TcpClient)
        Console.WriteLine("Client (Thread: {0}): Connected!", Thread.CurrentThread.ManagedThreadId)
        Dim bytes = Encoding.ASCII.GetBytes("Enter name: ")
        client.GetStream().Write(bytes, 0, bytes.Length)

        Dim done As Boolean
        Dim name As String
        Do
            If Not client.Connected Then
                Console.WriteLine("Client (Thread: {0}): Terminated!", Thread.CurrentThread.ManagedThreadId)
                client.Close()
                Thread.CurrentThread.Abort() ' Kill thread
            End If

            name = Receive(client)
            done = True

            For Each cl In connections
                Dim state = cl.Value
                If state.Name = name Then
                    bytes = Encoding.ASCII.GetBytes("Name already registered. Please enter your name: ")
                    client.GetStream().Write(bytes, 0, bytes.Length)
                    done = False
                End If
            Next
        Loop While Not done

        connections.Add(Thread.CurrentThread.ManagedThreadId, New State(name, client))
        Console.WriteLine(vbTab & "Total connections: {0}", connections.Count)
        Broadcast(String.Format("+++ {0} arrived +++", name))

        Do
            Dim text = Receive(client)
            If text = "/quit" Then
                Broadcast(String.Format("Connection from {0} closed.", name))
                connections.Remove(Thread.CurrentThread.ManagedThreadId)
                Console.WriteLine(vbTab & "Total connections: {0}", connections.Count)
                Exit Do
            End If

            If Not client.Connected Then
                Exit Do
            End If

            Broadcast(String.Format("{0}> {1}", name, text))
        Loop

        Console.WriteLine("Client (Thread: {0}): Terminated!", Thread.CurrentThread.ManagedThreadId)
        client.Close()
        Thread.CurrentThread.Abort()
    End Sub

    Private Function Receive(client As TcpClient) As String
        Dim sb As New StringBuilder
        Do
            If client.Available > 0 Then
                While client.Available > 0
                    Dim ch = Chr(client.GetStream.ReadByte())
                    If ch = vbCr Then
                        ' ignore
                        Continue While
                    End If
                    If ch = vbLf Then
                        Return sb.ToString()
                    End If
                    sb.Append(ch)
                End While

                ' pause
                Thread.Sleep(100)
            End If
        Loop
    End Function

    Private Sub Broadcast(text As String)
        Console.WriteLine(text)
        For Each client In connections
            If client.Key <> Thread.CurrentThread.ManagedThreadId Then
                Dim state = client.Value
                state.Send(text)
            End If
        Next
    End Sub

End Module
```



## zkl

{{trans|Python}}
On my Linux box, telnet seems to only want to connect to port 23.

```zkl
const PORT=23;

var users=Dictionary(); // ( handle:socket, ...)
pipe:=Thread.Pipe(); // how server tells thread to connect to user

fcn accept(pipe){ // a thread waiting for the server to send a socket
   while(socket:=pipe.read()){
      println("Somebody is connecting ...");
      socket.read();  // telnet stuff
      while(True){    // get credentials
	 reg name;
	 socket.write("Your handle: ");  // bottle neck
	 try{ name = socket.read().text.strip() } catch(IOError){ continue }
	 if(users.holds(name)) socket.write("Handle is already in use.\n");
	 else if(name){
	    users[name] = socket;
	    chat.launch(name,socket);	// thread
	    broadcast(name, "+++ %s arrived +++".fmt(name));
	    break;	// wait for next connection
	 }
      }//while
   }//while
}.launch(pipe); // thread

fcn chat(name,socket){		// a thread, one per user
   try{
      socket.write("^D to disconnect\n");
      while(True){
	 message:=socket.read().text.strip();
	 if(message=="\xff\xec") break; // ^D to disconnect.
	 broadcast(name, "%s> %s".fmt(name,message));
      }
   }catch{}  // eg socket pukes
   users.del(name); socket.close();
   broadcast(name, "--- %s leaves ---".fmt(name));
}

    // Send a message to all users from the given name.
fcn broadcast(name, message){ // called from user thread
   println(message);  // log message to server console
   users.pump(Void,'wrap([(toName,socket)]){
      if(toName != name) try{ socket.write(message + "\n") } catch(IOError){}
   });
}

   // Set up the server socket.
server:=Network.TCPServerSocket.open(PORT);
println("Listening on %s:%s".fmt(server.hostname,server.port));
server.listen(pipe);  // Main event loop
```

{{out}}
Start the server:

```txt

$ sudo ../../../Bin/zkl chatServer.zkl
Listening on Octavius:23
Somebody is connecting ...
+++ sam arrived +++
--- sam leaves ---
Somebody is connecting ...
+++ sam arrived +++
Somebody is connecting ...
+++ craig arrived +++
craig> this is a test
sam> sam i am
--- sam leaves ---
^\

```

Another terminal:

```txt

$ telnet localhost
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
Your handle: sam
^D to disconnect
+++ craig arrived +++
craig> this is a test
sam i am
^D
Connection closed by foreign host.

```

And yet another terminal:

```txt

$ telnet localhost
...
Your handle: craig
^D to disconnect
this is a test
sam> sam i am
--- sam leaves ---
Connection closed by foreign host.

```


{{omit from|AutoHotkey}}
{{omit from|Lilypond}}
{{omit from|ML/I}}
{{omit from|Mathematica}}
{{omit from|Maxima}}
{{omit from|PARI/GP|No good way to access network}}
{{omit from|Retro}}
{{omit from|SmileBASIC}}
{{omit from|Stata}}
