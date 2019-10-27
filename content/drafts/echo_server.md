+++
title = "Echo server"
description = ""
date = 2019-02-09T04:53:08Z
aliases = []
[extra]
id = 4155
[taxonomies]
categories = []
tags = []
+++

{{task|Networking and Web Interaction}}
Create a network service that sits on TCP port <tt>12321</tt>, which accepts connections on that port, and which echoes complete lines (using a carriage-return/line-feed sequence as line separator) back to clients. No error handling is required. For the purposes of testing, it is only necessary to support connections from localhost (<tt>127.0.0.1</tt> or perhaps <tt>::1</tt>). Logging of connection information to standard output is recommended.

The implementation must be able to handle simultaneous connections from multiple clients. A multi-threaded or multi-process solution may be used. Each connection must be able to echo more than a single line.

The implementation must not stop responding to other clients if one client sends a partial line or stops reading responses.

## Ada

{{works with|GNAT}}

single-threaded, one client served at a time.


```Ada
with Ada.Text_IO;
with Ada.IO_Exceptions;
with GNAT.Sockets;
procedure Echo_Server is
   Receiver   : GNAT.Sockets.Socket_Type;
   Connection : GNAT.Sockets.Socket_Type;
   Client     : GNAT.Sockets.Sock_Addr_Type;
   Channel    : GNAT.Sockets.Stream_Access;
begin
   GNAT.Sockets.Create_Socket (Socket => Receiver);
   GNAT.Sockets.Set_Socket_Option
     (Socket => Receiver,
      Level  => GNAT.Sockets.Socket_Level,
      Option => (Name    => GNAT.Sockets.Reuse_Address, Enabled => True));
   GNAT.Sockets.Bind_Socket
     (Socket  => Receiver,
      Address => (Family => GNAT.Sockets.Family_Inet,
                  Addr   => GNAT.Sockets.Inet_Addr ("127.0.0.1"),
                  Port   => 12321));
   GNAT.Sockets.Listen_Socket (Socket => Receiver);
   loop
      GNAT.Sockets.Accept_Socket
        (Server  => Receiver,
         Socket  => Connection,
         Address => Client);
      Ada.Text_IO.Put_Line
        ("Client connected from " & GNAT.Sockets.Image (Client));
      Channel := GNAT.Sockets.Stream (Connection);
      begin
         loop
            Character'Output (Channel, Character'Input (Channel));
         end loop;
      exception
         when Ada.IO_Exceptions.End_Error =>
            null;
      end;
      GNAT.Sockets.Close_Socket (Connection);
   end loop;
end Echo_Server;
```


Multi-threaded, multiple clients served. On OS X 10.10.5 with gcc 4.9.1, serves a maximum of about 2000 threads (communication tasks) per process.

```Ada

with Ada.Text_IO;
with Ada.IO_Exceptions;
with GNAT.Sockets;
procedure echo_server_multi is
-- Multiple socket connections example based on Rosetta Code echo server.

   Tasks_To_Create : constant := 3; -- simultaneous socket connections.

-------------------------------------------------------------------------------
-- Use stack to pop the next free task index. When a task finishes its
-- asynchronous (no rendezvous) phase, it pushes the index back on the stack.
   type Integer_List is array (1..Tasks_To_Create) of integer;
   subtype Counter is integer range 0 .. Tasks_To_Create;
   subtype Index is integer range 1 .. Tasks_To_Create;
   protected type Info is
      procedure Push_Stack (Return_Task_Index : in Index);
      procedure Initialize_Stack;
      entry Pop_Stack (Get_Task_Index : out Index);
   private
      Task_Stack   : Integer_List; -- Stack of free-to-use tasks.
      Stack_Pointer: Counter := 0;
   end Info;

   protected body Info is
      procedure Push_Stack (Return_Task_Index : in Index) is
      begin -- Performed by tasks that were popped, so won't overflow.
         Stack_Pointer := Stack_Pointer + 1;
         Task_Stack(Stack_Pointer) := Return_Task_Index;
      end;

      entry Pop_Stack (Get_Task_Index : out Index) when Stack_Pointer /= 0 is
      begin -- guarded against underflow.
         Get_Task_Index := Task_Stack(Stack_Pointer);
         Stack_Pointer := Stack_Pointer -  1;
      end;         

      procedure Initialize_Stack is
      begin
         for I in Task_Stack'range loop
            Push_Stack (I);
         end loop;
      end;
   end Info;

   Task_Info : Info;
     
-------------------------------------------------------------------------------
   task type SocketTask is
      -- Rendezvous the setup, which sets the parameters for entry Echo.
      entry Setup (Connection : GNAT.Sockets.Socket_Type;
                   Client     : GNAT.Sockets.Sock_Addr_Type;
                   Channel    : GNAT.Sockets.Stream_Access;
                   Task_Index : Index);
      -- Echo accepts the asynchronous phase, i.e. no rendezvous. When the
      -- communication is over, push the task number back on the stack.
      entry Echo;
   end SocketTask;

   task body SocketTask is
      my_Connection : GNAT.Sockets.Socket_Type;
      my_Client     : GNAT.Sockets.Sock_Addr_Type;
      my_Channel    : GNAT.Sockets.Stream_Access;
      my_Index      : Index;
   begin
      loop -- Infinitely reusable
         accept Setup (Connection : GNAT.Sockets.Socket_Type; 
                       Client  : GNAT.Sockets.Sock_Addr_Type; 
                       Channel : GNAT.Sockets.Stream_Access;
                       Task_Index   : Index) do
            -- Store parameters and mark task busy.
            my_Connection := Connection;
            my_Client     := Client;
            my_Channel    := Channel;
            my_Index      := Task_Index;
         end;
   
         accept Echo; -- Do the echo communications.
         begin
            Ada.Text_IO.Put_Line ("Task " & integer'image(my_Index));
            loop
               Character'Output (my_Channel, Character'Input(my_Channel));
            end loop;
         exception
            when Ada.IO_Exceptions.End_Error =>
              Ada.Text_IO.Put_Line ("Echo " & integer'image(my_Index) & " end");
            when others => 
              Ada.Text_IO.Put_Line ("Echo " & integer'image(my_Index) & " err");
         end;
         GNAT.Sockets.Close_Socket (my_Connection);
         Task_Info.Push_Stack (my_Index); -- Return to stack of unused tasks.
      end loop;
   end SocketTask;

-------------------------------------------------------------------------------
-- Setup the socket receiver, initialize the task stack, and then loop,
-- blocking on Accept_Socket, using Pop_Stack for the next free task from the
-- stack, waiting if necessary.
   task type SocketServer (my_Port : GNAT.Sockets.Port_Type) is
      entry Listen;
   end SocketServer;

   task body SocketServer is
   Receiver   : GNAT.Sockets.Socket_Type;
   Connection : GNAT.Sockets.Socket_Type;
   Client     : GNAT.Sockets.Sock_Addr_Type;
   Channel    : GNAT.Sockets.Stream_Access;
   Worker     : array (1..Tasks_To_Create) of SocketTask;
   Use_Task   : Index;

   begin
      accept Listen;
      GNAT.Sockets.Create_Socket (Socket => Receiver);
      GNAT.Sockets.Set_Socket_Option
        (Socket => Receiver,
         Level  => GNAT.Sockets.Socket_Level,
         Option => (Name    => GNAT.Sockets.Reuse_Address, Enabled => True));
      GNAT.Sockets.Bind_Socket
        (Socket  => Receiver,
         Address => (Family => GNAT.Sockets.Family_Inet,
                     Addr   => GNAT.Sockets.Inet_Addr ("127.0.0.1"),
                     Port   => my_Port));
      GNAT.Sockets.Listen_Socket (Socket => Receiver);
      Task_Info.Initialize_Stack;
Find: loop -- Block for connection and take next free task.
         GNAT.Sockets.Accept_Socket
           (Server  => Receiver,
            Socket  => Connection,
            Address => Client);
         Ada.Text_IO.Put_Line ("Connect " & GNAT.Sockets.Image(Client));
         Channel := GNAT.Sockets.Stream (Connection);
         Task_Info.Pop_Stack(Use_Task); -- Protected guard waits if full house.
         -- Setup the socket in this task in rendezvous.
         Worker(Use_Task).Setup(Connection,Client, Channel,Use_Task);
         -- Run the asynchronous task for the socket communications.
         Worker(Use_Task).Echo; -- Start echo loop.
      end loop Find;
   end SocketServer;

   Echo_Server : SocketServer(my_Port => 12321);

-------------------------------------------------------------------------------
begin
   Echo_Server.Listen;
end echo_server_multi;


```



## Aime


```aime
void
readc(dispatch w, file i, file o, data b)
{
    integer e;
    data t;

    while (1) {
        e = f_b_read(i, t, 1 << 10);
        if (e < 1) {
            if (e == -1) {
                w_resign(w, i);
            }

            break;
        } else {
            e = b_frame(t, '\n');
            if (e != -1) {
                e += 1;
                b_rule(b, -1, t, 0, e);
                f_data(o, b);
                w_register(w, o);
                b_ecopy(b, t, e, ~t - e);
            } else {
                b_add(b, t);
            }
        }
    }
}

void
serve(dispatch w, file s)
{
    file i, o;
    data b;

    accept(i, o, s, NONBLOCKING_INPUT | NONBLOCKING_OUTPUT);
    w.watch(i, readc, w, i, o, b);
}

integer
main(void)
{
    dispatch w;
    file s;

    tcpip_listen(s, 12321, 0);
    w.watch(s, serve, w, s);
    w.press;

    0;
}
```



## AutoHotkey

<tt>echoserver.ahk</tt>, modified from 
[http://www.autohotkey.com/forum/topic13829.html script] by zed gecko. 

```AutoHotkey
#SingleInstance Force
Network_Port = 12321
Network_Address = 127.0.0.1

NewData := false
DataReceived =
Gosub Connection_Init
return

Connection_Init:
OnExit, ExitSub
socket := PrepareForIncomingConnection(Network_Address, Network_Port)
if socket = -1
    ExitApp

Process, Exist
DetectHiddenWindows On
ScriptMainWindowId := WinExist("ahk_class AutoHotkey ahk_pid " . ErrorLevel)
DetectHiddenWindows Off

NotificationMsg = 0x5555
OnMessage(NotificationMsg, "ReceiveData")

ExitMsg = 0x6666
OnMessage(ExitMsg, "ExitData")

FD_READ = 1
FD_CLOSE = 32
FD_CONNECT = 20

if DllCall("Ws2_32\WSAAsyncSelect", "UInt", socket, 
      "UInt", ScriptMainWindowId, "UInt", ExitMsg, "Int", FD_CLOSE)
{
    msgbox, closed
}

if DllCall("Ws2_32\WSAAsyncSelect", "UInt", socket, 
        "UInt", ScriptMainWindowId, "UInt", NotificationMsg, "Int",
        FD_READ|FD_CONNECT) 
{
    MsgBox % "WSAAsyncSelect() indicated Winsock error "
          . DllCall("Ws2_32\WSAGetLastError")
    DllCall("Ws2_32\WSAAsyncSelect", "UInt", socket, 
          "UInt", ScriptMainWindowId, "UInt", ExitMsg, "Int", FD_CLOSE)
    ExitApp
}

SetTimer, NewConnectionCheck, 500
return

PrepareForIncomingConnection(IPAddress, Port)
{
    VarSetCapacity(wsaData, 32)
    result := DllCall("Ws2_32\WSAStartup", "UShort", 0x0002, "UInt", &wsaData)
    if ErrorLevel
    {
        MsgBox % "WSAStartup() could not be called due to error %ErrorLevel%. "
                . "Winsock 2.0 or higher is required."
        return -1
    }
    if result
    {
        MsgBox % "WSAStartup() indicated Winsock error "
                . DllCall("Ws2_32\WSAGetLastError")
        return -1
    }
    AF_INET = 2
    SOCK_STREAM = 1
    IPPROTO_TCP = 6
    socket := DllCall("Ws2_32\socket", "Int", AF_INET, 
          "Int", SOCK_STREAM, "Int", IPPROTO_TCP)
    if socket = -1
    {
        MsgBox % "socket() indicated Winsock error "
                . DllCall("Ws2_32\WSAGetLastError")
        return -1
    }
    SizeOfSocketAddress = 16
    VarSetCapacity(SocketAddress, SizeOfSocketAddress)
    InsertInteger(2, SocketAddress, 0, AF_INET)
    InsertInteger(DllCall("Ws2_32\htons", "UShort", Port), SocketAddress, 2, 2)
    InsertInteger(DllCall("Ws2_32\inet_addr", "Str", IPAddress),
            SocketAddress, 4, 4)
    if DllCall("Ws2_32\bind", "UInt", socket, 
            "UInt", &SocketAddress, "Int", SizeOfSocketAddress)
    {
        MsgBox % "bind() indicated Winsock error "
                . DllCall("Ws2_32\WSAGetLastError") . "?"
        return -1
    }
    if DllCall("Ws2_32\listen", "UInt", socket, "UInt", "SOMAXCONN")
    {
        MsgBox % "LISTEN() indicated Winsock error "
                . DllCall("Ws2_32\WSAGetLastError") . "?"
        return -1
    }
    return socket
}

ReceiveData(wParam, lParam)
{
    global DataReceived
    global NewData
    global mydata
    global ConnectionList
    socket := wParam
    ReceivedDataSize = 4096
    Loop
    {
        VarSetCapacity(ReceivedData, ReceivedDataSize, 0)
        ReceivedDataLength := DllCall("Ws2_32\recv", "UInt", 
              socket, "Str", ReceivedData, "Int", ReceivedDataSize, "Int", 0)
	if ReceivedDataLength = 0
        {   
            StringReplace, ConnectionList, ConnectionList, %socket%`n
            DllCall("Ws2_32\closesocket", "UInt", socket)
        } 
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
            {
                MsgBox % "recv() indicated Winsock error " . WinsockError
                StringReplace, ConnectionList, ConnectionList, %socket%`n
                DllCall("Ws2_32\closesocket", "UInt", socket)
            }
        }
        mydata := ReceivedData
        gosub myreceive
	if (A_Index = 1)
            TempDataReceived =
                TempDataReceived = %TempDataReceived%%ReceivedData%
    }
    return 1
}

ExitData(wParam, lParam)
{
    global ConnectionList
    socket := wParam
    ReceivedDataSize = 16
    VarSetCapacity(ReceivedData, ReceivedDataSize, 0)
    ReceivedDataLength := DllCall("Ws2_32\recv", "UInt", socket, 
          "Str", ReceivedData, "Int", ReceivedDataSize, "Int", 0)
    StringReplace, ConnectionList, ConnectionList, %socket%`n
    DllCall("Ws2_32\closesocket", "UInt", socket)
    return 1
}

SendData(wParam,SendData)
{
    SendDataSize := VarSetCapacity(SendData)
    SendDataSize += 1
    Loop, parse, wParam, `n
    {
        If A_LoopField =
           Continue
        socket := A_LoopField
        sendret := DllCall("Ws2_32\send", "UInt", socket, 
              "Str", SendData, "Int", SendDatasize, "Int", 0)
    }
}


InsertInteger(pInteger, ByRef pDest, pOffset = 0, pSize = 4)
{
    Loop %pSize%
        DllCall("RtlFillMemory", "UInt", &pDest + pOffset + A_Index-1, 
                "UInt", 1, "UChar", pInteger >> 8*(A_Index-1) & 0xFF)
}

NewConnectionCheck:
ConnectionCheck := DllCall("Ws2_32\accept", "UInt", socket, 
      "UInt", &SocketAddress, "Int", SizeOfSocketAddress)
if ConnectionCheck > 1
    ConnectionList = %ConnectionList%%ConnectionCheck%`n
Return

SendProcedure:
If ConnectionList <>
{
    SendText = %A_Hour%:%A_Min%:%A_Sec%
    SendData(ConnectionList,SendText)
}
Return

myreceive:
 TrayTip, server, %mydata%, ,16
  return

GuiClose:
ExitSub:
DllCall("Ws2_32\WSACleanup")
ExitApp
```

A [[/AutoHotkey Client|client]] is also available for testing this code.


## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      INSTALL @lib$+"SOCKLIB"
      PROC_initsockets
      
      maxSess% = 8
      DIM sock%(maxSess%-1), rcvd$(maxSess%-1), Buffer% 255
      
      ON ERROR PRINT REPORT$ : PROC_exitsockets : END
      ON CLOSE PROC_exitsockets : QUIT
      
      crlf$ = CHR$13 + CHR$10
      port$ = "12321"
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
              crlf% = INSTR(rcvd$(i%), crlf$)
              IF crlf% THEN
                echo$ = LEFT$(rcvd$(i%), crlf%-1)
                res% = FN_writelinesocket(sock%(i%), echo$)
                rcvd$(i%) = MID$(rcvd$(i%), crlf%+2)
              ENDIF
            ELSE
              PROC_closesocket(sock%(i%))
              PRINT "Connection on socket " ; sock%(i%) " closed"
              sock%(i%) = 0
            ENDIF
          ENDIF
        NEXT i%
        
        WAIT 0
      UNTIL FALSE
      END
```

'''Sample output:'''

```txt

Host name is PC236
Listening on port 12321
Connection on socket 1016 opened
Connection on socket 1012 opened
Connection on socket 1016 closed
Connection on socket 1016 opened
Connection on socket 1016 closed
Connection on socket 1012 closed

```



## C

{{works with|POSIX}}
This is a rather standard code (details apart); the reference guide for such a code is the [http://beej.us/guide/bgnet Beej's Guide to Network programming]. The dependency from POSIX is mainly in the use of the <tt>read</tt> and <tt>write</tt> functions, (using the socket as a file descriptor sometimes make things simpler).


```c>#include <stdio.h

#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <unistd.h>
#include <sys/wait.h>
#include <signal.h>

#define MAX_ENQUEUED 20
#define BUF_LEN 256
#define PORT_STR "12321"

/* ------------------------------------------------------------ */
/* How to clean up after dead child processes                   */
/* ------------------------------------------------------------ */

void wait_for_zombie(int s)
{
    while(waitpid(-1, NULL, WNOHANG) > 0) ;
}

/* ------------------------------------------------------------ */
/* Core of implementation of a child process                    */
/* ------------------------------------------------------------ */

void echo_lines(int csock)
{
    char buf[BUF_LEN];
    int r;

    while( (r = read(csock, buf, BUF_LEN)) > 0 ) {
        (void)write(csock, buf, r);
    }
    exit(EXIT_SUCCESS);
}

/* ------------------------------------------------------------ */
/* Core of implementation of the parent process                 */
/* ------------------------------------------------------------ */

void take_connections_forever(int ssock)
{
    for(;;) {
        struct sockaddr addr;
        socklen_t addr_size = sizeof(addr);
        int csock;

        /* Block until we take one connection to the server socket */
        csock = accept(ssock, &addr, &addr_size);

        /* If it was a successful connection, spawn a worker process to service it */
        if ( csock == -1 ) {
            perror("accept");
        } else if ( fork() == 0 ) {
            close(ssock);
            echo_lines(csock);
        } else {
            close(csock);
        }
    }
}

/* ------------------------------------------------------------ */
/* The server process's one-off setup code                      */
/* ------------------------------------------------------------ */

int main()
{
    struct addrinfo hints, *res;
    struct sigaction sa;
    int sock;

    /* Look up the address to bind to */
    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = AI_PASSIVE;
    if ( getaddrinfo(NULL, PORT_STR, &hints, &res) != 0 ) {
        perror("getaddrinfo");
        exit(EXIT_FAILURE);
    }

    /* Make a socket */
    if ( (sock = socket(res->ai_family, res->ai_socktype, res->ai_protocol)) == -1 ) {
        perror("socket");
        exit(EXIT_FAILURE);
    }

    /* Arrange to clean up child processes (the workers) */
    sa.sa_handler = wait_for_zombie;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = SA_RESTART;
    if ( sigaction(SIGCHLD, &sa, NULL) == -1 ) {
        perror("sigaction");
        exit(EXIT_FAILURE);
    }

    /* Associate the socket with its address */
    if ( bind(sock, res->ai_addr, res->ai_addrlen) != 0 ) {
        perror("bind");
        exit(EXIT_FAILURE);
    }

    freeaddrinfo(res);

    /* State that we've opened a server socket and are listening for connections */
    if ( listen(sock, MAX_ENQUEUED) != 0 ) {
        perror("listen");
        exit(EXIT_FAILURE);
    }

    /* Serve the listening socket until killed */
    take_connections_forever(sock);
    return EXIT_SUCCESS;
}
```


=={{header|C sharp|C#}}==

```csharp
using System.Net.Sockets;
using System.Threading;

namespace ConsoleApplication1
{
    class Program
    {
        static TcpListener listen;
        static Thread serverthread;

        static void Main(string[] args)
        {
            listen = new TcpListener(System.Net.IPAddress.Parse("127.0.0.1"), 12321);
            serverthread = new Thread(new ThreadStart(DoListen));
            serverthread.Start();
        }

        private static void DoListen()
        {
            // Listen
            listen.Start();
            Console.WriteLine("Server: Started server");

            while (true)
            {
                Console.WriteLine("Server: Waiting...");
                TcpClient client = listen.AcceptTcpClient();
                Console.WriteLine("Server: Waited");

                // New thread with client
                Thread clientThread = new Thread(new ParameterizedThreadStart(DoClient));
                clientThread.Start(client);
            }
        }

        private static void DoClient(object client)
        {
            // Read data
            TcpClient tClient = (TcpClient)client;

            Console.WriteLine("Client (Thread: {0}): Connected!", Thread.CurrentThread.ManagedThreadId);
            do
            {
                if (!tClient.Connected)
                { 
                    tClient.Close();
                    Thread.CurrentThread.Abort();       // Kill thread.
                }

                if (tClient.Available > 0)
                {
                    // Resend
                    byte pByte = (byte)tClient.GetStream().ReadByte();
                    Console.WriteLine("Client (Thread: {0}): Data {1}", Thread.CurrentThread.ManagedThreadId, pByte);
                    tClient.GetStream().WriteByte(pByte);
                }

                // Pause
                Thread.Sleep(100);
            } while (true);
        }
    }
}
```



## Clojure


```lisp
(use '[clojure.contrib.server-socket :only (create-server)])
(use '[clojure.contrib.duck-streams :only (read-lines write-lines)])

(defn echo [input output]
  (write-lines (java.io.PrintWriter. output true) (read-lines input)))

(create-server 12321 echo)
```


Note here that an auto-flushing PrintWriter needs to be created, otherwise 'output' could simply be passed to write-lines.


## CoffeeScript

{{trans|JavaScript}}
{{works with|node.js}}

```coffeescript

net = require("net")
server = net.createServer (conn) ->
  console.log "Connection from #{conn.remoteAddress} on port  #{conn.remotePort}"
  conn.setEncoding "utf8"
  buffer = ""
  conn.on "data", (data) ->
    i = 0

    while i <= data.length
      char = data.charAt(i)
      buffer += char
      if char is "\n"
        conn.write buffer
        buffer = ""
      i++

server.listen 12321, "localhost"

```



## Common Lisp


Here is a basic [http://common-lisp.net/project/usocket/ :usocket] example (it should work with any Common Lisp):


```lisp
(ql:quickload (list :usocket))
(defpackage :echo (:use :cl :usocket))
(in-package :echo)

(defun read-all (stream)
  (loop for char = (read-char-no-hang stream nil :eof)
     until (or (null char) (eql char :eof)) collect char into msg
     finally (return (values msg char))))

(defun echo-server (port &optional (log-stream *standard-output*))
  (let ((connections (list (socket-listen "127.0.0.1" port :reuse-address t))))
    (unwind-protect
	 (loop (loop for ready in (wait-for-input connections :ready-only t)
		  do (if (typep ready 'stream-server-usocket)
			 (push (socket-accept ready) connections)
			 (let* ((stream (socket-stream ready))
				(msg (concatenate 'string "You said: " (read-all stream))))
			   (format log-stream "Got message...~%")
			   (write-string msg stream)
			   (socket-close ready)
			   (setf connections (remove ready connections))))))
      (loop for c in connections do (loop while (socket-close c))))))

(echo-server 12321)

```


It's single threaded, so you can't REPL around with a running server. You'll need to start a second Lisp prompt, load the above and


```lisp
(defun echo-send (message port)
  (with-client-socket (sock str "127.0.0.1" port)
    (write-string message str)
    (force-output str)
    (when (wait-for-input sock :timeout 5)
      (coerce (read-all str) 'string))))

(echo-send "Hello echo!" 12321)

```


The return value of that call should be "You said: Hello echo!".

----

The usocket library notwithstanding, sockets are not a standard part of Common Lisp, but many implementations provide them. Here is a CLISP-specific example: {{works with|CLISP}}

```lisp
(defvar *clients* '()
    "This is a list of (socket :input status) which is used with
`socket:socket-status' to test for data ready on a socket.")

(defun echo-server (port)
    "Listen on `port' for new client connections and for data arriving on
any existing client connections"
    (let ((server (socket:socket-server port)))
        (format t "Echo service listening on port ~a:~d~%"
            (socket:socket-server-host server)
            (socket:socket-server-port server))
        (unwind-protect
            (loop 
                (when (socket:socket-status server 0 1)
                    (echo-accept-client (socket:socket-accept server 
                                            :external-format :dos
                                            :buffered t)))
                (when *clients*
                    (socket:socket-status *clients* 0 1)
                    (mapcar #'(lambda (client)
                                  (when (eq :input (cddr client))
                                      (echo-service-client (car client)))
                                  (when (eq :eof (cddr client))
                                      (echo-close-client (car client)))) *clients*)))
            (socket-server-close server))))

(defun echo-accept-client (socket)
    "Accept a new client connection and add it to the watch list."
    (multiple-value-bind 
        (host port) (socket:socket-stream-peer socket)
        (format t "Connect from ~a:~d~%" host port))
    (push (list socket :input nil) *clients*))
    
(defun echo-service-client (socket)
    (let ((line (read-line socket nil nil)))
        (princ line socket)
        (finish-output socket)))

(defun echo-close-client (socket)
    "Close a client connection and remove it from the watch list."
    (multiple-value-bind 
        (host port) (socket:socket-stream-peer socket)
        (format t "Closing connection from ~a:~d~%" host port))
    (close socket)
    (setq *clients* (remove socket *clients* :key #'car)))

(echo-server 12321)
```



## D

This is a very basic server that processes the buffers one character at a time.  In a real-world application, the buffers would be larger. More seriously, it processes one listener at a time. If the <code>currSock.receive()</code> blocks, the loop will not process other clients. This opens the door for a trivial denial-of-service attack. A realistic echo service must multiplex clients.

```d
import std.array, std.socket;

void main() {
    auto listener = new TcpSocket;
    assert(listener.isAlive);
    listener.bind(new InternetAddress(12321));
    listener.listen(10);

    Socket currSock;
    uint bytesRead;
    ubyte[1] buff;

    while (true) {
        currSock = listener.accept();
        while ((bytesRead = currSock.receive(buff)) > 0)
            currSock.send(buff);
        currSock.close();
        buff.clear();
    }
}
```


This example will handle many connections.

```d
import std.stdio, std.socket, std.array;

void main() {
    enum ushort port = 7;
    enum int backlog = 10;
    enum int max_connections = 60;
    enum BUFFER_SIZE = 16;

    auto listener = new TcpSocket;
    assert(listener.isAlive);
    listener.bind(new InternetAddress(port));
    listener.listen(backlog);
    debug writeln("Listening on port ", port);

    // Room for listener.
    auto sset = new SocketSet(max_connections + 1);

    Socket[] sockets;
    char[BUFFER_SIZE] buf;

    for (;; sset.reset()) {
        sset.add(listener);
        foreach (each; sockets)
            sset.add(each);

        // Update socket set with only those sockets that have data
        // avaliable for reading. Options are for read, write,
        // and error.
        Socket.select(sset, null, null);

        // Read the data from each socket remaining, and handle
        // the request.
        for (int i = 0; ; i++) {
NEXT:
            if (i == sockets.length)
                break;
            if (sset.isSet(sockets[i])) {
                int read = sockets[i].receive(buf);
                if (Socket.ERROR == read) {
                    debug writeln("Connection error.");
                    goto SOCK_DOWN;
                } else if (read == 0) {
                    debug {
                        try {
                            // If the connection closed due to an
                            // error, remoteAddress() could fail.
                            writefln("Connection from %s closed.",
                                     sockets[i].remoteAddress()
                                     .toString());
                        } catch (SocketException) {
                            writeln("Connection closed.");
                        }
                    }
SOCK_DOWN:
                    sockets[i].close(); //Release socket resources now.

                    // Remove from socket from sockets, and id from
                    // threads.
                    if (i != sockets.length - 1)
                        sockets[i] = sockets.back;
                    sockets.length--;
                    debug writeln("\tTotal connections: ",
                                  sockets.length);
                    goto NEXT; // -i- is still the NEXT index.
                } else {
                    debug
                        writefln("Received %d bytes from %s:"
                                 ~ "\n-----\n%s\n-----",
                                 read,
                                 sockets[i].remoteAddress().toString(),
                                 buf[0 .. read]);

                    // Echo what was sent.
                    sockets[i].send(buf[0 .. read]);
                }
            }
        }

        // Connection request.
        if (sset.isSet(listener)) {
            Socket sn;
            try {
                if (sockets.length < max_connections) {
                    sn = listener.accept();
                    debug writefln("Connection from %s established.",
                                   sn.remoteAddress().toString());
                    assert(sn.isAlive);
                    assert(listener.isAlive);
                    sockets ~= sn;
                    debug writefln("\tTotal connections: %d",
                                   sockets.length);
                } else {
                    sn = listener.accept();
                    debug writefln("Rejected connection from %s;"
                                   ~ " too many connections.",
                                   sn.remoteAddress().toString());
                    assert(sn.isAlive);
                    sn.close();
                    assert(!sn.isAlive);
                    assert(listener.isAlive);
                }
            } catch (Exception e) {
                debug writefln("Error accepting: %s", e.toString());
                if (sn)
                    sn.close();
            }
        }
    }
}
```



## Delphi


```Delphi
program EchoServer;

{$APPTYPE CONSOLE}

uses SysUtils, IdContext, IdTCPServer;

type
  TEchoServer = class
  private
    FTCPServer: TIdTCPServer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure TCPServerExecute(AContext: TIdContext);
  end;

constructor TEchoServer.Create;
begin
  FTCPServer := TIdTCPServer.Create(nil);
  FTCPServer.DefaultPort := 12321;
  FTCPServer.OnExecute := TCPServerExecute;
  FTCPServer.Active := True;
end;

destructor TEchoServer.Destroy;
begin
  FTCPServer.Active := False;
  FTCPServer.Free;
  inherited Destroy;
end;

procedure TEchoServer.TCPServerExecute(AContext: TIdContext);
var
  lCmdLine: string;
begin
  lCmdLine := AContext.Connection.IOHandler.ReadLn;
  Writeln('>' + lCmdLine);
  AContext.Connection.IOHandler.Writeln('>' + lCmdLine);

  if SameText(lCmdLine, 'QUIT') then
  begin
    AContext.Connection.IOHandler.Writeln('Disconnecting');
    AContext.Connection.Disconnect;
  end;
end;

var
  lEchoServer: TEchoServer;
begin
  lEchoServer := TEchoServer.Create;
  try
    Writeln('Delphi Echo Server');
    Writeln('Press Enter to quit');
    Readln;
  finally
    lEchoServer.Free;
  end;
end.
```



## Erlang


```erlang
-module(echo).
-export([start/0]).

start() ->
    spawn(fun () -> {ok, Sock} = gen_tcp:listen(12321, [{packet, line}]),
                    echo_loop(Sock)
          end).

echo_loop(Sock) ->
    {ok, Conn} = gen_tcp:accept(Sock),
    io:format("Got connection: ~p~n", [Conn]),
    Handler = spawn(fun () -> handle(Conn) end),
    gen_tcp:controlling_process(Conn, Handler),
    echo_loop(Sock).

handle(Conn) ->
    receive
        {tcp, Conn, Data} ->
            gen_tcp:send(Conn, Data),
            handle(Conn);
        {tcp_closed, Conn} ->
            io:format("Connection closed: ~p~n", [Conn])
    end.
```


=={{header|F Sharp|F#}}==

```fsharp
open System.IO
open System.Net
open System.Net.Sockets

let service (client:TcpClient) =
    use stream = client.GetStream()
    use out = new StreamWriter(stream, AutoFlush = true)
    use inp = new StreamReader(stream)
    while not inp.EndOfStream do
        match inp.ReadLine() with
        | line -> printfn "< %s" line
                  out.WriteLine(line)
    printfn "closed %A" client.Client.RemoteEndPoint
    client.Close |> ignore

let EchoService = 
    let socket = new TcpListener(IPAddress.Loopback, 12321)
    do socket.Start()
    printfn "echo service listening on %A" socket.Server.LocalEndPoint
    while true do
        let client = socket.AcceptTcpClient()
        printfn "connect from %A" client.Client.RemoteEndPoint
        let job = async {
            use c = client in try service client with _ -> () }
        Async.Start job

[<EntryPoint>]
let main _ =
    EchoService
    0
```



## Factor

Connections get logged to <code>/place-where-factor-is/logs/echo-server</code>.

```factor
USING: accessors io io.encodings.utf8 io.servers io.sockets threads ;
IN: rosetta.echo

CONSTANT: echo-port 12321

: handle-client ( -- )
   [ print flush ] each-line ;

: <echo-server> ( -- threaded-server )
    utf8 <threaded-server>
        "echo server" >>name
        echo-port >>insecure
        [ handle-client ] >>handler ;

: start-echo-server ( -- )
    <echo-server> [ start-server ] in-thread start-server drop ;

```



## Forth

{{works with|GNU Forth|0.7.0}}

```forth
include unix/socket.fs

128 constant size

: (echo) ( sock buf -- sock buf )
  begin
    cr ." waiting..."
    2dup 2dup size read-socket nip
    dup 0>
  while
    ."  got: " 2dup type
    rot write-socket
  repeat
  drop drop drop ;

create buf size allot

: echo-server ( port -- )
  cr ." Listening on " dup .
  create-server
  dup 4 listen
  begin
    dup accept-socket
    cr ." Connection!"
    buf ['] (echo) catch
    cr ." Disconnected (" . ." )"
    drop close-socket
  again ;

12321 echo-server
```

''TODO: use tasker.fs and non-blocking semantics to handle mutliple connections''


## Go


```go
package main

import (
	"fmt"
	"net"
	"bufio"
)

func echo(s net.Conn, i int) {
	defer s.Close();

	fmt.Printf("%d: %v <-> %v\n", i, s.LocalAddr(), s.RemoteAddr())
	b := bufio.NewReader(s)
	for {
		line, e := b.ReadBytes('\n')
		if e != nil {
			break
		}
		s.Write(line)
	}
	fmt.Printf("%d: closed\n", i)
}

func main() {
	l, e := net.Listen("tcp", ":12321")
	for i := 0; e == nil; i++ {
		var s net.Conn
		s, e = l.Accept()
		go echo(s, i)
	}
}
```



## Haskell



```haskell
module Main where
import Network (withSocketsDo, accept, listenOn, sClose, PortID(PortNumber))
import Control.Monad (forever)
import System.IO (hGetLine, hPutStrLn, hFlush, hClose)
import System.IO.Error (isEOFError)
import Control.Concurrent (forkIO)
import Control.Exception (bracket)

-- For convenience in testing, ensure that the listen socket is closed if the main loop is aborted
withListenOn port body = bracket (listenOn port) sClose body

echo (handle, host, port) = catch (forever doOneLine) stop where
  doOneLine = do line <- hGetLine handle
                 print (host, port, init line)
                 hPutStrLn handle line
                 hFlush handle
  stop error = do putStrLn $ "Closed connection from " ++ show (host, port) ++ " due to " ++ show error
                  hClose handle

main = withSocketsDo $
  withListenOn (PortNumber 12321) $ \listener ->
    forever $ do
      acc@(_, host, port) <- accept listener
      putStrLn $ "Accepted connection from " ++ show (host, port)
      forkIO (echo acc)
```


==Icon and {{header|Unicon}}==

The following is Unicon-specific:

```unicon
global mlck, nCons

procedure main()
    mlck := mutex()
    nCons := 0
    while f := open(":12321","na") do {
        handle_client(f)
        critical mlck: if nCons <= 0 then close(f)
        }
end

procedure handle_client(f)
    critical mlck: nCons +:= 1
    thread {
        select(f,1000) & repeat writes(f,reads(f))
        critical mlck: nCons -:= 1
        }
end
```



## Java


```java
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;

public class EchoServer {
	ServerSocket serverSocket;	
	public EchoServer(){		
	}
	
	public void start() {		
		try {
			serverSocket = new ServerSocket(12321);
			while(true){
				Thread clientThread = new Thread(new ClientHandler(serverSocket.accept()));
				clientThread.start();
			}
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			try {
				System.out.println("closing server socket");
				serverSocket.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		
	}
	
	public static void main(String[] args) {
		EchoServer es = new EchoServer();
		es.start();
	}
}

class ClientHandler implements Runnable {
	private static int numConnections;
	private int connectionId = 0;
	Socket clientSocket;
	
	public ClientHandler(Socket s) {
		connectionId = numConnections++;
		System.out.println("handling connection, #" + connectionId);
		clientSocket = s;
	}

	public void run() {
		PrintWriter out = null;
		BufferedReader in = null;
		try {
			out = new PrintWriter(clientSocket.getOutputStream(), true);
			in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));
			String inputLine, outputLine;
			while((inputLine = in.readLine()) != null){
				outputLine = inputLine;
				System.out.println("received: " + outputLine);
				out.write(outputLine+"\n");
				out.flush();
				if (outputLine.equals("exit"))
					break;
			}
		} catch(Exception e) {
			e.printStackTrace();
		} finally {
			out.close();
			try {
				in.close();
				clientSocket.close();
				System.out.println("closing connection, #" + connectionId);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
}
```



## JavaScript

{{works with|Node.js}}

```javascript
var net = require('net');

var server = net.createServer(function(conn) {
  console.log("Connection from " + conn.remoteAddress + " on port " + conn.remotePort);
  conn.setEncoding("utf8");
  var buffer = "";

  conn.on("data", function(data) {
    for(var i = 0; i <= data.length; i++) {
      var char = data.charAt(i);
      buffer += char;
      if(char == "\n") {
        conn.write(buffer);
        buffer = "";
      }
    }
  });
});

server.listen(12321, "localhost");
```




## Julia


```julia

using Sockets # for version 1.0
println("Echo server on port 12321")
try
    server = listen(12321)
    instance = 0
    while true
        sock = accept(server)
        instance += 1
        socklabel = "$(getsockname(sock)) number $instance"
        @async begin 
            println("Server connected to socket $socklabel")
            write(sock, "Connected to echo server.\r\n")
            while isopen(sock)
                str = readline(sock)
                write(sock,"$str\r\n")
                println("Echoed $str to socket $socklabel")
            end
            println("Closed socket $socklabel")
        end
    end
catch y
    println("Caught exception: $y")
end

```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.3

import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.PrintWriter
import java.net.ServerSocket
import java.net.Socket

class ClientHandler(private val clientSocket: Socket): Runnable {
    private val connectionId: Int

    init {
        connectionId = ++numConnections
        println("Handling connection, #$connectionId")
    }

    override fun run() {
        val pw = PrintWriter(clientSocket.outputStream, true)
        val br = BufferedReader(InputStreamReader(clientSocket.inputStream))
        while (true) {
            val line = br.readLine() ?: break
            println("Received: $line")
            pw.write("$line\n")
            pw.flush()
            if (line == "exit") break
        }
        br.close()
        pw.close()
        clientSocket.close()
        println("Closing connection, #$connectionId")
    }

    private companion object {
        var numConnections = 0
    }
}

fun main(args: Array<String>) {
    val serverSocket = ServerSocket(12321)
    try {
        while (true) {
           Thread(ClientHandler(serverSocket.accept())).start()
        }
    }
    finally {
        serverSocket.close()
        println("Closing server socket")
    }
}
```


{{out}}
Quick test using telnet:

```txt
telnet localhost 12321
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
Hello
Hello
Goodbye
Goodbye
exit
exit
Connection closed by foreign host.
```


Echo server window (closing with ^C):

```txt
Handling connection, #1
Received: Hello
Received: Goodbye
Received: exit
Closing connection, #1
^C
```



## LFE

{{trans|Erlang}}

Paste into the LFE REPL:

```lisp

(defun start ()
  (spawn (lambda ()
           (let ((`#(ok ,socket) (gen_tcp:listen 12321 `(#(packet line)))))
             (echo-loop socket)))))

(defun echo-loop (socket)
  (let* ((`#(ok ,conn) (gen_tcp:accept socket))
         (handler (spawn (lambda () (handle conn)))))
    (lfe_io:format "Got connection: ~p~n" (list conn))
    (gen_tcp:controlling_process conn handler)
    (echo-loop socket)))

(defun handle (conn)
  (receive
    (`#(tcp ,conn ,data)
     (gen_tcp:send conn data))
    (`#(tcp_closed ,conn)
     (lfe_io:format "Connection closed: ~p~n" (list conn)))))

```


Usage:


```txt

> (set server (start))
<0.38.0>
> (! server "hey!")
"hey!"
> (! server "wassup?")
"wassup?"

```



## Lua

{{libheader|LuaSocket}}
This implementation doesn't rely on coroutines because they're an additional (often confusing) notion that could make the example needlessly hard to understand.  Instead it uses a table of not-quite-non-blocking socket client objects (they block for one microsecond), which is iterated over to check on whether each one has either a line to echo or an error to warrant deletion.

```Lua
require("socket")

function checkOn (client)
    local line, err = client:receive()
    if line then
        print(tostring(client) .. " said " .. line)
        client:send(line .. "\n")
    end
    if err and err ~= "timeout" then
        print(tostring(client) .. " " .. err)
        client:close()
        return err
    end
    return nil
end

local delay, clients, newClient = 10^-6, {}
local server = assert(socket.bind("*", 12321))
server:settimeout(delay)
print("Server started")
while 1 do
    repeat
        newClient = server:accept()
        for k, v in pairs(clients) do
            if checkOn(v) then table.remove(clients, k) end
        end
    until newClient
    newClient:settimeout(delay)
    print(tostring(newClient) .. " connected")
    table.insert(clients, newClient)
end
```

Without the microsecond delays, the whole thing would become one 'hot' loop and eat all the CPU time for one core.  With them, it uses close to zero percent.


## Nim


```nim
import asyncnet, asyncdispatch

proc processClient(client: AsyncSocket) {.async.} =
  while true:
    let line = await client.recvLine()
    await client.send(line & "\c\L")

proc serve() {.async.} =
  var server = newAsyncSocket()
  server.bindAddr(Port(12321))
  server.listen()

  while true:
    let client = await server.accept()
    discard processClient(client)

discard serve()
runForever()
```



## Objeck


```objeck

use Net;
use Concurrency;

bundle Default {
  class SocketServer {
    id : static : Int;

    function : Main(args : String[]) ~ Nil {
      server := TCPSocketServer->New(12321);
      if(server->Listen(5)) {
        while(true) {
          client := server->Accept();
          service := Service->New(id->ToString());
          service->Execute(client);
          id += 1;
        };
      };
      server->Close();
    }
  }

  class Service from Thread {
    New(name : String) {
      Parent(name);
    }

    method : public : Run(param : Base) ~ Nil {
      client := param->As(TCPSocket);
      line := client->ReadString();
      while(line->Size() > 0) {
        line->PrintLine();
        line := client->ReadString();
      };
    }
  }
}

```



## Oz


```oz
declare
  ServerSocket = {New Open.socket init}

  proc {Echo Socket}
     case {Socket getS($)} of false then skip
     [] Line then
        {System.showInfo "Received line: "#Line}
        {Socket write(vs:Line#"\n")}
        {Echo Socket}
     end
  end

  class TextSocket from Open.socket Open.text end
in
  {ServerSocket bind(takePort:12321)}
  {System.showInfo "Socket bound."}

  {ServerSocket listen}
  {System.showInfo "Started listening."}

  for do
     ClientHost ClientPort
     ClientSocket = {ServerSocket accept(accepted:$
					 acceptClass:TextSocket
					 host:?ClientHost
					 port:?ClientPort
					)}
  in
     {System.showInfo "Connection accepted from "#ClientHost#":"#ClientPort#"."}
     thread
        {Echo ClientSocket}

	{System.showInfo "Connection lost: "#ClientHost#":"#ClientPort#"."}
        {ClientSocket close}
     end
  end
```


Client test code:

```oz
declare
  Socket = {New class $ from Open.socket Open.text end init}
in
  {Socket connect(port:12321)}
  {Socket write(vs:"Hello\n")}
  {System.showInfo "Client received: "#{Socket getS($)}}
  {Socket close}
```


Example session:

```txt

Socket bound.
Started listening.
Connection accepted from localhost:2048.
Received line: Hello
Client received: Hello
Connection lost: localhost:2048.

```



## Perl

This server will run indefinitely listening in the port 12321 and [http://perldoc.perl.org/functions/fork.html forking] every time a client connects, the childs listen to the client and write back.

This is an example using the [http://search.cpan.org/perldoc?IO::Socket IO::Socket] module:

```perl
use IO::Socket;
my $use_fork = 1;

my $sock = new IO::Socket::INET (
                                 LocalHost => '127.0.0.1',
                                 LocalPort => '12321',
                                 Proto => 'tcp',
                                 Listen => 1,   # maximum queued connections
                                 Reuse => 1,
                                )
		or die "socket: $!";	# no newline, so perl appends stuff

$SIG{CHLD} = 'IGNORE' if $use_fork;	# let perl deal with zombies

print "listening...\n";
while (1) {
	# declare $con 'my' so it's closed by parent every loop
        my $con = $sock->accept()
		or die "accept: $!";
	fork and next if $use_fork;	# following are for child only

	print "incoming..\n";
	print $con $_ while(<$con>);	# read each line and write back
	print "done\n";

	last	if $use_fork;	# if not forking, loop
}

# child will reach here and close its copy of $sock before exit
```


This is an equivalent program using the [http://search.cpan.org/perldoc?Net::Server Net::Server] module:

```perl
package Echo;
use base 'Net::Server::Fork';
sub process_request {
    print while <STDIN>;
}
Echo->run(port => 12321, log_level => 3);
```

It also prints the IP address and port number of every connection.

This is a more complicated example using preforking:

```perl
package Echo;
use base 'Net::Server::PreFork';
sub process_request {
    print while <STDIN>;
}
Echo->run(port => 12321, log_level => 3);
```

By default it spawns 5 child processes at startup, makes sure there are always at least 2 and at most 10 spare children available for new requests, each of which will be killed after processing 1000 requests and new ones will take their place.

## Perl 6

{{Works with|rakudo|2018.03}}

```perl6
my $socket = IO::Socket::INET.new:
    :localhost<localhost>,
    :localport<12321>,
    :listen;

while $socket.accept -> $conn {
    say "Accepted connection";
    start {
        while $conn.recv -> $stuff {
            say "Echoing $stuff";
            $conn.print($stuff);
        }
        $conn.close;
    }
}
```


Async version:

```perl6
react {
    whenever IO::Socket::Async.listen('0.0.0.0', 12321) -> $conn {
        whenever $conn.Supply.lines -> $line {
            $conn.print( "$line\n" ) ;
        }
    }
}

```



## PHP


```PHP
$socket = socket_create(AF_INET,SOCK_STREAM,SOL_TCP);
socket_bind($socket, '127.0.0.1', 12321);
socket_listen($socket);

$client_count = 0;
while (true){
  if (($client = socket_accept($socket)) === false) continue;
  $client_count++;

  $client_name = 'Unknown';
  socket_getpeername($client, $client_name);
  echo "Client {$client_count} ({$client_name}) connected\n";
  $pid = pcntl_fork();
  if($pid == -1) die('Could not fork');
  if($pid){
    pcntl_waitpid(-1, $status, WNOHANG);
    continue;
  }

  //In a child process
  while(true){
    if($input = socket_read($client, 1024)){
      socket_write($client, $input);
    } else {
      socket_shutdown($client);
      socket_close($client);
      echo "Client {$client_count} ({$client_name}) disconnected\n";
      exit();
    }
  }
}
```



## PicoLisp


```PicoLisp
(setq Port (port 12321))

(loop
   (setq Sock (listen Port))           # Listen
   (NIL (fork) (close Port))           # Accepted
   (close Sock) )                      # Parent: Close socket and continue

# Child:
(prinl (stamp) " -- (Pid " *Pid ") Client connected from " *Adr)

(in Sock
   (until (eof)                        # Echo lines
      (out Sock (prinl (line))) ) )

(prinl (stamp) " -- (Pid " *Pid ") Client disconnected")
(bye)                                  # Terminate child
```



## PureBasic


```Purebasic
NewMap RecData.s()
OpenWindow(0, 100, 200, 200, 100, "Echo Server", #PB_Window_SystemMenu | #PB_Window_MinimizeGadget )
InitNetwork()
CreateNetworkServer(1, 12321)

Repeat
   Event = NetworkServerEvent()
   ClientID = EventClient()
 
   If Event = #PB_NetworkEvent_Connect    ; When a new client has been connected...
      AddMapElement(RecData(), Str(ClientID))
      
   ElseIf Event = #PB_NetworkEvent_Data
      *Buffer = AllocateMemory(20000)
      count = ReceiveNetworkData(ClientID, *Buffer, 20000)
      For i = 1 To count
         RecData(Str(ClientID)) + Mid( PeekS(*Buffer, count), i , 1)
         If Right( RecData(Str(ClientID)), 2) = #CRLF$
            SendNetworkString (ClientID, RecData(Str(ClientID)))
            Debug  IPString(GetClientIP(ClientID)) + ":" + Str(GetClientPort(ClientID)) + "  " + RecData(Str(ClientID))
            RecData(Str(ClientID)) = ""   
         EndIf
      Next
      FreeMemory(*Buffer)
      
   ElseIf Event = #PB_NetworkEvent_Disconnect  ; When a client has closed the connection...
      DeleteMapElement(RecData(), Str(ClientID))
   EndIf

   Event = WaitWindowEvent(10)
Until Event = #PB_Event_CloseWindow
```



## Python

{{works with|Python|2.3 or above}}

```python
import SocketServer

HOST = "localhost"
PORT = 12321

# this server uses ThreadingMixIn - one thread per connection
# replace with ForkMixIn to spawn a new process per connection

class EchoServer(SocketServer.ThreadingMixIn, SocketServer.TCPServer):
    # no need to override anything - default behavior is just fine
    pass

class EchoRequestHandler(SocketServer.StreamRequestHandler):
    """
    Handles one connection to the client.
    """
    def handle(self):
        print "connection from %s" % self.client_address[0]
        while True:
            line = self.rfile.readline()
            if not line: break
            print "%s wrote: %s" % (self.client_address[0], line.rstrip())
            self.wfile.write(line)
        print "%s disconnected" % self.client_address[0]


# Create the server
server = EchoServer((HOST, PORT), EchoRequestHandler)

# Activate the server; this will keep running until you
# interrupt the program with Ctrl-C
print "server listening on %s:%s" % server.server_address
server.serve_forever()
```


{{works with|Python|3.5 or above}}

```python

#!/usr/bin/env python
# $ printf 'echo\r\n' | nc localhost 12321
# echo
import asyncio
import logging
import os

logger = logging.getLogger('echoserver')

async def echo_handler(reader, writer):
  address = writer.get_extra_info('peername')
  logger.debug('accept: %s', address)
  message = await reader.readline()
  writer.write(message)
  await writer.drain()
  writer.close()

if __name__ == '__main__':
  logging.basicConfig()
  logger.setLevel(logging.DEBUG)
  loop = asyncio.get_event_loop()
  factory = asyncio.start_server(
    echo_handler,
    os.environ.get('HOST'),
    os.environ.get('PORT', 12321)
  )
  server = loop.run_until_complete(factory)
  try:
    loop.run_forever()
  except KeyboardInterrupt:
    pass
  server.close()
  loop.run_until_complete(server.wait_closed())
  loop.close()

```


{{works with|Python|2 and 3}}
Using only the low-level socket and threading modules. Supports timing out inactive clients

```python

    #!usr/bin/env python
    import socket
    import threading

    HOST = 'localhost'
    PORT = 12321
    SOCKET_TIMEOUT = 30

    # This function handles reading data sent by a client, echoing it back
    # and closing the connection in case of timeout (30s) or "quit" command
    # This function is meant to be started in a separate thread
    # (one thread per client)
    def handle_echo(client_connection, client_address):
        client_connection.settimeout(SOCKET_TIMEOUT)
        try:
            while True:
                data = client_connection.recv(1024)
                # Close connection if "quit" received from client
                if data == b'quit\r\n' or data == b'quit\n':
                    print('{} disconnected'.format(client_address))
                    client_connection.shutdown(1)
                    client_connection.close()
                    break
                # Echo back to client
                elif data:
                    print('FROM {} : {}'.format(client_address,data))
                    client_connection.send(data)
        # Timeout and close connection after 30s of inactivity
        except socket.timeout:
            print('{} timed out'.format(client_address))
            client_connection.shutdown(1)
            client_connection.close()

    # This function opens a socket and listens on specified port. As soon as a
    # connection is received, it is transfered to another socket so that the main
    # socket is not blocked and can accept new clients.
    def listen(host, port):
        # Create the main socket (IPv4, TCP)
        connection = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        connection.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        connection.bind((host, port))
        # Listen for clients (max 10 clients in waiting)
        connection.listen(10)
        # Every time a client connects, allow a dedicated socket and a dedicated
        # thread to handle communication with that client without blocking others.
        # Once the new thread has taken over, wait for the next client.
        while True:
            current_connection, client_address = connection.accept()
            print('{} connected'.format(client_address))
            handler_thread = threading.Thread( \
                target = handle_echo, \
                args = (current_connection,client_address) \
            )
            # daemon makes sure all threads are killed if the main server process
            # gets killed
            handler_thread.daemon = True
            handler_thread.start()

    if __name__ == "__main__":
        try:
            listen(HOST, PORT)
        except KeyboardInterrupt:
            print('exiting')
            pass

```



## Racket


An example echo server from the front page of the Racket website:

```racket

#lang racket
(define listener (tcp-listen 12321))
(let echo-server ()
  (define-values [I O] (tcp-accept listener))
  (thread (λ() (copy-port I O) (close-output-port O)))
  (echo-server))

```



## REALbasic


This example uses the built-in ServerSocket class to handle multiple users.

```vb

Class EchoSocket
Inherits TCPSocket
  Sub DataAvailable()
    If Instr(Me.LookAhead, EndofLine.Windows) > 0 Then
      Dim data As String = Me.ReadAll
      Dim lines() As String = Split(data, EndofLine.Windows)
      For i As Integer = 0 To Ubound(lines)
        Me.Write(lines(i) + EndOfLine.Windows)
        Print(lines(i))
      Next
    End If
  End Sub
End Class

Class EchoServer
Inherits ServerSocket
  Function AddSocket() As TCPSocket
    Return New EchoSocket
  End Function
End Class

Class App
Inherits ConsoleApplication
  Function Run(args() As String) As Integer
    Listener = New EchoServer
    Listener.Port = 12321
    Listener.Listen()
    While True
      DoEvents() 'pump the event loop
    Wend
  End Function
  Private Listener As EchoServer
End Class

```



## REBOL


```rebol
server-port: open/lines tcp://:12321
forever [
    connection-port: first server-port
    until [
        wait connection-port
        error? try [insert connection-port first connection-port]
    ]
    close connection-port
]
close server-port
```



## Ruby


```ruby
require 'socket'
server = TCPServer.new(12321)

while (connection = server.accept)
  Thread.new(connection) do |conn|
    port, host = conn.peeraddr[1,2]
    client = "#{host}:#{port}"
    puts "#{client} is connected"
    begin
      loop do
        line = conn.readline
        puts "#{client} says: #{line}"
        conn.puts(line)
      end
    rescue EOFError
      conn.close
      puts "#{client} has disconnected"
    end
  end
end
```


Ruby 1.9.2 introduced an alternate method to create TCP server sockets. The <code>Socket.tcp_server_loop</code> method encapsulates the guts of the server into a block.
{{works with|Ruby|1.9.2}}

```ruby
require 'socket'

Socket.tcp_server_loop(12321) do |conn, addr|
  Thread.new do
    client = "#{addr.ip_address}:#{addr.ip_port}"
    puts "#{client} is connected"
    begin
      loop do
        line = conn.readline
        puts "#{client} says: #{line}"
        conn.puts(line)
      end
    rescue EOFError
      conn.close
      puts "#{client} has disconnected"
    end
  end
end
```



## Rust


```rust

use std::net::{TcpListener, TcpStream};
use std::io::{BufReader, BufRead, Write};
use std::thread;

fn main() {
    let listener = TcpListener::bind("127.0.0.1:12321").unwrap();
    println!("server is running on 127.0.0.1:12321 ...");
    
    for stream in listener.incoming() {
        let stream = stream.unwrap();
        thread::spawn(move || handle_client(stream));
    }
}

fn handle_client(stream: TcpStream) {
    let mut stream = BufReader::new(stream);
    loop {
        let mut buf = String::new();
        if stream.read_line(&mut buf).is_err() {
            break;
        }
        stream
            .get_ref()
            .write(buf.as_bytes())
            .unwrap();
    }
}

```



## Scala


```scala
import java.io.PrintWriter
import java.net.{ServerSocket, Socket}

import scala.io.Source

object EchoServer extends App {
  private val serverSocket = new ServerSocket(23)
  private var numConnections = 0

  class ClientHandler(clientSocket: Socket) extends Runnable {
    private val (connectionId, closeCmd) = ({numConnections += 1; numConnections}, ":exit")

    override def run(): Unit =
      new PrintWriter(clientSocket.getOutputStream, true) {
        println(s"Connection opened, close with entering '$closeCmd'.")
        Source.fromInputStream(clientSocket.getInputStream).getLines
          .takeWhile(!_.toLowerCase.startsWith(closeCmd))
          .foreach { line =>
            Console.println(s"Received on #$connectionId: $line")
            println(line)  // Echo
          }
        Console.println(s"Gracefully closing connection, #$connectionId")
        clientSocket.close()
    }

    println(s"Handling connection, $connectionId")
  }

  while (true) new Thread(new ClientHandler(serverSocket.accept())).start()
}
```



## Scheme

{{works with|Guile}}
Based on the [[Guile]] [http://www.gnu.org/software/guile/manual/html_node/Internet-Socket-Examples.html Internet Socket Server Example].

```scheme
; Needed in Guile for read-line
(use-modules (ice-9 rdelim))

; Variable used to hold child PID returned from forking
(define child #f)

; Start listening on port 12321 for connections from any address
(let ((s (socket PF_INET SOCK_STREAM 0)))
  (setsockopt s SOL_SOCKET SO_REUSEADDR 1)
  (bind s AF_INET INADDR_ANY 12321)
  (listen s 5) ; Queue size of 5

  (simple-format #t "Listening for clients in pid: ~S" (getpid))
  (newline)

; Wait for connections forever
  (while #t
    (let* ((client-connection (accept s))
        (client-details (cdr client-connection))
        (client (car client-connection)))
; Once something connects fork
      (set! child (primitive-fork))
      (if (zero? child)
      (begin
; Then have child fork to avoid zombie children (grandchildren aren't our responsibility)
        (set! child (primitive-fork))
        (if (zero? child)
          (begin
; Display some connection details
          (simple-format #t "Got new client connection: ~S" client-details)
          (newline)
          (simple-format #t "Client address: ~S"
            (gethostbyaddr (sockaddr:addr client-details)))
          (newline)
; Wait for input from client and then echo the input back forever (or until client quits)
          (do ((line (read-line client)(read-line client))) ((zero? 1))
            (display line client)(newline client))))
; Child exits after spawning grandchild.
        (primitive-exit))
; Parent waits for child to finish spawning grandchild
      (waitpid child)))))
```



## Seed7

The code below uses the library [http://seed7.sourceforge.net/libraries/listener.htm listener.s7i].
The function [http://seed7.sourceforge.net/libraries/listener.htm#waitForRequest%28inout_listener,inout_file,inout_file%29 waitForRequest] returns
requests from new and existing connections.


```Seed7
$ include "seed7_05.s7i";
  include "socket.s7i";
  include "listener.s7i";

const proc: main is func
  local
    var listener: aListener is listener.value;
    var file: existingConnection is STD_NULL;
    var file: newConnection is STD_NULL;
  begin
    aListener := openInetListener(12321);
    listen(aListener, 10);
    while TRUE do
      waitForRequest(aListener, existingConnection, newConnection);
      if existingConnection <> STD_NULL then
        if eof(existingConnection) then
          writeln("Close connection " <& numericAddress(address(existingConnection)) <&
                  " port " <& port(existingConnection));
          close(existingConnection);
        else
          write(existingConnection, gets(existingConnection, 1024));
        end if;
      end if;
      if newConnection <> STD_NULL then
        writeln("New connection " <& numericAddress(address(newConnection)) <&
                " port " <& port(newConnection));
      end if;
    end while;
  end func;
```



## Tcl

This code is single-threaded. It uses non-blocking I/O to perform the transfers, sitting on top of the event multiplexer system call (e.g., <code>select()</code> on Unix) to decide when to take new connections or service a particular socket. This makes this into a very lightweight echo server in terms of overall system resources.


```tcl
# How to handle an incoming new connection
proc acceptEcho {chan host port} {
    puts "opened connection from $host:$port"
    fconfigure $chan -blocking 0 -buffering line -translation crlf
    fileevent $chan readable [list echo $chan $host $port]
}

# How to handle an incoming message on a connection
proc echo {chan host port} {
    if {[gets $chan line] >= 0} {
        puts $chan $line
    } elseif {[eof $chan]} {
        close $chan
        puts "closed connection from $host:$port"
    }
    # Other conditions causing a short read need no action
}

# Make the server socket and wait for connections
socket -server acceptEcho -myaddr localhost 12321
vwait forever
```


### Alternative version

A more succinct version (though one harder to adapt to other kinds of services, but closer to the standard unix echo daemon since it has no line-buffering) is to use an asynchronous binary copy.

```tcl
# How to handle an incoming new connection 
proc acceptEcho {chan host port} {
    puts "opened connection from $host:$port"
    fconfigure $chan -translation binary -buffering none
    fcopy $chan $chan -command [list done $chan $host $port]
}

# Called to finalize the connection
proc done {chan host port args} {
    puts "closed connection from $host:$port"
    close $chan
}

# Make the server socket and wait for connections
socket -server acceptEcho -myaddr localhost 12321
vwait forever
```



## X86 Assembly


```x86asm


; x86_64 Linux NASM

global _start

%define af_inet 2
%define sock_stream 1
%define default_proto 0
%define sol_sock 1
%define reuse_addr 2
%define reuse_port 15
%define server_port 9001
%define addr_any 0
%define family_offset 0
%define port_offset 2
%define addr_offset 4
%define unused_offset 8
%define addr_len 16
%define buffer_len 64
%define max_connections 3


section .text

; rdi - 16 bit value to be byte swapped
; return - byte swapped value
htn_swap16:

  xor rax, rax
  mov rdx, 0x000000ff

  mov rsi, rdi
  and rsi, rdx
  shl rsi, 8
  or rax, rsi
  shl rdx, 8

  mov rsi, rdi
  and rsi, rdx
  shr rsi, 8
  or rax, rsi
  ret

; return - server socket
create_server_socket:

  mov rax, 41
  mov rdi, af_inet
  mov rsi, sock_stream
  mov rdx, default_proto
  syscall
  push rax

  mov rax, 54
  mov rdi, qword [rsp]
  mov rsi, sol_sock
  mov rdx, reuse_addr
  mov qword [rsp - 16], 1
  lea r10, [rsp - 16]
  mov r8, 4
  syscall

  mov rax, 54
  mov rdi, qword [rsp]
  mov rsi, sol_sock
  mov rdx, reuse_port
  mov qword [rsp - 16], 1
  lea r10, [rsp - 16]
  mov r8, 4
  syscall


  pop rax
  ret

; rdi - socket
; rsi - port
; rdx - connections
; return - void
bind_and_listen:

  push rdi
  push rdx

  mov rdi, rsi
  call htn_swap16

  lea rsi, [rsp - 16]
  mov word [rsi + family_offset], af_inet
  mov word [rsi + port_offset], ax
  mov dword [rsi + addr_offset], addr_any
  mov qword [rsi + unused_offset], 0

  mov rax, 49
  mov rdi, qword [rsp + 8]
  mov rdx, addr_len
  syscall

  mov rax, 50
  pop rsi
  pop rdi
  syscall
  ret

; rdi - server socket
; return - client socket
accept:

  mov rax, 43
  lea rsi, [rsp - 16]
  lea rdx, [rsp - 24]
  syscall
  ret

; rdi - client socket
; return - void
echo:

  push rdi
  mov rax, 0
  lea rsi, [rsp - 104]
  mov rdx, buffer_len
  syscall

  pop rdi
  mov rdx, rax 
  lea rsi, [rsp - 112]
  mov rax, 1
  syscall
  ret


_start:

  call create_server_socket
  mov r14, rax

  mov rdi, rax
  mov rsi, server_port
  mov rdx, max_connections
  call bind_and_listen

accept_connection:

  mov rdi, r14
  call accept

  mov r15, rax
  mov rax, 57
  syscall

  test rax, rax
  jz handle_connection

  ; close client socket
  mov rax, 3
  mov rdi, r15
  syscall
  jmp accept_connection
    
handle_connection:

  mov rdi, r15
  call echo

  close_client:
    mov rax, 3
    mov rdi, r15
    syscall

  close_server:
    mov rax, 3
    mov rdi, r14
    syscall

  exit:
    mov rax, 60
    xor rdi, rdi
    syscall

```



## zkl


```zkl
const PORT=12321;
pipe:=Thread.Pipe(); // how server tells thread to connect to user
 
fcn echo(socket){		// a thread, one per connection
   text:=Data();
   while(t:=socket.read()){
      text.append(t);
      if(text.find("\n",text.cursor)){ text.readln().print(); }
   }
   // socket was closed
}
 
   // Set up the server socket.
server:=Network.TCPServerSocket.open(PORT);
println("Listening on %s:%s".fmt(server.hostname,server.port));
server.listen(echo.launch);  // Main event loop
```

{{out}}
The next three windows overlap in time

```txt

//start the server
$ zkl bbb
Listening on Octavius:12321
hohothis is a test
a different terminal
^\Quit

```


```txt

//on another terminal, run the REPL
$ zkl
zkl: var s=Network.TCPClientSocket.connectTo("localhost",12321);
TCPClientSocket
zkl: s.write("hoho")
4
zkl: s.write("this is a test\n")
15
zkl: 

```


```txt

//and on a third terminal
$ zkl
zkl: var s=Network.TCPClientSocket.connectTo("localhost",12321);
TCPClientSocket
zkl: s.write("a different terminal\n")
21
zkl: 

```


{{omit from|ACL2}}
{{omit from|Lotus 123 Macro Scripting}}
{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have network access (just a serial port). -->
{{omit from|M4}}
{{omit from|Maxima}}
{{omit from|ML/I}}
{{omit from|PARI/GP}}
{{omit from|Retro|No concurrency support}}
{{omit from|SNUSP|No networking.}}
{{omit from|Unlambda|Does not have network access.}}
{{omit from|Commodore BASIC}}
