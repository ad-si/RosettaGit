+++
title = "Determine if only one instance is running"
description = ""
date = 2019-10-10T17:35:35Z
aliases = []
[extra]
id = 2241
[taxonomies]
categories = []
tags = []
+++

{{task|Programming environment operations}}

This task is to determine if there is only one instance of an application running. If the program discovers that an instance of it is already running, then it should display a message indicating that it is already running and exit.


## Ada


The following solution tries to open a file for reading. If the file does ''not'' exist, a 'Name_Error' is raised. The exception handler creates that file, allows the program to perform its task, and, eventually, makes sure the file is deleted. If no exception is raised, the file exists, so another instance is running, and the program stops. It also stops if the wrong exception is raised, i.e., any exception other than 'Name_Error'.


```Ada
with Ada.Text_IO;

procedure Single_Instance is

   package IO renames Ada.Text_IO;
   Lock_File: IO.File_Type;
   Lock_File_Name: String := "single_instance.magic_lock";

begin
   begin
      IO.Open(File => Lock_File, Mode=> IO.In_File, Name => Lock_File_Name);
      IO.Close(Lock_File);
      IO.Put_Line("I can't -- another instance of me is running ...");
   exception
      when IO.Name_Error =>
         IO.Put_Line("I can run!");
         IO.Create(File => Lock_File, Name => Lock_File_Name);
         for I in 1 .. 10 loop
            IO.Put(Integer'Image(I));
            delay 1.0; -- wait one second
         end loop;
         IO.Delete(Lock_File);
         IO.New_Line;
         IO.Put_Line("I am done!");
   end;
exception
   when others => IO.Delete(Lock_File);
end Single_Instance;
```


Note that there is a race condition: If another instance tries to open the file for reading, before the first one has created it, then more than one instance will actually run.


## AutoHotkey

AutoHotkey has a #SingleInstance command. If you run two scripts that don't have it at the same time, it alerts the user. #SingleInstance FORCE closes the older instance when a newer one is run, and #SingleInstance IGNORE does nothing when you try to open a new instance of an already-running script.


## BBC BASIC

{{works with|BBC BASIC for Windows}}
Change 'UniqueLockName' to something more likely to be unique, such as a GUID.

```bbcbasic
      SYS "CreateMutex", 0, 1, "UniqueLockName" TO Mutex%
      SYS "GetLastError" TO lerr%
      IF lerr% = 183 THEN
        SYS "CloseHandle", Mutex%
        SYS "MessageBox", @hwnd%, "I am already running", 0, 0
        QUIT
      ENDIF

      SYS "ReleaseMutex", Mutex%
      SYS "CloseHandle", Mutex%
      END
```



## C


###  POSIX with file lock

This solution opens <tt>~/rosetta-code-lock</tt> and uses ''fcntl()'' to set a write lock on the file. Only one instance can set this lock. If ''fcntl()'' fails, this program assumes that another instance is running. This program always clears its lock when it terminates.

The user might use an interrupt or other signal to terminate the program. If so, the lock file survives, but the system still clears the lock. The user can run the program again.

Note that the underlying file system needs to support file locking; for example if <tt>~/</tt> directory is on a NFS mounted partition, success in a locking <code>fcntl()</code> call is not always meaningful.

{{libheader|POSIX}}

```c
#include <fcntl.h> /* fcntl, open */
#include <stdlib.h>	/* atexit, getenv, malloc */
#include <stdio.h>	/* fputs, printf, puts, snprintf */
#include <string.h>	/* memcpy */
#include <unistd.h>	/* sleep, unlink */

/* Filename for only_one_instance() lock. */
#define INSTANCE_LOCK "rosetta-code-lock"

void
fail(const char *message)
{
	perror(message);
	exit(1);
}

/* Path to only_one_instance() lock. */
static char *ooi_path;

void
ooi_unlink(void)
{
	unlink(ooi_path);
}

/* Exit if another instance of this program is running. */
void
only_one_instance(void)
{
	struct flock fl;
	size_t dirlen;
	int fd;
	char *dir;

	/*
	 * Place the lock in the home directory of this user;
	 * therefore we only check for other instances by the same
	 * user (and the user can trick us by changing HOME).
	 */
	dir = getenv("HOME");
	if (dir == NULL || dir[0] != '/') {
		fputs("Bad home directory.\n", stderr);
		exit(1);
	}
	dirlen = strlen(dir);

	ooi_path = malloc(dirlen + sizeof("/" INSTANCE_LOCK));
	if (ooi_path == NULL)
		fail("malloc");
	memcpy(ooi_path, dir, dirlen);
	memcpy(ooi_path + dirlen, "/" INSTANCE_LOCK,
	    sizeof("/" INSTANCE_LOCK));  /* copies '\0' */

	fd = open(ooi_path, O_RDWR | O_CREAT, 0600);
	if (fd < 0)
		fail(ooi_path);

	fl.l_start = 0;
	fl.l_len = 0;
	fl.l_type = F_WRLCK;
	fl.l_whence = SEEK_SET;
	if (fcntl(fd, F_SETLK, &fl) < 0) {
		fputs("Another instance of this program is running.\n",
		    stderr);
		exit(1);
	}

	/*
	 * Run unlink(ooi_path) when the program exits. The program
	 * always releases locks when it exits.
	 */
	atexit(ooi_unlink);
}

/*
 * Demo for Rosetta Code.
 * http://rosettacode.org/wiki/Determine_if_only_one_instance_is_running
 */
int
main()
{
	int i;

	only_one_instance();

	/* Play for 10 seconds. */
	for(i = 10; i > 0; i--) {
		printf("%d...%s", i, i % 5 == 1 ? "\n" : " ");
		fflush(stdout);
		sleep(1);
	}
	puts("Fin!");
	return 0;
}
```



###  POSIX with file creation

This solution opens a file with <tt>O_CREAT|O_EXCL</tt>. If the file already exists, this program assumes that another instance is running. This solution is not as good as file locking, because the program might terminate without deleting the file.

The program, when terminating, must be sure to ''unlink()'' the file. This example has ''unlink()'' at two places: at the end of ''main()'', and at a SIGINT handler. If you interrupt the program, it will probably delete <tt>/tmp/MyUniqueName</tt>, but not if SIGINT wins a race before the program installs its handler. If you terminate the program with a different signal, then you will get stuck, because <tt>/tmp/MyUniqueName</tt> will still exist, preventing another execution of the program. One might add code to catch some other signals, but there is no way to catch SIGKILL!

This program uses a regular file, with ''open()'' and ''unlink()''. There is [http://rosettacode.org/mw/index.php?title=Determine_if_only_one_instance_is_running&oldid=97253 an older version that uses a semaphore], with ''sem_open()'' and ''sem_unlink()''. The switch from a semaphore to a regular file was easy, because the program never used the semaphore as a semaphore; it only checked the existence of a semaphore. If you get stuck, <tt>rm /tmp/MyUniqueName</tt> might be easier than deleting a semaphore.

{{libheader|POSIX}}

```c
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
/* unistd for sleep */

void sigint_handler(int sig)
{
   fprintf(stderr, "Caught signal %d.\n", sig);
   unlink("/tmp/MyUniqueName");
   /* exit() is not safe in a signal handler, use _exit() */
   _exit(1);
}

int main()
{
   struct sigaction act;
   int myfd;

   myfd = open("/tmp/MyUniqueName", O_CREAT|O_EXCL);
   if ( myfd < 0 )
   {
      fprintf(stderr, "I am already running!\n");
      exit(1);
   }
   act.sa_handler = sigint_handler;
   sigemptyset(&act.sa_mask);
   act.sa_flags = 0;
   sigaction(SIGINT, &act, NULL);
   /* here the real code of the app*/
   sleep(20);
   /* end of the app */
   unlink("/tmp/MyUniqueName"); close(myfd);
   return 0;
}
```



## C++


### Microsoft Windows

{{works with|Windows|2000 or later}}
This line needs to be near the top of the file (or in stdafx.h, if you use one.)

```cpp
#include <afx.h>
```


You need a variable of type HANDLE with the same lifetime as your program.  Perhaps as a member of your CWinApp object.

```cpp
HANDLE mutex;
```

At the earliest possible point in your program, you need to initialize it and perform your check. "MyApp" should be a string unique to your application.  See [http://msdn2.microsoft.com/en-us/library/ms682411.aspx here] for full details.

```cpp
mutex = CreateMutex( NULL, TRUE, "MyApp" );
if ( GetLastError() == ERROR_ALREADY_EXISTS )
{
     // There's another instance running.  What do you do?
}
```


Finally, near the end of your program, you need to close the mutex.

```cpp
CloseHandle( mutex );
```


## C#

### Using a TCP Port



```c#
using System;
using System.Net;
using System.Net.Sockets;

class Program {
    static void Main(string[] args) {
        try {
            TcpListener server = new TcpListener(IPAddress.Any, 12345);
            server.Start();
        }

        catch (SocketException e) {
            if (e.SocketErrorCode == SocketError.AddressAlreadyInUse) {
                Console.Error.WriteLine("Already running.");
            }
        }
    }
}
```



### Using a mutex


```c#

// Use this class in your process to guard against multiple instances
//
// This is valid for C# running on Windows, but not for C# with Linux.
//
using System;
using System.Threading;

/// <summary>
/// RunOnce should be instantiated in the calling processes main clause
/// (preferably using a "using" clause) and then calling process
/// should then check AlreadyRunning and do whatever is appropriate
/// </summary>
public class RunOnce : IDisposable
{
	public RunOnce( string name )
	{
		m_name = name;
		AlreadyRunning = false;

		bool created_new = false;

		m_mutex = new Mutex( false, m_name, out created_new );

		AlreadyRunning = !created_new;
	}

	~RunOnce()
	{
		DisposeImpl( false );
	}

	public bool AlreadyRunning
	{
		get { return m_already_running; }
		private set { m_already_running = value; }
	}

	private void DisposeImpl( bool is_disposing )
	{
		GC.SuppressFinalize( this );

		if( is_disposing )
		{
			m_mutex.Close();
		}
	}

	#region IDisposable Members

	public void Dispose()
	{
		DisposeImpl( true );
	}

	#endregion

	private string m_name;
	private bool m_already_running;
	private Mutex m_mutex;
}

class Program
{
    // Example code to use this
    static void Main( string[] args )
    {
        using ( RunOnce ro = new RunOnce( "App Name" ) )
        {
            if ( ro.AlreadyRunning )
            {
                Console.WriteLine( "Already running" );
                return;
            }

            // Program logic
        }
    }
}
```



## Clojure

{{trans|Java}}

```clojure
(import (java.net ServerSocket InetAddress))

(def *port* 12345) ; random large port number
(try (new ServerSocket *port* 10 (. InetAddress getLocalHost))
     (catch IOException e (System/exit 0))) ; port taken, so app is already running
```


## Delphi


```Delphi
program OneInstance;

{$APPTYPE CONSOLE}

uses SysUtils, Windows;

var
  FMutex: THandle;
begin
  FMutex := CreateMutex(nil, True, 'OneInstanceMutex');
  if FMutex = 0 then
    RaiseLastOSError
  else
  begin
    try
      if GetLastError = ERROR_ALREADY_EXISTS then
        Writeln('Program already running.  Closing...')
      else
      begin
        // do stuff ...
        Readln;
      end;
    finally
      CloseHandle(FMutex);
    end;
  end;
end.
```


## D


### Unix Domain Socket

Unix domain sockets support another addressing mode, via the so-called abstract namespace. This allows us to bind sockets to names rather than to files. What we get are the following benefits (see page 1175 in The Linux Programming Interface):
* There is no need to worry about possible collisions with existing files in the filesystem.
* There is no socket file to be removed upon program termination.
* We do not need to create a file for the socket at all. This obviates target directory existence, permissions checks, and reduces filesystem clutter. Also, it works in chrooted environments.

All we need is to generate a unique name for our program and pass it as the address when calling bind(). The trick is that instead of specifying a file path as the address, we pass a null byte followed by the name of our choosing (e.g. "\0my-unique-name"). The initial null byte is what distinguishes abstract socket names from conventional Unix domain socket path names, which consist of a string of one or more non-null bytes terminated by a null byte.
Read more here: [https://blog.petrzemek.net/2017/07/24/ensuring-that-a-linux-program-is-running-at-most-once-by-using-abstract-sockets/]

```d
import std.socket;

bool is_unique_instance() {
    auto socket = new Socket(AddressFamily.UNIX, SocketType.STREAM);
    auto addr = new UnixAddress("\0/tmp/myapp.uniqueness.sock");
    try {
        socket.bind(addr);
        return true;
    } catch (SocketOSException e) {
        return false;
    }
}

```


## Erlang

From the Erlang shell, or in a program, register the application process. If this works, the process is the only one.
{{out}}

```txt

7> erlang:register( aname, erlang:self() ).
true
8> erlang:register( aname, erlang:self() ).
** exception error: bad argument
     in function  register/2
        called as register(aname,<0.42.0>)

```



## Go


### Port

Recommended over file based solutions.  It has the advantage that the port is always released
when the process ends.

```go
package main

import (
    "fmt"
    "net"
    "time"
)

const lNet = "tcp"
const lAddr = ":12345"

func main() {
    if _, err := net.Listen(lNet, lAddr); err != nil {
        fmt.Println("an instance was already running")
        return
    }
    fmt.Println("single instance started")
    time.Sleep(10 * time.Second)
}
```


### File

Solution using O_CREATE|O_EXCL.  This solution has the problem that if anything terminates the
program early, the lock file remains.

```go
package main

import (
    "fmt"
    "os"
    "time"
)

// The path to the lock file should be an absolute path starting from the root.
// (If you wish to prevent the same program running in different directories,
// that is.)
const lfn = "/tmp/rclock"

func main() {
    lf, err := os.OpenFile(lfn, os.O_RDWR|os.O_CREATE|os.O_EXCL, 0666)
    if err != nil {
        fmt.Println("an instance is already running")
        return
    }
    lf.Close()
    fmt.Println("single instance started")
    time.Sleep(10 * time.Second)
    os.Remove(lfn)
}
```

Here's a fluffier version that stores the PID in the lock file to provide better messages.
It has the same problem of the lock file remaining if anything terminates the program early.

```go
package main

import (
    "fmt"
    "os"
    "strconv"
    "strings"
    "time"
)

// The path to the lock file should be an absolute path starting from the root.
// (If you wish to prevent the same program running in different directories, that is.)
const lfn = "/tmp/rclock"

func main() {
    lf, err := os.OpenFile(lfn, os.O_RDWR|os.O_CREATE|os.O_EXCL, 0666)
    if err == nil {
        // good
        // 10 digit pid seems to be a standard for lock files
        fmt.Fprintf(lf, "%10d", os.Getpid())
        lf.Close()
        defer os.Remove(lfn)
    } else {
        // problem
        fmt.Println(err)
        // dig deeper
        lf, err = os.Open(lfn)
        if err != nil {
            return
        }
        defer lf.Close()
        fmt.Println("inspecting lock file...")
        b10 := make([]byte, 10)
        _, err = lf.Read(b10)
        if err != nil {
            fmt.Println(err)
            return
        }
        pid, err := strconv.Atoi(strings.TrimSpace(string(b10)))
        if err != nil {
            fmt.Println(err)
            return
        }
        fmt.Println("lock file created by pid", pid)
        return
    }
    fmt.Println(os.Getpid(), "running...")
    time.Sleep(1e10)
}
```



## Haskell

Simple implementation using a lock file. Two threads are launched, but the second cannot start because the first has created a lock file which is deleted when it has finished.

```Haskell
import Control.Concurrent
import System.Directory (doesFileExist, getAppUserDataDirectory,
    removeFile)
import System.IO (withFile, Handle, IOMode(WriteMode), hPutStr)

oneInstance :: IO ()
oneInstance = do
    -- check if file "$HOME/.myapp.lock" exists
    user <- getAppUserDataDirectory "myapp.lock"
    locked <- doesFileExist user
    if locked
    then print "There is already one instance of this program running."
    else do
        t <- myThreadId
        -- this is the entry point to the main program:
        -- withFile creates a file, then calls a function,
        -- then closes the file
        withFile user WriteMode (do_program t)
        -- remove the lock when we're done
        removeFile user

do_program :: ThreadId -> Handle -> IO ()
do_program t h = do
    let s = "Locked by thread: " ++ show t
    -- print what thread has acquired the lock
    putStrLn s
    -- write the same message to the file, to show that the
    -- thread "owns" the file
    hPutStr h s
    -- wait for one second
    threadDelay 1000000

main :: IO ()
main = do
    -- launch the first thread, which will create the lock file
    forkIO oneInstance
    -- wait for half a second
    threadDelay 500000
    -- launch the second thread, which will find the lock file and
    -- thus will exit immediately
    forkIO oneInstance
    return ()
```


=={{header|Icon}} and {{header|Unicon}}==

The following only works in Unicon.  The program uses a socket as a flag.


```unicon
procedure main(A)
   if not open(":"||54321,"na") then stop("Already running")
   repeat {}	# busy loop
end
```


Sample run:


```txt

->self &
[1] 15358
->self
Already running
->

```



## Java


```java
import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.UnknownHostException;

public class SingletonApp
{
    private static final int PORT = 65000;  // random large port number
    private static ServerSocket s;

    // static initializer
    static {
        try {
            s = new ServerSocket(PORT, 10, InetAddress.getLocalHost());
        } catch (UnknownHostException e) {
            // shouldn't happen for localhost
        } catch (IOException e) {
            // port taken, so app is already running
            System.out.print("Application is already running,");
            System.out.println(" so terminating this instance.");
            System.exit(0);
        }
    }

    public static void main(String[] args) {
        System.out.print("OK, only this instance is running");
        System.out.println(" but will terminate in 10 seconds.");
        try {
            Thread.sleep(10000);
            if (s != null && !s.isClosed()) s.close();
        } catch (Exception e) {
            System.err.println(e);
        }
    }
}
```



## Jsish

Using a socket on a fixed port number.

```javascript
/* Determine if only one instance, in Jsish */
var sock;

try {
    sock = new Socket({client:false, port:54321});
    puts('\nApplication running for 30 seconds, from', strftime());
    update(30000);
    puts('\nApplication ended at', strftime());
} catch (err) {
    puts('Applicaion already running');
    exit(1);
}
```


{{out}}

```txt
prompt$ jsish oneInstance.jsi &
[1] 2003
prompt$
Application running for 30 seconds, from 2019-06-14 11:19:37

prompt$ jsish oneInstance.jsi
Applicaion already running
prompt$ jsish oneInstance.jsi
Applicaion already running
prompt$
Application ended at 2019-06-14 11:20:06

[1]+  Done                    jsish oneInstance.jsi
```



## Julia

{{trans|Java}}
```julia

using Sockets

const portnum = 12345

function canopen()
    try
        server = listen(portnum)
        println("This is the only instance.")
        sleep(20)
    catch y
        if findfirst("EADDRINUSE", string(y)) != nothing
            println("There is already an instance running.")
        end
    end
end

canopen()

```




## Kotlin


```scala
// version 1.0.6

import java.io.IOException
import java.net.*

object SingleInstance {
    private var ss: ServerSocket? = null

    fun alreadyRunning(): Boolean {
        try {
            ss = ServerSocket(65000, 10, InetAddress.getLocalHost()) // using private port 65000
        }
        catch (e: IOException) {
            // port already in use so an instance is already running
            return true
        }
        return false
    }

    fun close() {
        if (ss == null || ss?.isClosed() == true) return
        ss?.close()
    }
}

fun main(args: Array<String>) {
    if (SingleInstance.alreadyRunning()) {
        println("Application is already running, so terminating this instance")
        System.exit(0)
    }
    else {
        println("OK, only this instance is running but will terminate in 10 seconds")
        Thread.sleep(10000)
        SingleInstance.close()
    }
}
```


{{out}}
First output window:

```txt

OK, only this instance is running but will terminate in 10 seconds

```


{{out}}
Second output window (second instance started within 10 seconds of first):

```txt

Application is already running, so terminating this instance

```



## Lasso


```Lasso
#!/usr/bin/lasso9

local(lockfile = file('/tmp/myprocess.lockfile'))

if(#lockfile -> exists) => {
	stdoutnl('Error: App is running as of ' + #lockfile -> readstring)
	abort
}

handle => {
	#lockfile -> delete
}

stdoutnl('Starting execution')

#lockfile -> doWithClose => {
	#lockfile -> writebytes(bytes(date))
}

sleep(10000)

stdoutnl('Execution done')
```


Output Window 1:

```txt
$./rosetta
Starting execution
Execution done

```


Output Window 2:

```txt
$./rosetta
Error: App is running as of 2013-11-27 08:42:45

```



## Liberty BASIC


```lb
'Create a Mutex to prevent more than one instance from being open at a single time.
CallDLL #kernel32, "CreateMutexA", 0 as Long, 1 as Long, "Global\My Program" as ptr, mutex as ulong
CallDLL #kernel32, "GetLastError", LastError as Long

if LastError = 183 then 'Error returned when a Mutex already exists
    'Close the handle if the mutex already exists
    calldll #kernel32, "CloseHandle", mutex as ulong, ret as ulong
    notice "An instance of My Program is currently running!"
    end
end if

'Release the Mutex/ Close the handle prior to ending the program
'Comment out these lines to allow the program to remain active to test for the mutex's presence
calldll #kernel32, "ReleaseMutex", mutex as ulong, ret as ulong
calldll #kernel32, "CloseHandle", mutex as ulong, ret as ulong
end
```



## M2000 Interpreter

We can lock a file in user folder. Only one instance can lock a file.


```M2000 Interpreter

Module Checkit {
      Try {
            Open "MYLOCK" For Output Exclusive As #F
            Print "DO SOMETHING"
            A$=Key$
            Close#f
      }
}

```



=={{header|Mathematica}} / {{header|Wolfram Language}}==
$Epilog is the action performed upon session exit.
Running the following code before any other code will prevent 2 instances from concurrent execution.

```Mathematica
$Epilog := Print["Another instance is running "];
If[Attributes[Global`Mutex] == {Protected},
 Exit[],
 Global`Mutex[x_] := Locked; Protect[Global`Mutex];
 ]
```



## Nim


```nim
import os, posix

let fn = getHomeDir() & "rosetta-code-lock"
proc ooiUnlink {.noconv.} = discard unlink fn

proc onlyOneInstance =
  var fl = TFlock(lType: F_WRLCK.cshort, lWhence: SEEK_SET.cshort)
  var fd = getFileHandle fn.open fmReadWrite
  if fcntl(fd, F_SETLK, addr fl) < 0:
    stderr.writeln "Another instance of this program is running"
    quit 1
  addQuitProc ooiUnlink

onlyOneInstance()

for i in countdown(10, 1):
  echo i
  sleep 1000
echo "Fin!"
```



## OCaml

Replicates the '''C''' example, with the library [http://ocaml-sem.sourceforge.net/ ocaml-sem].

```ocaml
open Sem

let () =
  let oflags = [Unix.O_CREAT;
                Unix.O_EXCL] in
  let sem = sem_open "MyUniqueName" ~oflags () in
  (* here the real code of the app *)
  Unix.sleep 20;
  (* end of the app *)
  sem_unlink "MyUniqueName";
  sem_close sem
```


The standard library of OCaml also provides a [http://caml.inria.fr/pub/docs/manual-ocaml/libref/Mutex.html Mutex] module.


## Oz


```oz
functor
import Application Open System
define
   fun {IsAlreadyRunning}
      try
	 S = {New Open.socket init}
      in
	 {S bind(takePort:12345)}
	 false
      catch system(os(os "bind" ...) ...) then
	 true
      end
   end

   if {IsAlreadyRunning} then
      {System.showInfo "Exiting because already running."}
      {Application.exit 1}
   end
   {System.showInfo "Press enter to exit."}
   {{New Open.file init(name:stdin)} read(list:_ size:1)}
   {Application.exit 0}
end
```



## Perl

The INIT block is runned just before the Perl runtime begins execution. See [http://perldoc.perl.org/perlmod.html perlmod]

Then it tries to get a lock to its own file, from where the script was called.

```perl
use Fcntl ':flock';

INIT
{
	die "Not able to open $0\n" unless (open ME, $0);
	die "I'm already running !!\n" unless(flock ME, LOCK_EX|LOCK_NB);
}

sleep 60; # then your code goes here
```


## Perl 6

{{works with|rakudo|2018.03}}
An old-school Unix solution, none the worse for the wear:

```perl6
my $name = $*PROGRAM-NAME;
my $pid = $*PID;

my $lockdir = "/tmp";
my $lockfile = "$lockdir/$name.pid";
my $lockpid = "$lockfile$pid";
my $havelock = False;

END {
    unlink $lockfile if $havelock;
    try unlink $lockpid;
}

my $pidfile = open "$lockpid", :w orelse .die;
$pidfile.say($pid);
$pidfile.close;

if try link($lockpid, $lockfile) {
    $havelock = True;
}
else {
    shell "kill -CONT `cat $lockfile` || rm $lockfile";
    if try link($lockfile, $lockpid) {
        $havelock = True;
    }
    else {
        die "You can't run right now!";
    }
}
note "Got lock!";
unlink $lockpid;
```



## Phix

{{libheader|pGUI}}

```Phix
--
-- demo\rosetta\Single_instance.exw
--
include pGUI.e

function copydata_cb(Ihandle /*ih*/, atom pCommandLine, integer size)
    -- (the first instance is sent a copy of the second one's command line)
    printf(1,"COPYDATA(%s, %d)\n",{peek_string(pCommandLine), size});
    return IUP_DEFAULT;
end function

function esc_close(Ihandle /*ih*/, atom c)
    return iff(c=K_ESC?IUP_CLOSE:IUP_CONTINUE)
end function

IupOpen()
IupSetGlobal("SINGLEINSTANCE", "Single") -- (must [partially] match the main window title)
if IupGetGlobal("SINGLEINSTANCE")!="" then
    Ihandle dlg = IupDialog(IupVbox({IupLabel("hello")},"MARGIN=200x200"))
    IupSetAttribute(dlg,"TITLE","Single Instance")
    IupSetCallback(dlg, "K_ANY", Icallback("esc_close"))
    IupSetCallback(dlg, "COPYDATA_CB", Icallback("copydata_cb"));
    IupShow(dlg)
    IupMainLoop()
end if
IupClose()
```



## PicoLisp

===Calling 'killall'===
One possibility is to send a zero-signal with 'killall', and check the return
value. This is useful if each application is started by a hash-bang script (the
first line is e.g. "#!/usr/bin/picolisp /usr/lib/picolisp/lib.l"). In that way,
each application has its own name which can be passed to 'killall'.

```txt
$ cat myScript
#!/usr/bin/picolisp /usr/lib/picolisp/lib.l

(wait 120000)
(bye)
```


```txt
$ ./myScript &  # Start in the background
[1] 26438
```


```PicoLisp
$ pil +
: (call "killall" "-0" "-q" "myScript")
-> T
```



### Using a mutex

Another possibility is to 'acquire' a mutex on program start, and never release
it.

```PicoLisp
: (acquire "running1")
-> 30817  # A successful call returns the PID
```

A second application trying to acquire the same mutex would receive 'NIL'


## PowerShell


```PowerShell

if (Get-Process -Name "notepad" -ErrorAction SilentlyContinue)
{
    Write-Warning -Message "notepad is already running."
}
else
{
    Start-Process -FilePath C:\Windows\notepad.exe
}

```

No output because notepad.exe was not running, so it was started.
{{Out}}

```txt


```

Run it again.

```PowerShell

if (Get-Process -Name "notepad" -ErrorAction SilentlyContinue)
{
    Write-Warning -Message "notepad is already running."
}
else
{
    Start-Process -FilePath C:\Windows\notepad.exe
}

```

Since it is running a warning message is output.
{{Out}}

```txt

WARNING: notepad is already running

```



## PureBasic


```PureBasic
#MyApp="MyLittleApp"
Mutex=CreateMutex_(0,1,#MyApp)
If GetLastError_()=#ERROR_ALREADY_EXISTS
  MessageRequester(#MyApp,"One instance is already started.")
  End
EndIf

; Main code executes here

ReleaseMutex_(Mutex)
End
```



## Python

===Linux (including cygwin) and Mac OSX Leopard===
{{works with|Python|2.6}}

Must be run from an application, not the interpreter.


```python
import __main__, os

def isOnlyInstance():
    # Determine if there are more than the current instance of the application
    # running at the current time.
    return os.system("(( $(ps -ef | grep python | grep '[" +
                     __main__.__file__[0] + "]" + __main__.__file__[1:] +
                     "' | wc -l) > 1 ))") != 0
```


This is not a solution - one can run the same app by copying the code to another location. A solution may be a lock file or lock directory created by the first instance and hold while the first instance is running.


## Racket

{{trans|Java}}

```racket

#lang racket
(define *port* 12345) ; random large port number
(define listener-handler
  (with-handlers ([exn? (λ(e) (printf "Already running, bye.\n") (exit))])
    (tcp-listen *port*)))
(printf "Working...\n")
(sleep 10)

```



## REXX

{{works with|ARexx}}
Solutions using a temporary file as a semaphore aren't very clean; if the program ends abruptly, the file isn't cleaned up. In this solution, we will instead open an ARexx port of our own. Ports are automatically closed by the interpreter if the program is ABENDed.

```rexx
/* Simple ARexx program to open a port after checking if it's already open */
IF Show('PORTS','ROSETTA') THEN DO           /* Port is already open; exit */
   SAY 'This program may only be run in a single instance at a time.'
   EXIT 5                                    /* Exit with a mild warning   */
   END
                 /* Open rexxsupport.library so that ports can be opened   */
IF ~Show('LIBRARIES','rexxsupport.library')
   THEN CALL AddLib('rexxsupport.library',0,-30,0)

IF ~OpenPort('ROSETTA')    THEN EXIT 10       /* Open port, end if it fails */

SAY 'Program is now running.'

DO FOREVER                                    /* Busyloop                   */
   /* Program stuff here */
   END

EXIT 0
```



## Ring


```ring

# Project : Determine if only one instance is running

task = "ringw.exe"
taskname = "tasklist.txt"
remove(taskname)
system("tasklist >> tasklist.txt")
fp = fopen(taskname,"r")
tasks = read("tasklist.txt")
counttask = count(tasks,task)
if counttask > 0
   see task + " running in " + counttask + " instances" + nl
else
   see task + " is not running" + nl
ok

func count(cString,dString)
     sum = 0
     while substr(cString,dString) > 0
           sum++
           cString = substr(cString,substr(cString,dString)+len(string(sum)))
     end
     return sum

```

Output:

```txt

ringw.exe running in 2 instances

```



## Ruby

Uses file locking on the program file

```ruby
def main
  puts "first instance"
  sleep 20
  puts :done
end

if $0 == __FILE__
  if File.new(__FILE__).flock(File::LOCK_EX | File::LOCK_NB)
    main
  else
    raise "another instance of this program is running"
  end
end

__END__
```



## Run BASIC


```runbasic
if instr(shell$("tasklist"),"rbp.exe") <> 0 then print "Task is Running"
```



## Rust

Using TCP socket

```rust
use std::net::TcpListener;

fn create_app_lock(port: u16) -> TcpListener {
    match TcpListener::bind(("0.0.0.0", port)) {
        Ok(socket) => {
            socket
        },
        Err(_) => {
            panic!("Couldn't lock port {}: another instance already running?", port);
        }
    }
}

fn remove_app_lock(socket: TcpListener) {
    drop(socket);
}

fn main() {
    let lock_socket = create_app_lock(12345);
    // ...
    // your code here
    // ...
    remove_app_lock(lock_socket);
}
```



## Scala


### Java Interoperability

{{Out}}Best seen running in your browser [https://scastie.scala-lang.org/ja0sMzt5SGKHSu3w8qnlQQ Scastie (remote JVM)].

```Scala
import java.io.IOException
import java.net.{InetAddress, ServerSocket}

object SingletonApp extends App {
  private val port = 65000

  try {
    val s = new ServerSocket(port, 10, InetAddress.getLocalHost)
  }
  catch {
    case _: IOException =>
      // port taken, so app is already running
      println("Application is already running, so terminating this instance.")
      sys.exit(-1)
  }

  println("OK, only this instance is running but will terminate in 10 seconds.")

  Thread.sleep(10000)

  sys.exit(0)

}
```


## Sidef


```ruby
# For this to work, you need to explicitly
# store the returned fh inside a variable.
var fh = File(__FILE__).open_r

# Now call the flock() method on it
fh.flock(File.LOCK_EX | File.LOCK_NB) ->
    || die "I'm already running!"

# Your code here...
say "Running..."
Sys.sleep(20)
say 'Done!'
```



## Swift

Uses NSDistributedNotificationCenter. Works with Swift 1.2.

```Swift
import Foundation

let globalCenter = NSDistributedNotificationCenter.defaultCenter()
let time = NSDate().timeIntervalSince1970

globalCenter.addObserverForName("OnlyOne", object: nil, queue: NSOperationQueue.mainQueue()) {not in
    if let senderTime = not.userInfo?["time"] as? NSTimeInterval where senderTime != time {
        println("More than one running")
        exit(0)
    } else {
        println("Only one")
    }
}

func send() {
    globalCenter.postNotificationName("OnlyOne", object: nil, userInfo: ["time": time])

    let waitTime = dispatch_time(DISPATCH_TIME_NOW, Int64(3 * NSEC_PER_SEC))

    dispatch_after(waitTime, dispatch_get_main_queue()) {
        send()
    }
}

send()
CFRunLoopRun()
```



## Tcl

{{trans|Java}}
{{works with|Tcl|8.6}}

```Tcl
package require Tcl 8.6
try {
    # Pick a port number based on the name of the main script executing
    socket -server {apply {{chan args} {close $chan}}} -myaddr localhost \
            [expr {1024 + [zlib crc32 [file normalize $::argv0]] % 30000}]
} trap {POSIX EADDRINUSE} {} {
    # Generate a nice error message
    puts stderr "Application $::argv0 already running?"
    exit 1
}
```



## TXR



### = Microsoft Windows =



```txrlisp
;;; Define some typedefs for clear correspondence with Win32
(typedef HANDLE cptr)
(typedef LPSECURITY_ATTRIBUTES cptr)
(typedef WINERR (enum WINERR ERROR_SUCCESS
                             (ERROR_ALREADY_EXISTS 183)))
(typedef BOOL (enum BOOL FALSE TRUE))
(typedef LPCWSTR wstr)

;;; More familiar spelling for null pointer.
(defvarl NULL cptr-null)

;;; Define access to foreign functions.
(with-dyn-lib "kernel32.dll"
  (deffi CreateMutex "CreateMutexW" HANDLE (LPSECURITY_ATTRIBUTES BOOL LPCWSTR))
  (deffi CloseHandle "CloseHandle" BOOL (HANDLE))
  (deffi GetLastError "GetLastError" WINERR ()))

;;; Now, the single-instance program:
(defvar m (CreateMutex NULL 'TRUE "ApplicationName"))

(unless (eq (GetLastError) 'ERROR_ALREADY_EXISTS)
  ;; mutual exclusion here
  )

(CloseHandle m)
```



## UNIX Shell


{{works with|Bourne Shell}}
{{works with|Bourne Again SHell}}

```sh

# (c) Copyright 2005 Mark Hobley
#
# This is free software. This file can be redistributed or modified
# under the terms of version 1.2 of the GNU Free Documentation Licence
# as published by the Free Software Foundation.
#

 singleinstance ()
 {
   if [ -d $SRUNDIR ] ; then
     if [ -w $SRUNDIR ] ; then
       if [ -d $SRUNDIR/$APPNAME ] ; then
         echo "Process Already Running" >& 2
         return 221
       else
         mkdir $SRUNDIR/$APPNAME
         if [ "$?" -ne 0 ] ; then
           if [ -d $SRUNDIR/$APPNAME ] ; then
             echo "Process Already Running" >& 2
             return 221
           else
             echo "Unexpected Error" >& 2
             return 239
           fi
         fi
         return 0 ; # This is a unique instance
       fi
     else
       echo "Permission Denied" >& 2
       return 210
     fi
   else
     echo "Missing Directory" >& 2
     return 199
   fi
 }

```



## Visual Basic

{{works with|Visual Basic|4}} <!-- previous versions possible but unlikely; no way to test right now -->
{{works with|Visual Basic|5}}
{{works with|Visual Basic|6}}

```vb
Dim onlyInstance as Boolean
onlyInstance = not App.PrevInstance
```


{{omit from|ACL2}}
{{omit from|GUISS|Only the operator call tell if there is more than one instance running}}
{{omit from|HTML}}
{{omit from|Lotus 123 Macro Scripting}}
{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have concurrency or background processes. -->
{{omit from|M4}}
{{omit from|PARI/GP}}
{{omit from|Retro}}
{{omit from|Inform 7|Does not have concurrency.}}
{{omit from|ZX Spectrum Basic|Does not have concurrency.}}

[[Category:Concurrency]]
