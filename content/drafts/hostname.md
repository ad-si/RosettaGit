+++
title = "Hostname"
description = ""
date = 2019-10-20T02:12:28Z
aliases = []
[extra]
id = 1909
[taxonomies]
categories = []
tags = []
+++

{{task|Programming environment operations}}
[[Category:Networking and Web Interaction]]

;Task:
Find the name of the host on which the routine is running.





## Ada

Works with GCC/GNAT

```ada
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Sockets;

procedure Demo is
begin
   Put_Line (GNAT.Sockets.Host_Name);
end Demo;
```



## ALGOL 68

<!-- {{may not works with|ALGOL 68|Standard - extensions to libraries used}} -->
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}

<!-- {{does not works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386 - No such library function.}} -->
{{works with|POSIX|.1}}

```algol68
STRING hostname;
get(read OF execve child pipe("/bin/hostname","hostname",""), hostname);
print(("hostname: ", hostname, new line))
```



## Aikido


```aikido

println (System.hostname)

```



## AppleScript


```applescript

host name of (system info)

```



## AutoHotkey


```AutoHotkey
MsgBox % A_ComputerName
```


via Windows Management Instrumentation (WMI)

```AutoHotkey
for objItem in ComObjGet("winmgmts:\\.\root\CIMV2").ExecQuery("SELECT * FROM Win32_ComputerSystem")
    MsgBox, % "Hostname:`t" objItem.Name
```



## Arc


```Arc
(system "hostname -f")
```



## AWK


{{noticebox||WARNING: the following purported solution makes an assumption about environment variables that may not be applicable in all circumstances.}}

```awk
$ awk 'BEGIN{print ENVIRON["HOST"]}'
E51A08ZD
```



## BaCon


```freebasic
PRINT "Hostname: ", HOSTNAME$
```



## Batch File

Since Windows 2000 :

```dos>Hostname</lang



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      INSTALL @lib$+"SOCKLIB"
      PROC_initsockets
      PRINT "hostname: " FN_gethostname
      PROC_exitsockets
```


=={{header|C}}/{{header|C++}}==
{{works with|gcc|4.0.1}}

{{works with|POSIX|.1}}

```cpp
#include <iostream>
#include <stdio.h>
#include <limits.h>
#include <unistd.h>

int main(void)
{
 char name[_POSIX_HOST_NAME_MAX + 1];
 return gethostname(name, sizeof name) == -1 || printf("%s\n", name) < 0 ? EXIT_FAILURE : EXIT_SUCCESS;
}
```


=={{header|C sharp|C#}}==

```csharp
System.Net.Dns.GetHostName();
```


=={{header|Caché ObjectScript}}==

```txt
Write ##class(%SYS.System).GetNodeName()
```



## Clojure



```clojure

(.. java.net.InetAddress getLocalHost getHostName)

```



```shell

java -cp clojure.jar clojure.main -e "(.. java.net.InetAddress getLocalHost getHostName)"

```



## COBOL


```cobol
       identification division.
       program-id. hostname.

       data division.
       working-storage section.
       01 hostname pic x(256).
       01 nullpos  pic 999 value 1.

       procedure division.
       call "gethostname" using hostname by value length of hostname
       string hostname delimited by low-value into hostname
           with pointer nullpos
       display "Host: " hostname(1 : nullpos - 1)
       goback.
       end program hostname.

```



## CoffeeScript


```coffeescript

os = require 'os'
console.log os.hostname()

```



## Common Lisp

Another operating system feature that is implemented differently across lisp implementations. Here we show how to create a function that obtains the required result portably by working differently for each supported implementation. This technique is heavily used to make portable lisp libraries.

```lisp
(defun get-host-name ()
    #+(or sbcl ccl) (machine-instance)
    #+clisp (let ((s (machine-instance))) (subseq s 0 (position #\Space s)))
    #-(or sbcl ccl clisp) (error "get-host-name not implemented"))
```


{{libheader|CFFI}}

Another way is to use the [[FFI]] to access POSIX' <code>gethostname(2)</code>:


```lisp
(cffi:defcfun ("gethostname" c-gethostname) :int
  (buf :pointer) (len :unsigned-long))

(defun get-hostname ()
  (cffi:with-foreign-object (buf :char 256)
    (unless (zerop (c-gethostname buf 256))
      (error "Can't get hostname"))
    (values (cffi:foreign-string-to-lisp buf))))
```



```lisp>BOA
 (get-hostname)
"aurora"
```



## Crystal


```crystal>hostname = System.hostname</lang



## D


```d
import std.stdio, std.socket;

void main() {
    writeln(Socket.hostName());
}
```



## Delphi


```Delphi
program ShowHostName;

{$APPTYPE CONSOLE}

uses Windows;

var
  lHostName: array[0..255] of char;
  lBufferSize: DWORD;
begin
  lBufferSize := 256;
  if GetComputerName(lHostName, lBufferSize) then
    Writeln(lHostName)
  else
    Writeln('error getting host name');
end.
```



## E



```e
makeCommand("hostname")()[0].trim()
```


Not exactly a good way to do it. A better way ought to be introduced along with a proper socket interface. [[Category:E examples needing attention]]


## Emacs Lisp


```Lisp
(system-name)
```


=={{header|F_Sharp|F#}}==

```fsharp
printfn "%s" (System.Net.Dns.GetHostName())
```



## Factor


```factor
USE: io.sockets
host-name
```



## Forth

{{works with|GNU Forth|0.7.0}}

```forth
include unix/socket.fs

hostname type
```



## Erlang


```Erlang
Host = net_adm:localhost().
```



## friendly interactive shell

{{trans|UNIX Shell}}


```fishshell>hostname</lang

or

```fishshell
uname -n
```



## Fortran

{{works with|gfortran}}

The function/subroutine <tt>HOSTNM</tt> is a GNU extension.

```fortran
program HostTest
  character(len=128) :: name
  call hostnm(name)
  print *, name
end program HostTest
```


Using fortran 2003 C-interoperability we can call posix C function gethostname (unix system call) directly


```fortran

program test_hostname
   use, intrinsic  :: iso_c_binding
   implicit none
   interface !to function: int gethostname(char *name, size_t namelen);
      integer(c_int) function gethostname(name, namelen) bind(c)
         use, intrinsic  :: iso_c_binding, only: c_char, c_int, c_size_t
         integer(c_size_t), value, intent(in) :: namelen
         character(len=1,kind=c_char), dimension(namelen),  intent(inout) ::  name
      end function gethostname
   end interface
   integer(c_int) :: status
   integer,parameter :: HOST_NAME_MAX=255
   character(kind=c_char,len=1),dimension(HOST_NAME_MAX) :: cstr_hostname
   integer(c_size_t) :: lenstr
   character(len=:),allocatable :: hostname
   lenstr = HOST_NAME_MAX
   status = gethostname(cstr_hostname, lenstr)
   hostname = c_to_f_string(cstr_hostname)
   write(*,*) hostname, len(hostname)

 contains
   ! convert c_string to f_string
   pure function c_to_f_string(c_string) result(f_string)
      use, intrinsic :: iso_c_binding, only: c_char, c_null_char
      character(kind=c_char,len=1), intent(in) :: c_string(:)
      character(len=:), allocatable :: f_string
      integer i, n
      i = 1
      do
         if (c_string(i) == c_null_char) exit
         i = i + 1
      end do
      n = i - 1  ! exclude c_null_char
      allocate(character(len=n) :: f_string)
      f_string = transfer(c_string(1:n), f_string)
   end function c_to_f_string

end program test_hostname

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' On Windows 10, the command line utility HOSTNAME.EXE prints the 'hostname' to the console.
' We can execute this remotely and read from its 'stdin' stream as follows:

Dim As String hostname
Open Pipe "hostname" For Input As #1
Input #1, hostname
Close #1
Print hostname
Print
Print "Press any key to quit"
Sleep
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=11d7f489117d7909da509050103b7891 Click this link to run this code]'''

```gambas
Public Sub Main()

Print System.Host

End
```

Output:

```txt

charlie

```



## Go

Use [https://golang.org/pkg/os/#Hostname <code>os.Hostname</code>].

```go
package main

import (
	"fmt"
	"os"
)

func main() {
	fmt.Println(os.Hostname())
}
```



## Groovy



```groovy>println InetAddress.localHost.hostName</lang



## Harbour


```visualfoxpro
? NetName()
```



## Haskell

{{libheader|network}}

```haskell
import Network.BSD
main = do hostName <- getHostName
          putStrLn hostName
```


Or if you don't want to depend on the network package being installed, you can implement it on your own (this implementation is based on the implementation in the network package).



```haskell
module GetHostName where

import Foreign.Marshal.Array ( allocaArray0, peekArray0 )
import Foreign.C.Types ( CInt(..), CSize(..) )
import Foreign.C.String ( CString, peekCString )
import Foreign.C.Error ( throwErrnoIfMinus1_ )

getHostName :: IO String
getHostName = do
  let size = 256
  allocaArray0 size $ \ cstr -> do
    throwErrnoIfMinus1_ "getHostName" $ c_gethostname cstr (fromIntegral size)
    peekCString cstr

foreign import ccall "gethostname"
   c_gethostname :: CString -> CSize -> IO CInt

main = do hostName <- getHostName
          putStrLn hostName
```


=={{header|Icon}} and {{header|Unicon}}==

```icon
procedure main()
  write(&host)
end
```



## IDL


```idl
hostname = GETENV('computername')
```



## J


```j
NB. Load the socket libraries

  load 'socket'
  coinsert 'jsocket'

NB. fetch and implicitly display the hostname

  > {: sdgethostname ''

NB. If fetching the hostname is the only reason for loading the socket libraries,
NB. and the hostname is fetched only once, then use a 'one-liner' to accomplish it:

  > {: sdgethostname coinsert 'jsocket' [ load 'socket'
```



## Java


```java
import java.net.*;
class DiscoverHostName {
 public static void main(final String[] args) {
  try {
   System.out.println(InetAddress.getLocalHost().getHostName());
  } catch (UnknownHostException e) { // Doesn't actually happen, but Java requires it be handled.
  }
 }
}
```



## JavaScript

{{works with|JScript}}

```javascript
var network = new ActiveXObject('WScript.Network');
var hostname = network.computerName;
WScript.echo(hostname);
```



## jq

Currently jq does not have a "gethostname" or a "system" command, so the best ways for a jq program to have access to the hostname are via an environment variable, or via a command line argument, as illustrated here:

```txt
HOST=$(hostname) jq -n --arg hostname $(hostname) '[env.HOST, $hostname]'
```

{{output}}

```txt
[
  "mini.local",
  "mini.local"
]
```



## Jsish


```javascript
var hn = exec("hostname", {retAll:true}).data.trim();
```



## Julia


```Julia

println(gethostname())

```


{{out}}

```txt

harlan

```



## K


```K

_h

```


{{out}}

```txt

`"narasimman-pc"

```



## Kotlin


```scala
// version 1.1.4

import java.net.InetAddress

fun main(args: Array<String>) {
    println(InetAddress.getLocalHost().hostName)
}
```



## Lasso

This will ge the hostname as reported by the web server

```Lasso
[web_request->httpHost]
```

-> www.myserver.com

This will ge the hostname as reported by the system OS

```Lasso
define host_name => thread {

	data
		public initiated::date, // when the thread was initiated. Most likely at Lasso server startup
		private hostname::string // as reported by the servers hostname

	public onCreate() => {
		.reset
	}

	public reset() => {
		if(lasso_version(-lassoplatform) >> 'Win') => {
			protect => {
				local(process = sys_process('cmd',(:'hostname.exe')))
				#process -> wait
				.hostname = string(#process -> readstring) -> trim&
				#process -> close
			}
		else
			protect => {
				local(process = sys_process('/bin/hostname'))
				#process -> wait
				.hostname = string(#process -> readstring) -> trim&
				#process -> close
			}
		}
		.initiated = date(date -> format(`yyyyMMddHHmmss`)) // need to set format to get rid of nasty hidden fractions of seconds
		.hostname -> size == 0 ? .hostname = 'undefined'
	}

	public asString() => .hostname

}

host_name
```

-> mymachine.local


## LFE



```lisp

(net_adm:localhost)

```



## Liberty BASIC


```lb
lpBuffer$=Space$(128) + Chr$(0)
struct SIZE,sz As Long
SIZE.sz.struct=Len(lpBuffer$)

calldll #kernel32, "GetComputerNameA",lpBuffer$ as ptr, SIZE as struct, result as Long
CurrentComputerName$=Trim$(Left$(lpBuffer$, SIZE.sz.struct))

print CurrentComputerName$
```



## Limbo

As with nearly anything in Inferno, it boils down to reading a file:


```Limbo
implement Hostname;

include "sys.m"; sys: Sys;
include "draw.m";

Hostname: module {
	init: fn(nil: ref Draw->Context, nil: list of string);
};

init(nil: ref Draw->Context, nil: list of string)
{
	sys = load Sys Sys->PATH;
	buf := array[Sys->ATOMICIO] of byte;

	fd := sys->open("/dev/sysname", Sys->OREAD);
	if(fd == nil)
		die("Couldn't open /dev/sysname");

	n := sys->read(fd, buf, len buf - 1);
	if(n < 1)
		die("Couldn't read /dev/sysname");

	buf[n++] = byte '\n';
	sys->write(sys->fildes(1), buf, n);
}

die(s: string)
{
	sys->fprint(sys->fildes(2), "hostname: %s: %r", s);
	raise "fail:errors";
}

```


Sys->ATOMICIO is usually 8 kilobytes; this version truncates if you have a ridiculously long hostname.


## Lingo

{{libheader|Shell Xtra}}

```lingo

sx = xtra("Shell").new()
if the platform contains "win" then
  hostname = sx.shell_cmd("hostname", ["eol":RETURN]).line[1] -- win 7 or later
else
  hostname = sx.shell_cmd("hostname", RETURN).line[1]
end if
```



## LiveCode


```LiveCode>answer the hostName</lang



## Lua

Requires: LuaSocket

```lua
socket = require "socket"
print( socket.dns.gethostname() )
```



## M2000 Interpreter


```M2000 Interpreter

Module Host {
      \\ one way
      Print computer$
      \\ second way
      Declare objNetwork "WScript.Network"
      With objNetwork,  "ComputerName" as cName$
      Print cName$, cName$=Computer$
      Declare objNetwork Nothing
}
Host

```



## Maple


```Maple
Sockets:-GetHostName()
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
$MachineName
```



## MATLAB

This is a built-in MATLAB function. "failed" is a Boolean which will be false if the command sent to the OS succeeds. "hostname" is a string containing the system's hostname, provided that the external command <tt>hostname</tt> exists.


```Matlab
[failed,hostname] = system('hostname')
```



## mIRC Scripting Language


```mirc
echo -ag $host
```


=={{header|Modula-3}}==

```modula3
MODULE Hostname EXPORTS Main;

IMPORT IO, OSConfig;

BEGIN
  IO.Put(OSConfig.HostName() & "\n");
END Hostname.
```



## MUMPS


```MUMPS
Write $Piece($System,":")
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols binary

say InetAddress.getLocalHost.getHostName

```



## NewLISP


```NewLISP
(! "hostname")
```



## Nim


```Nim
import posix
const size = 64
var s = cstring(newString(size))
discard s.getHostname(size)
echo s
```


=={{header|Oberon-2}}==
Works with oo2c version 2

```oberon2

MODULE HostName;
IMPORT
  OS:ProcessParameters,
  Out;
BEGIN
  Out.Object("Host: " + ProcessParameters.GetEnv("HOSTNAME"));Out.Ln
END HostName.

```

Output:

```txt

Host: localhost.localdomain

```

=={{header|Objective-C}}==

Cocoa / Cocoa Touch / GNUstep:


```objc

NSLog(@"%@", [[NSProcessInfo processInfo] hostName]);

```


Example Output:


```objc

2010-09-16 16:20:00.000 Playground[1319:a0f] sierra117.local // Hostname is sierra117.local.

```



## Objeck


```objeck

use Net;

bundle Default {
  class Hello {
    function : Main(args : String[]) ~ Nil {
      TCPSocket->HostName()->PrintLine();
    }
  }
}

```



## OCaml


```ocaml
Unix.gethostname()
```



## Octave

Similarly to [[Discover the Hostname#MATLAB|MATLAB]], we could call system command <tt>hostname</tt> to know the hostname. But we can also call the internal function <tt>uname()</tt> which returns a structure holding several informations, among these the hostname (nodename):


```octave
uname().nodename
```



## ooRexx

These solutions are platform specific.

### Windows Platform

A solution using ActiveX/OLE on Windows


```ooRexx
say .oleObject~new('WScript.Network')~computerName
```


and one using the Windows environment variables


```ooRexx
say value('COMPUTERNAME',,'environment')
```



### UNIX Platform

Some UNIX solutions (tested under Mac OS X):

ooRexx (and [[REXX|Rexx]]) can issue commands directly to the shell it's running under.
Output of the shell commands will normally be STDOUT and STDERR.
These next two samples will simply output the host name to the console if the program is run from a command prompt.
:'''Note:'''  The '''<tt>address command</tt>''' clause causes the contents of the literal string that follows it to be sent to the command shell.


```ooRexx
address command 'hostname -f'
```



```ooRexx
address command "echo $HOSTNAME"
```


Command output can also be captured by the program to allow further processing.
ooRexx provides an external data queue manager ('''''<tt>rxqueue</tt>''''') that can be used for this.
In the following examples output written to STDOUT/STDERR is piped into '''<tt>rxqueue</tt>''' which sends it in turn to a Rexx queue for further processing by the program:


```ooRexx
/* Rexx */
address command "echo $HOSTNAME | rxqueue"
address command "hostname -f | rxqueue"
loop q_ = 1 while queued() > 0
  parse pull hn
  say q_~right(2)':' hn
  end q_

```


A utility class is also provided as a wrapper around the external data queue:


```ooRexx
/* Rexx */
qq = .rexxqueue~new()
address command "echo $HOSTNAME | rxqueue"
address command "hostname -f | rxqueue"
loop q_ = 1 while qq~queued() > 0
  hn = qq~pull()
  say q_~right(2)':' hn
  end q_

```



## Oz


```oz
{System.showInfo {OS.getHostByName 'localhost'}.name}
```



## PARI/GP

Running the <code>hostname</code> or <code>uname</code> program and capturing its output (the first line of output) in a string.


```parigp
str = externstr("hostname")[1];
str = externstr("uname -n")[1];
```



## Pascal

For Windows systems see the [[Hostname#Delphi | Delphi]] example.
On Unix systems, FreePascal has the function GetHostName:

```pascal
Program HostName;

uses
  unix;

begin
  writeln('The name of this computer is: ', GetHostName);
end.
```

Output example on Mac OS X:

```txt

The name of this computer is: MyComputer.local

```



## Perl

{{works with|Perl|5.8.6}}

{{libheader|Sys::Hostname}}

```perl
use Sys::Hostname;

$name = hostname;
```


## Perl 6


```perl6
my $host = qx[hostname];
```



## Phix


```Phix
constant tmp = "hostname.txt",
         cmd = iff(platform()=WINDOWS?"hostname":"uname -n")
{} = system_exec(sprintf("%s > %s",{cmd,tmp}),4)
string host = trim(get_text(tmp))
{} = delete_file("hostname.txt")
?host
```

{{out}}

```txt

"Pete-PC"

```



## PHP


```php
echo $_SERVER['HTTP_HOST'];
```



```php
echo php_uname('n');
```


{{works with|PHP|5.3+}}

```php
echo gethostname();
```



## PicoLisp

This will just print the hostname:

```PicoLisp
(call 'hostname)
```

To use it as a string in a program:

```PicoLisp
(in '(hostname) (line T))
```



## Pike


```pike
import System;

int main(){
   write(gethostname() + "\n");
}
```



## PL/SQL


```plsql
SET serveroutput on
BEGIN
  DBMS_OUTPUT.PUT_LINE(UTL_INADDR.GET_HOST_NAME);
END;
```



## Pop11


```pop11
lvars host = sys_host_name();
```



## PowerBASIC

This retreives the localhost's name:


```powerbasic
HOST NAME TO hostname$
```


This attempts to retreive the name of an arbitrary machine on the network (assuming ipAddress& is valid):


```powerbasic
HOST NAME ipAddress& TO hostname$
```



## PowerShell

Windows systems have the <code>ComputerName</code> environment variable which can be used:

```powershell
$Env:COMPUTERNAME
```

Also PowerShell can use .NET classes and methods:

```powershell
[Net.Dns]::GetHostName()
```



## PureBasic

{{works with|PureBasic|4.41}}

```PureBasic
InitNetwork()
answer$=Hostname()
```



## Python

{{works with|Python|2.5}}

```python
import socket
host = socket.gethostname()
```



## R

Sys.info provides information about the platform that R is running on.  The following code returns the hostname as a string.

```R
Sys.info()[["nodename"]]
```

Note that Sys.info isn't guaranteed to be available on all platforms.  As an alternative, you can call an OS command.

```R
system("hostname", intern = TRUE)
```

... or retrieve an environment variable

```R

env_var <- ifelse(.Platform$OS.type == "windows", "COMPUTERNAME", "HOSTNAME")
Sys.getenv(env_var)

```



## Racket


```Racket

#lang racket/base
(require mzlib/os)
(gethostname)

```



## REBOL


```REBOL>print system/network/host</lang



## REXX


### REGINA and PC/REXX under most MS NT Windows

This REXX solution is for REGINA and PC/REXX under the Microsoft NT family of Windows (XP, Vista, 7, etc).

Other names could be used for the 3rd argument.


The   ''computername''   is the same as the output for the   '''hostname.exe'''   program.

```REXX
say value('COMPUTERNAME',,"ENVIRONMENT")
say value('OS',,"ENVIRONMENT")
```

'''output''' (using Windows/XP)

```txt

GERARD46
Windows_NT

```


### R4 and ROO under most MS NT Windows

This REXX solution is for R4 and ROO under the Microsoft NT family of Windows (XP, Vista, 7, etc).

Other names could be used for the 3rd argument.

```REXX
say value('COMPUTERNAME',,"SYSTEM")
say value('OS',,"SYSTEM")
```


===MS DOS (without Windows), userid===
Under Microsoft DOS (with no Windows), the closest thing to a name of a host would be the userid.

```rexx
say userid()
```


===MS DOS (without Windows), version of DOS===
But perhaps the name or version of the MS DOS system would be more appropriate than the userid.

```REXX
'VER'    /*this passes the  VER  command to the MS DOS system. */
```

Each REXX interpreter has their own name (some have multiple names) for the environmental variables.

Different operating systems may call their hostnames by different identifiers.

IBM mainframes (at one time) called the name of the host as a ''nodename'' and it needn't be

specified, in which case an asterisk (*) is returned.

I recall (perhaps wrongly) that Windows/95 and Windows/98 had a different environmental name for the name of the host.


### UNIX Solution

This solution is platform specific and uses features that are available to the Regina implementation of Rexx.
:Tested with Regina on Mac OS X.  Should work on other UNIX/Linux distros.

```REXX
/* Rexx */
address command "hostname -f" with output stem hn.
do q_ = 1 to hn.0
  say hn.q_
  end q_
exit
```



## Ruby


```ruby
require 'socket'
host = Socket.gethostname
```



## Run BASIC


```runbasic
print Platform$    ' OS where Run BASIC is being hosted
print UserInfo$    ' Information about the user's web browser
print UserAddress$ ' IP address of the user
```



## Rust

Works on windows and linux with crate <code>hostname</code> version 0.1.5

```Rust
fn main() {
    match hostname::get_hostname() {
        Some(host) => println!("hostname: {}", host),
        None => eprintln!("Could not get hostname!"),
    }
}
```



## Scala


```scala
println(java.net.InetAddress.getLocalHost.getHostName)
```



## Scheme

{{works with|Chicken Scheme}}

```scheme
(use posix)
(get-host-name)
```

{{works with|Guile}}

```scheme
(gethostname)
```



## Seed7

The library [http://seed7.sourceforge.net/libraries/socket.htm socket.s7i]
defines the function [http://seed7.sourceforge.net/libraries/socket.htm#getHostname getHostname],
which returns the hostname.


```seed7
$ include "seed7_05.s7i";
  include "socket.s7i";

const proc: main is func
  begin
    writeln(getHostname);
  end func;
```



## Sidef


```ruby
var sys = frequire('Sys::Hostname');
var host = sys.hostname;
```

Or:

```ruby
var host = `hostname`.chomp;
```



## Slate


```slate>Platform current nodeName</lang



## SNOBOL4


```snobol4

      output = host(4,"HOSTNAME")
end
```



## Standard ML


```sml
NetHostDB.getHostName ()
```



## Smalltalk

{{works with|Smalltalk/X}}

```Smalltalk>OperatingSystem getHostName</lang



## SQL

{{works with|Oracle}}

```sql

select host_name from v$instance;

```



## SQL PL

{{works with|Db2 LUW}}

```sql pl

SELECT HOST_NAME FROM SYSIBMADM.ENV_SYS_INFO

```

Output:

```txt

HOST_NAME
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
hostname

  1 record(s) selected.


```



## Swift

Swift 3

```Swift
print(ProcessInfo.processInfo.hostName)
```



## Tcl

The basic introspection tool in TCL is the <tt>info</tt> command. It can be used to find out about the version of the current Tcl or Tk, the available commands and libraries, variables, functions, the level of recursive interpreter invocation, and, amongst a myriad other things, the name of the current machine:


```Tcl
set hname [info hostname]
```



## Toka


```toka
2 import gethostname
1024 chars is-array foo
foo 1024 gethostname
foo type
```


## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
host=HOST ()

```



## UNIX Shell


```bash>hostname</lang

or

```bash
uname -n
```



## Ursa


```ursa
out (ursa.net.localhost.name) endl console
```



## Ursala

The user-defined hostname function ignores its argument and returns a string.

```Ursala
#import cli

hostname = ~&hmh+ (ask bash)/<>+ <'hostname'>!
```

For example, the following function returns the square root of its argument
if it's running on host kremvax, but otherwise returns the square.

```Ursala
#import flo

creative_accounting = (hostname== 'kremvax')?(sqrt,sqr)
```



## VBScript


```vb

Set objNetwork = CreateObject("WScript.Network")
WScript.Echo objNetwork.ComputerName

```



## Vim Script


```vim
echo hostname()
```



## Visual Basic

{{works with|Visual Basic|5}}
{{works with|Visual Basic|6}}
{{works with|VBA|Access 97}}
{{works with|VBA|6.5}}
{{works with|VBA|7.1}}

```vb
Option Explicit

Private Declare Function GetComputerName Lib "kernel32.dll" Alias "GetComputerNameW" _
  (ByVal lpBuffer As Long, ByRef nSize As Long) As Long

Private Const MAX_COMPUTERNAME_LENGTH As Long = 31
Private Const NO_ERR As Long = 0

Private Function Hostname() As String
Dim i As Long, l As Long, s As String
  s = Space$(MAX_COMPUTERNAME_LENGTH)
  l = Len(s) + 1
  i = GetComputerName(StrPtr(s), l)
  Debug.Assert i <> 0
  Debug.Assert l <> 0
  Hostname = Left$(s, l)
End Function

Sub Main()
  Debug.Assert Hostname() = Environ$("COMPUTERNAME")
End Sub
```



## zkl


```zkl>System.hostname</lang

Or open a server socket, which contains the hostname.

```zkl
Network.TCPServerSocket.open(8080).hostname
```



{{omit from|ACL2}}
{{omit from|Locomotive Basic|Does not have a hostname.}}
{{omit from|ML/I}}
{{omit from|PARI/GP}}
{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have network access. -->
{{omit from|Unlambda|Does not have network access.}}
{{omit from|ZX Spectrum Basic|Does not have a hostname.}}
{{omit from|Commodore BASIC}}
