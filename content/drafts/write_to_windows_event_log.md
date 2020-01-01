+++
title = "Write to Windows event log"
description = ""
date = 2019-07-20T17:11:03Z
aliases = []
[extra]
id = 7181
[taxonomies]
categories = []
tags = []
+++

{{task}}

;Task:
Write script status to the Windows Event Log





## AutoHotkey


```autohotkey
; By ABCza, http://www.autohotkey.com/board/topic/76170-function-send-windows-log-events/
h := RegisterForEvents("AutoHotkey")
SendWinLogEvent(h, "Test Message")
DeregisterForEvents(h)

/*
--------------------------------------------------------------------------------------------------------------------------------
FUNCTION: SendWinLogEvent
--------------------------------------------------------------------------------------------------------------------------------
Writes an entry at the end of the specified Windows event log. Returns nonzero if the function succeeds or zero if it fails.

PARAMETERS:
~~~~~~~~~~~
hSource		- Handle to a previously registered events source with RegisterForEvents.
evType		- EVENTLOG_SUCCESS			:= 0x0000
			  EVENTLOG_AUDIT_FAILURE	:= 0x0010
			  EVENTLOG_AUDIT_SUCCESS	:= 0x0008
			  EVENTLOG_ERROR_TYPE		:= 0x0001
			  EVENTLOG_INFORMATION_TYPE	:= 0x0004
			  EVENTLOG_WARNING_TYPE		:= 0x0002
evId		- Event ID, can be any dword value.
evCat		- Any value, used to organize events in categories.
pStrings	- A continuation section with newline separated strings (each max 31839 chars).
pData		- A buffer containing the binary data.
--------------------------------------------------------------------------------------------------------------------------------
SYSTEM CALLS, STRUCTURES AND INFO:
--------------------------------------------------------------------------------------------------------------------------------
ReportEvent										- http://msdn.microsoft.com/en-us/library/windows/desktop/aa363679(v=vs.85).aspx
Event Identifiers								- http://msdn.microsoft.com/en-us/library/windows/desktop/aa363651(v=vs.85).aspx
Event categories								- http://msdn.microsoft.com/en-us/library/windows/desktop/aa363649(v=vs.85).aspx
--------------------------------------------------------------------------------------------------------------------------------
*/
SendWinLogEvent(hSource, String="", evType=0x0004, evId=0x03EA, evCat=0, pData=0) {
	Ptr := A_PtrSize ? "Ptr" : "UInt"
	StringPut := A_IsUnicode ? "StrPut" : "StrPut2"

	; Reserve and initialise space for the event message.
	VarSetCapacity(eventMessage, StrLen(String), 0)
	%StringPut%(String, eventMessage)

	r := DllCall("Advapi32.dll\ReportEvent" (A_IsUnicode ? "W" : "A")
		, UInt, hSource			; handle
		, UShort, evType		; WORD, eventlog_information_type
		, UShort, evCat			; WORD, category
		, UInt, evId			; DWORD, event ID, 0x03EA
		, Ptr, 0				; PSID, ptr to user security ID
		, UShort, 1				; WORD, number of strings
		, UInt, VarSetCapacity(pData)					; DWORD, data size
		, Ptr, &eventMessage							; LPCTSTR*, ptr to a buffer ...
		, Ptr, (VarSetCapacity(pData)) ? &pData : 0 )	; ptr to a buffer of binary data

	; Release memory.
	VarSetCapacity(eventMessage, 0)

	Return r
}
/*
--------------------------------------------------------------------------------------------------------------------------------
FUNCTION: RegisterForEvents
--------------------------------------------------------------------------------------------------------------------------------
Registers the application to send Windows log events. Returns a handle to the registered source.

PARAMETERS:
~~~~~~~~~~~
logName	 - Can be "Application", "System" or a custom log name.
--------------------------------------------------------------------------------------------------------------------------------
SYSTEM CALLS, STRUCTURES AND INFO:
--------------------------------------------------------------------------------------------------------------------------------
RegisterEventSource							- http://msdn.microsoft.com/en-us/library/windows/desktop/aa363678(v=VS.85).aspx
Event Sources								- http://msdn.microsoft.com/en-us/library/windows/desktop/aa363661(v=VS.85).aspx
--------------------------------------------------------------------------------------------------------------------------------
*/
RegisterForEvents(logName) {
	Return DllCall("Advapi32.dll\RegisterEventSource" (A_IsUnicode ? "W" : "A")
		, UInt, 0				; LPCTSTR, Local computer
		, Str, logName)			; LPCTSTR Source name
}
/*
--------------------------------------------------------------------------------------------------------------------------------
FUNCTION: DeregisterForEvents
--------------------------------------------------------------------------------------------------------------------------------
Deregisters the previously registered application.

PARAMETERS:
~~~~~~~~~~~
hSource	 - Handle to a registered source.
--------------------------------------------------------------------------------------------------------------------------------
SYSTEM CALLS, STRUCTURES AND INFO:
--------------------------------------------------------------------------------------------------------------------------------
DeregisterEventSource						- http://msdn.microsoft.com/en-us/library/windows/desktop/aa363642(v=vs.85).aspx
Event Sources								- http://msdn.microsoft.com/en-us/library/windows/desktop/aa363661(v=VS.85).aspx
--------------------------------------------------------------------------------------------------------------------------------
*/
DeregisterForEvents(hSource) {
	IfNotEqual, hSource, 0, Return DllCall( "Advapi32.dll\DeregisterEventSource"
		, UInt, hSource )
}

; StrPut for AutoHotkey Basic
StrPut2(String, Address="", Length=-1, Encoding=0)
{
	; Flexible parameter handling:
	if Address is not integer		 ; StrPut(String [, Encoding])
		Encoding := Address,	Length := 0,	Address := 1024
	else if Length is not integer	 ; StrPut(String, Address, Encoding)
		Encoding := Length,	Length := -1

	; Check for obvious errors.
	if (Address+0 < 1024)
		return

	; Ensure 'Encoding' contains a numeric identifier.
	if Encoding = UTF-16
		Encoding = 1200
	else if Encoding = UTF-8
		Encoding = 65001
	else if SubStr(Encoding,1,2)="CP"
		Encoding := SubStr(Encoding,3)

	if !Encoding ; "" or 0
	{
		; No conversion required.
		char_count := StrLen(String) + 1 ; + 1 because generally a null-terminator is wanted.
		if (Length)
		{
			; Check for sufficient buffer space.
			if (StrLen(String) <= Length || Length == -1)
			{
				if (StrLen(String) == Length)
					; Exceptional case: caller doesn't want a null-terminator.
					char_count--
				; Copy the string, including null-terminator if requested.
				DllCall("RtlMoveMemory", "uint", Address, "uint", &String, "uint", char_count)
			}
			else
				; For consistency with the sections below, don't truncate the string.
				char_count = 0
		}
		;else: Caller just wants the the required buffer size (char_count), which will be returned below.
	}
	else if Encoding = 1200 ; UTF-16
	{
		; See the 'else' to this 'if' below for comments.
		if (Length <= 0)
		{
			char_count := DllCall("MultiByteToWideChar", "uint", 0, "uint", 0, "uint", &String, "int", StrLen(String), "uint", 0, "int", 0) + 1
			if (Length == 0)
				return char_count
			Length := char_count
		}
		char_count := DllCall("MultiByteToWideChar", "uint", 0, "uint", 0, "uint", &String, "int", StrLen(String), "uint", Address, "int", Length)
		if (char_count && char_count < Length)
			NumPut(0, Address+0, char_count++*2, "UShort")
	}
	else if Encoding is integer
	{
		; Convert native ANSI string to UTF-16 first.	NOTE - wbuf_len includes the null-terminator.
		VarSetCapacity(wbuf, 2 * wbuf_len := StrPut2(String, "UTF-16")), StrPut2(String, &wbuf, "UTF-16")

		; UTF-8 and some other encodings do not support this flag.	Avoid it for UTF-8
		; (which is probably common) and rely on the fallback behaviour for other encodings.
		flags := Encoding=65001 ? 0 : 0x400	; WC_NO_BEST_FIT_CHARS
		if (Length <= 0) ; -1 or 0
		{
			; Determine required buffer size.
			loop 2 {
				char_count := DllCall("WideCharToMultiByte", "uint", Encoding, "uint", flags, "uint", &wbuf, "int", wbuf_len, "uint", 0, "int", 0, "uint", 0, "uint", 0)
				if (char_count || A_LastError != 1004) ; ERROR_INVALID_FLAGS
					break
				flags := 0	; Try again without WC_NO_BEST_FIT_CHARS.
			}
			if (!char_count)
				return ; FAIL
			if (Length == 0) ; Caller just wants the required buffer size.
				return char_count
			; Assume there is sufficient buffer space and hope for the best:
			Length := char_count
		}
		; Convert to target encoding.
		char_count := DllCall("WideCharToMultiByte", "uint", Encoding, "uint", flags, "uint", &wbuf, "int", wbuf_len, "uint", Address, "int", Length, "uint", 0, "uint", 0)
		; Since above did not null-terminate, check for buffer space and null-terminate if there's room.
		; It is tempting to always null-terminate (potentially replacing the last byte of data),
		; but that would exclude this function as a means to copy a string into a fixed-length array.
		if (char_count && char_count < Length)
			NumPut(0, Address+0, char_count++, "Char")
		; else no space to null-terminate; or conversion failed.
	}
	; Return the number of characters copied.
	return char_count
}
```


## AWK


```AWK

# syntax: GAWK -f WRITE_TO_WINDOWS_EVENT_LOG.AWK
BEGIN {
    write("INFORMATION",1,"Rosetta Code")
    exit (errors == 0) ? 0 : 1
}
function write(type,id,description,  cmd,esf) {
    esf = errors # errors so far
    cmd = sprintf("EVENTCREATE.EXE /T %s /ID %d /D \"%s\" >NUL",type,id,description)
    printf("%s\n",cmd)
    if (toupper(type) !~ /^(SUCCESS|ERROR|WARNING|INFORMATION)$/) { error("/T is invalid") }
    if (id+0 < 1 || id+0 > 1000) { error("/ID is invalid") }
    if (description == "") { error("/D is invalid") }
    if (errors == esf) {
      system(cmd)
    }
    return(errors)
}
function error(message) { printf("error: %s\n",message) ; errors++ }

```

{{out}}

```txt

EVENTCREATE.EXE /T INFORMATION /ID 1 /D "Rosetta Code" >NUL

```



## Batch File

The "EventCreate" command does the task.

```dos
@echo off
EventCreate /t ERROR /id 123 /l SYSTEM /so "A Batch File" /d "This is found in system log."
EventCreate /t WARNING /id 456 /l APPLICATION /so BlaBla /d "This is found in apps log"
```

{{Out}}

```txt
>EventLog.BAT

SUCCESS: An event of type 'ERROR' was created in the 'SYSTEM' log with 'A Batch File' as the source.

SUCCESS: An event of type 'WARNING' was created in the 'APPLICATION' log with 'BlaBla' as the source.

>
```

If you do not want the command to display its result or errors...

```dos
@echo off
EventCreate /t ERROR /id 123 /l SYSTEM /so "A Batch File" /d "This is found in system log." 2>NUL>NUL
EventCreate /t WARNING /id 456 /l APPLICATION /so BlaBla /d "This is found in apps log" 2>NUL>NUL
::That "2>NUL>NUL" trick actually works in any command!
```


'''NOTE:''' This will (...or might) not work if you do not have administrator privileges.


## BBC BASIC

{{works with|BBC BASIC for Windows}}
Writes to the Application Log:

```bbcbasic
      INSTALL @lib$+"COMLIB"
      PROC_cominitlcid(1033)

      WshShell% = FN_createobject("WScript.Shell")
      PROC_callmethod(WshShell%, "LogEvent(0, ""Test from BBC BASIC"")")

      PROC_releaseobject(WshShell%)
      PROC_comexit
```



## C

The following is a wrapper on the EventCreate utility provided in Windows. Note that to use this wrapper, the code must be executed from a console/IDE running as Administrator. The utility itself does extensive error-checking and validation, so apart from the check that 5 arguments have been supplied, no other validations or checks are performed.

```C

#include<stdlib.h>
#include<stdio.h>

int main(int argC,char* argV[])
{
	char str[1000];

	if(argC!=5)
		printf("Usage : %s < Followed by level, id, source string and description>",argV[0]);
	else{
		sprintf(str,"EventCreate /t %s /id %s /l APPLICATION /so %s /d \"%s\"",argV[1],argV[2],argV[3],argV[4]);
		system(str);
	}

	return 0;
}

```

Invocation and output on console :

```txt

C:\rosettaCode>eventLog.exe WARNING 458 SOmeString "This is a joke"

SUCCESS: An event of type 'WARNING' was created in the 'APPLICATION' log with 'SOmeString' as the source.

```

Microsoft does provide an C/C++ API for EventCreate, but as with everything Microsoft, it's so wonderfully convoluted, that I will just give a link to the [https://msdn.microsoft.com/en-us/library/aa363680(v=vs.85).aspx ReportEvent] example.


## C++

{{trans|C}}

```cpp
#include <iostream>
#include <sstream>

int main(int argc, char *argv[]) {
    using namespace std;

#if _WIN32
    if (argc != 5) {
        cout << "Usage : " << argv[0] << " (type)  (id)  (source string) (description>)\n";
        cout << "    Valid types: SUCCESS, ERROR, WARNING, INFORMATION\n";
    } else {
        stringstream ss;
        ss << "EventCreate /t " << argv[1] << " /id " << argv[2] << " /l APPLICATION /so " << argv[3] << " /d \"" << argv[4] << "\"";
        system(ss.str().c_str());
    }
#else
    cout << "Not implemented for *nix, only windows.\n";
#endif

    return 0;
}
```



## C sharp

In Windows Vista and later or Windows Server 2003, you must have administrative privileges to execute this code.

```csharp
using System.Diagnostics;

namespace RC
{
  internal class Program
  {
    public static void Main()
    {
      string sSource  = "Sample App";
      string sLog     = "Application";
      string sEvent   = "Hello from RC!";

      if (!EventLog.SourceExists(sSource))
        EventLog.CreateEventSource(sSource, sLog);

      EventLog.WriteEntry(sSource, sEvent);
      EventLog.WriteEntry(sSource, sEvent, EventLogEntryType.Information);
    }
  }
}
```



## Clojure


```clojure
(use 'clojure.java.shell)
(sh "eventcreate" "/T" "INFORMATION" "/ID" "123" "/D" "Rosetta Code example")
```



## D

{{trans|Kotlin}}

```D
import std.process;
import std.stdio;

void main() {
    auto cmd = executeShell(`EventCreate /t INFORMATION /id 123 /l APPLICATION /so Dlang /d "Rosetta Code Example"`);

    if (cmd.status == 0) {
        writeln("Output: ", cmd.output);
    } else {
        writeln("Failed to execute command, status=", cmd.status);
    }
}
```



## Delphi


```Delphi
program WriteToEventLog;

{$APPTYPE CONSOLE}

uses Windows;

procedure WriteLog(aMsg: string);
var
  lHandle: THandle;
  lMessagePtr: Pointer;
begin
  lMessagePtr := PChar(aMsg);
  lHandle := RegisterEventSource(nil, 'Logger');
  if lHandle > 0 then
  begin
    try
      ReportEvent(lHandle, 4 {Information}, 0, 0, nil, 1, 0, @lMessagePtr, nil);
    finally
      DeregisterEventSource(lHandle);
    end;
  end;
end;

begin
  WriteLog('Message to log.');
end.
```


=={{header|F_Sharp|F#}}==
<p>Bare bone writing to the Application Eventlog giving no event-ID and using the default event type (information.)</p>

```fsharp
use log = new System.Diagnostics.EventLog()
log.Source <- "Sample Application"
log.WriteEntry("Entered something in the Application Eventlog!")
```



## Go

This works on Windows 10 with administrative privileges.

```go
package main

import (
    "fmt"
    "os/exec"
)

func main() {
    command := "EventCreate"
    args := []string{"/T", "INFORMATION", "/ID", "123", "/L", "APPLICATION",
        "/SO", "Go", "/D", "\"Rosetta Code Example\""}
    cmd := exec.Command(command, args...)
    err := cmd.Run()
    if err != nil {
        fmt.Println(err)
    }
}
```



## Java


```java
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Locale;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

public class WriteToWindowsEventLog {
    public static void main(String[] args) throws IOException, InterruptedException {
        String osName = System.getProperty("os.name").toUpperCase(Locale.ENGLISH);
        if (!osName.startsWith("WINDOWS")) {
            System.err.println("Not windows");
            return;
        }

        Process process = Runtime.getRuntime().exec("EventCreate /t INFORMATION /id 123 /l APPLICATION /so Java /d \"Rosetta Code Example\"");
        process.waitFor(10, TimeUnit.SECONDS);
        int exitValue = process.exitValue();
        System.out.printf("Process exited with value %d\n", exitValue);
        if (exitValue != 0) {
            InputStream errorStream = process.getErrorStream();
            String result = new BufferedReader(new InputStreamReader(errorStream))
                .lines()
                .collect(Collectors.joining("\n"));
            System.err.println(result);
        }
    }
}
```



## Julia

Run as an administrator.

```julia

 cmd = "eventcreate /T INFORMATION /ID 123 /D \"Rosetta Code Write to Windows event log task example\""
 Base.run(`$cmd`)

```




## Kotlin

The following works on Windows 10 with administrative privileges:

```scala
// version 1.1.4-3

fun main(args: Array<String>) {
    val command = "EventCreate" +
                  " /t INFORMATION" +
                  " /id 123" +
                  " /l APPLICATION" +
                  " /so Kotlin" +
                  " /d \"Rosetta Code Example\""

    Runtime.getRuntime().exec(command)
}
```



## Lingo

{{libheader|Shell xtra}}

```Lingo
shell = xtra("Shell").new()
props = [:]
props["operation"] = "runas"
props["parameters"] = "/t INFORMATION /id 123 /l APPLICATION /so Lingo /d "&QUOTE&"Rosetta Code Example"&QUOTE
shell.shell_exec("EventCreate", props)
```



## Perl

{{libheader|Win32&#58;&#58;EventLog}}
The Win32::EventLog module has the Report method to write in the EventLog


```perl

use strict;
use warnings;

use Win32::EventLog;
my $handle = Win32::EventLog->new("Application");

my $event = {
	Computer 	=>	$ENV{COMPUTERNAME},
	Source		=> 	'Rosettacode',
	EventType 	=> 	EVENTLOG_INFORMATION_TYPE,
	Category  	=> 	'test',
	EventID 	=> 	0,
	Data 		=> 	'a test for rosettacode',
	Strings 	=> 	'a string test for rosettacode',
};
$handle->Report($event);

```



## Phix


```Phix
system(`eventcreate /T INFORMATION /ID 123 /D "Rosetta Code Write to Windows event log task example"`)
```

{{out}} (when running as administrator)

```txt

SUCCESS: An event of type 'INFORMATION' was created in the 'Application' log with 'EventCreate' as the source.

```



## PicoLisp

PicoLisp doesn't run on Windows. In case of Linux, the equivalent of the event log is the syslog. It can be written with '[http://software-lab.de/doc/refN.html#native native]' C functions, or simply with the 'logger' utility:

```PicoLisp
: (call 'logger "This is a test")
-> T

: (call 'logger "This" 'is "another" 'test)
-> T
```



## PureBasic


```PureBasic
Procedure WriteToLog(Event_App$,EventMessage$,EvenetType,Computer$)

  Protected wNumStrings.w, lpString=@EventMessage$, lReturnX, CMessageTyp, lparray
  Protected lprawdata=@EventMessage$, rawdata=Len(EventMessage$), Result
  Protected lLogAPIRetVal.l = RegisterEventSource_(Computer$, Event_App$)

  If lLogAPIRetVal
    lReturnX = ReportEvent_(lLogAPIRetVal,EvenetType,0,CMessageTyp,0,wNumStrings,rawdata,lparray,lprawdata
    DeregisterEventSource_(lLogAPIRetVal)
    Result=#True
  EndIf

  ProcedureReturn Result
EndProcedure
```



## PowerShell


```powershell
# Create Event Log object
$EventLog=new-object System.Diagnostics.EventLog("Application")
#Declare Event Source; must be 'registered' with Windows
$EventLog.Source="Application" # It is possible to register a new source (see Note2)
# Setup the Event Types; you don't have to use them all, but I'm including all the possibilities for reference
$infoEvent=[System.Diagnostics.EventLogEntryType]::Information
$errorEvent=[System.Diagnostics.EventLogEntryType]::Error
$warningEvent=[System.Diagnostics.EventLogEntryType]::Warning
$successAuditEvent=[System.Diagnostics.EventLogEntryType]::SuccessAudit
$failureAuditEvent=[System.Diagnostics.EventLogEntryType]::FailureAudit

# Write the event in the format "Event test",EventType,EventID
$EventLog.WriteEntry("My Test Event",$infoevent,70)
```

''Note1:'' Thanks to PoSH Fan for posting information that got me started on this at [http://winpowershell.blogspot.com/2006/07/writing-windows-events-using.html Windows PowerShell Blog]


''Note2:'' See details on registering a new Event Source with Windows at [http://msdn.microsoft.com/en-us/library/system.diagnostics.eventlog.registerdisplayname.aspx MSDN]


### Source and event log existing


```powershell

   $MessageFreeLula = 'Global unions and union leaders from more than 50 countries came together ' +
      'in Geneva today to stand in solidarity with former Brazilian President Lula, calling for ' +
      'his immediate release from jail and that he be allowed to run in the upcoming elections.'
   Write-EventLog -LogName 'System' -Source 'Eventlog' -Message $MessageFreeLula -EventId 13 -EntryType 'Information'
   'SUCCESS: The Lula Livre message (#FreeLula) has been recorded in the system log event.'

```

{{out|output}}

```txt

SUCCESS: The Lula Livre message (#FreeLula) has been recorded in the system log event.

```


### New event log


```powershell

   $MessageFreeLula = 'Global unions and union leaders from more than 50 countries came together ' +
      'in Geneva today to stand in solidarity with former Brazilian President Lula, calling for ' +
      'his immediate release from jail and that he be allowed to run in the upcoming elections.'
   New-EventLog -LogName 'Free Lula!' -Source '#FreeLula'
   Limit-EventLog -OverflowAction 'OverWriteAsNeeded' -MaximumSize (64KB*13) -LogName 'Free Lula!'
   Write-EventLog -LogName 'Free Lula!' -Source '#FreeLula' -Message $MessageFreeLula -EventId 13 -EntryType 'Information'
   'SUCCESS: The Lula Livre message (#FreeLula) has been recorded in the "Free Lula!" log event.'

```

{{out|output}}

```txt

SUCCESS: The Lula Livre message (#FreeLula) has been recorded in the "Free Lula!" log event.

```




## Python


{{libheader|PyWin32}}


```Python
import win32api
import win32con
import win32evtlog
import win32security
import win32evtlogutil

ph = win32api.GetCurrentProcess()
th = win32security.OpenProcessToken(ph, win32con.TOKEN_READ)
my_sid = win32security.GetTokenInformation(th, win32security.TokenUser)[0]

applicationName = "My Application"
eventID = 1
category = 5	# Shell
myType = win32evtlog.EVENTLOG_WARNING_TYPE
descr = ["A warning", "An even more dire warning"]
data = "Application\0Data".encode("ascii")

win32evtlogutil.ReportEvent(applicationName, eventID, eventCategory=category,
	eventType=myType, strings=descr, data=data, sid=my_sid)
```



## Racket

Racket's logging facility creates windows events when running on Windows.


```Racket

#lang racket
(log-warning "Warning: nothing went wrong.")

```


## Scala

The following works on Windows 10 with elevated (administrative) permission:

```Scala
object RegisterWinLogEvent extends App {

  import sys.process._

  def eventCreate= Seq("EVENTCREATE", "/T", "SUCCESS", "/id", "123", "/l", "APPLICATION", "/so", "Scala RegisterWinLogEvent", "/d", "Rosetta Code Example"  ).!!

  println(eventCreate)

  println(s"\nSuccessfully completed without errors. [total ${scala.compat.Platform.currentTime - executionStart} ms]")

}
```



## REXX

This was executed on a (Microsoft) Windows/XP PRO system.


### annotated


```rexx
/*REXX program writes a "record" (event) to the  (Microsoft)  Windows event log.        */

    eCMD = 'EVENTCREATE'                         /*name of the command that'll be used. */
    type = 'INFORMATION'                         /*one of:  ERROR  WARNING  INFORMATION */
      id =  234                                  /*in range:  1 ───►  1000  (inclusive).*/
 logName = 'APPLICATION'                         /*information about what this is.      */
  source = 'REXX'                                /*     "        "   who's doing this.  */
    desc = 'attempting to add an entry for a Rosetta Code demonstration.'
    desc = '"'  ||  desc  ||  '"'                /*enclose description in double quotes.*/

eCMD  '/T'  type       "/ID"  id       '/L'  logName       "/SO"  source       '/D'  desc

                                                 /*stick a fork in it,  we're all done. */
```

{{out|output}}

```txt

SUCCESS: A 'INFORMATION' type event is created in the 'REXX' log/source.

```



### bare bones


```rexx
/*REXX program writes a "record" (event) to the  (Microsoft)  Windows event log.        */
                                                 /* [↓]  cmd options have extra spacing.*/

'EVENTCREATE     /T  INFORMATION      /ID  234      /L  APPLICATION      /SO  REXX' ,
                '/D  "attempting to add an entry for a Rosetta Code demonstration."'

                                                 /*stick a fork in it,  we're all done. */
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version.}}




## Ruby

{{libheader|win32-utils}}

```ruby
require 'win32/eventlog'
logger = Win32::EventLog.new
logger.report_event(:event_type => Win32::EventLog::INFO, :data => "a test event log entry")
```


Instructions on setting up an Event Source is [http://rubyforge.org/docman/view.php/85/1734/mc_tutorial.html here]


## Tcl

{{libheader|TWAPI}}

```tcl
package require twapi

# This command handles everything; use “-type error” to write an error message
twapi::eventlog_log "My Test Event" -type info
```



## VBScript


```vb

Sub write_event(event_type,msg)
	Set objShell = CreateObject("WScript.Shell")
	Select Case event_type
		Case "SUCCESS"
			n = 0
		Case "ERROR"
			n = 1
		Case "WARNING"
			n = 2
		Case "INFORMATION"
			n = 4
		Case "AUDIT_SUCCESS"
			n = 8
		Case "AUDIT_FAILURE"
			n = 16
	End Select
	objShell.LogEvent n, msg
	Set objShell = Nothing
End Sub

Call write_event("INFORMATION","This is a test information.")

```



## zkl

{{trans|Clojure}}

```zkl
zkl: System.cmd(0'|eventcreate "/T" "INFORMATION" "/ID" "123" "/D" "Rosetta Code example"|)
```

{{out}}

```txt

SUCCESS: A 'INFORMATION' type event is created in the 'EventCreate' log/source.
0

```


{{omit from|Lotus 123 Macro Scripting|Would overwrite the log file}}
{{omit from|Mathematica}}
{{omit from|Maxima}}
{{omit from|PARI/GP}}
{{omit from|PostScript}}
{{omit from|Retro|No access to Windows APIs}}
{{omit from|Swift|No access to Windows APIs}}
{{omit from|TI-83 BASIC|Cannot run on Windows}}
{{omit from|zkl}}
{{omit from|ZX Spectrum Basic|Microdrive access is slow, and there is no real time clock, so event logging is not usually done}}
