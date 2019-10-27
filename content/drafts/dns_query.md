+++
title = "DNS query"
description = ""
date = 2019-07-10T09:17:59Z
aliases = []
[extra]
id = 9683
[taxonomies]
categories = []
tags = []
+++

{{task|Networking and Web Interaction}}
DNS is an internet service that maps domain names, like <code>rosettacode.org</code>, to IP addresses, like <code>66.220.0.231</code>.

Use DNS to resolve <code>www.kame.net</code> to both IPv4 and IPv6 addresses. Print these addresses.





## Ada

{{works with|GNAT GPL|Any - package Gnat.Sockets supports only IPv4 as of Jun 2011}} 

```Ada
with Ada.Text_IO;  use Ada.Text_IO;
with GNAT.Sockets; use GNAT.Sockets;

procedure DNSQuerying is

   Host         : Host_Entry_Type (1, 1);
   Inet_Addr_V4 : Inet_Addr_Type (Family_Inet);
begin

   Host         := Get_Host_By_Name (Name => "www.kame.net");
   Inet_Addr_V4 := Addresses (Host);
   Put ("IPv4: " & Image (Value => Inet_Addr_V4));
end DNSQuerying;
```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program dnsquery.s   */

/************************************/
/* Constantes                       */
/************************************/
.equ STDIN, 0         @ Linux input console
.equ STDOUT, 1        @ Linux output console

.equ EXIT,   1         @ Linux syscall END PROGRAM
.equ FORK,   2         @ Linux syscall
.equ READ,   3         @ Linux syscall
.equ WRITE,  4         @ Linux syscall
.equ OPEN,   5         @ Linux syscall
.equ CLOSE,  6         @ Linux syscall
.equ EXECVE, 0xB      @ Linux syscall
.equ PIPE,   0x2A     @ Linux syscall
.equ DUP2,   0x3F        @ Linux syscall
.equ WAIT4,  0x72     @ Linux syscall

.equ WUNTRACED,   2   @ Wait, return status of stopped child
.equ TAILLEBUFFER,  500
/*********************************/
/* Initialized data              */
/*********************************/
.data
szCarriageReturn:    .asciz "\n"
szMessFinOK:          .asciz "Fin normale du programme. \n"
szMessError:          .asciz "Error occured !!!"
szCommand:             .asciz "/usr/bin/host"  @ command host
szNameHost:            .asciz "www.kame.net"   @ string query name
.align 4
stArg1:                 .int szCommand           @ address command
                          .int szNameHost          @ address argument
                          .int 0,0                   @ zeroes

/*********************************/
/* UnInitialized data            */
/*********************************/
.bss  
.align 4
iStatusThread:      .skip 4
pipefd:               .skip 8
sBuffer:              .skip  TAILLEBUFFER
stRusage:             .skip TAILLEBUFFER
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main 
main:                                           @ entry of program 
    /* création pipe  */
    ldr r0,iAdrpipefd                          @ FDs address
    mov r7, #PIPE                               @ create pipe
    svc 0                                        @ call system Linux
    cmp r0,#0                                    @ error  ?
    blt 99f

    /* create child thread */
    mov r0,#0
    mov r7, #FORK                                @ call system
    svc #0 
    cmp r0,#0                                     @ error ?
    blt 99f
    bne parent                                  @ if <> zero r0 contains father pid
                                                  @ else is the child
/****************************************/
/*  Child thread                       */
/****************************************/
    /* redirection sysout -> pipe */ 
    ldr r0,iAdrpipefd
    ldr r0,[r0,#4]
    mov r7, #DUP2                                @ call system linux 
    mov r1, #STDOUT                             @
    svc #0
    cmp r0,#0                                    @ error ?
    blt 99f

    /* run command host      */
    ldr r0, iAdrszCommand                    @ r0 = address de "/usr/bin/host"
    ldr r1,iAdrstArg1                         @ address argument 1
    mov r2,#0
    mov r7, #EXECVE                            @ call system linux (execve)
    svc #0                                      @ if ok -> no return !!!
    b 100f                                      @ never exec this label
/****************************************/
/*  Father thread                       */
/****************************************/
parent:	
    mov r4,r0                                     @ save child pid
1:                                                @ loop child signal
    mov r0,r4
    ldr r1,iAdriStatusThread                  @ return status thread
    mov r2,#WUNTRACED                           @ flags 
    ldr r3,iAdrstRusage                        @ return structure thread
    mov r7, #WAIT4                               @ Call System 
    svc #0 
    cmp r0,#0                                    @ error ?
    blt 99f
    @ recup status 
    ldr r0,iAdriStatusThread                 @ analyse status
    ldrb r0,[r0]                                @ firest byte
    cmp r0,#0                                    @ normal end thread ?
    bne 1b                                      @ loop

    /* close entry pipe */ 
    ldr r0,iAdrpipefd
    mov r7,#CLOSE                               @ call system
    svc #0 

    /* read datas pipe */ 
    ldr r0,iAdrpipefd
    ldr r0,[r0]
    ldr r1,iAdrsBuffer                        @ buffer address
    mov r2,#TAILLEBUFFER                      @ buffer size
    mov r7, #READ                               @ call system
    svc #0 
    ldr r0,iAdrsBuffer                        @ display buffer
    bl affichageMess

    ldr r0,iAdrszMessFinOK                   @ display message Ok
    bl affichageMess
    mov r0, #0                                   @ return code
    b 100f
99:
    ldr r0,iAdrszMessError                   @ erreur
    bl affichageMess
    mov r0, #1                                   @ return code
    b 100f
100:                                            @ standard end of the program 
    mov r7, #EXIT                               @ request to exit program
    svc #0                                      @ perform the system call

iAdrszCarriageReturn:      .int szCarriageReturn
iAdrszMessFinOK:            .int szMessFinOK
iAdrszMessError:            .int szMessError
iAdrsBuffer:                 .int sBuffer
iAdrpipefd:                  .int pipefd
iAdrszCommand:              .int szCommand
iAdrstArg1:                  .int stArg1
iAdriStatusThread:         .int iStatusThread
iAdrstRusage:                .int stRusage


/******************************************************************/
/*     display text with size calculation                         */ 
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                          @ save  registres
    mov r2,#0                                      @ counter length 
1:                                                 @ loop length calculation 
    ldrb r1,[r0,r2]                               @ read octet start position + index 
    cmp r1,#0                                      @ if 0 its over 
    addne r2,r2,#1                                @ else add 1 in the length 
    bne 1b                                        @ and loop 
                                                   @ so here r2 contains the length of the message 
    mov r1,r0                                      @ address message in r1 
    mov r0,#STDOUT                                @ code to write to the standard output Linux 
    mov r7, #WRITE                                @ code call system "write" 
    svc #0                                        @ call systeme 
    pop {r0,r1,r2,r7,lr}                           @ restaur des  2 registres */ 
    bx lr                                          @ return  

```



## AutoHotkey

This code uses Windows built-in 'nslookup' command (and a temporary file):

```AutoHotkey
Url := "www.kame.net" , LogFile := "Ping_" A_Now ".log"
Runwait, %comspec% /c nslookup %Url%>%LogFile%, , hide
FileRead, Contents, %LogFile%
FileDelete, %LogFile%
RegExMatch(Contents,"Addresses:.+(`r?`n\s+.+)*",Match)
MsgBox, % RegExReplace(Match,"(Addresses:|[ `t])")
```



## Batch File


```dos
:: DNS Query Task from Rosetta Code Wiki
:: Batch File Implementation

@echo off

set "domain=www.kame.net"
echo DOMAIN: "%domain%"
echo(
call :DNS_Lookup "%domain%"
pause
exit /b

::Main Procedure
::Uses NSLOOKUP Command. Also uses a dirty "parsing" to detect IP addresses.
:DNS_Lookup [domain]

::Define Variables and the TAB Character
set "dom=%~1"
set "record="
set "reccnt=0"
for /f "delims=" %%T in ('forfiles /p "%~dp0." /m "%~nx0" /c "cmd /c echo(0x09"') do set "TAB=%%T"

setlocal enabledelayedexpansion
for /f "tokens=1* delims=:" %%x in ('nslookup "!dom!" 2^>nul') do (
    set "line=%%x"
    if /i "!line:~0,4!"=="Name" set "record=yes"
    if /i "!line:~0,5!"=="Alias" set "record="
    if "!record!"=="yes" (
        set /a reccnt+=1
        if "%%y"=="" (set "catch_!reccnt!=%%x") else (set "catch_!reccnt!=%%x:%%y")
    )
)
for /l %%c in (1,1,%reccnt%) do (
    if /i "!catch_%%c:~0,7!"=="Address" echo(!catch_%%c:*s:  =!
    if /i "!catch_%%c:~0,1!"=="%TAB%" echo(!catch_%%c:%TAB%  =!
)
endlocal
goto :EOF
```

{{Out}}

```txt
DOMAIN: "www.kame.net"

2001:200:dff:fff1:216:3eff:feb1:44d7
203.178.141.194
Press any key to continue . . .
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      name$ = "www.kame.net"
      
      AF_INET = 2
      AF_INET6 = 23
      WSASYS_STATUS_LEN = 128
      WSADESCRIPTION_LEN = 256
      
      SYS "LoadLibrary", "WS2_32.DLL" TO ws2%
      SYS "GetProcAddress", ws2%, "WSAStartup"  TO `WSAStartup`
      SYS "GetProcAddress", ws2%, "WSACleanup"  TO `WSACleanup`
      SYS "GetProcAddress", ws2%, "getaddrinfo" TO `getaddrinfo`
      
      DIM WSAdata{wVersion{l&,h&}, wHighVersion{l&,h&}, \
      \ szDescription&(WSADESCRIPTION_LEN), szSystemStatus&(WSASYS_STATUS_LEN), \
      \ iMaxSockets{l&,h&}, iMaxUdpDg{l&,h&}, lpVendorInfo%}
      
      DIM addrinfo{ai_flags%, ai_family%, ai_socktype%, ai_protocol%, \
      \      ai_addrlen%, lp_ai_canonname%, lp_ai_addr%, lp_ai_next%}
      DIM ipv4info{} = addrinfo{}, ipv6info{} = addrinfo{}
      
      DIM sockaddr_in{sin_family{l&,h&}, sin_port{l&,h&}, sin_addr&(3), sin_zero&(7)}
      DIM sockaddr_in6{sin6_family{l&,h&}, sin6_port{l&,h&}, sin6_flowinfo%, \
      \                sin6_addr&(15), sin6_scope_id%}
      
      SYS `WSAStartup`, &202, WSAdata{} TO res%
      IF res% ERROR 102, "WSAStartup failed"
      
      addrinfo.ai_family% = AF_INET
      SYS `getaddrinfo`, name$, 0, addrinfo{}, ^ipv4info{}+4 TO res%
      IF res% ERROR 103, "getaddrinfo failed"
      
      !(^sockaddr_in{}+4) = ipv4info.lp_ai_addr%
      PRINT "IPv4 address = " ;
      PRINT ;sockaddr_in.sin_addr&(0) "." sockaddr_in.sin_addr&(1) "." ;
      PRINT ;sockaddr_in.sin_addr&(2) "." sockaddr_in.sin_addr&(3)
      
      addrinfo.ai_family% = AF_INET6
      SYS `getaddrinfo`, name$, 0, addrinfo{}, ^ipv6info{}+4 TO res%
      IF res% ERROR 104, "getaddrinfo failed"
      
      !(^sockaddr_in6{}+4) = ipv6info.lp_ai_addr%
      PRINT "IPv6 address = " ;
      PRINT ;~sockaddr_in6.sin6_addr&(0) * 256 + sockaddr_in6.sin6_addr&(1) ":" ;
      PRINT ;~sockaddr_in6.sin6_addr&(2) * 256 + sockaddr_in6.sin6_addr&(3) ":" ;
      PRINT ;~sockaddr_in6.sin6_addr&(4) * 256 + sockaddr_in6.sin6_addr&(5) ":" ;
      PRINT ;~sockaddr_in6.sin6_addr&(6) * 256 + sockaddr_in6.sin6_addr&(7) ":" ;
      PRINT ;~sockaddr_in6.sin6_addr&(8) * 256 + sockaddr_in6.sin6_addr&(9) ":" ;
      PRINT ;~sockaddr_in6.sin6_addr&(10) * 256 + sockaddr_in6.sin6_addr&(11) ":" ;
      PRINT ;~sockaddr_in6.sin6_addr&(12) * 256 + sockaddr_in6.sin6_addr&(13) ":" ;
      PRINT ;~sockaddr_in6.sin6_addr&(14) * 256 + sockaddr_in6.sin6_addr&(15)
      
      SYS `WSACleanup`

```

Output:

```txt
IPv4 address = 203.178.141.194
IPv6 address = 2001:200:DFF:FFF1:216:3EFF:FEB1:44D7
```


== {{header|C}} ==
This solution uses <code>getaddrinfo()</code>, a standard function from RFC 3493. This code resembles an example from [http://www.openbsd.org/cgi-bin/man.cgi?query=getaddrinfo&apropos=0&sektion=3&manpath=OpenBSD+Current&arch=i386&format=html getaddrinfo(3)], the [[BSD]] manual page. Whereas the man page code connects to <code>www.kame.net</code>, this code only prints the numeric addresses.


```c>#include <sys/types.h

#include <sys/socket.h>
#include <netdb.h>		/* getaddrinfo, getnameinfo */
#include <stdio.h>		/* fprintf, printf */
#include <stdlib.h>		/* exit */
#include <string.h>		/* memset */

int
main()
{
	struct addrinfo hints, *res, *res0;
	int error;
	char host[NI_MAXHOST];

	/*
	 * Request only one socket type from getaddrinfo(). Else we
	 * would get both SOCK_DGRAM and SOCK_STREAM, and print two
	 * copies of each numeric address.
	 */
	memset(&hints, 0, sizeof hints);
	hints.ai_family = PF_UNSPEC;     /* IPv4, IPv6, or anything */
	hints.ai_socktype = SOCK_DGRAM;  /* Dummy socket type */

	/*
	 * Use getaddrinfo() to resolve "www.kame.net" and allocate
	 * a linked list of addresses.
	 */
	error = getaddrinfo("www.kame.net", NULL, &hints, &res0);
	if (error) {
		fprintf(stderr, "%s\n", gai_strerror(error));
		exit(1);
	}

	/* Iterate the linked list. */
	for (res = res0; res; res = res->ai_next) {
		/*
		 * Use getnameinfo() to convert res->ai_addr to a
		 * printable string.
		 *
		 * NI_NUMERICHOST means to present the numeric address
		 * without doing reverse DNS to get a domain name.
		 */
		error = getnameinfo(res->ai_addr, res->ai_addrlen,
		    host, sizeof host, NULL, 0, NI_NUMERICHOST);

		if (error) {
			fprintf(stderr, "%s\n", gai_strerror(error));
		} else {
			/* Print the numeric address. */
			printf("%s\n", host);
		}
	}

	/* Free the linked list. */
	freeaddrinfo(res0);

	return 0;
}
```



## C++

This example makes use of the Boost library. The asio bit is header-only, but requires linking boost-system (e.g. -lboost_system). The compiler may also need to be told to use C++11 semantics (e.g. -std=c++11).

```C++

#include <boost/asio.hpp>
#include <iostream>

int main() {
  int rc {EXIT_SUCCESS};
  try {
    boost::asio::io_service io_service;
    boost::asio::ip::tcp::resolver resolver {io_service};
    auto entries = resolver.resolve({"www.kame.net", ""});
    boost::asio::ip::tcp::resolver::iterator entries_end;
    for (; entries != entries_end; ++entries) {
      std::cout << entries->endpoint().address() << std::endl;
    }
  }
  catch (std::exception& e) {
    std::cerr << e.what() << std::endl;
    rc = EXIT_FAILURE;
  }
  return rc;
}

```



## C sharp

Implementation takes a host name string as a parameter, and returns the IP addresses in a comma-delimited string.  Note that a failed lookup throws a SocketException. 

```csharp

        private string LookupDns(string s)
        {
            try
            {
                System.Net.IPHostEntry ip = System.Net.Dns.GetHostEntry(s);

                string result = ip.AddressList[0].ToString();

                for (int i = 1; i < ip.AddressList.Length; ++i)
                    result += ", " + ip.AddressList[i].ToString();

                return result;
            }
            catch (System.Net.Sockets.SocketException se)
            {
                return se.Message;
            }
        }

```


=={{header|Caché ObjectScript}}==


```cos

Class Utils.Net Extends %RegisteredObject
{

ClassMethod QueryDNS(pHost As %String, Output ip As %List) As %Status
{
	// some initialisation
	K ip S ip=""
 
	// check host operating system and input parameters
	S OS=$SYSTEM.Version.GetOS()
	I '$LF($LB("Windows","UNIX"), OS) Q $$$ERROR($$$GeneralError, "Not implemented.")
	I OS="Windows" S cmd="nslookup "_pHost
	I OS="UNIX" S cmd="host "_pHost
	I $MATCH(pHost, "^([a-zA-Z0-9]([a-zA-Z0-9\-]{0,61}[a-zA-Z0-9])?\.)+[a-zA-Z]{2,6}$")=0 {
		Q $$$ERROR($$$GeneralError, "Invalid host name.")
	}
	
	// invoke command
	S list=##class(Utils.OS).Call(cmd, 0)
	
	// iterate through list
	S ptr=0, skip=1
    WHILE $LISTNEXT(list,ptr,value) {
	    I value="" CONTINUE
	    I skip, OS="Windows" S skip=$S(value["Name:": 0, 1: 1) CONTINUE
	    S ipv4=..GetIPAddr("ipv4", value) I $L(ipv4) S $LI(ip, 4)=ipv4
	    S ipv6=..GetIPAddr("ipv6", value) I $L(ipv6) S $LI(ip, 6)=ipv6
    }
	
	// finished
	I $LD(ip, 4)=0, $LD(ip, 6)=0 Q $$$ERROR($$$GeneralError, "Lookup failed.")
	QUIT $$$OK
}

ClassMethod GetIPAddr(pType As %String = "", pValue As %String = "") As %String
{
	I pType="ipv4" {
		S pos=$LOCATE(pValue, "((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.|$)){4}")
		I pos Q $P($E(pValue, pos, *), " ")
	}
	I pType="ipv6" {
		S pos=$LOCATE(pValue, "([0-9A-Fa-f]{0,4}:){2,7}([0-9A-Fa-f]{1,4}$|((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)(\.|$)){4})")
		I pos Q $P($E(pValue, pos, *), " ")
	}
	QUIT ""
}

}

```



```cos

Class Utils.OS Extends %RegisteredObject
{

ClassMethod Call(cmd, echo = 1) As %List
{
	// instatiate pipe object
	S list=""
	S pipe=##class(%File).%New(cmd)
		
	TRY {
		// 
		S sc=pipe.Open("QR")
		I $$$ISERR(sc) Q
		
		// read queue/pipe and output to screen
		DO {
			K len S line=pipe.ReadLine(.len) I len=-1 Q
			S $LI(list,$I(pos))=line
			I echo W line,!
		} WHILE $G(pos)<1000
		
	} CATCH {
		S ZE=$ZE
		BREAK
	}
	
	// close pipe
	D pipe.Close()
	
	// return list value
	Q list
}

}

```

{{out|Examples}}

```txt

USER>Do ##class(Utils.Net).QueryDNS("www.kame.net", .ip)
USER>Write "IPv4 address = ", $ListGet(ip, 4, "Not found")
IPv4 address = 203.178.141.194
USER>Write "IPv6 address = ", $ListGet(ip, 6, "Not found")
IPv6 address = 2001:200:dff:fff1:216:3eff:feb1:44d7

USER>Do ##class(Utils.Net).QueryDNS("ipv6.google.com", .ip) 
USER>Write "IPv4 address = ", $ListGet(ip, 4, "Not found")
IPv4 address = Not found
USER>Write "IPv6 address = ", $ListGet(ip, 6, "Not found")
IPv6 address = 2a00:1450:400c:c05::93

```



## Clojure


```clojure
(import java.net.InetAddress java.net.Inet4Address java.net.Inet6Address)

(doseq [addr (InetAddress/getAllByName "www.kame.net")] 
  (cond
    (instance? Inet4Address addr) (println "IPv4:" (.getHostAddress addr))
    (instance? Inet6Address addr) (println "IPv6:" (.getHostAddress addr))))
```

Output:

```txt
IPv4: 203.178.141.194
IPv6: 2001:200:dff:fff1:216:3eff:feb1:44d7
```



## CoffeeScript


```coffeescript

# runs under node.js
dns = require 'dns'

dns.resolve4 'www.kame.net', (err, addresses) ->
  console.log 'IP4'
  console.log addresses
  
dns.resolve6 'www.kame.net', (err, addresses) ->
  console.log 'IP6'
  console.log addresses

```



## Common Lisp

common lisp does not have a standard network api. the following examples are using native implementations
{{works with|SBCL}}

```lisp
(sb-bsd-sockets:host-ent-addresses 
      (sb-bsd-sockets:get-host-by-name "www.rosettacode.org"))
(#(71 19 147 227))
```

{{works with|CMUCL}}

```lisp
(let ((hostname (extensions:lookup-host-entry "www.rosettacode.org")))
                (print (map 'list #'extensions:ip-string (host-entry-addr-list hostname))))
("71.19.147.227")
```

{{works with|Clozure}}

```lisp
(ipaddr-to-dotted (lookup-hostname "www.rosettacode.org"))
"104.28.10.103"
```

{{works with|Clisp}}

```lisp
(hostent-addr-list (resolve-host-ipaddr "www.rosettacode.org"))
("104.28.11.103" "104.28.10.103")
```

the usocket library contains a (get-hosts-by-name) function in all of its [http://trac.common-lisp.net/usocket/browser/usocket/trunk/backend backends]. unfortunately it does not expose the functions in its public interface. but since the license is MIT, it may be a suitable source to copy code for your own use.

{{libheader|iolib}} is a portable library that:
{{works with|SBCL}}{{works with|CMUCL}}{{works with|Clisp}}{{works with|Clozure}}

```lisp
(iolib:lookup-hostname "www.kame.net" :ipv6 t)

#/IOLIB.SOCKETS:IP/203.178.141.194
(#/IOLIB.SOCKETS:IP/2001:200:dff:fff1:216:3eff:feb1:44d7)
"orange.kame.net"
(("orange.kame.net" . #/IOLIB.SOCKETS:IP/203.178.141.194)
 ("orange.kame.net" . #/IOLIB.SOCKETS:IP/2001:200:dff:fff1:216:3eff:feb1:44d7))
```


In Allegro Common Lisp there's a nice standard library called [http://franz.com/support/documentation/current/doc/socket.htm socket].
{{works with|Allegro}}

```lisp
(socket:ipaddr-to-dotted
 (socket:dns-query "www.rosettacode.org"))
"104.28.10.103"
```


In Lispworks the [http://www.lispworks.com/documentation/lw71/LW/html/lw-269.htm COMM] package provides information about IP addresses.

{{works with|Lispworks}}

```lisp
(require "comm")
(comm:ip-address-string (comm:get-host-entry "www.rosettacode.org" :fields '(:address)))
"104.28.10.103"

```



## Crystal


```ruby
require "socket"

Socket::Addrinfo.resolve(
	"www.kame.net",
	80,
	type: Socket::Type::STREAM
).each { |a|
	puts a.ip_address.address
}
```

{{out}}

```txt

203.178.141.194
2001:200:dff:fff1:216:3eff:feb1:44d7

```



## D


```d
import std.stdio, std.socket;

void main() {
    auto domain = "www.kame.net", port = "80";

    auto a = getAddressInfo(domain, port, AddressFamily.INET);
    writefln("IPv4 address for %s: %s", domain, a[0].address);

    a = getAddressInfo(domain, port, AddressFamily.INET6);
    writefln("IPv6 address for %s: %s", domain, a[0].address);
}
```


```txt
IPv4 address for www.kame.net: 203.178.141.194:80
IPv6 address for www.kame.net: [2001:200:dff:fff1:216:3eff:feb1:44d7]:80
```



## Delphi

The included Indy components wrap GetAddressInfo.


```Delphi
program DNSQuerying;

{$APPTYPE CONSOLE}

uses
  IdGlobal, IdStackWindows;

const
  DOMAIN_NAME = 'www.kame.net';
var
  lStack: TIdStackWindows;
begin
  lStack := TIdStackWindows.Create;
  try
    Writeln(DOMAIN_NAME);
    Writeln('IPv4: ' + lStack.ResolveHost(DOMAIN_NAME));
    Writeln('IPv6: ' + lStack.ResolveHost(DOMAIN_NAME, Id_IPv6));
  finally
    lStack.Free;
  end;
end.
```


Output:

```txt
www.kame.net
IPv4: 203.178.141.194
IPv6: 2001:200:DFF:FFF1:216:3EFF:FEB1:44D7
```



## Erlang


```Erlang

33> {ok, {hostent, Host, Aliases, AddrType, Bytes, AddrList}} = inet:gethostbyname("www.kame.net", inet). 
{ok,{hostent,"orange.kame.net",
             ["www.kame.net"],
             inet,4,
             [{203,178,141,194}]}}
34> [inet_parse:ntoa(Addr) || Addr <- AddrList].                                                         
["203.178.141.194"]
35> f().                                                                                                 
ok
36> {ok, {hostent, Host, Aliases, AddrType, Bytes, AddrList}} = inet:gethostbyname("www.kame.net", inet6).
{ok,{hostent,"orange.kame.net",[],inet6,16,
             [{8193,512,3583,65521,534,16127,65201,17623}]}}
37> [inet_parse:ntoa(Addr) || Addr <- AddrList].                                                          
["2001:200:DFF:FFF1:216:3EFF:FEB1:44D7"]

```



## Factor


```factor
USING: dns io kernel sequences ;

"www.kame.net" [ dns-A-query ] [ dns-AAAA-query ] bi
[ message>names second print ] bi@
```

{{out}}

```txt

203.178.141.194
2001:200:dff:fff1:216:3eff:feb1:44d7

```



## Go


```go
package main

import (
    "fmt"
    "net"
)

func main() {
    if addrs, err := net.LookupHost("www.kame.net"); err == nil {
        fmt.Println(addrs)
    } else {
        fmt.Println(err)
    }
}
```

Output:

```txt

[203.178.141.194 2001:200:dff:fff1:216:3eff:feb1:44d7]

```



## Groovy


```groovy
def addresses = InetAddress.getAllByName('www.kame.net')
println "IPv4: ${addresses.find { it instanceof Inet4Address }?.hostAddress}"
println "IPv6: ${addresses.find { it instanceof Inet6Address }?.hostAddress}"
```

Output:

```txt
IPv4: 203.178.141.194
IPv6: 2001:200:dff:fff1:216:3eff:feb1:44d7
```


== {{header|Haskell}} ==

```haskell
module Main where

import Network.Socket

getWebAddresses :: HostName -> IO [SockAddr]
getWebAddresses host = do
  results <- getAddrInfo (Just defaultHints) (Just host) (Just "http")
  return [ addrAddress a | a <- results, addrSocketType a == Stream ]

showIPs :: HostName -> IO ()
showIPs host = do
  putStrLn $ "IP addresses for " ++ host ++ ":"
  addresses <- getWebAddresses host
  mapM_ (putStrLn . ("  "++) . show) addresses
  
main = showIPs "www.kame.net"

```

Output:

```txt

IP addresses for www.kame.net:
  203.178.141.194:80
  [2001:200:dff:fff1:216:3eff:feb1:44d7]:80

```


==Icon and {{header|Unicon}}==

The following was tested only in Unicon:


```unicon

procedure main(A)
   host := gethost( A[1] | "www.kame.net") | stop("can't translate")
   write(host.name, ": ", host.addresses)
end

```


Sample Run:


```txt

--> dns
orange.kame.net: 203.178.141.194

--> dns www.gogole.com
www.gogole.com: 74.125.225.183,119.119.119.46,74.125.225.184


```



## J


J currently doesn't have a native DNS implementation, and this task doesn't seem to be asking for us to implement DNS on top of UDP+TCP (a full implementation of DNS has to fall back to TCP for messages which cannot fit in a UDP packet).

Also, there's currently an issue with IPv6 DNS servers not being reachable, so that query did not resolve.

Anyways:


```J
   2!:0'dig -4 +short www.kame.net'
orange.kame.net.
203.178.141.194

   2!:0'dig -6 +short www.kame.net'
|interface error
|       2!:0'dig -6 +short www.kame.net'
```


Put differently: in the IPv4 DNS system, www.kame.net is currently a CNAME record for orange.kame.net which had the address 203.178.141.194.

And, as mentioned above, the IPv6 DNS system was not reachable.

== {{header|Java}} ==

```java
import java.net.InetAddress;
import java.net.Inet4Address;
import java.net.Inet6Address;
import java.net.UnknownHostException;

class DnsQuery {
    public static void main(String[] args) {
        try {
            InetAddress[] ipAddr = InetAddress.getAllByName("www.kame.net");
            for(int i=0; i < ipAddr.length ; i++) {
                if (ipAddr[i] instanceof Inet4Address) {
                    System.out.println("IPv4 : " + ipAddr[i].getHostAddress());
                } else if (ipAddr[i] instanceof Inet6Address) {
                    System.out.println("IPv6 : " + ipAddr[i].getHostAddress());
                }
            }
        } catch (UnknownHostException uhe) {
            System.err.println("unknown host");
        }
    }
}

```

Output:

```txt

IPv4 : 203.178.141.194
IPv6 : 2001:200:dff:fff1:216:3eff:feb1:44d7

```


== {{header|JavaScript}} ==

```JavaScript
const dns = require("dns");

dns.lookup("www.kame.net", {
             all: true
          }, (err, addresses) => {
              if(err) return console.error(err);
              console.log(addresses);
          })

```

Output:

```txt

[ { address: '203.178.141.194', family: 4 },
  { address: '2001:200:dff:fff1:216:3eff:feb1:44d7', family: 6 } ]

```




## Julia

As entered at the REPL command line:

```julia

julia> using Sockets

julia> getaddrinfo("www.kame.net")
ip"203.178.141.194"

julia> getaddrinfo("www.kame.net", IPv6)
ip"2001:200:dff:fff1:216:3eff:feb1:44d7"


```



## Kotlin


```scala
// version 1.1.3

import java.net.InetAddress
import java.net.Inet4Address
import java.net.Inet6Address

fun showIPAddresses(host: String) {
    try {
        val ipas = InetAddress.getAllByName(host)
        println("The IP address(es) for '$host' is/are:\n")
        for (ipa in ipas) {
            print(when (ipa) {
                is Inet4Address -> "  ipv4 : "
                is Inet6Address -> "  ipv6 : "
                else            -> "  ipv? : "
            })
            println(ipa.hostAddress)
        }
    }
    catch (ex: Exception) {
        println(ex.message)
    } 
}

fun main(args: Array<String>) {
    showIPAddresses("www.kame.net")
}
```


{{out}}

```txt

The IP address(es) for 'www.kame.net' is/are:

  ipv4 : 203.178.141.194
  ipv6 : 2001:200:dff:fff1:216:3eff:feb1:44d7

```



## Lasso

The DNS lookup methods in Lasso do not support IPv6 addresses at this time, only IPv4.

```Lasso
dns_lookup('www.kame.net', -type='A')
```



## Lua

{{works with|Lua|5.1 - 5.3}}
{{libheader|LuaSocket}}


```lua
local socket = require('socket')
local ip_tbl = socket.dns.getaddrinfo('www.kame.net')

for _, v in ipairs(ip_tbl) do
  io.write(string.format('%s: %s\n', v.family, v.addr))
end

```

{{out}}

```txt

inet: 203.178.141.194
inet6: 2001:200:dff:fff1:216:3eff:feb1:44d7

```




## Neko

Neko does not yet support ipv6.  ipv4 addresses are returned as Int32.


```ActionScript
/* dns in neko */
var host_resolve = $loader.loadprim("std@host_resolve", 1);
var host_to_string = $loader.loadprim("std@host_to_string", 1);
var host_reverse = $loader.loadprim("std@host_reverse", 1);

var ip = host_resolve("www.kame.net");

$print("www.kame.net: ", ip, ", ", host_to_string(ip), "\n");
$print(host_to_string(ip), ": ", host_reverse(ip), "\n");
```


{{out}}

```txt
prompt$ nekoc dns.neko
prompt$ neko dns
www.kame.net: -1030901045, 203.178.141.194
203.178.141.194: orange.kame.net
```


== {{header|NetRexx}} ==

```netrexx

/* NetRexx */
options replace format comments java crossref symbols nobinary

ir = InetAddress
addresses = InetAddress[] InetAddress.getAllByName('www.kame.net')
loop ir over addresses
  if ir <= Inet4Address then do
    say 'IPv4 :' ir.getHostAddress
    end
  if ir <= Inet6Address then do
    say 'IPv6 :' ir.getHostAddress
    end
  end ir

```

;Output

```txt

IPv4 : 203.178.141.194
IPv6 : 2001:200:dff:fff1:216:3eff:feb1:44d7

```


== {{header|NewLISP}} ==

```newLISP


(define (dnsLookup site , ipv)
    ;; captures current IPv mode
    (set 'ipv (net-ipv))
    ;; IPv mode agnostic lookup
    (println "IPv4: " (begin (net-ipv 4) (net-lookup site)))
    (println "IPv6: " (begin (net-ipv 6) (net-lookup site)))
    ;; returns newLISP to previous IPv mode
    (net-ipv ipv)
)

(dnsLookup "www.kame.net")

```

;Output

```txt

IPv4: 203.178.141.194
IPv6: 2001:200:dff:fff1:216:3eff:feb1:44d7

```



## Nim



```nim
import nativesockets

iterator items(ai: ptr AddrInfo): ptr AddrInfo =
  var current = ai
  while current != nil:
    yield current
    current = current.aiNext

proc main() =
  let addrInfos = getAddrInfo("www.kame.net", Port 80, AfUnspec)
  defer: freeAddrInfo addrInfos

  for i in addrInfos:
    echo getAddrString i.aiAddr

when isMainModule: main()
```


{{out}}

```txt
203.178.141.194
2001:200:dff:fff1:216:3eff:feb1:44d7
```


=={{header|Oberon-2}}==
{{Works with|oo2c version 2}}
IO:Address module supports only IPv4

```oberon2

MODULE DNSQuery;
IMPORT
  IO:Address,
  Out := NPCT:Console;

PROCEDURE Do() RAISES Address.UnknownHostException;
VAR
  ip: Address.Inet;
BEGIN
  ip := Address.GetByName("www.kame.net");
  Out.String(ip.ToString());Out.Ln
END Do;

BEGIN
  Do;
END DNSQuery.

```

{{Out}}

```txt

203.178.141.194

```


## Objeck


```objeck
use System.IO.Net;

class Rosetta {
  function : Main(args : String[]) ~ Nil {
    resoloved := TCPSocket->Resolve("www.kame.net");
    each(i : resoloved) {
      resoloved[i]->PrintLine();
    };
  }
}
```


Output:

```txt

203.178.141.194

```



## OCaml



```ocaml
let dns_query ~host ~ai_family =
  let opts = [
    Unix.AI_FAMILY ai_family;
    Unix.AI_SOCKTYPE Unix.SOCK_DGRAM;
  ] in
  let addr_infos = Unix.getaddrinfo host "" opts in
  match addr_infos with
  | [] -> failwith "dns_query"
  | ai :: _ ->
    match ai.Unix.ai_addr with
    | Unix.ADDR_INET (addr, _) -> (Unix.string_of_inet_addr addr)
    | Unix.ADDR_UNIX addr -> failwith "addr_unix"

let () =
  let host = "www.kame.net" in
  Printf.printf "primary addresses of %s are:\n" host;

  Printf.printf " IPv4 address: %s\n" (dns_query host Unix.PF_INET);
  Printf.printf " IPv6 address: %s\n" (dns_query host Unix.PF_INET6);
;;
```



## Perl

Unfortunately IPv6 was only added to Perl 5.14, a relatively new version.

```perl
require 5.014;    # Older versions can't resolve IPv6 with just core Socket module

use Socket qw(getaddrinfo getnameinfo);
my ($err, @res) = getaddrinfo("www.kame.net", 0,
                { protocol=>Socket::IPPROTO_TCP } );
die "getaddrinfo error: $err" if $err;

print getnameinfo($_->{addr}, Socket::NI_NUMERICHOST), "\n" for @res

```



## Perl 6

{{works with|Rakudo|2017.01}}


```perl6
use Net::DNS;

my $resolver = Net::DNS.new('8.8.8.8');

my $ip4 = $resolver.lookup('A',    'orange.kame.net');
my $ip6 = $resolver.lookup('AAAA', 'orange.kame.net');

say $ip4[0].octets.join: '.';
say $ip6[0].octets.».fmt("%.2X").join.comb(4).join: ':';
```

{{out}}

```txt
203.178.141.194
2001:0200:0dff:fff1:0216:3eff:feb1:44d7
```



## Phix

Translated from C/MSDN/several man pages.

<b>NB:</b> may warrant further testing, see output.

```Phix
include builtins\cffi.e

constant AF_UNSPEC = 0,
--       AF_INET = 2,
--       AF_INET6 = 23,
--       SOCK_STREAM = 1,
         SOCK_DGRAM = 2,
--       IPPROTO_TCP = 6,
         NI_MAXHOST = 1025,
         NI_NUMERICHOST = iff(platform()=LINUX?1:2)

constant tWAD = """
typedef struct WSAData {
  WORD           wVersion;
  WORD           wHighVersion;
  char           szDescription[257];
  char           szSystemStatus[129];
  unsigned short iMaxSockets;
  unsigned short iMaxUdpDg;
  char           *lpVendorInfo;
} WSADATA, *LPWSADATA;
""",
tWAS = """
int WSAStartup(
  _In_   WORD wVersionRequested,
  _Out_  LPWSADATA lpWSAData
);
""",
tWAC = """
int WSACleanup(void);
""",
tAI_W="""
typedef struct addrinfo {
  int             ai_flags;
  int             ai_family;
  int             ai_socktype;
  int             ai_protocol;
  size_t          ai_addrlen;
  char            *ai_canonname;
  struct sockaddr  *ai_addr;
  struct addrinfo  *ai_next;
} ADDRINFOA, *PADDRINFOA;
""",
tAI_L="""
typedef struct addrinfo {
    int              ai_flags;
    int              ai_family;
    int              ai_socktype;
    int              ai_protocol;
    int              ai_addrlen;
    struct sockaddr *ai_addr;
    char            *ai_canonname;
    struct addrinfo *ai_next;
};
""",
tGAI = """
int getaddrinfo(
  _In_opt_  PCSTR pNodeName,
  _In_opt_  PCSTR pServiceName,
  _In_opt_  const ADDRINFOA *pHints,
  _Out_     PADDRINFOA *ppResult
);
""",
--int getaddrinfo(const char *node, const char *service,
--                     const struct addrinfo *hints,
--                     struct addrinfo **res);
tGNI = """
int getnameinfo(
  _In_   sockaddr *sa,
  _In_   int salen,
  _Out_  char *host,
  _In_   DWORD hostlen,
  _Out_  char *serv,
  _In_   DWORD servlen,
  _In_   int flags
);
""",
--int getnameinfo(const struct sockaddr *addr, socklen_t addrlen,
--                     char *host, socklen_t hostlen,
--                     char *serv, socklen_t servlen, int flags);
tFAI = """
void freeaddrinfo(
  _In_  struct addrinfo *ai
);
"""
--void freeaddrinfo(struct addrinfo *res);

integer xgetaddrinfo = NULL, xgetnameinfo, xfreeaddrinfo, idAI,
        xwsastartup, xwsacleanup, error

function get_name_info(string fqdn)
    if xgetaddrinfo=NULL then
        atom lib
        if platform()=WINDOWS then
            integer idWAD = define_struct(tWAD)
            atom pWAD = allocate_struct(idWAD,cleanup:=true)
            lib = open_dll("Ws2_32.dll")
            xwsastartup = define_cffi_func(lib,tWAS)
            xwsacleanup = define_cffi_func(lib,tWAC)
            error = c_func(xwsastartup,{#00020002,pWAD})
            if error then ?9/0 end if
            idAI = define_struct(tAI_W)
        elsif platform()=LINUX then
            lib = open_dll("libc.so.6")
            idAI = define_struct(tAI_L)
        end if
        xgetaddrinfo = define_cffi_func(lib,tGAI)
        xgetnameinfo = define_cffi_func(lib,tGNI)
        xfreeaddrinfo = define_cffi_proc(lib,tFAI)
    end if
    atom hints = allocate_struct(idAI,cleanup:=true), 
         res = allocate(machine_word(),cleanup:=true),
         host = allocate(NI_MAXHOST,cleanup:=true)
    set_struct_field(idAI,hints,"ai_family",AF_UNSPEC)
--  set_struct_field(idAI,hints,"ai_socktype",SOCK_STREAM)
    set_struct_field(idAI,hints,"ai_socktype",SOCK_DGRAM)
    error = c_func(xgetaddrinfo,{fqdn,NULL,hints,res})
    if error then ?9/0 end if
    res = peekNS(res,machine_word(),false)
    atom ptr = res
    sequence results = {}
    while ptr!=NULL do
        atom addr = get_struct_field(idAI,ptr,"ai_addr")
        integer len = get_struct_field(idAI,ptr,"ai_addrlen")
        error = c_func(xgetnameinfo,{addr, len, host, NI_MAXHOST, NULL, 0, NI_NUMERICHOST})
        if error then ?9/0 end if
        results = append(results,peek_string(host))
        ptr = get_struct_field(idAI,ptr,"ai_next")
    end while
    c_proc(xfreeaddrinfo,{res})
    return results
end function
 
procedure WSACleanup()
    if platform()=WINDOWS then
        error = c_func(xwsacleanup,{})
        if error then crash("WSACleanup failed: %d\n",{error}) end if
    end if
end procedure

?get_name_info("www.kame.net")
WSACleanup()
```

{{out}}
Note that windows nslookup shows an IPv6 that this does not, whereas 
the exact reverse is true for linux on a VirtualBox (same machine)...

```txt

Windows:
>nslookup www.kame.net
Addresses:  2001:200:dff:fff1:216:3eff:feb1:44d7
          203.178.141.194
>p test
{"203.178.141.194"}

Linux:
$ nslookup www.kame.net
Address: 203.178.141.194
$ ./p test
{"203.178.141.194","2001:200:dff:fff1:216:3eff:feb1:44d7"}

```



## PHP

Works for PHP5 (Windows > 5.3.0)

```php
<?php
  $ipv4_record = dns_get_record("www.kame.net",DNS_A);
  $ipv6_record = dns_get_record("www.kame.net",DNS_AAAA);
  print "ipv4: " . $ipv4_record[0]["ip"] . "\n";
  print "ipv6: " . $ipv6_record[0]["ipv6"] . "\n";
?>
```



## PicoLisp


```PicoLisp
(make
   (in '(host "www.kame.net")
      (while (from "address ")
         (link (till "^J" T)) ) ) )
```

Output:

```txt
-> ("203.178.141.194" "2001:200:dff:fff1:216:3eff:feb1:44d7")
```



## Pike



```Pike

> array ips = Protocols.DNS.gethostbyname("www.kame.net")[1] || ({});
> write(ips*"\n");

```

Output:

```txt
203.178.141.194
2001:0200:0DFF:FFF1:0216:3EFF:FEB1:44D7
```



## PowerShell


```powershell
$DNS = Resolve-DnsName -Name www.kame.net
Write-Host "IPv4:" $DNS.IP4Address "`nIPv6:" $DNS.IP6Address
```


== {{header|Python}} ==

```python>>>
 import socket
>>> ips = set(i[4][0] for i in socket.getaddrinfo('www.kame.net', 80))
>>> for ip in ips: print ip
...
2001:200:dff:fff1:216:3eff:feb1:44d7
203.178.141.194
```



## R


R has no built-in function to accomplish the task, but with the help of the <tt>Rcpp</tt> package, it's possible to implement this functionality.

If the following is saved as <tt>dns.cpp</tt>:


```cpp

#include <Rcpp.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

using namespace Rcpp ;

// [[Rcpp::export]]
CharacterVector getNameInfo(std::string fqdn) {

  struct addrinfo hints, *res, *res0;
	int error;
	char host[NI_MAXHOST];

  memset(&hints, 0, sizeof hints);
	hints.ai_family = PF_UNSPEC;
	hints.ai_socktype = SOCK_DGRAM;

	error = getaddrinfo(fqdn.c_str(), NULL, &hints, &res0);
	if (error) { return(NA_STRING);	}

  int i = 0 ;
	for (res = res0; res; res = res->ai_next) {
  	error = getnameinfo(res->ai_addr, res->ai_addrlen,
		    host, sizeof host, NULL, 0, NI_NUMERICHOST);
		if (!error) { i++ ; }
	}

  CharacterVector results(i) ;

  i = 0;

  for (res = res0; res; res = res->ai_next) {
		error = getnameinfo(res->ai_addr, res->ai_addrlen,
		    host, sizeof host, NULL, 0, NI_NUMERICHOST);
		if (!error) { results[i++] = host ; }
	}

  freeaddrinfo(res0);

  return(results) ;

}

```


It can be used to perform the task in R:


```R

library(Rcpp)
sourceCpp("dns.cpp")
getNameInfo("www.kame.net")
## [1] "203.178.141.194"                     
## [2] "2001:200:dff:fff1:216:3eff:feb1:44d7"

```



## Racket


The following finds an IPv4 address. Currently, the API does not support returning an IPv6 address.


```racket

#lang racket

(require net/dns)
(dns-get-address "8.8.8.8" "www.kame.net")

```



## REXX

This REXX version uses the Windows (DOS)   '''PING'''   command to resolve the domain name.

Execution note:   if the information for     '''PING   -6   ···'''     is blank, you may need to install   (on some 
Microsoft Windows systems)   the IPV6 interface using the command:     ''' IPV6   install '''

```rexx
/*REXX program displays  IPV4 and IPV6  addresses for a supplied  domain name.*/
parse arg tar .                        /*obtain optional domain name from C.L.*/
if tar==''  then tar= 'www.kame.net'   /*Not specified?  Then use the default.*/
tFID    = '\TEMP\DNSQUERY.$$$'         /*define temp file to store IPV4 output*/
pingOpts= '-l 0    -n 1    -w 0'   tar /*define options for the PING command. */
trace off                              /*don't show PING none─zero return code*/
                                       /* [↓]  perform 2 versions of PING cmd.*/
  do j=4  to 6  by 2                   /*handle  IPV4  and  IPV6  addresses.  */
  'PING'  (-j)  pingOpts  ">"   tFID   /*restrict PING's output to a minimum. */
  q=charin(tFID, 1, 999)               /*read the output file from  PING  cmd.*/
  parse var  q   '['   ipaddr    "]"   /*parse  IP  address from the output.  */
  say 'IPV'j "for domain name  "  tar  '  is  '  ipaddr        /*IPVx address.*/
  call lineout tFID                    /* ◄──┬─◄  needed by some REXXes to    */
  end   /*j*/                          /*    └─◄  force (TEMP) file integrity.*/
                                       /*stick a fork in it,  we're all done. */
'ERASE'  tFID                          /*clean up (delete) the temporary file.*/
```

'''output'''   using the default input:

```txt

IPV4 for domain name   www.kame.net   is   203.178.141.194
IPV6 for domain name   www.kame.net   is   2001:200:dff:fff1:216:3eff:feb1:44d7

```


== {{header|Ruby}} ==

```ruby
irb(main):001:0> require 'socket'
=> true
irb(main):002:0> Addrinfo.getaddrinfo("www.kame.net", nil, nil, :DGRAM) \
irb(main):003:0*   .map! { |ai| ai.ip_address }
=> ["203.178.141.194", "2001:200:dff:fff1:216:3eff:feb1:44d7"]
```



## Rust


```rust
use std::net::ToSocketAddrs;

fn main() {
    let host = "www.kame.net";
    // Ideally, we would want to use std::net::lookup_host to resolve the host ips,
    // but at time of writing this, it is still unstable. Fortunately, we can
    // still resolve using the ToSocketAddrs trait, but we need to add a port,
    // so we use the dummy port 0.
    let host_port = (host, 0);
    let ip_iter = host_port.to_socket_addrs().unwrap();


    for ip_port in ip_iter {
        println!("{}", ip_port.ip());
    }
}
```


Output:
```txt
203.178.141.194
2001:200:dff:fff1:216:3eff:feb1:44d7
```



## Scala

{{libheader|Scala}}

```Scala
import java.net._

InetAddress.getAllByName("www.kame.net").foreach(x => println(x.getHostAddress))
```



## Scheme

{{works with|Guile}}

```scheme
; Query DNS
(define n (car (hostent:addr-list (gethost "www.kame.net"))))

; Display address as IPv4 and IPv6
(display (inet-ntoa n))(newline)
(display (inet-ntop AF_INET n))(newline)
(display (inet-ntop AF_INET6 n))(newline)
```

Output:
```txt
203.178.141.194
203.178.141.194
::203.178.141.194
```



## Seed7

The library [http://seed7.sourceforge.net/libraries/socket.htm socket.s7i] defines the function
[http://seed7.sourceforge.net/libraries/socket.htm#inetSocketAddress%28in_string,__in_integer%29 inetSocketAddress], which
returns an IPv4 address. It only returns an IPv6 address, when a host has no IPv4 address and the operating system supports IPv6.
The function [http://seed7.sourceforge.net/libraries/socket.htm#numericAddress%28in_socketAddress%29 numericAddress]
is used to get the IP address of the specified host.

```seed7
$ include "seed7_05.s7i";
  include "socket.s7i";

const proc: main is func
  begin
    writeln(numericAddress(inetSocketAddress("www.kame.net", 1024)));
  end func;
```


Output:

```txt

203.178.141.194

```



## Sidef


```ruby
var (err, *res) = Socket.getaddrinfo(
        'www.kame.net', 0,
        Hash.new(protocol => Socket.IPPROTO_TCP)
);
err && die err;
res.each { |z|
    say [Socket.getnameinfo(z{:addr}, Socket.NI_NUMERICHOST)][1];
}
```


{{out}}

```txt

203.178.141.194
2001:200:dff:fff1:216:3eff:feb1:44d7

```



## Tcl

While Tcl does internally do address resolution, it doesn't directly support looking up addresses without actually connecting to them. This is left to support packages to provide. An additional complexity is that while the DNS protocol itself is defined to work with both TCP and UDP as the transport layer, Tcl only supports TCP sockets by default (because they have a stream model) whereas DNS is typically only deployed with UDP enabled.
{{tcllib|dns}}
{{libheader|udp}}

```tcl
package require udp;  # Query by UDP more widely supported, but requires external package
package require dns

set host "www.kame.net"
set v4 [dns::resolve $host -type A];    # Specifically get IPv4 address
set v6 [dns::resolve $host -type AAAA]; # Specifically get IPv6 address
while {[dns::status $v4] eq "connect" || [dns::status $v6] eq "connect"} {
    update; # Let queries complete
}
puts "primary addresses of $host are:\n\tIPv4» [dns::address $v4]\n\tIPv6» [dns::address $v6]"
```

Output:

```txt

primary addresses of www.kame.net are:
	IPv4» 203.178.141.194
	IPv6» 2001:200:dff:fff1:216:3eff:feb1:44d7

```



## VBScript


```vb

Function dns_query(url,ver)
	Set r = New RegExp
	r.Pattern = "Pinging.+?\[(.+?)\].+"
	Set objshell = CreateObject("WScript.Shell")
	Set objexec = objshell.Exec("%comspec% /c " & "ping -" & ver & " " & url)
	WScript.StdOut.WriteLine "URL: " & url
	Do Until objexec.StdOut.AtEndOfStream
		line = objexec.StdOut.ReadLine
		If r.Test(line) Then
			WScript.StdOut.WriteLine "IP Version " &_
				ver & ": " & r.Replace(line,"$1")
		End If
	Loop
End Function

Call dns_query(WScript.Arguments(0),WScript.Arguments(1))

```


{{Out}}

```txt

F:\>cscript /nologo dns_query.vbs www.kame.net 4
URL: www.kame.net
IP Version 4: 203.178.141.194

F:\>cscript /nologo dns_query.vbs www.kame.net 6
URL: www.kame.net
IP Version 6: 2001:200:dff:fff1:216:3eff:feb1:44d7

```



## zkl

The addrInfo method is just a front end to POSIX getaddrinfo.

```zkl
zkl: Network.TCPClientSocket.addrInfo("www.kame.net")
L("orange.kame.net",L("203.178.141.194","2001:200:dff:fff1:216:3eff:feb1:44d7"))
```



{{omit from|PARI/GP|No network access.}}
{{omit from|Mathematica}}
{{omit from|Maxima}}
{{omit from|Lotus 123 Macro Scripting}}
{{omit from|Locomotive Basic|Does not support Internet Protocol.}}
{{omit from|ML/I}}
{{omit from|Retro}}
{{omit from|TI-83 BASIC|No network access.}}
{{omit from|ZX Spectrum Basic|Does not support Internet Protocol.}}
{{omit from|Commodore BASIC}}

[[Category:Internet Protocol (Multiprotocol)]]
