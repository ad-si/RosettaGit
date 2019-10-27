+++
title = "Talk:Parse an IP Address"
description = ""
date = 2018-07-09T18:31:39Z
aliases = []
[extra]
id = 10583
[taxonomies]
categories = []
tags = []
+++

==IP address in decimal==
Hm. I'm thinking the decimal number requirement isn't necessary. Removing. --[[User:Short Circuit|Michael Mol]] 12:35, 27 September 2011 (UTC)

: Yes, in all my many years, I've never seen a complete (full) IP address in decimal: it's either in hex or dotted decimal: <tt> w.x.y.z </tt>
: I only included the decimal version of an IP address (for the REXX language) because it was shown in decimal a couple of places in the task description. -- [[User:Gerard Schildberger|Gerard Schildberger]] 14:45, 23 April 2012 (UTC)

== Sloppy ==

This task currently seems sloppy.

First, it requires that extra data (the port) be ignored.  In practical use, if the port is being passed to a routine that parses ip addresses, this probably means the data is being mishandled.  So good practice suggests that this be treated as an error case.  This is a minor but annoying complexity in the task.

Second, it asks us to mix ipv4 and ipv6 addresses but ipv4 addresses have a representation as ipv6 addresses (see rfc 2373, for example) but the example implementation mixes them in the same data structure without labeling the type and without using a consistent mapping.

I can think of several ways to go here:

# Discard the port parsing requirement
# Ask for ip,port to be reported as a pair
# Ask for ipv4 and ipv6 address results to be distinguished (or put them in separate tasks)
# Ask for the ipv4 addresses to be encoded as ipv6 addresses (127.0.0.1 becomes ::127.0.0.1 to distinguish it from ::7f00:1).
# change the task to make some of these issues moot

But I am not sure which way to go here. --[[User:Rdm|Rdm]] 13:58, 28 September 2011 (UTC)
: The trouble is, connection targets (IP/port combinations) are frequently passed around in ''string'' format with the version undeclared. Many programs which need to operate with IPv4 and IPv6 addresses actually have difficulty with it, even though the standardized formats are documented. Correctly converting that string representation into a logical representation is what this task is intended to be about. The inclusion of both IPv4 and IPv6 notation, as well as the inclusion ":portNumber", is intentional, to attempt reflect the full complexity of the problem. So what remains is likely a poorly chosen task title and an an insufficiently-described task. I have extreme difficulty being clear on the latter point, though. --[[User:Short Circuit|Michael Mol]] 14:27, 28 September 2011 (UTC)

:: The string format is unambiguous.  But result format is ambiguous, here.  --[[User:Rdm|Rdm]]
::: Ah, I see now. I only saw the output format of the task to be relevant in that it required comprehension of the input format to generate. The internal representation (in particular, correct distinction of host and port addresses) was what I was more interested in. Requiring the port address in the output helps a reviewer understand how the port number was understood. Change the requirements for the output format however you see fit, with that requirement in mind. (I'd try changing it, but I've been without my glasses for a week, and doing things on the computer has been very, very error-prone--even trying to write coherently. )--[[User:Short Circuit|Michael Mol]] 15:15, 28 September 2011 (UTC)

: I don't understand (above) the statement saying why the "extra data" (the port) is required to be ignored (or is being mishandled).  I see no problem with parsing/displaying it.  If it can be parsed, it's shouldn't be an error to include it in the IP address. -- [[User:Gerard Schildberger|Gerard Schildberger]] 15:00, 23 April 2012 (UTC)

:: Retrospect from a few years later: my sentence "First, it requires that extra data (the port) be ignored." was unclear and ambiguous. This task was, I imagine, aimed at being a crude model of a web browser (except without any dns names or user names, etc.). It makes an odd sort of sense from that perspective. But see the next section for where this conversation drifted, back then. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:17, 11 May 2014 (UTC)


###  Port 0 

The 'bit specifying if port was given' doesn't seem necessary, how about just use port 0? --[[User:Ledrug|Ledrug]] 15:37, 28 September 2011 (UTC)
: Specifying port 0 means "any available", but only in some contexts. A more general solution should be preferred, IMO. I took the "bit specifying" bit as "an indicator", rather than as an explicit like-formatted boolean bit. The way I read it, it could be equally (and possibly more clearly) written as "a note specifying if port was given". --[[User:Short Circuit|Michael Mol]] 15:40, 28 September 2011 (UTC)

:I have (after several not-so-good attempts) done my best making the result format unambiguous and complete for this task.  Let me know what you think.
:Meanwhile, as Michael Mol points out, port 0 can be different from no port specified.  (For example, when configuring an http server using sockets, port 0 would mean "let the OS pick an unassigned port to listen on" while no port would mean "listen on port 80 or fail if it's already in use".)  --[[User:Rdm|Rdm]] 15:47, 28 September 2011 (UTC)
:: The only part I don't understand is "In languages where variant result types are clumsy". --[[User:Short Circuit|Michael Mol]] 15:48, 28 September 2011 (UTC)
::: How would you represent the required results in C, for example?  --[[User:Rdm|Rdm]] 15:49, 28 September 2011 (UTC)
:::: How would it be any different from a console-output-capable language that wasn't C? --[[User:Short Circuit|Michael Mol]] 15:50, 28 September 2011 (UTC)
::::: In languages which have variable length tuples as a native type(like Pico Lisp) returning a port number or omitting a port number is trivial.  You can either return (addr-family ip-address port) or (addr-family ip-address).  In a language like C, it's much more natural to use something like a struct with a fixed memory layout.  Of course, whether this issue propagates to a textual representation of the result is a different issue...  But currently the task is asking for a result and a textual representation of that result would be for illustrative purposes.  --[[User:Rdm|Rdm]] 16:21, 28 September 2011 (UTC)
:::::: So the "In languages where variant result types are clumsy" bit is about how an example represents data internally, then. I think I understand, then. Not certain it's necessary, though. In answer to your question, the address would probably be represented using something like a [http://msdn.microsoft.com/en-us/library/windows/desktop/ms737530(v=VS.85).aspx sockaddr] structure.--[[User:Short Circuit|Michael Mol]] 16:48, 28 September 2011 (UTC)
::::::: I do not believe sockaddr distinguishes between port 0 and default port (which makes that particular issue a sore subject -- this design decision pushes complexity into the surrounding context).  --[[User:Rdm|Rdm]] 16:59, 28 September 2011 (UTC)

: TCP and UDP never use port 0. Sockets use port 0 to mean "port not specified". The meaning of "port not specified" changes with context. With a server socket, during a bind() to "port not specified", system chooses a default port in range 49152 to 65535. With a server socket, port 0 is same as "port not specified". --[[User:Kernigh|Kernigh]] 22:13, 28 September 2011 (UTC)
:: Yes, but "port not specified" will typically mean "use the default port for that protocol" in any server that serves a single protocol. --[[User:Rdm|Rdm]] 00:19, 29 September 2011 (UTC)
::: More to the point, we're not talking about TCP or UDP, but IP. --[[User:Short Circuit|Michael Mol]] 13:56, 29 September 2011 (UTC)
:::: We are indeed talking about IP addresses, but the task also asks us to deal with ports, and IP does not have ports without something like TCP or UDP.  --[[User:Rdm|Rdm]] 14:10, 29 September 2011 (UTC)
::::: Fair enough. (And I wouldn't want to remove the port-awareness requirement, because of its impact on parsing.) --[[User:Short Circuit|Michael Mol]] 14:12, 29 September 2011 (UTC)

==format description of IP addresses==

I think it would be a good idea to provide a link (or links) describing the various (legal) formats of an IP address. -- [[User:Gerard Schildberger|Gerard Schildberger]] 14:06, 23 April 2012 (UTC)

==other IP address formats==

I noticed that at least two (or more) other formats for IP address aren't represented in the examples (used for input):

* "slash" or CIDR or netmask format: <tt> w.x.y.z/25 </tt> for instance.
* IPv4-compatible address: <tt> 0:0:0:0:0:0:w.x.y.z </tt> or <tt> ::w.x.y.z </tt>
* IPv4-mapped address: <tt> 0:0:0:0:0:FFFF:w.x.t.z </tt> or <tt> ::FFFF:w,x,y,z </tt>  
* 6to4 address: <tt> 2002::/16 </tt> described in RFC 3056.
* ISATAP address (Intra-Site Automatic Tunnel Addressing Protocol): <tt> :::5EFE:w.x.y.z </tt> and also <tt> FE80::/64 </tt> described in RFC 4214.
* Teredo address: <tt> 2001::/32 </tt> described in RFC 4380.
[All of this is way over my comprehension level.] 

I see nothing detailed above that would exclude these from adding them as examples (for input). 
-- [[User:Gerard Schildberger|Gerard Schildberger]] 14:06, 23 April 2012 (UTC)

: I'd ignore anything with a <code>/</code> in it; they specify ''networks'', not individual addresses. Similarly, I also wouldn't worry about the v4-in-v6 magic. It's enough to handle normal IPv4 (in conventional dotted quad form) and IPv6 (with usual abbreviating, as described by the RFC on that). –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 16:38, 11 May 2014 (UTC)
