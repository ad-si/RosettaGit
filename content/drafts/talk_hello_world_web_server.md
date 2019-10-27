+++
title = "Talk:Hello world/Web server"
description = ""
date = 2011-10-30T10:15:04Z
aliases = []
[extra]
id = 10005
[taxonomies]
categories = []
tags = []
+++

Is this supposed to be a server that does its own HTTP protocol, or just a CGI will do? --[[User:Ledrug|Ledrug]] 21:49, 30 June 2011 (UTC)
:  CGI is fine for producing the text, but starting a server program cannot be done outside of the task.  A valid solution must show how to get a server process running.  I was hoping a number of languages would be able to show off easy built-in or library support for this, but I didn't want to exclude languages where the easiest solution was to launch a copy of some existing server program.  In this case, a valid solution must both generate the text to serve (writing it to a static text file would be fine) and start a server that will serve the text. &mdash;[[User:Sonia|Sonia]] 01:10, 1 July 2011 (UTC)
:: Ok. Though if launching Apache is allowed, there's really no meaningful limit left&mdash; 
: Limit--to languages that can solve the task?  I didn't want to task to be especially limiting, but there will certainly be a few languages where solution is difficult or impossible.
::BTW, the Go code didn't seem to server HTTP headers.  Is it handled by the package, or is it not part of the requirement? --[[User:Ledrug|Ledrug]] 01:24, 1 July 2011 (UTC)
: Interesting!  This isn't an area where I know a lot, but I'll look into it.  I would expecte an HTTP server to serve whatever headers are standard for the protocol.  (I confess, I just hacked up a couple of lines of code, saw that Chrome displayed the text, and called it done.) &mdash;[[User:Sonia|Sonia]] 01:49, 1 July 2011 (UTC)

```txt

> echo -n "GET / HTTP/1.0\r\n\r\n" | nc localhost 8080
HTTP/1.0 200 OK
Content-Type: text/html; charset=utf-8
Date: Fri, 01 Jul 2011 01:59:50 GMT

Goodbye, World!

```

Should there be more? &mdash;[[User:Sonia|Sonia]] 02:02, 1 July 2011 (UTC)
: No it's fine, as long as there is 200 OK and Content-type, it's valid.  I was merely curious since if no headers are served, most clients would be pretty upset.  Thanks for the test. --[[User:Ledrug|Ledrug]] 02:13, 1 July 2011 (UTC)

== Clarfication: accepts multiple client ==

"Multiple" means "simultaneously" or "sequentially"?  I suggest leaving this up to the coders, but make it clear, since this task it self doesn't really need to involve forking or threads. --[[User:Ledrug|Ledrug]] 00:42, 2 July 2011 (UTC)
: Good point.  For Hello World, I should think sequentially should be fine.  &mdash;[[User:Sonia|Sonia]] 01:38, 2 July 2011 (UTC)

== does the task mandate HTTP/1.0 or above? ==
The task description should specify if the server should implement HTTP/1.0 or above, currently some examples implement a plain socket that returns one text line, which is possible in HTTP 0.9, but is not valid in current clients. E.g. in the Java server, curl and wget:


```txt
wget http://localhost:8080
--22:36:36--  http://localhost:8080/
           => `index.html.2'
Resolving localhost... done.
Connecting to localhost[127.0.0.1]:8080... connected.
HTTP request sent, awaiting response...
Read error (No such file or directory) in headers.
Retrying.

```


```txt
curl -i localhost:8080
curl: (56) Failure when receiving data from the peer

```


Regular browsers still handle HTTP 0.9 replies though.
:The task is supposed to be minimal, but by "browser" I was thinking a modern browser, and a solution that doesn't work with wget and curl does seem to be a stretch.  Perhaps it would make sense to add a task requirement along the lines of "Use a modern browser or client program to test your server.  Showing output is not neccessary, but state the relevant details of the program you used. (OS, version, browser, version, etc.)" &mdash;[[User:Sonia|Sonia]] 22:48, 28 October 2011 (UTC)
::my point is that the solutions neither process the request line (the reply is sent before the request is sent) nor provide the proper header (some solutions like C at least provide headers) --[[User:AlexLehm|AlexLehm]] 10:15, 30 October 2011 (UTC)
