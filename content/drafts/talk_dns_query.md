+++
title = "Talk:DNS query"
description = ""
date = 2011-08-27T15:18:51Z
aliases = []
[extra]
id = 9694
[taxonomies]
categories = []
tags = []
+++

== Task definition ==

Is this task just about address lookups, or are we to look up more information besides? For example, MX records (which I note <tt>www.kame.net</tt> doesn't have…) –[[User:Dkf|Donal Fellows]] 12:21, 17 May 2011 (UTC)
: Additional record types would certainly be interesting. I can see about adding each of A, AAAA, MX and SRV records to rosettacode.org. (A and MX should already be there. SRV is the next one likely to be implemented, as i get SIP set up. AAAA is going to require me to do a bit more setup wrt IPv6, and thus probably won't happen for a couple weeks, at least. --[[User:Short Circuit|Michael Mol]] 15:11, 17 May 2011 (UTC)

:: I found that Rosetta Code had no DNS tasks, so I created this one. If rosettacode.org would have AAAA record, we can change this task to use rosettacode.org, but keeping the old examples that use www.kame.net. We might want to have other tasks to query MX, or query ANY, or do reverse DNS. --[[User:Kernigh|Kernigh]] 22:05, 17 May 2011 (UTC)

== Differentiate between IP4 and IPV6 ==

Would it be better to split the IP4 and IPV6 into separate tasks here? Or maybe require the code sections to be suitably commented so that we can see which sections of code apply to each of the protocols.

[[User:Markhobley|Markhobley]] 20:50, 17 May 2011 (UTC)

:It would probably be worth creating a separate task for getting an ipv4 ip address.  However, the technique you would use there need not be the technique used here (think gethostbyname vs. getaddrinfo), and having two different queries to contrast might be informative.  --[[User:Rdm|Rdm]] 21:36, 17 May 2011 (UTC)

:: Some programs want to support both IPv4 and IPv6. These programs get a list of addresses. Then the clients try each address, or the servers bind all addresses from the list. This is why I want both IPv4 and IPv6 in one task. A separate IPv4-only task might be a good place to use <code>gethostbyname()</code> or languages that know only IPv4. --[[User:Kernigh|Kernigh]] 22:05, 17 May 2011 (UTC)

::: Current best practice is to only use getaddrinfo (when not talking DNS directly) with gethostbyname being a legacy interface. As such, I'd consider this task to have two levels of completion: '''Basic''' (getting ''an'' address for the host, any address family) and '''Extended''' (getting both the IPv4 and the IPv6 addresses). However, both are “querying for host addresses” so it's still a solution of the task to do the basic version. Consider phrasing as “extra credit”. –[[User:Dkf|Donal Fellows]] 14:43, 18 May 2011 (UTC)
