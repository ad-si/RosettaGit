+++
title = "Talk:Hostname"
description = ""
date = 2012-06-17T21:44:10Z
aliases = []
[extra]
id = 11883
[taxonomies]
categories = []
tags = []
+++

== [[REXX]] example ==

I think this is wrong.  The userId bif does not necesarily return the hostname (although they may be the same).

The only Rexx interpreter I have to hand is ooRexx & it produces the username for userid() which is what I would expect.  Perhaps someone could test this on Regina &/or mainframe interpreters. --[[User:Sahananda|Sahananda]] 20:04, 15 June 2012 (UTC)

: I agree.  I just tried with ooRexx and Regina on Mac OS X and on CMS in z/VM.  '''&quot;<nowiki>say userid()</nowiki>&quot;''' doesn't return the host name; it returns the user ID.  (No surprise there though!) -[[User:Alansam|Alansam]] 01:46, 16 June 2012 (UTC)

: It's more normal to discuss the correctness of an example on the talk page for the task where the example is located. –[[User:Dkf|Donal Fellows]] 16:55, 16 June 2012 (UTC)
