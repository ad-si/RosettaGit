+++
title = "Talk:URL parser"
description = ""
date = 2016-07-22T13:21:10Z
aliases = []
[extra]
id = 19436
[taxonomies]
categories = []
tags = []
+++

== LDAP URL non-conformant ==

Great task!  The example URLs provide good coverage, but the example <tt> ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two </tt> is invalid per RFC2255.  For solutions exercising library code that knows about more URL structures than HTTP, this is distracting.  I suggest replacing it with the example in RFC3986:  <tt> ldap://[2001:db8::7]/c=GB?objectClass?one </tt> which is just as parseable under HTTP rules, but won't blow up a parser that understands the ldap scheme.

--[[User:Aspectcl|Aspectcl]] ([[User talk:Aspectcl|talk]]) 03:59, 24 July 2015 (UTC)

: That matches my reading of rfc2255 also. I'd say go for it (and mark the existing implementations with a task description updated tag). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 14:36, 26 July 2015 (UTC)

== hostname for arbitrary schemes ==

The introduction claims that arbitrary URI schemes (eg. foo://) should parse with full hostname, etc.  However, [[User:Choroba]] claimed that "you don't get the host from the foo:// scheme, as host is only valid for schemes that define it." ([[URL parser#Perl|Perl section intro]]).  One of these is incorrect, and should be amended. --[[User:Sondra.kinsey|Sondra.kinsey]] ([[User talk:Sondra.kinsey|talk]]) 16:00, 21 July 2016 (UTC)

Yes, [https://tools.ietf.org/html/rfc3986#appendix-A appendix A of rfc 3986] defines a different syntax than the simplified statement at the beginning of this task. This is a flaw in this task's description, and someone should replace the incorrect statements in the task description with a rendition of that appendix A (and after that any task implementations which don't do it right should be fixed, or marked as incorrect). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 13:20, 22 July 2016 (UTC)
