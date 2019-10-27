+++
title = "Talk:Remove duplicate elements"
description = ""
date = 2010-02-06T12:56:10Z
aliases = []
[extra]
id = 4041
[taxonomies]
categories = []
tags = []
+++

==APL example==
Encodings problem of the APL example? Where's the Unique operator (it appears in the text, but not in the code) --[[User:ShinTakezou|ShinTakezou]] 13:54, 31 March 2009 (UTC)
:GeSHi doesn't do APL correctly. It does seem to be an encoding problem. If you see it elsewhere, change the lang tags to pre tags for now. Maybe it will be fixed in future versions of GeSHi. --[[User:Mwn3d|Mwn3d]] 18:29, 13 July 2009 (UTC)
:: Not a GeSHi problem, a ''PHP'' problem.  Believe it or not, PHP has problems with UTF-8.  Will be fixed in PHP 6, but in the mean time, best-practices methods that do things like escape html-sensitive entities break.  That's what turned out to be the root cause of the moose issue, too. --[[User:Short Circuit|Short Circuit]] 22:55, 13 July 2009 (UTC)

== Does Order Matter ==

If order matters, then there are some solutions here that will not suffice. If it doesn't matter, it should be made explicit.
:I'm gonna guess that order shouldn't matter. The focus of the task is uniqueness. I didn't make the task though so I don't know, but my vote is for it not mattering. --[[User:Mwn3d|Mwn3d]] 18:30, 13 July 2009 (UTC)

::The allowed use of a hash makes order unimportant. --[[User:Paddy3118|Paddy3118]] 03:04, 7 October 2009 (UTC)

== Bash ==

Does anybody know how to do it with Bash (or any other shell)? 
for example, if I start with "-I/usr/include -I/usr/include -I/usr/other" I would like to get "-I/usr/include -I/usr/other"

: sort -u will take a line delimited list and eliminate non-unique lines.  The trick for your example is that you have everything on one line.  But you can use fmt to split lines and xargs to reassemble them:

 $ echo -I/usr/include -I/usr/include -I/usr/other | fmt -sw1 | sort -u | xargs echo 
 -I/usr/include -I/usr/other

== APL ==

The "Works with APL2" code looks a wee bit mucked up:

w←1 2 3 1 2 3 4 1
     ((⍳⍨w)=⍳⍴w)/w
1 2 3 4

I think it should be ((w {iota} w) = {iota}{rho}w)/w

where {iota} is the APL iota symbol
and   {rho}  is the APL rho  symbol
— (Unsigned by [[User:69.254.121.12]],  02:41, 2 February 2010)
