+++
title = "Talk:BNF Grammar"
description = ""
date = 2009-12-09T09:35:20Z
aliases = []
[extra]
id = 4415
[taxonomies]
categories = []
tags = []
+++

{{alertbox|#eeeeee|This task has been deprecated. Discussion of why is on this page, in [[#Task?|this section]].}}
==What's with all the empty header sections?==
No language will find this page on their "unimplemented tasks" page. --[[User:Glennj|glennj]] 11:40, 21 June 2009 (UTC)
: Fixed. --[[User:Kevin Reid|Kevin Reid]] 12:41, 21 June 2009 (UTC)

==This is not WORKING==
I got the following error:
:Fatal error: Allowed memory size of 33554432 bytes exhausted (tried to allocate 398947 bytes) in /var/www/rosettacode.org/public/w/includes/memcached-client.php on line 956
The information, I think, might best be added to each languages category page, e.g. as a section in [[:Category:C]] for C. --[[User:Paddy3118|Paddy3118]] 09:10, 21 June 2009 (UTC)
::Perhaps, assuming we want the information at all, it should be split up like the large tasks such as [[RCBF]] and [[RCRPG]]? --[[User:Kevin Reid|Kevin Reid]] 12:43, 21 June 2009 (UTC)
:::I think it needs to be formatted as RCBF and RCRPG, ''however'', the root page should be [[:Category:lang]] for any given page.  One question, though: ''Where is this data coming from?'''  Is it being written from scratch, or is it being copied?  If it's being copied, then we need to establish cross-license permission to allow us to relicense under the GFDL. In any case, the page needs to be broken up; If a high-traffic site links to a fully-populated version of this, the server will go down due to overload. --[[User:Short Circuit|Short Circuit]] 18:13, 21 June 2009 (UTC)


Ouch! Impedance mismatch. Is it just me, or does this entry read like no other on RC? --[[User:Paddy3118|Paddy3118]] 05:17, 21 June 2009 (UTC)
:It may well be me :-)   --[[User:Paddy3118|Paddy3118]] 05:17, 21 June 2009 (UTC)
::I agree -- this doesn't seem to fit. It's possibly useful information, and so perhaps it should be somewhere, but I don't think it's a ''task''. Also, it feels like one person's work to create/unify these grammars; they seem to have a common style in my brief skimming. --[[User:Kevin Reid|Kevin Reid]] 12:41, 21 June 2009 (UTC)
:::It falls under encyclopedic and supporting information, if nothing else, and is somewhat akin to lists of implementations.  However, I would be particularly pleased if there were a way to break it down into something with direct chrestomathic utility. --[[User:Short Circuit|Short Circuit]] 18:13, 21 June 2009 (UTC)

This page is getting kind of big... seems to be crashing.  I was moving the BNF for c, and can't put it back, so here it is: --[[User:Tinku99|Tinku99]] 08:09, 21 June 2009 (UTC)

: I've found error (BF eaten C#), and formatting oddities (browser-related? maybe not), like a huge amount of void space after C# and at least one more. --[[User:ShinTakezou|ShinTakezou]] 15:08, 21 June 2009 (UTC)

OK, Mea Culpa. I moved the C block to Categeory:C page. I'll move the rest appropriately if it makes sense. I'm new here and wanted to see how things work. Most of the content is from ''Gold Parsing System'' http://www.devincook.com/goldparser/ but I have contributed the Perl block. I would like to know how copyrights are negotiated. I feel this information falls in the ''spirit'' of the programming chrestomathy and the intent of the Rosetta Stone (kin of Rosetta Code) which was translation. Greek and Coptic grammar were well known 
before Champollion could translate heiroglyphics.
Below is the Gold Freeware Licence Agreement,a ''zlib/libpng Open Source License Agreement'':

```txt

This software is provided 'as-is', without any expressed or implied warranty. In no
event will the author(s) be held liable for any damages arising from the use of this software.
Permission is granted to anyone to use this software for any purpose. If you use
this software in a product, an acknowledgment in the product documentation would be deeply
appreciated but is not required.

In the case of the GOLD Parser Engine source code, permission is granted
to anyone to alter it and redistribute it freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not claim that you wrote
the original software. 
2. Altered source versions must be plainly marked as such, and must not be misrepresented
as being the original software. 
3. This notice may not be removed or altered from any source distribution. 

```

--[[User:rlrandallx|rlrandallx]] 14:00, 21 June 2009 (UTC)

:: It should be checked if this is compatible with GFDL. Maybe it is, and so citing the source from where the bnfs come should be enough; but I am not very good at this law-related stuffs. --[[User:ShinTakezou|ShinTakezou]] 22:02, 22 June 2009 (UTC)
::: It may be, but impractically so.  While clause 3 might be considered akin to the GFDL's "invariant section", clause 2 is impractically difficult to maintain compliance with in a wiki environment, as edit history isn't presented as part of normal page display.  And is pretty much lost in a page export or other flattening.  And should Rosetta Code adopt a more permissive license in the future (GFDL isn't even compatible with [L]GPL), maintaining compliance will be that much harder.
::: There are three options, from what I can see.  First, we can drop the content altogether.  Second, we can "clean-room" it, by writing from-scratch versions.  Third, we can seek special permission from the original authors.  Option three has worked well for us in the past, and should be pursued; Referring them to  [[Rosetta_Code:Copyrights#Exemptions]] may help.  Meanwhile, the content as is should be removed from Rosetta Code, as it's currently out of compliance. --[[User:Short Circuit|Short Circuit]] 00:45, 23 June 2009 (UTC)
::::Some of these may be available on language websites (for instance, Java's entire grammar is available [http://java.sun.com/docs/books/jls/third_edition/html/j3TOC.html here] with explanations). Would it be ok to just link to them from here? That'll automatically give credit to whoever they want. --[[User:Mwn3d|Mwn3d]] 18:22, 23 June 2009 (UTC)
:::::Linking to the BNF grammer is an excellent option, and could conceivably even be done as part of the language template.  I strongly recommend pointing to a reputable mirror, however, either a Google Cache page or archive.org. (Submit the link to the Wayback Machine if necessary, but keep in mind it can take six months before something will appear in their archives.)  I wouldn't want the information to disappear simply because the originator of the content did. --[[User:Short Circuit|Short Circuit]] 23:01, 23 June 2009 (UTC)

== Task? ==

This is not a task. Moreover, being the grammars hardly works made by "us", we should check if their inclusion here is compatible with the GFDL. I suppose it should be so for any lang, but never say never. --[[User:ShinTakezou|ShinTakezou]] 15:05, 21 June 2009 (UTC)
: I find myself very much in agreement at this point; This task has gone from writing something descriptive or illustrative of the language to a list of off-site links. The task should be dismantled and the relevant links be put on the language's individual pages.  For cases where we have the actual BNF code, with appropriate licensing, they can be moved to subpages of their relevant language. --[[User:Short Circuit|Michael Mol]] 23:40, 6 December 2009 (UTC)
:: +1 --[[User:Paddy3118|Paddy3118]] 05:41, 7 December 2009 (UTC)

== Tcl's BNF ==

Though it is possible to define a BNF grammar for Tcl, I have chosen to not do so. This is because the level at which such a grammar operates is so low level that nobody thinking about the language actually uses it. Instead, to understand the language requires understanding a higher level of processing which is the currently defined set of commands in the language, all of which can be replaced with something else and all of which have total freedom to reinterpret their arguments any way they want. In many real ways, Tcl is fundamentally a context-sensitive language and not a context-free one, and BNF is not the right tool for describing it.

FWIW, the portion of the language that can be described in a context-free way is the language basic parser, as documented in [http://www.tcl.tk/man/tcl8.6/TclCmd/Tcl.htm Tcl(n)], but that is only one very small part of the whole. To write a BNF in for this “task” would be a sham. —[[User:Dkf|Donal Fellows]] 22:39, 21 June 2009 (UTC)
