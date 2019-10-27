+++
title = "Help talk:Syntax Highlighting"
description = ""
date = 2017-10-01T17:40:59Z
aliases = []
[extra]
id = 4056
[taxonomies]
categories = []
tags = []
+++

== [[Help:GeSHi]] ==

Is there anything anyone wants to keep from this page? If not I can redirect it to [[Help:GeSHi]]. --[[User:Mwn3d|Mwn3d]] 18:43, 5 April 2009 (UTC)
: I'd like to keep the content.  There are still some open issues.  I don't care one way or another if it's automatically redirected. --[[User:Short Circuit|Short Circuit]] 20:11, 5 April 2009 (UTC)
: I redirected that page here, since I like this page better. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 00:36, 18 December 2009 (UTC)
:: Works for me. :) --[[User:Short Circuit|Michael Mol]] 07:43, 18 December 2009 (UTC)

== Vim to Geshi translation? ==

For the language [[TXR]], I have produced syntax file that works with Vim. I don't want to maintain syntax highlighting definitions in other formats (annoying chore with no benefit other than seeing the same end result in a different application). Is there some compiler from vim syntax files to GeSHi? Maybe this could be posed as a Rosetta task. :) [[Special:Contributions/192.139.122.42|192.139.122.42]] 22:27, 1 November 2011 (UTC)
: Actually, Vim can put out colorized HTML. I'm going to experiment integrating Vim into the syntax highlighting dispatch script I use in my GIT repository to use it dynamically. There is no reason to use one highlighting engine for every file type; I already use two different ones in my repo.[[Special:Contributions/192.139.122.42|192.139.122.42]] 23:23, 1 November 2011 (UTC)
: Trying to get that to work proved elusive. Vim only does syntax highlighting properly, including HTML generation, when invoked in the context of a terminal session.[[Special:Contributions/24.85.131.247|24.85.131.247]] 06:41, 11 November 2011 (UTC)

== Rexx Family ==

The three languages of that family (Rexx, NetRexx, and ooRexx) are now listed as supported
but the coloring does not correspond to the three php files that I prepared and sent to Benny.
How can find what's used for those languages and/or how I can langchk 'my' files?

--[[User:Walterpachl|Walterpachl]] 18:59, 13 July 2012 (UTC)

== Broken When Logged In ==

I noticed that when I logged in, syntax highlighting no longer shows up.  Is there some sort of setting to be toggled to fix this? [[User:Kzh|Kzh]] 18:51, 18 September 2012 (UTC)

-- works for me ..[[User:Walterpachl|Walterpachl]] 19:29, 18 September 2012 (UTC)

== Assembly Languages ==

Let's hope this is an appropriate spot for this. The page seems dead...
The assembly languages are a mess. Besides the other issues, the lang tags, well, there's only one: '''asm'''. And it's for x86. Two ideas for cleaning it up:<ol>
<li> merge all known instructions into asm (so that asm becomes the only assembly lang tag). This might be a bureaucratic nightmare but it's bound to be done quickly and quietly.
<li> have some sort of 'inheritance' in GeSHi files (is this possible?) so that asm will define:
* common instructions that most/all assembly languages recognise (such as MOV)
* common pseudo-instructions that most assemblers recognise (such as EQU, SET, macro)
* colour definitions for instructions, comments, numbers, etc.
asm would then be extended with additional instructions by each different assembly language. There should be a standard format for them, such as asm_MIPS, asm_ARM, etc. Unrecongised lang tags should default to asm if their specific tag doesn't exist (if that's also possible).
</ol>
--[[User:Alt|Alt]] ([[User talk:Alt|talk]]) 04:36, 16 September 2014 (UTC)

:This page links to [[Rosetta_Code:Village_Pump/Syntax_Highlighting#Relationship_Between_Rosetta_Code_and_GeSHi|a note on the relationship between Rosetta Code and GeSHi]] which to me indicates that you may need to talk to the GeSHi project if you want changes made. (I'm not certain tho.) &mdash;[[User:dchapes|dchapes]] ([[User talk:dchapes|talk]] | [[Special:Contributions/dchapes|contribs]]) 16:15, 16 September 2014 (UTC)

:: First time I read that link, I thought this site used a custom compilation of GeSHi. My mistake. Sounds like any change is not that easy. Guess I gotta learn me some php... --[[User:Alt|Alt]] ([[User talk:Alt|talk]]) 04:20, 17 September 2014 (UTC)

== Could we get something like <nowiki><code lang=FOO></nowiki> ? ==

MediaWiki supports [[mw:Extension:SyntaxHighlight#inline|inline]] styles with syntax highlighting.

I don't have a clear idea about how mediawiki tags are implemented, but it seems like this would be a nice feature to have, here, if it's not too much work. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 06:07, 27 February 2016 (UTC)

== Scilab highlighting problem ==

I've just added a Scilab example to the [[Barnsley fern]] page. However, when the highlighting is enabled it adds unnecessary text to reserved words such as <code>while</code> and <code>if</code>, e.g., <code>while</code> becomes <code>scilab.org/product/dic-mat-sci/M2SCI_doc.htm">while</code>. Is there any way a regular user can fix this? If no, how should I proceed form here to get it fixed?
--[[User:Luispauloml|Luispauloml]] ([[User talk:Luispauloml|talk]]) 08:31, 2 June 2017 (UTC)

:Start here: [[Rosetta_Code:Village_Pump/Syntax_highlighting#Relationship_Between_Rosetta_Code_and_GeSHi]] --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 08:40, 2 June 2017 (UTC)

::I actually read that page before asking here. It explains about GeSHi and how to create a php file for syntax highlighting. It seems like a small fix, though. Should I really go through the whole process of creating a new style file and submitting it? Perhaps there's a easier way to report this bug. Doing something like "[[Rosetta Code:Village Pump/Syntax highlighting#MATLAB problem]]" would be considered a bug report? --[[User:Luispauloml|Luispauloml]] ([[User talk:Luispauloml|talk]]) 17:50, 2 June 2017 (UTC)

:::Presumably the style file already exists and just needs to be fixed. Poking around a bit, the help page here says that we're using Geshi 1.0.8.10 while http://qbnz.com/highlighter/ says that version 1.0.9.0 has been released. So if you poke around in the [https://github.com/GeSHi/ source] you might be able to see whether the problem has been fixed already - if so, just need to hook up with [[User:Short_Circuit]] and see if there are any issues with upgrading. Or, if it's not been fixed in 1.0.9.0, maybe it has been fixed since then... anyways, if you want this fixed in a short time you'll need to dig in a bit and maybe both see that it gets fixed in geshi for the future and also identify a minimal change that can be deployed right now. Or, if you don't have the energy for that, I guess just don't use the lang tag - use pre or whatever? Anyways, good luck... --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:32, 2 June 2017 (UTC)

== Which version of GeSHi does this site support? ==

Does anyone know whether versions of GeSHi later than 1.0.8.10 are yet supported.

As I write this, the latest stable version appears to be 1.0.9.0 and includes support for a number of languages which, although recently added, still lack syntax highlighting on this site. These languages include Kotlin, Julia, Swift and Phix.

In the case of Kotlin one can use the Scala language tag instead though this isn't an ideal solution.--[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 17:34, 30 September 2017 (UTC)

: That is an interesting question - according to the [[Special:Version|Version]] page, GeSHi is currently not actually an installed extension. [[User:Short Circuit|Short_Circuit]] would be the obvious person to talk to for more information. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 21:26, 30 September 2017 (UTC)
:: Strange it's not listed on the [[Special:Version|Version]] page but I'll ask [[User:Short Circuit|Short_Circuit]] where we are with this. Thanks.--[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 17:40, 1 October 2017 (UTC)
