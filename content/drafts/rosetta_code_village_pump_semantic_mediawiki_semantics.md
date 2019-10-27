+++
title = "Rosetta Code:Village Pump/Semantic MediaWiki/Semantics"
description = ""
date = 2010-11-28T17:10:52Z
aliases = []
[extra]
id = 8175
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Semantics
|summary=Semantic properties used on Rosetta Code
}}
We need a place to document the ontologies and semantic properties we're working on around the site. For new, let's let this be the place. There have been discussions about SMW properties in various places, and some of that discussion should be mentioned or documented here. (Also, it would probably be good to have a category for tracking these, to aid documentation.) --[[User:Short Circuit|Michael Mol]] 14:23, 29 August 2010 (UTC)

: See [[:Category:Property]] and <nowiki>{{</nowiki>[[:Template:Property|property]]<nowiki>}}</nowiki>. –[[User:Dkf|Donal Fellows]] 18:43, 29 August 2010 (UTC)

I don't know what property set will ultimately prove to be the most useful. So long as properties and queries don't cause conflict with each other, I have no problem if people want to try taking multiple approaches to find what works best. --[[User:Short Circuit|Michael Mol]] 18:04, 29 August 2010 (UTC)

=Ontologies=
==Task Page Construction==
(Moved from [[Talk:Hough transform]] (--[[User:Short Circuit|Michael Mol]] 18:04, 29 August 2010 (UTC)))

I'm going to use <s>this task</s>[[Hough transform]] (--[[User:Short Circuit|Michael Mol]] 18:04, 29 August 2010 (UTC)) as a guinea pig for the new Semantic MediaWiki features. --[[User:Short Circuit|Michael Mol]] 10:07, 27 July 2010 (UTC)
: Ok, so looking at it, I'm thinking:
* '''satisfies the Hough transform task'''' is a property of the Tcl code example. ''There should be a way to make this imply that it satisfies the Hough transform task, but I don't know what it is.''
* '''uses Tk''' is a property of the Tcl code example
* '''demonstrates (Tcl code example identifier)''' would be a property of the demonstration code.
* '''uses Tk''' is a property of the demonstration code. ''I'm sure there's a way to tie the library version range in there, but I don't know what it is.''
* '''uses TkImg''' is a property of the demonstration code
* '''uses (support image identifier)''' is a property of that support image
* '''supports the Hough transform task''' is a property of that support image
* '''explicitly requires the Hough transform''' is a property of the Hough Transform task
* '''example output of (demonstration code)''' is a property of that demonstration code's example 
: Those are the properties I'm seeing. I could use some help figuring out how to apply them. --[[User:Short Circuit|Michael Mol]] 10:50, 27 July 2010 (UTC)

: Undid edit. I don't know why Coderjoe's template didn't work. I saw it working elsewhere... --[[User:Short Circuit|Michael Mol]] 21:35, 19 August 2010 (UTC)

==Languages, libraries, implementations==
(Moved from [[User talk:EdK]] (--[[User:Short Circuit|Michael Mol]] 18:04, 29 August 2010 (UTC)))

I really appreciate all the work you're doing with language, implementation and library association and documentation. I've got some ideas about associating those pages within the context of Semantic MediaWiki. Would you be interested in hashing this out and helping get it implemented? (It should only involve making changes to wiki pages, much like what you're already doing.)

My thoughts are:
* Distinguish between library, API and bindings between libraries and bindings to a language. That way:
** An example said to use, e.g. PyGTK could be programattically assumed to use GTK, and an example said to use Xcb could be assumed to use X11.
** An example using GLib could be assumed to be using OpenGL
** GLUT is both an API and a library, but the original has long lapsed. There are replacements, though, and they could all be assumed to be implementations of GLUT.
** The ISO89 C Runtime has an API spec, and any library/C language binding that correctly implements it could be reasoned by SMW to provide that runtime, and so any C example that uses an API spec could be reasoned to be an example use of those libraries.

That's the gist of the idea, for now. Does this strike your interest? --[[User:Short Circuit|Michael Mol]] 17:28, 13 August 2010 (UTC)

I think I understand. 

For example:
* pyGame is a binding of SDL to Python. SDL in turn uses OpenGL. This information should be captured somehow.
* There are several differant impementations of MPI, some of them have several language bindings. This should be captured.

but I am not sure why it matters what implementation of ISO89 is being used. They are generaly closly tied to the compiler, so specifing the compiler would generaly specify the library. Or am I misssing something? 

I'm not exactly sure how to do this in this contect. [[User:EdK|-EdK]] 20:40, 19 August 2010 (UTC)
:* I think the pyGame->SDL->OpenGL can be captured. [[User:Ce]] and [[User:Coderjoe]] have been doing a lot of work in sussing out the specifics of how SMW operates.
:* I agree about the MPI considerations. It's rather similar to the variations between BASIC, FORTH, SQL or LISP implementations, when you look at it in a particular way. Each is similar, but significantly vary between implementations to the point where some implementations are considered to be their own languages. I'd ''love'' to find a way to decently capture that!
:* I don't know that the particular version of the ISO89 runtime has practical value, but it ''is'' a distinction in how code may operate between compilers that purport to implement it, due to compiler-specific bugs and omissions. (If it were possible to break down the standard/implementation relationship by implemented/unimplemented/buggy components, I'd certainly prefer ''that!'') --[[User:Short Circuit|Michael Mol]] 15:55, 20 August 2010 (UTC)
