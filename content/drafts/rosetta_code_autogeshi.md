+++
title = "Rosetta Code:AutoGeSHi"
description = ""
date = 2015-08-18T07:15:46Z
aliases = []
[extra]
id = 7171
[taxonomies]
categories = []
tags = []
+++

''(Moved from [[Category talk:J]] and then from [[Rosetta Code:Village Pump/Syntax Highlighting]])''

While we're on the subject, I would be interested in a programmatic approach to generating these files. The structure of the GeSHi language files is very, very simple; It's little more than a PHP-native serialization of a few regex setrings and symbol constants.  If GeSHi supported JSON for that structure, it would be trivial to import language highlighting as a JSON file, and such a file would be trivial to generate programmatically.  But GeSHi doesn't, so I'm stuck with PHP files until I (or someone else) writes a JSON->PHP conversion.  That said, a number of folks have sent me language files, and so are familiar with its structure. Would anyone be interested in writing a webform-driven language-file ''generator''?  For security's sake, I can't automate the import of the generated files, but it would greatly open up the process of generating the files, and perhaps make maintenance easier. I'd give it a subdomain such as geshi.rosettacode.org. --[[User:Short Circuit|Michael Mol]] 03:33, 10 November 2009 (UTC)
:Hm, I think I could write a CGI script (in Perl 5) to let folks fill out a form, perhaps with some minimal markup, to create a language definition. With careful use of resource limits and sanitization of the input, we could even let the user test the new definition without a local copy of PHP. But I'm not sure how using such a Web application would actually be easier or quicker than writing the literal definition. I mean, one of the things that makes writing new definitions so easy is that you can use a preexisting definition of a similar language as a starting point. Can you describe in more detail the interface you're imagining? —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 13:23, 10 November 2009 (UTC)
:: It's been ages since I've had a chance to look at the structure, but an editable list for each of the list-type members would be good.  I don't know what to do about the regex-driven ones; A wizard would be sweet, but I don't know that that would be possible.
:: I could provide a MySQL backend for persistence  That would make it plausible to tweak existing support.
:: Not sure how far to go as far as sanitation and execution. The more secure I try make it, the more time I'll need to spend responding to problems. --[[User:Short Circuit|Michael Mol]] 16:09, 10 November 2009 (UTC)
::: How about this: sometime in the near future, I'll write the simplest such program that could possibly be useful. Then I'll take feature requests, or anybody who wants to can submit patches. How does that sound? Could you give me FTP access to geshi.rosettacode.org, or some other way of controlling what appears there? —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 23:28, 10 November 2009 (UTC)
:::: Sounds like a plan. I'll send you an email regarding connectivity. --[[User:Short Circuit|Michael Mol]] 00:17, 11 November 2009 (UTC)
: Okay, everyone, a basic implementation is up at http://rosettacode.org/geshi. The source is available upon request. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 23:16, 17 November 2009 (UTC)
:: Here are some notes about the current version:
::* You could add some more instructions for filling the field. For example, what is used as separator character (a space?).
::* The current version only has one keyword group. If you could add at least 2nd keyword group, then it would be easier to add more groups by editing the resulting file.
::Will the form be inserted within a normal wiki page so that there will be navigation links?
::--[[User:PauliKL|PauliKL]] 16:04, 16 December 2009 (UTC)
:::* I figured the program would be easier to use if the user could just deduce the syntax from the example input rather than reading a detailed description of it. For instance, I think that the example provided for COMMENTS_MULTI (<code>/* */, {- -}</code>) is much easier to understand than an explanation like "separate the two delimiters of each pair with whitespace and separate each pair with a comma and optionally some whitespace". Tell me if there's a particular detail that you think could use explicit explanation.
:::* Not a bad idea; I think I'll make that change when I get the opportunity. (I'm using a public computer running Windoze at the moment.)
:::* It appears that MediaWiki blithely ignores <code><form></code>, unfortunately. Are there any particular links you'd like to see on <code>rosettacode.org/geshi</code>? —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 17:40, 16 December 2009 (UTC)
:::: If it is not possible to embedd the form into Rosetta Code navigation frame, then maybe just add a link to the main page. And maybe a link to GeSHi site page that describes the format of the syntax file (I once found the page but I have lost the link). --[[User:PauliKL|PauliKL]] 12:23, 23 December 2009 (UTC)
::::: Okay, now, there are a few links at the top of the page, and you can define multiple keyword groups. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 14:57, 23 December 2009 (UTC)
::: For the form to be inserted, it would probably best be done as a Special page MediaWiki extension, which would require the extension to be written in PHP. A couple possibilities come to mind for marshalling from PHP to AutoGeSHi (which is Perl).  Probably the best (least work for me, least invasiveness to MediaWiki processes) way would be if AutoGeSHi could export a descriptor of the fields, field types, field labels, form target and method, the extension would build the form, and the Submit button would pass the data back to AutoGeSHi. --[[User:Short Circuit|Michael Mol]] 19:49, 16 December 2009 (UTC)
:::: Actually, AutoGeSHi doesn't produce the form; it only processes the input. rosettacode.org/geshi/index.html is a static HTML document that I hand-wrote. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 20:47, 16 December 2009 (UTC)
Currently there's a language file creation tool under construction for GeSHi itself that will be included in upcoming releases and will offer basic support for writing language files. This tool isn't finished yet though. --[[User:BenBE|BenBE]] 13:58, 6 January 2010 (UTC)

The link http://rosettacode.org/geshi is not found. -[[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 07:15, 18 August 2015 (UTC)
