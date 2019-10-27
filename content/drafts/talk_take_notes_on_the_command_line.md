+++
title = "Talk:Take notes on the command line"
description = ""
date = 2013-03-02T19:19:31Z
aliases = []
[extra]
id = 7383
[taxonomies]
categories = []
tags = []
+++

== Clarify task ==
I think the task description implicitly suggests that if a local NOTES.TXT file does not currently exist, and NOTES is called ''with'' arguments, then a new NOTES.TXT file should be created with the current time and those arguments. If that is the intention it would be useful to make this explicit in the task description.--[[User:Tikkanz|Tikkanz]] 00:10, 21 May 2010 (UTC)
:Your guess is correct. Feel free to correct it if you so desire or else wait till I've got time --[[User:Axtens|Axtens]] 03:32, 21 May 2010 (UTC)

==use of newline==
Not all operating system's file structure use an imbedded ''newline''.   Some use a metastructure.   Are programs (solutions) supposed to insert a bogus character at the end-of-line for those cases? -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:04, 2 March 2013 (UTC)

The same is true of a ''tab'' character.   How a tab character is treated can be changed (or nullified) in some operating systems, and also application programs. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:01, 2 March 2013 (UTC)

This task seems to be ASCII-centric. 
* IBM mainframes (CMS &amp; TSO mostly) don't use ''newline'' symbols within a file. 
* Tab characters are different in an ASCII system vs. an EBCDIC system. 
* CMS systems rarely use periods in the fileIDs   (they use ''filename filetype filemode''). 
-- [[User:Gerard Schildberger|Gerard Schildberger]] 19:04, 2 March 2013 (UTC)

Not all file systems support the use of a period (dot) in the filename.   CMS for one, the file would be normally be named:   '''NOTES   TXT'''   with an appended ''filemode''   (most often,   '''A1''').

Lower case is supported, but it's not recommended for ease-of-use. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:09, 2 March 2013 (UTC)
