+++
title = "Talk:File input/output"
description = ""
date = 2019-08-21T18:37:00Z
aliases = []
[extra]
id = 1651
[taxonomies]
categories = []
tags = []
+++

== AppleScript example ==
Is not really a file I/O - its a file copy operation done by the file system, without even the ability to write in the file names (IIRC it executes interactive file chooser dialogs) and shouldn't be here. I don't know if AppleScript has IO commands, but if it has real IO then the example should show that and if it hasn't, then the example should be removed.
:AppleScript does have real I/O, and I've written a new example that illustrates it. --[[User:Bob9000|Bob9000]] 01:05, 28 January 2007 (EST)
:[[File copy]] would be a useful task. In Apple's HFS+ and perhaps other file systems, you need to use special copy commands if you want to preserve file's metadata (resource fork, tags, file times, etc.) as well as the file's data. --[[User:IanOsgood|IanOsgood]] 16:27, 8 October 2008 (UTC)

== Perl cleanup ==

Removing the latest addition to the Perl section as it shows bad style. It is not representative of the current Perl version and programming practice. See reasoning in next paragraph. I'm keeping the nice while oneliner and binmode caveat, though.

Non-lexical filehandles have global scope and pollute the namespace as they cannot be restricted to the least possible lifetime. The || operator binds stricter than the or operator and forces the open function to have parentheses. The two-argument version of open is unelegant as it invites programming mistakes (see "dispelling the dweomer" in the Camel Book).
[[User:89.49.121.184|89.49.121.184]] 19:31, 22 January 2007 (EST)
==Race conditions==
A thought: Should this task be rewritten to guard against race conditions? Many of these code examples check for a file's existence, and then create the file.  Between the check and the creation, another process might already have created the file. --[[User:Short Circuit|Short Circuit]] 17:55, 12 November 2007 (MST)


==Purpose==

When I wrote this task, the purpose was to demonstrate file input and file output, but the overreaching description of the task has let to filesystem operations.  Obviously, the task needs to be clarified.  Anyone have suggestions?  I'm thinking separate tasks each for input and output.  I'll get on creating those, and then we can figure out what to do about this task. --[[User:Short Circuit|Short Circuit]] 03:47, 7 October 2008 (UTC)

== Binary vs. Text ==

:On second thought, I'm not sure how to handle the difference between binary and text-mode files.  I'm primarily used to working with binary files in proprietary formats. Separate tasks for text/binary, and a set of each for input and output? --[[User:Short Circuit|Short Circuit]] 03:53, 7 October 2008 (UTC)
:Does [[Basic input loop]] cover text input? --[[User:Mwn3d|Mwn3d]] 03:55, 7 October 2008 (UTC)

::On many systems and with many languages there are no special considerations for handling binary vs. text in files.  Separate tasks for each could result in duplication without any clarification to the readership.  Perhaps one task specifying processing of a binary file and a text file.  One might have a task to read a specific binary file and a text file, extract some data from each, and then write the results to a text file.  The code implementing that task could then highlight any differences in handling that are required by the language (as well as any portability features that show up for languages that are supported in multiple platforms).  For example a Python example would show binfile.read() vs. txtfile.readline(). [[User:JimD|JimD]] 15:17, 20 November 2008 (UTC)

== Haskell issue ==

The current Haskell implementation does not comply with the requirement of intermediate variable. [[User:Marius|Marius Amado-Alves]] ([[User talk:Marius|talk]])

: Arguably, 'c' is an intermediate variable. (Also, please [[mw:Help:Signatures|sign]] your talk edits? I threw in a minimal basic signature for you, but the wiki does a much better job if you let it.)

::What 'c'? [[User:Marius|Marius Amado-Alves]] ([[User talk:Marius|talk]]) 18:23, 21 August 2019 (UTC)

:::Oops, I must have been looking at the following (hexscript) entry. Ignore my comment about c - that was a mistake. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:37, 21 August 2019 (UTC)

: (But, that said, ... I can see a reasonable desire for alternate concepts of "reading the contents of a file into a variable" especially for the Haskell case. But there's a variety of those, and we didn't specify which, here. Still, there's certainly room for alternate implementations, on the page.) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 15:53, 21 August 2019 (UTC)
