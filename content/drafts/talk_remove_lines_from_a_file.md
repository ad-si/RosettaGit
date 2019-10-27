+++
title = "Talk:Remove lines from a file"
description = ""
date = 2011-07-19T09:34:21Z
aliases = []
[extra]
id = 10089
[taxonomies]
categories = []
tags = []
+++

I'm not sure if this qualifies so much as a file system operation as data processing. A filesystem is part of the operating system, or sometimes intrinsic to a language's virtualized representation of a filesystem. I ''suspect'' that even describing this task as specific to the concept of a file may be inappropriate; it may be more appropriate to apply it to a buffer into which a file was read, or which maps to the context of some text field. By leaving it nonspecific, it can cover special case-behaviors (such as file-specific) as well as general-case behaviors (such as operating on arbitrary buffers of text). --[[User:Short Circuit|Michael Mol]] 02:19, 13 July 2011 (UTC)

:Yeah. I noticed that other file handling tasks were in this category, so I dropped these in to match. I'm not really happy with this either, and I agree with you. I think we need a category for "File Handling", because this is a different concept to "File System Operations". We probably need to flesh out the descriptions to highlight the differences, and make the split at some point. [[User:Markhobley|Markhobley]] 06:00, 13 July 2011 (UTC)
::FWIW, from memory, I think I categorized as "File Handling" before, and someone referred me to "File System Operations". I didn't really agree with this, but there was some controversy over new categories appearing at the time, so I held back on this. [[User:Markhobley|Markhobley]] 06:07, 13 July 2011 (UTC)

:With regards to the buffer thingy, the intent is to make revisions to the file on disk. I'm still pondering over this one, because some languages handle the buffering, leaving it as a file operation from a programming perspective. There is an excellent Unix shell solution that makes use of ed. I haven't pasted this yet though, because it is lost in my head somewhere :), but when I remember it will appear. Some languages offer "random access" to files. These might also be a good method for achieving this goal using deletion, shuffle and truncation. The task is in its early draft stages at the moment, so this gives us a chance to evolve things. [[User:Markhobley|Markhobley]] 06:23, 13 July 2011 (UTC)

==Argument order==
How about specifically ''not'' setting the order of arguments? This would allow the creation of a program that by default did one thing, but could be given options to remove specified options from a file.
:I limited this task specifically to removing lines from the file, rather than a combination tool.

Woops, I've thought of something that is close to this. What do you think about creating a command line utility "extract" that by default extracted the first line from a file and printed it to stdout whilst leaving the file behind with its first line missing. extract could be given two options: a starting line (defaults to 1), and the number of (this and subsequent), lines to extract (defaults to 1). Missing lines return the null string (without a line terminator). If the file ends without a line terminator then the if that line is returned then it is without a line terminator. Numeric options should not be zero or negative. --[[User:Paddy3118|Paddy3118]] 06:55, 13 July 2011 (UTC)

:Yeah that sounds a useful gadget. I'm not sure about the name "extract", because it actually does a deletion too, but it is a good idea. By default I would just return an errorlevel, if appropriate parameters were not provided. [[User:Markhobley|Markhobley]] 21:53, 13 July 2011 (UTC)
