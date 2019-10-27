+++
title = "Talk:File size"
description = ""
date = 2013-04-24T21:21:27Z
aliases = []
[extra]
id = 13359
[taxonomies]
categories = []
tags = []
+++

==task clarification==

It's not specifically mentioned, but everyone seems to have taken '''file size''' to mean bytes (or characters), instead of (perhaps), the size of the file in lines (records). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:10, 24 April 2013 (UTC)

: And for good reason - "bytes" is as close as we have to an unambiguous standard for file size. Absent any specification of some other mechanism, "size in bytes" is the most reasonable interpretation of the current task description. Also, "size in lines" would introduce unnecessary complexity (what is a line?) and computation (no modern OS provides an efficient mechanism for obtaining the number of lines in a file). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 20:23, 24 April 2013 (UTC)

:: VM/CMS is a modern OS, and the filesize (number of lines) is built into the CMS file structure.   As for what is a "line", it is a record; the CMS and OS operating systems use a meta structure of what a line (or record) is; in *NIX-type systems, this is blurred by the use of vertical tab characters (VT), line feeds (LF), carriage returns (CR), and/or other whatnots, including combinations of same.   [By the way, CMS is where REXX was developed.] -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:50, 24 April 2013 (UTC)

:: In any case, it would be a good idea to specify exactly what is meant by ''filesize'' instead of letting the programs use (or guess) the most reasonable interpretation of what's required.   Adding a couple of words to the task requirement wouldn't clutter up the requirement, and making the interpretation of what's required moot.   That way, there isn't a need for this discussion (or my blathering). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:50, 24 April 2013 (UTC)

:: My first interpretation was, in fact, the size of the file in lines (records), and nobody bothered to comment on the REXX solution which reported it thus.   (I've since changed it to reflect bytes.)   If you ask most (mainframe) programmers what's the size of that so-and-so program, the answer is almost always:   xxx lines-of-code   (or xxx lines).   To most (old school) mainframe programmers, a ''program'' most likely refers to the ''source'' (measured in lines-of-code), the ''module'' would be the executable, and that would be measured in bytes.   Most mainframer programmers tend to think in terms of (logical) records for a file instead of a stream of characters (bytes). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:50, 24 April 2013 (UTC)

:: In fact, the original version of the REXX example did use the '''lines''' BIF which returns (for most REXXes) the number of lines (records) in the file.    The CMS, R4, and ROO REXX interpreters do this by default, Regina can mimic this by the use of the '''option noFast_lines_bif_default''' statement, or via the 2<sup>nd</sup> argument for the '''lines''' BIF:   '''lines(xxx,'C')'''       ---   '''C'''   is for   '''C'''ount. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:21, 24 April 2013 (UTC)
