+++
title = "Talk:Extract file extension"
description = ""
date = 2017-12-21T16:32:00Z
aliases = []
[extra]
id = 19094
[taxonomies]
categories = []
tags = []
+++

==a new contributor== 

Hi, I was a quiet reader but decided to contribute now. That is my first every wiki-like article so please excuse my mistakes and kindly tell how to improve. :)

:Hi, First thanks for taking the trouble to lurk and try and get the hang of things before making your first post. It is appreciated :-)
:I re-wrote bits for style and trying to be a little more exacting in the definition. I also added two more tests that fit the revised definition, but the existing C# might need revising and extra output.  I also stuck nowiki tags around the example link.
:What do you think? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 10:22, 4 May 2015 (UTC)

Thank you Paddy. I did not think about your test cases. I believe the underscore one is legal according to this site: http://filext.com/faq/file_extension_information.php
Yes, you are right. My C# code needs to be updated. I will try to do that, when I find time.

==definition of a legal file extension==

If the   ''file extension''   appears   '''after'''   the last period (using the first example), then the file extension should be   <big>'''jpg'''</big>,   and not   <big>'''.jpg'''</big>   (just a nitpick).   Also note that   <big>'''.jpg'''</big>   contains a non-alphanumeric character, namely the period.   In various documents that I read, the file extension is to the   ''right''   of the last period (with other caveats, of course), and does not include the period.   But other sites do include the period, so ... -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:26, 4 May 2015 (UTC)

Also note that the legality of what is a legal file extension depends on the operating system.   Windows/95 for instance, greatly expanded what is legal (including the use of additional characters, including a blank, as well as lowercase letters).   And I can't vouch for the various flavors of *NIX. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:26, 4 May 2015 (UTC)

Also, I would use the word   '''digits'''   (or better yet,   '''decimal digits''')   instead of   '''numbers'''   when defining a legal file extension (for this Rosetta Code task). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:42, 4 May 2015 (UTC)

It wouldn't hurt to define what a   '''letter'''   is;   I assume you meant lower and uppercase versions of the Latin (Roman) alphabet.   If so, specifically mention them.   I don't feel comfortable assuming what was stated. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:42, 4 May 2015 (UTC)

'''Numerals'''   is a better word to use instead of   '''numbers'''   in this case. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:25, 5 May 2015 (UTC)

'''<big><big><math>\pi</math></big></big>'''   (pi)   is a number,   so is  '''+1.5'''   and   '''-6'''.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:42, 4 May 2015 (UTC)


Another also:

The 2nd part of the definition of a filename extension states that:
   *  consists of a period, followed by one or more ASCII letters or digits (A-Z, a-z, 0-9)
I would argue that the filename   
  abc._#5
fits that requirement as the numeral   '''5'''   follows the period.

Therefore, the following would be more apt: 
   *  consists of a period, followed  ''solely''  by one or more ASCII letters or digits (A-Z, a-z, 0-9)
which would exclude any extraneous characters.

And, yes, I know that one of the test cases (examples) shows this, but examples shouldn't be used as definitions. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:40, 31 August 2016 (UTC)

: Updated. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 06:19, 1 September 2016 (UTC)

Another case I find useful is "libglfw.so.3.1" => "so", and always returning lowercase results. Just sayin [[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 12:18, 9 September 2016 (UTC)

In Unix and Unix-like environments
 .desktop
is a hidden file or directory and not a file extension.

: Updated:--[[User:Garak|Garak]] ([[User talk:Garak|talk]]) 12:34, 21 December 2017 (UTC):

:: That's why the specification says "For the purposes of this task", and the task allows showing a built-in standard library function that behaves differently. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 16:32, 21 December 2017 (UTC)
