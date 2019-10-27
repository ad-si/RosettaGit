+++
title = "Talk:Base64 encode data"
description = ""
date = 2014-05-11T15:58:42Z
aliases = []
[extra]
id = 16063
[taxonomies]
categories = []
tags = []
+++

==Changes prior to promoting to full task==
We should use some different, shorter test data. Right now, there are two problems:
# the output is really too long to include on the page, and
# there's no guarantee that the icon will stay the same.
Using a short piece of sample text would make it much more practical to include the output with the solution, so allowing everyone to trivially check their implementation. Once that's sorted out, there's no reason we shouldn't promote to a full task as there are plenty of implementations. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 15:58, 11 May 2014 (UTC)

==task requirements==

Should the output be shown when converting the Rosetta Code icon (''favicon.ico'')?

I would prefer to show something shorter/smaller (and can be used to verify the various program's outputs) such as the five examples that the Wikipedia article on ''Base64''   [http://en.wikipedia.org/wiki/Base64]   that were used as illustrations (as used in the REXX example). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 16:25, 27 August 2013 (UTC)
:Now that I think of it I'd like it to split the task or to merge these tasks.
:*Base64 Encode a string
:*Base64 Encode an image
:*Base64 '''Decode''' a Base64'd string.
:But these would require a change on the task requirements, I guess I didn't thought that well, before.. creating this draft. [[User:Rainb|Rainb]] ([[User talk:Rainb|talk]]) 04:37, 28 August 2013 (UTC)

:: That's why the task is first made a draft. --- to iron out the requirements and make the task clearer.   Currently, the requirement is to use/convert the   ''favicon.ico''   (file),   but not to show the result.   How is anyone to know the currect result ''is'' if nobody compares/verifies the OUTPUTs?   I would like to see if the programming examples can correctly handle the various padding variations   ('''=''' and '''==''' and no equal signs).   Unfortunately, the file to be used doesn't have any   '''='''   padding in the output.   So far, nobody has shown the 4,850 bytes of output ... until now. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 05:12, 28 August 2013 (UTC)
'''output''' from the REXX example using an eighty-byte wide screen:

```txt

AAABAAIAEBAAAAAAAABoBQAAJgAAACAAAAAAAACoCAAAjgUAACgAAAAQAAAAIAAAAAEACAAAAAAAQAE
AAAAAAAAAAAAAAAAAAAAAAAAAAAAA////AIaJhwBGSEcAv8LAACYpJwBqbWsA4+bkAKCjoQBXWlgAFx
kYAM7RzwA4OzkArbCuAHN2dADr7uwA/P/9AAADAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
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
AAQAAAAEAAAABAAAAAQAAAAEAAAAB

```


 -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 05:12, 28 August 2013 (UTC)

-----

:::You're completely right, I thought this image was small enough, apparently it wasn't small enough, so.. how do you upload images to this wiki? [[User:Rainb|Rainb]] ([[User talk:Rainb|talk]]) 12:18, 28 August 2013 (UTC)

:::: I had the exact same problem a few years back.   One had to use the   '''Upload file'''   link at the bottom of the (left) sidebar, but I can't find it now. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:30, 28 August 2013 (UTC)

:::::Image uploads are disabled. Security problems (and spammers). Not sure when we will be able to bring them back. --[[User:Mwn3d|Mwn3d]] ([[User talk:Mwn3d|talk]]) 13:54, 29 August 2013 (UTC)

:::Also I just remembered that some languages may have issues with requesting binary HTTP, perhaps we should omit the image from the task..? [[User:Rainb|Rainb]] ([[User talk:Rainb|talk]]) 12:27, 28 August 2013 (UTC)

:::: Yes, I think omit using (web) images as input to Rosetta Code programs should be dropped.   For me, it's a bit of bother as my language-of-choice doesn't grab files from the web, so I have to download them first. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:08, 28 August 2013 (UTC)

::::: Don't make downloading the file a requirement of the task itself; it's just a source of binary data, not an HTTP client task (we've got another task for that). But it is reasonable to have a defined input to work with so that outputs can be compared. (Note that whitespace differences in the output are going to have to be allowable; the base64 spec ''specifically'' allows them.) –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 10:31, 29 August 2013 (UTC)

:: There's not much logical difference between a string and an image at this level; you're really encoding a byte sequence (and logically you have to start by encoding both the string and the image as bytes before you can apply the encoding at all, even if that's typically trivial for ASCII strings). I'll rename the task though, probably to “Base64 encoding” so as to better reflect what the task actually asks for. I encourage writing a matching decoding task; we can put in cross links between the two too. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 10:36, 29 August 2013 (UTC)

::: But the thing is that the current task requires downloading of a file from the Web, which is conceptually separate from base-64. And downloading from the Web may be a very complicated task or it may not be supported by standard libraries in some languages, so I don't think it should be included. --[[User:Spoon!|Spoon!]] ([[User talk:Spoon!|talk]]) 20:36, 29 August 2013 (UTC)

The Base64 for the image file seems to have changed (at least it's different than what's above. Can anyone verify this? --[[User:Spoon!|Spoon!]] ([[User talk:Spoon!|talk]]) 00:13, 9 September 2013 (UTC)

== Trouble downloading icon programmatically ==

In the Python on my computer, if I do

```txt
urllib2.urlopen('http://rosettacode.org/favicon.ico')
```

, it gives a 403 error. And if I do

```txt
print urllib.urlopen('http://rosettacode.org/favicon.ico').read()
```

, I get HTML that, among other things, says

```txt

The owner of this website (rosettacode.org) has banned your access based on your browser's signature (a6d6708c9990293-ua48).

```

--[[User:Spoon!|Spoon!]] ([[User talk:Spoon!|talk]]) 23:47, 31 August 2013 (UTC)
:I don't know much python but this gets the file

```Python
import socket
s=socket.socket(socket.AF_INET,socket.SOCK_STREAM)
s.connect((socket.gethostbyname("rosettacode.org"),80))
s.send(bytes("GET /favicon.ico HTTP/1.1\r\nHost:rosettacode.org\r\n\r\n","ASCII"))
data = s.recv(1024)
s.close()
print(data)
```
 [[User:Rainb|Rainb]] ([[User talk:Rainb|talk]]) 02:19, 1 September 2013 (UTC)
