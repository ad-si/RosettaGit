+++
title = "Rosetta Code:Village Pump/Bash examples"
description = ""
date = 2010-11-10T02:00:33Z
aliases = []
[extra]
id = 3043
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Bash examples
|summary=Discussion of various shell types
}}
If you wish to insert a Bash examples, do not add it with a title "Bash" but rather with a title "UNIX Shell" which is more generic, and add a comment about the kind of shell the example is written with. See [[Date_format#UNIX_Shell|this page]] to see an example.
: Does anyone feel up to creating a GeSHi language file for bash?  "man bash" on most Linux boxes should give you a complete listing of all of the keywords and special vars. --[[User:Short Circuit|Short Circuit]] 16:54, 11 February 2009 (UTC)
:: Is not supported? [http://qbnz.com/highlighter/ Here] says it is supported (is that the official site? I don't know). --[[User:ShinTakezou|ShinTakezou]] 15:33, 12 February 2009 (UTC)
::: Yeah, I just discovered this last night.  The language ID is '''bash'''.  I'm hoping we don't get too much flack for lumping POSIX shell, bash, tsch, psh and others all under "unix shell." --[[User:Short Circuit|Short Circuit]] 17:07, 12 February 2009 (UTC)
:::: For anything nontrivial in the shell (e.g. setting environment variables, control structures, builtin commands, redirections other than stdin-from-file or stdout-to-file, etc.), the C shell (csh, tcsh) family is significantly different from the POSIX shell (csh, bash) family. --[[User:Kevin Reid|Kevin Reid]] 22:55, 12 February 2009 (UTC)
:::::How about <nowiki>
```csh
 for C shells (which she sells down by the C shore) and 
```posh> for POSIX shells?</nowiki
 --[[User:Mwn3d|Mwn3d]] 22:59, 12 February 2009 (UTC)
::::::People don't always know whether they're writing for '''bash''' or '''sh'''.  I discovered this when I tried manually building Cinelerra a while back on Ubuntu.  Their script had

```bash
#!/bin/sh
```

::::::at the beginning, indicating to the system they wanted a POSIX shell.  That edition of Ubuntu had /bin/sh symlinked to '''dash''', which is a ''strictly'' POSIX shell.  And it turned out part of their build script depended on '''bash''' extensions.  Symlinking /bin/sh to /bin/bash fixed most of the problems.  However, the '''csh''' lang ID idea is good...is '''tcsh''' the same?  And what do we do for '''psh''' and friends? --[[User:Short Circuit|Short Circuit]] 06:34, 13 February 2009 (UTC)
::::::: ... [[wp:Almquist_shell|Interesting]] and creating confusion... but from a syntax-HLing point of view, it is not a problem, I believe a bash-syntax-HLer highlights well also ash/dash/sh; handling them as aliases? --[[User:ShinTakezou|ShinTakezou]] 14:45, 13 February 2009 (UTC)
:::::::: The question is whether or not it's appropriate for the highlighter to imply support for keywords and other functionality that aren't in the language being highlighted. --[[User:Short Circuit|Short Circuit]] 16:08, 13 February 2009 (UTC)
::::::: Back to the question about lumping '''tcsh''' with '''csh'''. Was anything ever decided about this? IIRC, almost all the differences between the two were in interactive behavior. I don't think I've ever written a '''csh''' script that had to be modified to run in '''tcsh'''. It seems to me that it would be considerably safer to put '''csh''' and '''tcsh''' together than to put the POSIX umbrella shells together. --[[User:Balrog|Balrog]] 00:24, 28 May 28 2009 (UTC)
