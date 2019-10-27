+++
title = "Talk:Terminal control/Unicode output"
description = ""
date = 2018-08-10T21:26:47Z
aliases = []
[extra]
id = 10506
[taxonomies]
categories = []
tags = []
+++

==Unix shell embedding Awk?==
Surely you can check an environment variables value in the shell, without the need to call Awk. As it stands, it doesn't seem to be a good example of using the Unix shell. --[[User:Paddy3118|Paddy3118]] 07:59, 11 September 2011 (UTC)
:We cannot check for substrings on a Bourne shell, so we have to use AWK. Other shell implementation may not need to do this of course. [[User:Markhobley|Markhobley]] 08:02, 11 September 2011 (UTC)
::Actually, it looks like you can match substrings by using the switch command. If this works I will try and rewrite this to get rid of awk. [[User:Markhobley|Markhobley]] 08:20, 11 September 2011 (UTC)

::Thanks Mark. --[[User:Paddy3118|Paddy3118]] 16:05, 11 September 2011 (UTC)

== Er, "Terminal supports"? ==

I can run <code>xterm +u8</code> with UTF-8 locale, and both current awk and shell solutions output garbage.  What does it mean, "terminal supports unicode"?

The ZX spectrum basic solution doesn't even make sense: can you display more than 256 different characters on screen at once? If not, what good is this "unicode support"? --[[User:Ledrug|Ledrug]] 03:12, 12 September 2011 (UTC)

: Terminal supports unicode means that the terminal supports Unicode output. Not all terminals can do this.

:Yes, the ZX Spectrum can display more than 256 different characters at once. You need to define some switching code though, and you are limited to about 40k usable memory,but that is more than enough to fill the screen. You can even reference the symbol numbers using the listed Unicode symbols, but you would need to code this. The ZX Spectrum was a versatile little machine. FWIW, I used an alternative character set called M5C (one of my own inventions), which I also implemented on IBM compatible computers. This used a 5 bit character set with page switching, and supported many different characters. I will eventually expand and publish updated specifications (because my notes were only partially complete. There are still unused pages that could be implemented. I only implemented what was needed at the time, and the unused pages left room for expansion.)

:: Which part in the examples do you actually verify your terminal "can do this"> Checking <code>LC_*</code> has almost nothing to do with underlying tty device's capability, and for what it's worth, the awk and shell examples don't even check if output device is a terminal, it could be a file or the bit bucket for all they care.  I just don't see how this is a terminal related work. --[[User:Ledrug|Ledrug]] 19:05, 12 September 2011 (UTC)

::: Yeah, checking locale is the current method for determining output type (on a Unix system). There is also termcap, but this has no information on Unicode support AFAIK. Applications should all obey locale, so if the locale says use Unicode, then yay. If not, nay. The locale should always be configured for the terminal, so a session on a Unicode terminal would have a Unicode compatible locale set. On a conventional Ascii compatible terminal the locale would differ. With regards to files and bit buckets, again locale is the boss wrt output. I'm not aware of any other mechanisms for checking terminal type. [[User:Markhobley|Markhobley]] 20:21, 4 November 2012 (UTC)

Yeah, the printf may not work on some systems. I am working on some Unicode compatible helper programs which will be guaranteed to produce the appropriate Unicode output from shell scripts, etc. This in on my todo list.

There are some other problems with the Unix shell solution. It uses a non-portable construct "set --" instead of "unset". I will try to find why this construct was used instead of "unset" (which is more portable). Maybe we need two versions here. I am investigating this. [[User:Markhobley|Markhobley]] 15:48, 12 September 2011 (UTC)

: <code>set -- "$LANG"</code> and <code>unset "$LANG"</code> are not the same. I am using <code>set -- "$LANG"</code> to assign the positional parameters, so "$1" expands to the same value as "$LANG". A program would use <code>unset "$LANG"</code> to unset the variable named en_US.UTF-8 (if "$LANG" expanded to en_US.UTF-8), but there is probably no variable named en_US.UTF-8, so <code>unset "$LANG"</code> would probably do nothing.

Right. What about a set command without hyphens? How does that differ? There is a set command without hyphens, which is portable. [[User:Markhobley|Markhobley]] 19:13, 12 September 2011 (UTC)

: [http://www.in-ulm.de/~mascheck/bourne/index.html ''The Traditional Bourne Shell Family: History and Development''] claims that <code>set --</code> appears in System III. Later, <code>unset</code> appears in System V Release 2 (SVR2). Shell functions, like <code>unicode_tty() { ... }</code>, also appear in SVR2. I expect that today's shells have SVR4 features. --[[User:Kernigh|Kernigh]] 18:04, 12 September 2011 (UTC)

My books are based on System V, and they do not mention it, so it is probably an undocumented feature. There are some compatible shells, where the set -- does not work either. I stumbled across some notes somewhere, which talks about the implementation of the set command, and how the behaviour of the doublehypen is inconsistent when given as a parameter to set. I can't remember where I saw it at the moment though, I think it might have been an awk book, or a korn shell book, rather than a System V book. I am looking into it. [[User:Markhobley|Markhobley]] 19:25, 12 September 2011 (UTC)

: The hyphens with <code>set --</code> are marking end of options, like <code>ls --</code> or <code>rm --</code>. So <code>set -x</code> turns on trace mode, but <code>set -- -x</code> assigns parameter $1. I have used <code>set --</code> around the wiki ([[Factorial#UNIX Shell]], [[Greatest common divisor#UNIX Shell]]). It would be interesting to try a shell where <code>set --</code> does not work. --[[User:Kernigh|Kernigh]] 15:06, 13 September 2011 (UTC)

==Locales and terminal capabilities==

I could probably improve the Unicode detection to include Unicode Transformation locale naming schemes, but I have no data on this. Does anyone know where these are listed? Ideally, I could do with seeing a full list of locales (for Unix). There might be a pattern in there somewhere that could be used to improve Unicode support detection handlers. I am also wondering if there is a terminal capability that could be queried. [[User:Markhobley|Markhobley]] 17:26, 12 September 2011 (UTC)

:If the locale contains a numeral, then the environment is Unicode. (I think, but I am guessing here). If there are no numerals, then the environment is extended Ascii. [[User:Markhobley|Markhobley]] 20:35, 4 November 2012 (UTC)

==Other characters==
Windows unicode does not appear to support delta, at least with the default fonts, effectively making this an anti-microsoft task.

Other unicode characters should be permitted, I propose the following change:
    Show at least one unicode character such as pi, root, lambda, sigma, delta, ie "π√λΣ△" or 
    "\u03C0\u221A\u03BB\u03A3\u25B3". 
    Note that the last, delta, does not appear to work on Windows (10), but the first four do.
    
    State whether the code submitted works on linux, windows, or both.
[[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 16:44, 10 August 2018 (UTC)
