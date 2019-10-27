+++
title = "Talk:URL decoding"
description = ""
date = 2015-05-29T23:36:54Z
aliases = []
[extra]
id = 19179
[taxonomies]
categories = []
tags = []
+++

Task update suggestion: support for extended ascii UTF-8. -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 05:31, 26 May 2015 (UTC)
: in what way? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 08:21, 26 May 2015 (UTC)

::Say for example Google search `Abdu'l-Bahá .. https://www.google.com/search?q=%60Abdu%27l-Bah%C3%A1 .. how to decode %60Abdu%27l-Bah%C3%A1 = `Abdu'l-Bahá? -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 16:04, 26 May 2015 (UTC)

:::Any existing implementation should have no problem with the url https://www.google.com/search?q=%60Abdu%27l-Bah%C3%A1 - so it would be reasonable to add that as a test case. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:29, 26 May 2015 (UTC)

::::Ok added it as a test case. I know it breaks the Awk code. I left a note saying where to find working gawk code, but it lists every potential UTF-8 character so it's large (and given the possibilities not even complete). I suspect other languages could have similar problems. -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 00:47, 27 May 2015 (UTC)

:::::I had no serious problem with the existing awk implementation on your new example. I did have two minor issues I needed to deal with:

:::::# The url being decoded is hardcoded into the example. I dealt with this by replacing the hardcoded url. A more general solution might place the url on stdin.
:::::# I use using LC_ALL=C which prevented display of text as utf-8. I dealt with this by unsetting that environmental variable. (LC_CTYPE and LANG might have similar effects, but I was not using them.)

:::::I suspect that if you were encountering issues that they might be similar. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 04:31, 27 May 2015 (UTC)
::::::If you pass the url from stdin, is the decode work being done by the shell? When I ran it on one machine it worked, but on another machine it didn't. I looked at the code and saw it wasn't multi-byte aware, and so figured the working machine was a fluke. As you say something with LC and LANG makes sense - on the machine it didn't work is set: 
:::::::*LANG=en_US.UTF-8
:::::::*LC_ALL=en_US.UTF-8
:::::::*LC_COLLATE=en_US.UTF-8. 
::::::On the machine it worked none of those are set. For Rosetta Code purposes for awk to rely on the shell environment to do a urldecode seems not right? At least not very portable. (and I don't understand why enabling UTF-8 would make it not work.) -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 19:51, 27 May 2015 (UTC)

:::::::: If you pass the url from stdin it's not hardcoded, that's all. 

:::::::: Meanwhile, it's the same character sequence emitted regardless of the environment. The issue with those variables is how they get handled by the OS (which is responsible for putting the font on the screen). 

:::::::: As for why it's broken for you when you set those variables, I will guess that that's because whatever is interpreting those variables expects that it's talking to something other than what it's really talking to. Where if you erased them they would be passed through unchanged (and handled as utf-8 by a different part of the OS). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 00:28, 28 May 2015 (UTC)

:::::::::Well, a version of the function which works regardless of environment settings or OS exists so I'll probably stick with that one as I can be sure it will work on any OS. Out of curiosity I tried it with windows compiled gawk.exe from a Windows 7 console and it doesn't work. The function isn't multi-byte aware, it treats each character as a separate character. If it does work it's just blind luck your OS is correctly handling it but that can't be relied on if you plan on distributing your code for others to use. -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 14:28, 28 May 2015 (UTC)

:::::::::: Ok, you have lost me. What do you mean by "works" in that paragraph? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 14:33, 28 May 2015 (UTC)

::::::::::: You mean in the first sentence? I'm saying that the string %60Abdu%27l-Bah%C3%A1 is correctly URL-decoded to `Abdu'l-Bahá .. that works. Try this URL: https://ideone.com/H9QJMT (bottom of the page "stdout") .. as you can see, it doesn't work here. It doesn't work under Windows gawk.exe. etc.. in these place it returns the string  `Abdu'l-BahÃ¡ .. that's not working. Actually it works as expected because the function is not multi-byte aware so it converts %C3%A1 into two separate letters Ã and ¡ .. which correspond to %C3 and %A1. Rather it should be printing á which is a multi-byte letter corresponding to %C3%A1. But the function is not multi-byte aware. If it is working someplace it's only blind luck that the OS is doing some magic for you, but that can't be relied on. -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 14:42, 28 May 2015 (UTC)

:::::::::::: It works for me with windows cygwin awk in its default state. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:50, 28 May 2015 (UTC)
:::::::::::::Well yeah due to LC/LANG having the right setting (or no setting?). I should have clarified the Windows native gawk.exe run in a Windows console doesn't work, not the Cygwin gawk.exe version. -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 18:24, 28 May 2015 (UTC)

:::::::::::::: I think you need to ignore LC_ALL/LANG (other than requiring that they have sensible values for you environment and OS), since their purpose is to be able to turn unicode support on or off outside the context of the program. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:28, 28 May 2015 (UTC)
:::::::::::::::Yes I would like to ignore locale settings entirely, believe me! Yet, it is the key to making the URL decode function work correctly as evidenced from our discussion. The alternative is to use [https://github.com/kevin-albert/awkserver/blob/master/src/core.awk this urldecode() function] which has an array of every potential character needing to be decoded. -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 01:40, 29 May 2015 (UTC)

:::::::::::::::: https://github.com/kevin-albert/awkserver/blob/master/src/core.awk should have the same issue with utf-8. If you tell awk that it should represent its characters using utf-8 then what you'll get from the urldecode will not be the right code points. 

:::::::::::::::: Maybe think of this as utf-8 code points (which are what the awk code emits, in either version) vs characters (which in utf-8 can each be a sequence of one or more code points)? ---[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 09:13, 29 May 2015 (UTC)
:::::::::::::::::You are correct my mistake. Kevin's function is based on the extended ascii table. For the character "á" it encodes it as %E1 (which works in most browsers) however Kevin's function can't independently decode UTF-8 %C3%A1 back to "á", rather that depends on the OS (locale settings). The reason for UTF-8 is because most browsers and HTML pages encode in UTF-8 format so when you do web scrapping and want to extract a URL (say, a href link tag), it's encoded in UTF-8, and if you then want to display part of that URL (say, the name of a search term) you have to convert it back to visible characters. I've yet to find an OS-independent way to do it (in Awk) that doesn't rely on an external tool (such as Bill Poser's [http://billposer.org/Software/uni2ascii.html ascii2uni] .. which isn't very portable as an external tool). Really what I'm looking for is an Awk program that will covert RFC 2396 URI format (e.g. %C3%A9) -> Unicode, independent of locale settings. -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 16:14, 29 May 2015 (UTC)
:::::::::::::::::: Why not make the locale settings a part of the implementation? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:42, 29 May 2015 (UTC)
::::::::::::::::::: Right. There's no way to modify the environment from within awk, but make it requirement before running. Or a wrapper bash script. -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 23:36, 29 May 2015 (UTC)
