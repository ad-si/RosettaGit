+++
title = "Talk:Caesar cipher"
description = ""
date = 2019-03-04T02:00:29Z
aliases = []
[extra]
id = 9914
[taxonomies]
categories = []
tags = []
+++

Draft task because I have not yet posted a solution. --[[User:Kernigh|Kernigh]] 00:51, 12 June 2011 (UTC)
:When you create a task you don't have to explain why it's a draft. All tasks should be drafts when they start out. --[[User:Mwn3d|Mwn3d]] 00:54, 12 June 2011 (UTC)

== more specifications? ==

How lenient is this task? Can I capitalize all input and throw out all non-alphabetic characters? Can I assume ASCII? On the IRC we came up with a 65 character solution which assumes all input is capital alphabetic... --[[User:Crazyfirex|Crazyfirex]] 20:56, 18 September 2011 (UTC)
: Why not provide two solutions then, a more general program besides the 65 character one.  It's unlikely either one will take too much effort to write.  (btw, I think at Caesar's time there were only 24 letters in the Latin alphabet, all upper case, and no punctuations or arabic numerals existed; buts that's probably not relevant) --[[User:Ledrug|Ledrug]] 21:13, 18 September 2011 (UTC)
:: I was the task author. [[Vigenère cipher]] required to discard non-alphabetic characters, and [[Rot-13]] required to preserve them. [[Caesar cipher]] has no such requirements. Among these solutions, some discard non-alphabetic characters, some preserve them, and some might only work with uppercase (not lowercase) input. --[[User:Kernigh|Kernigh]] 21:54, 18 September 2011 (UTC)
::: I went with Ledrug's suggestion, see the AutoHotkey example. (Oddly enough, I was unable to get it below 70 characters) --[[User:Crazyfirex|Crazyfirex]] 01:59, 19 September 2011 (UTC)
:::: Care to explain why <code>t+=2</code> (assuming it's not a bug)? --[[User:Ledrug|Ledrug]] 02:03, 19 September 2011 (UTC)
::::: Probably 16 bit wide characters and byte addressing? --[[User:Rdm|Rdm]] 11:19, 19 September 2011 (UTC)
:::::: Number one, the characters were eventually shaved off: I was using a 3-letter variable name instead of one char! :P  Two, the line ''t:=&s'' retrieves a memory address of the string and stores it in ''t''. The code ''*t'' retrieves a byte (actually, a character-wide number) at ''t''. To advance through the string, we must increment t. I was using it on a Unicode build, so 16 bits. On an ANSI build, we could probably remove <code>,t+=2</code> and use <code>While *t++</code> or the like. (Although, incrementing in the while would screw up the use of *t in the Chr() statement.) I will add a note that the golfed version works on Unicode. --[[User:Crazyfirex|Crazyfirex]] 22:09, 19 September 2011 (UTC)
::: For what it's worth, a scheme that discarded Greek characters would have been useless to Caesar himself. (Opinion is divided as to whether he expired with "Et tu, Brute" or "καὶ σὺ, τέκνον"). Perhaps, in any case, all such discussions should by now incline to UTF-8 by default. The shadows already grow long on ASCII and the Pax Americana. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 14:21, 4 November 2016 (UTC)

==Undiscussed deletion (JavaScript) June 13 2016==

:I notice that two JavaScript examples, one functional, one iterative, were deleted without discussion on June 13 2016, and replaced only by an imperative example. 
:Addition is generally preferable to deletion, particularly where approaches diverge, but more importantly, proposed deletions do need to be motivated and explained here on the discussion page.
:Unless there are objections, I propose to restore at least the functional version, so that readers are allowed see both approaches [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 12:11, 4 November 2016 (UTC)

==Correction of C solution - 2019-03-04==

:Compilation of the original program was carried out on Linux, where the implicit function declaration warnings for isalpha, isupper and tolower, are resolved by adding an #include <ctype.h> statement at the top. However, the program crashes at run-time causing a segmentation fault at the point of transforming the first char in the string. The reason for the failure is because the isupper function is only guaranteed to return zero if it fails, and a non-zero value if it passes. On linux this non-zero value is not '1' as the original author may have expected, but '255' (checked the result by using gdb); as such it breaks the idea of using the result of isupper in this fashion to determine which alphabet string (lower or upper) to use as there's only index '0' and '1' in the 'alpha' const char array named 'alpha[2]' and no there's no index at '255' (reason for the seg fault!). This is resolved by using an if statement to test the case of current alpha character and to set the index of 'alpha' array to 0 or 1 accordingly.
