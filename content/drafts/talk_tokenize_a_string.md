+++
title = "Talk:Tokenize a string"
description = ""
date = 2013-12-07T18:42:58Z
aliases = []
[extra]
id = 4222
[taxonomies]
categories = []
tags = []
+++

==Joining collections of strings==
Does RC already have this? Coders join strings almost as often as they tokenize them. --[[User:Mcandre]]

== Remove tr==
I suggest we remove tr as:
# It isn't a programming language in the commonly held sense of the term, it is a (good) Unix utility.
# It does not fulfil the task by using an array/list.
# There are likely to be too many tasks that tr cannot perform.
--[[User:Paddy3118|Paddy3118]] 19:49, 20 May 2009 (UTC)
: While it's not a programming language in the commonly held sense of the term, that puts it in the same company as declarative languages such as ladder logic or languages used in PLC and ASIC design. (And I would argue that there's enough variability in those domains to make them worth comparison as well.)
: I also agree it's broken with respect to meeting the task requirements.  Using a period for item separation is unidiomatic for the context within which tr would get used for such a purpose.  More appropriate would be replacing the , with a newline.  Still ''more'' appropriate would be replacing it with a null character.
: The question of how many tasks tr can perform is a question of the tool's domain; Its parameter syntax effectively puts it in the category of a domain-specific language.  If there are other languages which service the domain (which practically any string processing language does, in addition to '''sed''', another common UNIX tool), then it becomes worthy of intradomain comparison with those languages.  If the issue is whether or not a particular UNIX tool can be considered a language in and of itself, then consider whether or not that tool is part of a POSIX spec; It's plausible that that particular spec might be a more suitable umbrella for the code example. --[[User:Short Circuit|Short Circuit]] 02:45, 21 May 2009 (UTC)
Hi Short Circut, If the tasks ask us to write a program to do something then we should use a programming language and tr is not a programming language. It is a program, a utility. Just because the program can be applied to give the right answer doesn't make it a programming language in the conventional sense. If you look at the man page for tr or the [[wp:http://en.wikipedia.org/wiki/Tr_(Unix)|WP]] page, it is not billed as a programming language. I am not saying that tr cannot produce the right output. I do think you go to far in calling tr a programming language as you would then allow too much 'noise' in RC. What is to stop anyone using your argument to add the Unix <code>sort</code> command to each and every one of the RC sort algorithm entries? or <code>echo "the correct answer"</code> to numerous tasks? I think that this would be a good point to make the distinction.

Calling tr a programming language would leave you out on a limb w.r.t. tr's documentation and articles about tr on the web such as [[http://www.linfo.org/tr.html tr]], and [[http://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html#tr-invocation GNU tr]].

As for the phrase 'domain-specific language' it has little real use as it is so widely applied, and should not be confused with a [[wp:Alphabetical_list_of_programming_languages|programming language]] in this context --[[User:Paddy3118|Paddy3118]] 05:27, 21 May 2009 (UTC)
::Moreover, <tt>tr</tt> is surely part of the [[:Category:UnixPipes]] paradigm, and can be also used in shell scripts as part of the almost omnipresent coreutils package (see e.g. [[Change string case#UnixPipes]]). --[[User:ShinTakezou|ShinTakezou]] 11:34, 21 May 2009 (UTC)
Revisiting this old thread ... I have no problem with tr. But whether it's a programming language or not, the tr example doesn't even come close to meeting the task description. There are 2 steps. First, identifying the tokens by the delimiters, and placing them in a structured data entity for further processing. Second, processing the structured data to produce the desired output.

The tr example does neither of these. It's one-step text substitution that acts only on delimiters, not tokens. And it operates blindly even on a token-free string. For example, the string ",,,," contains zero tokens. Step 1 should produce an empty data structure, and Step 2 should output an empty string, since there are no tokens to separate with a period. Instead, tr will blindly produce "....".

As a Snobol user, I'm sensitive to languages where strings are a primary datatype and often stand in for arrays or other structures. But delimiter substitution is simply not tokenization. --[[User:Snoman|Snoman]] 20:15, 23 July 2010 (UTC)

==Unix Pipes and the shell==
You need to mention which shell is being used in the example. (It doesn't look like csh for example). --[[User:Paddy3118|Paddy3118]] 19:57, 20 May 2009 (UTC)

==incorrect examples==

I noticed quite a few programming examples that failed to meet the two requirements:

:::* output the words separated by a period.
:::* parse the words into an array (or list).

Some of the examples merely treated the input as a string and changed the
comma to a period, bypassing the words-into-a-array-or-list requirement. 

There were numerous examples, too many to start flagging as I may be misinterpreting what a "list" is. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:42, 7 December 2013 (UTC)
