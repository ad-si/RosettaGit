+++
title = "Talk:Bitcoin/address validation"
description = ""
date = 2017-07-13T08:43:17Z
aliases = []
[extra]
id = 12632
[taxonomies]
categories = []
tags = []
+++

==Task definition==
Is there a link to the actual definition of what is a valid BTC address? The wikipedia link (at time of writing) doesn't say anything on the topic other than that it is “human readable” and “around 33 characters long” and “always starts with a 1 or 3”; that's far too vague for a Rosetta Code task! (Yes, I could try decoding some of the existing examples, but I greatly prefer to be able to independently code tasks if I can. Independent implementations help ensure that the task itself is properly possible.) A link for this sort of thing is entirely adequate for now. –[[User:Dkf|Donal Fellows]] 00:16, 28 November 2012 (UTC)
: Not sure if [https://en.bitcoin.it/wiki/Technical_background_of_Bitcoin_addresses This] is the definitive thing.  On the other hand though, is it really necessary to have a task with such a narrow range of application? --[[User:Ledrug|Ledrug]] 03:39, 28 November 2012 (UTC)
:: The bitcoin wiki is a more accurate source than wikipedia indeed.  As far as whether it is "necessary" to have this task in RC:   no, it is not.  I don't think it would hurt, though.  On the over hand, I know there are some political issues with the concept so I won't insist much if whoever is in charge here doesn't accept it, even as a draft.--[[User:Grondilu|Grondilu]] 04:07, 28 November 2012 (UTC)
::: I'm not too worried about the narrowness of the task spec, just that it's possible to work out how to implement it without reading someone else's solution and that it is possible (even if not necessarily ''easy'') to verify the correctness without reference to someone else's code. If these are satisfied and we've got evidence of independent implementations, it's a good draft and can advance to being a full task.
::::I've added more details in the description of the task.  I'll add an example of a correct bitcoin address later.  (a wrong one is easy to forge).--[[User:Grondilu|Grondilu]] 10:46, 28 November 2012 (UTC)
::: Of more interest though is whether there will be other bitcoin-related tasks in the future (specifically, the next 3 months; there's no point trying to plan further ahead than that). If not, the name of this one should change. That's independent of whether this is draft or not. –[[User:Dkf|Donal Fellows]] 09:09, 28 November 2012 (UTC)
::::I was thinking of making a "create a bitcoin address and its private key" task, but on second thought I'm not sure it can be done in a short program.  It would require [[wp:elliptic curve]] arithmetic so I may have to create a task about that first.--[[User:Grondilu|Grondilu]] 10:50, 28 November 2012 (UTC)
:::::I advise starting with “convert key system to bitcoin address”, which will include the ripemd160 step but not the key generation itself, as well as needing a base58 encoder. (This task has the decoder instead.) –[[User:Dkf|Donal Fellows]] 22:55, 28 November 2012 (UTC)
::::::Good idea.  I'll do that.  I'll call it "bitcoin/Public point to address".--[[User:Grondilu|Grondilu]] 02:44, 29 November 2012 (UTC)


### Vague task

Looking at this further (and peeking at the proposed solutions) the task appears to be to verify that the data is base58-encoded and that the computed check-digits match the supplied ones (which is basically following the bottom of [https://en.bitcoin.it/w/images/en/9/9b/PubKeyToAddr.png this diagram], i.e., after the ripemd160 step). If this is the case, it is important to '''spell this out''' in the task definition rather than leaving it to people to guess. –[[User:Dkf|Donal Fellows]] 09:40, 28 November 2012 (UTC)

: I would second that opinion of Dkf. The only way I could work out what to do was to run another solution and find out what the intermediate values were the nconvert that to Python 3! --[[User:Paddy3118|Paddy3118]] 04:48, 29 November 2012 (UTC)

::Really?  Even with the detailed steps I added in the description? --[[User:Grondilu|Grondilu]] 04:51, 29 November 2012 (UTC)

:::Hi Grindilu, Yep. I guess crypto is outside my comfort zone, I have very little knowledge in that area so I had to work hard on every assumption in both Python's libraries and in your description. Not sure if that is the same for Dkf, but I am rethinking how much I assume readers know about the tasks I write too :-)
 --[[User:Paddy3118|Paddy3118]] 07:31, 29 November 2012 (UTC)

:::P.S. Now that I have done it, your explanation seems a lot clearer :-)
--[[User:Paddy3118|Paddy3118]] 07:31, 29 November 2012 (UTC)

:::: I actually know rather more about crypto; while it's certainly not in my comfort zone, it's not ''that'' hard (well, not once you've got a decent arbitrary-size integer library; that's the critical component which it is very difficult to make from scratch). No, my original complaint was that I couldn't find good links to describe what to do or how to validate it; those are key aspects of a good task. (There are some very scary parts of crypto, to be sure, but the actual message verification step isn't one of them, not like key generation or processing the wilder policy parts.) –[[User:Dkf|Donal Fellows]] 11:06, 29 November 2012 (UTC)

:::::Indeed there is barely any crypto in this task, apart from the use of sha-256.  It mainly consists in big integer arithmetic (and somehow the C code manages not to use big integer, I have to check it out) and byte string manipulations.--[[User:Grondilu|Grondilu]] 11:25, 29 November 2012 (UTC)

===C-based code (and possibly others) improperly validates===
Much of the code here does not check to make sure that the bitcoin addresses are 25-bytes.  Hence, testing using the C-based code shows that it validates the invalid six-character bitcoin address "BZbvjr".  However, trying to send to this address using the standard bitcoin client will naturally produce an error.  I have edited the Perl implementation to make it work correctly, but someone who is a better coder than I should do it for the other languages too. [[User:Schulwitz|Schulwitz]] ([[User talk:Schulwitz|talk]]) 22:57, 21 October 2014 (UTC)
: The PHP code, C code, and probably many more of these snippets are incomplete implementations. I have added a warning at the top of the page to indicate this, as this page is linked to from [https://en.bitcoin.it/wiki/Address#Address_validation the bitcoin wiki]. [[User:Kale|Kale]] ([[User talk:Kale|talk]]) 11:19, 12 September 2015 (UTC)

== Base58Check encoding algorithm ==

This algorithm, "address validation", is not so popular... Is difficult to change its status of "snippets are incomplete".

It uses another algorithm inside it, that is simpler and popular, the [https://en.bitcoin.it/wiki/Base58Check_encoding Base58Check encoding algorithm]. So, creating it: [[Base58Check encoding]].  

--[[User:Krauss|Krauss]] ([[User talk:Krauss|talk]]) 08:43, 13 July 2017 (UTC)
