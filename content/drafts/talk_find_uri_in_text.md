+++
title = "Talk:Find URI in text"
description = ""
date = 2012-01-09T02:56:24Z
aliases = []
[extra]
id = 11137
[taxonomies]
categories = []
tags = []
+++

== Unicode Chars ==

My hunch is just to leave Unicode characters alone. This can be regarded as a matter of conversion before the URL is used. It depends on the purpose of extracting URL's from text. (Are they headed for a processing stage which deals with those characters fine?)[[Special:Contributions/24.85.131.247|24.85.131.247]] 19:01, 3 January 2012 (UTC)
: that's the intention exactly. non-ascii characters are mentioned because they should be included. a parser that only accepts legal characters would not do that.--[[User:EMBee|eMBee]] 02:14, 4 January 2012 (UTC)
:: So, since spaces can be entered in a browser, they can be accepted as part of a URI, here? --[[User:Rdm|Rdm]] 18:17, 5 January 2012 (UTC)
::: i suppose if the url has a delimiter like quotes or <http://go.here/to this place>, then i don't see why not. it's depends on the ability to figure out the users intent. and on the application. depending on where the parser is used there might even be an opportunity to verify that a url actually exists. (now that would actually be an interesting feature: you type some text on some website, and the browser or server tells you that the url you typed does not exist)--[[User:EMBee|eMBee]] 03:38, 6 January 2012 (UTC)

== What actually parses as a URI may not be what was intended by the task author ==
I just had a look at this against the cited RFC.  I believe the task description is inconsistent with the syntax spelled out in the RFC. For instance, 
* Even though it doesn't make sense to a person "stop:" is a valid scheme and parses as a URI through 'path-empty'.  To rule it out you must know what the valid schemes are.
* nothing in the RFC indicates that parenthesis must be balanced and the characters are allowed via the 'segment' parts of URIs.
Based on this a solution that gives "stop:" and containing unbalanced parenthesis are technically valid but probably not what the author intended.  --[[User:Dgamey|Dgamey]] 02:17, 8 January 2012 (UTC)
: not sure about "stop:". because for one, new schemes can be made up. some applications have internal schemes that are known to us. the task only asks to find URIs, not process them, thus the decision to deal with "stop:" or not, can be handled in the processing stage. for example in some cases you may only be interested in http, https, and maybe ftp. in such a case you'd go through the list of matches and remove anything that is not of interest. of course one could write the parser in a way that it can take a list going in to decided which schemes should be found, but by default there is no harm in finding to much.
: nothing in the task indicates that parenthesis must be balanced either.  unbalanced parenthesis are certainly valid and are what the author intended too. please look at the live example i found from wikipedia: [http://en.wikipedia.org/wiki/-) http://en.wikipedia.org/wiki/-)] (and note how mediawiki parses it wrong :-).--[[User:EMBee|eMBee]] 03:57, 8 January 2012 (UTC)
:: I had another look and the RFC definitions also allow '-' and '.' in through 'unreserved', 'pchar', and 'segment' so "http://en.wikipedia.org/wiki/-)" and "http://en.wikipedia.org/wiki/-" are valid as you indicated as well as "http://mediawiki.org/).".  Also the URI with the illegal character is valid up until that character so "http://en.wikipedia.org/wiki/Erich_K" is valid.  Appendix C doesn't help much as none of the sample URI's are cleanly delineated. --[[User:Dgamey|Dgamey]] 04:37, 8 January 2012 (UTC)

== Expected Output Needed ==
A list of expected output should be given to avoid confusion.  Some of the examples are clearly wrong.  
* Pike is incomplete and includes the illegal char
* TXR also includes the illegal character
At this time that would be all of the examples are wrong.  --[[User:Dgamey|Dgamey]] 04:37, 8 January 2012 (UTC)
: this is maybe not clear from the task description, handling unicode characters is intentional in order to allow a user to write an url as they see it. (look at how http://en.wikipedia.org/wiki/Erich_Kästner_(camera_designer) is displayed in the browser when you follow the link.)
:: This gets a bit into the details.  The link is encoded with &amp;auml; which is allowed in a URI.  If it's Unicode then it is not technically a URI but an IRI (see below). --[[User:Dgamey|Dgamey]] 15:19, 8 January 2012 (UTC)
::: (no it isn't (ok, i don't have a german keyboard and i was just lazy ;-)) i am not talking about the encoding here in the text, but the display in the browser address bar. (imagine looking at a screenshot). it is conceivable and to be expectd that a person would type such an address as she sees it, and expect it to work.--[[User:EMBee|eMBee]] 15:29, 8 January 2012 (UTC)
:::: That would be the wiki then.  The character I get back is a single byte extended ASCII value of 228 or xe4. --[[User:Dgamey|Dgamey]] 15:32, 8 January 2012 (UTC)
::::: sorry, i was intentionally opaque. it was <code>&amp;auml;</code> because i was to lazy to copy-paste a real <code>ä</code> until i fixed it. my point though was that the encoding in the text is not relevant to what i am talking about, but the way it is displayed in the address bar. anyways, the whole argument is moot because RFC 3987 covers exactly what i mean and i have updated the task accordingly (thanks again for that pointer).--[[User:EMBee|eMBee]] 16:05, 8 January 2012 (UTC)
:::::: Thanks.  I was going to start a discussion on task description but I think you covered it.  Even though it's possible to have a good gut feel about what happens, some of these things get picky when you wade into the details.  Also this is the price one pays for dabbling in draft tasks :).  I may tweak the task description some for elaboration as many may not know what an IRI is.  --[[User:Dgamey|Dgamey]] 17:52, 8 January 2012 (UTC) 
::::::: that would be great, thank you!--[[User:EMBee|eMBee]] 02:56, 9 January 2012 (UTC)
:it is not necessary to copy the example input exactly. if you can think of other examples that are worth testing, please include them too.
:as for the expected output, this is a question of the balance beween following the rfc and handling user expectations. for example, a <code> . </code> or <code> , </code> at the end of a URI is most likely not part of the URI according to user expectation, but it is a legal character in the RFC. which rule is better? i don't know. until someone can show a live URI that has <code> . </code> or <code> , </code> at the end i am inclined to remove them. in contrast the <code>()</code> case is somewhat easier to decide. if there is a <code>(</code> before the URI, then clearly the <code>)</code> at the end is also not part of the URI, but there are edge-cases too.--[[User:EMBee|eMBee]] 06:58, 8 January 2012 (UTC)

== Unicode and URIs ==
[http://www.ietf.org/rfc/rfc3986.txt RFC 3986] defines URIs and does not allow Unicode; however, the IETF addresses this in [http://www.ietf.org/rfc/rfc3987.txt RFC 3987] via the IRI mechanism which is related but separate.  The syntactic definitions are very similar where most of the elements are extended.  Two lower level elements are added 'iprivate' and 'ucschar' which are specific ranges of two byte percent encoded values.  These elements percolate up through most of the higher syntax elements such as the authority, paths, and segments which have i-versions.  Other elements such as 'scheme' and the IP address elements are left alone.  There is also no 'ireserved' element. --[[User:Dgamey|Dgamey]] 14:50, 8 January 2012 (UTC)
: Having worked on a couple of projects that involve parsing things defined by RFCs I've found that, unless it's a use once and throw away solution, straying from the RFC or reinterpreting them is generally asking for trouble.  --[[User:Dgamey|Dgamey]] 14:50, 8 January 2012 (UTC)
:: there is also the general rule: be strict in what you produce, but be liberal in what you accept. i believe this applies here. but thank you for pointing to RFC 3987. looks like that is exactly what i meant, and i wouldn't mind if that is used as a base to decide what is valid and what not. however, i believe that using "any text" except <code>" < > </code> and whitespace as delimiters at the end of an URI is sufficient for most use cases.
::as for once off or throwaway code, i see rosettacode not as a place to provide finalized libraries but code that anyone can use as a starting point to implement their own. for that i favor simpler code that is easier to understand and modify rather than complete code that solves all edge cases which a user may not even be interested in.--[[User:EMBee|eMBee]] 15:55, 8 January 2012 (UTC)
