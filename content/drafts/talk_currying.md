+++
title = "Talk:Currying"
description = ""
date = 2013-04-11T08:41:23Z
aliases = []
[extra]
id = 13241
[taxonomies]
categories = []
tags = []
+++

==Copying examples from Wikipedia==
Hi NevilleDNZ. Unless you have permission from the authors, I don't think you can neccessarily copy examples from wp for this site. Authorship is lost for example. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:25, 2 April 2013 (UTC)

: The license on Wikipedia content is now one of the CC ones (CC-SA?) and not GNU FDL. The {{tmpl|wikipedia}} template seems to be out of date. (I don't know if I wrote any of the examples on the Wikipedia page; if I did, I grant permission to copy. Suspect that I didn't though. Sorry about that.) Of course, I think a number of the examples should be rewritten anyway, as they don't demonstrate things in a way that really matches up to the RC style. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 19:45, 2 April 2013 (UTC)

: Does anyone else know if we can just reproduce the content from wp? If we can, would it be better to state in each example used where it came from? Sould we just link to a possible wp example it if is good enough? (Would they have a suitable anchor by the wp example we need? I know, a lot of questions. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:47, 2 April 2013 (UTC)

:: I don't know. On the one hand, we may be playing fast and loose with the license. On the other hand, I note that Wikipedia has [[wp: Wikipedia:Licensing update|changed the license over time]]; they ''used to use'' the GFDL only until 2008–9. The gripping hand is that this page is made out of content removed in 2007; someone decided (years ago) that the WP page didn't need examples and removed them all. It's thus quite possible that the license that the examples were provided under and which they were provided under while they existed as actual current page content (instead of as ''historic'' content) is the GFDL, and as such we would be totally OK with this page (can you imagine any GNU license that didn't allow you to do what we're doing?) I don't feel like investigating too much, so let's assume that we're OK. (We link to the source which links to the history, and this page shows we're aware of the issues and reasonably respectful.) –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 22:57, 2 April 2013 (UTC)


Fair/interesting points... (as others have said) this code is from "old" wikipedia c. 2007, maybe before a license change.
Here is an archived version from December 2007, it refers to the "All text is available under the terms of the GNU Free Documentation License. "
* http://web.archive.org/web/20071201120319/http://en.wikipedia.org/wiki/Currying
The version archived before this appears to have no archived license:
* http://web.archive.org/web/20070320165459/http://en.wikipedia.org/wiki/Currying

I am a bit puzzled that wikipedia can change my "GNU Free Documentation License 1.2" contributions to "Creative Commons Attribution-ShareAlike License;" without specifically asking me. I am guess GFDL is "up gradeable" to "CCASAL", or wikipedia set a re-licensing trip-wire on with the statement "By using this site, you agree to the Terms of Use and Privacy Policy."

re: "a number of the examples should be rewritten anyway, as they don't demonstrate things in a way that really matches up to the RC style"

Agreed... wikipedia never really warmed to algorithms coded in a programming language.   Rosettacode has done a more comprehensive job in this area.  One quirk of this "task" is that it very "open" and (aside from currying) not a specific "task" per se.  I guess "Currying" is more of a concept, then a task.  (For the record I coded the [[ALGOL 68]] version, and am happy that it stays GNU Free Documentation License 1.2)

BTW: here are details of wikipedia's copyright change over dates in 2009:
*Jun 14th GFDL - http://web.archive.org/web/20090614054151/http://en.wikipedia.org/wiki/Main_Page
*Jun 16th CCASAL - http://web.archive.org/web/20090616190648/http://en.wikipedia.org/wiki/Main_Page

Here is a page that talks about the details: http://www.conservapedia.com/Wikipedia_copyright.

Here is wikipedia's position: [[wp:Wikipedia:Reusing_Wikipedia_content#Re-use_of_text_under_the_GNU_Free_Documentation_License|Re-use of text under the GNU Free Documentation License]].

So it seems to me that as the example I pulled was from 2 July 2007 then it is GDPL and RC are free to use it.  Whether is is "good enough" for me/us/RC is another point.  (IMHO we needed a currying page anyhow, so this first try will do for now)

At this point my head hurts.  Copyrights are a tangled web, outside of GPL/CC this compounded by lost author identities and lost motivations/licenses.  (And clingy university intellectual property departments that smother possibility)

[[User:NevilleDNZ|NevilleDNZ]] ([[User talk:NevilleDNZ|talk]]) 02:57, 3 April 2013 (UTC)

: RC missed the boat in the license switchover. RC is also fixed at GFDL v1.2, as opposed to more recent versions. This is because I cannot arbitrarily switch the license; there's been no copyright assignment (and such a thing isn't possible in Europe, anyway), so I can't simply do whatever I wish with the code. What's important is right at the bottom of the form I'm filling: "Please note that all contributions to Rosetta Code are considered to be released under the GNU Free Documentation License 1.2 (see Rosetta Code:Copyrights for details) ... You are also promising us that you wrote this yourself, or copied it from a public domain or similar free resource. Do not submit copyrighted work without permission!". It is not permissible to simply copy code from other resources without the consent of the original author, except in the nebulous region of 'fair use'. It's easy to make arguments for and against the idea that copying code snippets to RC from other places qualifies as 'fair use', but I'm of the opinion that it probably should **not** be done. RC is a sole proprietorship, and, frankly, *I'm* the guy on the hook for all 800-odd megabytes of content on the site if I don't do things properly. When in doubt, reimplement. --[[User:Short Circuit|Michael Mol]] ([[User talk:Short Circuit|talk]]) 03:19, 3 April 2013 (UTC)

== What is the task? ==

What does it mean to demonstrate an "example of Currying"? A few languages have syntax that makes it easy to write curried functions, e.g. Haskell, OCaml, Standard ML, etc. where the normal way to write "multi-parameter functions" actually defines curried functions. Obviously in this case you could show how it is used.

The vast majority of languages do not have such syntax. It is unclear then what the task is asking for. Many solutions write a function that takes a function and some arguments and partially applies them (i.e. returns a function that takes the remaining arguments). That seems like a duplicate of [[Partial function application]]. Some other solutions simply write a partially-applied function. This mix of solutions makes it really confusing. --[[User:Spoon!|Spoon!]] ([[User talk:Spoon!|talk]]) 08:41, 11 April 2013 (UTC)
