+++
title = "Talk:Soundex"
description = ""
date = 2017-09-04T06:27:28Z
aliases = []
[extra]
id = 4967
[taxonomies]
categories = []
tags = []
+++

== Task Improvement ==
It's all very well to have "do soundex" as a task, but it would be far better if we had a more concrete task. For example, attempting spelling corrections on a short text using soundex matching against a supplied dictionary. Right now, it feels like this task isn't really going anywhere. –[[User:Dkf|Donal Fellows]] 15:51, 12 November 2009 (UTC)
:A contributor to the problem is that there's no algorithm. I looked at the one on WP and it doesn't make that much sense to me. I checked the talk page and there's an alternate algorithm proposed, but it apparently doesn't cover all cases. Also, for languages without built-in soundex libraries, doing the conversion alone seems like task enough to me. --[[User:Mwn3d|Mwn3d]] 16:07, 12 November 2009 (UTC)
::Fair point about languages without soundex in libs. Maybe the other idea would be better as a task that builds on this one… –[[User:Dkf|Donal Fellows]] 16:30, 12 November 2009 (UTC)
:As I understand, there are different soundex algorithms, based somewhat on the language and on the applicatons. I also read the Wikipedia entry and it does not present the algorithm clearly. A couple of years ago I needed an algorithm to match information for new entries in a database to existing names. That's when I ran across the soundex algorithm. --[[User:Rldrenth|Rldrenth]] 21:03, 12 November 2009 (UTC)
The task seems very ambiguous to me. Should I be writing code that parses a word based on the Wikipedia "Rules" section? Should I show that burrows and Burroughs have the same soundex index? Should there be a "Sample Output" section? The task may be simple to implement, if I only knew what was expected.   -[[User:Crazyfirex|Crazyfirex]] Feb. 20, 19:39:47 (UTC)

::: Yes, the   ''Burroughs''   and   ''burrows''   words took me a while to get straight, but in doing so, I found the bug in the program.   It was because of this bug that I verified my REXX program with almost all other samples to verify my interpretation and implementation of the rules were correct.  

::: Also, thanks to the   '''Go'''   program, I found another bug   (using   ''12346''   as a word).   If I hadn't perused through   ''all''   of the examples' outputs, I'd never found that error. 

::: I think the ole saw about   ''it's not over until the fat lady sings''   should apply here.    If you don't show any output, we can't assume the program (example) is correct.   I coded up an equivalent program of the PL/I example and it produced incorrect results.   [I don't have a PL/I compiler, so I can't bet my life on it that it's wrong.]   I'm sure that there are other examples that are incorrect, particularly those examples that assume the first character is a letter, and in other cases, where the character being examined isn't a letter of the Latin alphabet (punctuation, blanks, apostrophes, hyphens, etc).   -- [[User:Gerard Schildberger|Gerard Schildberger]] 06:57, 31 May 2012 (UTC)

== Which Soundex? ==

It isn't clear which Soundex algorithm each example is implementing. For example, the US Census rules have a special case for "H" and "W" (ignored but don't separate runs of consonants). I suggest adding a set of test cases to the problem description which can distinguish between the many variants of Soundex out there. For starters:
 A261 for Ashcraft
 B620 for Burroughs and Burrows
--[[User:IanOsgood|IanOsgood]] 15:35, 13 November 2009 (UTC)

:There's an algorithm that's been floating around for years that's attributed to (DE?) Knuth, but that's not the original. Mind you, the original was not designed for use by computers either. In any case, go with the Knuth algorithm (Google can find implementations of it easily enough). –[[User:Dkf|Donal Fellows]] 12:12, 14 November 2009 (UTC)

::The problem with saying "go with the Knuth algorithm" is that it's not clear what that actually is. The Php soundex function and the Tcllib soundex package both claim to implement the Knuth algorithm and return A226 for Ashcraft. However, the Perl Text::Soundex module supports two functions: soundex (returning A226) and soundex_nara (returning A261), with the documentation claiming "the algorithm described by Knuth is the NARA algorithm" (i.e. A261).

::Now if you were to pop into a book store and actually have a look at a recent edition of Knuth's TAOCP, you'll see that his algorithm clearly describes the special case handling of H and W, so that would suggest that Perl is correct, and Php and Tcl are wrong. But it's not as simple as that. From what I've read, Knuth's definition actually changed between editions 2 and 3 of his book, so both are probably correct - they've just been referencing two different editions of the book!

::The bottom line is there is no standard soundex definition. Rerencing Wikipedia is pointless since it's constantly changing and referencing Knuth doesn't seem to be any better. It would have helped if this task actually included the algorithm (any algorithm) as part of the task definition, but it's far too late to fix that now. Just accept the fact that half the solutions are going to implement the H/W rule and half aren't. --[[User:J4 james|j4_james]] ([[User talk:J4 james|talk]]) 00:44, 23 October 2015 (UTC)
:::I have the second edition of Knuth's TAOCP vol. 3, and it states in rule 3 p. 394: ''"If two or more letters with the same code were adjacent in the original name (before step 1), or adjacent except for intervening h's and w's, omit all but the first."'' This seems to follow the "A261" rule. However, a Google search shows me that both conventions are widespread. There is even a case where both algorithms seem to be used in the same place (probably a bug): [http://rsl.rootsweb.ancestry.com/cgi-bin/rslsql.cgi here] and [http://resources.rootsweb.ancestry.com/cgi-bin/soundexconverter here] at rootsweb.ancestry.com. Try ASHCROFT in both (select "soundex" in "Select type of search" in the first page). There is a [http://search.cpan.org/~rjbs/Text-Soundex-3.05/Soundex.pm Perl package] providing both functions as '''soundex''' and '''soundex_nara''', and so does [https://www.stata.com/help.cgi?soundex Stata]. The book "SQL for Smarties: Advanced SQL Programming" states on page 245 that both methods were actually used by the Census Bureau (the rules were not applied uniformly). I don't think the A226 rule is really a "mistake". It's just another convention, that has been used in the past, and still is in many implementations. However, when someone wants to implement the Soundex from National Archives, then the correct one is A261. See '''[https://www.archives.gov/research/census/soundex.html here]'''. [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 06:26, 4 September 2017 (UTC)
