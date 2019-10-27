+++
title = "Talk:Taxicab numbers"
description = ""
date = 2016-04-23T23:51:47Z
aliases = []
[extra]
id = 17374
[taxonomies]
categories = []
tags = []
+++

== Not how Wolfram defines Taxicab numbers ==
In http://mathworld.wolfram.com/TaxicabNumber.html it says a Taxicab number has to be defined n ways, not just 2 ways.  So at minimum, this may be a bad title.  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 05:45, 14 March 2014 (UTC)

: Yes, I'm aware that Wolfram's MathWorld (TM) defines taxicab numbers differently than OEIS does   (well, only in part).   OEIS also used this alternative definition   (which MathWorld uses)   as A11541.   OEIS entry A1235 is the one that is used for a model:   ''Taxi-cab numbers: sums of 2 cubes in more than 1 way''.   I specifically noted that this (more or less) is the definition used here in this task.   If OEIS defines it thusly, I saw no reason not to use that name --- albeit there are two different definitions, but that isn't unusual to have multiple (and/or conflicting) definitions.   The major difference is that the OEIS A1235 sequence definition omits the (non-)usage of negative numbers (for the cubes), but from the context, he (or they) only used positive integers when generating that OEIS list.   Using the OEIS A11541 definition would be beyond the scope of most programs or computers for most, I would think, the numbers almost look rarer than hen's teeth. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:46, 14 March 2014 (UTC)

:: P.S.   I don't mind someone changing my quotes/wording (in the talk sections) if there is are typos, misspellings, or bad or outdated links, but not the wording or the look-and-form.   After all, the good Samaritan thing is to make the original intent clear and not obfuscated with my bad spelling and/or typos.   I was given the impression that my "talk-section" words aren't to be (or shouldn't be) edited and/or reformatted, or in someone's words, inviolate, immutable.   I would put things like italicizing, bold facing (it that a word?),  superscripting/subscripting and the like into this category.   I have corrected misspellings in the past, but have been told that those changes were <strike>possible</strike> possibly inappropriate.   I prefer that my paragraphs have sentences with a double blank after each sentence, and other stuff such as keywords, keyphrases, mathematical notations, and such thingys for readability --- unless you think that   '''&amp;nbsp;'''   has some other meaning or use.   I hate to revert changes to other people's changes to my wording (either in content or form).   I assume the intent of the changes is to make my words easier to read, but it's my choice to have more blanks to make the words, phrases, mathematical expressions, or sentences easier to peruse.   Removing them from my wording doesn't do so in my opinion, and I'd like the intent of my words or phrasing to remain unaltered.   A couple of blanks hither and thither shouldn't be a bother. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:46, 14 March 2014 (UTC)

==Extra credit discrepancy==
'''Extra credit answers of Python and Rexx do not agree'''. If you swap the commented <code>sumcubes</code> line in and use it instead of the line above in the Python solution I can derive exactly the same Python result by explicitly counting the ''sets'' of different ways forming the sums of two cubes. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:42, 14 March 2014 (UTC)

: I don't quite fully understand what you just wrote (two things).   Did you say if you swap a Python statement around, it produces the same result as the REXX extra-credit solution?   The other thing I don't understand is what ''sets'' Python is counting (''sets'' of two sums of cubes?).   Are you saying something about '''1+3''' is the same ''set'' as '''3+1'''?   Since the REXX solution has lower numbers than the Python solution (in the 2,000 range extra-credit thingy), I suspect one reason may be that the REXX version is computing more taxicab numbers, and since they are out of order, the more taxicab numbers are generated, the more likely chance that a lower taxicab number would be found.   One method of resolving this would be for me to (re-)generate the list, but this time, list all 2,007 numbers (and look for mirror sums.   There shouldn't be any, but I've been wrong before (once or twice ...).   By the way, a nit ... The Python is one number short.   ... List the 2,000<sup>th</sup> plus a half-dozen more, that is, for a total of seven highish taxicab numbers.   Not worthy of a flag.   I thought I had asked for an index number, but I had forgotten to include it in the task requirement (so everyone could see what the 20th taxicab number is without counting from the top). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:46, 14 March 2014 (UTC)

The OEIS A001235 b-file gives the extra credit answers as

```txt

2000 1671816384
2001 1672470592
2002 1673170856
2003 1675045225
2004 1675958167
2005 1676926719
2006 1677646971
2007 1680918365

```

&mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 16:43, 14 March 2014 (UTC)


###  OEIS Reference confirmation 

I downloaded the 10000 values from the OEIS b-file then compared the first 2007 with what my Python program generates.

Apart from 10 values the b-file values are the same as what the python solution produces. The 10 discrepancies are all values that have '''three''' cubic sums rather than 2. On re-reading the task taxicab numbers just need to have two ''or more'' ways of summing. I had missed that and will ammend the python solution forthwith! --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 17:25, 14 March 2014 (UTC)

:This still leaves a problem with REXX which doesn't agree with the b-file at oeis.org/A001235/b001235.txt, for example REXX 2000'th == b-file 2080'th value. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 17:40, 14 March 2014 (UTC)

:: This'll take some time, so please be patient if I don't get back within a couple of days.   What I really need is another fast PeeCee and a clone or two to work on this stuff. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:46, 14 March 2014 (UTC)

::: I found the problem with the REXX (extra credit) numbers.   The REXX code was counting "triple" taxicab numbers twice (that is, those taxicab numbers which had three sets of sums).   The problem was obvious once I had actually looked at the first such animal, taxicab number 455   (87,539,319).

```txt

453        87029432   ────►   380**3   +   318**3,      443**3   +    45**3
454        87483968   ────►   363**3   +   341**3,      440**3   +   132**3
455        87539319   ────►   414**3   +   255**3,      423**3   +   228**3,      436**3   +   167**3
456        87579037   ────►   370**3   +   333**3,      444**3   +    37**3
457        87699456   ────►   432**3   +   192**3,      440**3   +   136**3

```


== Another Implementation needed ==
The C entry is quite fast, but it's quite fiddly (my second D entry comes from the C entry and it's just slightly less fiddly). Is someone willing to create an implementation in some language (even Java) similar to the C entry but at a higher level, that uses a heap library? So many imperative languages can copy from it.-[[User:Bearophile|bearophile]] ([[User talk:Bearophile|talk]])
: Thank you Ledrug :-)

== tidying up wording in the task description/requirements ==

Now that the task requirements have been changed again, who will be (or should be) responsible in marking/flagging the two programming entries as "incorrect" or "needs-review" as they no longer meet all the requirements?   I had originally had finding the 2,000<sup>th</sup> taxicab number (and a half-dozen more) as extra credit, and now, finding the 2,000<sup>th</sup> taxicab number is a requirement, and finding the next half-dozen taxicab numbers are now ''extra credit''.   Or, I could change (tidy-up) my wording as I had originally entered (ditto with the change mentioned below).

Also, the definition of taxicab numbers that's now in place for this Rosetta Code task has a link to Wikipedia's (Hardy-Ramanujan number) definition of a taxicab number, which isn't the one that was used for the requirement, but the sentence says (some could say, implied) that it's the one being used (here), depending on one's interpretation of the phrase: ''(the definition that is being used here)'';   I interpreted that to mean what was immediately read previously (the words before the parenthesization).   If the link were removed, then it would be as I originally intended, that the ''immediately following'' definition is the one I had intended to be used (and had specified), as there are several definitions of taxicab numbers.   This would also require the re-instating of the link I had originally entered (twice) that had pointed (shown a link) to Wikipedia's entry of "Hardy-Ramanujan Number"   (which is different than the definition that I specified for use for this Rosetta Code task. 

I certainly don't want to start a tidying up war, when changes (drastic or otherwise) are made to another person's wording, it can be offending, especially if it appears to be less accurate or effective (which is ''very'' subjective, of course, it may just boil down to ego ... my wording is better than your wording, so I'll make a change to your wording ... Ugh).   I certainly don't agree with most of the "tidying up" being done to various Rosetta Code task requirements that I've entered, after all, most of us have an idea what is tidy or not, especially if we're entering the task definitions, requirements, links, and other notes and comments.   Certainly, I believe that I've some better ideas for other Rosetta Code tasks (wording), but changing someone else's wording, format, links, or whatever could be interpreted as not respecting their opinions or expertise.   I discount spelling or typo errors, or some other such finger bloopers.   Keeping the same format and adding better (or more) whitespace, more links, more auxiliary definitions (aliases and alternate definitions and such), or other notes/exceptions are welcome.   Additions are always welcome.   Making it worse in the reading and/or looking-at is a step in the wrong direction.   I generally prefer more whitespace, shorter sentences (no compound sentences in definitions if possible), and not a string of many sentences on one line (or long lines in general), especially for definitions, links, and requirements.   The intent is to make the wording more readable and perusable.   After all, this will be read by everyone, not like specific comments in the various language entries (which are skipped over by most people not interested in most other languages).   <strike>Added</strike> Adding a blank line here and there makes the first impression of the task easier to eyeball and to find out what is required.   What would be more appreciated if people (who make unannounced changes, and sometimes, unappreciated or unwarranted) would ask first, saying something like, "would you mind reformatting or rewording this and that?   Here's what I had in mind: ...   and possibly followed (hopefully) with some justification or other thoughts in explaining your thinking.   This would make the changing and tidying up much more collaborative and instill a genteel and polite relationship when (maybe drastic) changes are made, even if only "tidying up".   In another matter, calling an attempt to flag a task as needing review because a requirement had changed (not knowing the proper template) as "lame", falls short of being polite and respectful.   Just replying with the proper template to use (or implementing it yourself) would be much better than just removing it with an offensive comment. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 11:24, 26 March 2014 (UTC)

: I changed the extra credit back to include 2000-2006th number.  There's no legitimate reason to think 2001st is somehow much harder to obtain than the one before it, so at least we don't pretend to give people a choice when there really isn't one.  If you do think everyone needs to be able to calculate up to 2000th, make the whole thing mandatory. If you ''really'' want people to worry about efficiency, use a much larger number as extra credit. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 18:53, 26 March 2014 (UTC)

:: You are apparently confusing me (the original author of this task and the author of the comments above yours) with someone who made the various changes you speak of.   I stopped trying to make changes to the task's description and requirements a while ago, it was being tidied up a bit too much for my taste.   The last change I did to the task description was to add another alias for ''taxicab''.   Essentially, I feel that it's not my dog anymore. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:08, 26 March 2014 (UTC)

== Video talking about the story ==

I just spotted a video explaining the story and the name: https://www.youtube.com/watch?v=bJDiZi9dqOg. Interesting stuff. --[[User:Mwn3d|Mwn3d]] ([[User talk:Mwn3d|talk]]) 01:26, 29 August 2015 (UTC)
