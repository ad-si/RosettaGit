+++
title = "Talk:Text to HTML"
description = ""
date = 2015-05-06T02:12:44Z
aliases = []
[extra]
id = 11142
[taxonomies]
categories = []
tags = []
+++

== Too broad ==

Right now, I think the task is a bit too broad. In particular, there's too many options for what to do and we should instead focus on a more restricted set of things that everyone can implement “the same”. Since we're really talking something like wikitext or markdown, we should use something like that (including allowing people to make use of useful libraries if they wish). Paragraphs are really the minimum; inline bold, italic and fixed-width are also very useful in the minimum set (if only there was single accepted standard for doing them…) –[[User:Dkf|Donal Fellows]] 12:03, 5 January 2012 (UTC)
: we are specifically not talking about wikitext or markdown, but text without any markup at all, except for indenting, empty lines and numbers and bullets, things you would use in plain text. specifically things like inline bold, italic and fixed-width are not possible without some kind of markup, and thus not what this task is looking for.
:: Bold and italic can be recognized by things like *foo* and /foo/ which people use in plain text anyway.
: what i am looking for is to go beyond just recognizing paragraphs, to explore what else can be analyzed out of plain text. i very much expect that the task description will be in flux for a while until we can work out a reasonable set of requirements.
: think of what you would get when using a commandline browser like lynx or w3m on a terminal with out colors or bold text. what you see there is potential input for this task. i do not expect all such input to be parsable, but a reasonable set that goes beyond just paragraphs.--[[User:EMBee|eMBee]] 12:36, 5 January 2012 (UTC)
:: It is going to be difficult to compare implementations if none of them are doing the same thing.  And, for example, the open ended concept of "plain text tables" pretty much guarantees that any implementation which does not ignore that part of the task will be different from any other implementation where a "copy of implementation" relationship does not exist.  A lack of examples will also make comparison difficult. --[[User:Rdm|Rdm]] 18:11, 5 January 2012 (UTC)
::: doesn't that depend on what level you compare? even with exact input and output requirements, how do you compare solutions when one uses regexps and the other uses a state machine? the algorithms will be completely different. as tasks get more complex the comparability will have to be on a higher level. in this task the highest level of comparison is: plain text goes in, html comes out. examples will surely help, but i think these too need to come out of this discussion. once we can agree on a reasonable set of requirements, we can create a few input examples to test the requirements on.--[[User:EMBee|eMBee]] 02:57, 6 January 2012 (UTC)
:::: I was assuming that the results would be comparable.  So, for example, if one implementation treats hanging indentation as a paragraph format, another treats it as a list format and a third and fourth treats that case as a table (one as a single row table, another as a table with one row per line of plain text), what would we be comparing?  Or, as an extreme case (emulating fixed width presentation despite the possible absence of any fixed-width fonts) I might represent plain text as a (borderless) table with one cell per character (and all the characters in the original which represent a url would have a link to the corresponding url).  --[[User:Rdm|Rdm]] 14:40, 6 January 2012 (UTC)
::::: well, yes, the results should be comparable. after the requirements are fixed. before that i hope we can explore which requirements are sensible by discussing them here, possibly including a few sample implementations that demonstrate how specific requirements can be met.--[[User:EMBee|eMBee]] 15:03, 6 January 2012 (UTC)

I've done a Tcl “solution” to this that only handles a subset of all the things mentioned. In particular, it doesn't do links, centering or tables. The latter two are really rather optional IMO (I may add links later). But it does do (non-nested) numbered and bulleted lists, and it does headings too. It also does simple bold, italic and typewriter formatting; that actually covers enough to be useful. Remember, it shouldn't feel like ''work'' to implement an RC task! –[[User:Dkf|Donal Fellows]] 20:20, 31 March 2012 (UTC)

== Concrete requirements? ==


```txt

    Recognize
a leading indentation.

Also hanging
    indentations.

   Block
   indentations.

A paragraph
* with
* bullets, some
of which are like this, but
the additional lines should line up with the first word.

Treat     this
as a      table
because   of
alignment.

A little convention for *bold* or /italic/ or _underline_ is not
such a terrible thing.

Horizontal rule:

----------------

+-----------------------+
| Box                   |
+-----------+-----------+
| Structure |           |
+-----------------------+
| How about it?         |
+-----------------------+

Of course, it should go without saying that, HTML characters like
< and & must be properly escaped.

But http://this.is.a/url turned into a link.
```


[[Special:Contributions/192.139.122.42|192.139.122.42]] 21:14, 5 January 2012 (UTC)

: Some of those seem plausible.
: I'm dubious about hanging indentation (is that first line a part of the following text or is it independent?  How many people actually use hanging indentation? Isn't that likely to cause problems with automatic list formatting?  What is it doing in a plain-text converter?)
:: i use hanging indentation inside a list:
 list item:
   content
   content
 list item:
   sublist:
     content
:: but that's just one example, there might be counter examples--[[User:EMBee|eMBee]] 02:33, 6 January 2012 (UTC)
: I'm dubious about line-end treatment, in general.  (In part because I have been discovering, recently, that what I thought was plain text treatment sometimes is not, and my lines are being glued together in contexts where that destroys my presentation.)
:: but in those cases don't you also need monospace? this is the hardest part to solve. because it could be wrong either way. some of the text-to-html converters i found offered the user a choice, if they wanted <br/> inserted or not.--[[User:EMBee|eMBee]] 02:33, 6 January 2012 (UTC)
: I'm also dubious about that table concept: If I put two spaces between my sentences and I use line ends in my text, and a space happens to fall under the second space on the following line, is that a table?  If the line after it is short, does that make it a table?  Where are the row boundaries?
: And what about boxes that do not align?
:: i don't know if tables can be done in any reasonable manner. two lines and two columns is certainly not enough to recognize a table. but what about this?
 this is no table.  just 9 sentences.  that align.
 a short sentence.  another sentence.  and more.
 some longer text.  this too is text.  so is this.
:: also, sentences in a table are unlikely all ending in .
 the 1 above may  but this text
 not be a table.  here surely is.
:: tables were just a thought, wondering if they can be recognized in a reasonable way. if they can't i'd drop the idea.--[[User:EMBee|eMBee]] 02:33, 6 January 2012 (UTC)
::: Tables are an immense subject.  Table heuristics can be guaranteed to work properly in some cases and can be guaranteed to fail in other cases.  The first example that was presented on this page (which had a two cell table without any character position that could be identified as a boundary between the two cells) was a good example of something that would be difficult to generalize robustly.  --[[User:Rdm|Rdm]] 14:48, 6 January 2012 (UTC)
: Anyways, this is a plausible start, but some aspects of it still seem overly open-ended. --[[User:Rdm|Rdm]] 22:11, 5 January 2012 (UTC)
:: the requirements are currently open ended exactly because i want to bring out questions like yours. thank you. we can discuss these questions then use them to formulate more concrete requirements.--[[User:EMBee|eMBee]] 02:33, 6 January 2012 (UTC)
::: This task is ill-conceived.  You want to derive markup information from text "without format information", yet the only part that fits it is the URL extraction.  For everything else, such as indentation, paragraph separation, bullets, etc, you'd always need ''some'' format information from the so-called "plain" text source: you need to venture guesses using information provided by whitespaces and special characters.  The problem is, simple markups people use in a text file is almost always informal and ambiguous: a paragraph begining with an asterisk might be a bulleted list item, but it could also be a footnote, or a bold type face, or any other kind of thing people may fancy.--[[User:EMBee|eMBee]] 09:52, 7 January 2012 (UTC)
:::: could you come up with some examples? if we restrict the scope of the input to not process large documesnts but things you'd write in a forum, an email, or a blog post, will that simplify the problem? --[[User:EMBee|eMBee]] 09:52, 7 January 2012 (UTC)

:::  When you make a guess at a piece of ambiguous text and put some HTML tags around it, you eliminate all but one interpretation, which can be horribly wrong.  If you luckily managed to make all the right guesses, you wouldn't be adding any useful information to the text anyway, and if you make any wrong guesses, you end up assigning a document a structure that mangles the meaning of the text.  It's ok to want to make some text appear more pleasing, but not at the cost of loss of information (or the chance for a human reader to guess at the correct information). Without a well defined convention for text markup such as in wiki text, this whole exercise seems very pointless to me. --[[User:Ledrug|Ledrug]] 08:31, 7 January 2012 (UTC)
:::: you are right, it won't go without conventions. but conventions do not imply markup. unless you count whitespace as markup. what i mean is the difference between:
                                    title without markup
 and
 ==title with markup==
:::: of course there need to be formatting rules. the interpretation by the parser makes it a rule. if the interpretation by the parser is wrong then the input text doesn't fit the rules.
:::: the goal of this discussion is to work out which rules or conventions make sense, are commonly found in the wild and not surprising to a user whose text is getting processed.

:::: the conventions applied in the pike solution would be:
* isolated lines are titles
* bullets and numbers are lists
* empty lines end or begin a paragraph
:::: the above discussion about a table format is also a convention. is it a good one? we don't know yet. so we are already discussing that, we just didn't call it conventions or rules.
:::: which other conventions can we find?--[[User:EMBee|eMBee]] 09:52, 7 January 2012 (UTC)

== potential rulesets ==

one large body of text with a mostly consistent ruleset that can possibly be extracted (at least in part) is RFCs, another is unix man-pages. other rulesets may exist. however, please keep in mind that the target userbase where this parser is to be applied is websites with user input either in a textarea in a forum or CMS or as email.

the concern and motivation for this task (which rules out just using <code>&lt;pre&gt;&lt;/pre&gt;</code>) is that plaintext without linebreaks can become unreadable if the lines are to long. any solution that can address this problem is worth considering.--[[User:EMBee|eMBee]] 16:39, 8 January 2012 (UTC)

:Quoting >, < and & and prepending newline characters with <nowiki><br /></nowiki> should be sufficient for that purpose?  --[[User:Rdm|Rdm]] 22:38, 8 January 2012 (UTC)
:: for that purpose only,sure. but then that wouldn't be much of a task. therefore i want to explore what else can be done to improve the presentation.--[[User:EMBee|eMBee]] 03:30, 9 January 2012 (UTC)
::: In my opinion, there are two kinds of transforms to consider: "changes that lose information" and "changes that do not lose information".  A "non-lossy" transformation is one that can be reversed.  One ideal would be a non-lossy mechanical transformation which minimizes information losses at rendering time for typical browsers -- here, the goal is to get out of the way and not be an obstacle for the provider of the text.
::: Much of the discussion on this page seems to be about "lossy transformations".  For a lossy transformation to be implemented properly, I think that we would need a clear understanding of the purpose of the transformation.--[[User:Rdm|Rdm]] 15:07, 9 January 2012 (UTC)
:::: the purpose of the transformation is stated above. which aspects would you like me to clarify? i don't know if the transformation can be done non-lossy. certainly sounds like it would be nice to get that. but it's not a deciding factor. ''"get out of the way and not be an obstacle for the provider of the text"'', however is cetainly a quality i'd like to achive. it is one reason why i do not want to consider markup.--[[User:EMBee|eMBee]] 17:38, 9 January 2012 (UTC)
::: Also, is there a reason why we are discussing a new transform here, and not an existing transform?  --[[User:Rdm|Rdm]] 15:07, 9 January 2012 (UTC)
:::: i am not aware of any existing transforms that do not depend on explicit markup. if you can point to any, please share. i'd be happy if this task can be solved with existing tools or based on existing rules.--[[User:EMBee|eMBee]] 17:38, 9 January 2012 (UTC)
::::: What is the difference between the rules you are suggesting here, and "markup"?
::::: Anyways, a google search for <code>plain text to html</code> finds numerous hits.  Superficially speaking, it looks like http://txt2html.sourceforge.net/ does what you want (and its design should serve as a caution for this as a rosettacode task).  --[[User:Rdm|Rdm]] 17:48, 9 January 2012 (UTC)
:::::: i did search, but somehow i missed this one. interesting find,thank you! yes, it does pretty much what i want. and as you note it does to much to be suitable for a task. but perhaps a subset of its features that we can agree on would be appropriate?--[[User:EMBee|eMBee]] 16:22, 10 January 2012 (UTC)
::::::: Fine by me. Pick some feature set, list those features, and create some task examples? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 02:12, 6 May 2015 (UTC)
