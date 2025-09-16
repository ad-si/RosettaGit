+++
title = "Rosetta Code:TODO"
description = ""
date = 2010-03-29T02:34:14Z
aliases = []
[extra]
id = 2215
[taxonomies]
categories = []
tags = []
+++

Here's a list of "Someone needs to..." items.  If you're looking for something to do, you can always do something on this list.

=TODO=
*Fix [[Special:DoubleRedirects|double redirects]], probably with a bot.
* Merge similar tasks together:
** [[Singly-linked list]] from [[Singly-Linked List (element)]], [[Singly-Linked List (element insertion)]], [[Singly-Linked List (traversal)]]
** [[Doubly-Linked List]] from [[Doubly-Linked List (element)]], [[Doubly-Linked List (element insertion)]]
** [[Arrays]] from [[Creating an Array]], [[Assigning Values to an Array]], [[Retrieving an Element of an Array]]
* Copy/modify code from [[Loop Structures]] to pages in [[:Category:Iteration]].
* Change ''all'' code examples and creation guides to use <nowiki><lang></nowiKi> instead of '''tt''' or '''pre'''.  GeshiCodeTag will now quietly wrap those in HTML '''pre''' blocks.  Use a standardized language ID for each language.  I'll alias them to existing or newly supported language IDs as appropriate and possible.
** It would be nice to have the common mistake of <nowiki>
```name
...>
``` name
</nowiki> either fail hard or silently be fixed to the proper form of <nowiki><lang name>...>
```
</nowiki
.  Otherwise, it hides all subsequent page content until it finds an correct <nowiki>
```
</nowiki> end tag.
* Go through the "needs-review" and "incorrect" examples in [[String length]] and make sure they're correct.
==Ongoing==
* Look through the [[:Category:WikiStubs|stubs]], and research and fill in useful information.  Cite sources as needed, of course.  Remove stub template when done with each page.
* Comb through the suggestions on the [[Help:Request a new programming task|task request]] page and put a few together.  Or drop some into a "Questionable" category, so they can be debated.
* Add features and paradigms to all language pages using parameters in [[Template:Language]] and [[Template:Language programming paradigm]]
** Add features and paradigms to the LCT where missing and known
*Break extra long examples out into their own pages (similarly to [[99 Bottles of Beer#An object-oriented solution]] and [[99 Bottles of Beer/C++/Object Oriented]]) to reduce server load when loading and editing these pages. [[Special:LongPages]] may be a good place to look.
==Automation Candidates==
Any process with a straightforward procedure likely has a programmatic solution, or at least a means to be accelerated programmatically, either via clever usages of templates, page transclusion, a MediaWiki extension, JavaScript, or a bot.
* Anything in the language comparison table should be reflected in the relevant language page.  Additionally, everything in [[:Category:Programming_Languages]] should be reflected in the LCT. This includes:
**[[Template:Language|Language template]] parameters
**[[Programming paradigm]]s
**[[Template:Anchor|HTML anchors]] in the LCT
* Rather than having pages such as [[Perl]] redirect to [[:Category:Perl]], it might be worthwhile to have [[Perl]] (or any other language) as its own page with highlights, and a link to [[:Category:Perl]] for a list of all of the examples in that language.  Updated daily.  Highlights might include the ten pages in [[:Category:Perl]] with the most views, the ten pages in the language's category with most languages, and listings of the category's subcategories. (Currently including users, implementations and libraries of the language, but may expand in the future.)  Might also include a listing of languages related by some characteristic of the LCT, etc. (It might be amusing to try to rank a language's similarity with other languages by how many common characteristics it has in the LCT.  This has the double benefit of allowing us to gauge the effectiveness of the LCT for comparison purposes.)
** This breaks down into several components (Each of these might be part of, say [[:Category:Common Language Highlights]], and could be included based on presence in that category):
*** Daily updates of a template for each member of [[:Category:Programming Languages]] for each of the following characteristics
**** Task pages in the language's category with the top ten most views ("Most popular tasks")
**** Task pages in the language's category with the top ten number of languages which implemented them.
**** Libraries
**** Users
**** Implementations
**** Top ten similar languages (and their similarity rank
*** Weekly update of a page for each member of [[:Category:Programming Languages]] for each of the above.
** Daily updates should be staggered throughout the day; The list of languages to process for should be retrieved daily, and the next day's work scheduled and divided evenly over the day to avoid spiking server usage. (With 142 languages, this works out to one language every ten minutes.)

==Tasks Awaiting Discussion==
These tasks have been disputed or change to them has been discussed in some way, but nothing (or not much) has been done about it. They need to be discussed and/or action needs to be taken accordingly.
*[[Array Initialization]]
*[[BNF Grammar]]
*[[Data Munging]] / [[Data Munging 2]]
*[[Loop Structures]]
*[[Variables]]
*[[Conditional Structures]]

==Ideal Minimum Penetration==

### Tasks

Tasks should have a minimum language penetration of 25% of the languages on Rosetta Code.  That is to say, if there are 100 [[:Category:Solutions by Programming Language|languages]] on Rosetta Code, a task should have examples in at least 25 languages. An exception to this would be if there are fewer than 25 languages capable of implementing the task.


### Languages

Languages should have a minimum penetration of 25% of the tasks on Rosetta Code.  That is to say, if there are 100 [[:Category:Solutions by Programming Task|tasks]] on Rosetta Code, a language should have examples written for at least 25 tasks.  An exception to this would be if there aren't enough tasks in the language's domain for this to be possible. (Though if this is the case, there should be discussion aimed toward introducing tasks that fall in a given language's domain.)

[[Category:Maintenance]]
