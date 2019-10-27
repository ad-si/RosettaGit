+++
title = "Rosetta Code:Village Pump/Task titles"
description = ""
date = 2010-12-08T22:45:09Z
aliases = []
[extra]
id = 5269
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Task titles
|summary=Rules for how to name a task.
}}
For a while now, this has been something of an elephant-in-the-room for me: tasks aren't named consistently. Some names are in title case ([[Missing Permutation]], [[Walk Directory Tree]], [[List Comprehension]]) and some are in Wikipedia-style sentence case ([[Increment numerical string]], [[Atomic updates]], [[Count programming examples]]). Some are imperative clauses ([[Loop over multiple arrays simultaneously‎]], [[Execute a System Command‎]]), some are gerund phrases ([[Repeating a string]], [[Testing a Function‎]]), and some use agent nouns ([[Simple Random Distribution Checker]], [[24 game Player]]). Some use articles ([[Print a Multiplication Table]]) and some omit them ([[Simulate mouse click]]). I don't mean to say that every task title should have exactly the same grammatical form; I just think that this much diversity is ugly and confusing and makes it harder to remember a given task's exact title.

I suggest we adopt conventions for page titles. The exact conventions we choose doesn't matter to me so much as the issue of whether we use conventions in the first place, but here are my suggestions:

* Page titles should be in sentence case (that is, only the first word and proper nouns should be capitalized). This would be consistent with Wikipedia and with MediaWiki's (and hence Rosetta Code's) special pages. Also, in my opinion, sentence case is more legible than title case, since it preserves the distinction between proper nouns and common nouns.
* Imperative clauses should be preferred to gerund phrases: that is, "Testing a function‎" should be renamed "Test a function". Similarly, agent nouns should be avoided: "Fix code tags" is better than "Code tag fixer".
* Articles shouldn't be omitted, except at the beginning: prefer "Increment a numerical string" over "Increment numerical string" but "Longest common subsequence" over "The longest common subsequence".

Should we reach a consensus that a mass renaming is called for, I'm willing to carry it out. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 22:01, 3 January 2010 (UTC)

: 1+ --[[User:IanOsgood|IanOsgood]] 00:30, 5 January 2010 (UTC)
: Once a more thorough discussion on what to rename them ''to'' has been had, I'll give you the privs you need to do it. (I don't think normal users have the move privilege, but I've never been one...) There are over 400 tasks on the site, and I'd like to see what happens when the renaming principles are applied to the entire list; I want to catch edge cases as early as possible. --[[User:Short Circuit|Michael Mol]] 04:21, 5 January 2010 (UTC)
:*Agent nouns have the advantage of putting the subject up front i.e. "Code tag" before the fix, which I prefer because in any sorted list, I want to have the "Code tag" info sort before the fact that it is a fixer.
:*Would prefer String/Increment to both the above as it would sort better - as you have suggested Averages/Median etc.
:--[[User:Paddy3118|Paddy3118]] 04:53, 2 February 2010 (UTC)
:: We don't have to worry about sort order here, since MediaWiki lets you define the title used for sorting differently from the real title. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 14:10, 2 February 2010 (UTC)
:It's not the MediaWiki order so much. I think that for short punchy titles, they read better if you put the main bits up front and follow with any qualifiers. --[[User:Paddy3118|Paddy3118]] 14:46, 2 February 2010 (UTC)

== Proposal ==
To show you how I intend my rules to be applied, and to discern for myself how well they work, I've chosen [[Rosetta Code:Village Pump/Task titles/List|a title for every task]]. I've made a lot of exceptions, since I figure it's more important for titles to be short, accurate, and easy to understand than consistent with each other. One issue that particularly bothers me is number: some of my proposed titles are singular and some are plural, and my method to decide which should be which is pretty arbitrary; all I know for sure is that "Array" and "Accumulator factories" are both bad titles. And there are some tasks, like [[Top Rank Per Group]], for which I can't think of any remotely good title. At any rate, for the most part, I think the new titles would be an improvement. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 20:38, 6 January 2010 (UTC)
: As I haven't seen anyone seriously critique your new names, I've put you in the groups with the relevant privs. --[[User:Short Circuit|Michael Mol]] 23:17, 29 January 2010 (UTC)
:: Actually, one doesn't need special privileges to move pages, and I think I'll do all the moving with [[User:UnderBot]] so as not to spam recent changes. (Assuming moves can be hidden like normal edits can. I hope so.) If there are no objections, I'll start as soon as I get round to writing the code and running it; I'll have the program move a page every thirty seconds so the whole operation will take less than four hours. I wouldn't be surprised if some users object to the new titles ''after'' I've made the changes, but there's not much I can do about that now. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 15:21, 31 January 2010 (UTC)
::: The deed is done. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 16:01, 6 February 2010 (UTC)

I missed the above. Probably because of the holiday season. I have a comment, OK, it is vanity, but I am wondering about references to the original task names, Will they still work? I would like to still be able to track [[User talk:Paddy3118#RC_vanity_search.py|my initial contributions]]. --[[User:Paddy3118|Paddy3118]] 05:27, 30 January 2010 (UTC)
: Yes; MediaWiki's "move" method automatically creates a redirect page. --[[User:Short Circuit|Michael Mol]] 08:10, 30 January 2010 (UTC)

P.S. SEDOL -> SEDOL check digit and validation. ? (or just SEDOL) --[[User:Paddy3118|Paddy3118]] 06:34, 30 January 2010 (UTC)
: Sorry, I didn't notice that the task calls for validating the input, too. I've chosen "SEDOLs" instead. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 15:21, 31 January 2010 (UTC)

: I'ld prefer Spiral matrix instead of Spiral array, and  Zig-zag matrix instead of Zig zag array --[[User:Rldrenth|Rldrenth]] 16:01, 31 January 2010 (UTC)
:: Good idea; changed. I'll use, e.g., "Zig-zag matrix" instead of "Matrices/Zig-zag" because these tasks don't have to do with linear algebra. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 15:29, 1 February 2010 (UTC)

: And Also:	
:* Doubly-linked list/Define, 
:* Doubly-linked list/Insert element, 
:* Doubly-linked list/Define element
:* Doubly-linked list/Traverse
: --[[User:Rldrenth|Rldrenth]] 16:12, 31 January 2010 (UTC)
:: I disagree. On the principle of not omitting articles, "Insert element" ought to be "Insert an element". But "Define element" can't be "Define an element", of course; it would have to be "Define the element type", and by that point I think we're better off with just plain old "Element definition". —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 15:29, 1 February 2010 (UTC)
::Personally, I think the doubly-linked list tasks should simply be merged into a single task. (Same for the singly linked list tasks.) --[[User:IanOsgood|IanOsgood]] 17:27, 1 February 2010 (UTC)
::: Agreed; there's no sense in having them split up like this when they're so dependent upon one another. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 22:55, 1 February 2010 (UTC)

: That list is a little hard to read. It'd help if we somehow highlighted where changes are proposed at all, and further differentiated between minor changes (e.g., case, addition/subtraction of small words like “a”) and more major ones (e.g., “Basic Animation” to “Animation”, which might cause issues later). We can expedite the minor changes easily enough, but the major ones require consideration of what we actually want to achieve. –[[User:Dkf|Donal Fellows]] 15:51, 1 February 2010 (UTC)

:I think the renamings are well chosen. Some comments:
:*I would never guess that "Arena storage pool" had to do with object allocation.
:*Shouldn't "Apply a callback to an array" be merged into "Loop/Foreach"?
:*"Arithmetic/Bitwise operations"
:*"Sorts/*" -> "Sorting/*" or "Sorting algorithms/*"
:*Maybe "GUI/Window creation", etc.
:*"Text processing" is an improvement, but "1" and "2" could be improved on
:*"Bitmap/Flood fill", etc.
:*"Fork process" is still a good name
:*"Machine address" is a good name
:*"Maximum Value" -> "Reduce" (though a more general example might be better [like sum and product of array], since so many languages have "max" built in)
:*"Null" was a better name (maybe "Literals/Null"?)
:*"Number base conversion" is a duplicate task
:*"Proof" -> "Automated theorem proving"
:*"Execute *" -> "* interpreter"
:*"Filter an Array" -> "Filter" (which reminds be that we need a "Map" task)
:*"Literals/Boolean"
:*Keep "User output/*" (since we use "Goodbye world" anyway)
:--[[User:IanOsgood|IanOsgood]] 17:27, 1 February 2010 (UTC)
::* Me neither. Suggestions, anyone?
::* I don't think so, if we're going to distinguish list operations from loops. I think this is the "Map" task you want; should we just name it that?
::* I understand your thinking, but let me make clear that my intention in using titles with slashes is ''not'' to duplicate the function of ordinary MediaWiki categories. That would be pointless. The idea is rather to note tasks that are interdependent or that have largely parallel goals. "Bitwise operators" demonstrates AND, OR, etc., not addition etc., so it doesn't belong in "Arithmetic/*".
::* Yeah, "Sorting algorithms/*" is probably an improvement. Changed.
::* See my comments about "Bitwise operations".
::* Again, good idea, but I don't know what to use instead.
::* Agreed; changed. (The articles which belong to [[:Category:Raster graphics operations]] but which I didn't put in "Bitmap/*" are those which don't depend on [[Basic bitmap storage]].)
::* Okay, but which process? "Fork the current process"? 
::* "Machine address" makes me think the task has to do with finding the address of a machine.
::* Man, I just don't know. I'm not sure how much we should insist that tasks with names like "map", "filter", and "reduce" use actual list operators instead of loops.
::* But [[Null]] isn't about literal undefined values, it's about checking to see if a variable is defined. Hence my choice.
::* What other task does it duplicate?
::* Some of the examples are complete proofs which the software in question verifies. This is distinct from automated theorem proving, in which the software writes the proof itself.
::* But an example doesn't have to interpret. It could also compile. The objective of each of these tasks is just to execute some code; how an example does it is up to the implementer.
::* Agreed; changed.
::* The task's scope is somewhat greater than that. Maybe "Literals/Boolean" should be a redirect?
::* I think a newbie is much more likely to gather what the purpose of the task is if it's called "Hello world". "User output" is misleading because we generally use the word "user" to mean somebody who runs a program, not writes it. The use of "goodbye" instead of "hello" is almost meaningless. Probably we should change the task to use "hello"; Rosetta Code has gotten a lot more serious since the days it was named Goodbye World.
::—[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 22:55, 1 February 2010 (UTC)


### Exception?

Um, request for an exception Sir. 

At one time, it was noted in the blog that we had no tasks beginning with the letters K and Y. I remember it was close to Xmas too, and created the [[Yuletide Holiday]] task to fill a need where the name was important. Could I most humbly ask that the name be changed to just have a lower-case H becoming: ''Yuletide holiday''?  (The same applies to [[Knapsack Problem]], but you already seem to be set to change it to ''Knapsack problem'' with a lower-case p). Thanks --[[User:Paddy3118|Paddy3118]] 07:05, 1 February 2010 (UTC)
: But you also made [[Y combinator]], which I intend to leave alone. That ought to cover it, right? :) At any rate, don't forget that "Yuletide holiday" will hang around as a redirect until someone deletes it, and while I have the power to do that, I have no plans to. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 15:29, 1 February 2010 (UTC)

===If you disagree…===
My real goal, at least for the moment, is just to impose some conventions on Rosetta's titles as a whole, not to worry about individual titles. So if you disagree with some of my (necessarily) arbitrary selections and find my justification for them unconvincing, you can change [[Rosetta Code:Village Pump/Task titles/List|the list]] yourself. When I run UnderBot, I'll use whatever's there. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 22:55, 1 February 2010 (UTC)

== Capitalization ==
I've been going through the task pages to impose some capitalization rules. These rules are:
# First word is always capitalized, the rest of the title is lower case
#* Sub-pages are capitalized independently; this would be correct: <code>Foo bar/Grill sausage</code>
# Proper names and abbreviations are an exception, and should always be capitalized correctly. (e.g., <code>XML/XPath</code>, <code>Bitmap/Read a PPM file</code>, <code>Lucas-Lehmer test</code>)
I don't know if that covers everything, but I think it does. –[[User:Dkf|Donal Fellows]] 10:54, 1 December 2010 (UTC)
:I officially approve. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 22:45, 8 December 2010 (UTC)
