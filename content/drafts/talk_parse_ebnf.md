+++
title = "Talk:Parse EBNF"
description = ""
date = 2012-01-20T03:06:15Z
aliases = []
[extra]
id = 7507
[taxonomies]
categories = []
tags = []
+++

== Initial comments ==

I guess I started this as a draft task because I haven't implemented it myself yet. 

There is no reason to keep it a draft too long if we get a few fresh implementations here. [[User:Tinku99|Tinku99]] 05:28, 12 June 2010 (UTC)tinku99

: Interesting. I usually create a task after having an implementation as it helps with the task description etc. --[[User:Paddy3118|Paddy3118]] 10:31, 12 June 2010 (UTC)

The task wording is somewhat sloppy. A parser could be for a (EBNF) grammar, not (plural) grammars. For grammars there can be a parser generator. The existing examples in fact build a parser for a particular grammar, and they don't use EBNF at all. May be task should be renamed as "Simple calculator parser"?[[User:Avmich|Avmich]] 20:30, 22 April 2011 (UTC)

== EBNF parser or parser for the given EBNF grammar? ==

Is the task to write a parser that takes an EBNF grammar as input? That's how the task description sounds for me, but all implementation examples don't do this..
Or is the task to write a parser for the given EBNF grammar? Then the description should be clarified and the title changed.
None of the examples parses the EBNF grammar.
--[[User:Oenone|Oenone]] 11:48, 10 May 2011 (UTC)

: Yes, the parser should take an EBNF grammar as input. The two examples (PicoLisp and Tcl) are wrong. I would keep the title "EBNF parser". --[[User:Kernigh|Kernigh]] 14:34, 11 May 2011 (UTC)

::If it needs to be an EBNF parser then we have a task description with no working language implementation after 11 months. Do we need a rule that tasks should have at least one correct implementation or at least some indication from the task creator, Tinku, that an implementation is correct after a reasonal amount of time - say - a week or two? Looking at this task, it seems to have been abandoned by Tinku without them OK'ing any of the implementations. --[[User:Paddy3118|Paddy3118]] 16:44, 11 May 2011 (UTC)

::OK, this makes sense. In which format is the grammar supposed to be? Does it have to be plain text form, or could it also be e.g. in s-expressions, which would be the natural choice in case of PicoLisp.

: The task description links to at least two different EBNF formats. So I cannot know which format to use. I guess that all the solutions will use different formats? --[[User:Kernigh|Kernigh]] 01:02, 12 May 2011 (UTC)

:: I think it should be plain text form, like http://en.wikipedia.org/wiki/EBNF --[[User:Oenone|Oenone]] 08:24, 12 May 2011 (UTC)

== Output form ==

What should the output of the parser be? The rules as supplied don't give any information about result, so ''strictly'' it can only be a recognizer… –[[User:Dkf|Donal Fellows]] 14:33, 12 May 2011 (UTC)

: For the Ruby implementation, I intend to give error messages about syntax errors. --[[User:Kernigh|Kernigh]] 23:19, 12 May 2011 (UTC)

== Is the example grammar correct? ==

The given calculator grammar uses the identifiers <code>PLUS</code>, <code>MINUS</code>, <code>MULT</code>, <code>DIV</code>, and <code>NUMBER</code> without defining them. Don't they need to be defined? —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 20:54, 12 May 2011 (UTC)

: +1 on that. --[[User:Paddy3118|Paddy3118]] 04:55, 13 May 2011 (UTC)

== PicoLisp grammar is PicoLisp not EBNF ==
PicoLisp seems to parse PicoLisp rather than EBNF. This makes for a shorter program, but, if allowed, then the task description should state something like ''"parse an EBNF-like input grammer expressed as code for your programming language"'' to be fair. --[[User:Paddy3118|Paddy3118]] 05:05, 13 May 2011 (UTC)
: Well, but that's trivial. A one-line to convert the text form:
```PicoLisp
(for E EBNF 
   (use (@S @E)
      (unless (and (match '(@S : @E ;) (str E)) (not (cdr @S)))
         (quit "Invalid EBNF" E) )
      (put (car @S) 'ebnf @E) ) )
```
--[[User:Abu|Abu]] 07:20, 17 May 2011 (UTC)

== ANTLR parser EBNF or ISO standard EBNF? ==
There is a [http://www.cl.cam.ac.uk/~mgk25/iso-14977.pdf difference]. --[[User:Paddy3118|Paddy3118]] 05:43, 13 May 2011 (UTC)

: Oh, in that case: let's go for ISO EBNF (though perhaps without the rules on exceptions; they're non-trivial to implement and the standard actually says that no grammar may be written that ''requires'' them, making them a shorthand form only). –[[User:Dkf|Donal Fellows]] 09:47, 13 May 2011 (UTC)

== Rewrite?==
How about defining an EBNF-like language here and have the task be to:
# Parse a ''given'' language definition for a simple calculator that goes right down to terminals.
# Parse a given expression including numbers with all digits and traversing all edges in the language graph.
# Parse a modified language definition where NUMBERS do not include 6.
# Print and show that the earlier expression fails to parse.
--[[User:Paddy3118|Paddy3118]] 05:43, 13 May 2011 (UTC)

==Rewrite!==
I've '''been bold''' and changed the task. Tell me what you think. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 22:50, 22 May 2011 (UTC)

==Making this a full task==
Why this task is still not a full task, why it's a draft? Let's make it just a regular task, we seem to have already some implementations. [[User:Avmich|Avmich]] 18:21, 19 January 2012 (UTC)
: There's been a lot of disagreement over what this task was supposed to be; there's no hurry to promote, especially when there is no consensus. (It's not like people solve these things for a full-time job either.) –[[User:Dkf|Donal Fellows]] 21:54, 19 January 2012 (UTC)
:: I think right now the task is clear enough - you get a string with grammar and an input string of the grammar's language, and you should check if the correspondence holds.
:: Promoting would add attention and may be someone would add a solution or two. [[User:Avmich|Avmich]] 23:36, 19 January 2012 (UTC)
::: It's definitely clearer than it was but it still needs work.  The description is a bit thin and some of the questions asked here aren't really answered.  Also with the clarification, it's harder to tell which examples are correct. Which EBNF to use (or if it even matters) if you declare which. The end result is if someone is just skimming along through tasks that might interest them then they have to do more work to understand this task over others.  It's a disincentive.  I think fixing the incorrect examples would do more than just removing draft.  Right now it will look confused if promoted.  I'd also suggest copying out the test input to the main page as there are only two.  The tests page is still valuable for "more information".  --[[User:Dgamey|Dgamey]] 03:06, 20 January 2012 (UTC)
