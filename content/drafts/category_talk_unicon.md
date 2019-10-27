+++
title = "Category talk:Unicon"
description = ""
date = 2012-01-12T01:07:38Z
aliases = []
[extra]
id = 6807
[taxonomies]
categories = []
tags = []
+++

Note: This page is linked to from the [[:Category_talk:Icon]] page.

= How to go about solving the tasks =
== Intent .v. Direction of tasks ==
There was an interesting discussion on [http://sourceforge.net/mailarchive/forum.php?forum_name=unicon-group the Unicon group forum] about meeting the intent .v. literacy in solving tasks.  The [[Loops/Break]] task was used as a point of discussion.  

The consensus seemed to be that we should be meeting the intent and not reading too much into the literal interpretation of the title.  Unless there is specific direction within the task description we should have a free hand.

Thanks to Steve, Andrew, Clint, and Charles [http://sourceforge.net/mailarchive/forum.php?thread_name=4BBDFF4D.9000505%40noao.edu&forum_name=unicon-group for this and other discussions].  The overall consensus is that '''we should show our best'''. --[[User:Dgamey|Dgamey]] 21:17, 8 April 2010 (UTC) 


###  Style 

We should be aiming for programs that are Icon/Unicon like. Not just for translations from something like JavaScript or Basic.
* What should be done with if we find code that looks like a bad translation of something else or even something that could better represent Icon/Unicon.  Personally I think they should be replaced with examples that show how the language can be used.  There may be a case for keeping them as an alternate version and calling out that it is a bad translation; however, given that this site is about showing off the essence of languages - why would you do that?  --[[User:Dgamey|Dgamey]] 21:17, 8 April 2010 (UTC) 


###  Quality 

There are a number of aspects of the sample programs that need improvement.  These include:

* Commenting and documentation.  Many of the examples (not just Icon/Unicon) have very little in the way of supporting comments, documentation, or description.  There really should be a reasonable amount.
* In a couple of cases there are links to IPL modules. Inevitably there will be more.  How best to handle these?  We could copy the code into a separate code box and call it out - but this could get repetitive.  I believe that we should at a minimum place an off site link to the IPL web page for the code.  If the library function is the core of the solution (as in [[Input_loop#Icon|Input Loop]] then it should be included.  But if the function is less important a reference should be sufficient.
::''I don't see it's necessary to give a copy of the IPL library function. To mention on the language page that the IPL exists and then link to the offsite code where applicable should be enough. Java has a huge standard library that is expected to be present with all implementations, as does C/C++ and many other languages. Task implementations gloss over the possibly hundreds of lines of library code all over Rossetacode, as this would be far too verbose and destroy the ability to make quick comparisons, more so with a large comment block for attribution.'' [[User:MattOates|MattOates]] 07:34, 7 April 2010 (UTC)
::''Fair enough I wasn't sure what the overall convention was here.   In this case where the main program does almost nothing and the IPL procedure does all the heavy lifting something more seemed to be needed. Perhaps a note and brief description.  Or perhaps just the link.'' --[[User:Dgamey|Dgamey]] 21:17, 8 April 2010 (UTC) 


###  Multiple Solutions 

What happens when there are multiple good examples of how to solve a task with different approaches?  Should we show more than one?

By way of example, on the [https://tapestry.tucson.az.us/twiki/bin/view/Main/StateNamesPuzzle | Unicon Twiki - States Names Problem] there are at least two different approaches to the problem.  If we were to hit one of those here what to do?

Certainly we could talk about or refer to other solutions.  But, should we show more than one?  My take is that it's not unreasonable to show a couple of good alternate approaches in the same language.  Just don't flood the reader.  --[[User:Dgamey|Dgamey]] 21:17, 8 April 2010 (UTC)


###  Constrained Tasks 

I'm not sure how many of these there may be here.  But using the [https://tapestry.tucson.az.us/twiki/bin/view/Main/LongestStringsPuzzle| Unicon Twiki - Find the longest string problem without using comparisons, math, or lists] as an example, perhaps we should introduce a few.  I'd like to see how some of the more traditional languages do this :)  --[[User:Dgamey|Dgamey]] 21:17, 8 April 2010 (UTC)


###  Level of detail where Icon and Unicon are different  


A number of task writers had taken to explaining some of the fundamental differences underlying Icon and Unicon in individual tasks.  It's better to put the text into the Introduction and refer to it from the task.  See:
[[Icon+Unicon/Intro|To provide detailed introduction to Unicon and Icon that can be referenced in tasks.]]

=== ObjectIcon, Jcon and Other variants ===

I recently discovered Object Icon and will set up some stub pages for it.  
* There will be a link back here for this discussion
* I strongly encourage other contributors to keep all the Icon variants together under one heading set as has been done with Icon and Unicon.

= HELP Wanted =
== Icon/Unicon features ==
* Object oriented examples
* Graphics examples, particularly where we can show differences between Unicon and Icon
* Unicon Execution Monitoring examples, where they make sense

== Programming Tasks not Implemented ==
These will never end but [[Reports:Tasks_not_implemented_in_Icon|Icon - tasks not_implemented]] and [[Reports:Tasks_not_implemented_in_Unicon|Unicon - tasks not_implemented]]  --[[User:Dgamey|Dgamey]] 03:10, 12 April 2010 (UTC)

== Easy Wins ==
I expect that many of these tasks are already coded in the Icon Programming Library or one of the Unicon packages. There are also example in the Icon and Unicon books.  If you find an interesting task, check these sources first.

* [http://www.cs.arizona.edu/icon/library/pdx.htm The IPL permuted index of basic procedures]
* [http://www.cs.arizona.edu/icon/library/gpdx.htm The IPL permuted index of graphic procedures]

== Errors/Review Lists ==
See [[Template:Example-needs-review]] and [[Template:Incorrect]].  I'm not sure how you're supposed to know what is to be reviewed.  Examples I've seen don;t have a lot of explanation.

Needs review doesn't seem to take an argument for formatting.  Please add a brief note after the tag describing why.  You can add more on the discussion/talk page if needed.

* Use <nowiki>{{improve|lang|Explanation}}</nowiki> for code improvement such as in 
** <nowiki>{{improve|Unicon|The example is correct; however, Unicon implemented additional graphical features and a better example may be possible.}}</nowiki>
* Use <nowiki>{{example-needs-review|lang}}</nowiki> when the code may not meet the task description. Especially after a task description change.
* Use <nowiki>{{incorrect|lang|Explanation}}</nowiki> if the code doesn't meet the task.

There are a number of other related templates like improve that can be found [[:Category:Example_attention_templates]].

== The IPL and Uni Libraries ==
[[:Category:Solutions_by_Library]] need writing.  Stubs have been created for:

* [[:Category:Icon_Programming_Library|The Icon Programming Library]] --> [http://www.cs.arizona.edu/icon/library/ IPL]
* [[:Category:Unicon_Code_Library|The Unicon Code Library]] --> [https://tapestry.tucson.az.us/unilib Unilib]

To reference these pages '''without inclusion''' on the library pages use these:

```txt
[[:Category:Icon_Programming_Library|The Icon Programming Library]]
[[:Category:Unicon_Code_Library|The Unicon Code Library]]
```


To reference library code and include the task page in the library use the [[Template:Libheader]]

```txt
<nowiki>
{{libheader|Icon Programming Library}}  
{{libheader|Unicon Code Library}}
</nowiki>
```


Respectively these generate:
<nowiki><noinclude>{{libheader|Icon Programming Library}}</noinclude> and <noinclude>{{libheader|Unicon Code Library}}</noinclude></nowiki>

Notes: 
* The Wiki automatically replaces spaces with underscores when constructing the link.  
* Some pages in the Wiki have a 2nd parameter for libheader; however, it is not used.

== Unimplementable tasks ==
Mark tasks that aren't doable in Icon/Unicon as such.  I believe there is an omit markup in curly braces, but I don't know much about it.

See [[:Category:Icon/Omit]] and [[:Category:Unicon/Omit]]

The syntax is <nowiki>{{omit from|language}}</nowiki>

* An example would be [[Address_of_a_variable|getting/setting the address of a variable]]
* Object oriented stuff in Icon


```txt
<nowiki>{{omit from|Icon}}{{omit from|Unicon}}</nowiki>
```


== Implementations ==
There are Category Pages for Implementations of other languages.  There are certainly several variants that can be described for Icon including Jcon, MT Icon, IDOL, etc.

== Requesting Tasks ==
See [[Rosetta_Code:Village_Pump/Request_a_programming_task]]

= Stuff Done =
== Language Formatting ==

It turns out the <nowiki>
```blahblah>
```
</nowiki
 tags just provides syntax highlighting via something called GeSHi for syntax highlighting.  Although there are Icon and Unicon tags on RC articles, there is GeSHi for either language.   The details on AutoGeSHi are at http://rosettacode.org/geshi/ and maintained by user BenBE.  (Thanks Mike) --[[User:Dgamey|Dgamey]] 03:10, 12 April 2010 (UTC)
* Thanks to [[User:MattOates|Matt Oates]] for getting these done --[[User:Dgamey|Dgamey]] 02:14, 20 April 2010 (UTC)

== Thanks ==
* Clint for valuable insight into everything
* [[User:SteveWampler]] for valuable insight into co-expressions
* [[User:MattOates|Matt Oates]] for work on the syntax coloring templates
* Art E, Andrew C
* [[User:Peter|Peter Lane]] for numerous contributions and helping us cross into the top 20!
* Anyone else I forgot?

= Wiki and Icon/Unicon task Formatting =
== How much to put in the language templates ==
One question that comes to mind when thinking about Rosetta Code is how much to put into the language category pages?

It makes sense not to duplicate material in Wikipedia or on the language home pages.  But it does make sense to provide enough information to readers to make useful comparisons.

While investigating this, the [[:Category:Oz|Oz]], [[:Category:Python|Python]], [[:Category:Tcl|TCL]], and [[:Category:J|J]] category pages were pointed out as good examples.


###  Category Page Language Box 

I had a go at the main box.  Unfortunately a lot of the terms aren't explicitly defined here.  
* byte code compiled as I recall (way before Java made it popular)
* strong typing (This was Griswold's contention based not on declaration but the robustness of the operator framework) I think that this description of typing isn't as useful as it once was
* safe typing (based on robustness of the operator framework)
* implicit type expression

Here are the [[:Category:Type_System|Type System descriptions]] 

Does anyone know of Icon/Unicon BNF descriptions?  The box can reference these and other languages do it.

--[[User:Dgamey|Dgamey]] 23:49, 8 April 2010 (UTC)


###  Programming Paradigms 

Various [[:Category:Programming_paradigm|programming paradigm tags]] can be added to the language box .  I find that some of the descriptions are a bit vague and abstract.  I'm also trying to make sense out of why some pages are Categories and others aren't.


### = Missing and Needed =

* None at this time


### = Clearly applicable =

* [[:Category:Programming_paradigm/Logic_Programming]]
** Goal directed doesn't appear to be on the list and there doesn't seem to be a good fit.  This might also be similar to Logic Programming as languages like Prolog can operate similarly.  
** A quick check of Wikipedia doesn't turn up an article on goal directed programming; although, the Icon article does come up.  See the following:
*** [[wp:Logic_programming]] seems to be it
*** [[wp:Goal-oriented_programming_language]] too vague
* [[:Category:Programming_paradigm/Procedural]] for both
** [[:Category:Programming_paradigm/Imperative]] given as a sub-set of procedural. 
* [[:Category:Programming_paradigm/Event-driven]] applies to the monitoring interface MT-Icon and Unicon, and probably co-expressions.  Although some of the comments on the page such as about poor performance I'm not sure are apply.
* [[:Category:Programming_paradigm/Object-oriented]] Unicon
* [[:Category:Programming_paradigm/Concurrent]] may be a stretch for co-expressions; however, Unicon v12 adds support for mutexes


### = Clearly not applicable =

* [[:Category:Programming_paradigm/Distributed]]
* [[:Category:Programming_paradigm/Functional]]


### = Not clear / Mixed =

* [[:Category:Programming paradigm/Declarative]] if only for OO in Unicon.  SNOBOL4 patterns were given as an example of declarative but I think it's a stretch.  
* [[:Category:Programming_paradigm/Dynamic]] as a paradigm seems to mean scripting but there are also references to dynamic record construction
* [[:Category:Programming_paradigm/Generic]] (sorry I don't do much with OO)
* [[:Category:Programming_paradigm/Reflective]]  Icon and Unicon have features that can inspect program state, so probably yes. As I recall Unicon has more.


###  Language Comparison Table 

A first stab at the LCT
* paradigms = procedural,  object oriented, goal-directed (not defined and is this the same as logical programming)
* not standardized
* strongly typed 
* safe types
* implicit 
* what is typc 
* nominative type compatibility
* dynamic type checking
* parameters by value?  Not really.  Mutable/Immutable.
* Garbage Collecting
* intended use .... a lot could be written here
* design goals .... comments from someone closer to the development would be better than mine Clint?

The [[Parameter_Passing]] page could use an update from a SNOBOL/Icon/Unicon specialist

--[[User:Dgamey|Dgamey]] 23:49, 8 April 2010 (UTC)


###  Other References 

[[wp:Comparison_of_programming_languages|Wikipedia: Comparison of programming languages]]

== How to reasonably handle Icon .v. Union similarities and differences ==

###  Summary 


The writeup of Icon and Unicon programs on Rosetta evolved from lone haphazard additions to a more structured approach.  The rationale behind how this came to be is explained below.  It is however partially constrained by the capabilities of the wiki and limitations of understanding of the wiki. It is also due to the fact that while the two languages are closely related; Unicon is mostly but not always a super-set of Icon. I hope we have finally arrived at a usable format. --[[User:Dgamey|Dgamey]] 15:40, 31 December 2010 (UTC)

: Conversion to this format is ongoing and proceeding alphabetically --[[User:Dgamey|Dgamey]] 14:16, 30 January 2011 (UTC)


###  Note on templates for other variants 

I've stubbed in categories for [[Jcon]] and [[ObjectIcon]]; however, there are no code examples for either at this time.  If you plan on adding them please add the code as per the templates in the sections below using a single Icon/Jcon/ObjectIcon/Unicon header like one of these (assuming all examples are present):

```txt
<nowiki>
=={{header|Icon}}, {{header|Jcon}}, {{header|ObjectIcon}}, and {{header|Unicon}}==
</nokiki>
```




###  Template/ How to write up Icon and Unicon markup 

As a general rule for Icon and Unicon tasks, unless there is separate code or notations about differences the code should run under both dialects.

There are several cases where Icon and Unicon are marked up differently:

* the same code (or nearly the same code is usable with only minor differences requiring comment)
* there is a significant benefit to a Unicon only solution that requires a separate example
* there is no Icon solution provided (or possible) and the Unicon solution makes use of significant features exclusive to the dialect

A template for the general case is shown below.  It has the following characteristics (a) the listing in a task is concise and visibly shows that the solution is the same, (b) both languages are indexed by the header macro, (c) the Icon language template is used for keyword coloring, etc., (d) someone searching the page for either language will find them with a text search, (e) where pages/indices are split the code shows in the first half of the alphabet making it more accessible.

''Note: many examples in Rosetta use the second format as this general format arrived somewhat late on the scene.  Eventually they will get converted.''

```txt
<nowiki>
=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
# pure Icon code
...
end
```


{{libheader|Icon Programming Library}}  
[http://www.cs.arizona.edu/icon/library/src/procs/XXXX.icn XXXX provides YYYY] 
{{libheader|Unicon Code Library}}

Output:&lt;pre&gt;sample output&lt;/pre&gt;</nowiki>
```


The second case is where Icon and Unicon examples are ''significantly'' different. In this case the difference will be visually obvious in task indices.
Note: this was used as the general format from April 2010.

```txt
<nowiki>
== Icon and Unicon ==
=
## Icon
=

```Icon
procedure main()
# pure Icon code
...
end
```


=
## Unicon
=

```Unicon
procedure main()
# Unicon code
...
end
```


{{libheader|Icon Programming Library}}  
[http://www.cs.arizona.edu/icon/library/src/procs/XXXX.icn XXXX provides YYYY] 
{{libheader|Unicon Code Library}}

Output:&lt;pre&gt;sample output&lt;/pre&gt;</nowiki>
```


In the third case, only one solution is presented.  At this time it is unlikely there will be an Icon only case.  Unicon only solutions should be presented as shown below.  If it is the case that the solution isn't possible in Icon then an omit template should be coded as well.  Using this structure will keep the look and feel of the task index, be properly reflected in tasks implemented/not implemented on the language category page, and allow someone to provide an Icon solution at a later date with minimal modification by converting to the second format.


```txt
<nowiki>
==Icon and {{header|Unicon}}==
The following code uses features exclusive to Unicon.


```Unicon
procedure main()
# Unicon code
...
end
```


{{libheader|Icon Programming Library}}  
[http://www.cs.arizona.edu/icon/library/src/procs/XXXX.icn XXXX provides YYYY] 
{{libheader|Unicon Code Library}}
</nowiki>
```



###  Template / Other Useful Markup 


### = References to the Icon Programming Library =

Use the following for IPL references.  Note: if the IPL module solves the task, you should also include the critical code from the IPL so that a user of the site can see how the task was met without following offsite links.  This isn't needed if the module is only incidental.

```txt
<nowiki>
[http://www.cs.arizona.edu/icon/library/src/procs/XXXX.icn XXXX provides YYYY] 
</nowiki>
```


During the header and works with cleanup I noticed IPL references are all over the place and some are not formatted nicely.  Some tips/suggestions:
* The library category markup formats new lines before and after.  Placing text around it will not format cleanly as a sentence.  Treat it like a heading.
* The text in the offsite links is inconsistent and probably needs cleaning up.  Just make clear the name of the file included and procedure names if you go to that much detail.  "Uses YYYY, ZZZZ from XXXX", "Requires XXXX", and "XXXX provides YYYY" all work.
* I thought about using bullets, but it's overdoing it.
--[[User:Dgamey|Dgamey]] 14:25, 30 January 2011 (UTC)


### = Unimplementable =

Use one or both of these macros if the task cannot be implemented.

```txt
<nowiki>{{omit from|Icon}}
{{omit from|Unicon}}</nowiki>
```


### = Needs improvement =

The needs improvement tag can be used to mark possible improvements and give a reason. 

```txt
<nowiki>{{improve|Unicon|The example is correct; however, Unicon implemented additional graphical features and a better example may be possible.}}</nowiki>
```

==== Works With (Do not Use) ====
The works with macro provides standard text.  '''Beyond that I'm not sure what advantage it has with regard to indexes or special pages?'''

```txt
<nowiki>{{works with|Unicon}}</nowiki>
```


The full syntax is below.  At this point I don't see a use for this in Icon/Unicon tasks.  

```txt
<nowiki>{{works with|language|version}}</nowiki>
```


'''If you are cleaning up the Icon/Unicon headers, please don't remove either and add a works with.  Works with appears not to add the task to the language so the category pages will be wrong.'''  Thanks (Someone in a well intended cleanup effort introduced some of these and dropped the Unicon header).
:You definitely don't need the works with template if you're using the header template for both language names. The header templates will add the task to the language categories. The works with template will only link back to the Unicon page (already linked to by the header). --[[User:Mwn3d|Mwn3d]] 16:39, 3 January 2011 (UTC)
::Thanks for clearing this up.  However, the task count on most linked to pages dropped when works with was used instead of a header. --[[User:Dgamey|Dgamey]] 17:21, 3 January 2011 (UTC)

The template suggests this may be incorrect usage, see [[Template:Works_with]].  Specifically it says "Do not use this template to merely indicate what language implementation you used, ''but only if the example relies on features only a particular implementation/variant has.''"

I really don't know what goes on behind the scene with this.  
: For now I am including it in the template. --[[User:Dgamey|Dgamey]] 03:11, 3 January 2011 (UTC)
:: It's now been removed from the template.  I guess I have some cleanup to do (A-I entries). --[[User:Dgamey|Dgamey]] 17:21, 3 January 2011 (UTC)
::: cleaned up --[[User:Dgamey|Dgamey]] 14:13, 30 January 2011 (UTC)

--[[User:Dgamey|Dgamey]] 15:53, 31 December 2010 (UTC)


### = Translation of =
 

```txt
<nowiki>{{trans|language}}</nowiki>
```

--[[User:Dgamey|Dgamey]] 11:10, 23 June 2011 (UTC)


###  Template for new tasks 

The following quick copy and paste may be helpful creating new tasks.

```txt
<nowiki>== {{header|Icon}} and {{header|Unicon}} ==

```Icon


```



```Unicon


```


Sample Output:
...

{{libheader|Icon Programming Library}}  
[http://www.cs.arizona.edu/icon/library/src/procs/XXXX.icn fib provides YYYYY] 
</nowiki>
```



###  When to have separate examples 

Next is the question of when to have a separate Unicon example?  
* Clearly anything that makes use of a major extension should have a separate example. A simple example of this is [[HTTP#Icon|HTTP - Icon]] .v. [[HTTP#Unicon|HTTP - Unicon]].
* But what of simple syntactic sugar and conveniences?  Certainly they should be shown, but if that's the only difference between the Icon and Unicon examples, I'm inclined to say no. Consider the examples below.
* Differences in program behavior are worth documenting. By way of example [[Loops/Break]] contains a note that describes how the Icon/Unicon random number generators differ in behavior.

### = A poor reason for both versions =


Unless the task were parameter passing, type checking, and initialization the following would be a poor reason to have both versions.

```txt

#
#  Icon implementation of foo
#
procedure foo(i)  
local bar
i := integer(i)
bar := 0
....
return bar
end

```

and

```txt

#
#  Unicon implementation of foo
#
procedure foo(i:integer)  
local bar := 0
....
return bar
end

```
 

--[[User:Dgamey|Dgamey]] 02:21, 14 April 2010 (UTC)


###  Background / How this evolved 


### = Similar languages and the wiki at a high level =

The differences between Icon and Unicon programs can range the from obvious to very subtle.  It can include entire paradigms, semantic differences, syntactic differences, and operational/behavior differences.  A key challenge on Rosetta Code will be to fairly represent these without (a) duplicating everything and (b) under representing differences.

I don't believe there is an ideal way to do this.  Here are some of the considerations:

* The wiki is only able to represent things a certain way and we need to work within the available framework
* Most of the programs entered under Unicon were more strictly Icon programs that also ran under Unicon
* There doesn't seem to be a useful way to have two very closely related languages:
** Separate languages mean that contributors can take advantage of built in reports such as the [[:Category:Unimplemented_tasks_by_language]] or [[:Category:Unicon_examples_needing_attention]].  Combining the two would loose some of this.
** Placing the two together everywhere they appeared might work but would be at risk of being sorted either manually or automatically (in the future).
** '''''The two could be placed together if they had similar names, say Icon_(original) and Icon_(Unicon).  We'd probably want a redirect as well from Icon and Unicon. However, I suspect there are places where the underlying naming will follow through.  Otherwise if the links could be maintained alphabetically, then this might work.  Feedback on this could be useful.'''''


### = Consistency and Eliminating Duplication =

It's desirable to present all Icon and Unicon code consistently.  This discussion was started to help achieve that goal. To that end, there are some objectives:
* Eliminate duplication 
* Present the languages as closely related noting important differences
* Be able to easily tell when the code is Icon/Unicon
* Not confuse people new to the language

My initial attempt at this involved creating separate Icon and Unicon sections.  Moving the code to the best fit.  Often to Icon. Back/Forward referencing to/from the other section.  This had several disadvantages including:
* separation of code
* inability to use the unimplemented tasks report or otherwise easily tell if the code was base Icon or exploited Unicon extensions

Next came realizations about how the wiki worked. 
* Manual co-grouping would be subject to manual or (future) automated sorting resulting in confusion and inconsistency. 
* Namely the <nowiki>{{header|language}}</nowiki> markup was responsible for registering completed tasks.  The heading levels were not tied to this.
* Using the unimplemented tasks report to track unicon extensions separately was potentially confusing to new *Icon programmers.


### == Discussion on how this evolved ==

: May I suggest creating templates for these various references so that the wording can be adjusted uniformly if the need is felt? (Don't put the <nowiki>==...==</nowiki> in a template, though, because that breaks section editing's ability to pick out the right sections.) Also, insofar as Unicon is ''not'' actually Icon (I don't know either language so I can't judge this), for the second sort of cross-reference, use a header of the form ==[[Icon]]== instead because that will leave it on Icon's unimplemented list for people to work on as an Icon-only example. —[[User:Kevin Reid|Kevin Reid]] 11:22, 11 April 2010 (UTC)
::This is a good idea.  I'm still not entirely happy with the way this works.  I'd really like to be able to group Unicon and Icon side by side.  But without doing something odd with the names the risk is that they would get moved at some point.  --[[User:Dgamey|Dgamey]] 17:29, 11 April 2010 (UTC)
::: If the formatting and language/task recognition through <nowiki>{{header|somelanguage}}</nowiki> are separate this suggests a way to group the two languages together in a more satisfying and useful way.  I'll post an update later. --[[User:Dgamey|Dgamey]] 02:31, 12 April 2010 (UTC)
:: As far as Unicon and Icon being different.  Unicon provides some very significant extensions to Icon.  There are also some niceties and syntactic sugar.  A very few things behave differently (like random()).  Having said that there are also a few things that won't directly move from Icon to Unicon.  Probably 99% or more of Icon programs will run unmodified under Unicon.  --[[User:Dgamey|Dgamey]] 17:29, 11 April 2010 (UTC)
