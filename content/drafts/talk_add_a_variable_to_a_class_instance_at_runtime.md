+++
title = "Talk:Add a variable to a class instance at runtime"
description = ""
date = 2010-06-02T11:14:26Z
aliases = []
[extra]
id = 4199
[taxonomies]
categories = []
tags = []
+++

==What's exactly monkeypatching?==
Trying to fix Smalltalk... The text explains clearly what is requested... but from where it came the information Smalltalk can do ''that'' monkeypatching? As far as I know, and as far as I know the language,  it can do the ''monkeypatching'' explained in [[wp:Monkey patch|wikipedia]] (and so it can Objective-C), i.e.
<blockquote>A monkey patch is a way to extend or modify the runtime code of dynamic languages without altering the original source code.</blockquote>

This explanation fits with what I know Obj-C and Smalltalk can do, and implies something a little bit different from what is required to accomplish the task; an example could be to change the (NS)String class so that it prints everything reversed always, ... even in already written code... i.e. we changed the behaviour of already existing code without altering the original source code, just adding/modifying a "feature" to '''classes'''.

So, as far as I know (I am not a Smalltalk guru anyway), Smalltalk can't add instance variables to an instance of a class, but only to a class, and all already created instances of that class will take the new variables (or methods, whichever was added), no matter if they were created before or after patching. 

To achieve what requested, one could just create a "special" use-once subclass and '''then''' create the instance of it... but this is not so special, we already know that every OO language can, and it seems to me it would not be exactly what the task wants. --[[User:ShinTakezou|ShinTakezou]] 13:26, 19 May 2009 (UTC)

: The task description is clear enough. Add the new things to an instance of the class without modifying the class (i.e., other instances of the class must be unaffected). What would be a more exciting way to do it would be to dynamically create a subclass of the class with the required modifications and then change the class of the instance ''after creation'' to the subclass. For one thing, that's a sequence of operations that shows that a simple type algebra isn't sufficient to understand the object system… –[[User:Dkf|Donal Fellows]] 01:32, 5 January 2010 (UTC)
::: Almost, but somewhere past in time it changed. Before the changing, it cited Smalltalk among langs able to do this kind of monkypatching à la Pythonistas, and this made the task unclear for a poor smalltalker (what an irony!) like me. Luckly the citation to Smalltalk disappeared but unluckly I am not still able to fix it (I still think Smalltalk can't). --[[User:ShinTakezou|ShinTakezou]] 11:14, 2 June 2010 (UTC)

:: The task description is certainly not clear enough if it means what you say it means, Donal. One reason for disagreement is now obvious to me: in Smalltalk, classes are objects like any other. So a "class instance" is just the run-time representation of a class, as opposed to "an instance of a class". But that's actually a minor point. The major point is that it refers to [http://en.wikipedia.org/wiki/Monkey_patch:Monkey Patch], which on Ruby and Python communities refers to changing a ''class'', not an object. That's so much true that one of the criticism leveled at monkey patching as practiced in the Ruby community is that sometimes different libraries perform incompatible changes to class used by both, and that is very difficult to track.

::So, if something else is meant, I strongly suggest modifying the question to leave no question about it. --[[User:dcsobral|Daniel Sobral]] 02:04, 05 Jan 2010 (UTC)

The page title and task description seem to accurately describe what is needed to complete the task. The use of the word "monkeypatching" is qualified by the use of the phrase "by Pythonistas and some others". This may not be how Monkey patching can be applied in other dynamic languages but the task descriptions use of the qualifier seems to me to make it OK? --[[User:Paddy3118|Paddy3118]] 06:52, 5 January 2010 (UTC)

Hmm, it seems that there's a lot of disagreement of what is actually meant. Meh. (I can patch my objects and classes any way I want; I'll let others worry about theirs.)

== That articles task ==

On the other hand, the task actually described in the article that the task points at is rather interesting in itself, as it involves (in effect) runtime class synthesis based on the header row from a CSV file, followed by filling in the fields for a list of instances of that synthetic class from the remaining rows. Oh, and the synthesized class name is derived from the argument filename too. Probably outside what a standard compiled language/class system can do (and would require interesting bytecode generation for some [[Java]] or [[C sharp|C#]]). Would probably be better done as another task though. –[[User:Dkf|Donal Fellows]] 11:06, 5 January 2010 (UTC)

:Yeah, I saw that. It looks, in fact, somewhat like what the Smalltalk algorithm was doing. Yes, it would be better off as a new task. Now, I'm newbie here, but I think this task's description should be revised to more clearly specify what most of the answers are doing. -[[User:dcsobral|Daniel Sobral]] 16:50, 5 January 2010 (UTC)

I think the articles task only makes sense if certain data manipulations are also input from an external source and the manipulations are in terms of the column names of the CSV file. Something [[wp:SQL|SQL]]-ish or [[wp:XPath|Xpath]]-ish, but ''very'' easy to parse as it is not about the parsing. 
On the parsing, we could even describe a simple space separated file format as a subset of the CSV format and use that.

Given a task like that though, you don't need to use dynamically modified classes to solve it. (But maybe that is the right thing to do. Give a slightly more involved task and leave it up to the implementers to work out how to solve it)? --[[User:Paddy3118|Paddy3118]] 06:10, 6 January 2010 (UTC)
