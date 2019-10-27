+++
title = "Talk:Topological sort/Extracted top item"
description = ""
date = 2011-08-04T21:02:12Z
aliases = []
[extra]
id = 8427
[taxonomies]
categories = []
tags = []
+++

==Reasons for draft status==
I think it is worthwhile as this task extracts and orders just those items necessary to compile a given top level whereas [[Topological sort]] would order everything. Hopefully you do too? --[[User:Paddy3118|Paddy3118]] 19:48, 6 October 2010 (UTC)
: This seems like a variation on a theme.  But I have to ask if my current J implementation satisfies your intent for what you wanted displayed?  (If not, could you update the task description with whatever additional requirements?)  --[[User:Rdm|Rdm]] 16:56, 13 October 2010 (UTC)

::Yep, it is a variation on a theme, but during development, it could be that all items could not be compiled but the items for one particular top level might be compilable. Extracting and compiling just the items for a particular top level allows work to continue. It looks as if the J output is OK. --[[User:Paddy3118|Paddy3118]] 20:38, 13 October 2010 (UTC)
::: When I first saw this, it brought to mind code optimizations where a relaxation of complex ordering rules could reduce processing time, yet still provide results good enough to satisfy core requirements in problem context. I'll be watching with interest to see where specializations for this task crop up. --[[User:Short Circuit|Michael Mol]] 20:46, 13 October 2010 (UTC)
:::: Hypothetically, we could attempt to implement a specification where we attempt to put "close to equal numbers of files" at each level.  The concept of sorting against the dependency graph allows this kind of flexibility in some cases, and the example dependency structure is one of those cases.  However, solutions to packing problems often have painful resource requirements for large data sets and/or cases where they do not produce the best results.  --[[User:Rdm|Rdm]] 18:49, 15 October 2010 (UTC)

:: I spoke too soon. The J example needs to also extract the top levels. --[[User:Paddy3118|Paddy3118]] 20:40, 13 October 2010 (UTC)

::: What does this mean?  Do you want me to copy my argument that gives me the list of targets to the output, as an extra line?  --[[User:Rdm|Rdm]] 20:51, 13 October 2010 (UTC)
:::: Given the definition of a top level file and the dependencies, extrat the top levels from the dependencies according to those rules. --[[User:Paddy3118|Paddy3118]] 15:45, 14 October 2010 (UTC)
::::: I am sorry, but I am still not understanding you.  So, I am going to ask some questions whose answers, I hope, will help me understand what you are getting at:  What do you mean by "extract"?  How would my result be any different than what it currently is?  What part of the task specification have I failed to satisfy?  Thanks! --[[User:Rdm|Rdm]] 16:05, 14 October 2010 (UTC)
::::: Ok, I have finally noticed the line in the python display which reads: "The top levels of the dependency graph are: top2 top1".  But please note that the current task description does not ask for this to be displayed, and (I do represent this information internally, in the J implementation).  Also, as near as I can tell, identification of the top levels is completely independent from displaying the compile order (it will be the same regardless of which files we are compiling).  That said, if you are willing to update the task to ask that this be displayed, I will update my J implementation to display it.  --[[User:Rdm|Rdm]] 16:12, 14 October 2010 (UTC)
:::::: Specifically: the top levels are identified by <code>(+./<+./"1)depends</code>.  And the names of the top levels would be <code>names#~(+./<+./"1)depends</code>, and I could format and display them.  But it is conceptually simpler to extract the top levels directly from the raw dependencies, and to ignore this topology.  --[[User:Rdm|Rdm]] 16:29, 14 October 2010 (UTC) (updated 20:59, 15 October 2010 (UTC))

Hi Rdm. The description includes this definition of what makes a top level:
:A top level file is defined as a file that:
:# Has dependents.
:# Is not itself the dependent of another file 

And also item one of the task description asks for a routine that can calculate all top levels for a given set of dependencies:
:The task is to create a program that given a graph of the dependency:
:# Determines the top levels

I could change this to read determine ''and show'' the top levels if this would be more clear. Determining the top levels is a useful function in practice as top levels can be 'forgotten' in documentation if they are not primary, but may still remain useful. --[[User:Paddy3118|Paddy3118]] 21:49, 14 October 2010 (UTC)

:Ok, I have added "top level support" for J.  However, I could not see any way to make this result relevant to the compile order, and it was quite simple to implement without reference to my dependency structure (the <code>depends</code> value), so I implemented it as an independent word.  --[[User:Rdm|Rdm]] 18:40, 15 October 2010 (UTC)

== Python output ==

Isn't it actually wrong? --[[User:Ledrug|Ledrug]] 01:31, 4 August 2011 (UTC)
:What is wrong with it?  The task allows for some variation in which items are compiled in which pass.  And a quick glance did not show any missing dependencies nor did I spot any files which were compiled before their dependencies were compiled. --[[User:Rdm|Rdm]] 20:37, 4 August 2011 (UTC)
::Yeah ok.  I was wondering why its output is different.  The C and J answers decide compile level of an item by how many levels of items it depends on, while Python code decides on how far away it is from the top level item.  The former compiles an item as soon as possible, the latter is the opposite, but both are fine in resolving dependencies. --[[User:Ledrug|Ledrug]] 21:02, 4 August 2011 (UTC)
