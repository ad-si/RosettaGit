+++
title = "Rosetta Code:Add a Task"
description = ""
date = 2015-03-04T09:46:45Z
aliases = []
[extra]
id = 8337
[taxonomies]
categories = []
tags = []
+++

{{#set:is contribution page=true}}<!-- prologue. Feel free to help fill this page out; you guys tend to refine this more than I do. This is (currently) mostly a brain dump of my general lines of preference embedded in a structured outline.-->So you want to see a problem tackled, and you know enough about it that you can write a solution or two yourself. If you're beyond simply [[Rosetta Code:Village Pump/Suggest a programming task|suggesting a task]], you can add one yourself.

A task has a very simple layout:

```txt
{{task}}Description of the task

...

Examples
```

==Prerequisites==

### Create the page

Come up with a title for your task (look at [[:Category:Programming Tasks|the current tasks]] to see what kind of name you should choose), type it in the search bar, and click "Go". There will be a "Create page" link on the resulting page somewhere. Click that, and you can begin editing.
===Draft vs non-draft===
Not all tasks are immediately ready to be thrown at the casual Rosetta Code participant. Some need a review or draft phase before they're in good shape.
* For a non-draft task, one would use [[Template:Task]] by putting {{tmpl|task}} at the top of the page.
* To note that a task is a draft, use [[Template:draft task]], by putting {{tmpl|draft task}} at the top, rather than {{tmpl|task}}. If this is your first task, you should probably just start with a draft.

It's up to you to decide which you start with, but another community member may choose to change your created task to a draft. If there is some question on the general suitability of the task then create a draft task and discuss the reason for it being a draft in the talk page. This will warn potential contributors that there may be substantial changes in the task description whilst still in draft status.


### =Reasons for draft status=

Reasons for draft status might include, but not be limited to:
* '''The task is too large.''' If a task can accomplish the same goals with a smaller spec, it probably should.
* '''The task is too general,''' leading to code examples which are particularly difficult to compare.
* '''The task is too specific,''' implementable by too few languages.


### Task focus



### =Things to try for=

Generally speaking, '''the goal is to address a problem a programmer may face or want to think about.''' These include (but aren't strictly limited to):
* '''Practical problems.''' These are problems with may occur regularly in practical application of programming languages, or at least are within the target domain of the language.
* '''Problems which demonstrate concepts.''' These are problems which attempt to highlight particular features, patterns or ideas in programming, and tend to contrast different languages' approaches to solving problems.
* '''Entertainment.''' Some tasks (see [[RCRPG]] or [[24 game]]) are created primarily for entertainment purposes, and represent an aggregate challenge of smaller problems.

As a common theme, '''all tasks must seek to increase competence and understanding of the tools in question''', by example or annotated counterexample, if necessary.


### =Things to avoid=

* '''Don't require a specific language.''' Tasks which specify a particular language will not tend to achieve many useful comparisons or solutions, as languages are the richest resource on Rosetta Code. If your personal goal is to create a task which highlights a feature of a particular language, where that feature is unique or extraordinarily rare, don't create a task where that feature is ''required'' to solve the task. Instead, create a task that can be solved using a variety of means, but where that feature can greatly help in solving it. You may wish to highlight a unique feature of a particular language (yes, there are specific language advocates on Rosetta Code, and that's fine), but nobody will see that feature's usefulness if there are very few other languages for them to compare against.
* '''Don't require exceedingly rare features.''' Requiring unique language features, or rare combinations of features, leads to the same problems as requiring a specific language.
** The caveat to the above, of course, is that '''best-effort solutions are often fine''' Some example solutions can fudge the spec without being inappropriate. ''"This isn't exactly possible in Ayrch, but something practical solving the language's idiomatic analog would be..."''
***Think carefully about adding a later partial solution to a task if a working, full solution exists.
***When giving a best effort solution then state near its top, just what aspects of the task are not implemented, to avoid later editors marking the example as incorrect.
***Consider the removal of a prior partial solution to a task when adding a new full solution to the task.
* '''Avoid creating tasks seeking the smallest possible solution.''' Code golf, or the finding of the absolute most succinct expression of a solution as its own goal, is not often an idiomatic, practical or comprehensible use of the language in question, and so is difficult to justify in a demonstrative context. Strokes are not points.
* '''Avoid creating tasks seeking the fastest possible solution.''' Optimized code is rarely easy to read, and is often exceptionally complex as something to learn from. However, provided it does not make the overall section for the language too long, users may provide a more-optimal solution as an alternative solution to a particular task so that learners can compare it to the “optimized for clarity to people” version that should be the main solution for the language. (We also do not want to see the site devolving into “my language is faster than yours” silliness.)


### Basic information

A task needs a few basic components. It needs a '''simple description of the problem'''. Having a solution to the task allows you to tune the task description such that the run times and/or size of outputs are reasonable.

'''Inline references to specifically-related information''' are important. While off-site links are often necessary (if only for appropriate citation), enough cited, excerpted information should be included such that the task may still be solved.

Where relevant, '''sample input''' should be included; it gives task solvers something to work with.


### Example code

It is usually a good idea to '''have at least one example implementation completed, tested, and working''' ''before'' you start writing the description of the task, as well as '''a sample of correct output.''' It is usually a good idea if this first example shows its output; even if it isn't strictly necessary for the completion of the task, it helps other implementers understand the task and what they need to do.

In short, solve your own task. Show us how it's done.

==Additional information==
===Lurk!===
'''Read existing task descriptions''', and model yours after ones you like. If you follow the Recent Changes feed, you can watch the creation of one, live. Alternately, you might take a look at the edit history of existing tasks (and their talk pages), to get a feel of the process and the (unwritten) "house style."

'''Hang around, answer and solicit questions''' in your task's talk pages, especially in its early days. (It may help to Watch the task's talk page, if you're so inclined) This will ensure that people have enough information to implement the task in other languages, allow you (and them) to fine tune the task description as needed, and ultimately end up with something many can implement without ambiguity.


### Jargon

It helps to '''explain jargon''', as about the only common jargon that's likely be understood would be in the fields of programming or computer science — and even that's not guaranteed (this is an educational site, after all). Be especially aware of unexplained maths jargon, and watch the talk page and other implementations for signs that the task description may not be sufficiently clear.


### Extreme Language

Some schools, libraries and parental filters filter pages whose URLs match wordlists. This even occasionally impacts Rosetta Code's reCAPTCHA API key. We try to include this audience, so please [[wp:bowlderise|self-censor]] such content. (For an example, see the discussion page for language [[Category talk:Brainfuck|Brainfuck]]).
