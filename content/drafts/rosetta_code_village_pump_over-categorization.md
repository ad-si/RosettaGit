+++
title = "Rosetta Code:Village Pump/Over-categorization"
description = ""
date = 2011-06-24T17:50:04Z
aliases = []
[extra]
id = 9831
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Over-categorization
|summary=Discussion on the new task categories that have been sprouting up lately
}}
I am not very happy with a lot of the new task categories that have been created lately. Lots of them seem to be too specific. Some of them don't seem necessary. I think we need to stop blindly and unilaterally categorizing tasks and plan some of them out instead. We need to talk about how to organize these tasks in a way that best benefits the users. Some examples include (and I don't mean to pick on [[User:Markhobley|Markhobley]], but I know he has created a lot so I looked through his for examples):

Too specific:
*[[:Category:Joystick]]
*[[:Category:Internet Protocol 4]]/[[:Category:Internet Protocol 6]]

Unnecessary:
*[[:Category:Arithmetic]] ([[:Category:Arithmetic operations]] was enough]])
*[[:Category:Loop modifiers]] (see: [[:Category:Loops]])
*[[:Category:Scope]]
*[[:Category:Joystick]]
*[[:Category:Initialization]] (this doesn't look like it has the right name)

We need to come up with the way we want to do this. We can use semantic mediawiki to tag tasks (which was a goal for that system here). If we do that we need to figure out how exactly that can happen. We can keep using categories. If we do that I think we need to decide if we want a tree, how deep it goes, how we want it organized, or, if we don't want a tree, what flat categories we want to use. --[[User:Mwn3d|Mwn3d]] 02:51, 31 May 2011 (UTC)

:I agree with at least some of these. In particular, a category shouldn't be created unless there's multiple existing full tasks that can be categorized with it, ''and'' it should be distinct from all the existing categories too. –[[User:Dkf|Donal Fellows]] 10:55, 31 May 2011 (UTC)

Here's another one. –[[User:Dkf|Donal Fellows]] 13:25, 31 May 2011 (UTC)
*[[:Category:Inverted syntax]] (only had [[Inverted syntax]] in it; empty now following edit)
: I'd hold off on clearing some of those categories quite yet; it looks like Mark is moving content from his site to RC, and I'd wager there's already an organizational structure in place. If the concern here is about duplicate categories, I ''seriously'' recommend taking a look at SMW's semantic properties. You can have a property be a subproperty of another, such that Loop Modifiers might appear as a more specific aspect of loops. This is ''exactly'' the kind of problem SMW is designed to be good at. --[[User:Short Circuit|Michael Mol]] 16:37, 31 May 2011 (UTC)
::How do we set up these sub-properties? I think it would be easiest to have a "tag" template that just adds <nowiki>[[tag::argument]]</nowiki> to a page. We can have multiple tag templates used on the page since they stack (see the "implemented in language" property). The structure of those tags can probably be exactly the same as any category structure we come up with. What do we think that should look like? What categories/tags do we start with? --[[User:Mwn3d|Mwn3d]] 16:59, 31 May 2011 (UTC)
:::If I understand it right, each property has its own page (in the Property namespace), and you can assign properties to those pages as well. See [[smw:Help:Properties and types]] --[[User:Short Circuit|Michael Mol]] 18:55, 31 May 2011 (UTC)
::::What do we do with that? Make like a "One level up" property? --[[User:Mwn3d|Mwn3d]] 19:12, 31 May 2011 (UTC)
::::: It looks like [[smw:Property:Subproperty of]] has the specific information of interest. Add <nowiki>{{#set:Subproperty of=some other property}}</nowiki> to the property page in question. You might have [[Property:Inverted syntax]] as a subproperty of [[Property:Syntax modification]], which in turn would be a subproperty of [[Property:Task subject]]. Then a semantic query for everything in [[Property:Syntax modification]] would list everything directly part of that set, as well as anything in in [[Property:Inverted Syntax]]. --[[User:Short Circuit|Michael Mol]] 20:17, 31 May 2011 (UTC)
::::::I tried to set something like that up using {{tmpl|tag}} and [[Property:Tag]]. It doesn't seem to be working the way I thought it would. Do the values for the tag property need to be properties too? It seems like this is gonna be one of those things where it takes lots of work to set it up but then after that it's awesome. --[[User:Mwn3d|Mwn3d]] 16:48, 1 June 2011 (UTC)
::::::I also just tried using the #show parser function. I get weird error messages like this (you have to click the little warning sign): {{#show: [[Rosetta Code:Village Pump/Over-categorization]] | ?topic}}
:::::: I was intending to use that function to put a tag box at the bottom of tasks. --[[User:Mwn3d|Mwn3d]] 17:50, 24 June 2011 (UTC)
