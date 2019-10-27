+++
title = "Rosetta Code:Village Pump/Semantic Solutions with Forms"
description = ""
date = 2010-10-24T02:51:33Z
aliases = []
[extra]
id = 8559
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Semantic Solutions with Forms
|summary=Feedback requested for forms that would allow semantic input of solutions for tasks. This page itself is using a tiny part of SMW's power, using a rudimentary form, and including itself into the [[Rosetta Code:Village Pump]] using [http://semantic-mediawiki.org/wiki/Help:Properties_and_types '''semantic properties'''].
}}
I am in the midst of designing a semantic form giving a solution to a task. This will force solutions to be properly tagged and categorized by their properties in a consistent manner. The best way to demonstrate this is by showing what I have so far.

[[Sandbox/Realazthat/Hough_transform/C]] is the solution page. Notice that there now an "edit with form" tab next to the normal "edit". According to this design, each solution will have its own '''solution page''', and will be transcluded  into the '''task page''', which will itself use a form for input. 

'''Design:'''
* Solutions associate themselves with tasks using a '''semantic property''', part of the new Semantic Wiki extensions that are part of the wiki.
** This property is set based on the '''For task''' section of the form. 
** Aside from such a '''semantic property''', by convention, solutions will be subpages of their respective task.

'''Some things that I already know needs to be fixed/improved:'''
* Many of the fields need to be restricted to certain values, instead of simple string input (prone to error):
** The '''For task''' field needs to be restricted to existing tasks
** The '''Lang''' field needs to be restricted to existing languages
** The '''Translation of''' field needs to be restricted to existing solutions (of this task, if possible)
** ... And so on
* The '''Example usage''' field should have an additional optional language selection (eg. bash, etc., or if used within a programming environment etc.)
** Same with the '''Output'''
* Add links to adding possible values to restricted fields listed above.
*: Example: Libraries are restricted to existing libraries, but frequently a library required will not be an exist, so there will be an "Add library" link next to the field. Same goes for the '''Language''' field.
[[User:Realazthat|Realazthat]] 23:27, 23 October 2010 (UTC)
