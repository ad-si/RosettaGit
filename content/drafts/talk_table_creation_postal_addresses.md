+++
title = "Talk:Table creation/Postal addresses"
description = ""
date = 2018-02-25T16:33:39Z
aliases = []
[extra]
id = 2047
[taxonomies]
categories = []
tags = []
+++

Again this is a nebulous task.  Are we talking only database servers or a structured data file in any language?
:Also, address formats vary by country. (Yes, really!) For the purposes of comparison, additional constraints on the task should be made. —[[User:Dkf|Dkf]] 20:31, 10 May 2009 (UTC)
::OK, I've tried to clarify this task to allow more languages to participate and to make the results easier to compare; after all, the point isn't to write full business logic for a global application, but rather to show off table creation. —[[User:Dkf|Dkf]] 11:54, 24 May 2009 (UTC)

==Rename this task==

TL;DR:
# The problem domain ("Database") needs to be in the title to avoid ambiguity.
# The example data set used ("Postal addresses") doesn't need to be in the title.
# Therefore I propose to rename this page to "<b style="color:red">Database/Table creation</b>".


Regarding (1): The word "Table" is used for many different concepts in programming, and "table creation" could refer to completely different things, like creating a hash-table data structure, or generating a nicely formatted ASCII table, etc. The title needs to make it clear we're talking about database tables here.

Regarding (2): The problem to be solved, is to create a database table using some common field types. "Postal addresses" was simply chosen as the data set used to showcase this problem in a practical way. The task wouldn't be fundamentally different if it asked to create a database table for any other kind of practical data set. So there's no need to include it in the title.

Any objections? --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 12:52, 31 July 2016 (UTC)

== Database language? ==
It seems the creator of the tasks had Oracle or SQL Server in mind. But statistical software is often used to store addresses. Even Excel is used for such data. A question then: is it allowed to simply create an empty table with such software? There is already a case: while SAS sems to be using SQL, it actually creates a "classic" SAS dataset, with integrated SQL syntax. Along with SAS, R and Stata are perfectly able to store data tables, though none of them may be considered a database language. I'll add a Stata example to show this.

[[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 21:13, 15 December 2017 (UTC)
