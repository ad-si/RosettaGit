+++
title = "Talk:Table creation"
description = ""
date = 2019-01-22T22:11:39Z
aliases = []
[extra]
id = 2046
[taxonomies]
categories = []
tags = []
+++

== nebulous task ==
This is a nebulous task.  Are we talking strictly SQL servers??  Or are we talking creating a table ( a structured data file ) in any language?

: I kinda instinctively assumed that this was referring to SQLalikes only. Now that you mention it, though, it could be read as if it means "rectangular data structure of some form". Dunno. I'm in favor of some kind of "subsection" for DB languages (a tag, a category, whatever) so that articles like this could be clearly marked as "in a DB sense" or "not in a DB sense". [[User:Sgeier|Sgeier]] 02:26, 3 May 2007 (EDT)

:: I read this the other way, ie that it is not SQL related, and the task speaks about different data types, so this appears to be a table type for the language, albeit the language may use SQL for a solution, It doesn't look like a requirement. --[[User:Markhobley|Markhobley]] 18:59, 20 June 2011 (UTC)
::: I created the original task. The intent at the time was to broaden the selection of tasks on RC (we had fewer than twenty, and I'd written all of them) to try to add support for SQL languages to the site by defining a task which was a common necessary activity in those languages. At the time I had no concept of the dizzying array of different language domains and paradigms that exist. If the task can be scrapped and replaced with something else more appropriate (common activities in data storage, retrieval and processing languages were what I was shooting for), then by all means, go for it. --[[User:Short Circuit|Michael Mol]] 19:22, 20 June 2011 (UTC)

:::From the [http://rosettacode.org/mw/index.php?title=Table_creation&action=historysubmit&diff=1836&oldid=1670 first edits] it seems the task is to create a database table but not necessarily using SQL. --[[User:Paddy3118|Paddy3118]] 19:24, 20 June 2011 (UTC)

==Redundant==
Is this task redundant?  It's along the same lines as [[Table Creation - Address]], but more generic. --[[User:Short Circuit|Short Circuit]] 21:13, 12 November 2007 (MST)
:Agreed. This is the task to keep, since it points out all the data types allowed by the relational langauge/database. --[[User:IanOsgood|IanOsgood]] 10:23, 13 November 2007 (MST)
:: But the task is just too nebulous right now. The other one is much easier to compare across different languages —[[User:Dkf|Donal Fellows]] 12:49, 8 July 2009 (UTC)

==Draft status==
Changed the task too draft status as it is not clear that this task is about SQL table creation from its description. --[[User:Paddy3118|Paddy3118]] 05:19, 6 December 2010 (UTC)
: I meant to respond to this months ago...See my comment above. --[[User:Short Circuit|Michael Mol]] 19:22, 20 June 2011 (UTC)

==Turning this task into something useful==

As others have pointed out, a "[Database] Table creation" task is redundant with [[Table_creation/Postal_addresses]] which is more practical, less fuzzily defined, and therefore a better task.

Based on the current task description and the original task author's own solution (PostgreSQL), it appears the point of this task here was to showcase the different data types supported by database languages, in a similar fashion to how tasks like [[Conditional_structures]] showcases language features of general-purpose programming languages.

In this spirit, I propose to:

# Rename the task to <b style="color:red">Database/Data types</b>.
# Either restrict the task to database languages, or make it a dual-purpose task:
** Database languages get to show what data types they support.
** General purpose languages get to show how their native data types map to the data types of a common database solution (e.g. SQlite).


(See also my comment in [[Talk:Table_creation/Postal_addresses]].)

--[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 13:09, 31 July 2016 (UTC)

==J code is broken==

It can be clearly seen right in provided example, that "sortBy" feature does't work. Also, seems, that passing another argument to "sortBy" parameter leads to table column sorting instead of row sorting. I'm at very beginning of learning J, so can not provide fix right now. [[User:CoruNethron|CoruNethron]] ([[User talk:CoruNethron|talk]]) 5:12, 14 December 2018 (UTC)

: Thank you for noticing and reporting this error. (Fixed.) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:09, 14 December 2018 (UTC)
