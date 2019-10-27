+++
title = "Rosetta Code:Village Pump/DB vs Programming Languages"
description = ""
date = 2010-11-28T17:25:19Z
aliases = []
[extra]
id = 7073
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=DB vs Programming Languages
|summary=DB languages should be separate from programming languages
}}
== DB vs. Programming Languages == 

I think that DB languages, such as SQL, should be different from the programming languages. It'd be the same if CUDA was introduced. You're practically able to call the Databases from any language available, with the '''SAME''' syntax. 

Ie. I think it'd be far more interesting to have some examples in each programming language of how to "Execute a command", and "Do a select" and just leave it there. Then have examples for each DB system of how to create tables and databases, create users and so forth - since they all fall under the "Do a command" type of thing.

EDIT:
Btw, it'd be the same with CUDA. Each language initiates CUDA connections differently, but the CUDA commands are card-specific, and thus do not vary across a multiple of platforms and languages. :P

Thoughts?
--[[User:LordMike|LordMike]] 06:37, 18 April 2010 (UTC)

: It's a mess. It's a lot less messy than it was. Assistance with doing this is welcome.
: That said, there are languages which embed database access in more deeply than others, even if we don't yet have examples of them (I'm thinking of MUMPS here) so I'd like to avoid requiring SQL for the "database" tasks if not strictly necessary. –[[User:Dkf|Donal Fellows]] 07:12, 18 April 2010 (UTC)

:: Hmm, perhaps add f.ex. MUMPS as a DB *and* a Programming language then?
:: I think it'd be practical, as everything else is redundant.. Copy paste would be the preferred tools for people writing database queries... :P 
:: --[[User:LordMike|LordMike]] 07:58, 18 April 2010 (UTC)
: I'd be careful lumping DB languages together, or even SQL variants. DB2 code entirely directly compatible with MSSQL, which isn't entirely compatible with MySQL, etc. Also, there's a relationship with declarative languages in there that I haven't quite figured out yet. Some languages support the same basic query concepts, but use ''very'' different syntax. (The last time I played with Access, for example, the queries reminded me of bangpaths.)
: Most of it (DB2 vs MSSQL vs MySQL, for example) could be handled with liberal application of the workswith template, but better tracking of support might require something akin to the Library template. Also, the stored procedure languages are a different beast, entirely. --[[User:Short Circuit|Michael Mol]] 14:42, 18 April 2010 (UTC)
