+++
title = "Talk:Collections"
description = ""
date = 2013-05-13T05:05:35Z
aliases = []
[extra]
id = 1769
[taxonomies]
categories = []
tags = []
+++

"Collections", as a task, is far from being clearly defined. Can someone add a brief description or a list of task requirements? --[[User:Shock|Shock]] 20:24, 24 January 2007 (EST)

Seconded. It's unclear what "objects" should be in a language that doesn't have any objects, but where all "collections" are polymorphic anyway, and hence can store any type (not only primitive types). And why is it necessary to have this restriction in the first place? To rule out simple C-style arrays? It's also unclear what interface a "collection" should expose (other that than one can add elements). In many languages, collections offer a generic way to unify '''access''' to some concrete container datatype, with operations like ''map'', ''fold'' (sometimes called ''reduce''), or ''filter'' (sometimes called ''select''). Is that what is meant here? If so, is only the generic interface of interest, or also concrete implementations (lists, arrays, various trees)? --[[User:DirkT|Dirk Thierbach]] 18 November 2007
: Clarified.  I removed the type restriction, as well. --[[User:Short Circuit|Short Circuit]] 12:57, 21 November 2007 (MST)

Please clarify in which sense the words "set" and "value" are used. Is set ordered? Are values comparable? Mathematically, array is not a set, it is an ordered set. Do arrays qualify? Further, the word "value" presume "value semantics." Is it an intention of the task? Shall the entities put into the collection be copied upon the operation? --[[User:Dmitry-kazakov|Dmitry-kazakov]] 02:38, 3 June 2008 (MDT)

: Some examples are "marked" but they seems complete, showing various kinds of collection (which is a rather generic term even in computer world); e.g. C++ is the most "complete", to me. D shows something similar, and it is not marked. Java shows lists as collection, and it is not marked (so, it shouldn't be marked C++; at most it should be shortened). Javascript (PHP...) is(are) marked, but it(they) show/s something not so different from Python or Ruby, which are not marked. Similarly for Smalltalk, which anyway shows only an ordered collection (enough to eliminate the Ordered part?)...
: Wanting to add languages like C and Fortran, a simple array or a queue (like FIFO...) seem (sometimes) unable to accomplish the task... Is it so? --[[User:ShinTakezou|ShinTakezou]] 13:42, 2 March 2009 (UTC)

== too much J? ==

The J entry currently provides a variety of collection related options which were not required by the task [so, ok, the task was trivially simple in J and maybe more stuff makes this interesting].  Should they be left there because they may be of interest?  Should they be removed so that the task requirements are followed more precisely?  Or should anything else be done with them (moved to the talk page, turned into distinct tasks, elaborated on, printed on a t-shirt, fed to penguins, ...)?  --[[User:Rdm|Rdm]] 02:42, 3 June 2010 (UTC)

: Nah. Collections seem to be big in J, hence the length of its entry. --[[User:Paddy3118|Paddy3118]] 05:09, 3 June 2010 (UTC)
: Take a peek at [[RCBF]] for what we've done when example groups get very large. However, I'm not averse to the t-shirt idea, myself. :) --[[User:Short Circuit|Michael Mol]] 12:13, 3 June 2010 (UTC)


### Racket and laundry lists

Your right, 7000 is too much to mention, but if there is an easy way to find collections in the languages docs then a link and a note to search for a keyword/phrase that would lead to a large set of the languages documentation on collections would be good. It seems as if collections may be something that makes Racket special compared to other languages? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:05, 13 May 2013 (UTC)

==Problem with OO and C==
Surely C is not an OO language and so should not be said to have classes and methods. Would it not be better to state that there are a number of functions that manipulate strings and are mostly named with a prefix of "str". --[[User:Paddy3118|Paddy3118]] 06:24, 5 June 2012 (UTC)
