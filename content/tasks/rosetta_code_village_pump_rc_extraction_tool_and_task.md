+++
title = "Rosetta Code:Village Pump/RC extraction Tool and Task"
description = ""
date = 2019-05-10T21:21:58Z
aliases = []
[extra]
id = 9520
[taxonomies]
categories = ["task", "Category"]
tags = []
+++

## Task

{{Vptopic
|topic=RC extraction Tool and Task
|summary=Extracting material from RC specific to one language}}
==Idea==

I would like to be able to include all RC tasks and documentation for a specific langauge (Language of Your Choice = LOYC) in a language distro. In a Contributions/RosettaCode folder.

This could be pulled manually, but ...

There are a bunch of packaging issues including documentation (like task name URL, etc.) that need to be worked out.

Some automation would help but it isn't straight forward and some of it may require more/different access to RC.  Getting the automation to do most of the heavy lifting is one possibility, another is to use it to make manual extraction (what changed since the last pull) easier.

Possibilities:
*  Extract all the pages with code for LOYC
*  Extract just the task description and language segment
*  Extract the date the language segment last changed
:*  The bits between any <nowiki>
```XYZ
...>
```
</nowiki
 tags --[[User:Dgamey|Dgamey]] 17:30, 2 May 2011 (UTC)
*  Extract the code segments into separate files.

Doing all this from the html could be messy.
:  It should be possible to extract the raw wiki data using the Edit tab and then abandon the page/edit.--[[User:Dgamey|Dgamey]] 01:59, 12 May 2011 (UTC)
:: The RC count example provides the right level of access for this.  --[[User:Dgamey|Dgamey]] 10:44, 27 May 2011 (UTC)

A read only access to the wiki editor might be easier.

Building some kind of automatic SMW template/pages might make this information more accessible.

Thoughts are appreciated. --[[User:Dgamey|Dgamey]] 13:33, 2 May 2011 (UTC)
:What do you mean by "language segment"? There are big plans (not very detailed or solid plans, but plans) to use SMW to add examples to the site. The idea is/was to have each example as its own page with SMW tags for what language it was in, what task it was for, probably a subheading if necessary (e.g. "Iterative" or "Recursive"), and other metadata. A task page would then transclude the content from example pages which implement that task. "Transclusion" was a new concept for me so I will explain it in case it's not clear. Transcluding a page basically copies its wiki-rendered content on to a destination page. In this case it would take the content from an example page and place it (in alphabetical order by language) on the task page. Hopefully we could get edit links to link to the example pages and other little things like that to work, but editing the actual task page would not be an option to edit an example. The task page content would just be <nowiki>"{{task|Category}}Description description. Example input/output. {{template to transclude examples by task name}}"</nowiki>. If that sort of system were put in place, the ideas listed here would probably be simple SMW queries. --[[User:Mwn3d|Mwn3d]] 15:40, 2 May 2011 (UTC)
:: That would probably work.  But it sounds like it's a way off.  --[[User:Dgamey|Dgamey]] 17:30, 2 May 2011 (UTC)

:: I will probably prepare an example and draft task on this.  In that order. If RC changes it should not be a huge re-work.  What I would be looking at is if the bot grabs the raw format of a task page that there be some easily recognized header for the language and the bot can pick up the <nowiki>[[task/Language:langaugename]] or [[Language:languagename/taskname]]</nowiki> page in whatever form that takes.  
::: This reorg could cause Icon/Unicon some problems as many of the tasks share code.
:: All the RC count example tasks will need to be redone.  Ouch.  
:: --[[User:Dgamey|Dgamey]] 10:44, 27 May 2011 (UTC)


###  Concrete Example 

I've spoken with the Unicon team and there is some support for including RC material in the Unicon distribution in a dedicated Rosetta directory similar to the IPL and UniLib contributions.  The tools I'm imagining could help pull this material out of RC, identify changes, and produce supplementary documentation (like the URL, date posted, referenced libraries, possibly author(s), etc.

I also think that other LOYC might wish to do something similar.  Maybe.

It could also make a great task too.  

--[[User:Dgamey|Dgamey]] 17:30, 2 May 2011 (UTC)


### = Licensing Issues =

: Be aware that content on RC is GFDL licensed. Some contributors dual-license, but the deltas applied by subsequent editors may or may not be. --[[User:Short Circuit|Michael Mol]] 13:28, 3 May 2011 (UTC)
:: Icon/Unicon code tends to be in the public domain and major contributors are aware of that. It's all publicly declared.
::: Unicon Book (GFDL)
::: IPL code contains "this file is in public domain"
::: Unicon is public, but under what licensce I'm unsure (There was NSF funding at one point).
::: UniLib is also stated in the public domain
:: The dual license sounds awkward and unworkable.  If A adds code with some other license, how can B come along and undo that?
:: Do we need to 'declare' somewhere (I was intending to) that this is going to happen.
:: --[[User:Dgamey|Dgamey]] 21:00, 3 May 2011 (UTC)
::: I do think the dual-license side is extremely difficult to navigate, if not impossible. However, the nature of the dual-license is use-as-this-or-that, and since GFDL is one of those, then treating it as GFDL should be safe. IANAL, but that's what I believe. No content on Rosetta Code has an "invariant" section, as described in the GFDL license. --[[User:Short Circuit|Michael Mol]] 13:33, 4 May 2011 (UTC)

:: I'm not sure how much the Dual Licensing will hold up.  Every page clearly states the contents are GDFL and not to contribute non-public domain works.  If someone does, and it were found the contributor would have violated the license and the RC terms of use and presumably we would just remove the offending bits.  --[[User:Dgamey|Dgamey]] 13:13, 27 May 2011 (UTC)
::: I agree, and I don't think it's really a worthwhile thing to consider unless someone did not read the directions. The key point I wanted to bring up is that RC content is GFDL, and that includes some frustrating restrictions. --[[User:Short Circuit|Michael Mol]] 17:08, 27 May 2011 (UTC)


### = Did it Already =


I've been [http://nongnu.org/txr/rosetta-solutions.html doing this] for the TXR language for many years now. If you read the second paragraph in the navigation pane, you will notice that it links to the pair of scripts for generating the page.

One script scrapes the examples over HTTP; it works by navigating edit links and getting the actual markup source code inside the examples. The generation script parses the markup, and implements a lot of the formatting. It extracts the TXR code and applies syntax coloring to it with the help of the Vim editor.  The [http://nongnu.org/txr/highlight.exp <code>highlight.exp</code>] expect script for invoking Vim noninteractively isn't linked to from the page, but here it is.

From time to time (once in a blue moon) I run the fetching script. Then do a <code>diff</code> between the newly downloaded file and the previously downloaded one. If there are any changes, I replace the stable copy and run the second script to regenerate the page.

There is another reason why I follow this procedure. There are some race conditions. The edit links which are chased from the task page to the edit of a particular solution are numerically indexed. If someone inserts a solution, the numbering changes. I've also seen strange bugs where the script repeatedly fetches the wrong task for the wrong language, even when re-run. The URL it is using is fetching stale data somehow; but the issue doesn't reproduce with a browser. If we fetch exactly the same URL in a browser that the script is using, the browser gets the correct data. This happens rarely; it's some sort of strange caching or something. In any case, with the "diff against sane previous copy" approach, I catch these things.

[[User:Kazinator|Kazinator]] ([[User talk:Kazinator|talk]]) 00:13, 30 October 2018 (UTC)


### = Python Library =


I created [https://github.com/gerph/rosettacode a library] for my own use yesterday, which can extract the Category, Task, Language and CodeBlock (my names for those things - it's likely that they have other names on RC), from the site. I'm only using it so that I could extract C snippets to test against a compiler, but I thought it may be useful to others, so I've tidied it up and added a simple CLI tool to the front. It doesn't work well for some things, but it might be useful for this case or anyone else wanting to extract the code in a structured way.

[[User:Gerph|Gerph]] ([[User talk:Gerph|talk]]) 21:21, 10 May 2019 (UTC)
