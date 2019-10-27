+++
title = "Rosetta Code:Village Pump/Automatic omit"
description = ""
date = 2010-11-28T17:20:10Z
aliases = []
[extra]
id = 7893
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Automatic omit
|summary=Defining required features (eg GUI, network access) as a property of tasks and languages
}}
Often several different tasks are about the same general feature (e.g. concurrency, GUI, network access, file system operations, etc.). Now if a language doesn't support any of those, then it will of course support none of the specific tasks for that feature (e.g. a language that doesn't support concurrency will not support any task that involves threads, mutexes or semaphores). Currently you need to have an explicit <nowiki>{{omit from|Language}}</nowiki> for each of the tasks. I think it would be a good idea if there were a defined set of general features, where the task could state whether the features are needed, and the language page may state which features are not supported, and then to automatically omit all languages from a task if they don't support the features (except if there's an explicit example for the language in that task; maybe there's a workaround for the specific tasks in the language).

I don't know how hard this would be, but it sounds like a good match for semantic Wiki. The features could be implemented through categories (actually, some features like supported programming paradigms already are), or maybe through specific structures of semantic Wiki markup. A task would then use either a special template (e.g. <nowiki>{{requires feature|Concurrency}}</nowiki>, or the task template could be extended to allow specifying required features. Similarly, the language would either use specific templates like <nowiki>{{supports featuire|Concurrency}}</nowiki>, or allow specifying them through the language template.

Here's a list of language features where this feature might be useful:
* Concurrency
* Network access
* Graphics
* GUI (implies Graphics)
* OpenGL (implies Graphics)
* File access (i.e. file open, read, write)
* File system operations (implies File access)
--[[User:Ce|Ce]] 08:00, 6 August 2010 (UTC)

: It is indeed a good match for Semantic Mediawiki, but I don't know how to apply it quite yet, as SMW only has limited support for negation (as found in the Unimpl pages). That's going to need to get hacked in there, obviously. I would be wary of applying most semantic properties from within templates, though, because there are often workarounds to apparent dependencies. GUI doesn't need Graphics if you use, e.g. ncurses. OpenGL doesn't need graphics if you're mutating an instruction stream. Generally, I think semantic properties should be expressed within the task description. See, for example, [http://rosettacode.org/mw/index.php?title=User%3AMwn3d&diff=87343&oldid=87340 a change] I made on Mwn3d's user page. (That might be helpful for ensuring better task descriptions, too.)
:
: Really, though, there are dozens of ways we could try using SMW, and I can't accurate predict which will work well and which won't. The best I can suggest is to start applying properties to things, and find out what opportunities turn up. --[[User:Short Circuit|Michael Mol]] 13:37, 6 August 2010 (UTC)

:: OK, I'm now applying properties named "provides::" (the language or standard library directly provides it) and "allows::" (the language doesn't directly provide it, but there are libraries or other methods to do it) to languages, and "requires::" to tasks. The values of these properties all start with <tt>Capability:</tt>. --[[User:Ce|Ce]] 20:15, 12 August 2010 (UTC)
:: Capabilities used so far:
::* Capability:Network access
::* Capability:Database access
::* Capability:Concurrency
::* Capability:Windowing UI (includes both GUIs and text based windowing UIs)
::* Capability:Graphics
::* Capability:OpenGL
::* Capability:Dynamic linking
::* Capability:File system access
::* Capability:Run time polymorphism
::* Capability:Compile time polymorphism
::* Capability:File access
::* Capability:Objects
::* Capability:First class functions
::* Capability:Signal handling
:: I'm currently not creating pages for them, so that one can easily change and/or remove the definitions  if the current ones are considered a bad idea.
:: BTW, I noticed that all properties are in a namespace "Example" - this doesn't make sense to me. I would have expected a namespace "Property" or similar. What's the reason for this name? --[[User:Ce|Ce]] 20:44, 12 August 2010 (UTC)
::: I typo'd something in the SMW configuration file, and didn't notice until much later. It'll get fixed, but it'll be a couple weeks; it's going to be a bit of an involved process. Take a backup, restore on local system, identify steps needed to get namespace IDs corrected, clear local system, verify steps work, lock down public wiki, take a backup, apply procedure, verify nothing broke, unlock wiki.  It's probably going to take much of a Sunday, and I'll probably tag [[User:Coderjoe]] to help me with it. --[[User:Short Circuit|Michael Mol]] 01:28, 13 August 2010 (UTC)
