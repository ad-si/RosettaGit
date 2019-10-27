+++
title = "Rosetta Code:Village Pump/Web Programming tasks?"
description = ""
date = 2010-11-28T17:34:26Z
aliases = []
[extra]
id = 5333
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Web programming tasks
|summary=Web programming task category
}}
Should we have a new category of tasks about Web Programming? I'm thinking of stuff like "Generate a Hello World page", "Setting and Getting Cookies", "[http://www.paulgraham.com/arcchallenge.html The Arc Challenge]", "Basic User Authentication"...

Or would you consider tasks like these as too-narrow / too specialized? -- [[User:Wmeyer|Wmeyer]] 20:11, 13 January 2010 (UTC)
: For most of the tasks, the output medium is independent of what the task accomplishes. For example, one of the [[User Output]] should cover the web case of "Hello World" (with the minor exception that the string name is "Goodbye, World!").  Particular types of authentication, such as [[HTTP Authentication]] would be a worthwhile tasks, but that's from the client perspective.

: I ''think'' a general requirement could be that the goal of the task needs to be specific enough that multiple language may be able to have compareable solutions, but also that [[wp:Occam's Razor|Occam's Razor]] needs to be considered; A task shouldn't be more complicated in description than required to illustrate the tool functions it exposes.

: That's not to say things from a web perspective aren't useful; Tasks that illustrate the particulars of dealing with specifics of the SSL and HTTP protocols in various languages would be worthwhile, and that would include (but not be limited to) header manipulation, differences between GET/PUT/POST requests and HTTP-level authentication. Crypto certificate creation, storage, selection and checking could also be worthwhile, particularly if there are language features or accepted libraries for doing so. Otherwise, their utility boils down to demonstrating how a language might handle the underlying math. --[[User:Short Circuit|Michael Mol]] 23:26, 13 January 2010 (UTC)
::It seems to me to be entirely appropriate for RC to have tasks which happen to compare, not ''general programming languages'' but ''web frameworks'' (broadly defined). In which case web-focused tasks are appropriate, and 'dealing with HTTP' is part of that, but so might also be e.g. 'generate a web form, and on submit return it with validation errors, if any', for example. Or tasks about handling mapping URLs to application objects/appropriate handlers. Whatever. (I don't actually know as much about what modern web frameworks do as I probably ought to.) --[[User:Kevin Reid|Kevin Reid]] 01:40, 14 January 2010 (UTC)
::: That's true; It follows as a generalization of APIs and libraries in other programming contexts, such as DirectX vs OpenGL vs Blender, or Gtk+ vs Qt vs Win32. --[[User:Short Circuit|Michael Mol]] 04:48, 14 January 2010 (UTC)
:I am not sure about what this could lead to and so would want to comment on each proposed task. I don't think it would be good to have tasks aimed solely at particular web framework solutions where set-up may be tedious, OS specific, and not appropriate for inclusion in RC, but any code given may need particular set-up to run and so be hard to replicate. --[[User:Paddy3118|Paddy3118]] 06:10, 14 January 2010 (UTC)

::Thanks for the feedback, guys! I conclude that web programming tasks are welcome in principal, but that it will depend on the specifics of the task.
::Regarding the problem of tedious or OS-specific setup: For many languages there are frameworks which are very simple to use and have a rudimentary web server integrated, like web.py, Sinatra (Ruby), Compojure (Clojure) etc. And then there is PHP of course. So it should be possible to keep it simple.
::I'm not really a web programmer, but I will think about a very basic task to suggest. How do I suggest a task, btw? Do I just create the page and we will see whether it survives? -- [[User:Wmeyer|Wmeyer]] 02:28, 15 January 2010 (UTC)
::: The mechanism for suggesting a task has changed recently.  These days, you just create it, and use [[Template:Draft task]] instead of [[Template:Task]].  Folks will see it, try it, help refine it, and it will hopefully eventually graduate into something people are comfortable with.  At some point, someone will replace the usage of the Draft task template with the Task template, at which point it will have graduated. --[[User:Short Circuit|Michael Mol]] 05:39, 15 January 2010 (UTC)
