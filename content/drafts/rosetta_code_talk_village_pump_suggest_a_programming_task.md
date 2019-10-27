+++
title = "Rosetta Code talk:Village Pump/Suggest a programming task"
description = ""
date = 2010-10-08T19:56:40Z
aliases = []
[extra]
id = 1756
[taxonomies]
categories = []
tags = []
+++

==Get moving on these==
Can we get moving on some of these? --[[User:Short Circuit|Short Circuit]] 23:00, 7 November 2007 (MST)
: To facilitate the process, I've created [[Template:Draft task]], taken from observing [[User:Kevin Reid|Kevin Reid]]'s [http://rosettacode.org/mw/index.php?title=Stem-and-leaf_plot&oldid=70027 initial stages] of the [[Stem-and-leaf plot]] task.  Any page this template is on will show up in [[:Category:Draft Programming Tasks]]. Ideally, pages that wind up in there should be fixed up and clarified so that they're acceptable enough to be added to the Task roster.  Just about anything that's in the "Request a programming task" page that's remotely workable should be put in a draft task, where the task ideas can be refined until people can agree they're clear enough for comfortable unambiguous implementation. --[[User:Short Circuit|Michael Mol]] 06:05, 22 December 2009 (UTC)
:: ... Or removal? Hopefully we can turn a large proportion of drafts into true tasks, but others might be split, or removed as unworkable I would think. --[[User:Paddy3118|Paddy3118]] 08:29, 22 December 2009 (UTC)
::: ... Or removal.  Or conversion or splitting into something useful. If something's totally unsuable, I'd probably want to shuffle it into a "dead task ideas" category, rather than erasing it. --[[User:Short Circuit|Michael Mol]] 16:26, 22 December 2009 (UTC)
:::: +1 on the dead tasks so that the reason for their rejection is kept. (Maybe rejected task category)? --[[User:Paddy3118|Paddy3118]] 18:30, 22 December 2009 (UTC)
::::: Call it "dormant".  You never know if someone might figure out how to revive it usefully. --[[User:Short Circuit|Michael Mol]] 20:56, 22 December 2009 (UTC)
:::::: +1 --[[User:Paddy3118|Paddy3118]] 05:31, 23 December 2009 (UTC)

== Task guidelines? ==

Moved to [[Village Pump:Home/Task creation process discussion]]

The project page asks that a task be put on the "unsorted" section, which doesn't seem to exist. What should I do? --[[User:dcsobral|Daniel Sobral]] 19:08, 03 February 2009 (UTC)
: [[Rosetta Code:Village Pump/Request a programming task#Unsorted|Added back]]; Someone (quite possibly me) must have removed it the last time it was emptied. --[[User:Short Circuit|Michael Mol]] 05:46, 4 February 2010 (UTC)

== Assertions ==

Are there enough languages that use assertions to warrant a task for them? I know Java, C, and C++ have them, but I'm not sure about other languages. Also, do assertions work similarly enough across all of the languages to make comparison worthwhile? --[[User:Mwn3d|Mwn3d]] 21:22, 3 February 2009 (UTC)
: Is there a literary definition of the concept?  As far as I know, it's a runtime check of a boolean condition, combined with a debugging aid, such as a breakpoint or logging command.  That concept seems fairly universal, at least for procedural languages.  I'd be curious what the analog would be for languages like Lisp or Haskell. --[[User:Short Circuit|Short Circuit]] 23:39, 3 February 2009 (UTC)

:: I know Modula-3 and Ada both have assertion pragmas.  I believe OCaml/F# have assertion commands.  Oberon-2 has an ASSERT builtin command.  I'd say it's "universal" enough that there should at least be libraries for basic assertions in all languages. --01:12, 4 February 2009 (UTC)

[[Assertions|Done]]!

== Gamma function and similar ==

What should it happen when one asks for a "function"/algorithm which is already implemented in a language, in its "standard libs/functions...", or which is implemented in a external extra lib? Should one just show its usage, or show how it would be implemented if the language wouldn't have that function?

It seems it strongly depends on how the task was written, but doubts and shadows remain to me; examples:

* The Gamma function exists in Fortran2008; if the task asks just ''show how to compute the Gamma function in your language'', I surely would use the intrinsic function, or well known libraries (Python numpy/scipy e.g.). If the task asks for a specific algorithm...? I suppose I am ''forced'' to implement it forgetting about libraries and intrinsics.
* [[Basic bitmap storage]]... Perl uses the Imlib2; it achieves the aim of the task (providing a graphics framework for other related tasks?), but it does not show how in Perl you would create a basic bitmap storage...
* [[Bresenham's line algorithm]] ... Perl again... Imlib2 has its own draw_line routine... since here the task asks explicitly for the implementation of a specific algorithm for drawing lines, cannot we use ''intrinsics'' if any? This time, Perl both shows how to use the Imlib2 and implements the Bresenham algorithm... what if the task would have been ''show how to draw a line using the basic bitmap storage from This Other Task''?
* [[LZW compression]] ... I've not uploaded my C version because it is long since I had to implement a Dictionary and a String facility (so it was easy to translate the Java code...); then I've found the Judy lib (used in [[Creating an Associative Array]]), but I haven't adapted the code yet, nor I am sure Judy can fit the specific need directly (I should anyway write support functions). But in this case, since the algorithm requested by the task is LZW, not Dictionary (or associative arrays, for which there's another task), using heavily extern functions/libraries is ok... except if we use a ''liblzw'' that with a simple <tt>lzwcompress(in, out)</tt> resolve the task...?

'''Maybe a moral''': if the task is written in a rather ''general'' way (like: ''show how to draw a line'', ''show how to compute the Gamma function in your language'' and so on), then it seems you can use everything you can, even implement a well-known/used algorithm. If the task asks explicitly for the implementation of an algorithm (Bresenham, LZW, Lanczos approximation...), then we must implement it, disregarding just "details" (e.g.: Bresenham needs to plot single points, we can use any function we can/want to plot them; LZW needs associative arrays, and we can use any already made function/method/object ... and so on)

(Middle point: a task could be permissive: ''show how to draw a line, e.g. using the Bresenham algorithm or the Xiaolin Wu algorithm'', but it would be better in this case if the task is separated, since the former just draws a ''sharp line'', the latter draws an ''antialiased line'')

Is this The Interpretation we should follow? --[[User:ShinTakezou|ShinTakezou]] 14:14, 24 February 2009 (UTC)

:maybe editors should be aware of the issue, but can't talk pages remain the place to discus each individual issue and come to agreement? --[[User:Paddy3118|Paddy3118]] 08:36, 22 December 2009 (UTC)

== Memory allocation? ==

I'm not sure how a lot of languages work with memory allocation, but I know that a lot C++ students at my college often forget the difference between <tt>delete</tt> and <tt>delete[]</tt>. I'm not really sure how to put something like that in a task though. Something to show memory allocation and deallocation, and memory allocation on the [[system stack]] vs. the [[heap]]. It may be appropriate to talk about ways of customizing [[garbage collection]] in this task. It may take a couple of tasks, though. Anyone have a good idea of how to set it up? --[[User:Mwn3d|Mwn3d]] 22:00, 10 May 2009 (UTC)
: A large number of programming languages recognize the difference between heap memory and stack memory; For many of them, [[Memory Allocation]] is as fundamental as control structures.  Simply creating a task dedicated to the allocation of data and objects on both types should be sufficient; The information will find its way there. --[[User:Short Circuit|Short Circuit]] 15:55, 11 May 2009 (UTC)

:: How about ''"Create, use, then destroy a number of objects in series where the total amount of memory used by the objects if not re-used, would be comfortably more than the amount of memory available to the program"''. It does not distinguish between stack and heap but allows garbaage collected dynamic languages to give solutions too. --[[User:Paddy3118|Paddy3118]] 08:44, 22 December 2009 (UTC)

::: Whoops, there is already [Memory Allocation]. Do we need mine? --[[User:Paddy3118|Paddy3118]] 08:46, 22 December 2009 (UTC)

== Move the "Suggest a task" process? ==

I'd like to propose moving the "Suggest a Programming Task" logic over to something like a subreddit. The [http://www.reddit.com/r/rosettacodeorg/ Rosetta Code subreddit] might be appropriate. An [http://www.ideatorrent.org/ IdeaTorrent] might be good, too, but I don't want to maintain another software package on the server if I don't need to, and it looks like that project may be stagnating.

Several reasons come to mind:
# Easier debate and discussion
# More effective triaging; upvoted tasks are more likely to be implemented by people simply looking for something to do.
# RSS feed. (Always convenient)
--[[User:Short Circuit|Michael Mol]] 15:16, 2 August 2010 (UTC)

: What is a subreddit? Would I need a separate account for that? And how would it ease the discussion? Would it still support Wikilinks?
: PS: Why do I suddenly have to enter a captcha for each edit? --[[User:Ce|Ce]] 18:15, 2 August 2010 (UTC)
:# Click on "Reddit" in the navigation bar. Or see [http://programming.reddit.com proggit] for a more active example.
:# Yes
:# Better threading, mostly. Relates to the upvote/downvote behavior.
:# I had to make the CAPTCHA behavior more aggressive in response to the increased amounts of spam we'd been seeing. Logged-in users shouldn't be seeing CAPTCHAs except in certain circmstances, though; looks like I'll have to tweak some server settings.--[[User:Short Circuit|Michael Mol]] 18:25, 2 August 2010 (UTC)
:: FWIW, Mike, I'm also seeing the CAPTCHA for every edit when logged in. --[[User:Snoman|Snoman]] 18:53, 2 August 2010 (UTC)
::: Me too! --[[User:Paddy3118|Paddy3118]] 18:58, 2 August 2010 (UTC)
:::: That should be fixed; "Autoconfirmed" users (users older than a certain amount of time) should be able to skip captchas, now.--[[User:Short Circuit|Michael Mol]] 00:33, 3 August 2010 (UTC)
::::: Disabled 'skipcaptcha' for sysop group; I should see captchas, same as normal users, now.--[[User:Short Circuit|Michael Mol]] 00:39, 3 August 2010 (UTC)
:::::: And now the captcha doesn't appear for me. [[User:Stormneedle|Stormneedle]] 04:07, 3 August 2010 (UTC)
::::::: Right, because you're "autoconfirmed" ... meaning your account is beyond a certain number of days old. --[[User:Short Circuit|Michael Mol]] 12:59, 3 August 2010 (UTC)
== Re-thinking what we're doing in this page ==
Rather than trying to find a full task to be suggested, we should be looking at them as properties. So if someone wants to see Monads demonstrated, we can link to tasks that have the monad property (or examples which do). If someone wants to see "an encryption program", we can link to tasks and examples which have the 'cryptography' property. That should help us move through the task page more quickly, and give us some creative leeway in building tasks that solve some of the underlying goals of the person making the suggestion.--[[User:Short Circuit|Michael Mol]] 19:56, 8 October 2010 (UTC)
