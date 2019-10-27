+++
title = "Talk:Convert seconds to compound duration"
description = ""
date = 2015-08-21T06:43:18Z
aliases = []
[extra]
id = 19232
[taxonomies]
categories = []
tags = []
+++

== Task is ready for accepting solutions! ==

I've created it as a draft task for now as a formality, but I consider the task description finished - unless of course there is feedback demanding changes. So code away, and say if anything is unclear! --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 21:56, 6 June 2015 (UTC)‎

:Hi, the usual process is to wait for at least four different language implementations from four people and no unanswered questions on the talk page before promotion from draft to full task status. This helps the task to mature without too many people doing implementations if problems are found.
:I enjoyed doing the Python example and found the task description gave me the info I needed. Thanks. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 03:41, 7 June 2015 (UTC)

== About the python examples ==

The first python example doesn't run, because of undefined <code>t</code>.  The second one runs, but if fed 0 seconds as input, it gives a blank in return, which is probably not ideal.  Other examples may be doing the same thing, but I haven't checked.

Another issue: is it really necessary to post code in the interactive prompt format? It makes testing the code unpleasant because of the ">>>" prompt, and the mixed tab/space indentations looks like crap in my text editor. It may be useful to show users how to use the interactive shell, but since majority of coding is not going to be done in this fashion, having half of the python examples on RC so presented is overdoing it. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 04:40, 7 June 2015 (UTC)

: The current specification clearly states that the result for 0 seconds should be blank (or the empty string). I'm fine with that - makes for a clean implementation. Specifically:

::<li>However, '''only''' include quantities with non-zero values in the output (e.g. ''...'').

: Meanwhile, as a curiosity question - would the python language spec lose anything if it were changed so that prompts (at least the default prompt) were neutral? Copy and paste of lines containing previously run statements is indeed a handy tool... --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 11:42, 7 June 2015 (UTC)

: I hadn't thought about the 0 specifically, but I did qualify the problem statement by writing that the function "takes a positive integer" (which I wrote to exclude negative numbers, but actually excludes the 0 as well). In other words: An input of 0 is outside the scope of the task. You can handle it however you want. In practice, the cleanest way would be to reject non-positive numbers with a descriptive error message, but such boiler-plate input validation is not what the task is meant to be about, so it is not mandatory. Simply assuming that the function will only be called with positive numbers, is fine for this task. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 12:30, 7 June 2015 (UTC)

:I post some small examples of code as entered in the command line prompt because it is a distinguishing feature of the language. Other languages may have a REPL but Pythons' REPL is used, in fact, the [https://docs.python.org/3/tutorial/index.html Python tutorial] uses the REPL.
:I understand that it may cause problems in cut-n-paste trying out of examples, but the REPL adds to Pythons approachability I think and stands out as a distinguishing feature when compared to other examples. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:19, 7 June 2015 (UTC)

:I have fixed the first Python example. Thanks for pointing out my error. 
:Another point on keeping REPL examples is that it looks a lot more like IPython and IPython notebook input which is growing in popularity it seems. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:29, 7 June 2015 (UTC)

== possible optional or extra credit ==
Some options that would be a nice addition:
::*   ability to handle an input value of   '''0'''   (zero) seconds 
::*   ability to handle fractional seconds, such as:    '''123.7'''
::*   ability to handle higher time units, such as:   years, centuries

-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 10:36, 20 June 2015 (UTC)

: Hmm... 
:::* the current spec requires that 0 seconds be displayed as an empty string. 
:::* fractional seconds would not conflict at all with the current spec (though might require some implementations change)
:::* years (and centuries) might be something of a problem because the relation between weeks and years is not simple. But it would be doable if the task defined a year as (for example) 52.1786 weeks. The numbers would be a bit nonsensical, but the other option would be be that you have to specify the starting second. And if you are going to do that you might as well do months as well (but this of course changes the task into something very different - see also [[Talk:Holidays_related_to_Easter]] and [[Talk:Last_Friday_of_each_month]] for some of the issues which might be relevant for that kind of a task. 
:::* Negative seconds might also change the task, if you wanted to go there. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:35, 20 June 2015 (UTC)

::I'm not really a fan of "extra credit" specs, because they make it more difficult to compare solutions. Though if the admins want them, I suppose "ability to handle fractional seconds" would a suitable candidate. Regarding the other two though:
::* '''0 seconds case:''' I don't see returning an empty string for an input of 0 as problematic, even if the function were meant for practical use. It's logically consistent, and it allows the calling code to use the function like 
```perl6
say compound-duration($seconds) || "now";
```
 in languages where the "or" operator supports passing values through. Because only the calling code knows what is best printed in the 0 case - e.g. if the duration is meant to represent a countdown, it may want to print "now"; in other cases "none"; in other cases "0 sec". OTOH doing it this way may not make sense at all in some languages or contexts, which is IMO an indicator that the task should not mandate it either way.
::* '''higher time units:''' What would be the point? AFAICT, it would just be more of the same and not add anything new/interesting to the solutions - just make them longer and thus less convenient to read/compare. Of course if you mean using actual ''calendar'' months and years (i.e. handling days in month / leap years / leap seconds), that's out of the scope of what this task is meant to show (decomposing a number into a sum of unit quantities). There are other tasks for calendar math.
::--[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 11:35, 30 June 2015 (UTC)

-----

As for that   '''0'''   seconds be displayed as an empty string --- well, the task's requirements are for using a   ''positive''   integer (for the input), and zero isn't a positive integer.   '''But''', if zero '''is''' specified, displaying no output is deceiving.   I chose to display a value in that non-conforming case.   Another solution would be to display an error message (indicating that a non-positive integer was specified), but that seems a cheap way out in handling the case of zero seconds.   I see nothing wrong with a zero-value (input) time unit being handling for this Rosetta Code task.   But, specs are specs. 

I've been thinking about adding my full-blown, handy-dandy, fancy-dancy, general-purpose, all-inclusive, all-encompassing time (units) converter (to be saved/stored in it's own Rosetta Code page, it has over 600 lines of statements, not counting the documentation, which has almost 500 lines --- if I'm known for anything, it's robust documentation)   that does exactly what this Rosetta Code task addresses, including the somewhat unconventional abbreviations   (although it pluralizes the time units specified if appropriate).   My REXX program lets the user choose what time units (note the plural) to be used (for input as well as output).   Both the input time units (default time unit is in seconds if no time units is/are specified), and the output time units can also be specified, the default output times units are: 
::* eon, millennium, century, year, month, day, hour, minute, second
(all are pluralized if necessary). 

Also, the input time units can be additive or subtractive, as: 
::*   2day-2.5001hrs+1week+2days+0fortnights
and almost all forms of abbreviations (and alternative spellings) are accepted:

::*   PLanck PLancks PLank PLanks PLANCKTIMe ...
::*   Year Years Yr Yrs 

where the capital letters indicate the minimum abbreviation(s).

The user can also specify which time units to be used for the output, so that can (may?) solve the problem/issue with the   ''not''   using of months or weeks, --- as a month isn't exactly defined and most people are used to (roughly) 30 days per month, and (roughly) 52 weeks a year.   If desired, weeks and months can be "skipped" and just use years, days, hours, ...   or whatever time units are desired. 

Whatever the last time unit is specified for the output, that time unit would/could contain a fractional value (say, if minutes and/or seconds were NOT specified for (in) the output time units.

Among other things, the REXX program supports things like the time for any planet's orbit time (or ''year'')   and converting it into whatever time unit(s) are specified (or use the defaults).   There are over 170 different time units supported ('''not''' counting the using of 42 metric and/or binary multipliers such as:   mega, kilo, giga, kibi, micro, nano, femto,   yadda, yadda, yadda).

Yuppers.    174 time units.    Who'd thunk it?   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:58, 20 June 2015 (UTC)


:That's awesome! --[[User:Simple9371|Simple9371]] ([[User talk:Simple9371|talk]]) 11:31, 1 July 2015 (UTC)
