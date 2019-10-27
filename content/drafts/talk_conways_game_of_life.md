+++
title = "Talk:Conway's Game of Life"
description = ""
date = 2017-09-28T06:49:11Z
aliases = []
[extra]
id = 3078
[taxonomies]
categories = []
tags = []
+++

==Fold-away Sample output sections==
It would be great if each example could come with a Sample output section that initially displayed in a hidden state, or that scrolled within a fixed height. This would allow much longer output samples so we could show multiple 'gliders' on larger grids for example. --[[User:Paddy3118|Paddy3118]] 16:48, 11 October 2008 (UTC)
:How to do this, I mean technically? --[[User:Dmitry-kazakov|Dmitry-kazakov]] 09:38, 12 October 2008 (UTC)
A quick google and I found [http://bonrouge.com/togglescripts.php?script=byidtagandclass this], but I know I have seen better - I just cannot remember where. --[[User:Paddy3118|Paddy3118]] 17:20, 12 October 2008 (UTC)

Or [http://www.dustindiaz.com/basement/block-toggle.html this]. --[[User:Paddy3118|Paddy3118]] 17:26, 12 October 2008 (UTC)

== No headers for sample output ==

I don't think it is good idea to have headers for sample output. This makes the Contents messy. --[[User:PauliKL|PauliKL]] 10:05, 10 November 2008 (UTC)

== SETL ==
There's a problem with the SETL sample. It was made to be highlighted with the wiki syntax, then it was put in a <nowiki><lang></nowiki> tag without removing the wiki code.

== Java Swing ==
My Swing implementation is kind of long (because it's a swing implementation). If anyone finds it inappropriately long, we can discuss removing it.--[[User:Burke9077|Burke9077]] ([[User talk:Burke9077|talk]]) 17:34, 17 January 2014 (UTC)

: I've not got a problem with having it, but the code could be better. :-) For example, it's more idiomatic to not make <code>GameBoard</code> implement interfaces like <code>ComponentListener</code> and to instead pass in a inner subclass of <code>ComponentAdapter</code>. Similarly, you've got a big <code>if</code> chain inside <code>actionPerformed</code> (in <code>ConwaysGameOfLife</code>) whereas it is better to use some inner classes that are specific to each event source.  You should separate the model from the GUI too (turning everything between a list of points and an array of values every turn has got to be less than perfectly efficient). All criticism offered in the spirit of trying to help you make your code be more idiomatic (my watchword for what makes a good RC example).
: There are some MediaWiki tricks for having a long example on the page without making the page (well, the page source) too long. I'll apply them if it becomes an issue (it's not, yet). –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 09:40, 17 January 2014 (UTC)

: You could move it to a sub-page referenced from the task page like what is done here: [[Formal_power_series#Java|Formal_power_series#Java]]. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 12:00, 17 January 2014 (UTC)

:: Thanks for the advice Donal''d'', I will move the actions that I can to annon innerclasses. I think I will take your advice Paddy3118, I removed a lot of commenting when I posted it here, so I could restore that in a separate page.--[[User:Burke9077|Burke9077]] ([[User talk:Burke9077|talk]]) 17:34, 17 January 2014 (UTC)

::: You're welcome. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:50, 17 January 2014 (UTC)
