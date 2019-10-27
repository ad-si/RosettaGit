+++
title = "Template talk:Task heading"
description = ""
date = 2016-08-23T17:50:48Z
aliases = []
[extra]
id = 21076
[taxonomies]
categories = []
tags = []
+++

==Whether to use this template==

Quoting from the discussion at [[Talk:Search_a_list_of_records#Overhauled task]]:

{{quote
| Also, I much prefer   (for this task and other tasks being changed)   the old form of the task's preamble where all the section headers are in '''bold'''   (which makes it much easier to find the relevant sections).
| [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:21, 22 August 2016 (UTC)
}}

: Hm, one of the reason why I made this template was that I found that the bold headers (without increased font size and spacing), ''didn't'' stand out well enough as headings, especially in task descriptions that already use bold font to highlight passages inside normal text. Are you sure it's not just because it's still unfamiliar, and you're still used to looking for <code>;bold:</code> headings when skimming tasks?

:: Yes, I'm sure.   A slightly larger font doesn't help me that much when I'm perusing the preamble.   But bold headers do help.   Having a bold section header makes it "stand out" and much easier to find.   I never had trouble looking for a bold   '''Task'''   (in the left side), any boldface text elsewhere in the text doesn't get skimming by my eyeballs -- I concentrated on the left margins for bold text when looking for important ... er, "headings".   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:40, 23 August 2016 (UTC)

::: Understood. I've updated the template to make them bold now. I'll try to mess around with the TOC heading style later. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 10:21, 23 August 2016 (UTC)

: I suppose we could make them bold ''in addition'' to being larger and have more spacing than normal text, but I quite like them having the same style as the "Contents" heading of the table of contents. I think I'll put in a request to have the site CSS adapted to make the TOC heading bold, and if it is accepted, adapt this template accordingly.

:: Yes, making them bold in addition would solve the problem.   I don't see how making it look the same as the table-of-contents helps anything as far as peruseability.   The TOC (contents box) used to be shaded a very light blue (before the changes that were implemented in December of 2015), which made it stand out as a "light-blue" box (a list of computer programming entries) and it was very very easy to find the (presumptive) end of the task's preamble --- you'd have to be half blind to miss it.   Now that the TOC box is the same color background as everything else (poo-bah!), it's harder to find when perusing, so that is why I've been including a   <big> <nowiki> 

 </nowiki> </big>   after the "end" of most Rosetta Code preambles --- to make the TOC box easier to find and click on a particular computer programming entry.   Sometimes, the "improvements" ··· aren't.   I yearn for the old (better) light-blue shaded table-of-contents box.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:40, 23 August 2016 (UTC)

::: Do you know if those particular style changes were made on purpose, or if the site was just upgraded to a newer MediaWiki version and/or theme and those things happened to get worse as a by-product? Because in the latter case, we may be able to convince the admins to add some custom CSS rules to re-add a TOC background color and make the edit links float right. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 10:21, 23 August 2016 (UTC)

:::: Michael Mol (as I understand it) upgraded Rosetta Code to a newer/updated MediaWiki   (and most likely, other changes were made as well, but I don't know this for a fact), but a number of changes were implemented (or happened) all at once around December, 2015.   The updating was, I assume, purposeful, but possibly some side affects were unknown or unforeseen.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:50, 23 August 2016 (UTC)

: In any case, using a dedicated template for these headings makes it easy to change things about them (without having to update all task pages each time). So that's another advantage. The <code>;bold:</code> syntax was never meant for section headings, nor is it specific to them - it is actually part of the wiki syntax for definition lists.
: --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 04:49, 23 August 2016 (UTC)

:: Being that is what it may, bold    '''Task'''   "headings" is what the majority of Rosetta Code tasks (and draft tasks) have now.   I used to use   <big> '''Task''' </big>   for all the Rosetta Code (draft) tasks that I entered, but somebody (an administrator, I believe) changed all those to   ''';Task:'''   --- so that's what I used since then   (if anything, to make it consistent to all the other Rosetta Code tasks.   ''I didn't want to start an edit war with people who buy ink by the barrel.'' -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:40, 23 August 2016 (UTC) 

::: Heh, it should be easy enough to write a bot to change them en masse... ;) --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 10:21, 23 August 2016 (UTC)


{{quote
| Also, I think all those   <small> [edit] </small>   thingys just obscure the readability of the task's preamble.
| [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:21, 22 August 2016 (UTC)
}}

: But it is like that for ''all'' section headings on the wiki. Do you think that the edit links also obscure the readability of the stuff ''below'' the table of contents? If so, maybe put in a request at [[MediaWiki_talk:Common.css]] to have them styled less obtrusively. For example, they could be made to float right - that way they'd stick to the right edge of the page, rather than immediately following the section title.

:: I understand that it does that for all (the new) section headings, but with the "old" bold headings (I now know that those weren't really section headers, although I've been calling them [wrongly] that for years), it didn't create a section heading, and the "old" bold headings didn't have that   <small> [edit] </small>   thingy.   Before the new stuff, there were no section headings at all (that is, no <small> [edit] </small>   tags).   I understand that all the (old) section headings   ''below''   the TOC are, in fact, computer programming entries, and   ''those''   section headings need/require separate   <small> [edit] </small>   thingys.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:40, 23 August 2016 (UTC) 

:: Yes, I liked the old way (before the 2015 December changes) that the   <small> [edit] </small>   thingys used to float to the right, that made the "button" less obtrusive (eyeball wise) and made it less likely to be hit (pressed) by accident.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:40, 23 August 2016 (UTC)

: You're right, however, that we don't need multiple edit links in the task description as long as they all give us the whole task description to edit (due to a technical limitation for which I haven't found a work-around). I've now updated the template to only show the edit link for the primary "Task" heading, not for other ones like "Test cases" or "See also".
: --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 04:49, 23 August 2016 (UTC)

:: Can the   '''See also'''   be reverted back to   '''Related tasks'''     (where appropriate)?   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:40, 23 August 2016 (UTC)

::: Oh? I think both are fine when there's just links to other Rosetta Code tasks. But when there are also links to useful external resources (like Wikipedia), it makes sense to use "See also". I thought it's safer to always name it that way, so that if someone wants to add an external resource link to a task that previously didn't have one, they known where to put it - but I don't feel strongly about it. Feel free to change <code><nowiki>{{task heading|See also}}</nowiki></code> to <code><nowiki>{{task heading|Related tasks}}</nowiki></code> where you think it makes sense. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 10:28, 23 August 2016 (UTC)

:::: I agree with you in that when using the   ''';See also'''   thingy, it refers to links to "other" references.   However, I like   ''';Related tasks'''   to refer to links to Rosetta Code tasks that are related;   it clearly indicates what those other links are ··· Rosetta Code tasks that are similar or related in some way --- and not reference links to help with the Rosetta Code task at hand.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:50, 23 August 2016 (UTC) 

{{quote
| If somebody wants to edit the task's requirement(s), description, related tasks, examples, etc.,   they can use the   <big> ''Edit'' </big>   tab entry   (at the top, just like always).
| [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:21, 22 August 2016 (UTC)
}}

: No, that's not nearly as convenient. When you edit the whole page, with all solutions, then each time you click "Preview" it tends to involve a ''lot'' of scrolling, both in the previewed page and in the edit box, to get back to where you were and continue editing. And you also won't automatically get <code>/* Task */</code> pre-filled in the Edit Summary field, which means that people who edit the task description will often forget to mention that in the summary, and people watching [[Special:RecentChanges]] will miss such changes.

:: If I wanted to change stuff in three sections, I'd just edit the (whole) task's preamble;   a single edit is still easier than three separate edits, especially if the changes tend to be connected (globally).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:40, 23 August 2016 (UTC)
 
:: I never had a problem "scrolling", as I use the   '''End'''   (keyboard) key   (which puts me in the edit box directly).   Using the   '''Home'''   (keyboard) key   puts me at the top of the task's preamble.   This "bypasses" all that scrolling.   Most Rosetta Code task preambles "fit" on my screen, so scrolling isn't needed.     But that's my style of editing and I certainly don't want to force anybody from changing   ''their''   style.   Just knowing about the shortcuts helps.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:40, 23 August 2016 (UTC)

: Those are also the reason why the wiki software adds the edit links to all normal headings, and I don't why these headings should be different (except for the technical limitation mentioned above).
: --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 04:49, 23 August 2016 (UTC)

:: Well, changing them to "normal headings" would mandate to have Wiki treat them like all the other headings in Wiki-land.   But I don't see the need to have the   <small> [edit] </small>   thingys (in "sections", as I know them) for Rosetta Code task preambles;   RC tasks are a different breed of animal than almost all of the other Wikipedia entries.   But, I don't want to be in the way of progress.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:40, 23 August 2016 (UTC)
