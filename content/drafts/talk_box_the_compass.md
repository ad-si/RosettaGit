+++
title = "Talk:Box the compass"
description = ""
date = 2016-04-05T22:28:52Z
aliases = []
[extra]
id = 9400
[taxonomies]
categories = []
tags = []
+++

==General==
The naming sequence was more complex than I originally thought. I started by coding up a generic quarter of the points but North and South take precedence over East and West in some of the orderings and so I had to 'reflect' a quarter, quarter1 to make quarter2 in the Python solution.

P.S. Couldn't help the Pirate speak :-)

--[[User:Paddy3118|Paddy3118]] 21:43, 27 March 2011 (UTC)
: I chose to just put in a list of all the packed representations (which are very slightly modified from the official abbreviations to add hyphens). The fact that I don't have to do a lot of quoting of the strings makes this a very attractive option in Tcl. –[[User:Dkf|Donal Fellows]] 10:10, 28 March 2011 (UTC)
::I like the way you've done it. Coding all 32 points is done so compactly and you skip all the logic for choosing quarters. Neat! --[[User:Paddy3118|Paddy3118]] 12:53, 28 March 2011 (UTC)
:In my VB.net implementation I chose to build the list of points into an array beforehand and keep it separate from the calculation subroutine.  It was implemented in a similar way to the Python routine otherwise.  Admittedly, it'd probably have been more efficient just to write the list of points straight into the array, but would have been far less interesting that way! --[[User:TheMadman|TheMadman]] 13:31, 28 March 2011 (UTC)
:For J, I decided to do the simple thing, and then I fix the result by preventing the text of the first word from appearing multiple times in multi-word names. --[[User:Rdm|Rdm]] 15:35, 28 March 2011 (UTC)

: The walk through the table (the actual degrees used) struck me as odd.  Basically a sawtooth walk going from lowest to middle to highest as each row is advanced.  When I saw the task I expected to see the midpoints as the most obvious.  Not sure what the reason was and it's not really important if your function translates an angle to a compass point. I suppose it test finding the edges. Was there a specific reason for that pattern of angles? --[[User:Dgamey|Dgamey]] 15:39, 23 April 2011 (UTC)

:: Hi Dgamey, yes it was to test the ranges corresponding to a particular point of the compass but still be easy enough to check. --[[User:Paddy3118|Paddy3118]] 18:51, 23 April 2011 (UTC)

== Capitalization? ==

The Wikipedia article capitalizes, for example, "Southwest by south".  However, a google search shows that as a general rule the capitalization will be either "southwest by south" or "Southwest by South".  The Wikipedia capitalization could be rationalized as the lower case variation as it would appear at the beginning of a sentence...  Anyways, perhaps the task description should either explicitly require one form of capitalization (and I do not much care which, though the Wikipedia form seems awkward) or should explicitly state that capitalization is a non-issue? --[[User:Rdm|Rdm]] 14:50, 28 March 2011 (UTC)

:The current J implementation abuses the information contained in the wikipedia capitalization to deal with one of the wording quirks.  (I would prefer to get rid of that abuse but do not want to code so generally that I am explicitly parameterizing and supporting all three forms of capitalization -- at least not if the task does not require it.)   --[[User:Rdm|Rdm]] 15:10, 28 March 2011 (UTC)

::Hi Rdm, What is important is matching the capitalisation of the wp table. But I just looked at the capitalization from [http://oxforddictionaries.com/view/entry/m_en_gb0794290#m_en_gb0794290 the OED], which seems to point to south being lower cased for cardinal points, (except when starting a sentence of course). --[[User:Paddy3118|Paddy3118]] 15:39, 28 March 2011 (UTC)


:I would argue that the capitalisation is a bit of a non-issue.  Strictly speaking, the cardinal directions are not proper nouns therefore shouldn't really be capitalised.  I did capitalise mine in the same way as the Wikipedia entry simply because it just wouldn't have looked right otherwise, but I did make sure that the capitalisation subroutine acted on the direction names at display time, not on insertion into the array. --[[User:TheMadman|TheMadman]] 15:32, 28 March 2011 (UTC)
::It should appear as in the wp table. Other capitalisations wouldn't be following the task description. --[[User:Paddy3118|Paddy3118]] 15:39, 28 March 2011 (UTC)
:::Absolutely, although if you look at the example at the bottom of the page, the Wikipedia entry does put "east by north" in lower case, suggesting that it's done in the table purely for aesthetics (which, in those circumstances, is probably the correct way to write it), hence my argument of non-issue.  --[[User:TheMadman|TheMadman]] 15:50, 28 March 2011 (UTC)
::I would also argue that capitalization should not be an issue. If you get concerned about something like that, you're starting to worry about the brand of mustard on the hamburger instead of the actual meat. --[[User:Mwn3d|Mwn3d]] 16:11, 28 March 2011 (UTC)

==J and task specifics==
What should I do about the J solution. No doubt, as the comment seems to suggest, J makes sure that the compass point for all three angles in its fuller row of three headings are all the same, but I am concerned that:
# Others will just look at J's solution and ''not'' check.
# It is not strictly comparable to other entries.
# The full wp table misses out -5.62 and 365.62
For these reasons, I am inclined to stick an incomplete tag on the J example and require it to produce what was asked for; or add to their current entry another version that does strictly what was asked for. Would this be OK Rdm? --[[User:Paddy3118|Paddy3118]] 15:54, 28 March 2011 (UTC)

:I think I understand your concerns, ... (Editted: and the task description now matches them). --[[User:Rdm|Rdm]] 

::Hi Rdm, the task has the note on computing the index and headings to convert that isn't in your current example but is in others. --[[User:Paddy3118|Paddy3118]] 19:48, 28 March 2011 (UTC)
:::I believe I have the compass index in the J implementation.  I believe I have included all the headings asked for in the task description.  I believe the headings I use are in the same order that they are specified in the task description.  So I am not sure what you meant here.  Is it possible you were looking at a cached version of the page when you wrote this?  --[[User:Rdm|Rdm]] 20:26, 28 March 2011 (UTC)

:Hi Rdm, points at index 7,15,23,31 are off slightly. (The wording of the point). --[[User:Paddy3118|Paddy3118]] 20:01, 28 March 2011 (UTC)
::Thank you, fixed.  (I now sort the hyphenated words so that the shortest name in the pair comes first.) --[[User:Rdm|Rdm]] 20:19, 28 March 2011 (UTC)

==ALGOL's Traditional wind points==
I tracked them down to being the names of Mediterannean winds. Nice to know. ([http://www.mediterranean-yachting.com/winds.htm here] is one of many references or track each name in wikipedia individually). --[[User:Paddy3118|Paddy3118]] 06:11, 16 June 2011 (UTC)

==Task clarification needed==
I think the interval between two neighboring directions is exactly 360/32=11.25 degrees. It is conforming to the '''Middle''' column of the table in the [[wp:Boxing the compass|wikipedia article]]. So I do not understand the meaning of the given sequence of numbers: <code>[0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, 84.38, 101.25, 118.12, 118.13, 135.0, 151.87, 151.88, 168.75, 185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270.0, 286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, 354.38]</code>.
Sorry for my english:)
[[User:Insolor|Insolor]] 12:09, 24 June 2011 (UTC)
:I think those numbers are to demonstrate that the ranges are properly positioned and sized. The middle of the ranges may be 11.25 degrees apart, but the different directions that a compass can show (in degrees) are not. It just so happens that we name 16.88 degrees "North-northeast" because it's closest to that named direction (centered at 22.5 degrees). --[[User:Mwn3d|Mwn3d]] 12:58, 24 June 2011 (UTC)
::Understood, they are to test the function. Thanks. [[User:Insolor|Insolor]] 16:25, 24 June 2011 (UTC)

==Odd omission from task description==
In 1. the task requires you to write a function that translates <i>degrees</i> to points, and then in point 2. doesn't in fact require you to test that function.  Many of the solutions cheat and just use the index of the test entries (0..32) rather than the actual angles to construct the point-name table, so the scattering of the acceptance values is, in fact, completely useless.  Printing out the angle makes it <i>appear</i> that the angle to point translation is being tested, when in fact it isn't.  I was going to mark the [[Perl]] entry as incorrect, when I realized that it was fulfilling the letter of the task without fulfilling the spirit.  --[[User:TimToady|TimToady]] 17:50, 16 July 2011 (UTC)

:Thanks TT. That was the previously unwritten intent of the task. I've updated it and need to add some incorrect tags. --[[User:Paddy3118|Paddy3118]] 19:36, 16 July 2011 (UTC)

==Why C needs fixing==
Hi Ledrug. If you follow this link copied from the task description:  [[wp:Boxing the compass|box the compass]] to the wp article it shows the original table. Note that the last index wraps around to 1. There is no 33. If you then check the note part of the task description, the pseudo code incorporates a modulo on the index. I do make mistakes, and I try and learn from them when I make them, but I believe that I legitimally flagged a problem with the C code as presented. --[[User:Paddy3118|Paddy3118]] 05:23, 28 November 2011 (UTC)
: For one, the indices are just row count, nothing more, becuase there's no legitimate reason to associate numbers to cardinal directions, that's what the names are for.  If you want a numeric representation of directions, maybe you should consider using, I don't know, degrees?  Secondly, the wp table may justify wrapping table indices around, because its first row centers on 0 while last row centers on 360, which are really the same direction.  Numbers here are slightly off center, I don't see why one should give the impression 354.38 is the same as 0 by forcing the last row index to 1 ("just because a table on WP wraps"? Please.)  You want to know the direction, read the second column.  I don't care what the pseudo code in the task does: translating degrees into direction names should not depend on that. --[[User:Ledrug|Ledrug]] 05:48, 28 November 2011 (UTC)

::Except that we are asked to reproduce the table; and the algorithm for both the degrees and the indices is a given part of the task. Not doing the indicated change is to solve a different task.
::There still is allowed a fair degree of freedom in interpreting the goals of the task, for example in the ommision of bounding boxes; the order of the three columns; the justification of values in the three columns. Changes to those have not been an issue. Some others have managed to add even more information in their solutions whilst still satisfying the task.
::For this task, you are not asked to interpret the validity of the index column of the table, merely to reproduce it, as this should lead to a simpler task of the form "Do this", "Like this". Your insights might well hold technical merit, and might best be added to the wp talk page, but they go beyond the needs of this simpler RC task. --[[User:Paddy3118|Paddy3118]] 06:22, 28 November 2011 (UTC)
::: What ''is'' the task?  To reproduce the wp table which ''defines'' the angle ranges of each direction?  Then what are the input numbers for, particularly when they are neither center nor edges of the 32 directions?  Or is the task asking to translate those numbers into corresponding direction names, hence the word "reproduce" is a farce anyway? I don't think I had any deep "insight" on this, the task is merely lousily worded if you had some well defined goal in mind. --[[User:Ledrug|Ledrug]] 07:38, 28 November 2011 (UTC)

::::Any thoughts on a task clarification to help future implementers not get into the same issue that you have? --[[User:Paddy3118|Paddy3118]] 10:17, 28 November 2011 (UTC)
:::::I don't know--I'm very confused by the task, and I doubt you are any less so.  Let's see the facts:
:::::# There is an established mapping from degrees to 32 direction names (stuff like 5.625 notwithstanding);
:::::# We are to provide a method to implement said mapping; it's not to break the convention used by generations of pirates;
:::::# A list of 33 values are provided, and we are expected to generate a table of some kind that uses a value on each row; the values are not the same as the defining values for the cardinal directions (centers or edges)
:::::# The table is to look like the table in the WP article.
:::::Disregarding 4, then the only sensible interpretation is that the table is a list of the values and their corresponding direction names.  If so, there's not much reason to mandate the table rows be numbered, much less that the last index should wrap around.  And the meaning of 4 is clear as mud: the WP table provides 33 rows containing 32 definitions of the direction names, while table here contains 33 rows with 33 test values, they are not the same thing at all, why do you want any superficial resemblance between them?  If you want the task desc to be clear, getting rid of either 3 or 4 would be a start.
:::::Now for something completely different.  Instead of looking up names in a simple 32-list, most solutions strive to be clever and generate names from shorter strings.  To me this feels like wasted brain activity, which generates heat and contribute to global warming without a just cause.  That, and the fact that the program becomes more complicated without saving much, if any, space.  Plus, it will only work in English: you can concatenate "north" and "east" and tell an English pirate to sail northeast, but try telling a self-respecting French pirate to sail "le nordl'est" and he'll report you to L'Académie française.  Similarly, a CJK pirate sails "東北", that is, "East North".  A dumber method requires straightforward string translation, while the clever ones would have to have the logic rewritten for different languages, totally not worth the effort. --[[User:Ledrug|Ledrug]] 12:45, 28 November 2011 (UTC)
::::::The non-English language argument seems like a good reason to keep the indices. The table in WP defines 32 ranges which just happen to have names like "North" and "Southwest" in English. The names will change between spoken languages but the index (1 to 32) will be the same in all of them. So the index would be the important part and the direction name would just be the human-readable form. --[[User:Mwn3d|Mwn3d]] 13:19, 28 November 2011 (UTC)

::::::You extract four  'facts' from the task decription then immediately disregard one before continuing?

::::::You seem to have given the column marked '#' a meaning of 'index' then say that that meaning does not make sense. What if the '#' column where a shaft encoding, one turn encoded as digits 1 to 32? What if ...? The meaning of that column is not given. --[[User:Paddy3118|Paddy3118]] 20:04, 28 November 2011 (UTC) 
::::::: I disregarded 4 because with it there would be no clear interpretation for the task (what does "instead use these values" even mean? Redefine what "North" is?), that's not my wrongdoing. Like I said, before worrying about what the column is, why don't you clarify what the ''table'' is, and why are we given a list of 33 strange numbers?  Also, if the meaning is "not given", on what basis can you claim the values are incorrect to begin with? --[[User:Ledrug|Ledrug]] 20:30, 28 November 2011 (UTC)
::::::::"The table" is a list of 32 ranges of degrees ("arcs"?) on a compass (for some reason they chose to split "North" into two rows with the same number) and their names. The "number" column (also called "index" in other parts of this discussion) corresponds to the numbers on the compass picture on that article ("1" being "North"). The "strange numbers" are test values we can use to make sure that the algorithm properly maps values to the given named ranges. Since there is no range #33 on the compass (which should probably be mentioned or maybe even duplicated in the task), no given value can correspond to #33. The last value falls within range #1, "North". --[[User:Mwn3d|Mwn3d]] 20:39, 28 November 2011 (UTC)
::::::::: In that case perhaps the index should be tied to the result rather than being taken mod 32. [[User:CRGreathouse|CRGreathouse]] 21:43, 28 November 2011 (UTC)
::: Yes, it's a pretty silly task in that respect.  I found it easier to change the code than to argue for the obvious numbering. [[User:CRGreathouse|CRGreathouse]] 19:01, 28 November 2011 (UTC)
:::: It indeed takes a few keystrokes to make the output compliant--but compliant to ''what''?  In any event, I'm done arguing, I've said more than enough already. --[[User:Ledrug|Ledrug]] 20:30, 28 November 2011 (UTC)

===Direction, index, and angle===
When comparing angles and directions as the wp table does, then you have to deal with the issue of ranges. Angles can go beyond representing one turn - directions are merely headings: turn 720 degrees and you end up at the same heading. If the names of the points of the compass in order are enumerated from 1 you end up with an index that has no 33. It makes sense to index the compass points in such a way as they are difficult to remember - hence their recollection in order being a test. Having thirty three and one as indices would be implying that North and North are different headings/bearings/directions which is an absurdity.

In creating the wp table, the author chose to show a mapping between ranges of angles and compass points that goes (just) beyond one turn of angle. They show a second occurrence of North. They show a second occurrence of Norths index too which is 1. 

If the tables index were of ranges of angle rather than compass point then we would indeed have a different range and so a different index would be appropriate, ''but that is not the case''.

I must admit that I did not have this argument in full to handuntil now. I must also say though that I did note that the index wrapped and that it seemed right and proper to do so. (Wrapping angles so they end up in 0-360 degree range is commonplace for me, almost like a "normallization for angles", so seeing an index wrap when the direction wrapped rang no bells in my case). --[[User:Paddy3118|Paddy3118]] 04:57, 29 November 2011 (UTC)

==Renaming of a programming language==

I hope I didn't break the rules.   The (old) entry under   '''REXX'''   was instead,   '''ooRexx'''.


This would be similar to someone entering a   '''C++'''   entry under   '''C'''. 


If this isn't acceptable, I would like the know what the procedure is, and should the original poster be told of this? 

I didn't like to move an entire (big) programming entry like that, too many things could've gone wrong with my fat fingers and not-so-good eyesight.   -- [[User:Gerard Schildberger|Gerard Schildberger]] 18:21, 13 May 2012 (UTC)
