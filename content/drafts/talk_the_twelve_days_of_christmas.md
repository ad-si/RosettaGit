+++
title = "Talk:The Twelve Days of Christmas"
description = ""
date = 2017-12-21T21:42:47Z
aliases = []
[extra]
id = 16951
[taxonomies]
categories = []
tags = []
+++

==Draft status==
We usually keep a task in draft status until there are five or so solutions, and no problems noticed with the task description.  Thanks.  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 18:36, 19 December 2013 (UTC)

==The Gifts of the 12 days==

A related task is to count the gifts of the 12 days. 
*1st day: 1 partridge in a pear tree
*2nd day: 2 turtle doves, 1 more partridge, etc.
*12th day: 12 drummers, 11 more lords ... 1 more partridge

364 gifts total.

Or just the total number birds given: 184 birds


==nit picking capitals==

The lyrics that this Rosetta Code tasks points to have two idiosyncrasies (which may be dependent upon one's viewpoint):
:::* The word '''french''' (''French hen'') should be capitalized.
:::* The word '''twelfth''' ("twelfth day of") in the last verse shouldn't be capitalized.
Should the programming examples include the two idiosyncrasies? 

-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard 
Schildberger|talk]]) 21:43, 19 December 2013 (UTC)
:I would note that we do not capitalize "french fries".  And "Twelfth Night" is a holiday in some circles.  But I think it doesn't matter, and will gladly include "capitalization" in the list of things that are up to the programmer's discretion.  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 01:03, 20 December 2013 (UTC)

:: The reason we don't capitalize ''french fries'', ''brussel spouts'', or ''manhattan'', ''margarita'', and ''daiquiri'' (the drinks: named after a US city, a first name, and a Cuban city) --- those names are no longer associated with their origin, but instead, the particular food or drink.   Capitalization isn't consistent,  however:   ''Bloody Mary''   (named after Queen Mary I of England).   I'm sure there are a lot of counterexamples (and possibly counter-counterexamples).   French fries are claimed (by some) to be originated in the Spanish Netherlands (now Belgium), but them's fighting words and there's no need to start a who-did-what-first war. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:19, 20 December 2013 (UTC) 

-----

Also, "The Twelve Days of Christmas is a religious song, that is say, there are deeply religious symbolisms in the lyrics.

::* 1   True Love refers to (the Christian) God
::* 2   Turtle Doves refers to the Old and New Testaments
::* 3   French Hens refers to Faith, Hope and Charity, the Theological Virtues
::* 4   Calling Birds refers to the 4 Gospels and/or the Four Evangelists
::* 5   Golden Rings refers to the first 5 Books of the Old Testament, the "Pentateuch"
::* 6   Geese A-laying refers to the 6 days of creation
::* 7   Swans A-swimming refers to the 7 gifts of the Holy Spirit, the seven sacraments
::* 8   Maids A-milking refers to the 8 beatitudes
::* 9   Ladies Dancing refers to the 9 Fruits of the Holy Spirit
::* 10   Lords A-leaping refers to the 10 commandments
::* 11   Pipers Piping refers to the 11 faithful apostles
::* 12   Drummers Drumming refers to the 12 points of doctrine in the Apostle's Creed

Because of this, point (1) means that '''True Love''' (as it refers to a deity), should be capitalized.

Also, the word '''Bible''' (and parts thereof) when referring to the Christian or Hebrew Bible (Torah, Koran, and others?), must be capitalized.   Because of this rule, ''' Turtle Doves''', '''Golden Rings''', '''Calling Birds''' should also be capitalized.


Who'd ever thunk that this situation would even be brought up? 

Capitalization based on religious beliefs seems very strange thing to be talked about on a programming Wiki.

This sure turned into a sticky wicket.
 
-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:49, 19 December 2013 (UTC)
:I've been in religious circles all my life, and this is the first I've heard of this.  And to my ear, it sounds like a cheesy Victorian attempt to retrofit the lyrics with numerology.  As such, I'd pay it no mind, and again make the capitalization completely optional.  (By the way, in many modern translations of the Bible, they have at least stopped capitalizing the pronouns referring to God, even if they still capitalize the proper nouns.  The 19th century sentiment of capitalizing the metaphors because they <em>might</em> refer to diety is just Right Out these days.  :)  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 01:03, 20 December 2013 (UTC)

:: You're right about the cheesy part -- it sounds too convenient and artificial.   I was informed that it was used as a religious teaching aid (and a way to learn about certain articals of faith, or a catechism).   As an aside, I learned that the gifts were ''sent to me'', not ''gave to me'', but a lot of the words and phrases have changed over the years and varies what country/region it was first heard. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:19, 20 December 2013 (UTC)

::yes to retrofit

==Suggest a task name change==
To "The twelve days of christmas" to get the right capitalization. Or maybe without the initial "The". --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:40, 20 December 2013 (UTC)

: If the Rosetta Code task name refers (or ''is'') the title of the song ''The Twelve Days of Christmas'', then it would need capalization (as shown in the italicization).   Poem and song titles have their own (some quirky) rules of capitalization.   If, on the other hand, you're referring to or merely naming a Rosetta Code task, than all bets are off, as the ole saying goes. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 14:04, 22 December 2013 (UTC)

==question about Ruby==

I have a question about Ruby (in gernal) in using the   '''.split("\n")'''   function  (or whatever it's called).

What happens when the same program is put on a system where there are no   '''\n'''   indicators in the file, but the ''newline'' is instead, ''indicated'' by the end-of-record in the file structure.   This would be the case of (generic) OS (MVS) or CMS (both IBM systems). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 16:14, 22 December 2013 (UTC)

Probably the correct way to split would be split(&/), &/ being the input record separator global variable. I've never seen it used, except for golfing, but Ruby uses it with methods like gets and readline.--[[User:Steenslag|Steenslag]] ([[User talk:Steenslag|talk]]) 21:34, 22 December 2013 (UTC)

: Is it a "real character(s)" or can it be a metaphysical construct (such as a pseudo-End-of-Record)? -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:36, 22 December 2013 (UTC)

== Lyrics Standardization ==

The linked lyrics are one variation, but not the traditional version, which has "My true love ''sent'' to me" rather than "gave", and "gold rings" rather than "golden". (Admittedly, "golden" scans better, but it's pretty much purely an American innovation; see [https://en.wikipedia.org/wiki/The_Twelve_Days_of_Christmas_(song)#Variations_of_the_lyrics Wikipedia].) My submissions all have "gold rings", although I was inconsistent with regard to 'gave" vs "sent"; going back to correct that now. Anyway, I suggest the task description be modified to allow the traditional lyrics instead of requiring exactly the linked ones. --[[User:Markjreed|Markjreed]] ([[User talk:Markjreed|talk]]) 15:12, 22 December 2015 (UTC)
