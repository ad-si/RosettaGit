+++
title = "Talk:Strip control codes and extended characters from a string"
description = ""
date = 2017-04-22T23:03:34Z
aliases = []
[extra]
id = 9876
[taxonomies]
categories = []
tags = []
+++

This is very much like other tasks. How about stripping any set of chars from a string, the front, or the back of a string? --[[User:Paddy3118|Paddy3118]] 07:03, 5 June 2011 (UTC)

:This task is about all occurances. I hadn't planned on breaking it down to front, back and within the middle of the string. --[[User:Markhobley|Markhobley]] 10:20, 5 June 2011 (UTC)

:We could always create an additional task "Strip control codes and extended characters from a string/Top and tail" with such details. --[[User:Markhobley|Markhobley]] 10:24, 5 June 2011 (UTC)

:: No point. Anyone wanting stripping of non-ASCII won't want it done just at the front and back of the string. –[[User:Dkf|Donal Fellows]] 14:03, 9 June 2011 (UTC)

::: (To me, non-ASCII means EBCDIC.) I assume your "non-ASCII" meant "ASCII control-codes". -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:26, 1 April 2012 (UTC)

:The reason I didn't go for "any sets of chars", is that the logic may be different. Control codes and extended characters may be filtered without a table lookup (on an ascii based system). I think it would be a good idea to create an additional task to "Strip characters not belonging to a set" to cover this. --[[User:Markhobley|Markhobley]] 10:40, 5 June 2011 (UTC)

:: No need for a table lookup for ASCII or EBCDIC.  Essentially, anything below a "blank" is a control character, with the addition of '7f'x for ASCII. -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:26, 1 April 2012 (UTC)

:The general task could be about stripping a set of chars from a string. For display purposes it would be easier to use something other than control codes as the example. Showing special facilities that a language has for stripping control codes and extended characters could be an optional requirement.--[[User:Tikkanz|Tikkanz]] 12:09, 5 June 2011 (UTC)

::Stripping a set of characters from a string is a good idea. I have create a separate task for that. --[[User:Markhobley|Markhobley]] 13:10, 5 June 2011 (UTC)

== ASCII code 127 is a control character ==

I have updated the task description to be consistent with character code 127 being a control character.  See also: http://www.lammertbies.nl/comm/info/ascii-characters.html or http://en.wikipedia.org/wiki/ASCII#ASCII_control_characters --[[User:Rdm|Rdm]] 15:33, 6 June 2011 (UTC)

== Incorrect statement about ASCII ==

The statement

<blockquote>In ASCII, the control codes have decimal codes 0 through to 31 and 127 and the extended characters have decimal codes greater than 127.</blockquote>

is incorrect because ASCII is only 7-bit (it only defines characters up to 127), so cannot have characters with "decimal codes greater than 127". --[[Special:Contributions/76.21.41.59|76.21.41.59]] 09:53, 25 August 2011 (UTC)

:You are correct.  Extended codes are not a part of the ascii standard.  Also, character 127 is a control character.  I have updated the task description to match.  It needs a sentence added back in, about extended codes for other standards which incorporate ascii.  --[[User:Rdm|Rdm]] 11:01, 25 August 2011 (UTC)

== REXX via translate() ==

I found this page while searching on how to remove extended ASCII codes using REXX.  This helped me to get the answer but the code as presented did not remove the extended characters from even its own example here (tested on Windows and Linux, ooREXX 5.0 and 4.2 respectively).  
My solution probably does not work on EBCDIC without modification but as I have no way of testing I can't say what changes would be needed.

```rexx
/*REXX program strips all  "control codes"  from a character string  (ASCII or EBCDIC). */
xxx= 'string of ☺☻♥♦⌂, may include control characters and other ilk.♫☼§►↔◄'
above = xrange('80'x,'FF'x)  /* extended ASCII characters */
below = xrange('00'x,'1F'x)  /* control characters */
yyy = space(translate(xxx,,below))  /* removes control characters */
zzz = space(translate(yyy,,above))  /* removes extended characters */
say 'old = »»»'xxx"«««"                          /*add ««fence»» before & after old text*/
say 'new = »»»'yyy"«««"                          /* "      "        "   "   "   new   " */
say 'newer = »»»'zzz"«««"                        /* "      "        "   "   "   newer " */
                                                 /*stick a fork in it,  we're all done. */

```

From this, I cannot see that there is any change with removing the control characters from the string specified.  It could be done in one line: <code>yyy = space(translate(translate(xxx,,below),,above))</code>
I post this not as a "better" way but because, at least in the REXX implementation I used, the example code printed the old and new exactly the same with both printing the control characters.

--[[User:Abwillis|Abwillis]] 15:47, 20 April 2017 (UTC)



-----



Thanks for finding that error in my REXX programs that I entered   (concerning the extended characters).   That error has been corrected in both posted REXX programs.

On further examination, I also found that the two older REXX versions where still in error   (which your above version also has the same problem)   in that they were/are also removing multiple (adjacent) blanks, so I rewrote and updated the erroneous two REXX examples.

By the way, the error has to do with translating unwanted characters to a blank, and then removing the   ''excess''   blanks. 
 But that also removes excess blanks   (blanks that are adjacent to another)   from the original string.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:02, 22 April 2017 (UTC)
