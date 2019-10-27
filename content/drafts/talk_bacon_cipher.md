+++
title = "Talk:Bacon cipher"
description = ""
date = 2015-09-12T11:55:16Z
aliases = []
[extra]
id = 19574
[taxonomies]
categories = []
tags = []
+++

==Focus?==
Hi is this task good to leave draft status or does it need more focus - thinking of the intermediate form which is historically to hide the secret message in font changes, which would shift the tasks focus away from the cipher side and would be hard to show output on RC and hard to get input in differing fonts for 'round-tripping' tests. Some have displayed the binary only; others have used capitalisation changes on either random text or some story.

--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 17:28, 10 September 2015 (UTC)

: If you're referring to the REXX entry, I chose to use the decimal digits   '''1'''s   and   '''0'''s   (ones and zeroes)   instead of   '''A'''s   and   '''B'''s.   It may look like binary, but it tain't.   I had thought of using what the Rosetta Code task's example uses, but I went with another set of characters, just to show how faux "binary" would look like.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:23, 10 September 2015 (UTC)

:: Since it appears that the REXX output looks like binary (and that was the original intent, to make the encoded text look like something it's not), I then added another REXX version (more idiomatic) that takes that concept (of using fonts) even further.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 08:31, 11 September 2015 (UTC)

:: (or should I sign it as): ┴┴┬┬┴┴┴┬┴┴┬┴┴┴┬┴┴┴┴┴┬┴┴┴┬┴┴┴┬┬┬┬┴┬┴┬┴┴┬┴┴┴┴┬┴┴┴┬┬┬┴┬┴┴┴┴┬┴┬┬┴┴┴┬┬┴┴┴┴┬┴┴┬┴┴┬┴┴┴┬┴┴┬┬┴┴┴┬┴┴┬┴┴┴┬┴┴┬┬┴┴┴┬┴┴┬┴┴┴┬┴┴┴┴┴┬┴┴┴┬┴┴┴┬┬┬┬┴┬┴┬┴┴┬┴┴┴┴┬┴┴┴┬┬┬┴┬┴┴┴┴┬┴┬┬┴┴┴┬┬┴┴┴┴┬┴┴┬┴┴┬┴┴┴┬┴┴┬┬┴┴┴┬┴┴┬┴┴┴┬

::: Hi Gerard, you are obviously having a great time thinking of alternate ways of depicting the encrypted output. However, how about a rewording that states that mixing of case on some unrelated snippet of usable (check the license) text should be used in the first instance, but other methods could be shown too? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:30, 12 September 2015 (UTC)

:::: I don't mind asking for a task's requirements (or wording) to make the requirements clearer (or the definitions being used), but I've been on the receiving end of people changing (to the point of being a different task), often for the worse, to the point of the task not making any sense, and the changes made without any discussion.   I thought a purpose of encrypting (using a cipher) was to make the code hard (well, harder) to crack (understand), and using one Latin letter (both the lowercase and uppercase version of '''S''') certainly went a long way towards that goal for this task's intent.   I took a a set of characters (or glyphs) one step further (as witnessed above), which I thought were nice looking to boot.   Some Latin letters make great choices for that purpose:   Cc, Kk, Oo, Ss, Uu, Vv, Ww, and Xx come to mind.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:55, 12 September 2015 (UTC)    

::::: Hi, the wikipedia article uses that word steganography which I took to mean a kind of hiding in plain sight. If the text the message was hiding in was riveting enough then the reader is supposed to skip over the changes in font used. Some captain of the guard reading of some tryst between the gentry and his titillation making him forget the odd use of font or in our case capitalisation. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:09, 12 September 2015 (UTC)

: I could see standardizing some aspects of the task. Perhaps requiring a specific text (either to encrypt or to hold the encrypted text or both), for example. Though I could also see it being more interesting with variation being either allowed or specified. But the font aspect is something of a problem - if you seriously wanted font support, you should first implement a Rosetta Code task which involves multiple fonts. But since there's no file upload capability, the only way to see correctness would be to render to html or wiki-markup. So that might be a dead-end. So perhaps for this task we should just standardize on using letter case in place of font? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:36, 10 September 2015 (UTC)

:: Scanning quickly through Rosetta's 3 goals as a kind of checklist
:::# '''"as many different languages as possible"''' – the task is already good in this respect
:::# '''"demonstrate how languages are similar and different"''' – requiring a specific text would make comparison easier and more revealing. (And wouldn't exclude a second text for optionally added mystery :-)
:::# '''"aid a person with a grounding in one approach to a problem in learning another"''' (as above – comparing like with like is a value for the learner) – perhaps also explicitly encourage explanatory comments  ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 11:55, 12 September 2015 (UTC)

:: +1 on a change to specify the use of upper/lower case to hide the message instead of the font change to make the task easily representable on RC and focus on the other parts of the cipher. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:55, 11 September 2015 (UTC)


### Task changed with three added points. Acceptable?

I have added three points to focus the task without causing too much pain to the authors of existing examples (I hope), and following the discussion above. Comments/corrections welcome :-)

  --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:45, 12 September 2015 (UTC)

-----

I'm not sure I completely understand your wording (or perhaps, word choice):

 '' ... must provide an example that uses a change in the case of successive alphabetical characters instead. ''

Now, to me, successive alphabetical characters could be   '''AB''',   '''RS''',   or   '''KL'''   ...   for instance).

Now, in     ''changing the case''     of those successive characters   ...   to what?   '''Ab''',   '''rS''',   ... ? 

As I understand the Bacon cipher, only two characters (usually Latin letters for this audience) are used.   Once two successive letters (characters) are chosen, I don't see how changing the case could be done;   it only makes sense to me if <strike>one</strike> there is a change to the case of a single letter, and that the upper and lower case versions (of a single letter) are the characters chosen for the Bacon cipher. 

(I assume the Latin alphabet is being referred to, but could Greek or Cyrillic be used?   Or Hebrew?   Arabic?) 

By the way, did you mean to say,   ''successive alphabetical letters''   (instead of   ''characters'')?   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 08:26, 12 September 2015 (UTC)

: I meant skipping (as in carying through without altering), any non a-z in the text used to hide the message. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 09:39, 12 September 2015 (UTC)
