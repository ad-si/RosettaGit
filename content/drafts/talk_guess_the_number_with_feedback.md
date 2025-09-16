+++
title = "Talk:Guess the number/With feedback"
description = ""
date = 2012-07-01T19:30:19Z
aliases = []
[extra]
id = 8635
[taxonomies]
categories = []
tags = []
+++

==Why draft?==
Please sync with: [[Talk:Guess the number#dupe? (sort of) ]]  --[[User:Paddy3118|Paddy3118]] 6:06, 29 October 2010 (UTC)
:Personally, I think this game is a more interesting game than that one. --[[User:Rdm|Rdm]] 16:44, 2 November 2010 (UTC)

I feel quite lucky to have found this bit of info. However, I rather be able to compare each of the coded at it's most minimal to process the same randomized fully interactive goal...

GUESS THE NUMBER GAME

Please enter a number between 1 and 100: 50
Your guess was too low.
Try again: 75
Your guess was too high.
Try again: 60
You guessed correctly in 3 attempt(s)

Play again? y/n
n

==REXX example==
In REXX
 if g>high then do
 call ser g 'is above the higher limit of' low
 iterate
 end

shouldn't this be high?

--[[User:Walterpachl|Walterpachl]] 19:10, 30 June 2012 (UTC)

-----

Yes, corrected to:

```rexx
  if g
high then do
                 call ser g 'is above the higher limit of' high
                 iterate
                 end
```

-- [[User:Gerard Schildberger|Gerard Schildberger]] 19:30, 1 July 2012 (UTC)
