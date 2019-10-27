+++
title = "Talk:Jensen's Device"
description = ""
date = 2011-05-05T21:57:06Z
aliases = []
[extra]
id = 3140
[taxonomies]
categories = []
tags = []
+++

It is amazing how wrong some ideas of early computing were. Fortunately none of modern languages really supports this mess. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 11:53, 22 November 2008 (UTC)

I don't think the guys in the 1960's had a monopoly on bad ideas... take the new "\" (back slash) operator in PHP[http://news.php.net/php.internals/41374] as an example.  Fortunately, every now and then, a good idea floats to the top, the trick is being able to spot the good idea early and rewind all the bad ideas even earlier. [[User:NevilleDNZ|NevilleDNZ]] 15:01, 22 November 2008 (UTC)

Seeing an entry on Jensen's Device just floats my boat.

Not knowing much about ALGOL60, I'm curious about its semantics -- why is i declared twice? When iterating over i, is that value being assigned local to sum, or is it being assigned in sum's caller? --[[User:Saccade|Saccade]] 21:50, 5 May 2011 (UTC)

:<code>i</code> is passed by reference to <code>sum</code>, the "first declaration" represents the declaration of the storage and the "second declaration" is the declaration of the type of the argument (much like a K&R C parameter list) in the parameters for <code>sum</code>.  --[[User:Rdm|Rdm]] 21:57, 5 May 2011 (UTC)
