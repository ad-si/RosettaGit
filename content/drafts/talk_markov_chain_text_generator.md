+++
title = "Talk:Markov chain text generator"
description = ""
date = 2018-10-10T07:03:11Z
aliases = []
[extra]
id = 20969
[taxonomies]
categories = []
tags = []
+++

== Task is ambiguous ==

Need to specify algorithm in task description (either copy the rose-hulman.edu description or get a different one here). Need to specify test data in task description (ideally host that here, but if it is huge at least provide a stable off-site link). Need to specify value (or values) for n (in n-grams) in task description - the current implementation looks like it only uses 2-grams and, if that is the intent, this constraint should be specified in the task description. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:21, 27 June 2016 (UTC)


== Rewording ==
I hope the text for the task is now clearer!

And by the way, the C++ implementation does not uses 2-grams as [[User:Rdm|Rdm]] supposed - take a look at the call to create in "main".
--[[User:Paulo Jorente|Paulo Jorente]] ([[User talk:Paulo Jorente|talk]]) 07:43, 28 June 2016 (UTC)

== Strangeness with safari ==

Currently, the J entry wraps oddly in Safari.

This seems to be because of the style sheet http://rosettacode.org/mw/load.php?debug=false&lang=en&modules=ext.bootstrap.styles&only=styles&skin=chameleon which contains '''word-wrap: break-word;''' (along with a bunch of other stuff). Disabling this removes the wrapping (which seems to be on character boundaries rather than word boundaries).

I'm not sure why we have things set up this way, but my impression is that if we are trying to achieve word wrap we should not be using the pre tag (even in the heavily doctored way that it's currently being used).

Any thoughts on this? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 08:07, 28 September 2016 (UTC)

: I just had my REXX entry use a simple word-wrap for the output, based on the width (or linesize) of the terminal screen.     Easy peasy, lemon squeezy.        <!--       Earliest quote is probably from the Hollywood film   "The Long Voyage Home", 1940.       !-->            -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:47, 10 October 2018 (UTC)

== (kinda) broken link? ==
Does anyone else get a semi-broken link    (within the first sentence of this task's preamble)?

It does directs me to the 

<nowiki> 
[http://www.rose-hulman.edu/Users/faculty/young/CS-Classes/csse220/200820/web/Programs/Markov/markov.html Markov Chain algorithm] </nowiki>  

and it displays:
                              Please accept our apologies!
                              We can't seem to find the page you're looking for.

Once there, I enter   (at their site search window):
                              Markov Chain algorithm
but still no joy.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:00, 10 October 2018 (UTC)
