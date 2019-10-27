+++
title = "Talk:Bilinear interpolation"
description = ""
date = 2019-10-14T00:28:55Z
aliases = []
[extra]
id = 16075
[taxonomies]
categories = []
tags = []
+++

== What is the task? ==

What is the task? Right now, it's just a description and some examples. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 12:44, 28 August 2013 (UTC)


== J Implementation? (and more "what is the task here?") ==

I'm trying to understand what all is going on in the J implementation.

First off, the obvious: this is a lot of different implementations of interpolation (for the 1d cases), bilinear interpolation (for the 2d cases) and what I suppose would be trilinear interpolation (for the 3d cases). It really warrants some good examples to illustrate what all is going on.

But some parts of it mystify me. For example, there is the commented out implementation of <code>combinations</code> which seems to be another alternative for the [[Combinations#J|Combinations]] task. But the only use of that routine is to compute <code>2 comb 3 [ require'stats'</code> and even that is commented out. Why is that there?

Also, there's the structures <code>CORNERS</code>, <code>N2</code> and <code>N3</code>. <code>CORNERS</code> is <code>_1+2*#:i.4</code> and <code>CORNERS</code> is also a prefix of <code>N2</code>. But the prefix of <code>N3</code> is <code>(|."1)_1+2*#:i.8</code> and that <code>(|."1)</code> part seems arbitrary, so I'm wondering how much of the rest of these constants is arbitrary. The use of A. already hints at something a bit arbitrary, and I guess I'd like to understand that better. For example, why isn't <code>N2</code> something like <code>(\:|),/,"0/~~,N1</code> or <code>(\:|)_1+3 3#:i.9</code>? Is that because of something to do with parity? Anyways, I think this deserves some explanation.

And, finally, the page suggests this be used for image processing, which in turn suggests something like the approach used in the [[Image_convolution#J|Image convolution]] task. (And, also, that only the 2 dimensional cases are relevant as anything other than exposition.) But that mostly just reminds me that [[User:Dkf|Donal Fellows]]'s question (above) remains unanswered. And, I guess I don't really understand what this task is about yet.

Anyways, there's what looks like some fun stuff here, but I'm thinking it might belong on the J wiki, rather than here on Rosetta code?

Then again, I'm not at all clear what the Rosetta Code task here should be... --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:14, 26 July 2015 (UTC)

:I unilaterally decided on a task, but I'm open to discussion. Most people use bilinear interpolation for image enlargement. --[[User:TimSC|TimSC]] ([[User talk:TimSC|talk]]) 07:54, 16 December 2016 (UTC)
