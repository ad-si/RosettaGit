+++
title = "Talk:Image convolution"
description = ""
date = 2014-06-30T16:35:02Z
aliases = []
[extra]
id = 3229
[taxonomies]
categories = []
tags = []
+++

==References==
I haven't been able to find the explanation of this technic on the English wikipedia.
There is an article about [http://en.wikipedia.org/wiki/Convolution convolution] but it does not talk at all about how to apply it on image processing. In the French area it is in this article:
* [http://fr.wikipedia.org/wiki/Traitement_d'images#Filtres_lin.C3.A9aires (image processing # linear filters)] (the target points on the good paragraph)
but when I grep in the [http://en.wikipedia.org/wiki/Image_processing English equivalent] I find nothing. Nothing too in the article [http://en.wikipedia.org/wiki/Linear_filter linear filter] even if the word convolution appears, but without details.<BR>
Maybe someone else will be more lucky than me to find where it is (or maybe there is a lack).
[[User:Blue Prawn|Blue Prawn]] 19:31, 7 December 2008 (UTC)

:I've found the following, even though some are still too technical. (First time I've heard about convolution kernels, it was about CA, so my search started from there)

* [http://www.complexity.org.au/ci/vol06/blanc-talon/blanc-talon.html Effective Computation of 2D Coupled Map Lattices]; this is very interesting to me, but too complex and maybe not so useful for the specific case
* [http://www.stephenwolfram.com/publications/articles/ca/85-two/4/text.html CA in 2D]; here the ''convolution'' ''of'' ''a kernel'' is used, but it is not clear where and how... nonetheless, could be interesting
* [http://mathworld.wolfram.com/AdditiveCellularAutomaton.html Additive CA]; this page tells about the link between convolution explained into Wikipedia and its CA-related analog
* [http://www2.bakersfieldcollege.edu/resperic/ca/cellular_automata_dynamics.pdf CA dynamics]; this one maybe is the best, showing how a CA can be defined in term of convolution kernel (both 1D and 2D); from here it is very easy to implement the code for ''applying'' a convolution kernel to an image.

:--[[User:ShinTakezou|ShinTakezou]] 13:22, 17 December 2008 (UTC)

==Border Handling==
I don't know if this is common knowledge that I'm missing, but maybe the task should specify how to handle the border pixels. I think the standard method is padding the image with zeroes (or something of that equivalence) so that the convolution can be taken on all pixels. Some functions, like edge-detection, are benefited from the zero-padding. Others, like smoothing or blurring, are benefited more by not padding and not taking the convolution on the border pixels. Maybe the kernal matrix could be adapted on a step-by-step basis (at least for the border pixels) to still process the border, but normalize the weight of the valid pixels that will be processed (e.g. if on left edge and ker = [0.25 0.5 0.25] the kernal will be adjusted to [0 0.666 0.333] before taking convolution of those pixels).

I would suggest making it clear which should be used in the task, and perhaps make other options extra credit.--[[User:Ooorah|Ooorah]] ([[User talk:Ooorah|talk]]) 13:54, 30 June 2014 (UTC)
