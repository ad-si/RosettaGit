+++
title = "Talk:Waveform analysis/Doh ray me"
description = ""
date = 2012-11-04T00:46:51Z
aliases = []
[extra]
id = 10084
[taxonomies]
categories = []
tags = []
+++

==Explanation==
Solfa Trigraphs? Could you add more explanation/links to help those not from that audio processing background? Thanks.--[[User:Paddy3118|Paddy3118]] 13:46, 12 July 2011 (UTC)

:http://en.wikipedia.org/wiki/Tonic_sol-fa
:These have used digraph spelling variant with a single reh. (do, reh, me, fa, so, la, te, do), but I don't suppose that matters. You must remember Julie Andrews: Doe - a deer a female deer, Ray - a drop of golden sun, etc :) [[User:Markhobley|Markhobley]] 14:03, 12 July 2011 (UTC)

==Average pitch or average frequency?==

I think for this task we could use either average pitch or average frequency to achieve this, although I am fairly open minded as to what methods should be used here.

I think we first need to examine the waveform to determine if it is fairly even. (Hopefully, it will just be a "lah" sound from the microphone.

:Maybe find the range of the frequencies used, and hopefully that will fall into one of our solfa bands. If not, then it might be a just a case of deciding between two notes. [[User:Markhobley|Markhobley]] 10:55, 13 July 2011 (UTC)

If the given waveform is fairly even, then I am thinking that we could take the modal result of the frequencies used throughout the given waveform and use the mathematical mode for our average, or if the results do not produce a mode, then we take a median.

:This would really be the modal result of bands of frequencies within the waveform. [[User:Markhobley|Markhobley]] 10:55, 13 July 2011 (UTC)

Others have suggested Fast Fourier Transformation and Enhanced Autocorrelation functions to determine the pitch.

Ideally we want an algorithm that can make the decision reasonably quickly, so that it can be used in sing along type applications, where temporal media is used to provide the waveform, and the results are displayed on the screen as notes are sung into the microphone.

[[User:Markhobley|Markhobley]] 18:03, 12 July 2011 (UTC)

== Move to 'Suggested Tasks'? ==

This task has no implementations and a terrible description. The only thing differing it from the tasks on the suggestion page is a rack full of omit tags.

:What is wrong with the task description? Please make corrections if there is a problem. I've got to get my head around the mathematics for Fourier Transformations, which has delayed the creation of a solution. I am surprised that noone else came up with a solution for this. Maybe there is not a library that provides this facility or not enough people know how to achieve this goal. I have stumbled across a piece of guitar tuner software now, but I haven't tested it for operation yet. However, if it works, maybe I can reverse engineer it to determine its operation, and post some sort of method here. Unfortunately, I am still learning C, so I am not that quick at reverse engineering. It is certainly doable, I have seen a Java based guitar tuner software running and I know that there are sing along applications like Sing Star that utilize this sort of technology. [[User:Markhobley|Markhobley]] 06:03, 11 September 2011 (UTC)
::Well now that I think of it, the description is OK. But you'll just get another omit from me: The languages I use have no audio-manipulation libraries, and thus this would have to be done in binary... I think the real problem w this task is that it is so limited in the number of implemenations it can receive
:::Yeah go ahead and omit. It is also useful to know which languages cannot do this. [[User:Markhobley|Markhobley]] 13:08, 13 September 2011 (UTC)

== Just give frequency ==

Actually, all we need is frequency here. The task could be like a simple instrument tuner outputting the frequency of the given waveform. From the frequency, it is possible to work out the note or the position on the solfa, but all we really need is demonstration of the determine frequency bit. [[User:Markhobley|Markhobley]] 00:46, 4 November 2012 (UTC)
