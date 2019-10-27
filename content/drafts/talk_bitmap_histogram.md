+++
title = "Talk:Bitmap/Histogram"
description = ""
date = 2010-06-24T15:48:09Z
aliases = []
[extra]
id = 3227
[taxonomies]
categories = []
tags = []
+++

It would be nice to host the test PPM files at Rosetta. Anybody to help? --[[User:Dmitry-kazakov|Dmitry-kazakov]] 18:41, 7 December 2008 (UTC)
: Along what I have understood you need the "bureaucrat" status. Dunno who you have to ask, probably the admin. I think it would be nice to put an image for cubic and quadratic bezier curves too even if everyone know how it looks like. [[User:Blue Prawn|Blue Prawn]] 19:31, 7 December 2008 (UTC)
:: Bureaucrat status would be sufficient if I could get around to making sure file uploads worked.  Most times I've just uploaded the files manually.  I'll hopefully have some time tomorrow or the day after to get things working properly.  I'll also take a peek at making the upload functionality more broadly available.  --[[User:Short Circuit|Short Circuit]] 10:14, 22 January 2009 (UTC)

== Task description ==

What are we supposed to do in this task? The title hints that we should write a function to create an image histogram. That would make sense. But the description talks about who knows what.

What does it mean that a data storage should "support image histograms"? That makes no sense. The data storage stores an image, I don't see how this storage could "support histograms". 

I don't understand the sentence "Choosing the histogram data format take care about the data type used for the counts.". What is "histogram data format"? How choosing it could take care of some data type? Maybe the purpose of the writer was to say something like: "You must choose a data type that is capable of storing count even if all the pixels are of the same color."

If the purpose of this task is to create image histogram, why does most of the description talk about converting image into black and white art? It has very little to do with histogram. (Histogram would only be used as one possible way to automatically choose the threshold level). If such conversion is needed, it should be a separate task.

--[[User:PauliKL|PauliKL]] 17:20, 15 January 2009 (UTC)

: I have fixed the working. Is it better now? --[[User:Dmitry-kazakov|Dmitry-kazakov]] 18:16, 15 January 2009 (UTC)

:: Not much. I still do not understand why the bitmap storage would need to be extended. We only need a separate histogram function that reads image data from that storage.
:: With the sentence "Choosing a histogram representation...", I guess you mean something like "When choosing a histogram representation, ...".
:: But I still think converting image to B/W art has very little to do with histogram, and it should be a separate task.
:: --[[User:PauliKL|PauliKL]] 16:14, 16 January 2009 (UTC)

::: It already is [[grayscale image|see here]]. "Bitmap storage" is a task, which was intended, as I understand it, to define types and/or other language objects necessary to deal with images. Histogram type/class, call it is as you wish, logically belongs there. It extends that package. If you want to restructure it, I think you should discuss it there, or ask [[User:ShinTakezou]], who created it. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 16:55, 16 January 2009 (UTC)

::::: I've not created the [[grayscale image]] task, nor the [[basic bitmap storage]] task; I've created the [[read image file through a pipe]], which is "complementar" to [[PPM conversion through a pipe]]. I suppose there's nothing odd asking a "pragmatic" usage of a task, that also works like an example of usage of a function that complete the task. There are many other tasks having usage example. The B/W (even though using the grayscale output, using just two levels, Black and White) through histogram "analysis" is so easy ans simple that likely it does not throw shadows over the real task. --[[User:ShinTakezou|ShinTakezou]] 22:47, 18 January 2009 (UTC)

:::: No, no, no and no. The link you gave is '''not''' B/W image, it is grayscale image. I am talking about the B/W conversion that is specified in '''this''' task. And it should '''not''' be in this task. It is an entirely separate function from histogram calculation. The idea of Rosetta Code is to have separate simple programming tasks impemented in different languages. There is no point putting multiple unrelated tasks into a single task definition.
:::: Histogram calculation is an individual function that performs an operation. It is not a type or a class. And it definitely does not belong in the definition of image storage. But if you think it belongs to image storage, the why is it not there? It is ''here''. Similarely, conversion to 1 bit/pixel B/W image is an etirely separate task, unrelated to histogram calculation, so it should not be here in the histogram task. (But that conversion is so simple that there is probably no deed to create a task for it.)
:::: --[[User:PauliKL|PauliKL]] 21:30, 16 January 2009 (UTC)

:::::OK, if you think that black and white images deserve a separate task, you can create it. But this task was to use histogram in order to create black and white art image, which is still a grayscale one. I could use three clusters instead of two, or N clusters, but I felt it overstretched to add local maximum/minimum search or other methods of clustering. Would that be a three-color image task then? The number of result colors is irrelevant in the task. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 15:49, 17 January 2009 (UTC)

::::::You just do not get it. It is not about the number of colors. The point is that the black and white conversion ''should not be here at all''. This task is titled '''Image histogram''', so one would expect it to be about creating a histogram. Not about b/w conversion. If you insist that there must be a task for b/w conversion, you can create it. But, as I said, there probably is not much use for such task. --[[User:PauliKL|PauliKL]] 21:05, 28 January 2009 (UTC)
