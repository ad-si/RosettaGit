+++
title = "Talk:Bitmap"
description = ""
date = 2017-08-20T02:14:21Z
aliases = []
[extra]
id = 3834
[taxonomies]
categories = []
tags = []
+++

==some examples don't fulfill the task==
It seems quite a few of the examples don't fulfill the task. The task asks for 3 procedures, Create, Get, and Set, but only two or three of the examples have them.

==unclear on "get the color"==
Also, I'm kind of unclear on the "get the color" procedure. For example, C++ returns a boolean?  I thought the point was to return the pixel's color? --[[User:Mbishop|Mbishop]] 08:02, 20 February 2009 (UTC)
: The boolean return value indicates success.  The C++ GetPixel routine returns the color by modifying three variables the caller passes into it.  The alternative would be to return a special data type, or a packed integer.  Many C++ environments have the special datatype, such as Win32's [http://msdn.microsoft.com/en-us/library/dd162939(VS.85).aspx RGBTRIPLE], [http://msdn.microsoft.com/en-us/library/dd162938(VS.85).aspx RGBQUAD] and [http://msdn.microsoft.com/en-us/library/dd183449(VS.85).aspx COLORREF], but it's not strictly C++, and they're not really necessary.  And returning a packed integer would require the caller to do their own processing.  Using pointers or references as arguments is a common idiom in C++.  However, I can add another C++ example that provides those three types as arguments to SetPixel and return values for GetPixel, and throws exceptions on failure instead of returning false.  --[[User:Short Circuit|Short Circuit]] 09:00, 20 February 2009 (UTC)

:What is unclear to me is, can we use the existing bitmap image services of the language, or should we implement it from scratch using e.g. arrays? In RapidQ, I used QCanvas object. It has built-in methods for filling image, for drawing pixel and for reading pixel color, so there is no need to define any procedures. Many BASIC versions write directly to display hardware (e.g. in DOS or when using OpenGL or DirectX). Can this be used or should we have an off-screen image storage? --[[User:PauliKL|PauliKL]] 16:31, 20 February 2009 (UTC)

::I would say it is OK to use device contexts, hardware buffers etc. So long you can implement the task from this [[:Raster graphics operations|category]]. However it could become quite tricky when it comes to digital filtration and other image processing techniques, because drawing contexts are usually oriented only to rendering. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 20:24, 20 February 2009 (UTC)

:::I would argue that it's appropriate to use any and every OS-specific support libraries, as long as it's not to the exclusion of other approaches.  I don't think "exclusion of external libraries" is idiomatic to any language that supports them.
:::As for issues surrounding drawing contexts...I only have experience with graphics programming with GDI on Win32, but DCs there can be attached to memory buffers for offscreen rendering; It was likely essential for double-buffering on early graphics hardware with limited onboard memory.  I expect most languages support similar mechanisms one way or another.--[[User:Short Circuit|Short Circuit]] 04:38, 21 February 2009 (UTC)
::::I believe it's ok, until you can manipulate the buffers (basically, peeking and poking pixels some way, i.e. if you can do a "get pixel" and a "put pixel"), so that you can implement filters or whatever else, using the same buffer or another one as output. I was thinking about Perl and ImageMagick or Perl and Imglib... I believe also the main aim is to provide a ''framework'' for algorithms that manipulates data that can be thought as images, and so shown. --[[User:ShinTakezou|ShinTakezou]] 14:51, 21 February 2009 (UTC)

==Interface vs Implementation==
I noticed that [[User:Mbishop|Mbishop]] separated Modula-3's interface from its implementation last night.  I think that's actually a very good idea, and should be done for any language where it's considered best practice. --[[User:Short Circuit|Short Circuit]] 17:22, 23 February 2009 (UTC)
: About Fortran I gave up; separating interface and implementation would mean to make the whole stuff a lot longer; and it would make sense if one wants to give the compiled code (while not undisclosing source) and make in the same time people to use the library. --[[User:ShinTakezou|ShinTakezou]] 23:53, 27 March 2009 (UTC)

== Amount of comments ==

This is in particular related to J section. I completely agree that everybody has his own style of documenting the code, and J particularly favors brevity. However, in my opinion, for someone new in J extra comments could be quite helpful, and the purpose of this site is to show how to solve simple tasks.

So, I'd appreciate opinions about what is, in practice, a good level of commenting here for such a language.

A good comment could succinctly convey the purpose of the verb, and even help catch errors in the implementation, which may go unnoticed even with some examples of use. Without phrases like "For the basic bitmap structure, this way to store data is likely the most convenient and natural for manipulating with J." one could wonder why this particular data structure was chosen for the task in hand. [[User:Avmich|Avmich]] 17:57, 29 August 2009 (UTC)

:I totally agree that standalone code should describe the purpose and interface of the verbs/functions defined. However on this site (and for this page) the task description at the top of the page lays out the spec for each of the verbs given, the choice of name for each of those verbs makes it clear which is which rendering the repetition of that spec superfluous and in my opinion it actually detracts from the impact of the entry. None of the other languages repeat that description for each of their functions or document their interfaces, in fact many have no comments at all! I think that the commented example usage section shows more succinctly and clearly the interface for the verbs than a description would.

:Regarding the comment re choice of structure - in my opinion the idea of the task (and site in general) is to show how "best" to complete a task using various languages. The default assumption therefore, is that the approach shown for a language ''is'' the most convenient and natural method.  If it is not, then a comment would certainly be appropriate, or perhaps the less natural approach should be replaced with a more natural approach for the language.

:Regarding the <tt>allocImage</tt> and <tt>fillImage</tt> verbs, I'm not sure the current definition for <tt>fillImage</tt> is really "filling" an existing image, instead it creates a new image with the same shaped structure as an existing one. I think the main issue here is that allocating an uninitialized image isn't really "natural" in J. Instead it would probably be more common to combine the two steps as one. Maybe it would be better to make that comment and replace the current definitions of <tt>allocImage</tt> and <tt>fillImage</tt> with the following which takes the desired size as the right argument, with an optional left argument specifying the colour with black as the default? --[[User:Tikkanz|Tikkanz]] 00:38, 30 August 2009 (UTC)


```j
createImage=: 0&$: : ($~ 3 ,~ ])
myimg=: createImage 7 5               NB. create a 7 by 5 bitmap (black)
myimg=: 255 255 255 createImage 7 5   NB. create a 7 by 5 white bitmap
```


:: The site seeks to aid two groups of users.  One group consists of people who want to learn a language; To that end, the site seeks to aid learning that language by comparing how a familiar or desired task is accomplished in that language with how something is done in a language the visitor is already familiar with.  The other group are the developers of languages and other tools.  Several times in the history of the site, languages have been shown to have elegant solutions (or at least to be capable of solving) to problems where no such solution was thought to exist.  At other times, language developers have found unexpected quirks, inconveniences or inabilities that they were then able to document and resolve in their develoment process.  Additionally, what may be the greatest aid Rosetta Code offers to language developers, advocates and enthusiasts is the exposure that their language gets along side other languages that are perhaps more better known.

:: The purpose of an individual task is whatever the creator of the task had in mind; Anyone is free to create a task and show how it may be accomplished in two or three languages of their choice.  Anyone who provides a solution for the task is free do code and document that solution however they wish, so long as the solution fits the task, and so long as they're aware of and acknowledge normal editing concerns (as linked to at the bottom of every editing form.) 

:: Having an explanation of the spec as it applies to the language provides context and therefore improves that solution's readability and accessability by providing context, which is always a good thing.  The task description doesn't require the documenting of the solution's code with respect to the defined spec, and there's no sitewide policy demanding it, so that documentation isn't required.  That doesn't mean it's unwise. --[[User:Short Circuit|Short Circuit]] 05:01, 30 August 2009 (UTC)

::: I'd note that when doing several of the tasks, I've had to push my knowledge of my [[Tcl|chosen language]] quite hard and even on occasion learn a new library. But in all cases, the perfect solution is one that other users of the site can quickly read, understand, and learn to replicate, and not the shortest answer. After all, we're not playing Code Golf. —[[User:Dkf|Donal Fellows]] 15:56, 30 August 2009 (UTC)

:::: I think we're starting to get off topic here. The original question was about code documentation not about the length of code itself. Documentation/comments fall into two main categories for me. Documenting the purpose and user interface for the functions (how to use the code), and documenting what the code is doing (how the code solves the problem). If I want to use a function, it is frustrating not to know its purpose or how to use it. Documentation and especially (for me) examples of use are especially helpful in that situation. However I think what prompted the question (Avmich, please correct me if I'm wrong) was the removal of some lines that had been added describing the purpose and user interface for the verbs <tt>allocImage, fillImage, getPixels, setPixels</tt>. I removed those lines because in the context of this page and with the usage examples and the initial description of the basic data structure provided, I believe that the user interface is quite clear.

:::: The second category of documentation (description of what the code is doing) can be very helpful to someone trying to understand the approach that has been taken - see [[Happy_Number#J]]. In many cases a big increase in readability can be accomplished by the judicious naming of intermediate functions and objects. 

:::: With regards to the Code Golf comment, I agree that the objective is not to show the shortest possible answer, and I understand that sometimes solutions, especially for a language like J, will be perceived by someone not familiar with it as an attempt at Code Golf. In most cases though I think that is a misconception and is simply due to the increased semantic density of the language achieved by its grammar and use of symbols to represent functions and operators.  This is similar to the difference between the representation of a phrase or concept using one or two Chinese characters rather than a whole sentence in English. I don't know any Chinese and I don't expect to understand it without first learning the meaning of the individual symbols. On the other hand I admit that in many cases the readability of some solutions (especially for beginning users) could be improved by breaking up longer lines into shorter chunks and giving them appropriate names - for example the [[Standard_Deviation#J|J solution for Standard Deviation]] could have been provided as the one-liner: 
```j
 stddevP=: [: %: +/@:*:@(- (+/ % #)) % #
```
 - not an especially complex J expression, but breaking it up in to chunks as on that page adds to readability in my opinion. --[[User:Tikkanz|Tikkanz]] 22:51, 30 August 2009 (UTC)

::::: Having had a loop at those examples, it's clear that excessive terseness is an issue with J code (since it can lead to things that are a challenge to comprehend) and that you're already on the case. Which is very good. In the specific case of comments, they're good anyway, since even a novice can quickly learn to remove them. It'd be greater if we had some syntax highlighting too; even the basic kind helps ever so much. —[[User:Dkf|Donal Fellows]] 08:00, 31 August 2009 (UTC)

== The J implementation ==

First, its probably worth noting that the person writing the specification for "Basic bitmap storage" probably thought that the operations defined here would be useful.  But that's not really the case for J's implementation.  Useful J image manipulation primitives would best be thought of as operations on layers, not operations on pixels.

Second, the J implementation on the main page has been simplified since it was quoted, above.  If people are interested, I could also explain how the older code worked.

That said, here's a breakdown on how the J implementation works:

Note that <tt>allocImage</tt> and <tt>fillImage</tt> have been subsequently replaced by more complicated (but more useful) verbs on the main page. However the descriptions below are still of interest and to some degree relevant to the current verbs <tt>makeRGB</tt> and <tt>fillRGB</tt>.


```J
allocImage=: $&(,:0 0 0)
```


Here 0 0 0 represents one black pixel, and (,:0 0 0) is a one element list containing that pixel.  Meanwhile 
```J
dimensions $ items
```
 creates an array with the specified dimensions from the list of items on the right.  Meanwhile, & curries an operation.  For example:

    +&2 (10 100 1000)
 12 102 1002


```J
fillImage=: $~ $
```


When exactly two verbs (functions) are placed side by side, in isolation, J treats them specially.  The one on the left has its normal dyadic (2 argument, or combining form - arguments appear on each side) meaning, the one on the right has its normal monadic (one argument - which appears on the right) meaning.

    5 (* !) 3
 30
    5 * ! 3
 30
    5 * 6
 30
    !3            NB. 3 factorial
 6

Also, the ~ modifier creates a new function based on the original (but with its arguments swapped, left and right.

   1 -~ 10
 9

Finally, the monadic meaning of $ is the dimensions of the array.  So, $ finds the dimensions of our bitmap and $~ creates a new array with those dimensions.  The items of the list on the left are color values, and they are repeated as necessary to build the new array.


```J
setPixels=: (1&{::@[)`(<"1@(0&{::@[))`]}
```


This one is bit noisy -- unfortunately, J sentences which amend arrays usually have lots of words in them.  Basically,  x f`g`h} y is an operation which modifies elements of an array, creating a new array with those elements.  The new elements of the array are specified by (x f y), the indices of the array are specified by (x g y), the array itself is specified by (x h y).

Here, 1&{::@[ is the contents of the second box in the left hand argument.  [ is the left identity function.

    1 [ 2
 1

And J arrays are indexed starting from 0.  And {:: is a primitive which indexes an array and extracts the contents of a box.  (A box is, in essence, a reference to an array.)

Perhaps it's also worth noting that ; is used to manually construct lists of boxes.

    (2 4 ; 255 255 255)
 +---+-----------+
 |2 4|255 255 255|
 +---+-----------+
    (2 4 ; 255 255 255) (1&{::@[) _
 255 255 255

Similarly, 0&{::@[ is the contents of the first box in the left hand argument.  These are meant to be pixel coordinates, and <"1 puts each each list of coordinates in its own box.

    (2 4 ; 255 255 255) (<"1@(0&{::@[)) _
 +---+
 |2 4|
 +---+

If the indices were not boxed, they would be integers and not lists of integers, and a single integer would only index along the leading dimension of the array -- if the indices were not boxed, they would refer to horizontal rasters, and not to individual pixels.

"Boxing" means the operation < used monadically, which returns a reference to its argument array (much like C's & but J does not allow pointer arithmetic)

    <2 3
 +---+
 |2 3|
 +---+

As an aside: the trailing notation "1 is not very significant in this example, but if we had provided a list of dimensions, <"1 would individually box each dimension.  The problem statement does not require support for lists of coordinates, but it would be rather silly for a J implementation to not support such lists.

Finally, ] is the right identity.

     (2 4 ; 255 255 255) ] _
 _

Thus:

    (0 1 ; 255 255 255) setPixels allocImage 2 4
   0   0   0
 255 255 255
   0   0   0
   0   0   0
 
   0   0   0
   0   0   0
   0   0   0
   0   0   0


```J
getPixels=: <"1@[ { ]
```


The only new operation here is { which selects indexed elements from an array.

    0 1 getPixels (0 1 ; 255 255 255) setPixels allocImage 2 4
 255 255 255
    1 1 getPixels (0 1 ; 255 255 255) setPixels allocImage 2 4
 0 0 0

[[User:Rdm|Rdm]] 00:03, 1 September 2009 (UTC)

== Deprecation ==

I'd like to see this deprecated and replaced with something better. Better in what way? I'm not sure. I just know I created this task ages ago, and I don't particularly care for its not-so-good representation of bitmap processing. --[[User:Short Circuit|Michael Mol]] 14:09, 4 October 2010 (UTC)

== Making the task description less strict ==

I'd like to slightly rephrase this part of the task description:

<div style="background:#eee; margin-left:2em; font-style: italic">
If possible provide a function to allocate an uninitialised image, 
given its width and height, and provide 3 additional functions:
* one to fill an image with a plain RGB color,
* one to set a given pixel with a color,
* one to get the color of a pixel.
</div>

...to this:

<div style="background:#eee; margin-left:2em; font-style: italic">
If possible, show how to:
* allocate an uninitialised image, given its width and height
* fill an image with a plain RGB color
* set a pixel to a given color
* get the color of a pixel
</div>

The difference is to '''not''' explicitly demand one "function" per feature. I propose this because:
* In some languages (e.g. Perl 6), it does not make sense to have separate getter and setter functions for accessing the same property (i.e. get/set a pixel)
* Constructors and methods (which is what most languages use to implement this), are not technically considered "functions" in some language traditions.
* Functional languages may be able to provide more elegant solutions if not restricted to this object-oriented paradigm.

So let's not needlessly restrict the task to a particular language's (or set of languages') way of doing things, and relax the description like proposed above. Since it won't invalidate any existing solutions (and thus won't be very disruptive), I don't think it's too late to make such a change.

Objections?

--[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 11:00, 19 July 2015 (UTC)
