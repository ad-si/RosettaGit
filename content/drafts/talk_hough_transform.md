+++
title = "Talk:Hough transform"
description = ""
date = 2010-09-16T18:33:31Z
aliases = []
[extra]
id = 5347
[taxonomies]
categories = []
tags = []
+++

==Can I get some pseudo-code...and possibly an amen==
I have read about the Hough Transform in an attempt to create a MATLAB implementation, but every resource I use has a different take on this transform. The background theory is the same, but the algorithms that I have seen are different. Even the TCL and C implementations on this site are somewhat different from some other implementations I have found. Can we possibly get some solid pseudo-code to implement to make sure any other solutions provided in different languages conform to some standard specification? Because, at this point, my MATLAB implementation is not going to come close to resembling the algorithm implemented in TCL and C. Can I get an Amen?!--[[User:Cferri|Chris Ferri]]
: Not really. What's important at the surface is that the inputs match and the outputs match. How different examples achieve this will vary by how the problem best maps to each language. MATLAB's implementation ''should'' differ, because MATLAB looks at mathematical problems differently from TCL or C. Though, yes, we definitely need more examples. --[[User:Short Circuit|Michael Mol]] 18:35, 9 August 2010 (UTC)
::But, I thought the point of Rosetta Code was to be able to compare implementations of a common task between languages. I understand that this definition doesn't invalidate your response, but I don't see the point of ''not'' specifying a pseudo-code to implement. It would be much easier for: not only novices trying to figure out how to do something using the solutions as examples, but also developers who are confused about how to implement this particular algorithm, if there were pseudo-code to refer to. I say this especially since my implementation requires an "edge-detected" version of the image as the input (a boolean array where the array member at the index of an "edge" pixel is true). And, from what I can tell, the TCL and C solutions don't. So, I am not exactly sure what is going on in those algorithms which allows them to produce valid output. This is why pseudo-code is important.--[[User:Cferri|Chris Ferri]]
::: The problem is that pseudocode makes assumptions about implementation. I can write pseudocode that says "do this, then do this, then do this", but that tends to assume an imperative programming paradigm. It may be possible to have a universal pseudocode, but I doubt it. [http://www.reddit.com/r/programming/comments/5zypl/ask_reddit_is_therecan_there_be_a_standard_for/ This] is what I got the last time I poked around for one. That doesn't leave me hopeful. I've seen what happens when someone creates procedural pseudocode as part of a task description, assuming that it was part and parcel to what the task was supposed to accomplish, and then I've had to deal with reconciling the task's description against its intent and the languages which it accidentally made things structurally difficult for.
::: As I understand it, Hough depends a great deal on averaging values, which is going to be very difficult if you're only allowing yourself 1 bit of precision. You might try expanding that 1 bit to a larger range (i.e. an integer 0 for false, 255 for true, or a float type with 0 for false, 1 for true) before doing your operations, and then compressing back down to 1 bit prior to output. I'd make suggestions about how to choose range size, but I think that depends a great deal on your use case.) --[[User:Short Circuit|Michael Mol]] 19:40, 9 August 2010 (UTC)
::::Ummm. I guess I don't really mean pseudo-code but a human language translation of the algorithm the the C and TCL implementations are using. So this is what I mean by pseudo-code.<br />
Example:
<lang>function houghTransform
input: Image (boolean) where If Image(x,y) == True Then pixel at (x,y) is an edge pixel
       Angular Resolution

for x from 0 to width of Image
  store in a 2D array: x * cosine(theta from 0 to pi in steps of Angular Resolution)

for y from 0 to height of Image
  store in a 2D array: y * sine(theta from 0 to pi in steps of Angular Resolution)

for x from 0 to width of Image
  for y from 0 to height of Image
   if Image[x,y] == True
     store in a 2D array called accumulator: rho[j] = x*cosine(theta[j]) + y*sine(theta[j]) for all j

For each value of theta
  histogram the rho values in accumulator where the bin size is equivalent to 1 pixel starting at 0 until all rhos are bins
  store in a 2D array called houghSpace: the histogram where each histogram is indexed by theta and
  the contents of each bin are the amount of "votes" for each rho value

Optional: Normalize houghSpace so the max votes are 255

Plot houghSpace where the x axis is Theta from 0 to pi in degrees
and y axis is rho from -sqrt(width^2+height^2) to sqrt(width^2+height^2) in pixels.

```


::::It's obvious that there is no pseudocode that would work universally. But, in the case that a language can not directly implement some arbitrary pseudo-code, it is still possible for someone to read the pseudo-code, understand the general algorithm and then transform the pseudo-code into something that works in that language.

::::This is something that I had to do when I programmed all the sort functions in LabVIEW and MATLAB. All the pseudo-code for the sorts are written for languages with while loops and 0-Based arrays, where as LabVIEW only has the do...while loop and MATLAB uses 1-Based Arrays. It would have been impossible for me to program the sort functions in these languages without that pseudocode.

::::This is especially important here because the literature I've read on the hough transform is inconsistent about how to define the hough space. Is it theta from -90 degrees to 90 degrees? Or mayhaps 0 to 360, or 0 to 180? And is rho defined on the interval from -Image Height to +Image Height, or 0 to Image Height, or -sqrt(width^2+height^2) to sqrt(width^2+height^2) or 0 to sqrt(width^2+height^2). Is the granularity of the rho axis 1 or something more or less than 1? 

::::Also, how are the votes tallied? Some people use some sort of gradient weighted method. Some just have a simple 1 vote per edge pixel. These happen to be the two most common voting metrics, but there are others.

::::I think all of these things should be specified in the task description, and the most comprehensive way to do that, in my opinion, is to provide some sort of pseudo-code. --[[User:Cferri|Chris Ferri]]

::::: You have my sympathy, but I question the extent to which things must be identical. The point is that the generated output is not strictly an image, but rather a graphical plot of intensity over Hough Space. This means that you can scale either axis or rotate the angular axis and still get something that is “the same”. Add in the point that different languages have different natural interpretations of pixels (How many color channels? Over what range?) and you really end up with something other than what you might wish. However, I ''specifically'' want to permit multiple methods of implementing the transform; if someone's got some clever technique or useful library, let them use it! This is not a task that calls for slavish copying. As long as the mathematical transform is implemented and it is capable of processing images, I would count it as a solution (and I'm deliberately vague about whether it should be a color or B+W image).
::::: For the record, it's not very difficult to implement from scratch IMO once you get the idea of what the transform is doing. –[[User:Dkf|Donal Fellows]] 22:55, 9 August 2010 (UTC)

:::::: Yeah, it's not difficult to implement from scratch... the problem is there are many algorithms that perform this transform...just like almost every mathematical transform. And, in cases like the DFTF, there is an established algorithm that is generally used because it is the fastest, or most precise, etc whereas the others that perform the same task are disused. This is the problem I had. It was no problem for me to design the algorithm...but I ended up doing it 4 different ways in an attempt to figure out the best way. This is why I think we should define a specific algorithm that performs this transform. Leaving the option for varied inputs and outputs is cool, but if we want to be able to compare implementations something in this task should be specified. Especially, the inputs and the outputs. I can't even tell if the TCL output is wrong. (though I have the feeling it is, by definition of the hough transform you have all the information within one period of cos and sin by plotting two periods you are simply replotting information you already know from the first period. But that is for another thread.)

:::::: Anywho if I'm the minority vote here, I am not going to go cavalier and modify the task spec. But, I think the task designer should ''seriously'' consider making the task spec more specific. If not for the sake of clarity, then for the sake of the sanity of non-guru programmers and people not willing to do serious research on the topic.  --[[User:Cferri|Chris Ferri]]

::::::: I will say that I expect that the task would be easy, if I had enough of a clue to recognize when I had a correct solution.  Right now, I have no way to test an implementation for correctness, so I do not find this task interesting.  --[[User:Rdm|Rdm]] 00:09, 10 August 2010 (UTC)
:::::::: A pixel-pixel comparison against the output of the original Tcl program wouldn't suffice? --[[User:Short Circuit|Michael Mol]] 00:13, 10 August 2010 (UTC)
::::::::: That has the potential to fail, depending on floating point implementations and rounding/truncating behavior of each language and/or platform. --[[User:Coderjoe|Coderjoe]] 00:41, 10 August 2010 (UTC)

:::::::::: Is the TCL implementation correct in the first place? I've tested my MATLAB code against other test inputs like the one given on the wikipedia page for the hough transform and my output was correct in those cases. But, the output from my MATLAB code and the TCL code disagree for the pentagon. --[[User:Cferri|Chris Ferri]]
::::::::::: Based on your output image alone, I can see that your MATLAB code iterates a half-circle and has both negative and positive rho. The TCL implementation (mistakenly) iterates two full circles (only one is needed), and only positive rho. The output image is a plot of theta on the X and rho on the Y, with the top-left corner being 0,0. Obviously your implementation's output wouldn't match the TCL implementation. Does that mean that the TCL is wrong? Or is the MATLAB wrong? Or are they different ways of doing the same basic thing, with correspondingly different outputs? --[[User:Coderjoe|Coderjoe]] 01:17, 10 August 2010 (UTC)

:::::::::::: Hm. Well, it's a sinusoidal function, so I think it means the Tcl output image is only twice as wide as necessary. It'd be like mapping sin(theta) from zero to 4pi, instead of -pi to pi. You're correct in that that makes it incompatible as a comparison without including theta range and pixel mapping details in the task description. In this case, I think that falls somewhere akin to specifying input/output file formats. It doesn't change the core logic, but it does change interpretation of the thing. --[[User:Short Circuit|Michael Mol]] 01:29, 10 August 2010 (UTC)

:::::::::::: Except the TCL output image isn't twice as wide. The TCL output is only 360 wide, and as far as I can tell, the extra 360 pixels added to "row" are discarded by tkimg. --[[User:Coderjoe|Coderjoe]] 01:33, 10 August 2010 (UTC)

::::::::::::: My problem with the TCL code is not how it outputs the image or what ever...it's the algorithm it uses to "tally the votes." The MATLAB code iterates through each of the original image pixels and bins all of the sinusoids for "edge" pixels (aka the votes). From what I can tell the TCL implementation iterates over each of the pixels in the hough space to construct the transform. But, I can not for the life of me figure out how the original image pixels cast votes for rho and theta values in the hough space in the algorithm implemented by the TCL code. --[[User:Cferri|Chris Ferri]]

:::::::::::: The Tcl code uses the ranges ''ρ''∈[0…''R'' ] and ''θ''∈[0…2''π'' ] (where ''R'' is a “radius” that is “sufficiently large”). It also uses an origin in the middle of the image. Saying that the ranges should instead be [−''R''…''R'' ] and [−''π''…''π'' ] respectively is just nit-picking, as is picking any other origin. All it does is change the offsets of (and possibly cyclically rotate, due to the sinusoidal repetition) the output. –[[User:Dkf|Donal Fellows]] 08:54, 10 August 2010 (UTC)

::::::::::::: The TCL code, as written, actually runs ''θ''∈[0…4''π'' ), and I had a question about that below. (720/180 = 4) --[[User:Coderjoe|Coderjoe]] 09:11, 10 August 2010 (UTC)

:::::::::::::: Actually, it runs to 2''π''; it's proceeding by ''half'' degrees. If it was going twice round, it would have double the number of spots in the generated image. –[[User:Dkf|Donal Fellows]] 14:03, 12 August 2010 (UTC)
:::::::::::::: I was wrong. I just had a bug that was being hidden by the fact that I'd configured the size of the image first. Oops! :-) –[[User:Dkf|Donal Fellows]] 14:19, 12 August 2010 (UTC)

::::::::::::: I'd like to point out this paragraph from the page describing how to add a new programing task. [[Help:Adding_a_new_programming_task]] "The criteria you set should not be so tight as to be language-specific. After all, that defeats the founding point of Rosetta Code. However, the criteria should not be so vague as to have multiple interpretations. Removing code from the wiki would be the tragic result. Situations where the appropriateness of a programming example hinges on the interpretation of the tasks' criteria should be avoided as much as possible." [[User:Cferri|Chris Ferri]] 03:20, 12 August 2010 (UTC)
:::::::::::::: I wrote that. I've also learned a lot about creating tasks since then--to the point where I haven't tried doing it myself in a long time. I'll see about refining that later. --[[User:Short Circuit|Michael Mol]] 13:05, 12 August 2010 (UTC)


### Scaling?


: The above pseudo-code has: <lang>and y axis is rho from -sqrt(width^2+height^2) to sqrt(width^2+height^2) in pixels.
```
 But is width and height the width and height of the png (like the J and Matlab implementations)? Or is it the width and height of the contained geometry (like the C and TCL implementations)?  Or am I mis-interpreting those images? --[[User:Rdm|Rdm]] 12:50, 16 September 2010 (UTC)
::Those are the width and height in pixels of the image that is being transformed[[User:Cferri|Chris Ferri]] 18:33, 16 September 2010 (UTC)

==PNG Image==
I have a small png file available (320x240)to use with this task, created with Inkscape, but I don't know how to upload it to the site. --[[User:Rldrenth|Rldrenth]] 21:01, 21 January 2010 (UTC)

:Choose “Upload file” in the “toolbox” sidebar, or by any other means go to [[Special:Upload]]. —[[User:Kevin Reid|Kevin Reid]] 21:25, 21 January 2010 (UTC)

::Thanks. --[[User:Rldrenth|Rldrenth]] 22:20, 21 January 2010 (UTC)

::: I had to replace that image; it was done with a transparent background rather than a white one, which is highly unhelpful for this task. –[[User:Dkf|Donal Fellows]] 00:02, 22 January 2010 (UTC)

:::: Running the current Tcl code gives black in the (lower) unaffected area ("#000000" as default fieldColor); (my C impl does so without chance to change it but recompiling). --[[User:ShinTakezou|ShinTakezou]] 08:42, 6 August 2010 (UTC)

:I uploaded a new copy of the PNG. The one that was in use had a 1px border on the right and bottom edges which were transparent black. Existing example output probably should be recomputed to reflect this correction. --[[User:Coderjoe|Coderjoe]] 23:50, 9 August 2010 (UTC)

==Expand To Print Peaks==
Should we expand the scope so the program to determine the location of peaks in the transformed image and then print then r & theta that correspond to the lines? --[[User:Rldrenth|Rldrenth]] 21:01, 21 January 2010 (UTC)
: Probably better done as another task that builds on top of this one as locating the peaks is ''not'' part of the Hough transform itself. –[[User:Dkf|Donal Fellows]] 00:02, 22 January 2010 (UTC)

== I think I am missing something ==

How is this useful?  If I try and implement this, how can I tell if I have implemented it correctly?  I look at the description of the task, and then I look at that result image and I look at the references and I feel like I am missing something.  --[[User:Rdm|Rdm]] 15:37, 19 May 2010 (UTC)
: Being a type of transform, I suppose I'd try to use it as part of image fingerprinting. Just a rough idea, one could run the transform, find the median value M, max everything there and above M, drop everything below M, find center points and store their relative positions. (I'd probably go back and adjust M until I had a certain fixed number of center points, or as close to it as possible.) --[[User:Short Circuit|Michael Mol]] 18:22, 19 May 2010 (UTC)
I must be missing something. Is there a reason the tcl implementation (the only one at this time) is looping theta through two complete circles? --[[User:Coderjoe|Coderjoe]] 17:04, 22 July 2010 (UTC)
:Because it had a dumb bug. A think-o. –[[User:Dkf|Donal Fellows]] 14:20, 12 August 2010 (UTC)

== Semantic MediaWiki ==
(Moved to [[Rosetta Code:Village Pump/Semantic MediaWiki/Semantics]] (--[[User:Short Circuit|Michael Mol]] 18:05, 29 August 2010 (UTC)))
