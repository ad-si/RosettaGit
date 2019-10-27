+++
title = "Talk:Fast Fourier transform"
description = ""
date = 2015-11-15T10:47:40Z
aliases = []
[extra]
id = 10064
[taxonomies]
categories = []
tags = []
+++

==Possible renaming==
Would this be better named as "Fast Fourier Transformation", rather than just the initials? [[User:Markhobley|Markhobley]] 06:33, 11 July 2011 (UTC)
:Or "Fast fourier transform"? --[[User:Paddy3118|Paddy3118]] 07:38, 11 July 2011 (UTC)
:: Wikipedia uses “Fast Fourier transform”. This would fit with our capitalization policy (since “Fourier” is a surname). –[[User:Dkf|Donal Fellows]] 07:52, 11 July 2011 (UTC)
:When I google for fft my first hits are the relevant ones.  I suspect it's good enough, though we should probably link FFT to one of those pages, just in case. --[[User:Rdm|Rdm]] 08:10, 11 July 2011 (UTC)

::Moved and linked. --[[User:Paddy3118|Paddy3118]] 08:39, 11 July 2011 (UTC)

== Explanation of input data please ==

Hi. A big problem with pages such as this (and I challenge you to try finding FFT information anywhere else on the internet which is simpler) is the lack of explanation of various parts of the explanantion.
To explain: The code is nicely clear in the various langauges but no mention is given of what the input data is or what the output data is. I am looking for FFT information in the context of feeding in WAV data (audio) and getting out an array of frequencies for a particular time-frame (say 100ms). This 1,1,1,1,0,0,0,0 is meaningless without an explanation.
Could somebody please fill in the blanks for the rest of us?
Note to others: You can never put enough information in these articles. Assume everybody is in the dark about something...
Thank you
 -Sam

: The FFT is a sort of "self inverting transformation" (within limitations - your data needs to have a length which is a power of 2, for example).  If you use it on the 1 1 1 1 0 0 0 0 result you should get back WAV data you started with (multiplied by a constant).  There are a variety of directions you can take this. One thing you can do is make small changes to the FFT result before you transform it back, and then compare how the original sounds with how the result sounds. (You will probably want to cancel out the constant so the volumes are the same.)  Basically, though, what you are seeing is the volumes of the different frequencies of the sounds. Higher frequence sounds sound "higher" or "sharper" while lower frequency sounds sound "lower". Try it and see? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:40, 13 December 2013 (UTC)

:: Hi Rdm, Thanks for the reply. I think the problem is that there is nothing on the page to show the relationship between the 1,1,1,1,0,0,0,0 and the output numbers. I suppose this might be obvious to someone completely familiar with fourier transforms, but without a guide "key" I can't see it. Usually at least two examples are the minimum needed to show the working of a function (if the function mechanics are not terribly obvious). For me, it isn't so important to just be told "Put X into Y and you should get Z" (although that would be nice) but rather an explanation of why X into Y yeilds Z. - It's a similar problem with Wikipedia maths articles - they are all shorthand maths-professional to maths-professional articles and take a bit of figuring out. I don't mind figuring stuff out but need a bit more to go on (eg, more than one example data set). Eg, suppose I were to input 100ms of samples at a sampling rate of 22khz, mono, 16 bit - I would expect to see an array of frequencies output, but looking at the code, I can't immediately see what the resolution of that output array would be (how many elements), and how it knows that a window of 100ms worth of samples have been input. 

::This is the closest thing I have found so far to an easy-to-read code block, and I am very grateful to have found it here (I'm looking mostly at the C++ one, but having all the different languages here make it much easier to see the common elements of the code) but it isn't quite complete. A few more examples of input/output data would be a huge help to anybody reading this (any language)
::Thanks again for writing- 

::Sam

::: So, ok, let's say that you have 22khz mono 100ms of samples. As I understand it, 22khz means you are taking 22050 samples per second, or 2205 samples in 100 ms. This will not work for FFT since 2205 is not a power of 2.  The nearest power of 2 is 2048 (92.87 ms).  You can do a fourier transform with 2205 samples, but it will be a general fourier transform and not the fast one documented here.  So let's say you've got 2048 samples and they are 16 bit samples. And, let's further say that you are using the J implementation (since that is the one I am most familiar with).  In the J implementation, the first number is the amplitude of the dc component of your 92.87 ms of wav samples - this is inaudible and you can probably ignore it. The second number, in this case, is the amplitude of the base frequency of your sample, which has a wave length of 92.87 ms (or about 10.8 Hz). The third number is the amplitude of the next harmonic (21.5Hz = 2*10.8Hz). The fourth number is the amplitude of the next harmonic (32.3Hz = 3 * 10.8Hz), the next number is for 4 * 10.8Hz and the one after that is for 5 * 10.8Hz... They are just volumes, basically.  

::: (Note also that the J implementation needs to be scaled by the number of samples when you convert back. So, in this case, you would need to divide fft(fft(samples)) by 2048 - or you could modify fft so that it divides by the square root of the number of samples...). Presumably other implementations will have similar issues, but it's easy enough to plug in some values and see. (make an array of 2048 instances of the value 1, run fft on it, run fft on it again - if the result is 2048 instances of the value 2048 that means you need to divide by 2048 to get the right volume.)

::: Does this make sense?  

::: Note also that the second half of the result of fft (the last 1024 values in our hypothetical 92.87 ms sample) should be zero or nearly so (when converting from wav). Lots of volume in those values there should be expected to sound awful when you convert back (because of the simplifications that made this technique work in the first place). If this is a problem you should probably go with a 44KHz sample rate instead...

::: Is this within the realm of things you feel comfortable experimenting with? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 21:11, 14 December 2013 (UTC)


Hi Rdm, Happy New Year
Just wanted to say thanks a lot for your explanation. I have successfully generated a frequency chart from my WAV data based on what you said. Interesting that only half of the output from the FFT function is useful.

So just to clarify for anybody else who is reading this: 
WAV sample values (for example) go into the FFT function in batches of say 2048 (or any multiple of 2 because that is how the FFT works, and requires it) and 2048 complex numbers are then output.
Just input the raw sample values in groups of a multiple of 2 into the FFT function. The same number of <complex> numbers will be output from the FFT.

Only half of the numbers that the FFT function outputs are useful for plotting the frequency spectrum. The other half are a mirror image of the first half so can be disregarded in most common cases.
Because the FFT function outputs complex numbers (made up of a real part an an imaginary part) you need to convert the complex numbers back to real numbers for plotting. 
This is done as follows (C++):

// Calculate magnitude using:
double FFTABSValue = sqrt( real(data[i])*real(data[i]) + imag(data[i])*imag(data[i]) );

You take the square of the real portion of the complex number and add it to the square of the imaginary part of the complex number, and then take the square root of sum of those two. This gives a plottable "magnitide" - might need to adjust it for scale. (in C++, "real" and "imag" are built in functions to take the appropriate part of a complex number separately)

Note: this also makes the result absolute due to its squaring, and absolute values is what you want to plot.

Once you have plotted the 1024 frequencies (frequency bins I believe they are referred to as) you can then choose a different part of your WAV file, read in another 2048 values into the FFT function and plot a new frequency chart from the results. Each chart will probably only represent a few milliseconds of your WAV file, but by passing in successive chunks of 2048 samples from your wav file, you should get a dynamic moving frequency chart (FFT) which if you plot in 3D (one chart behind the other) you will see how frequencies change with time.

Any comments or corrections to what I have just said would be welcome - but I just wanted to write what I have figured out, and what I understand so far.

-Sam

== scala ==
Hey, I rewrote the Scala implementation since it needed attention. I think it is much more readable now, but there are two places that could be more idiomatic, and I should probably put an @tailrec hint in there somewhere.
Does Scala really not have a standard complex math library? Surprising...
--[[User:BooleanLobster|BooleanLobster]] ([[User talk:BooleanLobster|talk]]) 2014-04-27

I prefer to declare exp as a method of the Complex class:

```scala
def exp = Complex(cos(im), sin(im)) * (cosh(re) + sinh(re))
```

Invocations become z.exp instead of exp(z)

--[[User:Jolkdarr|Jolkdarr]] ([[User talk:Jolkdarr|talk]]) 2015-11-15
