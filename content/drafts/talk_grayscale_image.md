+++
title = "Talk:Grayscale image"
description = ""
date = 2009-01-28T20:24:38Z
aliases = []
[extra]
id = 3308
[taxonomies]
categories = []
tags = []
+++

== Linear color space ==
If I am not wrong, the equation <tt> L = 0.2126·R + 0.7152·G + 0.0722·B </tt> only works correctly in linear color space.
Most images, however, are stored in sRGB color space. If you want to do the conversion correctly for a real world image, you should first convert it into linear color space.

But for simplicity, I guess, we could assume that the image is already in linear color space (even if it actually would require at least 16 bit color depth instead of 8 bit).
--[[User:PauliKL|PauliKL]] 17:00, 15 January 2009 (UTC)

: The "parent" [[Basic_bitmap_storage|task]] is silent about the color model. If you want to fix it in this way or another, I think, you should better start there.

: Surely we could define some additional tasks dealing with different color models and conversions between them. I am not sure that this would meet much interest.

: Thinking further in this direction, there are so many areas in image processing / computer graphics, that I seriously doubt if RC would ever able to represent more than just a fraction of. Especially because most of them quickly grow out of "boy" format. I wished to add image segmenting tasks, by region growing, by pyramid etc, but then I realized that already this would be far over the top... --[[User:Dmitry-kazakov|Dmitry-kazakov]] 18:33, 15 January 2009 (UTC)

::Color models are not relevant in the parent task. Creating a storage for image and plotting pixels do not care about color models. Color models are needed when doing conversions, such as RGB to grayscale conversion.

:::How so? Color points are kept in a definite model. When plotted the device expects images in a certain format which includes color model specification as well. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 17:11, 16 January 2009 (UTC)

::::No. Color points are not "kept" in any model. The image storage is just a block of memory. The memory does not care what is stored in it. You only need to know how much memory you need to reserve. An RGB value is plotted into the image by writing the values of R, G and B in memory. The color space used (gamma etc.) has no effect on this operation. --[[User:PauliKL|PauliKL]] 21:01, 16 January 2009 (UTC)

:::::Image storage is not a memory block. It is a set of language objects that used to store and process images. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 15:39, 17 January 2009 (UTC)

::::::Wrong. Image storage ''is'' a memory block. In ''some'' languages there may be sugar coating trying to hide this fact, but it does not change the fact that image is always stored in a block of memory. The functions used to process images have nothing to do with the ''storage''. --[[User:PauliKL|PauliKL]] 11:20, 23 January 2009 (UTC)

::The point is that the CIE conversion function mentioned requires linear color mode. If linear color model is ''not'' used, you could use just about any weighting factors for R, G and B, and it does not help to use so many decimal digits. A common function used in sRGB color space is <tt> L = 0.3*R + 0.59*G + 0.11*B </tt>. When this is applied to and sRGB image, the results are closer to CIE conversion than when you use the CIE function in sRGB color space. I just thought that this should be mentioned, in case someone is planning to use the code examples given here in a real application.

::Another commonly overlooked fact is that most image processing functions such as resizing only work correctly in linear color space. But that is another story.
:: --[[User:PauliKL|PauliKL]] 16:00, 16 January 2009 (UTC)

:::No, they work in any model, of course. It is so that the algorithm depends on the model, obviously. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 17:11, 16 January 2009 (UTC)

::::No, they do not work. An image editor performs an operation always the same way, it does not care about the color space. If you order the program to add the values of two pixels, it adds the values of two pixels. And that works correctly only in linear color space. If you do not believe me, try it. The color space is only relevant when you perform conversion from one color space to another. In theory, it would be possible to convert each pixel separately to linear color space and back when performing each image editing operation, but that would make the image editor so extremely slow that nobody would want to use it. --[[User:PauliKL|PauliKL]] 21:01, 16 January 2009 (UTC)

:::::No, I don't believe you. Pixels are not numbers, you cannot add them unless you define a vector space of, supplied by a definition of the operation +. The operation + in that space corresponding to certain semantics (to be specified) is a part of a color model. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 15:39, 17 January 2009 (UTC)

::::::Wrong. Pixels ''are'' numbers, and of course they can be (and are) added, multiplied or whatever operations you are doing. Everything stored in computers memory is numbers.

:::::::That does not make any sense, sorry. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 17:39, 23 January 2009 (UTC)

::::::::These facts make perfect sense to anybody who has even basic knowledge about computers. Obviously you are not one of them. May I suggest that you get one of those "Computers for dummies" books and start learning the basics. --[[User:PauliKL|PauliKL]] 19:45, 25 January 2009 (UTC)

::::::But adding two pixels in non-linear color space does not give correct results. That was my original message. If you are editing an image in an image editor and you want to do it right, you should first convert the image into linear color space. (In case of RGB to grayscale conversion, the image editor probably does the color space conversion automatically.) --[[User:PauliKL|PauliKL]] 11:20, 23 January 2009 (UTC)

:::::::When something does not give correct results, then it is called incorrect. Which in this case is another way to say that it cannot be added, like apples and oranges. See [http://en.wikipedia.org/wiki/Pixel pixel] --[[User:Dmitry-kazakov|Dmitry-kazakov]] 17:39, 23 January 2009 (UTC)

::::::::That is just plain ridiculous. There is no law of nature that would stop people from making mistakes. If there was, accidents would never happen, and for example at school everybody would always get just straight A's. The fact is that it is perfectly possible to perform image editing operations in non-linear color space. And ignorant people like you do that all the time, and therefore get bad results. That is why I gave the advice. --[[User:PauliKL|PauliKL]] 19:45, 25 January 2009 (UTC)

:::::::::I don't know what did you mean by saying this. An ability to implement certain image operations on images represented using certain color model does not imply that you can do it without any model. The rest is just a lack of some basic understanding of the fundamental difference between representation (computer model) and the modeled thing. Did you have a [http://en.wikipedia.org/wiki/Computer_science CS] course? In short, you can imagine it as the memory device having states associated with things being modeled, like number, color point, text, employee, program etc. The program itself is merely description of state transitions, see [http://en.wikipedia.org/wiki/Finite_state_machine FSM]. Then you might try to understand the difference between a hardware instruction (merely controlling transitions) and what this instruction is about to implement (like the [http://en.wikipedia.org/wiki/Ring_(mathematics) ring] operation +). That would help you to learn why there are so many numbers in computing (modular, ranged, saturated, fixed-point, packed decimal, floating-point etc). This might save you many silly discussions in the future. Pixel is not a number in any possible sense. It is not as a model ([http://en.wikipedia.org/wiki/Palette_(computing) for example]), in case you mixed it with what it represents, namely a color point. On the other side a color space is a [http://en.wikipedia.org/wiki/Vector_space vector space], not a [http://en.wikipedia.org/wiki/Ring_(mathematics) ring], [http://en.wikipedia.org/wiki/Field_(mathematics) field] or other mathematical structure associated with vague "number." Furthermore, in the color space the operations on the points can be defined differently depending on semantic meaning of the arguments and the result. For example, when addition of colors means mixing corresponding water-color pigments on white paper as observed by a statistically normal human, or else whatsoever. I hope that helps. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 10:16, 26 January 2009 (UTC)

::::::::::Yes, it helped to confirm that, indeed, you do not know anything about computers, and definitely nothing about computer graphics. You are right in one thing: you do not understand. You do not understand what pixels are. You do not understand that everything in computer memory is numbers and can be manipulated. Heck, you do not even understand that it is possible to make mistakes! Not even after it was explained to you!
::::::::::Obviously, it is hopeless to try and explain things to Dmitry-kazakov. But perhaps there are others who are willing to learn. --[[User:PauliKL|PauliKL]] 20:24, 28 January 2009 (UTC)

: As I said earlier, most '''image processing functions work correctly only in linear [[wp:color space|color space]]''' (gamma 1.0), since that is what they have been designed for. However, most images are in [[wp:sRGB|sRGB]] color space, which uses [[wp:Gamma correction|gamma]] of approximately 2.2. Therefore, it is advisable to convert the image into linear color space for editing, and then convert the finished image back to sRGB.
: Here are some links for more information:
:* [http://www.4p8.com/eric.brasseur/gamma.html Gamma error in picture scaling]
:* [http://www.sjbrown.co.uk/2004/05/14/gamma-correct-rendering/ Gamma-Correct Rendering]
:* [http://softimage.wiki.softimage.com/index.php/Gamma,_Linear_Color_Space_and_HDR Gamma, Linear Color Space and HDR]
:--[[User:PauliKL|PauliKL]] 20:24, 28 January 2009 (UTC)
