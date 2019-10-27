+++
title = "Rosetta Code:Village Pump/LibRC, A Library for RosettaCode Code"
description = ""
date = 2016-09-29T12:33:06Z
aliases = []
[extra]
id = 20996
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=LibRC, A Library for RosettaCode Code
|summary=I wish to take all the code segments on RosettaCode that can be libified and turn all of it into a one-stop shop library for anyone to use. (The only problem is I don't know some of the languages on here and so need contributors!)
}}


Hello all,
I wish to create a library, called LibRC, that will contain all the code segments that can be turned into a library code fragment and turn them into a code fragment. I can take all of the C, C++, Fortran, Go, Java, Objective-C, and Objective-C++ ones because I compiled, myself, a copy of all the GCC 6.1.0 languages (accept Ada because Gentoo's Ada compiler won't emerge properly) and so I can't compile that one.

The library name is LibRC, but I can change it if you like. (I haven't even managed to compile it all yet!)
I would like to also discuss possible naming schemes for the library files, if this is an accepted project by everyone. Here's a possible one that I just thought of:

* Ada: librcada.so (-lrcada in GCC)
* C: librcc.so (-lrcc in GCC)
* C++: librccpp.so (-lrccpp in GCC)
* Fortran: librcfortran.so (-lrcfortran in GCC)
* Go: librcgo.so (-lrcgo in GCC)
* Java: librcjava.so (-lrcjava in GCC)
* Objective-C: librcobjc.so (-lrcobjc in GCC)
* Objective-C++: librcobjcpp.so (-lrcobjcpp in GCC)
Of course, I'm willing to change this if you guys think that this is an inadequate or protracted naming scheme. (I'm not sure how we'll compile the interpreted languages like Make, CMake, etc.)

So, what do you guys think? If you want to help me with this project, simply say so and I'll work on adding you to the development team! (Or, you can even suggest other methods of working on this!)

: So we will have both support for 99 bottles of beer on the wall and for sleep sort in the same library?

: I joke, but so does some of the code...

: Anyways, if getting that to compile sounds like an interesting task. But before you spend too much time organizing, I think I would try to characterize some of the difficulties and some of the hardware dependencies that you care about.

: More than that, though, the primary benefit of Rosetta code is having something that people can read and compare. (But making sure all the code compiles is important - I know that I myself have sometimes come back later and found that code that I had written did not work, because I had either not made an edit or some other stupid mistake.)

: Still, if you also mean for this library to be of use to other people you would need to spend some time thinking about those people's needs and how to document the library and its use for them. The easy thing to do (just link to the site) will work about like just pulling down all the code samples and trying to compile them together.

: Anyways, *we* need efforts like yours, to help us isolate our problems. But I expect that most of the resulting library would mostly not be things most other people need. Still, the skills you might develop, working through this, might turn out to be useful to you, in other contexts.

: But I would not worry about the naming, either, I guess...

: (But, do not take my thoughts too seriously, either, I always manage to overlook important stuff.) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 15:51, 10 July 2016 (UTC)
: I wish to answer each of your questions in order:
:: So we will have both support for 99 bottles of beer on the wall and for sleep sort in the same library?
: The support is based on if the code sample's directly executable code (like main() functions or other such functions that an operating system calls automatically) can be stripped safely without damaging the original sample and making it uncompilable. Since, for instance, C/C++/C++/CLI main() functions can be usually removed safely either without moving the code within the code sample because it can be extern'd or by moving the code within the main() function and relocating it to another class or set of function calls, this could be done easily. However, for other languages where main() does not exist or main() does not need to exist (like PureBASIC and other BASIC dialects), this will be much harder.
:: I joke, but so does some of the code...
: Very true. I do see that. Comments shall remain in the code samples for the singular objective of preserving the code sample as much as possible during libification. If the comment describes a function, it shall be turned into a documentation comment. Once all comments are updated (if updating is required) then a documentation generation tool like doxygen will be run on the code that doxygen can understand and we'll find other ways to document the code that cannot be interpreted by doxygen.
:: Anyways, if getting that to compile sounds like an interesting task. But before you spend too much time organizing, I think I would try to characterize some of the difficulties and some of the hardware dependencies that you care about.
: I don't necessarily care about hardware dependencies. Seeing as the library will be built via the standard GNU build system (configure, make, test, install) hardware dependencies shouldn't be a problem. Regarding difficulties, I think that the primary difficulties will be documenting the library, writing the tests, and writing the scripts required to build the library in the first place.
:: More than that, though, the primary benefit of Rosetta code is having something that people can read and compare. (But making sure all the code compiles is important - I know that I myself have sometimes come back later and found that code that I had written did not work, because I had either not made an edit or some other stupid mistake.)
: I do understand that. I shall try and preserve the code as much as possible and ensure that it is readable at the same time. Formatting tools will be used to assist in this task.
:: Still, if you also mean for this library to be of use to other people you would need to spend some time thinking about those people's needs and how to document the library and its use for them. The easy thing to do (just link to the site) will work about like just pulling down all the code samples and trying to compile them together.
: And that's what I will do: link to the site as a reference guide.
:: Anyways, *we* need efforts like yours, to help us isolate our problems. But I expect that most of the resulting library would mostly not be things most other people need. Still, the skills you might develop, working through this, might turn out to be useful to you, in other contexts.
: That's why the library will have 624 sub-libraries (and more than 800 components) (each sub-library being a library for each language). The library will be humongous, but people will be able to decide what they want compiled in and what they do not want compiled in. (The configure script is going to have soooooooooooooooo many options, though...)
:: But I would not worry about the naming, either, I guess...
: That's good. I thought the name would have to be changed. I'm glad I don't need to worry about that aspect.
:: (But, do not take my thoughts too seriously, either, I always manage to overlook important stuff.)
: That's fine. Everyone does that. By the way, how are you replying to these messages? I'm hand editing it for this particular reply. [[User:ethin|Ethin]] ([[User talk:ethin|talk]])

::: Yes, I am also editing and typing by hand. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 16:25, 12 July 2016 (UTC)

:But but... Many of the code examples have not been written with a "library" intent so as to be worthy of reliance in all (or even most) circumstances! Surely many have been written with a view to demonstrating the idea and especially for the examples only, rather than full generality with diligent attention to structure and interface and accuracy and speed and storage use and error testing and recovery and ... Indeed, in many examples it is stated that error checking is not wanted because the example input ''is'' correct. In other words, someone intent on implementing some algorithm for a serious purpose could well gain insight from the various demonstration routines, but ''must not hope to evade putting thought to the matter'' by copying in a "library" routine. And, in my case, I have provided at least one example (of valid code) with the remark "This will ''not'' work" - this in [[Jensen's_Device|Jensen's Device]] - and others may have done so as well... So, there seems to me to be no way to select suitable library-worthy examples without a great deal of human effort. [[User:Dinosaur|Dinosaur]] ([[User talk:Dinosaur|talk]]) 12:33, 29 September 2016 (UTC)
