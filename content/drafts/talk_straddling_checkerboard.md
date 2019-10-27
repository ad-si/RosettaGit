+++
title = "Talk:Straddling checkerboard"
description = ""
date = 2011-06-24T20:52:12Z
aliases = []
[extra]
id = 9885
[taxonomies]
categories = []
tags = []
+++

==Scramble?==
The [[wp:Straddling checkerboard|Wikipedia page]] implies that the order of the digits 0-9 can also be scrambled to improve security. To make the task more complete but not much more complicated, I think it would make sense to also provide numbers to the function in the desired order. To leave the existing cipher for the example valid, the inputs to the StraddlingCheckerboard function would become ("0123456789HOLMESRTABCDFGIJKNPQUVWXYZ./", 3, 7) --[[User:Tikkanz|Tikkanz]] 15:05, 6 June 2011 (UTC)

: You mean the order of the columns? Changing the order of the columns would be the same as rearranging each of the rows. Since you can input the whole alphabet in any order (as opposed to just a keyword to get things started, for example), it wouldn't add anything new. [[User:MagiMaster|MagiMaster]] 19:44, 6 June 2011 (UTC)
::It might be nice just to standardize the examples. Then we can have example input and output in the description for testing purposes/proof. --[[User:Mwn3d|Mwn3d]] 19:58, 6 June 2011 (UTC)

::: I was having trouble finding some existing piece of text that was relatively short and had a few numbers and full stops. [[User:MagiMaster|MagiMaster]] 20:49, 6 June 2011 (UTC)

== Category ==

I hit the wrong button while I typing in the reason for the undo. Anyway, the category will be included automatically once this is promoted to full task status. [[User:MagiMaster|MagiMaster]] 19:51, 6 June 2011 (UTC)

== Stop character ==

I kind of like how it looks when using the stop character to (instead) represent any unencrypted characters (like the Util's Perl6 solution did). Should I put this as part of the specification, or leave it as is? Since this is supposed to be a demonstration of a historical method, how it was actually used should weigh in the decision, but I don't really know much about that. [[User:MagiMaster|MagiMaster]] 04:07, 7 June 2011 (UTC)

From [http://users.telenet.be/d.rijmenants/en/secom.htm The SECOM Cipher] (which uses a straddling checkerboard):
:In our example, several spaces are used. However, it is recommended to omit all spaces where legibility preserved.
AFAIK, when you use *any* field cypher in real life, you make a first pass over the plaintext to reduce whitespace, using human judgement on where space must be maintained to preserve the meaning of the message. This improves the cyphertext's resistance to cracking. The cipher procedure itself is mechanical, and cannot presume to know what will be "legible" if words were to be run together. Space reduction is not helpful in examples, and not appropriate for this task.

The only reason I added the $collapse version in my Perl 6 solution was to show encoded&decoded output *identical* to the only other solution. I would prefer to remove the $collapse code.

I recommend these changes to the task:
* Specify the plaintext and key+u+v in the task.
:* This will make it easier to check for correctness in the encoded output across all solutions.
* Choose the task's plaintext to contain a mix of only spaces, numbers, and uppercase letters.
:* Perhaps: RV TOMORROW AT 1400PM TO COMPLETE TRANSACTION USE DEADDROP AS USUAL
* Choose the task's key such that the first row contains ESTONIAR in some order, and place . and / somewhere in row 2|3 that is *not* in the last two positions.
* Continue to assume an order of 0123456789, since the option of its reordering is redundant to reordering the alphabet in the key.
* Choose u and v where u>v, to catch solutions that miss (as the C++ solution does) the distinction in where to use original-(u,v) versus sorted-(u,v),
* Specify that space becomes stop char when encoding.
* Leave as unspecified any details on how to handle invalid chars in the plaintext. Let solutions die on invalid input, or translate to stop char, or whatever.
:* Alternately, specify that non-alphanumeric chars be treated as whitespace, since that is what would probably happen if the field encoder was not the plaintext author.
:* This point is moot if you limit the task's plaintext to only spaces, numbers, and uppercase letters.
* Allow for solutions to (when decoding) either convert stop char to space, or leave as stop char.
--[[User:Util|Util]] 15:50, 9 June 2011 (UTC)

:: Sounds good to me. If no one else makes the changes, I'll make them later. Either way, I'll rerun the C++ solution afterwards. [[User:MagiMaster|MagiMaster]] 18:15, 9 June 2011 (UTC)

:: I agree that these sound like sensible changes. There are a number of solutions using the initial (draft) task description, is it still acceptable to make these changes?--[[User:Tikkanz|Tikkanz]] 15:39, 18 June 2011 (UTC)

::: Sorry. I completely forgot that I said I'd make those changes. Yeah, I think we should change the description before any more examples get added. [[User:MagiMaster|MagiMaster]] 21:28, 18 June 2011 (UTC)
::: I'm going to have to let someone else do it though. I'm no good at picking out demonstrative examples here. I'll fix the C++ code to whatever gets posted. [[User:MagiMaster|MagiMaster]] 21:42, 18 June 2011 (UTC)

:::: Too late...! I think that specifying "numbers should be encrypted by inserting the escape character before each digit" is redundant, since it is a part of the algorithm (wikipedia explains how digits should be handled). The first paragraph seems to give a particular "function signature" as task requirement, though it sounds strange/weak requirement, and thus I did not create functions taking that input explicitly. --[[User:ShinTakezou|ShinTakezou]] 21:45, 18 June 2011 (UTC)

== Min/max ==

I don't think min and max are in <algorithm>, at least not in Visual Studio. Is it different with GCC? [[User:MagiMaster|MagiMaster]] 11:06, 7 June 2011 (UTC)
: When I comment out the #include line for algorithm, and compile with GCC's `g++` command, I see no change in behavior. --[[User:Util|Util]] 15:32, 9 June 2011 (UTC)
:: K. No point in including extra files then. [[User:MagiMaster|MagiMaster]] 18:15, 9 June 2011 (UTC)
::: In C and C++, removing headers and seeing if it compiles is ''never'' a good way to determine if a header is needed. It is very likely that your compiler's version of some other header you included (or some header it included, etc.) happen to have included <algorithm>, but it is possible that some other implementation of those same headers won't. To make your code portable you should always make sure to include the right headers for all the functions you use, from the language standard. min and max are defined in the <algorithm> header in section 25 (Algorithms library) of the C++ standard. Otherwise, what header do you think it's defined in? --[[User:Spoon!|Spoon!]] 20:02, 9 June 2011 (UTC)
:::: I don't really know where they're getting included. It works with both the major compilers, but it's fine to leave it as long as there's a reason. [[User:MagiMaster|MagiMaster]] 01:22, 10 June 2011 (UTC)
:::::For the record, in GCC, the file that defines min&max (stl_algobase.h) is included not only by algorithm.h, but also (at some level of #include depth) by any of the other headers being used: iostream, string, or map. I suspect that such under-the-cover inclusion would be the case in many compilers.
:::::After some thought, I agree with Spoon!. Since I have had trouble compiling the (never tested on GCC) C++ solutions of some other tasks, I encourage portable code. In this case, that means leaving the #include of algorithm intact, even though we do not know of a compiler that actually explicitly needs that header. --[[User:Util|Util]] 16:15, 11 June 2011 (UTC)

==Rename?==
Might this page be better named as "Straddling checkerboard encryption"? (Rationale: straddling checkerboard may have other meanings and interpretations in isolation). --[[User:Markhobley|Markhobley]] 11:40, 8 June 2011 (UTC)
: We can jump that hurdle when we come to it. No sense in worrying too much about what only ''might'' happen in the future. –[[User:Dkf|Donal Fellows]] 14:29, 8 June 2011 (UTC)

::Ok, I had to examine the task to examine the nature of it contents. The encryption suffix would have saved a few moments. Is there any reason not to rename here? It can be done at the click of a button. --[[User:Markhobley|Markhobley]] 16:15, 8 June 2011 (UTC)
:::Shorter names are nicer/cleaner in lists and links. Proper categorization/tagging should avoid that confusion. Really, what else could it be? "Straddling checkerboard cipher" is probably a better name than the "encryption" one, but that seems to be an unnecessary specification like calling [[Linked list]] "Linked list data structure". --[[User:Mwn3d|Mwn3d]] 16:27, 8 June 2011 (UTC)

==Bug==
Note that in the C++ solution, changing 3,7 to 7,3 results in identical encoding, indicating that rowU,rowV are being used in places that need u,v instead. --[[User:Util|Util]] 15:32, 9 June 2011 (UTC)

: You mentioned this again above, so I'm guessing you changed your mind about it being a bug? Anyway, I made it so that 3,7 and 7,3 gave identical results on purpose. You can achieve the same thing by rearranging the alphabet, and I thought it might make some other parts of the code a little simpler. (I'm not sure it actually did though.) [[User:MagiMaster|MagiMaster]] 18:15, 9 June 2011 (UTC)

::No, I have not changed my mind; I mentioned it again as the rationale for my recommendation for a change to the task.
::The only reason you are getting away with `exchange(u,v) when u>v` is because you are using the same program to encode and decode. If one program changes (7,3) into (3,7) and encodes, and then a correct program (that does not exchange()) tries to decode using (7,3), FAIL.
::Also, since you are building the first[] row in a straight loop, skipping the gaps with "(i != u && i != v)", you should not need to reorder (u,v) at all! [ Which also makes moot the whole `#include algorithm` issue :) ] Using the current C++ code, when I change "rowU = min(u, v); rowV = max(u, v);" to "rowU = u; rowV = v;", the program works correctly for (3,7) and (7,3). --[[User:Util|Util]] 16:52, 11 June 2011 (UTC)
::: Ok. I see what you're saying, but between this and the other section, I can't tell if you're proposing that the task description should be changed so that u should be less than v, or that the C++ code should be changed. [[User:MagiMaster|MagiMaster]] 19:39, 11 June 2011 (UTC)
:::: Thinking about it again, I don't really see the point. Why should we distinguish between reordering columns and reordering rows when both are redundant? [[User:MagiMaster|MagiMaster]] 21:42, 18 June 2011 (UTC)

== Someone else ==

Would someone else want to take over handling this task? I don't seem to be getting much done and I'd rather hand it off than see it stuck in draft. [[User:MagiMaster|MagiMaster]] 00:27, 21 June 2011 (UTC)

: I think you are doing fine.
: On Wed July 6th, I will be able to focus on the task again, and can also take over the process of maturing the draft to full Task status, if you still want to hand over the duty at that time. --[[User:Util|Util]] 14:46, 24 June 2011 (UTC)
:: Yeah, that'd be fine with me. [[User:MagiMaster|MagiMaster]] 20:52, 24 June 2011 (UTC)
