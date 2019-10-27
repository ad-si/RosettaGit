+++
title = "Talk:Vigenère cipher/Cryptanalysis"
description = ""
date = 2011-07-29T19:03:31Z
aliases = []
[extra]
id = 9838
[taxonomies]
categories = []
tags = []
+++

I'm not completely sure how algorithmically feasible this task is, but it at least sounds interesting. (Well, I'm not sure how feasible it is to do this completely automatically.) I'll try to add an example soon. [[User:MagiMaster|MagiMaster]] 05:37, 31 May 2011 (UTC)

: C++ code added, but I'm not sure about how robust it is. There seems like a decent chance that the first part will pass a too-short key, and the second part doesn't have any checks on the final output. Another chi-squared check doesn't work, which also makes me suspect that the chi-squared check in the first part isn't the best solution either, even if it does ok. [[User:MagiMaster|MagiMaster]] 11:28, 31 May 2011 (UTC)

: There. I changed the chi-squared test to something that maximizes the correlation instead. Smaller pieces increase the correlation artificially, so I added a weight against them. It seems to work on nearly all the ciphertext I've tried. Long keys are more likely to cause errors, but it still gets the length and most of the characters. A second pass to try and correct any errors by looking for words or common trigrams might not be a bad idea, but I don't think I'll do that myself. [[User:MagiMaster|MagiMaster]] 20:26, 31 May 2011 (UTC)

== Task specification ==
I think that there should be a requirement to provide some output.  Specifically each solution should output the key length determined, the discovered key, and some of the decrypted text (say a line).  --[[User:Dgamey|Dgamey]] 14:14, 3 June 2011 (UTC)

: Well, outputting the key gives the length of the key too, but yeah I could add some details to the output specification. [[User:MagiMaster|MagiMaster]] 04:48, 4 June 2011 (UTC)

== Difficulty and suggested improvements ==

This is a huge amount of text which makes the task much easier.  As a hobby I used to solve these by hand and we would typically get cipher texts of 15-20 5 letter groups.  This cipher text has 165 groups!

I suggest to make it more interesting, that we try our algorithms on successively shorter texts (halving the text each time) and see how low we can drive it.--[[User:Dgamey|Dgamey]] 12:28, 3 June 2011 (UTC)

: Well, I put more text since this is supposed to be fully autonomous. I think if you can look at the output, look for word pieces, and use that to improve your guess, you wouldn't need as much text. That said, I did test the algorithm I posted on shorter inputs and it did fine. It was only when the key was fairly long compared to the text that it started to make mistakes. Also, the text I gave is a specific chunk of a certain work. I didn't put the output of my program on the page in case anyone wanted to view this as a puzzle. (Adding a second, shorter piece of ciphertext would be fine though.) [[User:MagiMaster|MagiMaster]] 04:49, 4 June 2011 (UTC)

== Fully autonomous ==

When trying to generate a fully autonomous version, my program generated a wrong (but "almost right") key, E.g., if ABCDE is the real key, the ciphertext decryption under ABxDEABCDEABCDEyBCDz may have a "better" distribution of letters then decryption under the correct key ABCDE. Perhaps, the length of the key should be provided as an additional input.

: No, the key length is one of the critical factors in deciphering a message.  If ABCDE is a correct key, then ABCDEABCDE is of course also a correct key; but statictical fluctuaion may make a slight variation of the longer key appear "more correct" because it has a chance of giving higher (but incidental) correlations.  This is an inevitable artifact, and it is normal. --[[User:Ledrug|Ledrug]] 17:33, 21 June 2011 (UTC)

== Faster? ==

I'm just curious how the C++ changes that were made make it faster? [[User:MagiMaster|MagiMaster]] 02:22, 21 June 2011 (UTC)
: *shrug* I can't tell.  C++ people are optimization freaks. But on a side note, g++ gives quite a few warnings about ">>" at the end of nested templates (it's ok here, but can be confused with ">>" operator at times), maybe someone should take a look at it. --[[User:Ledrug|Ledrug]] 02:30, 21 June 2011 (UTC)
:: The >> for templates is a C++0x fix. Before that, it was a syntax error, but I guess g++ does it half way even without the -std flag. [[User:MagiMaster|MagiMaster]] 09:55, 21 June 2011 (UTC)
:::I'm pretty sure the easy fix for ">>" after templates is to put a space between the characters ("> >"). It's been a long time since I did anything in C++ though. --[[User:Mwn3d|Mwn3d]] 19:59, 21 June 2011 (UTC)
:::: Yeah, that was how you are supposed to do it in the current version of C++. C++0x made it legal to leave out the space. [[User:MagiMaster|MagiMaster]] 05:00, 22 June 2011 (UTC)
:In this program frequency() is a critical function. The length of the vector of pairs 'result' is known at compile time. Avoiding this push_back speeds up this program more than 20%. This optimization doesn't make this program longer or more complex.
:: It's a good optimization, but a better one might be to avoid allocation of the vector on every function call. Making 'results' function static, passed in by reference, or even file static would all be reasonable options. There's no threading in this program, so thread safety isn't a concern. 'results' is completely recalculated on each call, so there's no need to clear out its contents between calls, either. Finally, it's probably also perfectly appropriate for it to be a pair<char,double> array, rather than a vector, though that's less significant as an optimization as long as per-call reallocation is avoided. --[[User:Short Circuit|Michael Mol]] 18:15, 21 June 2011 (UTC)
::: I should have read the code more thoroughly. I didn't notice that the entire vector was being returned out. Pass-by-reference would be useful, there. The data is used by correlation() with the sort() algorithm, so a simple array is inappropriate. std::vector is the better option. However, the data is only used in frequency() and correlation(), so a class-instance std::vector as a buffer still strikes me as a good optimization. --[[User:Short Circuit|Michael Mol]] 18:22, 21 June 2011 (UTC)
:::: Ok. That makes sense, but what about all the other changes made? [[User:MagiMaster|MagiMaster]] 19:45, 21 June 2011 (UTC)
::::: It looks like the editor changed instances of 'out' to 'result' to avoid sharing symbol names with VigenereAnalyser::out. That's a reasonable change for defensive coding. I don't see the value of changing it from 'class' to 'struct'...the only consequence I know of is that 'struct' has all members default to 'public', instead of 'private', and I don't see a need for any of the class's members to be public except the constructor and analyze(). There were a lot of changes from using 'int' to using 'size_t' in loops. That's generally sensible for any time one is calling into array indexes; size_t will tend to be the systems largest unsigned integer type. He split one common across two lines, so I presume he was cleaning code up for 80-char-wide presentation. That's a common style preference. He removed VigenereAnalyser::key as a member variable, and made it a function-scope stack variable, which is reasonable enough; it helps make it clear that the variable is only for use in that function, and I suppose the allocation cost for a function called once in the class's lifetime is not a significant concern. (It looks like he generally reduced the scope lifetimes of the variables used in analyze(). See corr, for example; it was moved to being declared inside the code block it was used in. That's o...k, as long as the variables are simple and their allocation/deallocation expense isn't going to be spent comparatively often. If it were a more complicated type, I'd have kept it outside that for(..) loop. It comes down to the individual coder's perception of the performance tradeoff vs code readability.)  Replacing '> >' with '>>' was unnecessary, but probably intended as part of showcasing differences between C++0x and C++03. gcc throws warnings on the form, though Microsoft's compiler hasn't complained at me for using '>>' as far back as VS2005. I'd venture a guess he was actively cleaning up the code in preparation for some kind of presentation. --[[User:Short Circuit|Michael Mol]] 20:42, 22 June 2011 (UTC)
::::: I went to do some changes of my own yesterday, but was stymied by my not having a C++0x compiler handy enough to stick with it; codepad.org hasn't upgraded yet. I'd make the further change of using std::vector for frequency()'s buffer, but making it a class member variable. I'd have frequency() take a reference as an argument, and pass a reference as its return value. And there were some other dependent changes, but that completely avoids the reallocation issue; frequency()'s buffer gets re-written from scratch each call through, so there's no need to clear and reinitialize separately. Just pass double(26) into its constructor as part of the class's constructor. --[[User:Short Circuit|Michael Mol]] 20:42, 22 June 2011 (UTC)
:::::: Alright. That's a reasonable explanation. (Except for class to struct, which just bugs me.) I think the C++0x features used here are somewhat minor at the moment, but as I mentioned before, the array class might be appropriate. I can try and make the suggested changes. [[User:MagiMaster|MagiMaster]] 21:11, 22 June 2011 (UTC)
:::::: Ok. I made frequency() use a member variable, and changed several vectors to arrays. I also added a typedef since array<pair<char, double>, 26> is kind of long and is used several times. (That also happened to remove most instances of ">>".) The targets and sortedTargets could also be of the FreqArray type for consistency, but they wouldn't be using the extra char info, so I left them as double arrays for simplicity. [[User:MagiMaster|MagiMaster]] 21:35, 22 June 2011 (UTC)
:::: Actually, using the C++0x array class might be appropriate here. It's size is a compile-time constant, but it can still be sorted and everything. (Actually, I'm not sure, but I think you can sort a normal array too, but the array class is safer.) [[User:MagiMaster|MagiMaster]] 19:52, 21 June 2011 (UTC)

== Promote to full task? ==

Anyone have any reasons to not promote this? [[User:MagiMaster|MagiMaster]] 14:15, 20 July 2011 (UTC)
: Sounds reasonable to me, now that there are a number of solutions. We probably ought to start putting our solutions in as well so that newcomers to the problem can check their answers, and it's also reasonable to encourage people to use code from [[Vigenère cipher]] for the final decrypt once the key is derived; no point in duplicating needlessly. –[[User:Dkf|Donal Fellows]] 10:42, 29 July 2011 (UTC)
:: Isn't the code from the [[Vigenère cipher]] task a little short to be decrypted autonomously? (I haven't actually tried, so I'm not sure.) Also, I think the answer for the code given should be obviously right, but I'd like to hear other's opinions on this. I didn't want to include the answer so that people could treat it as a puzzle if they wanted. (Would putting the answer on a separate page be ok?) [[User:MagiMaster|MagiMaster]] 18:54, 29 July 2011 (UTC)
::: "Using code from" probably only means doing the output for this task once the key is determined.  I'd prefer leaving the output option open to the implementions, for example in the C code I only listed most probable keys found, but didn't do any actual decoding because only a human can have a final say in the correctness of key and deciphered text. --[[User:Ledrug|Ledrug]] 19:03, 29 July 2011 (UTC)
