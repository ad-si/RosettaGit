+++
title = "Talk:Bitwise IO"
description = ""
date = 2012-11-02T20:23:49Z
aliases = []
[extra]
id = 3267
[taxonomies]
categories = []
tags = []
+++

==Real intention==
The real intention of this task and code was to have a bunch of functions to test the LZW compressor / decompressor on a binary real output (instead of ''array'' or similar output); this way compression ratio statistics for the LZW of the task [[LZW compression]] can be done. --[[User:ShinTakezou|ShinTakezou]] 16:57, 19 December 2008 (UTC)

: Hmm, strictly speaking it is not bit-oriented. It is rather a bit-stream interface to a byte-oriented I/O. Exactly because it is not bit I/O (as for instance a serial I/O is) you have an endianness issue here. So in your task you should specify whether it is big or little endian encoding of bits into bytes. Your code looks like big endian. Was it your intention? --[[User:Dmitry-kazakov|Dmitry-kazakov]] 18:28, 19 December 2008 (UTC)

:: It is bit-oriented, as common ''software'' (not my intention to drive serial hardware this way!) I/O can be, i.e. you think you are writing one or more bits, indeed they are sent only grouped by eight, since we can only write bytes (and no more of one byte per time, or we have surely endianness issues). For the endianness while computing, it could be a point, but here I can't see how, since endianness issues are related to how bytes are stored into memory. Let us take a 0xFF packed into a 16 bit word. It will be written into memory, on little endian arch, as 0xFF 0x00. But when you ''take'' it, you can consider it ''logically'' (I love big endian!) as 0x00FF and you can be sure that if you perform a left shift, you will obtain 0x01FE... If you write it into memory, you have again 0xFE 0x01 on LE arch. '''But''', if you shift left 9 time, you will obtain 0xFE00 with a carry (or whatever the status flag for a bit ''slipped'' away from ''left'' is called). Again, iff you write it into memory, and pretend to read it as ''sequencial'' bytes instead of word, you get an issue. Again, into memory it is 0x00 0xFE for LE arch. Luckly even LE processor handle data into registers in a more logical way!
:: You can object that a variable such <tt>d</tt> is stored into memory somewhere, so it is LE encoded. But all operations in a processor are rather endianness-logical (!)! So when I left-shift a 0x00FF that is stored into memory as 0xFF 0x00, the processor first load the datum, so that now you can think of it as 0x00FF, then perform a left shift, obtaining e.g. 0xFF00, then storing into memory, as 0x00 0xFF. If I'd read from memory byte by byte, loading and shifting to ''create'' a datum longer than one byte, I should have considered endianness seriously. But that's also why read and write operation are performed byte by byte rather than accumulating into a 16 or 32 bit word.
:: To say it briefly, I can't see any endianness issues. I am not interested how the processor stores the unsigned int I use to take the bits from; the fact is that when I do a datum << (32-4) for a 4 bit datum of 0xF, what I obtain is (luckly) 0xF0000000 (stored as 0x00 0x00 0x00 0xF0). When I shift again 1 bit left, I expect to have 0xE0000000, not 0xE0010000 (stored as 0x00 0x00 0x01 0xE0). I am telling it harder than it is. It's late for me... hopely tomorrow I will find better words. --[[User:ShinTakezou|ShinTakezou]] 01:07, 20 December 2008 (UTC)

:::Endianness is an issue here because you have to specify how a tuple of N bits is to be packed into an integral unit called byte, since your I/O deals with these units, not with bits. Note, this has nothing to do with the endianness of the machine. If N=8, then, to illustrate the case, let B<sub>i</sub>=(1,0,0,0,1,0,0,1), then B is 137<sub>10</sub> using big endian and 145<sub>10</sub> using little endian encoding, and there are N! variants of packing in total. The code you provided looks like big endian. If you don't like the term ''endian'', no problem. You can simply provide a formula, like byte = &Sigma; B<sub>i</sub>*2<sup>i-1</sup> (=little endian) or &Sigma; B<sub>i</sub>*2<sup>8-i</sup> (=big endian).

::::As far as I know, packing into bytes is as expected in common mathematics (that is, if we want to talk about endianness, big endian): less significative bits are on the right, most significative bits are on the left, so that being A<sub>i</sub> with i &#x2208; [0,7] the binary digits, A<sub>0</sub> is the ''unity'' bit, which is so to say A<sub>0</sub>&sdot;2<sup>0</sup>. So having the binary number 10010001, this is ''packed'' into a byte as 10010001 simply. When we are interested in less than 8 bits, they should be ''counted'' (always so to say) from right to left; e.g. if I want to write the 4 bits 1001, these must be right-aligned into the ''variable'' holding the bits. But these are conventions the user of the functions must know, they are not mandatory to do the way I did. I've implemented all the stuff so that you must always right-align the bits, then the functions will shift to left so that the intended most significative bit of the ''bits datum'' becomes the leftmost bit in the ''container'' (unsigned int in the C implementation). It is enough the user of the functions gets knowledge about how your functions extract bits (in which order) from the data they want to pass; then, it is ''intended'' that the first (being it the leftmost or the rightmost according to your convention) must be the first of the output stream. So that it will be the first bit you read when you ask for a single bit from the so output stream. Maybe your misunderstanding comes from the fact that you handle single bits (in your expl, still not looked your code) as unit held by an array. In C it would be like having char bitarray[N], where each ''char'' can be only 0 or 1. Doing this way, you can decide your preferred convention, i.e. if bitarray[0] is the first bit to be output or it is instead bitarray[N-1]. In C this implementation would be hard to use; if I want to write integers less than 16, which can be stored in just 4 bits, it is enough I pass to the function the number, like 12 (binary 1100), and tell the function I want just (the first) 4 bits; otherwise, I (as user of the functions) should split the integer into chars, each rapresenting a bit, pack it into an array in the right ''endianness'', and then pass the array to the function. Maybe this is easier in Ada (I don't know), but it would be harder in C. Consider e.g. the integers output array of the LZW task; in C, it is enough to take one integer per time, and say to the function to output (e.g.) 11 bits of that integer (of course, 11 bits must be enough to hold the value!); you are outputting 11-bits long ''words'' (which are the ''words'' of the LZW dictionary). Hope I've understood well what you've said and expressed well my explanation (too long as usual, sorry). --[[User:ShinTakezou|ShinTakezou]] 21:53, 20 December 2008 (UTC)

Ok, seen the code. In your implementation, you need this specification since you can choose. In the C implementation, or in any other ''lowlevel'' implementation, it is not a need since the choice is one: we must put bits so that the leftmost is the most significative, and rightmost the less significative (which is, so to say, the ''mathematical'' convention). How these ''significative'' bits are aligned into the container, is a choice of the implementation. The only important thing is that if I wanted to output the 4 bits 1100, then the only byte of output must be 0xC0 (i.e. 1100'''0000''' where bold bits are just for padding). In this way, when reading, the first bit will be 1, the second 1, the third 0 and the fourth 0. If we put one bit after another according to the reading order, we obtain 1100 (and we must obtain 1100 also if we read the bits in just one shot). This is the way ''bit oriented'' is meant. --[[User:ShinTakezou|ShinTakezou]] 22:07, 20 December 2008 (UTC)

<blockquote>
# The thing you mean is the binary positional numeral system, which provides a notation of natural numbers. An unrelated issue, also.
# Your task, speaking mathematically, is about [bijective] mapping of ordered sets of bits onto ordered sets of numbers from the range 0..2<sup>N-1</sup>.
# I don't want to comment on [[C]], but an argumentation that C does something hard cannot serve as an argument.
# Mathematically there is no "first" and "last" bits of numbers. Number is an integral entity. There are '''encoding''' and representations of numbers in the memory of a computer. Only a representation of the number may have first bits, provided that memory bits are ordered in some way. Which BTW is not the case on most modern architectures, and never was the case for files. So it does not make any sense anyway.
--[[User:Dmitry-kazakov|Dmitry-kazakov]] 09:08, 21 December 2008 (UTC)
</blockquote>

Interesting. I am planning to rewrite the task text, maybe in a more formal way (and hopefully clearer), but now I am more busy on LZW and Xmas stuffs. I don't think the following clarify the task, but it is worth noting (I believe so).
# All the task can be rewritten shortly: provide functions to write integer numbers packed into ''n'' bits (binary digits), and read integer numbers packed into ''n'' bits. Consider a very long binary number, composed by N bits. Bits of this huge number can be grouped in M integral number n<sub>i</sub>; the grouping is done taking L<sub>1</sub> bits for n<sub>1</sub>, L<sub>2</sub> bits for n<sub>i</sub> and so on. This suggests also the reading process (which is clarified in the next sentece, since we must know the ''convention'' of the grouping). About writing: consider M integral numbers n<sub>i</sub>; the number N will be n<sub>1</sub> + n<sub>2</sub> &sdot; 2<sup>L<sub>1</sub></sup> + n<sub>3</sub> &sdot; 2<sup>L<sub>1</sub> + L<sub>2</sub></sup> + ... which can be written as N = &sum;<sub>i</sub> n<sub>i</sub> &sdot; 2<sup>&sum;<sub>j&lt;i</sub>L<sub>j</sub></sup>, ''i'' from 1 to M, ''j'' from 0 to i-1, and with L<sub>0</sub> = 0. This also should clarify the reading (but to me it makes no clearer the task for everyone). (In this mathematicsish, n<sub>M</sub> is the first number the user requested to output; we could say it is the last, as natural; but this way reading from a stream would mean to have the whole stream &mdash;the huge number&mdash; into memory, instead of splitted into bytes)
# Yes I suppose you can see it that way. It is not important how you define the thing, the important is that the result is as expected. If I want to output the number 1100 in 4 bits, I must obtain the byte 1100'''0000''', bold for padding. If I want to output the number 1001 in 9 bits, I must obtain 00000100 1'''0000000''', bold for padding. If I want to output the number 1100, 4 bits, "followed" by the number 1001 in 9 bits, I must obtain 11000000 01001'''000'''. You can tell the user of your functions that to write the number 1100 (and obtain as output 1100'''0000''') s/he must pack the bits into an array so that <nowiki>A[0] = 1, A[1] = 1, A[2] = 0, A[3] = 0</nowiki>, or <nowiki>A[0] = 0, A[1] = 0, A[2] = 1, A[3] = 1</nowiki> or in any other of the N! variants. Why the output must be that way? Conventionally, I want that if I read a bit per time and build the "number" by shifting leftward already stored bits, I obtain the intended "number". While writing 1100, I want that shifting leftward in the requested size (4 bits), the first bit which goes out is the first bit of output, the second is the second and so on. So let us have an accumulator of infinite size (not important) and the huge number of N bits, the MSB being 1100.... Put the "accumulator" A in front of the huge number H, like A|H (you can consider the whole as the same huge number H, since at the beginning the accumulator is void, so that it is just as writing a lot of zeros in front of the H, like ...00000H, where H are N binary digits). Now, leftshift the "number" A|H (ie multiply it by 2), you obtain 1|100.... The accumulator holds (infinite sequence of 0s)1. It is as if you have read into the accumulator one bit. Leftshift again. Now, we have 11|00....., you can leftshift two more times, obtaining 1100|.... Now the accumulator holds the number 1100, since we have read the same number of bits we wrote (of course, this must be known!). If we want a "new" number, we clear the accumulator and continue the process. Imagining it this way should also make clearer why I called this bit oriented reading (writing is not so different). When building the tape, I must provide a way I give the bits I want to write. Here, A and H are swapped: the accumulator A holds exactly the bits I want to write, H is a growing number, at the beginning just 0 (i.e. an infinite sequence of zeros), but of course in any point of the process it just hold a sequence of bits. I put in the accumulator (shrunk to 4 bits) the number 1100, having H|1100 (it would have been 01100 if I wanted to write the same number, but packed into 5 bits). Leftshifting gives H1|100'''0''', then H11|00'''00''', then H110|0'''000''', then H1100|'''0000'''. At this point I have "appended" the 4bits binary number 1100 to H, building a bigger number. This all suggests an implementation, but I can't say it is mandatory to implement it this way. I just want the following: if the huge number is 1100H, and I ask for 4 bits, I must obtain 1100 (as if I say, in the natural language, to take the first four bits, write it apart, and then delete them, e.g. <del>1100</del>H). If the number is H, and I say I want to write the number 1100, I must obtain H1100. In base 10 seeing it like a turn-game: player 1 write a number (he can write also leading 0 digits), e.g. 06744. Player 2 writes another number, just after the previous; he wants to write 998, so that on the paper now we can read 06744998. Now back: player 1 must extracts digits in order to obtain back its number; he extracts 06744 and to signal how many digits he took, delete them: <del>06744</del>; player 2 must do the same, ignoring of course deleted digits; he takes 998. If we have N players, we obtain a big "unique" number composed by N numbers. Of course, each player, in order to get back its number, must remember rightly how many digits it had, and must hope the player coming before remembered its number of digits too. E.g., if the player 1 deletes just 0674, even though player 2 remembered he wrote 3 digits, he obtains 499, which is wrong... Hope this stream of consciousness helps you to help me to understand how to write the task clearly and (oh yes!) briefly! Rather than as maths, I would say it in a manipulation-of-bits-data way.
# Can't understand the sentence. HL languages sometimes make it harder to make things that in assembly would be a lot easier and "direct". C is HL, but not so HL. The sentence "Maybe this is easier in Ada (I don't know), but it would be '''harder''' in C" (did you refer to this "hard"?), refer to the fact that it would be hard (difficult) to implement in C the task the way you implemented in Ada (i.e. using array to hold the bits): it would make the code not so usable (for LZW, e.g.); the best way (at least, in C) is to manipulate bits with shifts, ands and ors (nearest to operations a processor can do). Not all implementation will follow this way; I wonder how the code can be used to implement a real-world LZW compressor based on the code given in [[LZW compression]].
# Mathematically, there's a "first" bit and a "last" bit. Mathematicians can feel disgust for the expression, but once one learn about numerical positional systems, it is easily understandable what one can mean by saying first digit and last digit of a number. Take it as a short form for "the least significative digit" (LSB for binary) and "the most significative non-zero digit" (but in computer world where we are used to consider integers packed into "fixed size" container, so this can be simply "the most significative digit", MSB for binary). My usage of ''first'' is clear by the context (or so I believed), and sometimes refers to the order of intended output rather than mathematical stuffs, so that the first bit of a stream is the one we wrote first, and the last the one we wrote last, e.g. F1100.....0101L. Users of the functions must know if the first bit of output will be the rightmost (or the bit held into the first/last element of the array) or the leftmost (or the bit held into the last/first element of the array) of the input "bit string", so they can ''accomodate'' the bits to obtain the output they wanted. Rappresentations of numbers are numbers, unless you want to consider physical processes, since bits are "rappresentations" of physical processes into the hardware; but noone is interested in these details while programming. I suppose "representation" is what I've called "packing" (packaging?). We pack numbers into fixed finite size container since for a computer numbers must be "real", not abstract entities. The concept of LSB (least significative bit) is ok for abstract entities too. The MSB no, we must use the "most significative non-zero bit" form. A file has an ordering too. We can imagine a file as a sequences of bytes, so there's a first byte and a last byte. Since each byte "contains" bits, if we look at this bitstream, there's again a first bit and a last bit in the sequence, as said before. If we consider it like a whole huge number with N digits (the number 00030 has 5 digits, even if we can write as 30, and it's the same number but with 2 digits only), as I explained before, then the first being the LSB, would be the "last" in the precious speech. (But I believed it was clear the meaning of my usage). Noone is interested in the physical ordering of the bits into the memory; likely they are rather scattered; but these are uninteresting details we can disregard while programming: we can consider all bits as if they had an ordering. The same for file: we are not interested in how the data are organized on disk; it is enough we can get them in the order we expect!

I will try to rewrite the task taking into account these misunderstandings, but not before I make the LZW working (which by the way is also a test-bed for the bits_read/write functions:D!), understand how to split into smaller tasks, and iff this Xmas won't kill me. --[[User:ShinTakezou|ShinTakezou]] 00:19, 22 December 2008 (UTC)

:4. Ooch, no, in mathematics numbers have neither bits nor digits. For that matter, in [http://en.wikipedia.org/wiki/Zermelo%E2%80%93Fraenkel_set_theory Zermelo–Fraenkel set theory] numbers are first introduced as sets {} (empty set is 0), {{}} (set with 0 inside is 1), {{{}}} (set with 1 inside is 2). No any bits in sight. Nor it means that numbers have first and last brackets! Digit, bit, LSB, MSB, exponent, bracket etc are entities of representations. There are countless ways to representation numbers. Representations themselves are not numbers. Representation R is in a mapping from some set S to N (the set of numbers): R:S->N. When S is the set of English words, then "two" is a representation of the number {{{}}}.  When S is the set of binary numerals then 10<sub>2</sub> is a representation of the same number {{{}}}. Division between "physical" and what? is below my radar, because you could not define "physical" anyway. About files. A byte-oriented file has bytes, these bytes contain themselves. They don't contain bits, characters, images or Shakespear's plays in PDF format. They just do not. It is a ''media layer'' in [http://en.wikipedia.org/wiki/OSI_layer#Layer_2:_Data_Link_Layer OSI] terms. The content (meaning), e.g. bits, characters etc, lies above it in the application that deals with the file. Your '''application''' knows the meaning of bytes, as defined by the task, i.e. to keep sequences of bits in a '''certain''' way. Merry Christmas! --[[User:Dmitry-kazakov|Dmitry-kazakov]] 08:59, 22 December 2008 (UTC)

::I see Shin's point. In the real world, the LZW symbol stream is always packed into octets. The GIF and TIFF standards each specify a packing scheme. However, the method is not as simple as you may realize. As the symbol table is filled, the maximum bit length goes up one bit at specific points (i.e. 511 -> 512), and the packing scheme takes advantage of that. Perhaps Shin could reference one of these standards for the task.
::If Shin's actual goal is to measure the compression achieved by LZW compared to the input stream, that is more easily accomplished.  The output symbols start at 9-bits, so simply multiply output symbols by 9 and divide by 8. For the test string of the task"TOBEORNOTTOBEORTOBEORNOT" (24 bytes), it compresses to 16 symbols which would pack into ((9*16)+7)/8 = 18 bytes. The calculation becomes more complex if there are more than 256 output symbols, because the symbol size increases.  (I implemented an 8086 assembly TIFF LZW codec once upon a time.) --[[User:IanOsgood|IanOsgood]] 17:09, 22 December 2008 (UTC)

:::I think I have to rewrite the text of the task. The approach I've used of course has a direct analog into streaming of bytes. No one would have talked about endianness or fine maths points if I had said "write a stream of bytes", and read them "keeping the streaming order"; said badly, but just an example would have been enough to understand: <tt>printf("hello world")</tt>, or <tt>fwrite("\x45\x20\x50\xAA", 1, 4, out)</tt> ... the two functions printf and fwrite (in C) would have completed the task (fwrite being more general, while printf can't allow a "null" byte). Simply I wanted something similar, but for bits (and it is what my C impl does). One can also implement the whole stuff so that what was leftshift in my previous speech is now a rightshift; it is enough to pick bytes from "the bottom" and walk backward, which means seeking into a file (also if using buffering, and process the bytes from the end of the buffer); it will be inefficient if one would like to use the technics on a pipe, since in order to start the decompression, one must wait for the end of the stream (that could mean: buffering the whole stream... memory consuming). I wanted no to have a way of misuring efficiency (as you say, this can be done by exstimation); I've just implemented, in C, a working LZW compressor: instead of outputting an array, I output bits; added a big endian 32 bit integer to store how many bytes there were in the uncompressed file, and I've a compressor. I needed these bits routines to achieve my aim. The LZW C compression code for now can't fit into RC (I've just written one in Obj-C, which has a dictionary object..., the way the others did, ie. outputting an array), since I had to implement a Dictionary and a general way to handle strings of arbitrary bytes, so that I was able to translate the Java code. 

4. You are dealing with words of a language, which always have someway elastic meaning and usage, and it was what I tried to stress. If a number is abstract for a mathematician (what is an abstract object without representation? a definition could be counted as a form of representation?), or if it is defined just as a successor of a successor of a successor ... of 0, e.g. 0SSSS would be a rapresentation of 4, which is a widespread common representation itself, or what, it's not so interesting after all. There's no single human being (mathematicians included) able to use abstract numbers. You can talk about these, but they would be a lot unuseful for a lot of tasks. Bit is the short form for Binary digIT; so let's talk about digits. What is a digits? A matter of definition. I say a matching couple {} is a digit. Here's it comes digits into Zermelo–Fraenkel. You could say {} itself is a representation of something. Yes, indeed every symbols could be considered a representation of something, more or less abstract. So that mathematicians using symbols to talk about abstract numbers, are using a representation of something; so that I can define the word "digit" to match a unit of their rapresentation. And I can also define someway what is first digit and last digit. Philosophically, I can say that a representation of something could be itself; my body is a representation of myself, and I feel also it is myself (My body is really me, sort of citation of a Cohen's song). And it becomes harder if we consider that the way we communicate is done by symbols and representations. If representations are nothing (since they exist Real Thing which are no representations and are not even only representations of themselves), our speechs are void. We exist by reference. Everything exists by reference. (I use ''referencing'' to mean "assigning a representation", or definition). Referencing is what makes the things exist. And it's why I believe there's such a digit in a number. I've just "created" it by referencing it in my speech (I've defined it, more or less explicitly). Back to the earthground, we use representations of numbers which has "concept" of digits, and first and last digit as I've defined before; human languages allow us to say simply that numbers are made of digits. If you write an math article about number theory, you can write as you prefer; but out of that world and article (or out of the world of the article), it makes few sense: for all practical and communicative use, numbers have digits. By the way, I define the less significative digit of ZF as the innermost {}. As you can see, there's no escape, since to express your thoughts (no matter the concepts involved!) you '''need''' using symbols, which always are representations of something!!

Definitions are important, make the things exist, as said before. A byte is an entity that is made of bits (bits are binary digits). By convention, eight. A byte-oriented file has bytes, and bytes being made by bits, contain bits (sometimes I could put "", but now I won't). A byte can be interpreted (interpretation and representation are different, even though one can link them some conventional way) as character or number. We are interested in the number interpretation. Since a byte is made by 8 binary digits, we understand that a byte is just a number rapresented in base 2 with at most 8 digits. So, it is a number from 0 to 255, no more. We learned about numerical system, and so we know what is a less significative digit (and if not, I can reference my previous talk and it is enough to continue speaking about LS digits). When I must write my age, I write 30, no 03. The same apply to a number written in base 2. It has a well know order. Conventional, but of that kind of common widespread convention that one can't ignore, since it's one of the basis of the communication; one can change the convention, but then can't be surprised a lot of persons misunderstand what s/he says. So, we have data called bytes that we can interpret as binary numbers, and this interpretation is suitable for many tasks, and there are simple way we can link between different interpretations. All, maybe by chance maybe not, is related.
And after all, it is all related to how the hardware is built and to the physical processes involved in keeping/retrieving a "''quantum''" of information into a thing we call simply "memory". Of course you are right, the meaning is in the interpretation. And haven't I given you a lot of different (but correlated) interpretations of the task? I've talk about a huge number, ''represented'' in base 2, and how to "manipulate" it in order to obtain smaller base 2 numbers, and how these are related to the inputs of the functions of the task. '''But''', the fact that byte in a computer a made of bits, is a little bit (!) below the level of an application. It is just something an application "must" know before, it is not it that decided how bits are packed into a byte, nor the order of them that makes it meaningful to say a+b=c i.e. and take a human representation that (by chance!) matchs what we know about sum of binary numbers (with special conventions for the computer world). Applications just use those conventions. They indeed can change it totally... but you need writing a lof of code to do so! Relying on the underlaying conventions, make it all easier. (In fact, try to use any of the other 8! arrangements of bits into a byte and still use your preferred language +, = and so on... I suppose you must write a lot of code to use them the same way as you normally do).

I believed my "mathematical" approach of the "huge {''rapresented in'' base 2} number" was clear; maybe I will rewrite the task using the byte streaming analogy (print "hello world" or print bin"11010100101001010110" should be similar...). Good Xmas and 2009 to all --[[User:ShinTakezou|ShinTakezou]] 02:01, 23 December 2008 (UTC)

: Yes, number is abstract and independent on the representation. This is why we can have different representations of numbers. This is why we can use numbers to describe so many different things. It is important to differentiate abstractions and representations, otherwise we would still be counting using fingers. One finger is not one.

: Digit is not a number, it is a symbol. [http://en.wikipedia.org/wiki/Numerical_digit See].

: Byte has many meanings. When byte is a minimal addressable/transportable storage unit, it becomes meaningless (and even dangerous) to talk about bits ordering in it. Sorry for being stubborn, but communication protocols in my bread. Too often people make nasty errors, leading to non-portable code, and sometimes putting someone's life in danger.

: There is no preferred ordering when we deal with conventional computer architectures, network protocols, parallel communication devices etc. Surely, since byte has a finite number of states, you can group states and then order these groups declaring them bits. But there is no preferred way to do it. If bits were addressable, then the order of addresses would be such a preferred way. In information systems there also are no bits in bytes, because bytes are used merely as a media layer.

: This is exactly your case. You have a sequence of bits, which you want to store as a sequence of bytes. Certainly you need to define how these bits define the bytes of in the sequence.

: But you don't need all this philosophy in order to unambiguously specify the task... (:-)) Merry Christmas! --[[User:Dmitry-kazakov|Dmitry-kazakov]] 09:51, 23 December 2008 (UTC)


==After task rewriting; but still on the task sense dialog==

I still can't get the point and how it is (was) related to the way the task is (was) specified. But '''to me''' it sounds a little bit confusing what you are saying. In common computerish world, a byte is a minimal addressable storage unit; nonetheless, it is '''meaningful''' to talk about bits ordering in it, and in fact many processors allow to handle single bits into a byte (or bigger "units"), once it is loaded into a processor register. E.g. motorola 680x0 family has bset, bclr and btst to set, clear or test a single bit into a register (and by convention the bit "labelled" as "0" is the LSB; it is not my way of saying it, it was the one of the ''engineers'' who wrote the manual &mdash;shipped from Motorola&mdash; where I've read about 680x0 instructions set). The same applies if you want to use logical bitwise operations that all the processors I know have; for instance ''and'', ''or'', ''exclusive or'', ''not''. To keep all the stuff coherent, you must "suppose" (define an already defined) bit ordering.

A bit ordering is always defined, or a lot of things would be meaningless when computers are used. The following is an excerpt from RFC 793.


```txt

TCP Header Format
    0                   1                   2                   3   
    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |          Source Port          |       Destination Port        |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                        Sequence Number                        |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                    Acknowledgment Number                      |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |  Data |           |U|A|P|R|S|F|                               |
   | Offset| Reserved  |R|C|S|S|Y|I|            Window             |
   |       |           |G|K|H|T|N|N|                               |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |           Checksum            |         Urgent Pointer        |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                    Options                    |    Padding    |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                             data                              |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

```


Numbers on the top define a bit ordering of 32 bits (4 bytes). In order to give "universal" (shareable and portable) meaning to "numbers" that span more than a single byte, the big endian convention is used, and this must be stated, and in fact it is, somewhere (but again, I would refer to the big-endian convention as the "natural" one, since we are used to write numbers from left to right, from the most significant digit to the less significant digit &mdash;the unity). In order to check, get or set the flags URG, ACK, PSH, RST, SYN and FIN the bit ordering is essential; You may need to translate from a convention to another (e.g. if I would have loaded the 16 bit of the Source Port into a register of 680x0, which is a big endian processor, I would had the right "number" of the Source Port, since endianness matchs, but to refer to bit here labelled as 15, I should use 0 instead). Despite the bit ordering here given, I can access URG flags by an ''and'' mask, which I can build without considering that labelling: the mask would be simply 0000 0000 0010 0000 0000 0000 0000 0000, which a can write shortly as hex number 00200000, i.e. simply hex 200000, which I could also express (not conveniently but keeping the "meaning") as 2097152 (base 10). Big endianness apart (since the data are stored somewhere by computers that can have different conventions), no more specification is needed to build the mask.

The preferred ordering is the one in use on a particular system, and '''could''' be different. But it is not: since bytes are handled atomically, bit ordering into a byte, for all the ways common people are able to access them (i.e. using machine code or maybe assembly), is always the same. So that I can write the byte 01000001 (in hex 41), and be sure that it will be always the same, from "point" to "point" (i.e. transmission in the middle can change the thing, but some layer, at hw level or/and sw level, must "adjust" everything so that an "application" can read 0x41, and interpret it, e.g. as an ASCII "A").

Another example where nobody felt necessary to specify more than what it is obvious by exposing the matter itself, could be the MIPS instructions set you can read at [http://www.mrc.uidaho.edu/mrc/people/jff/digital/MIPSir.html MIPS reference]. In order to program an assembler for that MIPS, you don't need more information than the one that is already given in the page. Using "mine" implementation of ''bit'' ''oriented'' stream read/write functions, I could code the Add instruction in the following way:


```txt

bits_write(0, 6, stdout);
bits_write(source, 5, stdout);
bits_write(target, 5, stdout);
bits_write(destreg, 5, stdout);
bits_write(0x20, 11, stdout);

```


Where source, target and destreg are integers (only the first 5 bits are taken, so that the integer range is from 0 to 31). One could do it with ''and''s, ''or''s and ''shift''s, but then when writing the final 32 bit integer into memory, s/he must pay attention to endiannes, so again a bunch of and+shift could be used to "select" single bytes of the 32 bit integer and write the integer into memory in an endiannes-free way (a way which works on every processor, despite of its endianness). (Here I am also supposing that the processor in use is able to handle 32bit integers, i.e. is a 32 bit processor).

As the MIPS reference given above, I don't need to define how the bits define the bytes in the sequence. It is straightforward and "natural", as the MIPS bit encoding is. If I want to write the bit sequence 101010010101001001010101010, I've just to group it 8 by 8:


```txt

1010 1001 0101 0010 0101 0101 010
\_______/ \_______/ \_______/ \____

```


and "pad" the last bit "creating" fake 0s until we have a number of bits multiple of 8:


```txt

1010 1001 0101 0010 0101 0101 010X XXXX
\_______/ \_______/ \_______/ \_______/

```


So, the output bytes will be (in hex): A9 52 55 40. Also, to write that sequence as a whole, I must write <tt>bits_write(0x54A92AA, 27, stdout)</tt>, which could be a little bit confusing, but it is not, since if you write the hex number in base 2 you find exactly the sequence I wanted to write, right aligned (and only this one is a convention I decided in my code, and that could be different for different implementations, but this is also the most logical convention not to create code depending on the maximum size of a register: if the left-aligning would be left to the user of the function, he should code always it so that it takes into account the size of an "int" in that arch; in my implementation, this "count" is left to the functions, which in fact left align the bits taking into account the "real" size of the container &mdash;a #define also should allow to use the code on archs that have bytes made of a different number of bits, e.g. 9)

Another way of writing the same sequence, and obtain the same bytes as output, could be:

```txt

bits_write(1, 1, stdout); bits_write(0, 1, stdout);
bits_write(1, 1, stdout); bits_write(0, 1, stdout);

```


And so on. If the task is not well explained in these details, examples should clarify it. But maybe I am wrong. --[[User:ShinTakezou|ShinTakezou]] 01:14, 28 December 2008 (UTC)

: My point is about [http://en.wikipedia.org/wiki/Bit_numbering bit-endianness], it must be specified. You did it by providing an example in the task description. Note that the talk about writing strings into (binary?) files is superfluous and, worse, it is misleading, for exactly same reasons, BTW. Text is '''encoded''' when written as bytes (or any other storage unit). It is sloppiness of [[C]] language, which lets you mix these issues. If the file were UCS-32 encoded your text output would be rubbish.

: TCP header defines bits because it contains fields shorter than one byte, and because the physical layer is bit-oriented, which is '''not''' the case for byte-oriented files.

: If MIPS architecture has preferred bit-endianness, why should that wonder anybody? --[[User:Dmitry-kazakov|Dmitry-kazakov]] 10:32, 28 December 2008 (UTC)

The task specifies we are handling ASCII (encoded) strings. Hopely this is enough to avoid loosing information, that it would happen with any other encoding that uses "full" byte. The bit-endianness is just a labelling problem. Even in the wikipage you linked, no matter if the left bit is labelled as 7 or 0, the byte (binary number with at most 8 digit) is still 10010110. That we can read as the "number" 96 in hex (too lazy to get it in decimal now:D); and if I write such a byte "formed" by such bits into a file, I expect that a hexadecimal dump of that file will give me 96 (string) as output. These are details hidden into the code; no matter how you label the bits; the important fact is that when you use the functions to write the bits 10010110 as a "whole", 
you get the byte 96 into the output; and viceversa, when you read first 8 bits from a file having as first byte 96, you must get 10010110 (i.e. 96 :D). And the same if you write an arbitrary sequence, like 100101101100, as a "whole"; when you read back 12 bits, you get back 100101101100 (which is the "integer" 96C in hex)

I still can't get the point of the statement of the second paragraph. When I "code" software at some not too low hw level, I deal with bytes, I can't see the bit-orientation. And it is why the RFC can talk that way letting programmer understand and code in the right way application handling that TCP data, disregarding the physical layer. These things could be an issue when writing lowlevel drivers, dealing with serial communication or whatever... But we are a little ''bit'' higher than this! Files are byte-oriented, and it is the reason why we need to pad with "spurious" bits if the bits sequence we want to write has no a number of bits multiple of 8 (supposing a byte "contains" 8 bit); but if we "expand" the bits of each byte, we have a sequence of bits (and maybe the last bits are padding bits...); this is the "vision" of the task.

It does not wonder; it just hasn't specified a bit-endiannes, that as said before, is a labelling problem; encoding of the addu instruction is


```txt

0000 00ss ssst tttt dddd d000 0010 0001

```


and nobody is telling that the leftmost bit is the 0, or 31. No matter, since encoding of the instruction remain the same, and it is written into memory in the same way. So here indeed we don't know if MIPS prefers to call the leftmost bit 0 or 31.

One could think about what's happening with a little endian processor; to have a feel on it


```txt

0000033A  681000            push word 0x10

```


from a disassembly; we have 68 (binary 01101000) followed by 16bit LE encoded value. If bits into the first instruction byte have meaning, we could say the encoding would be:


```txt

push byte/word/dword + DATA   ->   0110AB00 + DATA

```


(It is a fantasy example, x86 push is not this way!) Where bits AB specifies if we are pushing a byte a word or a dword (32bit); AB=00 push byte, AB=10 push word, AB=11 push dword (while AB=01 could be used to specify another kind of instruction); and somewhere it will be stated that DATA must be LE encoded. But one must remember that once the DATA is got from memory, into the (8/16/32 bits) register there's no endianness; if stored DATA is hex 1000, this is, into the reg, just the 16 bit "integer" hex 1000. To talk about the encoding of push byte/word/dword, I don't need to specify a bit-endianness. I must know it when using intruction that manipulates single bits of a register (a said before, motorola 680x0 label the LSB as 0).


```txt

00000336  50                push ax
00000395  51                push cx
0000038A  52                push dx
0000045F  53                push bx
00000136  55                push bp
00000300  58                pop ax

```


These "pushes"/pop suggest us that we could say the encoding of the push REG instruction is something like


```txt

 0101PRRR       RRR =  000 ax      P = 0 push
                       011 bx          1 pop
                       001 cx
                       010 dx
                       101 bp

```


It happens that x86 instrunctions are not all of the same length, but it should not throw confusion; the way we use to say how x86 instructions are encoded is the same as the one for the MIPS, the 680x0 or whatelse. And despite of the ''preferred'' endianness(!!) if we like to say it in a bit-wise manner:


```txt

push word DATA   ->  0110 1000 LLLL LLLL HHHH HHHH 
   L = bits of the LS byte (Low)
   H = bits of the MS byte (High)

```


And this way, which is sometime used, don't need to specify any "bit-endianness": it is clear how bits of the LS byte LLLL LLLL must be put. E.g. for the "integer" 0010, LS byte is 10 (binary 00010000) and MS byte is 00, so we fill L and H this way:


```txt

LLLL LLLL HHHH HHHH
0001 0000 0000 0000

```


The endiannes which could lead to problems is the endianness regarding bytes for "integers" stored with more than a single byte. At this (not so low) level, bit-endianness is just a labelling issue and matter just when using instructions like 680x0 bset, bclr and so on.

Hopely the task is clear(er) (at least an OCaml programmer seemed to have got it!), and I've learned
«Ada allows specifying a bit order for data type representation» (but underlying implementation will need to map to hardware convention, so it would be faster just to use the "default", I suppose!) --[[User:ShinTakezou|ShinTakezou]] 00:10, 6 January 2009 (UTC)
: Everything in mathematics is just a labeling problem. Mathematics is a process of labeling, no more. As well as computing itself is, by the way. Your following reasoning makes no sense to me. When byte is a container of bits, you cannot name the ordinal number corresponding to the byte '''before''' you label its bits (more precisely define the encoding). The fallacy of your further reasoning is that you use a certain encoding (binary, positional, right to left) without naming it, and then start to argue that there is no any other, that this is natural (so there are others?), that everything else is superfluous etc. In logic A=>A, but proves nothing.
: Here are some examples of encoding in between bits and bytes: [http://en.wikipedia.org/wiki/RADIX-50 4-bit character codes], [http://en.wikipedia.org/wiki/Binary-coded_decimal packed decimal numbers].
: This is an example of a serial bit-oriented protocol [http://en.wikipedia.org/wiki/Controller_Area_Network CAN], note how transmission conflicts are resolved in CAN using the identifier's bits put on the wire. Also note that a CAN controller is responsible to deliver CAN messages to the CPU in the endianness of the later. I.e. it must recode sequences of bits on the wire into 8-bytes data frames + identifiers.
: More about [http://www.linuxjournal.com/article/6788 endianness] --[[User:Dmitry-kazakov|Dmitry-kazakov]] 10:08, 6 January 2009 (UTC)

:: Sorry at this point I think we can understand each others. I believe I've explained in a rather straightforward (even though too long) way the point, and can't do better myself. In my computer experience, the "problem" and the task is understandable, clear and not ambiguous. In a implementation-driven way I can say that you've got it as I intended iff the output of the program feeded with the bytes sequence (bytes written in hex)


```txt

41 42 41 43 55 53

```


:: (which in ASCII can be read as "ABACUS") is


```txt

83 0a 0c 3a b4 c0

```


:: i.e. if you save the output in a file and see it with a hexdumper, you see it; e.g.


```txt

[mauro@beowulf-1w bitio]$ echo -n "ABACUS" |./asciicompress |hexdump -C
00000000  83 0a 0c 3a b4 c0                                 |...:..|
00000006
[mauro@beowulf-1w bitio]$ echo -n "ABACUS" |./asciicompress |./asciidecompress
ABACUS[mauro@beowulf-1w bitio]$

```


::--[[User:ShinTakezou|ShinTakezou]] 18:10, 13 January 2009 (UTC)

== PL/I bitstring longer than REXX'... ==

because the input seems to be STRINGS followed by '0D0A00'x 
[[User:Walterpachl|Walterpachl]] 20:22, 2 November 2012 (UTC)
