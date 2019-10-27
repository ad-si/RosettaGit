+++
title = "Talk:Find first and last set bit of a long integer"
description = ""
date = 2014-02-01T22:39:32Z
aliases = []
[extra]
id = 11022
[taxonomies]
categories = []
tags = []
+++

== More Clarification "set bit" ==

The example gives 
```txt
INT: find first & last set bit  |LWB|UPB|Bits
                               0| 33| 32|0
```

How are there any set bits in an unsigned integer ZERO?  --[[User:Dgamey|Dgamey]] 03:43, 8 December 2011 (UTC)
: I can't believe I forgot the zero! --[[User:Dgamey|Dgamey]] 03:06, 9 December 2011 (UTC)
: Well, you need to realize that it's an Algol zero, which can be anything you want really. --[[User:Ledrug|Ledrug]] 03:20, 9 December 2011 (UTC)
:: (hrrmmmph ... depends if its a man or boy compiler then does it)  I would expect that 'set bit' should be defined.  If it means a bit that is set to one then the example makes no sense.  If it means set to zero, well then it does.  But someone needs to say. --[[User:Dgamey|Dgamey]] 03:44, 9 December 2011 (UTC)
::: Er sorry, I was just poking fun at Algol.  The "0 with a set bit" was more likely evidence that stuff were not thought through when the task was posted, as can be seen in the whole page of discussion below.  Set bit of course means 1 here, and the task is now changed to number bits from 0 up, where the Algol example currently show integer 0 as having set bit at position -1, which is sensible, so I guess this issue is no more. --[[User:Ledrug|Ledrug]] 03:57, 9 December 2011 (UTC)
:::: Shouldn't it be marked incorrect then? --[[User:Dgamey|Dgamey]] 04:29, 9 December 2011 (UTC)
::::: Which part? The line you quoted above no longer exists, and the current Algol implementation, though inconsistent in my opinion, isn't exactly incorrect (the task description isn't clear on some issues).  What do you have in mind? --[[User:Ledrug|Ledrug]] 04:53, 9 December 2011 (UTC)
:::::: Ah well that changes things then --[[User:Dgamey|Dgamey]] 14:15, 9 December 2011 (UTC)

Can you clarify? For it would be an interesting aside to see a host code sample that is not for [[wp:Two's complement|Two's complement]].  [[User:NevilleDNZ|NevilleDNZ]] 04:39, 8 December 2011 (UTC)

:Hrm, if it's not in two's compliment, what motivates you to find ''bits'' in it to begin with? --[[User:Ledrug|Ledrug]] 04:49, 8 December 2011 (UTC)
Wikipedia has a few non-Two's complement ideas:
* [[wp:Bitboard|Bitboard]] 
** e.g. [http://nanochess.110mb.com/chess3.html#algol nanochess68]
* [[wp:Mask_(computing)#Uses_of_bitmasks|Mask_(computing)#Uses_of_bitmasks]]
* [[wp:Bit_field#Examples|Bit_field#Examples]]
* [[wp:Affinity_mask|Affinity_mask]]
* [[wp:Hash_table#In_programming_languages|Hash_table#In_programming_languages]]
* [[wp:Bitwise_operation#Applications|Bitwise_operation#Applications]]
* [[wp:umask|umask]]
* and probably a couple of others...

Sometimes it is just done for fun:
* [http://graphics.stanford.edu/~seander/bithacks.html#IntegerLog Find the log base 2 of an N-bit integer in O(lg(N)) operations]
[[User:NevilleDNZ|NevilleDNZ]] 05:52, 8 December 2011 (UTC)

I was almost thinking about added a REXX solution for this problem, as REXX doesn't store it's numbers in binary on any computer, but instead, REXX stores them as characters.  So the problem of finding the first and last set bits could be an exercise in finding what bits are set for the various characters (that represent the ten digits) --- and this would make it dependent whether it being ASCII or EBCDIC  (or BCD for that matter) computer.  The REXX program of course, could pretend that the number is stored in binary, and then return the appropiate bits as requested. This would be the equivalent of fantasy baseball. -- [[User:Gerard Schildberger|Gerard Schildberger]] 04:17, 8 May 2012 (UTC)

Except, this task is asking for the first and last set bit of the integer, not of the characters which you use to represent that integer.  But that's just the title, and I agree that the task itself is not very clear about the relevance of the representation.  Integers represented using floating point notation have a similar issue here.  Ultimately, the task is not asking for the bits -- that part is trivial -- what the task is asking for is where these bits would be placed in the integer.  And the task is also foggy about "integers" -- integers can be negative... --[[User:Rdm|Rdm]] 13:01, 8 May 2012 (UTC)

Yes, I read the task's description.  Integers are stored as characters in REXX.  So the bits of the integer are just the bits of the characters that are used to describe (hold) the value of the integer.  Negative integers normally (but not always) start with the character '-' (the minus sign).  Integers can also have any number of leading (and/or trailing and/or imbedded in some cases) blanks, but that's another complication.  Integers also can have a leading plus (+) sign.  It appears to me that most everyone is making the assumption that integers are stored in binary, and that's not necessary true for all languages.  A few tasks seem to be "C"-centric.  It would make more sense (to me) not to assume how integers are stored, but word the task something like: find the first and last set bit of an integer when (or if) stored in a binary form --- and even that can be misinterpreted --- but it would preclude some languages, however.  I would hope that these tasks wouldn't be worded to do such things (preclusions or exclusions).  When using the word LONG, a (huge?) assumption is already being made.  REXX has no such thing as short, long, longlong, etc.  And, as it was said, it's a bit foggy (about integers). The REXX language doesn't have, in the true sense (whatever that means), integers.  It has character strings, and some character strings are numbers.  And some numbers are whole numbers (I guess you could call them integers), and some whole numbers look a lot like integers.  Hold on, this train has a caboose.  A whole number (in REXX) is a number that has no fractional part when expressed within the current NUMERIC DIGITS (that's the size or width, if you will, of the largest "mantissa" of a number --- the default is 9).  So "1.5e5" is a whole number, so would be "7.9999999999999999", and  " +  5 " (I know the latter looks like more of an expression than how an integer would be stored internally). -- [[User:Gerard Schildberger|Gerard Schildberger]] 17:28, 8 May 2012 (UTC)

:: It's not valid to say that "the bits of the integer are just the bits of the characters that are used to describe (hold) the value of the integer".  Proof: Change the leading bit from a 1 to a 0.  The result is not a valid integer.  --[[User:Rdm|Rdm]] 17:52, 8 May 2012 (UTC)

::: But it is valid to say that for the REXX language (that the bits of the integer are just the bits of the characters ...) because that's the way REXX stores "integers" (well, for that matter, the way REXX stores everything).  Just because the leading bit is changed from "1" to a "0" (and thereby changing a leading bit in the character which is representing a numeric digit), doesn't make it a proof that the character that represented the digit wasn't valid in the first place.  If the character (say) was a 7 (if using ASCII),  now it's a '17'x.  If it were on an EBCDIC system, the new character would be a lowercase "w".  Of course, the leading character in the number could've been a blank.   -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:04, 8 May 2012 (UTC)
:::: I think that that's like saying that it's reasonable to pull a bit from a stack frame because the stack frame is being used to represent the number.  --[[User:Rdm|Rdm]] 18:59, 9 May 2012 (UTC)

::::: [I hope I understand your wording, but instead:]  ... ''because the stack frame is being used to'' '''STORE''' ''the number''.


### Clarification

The task is unclear in what number should be computed. It mentions "first" and "last" set bits, but with bits written in most-significant to least-significant order, or the other way around? (i.e. which of "upb" and "lwb" should compute the least significant set bit? and which computes the most significant set bit?) For a 32-bit integer should the least significant bit be numbered 0, 1, 31, or 32? Also, what values should be returned if there are no set bits? And for negative integers, should the most significant set bit be the sign bit?

The ALGOL example seems to indicate that: the least significant bit is numbered 32 (and the most significant bit is numbered 1). This seems the reverse of most people's bit numberings (where the least significant bit should be numbered 0, and the most significant 31), and also wouldn't make sense for arbitrary-precision integers (where would you start counting?). Also, it seems that "lwb" computes the most significant set bit, while "upb" computes the least significant set bit. Is this the example we should follow for other languages? --[[Special:Contributions/208.80.119.67|208.80.119.67]] 23:38, 7 December 2011 (UTC)

: The whole thing doesn't make sense for multi-precision integers.  If you are willing to use language/library bigint implementation, you basically willingly forfeit control over how it's done behind the scenes, it doesn't make sense to "efficiently find a set bit" -- who says the numbers are internally represented by bits to begin with? --[[User:Ledrug|Ledrug]] 00:30, 8 December 2011 (UTC)

'''/* Clarification */''' integers are used in the ''test case'' only to conveniently supply binary specimen for testing.

'''re:''' This seems the reverse of most people's bit numberings.

C also - correct me if I am wrong - prints the most significant bit/octel/hex digit on the left, and the least on the extreme right, hence the operators << and >>. Also python, and most other programming languages display and (pretend to) store right to left.  It's probably a human thing.

: I was referring to the bit numbering, not how they are printed. From what I've seen, the least significant bit is usually numbered "0" and the most significant bit "31". --[[Special:Contributions/208.80.119.67|208.80.119.67]] 02:34, 8 December 2011 (UTC)

'''re:''' And for negative integers

The task is focused on binary/bits.  Whether it is signed or unsigned is not so important.  The '''test case''' requires only +ve.  Beyond that just do the natural thing for the specific language & CPU.

'''re:''' wouldn't make sense for arbitrary-precision integers

Agreed.  It does seem odd having the least significant bit at (algol68g's) 116 for '''long bits'''/'''long int'''s.  Thus this could make Arbitrary Precision ''integers'' particularly weird.  But this task is about bit manipulation.  The '''test case''' happens to use '''long int'''.  Maybe a particular language/CPU puts these in reverse.  In such a case just find the location of first and last non zero bits on this platform.  Try not to worry about what is being represented.  Try not to worry about the significance of the actual '''bits'''.

Also: The task doesn't request for Arbitrary Precision ''bits'' per se, rather it asks for an algorithm that works 32 bits, and then another algorithm that works for <u>any</u> particular precision implementation.  Hopefully his would not preclude an ''Arbitrary Width Bits'' algorithm.  ¢ A '''not'' of 0 on Arbitrary Precision ''bits'' is going to be very interesting! :-) ¢

FYI: Algol68 permits the coercion/casting/widening of '''bits''' to a '''bool''' array, eg
```algol68
[]BOOL bools a = 2r101, 
       bools b = (TRUE,FALSE,TRUE); 
BITS bits c = 2r101, 
     bits d = bits pack((TRUE,FALSE,TRUE)); # Hence ... #
     print((bits width ELEM bits a, []BOOL(bits a)[bits width])) # prints "1F" - the same last bit #
```
 Hence there are strong synergies between '''bits''' and []'''bool''' type, but they are not the same type.  Similarly the types '''int''' and '''bits''' are distinct and require explicit casting with the operators '''bin''' and '''abs'''.  C - by contrast - does not have this distinction, in C an '''int''' also happens to be '''bits'''.

Good luck, and Merry Xmas!

[[User:NevilleDNZ|NevilleDNZ]] 02:02, 8 December 2011 (UTC)

== Initial C code specimen requires a long int algorithm ==

The use of #define in the C code specimen works really well when unrolling a loop.

The code issues are:
* code specimen lacks any more generally algorithm that would work - for example - with 128, or 256 width '''bits'''.  
* the code confuses "lowest set bit" (a bit order concept) with "least significant bit" (an integer concept).  These are actually almost opposites on most machines.
* ditto for "upper set bit" and "most significant bit".
* the results are in hex eg. 0xc0ffee, not binary 0b110000001111111111101110 making it difficult to visually discover/confirm the results.

Try using [[wp:Duff's device|Duff's device]] to unroll wider '''bits''' types.

A thought: maybe ( as C has given us << ''left'' shift and >> ''right'' shift ) this '''bits''' task should be asking for the first left bit set, and the first right bit set?  This would be "type agnostic" avoiding confusion with ''integer''s arithmetic properties.

NJoy [[User:NevilleDNZ|NevilleDNZ]] 03:17, 8 December 2011 (UTC)

: From a practical point of view (C is not likely your go-to language for proof of concept stuff), anything longer than the longest native type doesn't need its individual method, because it has to be in a struct or array, and there's no atomic bit operations anyway -- we are worried about efficiency (are we?), so a generic "any length bit operation" is out of question.  Just scan whatever struct or array from one side to the other and deal with the first non-zero item.  You can hardly do better than that without special hardware.  Unless we are talking about say, OpenCL, where it's a whole different matter.
: For the first/last nomenclature, an integer type has well defined least/most signicant bit concept, while first/last/left/right all depends on endianness.  If the bits are transmitted on a wire, we also need to know the bit ordering in a byte, so these are hazy at best.  The 32-bit number 257 can be stored in memory as either "01 01 00 00" or "00 00 01 01", so what's the "last" set bit?  Or the "left most" one?  Are we going to consider 32 bit as smallest unit, or 8 bit?  Unless you have a spec about using these bits, lsb/msb is a better way to refer to them.
: As to the hex vs binary output, I personally find reading 8 hex digit is less stressful than reading 32 1s and 0s.  I did provide the positions of the related bits, though. --[[User:Ledrug|Ledrug]] 04:22, 8 December 2011 (UTC)

C has given us << ''left'' shift and >> ''right'' shift operators for '''int''', '''long int''', and '''long long int'''.  Clearly C knows the difference between left and right. That could be a good starting point.  [[User:NevilleDNZ|NevilleDNZ]] 04:31, 8 December 2011 (UTC)

== Errors ==

The C example goes one power too far and runs into problems with losing the MSB. Minor correction needed though; it's not the core task itself that is wrong.

The Algol 68 example though, I cannot see how those figures (especially the LSB) could possibly be correct on a machine that uses binary digits. They are just wildly off. I don't know ''exactly'' what's wrong, but I truly don't believe them. Or are they counting from the other end? That'd just be weird given that virtually everyone numbers bits from the LSB, as that gives consistent answers for small integers irrespective of the size of machine integers being used. –[[User:Dkf|Donal Fellows]] 09:01, 8 December 2011 (UTC)

Compare:
* C's '''unsigned''' has the '''bits''' organised right to left, but - IIRC - not numbered in any way.
** The numbering is "virtual" depending on whether << or >> is used to extract the bit.
** Maybe C's closest "ordering" concept would be: 
```c
struct {signed int a:1, b:1, c:1, d:1, e:1, f:1, g:1, h:1;} ordered;
```

*** IIRC this won't work: 
```c
struct {signed int bits:1[32];} ordered;
```

** In particular unsigned '''bits''' (or unsigned) cannot ever be <u>coerced</u> to a '''bool''' array.
** There is no need or use in an actual number or index to the '''bits'''.
* Algol68 has two places where the bit number/index is used:
** '''elem''' operator, eg x '''elem''' 1 give the "first" bit of x, which is the bit on the extreme left.
** When a '''bits''' variable (eg x) is coerced to a '''bool''' array - []'''bool'''(x) - again the "first" bool - x[1] - is the extreme left bit. 
** One nice effect is x[LWB x:UPB x] would exactly extract the non-zero bits 2r11101101 of 2r0000000111011010000
* Weirdly, when a short '''bits''' literal e.g. '''bits''' x:=2r11101101 is assigned into the '''bits''' variable x, it is padded to the <u>left</u> 24 with zeros and become 2r0000000000000000000011101101!  i.e <u>right</u> justified just like C!
* '''shl''' and '''shr''' work the same as C's << and >>
[[User:NevilleDNZ|NevilleDNZ]] 12:01, 8 December 2011 (UTC)

: The problem comes when comparing, say, calculations of the bits for an 8-bit number with those for a 64-bit number. The only non-crazy interpretation of <code>upb</code> and <code>lwb</code> involves giving the same values for “65” however many bits are used to represent it. This in turn enables arbitrary-precision integer arithmetic (because you can ignore the actual representation width — a good thing because it may be variable in the implementation). OTOH, it also means that you probably lose the duality between bit arrays and numbers as with numbers you enumerate the bits in ''logical'' order, whereas with bit arrays you enumerate in physical order; there's no reason to assume the two are identical (nor is it possible to know the physical order of bits in a byte with modern hardware, though it probably follows the byte order in order to keep circuitry smaller in ALUs). The long and short of it is this: the task asked about integers so the results ''must'' be sane for them even if that leads to odd results for bit strings. (I prefer the convention of using <tt>-1</tt> for bit positions of <tt>0</tt>, as that at least is a position that is always not in the bit sequence at all; this might be in part implied by the fact that the Tcl solution is always using arbitrary-precision math; that's how Tcl's math works.) –[[User:Dkf|Donal Fellows]] 14:51, 8 December 2011 (UTC)

Fair enough, getting the answer for '''short int''', vs '''long int''' is useful, and OTOH it is also useful to know just the actual position of the bit without counting backwards from the last bit.  Note also:
* RFC's counts from the left to the right [http://tools.ietf.org/html/rfc2360#section-3.1 rfc2360].  
* There is a comparison in wikipedia ([[wp:Bit_numbering#LSB_0_bit_numbering|LSB_0_bit_numbering]] vs [[wp:Bit_numbering#MSB_0_bit_numbering|MSB_0_bit_numbering]]) and a description of [[wp:MSB_0_bit_numbering#Bit_numbering|Bit_numbering]].
* Intel asm uses the [http://pdos.csail.mit.edu/6.858/2011/readings/i386/BSF.htm BSF] & [http://pdos.csail.mit.edu/6.858/2011/readings/i386/BSR.htm BSR] op codes.
* Maybe the '''lwb''' and '''upb''' routines can be modelled (and renamed) to match intels BSF/BSR to avoid confusion?  
* Or keep  use '''lwb''' and '''upb''' for absolute position ([[wp:Bit_numbering#MSB_0_bit_numbering|MSB_0_bit_numbering]]), but use '''lwb0''' and '''upb0''' for LSB (bits width) reverse relative positions ([[wp:Bit_numbering#LSB_0_bit_numbering|LSB_0_bit_numbering]]). 
* Maybe '''rlwb''' and '''rupb''' for the "right/reverse/relative" '''lwb''' and '''upb'''.

Certainly greater minds then ours have already pondered this. c.f. [[wp:Lilliput_and_Blefuscu#Satirical_interpretations|Lilliput and Blefuscu]]. :-)

BTW: What is the representation of '''not''' 0xf in "arbitrary-precision" TCL?  I (tongue in cheek) suggest hex 0x[[wp:Aleph_number|&alefsym;0]] where &alefsym; is the HTML entity &amp;alefsym;... this can simply be pronounced "all F symbol", meaning an infinite number of Fs. ;-)

[[User:NevilleDNZ|NevilleDNZ]] 23:08, 8 December 2011 (UTC)

: It's a little complex (there's multiple representations of integers under the covers; small numbers are stored as C <code>long</code>s and slightly larger ones as <code>long long</code>s, assuming this is on a platform where those are a different type) but effectively (and simplifying vastly) it's 0xF0 plus some metadata to trigger the generation additional 0xFF bytes on the left as necessary, i.e., it stores that the number is negative. The actual padding out is done on the fly as forced by other code. However, the abstract model is indeed of an unbounded number of bits (just as a positive number would have an unbounded number of zero bits to the left). It just doesn't ever actually need that many for any even vaguely-reasonable number. :-) Of course, users of Tcl hardly ever have to deal with this: they're either in scripts — and so are using the abstract model — or they're in C and want a particular native numeric type; Tcl's implementation hides the gory details away unless they're writing C code that needs to efficiently operate on both small numbers and full bignums. That's kind-of rare in practice. –[[User:Dkf|Donal Fellows]] 03:40, 9 December 2011 (UTC)
== Assumption - host word size ==
Not all languages know about their host word size.  Implementing an "efficient" algorithm without loops or recursion for this special case is a bit arbitrary and dodgy in these cases. Sure an arbitrary special case could be coded but would that be efficient? What actually makes sense here? Furthermore the next natural long is also arbitrary.  --[[User:Dgamey|Dgamey]] 18:45, 9 December 2011 (UTC)
: There's worse problem than that.  The task wants you to use the same bit methods on arbitrary precision integers, "efficiently", which is oxymoronic.  Suppose you have a multiprecision integer that's made of an array of ''n'' native integers each of ''m'' bits long.  The sane way of finding the leading/trailing bit is by scaning those native integers from either end until you find the first nonzero one, then use the native bit operations to determine which bit in that you want.  Suppose you are looking for LSB, on average, if the bigint values is not with some oddball distribution, the very first native integer (the least significant word) is the one you'll need, and average complexity is O(1 log m). The task, as worded, seems to want one to use the same "efficient" bit operations in such a situation, because binary search is nominally O(log N).  Except if you actually perform the test <code>0 == value & 0xfff..(a million more)..ffff</code>, each bitwise <code>and</code> operation itself is already O(n), while the extend <code>== 0</code> test may be O(n) as well, not to mention the intermediary value is probably going to be stored somewhere which induces god knows how many clock cycles (likely the code will need to allocate memory), so the binary search is bound to be at least O(n (log n) (log m)), possibly a lot more.  It's plainly the wrong approach of doing things.
: This kind of things happen to people who love to overload operators.  What many fail to realize is that seemingly innocuous expressions <code>a & b == 1</code> could do a lot of harm if one insists on using overloaded operators to force a uniform appearance on tasks that are only superficially similar. --[[User:Ledrug|Ledrug]] 19:44, 9 December 2011 (UTC)
:: Ya, I was beginning to think about that.  I personally like polymorphism in operators and functions as a first cut.  But I also like to have the ability to step out and get deep into the weeds if something demands it.  Problem is that a lot of people seem to be in one camp or the other exclusively. :( --[[User:Dgamey|Dgamey]] 23:12, 9 December 2011 (UTC)
::: Actually, efficient MP engines only keep enough words around to represent the largest <code>1</code> bit needed to represent the number (they also do special tricks to handle negative numbers); they also keep around a count of how many words they are using (well duh!). That means that finding the “largest bit” (assuming positive) is possible in constant time. I don't know whether anyone really bothers with it though; the binary logarithm is not an operation that's needed very often given that the storage size is already known. (Finding the decimal logarithm though, that's tremendously valuable and also horribly expensive.) –[[User:Dkf|Donal Fellows]] 10:18, 12 June 2012 (UTC)

== task requirements ==
Four (or two, depending on you count them) of the task's requirements are:

::::* Define routines (or operators) '''lwb''' and '''upb''' that find the ···
::::* Also: Define the reverse routines (or operators) '''rlwb''' and '''rupb''' that find ···
(The added bold highlighting are mine, as well as the trimming.)

It appears that a vast majority of solutions (examples) haven't implemented the (above) four routines.   Are these ''strict'' requirements, or are people (or programmers <big><big>☺</big></big>)   allowed to ignore them completely or in part?

As for me, I ignored the first pair of requirements (for the REXX solution) as there didn't seem to be a need for REXX programmers to find the first bit of a number (a number that isn't even stored in binary in the first place), but I could write a pair of REXX subroutines to return a value that satisfies that requirement '''as if''' the number was stored in binary.   One thing about REXX is that it can be made to pretend about almost anything. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:39, 1 February 2014 (UTC)
