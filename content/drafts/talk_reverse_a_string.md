+++
title = "Talk:Reverse a string"
description = ""
date = 2011-04-22T14:50:19Z
aliases = []
[extra]
id = 4598
[taxonomies]
categories = []
tags = []
+++

==Extra Credit?==
Does any example go for the extra credit Unicode combining characters? It seems to have been introduced [[http://rosettacode.org/mw/index.php?title=Reversing_a_string&oldid=22893 here]] by Kevin Reid, but I am not sure that even his [[http://rosettacode.org/mw/index.php?title=Reversing_a_string&oldid=36231#E E example]] goes for the extra credit. --[[User:Paddy3118|Paddy3118]] 04:35, 28 July 2009 (UTC)

:Nobody's tackled it since the requirement was introduced. It's moderately tricky too IIRC, as it gets into the whole problem of normalization of strings. —[[User:Dkf|Donal Fellows]] 08:07, 28 July 2009 (UTC)

::I've cooked up something that works for the given Unicode string in Python and I have tried to make it generic, but the more I read about Unicode, the more I know I don't know :-)     --[[User:Paddy3118|Paddy3118]] 08:30, 28 July 2009 (UTC)

I have found the [http://www.unicode.org/Public/5.2.0/ucd/Index-5.2.0d2.txt data table], ([http://www.unicode.org/copyright.html#Exhibit1 license]),  that is embedded in the Python module on-line. Should I split the task and have the stretch goal as a task on its own? (parse the table if needed, reverse a unicode string using the info from the table/an internal function with the combining info). --[[User:Paddy3118|Paddy3118]] 09:20, 28 July 2009 (UTC)

About my addition: Yep, it's often tricky, and that's why I said "extra credit". The thing is, if you ''don't'' do it, you get nonsense from certain Unicode strings; this will become increasingly relevant as the world drifts away from the habits of ASCII-and-a-few-extras. So I added this to spread a little Unicode-handling-awareness (though this isn't even complete: there are also e.g. bidirectional formatting markers, which need even more complicated handling). And that's why I think it shouldn't be a separate task: it's not a ''different problem'', it's ''more correctness'' (unless the string you're reversing is not really text, in which case you're looking for [[Binary_string_manipulation_functions|binary]] tasks). --[[User:Kevin Reid|Kevin Reid]] 12:03, 28 July 2009 (UTC)

:I am so relieved ''not'' to have to delve any further into Unicode. Whilst doing my background reading, I could not help but think that the margins were littered with little arrows and in much smaller text <small>Here be Dragons</small>. :-)   --[[User:Paddy3118|Paddy3118]] 16:35, 28 July 2009 (UTC)

:What unicode characters should be handled?  For example in ShinTakez' resolution of the issue in R it was assumed that the unicode character meant was not either of the specials, U+FFFC or U+FFFD; is that a reasonable assumption? -[[User:Russell|Russell Pierce]] 18:50, 28 July 2009 (UTC)

:: i realised that their could be further complications, but stopped at something that could handle unicode of the type given, i.e. simple chars with optional simple composable chars.

::: To me, it is a reasonable assumption. In fact I've not used the example given in the task, which is a real special one beyond common usage; handling Unicode like single characters (UTF-8 encoded or whatever) should not imply handling everything. I bet a lot of examples which handle just single byte encoding won't work if the single byte encoding used would have special character like those (i.e. which should be considered tied to the next character): they require a special handling... just with reversing and few more cases. I would change the example string just to stress the ability to handle multibyte encodings, rather than special composed characters in whichever single byte or multibyte encodings --[[User:ShinTakezou|ShinTakezou]] 22:38, 28 July 2009 (UTC)

::::Hi, their are two multibyte characters in the example given, as I get the hex values of the characters as being: <code>'61', '73', '20dd', '64', '66'</code>, and <code>'305'</code>. --[[User:Paddy3118|Paddy3118]] 01:50, 29 July 2009 (UTC)

::::: It's right; but they are both "combining" (combining enclosing circle and combining overline); the problem is in their "combining" ability, since they should be considered altogether with the character they combine with. When one reverts them just as common (multibyte) characters, the combination changes... --[[User:ShinTakezou|ShinTakezou]] 10:22, 29 July 2009 (UTC)


### What is needed to get the extra credit

Oh wait, I had forgot the years of heartache in the Python community before we got this far with Unicode. What you might need to get anywhere with the stretch goal would be:
#  Handle UTF-8 character strings
#  Handle multibyte unicode strings
#  Treat composed, possibly multibyte characters as an entity when reversing.


### Clarification


"Handling multibyte encodings" is '''NOT''' part of my extra credit; I assume it (where applicable) to be part of the basic task. If your string reversing mangles non-ASCII characters then I would consider it just incorrect to start with. --[[User:Kevin Reid|Kevin Reid]] 14:16, 29 July 2009 (UTC)

:Woa. Normally when handling text ASCII is assumed and unicode must be asked for. The 'normal credit' task does not mention unicode, and so answers assuming ASCII are correct. In your extra credit extension, you give characters that need unicode and so to get this extra credit, unicode handling is required. I don't think you should now force the 'normal credit' task to require unicode. --[[User:Paddy3118|Paddy3118]] 22:29, 29 July 2009 (UTC)

Um. If your character set includes Unicode, a reversing routine should handle it. If your character set does not include Unicode, the reversing routine need not handle it. --[[User:Kevin Reid|Kevin Reid]] 00:41, 30 July 2009 (UTC)

== Notes about Unicode combining characters ==

[[Ruby]] has the regular expression <tt>/\p{M}/</tt> which matches a combining mark. With this expression, I might be able to reverse a string while preserving the combining marks.

# The most relevant parts of [http://www.unicode.org/versions/Unicode6.0.0/ Unicode 6.0.0] seem to be section 3.6 "Combination" and section 3.12 "Conjoining Jamo Behavior".
# I am not yet certain whether to preserve "combining character sequences" or "grapheme clusters". My best guess for now is to preserve the combining character sequences (CCS), not the grapheme clusters.
# The regular expression for a CCS-or-char might look like <tt>/(?>#{base}\p{M}*|\p{M}+|.)/</tt> where <tt>#{base}</tt> is whatever regular expression matches a base character or extended base. The <tt>?></tt> prevents backtracking, so the regexp always matches the longest possible CCS.
# I need some way with Ruby to comb a string for all matches of a regular expression. For example, with <tt>/[aeiou]./</tt> and <tt>"Rosetta Code"</tt>, I want <tt>["os", "et", "a ", "od"]</tt>. Then I would comb a string for CCS-or-char, reverse the array, join.
# Korean hangul is a special case. A group of 2 or 3 jamo characters might form an extended base (a syllable with an leading consonant, a medial vowel and perhaps a trailing consonant. Because a CCS may contain an extended base, I need some way to group jamos.
# I probably want a Korean test string. I must enter this string with jamo characters, not syllable characters, to test the code to group jamos.
# If EUC-KR has jamo characters, then the code should work with both EUC-KR and UTF-8.
# Avoid normalization. A normalization to NFC would replace some CCS with individual characters, but Unicode does not have individual characters for every possible sequence.
# Do I have a library that already does some of this?

--[[User:Kernigh|Kernigh]] 04:00, 31 January 2011 (UTC)

:Have you considered [http://unicode.org/reports/tr9/ directionality]? --[[User:Rdm|Rdm]] 04:13, 31 January 2011 (UTC)

== Extra Credit ==

The extra credit task seems artificial.

Specifically, it's using unicode, and I can see that demonstrating unicode handling in string reversal could be a good thing.

However, a close look at the characters involved in the extra credit task shows that two of the character codes in the "reversed string" are ''required to be '''not reversed''''' from the order in which they appear in the original string.  And while that could indeed be an interesting task, I do not think that it belongs in the "Reverse a string" task.

The task that would be appropriate to this example might perhaps be better labeled "manipulate a string while retaining structures implied by the use of some contained unicode characters" though perhaps a shorter name is possible.

--[[User:Rdm|Rdm]] 14:50, 22 April 2011 (UTC)
