+++
title = "Talk:Unicode variable names"
description = ""
date = 2011-07-10T23:45:26Z
aliases = []
[extra]
id = 10010
[taxonomies]
categories = []
tags = []
+++

==Why!==
I just saw [http://rosettacode.org/mw/index.php?title=Bitmap/Bresenham%27s_line_algorithm&curid=3214&diff=112356&oldid=112342&rcid=113529 this] edit and wondered ...? --[[User:Paddy3118|Paddy3118]] 07:23, 1 July 2011 (UTC)
:Yeah, that is not good at all. It means that the source code needs conversion to be usable on ascii based platforms. Maybe we will need a unidecoder for such sources. That is probably a future task. [[User:Markhobley|Markhobley]] 20:57, 6 July 2011 (UTC)
::The change was made to Perl 6 code, and it is my understanding that that's correct code for that language. Usage of Unicode glyphs in Perl 6 code is pretty normal. --[[User:Short Circuit|Michael Mol]] 21:58, 6 July 2011 (UTC)
:::I'm not saying that it is incorrect. However, I do not think that it is a good idea to use non ascii characters in source code, because this will not look good on some terminals, and printed listings may have wrong glyphs or these characters may be omitted completely from the printout. The older version of the code was better IMHO. [[User:Markhobley|Markhobley]] 22:19, 6 July 2011 (UTC)
::All Perl 6 compilers are required to understand Unicode.  It might mean that the code isn't editable in ASCII-only editors... but we don't intend to tie ourselves to ancient editors.  --[[User:Pmichaud|Pmichaud]] 22:05, 6 July 2011 (UTC)
:::Some ancient editors are excellent. Why would you not want to be able to edit the code in these? [[User:Markhobley|Markhobley]] 22:21, 6 July 2011 (UTC)
::::'Cause those ancient editors won't allow me to edit a newfangled language like ALGOL 58 that uses ← for assignment.  <tt>:-)</tt> --[[User:TimToady|TimToady]] 22:37, 6 July 2011 (UTC)
:All this stick-in-the-muddery is kinda beside the point; we'll all have to get used to the future sooner or later.  To answer the question behind the question, Perl 6 emphasizes use of Unicode where it enhances readability with traditional (and I don't mean ASCII) symbols.  It's arguable whether this helps a great deal with variable names (I think it does in this case), but certainly for operator names it turns what is normally evil, overloading existing operator names, into something that can instead clarify by adding new operator names where the need for visual distinction converges with traditional notations to make that possible.  Of course it can be abused, but that's never been a valid argument in Perl culture.  Rules that prevent you from writing ugly code also tend to prevent you from writing beautiful code. --[[User:TimToady|TimToady]] 22:25, 6 July 2011 (UTC)
::We could develop a transcoder, using unidecode to convert the source code into portable ascii format and uniencode to adapt the ascii based source for unicode platforms. [[User:Markhobley|Markhobley]] 22:31, 6 July 2011 (UTC)

:Not everyone speaks in a language that is adequately conveyed by ASCII. And even in English, some problems are routinely described using non-ASCII characters (just general maths equations comes to mind). We have learnt to convert to ASCII, and may not even notice we are doing it in english, but speakers of other languages might feal more pain. --[[User:Paddy3118|Paddy3118]] 00:11, 7 July 2011 (UTC)
::Even for English speakers, ASCII has always been a compromise.  Math symbols as mentioned; accented characters à la "café"; real quote marks; hyphens/dashes; currency marks, chapter marks, common symbols, ligatures, etc.  We are no longer in the age where 64k memory is the norm and 640k ought to be big enough for everybody, so we really shouldn't keep ourselves in the narrow mindset of lower 7 bits. --[[User:Ledrug|Ledrug]] 00:58, 7 July 2011 (UTC)
:::You do not need to speak English. You just need to speak the language of the of compiler or interpreter. You can still use produce Unicode programmatically, without the need to use non ascii source code. There are lots of arguments against Unicode in source code, but you will need to google for these. I am not going to go into all of these arguments here. One of the big problems was that the Egyptian and Thailand glyphs and some of the Chinese characters are not sufficiently different from each other to prevent confusion from readers that do not use these languages. [[User:Markhobley|Markhobley]] 16:16, 7 July 2011 (UTC)
::::All computer languages ignore some set of confused people.  If we all ignored the same set of confused people, we could all use the same computer language.  <tt>:-)</tt>  --[[User:TimToady|TimToady]] 16:55, 7 July 2011 (UTC)
:::: "You just need to speak the language of the of compiler or interpreter." ... That's a matter of syntax and semantic, and ignores the areas of leniency that languages may offer, including variable and other symbol names. I'm certain that's leveraged around the world; we've had a couple cases where people came to RC, and needed to use translation tools to communicate to us. Their source languages wouldn't have been fully representable in ASCII, and one's native language used a Cyrillic alphabet. I don't know for certain, but I would assume that, in their case, new meaningful symbol names would not be easily and conveniently represented in ASCII. --[[User:Short Circuit|Michael Mol]] 17:59, 7 July 2011 (UTC)
::::What? I don't know about Egyption or Thai, but I do speak Chinese.  There are historical (very rare) Chinese glyphs that don't have code points defined in Unicode, and there is the Unihan scheme that some people feel can be improved, but what is this "not sufficiently different from each other to prevent confusion" you are talking about?  And even if it were true, whatever it means, how is that worse than not being able to write certain things at all? --[[User:Ledrug|Ledrug]] 00:13, 8 July 2011 (UTC)
:::: The triangle symbol was a good example, but it is not the only problematic symbol. There was a really excellent article on this, but I can't find at the moment. What can you not write, because the source code is limited to ASCII? BTW I am not saying don't use Unicode output on a graphical display, I am just saying that source code should be editable using a traditional ascii based editor, and the symbols should be limited to those that are available on an ascii based keyboard. [[User:Markhobley|Markhobley]] 23:03, 9 July 2011 (UTC)
::::: Actually keyboard entry is important. I haven't got delta keys on my keyboard. To get that triangle, I have to type 6 keys: \u25b3, and this does not actually produce the triangle visual, but a gives a backslash followed by the hex code that I typed. [[User:Markhobley|Markhobley]] 23:35, 9 July 2011 (UTC) 
::::: How humanist.  So a Russian wishing to type some text in, I don't know, Russian, he should use an US keyboard instead of a Cyrillic one, and type a whole lot of escape sequences, so that not even a Russian can read it, but some outdated compiler will be happy.  Do we serve the computers, or do they serve us? --[[User:Ledrug|Ledrug]] 01:30, 10 July 2011 (UTC)
:::::: He can type text using a Russian keyboard. We are not talking about text, we are talking about source code. He can have ASCII symbols on the keyboard, so that he can still enter source code. [[User:Markhobley|Markhobley]] 07:16, 10 July 2011 (UTC)
:::::: Supposing a person in Thailand, has written the source code using Thai characters, and the Russian wants to edit it. He has not got the Thai symbols on his keyboard. The job is not going to be easy. It would be better if the code was in ASCII, and both keyboards carried the ASCII symbols. [[User:Markhobley|Markhobley]] 08:05, 10 July 2011 (UTC)
::::::: You can assume a Russian or Thai is better off typing in escape sequences than his natural language, he might even enjoy it, who knows.  You can stick with ASCII, or go back to punch cards, or directly wire 1s and 0s into your computer with a soldering iron for all I care, it's really not my problem.  The rest of the world does see the benifit of a large unified character set and will move towards it, and I personally would rather get along with it -- but no more arguing here from me, you win. --[[User:Ledrug|Ledrug]] 08:43, 10 July 2011 (UTC)
::::“You just need to speak the language of the of compiler or interpreter.” Isn't it nice that a number of languages are happy to support non-ASCII in identifiers then? People can use (variations on) their own (human) language when communicating with the computer, and it will all be semantically sound too. Moreover, if the language supports them, it'd be a poor implementation of that language that didn't. ;–) –[[User:Dkf|Donal Fellows]] 21:45, 9 July 2011 (UTC)
:::::: Just to be clear, I meant keyboards should carry ASCII symbols in addition to native language symbols, not instead of native language symbols. :) [[User:Markhobley|Markhobley]] 09:30, 10 July 2011 (UTC)

== The wrong triangle ==

This is how I wrote the Ruby code:


```ruby
# -*- coding: us-ascii -*-
STDOUT << "# -*- coding: euc-jp -*-

class Numeric
  def \xa1\xe5(\xa2\xf4\xa2\xf5\xa2\xf6)
    self <= \xa2\xf4\xa2\xf5\xa2\xf6
  end
end

\xa1\xe7 = Float::INFINITY
\xa1\xde5 = [-5, 5]
p [(\xa1\xde5.first.\xa1\xe5 \xa1\xe7),
   (\xa1\xde5.last.\xa1\xe5 \xa1\xe7),
   (\xa1\xe7.\xa1\xe5 \xa1\xe7)]
"
```


I found characters in some [http://www.rikai.com/library/kanjitables/kanji_codes.euc.shtml EUC-JP Kanji Code Table], and wrote the backslash escape sequences. I also found a delta character in the EUC-JP Kanji Code Table, but I confused two different characters.

{| class="wikitable"
!| Characters || EUC-JP   || Big5     || Unicode
|-
|| &#x25b3;   || \xa2\xa4 || \xa1\xb5 || U+25b3
|-
|| &#x0394;   || \xa6\xa4 || \xa3\x47 || U+0394
|}

I first found \xa2\xa4 U+25b3, and put this one in my program. I know enough of the Greek alphabet to wonder why my Greek delta was not with other Greek letters in the table. So I checked the EUC-JP Kanji Code Table again, and found the uppercase Greek alphabet in a different part of the table. This is when I switched to \xa6\xa4 U+0394. But &#x25b3; U+25b3 and &#x0394; U+0394 look so alike, I still confuse them. --[[User:Kernigh|Kernigh]] 03:00, 8 July 2011 (UTC)
:That's really fascinating from a language design point of view.  The problem of visual confusion is, of course, always going to occur when you try to cram too many codepoints into too few pixels.  There are a few things to be said, though.  First, choosing an appropriate font to the task can help.  I wish the Unicode Consortium's fonts were a bit more available, since they often try to maintain at least some subtle distinctions between similar glyphs.  This situation will likely improve somewhat over time.  Second, this is a really good argument for domain-specific subsetting of codepoint repertoires.  I'm not talking about reverting to ASCII, but rather contextual sanity checks.  For instance, in Perl 6, identifiers cannot contain △, U+25B3 WHITE UP-POINTING TRIANGLE, because only alphanumerics (including ideographs) are allowed there. Third, since substituting △ for Δ is really in the category of a typo, it's also a good argument for explicit declaration of new identifiers, a spot that Ruby is arguably a bit weak on...at least from my perspective. <tt>:-)</tt> --[[User:TimToady|TimToady]] 19:05, 9 July 2011 (UTC)

:They could do with grouping together similar looking symbols, and making them interchangeable, so that substitution can occur. I was just looking at the Unicode table: 	  (five identical looking pipe signs), પ and ૫, ඤ and ඥ, ረ and ሪ, ⬦	and ⬨. That was interesting. These last two appeared filled in one browser window, but hollowed out, when I pasted them here. Some symbols have no visual difference whatsoever. It is going to be a right nightmare trying to spot mismatched characters. [[User:Markhobley|Markhobley]] 01:12, 10 July 2011 (UTC)

:: They're not ''semantically'' interchangeable. They might not have the same glyph in all fonts. They might participate in ligatures differently (Latin-based writing systems are largely simple that way, but other writing systems are very much not). And anyway, it tends to not be such a huge problem in practice; individual (human) languages don't have to deal with the problem in the first place, it's only when you try to support all known writing systems that you run into problems. (If you want an area where there ''are'' problems due to the sorts of issues you mention, try unicode domain names; that's a totally different problem from source code though.) –[[User:Dkf|Donal Fellows]] 23:45, 10 July 2011 (UTC)
