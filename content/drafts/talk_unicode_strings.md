+++
title = "Talk:Unicode strings"
description = ""
date = 2012-11-22T02:57:08Z
aliases = []
[extra]
id = 10140
[taxonomies]
categories = []
tags = []
+++

==The task description change==
I strongly object to the modified task description and requirement here.  My intention was to have people tell us about their languanges' Unicode capabilities in general, not having them solve puzzles.  This is a fairly big topic, and people should be allowed to talk about various aspects of it, not just the specified 4 things.  The original requirement was specifically made as suggestions, not homework assignment. --[[User:Ledrug|Ledrug]] 15:29, 22 July 2011 (UTC)
:I'll second that the modifications are inconsiderate.  They should be discussed, especially with the original task author.  Immediate self-promotion from draft status is even worse.  &mdash;[[User:Sonia|Sonia]] 17:06, 22 July 2011 (UTC)

:Original wording "Demonstrate how one is expected to handle Unicode strings.  Some example considerations: can a Unicode string be directly written in the source code?", rewording "Demonstrate how Unicode strings are represented in source code". (The new wording gives scope for embedding of Unicode strings or representation by other means. The old wording just gives a boolean yes or no,)

Original wording "How does one do IO with unicode strings?", rewording "How to perform input and output using Unicode strings" (They are the same thing).

Original wording "Can these strings be manipulated easily?", rewording "Demonstrate examples of string manipulation" (The meaning has not changed, but we have the facility to demonstrate, rather than a boolean yes or no response.)

Can non-ASCII characters be used for keywords/identifiers/etc (see [[Unicode variable names]])? I dropped this, because we have separate tasks for part of it, and we can create a "Unicode keywords" task to cover the keywords bit.

Original wording "What encodings (UTF-8, UTF-16, etc) can your language accept without much trouble?", rewording "Demonstrate: Encodings supported by the language (eg UTF-8, UTF-16, etc)". Again the revision gives scope for demontration, rather than a boolean response.

Where is the problem? [[User:Markhobley|Markhobley]] 17:34, 22 July 2011 (UTC)
:Originally they were preceded by "some example considerations:", which makes a big difference--they were not task requirements, just some suggestions of what to say.  And some of these can be talked about, but difficult to ''demonstrate''.  How are you going to ''demonstrate'' your language can handle UTF-16 when the web page is encoded in UTF-8?  Again, this is a very big and important topic, so people should be allowed to talk about what they think is most important.  It helps to gather key points here, instead of forcing readers to go to one page for keywords, another for variable names, etc.  And at least, ''discuss'' about intended changes before you just go and do it: apparently I am not alone in disliking the changes.  I'd like to hear some more opinions on this, and if not many people support the changes, I'll revert it later. --[[User:Ledrug|Ledrug]] 17:55, 22 July 2011 (UTC)
::Ok, I have made the demonstrations optional. I am sure that it must be possible to demonstrate handling of UTF16. Just a quick search through google, and it appears to be done on other websites ok. [[User:Markhobley|Markhobley]] 18:06, 22 July 2011 (UTC)
:::We could merge the Unicode tasks into one task called Unicode, or maybe we could create a parent task that links to these subtasks. I had the reverse problem on a task that I had written. I tried to gather information points in one place, and people wanted it breaking down. There are probably pros and cons to both methods. I think they should be kept separate, but grouped together under a category "Unicode". [[User:Markhobley|Markhobley]] 18:12, 22 July 2011 (UTC)
:The wording still feels too much like homework assignment.  I'll probably change something later.  On a separate note, where did that "510k" figure come from in Locomotive Basic section? --[[User:Ledrug|Ledrug]] 20:43, 22 July 2011 (UTC)
::Yeah, I have tidied up the English a bit, to make it more formal, other than that, the task looked good to go. You can add more bullet point options, if there is something missing. 

The 510k estimate was based on 8 bytes per character, and I made the calculation based on a 65535 character Unicode page and allowed for 96 characters that are already in ROM. This was (8 bytes x (65536-96) characters) / 1024. [[User:Markhobley|Markhobley]] 21:41, 22 July 2011 (UTC)

== New task description ==

I think we can cut the introduction.

"As the world gets smaller each day, internationalization becomes more and more important." - This is not about Unicode, and nothing to do with the task.

"For handling multiple languages. Unicode is your best friend. It is a very capable tool." - This is sales pitch... we don't need that either: It is nothing to do with the task.

"but also quite complex compared to older single- and double-byte character encodings." - pros and cons, again nothing to do with the task.

"How well prepared is your programming language for Unicode?" - Ok, now that we can use. - Lets add it to the topic list.

Discuss and demonstrate its Unicode awareness and capabilities. Some suggested topics:

We might want to give indication as to what Unicode is, (rather than saying it is our "best friend", link the word Unicode to the wikipedia article).

FWIW, Unicode can also be our [http://rosettacode.org/wiki/Talk:Unicode_variable_names worst enemy], so I would not agree with "best friend" as being an adequate description.

[[User:Markhobley|Markhobley]] 07:38, 23 July 2011 (UTC)

I found some more sales pitch: "Unicode support is so fundamental nowadays that there is probably not much room left for cleverness." - We don't need that either. It has nothing to do with the task. [[User:Markhobley|Markhobley]] 07:44, 23 July 2011 (UTC)

:I preferred the chatty, inclusive, original. This is an English website, but I happen to agree with Ledrug and the original text. It seems that many native English speaking programmers need to be goaded into thinking about Unicode, and the tone of the original was more likely to involve the reader, IM'''H'''O. --[[User:Paddy3118|Paddy3118]] 10:40, 23 July 2011 (UTC)
:: Maybe we could link to a separate article on the pros and cons of Unicode, but I don't think that is really required. I think we should be brief and precise and stick to the task. We could create a separate article about Unicode, which covers the pros and cons. The original unnecessay verbage was not chatty, tt was one sided and did not state drawbacks. However, I don't think such an article is really necessary here. It does not affect the implementation of the task in any way. [[User:Markhobley|Markhobley]] 19:52, 23 July 2011 (UTC)
::: What "implementation"? You still don't get it.  It's ''not'' a coding task.  Let me say it again, it's '''not''' a coding task.  It's a place where people can share their knowledge with others who are not familiar with a specific language.  And I did say Unocode is complex, what more drawbacks do you need?  Please stop trying to force everyone to agree with your narrow-minded point of view, and ''do'' discuss on talk page before you go around and change descriptions according to your feelings alone. --[[User:Ledrug|Ledrug]] 20:13, 23 July 2011 (UTC)
:::: I haven't stated my "points of view". What are you talking about? The task description is currently neutral as I can see, and the task is in draft. [[User:Markhobley|Markhobley]] 20:25, 23 July 2011 (UTC)
:Given that Markhobley's edits didn't receive any support in past three days, I'm changing it back to original Q/A format.  Also, "how well prepared" is ''the'' question for the task, it should not be in the suggestions list;  encoding/normalization/canonization are in the same category, they should not be separated.  Before you edit it again, please discuss here first. --[[User:Ledrug|Ledrug]] 03:19, 25 July 2011 (UTC)

== Request to undraft ==

I think there are enough good discussions on the page that it can be promoted out of draft state now. --[[User:Ledrug|Ledrug]] 05:25, 3 August 2011 (UTC)

== Bi-directional text ==
I'm surprised to see no mention of right-to-left or bi-directional text in the task description.  If this is really about 'internationalization' then being able to cope with these is as important as being able to render Unicode characters (if not more so); it's certainly more challenging!  If this isn't thought to be an appropriate task to investigate those language capabilities perhaps there should be a new one. [[User:RichardRussell|RichardRussell]] 22:36, 17 November 2012 (UTC)

It's not clear that that's a programming language issue. Consider, for example, a programming language which runs on a variety of hardware, and hosted by a variety of operating systems and using a variety of display mechanics.  If the programming language is displaying text via a web browser, for example, then the issue of text rendering is a browser issue.  If the programming language is displaying text via a java app (for portability) then this becomes a java issue.  If the programming language is displaying text via gtk, then this becomes a gtk issue.  And so on... --[[User:Rdm|Rdm]] 02:57, 22 November 2012 (UTC)
