+++
title = "Template talk:Wikipedia"
description = ""
date = 2013-04-03T03:28:07Z
aliases = []
[extra]
id = 9166
[taxonomies]
categories = []
tags = []
+++

==Extension request==

I think this template needs to be extended so a date can be included. For example, under [[Bywater BASIC]], this template reads as follows:

<blockquote>This page uses content from Wikipedia. The original article was at Bywater_BASIC. The list of authors can be seen in the page history. As with Rosetta Code, the text of Wikipedia is available under the GNU Free Documentation License.</blockquote>

What I'd like to see is something that lets users add an ''optional'' date that indicates when the WP edits are from, something like this:

<blockquote>This page uses content from Wikipedia. The original article was at Bywater_BASIC, and the date of the text used is 11 May 2009. The list of authors can be seen in the page history. As with Rosetta Code, the text of Wikipedia is available under the GNU Free Documentation License.</blockquote>

...or something like that. I don't know how to do it or I'd do it myself. -- [[User:Eriksiers|Erik Siers]] 18:24, 15 January 2011 (UTC)

: When someone works on this, note that we nowhere use the second parameter (the language code) any more. The only case where it was present and not the default was to an article (in German) that happened to be less informative than the English version. –[[User:Dkf|Donal Fellows]] 02:49, 17 January 2011 (UTC)

:: I vote keep the language parameter; it seems potentially-useful to me. -- [[User:Eriksiers|Erik Siers]] 22:11, 19 January 2011 (UTC)

::: A date is more useful. Put it like this: I went through all the pages that use this template; most did not use the language parameter, and of those that ''did'', only one used anything other than <tt>en</tt>. In that one case (on the [[Dragon curve]]) it turned out that we didn't actually use any content from wikipedia (!) and that the equivalent page linked to on the english version was of substantially higher quality. (It had better pictures and more discussion of the algorithms, mathematical properties and history; to my mind, that's a definite count for “objectively better in an encyclopædic sense”.) Having done that, it was clear that the language parameter was not actually serving any useful purpose; it's a parameterization that makes things more obscure and harder to use, not less. (Also, wikipedia is good about linking between different language versions of a page.) –[[User:Dkf|Donal Fellows]] 00:16, 20 January 2011 (UTC)

RC needs this extension.... alternatively rename to Template:Wikipedia_pre_15_June_2009 & create Wikipedia_post_15_June_2009, and put a notice/warning on the Template detailing precautions that need to be taken.

Try:
* <nowiki>{{Wikipedia_pre_June 15_2009|oldid=142127294}}</nowiki> or alternatively
* <nowiki>{{Wikipedia|Page=Currying|oldid=142127294|time=18:38|date=2 July 2007}}</nowiki> with an error message if the date is missing.

[[User:NevilleDNZ|NevilleDNZ]] ([[User talk:NevilleDNZ|talk]]) 03:28, 3 April 2013 (UTC)

==Why Wikia link?==

Unrelated to the above, is there a reason the GNU FDL text links to Wikia, rather than the FSF? The FDL is [http://www.gnu.org/licenses/fdl.html here], and I think it would make better sense to link directly to it; I'm just wondering if there's a reason to leave it as-is. -- [[User:Eriksiers|Erik Siers]] 18:24, 15 January 2011 (UTC)
: I never noticed it. I didn't create the template. Fixed. --[[User:Short Circuit|Michael Mol]] 00:35, 16 January 2011 (UTC)
