+++
title = "Village Pump:Home/Syntax Highlighting ( archived 2009-06-18 )"
description = ""
date = 2009-06-18T20:29:01Z
aliases = []
[extra]
id = 2900
[taxonomies]
categories = []
tags = []
+++

This page is to discuss changes to the syntax highlighting system.

==About bugs in GeSHi==
Please don't hesitate to report code highlighting bugs (missing keywords, problems with strings, mishighlighted stuff) upstream at BenBE@geshi.org to have them fixed in the official release. Also if you happen to have a new language file I'm happy to include it (if it passes the langcheck script included with current releases or you have a good excuse for it not to ;-)).

Regarding some issues mentioned here:
* Double escape in D has been fixed in SVN
* The Lisp problem could only be fixed, because someone reported it ...
* The C issues has been included; also fixed c_mac, cpp and cpp_qt
* The multiline preprocessor directives are fixed in SVN trunk; RC coming soon
* Modula3 included in SVN trunk; coming with the RC
* The whitespace being trimmed is a MediaWiki bug. Whitespace is an esotheric language; I didn't intent people to test their sites with it ;-) Though it beautifully works to find CSS bugs (have to patch the CSS on the official GeSHi page ;-))
* Cannot reproduce the OCaml issue locally. Your site simply is lacking CSS for class coMULTI.

Latest language files can be found at https://geshi.svn.sourceforge.net/svnroot/geshi/trunk/geshi-1.0.X/src/, although some changes between releases might require you to update the parser too. Latest bugfixes not yet considered stable are reflected there. Release Candidate grade updates can be found at https://geshi.svn.sourceforge.net/svnroot/geshi/branches/RELEASE_1_0_X_STABLE/geshi-1.0.X/src/

BenBE.
Author of GeSHi
: I'll be moving Rosetta Code over to GeSHi svn HEAD to take advantage of these fixes, and to make merging our fixes with upstream simpler. --[[User:Short Circuit|Short Circuit]] 19:16, 8 February 2009 (UTC)

==Code tag change==

I'm going to make some major changes to the Syntax Highlighting extension this weekend.  Instead of denoting a block of C code as:

&lt;C&gt;(some code)&lt;/C&gt;

Code will be denoted as:

&lt;lang C&gt;(some code)&lt;code&gt;

This will significantly clean up the Mediawiki extension namespace, and make formatting tricks with CSS easier.  I'd rather create an attribute to &lt;pre&gt;, but that appears as though it could be more complicated. --[[User:Short Circuit|Short Circuit]] 05:52, 2 July 2008 (UTC) (updated) --[[User:Short Circuit|Short Circuit]] 21:59, 29 January 2009 (UTC)
:Will we need to go and change all of the previous highlighting then? --[[User:Mwn3d|Mwn3d]] 06:11, 2 July 2008 (UTC)
::Yes.  Both approaches will be supported for a little while, I expect, but the current system will definitely be phased out. --[[User:Short Circuit|Short Circuit]] 23:42, 2 July 2008 (UTC)
:::Isn't it possible to access the raw files behind the Wiki, so one could run a simple replace operation on all of them? Also, is there any way to add syntax highlighting for languages for which it isn't supported yet? --[[User:Dirkt|Dirkt]] 11:07, 6 July 2008 (UTC)
::::I'm not sure about the raw files idea (it sounds easy enough), but the new languages idea gets a bit hairy. The latest GeSHi (v1.0.7.22) has support for 96 languages (counting things like "java" and "java5" as different languages). These languages don't necessarily overlap with the 103 here. Newer languages like Rhope and non-computer languages like TI-83 BASIC will probably never have highlighting. In order to add a new language, we would need to make a special PHP file for it with lists of the following:
::::*All keywords that could be highlighted (in a jagged two dimensional array, separated into groups which get different kinds of highlighting)
::::*Style codes for each group of keywords, comments, escape characters, brackets, strings, numbers, function names, symbols, scripts, and regex's
::::*URLs for each group (if you want keywords to link anywhere like javadocs for Java)
::::*Quote characters
::::*Symbols from the language (besides math operators)
::::*Comment characters
::::Also it would need a regular expression for comments. It would probably be best to talk to the people working on [http://sourceforge.net/projects/geshi/ the GeSHi project] about adding new languages.--[[User:Mwn3d|Mwn3d]] 16:29, 7 July 2008 (UTC)

: It's happened!  I've replaced the GeSHiSyntaxHighlight Mediawiki extension with the GeSHiCodeTag extension.  The old tag format works (for now), but it ''will'' be removed as soon as possible.  All tags of <nowiki>&lt;mylang&gt;some_code&lt;mylang&gt;</nowiki> need to be changed to <nowiki>&lt;lang mylang&gt;some_code&lt;/lang&gt;</nowiki>.  So this block:

```cpp
#include <stdio>

int main()
{
     std::cout << "Hi!" << std::endl;
     return 0;
}
```


: Should be rewrapped like this:

```cpp
#include <stdio>

int main()
{
     std::cout << "Hi!" << std::endl;
     return 0;
}
```

: This is going to require a massive site-wide effort for all supported languages.  See [[Help:GeSHi]] for details. As soon as we're confident most of the pages have been handled, I'll disable support for <nowiki>&lt;lang&gt;</nowiki> entirely. --[[User:Short Circuit|Short Circuit]] 02:33, 23 January 2009 (UTC)
:: When the <nowiki><lang></nowiki> tags are removed, [[Special:Version]] will not any more list all the languages in the tags section, right? Maybe there should be a (perhaps auto-generated) page listing all supported languages (or maybe it's even possible to include them in [[Special:Version]] as separate section). Or maybe there's already such a list somewhere, which I simply didn't notice? --[[User:Ce|Ce]] 08:18, 23 January 2009 (UTC)
::: We are now only using <nowiki></nowiki>
 tags, where "cpp" is replaced by the language ID in question.  Additionally, all language IDs are now case-insensitive, so <nowiki>
```c
</nowiki> works the same as <nowiki><lang c></nowiki
.  This is a Good Thing, because the supported case was originally determined by the case of the language file name.  And those were all lower case... --[[User:Short Circuit|Short Circuit]] 08:11, 11 February 2009 (UTC)
:::You mean [[Help:GeSHi#Supported_source_tags|this]]? --[[User:Mwn3d|Mwn3d]] 13:27, 23 January 2009 (UTC)
::: Another problem I just noticed: The old meaning of <nowiki><tt>text</tt></nowiki> is now changed (it now creates a div block instead of inline text). Those tags might have been used already in this wiki. Maybe it would have been a better idea to use a tag which didn't yet have a meaning (say, <nowiki><source></nowiki>). I'm not sure whether it's a good idea to change the GeSHi tag now, or to find and change all previous usages of <nowiki><code></nowiki> instead. --[[User:Ce|Ce]] 08:50, 23 January 2009 (UTC)
:::: I just noticed this too (when writing my task [[N distinct objects]]). Since &amp;code> being inline is defined by the HTML specification, to be inline, we should not redefine it to be block. --[[User:Kevin Reid|Kevin Reid]] 00:48, 29 January 2009 (UTC)
:::::You should use the <nowiki><tt></nowiki> tag instead. --[[User:Mwn3d|Mwn3d]] 01:57, 29 January 2009 (UTC)
:::::Sorta Fixed.  We now use &lt;lang mylang&gt; instead of &lt;code mylang&gt;.  &lt;code&gt; will remain broken until the existing pages are fixed. --[[User:Short Circuit|Short Circuit]] 21:59, 29 January 2009 (UTC)
: It seems color themes have changed for quite a number of languages...perhaps we should decide on a favorite theme, and then use that for every language? Would take quite a bit of work though... --[[User:Mbishop|Mbishop]]
:: Should be fixed.  The upgrade in versions of GeSHi obliterated a configuration change I'd forgotten I made.  I need to make a list of these things.  As for a common color scheme, I'm all for it.  GeSHi is now set to apply different CSS classes to the different code concepts it's familiar with.  The current theme is set in [[Mediawiki:Common.css]], and can be changed sitewide by modifying that. --[[User:Short Circuit|Short Circuit]] 20:21, 24 January 2009 (UTC)


### Old code used the right way


Now I am using the <nowiki>
```LANGNAME
</nowiki>
 syntax for new examples, and when I've landed on the previous usage, <nowiki><LANGNAME></nowiki>, I've made here and there some fixes. '''But''' I've indeed fixed also the ''right'' html usage of the <nowiki><code></nowiki> code tag into text (changed to <nowiki><tt></nowiki>), since it raises a warning and ruins the output layout when used as ''normal'' html tag. Anyway now I stop fixing that, I suppose everything will go fine when the old code-style syntax-HL syntax will be abandoned completly. (Right?) --[[User:ShinTakezou|ShinTakezou]] 19:00, 1 February 2009 (UTC)
: I know you already know this now, but I'm just mentioning this here because it's been talked about and resolved in a few places.  <nowiki><code></nowiki> is now safe to use anywhere where it's preferable to use the HTML tag of the same name; The Syntax highlighting engine doesn't care about it one whit. --[[User:Short Circuit|Short Circuit]] 19:17, 2 February 2009 (UTC)

==Switch!! &lt;code&gt; is now &lt;lang&gt;==
I realize we just switched, but it has been pointed out that '''code''' is already an HTML element, and appropriating that tag for wiki text will cause problems for folks who sprinkle HTML into their edits. (Shame on them. ;-) )

Anyhow, you can now use &lt;lang c&gt;/* some C code */&lt;lang&gt; the same way you would have used &lt;code&gt;.  Like so:


```perl
print scalar @ARGV
```


Sorry for the inconvenience and the flip-flopping.  Had to do this soon, before the entire site managed to switch.  On the bright side, with the changes I made to GeSHiCodeTag, it will be very, very obvious which examples need to be switched.  I also noticed that Mediawiki reinterprets the output of code tags, so I may be able to insert category references automagically.  Need to look into this further... --[[User:Short Circuit|Short Circuit]] 21:03, 29 January 2009 (UTC)
:Booo! All that work I did for [[RCBF]], [[RCSNUSP]], and [[RCRPG]] is worthless!...No this is a good idea. I hadn't realized how many people used the code tag for inline text before this. i'll change the TODO list item and get cracking on it sometime. --[[User:Mwn3d|Mwn3d]] 21:09, 29 January 2009 (UTC)

==Adding new languages==

Ok, I had a look at the source in the SVN. Doesn't seem too difficult to add new languages; basically all one has to do is to copy the php files for one of the existing languages and change it to suit the new one. I guess I could make for Haskell, say, in less than an hour. BTW, I cannot see 96 languages, the SVN just has C, Codeworker, C#, CSS, Delphi, Doxygen, Eiffel, HTML, Java, Javascript,
PHP, QBasic, SQL, VHDL and Web3D. Unless I looked in the wrong place.

I'd like to propose the following:

* Add a page about syntax highlighting, what languages are available, and what one must do to extend/change the existing highlighting.
* Link that page from the homepage.
* Make a copy of the php-files with the syntax highlighting code available via the Wiki. Then people can just grab the code that fits best, and turn it into code for a new language.
* When they've done that, they should ask someone with admin rights to incorporate the changes. That shouldn't happen too frequently, so the workload for the admins should be tolerable. For security reasons, it's probably a bad idea to allow editing of "life" php code.
* Make &lt;code lang="xyz"&gt;...&lt;/code&gt; act the same as &lt;pre&gt;...&lt;/pre&gt;, if there's no syntax highlightling definition for '''xyz'''. This will allow to write syntax highlighting tags ''right now'', instead of having to replace them all later when syntax highlighting for that languages becomes available
* Once the php code has settled, one can submit it back upstream.

Rationale:

* Rosetta is the ideal place to write and test syntax highlighting. There are already many code examples available, and it will be immediately useful. And it will also offer motivation to make syntax highlightling for more esoteric languages.
* I'd like the "feedback loop" to be as short as possible, in true Wiki style. People contribute because they can immediately see the results. If one first has to contact the upstream developers, then wait until the next version of Geshi comes out, then wait until it's installed at Rosetta, etc., I guess the motivation to do something will be pretty low. It's already bad that one ask to one of the admins to "go live" with it, but I guess otherwise the security risk is just too great.
* I offer to do all of the work outlined above myself as far as I am able to :-) So I need someone with admin rights to make a copy of the php scripts of the currently installed available through the Wiki, but I can write the other Wiki pages etc.

Comments? --[[User:Dirkt|Dirkt]] 09:29, 8 July 2008 (UTC)
:You can see all of 96 languages here: http://geshi.svn.sourceforge.net/viewvc/geshi/tags/RELEASE_1_0_7_22/geshi-1.0.X/src/geshi/
:I guess you could try to just morph existing languages into new ones...I wouldn't want to do it, but I can add them if you make them. Maybe we should talk about it in the IRC channel. --[[User:Mwn3d|Mwn3d]] 20:58, 8 July 2008 (UTC)

::Ok, that's better :-) I guess those languages should be enough for most cases. BTW, is the new code/lang-tag already active? I cannot get it to render. While we're at it, the math-tag is also broken.
::I also created/changed a couple of pages to document the syntax highlighting stuff. Please correct/update as appropriate. --[[User:Dirkt|Dirkt]] 10:26, 9 July 2008 (UTC)
:::We had a page on [[Help:GeSHi|GeSHi]] already. The math tag was never installed. When I made the Formatting page before I told Short Circuit about it, but it wasn't very important back then. We don't deal much with math symbols more complicated than <nowiki><sup> and <sub></nowiki> anyway. --[[User:Mwn3d|Mwn3d]] 14:18, 9 July 2008 (UTC)
:::: Well, that page isn't particularly easy to find: [[Special:Whatlinkshere/Help:GeSHi]] and the two links there list no pages linking to it except the above link you gave. And if you don't know that GeSHi is installed in the first place (and what it is), the name doesn't help, either. IMHO, it's really better to  naming pages after their function. For the math tags, some people (not me) '''did''' propose problems that need a substantial amount of math, and we have several problems that at least come near a moderate amount of maths (like eigenvalues for matrices). So occasionally, the math tags would be useful. Actually, when involved with the first problem mentioned, I spend quite some time rewriting stuff into math tags, only to discover they don't work. I don't know how difficult it is to support them, but at least I would appreciate a clear decision, and easy to find information that tells you if they are supported, or not :-) --[[User:Dirkt|Dirkt]] 10:06, 10 July 2008 (UTC)

==Problem with Lisp==
There is something wrong with the highlighting of certain keywords in Lisp, for example:

```lisp
defun
```


```lisp
list
```


```lisp
length
```

--[[User:Spoon!|Spoon!]] 09:03, 3 November 2008 (UTC)

This still hasn't been fixed, as of December 31st, 2008
:This still isn't really our problem to fix. Check the discussion above. The syntax highlighter we use is from the open source project GeSHi. If someone could help us find a suitable replacement, that'd be a big help. --[[User:Mwn3d|Mwn3d]] 01:00, 1 January 2009 (UTC)
: Looks like the GeSHi update fixed it. --[[User:Short Circuit|Short Circuit]] 00:54, 23 January 2009 (UTC)

==Missing Python keywords==
At least two keywords aren't getting highlighted for python syntax, 'any' and 'all'.
I suspect that 'with' is also missing.

--[[User:64.238.49.65|64.238.49.65]] 00:46, 14 November 2008 (UTC)

:The latest GeSHi release has the new Python keywords, builtins, and types defined. This would be the GeSHI version released on 25 Dec 2008. --[[User:Rldrenth|Rldrenth]] 15:02, 2 January 2009 (UTC)

:: The new version of GeSHi is installed. --[[User:Short Circuit|Short Circuit]] 00:54, 23 January 2009 (UTC)

==Enhancement for C syntax hl==
While adding some example for C, I noticed the following oddities:
* the parser is not case sensitive (C is!), since it hl-ed '''If''' as the keyword '''if'''--[[User:ShinTakezou|ShinTakezou]] 14:25, 17 December 2008 (UTC)

```c
if
```


```c
If
```


```c
iF
```


```c
IF
```

: Fixed. I'll send the relevant changes upstream. --[[User:Short Circuit|Short Circuit]] 00:53, 23 January 2009 (UTC)

* the multiline preprocessor defines (using \ at the end of the line) are not handled
--[[User:ShinTakezou|ShinTakezou]] 14:25, 17 December 2008 (UTC)
: It looks like GeSHi handles preprocessor directives by identifying the # as a single-line comment character.  In order to use a single # for a multi-line comment, one would need to modify the custom regex field:

```php
'COMMENT_REGEXP' => array(1 => '/\/\/(?:\\\\\\\\|\\\\\\n|.)*$/m'),
```

: Adding another regex to the array to handle multiline preprocessor directives should fix that.  My regex is rusty, though. --[[User:Short Circuit|Short Circuit]] 00:53, 23 January 2009 (UTC)

==Update GeSHi==
Can we get GeSHi updated? Also I created a Modula-3 language file for GeSHi and submitted it to their sourceforge forums, hopefully it will get put in SVN for their next release. --[[User:Mbishop|Mbishop]] 05:22, 22 January 2009 (UTC)

:It's been updated, per your request.  Also, I created [http://rosettacode.org/geshi.txt this 1.5MB file] in an attempt to put the GeSHi source files where everyone could see and correct them.  I'd intended to place it into a subpage of this one, but MediaWiki OOMs while it parses the wikicode.  And I'm not fond of the idea of increasing the interpreter's memory consumption limit again. --[[User:Short Circuit|Short Circuit]] 10:02, 22 January 2009 (UTC)
::Is there a way to edit that file? Is that the one that the site uses or is it just compiled from the GeSHi files? --[[User:Mwn3d|Mwn3d]] 19:14, 22 January 2009 (UTC)
:::No way to edit it where it currently sits...It's too large for Mediawiki in one large clump.  It could be broken into per-language pages, though. --[[User:Short Circuit|Short Circuit]] 23:42, 22 January 2009 (UTC)
::::It would be nice if there was a separate page for syntax highlight for each language, and an option to add new languages. But meanwhile, if I create GeSHi file for a new language, could I send it to someone who has ability to integrate it in the system? --[[User:PauliKL|PauliKL]] 10:09, 23 January 2009 (UTC)

:I think if you do it right, languages you add to the syntax highlighting will show up in the "Parser extension tags" [[Special:Version|here]]. I'm not quite sure though. --[[User:Mwn3d|Mwn3d]] 13:48, 22 January 2009 (UTC)

::I can't seem to get the Modula-3 highlighting to work? I tried <nowiki>
```modula3
</nowiki>
 and even <source lang="modula-3">. --[[User:Mbishop|Mbishop]]

::: <nowiki>
```modula3
</nowiki>
 isn't listed in the parser extension tags on [[Special:Version]]. I guess something has to be inserted somewhere in the MediaWiki code, so it knows that <nowiki>
```modula3
</nowiki>
 stuff has to be handed to GeSHi. --[[User:Ce|Ce]] 18:45, 22 January 2009 (UTC)

:::: Perhaps, I know the language file works (tested it on my own apache), but I didn't test with mediawiki, not sure what needs to be done there. --[[User:Mbishop|Mbishop]] 19:52, 22 January 2009 (UTC)
::::: The Mediawiki extension I'm using may have a specific list of supported languages.  I'll check to see what exactly is going on... --[[User:Short Circuit|Short Circuit]] 23:42, 22 January 2009 (UTC)
:::::: Indeed, that was the problem.  Changed. <nowiki>
```modula3
</nowiki>
 should now work.  But now the namespace seems to have conflicts.  The contribution copyright warning seems to be being parsed as a language.  This is the kind of thing I was worried about with the namespace pollution.  Working on it... --[[User:Short Circuit|Short Circuit]] 00:04, 23 January 2009 (UTC)
::::::: Fixed.  Apparently, there's a language whose GeSHi tag would be &lt;div&gt;. --[[User:Short Circuit|Short Circuit]] 00:22, 23 January 2009 (UTC)

:The failure mode for an unrecognized language is pretty annoying: the code itself is replaced with an unsorted list of available languages. Could this be fixed to just show the code in a PRE block? This way we can start using CODE tags for ''all'' languages now, and they'll be automagically formatted when language support is added. --[[User:IanOsgood|IanOsgood]] 18:21, 24 January 2009 (UTC)

==Whitespace==
If anyone ever feels like putting whitespace code on here (unlikely but possible) they may have trouble highlighting it:

```whitespace






```

Taking out the nowiki tags eliminates the code:

```whitespace






```

Not sure if that can be fixed or not. --[[User:Mwn3d|Mwn3d]] 18:58, 23 January 2009 (UTC)

: I guess it comes from the code removing those superfluous (in other languages) lines at the beginning and end (e.g. instead of the ugly

```c
int main()
{
}
```

: you can write a more reasonable

```c

int main()
{
}

```

: without getting empty lines at the beginning/end.
: Note that in Whitespace, every non-whitespace character is a comment, so you can simply write any non-whitespace stuff at the beginning/end:

```whitespace

code:


    end

```

: (note that this probably isn't a valid whitespace program; I just randomly added spaces/tabs).
: However, last friday, whitespace got nicely colored (tabs were bright red and spaces bright blue, IIRC). Now it's just white. Looking at the generated HTML, it seems classes "re2" (space) and "re3" (tab) are responsible for whitespace hilighting. Setting the background property for them should re-enable whitespace highlighting. --[[User:Ce|Ce]] 11:57, 26 January 2009 (UTC)
:: I've now found a trick to get Whitespace highlighting without any (visible) text around it: Use the Unicode character U+FEFF (ZERO WIDTH NO-BREAK SPACE) to mark the start/end of the code:

```whitespace







```

:: Note that U+FEFF renders as absolutely nothing, and is not whitespace as defined by Whitespace (although it ''is'' whitespace according to Unicode), and therefore should be ignored as "comment" by whitespace interpreters (I didn't test that, though). It apparently also isn't considered whitespace by the start/end line removing code, therefore it's not removed. --[[User:Ce|Ce]] 15:22, 26 January 2009 (UTC)

: Whitespace hightlighting now works as expected.  I updated the code in this section to reflect the new lang tag, and I'm amazed that it works, myself... --[[User:Short Circuit|Short Circuit]] 08:09, 30 January 2009 (UTC)

==Another problem with eating whitespace characters==
I just noted another problem with eating whitespace characters, which is related to the Whitespace language problem, but exists for all languages: If the first line is indented, the indentation is eaten. Example:

```cpp

  int i = 1;
  ++i;
  return i;

```

In the source, all lines are indented with two spaces. In the resulting page, the indentation of the first line is gone.

IMHO whitespace characters should only be eaten up to and including the first newline. I guess currently there's a regexp like <tt><nowiki>[ \t\n]*</nowiki></tt> for eating the initial whitespace. This should be changed to <tt><nowiki>[ \t]*\n</nowiki></tt>. --[[User:Ce|Ce]] 16:12, 26 January 2009 (UTC)
:Fixed.  GeshiCodeTag was feeding the source block through trim() prior to passing it on to GeSHi.  I removed the call to trim(). --[[User:Short Circuit|Short Circuit]] 22:00, 29 January 2009 (UTC)
::That introduces another "problem". People seem to like moving the lang tags to the lines before and after the code. Before it didn't add an extra line before and after, but now it does. Compare the cpp example above (with the tags separate from the code) to the code below (with the tags inline with code):
::
```cpp
  int i = 1;
  ++i;
  return i;
```

::It doesn't make much difference to me;I'll put the tags inline. It's just that other people (I forget who) thought it was better to separate the tags out.--[[User:Mwn3d|Mwn3d]] 22:34, 29 January 2009 (UTC)
::: I definitely prefer it that way. With code tags inline, you have basically two problems:
:::* First (and most important): It's very hard to see where the code section ends. This actually already caused one mistake during tag changing
:::* Second, the first line of code starts at a later column in the page source, which is not nice for editing code.
::: Also note that it's a difference to how pre works:

```txt

This is a separate line in pre tags

```

::: vs.
<lang>
This is a separate line in lang tags

```

::: Such inconsistencies make the tags harder to use. --[[User:Ce|Ce]] 08:28, 30 January 2009 (UTC)
:::: While I don't have any real PHP knowledge, I think passing through the following function where it was passed through trim should probably fix the probem:

```php

function prestyletrim($text) {
  return preg_replace("/(^[ \t]*\n|\n[ \t]*\$)/","",$text);
}

```

:::: --[[User:Ce|Ce]] 17:26, 30 January 2009 (UTC)

::::: Please do not forget this! --[[User:Ce|Ce]] 15:31, 25 March 2009 (UTC)
:::::: Consider it no longer forgotten. :-)  I believe I tried the code back in February, but it didn't work in some way. (I forget why not.)  I'm heading over to Qrush, Slawmaster and Mwn3d's place this weekend for a brief vacation, and I'll poke at it some more while I'm there. --[[User:Short Circuit|Short Circuit]] 18:41, 25 March 2009 (UTC)
::::::: Thanks. As I said, I don't really know PHP, so it's not really a surprise if it doesn't work correctly out of the box. Probably I missed some detail that any real PHP hacker would know ... --[[User:Ce|Ce]] 14:33, 26 March 2009 (UTC)

==D problem==
Check out [[RCSNUSP/D]] to see a double backslash problem in the D highlighting. --[[User:Mwn3d|Mwn3d]] 16:55, 28 January 2009 (UTC)

==Change in behavior for unsupported languages -- we have unity!==
I've modified GeshiCodeTag to "fail" silently on unsupported languages, wrapping the source code in a &lt;pre&gt; block, and prepending some HTML comments indicating that the language isn't supported. (The comments are there so that if you use a language you think is supported, and you don't see highighting, you can view selection source to double-check.  I might add a "similar language tags supported" bit to the comments to aid debugging.)

What this means is that virtually ''all'' code examples should now be wrapped in &lt;lang&gt; blocks, whether or not the highlighter supports them.  If highlighting is supported by a later version of GeSHi, code examples across the site will then be decorated automagically.  I just ask that each language try to standardize on a language ID, so that when support is added by GeSHi, I can simply alias our standardized ID to point to whatever ID GeSHi decided on upstream.--[[User:Short Circuit|Short Circuit]] 08:24, 30 January 2009 (UTC)

: I've just found that the "fail" on unsupported languages indeed fails, i.e. doesn't work correctly. See e.g. [[http://www.rosettacode.org/wiki/Apply_a_callback_to_an_Array#E]]. Looking at the generated HTML, you obviously inserted &lt;code&gt; tags instead of &lt;pre&gt; tags. --[[User:Ce|Ce]] 16:57, 30 January 2009 (UTC)


### Inconsistent content model

There's a problem with this fallback: if the language is recognized, then the content is treated literally (except for &lt;/lang>, of course), but if it is unrecognized, then it is treated as HTML. The following two examples are of &lt;lang foo&gt;abc &lt;fnord&gt; def&lt;/lang&gt; and &lt;lang c&gt;abc &lt;fnord&gt; def&lt;/lang&gt;:

```foo
abc <fnord> def>
```


```c
abc <fnord> def
```

This difference means that code examples containing &lt; or &amp; will stop displaying correctly if the specified language becomes supported.
I would prefer that everything be treated as HTML (as &lt;pre>, indenting, and the current unsupported-language behavior do) so that it's possible to insert markup in examples (e.g. hyperlinks in comments),
```
 is not a magic string impossible to include, and for consistency with most of the rest of HTML.
On the other hand, treating the content literally does have the advantage of making it easier to paste in examples containing &lt;s.
--[[User:Kevin Reid|Kevin Reid]] 00:36, 9 February 2009 (UTC)
: I can fix that  inconsistency by feeding the source code snippet through the PHP htmlentities or htmlspecialchars functions, so they show up literally.  Of course, if &lt;code&gt; obviates that need, I can use that instead of &lt;pre&gt;.  If I do that, though, any code example for an unsupported language already using an HTML entity as a workaround is going to break. --[[User:Short Circuit|Short Circuit]] 05:21, 9 February 2009 (UTC)

== Code tag special behaviour ==
Hi, I'm pretty positive that there are no more code tags which should be lang tags (I'm not 100% sure, of course, because there's no way to effectively search for that). Therefore I think the normal behaviour of HTML code tags should be restored as soon as possible. --[[User:Ce|Ce]] 08:53, 2 February 2009 (UTC)
: Normal code tag behavior restored.  It's no longer tied to the syntax highlighting engine in any way. --[[User:Short Circuit|Short Circuit]] 18:42, 2 February 2009 (UTC)

== OCaml comments ==
Hey, can someone highlight comments in OCaml, please? Thanks, --[[User:Spoon!|Spoon!]] 01:51, 5 February 2009 (UTC)

```ocaml
(* a comment *)
```

: More generally, can we get this for any language? "Pascal family" languages all use (* foo *) for comments, so adding it would add comment highlighting for lots of languages. --[[User:Mbishop|Mbishop]] 07:02, 5 February 2009 (UTC)
:: Non-trivial.  Doing something like that would require different languages to be able to inherit from other languages.  Doing something like that would very likely break compatibility with upgrades to the GeSHi engine.  It's be nice, though... --[[User:Short Circuit|Short Circuit]] 07:21, 5 February 2009 (UTC)
::: You sure it's not just the default color theme or whatever that doesn't highlight (* ... *) style comments? --[[User:Mbishop|Mbishop]] 15:44, 5 February 2009 (UTC)
: Looking at the source for the ocaml language file, it's being defined correctly according to GeSHi language file syntax.  I suspect an engine bug.  --[[User:Short Circuit|Short Circuit]] 07:21, 5 February 2009 (UTC)
:: Modula-3 comments should also be highlighted, as well as Pascal comments, but they are not. --[[User:Mbishop|Mbishop]] 15:44, 5 February 2009 (UTC)
::: Hehe.  A quick "view selection source" indicates you're correct; I'd assumed somebody had already checked that.  Colors needed to be added for the class "coMULTI". --[[User:Short Circuit|Short Circuit]] 17:09, 5 February 2009 (UTC)
::: Fixed. --[[User:Short Circuit|Short Circuit]] 17:00, 9 February 2009 (UTC)

==C# Break==

Take a look at [[99 Bottles of Beer]], [[C sharp]] break statement is not highlighted.
Someone can fix csharp.php file? --[[User:Guga360|Guga360]] 20:22, 15 February 2009 (UTC)
: It's in the language file as a keyword, but the HTML source of that snippet shows that it's not being given a CSS style.  I don't know what's going on; It might be an engine bug.  I plan to try switching to GeSHi's SVN HEAD some time today, so perhaps that engine bug has been fixed. --[[User:Short Circuit|Short Circuit]] 21:01, 15 February 2009 (UTC)
: Is this still an issue?  I didn't see the break keyword in the code example. --[[User:Short Circuit|Short Circuit]] 03:01, 3 April 2009 (UTC)

==C# List Comprehension==

Take a look at [[Yuletide Holiday]], [[C sharp]] "from", "select" and "where" are not highlighted.
--[[User:Guga360|Guga360]] 22:18, 2 April 2009 (UTC)
: Fixed, I think.  Can someone verify that that code example actually compiles? I want to make sure I'm not adding stuff to the language file that isn't supported by the language. If it's good, I'll send the revised file upstream to be included with the next GeSHi release. --[[User:Short Circuit|Short Circuit]] 03:06, 3 April 2009 (UTC)
:: Yes, it works.

==Highlighting of [[Tcl]]==
===Braces aren't comments===
Is it possible to change the highlighting of Tcl so that sequences where there is an open and close brace on the same line are not highlighted as (presumably) comments? This makes expressions and one-liners much more difficult to read than they otherwise would be. For example, this is a one line <tt>if</tt>:

```tcl
if {[incr $a] == [list $b $c]} {puts [$d $a]} {error "$e $a"}
```

It's probably best for “{…}” to be not treated specially at all. (At some point we could also do with updating the list of ”keywords”, but that's nothing like as important.) —[[User:Dkf|Dkf]] 09:07, 22 May 2009 (UTC)
:Thanks for fixing this. —[[User:Dkf|Donal Fellows]] 14:34, 17 June 2009 (UTC)

### Keywords

The current list of "keywords" for Tcl 8.6 (which is quite a bit longer than for previous versions) is:
:'''Normal Keywords:''' append apply bgerror break catch cd class close concat constructor continue copy define deletemethod destructor else elseif eof error eval exec exit export expr fblocked fconfigure fcopy fileevent filter finally flush for foreach format gets glob if incr join lappend lassign lindex linsert list llength load lrange lrepeat lreplace lreverse lsearch lset lsort mixin my next objdefine object on open parray pid puts pwd read regexp regsub rename renamemethod return scan seek self set socket source split subst superclass switch tell then throw time trap try unexport unload unset uplevel vwait while
:'''Function Definition Keywords:''' create forward method new proc
:'''Variable Definition Keywords:''' global upvar variable
:'''Compound Keywords:''' after array binary chan clock dde dict encoding file info interp namespace package prefix registry string trace update zlib
(With compound keywords, the word after the listed keyword should also be highlighted.) OK, they're not formally keywords, but they should be formatted like they are. —[[User:Dkf|Donal Fellows]] 09:58, 17 June 2009 (UTC)
:The following words are linkable to <code><nowiki>http://www.tcl.tk/man/tcl8.6/TclCmd/</nowiki>''blah''<nowiki>.htm</nowiki></code>:
::proc global upvar variable after append apply array bgerror binary break catch cd chan clock close concat continue dde dict encoding eof error eval exec exit expr fblocked fconfigure fcopy file fileevent flush for foreach format gets glob if incr info interp join lappend lassign lindex linsert list llength load lrange lrepeat lreplace lreverse lsearch lset lsort my namespace next open package parray pid prefix puts pwd read regexp registry regsub rename return scan seek self set socket source split string subst switch tell throw time trace try unload unset update uplevel vwait while zlib
:Alternatively go to <code><nowiki>http://wiki.tcl.tk/</nowiki>''blah''</code> for any identified keyword and, if the page isn't there now it soon will be... ;-) —[[User:Dkf|Donal Fellows]] 14:14, 17 June 2009 (UTC)

### Variables

A “$” followed by alphanumerics should be highlighted as a variable reference (if you highlight such things in other languages, of course). —[[User:Dkf|Donal Fellows]] 14:34, 17 June 2009 (UTC)

### Comments

The comment regexp should (probably) be:
 (?:^|[[{;])[ \t]*(#[^\n]*)
(However that is encoded in PHP, I don't know.) It doesn't handle multi-line comments but we're not really using those on RC anyway. —[[User:Dkf|Donal Fellows]] 14:34, 17 June 2009 (UTC)

==Java5 messed up==
Recently the highlighting for "java5" got messed up. For example:

```java5
String
```

It appears that some "span" tags got inserted in the middle of the URL for the link. --[[Special:Contributions/76.173.203.58|76.173.203.58]] 07:03, 14 June 2009 (UTC)
: Upgraded GeSHi last night.  Will look to see what changed in the java5 language file. --[[User:Short Circuit|Short Circuit]] 19:34, 14 June 2009 (UTC)
:: It seems to be fixed now. Thanks. --[[Special:Contributions/76.173.203.58|76.173.203.58]] 05:37, 18 June 2009 (UTC)

==Smalltalk oddness==
Take a look at [[Mode#Smalltalk]]... it appears a <tt>1/></tt> (after the "s := ")... editing, I can't see nothing special; if a put a generic lang, it does not appear. --[[User:ShinTakezou|ShinTakezou]] 13:55, 14 June 2009 (UTC)
: If I change "self" in "solf" (or whatever), the problem disappears, so the problem is about the tags for highlighting the special "self" word. --[[User:ShinTakezou|ShinTakezou]] 14:01, 14 June 2009 (UTC)
: The same happens even for other special words, like <tt>nil</tt>, cfr. e.g. [[Gnome sort#Smalltalk]]. --[[User:ShinTakezou|ShinTakezou]] 14:40, 14 June 2009 (UTC)

==APL==
It seems APL (APL2) in this page (and maybe more) is bad encoded: [[Mean#APL]]. (Editing the example, I can see the right symbols, likely UTF-8 encoded) --[[User:ShinTakezou|ShinTakezou]] 16:05, 14 June 2009 (UTC)
: Rather: the lang tag seems not suitable for APL, or at least it should be created a fake geshi APL descriptor specifying that APL source encoding is utf-8 rather than whatever... If it is possible to specify such an information (otherwise, we must use indentation for APL code rather than lang tag? ugly solution) --[[User:ShinTakezou|ShinTakezou]] 16:10, 14 June 2009 (UTC)

==Groovy also not copacetic==
Groovy highlighting is exhibiting a problem similar to the Java problem.

```groovy
Binding>
```

Just for the record, so that future generations will know what the heck we were talking about even after the bug is fixed, the above looks something like this when rendered:
<pre style="color:blue;">5.0%2Fdocs%2Fapi%2F">Binding
```

Maybe somebody has something against the JVM? (besides the usual, I mean) --[[User:Balrog|Balrog]] 03:29, 17 June 2009 (UTC)
: For some reason the updating of geshi seems to have messed up several languages; my current list has Java (fixed?), Smalltalk, Matlab, Groovy ... open eyes for more... --[[User:ShinTakezou|ShinTakezou]] 13:00, 17 June 2009 (UTC)
::Java isn't fixed. The tag "java" was never broken, but "java5" is the problem. Check it out:
::
```java5
this is a test String
```

::Which (for future generations) renders as:
::
```txt
this is a test 1.5.0/docs/api/java/lang/String.html">String
```

::--[[User:Mwn3d|Mwn3d]] 13:16, 17 June 2009 (UTC)
:::I hearby move that we ask [[User:Short Circuit|Short Circuit]] to back out the most recent geshi update if that's possible. Do we have a second? --[[User:Balrog|Balrog]] 21:43, 17 June 2009 (UTC)
:::Java 5 appears to be fixed. Groovy is still hosed. --[[User:Balrog|Balrog]] 00:26, 18 June 2009 (UTC)

::::From the moment I woke up this morning to a couple hours past when I was supposed to be at work, I've been working with the GeSHi and Tcl folks to get things fixed.  I now have GeSHi SVN commit access, and I appear to have been handed some degree of responsibility to intake and process new language files, as well as finish the langfile creation wizard they started working on at my behest a couple weeks ago.  I'm also planning on adding a JavaScript widget that allows the user to change the syntax highlighting CSS on the local side, so I can get better defaults for sitewide CSS.  Syntax highlighting on RC is about to improve significantly.
::::I apologize for the issues we've been having lately.  I'll roll back GeSHi this evening, and modify the tag extension to enable the old version of GeSHi for most users, and use GeSHi 1.0.x HEAD for anyone interested in helping test new languages and language support. (All anonymous visits would still see the old system, or at least some working intermediate revision.)  The enabling of devel version of GeSHi for any given account will have to be done by someone with Bureaucrat access or higher (not a lot of these right now, but if someone wants to volunteer for the role, email me and we'll discuss it.).
::::Oh, and for the record, I hate [[wp:Robert's Rules of Order]]...They always seem to create more problems than they solve. --[[User:Short Circuit|Short Circuit]] 03:32, 18 June 2009 (UTC)
:::::Just in case I haven't said that you're amazing lately, let me just say (for future generations) that you're amazing. --[[User:Balrog|Balrog]] 03:54, 18 June 2009 (UTC) (hoping that you're okay with emoticons :-)
:::::Hmmmm... I don't know if [[User:Short Circuit|Short Circuit]] has rolled back GeSHi yet (is there a way I could check?), but Groovy still has junk in the GeSHi rendering --[[User:Balrog|Balrog]] 17:00, 18 June 2009 (UTC)
::::::I thought I had, but I may have erred.  I'll have to double-check when I get home tonight.
