+++
title = "Category talk:J"
description = ""
date = 2016-02-26T21:47:39Z
aliases = []
[extra]
id = 2409
[taxonomies]
categories = []
tags = []
+++

== bytecode and interpreted ==

I have added both "bytecode" and "interpreted" execution models to J.  This is because bytecode is the available category which best describes how tacit code is represented, and interpreted is the available category which best describes how explicit code is represented in J602 (and, presumably, the upcoming versions).

--[[User:Rdm|Rdm]] 15:54, 22 April 2010 (UTC)

== Is J esoteric? ==

Can we call this language "esoteric"? I think any language where emoticons are mathematical instructions deserves that title. Some lines look like [[Befunge]]...like this one from [http://www.jsoftware.com/jwiki/Phrases/Arith the wiki]:
 p2=.(".@(,&'x') %&x: <:@(x.&^)@#) p
That line presumably does math. --[[User:Mwn3d|Mwn3d]] 07:31, 10 December 2007 (MST)
: I wouldn't call it "esoteric"...Esoteric languages, to my mind, are ones that make difficult even common tasks.  A look at [http://rosettacode.org/rosettacode/w/index.php?title=Quicksort#J J's quicksort] wouldn't lead me to that conclusion. --[[User:Short Circuit|Short Circuit]] 08:37, 10 December 2007 (MST)
:: Yeah, you're right. I guess I didn't look at enough examples. --[[User:Mwn3d|Mwn3d]] 08:53, 10 December 2007 (MST)
:Definitely not esoteric, though very hard to read unless you have learned the vocabulary and tacit programming idioms. I would encourage folks to annotate the examples here and also contribute well annotated examples to the Literate Programs wiki (see [[Help:Similar Sites]]). --[[User:IanOsgood|IanOsgood]] 13:11, 10 December 2007 (MST)
::: I was curious about that quoted line and so I went and looked it up -- it actually would not work in current versions of J (because <code>x.</code> was replaced with <code>x</code> in 1996 when J version 6 was released) - unless backwards compatibility is enabled, you should use <code>x</code> instead of <code>x.</code> to refer to the left argument. (That line of code was also commented out, on the [http://code.jsoftware.com/wiki/Phrases/Arith source page].) That said, the code this was extracted from would take a string like '1.25' or '1.25(25)' and return an exact fraction (5 divided by 4 for the first example and 124 divided by 99 for the second - treating the part in parenthesis as repeating infinitely). In this context, <code>x.</code> was the numeric base for the conversion (10 by default) and p was the number extracted from the parenthesis, and the above line of code would have dealt with the repeating fraction part of the number.  It would have calculated nothing meaningful for '1.25' and for '1.25(25)' its value would have been 25 divided by 99. --[[User:Rdm|Rdm]] 16:11, 22 April 2010 (UTC)

== Jedi (was Jers) ==

The "Jers" section could be maintained by the site if all the users would sign up and put a <nowiki>{{mylang|J|proficiency}} row in a {{mylangbegin}}...{{mylangend}}</nowiki> table. The users would show up in [[:Category:J User]] which is already on the language page. --[[User:Mwn3d|Mwn3d]] 13:17, 27 August 2009 (UTC)
:: Yes, but I'm not sure some of the Jers perceive themselves to be involved enough in RC to sign up.  Also, having the special section allows me to add value to the Jers list (like links to the J wiki).
I changed the designation of people-who-use-J to "Jedi" because it's more fun but I didn't change the earlier references here.  --[[User:DevonMcC|DevonMcC]] 10:51 16 July 2015 (UTC-5)

==Tasks==

Since language categories do not automatically link to their "need work" pages, here is a manual link: [[Reports:Tasks not implemented in J]]
:Er, they do. It's in the sidebar, "unimplemented tasks" --[[User:Kevin Reid|Kevin Reid]] 16:04, 29 August 2009 (UTC)
::oops, thank you.  [[User:Rdm|Rdm]] 22:25, 31 August 2009 (UTC)
:::It is probably worth noting that unimplemented tasks no longer appears in the sidebar. --[[User:Rdm|Rdm]] 18:37, 20 January 2010 (UTC)
:::: I thought that having those pages appear within their language categories would be sufficient, but it appears that turned out not to be the case.  Whoever wants to revert those changes, go ahead. I've been meaning to, but work has been insane all week, and I can't take the time to do much else that requires significant attention. --[[User:Short Circuit|Michael Mol]] 21:01, 20 January 2010 (UTC)
::::It does appear. It's just called "Solve a Task". --[[User:Mwn3d|Mwn3d]] 23:14, 20 January 2010 (UTC)

== Regarding the HouseStyle corrections ==

I don't think the straight / links work on pages that aren't in the main namespace (so they work on task pages, but not category pages). So I tried to set it up the way it looked like you wanted it. All the stuff I did is easily undoable if it's not what you want. It's a good idea though. Maybe we can work on standardizing {language}/HouseStyle pages so we can make a little template (for [http://www.mediawiki.org/wiki/Help:Templates#Usage substitution] instead of transclusion perhaps?). --[[User:Mwn3d|Mwn3d]] 15:22, 12 October 2009 (UTC)
:Yeah, I had trouble making the / link work, and in the end just gave up.  But you got it just right.  Thank you.  Regarding {language}/HouseStyle, I also think it would help the project, but before we make any decisions I suggest we wait for my experiment to mature a little.  It may go nowhere.  Of course we could also try it ad-hoc for other selected languages to speed up the process (the ones with the largest extant populations on RC would be good candidates). --[[User:DanBron|DanBron]] 15:33, 12 October 2009 (UTC) 
::PS: Does everyone else have to type the literal "<tt>--</tt>" before their four-tildes, or am I the only one whose signature lacks the dashes by default?  Or is this just a preference I haven't enabled? --[[User:DanBron|DanBron]] 15:33, 12 October 2009 (UTC)
:::Yeah I have to type the dashes. There's probably a way to set it up automatically in your preferences, but it's not too much of a hassle. --[[User:Mwn3d|Mwn3d]] 15:41, 12 October 2009 (UTC)
:::If you just click the signature button it should type the dashes for you. Requires JS. It's the second to last button on the toolbar. --[[User:Mwn3d|Mwn3d]] 17:25, 12 October 2009 (UTC)
::::Ah, thanks.  Trying it now.  --[[User:DanBron|DanBron]] 17:52, 12 October 2009 (UTC)

== GeSHi Highlighter for J ==

I've created a skeleton GeSHi template for J.
Hopefully it should at least handle single line comments and control words, but not being able to test it makes it hard to know.
 
I've added all the J vocab as symbols rather than keywords, but don't define styles for them. I've not tried to include any regex sections. I understand that files should be sent to MikeMol when ready but wondered if anyone else wanted to have a look at it and tweak it first?
--[[User:Tikkanz|Tikkanz]] 00:07, 10 November 2009 (UTC)

The template has now been tested and emailed to [[ShortCircuit]]. Hopefully in the near future there will be basic syntax highlighting for J on Rosetta Code. However there are quite a lot of J entries that either don't yet use the <tt>lang</tt> tags, or incorrectly use an uppercase rather than a lowercase J. I've made a start checking through the current J tasks updating the syntax where needed, if you check any please note them below so we can coordinate our efforts.  The correct syntax for the <tt>lang</tt> tags is:

<tt><nowiki>
```j></nowiki> one or more lines of code here <nowiki>
```
</nowiki></tt



###  Progress updating lang tags 

J tasks checked and updated so far include:

* All tasks under headings '''1, 9, A, B,''' and '''C'''.  --[[User:Tikkanz|Tikkanz]] 11:23, 12 November 2009 (UTC)
* All tasks under heading '''D'''. --[[User:Tikkanz|Tikkanz]] 05:08, 14 November 2009 (UTC)
You may want to reconsider doing this, since at the moment I'm working on a bot to correct lang tags for nearly every task and nearly every language (including J) on Rosetta Code. It won't be perfect, but it should get the majority of cases right. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 11:48, 14 November 2009 (UTC)

:Will it add tags where they don't alreadey exist, or just fix existing ones?--[[Special:Contributions/203.114.138.159|203.114.138.159]] 18:38, 14 November 2009 (UTC)
::The former. It's gotten rather smart. More details to follow soonish (probably before Monday, if not today). —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 19:16, 14 November 2009 (UTC)

== Testing a GeSHi template ==
I don't know J, but what's keeping you from testing your language definition? ——[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 01:04, 10 November 2009 (UTC)
:I suspect ignorance more than anything ;-). I imagine that it would probably involve installing and configuring a local PHP server? I've never worked with PHP or installed a PHP server before - not sure that I'm ready to go down that road! --[[User:Tikkanz|Tikkanz]] 05:11, 10 November 2009 (UTC)
::Oh, don't worry, it's easy. I don't know a word of PHP and I was able to test my Perl 6 language definition. You must already have a copy of GeSHi, so all you need is a PHP interpreter; if you're running a flavor of Unix, get it using your package manager, or if you're running Windows, [http://www.php.net/downloads.php get a binary here]. Then change to your GeSHi directory and create a new file there, <code>testgeshi.php</code>, with these contents:
<blockquote><blockquote><blockquote>
```php
<?php

include_once 'geshi.php'; 

$source = 'some test code';

$language = 'j';

$geshi = new GeSHi($source, $language);

echo '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en" dir="ltr">
	<head><title>GeSHi test</title><meta http-equiv="Content-Type" content="text/html; charset=utf-8" /></head><body>',
    $geshi->parse_code(),
    '</body></html>';

?>
```
</blockquote></blockquote></blockquote>
::Replace "some test code" with some test J code. (I assume you've named your language definition <code>j.php</code> and put it the directory <code>geshi</code>.) Then say <code>php testgeshi.php</code>, redirect the output to a file, and open the file in your favorite Web browser. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 12:56, 10 November 2009 (UTC)
:::Thanks a lot for that - it would be good to make this info available somewhere more prominently. Actually I just downloaded another template from the sourceforge repository, so don't have GeSHi installed yet, but may do so now that the road doesn't look so long and dark!--[[User:Tikkanz|Tikkanz]] 22:43, 10 November 2009 (UTC)
:::: I don't think any of the Rosetta Code: namespace is really protected, so feel free to make any alterations that clarify things; That namespace is mostly guidelines and walkthroughs. If you're really not sure or comfortable making a change, note it in the talk page, give people about '''48 hours''' to respond, and see where things go from there. --[[User:Short Circuit|Michael Mol]] 00:27, 11 November 2009 (UTC)
::::: When/if I get it to work I'll give it a shot, however my first try didn't work out. My understanding from the instructions above, was that I should be able to get PHP/GeSHi to process files without necessarily setting it up in conjunction with a web server. I downloaded the Windows binary for PHP, installed it using the default parameters, except I didn't pick any web server to configure it for. I downloaded GeSHi and extracted it to a folder. I created the <code>testgeshi.php</code> file in the same folder as the <code>geshi.php</code> file and copied my <code>j.php</code> file to <code>geshi\j.php</code> with the other language files. I then started a <code>cmd</code> session, made the folder containing <code>testgeshi.php</code> the current directory and then ran <code>php testgeshi.php > mytest.html</code>. The <code>mytest.html</code> is created but it is zero-length. The folder containing the php binaries (exe and dll) was added to the path by the PHP installer. Any suggestions?--[[User:Tikkanz|Tikkanz]] 21:22, 11 November 2009 (UTC)
:::::: It sounds like you did everything right; in particular, I can verify that you shouldn't need to deal with any Web servers. It's bizarre that you got no output at all, instead of, say, an error message at the very least. Possibly the culprit is that Windows doesn't have a real standard error stream; I really have no idea how <code>cmd.exe</code> works. Try a skeletal PHP script (<code>&lt;?php echo 'Hello, world!'; ?&gt;</code>) to verify that PHP is working; try highlighting a C program (with <code>$language = 'c';</code>) to verify that GeSHi is working; try running <code>php testgeshi.php</code> without redirecting to a file. Sorry if this doesn't help much; it's hard to debug this kind of thing through an asynchronous medium. Worse comes to worse, we could try meeting on IRC this weekend. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 23:09, 11 November 2009 (UTC)
::::::: In production PHP setups, error output is supposed to be disabled, to avoid tipping off malicious visitors about the internal organization of a site.  I believe current versions of MediaWiki disable error output by default.  See [http://www.php.net/manual/en/errorfunc.configuration.php#ini.error-reporting ini.error-reporting]. --[[User:Short Circuit|Michael Mol]] 02:42, 12 November 2009 (UTC)
:::::::: Yes it was the error output setting! Thanks for the tips. Have now got a script that seems to work OK for the basics (comments, control words, strings). Will email it on.--[[User:Tikkanz|Tikkanz]] 07:22, 12 November 2009 (UTC)
