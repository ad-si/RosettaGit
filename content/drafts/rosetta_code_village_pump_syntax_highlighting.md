+++
title = "Rosetta Code:Village Pump/Syntax highlighting"
description = ""
date = 2017-05-13T14:21:43Z
aliases = []
[extra]
id = 4402
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Syntax Highlighting
|summary=Discuss issues related to the Syntax Highlighting system here.  The old page got huge, and it became hard to discern what problems were current.
}}
For a prompt reply, please report [http://rosettacode.org/geshi/ AutoGeSHi] issues at [[Rosetta Code:AutoGeSHi]], not here. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]])



=Recent changes=
== 2011/07/17 - Bug fixing and Snippet fetching==
Because of some trouble with notices in one language file (already found to be Pari/GP) there hase been a language file update which besides the PariGP language also included an update to D and some other changes not yet deployed earlier.

Also, with the consent of [[User:Short Circuit|Michael Mol]] I added a small snippet within GeSHi to create an sorted dump of all snippets rendered by GeSHi. This is: All source present here on RC will be dumped in plain text into folders named by the language file they were rendered with. The data gathered within this directory only contains the raw source, not the highlighted data and will be used for a script to auto-detect source languages (The results will hopefully be included in one of the next GeSHi releases). This snippet gathering process will probably run for about a week. The data I collect during this time will be available via GFDL v1.2 to comply with the licensing terms of this Wiki.

More details on the results will be available on my blog once I'm done with the language detection/processing of the gathered code samples. I'll drop a note here.

--[[User:BenBE|BenBE]] 10:00, 17 July 2011 (UTC)

:The snippet fetching has been completed and resulted in about 30k of code snippets in all languages used for highlighting on RosettaCode which are about 100 languages. Most samples are highlighted as text (about 70%), but even then this gives a really nice code base for the project I'm working on. The collection will be available soon under the terms of the GFDL v1.2. More information will be available [http://blog.benny-baumann.de/ in my blog] or on the RosettaCode Planet. :There will be a project related to GeSHi which I'll detail there too.

:The snippet fetching has been removed from the RC site again as the purpose for which I added it has ben fulfilled. Stay tuned for updates!

:--[[User:BenBE|BenBE]] 22:37, 3 August 2011 (UTC)

== 2011/07/05 - GeSHi update to GeSHi 1.0.8.11pre1==
Intermediate update of the GeSHi files to include some new languages. Affected languages should include: Haskell, LDIF, MMIX, PHP, Pig, Spark, Stonescript, UPC. Well, and maybe others. I'm not Sure.

== 2010/05/23 - GeSHi update to GeSHi 1.0.8.8==
I just updated GeSHi on the server to the latest released version 1.0.8.8. There are some new languages in this release and quite a bunch of changes to other language files. Please check everything and report here as usual. --[[User:BenBE|BenBE]] 17:12, 23 May 2010 (UTC)

== 2010/04/25 ==
Uploaded a language file update for Bash, GDB, Icon/Unicon, MagikSF, Matlab, Oz, Powershell, Q and VIM. The language files for E and Algol68 have not been uploaded/updated yet - due to some issues that need to be fixed beforehand. --[[User:BenBE|BenBE]] 21:39, 25 April 2010 (UTC)

== 2010/03/01 ==
Uploaded a language file update for J by Ric that changes some behaviour with the way keywords are handled and fixes a bug with some comments being ignored, although they should get highlighted. --[[User:BenBE|BenBE]] 23:31, 1 March 2010 (UTC)

== 2010/02/17 - GeSHi update to GeSHi 1.0.8.7==
I just uploaded the latest GeSHi release 1.0.8.7 to the server. In addition this includes some new language files for 1.0.8.8 that aren't publically available as part of a release. As Usual: Contact me about issues with the highlighting. --[[User:BenBE|BenBE]] 20:42, 17 February 2010 (UTC)
:Which languages are added for this version? Do you have a l list or a link to release notes? --[[User:Mwn3d|Mwn3d]] 22:01, 17 February 2010 (UTC)
:: I don't know which were added, but there's always [[Rosetta Code:Village Pump/Syntax Highlighting#Language_tags|this]]. --[[User:Short Circuit|Michael Mol]] 23:59, 17 February 2010 (UTC)
:: You can find a list of changes between releases in the [http://geshi.svn.sourceforge.net/svnroot/geshi/trunk/geshi-1.0.X/src/docs/CHANGES CHANGELOG]. That also lists added languages. --[[User:BenBE|BenBE]] 23:38, 20 February 2010 (UTC)

== 2010/01/06 ==
* Updated language files: c, c_mac, cpp, cpp-qt, clojure, erlang, lisp, java5, prolog
* Parser Changes: Generally allowed .. before plain integers. Affects at least: delphi, modula3, pascal, perl and maybe some more.
** Just fixed the changed regexp as I forgot to escape two chars which made the number highlighting behave somewhat unexpectedly --[[User:BenBE|BenBE]] 17:25, 7 January 2010 (UTC)

== 2009/12/25 - GeSHi update to GeSHi 1.0.8.6==
Hi folks,

I'm helping out [[User:Short Circuit|Short Circuit]] with the Server and the GeSHi installation that drives the syntax highlighting of this Wiki. I therefore updated the installation of GeSHi on the RC server to the latest official release - version 1.0.8.6.

I hope nothing horribly went wrong during the update. If you notice any problems please notify me here on the Wiki Page or upstream by mail.

I'll try to work through the (highlighting-related) issues ASAP, though this might take some time.

--[[User:BenBE|BenBE]] 01:11, 26 December 2009 (UTC)
:It doesn't seem to have taken (yet?). Look at the listing in the [[#Language tags]] section below. It still says 1.0.8.3.--[[User:Mwn3d|Mwn3d]] 01:37, 26 December 2009 (UTC)
:: Caching, most likely.  I just '''touch'''ed the MediaWiki configuration file, so it should expire all the caches. Caches may last up to 24 hours. --[[User:Short Circuit|Michael Mol]] 07:38, 26 December 2009 (UTC)


### Relationship Between Rosetta Code and GeSHi


Rosetta Code first started using GeSHi for syntax highlighting a long while back, but due to our nature, we quickly discovered, and frequently continue to discover, programming languages which GeSHi does not provide highlighting functionality.  Additionally, we've uncovered bugs in various releases of the software, and the GeSHi folks have been welcoming of fixes sent to them.

Due to the way language support is managed in GeSHi, it's fairly trivial for someone who can follow PHP syntax to create a PHP file that adds support for the language of their choice.  GeSHi's support for [[Oberon-2]], [[Modula-3]] and [[AutoHotkey]] is the direct result of contributions by [[User:Mbishop|Mbishop]] and [[User:Tinku99|Tinku99]]. (If you like a language, and it doesn't appear to have syntax highlighting support, I strongly suggest you follow their lead. ;-) )

I am now also part of the GeSHi project focusing for now on adding and improving support for programming languages present on Rosetta Code. As a community of language aficionados and enthusiasts, Rosetta Code is a hotbed for opportunities for improving GeSHi, and improvement of GeSHi is extremely helpful for improving the readability code on Rosetta Code. --[[User:Short Circuit|Short Circuit]] 06:43, 18 June 2009 (UTC)

:So how can I add a language? I have made a GeSHi file for Vedit macro language. Where can I upload it? (I can not test it myself so I don't know if it is OK). I could do a file for RapidQ quite easily, too, since I have Vedit syntax file for it.
:It would be nice if there was an example GeSHi file downloadable somewhere (or perhaps even a few of them). The only one I found was at GeSHi web page. I had to cut and paste the source from the browser, and the do quite a lot editing to get newlines etc. fixed. --[[User:PauliKL|PauliKL]] 15:26, 30 June 2009 (UTC)
:: Email them to me at mikemol@gmail.com, and I'll get them tested, staged on RC and committed to SVN as quickly as I can. --[[User:Short Circuit|Short Circuit]] 17:06, 30 June 2009 (UTC)
:: Or upstream to BenBE at geshi (ddoott) org --[[User:BenBE|BenBE]] 01:15, 26 December 2009 (UTC)

If anyone else would like to make a language file, look at this explanation of the language file format from the GeSHi site: http://qbnz.com/highlighter/geshi-doc.html#language-files. --[[User:Mwn3d|Mwn3d]] 19:56, 8 September 2009 (UTC)

=Language tags=

Due to changes I want to implement in the way processing of the '''lang''' tag is done, we need to standardize codes for all of the languages currently on Rosetta Code ASAP.  Subsequently, all pages need to be scoured for code snippets.

Here is a list of the languages that currently exist on Rosetta code as I write this.  Next to each language, place the code for that language.  If the code is already established in GeSHi, make it '''bold'''.  If no code is provided by GeSHi, and no code is currently in use for that language on the wiki, make one up.  Once this list is fully populated, all the pages for each language need to be checked, and ensure that the code snippets for that language are correct. (Not sure what to do about command-line one-liners or similar yet, though.)

Note: AutoHotKey currently has two codes, ahk and autohotkey.  When AutoHotKey support was added to GeSHi, it was added with the longer code. (lang codes are derived from the filename.)  Since there were already code snippets on RC that used ahk for AutoHotKey, I created a symlink that allowed ahk to be used as a language code as well.  This should likely be reversed, meaning instances of the ahk tag need to be replaced with the autohotkey tag. --[[User:Short Circuit|Short Circuit]] 07:01, 18 June 2009 (UTC)

'''The list moved to [[Help:Syntax Highlighting]].'''

= Non-GeSHi-issues to take care of =
==Whitespace at program beginning/end==
I just wanted to make sure that this problem doesn't get forgotten now that the thread moved to the archive page. --[[User:Ce|Ce]] 23:18, 6 July 2009 (UTC)
: I think you're going to have to reiterate exactly what the problem is, and how it manifests itself under the current configuration. --[[User:Short Circuit|Short Circuit]] 23:50, 6 July 2009 (UTC)
:: In

```txt
<nowiki>

```cpp

int main {}

```

</nowiki>
```

:: there are additional blank lines,

```cpp

int main {}

```

:: which don't appear in

```txt
<nowiki>

```txt

int main()
&lt;/pre>
</nowiki>
```

:: which results in

```txt

int main()

```

:: Those lines shouldn't be there. Currently the only way to get rid of them is to put the beginning/end tag on the same line as the first/last line of the code, which makes editing (and especially modifying existing snippets) unnecessarily hard (especially it's ''easy'' to miss an end tag, since it's bolted onto the last line of the code). In addition, it's an inconsistency. The lang tags should work exactly like pre tags, except for syntax hilighting.
:: The old discussion is in [[Village Pump:Home/Syntax Highlighting ( archived 2009-06-18 )#Another problem with eating whitespace characters]] (note that the start of that discussion concerns an older state with a different problem; initially too much was removed). --[[User:Ce|Ce]] 10:42, 7 July 2009 (UTC)

::: Is there still hope that this will get fixed some day? --[[User:Ce|Ce]] 20:41, 7 October 2009 (UTC)
:::: I don't know.  The syntax highlighter was my top priority just before I got slammed with work months ago, and things are just settling down.  An update of the highlighter is definitely in order, but it's going to result in a significant number of breakages of things that currently work.  And I'm hoping for a change in some MW internals that may make the GeSHi work simpler.
::::
:::: In all honesty, I'm hopeful, but not expectant, of a solution that's going to work for the rest of the code examples, while at the same time working for the Whitespace language. --[[User:Short Circuit|Michael Mol]] 00:45, 8 October 2009 (UTC)

Test...


```cpp
int main {}
```

: Fix is simpler than it appeared.  Don't put newlines between the tags. :) --[[User:Short Circuit|Michael Mol]] 01:15, 8 October 2009 (UTC)

:: But that makes the page source worse. ''Especially'' it makes it easy to miss an end tag. (Yes, it already happened.) I ''strongly'' hope that one day, it will work correctly. --[[User:Ce|Ce]] 16:16, 10 October 2009 (UTC)
::: Yes, it does, but in the case of Whitespace, I think it's justified. Otherwise, you're putting language-significant information between the tags, and effectually asking the wiki to strip out some of your program. I don't see how that's better behavior. If copy/paste is necessary, ideally they would copy from the rendered wiki page, not the wiki source. --[[User:Short Circuit|Michael Mol]] 22:14, 10 October 2009 (UTC)
::: IMHO, the code begins ''in the line after the start tag'' and ends ''at the line before the end tag'' (putting it in the same line is just a hack to work around the bug). Therefore IMHO ''currently'' Whitespace is broken because the GeSHi adds ''extra'' newlines at the beginning/end. Also note that this is also how the HTML pre tag works, example:

```txt




```

::: As you see, a line break directly after the pre tag is removed.
::: One thing I previously got wrong it that this deletion is only if there's ''only'' a newline, any extra whitespace after the pre or before the /pre causes the newline not to be removed. But then, this should simplify the algorithm considerably: Instead of general trimming, one has only to check whether the first resp. last character of the text inside tags is a newline, and if so, remove it. --[[User:Ce|Ce]] 08:39, 11 October 2009 (UTC)
:::: Looking at the HTML source of the page, I now noticed several things:
:::: * GeSHi already uses pre tags internally, but:
:::: * it adds an <em>additional</em> &amp;nbsp; to the first and last (empty) line. This space is ''not'' in the source, and I'd therefore consider it a bug in its own right.
:::: * it replaces all newline characters by &lt;br /&gt; which is not only a waste of bandwidth (because pre already honors newline characters), but also defeats the pre tag handling of newlines at the beginning/end.
:::: So the fix would probably to just remove the two latter points.
:::: BTW, whitespace highlighting currently seems broken anyway, because it doesn't currently highlight newlines:

```whitespace

   <-_just_three_spaces
   <-_a_newline,_followed_by_three_more_spaces

```

:::: (my comments apply to what I think <em>should</em> be highlighted; from your interpretation, you'd expect two additional newlines at the beginning/end to be highlighted) Note that the extra spaces in the extra lines (although not highlighted) will break the whitespace code when doing copy/paste, even if using your interpretation. --[[User:Ce|Ce]] 09:03, 11 October 2009 (UTC)

It seems like it would be nice to have the lang tag work like the pre tag, but it may not be worth the work. An easy way to catch a lot (still not all) forgotten /lang tags would be to require a preview for all edits (I think it's a MW option). Everyone should preview anyway. And you're right the newline highlighting isn't showing up. It's even easier to see if you add a blank line between the text you showed:

```whitespace

   <-_just_three_spaces

   <-_a_newline,_followed_by_three_more_spaces

```
 I think they used to be red? Maybe that was tabs. --[[User:Mwn3d|Mwn3d]] 17:59, 11 October 2009 (UTC)
: Given the information I gathered lately (see my latest comment above), I don't think it would be much work. It would be just
:* Find the code which changes newline characters to br tags and remove it (my guess is that it's also the reason why whitespace highlighting is broken; probably the GeSHi highlighting code looks for newline characters and doesn't find them because they are replaced by br tags).
:* Find the code which adds the extra &amp;nbsp; at the beginning/end and remove it.
: I cannot imagine that taking a lot of time. --[[User:Ce|Ce]] 20:21, 11 October 2009 (UTC)
:: [http://rosettacode.org/resources/gct_rcode.phps Here's the hacked-up MediaWiki extension] RC is currrently using.  It differs fairly significantly from the original version due to fixes and issues that were brought up since its initial use here.  RC is currently using GeSHi 1.0.8.2.  The latest in the 1.0.x branch is 1.0.8.4, but we encountered significant breakage when I upgraded to that months ago, I had to roll it back.  I joined the GeSHi project with commit access, with the intention of adding languages, and finding and fixing issues, as well as running SVN HEAD on RC, but between ImplSearchBot issues and the StumbleUpon flood, my server (and remote backup target) at home suffering a [catastrophic hardware failure, my ''other'' computer's [http://www.youtube.com/watch?v=klwvstK6SPU screen flaking out], as well as time cramps relating to family, work emergencies running fairly continually since June, and personal health issues coming to a head in the past month, I ''really'' haven't had time.
::
:: The 1.0.x GeSHi branch is no longer under active development, and I don't know how painful the transition to the 1.1.x branch is going to be.  1.1.x was supposed to be released in August when I last had the time to talk to the other members of that project, and I don't know why it's still in alpha.
::
:: Opticron has made significant headway on the ISB replacement, but has slammed into a problem that may require filing a ticket with the MW folks.  I just finished getting my home server running again.  Once I've got my home server pulling daily site backups again, I should finally be able to turn my attention toward the syntax highlighting again.
::
:: As an aside, I don't think anything changed in the server software between when newlines were highlighted and when they weren't.  It may also be a bug brought on by a shift in browser usage. --[[User:Short Circuit|Michael Mol]] 01:31, 12 October 2009 (UTC)
::: I think that it's not a browser problem. Inspecting the generated HTML shows there's no span created around the line breaks.
::: BTW, the changing of newlines to br tags is in the second-to-last line in the linked file, http://rosettacode.org/resources/gct_rcode.phps<nowiki></nowiki> (i.e. in the return). Just replacing <code>str_replace("\n",'&lt;br />', $geshi->parse_code())</code> with <code>$geshi->parse_code()</code> should fix that part. However, that line also shows that my suspicion that this is responsible for the missing newline highlighting is wrong: It is only applied ''after'' highlighting.
::: I also see that you've added my previously suggested function prestyletrim; now it's also clear to me why it didn't work: You applied it after the parsing instead of before. But since I now recognized that the semantics would be wrong anyways, it should probably be removed. However, instead it may be modified to simply patch away the spurious &amp;nbsp; after the fact (the cleaner way would be not to generate it, but that seems to be in <code>$geshi->parse_code()</code>, which is not in the file you linked to).
::: I think changing prestyletrim to

```php

function prestyletrim($text) {
  return preg_replace("/^ /","",preg_replace("/ $/","",$text));
}

```

::: should work (I'm not completely sure because I'm no PHP programmer). This modified version should be called exactly where the original is called now (i.e. while it was the wrong place for the original version, it would be the right place for the new one).
::: Maybe if it works, the function should also be renamed.
::: BTW,. I now note that the extra empty line at the end does ''not'' appear on the PHP example, so adding the spurious &amp;nbsp; seems to be part of the programming language dependent code. --[[User:Ce|Ce]] 08:02, 12 October 2009 (UTC)
:::::The extra line didn't appear because the parser was in the middle of a literal string at the end of the example. I changed the text to what I think you might have meant to put and the blank line shows up. --[[User:Mwn3d|Mwn3d]] 12:11, 12 October 2009 (UTC)
:::::: Ah, thanks. However, you mis-fixed it (having the function call twice was intentional; probably it could have been done with one function call and a better regexp, but I chose the easy way, which didn't require me to dig up PHP regex info from the net again). I now replaced with the correctly corrected version (and at the same time also fixed another, unrelated bug in the function). --[[User:Ce|Ce]] 16:28, 12 October 2009 (UTC)

== OCaml for Standard ML? ==

It has been suggested to use the OCaml syntax highlighting for Standard ML, and some SML code has been changed to OCaml highlighting already. While the languages are similar, they have many differences in keywords and stuff, and I feel that Standard ML should have a separate highlighting scheme. For example, SML has "datatype" keyword and OCaml does not; logical operators are "andalso" and "orelse" instead of "&&" and "||"; pattern matching is "case ... of" instead of "match ... with"; the "fn" keyword; and lots of other stuff. If I have time I could try to translate the OCaml GeSHi language file into SML; but I am reluctant to do so as I do not own a copy of the Definition of Standard ML, and so I am not confident I will get everything. --[[Special:Contributions/76.91.63.71|76.91.63.71]] 08:22, 19 July 2009 (UTC)

:Sounds reasonable. Grepping around, I find that the list of keywords is this:
::<code>and abstype as case datatype else end eqtype exception do fn fun functor funsig handle if in include infix infixr lazy let local nonfix of op open overload raise rec sharing sig signature struct structure then type val where while with withtype orelse andalso</code>
:This list was extracted directly from the lexer's keyword table in the source to the SML/NJ implementation, so it should be complete and accurate. I've not categorized their meaning at all. —[[User:Dkf|Donal Fellows]] 13:46, 19 July 2009 (UTC)
::If the languages really are that similar, copy the language file, adjust it for any differences, and send it to me.  I'll see that it gets added in as a proper language (and winds up in GeSHi upstream, as well.) --[[User:Short Circuit|Short Circuit]] 14:47, 20 July 2009 (UTC)

== Non-ASCII characters and unrecognized languages ==
Look at the below, then look at the source markup of this section.

A smiley face: ☺


```haskell
(☺) :: Mood
```



```txt
Have a ☺ nice day!
```



```ratatouille
Have a ☺ nice day!
```


The first three smiley faces come through fine, but the last is somehow corrupted. It appears that in general, if GeSHi doesn't recognize a language name, any non-ASCII characters between the lang tags get screwed up. I noticed this bug when using non-ASCII operators in Perl 6 examples. —[[User:Underscore|Underscore]] 00:19, 7 November 2009 (UTC)
: I can <em>probably</em> fix that by changing my "not found" code path when it looks at languages.  It's using a PHP character-escaping builtin that's not Unicode-safe, as a brute method of guarding against code injection. --[[User:Short Circuit|Michael Mol]] 23:48, 14 November 2009 (UTC)
:For the record, this bug is now fixed. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 01:25, 2 December 2009 (UTC)

= Solved issues =

== Prolog ==
<blockquote>
The Prolog highlighter is emitting garbage instead of hyperlinks.

```prolog
write(X)
```

—[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 13:49, 1 January 2010 (UTC)</blockquote>
: This issue has been fixed on the server and upstream. Missed to forbid one character after variables. --[[User:BenBE|BenBE]] 22:37, 1 January 2010 (UTC)

==C syntax highlight==
I've noticed that the "string" word is highlighted as a type in C; it shouldn't be so. --[[User:ShinTakezou|ShinTakezou]] 23:00, 3 July 2009 (UTC)
: Removed string from plain C and C for Mac language files; yet added all the Standard Integer types from stdint.h instead. --[[User:BenBE|BenBE]]

Small task: For the C language file the section containing the standard function names like printf is awfully empty. I'd need some guys to fill it up ;-) Basically the list should contain all the functions usually found in libc that ships twith the C compiler. --[[User:BenBE|BenBE]]

==Java5 Highlighting==
The java5 highlighting removes links to classes when generics are specified for them without a space between the class name and the <. Example:

```java5>LinkedList<T></lang

"LinkedList" should be a link. If you add a space the link shows up:

```java5>LinkedList <T></lang

I think a lot of people leave the space out so the highlighting should put a link in with or without the space. --[[User:Mwn3d|Mwn3d]] 12:35, 29 July 2009 (UTC)
: Should be fixed now --[[User:BenBE|BenBE]] 13:03, 2 January 2010 (UTC)

The java5 (and java) highlighting doesn't highlight javadoc comments as multi-line comments:

```java5
/**
* this is a comment
*/
```

: The style for co3 is missing. GeSHi itself already highlights it. --[[User:BenBE|BenBE]] 13:03, 2 January 2010 (UTC)
::Which style does it use for regular comments? I can copy that and change the color slightly to make sure it looks like there's a little bit of difference. --[[User:Mwn3d|Mwn3d]] 19:23, 7 January 2010 (UTC)
It also doesn't highlight "import static" lines properly (this should only be in java5:

```java5>import static SomeClass.someMethod;</lang

"import" and "static" should both be blue and bold and "SomeClass.someMethod" should be gray, bold, and italic. --[[User:Mwn3d|Mwn3d]] 13:34, 23 September 2009 (UTC)
: Wasn't supported until now. Added support for import static. Should work now --[[User:BenBE|BenBE]] 13:03, 2 January 2010 (UTC)
::It mostly works now. Look at this example:
::
```java5

Map<String, Integer>

```

::"String" should also be a link like it is in this example:
::
```java5

Map < String, Integer >

```

::--[[User:Mwn3d|Mwn3d]] 23:27, 5 January 2010 (UTC)
:::Done --[[User:BenBE|BenBE]] 00:20, 6 January 2010 (UTC)


= Known bugs with GeSHi Syntax Highlighting =

== Haskell syntax highlighting issues ==

* A syntax highlighting issue with single quotes can be seen here: [[Bitmap/Bézier_curves/Quadratic#Haskell]] and there: [[Conway's_Game_of_Life#Haskell]]

== Unix Shell syntax highlighting issues ==

* a problem with escaped backticks in strings appears here: [[Simple_database#UNIX_Shell]]


== OCaml syntax highlighting issues, '''bis''' ==

* there is a problem with strings containing double quotes as we can see here [[JSON#OCaml]]
* a similar problem occurs with a double quote inside single quotes (the char '"'): [http://rosettacode.org/mw/index.php?title=S-Expressions&oldid=123055#OCaml S-Expressions#OCaml]
* the same problem affects Pike, but it doesn't affect Python.
```ocaml
'"' ocaml
```

```pike
'"' pike
```

```python
'"' python
```


== OCaml syntax highlighting issues ==

:* I've added some Modules for now. For the star-ed Modules I'd need to have the Parent Module name or the Doc-Link that I should be generating. As each Group requires some overhead I'd suggest linking only those with at least 2 or 3 Submodules in it (e.g. Array) and simply highlighting (without Link) the others. Could you compile me that list? --[[User:BenBE|BenBE]] 00:29, 18 February 2010 (UTC)
:: Here are the full links of the star'ed modules:
 Array1   => http://caml.inria.fr/pub/docs/manual-ocaml/libref/Bigarray.Array1.html
 Array2   => http://caml.inria.fr/pub/docs/manual-ocaml/libref/Bigarray.Array2.html
 Array3   => http://caml.inria.fr/pub/docs/manual-ocaml/libref/Bigarray.Array3.html
 Genarray => http://caml.inria.fr/pub/docs/manual-ocaml/libref/Bigarray.Genarray.html
 Scanning => http://caml.inria.fr/pub/docs/manual-ocaml/libref/Scanf.Scanning.html
 State    => http://caml.inria.fr/pub/docs/manual-ocaml/libref/Random.State.html



* this type is currently linked to the url [1], which is wrong, the right url is [2]

```ocaml
file_descr
[1] http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html#TYPEfile_descr
[2] http://caml.inria.fr/pub/docs/manual-ocaml/libref/Unix.html#TYPEfile_descr

```



* I think that some links could be modified:

```ocaml

 int32      => http://caml.inria.fr/pub/docs/manual-ocaml/libref/Int32.html
 int64      => http://caml.inria.fr/pub/docs/manual-ocaml/libref/Int64.html
 nativeint  => http://caml.inria.fr/pub/docs/manual-ocaml/libref/Nativeint.html

```




* The parser brackets ('''[<''' and '''>]''') should be completely highlighted:
<blockquote>

```ocaml
parser [< e = parse_expr >]
```

(here the character '''[''' is colored but not '''<''')
</blockquote>
:* Tweaked in language file but needs CSS class sy0 to be set for this to show. --[[User:BenBE|BenBE]] 00:29, 18 February 2010 (UTC)
:: Unfortunately this sy0 CSS class is the same than when '''<''' appears alone (the ''lower than'' opperator), and here the '''[''' is CSS'ed in the same way than when it appears alone too. A different class for when they appear un pairs would be better ('''[<''' and '''>]'''). [[User:Blue Prawn|Blue Prawn]] 19:54, 18 August 2011 (UTC)



== Comments highlighting issues ==

* On the [[Comments]] page we can see some languages that use nested comments, here is the list that I can see:
** [[Comments#AppleScript]]
** [[Comments#Haskell]]
** [[Comments#Lua]]
** [[Comments#Modula-3]]
** [[Comments#Perl_6]]
** [[Comments#PHP]]
** [[Comments#REBOL]]
** [[Comments#REXX]]
** [[Comments#NetRexx]]
** [[Comments#ooRexx]]

==Regression in Tcl Syntax Highlighting==
I was just looking through some examples today (notably [[Miller-Rabin test#Tcl]]) and I saw that the Tcl highlighting seems to have regressed to how it was back when I started using this site. In particular, lots of expressions are now being marked like they are some kind of vaguely comment-like thing (I've not pried in depth what it actually thinks of them...) I distinctly remember complaining about this before and something was done about it – don't remember what – but now we've gone backwards. What happened? (No, I've been too busy recently with other things so I don't know ''when'' the change occurred.) I complain because it's ugly and harder to read than before. –[[User:Dkf|Donal Fellows]] 15:09, 2 February 2010 (UTC)
: Applied a small fix for now which you can see in the upstream GeSHi SVN repository (trunk). The file has been updated on RC too and should take effect soon. --[[User:BenBE|BenBE]] 03:29, 3 February 2010 (UTC)
=MATLAB problem=
MATLAB has HTML leaking into its links. Take a look at [[Ethiopian_multiplication#MATLAB]] for an example. --[[User:Mwn3d|Mwn3d]] 00:29, 1 April 2010 (UTC)
:Should be fixed locally for GeSHi; pending upload to RC server. --[[User:BenBE|BenBE]] 10:43, 24 April 2010 (UTC)
::k, just applied the GeSHi SVN patches on RC. Should take effect soon. --[[User:BenBE|BenBE]] 21:42, 25 April 2010 (UTC)

=ALGOL 68, AutoGeSHi=
I ran http://rosettacode.org/geshi/ and it generated a script: algol68.php-v1.0.8.8.4 is now in 1.0.8.9.

```php
<?php
/*************************************************************************************
 * algol68.php
 * --------
 * Author: Neville Dempsey (NevilleD.sourceforge@sgr-a.net)
 * Copyright: (c) 2010 Neville Dempsey (https://sourceforge.net/projects/algol68/files/)
 * Release Version: 1.0.8.9
 * Date Started: 2010/04/24
 *
 * ALGOL 68 language file for GeSHi.
 *
 * CHANGES
 * -------
 * 2010/04/24 (1.0.8.8.0)
 *   - First Release - machine generated by http://rosettacode.org/geshi/
 * 2010/05/24 (1.0.8.8.1)
 *   - #2324 - converted comment detection to RegEx
 * 2010/06/16 (1.0.8.8.2)
 *   - separate symbols from keywords - quick fix
 * 2010/06/16 (1.0.8.8.3)
 *   - reverse length order symbols
 *   - Add RegEx for BITS and REAL literals (INT to do)
 *   - recognise LONG and SHORT prefixes to literals
 * 2010/07/23 (1.0.8.8.4)
 *   - fix errors detected by langcheck.php, eg rm tab, fix indenting, rm duplicate keywords, fix symbols as keywords etc
 *   - removed bulk of local variables from name space.
 *   - unfolded arrays
 *
 * TODO (updated yyyy/mm/dd)
 * -------------------------
 *   - Use "Parser Control" to fix KEYWORD parsing, eg: (INT minus one= -1; print(ABSminus one))
 *   - Parse $FORMATS$ more fully - if possible.
 *   - Pull reserved words from the source of A68G and A68RS
 *   - Pull stdlib PROC/OP/MODE symbols from the soruce of A68G and A68RS
 *   - Pull PROC/OP/MODE extensions from the soruce of A68G and A68RS
 *   - Use RegEx to detect extended precision PROC names, eg 'long long sin' etc
 *   - Use RegEx to detect white space std PROC names, eg 'new line'
 *   - Use RegEx to detect white space ext PROC names, eg 'cgs speed of light'
 *   - Use RegEx to detect BOLD symbols, eg userdefined MODEs and OPs
 *   - Add REgEx for INT literals - Adding INT breaks formatting...
 *   - Adding PIPE as a key word breaks formatting of "|" symbols!!
 *
 *************************************************************************************
 *
 *     This file is part of GeSHi.
 *
 *   GeSHi is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   GeSHi is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with GeSHi; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 ************************************************************************************/

function a68_vars(){
    $pre='(?<![0-9a-z_\.])';
    $post='?(?![0-9a-z]|\.(?:[eE][+\-]?)?\d)';
    $post=""; # assuming the RegEx is greedy #

    $_="\s*";

    $srad="Rr";        $rrad="[".$srad."]";  # either one digit, OR opt-space in digits #
    $sbin="0-1";       $rbin="[".$sbin."]";  $_bin=$rbin."(?:[".$sbin."\s]*".$rbin."|)";
    $snib="0-3";       $rnib="[".$snib."]";  $_nib=$rnib."(?:[".$snib."\s]*".$rnib."|)";
    $soct="0-7";       $roct="[".$soct."]";  $_oct=$roct."(?:[".$soct."\s]*".$roct."|)";
    $sdec="0-9";       $rdec="[".$sdec."]";  $_dec=$rdec."(?:[".$sdec."\s]*".$rdec."|)";
    $shex="0-9A-Fa-f"; $rhex="[".$shex."]";  $_hex=$rhex."(?:[".$shex."\s]*".$rhex."|)";

    # Define BITS: #
    $prebits=$pre; $postbits=$post;
    $bl="2".$_.$rrad.$_.$_bin;
    $bl=$bl."|"."2".$_.$rrad.$_.$_bin;
    $bl=$bl."|"."4".$_.$rrad.$_.$_nib;
    $bl=$bl."|"."8".$_.$rrad.$_.$_oct;
    $bl=$bl."|"."1".$_."0".$_.$rrad.$_.$_dec;
    $bl=$bl."|"."1".$_."6".$_.$rrad.$_.$_hex;

    # Define INT: #
    $preint=$pre; $postint=$post;
    # for some reason ".0 e - 2" is not recognised, but ".0 e + 2" IS!
    # work around: remove spaces between sign and digits! Maybe because
    # of the Unary '-' Operator
    $sign_="(?:-|\-|[-]|[\-]|\+|)";  # attempts #

    $sign_="(?:-\s*|\+\s*|)"; # n.b. sign is followed by white space #

    $_int=$sign_.$_dec;
    $il=          $_int;                      # +_9           #

    $GESHI_NUMBER_INT_BASIC='(?:(?<![0-9a-z_\.%])|(?<=\.\.))(?<![\d\.]e[+\-])([1-9]\d*?|0)(?![0-9a-z]|\.(?:[eE][+\-]?)?\d)';

    # Define REAL: #
    $prereal=$pre; $postreal=$post;
    $sexp="Ee\\\\";   $_exp="(?:⏨|[".$sexp."])".$_.$_int;
    $_decimal="[.]".$_.$_dec;

    # Add permitted permutations of various parts #
    $rl=        $_int.$_.$_decimal.$_.$_exp; # +_9_._9_e_+_9 #
    $rl=$rl."|".$_int.$_."[.]".$_.$_exp;     # +_9_.___e_+_9 #
    $rl=$rl."|".$_int.$_.$_exp;              # +_9_____e_+_9 #
    $rl=$rl."|".$sign_.$_decimal.$_.$_exp;   # +___._9_e_+_9 #

    $rl=$rl."|".$_int.$_.$_decimal;          # +_9_._9       #
    $rl=$rl."|".$sign_.$_decimal;            # +___._9       #

    # The following line damaged formatting...
    #$rl=$rl."|".$_int;                       # +_9           #

    # Apparently Algol68 does not support '2.', c.f. Algol 68G
    #$rl=$rl."|".$_int.$_."[.]";             # +_9_.         #

    # Literal prefixes are overridden by KEYWORDS :-(
    $LONGS="(?:(?:(LONG\s+)*|(SHORT\s+))*|)";

    return array(
        "BITS" => $prebits.$LONGS."(?:".$bl.")".$postbits,
        "INT" => $preint.$LONGS."(?:".$il.")".$postint,
        "REAL" => $prereal.$LONGS."(?:".$rl.")".$postreal,

        "BOLD" => 'color: #b1b100; font-weight: bold;',
        "ITALIC" => 'color: #b1b100;', # procedures traditionally italic #
        "NONSTD" => 'color: #FF0000; font-weight: bold;', # RED #
        "COMMENT" => 'color: #666666; font-style: italic;'
    );
}
$a68=a68_vars();

$language_data = array(
    'LANG_NAME' => 'ALGOL 68',
    'COMMENT_SINGLE' => array(),
    'COMMENT_MULTI' => array(
        '¢' => '¢',
        '£' => '£',
        '#' => '#',
        ),
    'COMMENT_REGEXP' => array(
        1 => '/\bCO((?:MMENT)?)\b.*?\bCO\\1\b/i',
        2 => '/\bPR((?:AGMAT)?)\b.*?\bPR\\1\b/i',
        3 => '/\bQUOTE\b.*?\bQUOTE\b/i'
        ),
    'CASE_KEYWORDS' => GESHI_CAPS_NO_CHANGE,
    'QUOTEMARKS' => array('"'),
    'ESCAPE_CHAR' => '"',
    'NUMBERS' => GESHI_NUMBER_HEX_SUFFIX,  # Warning: Feature!! #
#                GESHI_NUMBER_HEX_SUFFIX, # Attempt ignore default #
    'KEYWORDS' => array(
# Extensions
        1 => array('KEEP', 'FINISH', 'USE', 'SYSPROCS', 'IOSTATE', 'USING', 'ENVIRON', 'PROGRAM', 'CONTEXT'),
#        2 => array('CASE', 'IN', 'OUSE', 'IN', 'OUT', 'ESAC', '(', '|', '|:', ')', 'FOR', 'FROM', 'TO', 'BY', 'WHILE', 'DO', 'OD', 'IF', 'THEN', 'ELIF', 'THEN', 'ELSE', 'FI', 'PAR', 'BEGIN', 'EXIT', 'END', 'GO', 'GOTO', 'FORALL', 'UPTO', 'DOWNTO', 'FOREACH', 'ASSERT'), #
        2 => array('CASE', 'IN', 'OUSE', /* 'IN',*/ 'OUT', 'ESAC', 'PAR', 'BEGIN', 'EXIT', 'END', 'GO TO', 'GOTO', 'FOR', 'FROM', 'TO', 'BY', 'WHILE', 'DO', 'OD', 'IF', 'THEN', 'ELIF', /* 'THEN',*/ 'ELSE', 'FI' ),
        3 => array('BITS', 'BOOL', 'BYTES', 'CHAR', 'COMPL', 'INT', 'REAL', 'SEMA', 'STRING', 'VOID'),
        4 => array('MODE', 'OP', 'PRIO', 'PROC', 'FLEX', 'HEAP', 'LOC', 'REF', 'LONG', 'SHORT', 'EITHER'),
# Extensions or deprecated keywords
# 'PIPE': keyword somehow interferes with the internal operation of GeSHi
        5 => array('FORALL', 'UPTO', 'DOWNTO', 'FOREACH', 'ASSERT', 'CTB', 'CT', 'CTAB', 'COMPLEX', 'VECTOR', 'SOUND' /*, 'PIPE'*/),
        6 => array('CHANNEL', 'FILE', 'FORMAT', 'STRUCT', 'UNION', 'OF'),
# '(', '|', '|:', ')',  #
#        7 => array('OF', 'AT', '@', 'IS', ':=:', 'ISNT', ':/=:', ':≠:', 'CTB', 'CT', '::', 'CTAB', '::=', 'TRUE', 'FALSE', 'EMPTY', 'NIL', '○', 'SKIP', '~'),
        7 => array('AT', 'IS', 'ISNT', 'TRUE', 'FALSE', 'EMPTY', 'NIL', 'SKIP'),
        8 => array('NOT', 'UP', 'DOWN', 'LWB', 'UPB', /* '-',*/ 'ABS', 'ARG', 'BIN', 'ENTIER', 'LENG', 'LEVEL', 'ODD', 'REPR', 'ROUND', 'SHORTEN', 'CONJ', 'SIGN'),
# OPERATORS ordered roughtly by PRIORITY #
#       9 => array('¬', '↑', '↓', '⌊', '⌈', '~', '⎩', '⎧'),
#        10 => array('+*', 'I', '+×', '⊥', '!', '⏨'),
        10 => array('I'),
#        11 => array('SHL', 'SHR', '**', 'UP', 'DOWN', 'LWB', 'UPB', '↑', '↓', '⌊', '⌈', '⎩', '⎧'),
        11 => array('SHL', 'SHR', /*'UP', 'DOWN', 'LWB', 'UPB'*/),
#        12 => array('*', '/', '%', 'OVER', '%*', 'MOD', 'ELEM', '×', '÷', '÷×', '÷*', '%×', '□', '÷:'),
        12 => array('OVER', 'MOD', 'ELEM'),
#        13 => array('-', '+'),
#        14 => array('<', 'LT', '<=', 'LE', '>=', 'GE', '>', 'GT', '≤', '≥'),
        14 => array('LT', 'LE', 'GE', 'GT'),
#        15 => array('=', 'EQ', '/=', 'NE', '≠', '~='),
        15 => array('EQ', 'NE'),
#        16 => array('&', 'AND', '∧', 'OR', '∨', '/\\', '\\/'),
        16 => array('AND', 'OR'),
        17 => array('MINUSAB', 'PLUSAB', 'TIMESAB', 'DIVAB', 'OVERAB', 'MODAB', 'PLUSTO'),
#        18 => array('-:=', '+:=', '*:=', '/:=', '%:=', '%*:=', '+=:', '×:=', '÷:=', '÷×:=', '÷*:=', '%×:=', '÷::=', 'MINUS', 'PLUS', 'DIV', 'MOD', 'PRUS'),
# Extensions or deprecated keywords
        18 => array('MINUS', 'PLUS', 'DIV', /* 'MOD',*/ 'PRUS', 'IS NOT'),
# Extensions or deprecated keywords
        19 => array('THEF', 'ANDF', 'ORF', 'ANDTH', 'OREL', 'ANDTHEN', 'ORELSE'),
# Built in procedures - from standard prelude #
        20 => array('int lengths', 'intlengths', 'int shorths', 'intshorths', 'max int', 'maxint', 'real lengths', 'reallengths', 'real shorths', 'realshorths', 'bits lengths', 'bitslengths', 'bits shorths', 'bitsshorths', 'bytes lengths', 'byteslengths', 'bytes shorths', 'bytesshorths', 'max abs char', 'maxabschar', 'int width', 'intwidth', 'long int width', 'longintwidth', 'long long int width', 'longlongintwidth', 'real width', 'realwidth', 'long real width', 'longrealwidth', 'long long real width', 'longlongrealwidth', 'exp width', 'expwidth', 'long exp width', 'longexpwidth', 'long long exp width', 'longlongexpwidth', 'bits width', 'bitswidth', 'long bits width', 'longbitswidth', 'long long bits width', 'longlongbitswidth', 'bytes width', 'byteswidth', 'long bytes width', 'longbyteswidth', 'max real', 'maxreal', 'small real', 'smallreal', 'long max int', 'longmaxint', 'long long max int', 'longlongmaxint', 'long max real', 'longmaxreal', 'long small real', 'longsmallreal', 'long long max real', 'longlongmaxreal', 'long long small real', 'longlongsmallreal', 'long max bits', 'longmaxbits', 'long long max bits', 'longlongmaxbits', 'null character', 'nullcharacter', 'blank', 'flip', 'flop', 'error char', 'errorchar', 'exp char', 'expchar', 'newline char', 'newlinechar', 'formfeed char', 'formfeedchar', 'tab char', 'tabchar'),
        21 => array('stand in channel', 'standinchannel', 'stand out channel', 'standoutchannel', 'stand back channel', 'standbackchannel', 'stand draw channel', 'standdrawchannel', 'stand error channel', 'standerrorchannel'),
        22 => array('put possible', 'putpossible', 'get possible', 'getpossible', 'bin possible', 'binpossible', 'set possible', 'setpossible', 'reset possible', 'resetpossible', 'reidf possible', 'reidfpossible', 'draw possible', 'drawpossible', 'compressible', 'on logical file end', 'onlogicalfileend', 'on physical file end', 'onphysicalfileend', 'on line end', 'onlineend', 'on page end', 'onpageend', 'on format end', 'onformatend', 'on value error', 'onvalueerror', 'on open error', 'onopenerror', 'on transput error', 'ontransputerror', 'on format error', 'onformaterror', 'open', 'establish', 'create', 'associate', 'close', 'lock', 'scratch', 'space', 'new line', 'newline', 'print', 'write f', 'writef', 'print f', 'printf', 'write bin', 'writebin', 'print bin', 'printbin', 'read f', 'readf', 'read bin', 'readbin', 'put f', 'putf', 'get f', 'getf', 'make term', 'maketerm', 'make device', 'makedevice', 'idf', 'term', 'read int', 'readint', 'read long int', 'readlongint', 'read long long int', 'readlonglongint', 'read real', 'readreal', 'read long real', 'readlongreal', 'read long long real', 'readlonglongreal', 'read complex', 'readcomplex', 'read long complex', 'readlongcomplex', 'read long long complex', 'readlonglongcomplex', 'read bool', 'readbool', 'read bits', 'readbits', 'read long bits', 'readlongbits', 'read long long bits', 'readlonglongbits', 'read char', 'readchar', 'read string', 'readstring', 'print int', 'printint', 'print long int', 'printlongint', 'print long long int', 'printlonglongint', 'print real', 'printreal', 'print long real', 'printlongreal', 'print long long real', 'printlonglongreal', 'print complex', 'printcomplex', 'print long complex', 'printlongcomplex', 'print long long complex', 'printlonglongcomplex', 'print bool', 'printbool', 'print bits', 'printbits', 'print long bits', 'printlongbits', 'print long long bits', 'printlonglongbits', 'print char', 'printchar', 'print string', 'printstring', 'whole', 'fixed', 'float'),
        23 => array('pi', 'long pi', 'longpi', 'long long pi', 'longlongpi'),
        24 => array('sqrt', 'curt', 'cbrt', 'exp', 'ln', 'log', 'sin', 'arc sin', 'arcsin', 'cos', 'arc cos', 'arccos', 'tan', 'arc tan', 'arctan', 'long sqrt', 'longsqrt', 'long curt', 'longcurt', 'long cbrt', 'longcbrt', 'long exp', 'longexp', 'long ln', 'longln', 'long log', 'longlog', 'long sin', 'longsin', 'long arc sin', 'longarcsin', 'long cos', 'longcos', 'long arc cos', 'longarccos', 'long tan', 'longtan', 'long arc tan', 'longarctan', 'long long sqrt', 'longlongsqrt', 'long long curt', 'longlongcurt', 'long long cbrt', 'longlongcbrt', 'long long exp', 'longlongexp', 'long long ln', 'longlongln', 'long long log', 'longlonglog', 'long long sin', 'longlongsin', 'long long arc sin', 'longlongarcsin', 'long long cos', 'longlongcos', 'long long arc cos', 'longlongarccos', 'long long tan', 'longlongtan', 'long long arc tan', 'longlongarctan'),
        25 => array('first random', 'firstrandom', 'next random', 'nextrandom', 'long next random', 'longnextrandom', 'long long next random', 'longlongnextrandom'),
        26 => array('real', 'bits pack', 'bitspack', 'long bits pack', 'longbitspack', 'long long bits pack', 'longlongbitspack', 'bytes pack', 'bytespack', 'long bytes pack', 'longbytespack', 'char in string', 'charinstring', 'last char in string', 'lastcharinstring', 'string in string', 'stringinstring'),
        27 => array('utc time', 'utctime', 'local time', 'localtime', 'argc', 'argv', 'get env', 'getenv', 'reset errno', 'reseterrno', 'errno', 'strerror'),
        28 => array('sinh', 'long sinh', 'longsinh', 'long long sinh', 'longlongsinh', 'arc sinh', 'arcsinh', 'long arc sinh', 'longarcsinh', 'long long arc sinh', 'longlongarcsinh', 'cosh', 'long cosh', 'longcosh', 'long long cosh', 'longlongcosh', 'arc cosh', 'arccosh', 'long arc cosh', 'longarccosh', 'long long arc cosh', 'longlongarccosh', 'tanh', 'long tanh', 'longtanh', 'long long tanh', 'longlongtanh', 'arc tanh', 'arctanh', 'long arc tanh', 'longarctanh', 'long long arc tanh', 'longlongarctanh', 'arc tan2', 'arctan2', 'long arc tan2', 'longarctan2', 'long long arc tan2', 'longlongarctan2'),
        29 => array('complex sqrt', 'complexsqrt', 'long complex sqrt', 'longcomplexsqrt', 'long long complex sqrt', 'longlongcomplexsqrt', 'complex exp', 'complexexp', 'long complex exp', 'longcomplexexp', 'long long complex exp', 'longlongcomplexexp', 'complex ln', 'complexln', 'long complex ln', 'longcomplexln', 'long long complex ln', 'longlongcomplexln', 'complex sin', 'complexsin', 'long complex sin', 'longcomplexsin', 'long long complex sin', 'longlongcomplexsin', 'complex arc sin', 'complexarcsin', 'long complex arc sin', 'longcomplexarcsin', 'long long complex arc sin', 'longlongcomplexarcsin', 'complex cos', 'complexcos', 'long complex cos', 'longcomplexcos', 'long long complex cos', 'longlongcomplexcos', 'complex arc cos', 'complexarccos', 'long complex arc cos', 'longcomplexarccos', 'long long complex arc cos', 'longlongcomplexarccos', 'complex tan', 'complextan', 'long complex tan', 'longcomplextan', 'long long complex tan', 'longlongcomplextan', 'complex arc tan', 'complexarctan', 'long complex arc tan', 'longcomplexarctan', 'long long complex arc tan', 'longlongcomplexarctan', 'complex sinh', 'complexsinh', 'complex arc sinh', 'complexarcsinh', 'complex cosh', 'complexcosh', 'complex arc cosh', 'complexarccosh', 'complex tanh', 'complextanh', 'complex arc tanh', 'complexarctanh')
        ),
    'SYMBOLS' => array(
        1 => array( /* reverse length sorted... */ '÷×:=', '%×:=', ':≠:', '÷*:=', '÷::=', '%*:=', ':/=:', '×:=', '÷:=', '÷×', '%:=', '%×', '*:=', '+:=', '+=:', '+×', '-:=', '/:=', '::=', ':=:', '÷*', '÷:', '↑', '↓', '∧', '∨', '≠', '≤', '≥', '⊥', '⌈', '⌊', '⎧', '⎩', /* '⏨', */ '□', '○', '%*', '**', '+*', '/=', '::', '/\\', '\\/', '<=', '>=', '|:', '~=', '¬', '×', '÷', '!', '%', '&', '(', ')', '*', '+', ',', '-', '/', ':', ';', '<', '=', '>', '?', '@', '[', ']', '^', '{', '|', '}', '~')
    ),
    'CASE_SENSITIVE' => array(
        GESHI_COMMENTS => false,
        1 => true,
        2 => true,
        3 => true,
        4 => true,
        5 => true,
        6 => true,
        7 => true,
        8 => true,
#        9 => true,
        10 => true,
        11 => true,
        12 => true,
#        13 => true,
        14 => true,
        15 => true,
        16 => true,
        17 => true,
        18 => true,
        19 => true,
        20 => true,
        21 => true,
        22 => true,
        23 => true,
        24 => true,
        25 => true,
        26 => true,
        27 => true,
        28 => true,
        29 => true
        ),
    'STYLES' => array(
        'KEYWORDS' => array(
            1 => $a68['NONSTD'], 2 => $a68['BOLD'], 3 => $a68['BOLD'], 4 => $a68['BOLD'],
            5 => $a68['NONSTD'], 6 => $a68['BOLD'], 7 => $a68['BOLD'], 8 => $a68['BOLD'],
            /* 9 => $a68['BOLD'],*/ 10 => $a68['BOLD'], 11 => $a68['BOLD'], 12 => $a68['BOLD'],
            /* 13 => $a68['BOLD'],*/ 14 => $a68['BOLD'], 15 => $a68['BOLD'], 16 => $a68['BOLD'], 17 => $a68['BOLD'],
            18 => $a68['NONSTD'], 19 => $a68['NONSTD'],
            20 => $a68['ITALIC'], 21 => $a68['ITALIC'], 22 => $a68['ITALIC'], 23 => $a68['ITALIC'],
            24 => $a68['ITALIC'], 25 => $a68['ITALIC'], 26 => $a68['ITALIC'], 27 => $a68['ITALIC'],
            28 => $a68['ITALIC'], 29 => $a68['ITALIC']
            ),
        'COMMENTS' => array(
            1 => $a68['COMMENT'], 2 => $a68['COMMENT'], 3 => $a68['COMMENT'], /* 4 => $a68['COMMENT'],
            5 => $a68['COMMENT'],*/ 'MULTI' => $a68['COMMENT']
            ),
        'ESCAPE_CHAR' => array(
            0 => 'color: #000099; font-weight: bold;'
            ),
        'BRACKETS' => array(
            0 => 'color: #009900;'
            ),
        'STRINGS' => array(
            0 => 'color: #0000ff;'
            ),
        'NUMBERS' => array(
            0 => 'color: #cc66cc;',
            ),
        'METHODS' => array(
            0 => 'color: #004000;',
            1 => 'color: #004000;'
            ),
        'SYMBOLS' => array(
            0 => 'color: #339933;',
            1 => 'color: #339933;'
            ),
        'REGEXPS' => array(
            0  => 'color: #cc66cc;',   # BITS #
            1  => 'color: #cc66cc;',   # REAL #
            /* 2  => 'color: #cc66cc;',   # INT # */
            ),
        'SCRIPT' => array()
        ),
    'URLS' => array(
        1 => '',
        2 => '',
        3 => '',
        4 => '',
        5 => '',
        6 => '',
        7 => '',
        8 => '',
#        9 => '',
        10 => '',
        11 => '',
        12 => '',
#        13 => '',
        14 => '',
        15 => '',
        16 => '',
        17 => '',
        18 => '',
        19 => '',
        20 => '',
        21 => '',
        22 => '',
        23 => '',
        24 => '',
        25 => '',
        26 => '',
        27 => '',
        28 => '',
        29 => ''
        ),
    'OOLANG' => true,
    'OBJECT_SPLITTERS' => array(
        0 => '→',
        1 => 'OF'
        ),
    'REGEXPS' => array(
        0 => $a68['BITS'],
        1 => $a68['REAL']
        # 2 => $a68['INT'], # Breaks formatting for some reason #
        # 2 => $GESHI_NUMBER_INT_BASIC # Also breaks formatting  #
    ),
    'STRICT_MODE_APPLIES' => GESHI_NEVER,
    'SCRIPT_DELIMITERS' => array(),
    'HIGHLIGHT_STRICT_BLOCK' => array()
);

unset($a68);
?>
```


Test ASCII version:

```algol68
INT sum sq := 0;
FOR i WHILE sum sq /= 70*70 DO
  sum sq +:= i**2
OD
```

Test Unicode version:

```algol68
INT sum sq := 0;
FOR i WHILE sum sq ≠ 70×70 DO
  sum sq +:= i↑2
OD
```


ThanX [[User:NevilleDNZ|NevilleDNZ]]

: Yeah, you're right; there need to be some followup instructions. Email to mikemol@rosettacode.org and benbe@geshi.org. --[[User:Short Circuit|Michael Mol]] 16:04, 23 April 2010 (UTC)
:: Whoops, fixed. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 14:57, 24 April 2010 (UTC)

> Doesn't hurt anything to have the code exposed, does it?

LOL - pinch me... I forgot I was posting to rosettacode.org. ;-)

Maybe we could/should start a page: [[GeSHi#ALGOL_68]] etc (with the php for GeSHi, and a specimen of [[GeSHi#ALGOL 68 - specimen]] unittest code to for checking the result.  Ideally any changers would be tested enough for importing directly/live into GeSHi...

BTW: The operators: '&amp;#8804;' and '&amp;#8805;' can be done in html as '&amp;le;' and '&amp;ge;' eg '&le;' and '&ge;';

There are a few of others: ne: &ne;, cent: &cent; pound: &pound; deg: &deg;, and: &and;, or: &or;, not: &not;, lceil: &lceil;, lfloor: &lfloor;, times: &times;, divide: &divide; larr: &larr;, rarr: &rarr;, uarr: &uarr;, darr: &darr;, and perp: &perp;  <!-- The Algol 68 Report used alefsym: &alefsym;, "&#x226E;", "&#x226F;" and &#x2112; http://en.wikipedia.org/wiki/Van_Wijngaarden_grammar -->. But no HTML entities for □, ○, ⎩, ⎧ (Unicode 5.1) and "⏨" (decimal exponentiation - Unicode 5.2).

Detecting all the HTML entity defs (http://php.net/manual/en/function.htmlentities.php) is probably the easiest way of doing things.

Trivia: Standard Algol 60 required sup: &sup;, equiv: &equiv;, "&#x2423;" and "&#x23E8;" (decimal exponentiation - Unicode 5.2); The Soviet Algol compiler - used for the Buran Space Shuttle reentry software - even had the character: loz: &loz;, together with support for &#xB0;, &#x2205;, &#xB1; and &#x2207; ... I could never figure out what this [http://en.wikipedia.org/wiki/Behenian_fixed_stars#Table_of_Behenian_Stars diamond &loz;] character was used for!!!  The Germans also included the unusual "<font size=5>&#x16ED;</font>" (iron/runic cross), I'm not convinced it was used for multiplication.  Probably something used for something mundane like printing train time tables, you would have to be German to understand. :-)

More Trivia: Most of the HTML entities appear to have arrived via Adobe fonts from an IBM Selectric typewriter font ball that was popular prior to ASCII 69 becoming a mandated US standard.  Some of the HTML entities are fairly weird.

Njoy [[User:NevilleDNZ|NevilleDNZ]] 05:43, 24 April 2010 (UTC)

:The HTML escapes in that language file will probably cause some trouble with GeSHi. Could someone send me an Unicode version of that file without those HTML escapes? --[[User:BenBE|BenBE]] 10:58, 24 April 2010 (UTC)

OK... I used mediawiki to do the conversion. :-) ... Will email U a copy too.

[[User:NevilleDNZ|NevilleDNZ]] 13:01, 24 April 2010 (UTC)

Revised Algol68.php (Quick fix: 1.0.8.8.2) to remove errant ">" that was being injected into in code before some symbols.

[[User:NevilleDNZ|NevilleDNZ]] 07:04, 15 June 2010 (UTC)

Revised Algol68.php (Better fix: 1.0.8.8.5) to remove errant ">" that was being injected into in code before some symbols.  Still some quirks, eg kw6, kw7, kw8 ... highlighting inserted, but not apparent. Mostly OK. Enjoy.

[[User:NevilleDNZ|NevilleDNZ]] 14:37, 30 July 2010 (UTC)

=F# issue=
Apostrophes (') have three different roles in F#:
* they enclose character literals, like 'A',  '\065', '\n' or '\u0041'
* type parameters always begin with an apostrophe
* apostrophes can be part of a normal identifier (but not in the first position)
At the moment, all apostrophes are interpreted as starting a character. See for example here: http://rosettacode.org/wiki/Doubly-linked_list/Definition#F.23

Maybe a work-around could be: Ignore apostrophes except
* if followed by exactly one arbitrary character and an additional apostrophe, like 'A'
* or if followed by a backslash, like the other types of char literals
[[User:Wmeyer|Wmeyer]] 18:51, 27 August 2010 (UTC)

=PL/I, AutoGeSHi=

I ran [http://rosettacode.org/geshi/autogeshi.pl?identifier=pli&LANG_NAME=Programming+Language+One&COMMENT_SINGLE=&COMMENT_MULTI=%2F*+*%2F&QUOTEMARKS=%22+%27&ESCAPE_CHAR=&KEYWORDS=abnormal+abs+acos+acosf+add+addbuff+addr+addrdata+alias+aligned+all+alloc+allocate+allocation+allocn+allocsize+any+anycondition+area+ascii+asin+asinf+asm+asmtdli+assembler+assignable+atan+atand+atanf+atanh+attach+attention+attn+auto+automatic+availablearea+backwards+based+begin+bigendian+bin+binary+binaryvalue+bind+binvalue+bit+bitloc+bitlocation+bkwd+blksize+bool+buf+buffered+buffers+bufnd+bufni+bufoff+bufsp+builtin+bx+by+byaddr+byte+byvalue+b4+call+cast+cds+ceil+center+centerleft+centerright+centre+centreleft+centreright+char+character+charg+chargraphic+charval+check+checkstg+close+cmpat+cobol+col+collate+column+comment+compare+compiledate+compiletime+completion+complex+cond+condition+conjg+conn+connected+consecutive+controlled+conv+conversion+copy+cos+cosd+cosf+cosh+count+counter+cpln+cplx+cs+cstg+ctl+ctlasa+ctl360+currentsize+currentstorage+data+datafield+date+datetime+days+daystodate+daystosecs+db+dcl+dec+decimal+declare+def+default+define+defined+delay+delete+descriptor+descriptors+detach+dft+dim+dimacross+dimension+direct+display+divide+do+downthru+edit+else+empty+end+endfile+endpage+entry+entryaddr+env+environment+epsilon+erf+erfc+error+event+excl+exclusive+exit+exp+expf+exponent+exports+ext+external+fb+fbs+fetch+file+fileddint+fileddtest+fileddword+fileid+fileopen+fileread+fileseek+filetell+filewrite+finish+first+fixed+fixedbin+fixeddec+fixedoverflow+float+floatbin+floatdec+floor+flush+fofl+format+fortran+free+from+fromalien+fs+gamma+generic+genkey+get+getenv+go+goto+graphic+gx+handle+hbound+hex+hexadec+heximage+high+huge+iand+ieee+ieor+if+ignore+imag+in+index+indexarea+indexed+init+initial+inline+inonly+inot+inout+input+int+inter+internal+into+invalidop+ior+irred+irreducible+isfinite+isigned+isinf+isll+ismain+isnan+isnormal+isrl+iszero+iunsigned+key+keyed+keyfrom+keylength+keyloc+keyto+label+last+lbound+leave+left+length+like+limited+line+lineno+linesize+linkage+list+littleendian+loc+locate+location+log+logf+loggamma+log10+log10f+log2+low+lowercase+lower2+maccol+maclmar+macname+macrmar+main+max+maxexp+maxlength+memconvert+memcu12+memcu14+memcu21+memcu24+memcu41+memcu42+memindex+memsearch+memsearchr+memverify+memverifyr+min+minexp+mod+mpstr+multiply+name+native+ncp+new+nocharg+nochargraphic+nocheck+nocmpat+noconv+noconversion+nodescriptor+noexecops+nofixedoverflow+nofofl+noinline+nolock+nomap+nomapin+nomapout+nonasgn+nonassignable+nonconnected+nonnative+noofl+nooverflow+norescan+normal+nosize+nostrg+nostringrange+nostringsize+nostrz+nosubrg+nosubscriptrange+noufl+nounderflow+nowrite+nozdiv+nozerodivide+null+offset+offsetadd+offsetdiff+offsetsubtract+offsetvalue+ofl+omitted+on+onarea+onchar+oncode+oncondcond+oncondid+oncount+onfile+ongsource+onkey+online+onloc+onoffset+onsource+onsubcode+onwchar+onwsource+open+optional+options+order+ordinal+ordinalname+ordinalpred+ordinalsucc+other+otherwise+outonly+output+overflow+package+packagename+page+pageno+pagesize+parameter+parmset+password+pending+pic+picspec+picture+places+pliascii+plicanc+plickpt+plidelete+plidump+pliebcdic+plifill+plifree+plimove+pliover+plirest+pliretc+pliretv+plisaxa+plisaxb+plisaxc+plisaxd+plisrta+plisrtb+plisrtc+plisrtd+plitdli+plitran11+plitran12+plitran21+plitran22+pointer+pointeradd+pointerdiff+pointersubtract+pointervalue+poly+pos+position+prec+precision+pred+present+print+priority+proc+procedure+procedurename+procname+prod+ptr+ptradd+ptrdiff+ptrsubtract+ptrvalue+put+putenv+quote+radix+raise2+random+range+rank+read+real+record+recsize+recursive+red+reducible+reentrant+refer+regional+reg12+release+rem+reorder+repattern+repeat+replaceby2+reply+reread+rescan+reserved+reserves+resignal+respec+retcode+return+returns+reuse+reverse+revert+rewrite+right+round+rounddec+samekey+scalarvarying+scale+search+searchr+secs+secstodate+secstodays+select+seql+sequential+serialize4+set+sign+signal+signed+sin+sind+sinf+sinh+sis+size+skip+snap+sourcefile+sourceline+sqrt+sqrtf+stackaddr+statement+static+status+stg+stmt+stop+storage+stream+strg+string+stringrange+stringsize+structure+strz+subrg+subscriptrange+substr+subtract+succ+sum+suppress+sysin+sysnull+sysparm+sysprint+system+sysversion+tally+tan+tand+tanf+tanh+task+then+thread+threadid+time+tiny+title+to+total+tpk+tpm+transient+translate+transmit+trim+trkofl+trunc+type+ufl+ulength+ulength16+ulength8+unal+unaligned+unallocated+unbuf+unbuffered+undefinedfile+underflow+undf+unlock+unsigned+unspec+until+update+upos+uppercase+upthru+usubstr+usurrogate+uvalid+uwidth+valid+validdate+value+var+varglist+vargsize+variable+varying+varyingz+vb+vbs+verify+verifyr+vs+vsam+wait+wchar+wcharval+weekday+when+whigh+while+widechar+wlow+write+xmlchar+y4date+y4julian+y4year+zdiv+zerodivide%0D%0A&SYMBOLS=%28%29%2B-*%2F%3D%3C%3E%26|%3A%3B%2C&OBJECT_SPLITTERS=.&author=Robert+AH+Prins&email=robert%40prino.org&url=http%3A%2F%2Fhitchwiki.org%2Fen%2FUser%3APrino]

And the result was rather sad, the age seems to have a problem with the rather large number of PL/I keywords...

Any suggestions on how to proceed? [[User:Prino|Prino]] 12:46, 8 February 2011 (UTC)
: I'll look into it. Won't be able to get to it sooner than Saturday, though. --[[User:Short Circuit|Michael Mol]] 17:29, 5 May 2011 (UTC)

: One possibility is to run it with a small number of keywords and edit the others in (not hard). [[User:CRGreathouse|CRGreathouse]] 18:37, 11 May 2011 (UTC)

= PARI/GP =
I created a GeSHi file for GP scripts (starting from an [http://rosettacode.org/geshi/ AutoGeSHi] skeleton and modifying as needed).  It is in the old 1.0 format rather than the new 1.1 format.  (If it's more convenient I can try to make a new format file, but I haven't found any documentation on it.)

There are a few things I'd like to iron out, if possible.  First of all, \ is an escape character only in special cases: \n, \t, \e, and \", I think.  This is important because it must be an escape in \" (or else strings will be mis-terminated) and it must not be an escape outside of strings (or else 5\2 will be misinterpreted as an error or at least something other than a number).  In other cases it is integer division.

Second, I would like to limit category 2 and 3 keywords.  Category 2 is for defaults, which appear only in the context "default(''foo'')" or "default(''foo'', bar)" (with optional spaces*).  Category 3 is types, which occur only after a colon following a variable.  Any idea on how to code these?  Also numeric constants are slightly unusual in GP; 1. is a valid t_REAL (it would be written 1.0 in C).  I don't know if that causes problems.

<nowiki>*</nowiki> Spaces are very unusual in GP; they're actually entirely ignored -- literally! -- by the parser outside of strings.  So <code>foo==f  oo</code> is true.  I think this issue is best handled by sticking out fingers in our ears and humming loudly.

Any advice would be appreciated.  I'd prefer to have this fixed before sending it to Michael Mol.

[[User:CRGreathouse|CRGreathouse]] 21:54, 11 May 2011 (UTC)

<div style="height:30ex;overflow:scroll">
```PHP
<?php
/*************************************************************************************
 * parigp.php
 * --------
 * Author: Charles R Greathouse IV (----)
 * Copyright: 2011 Charles R Greathouse IV (http://math.crg4.com/)
 * Release Version:
 * Date Started: 2011/05/11
 *
 * PARI/GP language file for GeSHi.
 *
 * CHANGES
 * -------
 *
 * TODO (updated yyyy/mm/dd)
 * -------------------------
 *
 *
 *************************************************************************************
 *
 *     This file is part of GeSHi.
 *
 *   GeSHi is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   GeSHi is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with GeSHi; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 ************************************************************************************/

$language_data = array(
    'LANG_NAME' => 'PARI/GP',
    'COMMENT_SINGLE' => array(1 => '\\\\'),
    'COMMENT_MULTI' => array('/*' => '*/'),
    'CASE_KEYWORDS' => GESHI_CAPS_NO_CHANGE,
    'QUOTEMARKS' => array('"'),
    'ESCAPE_CHAR' => '\\',
    'KEYWORDS' => array(
    1 => array('addprimes', 'bestappr', 'bezout', 'bezoutres', 'bigomega', 'binomial', 'chinese',
	'content', 'contfrac', 'contfracpnqn', 'core', 'coredisc', 'dirdiv', 'direuler', 'dirmul',
	'divisors', 'eulerphi', 'factor', 'factorback', 'factorcantor', 'factorff', 'factorial',
	'factorint', 'factormod', 'ffgen', 'ffinit', 'fflog', 'fforder', 'ffprimroot', 'fibonacci',
	'gcd', 'hilbert', 'isfundamental', 'ispower', 'isprime', 'ispseudoprime', 'issquare',
	'issquarefree', 'kronecker', 'lcm', 'moebius', 'nextprime', 'numbpart', 'numdiv', 'omega',
	'partitions', 'polrootsff', 'precprime', 'prime', 'primepi', 'primes', 'qfbclassno',
	'qfbcompraw', 'qfbhclassno', 'qfbnucomp', 'qfbnupow', 'qfbpowraw', 'qfbprimeform', 'qfbred',
	'qfbsolve', 'quadclassunit', 'quaddisc', 'quadgen', 'quadhilbert', 'quadpoly', 'quadray',
	'quadregulator', 'quadunit', 'removeprimes', 'sigma', 'sqrtint', 'stirling', 'sumdedekind',
	'zncoppersmith', 'znlog', 'znorder', 'znprimroot', 'znstar', 'Col', 'List', 'Mat', 'Mod',
	'Pol', 'Polrev', 'Qfb', 'Ser', 'Set', 'Str', 'Strchr', 'Strexpand', 'Strtex', 'Vec', 'Vecrev',
	'Vecsmall', 'binary', 'bitand', 'bitneg', 'bitnegimply', 'bitor', 'bittest', 'bitxor', 'ceil',
	'centerlift', 'component', 'conj', 'conjvec', 'denominator', 'floor', 'frac', 'imag', 'length',
	'lift', 'norm', 'norml2', 'numerator', 'numtoperm', 'padicprec', 'permtonum', 'precision',
	'random', 'real', 'round', 'simplify', 'sizebyte', 'sizedigit', 'truncate', 'valuation',
	'variable', 'ellL1', 'elladd', 'ellak', 'ellan', 'ellanalyticrank', 'ellap', 'ellbil',
	'ellchangecurve', 'ellchangepoint', 'ellconvertname', 'elldivpol', 'elleisnum', 'elleta',
	'ellgenerators', 'ellglobalred', 'ellgroup', 'ellheight', 'ellheightmatrix', 'ellidentify',
	'ellinit', 'ellisoncurve', 'ellj', 'elllocalred', 'elllog', 'elllseries', 'ellminimalmodel',
	'ellmodulareqn', 'ellorder', 'ellordinate', 'ellpointtoz', 'ellpow', 'ellrootno', 'ellsearch',
	'ellsigma', 'ellsub', 'elltaniyama', 'elltatepairing', 'elltors', 'ellweilpairing', 'ellwp',
	'ellzeta', 'ellztopoint', 'bnfcertify', 'bnfcompress', 'bnfdecodemodule', 'bnfinit',
	'bnfisintnorm', 'bnfisnorm', 'bnfisprincipal', 'bnfissunit', 'bnfisunit', 'bnfnarrow',
	'bnfsignunit', 'bnfsunit', 'bnrL1', 'bnrclassno', 'bnrclassnolist', 'bnrconductor',
	'bnrconductorofchar', 'bnrdisc', 'bnrdisclist', 'bnrinit', 'bnrisconductor', 'bnrisprincipal',
	'bnrrootnumber', 'bnrstark', 'dirzetak', 'factornf', 'galoisexport', 'galoisfixedfield',
	'galoisgetpol', 'galoisidentify', 'galoisinit', 'galoisisabelian', 'galoisisnormal',
	'galoispermtopol', 'galoissubcyclo', 'galoissubfields', 'galoissubgroups', 'idealadd',
	'idealaddtoone', 'idealappr', 'idealchinese', 'idealcoprime', 'idealdiv', 'idealfactor',
	'idealfactorback', 'idealfrobenius', 'idealhnf', 'idealintersect', 'idealinv', 'ideallist',
	'ideallistarch', 'ideallog', 'idealmin', 'idealmul', 'idealnorm', 'idealpow', 'idealprimedec',
	'idealramgroups', 'idealred', 'idealstar', 'idealtwoelt', 'idealval', 'matalgtobasis',
	'matbasistoalg', 'modreverse', 'newtonpoly', 'nfalgtobasis', 'nfbasis', 'nfbasistoalg',
	'nfdetint', 'nfdisc', 'nfeltadd', 'nfeltdiv', 'nfeltdiveuc', 'nfeltdivmodpr', 'nfeltdivrem',
	'nfeltmod', 'nfeltmul', 'nfeltmulmodpr', 'nfeltnorm', 'nfeltpow', 'nfeltpowmodpr',
	'nfeltreduce', 'nfeltreducemodpr', 'nfelttrace', 'nfeltval', 'nffactor', 'nffactorback',
	'nffactormod', 'nfgaloisapply', 'nfgaloisconj', 'nfhilbert', 'nfhnf', 'nfhnfmod', 'nfinit',
	'nfisideal', 'nfisincl', 'nfisisom', 'nfkermodpr', 'nfmodprinit', 'nfnewprec', 'nfroots',
	'nfrootsof1', 'nfsnf', 'nfsolvemodpr', 'nfsubfields', 'polcompositum', 'polgalois', 'polred',
	'polredabs', 'polredord', 'poltschirnhaus', 'rnfalgtobasis', 'rnfbasis', 'rnfbasistoalg',
	'rnfcharpoly', 'rnfconductor', 'rnfdedekind', 'rnfdet', 'rnfdisc', 'rnfeltabstorel',
	'rnfeltdown', 'rnfeltreltoabs', 'rnfeltup', 'rnfequation', 'rnfhnfbasis', 'rnfidealabstorel',
	'rnfidealdown', 'rnfidealhnf', 'rnfidealmul', 'rnfidealnormabs', 'rnfidealnormrel',
	'rnfidealreltoabs', 'rnfidealtwoelt', 'rnfidealup', 'rnfinit', 'rnfisabelian', 'rnfisfree',
	'rnfisnorm', 'rnfisnorminit', 'rnfkummer', 'rnflllgram', 'rnfnormgroup', 'rnfpolred',
	'rnfpolredabs', 'rnfpseudobasis', 'rnfsteinitz', 'subgrouplist', 'zetak', 'zetakinit', 'plot',
	'plotbox', 'plotclip', 'plotcolor', 'plotcopy', 'plotcursor', 'plotdraw', 'ploth', 'plothraw',
	'plothsizes', 'plotinit', 'plotkill', 'plotlines', 'plotlinetype', 'plotmove', 'plotpoints',
	'plotpointsize', 'plotpointtype', 'plotrbox', 'plotrecth', 'plotrecthraw', 'plotrline',
	'plotrmove', 'plotrpoint', 'plotscale', 'plotstring', 'psdraw', 'psploth', 'psplothraw', 'O',
	'deriv', 'diffop', 'eval', 'factorpadic', 'intformal', 'padicappr', 'padicfields',
	'polchebyshev', 'polcoeff', 'polcyclo', 'poldegree', 'poldisc', 'poldiscreduced',
	'polhensellift', 'polhermite', 'polinterpolate', 'polisirreducible', 'pollead', 'pollegendre',
	'polrecip', 'polresultant', 'polroots', 'polrootsmod', 'polrootspadic', 'polsturm',
	'polsubcyclo', 'polsylvestermatrix', 'polsym', 'poltchebi', 'polzagier', 'serconvol',
	'serlaplace', 'serreverse', 'subst', 'substpol', 'substvec', 'taylor', 'thue', 'thueinit',
	'break', 'for', 'fordiv', 'forell', 'forprime', 'forstep', 'forsubgroup', 'forvec', 'if',
	'next', 'return', 'until', 'while', 'Strprintf', 'addhelp', 'alarm', 'alias', 'allocatemem',
	'apply', 'default', 'error', 'extern', 'externstr', 'getheap', 'getrand', 'getstack',
	'gettime', 'global', 'input', 'install', 'kill', 'print1', 'print', 'printf', 'printtex',
	'quit', 'read', 'readvec', 'select', 'setrand', 'system', 'trap', 'type', 'version', 'warning',
	'whatnow', 'write1', 'write', 'writebin', 'writetex', 'divrem', 'lex', 'max', 'min', 'shift',
	'shiftmul', 'sign', 'vecmax', 'vecmin', 'derivnum', 'intcirc', 'intfouriercos', 'intfourierexp',
	'intfouriersin', 'intfuncinit', 'intlaplaceinv', 'intmellininv', 'intmellininvshort', 'intnum',
	'intnuminit', 'intnuminitgen', 'intnumromb', 'intnumstep', 'prod', 'prodeuler', 'prodinf',
	'solve', 'sum', 'sumalt', 'sumdiv', 'suminf', 'sumnum', 'sumnumalt', 'sumnuminit', 'sumpos',
	'Euler', 'I', 'Pi', 'abs', 'acos', 'acosh', 'agm', 'arg', 'asin', 'asinh', 'atan', 'atanh',
	'bernfrac', 'bernreal', 'bernvec', 'besselh1', 'besselh2', 'besseli', 'besselj', 'besseljh',
	'besselk', 'besseln', 'cos', 'cosh', 'cotan', 'dilog', 'eint1', 'erfc', 'eta', 'exp', 'gamma',
	'gammah', 'hyperu', 'incgam', 'incgamc', 'lngamma', 'log', 'polylog', 'psi', 'sin', 'sinh',
	'sqr', 'sqrt', 'sqrtn', 'tan', 'tanh', 'teichmuller', 'theta', 'thetanullk', 'weber', 'zeta',
	'algdep', 'charpoly', 'concat', 'lindep', 'listcreate', 'listinsert', 'listkill', 'listpop',
	'listput', 'listsort', 'matadjoint', 'matcompanion', 'matdet', 'matdetint', 'matdiagonal',
	'mateigen', 'matfrobenius', 'mathess', 'mathilbert', 'mathnf', 'mathnfmod', 'mathnfmodid',
	'matid', 'matimage', 'matimagecompl', 'matindexrank', 'matintersect', 'matinverseimage',
	'matisdiagonal', 'matker', 'matkerint', 'matmuldiagonal', 'matmultodiagonal', 'matpascal',
	'matrank', 'matrix', 'matrixqz', 'matsize', 'matsnf', 'matsolve', 'matsolvemod',
	'matsupplement', 'mattranspose', 'minpoly', 'qfgaussred', 'qfjacobi', 'qflll', 'qflllgram',
	'qfminim', 'qfperfection', 'qfrep', 'qfsign', 'setintersect', 'setisset', 'setminus',
	'setsearch', 'setunion', 'trace', 'vecextract', 'vecsort', 'vector', 'vectorsmall', 'vectorv'),
    2 => array('TeXstyle', 'breakloop', 'colors', 'compatible', 'datadir', 'debug', 'debugfiles',
	'debugmem', 'echo', 'factor_add_primes', 'factor_proven', 'format', 'graphcolormap',
	'graphcolors', 'help', 'histfile', 'histsize', 'lines', 'log', 'logfile', 'new_galois_format',
	'output', 'parisize', 'path', 'prettyprinter', 'primelimit', 'prompt_cont', 'prompt', 'psfile',
	'readline', 'realprecision', 'realprecision', 'recover', 'secure', 'seriesprecision',
	'simplify', 'strictmatch', 'timer'),
    3 => array('void', 'bool', 'negbool', 'small', 'int', 'real', 'mp', 'var', 'pol', 'vecsmall',
	'vec', 'list', 'str', 'genstr', 'gen', 'lg', 'typ')
    ),
    'SYMBOLS' => array(
        1 => array(
            '(', ')', '{', '}', '[', ']', '+', '-', '*', '/', '%', '=', '<', '>', '!', '^', '&', '|', '?', ';', ':', ','
            )
        ),
    'CASE_SENSITIVE' => array(
        GESHI_COMMENTS => false,
        1 => true, 2 => true
        ),
    'STYLES' => array(
        'KEYWORDS' => array(1 => 'color: #0000FF;', 2 => 'color: #00d2d2;', 3 => 'color: #ff8000;'
            ),
        'COMMENTS' => array(
            1 => 'color: #008000;',
            'MULTI' => 'color: #008000;'
            ),
        'ESCAPE_CHAR' => array(
            0 => 'color: #111111; font-weight: bold;'
            ),
        'BRACKETS' => array(
            0 => 'color: #009900;'
            ),
        'STRINGS' => array(
            0 => 'color: #800080;'
            ),
        'NUMBERS' => array(
            0 => 'color: #000000;',
            ),
        'METHODS' => array(
            0 => 'color: #004000;'
            ),
        'SYMBOLS' => array(
            1 => 'color: #339933;'
            ),
        'REGEXPS' => array(),
        'SCRIPT' => array()
        ),
    'URLS' => array(1 => '', 2 => ''),
    'OOLANG' => true,
    'OBJECT_SPLITTERS' => array(1 => '.'),
    'REGEXPS' => array(),
    'STRICT_MODE_APPLIES' => GESHI_NEVER,
	'SCRIPT_DELIMITERS' => array(),
    'HIGHLIGHT_STRICT_BLOCK' => array()
);

?>
```
</div>

: Update: Submitted to the [http://sourceforge.net/tracker/?func=detail&aid=3303485&group_id=114997&atid=670234 GeSHi issue tracker]. [[User:CRGreathouse|CRGreathouse]] 15:50, 18 May 2011 (UTC)

= GeShi power appears to be very limited =

Look at this: no recogition of embedded command substitution syntax within the string:


```bash># this is <lang bash

echo "$(foo "$(bar "asdf\"")")"
```


Why is the last closing parenthesis green? WTF?
:Because GeSHi doesn't implement recursive matching of strings with nested shell commands (as this would cause quite some trouble with other places. IF you write up some proper PCRE that properly matches such strings, just provide it to me and I'm happy to include it. --[[User:BenBE|BenBE]] 02:38, 28 January 2012 (UTC)

Type that same example above into Vim and it's handled perfectly by Vim's recursive, hierarchical language of syntax highlighting regions.

Also, what about this:


```bash
#
cat <<!
echo while true do ; done if fi
what the hell
!
```


:Use the more common
:
```bash
#
cat <<HEREDOC
echo while true do ; done if fi
what the hell
HEREDOC
```

:and everything is fine. But if you can provide me with a more complete list of allowed characters in the definition of a HEREDOC spec, I'll try to add it. --[[User:BenBE|BenBE]] 01:12, 28 January 2012 (UTC)

Keywords are being highlighted inside a  here document, which is verbatim text. Shell expressions can occur in a here document, but only as command substitutions $(...).
:GeSHi as used at RC is a mostly context-free parser. If you want something that's context-aware use GeSHi 1.1.X. But unfortunately you can't mix both versions in one script easily. And since GeSHi 1.0.X has a much broader language support, RC is using the stable 1.0.X version. --[[User:BenBE|BenBE]] 01:12, 28 January 2012 (UTC)

Sorry, this is not a usable syntax coloring system.
:Write a better one and come back then. --[[User:BenBE|BenBE]] 01:12, 28 January 2012 (UTC)
:: If I write a better one, will Rosetta switch to it? I have absolutely no use for GeShi (and therefore not for any replacement for GeShi, either) except that it's the only system supported in Rosetta. Why would I waste my spare time hacking on this when I can, like, put my spare time programming cycles into something better, like my own project.  If we could use some HTML tags, the problem would be solved. Vim does the syntax highlighting perfectly, and can put out HTML that you can cut and paste into a web page or wiki. Say, maybe the HTML put out by Vim can be machine translated into colorized Wikimedia markup [http://en.wikipedia.org/wiki/Help:Wiki_markup#Coloring_text]. This may be the key to a GeShi-free solution, worth looking into.[[Special:Contributions/192.139.122.42|192.139.122.42]] 01:51, 28 January 2012 (UTC)
::: Well, let me thing about this for a moment ... No, you don't really want this. No. You don't! --[[User:BenBE|BenBE]] 02:25, 28 January 2012 (UTC)
::::: Proof of concept:
<div class="text highlighted_source" style="border:1pt dashed black;white-space:pre;overflow:auto;background:white;color:black;padding:1em;border:1px dashed #2f6fab;color:black;background-color:#f9f9f9;line-height:1.3em"><code><span style="color: #800090; background-color: #f0f0f0">#include </span><span style="color: #077807; background-color: #f0f0f0">&lt;stdio.h&gt;</span>

<span style="color: #912f11; background-color: #f0f0f0; font-weight: bold">char</span> input[] =
        <span style="color: #077807; background-color: #f0f0f0">&quot;Character,Speech</span><span style="color: #912f11; background-color: #f0f0f0">\n</span><span style="color: #077807; background-color: #f0f0f0">&quot;</span>
        <span style="color: #077807; background-color: #f0f0f0">&quot;The multitude,The messiah! Show us the messiah!</span><span style="color: #912f11; background-color: #f0f0f0">\n</span><span style="color: #077807; background-color: #f0f0f0">&quot;</span>
        <span style="color: #077807; background-color: #f0f0f0">&quot;Brians mother,&lt;angry&gt;Now you listen here! He's not the messiah; &quot;</span>
                <span style="color: #077807; background-color: #f0f0f0">&quot;he's a very naughty boy! Now go away!&lt;/angry&gt;</span><span style="color: #912f11; background-color: #f0f0f0">\n</span><span style="color: #077807; background-color: #f0f0f0">&quot;</span>
        <span style="color: #077807; background-color: #f0f0f0">&quot;The multitude,Who are you?</span><span style="color: #912f11; background-color: #f0f0f0">\n</span><span style="color: #077807; background-color: #f0f0f0">&quot;</span>
        <span style="color: #077807; background-color: #f0f0f0">&quot;Brians mother,I'm his mother; that's who!</span><span style="color: #912f11; background-color: #f0f0f0">\n</span><span style="color: #077807; background-color: #f0f0f0">&quot;</span>
        <span style="color: #077807; background-color: #f0f0f0">&quot;The multitude,Behold his mother! Behold his mother!&quot;</span>;

<span style="color: #912f11; background-color: #f0f0f0; font-weight: bold">int</span> main()
{
        <span style="color: #912f11; background-color: #f0f0f0; font-weight: bold">char</span> *s = input;
        printf(<span style="color: #077807; background-color: #f0f0f0">&quot;&lt;table&gt;&lt;tr&gt;&lt;td&gt;&quot;</span>);
        <span style="color: #1f3f81; background-color: #f0f0f0; font-weight: bold">for</span> (s = input; *s; s++) {
                <span style="color: #1f3f81; background-color: #f0f0f0; font-weight: bold">switch</span>(*s) {
                <span style="color: #1f3f81; background-color: #f0f0f0; font-weight: bold">case</span> <span style="color: #912f11; background-color: #f0f0f0">'\n'</span>: printf(<span style="color: #077807; background-color: #f0f0f0">&quot;&lt;/td&gt;&lt;/tr&gt;&lt;tr&gt;&lt;td&gt;&quot;</span>); <span style="color: #1f3f81; background-color: #f0f0f0; font-weight: bold">break</span>;
                <span style="color: #1f3f81; background-color: #f0f0f0; font-weight: bold">case</span> <span style="color: #077807; background-color: #f0f0f0">','</span>:  printf(<span style="color: #077807; background-color: #f0f0f0">&quot;&lt;/td&gt;&lt;td&gt;&quot;</span>); <span style="color: #1f3f81; background-color: #f0f0f0; font-weight: bold">break</span>;
                <span style="color: #1f3f81; background-color: #f0f0f0; font-weight: bold">case</span> <span style="color: #077807; background-color: #f0f0f0">'&lt;'</span>:  printf(<span style="color: #077807; background-color: #f0f0f0">&quot;&amp;lt;&quot;</span>); <span style="color: #1f3f81; background-color: #f0f0f0; font-weight: bold">break</span>;
                <span style="color: #1f3f81; background-color: #f0f0f0; font-weight: bold">case</span> <span style="color: #077807; background-color: #f0f0f0">'&gt;'</span>:  printf(<span style="color: #077807; background-color: #f0f0f0">&quot;&amp;gt;&quot;</span>); <span style="color: #1f3f81; background-color: #f0f0f0; font-weight: bold">break</span>;
                <span style="color: #1f3f81; background-color: #f0f0f0; font-weight: bold">case</span> <span style="color: #077807; background-color: #f0f0f0">'&amp;'</span>:  printf(<span style="color: #077807; background-color: #f0f0f0">&quot;&amp;amp;&quot;</span>); <span style="color: #1f3f81; background-color: #f0f0f0; font-weight: bold">break</span>;
                <span style="color: #1f3f81; background-color: #f0f0f0; font-weight: bold">default</span>:   putchar(*s);
                }
        }
        puts(<span style="color: #077807; background-color: #f0f0f0">&quot;&lt;/td&gt;&lt;/tr&gt;&lt;/table&gt;&quot;</span>);

        <span style="color: #1f3f81; background-color: #f0f0f0; font-weight: bold">return</span> <span style="color: #077807; background-color: #f0f0f0">0</span>;
}
</code></div>[[Special:Contributions/24.85.131.247|24.85.131.247]] 06:59, 28 January 2012 (UTC)
::::::I put <code>text</code> and <code>highlighted_code</code> classes onto the code tag, but nothing. RC's style sheet only asserts this on pre tags. I think that's where the dotted line border style is coming from, without which this is uselessly inconsistent.[[Special:Contributions/24.85.131.247|24.85.131.247]] 07:13, 28 January 2012 (UTC)
::: RC uses GeSHi in part because it's the best out there which can be integrated into PHP (or, at least, has been), and in part because I can trust BenBE enough to ''give him access to the server'', where he handles upgrading GeSHI for me. It reduces my maintenance load. He's also a little easier to work with. Coming in, ranting and raving as you appear to have done, does ''not'' make me want to work with you. --[[User:Short Circuit|Michael Mol]] 03:33, 28 January 2012 (UTC)

I'm giving up on defining a syntax coloring scheme for my language, since the official stuff in the GeShi distribution isn't doing the kinds of things that I need to do even a 60% accurate job of it. [[Special:Contributions/192.139.122.42|192.139.122.42]] 23:01, 27 January 2012 (UTC)
: Provide patches upstream and I'm sure to take care of it. Just complaining about the world being stupid doesn't help anyone. --[[User:BenBE|BenBE]] 01:12, 28 January 2012 (UTC)
: GeSHi also has problems with comments in Bash, and with here documents in Ruby. Many of the current syntax-highlighting rules are not correct. --[[User:Kernigh|Kernigh]] 00:32, 28 January 2012 (UTC)
:: And since nobody reported the problems here or upstream its unlikely they will be fixed soon. --[[User:BenBE|BenBE]] 01:12, 28 January 2012 (UTC)

= GeShi appears have become 99% transparent =
I just noticed that many code specimen on RC no longer have their Syntax Highlighting apparent.  At first I thought it may be just me, but I have the problem in both Firefox and Chrome.  So I checked wikipedia and there the Highlighting still appears fine.  eg [[wp:ALGOL_68C#The_ENVIRON_and_USING_clauses|ALGOL_68C]], Note the <font color="red">RED</font> where a non standard keyword is used.
By way of contrast <font color="red">ENVIRON</font> in the following is not <font color="red">RED</font> (currently) :

```algol68
BEGIN
   INT dim = 3; # a constant #
   INT a number := 120; # a variable #
   ENVIRON EXAMPLE1;
   MODE MATRIX = [dim, dim]REAL; # a type definition #
   MATRIX m1;
   a number := ENVIRON EXAMPLE2;
   print((a number))
END
```


A good place to see the problem is in [[Hello_world]]. eg in [[Hello_world#Python]] & al.  I (at least) cannot see any highlighting.
Then when one takes a closer look at the HTML source to the code specimen one finds:

```html
<pre class="python highlighted_source"><span class="kw1">print</span> <span class="st0">"Goodbye, World!"</span>
```

```


So it looks like there is a missing style sheet for class="python highlighted_source" & others.

Here is the HTML for ALGOL_68:

```html
<pre class="algol68 highlighted_source">main<span class="sy1">:</span> <span class="br0">(</span>

<span class="kw22">printf</span><span class="br0">(</span>$<span class="st0">"Goodbye, World!"</span>l$<span class="br0">)
</span>
<span class="br0">)</span><
```

```


A missing style sheet for class="algol68 highlighted_source" maybe?

Work around is: think in monochrome! :-)

[[User:NevilleDNZ|NevilleDNZ]] 03:49, 20 March 2013 (UTC)

Update... today the highlighting is working fine in Algol68 and Python and probably everything else....

Problem resolved by some phantom somewhere :-)

I note that the wikipedia color schemes are different from rosettacode GeShi color schemes.... Not a big problem, but is there someone somewhere that has proposed a viable "standard" color scheme for syntax highlighting.  GeShi itself hints at it, but gedit and vim hint totally different ideas.

[[User:NevilleDNZ|NevilleDNZ]] 07:03, 22 March 2013 (UTC)

= Highlighting for MK-61 =
I propose to install a syntax highlighting of MK-61/52: [http://pastebin.com/BNByHpac file]. [[User:Русский|Русский]] ([[User talk:Русский|talk]]) 16:02, 11 March 2014 (UTC)
: Or think the highlighting for it is not necessary and the code is clear so? [[User:Русский|Русский]] ([[User talk:Русский|talk]]) 16:09, 12 March 2014 (UTC)
: Do not present the technical opportunities to add new lighting? 19:18, 20 April 2014 (UTC)
:: I think you need to send a pull request if you want to add your language file to GeSHi. The repository is on [https://github.com/GeSHi/geshi-1.0 GitHub]. --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 06:53, 23 April 2014 (UTC)
::: So, I've finally managed to add a pull request for Phix, any idea how long that will take? [[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 01:00, 25 October 2015 (UTC)
* [https://github.com/GeSHi/geshi-1.0/blob/master/src/geshi/mk-61.php]
*: Is there still someone alive here? When is the update? 14:20, 13 May 2017 (UTC)

= AppleScript Comments =
AppleScript '#' comments do not get properly highlighted, nor do nested comments
