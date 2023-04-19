+++
title = "Category talk:Programming Languages"
description = ""
date = 2013-11-22T23:23:01Z
aliases = []
[extra]
id = 2392
[taxonomies]
categories = []
tags = []
+++

Isn't this covered by [[:Category:Solutions by Programming Language]]? Everything except [[:Category:SQL Derivatives|SQL derivatves]] is in the solutions category. --[[User:Mwn3d|Mwn3d]] 23:25, 4 December 2007 (MST)
:This category, as well as SQL derivatives, should probably be merged back with the "Solutions by" category. --[[User:Short Circuit|Short Circuit]] 09:33, 5 December 2007 (MST)
::Maybe we could leave this category and subdivide the solutions category similarly to solutions by task and programming tasks? We're up to an awful lot of languages. It'd be nice to get some categories in for extra encyclopedic knowledge too. --[[User:Mwn3d|Mwn3d]] 02:33, 28 April 2009 (UTC)

Just to say, [[:Category:SQL Derivatives|SQL derivatves]] are not programming languages, just servers. (MySQL, Oracle), programming languages are [[PL/SQL]], [[T-SQL]] and etc.

: SQL should be a "standard", but it is not: dialects may exist. So as far as I know, MySQL is yes a server, but also understand an "SQL" that can be called simply "MySQL"; I don't know how it relates to PL/SQL, T-SQL and so on... However on Wikipedia I read MySQL understand a <cite>broad subset of ANSI SQL 99, as well as extensions</cite>... --[[User:ShinTakezou|ShinTakezou]] 11:22, 28 April 2009 (UTC)

: I would be more comfortable grouping all such solutions under [[SQL]] (or perhaps [[PL/SQL]] or [[T-SQL]]) and then using the {works with} template to distinguish quirks that are specific to a particular implementation, such as [[MySQL]].  --[[User:IanOsgood|IanOsgood]] 17:22, 28 April 2009 (UTC)
::I like this idea too. But the dialects would have to become implementations(?) rather than languages. --[[User:Mwn3d|Mwn3d]] 18:00, 28 April 2009 (UTC)
::: If we're going to start seriously distinguishing between dialects, I don't think we should lump them in with "implementation".  Rather, [[Template:dialect]] ought to be created and used for the purpose.  Different implementations can be absolutely code-compatible in the sense that each of a pair of implementations' support for a language has a one-to-one mapping with the other, with the same output.  Clones and reverse engineering efforts are one place where this is strived for.  When you start talking about dialects, you're explicitly making a distinction between the nature of inputs.
::: The only bit I'm worried about is how one draws the line between different dialects of a language and different languages.  There is an SQL standard that came out in 1992, but many of today's SQL implementations which are equally compatible(No implementation that I'm aware of completely implements the standard) with that standard are incompatible with each other.  I believe this is largely a result of different decisions in expressing the same syntax. (I don't like putting it that way, exactly, but consider the difference between building an SQL query for Access or MS SQL Server and building the same query for Oracle or MySQL.).
::: As a side-benefit, this allows us to identify language extensions as dialects of a language; Code that uses gcc extensions can be identified as using the GCC dialect of the language, while code that uses MSVC extension can be similarly categorized.   the [[Template:works with|works with]] template is a horrible hack that resulted from needing to organize data without being able to properly define that organization.  If we can obviate it without necessitating forty different templates per code example, I'd be really, really happy.  --[[User:Short Circuit|Short Circuit]] 18:35, 28 April 2009 (UTC)
:::: This would mean also that all ''Pascal'' examples "working with" this or that "implementation" (gpc, FreePascal, TurboPascal...) should become ''dialects of''...? With Pascal, the template '''works with''' sufficed, or at least so it seemed to me. Maybe SQL after all fell in the same trap? --[[User:ShinTakezou|ShinTakezou]] 18:51, 28 April 2009 (UTC)
:::::I think BASIC has the same kind of thing going on. We need to work to distinguish dialects from implementations (if we want a distinction) if we want this to work. --[[User:Mwn3d|Mwn3d]] 19:17, 28 April 2009 (UTC)
::::: '''works with''' is particularly unsatisfactory to me because it provides information about a programming example without an effective way to organize that information in navigable categories.  With separate dialect and implementation templates, the dialect category can be made a subcategory of the parent language, while at the same time being listed as supported by particular implementations.  In this sense, one can identify C++98 as a dialect of C++, and identify which C++ implementations support the C++98 dialect.  An implementation's nonstandard extensions or definition of a standard's undefined behavior (i.e. gcc C and C++ language extensions, or a Brainfuck compiler's particular interpretation of BF's rather loose standard) would count as their own dialect. (It's notable that such dialects don't necessarily have only one implementation; ActiveState Perl and the official Perl distribution, for example, both implement the same dialect of the language, as far as I'm aware.) --[[User:Short Circuit|Short Circuit]] 22:09, 28 April 2009 (UTC)

Although the site contains the page 'Category:BBC_BASIC' that language does not appear on the main Programming Languages page.  How can that be resolved? --Richard Russell 09:28, 15 May 2011 (UTC)
: Hm. [[:Category:BBC BASIC|BBC BASIC]] is marked as an implementation of [[:Category:BASIC|BASIC]]. If you use {{tmpl|Language}} instead of {{tmpl|implementation}} on [[:Category:BBC BASIC]], it would be removed from being listed as an implementation, and instead be listed as a language of its own. The distinction between language implementations and derivative languages is difficult to be objective about, and not entirely clear; that's why we wind up with strange data organization quirks like these. --[[User:Short Circuit|Michael Mol]] 11:00, 15 May 2011 (UTC)
:: I'm confused.  If one looks at, for example, the [[:Category:Liberty BASIC]] page it also has {{tmpl|implementation}} yet that '''is''' indexed under [[:Category:Programming Languages]] as well as under [[:Category:Implementations]]. --Richard Russell 13:32, 15 May 2011 (UTC)
::: That's because [[:Category:Liberty BASIC]] has both {{tmpl|implementation}} and {{tmpl|language}}. I'm really not sure of the best way to go about indexing BBC BASIC, but if you want to follow the model demonstrated by Liberty BASIC, that's fine too. (So you would add {{tmpl|language}} to [[:Category:BBC BASIC]]). --[[User:Short Circuit|Michael Mol]] 11:38, 17 May 2011 (UTC)

== ... Z M M ==


At the end of this page one sees:

```txt

Z
Z80 Assembly
ZED
Zonnon
ZPL
ZX Spectrum Basic

Μ
ΜC++

М
MK-61/52

```


What could (some)one do so that these languages appear under the capital M? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 13:35, 22 November 2013 (UTC)

-----

: The   '''M'''   in the   '''MC++'''   is really a Greek micro   ('''µ''')   which has been translated/transcribed to a Latin   '''M'''   somewhere along the way by something along the way, and where does one sort a Greek micro among a Latin alphabet?

: The   '''MK'''   in the   '''MK-61/52'''   is really a Cyrillic   ('''МК''')    ... (like above).

: For a little more information, see the comments in the REXX (language) section header. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:41, 22 November 2013 (UTC)

:: I understand that, but in other pages MK... finds is proper place among the other M's. See, e.g..

```txt
http://rosettacode.org/wiki/Primality_by_trial_division
* 48 M4 <#M4>
  * 49 Maple <#Maple>
  * 50 Mathematica <#Mathematica>
  * 51 MATLAB <#MATLAB>
  * 52 Maxima <#Maxima>
  * 53 ÐœÐš-61/52 <#.D0.9C.D0.9A-61.2F52>
  * 54 MUMPS <#MUMPS>
```

--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 23:04, 22 November 2013 (UTC)

::: Well, I can't speak for Wiki (or whatever is formatting all this (and that), but for what I can tell (or remember), the first foray by MK-61/52 may have used Cyrillic or some other font which may have sorted correctly, and then appeared to use a meta-tag or whatever.   I know about this as much as I know about beekeeping.   Not a hill of beans. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:17, 22 November 2013 (UTC)

::: Also, the sorting code that arranges the category lists may be a different sort program (or a different algorithm) that sorts the list of languages for entries under a Rosetta Code task   (just stumbling around in the dark). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:23, 22 November 2013 (UTC)
