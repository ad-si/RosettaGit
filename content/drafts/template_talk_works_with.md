+++
title = "Template talk:Works with"
description = ""
date = 2012-12-30T14:19:18Z
aliases = []
[extra]
id = 7371
[taxonomies]
categories = []
tags = []
+++

== Regarding standardized languages ==

For standardized languages, I'd like to suggest that this template indicate the oldest standard that the code example works for, or the range of standards. This is particularly useful for a language like C where the standards aren't backwards-compatible with the original implementation, as well as other deprecation scenarios in their development. --[[User:Short Circuit|Michael Mol]] 13:37, 19 May 2010 (UTC)
: Specifying nothing would mean the latest known/accepted standard...?
: However, saying "works with gcc 4.x.x" does not tell us which standard the code is in (and maybe the same for VC++) since gcc implements the most recent standard but it compiles old ones too... and it is why when I've seen C/C++ (recent) standard code I've removed the "works with 'specific compiler'".
: Something different for languages having contemporary dialects (rather than different standards in time), like e.g. Smalltalk, or Prolog. Here the difference is done by the implementation, which '''de facto''' determines the dialect in use.
: Often one hardly can test the code with every implementation of the compiler / interpreter... so e.g. I always put "works with GNU Smalltalk" in my Smalltalk codes since it is the one I use, but indeed it means only "tested with GNU Smalltalk", while other implementations may work as well!
: Maybe we need three separate templates: about standards (compliant with standard ''last standard witch "fits" the code''), about implementation (works with gcc, e.g. when using specific gcc-ism; works with GNU Smalltalk, Free Pascal and so on ...) and a generic "tested with"...
: Standard compliance may be omitted if it is last-standard compliant (but a more recent standard could trigger a "fix-standard-compliance" on all the tasks?); implementation can be omitted if there's no in use any implementation-specific code; tested with can be omitted if we respected a common (among implementations) standard (so we stick to rules for standard compliance)...
: Just thoughts in my succint speach!
:: &mdash;[[User:ShinTakezou|ShinTakezou]] 14:34, 19 May 2010 (UTC)
:: I don't want to get overcomplicated and overspecific on it, because down that road lay madness, and I don't need any more of that. I'm only trying to address the stuff that's easy to roughly address.  The particular case I'm interested in at the moment is where the ''de facto'' standard doesn't very far from the ''de jure'' standard, so that one could specify the ''de jure'' standard and indicate deviant implementations if necessary. For cases where the ''de facto'' differs broadly from the ''de jure'', I don't see a long-term effective solution. I ''am'' getting worried about Python 2.x vs Python 3.x, and Perl5 vs Perl6 is a case which has already seen presentation on the site. --[[User:Short Circuit|Michael Mol]] 14:49, 19 May 2010 (UTC)


== Tag should flag task as implemented ==

It would be nice if the {{works with}} tag flagged the task as implemented, so that the tasks do not keep appearing on the unimplemented task lists.

[[User:Markhobley|Markhobley]] 19:37, 9 May 2011 (UTC)
:The unimplemented lists are only for languages. The works with tag is for specific versions of languages (which will already be marked as implemented with a {{tmpl|header}}) or compilers/implementations of languages/standards <s>and libraries</s> (which don't have unimplemented lists). --[[User:Mwn3d|Mwn3d]] 19:47, 9 May 2011 (UTC)

== Splitting the template? ==

I believe that the original purpose of the template was to provide the minimum version or standard of the language that provides language features used in the code, or minimum version of a particular implementation that supports an implementation-specific extension used in the code. (Perhaps it would be even better to explicitly state the feature(s) that requires that version.) But in some places, people have also used it to just indicate that the code is "tested and works" on a particular version of some implementation, which is useful but a different type of information. Part of the confusion might result from the name "works with", which sounds like it is for people to "report that it works". Perhaps we should make two templates for each of these uses? The original use (minimum supported version) might be better called "Requires" or something like that. --[[User:Spoon!|Spoon!]] 22:43, 29 December 2012 (UTC)

:A split to "Requires" and "Tested with" ? "Tested with" might need to allow a list of more than one version. --[[User:Paddy3118|Paddy3118]] 14:19, 30 December 2012 (UTC)
