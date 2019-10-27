+++
title = "Rosetta Code:Village Pump/Upgrades"
description = ""
date = 2010-11-28T17:06:02Z
aliases = []
[extra]
id = 8471
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Upgrades
|summary=Software versions in use on Rosetta Code
}}
Upgrades, past, present and planned.

=Latest news=

* See [[Special:Webchat]]
* [http://webchat.freenode.net/?channels=rosettacode Freenode's direct webchat page], or
* point your IRC client to #rosettacode on irc.freenode.net.

=2010-10-17=

== Software versions==
As of 2010-10-12, these are the software and versions we're running.

Questions to answer for each:
# Is this in use?
# Are there upgrades available?
# Do the upgrades conflict with current or targeted versions of other software?

* MediaWiki 1.15.3
** Upgraded to '''1.6.0'''.

* MultiCategorySearch 1.35, with modifications by [[User:Opticron]].
** Currently, ''yes'', this is used a lot. Namely, as part of [[Template:Unimpl Page]]. Modifying that template to leverage SMW instead would be nice, but SMW doesn't support set exclusions involving categories, so things like [[Template:Example]] and [[Template:Task]] would need to apply semantic properties in addition to categories, so that the SMW query could operate on those properties, instead of categories.
** Versions 1.4 and 1.5 are available. No obvious conflicts. May require editing by [[User:Opticron]] to include inline queries.
* Special:Interwiki r47067
** Yes, used very much.
** r32289 is current svn version number. No idea if there are any changes.
* WebChat 1.0.0
** Occasionally
** No new version.
* Cite Version r47190
** Don't know.
** 74711 is current svn version number. Don't know if there are any changes.
*** Upgraded.
* GeSHi CodeTag 1.65, ''liberally'' edited and rewritten by [[User:Short Circuit]]. It's nearly a completely different extension by now.
** Extensively.
** Updates are handle by [[User:BenBE]].
* IpbWiki PayPal 1.02
** Yes, for donations.
** No new version.
* ParserFunctions (no version number at [[Special:Version]]?
** Yes, for templates like [[Template:Language]].
** ''''Removed''' ''explicit'' inclusion, now pulled in via '''SemanticBundle'''.
* Semantic MediaWiki 1.5.1.1
** Yes
** SMW 1.5.2 is available. No obvious conflicts.
** Upgraded to '''1.5.2'''.
*** '''Removed''' ''explicit'' inclusion, now pulled in via '''SemanticBundle'''.
* AmazonPlus 0.5.2
** No. There's a bug in MediaWiki template processing that prevents AmazonPlus from being usable within templates.
** No new version.
* ConfirmEdit Version r36959
** Yes, as part of CAPTCHAs.
** Current svn trunk is r32289. Don't know if there are any changes.
* TextLink Ads (Version 0.9) (written entirely by [[User:Short Circuit]])
** Yes, on a couple pages, but it's not proven useful. The extension itself selectively pulls in and executes PHP files based on page name, though, so it has broader utility and may be used for other things.
** No new version.

* Squid version 2.7.STABLE3-4.1 (debian 5)
** Yes. I'm sure it has a few cache hits, too.
** I don't know if the X-Vary-Options patch is applied. The X-Vary-Options patch is [http://www.mail-archive.com/squid-dev@squid-cache.org/msg07066.html described here], latest references are [http://www.mail-archive.com/squid-dev@squid-cache.org/msg08321.html here] and [http://www.mail-archive.com/squid-dev@squid-cache.org/msg08320.html here], and the [http://wiki.squid-cache.org/Features/XvaryOptions wiki page] in question doesn't appear to have gone anywhere. Furthermore, I don't know what to make of [http://www.mail-archive.com/mediawiki-cvs@lists.wikimedia.org/msg02728.html this]. Without apt-get source 'ing the squid package, I don't know if I have the patch or not.
*** Have updated info. Don't have the patched version. Not going to do a custom build without further investigation toward integrating MediaWiki's patch set and building a local package. --[[User:Short Circuit|Michael Mol]] 13:22, 17 October 2010 (UTC)


### Added software

* Including the SemanticBundle.
** The following SB components were ''already'' on Rosetta Code:
*** '''Semantic MediaWiki'''
*** '''ParserFunctions'''
*** '''Semantic Forms'''
** Furthermore, these SB components are additional required components, and so have been added to Rosetta Code:
*** '''Semantic Forms Inputs'''
*** '''Semantic Drilldown'''
*** '''Admin Links'''
*** '''Widgets'''
** Finally, these optional comonents of SemanticBundle have been enabled:
*** '''SemanticCompoundQueries'''
*** '''Validator'''
*** '''SemanticTasks'''
*** '''SemanticInternalObjects'''
*** '''PageObjectModel'''

== Server tuning ==

I'd like to perform tuning on:
# MySQL/InnoDB.
# memcached (possibly grant it more memory)
# squid (Identify tuning opportunities.)
# php/apache (give the php extension more memory if we can; 24M is close to the roof of current processes.)
