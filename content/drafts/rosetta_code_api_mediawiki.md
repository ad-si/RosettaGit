+++
title = "Rosetta Code:API/MediaWiki"
description = ""
date = 2009-07-21T13:59:22Z
aliases = []
[extra]
id = 4554
[taxonomies]
categories = []
tags = []
+++

MediaWiki runs at the core of Rosetta Code. It offers several builtin programmatic interfaces, including [[Rosetta Code:API/MediaWiki/api.php|api.php]] and [[Rosetta Code:API/MediaWiki/index.php|index.php]].

Interactions with the MediaWiki builtin APIs will require authentication for some operations, and special account privileges for others.  Traditionally, the "Bot" status is granted to user accounts which are dedicated to use by a bot.  This allows the bot's access to the site to be disabled if its behavior becomes detrimental to the site. Examples of such cases might include consuming too many server resources, repeated corruption of pages or execution of processes and changes which have been decided are incorrect or inappropriate.

=Perl=
There are at least two [[Rosetta Code:API/MediaWiki/Perl|Perl bindings]] for interacting with MediaWiki, [[Rosetta Code:API/MediaWiki/Perl/MediaWiki::API|MediaWiki::API]] and [[Rosetta Code:API/MediaWiki/Perl/MediaWiki::Bot|MediaWiki::Bot]].  MediaWiki::Bot provides bot-specific abstractions to MediaWiki::API.
