+++
title = "Rosetta Code:API/MediaWiki/Perl/MediaWiki::API"
description = ""
date = 2009-07-21T14:56:10Z
aliases = []
[extra]
id = 4556
[taxonomies]
categories = []
tags = []
+++

=Official Resources=
[http://search.cpan.org/dist/MediaWiki-API/lib/MediaWiki/API.pm MediaWiki::API] on CPAN.
=Usage=
All of the methods take [[Associative Array|hashrefs]], which provides a named parameter approach to function arguments.  MediaWiki::API uses [[object-oriented principles]], which means you need to [[instanciate an object]].  Using MediaWiki::API involves four stages.
==Construction==
The '''new''' method instanciates a MediaWiki::API object

```perl
my %options; # Options might go in this hash, passed in via an anonymous hashref, or assigned later.
my $mw = MediaWiki::API->new( \%options ); #Construct the object
```


Some configuration options will be specific to Rosetta Code, though this currently only applies to the api_url.

```perl
my %rcopts = ( 'api_url' => 'http://rosettacode.org/mw/api.php' );
```


==Login==
Provide MediaWiki with your login credentials, or those of the account you created for your bot.

```perl
my %login = ( 'lgname' => 'Some Anonymous Bot', 'lgpassword' => 'hf6873#@!!hfFHs' );
```

==Use the API==
Assuming your login succeeded, you may now use the API.  See the '''api''', '''edit''', '''get_page''', '''list''', '''upload''' and '''download''' methods in the Method Reference below.
==Logout==
Log out of MediaWiki tidily.

=Method Reference=
==Primary Methods==
These methods perform unique operations with respect to MediaWiki and the API.
{|
|[[Rosetta Code:API/MediaWiki/Perl/MediaWiki::API/api|api]]
|Direct abstraction of [[Rosetta Code:API/MediaWiki/api.php|api.php]]
|-
|[[Rosetta Code:API/MediaWiki/Perl/MediaWiki::API/download|download]]
|Download files from the wiki
|-
|[[Rosetta Code:API/MediaWiki/Perl/MediaWiki::API/login|login]]
|Log in and gain access to MediaWiki functionality.
|-
|[[Rosetta Code:API/MediaWiki/Perl/MediaWiki::API/new|new]]
|Constructs a MediaWiki::API instance
|-
|[[Rosetta Code:API/MediaWiki/Perl/MediaWiki::API/upload|upload]]
|Upload files to the wiki
|-
|}

==Helper Methods==
These methods wrap one or more primary methods, providing default operations for common operations.  Anything you can do with one of these methods, you can do with one of the primary methods.
{|
|[[Rosetta Code:API/MediaWiki/Perl/MediaWiki::API/edit|edit]]
|Edit a page
|-
|[[Rosetta Code:API/MediaWiki/Perl/MediaWiki::API/get_page|get_page]]
|Retrieve the latest version of a page
|-
|[[Rosetta Code:API/MediaWiki/Perl/MediaWiki::API/list|list]]
|Retrieve listings
|-
|}
