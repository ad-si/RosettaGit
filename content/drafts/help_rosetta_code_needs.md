+++
title = "Help:Rosetta Code Needs"
description = ""
date = 2008-01-18T19:29:17Z
aliases = []
[extra]
id = 1932
[taxonomies]
categories = []
tags = []
+++

Rosetta Code has expand at a rate far exceeding my expectations.  Unfortunately, it now has needs that I don't know how to fill.  Since a sizable community of language enthusiasts have been participating in RC, it seems wise to get help from you.

=Bots=

RC needs bots to perform two functions, at least.

A bot is a piece of software that interacts with the MediaWiki engine through HTTP GET queries, much the same way as a user using a browser.  MediaWiki's index.php supports a [http://www.mediawiki.org/wiki/Manual:Parameters_to_index.php#Raw raw-access API], specifically to aid in that.

==Tidying up==

Most of my actual work with Rosetta Code involves keeping an eye on additions to the site and ensuring they implement a few basic conventions. For example:


### Example/task categorization

Rosetta Code's navigation organization depends heavily on proper categorization of programming tasks. Programming examples need to include code to categorize their parent task, based on the name of the section title. This is done by using the <tt><nowiki>{{header}}</nowiki></tt> template.

In addition, when new languages are added, two things need to take place.  A page and category need to be created for that language.

For example, let's say that someone adds an example for a language not previously covered on Rosetta Code.  We'll use [[Pseudocode]] here, because that's a language which, for some odd reason, hasn't shown up yet.

When a programming example is created set up the header like so:

```txt
<nowiki>

## Pseudocode

 (some code)
</nowiki>
```


The template adds a link to a nonexistent page, and at the bottom of the task page a link to a nonexistent category.
The language page needs to be created and filled with a simple redirect to the category page:

```txt
REDIRECT [[:Category:Pseudocode]]
```


Note that the colon preceding the word "Category" is important; it prevents the redirect page from being included in the language category.

Finally, we need to open the Category page, and fill in the <tt><nowiki>{{Programming Language}}</nowiki></tt> template:


```txt
<nowiki>{{Programming Language}}</nowiki>
```


MediaWiki handles the rest.
