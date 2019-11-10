+++
title = "Rosetta Code:Add a Language"
description = ""
date = 2019-09-13T17:03:14Z
aliases = []
[extra]
id = 5006
[taxonomies]
categories = []
tags = []
+++

{{#set:is contribution page=true}}Thanks to a system of templates, adding a language on Rosetta Code is fairly simple.  To begin with, consider the name of the language; This will be part of the name of the page that represents your language.

''After'' checking to see if the language is [[:Category:Programming Languages|already on Rosetta Code]], You're going to need a category page to list all the examples, a redirect in the main namespace to redirect to the category page, and, finally, an example or two.

''Notice that, for the purpose of instruction, we call the language "Ayrch", but that's almost certainly not going to be the name of the language you're adding; Replace "Ayrch" with the name of your language.''

==Prerequisites==

For inclusion on Rosetta Code, the requirements are reasonable: There should be an existing implementation of that language that is either mature or, at the very least, under active development. Language notability is unimportant; if the number of languages on the site grows enough to require distinction, they can be differentiated by example coverage.

==Basic Information==

These are the bits that should be done for every language.


### Category Page


Once you're sure the language doesn't already have a page on Rosetta Code, you'll need to create a category page for the language. Let's say you're adding a language called Ayrch ('''This is a hypothetical language name; please change it to your actual language name when you actually add your language.''') The first thing you need to do is create the category page. The easiest way is to click on the Search field, type '''Category:Ayrch''', and click Go. It will tell you there is no current page with that name. Click "create this page", and it will give you an empty page to edit.

One simple way to start is to make this the entire body of the page:


```txt
{{stub}}{{language|Ayrch}}
```


That will automatically give you a basic language page, and even a nice little stub notification reminding people who visit to fill in more information.


### Redirect


The next step is to create a redirect page.  This is important, because the ''actual'' page for your Ayrch language is at '''http://rosettacode.org/wiki/Category:Ayrch''', and we want people to be able to go to '''http://rosettacode.org/wiki/Ayrch''', and be able to use syntax like <nowiki>[[Ayrch]]</nowiki> within the wiki to refer to it.

As before, click on the Search field on the left, but this time type '''Ayrch''', and click Go.  Again, click "create this page", and it will again give you an empty page to edit.

This time, make the entire body of the page:


```txt
REDIRECT [[:Category:Ayrch]]
```


Now, when anyone goes to the Ayrch page, they will be immediately redirected to the category page for Ayrch.


### Examples


''You're not done yet!''

You've created a language category page and have ensured that people who visit the page in the main namespace will reach the right place.  You might even have gone back to the category page and filled in a few more details like some history and links to the official sites and resources for the language.

What could be missing?  ''Code!''

You need to provide at least one or two token examples, to give people a taste of the language.  Otherwise, there really isn't much of a point for the language to be mentioned on the wiki; Nobody is likely to notice it.

If you're pressed for time, browse the [[:Category:Programming Tasks|list of tasks]] and find a couple simple ones you can implement.  [[User Output]], [[Loop Structures]] and [[Conditional Structures]] are some common ones that most languages support. For the sake of this demonstration, let's suppose that Ayrch looks a lot like BASIC, and implement [[User Output]].

We would need to go to that page, find where the language name would fit (alphabetically), and add this code:


```txt


## Ayrch


```ayrch
PRINT "GOODBYE, WORLD!"
```


```


That's a very simple example; You might try adding some descriptive information before the <nowiki><lang></nowiki>, such as what compiler it works with, or perhaps some interesting information of how Ayrch does things differently from other languages. Whatever helps to illustrate the language and identify what makes this example interesting.


### Tasks Not Implemented


Finally, you're going to want to create an easy way for other people to discover and add tasks that have not yet been implemented in your language. In the bottom right of your page, click the link that says, "If you know '''Ayrch''', please write code for some of the ''tasks not implemented in Ayrch''." In the new page that opens, enter the following for the page contents:


```txt

{{unimpl_Page|Ayrch}}

```


==More Advanced==
These aren't strictly necessary, but are generally a plus if you want to increase awareness and penetration of your language on the site.


### User Boxes

You created a user page before doing your edits, didn't you? You don't have to, but it generally helps in identifying who created and contributed what.

In your user page (not your user talk page), try adding a user box. That generally looks something like this:


```txt
{{mylangbegin}}
{{mylang|Visual Basic|Active}}
{{mylang|BASIC|Very Very Rusty}}
{{mylang|Brainfuck|Rusty}}
{{mylang|C++|Very Active}}
{{mylang|Perl|Very Active}}
{{mylang|PHP|Semi-Active}}
{{mylang|UNIX Shell|Very Active}}
{{mylang|C|Semi-Active}}
{{mylang|Java|Rusty}}
{{mylang|JavaScript|Active}}
{{mylang|SQL|Active}}
{{mylang|Visual Basic .NET|Rusty}}
{{mylangend}}
```


Of course, you don't have to use words like "Active" or "Rusty"; You can use "Expert", "Novice" or "Author" (or any other way you want to describe your proficiency), if you like. If the only language you really know happens to be Ayrch, then your language box is pretty simple:


```txt
{{mylangbegin}}
{{mylang|Ayrch|Replace this with something reflective of your experience level}}
{{mylangend}}
```


If you just copy and paste that, you'll probably get the idea fairly quickly.


### Implementations


A language is only theoretical until it has an implementation.  An implementation might be a compiler, an interpreter, or even a piece of silicon.  It helps users tremendously if they can find implementations of the language you're trying to show them.  One good way to do that is to create an implementation page.

Let's say you have a compiler named ayrchc, and you want to create a page for it.  Click on the Search field on the left, type '''ayrchc''', and click Go.  Click "create this page", and give the page a body:


```txt
{{stub}}{{implementation|Ayrch}}
```


That's a start, but if you're this far, then you can go a step or two beyond that.  Instead of using <nowiki>{{stub}}</nowiki>, give a couple lines of description about the implementation, and, preferably, a link to the official page for the implementation.

==Conclusion==

If you've done all that, there's only one more thing you really ought to do: Get more people familiar with your language to fill in more tasks.  Remember that page you created for "Unimplemented Tasks"? Pass that around to interested parties, and things will generally start happening.

We'll be watching for you!
