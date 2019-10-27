+++
title = "Help:Adding a new programming example"
description = ""
date = 2015-10-10T04:40:27Z
aliases = []
[extra]
id = 1834
[taxonomies]
categories = []
tags = []
+++

{{#set:is contribution page=true}}So you want to contribute code, eh? If you've used wikis before, it's actually very easy. If not, then you'll need a few pointers to get started.

==Every page is editable (almost)==

Almost every page in the wiki is editable. Even this page is editable. (See that tab at the top labeled "edit"?)

To edit a wiki, you need only click that "edit" tab, make changes in the text field on the resulting page, and click Submit. Really, though we'd rather you clicked Preview first, and then clicked submit once you were satisfied with how it looks.

==Navigation==

To add your own programming example, you must [[:Category:Solutions by Programming Task|navigate]] to the programming task to which you wish to add a solution to. Once you find your task, go ahead and click the Edit tab at the top of the page, find the place in the list of languages already applied where your language would fall alphabetically, and make your changes.

In order to maintain the readability, searchability and browseability of Rosetta Code, we need you to follow certain formatting rules. Please ensure that your contributed code conforms to one of these [[Help:Programming Example Prototypes|programming example prototypes]]. A simple example should look something like this:

 <nowiki>
## C

{{works with|gcc|4.0.1}}
{{libheader|SDL}}

Optional text explaining stuff


```c

int main ( void ) {
    // Some code here
}

```
</nowiki>

You don't have to use the C programming language, and your code example will probably be different. You need to make sure you create a link to your language of choice, and to your compiler, interpreter, or what-have-you using the "works with" template. You should also note any special [[libraries]] you use with the "libheader" template.

The <nowiki><lang></nowiki> tags enclose the actual example, and allow [[Help:Syntax_highlighting|syntax highlighting]], which includes proper formatting for white space. Some languages don't have syntax highlighting support, but using the lang tags will not break anything. If your language is not supported, it will be as if you used pre tags. If support is added later, your example will automatically be highlighted, so add the lang tags no matter what.

==Formatting==

Formatting a wiki is easy once you've done it, but you have to get used to it, especially if you're used to working with HTML. If your browser has [[JavaScript]] enabled, there will be a little formatting bar above your editing area.  Try clicking on the buttons in that bar. See [[Help:Formatting]] for more help.

==Before you go==

Now, why don't you wander over to the [[Help:Sandbox|Sandbox]] and apply what you've learned?

[[Category:Adding Content]]
