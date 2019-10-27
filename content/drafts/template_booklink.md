+++
title = "Template:BookLink"
description = ""
date = 2013-03-30T16:03:12Z
aliases = []
[extra]
id = 5374
[taxonomies]
categories = []
tags = []
+++

<includeonly>{{#if:{{{2|}}}|<amazon keywords="{{{1}}}"><span class="plainlinks">''[%url% {{{2}}}]''</span></amazon>|<amazon keywords="{{{1}}}"><span class="plainlinks">''[%url% %title%]'' by '''%author%'''</span></amazon>}}</includeonly><noinclude>{{template}}

==Purpose==
The use of this template is preferred to direct linking to book resources, as purchases made through the generated link earn Rosetta Code a bit of money through the Amazon Associates program. This will [[Rosetta Code:Finances|help]].
==When and When Not to Use==
# '''Use''' when a link to a specific book resource is helpful in understanding the language or domain problem at hand.
# '''Use''' when citing such a resource.
# '''Do Not Use''' when a link to a specific book resource would be decidedly inappropriate or unhelpful.
# '''Do Not Use''' when there are already a large number of links in a context not dedicated to such a listing.
# '''Do Not Use''' when the matter is already covered by references to other, more appropriate resources.

### Are you published?


If you're a published author of books directly relating to programming, computer science, computer engineering and/or any of the tasks on Rosetta Code, here are some guidelines that will be of interest to you.

* If you sell through Amazon, you are '''encouraged''' to use this template on your user page to build a listing of your published works. Outside of your user page, please limit such links to places and quantities that are useful and relevant.
* A mundane link, of course, is fine if your works aren't sold through Amazon.
** Links to other sales portals may be changed to using this template.
** If you prefer a mundane link to your works' ''official'' page over a referral link through Amazon, that is also fine; the Rosetta Code community respect the desires of any author who expresses this preference.
* Links to your works will not be rejected ''solely'' due to conflict of interest on your part. By having a published work, you are likely to be more knowledgeable than most. Links to your work may be removed or adjusted for other reasons relating to maintaining and/or improving Rosetta Code's content and layout.

Obviously, if you fall under "Wrote It" or "Wrote the Book", it's very probable your book is appropriate on the page dedicated to It. (i.e. if you wrote It or The Book regarding a language, then a link on that language's page is appropriate. If you wrote It or The Book regarding one of the [[:Category:Programming Tasks|tasks]], then a link may be appropriate there, for those interested in learning more about the task's domain.

Also note that '''A Book''' is not the same as '''The Book'''. No disrespect intended, but not all works are equal. If you wrote '''A Book''', by all means, list it in your user page! Ideally, another active community member or two will have the opportunity to read it and, possibly, recommend it.

==How to use==

By correct selection of the template parameter (the [[wp:ISBN|ISBN]] number, '''dashes removed''', is ideal, followed by [[wp:ASIN|ASIN]] or [[wp:ESIN|ESIN]].), a referral link will be provided to the desired resource, hyperlinked as the title and author's name.

For example, Donald Knuth's ''The Art of Computer Programming: Volume 1 (3rd Edition)'' has the ISBN-10 of '''0201896834'''. To create a referral link to this resource, you may use this syntax:

```txt
{{BookLink|0201896834}}
```


This will appear as: {{BookLink|0201896834}}

The box set, volumes 1-4A has the ISBN-10 of '''0321751043'''. To create a referral link to this resource, you may use this syntax:

```txt
{{BookLink|0321751043}}
```


This will appear as: {{BookLink|0321751043}}


### Colloquial naming


Colloquial names are supported, too. For example, {{BookLink|9780596000271}} is colloquially known as the "camel book". To represent this, we take the ISBN (978-0-596-00027-1), remove all dashes (to get 9780596000271), and use this syntax:


```txt
{{BookLink|9780596000271|camel book}}
```


This will appear as: {{BookLink|9780596000271|camel book}}

[[Category:Possible Redundant Templates]]
