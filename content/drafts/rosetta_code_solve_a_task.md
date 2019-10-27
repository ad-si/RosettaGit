+++
title = "Rosetta Code:Solve a Task"
description = ""
date = 2018-06-20T09:59:20Z
aliases = []
[extra]
id = 5168
[taxonomies]
categories = []
tags = []
+++

So you'd like to solve a task? Great! Here's a brief walkthrough on how you might do that.  While you may already have a task and a language in mind, we're going to assume the language is "Ayrch", and the task is [[Hello world]].  If the language you're familiar with doesn't already have a [[:Category:Programming Languages|presence on Rosetta Code]], consider going through the motions of [[Rosetta Code:Add a Language|adding a language]].  If you don't have a task in mind, check out our [[:Category:Unimplemented_tasks_by_language|lists of unsolved tasks]].

=The Basics=

Quickly getting started, this is all you really need to do.

==Copyright==

Please familiarize yourself with [[Rosetta Code:Copyrights|Rosetta Code's copyright policies]]. If you want the layman's summary: Don't post code you don't have permission to, and be aware that you're giving us (and others) permission to use it under specific conditions.

==Solve the task==
It is best to read the task thoroughly, solve it, and check your solution ''before'' starting to edit the Rosetta Code page, (especially for tasks needing more than a very short solution).

==Adding Code==
Language examples on each page are in alphabetical order, so you need to find where your example would fit.  Once you've found that, click the "edit" link closest above the area where you want to insert your code on the task page, and add something like this to the bottom of the edit field:


```txt


## Ayrch


```Ayrch
PRINT "Goodbye, World!"
```


```

Remember, for the sake of simplicity, we're assuming your language is Ayrch, and the task is [[Hello world]].  We're also assuming, for the moment, that Ayrch looks a lot like BASIC.
Once you've added your code, hit the preview button to make sure you crossed all your T's and closed all your tags. If the language name shows up in red (a broken link), then either the language doesn't exist on the site yet (as a category), or you misspelled/mis-capitalized the name. Check your spelling against the one in [[:Category:Programming Languages]].
That's all you really need to do!

=Going a little further=

If you want to give your code that spit and polish shine, there are a few more steps you can take.

==Comments and Description==

Consider adding descriptions to your code examples, to help the reader understand what's going on.  This is particularly helpful if your code or language paradigms are very unlike ones that are already commonly known.  Regardless, it's considered good practice in any environment where you would like other people to understand what you've written.

==Libraries==

It's perfectly all right to depend on external (or even non-standard) libraries in your code examples.  However, it can be problematic for others if they don't know they need to use a library, or don't know where to find it.  There's a template for that: '''libheader'''.


```txt


## Ayrch


{{libheader|Ayrch Console Extensions}}


```ayrch
PRINT "Goodbye World!"
```


```


==Works With==
Not all code works with all versions of a language, all versions of a compiler, interpreter or other implementation, or even all operating systems that the language may run on.  If you're aware of certain constraints or other prerequisites that haven't already been mentioned, try using the '''works with''' template.


```txt


## Ayrch


{{works with|Ayrch Virtual Machine|6.2}}


```ayrch
PRINT "Goodbye World!"
```



```


=Conclusion=

Thank you for adding code, and even more thanks if you added the spit and polish to make your code shine!

==Where to go?==

Now that you've solved one task, you might like to be reminded that there are [[:Category:Unimplemented_tasks_by_language|lists of all the unsolved tasks]] for all of the languages that have a presence on Rosetta Code.  If your preferred language isn't there, then you may need to go through the motions of [[Rosetta Code:Add a Language|adding a language]] in order to get the site software to automatically generate the list.
