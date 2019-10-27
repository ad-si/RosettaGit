+++
title = "Talk:Window creation"
description = ""
date = 2010-02-06T12:58:14Z
aliases = []
[extra]
id = 1620
[taxonomies]
categories = []
tags = []
+++

==Clarification==

These examples show a weakness of this site: Questions needed to be better formed.


The windows that are created here, while undeniably windows, are also undeniably useless.

A better formed contract is this:

Write a program which instantiates a main window with the following properties:

 1. Has an absolute size of 400 x 300
 2. Can be resized to as large as 800x600
 3. Has a title of "Greeting!"
 4. Has a child widget in the middle which is a text area with two lines of text: "Greeting:" and "Hello, World". The text should be selectable.
 5. Has a menu bar with file and edit entries. In file there should be a "Quit" entry which quits the program. In Edit there should be "Copy" entries. It should copy the selected text to the main clipboard.
 6. This should not be a cursor grabbing dialog box, it should be a main window.
 7. It should have a background color of green.
 8. It should accept commandline arguments to replace the two lines of text.

These are perhaps out of order, but I think people will get the idea. This is a skeleton for a real program. You could modfiy this and get some small but useful thing done.

:I agree and have flagged the page for clarification so anyone better suited than I can see this. There are also many types of windows: translucent, borderless, fullscreen, etc. This page definitely needs a concrete definition. --[[User:Macawm|macawm]] 16:01, 24 January 2007 (EST)
::I don't think including widgets in the coding example is appropriate in this case.  I ''could'' see the above example as a part of a new "Program Template" class, similar to "Programming Tasks".  I would fully support the creation of such a class.  Heck, I'd do it myself right now if I wasn't already half-asleep and afraid of screwing it up.

::For this task, I would want to stick to the basics.  I've never had the courage to try GUI programming in anything but VB; the programming examples described in this article have illuminated how to get started.  Adding in a lot of code for additional widgets would make the code more difficult to read for beginners.
::: On the other hand, there's never going to be TCL/Tk code for "just creating a window" since that is automatic and implied in the use of Tk in the first place. The single line '''pack [label .l -text "Hello"]''' would create a window and a label and place the label in the window and display the window on the screen. I don't know what it would even mean to "just create a window" without actually doing anything with it. [[User:Sgeier|Sgeier]] 20:20, 30 January 2007 (EST)

::This also illuminates why it's important to start categorizing programming tasks.  There should be a task for adding each kind of generic widget, but that would overpopulate the existing programming tasks. --[[User:Short Circuit|Short Circuit]] 23:32, 25 January 2007 (EST)

::Perhaps I should have been more clear as to which aspect of the quote I was agreeing with, sorry. I was simply pointing out that I feel this task needs a specific, singular definition. For example, how to create a basic window with a few properties such as a title, a size, and supports resizing. That is all. --[[User:Macawm|macawm]] 10:49, 26 January 2007 (EST)

:::I don't want to lose the functionality offered by the SDL example.  In systems like DirectX, OpenGL and SDL, there may not be a direct abstraction corresponding to things like Window Size and resizability.  Perhaps that could be split off into another task? --[[User:Short Circuit|Short Circuit]] 09:01, 1 February 2007 (EST)

::: I made a task along the lines of something of a "[http://www.rosettacode.org/wiki/Simple_Windowed_Application basic window that actually has a function]" and filled in the IDL and Tcl examples. Have a look at it ;-) [[User:Sgeier|Sgeier]] 23:05, 19 February 2007 (EST)


### Hmm

I don't necessarily know if that's correct. If we take the name of the site, Rosetta stone, to be some indication of purpose, then these little snippets are far from useless.  They serve as a way to compare doing something, simple or complex, in a variety of different languages.  I think it's quite educational, and pretty entertaining, to see the vast differences even on this small question.

Besides, there's no reason why you can't add your question there as another one.  But understand its going to be far easier to get users to contribute snippets than skeletons.

Maybe what you actually have there are 8 different questions? Then you could draw the answers together to form your own skeleton. 
[[User:Frater|Frater]]

:I agree with Frater, snippets are better than skeletons, the purpose of this site (in my opinion) is to just see tid bits of code to get a person going. Not a full blown app. Once the person has an idea of what to do then he/she can go to the language/library's website for further information. --[[User:Adonis|Adonis]] 16:13, 24 January 2007 (EST)

==hmmm2==
can't say I'd wholly agree either:

```txt

> 4. Has a child widget in the middle which is a text area with two lines of text: "Greeting:" and "Hello, World". 
> The text should be selectable.
> 5. Has a menu bar with file and edit entries. In file there should be a "Quit" entry which quits the program.
> In Edit there should be "Copy" entries. It should copy the selected text to the main clipboard.

```

Both of these, while certainly doable, would take away from the simple task of getting a window open with SDL
They would require lenghty code and would make this tasks page monsterous in size for even just a few examples.

Calvin

==Cite Notes==

Removed from C+SDL example.

 Submitted by Calvin Arndt 2007-01-21 2:49pm CST

:Thanks Calvin! --[[User:Short Circuit|Short Circuit]] 19:32, 23 January 2007 (EST)
