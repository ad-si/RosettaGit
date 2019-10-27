+++
title = "Talk:Count in octal"
description = ""
date = 2011-08-26T16:23:18Z
aliases = []
[extra]
id = 9883
[taxonomies]
categories = []
tags = []
+++

== Dupe? ==

Now that I think about it, I think this task is covered by [[Non-decimal radices/Output]]. I know there's no "loop forever" in that task, but I think it covers the meat of this task (and more). Thoughts? --[[User:Mwn3d|Mwn3d]] 03:47, 6 June 2011 (UTC)

: Hmm... Seems like it. The "loop over every integer" bit isn't completely trivial, but yeah, it's probably not enough to differentiate this. [[User:MagiMaster|MagiMaster]] 03:50, 6 June 2011 (UTC)

:: The "loop over every integer" requirement is also not practical to implement.  4 billion lines (on a 32 bit machine) might be doable, in terms of time to display -- I am not sure if anyone has the patience to sit through a display of 4 billion lines, but hypothetically speaking a reasonably fast machine could complete this task.  However, on a 64 bit machine, you will die of old age before the task completes.  And, in my opinion, any task which requires the programmer be dead before the task completes is a bad task.  --[[User:Rdm|Rdm]] 20:04, 6 June 2011 (UTC)
:::Be careful with your modifiers. We have plenty of good tasks that run for "a really long time"/"forever". [[Lucas-Lehmer test]] and [[Loops/Infinite]] are good examples. In any case, if the range were smaller I would still call it a duplicate. --[[User:Mwn3d|Mwn3d]] 20:10, 6 June 2011 (UTC)
:::: I do not like those tasks either.  That said, "forever" really means "until interrupted". --[[User:Rdm|Rdm]] 20:18, 6 June 2011 (UTC)

:The other task mentions number conversion routines. For this task it may be possible to use native octal, rather than number conversion. However, this is not a requirement, so if the language does not support octal, then number conversion is permitted. IMHO It is better to split the common bases (hex,dec,oct,bin) into separate tasks, rather than having one task to implement them all, because a dedicated implementation may be smaller and more efficient than a multibase implementation, so if you are writing an application that has a primary purpose of dealing with octal numbers, it would be better to use dedicated octal number manipulation routines. --[[User:Markhobley|Markhobley]] 20:35, 6 June 2011 (UTC)
::Does one of the examples here use "native octal"? I'm not sure what that means. --[[User:Mwn3d|Mwn3d]] 20:42, 6 June 2011 (UTC)
::: Perhaps representing the octal value as a variable width ascii string (instead of a fixed integer) might be "native octal"? --[[User:Rdm|Rdm]] 21:27, 6 June 2011 (UTC)

:::: If the language natively supports representing fixed integers in octal notation, then it has native octal support. Some languages (such as awk), may support fixed integers, but do not have native octal, because manipulation has to be performed programmatically to make an octal representation. --[[User:Markhobley|Markhobley]] 23:00, 6 June 2011 (UTC)

:: I'd have to agree that native octal doesn't make much sense. All math routines work in binary, or otherwise operate very similarly regardless of base. Only input and output are different, so if the goal is to demonstrate octal manipulation, IO would be a better task. That's covered by other tasks though. [[User:MagiMaster|MagiMaster]] 21:34, 6 June 2011 (UTC)

::: I am talking about at language level, not cpu level here. --[[User:Markhobley|Markhobley]] 22:50, 6 June 2011 (UTC)

:::: It still doesn't make much sense. 0377 may be octal in C++, but that's not native. There's no type in any language I know of that's tied to a specific base in the sense you seem to be implying. Natively, things are either stored as binary for the machine's benefit, or decimal for the human's. Can you give an example of anything else? [[User:MagiMaster|MagiMaster]] 23:48, 6 June 2011 (UTC)

::::: Ok, it might a terminology thing. When I say native, I mean builtin to the language (possibly to an abstract level?). Lets take a variable A=19. From a language point of view, it is stored as a single decimal integer of value 19, from the point of view of the computer, it is stored in binary digits, but that is not a language concern. The native format of the variable is a decimal integer and its value 19, so I would say that the language has native decimal support. Does that make sense? Now, if the language also has native octal support, it could output that number as octal using a statement without evaluation code (again from a language point of view, the cpu might be doing something underneath, but that is not a language concern). If it does not have native octal support, then some digit crunching routines would have to be written within the language, to produce the required output. --00:13, 7 June 2011 (UTC)
::::::So then what language has native octal support? Or native anything but decimal support? I know in Java you can say "int a = 0xFF;" but when you print it without anything extra it will come out as "255". Does that still count as "native" to you? --[[User:Mwn3d|Mwn3d]] 00:57, 7 June 2011 (UTC)

:::::: So what you're looking for is something like "octal x = 12; print x;" giving back 14?. By that description, I'd bet nothing has native octal support, or native support for anything but decimal or, in some rarer cases, hexidecimal. (You could easily build such a thing in C++, but it would just be wrapping the number crunching into a class, which wouldn't be much different than the existing output modifiers.) [[User:MagiMaster|MagiMaster]] 01:30, 7 June 2011 (UTC)

::::::: I would say thay a language that has output specifiers for octal has native octal support. --20:14, 7 June 2011 (UTC)
:::::::: What does that mean?  For example, is this an output specifier for octal?
:::::::: 
```j
   8 #.inv 255
3 7 7
```
  --[[User:Rdm|Rdm]] 20:36, 7 June 2011 (UTC)
:::::::: Yes --[[User:Markhobley|Markhobley]] 23:30, 7 June 2011 (UTC)

::::::::FYI, that other task ([[Non-decimal radices/Output]]) used to be called "Common number base formatting", which I think is a better indication for what that task did than the title now, because it included the word "Common". That task was ''precisely'' for languages with output specifiers or specialized methods for ''specific'' bases, namely octal and hex (and binary in some). General base conversion routines for any base should not be there, and should instead be covered in the [[Non-decimal radices/Convert]] (which used to be called "Number base conversion") task. --[[User:Spoon!|Spoon!]] 07:10, 9 June 2011 (UTC)

::::::::: Octal, Hexadecimal, Decimal and Binary are special cases. Some languages have native support for these, some do not. So, conversion is required for some languages, and other languages do not require the conversion. This task neither requires or prohibits conversion, so best methods for handling an octal counter can be implemented here. The Non-decimal radices tasks either require conversion (not ideal for languages that have native support), or do not require conversion (not ideal for languages that do not have native support). Additionally counting does not necessarily involve conversion methods. This really should be a separate task IMHO. [[User:Markhobley|Markhobley]] 17:53, 5 July 2011 (UTC)
