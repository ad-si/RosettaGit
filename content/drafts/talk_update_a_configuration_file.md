+++
title = "Talk:Update a configuration file"
description = ""
date = 2016-12-03T13:46:49Z
aliases = []
[extra]
id = 9738
[taxonomies]
categories = []
tags = []
+++

== Task spec issues ==
This task spec seems needlessly complex.  For example "In commenting out an option, any duplicates should also become commented." does this mean that changes should be made by commenting out the old option instance and adding a new line?  If not, how can we satisfy this requirement in the task implementation?  --[[User:Rdm|Rdm]] 11:33, 20 May 2011 (UTC)

: He's trying to get us to duplicate the (over-complex IMO) functionality that he put into his implementation of this task. –[[User:Dkf|Donal Fellows]] 13:38, 20 May 2011 (UTC)

Ok, I have reworded that so that duplicate entries can simply be removed.

--[[User:Markhobley]] 15:46, 20 May 2011 (UTC)

:It's still a requirement beyond the scope of the task -- hypothetically speaking, someone could implement this functionality using assert("FIXME") and documentation that the person using the program should manually edit the binary image using a debugger, and no user interaction would be required for the task.  And, it's not the only such requirement, either.  --[[User:Rdm|Rdm]] 16:01, 20 May 2011 (UTC)

:: I'm not sure what that means. --[[User:Markhobley|Markhobley]] 22:44, 3 June 2011 (UTC)

:All that stuff about duplicate entries and duplicate semicolons is B.S. If the update program doesn't produce such crap, it shouldn't have to deal with it. Either you use the program for updating the file, or you're on your own.[[Special:Contributions/24.85.131.247|24.85.131.247]] 06:26, 22 October 2011 (UTC)

:: The duplicate option entries and duplicate semicolons could've been placed there by something other than the (an) update program. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:00, 12 May 2015 (UTC)

:: It's not explicitly stated, but I assumed that in   ''manipulating the configuration file'',   that phrase meant updating (the original) file that was used for input.   I've implemented that concept in the REXX (version 1) entry, and it does add complexity to the program.   But it does update the original input configuration file (data) program and it doesn't leave another copy of the input laying around.   Plus, it has the advantage that you can execute the REXX program again, and the input (and output) files don't change (the 2<sup>nd</sup> time). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:00, 12 May 2015 (UTC)

:: The phrase   ''lines beginning with a ...''   could be interpreted a few ways.   Does it mean that it is to start in column one of the record, and/or does it (or can it) have leading blanks before (say) the hash character?   If the former, than it could say   ''lines starting with (in column one) ...''   which would read better. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:00, 12 May 2015 (UTC) 

:: The phrase   ''If a configuration option does not exist with the file''   is a bit vague.   What does   ''<big><big>a</big></big>   configuration option''   mean?   One (or any) of the aforementioned options mentioned earlier in the Rosetta Code tasks?   Explicitly stating which options would be clearer and unambiguous. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:00, 12 May 2015 (UTC)  

:: What about the case of:

```txt

; prunes 10
peaches 20
apples none
prunes 48

```

:: and let's say that the PRUNES option setting to be "un-commented"   (as per a task requirement).   Then, the 2nd PRUNES option setting would be deleted as per a stated rule   (as there is now a duplicate PRUNES option).   Is this what the author had in mind? -- to invalidate an existing option setting when "un-commenting" an option setting?   It almost looks like the task's requirements might possibly require a two-pass solution for the above scenario. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:02, 12 May 2015 (UTC)

:: If the author of the task wanted to have duplicate entries (as well as spurious duplicate semicolons) removed, there should be some examples of each in the input file (plus the addition of same wouldn't change the output file, even at this late date). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:00, 12 May 2015 (UTC)

Mine, as many others, have the options to be changed/added hard coded in the program.
Wouldn't a second input file, specifying the desired CHANGES be more reasonable?
Such as

```txt
disable this
enable that
set blabla to value
remove otheroption
...
```

--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 06:05, 12 May 2015 (UTC)

:: Reasonable, yes, --- even desirable.   But as far as this Rosetta Code task (as it exists now), it would complicate the programming to have two input files (one for data, another for directives), and of course, it would be too late to change the tasks, even if it were a minor change.   What I was asking the task's author is clarification of the existing task's requirements, assumptions, wording, and/or (implied) directives. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:45, 12 May 2015 (UTC)
::: We could (and should) create another task then. "Update a configuration file from specs". How do others feel about that? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 07:15, 12 May 2015 (UTC)

:::: I would like to wait and get my (above) voiced concerns/queries answered/addressed first. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:36, 12 May 2015 (UTC)

== A generic solution for the problem ==
Well, as I saw in the previous comments, the task was really complex. And it could be, so I did a generic solution for the problem:

# The solution in BASIC (QB45) reads ANY configuration file and shows what it found.
# If there is an array (FAMILY John Smith, Lisa Smith, Charles Smith) the program will create an array of elements FAMILY.
# If FAMILY variable is repeated in another part of the configuration file, will simply add the found elements to the existing array.
# The main procedure can toggle the comment status of a variable in the configuration file as per User request.
# The main procedure will save all the elements of an array in just one line. This way, if you have a situation like this:
#:
#:FAMILY John Smith, Lisa Smith, Charles Smith
#:FRUITSILIKE banana
#:FAMILY Donna Smith
#:FAMILY Rachel Smith
#:
#:Will end as follows:
#:
#:FAMILY  John Smith, Lisa Smith, Charles Smith, Donna Smith, Rachel Smith
#:FRUITSILIKE banana
#:
# The main procedure is able, when saving, to record the variables and values with a "=" or with a space (" ") in between. The configuration file reader can identify both of them.
# The program can identify if a variable name is an array or not, and, if so, to return the number of elements in the array.
# The above explained functionality lead to a very large program. Nevertheless, as per my understanding, it amply meets the requirements of the task.
