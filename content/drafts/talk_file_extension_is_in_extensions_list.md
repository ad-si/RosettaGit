+++
title = "Talk:File extension is in extensions list"
description = ""
date = 2014-08-30T13:16:03Z
aliases = []
[extra]
id = 17842
[taxonomies]
categories = []
tags = []
+++

== Dupe? ==

I know it's not exactly the same but is this a close duplicate of [[Search a list]]? This one has an odd requirement that it has to be a file extension, but the other one has the extra requirement of using an indexed list. I think they're close enough and Search a list has the advantage of already being implemented a lot and not restricting the data. --[[User:Mwn3d|Mwn3d]] ([[User talk:Mwn3d|talk]]) 16:56, 11 August 2014 (UTC)

: Well, the file extension has to be parsed, and there is the case of the filename not having an extension (so the requirement of having to have a file extension ... isn't).   The handling of insensitive case is a minor requirement.   It's a common enough task to warrant it's own task. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:28, 11 August 2014 (UTC)

::* Some filenames that have no file extensions:
::::* somefile
::::* holy smoke
::::* afile.
::::* /a/path/to/glory.yup/or_not
::::* funny...
::* Some filenames that have unusual file extensions:
::::* unusual.½xy 
::::* fly_in_the_ointment. imbedded_blank_after_the_period

-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:28, 11 August 2014 (UTC)

 -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:07, 12 August 2014 (UTC)  {added two more examples above.}

 -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 16:22, 13 August 2014 (UTC) {clarified headers as to what is or isn't a file extension.}

::I didn't read it quite right the first time and didn't see that the entire filename is an input rather than just its extension. I think it would be better to have a task for separating the file extension from the full filename without the list search task that this one is similar to. The task here is just combining two really simple tasks into an uncommon compound task. --[[User:Mwn3d|Mwn3d]] ([[User talk:Mwn3d|talk]]) 20:50, 11 August 2014 (UTC)

::: That's what I thought at first, until I (gasp!) actually read my own post) ... and found that it's not that simple (separating a file extension from a filename that may have no valid extension?) --- but certainly not that complicated, but worthy of some thought.   I went for the boilerplate solution route (for REXX). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:08, 11 August 2014 (UTC)
::::Yeah it doesn't sound very different from the search task in my head but now that I've done it I see it has some key variations--most notably (for my Java version anyway) was the case-insensitivity. --[[User:Mwn3d|Mwn3d]] ([[User talk:Mwn3d|talk]]) 17:21, 12 August 2014 (UTC)

::::: I found that the fly-in-the-ointment (above, which deals with imbedded/leading/trailing blanks) to be a bit of a thorn in one's side, but after some thought, it only took one extra REXX program statement to make it compliant, the comment dealing with it however, was a lot larger. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:57, 12 August 2014 (UTC)

== Differing definitions of 'extension' ==

By the usual Unix Way of thinking, the OS and shells are largely agnostic to the notion of filename extension, so each individual program will interpret the idea as it sees fit.  I see only two constraints: first, the convention that an extension must start with the final period in a filename, and second, that an extension cannot contain characters that are illegal in a filename, namely / or the null character.  Other than that, it's perfectly legal to have characters like ½ or space in a filename, and the extension is no different from the rest of the filename in that regard.  (Usual caveats, of course, since most filesystems haven't a clue what that you've encoded a Unicode character, and if you use spaces in an extension for your program, we will hunt you down and kill you. <tt>:)</tt>  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 00:31, 13 August 2014 (UTC)

: The extension ''might'' start at the ''first'' period. But let's not worry too much about that; this task has the benefit of being something that matches up with the sorts of things that many programmers want to do when starting out with a programming language. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 13:16, 30 August 2014 (UTC)

== Programs violating the first sentence? ==
The first sentence of the task indicates that the extensions must include the dot.  Several entries have "" in the list of extensions, which does not have a dot, and seems to indicate a desire to count filenames without extensions as if they had an extension, which I can understand, but it's technically in violation of the task.  In the P6 solution I just treated extension-less files as a third option, outside the bounds of the question of whether the extension (if present) is a member of the listed extensions.  Should the solutions containing "" as a putative extension be asked to fix that?  Or should the task allow extensions that do not begin with "."?  The latter interpretation of the task seems somewhat problematic to me.  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 00:44, 13 August 2014 (UTC)

:So, is a file ending in a dot a file with a null-string extension, and a file without any dot a file without an extension?
:Seems right to me. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:02, 13 August 2014 (UTC)

::I currently have it that an extension has to have at least one character after the dot, on the presumption that filenames might have punctuational dots at the end (as in the "<tt>funny...</tt>" example) that are not intended as extension dots.  And that this overrides the usual desire that the degenerate case work the same as the non-degenerate case.  But I don't have strong feelings either way on that.  It's just the difference between a <tt>*</tt> and a <tt>+</tt> in Regexland. --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 15:14, 13 August 2014 (UTC)
