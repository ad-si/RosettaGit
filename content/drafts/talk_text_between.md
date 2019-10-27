+++
title = "Talk:Text between"
description = ""
date = 2018-01-06T18:00:22Z
aliases = []
[extra]
id = 21704
[taxonomies]
categories = []
tags = []
+++

I think the specification for this problem needs some more information, specifically:

How should implementations behave if the end delimiter is found in the search string before the start delimiter?
: This has been clarified. --[[User:Zbentley|Zbentley]] ([[User talk:Zbentley|talk]]) 18:00, 6 January 2018 (UTC)

Must an error be raised when a delimiter is not present in the search string? Some implementations do this, and some do not.
: This has been clarified. Some implementations will need to be fixed. --[[User:Zbentley|Zbentley]] ([[User talk:Zbentley|talk]]) 18:00, 6 January 2018 (UTC)

Are null/missing (or equivalent) delimiters allowed?

Are empty-string delimiters allowed? If so, how should they work? Are empty-string start delimiters equivalent to "start" special delims, and empty-string end delimiters equivalent to "end"?

If the start delimiter occurs multiple places in the search string, which one should be used? I think it should be the first occurrence, i.e. both start-search and end-search are eager. --[[User:Zbentley|Zbentley]] ([[User talk:Zbentley|talk]]) 18:00, 6 January 2018 (UTC)

# Thanks for the very welcome expansion of examples and spec. Perhaps there's a minor glitch in the current draft of Example 3 ? The output appears to have dropped a leading space. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 13:23, 6 January 2018 (UTC)
