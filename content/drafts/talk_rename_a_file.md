+++
title = "Talk:Rename a file"
description = ""
date = 2016-07-02T06:09:51Z
aliases = []
[extra]
id = 9771
[taxonomies]
categories = []
tags = []
+++

==REXX==

It looks like [[Rename a file#REXX|the REXX solutions]] are incorrect. To review, here they both are, version 1:

```rexx
do 2
  'RENAME' "input.txt"  "output.txt"
  'CD'     "\"
  'MOVE'   "\docs"  "\mydocs"
end
```

  
and version 2:

```rexx
do 2
  'RENAME' "input.txt  output.txt"
  'CD'     "\"
  'MOVE'   "\docs  \mydocs"
end
```


Problem is, it looks like they both fail to rename "docs" in the current directory. I '''''think''''' that the proper solutions would be more along the lines of these -- version 1:

```rexx
do 2
  'RENAME' "input.txt"  "output.txt"
  'MOVE'   "docs"  "mydocs"
  'CD'     "\"
end
```

  
and version 2:

```rexx
do 2
  'RENAME' "input.txt  output.txt"
  'MOVE'   "docs  mydocs"
  'CD'     "\"
end
```


...but I don't know enough about REXX to be able to say either way -- certainly not enough to mark the existing solutions as incorrect, or to know for sure if mine are right.

Any REXXers want to review what's here? -- [[User:Eriksiers|Erik Siers]] 03:20, 24 May 2011 (UTC)

: I've updated and fixed the original REXX programs.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:09, 2 July 2016 (UTC)
