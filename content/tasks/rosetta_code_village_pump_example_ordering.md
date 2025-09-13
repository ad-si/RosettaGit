+++
title = "Rosetta Code:Village Pump/Example ordering"
description = ""
date = 2011-05-10T19:55:58Z
aliases = []
[extra]
id = 8302
[taxonomies]
categories = ["task", "Category"]
tags = []
+++

## Task

{{Vptopic
|topic=Example ordering
|summary=correct alphabetic/ASCII ordering for languages
}}
In cleaning up the Ethiopian multiplication page I found that a few language examples were not in order and am in the process of moving them. I couldn't remember were it specifically said what the ordering should be w.r.t. case, so would like to propose that the '''examples be ordered without respect to case'''.

The other issue is the ordering of C++, C, C# C@ C... or whatever. I think it is too much to ask anyone to remember the ASCII ordering of other than alphanumerics and would ask that we ignore any mis-orderings if the ordering between language names depends on non-alphanumerics. How say you all? --[[User:Paddy3118|Paddy3118]] 10:44, 11 September 2010 (UTC)
:Case-insensitive ordering is good. I know C comes first in that list since it's shortest. By that logic C++ would come last. I'm not sure about the others. For our existing languages it would go "C, C#, C++" but rules should be made. With the advent of SMW this might be moot. I'm pretty sure using SMW we could put examples on their own pages (probably in the old Example namespace) with a tag for which task they satisfy. Then each task page could transclude those example pages. Basically a task page would look like this:

```txt
<nowiki>{{task|Category}}Description

More Description

Tests
{{task example list}}</nowiki>
```

:Then the task page would only have to be edited when the description needs to change. Maybe there could be an "Add example" button. In any case, at that point SMW would sort for us, and it would be uniform across the whole site. Until then  think we can sort languages that start with the same letters by length and that should handle most of the sorting problems. If any more arise we can fix them manually and then record a rule somewhere. --[[User:Mwn3d|Mwn3d]] 18:19, 11 September 2010 (UTC)

''From a dupliacte topic''
Each task page has the languages in alphabetical order, but the order of C# and F# is not consistent. Some pages have C# before C++. Some pages have C# after C++. Some pages have F# before Factor. Some pages have F# after Forth. [[Comments]] has C# before Chef, but F# after Fortran.

I guess that the best order is C# before C++, and F# before Factor. What is the best order? --[[User:Kernigh|Kernigh]] 18:06, 10 May 2011 (UTC)
:I think it should be C, C#, C++, Chef,... and F#, Factor, Fortran, ... --[[User:Mwn3d|Mwn3d]] 18:13, 10 May 2011 (UTC)
:For what it's worth, I wrote this quick little Java program:
:
```java5
public static void main(String[] args) {
	List<String> list= Arrays.asList("C","cpp","csharp","Chef", "C@", "C@q");
	Collections.sort(list);
	System.out.println(list);
}
```

:and it gave me this:
:
```txt
[C, C#, C++, C@, C@q, Chef]
```

:I don't think Java does anything weird, so that will probably be what most other automatic, ASCII sorts do. --[[User:Mwn3d|Mwn3d]] 19:55, 10 May 2011 (UTC)
