+++
title = "Category talk:C++"
description = ""
date = 2011-05-25T21:38:02Z
aliases = []
[extra]
id = 9773
[taxonomies]
categories = []
tags = []
+++

==std namespace==
Is there some reason none of the code examples include the line:

```C++>using namespace std;</lang


It'd make things a lot shorter and more readable. [[User:MagiMaster|MagiMaster]] 22:53, 24 May 2011 (UTC)
:It's a simple style choice. Some people like to be explicit in case other namespaces have similarly named functions. You can add the line for new examples but I don't think it's worth it to change the other ones. You could also try to organize the C++ community a la [[J/HouseStyle]]. --[[User:Mwn3d|Mwn3d]] 03:45, 25 May 2011 (UTC)
:Probably because I wrote more than a few of them (C++ is my day job, so sometimes I have the skill, if I can find the time), and I don't like the line; I've hit namespace conflicts with it, and so I avoid them by keeping things explicit. I'm also a creature of habit; even if a safeguard (such as not using '''using namespace std''') isn't necessary in a given case, I'll use it anyway, so I haven't erred in missing it, and so I can habitually avoid the conflict in the future. However, as Mwn3d noted, it's stylistic, and not part and parcel to "correct" code. There are reasonable cases where '''using namespace std''' in non-vendor code is probably appropriate. For example, I know someone who's a perfectly competent C++ programmer, but has difficulty with long identifier sequences because his typing accuracy is hampered by having large hands... --[[User:Short Circuit|Michael Mol]] 13:59, 25 May 2011 (UTC)
::Just checking. I'll include it in mine, since having std:: everywhere makes it hard for me to read, but I won't go editing existing examples. :) [[User:MagiMaster|MagiMaster]] 21:28, 25 May 2011 (UTC)
:::Hey, if there are significant fixes or improvements to existing examples, then go for it. As for '''std::''' making things hard to read...it can make things a bit dense, but the right combination of editor font, font size, syntax highlighting and typedefs usually clear that up for me. (For example, it's pretty unusual for my work-related code to have a raw description of '''std::set''' or '''std::map''', unless it's an unusually trivial use case. We typedef those to make things more readable and compact. --[[User:Short Circuit|Michael Mol]] 21:38, 25 May 2011 (UTC)
