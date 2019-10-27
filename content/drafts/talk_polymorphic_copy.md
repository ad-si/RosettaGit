+++
title = "Talk:Polymorphic copy"
description = ""
date = 2011-07-29T04:18:35Z
aliases = []
[extra]
id = 10184
[taxonomies]
categories = []
tags = []
+++

I had wanted to add a note about the statically-typed view of the task description, but on closer inspection I would also like to introduce the term "variable" instead of object in places. Something like:
:In a statically typed language, a variable is [[polymorphism|polymorphic]] when the specific type of its value may vary. 

:It is trivial to copy an object if its type is known and static, for example - in a 'C-type' language:
:
```c
int x;
int y = x;
```

:Here x is not polymorphic, so y is declared to be of the same type (''int'') as x. But if the specific type of x were unknown, then y could not be declared of any specific type.

:The task: let a polymorphic variable contain an instance of some specific type S derived from a type T. The type T is known. The type S is possibly unknown until [[run time]]. The objective is to create an exact copy of the polymorphic object (not to create a [[reference]] or pointer to the object). Let further the type T have a method overridden by S. This method is to be called on the copy to demonstrate that the specific type of the copy is indeed S.

:The task remains the same for dynamically typed languages.

If their are no significant objections, I'll update the task description. (My intention is to clarify rather than change the task requirements). --[[User:Paddy3118|Paddy3118]] 04:18, 29 July 2011 (UTC)
