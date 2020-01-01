+++
title = "Talk:Classes"
description = ""
date = 2011-07-02T04:11:33Z
aliases = []
[extra]
id = 2122
[taxonomies]
categories = []
tags = []
+++

The task here is not specific enough. The task should an object that does something very simple, for example, a person which has a first name and last name and a method to return full name or a shape returning its area, etc.
:I think it still works. Adding comments is simple enough for explanation. --[[User:Mwn3d|Mwn3d]] 16:45, 9 March 2009 (UTC)

==Destructor==
What do you guys think about adding a destructor requirement where appropriate? --[[User:Mwn3d|Mwn3d]] 16:45, 9 March 2009 (UTC)
: I think it's a good idea to show, but not worth putting as a requirement. —[[User:Dkf|Donal Fellows]] 09:09, 3 September 2009 (UTC)

== Classes as Objects ==

Some object systems have classes as being entities in the object system: there's a class of classes. Others do not. Is this worth mentioning here (possibly with a link to a task to show what's going on)? (Also, some of the object systems that have a class of classes also allow subclassing of that class...) —[[User:Dkf|Donal Fellows]] 09:12, 3 September 2009 (UTC)

== The point of the C code? ==

Isn't the C sample here a bit useless?  After all this work, one ends up with a blob of data, a few functions intended to operator on said data, a lot of ugly long identifiers but no way to do inheritance or polymorphism, because nothing is provided for method dispatching base on type or object.  Where's the OO in this? --[[User:Ledrug|Ledrug]] 22:00, 19 June 2011 (UTC)
: When I read the task, the requirements boil down to this sentence: "The purpose of this task is to create a basic class with a method, a constructor, an instance variable and how to instantiate it. ". It appears to only cover encapsulation, not polymorphism or inheritance. There's nothing in there about polymorphism or dispatching based on type. However, I think it may be perfectly appropriate to create multiple tasks, one to an aspect of OO, and deprecate this task in favor of them. That's likely to produce far better results. --[[User:Short Circuit|Michael Mol]] 10:30, 20 June 2011 (UTC)
: While C isn't OO, it can be used to create class-like things.  If you stick some function pointers in a struct, it can start to act like a rudimentary class from any number of OO languages.  With some work, one struct could also "inherit" from another by copying function pointers and data out of one class and into a second.  While none of this would happen automatically, it could be made to happen. Consider:

```c
#include <stdio.h>
#include <malloc.h>

struct foo{
  int some_int;
  void (*new)(struct foo *, int);
  void (*print)(struct foo *);
  void (*clean)(struct foo *);
};

void foo_new( struct foo * self, int a ){
  self->some_int = a;
}

void foo_print( struct foo * self ){
  printf( "%d\n", self->some_int );
}

void foo_clean( struct foo * self ){
  free( self );
}

struct foo * foo(){
  struct foo * new = malloc( sizeof( struct foo ) );
  new->new = &foo_new;
  new->print = &foo_print;
  new->clean = &foo_clean;
  return new;
}

int main(){
  struct foo * bar = foo();

  bar->new( bar, 42 );
  bar->print( bar );
  bar->clean( bar );

  return 0;
}
```


Now you could use some other "class" (struct + function pointers) to change the function pointers in some instance of struct foo, emulating some very basic polymorphism.  While it's not the best practice, some_int could be a pointer, allowing you to put different types of data "in" the struct, allowing the rest of the code to call foo->print while remaining oblivious to what's really there.

Convoluted, sure, but it works and allows for some OO-like programming.

--[[User:Bnlott|Bnlott]] 22:59, 1 July 2011 (UTC)
::Well sort of.  For you code to compile, the compiler has to know how to dereference <code>bar->new</code> to begin with, meaning the type is already known, so it's not polymorphism by type.  You can change function pointers on each instance, so it is dynamic dispatch per instance at run time.  The value of this can be questioned: how is this better than storing relevant attributes in the struct, and let handler functions do different things accordingly? (note: it can be different, I'm just saying you have to think about it before designing such an interface--syntax sugar, if sugar at all, may come with drawbacks).  Secondly, things like inheritance is still not supported at language level, if you want to add a new attribute and a new method to bar, the struct definition need to be changed, existing methods need to be type-coerced, you need to do a lot of type casting using them, and code becomes a mess real fast.  And because each instance has its own function pointers, when you create a new instance, you now need to remember to copy all of them, at each level of inheritance, in correct order, etc.  You main() function may look more "OO" like, but it's not sustainable when you need to extend it.  The real question is, if you <i>need</i> OO, why use C? --[[User:Ledrug|Ledrug]] 23:32, 1 July 2011 (UTC)
:::You are correct when you say that this isn't the most convenient thing ever.  For this to have anything like true polymorphism, the code will either become messy or dangerous (possibly both).  While you are correct that you shouldn't be using C if you really need OO, things like this can make you life easier.  Consider a contrived example: you have a DB access layer like Perl's DBI or Java's JDBC.  You could have something like:


```txt
struct DB_access{
+ bool:connect()
+ char**:getData()
+ void:disconnect()
}

mysql_connect();
sqlite_connect();
/* etc... */

mysql();
sqlite();

struct DB_access * DB = sqlite();
struct DB_access * DB = mysql();
```


The idea behind switching the function pointers around is to avoid the switch/case and if/else blocks that can result from trying to make single functions handle things differently.  This hardly gives you access to all of the capabilities that true OO languages provide, but it does allow for many of the OO design principals to be applied in C.

Take this with a large grain of salt; this needs to be done carefully to avoid ending up in data type hell.  It certainly shouldn't be used recklessly.  Like I said, it's not really OOP, just an imitation that allows for some OO design principals to be applied.  --[[User:Bnlott|Bnlott]] 00:12, 2 July 2011 (UTC)

::Yes I understand that.  It's just sometimes people get too excited and starts abusing some of these techniques.  It roughly goes like this, in order:

::# "Oooh let's make the struct inheritable" -- make struct foo "has-a" struct bar and extra members, typecast struct foo* to struct bar*;
::# "Oooh let's make it run-time polymorph" -- add function pointers to struct bar; function looks like method(bar *, ...) but clunky when used on foo; fine, change prototype to method(void *, ...)
::# "But inheritance is not convenient?" -- inplement some kind of vtbl, a global table storing method pointers, while first member of foo and bar refer to this table;
::# "But foo's vtbl and bar's vtbl should not be the same?" -- make foo's vtbl an inherited form of bar's; change the method lookup method--

::Notice how the problem of polymorphism of foo and bar now includes polymorphism of foo and bar's vtbls, and we have a meta recursion, goto 1, sanity lost.  It's not pertinent to the code example or your comments, just some amusing observations. --[[User:Ledrug|Ledrug]] 00:34, 2 July 2011 (UTC)

:::As a general rule of thumb, if you ever find yourself implementing half of language B in language A, just go use language B.  :)  --[[User:Bnlott|Bnlott]] 01:10, 2 July 2011 (UTC)
::::Sure, given reasonable circumstantial constraints. However, just to drop a bit of RC-specific perspective, seeing how to implement language B in language A ''greatly'' makes understanding that half of language B ''much'' easier for someone more versed in language A. Comparing and demonstrating languages is only part of what RC does; demonstrating tasks through code is another major part.  --[[User:Short Circuit|Michael Mol]] 04:11, 2 July 2011 (UTC)
