+++
title = "Talk:Create an object/Native demonstration"
description = ""
date = 2015-05-07T13:26:05Z
aliases = []
[extra]
id = 8373
[taxonomies]
categories = []
tags = []
+++

'''It is not yet considered ready to be promoted as a complete task, ''' because I'm not a native English speaker, my task description may cause misunderstanding. Hope that is a minimium, thanks. [[User:Dingowolf|dingowolf]]

== Tcl omitted ==

I marked this task as an omission for [[Tcl]] because the value system and object system in that language do not interact in a way that is necessary to support the prerequisites of this task. In particular, native dicts and arrays are not objects in the sense this task assumes, and Tcl's objects formally occupy part of the space of functions and commands, and not values or variables. –[[User:Dkf|Donal Fellows]] 08:38, 20 October 2010 (UTC)

:I am not sure that that was the right choice here.  The task is titled "Create an Object/Native ...", and while TCL might not have objects it certainly has native stuff?  Anyways, this site has always been about closest approximations (though I have been known to quibble about where we draw the boundaries on those issues).  --[[User:Rdm|Rdm]] 12:46, 20 October 2010 (UTC)

:: The [[Perl]] solution will need to use [http://perldoc.perl.org/perltie.html perltie] to tie a <tt>%hash</tt> to some <tt>$object</tt>, so that <tt>%hash{$key} = $value</tt> calls the magic method <tt>$object->STORE($key, $value)</tt>, and one writes code for <tt>STORE</tt>. If Perl has no perltie feature, I would omit this task from Perl. If Tcl has both dicts and objects, but Tcl has nothing like perltie, then one should omit this task from Tcl. --[[User:Kernigh|Kernigh]] 02:51, 17 February 2011 (UTC)

::: Hmm, I suppose I could use a trace on an array to stop someone from changing it (which would be similar in spirit to the perltie approach). Ugly as anything though. It's also not really what I'd call writing a ''native'' implementation of the “object”; it's layering a whole load off stuff over the top instead. Dictionary values work just as fine for non-modifiable (as Tcl's value-model is based on immutable values) but there I think it's violating the spirit of the task. –[[User:Dkf|Donal Fellows]] 12:19, 10 March 2013 (UTC)

:::: Just for the fun of it, I have added a Tcl implementation with variable traces. - [[User:Suchenwi|Suchenwi]] ([[User talk:Suchenwi|talk]]) 13:26, 7 May 2015 (UTC)

== wrong name? ==

I think this task might have the wrong name. It might be better to call it "immutable map" or something like that. It's not so much a demonstration of creating an object and "native demonstration" doesn't make sense to me. --[[User:Mwn3d|Mwn3d]] 22:31, 16 February 2011 (UTC)
:Sorry to causing misunderstanding. I think Ruby's demonstration match, and in fact correct, my idea. The object requirements should be 1. No new KEY could be added, and KEY set at initialization cannot be removed ; 2. VALUE can be modified and can be reset (by method delete/reset/clear etc. whatever appropriate in the language) to DEFAULT VALUE which has been set at initialization. But the object creation task is just a means to expose those Magic Method, which is the real goal of this task. For example, one can make a hash class object that access values by key by using a method '''getValueByKey''', or by using a Native(?) method, for example in D, '''opIndex'''. It is this '''opIndex''' that I called Magic Method. I cannot express my idea well :( [[User:Dingowolf|dingowolf]] 23:19, 16 February 2011 (UTC)

:I too can see that it is '''not''' the fact that you are indeed creating an immutable map that is the point. It is how to make an object that acts just as a map does/should do, but with your outlined changes. I cannot come up with a better name than you have, at the moment.Oh wait, how about ''"Emulating a native type"'' ? --[[User:Paddy3118|Paddy3118]] 02:34, 17 February 2011 (UTC)

:I am the author of the Ruby code in progress. I chose the name "fenced hash" in the Ruby code, but "fenced hash" might be bad name of page. I had trouble with two parts of the task description. First, I had to guess what "No new item can be added" does mean. For the Ruby code, I decided that no new KEY can be added. Second, I know what is "native hash", but I am not understanding what is "native way of object creation" or "native demonstration". The word "native" would have different meaning there. After I finish the Ruby code, I might edit the task description. After we fix the task description, we should drop "native demonstration" name. --[[User:Kernigh|Kernigh]] 02:51, 17 February 2011 (UTC)
:: And what do you think about the name ''"Emulating a native type"'' ? --[[User:Paddy3118|Paddy3118]] 05:12, 17 February 2011 (UTC)
::: More specifically, how about something in the direction of [[Associative Array/Emulation]]? That should relate well to [[Associative arrays/Creation]], and include a more specific hint as to what kind of native type is being emulated. --[[User:Short Circuit|Michael Mol]] 13:45, 17 February 2011 (UTC)
:::: I am not sure whether "Emulating an associative array" or "Emulating a native type" is better. I hate the names of the [[Associative arrays/Creation]] and [[Associative arrays/Iteration]] pages, because the slash suggests that these are subpages of an "Associative arrays" page, but there is no such page. (The [[Associative array]] page has no final "s".) --[[User:Kernigh|Kernigh]] 23:08, 21 February 2011 (UTC)
::::: Should we rename those by dropping the s?[[User:Markhobley|Markhobley]] 22:58, 6 July 2011 (UTC)
