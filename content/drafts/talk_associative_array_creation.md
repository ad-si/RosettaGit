+++
title = "Talk:Associative array/Creation"
description = ""
date = 2013-02-12T22:33:42Z
aliases = []
[extra]
id = 1650
[taxonomies]
categories = []
tags = []
+++

This info seems to be the as in [[Create a Hash]], I'd like to flag this for possible removal

:It looks like there's some duplication in the code examples, but the way the tasks are defined, they really address different things. (Despite the title of [[Create a Hash]].)
:Still, I'm going to move the contents of Create a Hash into Creating an Associative Array, with annotations where the behavior differs. (It's added behavior, not alternate behavior.  I'm comfortable with that, for now.) --[[User:Short Circuit|Short Circuit]] 13:46, 23 January 2007 (EST)
::Done.  Content from [[Create a Hash]] has been moved to here. ("Associative Array" is a more generic term.) --[[User:Short Circuit|Short Circuit]] 12:19, 24 January 2007 (EST)

Why does Perl have two separate sections?
:Looks like sloppy editing, perhaps on my part.  I didn't check who submitted it, I just removed the less-well-formed entry. --[[User:Short Circuit|Short Circuit]] 10:06, 25 January 2007 (EST)

== PHP ==
I don't know if it is relevant, but should we add user-made functions ? I know I often use a hand made function because it's really convenient... Anyway, here is the PHP Manual version, dunno if it should be added:

  <?
  // Append associative array elements
  function array_push_associative(&$arr) {
   $args = func_get_args();
   foreach ($args as $arg) {
       if (is_array($arg)) {
           foreach ($arg as $key => $value) {
               $arr[$key] = $value;
               $ret++;
           }
       }else{
           $arr[$arg] = "";
       }
   }
   return $ret;
  }

It's like [[http://fr2.php.net/array_push array_push ()]], up to the part that it actually allows associative elements to be added (pushed)... More info: http://fr2.php.net/manual/en/function.array-push.php#58705

== Requirements? ==

As stated, this task is rather meaningless, since it does not specify any kind of array semantics which must be supported.  We can assume "get element" and "set element", and I think the task should require examples of these assumable operations. --[[User:Rdm|Rdm]] 18:47, 5 January 2010 (UTC)
: Not a bad point. Rather than rewriting the task description, though, I think it would be a good idea to create a replacement task and phase this one out.  There aren't active contributors for all of the languages, and some languages don't get much ENA maintenance. --[[User:Short Circuit|Michael Mol]] 23:13, 5 January 2010 (UTC)
