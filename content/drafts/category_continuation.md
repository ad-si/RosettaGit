+++
title = "Category:Continuation"
description = ""
date = 2011-03-14T02:33:15Z
aliases = []
[extra]
id = 9370
[taxonomies]
categories = []
tags = []
+++

{{library}}
[[Ruby]] 1.9 moved the method <tt>Kernel#callcc</tt> and the class <tt>Continuation</tt> from the core language to a standard library. Starting with Ruby 1.9, programs that use these features must <tt>require 'continuation'</tt>.


```ruby
# This code works with both Ruby 1.8 and Ruby 1.9.
require 'continuation' unless defined? Continuation
```


Most Ruby programs will never use this library.

# [[MRI]] has a slow implementation of continuations. 
# Continuations make spaghetti code with very confusing control flow.


----
<tt>Kernel#callcc</tt> creates a continuation. <tt>Continuation#call</tt>, also known as <tt>Continuation#[]</tt>, continues the program from the place that called <tt>Kernel#callcc</tt>. With a continuation, you can continue a function call after it ends.


```ruby
def f
  puts "1st line of output"
  callcc { |cc| return cc }  # f ends with a return...
  puts "3rd line of output"
  return nil
end

cont = f
if cont
  puts "2nd line of output"
  cont.call                  # ...but this continues f
end
```


<tt>Kernel#callcc</tt> from Ruby is like <tt>call-with-current-continuation</tt> from [[Scheme]], and almost like <tt>setjmp</tt> from [[C]], except that <tt>setjmp</tt> saves less information. (With <tt>setjmp</tt>, you must not continue a function call after it ends, because C frees the stack frame when it ends.)
