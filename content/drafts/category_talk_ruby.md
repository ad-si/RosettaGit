+++
title = "Category talk:Ruby"
description = ""
date = 2011-08-27T03:49:28Z
aliases = []
[extra]
id = 4627
[taxonomies]
categories = []
tags = []
+++

== Pass by reference? ==

How is Ruby pass-by-reference? The passing of values in Ruby (which are all references) has exactly the same semantics as the passing of reference type values in Java. (Is it not?) Java is listed as pass-by-value only. So to be consistent we should list Ruby as pass-by-value. --[[Special:Contributions/76.91.63.71|76.91.63.71]] 20:02, 2 August 2009 (UTC)

: Ruby is pass-by-value as you say. I changed the page from 'parampass=reference' to 'parampass=value'. I also changed it from 'execution=bytecode' to 'execution=interpreted'. Ruby 1.9 compiles a program to bytecode and executes the bytecode, but this is an internal detail of the interpreter. There is no Java .class or Python .pyc file. --[[User:Kernigh|Kernigh]] 01:44, 9 March 2011 (UTC)

----
This program shows that Ruby passes by value:


```ruby
# Ruby
def f(x)
  x = "different string"
end

x = "original string"
f(x)
puts x  # => "original string"
```


If Ruby would pass by reference, then this program would print "different string". But it prints "original string", because the two <tt>x</tt> have distinct values. Contrast [[Perl]], which passes by reference:


```perl
# Perl
sub f {
  $_[0] = "different string";
}

my $x = "original string";
f($x);
print "$x\n";  # => "different string"
```


This program prints "different string" because Perl passes <tt>$x</tt> by reference, so <tt>$_[0]</tt> is an alias of <tt>$x</tt>. (Many Perl subs use <tt>my $x = shift;</tt> to create a distinct value.)

Ruby would act like Perl if the Ruby program would use <tt>x.replace "different string"</tt>. This would work because <tt>x</tt> is a reference to a mutable string. I think that Ruby is pass by value because I can use assignment to change the value of <tt>x</tt>, to refer to some other string. I am reverting the page to 'parampass=value', after 74.215.210.198 reverted the page to 'parampass=reference', after I changed it to 'parampass=value'. --[[User:Kernigh|Kernigh]] 02:31, 16 March 2011 (UTC)

-----

Ruby passes by reference since it don't make copies of the objects when passing then to a method. Look:


```ruby
def f(x)
  x.replace("different string")
  x << " from method"
  x = "another variable"
end

x = "original string"
f(x)
puts x  # => "different string from method"
```


All variables are, internally, pointers to objects, so, when I pass the variable, it passes the reference to the object, that allow me to change directly the object. But I can't set the variable because when you set it inside the method you are creating another variable that have nothing to do with the old one. I'm reversing the change on "parampass", to 'reference'.

[[Special:Contributions/189.104.25.246|189.104.25.246]] 02:03, 10 July 2011 (UTC)

: With Ruby, <code>x = "string"</code> never creates a variable, unless there is no <code>x</code> in scope. I can check this with a closure:

: 
```ruby
def f(x)
  x.replace("different string")
  x << " from method"
  $p = proc { x }
  x = "another variable?"
end

x = "original string"
f(x)
puts x        # => "different string from method"
puts $p.call  # => "another variable?"
```


: Some other languages are simpler. With [[Common Lisp]], <code>(let ((x "string")) ...)</code> creates a variable and <code>(setq x "string")</code> sets it. With [[Factor]] (inside a <code>[let ... ]</code> block), <code>"string" :> x!</code> creates it and <code>"string" x!</code> sets it. With Ruby, <code>x = "string"</code> can either create it or set it. This is only important if some closure or binding captures <code>x</code>. --[[User:Kernigh|Kernigh]] 04:00, 11 August 2011 (UTC)

:Ruby, Python, Scheme, Java (when talking about objects), etc.; all of these pass by value. People who argue against pass by value seem to have a different idea of what is being passed. They think that you are "passing" an "object". But that fact is, in all these languages, the values in the language are "references" (or pointers, or whatever you call them; the idea is the same: there is a level of indirection). You can never manipulate an "object" directly in these languages; you can only do it through references (you can see that values are references by assigning one variable to another and seeing that they point to the same object, hence a reference is copied, not the object). So when you are passing stuff, the values you are passing are the references, and those references are copied. In a pass-by-reference language, you would be able, in the function, to change the thing that is passed -- the reference -- in the calling scope; but you can't in Ruby; you can verify this by checking x.object_id to see that the reference still points to the same object.
:People tend to point out that if these objects are mutable (e.g. String in Ruby), then it is possible to use its mutation methods to mutate its internal state in a way that is visible to other people who have a reference to the same object; but this is irrelevant. Because it is not the object that is being passed -- it is the reference. We can separate out the mutation issue by using an object type that is not mutable, e.g. take an integer or float or boolean in Ruby, and passing it; see that you cannot affect it in the calling scope; whereas in a true pass-by-reference language, you always can. --[[User:Spoon!|Spoon!]] 07:01, 11 August 2011 (UTC)
::That said, note that this point of view is that you cannot pass an object to a method.  --[[User:Rdm|Rdm]] 12:33, 11 August 2011 (UTC)
:::Right; not only can you not pass an object, the point of view is that no expression in the language has the value of an object, only the value of a reference. This is the same point of view as in Java, Python, etc. Whatever we decide has to be consistent across all these languages. --[[User:Spoon!|Spoon!]] 19:44, 11 August 2011 (UTC)

== Phantom categories and Ruby's standard library ==

A few parts of Ruby's standard library have their own categories.

# <code>require 'curses'</code> => [[:Category:Curses]]
# <code>require 'rexml/document'</code> => [[:Category:REXML]]
# <code>require 'tk'</code> => [[:Category:Ruby/Tk]]

I am not wanting categories for most other parts of the standard library. I destroyed 11 ''phantom categories'' by removing all their members.

# Category:bigdecimal (2 members)
# Category:complex.rb (1 member)
# Category:dRuby (1 member)
# Category:fileutils.rb (1 member)
# Category:mathn (1 member)
# Category:matrix (2 members)
# Category:matrix.rb (3 members)
# Category:minitest (1 member)
# Category:optparse (1 member)
# Category:prime (1 members)
# Category:test/unit (2 members)

A ''phantom category'' is a category with some members, but no category page. Templates like <nowiki>{{libheader|prime}}</nowiki> did put pages in these categories. I started to destroy these categories after someone confused Category:prime with [[:Category:Prime Numbers]] ([[Rosetta Code:Village Pump/Grouping tasks#Library/Libheader appears to be being used incorrectly|ref 1]] and [[Talk:Count in factors#phantom categories - incorrect use of Library templates|ref 2]]). I think that many contributors will use the standard library without adding categories. I see code that calls <code>require 'find'</code> or <code>require 'securerandom'</code> without a category. --[[User:Kernigh|Kernigh]] 03:49, 27 August 2011 (UTC)
