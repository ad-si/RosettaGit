+++
title = "Conditional structures/Ruby"
description = ""
date = 2010-02-06T12:52:33Z
aliases = []
[extra]
id = 5302
[taxonomies]
categories = []
tags = []
+++

{{collection|Conditional Structures}}

===if-then-else===

```ruby
if s == 'Hello World'
  foo
elsif s == 'Bye World'
  bar
else
  deus_ex
end
```


Note that <code>if...end</code> is an expression, so its return value can be captured in a variable:


```ruby

s = 'yawn'
result = if s == 'Hello World'
           :foo
         elsif s == 'Bye World'
           :bar
         else
           :deus_ex
         end
# result now holds the symbol :deus_ex
```



### ternary


```ruby
 s == 'Hello World' ? foo : bar
```


===case-when-else===
A generic case statement

```ruby
case
when Time.now.wday == 5 
  puts "TGIF"
when rand(3) == 2
  puts "had a 33% chance of being right"
else
  puts "nothing special here"
end
```

or, comparing to a specific object

```ruby
case cartoon_character
when 'Tom'
  chase
when 'Jerry'
  flee
end
```


For the second case, the comparisions are preformed using the <code>===</code> "case equality" method like this: <code>'Tom' === cartoon_character</code>.  The default behaviour of <code>===</code> is simple <code>Object#==</code> but some classes define it differently.  For example the Module class (parent class of Class) defines <code>===</code> to return true if the class of the target is the specified class or a descendant:

```ruby
case some_object
when Numeric
  puts "I'm a number.  My absolute value is #{some_object.abs}"
when Array
  puts "I'm an array.  My length is #{some_object.length}"
when String
  puts "I'm a string.  When I'm down I look like this: #{some_object.downcase}"
else
  puts "I'm a #{some_object.class}"
end
```


The class Regexp aliases <code>===</code> to <code>=~</code> so you can write a case block to match against some regexes

```ruby
case astring
when /\A\Z/             then  puts "Empty"
when /\A[[:lower:]]+\Z/ then  puts "Lower case"
when /\A[[:upper:]]+\Z/ then  puts "Upper case"
else                    then  puts "Mixed case or not purely alphabetic"
end
```


The class Range aliases <code>===</code> to <code>include?</code>:

```ruby
case 79
when 1..50   then   puts "low"
when 51..75  then   puts "medium"
when 76..100 then   puts "high"
end
```

