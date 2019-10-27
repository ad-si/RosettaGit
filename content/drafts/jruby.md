+++
title = "JRuby"
description = ""
date = 2011-09-16T22:19:31Z
aliases = []
[extra]
id = 10529
[taxonomies]
categories = []
tags = []
+++

{{implementation|Ruby}}
{{infobox begin}}
```ruby
p RUBY_ENGINE
# => "jruby"
```
{{infobox end}}

JRuby is an implementation of Ruby that runs inside the [[runs on vm::Java Virtual Machine]] and can call Java libraries from Ruby code. JRuby 1.6.x implements both Ruby 1.8.7 and Ruby 1.9.2 in one install.

JRuby has preemptive concurrency like Java; each Ruby Thread is a Java Thread. JRuby can run multiple threads in parallel on multiple CPUs, if the JVM can do so. [[MRI]] has a Global VM Lock and can run these threads on only one CPU. In contrast, JRuby can never fork processes; MRI can fork with some platforms.

JRuby implements the interpreter and most of the core library in [[implemented in language::Java]]. The standard library is a mix of Java and Ruby.

* http://jruby.org
