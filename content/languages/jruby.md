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
```ruby
p RUBY_ENGINE
# => "jruby"
```
JRuby is an implementation of Ruby that runs inside the [runs on vm::Java Virtual Machine](https://rosettacode.org/wiki/runs_on_vm::Java_Virtual_Machine) and can call Java libraries from Ruby code. JRuby 1.6.x implements both Ruby 1.8.7 and Ruby 1.9.2 in one install.

JRuby has preemptive concurrency like Java; each Ruby Thread is a Java Thread. JRuby can run multiple threads in parallel on multiple CPUs, if the JVM can do so. [MRI](https://rosettacode.org/wiki/MRI) has a Global VM Lock and can run these threads on only one CPU. In contrast, JRuby can never fork processes; MRI can fork with some platforms.

JRuby implements the interpreter and most of the core library in [implemented in language::Java](https://rosettacode.org/wiki/implemented_in_language::Java). The standard library is a mix of Java and Ruby.

* http://jruby.org
