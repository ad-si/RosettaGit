+++
title = "Talk:Matrix-exponentiation operator"
description = ""
date = 2013-02-28T21:24:53Z
aliases = []
[extra]
id = 2873
[taxonomies]
categories = []
tags = []
+++

The implementations seem to be wrong. Matrix exponentiation is defined for any complex power. For example, like for numbers sqrt A = A**0.5. The implementation can be based on decomposition: A = V<sup>T</sup> diag{&lambda;<sub>1</sub>,..,&lambda;<sub>n</sub>} V. Then A<sup>X</sup> = V<sup>T</sup> diag{&lambda;<sub>1</sub><sup>X</sup>,..,&lambda;<sub>n</sub><sup>X</sup>} V

: Well, the task isn't specific in this respect. Exponentiation with an integer exponent just means repeatedly doing matrix multiplication, and that generalization is also useful for integer matrices, say. It's fortuitous that one can generalize this for complex matrices, but that won't be possible for any base type.
: Also, implementing this is mainly an exercise in using the available linear-algebra library packages. That doesn't really help in highlighting the differences between computer languages. Nor is it the goal of Rosetta to develop a complete set of libraries for every language.
: So I'd suggest that a task "define exponentiation for matrices with integer exponents and any basetype, including complex" is actually more interesting than "do it for complex exponents and complex matrices, by using LA-libraries". --[[User:Dirkt|Dirkt]] 04:59, 4 June 2008 (MDT)

::You mean natural power? Because negative values would require matrix inversion. So the focus is actually to implement it using matrix multiplication? --[[User:Dmitry-kazakov|Dmitry-kazakov]] 05:56, 4 June 2008 (MDT)

:::Well, negative integer exponents at least will work for any ring, they don't require any algebraically closed field like the complex numbers. But of course then the operation is partial. And it's not my task, I don't know what the focus actually "is" -- the description leaves this open. However, I think the most important criterion to decide this is "what variant does tell us the most about the differences between languages?" --[[User:Dirkt|Dirkt]] 06:22, 5 June 2008 (MDT)

== Descriptions and solutions don't match ==

The task description first states that many languages have a matrix exponentiation operator. 
Then it asks for an implementation of such an operation as an operator.  

Most solutions don't show how to implement the operation as an operator
They simply show how to use a builtin operation.

Should the task description be rewritten to match the solutions?

: The task description first states that many languages have an exponentiation operator for integers and reals only - the implication here is that they have exponentiation but not for matrices.
: Also, the implementations I looked at did not simply call a library routine for matrix exponentiation, so I do not know what to think about that part of your question.
: (However "Reals" probably means something like "floating point" where the phrase "floating point" is referring to a representation technique such as ieee 754 and not to any language keywords.)
: --[[User:Rdm|Rdm]] 21:24, 28 February 2013 (UTC)
