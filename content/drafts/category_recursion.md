+++
title = "Category:Recursion"
description = ""
date = 2017-09-03T11:29:31Z
aliases = []
[extra]
id = 2490
[taxonomies]
categories = []
tags = []
+++

[[Category:Solutions by Programming Task]][[Category:Encyclopedia]]'''Recursion''' is the idea that a function can come to an answer by repeatedly calling itself with new arguments until a "base case" or "end condition" is met. One simple example is a [[factorial function]] (for positive integers only). The base case for factorial is "0!" (some people like to use 1 or 2, but 0 is OK for instructional purposes). When 5 is sent as an argument to a recursive factorial function, the function does not know the answer right away. But it does know that 5! = 5 * 4!. So it calls itself to find out what 4! is. This process continues until it gets to 0!, which is 1. Now it has built up a train of answers: 5! = 5 * 4! = 5 * 4 * 3! etc., and it can find the final answer. Other common examples include [[tree traversal]] and the [[wp:Minimax|min/max algorithm]]. Anyone who can convert the likes of 12,345 into "twelve thousand, three hundred and forty-five" is engaging in the recursive use of language.

A pseudocode function to demonstrate recursion would look something like this:
 '''function''' F '''with''' arguments
   '''if''' ''end condition is not met''
     '''return''' ''F called with new set of arguments''
   ''//optional additional conditions which may or may not have direct answers''
   '''else'''
     '''return''' ''end condition value''
More than one end condition is allowed. More than one recursion condition is allowed. More generally, the function may be able to deliver the result for some arguments but not for others, and for those cases it is able to compose the result from some combination of function calls with different arguments. Simple recursion is when it invokes itself, but a group of functions may be involved in mutual recursion. The analyst's task is to devise a plan that will always end with a result for the desired initial invocations. In the case of the factorial function, this is easily seen since factorial(n) invokes factorial(n - 1) and so on: for every positive integer this sequence must eventually reach zero and the result for factorial(0) is directly available. A more complex example is provided by the [[wp:Ackermann_function]] 

Recursion is often difficult for programming students to grasp, but it's not much different from any other function call. In a normal function call, execution moves to another function with the given parameters. In a recursive function call, execution moves to the same function, but the parameters still act as they would in a normal function call.

Many recursion problems can be solved with an iterative method, or using a [[loop]] of some sort (usually recursion and iteration are contrasted in programming, even though recursion is a specific type of iteration). In some languages, the factorial example is best done with a loop because of function call overhead. Some other languages, like [[Scheme]], are designed to favor recursion over explicit looping, using tail recursion optimization to convert recursive calls into loop structures.

'''Tail recursion''' is a specific type of recursion where the recursive call is the last call in the function. Because tail recursive functions can easily and automatically be transformed into a normal [[:Category:Iteration|iterative]] functions, tail recursion is used in languages like Scheme or [[OCaml]] to optimize function calls, while still keeping the function definitions small and easy to read. The actual optimization transforms recursive calls into simple branches (or jumps) with logic to change the arguments for the next run through the function (which together may be thought of as a loop with local variables).

The benefits of this optimization are primarily [[System stack|stack]] related. Transforming recursive functions into iterative functions can save memory and time.

Memory is saved by reducing an entire call stack (with contexts for each call) to one function call. This way, more complex calls can be evaluated without running out of memory.

Time is saved because each function return takes a long time relative to a simple branch. This benefit is usually not noticed unless function calls are made that would result in large and/or complex call trees. For example, the time difference between iterative and recursive calls of <tt>fibonacci(2)</tt> would probably be minimal (if there is a difference at all), but the time difference for the call <tt>fibonacci(40)</tt> would probably be drastic.

Sometimes, tail-recursive functions are coded in a way that makes them not tail-recursive. The example above could become tail-recursive if it were transformed to look like this:
  '''function''' F '''with''' arguments
    '''if''' ''end condition is met''
      '''return''' ''end condition value''
    ''//optional additional conditions which may or may not have direct answers''
    '''else'''
      '''return''' ''F called with new set of arguments''

Below is a list of examples of recursion in computing.
