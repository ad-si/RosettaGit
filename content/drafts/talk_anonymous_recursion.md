+++
title = "Talk:Anonymous recursion"
description = ""
date = 2019-03-03T15:21:03Z
aliases = []
[extra]
id = 9129
[taxonomies]
categories = []
tags = []
+++

== What constitutes correctness in this task ==

It seems that I could not make clear what this task is about. Anonymous recursion is the '''call''' equivalent to the '''jump''' of an anonymous branch.

Most programming languages allow you to branch to a previous location without that you must invent a name (label) for that.

An example for a '''non-anonymous''' branch would be:

   doThis();
   myLabel1:
      if (!condition())
         goto myLabel2;
      doThat();
      goto myLabel1;
   myLabel2:
   doMore();

This is normally written as an '''anonymous''' branch:

   doThis();
   while (condition())
      doThat();
   doMore();

The advantages are obvious: You don't have to invent label names for separate goto statements, and the program flow is not obfuscated by the explicit housekeeping for the loop.

In that sense, 'while' implements an '''anonymous jump''': It does not disturb the program flow in any way, which goes from doThis() to doThat() without any special setup, and the branch back to the anonymous location (the start of the 'while' statement) is done transparently.

In the same sense, it is desirable to have an '''anonymous call'''. Ideally via some keyword, in the same way as 'while', 'for' etc. are keywords for anonymous jumps in most languages.

In PicoLisp, the corresponding keywords are 'recur' and 'recurse':

   (doThis)
   (recur ()
      (doThat)
      (if (condition)
         (recurse) ) )
   (doMore)

Note that 'recur' does not disrupt the program flow. It behaves like the 'while' in anonymous loops in that regard, the program goes from 'doThis' directly to 'doThat' without any intervening function call. Only if 'recurse' is executed somewhere in the body of 'recur', an actual call takes place.

In classic BASIC, such behavior could be directly programmed via GOSUB:

   100 doThis
   110 doThat
   120 IF condition THEN 140
   130 GOSUB 110
   140 doMore


Thus, defining an '''anonymous function''' is not a correct solution to this task.  Even if that function is local to the main function, it is just hidden, being equivalent to a 'static' function in C.

Take the Ada solution:

   function Fib ...
      function Actual_Fib ...
         ...
      end Actual_Fib;
   begin
      if X < 0 then
         raise Constraint_Error;
      else
         return Actual_Fib (X);
      end if;
   end Fib;

There are still two named functions being defined. This is equivalent to a C program

   static int Actual_Fib(..) {
      ...
   }

   int Fib(..) {
      if (X < 0)
         error();
      return Actual_Fib(X);
   }

Note that the 'if' statement does '''not''' directly proceed to the fibonacci processing. There is a top-level call to 'Actual_Fib' in between.

With a 'recur' statement, this is not the case:

   (de fibo (N)
      (if (lt0 N)
         (quit "Illegal argument" N) )
      (recur (N)
         (if (> 2 N)
            1
            (+ (recurse (dec N)) (recurse (- N 2))) ) ) )

The first invocation of '(if (> 2 N)' happens in the top level context of 'fibo', without an intervening function call and thus saving one recursion level.

The JavaScript solution

   function fibo(n) {
     if (n < 0)
       throw "Argument cannot be negative";
     else
       return (function(x) {
         if (n < 2)
           return 1;
         else
           return arguments.callee(n-1) + arguments.callee(n-2);
       })(n);
   }

is correct IMHO, despite that it defines a new function.  But it does this inline, in a clean way within the program flow.

If can't say whether the J solution is correct or not. I don't understand it well enough. Perhaps somebody else can judge?

The same goes for Mathematica. The text states that the check is performed once (though it still calls '#0'), so I'll believe it.

Concerning the Python, Ruby and Tcl versions, I'm not sure. They define a local function 'f' (or object 'o'), which is then called by name. It seems weaker to me than the elegant 'arguments.callee' mechanism of JavaScript.

The Ursala solution looks cool.--[[User:Abu|Abu]] 11:04, 8 January 2011 (UTC)

:The J solution is incorrect because it does not check for a negative sign, it simply implements the fibonacci function. Using $: is problematic because it is a reference to the largest verb which means that it will re-execute the entire function including the check if such a check is included. I have fixed the Mathematica example so that the check is performed only once, #0 is local to the anonymous function it is referenced in, this is the fibonacci function and not the function with the check. --[[User:Zeotrope|Zeotrope]] 16:18, 8 January 2011 (UTC)

::Since the J solution acts like an identity when called with negative arguments the check for negative numbers can be performed after the fibonacci function is called, signalling a domain error if the result is negative: 
```J
fibN =: 3 : '[:^:(0&>) (-&2 +&$: -&1)^:(1&<) M. y'"0
```
 Alternatively using a control structure: 
```J
(3 : 'if. y<0 do. <''negative argument'' else. <(-&2 +&$: -&1)^:(1&<) M. y end.'"0)
```
 --[[User:Zeotrope|Zeotrope]] 19:48, 8 January 2011 (UTC)

:::The J solution does indeed check if the number is negative before using recursion, and will not use recursion with a negative argument.  (Any number which is negative is not greater than 1, and the J solution checks to see if the number is greater than 1 before using recursion.)  That said, the task does not specify the result to be returned for negative values.  --[[User:Rdm|Rdm]] 03:25, 9 January 2011 (UTC)

:The Tcl version only gives the function a ''local'' name (i.e., stores the function in a local variable) in order to keep the lines relatively short. <code>f</code> is not a name in the space of commands (which Tcl partitions completely from the space of variables) so I'd argue that the code as written completely follows the rules of the task. It would certainly be understood as being anonymous from the perspective of a Tcl practitioner. –[[User:Dkf|Donal Fellows]] 09:06, 9 January 2011 (UTC)

== Recursion implies a function ==

I am the author of the examples for Ruby and for UNIX Shell which create and hide a recursive function.

I believe that the use of recursion implies the existence of a function. To have recursion, one must call a thing, and this thing must return; therefore this thing is a function. The task is to hide the extra function so that it does not pollute the namespace nor become available to other functions. JavaScript 'arguments.callee' and PicoLisp 'recurse' only hide a function. I show this by porting them to Ruby. --[[User:Kernigh|Kernigh]] 22:20, 8 January 2011 (UTC)


```ruby
# a function almost like JavaScript
class Function
  def initialize(*params, &block)
    @params = Struct.new(:callee, *params)
    @block = block  # ___ hiding a function! ___
  end

  def call(*args)
    @block[@params.new(self, *args)]
  end

  alias [] call
end

def fibo(n)
  if n < 0
    raise "Argument cannot be negative"
  else
    return (Function.new(:n) { |arguments|
              if arguments.n < 2
                next 1
              else
                next (arguments.callee[arguments.n-1] +
                      arguments.callee[arguments.n-2])
              end
            })[n]
  end
end
```



```ruby
# recur/recurse almost like PicoLisp
def recur(*args, &block)
  old_recurse = $recurse
  begin
    $recurse = block  # ___ hiding a function! ___
    $recurse[*args]
  ensure
    $recurse = old_recurse
  end
end

def fib(n)
  if n < 0
    raise "Illegal argument #{n}"
  end
  recur(n) { |n|
    if 2 > n
      1
    else
      $recurse[n.pred] + $recurse[n - 2]
    end
  }
end
```


:Agreed. I think this strikes home. You could stuff that 'recur' function into some library, and use it whenever needed, right? --[[User:Abu|Abu]] 07:46, 9 January 2011 (UTC)
:: Is there something I'm doing wrong? The above version of 'fib' just returns its argument. --[[User:Abu|Abu]] 09:15, 12 January 2011 (UTC)
::: You probably used Ruby 1.8, while I used Ruby 1.9. They changed the rules for when a block parameter has the same name as a variable outside the block. I misunderstood the rules for Ruby 1.8, and wrote a recursive singleton method, when a recursive block would have worked. --[[User:Kernigh|Kernigh]] 04:04, 11 February 2011 (UTC)
:::: Make sure you use the {{tmpl|works with}} template to document which version of Ruby is required. Let's ''capture'' that information properly instead of leaving it languishing in a comment on a talk page! –[[User:Dkf|Donal Fellows]] 10:45, 11 February 2011 (UTC)

:Concerning "recursion implies a function", it depends on what you consider a function. The GOSOB in the Basic code snippet is not really a function call. That's why I used the term "call" instead of "function". A Forth solution would involve two or three new immediate control words, similar to BEGIN/WHILE/REPEAT. In the PicoLisp version (and also in your second Ruby version, if I understand it right), the first pass through the 'recur' body does not actually involve a function call, but it is executed in the context of the surrounding function. --[[User:Abu|Abu]] 07:57, 9 January 2011 (UTC)

----

* "GOSUB" looks like a function call to me, because it reminds to me of those assembly instructions (6502 'jsr', 65816 'jsl' or PowerPC 'bl') that provide the usual way to call functions in assembly language.
* I could stuff Ruby 'recur' into a library. The above version was not good, because a Ruby program can have multiple call stacks (from fibers or threads). I post a different version of Ruby 'recur' on the page, where 'recurse' has no $ sign.
* Ruby is different from PicoLisp here: the first pass through the 'recur' block really is a function call! For example, <tt>recur { recurse }</tt> would be an infinite loop (until I run out of memory, with a very long stack trace), but <tt>recur { next 5; recurse }</tt> would return 5, because 'next' is the Ruby keyword to return from a block.
* I cannot find the difference between an ''invisible function'' and a ''function-that-is-not-considered-a-function''. If the wrong solutions use the invisible functions, and the correct solutions use the functions that are not considered functions, then I cannot know whether each solution is correct or wrong! --[[User:Kernigh|Kernigh]] 04:04, 11 February 2011 (UTC)
== Clarity needed - Confusion between task and examples  ==
: While some of this stuff, including article on WP which are marked needing expert help, aren't the easiest to understand, it seems that a number of the examples are probably/maybe incorrect (I don't claim to understand all the solutions).
:# [[wp:Fixed_point_combinator]]
:# [[wp:Anonymous_function]]
:# [http://stackoverflow.com/questions/156369/which-languages-support-recursive-function-literals-anonymous-functions] discussion on stack-overflow --[[User:Dgamey|Dgamey]] 22:58, 20 July 2011 (UTC)
:Examples that look like simple recursion no hidden function, no function not bound to an identifier (per #2).:
:* AutoHotkey, Forth
:Forth is already marked incorrect.  I think AutoHotKey should be too.
:Examples with nested functions (do these really qualify, maybe but not sure):
:* Ada, C, C++, Common Lisp, F# (one example), Factor, Nemerle, Unix shell
:Nested functions and binding to a local variable seem equivalent on one level, so maybe nested functions are fine.
:--[[User:Dgamey|Dgamey]] 22:01, 20 July 2011 (UTC)

:: It seems that the task just wants the recursive function not polluting global namespace.  The AutoHotkey and Forth examples are definitely incorrect.  For the nested functions, it depends on if a nested function is visible outside of the scope or not.  In most languages it shouldn't be, but not always: if you define a Perl5 function nested inside another function, it's still in the package namespace, not the lexical one.  For your list:
::# Ada I don't know;
::# C nested function is not visible outside as demonstrated (I wrote it);
::# C++ nested class definition (actual_fib) is not visible outside;
::# Common Lisp nested defun (fib1) is not visible;
::# F# fib2 is not visible;
::# Factor I have no idea what it's doing, but I guess it effectively creates a one-time eval-ish function to do the recursion; if so it would be ok;
::# Nemerle does some handwaving, I don't know the actual effect;
::# Unix shell creates a subshell which aims to not pollute the namespace in parent ''process'', but it does pollute the namespace in the same ''source file''.  I would say it's not ok.  Then again, you don't tend to write very large programs in shell script, so it's probably moot.
::--[[User:Ledrug|Ledrug]] 22:44, 20 July 2011 (UTC)
:: Your observation of "not polluting the global name space" seems to me to be spot on on how this is being interpreted and implemented.  But I'm not certain that I know enough about the task author's intent to say.  That raises the question, should the task be changed?  Clarified? Renamed? Both?  Should implementations be marked incorrect?  Given the number of solutions in place, the last choice is not one I'd take.  If the intent was missed change the task and if desired create a draft with a better description of the intent with clearer guidelines. --[[User:Dgamey|Dgamey]] 23:02, 20 July 2011 (UTC)
::: This was created by Abu in November - can you way in.
:::: Well, the author listed 3 disadvantages, the first two are about the namespace issue; the third one is, IMO, bogus, because immediately after saying "interrupting program flow", the Y combinator is suggested, which interrupts code flow like there's no tomorrow.  I think most examples are fine, although some are doing the task for the task's sake (such as the Go Y combinator solution) with no practical benefit.  As it stands, maybe only a few examples need to be marked incorrect, while others do show some useful techniques.  It never hurts to clarify the task, though. --[[User:Ledrug|Ledrug]] 23:27, 20 July 2011 (UTC)

== Task description is a bit childish ==

While I think the task is interesting, as a few languages permit this directly or at least clean workarounds, the arguments given are not very serious:

* ''You have to think up a name, which then pollutes the namespace'': if thinking up a name is a problem, consider another activity.
* ''Function is created which is called from nowhere else'' : It's called once. For some functions, it's enough to justify them. If it's really a problem, many languages have a way to make a function invisible from outside a given scope (for instance nested functions, static functions or namespaces), and even if there is no such thing, it's easy to add som prefix that make it very clear the function has "internal purpose".
* ''The program flow in the source code is interrupted'': that's the point of any flow control structure, and any function call. Again, if it's a problem, consider another activity.

I understand that making a program readable is important (it's one of my top priorities when writing programs, because I know ''I'' won't be able to maintain the program otherwise). However, it's silly to invent a problem where there is not really one. Thinking up a name and taking care of the program flow are basic tasks. Recursion is not adding complexity here. On the contrary, it usually simplifies things.

[[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 15:19, 3 March 2019 (UTC)
