+++
title = "Topic variable"
description = ""
date = 2019-04-18T19:23:58Z
aliases = []
[extra]
id = 12844
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "applescript",
  "axe",
  "clojure",
  "erlang",
  "forth",
  "freebasic",
  "go",
  "haskell",
  "j",
  "jq",
  "julia",
  "kotlin",
  "mathematica",
  "oforth",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powershell",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "sidef",
  "standard_ml",
  "unix_shell",
  "vba",
  "zkl",
]
+++

Several programming languages offer syntax shortcuts to deal with the notion of "current" or "topic" variable.  A topic variable is a [[Special variables|special variable]] with a very short name which can also often be omitted.

Demonstrate the utilization and behaviour of the topic variable within the language and explain or demonstrate how the topic variable behaves under different levels of nesting or scope, if this applies, within the language.

For instance you can (but you don't have to) show how the topic variable can be used by assigning the number <math>3</math> to it and then computing its square and square root.


## AppleScript


AppleScript binds the name '''result''' to the value of the expression most recently evaluated in the current scope.


```applescript
on run
    1 + 2

    ap({square, squareRoot}, {result})

    --> {9, 1.732050807569}
end run


-- square :: Num a => a -> a
on square(x)
    x * x
end square

-- squareRoot :: Num a, Float b => a -> b
on squareRoot(x)
    x ^ 0.5
end squareRoot


-- GENERIC FUNCTIONS ----------------------------------------------------------

-- A list of functions applied to a list of arguments
-- (<*> | ap) :: [(a -> b)] -> [a] -> [b]
on ap(fs, xs)
    set lst to {}
    repeat with f in fs
        tell mReturn(contents of f)
            repeat with x in xs
                set end of lst to |λ|(contents of x)
            end repeat
        end tell
    end repeat
    return lst
end ap

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn
```

```txt
{9, 1.732050807569}
```


The name '''result''' is still bound in this way if the most recently evaluated expression is a script rather than a simple value:

```applescript
on run
    script
        -- The given function applied to the value 3
        on |λ|(f)
            mReturn(f)'s |λ|(3)
        end |λ|
    end script

    -- Here, 'result' is bound to the script above
    map(result, {square, squareRoot})

    --> {9, 1.732050807569}
end run


-- square :: Num a => a -> a
on square(x)
    x * x
end square

-- squareRoot :: Num a, Float b => a -> b
on squareRoot(x)
    x ^ 0.5
end squareRoot


-- GENERIC FUNCTIONS ----------------------------------------------------------

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn
```

```txt
{9, 1.732050807569}
```


## Axe

In Axe, evaluated expressions can be "remembered" until the next expression is evaluated.

```axe
3
Disp *3▶Dec,i
```


Prints:

```txt

    9

```


However, attempting to use the result now would result in garbage due to the ▶Dec and Disp commands overwriting the previous result.


## Clojure

The Clojure REPL has '''*1''' (and also '''*2''' and '''*3''' for the 2nd or 3rd most recent)

```clojure

user=> 3
3
user=> (Math/pow *1 2)
9.0
user=> (Math/pow *2 0.5)
1.7320508075688772

```



## Erlang

There are no global variables, so Erlang uses a function, v(N), to get old results. Either the absolute command result, if N is positive, or, if N is negative, the return value of the Nth previous command.
```txt

7> 1 + 2.
3
8> v(-1).
3

```



## Forth

In a stack oriented language like Forth the definition of variables is minimized as much as possible. The closest thing to a topic variable is the use of '''R@'''. This gets the top item from the return stack, which by the way is also used for flow control. It is up to the programmer to keep the stack balanced. In some Forth dialects '''R@''' and '''I''' are identical. '''I''' is used as a loop index, e.g.


```forth
: myloop 11 1 do i . loop cr ; myloop>
```


Which will print all numbers from 1 to 10. A typical use of '''R@''' is illustrated here:


```forth
: ^2 dup * ;
: sqrt 0 tuck ?do 1+ dup 2* 1+ +loop ;
: topic >r r@ ^2 . r@ sqrt . r> drop ;

23 topic
```


The word '''>R''' places the item on the return stack and the word '''R>''' retrieves it from the return stack - an experienced Forth programmer would optimize this definition even further. Note that for technical reasons all words listed cannot be used outside definitions, so it may be argued that Forth doesn't have topic variables.


## FreeBASIC

The nearest thing FreeBASIC has to a topic variable is the name of the current function.

For example, if you declare a function called 'func' with a return type of Integer, the function behaves as though it contains a local variable called 'func' - defined at the top of its body - of type Integer. You can assign to this variable and, if you do, the return value of the function is the value this variable has when the function returns.

Alternatively, the keyword 'Function' can itself be used as an implicitly defined variable and behaves in exactly the same way as the function's name when used in this role. Similarly, the keywords 'Property' or 'Operator' can be used to return values from properties or operators respectively.


```freebasic
' FB 1.05.0 Win64

' Three different ways of returning a value from a function

Function Sum (x As Integer, y As Integer) As Integer
  Sum = x + y  '' using name of function
End Function

Function Sum2 (x As Integer, y As Integer) As Integer
  Function = x + y  '' using Function keyword
End Function

Function Sum3 (x As Integer, y As Integer) As Integer
  Return x + y  '' using Return keyword which always returns immediately
End Function

Print Sum (1, 2)
Print Sum2(2, 3)
Print Sum3(3, 4)
Sleep
```


```txt

 3
 5
 7

```



## Go

Go has nothing like this in the bare language, but the template package of the standard library has a similar mechanism.  Templates can have named variables, but they also have a cursor, represented by a period '.' and called "dot", that refers to a current value.

```go
package main

import (
    "math"
    "os"
    "strconv"
    "text/template"
)

func sqr(x string) string {
    f, err := strconv.ParseFloat(x, 64)
    if err != nil {
        return "NA"
    }
    return strconv.FormatFloat(f*f, 'f', -1, 64)
}

func sqrt(x string) string {
    f, err := strconv.ParseFloat(x, 64)
    if err != nil {
        return "NA"
    }
    return strconv.FormatFloat(math.Sqrt(f), 'f', -1, 64)
}

func main() {
    f := template.FuncMap{"sqr": sqr, "sqrt": sqrt}
    t := template.Must(template.New("").Funcs(f).Parse(`. = {{.}}
square: {{sqr .}}
square root: {{sqrt .}}
`))
    t.Execute(os.Stdout, "3")
}
```

```txt

. = 3
square: 9
square root: 1.7320508075688772

```



## Haskell

In Haskell terminal GHCi or WinGHCi, topic variable is called: it.

```Haskell

Prelude> [1..10]
[1,2,3,4,5,6,7,8,9,10]
Prelude> map (^2) it
[1,4,9,16,25,36,49,64,81,100]

```


## J

With this new definition of topic variables, the closest thing J has to a topic variable is probably dummy variables used in function definitions, because we always omit declaring which variables they are, and because we may omit them entirely.  But, we still need to place the function in the context of the value it's being used on.

Thus, for example (entirely eliminating the variable representing the argument):


```J
   example=: *:, %:  NB. *: is square, %: is square root
   example 3
9 1.73205
```


Or, if we want to see the dummy variable in place (though still not declared, because there is no point to that):


```J
   Example=: verb def '(*: y), (%: y)'
   Example 3
9 1.73205
```


Or course, if it's crucial to the concept of topic variables that they not be constrained to definitions of things like functions, then it might be argued that J does not have them.

On the third hand, note that "definitions of functions" do not actually need to be associated with names. At worst, some definitions might need to be enclosed in parenthesis:


```J
   (*:, %:) 3
9 1.73205
```


And if we were to insist on leaving out functions, it's not clear that there would be much of anything left of the language to be doing anything with. See also Henry Rich's writeup on [http://www.jsoftware.com/docs/help701/jforc/tacit_programs.htm Tacit Programs].


## jq

In jq, . (a period) generally refers to the "current input", which is always a JSON entity. For example, the jq program consisting of . alone is a filter, like "cat" in the Unix tradition.

The "." referring to the current input can often be omitted altogether, for example the expression ". | exp" for computing e<sup>x</sup> can always be written simply as "exp".

There are some special character combinations involving ".":
* .[] is a filter which expects the input to be an array or a JSON object, and converts the input to be a stream of the constituent values;
* if i as an integer then .[i] is a filter which expects the input to be an array, and which extracts its i-th element (counting from 0).

Multiple references to the current input are allowed, e.g. [.,.] constructs an array of length two, each component being equal to the current input; thus the compound expression "1 | [.,.]" yields [1,1].


## Julia

Julia REPL has `ans` variable:

```julia
julia>
 3
3
julia> ans * ans, ans - 1
(9, 2)
```



## Kotlin

The closest thing Kotlin has to a topic variable is the identifier 'it' which implicitly refers to the parameter of a lambda expression when it only has one. As in the case of all other parameters in Kotlin, 'it' is read-only and is in scope until the end of the lambda expression.

```scala
// version 1.1.2

fun main(args: Array<String>) {
    3.let {
        println(it)
        println(it * it)
        println(Math.sqrt(it.toDouble()))
    }
}
```


```txt

3
9
1.7320508075688772

```



## Mathematica

This depends on the Mathematica REPL. For the examples, I will use the textual format that Mathematica provides. Here's some basic examples:

```txt
In[1]:= 3

Out[1]= 3

In[2]:= %1^2

Out[2]= 9

In[3]:= Sqrt[%%]

Out[3]= Sqrt[3]

In[4]:= N[Out[-1]] (* for floating point *)

Out[4]= 1.73205
```

In this, I use 3 different forms. Here's a list of them:
* %<n>: Does the same as Out[n].
* %, %%, %%%, etc.: Does the same as Out[-<number of %s>].
* Out[n]: Returns the output of the nth cell.
* Out[-n]: Returns the output of the nth to last cell, excluding the one that it is in. (Out[-1] gives last output, -2 gives second to last, etc.)
* In[n]: Reevaluates and returns the input of the nth cell.
* In[-n]: Reevaluates and returns the input of the nth to last cell.
When an input is reevaluated, it also reassigns all relative Ins and Outs. Look at this for an example of its strange effects:

```txt
In[1]:= In[2]

Out[1]= In[2]

In[2]:= In[1]

$IterationLimit::itlim: Iteration limit of 4096 exceeded.

Out[2]= Hold[In[1]]
```

In it, it gets stuck in an infinite loop between In[1] and In[2], which evaluate to each other.


## Oforth


Oforth does not have global variables, topic or not.

The closest thing Oforth has to a topic variable definition is the top of its data stack which always holds the last result without the need to assign it to a (local) variable but I don't think it is the philosophy of this task.

This will push 3 on the stack and compute sq and sqrt :

```oforth
3 dup sq swap sqrt>
```



## PARI/GP


gp is a REPL for GP, within which <code>%</code> can be used to refer to the last result.

```parigp
3
[sqrt(%),%^2]
```

```txt
%1 = 3
%2 = [1.7320508075688772935274463415058723670, 9]
```



## Perl

In Perl the topic variable is $_.
It is the default parameter for loops, and some functions e.g. 'sqrt':


```Perl
print sqrt . " " for (4, 16, 64)
```

```txt
2 4 8
```


The topic variable is lexical, so its use can be nested into lexical scopes.
However, assignment to the topic variable at loop declaration is not lexical,
and the 'local' keyword is needed to enable loops to nest.


```perl
for (1..2) {
  print "outer $_:\n";
  local $_;
  for (1..3) {
    print " inner $_,";
  }
  print " fini\n";
}
```

```txt
outer 1:
 inner 1, inner 2, inner 3, fini
outer 2:
 inner 1, inner 2, inner 3, fini
```



## Perl 6


As in previous versions of Perl, in Perl6 the topic variable is $_.  In addition to a direct affectation, it can also be set with the 'given' keyword.  A method can be called from it with an implicit call:


```perl6
given 3 {
    .say for $_**2, .sqrt;
}
```

The scope of the <tt>$_</tt> variable is always lexical in Perl 6, though a function can access its caller's topic if it asks for it specially via <tt>CALLER::<$_></tt>.


## Phix

The closest thing would be to declare a variable with just an underscore as its identifier.

```Phix
object _
_ = 3
?_
?_*_
```

```txt

3
9

```



## PicoLisp


```PicoLisp
PicoLisp sets the value of the variable (symbol) '@' to the result of
conditional and controlling expressions in flow- and logic-functions (cond, if,
and, when, while, etc.).

Within a function or method '@' behaves like a local variable, i.e. its value is
automatically saved upon function entry and restored at exit.

For example, to read the current input channel until EOF, and print the square
of every item which is a number:
```

Test:

```PicoLisp
(while (read)
   (when (num? @)
      (println (* @ @)) ) )
abc   # Not a number
7     # Number
49    # -> print square
xyz   # Not a number
3     # Number
9     # -> print square
```



## PowerShell

In PowerShell the "topic" variable is <code>$_</code>.

<code>$_</code> is a placeholder for each object in the pipeline.  Any cmdlet or advanced function that accepts input from the pipeline may use this variable.


The most common use is in the <code>ForEach-Object</code> cmdlet:

```PowerShell

65..67 | ForEach-Object {$_ * 2}     # Multiply the numbers by 2
65..67 | ForEach-Object {[char]$_ }  # ASCII values of the numbers

```

```txt

130
132
134
A
B
C

```

Using <code>Where-Object</code> to filter the odd numbers from an array:

```PowerShell

65..67 | Where-Object {$_ % 2}

```

```txt

65
67

```

Using <code>Format-Wide</code> to force an array into columns:

```PowerShell

65..70 | Format-Wide {$_} -Column 3 -Force

```

```txt

65                                           66                                           67
68                                           69                                           70

```



## Python

Pythons REPL has '''_'''.

```python
>>>
 3
3
>>> _*_, _**0.5
(9, 1.7320508075688772)
>>>
```



## Racket


Racket doesn't have a "built-in" concept of a topic variable, but one is easy to add to the language with some macros.  In fact, the subject of adding such a facility to a language using a hygienic macro facility is a very popular topic in some macro circles, and Racket can do it very well using syntax parameters.  In the following there is a demonstration of two implementation approaches, the first uses a "parameter" -- a runtime value that is bound to a value in some dynamic extent, and the second uses a "syntax parameter" which is something that refers indirectly to a plain binding, and this binding can be adjusted by macros to point at an existing "real" binding.  See the end of the code for usage samples.  (Note that there is no point to talk about how these things behave wrt scope: since Racket is flexible enough to implement these with very different scopes...)


```racket

#lang racket

(module topic1 racket
  ;; define $ as a "parameter", but make it look like a plain identifier
  (provide $ (rename-out [$if if] [$#%app #%app]))
  (define current-value (make-parameter #f))
  (define-syntax $
    (syntax-id-rules (set!)
      [(_ x ...) ((current-value) x ...)]
      [(set! _ val) (current-value val)]
      [_ (current-value)]))
  ;; make an `if' form that binds it to the condition result
  (define-syntax-rule ($if C T E)
    (parameterize ([current-value C])
      (if $ T E)))
  ;; function application with []s uses the topic variable for the first arg
  (define-syntax ($#%app stx)
    (syntax-case stx ()
      [(_ f x y ...) (equal? #\[ (syntax-property stx 'paren-shape))
       #'(parameterize ([current-value x]) (f y ...))]
      [(_ f x ...) #'(f x ...)])))

(module topic2 racket
  ;; better variant: define `$' as a syntax parameter, which is adjusted to an
  ;; actual local binding; make it work in `if', and have a function definition
  ;; form that binds it to the actual arguments
  (provide $ (rename-out [$if if]) defun)
  (require racket/stxparam)
  (define-syntax-parameter $ (λ(stx) (raise-syntax-error '$ "not in scope")))
  (define-syntax-rule ($if C T E)
    (let ([c C]) (syntax-parameterize ([$ (make-rename-transformer #'c)])
                   (if c T E))))
  (define-syntax-rule (defun name body ...)
    (define (name arg)
      (syntax-parameterize ([$ (make-rename-transformer #'arg)])
        body ...)))
  )

(module sample1 racket
  (require (submod ".." topic1))
  (if (memq 2 '(1 2 3)) (cadr $) 'missing)
  ;; => 3
  (define (foo) (list (sqrt $) (* $ $)))
  [foo 9]
  ;; => '(3 81)
  )
(require 'sample1)

(module sample2 racket
  (require (submod ".." topic2))
  (if (memq 2 '(1 2 3)) (cadr $) 'missing)
  ;; => 3
  (defun foo (list (sqrt $) (* $ $)))
  (foo 9)
  ;; => '(3 81)
  )
(require 'sample2)

```



## REXX

With this new definition of topic variables, the closest thing REXX has to a topic variable is probably function/subroutine arguments being "passed" to the target function/subroutine/routine/procedure.

```rexx
/*REXX pgm shows something close to a "topic variable" (for funcs/subs).*/
parse arg N                            /*get an arg from CL, maybe a 3? */
say  mysub(N)   '  ◄───'               /*invoke a function to square it.*/
exit                                   /*stick a fork in it, we're done.*/
/*──────────────────────────────────MYSUB subroutine (function)─────────*/
mysub:  return arg(1)**2               /*return the square of passed arg*/
```

'''output'''   when the following input is used:   <tt> 3 </tt>

```txt

9   ◄───

```



## Ring


```ring

see "sum1 = " + sum1(1,2) + nl
see "sum2 = " + sum2(2,3) + nl

func sum1 (x, y)
     sum = x + y
     return sum

func sum2 (x, y)
     return x + y

```

Output:

```txt

sum1 = 3
sum2 = 5

```



## Ruby

In Ruby the topic variable is $_ (same as Perl).


```ruby
while DATA.gets     # assigns to $_ (local scope)
  print             # If no arguments are given, prints $_
end
__END__
This is line one
This is line two
This is line three
```


```txt

This is line one
This is line two
This is line three

```

'''example:'''

```ruby
DATA.gets
p [$_.to_i ** 2, Math.sqrt($_.to_i)]        #=> [9, 1.7320508075688772]
__END__
3
```

The style of programming using $_ as an implicit parameter is gradually losing favor in the Ruby community.


## Scala


```scala
object TopicVar extends App {
  class SuperString(val org: String){
    def it(): Unit = println(org)
  }

  new SuperString("FvdB"){it()}
  new SuperString("FvdB"){println(org)}

  Seq(1).foreach {println}
  Seq(2).foreach {println(_)}
  Seq(4).foreach { it => println(it)}
  Seq(8).foreach { it => println(it + it)}
}
```



## Sidef

The underscore (''_'') topic variable is defined at compile-time in every block of a program. To call a method on it, we can just use the prefix dot (''.'') operator, followed by a method name, which is equivalent with ''_.method_name''

```ruby
say [9,16,25].map {.sqrt};   # prints: [3, 4, 5]
```



## Standard ML

The SML language itself does not define a topic variable, but interactive implementations may define their own.
For example the SML/NJ REPL defines a topic variable named <tt>it</tt> which is bound any time the user types an
expression in the REPL (as opposed to a declaration).


```sml
- 3.0;
val it = 3.0 : real
- it * it;
val it = 9.0 : real
- Math.sqrt it;
val it = 3.0 : real
-
```



## UNIX Shell


The shell $? is a kind of limited topic variable that holds the return value of the last function called. However, using it in a function will change its value, so following the echo below, the dollarhook will now contain the return value of zero indicating a successful echo:


```sh
multiply 3 4    # We assume this user defined function has been previously defined
echo $?    # This will output 12, but $? will now be zero indicating a successful echo
```



## VBA

VBA does not have special or topic variables. All variables have a letter as their first character.

## zkl

No topic variable pre se (a variable name can be a single character however), but underscore has a special meaning in some cases. Its use is scoped to the expression it is used in.

```zkl
a,_,c:=List(1,2,3,4,5,6) //-->a=1, c=3, here _ is used as "ignore"
3.0 : _.sqrt() : println(_) //-->"1.73205", _ (and :) is used to "explode" a computation
                            // as syntactic sugar
1.0 + 2 : _.sqrt() : _.pow(4)  // no variables used, the compiler "implodes" the computation
    // --> 9

```


