+++
title = "Icon+Unicon/Intro"
description = ""
date = 2016-03-07T19:58:06Z
aliases = []
[extra]
id = 7090
[taxonomies]
categories = []
tags = []
+++

= Introduction to Icon and Unicon =
== Purpose ==
The purpose of this page is to provide a Rosetta Code users with a enough supporting detail about Icon and Unicon to facilitate understanding and appreciation of these languages. It would be expected that the level of detail would be significantly smaller than any of the online books and reference materials listed on the [[Icon]] and [[Unicon]] pages.  

Some of the sections should be referenceable from code tasks.

=== Books, Technical Reports, Newsletters, and Other Documentation ===


### = General Language References =


These are general language references.


### == Unicon ==


* [http://unicon.org/book/ub.pdf Book: Programming with Unicon 2nd edition; Jeffrey, Mohamed, Gharaibeh, Pereda, Parlett - note also documents details of the Icon Programming Library]  
* [http://unicon.org/utr/utr8.pdf Unicon Language Reference]


### == Icon ==

* [http://www.cs.arizona.edu/icon/ftp/doc/lb1up.pdf Book: The Icon Programming Language, 3rd Edition; Griswold and Griswold]
* [http://www2.cs.uidaho.edu/~jeffery/icon/humanists/humanist.pdf Book: Icon Programming for Humanists; Corré]
* [http://www.tools-of-computing.com/tc/CS/iconprog.pdf Book: Icon Programming Language Handbook by T.W. Christopher]
* [http://www.mitchellsoftwareengineering.com/icon/icon.sli.pdf Presentation: Fundamentals of Icon Programming; Mitchell]

==== Articles, Newsletters ====
Excellent sources of programming insight.
* [http://www.cs.arizona.edu/icon/analyst/contents.htm Newsletter:  Table of Contents for the Icon Analyst from 1990-2001] discussing advanced Icon programming and  [http://www.cs.arizona.edu/icon/analyst/ia.htm Archived issues]
* [http://www.cs.arizona.edu/icon/inl/inlx.txt Newsletter: Table of Contents for the Icon Newsletter from 1979-2000] discussing Icon programming and [http://www.cs.arizona.edu/icon/inl/inl.htm Archived issues]
* [http://unicon.org/generator/ Newsletter: Archived issues of the Generator from 2004-2005] discussing Unicon programming


### = Programming Libraries =

* [http://www.cs.arizona.edu/icon/library/ipl.htm Code: The Icon Programming Library and index]
* [https://tapestry.tucson.az.us/twiki/bin/view/UniLib/WebHome Code: The Unicon Library]
* [https://tapestry.tucson.az.us/twiki/bin/view/Main/WebHome Code: The Tapestry TWiki examples of Icon and Unicon programs]

==== Graphics, Network Messaging, Threads, Patterns, etc. ====
These represent extensions that have been integrated into the core langauge(s).
* [http://www.cs.arizona.edu/icon/ftp/doc/gb1up.pdf Book: Graphics Programming in Icon; Griswold,Jeffery, and Townsend] 
* [http://unicon.org/utr/utr6b.pdf An IVIB Primer (Visual Interface Builder, UTR6b)]
* [http://unicon.org/utr/utr9b.pdf Unicon 3D Graphics User's Guide and Reference Manual (UTR9b superseding UTR9a)]
* [http://unicon.org/utr/utr10.pdf Debugging with UDB (UTR10)]
* [http://unicon.org/utr/utr12.pdf Ui: a Unicon Development Environment (UTR12)]
* [http://unicon.org/utr/utr13.pdf The Unicon Messaging Facilities (UTR13)]
* [http://unicon.org/utr/utr14.pdf Unicon Threads User's Guide and Reference Manual (UTR14)]
* [http://unicon.org/utr/utr18.pdf Pattern Matching in Unicon (UTR18)]


### = Implementation =

* [http://www.cs.arizona.edu/icon/ftp/doc/ib1up.pdf The Implementation of the Icon Programming Language; Griswold and Griswold]
** [http://www.cs.arizona.edu/icon/docs/ipd112.htm Report: IPD112, Supplementary Information for Version 8]
** [http://www.cs.arizona.edu/icon/docs/ipd239.htm Report: IPD239, Supplementary Information for Version 9]
** [http://www.cs.arizona.edu/icon/ftp/doc/ipd261.pdf Report: IPD261 Icon's run-time implementation language]
* [http://unicon.org/book/ib.pdf Book: The Implementation of Icon and Unicon: a Compendium; Jeffery]
* [http://unicon.org/utr/utr5b.pdf The Implementation of Graphics in Unicon Version 12 (UTR5b)]

== Icon and Unicon Differences ==

[[Icon|The Icon Programming Language]] was the successor of a series of non-numeric languages including [[wp:COMIT|COMIT]], SNOBOL, [[:Category:SNOBOL4|SNOBOL4]], and SL5.  Icon provided several innovations and improvements over its predecessors including:  integrating the the powerful pattern matching capabilities of SNOBOL4 into a procedural language, banishing a number of unfortunate foibles, retaining the flexibility of a typeless language, reining in some programming side effects, keeping platform independence.  Icon was one of the first bytecode languages.  And it has been undergoing continuous improvement from it's inception in the late 1970s.

Over the years various improvements, extensions, and experimental variants were added including a platform independent graphics interface, IDOL (an object oriented pre-processor), MT Icon (a Multi-Threaded variant), Jcon (an implementation in Java), and others.  And while the graphics interface was integrated into Icon, many of these variants were not.

[[Unicon|The Unicon Programming Language]] integrated a number of these extensions into a single variant of Icon.  Unicon includes object-oriented support, improved system interfaces, messaging and data base interfaces.  Additionally, a number of syntactic improvements to the language and semantic extensions to some functions have been implemented.  Because of this Unicon isn't completely a superset of Icon.

The differences between these language dialects are elaborated on later in this document (see [[Icon%2BUnicon/Intro#Appendix_A_-_Icon_and_Unicon_Differences_.28Details.29|Appendix A]]).

== Variables, Data Types, and Structures ==

=== un-Declarations, it's all about Scope ===

Icon and Unicon do not require strict static typing of variables as do languages like [[Pascal]], nor does it require type definitions to reserve space such as in languages such as [[C]].  In fact, variables may happily change type from one moment to the next. Knowing this you might expect that declarations are non-existent.

Declarations are optional and any undeclared variables are either (a) parameters to procedures or (b) local to procedures.  This design decision ensured that Icon/Unicon are not susceptible to the kind of side-effects that the global nature of variables in [[SNOBOL4]] led to.

Still, declarations are desirable for clarity and needed for a number of special cases:
* local - variables local to a procedure
* static - permanent variables that transcend individual calls of procedures.  While not visible outside of a procedure they are visible between different instances and across recursion of a procedure. 
* global - variables with global visibility

Additionally, the following declarations apply to non-variables:
* record - used to define a structure with named fields
* procedure - used to define a procedure and its parameters
* invocable -  used to control program linking to ensure procedures are included and available if they are called (e.g. through string invocation)
* class - used to define an object class (Unicon)

=== Self-Descriptive Safe Types ===

Icon/Unicon data types are safe because they are implemented internally with descriptors which provide the data within a container.  All operations on data know not only the value of the data but the type of the data.  Programmers cannot incorrectly interpret the data as is possible in languages without type enforcement such as [[C]] or assemblers.  Similarly, programmers are not constrained as with languages like [[Pascal]] that have strong static typing.  Strong dynamic typing at run time results in Icon/Unicon because all operations on these self descriptive data types are consistent and correct.


###  Mutable and Immutable Types 


Icon/Unicon has both mutable (changeable) and immutable (unchangeable) data types.  All operations deal with these consistently and it isn't possible to directly discern which are which (say by returning the address of a value).

There are operations which can create separate copies of types and distinguish between different copies of mutable types.  These operations can be applied to immutable types in an intuitive manner and they perform consistently within those contexts as is shown in the following code snippet:


```icon
#  copy .v. assignment

   mutable := []         # lists are mutable
   immutable := "abc"    # strings are not
   m2 := mutable         # assignment copies the reference
   m3 := copy(mutable)   # creates a (1 level) copy
   i2 := immutable       # assignment copies the reference
   i3 := copy(immutable) # same as assignment

#  value equal ( === )

   mutable === m2        # succeeds
   mutable === m3        # fails
   immutable === i2      # succeeds
   immutable === i3      # also succeeds
```


Furthermore even though strings are immutable, it is possible to construct the same string in different memory locations.  You just can't tell if they are different or not.


###  Data Types 


The following summarizes the data types of Icon and Unicon.  Each type is described and its characteristics are noted:
* [[Icon%2BUnicon/Intro#Mutable and_Immutable Types|Mutable .vs. Immutable]] - changeable or not 
* coercible or not - coercion is implicit type conversion by operations
* convertible or not - convertible through explicit type conversion


### = null =

null (immutable, uncoercible, unconvertable) is unique.  It is a data type with only a single value, one instance only.  While this may sound odd at first, &null is at the core of Icon/Unicon and contributes to the power and robustness of the language.  Specifically, null arose to overcome a short coming that any [[SNOBOL4]] programmer will be familiar with.  Consider the following SNOBOL code:


```Snobol
    numb1 = 2
    numb2 = 3
    output = "numb1+numb2=" numb1 + nmub2
```


In SNOBOL an undefined value defaults to the null string "" and the null string is coerced into a zero 0 if needed for math.  Thus the surprise output above is 2 and not 5 as expected.  While on close inspection the error is apparent, you can see how this could be missed in a large program predominated by global variables.

In Icon/Unicon &null is not coerced by operators and so:

```Icon
   write( 2 + &null )         # run time error 101
   write( "abc" || &null )    # run time error 103
   write( "abc", &null)       # no error as write ignores &null 
```


The power comes from the simple null/non-null tests of which more anon.


### = integer =

Integers (immutable,coercible,convertible) come in two forms.  Regular integers and long integers; however, these are handled transparently for the programmer. Integer operations will coerce strings (or csets) of digits into integers.  Otherwise, integers are much like integers in many other languages.  Operations on two integers results in an integer.


### = real =

Reals (immutable,coercible,convertible) are available in one size (large).  Strings (and csets) can be coerced into reals by operations.  Otherwise operations with mixed reals and integers coerce the integers into reals.


### = string =

Strings (immutable,coercible,convertible) are variable length and may contain any character value within the platform's character set including the NUL character.  String operations may coerce other data types such as integers or reals.  Also strings are coerced by operations on other types.  Syntactically, strings are delimited by double quotes (") and escape special characters using a back slash such as in this example "\"".  

At the current time there is no support for Unicode.


### = cset =

Character sets or csets (immutable,coercible,convertible) is a special type that is used in conjunction with string scanning and string matching operations.  Syntactically, they look similar to strings but are delimited with single quotes (').  Semantically, csets are sets of characters in a universe consisting of the platforms full character set. Operations on csets include unions and intersections.  Csets may be coerced to or from strings and numeric values.


### = Lists =


Lists (mutable) are variable length structures containing arbitrary values.  Lists are indexed and accessed with integers ranging from 1 to the size of the structure.

Operations exists to work with lists as arrays and as queues, and stacks.


### = Records =

Records (mutable) provide fixed size data groupings that are accessible through field names. 

Dynamic records can be constructed in Unicon with the constructor procedure.  This is useful in conjunction with database access.


### = Sets =

Sets (mutable) are unordered structures that contain unique instances of arbitrary values.  Operations such as insertion, deletion, testing membership, intersection, and union are provided.  Lists can be explicitly converted into sets.

Please note that copies of mutable types are considered as distinct values.

```Icon
   L1 := []        # a list
   L2 := copy(L1)  # and list
   S1 := set()     # a set
   S2 := set()     # another
   every insert(S1,L1|L2)   # S1 will have 2 members
   every insert(S2,L1|L1)   # S2 will have just 1
```



### = Tables =

Tables (mutable) are structures where values are indexed by unique keys.  Syntactically they look a lot like lists indexed by arbitrary values.  Tables are one of the most commonly used and powerful features of Icon/Unicon.  

```Icon
   T := table(0)           # table with default 0 values
   every T[!words] +:= 1   # counts words
```


==== co–expressions ====

Co-expressions are a way of encapsulating code and state outside of the bounds of the normal program flow and scope. They can be used to create co-routines.  Co-routines are created in a dormant state and can be passed around, copied and rewound, and invoked or activated anywhere within a program.  

One of the most powerful aspects of co-expressions is the ability to use them to produce programmer defined control operations (PDCO).


### = procedures =


Procedures are also data types that can be assigned both to and from.  This allows for interesting capabilities such as tracing built-in functions, inserting a processing wedge around a procedure, creating procedures that can apply arbitrary functions, and modifying the behavior of procedures in more radical ways.

Consider the following code snippet which sets the variable verbose to the procedure write if any argument is "--verbose" or 1 (effectively a no-operation) otherwise:

```Icon
   if !arglist == "--verbose" then verbose := write else verbose := 1
   ...
   verbose("Some verbose mode diagnostic message :",var1, var2)

```


Examples of parametrized procedures can be found in [[Sorting algorithms/Bubble sort#Icon and Unicon]] and [[Apply a callback to an array#Icon and Unicon]].

==== classes and objects (unicon) ====
Unicon allows for user defined class and method definitions. Individual instances of these objects (mutable) can be created by constructor functions.


### = file and windows =


Files and windows (mutable) allow Icon/Unicon to interface with the operating system.  Operations on files and windows have side effects and they are considered mutable. Sockets and pipes are also implemented as "files".


###  Keywords 

Icon and Unicon has a list of special variables known as keywords.  Syntactically, keywords are variable names preceded by an &.

Keywords can be used to inquire about the current state of the program, as constants, and to modify the operation (tracing and error control).  For a list of keywords see [[Special_variables#Icon_and_Unicon]].

== Operators and Procedures ==

###  Intuitive Generalizations 


One of the strengths of Icon/Unicon is that for the most part operators work on intuitive level across types yielding results that make sense.  Examples, include:

: *x returns the size of x.  Number of characters for strings and csets.  Number of elements in a table, list, or set.  The number of defined fields in a record. And the number of times a co-expression has been evaluated.
: !x generates the elements of x.  Characters for stings and csets, Elements for tables, lists, sets, and records.
: ?x returns a random element or character from x.
: x[a:b], x[a+:b], x[a:-b] return subsections of lists and strings.

This philosophy is continued in many of the built-in functions.


###  Strong Typing through Operators 


Icon/Unicon has variously been described as an untyped or strongly typed langauge depending upon the perspective of the observer.  Consider the following examples:
* The lack of declaration and reassignment of x to different types of data suggest the language is untyped or loosely typed.

```Icon
  x := 1
  x := "Abc123"
  x := table()
```

* Having specific operators for ambiguous operations, such as comparisons, means the intent of the program should be clear.  While this can sometimes be annoying (see [[Sorting algorithms/Bubble sort]] it is necessary because in the case of mixed type comparisons the intent of a generalized comparison can't be determined and the operator would not know which operand to coerce.

```Icon
  a := "11"
  b := 2
  if a << b then write("a is lexically less than b.")     # "11" << "2" is true
  if a > b the write("a is numerically greater than b.")  # 11 > 2 is also true
```

Additionally, the strong typing is supported by safe data types.


###  Coercion: Implicit Type Conversions 


Icon/Unicon performs implicit type conversions (called coercion) where it makes sense to do so.  At the same time dangerous coercions are disallowed.   Details of these coercions are covered under the topic of [[Icon+Unicon/Intro#DataTypes|Data Types]].  Where a coercion is not possible, a run-time error is generated.


###  Lists of Operators 


In the lists below the following represent data by type:
: x - anything
: s - string
: n - numeric
: i - integer
: c - cset 
: S - set (or cset)
: C - co-expression
: v - variable


### = Assignment and Control Operators =



```Icon
  v := expr    # assignment
  v op:= expr  # augmented assignment (see below)
  v :=: v      # swap
  v <- expr    # conditional assign
  v <-> v      # reversible swap
```


  |x           # repeated alternation
  x | x        # alternation
  x \ i        # limit generation to i results
```



### = Unary =


```Icon
  !x           # generate elements
  /x           # null test
  \x           # non null test
  +n           # number (forces conversion)
  -n           # negate number 
  =s           # tab(match(s))
  *x           # size
  .x           # dereference to value
  ?x           # random element/value
  ~c           # cset complement
  ^            # regenerate co-expression
```


==== Binary (and Augmented) ====

```Icon
  n ^ n        # power
  n * n        # multiplication
  n / n        # division
  n % n        # modulus
  n + n        # addition
  n - n        # subtraction

  S ** S       # intersection
  S ++ S       # union
  S -- S       # difference
 
  n = n        # equal
  n ~= n       # unequal
  n < n        # less than
  n <= n       # less than or equal
  n > n        # greater than
  n >= n       # greater than or equal
  
  s == s       # equal
  s ~== s      # unequal
  s << s       # less than
  s <<= s      # less than or equal
  s >> s       # greater than
  s >>= s      # greater than or equal

  x === x      # equivalent
  x ~=== x     # not equivalent

  s ? expr     # scanning
  x @ C        # activate (x optional)
  s || s       # string concatenation
  L ||| L      # list concatenation
  x & x        # conjunction
```


The operators above may be used in augmented form with normal assignment (i.e., x1 op:= x2; short for x1 := x1 op x2) .  Many of these are common and natural, such as any of the mathematical group, concatenation, and comparisons for sorting.  And the most common uses will be building results inside loops or scanning operations.  Some of the examples include:

```Icon
  while x1 +:= x2   # accumulation
  while x1 *:= x2   # product
  while S1 ++= S2   # builds a set/cset
  while x1 @:= C    # repeatedly call a co-expression building on its argument
  while s1 ||:= s2  # build a string
  while L1 |||:= L2 # build a list
  while x1 <:= x2   # find maximum
  s1 ?:= expr       # replaces string subject
```


Some others are less obvious, such as:

```Icon
  if lastx ~===:= x then ... # do work and remember when something changed
```


Others are obscure or even a bit baffling, such as:

```Icon
  x1 ===:= x2        # obscure x1 := x1, can this be more that a no-op?
  s1 ==:= s2         # ensures arguments are both strings (assigns but no value change)
  n1 =:= n2          # numeric version of above
  x1 &:= x2          # x1 := x2 unless x1 or x2 fails
```


Note that some documentation indicates x1 |:= x2 exists, but it isn't implemented at this time.

== Program Flow and Control ==

At the core of the design of Icon/Unicon are a series of features that contribute to the power of the language.  These features set Icon/Unicon apart from many traditional procedural languages.


###  Failure is an Option 


Expression failure is a signal that cannot be ignored and is used to control program flow whether with operators, control structures, or procedures.  Failure effectively short circuits the evaluation of the expression and forces processing into an alternate path if any exists. 

This differs from the approach of many traditional procedural languages in which comparison operators return a true/false value. No value is associated with failure.  It also means that evaluation of an expression will be force you onto the correct logic path.

The following are equivalent:

```Icon

procedure AllwaysFail()   # return a fail signal (explicit)
return fail
end

procedure AllwaysFail()   # also return a fail signal
&fail
end

procedure AllwaysFail()   # return a fail signal (implicit)
end
```


=== Everything Returns a Value Except when it Doesn't ===

In Icon/Unicon just about everything expression returns a value (unless it fails).  

For example:

```Icon>   d := if a 
 b then a - b else b - a      # returns the positive difference of a and b
   d := a - (a > b) | b - a                 # is equivalent
```


In the above example the expression, a > b, either succeeds returning b or fails.  The alternative paths are provided by (else) in the first case and (|) in the second.

A second consequence of comparison operators returning values rather than a true/false is that you can write expressions like these:

```Icon
  i < j < k      # succeeds returning k, if i < j < k 
  (i < j) < k    # shows more clearly how this works.  Note if i < j fails the expression fails and nothing further is evaluated
```


=== Goal-Directed Evaluation and Generators ===

A central feature of Icon and Unicon is what is known as Goal-Directed Evaluation, and the intimately related concept of Generators. Briefly the idea is that expressions can yield more than one result (Generators) and if a further part of the expression results in failure, the earlier Generators will be driven to yield more results. These features implement [[:Category:Programming_paradigm/Logic_Programming|Logic Programming]] paradigms not unlike the backtracking found in Prolog or Regular Expressions.  These features are built into the very core of the language. Prolog programmers will find it very familiar but of course with differences because Icon and Unicon do not use the functional language pattern matching technique of Prolog.

As noted previously, when an Icon/Unicon expression fails it will take an alternate path such as an else.  In the case of generators, the alternate path is a resumption of the generator in order to produce another result. Thus expressions can consume the results of generators until they achieve their desired goal.  If no result is acceptable then the overall expression fails.

Icon and Unicon provide a variety of operators, control structures, and procedures that work with generators.  Examples include:

```Icon
   every expression do expression    # a looping control that forces the generation of every result
   X[1 to 10 by 2]                   # 'to'/'to by' is a generator that yields successive numerical results
   !X                                # generate every element of X
   suspend expression                # used instead of return inside a generator to setup for resumption of the procedure
   |expression                       # turns non-generators, like read(), into endless generators
```


Another way of looking at it is to understand that every expression can yield a ''result sequence'' and any code using this expression may choose to ask for more results, gather them into a container or aggregate, or choose to use one value and then move on without asking for all possible results.


###  No Spaghetti 

The clue's in the title. There is no 'goto' in Icon/Unicon.


###  Procedure Controls 


### =procedure=


Procedures are defined as given below.  All procedures have global visibility (there is no nesting). There may be an arbitrary number of arguments.  Variable numbers of arguments may be accommodated by specifying one of the arguments as a list.

```Icon
procedure name(arg1,arg2,arg3,arg4[])
...
end
```



### =fail=

Causes the the enclosing procedure to terminate without returning value. This is different from returning void or a null value that many other languages do when the code does not return an actual value. 


```Icon
   x := "unassigned"
   x := arglist[i]    # Succeeds assigning x the value of arglist[i] or fails (leaving x unchanged) if i is out of bounds.
   write(x)
```



### =return expr=

Return the results of 'expr' (the default value of 'expr' is ''&amp;null''). Apart from the usual meaning of ''return'', if the ''expr'' fails, then the procedure fails as well.  See [[Icon%2BUnicon/Intro#Failure_is_an_Option|Failure is an Option]] above.  If the ''expr'' is capable of yielding more than one result, only the first result is used.


### =suspend expr=

Suspend is semantically similar to 'return' with the exception that it sets up the possibility of producing additional results if needed.  Rather than terminating the procedure as return does, suspend returns a result while leaving the procedure in suspension in the event additional results are needed.  A suspended procedure will resume at the next point in the code.   This capability is built directly into the run time rather than being an artificially constructed behaviour provided by Python or C#'s use of the 'yield' keyword. Every and all expressions may suspend or be involved in a suspending expression without any effort. Behaviorally this is closer to Prolog which also supports backtracking as a core part of the language. If the ''expr'' is capable of yielding more than one result, then suspend (if driven) will progressively yield all of those values. If the expression fails, execution continues within the procedure until the next suspend, return, or fail.

A procedure can contain several uses of ''suspend'' and it's quite reasonable for the procedure to execute many of them in any chosen order.


###  Selection Controls / Control Structures 

All Icon and Unicon expressions, including control structures, yield results or signal failure. (Text from [[Conditional_structures#Icon_and_Unicon|Conditional Structures]]).

### = if then else =

Icon/Unicon has an if then else expressions.  Like many languages the else is optional.  The control structure evaluates expr1 if expr0 succeeds and expr2 if it fails.  Braces  are required if multiple expressions lines are needed.

```Icon
if expr0 then 
   expr1
else 
   expr2
```



### = case of =

The first successful selection expression will select and evaluate the specific case.

```Icon
case expr0 of {
   expr1 : expr2
   expr3 : expr4
   default: expr5
   }
```

Note that expr1 and expr3 are expressions and not constants and it is possible to write expressions such as:

```Icon
case x of {
   f(x) | g(x) : expr2
   s(x) & t(x) : expr4
   default: expr5
   }
```


====Compound expressions (blocks)====
In the examples below, multiple expressions can be grouped as in:

```Icon
{
   expr1
   expr2
   expr3
}
```

Which is equivalent to this:

```Icon
{expr1; expr2; expr3}
```

For example the following, which will write 4, looks strange but is valid:

```Icon
write({1;2;3;4})
```

The value of a compound expression is the value of the last expression in the block.

### =Alternation=

Alternation of expressions yields a value for the first succeeding expression.

```Icon
   expr1 | expr2 | expr3
```


### =Conjunction=

Conjunctions yeild the value of the final expression provided all the previous expressions succeed.

```Icon
   expr1 & expr2 & expr3
```

Alternately, conjunction can be written thus:

```Icon
   (expr1, expr2, expr3)
```

====Conjunction, yielding a different result====
The alternate form of conjunction can be modified to produce a different result (other than the last)

```Icon
   expr0(expr1, expr2, expr3)
```

For example: 

```Icon
   2(expr1, expr2, expr3)
```

Yields the value of expr2 if all of the expressions succeed.

A more complicated example showing non-constant expressions:

```Icon
   f(expr1)(g(expr1)(expr3,expr4,expr5))
```

Note: if expr0 yields a value of type 'procedure' or 'string' the appropriate procedure (or operator) is invoked.


###  Looping Controls 



### =repeat=

Repeat will loop endlessly and must be explicitly broken out of with a break, return, or suspend.


### =while=

While will loop as long the expression succeeds.  An optional expression can evaluated upon success via the do clause.


### =until=

Until will loop while the expression fails.  An optional expression can evaluated upon success via the do clause.


### =every=

The every clause will produce all instances of a generator.   An optional expression can evaluated upon success via the do clause.

The every clause is often used with the ternary operator to-by.  Because to-by is an operator it can be combined in interesting ways with other operators or itself.  For more examples, see [[Loops/For_with_a_specified_step#Icon_and_Unicon|Loops with a specified step]].


### =next=

Skips to the beginning of the next loop.  Restarts the enclosing loop. The conditional on the loop is evaluated as normal.

### =break expr=

Break is used to break out of or exit from one or more enclosing loops.  By default value of ''expr'' is ''&amp;null''.  Although a bit unusual, most loops can yield results and it is possible to write code such as this:

```Icon
   x := while expression1 do {
      ...
      if expression2 then break "1" 
      }    # x will be the string "1" 
```


Break can be used consecutively to break out of nested loops, such as in:


```Icon>    break break next</lang



###  Signals and Exceptions 


### = stop =

Terminates the current program and writes the result of expression to a file (&errout by default).  For example:

```icon
   stop(&output,expr1,expr2)  # writes expr1 and expr2 to standard output and terminates the program
```



### =error trapping=

The keyword '&amp;error' is normally zero, but if set to a positive value, this sets the number of fatal errors that are tolerated (i.e. converted to expression failure).  The value of &amp;error is decremented each time this happens. Therefore the now-common TRY-CATCH behaviour can be written as:


```Icon
    &error := 1
    mayErrorOut()
    if &error == 1 then
        &error := 0     # clear the trap
    else {
        # deal with the fault
        handleError(&errornumber, &errortext, &errorvalue)   # keyword values containing facts about the failure
    }
```



### =error throwing=

Errors can be thrown like this:

```icon
    runerr(errnumber, errorvalue)    # choose an error number and supply the offending value
```


=== Co-expression Flow ===
Co-expressions are an extremely powerful mechanism for program flow control that provide parallel execution.  Consequently, co-expressions facilitate flows that can be radically different from what would normally be expected in procedural languages.  Understanding, really understanding at a gut level, how these works comes with experience.

Among other uses, co-expressions can be used to implement: 
* co-routines
* programmer defined control objects (PDCOs)
* result sequences
* exception mechanisms
* closures
* pipeline flows

Some characteristics of co-expressions:
* They encapsulate their working environment.  They will have their own copies of local variables. They can maintain their state separate from their caller.
* Mutable local variables are still subject to side-effects if the objects they refer to are modified.
* They often have multiple entry points: the initial entry point and subsequent transfer points.
* Transfer points bind 'calling entry' and 'return' at one location in the code.
* They may need to be primed or started in order to get them to a transfer point.
* Arbitrary numbers of co-expressions may be run in parallel.
* Flows between co-expressions don't need to be hierarchical.

And while the original motivations for co-expressions was to facilitate co-routine calling flows, many of the solutions that use them do so to take advantage of these other characteristics.

==== Co-Expression Articles and References ====

Steve Wampler's "Fun With Co-Expressions" articles in The Generator 
* [http://www.unicon.org/generator/TheGeneratorVol1No1Letter.pdf "Fun With Co-Expressions, part 1"]
* [http://www.unicon.org/generator/v2n1.pdf "Fun With Co-Expressions, part 2"]
* [http://www.unicon.org/generator/v2n2.pdf "Fun With Co-Expressions, part 3"]

Other articles 
* [http://www.drones.com/coexp/ Shamim Mohamed's article on co-expressions]


### = Example: Function Composition =


The following Unicon example is taken from the Rosetta Code task  [[Function_composition#Unicon|function composition]] and uses a co-expression in a very procedural way.

```Unicon

    g := compose(sqrt,abs)
    h := compose("-",g)
    h(-49)

```



```Unicon

procedure compose(fL[])   #: compose(f1,f2,...) returns the functional composition of f1,f2,... as a co-expression
    ...
    fL := reverse(fL)                                    # reverse and isolate from mutable side-effects 
    cf := create {  saveSource := &source                # don't forget where we came from
                    repeat {
                        x := (x@saveSource)[1]           # return result and resume here
                        saveSource := &source            # ...
                        every f := !fL do x := f(x)      # apply the list of 'functions'
                        }
                 }
    return (@cf, cf)                                     # 'prime' the co-expr before returning it
end
```

Explanation:
* '''compose''' is called as a normal procedure and returns a composite function as a co-expression ('''cf''').
* When it returns '''cf''' it first 'activates' or 'primes' it by means of '''@cf''' the co-expression which executes up to the first transfer point '''x := (x@saveSource)[1]''' where the co-expression transfers back to its caller to wait for input.
* The co-expression is now primed and ready for use .
* When the composed functions '''h''' and '''g''' are 'called', execution resumes at the transfer point with '''x''' taking on the value transmitted from its caller (a list).
* ''Note: we don't use the calling syntax:'' '''x := (saveSource(x)[1]''' to transfer back because this would force the use of a '''h(x)[1]''' wherever the composed function is called.
* Because this example acts like a traditional procedure and can be nested it is important to save the callers identity so we can return correctly.  Clearly, arbitrarily complex transfer flows are possible here.
* After completing the core of the procedure (apply) the co-expression cycles and waits for another value.  There is no obvious '''return''' or completion step.  This co-expression never terminates and never needs to be refreshed.

== Contractions ==
Icon/Unicon have a rich set of [[Icon%2BUnicon/Intro#Binary_.28and_Augmented.29|augmented operators]] operators which when combined with the fact that all successful expressions produce values makes possible contractions.  These appeal of these is somewhat a question of style and if taken to extremes there is also the possibility of being overtaken by one-liners.  Having said that Icon/Unicon contractions are hardly in the same league as those of [[APL]] or [[J]].  A number of examples are presented below. 

These examples initializes sum and adding all the contents of arglist.  

```Icon
sum := 0                                             # initial and                            
every sum +:= !arglist do something()                #    loop in two statements

sum := 0; every sum +:= !arglist do something()      # ; splice

every (sum := 0) +:= !arglist do something()         # a common contraction, iteration begins at the !

while (sum := 0) +:= !arglist do something()         # an error.  while will only get the first result
```


More examples of initialization in every loops:
```Icon
every put(L := [], !X)                               # creates a empty list before populating it
every (s := "") || !S                                # create a null string before appending
```


Examples of using program control structures in expressions:

```Icon
   (if i > j then i else j) := 0     # sets the larger of i and j to 0

   d := if a > b then a-b else b-a   # sets d to the positive difference of a and b 

   x := case expr of {
      1: "Text 1"
      2: "Text 2"
      default: "Undefined text"
   }                                 # sets x to a string based on the value of expr
```


The following may vary well be the most common contraction used in Icon/Unicon, initially assigning a value.

```Icon
   if /x then                    # if x is null
      x := value                 # set it

   ...

   /x := value                   # contraction of the above
   /x[y] := value                # contraction using a table or list element
```


The following contraction can be used to do work in a conditional control structure. It takes advantage of the fact that comparison operators return results or fail.   The only caveat is that the rest of the expression must not 'fail'.  Note also this will not work with 'every'.


```Icon>while m 
 0 do {  # separate condition and statement
   x := m + b
   ...
   }

while x := (0 < m) + b do {  # contraction
   ...
   }
```



Deciding on the level of contraction is an art and, like any art form, easy to abuse.  For example, the following procedure (from the Van de Corput Sequence task solution):

```Unicon
procedure vdc(n, base)
    e := 1.0
    result := 0.0
    while result +:= 1(((0 < n) % base) / (e *:= base), n /:= base)
    return result
end
```

already contains a number of contractions.  Pushing this well beyond the bounds of
human decency yields:

```Unicon
procedure vdc(n, base)
   every ((e:=1.0, result:=0) +:= |((0 < 1(.n,n/:=base))%base/(e*:=base))) | return result
end
```

This is probably only lovable by J programmers.


###  An Ugly Example 


The following procedure "IsFilled" was taken from the [[Sierpinski_carpet#Icon_and_Unicon|Sierpinski Carpet task]]:
```Icon
procedure IsFilled(x,y).  It has the benefit of being clear.  It makes use of only a few natural contractions, augmented division and chaining of comparison operators.  
   while  x ~= 0 & y ~= 0 do {
      if x % 3 = y %3 = 1 then fail
      x /:= 3
      y /:=3
      }
   return
end
```


We can transform this.  Even if this result isn't helpful, understanding what can be done and how it works may be useful for less extreme cases.

Firstly, we can rewrite the while condition in the following ways:
```Icon
while  x ~= 0 & y ~= 0 do {                # original
until not ( x ~= 0 & y ~= 0) do {          # inverted 
every | (0 = (x|y) & break) | &null do {   # converted to an every loop (but incomplete)
```

One point worth noting is that expressions normally use short circuit evaluation.  This construct forces both alternatives to be generated even if the second isn't needed. If ''y'' is a complex expression with side-effects this could have unexpected results.

The final transformation of a while into an ''every'' expression can be useful sometimes, but its not without challenges and can produce ugly results.

The failing condition can also be converted as in these examples:
```Icon

   if x % 3 = y %3 = 1 then fail           # original
   ( x % 3 = y %3 = 1) & fail ) | &null    # converted for use with every | expr
```


The ''do'' part can be converted using conjunction.
```Icon

      x /:= 3                              # original 
      y /:=3                               # ... 
      ( x /:= 3 & y /:=3 )                 # contracted
```


Putting these together and eliminating the trailing ''return'' in favor of the default fail at end, we get the rather odd looking:
```Icon
procedure IsFilled(x,y)        
   every | ( (0 = (x|y) & return) | &null, 
             (( x % 3 = y %3 = 1) & break ) | &null,
             ( x /:= 3 & y /:=3 ) )
end
```


It's tempting to remove the ''| &null'' choices and convert the conjunction ''(expr1, expr2, expr3)'' to alternations but this isn't equivalent.

The alternations with &null can be eliminated by rewiring the path of alternatives with not expressions:
```Icon
procedure IsFilled(x,y)        
   every | (not (0 = (x|y), return)) & 
           (not ( x % 3 = y %3 = 1, break )) & 
           ( x /:= 3 & y /:=3 ) 
end
```


I'll resist compressing this into a one-liner.

The need for alternation with &null may require some explaining.  Repeated alternation is defined as:

```txt
  |expr repeatedly evaluates expr, producing its entire result
  sequence each time, but failing if evaluation of expr results
  in an empty result sequence.
```


The additional alternations are required to prevent the expression from running out of results.  Part of this is due to the use of ''return'' and ''break'' to exit the loop preventing further results being generated.  

The loop can be abstracted so:

```Icon
every (|A()) | B() | C()
```


Without the &null alternations (or the not expressions), once the repeated alternation fails there are no more results left to generate or backtrack into and the expression must move on.  In the example, neither A() nor B() would not produce any results as they exit if successful.  Since C() produces only one result and that ends the evaluation.

Overall, this example is not a very good use of these techniques.

== Odds and Ends ==
This section is a catchall for a number of Odds and Ends.  In some cases, the the only criteria for inclusion is that they may be found a bit odd and have no other logical home.

=== &fail considered harmful? ===

Okay, &fail is not the evil subversive scourge of programming that the GOTO was once (still is?).  However, at first glance it seems to be an oddity, it's easy to wonder about it's real worth.

In a recent discussion, someone expressed that &fail seemed like an anchor to the notion of languages that return Boolean values.  And while that isn't quite how it works the appeal of that idea, especially to people new to the language, makes sense on some gut level.  '&fail' simply causes the current expression to fail.  Yet, you will occasionally see it used like so:

```Icon
...
return &fail          # &fail fails and return causes the procedure to fail.
write("Surprise!")    # never executed
end
```

Yet, in this example:

```Icon
...
suspend &fail         # &fail fails and suspend fails 
write("Surprise!")    # this time this IS executed, and the procedure fails when it runs into the 'end' statement
end
```

Just for completeness:

```Icon
...
fail &fail            # is a syntax error as fail takes no expression

```

As one can see this is less about the behavior of &fail as it is about the behavior of return and suspend.

'&fail' does have uses as can be seen in this example from [https://www.cs.arizona.edu/icon/progcorn/pc_inl21.htm| The Icon Newsletter's Programming Corner]:

```Icon

#  The outcome of a looping expression such as 

   while expr1 do expr2

#  need not be failure. If a break expression in either expr1 or expr2 is evaluated, 
#  the outcome of the looping expression is the outcome of the argument of the break expression.

# It is common to omit the argument of a break expression. In this case, the argument defaults to a null value. 
# Consequently, if the break expression in 

   while expr1 do {
   . . .
   break
   . . .
   }

#  is evaluated, the outcome of the looping expression is the null value. 
#  In fact, if this effect is not wanted, 

   break &fail

#  can be used to assure the outcome of the looping expression is failure.

#  However, the argument of a break expression can be a generator. For example, if break 1 to 5
#  is evaluated in a looping expression, the result sequence for the looping expression is {1, 2, 3, 4, 5}.

```


=== Semi-colons ===

Unicon and Icon don't require you insert semi-colons as line terminators like some languages.  Instead the compiler/translator detects the end of an expression and automatically inserts one for you.  Two caveats, (1) you must insert them yourself if you wish to write multiple expressions in a line, and (2) if you split an expression across a line boundary at an operator you must leave the operator on the first line or your expression will be prematurely terminated.  As follows:

```Icon
   x := 1; y := 2; z:= -3    # 1st case
   s3 := s1 ||
         s2                  #   2nd case - like this 
   s3 := s1
      || s2                  #   ...        not this
```


The other place this comes up is writing one-liners.  While not a recommended style, there are tasks that require it [[Narcissist]] and [[Quine]] are examples.  Consider for example the following two programs:

```Icon
procedure main();sub();end;procedure sub();write("1st example");end
```


```Icon
procedure main();sub();end procedure sub();write("2nd example");end
```


It might seem natural at first to write the first example, however, no semi-colon is needed between and end and procedure declaration.  The second case is correct and the first generates a compile error.

= Run Time Considerations =
== Environment Variables ==

The following variables override the default storage allocations

* BLKSIZE    Region (bytes) used for block data types like tables and lists
* COEXPSIZE  Space (words) used for co-expressions
* MSTKSIZE   Space (words) used for the main interpreter stack
* STRSIZE    Region (bytes) use for strings

Other environment variables
* IPATH      Path for pre-compiled library routines
* LPATH      Path for source include files
* ICONFONT   Default font name for windows opened
* TRACE      Sets &trace for tracing
* NOERRBUF   &errout (stderr) is buffered unless this is set.
* WICONLOG   File to record output from wicont and wiconx


The defaults for [http://unicon.org/utr/utr7.html Unicon are documented in UTR7]

== Memory Allocation ==

Several keywords are available that provide information on memory allocations:
* &allocated - total memory allocated (bytes) - - generating ( total, static, string region, block region )
* &collections - number of times the garbage collector has reclaimed memory ( total, static region, string region, block region).  
* &regions - current memory size- generating ( static, string region, block region ).  
* &storage - current memory used - generating ( static, string region, block region ). 

Note: the value produced for static should always be 0 and is left for program compatibility. Sometimes you can see a non-zero number for static region collections (these are considered phantom collections - and this may have been fixed).

= Appendix A - Icon and Unicon Differences (Details) =

The purpose of this section is to capture some of the langauge differences at a detailed level. '''Note: this is not a complete list'''

== Major Differences == 

###  Classes and Objects 


```Unicon
package packagename

class classname : superclass(attributes)
   method meth1(att1)
       ...
       end

   method meth2(att1,att2)
      ...
      end

   initially (att1,att3 )
      ...
end
```


...

   object.name(parameters)
   object$superclassname.name(parameters)

== Minor Differences ==

There are numerous minor differences between Icon and Unicon and they are not rigorously documented.  See [http://unicon.org/generator/ The Generator articles entitled: Under-documented Unicon] for more information or contribute below.

=== Co-expression Calling ===
With co-expression ce,  the following pairs of activations are equivalent in Unicon: 

```Unicon
   []@ce          # sends a list to co-expression ce both Icon and Unicon
   [x,y,z]@ce
   ce()           # equivalent calls Unicon only
   ce(x,y,z)
```



###  Procedure Parameter Type Casting 

Unicon procedure definitions allow for specification of type coercion functions and default values.  There are currently two cases:
* for records and structures the type of the parameter is tested
* for types with type coercion functions (e.g. integer, real, numeric, string) the function is called to test type and perform type coercion.

```Unicon
procedure f1(i:integer:1,r:real,s:string,n:numeric,S:set,L:list,x:myrecordtype)
```


=== Event monitoring and cross-co-expression introspection === 
Unicon extends a number of procedures with extra parameters that default to the current co-expression and the current call level.

```Unicon
   name(s)             # Icon - return variable name
   name(s,C)           # Unicon 
   variable(s)         # Icon - local/global variable may be read or set 
   variable(s, C, i)   # Unicon - extended to access co-expression C and i levels up the call chain

```



###  Procedures with Extended Domains 

Unicon extended the behavior of a number of procedures.

```Unicon
  reverse(x)           # Icon x is string,  Unicon x is string,list
```

== The Unicon Pre-processor ==
Unicon implements some of its functionality in the translator (icont) and the rest in the pre-processor (unicon).  The pre-processor is typically used for syntactic enhancements like objects and parameter type checking.  

If you are interested in seeing the code generated by the pre-processor, try the following command:

```txt
unicon -E source.icn
```

