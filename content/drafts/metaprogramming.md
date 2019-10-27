+++
title = "Metaprogramming"
description = ""
date = 2019-03-29T21:31:54Z
aliases = []
[extra]
id = 8714
[taxonomies]
categories = []
tags = []
+++

{{task}}{{omit from|BBC BASIC}}

Name and briefly demonstrate any support your language has for metaprogramming. Your demonstration may take the form of cross-references to other tasks on Rosetta Code. When possible, provide links to relevant documentation. 

For the purposes of this task, "support for metaprogramming" means any way the user can effectively modify the language's syntax that's built into the language (like Lisp macros) or that's conventionally used with the language (like the C preprocessor). Such facilities need not be very powerful: even user-defined infix operators count. On the other hand, in general, neither operator overloading nor <code>eval</code> count. The task author acknowledges that what qualifies as metaprogramming is largely a judgment call.



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.win32}}
ALGOL 68 allows the definition of new unary and binary operators, as well at the overloading of existing operators.
This sample adds a COBOL-like INSPECT "statement" by defining suitable operators.


```algol68
# This example uses ALGOL 68 user defined operators to add a COBOL-style     #
# "INSPECT statement" to ALGOL 68                                            #
#                                                                            #
# The (partial) syntax of the COBOL INSPECT is:                              #
#    INSPECT string-variable REPLACING ALL     string BY string              #
# or INSPECT string-variable REPLACING LEADING string BY string              #
# or INSPECT string-variable REPLACING FIRST   string BY string              #
#                                                                            #
# Because "BY" is a reserved bold word in ALGOL 68, we use "WITH" instead    #
#                                                                            #
# We define unary  operators INSPECT, ALL, LEADING and FIRST                 #
#       and binary operators REPLACING and WITH                              #
# We choose the priorities of REPLACING and WITH so that parenthesis is not  #
# needed to ensure the correct interpretation of the "statement"             #
#                                                                            #
# We also provide a unary DISPLAY operator for a partial COBOL DISPLAY       #
# statement                                                                  #

# INSPECTEE is returned by the INSPECT unary operator                        #
MODE INSPECTEE   = STRUCT( REF STRING item, INT option );

# INSPECTTOREPLACE is returned by the binary REPLACING operator              #
MODE INSPECTTOREPLACE
                 = STRUCT( REF STRING item, INT option, STRING to replace );
# REPLACEMENT is returned by the unary ALL, LEADING and FIRST operators      #
MODE REPLACEMENT = STRUCT( INT option, STRING replace );

# REPLACING option codes, these are the option values for a REPLACEMENT      #
INT  replace all     = 1;
INT  replace leading = 2;
INT  replace first   = 3;

OP   INSPECT   = ( REF STRING s )INSPECTEE: ( s, 0 );
OP   ALL       = ( STRING replace )REPLACEMENT: ( replace all,     replace );
OP   LEADING   = ( STRING replace )REPLACEMENT: ( replace leading, replace );
OP   FIRST     = ( STRING replace )REPLACEMENT: ( replace first,   replace );
OP   ALL       = ( CHAR   replace )REPLACEMENT: ( replace all,     replace );
OP   LEADING   = ( CHAR   replace )REPLACEMENT: ( replace leading, replace );
OP   FIRST     = ( CHAR   replace )REPLACEMENT: ( replace first,   replace );

OP   REPLACING = ( INSPECTEE inspected, REPLACEMENT replace )INSPECTTOREPLACE:
                     ( item    OF inspected
                     , option  OF replace
                     , replace OF replace
                     );

OP   WITH      = ( INSPECTTOREPLACE inspected, CHAR   replace with )REF STRING:
    BEGIN
        STRING with := replace with;
        inspected WITH with
    END; # WITH #

OP   WITH      = ( INSPECTTOREPLACE inspected, STRING replace with )REF STRING:
    BEGIN

        STRING    to replace  = to replace OF inspected;
        INT       pos        := 0;
        STRING    rest       := item OF inspected;
        STRING    result     := "";

        IF   option OF inspected = replace all
        THEN
            # replace all occurances of "to replace" with "replace with"     #
            WHILE string in string( to replace, pos, rest )
            DO
                result +:= rest[ 1 : pos - 1 ] + replace with;
                rest    := rest[ pos + UPB to replace : ]
            OD

        ELIF option OF inspected = replace leading
        THEN
            # replace leading occurances of "to replace" with "replace with" #
            WHILE IF string in string( to replace, pos, rest )
                  THEN
                      pos = 1
                  ELSE
                      FALSE
                  FI
            DO
                result +:= replace with;
                rest    := rest[ 1 + UPB to replace : ]
            OD

        ELIF option OF inspected = replace first
        THEN
            # replace first occurance of "to replace" with "replace with"    #
            IF string in string( to replace, pos, rest )
            THEN
                result +:= rest[ 1 : pos - 1 ] + replace with;
                rest    := rest[ pos + UPB to replace : ]
            FI

        ELSE
            # unsupported replace option #
            write( ( newline, "*** unsupported INSPECT REPLACING...", newline ) );
            stop
        FI;

        result +:= rest;
        item OF inspected := result
    END; # WITH #

OP   DISPLAY = ( STRING s )VOID: write( ( s, newline ) );


PRIO REPLACING = 2, WITH = 1;




main: (

    # test the INSPECT and DISPLAY "verbs" #

    STRING  text := "some text";
    DISPLAY text;

    INSPECT text REPLACING FIRST   "e"    WITH "bbc";
    DISPLAY text;

    INSPECT text REPLACING ALL     "b"    WITH "X";
    DISPLAY text;

    INSPECT text REPLACING ALL     "text" WITH "some";
    DISPLAY text;

    INSPECT text REPLACING LEADING "som"  WITH "k";
    DISPLAY text


)
```

Output:

```txt
some text
sombbc text
somXXc text
somXXc some
kXXc some

```



## C


C preprocessor can be used to extend language to some extent. 

It's possible to create [http://stackoverflow.com/questions/3385515/static-assert-in-c static assertions]


```c

// http://stackoverflow.com/questions/3385515/static-assert-in-c
#define STATIC_ASSERT(COND,MSG) typedef char static_assertion_##MSG[(!!(COND))*2-1]
// token pasting madness:
#define COMPILE_TIME_ASSERT3(X,L) STATIC_ASSERT(X,static_assertion_at_line_##L)
#define COMPILE_TIME_ASSERT2(X,L) COMPILE_TIME_ASSERT3(X,L)
#define COMPILE_TIME_ASSERT(X)    COMPILE_TIME_ASSERT2(X,__LINE__)

COMPILE_TIME_ASSERT(sizeof(long)==8); 
int main()
{
    COMPILE_TIME_ASSERT(sizeof(int)==4); 
}

```


Another common usage is to create custom loops


```c

//requires C99
#define ITERATE_LIST(n, list) \
 for(Node *n = (list)->head; n; n = n->next)

...
ITERATE_LIST(n, list)
{
    printf("node value: %s\n", n->value);
}

```


For examples in real world, look [http://svn.gna.org/viewcvs/freeciv/trunk/common/city.h?view=markup FreeCiv], and [http://mz.openttdcoop.org/is2/openttd-is2-h1f270887-docs/engine__base_8h-source.html OpenTTD] macros(city_map_iterate for FreeCiv, FOR_ALL_ENGINES for OpenTTD).

Also, C does not support functions overloading, but becaus macro calls do not require type it's possible to emulate overloading to some extent


```c

#define my_min(x, y) ((x) < (y) ? (x) : (y))
...
printf("%f %d %ll\n", my_min(0.0f, 1.0f), my_min(1,2), my_min(1ll, 2ll));

```


The [[Order|Order programming language]] is implemented entirely using the C preprocessor, providing a portable, high-level, functional programming language that can be used to metaprogram any C99 project, in a fashion loosely similar to Lisp's macro system.


## C#


Metaprogramming in C# can be achieved using the [https://msdn.microsoft.com/en-us/library/bb126445.aspx Text Template Transformation Toolkit]. It is a textual preprocessor embedded in Visual Studio (it can also be executed from the command-line, e.g. in build scripts). It is language-agnostic, and therefore can generate code for C#, Visual Basic or other languages. This also means that it has no features which help manipulating the underlying language: it is purely textual, and does '''not''' include a C# parser to transform existing C# files (so you will need to roll your own or use [https://roslyn.codeplex.com/ Roslyn]), and does '''not''' include utilities which would help with combining pieces of code.


## Clojure

See [http://clojure-doc.org/articles/language/macros.html Clojure Macros and Metaprogramming] article.


## Common Lisp


===Built-In Fruits of Metaprogramming===

Common Lisp is based on decades of metaprogramming, so programmers don't have to roll their own to benefit from it.
For instance, the LOOP syntax is just a macro. Prior to becoming a standard language feature, it was just a library that users shared. The object system originated in the same way.

Calculate mean, and sample variance and sample standard deviation of some numbers:


```lisp

(loop for count from 1
      for x in '(1 2 3 4 5)
      summing x into sum
      summing (* x x) into sum-of-squares
      finally
        (return
          (let* ((mean (/ sum count))
                 (spl-var (- (* count sum-of-squares) (* sum sum)))
                 (spl-dev (sqrt (/ spl-var (1- count))))) 
            (values mean spl-var spl-dev)))) 
```


=> 
```txt
5/2 ;
105 ;
4.582576
```


Being a macro, if LOOP were removed from Lisp, it could be supplied by the application program.
In fact, sometimes programs have included their own LOOP to work around bugs in some implementations.

Here is what CLISP makes of the above, by investigating the macro expansion using the ANSI standard <code>macroexpand</code> function:


```lisp

[5]>    
  (macroexpand'
     (loop for count from 1
           for x in '(1 2 3 4 5)
           summing x into sum
           summing (* x x) into sum-of-squares
           finally
             (return
               (let* ((mean (/ sum count))
                      (spl-var (- (* count sum-of-squares) (* sum sum)))
                      (spl-dev (sqrt (/ spl-var (1- count)))))
                 (values mean spl-var spl-dev)))))
(MACROLET ((LOOP-FINISH NIL (SYSTEM::LOOP-FINISH-ERROR)))
 (BLOCK NIL
  (LET ((COUNT 1))
   (LET ((#:LIST-3047 '(1 2 3 4 5)))
    (PROGN
     (LET ((X NIL))
      (LET ((SUM-OF-SQUARES 0) (SUM 0))
       (MACROLET ((LOOP-FINISH NIL '(GO SYSTEM::END-LOOP)))
        (TAGBODY SYSTEM::BEGIN-LOOP (WHEN (ENDP #:LIST-3047) (LOOP-FINISH))
         (SETQ X (CAR #:LIST-3047))
         (PROGN (SETQ SUM (+ SUM X))
          (SETQ SUM-OF-SQUARES (+ SUM-OF-SQUARES (* X X))))
         (PSETQ COUNT (+ COUNT 1)) (PSETQ #:LIST-3047 (CDR #:LIST-3047))
         (GO SYSTEM::BEGIN-LOOP) SYSTEM::END-LOOP
         (MACROLET
          ((LOOP-FINISH NIL (SYSTEM::LOOP-FINISH-WARN) '(GO SYSTEM::END-LOOP)))
          (PROGN
           (RETURN
            (LET*
             ((MEAN (/ SUM COUNT))
              (SPL-VAR (- (* COUNT SUM-OF-SQUARES) (* SUM SUM)))
              (SPL-DEV (SQRT (/ SPL-VAR (1- COUNT)))))
             (VALUES MEAN SPL-VAR SPL-DEV)))))))))))))) ; T
```


Next, we can leave ANSI behind and call CLISP's internal code walker to expand the entire form, removing all traces of the definitions of local macros, leaving behind only pure code based on special forms and function calls:


```lisp
(system::expand-form 
    '(loop for count from 1
           for x in '(1 2 3 4 5)
           summing x into sum
           summing (* x x) into sum-of-squares
           finally                            
             (return
               (let* ((mean (/ sum count))
                      (spl-var (- (* count sum-of-squares) (* sum sum)))
                      (spl-dev (sqrt (/ spl-var (1- count)))))
                 (values mean spl-var spl-dev))))))
(BLOCK NIL
 (LET ((COUNT 1))
  (LET ((#:LIST-3230 '(1 2 3 4 5)))
   (LET ((X NIL))
    (LET ((SUM-OF-SQUARES 0) (SUM 0))
     (TAGBODY SYSTEM::BEGIN-LOOP
      (WHEN (ENDP #:LIST-3230) (GO SYSTEM::END-LOOP))
      (SETQ X (CAR #:LIST-3230))
      (PROGN (SETQ SUM (+ SUM X))
       (SETQ SUM-OF-SQUARES (+ SUM-OF-SQUARES (* X X))))
      (PSETQ COUNT (+ COUNT 1)) (PSETQ #:LIST-3230 (CDR #:LIST-3230))
      (GO SYSTEM::BEGIN-LOOP) SYSTEM::END-LOOP
      (RETURN-FROM NIL
       (LET*
        ((MEAN (/ SUM COUNT))
         (SPL-VAR (- (* COUNT SUM-OF-SQUARES) (* SUM SUM)))
         (SPL-DEV (SQRT (/ SPL-VAR (1- COUNT)))))
        (VALUES MEAN SPL-VAR SPL-DEV)))))))))
```



### Implement monadic comprehensions


We can use Lisp macros, and other features, to add support to Lisp for monads, which come from functional languages.  The following module of code provides a new macro form called COMPREHEND which works with monads. If we use the LIST monad, we get list comprehensions. For instance:


```lisp

;; The -> notation is not part of Lisp, it is used in examples indicate the output of a form.
;;
;;
(comprehend 'list-monad (cons x y) (x '(1 2 3)) (y '(A B C)))

     -> ((1 . A) (1 . B) (1 . C) 
         (2 . A) (2 . B) (2 . C) 
         (3 . A) (3 . B) (3 . C))
```


As you can see, the comprehension processes all combinations of X and Y from both sets, and collects the application of (CONS X Y) to these elements. 

In other words {&forall;x&forall;y:(cons x y) | x &isin; { 1, 2 ,3 } &and; y &isin; { A, B, C }}

Other monads are possible: idenitity, state transfomer, etc. Some of these are provided in the code below.

Furthermore, a form called DEFINE-MONAD is provided to define new kinds of monads. It is used to define the basic monads. DEFINE-MONAD also optionally generates a (trivial) short-hand comprehension macro for your monad type. So instead of (comprehend 'list ...) it is possible to write is also (list-comp ...).

Note how the state transformer monad uses the identity monad comprehension in its definition.

Also, a monad is a class, and there is a way in the DEFINE-MONAD syntax to declare what the base classes are (multiple inheritance) as well as any additional custom slots.

Another example, using the identity monad. With the identity monad, the comprehension becomes a sequence of successive variable bindings, and a form evaluated in the scope of those bindings. It is basically like a "Lispified" Haskell DO syntax:

```lisp
(identity-comp (list x y z) (x 1) (y (* 3 x)) (z (+ x y)))
->  (1 3 4)

```

I.e. combine the values X, Y and Z into a triplet list, were X is 1, Y is 3X, and Z is X + Y. 

To see the original version of this code with lengthy comments, have a look in the Lisp Pastebin. http://paste.lisp.org/display/71196


```lisp
(defgeneric monadic-map (monad-class function))

(defgeneric monadic-join (monad-class container-of-containers &rest additional))

(defgeneric monadic-instance (monad-class-name))

(defmacro comprehend (monad-instance expr &rest clauses)
  (let ((monad-var (gensym "CLASS-")))
    (cond
      ((null clauses) `(multiple-value-call #'monadic-unit 
                         ,monad-instance ,expr))
      ((rest clauses) `(let ((,monad-var ,monad-instance))
                         (multiple-value-call #'monadic-join ,monad-var 
                           (comprehend ,monad-var
                             (comprehend ,monad-var ,expr ,@(rest clauses))
                             ,(first clauses)))))
      (t (destructuring-bind (var &rest container-exprs) (first clauses)
           (cond
             ((and var (symbolp var))
              `(funcall (monadic-map ,monad-instance (lambda (,var) ,expr)) 
                        ,(first container-exprs)))
             ((and (consp var) (every #'symbolp var))
              `(multiple-value-call (monadic-map ,monad-instance 
                                                 (lambda (,@var) ,expr)) 
                                     ,@container-exprs))
             (t (error "COMPREHEND: bad variable specification: ~s" vars))))))))

(defmacro define-monad (class-name 
                        &key comprehension
                             (monad-param (gensym "MONAD-"))
                             bases slots initargs
                              ((:map ((map-param) 
                                      &body map-body)))
                              ((:join ((join-param 
                                        &optional 
                                          (j-rest-kw '&rest)
                                          (j-rest (gensym "JOIN-REST-")))
                                        &body join-body)))
                              ((:unit ((unit-param 
                                        &optional 
                                          (u-rest-kw '&rest)
                                          (u-rest (gensym "UNIT-REST-")))
                                       &body unit-body))))
  `(progn
     (defclass ,class-name ,bases ,slots)
     (defmethod monadic-instance ((monad (eql ',class-name)))
       (load-time-value (make-instance ',class-name ,@initargs)))
     (defmethod monadic-map ((,monad-param ,class-name) ,map-param)
       (declare (ignorable ,monad-param))
       ,@map-body)
     (defmethod monadic-join ((,monad-param ,class-name) 
                              ,join-param &rest ,j-rest)
       (declare (ignorable ,monad-param ,j-rest))
       ,@join-body)
     (defmethod monadic-unit ((,monad-param ,class-name)
                              ,unit-param &rest ,u-rest)
       (declare (ignorable ,monad-param ,u-rest))
       ,@unit-body)
     ,@(if comprehension
         `((defmacro ,comprehension (expr &rest clauses)
             `(comprehend (monadic-instance ',',class-name) 
                          ,expr  ,@clauses))))))

(defmethod monadic-map ((monad symbol) function)
  (monadic-map (monadic-instance monad) function))

(defmethod monadic-join ((monad symbol) container-of-containers &rest rest)
  (apply #'monadic-join (monadic-instance monad) container-of-containers rest))

(defmethod monadic-unit ((monad symbol) element &rest rest)
  (apply #'monadic-unit (monadic-instance monad) element rest))

(define-monad list-monad
  :comprehension list-comp
  :map ((function) (lambda (container) (mapcar function container)))
  :join ((list-of-lists) (reduce #'append list-of-lists))
  :unit ((element) (list element)))

(define-monad identity-monad
  :comprehension identity-comp
  :map ((f) f)
  :join ((x &rest rest) (apply #'values x rest))
  :unit ((x &rest rest) (apply #'values x rest)))

(define-monad state-xform-monad
  :comprehension state-xform-comp
  :map ((f) 
          (lambda (xformer) 
            (lambda (s)
               (identity-comp (values (funcall f x) new-state) 
                              ((x new-state) (funcall xformer s))))))
  :join ((nested-xformer)
           (lambda (s)
             (identity-comp (values x new-state)
                            ((embedded-xformer intermediate-state) 
                             (funcall nested-xformer s))
                            ((x new-state) 
                             (funcall embedded-xformer intermediate-state)))))
  :unit ((x) (lambda (s) (values x s))))
```


### Python in Lisp

The CLPython project (http://common-lisp.net/project/clpython) provides a Python implementation embedded in Common Lisp. Python modules can be included in Lisp programs and interoperate with Lisp code. There is even a mixed-mode interactive loop ("REPL") where one can use a dialect which mixes Python and Lisp:

From the project documentation:

<blockquote>
CLPython is able to turn a regular Lisp listener (REPL) into a "mixed-mode" listener that supports both Lisp and Python source as input:

```txt
clpython(213): (clpython:enter-mixed-lisp-python-syntax)
; The mixed Lisp/Python syntax mode is now enabled;
; Lispy *readtable* is now set.
clpython(214): print 123 * 2
246
clpython(215): range(100)[98:2:-2]
#(98 96 94 92 90 88 86 84 82 80 ...)
clpython(216): (+ 1 2)
3
```


It supports multi-line Python statements as long as the next lines are properly indented:


```txt
clpython(70): for i in range(4):
  print i,
  print i*2
0 0
1 2
2 4
3 6
```

</blockquote>

Unfortunately, further metaprogramming within the Python is evidently discouraged (see [[#Python|Python]] section below).


## D


[http://dlang.org/mixin.html Mixins] enable string constants to be compiled as regular D code and inserted into the program. Combining this with compile time manipulation of strings enables the creation of domain-specific languages.


```d
enum GenStruct(string name, string fieldName) =
    "struct " ~ name ~ "{ int " ~ fieldName ~ "; }";

// Equivalent to:  struct Foo { int bar; }
mixin(GenStruct!("Foo", "bar"));

void main() {
    Foo f;
    f.bar = 10;
}
```



## E


Forms of metaprogramming existant in E:

* ''Quasi-literals'' provide convenient notation for data structures and values for which there is not literal syntax, as discussed in [[String#E]].
* E program fragments may be quoted, manipulated as an AST, and evaluated, similarly to Lisp; lexical environments are first-class objects (though static with respect to the evaluated code). Demonstrated in [[Runtime evaluation#E]] and [[Eval in environment#E]].
* Control structures may be defined, as demonstrated in [[Extend your language#E]].


## Erlang


Metaprogramming, as understood for this task, is done with parse transformations in Erlang. This is what the documentation says: "Programmers are strongly advised not to engage in parse transformations".


## Forth

Perhaps the most obvious use of Metaprogramming in Forth is Forth itself. The Forth virtual machine traditionally has a primitive operation called BRANCH, which as you could guess, does an unconditional branch to somewhere in the program. There is also ?BRANCH (sometimes called 0BRANCH) which branches only if the top of the Forth DATA stack is zero.  There are typically also primitive "DO" and "LOOP" operators.  All of these primitives cannot be used on their own and must be compiled into program code by the Forth compiler which needs to compute where to branch or loop to in the context of the code.  The compiler itself is implemented in Forth and defines the syntax for conditional branches and loops. Understanding this code is not "needed" for application programming but exists inside the Forth system.  The application programmer is free to use these same tools to create new branching and looping syntax if there is a need to do so. (FOR/NEXT, CASE ENDCASE etc.)

<LANG>\ BRANCH and LOOP COMPILERS

\ branch offset computation operators
: AHEAD    ( -- addr)  HERE   0 , ;
: BACK     ( addr -- ) HERE   - , ;
: RESOLVE  ( addr -- ) HERE OVER - SWAP ! ;

\ LEAVE stack is called L0. It is initialized by QUIT.
: >L        ( x -- ) ( L: -- x )  2 LP +!  LP @ ! ;
: L>        ( -- x )  ( L: x -- ) LP @ @   -2 LP +! ;

\ finite loop compilers
: DO        ( -- ) POSTPONE <DO>     HERE 0 >L   3 ;  IMMEDIATE
: ?DO       ( -- ) POSTPONE <?DO>    HERE 0 >L   3 ;  IMMEDIATE
: LEAVE     ( -- ) ( L: -- addr )
            POSTPONE UNLOOP   POSTPONE BRANCH AHEAD >L ; IMMEDIATE

: RAKE      ( -- ) ( L: 0 a1 a2 .. aN -- )
            BEGIN  L> ?DUP WHILE  RESOLVE  REPEAT ;   IMMEDIATE

: LOOP      ( -- )  3 ?PAIRS POSTPONE <LOOP>  BACK  RAKE ; IMMEDIATE
: +LOOP     ( -- )  3 ?PAIRS POSTPONE <+LOOP> BACK  RAKE ; IMMEDIATE

\ conditional branches
: IF          ( ? -- ) POSTPONE ?BRANCH AHEAD 2 ;        IMMEDIATE
: THEN        ( -- )  ?COMP  2 ?PAIRS RESOLVE ;          IMMEDIATE
: ELSE        ( -- )  2 ?PAIRS  POSTPONE BRANCH AHEAD SWAP 2
                      POSTPONE THEN 2 ;                  IMMEDIATE

\ infinite loop compilers
: BEGIN       ( -- addr n) ?COMP HERE  1  ;              IMMEDIATE
: AGAIN       ( -- )   1 ?PAIRS POSTPONE BRANCH BACK   ;  IMMEDIATE
: UNTIL       ( ? -- ) 1 ?PAIRS POSTPONE ?BRANCH BACK  ;  IMMEDIATE
: WHILE       ( ? -- ) POSTPONE IF  2+  ;                IMMEDIATE
: REPEAT      ( -- )   2>R POSTPONE AGAIN  2R> 2- POSTPONE THEN ; IMMEDIATE</LANG>

Simple Usage Examples

<LANG> : CHARSET    [CHAR] ~  [CHAR] ! DO  I EMIT LOOP ;

: >DIGIT ( n -- c) DUP 9 > IF  7 +  THEN [CHAR] 0 + ;  

: -TRAILING  ( adr len -- adr len')  \ remove trailing blanks (spaces)
             BEGIN  
                2DUP + 1- C@ BL =    \ test if last char = blank
             WHILE  
                1-                   \ dec. length  
             REPEAT ;</LANG>


## FreeBASIC

Single line and multiple line macros can be used to modify or extend the language's syntax and are about as powerful as those found in the C language.

For example, we can create a 'forall' loop to iterate through the characters of a string rather than use a traditional 'for' loop:


```freebasic
' FB 1.05.0 Win64

#Macro ForAll(C, S)
For _i as integer = 0 To Len(s) 
#Define C (Chr(s[_i]))
#EndMacro
 
#Define In ,
 
Dim s As String = "Rosetta"
ForAll(c in s)
  Print c; " ";
Next
 
Print
Sleep
```


{{out}}

```txt

R o s e t t a 

```



## Go

Although Go has a relatively small number of keywords (25), it also has 39 predeclared identifiers many of which would be considered to be keywords in other languages. The latter include:

1. The names of the basic types such as int, float64, bool and string. 

2. Constants such as true, false and nil.

3. Functions such as append, copy, len, make, new and panic.

The predeclared identifiers have 'universal scope' and can therefore be redeclared to mean something different in all other scopes. To the extent that this can be considered to modify the normal operation of the language, Go has "support for metaprogramming" within the meaning of this task but has no other facilities of this nature.

Needless to say, redeclaring the predeclared identifiers in this way is potentially confusing and should not therefore be done without good reason.

In the following example the 'copy' function, which is normally used to copy slices, is redeclared to copy a 'person' struct instead:

```go
package main

import "fmt"

type person struct{
    name string
    age int
}

func copy(p person) person {
    return person{p.name, p.age}
}

func main() {
    p := person{"Dave", 40}
    fmt.Println(p)
    q := copy(p)
    fmt.Println(q)
    /*
    is := []int{1, 2, 3}
    it := make([]int, 3)
    copy(it, is)
    */ 
}
```


{{out}}

```txt

{Dave 40}
{Dave 40}

```


If the commented-out code, which uses 'copy' in its normal sense, were uncommented then the program would fail to compile with the following message:

```txt

./metaprog.go:22:9: too many arguments in call to copy
	have ([]int, []int)
	want (person)

```



## Haskell


Metaprogramming is implemented using [http://www.haskell.org/haskellwiki/Template_Haskell Template Haskell].


## J


J names have one of four different grammatic types:  noun, verb, adverb, conjunction.  Nouns and verbs are nothing special from a metaprogramming point of view.  However, adverbs and conjunctions are evaluated at "parse time" and can be used to introduce expression variants.  (The result of an adverb, or of a conjunction may be either a noun, a verb, an adverb or a conjunction.)

Additionally, J script blocks (a block of text terminated by a right parenthesis on a single line) can and are used with differing interpreters (which may be built into the language or user written).

The [[Y_combinator#J|J implementation of the Y combinator]] could be considered an example of metaprogramming.

J's [http://www.jsoftware.com/help/dictionary/d310n.htm explicit definitions] might also be considered an example of metaprogramming, since explicit definitions have data dependent syntactic types.



## Julia

Julia's metaprogramming features are descibed in the online documentation at 
https://docs.julialang.org/en/v1/manual/metaprogramming/index.html

Here is an example of metaprogramming. Julia in base form does not have C's do { } while() statement. 

Using metaprogramming, the do/while can be somewhat emulated:

```julia
macro dowhile(condition, block)
    quote
        while true
            $(esc(block))
            if !$(esc(condition))
                break
            end
        end
    end
end 

macro dountil(condition, block)
    quote
        while true
            $(esc(block))
            if $(esc(condition))
                break
            end
        end
    end
end 

using Primes

arr = [7, 31]

@dowhile (!isprime(arr[1]) && !isprime(arr[2])) begin
    println(arr)
    arr .+= 1
end
println("Done.")

@dountil (isprime(arr[1]) || isprime(arr[2])) begin
    println(arr)
    arr .+= 1
end
println("Done.")

```
{{output}}
```txt

[7, 31]
[8, 32]
[9, 33]
[10, 34]
Done.
[11, 35]
[12, 36]
Done.

```



## Kotlin

Although Kotlin doesn't support 'true' meta-programming, it does have facilities which make it possible to create something which closely resembles a language extension (see [[Extend_your_language#Kotlin]]).

It is also possible to define infix functions which look like user defined operators:


```scala
// version 1.0.6

infix fun Double.pwr(exp: Double) = Math.pow(this, exp)

fun main(args: Array<String>) {
   val d = 2.0 pwr 8.0
   println(d)
}
```


{{out}}

```txt

256.0

```



## Lingo

Lingo allows to create (and pre-compile) arbitrary code at runtime. You can't really change the language's syntax, but you can overwrite (or extend) built-in commands. Here as example some code that overwrite's Lingo's halt command, which would normally exit the current program:


```lingo
r = RETURN
str = "on halt"&r&"--do nothing"&r&"end"
new(#script).scripttext = str
```



## Lua


Due to the way Lua's syntactic sugar is designed, metatables can make some Lua code look like a Domain-Specific Language, despite technically being (mostly) just specialized operator overloading.

For example:

```lua

class "foo" : inherits "bar"
{
  
} 

```


is perfectly valid syntax. (Lua does not having a built in concept of classes or inheritance.)

## M2000 Interpreter


```M2000 Interpreter

Module Meta {
      FunName$="Alfa"
      Code1$=FunName$+"=lambda (X)->{"
      Code2$={
            =x**2
      }
      Code3$="}"
      
      Inline code1$+code2$+code3$
      
      Print Function(FunName$, 4)=16
      
}
Meta

```



## Mathematica

Mathematica can overload all symbols, though sometimes Unprotect has to be invoked. You can also introduce your own infix operators:

```Mathematica
CircleTimes[x_, y_] := Mod[x, 10] Mod[y, 10]
14\[CircleTimes]13
```

{{out}}

```txt
12
```

For more info see: [http://reference.wolfram.com/mathematica/tutorial/OperatorsWithoutBuiltInMeanings.html Operators in Mathematica]


## Nemerle

Nemerle provides support for macros, which range from defining new infix operators (in fact many 'built-in' operators are [http://nemerle.org/wiki/index.php?title=Macro_operators macros]) to new keywords or control structures.

See [http://nemerle.org/wiki/index.php?title=Macros here], [http://nemerle.org/wiki/index.php?title=Macros_tutorial here], and [http://nemerle.org/wiki/index.php?title=Category:Macro_packages here] on the Nemerle wiki for more information.


## Nim


### Infix Operators

You can define your own infix operators:

```nim
proc `^`*[T: SomeInteger](base, exp: T): T =
  var (base, exp) = (base, exp)
  result = 1
 
  while exp != 0:
    if (exp and 1) != 0:
      result *= base
    exp = exp shr 1
    base *= base
 
echo 2 ^ 10 # 1024
```



### Compile Time evaluation

<code>when</code> is a compile time <code>if</code> and can be used to prevent code from even being parsed at compile time, for example to write platform specific code:

```nim
when defined windows:
  echo "Call some Windows specific functions here"
elif defined linux:
  echo "Call some Linux specific functions here"
else:
  echo "Code for the other platforms"
```

Normal code can be executed at compile time if it is in a <code>static</code> block:

```nim
static:
  echo "Hello Compile time world: ", 2 ^ 10
```

As well as stored in compile time constants:

```nim
const x = 2 ^ 10
```


### Templates

The <code>expensive</code> procedure has to be evaluated even when <code>debug</code> is <code>false</code>:

```nim
import os

const debug = false

proc expensive: string =
  sleep(milsecs = 100)
  result = "That was difficult"

proc log(msg: string) =
  if debug:
    echo msg

for i in 1..10:
  log expensive()

```

This can be prevented using templates, as template calls are replaced with the template body at compile time:
<lang>template log(msg: string) =
  if debug:
    echo msg

for i in 1..10:
  log expensive()
```

Templates can use block syntax with statement parameters:

```nim
template times(x, y: untyped): untyped =
  for i in 1..x:
    y

10.times: # or times 10:  or times(10):
  echo "hi"
  echo "bye"
```



### Term Rewriting Templates

Term Rewriting Templates can be used to write your own optimizations:
<lang>template optLog1{a and a}(a): auto = a
template optLog2{a and (b or (not b))}(a,b): auto = a
template optLog3{a and not a}(a: int): auto = 0

var
  x = 12
  s = x and x
  # Hint: optLog1(x) --> ’x’ [Pattern]

  r = (x and x) and ((s or s) or (not (s or s)))
  # Hint: optLog2(x and x, s or s) --> ’x and x’ [Pattern]
  # Hint: optLog1(x) --> ’x’ [Pattern]

  q = (s and not x) and not (s and not x)
  # Hint: optLog3(s and not x) --> ’0’ [Pattern]
```


### Macros

The most powerful metaprogramming capabilities are offered by macros. They can generate source code or even an AST directly.

<code>dumpTree</code> can be useful when creating an AST, as it show you the AST of any code:

```nim
import macros

dumpTree:
  if x:
    if y:
      p0
    else:
      p1
  else:
    if y:
      p2
    else:
      p3
```

This prints:

```txt
StmtList
  IfStmt
    ElifBranch
      Ident !"x"
      StmtList
        IfStmt
          ElifBranch
            Ident !"y"
            StmtList
              Ident !"p0"
          Else
            StmtList
              Ident !"p1"
    Else
      StmtList
        IfStmt
          ElifBranch
            Ident !"y"
            StmtList
              Ident !"p2"
          Else
            StmtList
              Ident !"p3"
```

Using this information we can create an <code>if2</code> macro for two conditions, as is done in the [http://rosettacode.org/wiki/Extend_your_language#Nim "Extend your language" task].


## PARI/GP

The primary means of metaprogramming for GP is to extend it with PARI.  As an example, defining an infix <code>@</code> operator could work like

In src/functions/symbolic_operators/min0:

```txt
Function: _@_
Help: x@y: compute the lesser of x and y, or 0, whichever is larger.
Section: symbolic_operators
C-Name: gmin0
Prototype: GG
Description:
 (small, small):small	 smin0ss($1, $2)
 (mp, mp):mp            gmin0($1, $2)
 (gen, gen):gen     	 gmin0($1, $2)
```


In (e.g.) basemath/somefile.c:

```C
long
smin0ss(long a, long b)
{
  long c = a < b ? a : b;
  return c > 0 ? c : 0;
}


GEN
gmin0(GEN a, GEN b)
{
  GEN c = gcmp(a, b) < 1 ? a : b; /* copy pointer */
  return signe(c) > 0 ? gcopy(c) : gen_0;
}
```




## Perl


You can textually transform code with a [http://perldoc.perl.org/perlfilter.html source filter], a module that when <code>use</code>d modifies the following lines of source. [http://perldoc.perl.org/Filter/Util/Call.html Filter::Util::Call] provides a general means of writing source filters. [http://perldoc.perl.org/Filter/Simple.html Filter::Simple] is an interface to <code>Filter::Util::Call</code> that lets you elide a lot of boilerplate code. More important, <code>Filter::Simple</code> can hide the contents of quoting constructs from your filter, obviating the biggest dangers of textual metaprogramming. For example, given the following module:


```perl
package UnicodeEllipsis;

use Filter::Simple;

FILTER_ONLY code => sub { s/…/../g };
```


this program:


```perl
use UnicodeEllipsis;

print join(' … ', 1 … 5), "\n";
```


prints:

  1 … 2 … 3 … 4 … 5

[http://search.cpan.org/dist/Devel-Declare Devel::Declare] lets you define a new keyword by setting up a hook to be run whenever the parser encounters a bareword of your choice. Devel::Declare is powerful, but it has a reputation for being difficult to understand. See [http://transfixedbutnotdead.com/2009/12/16/url-develdeclare-and-no-strings-attached/ this blog post] for a relatively simple usage example.


## Perl 6


Perl 6 makes it very easy to do metaprogramming. It is a basic goal of the language.

It is trivial to add a new operator. Most Perl 6 operators are written as normal multiple-dispatch functions in the setting (known as a "prelude" in other languages, but in Perl 6 the setting is a lexical scope notionally surrounding your compilation unit).

There is no a built in factorial operator Perl 6. It was purposely left out to use as a demonstration of how easy it is to add it. <tt>:-)</tt>


```perl6
sub postfix:<!> { [*] 1..$^n }
say 5!;  # prints 120
```


You may augment a base class with a new method, as long as you declare that you are going to cheat.

Here we add a new method to do natural sorting to the base class <tt>Any</tt>. (<tt>List</tt> and <tt>Array</tt> are both subclasses of Any)


```perl6
use MONKEY-TYPING; # Needed to do runtime augmentation of a base class.

augment class List {
    method nsort { self.list.sort: {$^a.lc.subst(/(\d+)/, -> $/ {0 ~ $0.chars.chr ~ $0 }, :g) ~ "\x0" ~ $^a} }
};

say ~<a201st a32nd a3rd a144th a17th a2 a95>.nsort;
say ~<a201st a32nd a3rd a144th a17th a2 a95>.sort;
```


Prints

```txt

a2 a3rd a17th a32nd a95 a144th a201st
a144th a17th a2 a201st a32nd a3rd a95
```

Grammar mixins work in Perl 6 because grammar rules are just methods in grammar classes, and Perl 6 automatically writes a JIT lexer for you whenever you derive a new language.  This functionality already works internally in the standard parser—what is not yet implemented is the <tt>augment slang</tt> hook to allow user code to do this mixin.  Perl 6 itself is already parsed using such grammar mixins to provide extensible quoting and regex syntax.  For example, every time you pick your own quote characters, you're actually deriving a new Perl 6 dialect that supports those start and stop characters.  Likewise any switches to impose single or double-quote semantics, or heredoc semantics, is merely a grammar mixin on the basic <tt>Q</tt> language.

```perl6
say "Foo = $foo\n";  # normal double quotes
say Q:qq 【Foo = $foo\n】; # a more explicit derivation, new quotes
```



## Phix

Metaprogramming is frowned on in phix and generally considered a deliberate and unnecessary attempt to complicate matters.

Note however that I consider the necessity for metaprogramming in lisp-like languages to be a weakness, not a strength.

Some builtins can be overridden, but the language reserves the right to reject such wanton acts of intellectual terrorism.

One of the core principles of phix is that code should be utterly intuitive and easy to read.
=== compile-time assertions ===
The #isginfo{}, #isinit{}, and #istype{} directives instruct the compiler to perform various type-inference and legal value ranges checks. 
Primarily for compiler development use, rather than end user applications. No code is generated, but compilation will abort if they fail.
Some static assertions can be performed with #isginfo{}, eg:

```Phix
object x
#isginfo{x,0b0101,5,7,integer,3}
--    {var,type,min,max,etype,len}
    -- (0b0101 is dword sequence|integer)
    x = {1,2,3} -- sequence of integer, length 3
    x = 5       -- integer 5 (becomes min)
    x = 7       -- integer 7 (becomes max)
```

A compile-time error occurs if say either 7 is changed to 6 (but not both).

Note that you only get that error for "p -c test", not "p test".

###  symbol table hacking 

See builtins\VM\prtnidN.e for details of how to locate and process the symbol table. Again I would not recommend it,
but that would allow you to modify variables and invoke code fragments at will, in a metaprogrammy sort of way.

###  modifying the compiler 

see [[Extend_your_language#Phix]]


## PicoLisp

As in any Lisp, metaprogramming is an essential aspect of PicoLisp.
In most cases normal functions are used to extend the language
(see [[Extend your language#PicoLisp]]), 
[http://software-lab.de/doc/ref.html#macro-io read-macros] operate on
the source level, and also runtime 
[http://software-lab.de/doc/refM.html#macro macros] are used occasionally.


## PostScript

PostScript allows the reification of stack, scoping (dynamic scoping is default, but lexical scoping can be implemented using immediate loading), bindings using dictionaries, and even control flow. Here is an example of implementation of if statement
{{libheader|initlib}}

```postscript


/ift {
4 dict begin
    [/.if /.then] let*
    count array astore /.stack exch def
    /_restore {clear .stack aload pop}.
    .stack aload pop .if {
       _restore .then
    } {
       _restore
    } ifelse
end}.

```

The standard if expression in PostScript does not take a predicate. Instead it acts on the boolean value on top of the stack. This newly created word allows us to do

```postscript

>| 2 {1 gt} {=} ift
2

```

Instead of


```postscript

>| 2 dup 1 gt {=} ift
2

```


Note that even the let expression was implemented using meta programming

```postscript

/let* {reverse {exch def} forall}.

```



## Python


Metaprogramming is frowned on in Python and considered un-pythonic. The only widely known example of metaprogramming in Python was an implementation of a goto (and comefrom) keyword done as an [http://entrian.com/goto/ April-fools joke].

Another more recent library that shows it can be done in Python is [https://github.com/lihaoyi/macropy MacroPy].

==={{works with|https://github.com/lihaoyi/macropy MacroPy}}===

This is example is taken from MacroPy's [https://github.com/lihaoyi/macropy/blob/2885df8ca73fa0f6c17168a98d218dc4a3f088c2/docs/examples/first_macro/full/macro_module.py GitHub page]. It creates a macro called <tt>expand</tt> that, when invoked, generates the AST for a function in place of the original expression.


```python

from macropy.core.macros import *
from macropy.core.quotes import macros, q, ast, u

macros = Macros()

@macros.expr
def expand(tree, **kw):
    addition = 10
    return q[lambda x: x * ast[tree] + u[addition]]

```


It is then invoked like this:

```python

func = expand[1 + 2]
print func(5)

```



## Prolog


This example expands and prints a goal using [http://www.swi-prolog.org/pldoc/man?predicate=clause/2 clause/2]:


```prolog

:- initialization(main).
main :- clause(less_than(1,2),B),writeln(B).
less_than(A,B) :- A<B.


```

New goals can be created at runtime using [http://www.swi-prolog.org/pldoc/man?predicate=assertz/1 assertz/1]:

```prolog

assertz((mother(Child, Mother) :-
                parent(Child, Mother),
                female(Mother))).

```




## Racket


Racket has an extremely rich set of metaprogramming tools, which scale from simple pattern-based macros to implementing entire new languages with their own syntax, such as [http://docs.racket-lang.org/datalog/index.html Datalog] and [http://docs.racket-lang.org/algol60/index.html Algol 60]. Many parts of Racket itself, including its class-based object system, are implemented as macros that expand to a much smaller set of core forms.

As a descendent of the Scheme tradition, Racket provides [https://en.wikipedia.org/wiki/Hygienic_macro hygienic] pattern-based macros, allows the use of the full Racket language (including programmer-defined extensions) in implementing macros, and supports locally-defined macros.

Racket adds many extensions to this tradition, such as syntax-parse, which simplifies writing robust macros with good error reporting. For more information on Racket's metaprogramming features, see the relevant chapters of [https://docs.racket-lang.org/guide/macros.html The Racket Guide] and [https://docs.racket-lang.org/reference/Macros.html The Racket Reference].

For a simple example, this is the definition and a use of the macro list-when:

```racket
#lang racket

(define-syntax-rule (list-when test body)
  (if test
      body
      '()))

(let ([not-a-string 42])
  (list-when (string? not-a-string)
    (string->list not-a-string)))
```


Unlike a plain function, which would eagerly evaluate its arguments, list-when only evaluates its body expression when its test expression passes: otherwise, it evaluates to the empty list. Therefore, the example above does not produce an error.

Alternatively, list-when could be defined using syntax-parse, which provides better error messages for syntax errors:


```racket
(require (for-syntax syntax/parse))

(define-syntax list-when
  (syntax-parser
    [(_ test:expr body:expr)
     #'(if test
           body
           null)]))
```



## Rascal

Rascal has been developed for metaprogramming. Rascal modules already have functionality to analyse Java code (see [http://tutor.rascal-mpl.org/Courses/Rascal/Rascal.html#/Courses/Rascal/Libraries/lang/java/jdt/jdt.html documentation]).


### Syntax Definition


In Rascal, grammars can be easily defined. The example below shows the syntax definition for the easy languages C and E1. ViewParseTree visualises the parse tree and lets the user interactively check whether sentences belong to the grammar. The greater than symbol in language E1 means that multiplication has a higher priority than addition.


```rascal
extend ViewParseTree;

layout Whitespace = [\ \t\n]*;
syntax A = "a";
syntax B = "b";
start syntax C = "c" | A C B;

layout Whitespace = [\ \t\n]*;            
lexical Integer = [0-9]+;
start syntax E1 = Integer
               | E "*" E
	       > E "+" E
	       | "(" E ")"
               ;
```


An example of the parse viewer for E1

[[File:E1parseviewer.png]]


### Syntax Tree Traversal

Furthermore, Rascal has built-in functions to traverse trees. This can be used to visit all the nodes in the abstract syntax trees that are automatically generated by Rascal. This provides a powerful tool to analyse code. The following example counts for each operator how many of these the programme contains.

```rascal
map[str, int] operatorUsage(PROGRAM P) {
    m = ();
    visit(P){
    	case add(_,_): m["add"] ? 0 += 1;
    	case sub(_,_): m["sub"] ? 0 += 1;
    	case conc(_,_): m["conc"] ? 0 += 1;
    }
    return m;
}
```

Where the abstract syntax is defined as follows

```rascal
public data TYPE =
	  natural() | string();
	  
public alias PicoId = str;
	  
public data PROGRAM =
  program(list[DECL] decls, list[STATEMENT] stats);

public data DECL =
  decl(PicoId name, TYPE tp);

public data EXP = 
      id(PicoId name)
    | natCon(int iVal)
    | strCon(str sVal)
    | add(EXP left, EXP right)
    | sub(EXP left, EXP right)
    | conc(EXP left, EXP right)
    ;
    
public data STATEMENT =
  asgStat(PicoId name, EXP exp)
| ifElseStat(EXP exp, list[STATEMENT] thenpart, list[STATEMENT] elsepart)
| ifThenStat(EXP exp, list[STATEMENT] thenpart)
| whileStat(EXP exp, list[STATEMENT] body)
| doUntilStat(EXP exp, list[STATEMENT] body)
| unlessStat(EXP exp, list[STATEMENT] body)
;
```



### Pico in Rascal


This is part of the Pico syntax expressed in Rascal. 


```rascal
module lang::pico::Syntax

import Prelude;

lexical Id  = [a-z][a-z0-9]* !>> [a-z0-9];
lexical Natural = [0-9]+ ;
lexical String = "\"" ![\"]*  "\"";

layout Layout = WhitespaceAndComment* !>> [\ \t\n\r%];

lexical WhitespaceAndComment 
   = [\ \t\n\r]
   | @category="Comment" "%" ![%]+ "%"
   | @category="Comment" "%%" ![\n]* $
   ;

start syntax Program 
   = program: "begin" Declarations decls {Statement  ";"}* body "end" ;

syntax Declarations 
   = "declare" {Declaration ","}* decls ";" ;  
 
syntax Declaration = decl: Id id ":" Type tp;

syntax Type 
   = natural:"natural" 
   | string :"string" 
   ;

syntax Statement 
   = asgStat: Id var ":="  Expression val 
   | ifElseStat: "if" Expression cond "then" {Statement ";"}*  thenPart "else" {Statement ";"}* elsePart "fi"
   | ifThenStat: "if" Expression cond "then" {Statement ";"}*  thenPart "fi"
   | whileStat: "while" Expression cond "do" {Statement ";"}* body "od"
   | doUntilStat: "do" {Statement ";"}* body "until" Expression cond "od"
   | unlessStat: Statement "unless" Expression cond
  ;  
     
syntax Expression 
   = id: Id name
   | strCon: String string
   | natCon: Natural natcon
   | bracket "(" Expression e ")"
   > left conc: Expression lhs "||" Expression rhs
   > left ( add: Expression lhs "+" Expression rhs
          | sub: Expression lhs "-" Expression rhs
          )
  ;

public start[Program] program(str s) {
  return parse(#start[Program], s);
}

public start[Program] program(str s, loc l) {
  return parse(#start[Program], s, l);
} 
```



## REXX


```rexx
/*┌───────────────────────────────────────────────────────────────────┐
  │ The REXX language doesn't allow for the changing or overriding of │
  │ syntax per se,  but any of the built-in-functions (BIFs) can be   │
  │ overridden by just specifying your own.                           │
  │                                                                   │
  │ To use the REXX's version of a built-in-function, you simply just │
  │ enclose the BIF in quotation marks (and uppercase the name).      │
  │                                                                   │
  │ The intent is two-fold:  the REXX language doesn't have any       │
  │ reserved words,  nor reserved  BIFs  (Built-In-Functions).        │
  │                                                                   │
  │ So, if you don't know that  VERIFY  is a BIF,  you can just code  │
  │ a subroutine (or function)  with that name (or any name), and not │
  │ worry about your subroutine being pre-empted.                     │
  │                                                                   │
  │ Second:  if you're not satisfied how a BIF works, you can code    │
  │ your own.   This also allows you to front-end a BIF for debugging │
  │ or modifying the BIF's behavior.                                  │
  └───────────────────────────────────────────────────────────────────┘ */
yyy='123456789abcdefghi'

rrr =  substr(yyy,5)                   /*returns  efghi                 */
mmm = 'SUBSTR'(yyy,5)                  /*returns  56789abcdefgji        */
sss = "SUBSTR"(yyy,5)                  /* (same as above)               */
exit                                   /*stick a fork in it, we're done.*/

/*──────────────────────────────────SUBSTR subroutine───────────────────*/
substr: return right(arg(1),arg(2))

/*┌───────────────────────────────────────────────────────────────────┐
  │ Also, some REXX interpreters treat whitespace(s) as blanks   when │
  │ performing comparisons.    Some of the whitespace characters are: │
  │                                                                   │
  │           NL  (newLine)                                           │
  │           FF  (formFeed)                                          │
  │           VT  (vertical tab)                                      │
  │           HT  (horizontal tab or TAB)                             │
  │           LF  (lineFeed)                                          │
  │           CR  (carriage return)                                   │
  │           EOF (end-of-file)                                       │
  │         and/or others.                                            │
  │                                                                   │
  │ Note that some of the above are ASCII or EBCDIC specific.         │
  │                                                                   │
  │ Some REXX interpreters use the   OPTIONS   statement to force     │
  │ REXX to only treat blanks as spaces.                              │
  │                                                                   │
  │ (Both the  verb  and  option  may be in lower/upper/mixed case.)  │
  │                                                                   │
  │ REXX interpreters which don't recognize any  option  won't treat  │
  │ the (below) statement as an error.                                │
  └───────────────────────────────────────────────────────────────────┘ */
options  strict_white_space_comparisons   /*can be in lower/upper/mixed.*/
```



## Ring

The next program add new method to the object class during the runtime

```ring

o1 = new point { x=10 y=20 z=30 }
addmethod(o1,"print", func { see x + nl + y + nl + z + nl } )
o1.print()
Class point
        x y z

```


The next example presents how to create a class that defines two instructions
The first instruction is : I want window
The second instruction is : Window title = Expression
Also keywords that can be ignored like the ‘the’ keyword 


```ring

New App
{
        I want window
        The window title = "hello world"
}

Class App

        func geti
                if nIwantwindow = 0
                        nIwantwindow++
                ok

        func getwant
                if nIwantwindow = 1
                        nIwantwindow++
                ok

        func getwindow
                if nIwantwindow = 2
                        nIwantwindow= 0
                        see "Instruction : I want window" + nl
                ok
                if nWindowTitle = 0
                        nWindowTitle++
                ok

        func settitle cValue
                if nWindowTitle = 1
                        nWindowTitle=0
                        see "Instruction : Window Title = " + cValue + nl
                ok

        private

                # Attributes for the instruction I want window
                        i want window
                        nIwantwindow = 0
                # Attributes for the instruction Window title
                # Here we don't define the window attribute again
                        title
                        nWindowTitle = 0
                # Keywords to ignore, just give them any value
                        the=0

```





## Ruby

An rudimentary example of metaprogramming is presented in this simple identification system template:

```ruby
class IDVictim
  
  # Create elements of this man, woman, or child's identification.
  attr_accessor :name, :birthday, :gender, :hometown
  
  # Allows you to put in a space for anything which is not covered by the
  # preexisting elements.
  def self.new_element(element)
    attr_accessor element
  end
  
end
```


The "self.new_element" class method allows one to (later) specify a new attribute to be added to the defaults of "name", "birthday", "gender", and "hometown".



## Run BASIC

(This is not really metaprogramming, at least not under any useful meaning...)


```runbasic
' ---------------------------------------------------
' create a file to be run
' RB can run the entire program
'  or execute a function withing the RUNNED program
' ---------------------------------------------------
open "runned.bas" for output as #f                      ' open runned.bas as output

print #f, "text$ = ""I'm rinning the complete program.  ' print this program to the output
Or you can run a function.
The program or function within the RUNNED program
can execute all Run BASIC commands."""

print #f, "
x = displayText(text$)"

print #f, "                                            ' besides RUNNING the entireprogram
Function displayText(text$)                            ' we will execute this function only
print text$                                            '
end function"

' ----------------------------------------
' Execute the entire RUNNED program
' ----------------------------------------
RUN "runned.bas",#handle          ' setup run command to execute runned.bas and give it a handle
render #handle                    ' render the handle will execute the program

' ----------------------------------------
' Execute a function in the RUNNED program
' ----------------------------------------
RUN "runned.bas",#handle          ' setup run command to execute runned.bas and give it a handle
#handle displayText("Welcome!")   ' only execute the function withing the runned program
render #handle                    ' render the handle will execute the program
```



## Rust

Rust supports extensive metaprogramming via macros. Note that rust macros differ from, say, C preprocessor macros in that they are not mere text substitution (so operator precedence is preserved and name shadowing is not an issue). Here is an example from rustbyexample.com that implements and tests the <code>+=</code>, <code>-=</code>, and <code>*=</code> operators for Vectors.


```rust
// dry.rs
use std::ops::{Add, Mul, Sub};

macro_rules! assert_equal_len {
    // The `tt` (token tree) designator is used for
    // operators and tokens.
    ($a:ident, $b: ident, $func:ident, $op:tt) => (
        assert!($a.len() == $b.len(),
                "{:?}: dimension mismatch: {:?} {:?} {:?}",
                stringify!($func),
                ($a.len(),),
                stringify!($op),
                ($b.len(),));
    )
}

macro_rules! op {
    ($func:ident, $bound:ident, $op:tt, $method:ident) => (
        fn $func<T: $bound<T, Output=T> + Copy>(xs: &mut Vec<T>, ys: &Vec<T>) {
            assert_equal_len!(xs, ys, $func, $op);

            for (x, y) in xs.iter_mut().zip(ys.iter()) {
                *x = $bound::$method(*x, *y);
                // *x = x.$method(*y);
            }
        }
    )
}

// Implement `add_assign`, `mul_assign`, and `sub_assign` functions.
op!(add_assign, Add, +=, add);
op!(mul_assign, Mul, *=, mul);
op!(sub_assign, Sub, -=, sub);

mod test {
    use std::iter;
    macro_rules! test {
        ($func: ident, $x:expr, $y:expr, $z:expr) => {
            #[test]
            fn $func() {
                for size in 0usize..10 {
                    let mut x: Vec<_> = iter::repeat($x).take(size).collect();
                    let y: Vec<_> = iter::repeat($y).take(size).collect();
                    let z: Vec<_> = iter::repeat($z).take(size).collect();

                    super::$func(&mut x, &y);

                    assert_eq!(x, z);
                }
            }
        }
    }

    // Test `add_assign`, `mul_assign` and `sub_assign`
    test!(add_assign, 1u32, 2u32, 3u32);
    test!(mul_assign, 2u32, 3u32, 6u32);
    test!(sub_assign, 3u32, 2u32, 1u32);
}
```


{{out}}

```txt
$ rustc --test dry.rs && ./dry
running 3 tests
test test::mul_assign ... ok
test test::add_assign ... ok
test test::sub_assign ... ok

test result: ok. 3 passed; 0 failed; 0 ignored; 0 measured
```



## Scala


```Scala
import scala.language.experimental.macros
import scala.reflect.macros.Context

object Macros {
  def impl(c: Context) = {
    import c.universe._
    c.Expr[Unit](q"""println("Hello World")""")
  }

  def hello: Unit = macro impl
}
```


## SNOBOL4

There are several features of SNOBOL4 which could be considered meta-programming.  The first of these is the ability to define synonyms for existing operators or functions, a feature which can help in creating DSLs of sorts in SNOBOL4 programs.  For example the following code will alias the built-in function <code>IDENT</code> to <code>SAME</code> and the unary operator <code>*</code> to <code>$</code>:

```snobol4

        OPSYN('SAME','IDENT')
        OPSYN('$','*',1)

```


This is a simplistic use of <code>OPSYN</code>, however.  More interesting is the aliasing of a function to an operator:

```snobol4

        OPSYN('F','*',1)

```


In this setup, calling <code>F(X)</code> is the same as using the sequence <code>*X</code> which, in more complicated expressions, could result in better readability.

Other metaprogramming features supported would include the use of unevaluated expressions.  If, in code, <code>E</code> is an expression it has a value as soon as it is defined and/or assigned to.  <code>*E</code>, on the other hand, has a value only when it is evaluated either in the context of a pattern or in the context of an <code>EVAL</code>.  The following example shows the motivation for unevaluated expressions in pattern matching contexts:

```snobol4

        &ANCHOR = 0 ; &TRIM = 1
        WORD = BREAK(' .,') . W SPAN(' .,')
        STRING1 = INPUT                       :F(ERROR)
        STRING2 = INPUT                       :F(ERROR)
LOOP    STRING1 WORD =                        :F(OUTPUT)
        STRING2 ' ' W ANY(' .,')              :F(LOOP)
        LIST = LIST W ', '                    :(LOOP)
OUTPUT  OUTPUT = LIST
END

```


In this code, two strings are input and a list of words appearing in both strings is generated.  The problem with this code is that the pattern structure <code>' ' W ANY(' .,')</code> is built on each iteration.  Since pattern building is expensive, putting it in a loop like this is bad form.  It cannot be moved outside of the loop, however, since the value of W changes for each iteration.  The solution to this is to defer the evaluation of the variable <code>W</code> until it is needed while keeping the rest of the pattern intact:

```snobol4

        &ANCHOR = 0 ; &TRIM = 1
        WORD = BREAK(' .,') . W SPAN(' .,')
        FINDW = ' ' *W ANY(' .,')
        STRING1 = INPUT                       :F(ERROR)
        STRING2 = INPUT                       :F(ERROR)
LOOP    STRING1 WORD =                        :F(OUTPUT)
        STRING2 FINDW                         :F(LOOP)
        LIST = LIST W ', '                    :(LOOP)
OUTPUT  OUTPUT = LIST
END

```

In this code, the pattern is constructed only once in the line <code>FINDW = ' ' *W ANY(' .,')</code>.  The value of the variable <code>W</code>, however, is only provided when FINDW is used in a pattern match.  In this case it is given its value from the line before when <code>STRING1</code> is matched against the pattern <code>WORD</code>.  In this way the expense of building the pattern is paid only once, but the flexibility of matching a sequence of values is retained.

The final example of metaprogramming that's available lies in the idiosyncratic way that user-defined functions work in SNOBOL4.  The fact that the <code>DEFINE</code> function can be recalled at any point to redefine any function is a powerful feature that can lead to very efficient code.  (It can also lead to very unreadable code, of course, if not properly used.)

Consider this hand-waving example for the motivation:

```snobol4

* This example provides a bizarrely-expensive addition operation.
* It assumes the existence of an expensive procedure—say a database
* lookup—to extract the value to be added.  This version uses the
* typical initialize-on-definition approach to implementation.
         DEFINE('XADD(X)','XADD')
         ADDVALUE = CALL_SOME_EXPENSIVE_OPERATION()        :(XADD.END)
XADD     XADD = X + ADDVALUE                               :(RETURN)
XADD.END

```


In normal operation the interpreter will execute the <code>DEFINE</code> function and then execute the <code>ADDVALUE = ...</code> line, branching *past* the actual body of the function to the label <code>XADD.END</code>.  If, however, there are many such functions, and especially if there's the possibility that these functions are never actually called, this could render program startup very slow.  For purposes of amortizing initialization time, or for purposes of saving unnecessary initialization, the following code is better:

```snobol4

          DEFINE('XADD(X)','XADD.INIT')                    :(XADD.END)
XADD.INIT ADDVALUE = CALL_SOME_EXPENSIVE_OPERATION()
          DEFINE('XADD(X)','XADD')
XADD      XADD = X + ADDVALUE                              :(RETURN)
XADD.END

```


The code now defines the <code>XADD</code> function and immediately, without doing initialization, jumps to the <code>XADD.END</code> label, bypassing both initialization and the function body.  The trick here is that it defines the entry point of the function to be the <code>XADD.INIT</code> label.  Now the first time <code>XADD</code> is called, control is transferred to <code>XADD.INIT</code>, the expensive initialization is performed, then the <code>XADD</code> function is *redefined* to point to the <code>XADD</code> label as the entry point.  From this point onward all calls to <code>XADD</code> only perform the calculation, not the expensive initialization while the expensive initialization isn't paid at all unless the function is used at least once.

There are, of course, many other uses for function redefinition in this style which are suitable for metaprogramming efforts.  Indeed such features are used prominently in the debugging subsystems of SNOBOL4 implementations.


## Shen

Being a Lisp, metaprogramming is easily achievable in Shen through macros. However, defining macros is only possible when the typechecker is off.

```shen
(define make-list
  [A|D] -> [cons (make-list A) (make-list D)]
  X -> X)

(defmacro info-macro
  [info Exp] -> [output "~A: ~A~%" (make-list Exp) Exp])

(info (* 5 6)) \\ outputs [* 5 6]: 30
```

Like most macro systems, defmacro looks like a function that takes a sexp and returns one. However, Shen's defmacro is special in that it allows arbitrary activation of sexps.

```shen
(0-) (defmacro +-macro
       [A + B] -> [+ A B])
macro
+-macro

(1-) (1 + (* 2 3))
7
```

It's important to be careful when using macros like this; this example would be bad because + is sometimes used as an argument to a function (e.g. (fold-left + 0) would compile to (+ fold-left 0)). However, the fact that a symbol can at once match a macro and denote a function can give the illusion of optional arguments or polyadicity. This is how many mathematical operators and functions like append work while retaining their type signature:

```shen
(2-) (tc +)
true

(3+) (+ 1 2 3)
6 : number

(4+) +
+ : (number --> (number --> number))

(5-) (tc -)
false

(6-) (macroexpand [+ 1 2 3])
[+ 1 [+ 2 3]]
```



## Sidef

Sidef recognizes all mathematical operators in Unicode and allows the user to define methods that behave like infix operators, even for built-in types.

```ruby
class Number {
    method ⊕(arg) {
        self + arg
    }
}

say (21 ⊕ 42)
```


Another example of metaprogramming, is the definition of methods at run-time:


```ruby
var colors = Hash(
               'black'   => "000",
               'red'     => "f00",
               'green'   => "0f0",
               'yellow'  => "ff0",
               'blue'    => "00f",
               'magenta' => "f0f",
               'cyan'    => "0ff",
               'white'   => "fff",
             )

for color,code in colors {
    String.def_method("in_#{color}", func (self) {
        '<span style="color: #' + code + '">' + self + '</span>'
    })
}

say "blue".in_blue
say "red".in_red
say "white".in_white
```

{{out}}

```txt

<span style="color: #00f">blue</span>
<span style="color: #f00">red</span>
<span style="color: #fff">white</span>

```



## Tcl

Metaprogramming is considered to be normal in Tcl; the whole language was designed to support new operations that work with a similar level of integration to existing commands (and indeed, the standard commands are not syntactically special in any way), and the <code>upvar</code> and <code>uplevel</code> commands are ''specifically'' for this sort of use. Moreover, there are no language keywords that need to be worked around; words/tokens can be used to mean anything necessary. For example:

```tcl
proc loopVar {var from lower to upper script} {
    if {$from ne "from" || $to ne "to"} {error "syntax error"}
    upvar 1 $var v
    if {$lower <= $upper} {
        for {set v $lower} {$v <= $upper} {incr v} {
            uplevel 1 $script
        }
    } else {
        # $upper and $lower really the other way round here
        for {set v $lower} {$v >= $upper} {incr v -1} {
            uplevel 1 $script
        }
    }
}
```

The above creates a new <code>loopVar</code> command that might be used like this:

```tcl
loopVar x from 1 to 4 {
    loopVar y from $x to 6 {
        puts "pair is ($x,$y)"
        if {$y >= 4} break
    }
}
```

Which prints this:

```txt

pair is (1,1)
pair is (1,2)
pair is (1,3)
pair is (1,4)
pair is (2,2)
pair is (2,3)
pair is (2,4)
pair is (3,3)
pair is (3,4)
pair is (4,4)

```

As you can see, the new looping command is wholly integrated with the rest of the Tcl language.

Code generation is also possible. The easiest approach is to use the <code>list</code> command to generate substitution-free command, leaving all substitutions to places that are written by the programmer directly.
<!-- TODO: write an example of this -->

Finally, the total lack of keywords is exemplified by this classic fragment of Tcl:

```tcl>set set set</lang

In this, the first <code>set</code> is a command (that updates a variable), the second is the name of a variable in the current namespace, and the third is a string that will be placed in a variable.


## TXR


TXR has a built-in Lisp dialect called TXR Lisp, which supports meta-programming, some of which is patterned after ANSI Common Lisp. TXR provides:

* run-time access to its parser for Lisp expressions: <code>(read "(+ a b c)")</code>;
* a parser for regular exprssions: <code>(regex-parse "a.*b")</code> which produces abstract syntax;
* a run-time compiler for converting regular expression abstract syntax to compiled regular expression object;
* a <code>eval</code> function which expands and evaluates Lisp abstract syntax;
* global as well as lexically scoped macros, for both compound forms (with automatically destructured parameter lists) and symbols (symbol macros): the operators <code>defmacro</code>, <code>defsymacro</code>, <code>macrolet</code> and <code>symacrolet</code>;
* structural quasiquote for convenient macro writing.

Example define a while loop which supports break and continue. Two forms of break are supported <code>break</code> which causes the loop to terminate with the return value <code>nil</code> and <code>(break &lt;form&gt;)</code> which returns the specified value.


```txrlisp
(defmacro whil ((condition : result) . body)
  (let ((cblk (gensym "cnt-blk-"))
        (bblk (gensym "brk-blk-")))
    ^(macrolet ((break (value) ^(return-from ,',bblk ,value)))
       (symacrolet ((break (return-from ,bblk))
                    (continue (return-from ,cblk)))
         (block ,bblk
           (for () (,condition ,result) ()
             (block ,cblk ,*body)))))))

(let ((i 0))
  (whil ((< i 100))
    (if (< (inc i) 20)
      continue)
    (if (> i 30)
      break)
    (prinl i)))

(prinl
  (sys:expand
    '(whil ((< i 100))
       (if (< (inc i) 20)
         continue)
       (if (> i 30)
         break)
       (prinl i))))
```


{{out}}


```txt
20
21
22
23
24
25
26
27
28
29
30
(block #:brk-blk-0062
  (for () ((< i 100) ())
    () (block #:cnt-blk-0061
         (if (< (sys:setq i (succ i))
                20) (return-from
                      #:cnt-blk-0061))
         (if (> i 30)
           (return-from
             #:brk-blk-0062))
         (prinl i))))
```



## zkl

Meta programming is quite limited unless you hack the compiler (which is written in zkl) or you write a DSL. Otherwise, there is a "kinda like" C pre-processor support.

```zkl
#define name [0|1]
#if [0|1|name]
#else, #endif
```



```zkl
//Full zkl functionality but limited access to the parse stream; only #defines
#ifdef name
#fcn name {code}
```



```zkl
// Shove text into the parse stream
#text name text
#tokenize name, #tokenize f, #tokenize f(a)
```



```zkl
#<<<#
text, any text, inside #<<<# pairs is ignored
#<<<#
```


```zkl
string:=
#<<<
"here docs: 
all text in #<<< pairs is collected into one [long] line and passed 
verbatim to the tokenizer. Illustrated here as quoted (\") strings
can not span lines.";
#<<<
println(string);  // contains newlines
```

In addition, there is a concept of "parse space/time" - it is after parsing and before compiling where the full power of the language can be used to "so stuff". For example, enums can be implemented like so:

```zkl
const{ var _n=-1; var [proxy] N=fcn{ _n+=1 } }
const X=N;     // → 0
println(_n);   // → 2 code time is after const time
const Y=N,Z=N; // → 1,2
```


{{omit from|Ada}}
{{omit from|AWK}}
{{omit from|bc}}
{{omit from|JavaScript}}
{{omit from|sed}}
