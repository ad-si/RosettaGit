+++
title = "Talk:User defined pipe and redirection operators"
description = ""
date = 2017-06-04T11:56:20Z
aliases = []
[extra]
id = 10512
[taxonomies]
categories = []
tags = []
+++

== Yet another task without a task ==

this task has several potential areas of activity:

#  syntax (right to left infix notation)
#  serialization and deserialization
#  command chaining
#  dealing with external programs

Currently, it's not clear which of these areas of activity are desired, and there are at least 15 possibilities (considering only the inclusion or exclusion of each area of activity, and not considering variants within each area). --[[User:Rdm|Rdm]] 01:45, 13 September 2011 (UTC)

: I don't think the task wants any of the above.  Seems the goal is to define stream-like objects where each one's output can be taken up by another as input, and the task's focus is to device a mechanism to drive data through such a chain.  BTW, since data flows unidirectionally, it definitely does not require coroutines, all you need to do is have the object at the output end to pull data from upstream on-demand.  The problem of the task: it's asking to much. Tail, head, uniq, sort, grep, wc, file io, subshell, redirect-in, redirect-out, pipe -- it's what, reliving 40 years of unix experience in a flash? --[[User:Ledrug|Ledrug]] 02:04, 13 September 2011 (UTC)

:: As a general rule, languages already implement routines where on object's output can be taken by another as input.  In fact, it's hard to think of any language which implements objects which does not implement something like message passing.  Even functional languages let you pass the output of one function to another function.  So... that seems a bit trivial?  And if you are going to require one of them starts before the other completes, that gets into time slicing or multiprocessing or co-routines?  But some OS pipe implementations (*cough*windows*cough*) buffer the full output from one command before starting the next... --[[User:Rdm|Rdm]] 10:43, 13 September 2011 (UTC)

::: I'm looking for "User defined pipe and redirection operators" in particular.  However - you are right - how the actual commands are run (sequentially or concurrently) is relevant.  {Naively I was thinking nobody would want to actually implement "MSDS" sequential (MSDOS caches intermediate result in a file, faking piping, without multitasking)... oh... Microsoft did and conquered the world.... sigh} 
:::BTW you don't need to actually do any message passing, or multitasking.  It can be done totally within one process using co-procedures.
:::However (I believe) the "&" operator requires at least threads.  And... I just figured out how to define "&" in Algol68... cheers! :-)  I'll make the "&" operator a language "kudos" if achieved.
::::This is starting to sound like an OS [[wp:Command-line_interface|CLI]] implementation task... That said, yes, unix trailing & (as opposed to &&) requires either threading or coroutines.  That said, & could be implemented as "defer this operation until you have nothing else to do", in a single threaded environment -- this is equivalent to a time-slice implementation where backgrounded tasks do not get any resources (or to a perceived behavior similar to that of a time slice implementation under heavy load). --[[User:Rdm|Rdm]] 13:11, 13 September 2011 (UTC)

Adhere to the syntax of the specific language where required, eg the use of brackets and names of operators.

For example: I had to use the operator "=:" instead of the standard "|" pipe-char as the "|" char has a special (and fixed and unchangeable) meaning in Algol68.  (I could have used [[wp:Douglas McIlroy|Douglas McIlroy]]'s "^" char for piping maybe? but =: reads better.)

Here is the "''Sample shell script''", but rewritten in Algol.

```algol68
PR READ "prelude/general.a68" PR

MODE REC = STRING;
FORMAT rec fmt = $g$;

PR READ "Iterator_pipe_operators.a68" PR
PR READ "Iterator_pipe_utilities.a68" PR 

FLEX[0]STRING aa;

cat (
    head(4,) < "List_of_computer_scientists.lst",
    cat("List_of_computer_scientists.lst") =: grep(ALGOL,) =: tee("ALGOL_pioneers.lst"),
    tail(4,"List_of_computer_scientists.lst")
  ) =: sort =: uniq =: tee("the_important_scientists.lst") =: grep "aa" >> aa;

printf(($"Pioneer: ", $" "g$, aa, $l$))

```

I have almost finished, and hope it will take less then 300 lines of code.

''' So far:'''

```txt

$ wc -l *Iterator_pipe*s.a68
 174 Iterator_pipe_operators.a68
  58 Iterator_pipe_utilities.a68
  20 test_Iterator_pipe_operators.a68
 252 total

```


This task should be OK in python, especially the operators, and also Ada.  I figure the GNU C has a fair chance.  C++ should be able to handle the operator overloading.

I'm not familiar enough with other languages to make any real comment. [Ocaml can do any thing! (apparently)] :-) ... Go should be real interesting!

BTW: Here is a complete implementation of "''tail''", notice it uses a sliding window:

```algol68
PROC tail yield rec = (INT n, CONJUNCTION args, YIELDREC yield)VOID:
  FOR argn FROM LWB args TO UPB args DO
    INSTREAM rec gen = args[argn];
    CASE rec gen IN
      (FILENAME name): IF LWB args = UPB args THEN yield("==> "+name+" <==") FI
    ESAC;
    [0:n-1]REC window; INT window end := -1;
  # FOR REC rec IN # cat(rec gen)(#) DO #
  ##   (REC rec)VOID:
         window[(window end+:=1) MOD n]:= rec
  # OD #);
    done:
    FOR line FROM window end-n+1 TO window end DO
      IF line>=0 THEN
        yield(window[line MOD n])
      FI
    OD
  OD;

PROC tail = (INT n, CONJUNCTION args)GENREC:
  tail yield rec(n, args ,);

# Define an optional monadic TAIL OPerator #
OP TAIL = (INT n)MANYTOONE: tail(n,);
```


Note that this "''tail''" implementation requires just one argument "n", keeping things simple to satisfy the use of tail in the "''Sample shell script''".  The task is not asking for reinvention of head/tail etc. just enough to run the "''sample shell script''" while retaining the basic functionality of the cloned shell utility, basically just a proof of concept for a particular language.

'''Rationale''': Pipes appear in a hoard of different languages.  It always bugs me when a feature is '''cemented''' into a language and hence cannot enhanced.  Being such a wide spread and useful concept, it would be nice to simply define a few new operators and have piping/redirection available in the new language.

Indeed, having to the ability to add pipes & redirections to a language means a coder can evolve the pipe/redirection definition to match the environment.  For example the pipe/redirection operators defined above are "[[wp:Strong typing|strong typed]]" (currently '''string'''), hence the compiler will detect data of the wrong type (currently '''string''') being piped to the wrong "coprocedure" type and report with a '''compile time''' semantic error, hence one [[wp:Unit_testing|unit test]] script just wrote itself!! (joy).

[[User:NevilleDNZ|NevilleDNZ]] 03:26, 13 September 2011 (UTC)

== more ambitious than unix ==

The task currently says "Pass each record on as soon possible running all filters/procedures concurrently."

But unix does not know anything about records and passes blocks of characters which typically do not represent complete records or complete lines (except in non-portable contexts where the programs have records which match the OS buffer block size). Meanwhile, in a non-multi-tasking language "as soon [as] possible" conflicts with the task requirement "Specifically do not cache the entire stream before the subsequent filter/procedure start".

Also, there's an implicit task "requirement" here, that output be characters.  And a secondary implicit task "requirement" here that files be supported (since that what redirection means) which would also suggest that the task needs to support file reference by name.  And, finally, in unix, the commands are (as a general rule) external programs, but it's not clear if this task allows for that kind of implementation. --[[User:Rdm|Rdm]] 13:17, 13 September 2011 (UTC)

I think the confusion is that the task is using "shell" terminology such as "pipe" & "redirect".  This "terminology" created the expectation that the task ''needs'' to use actual OS based "pipes", "processes" and shell commands.  Basically the appearance that "shell like stuff" is being done ''creates'' the expectation that the task ''mandates'' the code to create some king of "CLI" (command line interpreter).  This is not the actuality.

In essence that is required is the creation of the operators "<", "|", ">", "<<" & ">>", with the basic plumbing such as "cat" & "tee".  

Here are my thoughts, sketched in python code, note:
* the code ''does not'' require OS pipes
* neither does it require OS multitasking
* data is being passed as "rec", this could be a string, but the key point is that (essentially) in this "python sketch" only a one record buffer is required, and this in actuality is only the ''argument list'' itself.

```python
#!/usr/bin/env python

class Filter(function):
  def __init__(self, code):
    self.code=code

  def __or__(self, next_cmd):
    for rec in self.code():
      yield(next_cmd(self))

  def __gt__(self, filename):
    file = open(filename, "w")
    for rec in self.code:
      print >> file, rec

  def __rshift__(self, filename):
    file = open(filename, "a")
    for rec in self.code:
      print >> file, rec
  # some more subclass attributes required to actually call "code" by proxy #

def cat_code(args):
  for file in args:
    for rec in open(file,"r"):
      yield(rec)


cat = Filter(cat_code)

def grep_code(pattern, args):
  for arg in args:
    for rec in arg:
      if pattern in rec:
        yield rec

grep = Filter(grep_code)

cat("List_of_computer_scientists.lst") | grep("ALGOL") > "ALGOL_pioneers.lst"
```


So try not using "Unix pipes" and "Unix processes".

I hope that helps.

[[User:NevilleDNZ|NevilleDNZ]] 22:24, 13 September 2011 (UTC)

Thinking aloud: Python three+ different ways of achieving this pseudo-piping.  Decorators, functional-programming and sub-classing.  However overloading the operators is easiest done via a subclass.   In Algol (I believe) functional-programming must be used.

Thinking again: In the case of the above python sketch, records are processed one at a time, and ''cat'' stops while ''grep'' does its work on that particular record.  So ''cat'' & ''grep'' are not ''exactly'' "concurrent".  Is there a name for this kind of programming?  I know of examples of this in application/utility programs (eg X-Windows and GTK) where the trick is using a "run-loop".  A better description might be "collateral programming" (or co-programming) instead of "concurrent programming"?

[[User:NevilleDNZ|NevilleDNZ]] 00:23, 14 September 2011 (UTC)

:It now sounds like you are describing co-routines as an alternative.  Though other models are also possible.  Meanwhile there are a lot of loose ends.  Consider, for example an implementation where it's desirable for grep to pass along empty records when they do not match (a GPU implementation might favor this kind of thing).  But since this is implementation by analogy with a few "don't do this the easy way" constraints, it's kind of hard to predict what is going to be acceptable and what is not.  --[[User:Rdm|Rdm]] 01:23, 14 September 2011 (UTC)
ThanX for that, the name "co-routines" sounds very close.  But when I read the wikipedia [[wp:Coroutine#Coroutines_and_generators|Coroutines and generators]] section it seems that "coroutines" require multi-threading.  This task does not (necessarily) require multi-threading.  A single threaded "generator" operator is enough.

You idea of a stream of different data types (esp. including empty records) is intriguing.  I will take your lead and provide a simple solution in my ''pet language'' where the record is the usual '''string'''.  And a more complex solution where the record type is '''union'''('''void''', '''string'''), this ''[[wp:Tagged_union#1960s|tagged-union]]'' allows the yeilding of empty records.
Further the redirection targets can be a regular '''file''', but it also can be a '''array''' or '''linked-list'''.  This nicely demonstrates a benefit of user-defined ''pipe and redirection operators''.

Having said the above.  I don't think the ability to pipe a ''generalised'' record should be a necessary part of the actual task solution for any specific language.  Piping variable length strings is enough to satisfy the task requirements.

BTW: I have been wondering how to ''naturally'' take advantage of a [[wp:Graphics Processing Unit|GPU]] and you have given me some ideas.  ThanX.

[[User:NevilleDNZ|NevilleDNZ]] 02:15, 14 September 2011 (UTC)
: Coroutines and multithreading are completely separate topics.  You yourself linked to coroutine implementation using Duff's device before, which decidedly is a single thread.  And for your stream objects, you don't really need either of those anyway. --[[User:Ledrug|Ledrug]] 02:48, 14 September 2011 (UTC)

Indeed you are right.  On reflection, linking to that page on [http://www.chiark.greenend.org.uk/~sgtatham/coroutines.html C coroutines] was a mistake.  Duff's device is kind of extreme.  (Interesting, but extreme!) In fact I've renamed the Algol routines to "Iterator_pipe_operators.a68".  I'll drop reference to co-processing out of the ''Task Description'' too. ThanX

[[User:NevilleDNZ|NevilleDNZ]] 03:50, 14 September 2011 (UTC)

:Looking at the current task, nothing in the task depends on data handling.  In fact, I could not see any issues where I could distinguish between "all at once" handling and "buffer block at a time" handling.  In some cases (for example, sort), it's simply not possible to produce any output until after the end of file is reached.  In other cases (cat) the command is a no-op.  In the remaining cases (grep, tee), the data set being produced is too small to fill a buffer block. --[[User:Rdm|Rdm]] 14:10, 16 September 2011 (UTC)

== test data? ==

The task page currently references "List_of_computer_scientists.lst - cut from wikipedia" but without any url.  I can find [[wp:List of computer scientists]] but it's not clear to me that the corresponding html of that page is the intended starting point.  Perhaps the task could be more specific here? --[[User:Rdm|Rdm]] 14:16, 14 September 2011 (UTC)

I've just added 'A test sample of scientists from wikipedia's "List of computer scientists"' table. 

As this task is still in "Draft" I am keen to find a "classic shell script" that showcases the important features of piping {cat,tee,misc filters} and the various redirections.  Any suggestions?  

Also the "List of computer scientists" is a bit random and the nature of the records is that ''sort | uniq'' filters are not required.  Any suggestions about more appropriate data.  Maybe we could use "[[Old lady swallowed a fly]]" to generate the test data as the lyrics are wonderfully repetitious?  Also the code to generate the test data appears to be suitably brief.

[[User:NevilleDNZ|NevilleDNZ]] 22:32, 17 September 2011 (UTC)

:Here's a one with some history: [[http://books.google.com/books?id=jO-iKwPRX0QC&lpg=PT126&ots=xiWAzX9HDK&dq=knuth%20bell%20lab%20challenge&ie=ISO-8859-1&pg=PT126#v=onepage&q&f=false]]. --[[User:Ledrug|Ledrug]] 23:55, 17 September 2011 (UTC)

== comments after writing the J implementation ==

I think its worth noting that by following the dictates of the task -- which specify "how" the algorithm should be implmented -- we get code an order of magnitude bulkier (and an order of magnitude harder to read, and measurably slower) than an implementation which ignores those dictates.

I have enjoyed my time here, on rosettacode, when we have tasks which allow me to implement them.  I have found the "tasks" which tell me how I am allowed to solve the problem much less enjoyable, because those kinds of constraints almost universally keep me from doing "the right thing".  And, this is not my idea of fun.

In this case, we have syntactic requirements (why?) and we have dataflow requirements (why?) and for illustration we have an example where none of these requirements matter at all (except in terms of code complexity).

I think that when we have an example which does not illustrate the task (or, worse, were the task asks us to implement no example) that we have something which no one really needs.  If there was a need, there would be a clear example which relates to those needs.  I think that the absence of any good example is a symptom of a task that needs to be replaced.

And, the same goes for tasks which specify "allowed techniques".  In my opinion, these are not the sort of thing that anyone trying to solve a problem in a language should reach for.  When we disallow better solutions we are, by definition, asking for a suboptimal implementation.  Anyone serious about using a computer to solve a problem should probably avoid any implementations taken from tasks which prohibit better solutions.  

At minimum, I think we should put all tasks which impose constraints on how the task is implemented in a category which would warn people that they should consider alternative techniques if they need to solve a related problem. --[[User:Rdm|Rdm]] 15:46, 16 September 2011 (UTC)

Of course, there are cases where the techniques used in this task are useful.  Unfortunately, this task is not currently one of those cases.  --[[User:Rdm|Rdm]] 15:46, 16 September 2011 (UTC)

:Then lets hope the task stays as draft until you've had time to discuss this with the original author as others have made similar comments, and I read the task and decided to wait before starting on a Python solution . --[[User:Paddy3118|Paddy3118]] 15:58, 16 September 2011 (UTC)

:P.S. What would you change the task to? (It's a collaboratve site after-all) --[[User:Paddy3118|Paddy3118]] 16:00, 16 September 2011 (UTC)

::Overall?  I do not know.  But I see three different tasks here:
::# Composing operations
::# Syntax declaration
::# Dataflow management
::Of those three, syntax declaration seems the most useless.  At the very least, I see no reason to impose "left to right" processing on all languages.  So I would be inclined to discard the syntactic requirements.
::Of the remaining two, composition should be trivial, for any language represented here.  But that might be worthy of a task, or at least a reference to an existing task.  (And we probably do have something like that already posted.)
::That leaves data flow manipulation. But the dataflow used in pipes is something of a hack -- it is neither reliable, nor algorithmically useful.  You mostly see it in action because file systems are so slow, but depending on what you are doing you still might need to wait hours before getting meaningful results... 
::Of course, composability matters -- it [http://blog.dbpatterson.com/post/10244529137 matters a lot] -- but it's scarce at the OS level, for some operating systems, and is readily available in the context of programming languages. 
::Similarly, good data flow also matters... and is also readily available in the context of programming languages (as long as you do not try to make one language behave exactly like another).
::So... personally?  I would not bother changing the task -- I would just file it somewhere out of the way.  It's not like there's any shortage of problems that need solving.  --[[User:Rdm|Rdm]] 16:56, 16 September 2011 (UTC)
::: I disagree about the pipe-like dataflow being not useful.  For example, when reading or writing a PDF, a PDF stream object may go through multiple encoders/filters in sequence, e.g. PNG, then hexencoded, then RLE, then flate, and each stream itself may contain multiple streams inside (not unlike tar xf).  When dealing with one of these, it's probably a good idea to use filters that pass on partial data as soon as possible so that total memory usage doesn't get blown out of proportion.  It has ''nothing'' to do with OS pipes, either.
::: The beef you have is probably more of a J thing: assume we'll all have massively parallel computers in the near future; assume we'll always have enough memory; thus always deal with the full extent of available data because it's the 'right thing' and will naturally lead to better-looking, more concise code.  Which is probably ok for academia, but it's not fair to blame everything else that fall outside of academic scope. --[[User:Ledrug|Ledrug]] 19:27, 16 September 2011 (UTC)
:::: I do not know enough about PDF internals to contemplate that one in any depth.  But my impression of PDF is that it represents a sequence of pages.  So I think the logical architecture there would be to produce each page independently.  Or, if there is intermediate state on one page that's needed for a subsequent page, the logical architecture would be to produce each page sequentially.  Or perhaps you split that in two and build up a document structure that's independent of the pages and then build the pages independently...  But I am not aware of any requirement for an arbitrarily sized buffer which has nothing to do with the structure of the data being processed.  That's what pipes do -- and it has no algorithmic value that I'm aware of.  Mind you, I have seen PDF implementations which produce arbitrary rectangles on a page rather than the whole page.  But that does not seem like much of a win when I am reading the thing -- usually it just means extra waits imposed on me before I can finish reading a page, and even when I am reading a PDF on my phone there seems to be enough memory to cache a complete image of the page at least for a short while.  And, even if "rectangle smaller than a page" was an algorithmic requirement, that's still different from a pipe -- your page winds up being a collection of rectangles and your implementation knows how big they are.  With pipes, the buffer size is imposed by the OS and can be subject to change for reasons that have nothing to do with your implementation.  --[[User:Rdm|Rdm]] 19:41, 16 September 2011 (UTC)

== Scoping ==

Right now, the task description is long and difficult to read. It also prescribes a lot of work. So for the Tcl implementation I didn't bother doing all the syntactic parts and instead focussed on the concept of a pipeline as a sequence of items (keeping redirections as their own pipeline elements). That gives a lot of bang for the buck yet with very little effort. What I don't know is whether this short-cutting is acceptable as a solution, which indicates that the scoping/description of the task isn't quite right yet IMO.

For the record, my test case was this:

```tcl
pipeline cat bigfile.txt | grep "foo" | sort | head 5 | >> /dev/tty
```

Except with some minor changes (I used a real file and searched for something that I knew was there on about 1% of lines). –[[User:Dkf|Donal Fellows]] 10:56, 20 September 2011 (UTC)

I have to agree, so I 'reduced task description..." by removing the wikipedia cut/paste of "[[wp:Pipeline_(Unix)|Pipeline_(Unix)]]" as it didn't seem to add much value.

I'm still keen to see a degree of parallelism in the piping in the test case eg `cmd1;cmd2...` But strictly speaking this isn't an pipe or a redirection operator!  Maybe it is worth putting conjunctions of pipes in another task...

[[User:NevilleDNZ|NevilleDNZ]] 12:53, 20 September 2011 (UTC)

:This comment reminds me of: http://jlouisramblings.blogspot.com/2011/07/erlangs-parallelism-is-not-parallelism.html -- or, from my point of view: we talk about "concurrency" and "parallelism" as if they were simple things when in fact they can represent a range of concepts, many of which are only loosely related to each other.  Meanwhile, depending on the application, some of those concepts can be undesirable even though others are desirable.  All of which can matter when dealing with issues of scope and practicality. --[[User:Rdm|Rdm]] 17:20, 21 September 2011 (UTC)

== Simplify description ==

Maybe the task description should simply read:

Create "user defined" the equivalents of the Unix shell "<", "|", ">", "<<", ">>" and $(cmd) operators and demonstate their operation.

Languages that do not support user defined operators should be omitted.

[[User:Markhobley|Markhobley]] 22:51, 9 February 2013 (UTC)

: That's not enough -- equivalence is contextual.  Here, I can easily identify three different forms of equivalence, each of which allows a variety of variations even without considering optional combinations with the others.

:: Syntactic equivalence (ordering of the components)

:: Functional equivalence (results after execution)

:: Implementation equivalence (for example: similar buffer size and structure, use of fork(), ...)

: One problem, here, is that the interpretation favored on this site (functional equivalence) is trivial in the context of most programming languages - most of these operators wind up being "put a result somewhere".  So, to avoid the trivialness of this task we might be inclined to favor an implementation equivalence (perhaps, including: buffer sizes less than multiple gigabytes in size, which in turn means buffers are required in the implementation), but where do you draw the line?  For example, is it important to implement a process scheduler as a part of this task? [probably not, but what about the many other dozens of facets of how I have used these unix mechanisms?]. --[[User:Rdm|Rdm]] 03:33, 10 February 2013 (UTC)

:: I think just go for the trivial context for the purpose of this task, ie "put a result somewhere" is fine. For more complicated scenarios create a new task, eg "User defined pipe and redirection operators/With buffering", or "User defined pipe and redirection operators/With scheduler".
[[User:Markhobley|Markhobley]] 07:24, 10 February 2013 (UTC)

== Different grep ? ==
Perhaps the task details should be less "Algol oriented". The "Algol pioneers" count could be replaced by something more generic - "compiler" maybe? --[[User:Tigerofdarkness|Tigerofdarkness]] ([[User talk:Tigerofdarkness|talk]]) 11:55, 4 June 2017 (UTC)
